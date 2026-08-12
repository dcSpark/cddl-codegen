// @ts-expect-error Bun's text loader supports the live Markdown pickup; TypeScript has no .md declaration.
import liveTestingProjectionText from "../../../tests/TESTING_ROADMAP.md" with { type: "text" };
import liveTestingSourceText from "../../../tests/testing-roadmap.toml" with { type: "text" };
import { composeRoadmapDocument } from "../compose.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import type { RepoPath, SpanId } from "../model/core.ts";
import type {
  RawFragmentV0,
  RawLegacyMarkerV0,
  RawPartV0,
  RawRecordV0,
  RawSectionV0,
  RoadmapDocumentV0,
  RoadmapDocumentV1,
  SourceReplacement,
  SourceSpan,
} from "../model/documents.ts";

const UTF8 = new TextEncoder();
const TESTING_SOURCE_PATH = "tests/testing-roadmap.toml" as RepoPath;
const TESTING_PROJECTION_PATH = "tests/TESTING_ROADMAP.md" as RepoPath;

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

export function liveTestingAuthoritativeDocument(): RoadmapDocumentV1 {
  const decoded = decodeRoadmapSource(
    UTF8.encode(liveTestingSourceText),
    TESTING_SOURCE_PATH,
    "testing",
  );
  assert(
    decoded.document.schema_version === 1 && decoded.document.authority === "authoritative",
    "committed testing self-test source is not authoritative schema v1",
  );
  assert(
    decoded.document.source_path === TESTING_SOURCE_PATH &&
      decoded.document.projection_path === TESTING_PROJECTION_PATH,
    "committed testing self-test source does not declare the live production paths",
  );
  return decoded as RoadmapDocumentV1;
}

export function liveTestingAuthoritativeSource(): Uint8Array {
  liveTestingAuthoritativeDocument();
  return UTF8.encode(liveTestingSourceText);
}

export function liveTestingProjection(): Uint8Array {
  return UTF8.encode(liveTestingProjectionText);
}

function sha256(value: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(value).digest("hex");
}

function reconstructedRawFields(
  value: { readonly span_ids?: readonly SpanId[]; readonly source_replacements?: readonly SourceReplacement[] },
  sourceKind: SourceSpan["source_kind"],
  ownerId: string,
  spans: ReadonlyMap<SpanId, SourceSpan>,
  projection: Uint8Array,
): { readonly source_block_md: Uint8Array; readonly span_ids: SpanId[] } {
  const spanIds = value.span_ids ?? value.source_replacements?.map((replacement) => replacement.span_id);
  assert(spanIds !== undefined && spanIds.length > 0, `live testing owner ${ownerId} has no reconstructable legacy span`);
  const selected = spanIds.map((spanId) => {
    const span = spans.get(spanId);
    assert(span !== undefined, `live testing owner ${ownerId} refers to missing span ${spanId}`);
    assert(span.source_kind === sourceKind && span.owner_id === ownerId, `live testing span ${span.id} has the wrong owner`);
    const bytes = projection.slice(span.start_byte, span.end_byte);
    assert(sha256(bytes) === span.sha256, `live testing span ${span.id} differs from frozen projection bytes`);
    return { span, bytes };
  }).sort((left, right) => left.span.start_byte - right.span.start_byte);
  const sourceBlock = new Uint8Array(selected.reduce((total, item) => total + item.bytes.byteLength, 0));
  let offset = 0;
  for (const item of selected) {
    sourceBlock.set(item.bytes, offset);
    offset += item.bytes.byteLength;
  }
  return { source_block_md: sourceBlock, span_ids: selected.map((item) => item.span.id) };
}

export function liveTestingShadowV0Document(
  authoritative: RoadmapDocumentV1 = liveTestingAuthoritativeDocument(),
): RoadmapDocumentV0 {
  assert(authoritative.document.source_path === TESTING_SOURCE_PATH && authoritative.document.projection_path === TESTING_PROJECTION_PATH, "testing shadow reconstruction requires live paths");
  const projection = liveTestingProjection();
  const spans = new Map(authoritative.spans.map((span) => [span.id, span]));
  const sections: RawSectionV0[] = authoritative.sections.map((owner) => ({
    section_id: owner.section_id, title: owner.title,
    ...(owner.legacy_aliases === undefined ? {} : { legacy_aliases: [...owner.legacy_aliases] }),
    ...reconstructedRawFields(owner, "section", owner.section_id, spans, projection),
  }));
  const fragments: RawFragmentV0[] = authoritative.fragments.map((owner) => ({
    fragment_id: owner.fragment_id, projection_group: owner.projection_group,
    ...(owner.title === undefined ? {} : { title: owner.title }),
    ...(owner.legacy_aliases === undefined ? {} : { legacy_aliases: [...owner.legacy_aliases] }),
    ...reconstructedRawFields(owner, "fragment", owner.fragment_id, spans, projection),
  }));
  const legacyMarkers: RawLegacyMarkerV0[] = authoritative.legacy_markers.map((owner) => ({
    marker_id: owner.marker_id, legacy_aliases: [...owner.legacy_aliases],
    ...reconstructedRawFields(owner, "legacy_marker", owner.marker_id, spans, projection),
  }));
  const records: RawRecordV0[] = authoritative.records.filter((owner) =>
    owner.render_authority !== "semantic" || owner.projection_visibility === "document"
  ).map((owner) => ({
    id: owner.id, title: owner.title, projection_group: owner.projection_group,
    ...(owner.legacy_aliases === undefined ? {} : { legacy_aliases: [...owner.legacy_aliases] }),
    ...(owner.tags === undefined ? {} : { tags: [...owner.tags] }),
    ...reconstructedRawFields(owner, "record", owner.id, spans, projection),
  }));
  const parts: RawPartV0[] = authoritative.parts.map((owner) => ({
    part_id: owner.part_id, parent_record_id: owner.parent_record_id,
    ...(owner.title === undefined ? {} : { title: owner.title }),
    ...reconstructedRawFields(owner, "part", owner.part_id, spans, projection),
  }));
  const { frozen_legacy_span_ids: _frozenLegacySpanIds, ...frozenSource } =
    authoritative.document;
  const shadow: RoadmapDocumentV0 = {
    document: {
      ...frozenSource,
      schema_version: 0,
      authority: "shadow",
    },
    sections,
    fragments,
    legacy_markers: legacyMarkers,
    records,
    parts,
    generated_slots: authoritative.generated_slots,
    manifest: authoritative.manifest.filter((entry) =>
      entry.kind !== "record" || records.some((record) => record.id === entry.record_id)
    ),
    spans: authoritative.spans.map((span) => span.source_kind === "generated_slot"
      ? span
      : { ...span, owner_field: "source_block_md", migration_status: "raw" }),
  };
  const canonical = composeRoadmapDocument(shadow);
  const roundTrip = decodeRoadmapSource(canonical, TESTING_SOURCE_PATH, "testing");
  assert(roundTrip.document.schema_version === 0, "derived testing shadow did not round-trip as v0");
  return shadow;
}

export function liveTestingShadowV0Source(
  authoritative: RoadmapDocumentV1 = liveTestingAuthoritativeDocument(),
): Uint8Array {
  return composeRoadmapDocument(liveTestingShadowV0Document(authoritative));
}
