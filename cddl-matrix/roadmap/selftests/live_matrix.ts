// @ts-expect-error Bun's text loader supports the live Markdown pickup; TypeScript has no .md declaration.
import liveMatrixProjectionText from "../../ROADMAP.md" with { type: "text" };
import liveMatrixSourceText from "../../roadmap.toml" with { type: "text" };
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
const MATRIX_SOURCE_PATH = "cddl-matrix/roadmap.toml" as RepoPath;
const MATRIX_PROJECTION_PATH = "cddl-matrix/ROADMAP.md" as RepoPath;

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

export function liveMatrixAuthoritativeDocument(): RoadmapDocumentV1 {
  const decoded = decodeRoadmapSource(
    UTF8.encode(liveMatrixSourceText),
    MATRIX_SOURCE_PATH,
    "matrix",
  );
  assert(
    decoded.document.schema_version === 1 && decoded.document.authority === "authoritative",
    "committed matrix self-test source is not authoritative schema v1",
  );
  assert(
    decoded.document.source_path === MATRIX_SOURCE_PATH &&
      decoded.document.projection_path === MATRIX_PROJECTION_PATH,
    "committed matrix self-test source does not declare the live production paths",
  );
  return decoded as RoadmapDocumentV1;
}

export function liveMatrixAuthoritativeSource(): Uint8Array {
  liveMatrixAuthoritativeDocument();
  return UTF8.encode(liveMatrixSourceText);
}

export function liveMatrixProjection(): Uint8Array {
  return UTF8.encode(liveMatrixProjectionText);
}

function sha256(value: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(value).digest("hex");
}

function frozenProjectionSlice(projection: Uint8Array, span: SourceSpan): Uint8Array {
  assert(
    Number.isSafeInteger(span.start_byte) && Number.isSafeInteger(span.end_byte) &&
      span.start_byte >= 0 && span.start_byte <= span.end_byte &&
      span.end_byte <= projection.byteLength,
    `live matrix span ${span.id} has invalid frozen projection range [${span.start_byte},${span.end_byte})`,
  );
  const bytes = projection.slice(span.start_byte, span.end_byte);
  assert(sha256(bytes) === span.sha256, `live matrix span ${span.id} differs from its frozen projection digest`);
  return bytes;
}

function replacementSpanIds(value: {
  readonly span_ids?: readonly SpanId[];
  readonly source_replacements?: readonly SourceReplacement[];
}): readonly SpanId[] {
  if (value.span_ids !== undefined) return value.span_ids;
  assert(value.source_replacements !== undefined, "live matrix owner has neither raw spans nor semantic replacements");
  return value.source_replacements.map((replacement) => replacement.span_id);
}

function reconstructedRawFields(
  value: {
    readonly span_ids?: readonly SpanId[];
    readonly source_replacements?: readonly SourceReplacement[];
  },
  sourceKind: SourceSpan["source_kind"],
  ownerId: string,
  spans: ReadonlyMap<SpanId, SourceSpan>,
  projection: Uint8Array,
): { readonly source_block_md: Uint8Array; readonly span_ids: SpanId[] } {
  const selected = replacementSpanIds(value).map((spanId) => {
    const span = spans.get(spanId);
    assert(span !== undefined, `live matrix owner ${ownerId} refers to missing span ${spanId}`);
    assert(
      span.source_kind === sourceKind && span.owner_id === ownerId,
      `live matrix span ${span.id} does not belong to ${sourceKind} ${ownerId}`,
    );
    return span;
  }).sort((left, right) => left.start_byte - right.start_byte);
  assert(selected.length > 0, `live matrix owner ${ownerId} has no reconstructable legacy span`);
  for (let index = 1; index < selected.length; index++) {
    assert(
      selected[index - 1]!.end_byte === selected[index]!.start_byte,
      `live matrix owner ${ownerId} has non-contiguous legacy spans`,
    );
  }
  const chunks = selected.map((span) => frozenProjectionSlice(projection, span));
  const sourceBlock = new Uint8Array(chunks.reduce((total, chunk) => total + chunk.byteLength, 0));
  let offset = 0;
  for (const chunk of chunks) {
    sourceBlock.set(chunk, offset);
    offset += chunk.byteLength;
  }
  return { source_block_md: sourceBlock, span_ids: selected.map((span) => span.id) };
}

export function liveMatrixShadowV0Document(
  authoritative: RoadmapDocumentV1 = liveMatrixAuthoritativeDocument(),
): RoadmapDocumentV0 {
  assert(
    authoritative.document.source_path === MATRIX_SOURCE_PATH &&
      authoritative.document.projection_path === MATRIX_PROJECTION_PATH,
    "matrix shadow reconstruction requires the live production paths",
  );
  const projection = liveMatrixProjection();
  const spansById = new Map(authoritative.spans.map((span) => [span.id, span]));
  const sections: RawSectionV0[] = authoritative.sections.map((owner) => ({
    section_id: owner.section_id,
    title: owner.title,
    ...(owner.legacy_aliases === undefined ? {} : { legacy_aliases: [...owner.legacy_aliases] }),
    ...reconstructedRawFields(owner, "section", owner.section_id, spansById, projection),
  }));
  const fragments: RawFragmentV0[] = authoritative.fragments.map((owner) => ({
    fragment_id: owner.fragment_id,
    projection_group: owner.projection_group,
    ...(owner.title === undefined ? {} : { title: owner.title }),
    ...(owner.legacy_aliases === undefined ? {} : { legacy_aliases: [...owner.legacy_aliases] }),
    ...reconstructedRawFields(owner, "fragment", owner.fragment_id, spansById, projection),
  }));
  const legacyMarkers: RawLegacyMarkerV0[] = authoritative.legacy_markers.map((owner) => ({
    marker_id: owner.marker_id,
    legacy_aliases: [...owner.legacy_aliases],
    ...reconstructedRawFields(owner, "legacy_marker", owner.marker_id, spansById, projection),
  }));
  const records: RawRecordV0[] = authoritative.records.filter((owner) =>
    owner.render_authority !== "semantic" || owner.projection_visibility === "document"
  ).map((owner) => ({
    id: owner.id,
    title: owner.title,
    projection_group: owner.projection_group,
    ...(owner.legacy_aliases === undefined ? {} : { legacy_aliases: [...owner.legacy_aliases] }),
    ...(owner.tags === undefined ? {} : { tags: [...owner.tags] }),
    ...reconstructedRawFields(owner, "record", owner.id, spansById, projection),
  }));
  const parts: RawPartV0[] = authoritative.parts.map((owner) => ({
    part_id: owner.part_id,
    parent_record_id: owner.parent_record_id,
    ...(owner.title === undefined ? {} : { title: owner.title }),
    ...reconstructedRawFields(owner, "part", owner.part_id, spansById, projection),
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
  const roundTrip = decodeRoadmapSource(canonical, MATRIX_SOURCE_PATH, "matrix");
  assert(roundTrip.document.schema_version === 0, "derived matrix shadow did not round-trip as v0");
  assert(
    sha256(composeRoadmapDocument(roundTrip)) === sha256(canonical),
    "derived matrix shadow did not retain canonical v0 bytes after decode",
  );
  return shadow;
}

export function liveMatrixShadowV0Source(
  authoritative: RoadmapDocumentV1 = liveMatrixAuthoritativeDocument(),
): Uint8Array {
  return composeRoadmapDocument(liveMatrixShadowV0Document(authoritative));
}
