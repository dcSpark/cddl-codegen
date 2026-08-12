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
  RoadmapDocumentV2,
  SourceReplacement,
  SourceSpan,
} from "../model/documents.ts";

const UTF8 = new TextEncoder();
const MATRIX_SOURCE_PATH = "cddl-matrix/roadmap.toml" as RepoPath;
const MATRIX_PROJECTION_PATH = "cddl-matrix/ROADMAP.md" as RepoPath;

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

export function liveMatrixV2Document(): RoadmapDocumentV2 {
  const decoded = decodeRoadmapSource(
    UTF8.encode(liveMatrixSourceText),
    MATRIX_SOURCE_PATH,
    "matrix",
  );
  assert(
    decoded.document.schema_version === 2 && decoded.document.authority === "authoritative",
    "committed matrix self-test source is not authoritative schema v2",
  );
  assert(
    decoded.document.source_path === MATRIX_SOURCE_PATH &&
      decoded.document.projection_path === MATRIX_PROJECTION_PATH,
    "committed matrix self-test source does not declare the live production paths",
  );
  return decoded as RoadmapDocumentV2;
}

export function liveMatrixLegacyV2Document(): RoadmapDocumentV2 {
  const decoded = liveMatrixV2Document();
  return { ...decoded, document: { ...decoded.document, projection_layout: "legacy_v1" } };
}

/** Historical complete-v1 view retained for WP4/WP5 transition fixtures after the live WP6 cutover. */
export function liveMatrixAuthoritativeDocument(): RoadmapDocumentV1 {
  const decoded = liveMatrixV2Document();
  const { projection_layout: _projectionLayout, ...document } = decoded.document;
  return {
    ...decoded,
    document: {
      ...document,
      schema_version: 1,
      semantic_conversion: "complete",
      frozen_legacy_span_ids: [],
    },
  };
}

export function liveMatrixAuthoritativeSource(): Uint8Array {
  liveMatrixV2Document();
  return UTF8.encode(liveMatrixSourceText);
}

export function liveMatrixProjection(): Uint8Array {
  return UTF8.encode(liveMatrixProjectionText);
}

/** Frozen pre-WP7 projection used only by historical v0/v1 transition fixtures. */
export function liveMatrixLegacyProjection(): Uint8Array {
  const projection = UTF8.encode(liveMatrixProjectionText
    .replace(
      /^<!-- GENERATED FILE: owned by cddl-matrix\/roadmap\.toml; edit that TOML source and run project_roadmaps\.ts --write\. -->\n\n/u,
      "",
    )
    .replace(/^ *<a id="roadmap-id-[^"]+"><\/a>\n\n(?= *#)/gmu, "")
    .replace(/^ *<a id="roadmap-id-[^"]+"><\/a>\n/gmu, ""));
  assert(
    projection.byteLength === 84_580 &&
      sha256(projection) === "f010705393ddbd0b5fa25368c7903df5c5f87a6e500bbdb9c99f7f22bf9bd69e",
    "reconstructed matrix legacy projection escaped its frozen length/digest",
  );
  return projection;
}

function sha256(value: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(value).digest("hex");
}

function replacementSpanIds(value: {
  readonly span_ids?: readonly SpanId[];
  readonly source_replacements?: readonly SourceReplacement[];
}): readonly SpanId[] {
  if (value.span_ids !== undefined) return value.span_ids;
  assert(value.source_replacements !== undefined, "live matrix owner has neither raw spans nor semantic replacements");
  return value.source_replacements.map((replacement) => replacement.span_id);
}

function frozenOwnerField(value: object, spanId: SpanId): Uint8Array {
  if ("source_block_md" in value && value.source_block_md instanceof Uint8Array) return value.source_block_md;
  assert("source_replacements" in value && Array.isArray(value.source_replacements),
    `live matrix semantic owner lacks replacement for ${spanId}`);
  const replacement = (value.source_replacements as SourceReplacement[]).find((entry) => entry.span_id === spanId);
  assert(replacement !== undefined, `live matrix semantic owner lacks replacement row for ${spanId}`);
  if (replacement.replacement_field === "body_md" && "body_md" in value && value.body_md instanceof Uint8Array) return value.body_md;
  if (replacement.replacement_field === "marker_md" && "marker_md" in value && value.marker_md instanceof Uint8Array) return value.marker_md;
  if (replacement.replacement_field.startsWith("payload.") && "payload" in value) {
    const field = replacement.replacement_field.slice("payload.".length);
    const bytes = (value.payload as Record<string, unknown>)[field];
    if (bytes instanceof Uint8Array) return bytes;
  }
  throw new Error(`live matrix replacement ${replacement.replacement_field} has no authored bytes`);
}

function reconstructedRawFields(
  value: {
    readonly span_ids?: readonly SpanId[];
    readonly source_replacements?: readonly SourceReplacement[];
  },
  sourceKind: SourceSpan["source_kind"],
  ownerId: string,
  spans: ReadonlyMap<SpanId, SourceSpan>,
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
  const chunks = selected.map((span) => {
    const bytes = frozenOwnerField(value, span.id);
    assert(bytes.byteLength === span.end_byte - span.start_byte && sha256(bytes) === span.sha256,
      `live matrix authored field for ${span.id} differs from frozen legacy provenance`);
    return bytes;
  });
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
  const spansById = new Map(authoritative.spans.map((span) => [span.id, span]));
  const promotedParts = authoritative.records.filter((owner): owner is Extract<
    RoadmapDocumentV1["records"][number],
    { readonly render_authority: "semantic" }
  > =>
    owner.render_authority === "semantic" && owner.projection_visibility === "document" &&
    owner.source_replacements.length === 1 && owner.source_replacements[0]!.span_id.startsWith("part-")
  );
  const promotedPartIds = new Map(promotedParts.map((owner) => [
    owner.id,
    owner.source_replacements[0]!.span_id.slice("part-".length),
  ]));
  const sections: RawSectionV0[] = authoritative.sections.map((owner) => ({
    section_id: owner.section_id,
    title: owner.title,
    ...(owner.legacy_aliases === undefined ? {} : { legacy_aliases: [...owner.legacy_aliases] }),
    ...reconstructedRawFields(owner, "section", owner.section_id, spansById),
  }));
  const fragments: RawFragmentV0[] = authoritative.fragments.map((owner) => ({
    fragment_id: owner.fragment_id,
    projection_group: owner.projection_group,
    ...(owner.title === undefined ? {} : { title: owner.title }),
    ...(owner.legacy_aliases === undefined ? {} : { legacy_aliases: [...owner.legacy_aliases] }),
    ...reconstructedRawFields(owner, "fragment", owner.fragment_id, spansById),
  }));
  const legacyMarkers: RawLegacyMarkerV0[] = authoritative.legacy_markers.map((owner) => ({
    marker_id: owner.marker_id,
    legacy_aliases: [...owner.legacy_aliases],
    ...reconstructedRawFields(owner, "legacy_marker", owner.marker_id, spansById),
  }));
  const records: RawRecordV0[] = authoritative.records.filter((owner) =>
    (owner.render_authority !== "semantic" || owner.projection_visibility === "document") &&
    !promotedPartIds.has(owner.id)
  ).map((owner) => {
    const legacyAliases = owner.legacy_aliases?.filter((alias) => !/^README gap #\d+$/u.test(alias));
    return {
      id: owner.id,
      title: owner.title,
      projection_group: owner.projection_group,
      ...(legacyAliases === undefined || legacyAliases.length === 0 ? {} : { legacy_aliases: legacyAliases }),
      ...(owner.tags === undefined ? {} : { tags: [...owner.tags] }),
      ...reconstructedRawFields(owner, "record", owner.id, spansById),
    };
  });
  const parts: RawPartV0[] = authoritative.parts.map((owner) => ({
    part_id: owner.part_id,
    parent_record_id: owner.parent_record_id,
    ...(owner.title === undefined ? {} : { title: owner.title }),
    ...reconstructedRawFields(owner, "part", owner.part_id, spansById),
  }));
  for (const owner of promotedParts) {
    const partId = promotedPartIds.get(owner.id)!;
    const parentRelations = authoritative.relations.filter((relation) =>
      relation.kind === "parent_of" && relation.target === owner.id
    );
    assert(parentRelations.length === 1, `promoted matrix part ${partId} lacks one parent relation`);
    parts.push({
      part_id: partId as RawPartV0["part_id"],
      parent_record_id: parentRelations[0]!.source,
      title: owner.title,
      ...reconstructedRawFields(owner, "record", owner.id, spansById),
    });
  }
  assert(
    [...fragments, ...parts].every((owner) => !("lifecycle_disposition" in owner)),
    "matrix v0 reconstruction leaked a v1 lifecycle disposition",
  );
  const {
    frozen_legacy_span_ids: _frozenLegacySpanIds,
    semantic_conversion: _semanticConversion,
    ...frozenSource
  } =
    authoritative.document;
  const manifest: RoadmapDocumentV0["manifest"] = [];
  for (const entry of authoritative.manifest) {
    if (entry.kind !== "record") {
      manifest.push(entry);
      continue;
    }
    const partId = promotedPartIds.get(entry.record_id);
    if (partId !== undefined) manifest.push({ kind: "part", part_id: partId as RawPartV0["part_id"] });
    else if (records.some((record) => record.id === entry.record_id)) manifest.push(entry);
  }
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
    manifest,
    spans: authoritative.spans.map((span) => {
      if (span.source_kind === "generated_slot") return span;
      const promotedPart = span.source_kind === "record"
        ? promotedPartIds.get(span.owner_id as RoadmapDocumentV1["records"][number]["id"])
        : undefined;
      return promotedPart === undefined
        ? { ...span, owner_field: "source_block_md" as const, migration_status: "raw" as const }
        : {
          ...span,
          source_kind: "part" as const,
          owner_id: promotedPart,
          owner_field: "source_block_md" as const,
          migration_status: "raw" as const,
        };
    }),
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
