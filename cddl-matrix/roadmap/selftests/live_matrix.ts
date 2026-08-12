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
const RETIRED_FIXED_VALUE_CHOICE_MEMBER_LEGACY_BLOCK = `- **Enumerate the fixed-value KINDS in the bare TYPE-CHOICE arm role (\`role.choice-member\`).**
  Buildable now, same shape as the float enumeration that exposed it: the delivered float cells
  cover the member and group-choice-arm positions, and a due-diligence probe of the THIRD
  arm position found \`t = 1.5 / tstr\` and \`t = -1 / null / tstr\` refused for an unspellable derived
  variant identifier (\`F1.5\`, \`U-1\` — § findings, "No auto-naming scheme for a DERIVED variant
  identifier…") while the uint/text kinds are fine — exactly the known-NON-uniform kind axis
  measured on two of three positions. Add choice-member cells per fixed kind with truthful verdicts
  (uint and text as accept rows; float and nint as reject rows carrying the graceful-refusal
  evidence, which is what they stay until a derived-name scheme lands), so the kind × position
  product stops relying on per-delivery diligence for its last column.
`;

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

function withRetiredFixedValueLegacyFloor(authoritative: RoadmapDocumentV1): RoadmapDocumentV1 {
  if (authoritative.records.some((record) => record.id === "matrix.fixed-value-choice-member")) {
    return authoritative;
  }
  const retiredId = "matrix.fixed-value-choice-member" as RoadmapDocumentV1["records"][number]["id"];
  const retiredSpanId = "record-fixed-value-choice-member" as SpanId;
  const grammarId = "matrix.grammar-derived-legality" as RoadmapDocumentV1["records"][number]["id"];
  const record: RoadmapDocumentV1["records"][number] = {
    id: retiredId,
    title: "Enumerate the fixed-value KINDS in the bare TYPE-CHOICE arm role (`role.choice-member`).",
    projection_group: "matrix-side-work" as RoadmapDocumentV1["records"][number]["projection_group"],
    render_authority: "semantic",
    projection_visibility: "document",
    payload: {
      kind: "work",
      summary_md: UTF8.encode("Enumerate the fixed-value KINDS in the bare TYPE-CHOICE arm role (`role.choice-member`).\n"),
      detail_md: UTF8.encode(RETIRED_FIXED_VALUE_CHOICE_MEMBER_LEGACY_BLOCK),
      work_state: "ready",
      work_intent: "add_regression",
      work_kind: "coverage_cell",
      risk: "false_pass_or_red",
      family_id: "matrix.systematic.fixed-value-choice-member" as RoadmapDocumentV1["records"][number]["id"],
      acceptance_md: UTF8.encode("Add one role.choice-member row for each source-derived fixed kind, with uint and text represented as supported and float and nint represented by their current graceful-refusal evidence; do not claim kinds not derived by the reviewed source.\n"),
      priority_band: "normal",
      priority_rationale_md: UTF8.encode("This is the explicit buildable-now last-column hole in an already measured non-uniform kind × arm-position product.\n"),
    },
    source_replacements: [{
      span_id: retiredSpanId,
      replacement_field: "payload.detail_md",
      review_note_md: UTF8.encode("WP5M source-owner review: the complete legacy block is retained byte-for-byte as semantic detail while typed fields own lifecycle and joins.\n"),
    }],
  };
  const recordIndex = authoritative.records.findIndex((entry) => entry.id === grammarId);
  const manifestIndex = authoritative.manifest.findIndex((entry) =>
    entry.kind === "record" && entry.record_id === grammarId
  );
  assert(recordIndex >= 0 && manifestIndex >= 0, "legacy floor grammar insertion target is missing");
  const shiftedSpans = authoritative.spans.map((span) => {
    if (span.id === "slot-counts") return {
      ...span,
      sha256: "c4ba9082a3c86cee77945905c9e5e08430c6f7cfa6dbd39e4cf6d5066d10e6a9",
    };
    return span.start_byte < 13_350 ? span : {
      ...span,
      start_byte: span.start_byte + 936,
      end_byte: span.end_byte + 936,
    };
  });
  return {
    ...authoritative,
    document: {
      ...authoritative.document,
      frozen_source_sha256: "a5a90541a3d96d64107242653ae10d829fbf3d579223c1b9f4a5a5672819655f",
      frozen_source_byte_length: 84_590,
      frozen_source_line_count: 994,
    },
    records: [...authoritative.records.slice(0, recordIndex), record, ...authoritative.records.slice(recordIndex)],
    manifest: [
      ...authoritative.manifest.slice(0, manifestIndex),
      { kind: "record" as const, record_id: retiredId },
      ...authoritative.manifest.slice(manifestIndex),
    ],
    spans: [...shiftedSpans, {
      id: retiredSpanId,
      start_byte: 13_350,
      end_byte: 14_286,
      sha256: "f7d7e19e887d9f2ea51378696abaff2d42f4ad6524f214046db8b7ff55ed8f23",
      source_kind: "record" as const,
      owner_id: retiredId,
      owner_field: "payload.detail_md",
      migration_status: "replaced" as const,
    }].sort((left, right) => left.start_byte - right.start_byte),
  };
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

/** Current authored bytes with only the WP7 ownership/anchor layout removed. */
export function liveMatrixCurrentLegacyProjection(): Uint8Array {
  return UTF8.encode(liveMatrixProjectionText
    .replace(
      /^<!-- GENERATED FILE: owned by cddl-matrix\/roadmap\.toml; edit that TOML source and run project_roadmaps\.ts --write\. -->\n\n/u,
      "",
    )
    .replace(/^ *<a id="roadmap-id-[^"]+"><\/a>\n\n(?= *#)/gmu, "")
    .replace(/^ *<a id="roadmap-id-[^"]+"><\/a>\n/gmu, ""));
}

/** Frozen pre-WP7 projection used only by historical v0/v1 transition fixtures. */
export function liveMatrixLegacyProjection(): Uint8Array {
  const withoutWp7Layout = new TextDecoder().decode(liveMatrixCurrentLegacyProjection());
  const projection = UTF8.encode(withoutWp7Layout
    .replace(
      "144 containment cells, and 301 cddl-codegen annotations",
      "136 containment cells, and 293 cddl-codegen annotations",
    )
    .replace(
      "- **Grammar-derived legality denominator for the role × feature grid.**",
      `${RETIRED_FIXED_VALUE_CHOICE_MEMBER_LEGACY_BLOCK}- **Grammar-derived legality denominator for the role × feature grid.**`,
    ));
  const digest = sha256(projection);
  assert(
    projection.byteLength === 84_590 &&
      digest === "a5a90541a3d96d64107242653ae10d829fbf3d579223c1b9f4a5a5672819655f",
    `reconstructed matrix legacy projection escaped its frozen length/digest: ${projection.byteLength}/${digest}`,
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
  authoritative = withRetiredFixedValueLegacyFloor(authoritative);
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
