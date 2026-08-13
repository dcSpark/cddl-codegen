import { bytesEqual, RoadmapWireError } from "../markdown_codec.ts";
import type {
  DocumentMetaV2,
  GeneratedSlot,
  ManifestEntry,
  Reference,
  Relation,
  RoadmapDocument,
  RoadmapDocumentV2,
  SemanticAuthorityRecord,
  SemanticFragment,
  SemanticLegacyMarker,
  SemanticPart,
  SemanticPayload,
  SemanticSection,
  SourceReplacement,
  SourceSpan,
} from "../model/documents.ts";
import type {
  FragmentId,
  MarkerId,
  PartId,
  SectionId,
  SlotId,
  SpanId,
  RoadmapName,
} from "../model/core.ts";
import { composeRoadmapDocument } from "../compose.ts";
import { decodeMatrixPayload } from "./matrix.ts";
import {
  canonicalSet,
  childLogicalPath as p,
  expectArrayOf,
  expectEnum,
  expectExactTable,
  expectMarkdown,
  expectNonemptyArray,
  expectReferenceId,
  expectRepoPath,
  expectRoadmapId,
  expectSafeInteger,
  expectSha256,
  expectString,
  expectStringSet,
  expectSubordinateId,
  hasOwn,
  indexLogicalPath,
  optionalDecoded,
  optionalValue,
  requiredValue,
  schemaFail,
  type DecodeContext,
  type EnumSchemaField,
  type ExactSchemaRow,
  type SchemaDecodeTrace,
} from "./primitives.ts";
import { shieldTomlMarkdown, type MarkdownBindings } from "./raw_markdown.ts";
import { decodeSharedSemanticPayload } from "./semantic.ts";
import { decodeTestingPayload } from "./testing.ts";

const STRUCTURAL_KINDS = ["section", "fragment", "legacy_marker", "record", "part", "generated_slot"] as const;

export const ROADMAP_ENUM_FIELDS: readonly EnumSchemaField[] = [
  { name: "schema_version", values: ["2"] },
  { name: "authority", values: ["authoritative"] },
  { name: "roadmap", values: ["matrix", "testing"] },
  { name: "frozen_source_eof", values: ["lf", "none"] },
  { name: "projection_layout", values: ["legacy_v1", "anchors_v1", "standing_v1", "unnumbered_v1", "curated_v1"] },
  { name: "render_authority", values: ["semantic"] },
  { name: "fragment_lifecycle_disposition", values: ["document_prose"] },
  { name: "part_lifecycle_disposition", values: ["parent_supporting_prose"] },
  { name: "projection_visibility", values: ["document", "semantic_only"] },
  { name: "manifest_kind", values: STRUCTURAL_KINDS },
  { name: "source_kind", values: STRUCTURAL_KINDS },
  { name: "migration_status", values: ["raw", "replaced", "generated"] },
  { name: "relation_kind", values: ["parent_of", "depends_on", "blocked_by", "supersedes", "split_from", "reopens", "overlaps", "complements", "related", "delegates_to"] },
  { name: "reference_kind", values: ["roadmap", "matrix_feature", "matrix_role", "matrix_cell", "gate", "test_symbol", "file_heading", "spec_passage", "external_issue", "external_commit", "external_release", "consumer_report"] },
] as const;

const RAW_KEYS = ["source_block_md", "span_ids"] as const;
const REPLACEMENT_CHILD = ["source_replacement"] as const;

/**
 * The exact-key rows of the sole supported wire schema, addressed by name. Positional indexing is
 * deliberately absent: a row list that is edited by hand cannot safely be addressed by offset.
 */
export const ROADMAP_ROW = {
  root: { name: "roadmap v2 root", required: ["document", "section", "record", "manifest", "source_span"], optional: ["fragment", "legacy_marker", "part", "generated_slot", "relation", "reference"] },
  document: { name: "roadmap v2 document", required: ["schema_version", "authority", "roadmap", "source_path", "projection_path", "frozen_source_sha256", "frozen_source_byte_length", "frozen_source_line_count", "frozen_source_eof"], optional: ["projection_layout"], forbidden: ["semantic_conversion", "frozen_legacy_span_ids"] },
  section: { name: "semantic section", required: ["section_id", "title", "render_authority", "body_md"], optional: ["legacy_aliases", ...REPLACEMENT_CHILD], forbidden: [...RAW_KEYS] },
  fragment: { name: "semantic fragment", required: ["fragment_id", "projection_group", "render_authority", "lifecycle_disposition", "body_md"], optional: ["title", "legacy_aliases", ...REPLACEMENT_CHILD], forbidden: [...RAW_KEYS] },
  legacy_marker: { name: "semantic legacy marker", required: ["marker_id", "legacy_aliases", "render_authority", "marker_md"], optional: [...REPLACEMENT_CHILD], forbidden: [...RAW_KEYS] },
  record: { name: "semantic record", required: ["id", "title", "projection_group", "render_authority", "projection_visibility", "payload"], optional: ["legacy_aliases", "tags", ...REPLACEMENT_CHILD], forbidden: [...RAW_KEYS, "semantic_shadow"] },
  source_replacement: { name: "source replacement", required: ["span_id", "replacement_field", "review_note_md"] },
  part: { name: "semantic part", required: ["part_id", "parent_record_id", "render_authority", "lifecycle_disposition", "body_md"], optional: ["title", ...REPLACEMENT_CHILD], forbidden: [...RAW_KEYS] },
  generated_slot: { name: "generated slot", required: ["slot_id", "binding", "span_ids"] },
  manifest_table: { name: "manifest table", required: ["entry"] },
  manifest_entry: { name: "manifest entry", required: ["kind"], optional: ["section_id", "fragment_id", "marker_id", "record_id", "part_id", "slot_id"] },
  source_span: { name: "source span", required: ["id", "start_byte", "end_byte", "sha256", "source_kind", "owner_id", "owner_field", "migration_status"] },
  relation: { name: "relation", required: ["source", "kind", "target"], optional: ["note_md"] },
  reference_discriminator: { name: "reference discriminator", required: ["id", "source", "kind"], optional: ["target_id", "feature_id", "role_id", "cell_id", "gate_id", "test_id", "symbol", "path", "heading", "document", "passage", "repository", "issue", "commit", "project", "release", "consumer", "report_reference"] },
} as const satisfies Readonly<Record<string, ExactSchemaRow>>;

export const ROADMAP_SCHEMA_ROWS: readonly ExactSchemaRow[] = Object.values(ROADMAP_ROW);

const REFERENCE_REMAINING: Readonly<Record<Reference["kind"], readonly string[]>> = {
  roadmap: ["target_id"],
  matrix_feature: ["feature_id"],
  matrix_role: ["role_id"],
  matrix_cell: ["cell_id"],
  gate: ["gate_id"],
  test_symbol: ["test_id", "symbol"],
  file_heading: ["path", "heading"],
  spec_passage: ["document", "passage"],
  external_issue: ["repository", "issue"],
  external_commit: ["repository", "commit"],
  external_release: ["project", "release"],
  consumer_report: ["consumer", "report_reference"],
};

const MANIFEST_TARGET: Readonly<Record<(typeof STRUCTURAL_KINDS)[number], string>> = {
  section: "section_id",
  fragment: "fragment_id",
  legacy_marker: "marker_id",
  record: "record_id",
  part: "part_id",
  generated_slot: "slot_id",
};

export const MANIFEST_SCHEMA_ROWS: readonly ExactSchemaRow[] = STRUCTURAL_KINDS.map((kind) => ({
  name: `${kind} manifest entry`,
  required: ["kind", MANIFEST_TARGET[kind]],
}));

export const REFERENCE_SCHEMA_ROWS: readonly ExactSchemaRow[] = Object.entries(REFERENCE_REMAINING).map(
  ([kind, keys]) => ({ name: `${kind} reference`, required: ["id", "source", "kind", ...keys] }),
);

function sortBy<T>(items: T[], key: (item: T) => string): T[] {
  return items.sort((left, right) => {
    const a = key(left);
    const b = key(right);
    return a < b ? -1 : a > b ? 1 : 0;
  });
}

function referenceTuple(reference: Reference): string {
  switch (reference.kind) {
    case "roadmap": return reference.target_id;
    case "matrix_feature": return reference.feature_id;
    case "matrix_role": return reference.role_id;
    case "matrix_cell": return reference.cell_id;
    case "gate": return reference.gate_id;
    case "test_symbol": return `${reference.test_id}\0${reference.symbol}`;
    case "file_heading": return `${reference.path}\0${reference.heading}`;
    case "spec_passage": return `${reference.document}\0${reference.passage}`;
    case "external_issue": return `${reference.repository}\0${reference.issue}`;
    case "external_commit": return `${reference.repository}\0${reference.commit}`;
    case "external_release": return `${reference.project}\0${reference.release}`;
    case "consumer_report": return `${reference.consumer}\0${reference.report_reference}`;
  }
}

function aliases(ctx: DecodeContext, table: object, path: string): object {
  return hasOwn(table, "legacy_aliases")
    ? { legacy_aliases: expectStringSet(ctx, optionalValue(table, "legacy_aliases"), p(path, "legacy_aliases")) }
    : {};
}

function title(ctx: DecodeContext, table: object, path: string): object {
  return hasOwn(table, "title") ? { title: expectString(ctx, optionalValue(table, "title"), p(path, "title")) } : {};
}

function spans(ctx: DecodeContext, table: object, path: string): SpanId[] {
  return canonicalSet(
    ctx,
    expectArrayOf(ctx, requiredValue(table, "span_ids"), p(path, "span_ids"), (value, valuePath) =>
      expectSubordinateId(ctx, value, valuePath) as SpanId,
    ),
    p(path, "span_ids"),
    true,
  );
}

function decodeReplacement(ctx: DecodeContext, raw: unknown, path: string): SourceReplacement {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.source_replacement);
  const field = expectString(ctx, requiredValue(table, "replacement_field"), p(path, "replacement_field"));
  if (!/^[a-z][a-z0-9_]*(?:\.[a-z][a-z0-9_]*|\[[0-9]+\])*$/.test(field)) {
    schemaFail(ctx, "E-SCHEMA-TYPE", p(path, "replacement_field"), "replacement field does not match the closed path grammar");
  }
  return {
    span_id: expectSubordinateId(ctx, requiredValue(table, "span_id"), p(path, "span_id")) as SpanId,
    replacement_field: field,
    review_note_md: expectMarkdown(ctx, requiredValue(table, "review_note_md"), p(path, "review_note_md")),
  };
}

function replacements(ctx: DecodeContext, table: object, path: string): SourceReplacement[] {
  const values = optionalDecoded(table, "source_replacement", path, (raw, fieldPath) =>
    expectArrayOf(ctx, raw, fieldPath, (entry, entryPath) => decodeReplacement(ctx, entry, entryPath)),
  ) ?? [];
  return sortBy(values, (value) => value.span_id);
}

function decodeSemanticPayload(
  ctx: DecodeContext,
  raw: unknown,
  path: string,
  roadmap: RoadmapName,
): SemanticPayload {
  const pre = expectExactTable(ctx, raw, path, {
    name: "semantic payload discriminator",
    required: ["kind"],
    optional: raw !== null && typeof raw === "object" && !Array.isArray(raw) ? Object.keys(raw).filter((key) => key !== "kind") : [],
  });
  const kind = expectString(ctx, requiredValue(pre, "kind"), p(path, "kind"));
  const shared = decodeSharedSemanticPayload(ctx, raw, path);
  if (shared !== undefined) return shared;
  const domain = roadmap === "matrix"
    ? decodeMatrixPayload(ctx, raw, path, kind)
    : decodeTestingPayload(ctx, raw, path, kind);
  if (domain === undefined) {
    schemaFail(ctx, "E-SCHEMA-ENUM", p(path, "kind"), `${kind} is not a ${roadmap} semantic payload kind`);
  }
  return domain;
}

function decodeDocumentMeta(ctx: DecodeContext, raw: unknown): DocumentMetaV2 {
  const pre = expectExactTable(ctx, raw, "document", {
    name: "document discriminator",
    required: ["schema_version"],
    optional: ["authority", "roadmap", "source_path", "projection_path", "frozen_source_sha256", "frozen_source_byte_length", "frozen_source_line_count", "frozen_source_eof", "semantic_conversion", "frozen_legacy_span_ids", "projection_layout"],
  });
  const versionRaw = requiredValue(pre, "schema_version");
  if (typeof versionRaw !== "number" || !Number.isSafeInteger(versionRaw)) {
    schemaFail(ctx, "E-SCHEMA-TYPE", "document.schema_version", "schema_version must be an integer");
  }
  if (versionRaw !== 2) {
    schemaFail(ctx, "E-SCHEMA-VERSION", "document.schema_version", "roadmap schema_version must be 2");
  }
  const table = expectExactTable(ctx, raw, "document", ROADMAP_ROW.document);
  return {
    schema_version: 2,
    authority: expectEnum(ctx, requiredValue(table, "authority"), ["authoritative"] as const, "document.authority"),
    roadmap: expectEnum(ctx, requiredValue(table, "roadmap"), ["matrix", "testing"] as const, "document.roadmap"),
    source_path: expectRepoPath(ctx, requiredValue(table, "source_path"), "document.source_path"),
    projection_path: expectRepoPath(ctx, requiredValue(table, "projection_path"), "document.projection_path"),
    frozen_source_sha256: expectSha256(ctx, requiredValue(table, "frozen_source_sha256"), "document.frozen_source_sha256"),
    frozen_source_byte_length: expectSafeInteger(ctx, requiredValue(table, "frozen_source_byte_length"), "document.frozen_source_byte_length"),
    frozen_source_line_count: expectSafeInteger(ctx, requiredValue(table, "frozen_source_line_count"), "document.frozen_source_line_count"),
    frozen_source_eof: expectEnum(ctx, requiredValue(table, "frozen_source_eof"), ["lf", "none"] as const, "document.frozen_source_eof"),
    ...(hasOwn(table, "projection_layout")
      ? { projection_layout: expectEnum(ctx, optionalValue(table, "projection_layout"),
        ["legacy_v1", "anchors_v1", "standing_v1", "unnumbered_v1", "curated_v1"] as const,
        "document.projection_layout") }
      : {}),
  };
}

/** Every owner declares the semantic render authority; the wire has no other arm. */
function renderAuthority(ctx: DecodeContext, table: object, path: string): "semantic" {
  return expectEnum(ctx, requiredValue(table, "render_authority"), ["semantic"] as const, p(path, "render_authority"));
}

function decodeSection(ctx: DecodeContext, raw: unknown, path: string): SemanticSection {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.section);
  return {
    section_id: expectSubordinateId(ctx, requiredValue(table, "section_id"), p(path, "section_id")) as SectionId,
    title: expectString(ctx, requiredValue(table, "title"), p(path, "title")),
    ...aliases(ctx, table, path),
    render_authority: renderAuthority(ctx, table, path),
    body_md: expectMarkdown(ctx, requiredValue(table, "body_md"), p(path, "body_md")),
    source_replacements: replacements(ctx, table, path),
  };
}

function decodeFragment(ctx: DecodeContext, raw: unknown, path: string): SemanticFragment {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.fragment);
  return {
    fragment_id: expectSubordinateId(ctx, requiredValue(table, "fragment_id"), p(path, "fragment_id")) as FragmentId,
    projection_group: expectSubordinateId(ctx, requiredValue(table, "projection_group"), p(path, "projection_group")) as SectionId,
    ...title(ctx, table, path),
    ...aliases(ctx, table, path),
    render_authority: renderAuthority(ctx, table, path),
    lifecycle_disposition: expectEnum(ctx, requiredValue(table, "lifecycle_disposition"), ["document_prose"] as const, p(path, "lifecycle_disposition")),
    body_md: expectMarkdown(ctx, requiredValue(table, "body_md"), p(path, "body_md")),
    source_replacements: replacements(ctx, table, path),
  };
}

function decodeMarker(ctx: DecodeContext, raw: unknown, path: string): SemanticLegacyMarker {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.legacy_marker);
  return {
    marker_id: expectSubordinateId(ctx, requiredValue(table, "marker_id"), p(path, "marker_id")) as MarkerId,
    legacy_aliases: expectStringSet(ctx, requiredValue(table, "legacy_aliases"), p(path, "legacy_aliases"), true),
    render_authority: renderAuthority(ctx, table, path),
    marker_md: expectMarkdown(ctx, requiredValue(table, "marker_md"), p(path, "marker_md")),
    source_replacements: replacements(ctx, table, path),
  };
}

function envelope(ctx: DecodeContext, table: object, path: string, roadmap: RoadmapName): object {
  return {
    id: expectRoadmapId(ctx, requiredValue(table, "id"), p(path, "id"), roadmap),
    title: expectString(ctx, requiredValue(table, "title"), p(path, "title")),
    projection_group: expectSubordinateId(ctx, requiredValue(table, "projection_group"), p(path, "projection_group")) as SectionId,
    ...aliases(ctx, table, path),
    ...(hasOwn(table, "tags") ? { tags: expectStringSet(ctx, optionalValue(table, "tags"), p(path, "tags")) } : {}),
  };
}

function decodeRecord(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): SemanticAuthorityRecord {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.record);
  const projectionVisibility = expectEnum(
    ctx,
    requiredValue(table, "projection_visibility"),
    ["document", "semantic_only"] as const,
    p(path, "projection_visibility"),
  );
  const sourceReplacements = replacements(ctx, table, path);
  if (projectionVisibility === "document" && sourceReplacements.length === 0) {
    schemaFail(ctx, "E-SCHEMA-STATE", p(path, "source_replacement"), "document-visible semantic record requires at least one source replacement");
  }
  if (projectionVisibility === "semantic_only" && sourceReplacements.length !== 0) {
    schemaFail(ctx, "E-SCHEMA-STATE", p(path, "source_replacement"), "semantic-only record forbids source replacements");
  }
  return {
    ...envelope(ctx, table, path, roadmap),
    render_authority: renderAuthority(ctx, table, path),
    projection_visibility: projectionVisibility,
    payload: decodeSemanticPayload(ctx, requiredValue(table, "payload"), p(path, "payload"), roadmap),
    source_replacements: sourceReplacements,
  } as SemanticAuthorityRecord;
}

function decodePart(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): SemanticPart {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.part);
  return {
    part_id: expectSubordinateId(ctx, requiredValue(table, "part_id"), p(path, "part_id")) as PartId,
    parent_record_id: expectRoadmapId(ctx, requiredValue(table, "parent_record_id"), p(path, "parent_record_id"), roadmap),
    ...title(ctx, table, path),
    render_authority: renderAuthority(ctx, table, path),
    lifecycle_disposition: expectEnum(ctx, requiredValue(table, "lifecycle_disposition"), ["parent_supporting_prose"] as const, p(path, "lifecycle_disposition")),
    body_md: expectMarkdown(ctx, requiredValue(table, "body_md"), p(path, "body_md")),
    source_replacements: replacements(ctx, table, path),
  };
}

function decodeSlot(ctx: DecodeContext, raw: unknown, path: string): GeneratedSlot {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.generated_slot);
  return { slot_id: expectSubordinateId(ctx, requiredValue(table, "slot_id"), p(path, "slot_id")) as SlotId, binding: expectString(ctx, requiredValue(table, "binding"), p(path, "binding")), span_ids: spans(ctx, table, path) };
}

function decodeManifest(ctx: DecodeContext, raw: unknown): ManifestEntry[] {
  const table = expectExactTable(ctx, raw, "manifest", ROADMAP_ROW.manifest_table);
  return expectNonemptyArray(ctx, expectArrayOf(ctx, requiredValue(table, "entry"), "manifest.entry", (entry, path) => {
    const row = expectExactTable(ctx, entry, path, ROADMAP_ROW.manifest_entry);
    const kind = expectEnum(ctx, requiredValue(row, "kind"), STRUCTURAL_KINDS, p(path, "kind"));
    const target = MANIFEST_TARGET[kind];
    const exact = expectExactTable(ctx, entry, path, MANIFEST_SCHEMA_ROWS[STRUCTURAL_KINDS.indexOf(kind)]);
    const rawId = requiredValue(exact, target);
    if (kind === "record") return { kind, record_id: expectRoadmapId(ctx, rawId, p(path, target)) };
    const id = expectSubordinateId(ctx, rawId, p(path, target));
    if (kind === "section") return { kind, section_id: id as SectionId };
    if (kind === "fragment") return { kind, fragment_id: id as FragmentId };
    if (kind === "legacy_marker") return { kind, marker_id: id as MarkerId };
    if (kind === "part") return { kind, part_id: id as PartId };
    return { kind, slot_id: id as SlotId };
  }), "manifest.entry");
}

function decodeSourceSpan(ctx: DecodeContext, raw: unknown, path: string): SourceSpan {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.source_span);
  return { id: expectSubordinateId(ctx, requiredValue(table, "id"), p(path, "id")) as SpanId, start_byte: expectSafeInteger(ctx, requiredValue(table, "start_byte"), p(path, "start_byte")), end_byte: expectSafeInteger(ctx, requiredValue(table, "end_byte"), p(path, "end_byte")), sha256: expectSha256(ctx, requiredValue(table, "sha256"), p(path, "sha256")), source_kind: expectEnum(ctx, requiredValue(table, "source_kind"), STRUCTURAL_KINDS, p(path, "source_kind")), owner_id: expectString(ctx, requiredValue(table, "owner_id"), p(path, "owner_id")), owner_field: expectString(ctx, requiredValue(table, "owner_field"), p(path, "owner_field")), migration_status: expectEnum(ctx, requiredValue(table, "migration_status"), ["raw", "replaced", "generated"] as const, p(path, "migration_status")) };
}

function decodeRelation(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): Relation {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.relation);
  return { source: expectRoadmapId(ctx, requiredValue(table, "source"), p(path, "source"), roadmap), kind: expectEnum(ctx, requiredValue(table, "kind"), ["parent_of", "depends_on", "blocked_by", "supersedes", "split_from", "reopens", "overlaps", "complements", "related", "delegates_to"] as const, p(path, "kind")), target: expectRoadmapId(ctx, requiredValue(table, "target"), p(path, "target")), ...(hasOwn(table, "note_md") ? { note_md: expectMarkdown(ctx, optionalValue(table, "note_md"), p(path, "note_md")) } : {}) };
}

function decodeReference(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): Reference {
  const pre = expectExactTable(ctx, raw, path, ROADMAP_ROW.reference_discriminator);
  const kind = expectEnum(ctx, requiredValue(pre, "kind"), Object.keys(REFERENCE_REMAINING) as Reference["kind"][], p(path, "kind"));
  const referenceKinds = Object.keys(REFERENCE_REMAINING) as Reference["kind"][];
  const table = expectExactTable(ctx, raw, path, REFERENCE_SCHEMA_ROWS[referenceKinds.indexOf(kind)]!);
  const base = { id: expectReferenceId(ctx, requiredValue(table, "id"), p(path, "id")), source: expectRoadmapId(ctx, requiredValue(table, "source"), p(path, "source"), roadmap), kind };
  const string = (key: string): string => expectString(ctx, requiredValue(table, key), p(path, key));
  switch (kind) {
    case "roadmap": return { ...base, kind, target_id: expectRoadmapId(ctx, requiredValue(table, "target_id"), p(path, "target_id")) };
    case "matrix_feature": return { ...base, kind, feature_id: string("feature_id") };
    case "matrix_role": return { ...base, kind, role_id: string("role_id") };
    case "matrix_cell": return { ...base, kind, cell_id: string("cell_id") };
    case "gate": return { ...base, kind, gate_id: string("gate_id") };
    case "test_symbol": return { ...base, kind, test_id: string("test_id"), symbol: string("symbol") };
    case "file_heading": {
      const targetPath = expectRepoPath(ctx, requiredValue(table, "path"), p(path, "path"));
      if (targetPath.startsWith("draft/")) {
        schemaFail(ctx, "E-REFERENCE-FORBIDDEN", p(path, "path"), "draft/ paths cannot provide durable evidence");
      }
      return { ...base, kind, path: targetPath, heading: string("heading") };
    }
    case "spec_passage": return { ...base, kind, document: string("document"), passage: string("passage") };
    case "external_issue": return { ...base, kind, repository: string("repository"), issue: string("issue") };
    case "external_commit": return { ...base, kind, repository: string("repository"), commit: string("commit") };
    case "external_release": return { ...base, kind, project: string("project"), release: string("release") };
    case "consumer_report": return { ...base, kind, consumer: string("consumer"), report_reference: string("report_reference") };
  }
}

function optionalRows<T>(ctx: DecodeContext, root: object, key: string, decode: (raw: unknown, path: string) => T): T[] {
  return hasOwn(root, key)
    ? expectArrayOf(ctx, optionalValue(root, key), key, (entry, entryPath) => decode(entry, entryPath))
    : [];
}

function assertSpanBounds(ctx: DecodeContext, doc: RoadmapDocument): void {
  for (const [index, span] of doc.spans.entries()) {
    const path = `source_span[${index}]`;
    if (span.end_byte <= span.start_byte) {
      schemaFail(ctx, "E-SPAN-EMPTY", path, `source span ${span.id} must be nonempty and forward`);
    }
    if (span.start_byte > doc.document.frozen_source_byte_length || span.end_byte > doc.document.frozen_source_byte_length) {
      schemaFail(ctx, "E-SPAN-BOUNDS", p(path, "end_byte"), `source span ${span.id} exceeds frozen source byte length`);
    }
  }
}

function assertDecodedDomainJoins(ctx: DecodeContext, doc: RoadmapDocumentV2): void {
  const payloads = new Map<string, SemanticPayload>(doc.records.map((record) => [record.id, record.payload]));

  for (const record of doc.records) {
    const payload = payloads.get(record.id);
    if (payload?.kind === "work") {
      if ("transition_ids" in payload) {
        for (const transitionId of payload.transition_ids) {
          const transition = payloads.get(transitionId);
          if (transition?.kind === "signal" && transition.evaluation === "met") {
            schemaFail(ctx, "E-SCHEMA-STATE", `record.${record.id}.transition_ids`, `work cannot park already-fired transition ${transitionId}`);
          }
        }
      }
      if (payload.work_kind === "defect") {
        const evidenceIds = [...(payload.evidence_ids ?? []), ...(payload.regression_evidence_ids ?? [])];
        const evidence = evidenceIds.map((id) => payloads.get(id));
        if (evidence.length !== 0 && evidence.every((entry) => entry !== undefined) && !evidence.some((entry) => entry?.kind === "evidence" && entry.evidence_kind === "harness_free_repro")) {
          schemaFail(ctx, "E-SCHEMA-STATE", `record.${record.id}.evidence_ids`, "generator defect requires harness-free reproduction evidence");
        }
      }
    }
    if (payload?.kind === "testing_cost" && payload.cost_posture === "historical_observation") {
      const evidence = payload.evidence_ids.map((id) => payloads.get(id));
      if (evidence.every((entry) => entry !== undefined) && evidence.some((entry) => entry?.kind !== "evidence")) {
        schemaFail(ctx, "E-SCHEMA-STATE", `record.${record.id}.evidence_ids`, "historical timing evidence IDs must resolve to structural evidence records");
      }
    }
  }
}

export function decodeRoadmapFromBindings(
  bindings: MarkdownBindings,
  expectedRoadmap?: RoadmapName,
  schemaTrace?: SchemaDecodeTrace,
): RoadmapDocument {
  const ctx: DecodeContext = { source: bindings.source, bindings, schema_trace: schemaTrace };
  const rootPre = expectExactTable(ctx, bindings.parsed, "$", { name: "roadmap root discriminator", required: ["document"], optional: ["section", "fragment", "legacy_marker", "record", "part", "generated_slot", "manifest", "source_span", "relation", "reference"] });
  const meta = decodeDocumentMeta(ctx, requiredValue(rootPre, "document"));
  if (expectedRoadmap !== undefined && meta.roadmap !== expectedRoadmap) schemaFail(ctx, "E-ID-NAMESPACE", "document.roadmap", `expected ${expectedRoadmap} roadmap source`);
  const root = expectExactTable(ctx, bindings.parsed, "$", ROADMAP_ROW.root);
  const roadmap = meta.roadmap;
  const doc: RoadmapDocumentV2 = {
    document: meta,
    sections: sortBy(optionalRows(ctx, root, "section", (raw, path) => decodeSection(ctx, raw, path)), (value) => value.section_id),
    fragments: sortBy(optionalRows(ctx, root, "fragment", (raw, path) => decodeFragment(ctx, raw, path)), (value) => value.fragment_id),
    legacy_markers: sortBy(optionalRows(ctx, root, "legacy_marker", (raw, path) => decodeMarker(ctx, raw, path)), (value) => value.marker_id),
    records: sortBy(optionalRows(ctx, root, "record", (raw, path) => decodeRecord(ctx, raw, path, roadmap)), (value) => value.id),
    parts: sortBy(optionalRows(ctx, root, "part", (raw, path) => decodePart(ctx, raw, path, roadmap)), (value) => value.part_id),
    generated_slots: sortBy(optionalRows(ctx, root, "generated_slot", (raw, path) => decodeSlot(ctx, raw, path)), (value) => value.slot_id),
    manifest: decodeManifest(ctx, requiredValue(root, "manifest")),
    spans: optionalRows(ctx, root, "source_span", (raw, path) => decodeSourceSpan(ctx, raw, path)).sort((left, right) => left.start_byte - right.start_byte),
    relations: sortBy(
      optionalRows(ctx, root, "relation", (raw, path) => decodeRelation(ctx, raw, path, roadmap)),
      (relation) => `${relation.source}\0${relation.kind}\0${relation.target}`,
    ),
    references: sortBy(
      optionalRows(ctx, root, "reference", (raw, path) => decodeReference(ctx, raw, path, roadmap)),
      (reference) => `${reference.source}\0${reference.kind}\0${referenceTuple(reference)}`,
    ),
  };
  if (doc.spans.some((span) => span.migration_status === "raw")) {
    schemaFail(ctx, "E-SCHEMA-STATE", "source_span.migration_status", "roadmap schema v2 forbids raw migration spans");
  }
  if (doc.sections.length === 0 || doc.records.length === 0 || doc.spans.length === 0) schemaFail(ctx, "E-SCHEMA-FLOOR", "$", "roadmap requires at least one section, record, manifest entry, and source span");
  for (const record of doc.records) {
    if (
      record.projection_visibility === "semantic_only" &&
      doc.spans.some((span) => span.source_kind === "record" && span.owner_id === record.id)
    ) {
      schemaFail(ctx, "E-SCHEMA-STATE", `record.${record.id}.projection_visibility`, "semantic-only record forbids source spans");
    }
  }
  assertSpanBounds(ctx, doc);
  assertDecodedDomainJoins(ctx, doc);
  bindings.assertAllConsumed();
  return doc;
}

export function decodeRoadmapSource(
  bytes: Uint8Array,
  source: string,
  expectedRoadmap?: RoadmapName,
  requireCanonical = true,
  schemaTrace?: SchemaDecodeTrace,
): RoadmapDocument {
  const doc = decodeRoadmapFromBindings(shieldTomlMarkdown(bytes, source), expectedRoadmap, schemaTrace);
  if (requireCanonical) {
    const canonical = composeRoadmapDocument(doc);
    if (!bytesEqual(bytes, canonical)) {
      throw new RoadmapWireError({ code: "E-TOML-NONCANONICAL", source, logical_path: "$", message: "roadmap TOML bytes do not equal canonical composition", exit: 1 });
    }
  }
  return doc;
}
