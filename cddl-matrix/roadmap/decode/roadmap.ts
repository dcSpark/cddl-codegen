import { bytesEqual, RoadmapWireError } from "../markdown_codec.ts";
import type {
  DocumentMetaV0,
  DocumentMetaV1,
  GeneratedSlot,
  ManifestEntry,
  RawAuthorityRecordV1,
  RawFragmentV0,
  RawFragmentV1,
  RawLegacyMarkerV0,
  RawLegacyMarkerV1,
  RawPartV0,
  RawPartV1,
  RawRecordV0,
  RawSectionV0,
  RawSectionV1,
  Reference,
  Relation,
  RoadmapDocument,
  RoadmapDocumentV0,
  RoadmapDocumentV1,
  SemanticAuthorityRecordV1,
  SemanticFragmentV1,
  SemanticLegacyMarkerV1,
  SemanticPartV1,
  SemanticPayload,
  SemanticSectionV1,
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
import { decodeSharedSemanticPayload, type SemanticDecodePosition } from "./semantic.ts";
import { decodeTestingPayload } from "./testing.ts";

const STRUCTURAL_KINDS = ["section", "fragment", "legacy_marker", "record", "part", "generated_slot"] as const;

export const ROADMAP_ENUM_FIELDS: readonly EnumSchemaField[] = [
  { name: "schema_version", values: ["0", "1"] },
  { name: "authority_v0", values: ["shadow"] },
  { name: "authority_v1", values: ["authoritative"] },
  { name: "roadmap", values: ["matrix", "testing"] },
  { name: "frozen_source_eof", values: ["lf", "none"] },
  { name: "render_authority", values: ["raw", "semantic"] },
  { name: "manifest_kind", values: STRUCTURAL_KINDS },
  { name: "source_kind", values: STRUCTURAL_KINDS },
  { name: "migration_status", values: ["raw", "replaced", "generated"] },
  { name: "relation_kind", values: ["parent_of", "depends_on", "blocked_by", "supersedes", "split_from", "reopens", "overlaps", "complements", "related", "delegates_to"] },
  { name: "reference_kind", values: ["roadmap", "matrix_feature", "matrix_role", "matrix_cell", "gate", "test_symbol", "file_heading", "spec_passage", "external_issue", "external_commit", "external_release", "consumer_report", "unresolved_migration"] },
] as const;

const RAW_KEYS = ["source_block_md", "span_ids"] as const;
const REPLACEMENT_CHILD = ["source_replacement"] as const;

export const ROADMAP_SCHEMA_ROWS: readonly ExactSchemaRow[] = [
  { name: "roadmap v0 root", required: ["document", "section", "record", "manifest", "source_span"], optional: ["fragment", "legacy_marker", "part", "generated_slot"], forbidden: ["relation", "reference"] },
  { name: "roadmap v1 root", required: ["document", "section", "record", "manifest", "source_span"], optional: ["fragment", "legacy_marker", "part", "generated_slot", "relation", "reference"] },
  { name: "roadmap v0 document", required: ["schema_version", "authority", "roadmap", "source_path", "projection_path", "frozen_source_sha256", "frozen_source_byte_length", "frozen_source_line_count", "frozen_source_eof"], forbidden: ["frozen_legacy_span_ids"] },
  { name: "roadmap v1 document", required: ["schema_version", "authority", "roadmap", "source_path", "projection_path", "frozen_source_sha256", "frozen_source_byte_length", "frozen_source_line_count", "frozen_source_eof", "frozen_legacy_span_ids"] },
  { name: "v0 section", required: ["section_id", "title", ...RAW_KEYS], optional: ["legacy_aliases"], forbidden: ["render_authority", "body_md", "source_replacement"] },
  { name: "v1 raw section", required: ["section_id", "title", "render_authority", ...RAW_KEYS], optional: ["legacy_aliases"], forbidden: ["body_md", "source_replacement"] },
  { name: "v1 semantic section", required: ["section_id", "title", "render_authority", "body_md"], optional: ["legacy_aliases", ...REPLACEMENT_CHILD], forbidden: ["source_block_md", "span_ids"] },
  { name: "v0 fragment", required: ["fragment_id", "projection_group", ...RAW_KEYS], optional: ["title", "legacy_aliases"], forbidden: ["render_authority", "body_md", "source_replacement"] },
  { name: "v1 raw fragment", required: ["fragment_id", "projection_group", "render_authority", ...RAW_KEYS], optional: ["title", "legacy_aliases"], forbidden: ["body_md", "source_replacement"] },
  { name: "v1 semantic fragment", required: ["fragment_id", "projection_group", "render_authority", "body_md"], optional: ["title", "legacy_aliases", ...REPLACEMENT_CHILD], forbidden: ["source_block_md", "span_ids"] },
  { name: "v0 legacy marker", required: ["marker_id", "legacy_aliases", ...RAW_KEYS], forbidden: ["render_authority", "marker_md", "source_replacement"] },
  { name: "v1 raw legacy marker", required: ["marker_id", "legacy_aliases", "render_authority", ...RAW_KEYS], forbidden: ["marker_md", "source_replacement"] },
  { name: "v1 semantic legacy marker", required: ["marker_id", "legacy_aliases", "render_authority", "marker_md"], optional: REPLACEMENT_CHILD, forbidden: ["source_block_md", "span_ids"] },
  { name: "v0 record", required: ["id", "title", "projection_group", ...RAW_KEYS], optional: ["legacy_aliases", "tags"], forbidden: ["render_authority", "semantic_shadow", "payload", "source_replacement"] },
  { name: "v1 raw record", required: ["id", "title", "projection_group", "render_authority", ...RAW_KEYS], optional: ["legacy_aliases", "tags", "semantic_shadow"], forbidden: ["payload", "source_replacement"] },
  { name: "v1 semantic record", required: ["id", "title", "projection_group", "render_authority", "payload"], optional: ["legacy_aliases", "tags", ...REPLACEMENT_CHILD], forbidden: ["source_block_md", "span_ids", "semantic_shadow"] },
  { name: "source replacement", required: ["span_id", "replacement_field", "review_note_md"] },
  { name: "v0 part", required: ["part_id", "parent_record_id", ...RAW_KEYS], optional: ["title"], forbidden: ["render_authority", "body_md", "source_replacement"] },
  { name: "v1 raw part", required: ["part_id", "parent_record_id", "render_authority", ...RAW_KEYS], optional: ["title"], forbidden: ["body_md", "source_replacement"] },
  { name: "v1 semantic part", required: ["part_id", "parent_record_id", "render_authority", "body_md"], optional: ["title", ...REPLACEMENT_CHILD], forbidden: ["source_block_md", "span_ids"] },
  { name: "generated slot", required: ["slot_id", "binding", "span_ids"] },
  { name: "manifest table", required: ["entry"] },
  { name: "manifest entry", required: ["kind"], optional: ["section_id", "fragment_id", "marker_id", "record_id", "part_id", "slot_id"] },
  { name: "source span", required: ["id", "start_byte", "end_byte", "sha256", "source_kind", "owner_id", "owner_field", "migration_status"] },
  { name: "relation", required: ["source", "kind", "target"], optional: ["note_md"] },
  { name: "reference discriminator", required: ["id", "source", "kind"], optional: ["target_id", "feature_id", "role_id", "cell_id", "gate_id", "test_id", "symbol", "path", "heading", "document", "passage", "repository", "issue", "commit", "project", "release", "consumer", "report_reference", "local_reference", "uncertainty_md", "expires_at"] },
] as const;

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
  unresolved_migration: ["local_reference", "uncertainty_md", "expires_at"],
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
    case "unresolved_migration": return `${reference.local_reference}\0${reference.expires_at}`;
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

function rawBytes(ctx: DecodeContext, table: object, path: string): { source_block_md: Uint8Array; span_ids: SpanId[] } {
  return {
    source_block_md: expectMarkdown(ctx, requiredValue(table, "source_block_md"), p(path, "source_block_md")),
    span_ids: spans(ctx, table, path),
  };
}

function decodeReplacement(ctx: DecodeContext, raw: unknown, path: string): SourceReplacement {
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[16]);
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
  position: SemanticDecodePosition,
): SemanticPayload {
  const pre = expectExactTable(ctx, raw, path, {
    name: "semantic payload discriminator",
    required: ["kind"],
    optional: raw !== null && typeof raw === "object" && !Array.isArray(raw) ? Object.keys(raw).filter((key) => key !== "kind") : [],
  });
  const kind = expectString(ctx, requiredValue(pre, "kind"), p(path, "kind"));
  const shared = decodeSharedSemanticPayload(ctx, raw, path, position);
  if (shared !== undefined) return shared;
  const domain = roadmap === "matrix"
    ? decodeMatrixPayload(ctx, raw, path, kind)
    : decodeTestingPayload(ctx, raw, path, kind);
  if (domain === undefined) {
    schemaFail(ctx, "E-SCHEMA-ENUM", p(path, "kind"), `${kind} is not a ${roadmap} semantic payload kind`);
  }
  return domain;
}

function decodeDocumentMeta(ctx: DecodeContext, raw: unknown): DocumentMetaV0 | DocumentMetaV1 {
  const pre = expectExactTable(ctx, raw, "document", {
    name: "document discriminator",
    required: ["schema_version"],
    optional: ["authority", "roadmap", "source_path", "projection_path", "frozen_source_sha256", "frozen_source_byte_length", "frozen_source_line_count", "frozen_source_eof", "frozen_legacy_span_ids"],
  });
  const versionRaw = requiredValue(pre, "schema_version");
  if (typeof versionRaw !== "number" || !Number.isSafeInteger(versionRaw)) {
    schemaFail(ctx, "E-SCHEMA-TYPE", "document.schema_version", "schema_version must be an integer");
  }
  if (versionRaw !== 0 && versionRaw !== 1) {
    schemaFail(ctx, "E-SCHEMA-VERSION", "document.schema_version", "WP1 accepts roadmap schema versions 0 and 1 only");
  }
  const table = expectExactTable(ctx, raw, "document", ROADMAP_SCHEMA_ROWS[versionRaw === 0 ? 2 : 3]);
  const common = {
    source_path: expectRepoPath(ctx, requiredValue(table, "source_path"), "document.source_path"),
    projection_path: expectRepoPath(ctx, requiredValue(table, "projection_path"), "document.projection_path"),
    frozen_source_sha256: expectSha256(ctx, requiredValue(table, "frozen_source_sha256"), "document.frozen_source_sha256"),
    frozen_source_byte_length: expectSafeInteger(ctx, requiredValue(table, "frozen_source_byte_length"), "document.frozen_source_byte_length"),
    frozen_source_line_count: expectSafeInteger(ctx, requiredValue(table, "frozen_source_line_count"), "document.frozen_source_line_count"),
    frozen_source_eof: expectEnum(ctx, requiredValue(table, "frozen_source_eof"), ["lf", "none"] as const, "document.frozen_source_eof"),
  };
  const roadmap = expectEnum(ctx, requiredValue(table, "roadmap"), ["matrix", "testing"] as const, "document.roadmap");
  if (versionRaw === 0) {
    return { schema_version: 0, authority: expectEnum(ctx, requiredValue(table, "authority"), ["shadow"] as const, "document.authority"), roadmap, ...common };
  }
  return {
    schema_version: 1,
    authority: expectEnum(ctx, requiredValue(table, "authority"), ["authoritative"] as const, "document.authority"),
    roadmap,
    ...common,
    frozen_legacy_span_ids: canonicalSet(
      ctx,
      expectArrayOf(ctx, requiredValue(table, "frozen_legacy_span_ids"), "document.frozen_legacy_span_ids", (entry, entryPath) => expectSubordinateId(ctx, entry, entryPath) as SpanId),
      "document.frozen_legacy_span_ids",
    ),
  };
}

function decodeV0Section(ctx: DecodeContext, raw: unknown, path: string): RawSectionV0 {
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[4]);
  return { section_id: expectSubordinateId(ctx, requiredValue(table, "section_id"), p(path, "section_id")) as SectionId, title: expectString(ctx, requiredValue(table, "title"), p(path, "title")), ...aliases(ctx, table, path), ...rawBytes(ctx, table, path) } as RawSectionV0;
}

function decodeV1Section(ctx: DecodeContext, raw: unknown, path: string): RawSectionV1 | SemanticSectionV1 {
  const pre = expectExactTable(ctx, raw, path, { name: "v1 section discriminator", required: ["section_id", "title", "render_authority"], optional: ["legacy_aliases", "source_block_md", "span_ids", "body_md", "source_replacement"] });
  const authority = expectEnum(ctx, requiredValue(pre, "render_authority"), ["raw", "semantic"] as const, p(path, "render_authority"));
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[authority === "raw" ? 5 : 6]);
  const base = { section_id: expectSubordinateId(ctx, requiredValue(table, "section_id"), p(path, "section_id")) as SectionId, title: expectString(ctx, requiredValue(table, "title"), p(path, "title")), ...aliases(ctx, table, path), render_authority: authority };
  return authority === "raw" ? { ...base, render_authority: authority, ...rawBytes(ctx, table, path) } : { ...base, render_authority: authority, body_md: expectMarkdown(ctx, requiredValue(table, "body_md"), p(path, "body_md")), source_replacements: replacements(ctx, table, path) };
}

function decodeV0Fragment(ctx: DecodeContext, raw: unknown, path: string): RawFragmentV0 {
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[7]);
  return { fragment_id: expectSubordinateId(ctx, requiredValue(table, "fragment_id"), p(path, "fragment_id")) as FragmentId, projection_group: expectSubordinateId(ctx, requiredValue(table, "projection_group"), p(path, "projection_group")) as SectionId, ...title(ctx, table, path), ...aliases(ctx, table, path), ...rawBytes(ctx, table, path) } as RawFragmentV0;
}

function decodeV1Fragment(ctx: DecodeContext, raw: unknown, path: string): RawFragmentV1 | SemanticFragmentV1 {
  const pre = expectExactTable(ctx, raw, path, { name: "v1 fragment discriminator", required: ["fragment_id", "projection_group", "render_authority"], optional: ["title", "legacy_aliases", "source_block_md", "span_ids", "body_md", "source_replacement"] });
  const authority = expectEnum(ctx, requiredValue(pre, "render_authority"), ["raw", "semantic"] as const, p(path, "render_authority"));
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[authority === "raw" ? 8 : 9]);
  const base = { fragment_id: expectSubordinateId(ctx, requiredValue(table, "fragment_id"), p(path, "fragment_id")) as FragmentId, projection_group: expectSubordinateId(ctx, requiredValue(table, "projection_group"), p(path, "projection_group")) as SectionId, ...title(ctx, table, path), ...aliases(ctx, table, path), render_authority: authority };
  return authority === "raw" ? { ...base, render_authority: authority, ...rawBytes(ctx, table, path) } : { ...base, render_authority: authority, body_md: expectMarkdown(ctx, requiredValue(table, "body_md"), p(path, "body_md")), source_replacements: replacements(ctx, table, path) };
}

function decodeV0Marker(ctx: DecodeContext, raw: unknown, path: string): RawLegacyMarkerV0 {
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[10]);
  return { marker_id: expectSubordinateId(ctx, requiredValue(table, "marker_id"), p(path, "marker_id")) as MarkerId, legacy_aliases: expectStringSet(ctx, requiredValue(table, "legacy_aliases"), p(path, "legacy_aliases"), true), ...rawBytes(ctx, table, path) };
}

function decodeV1Marker(ctx: DecodeContext, raw: unknown, path: string): RawLegacyMarkerV1 | SemanticLegacyMarkerV1 {
  const pre = expectExactTable(ctx, raw, path, { name: "v1 legacy marker discriminator", required: ["marker_id", "legacy_aliases", "render_authority"], optional: ["source_block_md", "span_ids", "marker_md", "source_replacement"] });
  const authority = expectEnum(ctx, requiredValue(pre, "render_authority"), ["raw", "semantic"] as const, p(path, "render_authority"));
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[authority === "raw" ? 11 : 12]);
  const base = { marker_id: expectSubordinateId(ctx, requiredValue(table, "marker_id"), p(path, "marker_id")) as MarkerId, legacy_aliases: expectStringSet(ctx, requiredValue(table, "legacy_aliases"), p(path, "legacy_aliases"), true), render_authority: authority };
  return authority === "raw" ? { ...base, render_authority: authority, ...rawBytes(ctx, table, path) } : { ...base, render_authority: authority, marker_md: expectMarkdown(ctx, requiredValue(table, "marker_md"), p(path, "marker_md")), source_replacements: replacements(ctx, table, path) };
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

function decodeV0Record(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): RawRecordV0 {
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[13]);
  return { ...envelope(ctx, table, path, roadmap), ...rawBytes(ctx, table, path) } as RawRecordV0;
}

function decodeV1Record(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): RawAuthorityRecordV1 | SemanticAuthorityRecordV1 {
  const pre = expectExactTable(ctx, raw, path, { name: "v1 record discriminator", required: ["id", "title", "projection_group", "render_authority"], optional: ["legacy_aliases", "tags", "source_block_md", "span_ids", "semantic_shadow", "payload", "source_replacement"] });
  const authority = expectEnum(ctx, requiredValue(pre, "render_authority"), ["raw", "semantic"] as const, p(path, "render_authority"));
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[authority === "raw" ? 14 : 15]);
  const base = { ...envelope(ctx, table, path, roadmap), render_authority: authority };
  if (authority === "raw") {
    return {
      ...base,
      render_authority: authority,
      ...rawBytes(ctx, table, path),
      ...(hasOwn(table, "semantic_shadow") ? { semantic_shadow: decodeSemanticPayload(ctx, optionalValue(table, "semantic_shadow"), p(path, "semantic_shadow"), roadmap, "shadow") } : {}),
    } as RawAuthorityRecordV1;
  }
  return {
    ...base,
    render_authority: authority,
    payload: decodeSemanticPayload(ctx, requiredValue(table, "payload"), p(path, "payload"), roadmap, "authority"),
    source_replacements: replacements(ctx, table, path),
  } as SemanticAuthorityRecordV1;
}

function decodeV0Part(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): RawPartV0 {
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[17]);
  return { part_id: expectSubordinateId(ctx, requiredValue(table, "part_id"), p(path, "part_id")) as PartId, parent_record_id: expectRoadmapId(ctx, requiredValue(table, "parent_record_id"), p(path, "parent_record_id"), roadmap), ...title(ctx, table, path), ...rawBytes(ctx, table, path) } as RawPartV0;
}

function decodeV1Part(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): RawPartV1 | SemanticPartV1 {
  const pre = expectExactTable(ctx, raw, path, { name: "v1 part discriminator", required: ["part_id", "parent_record_id", "render_authority"], optional: ["title", "source_block_md", "span_ids", "body_md", "source_replacement"] });
  const authority = expectEnum(ctx, requiredValue(pre, "render_authority"), ["raw", "semantic"] as const, p(path, "render_authority"));
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[authority === "raw" ? 18 : 19]);
  const base = { part_id: expectSubordinateId(ctx, requiredValue(table, "part_id"), p(path, "part_id")) as PartId, parent_record_id: expectRoadmapId(ctx, requiredValue(table, "parent_record_id"), p(path, "parent_record_id"), roadmap), ...title(ctx, table, path), render_authority: authority };
  return authority === "raw" ? { ...base, render_authority: authority, ...rawBytes(ctx, table, path) } : { ...base, render_authority: authority, body_md: expectMarkdown(ctx, requiredValue(table, "body_md"), p(path, "body_md")), source_replacements: replacements(ctx, table, path) };
}

function decodeSlot(ctx: DecodeContext, raw: unknown, path: string): GeneratedSlot {
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[20]);
  return { slot_id: expectSubordinateId(ctx, requiredValue(table, "slot_id"), p(path, "slot_id")) as SlotId, binding: expectString(ctx, requiredValue(table, "binding"), p(path, "binding")), span_ids: spans(ctx, table, path) };
}

function decodeManifest(ctx: DecodeContext, raw: unknown): ManifestEntry[] {
  const table = expectExactTable(ctx, raw, "manifest", ROADMAP_SCHEMA_ROWS[21]);
  return expectNonemptyArray(ctx, expectArrayOf(ctx, requiredValue(table, "entry"), "manifest.entry", (entry, path) => {
    const row = expectExactTable(ctx, entry, path, ROADMAP_SCHEMA_ROWS[22]);
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
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[23]);
  return { id: expectSubordinateId(ctx, requiredValue(table, "id"), p(path, "id")) as SpanId, start_byte: expectSafeInteger(ctx, requiredValue(table, "start_byte"), p(path, "start_byte")), end_byte: expectSafeInteger(ctx, requiredValue(table, "end_byte"), p(path, "end_byte")), sha256: expectSha256(ctx, requiredValue(table, "sha256"), p(path, "sha256")), source_kind: expectEnum(ctx, requiredValue(table, "source_kind"), STRUCTURAL_KINDS, p(path, "source_kind")), owner_id: expectString(ctx, requiredValue(table, "owner_id"), p(path, "owner_id")), owner_field: expectString(ctx, requiredValue(table, "owner_field"), p(path, "owner_field")), migration_status: expectEnum(ctx, requiredValue(table, "migration_status"), ["raw", "replaced", "generated"] as const, p(path, "migration_status")) };
}

function decodeRelation(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): Relation {
  const table = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[24]);
  return { source: expectRoadmapId(ctx, requiredValue(table, "source"), p(path, "source"), roadmap), kind: expectEnum(ctx, requiredValue(table, "kind"), ["parent_of", "depends_on", "blocked_by", "supersedes", "split_from", "reopens", "overlaps", "complements", "related", "delegates_to"] as const, p(path, "kind")), target: expectRoadmapId(ctx, requiredValue(table, "target"), p(path, "target")), ...(hasOwn(table, "note_md") ? { note_md: expectMarkdown(ctx, optionalValue(table, "note_md"), p(path, "note_md")) } : {}) };
}

function decodeReference(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): Reference {
  const pre = expectExactTable(ctx, raw, path, ROADMAP_SCHEMA_ROWS[25]);
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
    case "unresolved_migration": return { ...base, kind, local_reference: string("local_reference"), uncertainty_md: expectMarkdown(ctx, requiredValue(table, "uncertainty_md"), p(path, "uncertainty_md")), expires_at: string("expires_at") };
  }
}

function optionalRows<T>(ctx: DecodeContext, root: object, key: string, decode: (raw: unknown, path: string) => T): T[] {
  return hasOwn(root, key)
    ? expectArrayOf(ctx, optionalValue(root, key), key, (entry, entryPath) => decode(entry, entryPath))
    : [];
}

function assertFrozenRawSpans(ctx: DecodeContext, doc: RoadmapDocumentV1): void {
  const frozen = new Set(doc.document.frozen_legacy_span_ids);
  const rawOwners = [
    ...doc.sections.filter((owner): owner is RawSectionV1 => owner.render_authority === "raw"),
    ...doc.fragments.filter((owner): owner is RawFragmentV1 => owner.render_authority === "raw"),
    ...doc.legacy_markers.filter((owner): owner is RawLegacyMarkerV1 => owner.render_authority === "raw"),
    ...doc.records.filter((owner): owner is RawAuthorityRecordV1 => owner.render_authority === "raw"),
    ...doc.parts.filter((owner): owner is RawPartV1 => owner.render_authority === "raw"),
  ];
  for (const owner of rawOwners) {
    for (const span of owner.span_ids) {
      if (!frozen.has(span)) schemaFail(ctx, "E-SCHEMA-STATE", "document.frozen_legacy_span_ids", `raw v1 span ${span} is not frozen legacy source`);
    }
  }
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

function assertDecodedDomainJoins(ctx: DecodeContext, doc: RoadmapDocumentV1): void {
  const payloads = new Map<string, SemanticPayload>();
  for (const record of doc.records) {
    const payload = record.render_authority === "semantic" ? record.payload : record.semantic_shadow;
    if (payload !== undefined) payloads.set(record.id, payload);
  }

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
  const root = expectExactTable(ctx, bindings.parsed, "$", ROADMAP_SCHEMA_ROWS[meta.schema_version]);
  const roadmap = meta.roadmap;
  if (meta.schema_version === 0) {
    const doc: RoadmapDocumentV0 = {
      document: meta,
      sections: sortBy(optionalRows(ctx, root, "section", (raw, path) => decodeV0Section(ctx, raw, path)), (value) => value.section_id),
      fragments: sortBy(optionalRows(ctx, root, "fragment", (raw, path) => decodeV0Fragment(ctx, raw, path)), (value) => value.fragment_id),
      legacy_markers: sortBy(optionalRows(ctx, root, "legacy_marker", (raw, path) => decodeV0Marker(ctx, raw, path)), (value) => value.marker_id),
      records: sortBy(optionalRows(ctx, root, "record", (raw, path) => decodeV0Record(ctx, raw, path, roadmap)), (value) => value.id),
      parts: sortBy(optionalRows(ctx, root, "part", (raw, path) => decodeV0Part(ctx, raw, path, roadmap)), (value) => value.part_id),
      generated_slots: sortBy(optionalRows(ctx, root, "generated_slot", (raw, path) => decodeSlot(ctx, raw, path)), (value) => value.slot_id),
      manifest: decodeManifest(ctx, requiredValue(root, "manifest")),
      spans: optionalRows(ctx, root, "source_span", (raw, path) => decodeSourceSpan(ctx, raw, path)).sort((left, right) => left.start_byte - right.start_byte),
    };
    if (doc.sections.length === 0 || doc.records.length === 0 || doc.spans.length === 0) schemaFail(ctx, "E-SCHEMA-FLOOR", "$", "roadmap requires at least one section, record, manifest entry, and source span");
    assertSpanBounds(ctx, doc);
    bindings.assertAllConsumed();
    return doc;
  }
  const relations = sortBy(
    optionalRows(ctx, root, "relation", (raw, path) => decodeRelation(ctx, raw, path, roadmap)),
    (relation) => `${relation.source}\0${relation.kind}\0${relation.target}`,
  );
  const references = sortBy(
    optionalRows(ctx, root, "reference", (raw, path) => decodeReference(ctx, raw, path, roadmap)),
    (reference) => `${reference.source}\0${reference.kind}\0${referenceTuple(reference)}`,
  );
  const doc: RoadmapDocumentV1 = {
    document: meta,
    sections: sortBy(optionalRows(ctx, root, "section", (raw, path) => decodeV1Section(ctx, raw, path)), (value) => value.section_id),
    fragments: sortBy(optionalRows(ctx, root, "fragment", (raw, path) => decodeV1Fragment(ctx, raw, path)), (value) => value.fragment_id),
    legacy_markers: sortBy(optionalRows(ctx, root, "legacy_marker", (raw, path) => decodeV1Marker(ctx, raw, path)), (value) => value.marker_id),
    records: sortBy(optionalRows(ctx, root, "record", (raw, path) => decodeV1Record(ctx, raw, path, roadmap)), (value) => value.id),
    parts: sortBy(optionalRows(ctx, root, "part", (raw, path) => decodeV1Part(ctx, raw, path, roadmap)), (value) => value.part_id),
    generated_slots: sortBy(optionalRows(ctx, root, "generated_slot", (raw, path) => decodeSlot(ctx, raw, path)), (value) => value.slot_id),
    manifest: decodeManifest(ctx, requiredValue(root, "manifest")),
    spans: optionalRows(ctx, root, "source_span", (raw, path) => decodeSourceSpan(ctx, raw, path)).sort((left, right) => left.start_byte - right.start_byte),
    relations,
    references,
  };
  if (doc.sections.length === 0 || doc.records.length === 0 || doc.spans.length === 0) schemaFail(ctx, "E-SCHEMA-FLOOR", "$", "roadmap requires at least one section, record, manifest entry, and source span");
  assertSpanBounds(ctx, doc);
  assertFrozenRawSpans(ctx, doc);
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
