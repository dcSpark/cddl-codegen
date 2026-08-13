import { bytesEqual, RoadmapWireError } from "../markdown_codec.ts";
import type {
  DocumentMetaV3,
  GeneratedSlot,
  RenderNodeKind,
  Reference,
  Relation,
  RoadmapDocument,
  RoadmapDocumentV3,
  SemanticAuthorityRecord,
  SemanticPart,
  SemanticPayload,
  SemanticSection,
} from "../model/documents.ts";
import type {
  PartId,
  SectionId,
  SlotId,
  RoadmapName,
} from "../model/core.ts";
import { composeRoadmapDocument } from "../compose.ts";
import { decodeMatrixPayload } from "./matrix.ts";
import {
  childLogicalPath as p,
  expectArrayOf,
  expectEnum,
  expectExactTable,
  expectMarkdown,
  expectNonemptyArray,
  expectReferenceId,
  expectRepoPath,
  expectRoadmapId,
  expectString,
  expectStringSet,
  expectSubordinateId,
  hasOwn,
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


export const ROADMAP_ENUM_FIELDS: readonly EnumSchemaField[] = [
  { name: "schema_version", values: ["3"] },
  { name: "roadmap", values: ["matrix", "testing"] },
  { name: "relation_kind", values: ["parent_of", "depends_on", "blocked_by", "supersedes", "split_from", "reopens", "overlaps", "complements", "related", "delegates_to"] },
  { name: "reference_kind", values: ["roadmap", "matrix_feature", "matrix_role", "matrix_cell", "gate", "test_symbol", "file_heading", "spec_passage", "external_issue", "external_commit", "external_release", "consumer_report"] },
] as const;

/**
 * The exact-key rows of the sole supported wire schema, addressed by name. Positional indexing is
 * deliberately absent: a row list that is edited by hand cannot safely be addressed by offset.
 */
export const ROADMAP_ROW = {
  root: { name: "roadmap v3 root", required: ["document", "section", "record"], optional: ["part", "relation", "reference"] },
  document: { name: "roadmap v3 document", required: ["schema_version", "roadmap", "source_path", "projection_path"] },
  section: { name: "semantic section", required: ["section_id", "title", "body_md", "entries"], optional: ["legacy_aliases", "slots"] },
  section_slot: { name: "section slot declaration", required: ["binding"] },
  record: { name: "semantic record", required: ["id", "title", "payload"], optional: ["legacy_aliases", "tags"] },
  part: { name: "semantic part", required: ["part_id", "parent_record_id", "body_md"], optional: ["title"] },
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

function decodeDocumentMeta(ctx: DecodeContext, raw: unknown): DocumentMetaV3 {
  const pre = expectExactTable(ctx, raw, "document", {
    name: "document discriminator",
    required: ["schema_version"],
    optional: ["roadmap", "source_path", "projection_path"],
  });
  const versionRaw = requiredValue(pre, "schema_version");
  if (typeof versionRaw !== "number" || !Number.isSafeInteger(versionRaw)) {
    schemaFail(ctx, "E-SCHEMA-TYPE", "document.schema_version", "schema_version must be an integer");
  }
  if (versionRaw !== 3) {
    schemaFail(ctx, "E-SCHEMA-VERSION", "document.schema_version", "roadmap schema_version must be 3");
  }
  const table = expectExactTable(ctx, raw, "document", ROADMAP_ROW.document);
  return {
    schema_version: 3,
    roadmap: expectEnum(ctx, requiredValue(table, "roadmap"), ["matrix", "testing"] as const, "document.roadmap"),
    source_path: expectRepoPath(ctx, requiredValue(table, "source_path"), "document.source_path"),
    projection_path: expectRepoPath(ctx, requiredValue(table, "projection_path"), "document.projection_path"),
  };
}

function decodeSectionSlots(ctx: DecodeContext, raw: unknown, path: string): GeneratedSlot[] {
  const slotPath = p(path, "slots");
  if (raw === null || typeof raw !== "object" || Array.isArray(raw)) {
    schemaFail(ctx, "E-SCHEMA-TYPE", slotPath, "section slots must be a table of slot declarations");
  }
  const declared = Object.keys(raw as object);
  if (declared.length === 0) {
    schemaFail(ctx, "E-SCHEMA-FLOOR", slotPath, "section slots table cannot be empty");
  }
  return declared.map((key) => {
    const entryPath = p(slotPath, key);
    const slotId = expectSubordinateId(ctx, key, entryPath) as SlotId;
    const table = expectExactTable(ctx, (raw as Record<string, unknown>)[key], entryPath, ROADMAP_ROW.section_slot);
    return { slot_id: slotId, binding: expectString(ctx, requiredValue(table, "binding"), p(entryPath, "binding")) };
  }).sort((left, right) => (left.slot_id < right.slot_id ? -1 : left.slot_id > right.slot_id ? 1 : 0));
}

function decodeSection(ctx: DecodeContext, raw: unknown, path: string): SemanticSection {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.section);
  return {
    section_id: expectSubordinateId(ctx, requiredValue(table, "section_id"), p(path, "section_id")) as SectionId,
    title: expectString(ctx, requiredValue(table, "title"), p(path, "title")),
    ...aliases(ctx, table, path),
    body_md: expectMarkdown(ctx, requiredValue(table, "body_md"), p(path, "body_md")),
    entries: expectArrayOf(
      ctx,
      requiredValue(table, "entries"),
      p(path, "entries"),
      (entry, entryPath) => expectString(ctx, entry, entryPath),
    ),
    ...(hasOwn(table, "slots") ? { slots: decodeSectionSlots(ctx, optionalValue(table, "slots"), path) } : {}),
  };
}

function envelope(ctx: DecodeContext, table: object, path: string, roadmap: RoadmapName): object {
  return {
    id: expectRoadmapId(ctx, requiredValue(table, "id"), p(path, "id"), roadmap),
    title: expectString(ctx, requiredValue(table, "title"), p(path, "title")),
    ...aliases(ctx, table, path),
    ...(hasOwn(table, "tags") ? { tags: expectStringSet(ctx, optionalValue(table, "tags"), p(path, "tags")) } : {}),
  };
}

function decodeRecord(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): SemanticAuthorityRecord {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.record);
  return {
    ...envelope(ctx, table, path, roadmap),
    payload: decodeSemanticPayload(ctx, requiredValue(table, "payload"), p(path, "payload"), roadmap),
  } as SemanticAuthorityRecord;
}

function decodePart(ctx: DecodeContext, raw: unknown, path: string, roadmap: RoadmapName): SemanticPart {
  const table = expectExactTable(ctx, raw, path, ROADMAP_ROW.part);
  return {
    part_id: expectSubordinateId(ctx, requiredValue(table, "part_id"), p(path, "part_id")) as PartId,
    parent_record_id: expectRoadmapId(ctx, requiredValue(table, "parent_record_id"), p(path, "parent_record_id"), roadmap),
    ...title(ctx, table, path),
    body_md: expectMarkdown(ctx, requiredValue(table, "body_md"), p(path, "body_md")),
  };
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

function assertDecodedDomainJoins(ctx: DecodeContext, doc: RoadmapDocumentV3): void {
  const payloads = new Map<string, SemanticPayload>(doc.records.map((record) => [record.id, record.payload]));

  for (const record of doc.records) {
    const payload = payloads.get(record.id);
    if (payload?.kind === "work") {
      // A parked state cannot carry a nested transition whose evaluation already reads met.
      for (const field of ["promotion_trigger", "reopening_signal", "unblock_predicate", "retirement_predicate"] as const) {
        const nested = (payload as unknown as Record<string, { evaluation?: string } | undefined>)[field];
        if (nested !== undefined && nested.evaluation === "met") {
          schemaFail(ctx, "E-SCHEMA-STATE", `record.${record.id}.${field}`, `work cannot park already-fired nested ${field}`);
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
  const rootPre = expectExactTable(ctx, bindings.parsed, "$", { name: "roadmap root discriminator", required: ["document"], optional: ["section", "record", "part", "relation", "reference"] });
  const meta = decodeDocumentMeta(ctx, requiredValue(rootPre, "document"));
  if (expectedRoadmap !== undefined && meta.roadmap !== expectedRoadmap) schemaFail(ctx, "E-ID-NAMESPACE", "document.roadmap", `expected ${expectedRoadmap} roadmap source`);
  const root = expectExactTable(ctx, bindings.parsed, "$", ROADMAP_ROW.root);
  const roadmap = meta.roadmap;
  const doc: RoadmapDocumentV3 = {
    document: meta,
    // Section order is authored presentation order; unlike every other table it is never sorted.
    sections: optionalRows(ctx, root, "section", (raw, path) => decodeSection(ctx, raw, path)),
    records: sortBy(optionalRows(ctx, root, "record", (raw, path) => decodeRecord(ctx, raw, path, roadmap)), (value) => value.id),
    parts: sortBy(optionalRows(ctx, root, "part", (raw, path) => decodePart(ctx, raw, path, roadmap)), (value) => value.part_id),
    relations: sortBy(
      optionalRows(ctx, root, "relation", (raw, path) => decodeRelation(ctx, raw, path, roadmap)),
      (relation) => `${relation.source}\0${relation.kind}\0${relation.target}`,
    ),
    references: sortBy(
      optionalRows(ctx, root, "reference", (raw, path) => decodeReference(ctx, raw, path, roadmap)),
      (reference) => `${reference.source}\0${reference.kind}\0${referenceTuple(reference)}`,
    ),
  };
  if (doc.sections.length === 0 || doc.records.length === 0) schemaFail(ctx, "E-SCHEMA-FLOOR", "$", "roadmap requires at least one section and one record");
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
