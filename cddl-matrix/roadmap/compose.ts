import type {
  ManifestEntry,
  Part,
  RecordNode,
  Reference,
  Relation,
  RoadmapDocument,
  Section,
  SemanticPayload,
} from "./model/documents.ts";
import {
  armOfGroupValue,
  armOfPayload,
  fieldProperty,
  type PayloadArm,
  type PayloadField,
} from "./payload_descriptors.ts";
import { CanonicalTomlWriter } from "./toml_writer.ts";

const compare = (left: string, right: string): number => (left < right ? -1 : left > right ? 1 : 0);
const sorted = <T>(items: readonly T[], key: (item: T) => string): T[] =>
  [...items].sort((left, right) => compare(key(left), key(right)));

function optionalString(writer: CanonicalTomlWriter, key: string, value: string | undefined): void {
  if (value !== undefined) writer.string(key, value);
}

function optionalMarkdown(writer: CanonicalTomlWriter, key: string, value: Uint8Array | undefined): void {
  if (value !== undefined) writer.markdown(key, value);
}

function optionalStrings(
  writer: CanonicalTomlWriter,
  key: string,
  value: readonly string[] | undefined,
  sort = true,
): void {
  if (value !== undefined) writer.strings(key, value, sort);
}

/**
 * Descriptor-driven payload emission: the arm's field list IS the canonical TOML order, and each
 * field's value kind selects the writer form.  Nested single tables and array tables recurse with
 * an extended header prefix, exactly as the retired per-kind writers spelled by hand.
 */
function writeFieldValue(
  writer: CanonicalTomlWriter,
  field: PayloadField,
  value: unknown,
  prefix: string,
): void {
  switch (field.value.t) {
    case "kind":
    case "enum":
    case "string":
    case "slug":
    case "commit":
    case "civil_date":
    case "roadmap_id":
    case "reference_id":
      writer.string(field.name, value as string);
      return;
    case "markdown":
      writer.markdown(field.name, value as Uint8Array);
      return;
    case "number":
      writer.number(field.name, value as number);
      return;
    case "string_set":
    case "roadmap_id_set":
    case "reference_id_set":
      writer.strings(field.name, value as readonly string[], true);
      return;
    case "table": {
      const header = `${prefix}.${field.name}`;
      writer.table(header);
      writeArmFields(writer, value as object, armOfGroupValue(field.value.group, value), header);
      return;
    }
    case "array_table": {
      const header = `${prefix}.${field.name}`;
      const flatten = field.value.flatten;
      for (const element of value as readonly unknown[]) {
        writer.arrayTable(header);
        if (flatten !== undefined) {
          writer.string(flatten, element as string);
          continue;
        }
        writeArmFields(writer, element as object, armOfGroupValue(field.value.group, element), header);
      }
      return;
    }
  }
}

function writeArmFields(
  writer: CanonicalTomlWriter,
  value: object,
  arm: PayloadArm,
  prefix: string,
): void {
  for (const field of arm.fields) {
    const fieldValue = (value as Record<string, unknown>)[fieldProperty(field)];
    if (field.presence === "optional" && fieldValue === undefined) continue;
    writeFieldValue(writer, field, fieldValue, prefix);
  }
}

function writeSemanticPayload(
  writer: CanonicalTomlWriter,
  payload: SemanticPayload,
  prefix: string,
): void {
  writeArmFields(writer, payload, armOfPayload(payload), prefix);
}

function writeManifestEntry(writer: CanonicalTomlWriter, entry: ManifestEntry): void {
  writer.arrayTable("manifest.entry");
  writer.string("kind", entry.kind);
  switch (entry.kind) {
    case "section": writer.string("section_id", entry.section_id); break;
    case "record": writer.string("record_id", entry.record_id); break;
    case "part": writer.string("part_id", entry.part_id); break;
  }
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

function writeReference(writer: CanonicalTomlWriter, reference: Reference): void {
  writer.arrayTable("reference");
  writer.string("id", reference.id);
  writer.string("source", reference.source);
  writer.string("kind", reference.kind);
  switch (reference.kind) {
    case "roadmap": writer.string("target_id", reference.target_id); break;
    case "matrix_feature": writer.string("feature_id", reference.feature_id); break;
    case "matrix_role": writer.string("role_id", reference.role_id); break;
    case "matrix_cell": writer.string("cell_id", reference.cell_id); break;
    case "gate": writer.string("gate_id", reference.gate_id); break;
    case "test_symbol": writer.string("test_id", reference.test_id); writer.string("symbol", reference.symbol); break;
    case "file_heading": writer.string("path", reference.path); writer.string("heading", reference.heading); break;
    case "spec_passage": writer.string("document", reference.document); writer.string("passage", reference.passage); break;
    case "external_issue": writer.string("repository", reference.repository); writer.string("issue", reference.issue); break;
    case "external_commit": writer.string("repository", reference.repository); writer.string("commit", reference.commit); break;
    case "external_release": writer.string("project", reference.project); writer.string("release", reference.release); break;
    case "consumer_report": writer.string("consumer", reference.consumer); writer.string("report_reference", reference.report_reference); break;
  }
}

export function composeRoadmapDocument(document: RoadmapDocument): Uint8Array {
  const writer = new CanonicalTomlWriter();
  const meta = document.document;
  writer.table("document");
  writer.integer("schema_version", meta.schema_version);
  writer.string("roadmap", meta.roadmap);
  writer.string("source_path", meta.source_path);
  writer.string("projection_path", meta.projection_path);

  const sections: readonly Section[] = document.sections;
  for (const section of sorted(sections, (value) => value.section_id)) {
    writer.arrayTable("section");
    writer.string("section_id", section.section_id);
    writer.string("title", section.title);
    optionalStrings(writer, "legacy_aliases", section.legacy_aliases);
    writer.markdown("body_md", section.body_md);
    for (const slot of sorted(section.slots ?? [], (value) => value.slot_id)) {
      writer.table(`section.slots.${slot.slot_id}`);
      writer.string("binding", slot.binding);
    }
  }

  const records: readonly RecordNode[] = document.records;
  for (const record of sorted(records, (value) => value.id)) {
    writer.arrayTable("record");
    writer.string("id", record.id);
    writer.string("title", record.title);
    writer.string("projection_group", record.projection_group);
    optionalStrings(writer, "legacy_aliases", record.legacy_aliases);
    optionalStrings(writer, "tags", record.tags);
    writer.table("record.payload");
    writeSemanticPayload(writer, record.payload, "record.payload");
  }

  const parts: readonly Part[] = document.parts;
  for (const part of sorted(parts, (value) => value.part_id)) {
    writer.arrayTable("part");
    writer.string("part_id", part.part_id);
    writer.string("parent_record_id", part.parent_record_id);
    optionalString(writer, "title", part.title);
    writer.markdown("body_md", part.body_md);
  }

  for (const entry of document.manifest) writeManifestEntry(writer, entry);
  {
    const relations = [...document.relations].sort((left, right) =>
      compare(`${left.source}\0${left.kind}\0${left.target}`, `${right.source}\0${right.kind}\0${right.target}`),
    );
    for (const relation of relations) {
      writer.arrayTable("relation");
      writer.string("source", relation.source);
      writer.string("kind", relation.kind);
      writer.string("target", relation.target);
      optionalMarkdown(writer, "note_md", relation.note_md);
    }
    const references = [...document.references].sort((left, right) =>
      compare(
        `${left.source}\0${left.kind}\0${referenceTuple(left)}`,
        `${right.source}\0${right.kind}\0${referenceTuple(right)}`,
      ),
    );
    for (const reference of references) writeReference(writer, reference);
  }
  return writer.finish();
}

export type CanonicalTomlDocument = RoadmapDocument;

export function composeCanonicalDocument(document: CanonicalTomlDocument): Uint8Array {
  return composeRoadmapDocument(document);
}
