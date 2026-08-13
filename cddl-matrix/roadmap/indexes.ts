import type { Indexes as AdapterIndexes } from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import type {
  PartId,
  ReferenceId,
  RoadmapId,
  RoadmapName,
  SectionId,
  SlotId,
} from "./model/core.ts";
import type {
  GeneratedSlot,
  Part,
  RecordNode,
  Reference,
  Relation,
  RoadmapDocument,
  Section,
  SemanticPayload,
  SemanticRecord,
} from "./model/documents.ts";
import { validateReferenceId, validateRoadmapId, validateSubordinateId } from "./ids.ts";
import {
  armOfGroupValue,
  armOfPayload,
  fieldProperty,
  type PayloadArm,
} from "./payload_descriptors.ts";
import { documentSlots } from "./slots.ts";
import { codePointSort } from "./kernel.ts";
import { sortRoadmapIssues as sortIssues } from "./errors.ts";

export type FirstClassIdProviderKind = "record";

export type FirstClassIdProviderValue = RecordNode;

export interface RoadmapIdProviderFact {
  readonly id: RoadmapId;
  readonly namespace: RoadmapName;
  readonly kind: FirstClassIdProviderKind;
  readonly owner_record_id: RoadmapId;
  readonly logical_path: string;
  readonly value: FirstClassIdProviderValue;
}

export type AliasOwnerKind = "section" | "record";

export interface AliasProviderFact {
  readonly alias: string;
  readonly namespace: RoadmapName;
  readonly owner_kind: AliasOwnerKind;
  readonly owner_id: string;
  readonly logical_path: string;
}

export type SubordinateIdProviderKind =
  | "section"
  | "part"
  | "generated_slot"
  | "reference";

export type SubordinateIdProviderValue =
  | Section
  | Part
  | GeneratedSlot
  | Reference;

export interface SubordinateIdProviderFact {
  readonly id: string;
  readonly namespace: RoadmapName;
  readonly kind: SubordinateIdProviderKind;
  readonly logical_path: string;
  readonly value: SubordinateIdProviderValue;
}

export type RoadmapIdUseRole =
  | "provider"
  | "parent_record"
  | "manifest_record"
  | "relation_source"
  | "relation_target"
  | "reference_source"
  | "reference_target"
  | "semantic_target";

export interface RoadmapIdUseFact {
  readonly id: RoadmapId;
  readonly logical_path: string;
  readonly role: RoadmapIdUseRole;
  readonly expected_namespace?: RoadmapName;
}

export interface ReferenceIdUseFact {
  readonly id: ReferenceId;
  readonly logical_path: string;
}

export interface SemanticPayloadProviderFact {
  readonly record: RecordNode;
  readonly payload: SemanticPayload;
  readonly authority: "semantic";
  readonly logical_path: string;
}

/** Inputs consumed later by cross-document identity normalization; this layer assigns no policy. */
export interface DocumentIdentityInputs {
  readonly namespace: RoadmapName;
  readonly id_providers: readonly RoadmapIdProviderFact[];
  readonly alias_providers: readonly AliasProviderFact[];
}

export interface RoadmapIndexes extends AdapterIndexes {
  readonly roadmap: RoadmapName;
  readonly id_providers: readonly RoadmapIdProviderFact[];
  readonly id_provider_claims: ReadonlyMap<RoadmapId, readonly RoadmapIdProviderFact[]>;
  readonly first_class: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>;
  readonly id_uses: readonly RoadmapIdUseFact[];
  readonly reference_id_uses: readonly ReferenceIdUseFact[];
  readonly alias_providers: readonly AliasProviderFact[];
  readonly aliases: ReadonlyMap<string, readonly AliasProviderFact[]>;
  readonly subordinate_id_providers: readonly SubordinateIdProviderFact[];
  readonly subordinate_id_claims: ReadonlyMap<
    SubordinateIdProviderKind,
    ReadonlyMap<string, readonly SubordinateIdProviderFact[]>
  >;
  readonly record_nodes: ReadonlyMap<RoadmapId, RecordNode>;
  readonly payload_records: ReadonlyMap<RoadmapId, SemanticPayloadProviderFact>;
  readonly evidence_records: ReadonlyMap<RoadmapId, SemanticPayloadProviderFact>;
  readonly sections: ReadonlyMap<SectionId, Section>;
  readonly parts: ReadonlyMap<PartId, Part>;
  readonly generated_slots: ReadonlyMap<SlotId, GeneratedSlot>;
  readonly relations: readonly Relation[];
  readonly relations_by_source: ReadonlyMap<RoadmapId, readonly Relation[]>;
  readonly relations_by_target: ReadonlyMap<RoadmapId, readonly Relation[]>;
  readonly identity_inputs: DocumentIdentityInputs;
}

export interface RoadmapIndexBuildResult {
  readonly indexes: RoadmapIndexes;
  readonly issues: readonly RoadmapIssue[];
}

const SUBORDINATE_ID_FIELD: Readonly<Record<SubordinateIdProviderKind, string>> = {
  section: "section_id",
  part: "part_id",
  generated_slot: "slot_id",
  reference: "id",
};

function quoted(value: string): string {
  return JSON.stringify(value);
}

function recordPath(id: RoadmapId): string {
  return `record[${quoted(id)}]`;
}

function issue(
  document: RoadmapDocument,
  code: Extract<RoadmapIssue["code"], `E-ID-${string}`>,
  logicalPath: string,
  message: string,
): RoadmapIssue {
  return {
    code,
    source: document.document.source_path,
    logical_path: logicalPath,
    message,
    exit: 1,
  };
}

function firstById<K extends string, V extends { readonly id: K }>(
  values: readonly V[],
): ReadonlyMap<K, V> {
  const result = new Map<K, V>();
  for (const value of values) if (!result.has(value.id)) result.set(value.id, value);
  return result;
}

function valuesById<K extends string, V extends { readonly id: K }>(
  values: readonly V[],
): ReadonlyMap<K, readonly V[]> {
  const grouped = new Map<K, V[]>();
  for (const value of values) {
    const group = grouped.get(value.id) ?? [];
    group.push(value);
    grouped.set(value.id, group);
  }
  return new Map([...grouped.entries()].sort(([left], [right]) => codePointSort(left, right)).map(
    ([id, group]) => [id, Object.freeze(group)] as const,
  ));
}

function valuesByString<T>(
  values: readonly T[],
  key: (value: T) => string,
): ReadonlyMap<string, readonly T[]> {
  const grouped = new Map<string, T[]>();
  for (const value of values) {
    const id = key(value);
    const group = grouped.get(id) ?? [];
    group.push(value);
    grouped.set(id, group);
  }
  return new Map([...grouped.entries()].sort(([left], [right]) => codePointSort(left, right)).map(
    ([id, group]) => [id, Object.freeze(group)] as const,
  ));
}

function semanticPayload(record: RecordNode): SemanticPayloadProviderFact {
  return {
    record,
    payload: record.payload,
    authority: "semantic",
    logical_path: `${recordPath(record.id)}.payload`,
  };
}

/**
 * Every roadmap-ID and reference-ID citation a payload can carry derives from the descriptor
 * table: the arm walk visits ID-valued fields in place (nested tables prefixed by wire key, array
 * tables by the element's own ID field), so the use inventory can never disagree with the schema.
 */
function collectPayloadUses(
  provider: SemanticPayloadProviderFact,
  roadmapIdUses: RoadmapIdUseFact[],
  referenceIdUses: ReferenceIdUseFact[],
): void {
  const { payload, logical_path: path } = provider;
  const walk = (value: object, arm: PayloadArm, prefix: string): void => {
    for (const field of arm.fields) {
      const spec = field.value;
      const fieldValue = (value as Record<string, unknown>)[fieldProperty(field)];
      if (fieldValue === undefined) continue;
      const label = `${path}.${prefix}${field.name}`;
      switch (spec.t) {
        case "roadmap_id":
          roadmapIdUses.push({ id: fieldValue as RoadmapId, logical_path: label, role: "semantic_target" });
          break;
        case "roadmap_id_set":
          for (const id of fieldValue as readonly RoadmapId[]) {
            roadmapIdUses.push({ id, logical_path: label, role: "semantic_target" });
          }
          break;
        case "reference_id":
          referenceIdUses.push({ id: fieldValue as ReferenceId, logical_path: label });
          break;
        case "reference_id_set":
          for (const id of fieldValue as readonly ReferenceId[]) {
            referenceIdUses.push({ id, logical_path: label });
          }
          break;
        case "table":
          walk(fieldValue as object, armOfGroupValue(spec.group, fieldValue), `${prefix}${field.name}.`);
          break;
        case "array_table":
          if (spec.flatten !== undefined) break;
          for (const element of fieldValue as readonly object[]) {
            walk(
              element,
              armOfGroupValue(spec.group, element),
              `${prefix}${field.name}[${quoted(String((element as Record<string, unknown>)[spec.id_field]))}].`,
            );
          }
          break;
        default:
          break;
      }
    }
  };
  walk(payload, armOfPayload(payload), "");
}

function providerSort(left: RoadmapIdProviderFact, right: RoadmapIdProviderFact): number {
  return codePointSort(left.id, right.id) ||
    codePointSort(left.kind, right.kind) ||
    codePointSort(left.owner_record_id, right.owner_record_id) ||
    codePointSort(left.logical_path, right.logical_path);
}

function aliasSort(left: AliasProviderFact, right: AliasProviderFact): number {
  return codePointSort(left.alias, right.alias) ||
    codePointSort(left.owner_kind, right.owner_kind) ||
    codePointSort(left.owner_id, right.owner_id) ||
    codePointSort(left.logical_path, right.logical_path);
}

function subordinateSort(
  left: SubordinateIdProviderFact,
  right: SubordinateIdProviderFact,
): number {
  return codePointSort(left.kind, right.kind) ||
    codePointSort(left.id, right.id) ||
    codePointSort(left.logical_path, right.logical_path);
}

function idUseSort(left: RoadmapIdUseFact, right: RoadmapIdUseFact): number {
  return codePointSort(left.id, right.id) ||
    codePointSort(left.role, right.role) ||
    codePointSort(left.logical_path, right.logical_path);
}

function relationSort(left: Relation, right: Relation): number {
  return codePointSort(left.source, right.source) ||
    codePointSort(left.kind, right.kind) ||
    codePointSort(left.target, right.target);
}

function mapValues<K extends string, V>(
  values: readonly V[],
  key: (value: V) => K,
): ReadonlyMap<K, V> {
  const result = new Map<K, V>();
  const sorted = [...values].sort((left, right) => codePointSort(key(left), key(right)));
  for (const value of sorted) if (!result.has(key(value))) result.set(key(value), value);
  return result;
}

function groupedRelations(
  relations: readonly Relation[],
  key: (relation: Relation) => RoadmapId,
): ReadonlyMap<RoadmapId, readonly Relation[]> {
  const grouped = new Map<RoadmapId, Relation[]>();
  for (const relation of relations) {
    const id = key(relation);
    const group = grouped.get(id) ?? [];
    group.push(relation);
    grouped.set(id, group);
  }
  return new Map([...grouped.entries()].sort(([left], [right]) => codePointSort(left, right)).map(
    ([id, group]) => [id, Object.freeze(group)] as const,
  ));
}

/** Build all deterministic document-local indexes before any adapter or graph validation runs. */
export function buildRoadmapIndexes(document: RoadmapDocument): RoadmapIndexBuildResult {
  const namespace = document.document.roadmap;
  const recordNodes: readonly RecordNode[] = document.records;
  const sections: readonly Section[] = document.sections;
  const parts: readonly Part[] = document.parts;
  const references: readonly Reference[] = "references" in document ? document.references : [];
  const documentRelations: readonly Relation[] = "relations" in document ? document.relations : [];
  const issues: RoadmapIssue[] = [];
  const idProviders: RoadmapIdProviderFact[] = [];
  const aliasProviders: AliasProviderFact[] = [];
  const subordinateProviders: SubordinateIdProviderFact[] = [];
  const roadmapIdUses: RoadmapIdUseFact[] = [];
  const referenceIdUses: ReferenceIdUseFact[] = [];

  const addAlias = (
    ownerKind: AliasOwnerKind,
    ownerId: string,
    aliases: readonly string[] | undefined,
    path: string,
  ): void => {
    for (const alias of aliases ?? []) {
      aliasProviders.push({
        alias,
        namespace,
        owner_kind: ownerKind,
        owner_id: ownerId,
        logical_path: `${path}.legacy_aliases`,
      });
    }
  };
  const addSubordinate = (
    kind: SubordinateIdProviderKind,
    id: string,
    path: string,
    value: SubordinateIdProviderValue,
  ): void => {
    subordinateProviders.push({ id, namespace, kind, logical_path: path, value });
  };

  for (const value of sections) {
    const path = `section[${quoted(value.section_id)}]`;
    addSubordinate("section", value.section_id, path, value);
    addAlias("section", value.section_id, value.legacy_aliases, path);
  }
  for (const value of parts) {
    const path = `part[${quoted(value.part_id)}]`;
    addSubordinate("part", value.part_id, path, value);
    roadmapIdUses.push({
      id: value.parent_record_id,
      logical_path: `${path}.parent_record_id`,
      role: "parent_record",
      expected_namespace: namespace,
    });
  }
  for (const section of sections) {
    for (const value of section.slots ?? []) {
      addSubordinate(
        "generated_slot",
        value.slot_id,
        `section[${quoted(section.section_id)}].slots.${value.slot_id}`,
        value,
      );
    }
  }

  const payloadProviders: SemanticPayloadProviderFact[] = [];
  for (const record of recordNodes) {
    const path = recordPath(record.id);
    idProviders.push({
      id: record.id,
      namespace,
      kind: "record",
      owner_record_id: record.id,
      logical_path: path,
      value: record,
    });
    addAlias("record", record.id, record.legacy_aliases, path);
    const payload = semanticPayload(record);
    payloadProviders.push(payload);
    collectPayloadUses(payload, roadmapIdUses, referenceIdUses);
  }

  {
    for (const reference of references) {
      const path = `reference[${quoted(reference.id)}]`;
      addSubordinate("reference", reference.id, path, reference);
      roadmapIdUses.push({
        id: reference.source,
        logical_path: `${path}.source`,
        role: "reference_source",
        expected_namespace: namespace,
      });
      if (reference.kind === "roadmap") {
        roadmapIdUses.push({
          id: reference.target_id,
          logical_path: `${path}.target_id`,
          role: "reference_target",
        });
      }
    }
    for (const [index, relation] of documentRelations.entries()) {
      const path = `relation[${index}]`;
      roadmapIdUses.push({
        id: relation.source,
        logical_path: `${path}.source`,
        role: "relation_source",
        expected_namespace: namespace,
      });
      roadmapIdUses.push({
        id: relation.target,
        logical_path: `${path}.target`,
        role: "relation_target",
      });
    }
  }
  const partIds = new Set(parts.map((part) => String(part.part_id)));
  for (const section of sections) {
    for (const [index, entryId] of section.entries.entries()) {
      const path = `section[${quoted(section.section_id)}].entries[${index}]`;
      if (!partIds.has(entryId)) {
        roadmapIdUses.push({
          id: entryId as RoadmapId,
          logical_path: path,
          role: "manifest_record",
          expected_namespace: namespace,
        });
      }
    }
  }

  idProviders.sort(providerSort);
  aliasProviders.sort(aliasSort);
  subordinateProviders.sort(subordinateSort);
  for (const provider of idProviders) {
    roadmapIdUses.push({
      id: provider.id,
      logical_path: `${provider.logical_path}.id`,
      role: "provider",
      expected_namespace: namespace,
    });
  }
  roadmapIdUses.sort(idUseSort);
  referenceIdUses.sort((left, right) =>
    codePointSort(left.id, right.id) || codePointSort(left.logical_path, right.logical_path)
  );

  for (const use of roadmapIdUses) {
    const result = validateRoadmapId(use.id, use.expected_namespace);
    if (!result.ok) issues.push(issue(document, result.code, use.logical_path, result.message));
  }
  for (const use of referenceIdUses) {
    const result = validateReferenceId(use.id);
    if (!result.ok) issues.push(issue(document, result.code, use.logical_path, result.message));
  }
  for (const provider of subordinateProviders) {
    const result = provider.kind === "reference"
      ? validateReferenceId(provider.id)
      : validateSubordinateId(provider.id);
    if (!result.ok) {
      issues.push(issue(
        document,
        result.code,
        `${provider.logical_path}.${SUBORDINATE_ID_FIELD[provider.kind]}`,
        result.message,
      ));
    }
  }

  const idProviderClaims = valuesById<RoadmapId, RoadmapIdProviderFact>(idProviders);
  for (const [id, providers] of idProviderClaims) {
    if (providers.length > 1) {
      issues.push(issue(
        document,
        "E-ID-DUPLICATE",
        `id[${quoted(id)}]`,
        `first-class roadmap ID ${quoted(id)} has ${providers.length} document-local providers`,
      ));
    }
  }

  const subordinateClaims = new Map<
    SubordinateIdProviderKind,
    ReadonlyMap<string, readonly SubordinateIdProviderFact[]>
  >();
  const subordinateKinds = [
    "section",
    "part",
    "generated_slot",
    "reference",
  ] as const;
  for (const kind of [...subordinateKinds].sort(codePointSort)) {
    const claims = valuesByString(
      subordinateProviders.filter((provider) => provider.kind === kind),
      (provider) => provider.id,
    );
    subordinateClaims.set(kind, claims);
    for (const [id, providers] of claims) {
      if (providers.length > 1) {
        issues.push(issue(
          document,
          "E-ID-DUPLICATE",
          `${kind}[${quoted(id)}]`,
          `${kind} ID ${quoted(id)} has ${providers.length} document-local providers`,
        ));
      }
    }
  }

  const semanticRecords: readonly SemanticRecord[] = recordNodes;
  const payloadRecords = payloadProviders.sort((left, right) =>
    codePointSort(left.record.id, right.record.id) ||
    codePointSort(left.authority, right.authority) ||
    codePointSort(left.logical_path, right.logical_path)
  );
  const relations = Object.freeze(
    [...documentRelations].sort(relationSort),
  );

  const indexes: RoadmapIndexes = {
    roadmap: namespace,
    id_providers: Object.freeze(idProviders),
    id_provider_claims: idProviderClaims,
    first_class: firstById(idProviders),
    id_uses: Object.freeze(roadmapIdUses),
    reference_id_uses: Object.freeze(referenceIdUses),
    alias_providers: Object.freeze(aliasProviders),
    aliases: valuesByString(aliasProviders, (provider) => provider.alias),
    subordinate_id_providers: Object.freeze(subordinateProviders),
    subordinate_id_claims: subordinateClaims,
    record_nodes: mapValues(recordNodes, (record) => record.id),
    records: mapValues(semanticRecords, (record) => record.id),
    payload_records: mapValues(payloadRecords, (provider) => provider.record.id),
    evidence_records: mapValues(
      payloadRecords.filter((provider) => provider.payload.kind === "evidence"),
      (provider) => provider.record.id,
    ),
    sections: mapValues(sections, (value) => value.section_id),
    parts: mapValues(parts, (value) => value.part_id),
    generated_slots: mapValues(documentSlots(sections), (value) => value.slot_id),
    references: mapValues(references, (reference) => reference.id),
    relations,
    relations_by_source: groupedRelations(relations, (relation) => relation.source),
    relations_by_target: groupedRelations(relations, (relation) => relation.target),
    identity_inputs: {
      namespace,
      id_providers: Object.freeze(idProviders),
      alias_providers: Object.freeze(aliasProviders),
    },
  };

  return { indexes, issues: sortIssues(issues) };
}
