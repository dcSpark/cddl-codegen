import type { Indexes as AdapterIndexes } from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import type {
  FragmentId,
  MarkerId,
  PartId,
  ReferenceId,
  RoadmapId,
  RoadmapName,
  SectionId,
  SlotId,
  SpanId,
} from "./model/core.ts";
import type {
  Fragment,
  GeneratedSlot,
  LegacyMarker,
  Part,
  RecordNode,
  Reference,
  Relation,
  RoadmapDocument,
  Section,
  SemanticPayload,
  SemanticRecord,
  SourceSpan,
} from "./model/documents.ts";
import type {
  FamilyAxis,
  FamilyAxisValue,
  FamilyCell,
  FamilyEvidenceRequirement,
  FamilyExclusion,
  FamilyPayload,
} from "./model/systematic.ts";
import { validateReferenceId, validateRoadmapId, validateSubordinateId } from "./ids.ts";
import { codePointSort } from "./kernel.ts";
import { sortRoadmapIssues as sortIssues } from "./errors.ts";

export type FirstClassIdProviderKind =
  | "record"
  | "family_axis"
  | "family_axis_value"
  | "family_evidence_requirement"
  | "family_cell"
  | "family_exclusion";

export type FirstClassIdProviderValue =
  | RecordNode
  | FamilyAxis
  | FamilyAxisValue
  | FamilyEvidenceRequirement
  | FamilyCell
  | FamilyExclusion;

export interface RoadmapIdProviderFact {
  readonly id: RoadmapId;
  readonly namespace: RoadmapName;
  readonly kind: FirstClassIdProviderKind;
  readonly owner_record_id: RoadmapId;
  readonly logical_path: string;
  readonly value: FirstClassIdProviderValue;
}

export type AliasOwnerKind = "section" | "fragment" | "legacy_marker" | "record";

export interface AliasProviderFact {
  readonly alias: string;
  readonly namespace: RoadmapName;
  readonly owner_kind: AliasOwnerKind;
  readonly owner_id: string;
  readonly logical_path: string;
}

export type SubordinateIdProviderKind =
  | "section"
  | "fragment"
  | "legacy_marker"
  | "part"
  | "generated_slot"
  | "source_span"
  | "reference";

export type SubordinateIdProviderValue =
  | Section
  | Fragment
  | LegacyMarker
  | Part
  | GeneratedSlot
  | SourceSpan
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
  | "span_record_owner"
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
  readonly family_records: ReadonlyMap<RoadmapId, SemanticPayloadProviderFact>;
  readonly sections: ReadonlyMap<SectionId, Section>;
  readonly fragments: ReadonlyMap<FragmentId, Fragment>;
  readonly legacy_markers: ReadonlyMap<MarkerId, LegacyMarker>;
  readonly parts: ReadonlyMap<PartId, Part>;
  readonly generated_slots: ReadonlyMap<SlotId, GeneratedSlot>;
  readonly spans: ReadonlyMap<SpanId, SourceSpan>;
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
  fragment: "fragment_id",
  legacy_marker: "marker_id",
  part: "part_id",
  generated_slot: "slot_id",
  source_span: "id",
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

function collectPayloadUses(
  provider: SemanticPayloadProviderFact,
  roadmapIdUses: RoadmapIdUseFact[],
  referenceIdUses: ReferenceIdUseFact[],
): void {
  const { payload, logical_path: path } = provider;
  const roadmap = (id: RoadmapId | undefined, field: string): void => {
    if (id !== undefined) roadmapIdUses.push({ id, logical_path: `${path}.${field}`, role: "semantic_target" });
  };
  const roadmaps = (ids: readonly RoadmapId[] | undefined, field: string): void => {
    for (const id of ids ?? []) roadmap(id, field);
  };
  const reference = (id: ReferenceId | undefined, field: string): void => {
    if (id !== undefined) referenceIdUses.push({ id, logical_path: `${path}.${field}` });
  };
  const references = (ids: readonly ReferenceId[] | undefined, field: string): void => {
    for (const id of ids ?? []) reference(id, field);
  };

  switch (payload.kind) {
    case "work":
      roadmap(payload.family_id, "family_id");
      roadmaps(payload.evidence_ids, "evidence_ids");
      roadmaps(payload.control_ids, "control_ids");
      roadmaps(payload.regression_evidence_ids, "regression_evidence_ids");
      roadmaps(payload.regression_gap_ids, "regression_gap_ids");
      roadmaps(payload.admission_ids, "admission_ids");
      if ("transition_ids" in payload) roadmaps(payload.transition_ids, "transition_ids");
      if (payload.work_state === "waiting_external") {
        reference(payload.external_owner_reference_id, "external_owner_reference_id");
      }
      return;
    case "decision":
      if ("transition_ids" in payload) roadmaps(payload.transition_ids, "transition_ids");
      if (payload.decision_state === "decided") {
        reference(payload.authority_reference_id, "authority_reference_id");
      }
      return;
    case "signal":
      if (payload.transition_kind === "promotion_trigger" || payload.transition_kind === "reopening_signal") {
        roadmaps(payload.current_evidence_ids, "current_evidence_ids");
        if (payload.predicate.predicate_kind !== "quantitative") {
          roadmaps(payload.predicate.evidence_ids, "predicate.evidence_ids");
        }
      } else if (payload.transition_kind === "retirement_predicate") {
        reference(payload.external_owner_reference_id, "external_owner_reference_id");
      } else if (payload.transition_kind === "unblock_predicate" || payload.transition_kind === "cadence") {
        reference(payload.owner_reference_id, "owner_reference_id");
        if (payload.transition_kind === "cadence") {
          reference(payload.last_completion_reference_id, "last_completion_reference_id");
        }
      }
      return;
    case "evidence":
      references(payload.reference_ids, "reference_ids");
      reference(payload.refresh_reference_id, "refresh_reference_id");
      roadmaps(payload.scope.cell_ids, "scope.cell_ids");
      return;
    case "control":
      references(payload.reference_ids, "reference_ids");
      return;
    case "family":
      roadmaps(payload.work_ids, "work_ids");
      roadmaps(payload.control_ids, "control_ids");
      reference(payload.completion_owner_reference_id, "completion_owner_reference_id");
      reference(payload.retirement_owner_reference_id, "retirement_owner_reference_id");
      if (payload.family_maturity === "observed_only") {
        references(payload.observation_reference_ids, "observation_reference_ids");
      } else {
        reference(payload.authority_reference_id, "authority_reference_id");
        reference(payload.legality_owner_reference_id, "legality_owner_reference_id");
        if (payload.family_maturity === "closed_denominator") {
          reference(payload.drift_check_reference_id, "drift_check_reference_id");
          reference(payload.mutation_test_reference_id, "mutation_test_reference_id");
        }
      }
      for (const axis of payload.axes) {
        reference(axis.authority_reference_id, `axis[${quoted(axis.id)}].authority_reference_id`);
        for (const value of axis.values) {
          reference(value.source_reference_id, `axis[${quoted(axis.id)}].value[${quoted(value.id)}].source_reference_id`);
        }
      }
      for (const cell of payload.cells) {
        roadmaps(cell.evidence_ids, `cell[${quoted(cell.id)}].evidence_ids`);
        for (const binding of cell.evidence_bindings ?? []) {
          roadmap(binding.requirement_id, `cell[${quoted(cell.id)}].evidence_binding.requirement_id`);
          roadmap(binding.evidence_id, `cell[${quoted(cell.id)}].evidence_binding.evidence_id`);
        }
        roadmap(cell.work_id, `cell[${quoted(cell.id)}].work_id`);
        for (const coordinate of cell.coordinates) {
          roadmap(coordinate.axis_id, `cell[${quoted(cell.id)}].coordinate.axis_id`);
          roadmap(coordinate.value_id, `cell[${quoted(cell.id)}].coordinate.value_id`);
        }
      }
      for (const exclusion of payload.exclusions) {
        reference(exclusion.owner_reference_id, `exclusion[${quoted(exclusion.id)}].owner_reference_id`);
        reference(exclusion.source_reference_id, `exclusion[${quoted(exclusion.id)}].source_reference_id`);
        reference(exclusion.liveness_reference_id, `exclusion[${quoted(exclusion.id)}].liveness_reference_id`);
        for (const coordinate of exclusion.coordinates) {
          roadmap(coordinate.axis_id, `exclusion[${quoted(exclusion.id)}].coordinate.axis_id`);
          roadmap(coordinate.value_id, `exclusion[${quoted(exclusion.id)}].coordinate.value_id`);
        }
      }
      return;
    case "matrix_external_closeout":
      reference(payload.upstream_owner_reference_id, "upstream_owner_reference_id");
      roadmaps(payload.transition_ids, "transition_ids");
      references(payload.prune_reference_ids, "prune_reference_ids");
      for (const branch of payload.branches) {
        references(branch.prune_reference_ids, `branch[${quoted(branch.branch_id)}].prune_reference_ids`);
      }
      return;
    case "matrix_policy":
      reference(payload.authority_reference_id, "authority_reference_id");
      roadmap(
        payload.policy_kind === "maintenance_protocol"
          ? payload.cadence_transition_id
          : payload.reopening_transition_id,
        payload.policy_kind === "maintenance_protocol"
          ? "cadence_transition_id"
          : "reopening_transition_id",
      );
      return;
    case "testing_operational_watch":
      roadmap(payload.escalation_transition_id, "escalation_transition_id");
      if (payload.watch_state !== "watching") {
        reference(payload.operating_rule_reference_id, "operating_rule_reference_id");
      }
      if (payload.watch_state === "retire_pending") {
        reference(payload.retirement_reference_id, "retirement_reference_id");
      }
      return;
    case "testing_incident":
      roadmaps(payload.evidence_ids, "evidence_ids");
      if (payload.incident_posture !== "live") {
        reference(payload.operating_rule_reference_id, "operating_rule_reference_id");
      }
      if (payload.incident_posture === "historical") {
        reference(payload.retirement_reference_id, "retirement_reference_id");
      }
      return;
    case "testing_cost":
      if (payload.cost_posture === "live_registry") {
        reference(payload.gate_reference_id, "gate_reference_id");
      } else {
        roadmaps(payload.evidence_ids, "evidence_ids");
      }
      return;
    case "testing_system_admission":
      roadmaps(payload.evidence_ids, "evidence_ids");
      if (payload.admission_kind === "independent_recurrence") {
        roadmaps(payload.incident_ids, "incident_ids");
      } else if (payload.admission_kind === "bounded_denominator") {
        roadmap(payload.family_id, "family_id");
        roadmap(payload.cost_record_id, "cost_record_id");
      }
      return;
  }
}

function collectFamilyProviders(
  provider: SemanticPayloadProviderFact,
  namespace: RoadmapName,
  out: RoadmapIdProviderFact[],
): void {
  if (provider.payload.kind !== "family") return;
  const family = provider.payload as FamilyPayload;
  const owner = provider.record.id;
  const add = (
    kind: Exclude<FirstClassIdProviderKind, "record">,
    id: RoadmapId,
    path: string,
    value: Exclude<FirstClassIdProviderValue, RecordNode>,
  ): void => {
    out.push({ id, namespace, kind, owner_record_id: owner, logical_path: path, value });
  };
  for (const axis of family.axes) {
    const axisPath = `${provider.logical_path}.axis[${quoted(axis.id)}]`;
    add("family_axis", axis.id, axisPath, axis);
    for (const value of axis.values) {
      add(
        "family_axis_value",
        value.id,
        `${axisPath}.value[${quoted(value.id)}]`,
        value,
      );
    }
  }
  for (const requirement of family.evidence_requirements) {
    add(
      "family_evidence_requirement",
      requirement.id,
      `${provider.logical_path}.evidence_requirement[${quoted(requirement.id)}]`,
      requirement,
    );
  }
  for (const cell of family.cells) {
    add("family_cell", cell.id, `${provider.logical_path}.cell[${quoted(cell.id)}]`, cell);
  }
  for (const exclusion of family.exclusions) {
    add(
      "family_exclusion",
      exclusion.id,
      `${provider.logical_path}.exclusion[${quoted(exclusion.id)}]`,
      exclusion,
    );
  }
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
  const fragments: readonly Fragment[] = document.fragments;
  const legacyMarkers: readonly LegacyMarker[] = document.legacy_markers;
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
  for (const value of fragments) {
    const path = `fragment[${quoted(value.fragment_id)}]`;
    addSubordinate("fragment", value.fragment_id, path, value);
    addAlias("fragment", value.fragment_id, value.legacy_aliases, path);
  }
  for (const value of legacyMarkers) {
    const path = `legacy_marker[${quoted(value.marker_id)}]`;
    addSubordinate("legacy_marker", value.marker_id, path, value);
    addAlias("legacy_marker", value.marker_id, value.legacy_aliases, path);
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
  for (const value of document.generated_slots) {
    addSubordinate(
      "generated_slot",
      value.slot_id,
      `generated_slot[${quoted(value.slot_id)}]`,
      value,
    );
  }
  for (const value of document.spans) {
    const path = `source_span[${quoted(value.id)}]`;
    addSubordinate("source_span", value.id, path, value);
    if (value.source_kind === "record") {
      roadmapIdUses.push({
        id: value.owner_id as RoadmapId,
        logical_path: `${path}.owner_id`,
        role: "span_record_owner",
        expected_namespace: namespace,
      });
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
    collectFamilyProviders(payload, namespace, idProviders);
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
  for (const [index, entry] of document.manifest.entries()) {
    if (entry.kind === "record") {
      roadmapIdUses.push({
        id: entry.record_id,
        logical_path: `manifest[${index}].record_id`,
        role: "manifest_record",
        expected_namespace: namespace,
      });
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
    "fragment",
    "legacy_marker",
    "part",
    "generated_slot",
    "source_span",
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

  const semanticRecords = recordNodes.filter(
    (record): record is SemanticRecord =>
      "render_authority" in record && record.render_authority === "semantic",
  );
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
    family_records: mapValues(
      payloadRecords.filter((provider) => provider.payload.kind === "family"),
      (provider) => provider.record.id,
    ),
    sections: mapValues(sections, (value) => value.section_id),
    fragments: mapValues(fragments, (value) => value.fragment_id),
    legacy_markers: mapValues(legacyMarkers, (value) => value.marker_id),
    parts: mapValues(parts, (value) => value.part_id),
    generated_slots: mapValues(document.generated_slots, (value) => value.slot_id),
    spans: mapValues([...document.spans].sort((left, right) => codePointSort(left.id, right.id)), (value) => value.id),
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
