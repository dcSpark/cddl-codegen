import type { RegistryView } from "./adapters/types.ts";
import {
  FIXED_VALUE_DELIVERY_BASE,
  FIXED_VALUE_FAMILY_ROOT,
  FIXED_VALUE_WORK,
} from "./fixed_value_guards.ts";
import type { FullCommitId, RoadmapId } from "./model/core.ts";
import type { CampaignDocumentV1, RetiredIdsDocumentV1, RoadmapDocument, SemanticPayload } from "./model/documents.ts";

export const BURNDOWN_FOUR_BASE = FIXED_VALUE_DELIVERY_BASE as FullCommitId;

const rid = (value: string): RoadmapId => value as RoadmapId;

export const BURNDOWN_FOUR_COHORT = Object.freeze([
  rid("testing.check-merged-d-ts-name-collisions-two-halves"),
  rid("testing.corpus-c-style-enum-data-type-choice-arm"),
  rid("testing.corpus-preserve-pair-map-key-value-self-encoding"),
  rid("testing.execution-binary-wrappers-true-last-documented-flag-value"),
  rid("testing.lockstep-drift-gate-rust-ts-kebab-ident-mirror"),
  rid("testing.pin-toolchain-verify-ts-s-nested-cargo-scratch"),
  rid("testing.verify-ts-refuse-unknown-flag-silently-degrading-plain"),
]);

export interface BurndownLifecycleFacts {
  readonly campaign: CampaignDocumentV1;
  readonly retired: RetiredIdsDocumentV1;
  readonly registry: RegistryView;
  readonly documents: readonly RoadmapDocument[];
}

function payloadOf(record: RoadmapDocument["records"][number]): SemanticPayload | undefined {
  if ("payload" in record) return record.payload;
  return "semantic_shadow" in record ? record.semantic_shadow : undefined;
}

function payloadRows(facts: BurndownLifecycleFacts) {
  return facts.documents.flatMap((document) => document.records.flatMap((record) => {
    const payload = payloadOf(record);
    return payload === undefined ? [] : [{ id: record.id, payload }];
  }));
}

function selectedIds(campaign: CampaignDocumentV1, cycle?: string): readonly RoadmapId[] {
  return Object.freeze(campaign.selections.filter((selection) =>
    cycle === undefined || selection.cycle === cycle
  ).map((selection) => selection.item_id).sort());
}

const UTF8 = new TextDecoder("utf-8", { fatal: true });

function structuredCostOf(selection: CampaignDocumentV1["selections"][number] | undefined) {
  const bound = selection?.cost_bound;
  if (bound === undefined) return Object.freeze({ status: "missing" });
  return Object.freeze({
    status: "bounded",
    posture: bound.posture,
    implementation_units: Object.freeze([...bound.implementation_units]),
    validation_units: Object.freeze([...bound.validation_units]),
    implementation_unit_count: bound.implementation_units.length,
    validation_unit_count: bound.validation_units.length,
    total_unit_count: bound.implementation_units.length + bound.validation_units.length,
    assumption_md: UTF8.decode(bound.assumption_md),
  });
}

export function burndownStableSetDelta(expected: readonly RoadmapId[], actual: readonly RoadmapId[]) {
  const expectedSet = new Set(expected);
  const actualSet = new Set(actual);
  return Object.freeze({
    added: Object.freeze(actual.filter((id) => !expectedSet.has(id))),
    removed: Object.freeze(expected.filter((id) => !actualSet.has(id))),
    missing: Object.freeze(expected.filter((id) => !actualSet.has(id))),
    unexpected: Object.freeze(actual.filter((id) => !expectedSet.has(id))),
    stable: expected.length === actual.length && expected.every((id) => actualSet.has(id)),
  });
}

/** Lifecycle-aware burndown report with one exact historical baseline and no invented costs. */
export function buildBurndownFourReport(
  baseline: BurndownLifecycleFacts,
  current: BurndownLifecycleFacts,
): Readonly<Record<string, unknown>> {
  const baselineRows = payloadRows(baseline);
  const currentRows = payloadRows(current);
  const baselineById = new Map(baselineRows.map((row) => [row.id, row.payload]));
  const currentById = new Map(currentRows.map((row) => [row.id, row.payload]));
  const currentCycle = selectedIds(current.campaign, "burndown-four");
  const currentCycleSelections = current.campaign.selections.filter((selection) =>
    selection.cycle === "burndown-four"
  ).sort((left, right) => left.item_id < right.item_id ? -1 : left.item_id > right.item_id ? 1 : 0);
  const currentCycleSelectionById = new Map(currentCycleSelections.map((selection) =>
    [selection.item_id, selection]
  ));
  const baselineSelected = selectedIds(baseline.campaign);
  const currentTombstones = new Set(current.retired.entries.map((entry) => entry.id));
  const authority = current.registry.fixed_value_closure;
  if (authority === undefined || authority.baseline_commit !== BURNDOWN_FOUR_BASE) {
    throw new Error("burndown current registry lacks the exact FixedValue delivered-closure authority");
  }
  const guards = current.registry.current_guards.filter((guard) =>
    guard.guard_role !== undefined && guard.family_root_id === FIXED_VALUE_FAMILY_ROOT
  );
  const relations = current.documents.flatMap((document) =>
    "relations" in document ? document.relations : []
  );
  const reopeningRows = relations.filter((relation) =>
    relation.kind === "reopens" && relation.target === FIXED_VALUE_FAMILY_ROOT
  ).map((relation) => ({ source_work_id: relation.source, family_root_id: relation.target }));
  const retainedEvidence = new Set(authority.retained_evidence_ids);
  const retainedEvidenceCount = currentRows.filter((row) =>
    row.payload.kind === "evidence" && retainedEvidence.has(row.id)
  ).length;
  const cohortItems = BURNDOWN_FOUR_COHORT.map((id) => {
    const basePayload = baselineById.get(id);
    const payload = currentById.get(id);
    const work = payload?.kind === "work" ? payload : basePayload?.kind === "work" ? basePayload : undefined;
    return Object.freeze({
      id,
      baseline_active: basePayload !== undefined,
      current_active: payload !== undefined,
      current_selected: currentCycle.includes(id),
      current_retired: currentTombstones.has(id),
      work_state: work?.work_state ?? null,
      work_kind: work?.work_kind ?? null,
      risk: work?.risk ?? null,
      work_intent: work?.work_intent ?? null,
      admission_status: work?.work_kind !== "missing_system"
        ? "not_applicable"
        : (work.admission_ids?.length ?? 0) > 0 ? "admitted" : "missing",
      structured_cost: structuredCostOf(currentCycleSelectionById.get(id)),
    });
  });
  const boundedCosts = currentCycleSelections.flatMap((selection) =>
    selection.cost_bound === undefined ? [] : [selection.cost_bound]
  );
  const missingCostSelectionIds = currentCycleSelections.filter((selection) =>
    selection.cost_bound === undefined
  ).map((selection) => selection.item_id);
  const implementationUnitCount = boundedCosts.reduce(
    (count, bound) => count + bound.implementation_units.length,
    0,
  );
  const validationUnitCount = boundedCosts.reduce(
    (count, bound) => count + bound.validation_units.length,
    0,
  );
  const openFamilies = currentRows.flatMap((row) =>
    row.payload.kind === "family" && row.payload.family_maturity !== "closed_denominator"
      ? [{ id: row.id, observed_lower_bound: row.payload.cells.length }]
      : []
  ).sort((left, right) => left.id < right.id ? -1 : left.id > right.id ? 1 : 0);
  return Object.freeze({
    baseline_commit: BURNDOWN_FOUR_BASE,
    cycle: "burndown-four",
    cohort: Object.freeze({
      expected_ids: BURNDOWN_FOUR_COHORT,
      baseline_selected_ids: baselineSelected,
      current_selected_ids: currentCycle,
      campaign_selection_delta: burndownStableSetDelta(baselineSelected, currentCycle),
      stable_set_delta: burndownStableSetDelta(BURNDOWN_FOUR_COHORT, currentCycle),
      current_items: Object.freeze(cohortItems),
    }),
    retired_family: Object.freeze({
      root_id: FIXED_VALUE_FAMILY_ROOT,
      linked_work_id: FIXED_VALUE_WORK,
      baseline: Object.freeze({
        active: true,
        active_item_count: 1,
        legal_cell_count: authority.legal_cell_count,
        evidence_coordinate_count: authority.evidence_coordinate_count,
      }),
      current: Object.freeze({
        active: currentById.has(FIXED_VALUE_FAMILY_ROOT),
        guarded: guards.some((guard) => guard.guard_role === "closed_family_root"),
        linked_work_tombstoned: currentTombstones.has(FIXED_VALUE_WORK),
        root_guard_count: guards.filter((guard) => guard.guard_role === "closed_family_root").length,
        total_guard_count: guards.length,
        retained_evidence_count: retainedEvidenceCount,
      }),
      deltas: Object.freeze({
        active_items: currentById.has(FIXED_VALUE_FAMILY_ROOT) ? 0 : -1,
        active_cells: currentById.has(FIXED_VALUE_FAMILY_ROOT) ? 0 : -authority.legal_cell_count,
        active_evidence_coordinates: currentById.has(FIXED_VALUE_FAMILY_ROOT)
          ? 0
          : -authority.evidence_coordinate_count,
      }),
      same_family_reopening_rows: Object.freeze(reopeningRows),
      verification_cost_envelope: Object.freeze({
        status: "bounded",
        legal_cell_count: authority.legal_cell_count,
        evidence_coordinate_count: authority.evidence_coordinate_count,
        typed_guard_count: guards.length,
        retained_evidence_count: retainedEvidenceCount,
      }),
    }),
    active_open_families: Object.freeze(openFamilies),
    structured_costs: Object.freeze({
      status: missingCostSelectionIds.length === 0 ? "bounded" : "missing",
      implementation_unit_count: implementationUnitCount,
      validation_unit_count: validationUnitCount,
      total_unit_count: implementationUnitCount + validationUnitCount,
      missing_selection_ids: Object.freeze(missingCostSelectionIds),
    }),
  });
}
