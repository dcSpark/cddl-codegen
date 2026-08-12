import type { RegistryView } from "./adapters/types.ts";
import { validateSystematicFamilies } from "./denominator.ts";
import { MATRIX_DENOMINATOR_AUTHORITIES } from "./fixed_value_authority.ts";
import { buildRoadmapIndexes } from "./indexes.ts";
import type { FullCommitId, RepoPath, RoadmapId } from "./model/core.ts";
import type {
  CurrentFamilyGuard,
  FixedValueClosureAuthorityFact,
  ReplacementPin,
  RetiredIdsDocumentV1,
  RoadmapDocument,
  SemanticPayload,
} from "./model/documents.ts";

const rid = (value: string): RoadmapId => value as RoadmapId;
const cp = (left: string, right: string): number => left < right ? -1 : left > right ? 1 : 0;
const same = (left: unknown, right: unknown): boolean => JSON.stringify(left) === JSON.stringify(right);

export const FIXED_VALUE_FAMILY_ROOT = rid("matrix.systematic.fixed-value-choice-member");
export const FIXED_VALUE_WORK = rid("matrix.fixed-value-choice-member");
export const FIXED_VALUE_DELIVERY_BASE =
  "8320fb3330c5d6febda8910e5a3bb944bcec1466" as FullCommitId;
export const FIXED_VALUE_CONTROL = rid("matrix.control.fixed-value-choice-member-closure");

function payloadOf(record: RoadmapDocument["records"][number] | undefined): SemanticPayload | undefined {
  if (record === undefined) return undefined;
  if ("payload" in record) return record.payload;
  return "semantic_shadow" in record ? record.semantic_shadow : undefined;
}

function activationState(
  document: RoadmapDocument | undefined,
  retired: RetiredIdsDocumentV1 | undefined,
): boolean {
  if (document === undefined || document.document.schema_version === 0) return false;
  const ids = new Set(document.records.map((record) => record.id));
  const workTombstone = retired?.entries.find((entry) => entry.id === FIXED_VALUE_WORK);
  return !ids.has(FIXED_VALUE_FAMILY_ROOT) && !ids.has(FIXED_VALUE_WORK) &&
    workTombstone?.last_active_at === FIXED_VALUE_DELIVERY_BASE;
}

export function fixedValueGuardActivationState(
  document: RoadmapDocument | undefined,
  retired: RetiredIdsDocumentV1 | undefined,
): boolean {
  return activationState(document, retired);
}

function closureError(message: string): never {
  throw new Error(`FixedValue delivered closure invalid: ${message}`);
}

export function retainedClosurePayloadsEqual(
  expected: ReadonlyMap<RoadmapId, SemanticPayload>,
  current: ReadonlyMap<RoadmapId, SemanticPayload>,
  ids: readonly RoadmapId[],
): boolean {
  return ids.every((id) => expected.has(id) && current.has(id) &&
    same(expected.get(id), current.get(id)));
}

/**
 * Revalidate the exact WP9 family against the CURRENT live authority while substituting the
 * CURRENT retained evidence/control records. This transfers the complete closed-family proof —
 * IDs, applicability, outcomes, evidence scopes and all 36 binding coordinates — rather than
 * merely remembering that twenty names once existed.
 */
export function deriveFixedValueClosureAuthority(
  baseline: RoadmapDocument,
  current: RoadmapDocument,
  registry: RegistryView,
): FixedValueClosureAuthorityFact {
  if (baseline.document.schema_version === 0 || current.document.schema_version === 0) {
    return closureError("authoritative baseline and current roadmap documents are required");
  }
  const baselineIndexes = buildRoadmapIndexes(baseline);
  if (baselineIndexes.issues.length > 0) return closureError("baseline roadmap indexes are invalid");
  const familyProvider = baselineIndexes.indexes.family_records.get(FIXED_VALUE_FAMILY_ROOT);
  const family = familyProvider?.payload;
  if (family?.kind !== "family" || family.family_maturity !== "closed_denominator") {
    return closureError("exact baseline closed family is missing");
  }
  const evidenceIds = [...new Set(family.cells.flatMap((cell) =>
    cell.evidence_bindings?.map((binding) => binding.evidence_id) ?? []
  ))].sort(cp);
  const retainedIds = new Set<RoadmapId>([...evidenceIds, ...family.control_ids]);
  const currentById = new Map(current.records.map((record) => [record.id, record]));
  const baselineById = new Map(baseline.records.map((record) => [record.id, record]));
  const baselinePayloads = new Map([...retainedIds].flatMap((id) => {
    const payload = payloadOf(baselineById.get(id));
    return payload === undefined ? [] : [[id, payload] as const];
  }));
  const currentPayloads = new Map([...retainedIds].flatMap((id) => {
    const payload = payloadOf(currentById.get(id));
    return payload === undefined ? [] : [[id, payload] as const];
  }));
  if (!retainedClosurePayloadsEqual(baselinePayloads, currentPayloads, [...retainedIds])) {
    return closureError("one or more retained evidence/control records drifted from the exact baseline payload");
  }
  const transferredRecords = baseline.records.map((record) =>
    retainedIds.has(record.id) ? currentById.get(record.id)! : record
  );
  const transferred: RoadmapDocument = {
    ...baseline,
    records: transferredRecords,
  } as RoadmapDocument;
  const transferredIndexes = buildRoadmapIndexes(transferred);
  if (transferredIndexes.issues.length > 0) return closureError("transferred proof indexes are invalid");
  const familyIssues = validateSystematicFamilies(
    transferredIndexes.indexes,
    registry,
    MATRIX_DENOMINATOR_AUTHORITIES,
    "cddl-matrix/roadmap.toml" as RepoPath,
  );
  if (familyIssues.length > 0) {
    return closureError(familyIssues.map((entry) => `${entry.logical_path}: ${entry.message}`).join("; "));
  }
  const expectedGuards = baselineIndexes.indexes.id_providers.flatMap((provider) => {
    if (provider.owner_record_id !== FIXED_VALUE_FAMILY_ROOT) return [];
    return [{
      id: provider.id,
      guard_role: provider.kind === "record" ? "closed_family_root" as const : provider.kind,
    }];
  }).sort((left, right) => cp(left.id, right.id));
  const roleCounts = Object.fromEntries([
    "closed_family_root", "family_axis", "family_axis_value", "family_evidence_requirement",
    "family_cell", "family_exclusion",
  ].map((role) => [role, expectedGuards.filter((guard) => guard.guard_role === role).length]));
  if (!same(roleCounts, {
    closed_family_root: 1,
    family_axis: 1,
    family_axis_value: 8,
    family_evidence_requirement: 2,
    family_cell: 8,
    family_exclusion: 0,
  }) || expectedGuards.length !== 20 || evidenceIds.length !== 8) {
    return closureError(`live/base provider distribution drifted: ${JSON.stringify(roleCounts)}`);
  }
  const bindingCount = family.cells.reduce((sum, cell) => sum + (cell.evidence_bindings?.length ?? 0), 0);
  if (family.cells.length !== 8 || bindingCount !== 36) {
    return closureError(`baseline proof is not the exact 8-cell/36-coordinate closure`);
  }
  return Object.freeze({
    baseline_commit: FIXED_VALUE_DELIVERY_BASE,
    expected_guards: Object.freeze(expectedGuards.map((guard) => Object.freeze(guard))),
    retained_evidence_ids: Object.freeze(evidenceIds),
    legal_cell_count: family.cells.length,
    evidence_coordinate_count: bindingCount,
  });
}

export function validateFixedValueCurrentGuardSet(
  authority: FixedValueClosureAuthorityFact,
  guards: readonly Pick<CurrentFamilyGuard, "id" | "guard_role" | "family_root_id">[],
): boolean {
  const expected = authority.expected_guards.map(({ id, guard_role }) => [id, guard_role]).sort((a, b) => cp(a[0], b[0]));
  const actual = guards.map(({ id, guard_role, family_root_id }) => [id, guard_role, family_root_id])
    .sort((a, b) => cp(a[0], b[0]));
  return same(actual, expected.map(([id, role]) => [id, role, FIXED_VALUE_FAMILY_ROOT]));
}

const replacementPin = (): ReplacementPin => Object.freeze({
  kind: "gate",
  gate_id: "roadmap_projection_check",
  claim_md: new TextEncoder().encode(
    "The roadmap projection gate preserves the delivered FixedValue denominator through revision-scoped typed guards and live authority derivation.\n",
  ),
});

export function deriveFixedValueCurrentGuards(
  current: RoadmapDocument | undefined,
  baseline: RoadmapDocument | undefined,
  retired: RetiredIdsDocumentV1 | undefined,
  registry: RegistryView,
): { readonly guards: readonly CurrentFamilyGuard[]; readonly closure?: FixedValueClosureAuthorityFact } {
  if (!activationState(current, retired)) return Object.freeze({ guards: Object.freeze([]) });
  if (baseline === undefined || current === undefined) return closureError("baseline/current source is missing");
  const closure = deriveFixedValueClosureAuthority(baseline, current, registry);
  const pin = replacementPin();
  const guards = Object.freeze(closure.expected_guards.map(({ id, guard_role }) => Object.freeze({
    id,
    guard_role,
    family_root_id: FIXED_VALUE_FAMILY_ROOT,
    replacement_pin: pin,
    owner_registry: "fixed-value-choice-member-closure",
  })));
  if (!validateFixedValueCurrentGuardSet(closure, guards)) return closureError("derived guard set is not exact");
  return Object.freeze({ guards, closure });
}
