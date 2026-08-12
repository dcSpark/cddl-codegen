import type { RegistryView } from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import type { RoadmapIndexes, SemanticPayloadProviderFact } from "./indexes.ts";
import type { ReferenceId, RepoPath, RoadmapId } from "./model/core.ts";
import type { EvidenceStageOutcome, FamilyCoordinate, FamilyEvidenceRequirement, FamilyPayload } from "./model/systematic.ts";

export interface DerivedDenominatorCandidate {
  readonly coordinates: readonly FamilyCoordinate[];
  readonly spec_legality: "legal" | "illegal";
  readonly affected_profiles: readonly string[];
  readonly affected_faces: readonly string[];
  readonly expected_disposition?: "supported" | "safely_refused" | "deliberately_unsupported";
  readonly expected_outcomes?: readonly {
    readonly requirement_id: RoadmapId;
    readonly profile: string;
    readonly face: string;
    readonly stage: FamilyEvidenceRequirement["stages"][number];
    readonly outcome: EvidenceStageOutcome;
  }[];
}

export interface DerivedDenominator {
  readonly axes: readonly { readonly id: RoadmapId; readonly value_ids: readonly RoadmapId[] }[];
  readonly candidates: readonly DerivedDenominatorCandidate[];
  readonly evidence_requirements: readonly FamilyEvidenceRequirement[];
  readonly legal_cell_floor: number;
  readonly evidence_binding_floor: number;
}

export interface DenominatorAuthorityAdapter {
  readonly family_id: RoadmapId;
  readonly authority_kind: "grammar" | "registry" | "reviewed_relation";
  readonly authority_reference_id: ReferenceId;
  derive(view: RegistryView): DerivedDenominator;
}

export type DenominatorAuthorityRegistry = ReadonlyMap<RoadmapId, DenominatorAuthorityAdapter>;
export const EMPTY_DENOMINATOR_AUTHORITIES: DenominatorAuthorityRegistry = Object.freeze(new Map());

const cp = (a: string, b: string): number => a < b ? -1 : a > b ? 1 : 0;
const sorted = (xs: readonly string[]): readonly string[] => [...xs].sort(cp);
const same = (a: unknown, b: unknown): boolean => JSON.stringify(a) === JSON.stringify(b);
const coordinateKey = (coordinates: readonly FamilyCoordinate[]): string => JSON.stringify(
  [...coordinates].sort((a, b) => cp(a.axis_id, b.axis_id)).map(({ axis_id, value_id }) => [axis_id, value_id]),
);

function issue(source: RepoPath, path: string, message: string): RoadmapIssue {
  return { code: "E-SCHEMA-STATE", source, logical_path: path, message, exit: 1 };
}

function providerPath(provider: SemanticPayloadProviderFact): string {
  return provider.logical_path;
}

function localShape(provider: SemanticPayloadProviderFact, family: FamilyPayload, source: RepoPath): RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  const axes = new Map(family.axes.map((axis) => [axis.id, new Set(axis.values.map((value) => value.id))]));
  if (axes.size !== family.axes.length || family.axes.some((axis) => axis.values.length === 0 || axes.get(axis.id)?.size !== axis.values.length)) {
    issues.push(issue(source, `${providerPath(provider)}.axis`, "family axes and values must be unique and every declared axis must have a value"));
  }
  const seen = new Set<string>();
  for (const [kind, values] of [["cell", family.cells], ["exclusion", family.exclusions]] as const) {
    for (const value of values) {
      const key = coordinateKey(value.coordinates);
      if (seen.has(key)) issues.push(issue(source, `${providerPath(provider)}.${kind}`, `coordinate ${key} has more than one cell/exclusion owner`));
      seen.add(key);
      if (value.coordinates.length !== axes.size) issues.push(issue(source, `${providerPath(provider)}.${kind}`, "coordinate must bind every declared axis exactly once"));
      if ("affected_profiles" in value && value.affected_profiles.some((profile) => !family.affected_profiles.includes(profile))) {
        issues.push(issue(source, `${providerPath(provider)}.${kind}.affected_profiles`, "cell profiles must be a subset of family profiles"));
      }
      if ("affected_faces" in value && value.affected_faces.some((face) => !family.affected_faces.includes(face))) {
        issues.push(issue(source, `${providerPath(provider)}.${kind}.affected_faces`, "cell faces must be a subset of family faces"));
      }
      const coordinateAxes = new Set<string>();
      for (const coordinate of value.coordinates) {
        if (coordinateAxes.has(coordinate.axis_id) || !axes.get(coordinate.axis_id)?.has(coordinate.value_id)) {
          issues.push(issue(source, `${providerPath(provider)}.${kind}.coordinate`, "coordinate must name one declared value of each distinct family axis"));
        }
        coordinateAxes.add(coordinate.axis_id);
      }
    }
  }
  for (const requirement of family.evidence_requirements) {
    if (requirement.profiles.some((profile) => !family.affected_profiles.includes(profile)) ||
      requirement.faces.some((face) => !family.affected_faces.includes(face))) {
      issues.push(issue(source, `${providerPath(provider)}.evidence_requirement`, "requirement profiles/faces must be subsets of family applicability"));
    }
  }
  return issues;
}

function closedShape(
  provider: SemanticPayloadProviderFact,
  family: Extract<FamilyPayload, { family_maturity: "closed_denominator" }>,
  indexes: RoadmapIndexes,
  view: RegistryView,
  authorities: DenominatorAuthorityRegistry,
  source: RepoPath,
): RoadmapIssue[] {
  const path = providerPath(provider);
  const issues: RoadmapIssue[] = [];
  const adapter = authorities.get(provider.record.id);
  if (adapter === undefined || adapter.family_id !== provider.record.id) {
    return [issue(source, `${path}.family_maturity`, "closed denominator requires one registered live authority adapter")];
  }
  if (adapter.authority_kind !== family.authority_kind || adapter.authority_reference_id !== family.authority_reference_id) {
    issues.push(issue(source, `${path}.authority_reference_id`, "closed-family authority tuple must exactly match its registered adapter"));
  }
  let derived: DerivedDenominator;
  try {
    derived = adapter.derive(view);
    if (!same(derived, adapter.derive(view))) {
      return [issue(source, `${path}.family_maturity`, "denominator authority adapter is nondeterministic")];
    }
  } catch (error) {
    return [issue(source, `${path}.family_maturity`, `denominator authority adapter failed: ${error instanceof Error ? error.message : String(error)}`)];
  }
  if (family.campaign_state !== "closing") issues.push(issue(source, `${path}.campaign_state`, "closed denominator must be in closing campaign state"));
  if (family.axes.length === 0 || family.evidence_requirements.length === 0 || family.cells.length === 0) {
    issues.push(issue(source, path, "closed denominator requires nonempty axes, requirements, and legal cells"));
  }
  if (!Number.isSafeInteger(derived.legal_cell_floor) || derived.legal_cell_floor <= 0 ||
    !Number.isSafeInteger(derived.evidence_binding_floor) || derived.evidence_binding_floor <= 0 ||
    family.control_ids.length === 0) {
    issues.push(issue(source, path, "closed denominator requires positive safe-integer derived floors and at least one live control"));
  }
  const authoredAxes = family.axes.map((axis) => ({ id: axis.id, value_ids: sorted(axis.values.map((value) => value.id)) }));
  const derivedAxes = derived.axes.map((axis) => ({ id: axis.id, value_ids: sorted(axis.value_ids) }));
  if (new Set(derived.axes.map((axis) => axis.id)).size !== derived.axes.length ||
    derived.axes.some((axis) => axis.value_ids.length === 0 || new Set(axis.value_ids).size !== axis.value_ids.length)) {
    issues.push(issue(source, `${path}.axis`, "authority derived duplicate or empty axes/values"));
  }
  if (!same([...authoredAxes].sort((a, b) => cp(a.id, b.id)), [...derivedAxes].sort((a, b) => cp(a.id, b.id)))) {
    issues.push(issue(source, `${path}.axis`, "authored axes and values must exactly equal the authority-derived axes"));
  }
  const authoredRequirements = family.evidence_requirements.map((value) => ({
    id: value.id, profiles: sorted(value.profiles), faces: sorted(value.faces), stages: sorted(value.stages),
  })).sort((a, b) => cp(a.id, b.id));
  const derivedRequirements = derived.evidence_requirements.map((value) => ({
    id: value.id, profiles: sorted(value.profiles), faces: sorted(value.faces), stages: sorted(value.stages),
  })).sort((a, b) => cp(a.id, b.id));
  if (!same(authoredRequirements, derivedRequirements)) issues.push(issue(source, `${path}.evidence_requirement`, "authored requirements must exactly equal authority-derived requirements"));
  const legal = new Map(derived.candidates.filter((value) => value.spec_legality === "legal").map((value) => [coordinateKey(value.coordinates), value]));
  const illegal = new Map(derived.candidates.filter((value) => value.spec_legality === "illegal").map((value) => [coordinateKey(value.coordinates), value]));
  if (legal.size + illegal.size !== derived.candidates.length ||
    new Set(derived.candidates.map((value) => coordinateKey(value.coordinates))).size !== derived.candidates.length) {
    issues.push(issue(source, `${path}.cell`, "authority derived duplicate candidate coordinates"));
  }
  const cells = new Map(family.cells.map((value) => [coordinateKey(value.coordinates), value]));
  const exclusions = new Map(family.exclusions.map((value) => [coordinateKey(value.coordinates), value]));
  if (!same(sorted([...legal.keys()]), sorted([...cells.keys()]))) issues.push(issue(source, `${path}.cell`, "legal authority coordinates and authored cells must match in both directions"));
  if (!same(sorted([...illegal.keys()]), sorted([...exclusions.keys()]))) issues.push(issue(source, `${path}.exclusion`, "illegal authority coordinates and authored exclusions must match in both directions"));
  if (legal.size < derived.legal_cell_floor) issues.push(issue(source, `${path}.cell`, "derived legal-cell anti-vacuity floor was not met"));
  let bindingCount = 0;
  for (const [key, cell] of cells) {
    const candidate = legal.get(key);
    if (candidate === undefined) continue;
    if (cell.cell_disposition === "unknown") issues.push(issue(source, `${path}.cell[${JSON.stringify(cell.id)}].cell_disposition`, "closed denominator forbids unknown disposition"));
    if (candidate.expected_disposition === undefined || cell.cell_disposition !== candidate.expected_disposition) {
      issues.push(issue(source, `${path}.cell[${JSON.stringify(cell.id)}].cell_disposition`, "cell disposition must exactly equal the authority-derived disposition"));
    }
    if (cell.evidence_ids !== undefined) issues.push(issue(source, `${path}.cell[${JSON.stringify(cell.id)}].evidence_ids`, "closed denominator forbids loose evidence_ids; use exact evidence bindings"));
    if (!same(sorted(cell.affected_profiles), sorted(candidate.affected_profiles)) || !same(sorted(cell.affected_faces), sorted(candidate.affected_faces))) {
      issues.push(issue(source, `${path}.cell[${JSON.stringify(cell.id)}]`, "cell applicability must equal authority-derived profiles/faces"));
    }
    for (const profile of cell.affected_profiles) {
      for (const face of cell.affected_faces) {
        if (!derived.evidence_requirements.some((requirement) =>
          requirement.profiles.includes(profile) && requirement.faces.includes(face) && requirement.stages.length > 0
        )) {
          issues.push(issue(source, `${path}.cell[${JSON.stringify(cell.id)}].evidence_binding`, `no derived evidence requirement covers applicability coordinate ${JSON.stringify([profile, face])}`));
        }
      }
    }
    const required = derived.evidence_requirements.flatMap((requirement) =>
      requirement.profiles.filter((profile) => cell.affected_profiles.includes(profile)).flatMap((profile) =>
        requirement.faces.filter((face) => cell.affected_faces.includes(face)).flatMap((face) =>
          requirement.stages.map((stage) => JSON.stringify([requirement.id, profile, face, stage])))));
    const expectedOutcomes = new Map((candidate.expected_outcomes ?? []).map((value) => [
      JSON.stringify([value.requirement_id, value.profile, value.face, value.stage]), value.outcome,
    ]));
    const actual = new Map<string, number>();
    for (const binding of cell.evidence_bindings ?? []) {
      bindingCount++;
      const tuple = JSON.stringify([binding.requirement_id, binding.profile, binding.face, binding.stage]);
      actual.set(tuple, (actual.get(tuple) ?? 0) + 1);
      const evidence = indexes.payload_records.get(binding.evidence_id)?.payload;
      if (expectedOutcomes.get(tuple) !== binding.outcome) {
        issues.push(issue(source, `${path}.cell[${JSON.stringify(cell.id)}].evidence_binding`, "binding outcome must exactly equal the authority-derived stage outcome"));
      }
      if (evidence?.kind !== "evidence" || evidence.evidence_verdict !== "confirmed" ||
        evidence.freshness !== "live" || !evidence.scope.cell_ids?.includes(cell.id) ||
        !evidence.scope.profiles?.includes(binding.profile) || !evidence.scope.faces?.includes(binding.face)) {
        issues.push(issue(source, `${path}.cell[${JSON.stringify(cell.id)}].evidence_binding`, "binding must resolve to applicable confirmed live evidence scoped to its cell/profile/face"));
      }
    }
    if (!same(sorted(required), sorted([...actual.keys()])) || [...actual.values()].some((count) => count !== 1)) {
      issues.push(issue(source, `${path}.cell[${JSON.stringify(cell.id)}].evidence_binding`, "every derived requirement/profile/face/stage tuple requires exactly one evidence binding"));
    }
    if (!same(sorted(required), sorted([...expectedOutcomes.keys()]))) {
      issues.push(issue(source, `${path}.cell[${JSON.stringify(cell.id)}].evidence_binding`, "authority must derive one exact stage outcome for every required binding tuple"));
    }
  }
  if (bindingCount < derived.evidence_binding_floor) issues.push(issue(source, `${path}.cell.evidence_binding`, "derived evidence-binding anti-vacuity floor was not met"));
  for (const controlId of family.control_ids) {
    const control = indexes.payload_records.get(controlId)?.payload;
    if (control?.kind !== "control" || control.control_state !== "live") {
      issues.push(issue(source, `${path}.control_ids`, "closed denominator controls must resolve to live control records"));
    }
  }
  for (const exclusion of family.exclusions) {
    if (new Set([exclusion.owner_reference_id, exclusion.source_reference_id, exclusion.liveness_reference_id]).size !== 3) {
      issues.push(issue(source, `${path}.exclusion`, "closed exclusion owner/source/liveness references must be distinct"));
    }
    for (const [field, ref] of [["owner_reference_id", exclusion.owner_reference_id], ["source_reference_id", exclusion.source_reference_id], ["liveness_reference_id", exclusion.liveness_reference_id]] as const) {
      if (!indexes.references.has(ref)) issues.push(issue(source, `${path}.exclusion.${field}`, "closed exclusion reference must resolve in the roadmap reference index"));
    }
  }
  return issues;
}

export function validateSystematicFamilies(
  indexes: RoadmapIndexes,
  view: RegistryView,
  authorities: DenominatorAuthorityRegistry,
  source: RepoPath,
): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  for (const provider of indexes.family_records.values()) {
    const family = provider.payload as FamilyPayload;
    issues.push(...localShape(provider, family, source));
    const linked = [...indexes.payload_records.values()].filter((candidate) =>
      candidate.payload.kind === "work" && candidate.payload.family_id === provider.record.id
    ).map((candidate) => candidate.record.id).sort(cp);
    if (!same(linked, [...family.work_ids].sort(cp))) {
      issues.push(issue(source, `${provider.logical_path}.work_ids`, "family.work_ids and work.family_id must match in both directions"));
    }
    if (family.family_maturity === "closed_denominator") {
      issues.push(...closedShape(provider, family, indexes, view, authorities, source));
    }
  }
  return Object.freeze(issues.sort((a, b) => cp(`${a.logical_path}\0${a.message}`, `${b.logical_path}\0${b.message}`)));
}
