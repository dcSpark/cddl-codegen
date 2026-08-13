/**
 * The reference-provider registry and the join validation built on it: which provider resolves each
 * reference kind, which kinds a citing field may name, and whether every cited target resolves with
 * the expected kind.  Repository-fact SCANNING lives in `repository_facts.ts` / `rust_symbols.ts`;
 * this module consumes the facts those produce through the registry view.
 */
import type {
  ReferenceProvider,
  RegistryView,
  Resolution,
} from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import type {
  RoadmapIndexes,
  RoadmapIdProviderFact,
  SemanticPayloadProviderFact,
} from "./indexes.ts";
import { namespaceOf } from "./ids.ts";
import type { RoadmapId } from "./model/core.ts";
import type { Reference } from "./model/documents.ts";
import type { CurrentGuard, FamilyGuardRole, Relation, SemanticPayload } from "./model/documents.ts";
import { codePointSort } from "./kernel.ts";
import { sortRoadmapIssues as sortIssues } from "./errors.ts";

export interface ReferenceValidationOptions {
  readonly source?: string;
  readonly providers?: readonly ReferenceProviderLike[];
  /** Global first-class view; defaults to the selected document for single-roadmap validation. */
  readonly first_class?: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>;
  /** Scoped lanes defer only well-formed opposite-namespace RoadmapId targets to the cross-roadmap join. */
  readonly defer_foreign_roadmap_joins?: "matrix" | "testing";
}

export interface SemanticJoinUniverse {
  readonly first_class: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>;
  readonly payload_records: ReadonlyMap<RoadmapId, SemanticPayloadProviderFact>;
  readonly current_guards?: readonly CurrentGuard[];
}

export function validateGuardedFamilyReopens(
  payloadRecords: ReadonlyMap<RoadmapId, SemanticPayloadProviderFact>,
  relations: readonly Relation[],
  currentGuards: readonly CurrentGuard[],
  source = "<guarded-family-reopens>",
): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  const guards = new Map(currentGuards.map((guard) => [guard.id, guard]));
  for (const provider of payloadRecords.values()) {
    const payload = provider.payload;
    if (payload.kind !== "work" || payload.family_id === undefined ||
      guards.get(payload.family_id)?.guard_role !== "closed_family_root") continue;
    const targets = relations.filter((relation) =>
      relation.source === provider.record.id && relation.kind === "reopens"
    ).map((relation) => relation.target);
    if (targets.length !== 1 || targets[0] !== payload.family_id) {
      issues.push(issue(
        "E-REFERENCE-FORBIDDEN",
        source,
        `${provider.logical_path}.family_id`,
        "work targeting a guarded family requires exactly one same-root outgoing reopens relation",
      ));
    }
  }
  for (const relation of relations.filter((candidate) => candidate.kind === "reopens")) {
    const guard = guards.get(relation.target);
    if (guard === undefined) continue;
    const sourcePayload = payloadRecords.get(relation.source)?.payload;
    if (guard.guard_role !== "closed_family_root" || sourcePayload?.kind !== "work" ||
      sourcePayload.family_id !== relation.target) {
      issues.push(issue(
        "E-REFERENCE-FORBIDDEN",
        source,
        `relation.reopens[${JSON.stringify([relation.source, relation.target])}]`,
        "guarded reopens must originate from work whose family_id targets the same closed-family root guard",
      ));
    }
  }
  return sortIssues(issues);
}

function guardedSemanticRole(
  logicalPath: string,
  sourcePayload: SemanticPayload | undefined,
): FamilyGuardRole | undefined {
  if (logicalPath.endsWith("scope.cell_ids")) return "family_cell";
  if (logicalPath.endsWith("family_id") && sourcePayload?.kind === "work") return "closed_family_root";
  return undefined;
}

export type AnyReferenceProvider = {
  [K in Reference["kind"]]: ReferenceProvider<K>;
}[Reference["kind"]];

export type ReferenceProviderLike = ReferenceProvider | AnyReferenceProvider;

export interface ReferenceProviderRegistry {
  readonly by_kind: ReadonlyMap<ReferenceKind, ReferenceProviderLike>;
  readonly issues: readonly RoadmapIssue[];
}

function provider<K extends Reference["kind"]>(
  kind: K,
  resolveReference: (
    reference: Extract<Reference, { kind: K }>,
    view: RegistryView,
  ) => Resolution,
): ReferenceProvider<K> {
  return { kind, resolve: resolveReference };
}

function issue(
  code: RoadmapIssue["code"],
  source: string,
  logicalPath: string,
  message: string,
  span?: { start_byte: number; end_byte: number },
): RoadmapIssue {
  return { code, source, logical_path: logicalPath, message, span, exit: 1 };
}

function exactOne<T>(values: readonly T[], predicate: (value: T) => boolean): T | undefined {
  const matches = values.filter(predicate);
  return matches.length === 1 ? matches[0] : undefined;
}

function resolved(provider: string): Resolution {
  return { resolved: true, provider };
}

function unresolved(reason: string): Resolution {
  return { resolved: false, reason };
}

function structuralProvider<K extends Reference["kind"]>(
  kind: K,
  target: (reference: Extract<Reference, { kind: K }>) => readonly string[],
): ReferenceProvider<K> {
  return provider(kind, (reference) => {
      const values = target(reference);
      return values.length > 0 && values.every((value) => value.length > 0)
        ? resolved(`${kind}:${JSON.stringify(values)}`)
        : unresolved("unresolved: the structured reference target is empty");
  });
}

/**
 * The closed core provider registry. External/spec/consumer references are deliberately distinct
 * structural providers; they are never accepted by a generic catch-all arm.
 */
export function createCoreReferenceProviders(
  firstClass: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>,
): readonly AnyReferenceProvider[] {
  const providers: AnyReferenceProvider[] = [
    provider("roadmap", (reference) => {
        return firstClass.has(reference.target_id)
          ? resolved(`roadmap:${reference.target_id}`)
          : unresolved(`unresolved: roadmap target ${JSON.stringify(reference.target_id)} is not active`);
    }),
    provider("gate", (reference, view) => {
        const gate = exactOne(view.gates, (fact) => fact.id === reference.gate_id);
        if (gate === undefined) {
          return unresolved(`unresolved: gate ${JSON.stringify(reference.gate_id)} is absent`);
        }
        return gate.stub
          ? unresolved(`stub: gate ${JSON.stringify(reference.gate_id)} is a stub`)
          : resolved(`gate:${reference.gate_id}`);
    }),
    provider("test_symbol", (reference, view) => {
        return exactOne(
          view.test_symbols,
          (fact) => fact.test_id === reference.test_id && fact.symbol === reference.symbol,
        )
          ? resolved(`test_symbol:${reference.test_id}:${reference.symbol}`)
          : unresolved(
            `unresolved: derived test symbol (${JSON.stringify(reference.test_id)}, ${JSON.stringify(reference.symbol)}) is absent`,
          );
    }),
    provider("file_heading", (reference, view) => {
        if (reference.path === "draft" || reference.path.startsWith("draft/")) {
          return unresolved(`forbidden: draft path ${JSON.stringify(reference.path)} is not durable`);
        }
        return exactOne(
          view.tracked_headings,
          (fact) => fact.path === reference.path && fact.heading === reference.heading,
        )
          ? resolved(`file_heading:${reference.path}:${reference.heading}`)
          : unresolved(
            `unresolved: tracked heading (${JSON.stringify(reference.path)}, ${JSON.stringify(reference.heading)}) is absent`,
          );
    }),
    structuralProvider("spec_passage", (reference) =>
      [reference.document, reference.passage]),
    structuralProvider("external_issue", (reference) =>
      [reference.repository, reference.issue]),
    structuralProvider("external_commit", (reference) =>
      [reference.repository, reference.commit]),
    structuralProvider("external_release", (reference) =>
      [reference.project, reference.release]),
    structuralProvider("consumer_report", (reference) =>
      [reference.consumer, reference.report_reference]),
  ];
  return Object.freeze(providers.sort((left, right) => codePointSort(left.kind, right.kind)));
}

export const REFERENCE_KIND_REGISTRY = Object.freeze([
  "roadmap",
  "matrix_feature",
  "matrix_role",
  "matrix_cell",
  "gate",
  "test_symbol",
  "file_heading",
  "spec_passage",
  "external_issue",
  "external_commit",
  "external_release",
  "consumer_report",
] as const satisfies readonly Reference["kind"][]);

export function collectReferenceProviders(
  claims: readonly ReferenceProviderLike[],
  source?: string,
): ReferenceProviderRegistry;
export function collectReferenceProviders(
  firstClass: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>,
  injected: readonly ReferenceProviderLike[],
  source?: string,
): ReferenceProviderRegistry;
export function collectReferenceProviders(
  claimsOrFirstClass: readonly ReferenceProviderLike[] | ReadonlyMap<RoadmapId, RoadmapIdProviderFact>,
  injectedOrSource: readonly ReferenceProviderLike[] | string = [],
  explicitSource?: string,
): ReferenceProviderRegistry {
  let claims: readonly ReferenceProviderLike[];
  if (Array.isArray(claimsOrFirstClass)) {
    claims = claimsOrFirstClass as readonly ReferenceProviderLike[];
  } else {
    claims = [
      ...createCoreReferenceProviders(claimsOrFirstClass as ReadonlyMap<RoadmapId, RoadmapIdProviderFact>),
      ...injectedOrSource as readonly ReferenceProviderLike[],
    ];
  }
  const source = typeof injectedOrSource === "string"
    ? injectedOrSource
    : explicitSource ?? "<reference-providers>";
  const sortedClaims = [...claims].sort((left, right) => codePointSort(left.kind, right.kind));
  const groups = new Map<ReferenceKind, ReferenceProviderLike[]>();
  for (const claim of sortedClaims) {
    const group = groups.get(claim.kind) ?? [];
    group.push(claim);
    groups.set(claim.kind, group);
  }
  const byKind = new Map<ReferenceKind, ReferenceProviderLike>();
  const issues: RoadmapIssue[] = [];
  for (const kind of REFERENCE_KIND_REGISTRY) {
    const group = groups.get(kind) ?? [];
    if (group.length !== 1) {
      issues.push(issue(
        "E-REFERENCE-UNRESOLVED",
        source,
        `reference-provider.${kind}`,
        `reference kind ${JSON.stringify(kind)} must have exactly one explicit provider; found ${group.length}`,
      ));
    } else {
      byKind.set(kind, group[0]!);
    }
  }
  return { by_kind: byKind, issues: sortIssues(issues) };
}

export function referenceTargetTuple(reference: Reference): readonly string[] {
  switch (reference.kind) {
    case "roadmap": return [reference.target_id];
    case "matrix_feature": return [reference.feature_id];
    case "matrix_role": return [reference.role_id];
    case "matrix_cell": return [reference.cell_id];
    case "gate": return [reference.gate_id];
    case "test_symbol": return [reference.test_id, reference.symbol];
    case "file_heading": return [reference.path, reference.heading];
    case "spec_passage": return [reference.document, reference.passage];
    case "external_issue": return [reference.repository, reference.issue];
    case "external_commit": return [reference.repository, reference.commit];
    case "external_release": return [reference.project, reference.release];
    case "consumer_report": return [reference.consumer, reference.report_reference];
  }
}

function compareStringTuples(left: readonly string[], right: readonly string[]): number {
  const shared = Math.min(left.length, right.length);
  for (let index = 0; index < shared; index += 1) {
    const compared = codePointSort(left[index]!, right[index]!);
    if (compared !== 0) return compared;
  }
  return left.length - right.length;
}

export function compareReferenceTargets(left: Reference, right: Reference): number {
  return codePointSort(left.source, right.source) || codePointSort(left.kind, right.kind) ||
    compareStringTuples(referenceTargetTuple(left), referenceTargetTuple(right));
}

type ReferenceKind = Reference["kind"];

const durableOwnerKinds = Object.freeze([
  "consumer_report", "external_commit", "external_issue", "external_release", "file_heading",
  "gate", "matrix_feature", "matrix_role", "roadmap", "spec_passage", "test_symbol",
] as const satisfies readonly ReferenceKind[]);

const externalOwnerKinds = Object.freeze([
  "consumer_report", "external_commit", "external_issue", "external_release",
] as const satisfies readonly ReferenceKind[]);

const authoritativeReferenceKinds = Object.freeze([
  "file_heading", "gate", "matrix_feature", "roadmap", "spec_passage",
] as const satisfies readonly ReferenceKind[]);

function evidenceReferenceKinds(
  payload: Extract<SemanticPayloadProviderFact["payload"], { kind: "evidence" }>,
): readonly ReferenceKind[] {
  switch (payload.evidence_kind) {
    case "regression_pin": return ["gate", "test_symbol"];
    case "gate": return ["gate"];
    case "harness_free_repro": return durableOwnerKinds;
    case "committed_vector": return ["file_heading", "test_symbol"];
    case "execution_probe": return durableOwnerKinds;
    case "registry_enumeration": return ["file_heading", "gate", "matrix_cell", "matrix_feature", "matrix_role"];
    case "source_read": return ["file_heading"];
    case "spec_read": return ["spec_passage"];
    case "consumer_report": return ["consumer_report"];
    case "incident": return ["file_heading", "roadmap"];
    case "external_issue": return ["external_issue"];
    case "external_commit": return ["external_commit"];
    case "decision": return ["roadmap"];
  }
}

function allowedReferenceKinds(
  payload: SemanticPayloadProviderFact["payload"],
  logicalPath: string,
): readonly ReferenceKind[] {
  if (payload.kind === "work" && logicalPath.endsWith("external_owner_reference_id")) {
    return externalOwnerKinds;
  }
  if (payload.kind === "decision" && logicalPath.endsWith("authority_reference_id")) {
    return authoritativeReferenceKinds;
  }
  if (payload.kind === "signal") {
    if (logicalPath.endsWith("external_owner_reference_id")) return externalOwnerKinds;
    if (
      logicalPath.endsWith("owner_reference_id") ||
      logicalPath.endsWith("last_completion_reference_id")
    ) return durableOwnerKinds;
  }
  if (payload.kind === "control" && logicalPath.endsWith("reference_ids")) {
    switch (payload.control_kind) {
      case "gate": return ["gate"];
      case "test": return ["matrix_cell", "test_symbol"];
      case "fixture": return ["file_heading", "test_symbol"];
      case "review_rule": return ["file_heading"];
      case "consumer_ci": return ["consumer_report", "gate"];
      case "upstream_issue": return ["external_issue"];
      case "operator_procedure": return ["file_heading"];
    }
  }
  if (
    payload.kind === "evidence" &&
    (logicalPath.endsWith("reference_ids") || logicalPath.endsWith("refresh_reference_id"))
  ) {
    return evidenceReferenceKinds(payload);
  }
  if (payload.kind === "family") {
    if (logicalPath.includes(".axis[") && logicalPath.endsWith("authority_reference_id")) return ["spec_passage"];
    if (logicalPath.includes(".value[") && logicalPath.endsWith("source_reference_id")) return ["spec_passage"];
    if (logicalPath.includes(".exclusion[") && logicalPath.endsWith("owner_reference_id")) return ["file_heading"];
    if (logicalPath.includes(".exclusion[") && logicalPath.endsWith("source_reference_id")) return ["spec_passage"];
    if (logicalPath.includes(".exclusion[") && logicalPath.endsWith("liveness_reference_id")) return ["gate"];
    if (logicalPath.endsWith("authority_reference_id")) {
      if (payload.family_maturity === "observed_only") return [];
      return payload.authority_kind === "grammar"
        ? ["spec_passage"]
        : payload.authority_kind === "registry"
        ? ["file_heading", "gate", "matrix_cell", "matrix_feature", "matrix_role"]
        : ["roadmap"];
    }
    if (logicalPath.endsWith("legality_owner_reference_id")) return ["file_heading", "spec_passage"];
    if (logicalPath.endsWith("drift_check_reference_id") || logicalPath.endsWith("mutation_test_reference_id")) {
      return ["file_heading", "gate", "test_symbol"];
    }
    if (logicalPath.endsWith("observation_reference_ids")) return durableOwnerKinds;
    if (logicalPath.endsWith("completion_owner_reference_id") || logicalPath.endsWith("retirement_owner_reference_id")) {
      return durableOwnerKinds;
    }
  }
  if (payload.kind === "matrix_external_closeout" && logicalPath.endsWith("upstream_owner_reference_id")) {
    return ["external_commit", "external_issue", "external_release"];
  }
  if (
    payload.kind === "matrix_external_closeout" &&
    logicalPath.endsWith("prune_reference_ids")
  ) {
    return ["external_commit", "external_issue", "external_release"];
  }
  if (payload.kind === "matrix_policy" && logicalPath.endsWith("authority_reference_id")) {
    return authoritativeReferenceKinds;
  }
  if (payload.kind === "testing_cost" && logicalPath.endsWith("gate_reference_id")) return ["gate"];
  if (
    (payload.kind === "testing_operational_watch" || payload.kind === "testing_incident") &&
    logicalPath.endsWith("operating_rule_reference_id")
  ) return ["file_heading", "gate"];
  if (
    (payload.kind === "testing_operational_watch" || payload.kind === "testing_incident") &&
    logicalPath.endsWith("retirement_reference_id")
  ) return ["file_heading", "gate", "test_symbol"];
  if (logicalPath.endsWith("external_owner_reference_id")) {
    return externalOwnerKinds;
  }
  return [];
}

const roadmapNamespace = namespaceOf;

function deferredForeignTarget(
  id: RoadmapId,
  local: "matrix" | "testing" | undefined,
  firstClass: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>,
): boolean {
  const target = roadmapNamespace(id);
  return local !== undefined && target !== undefined && target !== local && !firstClass.has(id);
}

export function validateRoadmapReferences(
  indexes: RoadmapIndexes,
  view: RegistryView,
  options: ReferenceValidationOptions = {},
): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  const source = options.source ?? `<roadmap:${indexes.roadmap}>`;
  const firstClass = options.first_class ?? indexes.first_class;
  const providerRegistry = collectReferenceProviders(
    [
      ...createCoreReferenceProviders(firstClass),
      ...options.providers ?? [],
    ],
    source,
  );
  issues.push(...providerRegistry.issues);

  const semanticTuples = new Map<string, Reference[]>();
  for (const reference of [...indexes.references.values()].sort(compareReferenceTargets)) {
    const path = `reference[${JSON.stringify(reference.id)}]`;
    if (!firstClass.has(reference.source)) {
      issues.push(issue(
        "E-REFERENCE-FORBIDDEN",
        source,
        `${path}.source`,
        `reference source ${JSON.stringify(reference.source)} is not an active first-class ID`,
      ));
    }
    const tupleKey = JSON.stringify([reference.source, reference.kind, ...referenceTargetTuple(reference)]);
    const tupleGroup = semanticTuples.get(tupleKey) ?? [];
    tupleGroup.push(reference);
    semanticTuples.set(tupleKey, tupleGroup);
    const selectedProvider = providerRegistry.by_kind.get(reference.kind);
    if (selectedProvider !== undefined) {
      if (
        reference.kind === "roadmap" &&
        deferredForeignTarget(reference.target_id, options.defer_foreign_roadmap_joins, firstClass)
      ) continue;
      const resolution = (selectedProvider as ReferenceProvider).resolve(reference, view);
      if (!resolution.resolved) {
        const code = resolution.reason.startsWith("forbidden:")
          ? "E-REFERENCE-FORBIDDEN"
          : resolution.reason.startsWith("stub:")
          ? "E-REFERENCE-STUB"
          : "E-REFERENCE-UNRESOLVED";
        issues.push(issue(code, source, path, resolution.reason.replace(/^[a-z]+: /u, "")));
      }
    }
  }
  for (const [key, references] of [...semanticTuples].sort((left, right) =>
    compareReferenceTargets(left[1][0]!, right[1][0]!)
  )) {
    if (references.length > 1) {
      issues.push(issue(
        "E-REFERENCE-UNRESOLVED",
        source,
        `reference-tuple.${key}`,
        `duplicate semantic reference tuple is authored ${references.length} times`,
      ));
    }
  }

  for (const use of indexes.reference_id_uses) {
    const reference = indexes.references.get(use.id);
    if (reference === undefined) {
      issues.push(issue(
        "E-REFERENCE-UNRESOLVED",
        source,
        use.logical_path,
        `reference ID ${JSON.stringify(use.id)} has no declared reference`,
      ));
      continue;
    }
    const consumer = [...indexes.payload_records.values()].find((provider) =>
      use.logical_path.startsWith(`${provider.logical_path}.`)
    );
    if (consumer !== undefined) {
      const allowed = allowedReferenceKinds(consumer.payload, use.logical_path);
      if (!allowed.includes(reference.kind)) {
        issues.push(issue(
          "E-REFERENCE-FORBIDDEN",
          source,
          use.logical_path,
          `reference kind ${reference.kind} is invalid for this ${consumer.payload.kind} field; expected ${allowed.join("|") || "no reference"}`,
        ));
      }
    }
  }
  return sortIssues(issues);
}

/** Close the one reference seam scoped validation may defer: opposite-lane roadmap targets. */
export function validateCombinedRoadmapReferences(
  indexes: RoadmapIndexes,
  firstClass: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>,
  source = `<roadmap:${indexes.roadmap}>`,
): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  for (const reference of [...indexes.references.values()].sort(compareReferenceTargets)) {
    if (reference.kind !== "roadmap") continue;
    if (!firstClass.has(reference.target_id)) {
      issues.push(issue(
        "E-REFERENCE-UNRESOLVED",
        source,
        `reference[${JSON.stringify(reference.id)}]`,
        `roadmap target ${JSON.stringify(reference.target_id)} is not active in the combined roadmap universe`,
      ));
    }
  }
  return sortIssues(issues);
}

type ExpectedSemanticTarget =
  | { readonly provider_kind: RoadmapIdProviderFact["kind"] }
  | {
      readonly payload_kind: SemanticPayloadProviderFact["payload"]["kind"];
      readonly work_kind?: "regression_gap";
      readonly transition_kinds?: readonly Extract<
        SemanticPayloadProviderFact["payload"],
        { kind: "signal" }
      >["transition_kind"][];
      readonly control_state?: "live";
    };

function semanticTargetExpectation(
  logicalPath: string,
  sourcePayload: SemanticPayloadProviderFact["payload"] | undefined,
): ExpectedSemanticTarget | undefined {
  if (logicalPath.includes(".coordinate.axis_id")) return { provider_kind: "family_axis" };
  if (logicalPath.includes(".coordinate.value_id")) return { provider_kind: "family_axis_value" };
  if (logicalPath.endsWith("scope.cell_ids")) return { provider_kind: "family_cell" };
  if (logicalPath.endsWith("family_id")) return { payload_kind: "family" };
  if (logicalPath.endsWith("control_ids")) {
    return sourcePayload?.kind === "work" && sourcePayload.work_state === "armed"
      ? { payload_kind: "control", control_state: "live" }
      : { payload_kind: "control" };
  }
  if (logicalPath.endsWith("cadence_transition_id")) {
    return { payload_kind: "signal", transition_kinds: ["cadence"] };
  }
  if (logicalPath.endsWith("reopening_transition_id")) {
    return { payload_kind: "signal", transition_kinds: ["reopening_signal"] };
  }
  if (logicalPath.endsWith("escalation_transition_id")) {
    return { payload_kind: "signal", transition_kinds: ["watch_escalation"] };
  }
  if (logicalPath.endsWith("transition_ids")) {
    if (sourcePayload?.kind === "work") {
      if (sourcePayload.work_state === "blocked") return { payload_kind: "signal", transition_kinds: ["unblock_predicate"] };
      if (sourcePayload.work_state === "armed") return { payload_kind: "signal", transition_kinds: ["promotion_trigger"] };
      if (sourcePayload.work_state === "deferred") return { payload_kind: "signal", transition_kinds: ["reopening_signal"] };
      if (sourcePayload.work_state === "waiting_external") {
        return { payload_kind: "signal", transition_kinds: ["retirement_predicate", "unblock_predicate"] };
      }
    }
    if (sourcePayload?.kind === "decision") {
      return {
        payload_kind: "signal",
        transition_kinds: sourcePayload.decision_state === "pending"
          ? ["unblock_predicate"]
          : ["reopening_signal"],
      };
    }
    if (sourcePayload?.kind === "matrix_external_closeout") {
      return { payload_kind: "signal", transition_kinds: ["retirement_predicate"] };
    }
    return { payload_kind: "signal", transition_kinds: [] };
  }
  if (logicalPath.endsWith("regression_gap_ids")) return { payload_kind: "work", work_kind: "regression_gap" };
  if (logicalPath.endsWith("work_ids") || logicalPath.endsWith("work_id")) return { payload_kind: "work" };
  if (logicalPath.endsWith("admission_ids")) return { payload_kind: "testing_system_admission" };
  if (logicalPath.endsWith("incident_ids")) return { payload_kind: "testing_incident" };
  if (logicalPath.endsWith("cost_record_id")) return { payload_kind: "testing_cost" };
  if (logicalPath.endsWith("evidence_ids") || logicalPath.endsWith("current_evidence_ids") || logicalPath.endsWith("regression_evidence_ids")) {
    return { payload_kind: "evidence" };
  }
  if (logicalPath.endsWith("evidence_binding.evidence_id")) return { payload_kind: "evidence" };
  if (logicalPath.endsWith("evidence_binding.requirement_id")) return { provider_kind: "family_evidence_requirement" };
  return undefined;
}

/** Validate every C4A-collected RoadmapId use against its exact global provider universe. */
export function validateSemanticRoadmapJoins(
  indexes: RoadmapIndexes,
  universe: SemanticJoinUniverse = indexes,
  source = `<roadmap:${indexes.roadmap}>`,
  deferForeignRoadmapJoins?: "matrix" | "testing",
  deferGuardedReopenPairing = false,
): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  for (const use of indexes.id_uses) {
    const sourceProvider = [...indexes.payload_records.values()].find((provider) =>
      use.logical_path.startsWith(`${provider.logical_path}.`)
    );
    const target = universe.first_class.get(use.id);
    if (target === undefined) {
      const guard = universe.current_guards?.find((candidate) => candidate.id === use.id);
      const expectedGuardRole = use.role === "semantic_target"
        ? guardedSemanticRole(use.logical_path, sourceProvider?.payload)
        : undefined;
      // Relation endpoint legality, including root-only guarded reopens, is owned by
      // validateRelations. Do not diagnose the same guarded target as an unresolved payload join.
      if (guard !== undefined && /^relation\[\d+\]\.target$/u.test(use.logical_path)) continue;
      if (guard !== undefined && expectedGuardRole !== undefined) {
        if (guard.guard_role !== expectedGuardRole) {
          issues.push(issue(
            "E-REFERENCE-FORBIDDEN",
            source,
            use.logical_path,
            `roadmap ID ${JSON.stringify(use.id)} resolves to guard role ${guard.guard_role ?? "untyped"}, expected ${expectedGuardRole}`,
          ));
        }
        continue;
      }
      if (deferredForeignTarget(use.id, deferForeignRoadmapJoins, universe.first_class)) continue;
      issues.push(issue(
        "E-REFERENCE-UNRESOLVED",
        source,
        use.logical_path,
        `roadmap ID ${JSON.stringify(use.id)} has no active first-class provider`,
      ));
      continue;
    }
    const structuralKind = use.role === "parent_record" || use.role === "manifest_record"
      ? "record"
      : undefined;
    if (structuralKind !== undefined && target.kind !== structuralKind) {
      issues.push(issue(
        "E-REFERENCE-FORBIDDEN",
        source,
        use.logical_path,
        `roadmap ID ${JSON.stringify(use.id)} resolves to ${target.kind}, expected ${structuralKind}`,
      ));
      continue;
    }
    if (use.role !== "semantic_target") continue;
    const expected = semanticTargetExpectation(use.logical_path, sourceProvider?.payload);
    if (expected === undefined) {
      issues.push(issue(
        "E-REFERENCE-FORBIDDEN",
        source,
        use.logical_path,
        "semantic roadmap-ID field has no closed join policy",
      ));
      continue;
    }
    if ("provider_kind" in expected) {
      if (target.kind !== expected.provider_kind) {
        issues.push(issue(
          "E-REFERENCE-FORBIDDEN",
          source,
          use.logical_path,
          `roadmap ID ${JSON.stringify(use.id)} resolves to ${target.kind}, expected ${expected.provider_kind}`,
        ));
      }
      continue;
    }
    const payload = universe.payload_records.get(target.owner_record_id);
    if (payload === undefined || payload.payload.kind !== expected.payload_kind) {
      issues.push(issue(
        "E-REFERENCE-FORBIDDEN",
        source,
        use.logical_path,
        `roadmap ID ${JSON.stringify(use.id)} must resolve to a ${expected.payload_kind} payload`,
      ));
      continue;
    }
    if (
      expected.work_kind !== undefined &&
      (payload.payload.kind !== "work" || payload.payload.work_kind !== expected.work_kind)
    ) {
      issues.push(issue(
        "E-REFERENCE-FORBIDDEN",
        source,
        use.logical_path,
        `roadmap ID ${JSON.stringify(use.id)} must resolve to work_kind ${expected.work_kind}`,
      ));
    }
    if (
      expected.transition_kinds !== undefined &&
      (
        payload.payload.kind !== "signal" ||
        !expected.transition_kinds.includes(payload.payload.transition_kind)
      )
    ) {
      issues.push(issue(
        "E-REFERENCE-FORBIDDEN",
        source,
        use.logical_path,
        expected.transition_kinds.length === 0
          ? "semantic transition field has no valid state-specific target policy"
          : `roadmap ID ${JSON.stringify(use.id)} must resolve to transition kind ${expected.transition_kinds.join("|")}`,
      ));
    }
    if (
      expected.control_state !== undefined &&
      (payload.payload.kind !== "control" || payload.payload.control_state !== expected.control_state)
    ) {
      issues.push(issue(
        "E-REFERENCE-FORBIDDEN",
        source,
        use.logical_path,
        `roadmap ID ${JSON.stringify(use.id)} must resolve to a ${expected.control_state} control`,
      ));
    }
  }
  for (const provider of indexes.payload_records.values()) {
    const payload = provider.payload;
    const expectedCount = payload.kind === "work" && ["blocked", "armed", "deferred", "waiting_external"].includes(payload.work_state)
      ? 1
      : payload.kind === "decision" && (payload.decision_state === "pending" || payload.decision_state === "held" || payload.permanence === "reopenable")
      ? 1
      : payload.kind === "matrix_external_closeout"
      ? 1
      : undefined;
    if (expectedCount !== undefined) {
      const transitionIds = "transition_ids" in payload ? payload.transition_ids ?? [] : [];
      if (transitionIds.length !== expectedCount) {
        issues.push(issue(
          "E-REFERENCE-FORBIDDEN",
          source,
          `${provider.logical_path}.transition_ids`,
          `state-specific transition list must contain exactly ${expectedCount} target`,
        ));
      }
    }
    if (payload.kind === "work" && payload.work_state === "delegated") {
      const edges = indexes.relations.filter((relation) =>
        relation.source === provider.record.id && relation.kind === "delegates_to"
      );
      if (edges.length !== 1) {
        issues.push(issue(
          "E-REFERENCE-FORBIDDEN",
          source,
          `${provider.logical_path}.delegates_to`,
          "delegated work requires exactly one outgoing delegates_to relation",
        ));
      }
    }
  }
  if (!deferGuardedReopenPairing) {
    issues.push(...validateGuardedFamilyReopens(
      indexes.payload_records,
      indexes.relations,
      universe.current_guards ?? [],
      source,
    ));
  }
  return sortIssues(issues);
}
