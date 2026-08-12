import type {
  FileHeadingFact,
  GateFact,
  ReferenceProvider,
  RegistryView,
  Resolution,
  RoadmapCitationFact,
  TestSymbolFact,
} from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import type {
  RoadmapIndexes,
  RoadmapIdProviderFact,
  SemanticPayloadProviderFact,
} from "./indexes.ts";
import { validateRoadmapId } from "./ids.ts";
import type { RepoPath, RoadmapId } from "./model/core.ts";
import type { Reference } from "./model/documents.ts";

const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

const textDecoder = new TextDecoder("utf-8", { fatal: true });
const textEncoder = new TextEncoder();

export interface TrackedTextInput {
  readonly source: RepoPath;
  /** Undefined represents a tracked regular path whose revision-scoped bytes are missing. */
  readonly bytes?: Uint8Array;
}

export interface RepositoryFactResult<T> {
  readonly facts: readonly T[];
  readonly issues: readonly RoadmapIssue[];
}

/** Narrow range-read surface implemented by the immutable expected/legacy Markdown views. */
export interface RoadmapMarkdownByteView {
  readonly byte_length: number;
  sliceBytes(start: number, end: number): Uint8Array;
}

export interface RoadmapMarkdownRepositoryFacts {
  readonly citations: readonly RoadmapCitationFact[];
  readonly headings: readonly FileHeadingFact[];
  readonly stable_anchor_ids: readonly RoadmapId[];
  readonly issues: readonly RoadmapIssue[];
}

export interface ReferenceValidationOptions {
  readonly source?: string;
  readonly unresolved_migration_authority?: UnresolvedMigrationAuthority;
  readonly providers?: readonly ReferenceProviderLike[];
  /** Global first-class view; defaults to the selected document for single-roadmap validation. */
  readonly first_class?: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>;
  /** Scoped lanes defer only well-formed opposite-namespace RoadmapId targets to WP5C. */
  readonly defer_foreign_roadmap_joins?: "matrix" | "testing";
}

export interface SemanticJoinUniverse {
  readonly first_class: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>;
  readonly payload_records: ReadonlyMap<RoadmapId, SemanticPayloadProviderFact>;
}

export type AnyReferenceProvider = {
  [K in Reference["kind"]]: ReferenceProvider<K>;
}[Reference["kind"]];

export type ReferenceProviderLike = ReferenceProvider | AnyReferenceProvider;

export interface ReferenceProviderRegistry {
  readonly by_kind: ReadonlyMap<ReferenceKind, ReferenceProviderLike>;
  readonly issues: readonly RoadmapIssue[];
}

declare const unresolvedMigrationAuthorityBrand: unique symbol;
export interface UnresolvedMigrationAuthority {
  readonly [unresolvedMigrationAuthorityBrand]: true;
}

export interface UnresolvedMigrationDebtFact {
  readonly reference_id: Reference["id"];
  readonly source: RoadmapId;
  readonly local_reference: string;
  readonly expires_at: string;
  readonly shadow_record_ids: readonly RoadmapId[];
}

export interface UnresolvedMigrationAuthorityResult {
  readonly authority?: UnresolvedMigrationAuthority;
  readonly debt: readonly UnresolvedMigrationDebtFact[];
  readonly issues: readonly RoadmapIssue[];
}

const unresolvedAuthorityFacts = new WeakMap<object, ReadonlySet<string>>();

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

function sortIssues(values: RoadmapIssue[]): readonly RoadmapIssue[] {
  return Object.freeze(values.sort((left, right) =>
    codePointSort(left.source, right.source) ||
    codePointSort(left.logical_path, right.logical_path) ||
    (left.span?.start_byte ?? -1) - (right.span?.start_byte ?? -1) ||
    codePointSort(left.code, right.code) ||
    codePointSort(left.message, right.message)
  ));
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
  unresolvedMigrationAuthority?: UnresolvedMigrationAuthority,
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
    provider("unresolved_migration", (reference) => {
        const authorized = unresolvedMigrationAuthority === undefined
          ? undefined
          : unresolvedAuthorityFacts.get(unresolvedMigrationAuthority);
        return authorized?.has(unresolvedReferenceKey(reference)) === true
          ? resolved(`unresolved_migration:${reference.local_reference}:${reference.expires_at}`)
          : unresolved("forbidden: unresolved migration reference lacks derived frozen-shadow enumeration authority");
    }),
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
  "unresolved_migration",
] as const satisfies readonly Reference["kind"][]);

export function collectReferenceProviders(
  claims: readonly ReferenceProviderLike[],
  source?: string,
): ReferenceProviderRegistry;
export function collectReferenceProviders(
  firstClass: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>,
  injected: readonly ReferenceProviderLike[],
  unresolvedMigrationAuthority?: UnresolvedMigrationAuthority,
  source?: string,
): ReferenceProviderRegistry;
export function collectReferenceProviders(
  claimsOrFirstClass: readonly ReferenceProviderLike[] | ReadonlyMap<RoadmapId, RoadmapIdProviderFact>,
  injectedOrSource: readonly ReferenceProviderLike[] | string = [],
  unresolvedMigrationAuthority?: UnresolvedMigrationAuthority,
  explicitSource?: string,
): ReferenceProviderRegistry {
  let claims: readonly ReferenceProviderLike[];
  if (Array.isArray(claimsOrFirstClass)) {
    claims = claimsOrFirstClass as readonly ReferenceProviderLike[];
  } else {
    claims = [
      ...createCoreReferenceProviders(
        claimsOrFirstClass as ReadonlyMap<RoadmapId, RoadmapIdProviderFact>,
        unresolvedMigrationAuthority,
      ),
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
    case "unresolved_migration": return [reference.local_reference, reference.expires_at];
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

function unresolvedReferenceKey(reference: Extract<Reference, { kind: "unresolved_migration" }>): string {
  return JSON.stringify([reference.id, reference.source, reference.local_reference, reference.expires_at]);
}

function roadmapNamespace(id: RoadmapId): "matrix" | "testing" | undefined {
  return id.startsWith("matrix.") ? "matrix" : id.startsWith("testing.") ? "testing" : undefined;
}

function deferredForeignTarget(
  id: RoadmapId,
  local: "matrix" | "testing" | undefined,
  firstClass: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>,
): boolean {
  const target = roadmapNamespace(id);
  return local !== undefined && target !== undefined && target !== local && !firstClass.has(id);
}

/**
 * Derive the only authority capable of resolving enumerated migration placeholders. The authority
 * is bound in a private WeakMap to this index's exact unresolved tuples and cannot be caller-minted.
 */
export function deriveUnresolvedMigrationAuthority(
  indexes: RoadmapIndexes,
  source = `<roadmap:${indexes.roadmap}>`,
): UnresolvedMigrationAuthorityResult {
  const unresolvedReferences = [...indexes.references.values()].filter(
    (reference): reference is Extract<Reference, { kind: "unresolved_migration" }> =>
      reference.kind === "unresolved_migration",
  ).sort(compareReferenceTargets);
  const shadowRecordIds = [...indexes.payload_records.values()]
    .filter((provider) => provider.authority === "semantic_shadow")
    .map((provider) => provider.record.id)
    .sort(codePointSort);
  const referencedIds = new Set(indexes.reference_id_uses.map((use) => use.id));
  const issues: RoadmapIssue[] = [];
  const debt: UnresolvedMigrationDebtFact[] = unresolvedReferences.map((reference) => ({
    reference_id: reference.id,
    source: reference.source,
    local_reference: reference.local_reference,
    expires_at: reference.expires_at,
    shadow_record_ids: Object.freeze([...shadowRecordIds]),
  }));
  if (unresolvedReferences.length === 0) return { debt: Object.freeze(debt), issues: [] };
  if (shadowRecordIds.length === 0) {
    issues.push(issue(
      "E-REFERENCE-FORBIDDEN",
      source,
      "reference.unresolved_migration",
      "unresolved migration references require at least one decoded semantic_shadow migration owner",
    ));
  }
  for (const reference of unresolvedReferences) {
    if (referencedIds.has(reference.id)) {
      issues.push(issue(
        "E-REFERENCE-FORBIDDEN",
        source,
        `reference[${JSON.stringify(reference.id)}]`,
        "unresolved migration reference must remain enumerated debt and cannot be consumed by semantic authority",
      ));
    }
  }
  if (issues.length > 0) return { debt: Object.freeze(debt), issues: sortIssues(issues) };
  const authority = Object.freeze({}) as UnresolvedMigrationAuthority;
  unresolvedAuthorityFacts.set(authority, new Set(unresolvedReferences.map(unresolvedReferenceKey)));
  return { authority, debt: Object.freeze(debt), issues: [] };
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
      ...createCoreReferenceProviders(firstClass, options.unresolved_migration_authority),
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
): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  for (const use of indexes.id_uses) {
    const target = universe.first_class.get(use.id);
    if (target === undefined) {
      if (deferredForeignTarget(use.id, deferForeignRoadmapJoins, universe.first_class)) continue;
      issues.push(issue(
        "E-REFERENCE-UNRESOLVED",
        source,
        use.logical_path,
        `roadmap ID ${JSON.stringify(use.id)} has no active first-class provider`,
      ));
      continue;
    }
    const structuralKind = use.role === "parent_record" || use.role === "manifest_record" ||
        use.role === "span_record_owner"
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
    const sourceProvider = [...indexes.payload_records.values()].find((provider) =>
      use.logical_path.startsWith(`${provider.logical_path}.`)
    );
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
  return sortIssues(issues);
}

function decodeTrackedText(input: TrackedTextInput, issues: RoadmapIssue[]): string | undefined {
  if (input.source === "draft" || input.source.startsWith("draft/")) return undefined;
  if (input.bytes === undefined) {
    issues.push(issue("E-SOURCE-MISSING", input.source, "tracked-text", "tracked regular file is missing from the selected revision"));
    return undefined;
  }
  if (input.bytes.includes(0)) return undefined;
  try {
    const text = textDecoder.decode(input.bytes);
    if (text.includes("\r")) {
      issues.push(issue("E-SOURCE-LINE-END", input.source, "tracked-text", "tracked text must use LF line endings"));
      return undefined;
    }
    return text;
  } catch {
    issues.push(issue("E-SOURCE-UTF8", input.source, "tracked-text", "tracked text must be strict UTF-8"));
    return undefined;
  }
}

function trackedCitationBytes(
  input: TrackedTextInput,
  issues: RoadmapIssue[],
): Uint8Array | undefined {
  if (input.source === "draft" || input.source.startsWith("draft/")) return undefined;
  if (input.bytes === undefined) {
    issues.push(issue("E-SOURCE-MISSING", input.source, "tracked-text", "tracked regular file is missing from the selected revision"));
    return undefined;
  }
  return input.bytes.includes(0) ? undefined : input.bytes;
}

/** Scan revision-injected tracked bytes for canonical durable roadmap citations. */
export function scanRoadmapCitations(
  inputs: readonly TrackedTextInput[],
): RepositoryFactResult<RoadmapCitationFact> {
  const facts: RoadmapCitationFact[] = [];
  const issues: RoadmapIssue[] = [];
  const sorted = [...inputs].sort((left, right) => codePointSort(left.source, right.source));
  for (const input of sorted) {
    const sourceBytes = trackedCitationBytes(input, issues);
    if (sourceBytes === undefined) continue;
    const readByte = (offset: number): number => sourceBytes[offset]!;
    let cursor = 0;
    while ((cursor = indexOfBytes(sourceBytes, ROADMAP_CITATION_PREFIX, cursor)) >= 0) {
      const valueStart = cursor + ROADMAP_CITATION_PREFIX.byteLength;
      const token = tokenizeRoadmapCitation(sourceBytes.byteLength, readByte, valueStart);
      if (token === undefined) {
        cursor = valueStart;
        continue;
      }
      const raw = textDecoder.decode(sourceBytes.slice(cursor, token.end_byte));
      const value = raw.slice("roadmap:".length);
      const validation = validateRoadmapId(value);
      const span = { start_byte: cursor, end_byte: token.end_byte };
      if (!validation.ok) {
        issues.push(issue(validation.code, input.source, "roadmap-citation", validation.message, span));
      } else {
        facts.push({ id: validation.id, source: input.source, span, raw });
      }
      cursor = token.end_byte;
    }
  }
  facts.sort((left, right) =>
    codePointSort(left.source, right.source) || left.span.start_byte - right.span.start_byte ||
    left.span.end_byte - right.span.end_byte || codePointSort(left.id, right.id)
  );
  return { facts: Object.freeze(facts), issues: sortIssues(issues) };
}

const ROADMAP_CITATION_PREFIX = textEncoder.encode("roadmap:");
const BYTE_WINDOW = 64 * 1024;

function indexOfBytes(haystack: Uint8Array, needle: Uint8Array, from: number): number {
  outer: for (let cursor = from; cursor + needle.byteLength <= haystack.byteLength; cursor += 1) {
    for (let index = 0; index < needle.byteLength; index += 1) {
      if (haystack[cursor + index] !== needle[index]) continue outer;
    }
    return cursor;
  }
  return -1;
}

function byteReader(view: RoadmapMarkdownByteView): (offset: number) => number {
  let start = -1;
  let bytes: Uint8Array = new Uint8Array();
  return (offset: number): number => {
    if (offset < 0 || offset >= view.byte_length) throw new RangeError(`byte offset ${offset} is outside the Markdown view`);
    if (offset < start || offset >= start + bytes.byteLength) {
      start = Math.floor(offset / BYTE_WINDOW) * BYTE_WINDOW;
      bytes = view.sliceBytes(start, Math.min(view.byte_length, start + BYTE_WINDOW));
    }
    return bytes[offset - start]!;
  };
}

function asciiAlphanumeric(byte: number): boolean {
  return (byte >= 0x30 && byte <= 0x39) || (byte >= 0x41 && byte <= 0x5a) ||
    (byte >= 0x61 && byte <= 0x7a);
}

function asciiLowercase(byte: number): boolean {
  return byte >= 0x61 && byte <= 0x7a;
}

function identifierLookingByte(byte: number): boolean {
  return asciiAlphanumeric(byte) || byte === 0x2e || byte === 0x5f || byte === 0x2d;
}

interface RoadmapCitationToken {
  readonly end_byte: number;
}

/**
 * Tokenize the suffix after `roadmap:` once for both tracked-file and immutable-view scanners.
 * Separators belong to the token only when a lowercase component can follow. A separator followed
 * by another identifier-looking starter is retained as a malformed candidate so validation fails
 * at the complete candidate span; terminal sentence punctuation remains outside a valid fact.
 */
function tokenizeRoadmapCitation(
  byteLength: number,
  readByte: (offset: number) => number,
  valueStart: number,
): RoadmapCitationToken | undefined {
  if (valueStart >= byteLength || !asciiAlphanumeric(readByte(valueStart))) return undefined;
  let end = valueStart;
  while (end < byteLength) {
    const byte = readByte(end);
    if (asciiAlphanumeric(byte) || byte === 0x5f) {
      end += 1;
      continue;
    }
    if (byte !== 0x2e && byte !== 0x2d) break;
    const next = end + 1 < byteLength ? readByte(end + 1) : undefined;
    if (next !== undefined && asciiLowercase(next)) {
      end += 1;
      continue;
    }
    if (next !== undefined && (asciiAlphanumeric(next) || next === 0x5f)) {
      // Preserve an identifier-looking invalid component in the diagnostic span. This branch is
      // never a valid tokenization: the exact RoadmapId validator below rejects the retained text.
      end += 2;
      while (end < byteLength && identifierLookingByte(readByte(end))) end += 1;
    }
    break;
  }
  return { end_byte: end };
}

function validateMarkdownViewText(
  source: RepoPath,
  view: RoadmapMarkdownByteView,
  readByte: (offset: number) => number,
  issues: RoadmapIssue[],
): boolean {
  const decoder = new TextDecoder("utf-8", { fatal: true });
  try {
    for (let start = 0; start < view.byte_length; start += BYTE_WINDOW) {
      const end = Math.min(view.byte_length, start + BYTE_WINDOW);
      decoder.decode(view.sliceBytes(start, end), { stream: end < view.byte_length });
    }
    decoder.decode();
  } catch {
    issues.push(issue("E-SOURCE-UTF8", source, "tracked-text", "tracked text must be strict UTF-8"));
    return false;
  }
  for (let offset = 0; offset < view.byte_length; offset += 1) {
    if (readByte(offset) === 0x0d) {
      issues.push(issue("E-SOURCE-LINE-END", source, "tracked-text", "tracked text must use LF line endings"));
      return false;
    }
  }
  return true;
}

/**
 * Scan one immutable roadmap Markdown view without materializing its conceptual whole-file bytes.
 * This is the projection-path counterpart of scanRoadmapCitations. Its caller selects provenance
 * by lifecycle stage: authoritative Markdown input for legacy/shadow, or expected projection bytes
 * for authoritative v1.
 */
export function scanRoadmapMarkdownFacts(
  source: RepoPath,
  view: RoadmapMarkdownByteView,
): RoadmapMarkdownRepositoryFacts {
  const citations: RoadmapCitationFact[] = [];
  const headings: FileHeadingFact[] = [];
  const stableAnchorIds: RoadmapId[] = [];
  const issues: RoadmapIssue[] = [];
  const readByte = byteReader(view);
  if (!validateMarkdownViewText(source, view, readByte, issues)) {
    return { citations: Object.freeze([]), headings: Object.freeze([]),
      stable_anchor_ids: Object.freeze([]), issues: sortIssues(issues) };
  }

  for (let cursor = 0; cursor + ROADMAP_CITATION_PREFIX.byteLength <= view.byte_length;) {
    let prefixMatches = true;
    for (let index = 0; index < ROADMAP_CITATION_PREFIX.byteLength; index += 1) {
      if (readByte(cursor + index) !== ROADMAP_CITATION_PREFIX[index]) {
        prefixMatches = false;
        break;
      }
    }
    if (!prefixMatches) {
      cursor += 1;
      continue;
    }
    const valueStart = cursor + ROADMAP_CITATION_PREFIX.byteLength;
    const token = tokenizeRoadmapCitation(view.byte_length, readByte, valueStart);
    if (token === undefined) {
      cursor = valueStart;
      continue;
    }
    const raw = textDecoder.decode(view.sliceBytes(cursor, token.end_byte));
    const value = raw.slice("roadmap:".length);
    const validation = validateRoadmapId(value);
    const span = { start_byte: cursor, end_byte: token.end_byte };
    if (!validation.ok) {
      issues.push(issue(validation.code, source, "roadmap-citation", validation.message, span));
    } else {
      citations.push({ id: validation.id, source, span, raw });
    }
    cursor = token.end_byte;
  }

  for (let lineStart = 0; lineStart < view.byte_length;) {
    let lineEnd = lineStart;
    while (lineEnd < view.byte_length && readByte(lineEnd) !== 0x0a) lineEnd += 1;
    let hashes = 0;
    while (hashes < 6 && lineStart + hashes < lineEnd && readByte(lineStart + hashes) === 0x23) hashes += 1;
    const line = textDecoder.decode(view.sliceBytes(lineStart, lineEnd));
    if (hashes > 0 && lineStart + hashes < lineEnd && readByte(lineStart + hashes) === 0x20) {
      const match = /^#{1,6} +(.+?)(?: +#*)?$/u.exec(line);
      if (match !== null) {
        const heading = match[1]!;
        const characterStart = line.indexOf(heading);
        const startByte = lineStart + textEncoder.encode(line.slice(0, characterStart)).byteLength;
        headings.push({
          path: source,
          heading,
          span: { start_byte: startByte, end_byte: startByte + textEncoder.encode(heading).byteLength },
        });
      }
    }
    const anchor = /^\s*<a id="roadmap-id-([^"]+)"><\/a>\s*$/u.exec(line);
    if (anchor !== null) {
      const validation = validateRoadmapId(anchor[1]!);
      if (!validation.ok) issues.push(issue(validation.code, source, "roadmap-anchor", validation.message,
        { start_byte: lineStart, end_byte: lineEnd }));
      else stableAnchorIds.push(validation.id);
    }
    lineStart = lineEnd + 1;
  }

  citations.sort((left, right) =>
    codePointSort(left.source, right.source) || left.span.start_byte - right.span.start_byte ||
    left.span.end_byte - right.span.end_byte || codePointSort(left.id, right.id)
  );
  headings.sort((left, right) =>
    codePointSort(left.path, right.path) || left.span.start_byte - right.span.start_byte ||
    codePointSort(left.heading, right.heading)
  );
  stableAnchorIds.sort(codePointSort);
  for (let index = 1; index < stableAnchorIds.length; index++) {
    if (stableAnchorIds[index] === stableAnchorIds[index - 1]) issues.push(issue(
      "E-ID-DUPLICATE", source, "roadmap-anchor",
      `stable roadmap anchor ${JSON.stringify(stableAnchorIds[index])} occurs more than once`,
    ));
  }
  return {
    citations: Object.freeze(citations),
    headings: Object.freeze(headings),
    stable_anchor_ids: Object.freeze(stableAnchorIds),
    issues: sortIssues(issues),
  };
}

function utf8Offsets(value: string): readonly number[] {
  const result: number[] = new Array(value.length + 1).fill(0);
  let bytes = 0;
  for (let index = 0; index < value.length;) {
    result[index] = bytes;
    const point = value.codePointAt(index)!;
    const width = point > 0xffff ? 2 : 1;
    bytes += textEncoder.encode(String.fromCodePoint(point)).byteLength;
    for (let offset = 1; offset <= width; offset += 1) result[index + offset] = bytes;
    index += width;
  }
  result[value.length] = bytes;
  return result;
}

export interface RustToken {
  readonly text: string;
  readonly start: number;
  readonly end: number;
}

export function rustTokens(source: string): readonly RustToken[] {
  const offsets = utf8Offsets(source);
  const tokens: RustToken[] = [];
  let index = 0;
  const push = (start: number, end: number): void => {
    tokens.push({ text: source.slice(start, end), start: offsets[start]!, end: offsets[end]! });
  };
  const skipQuoted = (quote: string): void => {
    index += 1;
    while (index < source.length) {
      if (source[index] === "\\") index += 2;
      else if (source[index] === quote) { index += 1; break; }
      else index += 1;
    }
  };
  const skipCharacterLiteral = (): boolean => {
    let cursor = index + 1;
    if (cursor >= source.length) return false;
    if (source[cursor] === "\\") {
      cursor += 1;
      if (source[cursor] === "u" && source[cursor + 1] === "{") {
        const close = source.indexOf("}", cursor + 2);
        if (close < 0) return false;
        cursor = close + 1;
      } else if (source[cursor] === "x") {
        cursor += 3;
      } else {
        cursor += 1;
      }
    } else {
      const point = source.codePointAt(cursor)!;
      cursor += point > 0xffff ? 2 : 1;
    }
    if (source[cursor] !== "'") return false;
    index = cursor + 1;
    return true;
  };
  while (index < source.length) {
    if (/\s/u.test(source[index]!)) { index += 1; continue; }
    if (source.startsWith("//", index)) {
      const end = source.indexOf("\n", index + 2);
      index = end < 0 ? source.length : end + 1;
      continue;
    }
    if (source.startsWith("/*", index)) {
      let depth = 1;
      index += 2;
      while (index < source.length && depth > 0) {
        if (source.startsWith("/*", index)) { depth += 1; index += 2; }
        else if (source.startsWith("*/", index)) { depth -= 1; index += 2; }
        else index += 1;
      }
      continue;
    }
    const raw = /^(?:b|c)?r(#+)?"/u.exec(source.slice(index));
    if (raw !== null) {
      const hashes = raw[1] ?? "";
      const close = `"${hashes}`;
      const end = source.indexOf(close, index + raw[0].length);
      index = end < 0 ? source.length : end + close.length;
      continue;
    }
    if (
      source[index] === '"' ||
      ((source[index] === "b" || source[index] === "c") && source[index + 1] === '"')
    ) {
      if (source[index] !== '"') index += 1;
      skipQuoted('"');
      continue;
    }
    if (source[index] === "'") {
      if (skipCharacterLiteral()) continue;
      push(index, index + 1);
      index += 1;
      continue;
    }
    if (/[A-Za-z_]/u.test(source[index]!)) {
      const start = index++;
      while (index < source.length && /[A-Za-z0-9_]/u.test(source[index]!)) index += 1;
      push(start, index);
      continue;
    }
    push(index, index + 1);
    index += 1;
  }
  return tokens;
}

export function matchingRustDelimiter(tokens: readonly RustToken[], open: number): number | undefined {
  const opening = tokens[open]?.text;
  const closing = opening === "{" ? "}" : opening === "[" ? "]" : opening === "(" ? ")" : undefined;
  if (closing === undefined) return undefined;
  let depth = 0;
  for (let index = open; index < tokens.length; index += 1) {
    if (tokens[index]!.text === opening) depth += 1;
    if (tokens[index]!.text === closing && --depth === 0) return index;
  }
  return undefined;
}

function tokenSequenceAt(
  tokens: readonly RustToken[],
  start: number,
  sequence: readonly string[],
): boolean {
  return sequence.every((expected, offset) => tokens[start + offset]?.text === expected);
}

function skipMacroRules(tokens: readonly RustToken[], start: number, end: number): number | undefined {
  if (tokens[start]?.text !== "macro_rules" || tokens[start + 1]?.text !== "!") return undefined;
  let body = start + 2;
  while (body < end && !["{", "[", "("].includes(tokens[body]!.text)) body += 1;
  const close = matchingRustDelimiter(tokens, body);
  return close === undefined || close >= end ? end : close + 1;
}

function isRustIdentifier(token: RustToken | undefined): boolean {
  return token !== undefined && /^[A-Za-z_][A-Za-z0-9_]*$/u.test(token.text);
}

/** Skip a complete path-qualified macro invocation, including its optional item semicolon. */
function skipMacroInvocation(
  tokens: readonly RustToken[],
  start: number,
  end: number,
): number | undefined {
  if (!isRustIdentifier(tokens[start])) return undefined;
  let cursor = start + 1;
  while (
    tokens[cursor]?.text === ":" && tokens[cursor + 1]?.text === ":" &&
    isRustIdentifier(tokens[cursor + 2])
  ) cursor += 3;
  if (tokens[cursor]?.text !== "!" || !["{", "[", "("].includes(tokens[cursor + 1]?.text ?? "")) {
    return undefined;
  }
  const close = matchingRustDelimiter(tokens, cursor + 1);
  if (close === undefined || close >= end) return end;
  return tokens[close + 1]?.text === ";" ? close + 2 : close + 1;
}

const TEST_ROOT_DECLARATION = Object.freeze([
  "#", "[", "cfg", "(", "test", ")", "]", "mod", "tests", ";",
] as const);

interface ModuleDeclaration {
  readonly start: number;
  readonly module_index: number;
  readonly name?: string;
  readonly terminator?: ";" | "{";
  readonly end: number;
  readonly exact_crate_visibility: boolean;
}

function moduleDeclarationAt(
  tokens: readonly RustToken[],
  start: number,
  end: number,
): ModuleDeclaration | undefined {
  let moduleIndex: number | undefined;
  let exactCrateVisibility = false;
  if (tokens[start]?.text === "mod") {
    moduleIndex = start;
  } else if (tokens[start]?.text === "pub" && tokens[start + 1]?.text === "mod") {
    moduleIndex = start + 1;
  } else if (tokens[start]?.text === "pub" && tokens[start + 1]?.text === "(") {
    const close = matchingRustDelimiter(tokens, start + 1);
    if (close === undefined || close >= end) {
      return { start, module_index: start, end, exact_crate_visibility: false };
    }
    if (tokens[close + 1]?.text !== "mod") return undefined;
    moduleIndex = close + 1;
    exactCrateVisibility = close === start + 3 && tokens[start + 2]?.text === "crate";
  }
  if (moduleIndex === undefined) return undefined;
  const name = tokens[moduleIndex + 1]?.text;
  const hasName = name !== undefined && /^[A-Za-z_][A-Za-z0-9_]*$/u.test(name);
  const terminatorToken = hasName ? tokens[moduleIndex + 2]?.text : undefined;
  const terminator = terminatorToken === ";" || terminatorToken === "{"
    ? terminatorToken
    : undefined;
  let declarationEnd = Math.min(end, moduleIndex + (hasName ? 2 : 1));
  if (terminator === ";") declarationEnd = moduleIndex + 3;
  if (terminator === "{") {
    const close = matchingRustDelimiter(tokens, moduleIndex + 2);
    declarationEnd = close === undefined || close >= end ? end : close + 1;
  }
  return {
    start,
    module_index: moduleIndex,
    name: hasName ? name : undefined,
    terminator,
    end: declarationEnd,
    exact_crate_visibility: exactCrateVisibility,
  };
}

function contiguousAttributes(
  tokens: readonly RustToken[],
  start: number,
  end: number,
): { readonly attributes: readonly (readonly RustToken[])[]; readonly next: number } {
  const attributes: RustToken[][] = [];
  let index = start;
  while (tokens[index]?.text === "#" && tokens[index + 1]?.text === "[") {
    const close = matchingRustDelimiter(tokens, index + 1);
    if (close === undefined || close >= end) return { attributes, next: end };
    attributes.push(tokens.slice(index, close + 1));
    index = close + 1;
  }
  return { attributes, next: index };
}

function exactCfgTestAttribute(attribute: readonly RustToken[]): boolean {
  return tokenSequenceAt(attribute, 0, ["#", "[", "cfg", "(", "test", ")", "]"]) &&
    attribute.length === 7;
}

function validateTestRoot(tokens: readonly RustToken[]): readonly string[] {
  const problems: string[] = [];
  let declarations = 0;
  for (let index = 0; index < tokens.length;) {
    const afterMacro = skipMacroRules(tokens, index, tokens.length);
    if (afterMacro !== undefined) { index = afterMacro; continue; }
    const afterInvocation = skipMacroInvocation(tokens, index, tokens.length);
    if (afterInvocation !== undefined) { index = afterInvocation; continue; }
    if (tokens[index]!.text === "#" && tokens[index + 1]?.text === "[") {
      const parsed = contiguousAttributes(tokens, index, tokens.length);
      const declaration = moduleDeclarationAt(tokens, parsed.next, tokens.length);
      if (declaration?.name === "tests") {
        if (
          parsed.attributes.length === 1 && exactCfgTestAttribute(parsed.attributes[0]!) &&
          declaration.start === declaration.module_index && declaration.terminator === ";"
        ) declarations += 1;
        else problems.push("root test module declaration must be exactly #[cfg(test)] mod tests;");
        index = declaration.end;
        continue;
      }
      index = parsed.next;
      continue;
    }
    if (tokenSequenceAt(tokens, index, TEST_ROOT_DECLARATION)) {
      declarations += 1;
      index += TEST_ROOT_DECLARATION.length;
      continue;
    }
    if (tokens[index]!.text === "{") {
      const close = matchingRustDelimiter(tokens, index);
      index = close === undefined ? tokens.length : close + 1;
      continue;
    }
    const declaration = moduleDeclarationAt(tokens, index, tokens.length);
    if (declaration?.name === "tests") {
      problems.push("root test module declaration must be exactly #[cfg(test)] mod tests;");
      index = declaration.end;
      continue;
    }
    index += 1;
  }
  if (declarations !== 1) {
    problems.push(`expected exactly one root #[cfg(test)] mod tests; declaration, found ${declarations}`);
  }
  return Object.freeze(problems);
}

interface DeclaredTestModules {
  readonly modules: readonly string[];
  readonly problems: readonly string[];
}

function declaredTestModules(tokens: readonly RustToken[]): DeclaredTestModules {
  const modules: string[] = [];
  const problems: string[] = [];
  for (let index = 0; index < tokens.length;) {
    const afterMacro = skipMacroRules(tokens, index, tokens.length);
    if (afterMacro !== undefined) { index = afterMacro; continue; }
    const afterInvocation = skipMacroInvocation(tokens, index, tokens.length);
    if (afterInvocation !== undefined) { index = afterInvocation; continue; }
    if (tokens[index]!.text === "#" && tokens[index + 1]?.text === "[") {
      const parsed = contiguousAttributes(tokens, index, tokens.length);
      const attributedModule = moduleDeclarationAt(tokens, parsed.next, tokens.length);
      if (attributedModule !== undefined) {
        problems.push(`test module declaration at byte ${tokens[index]!.start} must be exactly pub(crate) mod <ident>;`);
        index = attributedModule.end;
      } else {
        index = parsed.next;
      }
      continue;
    }
    if (tokens[index]!.text === "{") {
      const close = matchingRustDelimiter(tokens, index);
      index = close === undefined ? tokens.length : close + 1;
      continue;
    }
    const declaration = moduleDeclarationAt(tokens, index, tokens.length);
    if (declaration !== undefined) {
      if (
        declaration.exact_crate_visibility && declaration.name !== undefined &&
        declaration.terminator === ";"
      ) modules.push(declaration.name);
      else {
      problems.push(`test module declaration at byte ${tokens[index]!.start} must be exactly pub(crate) mod <ident>;`);
      }
      index = declaration.end;
      continue;
    }
    if (tokens[index]!.text === "pub" && tokens[index + 1]?.text === "(" && matchingRustDelimiter(tokens, index + 1) === undefined) {
      problems.push(`malformed visibility at byte ${tokens[index]!.start} in test module registry`);
      index = tokens.length;
      continue;
    }
    if (tokens[index]!.text === "pub" && tokens[index + 1] === undefined) {
      problems.push(`truncated public declaration at byte ${tokens[index]!.start} in test module registry`);
      index += 1;
      continue;
    }
    index += 1;
  }
  return {
    modules: Object.freeze(modules.sort(codePointSort)),
    problems: Object.freeze(problems.sort(codePointSort)),
  };
}

function scanTestItems(
  source: RepoPath,
  tokens: readonly RustToken[],
  modulePath: readonly string[],
  out: TestSymbolFact[],
  begin = 0,
  end = tokens.length,
): void {
  let testAttribute = false;
  for (let index = begin; index < end;) {
    const afterMacro = skipMacroRules(tokens, index, end);
    if (afterMacro !== undefined) {
      index = afterMacro;
      testAttribute = false;
      continue;
    }
    const afterInvocation = skipMacroInvocation(tokens, index, end);
    if (afterInvocation !== undefined) {
      index = afterInvocation;
      testAttribute = false;
      continue;
    }
    if (["{", "[", "("].includes(tokens[index]!.text)) {
      const close = matchingRustDelimiter(tokens, index);
      const preservesTestAttribute = testAttribute &&
        tokens[index]!.text === "(" && tokens[index - 1]?.text === "pub";
      index = close === undefined || close >= end ? end : close + 1;
      if (!preservesTestAttribute) testAttribute = false;
      continue;
    }
    if (tokens[index]!.text === "#" && tokens[index + 1]?.text === "[") {
      const close = matchingRustDelimiter(tokens, index + 1);
      if (close === undefined || close >= end) return;
      if (close === index + 3 && tokens[index + 2]?.text === "test") testAttribute = true;
      index = close + 1;
      continue;
    }
    if (
      tokens[index]!.text === "mod" &&
      /^[A-Za-z_][A-Za-z0-9_]*$/u.test(tokens[index + 1]?.text ?? "") &&
      tokens[index + 2]?.text === "{"
    ) {
      const close = matchingRustDelimiter(tokens, index + 2);
      if (close === undefined || close > end) return;
      scanTestItems(source, tokens, [...modulePath, tokens[index + 1]!.text], out, index + 3, close);
      index = close + 1;
      testAttribute = false;
      continue;
    }
    let fnIndex = index;
    if (tokens[fnIndex]!.text === "async") fnIndex += 1;
    if (testAttribute && tokens[fnIndex]?.text === "fn") {
      const name = tokens[fnIndex + 1];
      if (name !== undefined && /^[A-Za-z_][A-Za-z0-9_]*$/u.test(name.text)) {
        const symbol = [...modulePath, name.text].join("::");
        out.push({
          test_id: `rust-test:cddl-codegen#${symbol}`,
          symbol,
          source,
          span: { start_byte: name.start, end_byte: name.end },
          module_path: Object.freeze([...modulePath]),
        });
      }
      testAttribute = false;
      index = fnIndex + 2;
      continue;
    }
    if (!["pub", "(", ")", "crate", "async"].includes(tokens[index]!.text)) {
      testAttribute = false;
    }
    index += 1;
  }
}

/** Derive the bounded Rust test-symbol registry from revision-injected tracked bytes. */
export function extractRustTestSymbols(
  inputs: readonly TrackedTextInput[],
): RepositoryFactResult<TestSymbolFact> {
  const facts: TestSymbolFact[] = [];
  const issues: RoadmapIssue[] = [];
  const files = new Map<RepoPath, string>();
  for (const input of [...inputs].sort((left, right) => codePointSort(left.source, right.source))) {
    const text = decodeTrackedText(input, issues);
    if (text !== undefined) files.set(input.source, text);
  }
  const main = files.get("src/main.rs" as RepoPath);
  const testsMod = files.get("src/tests/mod.rs" as RepoPath);
  if (main === undefined) {
    issues.push(issue("E-SOURCE-MISSING", "src/main.rs", "test-symbol-registry", "tracked test root is missing"));
  } else {
    for (const problem of validateTestRoot(rustTokens(main))) {
      issues.push(issue("E-REFERENCE-UNRESOLVED", "src/main.rs", "test-symbol-registry", problem));
    }
  }
  if (testsMod === undefined) {
    issues.push(issue("E-SOURCE-MISSING", "src/tests/mod.rs", "test-symbol-registry", "tracked tests module registry is missing"));
  } else {
    const testsModTokens = rustTokens(testsMod);
    // The registry module is itself a registered source module. Direct tests in this file have the
    // exact `tests::<fn>` identity; child declarations only extend the bounded source universe.
    scanTestItems("src/tests/mod.rs" as RepoPath, testsModTokens, ["tests"], facts);
    const declaration = declaredTestModules(testsModTokens);
    for (const problem of declaration.problems) {
      issues.push(issue("E-REFERENCE-UNRESOLVED", "src/tests/mod.rs", "test-symbol-registry", problem));
    }
    const seen = new Set<string>();
    for (const module of declaration.modules) {
      if (seen.has(module)) {
        issues.push(issue("E-ID-DUPLICATE", "src/tests/mod.rs", `module.${module}`, `test module ${JSON.stringify(module)} is declared more than once`));
        continue;
      }
      seen.add(module);
      const source = `src/tests/${module}.rs` as RepoPath;
      const body = files.get(source);
      if (body === undefined) {
        issues.push(issue("E-SOURCE-MISSING", source, "test-symbol-registry", `declared test module ${JSON.stringify(module)} is missing`));
        continue;
      }
      scanTestItems(source, rustTokens(body), ["tests", module], facts);
    }
  }
  facts.sort((left, right) =>
    codePointSort(left.test_id, right.test_id) || codePointSort(left.source, right.source) ||
    left.span.start_byte - right.span.start_byte || left.span.end_byte - right.span.end_byte
  );
  for (let index = 1; index < facts.length; index += 1) {
    if (facts[index - 1]!.test_id === facts[index]!.test_id) {
      issues.push(issue(
        "E-ID-DUPLICATE",
        facts[index]!.source,
        facts[index]!.test_id,
        `derived test ID ${JSON.stringify(facts[index]!.test_id)} is duplicated`,
        facts[index]!.span,
      ));
    }
  }
  return { facts: Object.freeze(facts), issues: sortIssues(issues) };
}

export function gateFact(id: string, stub = false): GateFact {
  return { id, kind: "cmd", stub };
}

export function headingFact(path: RepoPath, heading: string): FileHeadingFact {
  return { path, heading, span: { start_byte: 0, end_byte: textEncoder.encode(heading).byteLength } };
}
