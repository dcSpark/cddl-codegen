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
import {
  buildRoadmapIndexes,
  type RoadmapIndexes,
  type RoadmapIdProviderFact,
  type SemanticPayloadProviderFact,
} from "./indexes.ts";
import { namespaceOf } from "./ids.ts";
import {
  EXTERNAL_OWNER_REFERENCE_KINDS,
  armOfPayload,
  type AllowedReferenceKinds,
  type PayloadArm,
  type PayloadField,
} from "./payload_descriptors.ts";
import type { ReferenceId, RoadmapId } from "./model/core.ts";
import type { Reference, RoadmapDocument } from "./model/documents.ts";
import type { CurrentGuard, Relation, SemanticPayload } from "./model/documents.ts";
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

const externalOwnerKinds = EXTERNAL_OWNER_REFERENCE_KINDS;

/**
 * Resolve a citing field's descriptor by its path relative to the payload root
 * (`reference_ids`, `predicate.evidence_ids`, `branch["slug"].prune_reference_ids`).
 */
function findFieldDescriptor(
  arm: PayloadArm,
  relative: string,
  accepts: (value: PayloadField["value"]) => boolean,
): PayloadField | undefined {
  const direct = arm.fields.find((entry) => entry.name === relative && accepts(entry.value));
  if (direct !== undefined) return direct;
  const bracket = relative.indexOf("[");
  const dot = relative.indexOf(".");
  let head: string;
  let rest: string;
  if (bracket !== -1 && (dot === -1 || bracket < dot)) {
    const close = relative.indexOf("].");
    if (close === -1) return undefined;
    head = relative.slice(0, bracket);
    rest = relative.slice(close + 2);
  } else {
    if (dot === -1) return undefined;
    head = relative.slice(0, dot);
    rest = relative.slice(dot + 1);
  }
  const parent = arm.fields.find((entry) =>
    entry.name === head && (entry.value.t === "table" || entry.value.t === "array_table")
  );
  if (parent === undefined || (parent.value.t !== "table" && parent.value.t !== "array_table")) {
    return undefined;
  }
  for (const groupArm of parent.value.group.arms) {
    const found = findFieldDescriptor(groupArm, rest, accepts);
    if (found !== undefined) return found;
  }
  return undefined;
}

function armOfPayloadOrUndefined(
  payload: SemanticPayloadProviderFact["payload"],
): PayloadArm | undefined {
  try {
    return armOfPayload(payload);
  } catch {
    // A synthetic or mutated payload outside every closed arm falls back to suffix policy.
    return undefined;
  }
}

function resolveAllowedKinds(
  allowed: AllowedReferenceKinds,
  payload: SemanticPayloadProviderFact["payload"],
): readonly ReferenceKind[] {
  if (Array.isArray(allowed)) return allowed as readonly ReferenceKind[];
  const byField = allowed as Exclude<AllowedReferenceKinds, readonly ReferenceKind[]>;
  const discriminant = (payload as unknown as Record<string, unknown>)[byField.by];
  return typeof discriminant === "string" ? byField.map[discriminant] ?? [] : [];
}

function allowedReferenceKinds(
  payload: SemanticPayloadProviderFact["payload"],
  relative: string,
): readonly ReferenceKind[] {
  const arm = armOfPayloadOrUndefined(payload);
  if (arm !== undefined) {
    const descriptor = findFieldDescriptor(
      arm,
      relative,
      (value) => value.t === "reference_id" || value.t === "reference_id_set",
    );
    if (descriptor !== undefined) {
      const spec = descriptor.value;
      if (spec.t === "reference_id" || spec.t === "reference_id_set") {
        return resolveAllowedKinds(spec.allowed, payload);
      }
    }
  }
  if (relative.endsWith("external_owner_reference_id")) {
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

/**
 * Reference rows nothing cites are garbage: they carry no claim any record makes, and the
 * migration left 131 of them behind. `--format-source` collects them, so a hand edit that drops
 * the last citation of a reference does not leave the row for a reader to interpret. Collection
 * runs BEFORE validation, so residue pointing at a target that has since disappeared is collected
 * rather than blocking the format run.
 */
export function garbageCollectUncitedReferences(
  document: RoadmapDocument,
): { readonly document: RoadmapDocument; readonly collected: readonly ReferenceId[] } {
  const cited = new Set(
    buildRoadmapIndexes(document).indexes.reference_id_uses.map((use) => String(use.id)),
  );
  const collected = document.references
    .filter((reference) => !cited.has(String(reference.id)))
    .map((reference) => reference.id);
  if (collected.length === 0) return { document, collected: Object.freeze([]) };
  return {
    document: {
      ...document,
      references: document.references.filter((reference) => cited.has(String(reference.id))),
    },
    collected: Object.freeze(collected),
  };
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
      const allowed = allowedReferenceKinds(
        consumer.payload,
        use.logical_path.slice(consumer.logical_path.length + 1),
      );
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
      readonly control_state?: "live";
    };

/**
 * Join policy for a roadmap-ID citing field.  When the use resolves to a descriptor field on the
 * source payload's arm, the policy is that field's declared target; otherwise the closed suffix
 * fallback below answers for synthetic uses that sit under no descriptor field.
 */
function semanticTargetExpectation(
  logicalPath: string,
  sourcePayload: SemanticPayloadProviderFact["payload"] | undefined,
  relative?: string,
): ExpectedSemanticTarget | undefined {
  if (sourcePayload !== undefined && relative !== undefined) {
    const arm = armOfPayloadOrUndefined(sourcePayload);
    if (arm !== undefined) {
      const descriptor = findFieldDescriptor(
        arm,
        relative,
        (value) => value.t === "roadmap_id" || value.t === "roadmap_id_set",
      );
      if (descriptor !== undefined) {
        const spec = descriptor.value;
        if (spec.t === "roadmap_id" || spec.t === "roadmap_id_set") {
          const target = spec.target;
          return {
            payload_kind: target.payload_kind,
            ...(target.work_kind === undefined ? {} : { work_kind: target.work_kind }),
            ...(target.control_state === undefined ? {} : { control_state: target.control_state }),
          };
        }
      }
    }
  }
  if (logicalPath.endsWith("control_ids")) return { payload_kind: "control" };
  if (logicalPath.endsWith("regression_gap_ids")) return { payload_kind: "work", work_kind: "regression_gap" };
  if (logicalPath.endsWith("work_ids") || logicalPath.endsWith("work_id")) return { payload_kind: "work" };
  if (logicalPath.endsWith("admission_ids")) return { payload_kind: "testing_system_admission" };
  if (logicalPath.endsWith("incident_ids")) return { payload_kind: "testing_incident" };
  if (logicalPath.endsWith("evidence_ids") || logicalPath.endsWith("current_evidence_ids") || logicalPath.endsWith("regression_evidence_ids")) {
    return { payload_kind: "evidence" };
  }
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
      // Relation endpoint legality is owned by validateRelations. Do not diagnose the same
      // guarded target as an unresolved payload join.
      if (guard !== undefined && /^relation\[\d+\]\.target$/u.test(use.logical_path)) continue;
      if (deferredForeignTarget(use.id, deferForeignRoadmapJoins, universe.first_class)) continue;
      issues.push(issue(
        "E-REFERENCE-UNRESOLVED",
        source,
        use.logical_path,
        `roadmap ID ${JSON.stringify(use.id)} has no active first-class provider`,
      ));
      continue;
    }
    const structuralKind = use.role === "parent_record" || use.role === "section_entry"
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
    const expected = semanticTargetExpectation(
      use.logical_path,
      sourceProvider?.payload,
      sourceProvider === undefined
        ? undefined
        : use.logical_path.slice(sourceProvider.logical_path.length + 1),
    );
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
