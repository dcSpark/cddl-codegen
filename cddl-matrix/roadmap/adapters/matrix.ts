import type { IssueCollector, RoadmapIssue } from "../errors.ts";
import { EMPTY_DENOMINATOR_AUTHORITIES, validateSystematicFamilies, type DenominatorAuthorityRegistry } from "../denominator.ts";
import { buildRoadmapIndexes, type RoadmapIndexes } from "../indexes.ts";
import { deriveMatrixStatusFacts, renderMatrixStatusPayloads } from "../matrix_status_facts.ts";
import type { RepoPath, RoadmapId, RoadmapName, SlotId } from "../model/core.ts";
import type {
  Reference,
  RoadmapDocument,
  SemanticPayload,
  SemanticRecord,
} from "../model/documents.ts";
import type { MatrixSemanticPayload } from "../model/matrix.ts";
import type { SemanticPayloadProviderFact } from "../indexes.ts";
import {
  validateRoadmapReferences,
  validateSemanticRoadmapJoins,
  type ReferenceProviderLike,
  type SemanticJoinUniverse,
} from "../references.ts";
import { validateRelations } from "../relations.ts";
import type {
  FieldConsumer,
  GeneratedSlotResolver,
  Indexes,
  ReferenceProvider,
  RegistryView,
  RoadmapAdapter,
} from "./types.ts";
import { MATRIX_SLOT_FLOORS } from "./matrix_slot_floors.ts";
import { namespaceOf } from "../ids.ts";
import { concatenate } from "../kernel.ts";
import { sortRoadmapIssues as sortIssues } from "../errors.ts";

const MATRIX_SOURCE_PATH = "cddl-matrix/roadmap.toml" as RepoPath;
const MATRIX_PROJECTION_PATH = "cddl-matrix/ROADMAP.md" as RepoPath;
const SLOT_BINDINGS = Object.freeze([
  ["constraint" as SlotId, "status_header_markers:roadmap-constraint"],
  ["counts" as SlotId, "status_header_markers:roadmap-counts"],
  ["emission" as SlotId, "status_header_markers:roadmap-emission"],
  ["ops" as SlotId, "status_header_markers:roadmap-ops"],
] as const);

function issue(
  provider: SemanticPayloadProviderFact,
  source: string,
  code: "E-SCHEMA-STATE" | "E-SCHEMA-FLOOR",
  logicalPath: string,
  message: string,
): RoadmapIssue {
  return {
    code,
    source,
    logical_path: `${provider.logical_path}.${logicalPath}`,
    message,
    exit: 1,
  };
}

function usesLiveMatrixInlineSlots(doc: RoadmapDocument): boolean {
  if (
    doc.document.source_path !== MATRIX_SOURCE_PATH ||
    doc.document.projection_path !== MATRIX_PROJECTION_PATH ||
    doc.generated_slots.length !== MATRIX_SLOT_FLOORS.length
  ) return false;
  const slots = new Map(doc.generated_slots.map((slot) => [slot.slot_id, slot]));
  return slots.size === MATRIX_SLOT_FLOORS.length && MATRIX_SLOT_FLOORS.every(
    ([slotId, binding, spanId, start, end]) => {
      const slot = slots.get(slotId);
      const span = doc.spans.find((entry) => entry.id === spanId);
      return slot?.binding === binding && slot.span_ids.length === 1 &&
        slot.span_ids[0] === spanId && span?.source_kind === "generated_slot" &&
        span.owner_id === slotId && span.owner_field === "generated" &&
        span.migration_status === "generated" && span.start_byte === start && span.end_byte === end;
    },
  );
}

function payloadAt(indexes: Indexes, id: RoadmapId): SemanticPayload | undefined {
  if ("payload_records" in indexes) {
    return (indexes as Indexes & Pick<RoadmapIndexes, "payload_records">).payload_records.get(id)?.payload;
  }
  return indexes.records.get(id)?.payload;
}

function requirePayloadKind(
  provider: SemanticPayloadProviderFact,
  source: string,
  indexes: Indexes,
  id: RoadmapId,
  field: string,
  predicate: (payload: SemanticPayload) => boolean,
  expected: string,
  out: IssueCollector,
): void {
  const target = payloadAt(indexes, id);
  const deferred = (indexes as Indexes & { readonly deferred_foreign_roadmap_joins?: RoadmapName })
    .deferred_foreign_roadmap_joins;
  const targetNamespace = namespaceOf(id);
  if (target === undefined && deferred !== undefined && targetNamespace !== undefined &&
    targetNamespace !== deferred) return;
  if (target === undefined || !predicate(target)) {
    out.add(issue(provider, source, "E-SCHEMA-STATE", field, `${id} must resolve to ${expected}`));
  }
}

export interface CanonicalSemanticMarkdownField {
  readonly logical_path: string;
  readonly bytes: Uint8Array;
}

export function canonicalSemanticMarkdownFields(
  value: SemanticPayload,
): readonly CanonicalSemanticMarkdownField[] {
  const fields: CanonicalSemanticMarkdownField[] = [];
  const add = (path: string, bytes: Uint8Array | undefined): void => {
    if (bytes !== undefined) {
      fields.push(Object.freeze({ logical_path: `payload.${path}`, bytes }));
    }
  };
  add("summary_md", value.summary_md);
  add("detail_md", value.detail_md);
  switch (value.kind) {
    case "work":
      add("acceptance_md", value.acceptance_md);
      if (value.work_state === "ready") add("priority_rationale_md", value.priority_rationale_md);
      if (value.work_state === "blocked") add("blocker_md", value.blocker_md);
      if (value.work_state === "delegated") add("return_condition_md", value.return_condition_md);
      if (value.work_state === "pending_review") add("uncertainty_md", value.uncertainty_md);
      break;
    case "decision":
      if (value.decision_state === "pending") add("question_md", value.question_md);
      else add("rationale_md", value.rationale_md);
      break;
    case "signal":
      if (value.transition_kind === "promotion_trigger" || value.transition_kind === "reopening_signal") {
        add("action_on_fire_md", value.action_on_fire_md);
        if (value.predicate.predicate_kind === "event") add("predicate.event_md", value.predicate.event_md);
        if (value.predicate.predicate_kind === "manual") add("predicate.review_procedure_md", value.predicate.review_procedure_md);
      } else if (value.transition_kind === "unblock_predicate") {
        add("event_md", value.event_md);
        add("check_procedure_md", value.check_procedure_md);
        add("due_action_md", value.due_action_md);
      } else if (value.transition_kind === "watch_escalation") {
        add("failure_signature_md", value.failure_signature_md);
        add("capture_procedure_md", value.capture_procedure_md);
        add("response_md", value.response_md);
        add("escalation_action_md", value.escalation_action_md);
        add("retirement_semantics_md", value.retirement_semantics_md);
      } else if (value.transition_kind === "retirement_predicate") {
        add("external_predicate_md", value.external_predicate_md);
        add("verification_md", value.verification_md);
        add("due_action_md", value.due_action_md);
      } else {
        add("period_or_event_md", value.period_or_event_md);
        add("checklist_md", value.checklist_md);
        add("missed_action_md", value.missed_action_md);
      }
      break;
    case "evidence":
      add("claim_md", value.claim_md);
      add("command_md", value.command_md);
      add("result_md", value.result_md);
      add("environment_md", value.environment_md);
      add("unprobed_remainder_md", value.unprobed_remainder_md);
      break;
    case "control":
      add("claim_md", value.claim_md);
      add("boundary_md", value.boundary_md);
      break;
    case "family":
      add("goal_md", value.goal_md);
      add("boundary_md", value.boundary_md);
      if (value.family_maturity !== "observed_only") {
        add("derivation_md", value.derivation_md);
        add("legality_rule_md", value.legality_rule_md);
      }
      if (value.family_maturity === "under_design") {
        add("denominator_unknowns_md", value.denominator_unknowns_md);
      }
      value.exclusions.forEach((entry, index) => add(`exclusions[${index}].reason_md`, entry.reason_md));
      break;
    case "matrix_external_closeout":
      add("current_upstream_state_md", value.current_upstream_state_md);
      if (value.closeout_state === "blocked") add("blocker_md", value.blocker_md);
      add("verification_md", value.verification_md);
      value.actions.forEach((entry, index) => add(`actions[${index}].action_md`, entry.action_md));
      value.branches.forEach((entry, index) => add(`branches[${index}].predicate_md`, entry.predicate_md));
      break;
    case "matrix_policy":
      if (value.policy_kind === "maintenance_protocol") add("protocol_md", value.protocol_md);
      else add("rationale_md", value.rationale_md);
      break;
    case "testing_operational_watch":
      add("signature_md", value.signature_md);
      if (value.watch_state !== "watching") add("attribution_md", value.attribution_md);
      add("response_md", value.response_md);
      add("retirement_semantics_md", value.retirement_semantics_md);
      value.capture_steps.forEach((entry, index) => add(`capture_steps[${index}].capture_md`, entry.capture_md));
      break;
    case "testing_incident":
      add("signature_md", value.signature_md);
      if (value.incident_posture !== "live") add("attribution_md", value.attribution_md);
      break;
    case "testing_cost":
      add("scope_md", value.scope_md);
      if (value.cost_posture === "historical_observation") add("environment_md", value.environment_md);
      break;
    case "testing_system_admission":
      add("claim_md", value.claim_md);
      break;
    default: {
      const exhaustive: never = value;
      return exhaustive;
    }
  }
  return Object.freeze(fields);
}

/**
 * Consume every decoded Markdown field exactly once. Document-visible converted owners project
 * only reviewed replacement fields in canonical payload order. Semantic-only records intentionally
 * emit no bytes. Metadata Markdown remains ledgered as consumed nonrendering content.
 */
export function renderCanonicalSemanticRecord(
  record: SemanticRecord,
  fields: FieldConsumer,
): Uint8Array {
  const replacements = new Set(record.source_replacements.map((entry) => entry.replacement_field));
  const consumed = canonicalSemanticMarkdownFields(record.payload).map((entry) => {
    const bytes = fields.consume(entry.logical_path, entry.bytes);
    return { path: entry.logical_path, bytes };
  });
  const rendered = record.projection_visibility === "semantic_only"
    ? []
    : consumed.filter((entry) => replacements.has(entry.path));
  return concatenate(rendered.map((entry) => entry.bytes));
}

function exactRegistryProvider(
  kind: "matrix_feature" | "matrix_role" | "matrix_cell",
  members: (view: RegistryView) => readonly { id: string }[],
): ReferenceProvider {
  return Object.freeze({
    kind,
    resolve(reference: Reference, view: RegistryView) {
      if (reference.kind !== kind) {
        return { resolved: false as const, reason: `provider ${kind} received ${reference.kind}` };
      }
      const id = reference.kind === "matrix_feature"
        ? reference.feature_id
        : reference.kind === "matrix_role" ? reference.role_id : reference.cell_id;
      const matches = members(view).filter((member) => member.id === id);
      return matches.length === 1
        ? { resolved: true as const, provider: `${kind}:${id}` }
        : {
            resolved: false as const,
            reason: matches.length === 0
              ? `${kind} ${JSON.stringify(id)} is absent from the injected registry view`
              : `${kind} ${JSON.stringify(id)} has ${matches.length} injected registry claims`,
          };
    },
  });
}

function matrixProviders(): readonly ReferenceProvider[] {
  return Object.freeze([
    exactRegistryProvider("matrix_cell", (view) => view.matrix_cells),
    exactRegistryProvider("matrix_feature", (view) => view.matrix_features),
    exactRegistryProvider("matrix_role", (view) => view.matrix_roles),
  ]);
}

function matrixSlotResolvers(
  view: RegistryView,
  document: RoadmapDocument,
): ReadonlyMap<SlotId, GeneratedSlotResolver> {
  const facts = deriveMatrixStatusFacts(view.matrix_status_inputs);
  if (facts.validation_problems.length > 0) {
    throw new Error(`matrix status inputs fail anti-vacuity: ${facts.validation_problems.join("; ")}`);
  }
  const inlineProductionSlots = usesLiveMatrixInlineSlots(document);
  const payloads = new Map<string, Uint8Array>(
    renderMatrixStatusPayloads(facts)
      .filter((payload) => payload.path === MATRIX_PROJECTION_PATH)
      .map((payload) => {
        if (inlineProductionSlots) {
          // The live roadmap slots own only inline marker interiors while the document retains the
          // frozen production slot contract. Owner authority and joins do not change the
          // surrounding layout owned by other nodes.
          return [`status_header_markers:${payload.slot_id}`, payload.bytes] as const;
        }
        // Compatibility fixtures model a slot as a complete standalone projection line.
        const line = new Uint8Array(payload.bytes.byteLength + 1);
        line.set(payload.bytes);
        line[payload.bytes.byteLength] = 0x0a;
        return [`status_header_markers:${payload.slot_id}`, line] as const;
      }),
  );
  return new Map(SLOT_BINDINGS.map(([slotId, expectedBinding]) => [
    slotId,
    Object.freeze({
      resolve(slot) {
        if (slot.slot_id !== slotId) {
          throw new Error(`resolver ${slotId} cannot resolve slot ${slot.slot_id}`);
        }
        if (slot.binding !== expectedBinding) {
          throw new Error(`slot ${slotId} binding must be ${expectedBinding}`);
        }
        const bytes = payloads.get(expectedBinding);
        if (bytes === undefined || bytes.byteLength === 0) {
          throw new Error(`slot ${slotId} produced no matrix roadmap status bytes`);
        }
        return { binding: expectedBinding, bytes: new Uint8Array(bytes) };
      },
    } satisfies GeneratedSlotResolver),
  ]));
}

export function validateMatrixPayloadFact(
  provider: SemanticPayloadProviderFact,
  indexes: Indexes,
  out: IssueCollector,
  source: string = MATRIX_SOURCE_PATH,
): void {
  const payload = provider.payload;
  if (payload.kind !== "matrix_external_closeout" && payload.kind !== "matrix_policy") return;
  if (payload.kind === "matrix_external_closeout") {
    if (payload.transition_ids.length !== 1) {
      out.add(issue(provider, source, "E-SCHEMA-STATE", "transition_ids", "matrix closeout requires exactly one retirement predicate"));
    }
    for (const id of payload.transition_ids) {
      requirePayloadKind(
        provider,
        source,
        indexes,
        id,
        "transition_ids",
        (target) => target.kind === "signal" && target.transition_kind === "retirement_predicate",
        "one retirement-predicate signal",
        out,
      );
    }
    const actionIds = new Set<string>();
    for (const action of payload.actions) {
      if (actionIds.has(action.action_id)) {
        out.add(issue(provider, source, "E-SCHEMA-STATE", `actions[${JSON.stringify(action.action_id)}]`, "closeout action ID is duplicated"));
      }
      actionIds.add(action.action_id);
    }
    const branchIds = new Set<string>();
    for (const branch of payload.branches) {
      if (branchIds.has(branch.branch_id)) {
        out.add(issue(provider, source, "E-SCHEMA-STATE", `branches[${JSON.stringify(branch.branch_id)}]`, "closeout branch ID is duplicated"));
      }
      branchIds.add(branch.branch_id);
      const branchActionIds = new Set<string>();
      for (const actionId of branch.action_ids) {
        if (branchActionIds.has(actionId)) {
          out.add(issue(
            provider,
            source,
            "E-SCHEMA-STATE",
            `branches[${JSON.stringify(branch.branch_id)}].action_ids`,
            `${actionId} is duplicated within the branch action sequence`,
          ));
        }
        branchActionIds.add(actionId);
        if (!actionIds.has(actionId)) {
          out.add(issue(
            provider,
            source,
            "E-SCHEMA-STATE",
            `branches[${JSON.stringify(branch.branch_id)}].action_ids`,
            `${actionId} does not resolve to a local closeout action`,
          ));
        }
      }
    }
    return;
  }
  const targetId = payload.policy_kind === "maintenance_protocol"
    ? payload.cadence_transition_id
    : payload.reopening_transition_id;
  if (targetId === undefined) {
    if (payload.policy_kind === "boundary" && payload.permanence === "reopenable") {
      out.add(issue(provider, source, "E-SCHEMA-STATE", "reopening_transition_id", "reopenable matrix boundary requires a reopening signal"));
    }
    return;
  }
  const expected = payload.policy_kind === "maintenance_protocol" ? "cadence" : "reopening_signal";
  requirePayloadKind(
    provider,
    source,
    indexes,
    targetId,
    payload.policy_kind === "maintenance_protocol" ? "cadence_transition_id" : "reopening_transition_id",
    (target) => target.kind === "signal" && target.transition_kind === expected,
    `${expected} signal`,
    out,
  );
}

export interface DecodedRoadmapValidationObserver {
  sharedValidationStarted(indexes: RoadmapIndexes): void;
  domainPayloadValidated(provider: SemanticPayloadProviderFact): void;
}

export interface DecodedRoadmapValidationOptions {
  readonly universe?: SemanticJoinUniverse;
  readonly defer_foreign_roadmap_joins?: boolean;
  readonly observer?: DecodedRoadmapValidationObserver;
  readonly denominator_authorities?: DenominatorAuthorityRegistry;
}

export interface DecodedRoadmapValidationResult {
  readonly indexes: RoadmapIndexes;
  readonly issues: readonly RoadmapIssue[];
}

export type DomainPayloadFactValidator = (
  provider: SemanticPayloadProviderFact,
  indexes: RoadmapIndexes,
  out: IssueCollector,
  source: string,
) => void;

/**
 * Pure production orchestration over one already-decoded document. C4A is the mandatory first
 * callback boundary: any index issue returns immediately before floors, shared joins, relations,
 * providers, or domain payload validation can run.
 */
export function validateDecodedRoadmapDocument(
  document: RoadmapDocument,
  view: RegistryView,
  adapter: RoadmapAdapter<SemanticPayload>,
  referenceProviders: readonly ReferenceProviderLike[],
  validateDomainPayload: DomainPayloadFactValidator,
  options: DecodedRoadmapValidationOptions = {},
): DecodedRoadmapValidationResult {
  const built = buildRoadmapIndexes(document);
  if (built.issues.length > 0) {
    return Object.freeze({ indexes: built.indexes, issues: built.issues });
  }
  const indexes = built.indexes;
  const baseUniverse = options.universe ?? indexes;
  const universe = Object.freeze({
    first_class: baseUniverse.first_class,
    payload_records: baseUniverse.payload_records,
    current_guards: view.current_guards.length > 0
      ? view.current_guards
      : "current_guards" in baseUniverse ? baseUniverse.current_guards : undefined,
  });
  // One-document adapter validation is the scoped lane. Supplying a combined universe closes the
  // seam and disables deferral unless a caller explicitly requests it for a focused probe.
  const deferredNamespace = options.defer_foreign_roadmap_joins === true ||
      (options.defer_foreign_roadmap_joins === undefined && options.universe === undefined)
    ? indexes.roadmap
    : undefined;
  const domainIndexes = Object.freeze({
    ...indexes,
    first_class: universe.first_class,
    payload_records: universe.payload_records,
    ...(deferredNamespace === undefined ? {} : { deferred_foreign_roadmap_joins: deferredNamespace }),
  }) as RoadmapIndexes & { readonly deferred_foreign_roadmap_joins?: RoadmapName };
  const source = document.document.source_path;
  options.observer?.sharedValidationStarted(indexes);
  const issues: RoadmapIssue[] = [];
  const collector: IssueCollector = { issues, add: (value) => issues.push(value) };
  adapter.validateFloors(document, collector);
  issues.push(...validateSemanticRoadmapJoins(indexes, universe, source, deferredNamespace));
  issues.push(...validateRoadmapReferences(indexes, view, {
    source,
    providers: referenceProviders,
    first_class: universe.first_class,
    defer_foreign_roadmap_joins: deferredNamespace,
  }));
  issues.push(...validateRelations(indexes.relations, universe.first_class, source, deferredNamespace, view.current_guards));
  issues.push(...validateSystematicFamilies(
    indexes,
    view,
    options.denominator_authorities ?? EMPTY_DENOMINATOR_AUTHORITIES,
    source,
  ));
  for (const provider of indexes.payload_records.values()) {
    validateDomainPayload(provider, domainIndexes, collector, source);
    options.observer?.domainPayloadValidated(provider);
  }
  return Object.freeze({ indexes, issues: sortIssues(issues) });
}

export const MATRIX_ADAPTER: RoadmapAdapter<SemanticPayload> = Object.freeze({
  roadmap: "matrix",
  namespace: "matrix",
  source_path: MATRIX_SOURCE_PATH,
  projection_path: MATRIX_PROJECTION_PATH,
  validateExtension(record: SemanticRecord<SemanticPayload>, indexes: Indexes, out: IssueCollector) {
    validateMatrixPayloadFact({
      record,
      payload: record.payload,
      authority: "semantic",
      logical_path: `record[${JSON.stringify(record.id)}].payload`,
    }, indexes, out);
  },
  renderSemantic: renderCanonicalSemanticRecord,
  referenceProviders(_view: RegistryView) {
    return [...matrixProviders()];
  },
  slotResolvers(view: RegistryView, document: RoadmapDocument) {
    return matrixSlotResolvers(view, document);
  },
  validateFloors(doc: RoadmapDocument, out: IssueCollector) {
    if (doc.document.roadmap !== "matrix") {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "document.roadmap",
        message: "matrix adapter requires a matrix roadmap document",
        exit: 1,
      });
    }
    if (doc.document.source_path !== MATRIX_SOURCE_PATH) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "document.source_path",
        message: `matrix source path must be ${MATRIX_SOURCE_PATH}`,
        exit: 1,
      });
    }
    if (doc.document.projection_path !== MATRIX_PROJECTION_PATH) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "document.projection_path",
        message: `matrix projection path must be ${MATRIX_PROJECTION_PATH}`,
        exit: 1,
      });
    }
    if (doc.records.length === 0 || doc.manifest.length === 0 || doc.spans.length === 0) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "$",
        message: "matrix roadmap requires records, manifest placements, and source spans",
        exit: 1,
      });
    }
    const slots = new Map(doc.generated_slots.map((slot) => [slot.slot_id, slot]));
    if (doc.generated_slots.length !== SLOT_BINDINGS.length || slots.size !== SLOT_BINDINGS.length) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "generated_slot",
        message: "matrix roadmap requires exactly four distinct generated status slots",
        exit: 1,
      });
    }
    for (const [slotId, binding] of SLOT_BINDINGS) {
      const slot = slots.get(slotId);
      if (slot === undefined || slot.binding !== binding) {
        out.add({
          code: "E-SCHEMA-FLOOR",
          source: doc.document.source_path,
          logical_path: `generated_slot[${JSON.stringify(slotId)}].binding`,
          message: `matrix slot ${slotId} must declare binding ${binding}`,
          exit: 1,
        });
      }
    }
  },
});

export const MATRIX_GENERATED_SLOT_BINDINGS = SLOT_BINDINGS;

export function validateMatrixRoadmapDocument(
  document: RoadmapDocument,
  view: RegistryView,
  options: DecodedRoadmapValidationOptions = {},
): DecodedRoadmapValidationResult {
  return validateDecodedRoadmapDocument(
    document,
    view,
    MATRIX_ADAPTER,
    MATRIX_ADAPTER.referenceProviders(view),
    validateMatrixPayloadFact,
    options,
  );
}
