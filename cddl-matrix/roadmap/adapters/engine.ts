/**
 * The roadmap-agnostic validation and rendering engine.  Everything here is shared by every
 * adapter: the canonical Markdown field order a semantic record renders in, the payload-fact
 * helpers domain validators are built from, the structural floor check driven by an adapter's own
 * coordinates, and the one orchestration that turns a decoded document plus a registry view into
 * indexes and sorted issues.  Adapters supply the domain knowledge; this module supplies the
 * machinery, so no adapter needs to import a sibling adapter to reach it.
 */
import type { IssueCollector, RoadmapIssue } from "../errors.ts";
import { sortRoadmapIssues as sortIssues } from "../errors.ts";
import { EMPTY_DENOMINATOR_AUTHORITIES, validateSystematicFamilies, type DenominatorAuthorityRegistry } from "../denominator.ts";
import { buildRoadmapIndexes, type RoadmapIndexes, type SemanticPayloadProviderFact } from "../indexes.ts";
import { namespaceOf } from "../ids.ts";
import { concatenate } from "../kernel.ts";
import type { RepoPath, RoadmapId, RoadmapName, SlotId } from "../model/core.ts";
import type {
  RoadmapDocument,
  SemanticPayload,
  SemanticRecord,
} from "../model/documents.ts";
import {
  validateRoadmapReferences,
  validateSemanticRoadmapJoins,
  type ReferenceProviderLike,
  type SemanticJoinUniverse,
} from "../references.ts";
import { validateRelations } from "../relations.ts";
import type {
  FieldConsumer,
  Indexes,
  RegistryView,
  RoadmapAdapter,
} from "./types.ts";

/**
 * Domain payload-fact issue factory: the logical path is always the fact's own path plus the field
 * that failed, so a diagnostic points at the exact payload coordinate.
 */
export function payloadFactIssue(
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

function payloadAt(indexes: Indexes, id: RoadmapId): SemanticPayload | undefined {
  if ("payload_records" in indexes) {
    return (indexes as Indexes & Pick<RoadmapIndexes, "payload_records">).payload_records.get(id)?.payload;
  }
  return indexes.records.get(id)?.payload;
}

export function requirePayloadKind(
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
    out.add(payloadFactIssue(provider, source, "E-SCHEMA-STATE", field, `${id} must resolve to ${expected}`));
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

export interface RoadmapFloorSpec {
  /** The roadmap name the document must declare. */
  readonly roadmap: RoadmapName;
  readonly source_path: RepoPath;
  readonly projection_path: RepoPath;
  /** The exact generated slots the roadmap declares, in `[slot_id, binding]` form. */
  readonly slot_bindings: readonly (readonly [SlotId, string])[];
  /** The diagnostic for a slot inventory that is not exactly `slot_bindings`. */
  readonly slot_inventory_message: string;
}

/**
 * The structural floors every roadmap document must meet, as one check driven by the adapter's own
 * coordinates.  Both adapters previously carried a hand-written copy of these five checks, which is
 * how the two wordings of the slot-inventory diagnostic drifted apart -- the remaining per-roadmap
 * text is data here, not code.
 */
export function createRoadmapFloorValidator(
  spec: RoadmapFloorSpec,
): (doc: RoadmapDocument, out: IssueCollector) => void {
  const floor = (source: RepoPath, logicalPath: string, message: string): RoadmapIssue =>
    ({ code: "E-SCHEMA-FLOOR", source, logical_path: logicalPath, message, exit: 1 });
  return (doc: RoadmapDocument, out: IssueCollector): void => {
    const source = doc.document.source_path;
    if (doc.document.roadmap !== spec.roadmap) {
      out.add(floor(source, "document.roadmap", `${spec.roadmap} adapter requires a ${spec.roadmap} roadmap document`));
    }
    if (doc.document.source_path !== spec.source_path) {
      out.add(floor(source, "document.source_path", `${spec.roadmap} source path must be ${spec.source_path}`));
    }
    if (doc.document.projection_path !== spec.projection_path) {
      out.add(floor(source, "document.projection_path", `${spec.roadmap} projection path must be ${spec.projection_path}`));
    }
    if (doc.records.length === 0 || doc.manifest.length === 0 || doc.spans.length === 0) {
      out.add(floor(source, "$", `${spec.roadmap} roadmap requires records, manifest placements, and source spans`));
    }
    const slots = new Map(doc.generated_slots.map((slot) => [slot.slot_id, slot]));
    if (doc.generated_slots.length !== spec.slot_bindings.length || slots.size !== spec.slot_bindings.length) {
      out.add(floor(source, "generated_slot", spec.slot_inventory_message));
    }
    for (const [slotId, binding] of spec.slot_bindings) {
      const slot = slots.get(slotId);
      if (slot === undefined || slot.binding !== binding) {
        out.add(floor(
          source,
          `generated_slot[${JSON.stringify(slotId)}].binding`,
          `${spec.roadmap} slot ${slotId} must declare binding ${binding}`,
        ));
      }
    }
  };
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
