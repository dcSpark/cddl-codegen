export type Brand<T, Name extends string> = T & { readonly __brand: Name };

export type RoadmapName = "matrix" | "testing";
export type RoadmapSelection = RoadmapName | "all";
export type RoadmapId = Brand<string, "RoadmapId">;
export type SpanId = Brand<string, "SpanId">;
export type SectionId = Brand<string, "SectionId">;
export type FragmentId = Brand<string, "FragmentId">;
export type MarkerId = Brand<string, "MarkerId">;
export type PartId = Brand<string, "PartId">;
export type SlotId = Brand<string, "SlotId">;
export type LowercaseSlug = Brand<string, "LowercaseSlug">;
export type ReferenceId = LowercaseSlug & { readonly __referenceIdBrand: "ReferenceId" };
export type FullCommitId = Brand<string, "FullCommitId">;
export type CivilDate = Brand<string, "CivilDate">;
export type AsOfDate = CivilDate & { readonly __asOfBrand: "AsOfDate" };
export type RepoPath = Brand<string, "RepoPath">;
export type FixtureRelativePath = Brand<string, "FixtureRelativePath">;

export type QueryView =
  | "summary"
  | "references"
  | "transitions"
  | "actionables"
  | "decisions"
  | "watches"
  | "content"
  | "output-owners"
  | "index";

export type CliRequest =
  | { mode: "selftest" }
  | { mode: "check"; roadmap: RoadmapSelection }
  | { mode: "write"; roadmap: RoadmapName }
  | {
      mode: "query";
      roadmap: RoadmapSelection;
      view: QueryView;
      json: boolean;
      as_of?: AsOfDate;
    }
  | { mode: "format_source"; source_path: RepoPath };

export type RepositoryRevision =
  | { kind: "worktree" }
  | { kind: "commit"; commit: FullCommitId };

export type WorkState =
  | "ready"
  | "blocked"
  | "armed"
  | "deferred"
  | "waiting_external"
  | "delegated"
  | "pending_review";
export type WorkIntent =
  | "repair"
  | "add_regression"
  | "build_capability"
  | "build_system"
  | "establish_honest_refusal"
  | "optimize"
  | "change_documentation";
export type WorkKind =
  | "defect"
  | "regression_gap"
  | "coverage_cell"
  | "missing_system"
  | "feature"
  | "optimization"
  | "documentation_integrity"
  | "infrastructure";
export type Risk =
  | "silent_wrong_bytes"
  | "invalid_acceptance"
  | "valid_rejection"
  | "wrong_public_api"
  | "compile_failure"
  | "abort_or_panic"
  | "false_pass_or_red"
  | "misleading_docs"
  | "resource_exhaustion"
  | "cosmetic";
export type PriorityBand = "critical" | "high" | "normal" | "low";

export interface SemanticPayloadBase {
  kind: string;
  detail_md?: Uint8Array;
}

interface WorkBase extends SemanticPayloadBase {
  kind: "work";
  work_state: WorkState;
  work_intent: WorkIntent;
  work_kind: WorkKind;
  risk: Risk;
  evidence_ids?: RoadmapId[];
  acceptance_md?: Uint8Array;
  control_ids?: RoadmapId[];
  regression_evidence_ids?: RoadmapId[];
  regression_gap_ids?: RoadmapId[];
  admission_ids?: RoadmapId[];
}

export interface ReadyWork extends WorkBase {
  work_state: "ready";
  acceptance_md: Uint8Array;
  priority_rationale_md: Uint8Array;
  priority_band?: PriorityBand;
}

export interface BlockedWork extends WorkBase {
  work_state: "blocked";
  blocker_md: Uint8Array;
  unblock_predicate: NestedUnblockPredicate;
}

export interface ArmedWork extends WorkBase {
  work_state: "armed";
  control_ids: RoadmapId[];
  promotion_trigger: NestedTransition;
}

/** Deferred work carries a nested reopening signal, a standalone-transition citation, or both. */
export interface DeferredWork extends WorkBase {
  work_state: "deferred";
  transition_ids?: RoadmapId[];
  reopening_signal?: NestedTransition;
}

/** Waiting-external work carries exactly one of the two admissible transition contracts. */
export interface WaitingExternalWork extends WorkBase {
  work_state: "waiting_external";
  external_owner_reference_id: ReferenceId;
  retirement_predicate?: NestedRetirementPredicate;
  unblock_predicate?: NestedUnblockPredicate;
}

export interface DelegatedWork extends WorkBase {
  work_state: "delegated";
  return_condition_md: Uint8Array;
}

export interface PendingReviewWork extends WorkBase {
  work_state: "pending_review";
  uncertainty_md: Uint8Array;
}

export type WorkPayload =
  | ReadyWork
  | BlockedWork
  | ArmedWork
  | DeferredWork
  | WaitingExternalWork
  | DelegatedWork
  | PendingReviewWork;

export interface PendingDecision extends SemanticPayloadBase {
  kind: "decision";
  decision_state: "pending";
  question_md: Uint8Array;
  unblock_predicate: NestedUnblockPredicate;
}

export interface HeldDecision extends SemanticPayloadBase {
  kind: "decision";
  decision_state: "held";
  rationale_md: Uint8Array;
  permanence: "reopenable";
  reopening_signal: NestedTransition;
}

export interface DecidedDecision extends SemanticPayloadBase {
  kind: "decision";
  decision_state: "decided";
  rationale_md: Uint8Array;
  authority_reference_id: ReferenceId;
  permanence: "permanent" | "reopenable";
  reopening_signal?: NestedTransition;
}

export type DecisionPayload = PendingDecision | HeldDecision | DecidedDecision;

export type TransitionEvaluation = "met" | "unmet" | "unknown" | "stale";
export type Comparator = "lt" | "le" | "eq" | "ge" | "gt";

export interface QuantitativePredicate {
  predicate_kind: "quantitative";
  comparator: Comparator;
  threshold: number;
  unit: string;
  scope: string;
  measurement: number;
  as_of: CivilDate;
  evidence_ids?: RoadmapId[];
}

/** evidence_ids absent means "no evidence of the event recorded yet" (3A-2 re-cut ruling 1). */
export interface EventPredicate {
  predicate_kind: "event";
  event_md: Uint8Array;
  evidence_ids?: RoadmapId[];
}

export interface ManualPredicate {
  predicate_kind: "manual";
  review_procedure_md: Uint8Array;
  evidence_ids?: RoadmapId[];
}

export type TransitionPredicate = QuantitativePredicate | EventPredicate | ManualPredicate;

interface PredicateTransitionBase extends SemanticPayloadBase {
  kind: "transition";
  observer: string;
  dimension: string;
  /**
   * The authored trigger condition. Arm-dependent home (Phase 2 re-cut): FORBIDDEN when the
   * nested predicate is an event (predicate.event_md is the condition's single home) and
   * REQUIRED for manual and quantitative predicates, where no nested field carries it.
   * The decoder enforces the arm rule; the type stays one shape for switch ergonomics.
   */
  observable?: string;
  action_on_fire_md: Uint8Array;
  evaluation: TransitionEvaluation;
  predicate: TransitionPredicate;
}

export interface PromotionTrigger extends PredicateTransitionBase {
  transition_kind: "promotion_trigger";
}

export interface ReopeningSignal extends PredicateTransitionBase {
  transition_kind: "reopening_signal";
}

export interface UnblockPredicate extends SemanticPayloadBase {
  kind: "transition";
  transition_kind: "unblock_predicate";
  owner_reference_id: ReferenceId;
  event_md: Uint8Array;
  check_procedure_md: Uint8Array;
  due_action_md: Uint8Array;
  evaluation: TransitionEvaluation;
}

export interface WatchEscalation extends SemanticPayloadBase {
  kind: "transition";
  transition_kind: "watch_escalation";
  failure_signature_md: Uint8Array;
  capture_procedure_md: Uint8Array;
  response_md: Uint8Array;
  escalation_action_md: Uint8Array;
  retirement_semantics_md: Uint8Array;
  evaluation: TransitionEvaluation;
}

export interface RetirementPredicate extends SemanticPayloadBase {
  kind: "transition";
  transition_kind: "retirement_predicate";
  external_owner_reference_id: ReferenceId;
  external_predicate_md: Uint8Array;
  verification_md: Uint8Array;
  due_action_md: Uint8Array;
  evaluation: TransitionEvaluation;
}

export interface CadenceTransition extends SemanticPayloadBase {
  kind: "transition";
  transition_kind: "cadence";
  owner_reference_id: ReferenceId;
  event_source: string;
  period_or_event_md: Uint8Array;
  checklist_md: Uint8Array;
  missed_action_md: Uint8Array;
  last_completion_reference_id?: ReferenceId;
  due_on?: CivilDate;
  as_of?: CivilDate;
  evaluation: TransitionEvaluation;
}

export type TransitionPayload =
  | PromotionTrigger
  | ReopeningSignal
  | UnblockPredicate
  | WatchEscalation
  | RetirementPredicate
  | CadenceTransition;

/**
 * Nested transition tables (Packet 3A-2): the standalone transition arms' typed contracts packaged as
 * tables on the owning record — the field name supplies `transition_kind`, the owner supplies the
 * identity, so those and `detail_md` have no nested representation.
 */
export type NestedTransition = Omit<PromotionTrigger, "kind" | "transition_kind" | "detail_md">;
export type NestedUnblockPredicate = Omit<UnblockPredicate, "kind" | "transition_kind" | "detail_md">;
export type NestedWatchEscalation = Omit<WatchEscalation, "kind" | "transition_kind" | "detail_md">;
export type NestedRetirementPredicate = Omit<RetirementPredicate, "kind" | "transition_kind" | "detail_md">;
export type NestedCadenceTransition = Omit<CadenceTransition, "kind" | "transition_kind" | "detail_md">;

export type EvidenceKind =
  | "regression_pin"
  | "gate"
  | "harness_free_repro"
  | "committed_vector"
  | "execution_probe"
  | "registry_enumeration"
  | "source_read"
  | "spec_read"
  | "consumer_report"
  | "incident"
  | "external_issue"
  | "external_commit"
  | "decision";
export type EvidenceVerdict = "proposed" | "confirmed" | "falsified" | "unknown" | "inapplicable";
export type Freshness = "live" | "as_of" | "historical" | "stale";

export interface EvidenceScope {
  surfaces?: string[];
  faces?: string[];
  profiles?: string[];
  flags?: string[];
  input_modes?: string[];
  toolchains?: string[];
  executors?: string[];
  tiers?: string[];
}

export interface EvidencePayload extends SemanticPayloadBase {
  kind: "evidence";
  evidence_kind: EvidenceKind;
  claim_md: Uint8Array;
  evidence_verdict: EvidenceVerdict;
  freshness: Freshness;
  reference_ids: ReferenceId[];
  command_md?: Uint8Array;
  result_md?: Uint8Array;
  at_commit?: FullCommitId;
  observed_at?: CivilDate;
  valid_through?: CivilDate;
  environment_md?: Uint8Array;
  unprobed_remainder_md: Uint8Array;
  refresh_reference_id?: ReferenceId;
  enumerated_registry?: string;
  scope: EvidenceScope;
}

export type ControlKind =
  | "gate"
  | "test"
  | "fixture"
  | "review_rule"
  | "consumer_ci"
  | "upstream_issue"
  | "operator_procedure";

export interface ControlPayload extends SemanticPayloadBase {
  kind: "control";
  control_kind: ControlKind;
  control_state: "live" | "proposed" | "stale";
  reference_ids: ReferenceId[];
  claim_md: Uint8Array;
  boundary_md: Uint8Array;
}

export type SharedSemanticPayload =
  | WorkPayload
  | DecisionPayload
  | TransitionPayload
  | EvidencePayload
  | ControlPayload;
