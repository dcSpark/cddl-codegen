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
  | "debt"
  | "references"
  | "campaign"
  | "signals"
  | "actionables"
  | "decisions"
  | "families"
  | "watches"
  | "content"
  | "output-owners";

export type CliRequest =
  | { mode: "selftest" }
  | { mode: "check"; roadmap: RoadmapSelection; against?: FullCommitId }
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
export type FamilyClassification = "none_reviewed" | "pending";

export interface SemanticPayloadBase {
  kind: string;
  summary_md: Uint8Array;
  detail_md?: Uint8Array;
}

interface WorkBase extends SemanticPayloadBase {
  kind: "work";
  work_state: WorkState;
  work_intent: WorkIntent;
  work_kind: WorkKind;
  risk: Risk;
  family_id?: RoadmapId;
  family_classification?: FamilyClassification;
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
  transition_ids: RoadmapId[];
}

export interface ArmedWork extends WorkBase {
  work_state: "armed";
  control_ids: RoadmapId[];
  transition_ids: RoadmapId[];
}

export interface DeferredWork extends WorkBase {
  work_state: "deferred";
  transition_ids: RoadmapId[];
}

export interface WaitingExternalWork extends WorkBase {
  work_state: "waiting_external";
  transition_ids: RoadmapId[];
  external_owner_reference_id: ReferenceId;
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
  transition_ids: RoadmapId[];
}

export interface HeldDecision extends SemanticPayloadBase {
  kind: "decision";
  decision_state: "held";
  rationale_md: Uint8Array;
  permanence: "reopenable";
  transition_ids: RoadmapId[];
}

export interface DecidedDecision extends SemanticPayloadBase {
  kind: "decision";
  decision_state: "decided";
  rationale_md: Uint8Array;
  authority_reference_id: ReferenceId;
  permanence: "permanent" | "reopenable";
  transition_ids?: RoadmapId[];
}

export type DecisionPayload = PendingDecision | HeldDecision | DecidedDecision;

export type SignalEvaluation = "met" | "unmet" | "unknown" | "stale";
export type Comparator = "lt" | "le" | "eq" | "ge" | "gt";

export interface QuantitativePredicate {
  predicate_kind: "quantitative";
  comparator: Comparator;
  threshold: number;
  unit: string;
  scope: string;
  measurement: number;
  as_of: CivilDate;
}

export interface EventPredicate {
  predicate_kind: "event";
  event_md: Uint8Array;
  evidence_ids: RoadmapId[];
}

export interface ManualPredicate {
  predicate_kind: "manual";
  review_procedure_md: Uint8Array;
  evidence_ids: RoadmapId[];
}

export type SignalPredicate = QuantitativePredicate | EventPredicate | ManualPredicate;

interface PredicateSignalBase extends SemanticPayloadBase {
  kind: "signal";
  observer: string;
  dimension: string;
  observable: string;
  predicate_kind: SignalPredicate["predicate_kind"];
  current_evidence_ids: RoadmapId[];
  action_on_fire_md: Uint8Array;
  evaluation: SignalEvaluation;
  predicate: SignalPredicate;
}

export interface PromotionTrigger extends PredicateSignalBase {
  transition_kind: "promotion_trigger";
}

export interface ReopeningSignal extends PredicateSignalBase {
  transition_kind: "reopening_signal";
}

export interface UnblockPredicate extends SemanticPayloadBase {
  kind: "signal";
  transition_kind: "unblock_predicate";
  owner_reference_id: ReferenceId;
  event_md: Uint8Array;
  check_procedure_md: Uint8Array;
  due_action_md: Uint8Array;
  evaluation: SignalEvaluation;
}

export interface WatchEscalation extends SemanticPayloadBase {
  kind: "signal";
  transition_kind: "watch_escalation";
  failure_signature_md: Uint8Array;
  capture_procedure_md: Uint8Array;
  response_md: Uint8Array;
  escalation_action_md: Uint8Array;
  retirement_semantics_md: Uint8Array;
  evaluation: SignalEvaluation;
}

export interface RetirementPredicate extends SemanticPayloadBase {
  kind: "signal";
  transition_kind: "retirement_predicate";
  external_owner_reference_id: ReferenceId;
  external_predicate_md: Uint8Array;
  verification_md: Uint8Array;
  due_action_md: Uint8Array;
  evaluation: SignalEvaluation;
}

export interface CadenceSignal extends SemanticPayloadBase {
  kind: "signal";
  transition_kind: "cadence";
  owner_reference_id: ReferenceId;
  event_source: string;
  period_or_event_md: Uint8Array;
  checklist_md: Uint8Array;
  missed_action_md: Uint8Array;
  last_completion_reference_id?: ReferenceId;
  due_on?: CivilDate;
  as_of?: CivilDate;
  evaluation: SignalEvaluation;
}

export type SignalPayload =
  | PromotionTrigger
  | ReopeningSignal
  | UnblockPredicate
  | WatchEscalation
  | RetirementPredicate
  | CadenceSignal;

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
  cell_ids?: RoadmapId[];
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
  | SignalPayload
  | EvidencePayload
  | ControlPayload;
