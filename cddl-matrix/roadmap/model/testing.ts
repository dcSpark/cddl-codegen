import type {
  CivilDate,
  ReferenceId,
  RoadmapId,
  SemanticPayloadBase,
} from "./core.ts";

export interface TestingCaptureStep {
  step_id: string;
  capture_md: Uint8Array;
}

interface TestingOperationalWatchBase extends SemanticPayloadBase {
  kind: "testing_operational_watch";
  watch_state: "watching" | "attributed" | "retire_pending";
  signature_md: Uint8Array;
  response_md: Uint8Array;
  escalation_transition_id: RoadmapId;
  retirement_semantics_md: Uint8Array;
  capture_steps: TestingCaptureStep[];
}

export interface WatchingTestingOperationalWatch extends TestingOperationalWatchBase {
  watch_state: "watching";
}

export interface AttributedTestingOperationalWatch extends TestingOperationalWatchBase {
  watch_state: "attributed";
  attribution_md: Uint8Array;
  operating_rule_reference_id: ReferenceId;
}

export interface RetirePendingTestingOperationalWatch extends TestingOperationalWatchBase {
  watch_state: "retire_pending";
  attribution_md: Uint8Array;
  operating_rule_reference_id: ReferenceId;
  retirement_reference_id: ReferenceId;
}

export type TestingOperationalWatchPayload =
  | WatchingTestingOperationalWatch
  | AttributedTestingOperationalWatch
  | RetirePendingTestingOperationalWatch;

interface TestingIncidentBase extends SemanticPayloadBase {
  kind: "testing_incident";
  incident_posture: "live" | "attributed" | "historical";
  signature_md: Uint8Array;
  evidence_ids: RoadmapId[];
}

export interface LiveTestingIncident extends TestingIncidentBase {
  incident_posture: "live";
}

export interface AttributedTestingIncident extends TestingIncidentBase {
  incident_posture: "attributed";
  attribution_md: Uint8Array;
  operating_rule_reference_id: ReferenceId;
}

export interface HistoricalTestingIncident extends TestingIncidentBase {
  incident_posture: "historical";
  attribution_md: Uint8Array;
  operating_rule_reference_id: ReferenceId;
  retirement_reference_id: ReferenceId;
}

export type TestingIncidentPayload =
  | LiveTestingIncident
  | AttributedTestingIncident
  | HistoricalTestingIncident;

export interface LiveRegistryTestingCost extends SemanticPayloadBase {
  kind: "testing_cost";
  cost_posture: "live_registry";
  unit: string;
  scope_md: Uint8Array;
  gate_reference_id: ReferenceId;
}

export interface HistoricalObservationTestingCost extends SemanticPayloadBase {
  kind: "testing_cost";
  cost_posture: "historical_observation";
  unit: string;
  scope_md: Uint8Array;
  value_min: number;
  value_max: number;
  observed_at: CivilDate;
  environment_md: Uint8Array;
  evidence_ids: RoadmapId[];
}

export type TestingCostPayload = LiveRegistryTestingCost | HistoricalObservationTestingCost;

interface TestingSystemAdmissionBase extends SemanticPayloadBase {
  kind: "testing_system_admission";
  admission_kind: "silent_corruption" | "independent_recurrence" | "bounded_denominator";
  claim_md: Uint8Array;
  evidence_ids: RoadmapId[];
}

export interface SilentCorruptionAdmission extends TestingSystemAdmissionBase {
  admission_kind: "silent_corruption";
}

export interface IndependentRecurrenceAdmission extends TestingSystemAdmissionBase {
  admission_kind: "independent_recurrence";
  incident_ids: RoadmapId[];
}

export interface BoundedDenominatorAdmission extends TestingSystemAdmissionBase {
  admission_kind: "bounded_denominator";
  family_id: RoadmapId;
  cost_record_id: RoadmapId;
}

export type TestingSystemAdmissionPayload =
  | SilentCorruptionAdmission
  | IndependentRecurrenceAdmission
  | BoundedDenominatorAdmission;

export type TestingSemanticPayload =
  | TestingOperationalWatchPayload
  | TestingIncidentPayload
  | TestingCostPayload
  | TestingSystemAdmissionPayload;
