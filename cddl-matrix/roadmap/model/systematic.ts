import type {
  ReferenceId,
  RoadmapId,
  SemanticPayloadBase,
} from "./core.ts";

export type FamilyMaturity = "observed_only" | "under_design";
export type CampaignState = "designing" | "enumerating" | "closing";
export type EvidenceStage =
  | "generated"
  | "compiled"
  | "executed"
  | "round_tripped"
  | "independently_decoded"
  | "constraint_enforced"
  | "over_accepted";

export interface FamilyAxisValue {
  id: RoadmapId;
  label: string;
  source_reference_id: ReferenceId;
}

export interface FamilyAxis {
  id: RoadmapId;
  label: string;
  authority_reference_id: ReferenceId;
  values: FamilyAxisValue[];
}

export interface FamilyEvidenceRequirement {
  id: RoadmapId;
  profiles: string[];
  faces: string[];
  stages: EvidenceStage[];
}

export interface FamilyCoordinate {
  axis_id: RoadmapId;
  value_id: RoadmapId;
}

export interface FamilyCell {
  id: RoadmapId;
  spec_legality: "legal";
  cell_disposition: "supported" | "safely_refused" | "deliberately_unsupported" | "unknown";
  affected_profiles: string[];
  affected_faces: string[];
  evidence_ids?: RoadmapId[];
  work_id?: RoadmapId;
  coordinates: FamilyCoordinate[];
}

export interface FamilyExclusion {
  id: RoadmapId;
  spec_legality: "illegal";
  reason_md: Uint8Array;
  owner_reference_id: ReferenceId;
  source_reference_id: ReferenceId;
  liveness_reference_id: ReferenceId;
  coordinates: FamilyCoordinate[];
}

interface FamilyBase extends SemanticPayloadBase {
  kind: "family";
  family_maturity: FamilyMaturity;
  campaign_state: CampaignState;
  goal_md: Uint8Array;
  boundary_md: Uint8Array;
  work_ids: RoadmapId[];
  affected_profiles: string[];
  affected_faces: string[];
  control_ids: RoadmapId[];
  completion_owner_reference_id: ReferenceId;
  retirement_owner_reference_id: ReferenceId;
  axes: FamilyAxis[];
  evidence_requirements: FamilyEvidenceRequirement[];
  cells: FamilyCell[];
  exclusions: FamilyExclusion[];
}

export interface ObservedOnlyFamily extends FamilyBase {
  family_maturity: "observed_only";
  observation_reference_ids: ReferenceId[];
}

export interface UnderDesignFamily extends FamilyBase {
  family_maturity: "under_design";
  authority_kind: "grammar" | "registry" | "reviewed_relation";
  authority_reference_id: ReferenceId;
  derivation_md: Uint8Array;
  legality_rule_md: Uint8Array;
  legality_owner_reference_id: ReferenceId;
  denominator_unknowns_md?: Uint8Array;
}

export type FamilyPayload = ObservedOnlyFamily | UnderDesignFamily;
