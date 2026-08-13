import type {
  LowercaseSlug,
  NestedCadenceSignal,
  NestedRetirementPredicate,
  NestedTriggerSignal,
  ReferenceId,
  RepoPath,
  SemanticPayloadBase,
} from "./core.ts";

export interface MatrixCloseoutAction {
  action_id: LowercaseSlug;
  action_md: Uint8Array;
}

export interface MatrixCloseoutBranch {
  branch_id: LowercaseSlug;
  predicate_md: Uint8Array;
  prune_reference_ids?: ReferenceId[];
  action_ids: LowercaseSlug[];
}

interface MatrixExternalCloseoutBase extends SemanticPayloadBase {
  kind: "matrix_external_closeout";
  closeout_state: "waiting" | "due" | "blocked";
  upstream_owner_reference_id: ReferenceId;
  current_upstream_state_md: Uint8Array;
  verification_md: Uint8Array;
  prune_reference_ids?: ReferenceId[];
  retirement_predicate: NestedRetirementPredicate;
  actions: MatrixCloseoutAction[];
  branches: MatrixCloseoutBranch[];
}

export interface WaitingMatrixExternalCloseout extends MatrixExternalCloseoutBase {
  closeout_state: "waiting";
}

export interface DueMatrixExternalCloseout extends MatrixExternalCloseoutBase {
  closeout_state: "due";
}

export interface BlockedMatrixExternalCloseout extends MatrixExternalCloseoutBase {
  closeout_state: "blocked";
  blocker_md: Uint8Array;
}

export type MatrixExternalCloseoutPayload =
  | WaitingMatrixExternalCloseout
  | DueMatrixExternalCloseout
  | BlockedMatrixExternalCloseout;

export interface MatrixMaintenancePolicy extends SemanticPayloadBase {
  kind: "matrix_policy";
  policy_kind: "maintenance_protocol";
  authority_reference_id: ReferenceId;
  protocol_md: Uint8Array;
  cadence: NestedCadenceSignal;
}

export interface MatrixBoundaryPolicy extends SemanticPayloadBase {
  kind: "matrix_policy";
  policy_kind: "boundary";
  authority_reference_id: ReferenceId;
  rationale_md: Uint8Array;
  permanence: "permanent" | "reopenable";
  reopening_signal?: NestedTriggerSignal;
}

export type MatrixPolicyPayload = MatrixMaintenancePolicy | MatrixBoundaryPolicy;
export type MatrixSemanticPayload = MatrixExternalCloseoutPayload | MatrixPolicyPayload;

export interface MatrixStatusAnnotationInput {
  id: string;
  status: string;
  emission?: Readonly<Record<string, { status?: string }>>;
}

export interface MatrixStatusFeatureInput {
  id: string;
  profile?: string;
}

export interface MatrixStatusCatalogVectorInput {
  expect?: string;
  class?: string;
}

export interface MatrixStatusCatalogRowInput {
  id: string;
  vectors: readonly MatrixStatusCatalogVectorInput[];
}

export interface MatrixStatusGateInput {
  id: string;
  kind: "cmd" | "cargo" | "stub";
  ignored_test?: string;
}

export interface MatrixStatusTimingInput {
  tier: "fast" | "local" | "full";
  wall_ms?: number;
}

export interface MatrixStatusInputs {
  matrix: {
    annotations: readonly MatrixStatusAnnotationInput[];
    features: readonly MatrixStatusFeatureInput[];
    containment_ids: readonly string[];
    control_operator_ids: readonly string[];
  };
  catalog: { rows: readonly MatrixStatusCatalogRowInput[] };
  registry: { gates: readonly MatrixStatusGateInput[] };
  timings: { tiers: readonly MatrixStatusTimingInput[] };
}

export interface LegacyStatusHeaderRunPlan {
  exit_code: 0 | 1;
  stdout: Uint8Array;
  stderr: Uint8Array;
  writes: readonly { path: RepoPath; bytes: Uint8Array }[];
}

export type ClassifiedLegacyStatusInvocation =
  | { mode: "report"; argv: readonly string[] }
  | {
      mode: "check" | "write";
      argv: readonly string[];
      targets: ReadonlyMap<RepoPath, Uint8Array>;
    };
