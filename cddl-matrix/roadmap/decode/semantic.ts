import type {
  ControlPayload,
  DecisionPayload,
  EvidencePayload,
  SharedSemanticPayload,
  WorkPayload,
} from "../model/core.ts";
import type { SemanticPayload } from "../model/documents.ts";
import {
  COMPARATORS,
  CONTROL_KINDS,
  CONTROL_STATES,
  DECISION_STATES,
  DISCRIMINATOR_ROWS,
  EVIDENCE_KINDS,
  FRESHNESS,
  PERMANENCE,
  PREDICATE_KINDS,
  PRIORITIES,
  RISKS,
  SHARED_SEMANTIC_KINDS,
  SHARED_SEMANTIC_SCHEMA_ROW_LIST,
  WORK_INTENTS,
  WORK_KINDS,
  WORK_STATES,
  armForDiscriminants,
  armOfGroupValue,
} from "../payload_descriptors.ts";
import { decodeArmFields, decodePredicate } from "./fields.ts";
import {
  childLogicalPath as p,
  expectEnum,
  expectExactTable,
  expectString,
  hasOwn,
  requiredValue,
  schemaFail,
  type DecodeContext,
  type EnumSchemaField,
  type ExactSchemaRow,
} from "./primitives.ts";

export const SEMANTIC_ENUM_FIELDS: readonly EnumSchemaField[] = [
  { name: "shared_semantic_kind", values: SHARED_SEMANTIC_KINDS },
  { name: "work_state", values: WORK_STATES },
  { name: "work_intent", values: WORK_INTENTS },
  { name: "work_kind", values: WORK_KINDS },
  { name: "risk", values: RISKS },
  { name: "priority_band", values: PRIORITIES },
  { name: "decision_state", values: DECISION_STATES },
  { name: "decision_permanence", values: PERMANENCE },
  { name: "predicate_kind", values: PREDICATE_KINDS },
  { name: "comparator", values: COMPARATORS },
  { name: "evidence_kind", values: EVIDENCE_KINDS },
  { name: "freshness", values: FRESHNESS },
  { name: "control_kind", values: CONTROL_KINDS },
  { name: "control_state", values: CONTROL_STATES },
];

export const SHARED_SEMANTIC_SCHEMA_ROWS: readonly ExactSchemaRow[] = SHARED_SEMANTIC_SCHEMA_ROW_LIST;

function decodeWork(ctx: DecodeContext, raw: unknown, path: string): WorkPayload {
  const discriminator = expectExactTable(ctx, raw, path, DISCRIMINATOR_ROWS.work);
  const state = expectEnum(ctx, requiredValue(discriminator, "work_state"), WORK_STATES, p(path, "work_state"));
  const arm = armForDiscriminants("work", { work_state: state });
  const payload = decodeArmFields(ctx, raw, path, arm, { kind: "work", work_state: state }) as unknown as WorkPayload;
  if (
    payload.work_kind === "defect" &&
    (payload.regression_evidence_ids?.length ?? 0) + (payload.regression_gap_ids?.length ?? 0) === 0
  ) {
    schemaFail(ctx, "E-SCHEMA-STATE", path, "defect work requires regression evidence or a regression-gap ID");
  }
  if (state === "ready" && payload.work_kind === "missing_system" && (payload.admission_ids?.length ?? 0) === 0) {
    schemaFail(ctx, "E-SCHEMA-STATE", path, "ready missing-system work requires an admission ID");
  }
  const record = payload as unknown as Record<string, unknown>;
  if (state === "waiting_external" && (record.retirement_predicate === undefined) === (record.unblock_predicate === undefined)) {
    schemaFail(ctx, "E-SCHEMA-STATE", path, "waiting-external work requires exactly one of retirement_predicate or unblock_predicate");
  }
  return payload;
}

function decodeDecision(ctx: DecodeContext, raw: unknown, path: string): DecisionPayload {
  const discriminator = expectExactTable(ctx, raw, path, DISCRIMINATOR_ROWS.decision);
  const state = expectEnum(ctx, requiredValue(discriminator, "decision_state"), DECISION_STATES, p(path, "decision_state"));
  if (state !== "decided") {
    const arm = armForDiscriminants("decision", { decision_state: state });
    return decodeArmFields(ctx, raw, path, arm, { kind: "decision", decision_state: state }) as unknown as DecisionPayload;
  }
  const pre = expectExactTable(ctx, raw, path, DISCRIMINATOR_ROWS.decided_decision);
  const permanence = expectEnum(ctx, requiredValue(pre, "permanence"), PERMANENCE, p(path, "permanence"));
  const arm = armForDiscriminants("decision", { decision_state: state, permanence });
  return decodeArmFields(
    ctx,
    raw,
    path,
    arm,
    { kind: "decision", decision_state: state, permanence },
  ) as unknown as DecisionPayload;
}

function decodeEvidence(ctx: DecodeContext, raw: unknown, path: string): EvidencePayload {
  const arm = armForDiscriminants("evidence", {});
  const table = expectExactTable(ctx, raw, path, arm.row);
  const kind = expectEnum(ctx, requiredValue(table, "evidence_kind"), EVIDENCE_KINDS, p(path, "evidence_kind"));
  const freshness = expectEnum(ctx, requiredValue(table, "freshness"), FRESHNESS, p(path, "freshness"));
  const hasPoint = hasOwn(table, "at_commit") || hasOwn(table, "environment_md");
  if (freshness === "live") {
    if (!hasOwn(table, "refresh_reference_id") || hasOwn(table, "observed_at") || hasPoint || hasOwn(table, "valid_through")) schemaFail(ctx, "E-SCHEMA-STATE", path, "live evidence requires refresh and forbids point provenance/validity");
  } else {
    if (!hasOwn(table, "observed_at") || !hasPoint || hasOwn(table, "refresh_reference_id")) schemaFail(ctx, "E-SCHEMA-STATE", path, `${freshness} evidence requires observed_at plus point provenance and forbids refresh`);
    if (freshness !== "as_of" && hasOwn(table, "valid_through")) schemaFail(ctx, "E-SCHEMA-FORBIDDEN-KEY", p(path, "valid_through"), "valid_through is permitted only for as_of evidence");
  }
  if (kind === "registry_enumeration" && !hasOwn(table, "enumerated_registry")) schemaFail(ctx, "E-SCHEMA-STATE", p(path, "enumerated_registry"), "registry enumeration evidence requires enumerated_registry");
  if (kind !== "registry_enumeration" && hasOwn(table, "enumerated_registry")) schemaFail(ctx, "E-SCHEMA-FORBIDDEN-KEY", p(path, "enumerated_registry"), "enumerated_registry belongs only to registry_enumeration evidence");
  const commandAllowed = ["harness_free_repro", "execution_probe", "registry_enumeration", "source_read"].includes(kind);
  if (!commandAllowed && (hasOwn(table, "command_md") || hasOwn(table, "result_md"))) schemaFail(ctx, "E-SCHEMA-FORBIDDEN-KEY", path, `${kind} evidence forbids command/result fields`);
  if (["harness_free_repro", "execution_probe"].includes(kind) && (!hasOwn(table, "command_md") || !hasOwn(table, "result_md") || !hasPoint)) schemaFail(ctx, "E-SCHEMA-STATE", path, `${kind} evidence requires command, result, and point provenance`);
  const payload = decodeArmFields(
    ctx,
    raw,
    path,
    arm,
    { kind: "evidence", evidence_kind: kind, freshness },
  ) as unknown as EvidencePayload;
  if (payload.observed_at !== undefined && payload.valid_through !== undefined && payload.valid_through < payload.observed_at) {
    schemaFail(ctx, "E-SCHEMA-STATE", p(path, "valid_through"), "valid_through must not precede observed_at");
  }
  if (Object.keys(payload.scope).length === 0) {
    schemaFail(ctx, "E-SCHEMA-FLOOR", p(path, "scope"), "evidence scope requires at least one coordinate set");
  }
  return payload;
}

function decodeControl(ctx: DecodeContext, raw: unknown, path: string): ControlPayload {
  const arm = armForDiscriminants("control", {});
  return decodeArmFields(ctx, raw, path, arm, { kind: "control" }) as unknown as ControlPayload;
}

export function decodeSharedSemanticPayload(
  ctx: DecodeContext,
  raw: unknown,
  path: string,
): SharedSemanticPayload | undefined {
  const pre = expectExactTable(ctx, raw, path, { name: "semantic payload discriminator", required: ["kind"], optional: Object.keys(raw === null || typeof raw !== "object" || Array.isArray(raw) ? {} : raw).filter((key) => key !== "kind") });
  const kind = expectString(ctx, requiredValue(pre, "kind"), p(path, "kind"));
  switch (kind) {
    case "work": return decodeWork(ctx, raw, path);
    case "decision": return decodeDecision(ctx, raw, path);
    case "evidence": return decodeEvidence(ctx, raw, path);
    case "control": return decodeControl(ctx, raw, path);
    default: return undefined;
  }
}

export function assertSharedPayload(payload: SharedSemanticPayload): SemanticPayload {
  return payload;
}
