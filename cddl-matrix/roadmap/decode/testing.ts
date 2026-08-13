import type {
  TestingCaptureStep,
  TestingCostPayload,
  TestingIncidentPayload,
  TestingOperationalWatchPayload,
  TestingSemanticPayload,
  TestingSystemAdmissionPayload,
} from "../model/testing.ts";
import {
  childLogicalPath as p,
  expectArrayOf,
  expectCivilDate,
  expectEnum,
  expectExactTable,
  expectFiniteNumber,
  expectMarkdown,
  expectNonemptyArray,
  expectReferenceId,
  expectRoadmapId,
  expectRoadmapIdSet,
  expectString,
  expectSubordinateSlug,
  hasOwn,
  optionalValue,
  requiredValue,
  schemaFail,
  type DecodeContext,
  type EnumSchemaField,
  type ExactSchemaRow,
} from "./primitives.ts";

export const TESTING_ENUM_FIELDS: readonly EnumSchemaField[] = [
  { name: "testing_semantic_kind", values: ["testing_operational_watch", "testing_incident", "testing_cost", "testing_system_admission"] },
  { name: "watch_state", values: ["watching", "attributed", "retire_pending"] },
  { name: "incident_posture", values: ["live", "attributed", "historical"] },
  { name: "cost_posture", values: ["live_registry", "historical_observation"] },
  { name: "admission_kind", values: ["silent_corruption", "independent_recurrence"] },
] as const;

export const TESTING_SCHEMA_ROWS: readonly ExactSchemaRow[] = [
  { name: "watching operational watch", required: ["kind", "watch_state", "signature_md", "response_md", "escalation_transition_id", "retirement_semantics_md", "capture_step"], optional: ["detail_md"], forbidden: ["attribution_md", "operating_rule_reference_id", "retirement_reference_id"] },
  { name: "attributed operational watch", required: ["kind", "watch_state", "signature_md", "attribution_md", "operating_rule_reference_id", "response_md", "escalation_transition_id", "retirement_semantics_md", "capture_step"], optional: ["detail_md"], forbidden: ["retirement_reference_id"] },
  { name: "retire-pending operational watch", required: ["kind", "watch_state", "signature_md", "attribution_md", "operating_rule_reference_id", "response_md", "escalation_transition_id", "retirement_reference_id", "retirement_semantics_md", "capture_step"], optional: ["detail_md"] },
  { name: "watch capture step", required: ["step_id", "capture_md"] },
  { name: "live testing incident", required: ["kind", "incident_posture", "signature_md", "evidence_ids"], optional: ["detail_md"], forbidden: ["attribution_md", "operating_rule_reference_id", "retirement_reference_id"] },
  { name: "attributed testing incident", required: ["kind", "incident_posture", "signature_md", "evidence_ids", "attribution_md", "operating_rule_reference_id"], optional: ["detail_md"], forbidden: ["retirement_reference_id"] },
  { name: "historical testing incident", required: ["kind", "incident_posture", "signature_md", "evidence_ids", "attribution_md", "operating_rule_reference_id", "retirement_reference_id"], optional: ["detail_md"] },
  { name: "live registry testing cost", required: ["kind", "cost_posture", "unit", "scope_md", "gate_reference_id"], optional: ["detail_md"], forbidden: ["value_min", "value_max", "observed_at", "environment_md", "evidence_ids", "valid_through"] },
  { name: "historical testing cost", required: ["kind", "cost_posture", "unit", "scope_md", "value_min", "value_max", "observed_at", "environment_md", "evidence_ids"], optional: ["detail_md"], forbidden: ["gate_reference_id", "valid_through"] },
  { name: "silent-corruption admission", required: ["kind", "admission_kind", "claim_md", "evidence_ids"], optional: ["detail_md"], forbidden: ["incident_ids"] },
  { name: "independent-recurrence admission", required: ["kind", "admission_kind", "claim_md", "evidence_ids", "incident_ids"], optional: ["detail_md"] },
] as const;

function common(
  ctx: DecodeContext,
  table: object,
  path: string,
): { detail_md?: Uint8Array } {
  return {
    ...(hasOwn(table, "detail_md")
      ? { detail_md: expectMarkdown(ctx, optionalValue(table, "detail_md"), p(path, "detail_md")) }
      : {}),
  };
}

function decodeCapture(ctx: DecodeContext, raw: unknown, path: string): TestingCaptureStep {
  const table = expectExactTable(ctx, raw, path, TESTING_SCHEMA_ROWS[3]);
  const step = expectSubordinateSlug(ctx, requiredValue(table, "step_id"), p(path, "step_id"));
  return { step_id: step, capture_md: expectMarkdown(ctx, requiredValue(table, "capture_md"), p(path, "capture_md")) };
}

function decodeWatch(ctx: DecodeContext, raw: unknown, path: string): TestingOperationalWatchPayload {
  const pre = expectExactTable(ctx, raw, path, {
    name: "operational watch discriminator",
    required: ["kind", "watch_state"],
    optional: ["detail_md", "signature_md", "attribution_md", "operating_rule_reference_id", "response_md", "escalation_transition_id", "retirement_reference_id", "retirement_semantics_md", "capture_step"],
  });
  const state = expectEnum(ctx, requiredValue(pre, "watch_state"), ["watching", "attributed", "retire_pending"] as const, p(path, "watch_state"));
  const table = expectExactTable(ctx, raw, path, TESTING_SCHEMA_ROWS[state === "watching" ? 0 : state === "attributed" ? 1 : 2]);
  const capture_steps = expectNonemptyArray(
    ctx,
    expectArrayOf(ctx, requiredValue(table, "capture_step"), p(path, "capture_step"), (entry, entryPath) =>
      decodeCapture(ctx, entry, entryPath),
    ),
    p(path, "capture_step"),
  );
  const base = {
    kind: "testing_operational_watch" as const,
    ...common(ctx, table, path),
    watch_state: state,
    signature_md: expectMarkdown(ctx, requiredValue(table, "signature_md"), p(path, "signature_md")),
    response_md: expectMarkdown(ctx, requiredValue(table, "response_md"), p(path, "response_md")),
    escalation_transition_id: expectRoadmapId(ctx, requiredValue(table, "escalation_transition_id"), p(path, "escalation_transition_id")),
    retirement_semantics_md: expectMarkdown(ctx, requiredValue(table, "retirement_semantics_md"), p(path, "retirement_semantics_md")),
    capture_steps,
  };
  if (state === "watching") return { ...base, watch_state: state };
  const attributed = {
    ...base,
    watch_state: state,
    attribution_md: expectMarkdown(ctx, requiredValue(table, "attribution_md"), p(path, "attribution_md")),
    operating_rule_reference_id: expectReferenceId(ctx, requiredValue(table, "operating_rule_reference_id"), p(path, "operating_rule_reference_id")),
  };
  if (state === "attributed") return { ...attributed, watch_state: state };
  return {
    ...attributed,
    watch_state: state,
    retirement_reference_id: expectReferenceId(ctx, requiredValue(table, "retirement_reference_id"), p(path, "retirement_reference_id")),
  };
}

function decodeIncident(ctx: DecodeContext, raw: unknown, path: string): TestingIncidentPayload {
  const pre = expectExactTable(ctx, raw, path, { name: "testing incident discriminator", required: ["kind", "incident_posture"], optional: ["detail_md", "signature_md", "evidence_ids", "attribution_md", "operating_rule_reference_id", "retirement_reference_id"] });
  const posture = expectEnum(ctx, requiredValue(pre, "incident_posture"), ["live", "attributed", "historical"] as const, p(path, "incident_posture"));
  const table = expectExactTable(ctx, raw, path, TESTING_SCHEMA_ROWS[posture === "live" ? 4 : posture === "attributed" ? 5 : 6]);
  const base = { kind: "testing_incident" as const, ...common(ctx, table, path), incident_posture: posture, signature_md: expectMarkdown(ctx, requiredValue(table, "signature_md"), p(path, "signature_md")), evidence_ids: expectRoadmapIdSet(ctx, requiredValue(table, "evidence_ids"), p(path, "evidence_ids"), true) };
  if (posture === "live") return { ...base, incident_posture: posture };
  const attributed = { ...base, incident_posture: posture, attribution_md: expectMarkdown(ctx, requiredValue(table, "attribution_md"), p(path, "attribution_md")), operating_rule_reference_id: expectReferenceId(ctx, requiredValue(table, "operating_rule_reference_id"), p(path, "operating_rule_reference_id")) };
  if (posture === "attributed") return { ...attributed, incident_posture: posture };
  return { ...attributed, incident_posture: posture, retirement_reference_id: expectReferenceId(ctx, requiredValue(table, "retirement_reference_id"), p(path, "retirement_reference_id")) };
}

function decodeCost(ctx: DecodeContext, raw: unknown, path: string): TestingCostPayload {
  const pre = expectExactTable(ctx, raw, path, { name: "testing cost discriminator", required: ["kind", "cost_posture"], optional: ["detail_md", "unit", "scope_md", "gate_reference_id", "value_min", "value_max", "observed_at", "environment_md", "evidence_ids", "valid_through"] });
  const posture = expectEnum(ctx, requiredValue(pre, "cost_posture"), ["live_registry", "historical_observation"] as const, p(path, "cost_posture"));
  const table = expectExactTable(ctx, raw, path, TESTING_SCHEMA_ROWS[posture === "live_registry" ? 7 : 8]);
  const base = { kind: "testing_cost" as const, ...common(ctx, table, path), cost_posture: posture, unit: expectString(ctx, requiredValue(table, "unit"), p(path, "unit")), scope_md: expectMarkdown(ctx, requiredValue(table, "scope_md"), p(path, "scope_md")) };
  if (posture === "live_registry") return { ...base, cost_posture: posture, gate_reference_id: expectReferenceId(ctx, requiredValue(table, "gate_reference_id"), p(path, "gate_reference_id")) };
  const value_min = expectFiniteNumber(ctx, requiredValue(table, "value_min"), p(path, "value_min"));
  const value_max = expectFiniteNumber(ctx, requiredValue(table, "value_max"), p(path, "value_max"));
  if (value_min < 0 || value_max < 0 || value_min > value_max) schemaFail(ctx, "E-SCHEMA-STATE", path, "historical cost requires 0 <= value_min <= value_max");
  return { ...base, cost_posture: posture, value_min, value_max, observed_at: expectCivilDate(ctx, requiredValue(table, "observed_at"), p(path, "observed_at")), environment_md: expectMarkdown(ctx, requiredValue(table, "environment_md"), p(path, "environment_md")), evidence_ids: expectRoadmapIdSet(ctx, requiredValue(table, "evidence_ids"), p(path, "evidence_ids"), true) };
}

function decodeAdmission(ctx: DecodeContext, raw: unknown, path: string): TestingSystemAdmissionPayload {
  const pre = expectExactTable(ctx, raw, path, { name: "testing admission discriminator", required: ["kind", "admission_kind"], optional: ["detail_md", "claim_md", "evidence_ids", "incident_ids"] });
  const kind = expectEnum(ctx, requiredValue(pre, "admission_kind"), ["silent_corruption", "independent_recurrence"] as const, p(path, "admission_kind"));
  const table = expectExactTable(ctx, raw, path, TESTING_SCHEMA_ROWS[kind === "silent_corruption" ? 9 : 10]);
  const base = { kind: "testing_system_admission" as const, ...common(ctx, table, path), admission_kind: kind, claim_md: expectMarkdown(ctx, requiredValue(table, "claim_md"), p(path, "claim_md")), evidence_ids: expectRoadmapIdSet(ctx, requiredValue(table, "evidence_ids"), p(path, "evidence_ids"), true) };
  if (kind === "silent_corruption") return { ...base, admission_kind: kind };
  const incident_ids = expectRoadmapIdSet(ctx, requiredValue(table, "incident_ids"), p(path, "incident_ids"), true);
  if (incident_ids.length < 2) schemaFail(ctx, "E-SCHEMA-FLOOR", p(path, "incident_ids"), "independent recurrence requires two distinct incidents");
  return { ...base, admission_kind: kind, incident_ids };
}

export function decodeTestingPayload(
  ctx: DecodeContext,
  raw: unknown,
  path: string,
  kind: string,
): TestingSemanticPayload | undefined {
  if (kind === "testing_operational_watch") return decodeWatch(ctx, raw, path);
  if (kind === "testing_incident") return decodeIncident(ctx, raw, path);
  if (kind === "testing_cost") return decodeCost(ctx, raw, path);
  if (kind === "testing_system_admission") return decodeAdmission(ctx, raw, path);
  return undefined;
}
