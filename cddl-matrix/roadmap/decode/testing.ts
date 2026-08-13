import type {
  TestingCostPayload,
  TestingIncidentPayload,
  TestingOperationalWatchPayload,
  TestingSemanticPayload,
  TestingSystemAdmissionPayload,
} from "../model/testing.ts";
import {
  ADMISSION_KINDS,
  COST_POSTURES,
  DISCRIMINATOR_ROWS,
  INCIDENT_POSTURES,
  TESTING_SCHEMA_ROW_LIST,
  TESTING_SEMANTIC_KINDS,
  WATCH_STATES,
  armForDiscriminants,
} from "../payload_descriptors.ts";
import { decodeArmFields } from "./fields.ts";
import {
  childLogicalPath as p,
  expectEnum,
  expectExactTable,
  requiredValue,
  schemaFail,
  type DecodeContext,
  type EnumSchemaField,
  type ExactSchemaRow,
} from "./primitives.ts";

export const TESTING_ENUM_FIELDS: readonly EnumSchemaField[] = [
  { name: "testing_semantic_kind", values: TESTING_SEMANTIC_KINDS },
  { name: "watch_state", values: WATCH_STATES },
  { name: "incident_posture", values: INCIDENT_POSTURES },
  { name: "cost_posture", values: COST_POSTURES },
  { name: "admission_kind", values: ADMISSION_KINDS },
] as const;

export const TESTING_SCHEMA_ROWS: readonly ExactSchemaRow[] = TESTING_SCHEMA_ROW_LIST;

function decodeWatch(ctx: DecodeContext, raw: unknown, path: string): TestingOperationalWatchPayload {
  const pre = expectExactTable(ctx, raw, path, DISCRIMINATOR_ROWS.testing_watch);
  const state = expectEnum(ctx, requiredValue(pre, "watch_state"), WATCH_STATES, p(path, "watch_state"));
  const arm = armForDiscriminants("testing_operational_watch", { watch_state: state });
  return decodeArmFields(
    ctx,
    raw,
    path,
    arm,
    { kind: "testing_operational_watch", watch_state: state },
  ) as unknown as TestingOperationalWatchPayload;
}

function decodeIncident(ctx: DecodeContext, raw: unknown, path: string): TestingIncidentPayload {
  const pre = expectExactTable(ctx, raw, path, DISCRIMINATOR_ROWS.testing_incident);
  const posture = expectEnum(ctx, requiredValue(pre, "incident_posture"), INCIDENT_POSTURES, p(path, "incident_posture"));
  const arm = armForDiscriminants("testing_incident", { incident_posture: posture });
  return decodeArmFields(
    ctx,
    raw,
    path,
    arm,
    { kind: "testing_incident", incident_posture: posture },
  ) as unknown as TestingIncidentPayload;
}

function decodeCost(ctx: DecodeContext, raw: unknown, path: string): TestingCostPayload {
  const pre = expectExactTable(ctx, raw, path, DISCRIMINATOR_ROWS.testing_cost);
  const posture = expectEnum(ctx, requiredValue(pre, "cost_posture"), COST_POSTURES, p(path, "cost_posture"));
  const arm = armForDiscriminants("testing_cost", { cost_posture: posture });
  const payload = decodeArmFields(
    ctx,
    raw,
    path,
    arm,
    { kind: "testing_cost", cost_posture: posture },
  ) as unknown as TestingCostPayload;
  if (payload.cost_posture === "historical_observation") {
    if (payload.value_min < 0 || payload.value_max < 0 || payload.value_min > payload.value_max) {
      schemaFail(ctx, "E-SCHEMA-STATE", path, "historical cost requires 0 <= value_min <= value_max");
    }
  }
  return payload;
}

function decodeAdmission(ctx: DecodeContext, raw: unknown, path: string): TestingSystemAdmissionPayload {
  const pre = expectExactTable(ctx, raw, path, DISCRIMINATOR_ROWS.testing_admission);
  const kind = expectEnum(ctx, requiredValue(pre, "admission_kind"), ADMISSION_KINDS, p(path, "admission_kind"));
  const arm = armForDiscriminants("testing_system_admission", { admission_kind: kind });
  const payload = decodeArmFields(
    ctx,
    raw,
    path,
    arm,
    { kind: "testing_system_admission", admission_kind: kind },
  ) as unknown as TestingSystemAdmissionPayload;
  if (payload.admission_kind === "independent_recurrence" && payload.incident_ids.length < 2) {
    schemaFail(ctx, "E-SCHEMA-FLOOR", p(path, "incident_ids"), "independent recurrence requires two distinct incidents");
  }
  return payload;
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
