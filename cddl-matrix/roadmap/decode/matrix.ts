import type {
  MatrixExternalCloseoutPayload,
  MatrixPolicyPayload,
  MatrixSemanticPayload,
} from "../model/matrix.ts";
import {
  CLOSEOUT_STATES,
  DISCRIMINATOR_ROWS,
  MATRIX_SCHEMA_ROW_LIST,
  MATRIX_SEMANTIC_KINDS,
  PERMANENCE,
  POLICY_KINDS,
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

export const MATRIX_ENUM_FIELDS: readonly EnumSchemaField[] = [
  { name: "matrix_semantic_kind", values: MATRIX_SEMANTIC_KINDS },
  { name: "closeout_state", values: CLOSEOUT_STATES },
  { name: "policy_kind", values: POLICY_KINDS },
  { name: "policy_permanence", values: PERMANENCE },
] as const;

export const MATRIX_SCHEMA_ROWS: readonly ExactSchemaRow[] = MATRIX_SCHEMA_ROW_LIST;

function decodeCloseout(ctx: DecodeContext, raw: unknown, path: string): MatrixExternalCloseoutPayload {
  const pre = expectExactTable(ctx, raw, path, DISCRIMINATOR_ROWS.matrix_closeout);
  const state = expectEnum(ctx, requiredValue(pre, "closeout_state"), CLOSEOUT_STATES, p(path, "closeout_state"));
  const arm = armForDiscriminants("matrix_external_closeout", { closeout_state: state });
  const payload = decodeArmFields(
    ctx,
    raw,
    path,
    arm,
    { kind: "matrix_external_closeout", closeout_state: state },
  ) as unknown as MatrixExternalCloseoutPayload;
  if (state === "due" && payload.actions.length === 0) {
    schemaFail(ctx, "E-SCHEMA-FLOOR", p(path, "action"), "due closeout requires at least one action");
  }
  return payload;
}

function decodePolicy(ctx: DecodeContext, raw: unknown, path: string): MatrixPolicyPayload {
  const pre = expectExactTable(ctx, raw, path, DISCRIMINATOR_ROWS.matrix_policy);
  const kind = expectEnum(ctx, requiredValue(pre, "policy_kind"), POLICY_KINDS, p(path, "policy_kind"));
  if (kind === "maintenance_protocol") {
    const arm = armForDiscriminants("matrix_policy", { policy_kind: kind });
    return decodeArmFields(
      ctx,
      raw,
      path,
      arm,
      { kind: "matrix_policy", policy_kind: kind },
    ) as unknown as MatrixPolicyPayload;
  }
  const boundary = expectExactTable(ctx, raw, path, DISCRIMINATOR_ROWS.matrix_boundary);
  const permanence = expectEnum(ctx, requiredValue(boundary, "permanence"), PERMANENCE, p(path, "permanence"));
  const arm = armForDiscriminants("matrix_policy", { policy_kind: kind, permanence });
  return decodeArmFields(
    ctx,
    raw,
    path,
    arm,
    { kind: "matrix_policy", policy_kind: kind, permanence },
  ) as unknown as MatrixPolicyPayload;
}

export function decodeMatrixPayload(
  ctx: DecodeContext,
  raw: unknown,
  path: string,
  kind: string,
): MatrixSemanticPayload | undefined {
  if (kind === "matrix_external_closeout") return decodeCloseout(ctx, raw, path);
  if (kind === "matrix_policy") return decodePolicy(ctx, raw, path);
  return undefined;
}
