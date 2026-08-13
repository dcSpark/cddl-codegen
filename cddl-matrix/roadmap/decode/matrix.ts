import type {
  MatrixCloseoutAction,
  MatrixCloseoutBranch,
  MatrixExternalCloseoutPayload,
  MatrixPolicyPayload,
  MatrixSemanticPayload,
} from "../model/matrix.ts";
import {
  childLogicalPath as p,
  expectArrayOf,
  expectEnum,
  expectExactTable,
  expectMarkdown,
  expectNonemptyArray,
  expectReferenceId,
  expectReferenceIdSet,
  expectRoadmapId,
  expectRoadmapIdSet,
  expectSubordinateSlug,
  hasOwn,
  optionalDecoded,
  optionalValue,
  requiredValue,
  schemaFail,
  type DecodeContext,
  type EnumSchemaField,
  type ExactSchemaRow,
} from "./primitives.ts";

export const MATRIX_ENUM_FIELDS: readonly EnumSchemaField[] = [
  { name: "matrix_semantic_kind", values: ["matrix_external_closeout", "matrix_policy"] },
  { name: "closeout_state", values: ["waiting", "due", "blocked"] },
  { name: "policy_kind", values: ["maintenance_protocol", "boundary"] },
  { name: "policy_permanence", values: ["permanent", "reopenable"] },
] as const;

export const MATRIX_SCHEMA_ROWS: readonly ExactSchemaRow[] = [
  { name: "waiting matrix closeout", required: ["kind", "closeout_state", "upstream_owner_reference_id", "current_upstream_state_md", "transition_ids", "verification_md"], optional: ["detail_md", "prune_reference_ids", "action", "branch"], forbidden: ["blocker_md"] },
  { name: "due matrix closeout", required: ["kind", "closeout_state", "upstream_owner_reference_id", "current_upstream_state_md", "transition_ids", "verification_md", "action"], optional: ["detail_md", "prune_reference_ids", "branch"], forbidden: ["blocker_md"] },
  { name: "blocked matrix closeout", required: ["kind", "closeout_state", "upstream_owner_reference_id", "current_upstream_state_md", "blocker_md", "transition_ids", "verification_md"], optional: ["detail_md", "prune_reference_ids", "action", "branch"] },
  { name: "matrix closeout action", required: ["action_id", "action_md"] },
  { name: "matrix closeout branch", required: ["branch_id", "predicate_md", "action"], optional: ["prune_reference_ids"] },
  { name: "matrix closeout branch action", required: ["action_id"] },
  { name: "matrix maintenance policy", required: ["kind", "policy_kind", "authority_reference_id", "protocol_md", "cadence_transition_id"], optional: ["detail_md"] },
  { name: "matrix permanent boundary", required: ["kind", "policy_kind", "authority_reference_id", "rationale_md", "permanence"], optional: ["detail_md"], forbidden: ["reopening_transition_id"] },
  { name: "matrix reopenable boundary", required: ["kind", "policy_kind", "authority_reference_id", "rationale_md", "permanence", "reopening_transition_id"], optional: ["detail_md"] },
] as const;

function detail(ctx: DecodeContext, table: object, path: string): object {
  return hasOwn(table, "detail_md")
    ? { detail_md: expectMarkdown(ctx, optionalValue(table, "detail_md"), p(path, "detail_md")) }
    : {};
}

function common(
  ctx: DecodeContext,
  table: object,
  path: string,
): { detail_md?: Uint8Array } {
  return {
    ...detail(ctx, table, path),
  };
}

function decodeAction(ctx: DecodeContext, raw: unknown, path: string): MatrixCloseoutAction {
  const table = expectExactTable(ctx, raw, path, MATRIX_SCHEMA_ROWS[3]);
  return {
    action_id: expectSubordinateSlug(ctx, requiredValue(table, "action_id"), p(path, "action_id")),
    action_md: expectMarkdown(ctx, requiredValue(table, "action_md"), p(path, "action_md")),
  };
}

function decodeBranch(ctx: DecodeContext, raw: unknown, path: string): MatrixCloseoutBranch {
  const table = expectExactTable(ctx, raw, path, MATRIX_SCHEMA_ROWS[4]);
  const actionIds = expectNonemptyArray(
    ctx,
    expectArrayOf(ctx, requiredValue(table, "action"), p(path, "action"), (entry, entryPath) => {
      const action = expectExactTable(ctx, entry, entryPath, MATRIX_SCHEMA_ROWS[5]);
      return expectSubordinateSlug(ctx, requiredValue(action, "action_id"), p(entryPath, "action_id"));
    }),
    p(path, "action"),
  );
  return {
    branch_id: expectSubordinateSlug(ctx, requiredValue(table, "branch_id"), p(path, "branch_id")),
    predicate_md: expectMarkdown(ctx, requiredValue(table, "predicate_md"), p(path, "predicate_md")),
    ...(hasOwn(table, "prune_reference_ids")
      ? {
          prune_reference_ids: expectReferenceIdSet(
            ctx,
            optionalValue(table, "prune_reference_ids"),
            p(path, "prune_reference_ids"),
            true,
          ),
        }
      : {}),
    action_ids: actionIds,
  };
}

function decodeCloseout(ctx: DecodeContext, raw: unknown, path: string): MatrixExternalCloseoutPayload {
  const pre = expectExactTable(ctx, raw, path, {
    name: "matrix closeout discriminator",
    required: ["kind", "closeout_state"],
    optional: [
      "detail_md",
      "upstream_owner_reference_id",
      "current_upstream_state_md",
      "blocker_md",
      "transition_ids",
      "verification_md",
      "prune_reference_ids",
      "action",
      "branch",
    ],
  });
  const state = expectEnum(
    ctx,
    requiredValue(pre, "closeout_state"),
    ["waiting", "due", "blocked"] as const,
    p(path, "closeout_state"),
  );
  const table = expectExactTable(ctx, raw, path, MATRIX_SCHEMA_ROWS[state === "waiting" ? 0 : state === "due" ? 1 : 2]);
  const actions = optionalDecoded(table, "action", path, (value, fieldPath) =>
    expectArrayOf(ctx, value, fieldPath, (entry, entryPath) => decodeAction(ctx, entry, entryPath)),
  ) ?? [];
  if (state === "due" && actions.length === 0) {
    schemaFail(ctx, "E-SCHEMA-FLOOR", p(path, "action"), "due closeout requires at least one action");
  }
  const branches = optionalDecoded(table, "branch", path, (value, fieldPath) =>
    expectArrayOf(ctx, value, fieldPath, (entry, entryPath) => decodeBranch(ctx, entry, entryPath)),
  ) ?? [];
  const base = {
    kind: "matrix_external_closeout" as const,
    ...common(ctx, table, path),
    closeout_state: state,
    upstream_owner_reference_id: expectReferenceId(
      ctx,
      requiredValue(table, "upstream_owner_reference_id"),
      p(path, "upstream_owner_reference_id"),
    ),
    current_upstream_state_md: expectMarkdown(
      ctx,
      requiredValue(table, "current_upstream_state_md"),
      p(path, "current_upstream_state_md"),
    ),
    transition_ids: expectRoadmapIdSet(ctx, requiredValue(table, "transition_ids"), p(path, "transition_ids"), true),
    verification_md: expectMarkdown(ctx, requiredValue(table, "verification_md"), p(path, "verification_md")),
    ...(hasOwn(table, "prune_reference_ids")
      ? {
          prune_reference_ids: expectReferenceIdSet(
            ctx,
            optionalValue(table, "prune_reference_ids"),
            p(path, "prune_reference_ids"),
            true,
          ),
        }
      : {}),
    actions,
    branches,
  };
  if (state === "blocked") {
    return {
      ...base,
      closeout_state: state,
      blocker_md: expectMarkdown(ctx, requiredValue(table, "blocker_md"), p(path, "blocker_md")),
    };
  }
  return { ...base, closeout_state: state };
}

function decodePolicy(ctx: DecodeContext, raw: unknown, path: string): MatrixPolicyPayload {
  const pre = expectExactTable(ctx, raw, path, {
    name: "matrix policy discriminator",
    required: ["kind", "policy_kind"],
    optional: [
      "detail_md",
      "authority_reference_id",
      "protocol_md",
      "cadence_transition_id",
      "rationale_md",
      "permanence",
      "reopening_transition_id",
    ],
  });
  const kind = expectEnum(
    ctx,
    requiredValue(pre, "policy_kind"),
    ["maintenance_protocol", "boundary"] as const,
    p(path, "policy_kind"),
  );
  if (kind === "maintenance_protocol") {
    const table = expectExactTable(ctx, raw, path, MATRIX_SCHEMA_ROWS[6]);
    return {
      kind: "matrix_policy",
      ...common(ctx, table, path),
      policy_kind: kind,
      authority_reference_id: expectReferenceId(
        ctx,
        requiredValue(table, "authority_reference_id"),
        p(path, "authority_reference_id"),
      ),
      protocol_md: expectMarkdown(ctx, requiredValue(table, "protocol_md"), p(path, "protocol_md")),
      cadence_transition_id: expectRoadmapId(
        ctx,
        requiredValue(table, "cadence_transition_id"),
        p(path, "cadence_transition_id"),
      ),
    };
  }
  const boundary = expectExactTable(ctx, raw, path, {
    name: "matrix boundary discriminator",
    required: ["kind", "policy_kind", "permanence"],
    optional: ["detail_md", "authority_reference_id", "rationale_md", "reopening_transition_id"],
  });
  const permanence = expectEnum(
    ctx,
    requiredValue(boundary, "permanence"),
    ["permanent", "reopenable"] as const,
    p(path, "permanence"),
  );
  const table = expectExactTable(ctx, raw, path, MATRIX_SCHEMA_ROWS[permanence === "permanent" ? 7 : 8]);
  return {
    kind: "matrix_policy",
    ...common(ctx, table, path),
    policy_kind: kind,
    authority_reference_id: expectReferenceId(
      ctx,
      requiredValue(table, "authority_reference_id"),
      p(path, "authority_reference_id"),
    ),
    rationale_md: expectMarkdown(ctx, requiredValue(table, "rationale_md"), p(path, "rationale_md")),
    permanence,
    ...(permanence === "reopenable"
      ? {
          reopening_transition_id: expectRoadmapId(
            ctx,
            requiredValue(table, "reopening_transition_id"),
            p(path, "reopening_transition_id"),
          ),
        }
      : {}),
  };
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
