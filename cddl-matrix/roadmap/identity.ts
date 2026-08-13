import type { RoadmapIssue } from "./errors.ts";
import type { DocumentIdentityInputs, RoadmapIdProviderFact } from "./indexes.ts";
import { validateRoadmapId } from "./ids.ts";
import type { RoadmapId, RoadmapName } from "./model/core.ts";
import type { CurrentGuard } from "./model/documents.ts";
import { isImmutableByteView } from "./render_ir.ts";
import { codePointSort } from "./kernel.ts";

export type GlobalOwnerKind = "first_class" | "current_guard";

export interface GlobalOwnerClaim {
  readonly id: RoadmapId;
  readonly namespace: RoadmapName;
  readonly owner_kind: GlobalOwnerKind;
  readonly source: string;
  readonly logical_path: string;
  readonly value: RoadmapIdProviderFact | CurrentGuard;
}

export interface GlobalIdentityValidationInputs {
  readonly documents: readonly DocumentIdentityInputs[];
  readonly current_guards?: readonly CurrentGuard[];
}

export interface GlobalIdentityResult {
  readonly owners: ReadonlyMap<RoadmapId, GlobalOwnerClaim>;
  readonly owner_claims: ReadonlyMap<RoadmapId, readonly GlobalOwnerClaim[]>;
  readonly aliases: ReadonlyMap<string, readonly {
    namespace: RoadmapName;
    owner_kind: string;
    owner_id: string;
    logical_path: string;
  }[]>;
  readonly issues: readonly RoadmapIssue[];
}

function issue(
  code: Extract<RoadmapIssue["code"], "E-OWNER-DUPLICATE" | "E-ALIAS-COLLISION" | `E-ID-${string}`>,
  logicalPath: string,
  message: string,
): RoadmapIssue {
  return { code, source: "<identity>", logical_path: logicalPath, message, exit: 1 };
}

const OWNER_RANK: Readonly<Record<GlobalOwnerKind, number>> = Object.freeze({
  first_class: 0,
  current_guard: 1,
});

function stableValueKey(value: unknown): string {
  if (isImmutableByteView(value)) {
    return `byte-view:${value.byte_length}:${value.wholeSha256()}`;
  }
  if (value instanceof Uint8Array) {
    return `bytes:${[...value].map((byte) => byte.toString(16).padStart(2, "0")).join("")}`;
  }
  if (Array.isArray(value)) return `array:[${value.map(stableValueKey).join(",")}]`;
  if (value !== null && typeof value === "object") {
    return `object:{${Object.keys(value as object).sort(codePointSort).map((key) =>
      `${JSON.stringify(key)}:${stableValueKey((value as Record<string, unknown>)[key])}`
    ).join(",")}}`;
  }
  return `${typeof value}:${JSON.stringify(value)}`;
}

export function identityOwnerClaimKey(claim: GlobalOwnerClaim): readonly (string | number)[] {
  return [
    claim.id,
    OWNER_RANK[claim.owner_kind],
    claim.namespace,
    claim.source,
    claim.logical_path,
    stableValueKey(claim.value),
  ];
}

function claimSort(left: GlobalOwnerClaim, right: GlobalOwnerClaim): number {
  const leftKey = identityOwnerClaimKey(left);
  const rightKey = identityOwnerClaimKey(right);
  for (let index = 0; index < leftKey.length; index += 1) {
    const leftValue = leftKey[index]!;
    const rightValue = rightKey[index]!;
    const compared = typeof leftValue === "number"
      ? leftValue - (rightValue as number)
      : codePointSort(leftValue, rightValue as string);
    if (compared !== 0) return compared;
  }
  return 0;
}

function namespaceOf(id: RoadmapId): RoadmapName | undefined {
  const result = validateRoadmapId(id);
  if (!result.ok) return undefined;
  return id.startsWith("matrix.") ? "matrix" : "testing";
}

/** Normalize the global first-class/guard domain after document-local indexing. */
export function validateGlobalIdentity(
  inputs: GlobalIdentityValidationInputs,
): GlobalIdentityResult {
  const issues: RoadmapIssue[] = [];
  const claims: GlobalOwnerClaim[] = [];
  const aliases = new Map<string, Array<{
    namespace: RoadmapName;
    owner_kind: string;
    owner_id: string;
    logical_path: string;
  }>>();

  for (const document of [...inputs.documents].sort((left, right) =>
    codePointSort(left.namespace, right.namespace)
  )) {
    for (const provider of document.id_providers) {
      claims.push({
        id: provider.id,
        namespace: document.namespace,
        owner_kind: "first_class",
        source: `roadmap:${document.namespace}`,
        logical_path: provider.logical_path,
        value: provider,
      });
    }
    for (const alias of document.alias_providers) {
      const group = aliases.get(alias.alias) ?? [];
      group.push({
        namespace: alias.namespace,
        owner_kind: alias.owner_kind,
        owner_id: alias.owner_id,
        logical_path: alias.logical_path,
      });
      aliases.set(alias.alias, group);
    }
  }
  for (const guard of inputs.current_guards ?? []) {
    const namespace = namespaceOf(guard.id);
    if (namespace === undefined) {
      const result = validateRoadmapId(guard.id);
      if (!result.ok) issues.push(issue(result.code, `guard[${JSON.stringify(guard.id)}]`, result.message));
      continue;
    }
    if (guard.guard_role !== "generic") {
      const familyResult = validateRoadmapId(guard.family_root_id);
      if (!familyResult.ok) {
        issues.push(issue(
          familyResult.code,
          `guard[${JSON.stringify(guard.id)}].family_root_id`,
          familyResult.message,
        ));
        continue;
      }
      const familyNamespace = namespaceOf(guard.family_root_id);
      if (familyNamespace !== namespace ||
        (guard.guard_role === "closed_family_root") !== (guard.family_root_id === guard.id)) {
        issues.push(issue(
          "E-ID-NAMESPACE",
          `guard[${JSON.stringify(guard.id)}].family_root_id`,
          "family guard root metadata must share the guard namespace and be self-identical exactly for the root role",
        ));
        continue;
      }
    }
    claims.push({
      id: guard.id,
      namespace,
      owner_kind: "current_guard",
      source: guard.owner_registry,
      logical_path: `guard[${JSON.stringify(guard.id)}]`,
      value: guard,
    });
  }
  claims.sort(claimSort);

  const grouped = new Map<RoadmapId, GlobalOwnerClaim[]>();
  for (const claim of claims) {
    const group = grouped.get(claim.id) ?? [];
    group.push(claim);
    grouped.set(claim.id, group);
  }
  const owners = new Map<RoadmapId, GlobalOwnerClaim>();
  const frozenGroups = new Map<RoadmapId, readonly GlobalOwnerClaim[]>();
  for (const [id, group] of [...grouped].sort(([left], [right]) => codePointSort(left, right))) {
    frozenGroups.set(id, Object.freeze(group));
    if (group.length === 1) {
      owners.set(id, group[0]!);
      continue;
    }
    issues.push(issue(
      "E-OWNER-DUPLICATE",
      `owner[${JSON.stringify(id)}]`,
      `global ID ${JSON.stringify(id)} has ${group.length} claims (${group.map((claim) => claim.owner_kind).join(", ")})`,
    ));
  }

  const firstClassIds = new Set(claims.map((claim) => claim.id));
  const frozenAliases = new Map<string, readonly {
    namespace: RoadmapName;
    owner_kind: string;
    owner_id: string;
    logical_path: string;
  }[]>();
  for (const [alias, group] of [...aliases].sort(([left], [right]) => codePointSort(left, right))) {
    group.sort((left, right) =>
      codePointSort(left.namespace, right.namespace) || codePointSort(left.owner_kind, right.owner_kind) ||
      codePointSort(left.owner_id, right.owner_id) || codePointSort(left.logical_path, right.logical_path)
    );
    frozenAliases.set(alias, Object.freeze(group));
    if (group.length > 1 || firstClassIds.has(alias as RoadmapId)) {
      issues.push(issue(
        "E-ALIAS-COLLISION",
        `alias[${JSON.stringify(alias)}]`,
        group.length > 1
          ? `legacy alias ${JSON.stringify(alias)} has ${group.length} global owners`
          : `legacy alias ${JSON.stringify(alias)} collides with a first-class global ID`,
      ));
    }
  }

  return {
    owners,
    owner_claims: frozenGroups,
    aliases: frozenAliases,
    issues: Object.freeze(issues.sort((left, right) =>
      codePointSort(left.logical_path, right.logical_path) || codePointSort(left.code, right.code) ||
      codePointSort(left.message, right.message)
    )),
  };
}
