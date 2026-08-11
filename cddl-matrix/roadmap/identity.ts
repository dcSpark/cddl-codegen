import type { RoadmapIssue } from "./errors.ts";
import type { DocumentIdentityInputs, RoadmapIdProviderFact } from "./indexes.ts";
import { validateRoadmapId } from "./ids.ts";
import type { RoadmapId, RoadmapName } from "./model/core.ts";
import type {
  CurrentGuard,
  IdentityOwnerFact,
  RetiredIdV1,
  ShadowRecordClaim,
} from "./model/documents.ts";

const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

export type GlobalOwnerKind =
  | "first_class"
  | "legacy_markdown_reservation"
  | "shadow_record_reservation"
  | "current_guard"
  | "tombstone";

export interface GlobalOwnerClaim {
  readonly id: RoadmapId;
  readonly namespace: RoadmapName;
  readonly owner_kind: GlobalOwnerKind;
  readonly source: string;
  readonly logical_path: string;
  readonly value: RoadmapIdProviderFact | CurrentGuard | RetiredIdV1 | IdentityOwnerFact;
}

export interface GlobalIdentityValidationInputs {
  readonly documents: readonly DocumentIdentityInputs[];
  readonly current_guards?: readonly CurrentGuard[];
  readonly tombstones?: readonly RetiredIdV1[];
  /** C5 supplies reservation/shadow facts after it has validated their byte binding. */
  readonly additional_owners?: readonly IdentityOwnerFact[];
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
  code: Extract<RoadmapIssue["code"], "E-OWNER-DUPLICATE" | "E-ALIAS-COLLISION" | "E-RETIRED-REUSE" | `E-ID-${string}`>,
  logicalPath: string,
  message: string,
): RoadmapIssue {
  return { code, source: "<identity>", logical_path: logicalPath, message, exit: 1 };
}

const OWNER_RANK: Readonly<Record<GlobalOwnerKind, number>> = Object.freeze({
  first_class: 0,
  legacy_markdown_reservation: 1,
  shadow_record_reservation: 2,
  current_guard: 3,
  tombstone: 4,
});

function stableValueKey(value: unknown): string {
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

function shadowClaimsEqual(left: ShadowRecordClaim | undefined, right: ShadowRecordClaim): boolean {
  return left !== undefined && left.id === right.id && left.namespace === right.namespace &&
    left.source_path === right.source_path && left.logical_path === right.logical_path &&
    left.legacy_span_ids.length === right.legacy_span_ids.length &&
    left.legacy_span_ids.every((span, index) => span === right.legacy_span_ids[index]);
}

function coalescesReservationAndShadow(claims: readonly GlobalOwnerClaim[]): boolean {
  if (claims.length !== 2) return false;
  const reservation = claims.find((claim) => claim.owner_kind === "legacy_markdown_reservation");
  const shadow = claims.find((claim) => claim.owner_kind === "shadow_record_reservation");
  if (reservation === undefined || shadow === undefined) return false;
  const reservationFact = reservation.value as Extract<IdentityOwnerFact, { owner_kind: "legacy_markdown_reservation" }>;
  const shadowFact = shadow.value as Extract<IdentityOwnerFact, { owner_kind: "shadow_record_reservation" }>;
  return reservation.namespace === shadow.namespace &&
    shadowClaimsEqual(reservationFact.corroborating_shadow, shadowFact.claim);
}

function toAdditionalClaim(owner: IdentityOwnerFact): GlobalOwnerClaim {
  switch (owner.owner_kind) {
    case "active_record":
      return {
        id: owner.id,
        namespace: owner.namespace,
        owner_kind: "first_class",
        source: `roadmap:${owner.namespace}`,
        logical_path: `record[${JSON.stringify(owner.id)}]`,
        value: owner,
      };
    case "legacy_markdown_reservation":
      return {
        id: owner.id,
        namespace: owner.namespace,
        owner_kind: owner.owner_kind,
        source: owner.reservation.roadmap_path,
        logical_path: `legacy_markdown_reservation[${JSON.stringify(owner.id)}]`,
        value: owner,
      };
    case "shadow_record_reservation":
      return {
        id: owner.id,
        namespace: owner.namespace,
        owner_kind: owner.owner_kind,
        source: owner.claim.source_path,
        logical_path: owner.claim.logical_path,
        value: owner,
      };
    case "current_guard":
      return {
        id: owner.id,
        namespace: owner.namespace,
        owner_kind: owner.owner_kind,
        source: owner.guard.owner_registry,
        logical_path: `guard[${JSON.stringify(owner.id)}]`,
        value: owner,
      };
    case "tombstone":
      return {
        id: owner.id,
        namespace: owner.namespace,
        owner_kind: owner.owner_kind,
        source: "roadmap-retired-ids.toml",
        logical_path: `retired_ids.entry[${JSON.stringify(owner.id)}]`,
        value: owner,
      };
  }
}

/** Normalize the global first-class/guard/tombstone domain after document-local indexing. */
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
    claims.push({
      id: guard.id,
      namespace,
      owner_kind: "current_guard",
      source: guard.owner_registry,
      logical_path: `guard[${JSON.stringify(guard.id)}]`,
      value: guard,
    });
  }
  for (const tombstone of inputs.tombstones ?? []) {
    const namespace = namespaceOf(tombstone.id);
    if (namespace === undefined) {
      const result = validateRoadmapId(tombstone.id);
      if (!result.ok) issues.push(issue(result.code, `tombstone[${JSON.stringify(tombstone.id)}]`, result.message));
      continue;
    }
    claims.push({
      id: tombstone.id,
      namespace,
      owner_kind: "tombstone",
      source: "roadmap-retired-ids.toml",
      logical_path: `retired_ids.entry[${JSON.stringify(tombstone.id)}]`,
      value: tombstone,
    });
  }
  for (const owner of inputs.additional_owners ?? []) claims.push(toAdditionalClaim(owner));
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
    if (group.length === 1 || coalescesReservationAndShadow(group)) {
      owners.set(id, group.find((claim) => claim.owner_kind === "legacy_markdown_reservation") ?? group[0]!);
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

/** Focused lifecycle policy: a tombstoned permanent ID can never become first-class again. */
export function validateRetiredIdReuse(
  documents: readonly DocumentIdentityInputs[],
  tombstones: readonly RetiredIdV1[],
): readonly RoadmapIssue[] {
  const retired = new Set(tombstones.map((entry) => entry.id));
  const reused = new Map<RoadmapId, RoadmapIdProviderFact[]>();
  for (const document of documents) {
    for (const provider of document.id_providers) {
      if (!retired.has(provider.id)) continue;
      const group = reused.get(provider.id) ?? [];
      group.push(provider);
      reused.set(provider.id, group);
    }
  }
  const issues: RoadmapIssue[] = [];
  for (const [id, providers] of [...reused].sort(([left], [right]) => codePointSort(left, right))) {
    issues.push(issue(
      "E-RETIRED-REUSE",
      `retired[${JSON.stringify(id)}]`,
      `retired roadmap ID ${JSON.stringify(id)} is reused by ${providers.length} first-class provider(s)`,
    ));
  }
  return Object.freeze(issues);
}
