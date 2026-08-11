import type { RoadmapIssue } from "./errors.ts";
import type { DocumentIdentityInputs, RoadmapIdProviderFact } from "./indexes.ts";
import { validateRoadmapId } from "./ids.ts";
import type { RoadmapId, RoadmapName } from "./model/core.ts";
import type {
  CurrentGuard,
  IdentityOwnerFact,
  LegacyMarkdownReservationV1,
  RetiredIdV1,
  RoadmapDocumentV0,
  ShadowRecordClaim,
} from "./model/documents.ts";
import {
  createImmutableByteView,
  isImmutableByteView,
  type ImmutableByteViewInput,
} from "./render_ir.ts";

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
  /** Opaque capability minted only from mechanically revalidated campaign evidence. */
  readonly additional_owners?: CampaignIdentityOwnerCapability;
}

export type CampaignAdditionalOwnerFact = Extract<
  IdentityOwnerFact,
  { owner_kind: "legacy_markdown_reservation" | "shadow_record_reservation" }
>;

export type CampaignIdentityOwnerEvidence =
  | {
      readonly kind: "legacy_markdown_reservation";
      readonly reservation: LegacyMarkdownReservationV1;
      readonly markdown: ImmutableByteViewInput;
      readonly shadow_document?: RoadmapDocumentV0;
    }
  | {
      readonly kind: "shadow_record_reservation";
      readonly id: RoadmapId;
      readonly namespace: RoadmapName;
      readonly markdown: ImmutableByteViewInput;
      readonly shadow_document: RoadmapDocumentV0;
    };

declare const campaignIdentityOwnerCapabilityBrand: unique symbol;

/** Nominally and dynamically opaque; structural clones have no WeakMap provenance. */
export interface CampaignIdentityOwnerCapability {
  readonly [campaignIdentityOwnerCapabilityBrand]: true;
  readonly owner_count: number;
}

export type CampaignIdentityOwnerCapabilityResult =
  | {
      readonly ok: true;
      readonly capability: CampaignIdentityOwnerCapability;
      readonly owners: readonly CampaignAdditionalOwnerFact[];
      readonly issues: readonly [];
    }
  | {
      readonly ok: false;
      readonly owners: readonly [];
      readonly issues: readonly RoadmapIssue[];
    };

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

const LEGACY_ROADMAP_PATH: Readonly<Record<RoadmapName, LegacyMarkdownReservationV1["roadmap_path"]>> = {
  matrix: "cddl-matrix/ROADMAP.md",
  testing: "tests/TESTING_ROADMAP.md",
};

interface PrivateCampaignIdentityOwnerCapability {
  readonly evidence: readonly CampaignIdentityOwnerEvidence[];
  readonly evidence_signature: string;
  readonly owner_signature: string;
}

const campaignIdentityOwnerCapabilities = new WeakMap<object, PrivateCampaignIdentityOwnerCapability>();

function sha256(value: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(value).digest("hex");
}

function bytesEqual(left: Uint8Array, right: Uint8Array): boolean {
  if (left.byteLength !== right.byteLength) return false;
  for (let index = 0; index < left.byteLength; index += 1) {
    if (left[index] !== right[index]) return false;
  }
  return true;
}

function markdownHeadingTitle(source: Uint8Array): string | undefined {
  let text: string;
  try {
    text = new TextDecoder("utf-8", { fatal: true }).decode(source);
  } catch {
    return undefined;
  }
  const newline = text.indexOf("\n");
  const firstLine = text.slice(0, newline < 0 ? text.length : newline);
  return /^(#{1,6})[ \t]+(.+?)(?:[ \t]+#+)?[ \t]*$/.exec(firstLine)?.[2];
}

function ownerSignature(owners: readonly CampaignAdditionalOwnerFact[]): string {
  return stableValueKey(owners);
}

function exactShadowClaim(
  id: RoadmapId,
  namespace: RoadmapName,
  markdownInput: ImmutableByteViewInput,
  document: RoadmapDocumentV0,
  reservation?: LegacyMarkdownReservationV1,
): ShadowRecordClaim | undefined {
  const markdown = createImmutableByteView(markdownInput);
  if (
    namespaceOf(id) !== namespace || document.document.schema_version !== 0 ||
    document.document.authority !== "shadow" || document.document.roadmap !== namespace ||
    document.document.projection_path !== LEGACY_ROADMAP_PATH[namespace] ||
    document.document.frozen_source_sha256 !== markdown.wholeSha256() ||
    document.document.frozen_source_byte_length !== markdown.byte_length
  ) return undefined;
  const records = document.records.filter((record) => record.id === id);
  const manifestRows = document.manifest.filter((entry) =>
    entry.kind === "record" && entry.record_id === id
  );
  if (records.length !== 1 || manifestRows.length !== 1) return undefined;
  const record = records[0]!;
  if (new Set(record.span_ids).size !== record.span_ids.length) return undefined;
  const spans = record.span_ids.map((spanId) => document.spans.find((span) => span.id === spanId));
  if (spans.length === 0 || spans.some((span) => span === undefined)) return undefined;
  const ordered = spans.map((span) => span!).sort((left, right) => left.start_byte - right.start_byte);
  const allOwnerSpans = document.spans.filter((span) =>
    span.source_kind === "record" && span.owner_id === id && span.owner_field === "source_block_md"
  ).map((span) => span.id).sort(codePointSort);
  const claimedSpans = [...record.span_ids].sort(codePointSort);
  const start = reservation?.source_start_byte ?? ordered[0]!.start_byte;
  const end = reservation?.source_end_byte ?? ordered.at(-1)!.end_byte;
  if (
    ordered[0]!.start_byte !== start || ordered.at(-1)!.end_byte !== end ||
    ordered.some((span, index) => index > 0 && ordered[index - 1]!.end_byte !== span.start_byte) ||
    ordered.some((span) => span.source_kind !== "record" || span.owner_id !== id ||
      span.owner_field !== "source_block_md" || span.migration_status !== "raw" ||
      span.start_byte < 0 || span.end_byte <= span.start_byte || span.end_byte > markdown.byte_length ||
      sha256(markdown.sliceBytes(span.start_byte, span.end_byte)) !== span.sha256) ||
    allOwnerSpans.length !== claimedSpans.length ||
    allOwnerSpans.some((spanId, index) => claimedSpans[index] !== spanId) ||
    !bytesEqual(record.source_block_md, markdown.sliceBytes(start, end)) ||
    markdownHeadingTitle(record.source_block_md) !== record.title ||
    (reservation !== undefined && (
      reservation.id !== id || reservation.roadmap_path !== LEGACY_ROADMAP_PATH[namespace] ||
      reservation.source_title !== record.title || reservation.source_sha256 !== sha256(record.source_block_md)
    ))
  ) return undefined;
  return Object.freeze({
    id,
    namespace,
    source_path: document.document.source_path,
    logical_path: `record[${JSON.stringify(id)}]`,
    legacy_span_ids: Object.freeze(record.span_ids.slice()),
  });
}

function deriveCampaignIdentityOwners(
  evidence: readonly CampaignIdentityOwnerEvidence[],
): { readonly owners: readonly CampaignAdditionalOwnerFact[]; readonly issues: readonly RoadmapIssue[] } {
  const owners: CampaignAdditionalOwnerFact[] = [];
  const issues: RoadmapIssue[] = [];
  for (const [index, input] of evidence.entries()) {
    const logicalPath = `additional_owners[${index}]`;
    if (input.kind === "legacy_markdown_reservation") {
      const reservation = input.reservation;
      const markdown = createImmutableByteView(input.markdown);
      const namespace = namespaceOf(reservation.id);
      const start = reservation.source_start_byte;
      const end = reservation.source_end_byte;
      const source = start >= 0 && end > start && end <= markdown.byte_length
        ? markdown.sliceBytes(start, end)
        : undefined;
      const pairedShadow = namespace === undefined ? undefined : evidence.find((candidate) =>
        candidate.kind === "shadow_record_reservation" && candidate.id === reservation.id &&
        candidate.namespace === namespace &&
        createImmutableByteView(candidate.markdown).bytesEqual(markdown)
      );
      const shadowDocument = input.shadow_document ?? pairedShadow?.shadow_document;
      const shadow = namespace === undefined || shadowDocument === undefined
        ? undefined
        : exactShadowClaim(reservation.id, namespace, input.markdown, shadowDocument, reservation);
      if (
        namespace === undefined || reservation.roadmap_path !== LEGACY_ROADMAP_PATH[namespace] ||
        source === undefined || reservation.whole_source_sha256 !== markdown.wholeSha256() ||
        reservation.source_sha256 !== sha256(source) ||
        markdownHeadingTitle(source) !== reservation.source_title ||
        (shadowDocument !== undefined && shadow === undefined)
      ) {
        issues.push(issue(
          "E-OWNER-DUPLICATE",
          logicalPath,
          "campaign reservation evidence does not exactly bind its namespace/path/source bytes/title or shadow owner/index/span facts",
        ));
        continue;
      }
      const reservationOwner = Object.freeze({
        owner_kind: "legacy_markdown_reservation" as const,
        id: reservation.id,
        namespace,
        work_kind: reservation.work_kind,
        reservation,
        ...(shadow === undefined ? {} : { corroborating_shadow: shadow }),
      });
      owners.push(reservationOwner);
      if (shadow !== undefined && input.shadow_document !== undefined) owners.push(Object.freeze({
        owner_kind: "shadow_record_reservation" as const,
        id: reservation.id,
        namespace,
        claim: shadow,
      }));
      continue;
    }
    const claim = exactShadowClaim(
      input.id,
      input.namespace,
      input.markdown,
      input.shadow_document,
    );
    if (claim === undefined) {
      issues.push(issue(
        "E-OWNER-DUPLICATE",
        logicalPath,
        "campaign shadow evidence does not exactly bind its namespace/path/source bytes/title or document/index/span facts",
      ));
      continue;
    }
    owners.push(Object.freeze({
      owner_kind: "shadow_record_reservation" as const,
      id: input.id,
      namespace: input.namespace,
      claim,
    }));
  }
  owners.sort((left, right) => codePointSort(left.id, right.id) ||
    codePointSort(left.owner_kind, right.owner_kind));
  return {
    owners: Object.freeze(owners),
    issues: Object.freeze(issues),
  };
}

/**
 * Validate raw campaign evidence and mint its opaque identity capability. This is intentionally the
 * only minting seam: it accepts no prevalidated owner facts or caller-asserted success flag.
 */
export function validateCampaignIdentityOwnerEvidence(
  evidence: readonly CampaignIdentityOwnerEvidence[],
): CampaignIdentityOwnerCapabilityResult {
  const derived = deriveCampaignIdentityOwners(evidence);
  if (derived.issues.length > 0) {
    return Object.freeze({
      ok: false,
      owners: Object.freeze([]) as readonly [],
      issues: derived.issues,
    });
  }
  const capability = Object.freeze({
    owner_count: derived.owners.length,
  }) as CampaignIdentityOwnerCapability;
  campaignIdentityOwnerCapabilities.set(capability, {
    evidence: Object.freeze(evidence.slice()),
    evidence_signature: stableValueKey(evidence),
    owner_signature: ownerSignature(derived.owners),
  });
  return Object.freeze({
    ok: true,
    capability,
    owners: derived.owners,
    issues: Object.freeze([]) as readonly [],
  });
}

function ownersFromCampaignCapability(
  capability: CampaignIdentityOwnerCapability,
): readonly CampaignAdditionalOwnerFact[] | undefined {
  const stored = campaignIdentityOwnerCapabilities.get(capability);
  if (stored === undefined) return undefined;
  const derived = deriveCampaignIdentityOwners(stored.evidence);
  return derived.issues.length === 0 && capability.owner_count === derived.owners.length &&
      stored.evidence_signature === stableValueKey(stored.evidence) &&
      stored.owner_signature === ownerSignature(derived.owners)
    ? derived.owners
    : undefined;
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
  if (inputs.additional_owners !== undefined) {
    const additionalOwners = ownersFromCampaignCapability(inputs.additional_owners);
    if (additionalOwners === undefined) {
      issues.push(issue(
        "E-OWNER-DUPLICATE",
        "additional_owners",
        "additional owners require one intact opaque campaign identity capability",
      ));
    } else {
      for (const owner of additionalOwners) claims.push(toAdditionalClaim(owner));
    }
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
