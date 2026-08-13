import type { RoadmapIssue } from "./errors.ts";
import {
  validateCampaignIdentityOwnerEvidence,
  type CampaignAdditionalOwnerFact,
  type CampaignIdentityOwnerCapability,
  type CampaignIdentityOwnerEvidence,
  type GlobalIdentityResult,
} from "./identity.ts";
import type { FullCommitId, RepoPath, RoadmapId, RoadmapName, WorkKind } from "./model/core.ts";
import type {
  CampaignDocumentV1,
  CampaignSelectionV1,
  LegacyMarkdownReservationV1,
  RoadmapAuthorityState,
  RoadmapDocument,
  RoadmapDocumentV0,
  RecordNode,
  SemanticPayload,
  ShadowRecordClaim,
} from "./model/documents.ts";
import {
  campaignAuthorityRank,
  validateCampaignAuthorityTuple,
} from "./campaign_authority.ts";
import {
  createImmutableByteView,
  type ImmutableByteView,
  type ImmutableByteViewInput,
} from "./render_ir.ts";
import { markdownHeadingTitle, shadowRecordSourceTitle } from "./shadow_title.ts";

export { createImmutableByteView };
export type ByteView = ImmutableByteView;
export type ByteViewInput = ImmutableByteViewInput;

const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

const ROADMAP_PATH: Readonly<Record<RoadmapName, LegacyMarkdownReservationV1["roadmap_path"]>> = {
  matrix: "cddl-matrix/ROADMAP.md",
  testing: "tests/TESTING_ROADMAP.md",
};

export interface CampaignRoadmapSnapshot<Markdown extends ImmutableByteViewInput = Uint8Array> {
  readonly markdown: Markdown;
  readonly document?: RoadmapDocument;
}

export type LifecycleRoadmapSnapshot = CampaignRoadmapSnapshot<ImmutableByteViewInput>;

export interface CrossRoadmapAllowlistEntry {
  readonly source: RoadmapId;
  readonly target: RoadmapId;
  readonly expires_when: "testing_authoritative";
}

export interface CrossRoadmapUnresolvedFact {
  readonly source: RoadmapId;
  readonly target: RoadmapId;
}

/** Opaque reviewed-title capability minted from the exact reservation-bound Markdown heading. */
export interface LegacyTitleBindingFact {
  readonly id: RoadmapId;
  readonly roadmap_path: LegacyMarkdownReservationV1["roadmap_path"];
  readonly source_title: string;
  readonly source_start_byte: number;
  readonly source_end_byte: number;
}

export interface CampaignValidationInputs {
  readonly campaign: CampaignDocumentV1;
  readonly roadmaps: Readonly<Record<RoadmapName, LifecycleRoadmapSnapshot>>;
  /** The final global identity view, assembled with `owners` below. */
  readonly identity?: GlobalIdentityResult;
  readonly legacy_title_bindings?: readonly LegacyTitleBindingFact[];
  readonly unresolved_cross_roadmap?: readonly CrossRoadmapUnresolvedFact[];
  readonly cross_roadmap_allowlist?: readonly CrossRoadmapAllowlistEntry[];
}

export interface ValidatedCampaignIdentityOwners {
  readonly owners: readonly CampaignAdditionalOwnerFact[];
  readonly issues: readonly RoadmapIssue[];
}

export interface CampaignValidationResult extends ValidatedCampaignIdentityOwners {
  readonly selections: ReadonlyMap<RoadmapId, CampaignSelectionV1>;
  readonly reservations: ReadonlyMap<RoadmapId, LegacyMarkdownReservationV1>;
  readonly fired_promotions: readonly RoadmapId[];
}

const validatedCampaignResults = new WeakMap<object, {
  readonly signature: string;
  readonly capability: CampaignIdentityOwnerCapability;
}>();
const validatedLegacyTitleBindings = new WeakMap<object, {
  readonly reservation: LegacyMarkdownReservationV1;
  readonly markdown: ImmutableByteViewInput;
  readonly signature: string;
}>();

function campaignOwnerSignature(owner: CampaignAdditionalOwnerFact): string {
  if (owner.owner_kind === "legacy_markdown_reservation") {
    const reservation = owner.reservation;
    const shadow = owner.corroborating_shadow;
    return JSON.stringify([
      owner.owner_kind, owner.id, owner.namespace, owner.work_kind,
      reservation.id, reservation.work_kind, reservation.roadmap_path, reservation.source_title,
      reservation.source_start_byte, reservation.source_end_byte, reservation.source_sha256,
      reservation.whole_source_sha256,
      shadow === undefined ? null : [
        shadow.id, shadow.namespace, shadow.source_path, shadow.logical_path, shadow.legacy_span_ids,
      ],
    ]);
  }
  return JSON.stringify([
    owner.owner_kind, owner.id, owner.namespace, owner.claim.id, owner.claim.namespace,
    owner.claim.source_path, owner.claim.logical_path, owner.claim.legacy_span_ids,
  ]);
}

function campaignResultSignature(result: ValidatedCampaignIdentityOwners): string {
  return JSON.stringify(result.owners.map(campaignOwnerSignature));
}

/** Return campaign-owned identity capabilities only for the exact validated result object. */
export function campaignIdentityOwners(
  result: ValidatedCampaignIdentityOwners,
): CampaignIdentityOwnerCapability | undefined {
  const validated = validatedCampaignResults.get(result);
  return validated?.signature === campaignResultSignature(result)
    ? validated.capability
    : undefined;
}

function issue(
  code: Extract<RoadmapIssue["code"], `E-CAMPAIGN-${string}` | "E-SCHEMA-STATE" | "E-SOURCE-MISSING">,
  logicalPath: string,
  message: string,
  source = "roadmap-campaign.toml",
): RoadmapIssue {
  return { code, source, logical_path: logicalPath, message, exit: 1 };
}

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

function titleBindingSignature(binding: LegacyTitleBindingFact): string {
  return JSON.stringify([
    binding.id,
    binding.roadmap_path,
    binding.source_title,
    binding.source_start_byte,
    binding.source_end_byte,
  ]);
}

/**
 * Mint a reviewed-title capability only from the reservation's exact immutable Markdown binding.
 * A structurally equal caller-created object is intentionally inert.
 */
export function validateLegacyTitleBinding(
  reservation: LegacyMarkdownReservationV1,
  markdown: ImmutableByteViewInput,
): LegacyTitleBindingFact | undefined {
  const view = createImmutableByteView(markdown);
  const start = reservation.source_start_byte;
  const end = reservation.source_end_byte;
  if (start < 0 || end <= start || end > view.byte_length ||
    view.wholeSha256() !== reservation.whole_source_sha256) return undefined;
  const source = view.sliceBytes(start, end);
  if (sha256(source) !== reservation.source_sha256 ||
    markdownHeadingTitle(source) !== reservation.source_title) return undefined;
  const binding: LegacyTitleBindingFact = Object.freeze({
    id: reservation.id,
    roadmap_path: reservation.roadmap_path,
    source_title: reservation.source_title,
    source_start_byte: start,
    source_end_byte: end,
  });
  validatedLegacyTitleBindings.set(binding, {
    reservation,
    markdown,
    signature: titleBindingSignature(binding),
  });
  return binding;
}

function isValidatedLegacyTitleBinding(
  binding: LegacyTitleBindingFact,
  reservation: LegacyMarkdownReservationV1,
  markdown: ImmutableByteViewInput,
): boolean {
  const validated = validatedLegacyTitleBindings.get(binding);
  return validated?.reservation === reservation && validated.markdown === markdown &&
    validated.signature === titleBindingSignature(binding);
}

function namespaceFor(id: RoadmapId): RoadmapName {
  return id.startsWith("matrix.") ? "matrix" : "testing";
}

export function campaignAuthority(
  campaign: CampaignDocumentV1,
  roadmap: RoadmapName,
): RoadmapAuthorityState {
  return roadmap === "matrix" ? campaign.campaign.matrix_authority : campaign.campaign.testing_authority;
}

export function workKindOfRecord(
  document: RoadmapDocument | undefined,
  id: RoadmapId,
): WorkKind | undefined {
  if (document === undefined || document.document.schema_version === 0) return undefined;
  const record = document.records.find((value) => value.id === id);
  if (record === undefined) return undefined;
  const payload = payloadOfRecord(record);
  return payload?.kind === "work" ? payload.work_kind : undefined;
}

function payloadOfRecord(record: RecordNode): SemanticPayload | undefined {
  if ("payload" in record) return record.payload;
  return "semantic_shadow" in record ? record.semantic_shadow : undefined;
}

function exactShadowClaim(
  id: RoadmapId,
  namespace: RoadmapName,
  snapshot: LifecycleRoadmapSnapshot,
  reservation?: LegacyMarkdownReservationV1,
): ShadowRecordClaim | undefined {
  const markdown = createImmutableByteView(snapshot.markdown);
  const document = snapshot.document;
  if (document === undefined || document.document.schema_version !== 0) return undefined;
  const matches = document.records.filter((record) => record.id === id);
  const manifestMatches = document.manifest.filter((entry) =>
    entry.kind === "record" && entry.record_id === id
  );
  if (matches.length !== 1 || manifestMatches.length !== 1) return undefined;
  const record = matches[0]!;
  if (!("source_block_md" in record) || !("span_ids" in record)) return undefined;
  if (new Set(record.span_ids).size !== record.span_ids.length) return undefined;
  const spans = record.span_ids.map((spanId) => document.spans.find((span) => span.id === spanId));
  if (spans.some((span) => span === undefined)) return undefined;
  const ordered = spans.map((span) => span!).sort((left, right) => left.start_byte - right.start_byte);
  const allOwnerSpans = document.spans.filter((span) =>
    span.source_kind === "record" && span.owner_id === id && span.owner_field === "source_block_md"
  ).map((span) => span.id).sort(codePointSort);
  const claimedSpans = [...record.span_ids].sort(codePointSort);
  const start = reservation?.source_start_byte ?? ordered[0]?.start_byte;
  const end = reservation?.source_end_byte ?? ordered.at(-1)?.end_byte;
  if (
    ordered.length === 0 || start === undefined || end === undefined || ordered[0]!.start_byte !== start ||
    ordered.at(-1)!.end_byte !== end ||
    ordered.some((span, index) => index > 0 && ordered[index - 1]!.end_byte !== span.start_byte) ||
    ordered.some((span) => span.source_kind !== "record" || span.owner_id !== id ||
      span.owner_field !== "source_block_md" || span.migration_status !== "raw" ||
      span.start_byte < 0 || span.end_byte <= span.start_byte || span.end_byte > markdown.byte_length ||
      sha256(markdown.sliceBytes(span.start_byte, span.end_byte)) !== span.sha256) ||
    allOwnerSpans.length !== claimedSpans.length ||
    allOwnerSpans.some((spanId, index) => claimedSpans[index] !== spanId) ||
    !bytesEqual(record.source_block_md, markdown.sliceBytes(start, end)) ||
    shadowRecordSourceTitle(record.source_block_md, namespace) !== record.title ||
    (reservation !== undefined && (
      sha256(record.source_block_md) !== reservation.source_sha256 ||
      record.title !== reservation.source_title
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

function validateAuthorityState(
  campaign: CampaignDocumentV1,
  roadmap: RoadmapName,
  snapshot: LifecycleRoadmapSnapshot,
  issues: RoadmapIssue[],
): void {
  const state = campaignAuthority(campaign, roadmap);
  const markdown = createImmutableByteView(snapshot.markdown);
  const document = snapshot.document;
  const path = `campaign.${roadmap}_authority`;
  if (state === "legacy_markdown") {
    if (document !== undefined) issues.push(issue("E-SCHEMA-STATE", path, `${roadmap} legacy Markdown authority forbids a roadmap TOML document`));
    return;
  }
  if (document === undefined) {
    issues.push(issue("E-SOURCE-MISSING", path, `${roadmap} ${state} authority requires its roadmap TOML document`));
    return;
  }
  const versionValid = state === "shadow"
    ? document.document.schema_version === 0
    : document.document.schema_version === 1 || document.document.schema_version === 2;
  if (!versionValid) {
    issues.push(issue("E-SCHEMA-STATE", path, `${roadmap} ${state} authority requires an applicable authoritative roadmap schema`));
  }
  if (state === "shadow" && document.document.schema_version === 0 &&
    (document.document.authority !== "shadow" ||
      document.document.frozen_source_sha256 !== markdown.wholeSha256() ||
      document.document.frozen_source_byte_length !== markdown.byte_length)) {
    issues.push(issue("E-SCHEMA-STATE", path, `${roadmap} shadow metadata must exactly bind the immutable Markdown snapshot`));
  }
}

function validateReservationBinding(
  reservation: LegacyMarkdownReservationV1,
  campaign: CampaignDocumentV1,
  snapshot: LifecycleRoadmapSnapshot,
  titleBindings: readonly LegacyTitleBindingFact[],
  logicalPath: string,
  issues: RoadmapIssue[],
): { readonly valid: boolean; readonly shadow?: ShadowRecordClaim } {
  const namespace = namespaceFor(reservation.id);
  const state = campaignAuthority(campaign, namespace);
  if (reservation.roadmap_path !== ROADMAP_PATH[namespace]) {
    issues.push(issue("E-CAMPAIGN-TARGET", logicalPath, `reservation path does not match the ${namespace} namespace`));
    return { valid: false };
  }
  if (state === "authoritative") {
    issues.push(issue("E-CAMPAIGN-TARGET-EXPIRED", logicalPath, `${namespace} reservations expire at authoritative v1`));
    return { valid: false };
  }
  const markdown = createImmutableByteView(snapshot.markdown);
  const { source_start_byte: start, source_end_byte: end } = reservation;
  const source = start >= 0 && end > start && end <= markdown.byte_length
    ? markdown.sliceBytes(start, end)
    : undefined;
  const titleMatches = titleBindings.filter((fact) =>
    fact.id === reservation.id && fact.roadmap_path === reservation.roadmap_path &&
    fact.source_title === reservation.source_title &&
    fact.source_start_byte === reservation.source_start_byte &&
    fact.source_end_byte === reservation.source_end_byte &&
    isValidatedLegacyTitleBinding(fact, reservation, snapshot.markdown)
  );
  if (
    source === undefined || sha256(source) !== reservation.source_sha256 ||
    markdown.wholeSha256() !== reservation.whole_source_sha256 || titleMatches.length !== 1
  ) {
    issues.push(issue(
      "E-CAMPAIGN-TARGET",
      logicalPath,
      "legacy reservation title/range/span digest/whole-source digest does not bind one immutable Markdown snapshot",
    ));
    return { valid: false };
  }
  if (state !== "shadow") return { valid: true };
  const claim = exactShadowClaim(reservation.id, namespace, snapshot, reservation);
  if (claim === undefined) {
    issues.push(issue("E-CAMPAIGN-TARGET", logicalPath, "shadow reservation lacks one exact same-ID reviewed span binding"));
    return { valid: false };
  }
  return { valid: true, shadow: claim };
}

interface ActiveWorkFact {
  readonly work_kind: WorkKind;
  readonly admission_ids: readonly RoadmapId[];
}

function activeWorkIds(inputs: CampaignValidationInputs): ReadonlyMap<RoadmapId, ActiveWorkFact> {
  const result = new Map<RoadmapId, ActiveWorkFact>();
  for (const roadmap of ["matrix", "testing"] as const) {
    if (campaignAuthority(inputs.campaign, roadmap) !== "authoritative") continue;
    const document = inputs.roadmaps[roadmap].document;
    if (document?.document.schema_version === 0 || document === undefined) continue;
    for (const record of document.records) {
      const payload = payloadOfRecord(record);
      if (payload?.kind === "work") result.set(record.id, {
        work_kind: payload.work_kind,
        admission_ids: payload.admission_ids ?? [],
      });
    }
  }
  return result;
}

function activeTestingSystemAdmissions(inputs: CampaignValidationInputs): ReadonlySet<RoadmapId> {
  const result = new Set<RoadmapId>();
  for (const roadmap of ["matrix", "testing"] as const) {
    if (campaignAuthority(inputs.campaign, roadmap) !== "authoritative") continue;
    const document = inputs.roadmaps[roadmap].document;
    if (document === undefined || document.document.schema_version === 0) continue;
    for (const record of document.records) {
      if (payloadOfRecord(record)?.kind === "testing_system_admission") result.add(record.id);
    }
  }
  return result;
}

function firedPromotionIds(inputs: CampaignValidationInputs): readonly RoadmapId[] {
  const fired = new Set<RoadmapId>();
  for (const roadmap of ["matrix", "testing"] as const) {
    const document = inputs.roadmaps[roadmap].document;
    if (document?.document.schema_version === 0 || document === undefined) continue;
    const firedSignals = new Set(document.records.filter((record) => {
      const payload = payloadOfRecord(record);
      return payload?.kind === "signal" && payload.transition_kind === "promotion_trigger" &&
        payload.evaluation === "met";
    }).map((record) => record.id));
    for (const record of document.records) {
      const payload = payloadOfRecord(record);
      if (payload?.kind === "work" && payload.work_state === "armed" &&
        payload.transition_ids.some((id) => firedSignals.has(id))) fired.add(record.id);
    }
  }
  return Object.freeze([...fired].sort(codePointSort));
}

function allowlistKey(value: CrossRoadmapUnresolvedFact | CrossRoadmapAllowlistEntry): string {
  return JSON.stringify([value.source, value.target]);
}

function validateAllowlist(inputs: CampaignValidationInputs, issues: RoadmapIssue[]): void {
  const unresolved = [...inputs.unresolved_cross_roadmap ?? []].sort((left, right) =>
    codePointSort(allowlistKey(left), allowlistKey(right))
  );
  const allowlist = [...inputs.cross_roadmap_allowlist ?? []].sort((left, right) =>
    codePointSort(allowlistKey(left), allowlistKey(right))
  );
  const unresolvedKeys = unresolved.map(allowlistKey);
  const allowlistKeys = allowlist.map(allowlistKey);
  const duplicate = allowlistKeys.find((key, index) => index > 0 && allowlistKeys[index - 1] === key);
  if (duplicate !== undefined) {
    issues.push(issue("E-CAMPAIGN-DUPLICATE", "cross_roadmap_allowlist", `duplicate allowlist tuple ${duplicate}`));
  }
  if (campaignAuthority(inputs.campaign, "testing") === "authoritative" && allowlist.length > 0) {
    issues.push(issue("E-CAMPAIGN-TARGET-EXPIRED", "cross_roadmap_allowlist", "temporary cross-roadmap allowlist expires at testing authority cutover"));
  }
  const exact = unresolvedKeys.length === allowlistKeys.length &&
    unresolvedKeys.every((key, index) => key === allowlistKeys[index]);
  if (!exact) {
    issues.push(issue("E-CAMPAIGN-TARGET", "cross_roadmap_allowlist", "allowlist must exhaustively equal the typed unresolved cross-roadmap tuple set"));
  }
}

/** Validate one decoded campaign against immutable decoded roadmap/Markdown snapshots. */
export function validateCampaign(inputs: CampaignValidationInputs): CampaignValidationResult {
  const authorityTupleIssues = validateCampaignAuthorityTuple(inputs.campaign.campaign);
  if (authorityTupleIssues.length > 0) {
    return Object.freeze({
      owners: Object.freeze([]),
      selections: new Map(),
      reservations: new Map(),
      fired_promotions: Object.freeze([]),
      issues: authorityTupleIssues,
    });
  }
  const issues: RoadmapIssue[] = [];
  validateAuthorityState(inputs.campaign, "matrix", inputs.roadmaps.matrix, issues);
  validateAuthorityState(inputs.campaign, "testing", inputs.roadmaps.testing, issues);
  const reservations = new Map<RoadmapId, LegacyMarkdownReservationV1>();
  const seenReservations = new Set<RoadmapId>();
  const ownerEvidence: CampaignIdentityOwnerEvidence[] = [];
  const sortedReservations = [...inputs.campaign.legacy_markdown_reservations].sort((left, right) =>
    codePointSort(left.id, right.id)
  );
  for (const [index, reservation] of sortedReservations.entries()) {
    const logicalPath = `legacy_markdown_reservation[${index}]`;
    if (seenReservations.has(reservation.id)) {
      issues.push(issue("E-CAMPAIGN-DUPLICATE", logicalPath, `duplicate reservation ${JSON.stringify(reservation.id)}`));
      continue;
    }
    seenReservations.add(reservation.id);
    const namespace = namespaceFor(reservation.id);
    const binding = validateReservationBinding(
      reservation,
      inputs.campaign,
      inputs.roadmaps[namespace],
      inputs.legacy_title_bindings ?? [],
      logicalPath,
      issues,
    );
    if (!binding.valid) continue;
    reservations.set(reservation.id, reservation);
    ownerEvidence.push({
      kind: "legacy_markdown_reservation",
      reservation,
      markdown: inputs.roadmaps[namespace].markdown,
      ...(binding.shadow === undefined
        ? {}
        : { shadow_document: inputs.roadmaps[namespace].document as RoadmapDocumentV0 }),
    });
  }
  for (const roadmap of ["matrix", "testing"] as const) {
    const document = inputs.roadmaps[roadmap].document;
    if (document?.document.schema_version !== 0) continue;
    for (const record of document.records) {
      if (reservations.has(record.id)) continue;
      const claim = exactShadowClaim(record.id, roadmap, inputs.roadmaps[roadmap]);
      if (claim === undefined) {
        issues.push(issue(
          "E-CAMPAIGN-TARGET",
          `record[${JSON.stringify(record.id)}]`,
          "shadow-only declaration lacks one exact owner/range/digest source-span binding",
          document.document.source_path,
        ));
        continue;
      }
      ownerEvidence.push({
        kind: "shadow_record_reservation",
        id: record.id,
        namespace: roadmap,
        markdown: inputs.roadmaps[roadmap].markdown,
        shadow_document: document as RoadmapDocumentV0,
      });
    }
  }
  const ownerValidation = validateCampaignIdentityOwnerEvidence(ownerEvidence);
  if (!ownerValidation.ok) issues.push(...ownerValidation.issues);
  const owners = ownerValidation.owners;

  const active = activeWorkIds(inputs);
  const admissions = activeTestingSystemAdmissions(inputs);
  const selections = new Map<RoadmapId, CampaignSelectionV1>();
  const sortedSelections = [...inputs.campaign.selections].sort((left, right) =>
    codePointSort(left.item_id, right.item_id)
  );
  for (const [index, selection] of sortedSelections.entries()) {
    const path = `selection[${index}]`;
    if (selections.has(selection.item_id)) {
      issues.push(issue("E-CAMPAIGN-DUPLICATE", path, `duplicate campaign selection ${JSON.stringify(selection.item_id)}`));
      continue;
    }
    selections.set(selection.item_id, selection);
    if (selection.cycle === "burndown-four" && selection.cost_bound === undefined) {
      issues.push(issue(
        "E-CAMPAIGN-STATE",
        `${path}.cost_bound`,
        "burndown-four selection requires one reviewed structured cost bound",
      ));
    }
    const reservation = reservations.get(selection.item_id);
    if (selection.target_kind === "active_id") {
      if (!active.has(selection.item_id) || reservation !== undefined) {
        issues.push(issue("E-CAMPAIGN-TARGET", path, "active_id must resolve to one authoritative-v1 work record and no reservation"));
      }
      const work = active.get(selection.item_id);
      if (work?.work_kind === "missing_system" &&
        !work.admission_ids.some((id) => admissions.has(id))) {
        issues.push(issue(
          "E-CAMPAIGN-TARGET",
          path,
          "selected missing_system work requires at least one qualifying testing-system admission",
        ));
      }
    } else if (reservation === undefined) {
      issues.push(issue("E-CAMPAIGN-TARGET", path, "legacy target must resolve to one same-ID standalone reservation"));
    }
    if (selection.selected_state === "selected") {
      if (selection.pickup_commit !== undefined) {
        issues.push(issue("E-CAMPAIGN-STATE", path, "selected state forbids pickup_commit"));
      }
    } else if (selection.assignee === undefined || selection.pickup_commit === undefined) {
      issues.push(issue("E-CAMPAIGN-STATE", path, "in_progress requires assignee and pickup_commit"));
    }
  }

  if (inputs.identity !== undefined) {
    for (const selection of selections.values()) {
      const owner = inputs.identity.owners.get(selection.item_id);
      const expected = selection.target_kind === "active_id" ? "first_class" : "legacy_markdown_reservation";
      if (owner?.owner_kind !== expected) {
        issues.push(issue("E-CAMPAIGN-TARGET", `selection[${JSON.stringify(selection.item_id)}]`, `selection target kind does not match normalized global owner ${owner?.owner_kind ?? "missing"}`));
      }
    }
  }

  const fired = firedPromotionIds(inputs);
  for (const id of fired) {
    if (!selections.has(id)) {
      issues.push(issue("E-CAMPAIGN-FIRED-HIDDEN", `selection[${JSON.stringify(id)}]`, `fired promotion for ${id} is hidden from the actionable campaign`));
    }
  }
  validateAllowlist(inputs, issues);
  issues.sort((left, right) => codePointSort(left.logical_path, right.logical_path) ||
    codePointSort(left.code, right.code) || codePointSort(left.message, right.message));
  const result: CampaignValidationResult = Object.freeze({
    owners: Object.freeze(owners),
    selections,
    reservations,
    fired_promotions: fired,
    issues: Object.freeze(issues),
  });
  if (ownerValidation.ok && issues.length === 0) {
    validatedCampaignResults.set(result, {
      signature: campaignResultSignature(result),
      capability: ownerValidation.capability,
    });
  }
  return result;
}

/** Validate the pre-root bootstrap's v0 shadow IDs without treating them as active records. */
export function validateBootstrapShadowOwners(
  roadmaps: Readonly<Record<RoadmapName, LifecycleRoadmapSnapshot>>,
): ValidatedCampaignIdentityOwners {
  const ownerEvidence: CampaignIdentityOwnerEvidence[] = [];
  const issues: RoadmapIssue[] = [];
  for (const roadmap of ["matrix", "testing"] as const) {
    const snapshot = roadmaps[roadmap];
    const markdown = createImmutableByteView(snapshot.markdown);
    const document = snapshot.document;
    if (document === undefined) continue;
    if (document.document.schema_version !== 0 || document.document.authority !== "shadow" ||
      document.document.frozen_source_sha256 !== markdown.wholeSha256() ||
      document.document.frozen_source_byte_length !== markdown.byte_length) {
      issues.push(issue("E-SCHEMA-STATE", `bootstrap.${roadmap}`, "bootstrap roadmap must be one exact v0 shadow snapshot"));
      continue;
    }
    for (const record of document.records) {
      const claim = exactShadowClaim(record.id, roadmap, snapshot);
      if (claim === undefined) {
        issues.push(issue(
          "E-CAMPAIGN-TARGET",
          `bootstrap.${roadmap}.record[${JSON.stringify(record.id)}]`,
          "bootstrap shadow record lacks one exact owner/range/digest source-span binding",
          document.document.source_path,
        ));
        continue;
      }
      ownerEvidence.push({
        kind: "shadow_record_reservation",
        id: record.id,
        namespace: roadmap,
        markdown: snapshot.markdown,
        shadow_document: document as RoadmapDocumentV0,
      });
    }
  }
  const ownerValidation = validateCampaignIdentityOwnerEvidence(ownerEvidence);
  if (!ownerValidation.ok) issues.push(...ownerValidation.issues);
  issues.sort((left, right) => codePointSort(left.source, right.source) ||
    codePointSort(left.logical_path, right.logical_path) || codePointSort(left.code, right.code));
  const result: ValidatedCampaignIdentityOwners = Object.freeze({
    owners: ownerValidation.owners,
    issues: Object.freeze(issues),
  });
  if (ownerValidation.ok && issues.length === 0) {
    validatedCampaignResults.set(result, {
      signature: campaignResultSignature(result),
      capability: ownerValidation.capability,
    });
  }
  return result;
}

export interface CampaignTransitionInputs {
  readonly base: CampaignValidationResult;
  readonly candidate: CampaignValidationResult;
  readonly base_document: CampaignDocumentV1;
  readonly candidate_document: CampaignDocumentV1;
  readonly against: FullCommitId;
}

function campaignCostBoundsEqual(
  left: NonNullable<CampaignSelectionV1["cost_bound"]>,
  right: NonNullable<CampaignSelectionV1["cost_bound"]>,
): boolean {
  return left.posture === right.posture &&
    left.implementation_units.length === right.implementation_units.length &&
    left.implementation_units.every((unit, index) => unit === right.implementation_units[index]) &&
    left.validation_units.length === right.validation_units.length &&
    left.validation_units.every((unit, index) => unit === right.validation_units[index]) &&
    bytesEqual(left.assumption_md, right.assumption_md);
}

/** Validate campaign-only transitions. Lifecycle removals remain transaction.ts's responsibility. */
export function validateCampaignTransition(inputs: CampaignTransitionInputs): readonly RoadmapIssue[] {
  const baseAuthorityIssues = validateCampaignAuthorityTuple(inputs.base_document.campaign);
  if (baseAuthorityIssues.length > 0) return baseAuthorityIssues;
  const candidateAuthorityIssues = validateCampaignAuthorityTuple(inputs.candidate_document.campaign);
  if (candidateAuthorityIssues.length > 0) return candidateAuthorityIssues;
  const issues: RoadmapIssue[] = [];
  for (const roadmap of ["matrix", "testing"] as const) {
    const base = campaignAuthority(inputs.base_document, roadmap);
    const candidate = campaignAuthority(inputs.candidate_document, roadmap);
    const delta = campaignAuthorityRank(candidate) - campaignAuthorityRank(base);
    if (delta < 0 || delta > 1) {
      issues.push(issue("E-CAMPAIGN-TRANSITION", `campaign.${roadmap}_authority`, `authority may move forward by one state only, got ${base} -> ${candidate}`));
    }
  }
  for (const [id, baseSelection] of inputs.base.selections) {
    const candidate = inputs.candidate.selections.get(id);
    if (candidate === undefined) continue;
    if (
      baseSelection.priority_class !== candidate.priority_class || baseSelection.cycle !== candidate.cycle ||
      !bytesEqual(baseSelection.selection_reason_md, candidate.selection_reason_md) ||
      !bytesEqual(baseSelection.remaining_scope_md, candidate.remaining_scope_md) ||
      (baseSelection.cost_bound !== undefined &&
        (candidate.cost_bound === undefined || !campaignCostBoundsEqual(baseSelection.cost_bound, candidate.cost_bound)))
    ) {
      issues.push(issue("E-CAMPAIGN-TRANSITION", `selection[${JSON.stringify(id)}]`, "state transition may not silently rewrite campaign selection identity fields"));
    }
  }
  for (const [id, baseReservation] of inputs.base.reservations) {
    const candidate = inputs.candidate.reservations.get(id);
    if (candidate === undefined) continue;
    if (baseReservation.work_kind !== candidate.work_kind || baseReservation.roadmap_path !== candidate.roadmap_path) {
      issues.push(issue("E-CAMPAIGN-TRANSITION", `legacy_markdown_reservation[${JSON.stringify(id)}]`, "reservation rebasing must preserve ID, namespace, and work kind"));
    }
  }
  for (const [id, selection] of inputs.candidate.selections) {
    if (selection.target_kind === "legacy_markdown_reservation" && !inputs.base.selections.has(id) &&
      !inputs.candidate.reservations.has(id)) {
      issues.push(issue("E-CAMPAIGN-TRANSITION", `selection[${JSON.stringify(id)}]`, "a new legacy selection must atomically add its reservation"));
    }
  }
  return Object.freeze(issues.sort((left, right) => codePointSort(left.logical_path, right.logical_path) ||
    codePointSort(left.code, right.code) || codePointSort(left.message, right.message)));
}

export function reservationBoundBytes(
  reservation: LegacyMarkdownReservationV1,
  markdown: ImmutableByteViewInput,
): Uint8Array | undefined {
  const view = createImmutableByteView(markdown);
  if (
    reservation.source_start_byte < 0 || reservation.source_end_byte <= reservation.source_start_byte ||
    reservation.source_end_byte > view.byte_length
  ) return undefined;
  const value = view.sliceBytes(reservation.source_start_byte, reservation.source_end_byte);
  return sha256(value) === reservation.source_sha256 ? value : undefined;
}

export function markdownContainsBytes(markdown: ImmutableByteViewInput, needle: Uint8Array): boolean {
  return createImmutableByteView(markdown).contains(needle);
}

export function expectedRoadmapPath(roadmap: RoadmapName): RepoPath {
  return ROADMAP_PATH[roadmap] as RepoPath;
}
