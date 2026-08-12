import type { RegistryView } from "./adapters/types.ts";
import { composeCampaignDocument, composeRetiredIdsDocument, composeRoadmapDocument } from "./compose.ts";
import {
  campaignIdentityOwners,
  campaignAuthority,
  createImmutableByteView,
  markdownContainsBytes,
  reservationBoundBytes,
  validateBootstrapShadowOwners,
  validateCampaign,
  validateCampaignTransition,
  workKindOfRecord,
  type CampaignRoadmapSnapshot,
  type CampaignValidationResult,
  type ByteViewInput,
  type LegacyTitleBindingFact,
} from "./campaign.ts";
import { validateCampaignAuthorityTuple } from "./campaign_authority.ts";
import {
  compareMigrationDebt,
  debtOwnerIndex,
  validateDebtGuardTransferFacts,
  validateDebtRetirementFacts,
  validateSemanticConversionFacts,
  type DebtReplacementResolutionFact,
  type MigrationDebt,
  type ValidatedDebtTransitionFacts,
} from "./debt.ts";
import type { RoadmapIssue } from "./errors.ts";
import { validateGlobalIdentity, type GlobalIdentityResult, type GlobalOwnerClaim } from "./identity.ts";
import { buildRoadmapIndexes } from "./indexes.ts";
import type { FullCommitId, RoadmapId, RoadmapName } from "./model/core.ts";
import type {
  CampaignDocumentV1,
  CurrentGuard,
  ActiveRecordOwnerFact,
  CurrentGuardOwnerFact,
  IdentityOwnerFact,
  ReplacementPin,
  Reference,
  RetiredIdsDocumentV1,
  RoadmapDocument,
  RoadmapDocumentV2,
  SemanticPayload,
  RecordNode,
  RoadmapDocumentV0,
  ShadowRecordClaim,
  TombstoneEligibleBaseOwner,
} from "./model/documents.ts";
import type { CompletedRenderIr } from "./render_ir.ts";
import {
  validateCombinedRoadmapReferences,
  validateSemanticRoadmapJoins,
  type SemanticJoinUniverse,
} from "./references.ts";
import { validateRelations } from "./relations.ts";
import {
  semanticConversionState,
  validateSemanticConversionCompletion,
  validateSemanticConversionTransition,
} from "./semantic_conversion.ts";
import { projectionLayout, projectionLayoutRank, validateProjectionLayoutTransition } from "./projection_layout.ts";
import { shadowRecordSourceTitle } from "./shadow_title.ts";
import {
  resolveReplacementPin,
  validateRetiredIds,
  validateRetiredTransition,
  type RetiredValidationResult,
  type TombstoneOriginFact,
} from "./retired.ts";

const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

export interface LifecycleRevisionInput {
  readonly campaign?: CampaignDocumentV1;
  readonly retired?: RetiredIdsDocumentV1;
  readonly roadmaps: Readonly<Record<RoadmapName, CampaignRoadmapSnapshot<ByteViewInput>>>;
  readonly registry: RegistryView;
  readonly legacy_title_bindings?: readonly LegacyTitleBindingFact[];
  readonly debt: Partial<Readonly<Record<RoadmapName, MigrationDebt>>>;
  readonly completed?: Partial<Readonly<Record<RoadmapName, CompletedRenderIr>>>;
}

export interface ValidatedLifecycleRevision {
  readonly campaign?: CampaignValidationResult;
  readonly campaign_document?: CampaignDocumentV1;
  readonly retired?: RetiredValidationResult;
  readonly retired_document?: RetiredIdsDocumentV1;
  readonly roadmaps: Readonly<Record<RoadmapName, CampaignRoadmapSnapshot<ByteViewInput>>>;
  readonly registry: RegistryView;
  readonly identity: GlobalIdentityResult;
  readonly debt: Partial<Readonly<Record<RoadmapName, MigrationDebt>>>;
  readonly completed: Partial<Readonly<Record<RoadmapName, CompletedRenderIr>>>;
  readonly issues: readonly RoadmapIssue[];
}

export interface ScopedRoadmapTransactionInputs {
  readonly scope: RoadmapName;
  readonly against: FullCommitId;
  readonly load_base: (roadmap: RoadmapName) => ScopedRoadmapBaseFacts;
  readonly candidate_document?: RoadmapDocument;
  readonly candidate_debt: MigrationDebt;
  readonly candidate_completed?: CompletedRenderIr;
  readonly candidate_registry: RegistryView;
  readonly candidate_global_identity: GlobalIdentityResult;
}

export interface ScopedRoadmapBaseFacts {
  readonly document?: RoadmapDocument;
  readonly debt: MigrationDebt;
  readonly completed?: CompletedRenderIr;
  readonly identity: GlobalIdentityResult;
  readonly registry: RegistryView;
}

export interface SelectedLifecycleContextInputs {
  readonly selection: RoadmapName;
  readonly campaign: CampaignDocumentV1;
  readonly retired: RetiredIdsDocumentV1;
  readonly document: RoadmapDocument;
  readonly registry: RegistryView;
}

export interface AllRoadmapsTransactionInputs {
  readonly scope: "all";
  readonly against: FullCommitId;
  readonly base: LifecycleRevisionInput;
  readonly candidate: LifecycleRevisionInput;
  readonly bootstrap?: boolean;
}

export type TransactionValidationInputs = ScopedRoadmapTransactionInputs | AllRoadmapsTransactionInputs;

export interface TransactionValidationResult {
  readonly issues: readonly RoadmapIssue[];
  readonly base?: ValidatedLifecycleRevision;
  readonly candidate?: ValidatedLifecycleRevision;
  readonly retired_ids: readonly RoadmapId[];
  readonly authority_transfers: readonly RoadmapId[];
  readonly guard_transfers: readonly RoadmapId[];
}

function issue(
  code: Extract<RoadmapIssue["code"], `E-TRANSACTION-${string}` | "E-SOURCE-MISSING" | "E-SCHEMA-STATE">,
  logicalPath: string,
  message: string,
): RoadmapIssue {
  return {
    code,
    source: "<transaction>",
    logical_path: logicalPath,
    message,
    exit: 1,
  };
}

function bytesEqual(left: Uint8Array, right: Uint8Array): boolean {
  return left.byteLength === right.byteLength && left.every((value, index) => value === right[index]);
}

function exactV2Promotion(base: RoadmapDocument, candidate: RoadmapDocument): boolean {
  if (base.document.schema_version !== 1 || candidate.document.schema_version !== 2 ||
    semanticConversionState(base).effective !== "complete") return false;
  const promoted = {
    ...base,
    document: {
      schema_version: 2,
      authority: "authoritative",
      roadmap: base.document.roadmap,
      source_path: base.document.source_path,
      projection_path: base.document.projection_path,
      frozen_source_sha256: base.document.frozen_source_sha256,
      frozen_source_byte_length: base.document.frozen_source_byte_length,
      frozen_source_line_count: base.document.frozen_source_line_count,
      frozen_source_eof: base.document.frozen_source_eof,
      projection_layout: "legacy_v1",
    },
  } as RoadmapDocument;
  return bytesEqual(composeRoadmapDocument(promoted), composeRoadmapDocument(candidate));
}

function allowedProjectionHeadingRetarget(base: Reference, candidate: Reference): boolean {
  return base.kind === "file_heading" && candidate.kind === "file_heading" &&
    base.id === candidate.id && base.source === candidate.source && base.path === candidate.path &&
    base.heading === "Next work items, in priority order" && candidate.heading === "Next work";
}

/** The WP7 layout flip is projection-only, except for the exact heading citations it invalidates. */
function exactProjectionLayoutPromotion(base: RoadmapDocument, candidate: RoadmapDocument): boolean {
  if (base.document.schema_version !== 2 || candidate.document.schema_version !== 2 ||
    projectionLayoutRank(projectionLayout(candidate)) !== projectionLayoutRank(projectionLayout(base)) + 1 ||
    (base as RoadmapDocumentV2).references.length !== (candidate as RoadmapDocumentV2).references.length) return false;
  const baseV2 = base as RoadmapDocumentV2;
  const candidateV2 = candidate as RoadmapDocumentV2;
  const baseReferences = new Map(baseV2.references.map((reference) => [reference.id, reference]));
  const normalizedReferences: Reference[] = [];
  for (const reference of candidateV2.references) {
    const prior = baseReferences.get(reference.id);
    if (prior === undefined) return false;
    const retarget = allowedProjectionHeadingRetarget(prior, reference);
    if (retarget && !(projectionLayout(base) === "standing_v1" &&
      projectionLayout(candidate) === "unnumbered_v1")) return false;
    normalizedReferences.push(retarget ? prior : reference);
  }
  const { projection_layout: _candidateLayout, ...candidateMeta } = candidate.document;
  const normalized: RoadmapDocument = {
    ...candidate,
    document: {
      ...candidateMeta,
      projection_layout: projectionLayout(base),
    },
    references: normalizedReferences,
  } as RoadmapDocument;
  return bytesEqual(composeRoadmapDocument(base), composeRoadmapDocument(normalized));
}

function completedBytesEqual(base: CompletedRenderIr | undefined, candidate: CompletedRenderIr | undefined): boolean {
  return base !== undefined && candidate !== undefined &&
    base.expected_bytes.bytesEqual(candidate.expected_bytes);
}

function selectedContextIssue(
  code: RoadmapIssue["code"],
  logical_path: string,
  message: string,
  source = "roadmap-campaign.toml",
): RoadmapIssue {
  return { code, source, logical_path, message, exit: 1 };
}

function namespaceOfRoadmapId(id: RoadmapId): RoadmapName | undefined {
  return id.startsWith("matrix.") ? "matrix" : id.startsWith("testing.") ? "testing" : undefined;
}

const LEGACY_ROADMAP_PATH = Object.freeze({
  matrix: "cddl-matrix/ROADMAP.md",
  testing: "tests/TESTING_ROADMAP.md",
} as const);

function sha256(value: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(value).digest("hex");
}

/**
 * Prove a selected v0 record's reservation ownership from the selected TOML alone. The scoped
 * query deliberately has no Markdown bytes: the frozen document metadata, raw owner bytes,
 * manifest placement, and exact span ledger are the complete evidence available at this seam.
 */
function selectedShadowClaim(
  document: RoadmapDocumentV0,
  id: RoadmapId,
  reservation?: CampaignDocumentV1["legacy_markdown_reservations"][number],
): ShadowRecordClaim | undefined {
  const namespace = document.document.roadmap;
  const records = document.records.filter((record) => record.id === id);
  const manifestRows = document.manifest.filter((entry) =>
    entry.kind === "record" && entry.record_id === id
  );
  if (records.length !== 1 || manifestRows.length !== 1) return undefined;
  const record = records[0]!;
  if (record.span_ids.length === 0 || new Set(record.span_ids).size !== record.span_ids.length) {
    return undefined;
  }
  const spans = record.span_ids.map((spanId) => document.spans.find((span) => span.id === spanId));
  if (spans.some((span) => span === undefined)) return undefined;
  const ordered = spans.map((span) => span!).sort((left, right) =>
    left.start_byte - right.start_byte || codePointSort(left.id, right.id)
  );
  const start = reservation?.source_start_byte ?? ordered[0]!.start_byte;
  const end = reservation?.source_end_byte ?? ordered.at(-1)!.end_byte;
  const allOwnerSpans = document.spans.filter((span) =>
    span.source_kind === "record" && span.owner_id === id && span.owner_field === "source_block_md"
  ).map((span) => span.id).sort(codePointSort);
  const claimedSpans = [...record.span_ids].sort(codePointSort);
  if (
    namespaceOfRoadmapId(id) !== namespace || document.document.authority !== "shadow" ||
    document.document.projection_path !== LEGACY_ROADMAP_PATH[namespace] ||
    ordered[0]!.start_byte !== start || ordered.at(-1)!.end_byte !== end ||
    end - start !== record.source_block_md.byteLength ||
    ordered.some((span, index) => index > 0 && ordered[index - 1]!.end_byte !== span.start_byte) ||
    ordered.some((span) => {
      const relativeStart = span.start_byte - start;
      const relativeEnd = span.end_byte - start;
      return span.source_kind !== "record" || span.owner_id !== id ||
        span.owner_field !== "source_block_md" || span.migration_status !== "raw" ||
        relativeStart < 0 || relativeEnd <= relativeStart || relativeEnd > record.source_block_md.byteLength ||
        sha256(record.source_block_md.slice(relativeStart, relativeEnd)) !== span.sha256;
    }) ||
    allOwnerSpans.length !== claimedSpans.length ||
    allOwnerSpans.some((spanId, index) => claimedSpans[index] !== spanId) ||
    shadowRecordSourceTitle(record.source_block_md, namespace) !== record.title ||
    (reservation !== undefined && (
      reservation.id !== id || reservation.roadmap_path !== LEGACY_ROADMAP_PATH[namespace] ||
      reservation.source_title !== record.title || reservation.source_sha256 !== sha256(record.source_block_md) ||
      reservation.whole_source_sha256 !== document.document.frozen_source_sha256
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

/**
 * Validate every activated invariant decidable from one selected roadmap plus global roots/registry.
 * Cross-roadmap joins deliberately remain the `all` scope's responsibility.
 */
export function validateSelectedLifecycleContext(
  inputs: SelectedLifecycleContextInputs,
): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [...validateCampaignAuthorityTuple(inputs.campaign.campaign)];
  const declaredAuthority = campaignAuthority(inputs.campaign, inputs.selection);
  if (declaredAuthority !== inputs.document.document.authority ||
    inputs.document.document.roadmap !== inputs.selection) {
    issues.push(selectedContextIssue(
      "E-SCHEMA-STATE",
      `campaign.${inputs.selection}_authority`,
      `campaign ${inputs.selection} authority ${declaredAuthority} does not match selected roadmap authority ${inputs.document.document.authority}`,
    ));
  }
  const seenReservations = new Set<RoadmapId>();
  for (const [index, reservation] of inputs.campaign.legacy_markdown_reservations.entries()) {
    if (seenReservations.has(reservation.id)) {
      issues.push(selectedContextIssue("E-CAMPAIGN-DUPLICATE", `legacy_markdown_reservation[${index}]`, "duplicate legacy reservation"));
    }
    seenReservations.add(reservation.id);
    const namespace = namespaceOfRoadmapId(reservation.id);
    const expectedPath = namespace === "matrix" ? "cddl-matrix/ROADMAP.md" : "tests/TESTING_ROADMAP.md";
    if (namespace === undefined || reservation.roadmap_path !== expectedPath) {
      issues.push(selectedContextIssue("E-CAMPAIGN-TARGET", `legacy_markdown_reservation[${index}]`, "reservation ID namespace and roadmap path differ"));
    }
    if (namespace !== undefined && campaignAuthority(inputs.campaign, namespace) === "authoritative") {
      issues.push(selectedContextIssue("E-CAMPAIGN-TARGET-EXPIRED", `legacy_markdown_reservation[${index}]`, `${namespace} reservations expire at authoritative v1`));
    }
  }
  const activeWork = new Map(inputs.document.records.flatMap((record) => {
    const kind = workKindOfRecord(inputs.document, record.id);
    return kind === undefined ? [] : [[record.id, kind] as const];
  }));
  const firedPromotions = new Set(inputs.document.records.filter((record) => {
    const payload = "payload" in record
      ? record.payload
      : "semantic_shadow" in record ? record.semantic_shadow : undefined;
    return payload?.kind === "signal" && payload.transition_kind === "promotion_trigger" &&
      payload.evaluation === "met";
  }).map((record) => record.id));
  const firedWork = new Set(inputs.document.records.filter((record) => {
    const payload = "payload" in record
      ? record.payload
      : "semantic_shadow" in record ? record.semantic_shadow : undefined;
    return payload?.kind === "work" && payload.work_state === "armed" &&
      payload.transition_ids.some((id) => firedPromotions.has(id));
  }).map((record) => record.id));
  const seenSelections = new Set<RoadmapId>();
  for (const [index, selection] of inputs.campaign.selections.entries()) {
    if (seenSelections.has(selection.item_id)) {
      issues.push(selectedContextIssue("E-CAMPAIGN-DUPLICATE", `selection[${index}]`, "duplicate campaign selection"));
    }
    seenSelections.add(selection.item_id);
    if (selection.selected_state === "selected") {
      if (selection.pickup_commit !== undefined) {
        issues.push(selectedContextIssue("E-CAMPAIGN-STATE", `selection[${index}]`, "selected state forbids pickup_commit"));
      }
    } else if (selection.pickup_commit === undefined || selection.assignee === undefined) {
      issues.push(selectedContextIssue("E-CAMPAIGN-STATE", `selection[${index}]`, "in_progress requires assignee and pickup_commit"));
    }
    const reservation = inputs.campaign.legacy_markdown_reservations.find((row) => row.id === selection.item_id);
    if (selection.target_kind === "legacy_markdown_reservation" && reservation === undefined) {
      issues.push(selectedContextIssue("E-CAMPAIGN-TARGET", `selection[${index}]`, "legacy target must resolve to one reservation"));
    }
    if (selection.target_kind === "active_id" && reservation !== undefined) {
      issues.push(selectedContextIssue("E-CAMPAIGN-TARGET", `selection[${index}]`, "active_id must not resolve to a legacy reservation"));
    }
    if (namespaceOfRoadmapId(selection.item_id) !== inputs.selection) continue;
    if (selection.target_kind === "active_id") {
      if (!activeWork.has(selection.item_id)) {
        issues.push(selectedContextIssue("E-CAMPAIGN-TARGET", `selection[${index}]`, "active_id must resolve to selected authoritative work and no reservation"));
      }
    }
  }
  for (const id of firedWork) {
    if (!seenSelections.has(id)) {
      issues.push(selectedContextIssue("E-CAMPAIGN-FIRED-HIDDEN", `selection[${JSON.stringify(id)}]`, `fired promotion for ${id} is hidden from the campaign`));
    }
  }
  const retired = validateRetiredIds(inputs.retired, inputs.registry);
  issues.push(...retired.issues);
  for (const guard of inputs.registry.current_guards) {
    if (!resolveReplacementPin(guard.replacement_pin, inputs.registry).resolved) {
      issues.push(issue("E-TRANSACTION-GUARD", `guard[${JSON.stringify(guard.id)}]`, "current guard replacement pin must resolve exactly once in this revision"));
    }
  }
  const indexes = buildRoadmapIndexes(inputs.document);
  issues.push(...indexes.issues);
  const selectedIsAuthoritative = declaredAuthority === "authoritative" &&
    inputs.document.document.schema_version !== 0;
  const identity = validateGlobalIdentity({
    documents: indexes.issues.length === 0 && selectedIsAuthoritative
      ? [indexes.indexes.identity_inputs]
      : [],
    current_guards: inputs.registry.current_guards,
    tombstones: retired.issues.length === 0 ? inputs.retired.entries : [],
  });
  issues.push(...identity.issues);

  const guards = new Set(inputs.registry.current_guards.map((guard) => guard.id));
  const tombstones = new Set(inputs.retired.entries.map((entry) => entry.id));
  const reservations = new Map(inputs.campaign.legacy_markdown_reservations.map((row) => [row.id, row]));
  const selectedFirstClass = new Set(selectedIsAuthoritative
    ? indexes.indexes.identity_inputs.id_providers.map((provider) => provider.id)
    : []);
  const selectedShadowClaims = new Map<RoadmapId, ShadowRecordClaim>();
  if (declaredAuthority === "shadow" && inputs.document.document.schema_version === 0) {
    const shadowDocument = inputs.document as RoadmapDocumentV0;
    for (const record of inputs.document.records) {
      const reservation = reservations.get(record.id);
      const claim = selectedShadowClaim(shadowDocument, record.id, reservation);
      if (claim === undefined) {
        issues.push(selectedContextIssue(
          "E-CAMPAIGN-TARGET",
          `record[${JSON.stringify(record.id)}]`,
          reservation === undefined
            ? "selected shadow declaration lacks one exact TOML owner/manifest/span binding"
            : "selected shadow reservation lacks one exact same-ID TOML owner/manifest/span binding",
          inputs.document.document.source_path,
        ));
      } else {
        selectedShadowClaims.set(record.id, claim);
      }
    }
    for (const reservation of inputs.campaign.legacy_markdown_reservations) {
      if (namespaceOfRoadmapId(reservation.id) === inputs.selection &&
        !selectedShadowClaims.has(reservation.id)) {
        issues.push(selectedContextIssue(
          "E-CAMPAIGN-TARGET",
          `legacy_markdown_reservation[${JSON.stringify(reservation.id)}]`,
          "selected shadow reservation lacks one exact same-ID TOML owner/manifest/span binding",
        ));
      }
    }
  }

  for (const reservation of inputs.campaign.legacy_markdown_reservations) {
    const id = reservation.id;
    if (guards.has(id) || tombstones.has(id) || selectedFirstClass.has(id)) {
      issues.push(selectedContextIssue(
        "E-OWNER-DUPLICATE",
        `owner[${JSON.stringify(id)}]`,
        "selected active/guard/tombstone ID collides with a legacy reservation",
        "<identity>",
      ));
    }
  }
  for (const id of selectedShadowClaims.keys()) {
    if (guards.has(id) || tombstones.has(id)) {
      issues.push(selectedContextIssue(
        "E-OWNER-DUPLICATE",
        `owner[${JSON.stringify(id)}]`,
        "selected shadow reservation collides with a guard or tombstone",
        "<identity>",
      ));
    }
  }
  for (const alias of indexes.indexes.identity_inputs.alias_providers) {
    if (reservations.has(alias.alias as RoadmapId)) {
      issues.push(selectedContextIssue(
        "E-ALIAS-COLLISION",
        `alias[${JSON.stringify(alias.alias)}]`,
        `legacy alias ${JSON.stringify(alias.alias)} collides with a legacy reservation ID`,
        "<identity>",
      ));
    }
  }
  return sortIssues(issues);
}

function sortIssues(issues: readonly RoadmapIssue[]): readonly RoadmapIssue[] {
  return Object.freeze([...issues].sort((left, right) =>
    codePointSort(left.source, right.source) || codePointSort(left.logical_path, right.logical_path) ||
    (left.span?.start_byte ?? -1) - (right.span?.start_byte ?? -1) ||
    codePointSort(left.code, right.code) || codePointSort(left.message, right.message)
  ));
}

function validCommit(value: string): boolean {
  return /^[0-9a-f]{40}(?:[0-9a-f]{24})?$/.test(value);
}

function namespaceOf(id: RoadmapId): RoadmapName {
  return id.startsWith("matrix.") ? "matrix" : "testing";
}

function ownerKind(claim: GlobalOwnerClaim): string {
  return claim.owner_kind;
}

function documentRecord(revision: ValidatedLifecycleRevision, id: RoadmapId) {
  const document = revision.roadmaps[namespaceOf(id)].document;
  return document?.records.find((record) => record.id === id);
}

function payloadOfRecord(record: RecordNode): SemanticPayload | undefined {
  if ("payload" in record) return record.payload;
  return "semantic_shadow" in record ? record.semantic_shadow : undefined;
}

function firstClassOrigin(
  revision: ValidatedLifecycleRevision,
  claim: GlobalOwnerClaim,
): TombstoneEligibleBaseOwner | "active_family" | "other_first_class" | undefined {
  if (claim.owner_kind === "legacy_markdown_reservation") return "legacy_markdown_reservation";
  if (claim.owner_kind === "current_guard") return "current_guard";
  if (claim.owner_kind !== "first_class") return undefined;
  const record = documentRecord(revision, claim.id);
  if (record === undefined) return "other_first_class";
  const payload = payloadOfRecord(record);
  return payload?.kind === "family" ? "active_family" : "active_record";
}

function isClosedFamily(revision: ValidatedLifecycleRevision, id: RoadmapId): boolean {
  const document = revision.roadmaps[namespaceOf(id)].document;
  if (document?.document.schema_version !== 2) return false;
  const record = document.records.find((candidate) => candidate.id === id);
  const payload = record === undefined ? undefined : payloadOfRecord(record);
  return payload?.kind === "family" && payload.family_maturity === "closed_denominator";
}

function componentOwnerRecordId(claim: GlobalOwnerClaim): RoadmapId | undefined {
  if (claim.owner_kind !== "first_class" || claim.value === null || typeof claim.value !== "object" ||
    !("owner_record_id" in claim.value) || !("kind" in claim.value) || claim.value.kind === "record") {
    return undefined;
  }
  return claim.value.owner_record_id as RoadmapId;
}

function firstClassProviderKind(claim: GlobalOwnerClaim): string | undefined {
  return claim.owner_kind === "first_class" && claim.value !== null && typeof claim.value === "object" &&
      "kind" in claim.value
    ? String(claim.value.kind)
    : undefined;
}

function pinEqual(left: ReplacementPin, right: ReplacementPin): boolean {
  if (left.kind !== right.kind) return false;
  const bytes = (value: Uint8Array): string => [...value].join(",");
  if (left.kind === "gate" && right.kind === "gate") {
    return left.gate_id === right.gate_id && bytes(left.claim_md) === bytes(right.claim_md);
  }
  if (left.kind === "test_symbol" && right.kind === "test_symbol") {
    return left.test_id === right.test_id && left.symbol === right.symbol &&
      bytes(left.claim_md) === bytes(right.claim_md);
  }
  return left.kind === "file_heading" && right.kind === "file_heading" &&
    left.path === right.path && left.heading === right.heading &&
    bytes(left.claim_md) === bytes(right.claim_md);
}

function activeIdentityDocuments(input: LifecycleRevisionInput) {
  return (["matrix", "testing"] as const).flatMap((roadmap) => {
    const document = input.roadmaps[roadmap].document;
    // V0 records reserve shadow IDs. They are never active first-class providers.
    if (document === undefined || document.document.schema_version === 0 || input.campaign === undefined ||
      campaignAuthority(input.campaign, roadmap) !== "authoritative") return [];
    return [buildRoadmapIndexes(document).indexes.identity_inputs];
  });
}

function validateCombinedRoadmapJoins(input: LifecycleRevisionInput): readonly RoadmapIssue[] {
  if (input.campaign === undefined) return [];
  const documents = (["matrix", "testing"] as const).flatMap((roadmap) => {
    const document = input.roadmaps[roadmap].document;
    return document !== undefined && document.document.schema_version !== 0 && campaignAuthority(input.campaign!, roadmap) === "authoritative"
      ? [document]
      : [];
  });
  if (documents.length !== 2) return [];
  const built = documents.map((document) => buildRoadmapIndexes(document));
  if (built.some((value) => value.issues.length > 0)) {
    return sortIssues(built.flatMap((value) => value.issues));
  }
  const firstClass = new Map(built.flatMap((value) => [...value.indexes.first_class]));
  const payloadRecords = new Map(built.flatMap((value) => [...value.indexes.payload_records]));
  const universe: SemanticJoinUniverse = Object.freeze({
    first_class: firstClass,
    payload_records: payloadRecords,
  });
  const perRoadmapIssues = built.flatMap(({ indexes }) => [
    ...validateSemanticRoadmapJoins(indexes, universe, indexes.roadmap === "matrix"
      ? "cddl-matrix/roadmap.toml"
      : "tests/testing-roadmap.toml"),
    ...validateCombinedRoadmapReferences(indexes, universe.first_class, indexes.roadmap === "matrix"
      ? "cddl-matrix/roadmap.toml"
      : "tests/testing-roadmap.toml"),
  ]);
  // Relation uniqueness and acyclicity are properties of the joined graph. Validating each
  // roadmap separately would miss an inverse symmetric edge or cycle split across the two files.
  const combinedRelations = built.flatMap(({ indexes }) => indexes.relations);
  return sortIssues([
    ...perRoadmapIssues,
    ...validateRelations(combinedRelations, universe.first_class, "<combined-roadmaps>"),
  ]);
}

/** Assemble one revision from explicit decoded documents and injected same-revision facts. */
export function validateLifecycleRevision(input: LifecycleRevisionInput): ValidatedLifecycleRevision {
  const issues: RoadmapIssue[] = [];
  if (input.campaign === undefined) {
    issues.push(issue("E-SOURCE-MISSING", "campaign", "campaign root is required outside the one explicit bootstrap base"));
  }
  if (input.retired === undefined) {
    issues.push(issue("E-SOURCE-MISSING", "retired_ids", "retired-ID root is required outside the one explicit bootstrap base"));
  }
  const authorityTupleIssues = input.campaign === undefined
    ? []
    : validateCampaignAuthorityTuple(input.campaign.campaign);
  if (authorityTupleIssues.length > 0) {
    const campaign = validateCampaign({
      campaign: input.campaign!,
      roadmaps: input.roadmaps,
      legacy_title_bindings: input.legacy_title_bindings,
    });
    const retired = input.retired === undefined ? undefined : validateRetiredIds(input.retired, input.registry);
    if (retired !== undefined) issues.push(...retired.issues);
    issues.push(...campaign.issues);
    return Object.freeze({
      campaign,
      campaign_document: input.campaign,
      retired,
      retired_document: input.retired,
      roadmaps: input.roadmaps,
      registry: input.registry,
      identity: validateGlobalIdentity({ documents: [] }),
      debt: input.debt,
      completed: input.completed ?? {},
      issues: sortIssues(issues),
    });
  }
  const firstCampaign = input.campaign === undefined ? undefined : validateCampaign({
    campaign: input.campaign,
    roadmaps: input.roadmaps,
    legacy_title_bindings: input.legacy_title_bindings,
  });
  const retired = input.retired === undefined ? undefined : validateRetiredIds(input.retired, input.registry);
  issues.push(...validateCombinedRoadmapJoins(input));
  if (retired !== undefined) issues.push(...retired.issues);
  for (const guard of input.registry.current_guards) {
    if (!resolveReplacementPin(guard.replacement_pin, input.registry).resolved) {
      issues.push(issue("E-TRANSACTION-GUARD", `guard[${JSON.stringify(guard.id)}]`, "current guard replacement pin must resolve exactly once in this revision"));
    }
  }
  const campaignOwners = firstCampaign !== undefined && firstCampaign.issues.length === 0
    ? campaignIdentityOwners(firstCampaign)
    : undefined;
  if (firstCampaign !== undefined && firstCampaign.issues.length === 0 && campaignOwners === undefined) {
    issues.push(issue("E-TRANSACTION-OWNER", "campaign.owners", "campaign identity capability is invalid or mutated"));
  }
  const identity = validateGlobalIdentity({
    documents: firstCampaign?.issues.length === 0 ? activeIdentityDocuments(input) : [],
    current_guards: input.registry.current_guards,
    tombstones: retired?.issues.length === 0 ? input.retired?.entries ?? [] : [],
    ...(campaignOwners === undefined ? {} : { additional_owners: campaignOwners }),
  });
  issues.push(...identity.issues);
  let campaign = firstCampaign;
  if (input.campaign !== undefined) {
    campaign = validateCampaign({
      campaign: input.campaign,
      roadmaps: input.roadmaps,
      legacy_title_bindings: input.legacy_title_bindings,
      identity,
    });
    issues.push(...campaign.issues);
  }
  return Object.freeze({
    campaign,
    campaign_document: input.campaign,
    retired,
    retired_document: input.retired,
    roadmaps: input.roadmaps,
    registry: input.registry,
    identity,
    debt: input.debt,
    completed: input.completed ?? {},
    issues: sortIssues(issues),
  });
}

function selectionFor(revision: ValidatedLifecycleRevision, id: RoadmapId) {
  return revision.campaign?.selections.get(id);
}

function guardFor(revision: ValidatedLifecycleRevision, id: RoadmapId): CurrentGuard | undefined {
  return revision.registry.current_guards.find((guard) => guard.id === id);
}

function candidateEdgesContain(revision: ValidatedLifecycleRevision, id: RoadmapId): boolean {
  for (const roadmap of ["matrix", "testing"] as const) {
    const document = revision.roadmaps[roadmap].document;
    if (document === undefined || document.document.schema_version === 0) continue;
    if (!("relations" in document)) continue;
    if (document.relations.some((edge) => edge.source === id || edge.target === id)) return true;
    if (document.references.some((reference) =>
      reference.source === id || (reference.kind === "roadmap" && reference.target_id === id)
    )) return true;
  }
  return false;
}

function candidateRelationsContain(revision: ValidatedLifecycleRevision, id: RoadmapId): boolean {
  return (["matrix", "testing"] as const).some((roadmap) => {
    const document = revision.roadmaps[roadmap].document;
    return document !== undefined && "relations" in document &&
      document.relations.some((edge) => edge.source === id || edge.target === id);
  });
}

function candidateReferencesContain(revision: ValidatedLifecycleRevision, id: RoadmapId): boolean {
  return (["matrix", "testing"] as const).some((roadmap) => {
    const document = revision.roadmaps[roadmap].document;
    return document !== undefined && "references" in document && document.references.some((reference) =>
      reference.source === id || (reference.kind === "roadmap" && reference.target_id === id)
    );
  });
}

function validateRetirementClosure(
  candidate: ValidatedLifecycleRevision,
  id: RoadmapId,
  issues: RoadmapIssue[],
): void {
  if (selectionFor(candidate, id) !== undefined) {
    issues.push(issue("E-TRANSACTION-CAMPAIGN", `selection[${JSON.stringify(id)}]`, "retired owner remains selected"));
  }
  const survivingCitations = candidate.registry.roadmap_citations
    .filter((citation) => citation.id === id)
    .sort((left, right) =>
      codePointSort(left.source, right.source) || left.span.start_byte - right.span.start_byte ||
      left.span.end_byte - right.span.end_byte || codePointSort(left.raw, right.raw)
    );
  for (const citation of survivingCitations) {
    issues.push({
      code: "E-TRANSACTION-CITATION",
      source: citation.source,
      logical_path: `citation[${JSON.stringify(id)}]`,
      message: "retired ID remains cited in a durable tracked repository file",
      span: citation.span,
      exit: 1,
    });
  }
  if (candidateRelationsContain(candidate, id)) {
    issues.push(issue("E-TRANSACTION-REFERENCE", `relation[${JSON.stringify(id)}]`, "retired ID remains a relation endpoint"));
  }
  if (candidateReferencesContain(candidate, id)) {
    issues.push(issue("E-TRANSACTION-REFERENCE", `reference[${JSON.stringify(id)}]`, "retired ID remains a typed reference source or target"));
  }
}

function validateLegacyDelivery(
  against: FullCommitId,
  base: ValidatedLifecycleRevision,
  candidate: ValidatedLifecycleRevision,
  id: RoadmapId,
  issues: RoadmapIssue[],
): void {
  const baseOwner = base.campaign?.owners.find((owner) =>
    owner.id === id && owner.owner_kind === "legacy_markdown_reservation"
  );
  if (baseOwner?.owner_kind !== "legacy_markdown_reservation") {
    issues.push(issue("E-TRANSACTION-ORIGIN", `owner[${JSON.stringify(id)}]`, "legacy delivery requires one reviewed base reservation"));
    return;
  }
  const namespace = baseOwner.namespace;
  const bound = reservationBoundBytes(baseOwner.reservation, base.roadmaps[namespace].markdown);
  if (bound === undefined || markdownContainsBytes(candidate.roadmaps[namespace].markdown, bound)) {
    issues.push(issue("E-TRANSACTION-CAMPAIGN", `owner[${JSON.stringify(id)}].bound_source`, "candidate Markdown still contains the complete base bound source slice"));
  }
  const candidateDocument = candidate.roadmaps[namespace].document;
  const candidateMarkdown = createImmutableByteView(candidate.roadmaps[namespace].markdown);
  if (candidateDocument?.records.some((record) => record.id === id)) {
    issues.push(issue("E-TRANSACTION-OWNER", `owner[${JSON.stringify(id)}].shadow`, "candidate shadow/active declaration survives legacy delivery"));
  }
  if (candidate.campaign_document !== undefined &&
    campaignAuthority(candidate.campaign_document, namespace) === "shadow" &&
    candidateDocument !== undefined &&
    (candidateDocument.document.frozen_source_sha256 !== candidateMarkdown.wholeSha256() ||
      candidateDocument.document.frozen_source_byte_length !== candidateMarkdown.byte_length)) {
    issues.push(issue("E-TRANSACTION-CAMPAIGN", `owner[${JSON.stringify(id)}].shadow_rebase`, "candidate shadow metadata is not rebased to the immutable candidate Markdown snapshot"));
  }
  const tombstone = candidate.retired?.entries.get(id);
  if (tombstone === undefined) {
    issues.push(issue("E-TRANSACTION-TOMBSTONE", `retired_ids.entry[${JSON.stringify(id)}]`, "legacy delivery requires one exact tombstone"));
  } else if (tombstone.last_active_at !== against) {
    issues.push(issue("E-TRANSACTION-TOMBSTONE", `retired_ids.entry[${JSON.stringify(id)}].last_active_at`, "legacy delivery tombstone must name the explicit base commit"));
  }
  validateRetirementClosure(candidate, id, issues);
}

function validateAuthorityTransfer(
  base: ValidatedLifecycleRevision,
  candidate: ValidatedLifecycleRevision,
  id: RoadmapId,
  issues: RoadmapIssue[],
): void {
  const baseOwner = base.campaign?.owners.find((owner) =>
    owner.id === id && owner.owner_kind === "legacy_markdown_reservation"
  );
  if (baseOwner?.owner_kind !== "legacy_markdown_reservation") return;
  const namespace = baseOwner.namespace;
  if (candidate.campaign_document === undefined ||
    campaignAuthority(candidate.campaign_document, namespace) !== "authoritative") {
    issues.push(issue("E-TRANSACTION-OWNER", `owner[${JSON.stringify(id)}]`, "reservation transfer requires candidate authoritative v1"));
  }
  const workKind = workKindOfRecord(candidate.roadmaps[namespace].document, id);
  if (workKind === undefined || workKind !== baseOwner.work_kind) {
    issues.push(issue("E-TRANSACTION-OWNER", `owner[${JSON.stringify(id)}].work_kind`, "authority transfer must preserve the reviewed work kind"));
  }
  const selection = selectionFor(candidate, id);
  if (selection !== undefined && selection.target_kind !== "active_id") {
    issues.push(issue("E-TRANSACTION-CAMPAIGN", `selection[${JSON.stringify(id)}].target_kind`, "surviving cutover selection must target active_id"));
  }
  if (candidate.retired?.entries.has(id)) {
    issues.push(issue("E-TRANSACTION-TOMBSTONE", `retired_ids.entry[${JSON.stringify(id)}]`, "authority transfer must not create a tombstone"));
  }
}

function bootstrapBaseValid(inputs: AllRoadmapsTransactionInputs): boolean {
  if (!inputs.bootstrap || inputs.base.campaign !== undefined || inputs.base.retired !== undefined) return false;
  const candidateCampaign = inputs.candidate.campaign;
  const candidateRetired = inputs.candidate.retired;
  const baseMatrix = inputs.base.roadmaps.matrix.document;
  if (
    candidateCampaign === undefined || candidateRetired === undefined || candidateRetired.entries.length !== 0 ||
    campaignAuthority(candidateCampaign, "matrix") !== "authoritative" ||
    baseMatrix?.document.schema_version !== 0
  ) return false;
  const testingState = campaignAuthority(candidateCampaign, "testing");
  if (testingState !== "legacy_markdown" && testingState !== "shadow") return false;
  const baseTesting = inputs.base.roadmaps.testing.document;
  if (testingState === "legacy_markdown" && baseTesting !== undefined) return false;
  if (testingState === "shadow" && baseTesting?.document.schema_version !== 0) return false;
  return inputs.candidate.registry.current_guards.length === 0;
}

function activeOwnerFact(
  revision: ValidatedLifecycleRevision,
  id: RoadmapId,
): ActiveRecordOwnerFact | undefined {
  const record = documentRecord(revision, id);
  if (record === undefined || !("render_authority" in record)) return undefined;
  return { owner_kind: "active_record", id, namespace: namespaceOf(id), record };
}

function guardOwnerFact(guard: CurrentGuard): CurrentGuardOwnerFact {
  return { owner_kind: "current_guard", id: guard.id, namespace: namespaceOf(guard.id), guard };
}

function replacementFacts(registry: RegistryView): readonly DebtReplacementResolutionFact[] {
  return Object.freeze([
    ...registry.gates,
    ...registry.test_symbols,
    ...registry.tracked_headings,
  ]);
}

function exactReplacementFact(
  pin: ReplacementPin,
  registry: RegistryView,
): DebtReplacementResolutionFact | undefined {
  if (!resolveReplacementPin(pin, registry).resolved) return undefined;
  if (pin.kind === "gate") return registry.gates.find((fact) => fact.id === pin.gate_id && !fact.stub);
  if (pin.kind === "test_symbol") {
    return registry.test_symbols.find((fact) => fact.test_id === pin.test_id && fact.symbol === pin.symbol);
  }
  return registry.tracked_headings.find((fact) => fact.path === pin.path && fact.heading === pin.heading);
}

function sourceFingerprint(document: RoadmapDocument) {
  return {
    source_path: document.document.source_path,
    sha256: document.document.frozen_source_sha256,
    byte_length: document.document.frozen_source_byte_length,
  };
}

function validateShadowAuthorityTransfer(
  base: ValidatedLifecycleRevision,
  candidate: ValidatedLifecycleRevision,
  id: RoadmapId,
  issues: RoadmapIssue[],
): void {
  const namespace = namespaceOf(id);
  const baseDocument = base.roadmaps[namespace].document;
  const candidateDocument = candidate.roadmaps[namespace].document;
  const baseRecord = baseDocument?.records.find((record) => record.id === id);
  const candidateRecord = candidateDocument?.records.find((record) => record.id === id);
  const basePayload = baseRecord === undefined ? undefined : payloadOfRecord(baseRecord);
  const candidatePayload = candidateRecord === undefined ? undefined : payloadOfRecord(candidateRecord);
  if (base.campaign_document !== undefined &&
    campaignAuthority(base.campaign_document, namespace) !== "shadow") {
    issues.push(issue("E-TRANSACTION-OWNER", `owner[${JSON.stringify(id)}]`, "shadow-only transfer requires a shadow-authority base"));
  }
  if (candidate.campaign_document === undefined ||
    campaignAuthority(candidate.campaign_document, namespace) !== "authoritative" ||
    baseDocument?.document.schema_version !== 0 || candidateDocument?.document.schema_version !== 1 ||
    baseRecord === undefined || candidateRecord === undefined ||
    (basePayload !== undefined && basePayload.kind !== candidatePayload?.kind) ||
    (basePayload?.kind === "work" && candidatePayload?.kind === "work" &&
      basePayload.work_kind !== candidatePayload.work_kind)) {
    issues.push(issue(
      "E-TRANSACTION-OWNER",
      `owner[${JSON.stringify(id)}]`,
      "shadow-only cutover requires one same-ID active owner with matching payload/work kind",
    ));
  }
  if (candidate.retired?.entries.has(id)) {
    issues.push(issue("E-TRANSACTION-TOMBSTONE", `retired_ids.entry[${JSON.stringify(id)}]`, "shadow authority transfer must not create a tombstone"));
  }
}

function validateScoped(inputs: ScopedRoadmapTransactionInputs): TransactionValidationResult {
  const issues: RoadmapIssue[] = [];
  const base = inputs.load_base(inputs.scope);
  if (!validCommit(inputs.against)) {
    issues.push(issue("E-TRANSACTION-BASE", "against", "transaction base must be one explicit full lowercase commit ID"));
  }
  if (base.registry.revision.kind !== "commit" || base.registry.revision.commit !== inputs.against) {
    issues.push(issue("E-TRANSACTION-BASE", "base.registry.revision", "base registry facts must come from the exact explicit --against commit"));
  }
  if (inputs.candidate_registry.revision.kind !== "worktree") {
    issues.push(issue("E-TRANSACTION-BASE", "candidate.registry.revision", "candidate registry facts must come from the worktree revision"));
  }
  if (
    base.document === undefined || inputs.candidate_document === undefined ||
    base.document.document.schema_version === 0 || inputs.candidate_document.document.schema_version === 0 ||
    base.document.document.schema_version !== inputs.candidate_document.document.schema_version ||
    base.document.document.roadmap !== inputs.scope ||
    inputs.candidate_document.document.roadmap !== inputs.scope
  ) {
    issues.push(issue("E-TRANSACTION-BASE", inputs.scope, "single-roadmap comparison requires matching authoritative schema versions; v2 promotion is global"));
  } else {
    issues.push(...validateSemanticConversionTransition(
      base.document,
      inputs.candidate_document,
    ));
    issues.push(...validateProjectionLayoutTransition(base.document, inputs.candidate_document));
    if (projectionLayoutRank(projectionLayout(inputs.candidate_document)) >
      projectionLayoutRank(projectionLayout(base.document)) &&
      (!exactProjectionLayoutPromotion(base.document, inputs.candidate_document) ||
        !completedBytesEqual(base.completed, inputs.candidate_completed))) {
      issues.push(issue(
        "E-TRANSACTION-BASE",
        `${inputs.scope}.document.projection_layout`,
        "projection-layout promotion must be one adjacent projection-only stage; only standing_v1 to unnumbered_v1 permits exact Next-heading reference retargets over byte-identical render IR",
      ));
    }
    if (semanticConversionState(inputs.candidate_document).effective === "complete") {
      if (inputs.candidate_completed === undefined) {
        issues.push(issue(
          "E-TRANSACTION-BASE",
          `${inputs.scope}.completed_render_ir`,
          "semantic_conversion = complete requires a complete candidate render IR audit",
        ));
      } else {
        issues.push(...validateSemanticConversionCompletion(
          inputs.candidate_document,
          inputs.candidate_debt,
          inputs.candidate_completed,
        ));
      }
    }
    const transition = validateSemanticConversionFacts(base.debt, inputs.candidate_debt, {
      base_document: base.document,
      candidate_document: inputs.candidate_document,
      ...(base.completed === undefined ? {} : { base_completed: base.completed }),
      ...(inputs.candidate_completed === undefined ? {} : { candidate_completed: inputs.candidate_completed }),
    });
    if (!transition.ok) issues.push(...transition.issues);
    issues.push(...compareMigrationDebt(base.debt, inputs.candidate_debt, {
      base_document: base.document,
      candidate_document: inputs.candidate_document,
      ...(base.completed === undefined ? {} : { base_completed: base.completed }),
      ...(inputs.candidate_completed === undefined ? {} : { candidate_completed: inputs.candidate_completed }),
      ...(transition.ok && transition.facts !== undefined ? { transition_facts: transition.facts } : {}),
    }));
  }
  issues.push(...inputs.candidate_global_identity.issues);
  for (const [id, baseOwner] of base.identity.owners) {
    if (namespaceOf(id) !== inputs.scope) continue;
    const candidateOwner = inputs.candidate_global_identity.owners.get(id);
    if (candidateOwner === undefined || ownerKind(candidateOwner) !== ownerKind(baseOwner)) {
      issues.push(issue("E-TRANSACTION-OWNER", `owner[${JSON.stringify(id)}]`, "lifecycle owner removal or kind change requires --roadmap all"));
    }
  }
  return Object.freeze({
    issues: sortIssues(issues),
    retired_ids: Object.freeze([]),
    authority_transfers: Object.freeze([]),
    guard_transfers: Object.freeze([]),
  });
}

function validateAll(inputs: AllRoadmapsTransactionInputs): TransactionValidationResult {
  const issues: RoadmapIssue[] = [];
  const retiredIds: RoadmapId[] = [];
  const authorityTransfers: RoadmapId[] = [];
  const guardTransfers: RoadmapId[] = [];
  const activeRetirements = new Map<RoadmapName, RoadmapId[]>();
  const familyGuardTransferIds = new Map<RoadmapName, RoadmapId[]>();
  if (!validCommit(inputs.against)) {
    issues.push(issue("E-TRANSACTION-BASE", "against", "transaction base must be one explicit full lowercase commit ID"));
  }
  if (inputs.base.registry.revision.kind !== "commit" ||
    inputs.base.registry.revision.commit !== inputs.against) {
    issues.push(issue("E-TRANSACTION-BASE", "base.registry.revision", "base registry facts must come from the exact explicit --against commit"));
  }
  if (inputs.candidate.registry.revision.kind !== "worktree") {
    issues.push(issue("E-TRANSACTION-BASE", "candidate.registry.revision", "candidate registry facts must come from the worktree revision"));
  }
  const bootstrap = bootstrapBaseValid(inputs);
  let base: ValidatedLifecycleRevision;
  if (bootstrap) {
    const bootstrapCampaign = validateBootstrapShadowOwners(inputs.base.roadmaps);
    issues.push(...bootstrapCampaign.issues);
    const bootstrapOwners = campaignIdentityOwners(bootstrapCampaign);
    if (bootstrapCampaign.issues.length === 0 && bootstrapOwners === undefined) {
      issues.push(issue("E-TRANSACTION-OWNER", "bootstrap.owners", "bootstrap shadow identity capability is invalid or mutated"));
    }
    const identity = validateGlobalIdentity({
      documents: [],
      ...(bootstrapOwners === undefined ? {} : { additional_owners: bootstrapOwners }),
    });
    issues.push(...identity.issues);
    base = Object.freeze({
      campaign: undefined,
      campaign_document: undefined,
      retired: undefined,
      retired_document: undefined,
      roadmaps: inputs.base.roadmaps,
      registry: inputs.base.registry,
      identity,
      debt: inputs.base.debt,
      completed: inputs.base.completed ?? {},
      issues: bootstrapCampaign.issues,
    });
  } else {
    base = validateLifecycleRevision(inputs.base);
    issues.push(...base.issues);
  }
  const candidate = validateLifecycleRevision(inputs.candidate);
  issues.push(...candidate.issues);
  if (!bootstrap && inputs.bootstrap) {
    issues.push(issue("E-TRANSACTION-BASE", "bootstrap", "missing-root bootstrap shape does not match the one WP4M exception"));
  }
  if (!bootstrap && base.campaign !== undefined && candidate.campaign !== undefined &&
    base.campaign_document !== undefined && candidate.campaign_document !== undefined) {
    issues.push(...validateCampaignTransition({
      base: base.campaign,
      candidate: candidate.campaign,
      base_document: base.campaign_document,
      candidate_document: candidate.campaign_document,
      against: inputs.against,
    }));
  }

  if (!bootstrap && base.retired !== undefined && candidate.retired !== undefined) {
    const origins: TombstoneOriginFact[] = [];
    for (const [id, claim] of base.identity.owners) {
      const origin = firstClassOrigin(base, claim);
      if (origin === "active_record" || origin === "current_guard" || origin === "legacy_markdown_reservation") {
        origins.push({ id, owner_kind: origin });
      }
    }
    issues.push(...validateRetiredTransition({
      base: base.retired,
      candidate: candidate.retired,
      against: inputs.against,
      eligible_base_origins: origins,
    }));
  }

  const lifecycleReady = base.issues.length === 0 && base.identity.issues.length === 0 &&
    candidate.issues.length === 0 && candidate.identity.issues.length === 0 &&
    validCommit(inputs.against) && base.registry.revision.kind === "commit" &&
    base.registry.revision.commit === inputs.against && candidate.registry.revision.kind === "worktree";
  const versionChanges = (["matrix", "testing"] as const).filter((roadmap) =>
    base.roadmaps[roadmap].document?.document.schema_version !==
      candidate.roadmaps[roadmap].document?.document.schema_version
  );
  for (const [label, revision] of [["base", base], ["candidate", candidate]] as const) {
    const matrixVersion = revision.roadmaps.matrix.document?.document.schema_version;
    const testingVersion = revision.roadmaps.testing.document?.document.schema_version;
    const bothAuthoritative = revision.campaign_document !== undefined &&
      campaignAuthority(revision.campaign_document, "matrix") === "authoritative" &&
      campaignAuthority(revision.campaign_document, "testing") === "authoritative";
    if (bothAuthoritative && matrixVersion !== undefined && matrixVersion !== 0 &&
      testingVersion !== undefined && testingVersion !== 0 && matrixVersion !== testingVersion) {
      issues.push(issue("E-TRANSACTION-BASE", `${label}.document.schema_version`, "authoritative matrix and testing roadmaps must use one schema version"));
    }
  }
  const v2Transition = versionChanges.some((roadmap) =>
    base.roadmaps[roadmap].document?.document.schema_version === 2 ||
    candidate.roadmaps[roadmap].document?.document.schema_version === 2
  );
  if (v2Transition) {
    if (versionChanges.length !== 2) {
      issues.push(issue("E-TRANSACTION-BASE", "document.schema_version", "roadmap schema v2 promotion must update matrix and testing atomically"));
    }
    for (const roadmap of ["matrix", "testing"] as const) {
      const baseDocument = base.roadmaps[roadmap].document;
      const candidateDocument = candidate.roadmaps[roadmap].document;
      if (baseDocument === undefined || candidateDocument === undefined || !exactV2Promotion(baseDocument, candidateDocument)) {
        issues.push(issue("E-TRANSACTION-BASE", `${roadmap}.document.schema_version`, "v1-complete to v2 promotion permits only the intrinsic schema metadata change"));
      }
      if (!completedBytesEqual(base.completed[roadmap], candidate.completed[roadmap])) {
        issues.push(issue("E-TRANSACTION-BASE", `${roadmap}.completed_render_ir`, "v2 promotion requires byte-identical completed projections"));
      }
    }
    if (base.campaign_document === undefined || candidate.campaign_document === undefined ||
      !bytesEqual(composeCampaignDocument(base.campaign_document), composeCampaignDocument(candidate.campaign_document))) {
      issues.push(issue("E-TRANSACTION-BASE", "campaign", "v2 promotion must not change the campaign document"));
    }
    if (base.retired_document === undefined || candidate.retired_document === undefined ||
      !bytesEqual(composeRetiredIdsDocument(base.retired_document), composeRetiredIdsDocument(candidate.retired_document))) {
      issues.push(issue("E-TRANSACTION-BASE", "retired_ids", "v2 promotion must not change the retired-ID document"));
    }
  }
  const familyGuardTransfers = new Set<RoadmapId>();
  if (lifecycleReady) {
    for (const [id, baseClaim] of base.identity.owners) {
      if (firstClassOrigin(base, baseClaim) === "active_family" &&
        candidate.identity.owners.get(id)?.owner_kind === "current_guard") {
        if (isClosedFamily(base, id)) familyGuardTransfers.add(id);
        else issues.push(issue("E-TRANSACTION-GUARD", `guard[${JSON.stringify(id)}]`, "only a closed-denominator family may transfer to a current guard"));
      }
    }
    for (const guard of candidate.registry.current_guards) {
      const baseClaim = base.identity.owners.get(guard.id);
      const parent = baseClaim === undefined ? undefined : componentOwnerRecordId(baseClaim);
      const retained = baseClaim?.owner_kind === "current_guard";
      const rootTransfer = baseClaim !== undefined && firstClassOrigin(base, baseClaim) === "active_family" &&
        familyGuardTransfers.has(guard.id);
      const childTransfer = parent !== undefined && familyGuardTransfers.has(parent);
      if (!retained && !rootTransfer && !childTransfer) {
        issues.push(issue(
          "E-TRANSACTION-GUARD",
          `guard[${JSON.stringify(guard.id)}]`,
          "candidate-only guard has no exact base guard or family/systematic-child transfer origin",
        ));
      }
    }
    for (const [id, baseClaim] of base.identity.owners) {
      const candidateClaim = candidate.identity.owners.get(id);
      const componentOwner = componentOwnerRecordId(baseClaim);
      if (componentOwner !== undefined && familyGuardTransfers.has(componentOwner)) {
        const sameActiveKind = candidateClaim?.owner_kind === "first_class" &&
          firstClassProviderKind(candidateClaim) === firstClassProviderKind(baseClaim);
        const protectedGuard = candidateClaim?.owner_kind === "current_guard" &&
          guardFor(candidate, id) !== undefined &&
          resolveReplacementPin(guardFor(candidate, id)!.replacement_pin, candidate.registry).resolved;
        if (!sameActiveKind && !protectedGuard) {
          issues.push(issue(
            "E-TRANSACTION-GUARD",
            `guard[${JSON.stringify(id)}]`,
            "every systematic family child ID must retain its exact provider kind or gain one resolving current guard",
          ));
        }
        continue;
      }
      const candidateClaims = candidate.identity.owner_claims.get(id) ?? [];
      if (candidateClaims.some((claim) => ownerKind(claim) === ownerKind(baseClaim))) continue;
      const origin = firstClassOrigin(base, baseClaim);
      if (baseClaim.owner_kind === "shadow_record_reservation" && candidateClaim?.owner_kind === "first_class") {
        validateShadowAuthorityTransfer(base, candidate, id, issues);
        authorityTransfers.push(id);
        continue;
      }
      if (origin === "legacy_markdown_reservation" && candidateClaim?.owner_kind === "first_class") {
        validateAuthorityTransfer(base, candidate, id, issues);
        authorityTransfers.push(id);
        continue;
      }
      if (origin === "active_family" && candidateClaim?.owner_kind === "current_guard") {
        if (!isClosedFamily(base, id)) continue;
        const guard = guardFor(candidate, id);
        if (guard === undefined || !resolveReplacementPin(guard.replacement_pin, candidate.registry).resolved ||
          candidate.retired?.entries.has(id)) {
          issues.push(issue("E-TRANSACTION-GUARD", `guard[${JSON.stringify(id)}]`, "family guard transfer requires one resolving current guard and no tombstone"));
        }
        validateRetirementClosure(candidate, id, issues);
        const namespace = namespaceOf(id);
        familyGuardTransferIds.set(namespace, [...familyGuardTransferIds.get(namespace) ?? [], id]);
        guardTransfers.push(id);
        continue;
      }
      if (candidateClaim?.owner_kind === "tombstone") {
        if (origin !== "active_record" && origin !== "current_guard" && origin !== "legacy_markdown_reservation") {
          issues.push(issue("E-TRANSACTION-ORIGIN", `owner[${JSON.stringify(id)}]`, `base owner ${origin ?? baseClaim.owner_kind} is not tombstone-eligible`));
          continue;
        }
        const tombstone = candidate.retired?.entries.get(id);
        if (tombstone === undefined || tombstone.last_active_at !== inputs.against ||
          !resolveReplacementPin(tombstone.replacement, candidate.registry).resolved) {
          issues.push(issue("E-TRANSACTION-TOMBSTONE", `retired_ids.entry[${JSON.stringify(id)}]`, "retirement requires one resolving tombstone at the explicit base commit"));
        }
        if (origin === "legacy_markdown_reservation") {
          // Legacy Markdown has no C3 document/debt owner when TOML is absent. Its exact C5 path is
          // intentionally independent of validateDebtRetirementFacts.
          validateLegacyDelivery(inputs.against, base, candidate, id, issues);
        } else {
          validateRetirementClosure(candidate, id, issues);
          if (origin === "active_record") {
            const namespace = namespaceOf(id);
            activeRetirements.set(namespace, [...activeRetirements.get(namespace) ?? [], id]);
          }
        }
        retiredIds.push(id);
        continue;
      }
      issues.push(issue("E-TRANSACTION-OWNER", `owner[${JSON.stringify(id)}]`, "base lifecycle owner disappeared or changed kind without one complete transfer/retirement outcome"));
    }
  }

  if (!bootstrap && candidate.retired !== undefined && base.retired !== undefined) {
    for (const id of candidate.retired.entries.keys()) {
      if (base.retired.entries.has(id)) continue;
      const origin = base.identity.owners.get(id);
      const kind = origin === undefined ? undefined : firstClassOrigin(base, origin);
      if (kind !== "active_record" && kind !== "current_guard" && kind !== "legacy_markdown_reservation") {
        issues.push(issue("E-TRANSACTION-ORIGIN", `retired_ids.entry[${JSON.stringify(id)}]`, "new tombstone has no exact eligible base owner"));
      }
    }
  }

  for (const roadmap of ["matrix", "testing"] as const) {
    const baseDocument = base.roadmaps[roadmap].document;
    const candidateDocument = candidate.roadmaps[roadmap].document;
    const baseDebt = base.debt[roadmap];
    const candidateDebt = candidate.debt[roadmap];
    const relevant = baseDocument !== undefined && candidateDocument !== undefined &&
      (baseDocument.document.schema_version !== 0 || candidateDocument.document.schema_version !== 0);
    if (!relevant) continue;
    if (baseDebt === undefined || candidateDebt === undefined) {
      issues.push(issue(
        "E-TRANSACTION-BASE",
        `debt.${roadmap}`,
        `relevant ${roadmap} base/candidate documents require both complete migration-debt views`,
      ));
      continue;
    }
    issues.push(...validateSemanticConversionTransition(baseDocument, candidateDocument));
    issues.push(...validateProjectionLayoutTransition(baseDocument, candidateDocument));
    if (projectionLayoutRank(projectionLayout(candidateDocument)) >
      projectionLayoutRank(projectionLayout(baseDocument)) &&
      (!exactProjectionLayoutPromotion(baseDocument, candidateDocument) ||
        !completedBytesEqual(base.completed[roadmap], candidate.completed[roadmap]))) {
      issues.push(issue(
        "E-TRANSACTION-BASE",
        `${roadmap}.document.projection_layout`,
        "projection-layout promotion must be one adjacent projection-only stage; only standing_v1 to unnumbered_v1 permits exact Next-heading reference retargets over byte-identical render IR",
      ));
    }
    if (semanticConversionState(candidateDocument).effective === "complete") {
      const candidateCompleted = candidate.completed[roadmap];
      if (candidateCompleted === undefined) {
        issues.push(issue(
          "E-TRANSACTION-BASE",
          `${roadmap}.completed_render_ir`,
          "semantic_conversion = complete requires a complete candidate render IR audit",
        ));
      } else {
        issues.push(...validateSemanticConversionCompletion(
          candidateDocument,
          candidateDebt,
          candidateCompleted,
        ));
      }
    }
    const facts: ValidatedDebtTransitionFacts[] = [];
    let factsSupplied = false;
    const conversion = validateSemanticConversionFacts(baseDebt, candidateDebt, {
      base_document: baseDocument,
      candidate_document: candidateDocument,
      ...(base.completed[roadmap] === undefined ? {} : { base_completed: base.completed[roadmap] }),
      ...(candidate.completed[roadmap] === undefined ? {} : { candidate_completed: candidate.completed[roadmap] }),
    });
    if (!conversion.ok) issues.push(...conversion.issues);
    else if (conversion.facts !== undefined) {
      factsSupplied = true;
      facts.push(conversion.facts);
    }
    if (lifecycleReady) {
      const retirementRequests = (activeRetirements.get(roadmap) ?? []).flatMap((id) => {
        const baseOwner = activeOwnerFact(base, id);
        const tombstone = candidate.retired?.entries.get(id);
        if (baseOwner === undefined || tombstone === undefined) return [];
        const candidateTombstone = {
          owner_kind: "tombstone" as const,
          id,
          namespace: roadmap,
          tombstone,
        };
        return [{
          base_owner: baseOwner,
          removed_debt_owners: [...baseDebt.owners.values()].filter(({ key }) =>
            key.roadmap === roadmap && key.owner_kind === "record" && key.owner_id === id &&
            !candidateDebt.owners.has(debtOwnerIndex(key))
          ).map(({ key }) => key),
          base_commit: inputs.against,
          base_source: sourceFingerprint(baseDocument),
          candidate_source: sourceFingerprint(candidateDocument),
          candidate_identity_facts: [candidateTombstone],
          candidate_tombstone: candidateTombstone,
          candidate_replacement_fact: exactReplacementFact(tombstone.replacement, candidate.registry),
        }];
      });
      if (retirementRequests.length > 0) {
        factsSupplied = true;
        const retirement = validateDebtRetirementFacts(
          baseDebt,
          candidateDebt,
          { base_document: baseDocument, candidate_document: candidateDocument },
          retirementRequests,
        );
        if (retirement.ok) facts.push(retirement.facts);
        else issues.push(...retirement.issues);
      }
      const guardRequests = (familyGuardTransferIds.get(roadmap) ?? []).flatMap((id) => {
        const baseOwner = activeOwnerFact(base, id);
        const guard = guardFor(candidate, id);
        return baseOwner === undefined || guard === undefined
          ? []
          : [{
            base_owner: baseOwner,
            candidate_guard: guardOwnerFact(guard),
            candidate_guards: candidate.registry.current_guards,
            candidate_replacement_facts: replacementFacts(candidate.registry),
          }];
      });
      if (guardRequests.length > 0) {
        factsSupplied = true;
        const guarded = validateDebtGuardTransferFacts(
          baseDebt,
          candidateDebt,
          { base_document: baseDocument, candidate_document: candidateDocument },
          guardRequests,
        );
        if (guarded.ok) facts.push(guarded.facts);
        else issues.push(...guarded.issues);
      }
    }
    issues.push(...compareMigrationDebt(baseDebt, candidateDebt, {
      base_document: baseDocument,
      candidate_document: candidateDocument,
      ...(base.completed[roadmap] === undefined ? {} : { base_completed: base.completed[roadmap] }),
      ...(candidate.completed[roadmap] === undefined ? {} : { candidate_completed: candidate.completed[roadmap] }),
      ...(factsSupplied ? { transition_facts: Object.freeze(facts) } : {}),
    }));
  }

  retiredIds.sort(codePointSort);
  authorityTransfers.sort(codePointSort);
  guardTransfers.sort(codePointSort);
  return Object.freeze({
    issues: sortIssues(issues),
    base,
    candidate,
    retired_ids: Object.freeze(retiredIds),
    authority_transfers: Object.freeze(authorityTransfers),
    guard_transfers: Object.freeze(guardTransfers),
  });
}

export function validateTransaction(inputs: TransactionValidationInputs): TransactionValidationResult {
  return inputs.scope === "all" ? validateAll(inputs) : validateScoped(inputs);
}

/** Exact universe used by tombstone-origin review and self-test enumeration. */
export const TOMBSTONE_ELIGIBLE_BASE_OWNER_KINDS: readonly TombstoneEligibleBaseOwner[] = Object.freeze([
  "active_record",
  "current_guard",
  "legacy_markdown_reservation",
]);

export const TOMBSTONE_INELIGIBLE_ORIGIN_LABELS = Object.freeze([
  "shadow_only",
  "active_family",
  "alias",
  "selection",
  "preexisting_tombstone",
] as const);

export function replacementPinsEqual(left: ReplacementPin, right: ReplacementPin): boolean {
  return pinEqual(left, right);
}

export function transactionHasDanglingEdge(revision: ValidatedLifecycleRevision, id: RoadmapId): boolean {
  return candidateEdgesContain(revision, id);
}
