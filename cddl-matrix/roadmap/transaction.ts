import type { RegistryView } from "./adapters/types.ts";
import {
  campaignIdentityOwners,
  campaignAuthority,
  markdownContainsBytes,
  reservationBoundBytes,
  validateBootstrapShadowOwners,
  validateCampaign,
  validateCampaignTransition,
  workKindOfRecord,
  type CampaignRoadmapSnapshot,
  type CampaignValidationResult,
  type LegacyTitleBindingFact,
} from "./campaign.ts";
import {
  compareMigrationDebt,
  debtOwnerIndex,
  validateDebtGuardTransferFacts,
  validateDebtRetirementFacts,
  type DebtReplacementResolutionFact,
  type MigrationDebt,
  type DebtTransitionFactsInput,
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
  RetiredIdsDocumentV1,
  RoadmapDocument,
  SemanticPayload,
  RecordNode,
  TombstoneEligibleBaseOwner,
} from "./model/documents.ts";
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
  readonly roadmaps: Readonly<Record<RoadmapName, CampaignRoadmapSnapshot>>;
  readonly registry: RegistryView;
  readonly legacy_title_bindings?: readonly LegacyTitleBindingFact[];
  readonly debt: Partial<Readonly<Record<RoadmapName, MigrationDebt>>>;
}

export interface ValidatedLifecycleRevision {
  readonly campaign?: CampaignValidationResult;
  readonly campaign_document?: CampaignDocumentV1;
  readonly retired?: RetiredValidationResult;
  readonly retired_document?: RetiredIdsDocumentV1;
  readonly roadmaps: Readonly<Record<RoadmapName, CampaignRoadmapSnapshot>>;
  readonly registry: RegistryView;
  readonly identity: GlobalIdentityResult;
  readonly debt: Partial<Readonly<Record<RoadmapName, MigrationDebt>>>;
  readonly issues: readonly RoadmapIssue[];
}

export interface ScopedRoadmapTransactionInputs {
  readonly scope: RoadmapName;
  readonly against: FullCommitId;
  readonly load_base: (roadmap: RoadmapName) => ScopedRoadmapBaseFacts;
  readonly candidate_document?: RoadmapDocument;
  readonly candidate_debt: MigrationDebt;
  readonly candidate_registry: RegistryView;
  readonly candidate_global_identity: GlobalIdentityResult;
  readonly debt_transition_facts?: DebtTransitionFactsInput;
}

export interface ScopedRoadmapBaseFacts {
  readonly document?: RoadmapDocument;
  readonly debt: MigrationDebt;
  readonly identity: GlobalIdentityResult;
  readonly registry: RegistryView;
}

export interface AllRoadmapsTransactionInputs {
  readonly scope: "all";
  readonly against: FullCommitId;
  readonly base: LifecycleRevisionInput;
  readonly candidate: LifecycleRevisionInput;
  readonly bootstrap?: boolean;
  readonly debt_transition_facts?: Partial<Readonly<Record<RoadmapName, DebtTransitionFactsInput>>>;
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

function sha256(value: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(value).digest("hex");
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
    if (document?.document.schema_version !== 1 || input.campaign === undefined ||
      campaignAuthority(input.campaign, roadmap) !== "authoritative") return [];
    return [buildRoadmapIndexes(document).indexes.identity_inputs];
  });
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
  const firstCampaign = input.campaign === undefined ? undefined : validateCampaign({
    campaign: input.campaign,
    roadmaps: input.roadmaps,
    legacy_title_bindings: input.legacy_title_bindings,
  });
  const retired = input.retired === undefined ? undefined : validateRetiredIds(input.retired, input.registry);
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
    if (document?.document.schema_version !== 1) continue;
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
  if (candidate.registry.roadmap_citations.some((citation) => citation.id === id)) {
    issues.push(issue("E-TRANSACTION-CITATION", `citation[${JSON.stringify(id)}]`, "retired ID remains cited in a durable tracked repository file"));
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
  if (candidateDocument?.records.some((record) => record.id === id)) {
    issues.push(issue("E-TRANSACTION-OWNER", `owner[${JSON.stringify(id)}].shadow`, "candidate shadow/active declaration survives legacy delivery"));
  }
  if (candidate.campaign_document !== undefined &&
    campaignAuthority(candidate.campaign_document, namespace) === "shadow" &&
    candidateDocument !== undefined &&
    (candidateDocument.document.frozen_source_sha256 !== sha256(candidate.roadmaps[namespace].markdown) ||
      candidateDocument.document.frozen_source_byte_length !== candidate.roadmaps[namespace].markdown.byteLength)) {
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
    base.document?.document.schema_version !== 1 ||
    inputs.candidate_document?.document.schema_version !== 1 ||
    base.document.document.roadmap !== inputs.scope ||
    inputs.candidate_document.document.roadmap !== inputs.scope
  ) {
    issues.push(issue("E-TRANSACTION-BASE", inputs.scope, "single-roadmap comparison requires matching authoritative-v1 selected documents"));
  } else {
    issues.push(...compareMigrationDebt(base.debt, inputs.candidate_debt, {
      base_document: base.document,
      candidate_document: inputs.candidate_document,
      ...(inputs.debt_transition_facts === undefined ? {} : { transition_facts: inputs.debt_transition_facts }),
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
  const familyGuardTransfers = new Set<RoadmapId>();
  if (lifecycleReady) {
    for (const [id, baseClaim] of base.identity.owners) {
      if (firstClassOrigin(base, baseClaim) === "active_family" &&
        candidate.identity.owners.get(id)?.owner_kind === "current_guard") familyGuardTransfers.add(id);
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
      if (candidateClaim !== undefined && ownerKind(candidateClaim) === ownerKind(baseClaim)) continue;
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
      (baseDocument.document.schema_version === 1 || candidateDocument.document.schema_version === 1);
    if (!relevant) continue;
    if (baseDebt === undefined || candidateDebt === undefined) {
      issues.push(issue(
        "E-TRANSACTION-BASE",
        `debt.${roadmap}`,
        `relevant ${roadmap} base/candidate documents require both complete migration-debt views`,
      ));
      continue;
    }
    const facts: ValidatedDebtTransitionFacts[] = [];
    let factsSupplied = false;
    const external = inputs.debt_transition_facts?.[roadmap];
    if (external !== undefined) {
      factsSupplied = true;
      if (Array.isArray(external)) facts.push(...external);
      else facts.push(external as ValidatedDebtTransitionFacts);
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
