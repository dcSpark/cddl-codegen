import type { RegistryView } from "./adapters/types.ts";
import { composeRoadmapDocument } from "./compose.ts";
import {
  compareMigrationDebt,
  validateMigrationCompletion,
  validateRecordOwnerTransition,
  type MigrationDebt,
} from "./debt.ts";
import type { RoadmapIssue } from "./errors.ts";
import type { GlobalIdentityResult, GlobalOwnerClaim } from "./identity.ts";
import { namespaceOf } from "./ids.ts";
import type { FullCommitId, RoadmapId, RoadmapName } from "./model/core.ts";
import type { Reference, RoadmapDocument, RoadmapDocumentV2 } from "./model/documents.ts";
import type { CompletedRenderIr } from "./render_ir.ts";
import { projectionLayout, projectionLayoutRank, validateProjectionLayoutTransition } from "./projection_layout.ts";
import { bytesEqual, codePointSort } from "./kernel.ts";
import { sortRoadmapIssues as sortIssues } from "./errors.ts";

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

export type TransactionValidationInputs = ScopedRoadmapTransactionInputs;

export interface TransactionValidationResult {
  readonly issues: readonly RoadmapIssue[];
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

function allowedProjectionHeadingRetarget(base: Reference, candidate: Reference): boolean {
  return base.kind === "file_heading" && candidate.kind === "file_heading" &&
    base.id === candidate.id && base.source === candidate.source && base.path === candidate.path &&
    base.heading === "Next work items, in priority order" && candidate.heading === "Next work";
}

/** The layout flip is projection-only, except for the exact heading citations it invalidates. */
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

function validCommit(value: string): boolean {
  return /^[0-9a-f]{40}(?:[0-9a-f]{24})?$/.test(value);
}


function ownerKind(claim: GlobalOwnerClaim): string {
  return claim.owner_kind;
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
    base.document.document.roadmap !== inputs.scope ||
    inputs.candidate_document.document.roadmap !== inputs.scope
  ) {
    issues.push(issue("E-TRANSACTION-BASE", inputs.scope, "single-roadmap comparison requires both sides to be the selected authoritative roadmap"));
  } else {
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
    if (inputs.candidate_completed === undefined) {
      issues.push(issue(
        "E-TRANSACTION-BASE",
        `${inputs.scope}.completed_render_ir`,
        "intrinsic migration completion requires a complete candidate render IR audit",
      ));
    } else {
      issues.push(...validateMigrationCompletion(
        inputs.candidate_document,
        inputs.candidate_debt,
        inputs.candidate_completed,
      ));
    }
    issues.push(...validateRecordOwnerTransition({
      base_document: base.document,
      candidate_document: inputs.candidate_document,
    }));
    issues.push(...compareMigrationDebt(base.debt, inputs.candidate_debt, {
      base_document: base.document,
      candidate_document: inputs.candidate_document,
      ...(base.completed === undefined ? {} : { base_completed: base.completed }),
      ...(inputs.candidate_completed === undefined ? {} : { candidate_completed: inputs.candidate_completed }),
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
  return Object.freeze({ issues: sortIssues(issues) });
}

export function validateTransaction(inputs: TransactionValidationInputs): TransactionValidationResult {
  return validateScoped(inputs);
}
