import {
  migrationProgressReport,
  type MigrationDebt,
  type MigrationProgressBlocker,
} from "./debt.ts";
import type { RoadmapIssue } from "./errors.ts";
import type { RoadmapDocument } from "./model/documents.ts";
import type { CompletedRenderIr } from "./render_ir.ts";

export type SemanticConversionState = "converting" | "complete";
export type DeclaredSemanticConversionState = SemanticConversionState | "omitted" | "intrinsic";

interface DocumentWithSemanticConversion {
  readonly semantic_conversion?: SemanticConversionState;
}

function authoredValue(document: RoadmapDocument): SemanticConversionState | undefined {
  return (document.document as DocumentWithSemanticConversion).semantic_conversion;
}

/** Historical v1 sources predate the declaration and are interpreted as converting, never complete. */
export function semanticConversionState(document: RoadmapDocument): Readonly<{
  declared: DeclaredSemanticConversionState;
  effective: SemanticConversionState | "not_applicable";
}> {
  if (document.document.schema_version === 0) {
    return Object.freeze({ declared: "omitted", effective: "not_applicable" });
  }
  if (document.document.schema_version === 2) {
    return Object.freeze({ declared: "intrinsic", effective: "complete" });
  }
  const declared = authoredValue(document);
  return Object.freeze({
    declared: declared ?? "omitted",
    effective: declared ?? "converting",
  });
}

export function semanticConversionTransitionSignature(document: RoadmapDocument): string {
  const state = semanticConversionState(document);
  return JSON.stringify([state.declared, state.effective]);
}

function schemaIssue(document: RoadmapDocument, logical_path: string, message: string): RoadmapIssue {
  return {
    code: "E-SCHEMA-STATE",
    source: document.document.source_path,
    logical_path,
    message,
    exit: 1,
  };
}

/** Current commands require an authored declaration; only commit-history readers may omit it. */
export function validateSemanticConversionDeclaration(
  document: RoadmapDocument,
  allowHistoricalV1Omission: boolean,
): readonly RoadmapIssue[] {
  const declared = authoredValue(document);
  if (document.document.schema_version === 0) {
    return declared === undefined
      ? Object.freeze([])
      : Object.freeze([schemaIssue(
        document,
        "document.semantic_conversion",
        "roadmap schema v0 forbids a semantic-conversion declaration",
      )]);
  }
  if (document.document.schema_version === 2) {
    return declared === undefined
      ? Object.freeze([])
      : Object.freeze([schemaIssue(
        document,
        "document.semantic_conversion",
        "roadmap schema v2 has intrinsic completion and forbids migration declarations",
      )]);
  }
  if (declared === undefined && !allowHistoricalV1Omission) {
    return Object.freeze([schemaIssue(
      document,
      "document.semantic_conversion",
      "current roadmap schema v1 requires semantic_conversion = converting or complete",
    )]);
  }
  return Object.freeze([]);
}

export interface SemanticConversionCompletionAudit {
  readonly declared: DeclaredSemanticConversionState;
  readonly effective: SemanticConversionState | "not_applicable";
  readonly blockers: readonly MigrationProgressBlocker[];
  readonly join_blockers: readonly MigrationProgressBlocker[];
}

/**
 * Complete is the irreversible per-roadmap lane declaration. Its blockers intentionally reuse the
 * exported lane-category registry; unresolved cross-roadmap joins remain visible and do not
 * reopen a completed semantic-conversion lane.
 */
export function semanticConversionCompletionAudit(
  document: RoadmapDocument,
  debt: MigrationDebt,
  completed: CompletedRenderIr,
): SemanticConversionCompletionAudit {
  const state = semanticConversionState(document);
  const progress = migrationProgressReport(document, debt, completed);
  return Object.freeze({
    ...state,
    blockers: progress.completion_audit.lane_blockers,
    join_blockers: progress.completion_audit.join_blockers,
  });
}

export function validateSemanticConversionCompletion(
  document: RoadmapDocument,
  debt: MigrationDebt,
  completed: CompletedRenderIr,
): readonly RoadmapIssue[] {
  const audit = semanticConversionCompletionAudit(document, debt, completed);
  if (audit.effective !== "complete" || audit.blockers.length === 0) return Object.freeze([]);
  return Object.freeze(audit.blockers.map((blocker) => schemaIssue(
    document,
    `document.semantic_conversion.blocker[${JSON.stringify(`${blocker.category}:${blocker.subject}`)}]`,
    `semantic_conversion = complete forbids ${blocker.category} blocker ${blocker.subject}`,
  )));
}

export function validateSemanticConversionTransition(
  base: RoadmapDocument,
  candidate: RoadmapDocument,
): readonly RoadmapIssue[] {
  const baseState = semanticConversionState(base);
  const candidateState = semanticConversionState(candidate);
  if (baseState.effective !== "complete" || candidateState.effective === "complete") {
    return Object.freeze([]);
  }
  return Object.freeze([{
    code: "E-TRANSACTION-BASE",
    source: "<transaction>",
    logical_path: `${candidate.document.roadmap}.document.semantic_conversion`,
    message: `semantic conversion is irreversible: base declared=${baseState.declared} effective=${baseState.effective}, candidate declared=${candidateState.declared} effective=${candidateState.effective}`,
    exit: 1,
  }]);
}
