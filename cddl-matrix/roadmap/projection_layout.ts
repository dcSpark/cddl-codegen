import type { RoadmapIssue } from "./errors.ts";
import type { RoadmapDocument } from "./model/documents.ts";

export type ProjectionLayout = "legacy_v1" | "anchors_v1" | "standing_v1" | "unnumbered_v1" | "curated_v1";
export const PROJECTION_LAYOUTS: readonly ProjectionLayout[] = Object.freeze([
  "legacy_v1", "anchors_v1", "standing_v1", "unnumbered_v1", "curated_v1",
]);

export function projectionLayoutRank(layout: ProjectionLayout): number {
  return PROJECTION_LAYOUTS.indexOf(layout);
}

/** Omitted schema-v2 metadata means the historical byte layout, never the newest layout. */
export function projectionLayout(document: RoadmapDocument): ProjectionLayout {
  return document.document.schema_version === 2
    ? document.document.projection_layout ?? "legacy_v1"
    : "legacy_v1";
}

function schemaIssue(document: RoadmapDocument, message: string): RoadmapIssue {
  return {
    code: "E-SCHEMA-STATE",
    source: document.document.source_path,
    logical_path: "document.projection_layout",
    message,
    exit: 1,
  };
}

/** Current v2 sources choose a layout explicitly; historical commit readers retain omission. */
export function validateProjectionLayoutDeclaration(
  document: RoadmapDocument,
  allowHistoricalV2Omission: boolean,
): readonly RoadmapIssue[] {
  if (document.document.schema_version !== 2) return Object.freeze([]);
  if (document.document.projection_layout !== undefined || allowHistoricalV2Omission) {
    return Object.freeze([]);
  }
  return Object.freeze([schemaIssue(
    document,
    "current roadmap schema v2 requires an explicit projection_layout; only historical commit sources may omit it",
  )]);
}

/** A curated projection cannot silently regress to the legacy layout in a later revision. */
export function validateProjectionLayoutTransition(
  base: RoadmapDocument,
  candidate: RoadmapDocument,
): readonly RoadmapIssue[] {
  if (base.document.schema_version !== 2 || candidate.document.schema_version !== 2 ||
    projectionLayoutRank(projectionLayout(candidate)) >= projectionLayoutRank(projectionLayout(base))) {
    return Object.freeze([]);
  }
  return Object.freeze([{
    code: "E-TRANSACTION-BASE",
    source: "<transaction>",
    logical_path: `${candidate.document.roadmap}.document.projection_layout`,
    message: `projection layout is irreversible: ${projectionLayout(base)} cannot regress to ${projectionLayout(candidate)}`,
    exit: 1,
  }]);
}
