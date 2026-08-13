import type { RepoPath, RoadmapName } from "./model/core.ts";

/**
 * The one declaration of where roadmap projections render. The projections are DERIVED,
 * gitignored human-review artifacts (the `/draft/` gitignore rule covers them): the TOML sources
 * are the only authority, `--check` renders in memory, and `--write` refreshes these files.
 *
 * Everything that names a projection path — the adapters' floor specs, the production output
 * inventory, the write plan's target authorization, and the two reference-rule exemptions below —
 * imports from here, so the path cannot fork across layers.
 */
export const PROJECTION_PATH_BY_ROADMAP: Readonly<Record<RoadmapName, RepoPath>> = Object.freeze({
  matrix: "draft/roadmaps/matrix-roadmap.md" as RepoPath,
  testing: "draft/roadmaps/testing-roadmap.md" as RepoPath,
});

/**
 * Whether a draft/ path is one of the two roadmap projections. The draft/ reference ban exists
 * because gitignored files cannot provide durable evidence; a `file_heading` reference into a
 * roadmap's own projection is the one exception, because it never resolves against the disk file
 * at all — the pipeline injects the freshly rendered projection's heading facts, so the referent
 * is derived deterministically from committed sources.
 */
export function isRoadmapProjectionPath(path: string): boolean {
  return path === PROJECTION_PATH_BY_ROADMAP.matrix || path === PROJECTION_PATH_BY_ROADMAP.testing;
}
