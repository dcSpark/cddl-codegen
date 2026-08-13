/**
 * The validation pipeline every command runs before it may render, write, compare, or report:
 * decode a revision's source, then take one decoded document through the ordered stages below.
 *
 * The stage ORDER is load-bearing and is therefore data here rather than a comment beside a long
 * function -- several stages exist only because an earlier one already established its authority
 * (expected bytes precede the projection scan; the projection scan precedes domain validation, so
 * references into the projection resolve against the bytes this source just produced, never
 * against the committed file, which is prior output). Every stage fails closed: the first stage
 * with issues throws, so no later stage ever sees a half-validated document.
 */
import { MATRIX_ADAPTER } from "./adapters/matrix.ts";
import { TESTING_ADAPTER } from "./adapters/testing.ts";
import { validateMatrixRoadmapDocument, validateTestingRoadmapDocument } from "./adapters/validation.ts";
import type { RegistryView, RoadmapAdapter } from "./adapters/types.ts";
import { deriveMigrationDebt, validateMigrationCompletion, type MigrationDebt } from "./debt.ts";
import { decodeRoadmapSource } from "./decode/roadmap.ts";
import {
  RoadmapFailure,
  sortRoadmapIssues,
  type RoadmapIssue,
} from "./errors.ts";
import type { ReadOnlyRoadmapPorts } from "./io.ts";
import { resolveManifest, type ManifestResolution } from "./manifest.ts";
import type {
  FullCommitId,
  RepoPath,
  RepositoryRevision,
  RoadmapName,
} from "./model/core.ts";
import type { RoadmapDocument, SemanticPayload } from "./model/documents.ts";
import { validateProductionOutputRegistry } from "./output_registry.ts";
import { renderValidatedChunks } from "./render.ts";
import {
  buildExpectedChunks,
  createImmutableByteView,
  validateCompletedChunks,
  type CompletedRenderIr,
} from "./render_ir.ts";
import { buildProjectionViews, type ProjectionViews } from "./projection_views.ts";
import { projectionLayout, projectionLayoutRank, validateProjectionLayoutDeclaration } from "./projection_layout.ts";
import { scanRoadmapMarkdownFacts } from "./repository_facts.ts";
import { validateSourceSpans } from "./spans.ts";

export const MATRIX_SOURCE = "cddl-matrix/roadmap.toml" as RepoPath;
export const TESTING_SOURCE = "tests/testing-roadmap.toml" as RepoPath;
export const SOURCE_BY_ROADMAP: Readonly<Record<RoadmapName, RepoPath>> = Object.freeze({
  matrix: MATRIX_SOURCE,
  testing: TESTING_SOURCE,
});
const ADAPTER_BY_ROADMAP: Readonly<Record<RoadmapName, RoadmapAdapter<SemanticPayload>>> = Object.freeze({
  matrix: MATRIX_ADAPTER,
  testing: TESTING_ADAPTER,
});

export interface ValidatedRoadmapCore {
  readonly document: RoadmapDocument;
  readonly registry: RegistryView;
  readonly completed: CompletedRenderIr;
  readonly debt: MigrationDebt;
  readonly projection_views: ProjectionViews;
}

export interface FinalizedRoadmap extends ValidatedRoadmapCore {
  readonly projection: Uint8Array;
}

export interface RevisionReader {
  read(path: RepoPath): Uint8Array;
  registry(): RegistryView;
  revision: RepositoryRevision;
}

export function failure(issues: readonly RoadmapIssue[]): never {
  throw new RoadmapFailure(sortRoadmapIssues(issues));
}

export function issue(
  code: RoadmapIssue["code"],
  source: string,
  logicalPath: string,
  message: string,
  exit: 1 | 2 = 1,
): RoadmapIssue {
  return { code, source, logical_path: logicalPath, message, exit };
}

export function strictSource(bytes: Uint8Array, source: RepoPath): void {
  if (bytes.byteLength === 0) {
    failure([issue("E-SOURCE-EMPTY", source, "$", "declared source is empty")]);
  }
}

export function worktreeReader(ports: ReadOnlyRoadmapPorts): RevisionReader {
  return {
    revision: { kind: "worktree" },
    read: (path) => ports.readDeclared(path),
    registry: () => ports.registryView({ kind: "worktree" }),
  };
}

export function commitReader(ports: ReadOnlyRoadmapPorts, commit: FullCommitId): RevisionReader {
  const revision: RepositoryRevision = { kind: "commit", commit };
  return {
    revision,
    read: (path) => ports.readDeclaredAtCommit(commit, path),
    registry: () => ports.registryView(revision),
  };
}

export function decodeAt(name: RoadmapName, reader: RevisionReader): { document: RoadmapDocument; source: Uint8Array } {
  const path = SOURCE_BY_ROADMAP[name];
  const source = new Uint8Array(reader.read(path));
  strictSource(source, path);
  const document = decodeRoadmapSource(source, path, name, true);
  const layoutIssues = validateProjectionLayoutDeclaration(
    document,
    reader.revision.kind === "commit",
  );
  if (layoutIssues.length > 0) failure(layoutIssues);
  return { document, source };
}

function domainValidation(
  document: RoadmapDocument,
  view: RegistryView,
  options: Parameters<typeof validateMatrixRoadmapDocument>[2] = {},
) {
  return document.document.roadmap === "matrix"
    ? validateMatrixRoadmapDocument(document, view, options)
    : validateTestingRoadmapDocument(document, view, options);
}

/** What a stage has produced so far; every optional field is filled by exactly one stage. */
interface CoreStageState {
  readonly document: RoadmapDocument;
  readonly adapter: RoadmapAdapter<SemanticPayload>;
  /** Replaced once, by the projection-fact stage, with the registry domain validation sees. */
  registry: RegistryView;
  manifest?: ManifestResolution;
  completed?: CompletedRenderIr;
  projection_views?: ProjectionViews;
  debt?: MigrationDebt;
}

interface CoreStage {
  readonly name: string;
  run(state: CoreStageState): void;
}

/** Read a value an earlier stage was supposed to establish; absence is a pipeline-order defect. */
function staged<T>(value: T | undefined, produced_by: string): T {
  if (value === undefined) {
    throw new Error(`roadmap pipeline ran out of order: the ${produced_by} stage has not run yet`);
  }
  return value;
}

const CORE_PIPELINE: readonly CoreStage[] = Object.freeze([
  {
    name: "resolve-manifest",
    run(state) {
      state.manifest = resolveManifest(state.document);
    },
  },
  {
    name: "build-expected-bytes",
    run(state) {
      const registry = state.registry;
      const adapter = state.adapter;
      const resolvers = adapter.slotResolvers(registry, state.document);
      state.completed = buildExpectedChunks(state.document, staged(state.manifest, "resolve-manifest").ops, {
        renderSemanticRecord(record, fields) {
          return adapter.renderSemantic(record, fields);
        },
        resolveGeneratedSlot(slot) {
          return resolvers.get(slot.slot_id)?.resolve(slot, registry);
        },
      });
    },
  },
  {
    name: "validate-expected-chunks",
    run(state) {
      const manifest = staged(state.manifest, "resolve-manifest");
      const renderIssues = [
        ...manifest.issues,
        ...validateCompletedChunks(state.document, manifest.ops, staged(state.completed, "build-expected-bytes")),
      ];
      if (renderIssues.length > 0) failure(renderIssues);
    },
  },
  {
    name: "build-projection-views",
    run(state) {
      const completed = staged(state.completed, "build-expected-bytes");
      const projectionViews = buildProjectionViews(
        state.document,
        completed,
        renderValidatedChunks(completed.chunks, [], completed.expected_bytes),
      );
      if (projectionViews.issues.length > 0) failure(projectionViews.issues);
      state.projection_views = projectionViews;
    },
  },
  {
    name: "validate-projection-anchors",
    run(state) {
      validateProjectionAnchors(state.document, staged(state.projection_views, "build-projection-views").full);
    },
  },
  {
    // Authoritative references to the roadmap projection resolve against the projection this
    // decoded source just built. The committed projection is prior output and therefore cannot be
    // an input to domain validation. Build/slot resolution deliberately precedes this scan because
    // expected bytes are its authority; neither depends on a successful domain verdict.
    name: "domain-validation",
    run(state) {
      state.registry = registryWithRoadmapMarkdownFact(
        state.registry,
        state.adapter.projection_path,
        createImmutableByteView(staged(state.projection_views, "build-projection-views").full),
      );
      const domain = domainValidation(state.document, state.registry, {
        defer_foreign_roadmap_joins: true,
      });
      const structuralIssues = [...domain.issues];
      if (structuralIssues.length > 0) failure(structuralIssues);
    },
  },
  {
    name: "validate-source-spans",
    run(state) {
      const spanIssues = validateSourceSpans({
        document: state.document,
        completed: staged(state.completed, "build-expected-bytes"),
      });
      if (spanIssues.length > 0) failure(spanIssues);
    },
  },
  {
    name: "derive-migration-debt",
    run(state) {
      const completed = staged(state.completed, "build-expected-bytes");
      const debt = deriveMigrationDebt(state.document, completed);
      const completionIssues = validateMigrationCompletion(state.document, debt, completed);
      if (completionIssues.length > 0) failure(completionIssues);
      state.debt = debt;
    },
  },
  {
    name: "validate-output-authority",
    run(state) {
      const productionOutput = validateProductionOutputRegistry(
        state.registry.output_claims,
        state.registry.production_output_stage,
      );
      if (!productionOutput.ok) failure(productionOutput.issues);
      const projectionIsOwned = productionOutput.claims.some((claim) =>
        claim.kind === "whole_file" && claim.path === state.document.document.projection_path
      );
      if (!projectionIsOwned) {
        failure([issue(
          "E-OUTPUT-AUTHORITY",
          state.document.document.source_path,
          "document.authority",
          "authoritative roadmap requires its same-revision production whole-file projection claim",
        )]);
      }
    },
  },
]);

export function prepareRoadmapCore(name: RoadmapName, reader: RevisionReader): ValidatedRoadmapCore {
  const { document } = decodeAt(name, reader);
  return prepareDecodedRoadmapCore(name, document, reader.registry());
}

export function prepareDecodedRoadmapCore(
  name: RoadmapName,
  document: RoadmapDocument,
  registry: RegistryView,
): ValidatedRoadmapCore {
  const state: CoreStageState = { document, adapter: ADAPTER_BY_ROADMAP[name], registry };
  for (const stage of CORE_PIPELINE) stage.run(state);
  return Object.freeze({
    document,
    registry: state.registry,
    completed: staged(state.completed, "build-expected-bytes"),
    debt: staged(state.debt, "derive-migration-debt"),
    projection_views: staged(state.projection_views, "build-projection-views"),
  });
}

export function finalizeRoadmap(core: ValidatedRoadmapCore): FinalizedRoadmap {
  return Object.freeze({
    ...core,
    projection: new Uint8Array(core.projection_views.full),
  });
}

export function registryWithRoadmapMarkdownFact(
  registry: RegistryView,
  path: RepoPath,
  markdown: Parameters<typeof scanRoadmapMarkdownFacts>[1],
): RegistryView {
  const facts = scanRoadmapMarkdownFacts(path, markdown);
  if (facts.issues.length > 0) failure(facts.issues);
  // Replace, rather than append, this projection's facts. Besides keeping revalidation
  // idempotent, this fails closed if an injected registry ever contains stale prior-projection
  // facts: only the selected immutable view can describe the current projection path.
  const citations = [
    ...registry.roadmap_citations.filter((value) => value.source !== path),
    ...facts.citations,
  ].sort((left, right) =>
    left.source < right.source ? -1 : left.source > right.source ? 1 :
      left.span.start_byte - right.span.start_byte || left.span.end_byte - right.span.end_byte ||
      (left.id < right.id ? -1 : left.id > right.id ? 1 : 0)
  );
  const headings = [
    ...registry.tracked_headings.filter((value) => value.path !== path),
    ...facts.headings,
  ].sort((left, right) =>
    left.path < right.path ? -1 : left.path > right.path ? 1 :
      left.span.start_byte - right.span.start_byte || (left.heading < right.heading ? -1 : left.heading > right.heading ? 1 : 0)
  );
  return Object.freeze({
    ...registry,
    tracked_headings: Object.freeze(headings),
    roadmap_citations: Object.freeze(citations),
  });
}

function validateProjectionAnchors(document: RoadmapDocument, projection: Uint8Array): void {
  if (projectionLayoutRank(projectionLayout(document)) < 1) return;
  const facts = scanRoadmapMarkdownFacts(document.document.projection_path, createImmutableByteView(projection));
  if (facts.issues.length > 0) failure(facts.issues);
  const expected = document.records.flatMap((record) =>
    "projection_visibility" in record && record.projection_visibility === "document" ? [record.id] : []
  ).sort();
  if (JSON.stringify(facts.stable_anchor_ids) !== JSON.stringify(expected)) failure([issue(
    "E-ID-DUPLICATE",
    document.document.projection_path,
    "roadmap-anchor",
    `stable anchor inventory must exactly equal document-visible record IDs (expected=${expected.length}, actual=${facts.stable_anchor_ids.length})`,
  )]);
}
