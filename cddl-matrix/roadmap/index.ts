import { migrationCompletionAudit, migrationDebtReport } from "./debt.ts";
import { parseRoadmapCli, ROADMAP_CLI_USAGE, RoadmapCliParseError } from "./cli.ts";
import { composeCanonicalDocument } from "./compose.ts";
import { decodeRoadmapSource } from "./decode/roadmap.ts";
import {
  failureFromUnknown,
  isRoadmapFailure,
  renderRoadmapIssues,
  sortRoadmapIssues,
  type RoadmapIssue,
} from "./errors.ts";
import { validateGlobalIdentity } from "./identity.ts";
import { buildRoadmapIndexes } from "./indexes.ts";
import {
  type RoadmapCliPorts,
  type RoadmapWritePorts,
  type ReadOnlyRoadmapPorts,
} from "./io.ts";
import { RoadmapWireError } from "./markdown_codec.ts";
import type {
  AsOfDate,
  CliRequest,
  FullCommitId,
  QueryView,
  RepoPath,
  RoadmapName,
  RoadmapSelection,
} from "./model/core.ts";
import type { RoadmapDocument } from "./model/documents.ts";
import {
  resolveOutputClaims,
  validateProductionOutputRegistry,
} from "./output_registry.ts";
import {
  checkCommittedProjectionBytes,
  renderThenCheckCommittedProjection,
  RenderValidationError,
} from "./render.ts";
import {
  commitReader,
  failure,
  finalizeRoadmap,
  issue,
  MATRIX_SOURCE,
  prepareDecodedRoadmapCore,
  prepareRoadmapCore,
  strictSource,
  TESTING_SOURCE,
  worktreeReader,
  type FinalizedRoadmap,
  type ValidatedRoadmapCore,
} from "./pipeline.ts";
import { queryText, queryValue, stableJsonValue } from "./query.ts";
import { validateProjectionLayoutDeclaration } from "./projection_layout.ts";
// Type-only: the selftest runner VALUE is injected by the entry point through
// RoadmapCliDispatchServices, so this module has no runtime edge into the selftest tree.
import type { runSelfTests } from "./selftest.ts";
import { validateTransaction } from "./transaction.ts";
import { applyProjectionWritePlan, createProjectionWritePlan } from "./write_plan.ts";
import { sha256 } from "./kernel.ts";

export { createNodeRoadmapCliPorts } from "./io.ts";

const UTF8 = new TextEncoder();

export interface RoadmapCliResult {
  exit_code: 0 | 1 | 2;
  stdout: Uint8Array;
  stderr: Uint8Array;
}

function selectedRoadmaps(selection: RoadmapSelection): readonly RoadmapName[] {
  return selection === "all" ? ["matrix", "testing"] : [selection];
}

function roadmapReceipt(prepared: FinalizedRoadmap): string {
  const debt = migrationDebtReport(prepared.debt);
  const independent = Object.values(debt.independent_counts).reduce((sum, value) => sum + value, 0);
  const owners = Object.values(debt.owner_counts).reduce((sum, value) => sum + value, 0);
  const projectionOwner = prepared.registry.output_claims.find((claim) =>
    claim.kind === "whole_file" && claim.path === prepared.document.document.projection_path
  )?.producer ?? "unclaimed";
  const completion = migrationCompletionAudit(
    prepared.document,
    prepared.debt,
    prepared.completed,
  );
  return `source=${prepared.document.document.source_path} schema=${prepared.document.document.schema_version} authority=${prepared.document.document.authority} semantic_conversion_declared=${completion.declared} semantic_conversion_effective=${completion.effective} completion_blockers=${completion.blockers.length} join_blockers=${completion.join_blockers.length} projection_bytes=${prepared.projection.byteLength} projection_sha256=${sha256(prepared.projection)} manifest=${prepared.document.manifest.length} spans=${prepared.document.spans.length} debt_owners=${owners} debt_independent=${independent} output_claims=${prepared.registry.output_claims.length} projection_owner=${projectionOwner}`;
}

function success(stdout: string | Uint8Array): RoadmapCliResult {
  return { exit_code: 0, stdout: typeof stdout === "string" ? UTF8.encode(stdout) : stdout, stderr: new Uint8Array() };
}

function checkRoadmaps(
  selection: RoadmapSelection,
  ports: ReadOnlyRoadmapPorts,
): RoadmapCliResult {
  const receipts: string[] = [];
  for (const name of selectedRoadmaps(selection)) {
    const core = prepareRoadmapCore(name, worktreeReader(ports));
    receipts.push(roadmapReceipt(checkCommittedProjection(core, ports)));
  }
  return success(`CHECK OK\n${receipts.join("\n")}\n`);
}

function checkCommittedProjection(
  core: ValidatedRoadmapCore,
  ports: ReadOnlyRoadmapPorts,
): FinalizedRoadmap {
  let checked: ReturnType<typeof renderThenCheckCommittedProjection>;
  try {
    checked = checkCommittedProjectionBytes(
      core.projection_views.full,
      core.document.document.projection_path,
      () => ports.readDeclared(core.document.document.projection_path),
    );
  } catch (error) {
    if (isRoadmapFailure(error)) {
      failure(error.issues.map((value) => value.code === "E-SOURCE-MISSING"
        ? { ...value, code: "E-PROJECTION-MISSING" as const, source: core.document.document.projection_path }
        : value));
    }
    throw error;
  }
  if (checked.issues.length > 0) failure(checked.issues);
  return Object.freeze({ ...core, projection: checked.expected });
}

function queryRoadmaps(
  selection: RoadmapSelection,
  view: QueryView,
  asOf: AsOfDate | undefined,
  ports: ReadOnlyRoadmapPorts,
  json = false,
): Uint8Array {
  const prepared = selectedRoadmaps(selection).map((name) => {
    return finalizeRoadmap(prepareRoadmapCore(name, worktreeReader(ports)));
  });
  const value = stableJsonValue(queryValue(prepared, view, asOf));
  return json ? UTF8.encode(`${JSON.stringify(value)}\n`) : queryText(value);
}

function identityFor(prepared: ValidatedRoadmapCore) {
  const indexes = buildRoadmapIndexes(prepared.document).indexes;
  return validateGlobalIdentity({
    documents: [indexes.identity_inputs],
    current_guards: prepared.registry.current_guards,
  });
}

/**
 * The candidate side of a scoped comparison: the selected roadmap's own identity universe over the
 * worktree registry. There is no cross-roadmap lifecycle root, so this is the complete authority.
 */
function scopedCandidateIdentity(prepared: ValidatedRoadmapCore) {
  return { registry: prepared.registry, identity: identityFor(prepared) };
}

function validateOneRoadmapChange(
  selection: RoadmapName,
  against: FullCommitId,
  ports: ReadOnlyRoadmapPorts,
): readonly RoadmapIssue[] {
  const candidate = prepareRoadmapCore(selection, worktreeReader(ports));
  const candidateGlobal = scopedCandidateIdentity(candidate);
  const baseReader = commitReader(ports, against);
  const result = validateTransaction({
    scope: selection,
    against,
    load_base: () => {
      const base = prepareRoadmapCore(selection, baseReader);
      return {
        document: base.document,
        debt: base.debt,
        completed: base.completed,
        identity: identityFor(base),
        registry: base.registry,
      };
    },
    candidate_document: candidate.document,
    candidate_debt: candidate.debt,
    candidate_completed: candidate.completed,
    candidate_registry: candidateGlobal.registry,
    candidate_global_identity: candidateGlobal.identity,
  });
  return result.issues;
}

function validateChange(
  selection: RoadmapSelection,
  against: FullCommitId,
  ports: ReadOnlyRoadmapPorts,
): readonly RoadmapIssue[] {
  return sortRoadmapIssues(selectedRoadmaps(selection).flatMap((name) =>
    validateOneRoadmapChange(name, against, ports)
  ));
}

function formatSource(path: RepoPath, ports: RoadmapWritePorts): RoadmapCliResult {
  const source = new Uint8Array(ports.readDeclared(path));
  strictSource(source, path);
  let document: RoadmapDocument;
  if (path === MATRIX_SOURCE || path === TESTING_SOURCE) {
    const name: RoadmapName = path === MATRIX_SOURCE ? "matrix" : "testing";
    document = decodeRoadmapSource(source, path, name, false);
    const declarationIssues = validateProjectionLayoutDeclaration(document, false);
    if (declarationIssues.length > 0) failure(declarationIssues);
    prepareDecodedRoadmapCore(name, document, ports.registryView({ kind: "worktree" }));
  } else failure([issue("E-CLI-FORMAT-TARGET", "<cli>", "format_source", "format target is not declared", 2)]);
  const canonical = composeCanonicalDocument(document);
  ports.atomicReplace(path, canonical);
  return success(`FORMAT OK source=${path} bytes=${canonical.byteLength} sha256=${sha256(canonical)}\n`);
}

function validateAgainst(candidate: string, ports: ReadOnlyRoadmapPorts): FullCommitId {
  const format = ports.repositoryObjectFormat();
  const length = format === "sha1" ? 40 : 64;
  if (!(new RegExp(`^[0-9a-f]{${length}}$`).test(candidate))) {
    failure([issue(
      "E-GIT-BASE-FORMAT",
      "<git>",
      "against",
      `--against must be exactly ${length} lowercase hexadecimal characters for repository object format ${format}`,
      2,
    )]);
  }
  let resolved: FullCommitId;
  try {
    resolved = ports.resolveFullCommit(candidate);
  } catch (error) {
    if (isRoadmapFailure(error)) throw error;
    failure([issue("E-GIT-BASE-LOOKUP", "<git>", "against", "--against names no commit object with that exact object ID", 2)]);
  }
  if (resolved !== candidate) {
    failure([issue("E-GIT-BASE-LOOKUP", "<git>", "against", "--against names no commit object with that exact object ID", 2)]);
  }
  return resolved;
}

function writeRoadmap(name: RoadmapName, ports: RoadmapWritePorts): RoadmapCliResult {
  const reader = worktreeReader(ports);
  const prepared = prepareRoadmapCore(name, reader);
  const finalized = finalizeRoadmap(prepared);
  const productionOutput = validateProductionOutputRegistry(
    prepared.registry.output_claims,
    prepared.registry.production_output_stage,
  );
  if (!productionOutput.ok) failure(productionOutput.issues);
  const projectionClaim = productionOutput.claims.find((claim) =>
    claim.kind === "whole_file" && claim.path === prepared.document.document.projection_path
  );
  const resolution = projectionClaim === undefined
    ? undefined
    : resolveOutputClaims({
      registry: productionOutput.registry,
      claims: [projectionClaim],
      targets: new Map([[
        prepared.document.document.projection_path,
        ports.readDeclared(prepared.document.document.projection_path),
      ]]),
    });
  const result = createProjectionWritePlan({
    write_coordinate: "projection",
    roadmap: name,
    document: prepared.document,
    projection_bytes: finalized.projection,
    ...(resolution?.authority === undefined ? {} : { output_authority: resolution.authority }),
    validation_issues: resolution?.issues ?? Object.freeze([]),
  });
  if (!result.ok) failure(result.issues);
  applyProjectionWritePlan(result.plan, ports);
  return success(`WRITE OK roadmap=${name} target=${result.plan.target} bytes=${result.plan.bytes.byteLength} sha256=${sha256(result.plan.bytes)}\n`);
}

function resultFromRoadmapCliFailure(error: unknown): RoadmapCliResult {
  let issues: readonly RoadmapIssue[];
  let usage = false;
  if (error instanceof RoadmapCliParseError) {
    issues = [error.issue];
    usage = true;
  } else if (error instanceof RoadmapWireError) {
    issues = [error.issue];
  } else if (error instanceof RenderValidationError) {
    issues = error.issues.length > 0
      ? error.issues
      : [issue("E-RENDER-EMPTY", "<internal>", "render", error.message, 2)];
  } else if (isRoadmapFailure(error)) {
    issues = error.issues;
    usage = issues.some((value) => value.code.startsWith("E-CLI-"));
  } else {
    const converted = failureFromUnknown(error, "runRoadmapCli");
    issues = converted.issues;
  }
  const sorted = sortRoadmapIssues(issues);
  const exit = sorted.some((value) => value.exit === 2) ? 2 : 1;
  const diagnostics = renderRoadmapIssues(sorted);
  const suffix = usage ? UTF8.encode(ROADMAP_CLI_USAGE) : new Uint8Array();
  const stderr = new Uint8Array(diagnostics.byteLength + suffix.byteLength);
  stderr.set(diagnostics);
  stderr.set(suffix, diagnostics.byteLength);
  return {
    exit_code: exit,
    stdout: new Uint8Array(),
    stderr,
  };
}

export interface RoadmapCliDispatchServices {
  readonly run_selftests: typeof runSelfTests;
}

function dispatchRoadmapCliRequest(
  request: CliRequest,
  ports: RoadmapCliPorts,
  services: RoadmapCliDispatchServices,
): RoadmapCliResult {
  switch (request.mode) {
    case "selftest": {
      const result = services.run_selftests(ports.selftest);
      return success(result.stdout);
    }
    case "check": {
      const against = request.against === undefined ? undefined : validateAgainst(request.against, ports.read);
      const selftest = services.run_selftests(ports.selftest);
      if (against !== undefined) {
        const transactionIssues = validateChange(request.roadmap, against, ports.read);
        if (transactionIssues.length > 0) failure(transactionIssues);
      }
      const checked = checkRoadmaps(request.roadmap, ports.read);
      const stdout = new Uint8Array(selftest.stdout.byteLength + checked.stdout.byteLength);
      stdout.set(selftest.stdout);
      stdout.set(checked.stdout, selftest.stdout.byteLength);
      return success(stdout);
    }
    case "write":
      return writeRoadmap(request.roadmap, ports.write);
    case "query":
      return success(queryRoadmaps(request.roadmap, request.view, request.as_of, ports.read, request.json));
    case "format_source":
      return formatSource(request.source_path, ports.write);
  }
}

/** Dispatch one argv vector with an already-constructed narrowed port bundle. */
export function runRoadmapCli(
  argv: readonly string[],
  ports: RoadmapCliPorts,
  services: RoadmapCliDispatchServices,
): RoadmapCliResult {
  try {
    return dispatchRoadmapCliRequest(parseRoadmapCli(argv), ports, services);
  } catch (error) {
    return resultFromRoadmapCliFailure(error);
  }
}
