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
import {
  type RoadmapCliPorts,
  type RoadmapWritePorts,
  type ReadOnlyRoadmapPorts,
} from "./io.ts";
import { RoadmapWireError } from "./markdown_codec.ts";
import type {
  AsOfDate,
  CliRequest,
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
import { RenderValidationError } from "./render.ts";
import {
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
} from "./pipeline.ts";
import { garbageCollectUncitedReferences } from "./references.ts";
import { queryText, queryValue, stableJsonValue } from "./query.ts";
// Type-only: the selftest runner VALUE is injected by the entry point through
// RoadmapCliDispatchServices, so this module has no runtime edge into the selftest tree.
import type { runSelfTests } from "./selftest.ts";
import { applyProjectionWritePlan, createProjectionWritePlan } from "./write_plan.ts";
import { bytesEqual, sha256 } from "./kernel.ts";

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
  const projectionOwner = prepared.registry.output_claims.find((claim) =>
    claim.kind === "whole_file" && claim.path === prepared.document.document.projection_path
  )?.producer ?? "unclaimed";
  return `source=${prepared.document.document.source_path} schema=${prepared.document.document.schema_version} projection_bytes=${prepared.projection.byteLength} projection_sha256=${sha256(prepared.projection)} entries=${prepared.document.sections.reduce((sum, section) => sum + section.entries.length, 0)} output_claims=${prepared.registry.output_claims.length} projection_owner=${projectionOwner}`;
}

function success(stdout: string | Uint8Array): RoadmapCliResult {
  return { exit_code: 0, stdout: typeof stdout === "string" ? UTF8.encode(stdout) : stdout, stderr: new Uint8Array() };
}

/**
 * `--check` validates the TOML sources and renders each projection in memory (anchors, views and
 * output authority included). There is no committed projection to compare against: the markdown
 * render is a gitignored human-review artifact (`--write` refreshes it), so the sources are the
 * only authority and a fresh checkout is check-clean by construction.
 */
function checkRoadmaps(
  selection: RoadmapSelection,
  ports: ReadOnlyRoadmapPorts,
): RoadmapCliResult {
  const receipts: string[] = [];
  for (const name of selectedRoadmaps(selection)) {
    const core = prepareRoadmapCore(name, worktreeReader(ports));
    receipts.push(roadmapReceipt(finalizeRoadmap(core)));
  }
  return success(`CHECK OK\n${receipts.join("\n")}\n`);
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
  return json ? UTF8.encode(`${JSON.stringify(value)}\n`) : queryText(value, view);
}

function formatSource(path: RepoPath, ports: RoadmapWritePorts): RoadmapCliResult {
  const source = new Uint8Array(ports.readDeclared(path));
  strictSource(source, path);
  let document: RoadmapDocument;
  let collected = 0;
  if (path === MATRIX_SOURCE || path === TESTING_SOURCE) {
    const name: RoadmapName = path === MATRIX_SOURCE ? "matrix" : "testing";
    const decoded = decodeRoadmapSource(source, path, name, false);
    const swept = garbageCollectUncitedReferences(decoded);
    document = swept.document;
    collected = swept.collected.length;
    prepareDecodedRoadmapCore(name, document, ports.registryView({ kind: "worktree" }));
  } else failure([issue("E-CLI-FORMAT-TARGET", "<cli>", "format_source", "format target is not declared", 2)]);
  const canonical = composeCanonicalDocument(document);
  ports.atomicReplace(path, canonical);
  // `already_canonical=true` confirms a hand-authored edit needed no reformatting — the receipt
  // used to be indistinguishable from a rewrite (first-authoring-run friction finding).
  return success(`FORMAT OK source=${path} bytes=${canonical.byteLength} sha256=${sha256(canonical)} collected_references=${collected} already_canonical=${bytesEqual(source, canonical)}\n`);
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
  // The whole-file claim resolves against the freshly rendered bytes, never the prior target:
  // the gitignored render may be absent or stale, and either way it is not an input.
  const resolution = projectionClaim === undefined
    ? undefined
    : resolveOutputClaims({
      registry: productionOutput.registry,
      claims: [projectionClaim],
      targets: new Map([[
        prepared.document.document.projection_path,
        finalized.projection,
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
      const selftest = services.run_selftests(ports.selftest);
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
