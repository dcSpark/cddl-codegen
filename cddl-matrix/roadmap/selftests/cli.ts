import type { RegistryView } from "../adapters/types.ts";
import { documentSlots } from "../slots.ts";
import { ROADMAP_CLI_USAGE } from "../cli.ts";
import { composeRoadmapDocument } from "../compose.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import { RoadmapFailure } from "../errors.ts";
import { codePointSort } from "../kernel.ts";
import type { IssueCode } from "../errors.ts";
import {
  createNodeRoadmapCliPorts,
  runRoadmapCli,
  type RoadmapCliDispatchServices,
  type RoadmapCliResult,
} from "../index.ts";
import type {
  FixtureFilesystemHarnessPorts,
  ReadOnlyRoadmapPorts,
  RoadmapCliPorts,
  RoadmapSelfTestPorts,
  RoadmapWritePorts,
  ScratchGitHarnessPorts,
} from "../io.ts";
import type { FixtureRelativePath, RepoPath, RepositoryRevision, RoadmapId } from "../model/core.ts";
import type { RoadmapDocumentV3, SemanticPayload } from "../model/documents.ts";
import type { MatrixStatusInputs } from "../model/matrix.ts";
import {
  LEGACY_STATUS_OUTPUT_CLAIMS,
  productionOutputInventory,
} from "../output_registry.ts";
import type { SelfTestCandidateCase as SelfTestCase, SelfTestContext, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import { observeSelfTestIssue } from "./observations.ts";
import { resolveSectionPlan } from "../section_plan.ts";
import { buildExpectedChunks } from "../render_ir.ts";
import { buildProjectionViews } from "../projection_views.ts";
import { TESTING_ADAPTER } from "../adapters/testing.ts";
import { PROJECTION_PATH_BY_ROADMAP } from "../projection_paths.ts";
import {
  liveMatrixAuthoritativeSource,
  liveMatrixV3Document,
} from "./live_matrix.ts";
import {
  liveTestingAuthoritativeSource,
  liveTestingV3Document,
} from "./live_testing.ts";

export const REQUIRED_CLI_SELFTEST_CASE_IDS = [
  "cli_selftest_exact",
  "cli_check_each_roadmap",
  "cli_write_each_single_roadmap",
  "cli_write_authoritative_testing",
  "cli_query_each_view",
  "cli_format_declared_source",
  "cli_format_collects_uncited_references",
  "cli_no_args_rejected",
  "cli_unknown_option",
  "cli_missing_value",
  "cli_duplicate_scalar",
  "cli_duplicate_primary_mode",
  "cli_roadmap_required",
  "cli_roadmap_forbidden_on_format",
  "cli_write_all_rejected",
  "cli_json_requires_query",
  "cli_as_of_requires_query",
  "cli_format_target_declared_only",
  "exit_declared_source_enoent_one",
  "exit_declared_source_eacces_two",
  "exit_declared_reference_enoent_one",
  "exit_other_read_io_two",
  "exit_malformed_toml_one",
  "exit_atomic_write_two",
  "exit_internal_fault_two",
  "exit_authority_stage_mismatch_one",
  "parse_error_stable_prefix",
  "query_stdout_payload_only",
  "live_subordinate_lifecycle_dispositions",
  "failure_stdout_empty",
  "success_receipt_nonvacuous",
  "cli_authoritative_fresh_projection_reference_provenance",
  "cli_as_of_valid_leap_day",
  "cli_as_of_invalid_leap_day",
  "cli_as_of_year_zero_rejected",
  "cli_as_of_short_component_rejected",
  "cli_as_of_timestamp_rejected",
  "cli_as_of_whitespace_rejected",
  "query_as_of_due_date_inclusive",
  "query_as_of_valid_through_inclusive",
  "query_as_of_after_valid_through_stale",
  "query_without_as_of_reads_no_clock",
  "query_as_of_does_not_select_git_revision",
  "cli_production_port_factory_smoke",
  "dispatch_capability_narrowing",
  "cli_dashboard_views_operationally_complete",
  "cli_query_index_shape",
] as const;

export type RequiredCliSelfTestCaseId = (typeof REQUIRED_CLI_SELFTEST_CASE_IDS)[number];

const UTF8 = new TextEncoder();
const DECODER = new TextDecoder();
const FIXTURE_ROOT = "cddl-matrix/roadmap/fixtures" as RepoPath;

function pass(polarity: "positive" | "negative", subcases?: readonly string[]): SelfTestResult {
  return { ok: true, polarity, ...(subcases === undefined ? {} : { subcases }) };
}

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

function text(bytes: Uint8Array): string {
  return DECODER.decode(bytes);
}

function roadmapFailure(
  code: IssueCode,
  source: string,
  logicalPath: string,
  message: string,
  exit: 1 | 2,
): RoadmapFailure {
  return new RoadmapFailure({ code, source, logical_path: logicalPath, message, exit });
}

const unusedFixtures: FixtureFilesystemHarnessPorts = {
  enumerateFixtureFiles: () => { throw new Error("unexpected fixture enumeration"); },
  readFixtureFile: () => { throw new Error("unexpected fixture read"); },
  createScratchRepository: () => { throw new Error("unexpected scratch creation"); },
  openScratchRoadmapPorts: () => { throw new Error("unexpected scratch open"); },
  replaceScratchFile: () => { throw new Error("unexpected scratch replace"); },
  removeScratchFile: () => { throw new Error("unexpected scratch remove"); },
  scratchRepositoryPresent: () => false,
  removeScratchRepository: () => { throw new Error("unexpected scratch cleanup"); },
};
const unusedGit: ScratchGitHarnessPorts = {
  runScratchGit: () => { throw new Error("unexpected scratch git"); },
};
const unusedSelftest: RoadmapSelfTestPorts = { fixtures: unusedFixtures, scratch_git: unusedGit };

const fakeSelftestServices: RoadmapCliDispatchServices = {
  run_selftests: (() => ({
    receipt: { categories: [], total: 1 },
    stdout: UTF8.encode("SELFTEST OK fake=1\n"),
  })) as RoadmapCliDispatchServices["run_selftests"],
};

function emptyRegistry(revision: RepositoryRevision): RegistryView {
  return {
    revision,
    production_output_stage: "pre_cutover",
    gates: [],
    matrix_features: [],
    matrix_roles: [],
    matrix_cells: [],
    tracked_headings: [],
    test_symbols: [],
    roadmap_citations: [],
    current_guards: [],
    output_claims: LEGACY_STATUS_OUTPUT_CLAIMS,
    matrix_status_inputs: {} as RegistryView["matrix_status_inputs"],
  };
}

function registryForDocuments(
  revision: RepositoryRevision,
  documents: readonly RoadmapDocumentV3[],
): RegistryView {
  const references = documents.flatMap((document) => document.references);
  const unique = <T>(values: readonly T[], key: (value: T) => string): readonly T[] =>
    [...new Map(values.map((value) => [key(value), value])).values()];
  return {
    ...emptyRegistry(revision),
    gates: unique(references.filter((reference) => reference.kind === "gate").map((reference) => ({
      id: reference.gate_id,
      kind: "cargo" as const,
      stub: false,
    })), (fact) => fact.id),
    matrix_features: unique(
      references.filter((reference) => reference.kind === "matrix_feature").map((reference) => ({ id: reference.feature_id })),
      (fact) => fact.id,
    ),
    matrix_roles: unique(
      references.filter((reference) => reference.kind === "matrix_role").map((reference) => ({ id: reference.role_id })),
      (fact) => fact.id,
    ),
    matrix_cells: unique(
      references.filter((reference) => reference.kind === "matrix_cell").map((reference) => ({ id: reference.cell_id })),
      (fact) => fact.id,
    ),
    tracked_headings: unique(
      references.filter((reference) => reference.kind === "file_heading").map((reference) => ({
        path: reference.path,
        heading: reference.heading,
        span: { start_byte: 0, end_byte: 1 },
      })),
      (fact) => JSON.stringify([fact.path, fact.heading]),
    ),
    test_symbols: unique(
      references.filter((reference) => reference.kind === "test_symbol").map((reference) => ({
        test_id: reference.test_id,
        symbol: reference.symbol,
        source: "src/tests/fixture.rs" as RepoPath,
        span: { start_byte: 0, end_byte: 1 },
        module_path: reference.symbol.split("::").slice(0, -1),
      })),
      (fact) => JSON.stringify([fact.test_id, fact.symbol]),
    ),
  };
}

let cachedLiveRegistry: RegistryView | undefined;

function liveRegistry(revision: RepositoryRevision): RegistryView {
  cachedLiveRegistry ??= registryForDocuments({ kind: "worktree" }, [
      liveMatrixV3Document(),
      liveTestingV3Document(),
    ]);
  return {
    ...cachedLiveRegistry,
    revision,
  };
}

interface FakeOptions {
  read?: (path: RepoPath) => Uint8Array;
  readAtCommit?: (path: RepoPath) => Uint8Array;
  objectFormat?: () => "sha1" | "sha256";
  resolve?: (candidate: string) => string;
  atomic?: (path: RepoPath, bytes: Uint8Array) => void;
  onReadArm?: (arm: "read" | "write") => void;
  onReadPath?: (path: RepoPath) => void;
  registry?: (revision: RepositoryRevision) => RegistryView;
}

function fakePorts(options: FakeOptions = {}): RoadmapCliPorts {
  const readDeclared = (path: RepoPath): Uint8Array => options.read?.(path) ?? (() => {
    throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
  })();
  const base: ReadOnlyRoadmapPorts = {
    readDeclared(path) {
      options.onReadArm?.("read");
      options.onReadPath?.(path);
      return readDeclared(path);
    },
    readDeclaredAtCommit(_commit, path) {
      return options.readAtCommit?.(path) ?? readDeclared(path);
    },
    repositoryObjectFormat: options.objectFormat ?? (() => "sha1"),
    resolveFullCommit(candidate) {
      return (options.resolve?.(candidate) ?? candidate) as import("../model/core.ts").FullCommitId;
    },
    registryView: options.registry ?? emptyRegistry,
  };
  const write: RoadmapWritePorts = {
    ...base,
    readDeclared(path) {
      options.onReadArm?.("write");
      options.onReadPath?.(path);
      return readDeclared(path);
    },
    atomicReplace(path, bytes) {
      options.atomic?.(path, bytes);
    },
  };
  return { read: base, write, selftest: unusedSelftest };
}

function run(argv: readonly string[], ports = fakePorts()): RoadmapCliResult {
  return runRoadmapCli(argv, ports, fakeSelftestServices);
}

function expectedIssue(
  code: IssueCode,
  source: string,
  logical_path: string,
  message: string,
  exit: 1 | 2,
  message_prefix = false,
) {
  return { code, source, logical_path, message, exit, ...(message_prefix ? { message_prefix: true as const } : {}) };
}

function cliIssue(code: IssueCode, index: number, message: string) {
  return expectedIssue(code, "<cli>", `argv[${index}]`, message, 2);
}

function expectFailure(
  argv: readonly string[],
  expected: {
    readonly code: IssueCode;
    readonly source: string;
    readonly logical_path: string;
    readonly message: string;
    readonly exit: 1 | 2;
    readonly message_prefix?: boolean;
  },
  ports = fakePorts(),
  usage = false,
): RoadmapCliResult {
  const result = run(argv, ports);
  const stderr = text(result.stderr);
  assert(result.exit_code === expected.exit, `${argv.join(" ")} returned ${result.exit_code}, expected ${expected.exit}`);
  assert(result.stdout.byteLength === 0, `${argv.join(" ")} wrote failure stdout`);
  const prefix = `FAIL [${expected.code}] ${expected.source}#${expected.logical_path}: ${expected.message}`;
  if (expected.message_prefix) {
    const lines = stderr.split("\n");
    assert(lines[0]!.startsWith(prefix), `${argv.join(" ")} diagnostic prefix drifted: ${stderr}`);
    const suffix = stderr.slice(stderr.indexOf("\n") + 1);
    assert(suffix === `FAILED: 1 issue(s)\n${usage ? ROADMAP_CLI_USAGE : ""}`, `${expected.code} summary/usage boundary drifted: ${stderr}`);
  } else {
    assert(stderr === `${prefix}\nFAILED: 1 issue(s)\n${usage ? ROADMAP_CLI_USAGE : ""}`, `${argv.join(" ")} diagnostic drifted: ${stderr}`);
  }
  observeSelfTestIssue(expected);
  return result;
}

function fixture(context: SelfTestContext, path: string): Uint8Array {
  return context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, path as FixtureRelativePath);
}

function validTestingPorts(context: SelfTestContext, atomic?: FakeOptions["atomic"]): RoadmapCliPorts {
  const generic = validGenericAllPorts(context);
  return fakePorts({
    read: generic.read.readDeclared,
    registry: generic.read.registryView,
    atomic,
  });
}

function renderedLiveTestingProjection(): Uint8Array {
  const document = liveTestingV3Document();
  const plan = resolveSectionPlan(document);
  assert(plan.issues.length === 0, "live testing section plan is invalid");
  const completed = buildExpectedChunks(document, plan.ops, {
    renderSemanticRecord: (record, fields) => TESTING_ADAPTER.renderSemantic(record, fields),
    resolveGeneratedSlot: () => undefined,
  });
  assert(completed.build_issues.length === 0, "live testing render IR failed to build");
  const views = buildProjectionViews(document, completed);
  assert(views.issues.length === 0, "live testing projection views reported issues");
  return new Uint8Array(views.full);
}

const CROSS_ROADMAP_TESTING_EXTERN_TARGET =
  "testing.extern-deps-wasm-boundary-surface-packaging-json-gen" as RoadmapId;

function withCrossRoadmapTestingExternTarget(source: Uint8Array): Uint8Array {
  const document = decodeRoadmapSource(
    source,
    "tests/testing-roadmap.toml" as RepoPath,
    "testing",
    true,
  );
  assert(document.document.schema_version === 3, "cross-roadmap testing target fixture must be v3");
  const base = document as RoadmapDocumentV3;
  if (base.records.some((record) => record.id === CROSS_ROADMAP_TESTING_EXTERN_TARGET)) return source;
  assert(base.sections.length > 0, "cross-roadmap testing target fixture lacks a section");
  return composeRoadmapDocument({
    ...base,
    records: [...base.records, {
      id: CROSS_ROADMAP_TESTING_EXTERN_TARGET,
      title: "Cross-roadmap extern execution owner",
      payload: {
        kind: "work",
        work_state: "ready",
        work_intent: "build_system",
        work_kind: "infrastructure",
        risk: "compile_failure",
        acceptance_md: UTF8.encode("The combined cross-roadmap universe resolves this exact target."),
        priority_rationale_md: UTF8.encode("Fixture-only counterpart for the live matrix delegation."),
      },
    }],
  });
}

function temporalTestingPorts(
  context: SelfTestContext,
  kind: "cadence" | "evidence",
): RoadmapCliPorts {
  const sourcePath = "tests/testing-roadmap.toml" as RepoPath;
  const projectionPath = PROJECTION_PATH_BY_ROADMAP.testing;
  const base = text(fixture(context, "positive/small-testing-v3.toml"))
    .replace("cddl-matrix/roadmap/fixtures/positive/small-testing-v3.toml", sourcePath)
    .replace("cddl-matrix/roadmap/fixtures/positive/small-testing-v3.expected.md", projectionPath);
  // The cadence flavor rides an armed work owner: nested tables are a cadence's only remaining
  // packaging (Phase 4 fold), and armed work is its testing-side home. The armed contract pulls
  // in a live control, whose upstream_issue kind needs no registry-resolved reference.
  const payload = kind === "cadence"
    ? `[record.payload]
kind = "work"
body_md = '''Semantic testing detail.'''
work_state = "armed"
work_intent = "build_system"
work_kind = "infrastructure"
risk = "false_pass_or_red"
control_ids = ["testing.fixture-small-control"]

[record.payload.promotion_trigger]
observer = "operator"
dimension = "count"
action_on_fire_md = '''Act on the fixture trigger.'''

[record.payload.promotion_trigger.predicate]
predicate_kind = "event"
event_md = '''A fixture event fires.'''

[record.payload.cadence]
owner_reference_id = "temporal-owner"
event_source = "fixture-calendar"
period_or_event_md = '''Review on the fixture date.'''
checklist_md = '''Check the decoded cadence.'''
missed_action_md = '''Escalate the missed fixture cadence.'''
due_on = "2025-01-01"

[[record]]
id = "testing.fixture-small-control"
title = "Temporal fixture control"

[record.payload]
kind = "control"
control_kind = "upstream_issue"
control_state = "live"
reference_ids = ["temporal-issue"]
claim_md = '''The fixture issue control stays live.'''
boundary_md = '''It covers only the temporal fixture.'''
`
    : `[record.payload]
kind = "evidence"
body_md = '''Semantic testing detail.'''
evidence_kind = "external_commit"
claim_md = '''The fixture commit is point-in-time evidence.'''
freshness = "as_of"
reference_ids = ["temporal-owner"]
at_commit = "1111111111111111111111111111111111111111"
observed_at = "2025-01-01"
valid_through = "2025-01-01"
unprobed_remainder_md = '''No timeless claim is made.'''

[record.payload.scope]
surfaces = ["fixture"]
`;
  // `String.prototype.replace` silently returns the input when the anchor does not match, which
  // would leave this port serving the unmodified work payload under a temporal case name.
  const spliced = base.replace(
    /\[record\.payload\]\n[\s\S]*$/u,
    payload,
  );
  assert(
    spliced !== base && spliced.includes(payload),
    "temporal payload splice matched nothing: the fixture no longer ends with [record.payload]",
  );
  // The splice appends records/references out of canonical order, so recompose canonically
  // before the extern-target pass (which preserves already-canonical bytes).
  const splicedCanonical = composeRoadmapDocument(decodeRoadmapSource(
    UTF8.encode(spliced + `
[[reference]]
id = "temporal-owner"
source = "testing.fixture-small-semantic"
kind = "external_commit"
repository = "fixture/repository"
commit = "1111111111111111111111111111111111111111"
` + (kind === "cadence"
      ? `
[[reference]]
id = "temporal-issue"
source = "testing.fixture-small-control"
kind = "external_issue"
repository = "fixture/repository"
issue = "17"
`
      : "")),
    sourcePath,
    "testing",
    false,
  ));
  const source = withCrossRoadmapTestingExternTarget(splicedCanonical);
  return fakePorts({
    read(path) {
      if (path === sourcePath) return new Uint8Array(source);
      if (path === "cddl-matrix/roadmap.toml") return liveMatrixAuthoritativeSource();
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    registry(revision) {
      return {
        ...liveRegistry(revision),
        production_output_stage: "both_authoritative",
        output_claims: productionOutputInventory("both_authoritative").claims,
        matrix_status_inputs: liveMatrixStatusInputs(),
      };
    },
  });
}

function liveMatrixStatusInputs(): MatrixStatusInputs {
  const features = [
    ...Array.from({ length: 95 }, (_, index) => ({ id: `r-${index}`, profile: "RFC8610" })),
    { id: "rfc9682", profile: "RFC9682" },
    ...Array.from({ length: 27 }, (_, index) => ({ id: `c-${index}`, profile: "CDDL_CODEGEN" })),
  ];
  const annotations = Array.from({ length: 301 }, (_, index) => ({
    id: index < 93 ? `row-${index}` : `annotation-${index}`,
    status: "supported",
    ...(index === 0 ? { emission: { preserve: { status: "unsupported" } } } : {}),
  }));
  return {
    matrix: {
      annotations,
      features,
      containment_ids: Array.from({ length: 144 }, (_, index) => `containment-${index}`),
      control_operator_ids: Array.from({ length: 37 }, (_, index) => `control-${index}`),
    },
    catalog: { rows: Array.from({ length: 93 }, (_, index) => ({
      id: `row-${index}`,
      vectors: Array.from({ length: index < 22 ? 2 : 1 }, () => ({
        expect: "reject",
        class: "constraint",
      })),
    })) },
    registry: { gates: Array.from({ length: 20 }, (_, index) => ({
      id: `gate-${index}`,
      kind: "cargo",
      ignored_test: `manual-${index}`,
    })) },
    timings: { tiers: [
      { tier: "fast", wall_ms: 1000 },
      { tier: "local", wall_ms: 2000 },
      { tier: "full", wall_ms: 3000 },
    ] },
  };
}

/**
 * The generic all-roadmap ports serve the committed v2 sources, whose references resolve only
 * against the live registry view; an empty view was sufficient only while these ports served a
 * reference-free raw document.
 */
function validGenericAllPorts(_context: SelfTestContext): RoadmapCliPorts {
  return bothAuthoritativePorts();
}

function bothAuthoritativePorts(atomic?: FakeOptions["atomic"]): RoadmapCliPorts {
  return fakePorts({
    read(path) {
      if (path === "cddl-matrix/roadmap.toml") return liveMatrixAuthoritativeSource();
      if (path === "tests/testing-roadmap.toml") return liveTestingAuthoritativeSource();
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    registry(revision) {
      return {
        ...liveRegistry(revision),
        production_output_stage: "both_authoritative",
        output_claims: productionOutputInventory("both_authoritative").claims,
        matrix_status_inputs: liveMatrixStatusInputs(),
      };
    },
    atomic,
  });
}

function uniqueProjectionHeading(projection: Uint8Array): string {
  const headings = [...text(projection).matchAll(/^#{1,6} +(.+?)(?: +#*)?$/gmu)].map((match) => match[1]!);
  const heading = headings.find((candidate) => headings.filter((value) => value === candidate).length === 1);
  assert(heading !== undefined, "projection has no unique Markdown heading for the provenance fixture");
  return heading;
}

function authoritativeProjectionReferencePorts(
  context: SelfTestContext,
  referencedHeading: string,
  priorHeading: string,
  projectionReads: { value: number },
): RoadmapCliPorts {
  const sourcePath = "tests/testing-roadmap.toml" as RepoPath;
  const projectionPath = PROJECTION_PATH_BY_ROADMAP.testing;
  const sourceFixture = text(fixture(context, "positive/small-testing-v3.toml"))
    .replace("cddl-matrix/roadmap/fixtures/positive/small-testing-v3.toml", sourcePath)
    .replace("cddl-matrix/roadmap/fixtures/positive/small-testing-v3.expected.md", projectionPath);
  const sourceBytes = UTF8.encode(
    `${sourceFixture}\n[[reference]]
id = "selftest-fresh-projection-heading"
source = "testing.fixture-small-semantic"
kind = "file_heading"
path = "${projectionPath}"
heading = ${JSON.stringify(referencedHeading)}
`,
  );
  const priorProjection = UTF8.encode(`# ${priorHeading}\n`);
  const priorHeadingBytes = UTF8.encode(priorHeading).byteLength;
  return fakePorts({
    read(path) {
      if (path === sourcePath) return new Uint8Array(sourceBytes);
      if (path === projectionPath) {
        projectionReads.value += 1;
        return new Uint8Array(priorProjection);
      }
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    registry(revision) {
      return {
        ...emptyRegistry(revision),
        production_output_stage: "both_authoritative" as const,
        output_claims: productionOutputInventory("both_authoritative").claims,
        matrix_status_inputs: liveMatrixStatusInputs(),
        // Model a mistakenly injected fact from committed prior output. Preparation must replace
        // every fact for this projection path with the freshly built expected-byte scan.
        tracked_headings: [{
          path: projectionPath,
          heading: priorHeading,
          span: { start_byte: 2, end_byte: 2 + priorHeadingBytes },
        }],
      };
    },
  });
}

function grammarCase(id: RequiredCliSelfTestCaseId, context: SelfTestContext): SelfTestResult | undefined {
  switch (id) {
    case "cli_selftest_exact": {
      const result = run(["--selftest"]);
      assert(result.exit_code === 0 && text(result.stdout) === "SELFTEST OK fake=1\n" && result.stderr.byteLength === 0, "selftest dispatch receipt changed");
      return pass("positive");
    }
    case "cli_check_each_roadmap": {
      const ports = validGenericAllPorts(context);
      for (const roadmap of ["matrix", "testing", "all"]) {
        const result = run(["--check", "--roadmap", roadmap], ports);
        const stdout = text(result.stdout);
        assert(
          result.exit_code === 0 && result.stderr.byteLength === 0 && stdout.includes("CHECK OK\n") &&
            (roadmap === "testing" || stdout.includes("source=cddl-matrix/roadmap.toml")) &&
            (roadmap === "matrix" || stdout.includes("source=tests/testing-roadmap.toml")),
          `${roadmap} check did not succeed over its exact selected roadmap sources: ${text(result.stderr)}`,
        );
      }
      return pass("positive", ["matrix", "testing", "all"]);
    }
    case "cli_write_each_single_roadmap":
      for (const roadmap of ["matrix", "testing"]) {
        const path = roadmap === "matrix" ? "cddl-matrix/roadmap.toml" : "tests/testing-roadmap.toml";
        expectFailure(
          ["--write", "--roadmap", roadmap],
          expectedIssue("E-SOURCE-MISSING", path, "$", "declared source is missing", 1),
        );
      }
      return pass("positive", ["matrix", "testing"]);
    case "cli_write_authoritative_testing": {
      const replacements: { path: RepoPath; bytes: Uint8Array }[] = [];
      const result = run(
        ["--write", "--roadmap", "testing"],
        bothAuthoritativePorts((path, value) => {
          replacements.push({ path, bytes: new Uint8Array(value) });
        }),
      );
      // The oracle is a hand-assembled render over the same pure stages (section plan → expected
      // chunks → projection views), independent of the write path's authority/plan plumbing; the
      // projection itself is gitignored, so there are no committed bytes to compare against.
      const expected = renderedLiveTestingProjection();
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 && replacements.length === 1 &&
          replacements[0].path === PROJECTION_PATH_BY_ROADMAP.testing &&
          replacements[0].bytes.byteLength === expected.byteLength &&
          replacements[0].bytes.every((value, index) => value === expected[index]) &&
          text(result.stdout).startsWith(
            `WRITE OK roadmap=testing target=${PROJECTION_PATH_BY_ROADMAP.testing} bytes=`,
          ),
        `authoritative testing write did not apply the exact rendered whole-file projection: ${text(result.stderr)}`,
      );
      return pass("positive");
    }
    case "cli_query_each_view": {
      for (const view of ["summary", "references", "transitions", "actionables", "decisions",
        "watches", "content", "output-owners", "index"]) {
        expectFailure(
          ["--roadmap", "testing", "--query", view],
          expectedIssue("E-SOURCE-MISSING", "tests/testing-roadmap.toml", "$", "declared source is missing", 1),
        );
      }
      return pass("positive", ["summary", "references", "transitions",
        "actionables", "decisions", "watches", "content", "output-owners", "index"]);
    }
    case "cli_no_args_rejected": expectFailure([], cliIssue("E-CLI-MODE", 0, "exactly one primary mode is required"), fakePorts(), true); return pass("negative");
    case "cli_unknown_option": expectFailure(["--wat"], cliIssue("E-CLI-UNKNOWN-OPTION", 0, 'unknown option "--wat"'), fakePorts(), true); return pass("negative");
    case "cli_missing_value": expectFailure(["--query"], cliIssue("E-CLI-MISSING-VALUE", 0, "--query requires a value"), fakePorts(), true); return pass("negative");
    case "cli_duplicate_scalar": expectFailure(["--check", "--roadmap", "matrix", "--roadmap", "testing"], cliIssue("E-CLI-DUPLICATE-OPTION", 3, "--roadmap may occur exactly once"), fakePorts(), true); return pass("negative");
    case "cli_duplicate_primary_mode": expectFailure(["--check", "--write", "--roadmap", "matrix"], cliIssue("E-CLI-MODE", 1, "exactly one primary mode is required"), fakePorts(), true); return pass("negative");
    case "cli_roadmap_required": expectFailure(["--check"], cliIssue("E-CLI-ROADMAP", 0, "--roadmap is required for this mode"), fakePorts(), true); return pass("negative");
    case "cli_roadmap_forbidden_on_format": expectFailure(["--format-source", "cddl-matrix/roadmap.toml", "--roadmap", "matrix"], cliIssue("E-CLI-INCOMPATIBLE", 2, "--roadmap is forbidden for this mode"), fakePorts(), true); return pass("negative");
    case "cli_write_all_rejected": expectFailure(["--write", "--roadmap", "all"], cliIssue("E-CLI-INCOMPATIBLE", 1, "--write requires matrix or testing, not all"), fakePorts(), true); return pass("negative");
    case "cli_json_requires_query": expectFailure(["--check", "--roadmap", "matrix", "--json"], cliIssue("E-CLI-INCOMPATIBLE", 3, "--json is valid only with --query"), fakePorts(), true); return pass("negative");
    case "cli_as_of_requires_query": expectFailure(["--check", "--roadmap", "matrix", "--as-of", "2024-01-01"], cliIssue("E-CLI-INCOMPATIBLE", 3, "--as-of is valid only with --query"), fakePorts(), true); return pass("negative");
    case "cli_format_target_declared_only": expectFailure(["--format-source", "draft/roadmap.toml"], cliIssue("E-CLI-FORMAT-TARGET", 0, "--format-source must name a declared roadmap TOML source"), fakePorts(), true); return pass("negative");
    case "cli_as_of_invalid_leap_day": return invalidDate("2023-02-29");
    case "cli_as_of_year_zero_rejected": return invalidDate("0000-01-01");
    case "cli_as_of_short_component_rejected": return invalidDate("2024-2-01");
    case "cli_as_of_timestamp_rejected": return invalidDate("2024-02-29T00:00:00Z");
    case "cli_as_of_whitespace_rejected": return invalidDate(" 2024-02-29");
    default: return undefined;
  }
}

function invalidDate(value: string): SelfTestResult {
  expectFailure(
    ["--roadmap", "testing", "--query", "transitions", "--as-of", value],
    cliIssue("E-CLI-AS-OF", 4, "--as-of must be an existing Gregorian date in YYYY-MM-DD form"),
    fakePorts(),
    true,
  );
  return pass("negative");
}

function gitAndExitCase(id: RequiredCliSelfTestCaseId, context: SelfTestContext): SelfTestResult | undefined {
  switch (id) {
    case "exit_declared_source_enoent_one": expectFailure(["--query", "summary", "--roadmap", "matrix"], expectedIssue("E-SOURCE-MISSING", "cddl-matrix/roadmap.toml", "$", "declared source is missing", 1)); return pass("negative");
    case "exit_declared_source_eacces_two": expectFailure(["--query", "summary", "--roadmap", "matrix"], expectedIssue("E-IO-PERMISSION", "cddl-matrix/roadmap.toml", "$", "permission denied", 2), fakePorts({ read: (path) => { throw roadmapFailure("E-IO-PERMISSION", path, "$", "permission denied", 2); } })); return pass("negative");
    case "exit_declared_reference_enoent_one": expectFailure(["--query", "references", "--roadmap", "matrix"], expectedIssue("E-REFERENCE-UNRESOLVED", "cddl-matrix/roadmap.toml", "reference", "declared reference missing", 1), fakePorts({ read: (path) => { throw roadmapFailure("E-REFERENCE-UNRESOLVED", path, "reference", "declared reference missing", 1); } })); return pass("negative");
    case "exit_other_read_io_two": expectFailure(["--query", "summary", "--roadmap", "matrix"], expectedIssue("E-IO-READ", "cddl-matrix/roadmap.toml", "$", "read failed", 2), fakePorts({ read: (path) => { throw roadmapFailure("E-IO-READ", path, "$", "read failed", 2); } })); return pass("negative");
    case "exit_malformed_toml_one":
    case "parse_error_stable_prefix": expectFailure(["--query", "summary", "--roadmap", "matrix"], expectedIssue("E-TOML-PARSE", "cddl-matrix/roadmap.toml", "$", "Bun rejected TOML structure", 1, true), fakePorts({ read: () => UTF8.encode("not = [toml\n") })); return pass("negative");
    case "exit_atomic_write_two": expectFailure(["--format-source", "tests/testing-roadmap.toml"], expectedIssue("E-IO-WRITE", "tests/testing-roadmap.toml", "atomic_replace", "write failed", 2), validTestingPorts(context, (path) => { throw roadmapFailure("E-IO-WRITE", path, "atomic_replace", "write failed", 2); })); return pass("negative");
    case "exit_internal_fault_two": expectFailure(["--query", "summary", "--roadmap", "matrix"], expectedIssue("E-INTERNAL", "<internal>", "runRoadmapCli", "fault sentinel", 2), fakePorts({ read: () => { throw new Error("fault sentinel"); } })); return pass("negative");
    case "exit_authority_stage_mismatch_one": {
      const mismatch = (
        source: Uint8Array,
        stage: "pre_cutover" | "matrix_authoritative" | "both_authoritative",
        matrixStatusInputs: MatrixStatusInputs,
      ) => fakePorts({
        read: (path) => path === "cddl-matrix/roadmap.toml"
          ? new Uint8Array(source)
          : (() => { throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1); })(),
        registry: (revision) => ({
          ...liveRegistry(revision),
          production_output_stage: stage,
          output_claims: productionOutputInventory(stage).claims,
          matrix_status_inputs: matrixStatusInputs,
        }),
      });
      for (const [source, stage, matrixStatusInputs, message] of [
        [
          liveMatrixAuthoritativeSource(),
          "pre_cutover",
          liveMatrixStatusInputs(),
          "authoritative roadmap requires its same-revision production whole-file projection claim",
        ],
      ] as const) {
        expectFailure(
          ["--query", "summary", "--roadmap", "matrix"],
          expectedIssue(
            "E-OUTPUT-AUTHORITY",
            "cddl-matrix/roadmap.toml",
            "document.authority",
            message,
            1,
          ),
          mismatch(source, stage, matrixStatusInputs),
        );
      }
      return pass("negative", ["authoritative_without_claim"]);
    }
    case "failure_stdout_empty": {
      const result = expectFailure([], cliIssue("E-CLI-MODE", 0, "exactly one primary mode is required"), fakePorts(), true);
      assert(result.stdout.byteLength === 0, "failure wrote stdout");
      return pass("negative");
    }
    default: return undefined;
  }
}

function positiveServiceCase(id: RequiredCliSelfTestCaseId, context: SelfTestContext): SelfTestResult | undefined {
  switch (id) {
    case "cli_dashboard_views_operationally_complete": {
      const result = run(["--roadmap", "matrix", "--query", "summary", "--json"], bothAuthoritativePorts());
      assert(result.exit_code === 0 && result.stderr.byteLength === 0, `live matrix summary failed: ${text(result.stderr)}`);
      const payload = JSON.parse(text(result.stdout));
      const matrixSummary = payload.roadmaps?.find((row: { roadmap: string }) => row.roadmap === "matrix");
      assert(typeof matrixSummary?.record_count === "number" && matrixSummary.schema_version === 3,
        `live matrix summary did not expose its record inventory: ${JSON.stringify(payload)}`);
      const dashboards = new Map<string, Record<string, unknown>>();
      for (const view of ["transitions", "actionables", "decisions", "watches", "content"] as const) {
        const first = run(["--roadmap", "all", "--query", view, "--json"], bothAuthoritativePorts());
        const second = run(["--roadmap", "all", "--query", view, "--json"], bothAuthoritativePorts());
        assert(first.exit_code === 0 && second.exit_code === 0 && first.stderr.byteLength === 0 &&
          second.stderr.byteLength === 0 && text(first.stdout) === text(second.stdout),
        `${view} dashboard is not a deterministic successful query`);
        dashboards.set(view, JSON.parse(text(first.stdout)));
      }
      const decisions = dashboards.get("decisions")!.decisions as Record<string, readonly Record<string, unknown>[]>;
      assert(Object.values(decisions).flat().length > 0 &&
        (decisions.pending ?? []).every((row) => "question_md" in row && "unblock_predicate" in row) &&
        (decisions.held ?? []).every((row) => "rationale_md" in row && "reopening_signal" in row) &&
        (decisions.decided ?? []).every((row) => "rationale_md" in row && "authority_reference_id" in row),
      "decisions dashboard dropped its state-specific question/rationale/authority fields");
      // Standalone transition records are unrepresentable (Phase 4 fold); the transitions
      // dashboard carries exactly the temporal rows — nested cadences under their owners' ids,
      // and as-of evidence freshness. The qualitative nested kinds surface via
      // actionables/decisions/watches.
      const transitions = dashboards.get("transitions")!.transitions as Record<string, readonly Record<string, unknown>[]>;
      assert(JSON.stringify(Object.keys(transitions)) === JSON.stringify(["cadence", "evidence_freshness"]),
        "transitions dashboard transition grouping changed");
      for (const [kind, required] of [
        ["cadence", ["period_or_event_md", "checklist_md", "missed_action_md", "due_on"]],
        ["evidence_freshness", ["claim_md", "reference_ids", "scope", "valid_through", "unprobed_remainder_md"]],
      ] as const) {
        assert(transitions[kind]!.length > 0 && required.every((field) => field in transitions[kind]![0]!),
          `${kind} dashboard dropped required operational fields`);
      }
      const actionables = dashboards.get("actionables")!;
      assert(JSON.stringify(Object.keys(actionables)) === JSON.stringify([
        "armed_recur_first", "blocked_external_delegated", "costs", "evaluation_as_of",
        "external_closeouts", "pending_review", "ready_by_admission_basis", "ready_by_consequence",
      ]), "actionables dashboard did not separate admission, consequence, cost, and external closeouts");
      const readyRows = Object.values(actionables.ready_by_consequence as Record<string, readonly Record<string, unknown>[]>)
        .flat();
      const blockedRows = Object.values(actionables.blocked_external_delegated as Record<string, readonly Record<string, unknown>[]>)
        .flat();
      assert(readyRows.length > 0 && readyRows.every((row) => row.work_state === "ready") &&
        blockedRows.some((row) => row.work_state === "blocked" && "blocker_md" in row &&
          "exact_unblock_predicates" in row) &&
        blockedRows.some((row) => row.work_state === "waiting_external" &&
          "external_owner_reference_id" in row && "exact_unblock_predicates" in row),
      "blocked/external actionable ownership and unblock views are incomplete");
      const closeouts = actionables.external_closeouts as Record<string, readonly Record<string, unknown>[]>;
      assert(closeouts.waiting!.length > 0 && closeouts.due!.length > 0 &&
        [...closeouts.waiting!, ...closeouts.due!].every((row) => "actions" in row && "branches" in row &&
          "verification_md" in row && "upstream_owner_reference_id" in row),
      "matrix external closeout dashboard is absent or lossy");
      const syntheticTesting = liveTestingV3Document();
      const syntheticRecords: RoadmapDocumentV3["records"] = [
        ...syntheticTesting.records,
        {
          id: "testing.fixture-dashboard-delegated" as RoadmapId,
          title: "Delegated dashboard fixture",
          payload: { kind: "work",
            work_state: "delegated", work_intent: "build_capability", work_kind: "feature",
            risk: "cosmetic",
            return_condition_md: UTF8.encode("Return when the delegated owner completes the fixture.") },
        },
        {
          id: "testing.fixture-dashboard-cost" as RoadmapId,
          title: "Cost dashboard fixture",
          payload: { kind: "testing_cost",
            cost_posture: "live_registry", unit: "milliseconds",
            scope_md: UTF8.encode("One synthetic dashboard operation."),
            gate_reference_id: syntheticTesting.references.find((reference) => reference.kind === "gate")!.id },
        },
      ];
      const syntheticDocument: RoadmapDocumentV3 = { ...syntheticTesting, records: syntheticRecords,
        relations: [...syntheticTesting.relations, {
          source: "testing.fixture-dashboard-delegated" as RoadmapId,
          kind: "delegates_to",
          target: syntheticTesting.records[0]!.id,
        }] };
      const baseDashboardPorts = bothAuthoritativePorts();
      const syntheticPorts = fakePorts({
        read(path) {
          if (path === "tests/testing-roadmap.toml") return composeRoadmapDocument(syntheticDocument);
          return baseDashboardPorts.read.readDeclared(path);
        },
        registry: baseDashboardPorts.read.registryView,
      });
      const syntheticResult = run(["--roadmap", "testing", "--query", "actionables", "--json"], syntheticPorts);
      assert(syntheticResult.exit_code === 0, `synthetic delegated/cost dashboard failed: ${text(syntheticResult.stderr)}`);
      const syntheticActionables = JSON.parse(text(syntheticResult.stdout));
      const syntheticRows = Object.values(syntheticActionables.blocked_external_delegated as Record<string, readonly Record<string, unknown>[]>)
        .flat();
      assert(syntheticRows.some((row) => row.work_state === "delegated" && "return_condition_md" in row &&
          Array.isArray(row.delegation_targets) && row.delegation_targets.length === 1) &&
        syntheticActionables.costs.length === 1 && syntheticActionables.costs[0].cost_posture === "live_registry" &&
        !("consequence" in syntheticActionables.costs[0]),
      "delegated ownership or cost/effort separation is absent from the actionable dashboard");
      const watches = dashboards.get("watches")!;
      const liveWatches = watches.live as readonly Record<string, unknown>[];
      const history = watches.attributed_history as readonly Record<string, unknown>[];
      assert(liveWatches.length === 4 && history.length === 10 && liveWatches.every((row) =>
        "payload_kind" in row && "signature_md" in row && ("capture_steps" in row || "evidence_ids" in row)) &&
        history.every((row) => row.payload_kind === "testing_incident" && "signature_md" in row &&
          "attribution_md" in row && "evidence_ids" in row && "operating_rule_reference_id" in row),
      "live watches or retained attributed-incident evidence lost lifecycle fields");
      const content = dashboards.get("content")!.roadmaps as readonly Record<string, unknown>[];
      assert(content.length === 2 && content.every((roadmap) => typeof roadmap.audit_markdown === "string" &&
        (roadmap.audit_markdown as string).startsWith("# Roadmap authored-content audit") &&
        Array.isArray(roadmap.authored_content) && (roadmap.authored_content as readonly Record<string, unknown>[])
          .every((entry) => !("markdown" in entry) && "transformation" in entry && "output_sha256" in entry)),
      "content dashboard does not expose audit-only prose plus metadata-only exact reachability");
      return pass("positive");
    }
    case "cli_format_collects_uncited_references": {
      // The planted row is deliberately UNRESOLVABLE: if collection did not precede validation,
      // the format run would fail on the missing gate instead of sweeping the row away.
      const planted = '\n[[reference]]\nid = "uncited-residue"\nsource = "testing.fixture-small-semantic"\n' +
        'kind = "gate"\ngate_id = "gate-that-does-not-exist"\n';
      const generic = validGenericAllPorts(context);
      const target = "tests/testing-roadmap.toml" as RepoPath;
      let written: Uint8Array | undefined;
      const ports = fakePorts({
        read: (path) => path === target
          ? UTF8.encode(`${new TextDecoder().decode(generic.read.readDeclared(path))}${planted}`)
          : generic.read.readDeclared(path),
        registry: generic.read.registryView,
        atomic: (_path, bytes) => { written = bytes; },
      });
      const result = run(["--format-source", target], ports);
      const stdout = text(result.stdout);
      assert(result.exit_code === 0 && stdout.includes("collected_references=1"),
        `uncited reference was not collected: ${stdout}`);
      assert(written !== undefined && !new TextDecoder().decode(written).includes("uncited-residue"),
        "collected reference survived into the formatted source");
      const clean = run(["--format-source", target], validTestingPorts(context, () => {}));
      assert(text(clean.stdout).includes("collected_references=0"),
        "a source with no residue reported a collection");
      return pass("positive");
    }
    case "cli_format_declared_source":
    {
      let writes = 0;
      const result = run(["--format-source", "tests/testing-roadmap.toml"], validTestingPorts(context, () => { writes++; }));
      assert(result.exit_code === 0 && result.stdout.byteLength > 0 && result.stderr.byteLength === 0 && writes === 1, "format success receipt/write mismatch");
      return pass("positive");
    }
    case "success_receipt_nonvacuous": {
      const result = run(["--check", "--roadmap", "testing"], validTestingPorts(context));
      const stdout = text(result.stdout);
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 &&
          stdout.startsWith("SELFTEST OK fake=1\nCHECK OK\n") &&
          stdout.includes("source=tests/testing-roadmap.toml") && stdout.includes("projection_sha256="),
        "successful check did not emit one nonvacuous selftest + projection receipt",
      );
      return pass("positive");
    }
    case "query_stdout_payload_only": {
      const result = run(["--roadmap", "testing", "--query", "summary", "--json"], validTestingPorts(context));
      assert(result.exit_code === 0 && result.stdout.byteLength > 0 && result.stderr.byteLength === 0, "query streams are not isolated");
      assert(!text(result.stdout).includes("CHECK OK") && JSON.parse(text(result.stdout)).evaluation_as_of === null, "query stdout contains a receipt or unstable envelope");
      return pass("positive");
    }
    case "live_subordinate_lifecycle_dispositions": {
      const matrix = liveMatrixV3Document();
      const testing = liveTestingV3Document();
      for (const [name, document, expectedSlots, expectedParts] of [
        ["matrix", matrix, 4, 9],
        ["testing", testing, 0, 27],
      ] as const) {
        assert(document.parts.every((part) => part.body_md.byteLength > 0),
          `${name} part prose is empty`);
        assert(documentSlots(document.sections).length === expectedSlots &&
          document.parts.length === expectedParts,
          `${name} subordinate denominator drifted`);
      }
      const testingNested = testing.parts.find((part) => part.part_id === "part-nested-cargo-test");
      assert(testingNested !== undefined, "testing nested-cargo supporting part drifted");
      return pass("positive");
    }
    case "cli_as_of_valid_leap_day": {
      const result = run(["--roadmap", "testing", "--query", "transitions", "--as-of", "2024-02-29", "--json"], validTestingPorts(context));
      assert(result.exit_code === 0 && text(result.stdout).includes("2024-02-29"), "valid leap day was not carried into query output");
      return pass("positive");
    }
    case "query_as_of_due_date_inclusive":
    case "query_as_of_valid_through_inclusive":
    case "query_as_of_after_valid_through_stale": {
      const date = id === "query_as_of_after_valid_through_stale" ? "2025-01-02" : "2025-01-01";
      const kind = id === "query_as_of_due_date_inclusive" ? "cadence" : "evidence";
      const result = run(
        ["--roadmap", "testing", "--query", "transitions", "--as-of", date, "--json"],
        temporalTestingPorts(context, kind),
      );
      assert(result.exit_code === 0, `${id} service query failed: ${text(result.stderr)}`);
      const payload = JSON.parse(text(result.stdout));
      const transitionGroup = kind === "cadence" ? "cadence" : "evidence_freshness";
      const transitions = payload.transitions?.[transitionGroup];
      const expected = id === "query_as_of_due_date_inclusive"
        ? "due"
        : id === "query_as_of_valid_through_inclusive" ? "as_of" : "stale";
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 && payload.evaluation_as_of === date &&
          Array.isArray(transitions) && transitions.length === 1 && transitions[0].id === "testing.fixture-small-semantic" &&
          transitions[0].evaluation === expected,
        `${id} did not derive ${expected} from the decoded service document: ${text(result.stdout)} ${text(result.stderr)}`,
      );
      return pass("positive");
    }
    case "query_without_as_of_reads_no_clock": {
      const valid = temporalTestingPorts(context, "cadence");
      const paths: RepoPath[] = [];
      const ports = fakePorts({
        read: valid.read.readDeclared,
        registry: valid.read.registryView,
        onReadPath: (path) => paths.push(path),
      });
      const result = run(["--roadmap", "testing", "--query", "transitions", "--json"], ports);
      assert(result.exit_code === 0, `no-as-of service query failed: ${text(result.stderr)}`);
      const payload = JSON.parse(text(result.stdout));
      const transitions = payload.transitions?.cadence;
      assert(
        result.exit_code === 0 && payload.evaluation_as_of === null && Array.isArray(transitions) &&
          transitions.length === 1 && transitions[0].evaluation === "unknown_no_as_of",
        "no-as-of query fabricated time or did not preserve the decoded unknown posture",
      );
      assert(!paths.includes(PROJECTION_PATH_BY_ROADMAP.testing), "query read the on-disk projection render");
      return pass("positive");
    }
    case "query_as_of_does_not_select_git_revision": {
      let git = 0;
      const ports = validTestingPorts(context);
      const wrapped = fakePorts({
        read: ports.read.readDeclared,
        registry: ports.read.registryView,
        objectFormat: () => { git++; return "sha1"; },
        resolve: (value) => { git++; return value; },
      });
      const result = run(["--roadmap", "testing", "--query", "summary", "--as-of", "2025-01-01"], wrapped);
      assert(result.exit_code === 0 && git === 0, "--as-of selected a Git revision");
      return pass("positive");
    }
    case "cli_authoritative_fresh_projection_reference_provenance": {
      const subcases: string[] = [];
      const projection = fixture(context, "positive/small-testing-v3.expected.md");
      const freshHeading = uniqueProjectionHeading(projection);

      const freshReads = { value: 0 };
      const fresh = run(
        ["--roadmap", "testing", "--query", "summary", "--json"],
        authoritativeProjectionReferencePorts(
          context,
          freshHeading,
          "Prior projection has no current heading",
          freshReads,
        ),
      );
      assert(
        fresh.exit_code === 0 && fresh.stderr.byteLength === 0 && freshReads.value === 0,
        `fresh expected-projection heading did not resolve without reading prior output: reads=${freshReads.value} ${text(fresh.stderr)}`,
      );
      subcases.push("fresh_resolves");

      const driftedHeading = `${freshHeading} (prior spelling)`;
      const driftedReads = { value: 0 };
      const drifted = run(
        ["--roadmap", "testing", "--query", "summary", "--json"],
        authoritativeProjectionReferencePorts(context, driftedHeading, driftedHeading, driftedReads),
      );
      assert(
        drifted.exit_code === 1 && drifted.stdout.byteLength === 0 && driftedReads.value === 0 &&
          text(drifted.stderr).includes("FAIL [E-REFERENCE-UNRESOLVED]") &&
          text(drifted.stderr).includes(JSON.stringify(driftedHeading)) &&
          text(drifted.stderr).includes("is absent"),
        `drifted prior-only heading influenced current validation: reads=${driftedReads.value} ${text(drifted.stderr)}`,
      );
      subcases.push("drifted_prior_rejects");

      const missingHeading = "Selftest heading absent from every projection view";
      const missingReads = { value: 0 };
      const missing = run(
        ["--roadmap", "testing", "--query", "summary", "--json"],
        authoritativeProjectionReferencePorts(
          context,
          missingHeading,
          "Different prior projection heading",
          missingReads,
        ),
      );
      assert(
        missing.exit_code === 1 && missing.stdout.byteLength === 0 && missingReads.value === 0 &&
          text(missing.stderr).includes("FAIL [E-REFERENCE-UNRESOLVED]") &&
          text(missing.stderr).includes(JSON.stringify(missingHeading)) &&
          text(missing.stderr).includes("is absent"),
        `missing fresh projection heading was not rejected: reads=${missingReads.value} ${text(missing.stderr)}`,
      );
      subcases.push("missing_rejects");
      observeSelfTestIssue({
        code: "E-REFERENCE-UNRESOLVED",
        logical_path: `reference["selftest-fresh-projection-heading"]`,
      });
      return pass("positive", subcases);
    }
    case "cli_production_port_factory_smoke": {
      let anchorReads = 0;
      let fixtureCount = 0;
      let fixtureBytes = 0;
      const productionPorts = createNodeRoadmapCliPorts({
        get matrix_dir() {
          anchorReads += 1;
          return `${import.meta.dir}/../..`;
        },
      });
      const result = runRoadmapCli(
        ["--selftest"],
        productionPorts,
        {
          run_selftests: ((ports) => {
            const fixtures = ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
            fixtureCount = fixtures.length;
            fixtureBytes = ports.fixtures.readFixtureFile(FIXTURE_ROOT, fixtures[0]!).byteLength;
            return {
              receipt: { categories: [], total: 1 },
              stdout: UTF8.encode(`SELFTEST OK production_factory fixtures=${fixtureCount}\n`),
            };
          }) as RoadmapCliDispatchServices["run_selftests"],
        },
      );
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 && anchorReads === 1 &&
          fixtureCount >= 20 && fixtureBytes > 0 &&
          text(result.stdout) === `SELFTEST OK production_factory fixtures=${fixtureCount}\n`,
        `direct frozen seam did not lazily initialize once from its absolute anchor: exit=${result.exit_code} anchor_reads=${anchorReads} fixtures=${fixtureCount} fixture_bytes=${fixtureBytes} stdout=${JSON.stringify(text(result.stdout))} stderr=${JSON.stringify(text(result.stderr))}`,
      );
      const currentRegistry = productionPorts.read.registryView({ kind: "worktree" });
      assert(
        currentRegistry.roadmap_citations.every((fact) =>
          !fact.source.startsWith("cddl-matrix/roadmap/") || !fact.source.endsWith(".ts")
        ),
        `tracked roadmap implementation/self-test source contains an unintended citation candidate: ${JSON.stringify(currentRegistry.roadmap_citations)}`,
      );
      assert(anchorReads === 1, "a second valid port method reinitialized the production root");
      let invalidAnchorReads = 0;
      const invalidPorts = createNodeRoadmapCliPorts({
        get matrix_dir(): string {
          invalidAnchorReads += 1;
          throw new Error("invalid argv must not initialize ports");
        },
      });
      const invalid = runRoadmapCli(["--wat"], invalidPorts, fakeSelftestServices);
      assert(
        invalid.exit_code === 2 && invalidAnchorReads === 0 &&
          text(invalid.stderr) === `FAIL [E-CLI-UNKNOWN-OPTION] <cli>#argv[0]: unknown option \"--wat\"\nFAILED: 1 issue(s)\n${ROADMAP_CLI_USAGE}`,
        "invalid argv initialized a lazy port method/effect before tokenization or lost the stable CLI diagnostic",
      );
      let failedAnchorReads = 0;
      const failedPorts = createNodeRoadmapCliPorts({
        get matrix_dir() {
          failedAnchorReads += 1;
          return ".";
        },
      });
      const failureServices = {
        run_selftests: ((ports) => {
          ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
          throw new Error("unreachable after lazy initialization failure");
        }) as RoadmapCliDispatchServices["run_selftests"],
      };
      const factoryFailure = runRoadmapCli(["--selftest"], failedPorts, failureServices);
      const cachedFactoryFailure = runRoadmapCli(["--selftest"], failedPorts, failureServices);
      assert(
        factoryFailure.exit_code === 2 && cachedFactoryFailure.exit_code === 2 && failedAnchorReads === 1 &&
          factoryFailure.stdout.byteLength === 0 && cachedFactoryFailure.stdout.byteLength === 0 &&
          text(factoryFailure.stderr) === text(cachedFactoryFailure.stderr) &&
          text(factoryFailure.stderr) ===
            "FAIL [E-INTERNAL] <internal>#matrix-dir: matrix_dir must be absolute\nFAILED: 1 issue(s)\n",
        `lazy initialization failure was not cached/rendered through the stable envelope: reads=${failedAnchorReads} first=${text(factoryFailure.stderr)} second=${text(cachedFactoryFailure.stderr)}`,
      );
      return pass("positive");
    }
    case "dispatch_capability_narrowing": {
      const arms: string[] = [];
      const ports = fakePorts({ onReadArm: (arm) => arms.push(arm) });
      expectFailure(["--check", "--roadmap", "matrix"], expectedIssue("E-SOURCE-MISSING", "cddl-matrix/roadmap.toml", "$", "declared source is missing", 1), ports);
      assert(arms.at(-1) === "read", "check did not receive read-only capability");
      expectFailure(["--query", "summary", "--roadmap", "matrix"], expectedIssue("E-SOURCE-MISSING", "cddl-matrix/roadmap.toml", "$", "declared source is missing", 1), ports);
      assert(arms.at(-1) === "read", "query did not receive read-only capability");
      expectFailure(["--write", "--roadmap", "matrix"], expectedIssue("E-SOURCE-MISSING", "cddl-matrix/roadmap.toml", "$", "declared source is missing", 1), ports);
      assert(arms.at(-1) === "write", "write did not receive write capability");
      let writes = 0;
      const format = run(["--format-source", "tests/testing-roadmap.toml"], validTestingPorts(context, () => { writes++; }));
      assert(format.exit_code === 0 && writes === 1, "format did not receive atomic replace capability");
      return pass("positive", ["check_read_only", "query_read_only", "write_gets_atomic_replace", "format_gets_atomic_replace"]);
    }
    case "cli_query_index_shape": {
      // The agent index is a pure projection: one line per record, every column derived, and the
      // text and JSON forms carrying the same rows in the same order.
      const first = run(["--roadmap", "all", "--query", "index"], bothAuthoritativePorts());
      const second = run(["--roadmap", "all", "--query", "index"], bothAuthoritativePorts());
      assert(first.exit_code === 0 && first.stderr.byteLength === 0 &&
        text(first.stdout) === text(second.stdout), `index view is not a deterministic query: ${text(first.stderr)}`);
      const json = run(["--roadmap", "all", "--query", "index", "--json"], bothAuthoritativePorts());
      assert(json.exit_code === 0, "index view rejected --json");
      const payload = JSON.parse(text(json.stdout)) as {
        evaluation_as_of: null;
        index: readonly { roadmap: string; id: string; arm: string; state: string; section: string;
          cited: readonly string[]; title: string }[];
      };
      const lines = text(first.stdout).split("\n").slice(0, -1);
      const records = [...liveMatrixV3Document().records, ...liveTestingV3Document().records];
      assert(payload.index.length === records.length && lines.length === records.length,
        `index view has ${payload.index.length} rows and ${lines.length} lines for ${records.length} records`);
      assert(JSON.stringify(payload.index.map((row) => `${row.roadmap}\u0000${row.id}`)) ===
        JSON.stringify([...payload.index].sort((left, right) =>
          codePointSort(left.roadmap, right.roadmap) || codePointSort(left.id, right.id))
          .map((row) => `${row.roadmap}\u0000${row.id}`)),
        "index rows are not ordered by roadmap then id");
      const columns = lines.map((line) => line.split("\t"));
      assert(columns.every((row) => row.length === 7), "an index line does not carry exactly seven columns");
      for (const [position, row] of payload.index.entries()) {
        const line = columns[position];
        assert(line[0] === row.roadmap && line[1] === row.id && line[2] === row.arm &&
          line[3] === row.state && line[4] === row.section &&
          line[5] === (row.cited.length === 0 ? "-" : row.cited.join(",")) &&
          line[6] === JSON.stringify(row.title),
        `index line ${position} disagrees with its JSON row`);
        assert(row.state !== "" && row.section !== "" && row.title !== "", "index row has an empty column");
      }
      // Non-rendering records report no section, and placed ones name the section that lists them.
      const placed = payload.index.filter((row) => row.section !== "-");
      const unplaced = payload.index.filter((row) => row.section === "-");
      assert(placed.length > 0 && unplaced.length > 0, "index view distinguishes no placement state");
      const sections = new Map([...liveMatrixV3Document().sections, ...liveTestingV3Document().sections]
        .map((section) => [section.section_id as string, section.entries]));
      assert(placed.every((row) => (sections.get(row.section) ?? []).includes(row.id)),
        "an index row names a section that does not list it");
      assert(payload.index.some((row) => row.cited.length > 0), "index view cites no record anywhere");
      return pass("positive");
    }
    default: return undefined;
  }
}

function runCase(id: RequiredCliSelfTestCaseId, context: SelfTestContext): SelfTestResult {
  return grammarCase(id, context) ?? gitAndExitCase(id, context) ?? positiveServiceCase(id, context) ?? (() => {
    throw new Error(`unhandled CLI self-test ${id}`);
  })();
}

export const CLI_SELFTEST_CASES: readonly SelfTestCase[] = Object.freeze(
  REQUIRED_CLI_SELFTEST_CASE_IDS.map((id): SelfTestCase => Object.freeze({
    id,
    category: "cli-diagnostics",
    run: (context: SelfTestContext) => runCase(id, context),
  })),
);
