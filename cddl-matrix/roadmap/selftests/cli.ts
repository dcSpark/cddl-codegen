import type { RegistryView } from "../adapters/types.ts";
import { ROADMAP_CLI_USAGE } from "../cli.ts";
import { composeRoadmapDocument } from "../compose.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import { RoadmapFailure } from "../errors.ts";
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
import type { FixtureRelativePath, RepoPath, RepositoryRevision } from "../model/core.ts";
import type { RoadmapDocumentV0 } from "../model/documents.ts";
import type { MatrixStatusInputs } from "../model/matrix.ts";
import {
  LEGACY_STATUS_OUTPUT_CLAIMS,
  productionOutputInventory,
} from "../output_registry.ts";
import type { SelfTestCandidateCase as SelfTestCase, SelfTestContext, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import { observeSelfTestIssue } from "./observations.ts";
import {
  liveMatrixAuthoritativeSource,
  liveMatrixProjection,
  liveMatrixShadowV0Source,
} from "./live_matrix.ts";
import {
  liveTestingAuthoritativeSource,
  liveTestingProjection,
  liveTestingShadowV0Source,
} from "./live_testing.ts";

export const REQUIRED_CLI_SELFTEST_CASE_IDS = [
  "cli_selftest_exact",
  "cli_check_each_roadmap",
  "cli_write_each_single_roadmap",
  "cli_write_authoritative_matrix",
  "cli_write_authoritative_testing",
  "cli_query_each_view",
  "cli_format_declared_source",
  "cli_no_args_rejected",
  "cli_unknown_option",
  "cli_missing_value",
  "cli_duplicate_scalar",
  "cli_duplicate_primary_mode",
  "cli_roadmap_required",
  "cli_roadmap_forbidden_on_format",
  "cli_write_all_rejected",
  "cli_against_requires_check",
  "cli_against_write_rejected",
  "cli_against_query_rejected",
  "cli_against_selftest_rejected",
  "cli_against_format_rejected",
  "cli_json_requires_query",
  "cli_as_of_requires_query",
  "cli_format_target_declared_only",
  "cli_unresolved_base_exit_two",
  "cli_noncommit_base_exit_two",
  "exit_declared_source_enoent_one",
  "exit_declared_projection_enoent_one",
  "exit_declared_source_eacces_two",
  "exit_declared_reference_enoent_one",
  "exit_other_read_io_two",
  "exit_malformed_toml_one",
  "exit_atomic_write_two",
  "exit_internal_fault_two",
  "exit_authority_stage_mismatch_one",
  "parse_error_stable_prefix",
  "query_stdout_payload_only",
  "failure_stdout_empty",
  "success_receipt_nonvacuous",
  "cli_against_matrix_check_allowed",
  "cli_against_testing_check_allowed",
  "cli_against_all_check_allowed",
  "cli_wp4m_mixed_authority_all",
  "cli_shadow_authoritative_markdown_provenance",
  "cli_against_missing_value",
  "cli_against_duplicate_value",
  "cli_against_bad_length_git_format_no_usage",
  "cli_against_uppercase_git_format_no_usage",
  "cli_against_nonhex_git_format_no_usage",
  "cli_against_unresolved_git_lookup_no_usage",
  "cli_against_incompatible_precedes_bad_format",
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
] as const;

export type RequiredCliSelfTestCaseId = (typeof REQUIRED_CLI_SELFTEST_CASE_IDS)[number];

const UTF8 = new TextEncoder();
const DECODER = new TextDecoder();
const FIXTURE_ROOT = "cddl-matrix/roadmap/fixtures" as RepoPath;
const HASH = "a".repeat(40);

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

function temporalTestingPorts(
  context: SelfTestContext,
  kind: "cadence" | "evidence",
): RoadmapCliPorts {
  const sourcePath = "tests/testing-roadmap.toml" as RepoPath;
  const projectionPath = "tests/TESTING_ROADMAP.md" as RepoPath;
  const base = text(fixture(context, "positive/mixed-testing-v1.toml"))
    .replace("cddl-matrix/roadmap/fixtures/positive/mixed-testing-v1.toml", sourcePath)
    .replace("cddl-matrix/roadmap/fixtures/positive/mixed-testing-v1.expected.md", projectionPath);
  const payload = kind === "cadence"
    ? `[record.payload]
kind = "signal"
summary_md = """## Semantic testing work

"""
detail_md = """Semantic testing detail."""
transition_kind = "cadence"
owner_reference_id = "temporal-owner"
event_source = "fixture-calendar"
period_or_event_md = """Review on the fixture date."""
checklist_md = """Check the decoded cadence."""
missed_action_md = """Escalate the missed fixture cadence."""
due_on = "2025-01-01"
evaluation = "unknown"
`
    : `[record.payload]
kind = "evidence"
summary_md = """## Semantic testing work

"""
detail_md = """Semantic testing detail."""
evidence_kind = "external_commit"
claim_md = """The fixture commit is point-in-time evidence."""
evidence_verdict = "confirmed"
freshness = "as_of"
reference_ids = ["temporal-owner"]
at_commit = "1111111111111111111111111111111111111111"
observed_at = "2025-01-01"
valid_through = "2025-01-01"
unprobed_remainder_md = """No timeless claim is made."""

[record.payload.scope]
surfaces = ["fixture"]
`;
  const source = UTF8.encode(base.replace(
    /\[record\.payload\]\n[\s\S]*?(?=\n\[\[record\.source_replacement\]\])/,
    payload,
  ) + `
[[reference]]
id = "temporal-owner"
source = "testing.fixture-mixed-semantic"
kind = "external_commit"
repository = "fixture/repository"
commit = "1111111111111111111111111111111111111111"
`);
  const projection = fixture(context, "positive/mixed-testing-v1.expected.md");
  const campaign = UTF8.encode(`[campaign]
schema_version = 1
matrix_authority = "authoritative"
testing_authority = "authoritative"
`);
  const retired = UTF8.encode(`[retired_ids]
schema_version = 1
`);
  return fakePorts({
    read(path) {
      if (path === sourcePath) return new Uint8Array(source);
      if (path === projectionPath) return new Uint8Array(projection);
      if (path === "cddl-matrix/roadmap.toml") return liveMatrixAuthoritativeSource();
      if (path === "cddl-matrix/ROADMAP.md") return liveMatrixProjection();
      if (path === "roadmap-campaign.toml") return new Uint8Array(campaign);
      if (path === "roadmap-retired-ids.toml") return new Uint8Array(retired);
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    registry(revision) {
      return {
        ...emptyRegistry(revision),
        production_output_stage: "both_authoritative",
        output_claims: productionOutputInventory("both_authoritative").claims,
        matrix_status_inputs: liveMatrixStatusInputs(),
      };
    },
  });
}

function decodedCampaignPorts(): RoadmapCliPorts {
  const campaign = UTF8.encode(`[campaign]
schema_version = 1
matrix_authority = "legacy_markdown"
testing_authority = "legacy_markdown"
`);
  const retired = UTF8.encode(`[retired_ids]
schema_version = 1
`);
  return fakePorts({
    read(path) {
      if (path === "roadmap-campaign.toml") return new Uint8Array(campaign);
      if (path === "roadmap-retired-ids.toml") return new Uint8Array(retired);
      if (path === "cddl-matrix/ROADMAP.md") return UTF8.encode("# Matrix legacy authority\n");
      if (path === "tests/TESTING_ROADMAP.md") return UTF8.encode("# Testing legacy authority\n");
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
  });
}

function liveMatrixStatusInputs(): MatrixStatusInputs {
  const features = [
    ...Array.from({ length: 95 }, (_, index) => ({ id: `r-${index}`, profile: "RFC8610" })),
    { id: "rfc9682", profile: "RFC9682" },
    ...Array.from({ length: 27 }, (_, index) => ({ id: `c-${index}`, profile: "CDDL_CODEGEN" })),
  ];
  const annotations = Array.from({ length: 293 }, (_, index) => ({
    id: index < 89 ? `row-${index}` : `annotation-${index}`,
    status: "supported",
    ...(index === 0 ? { emission: { preserve: { status: "unsupported" } } } : {}),
  }));
  return {
    matrix: {
      annotations,
      features,
      containment_ids: Array.from({ length: 136 }, (_, index) => `containment-${index}`),
      control_operator_ids: Array.from({ length: 37 }, (_, index) => `control-${index}`),
    },
    catalog: { rows: Array.from({ length: 89 }, (_, index) => ({
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

function validGenericAllPorts(_context: SelfTestContext): RoadmapCliPorts {
  const matrixSourcePath = "cddl-matrix/roadmap.toml" as RepoPath;
  const matrixProjectionPath = "cddl-matrix/ROADMAP.md" as RepoPath;
  const testingSourcePath = "tests/testing-roadmap.toml" as RepoPath;
  const testingProjectionPath = "tests/TESTING_ROADMAP.md" as RepoPath;
  const matrixSource = liveMatrixShadowV0Source();
  const matrixProjection = liveMatrixProjection();
  const testingSource = liveTestingShadowV0Source();
  const testingProjection = liveTestingProjection();
  return fakePorts({
    read(path) {
      if (path === matrixSourcePath) return new Uint8Array(matrixSource);
      if (path === matrixProjectionPath) return new Uint8Array(matrixProjection);
      if (path === testingSourcePath) return new Uint8Array(testingSource);
      if (path === testingProjectionPath) return new Uint8Array(testingProjection);
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    registry(revision) {
      return {
        ...emptyRegistry(revision),
        matrix_status_inputs: liveMatrixStatusInputs(),
      };
    },
  });
}

function bothAuthoritativePorts(atomic?: FakeOptions["atomic"]): RoadmapCliPorts {
  const campaign = UTF8.encode(`[campaign]
schema_version = 1
matrix_authority = "authoritative"
testing_authority = "authoritative"
`);
  const retired = UTF8.encode(`[retired_ids]
schema_version = 1
`);
  return fakePorts({
    read(path) {
      if (path === "cddl-matrix/roadmap.toml") return liveMatrixAuthoritativeSource();
      if (path === "cddl-matrix/ROADMAP.md") return liveMatrixProjection();
      if (path === "tests/testing-roadmap.toml") return liveTestingAuthoritativeSource();
      if (path === "tests/TESTING_ROADMAP.md") return liveTestingProjection();
      if (path === "roadmap-campaign.toml") return new Uint8Array(campaign);
      if (path === "roadmap-retired-ids.toml") return new Uint8Array(retired);
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    registry(revision) {
      return {
        ...emptyRegistry(revision),
        production_output_stage: "both_authoritative",
        output_claims: productionOutputInventory("both_authoritative").claims,
        matrix_status_inputs: liveMatrixStatusInputs(),
      };
    },
    atomic,
  });
}

function wp4mBootstrapProbePorts(
  context: SelfTestContext,
  onBaseRead: (path: RepoPath) => void,
): RoadmapCliPorts {
  const preRoots = validGenericAllPorts(context);
  const candidateMatrix = liveMatrixAuthoritativeSource();
  const baseMatrix = liveMatrixShadowV0Source();
  const matrixProjection = liveMatrixProjection();
  const shadowTesting = liveTestingShadowV0Source();
  const testingProjection = liveTestingProjection();
  const campaign = UTF8.encode(`[campaign]
schema_version = 1
matrix_authority = "authoritative"
testing_authority = "shadow"
`);
  const retired = UTF8.encode(`[retired_ids]
schema_version = 1
`);
  return fakePorts({
    read(path) {
      if (path === "roadmap-campaign.toml") return new Uint8Array(campaign);
      if (path === "roadmap-retired-ids.toml") return new Uint8Array(retired);
      if (path === "cddl-matrix/roadmap.toml") return new Uint8Array(candidateMatrix);
      if (path === "cddl-matrix/ROADMAP.md") return new Uint8Array(matrixProjection);
      if (path === "tests/testing-roadmap.toml") return new Uint8Array(shadowTesting);
      if (path === "tests/TESTING_ROADMAP.md") return new Uint8Array(testingProjection);
      return preRoots.read.readDeclared(path);
    },
    readAtCommit(path) {
      onBaseRead(path);
      if (path === "roadmap-campaign.toml" || path === "roadmap-retired-ids.toml") {
        throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
      }
      if (path === "cddl-matrix/roadmap.toml") return new Uint8Array(baseMatrix);
      if (path === "cddl-matrix/ROADMAP.md") return new Uint8Array(matrixProjection);
      if (path === "tests/testing-roadmap.toml") return new Uint8Array(shadowTesting);
      if (path === "tests/TESTING_ROADMAP.md") return new Uint8Array(testingProjection);
      return preRoots.read.readDeclared(path);
    },
    registry(revision) {
      const stage = revision.kind === "worktree" ? "matrix_authoritative" : "pre_cutover";
      return {
        ...preRoots.read.registryView(revision),
        production_output_stage: stage,
        output_claims: productionOutputInventory(stage).claims,
        matrix_status_inputs: liveMatrixStatusInputs(),
      };
    },
  });
}

function wp4mMixedAuthorityPorts(
  context: SelfTestContext,
  candidateReads: RepoPath[],
  baseReads: RepoPath[],
): RoadmapCliPorts {
  const matrixSourcePath = "cddl-matrix/roadmap.toml" as RepoPath;
  const matrixProjectionPath = "cddl-matrix/ROADMAP.md" as RepoPath;
  const testingSourcePath = "tests/testing-roadmap.toml" as RepoPath;
  const testingProjectionPath = "tests/TESTING_ROADMAP.md" as RepoPath;
  const baseMatrixSource = liveMatrixShadowV0Source();
  const candidateMatrixSource = liveMatrixAuthoritativeSource();
  const candidateProjection = liveMatrixProjection();
  const testingMarkdown = UTF8.encode("# Testing legacy authority\n");
  const campaign = UTF8.encode(`[campaign]
schema_version = 1
matrix_authority = "authoritative"
testing_authority = "legacy_markdown"
`);
  const retired = UTF8.encode(`[retired_ids]
schema_version = 1
`);
  return fakePorts({
    read(path) {
      candidateReads.push(path);
      if (path === matrixSourcePath) return new Uint8Array(candidateMatrixSource);
      if (path === matrixProjectionPath) return new Uint8Array(candidateProjection);
      if (path === testingProjectionPath) return new Uint8Array(testingMarkdown);
      if (path === "roadmap-campaign.toml") return new Uint8Array(campaign);
      if (path === "roadmap-retired-ids.toml") return new Uint8Array(retired);
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    readAtCommit(path) {
      baseReads.push(path);
      if (path === matrixSourcePath) return new Uint8Array(baseMatrixSource);
      if (path === matrixProjectionPath) return new Uint8Array(candidateProjection);
      if (path === testingProjectionPath) return new Uint8Array(testingMarkdown);
      if (path === testingSourcePath || path === "roadmap-campaign.toml" || path === "roadmap-retired-ids.toml") {
        throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
      }
      throw new Error(`unexpected WP4M base read ${path}`);
    },
    registry(revision) {
      const stage = revision.kind === "worktree" ? "matrix_authoritative" : "pre_cutover";
      return {
        ...emptyRegistry(revision),
        production_output_stage: stage,
        output_claims: productionOutputInventory(stage).claims,
        matrix_status_inputs: liveMatrixStatusInputs(),
      };
    },
  });
}

function shadowLifecyclePorts(
  projection: Uint8Array,
  reads: RepoPath[],
): RoadmapCliPorts {
  const matrixSourcePath = "cddl-matrix/roadmap.toml" as RepoPath;
  const matrixProjectionPath = "cddl-matrix/ROADMAP.md" as RepoPath;
  const matrixSource = liveMatrixShadowV0Source();
  const campaign = UTF8.encode(`[campaign]
schema_version = 1
matrix_authority = "shadow"
testing_authority = "legacy_markdown"
`);
  const retired = UTF8.encode(`[retired_ids]
schema_version = 1
`);
  return fakePorts({
    read(path) {
      reads.push(path);
      if (path === "roadmap-campaign.toml") return new Uint8Array(campaign);
      if (path === "roadmap-retired-ids.toml") return new Uint8Array(retired);
      if (path === matrixSourcePath) return new Uint8Array(matrixSource);
      if (path === matrixProjectionPath) return new Uint8Array(projection);
      if (path === "tests/TESTING_ROADMAP.md") return UTF8.encode("# Testing legacy authority\n");
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    registry(revision) {
      return { ...emptyRegistry(revision), matrix_status_inputs: liveMatrixStatusInputs() };
    },
  });
}

function scopedCandidateCollisionProbe(context: SelfTestContext): {
  readonly ports: RoadmapCliPorts;
  readonly candidate_reads: RepoPath[];
  readonly base_reads: RepoPath[];
  readonly collision_id: string;
} {
  const testingSourcePath = "tests/testing-roadmap.toml" as RepoPath;
  const testingSource = UTF8.encode(text(fixture(context, "positive/mixed-testing-v1.toml"))
    .replace("cddl-matrix/roadmap/fixtures/positive/mixed-testing-v1.toml", testingSourcePath)
    .replace("cddl-matrix/roadmap/fixtures/positive/mixed-testing-v1.expected.md", "tests/TESTING_ROADMAP.md"));
  const matrixSource = liveMatrixAuthoritativeSource();
  const matrixMarkdown = liveMatrixProjection();
  const collisionId = "matrix.additional-tool-annotations";
  const campaign = UTF8.encode(`[campaign]
schema_version = 1
matrix_authority = "authoritative"
testing_authority = "authoritative"
`);
  const retired = UTF8.encode(`[retired_ids]
schema_version = 1

[[retired_ids.entry]]
id = "${collisionId}"
last_active_at = "1111111111111111111111111111111111111111"

[retired_ids.entry.replacement]
kind = "gate"
gate_id = "durable-gate"
claim_md = """The durable gate owns the replacement claim."""
`);
  const candidateReads: RepoPath[] = [];
  const baseReads: RepoPath[] = [];
  const candidateSources = new Map<RepoPath, Uint8Array>([
    [testingSourcePath, testingSource],
    ["cddl-matrix/roadmap.toml" as RepoPath, matrixSource],
    ["cddl-matrix/ROADMAP.md" as RepoPath, matrixMarkdown],
    ["roadmap-campaign.toml" as RepoPath, campaign],
    ["roadmap-retired-ids.toml" as RepoPath, retired],
  ]);
  const ports = fakePorts({
    read(path) {
      candidateReads.push(path);
      const value = candidateSources.get(path);
      if (value !== undefined) return new Uint8Array(value);
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    readAtCommit(path) {
      baseReads.push(path);
      if (path === testingSourcePath) return new Uint8Array(testingSource);
      throw new Error(`single-roadmap comparison read unselected base path ${path}`);
    },
    registry(revision) {
      const stage = "both_authoritative";
      return {
        ...emptyRegistry(revision),
        production_output_stage: stage,
        output_claims: productionOutputInventory(stage).claims,
        gates: [{ id: "durable-gate", kind: "cmd", stub: false }],
        matrix_status_inputs: liveMatrixStatusInputs(),
      };
    },
  });
  return {
    ports,
    candidate_reads: candidateReads,
    base_reads: baseReads,
    collision_id: collisionId,
  };
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
          `pre-WP4M ${roadmap} check did not succeed without campaign/retired roots: ${text(result.stderr)}`,
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
    case "cli_write_authoritative_matrix": {
      const candidateReads: RepoPath[] = [];
      const baseReads: RepoPath[] = [];
      const replacements: { path: RepoPath; bytes: Uint8Array }[] = [];
      const fixturePorts = wp4mMixedAuthorityPorts(context, candidateReads, baseReads);
      const ports = fakePorts({
        read: fixturePorts.read.readDeclared,
        readAtCommit: (path) => fixturePorts.read.readDeclaredAtCommit(HASH as import("../model/core.ts").FullCommitId, path),
        registry: fixturePorts.read.registryView,
        atomic: (path, value) => replacements.push({ path, bytes: new Uint8Array(value) }),
      });
      const result = run(["--write", "--roadmap", "matrix"], ports);
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 && replacements.length === 1 &&
          replacements[0].path === "cddl-matrix/ROADMAP.md" && replacements[0].bytes.byteLength > 0 &&
          text(result.stdout).startsWith("WRITE OK roadmap=matrix target=cddl-matrix/ROADMAP.md bytes="),
        `authoritative matrix write did not apply exactly one validated whole-file plan: ${text(result.stderr)}`,
      );
      return pass("positive");
    }
    case "cli_write_authoritative_testing": {
      const replacements: { path: RepoPath; bytes: Uint8Array }[] = [];
      const result = run(
        ["--write", "--roadmap", "testing"],
        bothAuthoritativePorts((path, value) => {
          replacements.push({ path, bytes: new Uint8Array(value) });
        }),
      );
      const expected = liveTestingProjection();
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 && replacements.length === 1 &&
          replacements[0].path === "tests/TESTING_ROADMAP.md" &&
          replacements[0].bytes.byteLength === expected.byteLength &&
          replacements[0].bytes.every((value, index) => value === expected[index]) &&
          text(result.stdout).startsWith(
            "WRITE OK roadmap=testing target=tests/TESTING_ROADMAP.md bytes=",
          ),
        `authoritative testing write did not apply the exact committed whole-file projection: ${text(result.stderr)}`,
      );
      return pass("positive");
    }
    case "cli_query_each_view": {
      for (const view of ["summary", "debt", "references", "signals", "output-owners"]) {
        expectFailure(
          ["--roadmap", "testing", "--query", view],
          expectedIssue("E-SOURCE-MISSING", "tests/testing-roadmap.toml", "$", "declared source is missing", 1),
        );
      }
      const campaign = run(["--roadmap", "testing", "--query", "campaign", "--json"]);
      assert(
        campaign.exit_code === 0 && campaign.stderr.byteLength === 0 &&
          JSON.parse(text(campaign.stdout)).state === "not_activated",
        "pre-WP4M campaign query did not return the explicit inactive-stage payload",
      );
      const decodedCampaign = run(
        ["--roadmap", "all", "--query", "campaign", "--json"],
        decodedCampaignPorts(),
      );
      const decoded = JSON.parse(text(decodedCampaign.stdout));
      assert(
        decodedCampaign.exit_code === 0 && decodedCampaign.stderr.byteLength === 0 &&
          decoded.campaign.campaign.schema_version === 1 &&
          decoded.campaign.campaign.matrix_authority === "legacy_markdown" &&
          decoded.campaign.campaign.testing_authority === "legacy_markdown",
        `campaign query did not expose the decoded lifecycle document: ${text(decodedCampaign.stderr)}`,
      );
      return pass("positive", ["summary", "debt", "references", "campaign", "signals", "output-owners"]);
    }
    case "cli_no_args_rejected": expectFailure([], cliIssue("E-CLI-MODE", 0, "exactly one primary mode is required"), fakePorts(), true); return pass("negative");
    case "cli_unknown_option": expectFailure(["--wat"], cliIssue("E-CLI-UNKNOWN-OPTION", 0, 'unknown option "--wat"'), fakePorts(), true); return pass("negative");
    case "cli_missing_value": expectFailure(["--query"], cliIssue("E-CLI-MISSING-VALUE", 0, "--query requires a value"), fakePorts(), true); return pass("negative");
    case "cli_duplicate_scalar": expectFailure(["--check", "--roadmap", "matrix", "--roadmap", "testing"], cliIssue("E-CLI-DUPLICATE-OPTION", 3, "--roadmap may occur exactly once"), fakePorts(), true); return pass("negative");
    case "cli_duplicate_primary_mode": expectFailure(["--check", "--write", "--roadmap", "matrix"], cliIssue("E-CLI-MODE", 1, "exactly one primary mode is required"), fakePorts(), true); return pass("negative");
    case "cli_roadmap_required": expectFailure(["--check"], cliIssue("E-CLI-ROADMAP", 0, "--roadmap is required for this mode"), fakePorts(), true); return pass("negative");
    case "cli_roadmap_forbidden_on_format": expectFailure(["--format-source", "cddl-matrix/roadmap.toml", "--roadmap", "matrix"], cliIssue("E-CLI-INCOMPATIBLE", 2, "--roadmap is forbidden for this mode"), fakePorts(), true); return pass("negative");
    case "cli_write_all_rejected": expectFailure(["--write", "--roadmap", "all"], cliIssue("E-CLI-INCOMPATIBLE", 1, "--write requires matrix or testing, not all"), fakePorts(), true); return pass("negative");
    case "cli_against_requires_check":
    case "cli_against_write_rejected": expectFailure(["--write", "--roadmap", "matrix", "--against", "bad"], cliIssue("E-CLI-AGAINST", 3, "--against is valid only with --check"), fakePorts(), true); return pass("negative");
    case "cli_against_query_rejected": expectFailure(["--query", "summary", "--roadmap", "matrix", "--against", "bad"], cliIssue("E-CLI-AGAINST", 4, "--against is valid only with --check"), fakePorts(), true); return pass("negative");
    case "cli_against_selftest_rejected": expectFailure(["--selftest", "--against", "bad"], cliIssue("E-CLI-AGAINST", 1, "--against is valid only with --check"), fakePorts(), true); return pass("negative");
    case "cli_against_format_rejected": expectFailure(["--format-source", "cddl-matrix/roadmap.toml", "--against", "bad"], cliIssue("E-CLI-AGAINST", 2, "--against is valid only with --check"), fakePorts(), true); return pass("negative");
    case "cli_json_requires_query": expectFailure(["--check", "--roadmap", "matrix", "--json"], cliIssue("E-CLI-INCOMPATIBLE", 3, "--json is valid only with --query"), fakePorts(), true); return pass("negative");
    case "cli_as_of_requires_query": expectFailure(["--check", "--roadmap", "matrix", "--as-of", "2024-01-01"], cliIssue("E-CLI-INCOMPATIBLE", 3, "--as-of is valid only with --query"), fakePorts(), true); return pass("negative");
    case "cli_format_target_declared_only": expectFailure(["--format-source", "draft/roadmap.toml"], cliIssue("E-CLI-FORMAT-TARGET", 0, "--format-source must name a declared roadmap TOML source"), fakePorts(), true); return pass("negative");
    case "cli_against_missing_value": expectFailure(["--check", "--roadmap", "matrix", "--against"], cliIssue("E-CLI-MISSING-VALUE", 3, "--against requires a value"), fakePorts(), true); return pass("negative");
    case "cli_against_duplicate_value": expectFailure(["--check", "--roadmap", "matrix", "--against", HASH, "--against", HASH], cliIssue("E-CLI-DUPLICATE-OPTION", 5, "--against may occur exactly once"), fakePorts(), true); return pass("negative");
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
    ["--roadmap", "testing", "--query", "signals", "--as-of", value],
    cliIssue("E-CLI-AS-OF", 4, "--as-of must be an existing Gregorian date in YYYY-MM-DD form"),
    fakePorts(),
    true,
  );
  return pass("negative");
}

function gitAndExitCase(id: RequiredCliSelfTestCaseId, context: SelfTestContext): SelfTestResult | undefined {
  switch (id) {
    case "cli_against_bad_length_git_format_no_usage":
      expectFailure(["--check", "--roadmap", "matrix", "--against", "abc"], expectedIssue("E-GIT-BASE-FORMAT", "<git>", "against", "--against must be exactly 40 lowercase hexadecimal characters for repository object format sha1", 2)); return pass("negative");
    case "cli_against_uppercase_git_format_no_usage":
      expectFailure(["--check", "--roadmap", "matrix", "--against", "A".repeat(40)], expectedIssue("E-GIT-BASE-FORMAT", "<git>", "against", "--against must be exactly 40 lowercase hexadecimal characters for repository object format sha1", 2)); return pass("negative");
    case "cli_against_nonhex_git_format_no_usage":
      expectFailure(["--check", "--roadmap", "matrix", "--against", "z".repeat(40)], expectedIssue("E-GIT-BASE-FORMAT", "<git>", "against", "--against must be exactly 40 lowercase hexadecimal characters for repository object format sha1", 2)); return pass("negative");
    case "cli_unresolved_base_exit_two": {
      let lookups = 0;
      expectFailure(
        ["--check", "--roadmap", "matrix", "--against", HASH],
        expectedIssue("E-GIT-BASE-LOOKUP", "<git>", "against", "--against names no commit object with that exact object ID", 2),
        fakePorts({ resolve: () => { lookups += 1; throw new Error("unresolved object"); } }),
      );
      assert(lookups === 1, "unresolved Git vector did not perform exactly one lookup");
      return pass("negative");
    }
    case "cli_noncommit_base_exit_two": {
      const NONCOMMIT = "b".repeat(40);
      let lookups = 0;
      expectFailure(
        ["--check", "--roadmap", "testing", "--against", NONCOMMIT],
        expectedIssue("E-GIT-BASE-LOOKUP", "<git>", "against", "--against names no commit object with that exact object ID", 2),
        fakePorts({ resolve: () => { lookups += 1; throw new Error("object is not a commit"); } }),
      );
      assert(lookups === 1, "noncommit Git vector did not perform exactly one independent lookup");
      return pass("negative");
    }
    case "cli_against_unresolved_git_lookup_no_usage":
      expectFailure(
        ["--check", "--roadmap", "matrix", "--against", HASH],
        expectedIssue("E-GIT-BASE-LOOKUP", "<git>", "against", "--against names no commit object with that exact object ID", 2),
        fakePorts({ resolve: () => "b".repeat(40) }),
      );
      return pass("negative");
    case "cli_against_incompatible_precedes_bad_format": {
      let objectReads = 0;
      expectFailure(["--write", "--roadmap", "matrix", "--against", "bad"], cliIssue("E-CLI-AGAINST", 3, "--against is valid only with --check"), fakePorts({ objectFormat: () => { objectReads++; return "sha1"; } }), true);
      assert(objectReads === 0, "incompatible --against consulted Git format");
      return pass("negative");
    }
    case "exit_declared_source_enoent_one": expectFailure(["--query", "summary", "--roadmap", "matrix"], expectedIssue("E-SOURCE-MISSING", "cddl-matrix/roadmap.toml", "$", "declared source is missing", 1)); return pass("negative");
    case "exit_declared_projection_enoent_one": {
      const valid = validTestingPorts(context);
      const ports = fakePorts({ read: (path) => {
        if (path === "tests/TESTING_ROADMAP.md") {
          throw roadmapFailure("E-SOURCE-MISSING", path, "projection", "projection missing", 1);
        }
        return valid.read.readDeclared(path);
      }, registry: valid.read.registryView });
      expectFailure(["--check", "--roadmap", "testing"], expectedIssue("E-PROJECTION-MISSING", "tests/TESTING_ROADMAP.md", "projection", "projection missing", 1), ports);
      return pass("negative");
    }
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
      ) => fakePorts({
        read: (path) => path === "cddl-matrix/roadmap.toml"
          ? new Uint8Array(source)
          : (() => { throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1); })(),
        registry: (revision) => ({
          ...emptyRegistry(revision),
          production_output_stage: stage,
          output_claims: productionOutputInventory(stage).claims,
          matrix_status_inputs: liveMatrixStatusInputs(),
        }),
      });
      for (const [source, stage, message] of [
        [
          liveMatrixAuthoritativeSource(),
          "pre_cutover",
          "authoritative roadmap requires its same-revision production whole-file projection claim",
        ],
        [
          liveMatrixShadowV0Source(),
          "matrix_authoritative",
          "shadow roadmap forbids an authoritative whole-file projection claim",
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
          mismatch(source, stage),
        );
      }
      const lifecycle = wp4mMixedAuthorityPorts(context, [], []);
      const recognizedWrongStage = fakePorts({
        read: lifecycle.read.readDeclared,
        registry: (revision) => ({
          ...lifecycle.read.registryView(revision),
          production_output_stage: "both_authoritative",
          output_claims: productionOutputInventory("both_authoritative").claims,
        }),
      });
      for (const argv of [
        ["--roadmap", "matrix", "--query", "summary"],
        ["--roadmap", "matrix", "--check"],
        ["--format-source", "cddl-matrix/roadmap.toml"],
      ]) {
        expectFailure(
          argv,
          expectedIssue(
            "E-OUTPUT-CLAIM",
            "<output-registry>",
            "production_output_stage",
            "revision registry stage both_authoritative does not match campaign stage matrix_authoritative",
            1,
          ),
          recognizedWrongStage,
        );
      }
      const preActivationWrongStage = mismatch(liveMatrixAuthoritativeSource(), "both_authoritative");
      for (const argv of [
        ["--roadmap", "matrix", "--query", "summary"],
        ["--roadmap", "matrix", "--check"],
      ]) {
        expectFailure(
          argv,
          expectedIssue(
            "E-OUTPUT-CLAIM",
            "<output-registry>",
            "production_output_stage",
            "pre-activation registry stage both_authoritative does not match canonical stage pre_cutover",
            1,
          ),
          preActivationWrongStage,
        );
      }
      return pass("negative", [
        "authoritative_without_claim",
        "shadow_with_claim",
        "recognized_wrong_stage",
        "preactivation_recognized_wrong_stage",
      ]);
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
    case "cli_as_of_valid_leap_day": {
      const result = run(["--roadmap", "testing", "--query", "signals", "--as-of", "2024-02-29", "--json"], validTestingPorts(context));
      assert(result.exit_code === 0 && text(result.stdout).includes("2024-02-29"), "valid leap day was not carried into query output");
      return pass("positive");
    }
    case "query_as_of_due_date_inclusive":
    case "query_as_of_valid_through_inclusive":
    case "query_as_of_after_valid_through_stale": {
      const date = id === "query_as_of_after_valid_through_stale" ? "2025-01-02" : "2025-01-01";
      const kind = id === "query_as_of_due_date_inclusive" ? "cadence" : "evidence";
      const result = run(
        ["--roadmap", "testing", "--query", "signals", "--as-of", date, "--json"],
        temporalTestingPorts(context, kind),
      );
      assert(result.exit_code === 0, `${id} service query failed: ${text(result.stderr)}`);
      const payload = JSON.parse(text(result.stdout));
      const expected = id === "query_as_of_due_date_inclusive"
        ? "due"
        : id === "query_as_of_valid_through_inclusive" ? "as_of" : "stale";
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 && payload.evaluation_as_of === date &&
          payload.signals.length === 1 && payload.signals[0].id === "testing.fixture-mixed-semantic" &&
          payload.signals[0].evaluation === expected,
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
      const result = run(["--roadmap", "testing", "--query", "signals", "--json"], ports);
      assert(result.exit_code === 0, `no-as-of service query failed: ${text(result.stderr)}`);
      const payload = JSON.parse(text(result.stdout));
      assert(
        result.exit_code === 0 && payload.evaluation_as_of === null && payload.signals.length === 1 &&
          payload.signals[0].evaluation === "unknown_no_as_of",
        "no-as-of query fabricated time or did not preserve the decoded unknown posture",
      );
      assert(!paths.includes("tests/TESTING_ROADMAP.md" as RepoPath), "query read the committed projection");
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
    case "cli_wp4m_mixed_authority_all": {
      const candidateReads: RepoPath[] = [];
      const baseReads: RepoPath[] = [];
      const ports = wp4mMixedAuthorityPorts(context, candidateReads, baseReads);
      const checked = run(["--check", "--roadmap", "all", "--against", HASH], ports);
      assert(
        checked.exit_code === 0 && checked.stderr.byteLength === 0 &&
          text(checked.stdout).includes("source=cddl-matrix/roadmap.toml") &&
          !text(checked.stdout).includes("source=tests/testing-roadmap.toml"),
        `valid WP4M mixed-authority bootstrap did not check only its structured roadmap: ${text(checked.stderr)}`,
      );
      assert(
        candidateReads.filter((path) => path === "tests/testing-roadmap.toml").length === 2 &&
          baseReads.filter((path) => path === "tests/testing-roadmap.toml").length === 1,
        `mixed-authority check did not prove testing TOML absence without preparing it: candidate=${JSON.stringify(candidateReads)} base=${JSON.stringify(baseReads)}`,
      );

      candidateReads.length = 0;
      baseReads.length = 0;
      const queried = run(["--roadmap", "all", "--query", "summary", "--json"], ports);
      const payload = JSON.parse(text(queried.stdout));
      assert(
        queried.exit_code === 0 && queried.stderr.byteLength === 0 && payload.roadmaps.length === 1 &&
          payload.roadmaps[0].roadmap === "matrix" && payload.roadmaps[0].authority === "authoritative",
        `mixed-authority query all did not return exactly its activated structured roadmap: ${text(queried.stderr)}`,
      );
      assert(
        candidateReads.filter((path) => path === "tests/testing-roadmap.toml").length === 1 && baseReads.length === 0,
        `mixed-authority query prepared the absent testing TOML or consulted a base: candidate=${JSON.stringify(candidateReads)} base=${JSON.stringify(baseReads)}`,
      );
      return pass("positive");
    }
    case "cli_shadow_authoritative_markdown_provenance": {
      const validProjection = liveMatrixProjection();
      const validReads: RepoPath[] = [];
      const valid = run(
        ["--roadmap", "all", "--query", "summary", "--json"],
        shadowLifecyclePorts(validProjection, validReads),
      );
      assert(valid.exit_code === 0, `matching shadow Markdown did not validate: ${text(valid.stderr)}`);
      assert(
        validReads.filter((path) => path === "cddl-matrix/ROADMAP.md").length === 1,
        `matching shadow Markdown was not read exactly once: ${JSON.stringify(validReads)}`,
      );
      const driftedProjection = new Uint8Array(validProjection.byteLength + 1);
      driftedProjection.set(validProjection);
      driftedProjection[driftedProjection.byteLength - 1] = 0x21;
      const driftReads: RepoPath[] = [];
      const drifted = run(
        ["--roadmap", "all", "--query", "summary", "--json"],
        shadowLifecyclePorts(driftedProjection, driftReads),
      );
      assert(
        drifted.exit_code === 1 && drifted.stdout.byteLength === 0 &&
          text(drifted.stderr).startsWith("FAIL [E-PROJECTION-DRIFT] cddl-matrix/ROADMAP.md#projection:"),
        `shadow query did not reject bytes from authoritative Markdown: ${text(drifted.stderr)}`,
      );
      assert(
        driftReads.filter((path) => path === "cddl-matrix/ROADMAP.md").length === 1,
        `drifted shadow Markdown was not read exactly once: ${JSON.stringify(driftReads)}`,
      );
      return pass("positive");
    }
    case "cli_against_matrix_check_allowed":
    case "cli_against_testing_check_allowed":
    case "cli_against_all_check_allowed": {
      const roadmap = id.includes("matrix") ? "matrix" : id.includes("testing") ? "testing" : "all";
      if (roadmap === "all") {
        const result = run(["--check", "--roadmap", "all", "--against", HASH], fakePorts());
        assert(
          result.exit_code === 1 && result.stdout.byteLength === 0 &&
            text(result.stderr) ===
              "FAIL [E-SOURCE-MISSING] roadmap-campaign.toml#$: declared source is missing\n" +
              "FAIL [E-SOURCE-MISSING] roadmap-retired-ids.toml#$: declared source is missing\n" +
              "FAILED: 2 issue(s)\n",
          `all-roadmap allowed coordinate lost its exact two-root diagnostic: ${text(result.stderr)}`,
        );
        const baseReads: RepoPath[] = [];
        const probe = wp4mBootstrapProbePorts(context, (path) => baseReads.push(path));
        const transaction = run(["--check", "--roadmap", "all", "--against", HASH], probe);
        assert(
          transaction.exit_code === 0 && transaction.stderr.byteLength === 0 &&
            text(transaction.stdout).includes("CHECK OK\n"),
          `valid bootstrap probe fixture did not complete the allowed all-roadmap comparison: ${text(transaction.stderr)}`,
        );
        assert(
          baseReads.includes("roadmap-campaign.toml" as RepoPath) &&
            baseReads.includes("roadmap-retired-ids.toml" as RepoPath) &&
            baseReads.includes("tests/testing-roadmap.toml" as RepoPath),
          `WP4M bootstrap did not prove both roots absent and load testing v0 for shadow authority: ${JSON.stringify(baseReads)}`,
        );
        return pass("positive");
      }
      const source = roadmap === "testing" ? "tests/testing-roadmap.toml" : "cddl-matrix/roadmap.toml";
      expectFailure(
        ["--check", "--roadmap", roadmap, "--against", HASH],
        expectedIssue("E-SOURCE-MISSING", source, "$", "declared source is missing", 1),
      );
      if (roadmap === "testing") {
        const probe = scopedCandidateCollisionProbe(context);
        const collision = run(["--check", "--roadmap", "testing", "--against", HASH], probe.ports);
        assert(
          collision.exit_code === 1 && collision.stdout.byteLength === 0 &&
            text(collision.stderr) ===
              `FAIL [E-OWNER-DUPLICATE] <identity>#owner[${JSON.stringify(probe.collision_id)}]: global ID ${JSON.stringify(probe.collision_id)} has 2 claims (first_class, tombstone)\n` +
              "FAILED: 1 issue(s)\n",
          `single-roadmap service did not reject the unselected candidate/tombstone collision: ${text(collision.stderr)}`,
        );
        assert(
          JSON.stringify(probe.candidate_reads) === JSON.stringify([
            "tests/testing-roadmap.toml",
            "roadmap-campaign.toml",
            "roadmap-retired-ids.toml",
            "cddl-matrix/roadmap.toml",
          ]),
          `single-roadmap service did not read the exact full candidate authority universe: ${JSON.stringify(probe.candidate_reads)}`,
        );
        assert(
          JSON.stringify(probe.base_reads) === JSON.stringify(["tests/testing-roadmap.toml"]),
          `single-roadmap service read more than the selected base roadmap: ${JSON.stringify(probe.base_reads)}`,
        );
      }
      return pass("positive");
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
          fixtureCount >= 35 && fixtureBytes > 0 &&
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
      const invalid = runRoadmapCli(["--wat"], invalidPorts);
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
