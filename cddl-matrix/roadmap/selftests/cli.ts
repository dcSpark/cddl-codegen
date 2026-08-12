import type { RegistryView } from "../adapters/types.ts";
import { ROADMAP_CLI_USAGE } from "../cli.ts";
import { composeRoadmapDocument } from "../compose.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import { decodeCampaignSource } from "../decode/campaign.ts";
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
import type { RoadmapDocumentV0, RoadmapDocumentV1, SemanticPayload } from "../model/documents.ts";
import type { MatrixStatusInputs } from "../model/matrix.ts";
import {
  LEGACY_STATUS_OUTPUT_CLAIMS,
  productionOutputInventory,
} from "../output_registry.ts";
import type { SelfTestCandidateCase as SelfTestCase, SelfTestContext, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import { validateSelectedLifecycleContext } from "../transaction.ts";
import { observeSelfTestIssue } from "./observations.ts";
import {
  liveMatrixAuthoritativeDocument,
  liveMatrixAuthoritativeSource,
  liveMatrixProjection,
  liveMatrixShadowV0Source,
} from "./live_matrix.ts";
import {
  liveTestingAuthoritativeDocument,
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
  "live_subordinate_lifecycle_dispositions",
  "query_debt_live_migration_floors",
  "query_debt_scoped_does_not_load_other_roadmap",
  "query_debt_all_reports_both",
  "query_debt_duplicate_campaign_selection_rejected",
  "query_debt_unselected_duplicate_selection_rejected",
  "query_debt_unselected_duplicate_reservation_rejected",
  "query_debt_selected_shadow_reservation_coalesces",
  "query_debt_selected_shadow_title_binding_rejected",
  "query_debt_unselected_legacy_selection_missing_reservation_rejected",
  "query_debt_unselected_active_reservation_rejected",
  "query_debt_unselected_reservation_guard_tombstone_rejected",
  "query_debt_selected_alias_reservation_rejected",
  "query_debt_invalid_campaign_target_rejected",
  "query_debt_invalid_campaign_state_rejected",
  "query_debt_authoritative_reservation_rejected",
  "query_debt_active_tombstone_collision_rejected",
  "query_debt_active_guard_collision_rejected",
  "query_debt_active_reservation_collision_rejected",
  "query_debt_invalid_guard_pin_rejected",
  "query_debt_authority_mismatch_rejected",
  "query_debt_missing_campaign_root_rejected",
  "query_debt_missing_retired_root_rejected",
  "query_debt_invalid_retired_pin_rejected",
  "query_debt_stage_mismatch_rejected",
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
  "cli_against_semantic_promotion_scoped_matrix",
  "cli_against_semantic_promotion_scoped_testing",
  "cli_against_semantic_promotion_all_simultaneous",
  "cli_against_semantic_promotion_other_base_not_loaded",
  "cli_against_structural_promotion_scoped_matrix",
  "cli_against_structural_promotion_scoped_testing",
  "cli_against_structural_promotion_all_simultaneous",
  "cli_against_structural_promotion_other_base_not_loaded",
  "cli_against_structural_record_composition",
  "cli_against_structural_semantic_only_composition",
  "cli_semantic_conversion_current_omission_rejected",
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

function promotionDocuments(
  _context: SelfTestContext,
  roadmap: "matrix" | "testing",
  options: {
    readonly record?: boolean;
    readonly structural?: boolean;
    readonly semantic_only?: boolean;
  } = {},
): { readonly base: RoadmapDocumentV1; readonly candidate: RoadmapDocumentV1; readonly projection: Uint8Array } {
  const sourcePath = (roadmap === "matrix" ? "cddl-matrix/roadmap.toml" : "tests/testing-roadmap.toml") as RepoPath;
  const source = roadmap === "matrix" ? liveMatrixAuthoritativeSource() : liveTestingAuthoritativeSource();
  const decoded = decodeRoadmapSource(source, sourcePath, roadmap, true);
  if (decoded.document.schema_version !== 1) throw new Error("promotion fixture is not v1");
  const v1 = decoded as RoadmapDocumentV1;
  const rawRecord = v1.records.find((record) =>
    "source_block_md" in record && record.span_ids.length === 1 &&
    "semantic_shadow" in record && record.semantic_shadow !== undefined
  );
  if (rawRecord === undefined || !("source_block_md" in rawRecord)) throw new Error("promotion fixture lacks raw shadow");
  const payload: SemanticPayload = {
    kind: "work",
    summary_md: rawRecord.source_block_md,
    work_state: "ready",
    work_intent: roadmap === "matrix" ? "build_capability" : "build_system",
    work_kind: roadmap === "matrix" ? "feature" : "infrastructure",
    risk: roadmap === "matrix" ? "cosmetic" : "false_pass_or_red",
    family_classification: "none_reviewed",
    acceptance_md: UTF8.encode("Reviewed promotion preserves the exact legacy bytes."),
    priority_rationale_md: UTF8.encode("Fixture promotion is transaction-scoped."),
  };
  const includeRecord = options.record ?? true;
  const includeStructural = options.structural ?? false;
  const base: RoadmapDocumentV1 = {
    ...v1,
    records: includeRecord
      ? v1.records.map((record) => record === rawRecord ? { ...rawRecord, semantic_shadow: payload } : record)
      : v1.records,
  };
  const section = includeStructural ? base.sections.find((value) =>
    "source_block_md" in value && value.span_ids.length === 1
  ) : undefined;
  const fragment = includeStructural ? base.fragments.find((value) =>
    "source_block_md" in value && value.span_ids.length === 1 &&
    "lifecycle_disposition" in value && value.lifecycle_disposition === "document_prose"
  ) : undefined;
  const part = includeStructural ? base.parts.find((value) =>
    "source_block_md" in value && value.span_ids.length === 1 &&
    "lifecycle_disposition" in value && value.lifecycle_disposition === "parent_supporting_prose"
  ) : undefined;
  const rawSection = section !== undefined && "source_block_md" in section ? section : undefined;
  const rawFragment = fragment !== undefined && "source_block_md" in fragment ? fragment : undefined;
  const rawPart = part !== undefined && "source_block_md" in part ? part : undefined;
  if (includeStructural && (rawSection === undefined || rawFragment === undefined || rawPart === undefined)) {
    throw new Error(`${roadmap} structural promotion fixture lacks reviewed singleton owners`);
  }
  const structuralSpanIds = new Set([
    ...(rawSection?.span_ids ?? []),
    ...(rawFragment?.span_ids ?? []),
    ...(rawPart?.span_ids ?? []),
  ]);
  const convertedSpanIds = new Set([
    ...(includeRecord ? rawRecord.span_ids : []),
    ...structuralSpanIds,
  ]);
  const replacement = (span_id: (typeof base.spans)[number]["id"]) => ({
    span_id,
    replacement_field: "body_md",
    review_note_md: UTF8.encode("Reviewed exact same-kind structural promotion."),
  });
  let records: RoadmapDocumentV1["records"] = includeRecord
    ? base.records.map((record) => record.id === rawRecord.id ? {
      id: rawRecord.id,
      title: rawRecord.title,
      projection_group: rawRecord.projection_group,
      render_authority: "semantic" as const,
      projection_visibility: "document" as const,
      payload,
      source_replacements: rawRecord.span_ids.map((span_id) => ({
        span_id,
        replacement_field: "payload.summary_md",
        review_note_md: UTF8.encode("Reviewed exact raw-span promotion."),
      })),
    } : record)
    : base.records;
  let manifest = base.manifest;
  if (options.semantic_only === true) {
    const semanticOnlyId = `${roadmap}.fixture-semantic-only-structural` as (typeof rawRecord)["id"];
    records = [...records, {
      id: semanticOnlyId,
      title: "Semantic-only structural companion",
      projection_group: rawRecord.projection_group,
      render_authority: "semantic",
      projection_visibility: "semantic_only",
      payload,
      source_replacements: [],
    }];
    manifest = [...manifest, { kind: "record", record_id: semanticOnlyId }];
  }
  const candidate: RoadmapDocumentV1 = {
    ...base,
    document: {
      ...base.document,
      frozen_legacy_span_ids: base.document.frozen_legacy_span_ids.filter((spanId) => !convertedSpanIds.has(spanId)),
    },
    sections: rawSection === undefined ? base.sections : base.sections.map((value) => value !== rawSection ? value : {
      section_id: rawSection.section_id,
      title: rawSection.title,
      ...(rawSection.legacy_aliases === undefined ? {} : { legacy_aliases: rawSection.legacy_aliases }),
      render_authority: "semantic",
      body_md: rawSection.source_block_md,
      source_replacements: rawSection.span_ids.map(replacement),
    }),
    fragments: rawFragment === undefined ? base.fragments : base.fragments.map((value) => value !== rawFragment ? value : {
      fragment_id: rawFragment.fragment_id,
      projection_group: rawFragment.projection_group,
      ...(rawFragment.title === undefined ? {} : { title: rawFragment.title }),
      ...(rawFragment.legacy_aliases === undefined ? {} : { legacy_aliases: rawFragment.legacy_aliases }),
      render_authority: "semantic",
      lifecycle_disposition: "document_prose",
      body_md: rawFragment.source_block_md,
      source_replacements: rawFragment.span_ids.map(replacement),
    }),
    parts: rawPart === undefined ? base.parts : base.parts.map((value) => value !== rawPart ? value : {
      part_id: rawPart.part_id,
      parent_record_id: rawPart.parent_record_id,
      ...(rawPart.title === undefined ? {} : { title: rawPart.title }),
      render_authority: "semantic",
      lifecycle_disposition: "parent_supporting_prose",
      body_md: rawPart.source_block_md,
      source_replacements: rawPart.span_ids.map(replacement),
    }),
    records,
    manifest,
    spans: base.spans.map((span) => convertedSpanIds.has(span.id) ? {
      ...span,
      owner_field: structuralSpanIds.has(span.id) ? "body_md" : "payload.summary_md",
      migration_status: "replaced" as const,
    } : span),
  };
  return {
    base,
    candidate,
    projection: roadmap === "matrix" ? liveMatrixProjection() : liveTestingProjection(),
  };
}

function promotionPorts(
  context: SelfTestContext,
  selection: "matrix" | "testing" | "all",
  baseReads: RepoPath[] = [],
  options: Parameters<typeof promotionDocuments>[2] = {},
): RoadmapCliPorts {
  const matrix = promotionDocuments(context, "matrix", options);
  const testing = promotionDocuments(context, "testing", options);
  const campaign = UTF8.encode(`[campaign]\nschema_version = 1\nmatrix_authority = "authoritative"\ntesting_authority = "authoritative"\n`);
  const retired = UTF8.encode(`[retired_ids]\nschema_version = 1\n`);
  const candidate = new Map<RepoPath, Uint8Array>([
    ["cddl-matrix/roadmap.toml" as RepoPath, selection === "testing" ? composeRoadmapDocument(matrix.base) : composeRoadmapDocument(matrix.candidate)],
    ["tests/testing-roadmap.toml" as RepoPath, selection === "matrix" ? composeRoadmapDocument(testing.base) : composeRoadmapDocument(testing.candidate)],
    ["cddl-matrix/ROADMAP.md" as RepoPath, matrix.projection],
    ["tests/TESTING_ROADMAP.md" as RepoPath, testing.projection],
    ["roadmap-campaign.toml" as RepoPath, campaign],
    ["roadmap-retired-ids.toml" as RepoPath, retired],
  ]);
  const historical = (document: RoadmapDocumentV1): RoadmapDocumentV1 => ({
    ...document,
    document: { ...document.document, semantic_conversion: undefined },
  });
  const base = new Map<RepoPath, Uint8Array>([
    ["cddl-matrix/roadmap.toml" as RepoPath, composeRoadmapDocument(historical(matrix.base))],
    ["tests/testing-roadmap.toml" as RepoPath, composeRoadmapDocument(historical(testing.base))],
    ["cddl-matrix/ROADMAP.md" as RepoPath, matrix.projection],
    ["tests/TESTING_ROADMAP.md" as RepoPath, testing.projection],
    ["roadmap-campaign.toml" as RepoPath, campaign],
    ["roadmap-retired-ids.toml" as RepoPath, retired],
  ]);
  const readMap = (values: ReadonlyMap<RepoPath, Uint8Array>, path: RepoPath): Uint8Array => {
    const value = values.get(path);
    if (value === undefined) throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    return new Uint8Array(value);
  };
  return fakePorts({
    read: (path) => readMap(candidate, path),
    readAtCommit: (path) => {
      baseReads.push(path);
      return readMap(base, path);
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

const SCOPED_CAMPAIGN_ROOT = `[campaign]\nschema_version = 1\nmatrix_authority = "authoritative"\ntesting_authority = "authoritative"\n`;

function scopedSelection(id: string, extra = ""): string {
  return `\n[[selection]]\nitem_id = "${id}"\ntarget_kind = "active_id"\nselected_state = "selected"\npriority_class = "fixture"\nselection_reason_md = """Reason."""\ncycle = "fixture"\nremaining_scope_md = """Scope."""${extra}\n`;
}

function scopedLegacySelection(id: string): string {
  return `\n[[selection]]\nitem_id = "${id}"\ntarget_kind = "legacy_markdown_reservation"\nselected_state = "selected"\npriority_class = "fixture"\nselection_reason_md = """Reason."""\ncycle = "fixture"\nremaining_scope_md = """Scope."""\n`;
}

function scopedReservation(id: string, roadmapPath: "cddl-matrix/ROADMAP.md" | "tests/TESTING_ROADMAP.md"): string {
  return `\n[[legacy_markdown_reservation]]\nid = "${id}"\nwork_kind = "feature"\nroadmap_path = "${roadmapPath}"\nsource_title = "Fixture"\nsource_start_byte = 0\nsource_end_byte = 1\nsource_sha256 = "${"0".repeat(64)}"\nwhole_source_sha256 = "${"0".repeat(64)}"\n`;
}

function selectedShadowCampaignFixture(
  mutatedTitle?: string,
): { readonly campaign: string; readonly source: Uint8Array } {
  const originalSource = liveMatrixShadowV0Source();
  const document = decodeRoadmapSource(
    originalSource,
    "cddl-matrix/roadmap.toml" as RepoPath,
    "matrix",
    true,
  );
  assert(document.document.schema_version === 0, "selected shadow fixture did not decode as v0");
  const shadow = document as RoadmapDocumentV0;
  const record = shadow.records[0]!;
  const spans = record.span_ids.map((id) => shadow.spans.find((span) => span.id === id)!);
  const start = Math.min(...spans.map((span) => span.start_byte));
  const end = Math.max(...spans.map((span) => span.end_byte));
  const sourceSha = new Bun.CryptoHasher("sha256").update(record.source_block_md).digest("hex");
  const title = mutatedTitle ?? record.title;
  return {
    source: originalSource,
    campaign: `[campaign]\nschema_version = 1\nmatrix_authority = "shadow"\ntesting_authority = "shadow"\n\n[[legacy_markdown_reservation]]\nid = "${record.id}"\nwork_kind = "feature"\nroadmap_path = "cddl-matrix/ROADMAP.md"\nsource_title = ${JSON.stringify(title)}\nsource_start_byte = ${start}\nsource_end_byte = ${end}\nsource_sha256 = "${sourceSha}"\nwhole_source_sha256 = "${shadow.document.frozen_source_sha256}"\n`,
  };
}

interface ScopedDebtMutation {
  readonly campaign?: string | null;
  readonly retired?: string | null;
  readonly source?: Uint8Array;
  readonly guards?: RegistryView["current_guards"];
  readonly gates?: RegistryView["gates"];
  readonly stage?: RegistryView["production_output_stage"];
}

function scopedDebtMutationPorts(mutation: ScopedDebtMutation): RoadmapCliPorts {
  const campaign = mutation.campaign === undefined ? SCOPED_CAMPAIGN_ROOT : mutation.campaign;
  const retired = mutation.retired === undefined ? `[retired_ids]\nschema_version = 1\n` : mutation.retired;
  return fakePorts({
    read(path) {
      if (path === "cddl-matrix/roadmap.toml") return mutation.source ?? liveMatrixAuthoritativeSource();
      if (path === "roadmap-campaign.toml" && campaign !== null) return UTF8.encode(campaign);
      if (path === "roadmap-retired-ids.toml" && retired !== null) return UTF8.encode(retired);
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    registry(revision) {
      return {
        ...emptyRegistry(revision),
        production_output_stage: mutation.stage ?? "both_authoritative",
        output_claims: productionOutputInventory(mutation.stage ?? "both_authoritative").claims,
        matrix_status_inputs: liveMatrixStatusInputs(),
        current_guards: mutation.guards ?? [],
        gates: mutation.gates ?? [],
      };
    },
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
    case "query_debt_selected_shadow_reservation_coalesces": {
      const fixture = selectedShadowCampaignFixture();
      const result = run(
        ["--roadmap", "matrix", "--query", "debt", "--json"],
        scopedDebtMutationPorts({
          campaign: fixture.campaign,
          source: fixture.source,
          stage: "pre_cutover",
        }),
      );
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 &&
          JSON.parse(text(result.stdout)).roadmaps[0].roadmap === "matrix",
        `selected shadow reservation did not coalesce with its same-ID shadow: ${text(result.stderr)}`,
      );
      return pass("positive");
    }
    case "query_debt_selected_shadow_title_binding_rejected": {
      const fixture = selectedShadowCampaignFixture("Mutated fixture title");
      const recordId = "matrix.additional-tool-annotations";
      const decoded = decodeRoadmapSource(
        fixture.source,
        "cddl-matrix/roadmap.toml" as RepoPath,
        "matrix",
        true,
      );
      assert(decoded.document.schema_version === 0, "selected shadow title repro did not decode as v0");
      const document = {
        ...decoded,
        records: decoded.records.map((record) => record.id === recordId
          ? { ...record, title: "Mutated fixture title" }
          : record),
      } as RoadmapDocumentV0;
      const issues = validateSelectedLifecycleContext({
        selection: "matrix",
        campaign: decodeCampaignSource(UTF8.encode(fixture.campaign), "roadmap-campaign.toml" as RepoPath, true),
        retired: { retired_ids: { schema_version: 1 }, entries: [] },
        document,
        registry: scopedDebtMutationPorts({ stage: "pre_cutover" }).read.registryView({ kind: "worktree" }),
      });
      assert(issues.some((issue) =>
        issue.code === "E-CAMPAIGN-TARGET" && issue.logical_path === `record[${JSON.stringify(recordId)}]`
      ), `${id} accepted mutated record title plus matching reservation source_title`);
      observeSelfTestIssue({ code: "E-CAMPAIGN-TARGET", logical_path: `record[${JSON.stringify(recordId)}]` });
      return pass("negative");
    }
    case "query_debt_duplicate_campaign_selection_rejected":
    case "query_debt_unselected_duplicate_selection_rejected":
    case "query_debt_unselected_duplicate_reservation_rejected":
    case "query_debt_unselected_legacy_selection_missing_reservation_rejected":
    case "query_debt_unselected_active_reservation_rejected":
    case "query_debt_unselected_reservation_guard_tombstone_rejected":
    case "query_debt_selected_alias_reservation_rejected":
    case "query_debt_invalid_campaign_target_rejected":
    case "query_debt_invalid_campaign_state_rejected":
    case "query_debt_authoritative_reservation_rejected":
    case "query_debt_active_tombstone_collision_rejected":
    case "query_debt_active_guard_collision_rejected":
    case "query_debt_active_reservation_collision_rejected":
    case "query_debt_invalid_guard_pin_rejected":
    case "query_debt_authority_mismatch_rejected":
    case "query_debt_missing_campaign_root_rejected":
    case "query_debt_missing_retired_root_rejected":
    case "query_debt_invalid_retired_pin_rejected":
    case "query_debt_stage_mismatch_rejected": {
      const activeId = "matrix.fixed-array.static-representation";
      const validGate = { id: "fixture-durable", kind: "cmd", stub: false } as const;
      const retired = (recordId: string, gateId: string): string => `[retired_ids]\nschema_version = 1\n\n[[retired_ids.entry]]\nid = "${recordId}"\nlast_active_at = "${"a".repeat(40)}"\n\n[retired_ids.entry.replacement]\nkind = "gate"\ngate_id = "${gateId}"\nclaim_md = """Replacement."""\n`;
      let mutation: ScopedDebtMutation = {};
      let expected: { code: IssueCode; source: string; path: string };
      switch (id) {
        case "query_debt_duplicate_campaign_selection_rejected":
          mutation = { campaign: SCOPED_CAMPAIGN_ROOT + scopedSelection(activeId) + scopedSelection(activeId) };
          expected = { code: "E-CAMPAIGN-DUPLICATE", source: "roadmap-campaign.toml", path: "selection[1]" };
          break;
        case "query_debt_unselected_duplicate_selection_rejected":
          mutation = { campaign: SCOPED_CAMPAIGN_ROOT + scopedSelection("testing.fixture-unselected") + scopedSelection("testing.fixture-unselected") };
          expected = { code: "E-CAMPAIGN-DUPLICATE", source: "roadmap-campaign.toml", path: "selection[1]" };
          break;
        case "query_debt_unselected_duplicate_reservation_rejected": {
          const reservation = `\n[[legacy_markdown_reservation]]\nid = "testing.fixture-reserved"\nwork_kind = "feature"\nroadmap_path = "tests/TESTING_ROADMAP.md"\nsource_title = "Fixture"\nsource_start_byte = 0\nsource_end_byte = 1\nsource_sha256 = "${"0".repeat(64)}"\nwhole_source_sha256 = "${"0".repeat(64)}"\n`;
          mutation = { campaign: `[campaign]\nschema_version = 1\nmatrix_authority = "authoritative"\ntesting_authority = "shadow"\n${reservation}${reservation}`, stage: "matrix_authoritative" };
          expected = { code: "E-CAMPAIGN-DUPLICATE", source: "roadmap-campaign.toml", path: "legacy_markdown_reservation[1]" };
          break;
        }
        case "query_debt_unselected_legacy_selection_missing_reservation_rejected":
          mutation = {
            campaign: `[campaign]\nschema_version = 1\nmatrix_authority = "authoritative"\ntesting_authority = "shadow"\n` +
              scopedLegacySelection("testing.fixture-unselected"),
            stage: "matrix_authoritative",
          };
          expected = { code: "E-CAMPAIGN-TARGET", source: "roadmap-campaign.toml", path: "selection[0]" };
          break;
        case "query_debt_unselected_active_reservation_rejected": {
          const reservedId = "testing.fixture-reserved";
          mutation = {
            campaign: `[campaign]\nschema_version = 1\nmatrix_authority = "authoritative"\ntesting_authority = "shadow"\n` +
              scopedReservation(reservedId, "tests/TESTING_ROADMAP.md") + scopedSelection(reservedId),
            stage: "matrix_authoritative",
          };
          expected = { code: "E-CAMPAIGN-TARGET", source: "roadmap-campaign.toml", path: "selection[0]" };
          break;
        }
        case "query_debt_unselected_reservation_guard_tombstone_rejected": {
          const reservedId = "testing.fixture-reserved";
          const campaign = `[campaign]\nschema_version = 1\nmatrix_authority = "authoritative"\ntesting_authority = "shadow"\n` +
            scopedReservation(reservedId, "tests/TESTING_ROADMAP.md");
          const guard = {
            id: reservedId as import("../model/core.ts").RoadmapId,
            owner_registry: "fixture",
            replacement_pin: { kind: "gate" as const, gate_id: validGate.id, claim_md: UTF8.encode("Pin.") },
          };
          for (const [subcase, collision] of [
            ["guard", { guards: [guard], gates: [validGate] }],
            ["tombstone", { retired: retired(reservedId, validGate.id), gates: [validGate] }],
          ] as const) {
            const result = run(
              ["--roadmap", "matrix", "--query", "debt", "--json"],
              scopedDebtMutationPorts({ campaign, stage: "matrix_authoritative", ...collision }),
            );
            const prefix = `FAIL [E-OWNER-DUPLICATE] <identity>#owner[${JSON.stringify(reservedId)}]:`;
            assert(
              result.exit_code === 1 && result.stdout.byteLength === 0 && text(result.stderr).includes(prefix),
              `${id}/${subcase} did not reject at ${prefix}: ${text(result.stderr)}`,
            );
          }
          observeSelfTestIssue({ code: "E-OWNER-DUPLICATE", logical_path: `owner[${JSON.stringify(reservedId)}]` });
          return pass("negative", ["guard", "tombstone"]);
        }
        case "query_debt_selected_alias_reservation_rejected": {
          const reservedId = "testing.fixture-reserved";
          const source = UTF8.encode(text(liveMatrixAuthoritativeSource()).replace(
            `projection_group = "expansion"\nrender_authority = "raw"`,
            `projection_group = "expansion"\nlegacy_aliases = ["${reservedId}"]\nrender_authority = "raw"`,
          ));
          assert(text(source).includes(`legacy_aliases = ["${reservedId}"]`), "selected alias fixture mutation missed its target");
          mutation = {
            campaign: `[campaign]\nschema_version = 1\nmatrix_authority = "authoritative"\ntesting_authority = "shadow"\n` +
              scopedReservation(reservedId, "tests/TESTING_ROADMAP.md"),
            source,
            stage: "matrix_authoritative",
          };
          expected = { code: "E-ALIAS-COLLISION", source: "<identity>", path: `alias[${JSON.stringify(reservedId)}]` };
          break;
        }
        case "query_debt_invalid_campaign_target_rejected":
          mutation = { campaign: SCOPED_CAMPAIGN_ROOT + scopedSelection("matrix.fixture-missing") };
          expected = { code: "E-CAMPAIGN-TARGET", source: "roadmap-campaign.toml", path: "selection[0]" };
          break;
        case "query_debt_invalid_campaign_state_rejected":
        {
          const validCampaign = decodeCampaignSource(
            UTF8.encode(SCOPED_CAMPAIGN_ROOT + scopedSelection(activeId)),
            "roadmap-campaign.toml" as RepoPath,
            true,
          );
          const directIssues = validateSelectedLifecycleContext({
            selection: "matrix",
            campaign: {
              ...validCampaign,
              selections: validCampaign.selections.map((selection) => ({
                ...selection,
                pickup_commit: "a".repeat(40) as import("../model/core.ts").FullCommitId,
              })),
            },
            retired: { retired_ids: { schema_version: 1 }, entries: [] },
            document: decodeRoadmapSource(liveMatrixAuthoritativeSource(), "cddl-matrix/roadmap.toml" as RepoPath, "matrix", true),
            registry: scopedDebtMutationPorts({}).read.registryView({ kind: "worktree" }),
          });
          assert(directIssues.some((issue) => issue.code === "E-CAMPAIGN-STATE" && issue.logical_path === "selection[0]"), "selected-scope validator accepted a programmatic invalid campaign state");
          mutation = { campaign: SCOPED_CAMPAIGN_ROOT + scopedSelection(activeId, `\npickup_commit = "${"a".repeat(40)}"`) };
          expected = { code: "E-SCHEMA-FORBIDDEN-KEY", source: "roadmap-campaign.toml", path: "selection[0].pickup_commit" };
          break;
        }
        case "query_debt_authoritative_reservation_rejected":
          mutation = { campaign: SCOPED_CAMPAIGN_ROOT + `\n[[legacy_markdown_reservation]]\nid = "testing.fixture-reserved"\nwork_kind = "feature"\nroadmap_path = "tests/TESTING_ROADMAP.md"\nsource_title = "Fixture"\nsource_start_byte = 0\nsource_end_byte = 1\nsource_sha256 = "${"0".repeat(64)}"\nwhole_source_sha256 = "${"0".repeat(64)}"\n` };
          expected = { code: "E-CAMPAIGN-TARGET-EXPIRED", source: "roadmap-campaign.toml", path: "legacy_markdown_reservation[0]" };
          break;
        case "query_debt_active_tombstone_collision_rejected":
          mutation = { retired: retired(activeId, validGate.id), gates: [validGate] };
          expected = { code: "E-OWNER-DUPLICATE", source: "<identity>", path: `owner[${JSON.stringify(activeId)}]` };
          break;
        case "query_debt_active_guard_collision_rejected":
          mutation = { gates: [validGate], guards: [{ id: activeId as import("../model/core.ts").RoadmapId, owner_registry: "fixture", replacement_pin: { kind: "gate", gate_id: validGate.id, claim_md: UTF8.encode("Pin.") } }] };
          expected = { code: "E-OWNER-DUPLICATE", source: "<identity>", path: `owner[${JSON.stringify(activeId)}]` };
          break;
        case "query_debt_active_reservation_collision_rejected":
          mutation = { campaign: SCOPED_CAMPAIGN_ROOT + `\n[[legacy_markdown_reservation]]\nid = "${activeId}"\nwork_kind = "optimization"\nroadmap_path = "cddl-matrix/ROADMAP.md"\nsource_title = "Fixture"\nsource_start_byte = 0\nsource_end_byte = 1\nsource_sha256 = "${"0".repeat(64)}"\nwhole_source_sha256 = "${"0".repeat(64)}"\n` };
          expected = { code: "E-OWNER-DUPLICATE", source: "<identity>", path: `owner[${JSON.stringify(activeId)}]` };
          break;
        case "query_debt_invalid_guard_pin_rejected":
          mutation = { guards: [{ id: "matrix.fixture-guard" as import("../model/core.ts").RoadmapId, owner_registry: "fixture", replacement_pin: { kind: "gate", gate_id: "missing", claim_md: UTF8.encode("Pin.") } }] };
          expected = { code: "E-TRANSACTION-GUARD", source: "<transaction>", path: `guard["matrix.fixture-guard"]` };
          break;
        case "query_debt_authority_mismatch_rejected":
          mutation = { campaign: `[campaign]\nschema_version = 1\nmatrix_authority = "shadow"\ntesting_authority = "shadow"\n` };
          expected = { code: "E-SCHEMA-STATE", source: "roadmap-campaign.toml", path: "campaign.matrix_authority" };
          break;
        case "query_debt_missing_campaign_root_rejected":
          mutation = { campaign: null };
          expected = { code: "E-SOURCE-MISSING", source: "roadmap-campaign.toml", path: "$" };
          break;
        case "query_debt_missing_retired_root_rejected":
          mutation = { retired: null };
          expected = { code: "E-SOURCE-MISSING", source: "roadmap-retired-ids.toml", path: "$" };
          break;
        case "query_debt_invalid_retired_pin_rejected":
          mutation = { retired: retired("matrix.fixture-retired", "missing") };
          expected = { code: "E-RETIRED-REPLACEMENT", source: "roadmap-retired-ids.toml", path: "retired_ids.entry[0].replacement" };
          break;
        case "query_debt_stage_mismatch_rejected":
          mutation = { stage: "matrix_authoritative" };
          expected = { code: "E-OUTPUT-CLAIM", source: "<output-registry>", path: "production_output_stage" };
          break;
      }
      const result = run(["--roadmap", "matrix", "--query", "debt", "--json"], scopedDebtMutationPorts(mutation));
      const prefix = `FAIL [${expected.code}] ${expected.source}#${expected.path}:`;
      assert(result.exit_code === 1 && result.stdout.byteLength === 0 && text(result.stderr).includes(prefix), `${id} did not reject at ${prefix}: ${text(result.stderr)}`);
      observeSelfTestIssue({ code: expected.code, logical_path: expected.path });
      return pass("negative");
    }
    case "cli_against_semantic_promotion_scoped_matrix":
    case "cli_against_semantic_promotion_scoped_testing":
    case "cli_against_semantic_promotion_all_simultaneous":
    case "cli_against_semantic_promotion_other_base_not_loaded": {
      const selection = id.includes("all_simultaneous") ? "all" : id.includes("testing") ? "testing" : "matrix";
      const baseReads: RepoPath[] = [];
      const result = run(["--check", "--roadmap", selection, "--against", HASH], promotionPorts(context, selection, baseReads));
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 && text(result.stdout).includes("CHECK OK\n"),
        `${id} failed through public --against: ${text(result.stderr)}`,
      );
      if (id === "cli_against_semantic_promotion_other_base_not_loaded") {
        assert(
          JSON.stringify(baseReads) === JSON.stringify(["cddl-matrix/roadmap.toml"]),
          `scoped promotion loaded an unselected base roadmap: ${JSON.stringify(baseReads)}`,
        );
      }
      return pass("positive");
    }
    case "cli_against_structural_promotion_scoped_matrix":
    case "cli_against_structural_promotion_scoped_testing":
    case "cli_against_structural_promotion_all_simultaneous":
    case "cli_against_structural_promotion_other_base_not_loaded":
    case "cli_against_structural_record_composition":
    case "cli_against_structural_semantic_only_composition": {
      const selection = id.includes("all_simultaneous") ? "all" : id.includes("testing") ? "testing" : "matrix";
      const baseReads: RepoPath[] = [];
      const options = id === "cli_against_structural_record_composition"
        ? { record: true, structural: true }
        : id === "cli_against_structural_semantic_only_composition"
          ? { record: false, structural: true, semantic_only: true }
          : { record: false, structural: true };
      const result = run(
        ["--check", "--roadmap", selection, "--against", HASH],
        promotionPorts(context, selection, baseReads, options),
      );
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 && text(result.stdout).includes("CHECK OK\n"),
        `${id} failed through public structural --against: ${text(result.stderr)}`,
      );
      if (id === "cli_against_structural_promotion_other_base_not_loaded") {
        assert(
          JSON.stringify(baseReads) === JSON.stringify(["cddl-matrix/roadmap.toml"]),
          `scoped structural promotion loaded an unselected base roadmap: ${JSON.stringify(baseReads)}`,
        );
      }
      return pass("positive");
    }
    case "cli_semantic_conversion_current_omission_rejected": {
      const sources = bothAuthoritativePorts();
      const omitted = (bytes: Uint8Array): Uint8Array => UTF8.encode(
        text(bytes).replace('semantic_conversion = "converting"\n', ""),
      );
      const matrix = omitted(liveMatrixAuthoritativeSource());
      const testing = omitted(liveTestingAuthoritativeSource());
      const ports = fakePorts({
        read(path) {
          if (path === "cddl-matrix/roadmap.toml") return matrix;
          if (path === "tests/testing-roadmap.toml") return testing;
          return sources.read.readDeclared(path);
        },
        registry: sources.read.registryView,
      });
      const commands = [
        ["--check", "--roadmap", "matrix"],
        ["--check", "--roadmap", "testing"],
        ["--check", "--roadmap", "all"],
        ["--roadmap", "matrix", "--query", "debt", "--json"],
        ["--roadmap", "testing", "--write"],
        ["--format-source", "cddl-matrix/roadmap.toml"],
      ] as const;
      for (const argv of commands) {
        const result = run(argv, ports);
        assert(
          result.exit_code === 1 && result.stdout.byteLength === 0 &&
            text(result.stderr).includes("current roadmap schema v1 requires semantic_conversion"),
          `current command accepted omitted semantic conversion declaration: ${JSON.stringify(argv)} ${text(result.stderr)}`,
        );
      }
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
      const matrix = liveMatrixAuthoritativeDocument();
      const testing = liveTestingAuthoritativeDocument();
      const matrixIndependent = [
        "atomic-bounds-handover",
        "fixed-byte-representation",
        "multifile-extern-exclusion",
        "open-table-min-one-hardening",
      ];
      const testingIndependent = [
        "part-all-hit-cost",
        "part-another-profile-flip-abort-fix-widening-reachable-set",
        "part-arbitrary-derived-supported-cddl-ast-generation",
        "part-batch-masking-detector-layer-sweeps",
        "part-closure-audit-traced-set-extension",
        "part-collision-loser-row-schema-id-s-match",
        "part-collision-two-types-both-lack-rows",
        "part-comment-ast-grammar-change-any-form-just-new",
        "part-coverage-extensions",
        "part-cross-crate-collision-schema-id-s-match",
        "part-dedicated-collision-message-generic-instantiation-naming",
        "part-embed-site-leg-alias-classifying-roots-two-proven",
        "part-inline-anonymous-two-arm-choices-recognized",
        "part-mangling-still-general-fix",
        "part-member-position-duplicates-extension",
        "part-nested-cargo-test",
        "part-non-idiom-choice-bodied-generic-defs-refused-supported",
        "part-occurrence-aware-generic-instance-identity",
        "part-own-reviewed-change-never-drive",
        "part-read-caught-instance-class",
        "part-real-world-corpus-differential",
        "part-reopening-signal",
        "part-reopening-signal-nested",
        "part-reopening-signal-probe",
        "part-reopening-signal-wasm",
        "part-reopening-signal-workspace",
        "part-rustc-after-panic-ok-flip-panic-fix-lands",
        "part-rustfmt-code-never-reaches-rustc-all",
        "part-same-shape-set-instantiations",
        "part-schema-name-schemars-percent-encodes-ref",
        "part-scope-wide-probe",
        "part-spend-measurements",
      ];
      for (const [name, document, independent] of [
        ["matrix", matrix, matrixIndependent],
        ["testing", testing, testingIndependent],
      ] as const) {
        assert(
          document.fragments.every((fragment) =>
            fragment.render_authority === "raw" && fragment.lifecycle_disposition === "document_prose"
          ),
          `${name} live fragments are not all explicitly reviewed document prose`,
        );
        const actualIndependent = document.parts.filter((part) =>
          part.render_authority === "raw" && part.lifecycle_disposition === "independent_record"
        ).map((part) => part.part_id).sort();
        assert(JSON.stringify(actualIndependent) === JSON.stringify([...independent].sort()), `${name} independent part classification drifted`);
        assert(
          document.parts.every((part) =>
            part.render_authority === "raw" &&
            (part.lifecycle_disposition === "independent_record" || part.lifecycle_disposition === "parent_supporting_prose")
          ),
          `${name} live parts do not all carry an explicit reviewed lifecycle disposition`,
        );
      }
      assert(matrix.fragments.length === 5 && matrix.parts.length === 13, "matrix subordinate live denominator drifted");
      assert(testing.fragments.length === 2 && testing.parts.length === 60, "testing subordinate live denominator drifted");
      return pass("positive");
    }
    case "query_debt_live_migration_floors": {
      const result = run(["--roadmap", "all", "--query", "debt", "--json"], bothAuthoritativePorts());
      assert(result.exit_code === 0 && result.stderr.byteLength === 0, `live debt query failed: ${text(result.stderr)}`);
      const rows = JSON.parse(text(result.stdout)).roadmaps as readonly Record<string, any>[];
      const matrix = rows.find((row) => row.roadmap === "matrix");
      const testing = rows.find((row) => row.roadmap === "testing");
      assert(matrix !== undefined && testing !== undefined, "live debt query omitted a roadmap");
      const independentTotal = (row: Record<string, any>): number =>
        Object.values(row.independent_counts as Record<string, number>).reduce((sum, value) => sum + value, 0);
      const sorted = (values: readonly unknown[], key: (value: any) => string): boolean => {
        const keys = values.map(key);
        return JSON.stringify(keys) === JSON.stringify([...keys].sort((left, right) => left < right ? -1 : left > right ? 1 : 0));
      };
      assert(independentTotal(matrix) === 18, `matrix independent migration floor drifted: ${independentTotal(matrix)}`);
      assert(independentTotal(testing) === 34, `testing independent migration floor drifted: ${independentTotal(testing)}`);
      assert(JSON.stringify(matrix.independent_counts) === JSON.stringify({
        inferred_transitions: 7, pending_family_classifications: 7, raw_subordinate_lifecycles: 4,
        unmodelled_coordinates: 0, unrendered_fields: 0, unresolved_references: 0,
      }), "matrix independent category vector drifted");
      assert(JSON.stringify(testing.independent_counts) === JSON.stringify({
        inferred_transitions: 1, pending_family_classifications: 1, raw_subordinate_lifecycles: 32,
        unmodelled_coordinates: 0, unrendered_fields: 0, unresolved_references: 0,
      }), "testing independent category vector drifted");
      assert(
        matrix.migration_progress.raw_content_owners.count === 86 &&
          matrix.migration_progress.raw_spans.count === 86 &&
          matrix.migration_progress.frozen_spans.count === 86 &&
          matrix.migration_progress.semantic_shadows.count === 7 &&
          matrix.migration_progress.boundary_debt.count === 4 &&
          matrix.migration_progress.replacement_coverage.denominator === 0 &&
          matrix.migration_progress.replacement_coverage.numerator === 0 &&
          matrix.migration_progress.completion_audit.lane_blockers.length === 283 &&
          matrix.migration_progress.completion_audit.wp5c_join_blockers.length === 0,
        "matrix exact live migration facts drifted",
      );
      assert(
        testing.migration_progress.raw_content_owners.count === 208 &&
          testing.migration_progress.raw_spans.count === 208 &&
          testing.migration_progress.frozen_spans.count === 208 &&
          testing.migration_progress.semantic_shadows.count === 1 &&
          testing.migration_progress.boundary_debt.count === 32 &&
          testing.migration_progress.replacement_coverage.denominator === 0 &&
          testing.migration_progress.replacement_coverage.numerator === 0 &&
          testing.migration_progress.completion_audit.lane_blockers.length === 659 &&
          testing.migration_progress.completion_audit.wp5c_join_blockers.length === 0,
        "testing exact live migration facts drifted",
      );
      for (const row of [matrix, testing]) {
        const progress = row.migration_progress;
        assert(progress.independent_debt.items.filter((item: any) => item.category === "unmodelled_coordinates").length === 0, `${row.roadmap} visible unmodelled coordinate floor drifted`);
        assert(sorted(progress.raw_content_owners.owners, (owner) => JSON.stringify([owner.roadmap, owner.owner_kind, owner.owner_id, owner.owner_field])), `${row.roadmap} raw owner list is not canonical`);
        assert(sorted(progress.raw_spans.span_ids, String) && sorted(progress.frozen_spans.span_ids, String) &&
          sorted(progress.semantic_shadows.record_ids, String) &&
          sorted(progress.completion_audit.lane_blockers, (blocker) => JSON.stringify([blocker.category, blocker.subject])),
        `${row.roadmap} migration IDs/blockers are not canonical`);
      }
      return pass("positive");
    }
    case "query_debt_scoped_does_not_load_other_roadmap": {
      const source = bothAuthoritativePorts();
      const reads: RepoPath[] = [];
      const ports = fakePorts({
        read(path) {
          reads.push(path);
          return source.read.readDeclared(path);
        },
        registry: source.read.registryView,
      });
      const result = run(["--roadmap", "matrix", "--query", "debt", "--json"], ports);
      assert(result.exit_code === 0 && result.stderr.byteLength === 0, `scoped debt query failed: ${text(result.stderr)}`);
      const rows = JSON.parse(text(result.stdout)).roadmaps;
      assert(rows.length === 1 && rows[0].roadmap === "matrix", "scoped debt query returned another roadmap");
      assert(
        JSON.stringify(reads) === JSON.stringify([
          "cddl-matrix/roadmap.toml", "roadmap-campaign.toml", "roadmap-retired-ids.toml",
        ]),
        `scoped debt query read set drifted or loaded unselected TOML/Markdown: ${JSON.stringify(reads)}`,
      );
      return pass("positive");
    }
    case "query_debt_all_reports_both": {
      const result = run(["--roadmap", "all", "--query", "debt", "--json"], bothAuthoritativePorts());
      assert(result.exit_code === 0 && result.stderr.byteLength === 0, `all debt query failed: ${text(result.stderr)}`);
      const rows = JSON.parse(text(result.stdout)).roadmaps;
      assert(
        JSON.stringify(rows.map((row: { roadmap: string }) => row.roadmap)) === JSON.stringify(["matrix", "testing"]) &&
          rows.every((row: Record<string, unknown>) => "migration_progress" in row),
        "all debt query did not report both roadmap progress views in canonical order",
      );
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
