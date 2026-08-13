import type { RegistryView } from "../adapters/types.ts";
import { MATRIX_ADAPTER } from "../adapters/matrix.ts";
import { TESTING_ADAPTER } from "../adapters/testing.ts";
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
import type { FixtureRelativePath, RepoPath, RepositoryRevision, RoadmapId } from "../model/core.ts";
import type { RoadmapDocumentV1, RoadmapDocumentV2, SemanticPayload } from "../model/documents.ts";
import type { CurrentFamilyGuard } from "../model/documents.ts";
import { FIXED_VALUE_FAMILY_ROOT } from "../fixed_value_guards.ts";
import type { MatrixStatusInputs } from "../model/matrix.ts";
import { resolveManifest } from "../manifest.ts";
import {
  LEGACY_STATUS_OUTPUT_CLAIMS,
  productionOutputInventory,
} from "../output_registry.ts";
import type { SelfTestCandidateCase as SelfTestCase, SelfTestContext, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import { buildProjectionViews } from "../projection_views.ts";
import {
  projectionLayoutRank,
  validateProjectionLayoutTransition,
  type ProjectionLayout,
} from "../projection_layout.ts";
import { buildExpectedChunks, validateCompletedChunks } from "../render_ir.ts";
import { observeSelfTestIssue } from "./observations.ts";
import {
  liveMatrixAuthoritativeDocument,
  liveMatrixAuthoritativeSource,
  liveMatrixCurrentLegacyProjection,
  liveMatrixLegacyProjection,
  liveMatrixLegacyV2Document,
  liveMatrixProjection,
  liveMatrixShadowV0Document,
  liveMatrixShadowV0Source,
  liveMatrixV2Document,
} from "./live_matrix.ts";
import {
  liveTestingAuthoritativeDocument,
  liveTestingAuthoritativeSource,
  liveTestingLegacyProjection,
  liveTestingLegacyV2Document,
  liveTestingProjection,
  liveTestingShadowV0Document,
  liveTestingShadowV0Source,
  liveTestingV2Document,
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
  "failure_stdout_empty",
  "success_receipt_nonvacuous",
  "cli_against_matrix_check_allowed",
  "cli_against_testing_check_allowed",
  "cli_against_all_check_allowed",
  "cli_authoritative_fresh_projection_reference_provenance",
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
  "cli_against_part_to_record_scoped_matrix",
  "cli_against_part_to_record_scoped_testing",
  "cli_against_part_to_record_all_simultaneous",
  "cli_against_part_to_record_other_base_not_loaded",
  "cli_semantic_conversion_current_omission_rejected",
  "cli_against_projection_layout_promotion",
  "cli_against_v2_scoped_promotion_rejected",
  "cli_summary_open_family_lower_bound_only",
  "live_projection_pre_anchor_baseline",
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

function registryForDocuments(
  revision: RepositoryRevision,
  documents: readonly RoadmapDocumentV1[],
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
      liveMatrixAuthoritativeDocument(),
      liveTestingAuthoritativeDocument(),
    ]);
  return {
    ...cachedLiveRegistry,
    revision,
    current_guards: fixedValueClosureGuards(),
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

function replacementBytes(
  record: Extract<RoadmapDocumentV1["records"][number], { readonly render_authority: "semantic" }>,
): Uint8Array {
  assert(record.source_replacements.length === 1, `fixture record ${record.id} lacks one replacement`);
  const field = record.source_replacements[0]!.replacement_field;
  assert(field === "payload.summary_md" || field === "payload.detail_md", `fixture record ${record.id} uses unsupported replacement ${field}`);
  const value = field === "payload.summary_md" ? record.payload.summary_md : record.payload.detail_md;
  assert(value !== undefined, `fixture record ${record.id} lacks replacement bytes at ${field}`);
  return value;
}

/** Reintroduce only the raw owners needed by transaction tests into the completed live document. */
function rawPromotionBase(roadmap: "matrix" | "testing"): RoadmapDocumentV1 {
  const live = roadmap === "matrix" ? liveMatrixAuthoritativeDocument() : liveTestingAuthoritativeDocument();
  if (live.document.semantic_conversion === "converting") return live;
  const recordPrefix = roadmap === "matrix" ? "record-" : "span-record-";
  const partPrefix = roadmap === "matrix" ? "part-" : "span-part-";
  const record = live.records.find((value) =>
    value.render_authority === "semantic" && value.projection_visibility === "document" &&
    value.source_replacements.length === 1 && value.source_replacements[0]!.span_id.startsWith(recordPrefix)
  );
  assert(record?.render_authority === "semantic", `${roadmap} fixture lacks a reversible document record`);
  const recordBytes = replacementBytes(record);
  const shadow: SemanticPayload = {
    kind: "work",
    summary_md: recordBytes,
    work_state: "ready",
    work_intent: roadmap === "matrix" ? "build_capability" : "build_system",
    work_kind: roadmap === "matrix" ? "feature" : "infrastructure",
    risk: roadmap === "matrix" ? "cosmetic" : "false_pass_or_red",
    family_classification: "none_reviewed",
    acceptance_md: UTF8.encode("Reviewed promotion preserves the exact legacy bytes."),
    priority_rationale_md: UTF8.encode("Fixture promotion is transaction-scoped."),
  };
  const rawRecord: RoadmapDocumentV1["records"][number] = {
    id: record.id,
    title: record.title,
    projection_group: record.projection_group,
    ...(record.legacy_aliases === undefined ? {} : { legacy_aliases: record.legacy_aliases }),
    ...(record.tags === undefined ? {} : { tags: record.tags }),
    render_authority: "raw",
    source_block_md: recordBytes,
    span_ids: record.source_replacements.map((replacement) => replacement.span_id),
    semantic_shadow: shadow,
  };

  const section = live.sections.find((value) => value.render_authority === "semantic" && value.source_replacements.length === 1);
  const fragment = live.fragments.find((value) => value.render_authority === "semantic" && value.source_replacements.length === 1);
  const supportingPart = live.parts.find((value) =>
    value.render_authority === "semantic" && value.lifecycle_disposition === "parent_supporting_prose" &&
    value.source_replacements.length === 1
  );
  assert(section?.render_authority === "semantic", `${roadmap} fixture lacks a reversible section`);
  assert(fragment?.render_authority === "semantic", `${roadmap} fixture lacks a reversible fragment`);
  assert(supportingPart?.render_authority === "semantic", `${roadmap} fixture lacks a reversible supporting part`);

  const promotedPart = live.records.find((value) => {
    if (value.render_authority !== "semantic" || value.projection_visibility !== "document" ||
      value.source_replacements.length !== 1 || !value.source_replacements[0]!.span_id.startsWith(partPrefix)) return false;
    const parents = live.relations.filter((relation) => relation.kind === "parent_of" && relation.target === value.id);
    return parents.length === 1 && live.relations.every((relation) => relation.source !== value.id) &&
      (roadmap === "matrix" || live.references.every((reference) => reference.source !== value.id));
  });
  assert(promotedPart?.render_authority === "semantic", `${roadmap} fixture lacks a reversible promoted part`);
  const promotedSpanId = promotedPart.source_replacements[0]!.span_id;
  const promotedPartId = promotedSpanId.slice(partPrefix.length);
  const parent = live.relations.find((relation) => relation.kind === "parent_of" && relation.target === promotedPart.id)!;
  const rawPart: RoadmapDocumentV1["parts"][number] = {
    part_id: promotedPartId as RoadmapDocumentV1["parts"][number]["part_id"],
    parent_record_id: parent.source,
    title: promotedPart.title,
    render_authority: "raw",
    lifecycle_disposition: "independent_record",
    source_block_md: replacementBytes(promotedPart),
    span_ids: [promotedSpanId],
  };

  const rawSpanIds = new Set([
    ...rawRecord.span_ids,
    ...section.source_replacements.map((value) => value.span_id),
    ...fragment.source_replacements.map((value) => value.span_id),
    ...supportingPart.source_replacements.map((value) => value.span_id),
    promotedSpanId,
  ]);
  return {
    ...live,
    document: {
      ...live.document,
      semantic_conversion: "converting",
      frozen_legacy_span_ids: [...live.document.frozen_legacy_span_ids, ...rawSpanIds].sort(),
    },
    sections: live.sections.map((value) => value !== section ? value : {
      section_id: section.section_id,
      title: section.title,
      ...(section.legacy_aliases === undefined ? {} : { legacy_aliases: section.legacy_aliases }),
      render_authority: "raw",
      source_block_md: section.body_md,
      span_ids: section.source_replacements.map((replacement) => replacement.span_id),
    }),
    fragments: live.fragments.map((value) => value !== fragment ? value : {
      fragment_id: fragment.fragment_id,
      projection_group: fragment.projection_group,
      ...(fragment.title === undefined ? {} : { title: fragment.title }),
      ...(fragment.legacy_aliases === undefined ? {} : { legacy_aliases: fragment.legacy_aliases }),
      render_authority: "raw",
      lifecycle_disposition: fragment.lifecycle_disposition,
      source_block_md: fragment.body_md,
      span_ids: fragment.source_replacements.map((replacement) => replacement.span_id),
    }),
    parts: [...live.parts.map((value) => value !== supportingPart ? value : {
      part_id: supportingPart.part_id,
      parent_record_id: supportingPart.parent_record_id,
      ...(supportingPart.title === undefined ? {} : { title: supportingPart.title }),
      render_authority: "raw" as const,
      lifecycle_disposition: supportingPart.lifecycle_disposition,
      source_block_md: supportingPart.body_md,
      span_ids: supportingPart.source_replacements.map((replacement) => replacement.span_id),
    }), rawPart],
    records: live.records.filter((value) => value !== promotedPart).map((value) => value === record ? rawRecord : value),
    manifest: live.manifest.map((entry) => entry.kind === "record" && entry.record_id === promotedPart.id
      ? { kind: "part" as const, part_id: rawPart.part_id }
      : entry),
    spans: live.spans.map((span) => {
      if (span.id === promotedSpanId) return {
        ...span,
        source_kind: "part" as const,
        owner_id: rawPart.part_id,
        owner_field: "source_block_md" as const,
        migration_status: "raw" as const,
      };
      return rawSpanIds.has(span.id)
        ? { ...span, owner_field: "source_block_md" as const, migration_status: "raw" as const }
        : span;
    }),
    references: live.references.filter((reference) => reference.source !== promotedPart.id),
    relations: live.relations.filter((relation) => relation.source !== promotedPart.id && relation.target !== promotedPart.id),
  };
}

function promotionDocuments(
  _context: SelfTestContext,
  roadmap: "matrix" | "testing",
  options: {
    readonly record?: boolean;
    readonly structural?: boolean;
    readonly semantic_only?: boolean;
    readonly part_to_record?: boolean;
  } = {},
): { readonly base: RoadmapDocumentV1; readonly candidate: RoadmapDocumentV1; readonly projection: Uint8Array } {
  const sourcePath = (roadmap === "matrix" ? "cddl-matrix/roadmap.toml" : "tests/testing-roadmap.toml") as RepoPath;
  const v1 = rawPromotionBase(roadmap);
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
  const includePartToRecord = options.part_to_record ?? false;
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
  let parts = base.parts;
  let relations = base.relations;
  let partToRecordSpanId: RoadmapDocumentV1["spans"][number]["id"] | undefined;
  if (includePartToRecord) {
    const independentPart = base.parts.find((value) =>
      "source_block_md" in value && value.span_ids.length === 1 && value.title !== undefined &&
      value.lifecycle_disposition === "independent_record"
    );
    if (independentPart === undefined || !("source_block_md" in independentPart)) {
      throw new Error(`${roadmap} part-to-record fixture lacks a reviewed independent singleton owner`);
    }
    const parent = base.records.find((record) => record.id === independentPart.parent_record_id);
    if (parent === undefined) throw new Error(`${roadmap} part-to-record fixture lacks its parent`);
    const id = `${roadmap}.${independentPart.part_id}` as (typeof rawRecord)["id"];
    partToRecordSpanId = independentPart.span_ids[0]!;
    records = [...records, {
      id,
      title: independentPart.title!,
      projection_group: parent.projection_group,
      render_authority: "semantic",
      projection_visibility: "document",
      payload: {
        ...payload,
        summary_md: UTF8.encode("Typed part metadata."),
        detail_md: independentPart.source_block_md,
      },
      source_replacements: [{
        span_id: partToRecordSpanId,
        replacement_field: "payload.detail_md",
        review_note_md: UTF8.encode("Reviewed exact independent part conversion."),
      }],
    }];
    parts = base.parts.filter((value) => value !== independentPart);
    manifest = manifest.map((entry) => entry.kind === "part" && entry.part_id === independentPart.part_id
      ? { kind: "record" as const, record_id: id }
      : entry);
    relations = [...relations, { source: independentPart.parent_record_id, kind: "parent_of", target: id }];
  }
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
    parts: rawPart === undefined ? parts : parts.map((value) => value !== rawPart ? value : {
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
    relations,
    spans: base.spans.map((span) => span.id === partToRecordSpanId ? {
      ...span,
      source_kind: "record" as const,
      owner_id: `${roadmap}.${base.parts.find((value) => "span_ids" in value && value.span_ids.includes(span.id))!.part_id}`,
      owner_field: "payload.detail_md",
      migration_status: "replaced" as const,
    } : convertedSpanIds.has(span.id) ? {
      ...span,
      owner_field: structuralSpanIds.has(span.id) ? "body_md" : "payload.summary_md",
      migration_status: "replaced" as const,
    } : span),
  };
  if (partToRecordSpanId !== undefined) {
    candidate.document.frozen_legacy_span_ids = candidate.document.frozen_legacy_span_ids.filter((spanId) => spanId !== partToRecordSpanId);
  }
  return {
    base,
    candidate,
    projection: roadmap === "matrix" ? liveMatrixCurrentLegacyProjection() : liveTestingLegacyProjection(),
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
  const candidate = new Map<RepoPath, Uint8Array>([
    ["cddl-matrix/roadmap.toml" as RepoPath, selection === "testing" ? composeRoadmapDocument(matrix.base) : composeRoadmapDocument(matrix.candidate)],
    ["tests/testing-roadmap.toml" as RepoPath, selection === "matrix" ? composeRoadmapDocument(testing.base) : composeRoadmapDocument(testing.candidate)],
    ["cddl-matrix/ROADMAP.md" as RepoPath, matrix.projection],
    ["tests/TESTING_ROADMAP.md" as RepoPath, testing.projection],
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
        ...liveRegistry(revision),
        production_output_stage: "both_authoritative",
        output_claims: productionOutputInventory("both_authoritative").claims,
        matrix_status_inputs: liveMatrixStatusInputs(),
      };
    },
  });
}

type V2PromotionMutation =
  | "exact"
  | "mixed"
  | "semantic_drift"
  | "incomplete_base"
  | "downgrade"
  | "preexisting_mixed";

function v2PromotionPorts(mutation: V2PromotionMutation): RoadmapCliPorts {
  let baseMatrix: RoadmapDocumentV1 | RoadmapDocumentV2 = liveMatrixAuthoritativeDocument();
  let baseTesting: RoadmapDocumentV1 | RoadmapDocumentV2 = liveTestingAuthoritativeDocument();
  let candidateMatrix: RoadmapDocumentV1 | RoadmapDocumentV2 = liveMatrixLegacyV2Document();
  let candidateTesting: RoadmapDocumentV1 | RoadmapDocumentV2 = liveTestingLegacyV2Document();
  if (mutation === "mixed") candidateTesting = liveTestingAuthoritativeDocument();
  if (mutation === "semantic_drift") {
    candidateMatrix = {
      ...candidateMatrix,
      records: candidateMatrix.records.map((record, index) =>
        index === 0 ? { ...record, title: `${record.title} drift` } : record
      ),
    };
  }
  if (mutation === "incomplete_base") {
    baseMatrix = {
      ...baseMatrix,
      document: { ...baseMatrix.document, semantic_conversion: "converting" },
    };
    baseTesting = {
      ...baseTesting,
      document: { ...baseTesting.document, semantic_conversion: "converting" },
    };
  }
  if (mutation === "downgrade") {
    baseMatrix = liveMatrixLegacyV2Document();
    baseTesting = liveTestingLegacyV2Document();
    candidateMatrix = liveMatrixAuthoritativeDocument();
    candidateTesting = liveTestingAuthoritativeDocument();
  }
  if (mutation === "preexisting_mixed") {
    baseMatrix = liveMatrixLegacyV2Document();
    candidateMatrix = liveMatrixLegacyV2Document();
    baseTesting = liveTestingAuthoritativeDocument();
    candidateTesting = liveTestingAuthoritativeDocument();
  }
  const candidate = new Map<RepoPath, Uint8Array>([
    ["cddl-matrix/roadmap.toml" as RepoPath, composeRoadmapDocument(candidateMatrix)],
    ["tests/testing-roadmap.toml" as RepoPath, composeRoadmapDocument(candidateTesting)],
    ["cddl-matrix/ROADMAP.md" as RepoPath, liveMatrixCurrentLegacyProjection()],
    ["tests/TESTING_ROADMAP.md" as RepoPath, liveTestingLegacyProjection()],
  ]);
  const base = new Map<RepoPath, Uint8Array>([
    ["cddl-matrix/roadmap.toml" as RepoPath, composeRoadmapDocument(baseMatrix)],
    ["tests/testing-roadmap.toml" as RepoPath, composeRoadmapDocument(baseTesting)],
    ["cddl-matrix/ROADMAP.md" as RepoPath, liveMatrixCurrentLegacyProjection()],
    ["tests/TESTING_ROADMAP.md" as RepoPath, liveTestingLegacyProjection()],
  ]);
  const readMap = (values: ReadonlyMap<RepoPath, Uint8Array>, path: RepoPath): Uint8Array => {
    const value = values.get(path);
    if (value === undefined) throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    return new Uint8Array(value);
  };
  return fakePorts({
    read: (path) => readMap(candidate, path),
    readAtCommit: (path) => readMap(base, path),
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

type LayoutMutation = "exact" | "content_drift" | "reference_drift";

function withLayout(document: RoadmapDocumentV2, layout: ProjectionLayout): RoadmapDocumentV2 {
  const nextHeading = projectionLayoutRank(layout) >= projectionLayoutRank("unnumbered_v1")
    ? "Next work"
    : "Next work items, in priority order";
  return {
    ...document,
    document: { ...document.document, projection_layout: layout },
    references: document.references.map((reference) => reference.kind === "file_heading" &&
        reference.path === "tests/TESTING_ROADMAP.md" &&
        (reference.heading === "Next work" || reference.heading === "Next work items, in priority order")
      ? { ...reference, heading: nextHeading }
      : reference),
  };
}

function projectionForLayout(document: RoadmapDocumentV2, registry: RegistryView): Uint8Array {
  const adapter = document.document.roadmap === "matrix" ? MATRIX_ADAPTER : TESTING_ADAPTER;
  const manifest = resolveManifest(document);
  const resolvers = adapter.slotResolvers(registry, document);
  const completed = buildExpectedChunks(document, manifest.ops, {
    renderSemanticRecord: (record, fields) => adapter.renderSemantic(record, fields),
    resolveGeneratedSlot: (slot) => resolvers.get(slot.slot_id)?.resolve(slot, registry),
  });
  const renderIssues = [...manifest.issues, ...validateCompletedChunks(document, manifest.ops, completed)];
  assert(renderIssues.length === 0, `layout stage projection failed render validation: ${JSON.stringify(renderIssues)}`);
  const legacy = document.document.roadmap === "matrix" ? liveMatrixCurrentLegacyProjection() : liveTestingLegacyProjection();
  const views = buildProjectionViews(document, completed, legacy);
  assert(views.issues.length === 0, `layout stage projection failed view validation: ${JSON.stringify(views.issues)}`);
  return views.full;
}

const layoutStageProjectionCache = new Map<string, Uint8Array>();

function liveProjectionForLayout(
  roadmap: "matrix" | "testing",
  layout: ProjectionLayout,
  registry: RegistryView,
): Uint8Array {
  const key = `${roadmap}:${layout}`;
  const cached = layoutStageProjectionCache.get(key);
  if (cached !== undefined) return new Uint8Array(cached);
  const document = withLayout(roadmap === "matrix" ? liveMatrixV2Document() : liveTestingV2Document(), layout);
  const projection = projectionForLayout(document, registry);
  layoutStageProjectionCache.set(key, projection);
  return new Uint8Array(projection);
}

function layoutPorts(
  baseLayout: ProjectionLayout,
  candidateLayout: ProjectionLayout,
  mutation: LayoutMutation = "exact",
  v1Base = false,
): RoadmapCliPorts {
  let baseMatrix: RoadmapDocumentV1 | RoadmapDocumentV2 = v1Base
    ? liveMatrixAuthoritativeDocument()
    : withLayout(liveMatrixV2Document(), baseLayout);
  let baseTesting: RoadmapDocumentV1 | RoadmapDocumentV2 = v1Base
    ? liveTestingAuthoritativeDocument()
    : withLayout(liveTestingV2Document(), baseLayout);
  let candidateMatrix = withLayout(liveMatrixV2Document(), candidateLayout);
  let candidateTesting = withLayout(liveTestingV2Document(), candidateLayout);
  if (mutation === "content_drift") candidateMatrix = {
    ...candidateMatrix,
    records: candidateMatrix.records.map((record, index) => index === 0
      ? { ...record, title: `${record.title} illicit layout-rider` }
      : record),
  };
  if (mutation === "reference_drift") candidateTesting = {
    ...candidateTesting,
    references: candidateTesting.references.map((reference, index) => index === 0
      ? { ...reference, source: candidateTesting.records[1]!.id }
      : reference),
  };
  const registry = {
    ...liveRegistry({ kind: "worktree" }),
    production_output_stage: "both_authoritative" as const,
    output_claims: productionOutputInventory("both_authoritative").claims,
    matrix_status_inputs: liveMatrixStatusInputs(),
  };
  const candidateMatrixProjection = liveProjectionForLayout("matrix", candidateLayout, registry);
  const candidateTestingProjection = liveProjectionForLayout("testing", candidateLayout, registry);
  const baseMatrixProjection = baseMatrix.document.schema_version === 1
    ? liveMatrixCurrentLegacyProjection()
    : liveProjectionForLayout("matrix", baseLayout, registry);
  const baseTestingProjection = baseTesting.document.schema_version === 1
    ? liveTestingLegacyProjection()
    : liveProjectionForLayout("testing", baseLayout, registry);
  const candidate = new Map<RepoPath, Uint8Array>([
    ["cddl-matrix/roadmap.toml" as RepoPath, composeRoadmapDocument(candidateMatrix)],
    ["tests/testing-roadmap.toml" as RepoPath, composeRoadmapDocument(candidateTesting)],
    ["cddl-matrix/ROADMAP.md" as RepoPath, candidateMatrixProjection],
    ["tests/TESTING_ROADMAP.md" as RepoPath, candidateTestingProjection],
  ]);
  const base = new Map<RepoPath, Uint8Array>([
    ["cddl-matrix/roadmap.toml" as RepoPath, composeRoadmapDocument(baseMatrix)],
    ["tests/testing-roadmap.toml" as RepoPath, composeRoadmapDocument(baseTesting)],
    ["cddl-matrix/ROADMAP.md" as RepoPath, baseMatrixProjection],
    ["tests/TESTING_ROADMAP.md" as RepoPath, baseTestingProjection],
  ]);
  const readMap = (values: ReadonlyMap<RepoPath, Uint8Array>, path: RepoPath): Uint8Array => {
    const value = values.get(path);
    if (value === undefined) throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    return new Uint8Array(value);
  };
  return fakePorts({
    read: (path) => readMap(candidate, path),
    readAtCommit: (path) => readMap(base, path),
    registry(revision) {
      return { ...registry, revision };
    },
  });
}

function assertV2TransactionRejection(
  result: RoadmapCliResult,
  logicalPath: string,
  label: string,
): SelfTestResult {
  const prefix = `FAIL [E-TRANSACTION-BASE] <transaction>#${logicalPath}:`;
  assert(
    result.exit_code === 1 && result.stdout.byteLength === 0 && text(result.stderr).includes(prefix),
    `${label} did not reject at ${prefix}: ${text(result.stderr)}`,
  );
  observeSelfTestIssue({ code: "E-TRANSACTION-BASE", logical_path: logicalPath });
  return pass("negative");
}

function validTestingPorts(context: SelfTestContext, atomic?: FakeOptions["atomic"]): RoadmapCliPorts {
  const generic = validGenericAllPorts(context);
  return fakePorts({
    read: generic.read.readDeclared,
    registry: generic.read.registryView,
    atomic,
  });
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
  assert(document.document.schema_version === 1, "cross-roadmap testing target fixture must be v1");
  const v1 = document as RoadmapDocumentV1;
  if (v1.records.some((record) => record.id === CROSS_ROADMAP_TESTING_EXTERN_TARGET)) return source;
  const projectionGroup = v1.sections[0]?.section_id;
  assert(projectionGroup !== undefined, "cross-roadmap testing target fixture lacks a projection group");
  return composeRoadmapDocument({
    ...v1,
    records: [...v1.records, {
      id: CROSS_ROADMAP_TESTING_EXTERN_TARGET,
      title: "Cross-roadmap extern execution owner",
      projection_group: projectionGroup,
      render_authority: "semantic",
      projection_visibility: "semantic_only",
      payload: {
        kind: "work",
        summary_md: UTF8.encode("Cross-roadmap extern execution owner."),
        work_state: "ready",
        work_intent: "build_system",
        work_kind: "infrastructure",
        risk: "compile_failure",
        family_classification: "none_reviewed",
        acceptance_md: UTF8.encode("The combined cross-roadmap universe resolves this exact target."),
        priority_rationale_md: UTF8.encode("Fixture-only counterpart for the live matrix delegation."),
      },
      source_replacements: [],
    }],
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
  const source = withCrossRoadmapTestingExternTarget(UTF8.encode(base.replace(
    /\[record\.payload\]\n[\s\S]*?(?=\n\[\[record\.source_replacement\]\])/,
    payload,
  ) + `
[[reference]]
id = "temporal-owner"
source = "testing.fixture-mixed-semantic"
kind = "external_commit"
repository = "fixture/repository"
commit = "1111111111111111111111111111111111111111"
`));
  const projection = fixture(context, "positive/mixed-testing-v1.expected.md");
  return fakePorts({
    read(path) {
      if (path === sourcePath) return new Uint8Array(source);
      if (path === projectionPath) return new Uint8Array(projection);
      if (path === "cddl-matrix/roadmap.toml") return liveMatrixAuthoritativeSource();
      if (path === "cddl-matrix/ROADMAP.md") return liveMatrixProjection();
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

function legacyMatrixStatusInputs(): MatrixStatusInputs {
  const current = liveMatrixStatusInputs();
  return {
    ...current,
    matrix: {
      ...current.matrix,
      annotations: current.matrix.annotations.slice(0, 293),
      containment_ids: current.matrix.containment_ids.slice(0, 136),
    },
  };
}

function validGenericAllPorts(_context: SelfTestContext): RoadmapCliPorts {
  const matrixSourcePath = "cddl-matrix/roadmap.toml" as RepoPath;
  const matrixProjectionPath = "cddl-matrix/ROADMAP.md" as RepoPath;
  const testingSourcePath = "tests/testing-roadmap.toml" as RepoPath;
  const testingProjectionPath = "tests/TESTING_ROADMAP.md" as RepoPath;
  const matrixSource = liveMatrixShadowV0Source();
  const matrixProjection = liveMatrixLegacyProjection();
  const testingSource = liveTestingShadowV0Source();
  const testingProjection = liveTestingLegacyProjection();
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
        matrix_status_inputs: legacyMatrixStatusInputs(),
      };
    },
  });
}

function shadowBootstrapSource(roadmap: "matrix" | "testing"): Uint8Array {
  const shadow = roadmap === "matrix" ? liveMatrixShadowV0Document() : liveTestingShadowV0Document();
  const completed = roadmap === "matrix" ? liveMatrixAuthoritativeDocument() : liveTestingAuthoritativeDocument();
  const partPrefix = roadmap === "matrix" ? "part-" : "span-part-";
  const independentSpanIds = new Set(completed.records.flatMap((record) =>
    record.render_authority === "semantic" && record.projection_visibility === "document"
      ? record.source_replacements.filter((replacement) => replacement.span_id.startsWith(partPrefix)).map((replacement) => replacement.span_id)
      : []
  ));
  const document: RoadmapDocumentV1 = {
    ...shadow,
    document: {
      ...shadow.document,
      schema_version: 1,
      authority: "authoritative",
      semantic_conversion: "converting",
      frozen_legacy_span_ids: shadow.spans.filter((span) => span.migration_status === "raw").map((span) => span.id).sort(),
    },
    sections: shadow.sections.map((section) => ({ ...section, render_authority: "raw" })),
    fragments: shadow.fragments.map((fragment) => ({
      ...fragment,
      render_authority: "raw",
      lifecycle_disposition: "document_prose",
    })),
    legacy_markers: shadow.legacy_markers.map((marker) => ({ ...marker, render_authority: "raw" })),
    records: shadow.records.map((record) => ({ ...record, render_authority: "raw" })),
    parts: shadow.parts.map((part) => ({
      ...part,
      render_authority: "raw",
      lifecycle_disposition: part.span_ids.some((spanId) => independentSpanIds.has(spanId))
        ? "independent_record"
        : "parent_supporting_prose",
    })),
    references: [],
    relations: [],
  };
  return composeRoadmapDocument(document);
}

function bothAuthoritativePorts(atomic?: FakeOptions["atomic"]): RoadmapCliPorts {
  return fakePorts({
    read(path) {
      if (path === "cddl-matrix/roadmap.toml") return liveMatrixAuthoritativeSource();
      if (path === "cddl-matrix/ROADMAP.md") return liveMatrixProjection();
      if (path === "tests/testing-roadmap.toml") return liveTestingAuthoritativeSource();
      if (path === "tests/TESTING_ROADMAP.md") return liveTestingProjection();
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

const FIXED_VALUE_CLOSURE_GUARD_ROLES = Object.freeze([
  [FIXED_VALUE_FAMILY_ROOT, "closed_family_root"],
  ["matrix.fixed-value-representative-kind", "family_axis"],
  ...["bool", "bytes", "float", "nint", "null", "text", "uint", "undefined"].map((value) =>
    [`matrix.fixed-value-kind.${value}`, "family_axis_value"] as const
  ),
  ["matrix.requirement.fixed-value-choice-member-generation", "family_evidence_requirement"],
  ["matrix.requirement.fixed-value-choice-member-runtime", "family_evidence_requirement"],
  ...["bool", "bytes", "float", "nint", "null", "text", "uint", "undefined"].map((value) =>
    [`matrix.fixed-value-choice-member.coordinate-${value}`, "family_cell"] as const
  ),
] as const);

function fixedValueClosureGuards(): readonly CurrentFamilyGuard[] {
  return FIXED_VALUE_CLOSURE_GUARD_ROLES.map(([id, guard_role]) => ({
    id: id as RoadmapId,
    guard_role,
    family_root_id: FIXED_VALUE_FAMILY_ROOT,
    owner_registry: "fixed-value-choice-member-closure",
    replacement_pin: {
      kind: "gate", gate_id: "roadmap_projection_check", claim_md: UTF8.encode("Delivered closure."),
    },
  }));
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
  const projectionPath = "tests/TESTING_ROADMAP.md" as RepoPath;
  const sourceFixture = text(fixture(context, "positive/mixed-testing-v1.toml"))
    .replace("cddl-matrix/roadmap/fixtures/positive/mixed-testing-v1.toml", sourcePath)
    .replace("cddl-matrix/roadmap/fixtures/positive/mixed-testing-v1.expected.md", projectionPath);
  const sourceBytes = UTF8.encode(
    `${sourceFixture}\n[[reference]]
id = "selftest-fresh-projection-heading"
source = "testing.fixture-mixed-semantic"
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

interface ScopedDebtMutation {
  readonly stage?: RegistryView["production_output_stage"];
}

function scopedDebtMutationPorts(mutation: ScopedDebtMutation): RoadmapCliPorts {
  return fakePorts({
    read(path) {
      if (path === "cddl-matrix/roadmap.toml") return liveMatrixAuthoritativeSource();
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    registry(revision) {
      const registry = liveRegistry(revision);
      return {
        ...registry,
        production_output_stage: mutation.stage ?? "both_authoritative",
        output_claims: productionOutputInventory(mutation.stage ?? "both_authoritative").claims,
        matrix_status_inputs: mutation.stage === "pre_cutover"
          ? legacyMatrixStatusInputs()
          : liveMatrixStatusInputs(),
      };
    },
  });
}

function matrixMixedAuthorityPorts(
  context: SelfTestContext,
  candidateReads: RepoPath[],
  baseReads: RepoPath[],
): RoadmapCliPorts {
  const matrixSourcePath = "cddl-matrix/roadmap.toml" as RepoPath;
  const matrixProjectionPath = "cddl-matrix/ROADMAP.md" as RepoPath;
  const testingSourcePath = "tests/testing-roadmap.toml" as RepoPath;
  const testingProjectionPath = "tests/TESTING_ROADMAP.md" as RepoPath;
  const baseMatrixSource = liveMatrixShadowV0Source();
  const candidateMatrixSource = shadowBootstrapSource("matrix");
  const candidateProjection = liveMatrixLegacyProjection();
  const testingMarkdown = UTF8.encode("# Testing legacy authority\n");
  return fakePorts({
    read(path) {
      candidateReads.push(path);
      if (path === matrixSourcePath) return new Uint8Array(candidateMatrixSource);
      if (path === matrixProjectionPath) return new Uint8Array(candidateProjection);
      if (path === testingProjectionPath) return new Uint8Array(testingMarkdown);
      throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
    },
    readAtCommit(path) {
      baseReads.push(path);
      if (path === matrixSourcePath) return new Uint8Array(baseMatrixSource);
      if (path === matrixProjectionPath) return new Uint8Array(candidateProjection);
      if (path === testingProjectionPath) return new Uint8Array(testingMarkdown);
      if (path === testingSourcePath) {
        throw roadmapFailure("E-SOURCE-MISSING", path, "$", "declared source is missing", 1);
      }
      throw new Error(`unexpected matrix_authoritative base read ${path}`);
    },
    registry(revision) {
      const stage = revision.kind === "worktree" ? "matrix_authoritative" : "pre_cutover";
      return {
        ...liveRegistry(revision),
        production_output_stage: stage,
        output_claims: productionOutputInventory(stage).claims,
        matrix_status_inputs: legacyMatrixStatusInputs(),
        current_guards: [],
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
    case "cli_write_authoritative_matrix": {
      const candidateReads: RepoPath[] = [];
      const baseReads: RepoPath[] = [];
      const replacements: { path: RepoPath; bytes: Uint8Array }[] = [];
      const fixturePorts = matrixMixedAuthorityPorts(context, candidateReads, baseReads);
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
      for (const view of ["summary", "debt", "references", "signals", "actionables", "decisions",
        "families", "watches", "content", "output-owners"]) {
        expectFailure(
          ["--roadmap", "testing", "--query", view],
          expectedIssue("E-SOURCE-MISSING", "tests/testing-roadmap.toml", "$", "declared source is missing", 1),
        );
      }
      return pass("positive", ["summary", "debt", "references", "signals",
        "actionables", "decisions", "families", "watches", "content", "output-owners"]);
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
        [
          liveMatrixShadowV0Source(),
          "matrix_authoritative",
          legacyMatrixStatusInputs(),
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
          mismatch(source, stage, matrixStatusInputs),
        );
      }
      return pass("negative", [
        "authoritative_without_claim",
        "shadow_with_claim",
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
    case "cli_against_part_to_record_scoped_matrix":
    case "cli_against_part_to_record_scoped_testing":
    case "cli_against_part_to_record_all_simultaneous":
    case "cli_against_part_to_record_other_base_not_loaded": {
      const selection = id.includes("all_simultaneous") ? "all" : id.includes("testing") ? "testing" : "matrix";
      const baseReads: RepoPath[] = [];
      const result = run(
        ["--check", "--roadmap", selection, "--against", HASH],
        promotionPorts(context, selection, baseReads, { record: false, part_to_record: true }),
      );
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 && text(result.stdout).includes("CHECK OK\n"),
        `${id} failed through public part-to-record --against: ${text(result.stderr)}`,
      );
      if (id === "cli_against_part_to_record_other_base_not_loaded") {
        assert(
          JSON.stringify(baseReads) === JSON.stringify(["cddl-matrix/roadmap.toml"]),
          `scoped part-to-record promotion loaded an unselected base roadmap: ${JSON.stringify(baseReads)}`,
        );
      }
      return pass("positive");
    }
    case "cli_against_projection_layout_promotion": {
      const layouts = ["legacy_v1", "anchors_v1", "standing_v1", "unnumbered_v1", "curated_v1"] as const;
      for (let index = 0; index < layouts.length - 1; index++) {
        const baseLayout = layouts[index]!;
        const candidateLayout = layouts[index + 1]!;
        const layout = run(["--check", "--roadmap", "all", "--against", HASH],
          layoutPorts(baseLayout, candidateLayout));
        assert(layout.exit_code === 0 && layout.stderr.byteLength === 0 && text(layout.stdout).includes("CHECK OK\n"),
          `exact adjacent ${baseLayout} to ${candidateLayout} projection-only promotion failed: ${text(layout.stderr)}`);
      }
      for (const [baseLayout, candidateLayout, mutation, path, label, v1Base] of [
        ["legacy_v1", "anchors_v1", "content_drift", "matrix.document.projection_layout", "content drift", false],
        ["standing_v1", "unnumbered_v1", "reference_drift", "testing.document.projection_layout", "unrelated reference drift", false],
        ["legacy_v1", "standing_v1", "exact", "matrix.document.projection_layout", "skipped layout stage", false],
        ["legacy_v1", "curated_v1", "exact", "matrix.document.projection_layout", "direct legacy-to-curated layout", false],
        ["legacy_v1", "curated_v1", "exact", "matrix", "direct v1-to-curated layout", true],
      ] as const) {
        const rejected = run(["--check", "--roadmap", "all", "--against", HASH],
          layoutPorts(baseLayout, candidateLayout, mutation, v1Base));
        assert(rejected.exit_code === 1 && rejected.stdout.byteLength === 0 &&
          text(rejected.stderr).includes(`#${path}:`),
        `${label} did not reject at its exact projection-layout boundary: ${text(rejected.stderr)}`);
      }
      for (let baseIndex = 1; baseIndex < layouts.length; baseIndex++) {
        for (let candidateIndex = 0; candidateIndex < baseIndex; candidateIndex++) {
          const baseLayout = layouts[baseIndex]!;
          const candidateLayout = layouts[candidateIndex]!;
          const issues = validateProjectionLayoutTransition(
            withLayout(liveMatrixV2Document(), baseLayout),
            withLayout(liveMatrixV2Document(), candidateLayout),
          );
          assert(issues.length === 1 && issues[0]!.code === "E-TRANSACTION-BASE" &&
            issues[0]!.logical_path === "matrix.document.projection_layout",
          `${baseLayout} to ${candidateLayout} reversal did not reject exactly`);
        }
      }
      return pass("positive");
    }
    case "cli_against_v2_scoped_promotion_rejected":
      return assertV2TransactionRejection(
        run(["--check", "--roadmap", "matrix", "--against", HASH], v2PromotionPorts("exact")),
        "matrix",
        "scoped v2 promotion",
      );
    case "cli_summary_open_family_lower_bound_only": {
      const result = run(["--roadmap", "matrix", "--query", "summary", "--json"], bothAuthoritativePorts());
      assert(result.exit_code === 0 && result.stderr.byteLength === 0, `live matrix summary failed: ${text(result.stderr)}`);
      const payload = JSON.parse(text(result.stdout));
      const families = payload.roadmaps?.find((row: { roadmap: string }) => row.roadmap === "matrix")?.families;
      assert(Array.isArray(families) && families.length === 6, `live summary did not expose exactly 6 active matrix families: ${JSON.stringify(payload)}`);
      for (const family of families as readonly Record<string, unknown>[]) {
        assert(family.denominator_maturity === "observed_only", `live family is unexpectedly closable: ${JSON.stringify(family)}`);
        assert(typeof family.observed_lower_bound === "number", `open family omitted observed_lower_bound: ${JSON.stringify(family)}`);
        for (const forbidden of ["legal_total", "percentage", "completion", "completion_percentage", "percent_complete"]) {
          assert(!(forbidden in family), `open family fabricated ${forbidden}: ${JSON.stringify(family)}`);
        }
      }
      const dashboards = new Map<string, Record<string, unknown>>();
      for (const view of ["signals", "actionables", "decisions", "families", "watches", "content"] as const) {
        const first = run(["--roadmap", "all", "--query", view, "--json"], bothAuthoritativePorts());
        const second = run(["--roadmap", "all", "--query", view, "--json"], bothAuthoritativePorts());
        assert(first.exit_code === 0 && second.exit_code === 0 && first.stderr.byteLength === 0 &&
          second.stderr.byteLength === 0 && text(first.stdout) === text(second.stdout),
        `${view} dashboard is not a deterministic successful query`);
        dashboards.set(view, JSON.parse(text(first.stdout)));
      }
      const decisions = dashboards.get("decisions")!.decisions as Record<string, readonly Record<string, unknown>[]>;
      assert(Object.values(decisions).flat().length > 0 &&
        (decisions.pending ?? []).every((row) => "question_md" in row && "transition_ids" in row) &&
        (decisions.held ?? []).every((row) => "rationale_md" in row && "transition_ids" in row) &&
        (decisions.decided ?? []).every((row) => "rationale_md" in row && "authority_reference_id" in row),
      "decisions dashboard dropped its state-specific question/rationale/authority fields");
      const signals = dashboards.get("signals")!.signals as Record<string, readonly Record<string, unknown>[]>;
      assert(JSON.stringify(Object.keys(signals)) === JSON.stringify(["cadence", "evidence_freshness",
        "promotion_trigger", "reopening_signal", "retirement_predicate", "unblock_predicate", "watch_escalation"]),
      "signals dashboard transition grouping changed");
      for (const [kind, required] of [
        ["promotion_trigger", ["predicate", "predicate_kind", "current_evidence_ids", "action_on_fire_md"]],
        ["unblock_predicate", ["event_md", "check_procedure_md", "due_action_md"]],
        ["watch_escalation", ["capture_procedure_md", "response_md", "escalation_action_md", "retirement_semantics_md"]],
        ["retirement_predicate", ["external_predicate_md", "verification_md", "due_action_md"]],
        ["cadence", ["period_or_event_md", "checklist_md", "missed_action_md", "due_on"]],
        ["evidence_freshness", ["claim_md", "reference_ids", "scope", "valid_through", "unprobed_remainder_md"]],
      ] as const) {
        assert(signals[kind]!.length > 0 && required.every((field) => field in signals[kind]![0]!),
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
      const syntheticTesting = liveTestingV2Document();
      const syntheticRecords: RoadmapDocumentV2["records"] = [
        ...syntheticTesting.records,
        {
          id: "testing.fixture-dashboard-delegated" as RoadmapId,
          title: "Delegated dashboard fixture",
          projection_group: syntheticTesting.sections[0]!.section_id,
          render_authority: "semantic",
          projection_visibility: "semantic_only",
          payload: { kind: "work", summary_md: UTF8.encode("Delegated dashboard fixture."),
            work_state: "delegated", work_intent: "build_capability", work_kind: "feature",
            risk: "cosmetic", family_classification: "none_reviewed",
            return_condition_md: UTF8.encode("Return when the delegated owner completes the fixture.") },
          source_replacements: [],
        },
        {
          id: "testing.fixture-dashboard-cost" as RoadmapId,
          title: "Cost dashboard fixture",
          projection_group: syntheticTesting.sections[0]!.section_id,
          render_authority: "semantic",
          projection_visibility: "semantic_only",
          payload: { kind: "testing_cost", summary_md: UTF8.encode("Cost dashboard fixture."),
            cost_posture: "live_registry", unit: "milliseconds",
            scope_md: UTF8.encode("One synthetic dashboard operation."),
            gate_reference_id: syntheticTesting.references.find((reference) => reference.kind === "gate")!.id },
          source_replacements: [],
        },
      ];
      const syntheticDocument: RoadmapDocumentV2 = { ...syntheticTesting, records: syntheticRecords,
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
      const familyDashboard = dashboards.get("families")!.families as readonly Record<string, unknown>[];
      assert(familyDashboard.length === 6 && familyDashboard.every((family) =>
        family.denominator_maturity === "observed_only" && typeof family.observed_lower_bound === "number" &&
        typeof family.explicit_unknown === "number" && family.unmodelled_population === "unknown_open_denominator" &&
        family.denominator_authority === "observed_only" && Array.isArray(family.observation_reference_ids) &&
        Array.isArray(family.exclusions) &&
        !["legal_total", "percentage", "completion", "completion_percentage", "percent_complete",
          "unknown_or_unmodelled"].some((field) => field in family)),
      "open-family dashboard fabricated a denominator/percentage or conflated explicit unknowns with unmodelled space");
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
    case "live_projection_pre_anchor_baseline": {
      for (const [name, projection] of [
        ["matrix", liveMatrixLegacyProjection()],
        ["testing", liveTestingLegacyProjection()],
      ] as const) {
        const source = text(projection);
        assert(!source.includes('id="roadmap-'), `${name} legacy projection already renders stable-ID anchors`);
        assert(
          !/this document is generated|generated from .*roadmap\.toml|edit .*roadmap\.toml/iu.test(source),
          `${name} legacy projection already renders an ownership banner`,
        );
      }
      return pass("positive");
    }
    case "cli_semantic_conversion_current_omission_rejected": {
      const sources = bothAuthoritativePorts();
      const historical = (document: RoadmapDocumentV1): Uint8Array => composeRoadmapDocument({
        ...document,
        document: { ...document.document, semantic_conversion: undefined },
      });
      const matrix = historical(liveMatrixAuthoritativeDocument());
      const testing = historical(liveTestingAuthoritativeDocument());
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
      for (const [name, document, expectedFragments, expectedParts] of [
        ["matrix", matrix, 5, 9],
        ["testing", testing, 2, 27],
      ] as const) {
        const complete = document.document.semantic_conversion === "complete";
        assert(complete
          ? document.fragments.every((fragment) => fragment.render_authority === "semantic" &&
            fragment.lifecycle_disposition === "document_prose" && fragment.source_replacements.length > 0)
          : document.fragments.every((fragment) => fragment.render_authority === "raw" &&
            fragment.lifecycle_disposition === "document_prose" && fragment.span_ids.length > 0),
        `${name} fragment lifecycle does not match its declared conversion stage`);
        const independent = document.parts.filter((part) =>
          part.render_authority === "raw" && part.lifecycle_disposition === "independent_record"
        );
        assert(complete
          ? document.parts.every((part) => part.render_authority === "semantic" &&
            part.lifecycle_disposition === "parent_supporting_prose" && part.source_replacements.length > 0)
          : document.parts.every((part) => part.render_authority === "raw" && part.span_ids.length > 0) &&
            independent.length === (name === "matrix" ? 4 : 31),
        `${name} part lifecycle does not match its declared conversion stage`);
        assert(document.fragments.length === expectedFragments &&
          document.parts.length === (complete ? expectedParts : name === "matrix" ? 13 : 57),
        `${name} subordinate denominator drifted for its declared conversion stage`);
      }
      const testingNested = testing.parts.find((part) => part.part_id === "part-nested-cargo-test");
      assert(testingNested !== undefined && testingNested.lifecycle_disposition === "parent_supporting_prose" &&
        (testing.document.semantic_conversion === "complete"
          ? testingNested.render_authority === "semantic"
          : testingNested.render_authority === "raw"),
        "testing nested-cargo source classification exception drifted");
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
      const complete = matrix.migration_progress.semantic_shadows.count === 0 &&
        testing.migration_progress.semantic_shadows.count === 0;
      assert(independentTotal(matrix) === (complete ? 0 : 4), `matrix migration debt drifted: ${independentTotal(matrix)}`);
      assert(independentTotal(testing) === (complete ? 0 : 31), `testing migration debt drifted: ${independentTotal(testing)}`);
      const zeroCounts = {
        inferred_transitions: 0, pending_family_classifications: 0, raw_subordinate_lifecycles: 0,
        unmodelled_coordinates: 0, unrendered_fields: 0, unresolved_references: 0,
      };
      assert(JSON.stringify(matrix.independent_counts) === JSON.stringify({
        ...zeroCounts, raw_subordinate_lifecycles: complete ? 0 : 4,
      }), "matrix category vector drifted");
      assert(JSON.stringify(testing.independent_counts) === JSON.stringify({
        ...zeroCounts, raw_subordinate_lifecycles: complete ? 0 : 31,
      }), "testing category vector drifted");
      assert(
        matrix.migration_progress.raw_content_owners.count === (complete ? 0 : 86) &&
          matrix.migration_progress.raw_spans.count === (complete ? 0 : 86) &&
          matrix.migration_progress.frozen_spans.count === (complete ? 0 : 86) &&
          matrix.migration_progress.semantic_shadows.count === (complete ? 0 : 59) &&
          matrix.migration_progress.boundary_debt.count === (complete ? 0 : 4) &&
          matrix.migration_progress.replacement_coverage.denominator === (complete ? 85 : 0) &&
          matrix.migration_progress.replacement_coverage.numerator === (complete ? 85 : 0) &&
          matrix.migration_progress.completion_audit.lane_blockers.length === (complete ? 0 : 321) &&
          matrix.migration_progress.completion_audit.join_blockers.length === 0,
        "matrix exact completed migration facts drifted",
      );
      assert(
        testing.migration_progress.raw_content_owners.count === (complete ? 0 : 208) &&
          testing.migration_progress.raw_spans.count === (complete ? 0 : 208) &&
          testing.migration_progress.frozen_spans.count === (complete ? 0 : 208) &&
          testing.migration_progress.semantic_shadows.count === (complete ? 0 : 137) &&
          testing.migration_progress.boundary_debt.count === (complete ? 0 : 31) &&
          testing.migration_progress.replacement_coverage.denominator === (complete ? 198 : 0) &&
          testing.migration_progress.replacement_coverage.numerator === (complete ? 198 : 0) &&
          testing.migration_progress.completion_audit.lane_blockers.length === (complete ? 0 : 792) &&
          testing.migration_progress.completion_audit.join_blockers.length === 0,
        "testing exact completed migration facts drifted",
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
        JSON.stringify(reads) === JSON.stringify(["cddl-matrix/roadmap.toml"]),
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
      const signalGroup = kind === "cadence" ? "cadence" : "evidence_freshness";
      const signals = payload.signals?.[signalGroup];
      const expected = id === "query_as_of_due_date_inclusive"
        ? "due"
        : id === "query_as_of_valid_through_inclusive" ? "as_of" : "stale";
      assert(
        result.exit_code === 0 && result.stderr.byteLength === 0 && payload.evaluation_as_of === date &&
          Array.isArray(signals) && signals.length === 1 && signals[0].id === "testing.fixture-mixed-semantic" &&
          signals[0].evaluation === expected,
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
      const signals = payload.signals?.cadence;
      assert(
        result.exit_code === 0 && payload.evaluation_as_of === null && Array.isArray(signals) &&
          signals.length === 1 && signals[0].evaluation === "unknown_no_as_of",
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
    case "cli_authoritative_fresh_projection_reference_provenance": {
      const subcases: string[] = [];
      const projection = fixture(context, "positive/mixed-testing-v1.expected.md");
      const freshHeading = uniqueProjectionHeading(projection);

      const freshReads = { value: 0 };
      const fresh = run(
        ["--roadmap", "testing", "--query", "debt", "--json"],
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
        ["--roadmap", "testing", "--query", "debt", "--json"],
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
        ["--roadmap", "testing", "--query", "debt", "--json"],
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
    case "cli_against_matrix_check_allowed":
    case "cli_against_testing_check_allowed":
    case "cli_against_all_check_allowed": {
      const roadmap = id.includes("matrix") ? "matrix" : id.includes("testing") ? "testing" : "all";
      // --roadmap all fans the scoped comparison over matrix first, so its first unreadable
      // declared source is the matrix TOML.
      const source = roadmap === "testing" ? "tests/testing-roadmap.toml" : "cddl-matrix/roadmap.toml";
      expectFailure(
        ["--check", "--roadmap", roadmap, "--against", HASH],
        expectedIssue("E-SOURCE-MISSING", source, "$", "declared source is missing", 1),
      );
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
          fixtureCount >= 31 && fixtureBytes > 0 &&
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
