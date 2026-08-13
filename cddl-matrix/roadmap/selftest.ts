import type { IssueCode, RoadmapIssue } from "./errors.ts";
import type { RoadmapSelfTestPorts } from "./io.ts";
import type { FixtureRelativePath } from "./model/core.ts";
import { RoadmapFailure } from "./errors.ts";
import { createSelfTestRegistry, type SelfTestRegistry } from "./selftests/harness.ts";
import { CODEC_SELFTEST_CASES, REQUIRED_CODEC_SELFTEST_CASE_IDS } from "./selftests/codec.ts";
import { SCHEMA_SELFTEST_CASES, REQUIRED_SCHEMA_SELFTEST_CASE_IDS } from "./selftests/schema.ts";
import { PROJECTION_SELFTEST_CASES, REQUIRED_PROJECTION_SELFTEST_CASE_IDS } from "./selftests/projection.ts";
import { IDENTITY_SELFTEST_CASES, REQUIRED_IDENTITY_SELFTEST_CASE_IDS } from "./selftests/identity.ts";
import { ADAPTER_SELFTEST_CASES, REQUIRED_ADAPTER_SELFTEST_CASE_IDS } from "./selftests/adapters.ts";
import { FIXTURE_SELFTEST_CASES, REQUIRED_FIXTURE_SELFTEST_CASE_IDS } from "./selftests/fixtures.ts";
import { CLI_SELFTEST_CASES, REQUIRED_CLI_SELFTEST_CASE_IDS } from "./selftests/cli.ts";
import { PROJECTION_VIEW_SELFTEST_CASES, REQUIRED_PROJECTION_VIEW_SELFTEST_CASE_IDS } from "./selftests/projection_views.ts";

export interface SingleFileFixtureCaseRow {
  kind: "single_file";
  id: string;
  class: "codec" | "positive" | "all_fields";
  input: FixtureRelativePath;
  expected?: FixtureRelativePath;
  adapter: "codec" | "matrix" | "testing";
  schema_version?: 3;
  projection_eof?: "lf" | "none";
}

export interface StatusCompatibilityFixtureCaseRow {
  kind: "status_compat_bundle";
  id: "fixture_status_compat";
  class: "status-compat";
  files: readonly [
    "status-compat/diagnostics.toml",
    "status-compat/inputs.toml",
    "status-compat/matrix-readme.after.md",
    "status-compat/matrix-readme.before.md",
    "status-compat/modes.toml",
    "status-compat/roadmap.after.md",
    "status-compat/roadmap.before.md",
    "status-compat/tests-readme.after.md",
    "status-compat/tests-readme.before.md",
  ];
  inputs: "status-compat/inputs.toml";
  modes: "status-compat/modes.toml";
  diagnostics: "status-compat/diagnostics.toml";
  golden_targets: {
    "status-compat/matrix-readme.before.md": "status-compat/matrix-readme.after.md";
    "status-compat/roadmap.before.md": "status-compat/roadmap.after.md";
    "status-compat/tests-readme.before.md": "status-compat/tests-readme.after.md";
  };
}

export type FixtureCaseRow = SingleFileFixtureCaseRow | StatusCompatibilityFixtureCaseRow;

export interface StatusCompatibilityModeFixture {
  id:
    | "default"
    | "check"
    | "write"
    | "write_and_check_write_wins"
    | "unrelated_arg_default";
  argv: readonly string[];
  target_state: "none" | "before" | "after";
  exit_code: 0 | 1;
  stdout_md: Uint8Array;
  stderr_md: Uint8Array;
  target_reads: number;
  claim_resolutions: number;
  writes: number;
  write_order: readonly string[];
}

export interface StatusCompatibilityDiagnosticFixture {
  id:
    | "write_missing_marker"
    | "check_missing_open"
    | "check_missing_close"
    | "check_stale_inner"
    | "derivation_vacuity";
  argv: readonly string[];
  target: string;
  mutation:
    | "remove_marker_pair"
    | "remove_open_marker"
    | "remove_close_marker"
    | "replace_payload"
    | "remove_all_features";
  marker_id?: string;
  replacement?: string;
  exit_code: 1;
  stdout_md: Uint8Array;
  stderr_md: Uint8Array;
  writes: 0;
}

export interface StatusCompatibilityInputsWire {
  matrix: {
    rfc8610_feature_ids: readonly string[];
    rfc9682_feature_ids: readonly string[];
    cddl_codegen_feature_ids: readonly string[];
    containment_ids: readonly string[];
    control_operator_ids: readonly string[];
    supported_annotation_ids: readonly string[];
    divergent_annotation: readonly {
      id: string;
      status: string;
      profile: string;
      emission_status: string;
    }[];
  };
  catalog: { constraint_row_ids: readonly string[]; constraint_vector_count: number };
  registry: { ignored_gate_ids: readonly string[] };
  timings: { fast_wall_ms: number; local_wall_ms: number; full_wall_ms: number };
}

export type SelfTestCategory =
  | "codec"
  | "schema"
  | "domain-matrix"
  | "domain-testing"
  | "section-render"
  | "identity-retirement"
  | "references-relations"
  | "output-ownership"
  | "determinism-purity"
  | "cli-diagnostics"
  | "fixture-registry"
  | "io-harness"
  | "repository-facts"
  | "status-compat"
  | "adapter-pipeline";

export const SELFTEST_CATEGORIES = Object.freeze([
  "codec",
  "schema",
  "domain-matrix",
  "domain-testing",
  "section-render",
  "identity-retirement",
  "references-relations",
  "output-ownership",
  "determinism-purity",
  "cli-diagnostics",
  "fixture-registry",
  "io-harness",
  "repository-facts",
  "status-compat",
  "adapter-pipeline",
] as const satisfies readonly SelfTestCategory[]);

export interface ExpectedSelfTestIssue {
  code: IssueCode;
  logical_path: string;
}

export type SelfTestResult =
  | { ok: true; polarity: "positive"; subcases?: readonly string[] }
  | {
      ok: false;
      polarity: "positive";
      issues: readonly RoadmapIssue[];
      subcases?: readonly string[];
    }
  | {
      ok: true;
      polarity: "negative";
      expected: ExpectedSelfTestIssue;
      subcases?: readonly string[];
    }
  | {
      ok: false;
      polarity: "negative";
      issues: readonly RoadmapIssue[];
      expected: ExpectedSelfTestIssue;
      subcases?: readonly string[];
    };

/** Internal case-body result, normalized against the frozen independent authority before use. */
export type SelfTestCandidateResult =
  | { ok: true; polarity: "positive"; subcases?: readonly string[] }
  | { ok: true; polarity: "negative"; subcases?: readonly string[] }
  | {
      ok: false;
      polarity: "positive";
      issues: readonly RoadmapIssue[];
      subcases?: readonly string[];
    }
  | {
      ok: false;
      polarity: "negative";
      issues: readonly RoadmapIssue[];
      subcases?: readonly string[];
    };

export interface SelfTestContext {
  readonly ports: RoadmapSelfTestPorts;
  readonly registry: SelfTestRegistryInspection;
}

export interface SelfTestCase {
  id: string;
  category: SelfTestCategory;
  run(context: SelfTestContext): SelfTestResult;
}

export interface SelfTestCandidateCase {
  id: string;
  category: SelfTestCategory;
  run(context: SelfTestContext): SelfTestCandidateResult;
}

export interface SelfTestCategoryReceipt {
  category: SelfTestCategory;
  positive: number;
  negative: number;
}

export interface SelfTestReceipt {
  categories: readonly SelfTestCategoryReceipt[];
  total: number;
}

export interface SelfTestCategoryFloor {
  readonly total: number;
  readonly positive: number;
  readonly negative: number;
}

export interface SelfTestRegistryInspection {
  readonly cases: readonly SelfTestCase[];
  readonly category_floors: ReadonlyMap<SelfTestCategory, SelfTestCategoryFloor>;
  validate(cases?: readonly SelfTestCase[]): readonly RoadmapIssue[];
}

export interface SelfTestRunResult {
  readonly receipt: SelfTestReceipt;
  readonly stdout: Uint8Array;
}

export const SELFTEST_CATEGORY_FLOORS: ReadonlyMap<SelfTestCategory, SelfTestCategoryFloor> =
  new Map(SELFTEST_CATEGORIES.map((category) => [category, Object.freeze({
    total: category === "adapter-pipeline" || category === "determinism-purity" ? 1 : 2,
    positive: 1,
    negative: category === "adapter-pipeline" || category === "determinism-purity" ? 0 : 1,
  })]));

const REFERENCE_KIND_SUBCASES = [
  "roadmap", "matrix_feature", "matrix_role", "matrix_cell", "gate", "test_symbol",
  "file_heading", "spec_passage", "external_issue", "external_commit", "external_release",
  "consumer_report",
] as const;

/** Frozen named reviewer vectors. A case absent from this map must report no subcases. */
export const FROZEN_SELFTEST_SUBCASES: ReadonlyMap<string, readonly string[]> = new Map<string, readonly string[]>([
  ["projection_views_layout_and_provenance", ["banner", "anchor", "status_line", "layout", "full_audit_separation", "fragment_scan", "fragment_duplicate", "fragment_malformed"]],
  ["projection_views_content_exactly_once", ["exact", "missing", "duplicate", "mismatched_bytes"]],
  ["outputs_slot_cardinality", ["status_zero_open", "status_two_open", "status_zero_close", "status_two_close", "reversed", "crossed", "section_slot_zero", "section_slot_two_declarations", "section_slot_two_placements"]],
  ["outputs_interval_overlap", ["same_interval", "partial_left", "partial_right", "contained", "whole_vs_slot", "same_producer_overlap", "adjacent"]],
  ["status_projector_before_after_target_byte_parity", ["roadmap", "matrix_readme", "tests_readme"]],
  ["status_projector_before_after_mode_parity", ["default", "check", "write", "write_and_check_write_wins", "unrelated_arg_default"]],
  ["status_projector_before_after_message_parity", ["write_missing_marker", "check_missing_open", "check_missing_close", "check_stale_inner", "derivation_vacuity"]],
  ["exit_authority_stage_mismatch_one", ["authoritative_without_claim"]],
  ["cli_authoritative_fresh_projection_reference_provenance", [
    "fresh_resolves", "drifted_prior_rejects", "missing_rejects",
  ]],
  ["span_expected_byte_view_cross_chunk", ["zero_length_boundaries", "one_chunk", "adjacent_chunks", "three_plus_chunks", "mid_scalar_rejection", "checked_prefix_overflow"]],
  ["identity_alias_collision", ["alias_alias", "alias_first_class"]],
  ["reference_each_kind", REFERENCE_KIND_SUBCASES],
  ["negative_reference_registry_enumeration", REFERENCE_KIND_SUBCASES],
  ["test_symbol_fact_macro_template_excluded", ["brace", "bracket", "parenthesis"]],
  ["test_symbol_fact_comments_and_strings_excluded", ["block_comment", "character", "line_comment", "raw_string", "string"]],
  ["decoder_domain_dispatch_once", ["matrix", "testing"]],
  ["scratch_git_lifecycle", ["success_cleanup", "assertion_failure_cleanup", "foreign_handle_rejected", "double_cleanup_rejected", "seed_path_escape_rejected"]],
  ["fixture_read_rejects_escape", ["absolute", "dotdot", "foreign_root", "symlink", "non_regular"]],
  ["fixture_read_permission_exit_two", ["eacces", "eperm"]],
  ["cli_check_each_roadmap", ["matrix", "testing", "all"]],
  ["cli_write_each_single_roadmap", ["matrix", "testing"]],
  ["cli_query_each_view", ["summary", "references", "transitions", "actionables",
    "decisions", "watches", "content", "output-owners", "index"]],
  ["dispatch_capability_narrowing", ["check_read_only", "query_read_only", "write_gets_atomic_replace", "format_gets_atomic_replace"]],
]);

/**
 * Independent negative-case authority. Entries are reviewed coordinates, never derived from a
 * case result or from the observation collector that proves the exercised mutation reached them.
 */
export const FROZEN_NEGATIVE_SELFTEST_EXPECTATIONS: ReadonlyMap<string, ExpectedSelfTestIssue> =
  new Map<string, ExpectedSelfTestIssue>([
  ["codec_placeholder_path_mismatch", { code: "E-CODEC-PLACEHOLDER", logical_path: "row[1].value" }],
  ["codec_invalid_utf8", { code: "E-CODEC-UTF8", logical_path: "$" }],
  ["codec_crlf_rejected", { code: "E-CODEC-LINE-END", logical_path: "$" }],
  ["codec_bare_cr_rejected", { code: "E-CODEC-LINE-END", logical_path: "$" }],
  ["codec_surrogate_escape_rejected", { code: "E-CODEC-SCALAR", logical_path: "$" }],
  ["codec_malformed_token_rejected", { code: "E-CODEC-TOKEN", logical_path: "$" }],
  ["codec_literal_raw_control_rejected", { code: "E-CODEC-SCALAR", logical_path: "$" }],
  ["codec_alternate_string_form_rejected", { code: "E-CODEC-PLACEHOLDER", logical_path: "value" }],
  ["codec_placeholder_all_tokens_consumed", { code: "E-CODEC-PLACEHOLDER", logical_path: "b" }],
  ["strict_unknown_top", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "unknown" }],
  ["v2_unsupported", { code: "E-SCHEMA-VERSION", logical_path: "document.schema_version" }],
  ["v3_retired_keys_rejected", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "document.authority" }],
  ["transition_observable_arm_dependent", { code: "E-SCHEMA-FORBIDDEN-KEY", logical_path: "p.promotion_trigger.observable" }],
  ["strict_unknown_nested_record", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "record[0].unknown" }],
  ["strict_unknown_reference", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "reference[0].unknown" }],
  ["strict_unknown_every_table", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "fixture_schema_unknown" }],
  ["strict_unknown_kind", { code: "E-SCHEMA-ENUM", logical_path: "p.kind" }],
  ["strict_unknown_enum", { code: "E-SCHEMA-ENUM", logical_path: "p.work_state" }],
  ["strict_enum_every_field", { code: "E-SCHEMA-VERSION", logical_path: "document.schema_version" }],
  ["strict_missing_discriminator", { code: "E-SCHEMA-MISSING-KEY", logical_path: "p.kind" }],
  ["strict_generic_state_rejected", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "p.state" }],
  ["strict_generic_disposition_rejected", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "p.disposition" }],
  ["missing_section_entries", { code: "E-SCHEMA-MISSING-KEY", logical_path: "section[0].entries" }],
  ["empty_records_floor", { code: "E-SCHEMA-MISSING-KEY", logical_path: "record" }],
  ["noncanonical_basic_string", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["noncanonical_table_order", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["noncanonical_set_order", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["domain_state_required_forbidden", { code: "E-SCHEMA-FORBIDDEN-KEY", logical_path: "p.blocker_md" }],
  ["domain_defect_regression_required", { code: "E-SCHEMA-STATE", logical_path: "p" }],
  ["domain_missing_system_admission_required", { code: "E-SCHEMA-STATE", logical_path: "p" }],
  ["domain_quantitative_scope_unit_required", { code: "E-SCHEMA-MISSING-KEY", logical_path: "p.promotion_trigger.predicate.unit" }],
  ["evidence_point_requires_provenance", { code: "E-SCHEMA-STATE", logical_path: "p" }],
  ["evidence_negative_requires_enumeration", { code: "E-SCHEMA-STATE", logical_path: "p.enumerated_registry" }],
  ["evidence_generator_requires_harness_free", { code: "E-SCHEMA-STATE", logical_path: "record.matrix.fixture-task-a.evidence_ids" }],
  ["evidence_timing_join_structural", { code: "E-SCHEMA-STATE", logical_path: "record.testing.fixture-cost-historical.evidence_ids" }],
  ["evidence_draft_log_rejected", { code: "E-REFERENCE-FORBIDDEN", logical_path: "reference[4].path" }],
  ["schema_duplicate_assignment_rejected", { code: "E-TOML-PARSE", logical_path: "$" }],
  ["schema_duplicate_table_rejected", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["schema_duplicate_nested_payload_rejected", { code: "E-TOML-PARSE", logical_path: "$" }],
  ["noncanonical_comment", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["noncanonical_inline_table", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["schema_observed_at_civil_date", { code: "E-SCHEMA-TYPE", logical_path: "p.observed_at" }],
  ["schema_held_permanent_rejected", { code: "E-SCHEMA-ENUM", logical_path: "p.permanence" }],
  ["schema_due_on_valid_through_postures", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "p.unblock_predicate.due_on" }],
  ["section_entry_duplicate_record", { code: "E-SECTION-DUPLICATE", logical_path: "section[\"heading\"].entries[2]" }],
  ["section_entry_missing_part", { code: "E-SECTION-ORPHAN", logical_path: "part[\"part\"]" }],
  ["section_entry_orphan_record", { code: "E-SECTION-ORPHAN", logical_path: "record[\"matrix.fixture-work\"]" }],
  ["section_entry_unknown_id", { code: "E-SECTION-UNKNOWN", logical_path: "section[\"heading\"].entries[0]" }],
  ["section_entry_non_rendering_record", { code: "E-SECTION-KIND", logical_path: "section[\"heading\"].entries[0]" }],
  ["section_entry_duplicate_not_tiebroken", { code: "E-SECTION-DUPLICATE", logical_path: "section[\"heading\"].entries[2]" }],
  ["span_mid_scalar_boundary", { code: "E-SELFTEST-CASE", logical_path: "span_mid_scalar_boundary" }],
  ["render_zero_chunks_rejected", { code: "E-SELFTEST-CASE", logical_path: "render_zero_chunks_rejected" }],
  ["render_semantic_consumption_once", { code: "E-FIELD-CONSUMPTION", logical_path: "record[\"matrix.fixture-work\"]" }],
  ["outputs_duplicate_whole", { code: "E-OUTPUT-CLAIM", logical_path: "claims[1]" }],
  ["outputs_whole_vs_slot", { code: "E-OUTPUT-CLAIM", logical_path: "overlap[0,1]" }],
  ["outputs_duplicate_slot", { code: "E-OUTPUT-CLAIM", logical_path: "claims[1]" }],
  ["outputs_duplicate_binding", { code: "E-OUTPUT-CLAIM", logical_path: "claims[1]" }],
  ["outputs_overlapping_slots", { code: "E-OUTPUT-CLAIM", logical_path: "overlap[0,1]" }],
  ["outputs_path_escape", { code: "E-OUTPUT-PATH", logical_path: "claims[0]" }],
  ["outputs_empty_inventory", { code: "E-OUTPUT-CLAIM", logical_path: "claims" }],
  ["outputs_legacy_status_inventory_no_whole_file_claim", { code: "E-OUTPUT-AUTHORITY", logical_path: "output_claims" }],
  ["outputs_production_stage_required", { code: "E-OUTPUT-CLAIM", logical_path: "stage" }],
  ["outputs_matrix_handoff_collision", { code: "E-OUTPUT-CLAIM", logical_path: "overlap[0,4]" }],
  ["outputs_projection_path_floor", { code: "E-OUTPUT-AUTHORITY", logical_path: "output_claims.scope" }],
  ["outputs_slot_cardinality", { code: "E-OUTPUT-SLOT", logical_path: "slot[\"status-slot\"]" }],
  ["write_projection_rejects_toml", { code: "E-OUTPUT-TOML", logical_path: "document.projection_path" }],
  ["write_projection_rejects_authority_files", { code: "E-OUTPUT-PATH", logical_path: "document.projection_path" }],
  ["write_all_rejected", { code: "E-OUTPUT-AUTHORITY", logical_path: "output_claims.scope" }],
  ["format_source_single_explicit", { code: "E-OUTPUT-AUTHORITY", logical_path: "output_claims.scope" }],
  ["atomic_write_failure_preserves_target", { code: "E-SELFTEST-CASE", logical_path: "atomic_write_failure_preserves_target" }],
  ["render_chunks_precede_consumption_validation", { code: "E-FIELD-CONSUMPTION", logical_path: "record[\"matrix.fixture-work\"]" }],
  ["render_slots_resolved_before_slot_validation", { code: "E-OUTPUT-SLOT", logical_path: "section[\"heading\"].slots.status-slot" }],
  ["render_undeclared_slot_placeholder_rejected", { code: "E-OUTPUT-SLOT", logical_path: "section[\"heading\"].body_md" }],
  ["render_invalid_chunk_rejected_before_bytes", { code: "E-RENDER-AUTHORITY", logical_path: "span" }],
  ["outputs_interval_overlap", { code: "E-OUTPUT-CLAIM", logical_path: "overlap[0,1]" }],
  ["status_projector_before_after_message_parity", { code: "E-OUTPUT-SLOT", logical_path: "claims[0]" }],
  ["status_projector_preflight_no_partial_write", { code: "E-OUTPUT-SLOT", logical_path: "slot[\"tests-tier-full\"]" }],
  ["id_reserved_tokens", { code: "E-ID-RESERVED", logical_path: "matrix.fixture-item-owner" }],
  ["id_numeric_legacy_tokens", { code: "E-ID-RESERVED", logical_path: "testing.fixture-b1-owner" }],
  ["id_namespace_mismatch", { code: "E-ID-NAMESPACE", logical_path: "testing.fixture-alpha" }],
  ["identity_active_duplicate", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-alpha\"]" }],
  ["identity_active_guard_collision", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-alpha\"]" }],
  ["identity_alias_collision", { code: "E-ALIAS-COLLISION", logical_path: "alias[\"matrix.fixture-alpha\"]" }],
  ["reference_wrong_universe", { code: "E-REFERENCE-UNRESOLVED", logical_path: "reference.matrix_feature" }],
  ["reference_draft_rejected", { code: "E-REFERENCE-FORBIDDEN", logical_path: "reference.file_heading" }],
  ["reference_draft_log_rejected", { code: "E-REFERENCE-FORBIDDEN", logical_path: "reference.file_heading" }],
  ["reference_gate_stub_rejected", { code: "E-REFERENCE-STUB", logical_path: "reference.gate" }],
  ["reference_missing_file_heading", { code: "E-REFERENCE-UNRESOLVED", logical_path: "reference.file_heading" }],
  ["relation_missing_endpoint", { code: "E-RELATION-ENDPOINT", logical_path: "relation[0].target" }],
  ["relation_symmetric_duplicate", { code: "E-RELATION-DUPLICATE", logical_path: "relation-inverse.[\"overlaps\",\"matrix.fixture-alpha\",\"matrix.fixture-beta\"]" }],
  ["cycle_parent", { code: "E-RELATION-CYCLE", logical_path: "relation-cycle.parent_of" }],
  ["cycle_depends", { code: "E-RELATION-CYCLE", logical_path: "relation-cycle.depends_on" }],
  ["cycle_supersedes", { code: "E-RELATION-CYCLE", logical_path: "relation-cycle.supersedes" }],
  ["negative_reference_registry_enumeration", { code: "E-REFERENCE-UNRESOLVED", logical_path: "reference-provider.roadmap" }],
  ["citation_inventory_malformed_id_rejected", { code: "E-ID-GRAMMAR", logical_path: "roadmap-citation" }],
  ["citation_inventory_tracked_missing_rejected", { code: "E-SOURCE-MISSING", logical_path: "tracked-text" }],
  ["test_symbol_fact_missing_declared_file_rejected", { code: "E-SOURCE-MISSING", logical_path: "test-symbol-registry" }],
  ["test_symbol_fact_duplicate_id_rejected", { code: "E-ID-DUPLICATE", logical_path: "rust-test:cddl-codegen#tests::sample::works" }],
  ["fixture_registry_missing_file", { code: "E-FIXTURE-REGISTRY", logical_path: "fixture-registry.missing" }],
  ["fixture_registry_unlisted_file", { code: "E-FIXTURE-REGISTRY", logical_path: "fixture-registry.unlisted" }],
  ["fixture_registry_duplicate_id", { code: "E-FIXTURE-REGISTRY", logical_path: "fixture-registry.duplicate-id" }],
  ["selftest_empty_category_mutation", { code: "E-SELFTEST-FLOOR", logical_path: "codec" }],
  ["fixture_enumeration_rejects_escape", { code: "E-FIXTURE-REGISTRY", logical_path: "fixture-root" }],
  ["fixture_read_rejects_escape", { code: "E-FIXTURE-REGISTRY", logical_path: "fixture-path" }],
  ["fixture_read_missing_rejected", { code: "E-FIXTURE-REGISTRY", logical_path: "fixture-read" }],
  ["fixture_read_permission_exit_two", { code: "E-IO-PERMISSION", logical_path: "fixture-read" }],
  ["fixture_read_other_io_exit_two", { code: "E-IO-READ", logical_path: "fixture-read" }],
  ["fixture_expected_content_mismatch", { code: "E-FIXTURE-EXPECTED", logical_path: "golden" }],
  ["cli_no_args_rejected", { code: "E-CLI-MODE", logical_path: "argv[0]" }],
  ["cli_unknown_option", { code: "E-CLI-UNKNOWN-OPTION", logical_path: "argv[0]" }],
  ["cli_missing_value", { code: "E-CLI-MISSING-VALUE", logical_path: "argv[0]" }],
  ["cli_duplicate_scalar", { code: "E-CLI-DUPLICATE-OPTION", logical_path: "argv[3]" }],
  ["cli_duplicate_primary_mode", { code: "E-CLI-MODE", logical_path: "argv[1]" }],
  ["cli_roadmap_required", { code: "E-CLI-ROADMAP", logical_path: "argv[0]" }],
  ["cli_roadmap_forbidden_on_format", { code: "E-CLI-INCOMPATIBLE", logical_path: "argv[2]" }],
  ["cli_write_all_rejected", { code: "E-CLI-INCOMPATIBLE", logical_path: "argv[1]" }],
  ["cli_json_requires_query", { code: "E-CLI-INCOMPATIBLE", logical_path: "argv[3]" }],
  ["cli_as_of_requires_query", { code: "E-CLI-INCOMPATIBLE", logical_path: "argv[3]" }],
  ["cli_format_target_declared_only", { code: "E-CLI-FORMAT-TARGET", logical_path: "argv[0]" }],
  ["exit_declared_source_enoent_one", { code: "E-SOURCE-MISSING", logical_path: "$" }],
  ["exit_declared_source_eacces_two", { code: "E-IO-PERMISSION", logical_path: "$" }],
  ["exit_declared_reference_enoent_one", { code: "E-REFERENCE-UNRESOLVED", logical_path: "reference" }],
  ["exit_other_read_io_two", { code: "E-IO-READ", logical_path: "$" }],
  ["exit_malformed_toml_one", { code: "E-TOML-PARSE", logical_path: "$" }],
  ["exit_atomic_write_two", { code: "E-IO-WRITE", logical_path: "atomic_replace" }],
  ["exit_internal_fault_two", { code: "E-INTERNAL", logical_path: "runRoadmapCli" }],
  ["parse_error_stable_prefix", { code: "E-TOML-PARSE", logical_path: "$" }],
  ["exit_authority_stage_mismatch_one", { code: "E-OUTPUT-AUTHORITY", logical_path: "document.authority" }],
  ["failure_stdout_empty", { code: "E-CLI-MODE", logical_path: "argv[0]" }],
  ["cli_as_of_invalid_leap_day", { code: "E-CLI-AS-OF", logical_path: "argv[4]" }],
  ["cli_as_of_year_zero_rejected", { code: "E-CLI-AS-OF", logical_path: "argv[4]" }],
  ["cli_as_of_short_component_rejected", { code: "E-CLI-AS-OF", logical_path: "argv[4]" }],
  ["cli_as_of_timestamp_rejected", { code: "E-CLI-AS-OF", logical_path: "argv[4]" }],
  ["cli_as_of_whitespace_rejected", { code: "E-CLI-AS-OF", logical_path: "argv[4]" }],
  ]);

export function createCompleteSelfTestRegistry(): SelfTestRegistry {
  const families = [
    { required: REQUIRED_CODEC_SELFTEST_CASE_IDS, cases: CODEC_SELFTEST_CASES },
    { required: REQUIRED_SCHEMA_SELFTEST_CASE_IDS, cases: SCHEMA_SELFTEST_CASES },
    { required: REQUIRED_PROJECTION_SELFTEST_CASE_IDS, cases: PROJECTION_SELFTEST_CASES },
    { required: REQUIRED_IDENTITY_SELFTEST_CASE_IDS, cases: IDENTITY_SELFTEST_CASES },
    { required: REQUIRED_ADAPTER_SELFTEST_CASE_IDS, cases: ADAPTER_SELFTEST_CASES },
    { required: REQUIRED_FIXTURE_SELFTEST_CASE_IDS, cases: FIXTURE_SELFTEST_CASES },
    { required: REQUIRED_CLI_SELFTEST_CASE_IDS, cases: CLI_SELFTEST_CASES },
    { required: REQUIRED_PROJECTION_VIEW_SELFTEST_CASE_IDS, cases: PROJECTION_VIEW_SELFTEST_CASES },
  ] as const;
  const cases: SelfTestCase[] = [];
  const joinIssues: RoadmapIssue[] = [];
  for (const family of families) {
    const required = [...family.required];
    const actual = family.cases.map((testCase) => testCase.id);
    if (JSON.stringify(actual) !== JSON.stringify(required)) {
      joinIssues.push({
        code: "E-SELFTEST-CASE",
        source: "<selftest>",
        logical_path: required[0] ?? "empty-family",
        message: `case family registration differs: expected ${JSON.stringify(required)}, got ${JSON.stringify(actual)}`,
        exit: 1,
      });
    }
    for (const candidate of family.cases) {
      cases.push(Object.freeze({
        id: candidate.id,
        category: candidate.category,
        run(context: SelfTestContext): SelfTestResult {
          const result = candidate.run(context);
          const expected = FROZEN_NEGATIVE_SELFTEST_EXPECTATIONS.get(candidate.id);
          if (result.polarity === "positive") {
            if (expected !== undefined) {
              throw new Error(`${candidate.id}: positive case has frozen negative expectation`);
            }
            return result;
          }
          if (expected === undefined) {
            throw new Error(`${candidate.id}: negative case has no frozen exact expected issue`);
          }
          return Object.freeze({ ...result, expected });
        },
      }));
    }
  }
  for (const caseId of FROZEN_NEGATIVE_SELFTEST_EXPECTATIONS.keys()) {
    if (!cases.some((testCase) => testCase.id === caseId)) {
      joinIssues.push({
        code: "E-SELFTEST-CASE",
        source: "<selftest>",
        logical_path: caseId,
        message: "negative expectation names an unregistered case",
        exit: 1,
      });
    }
  }
  if (joinIssues.length > 0) throw new RoadmapFailure(joinIssues);
  return createSelfTestRegistry(
    Object.freeze(cases),
    SELFTEST_CATEGORY_FLOORS,
    FROZEN_SELFTEST_SUBCASES,
    FROZEN_NEGATIVE_SELFTEST_EXPECTATIONS,
  );
}

export function runSelfTests(ports: RoadmapSelfTestPorts): SelfTestRunResult {
  return createCompleteSelfTestRegistry().run(ports);
}
