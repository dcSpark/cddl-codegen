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
import {
  REQUIRED_SEMANTIC_CONVERSION_SELFTEST_CASE_IDS,
  SEMANTIC_CONVERSION_SELFTEST_CASES,
} from "./selftests/semantic_conversion.ts";
import { DENOMINATOR_SELFTEST_CASES, REQUIRED_DENOMINATOR_SELFTEST_CASE_IDS } from "./selftests/denominator.ts";
import { PROJECTION_VIEW_SELFTEST_CASES, REQUIRED_PROJECTION_VIEW_SELFTEST_CASE_IDS } from "./selftests/projection_views.ts";
import { RELOCATION_SELFTEST_CASES, REQUIRED_RELOCATION_SELFTEST_CASE_IDS } from "./selftests/relocation.ts";

export interface SingleFileFixtureCaseRow {
  kind: "single_file";
  id: string;
  class: "codec" | "positive" | "all_fields" | "irregular";
  input: FixtureRelativePath;
  expected?: FixtureRelativePath;
  adapter: "codec" | "matrix" | "testing" | "campaign" | "retired";
  schema_version?: 0 | 1;
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
  | "schema-v0"
  | "schema-v1"
  | "domain-matrix"
  | "domain-testing"
  | "manifest-render"
  | "spans"
  | "debt"
  | "campaign"
  | "identity-retirement"
  | "references-relations"
  | "output-ownership"
  | "determinism-purity"
  | "cli-diagnostics"
  | "fixture-registry"
  | "io-harness"
  | "repository-facts"
  | "status-compat"
  | "adapter-pipeline"
  | "denominator";

export const SELFTEST_CATEGORIES = Object.freeze([
  "codec",
  "schema-v0",
  "schema-v1",
  "domain-matrix",
  "domain-testing",
  "manifest-render",
  "spans",
  "debt",
  "campaign",
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
  "denominator",
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
  "consumer_report", "unresolved_migration",
] as const;

/** Frozen named reviewer vectors. A case absent from this map must report no subcases. */
export const FROZEN_SELFTEST_SUBCASES: ReadonlyMap<string, readonly string[]> = new Map<string, readonly string[]>([
  ["wp8_relocation_mutation_matrix", [
    "exact", "missing_tombstone", "wrong_last_active", "wrong_destination", "active_record",
    "retired_part", "manifest", "span", "relation", "reference", "retained_triad", "retained_placement",
    "retained_manifest", "retained_span", "destination_heading", "destination_claim", "part_destination_claim",
  ]],
  ["debt_external_relocation_mutation_matrix", [
    "positive", "empty", "source_fingerprint", "destination_missing", "destination_duplicate",
    "removed_owner_survives", "successor_state", "successor_document", "successor_roadmap",
    "duplicate_overlap", "capability_binding",
  ]],
  ["projection_views_layout_and_provenance", ["banner", "anchor", "layout_stages", "full_audit_separation", "span_provenance", "span_missing", "span_duplicate", "fragment_scan", "fragment_duplicate", "fragment_malformed"]],
  ["projection_views_content_exactly_once", ["exact", "missing", "duplicate", "mismatched_bytes"]],
  ["debt_semantic_promotion_rejection_matrix", [
    "raw_unclassified", "payload_markdown_bytes", "payload_array_order", "missing_replacement",
    "duplicate_replacement", "fresh_replacement_span", "missing_span", "duplicate_span", "extra_span",
    "start_coordinate", "end_coordinate", "digest", "source_kind", "owner_id", "wrong_replacement_field",
    "wrong_status", "incomplete_consumption", "rendered_byte_drift", "retained_frozen", "base_unfrozen",
    "candidate_only_raw", "existing_visibility_change",
  ]],
  ["denominator_v2_synthetic_authority", [
    "valid", "production_empty_registry", "full_pipeline_injected", "full_pipeline_empty_registry", "real_completed_render", "missing_axis_value", "derived_extra_axis_value",
    "derived_extra_legal_cell", "authored_extra_cell", "legality_flip", "duplicate_coordinate",
    "unknown_disposition", "loose_evidence", "missing_binding", "duplicate_binding",
    "wrong_evidence_scope", "uncovered_applicability", "as_of_evidence", "stale_evidence", "zero_floor", "nan_floor",
    "fractional_floor", "stale_control", "missing_exclusion_liveness",
  ]],
  ["v2_migration_escape_hatches_rejected", [
    "semantic_conversion", "frozen_legacy_span_ids", "raw_section", "raw_fragment",
    "raw_legacy_marker", "raw_record", "raw_part", "semantic_owner_raw_fields", "raw_span",
  ]],
  ["debt_semantic_promotion_capability_mutation_rejected", [
    "payload_after_mint", "completed_content_after_mint", "completed_clone_after_mint", "expected_bytes_after_mint",
  ]],
  ["debt_structural_promotion_exact", ["section", "fragment", "part", "aggregate_count"]],
  ["debt_structural_promotion_rejections", [
    "fragment_pending", "fragment_independent", "part_pending", "part_independent", "legacy_marker",
    "body_bytes", "title", "legacy_aliases", "projection_group", "parent_record", "missing_replacement",
    "duplicate_replacement", "wrong_replacement_field", "empty_review_note", "span_coordinate", "span_digest",
    "span_kind", "span_owner", "span_owner_field", "span_status", "extra_owner_span", "retained_frozen",
    "base_unfrozen", "manifest_index", "missing_base_completed", "missing_candidate_completed",
    "missing_segment", "expected_byte_drift", "debt_join",
  ]],
  ["debt_structural_promotion_capability_replay", [
    "body_after_mint", "metadata_after_mint", "disposition_after_mint", "review_note_after_mint",
    "manifest_after_mint", "relations_after_mint", "references_after_mint", "frozen_after_mint",
    "document_clone", "completed_clone", "expected_bytes_after_mint",
  ]],
  ["debt_structural_promotion_composes", ["record_shadow", "structural", "semantic_only_addition"]],
  ["debt_part_to_record_promotion_exact", [
    "deterministic_id", "manifest_slot", "parent_relation", "byte_span_frozen_debt", "blocker_delta_four",
  ]],
  ["debt_part_to_record_promotion_rejections", [
    "supporting_part", "pending_part", "omitted_disposition", "wrong_record_id", "invalid_deterministic_id",
    "base_record_collision", "base_systematic_collision", "candidate_systematic_collision", "duplicate_base_part",
    "duplicate_candidate_record", "candidate_part_retained", "missing_title", "title_drift",
    "projection_group_drift", "candidate_parent_group_move", "missing_parent_relation",
    "duplicate_parent_relation", "wrong_parent_direction", "manifest_missing", "manifest_duplicate",
    "manifest_wrong_index", "missing_replacement", "duplicate_replacement", "wrong_replacement_field",
    "empty_review_note", "detail_byte_drift", "missing_base_completed", "missing_candidate_completed",
    "base_chunk_owner_field", "candidate_chunk_owner_field", "missing_segment", "expected_byte_drift",
    "span_missing", "span_duplicate", "span_coordinate",
    "span_digest", "span_kind", "span_owner", "span_field", "span_status", "extra_owner_span",
    "retained_frozen", "base_unfrozen", "part_debt_join", "span_debt_join", "frozen_debt_join",
    "lifecycle_debt_join", "independent_growth", "unmatched_visible_record",
  ]],
  ["debt_part_to_record_promotion_capability_replay", [
    "base_part", "candidate_payload", "candidate_detail", "manifest", "relation", "span", "frozen",
    "base_debt", "candidate_debt", "base_document_clone", "candidate_document_clone",
    "base_completed_clone", "candidate_completed_clone", "completed_chunk", "expected_bytes",
  ]],
  ["debt_part_to_record_promotion_composes", [
    "part_to_record", "record_shadow", "same_kind", "semantic_only", "aggregate_count",
  ]],
  ["debt_part_adoption_exact", [
    "curated_id", "replacement_span_join", "parent_relation_join", "stable_semantics_graph", "exact_slot_bytes",
  ]],
  ["debt_part_adoption_rejections", [
    "base_not_semantic_only", "payload_drift", "title_drift", "aliases_drift", "tags_drift",
    "missing_replacement", "duplicate_replacement", "wrong_replacement_span", "wrong_replacement_field",
    "ambiguous_part_span", "candidate_part_retained", "missing_parent_relation", "wrong_parent_relation",
    "relation_graph_drift", "reference_graph_drift", "manifest_missing", "manifest_wrong_index",
    "span_owner_drift", "frozen_span_retained", "projected_bytes_drift",
  ]],
  ["render_structural_exact_field_binding_all_kinds", [
    "section", "fragment", "legacy_marker", "record", "part", "semantic_only_zero_segments",
    "progress_coverage",
  ]],
  ["render_structural_exact_field_binding_rejections", [
    "missing_segment", "duplicate_segment", "same_id_wrong_kind", "wrong_owner_id", "wrong_logical_path",
    "partial_full_field", "unsafe_coordinate", "segment_byte_drift", "expected_view_drift",
    "cloned_chunk_identity", "duplicate_chunk_identity", "missing_replacement", "duplicate_replacement",
    "wrong_replacement_field", "whole_chunk_without_segment",
  ]],
  ["outputs_slot_cardinality", ["status_zero_open", "status_two_open", "status_zero_close", "status_two_close", "reversed", "crossed", "manifest_zero", "manifest_two_declarations", "manifest_two_placements", "manifest_two_spans"]],
  ["outputs_interval_overlap", ["same_interval", "partial_left", "partial_right", "contained", "whole_vs_slot", "same_producer_overlap", "adjacent"]],
  ["status_projector_before_after_target_byte_parity", ["roadmap", "matrix_readme", "tests_readme"]],
  ["status_projector_before_after_mode_parity", ["default", "check", "write", "write_and_check_write_wins", "unrelated_arg_default"]],
  ["status_projector_before_after_message_parity", ["write_missing_marker", "check_missing_open", "check_missing_close", "check_stale_inner", "derivation_vacuity"]],
  ["exit_authority_stage_mismatch_one", ["authoritative_without_claim", "shadow_with_claim", "recognized_wrong_stage", "preactivation_recognized_wrong_stage"]],
  ["cli_authoritative_fresh_projection_reference_provenance", [
    "fresh_resolves", "drifted_prior_rejects", "missing_rejects",
  ]],
  ["query_debt_unselected_reservation_guard_tombstone_rejected", ["guard", "tombstone"]],
  ["span_expected_byte_view_cross_chunk", ["zero_length_boundaries", "one_chunk", "adjacent_chunks", "three_plus_chunks", "mid_scalar_rejection", "checked_prefix_overflow"]],
  ["identity_alias_collision", ["alias_alias", "alias_first_class"]],
  ["reference_each_kind", REFERENCE_KIND_SUBCASES],
  ["negative_reference_registry_enumeration", REFERENCE_KIND_SUBCASES],
  ["test_symbol_fact_macro_template_excluded", ["brace", "bracket", "parenthesis"]],
  ["test_symbol_fact_comments_and_strings_excluded", ["block_comment", "character", "line_comment", "raw_string", "string"]],
  ["campaign_legacy_digest_title_span", ["title", "range", "digest", "forged_title_capability", "shadow_owner", "shadow_digest", "shadow_range", "shadow_title", "shadow_index", "shadow_span_exhaustiveness"]],
  ["campaign_active_work_only", ["decision", "family", "current_guard", "tombstone", "shadow_only", "raw_without_work_shadow", "missing"]],
  ["campaign_state_fields", ["in_progress_missing_both", "in_progress_missing_pickup", "selected_forbids_pickup"]],
  ["campaign_state_transition", ["selected_to_in_progress", "in_progress_to_selected", "removal", "invalid"]],
  ["campaign_programmatic_impossible_authority_tuple", ["campaign", "lifecycle", "transition"]],
  ["campaign_allowlist_exhaustive", ["exact", "missing", "duplicate", "extra"]],
  ["retired_bad_hash_length_case", ["character", "case", "sha1_length", "sha256_length", "object_format_length"]],
  ["retired_unresolved_replacement", ["gate", "test_symbol", "file_heading", "draft", "duplicate", "external", "bare_title"]],
  ["retired_preexisting_keeps_base", ["gate", "test_symbol", "file_heading", "immutable", "reversal"]],
  ["transaction_complete_tombstone", ["complete", "missing_base_debt", "missing_candidate_debt"]],
  ["transaction_combined_relation_cycle", ["depends_on", "parent_of", "supersedes"]],
  ["transaction_complete_guard_transfer", [
    "observed_only_rejected", "under_design_rejected",
    "six_exact_debt_atoms", "six_exact_child_ids_and_kinds", "all_child_guards", "missing_base_debt", "missing_candidate_debt",
    "same_active_axis", "same_active_axis_value", "same_active_evidence", "same_active_cell", "same_active_exclusion",
    "wrong_provider_kind", "gate_pin", "test_symbol_pin", "file_heading_pin", "empty_claim", "draft_heading", "duplicate_provider", "stub_gate",
    "valid_input_reversal", "forged", "empty", "wrong_family_object", "wrong_guard_object", "direction_reversal", "overlap", "unrelated_capability",
    "attached_source_span", "unrelated_source_span", "unrelated_owner_regression", "independent_growth", "category_hide", "frozen_growth",
    "lifecycle_guard_retirement", "guard_retirement_forward", "guard_retirement_reverse", "guard_restructure_forward", "guard_restructure_reverse",
    "guard_semantic_conversion_forward", "guard_semantic_conversion_reverse",
    "all_semantic_conversion_reverse", "scoped_semantic_conversion_reverse",
  ]],
  ["transaction_v2_atomic_completed_guard", ["projected_bytes", "document_metadata"]],
  ["transaction_partial_guard", ["missing", "wrong_id", "unresolved_pin", "simultaneous_tombstone", "leftover_family", "missing_child_guard", "unused_guard", "future_reuse"]],
  ["transaction_full_hash_git_integration", ["scratch_lifecycle", "argv", "unsigned", "sha1", "sha256", "durable_corpus_facts", "crlf_byte_citation", "revision_isolation", "retirement_closure", "heading_text_precondition", "wrong_base_revision", "candidate_commit_rejected", "abbreviated"]],
  ["campaign_selection_target_kind_matches_owner", ["active_owner_tag", "valid_owner_invalid_selection", "no_capability", "structural_substitute", "undefined_substitute", "valid_capability"]],
  ["identity_reservation_shadow_binding_mismatch", ["cloned_capability", "owner_array", "sole_reservation", "sole_shadow", "shadow_evidence_mutation", "reservation_evidence_mutation"]],
  ["identity_evidence_digest_binary_inputs", ["equal_length_views", "raw_mutation", "shared_inputs"]],
  ["identity_reservation_shadow_third_owner_rejected", [
    "A-A", "A-R", "A-S", "A-G", "A-T", "R-R", "R-R-cross-namespace", "R-S", "R-S-mismatch", "R-G", "R-T", "S-S", "S-G", "S-T", "G-G", "G-T", "T-T",
    "R-S-third-A", "R-S-third-R", "R-S-third-S", "R-S-third-G", "R-S-third-T", "alias-A", "alias-R", "alias-S", "alias-G", "alias-T", "reversal",
  ]],
  ["transaction_legacy_delivery_bound_span_survives", ["moved_complete_slice", "stale_shadow_digest"]],
  ["transaction_legacy_delivery_shadow_owner_survives", ["shadow", "active", "guard", "alias"]],
  ["transaction_legacy_delivery_live_repo_citation", ["repository_citation", "relation_endpoint", "typed_reference"]],
  ["retired_test_symbol_requires_exact_id_and_symbol", ["exact", "id_mutation", "symbol_mutation", "duplicate"]],
  ["against_all_testing_authoritative_valid", ["complete_debt", "missing_base_debt", "missing_candidate_debt"]],
  ["against_all_wp4m_bootstrap_valid", ["valid_capability", "independent_issue", "no_capability", "structural_substitute", "undefined_substitute", "complete", "testing_shadow_reservation_retained", "missing_base_debt", "missing_candidate_debt", "testing_authoritative", "owner_coordinate", "frozen_span_coordinate", "debt_coordinate"]],
  ["against_all_post_activation_missing_root_rejected", ["campaign", "retired"]],
  ["against_all_shadow_to_authoritative_transfer", ["reservation_shadow_pair", "shadow_only", "reservation_missing_base_debt", "reservation_missing_candidate_debt", "shadow_missing_base_debt", "shadow_missing_candidate_debt"]],
  ["against_all_reverse_authority_rejected", ["legacy_to_shadow", "shadow_to_authoritative", "authoritative_to_shadow", "shadow_to_legacy", "legacy_to_authoritative_skipped"]],
  ["transaction_tombstone_eligible_base_owner_set", ["active_record", "current_guard", "legacy_markdown_reservation"]],
  ["transaction_tombstone_ineligible_base_owner_rejected", ["shadow_only", "active_family", "alias", "selection", "preexisting_tombstone"]],
  ["decoder_domain_dispatch_once", ["matrix", "testing"]],
  ["scratch_git_lifecycle", ["success_cleanup", "assertion_failure_cleanup", "foreign_handle_rejected", "double_cleanup_rejected", "seed_path_escape_rejected"]],
  ["fixture_read_rejects_escape", ["absolute", "dotdot", "foreign_root", "symlink", "non_regular"]],
  ["fixture_read_permission_exit_two", ["eacces", "eperm"]],
  ["cli_check_each_roadmap", ["matrix", "testing", "all"]],
  ["cli_write_each_single_roadmap", ["matrix", "testing"]],
  ["cli_query_each_view", ["summary", "debt", "references", "campaign", "signals", "actionables",
    "decisions", "families", "watches", "content", "output-owners"]],
  ["dispatch_capability_narrowing", ["check_read_only", "query_read_only", "write_gets_atomic_replace", "format_gets_atomic_replace"]],
]);

/**
 * Independent negative-case authority. Entries are reviewed coordinates, never derived from a
 * case result or from the observation collector that proves the exercised mutation reached them.
 */
export const FROZEN_NEGATIVE_SELFTEST_EXPECTATIONS: ReadonlyMap<string, ExpectedSelfTestIssue> =
  new Map<string, ExpectedSelfTestIssue>([
  ["denominator_v2_production_empty_registry_rejected", { code: "E-SCHEMA-STATE", logical_path: "record[\"matrix.fixture-denominator\"].payload.family_maturity" }],
  ["codec_placeholder_path_mismatch", { code: "E-CODEC-PLACEHOLDER", logical_path: "row[1].value" }],
  ["codec_invalid_utf8", { code: "E-CODEC-UTF8", logical_path: "$" }],
  ["codec_crlf_rejected", { code: "E-CODEC-LINE-END", logical_path: "$" }],
  ["codec_bare_cr_rejected", { code: "E-CODEC-LINE-END", logical_path: "$" }],
  ["codec_surrogate_escape_rejected", { code: "E-CODEC-SCALAR", logical_path: "$" }],
  ["codec_malformed_token_rejected", { code: "E-CODEC-TOKEN", logical_path: "$" }],
  ["codec_alternate_string_form_rejected", { code: "E-CODEC-PLACEHOLDER", logical_path: "value" }],
  ["codec_placeholder_all_tokens_consumed", { code: "E-CODEC-PLACEHOLDER", logical_path: "b" }],
  ["strict_unknown_top", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "unknown" }],
  ["strict_unknown_nested_record", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "record[0].unknown" }],
  ["strict_unknown_reference", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "reference[0].unknown" }],
  ["strict_unknown_campaign", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "campaign.unknown" }],
  ["strict_unknown_retired", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "retired_ids.unknown" }],
  ["strict_unknown_every_table", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "fixture_schema_unknown" }],
  ["strict_unknown_kind", { code: "E-SCHEMA-ENUM", logical_path: "p.kind" }],
  ["strict_unknown_enum", { code: "E-SCHEMA-ENUM", logical_path: "p.work_state" }],
  ["strict_enum_every_field", { code: "E-SCHEMA-VERSION", logical_path: "document.schema_version" }],
  ["strict_missing_discriminator", { code: "E-SCHEMA-MISSING-KEY", logical_path: "p.kind" }],
  ["strict_generic_state_rejected", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "p.state" }],
  ["strict_generic_disposition_rejected", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "p.disposition" }],
  ["v0_missing_manifest", { code: "E-SCHEMA-MISSING-KEY", logical_path: "manifest" }],
  ["v0_empty_records_floor", { code: "E-SCHEMA-MISSING-KEY", logical_path: "record" }],
  ["truncated_span_read", { code: "E-SPAN-BOUNDS", logical_path: "source_span[1].end_byte" }],
  ["campaign_missing_root", { code: "E-SCHEMA-MISSING-KEY", logical_path: "campaign" }],
  ["campaign_impossible_authority_tuple_rejected", { code: "E-SCHEMA-STATE", logical_path: "campaign.testing_authority" }],
  ["retired_missing_root", { code: "E-SCHEMA-MISSING-KEY", logical_path: "retired_ids" }],
  ["noncanonical_literal_string", { code: "E-CODEC-PLACEHOLDER", logical_path: "section[0].source_block_md" }],
  ["noncanonical_table_order", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["noncanonical_set_order", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["v0_rejects_semantics", { code: "E-SCHEMA-FORBIDDEN-KEY", logical_path: "record[0].render_authority" }],
  ["v0_rejects_authoritative", { code: "E-SCHEMA-ENUM", logical_path: "document.authority" }],
  ["v1_raw_requires_frozen_span", { code: "E-SCHEMA-STATE", logical_path: "document.frozen_legacy_span_ids" }],
  ["v1_new_raw_rejected", { code: "E-SCHEMA-STATE", logical_path: "document.frozen_legacy_span_ids" }],
  ["v1_semantic_forbids_raw", { code: "E-SCHEMA-FORBIDDEN-KEY", logical_path: "record[0].source_block_md" }],
  ["schema_lifecycle_semantic_requires_reviewed", { code: "E-SCHEMA-MISSING-KEY", logical_path: "fragment[0].lifecycle_disposition" }],
  ["schema_lifecycle_cross_kind_rejected", { code: "E-SCHEMA-ENUM", logical_path: "fragment[0].lifecycle_disposition" }],
  ["schema_lifecycle_v0_forbidden", { code: "E-SCHEMA-FORBIDDEN-KEY", logical_path: "fragment[0].lifecycle_disposition" }],
  ["schema_projection_visibility_document_arm", { code: "E-SCHEMA-MISSING-KEY", logical_path: "record[0].projection_visibility" }],
  ["schema_projection_visibility_semantic_only_arm", { code: "E-SCHEMA-STATE", logical_path: "record[0].source_replacement" }],
  ["schema_projection_visibility_forbidden_nonsemantic_arms", { code: "E-SCHEMA-FORBIDDEN-KEY", logical_path: "record[0].projection_visibility" }],
  ["v2_unresolved_migration_reference_rejected", { code: "E-SCHEMA-STATE", logical_path: "reference[0].kind" }],
  ["v3_unsupported", { code: "E-SCHEMA-VERSION", logical_path: "document.schema_version" }],
  ["domain_state_required_forbidden", { code: "E-SCHEMA-FORBIDDEN-KEY", logical_path: "p.blocker_md" }],
  ["domain_defect_regression_required", { code: "E-SCHEMA-STATE", logical_path: "p" }],
  ["domain_missing_system_admission_required", { code: "E-SCHEMA-STATE", logical_path: "p" }],
  ["domain_quantitative_scope_unit_required", { code: "E-SCHEMA-MISSING-KEY", logical_path: "p.predicate.unit" }],
  ["domain_fired_transition_not_parked", { code: "E-SCHEMA-STATE", logical_path: "record.matrix.fixture-task-f.transition_ids" }],
  ["domain_already_met_signal_rejected", { code: "E-SCHEMA-STATE", logical_path: "record.matrix.fixture-task-f.transition_ids" }],
  ["evidence_point_requires_provenance", { code: "E-SCHEMA-STATE", logical_path: "p" }],
  ["evidence_negative_requires_enumeration", { code: "E-SCHEMA-STATE", logical_path: "p.enumerated_registry" }],
  ["evidence_generator_requires_harness_free", { code: "E-SCHEMA-STATE", logical_path: "record.matrix.fixture-task-a.evidence_ids" }],
  ["evidence_timing_join_structural", { code: "E-SCHEMA-STATE", logical_path: "record.testing.fixture-cost-historical.evidence_ids" }],
  ["evidence_draft_log_rejected", { code: "E-REFERENCE-FORBIDDEN", logical_path: "reference[4].path" }],
  ["domain_closed_denominator_rejected", { code: "E-SCHEMA-ENUM", logical_path: "p.family_maturity" }],
  ["schema_duplicate_assignment_rejected", { code: "E-TOML-PARSE", logical_path: "$" }],
  ["schema_duplicate_table_rejected", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["schema_duplicate_nested_payload_rejected", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["schema_campaign_selection_binding_keys_forbidden", { code: "E-SCHEMA-FORBIDDEN-KEY", logical_path: "selection[0].pickup_commit" }],
  ["noncanonical_comment", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["noncanonical_inline_table", { code: "E-TOML-NONCANONICAL", logical_path: "$" }],
  ["systematic_illegal_cell_rejected", { code: "E-SCHEMA-ENUM", logical_path: "p.cell[0].spec_legality" }],
  ["systematic_unmodelled_coordinate_not_cell", { code: "E-SCHEMA-ENUM", logical_path: "record[40].semantic_shadow.cell[0].spec_legality" }],
  ["campaign_inline_legacy_binding_rejected", { code: "E-SCHEMA-FORBIDDEN-KEY", logical_path: "selection[0].pickup_commit" }],
  ["schema_campaign_slug_grammars", { code: "E-SCHEMA-TYPE", logical_path: "selection[0].priority_class" }],
  ["schema_observed_at_civil_date", { code: "E-SCHEMA-TYPE", logical_path: "p.observed_at" }],
  ["schema_held_permanent_rejected", { code: "E-SCHEMA-ENUM", logical_path: "p.permanence" }],
  ["schema_due_on_valid_through_postures", { code: "E-SCHEMA-UNKNOWN-KEY", logical_path: "p.due_on" }],
  ["manifest_duplicate_record", { code: "E-MANIFEST-DUPLICATE", logical_path: "manifest[6]" }],
  ["manifest_missing_part", { code: "E-MANIFEST-MISSING", logical_path: "part[\"part\"]" }],
  ["manifest_orphan_fragment", { code: "E-MANIFEST-ORPHAN", logical_path: "fragment[\"fragment\"].projection_group" }],
  ["manifest_unknown_id", { code: "E-MANIFEST-UNKNOWN", logical_path: "manifest[3]" }],
  ["manifest_wrong_kind", { code: "E-MANIFEST-KIND", logical_path: "manifest[1]" }],
  ["manifest_duplicate_legacy_marker", { code: "E-MANIFEST-DUPLICATE", logical_path: "manifest[6]" }],
  ["manifest_duplicate_not_tiebroken", { code: "E-MANIFEST-DUPLICATE", logical_path: "manifest[6]" }],
  ["span_gap", { code: "E-SPAN-GAP", logical_path: "source_span[\"span-m\"]" }],
  ["span_overlap", { code: "E-SPAN-OVERLAP", logical_path: "source_span[\"span-h\"]" }],
  ["span_wrong_digest", { code: "E-SPAN-DIGEST", logical_path: "source_span[\"span-h\"]" }],
  ["span_wrong_owner", { code: "E-SPAN-OWNER", logical_path: "source_span[\"span-h\"]" }],
  ["span_wrong_kind", { code: "E-SPAN-KIND", logical_path: "source_span[\"span-h\"]" }],
  ["span_wrong_status", { code: "E-SPAN-STATUS", logical_path: "source_span[\"span-h\"]" }],
  ["span_out_of_bounds", { code: "E-SPAN-BOUNDS", logical_path: "source_span[\"span-g\"]" }],
  ["span_reversed", { code: "E-SPAN-BOUNDS", logical_path: "source_span[\"span-h\"]" }],
  ["span_mid_scalar_boundary", { code: "E-SELFTEST-CASE", logical_path: "span_mid_scalar_boundary" }],
  ["span_empty_vacuity", { code: "E-SPAN-EMPTY", logical_path: "source_span" }],
  ["span_partial_prefix_rejected", { code: "E-SPAN-COVERAGE", logical_path: "source_span" }],
  ["span_source_change_digest_rejected", { code: "E-SOURCE-DIGEST", logical_path: "document.frozen_source_sha256" }],
  ["debt_semantic_to_raw_rejected", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "owners.[\"matrix\",\"record\",\"matrix.fixture-work\",\"source_block_md\"]" }],
  ["debt_new_key_rejected", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "owners.[\"matrix\",\"record\",\"matrix.fixture-work\",\"payload.summary_md\"]" }],
  ["debt_swap_same_count_rejected", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "owners.[\"matrix\",\"record\",\"matrix.fixture-work\",\"payload.summary_md\"]" }],
  ["debt_independent_set_growth_rejected", { code: "E-DEBT-SET-GROWTH", logical_path: "independent.[\"matrix\",\"inferred_transitions\",\"record\",\"matrix.fixture-work\",\"source_block_md\",\"transition\"]" }],
  ["debt_category_hiding_rejected", { code: "E-DEBT-CATEGORY-HIDE", logical_path: "independent.[\"matrix\",\"unresolved_references\",\"record\",\"matrix.fixture-work\",\"source_block_md\",\"transition\"]" }],
  ["debt_frozen_set_growth_rejected", { code: "E-DEBT-FROZEN-SET", logical_path: "frozen_legacy_spans" }],
  ["debt_v1_v0_rejected", { code: "E-DEBT-BASE-MISMATCH", logical_path: "document.schema_version" }],
  ["debt_unrelated_base_rejected", { code: "E-DEBT-BASE-MISMATCH", logical_path: "document" }],
  ["debt_semantic_promotion_payload_rejected", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "record[\"matrix.fixture-work\"]" }],
  ["debt_semantic_promotion_span_rejected", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "record[\"matrix.fixture-work\"]" }],
  ["debt_semantic_promotion_visibility_rejected", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "record[\"matrix.fixture-work\"]" }],
  ["debt_candidate_only_document_rejected", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "record[\"matrix.fixture-semantic-only-addition\"]" }],
  ["debt_semantic_promotion_rejection_matrix", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "record[\"matrix.fixture-work\"]" }],
  ["debt_semantic_promotion_swapped_segment_rejected", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "record[\"matrix.fixture-work\"]" }],
  ["debt_semantic_promotion_capability_mutation_rejected", { code: "E-DEBT-BASE-MISMATCH", logical_path: "transition_facts" }],
  ["debt_structural_promotion_rejections", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "fragment[\"fragment\"]" }],
  ["debt_structural_promotion_capability_replay", { code: "E-DEBT-BASE-MISMATCH", logical_path: "transition_facts" }],
  ["debt_part_to_record_promotion_rejections", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "part_to_record[\"matrix.part\"]" }],
  ["debt_part_to_record_promotion_capability_replay", { code: "E-DEBT-BASE-MISMATCH", logical_path: "transition_facts" }],
  ["debt_part_adoption_rejections", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "part_to_record[\"matrix.curated-adoption\"]" }],
  ["render_zero_chunks_rejected", { code: "E-SELFTEST-CASE", logical_path: "render_zero_chunks_rejected" }],
  ["render_semantic_consumption_once", { code: "E-FIELD-CONSUMPTION", logical_path: "record[\"matrix.fixture-work\"]" }],
  ["render_semantic_only_identity_debt", { code: "E-DEBT-BASE-MISMATCH", logical_path: "transition_facts" }],
  ["render_semantic_only_span_prohibition", { code: "E-SPAN-OWNER", logical_path: "source_span[\"span-summary\"]" }],
  ["render_semantic_exact_field_binding_swapped_labels", { code: "E-SPAN-OWNER", logical_path: "source_span[\"span-summary\"]" }],
  ["render_semantic_exact_field_binding_partial", { code: "E-SPAN-OWNER", logical_path: "source_span[\"span-summary\"]" }],
  ["render_semantic_exact_field_binding_duplicate", { code: "E-FIELD-CONSUMPTION", logical_path: "record[\"matrix.fixture-work\"].projected_field_segments" }],
  ["render_structural_exact_field_binding_rejections", { code: "E-FIELD-CONSUMPTION", logical_path: "section[\"heading\"].projected_field_segments" }],
  ["outputs_duplicate_whole", { code: "E-OUTPUT-CLAIM", logical_path: "claims[1]" }],
  ["outputs_whole_vs_slot", { code: "E-OUTPUT-CLAIM", logical_path: "overlap[0,1]" }],
  ["outputs_duplicate_slot", { code: "E-OUTPUT-CLAIM", logical_path: "claims[1]" }],
  ["outputs_duplicate_binding", { code: "E-OUTPUT-CLAIM", logical_path: "claims[1]" }],
  ["outputs_overlapping_slots", { code: "E-OUTPUT-CLAIM", logical_path: "overlap[0,1]" }],
  ["outputs_path_escape", { code: "E-OUTPUT-PATH", logical_path: "claims[0]" }],
  ["outputs_empty_inventory", { code: "E-OUTPUT-CLAIM", logical_path: "claims" }],
  ["outputs_production_stage_required", { code: "E-OUTPUT-CLAIM", logical_path: "stage" }],
  ["outputs_shadow_no_claim", { code: "E-OUTPUT-AUTHORITY", logical_path: "document.authority" }],
  ["outputs_matrix_handoff_collision", { code: "E-OUTPUT-CLAIM", logical_path: "overlap[0,4]" }],
  ["outputs_projection_path_floor", { code: "E-OUTPUT-AUTHORITY", logical_path: "output_claims.scope" }],
  ["outputs_slot_cardinality", { code: "E-OUTPUT-SLOT", logical_path: "slot[\"status-slot\"]" }],
  ["write_projection_rejects_toml", { code: "E-OUTPUT-TOML", logical_path: "document.projection_path" }],
  ["write_projection_rejects_authority_files", { code: "E-OUTPUT-PATH", logical_path: "document.projection_path" }],
  ["write_shadow_rejected", { code: "E-OUTPUT-AUTHORITY", logical_path: "output_claims.scope" }],
  ["write_all_rejected", { code: "E-OUTPUT-AUTHORITY", logical_path: "output_claims.scope" }],
  ["format_source_single_explicit", { code: "E-OUTPUT-AUTHORITY", logical_path: "output_claims.scope" }],
  ["atomic_write_failure_preserves_target", { code: "E-SELFTEST-CASE", logical_path: "atomic_write_failure_preserves_target" }],
  ["debt_owner_field_rename_requires_witness", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "owners.[\"matrix\",\"record\",\"matrix.fixture-work\",\"payload.summary_md\"]" }],
  ["debt_new_raw_span_rejected", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "owners.[\"matrix\",\"source_span\",\"span-new\",\"coverage\"]" }],
  ["debt_cutover_revealed_v1_v1_rejected", { code: "E-DEBT-SET-GROWTH", logical_path: "independent.[\"matrix\",\"inferred_transitions\",\"record\",\"matrix.fixture-work\",\"source_block_md\",\"payload.work_state\"]" }],
  ["debt_cutover_revealed_v1_v0_rejected", { code: "E-DEBT-BASE-MISMATCH", logical_path: "document.schema_version" }],
  ["debt_cutover_revealed_wrong_category_rejected", { code: "E-DEBT-SET-GROWTH", logical_path: "independent.[\"matrix\",\"unrendered_fields\",\"record\",\"matrix.fixture-work\",\"source_block_md\",\"payload.work_state\"]" }],
  ["debt_cutover_revealed_wrong_subject_rejected", { code: "E-DEBT-SET-GROWTH", logical_path: "independent.[\"matrix\",\"inferred_transitions\",\"record\",\"matrix.fixture-work\",\"source_block_md\",\"payload.not_the_pending_coordinate\"]" }],
  ["debt_cutover_revealed_wrong_owner_rejected", { code: "E-DEBT-SET-GROWTH", logical_path: "independent.[\"matrix\",\"inferred_transitions\",\"section\",\"heading\",\"source_block_md\",\"payload.work_state\"]" }],
  ["debt_cutover_revealed_missing_record_rejected", { code: "E-DEBT-SET-GROWTH", logical_path: "independent.[\"matrix\",\"inferred_transitions\",\"record\",\"matrix.fixture-missing\",\"source_block_md\",\"payload.work_state\"]" }],
  ["debt_cutover_revealed_no_shadow_rejected", { code: "E-DEBT-SET-GROWTH", logical_path: "independent.[\"matrix\",\"inferred_transitions\",\"record\",\"matrix.fixture-work\",\"source_block_md\",\"payload.work_state\"]" }],
  ["debt_cutover_revealed_nonwork_rejected", { code: "E-DEBT-SET-GROWTH", logical_path: "independent.[\"matrix\",\"inferred_transitions\",\"record\",\"matrix.fixture-work\",\"source_block_md\",\"payload.work_state\"]" }],
  ["debt_cutover_revealed_state_mismatch_rejected", { code: "E-DEBT-SET-GROWTH", logical_path: "independent.[\"matrix\",\"inferred_transitions\",\"record\",\"matrix.fixture-work\",\"source_block_md\",\"payload.work_state\"]" }],
  ["debt_cutover_revealed_classification_mismatch_rejected", { code: "E-DEBT-SET-GROWTH", logical_path: "independent.[\"matrix\",\"pending_family_classifications\",\"record\",\"matrix.fixture-work\",\"source_block_md\",\"payload.family_classification\"]" }],
  ["debt_cutover_revealed_category_hide_rejected", { code: "E-DEBT-CATEGORY-HIDE", logical_path: "independent.[\"matrix\",\"inferred_transitions\",\"record\",\"matrix.fixture-work\",\"source_block_md\",\"payload.work_state\"]" }],
  ["render_chunks_precede_consumption_validation", { code: "E-FIELD-CONSUMPTION", logical_path: "record[\"matrix.fixture-work\"]" }],
  ["render_chunks_precede_span_validation", { code: "E-SPAN-EMPTY", logical_path: "source_span" }],
  ["render_slots_resolved_before_slot_validation", { code: "E-OUTPUT-SLOT", logical_path: "generated_slot[\"status-slot\"]" }],
  ["render_invalid_chunk_skips_projection_read", { code: "E-SPAN-GAP", logical_path: "span" }],
  ["render_projection_mutation_changes_only_drift", { code: "E-PROJECTION-DRIFT", logical_path: "projection" }],
  ["outputs_interval_overlap", { code: "E-OUTPUT-CLAIM", logical_path: "overlap[0,1]" }],
  ["status_projector_before_after_message_parity", { code: "E-OUTPUT-SLOT", logical_path: "claims[0]" }],
  ["status_projector_preflight_no_partial_write", { code: "E-OUTPUT-SLOT", logical_path: "slot[\"tests-tier-full\"]" }],
  ["id_reserved_tokens", { code: "E-ID-RESERVED", logical_path: "matrix.fixture-item-owner" }],
  ["id_numeric_legacy_tokens", { code: "E-ID-RESERVED", logical_path: "testing.fixture-b1-owner" }],
  ["id_namespace_mismatch", { code: "E-ID-NAMESPACE", logical_path: "testing.fixture-alpha" }],
  ["identity_active_duplicate", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-alpha\"]" }],
  ["identity_active_guard_collision", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-alpha\"]" }],
  ["identity_active_tombstone_collision", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-alpha\"]" }],
  ["identity_guard_tombstone_collision", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-alpha\"]" }],
  ["identity_alias_collision", { code: "E-ALIAS-COLLISION", logical_path: "alias[\"matrix.fixture-alpha\"]" }],
  ["identity_retired_reuse", { code: "E-RETIRED-REUSE", logical_path: "retired[\"matrix.fixture-alpha\"]" }],
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
  ["transaction_combined_relation_cycle", { code: "E-RELATION-CYCLE", logical_path: "relation-cycle.depends_on" }],
  ["transaction_combined_relation_inverse_duplicate", { code: "E-RELATION-DUPLICATE", logical_path: "relation-inverse.[\"overlaps\",\"matrix.fixture-lifecycle\",\"testing.fixture-lifecycle\"]" }],
  ["negative_reference_registry_enumeration", { code: "E-REFERENCE-UNRESOLVED", logical_path: "reference-provider.roadmap" }],
  ["citation_inventory_malformed_id_rejected", { code: "E-ID-GRAMMAR", logical_path: "roadmap-citation" }],
  ["citation_inventory_tracked_missing_rejected", { code: "E-SOURCE-MISSING", logical_path: "tracked-text" }],
  ["test_symbol_fact_missing_declared_file_rejected", { code: "E-SOURCE-MISSING", logical_path: "test-symbol-registry" }],
  ["test_symbol_fact_duplicate_id_rejected", { code: "E-ID-DUPLICATE", logical_path: "rust-test:cddl-codegen#tests::sample::works" }],
  ["campaign_legacy_digest_title_span", { code: "E-CAMPAIGN-TARGET", logical_path: "legacy_markdown_reservation[0]" }],
  ["campaign_legacy_whole_digest", { code: "E-CAMPAIGN-TARGET", logical_path: "legacy_markdown_reservation[0]" }],
  ["campaign_expire_matrix_at_v1", { code: "E-CAMPAIGN-TARGET-EXPIRED", logical_path: "legacy_markdown_reservation[0]" }],
  ["campaign_expire_testing_at_v1", { code: "E-CAMPAIGN-TARGET-EXPIRED", logical_path: "legacy_markdown_reservation[0]" }],
  ["campaign_direct_requires_active_v1", { code: "E-CAMPAIGN-TARGET", logical_path: "selection[0]" }],
  ["campaign_active_work_only", { code: "E-CAMPAIGN-TARGET", logical_path: "selection[\"matrix.fixture-lifecycle\"]" }],
  ["campaign_unique_selection", { code: "E-CAMPAIGN-DUPLICATE", logical_path: "selection[1]" }],
  ["campaign_state_fields", { code: "E-CAMPAIGN-STATE", logical_path: "selection[0]" }],
  ["campaign_programmatic_impossible_authority_tuple", { code: "E-SCHEMA-STATE", logical_path: "campaign.testing_authority" }],
  ["campaign_fired_promotion_visible", { code: "E-CAMPAIGN-FIRED-HIDDEN", logical_path: "selection[\"matrix.fixture-lifecycle\"]" }],
  ["campaign_allowlist_stale_rejected", { code: "E-CAMPAIGN-TARGET-EXPIRED", logical_path: "cross_roadmap_allowlist" }],
  ["retired_bad_hash_length_case", { code: "E-RETIRED-HASH", logical_path: "retired_ids.entry[0].last_active_at" }],
  ["retired_missing_replacement", { code: "E-RETIRED-REPLACEMENT", logical_path: "retired_ids.entry[0].replacement" }],
  ["retired_roadmap_replacement_rejected", { code: "E-RETIRED-REPLACEMENT", logical_path: "retired_ids.entry[0].replacement" }],
  ["retired_unresolved_replacement", { code: "E-RETIRED-REPLACEMENT", logical_path: "retired_ids.entry[0].replacement" }],
  ["retired_gate_stub", { code: "E-RETIRED-REPLACEMENT", logical_path: "retired_ids.entry[0].replacement" }],
  ["retired_preexisting_keeps_base", { code: "E-RETIRED-REUSE", logical_path: "retired_ids.entry[\"matrix.fixture-lifecycle\"]" }],
  ["retired_wrong_against", { code: "E-RETIRED-HASH", logical_path: "retired_ids.entry[\"matrix.fixture-lifecycle\"].last_active_at" }],
  ["transaction_missing_campaign_removal", { code: "E-CAMPAIGN-TARGET", logical_path: "selection[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_live_citation", { code: "E-TRANSACTION-CITATION", logical_path: "citation[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_dangling_relation", { code: "E-TRANSACTION-REFERENCE", logical_path: "relation[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_dangling_reference", { code: "E-TRANSACTION-REFERENCE", logical_path: "reference[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_missing_tombstone", { code: "E-TRANSACTION-OWNER", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_unused_tombstone", { code: "E-TRANSACTION-ORIGIN", logical_path: "retired_ids.entry[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_partial_guard", { code: "E-TRANSACTION-GUARD", logical_path: "guard[\"matrix.fixture-unused-guard\"]" }],
  ["transaction_family_tombstone_rejected", { code: "E-TRANSACTION-ORIGIN", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_linked_work_tombstone_required", { code: "E-TRANSACTION-OWNER", logical_path: "owner[\"matrix.fixture-linked-work\"]" }],
  ["transaction_duplicate_current_owner", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_full_hash_git_integration", { code: "E-TRANSACTION-BASE", logical_path: "base.registry.revision" }],
  ["campaign_reservation_work_kind_required", { code: "E-TRANSACTION-OWNER", logical_path: "owner[\"matrix.fixture-lifecycle\"].work_kind" }],
  ["campaign_selection_target_kind_matches_owner", { code: "E-CAMPAIGN-TARGET", logical_path: "selection[\"matrix.fixture-lifecycle\"]" }],
  ["campaign_new_legacy_selection_is_atomic", { code: "E-CAMPAIGN-TRANSITION", logical_path: "selection[\"matrix.fixture-lifecycle\"]" }],
  ["campaign_reservation_rebinds_whole_source", { code: "E-CAMPAIGN-TARGET", logical_path: "legacy_markdown_reservation[1]" }],
  ["identity_reservation_cross_namespace_collision", { code: "E-OWNER-DUPLICATE", logical_path: "additional_owners[0]" }],
  ["identity_reservation_active_collision", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["identity_reservation_tombstone_collision", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["identity_reservation_shadow_binding_mismatch", { code: "E-OWNER-DUPLICATE", logical_path: "additional_owners" }],
  ["identity_reservation_shadow_third_owner_rejected", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_legacy_cutover_work_kind_mismatch", { code: "E-TRANSACTION-OWNER", logical_path: "owner[\"matrix.fixture-lifecycle\"].work_kind" }],
  ["transaction_legacy_delivery_selection_survives", { code: "E-CAMPAIGN-TARGET", logical_path: "selection[0]" }],
  ["transaction_legacy_delivery_reservation_survives", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_legacy_delivery_bound_span_survives", { code: "E-TRANSACTION-CAMPAIGN", logical_path: "owner[\"matrix.fixture-lifecycle\"].bound_source" }],
  ["transaction_legacy_delivery_shadow_owner_survives", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_legacy_delivery_missing_tombstone", { code: "E-TRANSACTION-OWNER", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_legacy_delivery_wrong_last_active", { code: "E-RETIRED-HASH", logical_path: "retired_ids.entry[\"matrix.fixture-lifecycle\"].last_active_at" }],
  ["transaction_legacy_delivery_live_repo_citation", { code: "E-TRANSACTION-CITATION", logical_path: "citation[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_shadow_only_delivery_rejected", { code: "E-TRANSACTION-ORIGIN", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_single_roadmap_owner_removal_rejected", { code: "E-TRANSACTION-OWNER", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["against_forged_debt_transition_facts_ignored", { code: "E-DEBT-OWNER-REGRESSION", logical_path: "record[\"matrix.fixture-lifecycle\"]" }],
  ["transaction_citation_in_nonroadmap_file_rejected", { code: "E-TRANSACTION-CITATION", logical_path: "citation[\"matrix.fixture-lifecycle\"]" }],
  ["against_per_roadmap_candidate_global_collision_rejected", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["against_per_roadmap_absent_selected_source_rejected", { code: "E-TRANSACTION-BASE", logical_path: "matrix" }],
  ["against_per_roadmap_shadow_selected_source_rejected", { code: "E-TRANSACTION-BASE", logical_path: "matrix" }],
  ["against_per_roadmap_owner_change_requires_all", { code: "E-TRANSACTION-OWNER", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
  ["against_all_state_forbids_unexpected_toml", { code: "E-SCHEMA-STATE", logical_path: "campaign.matrix_authority" }],
  ["against_all_state_requires_shadow_toml", { code: "E-SOURCE-MISSING", logical_path: "campaign.matrix_authority" }],
  ["against_all_state_requires_authoritative_toml", { code: "E-SOURCE-MISSING", logical_path: "campaign.matrix_authority" }],
  ["against_all_post_activation_missing_root_rejected", { code: "E-SOURCE-MISSING", logical_path: "campaign" }],
  ["against_all_reverse_authority_rejected", { code: "E-CAMPAIGN-TRANSITION", logical_path: "campaign.matrix_authority" }],
  ["transaction_tombstone_ineligible_base_owner_rejected", { code: "E-TRANSACTION-ORIGIN", logical_path: "owner[\"matrix.fixture-lifecycle\"]" }],
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
  ["cli_against_requires_check", { code: "E-CLI-AGAINST", logical_path: "argv[3]" }],
  ["cli_against_write_rejected", { code: "E-CLI-AGAINST", logical_path: "argv[3]" }],
  ["cli_against_query_rejected", { code: "E-CLI-AGAINST", logical_path: "argv[4]" }],
  ["cli_against_selftest_rejected", { code: "E-CLI-AGAINST", logical_path: "argv[1]" }],
  ["cli_against_format_rejected", { code: "E-CLI-AGAINST", logical_path: "argv[2]" }],
  ["cli_json_requires_query", { code: "E-CLI-INCOMPATIBLE", logical_path: "argv[3]" }],
  ["cli_as_of_requires_query", { code: "E-CLI-INCOMPATIBLE", logical_path: "argv[3]" }],
  ["cli_format_target_declared_only", { code: "E-CLI-FORMAT-TARGET", logical_path: "argv[0]" }],
  ["cli_unresolved_base_exit_two", { code: "E-GIT-BASE-LOOKUP", logical_path: "against" }],
  ["cli_noncommit_base_exit_two", { code: "E-GIT-BASE-LOOKUP", logical_path: "against" }],
  ["exit_declared_source_enoent_one", { code: "E-SOURCE-MISSING", logical_path: "$" }],
  ["exit_declared_projection_enoent_one", { code: "E-PROJECTION-MISSING", logical_path: "projection" }],
  ["exit_declared_source_eacces_two", { code: "E-IO-PERMISSION", logical_path: "$" }],
  ["exit_declared_reference_enoent_one", { code: "E-REFERENCE-UNRESOLVED", logical_path: "reference" }],
  ["exit_other_read_io_two", { code: "E-IO-READ", logical_path: "$" }],
  ["exit_malformed_toml_one", { code: "E-TOML-PARSE", logical_path: "$" }],
  ["exit_atomic_write_two", { code: "E-IO-WRITE", logical_path: "atomic_replace" }],
  ["exit_internal_fault_two", { code: "E-INTERNAL", logical_path: "runRoadmapCli" }],
  ["parse_error_stable_prefix", { code: "E-TOML-PARSE", logical_path: "$" }],
  ["exit_authority_stage_mismatch_one", { code: "E-OUTPUT-AUTHORITY", logical_path: "document.authority" }],
  ["failure_stdout_empty", { code: "E-CLI-MODE", logical_path: "argv[0]" }],
  ["query_debt_duplicate_campaign_selection_rejected", { code: "E-CAMPAIGN-DUPLICATE", logical_path: "selection[1]" }],
  ["query_debt_unselected_duplicate_selection_rejected", { code: "E-CAMPAIGN-DUPLICATE", logical_path: "selection[1]" }],
  ["query_debt_unselected_duplicate_reservation_rejected", { code: "E-CAMPAIGN-DUPLICATE", logical_path: "legacy_markdown_reservation[1]" }],
  ["query_debt_selected_shadow_title_binding_rejected", { code: "E-CAMPAIGN-TARGET", logical_path: "record[\"matrix.additional-tool-annotations\"]" }],
  ["query_debt_unselected_legacy_selection_missing_reservation_rejected", { code: "E-CAMPAIGN-TARGET", logical_path: "selection[0]" }],
  ["query_debt_unselected_active_reservation_rejected", { code: "E-CAMPAIGN-TARGET", logical_path: "selection[0]" }],
  ["query_debt_unselected_reservation_guard_tombstone_rejected", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"testing.fixture-reserved\"]" }],
  ["query_debt_selected_alias_reservation_rejected", { code: "E-ALIAS-COLLISION", logical_path: "alias[\"testing.fixture-reserved\"]" }],
  ["query_debt_invalid_campaign_target_rejected", { code: "E-CAMPAIGN-TARGET", logical_path: "selection[0]" }],
  ["query_debt_invalid_campaign_state_rejected", { code: "E-SCHEMA-FORBIDDEN-KEY", logical_path: "selection[0].pickup_commit" }],
  ["query_debt_authoritative_reservation_rejected", { code: "E-CAMPAIGN-TARGET-EXPIRED", logical_path: "legacy_markdown_reservation[0]" }],
  ["query_debt_active_tombstone_collision_rejected", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixed-array.static-representation\"]" }],
  ["query_debt_active_guard_collision_rejected", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixed-array.static-representation\"]" }],
  ["query_debt_active_reservation_collision_rejected", { code: "E-OWNER-DUPLICATE", logical_path: "owner[\"matrix.fixed-array.static-representation\"]" }],
  ["query_debt_invalid_guard_pin_rejected", { code: "E-TRANSACTION-GUARD", logical_path: "guard[\"matrix.fixture-guard\"]" }],
  ["query_debt_authority_mismatch_rejected", { code: "E-SCHEMA-STATE", logical_path: "campaign.matrix_authority" }],
  ["query_debt_missing_campaign_root_rejected", { code: "E-SOURCE-MISSING", logical_path: "$" }],
  ["query_debt_missing_retired_root_rejected", { code: "E-SOURCE-MISSING", logical_path: "$" }],
  ["query_debt_invalid_retired_pin_rejected", { code: "E-RETIRED-REPLACEMENT", logical_path: "retired_ids.entry[0].replacement" }],
  ["query_debt_stage_mismatch_rejected", { code: "E-OUTPUT-CLAIM", logical_path: "production_output_stage" }],
  ["cli_against_missing_value", { code: "E-CLI-MISSING-VALUE", logical_path: "argv[3]" }],
  ["cli_against_duplicate_value", { code: "E-CLI-DUPLICATE-OPTION", logical_path: "argv[5]" }],
  ["cli_against_bad_length_git_format_no_usage", { code: "E-GIT-BASE-FORMAT", logical_path: "against" }],
  ["cli_against_uppercase_git_format_no_usage", { code: "E-GIT-BASE-FORMAT", logical_path: "against" }],
  ["cli_against_nonhex_git_format_no_usage", { code: "E-GIT-BASE-FORMAT", logical_path: "against" }],
  ["cli_against_unresolved_git_lookup_no_usage", { code: "E-GIT-BASE-LOOKUP", logical_path: "against" }],
  ["cli_against_incompatible_precedes_bad_format", { code: "E-CLI-AGAINST", logical_path: "argv[3]" }],
  ["cli_against_v2_mixed_promotion_rejected", { code: "E-TRANSACTION-BASE", logical_path: "document.schema_version" }],
  ["cli_against_v2_scoped_promotion_rejected", { code: "E-TRANSACTION-BASE", logical_path: "matrix" }],
  ["cli_against_v2_semantic_drift_rejected", { code: "E-TRANSACTION-BASE", logical_path: "matrix.document.schema_version" }],
  ["cli_against_v2_incomplete_base_rejected", { code: "E-TRANSACTION-BASE", logical_path: "matrix.document.schema_version" }],
  ["cli_against_v2_downgrade_rejected", { code: "E-TRANSACTION-BASE", logical_path: "matrix.document.schema_version" }],
  ["cli_against_v2_preexisting_mixed_rejected", { code: "E-TRANSACTION-BASE", logical_path: "base.document.schema_version" }],
  ["cli_against_v2_campaign_drift_rejected", { code: "E-TRANSACTION-BASE", logical_path: "campaign" }],
  ["cli_against_v2_retired_drift_rejected", { code: "E-TRANSACTION-BASE", logical_path: "retired_ids" }],
  ["cli_as_of_invalid_leap_day", { code: "E-CLI-AS-OF", logical_path: "argv[4]" }],
  ["cli_as_of_year_zero_rejected", { code: "E-CLI-AS-OF", logical_path: "argv[4]" }],
  ["cli_as_of_short_component_rejected", { code: "E-CLI-AS-OF", logical_path: "argv[4]" }],
  ["cli_as_of_timestamp_rejected", { code: "E-CLI-AS-OF", logical_path: "argv[4]" }],
  ["cli_as_of_whitespace_rejected", { code: "E-CLI-AS-OF", logical_path: "argv[4]" }],
  ]);

export function createCompleteSelfTestRegistry(): SelfTestRegistry {
  // This list is deliberately constructed at call time. selftests/cli.ts exercises the public
  // service façade, so eager reads here would observe its ESM bindings during cycle initialization.
  const families = [
    { required: REQUIRED_CODEC_SELFTEST_CASE_IDS, cases: CODEC_SELFTEST_CASES },
    { required: REQUIRED_SCHEMA_SELFTEST_CASE_IDS, cases: SCHEMA_SELFTEST_CASES },
    { required: REQUIRED_PROJECTION_SELFTEST_CASE_IDS, cases: PROJECTION_SELFTEST_CASES },
    { required: REQUIRED_IDENTITY_SELFTEST_CASE_IDS, cases: IDENTITY_SELFTEST_CASES },
    { required: REQUIRED_ADAPTER_SELFTEST_CASE_IDS, cases: ADAPTER_SELFTEST_CASES },
    { required: REQUIRED_FIXTURE_SELFTEST_CASE_IDS, cases: FIXTURE_SELFTEST_CASES },
    { required: REQUIRED_CLI_SELFTEST_CASE_IDS, cases: CLI_SELFTEST_CASES },
    {
      required: REQUIRED_SEMANTIC_CONVERSION_SELFTEST_CASE_IDS,
      cases: SEMANTIC_CONVERSION_SELFTEST_CASES,
    },
    { required: REQUIRED_DENOMINATOR_SELFTEST_CASE_IDS, cases: DENOMINATOR_SELFTEST_CASES },
    { required: REQUIRED_PROJECTION_VIEW_SELFTEST_CASE_IDS, cases: PROJECTION_VIEW_SELFTEST_CASES },
    { required: REQUIRED_RELOCATION_SELFTEST_CASE_IDS, cases: RELOCATION_SELFTEST_CASES },
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
