export const REQUIRED_FIXTURE_SELFTEST_CASE_IDS = [
  "fixture_registry_no_orphans",
  "fixture_registry_missing_file",
  "fixture_registry_unlisted_file",
  "fixture_registry_duplicate_id",
  "fixture_family_floors",
  "slot_resolver_four_matrix_slots",
  "reference_provider_members_nonempty_or_declared_empty",
  "selftest_case_ids_unique",
  "selftest_case_count_floor",
  "selftest_empty_category_mutation",
  "selftest_all_categories_receipted",
  "io_effect_import_funnel",
  "fixture_enumeration_recursive_sorted",
  "fixture_enumeration_rejects_escape",
  "scratch_git_lifecycle",
  "scratch_git_argv_no_shell",
  "fixture_read_exact_bytes",
  "fixture_read_rejects_escape",
  "fixture_read_missing_rejected",
  "fixture_read_permission_exit_two",
  "fixture_read_other_io_exit_two",
  "fixture_expected_content_mismatch",
  "fixture_status_bundle_nine_paths",
  "fixture_status_bundle_golden_mapping",
] as const;

export type RequiredFixtureSelfTestCaseId =
  (typeof REQUIRED_FIXTURE_SELFTEST_CASE_IDS)[number];

// The all-fields oracle is deliberately independent of fixture bytes. C2 supplies decoded fixture
// tags to compareAllFieldsCoverageTags; it must not derive the expected universe from those tags.
export const ALL_FIELDS_EXPECTED_COVERAGE_BY_AXIS = {
  admission_kind: ["bounded_denominator", "independent_recurrence", "silent_corruption"],
  authority: ["authoritative"],
  authority_kind: ["grammar", "registry", "reviewed_relation"],
  campaign_state: ["closing", "designing", "enumerating"],
  cell_disposition: ["deliberately_unsupported", "safely_refused", "supported", "unknown"],
  closeout_state: ["blocked", "due", "waiting"],
  comparator: ["eq", "ge", "gt", "le", "lt"],
  control_kind: [
    "consumer_ci",
    "fixture",
    "gate",
    "operator_procedure",
    "review_rule",
    "test",
    "upstream_issue",
  ],
  control_state: ["live", "proposed", "stale"],
  cost_posture: ["historical_observation", "live_registry"],
  decision_arm: ["decided_permanent", "decided_reopenable", "held_reopenable", "pending"],
  decision_state: ["decided", "held", "pending"],
  evidence_kind: [
    "committed_vector",
    "consumer_report",
    "decision",
    "execution_probe",
    "external_commit",
    "external_issue",
    "gate",
    "harness_free_repro",
    "incident",
    "registry_enumeration",
    "regression_pin",
    "source_read",
    "spec_read",
  ],
  evidence_stage: [
    "compiled",
    "constraint_enforced",
    "executed",
    "generated",
    "independently_decoded",
    "over_accepted",
    "round_tripped",
  ],
  evidence_verdict: ["confirmed", "falsified", "inapplicable", "proposed", "unknown"],
  family_classification: ["none_reviewed", "pending"],
  family_maturity: ["observed_only", "under_design"],
  freshness: ["as_of", "historical", "live", "stale"],
  frozen_source_eof: ["lf"],
  incident_posture: ["attributed", "historical", "live"],
  manifest_kind: ["fragment", "generated_slot", "legacy_marker", "part", "record", "section"],
  migration_status: ["generated", "raw", "replaced"],
  payload_kind: [
    "control",
    "decision",
    "evidence",
    "family",
    "matrix_external_closeout",
    "matrix_policy",
    "signal",
    "testing_cost",
    "testing_incident",
    "testing_operational_watch",
    "testing_system_admission",
    "work",
  ],
  permanence: ["permanent", "reopenable"],
  policy_kind: ["boundary", "maintenance_protocol"],
  predicate_kind: ["event", "manual", "quantitative"],
  priority_band: ["critical", "high", "low", "normal"],
  reference_kind: [
    "consumer_report",
    "external_commit",
    "external_issue",
    "external_release",
    "file_heading",
    "gate",
    "matrix_cell",
    "matrix_feature",
    "matrix_role",
    "roadmap",
    "spec_passage",
    "test_symbol",
    "unresolved_migration",
  ],
  relation_kind: [
    "blocked_by",
    "complements",
    "delegates_to",
    "depends_on",
    "overlaps",
    "parent_of",
    "related",
    "reopens",
    "split_from",
    "supersedes",
  ],
  render_authority: ["raw", "semantic"],
  risk: [
    "abort_or_panic",
    "compile_failure",
    "cosmetic",
    "false_pass_or_red",
    "invalid_acceptance",
    "misleading_docs",
    "resource_exhaustion",
    "silent_wrong_bytes",
    "valid_rejection",
    "wrong_public_api",
  ],
  roadmap: ["matrix", "testing"],
  schema_version: ["1"],
  signal_evaluation: ["met", "stale", "unknown", "unmet"],
  source_kind: ["fragment", "generated_slot", "legacy_marker", "part", "record", "section"],
  spec_legality: ["illegal", "legal"],
  transition_kind: [
    "cadence",
    "promotion_trigger",
    "reopening_signal",
    "retirement_predicate",
    "unblock_predicate",
    "watch_escalation",
  ],
  watch_state: ["attributed", "retire_pending", "watching"],
  work_intent: [
    "add_regression",
    "build_capability",
    "build_system",
    "change_documentation",
    "establish_honest_refusal",
    "optimize",
    "repair",
  ],
  work_kind: [
    "coverage_cell",
    "defect",
    "documentation_integrity",
    "feature",
    "infrastructure",
    "missing_system",
    "optimization",
    "regression_gap",
  ],
  work_state: ["armed", "blocked", "deferred", "delegated", "pending_review", "ready", "waiting_external"],
} as const;

export type AllFieldsCoverageAxis = keyof typeof ALL_FIELDS_EXPECTED_COVERAGE_BY_AXIS;

const codePointSort = (left: string, right: string): number => (left < right ? -1 : left > right ? 1 : 0);

export const ALL_FIELDS_EXPECTED_COVERAGE_AXES = Object.freeze(
  (Object.keys(ALL_FIELDS_EXPECTED_COVERAGE_BY_AXIS) as AllFieldsCoverageAxis[]).sort(codePointSort),
);

export const ALL_FIELDS_EXPECTED_COVERAGE_TAGS = Object.freeze(
  ALL_FIELDS_EXPECTED_COVERAGE_AXES.flatMap((axis) =>
    ALL_FIELDS_EXPECTED_COVERAGE_BY_AXIS[axis].map((value) => `coverage:${axis}:${value}`),
  ),
);

export interface AllFieldsCoverageComparison {
  readonly ok: boolean;
  readonly missing_axes: readonly string[];
  readonly extra_axes: readonly string[];
  readonly missing_tags: readonly string[];
  readonly extra_tags: readonly string[];
  readonly duplicate_tags: readonly string[];
}

export function compareAllFieldsCoverageTags(
  observedTags: readonly string[],
): AllFieldsCoverageComparison {
  const expectedTags = new Set<string>(ALL_FIELDS_EXPECTED_COVERAGE_TAGS);
  const expectedAxes = new Set<string>(ALL_FIELDS_EXPECTED_COVERAGE_AXES);
  const observedCounts = new Map<string, number>();
  const observedAxes = new Set<string>();

  for (const tag of observedTags) {
    observedCounts.set(tag, (observedCounts.get(tag) ?? 0) + 1);
    const match = /^coverage:([^:]+):([^:]+)$/.exec(tag);
    if (match !== null) observedAxes.add(match[1]);
  }

  const observedUnique = new Set(observedCounts.keys());
  const difference = (left: ReadonlySet<string>, right: ReadonlySet<string>): string[] =>
    [...left].filter((value) => !right.has(value)).sort(codePointSort);
  const duplicate_tags = [...observedCounts]
    .filter(([, count]) => count > 1)
    .map(([tag]) => tag)
    .sort(codePointSort);
  const missing_axes = difference(expectedAxes, observedAxes);
  const extra_axes = difference(observedAxes, expectedAxes);
  const missing_tags = difference(expectedTags, observedUnique);
  const extra_tags = difference(observedUnique, expectedTags);

  return {
    ok:
      missing_axes.length === 0 &&
      extra_axes.length === 0 &&
      missing_tags.length === 0 &&
      extra_tags.length === 0 &&
      duplicate_tags.length === 0,
    missing_axes,
    extra_axes,
    missing_tags,
    extra_tags,
    duplicate_tags,
  };
}
