import type {
  SelfTestCandidateCase as SelfTestCase,
  SelfTestContext,
  SelfTestCandidateResult as SelfTestResult,
} from "../selftest.ts";
import type {
  FixtureRelativePath,
  FullCommitId,
  RepoPath,
  ReferenceId,
  RoadmapId,
  RoadmapName,
  WorkKind,
} from "../model/core.ts";
import type {
  CurrentGuard,
  CampaignDocumentV1,
  LegacyMarkdownReservationV1,
  Reference,
  Relation,
  RetiredIdV1,
  RetiredIdsDocumentV1,
  RoadmapDocument,
  RoadmapDocumentV0,
  RoadmapDocumentV1,
  SemanticAuthorityRecordV1,
} from "../model/documents.ts";
import type { RegistryView } from "../adapters/types.ts";
import { RoadmapFailure, type RoadmapIssue } from "../errors.ts";
import { observeMatchingIssue, observeSelfTestIssue } from "./observations.ts";
import { MATRIX_ADAPTER } from "../adapters/matrix.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import {
  ROADMAP_ID_POLICY_V1,
  validateReferenceId,
  validateRoadmapId,
  validateSubordinateId,
} from "../ids.ts";
import {
  buildRoadmapIndexes,
  type RoadmapIndexes,
  type SemanticPayloadProviderFact,
} from "../indexes.ts";
import {
  createCoreReferenceProviders,
  collectReferenceProviders,
  compareReferenceTargets,
  deriveUnresolvedMigrationAuthority,
  extractRustTestSymbols,
  gateFact,
  headingFact,
  REFERENCE_KIND_REGISTRY,
  scanRoadmapCitations,
  scanRoadmapMarkdownFacts,
  validateSemanticRoadmapJoins,
  validateRoadmapReferences,
  type TrackedTextInput,
} from "../references.ts";
import { deriveRelationViews, validateRelations } from "../relations.ts";
import {
  inspectStableEvidenceDigest,
  validateCampaignIdentityOwnerEvidence,
  identityOwnerClaimKey,
  validateGlobalIdentity,
  validateRetiredIdReuse,
  type CampaignIdentityOwnerCapability,
  type CampaignIdentityOwnerEvidence,
} from "../identity.ts";
import {
  campaignIdentityOwners,
  validateBootstrapShadowOwners,
  validateCampaign,
  validateCampaignTransition,
  validateLegacyTitleBinding,
  type CampaignRoadmapSnapshot,
  type CampaignValidationResult,
  type LegacyTitleBindingFact,
} from "../campaign.ts";
import { REPLACEMENT_PIN_KINDS, resolveReplacementPin, validateRetiredIds, validateRetiredTransition } from "../retired.ts";
import {
  TOMBSTONE_ELIGIBLE_BASE_OWNER_KINDS,
  TOMBSTONE_INELIGIBLE_ORIGIN_LABELS,
  validateLifecycleRevision,
  validateTransaction,
} from "../transaction.ts";
import {
  compareMigrationDebt,
  debtOwnerIndex,
  independentDebtIndex,
  validateDebtGuardTransferFacts,
  validateDebtRetirementFacts,
  validateSemanticConversionFacts,
  validateDebtTransitionFacts,
  type MigrationDebt,
} from "../debt.ts";
import {
  createImmutableByteView,
  createExpectedByteView,
  type ExpectedByteViewObserver,
  type CompletedRenderIr,
  type RenderChunk,
} from "../render_ir.ts";

export const IDENTITY_ROADMAP_FIXTURE_PATHS = Object.freeze([
  "all-fields/matrix-v1.toml",
  "all-fields/testing-v1.toml",
  "irregular/matrix-v0.toml",
  "irregular/testing-v0.toml",
  "positive/minimal-matrix-v0.toml",
  "positive/minimal-testing-v0.toml",
  "positive/mixed-matrix-v1.toml",
  "positive/mixed-testing-v1.toml",
] as const);

export type IdentityRoadmapFixturePath =
  (typeof IDENTITY_ROADMAP_FIXTURE_PATHS)[number];

const IDENTITY_FIXTURE_ROOT = "cddl-matrix/roadmap/fixtures" as RepoPath;

export interface IdentityFixtureBundle {
  readonly file_count: 8;
}

const identityFixtureFiles = new WeakMap<object, ReadonlyMap<
  IdentityRoadmapFixturePath,
  Uint8Array
>>();

export function createIdentityFixtureBundle(
  files: ReadonlyMap<IdentityRoadmapFixturePath, Uint8Array>,
): IdentityFixtureBundle {
  assert(
    files.size === IDENTITY_ROADMAP_FIXTURE_PATHS.length,
    "identity fixture bundle must contain exactly eight roadmap files",
  );
  const snapshots = new Map<IdentityRoadmapFixturePath, Uint8Array>();
  for (const path of IDENTITY_ROADMAP_FIXTURE_PATHS) {
    const value = files.get(path);
    assert(value !== undefined && value.byteLength > 0, `identity fixture bundle is missing ${path}`);
    snapshots.set(path, new Uint8Array(value));
  }
  const bundle: IdentityFixtureBundle = Object.freeze({ file_count: 8 });
  identityFixtureFiles.set(bundle, snapshots);
  return bundle;
}

/** Frozen permanent-ID cases. Later join modules append their own block below this one. */
export const PERMANENT_ID_SELFTEST_CASE_IDS = [
  "id_grammar_accept",
  "id_reserved_tokens",
  "id_numeric_legacy_tokens",
  "id_namespace_mismatch",
  "id_is_opaque_to_consumers",
] as const;

export type PermanentIdSelfTestCaseId = (typeof PERMANENT_ID_SELFTEST_CASE_IDS)[number];

export const REQUIRED_IDENTITY_SELFTEST_CASE_IDS = [
  ...PERMANENT_ID_SELFTEST_CASE_IDS,
  "identity_active_duplicate",
  "identity_active_guard_collision",
  "identity_active_tombstone_collision",
  "identity_guard_tombstone_collision",
  "identity_alias_collision",
  "identity_retired_reuse",
  "reference_each_kind",
  "reference_wrong_universe",
  "reference_draft_rejected",
  "reference_draft_log_rejected",
  "reference_gate_stub_rejected",
  "reference_missing_file_heading",
  "relation_missing_endpoint",
  "relation_symmetric_duplicate",
  "cycle_parent",
  "cycle_depends",
  "cycle_supersedes",
  "negative_reference_registry_enumeration",
  "citation_inventory_tracks_source_byte_spans",
  "citation_inventory_sorted_by_path_and_span",
  "citation_inventory_multiple_occurrences",
  "citation_inventory_malformed_id_rejected",
  "citation_inventory_draft_excluded",
  "citation_inventory_nul_binary_excluded",
  "citation_inventory_tracked_missing_rejected",
  "citation_inventory_base_revision_isolated",
  "test_symbol_fact_top_level_direct_test",
  "test_symbol_fact_declared_module_prefix",
  "test_symbol_fact_inline_module_path",
  "test_symbol_fact_attribute_between_test_and_fn",
  "test_symbol_fact_macro_template_excluded",
  "test_symbol_fact_macro_invocation_excluded",
  "test_symbol_fact_comments_and_strings_excluded",
  "test_symbol_fact_undeclared_file_excluded",
  "test_symbol_fact_missing_declared_file_rejected",
  "test_symbol_fact_id_derivation_exact",
  "test_symbol_fact_duplicate_id_rejected",
  "test_symbol_fact_base_revision_isolated",
  "campaign_direct_matrix",
  "campaign_direct_testing",
  "campaign_programmatic_impossible_authority_tuple",
  "campaign_legacy_matrix",
  "campaign_legacy_testing",
  "campaign_legacy_digest_title_span",
  "campaign_legacy_whole_digest",
  "campaign_expire_matrix_at_v1",
  "campaign_expire_testing_at_v1",
  "campaign_testing_survives_matrix_cutover",
  "campaign_direct_requires_active_v1",
  "campaign_active_work_only",
  "campaign_unique_selection",
  "campaign_state_fields",
  "campaign_state_transition",
  "campaign_deselect_active_allowed",
  "campaign_fired_promotion_visible",
  "campaign_allowlist_exhaustive",
  "campaign_allowlist_stale_rejected",
  "retired_bad_hash_length_case",
  "retired_missing_replacement",
  "retired_roadmap_replacement_rejected",
  "retired_unresolved_replacement",
  "retired_gate_stub",
  "retired_preexisting_keeps_base",
  "retired_new_last_active_matches_against",
  "retired_wrong_against",
  "transaction_complete_tombstone",
  "transaction_complete_guard_transfer",
  "transaction_missing_campaign_removal",
  "transaction_live_citation",
  "transaction_dangling_relation",
  "transaction_dangling_reference",
  "transaction_missing_tombstone",
  "transaction_unused_tombstone",
  "transaction_partial_guard",
  "transaction_family_tombstone_rejected",
  "transaction_linked_work_tombstone_required",
  "transaction_duplicate_current_owner",
  "transaction_deselect_active_allowed",
  "transaction_full_hash_git_integration",
  "campaign_reservation_owns_id_without_selection",
  "campaign_deselect_keeps_reservation",
  "campaign_reservation_work_kind_required",
  "campaign_selection_target_kind_matches_owner",
  "campaign_new_legacy_selection_is_atomic",
  "campaign_reservation_rebinds_whole_source",
  "identity_reservation_cross_namespace_collision",
  "identity_reservation_active_collision",
  "identity_reservation_tombstone_collision",
  "identity_shadow_record_reserves_id",
  "identity_reservation_shadow_coalesces",
  "identity_evidence_digest_binary_inputs",
  "identity_reservation_shadow_binding_mismatch",
  "identity_reservation_shadow_third_owner_rejected",
  "transaction_legacy_cutover_transfer_selected",
  "transaction_legacy_cutover_transfer_unselected",
  "transaction_legacy_cutover_work_kind_mismatch",
  "transaction_legacy_delivery_without_shadow",
  "transaction_legacy_delivery_with_shadow",
  "transaction_legacy_delivery_selection_survives",
  "transaction_legacy_delivery_reservation_survives",
  "transaction_legacy_delivery_bound_span_survives",
  "transaction_legacy_delivery_shadow_owner_survives",
  "transaction_legacy_delivery_missing_tombstone",
  "transaction_legacy_delivery_wrong_last_active",
  "transaction_legacy_delivery_live_repo_citation",
  "transaction_shadow_only_delivery_rejected",
  "transaction_single_roadmap_owner_removal_rejected",
  "transaction_citation_in_nonroadmap_file_rejected",
  "retired_test_symbol_requires_exact_id_and_symbol",
  "against_matrix_v1_debt_allowed",
  "against_testing_v1_debt_allowed",
  "against_per_roadmap_does_not_load_other_base",
  "against_per_roadmap_candidate_global_collision_rejected",
  "against_per_roadmap_absent_selected_source_rejected",
  "against_per_roadmap_shadow_selected_source_rejected",
  "against_per_roadmap_owner_change_requires_all",
  "against_forged_debt_transition_facts_ignored",
  "against_all_testing_legacy_absent_valid",
  "against_all_testing_shadow_valid",
  "against_all_testing_authoritative_valid",
  "against_all_state_forbids_unexpected_toml",
  "against_all_state_requires_shadow_toml",
  "against_all_state_requires_authoritative_toml",
  "against_all_base_uses_base_authority_metadata",
  "against_all_wp4m_bootstrap_valid",
  "against_all_post_activation_missing_root_rejected",
  "against_all_shadow_to_authoritative_transfer",
  "against_all_reverse_authority_rejected",
  "transaction_tombstone_eligible_base_owner_set",
  "transaction_tombstone_ineligible_base_owner_rejected",
] as const;

export type RequiredIdentitySelfTestCaseId =
  (typeof REQUIRED_IDENTITY_SELFTEST_CASE_IDS)[number];

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

function bytes(value: string): Uint8Array {
  return new TextEncoder().encode(value);
}

// Keep scanner vectors executable without making this tracked test source cite its own fixture IDs.
const ROADMAP_CITATION_PREFIX_FOR_TESTS = `${"road"}${"map:"}`;
function citationText(suffix: string): string {
  return `${ROADMAP_CITATION_PREFIX_FOR_TESTS}${suffix}`;
}

const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

function suffixed(prefix: string, suffixes: string): string[] {
  return [...suffixes].map((suffix) => `${prefix}${suffix}`);
}

type ExactGroups = Readonly<Record<string, readonly string[]>>;
type ExactCounts = Readonly<Record<string, number>>;

interface FixtureIndexExpectation {
  readonly roadmap: RoadmapName;
  readonly providers: ExactGroups;
  readonly payloads: ExactGroups;
  readonly id_use_roles: ExactCounts;
  readonly semantic_targets: ExactCounts;
  readonly reference_uses: ExactCounts;
  readonly aliases: readonly string[];
  readonly subordinate: ExactGroups;
  readonly references_by_kind: ExactGroups;
  readonly relations: readonly string[];
  readonly relations_by_source: ExactGroups;
  readonly relations_by_target: ExactGroups;
}

const EMPTY_SUBORDINATE: ExactGroups = {
  fragment: [],
  generated_slot: [],
  legacy_marker: [],
  part: [],
  reference: [],
  section: ["fixture"],
  source_span: [],
};

function exactEntries(value: Readonly<Record<string, unknown>>): readonly [string, unknown][] {
  return Object.entries(value).sort(([left], [right]) => codePointSort(left, right));
}

function normalizedGroups(groups: ExactGroups): readonly [string, readonly string[]][] {
  return exactEntries(groups).map(([key, values]) => [
    key,
    [...values as readonly string[]].sort(codePointSort),
  ] as const);
}

function assertExactGroups(actual: ExactGroups, expected: ExactGroups, label: string): void {
  assert(
    JSON.stringify(normalizedGroups(actual)) === JSON.stringify(normalizedGroups(expected)),
    `${label} differs: expected ${JSON.stringify(normalizedGroups(expected))}, got ${JSON.stringify(normalizedGroups(actual))}`,
  );
}

function assertExactCounts(actual: ExactCounts, expected: ExactCounts, label: string): void {
  assert(
    JSON.stringify(exactEntries(actual)) === JSON.stringify(exactEntries(expected)),
    `${label} differs: expected ${JSON.stringify(exactEntries(expected))}, got ${JSON.stringify(exactEntries(actual))}`,
  );
}

function assertExactStrings(
  actual: readonly string[],
  expected: readonly string[],
  label: string,
): void {
  const sortedActual = [...actual].sort(codePointSort);
  const sortedExpected = [...expected].sort(codePointSort);
  assert(
    JSON.stringify(sortedActual) === JSON.stringify(sortedExpected),
    `${label} differs: expected ${JSON.stringify(sortedExpected)}, got ${JSON.stringify(sortedActual)}`,
  );
}

function groupStrings<T>(
  values: readonly T[],
  key: (value: T) => string,
  item: (value: T) => string,
): ExactGroups {
  const groups: Record<string, string[]> = {};
  for (const value of values) (groups[key(value)] ??= []).push(item(value));
  return groups;
}

function countStrings<T>(values: readonly T[], key: (value: T) => string): ExactCounts {
  const counts: Record<string, number> = {};
  for (const value of values) counts[key(value)] = (counts[key(value)] ?? 0) + 1;
  return counts;
}

function groupValues(groups: ExactGroups, key: string): readonly string[] {
  return groups[key] ?? [];
}

function mapGroupValues(
  groups: ExactGroups,
  map: (value: string) => string,
): ExactGroups {
  return Object.fromEntries(exactEntries(groups).map(([key, values]) => [
    key,
    (values as readonly string[]).map(map),
  ]));
}

function nonEmptyGroups(groups: ExactGroups): ExactGroups {
  const result: Record<string, readonly string[]> = {};
  for (const [key, values] of Object.entries(groups)) {
    if (values.length > 0) result[key] = values;
  }
  return result;
}

function selectedPayloadRecordIds(
  expected: FixtureIndexExpectation,
  select: (authority: string, kind: string) => boolean,
): readonly string[] {
  return exactEntries(expected.payloads).flatMap(([key, ids]) => {
    const [authority, kind] = key.split(":");
    return select(authority!, kind!) ? ids as readonly string[] : [];
  });
}

function assertIdentityMap<K extends string, V>(
  actual: ReadonlyMap<K, V>,
  valueId: (value: V) => string,
  expectedIds: readonly string[],
  label: string,
): void {
  assertExactStrings(
    [...actual].map(([key, value]) => `${key}=${valueId(value)}`),
    expectedIds.map((id) => `${id}=${id}`),
    label,
  );
}

function rawFixtureExpectation(
  roadmap: RoadmapName,
  recordId: string,
  slots: readonly string[],
  spans: readonly string[],
): FixtureIndexExpectation {
  return {
    roadmap,
    providers: { record: [recordId] },
    payloads: {},
    id_use_roles: { manifest_record: 1, provider: 1, span_record_owner: 1 },
    semantic_targets: {},
    reference_uses: {},
    aliases: [],
    subordinate: { ...EMPTY_SUBORDINATE, generated_slot: slots, source_span: spans },
    references_by_kind: {},
    relations: [],
    relations_by_source: {},
    relations_by_target: {},
  };
}

function mixedFixtureExpectation(roadmap: RoadmapName): FixtureIndexExpectation {
  const prefix = `${roadmap}.fixture-mixed-`;
  return {
    roadmap,
    providers: { record: [`${prefix}raw`, `${prefix}semantic`] },
    payloads: {
      "semantic:work": [`${prefix}semantic`],
      "semantic_shadow:work": [`${prefix}raw`],
    },
    id_use_roles: { manifest_record: 2, provider: 2, span_record_owner: 3 },
    semantic_targets: {},
    reference_uses: {},
    aliases: [],
    subordinate: {
      ...EMPTY_SUBORDINATE,
      source_span: ["raw-record", "section", "semantic-detail", "semantic-summary"],
    },
    references_by_kind: {},
    relations: [],
    relations_by_source: {},
    relations_by_target: {},
  };
}

const MATRIX_RECORD_IDS = [
  ...suffixed("matrix.fixture-choice-", "abcd"),
  ...suffixed("matrix.fixture-control-", "abcdefg"),
  ...suffixed("matrix.fixture-evidence-", "abcdefghijklm"),
  ...suffixed("matrix.fixture-policy-", "abc"),
  "matrix.fixture-raw-owner",
  "matrix.fixture-semantic-owner",
  ...suffixed("matrix.fixture-signal-", "abcdefghijk"),
  ...suffixed("matrix.fixture-systematic-", "abcd"),
  ...suffixed("matrix.fixture-task-", "abcdefghij"),
  ...suffixed("matrix.fixture-upstream-", "abc"),
];

const MATRIX_RELATION_ROWS = [
  ["matrix.fixture-task-a", "parent_of"],
  ["matrix.fixture-task-b", "depends_on"],
  ["matrix.fixture-task-c", "blocked_by"],
  ["matrix.fixture-task-d", "supersedes"],
  ["matrix.fixture-task-e", "split_from"],
  ["matrix.fixture-task-f", "reopens"],
  ["matrix.fixture-task-g", "overlaps"],
  ["matrix.fixture-task-h", "complements"],
  ["matrix.fixture-task-i", "delegates_to"],
  ["matrix.fixture-task-i", "related"],
] as const;
const MATRIX_RELATION_TARGET = "matrix.fixture-semantic-owner";

const MATRIX_RELATIONS = MATRIX_RELATION_ROWS.map(
  ([source, kind]) => `${source}:${kind}:${MATRIX_RELATION_TARGET}`,
);
const MATRIX_RELATIONS_BY_SOURCE = groupStrings(
  MATRIX_RELATION_ROWS,
  ([source]) => source,
  ([, kind]) => `${kind}:${MATRIX_RELATION_TARGET}`,
);
const MATRIX_RELATIONS_BY_TARGET: ExactGroups = {
  [MATRIX_RELATION_TARGET]: MATRIX_RELATION_ROWS.map(
    ([source, kind]) => `${kind}:${source}`,
  ),
};

const MATRIX_ALL_FIELDS_EXPECTATION: FixtureIndexExpectation = {
  roadmap: "matrix",
  providers: {
    family_axis: suffixed("matrix.fixture-dimension-", "abcd"),
    family_axis_value: [
      "matrix.fixture-option-a-one",
      "matrix.fixture-option-a-two",
      "matrix.fixture-option-b-one",
      "matrix.fixture-option-c-one",
      "matrix.fixture-option-d-one",
    ],
    family_cell: [
      "matrix.fixture-coordinate-a",
      "matrix.fixture-coordinate-b",
      "matrix.fixture-coordinate-bx",
      "matrix.fixture-coordinate-c",
      "matrix.fixture-coordinate-cx",
      "matrix.fixture-coordinate-d",
      "matrix.fixture-coordinate-dx",
    ],
    family_evidence_requirement: suffixed("matrix.fixture-requirement-", "abcd"),
    family_exclusion: suffixed("matrix.fixture-exclusion-", "abcd"),
    record: MATRIX_RECORD_IDS,
  },
  payloads: {
    "semantic:matrix_policy": ["matrix.fixture-semantic-owner"],
    "semantic_shadow:control": suffixed("matrix.fixture-control-", "abcdefg"),
    "semantic_shadow:decision": suffixed("matrix.fixture-choice-", "abcd"),
    "semantic_shadow:evidence": suffixed("matrix.fixture-evidence-", "abcdefghijklm"),
    "semantic_shadow:family": suffixed("matrix.fixture-systematic-", "abcd"),
    "semantic_shadow:matrix_external_closeout": suffixed("matrix.fixture-upstream-", "abc"),
    "semantic_shadow:matrix_policy": suffixed("matrix.fixture-policy-", "abc"),
    "semantic_shadow:signal": suffixed("matrix.fixture-signal-", "abcdefghijk"),
    "semantic_shadow:work": [
      "matrix.fixture-raw-owner",
      ...suffixed("matrix.fixture-task-", "abcdefghij"),
    ],
  },
  id_use_roles: {
    manifest_record: 57,
    parent_record: 1,
    provider: 81,
    reference_source: 13,
    reference_target: 1,
    relation_source: 10,
    relation_target: 10,
    semantic_target: 70,
    span_record_owner: 58,
  },
  semantic_targets: {
    "matrix.fixture-control-a": 6,
    "matrix.fixture-coordinate-a": 1,
    "matrix.fixture-dimension-a": 5,
    "matrix.fixture-dimension-b": 2,
    "matrix.fixture-dimension-c": 2,
    "matrix.fixture-dimension-d": 2,
    "matrix.fixture-evidence-a": 10,
    "matrix.fixture-evidence-b": 3,
    "matrix.fixture-evidence-c": 4,
    "matrix.fixture-evidence-d": 1,
    "matrix.fixture-option-a-one": 3,
    "matrix.fixture-option-a-two": 2,
    "matrix.fixture-option-b-one": 2,
    "matrix.fixture-option-c-one": 2,
    "matrix.fixture-option-d-one": 2,
    "matrix.fixture-signal-b": 1,
    "matrix.fixture-signal-f": 4,
    "matrix.fixture-signal-h": 2,
    "matrix.fixture-signal-j": 4,
    "matrix.fixture-signal-k": 1,
    "matrix.fixture-systematic-a": 1,
    "matrix.fixture-task-a": 8,
    "matrix.fixture-task-b": 2,
  },
  reference_uses: {
    "ref-commit": 2,
    "ref-consumer": 3,
    "ref-feature": 2,
    "ref-file": 24,
    "ref-gate": 10,
    "ref-issue": 9,
    "ref-release": 1,
    "ref-roadmap": 3,
    "ref-spec": 20,
    "ref-test": 2,
  },
  aliases: ["matrix-all-fields-marker=legacy_marker:fixture-marker"],
  subordinate: {
    fragment: ["fixture-fragment"],
    generated_slot: ["constraint", "counts", "emission", "ops"],
    legacy_marker: ["fixture-marker"],
    part: ["fixture-part"],
    reference: [
      "ref-cell",
      "ref-commit",
      "ref-consumer",
      "ref-feature",
      "ref-file",
      "ref-gate",
      "ref-issue",
      "ref-migration",
      "ref-release",
      "ref-roadmap",
      "ref-role",
      "ref-spec",
      "ref-test",
    ],
    section: ["fixture"],
    source_span: [
      "fragment",
      "legacy-marker",
      "part",
      ...suffixed("record-fixture-closeout-", "abc"),
      ...suffixed("record-fixture-control-", "abcdefg"),
      ...suffixed("record-fixture-decision-", "abcd"),
      ...suffixed("record-fixture-evidence-", "abcdefghijklm"),
      ...suffixed("record-fixture-family-", "abcd"),
      ...suffixed("record-fixture-policy-", "abc"),
      "record-fixture-raw-owner",
      ...suffixed("record-fixture-signal-", "abcdefghijk"),
      ...suffixed("record-fixture-work-", "abcdefghij"),
      "section",
      "semantic-detail",
      "semantic-summary",
      "slot-constraint",
      "slot-counts",
      "slot-emission",
      "slot-ops",
    ],
  },
  references_by_kind: {
    consumer_report: ["ref-consumer"],
    external_commit: ["ref-commit"],
    external_issue: ["ref-issue"],
    external_release: ["ref-release"],
    file_heading: ["ref-file"],
    gate: ["ref-gate"],
    matrix_cell: ["ref-cell"],
    matrix_feature: ["ref-feature"],
    matrix_role: ["ref-role"],
    roadmap: ["ref-roadmap"],
    spec_passage: ["ref-spec"],
    test_symbol: ["ref-test"],
    unresolved_migration: ["ref-migration"],
  },
  relations: MATRIX_RELATIONS,
  relations_by_source: MATRIX_RELATIONS_BY_SOURCE,
  relations_by_target: MATRIX_RELATIONS_BY_TARGET,
};

const TESTING_RECORD_IDS = [
  "testing.fixture-admission-bounded",
  "testing.fixture-admission-independent",
  "testing.fixture-admission-silent",
  "testing.fixture-all-fields-raw",
  "testing.fixture-all-fields-semantic",
  "testing.fixture-control-review",
  "testing.fixture-cost-historical",
  "testing.fixture-evidence-gate",
  "testing.fixture-incident-attributed",
  "testing.fixture-incident-historical",
  "testing.fixture-incident-live",
  "testing.fixture-operational-attributed",
  "testing.fixture-operational-retire-pending",
  "testing.fixture-operational-watching",
  "testing.fixture-signal-escalation",
  "testing.fixture-systematic-observed",
  "testing.fixture-task-ready",
];

const TESTING_ALL_FIELDS_EXPECTATION: FixtureIndexExpectation = {
  roadmap: "testing",
  providers: { record: TESTING_RECORD_IDS },
  payloads: {
    "semantic:testing_cost": ["testing.fixture-all-fields-semantic"],
    "semantic_shadow:control": ["testing.fixture-control-review"],
    "semantic_shadow:evidence": ["testing.fixture-evidence-gate"],
    "semantic_shadow:family": ["testing.fixture-systematic-observed"],
    "semantic_shadow:signal": ["testing.fixture-signal-escalation"],
    "semantic_shadow:testing_cost": ["testing.fixture-cost-historical"],
    "semantic_shadow:testing_incident": [
      "testing.fixture-incident-attributed",
      "testing.fixture-incident-historical",
      "testing.fixture-incident-live",
    ],
    "semantic_shadow:testing_operational_watch": [
      "testing.fixture-operational-attributed",
      "testing.fixture-operational-retire-pending",
      "testing.fixture-operational-watching",
    ],
    "semantic_shadow:testing_system_admission": [
      "testing.fixture-admission-bounded",
      "testing.fixture-admission-independent",
      "testing.fixture-admission-silent",
    ],
    "semantic_shadow:work": [
      "testing.fixture-all-fields-raw",
      "testing.fixture-task-ready",
    ],
  },
  id_use_roles: {
    manifest_record: 17,
    provider: 17,
    reference_source: 12,
    semantic_target: 18,
    span_record_owner: 18,
  },
  semantic_targets: {
    "testing.fixture-admission-bounded": 1,
    "testing.fixture-all-fields-semantic": 1,
    "testing.fixture-control-review": 1,
    "testing.fixture-evidence-gate": 7,
    "testing.fixture-incident-attributed": 1,
    "testing.fixture-incident-live": 1,
    "testing.fixture-signal-escalation": 3,
    "testing.fixture-systematic-observed": 2,
    "testing.fixture-task-ready": 1,
  },
  reference_uses: {
    "control-review-heading": 1,
    "evidence-gate-target": 2,
    "incident-attributed-rule": 1,
    "incident-historical-retirement": 1,
    "incident-historical-rule": 1,
    "operational-attributed-rule": 1,
    "operational-retire-pin": 1,
    "operational-retire-rule": 1,
    "semantic-cost-gate": 1,
    "systematic-completion": 1,
    "systematic-observation": 1,
    "systematic-retirement": 1,
  },
  aliases: [],
  subordinate: {
    ...EMPTY_SUBORDINATE,
    reference: [
      "control-review-heading",
      "evidence-gate-target",
      "incident-attributed-rule",
      "incident-historical-retirement",
      "incident-historical-rule",
      "operational-attributed-rule",
      "operational-retire-pin",
      "operational-retire-rule",
      "semantic-cost-gate",
      "systematic-completion",
      "systematic-observation",
      "systematic-retirement",
    ],
    source_span: [
      "section",
      "semantic-detail",
      "semantic-summary",
      "span-admission-bounded",
      "span-admission-independent",
      "span-admission-silent",
      "span-all-fields-raw",
      "span-control-review",
      "span-cost-historical",
      "span-evidence-gate",
      "span-incident-attributed",
      "span-incident-historical",
      "span-incident-live",
      "span-operational-attributed",
      "span-operational-retire-pending",
      "span-operational-watching",
      "span-signal-escalation",
      "span-systematic-observed",
      "span-task-ready",
    ],
  },
  references_by_kind: {
    file_heading: [
      "control-review-heading",
      "incident-attributed-rule",
      "incident-historical-rule",
      "operational-attributed-rule",
      "operational-retire-rule",
      "systematic-completion",
    ],
    gate: [
      "evidence-gate-target",
      "incident-historical-retirement",
      "operational-retire-pin",
      "semantic-cost-gate",
      "systematic-observation",
    ],
    test_symbol: ["systematic-retirement"],
  },
  relations: [],
  relations_by_source: {},
  relations_by_target: {},
};

const EXPECTED_FIXTURE_INDEXES: Readonly<Record<
  IdentityRoadmapFixturePath,
  FixtureIndexExpectation
>> = {
  "all-fields/matrix-v1.toml": MATRIX_ALL_FIELDS_EXPECTATION,
  "all-fields/testing-v1.toml": TESTING_ALL_FIELDS_EXPECTATION,
  "irregular/matrix-v0.toml": rawFixtureExpectation(
    "matrix",
    "matrix.fixture-irregular",
    ["constraint", "counts", "emission", "ops"],
    ["record", "section", "slot-constraint", "slot-counts", "slot-emission", "slot-ops"],
  ),
  "irregular/testing-v0.toml": rawFixtureExpectation(
    "testing",
    "testing.fixture-irregular",
    [],
    ["record", "section"],
  ),
  "positive/minimal-matrix-v0.toml": rawFixtureExpectation(
    "matrix",
    "matrix.fixture-minimal",
    ["counts"],
    ["record", "section", "slot"],
  ),
  "positive/minimal-testing-v0.toml": rawFixtureExpectation(
    "testing",
    "testing.fixture-minimal",
    ["ignored-gates"],
    ["record", "section", "slot"],
  ),
  "positive/mixed-matrix-v1.toml": mixedFixtureExpectation("matrix"),
  "positive/mixed-testing-v1.toml": mixedFixtureExpectation("testing"),
};

function fixtureBytes(
  bundle: IdentityFixtureBundle,
  path: IdentityRoadmapFixturePath,
): Uint8Array {
  const files = identityFixtureFiles.get(bundle);
  assert(files !== undefined, "identity fixture bundle was not created by the fixture boundary");
  const value = files.get(path);
  assert(value !== undefined, `identity fixture bundle is missing ${path}`);
  return value;
}

function relationString(
  relation: RoadmapIndexes["relations"][number],
): string {
  return `${relation.source}:${relation.kind}:${relation.target}`;
}

function aliasStrings(indexes: RoadmapIndexes): readonly string[] {
  return [...indexes.aliases.entries()].flatMap(([alias, providers]) =>
    providers.map((provider) => `${alias}=${provider.owner_kind}:${provider.owner_id}`)
  );
}

function subordinateClaimGroups(indexes: RoadmapIndexes): ExactGroups {
  const groups: Record<string, string[]> = {};
  for (const [kind, claims] of indexes.subordinate_id_claims) {
    groups[kind] = [...claims.keys()];
  }
  return groups;
}

function relationIndexGroups(
  index: RoadmapIndexes["relations_by_source"],
  direction: "source" | "target",
): ExactGroups {
  const groups: Record<string, string[]> = {};
  for (const [id, relations] of index) {
    groups[id] = relations.map((relation) =>
      direction === "source"
        ? `${relation.kind}:${relation.target}`
        : `${relation.kind}:${relation.source}`
    );
  }
  return groups;
}

function assertFixtureIndexes(
  path: IdentityRoadmapFixturePath,
  indexes: RoadmapIndexes,
  expected: FixtureIndexExpectation,
): void {
  const label = `fixture ${path}`;
  assert(indexes.roadmap === expected.roadmap, `${label} roadmap must be ${expected.roadmap}`);
  assertExactGroups(
    groupStrings(indexes.id_providers, (provider) => provider.kind, (provider) => provider.id),
    expected.providers,
    `${label} provider index`,
  );
  assertExactGroups(
    groupStrings(
      [...indexes.id_provider_claims].flatMap(([claimId, providers]) =>
        providers.map((provider) => ({ claimId, provider }))
      ),
      ({ provider }) => provider.kind,
      ({ claimId, provider }) => `${claimId}=${provider.id}`,
    ),
    mapGroupValues(expected.providers, (id) => `${id}=${id}`),
    `${label} provider-claims index`,
  );
  assertExactGroups(
    groupStrings(
      [...indexes.first_class].map(([claimId, provider]) => ({ claimId, provider })),
      ({ provider }) => provider.kind,
      ({ claimId, provider }) => `${claimId}=${provider.id}`,
    ),
    mapGroupValues(expected.providers, (id) => `${id}=${id}`),
    `${label} first-class index`,
  );
  assertIdentityMap(
    indexes.record_nodes,
    (record) => record.id,
    groupValues(expected.providers, "record"),
    `${label} record-node index`,
  );
  assertIdentityMap(
    indexes.records,
    (record) => record.id,
    selectedPayloadRecordIds(expected, (authority) => authority === "semantic"),
    `${label} adapter record index`,
  );
  assertExactGroups(
    groupStrings(
      [...indexes.payload_records.values()],
      (provider) => `${provider.authority}:${provider.payload.kind}`,
      (provider) => provider.record.id,
    ),
    expected.payloads,
    `${label} payload index`,
  );
  assertIdentityMap(
    indexes.evidence_records,
    (provider) => provider.record.id,
    selectedPayloadRecordIds(expected, (_authority, kind) => kind === "evidence"),
    `${label} evidence-record index`,
  );
  assertIdentityMap(
    indexes.family_records,
    (provider) => provider.record.id,
    selectedPayloadRecordIds(expected, (_authority, kind) => kind === "family"),
    `${label} family-record index`,
  );
  assertExactCounts(
    countStrings(indexes.id_uses, (use) => use.role),
    expected.id_use_roles,
    `${label} roadmap-ID use roles`,
  );
  assertExactCounts(
    countStrings(
      indexes.id_uses.filter((use) => use.role === "semantic_target"),
      (use) => use.id,
    ),
    expected.semantic_targets,
    `${label} semantic targets`,
  );
  assertExactCounts(
    countStrings(indexes.reference_id_uses, (use) => use.id),
    expected.reference_uses,
    `${label} reference-ID uses`,
  );
  assertExactStrings(
    indexes.alias_providers.map((provider) =>
      `${provider.alias}=${provider.owner_kind}:${provider.owner_id}`
    ),
    expected.aliases,
    `${label} alias-provider index`,
  );
  assertExactStrings(aliasStrings(indexes), expected.aliases, `${label} alias index`);
  assertExactGroups(
    groupStrings(
      indexes.subordinate_id_providers,
      (provider) => provider.kind,
      (provider) => provider.id,
    ),
    nonEmptyGroups(expected.subordinate),
    `${label} subordinate-provider index`,
  );
  assertExactGroups(
    subordinateClaimGroups(indexes),
    expected.subordinate,
    `${label} subordinate claims`,
  );
  assertIdentityMap(
    indexes.sections,
    (section) => section.section_id,
    groupValues(expected.subordinate, "section"),
    `${label} section index`,
  );
  assertIdentityMap(
    indexes.fragments,
    (fragment) => fragment.fragment_id,
    groupValues(expected.subordinate, "fragment"),
    `${label} fragment index`,
  );
  assertIdentityMap(
    indexes.legacy_markers,
    (marker) => marker.marker_id,
    groupValues(expected.subordinate, "legacy_marker"),
    `${label} legacy-marker index`,
  );
  assertIdentityMap(
    indexes.parts,
    (part) => part.part_id,
    groupValues(expected.subordinate, "part"),
    `${label} part index`,
  );
  assertIdentityMap(
    indexes.generated_slots,
    (slot) => slot.slot_id,
    groupValues(expected.subordinate, "generated_slot"),
    `${label} generated-slot index`,
  );
  assertIdentityMap(
    indexes.spans,
    (span) => span.id,
    groupValues(expected.subordinate, "source_span"),
    `${label} source-span index`,
  );
  assertExactGroups(
    groupStrings(
      [...indexes.references.values()],
      (reference) => reference.kind,
      (reference) => reference.id,
    ),
    expected.references_by_kind,
    `${label} reference index`,
  );
  assertExactStrings(
    indexes.relations.map(relationString),
    expected.relations,
    `${label} relations`,
  );
  assertExactGroups(
    relationIndexGroups(indexes.relations_by_source, "source"),
    expected.relations_by_source,
    `${label} relations by source`,
  );
  assertExactGroups(
    relationIndexGroups(indexes.relations_by_target, "target"),
    expected.relations_by_target,
    `${label} relations by target`,
  );
  assert(indexes.identity_inputs.namespace === expected.roadmap, `${label} identity-input namespace`);
  assertExactGroups(
    groupStrings(
      indexes.identity_inputs.id_providers,
      (provider) => provider.kind,
      (provider) => provider.id,
    ),
    expected.providers,
    `${label} identity-input providers`,
  );
  assertExactStrings(
    indexes.identity_inputs.alias_providers.map((provider) =>
      `${provider.alias}=${provider.owner_kind}:${provider.owner_id}`
    ),
    expected.aliases,
    `${label} identity-input aliases`,
  );
}

function assertCommittedFixtureIndexes(bundle: IdentityFixtureBundle): void {
  const globalInputs: RoadmapIndexes["identity_inputs"][] = [];
  let committedReferenceTemplates: readonly Reference[] | undefined;
  for (const path of IDENTITY_ROADMAP_FIXTURE_PATHS) {
    const expected = EXPECTED_FIXTURE_INDEXES[path];
    const document = decodeRoadmapSource(fixtureBytes(bundle, path), path, expected.roadmap);
    const result = buildRoadmapIndexes(document);
    assert(
      result.issues.length === 0,
      `${path} must index without issues, got ${JSON.stringify(result.issues)}`,
    );
    assertFixtureIndexes(path, result.indexes, expected);
    globalInputs.push(result.indexes.identity_inputs);
    const references = [...result.indexes.references.values()].sort(compareReferenceTargets);
    if (path === "all-fields/matrix-v1.toml") {
      assert(
        new Set(references.map((reference) => reference.kind)).size === REFERENCE_KIND_REGISTRY.length,
        "matrix all-fields must provide one committed template for every Reference kind",
      );
      committedReferenceTemplates = Object.freeze([...references]);
    }
    const registry = registryViewForReferences(references);
    const unresolvedAuthority = deriveUnresolvedMigrationAuthority(result.indexes, path);
    assert(
      unresolvedAuthority.issues.length === 0,
      `${path} unresolved-migration authority derivation must succeed: ${JSON.stringify(unresolvedAuthority.issues)}`,
    );
    assert(
      unresolvedAuthority.debt.length === (path === "all-fields/matrix-v1.toml" ? 1 : 0),
      `${path} unresolved-migration debt enumeration must be exact`,
    );
    assert(
      validateRoadmapReferences(result.indexes, registry, {
        source: path,
        providers: MATRIX_ADAPTER.referenceProviders(registry),
        unresolved_migration_authority: unresolvedAuthority.authority,
      }).length === 0,
      `${path} references must resolve through the actual adapter providers and matching repository facts`,
    );
    assert(
      validateSemanticRoadmapJoins(result.indexes, result.indexes, path).length === 0,
      `${path} semantic roadmap-ID joins must resolve to their exact typed providers`,
    );
    assert(
      validateRelations(result.indexes.relations, result.indexes.first_class, path).length === 0,
      `${path} relations must validate against the committed first-class universe`,
    );
    if (path.startsWith("all-fields/")) {
      assert(committedReferenceTemplates !== undefined, "matrix reference templates must precede all field-use mutations");
      const candidates = committedReferenceTemplates
        .filter((reference) => reference.kind !== "unresolved_migration")
        .sort((left, right) => codePointSort(left.kind, right.kind));
      let mutationCount = 0;
      for (const use of result.indexes.reference_id_uses) {
        const original = result.indexes.references.get(use.id);
        assert(original !== undefined, `${path} use ${use.logical_path} must resolve before mutation`);
        let rejectedKind: Reference["kind"] | undefined;
        for (const template of candidates) {
          if (template.kind === original.kind) continue;
          const replacement = {
            ...template,
            id: original.id,
            source: original.source,
          } as Reference;
          const mutatedReferences = new Map(result.indexes.references);
          mutatedReferences.set(use.id, replacement);
          const mutated = { ...result.indexes, references: mutatedReferences } as RoadmapIndexes;
          const mutatedRegistry = registryViewForReferences([...mutatedReferences.values()]);
          const adapterProviders = MATRIX_ADAPTER.referenceProviders(mutatedRegistry);
          const providerRegistry = collectReferenceProviders(
            mutated.first_class,
            adapterProviders,
            unresolvedAuthority.authority,
            path,
          );
          assert(providerRegistry.issues.length === 0, `${path} mutation provider composition must remain valid`);
          const selectedProvider = providerRegistry.by_kind.get(replacement.kind);
          assert(selectedProvider !== undefined, `${path} replacement kind ${replacement.kind} must have a provider`);
          if (!selectedProvider.resolve(replacement as never, mutatedRegistry).resolved) continue;
          const mutationIssues = validateRoadmapReferences(mutated, mutatedRegistry, {
            source: path,
            providers: adapterProviders,
            unresolved_migration_authority: unresolvedAuthority.authority,
          });
          if (mutationIssues.some((value) =>
            value.code === "E-REFERENCE-FORBIDDEN" && value.logical_path === use.logical_path
          )) {
            rejectedKind = template.kind;
            break;
          }
        }
        assert(
          rejectedKind !== undefined,
          `${path} ReferenceId use ${use.logical_path} has no closed wrong-kind rejection policy`,
        );
        mutationCount += 1;
      }
      assert(
        mutationCount === result.indexes.reference_id_uses.length,
        `${path} must fault-inject every collected ReferenceId use`,
      );
    }
    const semanticUse = result.indexes.id_uses.find((use) => use.role === "semantic_target");
    if (semanticUse !== undefined) {
      const target = result.indexes.first_class.get(semanticUse.id)!;
      const wrongKind = new Map(result.indexes.first_class);
      wrongKind.set(semanticUse.id, {
        ...target,
        kind: target.kind === "record" ? "family_axis" : "record",
      } as typeof target);
      assert(
        validateSemanticRoadmapJoins(result.indexes, {
          first_class: wrongKind,
          payload_records: result.indexes.payload_records,
        }, path).some((value) => value.code === "E-REFERENCE-FORBIDDEN"),
        `${path} semantic target wrong-universe mutation must fail`,
      );
      const missing = new Map(result.indexes.first_class);
      missing.delete(semanticUse.id);
      assert(
        validateSemanticRoadmapJoins(result.indexes, {
          first_class: missing,
          payload_records: result.indexes.payload_records,
        }, path).some((value) => value.code === "E-REFERENCE-UNRESOLVED"),
        `${path} semantic target missing-provider mutation must fail`,
      );
    }
    if (path === "all-fields/matrix-v1.toml") {
      const mutateSemanticTarget = (
        pathNeedle: string,
        mutate: (payload: SemanticPayloadProviderFact["payload"]) => unknown,
      ): ReturnType<typeof validateSemanticRoadmapJoins> => {
        const use = result.indexes.id_uses.find((candidate) =>
          candidate.role === "semantic_target" && candidate.logical_path.includes(pathNeedle)
        );
        assert(use !== undefined, `${path} must contain semantic use ${pathNeedle}`);
        const target = result.indexes.first_class.get(use.id);
        assert(target !== undefined, `${path} semantic mutation target must be first-class`);
        const provider = result.indexes.payload_records.get(target.owner_record_id);
        assert(provider !== undefined, `${path} semantic mutation target must own a payload`);
        const payloadRecords = new Map(result.indexes.payload_records);
        payloadRecords.set(target.owner_record_id, {
          ...provider,
          payload: mutate(provider.payload) as typeof provider.payload,
        });
        return validateSemanticRoadmapJoins(result.indexes, {
          first_class: result.indexes.first_class,
          payload_records: payloadRecords,
        }, path);
      };
      assert(
        mutateSemanticTarget("fixture-task-e\"].semantic_shadow.transition_ids", (payload) => ({
          ...payload,
          transition_kind: "cadence",
        })).some((value) => value.code === "E-REFERENCE-FORBIDDEN" && value.logical_path.includes("fixture-task-e")),
        "blocked work must reject a cadence target in place of its unblock predicate",
      );
      assert(
        mutateSemanticTarget("fixture-policy-a\"].semantic_shadow.cadence_transition_id", (payload) => ({
          ...payload,
          transition_kind: "reopening_signal",
        })).some((value) => value.code === "E-REFERENCE-FORBIDDEN" && value.logical_path.endsWith("cadence_transition_id")),
        "maintenance policy cadence must reject a reopening signal",
      );
      assert(
        mutateSemanticTarget("fixture-policy-c\"].semantic_shadow.reopening_transition_id", (payload) => ({
          ...payload,
          transition_kind: "cadence",
        })).some((value) => value.code === "E-REFERENCE-FORBIDDEN" && value.logical_path.endsWith("reopening_transition_id")),
        "reopenable policy must reject a cadence signal",
      );
      assert(
        mutateSemanticTarget("fixture-task-f\"].semantic_shadow.control_ids", (payload) => ({
          ...payload,
          control_state: "planned",
        })).some((value) => value.code === "E-REFERENCE-FORBIDDEN" && value.logical_path.includes("fixture-task-f")),
        "armed work must resolve only to live controls",
      );
      const withoutDelegation = {
        ...result.indexes,
        relations: result.indexes.relations.filter((relation) => relation.kind !== "delegates_to"),
      } as RoadmapIndexes;
      assert(
        validateSemanticRoadmapJoins(withoutDelegation, withoutDelegation, path).some((value) =>
          value.code === "E-REFERENCE-FORBIDDEN" && value.logical_path.endsWith("delegates_to")
        ),
        "delegated work must reject a missing delegates_to relation",
      );
      const delegation = result.indexes.relations.find((relation) => relation.kind === "delegates_to");
      assert(delegation !== undefined, "matrix all-fields must contain a delegates_to relation");
      const duplicateDelegation = {
        ...result.indexes,
        relations: Object.freeze([...result.indexes.relations, delegation]),
      } as RoadmapIndexes;
      assert(
        validateSemanticRoadmapJoins(duplicateDelegation, duplicateDelegation, path).some((value) =>
          value.code === "E-REFERENCE-FORBIDDEN" && value.logical_path.endsWith("delegates_to")
        ),
        "delegated work must reject duplicate delegates_to relations",
      );

      const withoutShadowAuthority = {
        ...result.indexes,
        payload_records: new Map([...result.indexes.payload_records].map(([id, provider]) => [
          id,
          { ...provider, authority: "semantic" as const },
        ])),
      } as RoadmapIndexes;
      const rejectedAuthority = deriveUnresolvedMigrationAuthority(withoutShadowAuthority, path);
      assert(rejectedAuthority.authority === undefined, "semantic-only payloads must not mint unresolved-migration authority");
      assert(
        rejectedAuthority.issues.some((value) => value.code === "E-REFERENCE-FORBIDDEN") &&
          rejectedAuthority.debt.length === 1,
        "unresolved migration debt must remain observable when authority derivation fails",
      );
      assert(
        validateRoadmapReferences(withoutShadowAuthority, registry, {
          source: path,
          providers: MATRIX_ADAPTER.referenceProviders(registry),
        }).some((value) =>
          value.code === "E-REFERENCE-FORBIDDEN" && value.logical_path === "reference[\"ref-migration\"]"
        ),
        "unresolved migration must fail without structurally derived shadow authority",
      );
    }
  }
  const global = validateGlobalIdentity({ documents: globalInputs });
  assert(global.issues.length === 0, `committed roadmap fixtures must share one collision-free global identity domain: ${JSON.stringify(global.issues)}`);
  const duplicate = validateGlobalIdentity({ documents: [...globalInputs, globalInputs[0]!] });
  assert(duplicate.issues.some((value) => value.code === "E-OWNER-DUPLICATE"), "duplicating one committed document identity view must fail globally");
}

function identityFixtureBundleFromContext(context: SelfTestContext): IdentityFixtureBundle {
  const inventory = context.ports.fixtures.enumerateFixtureFiles(IDENTITY_FIXTURE_ROOT);
  const sortedInventory = [...inventory].sort(codePointSort);
  assert(
    JSON.stringify(inventory) === JSON.stringify(sortedInventory),
    "identity fixture inventory must be in code-point order",
  );
  assert(
    new Set(inventory).size === inventory.length,
    "identity fixture inventory must not contain duplicate paths",
  );

  const selectedInventory = inventory.filter((candidate) =>
    IDENTITY_ROADMAP_FIXTURE_PATHS.some((path) => path === candidate)
  );
  assert(
    JSON.stringify(selectedInventory) === JSON.stringify(IDENTITY_ROADMAP_FIXTURE_PATHS),
    `identity roadmap fixture inventory differs: expected ${JSON.stringify(IDENTITY_ROADMAP_FIXTURE_PATHS)}, got ${JSON.stringify(selectedInventory)}`,
  );
  const authorized = new Map<IdentityRoadmapFixturePath, FixtureRelativePath>();
  for (const path of IDENTITY_ROADMAP_FIXTURE_PATHS) {
    const inventoryPath = selectedInventory.find((candidate) => candidate === path);
    assert(inventoryPath !== undefined, `identity fixture inventory is missing ${path}`);
    authorized.set(path, inventoryPath);
  }

  return createIdentityFixtureBundle(new Map([...authorized].map(([path, inventoryPath]) => [
    path,
    context.ports.fixtures.readFixtureFile(IDENTITY_FIXTURE_ROOT, inventoryPath),
  ])));
}

function decodedDocument(
  recordIds: readonly string[],
  roadmap: RoadmapName = "matrix",
): RoadmapDocument {
  const sourceLength = recordIds.length + 1;
  const records = recordIds.map((id, index) => `
[[record]]
id = "${id}"
title = "Record ${index}"
projection_group = "fixture"
legacy_aliases = ["Legacy ${String.fromCharCode(90 - index)}"]
source_block_md = """R"""
span_ids = ["span-record-${String.fromCharCode(97 + index)}"]
`).join("");
  const manifest = recordIds.map((id) => `
[[manifest.entry]]
kind = "record"
record_id = "${id}"
`).join("");
  const spans = recordIds.map((id, index) => `
[[source_span]]
id = "span-record-${String.fromCharCode(97 + index)}"
start_byte = ${index + 1}
end_byte = ${index + 2}
sha256 = "${"0".repeat(64)}"
source_kind = "record"
owner_id = "${id}"
owner_field = "source_block_md"
migration_status = "raw"
`).join("");
  return decodeRoadmapSource(bytes(`[document]
schema_version = 0
authority = "shadow"
roadmap = "${roadmap}"
source_path = "cddl-matrix/ROADMAP.md"
projection_path = "cddl-matrix/ROADMAP.md"
frozen_source_sha256 = "${"0".repeat(64)}"
frozen_source_byte_length = ${sourceLength}
frozen_source_line_count = 1
frozen_source_eof = "none"

[[section]]
section_id = "fixture"
title = "Fixture"
legacy_aliases = ["Legacy Section"]
source_block_md = """S"""
span_ids = ["span-section"]
${records}
[manifest]
[[manifest.entry]]
kind = "section"
section_id = "fixture"
${manifest}
[[source_span]]
id = "span-section"
start_byte = 0
end_byte = 1
sha256 = "${"0".repeat(64)}"
source_kind = "section"
owner_id = "fixture"
owner_field = "source_block_md"
migration_status = "raw"
${spans}`), "<identity-selftest>", roadmap, false);
}

function requireAccepted(value: string, namespace?: RoadmapName): RoadmapId {
  const result = validateRoadmapId(value, namespace);
  assert(result.ok, `${value} should be accepted, got ${result.ok ? "success" : result.code}`);
  return result.id;
}

function requireRejected(
  value: string,
  code: "E-ID-GRAMMAR" | "E-ID-RESERVED" | "E-ID-NAMESPACE",
  namespace?: RoadmapName,
): void {
  const result = validateRoadmapId(value, namespace);
  assert(!result.ok && result.code === code, `${value} should fail with ${code}`);
  observeSelfTestIssue({ code, logical_path: value });
}

type JoinSelfTestCaseId = Exclude<
  RequiredIdentitySelfTestCaseId,
  PermanentIdSelfTestCaseId
>;

function brandedRoadmapId(value: string): RoadmapId {
  return requireAccepted(value);
}

function fakeRegistryView(overrides: Partial<RegistryView> = {}): RegistryView {
  return {
    revision: { kind: "worktree" },
    production_output_stage: "pre_cutover",
    gates: [gateFact("roadmap_projection_check")],
    matrix_features: [{ id: "type2.value" }],
    matrix_roles: [{ id: "role.top-level" }],
    matrix_cells: [{ id: "contain.map-value.type.choice" }],
    tracked_headings: [headingFact("cddl-matrix/README.md" as RepoPath, "What lives here")],
    test_symbols: [{
      test_id: "rust-test:cddl-codegen#tests::sample::works",
      symbol: "tests::sample::works",
      source: "src/tests/sample.rs" as RepoPath,
      span: { start_byte: 12, end_byte: 17 },
      module_path: ["tests", "sample"],
    }],
    roadmap_citations: [],
    current_guards: [],
    output_claims: [],
    matrix_status_inputs: {} as RegistryView["matrix_status_inputs"],
    ...overrides,
  };
}

function uniqueBy<T>(values: readonly T[], key: (value: T) => string): readonly T[] {
  const byKey = new Map<string, T>();
  for (const value of values) byKey.set(key(value), value);
  return Object.freeze([...byKey].sort(([left], [right]) => codePointSort(left, right)).map(
    ([, value]) => value,
  ));
}

function registryViewForReferences(references: readonly Reference[]): RegistryView {
  const sorted = [...references].sort(compareReferenceTargets);
  return fakeRegistryView({
    gates: uniqueBy(
      sorted.filter((reference): reference is Extract<Reference, { kind: "gate" }> =>
        reference.kind === "gate"
      ).map((reference) => gateFact(reference.gate_id)),
      (fact) => fact.id,
    ),
    matrix_features: uniqueBy(
      sorted.filter((reference): reference is Extract<Reference, { kind: "matrix_feature" }> =>
        reference.kind === "matrix_feature"
      ).map((reference) => ({ id: reference.feature_id })),
      (fact) => fact.id,
    ),
    matrix_roles: uniqueBy(
      sorted.filter((reference): reference is Extract<Reference, { kind: "matrix_role" }> =>
        reference.kind === "matrix_role"
      ).map((reference) => ({ id: reference.role_id })),
      (fact) => fact.id,
    ),
    matrix_cells: uniqueBy(
      sorted.filter((reference): reference is Extract<Reference, { kind: "matrix_cell" }> =>
        reference.kind === "matrix_cell"
      ).map((reference) => ({ id: reference.cell_id })),
      (fact) => fact.id,
    ),
    tracked_headings: uniqueBy(
      sorted.filter((reference): reference is Extract<Reference, { kind: "file_heading" }> =>
        reference.kind === "file_heading"
      ).map((reference) => headingFact(reference.path, reference.heading)),
      (fact) => JSON.stringify([fact.path, fact.heading]),
    ),
    test_symbols: uniqueBy(
      sorted.filter((reference): reference is Extract<Reference, { kind: "test_symbol" }> =>
        reference.kind === "test_symbol"
      ).map((reference) => ({
        test_id: reference.test_id,
        symbol: reference.symbol,
        source: "src/tests/fixture.rs" as RepoPath,
        span: { start_byte: 0, end_byte: 1 },
        module_path: Object.freeze(reference.symbol.split("::").slice(0, -1)),
      })),
      (fact) => JSON.stringify([fact.test_id, fact.symbol]),
    ),
  });
}

function firstClassFor(...ids: string[]): RoadmapIndexes["first_class"] {
  return new Map(ids.map((id) => {
    const roadmapId = brandedRoadmapId(id);
    return [roadmapId, {
      id: roadmapId,
      namespace: id.startsWith("matrix.") ? "matrix" : "testing",
      kind: "record",
      owner_record_id: roadmapId,
      logical_path: `record[${JSON.stringify(id)}]`,
      value: {} as RoadmapIndexes["id_providers"][number]["value"],
    }] as const;
  }));
}

function sampleReferences(source: RoadmapId): readonly Reference[] {
  const ref = (id: string): ReferenceId => id as ReferenceId;
  return [
    { id: ref("roadmap"), source, kind: "roadmap", target_id: source },
    { id: ref("feature"), source, kind: "matrix_feature", feature_id: "type2.value" },
    { id: ref("role"), source, kind: "matrix_role", role_id: "role.top-level" },
    { id: ref("cell"), source, kind: "matrix_cell", cell_id: "contain.map-value.type.choice" },
    { id: ref("gate"), source, kind: "gate", gate_id: "roadmap_projection_check" },
    {
      id: ref("test"), source, kind: "test_symbol",
      test_id: "rust-test:cddl-codegen#tests::sample::works",
      symbol: "tests::sample::works",
    },
    {
      id: ref("heading"), source, kind: "file_heading",
      path: "cddl-matrix/README.md" as RepoPath, heading: "What lives here",
    },
    { id: ref("spec"), source, kind: "spec_passage", document: "RFC 8610", passage: "Section 3" },
    { id: ref("issue"), source, kind: "external_issue", repository: "example/upstream", issue: "17" },
    {
      id: ref("commit"), source, kind: "external_commit", repository: "example/upstream",
      commit: "0123456789abcdef0123456789abcdef01234567",
    },
    { id: ref("release"), source, kind: "external_release", project: "upstream", release: "v1" },
    { id: ref("consumer"), source, kind: "consumer_report", consumer: "cml", report_reference: "r1" },
    {
      id: ref("migration"), source, kind: "unresolved_migration", local_reference: "legacy line",
      uncertainty_md: bytes("unknown"), expires_at: "cutover",
    },
  ];
}

function referenceProviderCase(id: JoinSelfTestCaseId): boolean {
  if (!id.startsWith("reference_") && id !== "negative_reference_registry_enumeration") return false;
  const source = brandedRoadmapId("matrix.fixture-source");
  const consumer = brandedRoadmapId("matrix.fixture-consumer");
  const view = fakeRegistryView();
  const base = buildRoadmapIndexes(decodedDocument([source, consumer])).indexes;
  const allReferences = sampleReferences(source);
  const gateReference = allReferences.find((reference) => reference.kind === "gate")!;
  const joined = {
    ...base,
    references: new Map(allReferences.map((reference) => [reference.id, reference])),
    reference_id_uses: [{
      id: gateReference.id,
      logical_path: `record[${JSON.stringify(consumer)}].semantic_shadow.reference_ids`,
    }],
    payload_records: new Map([[consumer, {
      record: base.record_nodes.get(consumer)!,
      payload: {
        kind: "control",
        summary_md: bytes("shared reference consumer\n"),
        control_kind: "gate",
        control_state: "live",
        reference_ids: [gateReference.id],
        claim_md: bytes("claim\n"),
        boundary_md: bytes("boundary\n"),
      },
      authority: "semantic_shadow",
      logical_path: `record[${JSON.stringify(consumer)}].semantic_shadow`,
    }]]),
  } as RoadmapIndexes;
  const unresolvedAuthority = deriveUnresolvedMigrationAuthority(joined);
  assert(
    unresolvedAuthority.issues.length === 0 && unresolvedAuthority.authority !== undefined &&
      unresolvedAuthority.debt.length === 1,
    "synthetic semantic-shadow debt must derive non-mintable unresolved authority",
  );
  const providerClaims = Object.freeze([
    ...createCoreReferenceProviders(joined.first_class, unresolvedAuthority.authority),
    ...MATRIX_ADAPTER.referenceProviders(view),
  ].sort((left, right) => codePointSort(left.kind, right.kind)));
  const providers = collectReferenceProviders(providerClaims);
  assert(providers.issues.length === 0, "actual core and matrix adapter providers must compose exactly once");
  const resolve = (reference: Reference): boolean => {
    const provider = providers.by_kind.get(reference.kind);
    assert(provider !== undefined, `provider for ${reference.kind} must exist`);
    return provider.resolve(reference as never, view).resolved;
  };
  const reject = (reference: Reference, code: import("../errors.ts").IssueCode, logicalPath: string): void => {
    const provider = providers.by_kind.get(reference.kind);
    assert(provider !== undefined, `provider for ${reference.kind} must exist`);
    const resolution = provider.resolve(reference as never, view);
    assert(!resolution.resolved, `${reference.kind} negative reference unexpectedly resolved`);
    observeSelfTestIssue({ code, logical_path: logicalPath });
  };
  switch (id) {
    case "reference_each_kind":
      for (const reference of sampleReferences(source)) {
        assert(resolve(reference), `${reference.kind} sample should resolve`);
      }
      assert(
        validateRoadmapReferences(joined, view, {
          providers: MATRIX_ADAPTER.referenceProviders(view),
          unresolved_migration_authority: unresolvedAuthority.authority,
        }).length === 0,
        "a typed semantic ReferenceId may consume another record's shared declaration",
      );
      const missingUse = {
        ...joined,
        reference_id_uses: [{ id: "missing" as ReferenceId, logical_path: "record.missing" }],
      } as RoadmapIndexes;
      assert(
        validateRoadmapReferences(missingUse, view, {
          providers: MATRIX_ADAPTER.referenceProviders(view),
          unresolved_migration_authority: unresolvedAuthority.authority,
        }).some((value) => value.code === "E-REFERENCE-UNRESOLVED"),
        "well-formed missing ReferenceId uses must be retained until join validation",
      );
      const missingUses = [
        { id: "missing-z" as ReferenceId, logical_path: "record.z" },
        { id: "missing-a" as ReferenceId, logical_path: "record.a" },
      ];
      const referenceFaultSignature = (uses: typeof missingUses): string => JSON.stringify(
        validateRoadmapReferences({ ...joined, reference_id_uses: uses } as RoadmapIndexes, view, {
          providers: MATRIX_ADAPTER.referenceProviders(view),
          unresolved_migration_authority: unresolvedAuthority.authority,
        }),
      );
      assert(
        referenceFaultSignature(missingUses) === referenceFaultSignature([...missingUses].reverse()),
        "reference diagnostics must be deterministic under use-list reversal",
      );
      assert(
        compareReferenceTargets({
          id: "tuple-a" as ReferenceId,
          source,
          kind: "file_heading",
          path: "a" as RepoPath,
          heading: "z",
        }, {
          id: "tuple-b" as ReferenceId,
          source,
          kind: "file_heading",
          path: "aa" as RepoPath,
          heading: "a",
        }) < 0,
        "reference target tuples must compare element by element",
      );
      return true;
    case "reference_wrong_universe":
      reject({ id: "wrong" as ReferenceId, source, kind: "matrix_feature", feature_id: "role.top-level" }, "E-REFERENCE-UNRESOLVED", "reference.matrix_feature");
      return true;
    case "reference_draft_rejected":
      reject({ id: "draft" as ReferenceId, source, kind: "file_heading", path: "draft/note.md" as RepoPath, heading: "Note" }, "E-REFERENCE-FORBIDDEN", "reference.file_heading");
      return true;
    case "reference_draft_log_rejected":
      reject({ id: "draft-log" as ReferenceId, source, kind: "file_heading", path: "draft/logs/run.log" as RepoPath, heading: "Run" }, "E-REFERENCE-FORBIDDEN", "reference.file_heading");
      return true;
    case "reference_gate_stub_rejected": {
      const stubView = fakeRegistryView({ gates: [gateFact("roadmap_projection_check", true)] });
      const provider = providers.by_kind.get("gate")!;
      const resolution = provider.resolve({ id: "stub" as ReferenceId, source, kind: "gate", gate_id: "roadmap_projection_check" } as never, stubView);
      assert(!resolution.resolved, "stub gate must not resolve");
      observeSelfTestIssue({ code: "E-REFERENCE-STUB", logical_path: "reference.gate" });
      return true;
    }
    case "reference_missing_file_heading":
      reject({ id: "missing" as ReferenceId, source, kind: "file_heading", path: "README.md" as RepoPath, heading: "Missing" }, "E-REFERENCE-UNRESOLVED", "reference.file_heading");
      return true;
    case "negative_reference_registry_enumeration":
      assertExactStrings([...providers.by_kind.keys()], REFERENCE_KIND_REGISTRY, "reference provider registry");
      for (const kind of REFERENCE_KIND_REGISTRY) {
        const selected = providerClaims.find((provider) => provider.kind === kind)!;
        const missing = collectReferenceProviders(providerClaims.filter((provider) => provider !== selected));
        assert(
          observeMatchingIssue(missing.issues, "E-REFERENCE-UNRESOLVED", `reference-provider.${kind}`) !== undefined,
          `actual provider collector must reject a missing ${kind} provider`,
        );
        const duplicate = collectReferenceProviders([...providerClaims, selected]);
        assert(
          observeMatchingIssue(duplicate.issues, "E-REFERENCE-UNRESOLVED", `reference-provider.${kind}`) !== undefined,
          `actual provider collector must reject a duplicate ${kind} provider`,
        );
      }
      return true;
  }
  return false;
}

function relationCase(id: JoinSelfTestCaseId): boolean {
  const applicable = new Set<JoinSelfTestCaseId>([
    "relation_missing_endpoint", "relation_symmetric_duplicate", "cycle_parent",
    "cycle_depends", "cycle_supersedes",
  ]);
  if (!applicable.has(id)) return false;
  const a = brandedRoadmapId("matrix.fixture-alpha");
  const b = brandedRoadmapId("matrix.fixture-beta");
  const c = brandedRoadmapId("matrix.fixture-gamma");
  const firstClass = firstClassFor(a, b, c);
  if (id === "relation_missing_endpoint") {
    const missing = brandedRoadmapId("matrix.fixture-missing");
    assert(observeMatchingIssue(validateRelations([{ source: a, kind: "related", target: missing }], firstClass), "E-RELATION-ENDPOINT") !== undefined, "missing endpoint must fail");
    return true;
  }
  if (id === "relation_symmetric_duplicate") {
    for (const kind of ["overlaps", "complements", "related"] as const) {
      const authored: Relation = { source: a, kind, target: b };
      const views = deriveRelationViews([authored]);
      assert(views.length === 2, `${kind} must derive exactly two directional views`);
      assert(
        views.some((view) =>
          view.source === a && view.target === b && view.kind === kind &&
          view.direction === "forward" && view.authored === authored
        ),
        `${kind} forward view must preserve exact authored identity and content`,
      );
      assert(
        views.some((view) =>
          view.source === b && view.target === a && view.kind === kind &&
          view.direction === "inverse" && view.authored === authored
        ),
        `${kind} inverse view must reverse only its endpoints`,
      );
      assert(
        observeMatchingIssue(validateRelations([authored, { source: b, kind, target: a }], firstClass), "E-RELATION-DUPLICATE") !== undefined,
        `${kind} authored symmetric inverse must fail`,
      );
    }
    return true;
  }
  const kind = id === "cycle_parent" ? "parent_of" : id === "cycle_depends" ? "depends_on" : "supersedes";
  const rows: Relation[] = [
    { source: c, kind, target: a },
    { source: a, kind, target: b },
    { source: b, kind, target: c },
  ];
  const cycle = validateRelations(rows, firstClass).find((value) => value.code === "E-RELATION-CYCLE");
  assert(cycle !== undefined && cycle.message.includes(`\"${a}\" -> \"${b}\" -> \"${c}\" -> \"${a}\"`), `${kind} cycle traversal must be lexical and deterministic`);
  observeSelfTestIssue(cycle);
  assert(
    JSON.stringify(validateRelations(rows, firstClass)) ===
      JSON.stringify(validateRelations([...rows].reverse(), firstClass)),
    `${kind} diagnostics must be deterministic under authored-edge reversal`,
  );
  return true;
}

function identityJoinCase(id: JoinSelfTestCaseId): boolean {
  if (!id.startsWith("identity_")) return false;
  const document = decodedDocument(["matrix.fixture-alpha"]);
  const index = buildRoadmapIndexes(document).indexes;
  const activeId = brandedRoadmapId("matrix.fixture-alpha");
  const guard: CurrentGuard = {
    id: activeId,
    replacement_pin: { kind: "gate", gate_id: "roadmap_projection_check", claim_md: bytes("pin") },
    owner_registry: "fixture-guards",
  };
  const tombstone: RetiredIdV1 = {
    id: activeId,
    last_active_at: "0".repeat(40) as FullCommitId,
    replacement: { kind: "gate", gate_id: "roadmap_projection_check", claim_md: bytes("pin") },
  };
  switch (id) {
    case "identity_active_duplicate": {
      const result = validateGlobalIdentity({ documents: [index.identity_inputs, index.identity_inputs] });
      assert(observeMatchingIssue(result.issues, "E-OWNER-DUPLICATE") !== undefined, "duplicate active claims must fail");
      const testing = buildRoadmapIndexes(decodedDocument(["testing.fixture-alpha"], "testing")).indexes;
      const tiedGuardA: CurrentGuard = {
        ...guard,
        replacement_pin: { ...guard.replacement_pin, claim_md: bytes("alpha") },
      };
      const tiedGuardB: CurrentGuard = {
        ...guard,
        replacement_pin: { ...guard.replacement_pin, claim_md: bytes("beta") },
      };
      const tombstones: RetiredIdV1[] = [
        { ...tombstone, id: brandedRoadmapId("matrix.fixture-retired-a") },
        { ...tombstone, id: brandedRoadmapId("matrix.fixture-retired-b") },
      ];
      const additionalGuards: CurrentGuard[] = [
        { ...guard, id: brandedRoadmapId("matrix.fixture-extra-a") },
        { ...guard, id: brandedRoadmapId("matrix.fixture-extra-b") },
      ];
      const forward = validateGlobalIdentity({
        documents: [index.identity_inputs, testing.identity_inputs],
        current_guards: [tiedGuardB, tiedGuardA, ...additionalGuards],
        tombstones,
      });
      const reversed = validateGlobalIdentity({
        documents: [testing.identity_inputs, index.identity_inputs],
        current_guards: [...additionalGuards].reverse().concat([tiedGuardA, tiedGuardB]),
        tombstones: [...tombstones].reverse(),
      });
      const claimsSignature = (value: typeof forward): string => JSON.stringify(
        [...value.owner_claims].map(([ownerId, claims]) => [
          ownerId,
          claims.map((claim) => identityOwnerClaimKey(claim)),
        ]),
      );
      const ownersSignature = (value: typeof forward): string => JSON.stringify(
        [...value.owners].map(([ownerId, claim]) => [ownerId, identityOwnerClaimKey(claim)]),
      );
      assert(
        claimsSignature(forward) === claimsSignature(reversed),
        "global owner claims must be deterministic under exact-rank/source/path tie reversal",
      );
      assert(
        JSON.stringify(forward.issues) === JSON.stringify(reversed.issues),
        "global identity diagnostics must be deterministic under every input-list reversal",
      );
      assert(
        ownersSignature(forward) === ownersSignature(reversed),
        "normalized global owners must be deterministic under every input-list reversal",
      );
      return true;
    }
    case "identity_active_guard_collision":
      assert(observeMatchingIssue(validateGlobalIdentity({ documents: [index.identity_inputs], current_guards: [guard] }).issues, "E-OWNER-DUPLICATE") !== undefined, "active/guard collision must fail");
      return true;
    case "identity_active_tombstone_collision":
      assert(observeMatchingIssue(validateGlobalIdentity({ documents: [index.identity_inputs], tombstones: [tombstone] }).issues, "E-OWNER-DUPLICATE") !== undefined, "active/tombstone collision must fail in the global owner domain");
      return true;
    case "identity_guard_tombstone_collision":
      assert(observeMatchingIssue(validateGlobalIdentity({ documents: [], current_guards: [guard], tombstones: [tombstone] }).issues, "E-OWNER-DUPLICATE") !== undefined, "guard/tombstone collision must fail");
      return true;
    case "identity_retired_reuse":
      assert(observeMatchingIssue(validateRetiredIdReuse([index.identity_inputs], [tombstone]), "E-RETIRED-REUSE") !== undefined, "retired first-class ID reuse must fail lifecycle policy");
      return true;
    case "identity_alias_collision": {
      const aliasInputs = {
        ...index.identity_inputs,
        alias_providers: [
          ...index.identity_inputs.alias_providers,
          { alias: activeId, namespace: "matrix" as const, owner_kind: "record" as const, owner_id: activeId, logical_path: "fixture.alias" },
        ],
      };
      assert(observeMatchingIssue(validateGlobalIdentity({ documents: [aliasInputs] }).issues, "E-ALIAS-COLLISION") !== undefined, "alias/ID collision must fail");
      const duplicateAliasInputs = {
        ...index.identity_inputs,
        alias_providers: [
          ...index.identity_inputs.alias_providers,
          { alias: "Legacy Z", namespace: "testing" as const, owner_kind: "record" as const, owner_id: "testing.fixture-beta", logical_path: "fixture.duplicate-alias" },
        ],
      };
      assert(observeMatchingIssue(validateGlobalIdentity({ documents: [duplicateAliasInputs] }).issues, "E-ALIAS-COLLISION") !== undefined, "alias/alias collision must fail globally");
      return true;
    }
  }
  return false;
}

function citationCase(id: JoinSelfTestCaseId): boolean {
  if (!id.startsWith("citation_inventory_")) return false;
  const file = (source: string, value?: string | Uint8Array): TrackedTextInput => ({
    source: source as RepoPath,
    bytes: typeof value === "string" ? bytes(value) : value,
  });
  switch (id) {
    case "citation_inventory_tracks_source_byte_spans": {
      const sourceBytes = bytes(`é ${citationText("matrix.fixture-alpha")}. ${citationText("testing.fixture-beta")}-`);
      const result = scanRoadmapCitations([file("README.md", sourceBytes)]);
      assert(result.issues.length === 0 && result.facts.length === 2, "sentence punctuation must not be swallowed into citation IDs");
      assert(
        result.facts[0]!.span.start_byte === 3 && result.facts[0]!.span.end_byte === 31 &&
          result.facts[0]!.raw === citationText("matrix.fixture-alpha"),
        "citation must use exact UTF-8 half-open byte offsets and exclude terminal period punctuation",
      );
      assert(
        result.facts[1]!.raw === citationText("testing.fixture-beta") &&
          sourceBytes[result.facts[1]!.span.end_byte] === "-".charCodeAt(0),
        "terminal hyphen punctuation without a next component must remain outside the citation span",
      );
      const immutable = scanRoadmapMarkdownFacts("README.md" as RepoPath, {
        byte_length: sourceBytes.byteLength,
        sliceBytes: (start, end) => sourceBytes.slice(start, end),
      });
      assert(
        immutable.issues.length === 0 && JSON.stringify(immutable.citations) === JSON.stringify(result.facts),
        "tracked-file and immutable-view citation scans must share one exact tokenizer",
      );
      const byteOnly = scanRoadmapCitations([
        file("crlf.fixture", `${citationText("matrix.fixture-alpha")}\r\n`),
        file("non-utf8.fixture", new Uint8Array([
          0xff,
          ...bytes(citationText("testing.fixture-beta")),
        ])),
      ]);
      assert(
        byteOnly.issues.length === 0 && byteOnly.facts.length === 2 &&
          byteOnly.facts[0]!.span.start_byte === 0 && byteOnly.facts[1]!.span.start_byte === 1,
        "citation extraction must remain byte-based across CRLF and non-UTF-8 non-NUL tracked files",
      );
      return true;
    }
    case "citation_inventory_sorted_by_path_and_span": {
      const result = scanRoadmapCitations([
        file("z.md", citationText("testing.fixture-z")),
        file("a.md", `x ${citationText("matrix.fixture-b")} y ${citationText("matrix.fixture-a")}`),
      ]);
      assert(result.facts.map((fact) => `${fact.source}:${fact.span.start_byte}`).join("|") === "a.md:2|a.md:29|z.md:0", "citations must sort by path then byte span");
      return true;
    }
    case "citation_inventory_multiple_occurrences":
      assert(scanRoadmapCitations([file("a.md", `${citationText("matrix.fixture-a")} ${citationText("matrix.fixture-a")}`)]).facts.length === 2, "all occurrences must be retained");
      return true;
    case "citation_inventory_malformed_id_rejected": {
      for (const candidate of [
        citationText("Matrix.bad"),
        citationText("matrix.fixture_alpha"),
        citationText("matrix.fixture-alpha.Bad"),
        citationText("matrix.fixture-alpha.2bad"),
      ]) {
        const regular = scanRoadmapCitations([file("a.md", candidate)]);
        const candidateBytes = bytes(candidate);
        const immutable = scanRoadmapMarkdownFacts("a.md" as RepoPath, {
          byte_length: candidateBytes.byteLength,
          sliceBytes: (start, end) => candidateBytes.slice(start, end),
        });
        const regularIssue = observeMatchingIssue(regular.issues, "E-ID-GRAMMAR");
        const immutableIssue = observeMatchingIssue(immutable.issues, "E-ID-GRAMMAR");
        assert(regularIssue !== undefined, `malformed citation candidate must fail: ${candidate}`);
        assert(
          immutableIssue !== undefined &&
            JSON.stringify(immutableIssue.span) === JSON.stringify(regularIssue.span),
          `both citation consumers must reject the same malformed candidate span: ${candidate}`,
        );
      }
      return true;
    }
    case "citation_inventory_draft_excluded":
      {
        const result = scanRoadmapCitations([
          file("README.md", citationText("matrix.fixture-alpha")),
          file("draft/note.md", citationText("matrix.fixture-beta")),
        ]);
        assert(
          result.issues.length === 0 && result.facts.length === 1 &&
            result.facts[0]!.id === "matrix.fixture-alpha",
          "draft exclusion must retain one eligible sentinel citation without diagnostics",
        );
      }
      return true;
    case "citation_inventory_nul_binary_excluded":
      {
        const result = scanRoadmapCitations([
          file("README.md", citationText("matrix.fixture-alpha")),
          file("binary.dat", bytes(`\0${citationText("matrix.fixture-beta")}`)),
        ]);
        assert(
          result.issues.length === 0 && result.facts.length === 1 &&
            result.facts[0]!.id === "matrix.fixture-alpha",
          "NUL exclusion must retain one eligible sentinel citation without diagnostics",
        );
      }
      return true;
    case "citation_inventory_tracked_missing_rejected":
      assert(observeMatchingIssue(scanRoadmapCitations([file("missing.md")]).issues, "E-SOURCE-MISSING") !== undefined, "missing tracked file must fail");
      return true;
    case "citation_inventory_base_revision_isolated": {
      const base = scanRoadmapCitations([file("a.md", citationText("matrix.fixture-alpha"))]);
      const current = scanRoadmapCitations([file("a.md", citationText("matrix.fixture-beta"))]);
      assert(base.facts[0]!.id !== current.facts[0]!.id, "revision snapshots must not leak citation facts");
      return true;
    }
  }
  return false;
}

function rustInputs(body: string, modules = "pub(crate) mod sample;", extra: readonly TrackedTextInput[] = []): readonly TrackedTextInput[] {
  return [
    { source: "src/main.rs" as RepoPath, bytes: bytes("#[cfg(test)] mod tests;") },
    { source: "src/tests/mod.rs" as RepoPath, bytes: bytes(modules) },
    { source: "src/tests/sample.rs" as RepoPath, bytes: bytes(body) },
    ...extra,
  ];
}

function rustInputsWithRoot(root: string, modules: string): readonly TrackedTextInput[] {
  return [
    { source: "src/main.rs" as RepoPath, bytes: bytes(root) },
    { source: "src/tests/mod.rs" as RepoPath, bytes: bytes(modules) },
  ];
}

function testSymbolCase(id: JoinSelfTestCaseId): boolean {
  if (!id.startsWith("test_symbol_fact_")) return false;
  switch (id) {
    case "test_symbol_fact_top_level_direct_test": {
      const rootBody =
        "fn borrow<'a>(value: &'a str) -> &'a str { 'label: loop { break 'label value; } }\n" +
        "const A: char = 'x'; const B: char = '\\n'; const C: char = '\\u{1f980}';\n" +
        "#[test] fn component_profile_row_is_live() {}\n" +
        "pub(crate) mod sample;";
      const result = extractRustTestSymbols([
        { source: "src/main.rs" as RepoPath, bytes: bytes("#[cfg(test)] mod tests;") },
        { source: "src/tests/mod.rs" as RepoPath, bytes: bytes(rootBody) },
        { source: "src/tests/sample.rs" as RepoPath, bytes: bytes("#[test] fn child_sentinel() {}") },
      ]);
      const rootFact = result.facts.find((fact) => fact.symbol === "tests::component_profile_row_is_live");
      assert(
        result.issues.length === 0 && result.facts.length === 2 && rootFact !== undefined &&
          rootFact.source === "src/tests/mod.rs" && rootFact.module_path.join("::") === "tests" &&
          rootFact.test_id === "rust-test:cddl-codegen#tests::component_profile_row_is_live" &&
          rootFact.span.start_byte === new TextEncoder().encode(rootBody.slice(0, rootBody.indexOf("component_profile_row_is_live"))).byteLength,
        "src/tests/mod.rs is a registered source module whose direct tests use the tests prefix and exact identifier span",
      );
      assert(
        result.facts.some((fact) => fact.symbol === "tests::sample::child_sentinel"),
        "scanning direct root tests must retain declared-child extraction",
      );
      return true;
    }
    case "test_symbol_fact_declared_module_prefix": {
      const result = extractRustTestSymbols([
        { source: "src/main.rs" as RepoPath, bytes: bytes("#[cfg(test)] mod tests;") },
        { source: "src/tests/mod.rs" as RepoPath, bytes: bytes("pub(crate) mod alternate;") },
        { source: "src/tests/alternate.rs" as RepoPath, bytes: bytes("#[test] fn prefix_sentinel() {}") },
      ]);
      assert(
        result.issues.length === 0 && result.facts.length === 1 &&
          result.facts[0]!.symbol === "tests::alternate::prefix_sentinel",
        "the exact declared filename must determine the test-symbol prefix",
      );
      return true;
    }
    case "test_symbol_fact_id_derivation_exact": {
      const body = "const É: &str = \"é\";\n#[test] fn exact_identifier() {}";
      const result = extractRustTestSymbols(rustInputs(body));
      const identifierStart = new TextEncoder().encode(body.slice(0, body.indexOf("exact_identifier"))).byteLength;
      assert(result.issues.length === 0 && result.facts.length === 1, "one exact-ID sentinel must be derived");
      assert(result.facts[0]!.test_id === "rust-test:cddl-codegen#tests::sample::exact_identifier", "test ID derivation must be exact");
      assert(
        result.facts[0]!.span.start_byte === identifierStart &&
          result.facts[0]!.span.end_byte === identifierStart + "exact_identifier".length,
        "identifier span must use exact UTF-8 byte offsets",
      );
      return true;
    }
    case "test_symbol_fact_inline_module_path": {
      const result = extractRustTestSymbols(rustInputs("mod outer { mod inner { #[test] fn nested_sentinel() {} } }"));
      assert(
        result.issues.length === 0 && result.facts.length === 1 &&
          result.facts[0]!.symbol === "tests::sample::outer::inner::nested_sentinel",
        "every inline module must extend the symbol path",
      );
      return true;
    }
    case "test_symbol_fact_attribute_between_test_and_fn": {
      const result = extractRustTestSymbols(rustInputs(
        "#[cfg_attr(test, test)] fn cfg_attr_fake() {}\n" +
          "#[tokio::test] async fn tokio_fake() {}\n" +
          "#[test] #[ignore] pub(crate) async fn async_sentinel() {}",
      ));
      assert(
        result.issues.length === 0 && result.facts.length === 1 &&
          result.facts[0]!.symbol.endsWith("::async_sentinel"),
        "only direct #[test] with intervening attributes/visibility and async fn must be derived",
      );
      return true;
    }
    case "test_symbol_fact_macro_template_excluded":
      for (const template of [
        "macro_rules! make { () => { #[test] fn fake() {} } }",
        "macro_rules! make ( () => { #[test] fn fake() {} } );",
        "macro_rules! make [ () => { #[test] fn fake() {} } ];",
      ]) {
        const result = extractRustTestSymbols(rustInputs(`${template}\n#[test] fn macro_sentinel() {}`));
        assert(
          result.issues.length === 0 && result.facts.length === 1 &&
            result.facts[0]!.symbol.endsWith("::macro_sentinel"),
          "complete macro templates must be excluded for every delimiter while retaining a sentinel",
        );
      }
      return true;
    case "test_symbol_fact_macro_invocation_excluded": {
      const macroRoots = ["(", "[", "{"].map((open) => {
        const close = open === "(" ? ")" : open === "[" ? "]" : "}";
        return `wrap!${open} #[cfg(test)] mod tests; ${close};`;
      }).join("\n");
      const macroModules = ["(", "[", "{"].map((open, index) => {
        const close = open === "(" ? ")" : open === "[" ? "]" : "}";
        return `qualified::wrap!${open} pub(crate) mod fake_${index}; ${close};`;
      }).join("\n");
      const macroTests = ["(", "[", "{"].map((open, index) => {
        const close = open === "(" ? ")" : open === "[" ? "]" : "}";
        return `wrap!${open} #[test] fn generated_fake_${index}() {} ${close};`;
      }).join("\n");
      const result = extractRustTestSymbols([
        {
          source: "src/main.rs" as RepoPath,
          bytes: bytes(`${macroRoots}\n#[cfg(test)] mod tests;`),
        },
        {
          source: "src/tests/mod.rs" as RepoPath,
          bytes: bytes(`${macroModules}\npub(crate) mod sample;`),
        },
        {
          source: "src/tests/sample.rs" as RepoPath,
          bytes: bytes(`${macroTests}\n#[test] fn invocation_sentinel() {}`),
        },
      ]);
      assert(
        result.issues.length === 0 && result.facts.length === 1 &&
          result.facts[0]!.symbol.endsWith("::invocation_sentinel"),
        "(), [], and {} macro invocations must not mint root, module, or test facts",
      );
      return true;
    }
    case "test_symbol_fact_comments_and_strings_excluded": {
      const result = extractRustTestSymbols(rustInputs(
        "// #[test] fn line_fake() {}\n" +
          "/* outer /* #[test] fn nested_comment_fake() {} */ end */\n" +
          "const A: &str = \"#[test] fn string_fake() {}\";\n" +
          "const B: &str = r#\"quote \\\" #[test] fn raw_fake() {}\"#;\n" +
          "const C: &[u8] = br##\"quote \\\" #[test] fn byte_raw_fake() {}\"##;\n" +
          "const D: &CStr = cr##\"quote \\\" #[test] fn c_raw_fake() {}\"##;\n" +
          "const E: char = '\\'';\n#[test] fn literal_sentinel() {}",
      ));
      assert(
        result.issues.length === 0 && result.facts.length === 1 &&
          result.facts[0]!.symbol.endsWith("::literal_sentinel"),
        "comments and every bounded literal form must be excluded while retaining one sentinel",
      );
      return true;
    }
    case "test_symbol_fact_undeclared_file_excluded": {
      const extra = [{ source: "src/tests/hidden.rs" as RepoPath, bytes: bytes("#[test] fn hidden() {}") }];
      const result = extractRustTestSymbols(rustInputs(
        "#[test] fn declared_sentinel() {}",
        "pub(crate) mod sample;",
        extra,
      ));
      assert(
        result.issues.length === 0 && result.facts.length === 1 &&
          result.facts[0]!.symbol.endsWith("::declared_sentinel"),
        "undeclared test files must be excluded while retaining the declared sentinel",
      );
      return true;
    }
    case "test_symbol_fact_missing_declared_file_rejected": {
      assert(observeMatchingIssue(extractRustTestSymbols(rustInputs("", "pub(crate) mod missing;")).issues, "E-SOURCE-MISSING") !== undefined, "missing declared test file must fail");
      for (const malformed of [
        "mod sample;",
        "pub mod sample;",
        "pub(super) mod sample;",
        "pub(in crate::tests) mod sample;",
        "pub(crate) mod sample {}",
        "#[path = \"other.rs\"] pub(crate) mod sample;",
        "pub(crate) mod",
        "pub",
        "pub(",
      ]) {
        const result = extractRustTestSymbols(rustInputsWithRoot("#[cfg(test)] mod tests;", malformed));
        assert(result.issues.length > 0, `malformed registry declaration must diagnose: ${malformed}`);
      }
      for (const malformedRoot of [
        "mod outer { #[cfg(test)] mod tests; }",
        "#[cfg(test)] mod tests {}",
        "#[path = \"tests/mod.rs\"] #[cfg(test)] mod tests;",
        "#[cfg(test)] #[path = \"tests/mod.rs\"] mod tests;",
        "#[cfg_attr(test, cfg(test))] mod tests;",
      ]) {
        const result = extractRustTestSymbols(rustInputsWithRoot(malformedRoot, "pub(crate) mod sample;"));
        assert(
          result.issues.some((value) => value.source === "src/main.rs"),
          `non-exact or nested root declaration must diagnose: ${malformedRoot}`,
        );
      }
      return true;
    }
    case "test_symbol_fact_duplicate_id_rejected":
      assert(observeMatchingIssue(extractRustTestSymbols(rustInputs("#[test] fn works() {} #[test] fn works() {}" )).issues, "E-ID-DUPLICATE") !== undefined, "duplicate derived test IDs must fail");
      return true;
    case "test_symbol_fact_base_revision_isolated": {
      const base = extractRustTestSymbols(rustInputs("#[test] fn old() {}"));
      const current = extractRustTestSymbols(rustInputs("#[test] fn new() {}"));
      assert(base.facts[0]!.test_id !== current.facts[0]!.test_id, "revision snapshots must not leak test facts");
      return true;
    }
  }
  return false;
}

function executeJoin(id: JoinSelfTestCaseId): void {
  assert(
    referenceProviderCase(id) || relationCase(id) || identityJoinCase(id) || citationCase(id) || testSymbolCase(id),
    `C4B-J case ${id} has no executable proof`,
  );
}

function execute(
  id: PermanentIdSelfTestCaseId,
  fixtureBundle?: IdentityFixtureBundle,
): void {
  switch (id) {
    case "id_grammar_accept": {
      for (const value of [
        "matrix.fixture-a",
        "matrix.fixture-a.deep-node",
        "testing.fixture-z9.deep-node-x7",
      ]) requireAccepted(value);
      for (const value of ["a", "a-b", "a9-b8-c7"]) {
        assert(validateSubordinateId(value).ok, `${value} should be a subordinate ID`);
        assert(validateReferenceId(value).ok, `${value} should be a reference ID`);
      }
      for (const value of [
        "fixture",
        "matrix_fixture",
        "Matrix.fixture",
        "matrix.9fixture",
        "matrix.fixture..deep",
        "matrix.fixture-9",
      ]) requireRejected(value, "E-ID-GRAMMAR");
      for (const value of ["", "9a", "a_thing", "a--thing", "a-9"]) {
        assert(!validateSubordinateId(value).ok, `${value} must not be a subordinate ID`);
        assert(!validateReferenceId(value).ok, `${value} must not be a reference ID`);
      }
      return;
    }
    case "id_reserved_tokens": {
      for (const token of ROADMAP_ID_POLICY_V1.reserved_tokens) {
        requireRejected(`matrix.fixture-${token}-owner`, "E-ID-RESERVED", "matrix");
        requireRejected(`matrix.fixture.${token}.owner`, "E-ID-RESERVED", "matrix");
      }
      requireAccepted("matrix.fixture-itemized", "matrix");
      requireAccepted("matrix.fixture-workable", "matrix");
      requireAccepted("matrix.fixture.itemized.owner", "matrix");
      requireAccepted("matrix.fixture.workable.owner", "matrix");
      const decoded = decodedDocument(["matrix.fixture-valid"]);
      const reservedDocument = {
        ...decoded,
        records: [{ ...decoded.records[0]!, id: "matrix.fixture-work-owner" as RoadmapId }],
      } as RoadmapDocument;
      assert(
        buildRoadmapIndexes(reservedDocument).issues.some((issue) => issue.code === "E-ID-RESERVED"),
        "decoded-document index construction must reapply permanent reserved-token policy",
      );
      return;
    }
    case "id_numeric_legacy_tokens": {
      for (const token of ["b1", "t22", "f3", "q44", "wp5", "item600"]) {
        requireRejected(`testing.fixture-${token}-owner`, "E-ID-RESERVED", "testing");
        requireRejected(`testing.fixture.${token}.owner`, "E-ID-RESERVED", "testing");
      }
      for (const value of ["testing.fixture-b", "testing.fixture-wp", "testing.fixture-itemized7"]) {
        requireAccepted(value, "testing");
      }
      for (const value of [
        "testing.fixture.b.owner",
        "testing.fixture.wp.owner",
        "testing.fixture.itemized7.owner",
      ]) requireAccepted(value, "testing");
      return;
    }
    case "id_namespace_mismatch": {
      requireRejected("testing.fixture-alpha", "E-ID-NAMESPACE", "matrix");
      requireRejected("matrix.fixture-alpha", "E-ID-NAMESPACE", "testing");
      return;
    }
    case "id_is_opaque_to_consumers": {
      assert(fixtureBundle !== undefined, "fixture-backed identity case requires an injected bundle");
      assertCommittedFixtureIndexes(fixtureBundle);
      const document = decodedDocument([
        "matrix.fixture.deep-provider",
        "matrix.fixture-shallow-provider",
      ]);
      const reversed = { ...document, records: [...document.records].reverse() } as RoadmapDocument;
      const forwardIndexes = buildRoadmapIndexes(document);
      const reverseIndexes = buildRoadmapIndexes(reversed);
      assert(forwardIndexes.issues.length === 0, "valid decoded document should index cleanly");
      assert(reverseIndexes.issues.length === 0, "record reversal should index cleanly");
      assert(
        [...forwardIndexes.indexes.first_class.keys()].join("|") ===
          [...reverseIndexes.indexes.first_class.keys()].join("|"),
        "first-class index order must not depend on record-table order",
      );
      assert(
        [...forwardIndexes.indexes.record_nodes.keys()].join("|") ===
          [...reverseIndexes.indexes.record_nodes.keys()].join("|"),
        "record index order must not depend on record-table order",
      );
      assert(
        forwardIndexes.indexes.id_providers.every((provider) => provider.kind === "record"),
        "provider kind must come from structural position, not dot depth or spelling",
      );
      assert(
        [...forwardIndexes.indexes.aliases.keys()].join("|") ===
          "Legacy Section|Legacy Y|Legacy Z",
        "aliases must be indexed as exact opaque strings in code-point order",
      );
      const duplicateRecord = {
        ...document,
        records: [document.records[0]!, document.records[0]!],
      } as RoadmapDocument;
      assert(
        buildRoadmapIndexes(duplicateRecord).issues.some((issue) => issue.code === "E-ID-DUPLICATE"),
        "duplicate first-class providers must remain visible before global identity joins",
      );
      const duplicateSection = {
        ...document,
        sections: [document.sections[0]!, document.sections[0]!],
      } as RoadmapDocument;
      assert(
        buildRoadmapIndexes(duplicateSection).issues.some((issue) => issue.code === "E-ID-DUPLICATE"),
        "duplicate subordinate providers must remain visible per owner kind",
      );
      return;
    }
  }
}

const C5_BASE = "0123456789abcdef0123456789abcdef01234567" as FullCommitId;
const C5_ID = "matrix.fixture-lifecycle" as RoadmapId;
const C5_TESTING_ID = "testing.fixture-lifecycle" as RoadmapId;

function c5Sha(value: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(value).digest("hex");
}

function c5Path(roadmap: RoadmapName): RepoPath {
  return (roadmap === "matrix" ? "cddl-matrix/ROADMAP.md" : "tests/TESTING_ROADMAP.md") as RepoPath;
}

function c5SourcePath(roadmap: RoadmapName): RepoPath {
  return `fixture/${roadmap}.toml` as RepoPath;
}

function c5LegacySource(): Uint8Array {
  return bytes("## Lifecycle\nlegacy body\n");
}

function c5ExpectedView(markdown: Uint8Array, observer: ExpectedByteViewObserver) {
  const first = Math.max(1, Math.floor(markdown.byteLength / 3));
  const second = Math.max(first + 1, Math.floor(markdown.byteLength * 2 / 3));
  const chunks: RenderChunk[] = [
    markdown.subarray(0, first),
    markdown.subarray(first, second),
    markdown.subarray(second),
  ].map((chunk, manifest_index) => ({
    manifest_index,
    owner: { kind: "fragment", id: `lifecycle-view-${manifest_index}`, field: "body_md" },
    bytes: chunk,
    source_span_ids: [],
    consumed_fields: ["body_md"],
  }));
  return createExpectedByteView(chunks, observer);
}

function c5V0(roadmap: RoadmapName, markdown: Uint8Array, id?: RoadmapId): RoadmapDocumentV0 {
  const records: RoadmapDocumentV0["records"] = id === undefined ? [] : [{
    id,
    title: "Lifecycle",
    projection_group: "fixture" as RoadmapDocumentV0["records"][number]["projection_group"],
    source_block_md: markdown.slice(),
    span_ids: ["lifecycle" as RoadmapDocumentV0["spans"][number]["id"]],
  }];
  return {
    document: {
      schema_version: 0,
      authority: "shadow",
      roadmap,
      source_path: c5SourcePath(roadmap),
      projection_path: c5Path(roadmap),
      frozen_source_sha256: c5Sha(markdown),
      frozen_source_byte_length: markdown.byteLength,
      frozen_source_line_count: [...markdown].filter((value) => value === 10).length,
      frozen_source_eof: markdown.at(-1) === 10 ? "lf" : "none",
    },
    sections: [], fragments: [], legacy_markers: [], records, parts: [], generated_slots: [],
    manifest: id === undefined ? [] : [{ kind: "record", record_id: id }],
    spans: id === undefined ? [] : [{
      id: "lifecycle" as RoadmapDocumentV0["spans"][number]["id"],
      start_byte: 0,
      end_byte: markdown.byteLength,
      sha256: c5Sha(markdown),
      source_kind: "record",
      owner_id: id,
      owner_field: "source_block_md",
      migration_status: "raw",
    }],
  };
}

function c5ReadyRecord(id: RoadmapId, workKind: WorkKind = "feature"): SemanticAuthorityRecordV1 {
  return {
    id,
    title: "Lifecycle",
    projection_group: "fixture" as RoadmapDocumentV1["records"][number]["projection_group"],
    render_authority: "semantic",
    projection_visibility: "semantic_only",
    payload: {
      kind: "work",
      summary_md: bytes("summary"),
      work_state: "ready",
      work_intent: "build_capability",
      work_kind: workKind,
      risk: "compile_failure",
      family_classification: "none_reviewed",
      acceptance_md: bytes("acceptance"),
      priority_rationale_md: bytes("priority"),
    },
    source_replacements: [],
  };
}

function c5FamilyRecord(id: RoadmapId, workIds: readonly RoadmapId[] = []): SemanticAuthorityRecordV1 {
  const axisId = `${id}.fixture-dimension` as RoadmapId;
  const valueId = `${id}.fixture-choice` as RoadmapId;
  return {
    id,
    title: "Family",
    projection_group: "fixture" as RoadmapDocumentV1["records"][number]["projection_group"],
    render_authority: "semantic",
    projection_visibility: "semantic_only",
    payload: {
      kind: "family",
      summary_md: bytes("family"),
      family_maturity: "observed_only",
      campaign_state: "designing",
      goal_md: bytes("goal"),
      boundary_md: bytes("boundary"),
      work_ids: [...workIds],
      observation_reference_ids: [],
      affected_profiles: [],
      affected_faces: [],
      control_ids: [],
      completion_owner_reference_id: "completion" as ReferenceId,
      retirement_owner_reference_id: "retirement" as ReferenceId,
      axes: [{
        id: axisId, label: "Axis", authority_reference_id: "axis-authority" as ReferenceId,
        values: [{ id: valueId, label: "Value", source_reference_id: "axis-source" as ReferenceId }],
      }],
      evidence_requirements: [{
        id: `${id}.fixture-proof` as RoadmapId,
        profiles: ["default"], faces: ["rust"], stages: ["compiled"],
      }],
      cells: [{
        id: `${id}.fixture-point` as RoadmapId,
        spec_legality: "legal", cell_disposition: "unknown", affected_profiles: ["default"],
        affected_faces: ["rust"], coordinates: [{ axis_id: axisId, value_id: valueId }],
      }],
      exclusions: [{
        id: `${id}.fixture-excluded` as RoadmapId,
        spec_legality: "illegal", reason_md: bytes("illegal"), owner_reference_id: "owner" as ReferenceId,
        source_reference_id: "source" as ReferenceId, liveness_reference_id: "liveness" as ReferenceId,
        coordinates: [{ axis_id: axisId, value_id: valueId }],
      }],
    },
    source_replacements: [],
  };
}

function c5FamilyChildIds(id: RoadmapId): readonly RoadmapId[] {
  return Object.freeze([
    `${id}.fixture-dimension` as RoadmapId,
    `${id}.fixture-choice` as RoadmapId,
    `${id}.fixture-proof` as RoadmapId,
    `${id}.fixture-point` as RoadmapId,
    `${id}.fixture-excluded` as RoadmapId,
  ]);
}

const C5_FAMILY_PROVIDER_KINDS = Object.freeze([
  "family_axis",
  "family_axis_value",
  "family_evidence_requirement",
  "family_cell",
  "family_exclusion",
] as const);

function c5FamilyRecordUsingChildIds(
  id: RoadmapId,
  childIds: readonly RoadmapId[],
): SemanticAuthorityRecordV1 {
  assert(childIds.length === 5, "family provider fixture requires five exact child IDs");
  const record = c5FamilyRecord(id);
  assert(record.payload.kind === "family", "family provider fixture must retain family payload");
  record.payload.axes[0]!.id = childIds[0]!;
  record.payload.axes[0]!.values[0]!.id = childIds[1]!;
  record.payload.evidence_requirements[0]!.id = childIds[2]!;
  record.payload.cells[0]!.id = childIds[3]!;
  record.payload.exclusions[0]!.id = childIds[4]!;
  return record;
}

function c5Guard(id: RoadmapId, gateId = "roadmap_projection_check"): CurrentGuard {
  return {
    id,
    replacement_pin: { kind: "gate", gate_id: gateId, claim_md: bytes("guard") },
    owner_registry: "fixture-guards",
  };
}

function c5FamilyGuards(id: RoadmapId): readonly CurrentGuard[] {
  return Object.freeze([c5Guard(id), ...c5FamilyChildIds(id).map((childId) => c5Guard(childId))]);
}

function c5V1(
  roadmap: RoadmapName,
  records: readonly RoadmapDocumentV1["records"][number][] = [],
  relations: readonly Relation[] = [],
  references: readonly Reference[] = [],
): RoadmapDocumentV1 {
  const markdown = bytes(`${roadmap}\n`);
  return {
    document: {
      schema_version: 1,
      authority: "authoritative",
      roadmap,
      source_path: c5SourcePath(roadmap),
      projection_path: c5Path(roadmap),
      frozen_source_sha256: c5Sha(markdown),
      frozen_source_byte_length: markdown.byteLength,
      frozen_source_line_count: 1,
      frozen_source_eof: "lf",
      frozen_legacy_span_ids: [],
    },
    sections: [], fragments: [], legacy_markers: [], records: [...records], parts: [],
    generated_slots: [],
    manifest: records.map((record) => ({ kind: "record" as const, record_id: record.id })),
    spans: [], relations: [...relations], references: [...references],
  };
}

function c5PromoteV0(
  base: RoadmapDocumentV0,
  semanticShadows: ReadonlyMap<RoadmapId, SemanticAuthorityRecordV1["payload"]> = new Map(),
): RoadmapDocumentV1 {
  return {
    document: {
      schema_version: 1,
      authority: "authoritative",
      roadmap: base.document.roadmap,
      source_path: base.document.source_path,
      projection_path: base.document.projection_path,
      frozen_source_sha256: base.document.frozen_source_sha256,
      frozen_source_byte_length: base.document.frozen_source_byte_length,
      frozen_source_line_count: base.document.frozen_source_line_count,
      frozen_source_eof: base.document.frozen_source_eof,
      frozen_legacy_span_ids: base.spans.filter((span) => span.migration_status === "raw").map((span) => span.id),
    },
    sections: base.sections.map((value) => ({ ...value, render_authority: "raw" as const })),
    fragments: base.fragments.map((value) => ({ ...value, render_authority: "raw" as const })),
    legacy_markers: base.legacy_markers.map((value) => ({ ...value, render_authority: "raw" as const })),
    records: base.records.map((value) => ({
      ...value,
      render_authority: "raw" as const,
      ...(semanticShadows.has(value.id) ? { semantic_shadow: semanticShadows.get(value.id)! } : {}),
    })),
    parts: base.parts.map((value) => ({ ...value, render_authority: "raw" as const })),
    generated_slots: [...base.generated_slots], manifest: [...base.manifest], spans: [...base.spans],
    relations: [], references: [],
  };
}

function c5Reservation(
  id: RoadmapId,
  markdown: Uint8Array,
  workKind = "feature" as const,
): LegacyMarkdownReservationV1 {
  const roadmap = id.startsWith("matrix.") ? "matrix" : "testing";
  return {
    id,
    work_kind: workKind,
    roadmap_path: c5Path(roadmap) as LegacyMarkdownReservationV1["roadmap_path"],
    source_title: "Lifecycle",
    source_start_byte: 0,
    source_end_byte: markdown.byteLength,
    source_sha256: c5Sha(markdown),
    whole_source_sha256: c5Sha(markdown),
  };
}

function c5Title(
  reservation: LegacyMarkdownReservationV1,
  markdown: Uint8Array,
): LegacyTitleBindingFact {
  const binding = validateLegacyTitleBinding(reservation, markdown);
  assert(binding !== undefined, "fixture reservation must mint one exact reviewed-title capability");
  return binding;
}

function c5OwnerCapability(
  evidence: readonly CampaignIdentityOwnerEvidence[],
): CampaignIdentityOwnerCapability {
  const result = validateCampaignIdentityOwnerEvidence(evidence);
  assert(result.ok, `fixture campaign-owner evidence must mint: ${JSON.stringify(result.issues)}`);
  return result.capability;
}

function c5Selection(id: RoadmapId, target: "active_id" | "legacy_markdown_reservation" = "active_id") {
  return {
    item_id: id,
    target_kind: target,
    selected_state: "selected" as const,
    priority_class: "high" as CampaignDocumentV1["selections"][number]["priority_class"],
    selection_reason_md: bytes("reason"),
    cycle: "cycle-a" as CampaignDocumentV1["selections"][number]["cycle"],
    remaining_scope_md: bytes("scope"),
  };
}

function c5Campaign(
  matrix: CampaignDocumentV1["campaign"]["matrix_authority"],
  testing: CampaignDocumentV1["campaign"]["testing_authority"],
  reservations: readonly LegacyMarkdownReservationV1[] = [],
  selections: readonly CampaignDocumentV1["selections"][number][] = [],
): CampaignDocumentV1 {
  return {
    campaign: { schema_version: 1, matrix_authority: matrix, testing_authority: testing },
    legacy_markdown_reservations: [...reservations],
    selections: [...selections],
  };
}

function c5Retired(entries: readonly RetiredIdV1[] = []): RetiredIdsDocumentV1 {
  return { retired_ids: { schema_version: 1 }, entries: [...entries] };
}

function c5Tombstone(id: RoadmapId, last = C5_BASE): RetiredIdV1 {
  return {
    id,
    last_active_at: last,
    replacement: { kind: "gate", gate_id: "roadmap_projection_check", claim_md: bytes("replacement") },
  };
}

function c5Registry(
  revision: RegistryView["revision"],
  overrides: Partial<RegistryView> = {},
): RegistryView {
  return fakeRegistryView({ revision, ...overrides });
}

function c5Snapshots(
  matrix: CampaignRoadmapSnapshot,
  testing: CampaignRoadmapSnapshot = { markdown: bytes("testing\n") },
): Readonly<Record<RoadmapName, CampaignRoadmapSnapshot>> {
  return { matrix, testing };
}

function c5EmptyDebt(): MigrationDebt {
  return { owners: new Map(), independent: new Map(), frozen_legacy_spans: new Map() };
}

function c5Debt(document: RoadmapDocument): MigrationDebt {
  const owners = new Map<string, { key: Parameters<typeof debtOwnerIndex>[0]; state: "raw_unclassified" | "raw_with_semantic_shadow" | "semantic" }>();
  const frozen = new Map<string, Parameters<typeof debtOwnerIndex>[0]>();
  const add = (key: Parameters<typeof debtOwnerIndex>[0], state: "raw_unclassified" | "raw_with_semantic_shadow" | "semantic"): void => {
    owners.set(debtOwnerIndex(key), { key, state });
  };
  const collectMarkdown = (value: unknown, path: string, out: string[]): void => {
    if (value instanceof Uint8Array) {
      if (path.endsWith("_md")) out.push(path);
      return;
    }
    if (Array.isArray(value)) {
      value.forEach((item, index) => collectMarkdown(item, `${path}[${index}]`, out));
      return;
    }
    if (value !== null && typeof value === "object") {
      for (const key of Object.keys(value).sort()) {
        collectMarkdown((value as Record<string, unknown>)[key], `${path}.${key}`, out);
      }
    }
  };
  for (const record of document.records) {
    if ("source_block_md" in record) {
      add({
        roadmap: document.document.roadmap,
        owner_kind: "record",
        owner_id: record.id,
        owner_field: "source_block_md",
      }, "semantic_shadow" in record && record.semantic_shadow !== undefined
        ? "raw_with_semantic_shadow"
        : "raw_unclassified");
    } else {
      const fields: string[] = [];
      collectMarkdown(record.payload, "payload", fields);
      for (const field of fields.sort()) add({
        roadmap: document.document.roadmap,
        owner_kind: "record",
        owner_id: record.id,
        owner_field: field,
      }, "semantic");
    }
  }
  for (const span of document.spans) {
    const key: Parameters<typeof debtOwnerIndex>[0] = {
      roadmap: document.document.roadmap,
      owner_kind: "source_span",
      owner_id: span.id,
      owner_field: "coverage",
    };
    const ownerRecord = span.source_kind === "record"
      ? document.records.find((record) => record.id === span.owner_id)
      : undefined;
    add(key, span.migration_status === "raw"
      ? ownerRecord !== undefined && "semantic_shadow" in ownerRecord && ownerRecord.semantic_shadow !== undefined
        ? "raw_with_semantic_shadow"
        : "raw_unclassified"
      : "semantic");
    const frozenIds = document.document.schema_version === 0
      ? document.spans.filter((value) => value.migration_status === "raw").map((value) => value.id)
      : document.document.frozen_legacy_span_ids;
    if (frozenIds.includes(span.id)) frozen.set(debtOwnerIndex(key), key);
  }
  return { owners, independent: new Map(), frozen_legacy_spans: frozen };
}

function c5SnapshotDebt(
  snapshots: Readonly<Record<RoadmapName, CampaignRoadmapSnapshot>>,
): Partial<Readonly<Record<RoadmapName, MigrationDebt>>> {
  const debt: Partial<Record<RoadmapName, MigrationDebt>> = {};
  for (const roadmap of ["matrix", "testing"] as const) {
    const document = snapshots[roadmap].document;
    if (document !== undefined) debt[roadmap] = c5Debt(document);
  }
  return debt;
}

function c5WithDebt<T extends {
  readonly roadmaps: Readonly<Record<RoadmapName, CampaignRoadmapSnapshot>>;
}>(revision: T): T & { readonly debt: Partial<Readonly<Record<RoadmapName, MigrationDebt>>> } {
  return { ...revision, debt: c5SnapshotDebt(revision.roadmaps) };
}

function c5ActiveRevision(
  document: RoadmapDocumentV1,
  selections: readonly CampaignDocumentV1["selections"][number][] = [],
  options: {
    readonly retired?: readonly RetiredIdV1[];
    readonly guards?: readonly CurrentGuard[];
    readonly citations?: RegistryView["roadmap_citations"];
  } = {},
) {
  return c5WithDebt({
    campaign: c5Campaign("authoritative", "legacy_markdown", [], selections),
    retired: c5Retired(options.retired),
    roadmaps: c5Snapshots({ markdown: bytes("matrix\n"), document }),
    registry: c5Registry({ kind: "worktree" }, {
      current_guards: options.guards ?? [],
      roadmap_citations: options.citations ?? [],
    }),
  });
}

function c5LegacyPair(shadow: boolean, selected = true) {
  const source = c5LegacySource();
  const reservation = c5Reservation(C5_ID, source);
  const state = shadow ? "shadow" as const : "legacy_markdown" as const;
  return {
    reservation,
    revision: c5WithDebt({
      campaign: c5Campaign(state, "legacy_markdown", [reservation], selected ? [c5Selection(C5_ID, "legacy_markdown_reservation")] : []),
      retired: c5Retired(),
      roadmaps: c5Snapshots(
        { markdown: source, ...(shadow ? { document: c5V0("matrix", source, C5_ID) } : {}) },
        { markdown: bytes("testing\n") },
      ),
      registry: c5Registry({ kind: "commit", commit: C5_BASE }, { roadmap_citations: [{
        id: C5_ID,
        source: "README.md" as RepoPath,
        span: { start_byte: 0, end_byte: 28 },
        raw: `roadmap:${C5_ID}`,
      }] }),
      legacy_title_bindings: [c5Title(reservation, source)],
    }),
  };
}

function c5LegacyCandidate(
  shadow: boolean,
  mutation: "none" | "selection" | "reservation" | "bytes" | "shadow" | "active" | "guard" | "stale_shadow" | "missing_tombstone" | "wrong_commit" | "citation" = "none",
) {
  const baseSource = c5LegacySource();
  const source = mutation === "shadow" ? baseSource
    : mutation === "bytes" ? bytes("moved-prefix\n## Lifecycle\nlegacy body\n")
    : mutation === "reservation" ? bytes("## Lifecycle\nrewritten open item\n")
    : bytes("delivered\n");
  const reservation = c5Reservation(C5_ID, baseSource);
  const retainedReservation = mutation === "reservation" ? [c5Reservation(C5_ID, source)] : [];
  const state = mutation === "active" ? "authoritative" as const : shadow ? "shadow" as const : "legacy_markdown" as const;
  const matrixDocument = mutation === "active"
    ? c5V1("matrix", [c5ReadyRecord(C5_ID)])
    : shadow
    ? c5V0("matrix", source, mutation === "shadow" ? C5_ID : undefined)
    : undefined;
  if (mutation === "stale_shadow" && matrixDocument !== undefined) {
    matrixDocument.document.frozen_source_sha256 = "0".repeat(64);
  }
  const guard: CurrentGuard | undefined = mutation === "guard" ? {
    id: C5_ID,
    replacement_pin: { kind: "gate", gate_id: "roadmap_projection_check", claim_md: bytes("guard") },
    owner_registry: "fixture-guards",
  } : undefined;
  return c5WithDebt({
    campaign: c5Campaign(state, "legacy_markdown", retainedReservation, mutation === "selection" ? [c5Selection(C5_ID, "legacy_markdown_reservation")] : []),
    retired: c5Retired(mutation === "missing_tombstone" ? [] : [c5Tombstone(C5_ID, mutation === "wrong_commit" ? "f".repeat(40) as FullCommitId : C5_BASE)]),
    roadmaps: c5Snapshots(
      { markdown: source, ...(matrixDocument === undefined ? {} : { document: matrixDocument }) },
      { markdown: bytes("testing\n") },
    ),
    registry: c5Registry({ kind: "worktree" }, {
      current_guards: guard === undefined ? [] : [guard],
      roadmap_citations: mutation === "citation" ? [{
        id: C5_ID,
        source: "README.md" as RepoPath,
        span: { start_byte: 0, end_byte: 28 },
        raw: `roadmap:${C5_ID}`,
      }] : [],
    }),
    legacy_title_bindings: mutation === "reservation"
      ? [c5Title(retainedReservation[0]!, source)]
      : [],
  });
}

function assertIssue(values: readonly { code: string; logical_path?: string }[], code: string, message: string): void {
  const matched = values.find((value) => value.code === code);
  assert(matched !== undefined, `${message}: ${JSON.stringify(values)}`);
  if (matched.logical_path !== undefined) {
    observeSelfTestIssue(matched as { code: import("../errors.ts").IssueCode; logical_path: string });
  }
}

function c5CampaignCase(id: C5SelfTestCaseId): boolean {
  if (!id.startsWith("campaign_")) return false;
  const matrixDocument = c5V1("matrix", [c5ReadyRecord(C5_ID)]);
  const testingDocument = c5V1("testing", [c5ReadyRecord(C5_TESTING_ID)]);
  const direct = (roadmap: RoadmapName) => {
    const roadmaps = c5Snapshots(
      roadmap === "matrix"
        ? { markdown: bytes("matrix\n"), document: matrixDocument }
        : { markdown: bytes("matrix\n"), document: c5V1("matrix") },
      roadmap === "testing"
        ? { markdown: bytes("testing\n"), document: testingDocument }
        : { markdown: bytes("testing\n") },
    );
    return validateLifecycleRevision({
      campaign: c5Campaign(
      "authoritative",
      roadmap === "testing" ? "authoritative" : "legacy_markdown",
      [],
      [c5Selection(roadmap === "matrix" ? C5_ID : C5_TESTING_ID)],
    ),
    retired: c5Retired(),
    roadmaps,
    registry: c5Registry({ kind: "worktree" }),
    debt: c5SnapshotDebt(roadmaps),
  });
  };
  switch (id) {
    case "campaign_direct_matrix":
    case "campaign_direct_testing":
      assert(direct(id.endsWith("matrix") ? "matrix" : "testing").issues.length === 0, "direct authoritative work selection must resolve");
      return true;
    case "campaign_programmatic_impossible_authority_tuple": {
      const impossible = c5Campaign(
        "legacy_markdown",
        "authoritative",
        [],
        [{ ...c5Selection(C5_ID), target_kind: "active_id", cycle: "cycle-b" as CampaignDocumentV1["selections"][number]["cycle"] }],
      );
      const roadmaps = c5Snapshots(
        { markdown: bytes("matrix\n") },
        { markdown: bytes("testing\n"), document: testingDocument },
      );
      const directResult = validateCampaign({ campaign: impossible, roadmaps });
      assert(directResult.issues.length === 1,
        `direct campaign tuple rejection must precede owner/selection work: ${JSON.stringify(directResult.issues)}`);
      assertIssue(directResult.issues, "E-SCHEMA-STATE", "direct campaign validation must reject an impossible authority tuple");
      assert(directResult.issues[0]!.logical_path === "campaign.testing_authority" &&
        directResult.owners.length === 0 && campaignIdentityOwners(directResult) === undefined,
      "impossible direct campaign must fail at the typed tuple coordinate without minting owners");

      const lifecycleResult = validateLifecycleRevision(c5WithDebt({
        campaign: impossible,
        retired: c5Retired(),
        roadmaps,
        registry: c5Registry({ kind: "worktree" }, {
          current_guards: [{
            id: C5_ID,
            replacement_pin: { kind: "gate", gate_id: "missing-gate", claim_md: bytes("missing") },
            owner_registry: "fixture-guards",
          }],
        }),
      }));
      assert(lifecycleResult.issues.length === 1,
        `lifecycle tuple rejection must precede owner/guard assembly: ${JSON.stringify(lifecycleResult.issues)}`);
      assertIssue(lifecycleResult.issues, "E-SCHEMA-STATE", "lifecycle validation must reject an impossible authority tuple");
      assert(lifecycleResult.issues[0]!.logical_path === "campaign.testing_authority" &&
        lifecycleResult.identity.owners.size === 0,
      "impossible lifecycle campaign must fail at the typed tuple coordinate before identity ownership");

      const validBaseDocument = c5Campaign("authoritative", "legacy_markdown", [], [c5Selection(C5_ID)]);
      const validBase = validateCampaign({
        campaign: validBaseDocument,
        roadmaps: c5Snapshots({ markdown: bytes("matrix\n"), document: matrixDocument }),
      });
      const transitionCandidate: CampaignValidationResult = Object.freeze({
        ...directResult,
        selections: new Map([[C5_ID, impossible.selections[0]!]]),
      });
      const transitionIssues = validateCampaignTransition({
        base: validBase,
        candidate: transitionCandidate,
        base_document: validBaseDocument,
        candidate_document: impossible,
        against: C5_BASE,
      });
      assert(transitionIssues.length === 1,
        `transition tuple rejection must precede authority deltas and selection rewrites: ${JSON.stringify(transitionIssues)}`);
      assertIssue(transitionIssues, "E-SCHEMA-STATE", "campaign transition validation must reject an impossible authority tuple");
      assert(transitionIssues[0]!.logical_path === "campaign.testing_authority",
        "impossible transition campaign must fail at the typed tuple coordinate");
      return true;
    }
    case "campaign_legacy_matrix":
    case "campaign_legacy_testing": {
      const roadmap = id.endsWith("matrix") ? "matrix" : "testing";
      const target = roadmap === "matrix" ? C5_ID : C5_TESTING_ID;
      const source = c5LegacySource();
      const reservation = c5Reservation(target, source);
      const result = validateCampaign({
        campaign: c5Campaign("legacy_markdown", "legacy_markdown", [reservation], [c5Selection(target, "legacy_markdown_reservation")]),
        roadmaps: c5Snapshots(
          { markdown: roadmap === "matrix" ? source : bytes("matrix\n") },
          { markdown: roadmap === "testing" ? source : bytes("testing\n") },
        ),
        legacy_title_bindings: [c5Title(reservation, source)],
      });
      assert(result.issues.length === 0, "legacy reservation selection must resolve against exact reviewed bytes");
      return true;
    }
    case "campaign_legacy_digest_title_span": {
      const { reservation, revision } = c5LegacyPair(false);
      const mutations = [
        { ...reservation, source_title: "Wrong" },
        { ...reservation, source_start_byte: 1 },
        { ...reservation, source_sha256: "0".repeat(64) },
      ];
      for (const mutated of mutations) {
        const invalid = validateCampaign({
          campaign: c5Campaign("legacy_markdown", "legacy_markdown", [mutated], [c5Selection(C5_ID, "legacy_markdown_reservation")]),
          roadmaps: revision.roadmaps,
          legacy_title_bindings: [c5Title(reservation, revision.roadmaps.matrix.markdown)],
        });
        assertIssue(invalid.issues, "E-CAMPAIGN-TARGET", "one title/range/span mutation must fail");
        assert(!invalid.owners.some((owner) => owner.id === C5_ID), "invalid binding must fail closed before minting an identity owner");
      }
      const forged = { ...c5Title(reservation, revision.roadmaps.matrix.markdown) };
      const forgedResult = validateCampaign({
        campaign: revision.campaign,
        roadmaps: revision.roadmaps,
        legacy_title_bindings: [forged],
      });
      assertIssue(forgedResult.issues, "E-CAMPAIGN-TARGET", "structurally matching caller-created title fact must not carry reviewed provenance");
      for (const mutation of ["owner", "digest", "range", "title", "index", "exhaustiveness"] as const) {
        const shadowPair = c5LegacyPair(true);
        const document = shadowPair.revision.roadmaps.matrix.document as RoadmapDocumentV0;
        const span = document.spans[0]!;
        if (mutation === "title") document.records[0]!.title = "Wrong title";
        else if (mutation === "index") document.manifest = [];
        else if (mutation === "exhaustiveness") {
          document.spans.push({
            ...span,
            id: "unclaimed-lifecycle" as RoadmapDocumentV0["spans"][number]["id"],
          });
        }
        else {
          document.spans[0] = mutation === "owner"
            ? { ...span, owner_id: "matrix.fixture-wrong" as RoadmapId }
            : mutation === "digest"
            ? { ...span, sha256: "0".repeat(64) }
            : { ...span, end_byte: span.end_byte - 1 };
        }
        assertIssue(validateCampaign({
          campaign: shadowPair.revision.campaign,
          roadmaps: shadowPair.revision.roadmaps,
          legacy_title_bindings: shadowPair.revision.legacy_title_bindings,
        }).issues, "E-CAMPAIGN-TARGET", `shadow ${mutation} mutation must invalidate exact corroboration`);
      }
      return true;
    }
    case "campaign_legacy_whole_digest": {
      const pair = c5LegacyPair(false);
      const bad = { ...pair.reservation, whole_source_sha256: "0".repeat(64) };
      assertIssue(validateCampaign({
        campaign: c5Campaign("legacy_markdown", "legacy_markdown", [bad]),
        roadmaps: pair.revision.roadmaps,
        legacy_title_bindings: [c5Title(pair.reservation, pair.revision.roadmaps.matrix.markdown)],
      }).issues, "E-CAMPAIGN-TARGET", "stale whole-source digest must fail");
      return true;
    }
    case "campaign_expire_matrix_at_v1":
    case "campaign_expire_testing_at_v1": {
      const roadmap = id.includes("matrix") ? "matrix" : "testing";
      const target = roadmap === "matrix" ? C5_ID : C5_TESTING_ID;
      const source = c5LegacySource();
      const reservation = c5Reservation(target, source);
      const result = validateCampaign({
        campaign: c5Campaign("authoritative", roadmap === "testing" ? "authoritative" : "legacy_markdown", [reservation]),
        roadmaps: c5Snapshots(
          roadmap === "matrix" ? { markdown: source, document: matrixDocument } : { markdown: bytes("matrix\n"), document: c5V1("matrix") },
          roadmap === "testing" ? { markdown: source, document: testingDocument } : { markdown: bytes("testing\n") },
        ),
        legacy_title_bindings: [c5Title(reservation, source)],
      });
      assertIssue(result.issues, "E-CAMPAIGN-TARGET-EXPIRED", "authoritative namespace must reject reservation");
      return true;
    }
    case "campaign_testing_survives_matrix_cutover": {
      const source = c5LegacySource();
      const reservation = c5Reservation(C5_TESTING_ID, source);
      const result = validateCampaign({
        campaign: c5Campaign("authoritative", "legacy_markdown", [reservation], [c5Selection(C5_TESTING_ID, "legacy_markdown_reservation")]),
        roadmaps: c5Snapshots({ markdown: bytes("matrix\n"), document: matrixDocument }, { markdown: source }),
        legacy_title_bindings: [c5Title(reservation, source)],
      });
      assert(result.issues.length === 0, "testing reservation must survive matrix-only cutover");
      return true;
    }
    case "campaign_direct_requires_active_v1": {
      const source = c5LegacySource();
      const shadow = c5V0("matrix", source, C5_ID);
      for (const snapshots of [
        c5Snapshots({ markdown: source, document: shadow }),
        c5Snapshots({ markdown: source }),
      ]) {
        const result = validateCampaign({
          campaign: c5Campaign("shadow", "legacy_markdown", [], [c5Selection(C5_ID)]),
          roadmaps: snapshots,
        });
        assertIssue(result.issues, "E-CAMPAIGN-TARGET", "absent/v0 source must not satisfy active_id");
      }
      return true;
    }
    case "campaign_active_work_only": {
      const family = c5V1("matrix", [c5FamilyRecord(C5_ID)]);
      const decision: SemanticAuthorityRecordV1 = {
        ...c5ReadyRecord(C5_ID),
        payload: {
          kind: "decision", summary_md: bytes("decision"), decision_state: "decided",
          rationale_md: bytes("rationale"), authority_reference_id: "authority" as ReferenceId,
          permanence: "permanent",
        },
      };
      const raw: RoadmapDocumentV1["records"][number] = {
        id: C5_ID, title: "Raw", projection_group: "fixture" as RoadmapDocumentV1["records"][number]["projection_group"],
        render_authority: "raw", source_block_md: bytes("raw"), span_ids: [],
      };
      const guard: CurrentGuard = {
        id: C5_ID,
        replacement_pin: { kind: "gate", gate_id: "roadmap_projection_check", claim_md: bytes("pin") },
        owner_registry: "fixture-guards",
      };
      for (const result of [
        validateLifecycleRevision(c5ActiveRevision(family, [c5Selection(C5_ID)])),
        validateLifecycleRevision(c5ActiveRevision(c5V1("matrix", [decision]), [c5Selection(C5_ID)])),
        validateLifecycleRevision(c5ActiveRevision(c5V1("matrix"), [c5Selection(C5_ID)], { guards: [guard] })),
        validateLifecycleRevision(c5ActiveRevision(c5V1("matrix"), [c5Selection(C5_ID)], { retired: [c5Tombstone(C5_ID)] })),
        validateLifecycleRevision(c5ActiveRevision(c5V1("matrix", [raw]), [c5Selection(C5_ID)])),
        validateLifecycleRevision(c5ActiveRevision(c5V1("matrix"), [c5Selection(C5_ID)])),
      ]) assertIssue(result.issues, "E-CAMPAIGN-TARGET", "family/guard/tombstone/missing ID must not be a campaign work target");
      const shadowSource = bytes("Lifecycle legacy body\n");
      assertIssue(validateCampaign({
        campaign: c5Campaign("shadow", "legacy_markdown", [], [c5Selection(C5_ID)]),
        roadmaps: c5Snapshots({ markdown: shadowSource, document: c5V0("matrix", shadowSource, C5_ID) }),
      }).issues, "E-CAMPAIGN-TARGET", "shadow-only owner must not be a campaign work target");
      return true;
    }
    case "campaign_unique_selection": {
      const progress = { ...c5Selection(C5_ID), selected_state: "in_progress" as const, assignee: "owner", pickup_commit: C5_BASE };
      for (const rows of [[c5Selection(C5_ID), c5Selection(C5_ID)], [c5Selection(C5_ID), progress]]) {
        const result = validateLifecycleRevision(c5ActiveRevision(matrixDocument, rows));
        assertIssue(result.issues, "E-CAMPAIGN-DUPLICATE", "duplicate/conflicting-state selections must fail");
      }
      return true;
    }
    case "campaign_state_fields": {
      for (const bad of [
        { ...c5Selection(C5_ID), selected_state: "in_progress" as const },
        { ...c5Selection(C5_ID), selected_state: "in_progress" as const, assignee: "maintainer" },
        { ...c5Selection(C5_ID), pickup_commit: C5_BASE },
      ]) {
        const result = validateLifecycleRevision(c5ActiveRevision(matrixDocument, [bad]));
        assertIssue(result.issues, "E-CAMPAIGN-STATE", "one selected/in-progress field mutation must fail");
      }
      return true;
    }
    case "campaign_state_transition": {
      const base = validateLifecycleRevision(c5ActiveRevision(matrixDocument, [c5Selection(C5_ID)]));
      const progress = {
        ...c5Selection(C5_ID), selected_state: "in_progress" as const, assignee: "maintainer",
        pickup_commit: C5_BASE,
      };
      const candidate = validateLifecycleRevision(c5ActiveRevision(matrixDocument, [progress]));
      assert(base.campaign !== undefined && candidate.campaign !== undefined, "campaigns must validate");
      assert(validateCampaignTransition({
        base: base.campaign, candidate: candidate.campaign,
        base_document: base.campaign_document!, candidate_document: candidate.campaign_document!, against: C5_BASE,
      }).length === 0, "selected -> in_progress must pass");
      const back = validateCampaignTransition({
        base: candidate.campaign, candidate: base.campaign,
        base_document: candidate.campaign_document!, candidate_document: base.campaign_document!, against: C5_BASE,
      });
      assert(back.length === 0, "in_progress -> selected must pass");
      assert(validateCampaignTransition({
        base: candidate.campaign,
        candidate: validateLifecycleRevision(c5ActiveRevision(matrixDocument)).campaign!,
        base_document: candidate.campaign_document!,
        candidate_document: c5ActiveRevision(matrixDocument).campaign,
        against: C5_BASE,
      }).length === 0, "either state -> removal must pass when the active owner remains");
      const invalidDocument = c5Campaign("authoritative", "legacy_markdown", [], [{ ...progress, cycle: "cycle-b" as typeof progress.cycle }]);
      const invalid = validateCampaign({ campaign: invalidDocument, roadmaps: c5Snapshots({ markdown: bytes("matrix\n"), document: matrixDocument }) });
      assertIssue(validateCampaignTransition({
        base: candidate.campaign, candidate: invalid,
        base_document: candidate.campaign_document!, candidate_document: invalidDocument, against: C5_BASE,
      }), "E-CAMPAIGN-TRANSITION", "state transition cannot rewrite invariant selection fields");
      return true;
    }
    case "campaign_deselect_keeps_reservation":
    case "campaign_reservation_owns_id_without_selection":
    case "campaign_deselect_active_allowed": {
      if (id === "campaign_deselect_active_allowed") {
        const base = validateLifecycleRevision(c5ActiveRevision(matrixDocument, [c5Selection(C5_ID)]));
        const candidate = validateLifecycleRevision(c5ActiveRevision(matrixDocument));
        assert(base.campaign !== undefined && candidate.campaign !== undefined && validateCampaignTransition({
          base: base.campaign, candidate: candidate.campaign,
          base_document: base.campaign_document!, candidate_document: candidate.campaign_document!, against: C5_BASE,
        }).length === 0, "active deselection must pass while owner remains");
      } else {
        const pair = c5LegacyPair(false, false);
        const result = validateLifecycleRevision(pair.revision);
        assert(result.issues.length === 0 && result.identity.owners.has(C5_ID), "reservation must own ID without selection");
        if (id === "campaign_deselect_keeps_reservation") {
          const selectedPair = c5LegacyPair(false, true);
          const selected = validateLifecycleRevision(selectedPair.revision);
          assert(selected.campaign !== undefined && result.campaign !== undefined && validateCampaignTransition({
            base: selected.campaign, candidate: result.campaign,
            base_document: selectedPair.revision.campaign,
            candidate_document: pair.revision.campaign,
            against: C5_BASE,
          }).length === 0, "legacy deselection must preserve its independently owning reservation");
        }
      }
      return true;
    }
    case "campaign_fired_promotion_visible": {
      // The all-fields fixture already proves domain validity; here a minimal semantic graph proves
      // campaign visibility from the decoded evaluation/transition join.
      const signalId = "matrix.fixture-promotion" as RoadmapId;
      const armed: RoadmapDocumentV1["records"][number] = {
        ...c5ReadyRecord(C5_ID),
        payload: {
          kind: "work", summary_md: bytes("armed"), work_state: "armed",
          work_intent: "build_capability", work_kind: "feature", risk: "compile_failure",
          family_classification: "none_reviewed", control_ids: [], transition_ids: [signalId],
        },
      };
      const signal: RoadmapDocumentV1["records"][number] = {
        ...c5ReadyRecord(signalId),
        payload: {
          kind: "signal", summary_md: bytes("fire"), transition_kind: "promotion_trigger",
          observer: "fixture", dimension: "state", observable: "done", predicate_kind: "event",
          current_evidence_ids: [], action_on_fire_md: bytes("select"), evaluation: "met",
          predicate: { predicate_kind: "event", event_md: bytes("event"), evidence_ids: [] },
        },
      };
      const result = validateLifecycleRevision(c5ActiveRevision(c5V1("matrix", [armed, signal])));
      assertIssue(result.issues, "E-CAMPAIGN-FIRED-HIDDEN", "fired promotion must be selected/actionable");
      const visible = validateLifecycleRevision(c5ActiveRevision(c5V1("matrix", [armed, signal]), [c5Selection(C5_ID)]));
      assert(visible.issues.length === 0, "the same fired promotion must pass when visible in the actionable campaign");
      return true;
    }
    case "campaign_allowlist_exhaustive":
    case "campaign_allowlist_stale_rejected": {
      const unresolved = [{ source: C5_ID, target: C5_TESTING_ID }];
      const allowlist = [{ ...unresolved[0]!, expires_when: "testing_authoritative" as const }];
      const campaign = c5Campaign("authoritative", id.endsWith("stale_rejected") ? "authoritative" : "legacy_markdown");
      const result = validateCampaign({
        campaign,
        roadmaps: c5Snapshots(
          { markdown: bytes("matrix\n"), document: matrixDocument },
          id.endsWith("stale_rejected") ? { markdown: bytes("testing\n"), document: testingDocument } : { markdown: bytes("testing\n") },
        ),
        unresolved_cross_roadmap: unresolved,
        cross_roadmap_allowlist: allowlist,
      });
      if (id.endsWith("stale_rejected")) assertIssue(result.issues, "E-CAMPAIGN-TARGET-EXPIRED", "stale allowlist must fail");
      else {
        assert(result.issues.length === 0, "exact typed allowlist must pass");
        const reversed = validateCampaign({
          campaign,
          roadmaps: c5Snapshots({ markdown: bytes("matrix\n"), document: matrixDocument }),
          unresolved_cross_roadmap: [...unresolved].reverse(),
          cross_roadmap_allowlist: [...allowlist].reverse(),
        });
        assert(JSON.stringify(result.issues) === JSON.stringify(reversed.issues), "allowlist validation must be deterministic under fact reversal");
        for (const mutated of [[], [...allowlist, allowlist[0]!], [...allowlist, { ...allowlist[0]!, target: C5_ID }]]) {
          assert(validateCampaign({
            campaign,
            roadmaps: c5Snapshots({ markdown: bytes("matrix\n"), document: matrixDocument }),
            unresolved_cross_roadmap: unresolved,
            cross_roadmap_allowlist: mutated,
          }).issues.length > 0, "missing, duplicate, and stale-extra allowlist mutations must fail");
        }
      }
      return true;
    }
    case "campaign_reservation_work_kind_required": {
      const pair = c5LegacyPair(true);
      const active = c5V1("matrix", [c5ReadyRecord(C5_ID, "defect")]);
      const candidate = c5ActiveRevision(active, [c5Selection(C5_ID)]);
      const result = validateTransaction({ scope: "all", against: C5_BASE, base: pair.revision, candidate });
      assertIssue(result.issues, "E-TRANSACTION-OWNER", "cutover work kind mismatch must fail");
      return true;
    }
    case "campaign_selection_target_kind_matches_owner": {
      const result = validateLifecycleRevision(c5ActiveRevision(matrixDocument, [c5Selection(C5_ID, "legacy_markdown_reservation")]));
      assertIssue(result.issues, "E-CAMPAIGN-TARGET", "selection tag must match normalized owner");
      const source = c5LegacySource();
      const reservation = c5Reservation(C5_ID, source);
      const invalid = validateCampaign({
        campaign: c5Campaign(
          "legacy_markdown",
          "legacy_markdown",
          [reservation],
          [c5Selection(C5_ID, "active_id")],
        ),
        roadmaps: c5Snapshots({ markdown: source }),
        legacy_title_bindings: [c5Title(reservation, source)],
      });
      assertIssue(invalid.issues, "E-CAMPAIGN-TARGET",
        "valid reservation evidence must not excuse an invalid active_id selection");
      assert(invalid.owners.length === 1 && campaignIdentityOwners(invalid) === undefined,
        "campaign owner capability must remain unexposed when any independent campaign issue exists");
      assertIssue(validateGlobalIdentity({
        documents: [],
        additional_owners: { owner_count: invalid.owners.length } as never,
      }).issues, "E-OWNER-DUPLICATE", "structural substitute for an invalid campaign result must not install owners");
      const invalidCapability = campaignIdentityOwners(invalid);
      const undefinedIdentity = validateGlobalIdentity({
        documents: [],
        ...(invalidCapability === undefined ? {} : { additional_owners: invalidCapability }),
      });
      assert(undefinedIdentity.owners.size === 0,
        "undefined capability from an invalid campaign result must not install owners");

      const valid = validateCampaign({
        campaign: c5Campaign(
          "legacy_markdown",
          "legacy_markdown",
          [reservation],
          [c5Selection(C5_ID, "legacy_markdown_reservation")],
        ),
        roadmaps: c5Snapshots({ markdown: source }),
        legacy_title_bindings: [c5Title(reservation, source)],
      });
      const validCapability = campaignIdentityOwners(valid);
      assert(valid.issues.length === 0 && validCapability !== undefined && validateGlobalIdentity({
        documents: [], additional_owners: validCapability,
      }).owners.get(C5_ID)?.owner_kind === "legacy_markdown_reservation",
      "issue-free campaign result must retain its valid opaque capability path");
      return true;
    }
    case "campaign_new_legacy_selection_is_atomic": {
      const base = c5LegacyPair(false, false);
      const baseValidated = validateLifecycleRevision(base.revision);
      const candidateDoc = c5Campaign("legacy_markdown", "legacy_markdown", [], [c5Selection(C5_ID, "legacy_markdown_reservation")]);
      const candidateValidated = validateCampaign({ campaign: candidateDoc, roadmaps: base.revision.roadmaps });
      assert(baseValidated.campaign !== undefined, "base campaign must validate");
      assertIssue(validateCampaignTransition({
        base: baseValidated.campaign, candidate: candidateValidated,
        base_document: base.revision.campaign, candidate_document: candidateDoc, against: C5_BASE,
      }), "E-CAMPAIGN-TRANSITION", "new legacy selection without reservation must fail atomically");
      return true;
    }
    case "campaign_reservation_rebinds_whole_source": {
      const source = bytes("## Lifecycle\nlegacy body\nsecond\n");
      const a = c5Reservation(C5_ID, source);
      const b = c5Reservation("matrix.fixture-second" as RoadmapId, source);
      const stale = { ...b, whole_source_sha256: "0".repeat(64) };
      const result = validateCampaign({
        campaign: c5Campaign("legacy_markdown", "legacy_markdown", [a, stale]),
        roadmaps: c5Snapshots({ markdown: source }),
        legacy_title_bindings: [c5Title(a, source), c5Title(b, source)],
      });
      assertIssue(result.issues, "E-CAMPAIGN-TARGET", "every surviving reservation must repeat the rebased whole digest");
      return true;
    }
  }
  return false;
}

function c5RetiredCase(id: C5SelfTestCaseId): boolean {
  if (!id.startsWith("retired_")) return false;
  const valid = c5Tombstone(C5_ID);
  switch (id) {
    case "retired_bad_hash_length_case": {
      for (const hash of ["abc", "g".repeat(40), "A".repeat(40), "a".repeat(39), "a".repeat(41), "a".repeat(63), "a".repeat(65)]) {
        const result = validateRetiredIds(c5Retired([{ ...valid, last_active_at: hash as FullCommitId }]), fakeRegistryView());
        assertIssue(result.issues, "E-RETIRED-HASH", `invalid object-format hash ${hash.length} must fail`);
      }
      return true;
    }
    case "retired_missing_replacement": {
      const result = validateRetiredIds(c5Retired([{ ...valid, replacement: { kind: "gate", gate_id: "missing", claim_md: bytes("pin") } }]), fakeRegistryView());
      assertIssue(result.issues, "E-RETIRED-REPLACEMENT", "missing replacement must fail");
      return true;
    }
    case "retired_roadmap_replacement_rejected": {
      assert(!REPLACEMENT_PIN_KINDS.some((kind) => (kind as string) === "roadmap"), "roadmap is not a replacement-pin arm");
      const result = validateRetiredIds(c5Retired([{ ...valid, replacement: {
        kind: "file_heading", path: "draft/roadmap.md" as RepoPath, heading: citationText("matrix.fixture-lifecycle"), claim_md: bytes("pin"),
      } }]), fakeRegistryView());
      assertIssue(result.issues, "E-RETIRED-REPLACEMENT", "roadmap/draft replacement must fail");
      return true;
    }
    case "retired_unresolved_replacement": {
      const view = fakeRegistryView();
      assert(!REPLACEMENT_PIN_KINDS.some((kind) => ["external_issue", "bare_title", "unresolved"].includes(kind)), "external, bare-title, and unresolved pins are not replacement arms");
      const pins = [
        { kind: "gate" as const, gate_id: "missing", claim_md: bytes("pin") },
        { kind: "test_symbol" as const, test_id: "missing", symbol: "tests::missing", claim_md: bytes("pin") },
        { kind: "file_heading" as const, path: "README.md" as RepoPath, heading: "Missing", claim_md: bytes("pin") },
        { kind: "file_heading" as const, path: "draft/note.md" as RepoPath, heading: "Note", claim_md: bytes("pin") },
      ];
      for (const replacement of pins) {
        assertIssue(validateRetiredIds(c5Retired([{ ...valid, replacement }]), view).issues, "E-RETIRED-REPLACEMENT", `${replacement.kind} unresolved replacement must fail`);
      }
      const duplicate = fakeRegistryView({ gates: [gateFact("roadmap_projection_check"), gateFact("roadmap_projection_check")] });
      assertIssue(validateRetiredIds(c5Retired([valid]), duplicate).issues, "E-RETIRED-REPLACEMENT", "duplicate replacement provider must fail exact-one resolution");
      return true;
    }
    case "retired_gate_stub": {
      const view = fakeRegistryView({ gates: [gateFact("roadmap_projection_check", true)] });
      assertIssue(validateRetiredIds(c5Retired([valid]), view).issues, "E-RETIRED-REPLACEMENT", "stub gate must fail");
      return true;
    }
    case "retired_preexisting_keeps_base": {
      const entries: RetiredIdV1[] = [
        valid,
        { id: "matrix.fixture-retired-test" as RoadmapId, last_active_at: C5_BASE, replacement: {
          kind: "test_symbol", test_id: "rust-test:cddl-codegen#tests::sample::works", symbol: "tests::sample::works", claim_md: bytes("test"),
        } },
        { id: "matrix.fixture-retired-heading" as RoadmapId, last_active_at: C5_BASE, replacement: {
          kind: "file_heading", path: "cddl-matrix/README.md" as RepoPath, heading: "What lives here", claim_md: bytes("heading"),
        } },
      ];
      const base = validateRetiredIds(c5Retired(entries), fakeRegistryView());
      const unchanged = validateRetiredIds(c5Retired([...entries].reverse()), fakeRegistryView());
      assert(validateRetiredTransition({ base, candidate: unchanged, against: C5_BASE, eligible_base_origins: [] }).length === 0, "preexisting gate/test/heading tombstones must remain immutable and order-independent");
      const changed = validateRetiredIds(c5Retired([{ ...valid, last_active_at: "f".repeat(40) as FullCommitId }, ...entries.slice(1)]), fakeRegistryView());
      assertIssue(validateRetiredTransition({ base, candidate: changed, against: C5_BASE, eligible_base_origins: [] }), "E-RETIRED-REUSE", "preexisting tombstone must preserve history");
      return true;
    }
    case "retired_new_last_active_matches_against": {
      const base = validateRetiredIds(c5Retired(), fakeRegistryView());
      const candidate = validateRetiredIds(c5Retired([valid]), fakeRegistryView());
      assert(validateRetiredTransition({
        base, candidate, against: C5_BASE,
        eligible_base_origins: [{ id: C5_ID, owner_kind: "active_record" }],
      }).length === 0, "new tombstone must accept exact --against");
      return true;
    }
    case "retired_wrong_against": {
      const base = validateRetiredIds(c5Retired(), fakeRegistryView());
      const candidate = validateRetiredIds(c5Retired([{ ...valid, last_active_at: "f".repeat(40) as FullCommitId }]), fakeRegistryView());
      assertIssue(validateRetiredTransition({
        base, candidate, against: C5_BASE,
        eligible_base_origins: [{ id: C5_ID, owner_kind: "active_record" }],
      }), "E-RETIRED-HASH", "wrong transaction base must fail");
      return true;
    }
    case "retired_test_symbol_requires_exact_id_and_symbol": {
      const pin = {
        kind: "test_symbol" as const,
        test_id: "rust-test:cddl-codegen#tests::sample::works",
        symbol: "tests::sample::wrong",
        claim_md: bytes("pin"),
      };
      assert(!resolveReplacementPin(pin, fakeRegistryView()).resolved, "test replacement must match exact derived tuple");
      assert(!resolveReplacementPin({ ...pin, test_id: "rust-test:cddl-codegen#tests::sample::wrong", symbol: "tests::sample::works" }, fakeRegistryView()).resolved, "test replacement must reject a separate test-ID mutation");
      const exact = { ...pin, symbol: "tests::sample::works" };
      assert(resolveReplacementPin(exact, fakeRegistryView()).resolved, "exact derived test tuple must resolve");
      const duplicate = fakeRegistryView({ test_symbols: [fakeRegistryView().test_symbols[0]!, fakeRegistryView().test_symbols[0]!] });
      assert(!resolveReplacementPin(exact, duplicate).resolved, "replacement resolution must be exact-one");
      return true;
    }
  }
  return false;
}

function c5IdentityReservationCase(id: C5SelfTestCaseId): boolean {
  if (!id.startsWith("identity_reservation_") && !id.startsWith("identity_shadow_") &&
    !id.startsWith("identity_evidence_")) return false;
  const pair = c5LegacyPair(true);
  const source = pair.revision.roadmaps.matrix.markdown;
  const shadowDocument = pair.revision.roadmaps.matrix.document as RoadmapDocumentV0;
  const campaign = validateCampaign({
    campaign: pair.revision.campaign,
    roadmaps: pair.revision.roadmaps,
    legacy_title_bindings: pair.revision.legacy_title_bindings,
  });
  assert(campaign.issues.length === 0, "reservation/shadow fixture must mint exact owner capabilities");
  const reservationOwner = campaign.owners.find((owner) => owner.owner_kind === "legacy_markdown_reservation")!;
  const shadowOwner = campaign.owners.find((owner) => owner.owner_kind === "shadow_record_reservation")!;
  const reservationEvidence: CampaignIdentityOwnerEvidence = {
    kind: "legacy_markdown_reservation",
    reservation: pair.reservation,
    markdown: source,
  };
  const shadowEvidence: CampaignIdentityOwnerEvidence = {
    kind: "shadow_record_reservation",
    id: C5_ID,
    namespace: "matrix",
    markdown: source,
    shadow_document: shadowDocument,
  };
  const reservationCapability = c5OwnerCapability([reservationEvidence]);
  const shadowCapability = c5OwnerCapability([shadowEvidence]);
  const pairCapability = campaignIdentityOwners(campaign);
  assert(pairCapability !== undefined, "validated campaign result must expose one opaque owner capability");
  const activeInput = buildRoadmapIndexes(c5V1("matrix", [c5ReadyRecord(C5_ID)])).indexes.identity_inputs;
  const guard: CurrentGuard = {
    id: C5_ID,
    replacement_pin: { kind: "gate", gate_id: "roadmap_projection_check", claim_md: bytes("pin") },
    owner_registry: "fixture-guards",
  };
  const tombstone = c5Tombstone(C5_ID);
  switch (id) {
    case "identity_reservation_cross_namespace_collision": {
      const invalid = validateCampaignIdentityOwnerEvidence([{
        ...reservationEvidence,
        reservation: { ...pair.reservation, id: C5_TESTING_ID },
      }]);
      assert(!invalid.ok && invalid.issues[0]?.code === "E-OWNER-DUPLICATE", "namespace/path mismatch must fail before capability minting");
      observeMatchingIssue(invalid.issues, "E-OWNER-DUPLICATE");
      return true;
    }
    case "identity_reservation_active_collision":
      assertIssue(validateGlobalIdentity({ documents: [activeInput], additional_owners: reservationCapability }).issues, "E-OWNER-DUPLICATE", "active/reservation collision must fail");
      return true;
    case "identity_reservation_tombstone_collision":
      assertIssue(validateGlobalIdentity({ documents: [], tombstones: [tombstone], additional_owners: reservationCapability }).issues, "E-OWNER-DUPLICATE", "reservation/tombstone collision must fail");
      return true;
    case "identity_shadow_record_reserves_id": {
      const result = validateGlobalIdentity({ documents: [], additional_owners: shadowCapability });
      assert(result.issues.length === 0 && result.owners.get(C5_ID)?.owner_kind === "shadow_record_reservation", "shadow-only v0 record must reserve but not activate ID");
      return true;
    }
    case "identity_reservation_shadow_coalesces": {
      const result = validateGlobalIdentity({ documents: [], additional_owners: pairCapability });
      assert(result.issues.length === 0 && result.owners.get(C5_ID)?.owner_kind === "legacy_markdown_reservation", "exact reservation/shadow pair must coalesce");
      const reversed = validateGlobalIdentity({
        documents: [],
        additional_owners: c5OwnerCapability([shadowEvidence, reservationEvidence]),
      });
      assert(JSON.stringify([...result.owners].map(([key, value]) => [key, identityOwnerClaimKey(value)])) === JSON.stringify([...reversed.owners].map(([key, value]) => [key, identityOwnerClaimKey(value)])), "coalescence must be deterministic under owner reversal");
      return true;
    }
    case "identity_evidence_digest_binary_inputs": {
      const viewA = createImmutableByteView(bytes("aa"));
      const viewB = createImmutableByteView(bytes("bb"));
      const firstView = inspectStableEvidenceDigest(viewA);
      const secondView = inspectStableEvidenceDigest(viewB);
      assert(
        firstView.digest !== secondView.digest && firstView.unique_binary_inputs === 1 &&
          secondView.unique_binary_inputs === 1,
        "equal-length immutable byte views with different bytes must have distinct evidence digests",
      );
      const mutable = bytes("aa");
      const beforeMutation = inspectStableEvidenceDigest(mutable).digest;
      mutable[0] = "b".charCodeAt(0);
      assert(
        inspectStableEvidenceDigest(mutable).digest !== beforeMutation,
        "mutating a raw byte input between evidence traversals must change its digest",
      );
      const shared = bytes("shared");
      const sharedInputs = inspectStableEvidenceDigest([
        shared,
        { raw: shared, view: viewA },
        viewA,
      ]);
      assert(
        sharedInputs.unique_binary_inputs === 2,
        `shared raw bytes and immutable views must each hash once per traversal, got ${sharedInputs.unique_binary_inputs}`,
      );
      return true;
    }
    case "identity_reservation_shadow_binding_mismatch": {
      for (const forged of [
        { ...pairCapability },
        [reservationOwner, shadowOwner],
        reservationOwner,
        shadowOwner,
      ]) {
        assertIssue(validateGlobalIdentity({ documents: [], additional_owners: forged as never }).issues, "E-OWNER-DUPLICATE", "structural clones, arrays, and direct sole R/S facts must lack opaque campaign provenance");
      }
      const mutableShadowSource = c5LegacySource();
      const mutableShadowDocument = c5V0("matrix", mutableShadowSource, C5_ID);
      const mutableShadowCapability = c5OwnerCapability([{
        kind: "shadow_record_reservation",
        id: C5_ID,
        namespace: "matrix",
        markdown: mutableShadowSource,
        shadow_document: mutableShadowDocument,
      }]);
      mutableShadowDocument.records[0]!.title = "Mutated after mint";
      assertIssue(validateGlobalIdentity({
        documents: [], additional_owners: mutableShadowCapability,
      }).issues, "E-OWNER-DUPLICATE", "post-mint shadow evidence mutation must invalidate capability provenance");
      const mutablePair = c5LegacyPair(false);
      const minted = validateCampaign({
        campaign: mutablePair.revision.campaign,
        roadmaps: mutablePair.revision.roadmaps,
        legacy_title_bindings: mutablePair.revision.legacy_title_bindings,
      }).owners.find((owner) => owner.owner_kind === "legacy_markdown_reservation")!;
      if (minted.owner_kind === "legacy_markdown_reservation") minted.reservation.source_sha256 = "0".repeat(64);
      assertIssue(validateLifecycleRevision(mutablePair.revision).issues, "E-CAMPAIGN-TARGET", "post-mint nested mutation must invalidate campaign provenance before transaction identity assembly");
      return true;
    }
    case "identity_reservation_shadow_third_owner_rejected": {
      const labels = ["A", "R", "S", "G", "T"] as const;
      for (let left = 0; left < labels.length; left += 1) {
        for (let right = left; right < labels.length; right += 1) {
          const a = labels[left]!;
          const b = labels[right]!;
          const ownerEvidence: CampaignIdentityOwnerEvidence[] = [];
          for (const label of [a, b]) {
            if (label === "R") ownerEvidence.push(reservationEvidence);
            if (label === "S") ownerEvidence.push(shadowEvidence);
          }
          const result = validateGlobalIdentity({
            documents: [a, b].filter((label) => label === "A").map(() => activeInput),
            ...(ownerEvidence.length === 0
              ? {}
              : { additional_owners: c5OwnerCapability(ownerEvidence) }),
            current_guards: [a, b].filter((label) => label === "G").map(() => guard),
            tombstones: [a, b].filter((label) => label === "T").map(() => tombstone),
          });
          const validPair = a === "R" && b === "S";
          assert(validPair ? result.issues.length === 0 : result.issues.some((value) => value.code === "E-OWNER-DUPLICATE"), `owner pair ${a}-${b} normalization differs`);
        }
      }
      const reversedPair = validateGlobalIdentity({
        documents: [],
        additional_owners: c5OwnerCapability([shadowEvidence, reservationEvidence]),
      });
      assert(reversedPair.issues.length === 0, "valid R-S pair must remain valid under reversal");
      for (const third of [
        { documents: [activeInput], additional_owners: pairCapability },
        { documents: [], additional_owners: c5OwnerCapability([reservationEvidence, shadowEvidence, reservationEvidence]) },
        { documents: [], additional_owners: c5OwnerCapability([reservationEvidence, shadowEvidence, shadowEvidence]) },
        { documents: [], additional_owners: pairCapability, current_guards: [guard] },
        { documents: [], additional_owners: pairCapability, tombstones: [tombstone] },
      ]) assertIssue(validateGlobalIdentity(third).issues, "E-OWNER-DUPLICATE", "each A/R/S/G/T third claim must invalidate coalesced pair");
      const aliasInput = {
        namespace: "matrix" as const,
        id_providers: [],
        alias_providers: [{ alias: C5_ID, namespace: "matrix" as const, owner_kind: "record" as const, owner_id: "matrix.fixture-alias-owner", logical_path: "fixture.alias" }],
      };
      for (const ownerInputs of [
        { documents: [activeInput, aliasInput] },
        { documents: [aliasInput], additional_owners: reservationCapability },
        { documents: [aliasInput], additional_owners: shadowCapability },
        { documents: [aliasInput], current_guards: [guard] },
        { documents: [aliasInput], tombstones: [tombstone] },
      ]) assertIssue(validateGlobalIdentity(ownerInputs).issues, "E-ALIAS-COLLISION", "alias must collide with every A/R/S/G/T owner kind");
      for (const forgedOwner of [
        { owner_kind: "active_record", id: C5_ID, namespace: "matrix", record: c5ReadyRecord(C5_ID) },
        { owner_kind: "current_guard", id: C5_ID, namespace: "matrix", guard },
        { owner_kind: "tombstone", id: C5_ID, namespace: "matrix", tombstone },
      ]) {
        assertIssue(validateGlobalIdentity({ documents: [], additional_owners: forgedOwner as never }).issues, "E-OWNER-DUPLICATE", "non-campaign owner must fail the additional-owner runtime channel");
      }
      return true;
    }
  }
  return false;
}

function withBaseRevision<T extends { readonly registry: RegistryView }>(value: T): T {
  return { ...value, registry: c5Registry({ kind: "commit", commit: C5_BASE }, {
    current_guards: value.registry.current_guards,
    roadmap_citations: value.registry.roadmap_citations,
  }) };
}

function c5AllTransaction(base: Parameters<typeof validateTransaction>[0] extends never ? never : ReturnType<typeof c5ActiveRevision>, candidate: ReturnType<typeof c5ActiveRevision>) {
  return validateTransaction({ scope: "all", against: C5_BASE, base: withBaseRevision(base), candidate });
}

function c5TransactionCase(id: C5SelfTestCaseId, context?: SelfTestContext): boolean {
  if (!id.startsWith("transaction_")) return false;
  const active = c5V1("matrix", [c5ReadyRecord(C5_ID)]);
  const empty = c5V1("matrix");
  const positiveRetirementInputs = () => {
    const other = "matrix.fixture-survivor" as RoadmapId;
    const baseDocument = c5V1("matrix", [c5ReadyRecord(C5_ID), c5ReadyRecord(other)], [
      { source: C5_ID, kind: "related", target: other },
    ], [{ id: "retiring-ref" as ReferenceId, source: C5_ID, kind: "roadmap", target_id: other }]);
    const candidateDocument = c5V1("matrix", [c5ReadyRecord(other)]);
    return {
      base: c5ActiveRevision(baseDocument, [c5Selection(C5_ID)], { citations: [{
        id: C5_ID, source: "README.md" as RepoPath,
        span: { start_byte: 0, end_byte: 28 }, raw: `roadmap:${C5_ID}`,
      }] }),
      candidate: c5ActiveRevision(candidateDocument, [], { retired: [c5Tombstone(C5_ID)] }),
    };
  };
  const positiveRetirement = () => {
    const inputs = positiveRetirementInputs();
    return c5AllTransaction(inputs.base, inputs.candidate);
  };
  switch (id) {
    case "transaction_complete_tombstone": {
      const result = positiveRetirement();
      assert(result.issues.length === 0 && result.retired_ids[0] === C5_ID, `complete active retirement must pass: ${JSON.stringify(result.issues)}`);
      const inputs = positiveRetirementInputs();
      for (const missing of ["base", "candidate"] as const) {
        const base = withBaseRevision(inputs.base);
        const transaction = validateTransaction({
          scope: "all",
          against: C5_BASE,
          base: missing === "base" ? { ...base, debt: {} } : base,
          candidate: missing === "candidate" ? { ...inputs.candidate, debt: {} } : inputs.candidate,
        });
        assert(transaction.issues.some((issue) => issue.code === "E-TRANSACTION-BASE" &&
          issue.logical_path === "debt.matrix"), `${missing} debt is mandatory for active retirement`);
      }
      return true;
    }
    case "transaction_complete_guard_transfer": {
      const workId = "matrix.fixture-linked-work" as RoadmapId;
      const familyBase = c5V1(
        "matrix",
        [c5FamilyRecord(C5_ID, [workId]), c5ReadyRecord(workId)],
        [{ source: C5_ID, kind: "parent_of", target: workId }],
        [{ id: "family-work" as ReferenceId, source: C5_ID, kind: "roadmap", target_id: workId }],
      );
      const candidateDocument = c5V1("matrix", [c5ReadyRecord(workId)]);
      const guards = c5FamilyGuards(C5_ID);
      const familyBaseRevision = c5ActiveRevision(familyBase, [], { citations: [{
          id: C5_ID, source: "README.md" as RepoPath,
          span: { start_byte: 0, end_byte: 28 }, raw: `roadmap:${C5_ID}`,
        }] });
      const familyCandidateRevision = c5ActiveRevision(candidateDocument, [], { guards });
      const result = c5AllTransaction(familyBaseRevision, familyCandidateRevision);
      assert(result.issues.length === 0 && result.guard_transfers[0] === C5_ID, `complete family guard transfer with surviving linked work must pass: ${JSON.stringify(result.issues)}`);
      for (const missing of ["base", "candidate"] as const) {
        const base = withBaseRevision(familyBaseRevision);
        const transaction = validateTransaction({
          scope: "all",
          against: C5_BASE,
          base: missing === "base" ? { ...base, debt: {} } : base,
          candidate: missing === "candidate"
            ? { ...familyCandidateRevision, debt: {} }
            : familyCandidateRevision,
        });
        assert(transaction.issues.some((issue) => issue.code === "E-TRANSACTION-BASE" &&
          issue.logical_path === "debt.matrix"), `${missing} debt is mandatory for family guard transfer`);
      }
      const debt = c5Debt(familyBase);
      const rootAtoms = [...debt.owners.values()].filter(({ key }) =>
        key.owner_kind === "record" && key.owner_id === C5_ID
      );
      assertExactStrings(rootAtoms.map(({ key }) => key.owner_field), [
        "payload.boundary_md",
        "payload.exclusions[0].reason_md",
        "payload.goal_md",
        "payload.summary_md",
      ], "family guard fixture exact removable record atoms");
      assertExactStrings(c5FamilyChildIds(C5_ID), [
        `${C5_ID}.fixture-dimension`, `${C5_ID}.fixture-choice`, `${C5_ID}.fixture-proof`,
        `${C5_ID}.fixture-point`, `${C5_ID}.fixture-excluded`,
      ], "family guard fixture systematic child denominator");
      const baseProviders = buildRoadmapIndexes(familyBase).indexes.identity_inputs.id_providers.filter((provider) =>
        provider.owner_record_id === C5_ID && provider.kind !== "record"
      );
      assertExactStrings(baseProviders.map((provider) => `${provider.id}:${provider.kind}`), [
        `${C5_ID}.fixture-dimension:family_axis`,
        `${C5_ID}.fixture-choice:family_axis_value`,
        `${C5_ID}.fixture-proof:family_evidence_requirement`,
        `${C5_ID}.fixture-point:family_cell`,
        `${C5_ID}.fixture-excluded:family_exclusion`,
      ], "family guard fixture exact systematic child IDs and provider kinds");
      const baseDebt = c5Debt(familyBase);
      const candidateDebt = c5Debt(candidateDocument);
      const registry = c5Registry({ kind: "worktree" }, { current_guards: guards });
      const request = {
        base_owner: {
          owner_kind: "active_record" as const,
          id: C5_ID,
          namespace: "matrix" as const,
          record: familyBase.records[0]!,
        },
        candidate_guard: {
          owner_kind: "current_guard" as const,
          id: C5_ID,
          namespace: "matrix" as const,
          guard: guards[0]!,
        },
        candidate_guards: guards,
        candidate_replacement_facts: [
          ...registry.gates, ...registry.test_symbols, ...registry.tracked_headings,
        ],
      };
      const capability = validateDebtGuardTransferFacts(
        baseDebt,
        candidateDebt,
        { base_document: familyBase, candidate_document: candidateDocument },
        [request],
      );
      assert(capability.ok, `exact guard-transfer debt capability must mint: ${JSON.stringify(capability.issues)}`);
      const replacementFamilyId = "matrix.fixture-replacement-family" as RoadmapId;
      const activeChildrenCandidate = c5V1("matrix", [
        c5FamilyRecordUsingChildIds(replacementFamilyId, c5FamilyChildIds(C5_ID)),
        c5ReadyRecord(workId),
      ]);
      const rootOnlyGuard = c5Guard(C5_ID);
      const activeChildrenRegistry = c5Registry({ kind: "worktree" }, { current_guards: [rootOnlyGuard] });
      const activeChildrenRequest = {
        ...request,
        candidate_guard: { ...request.candidate_guard, guard: rootOnlyGuard },
        candidate_guards: [rootOnlyGuard],
        candidate_replacement_facts: [
          ...activeChildrenRegistry.gates,
          ...activeChildrenRegistry.test_symbols,
          ...activeChildrenRegistry.tracked_headings,
        ],
      };
      const activeChildrenDebt = c5Debt(activeChildrenCandidate);
      const activeChildrenCapability = validateDebtGuardTransferFacts(
        baseDebt,
        activeChildrenDebt,
        { base_document: familyBase, candidate_document: activeChildrenCandidate },
        [activeChildrenRequest],
      );
      assert(activeChildrenCapability.ok && compareMigrationDebt(baseDebt, activeChildrenDebt, {
        base_document: familyBase,
        candidate_document: activeChildrenCandidate,
        transition_facts: activeChildrenCapability.facts,
      }).length === 0, "sameActiveKind must preserve every one of the five systematic provider kinds without child guards");
      const activeLifecycle = c5AllTransaction(
        c5ActiveRevision(familyBase),
        c5ActiveRevision(activeChildrenCandidate, [], { guards: [rootOnlyGuard] }),
      );
      assert(activeLifecycle.issues.length === 0, `sameActiveKind lifecycle arm must pass: ${JSON.stringify(activeLifecycle.issues)}`);
      const swappedKinds = [...c5FamilyChildIds(C5_ID)];
      [swappedKinds[0], swappedKinds[3]] = [swappedKinds[3]!, swappedKinds[0]!];
      const wrongKindCandidate = c5V1("matrix", [
        c5FamilyRecordUsingChildIds(replacementFamilyId, swappedKinds),
        c5ReadyRecord(workId),
      ]);
      const wrongKind = validateDebtGuardTransferFacts(
        baseDebt,
        c5Debt(wrongKindCandidate),
        { base_document: familyBase, candidate_document: wrongKindCandidate },
        [{ ...activeChildrenRequest }],
      );
      assert(!wrongKind.ok && wrongKind.issues[0]?.code === "E-DEBT-OWNER-REGRESSION",
        "same child IDs under wrong systematic provider kinds must not satisfy sameActiveKind");

      const exactPins = [
        { kind: "gate" as const, gate_id: "roadmap_projection_check", claim_md: bytes("gate") },
        {
          kind: "test_symbol" as const,
          test_id: "rust-test:cddl-codegen#tests::sample::works",
          symbol: "tests::sample::works",
          claim_md: bytes("test"),
        },
        {
          kind: "file_heading" as const,
          path: "cddl-matrix/README.md" as RepoPath,
          heading: "What lives here",
          claim_md: bytes("heading"),
        },
      ];
      for (const pin of exactPins) {
        const exactGuard = { ...rootOnlyGuard, replacement_pin: pin };
        const exact = validateDebtGuardTransferFacts(
          baseDebt,
          activeChildrenDebt,
          { base_document: familyBase, candidate_document: activeChildrenCandidate },
          [{
            ...activeChildrenRequest,
            candidate_guard: { ...request.candidate_guard, guard: exactGuard },
            candidate_guards: [exactGuard],
          }],
        );
        assert(exact.ok, `guard seam must resolve canonical ${pin.kind} replacement semantics itself`);
      }
      const invalidReplacementRequests = [
        {
          guard: { ...rootOnlyGuard, replacement_pin: { ...rootOnlyGuard.replacement_pin, claim_md: bytes("") } },
          facts: activeChildrenRequest.candidate_replacement_facts,
        },
        {
          guard: {
            ...rootOnlyGuard,
            replacement_pin: {
              kind: "file_heading" as const,
              path: "draft/review.md" as RepoPath,
              heading: "Draft",
              claim_md: bytes("draft"),
            },
          },
          facts: [headingFact("draft/review.md" as RepoPath, "Draft")],
        },
        {
          guard: rootOnlyGuard,
          facts: [gateFact("roadmap_projection_check"), gateFact("roadmap_projection_check")],
        },
        {
          guard: rootOnlyGuard,
          facts: [gateFact("roadmap_projection_check", true)],
        },
      ];
      for (const invalid of invalidReplacementRequests) {
        const rejected = validateDebtGuardTransferFacts(
          baseDebt,
          activeChildrenDebt,
          { base_document: familyBase, candidate_document: activeChildrenCandidate },
          [{
            ...activeChildrenRequest,
            candidate_guard: { ...request.candidate_guard, guard: invalid.guard },
            candidate_guards: [invalid.guard],
            candidate_replacement_facts: invalid.facts,
          }],
        );
        assert(!rejected.ok && rejected.issues[0]?.code === "E-DEBT-OWNER-REGRESSION",
          "empty claim, draft heading, duplicate provider, and stub gate must fail inside the debt seam");
      }
      const reversedInputs = validateDebtGuardTransferFacts(
        baseDebt,
        candidateDebt,
        { base_document: familyBase, candidate_document: candidateDocument },
        [{
          ...request,
          candidate_guards: [...request.candidate_guards].reverse(),
          candidate_replacement_facts: [...request.candidate_replacement_facts].reverse(),
        }],
      );
      assert(reversedInputs.ok && compareMigrationDebt(baseDebt, candidateDebt, {
        base_document: familyBase,
        candidate_document: candidateDocument,
        transition_facts: reversedInputs.facts,
      }).length === 0, "valid complete guard inputs must be order-independent");
      assertIssue(compareMigrationDebt(baseDebt, candidateDebt, {
        base_document: familyBase,
        candidate_document: candidateDocument,
        transition_facts: { restructure_count: 0, retirement_count: 0, guard_transfer_count: 1 },
      }), "E-DEBT-BASE-MISMATCH", "caller-shaped guard capability must be inert");
      assertIssue(compareMigrationDebt(baseDebt, candidateDebt, {
        base_document: familyBase,
        candidate_document: candidateDocument,
        transition_facts: [],
      }), "E-DEBT-BASE-MISMATCH", "empty capability composition must fail closed");
      for (const wrongRequest of [
        { ...request, base_owner: { ...request.base_owner, record: { ...request.base_owner.record } } },
        { ...request, candidate_guard: { ...request.candidate_guard, guard: { ...request.candidate_guard.guard } } },
      ]) {
        const wrong = validateDebtGuardTransferFacts(
          baseDebt,
          candidateDebt,
          { base_document: familyBase, candidate_document: candidateDocument },
          [wrongRequest],
        );
        assert(!wrong.ok && wrong.issues[0]?.code === "E-DEBT-OWNER-REGRESSION", "guard capability must require object-identical family and guard facts");
      }
      assertIssue(compareMigrationDebt(candidateDebt, baseDebt, {
        base_document: candidateDocument,
        candidate_document: familyBase,
        transition_facts: capability.facts,
      }), "E-DEBT-BASE-MISMATCH", "guard capability must be direction-bound");
      assertIssue(compareMigrationDebt(baseDebt, candidateDebt, {
        base_document: familyBase,
        candidate_document: candidateDocument,
        transition_facts: [capability.facts, capability.facts],
      }), "E-DEBT-BASE-MISMATCH", "overlapping capability rights must not compose");
      const unrelatedBase = c5V1("matrix", [c5FamilyRecord(C5_ID, [workId]), c5ReadyRecord(workId)]);
      const unrelatedCandidate = c5V1("matrix", [c5ReadyRecord(workId)]);
      const unrelatedGuards = c5FamilyGuards(C5_ID);
      const unrelatedRegistry = c5Registry({ kind: "worktree" }, { current_guards: unrelatedGuards });
      const unrelated = validateDebtGuardTransferFacts(
        c5Debt(unrelatedBase), c5Debt(unrelatedCandidate),
        { base_document: unrelatedBase, candidate_document: unrelatedCandidate },
        [{
          ...request,
          base_owner: { ...request.base_owner, record: unrelatedBase.records[0]! },
          candidate_guard: { ...request.candidate_guard, guard: unrelatedGuards[0]! },
          candidate_guards: unrelatedGuards,
          candidate_replacement_facts: [
            ...unrelatedRegistry.gates, ...unrelatedRegistry.test_symbols, ...unrelatedRegistry.tracked_headings,
          ],
        }],
      );
      assert(unrelated.ok, "unrelated comparison must mint its own capability");
      assertIssue(compareMigrationDebt(baseDebt, candidateDebt, {
        base_document: familyBase,
        candidate_document: candidateDocument,
        transition_facts: unrelated.facts,
      }), "E-DEBT-BASE-MISMATCH", "capability from another debt/document identity must fail");

      const attachedBase = c5V1(
        "matrix",
        [c5FamilyRecord(C5_ID, [workId]), c5ReadyRecord(workId)],
      );
      const attachedBytes = bytes("matrix");
      attachedBase.spans.push({
        id: "family-summary-span" as RoadmapDocumentV1["spans"][number]["id"],
        start_byte: 0,
        end_byte: attachedBytes.byteLength,
        sha256: c5Sha(attachedBytes),
        source_kind: "record",
        owner_id: C5_ID,
        owner_field: "payload.summary_md",
        migration_status: "replaced",
      });
      const attachedBaseDebt = c5Debt(attachedBase);
      const attachedRequest = {
        ...request,
        base_owner: { ...request.base_owner, record: attachedBase.records[0]! },
      };
      const attachedCapability = validateDebtGuardTransferFacts(
        attachedBaseDebt,
        candidateDebt,
        { base_document: attachedBase, candidate_document: candidateDocument },
        [attachedRequest],
      );
      assert(attachedCapability.ok && compareMigrationDebt(attachedBaseDebt, candidateDebt, {
        base_document: attachedBase,
        candidate_document: candidateDocument,
        transition_facts: attachedCapability.facts,
      }).length === 0, "guard transfer must authorize each exact source span attached to a derived family field atom");
      const unrelatedSpanBase = {
        ...attachedBase,
        spans: [...attachedBase.spans, {
          id: "unrelated-work-span" as RoadmapDocumentV1["spans"][number]["id"],
          start_byte: 0,
          end_byte: attachedBytes.byteLength,
          sha256: c5Sha(attachedBytes),
          source_kind: "record" as const,
          owner_id: workId,
          owner_field: "payload.summary_md",
          migration_status: "replaced" as const,
        }],
      };
      const unrelatedSpanDebt = c5Debt(unrelatedSpanBase);
      const unrelatedSpanCapability = validateDebtGuardTransferFacts(
        unrelatedSpanDebt,
        candidateDebt,
        { base_document: unrelatedSpanBase, candidate_document: candidateDocument },
        [{ ...attachedRequest, base_owner: { ...request.base_owner, record: unrelatedSpanBase.records[0]! } }],
      );
      assert(unrelatedSpanCapability.ok, "unrelated source span must not poison exact family capability minting");
      assertIssue(compareMigrationDebt(unrelatedSpanDebt, candidateDebt, {
        base_document: unrelatedSpanBase,
        candidate_document: candidateDocument,
        transition_facts: unrelatedSpanCapability.facts,
      }), "E-DEBT-OWNER-REGRESSION", "guard capability must not authorize removal of an unrelated source span");

      const compareWithGuard = (testBase: MigrationDebt, testCandidate: MigrationDebt) => {
        const guarded = validateDebtGuardTransferFacts(
          testBase,
          testCandidate,
          { base_document: familyBase, candidate_document: candidateDocument },
          [request],
        );
        assert(guarded.ok, `debt policy fixture must retain a valid guard capability: ${JSON.stringify(guarded.issues)}`);
        return compareMigrationDebt(testBase, testCandidate, {
          base_document: familyBase,
          candidate_document: candidateDocument,
          transition_facts: guarded.facts,
        });
      };
      const workSummaryKey = {
        roadmap: "matrix" as const,
        owner_kind: "record" as const,
        owner_id: workId,
        owner_field: "payload.summary_md",
      };
      const missingUnrelatedOwner: MigrationDebt = {
        ...candidateDebt,
        owners: new Map([...candidateDebt.owners].filter(([index]) => index !== debtOwnerIndex(workSummaryKey))),
      };
      assertIssue(compareWithGuard(baseDebt, missingUnrelatedOwner), "E-DEBT-OWNER-REGRESSION",
        "guard transfer must not hide an unrelated owner regression");
      const independentOwner = candidateDebt.owners.get(debtOwnerIndex(workSummaryKey))!.key;
      const grownIndependent = {
        roadmap: "matrix" as const,
        category: "unresolved_references" as const,
        owner: independentOwner,
        subject: "fixture-growth",
      };
      const candidateIndependentGrowth: MigrationDebt = {
        ...candidateDebt,
        independent: new Map([[independentDebtIndex(grownIndependent), grownIndependent]]),
      };
      assertIssue(compareWithGuard(baseDebt, candidateIndependentGrowth), "E-DEBT-SET-GROWTH",
        "guard transfer must not hide independent debt growth");
      const baseIndependent = { ...grownIndependent, category: "raw_subordinate_lifecycles" as const };
      const baseCategoryDebt: MigrationDebt = {
        ...baseDebt,
        independent: new Map([[independentDebtIndex(baseIndependent), baseIndependent]]),
      };
      assertIssue(compareWithGuard(baseCategoryDebt, candidateIndependentGrowth), "E-DEBT-CATEGORY-HIDE",
        "guard transfer must not hide an independent debt category move");
      const frozenGrowthKey = {
        roadmap: "matrix" as const,
        owner_kind: "source_span" as const,
        owner_id: "fixture-frozen-growth" as RoadmapDocumentV1["spans"][number]["id"],
        owner_field: "coverage" as const,
      };
      const candidateFrozenGrowth: MigrationDebt = {
        ...candidateDebt,
        frozen_legacy_spans: new Map([[debtOwnerIndex(frozenGrowthKey), frozenGrowthKey]]),
      };
      assertIssue(compareWithGuard(baseDebt, candidateFrozenGrowth), "E-DEBT-FROZEN-SET",
        "guard transfer must not hide frozen legacy-span growth");

      const retiringId = "matrix.fixture-retiring" as RoadmapId;
      const composed = c5AllTransaction(
        c5ActiveRevision(c5V1("matrix", [
          c5FamilyRecord(C5_ID, [workId]), c5ReadyRecord(workId), c5ReadyRecord(retiringId),
        ])),
        c5ActiveRevision(c5V1("matrix", [c5ReadyRecord(workId)]), [], {
          guards,
          retired: [c5Tombstone(retiringId)],
        }),
      );
      assert(composed.issues.length === 0 && composed.guard_transfers.includes(C5_ID) &&
        composed.retired_ids.includes(retiringId), `disjoint guard+retirement capabilities must compose: ${JSON.stringify(composed.issues)}`);

      const compositionBase = c5V1("matrix", [
        c5FamilyRecord(C5_ID, [workId]),
        c5ReadyRecord(workId),
        c5ReadyRecord(retiringId),
      ]);
      const compositionCandidate = c5V1("matrix", [c5ReadyRecord(workId)]);
      const compositionBaseDebt = c5Debt(compositionBase);
      const compositionCandidateDebt = c5Debt(compositionCandidate);
      const compositionRegistry = c5Registry({ kind: "worktree" }, { current_guards: guards });
      const compositionGuard = validateDebtGuardTransferFacts(
        compositionBaseDebt,
        compositionCandidateDebt,
        { base_document: compositionBase, candidate_document: compositionCandidate },
        [{
          ...request,
          base_owner: { ...request.base_owner, record: compositionBase.records[0]! },
          candidate_guard: { ...request.candidate_guard, guard: guards[0]! },
          candidate_guards: guards,
          candidate_replacement_facts: [
            ...compositionRegistry.gates,
            ...compositionRegistry.test_symbols,
            ...compositionRegistry.tracked_headings,
          ],
        }],
      );
      const retirement = c5Tombstone(retiringId);
      const retirementOwner = {
        owner_kind: "tombstone" as const,
        id: retiringId,
        namespace: "matrix" as const,
        tombstone: retirement,
      };
      const compositionRetirement = validateDebtRetirementFacts(
        compositionBaseDebt,
        compositionCandidateDebt,
        { base_document: compositionBase, candidate_document: compositionCandidate },
        [{
          base_owner: {
            owner_kind: "active_record",
            id: retiringId,
            namespace: "matrix",
            record: compositionBase.records[2]!,
          },
          removed_debt_owners: [...compositionBaseDebt.owners.values()].filter(({ key }) =>
            key.owner_kind === "record" && key.owner_id === retiringId
          ).map(({ key }) => key),
          base_commit: C5_BASE,
          base_source: {
            source_path: compositionBase.document.source_path,
            sha256: compositionBase.document.frozen_source_sha256,
            byte_length: compositionBase.document.frozen_source_byte_length,
          },
          candidate_source: {
            source_path: compositionCandidate.document.source_path,
            sha256: compositionCandidate.document.frozen_source_sha256,
            byte_length: compositionCandidate.document.frozen_source_byte_length,
          },
          candidate_identity_facts: [retirementOwner],
          candidate_tombstone: retirementOwner,
          candidate_replacement_fact: compositionRegistry.gates[0],
        }],
      );
      assert(compositionGuard.ok && compositionRetirement.ok,
        "disjoint direct guard and retirement capabilities must both mint");
      for (const facts of [
        [compositionGuard.facts, compositionRetirement.facts],
        [compositionRetirement.facts, compositionGuard.facts],
      ]) {
        assert(compareMigrationDebt(compositionBaseDebt, compositionCandidateDebt, {
          base_document: compositionBase,
          candidate_document: compositionCandidate,
          transition_facts: facts,
        }).length === 0, "disjoint guard+retirement capabilities must compose in both orders");
      }

      const restructureId = "matrix.fixture-restructure" as RoadmapId;
      const oldSpanId = "restructure-old" as RoadmapDocumentV1["spans"][number]["id"];
      const newSpanId = "restructure-new" as RoadmapDocumentV1["spans"][number]["id"];
      const rawRecord: RoadmapDocumentV1["records"][number] = {
        id: restructureId,
        title: "Restructure",
        projection_group: "fixture" as RoadmapDocumentV1["records"][number]["projection_group"],
        render_authority: "raw",
        source_block_md: bytes("## Restructure\n"),
        span_ids: [oldSpanId],
      };
      const semanticRecord = {
        id: restructureId,
        title: "Restructure",
        projection_group: "fixture" as RoadmapDocumentV1["records"][number]["projection_group"],
        render_authority: "semantic" as const,
        projection_visibility: "document" as const,
        payload: { kind: "decision", summary_md: bytes("done") },
        source_replacements: [{
          span_id: newSpanId,
          replacement_field: "payload.summary_md",
          review_note_md: bytes("reviewed"),
        }],
      } as unknown as RoadmapDocumentV1["records"][number];
      const restructureBase = c5V1("matrix", [c5FamilyRecord(C5_ID), rawRecord]);
      restructureBase.spans.push({
        id: oldSpanId,
        start_byte: 0,
        end_byte: 6,
        sha256: c5Sha(bytes("matrix")),
        source_kind: "record",
        owner_id: restructureId,
        owner_field: "source_block_md",
        migration_status: "raw",
      });
      const restructureCandidate = c5V1("matrix", [semanticRecord]);
      restructureCandidate.spans.push({
        id: newSpanId,
        start_byte: 0,
        end_byte: 6,
        sha256: c5Sha(bytes("matrix")),
        source_kind: "record",
        owner_id: restructureId,
        owner_field: "payload.summary_md",
        migration_status: "replaced",
      });
      const restructureBaseDebt = c5Debt(restructureBase);
      const restructureCandidateDebt = c5Debt(restructureCandidate);
      const restructureGuard = validateDebtGuardTransferFacts(
        restructureBaseDebt,
        restructureCandidateDebt,
        { base_document: restructureBase, candidate_document: restructureCandidate },
        [{
          ...request,
          base_owner: { ...request.base_owner, record: restructureBase.records[0]! },
        }],
      );
      const restructure = validateDebtTransitionFacts(
        restructureBaseDebt,
        restructureCandidateDebt,
        { base_document: restructureBase, candidate_document: restructureCandidate },
        [{
          removed: {
            roadmap: "matrix",
            owner_kind: "record",
            owner_id: restructureId,
            owner_field: "source_block_md",
          },
          added: [{
            roadmap: "matrix",
            owner_kind: "record",
            owner_id: restructureId,
            owner_field: "payload.summary_md",
          }],
        }],
      );
      assert(restructureGuard.ok && restructure.ok,
        "disjoint direct guard and restructure capabilities must both mint");
      for (const facts of [
        [restructureGuard.facts, restructure.facts],
        [restructure.facts, restructureGuard.facts],
      ]) {
        assert(compareMigrationDebt(restructureBaseDebt, restructureCandidateDebt, {
          base_document: restructureBase,
          candidate_document: restructureCandidate,
          transition_facts: facts,
        }).length === 0, "disjoint guard+restructure capabilities must compose in both orders");
      }

      const conversionId = "matrix.fixture-semantic-conversion" as RoadmapId;
      const conversionSpanId = "semantic-conversion-span" as RoadmapDocumentV1["spans"][number]["id"];
      const conversionPayload = {
        kind: "decision" as const,
        summary_md: bytes("matrix"),
        decision_state: "pending" as const,
        question_md: bytes("review"),
        transition_ids: [],
      };
      const conversionRaw: RoadmapDocumentV1["records"][number] = {
        id: conversionId,
        title: "Semantic conversion",
        projection_group: "fixture" as RoadmapDocumentV1["records"][number]["projection_group"],
        render_authority: "raw",
        source_block_md: bytes("matrix"),
        span_ids: [conversionSpanId],
        semantic_shadow: conversionPayload,
      };
      const conversionSemantic: RoadmapDocumentV1["records"][number] = {
        id: conversionId,
        title: "Semantic conversion",
        projection_group: "fixture" as RoadmapDocumentV1["records"][number]["projection_group"],
        render_authority: "semantic",
        projection_visibility: "document",
        payload: conversionPayload,
        source_replacements: [{
          span_id: conversionSpanId,
          replacement_field: "payload.summary_md",
          review_note_md: bytes("reviewed"),
        }],
      };
      const conversionBase = c5V1("matrix", [c5FamilyRecord(C5_ID), conversionRaw]);
      conversionBase.document.frozen_legacy_span_ids = [conversionSpanId];
      conversionBase.spans.push({
        id: conversionSpanId,
        start_byte: 0,
        end_byte: 6,
        sha256: c5Sha(bytes("matrix")),
        source_kind: "record",
        owner_id: conversionId,
        owner_field: "source_block_md",
        migration_status: "raw",
      });
      const conversionCandidate = c5V1("matrix", [conversionSemantic]);
      conversionCandidate.spans.push({
        id: conversionSpanId,
        start_byte: 0,
        end_byte: 6,
        sha256: c5Sha(bytes("matrix")),
        source_kind: "record",
        owner_id: conversionId,
        owner_field: "payload.summary_md",
        migration_status: "replaced",
      });
      const conversionBaseDebt = c5Debt(conversionBase);
      const conversionCandidateDebt = c5Debt(conversionCandidate);
      const completed = (
        ownerField: string,
        consumedFields: readonly string[],
        segments: CompletedRenderIr["projected_field_segments"],
      ): CompletedRenderIr => {
        const chunks: RenderChunk[] = [{
          manifest_index: 1,
          owner: { kind: "record", id: conversionId, field: ownerField },
          bytes: bytes("matrix"),
          source_span_ids: [conversionSpanId],
          consumed_fields: consumedFields,
        }];
        return {
        chunks,
        field_consumption: consumedFields.length === 0 ? [] : [{
          owner_kind: "record",
          owner_id: conversionId,
          expected_fields: ["payload.question_md", "payload.summary_md"],
          consumed_fields: ["payload.question_md", "payload.summary_md"],
          duplicate_fields: [], unknown_fields: [], mismatched_fields: [],
        }],
        projected_field_segments: segments,
        slot_resolutions: [], build_issues: [],
        expected_bytes: createExpectedByteView(chunks),
      };
      };
      const baseCompleted = completed("source_block_md", [], []);
      const candidateCompleted = completed("payload", ["payload.question_md", "payload.summary_md"], [{
        owner_kind: "record", owner_id: conversionId, logical_path: "payload.summary_md",
        start_in_chunk: 0, end_in_chunk: 6, bytes: bytes("matrix"),
      }]);
      const conversionGuard = validateDebtGuardTransferFacts(
        conversionBaseDebt,
        conversionCandidateDebt,
        { base_document: conversionBase, candidate_document: conversionCandidate },
        [{ ...request, base_owner: { ...request.base_owner, record: conversionBase.records[0]! } }],
      );
      const conversion = validateSemanticConversionFacts(
        conversionBaseDebt,
        conversionCandidateDebt,
        {
          base_document: conversionBase,
          candidate_document: conversionCandidate,
          base_completed: baseCompleted,
          candidate_completed: candidateCompleted,
        },
      );
      assert(conversionGuard.ok && conversion.ok && conversion.facts !== undefined,
        `disjoint direct guard and semantic-conversion capabilities must both mint: guard=${conversionGuard.ok ? "ok" : JSON.stringify(conversionGuard.issues)} conversion=${conversion.ok ? "ok" : JSON.stringify(conversion.issues)}`);
      for (const facts of [
        [conversionGuard.facts, conversion.facts],
        [conversion.facts, conversionGuard.facts],
      ]) {
        assert(compareMigrationDebt(conversionBaseDebt, conversionCandidateDebt, {
          base_document: conversionBase,
          candidate_document: conversionCandidate,
          base_completed: baseCompleted,
          candidate_completed: candidateCompleted,
          transition_facts: facts,
        }).length === 0, "disjoint guard+semantic-conversion capabilities must compose in both orders");
      }
      return true;
    }
    case "transaction_missing_campaign_removal": {
      const result = c5AllTransaction(
        c5ActiveRevision(active, [c5Selection(C5_ID)]),
        c5ActiveRevision(empty, [c5Selection(C5_ID)], { retired: [c5Tombstone(C5_ID)] }),
      );
      assertIssue(result.issues, "E-CAMPAIGN-TARGET", "retirement selection must fail closed before partial lifecycle authorization");
      return true;
    }
    case "transaction_live_citation":
    case "transaction_citation_in_nonroadmap_file_rejected": {
      const citation = {
        id: C5_ID, source: (id.endsWith("nonroadmap_file_rejected") ? "README.md" : "tests/TESTING_ROADMAP.md") as RepoPath,
        span: { start_byte: 2, end_byte: 30 }, raw: `roadmap:${C5_ID}`,
      };
      const secondCitation = {
        ...citation,
        source: "docs/retirement.md" as RepoPath,
        span: { start_byte: 41, end_byte: 69 },
      };
      const result = c5AllTransaction(
        c5ActiveRevision(active),
        c5ActiveRevision(empty, [], { retired: [c5Tombstone(C5_ID)], citations: [citation, secondCitation] }),
      );
      assert(
        observeMatchingIssue(
          result.issues,
          "E-TRANSACTION-CITATION",
          `citation[${JSON.stringify(C5_ID)}]`,
        ) !== undefined,
        "repository-wide live citation must block retirement",
      );
      const citationIssues = result.issues.filter((issue) => issue.code === "E-TRANSACTION-CITATION");
      assert(citationIssues.length === 2, "each surviving repository citation must independently block retirement");
      const expected = [citation, secondCitation].sort((left, right) => left.source < right.source ? -1 : left.source > right.source ? 1 : 0);
      assert(
        citationIssues.every((issue, index) =>
          issue.source === expected[index]!.source &&
          issue.span?.start_byte === expected[index]!.span.start_byte &&
          issue.span?.end_byte === expected[index]!.span.end_byte
        ),
        "retirement diagnostics must preserve each surviving citation's exact source and half-open byte span",
      );
      return true;
    }
    case "transaction_dangling_relation": {
      const other = "matrix.fixture-other" as RoadmapId;
      const candidate = c5V1("matrix", [c5ReadyRecord(other)], [{ source: other, kind: "related", target: C5_ID }]);
      const result = c5AllTransaction(c5ActiveRevision(active), c5ActiveRevision(candidate, [], { retired: [c5Tombstone(C5_ID)] }));
      assertIssue(result.issues, "E-TRANSACTION-REFERENCE", "dangling relation must fail");
      return true;
    }
    case "transaction_dangling_reference": {
      const other = "matrix.fixture-other" as RoadmapId;
      const candidate = c5V1("matrix", [c5ReadyRecord(other)], [], [{
        id: "roadmap-ref" as ReferenceId, source: other, kind: "roadmap", target_id: C5_ID,
      }]);
      const result = c5AllTransaction(c5ActiveRevision(active), c5ActiveRevision(candidate, [], { retired: [c5Tombstone(C5_ID)] }));
      assertIssue(result.issues, "E-TRANSACTION-REFERENCE", "dangling typed reference must fail");
      return true;
    }
    case "transaction_missing_tombstone": {
      const result = c5AllTransaction(c5ActiveRevision(active), c5ActiveRevision(empty));
      assertIssue(result.issues, "E-TRANSACTION-OWNER", "active removal without tombstone must fail");
      return true;
    }
    case "transaction_unused_tombstone": {
      const result = c5AllTransaction(c5ActiveRevision(empty), c5ActiveRevision(empty, [], { retired: [c5Tombstone(C5_ID)] }));
      assertIssue(result.issues, "E-TRANSACTION-ORIGIN", "unused tombstone must fail");
      return true;
    }
    case "transaction_partial_guard": {
      const workId = "matrix.fixture-linked-work" as RoadmapId;
      const familyBase = c5V1("matrix", [c5FamilyRecord(C5_ID, [workId]), c5ReadyRecord(workId)]);
      const survivor = c5V1("matrix", [c5ReadyRecord(workId)]);
      const validGuards = c5FamilyGuards(C5_ID);
      const validGuard = validGuards[0]!;
      const candidates = [
        c5ActiveRevision(survivor),
        c5ActiveRevision(survivor, [], { guards: [{ ...validGuard, id: "matrix.fixture-wrong" as RoadmapId }, ...validGuards.slice(1)] }),
        c5ActiveRevision(survivor, [], { guards: [{ ...validGuard, replacement_pin: { kind: "gate", gate_id: "missing", claim_md: bytes("pin") } }, ...validGuards.slice(1)] }),
        c5ActiveRevision(survivor, [], { guards: validGuards, retired: [c5Tombstone(C5_ID)] }),
        c5ActiveRevision(familyBase, [], { guards: validGuards }),
        c5ActiveRevision(survivor, [], { guards: validGuards.slice(0, -1) }),
      ];
      for (const candidate of candidates) {
        const result = c5AllTransaction(c5ActiveRevision(familyBase), candidate);
        assert(result.issues.length > 0, "missing/wrong/unresolved/tombstoned/leftover family guard transfer must fail");
      }
      const unrelatedGuard = c5Guard("matrix.fixture-unused-guard" as RoadmapId);
      assertIssue(c5AllTransaction(
        c5ActiveRevision(familyBase),
        c5ActiveRevision(survivor, [], { guards: [...validGuards, unrelatedGuard] }),
      ).issues, "E-TRANSACTION-GUARD", "candidate-only unused guard must fail with no base owner");
      const childId = c5FamilyChildIds(C5_ID)[0]!;
      const retainedBase = withBaseRevision(c5ActiveRevision(c5V1("matrix"), [], { guards: validGuards }));
      const reactivated = c5ActiveRevision(c5V1("matrix", [c5ReadyRecord(childId)]), [], {
        guards: validGuards.filter((guard) => guard.id !== childId),
      });
      assertIssue(validateTransaction({
        scope: "all", against: C5_BASE, base: retainedBase, candidate: reactivated,
      }).issues, "E-TRANSACTION-OWNER", "a protected systematic child ID cannot be reactivated by dropping its guard");
      return true;
    }
    case "transaction_family_tombstone_rejected": {
      const family = c5V1("matrix", [c5FamilyRecord(C5_ID)]);
      const result = c5AllTransaction(c5ActiveRevision(family), c5ActiveRevision(empty, [], { retired: [c5Tombstone(C5_ID)] }));
      assertIssue(result.issues, "E-TRANSACTION-ORIGIN", "active family is not tombstone eligible");
      return true;
    }
    case "transaction_linked_work_tombstone_required": {
      const workId = "matrix.fixture-linked-work" as RoadmapId;
      const family = c5FamilyRecord(C5_ID, [workId]);
      const base = c5V1("matrix", [family, c5ReadyRecord(workId)]);
      const candidate = c5V1("matrix", [family]);
      const result = c5AllTransaction(c5ActiveRevision(base), c5ActiveRevision(candidate));
      assertIssue(result.issues, "E-TRANSACTION-OWNER", "removed linked work needs its own tombstone");
      return true;
    }
    case "transaction_duplicate_current_owner": {
      const guard: CurrentGuard = {
        id: C5_ID,
        replacement_pin: { kind: "gate", gate_id: "roadmap_projection_check", claim_md: bytes("pin") },
        owner_registry: "fixture-guards",
      };
      const result = c5AllTransaction(c5ActiveRevision(active), c5ActiveRevision(active, [], { guards: [guard] }));
      assertIssue(result.issues, "E-OWNER-DUPLICATE", "duplicate candidate owner must fail before transaction authorization");
      const guardB: CurrentGuard = { ...guard, replacement_pin: { ...guard.replacement_pin, claim_md: bytes("other") } };
      const forward = c5AllTransaction(c5ActiveRevision(active), c5ActiveRevision(active, [], { guards: [guard, guardB] }));
      const reversed = c5AllTransaction(c5ActiveRevision(active), c5ActiveRevision(active, [], { guards: [guardB, guard] }));
      assert(JSON.stringify(forward.issues) === JSON.stringify(reversed.issues), "transaction diagnostics must be deterministic under current-owner reversal");
      return true;
    }
    case "transaction_deselect_active_allowed": {
      const result = c5AllTransaction(c5ActiveRevision(active, [c5Selection(C5_ID)]), c5ActiveRevision(active));
      assert(result.issues.length === 0, "deselection with active owner retained must pass");
      return true;
    }
    case "transaction_full_hash_git_integration": {
      assert(context !== undefined, "full-hash integration requires injected SelfTestContext ports");
      const durableCorpus = [
        { path: "cddl-matrix/roadmap/fixtures/durable-note.toml" as RepoPath, heading: "Fixture Corpus" },
        { path: "tests/preserve-fixtures/durable-note.mdx" as RepoPath, heading: "Preserve Corpus" },
        { path: "cddl-matrix/sources/durable-note.txt" as RepoPath, heading: "Sources Corpus" },
      ] as const;
      const corpusBytes = (id: RoadmapId, heading: string): Uint8Array =>
        bytes(`é roadmap:${id}\n# ${heading}\n`);
      const crlfPath = "tests/preserve-fixtures/crlf/no-heading.rs" as RepoPath;
      const crlfBytes = (id: RoadmapId): Uint8Array => bytes(`// roadmap:${id}\r\nfn sentinel() {}\r\n`);
      const worktreeId = "matrix.fixture-worktree" as RoadmapId;
      const exerciseObjectFormat = (format: "sha1" | "sha256"): FullCommitId => {
        const repository = context.ports.fixtures.createScratchRepository([
          { path: "fixture.txt" as RepoPath, bytes: bytes("base\n") },
          { path: "src/main.rs" as RepoPath, bytes: bytes("#[cfg(test)]\nmod tests;\n") },
          { path: "src/tests/mod.rs" as RepoPath, bytes: new Uint8Array() },
          ...durableCorpus.map((entry) => ({ path: entry.path, bytes: corpusBytes(C5_ID, entry.heading) })),
          { path: crlfPath, bytes: crlfBytes(C5_ID) },
        ]);
        let commit: FullCommitId | undefined;
        try {
          assert(context.ports.fixtures.scratchRepositoryPresent(repository), `${format} scratch repository must be live`);
          for (const argv of [
            ["init", "--quiet", `--object-format=${format}`],
            ["add", "--all"],
            ["commit", "--quiet", "--no-gpg-sign", "-m", "base;argv-is-not-shell"],
          ] as const) {
            const command = context.ports.scratch_git.runScratchGit(repository, argv);
            assert(command.exit_code === 0, `${format} scratch Git argv command failed: ${JSON.stringify(argv)}`);
          }
          const resolved = context.ports.scratch_git.runScratchGit(repository, ["rev-parse", "HEAD"]);
          assert(resolved.exit_code === 0, `${format} scratch Git must resolve the committed HEAD`);
          const oid = new TextDecoder().decode(resolved.stdout).trim();
          const length = format === "sha1" ? 40 : 64;
          assert(new RegExp(`^[0-9a-f]{${length}}$`, "u").test(oid), `${format} scratch Git must return an exact full lowercase OID`);
          commit = oid as FullCommitId;

          const ports = context.ports.fixtures.openScratchRoadmapPorts(repository);
          assert(ports.repositoryObjectFormat() === format, `${format} production scratch ports reported the wrong object format`);
          assert(ports.resolveFullCommit(oid) === oid, `${format} production scratch ports changed the exact full OID`);
          assert(
            new TextDecoder().decode(ports.readDeclaredAtCommit(commit, "fixture.txt" as RepoPath)) === "base\n",
            `${format} production commit read changed the exact fixture bytes`,
          );
          const view = ports.registryView({ kind: "commit", commit });
          assert(
            view.revision.kind === "commit" && view.revision.commit === commit,
            `${format} production registry view changed the exact commit revision`,
          );
          const raw = `roadmap:${C5_ID}`;
          for (const entry of durableCorpus) {
            const citation = view.roadmap_citations.find((fact) => fact.source === entry.path);
            assert(
              citation?.id === C5_ID && citation.raw === raw &&
                citation.span.start_byte === 3 && citation.span.end_byte === 3 + bytes(raw).byteLength,
              `${format} production registry omitted or moved the exact ${entry.path} citation span`,
            );
            const heading = view.tracked_headings.find((fact) => fact.path === entry.path);
            const headingStart = bytes(`é ${raw}\n# `).byteLength;
            assert(
              heading?.heading === entry.heading && heading.span.start_byte === headingStart &&
                heading.span.end_byte === headingStart + bytes(entry.heading).byteLength,
              `${format} extension-neutral heading inventory omitted or moved ${entry.path}`,
            );
          }
          const crlfCitation = view.roadmap_citations.find((fact) => fact.source === crlfPath);
          assert(
            crlfCitation?.id === C5_ID && crlfCitation.span.start_byte === 3 &&
              crlfCitation.span.end_byte === 3 + bytes(raw).byteLength,
            `${format} byte citation scanner rejected a CRLF tracked file with no heading candidate`,
          );
          const retirement = c5AllTransaction(
            c5ActiveRevision(active),
            c5ActiveRevision(empty, [], {
              retired: [c5Tombstone(C5_ID)],
              citations: view.roadmap_citations,
            }),
          );
          const citationIssues = retirement.issues.filter((issue) => issue.code === "E-TRANSACTION-CITATION");
          assert(
            citationIssues.length === durableCorpus.length + 1 &&
              citationIssues.every((issue) =>
                view.roadmap_citations.some((fact) =>
                  fact.source === issue.source && fact.span.start_byte === issue.span?.start_byte &&
                  fact.span.end_byte === issue.span?.end_byte
                )
              ),
            `${format} retained durable-corpus citations did not each block retirement at their production spans`,
          );

          for (const entry of durableCorpus) {
            context.ports.fixtures.replaceScratchFile(
              repository,
              entry.path,
              corpusBytes(worktreeId, `${entry.heading} Worktree`),
            );
          }
          context.ports.fixtures.replaceScratchFile(repository, crlfPath, crlfBytes(worktreeId));
          const worktreeView = ports.registryView({ kind: "worktree" });
          const committedAgain = ports.registryView({ kind: "commit", commit });
          assert(
            worktreeView.roadmap_citations.filter((fact) =>
              durableCorpus.some((entry) => entry.path === fact.source) || fact.source === crlfPath
            ).every((fact) => fact.id === worktreeId) &&
              committedAgain.roadmap_citations.filter((fact) =>
                durableCorpus.some((entry) => entry.path === fact.source) || fact.source === crlfPath
              ).every((fact) => fact.id === C5_ID),
            `${format} worktree and base citation inventories were not revision-isolated`,
          );
          assert(
            worktreeView.tracked_headings.filter((fact) =>
              durableCorpus.some((entry) => entry.path === fact.path)
            ).every((fact) => fact.heading.endsWith(" Worktree")) &&
              committedAgain.tracked_headings.filter((fact) =>
                durableCorpus.some((entry) => entry.path === fact.path)
              ).every((fact) => !fact.heading.endsWith(" Worktree")),
            `${format} worktree and base heading inventories were not revision-isolated`,
          );
          context.ports.fixtures.replaceScratchFile(
            repository,
            durableCorpus[0].path,
            bytes("# CRLF heading candidate\r\n"),
          );
          let headingFailure: unknown;
          try { ports.registryView({ kind: "worktree" }); }
          catch (error) { headingFailure = error; }
          assert(
            headingFailure instanceof RoadmapFailure &&
              headingFailure.issues.some((issue) =>
                issue.code === "E-SOURCE-LINE-END" && issue.source === durableCorpus[0].path
              ),
            `${format} heading parser consumed a CRLF candidate without enforcing its LF precondition`,
          );
        } finally {
          context.ports.fixtures.removeScratchRepository(repository);
        }
        assert(!context.ports.fixtures.scratchRepositoryPresent(repository), `${format} scratch repository leaked after finally`);
        assert(commit !== undefined, `${format} scratch repository did not produce a commit`);
        return commit;
      };

      const scratchCommit = exerciseObjectFormat("sha1");
      const sha256Commit = exerciseObjectFormat("sha256");
      for (const commit of [scratchCommit, sha256Commit]) {
        const result = validateTransaction({
          scope: "all", against: commit,
          base: { ...c5ActiveRevision(active), registry: c5Registry({ kind: "commit", commit }) },
          candidate: c5ActiveRevision(active),
        });
        assert(result.issues.length === 0, `injected scratch full hash must bind the base transaction: ${JSON.stringify(result.issues)}`);
      }
      const wrongBase = validateTransaction({
        scope: "all", against: C5_BASE,
        base: { ...withBaseRevision(c5ActiveRevision(active)), registry: c5Registry({ kind: "commit", commit: "f".repeat(40) as FullCommitId }) },
        candidate: c5ActiveRevision(active),
      });
      assertIssue(wrongBase.issues, "E-TRANSACTION-BASE", "same full revision must supply base documents and facts");
      const abbreviated = validateTransaction({
        scope: "all", against: "0123456" as FullCommitId,
        base: withBaseRevision(c5ActiveRevision(active)), candidate: c5ActiveRevision(active),
      });
      assertIssue(abbreviated.issues, "E-TRANSACTION-BASE", "abbreviated base must fail closed");
      const candidateCommit = validateTransaction({
        scope: "all", against: C5_BASE,
        base: withBaseRevision(c5ActiveRevision(active)),
        candidate: { ...c5ActiveRevision(active), registry: c5Registry({ kind: "commit", commit: C5_BASE }) },
      });
      assertIssue(candidateCommit.issues, "E-TRANSACTION-BASE", "candidate facts from an unrelated commit must not substitute for worktree facts");
      return true;
    }
    case "transaction_legacy_cutover_transfer_selected":
    case "transaction_legacy_cutover_transfer_unselected": {
      const selected = id === "transaction_legacy_cutover_transfer_selected";
      const pair = c5LegacyPair(true, selected);
      const shadow = pair.revision.roadmaps.matrix.document as RoadmapDocumentV0;
      const promoted = c5PromoteV0(shadow, new Map([[C5_ID, c5ReadyRecord(C5_ID).payload]]));
      const candidate = c5ActiveRevision(promoted, selected ? [c5Selection(C5_ID)] : []);
      const result = validateTransaction({ scope: "all", against: C5_BASE, base: pair.revision, candidate });
      assert(result.issues.length === 0 && result.authority_transfers[0] === C5_ID, `complete reservation cutover must pass: ${JSON.stringify(result.issues)}`);
      return true;
    }
    case "transaction_legacy_cutover_work_kind_mismatch": {
      const pair = c5LegacyPair(true);
      const shadow = pair.revision.roadmaps.matrix.document as RoadmapDocumentV0;
      const promoted = c5PromoteV0(shadow, new Map([[C5_ID, c5ReadyRecord(C5_ID, "defect").payload]]));
      const candidate = c5ActiveRevision(promoted, [c5Selection(C5_ID)]);
      const result = validateTransaction({ scope: "all", against: C5_BASE, base: pair.revision, candidate });
      assertIssue(result.issues, "E-TRANSACTION-OWNER", "cutover work kind mismatch must fail");
      return true;
    }
    case "transaction_legacy_delivery_without_shadow":
    case "transaction_legacy_delivery_with_shadow": {
      const shadow = id.endsWith("with_shadow");
      const pair = c5LegacyPair(shadow);
      const candidate = c5LegacyCandidate(shadow);
      const result = validateTransaction({ scope: "all", against: C5_BASE, base: pair.revision, candidate });
      assert(result.issues.length === 0 && result.retired_ids[0] === C5_ID, `complete legacy delivery must pass: ${JSON.stringify(result.issues)}`);
      if (shadow) {
        const allocations = { combined: 0, final: 0 };
        const observer: ExpectedByteViewObserver = {
          hashSegmentVisited: () => {},
          combinedHashBufferAllocated: () => { allocations.combined++; },
          finalProjectionAllocated: () => { allocations.final++; },
        };
        const originalBaseMarkdown = pair.revision.roadmaps.matrix.markdown;
        const originalCandidateMarkdown = candidate.roadmaps.matrix.markdown;
        const baseView = c5ExpectedView(originalBaseMarkdown, observer);
        const candidateView = c5ExpectedView(originalCandidateMarkdown, observer);
        const title = validateLegacyTitleBinding(pair.reservation, baseView);
        assert(title !== undefined, "expected-byte view must mint the same reviewed-title capability");
        originalBaseMarkdown[0] ^= 0xff;
        assert(baseView.bytesEqual(c5LegacySource()), "expected-byte view did not retain its immutable private chunks");
        assert(baseView.contains(bytes("cycle\nlegacy")), "streaming contains missed a cross-chunk needle");
        const viewBase = {
          ...pair.revision,
          roadmaps: {
            ...pair.revision.roadmaps,
            matrix: { ...pair.revision.roadmaps.matrix, markdown: baseView },
          },
          legacy_title_bindings: [title],
        };
        const viewCandidate = {
          ...candidate,
          roadmaps: {
            ...candidate.roadmaps,
            matrix: { ...candidate.roadmaps.matrix, markdown: candidateView },
          },
        };
        const viewDelivery = validateTransaction({
          scope: "all", against: C5_BASE, base: viewBase, candidate: viewCandidate,
        });
        assert(
          viewDelivery.issues.length === 0 &&
          viewDelivery.base?.identity.owners.get(C5_ID)?.owner_kind === "legacy_markdown_reservation",
          `expected-byte shadow snapshot lost campaign/identity provenance: ${JSON.stringify(viewDelivery.issues)}`,
        );

        const shadowDocument = pair.revision.roadmaps.matrix.document as RoadmapDocumentV0;
        const promoted = c5PromoteV0(shadowDocument, new Map([[C5_ID, c5ReadyRecord(C5_ID).payload]]));
        const active = c5ActiveRevision(promoted, [c5Selection(C5_ID)]);
        const activeMarkdown = active.roadmaps.matrix.markdown;
        const activeView = c5ExpectedView(activeMarkdown, observer);
        const viewCutover = validateTransaction({
          scope: "all",
          against: C5_BASE,
          base: viewBase,
          candidate: {
            ...active,
            roadmaps: {
              ...active.roadmaps,
              matrix: { ...active.roadmaps.matrix, markdown: activeView },
            },
          },
        });
        assert(
          viewCutover.issues.length === 0 && viewCutover.authority_transfers[0] === C5_ID,
          `expected-byte authoritative snapshot failed cutover: ${JSON.stringify(viewCutover.issues)}`,
        );
        assert(allocations.combined === 0 && allocations.final === 0,
          "lifecycle validation materialized a combined or final projection buffer");
      }
      return true;
    }
    case "transaction_legacy_delivery_selection_survives":
    case "transaction_legacy_delivery_reservation_survives":
    case "transaction_legacy_delivery_bound_span_survives":
    case "transaction_legacy_delivery_shadow_owner_survives":
    case "transaction_legacy_delivery_missing_tombstone":
    case "transaction_legacy_delivery_wrong_last_active":
    case "transaction_legacy_delivery_live_repo_citation": {
      const mutations = {
        transaction_legacy_delivery_selection_survives: "selection",
        transaction_legacy_delivery_reservation_survives: "reservation",
        transaction_legacy_delivery_bound_span_survives: "bytes",
        transaction_legacy_delivery_shadow_owner_survives: "shadow",
        transaction_legacy_delivery_missing_tombstone: "missing_tombstone",
        transaction_legacy_delivery_wrong_last_active: "wrong_commit",
        transaction_legacy_delivery_live_repo_citation: "citation",
      } as const;
      const selectedMutations = id === "transaction_legacy_delivery_shadow_owner_survives"
        ? ["shadow", "active", "guard", "alias"] as const
        : id === "transaction_legacy_delivery_bound_span_survives"
        ? ["bytes", "stale_shadow"] as const
        : id === "transaction_legacy_delivery_live_repo_citation"
        ? ["citation", "relation", "reference"] as const
        : [mutations[id]] as const;
      for (const mutation of selectedMutations) {
        const shadow = mutation === "reservation" ? false : true;
        const testsTestingLegacyReference = mutation === "alias" || mutation === "relation" || mutation === "reference";
        const testingTarget = C5_TESTING_ID;
        const testingSource = c5LegacySource();
        const testingReservation = c5Reservation(testingTarget, testingSource);
        const matrixRecord = {
          ...c5ReadyRecord(C5_ID),
          ...(mutation === "alias" ? { legacy_aliases: [testingTarget] } : {}),
        };
        const matrixDocument = c5V1(
          "matrix",
          [matrixRecord],
          mutation === "relation"
            ? [{ source: C5_ID, kind: "related", target: testingTarget }]
            : [],
          mutation === "reference"
            ? [{ id: "legacy-ref" as ReferenceId, source: C5_ID, kind: "roadmap", target_id: testingTarget }]
            : [],
        );
        const testingBaseDocument = c5V0("testing", testingSource, testingTarget);
        const testingCandidateSource = bytes("delivered\n");
        const pair = testsTestingLegacyReference
          ? {
            revision: c5WithDebt({
              campaign: c5Campaign(
                "authoritative",
                "shadow",
                [testingReservation],
                [c5Selection(testingTarget, "legacy_markdown_reservation")],
              ),
              retired: c5Retired(),
              roadmaps: c5Snapshots(
                { markdown: bytes("matrix\n"), document: c5V1("matrix", [c5ReadyRecord(C5_ID)]) },
                { markdown: testingSource, document: testingBaseDocument },
              ),
              registry: c5Registry({ kind: "commit", commit: C5_BASE }),
              legacy_title_bindings: [c5Title(testingReservation, testingSource)],
            }),
          }
          : c5LegacyPair(shadow);
        const candidate = testsTestingLegacyReference
          ? c5WithDebt({
            campaign: c5Campaign("authoritative", "shadow"),
            retired: c5Retired([c5Tombstone(testingTarget)]),
            roadmaps: c5Snapshots(
              { markdown: bytes("matrix\n"), document: matrixDocument },
              { markdown: testingCandidateSource, document: c5V0("testing", testingCandidateSource) },
            ),
            registry: c5Registry({ kind: "worktree" }),
          })
          : c5LegacyCandidate(shadow, mutation);
        const result = validateTransaction({
          scope: "all", against: C5_BASE, base: pair.revision,
          candidate,
        });
        const expectedTarget = testsTestingLegacyReference ? testingTarget : C5_ID;
        const expected = mutation === "selection" ? ["E-CAMPAIGN-TARGET", "selection[0]"]
          : mutation === "reservation" ? ["E-OWNER-DUPLICATE", `owner[\"${C5_ID}\"]`]
          : mutation === "bytes" ? ["E-TRANSACTION-CAMPAIGN", `owner[\"${C5_ID}\"].bound_source`]
          : mutation === "stale_shadow" ? ["E-SCHEMA-STATE", "campaign.matrix_authority"]
          : mutation === "alias" ? ["E-ALIAS-COLLISION", `alias[\"${expectedTarget}\"]`]
          : mutation === "relation" ? ["E-TRANSACTION-REFERENCE", `relation[\"${expectedTarget}\"]`]
          : mutation === "reference" ? ["E-TRANSACTION-REFERENCE", `reference[\"${expectedTarget}\"]`]
          : mutation === "missing_tombstone" ? ["E-TRANSACTION-OWNER", `owner[\"${C5_ID}\"]`]
          : mutation === "wrong_commit" ? ["E-RETIRED-HASH", `retired_ids.entry[\"${C5_ID}\"].last_active_at`]
          : mutation === "citation" ? ["E-TRANSACTION-CITATION", `citation[\"${C5_ID}\"]`]
          : ["E-OWNER-DUPLICATE", `owner[\"${C5_ID}\"]`];
        assert(result.issues.some((issue) => issue.code === expected[0] && issue.logical_path === expected[1]),
          `${id}/${mutation} must fail its focused obligation at ${expected.join("#")}: ${JSON.stringify(result.issues)}`);
        observeMatchingIssue(result.issues, expected[0] as RoadmapIssue["code"], expected[1]);
      }
      return true;
    }
    case "transaction_shadow_only_delivery_rejected": {
      const source = c5LegacySource();
      const base = c5WithDebt({
        campaign: c5Campaign("shadow", "legacy_markdown"), retired: c5Retired(),
        roadmaps: c5Snapshots({ markdown: source, document: c5V0("matrix", source, C5_ID) }),
        registry: c5Registry({ kind: "commit", commit: C5_BASE }),
      });
      const result = validateTransaction({ scope: "all", against: C5_BASE, base, candidate: c5LegacyCandidate(true) });
      assertIssue(result.issues, "E-TRANSACTION-ORIGIN", "shadow-only owner cannot retire before reviewed reservation/cutover");
      return true;
    }
    case "transaction_single_roadmap_owner_removal_rejected": {
      const baseIdentity = validateGlobalIdentity({ documents: [buildRoadmapIndexes(active).indexes.identity_inputs] });
      const candidateIdentity = validateGlobalIdentity({ documents: [] });
      const result = validateTransaction({
        scope: "matrix", against: C5_BASE,
        load_base: () => ({
          document: active, debt: c5Debt(active), identity: baseIdentity,
          registry: c5Registry({ kind: "commit", commit: C5_BASE }),
        }),
        candidate_document: empty, candidate_debt: c5Debt(empty),
        candidate_registry: c5Registry({ kind: "worktree" }),
        candidate_global_identity: candidateIdentity,
      });
      assertIssue(result.issues, "E-TRANSACTION-OWNER", "single-roadmap scope cannot authorize owner removal");
      return true;
    }
    case "transaction_tombstone_eligible_base_owner_set": {
      assertExactStrings(TOMBSTONE_ELIGIBLE_BASE_OWNER_KINDS, ["active_record", "current_guard", "legacy_markdown_reservation"], "eligible tombstone universe");
      const activeResult = positiveRetirement();
      assert(activeResult.issues.length === 0, "active_record origin must pass");
      const legacy = c5LegacyPair(false);
      assert(validateTransaction({ scope: "all", against: C5_BASE, base: legacy.revision, candidate: c5LegacyCandidate(false) }).issues.length === 0, "legacy reservation origin must pass");
      const guard: CurrentGuard = {
        id: C5_ID,
        replacement_pin: { kind: "gate", gate_id: "roadmap_projection_check", claim_md: bytes("old-pin") },
        owner_registry: "fixture-guards",
      };
      const base = withBaseRevision(c5ActiveRevision(empty, [], { guards: [guard] }));
      const candidate = c5ActiveRevision(empty, [], { retired: [c5Tombstone(C5_ID)] });
      const guardResult = validateTransaction({ scope: "all", against: C5_BASE, base, candidate });
      assert(guardResult.issues.length === 0, `current_guard origin accepts any resolving replacement, not byte-identical old pin: ${JSON.stringify(guardResult.issues)}`);
      return true;
    }
    case "transaction_tombstone_ineligible_base_owner_rejected": {
      assertExactStrings(TOMBSTONE_INELIGIBLE_ORIGIN_LABELS, ["shadow_only", "active_family", "alias", "selection", "preexisting_tombstone"], "ineligible tombstone universe");
      const shadowSource = c5LegacySource();
      const shadowBase = c5WithDebt({
        campaign: c5Campaign("shadow", "legacy_markdown"), retired: c5Retired(),
        roadmaps: c5Snapshots({ markdown: shadowSource, document: c5V0("matrix", shadowSource, C5_ID) }),
        registry: c5Registry({ kind: "commit", commit: C5_BASE }),
      });
      assertIssue(validateTransaction({ scope: "all", against: C5_BASE, base: shadowBase, candidate: c5LegacyCandidate(true) }).issues, "E-TRANSACTION-ORIGIN", "shadow-only origin must fail");
      assertIssue(c5AllTransaction(c5ActiveRevision(c5V1("matrix", [c5FamilyRecord(C5_ID)])), c5ActiveRevision(empty, [], { retired: [c5Tombstone(C5_ID)] })).issues, "E-TRANSACTION-ORIGIN", "active family origin must fail");
      const aliasOwner = "matrix.fixture-alias-owner" as RoadmapId;
      const aliasRecord = { ...c5ReadyRecord(aliasOwner), legacy_aliases: [C5_ID] };
      assertIssue(c5AllTransaction(
        c5ActiveRevision(c5V1("matrix", [aliasRecord])),
        c5ActiveRevision(c5V1("matrix", [aliasRecord]), [], { retired: [c5Tombstone(C5_ID)] }),
      ).issues, "E-TRANSACTION-ORIGIN", "a real base alias cannot authorize a tombstone");
      assertIssue(c5AllTransaction(
        c5ActiveRevision(empty, [c5Selection(C5_ID)]),
        c5ActiveRevision(empty, [], { retired: [c5Tombstone(C5_ID)] }),
      ).issues, "E-CAMPAIGN-TARGET", "a real but ownerless base selection fails before it can authorize a tombstone");
      assertIssue(c5AllTransaction(
        c5ActiveRevision(empty, [], { retired: [c5Tombstone(C5_ID, "f".repeat(40) as FullCommitId)] }),
        c5ActiveRevision(empty, [], { retired: [c5Tombstone(C5_ID)] }),
      ).issues, "E-RETIRED-REUSE", "a real pre-existing tombstone is immutable and cannot become a new origin");
      return true;
    }
  }
  return false;
}

function c5AgainstCase(id: C5SelfTestCaseId): boolean {
  if (!id.startsWith("against_")) return false;
  const roadmap: RoadmapName = id.includes("testing") ? "testing" : "matrix";
  const activeId = roadmap === "matrix" ? C5_ID : C5_TESTING_ID;
  const document = c5V1(roadmap, [c5ReadyRecord(activeId)]);
  const identity = validateGlobalIdentity({ documents: [buildRoadmapIndexes(document).indexes.identity_inputs] });
  const baseFacts = {
    document,
    debt: c5Debt(document),
    identity,
    registry: c5Registry({ kind: "commit", commit: C5_BASE }),
  };
  const scoped = () => validateTransaction({
    scope: roadmap,
    against: C5_BASE,
    load_base: () => baseFacts,
    candidate_document: document,
    candidate_debt: c5Debt(document),
    candidate_registry: c5Registry({ kind: "worktree" }),
    candidate_global_identity: identity,
  });
  const allSame = (
    campaign: CampaignDocumentV1,
    snapshots: Readonly<Record<RoadmapName, CampaignRoadmapSnapshot>>,
  ) => {
    const revision = c5WithDebt({
      campaign,
      retired: c5Retired(),
      roadmaps: snapshots,
      registry: c5Registry({ kind: "worktree" }),
    });
    return validateTransaction({ scope: "all", against: C5_BASE, base: withBaseRevision(revision), candidate: revision });
  };
  switch (id) {
    case "against_matrix_v1_debt_allowed":
    case "against_testing_v1_debt_allowed":
      assert(scoped().issues.length === 0, `${id} selected-only authoritative debt comparison must pass without another base roadmap`);
      return true;
    case "against_per_roadmap_does_not_load_other_base": {
      const other: RoadmapName = roadmap === "matrix" ? "testing" : "matrix";
      const byRoadmap = new Proxy({ [roadmap]: baseFacts } as Record<RoadmapName, typeof baseFacts>, {
        get(target, property, receiver) {
          if (property === other) throw new Error("unselected base read");
          return Reflect.get(target, property, receiver);
        },
      });
      const loaded: RoadmapName[] = [];
      const result = validateTransaction({
        scope: roadmap, against: C5_BASE,
        load_base: (selected) => {
          loaded.push(selected);
          return byRoadmap[selected];
        },
        candidate_document: document, candidate_debt: c5Debt(document),
        candidate_registry: c5Registry({ kind: "worktree" }), candidate_global_identity: identity,
      });
      assert(result.issues.length === 0 && loaded.length === 1 && loaded[0] === roadmap, "selected scope must call only the selected pure base loader once");
      return true;
    }
    case "against_per_roadmap_candidate_global_collision_rejected": {
      const guard: CurrentGuard = {
        id: activeId,
        replacement_pin: { kind: "gate", gate_id: "roadmap_projection_check", claim_md: bytes("pin") },
        owner_registry: "fixture-guards",
      };
      const collision = validateGlobalIdentity({
        documents: [buildRoadmapIndexes(document).indexes.identity_inputs], current_guards: [guard],
      });
      const result = validateTransaction({
        scope: roadmap, against: C5_BASE, load_base: () => baseFacts, candidate_document: document,
        candidate_debt: c5Debt(document), candidate_registry: c5Registry({ kind: "worktree" }),
        candidate_global_identity: collision,
      });
      assertIssue(result.issues, "E-OWNER-DUPLICATE", "candidate global collision must fail selected scope");
      return true;
    }
    case "against_per_roadmap_absent_selected_source_rejected":
    case "against_per_roadmap_shadow_selected_source_rejected": {
      const shadow = c5V0(roadmap, c5LegacySource(), activeId);
      const result = validateTransaction({
        scope: roadmap, against: C5_BASE,
        load_base: () => baseFacts,
        candidate_document: id.includes("shadow") ? shadow : undefined,
        candidate_debt: id.includes("shadow") ? c5Debt(shadow) : c5EmptyDebt(),
        candidate_registry: c5Registry({ kind: "worktree" }),
        candidate_global_identity: validateGlobalIdentity({ documents: [] }),
      });
      assertIssue(result.issues, "E-TRANSACTION-BASE", "selected scope requires authoritative-v1 source");
      return true;
    }
    case "against_per_roadmap_owner_change_requires_all": {
      const result = validateTransaction({
        scope: roadmap, against: C5_BASE, load_base: () => baseFacts, candidate_document: c5V1(roadmap),
        candidate_debt: c5Debt(c5V1(roadmap)), candidate_registry: c5Registry({ kind: "worktree" }),
        candidate_global_identity: validateGlobalIdentity({ documents: [] }),
      });
      assertIssue(result.issues, "E-TRANSACTION-OWNER", "owner change requires all scope");
      return true;
    }
    case "against_forged_debt_transition_facts_ignored": {
      const rawId = activeId;
      const semantic = c5ReadyRecord(rawId);
      const raw: RoadmapDocumentV1["records"][number] = {
        id: rawId,
        title: semantic.title,
        projection_group: semantic.projection_group,
        render_authority: "raw",
        source_block_md: bytes("summary"),
        span_ids: [],
        semantic_shadow: semantic.payload,
      };
      const baseDocument = c5V1(roadmap, [raw]);
      const candidateDocument = c5V1(roadmap, [semantic]);
      const baseIdentity = validateGlobalIdentity({ documents: [buildRoadmapIndexes(baseDocument).indexes.identity_inputs] });
      const candidateIdentity = validateGlobalIdentity({ documents: [buildRoadmapIndexes(candidateDocument).indexes.identity_inputs] });
      const forged = {
        scope: roadmap,
        against: C5_BASE,
        load_base: () => ({
          document: baseDocument,
          debt: c5Debt(baseDocument),
          identity: baseIdentity,
          registry: c5Registry({ kind: "commit", commit: C5_BASE }),
        }),
        candidate_document: candidateDocument,
        candidate_debt: c5Debt(candidateDocument),
        candidate_registry: c5Registry({ kind: "worktree" }),
        candidate_global_identity: candidateIdentity,
        debt_transition_facts: { restructure_count: 1, retirement_count: 0 },
      } as Parameters<typeof validateTransaction>[0];
      const result = validateTransaction(forged);
      const rejected = result.issues.find((issue) =>
        issue.code === "E-DEBT-OWNER-REGRESSION" && issue.logical_path === `record[${JSON.stringify(rawId)}]`
      );
      assert(rejected !== undefined, `caller-shaped transition facts must be ignored by the public transaction input: ${JSON.stringify(result.issues)}`);
      observeSelfTestIssue(rejected);
      return true;
    }
    case "against_all_testing_legacy_absent_valid": {
      const result = allSame(
        c5Campaign("legacy_markdown", "legacy_markdown"),
        c5Snapshots({ markdown: bytes("matrix\n") }, { markdown: bytes("testing\n") }),
      );
      assert(result.issues.length === 0, `legacy/absent authority state must pass: ${JSON.stringify(result.issues)}`);
      return true;
    }
    case "against_all_testing_shadow_valid": {
      const matrixBytes = bytes("matrix shadow\n");
      const testingBytes = bytes("testing shadow\n");
      const result = allSame(
        c5Campaign("shadow", "shadow"),
        c5Snapshots(
          { markdown: matrixBytes, document: c5V0("matrix", matrixBytes) },
          { markdown: testingBytes, document: c5V0("testing", testingBytes) },
        ),
      );
      assert(result.issues.length === 0, `testing shadow authority state must pass: ${JSON.stringify(result.issues)}`);
      return true;
    }
    case "against_all_testing_authoritative_valid": {
      const campaign = c5Campaign("authoritative", "authoritative");
      const snapshots = c5Snapshots(
        { markdown: bytes("matrix\n"), document: c5V1("matrix") },
        { markdown: bytes("testing\n"), document: c5V1("testing") },
      );
      const result = allSame(campaign, snapshots);
      assert(result.issues.length === 0, `testing authoritative state must pass: ${JSON.stringify(result.issues)}`);
      const revision = c5WithDebt({
        campaign, retired: c5Retired(), roadmaps: snapshots,
        registry: c5Registry({ kind: "worktree" }),
      });
      for (const missing of ["base", "candidate"] as const) {
        const transaction = validateTransaction({
          scope: "all", against: C5_BASE,
          base: missing === "base"
            ? { ...withBaseRevision(revision), debt: {} }
            : withBaseRevision(revision),
          candidate: missing === "candidate" ? { ...revision, debt: {} } : revision,
        });
        assert(transaction.issues.some((issue) => issue.code === "E-TRANSACTION-BASE" &&
          issue.logical_path === "debt.testing"), `${missing} missing relevant debt must fail at debt.testing`);
      }
      return true;
    }
    case "against_all_state_forbids_unexpected_toml": {
      const revision = c5WithDebt({
        campaign: c5Campaign("legacy_markdown", "legacy_markdown"), retired: c5Retired(),
        roadmaps: c5Snapshots({ markdown: bytes("matrix\n"), document: c5V0("matrix", bytes("matrix\n")) }),
        registry: fakeRegistryView(),
      });
      const result = validateLifecycleRevision(revision);
      assertIssue(result.issues, "E-SCHEMA-STATE", "legacy state forbids TOML");
      return true;
    }
    case "against_all_state_requires_shadow_toml":
    case "against_all_state_requires_authoritative_toml": {
      const state = id.includes("shadow") ? "shadow" as const : "authoritative" as const;
      const result = validateLifecycleRevision(c5WithDebt({
        campaign: c5Campaign(state, "legacy_markdown"), retired: c5Retired(),
        roadmaps: c5Snapshots({ markdown: bytes("matrix\n") }), registry: fakeRegistryView(),
      }));
      assertIssue(result.issues, "E-SOURCE-MISSING", `${state} state requires TOML`);
      return true;
    }
    case "against_all_base_uses_base_authority_metadata": {
      const matrixSource = bytes("matrix shadow\n");
      const source = bytes("testing shadow\n");
      const base = c5WithDebt({
        campaign: c5Campaign("shadow", "shadow"), retired: c5Retired(),
        roadmaps: c5Snapshots(
          { markdown: matrixSource, document: c5V0("matrix", matrixSource) },
          { markdown: source, document: c5V0("testing", source) },
        ),
        registry: c5Registry({ kind: "commit", commit: C5_BASE }),
      });
      const candidate = c5WithDebt({
        campaign: c5Campaign("authoritative", "authoritative"), retired: c5Retired(),
        roadmaps: c5Snapshots(
          { markdown: bytes("matrix\n"), document: c5V1("matrix") },
          { markdown: bytes("testing\n"), document: c5V1("testing") },
        ),
        registry: c5Registry({ kind: "worktree" }),
      });
      const result = validateTransaction({ scope: "all", against: C5_BASE, base, candidate });
      assert(result.issues.length === 0, `base must load from its own shadow metadata: ${JSON.stringify(result.issues)}`);
      return true;
    }
    case "against_all_wp4m_bootstrap_valid": {
      const matrixBytes = c5LegacySource();
      const baseDocument = c5V0("matrix", matrixBytes, C5_ID);
      const validBootstrapOwners = validateBootstrapShadowOwners(
        c5Snapshots({ markdown: matrixBytes, document: baseDocument }),
      );
      const validBootstrapCapability = campaignIdentityOwners(validBootstrapOwners);
      assert(validBootstrapOwners.issues.length === 0 && validBootstrapCapability !== undefined &&
        validateGlobalIdentity({
          documents: [], additional_owners: validBootstrapCapability,
        }).owners.get(C5_ID)?.owner_kind === "shadow_record_reservation",
      "issue-free bootstrap result must retain its valid opaque capability path");

      const bulletBytes = bytes("- **Lifecycle** — reviewed matrix bullet owner.\n");
      const bulletDocument = c5V0("matrix", bulletBytes, C5_ID);
      const bulletBootstrap = validateBootstrapShadowOwners(
        c5Snapshots({ markdown: bulletBytes, document: bulletDocument }),
      );
      const bulletCapability = campaignIdentityOwners(bulletBootstrap);
      assert(bulletBootstrap.issues.length === 0 && bulletCapability !== undefined &&
        validateGlobalIdentity({ documents: [], additional_owners: bulletCapability })
          .owners.get(C5_ID)?.owner_kind === "shadow_record_reservation",
      "strict reviewed matrix bullet owner must mint the bootstrap shadow capability");
      assert(
        validateLegacyTitleBinding(c5Reservation(C5_ID, bulletBytes), bulletBytes) === undefined,
        "matrix shadow bullet compatibility weakened the heading-only legacy reservation contract",
      );
      const malformedBullet = bytes("- Lifecycle — unreviewed plain bullet owner.\n");
      const malformedBootstrap = validateBootstrapShadowOwners(c5Snapshots({
        markdown: malformedBullet,
        document: c5V0("matrix", malformedBullet, C5_ID),
      }));
      assert(malformedBootstrap.issues.some((issue) => issue.code === "E-CAMPAIGN-TARGET" &&
        issue.logical_path === `bootstrap.matrix.record["${C5_ID}"]`),
      "unreviewed matrix bullet syntax must fail the bootstrap shadow title binding");

      const testingBytes = c5LegacySource();
      const invalidTestingDocument = c5V0("testing", testingBytes);
      invalidTestingDocument.document.frozen_source_sha256 = "0".repeat(64);
      const invalidBootstrapOwners = validateBootstrapShadowOwners(c5Snapshots(
        { markdown: matrixBytes, document: baseDocument },
        { markdown: testingBytes, document: invalidTestingDocument },
      ));
      assertIssue(invalidBootstrapOwners.issues, "E-SCHEMA-STATE",
        "independent invalid testing bootstrap metadata must fail alongside valid matrix owner evidence");
      assert(invalidBootstrapOwners.owners.length === 1 &&
        campaignIdentityOwners(invalidBootstrapOwners) === undefined,
      "bootstrap capability must remain unexposed when any independent bootstrap issue exists");
      assertIssue(validateGlobalIdentity({
        documents: [],
        additional_owners: { owner_count: invalidBootstrapOwners.owners.length } as never,
      }).issues, "E-OWNER-DUPLICATE", "structural substitute for invalid bootstrap result must not install owners");
      const invalidBootstrapCapability = campaignIdentityOwners(invalidBootstrapOwners);
      const invalidBootstrapIdentity = validateGlobalIdentity({
        documents: [],
        ...(invalidBootstrapCapability === undefined
          ? {}
          : { additional_owners: invalidBootstrapCapability }),
      });
      assert(invalidBootstrapIdentity.owners.size === 0,
        "undefined capability from invalid bootstrap result must not install owners");
      const candidateDocument = c5PromoteV0(baseDocument, new Map([[C5_ID, c5ReadyRecord(C5_ID).payload]]));
      const base = c5WithDebt({
        roadmaps: c5Snapshots({ markdown: matrixBytes, document: baseDocument }),
        registry: c5Registry({ kind: "commit", commit: C5_BASE }),
      });
      const candidate = c5WithDebt({
        campaign: c5Campaign("authoritative", "legacy_markdown", [], [c5Selection(C5_ID)]), retired: c5Retired(),
        roadmaps: c5Snapshots({ markdown: matrixBytes, document: candidateDocument }),
        registry: c5Registry({ kind: "worktree" }),
      });
      const result = validateTransaction({ scope: "all", against: C5_BASE, base, candidate, bootstrap: true });
      assert(result.issues.length === 0, `exact WP4M bootstrap must pass: ${JSON.stringify(result.issues)}`);
      const testingShadowBytes = c5LegacySource();
      const testingShadowDocument = c5V0("testing", testingShadowBytes, C5_TESTING_ID);
      const testingReservation = c5Reservation(C5_TESTING_ID, testingShadowBytes);
      const baseWithTestingShadow = c5WithDebt({
        roadmaps: c5Snapshots(
          { markdown: matrixBytes, document: baseDocument },
          { markdown: testingShadowBytes, document: testingShadowDocument },
        ),
        registry: c5Registry({ kind: "commit", commit: C5_BASE }),
      });
      const candidateRetainingTestingReservation = c5WithDebt({
        campaign: c5Campaign(
          "authoritative",
          "shadow",
          [testingReservation],
          [c5Selection(C5_ID), c5Selection(C5_TESTING_ID, "legacy_markdown_reservation")],
        ),
        retired: c5Retired(),
        roadmaps: c5Snapshots(
          { markdown: matrixBytes, document: candidateDocument },
          { markdown: testingShadowBytes, document: testingShadowDocument },
        ),
        registry: c5Registry({ kind: "worktree" }),
        legacy_title_bindings: [c5Title(testingReservation, testingShadowBytes)],
      });
      const retainedTestingReservation = validateTransaction({
        scope: "all",
        against: C5_BASE,
        base: baseWithTestingShadow,
        candidate: candidateRetainingTestingReservation,
        bootstrap: true,
      });
      assert(
        retainedTestingReservation.issues.length === 0,
        `WP4M bootstrap must retain a testing shadow owner when the candidate coalesces it with a same-ID reservation: ${JSON.stringify(retainedTestingReservation.issues)}`,
      );
      for (const missing of ["base", "candidate"] as const) {
        const transaction = validateTransaction({
          scope: "all",
          against: C5_BASE,
          base: missing === "base" ? { ...base, debt: {} } : base,
          candidate: missing === "candidate" ? { ...candidate, debt: {} } : candidate,
          bootstrap: true,
        });
        assert(transaction.issues.some((issue) => issue.code === "E-TRANSACTION-BASE" &&
          issue.logical_path === "debt.matrix"), `bootstrap requires ${missing} debt.matrix`);
      }
      const authoritativeTesting = c5WithDebt({
        campaign: c5Campaign("authoritative", "authoritative", [], [c5Selection(C5_ID)]),
        retired: c5Retired(),
        roadmaps: c5Snapshots(
          { markdown: matrixBytes, document: candidateDocument },
          { markdown: bytes("testing\n"), document: c5V1("testing") },
        ),
        registry: c5Registry({ kind: "worktree" }),
      });
      const rejectedTestingAuthority = validateTransaction({
        scope: "all",
        against: C5_BASE,
        base,
        candidate: authoritativeTesting,
        bootstrap: true,
      });
      assert(rejectedTestingAuthority.issues.some((issue) => issue.code === "E-TRANSACTION-BASE" &&
        issue.logical_path === "bootstrap"), "WP4M bootstrap must reject candidate testing authoritative state");

      const wrongOwnerDocument: RoadmapDocumentV0 = {
        ...baseDocument,
        spans: baseDocument.spans.map((span) => ({
          ...span,
          owner_id: span.owner_id === C5_ID ? "matrix.fixture-wrong-owner" : span.owner_id,
        })),
      };
      const wrongOwnerBase = c5WithDebt({
        roadmaps: c5Snapshots({ markdown: matrixBytes, document: wrongOwnerDocument }),
        registry: c5Registry({ kind: "commit", commit: C5_BASE }),
      });
      const wrongOwner = validateTransaction({
        scope: "all", against: C5_BASE, base: wrongOwnerBase, candidate, bootstrap: true,
      });
      assert(wrongOwner.issues.some((issue) => issue.code === "E-CAMPAIGN-TARGET" &&
        issue.logical_path === `bootstrap.matrix.record["${C5_ID}"]`),
      "bootstrap must reject one altered shadow span owner coordinate");

      const wrongFrozenDocument: RoadmapDocumentV1 = {
        ...candidateDocument,
        document: { ...candidateDocument.document, frozen_legacy_span_ids: [] },
      };
      const wrongFrozenCandidate = c5WithDebt({
        ...candidate,
        roadmaps: c5Snapshots({ markdown: matrixBytes, document: wrongFrozenDocument }),
      });
      const wrongFrozen = validateTransaction({
        scope: "all", against: C5_BASE, base, candidate: wrongFrozenCandidate, bootstrap: true,
      });
      assert(wrongFrozen.issues.some((issue) => issue.code === "E-DEBT-FROZEN-SET" &&
        issue.logical_path === "frozen_legacy_spans"),
      "bootstrap must reject one altered frozen-span coordinate");

      const candidateMatrixDebt = candidate.debt.matrix!;
      const firstOwner = candidateMatrixDebt.owners.entries().next().value!;
      const corruptedKey = {
        ...firstOwner[1].key,
        owner_field: `${firstOwner[1].key.owner_field}_wrong`,
      } as Parameters<typeof debtOwnerIndex>[0];
      const corruptedDebt: MigrationDebt = {
        ...candidateMatrixDebt,
        owners: new Map([
          ...candidateMatrixDebt.owners,
          [firstOwner[0], { ...firstOwner[1], key: corruptedKey }],
        ]),
      };
      const wrongDebt = validateTransaction({
        scope: "all",
        against: C5_BASE,
        base,
        candidate: { ...candidate, debt: { ...candidate.debt, matrix: corruptedDebt } },
        bootstrap: true,
      });
      assert(wrongDebt.issues.some((issue) => issue.code === "E-DEBT-BASE-MISMATCH" &&
        issue.logical_path === "candidate.owners"),
      "bootstrap must reject one altered migration-debt owner coordinate");
      return true;
    }
    case "against_all_post_activation_missing_root_rejected": {
      const candidate = c5WithDebt({
        campaign: c5Campaign("legacy_markdown", "legacy_markdown"), retired: c5Retired(),
        roadmaps: c5Snapshots({ markdown: bytes("matrix\n") }), registry: fakeRegistryView(),
      });
      for (const base of [
        c5WithDebt({ retired: c5Retired(), roadmaps: c5Snapshots({ markdown: bytes("matrix\n") }), registry: c5Registry({ kind: "commit", commit: C5_BASE }) }),
        c5WithDebt({ campaign: c5Campaign("legacy_markdown", "legacy_markdown"), roadmaps: c5Snapshots({ markdown: bytes("matrix\n") }), registry: c5Registry({ kind: "commit", commit: C5_BASE }) }),
      ]) {
        const result = validateTransaction({ scope: "all", against: C5_BASE, base, candidate });
        assertIssue(result.issues, "E-SOURCE-MISSING", "post-activation missing campaign or retired root must fail");
      }
      return true;
    }
    case "against_all_shadow_to_authoritative_transfer": {
      const pair = c5LegacyPair(true);
      const shadow = pair.revision.roadmaps.matrix.document as RoadmapDocumentV0;
      const promoted = c5PromoteV0(shadow, new Map([[C5_ID, c5ReadyRecord(C5_ID).payload]]));
      const candidate = c5ActiveRevision(promoted, [c5Selection(C5_ID)]);
      const result = validateTransaction({ scope: "all", against: C5_BASE, base: pair.revision, candidate });
      assert(result.issues.length === 0, `shadow -> authoritative transfer must pass: ${JSON.stringify(result.issues)}`);
      const shadowOnlyBase = c5WithDebt({
        campaign: c5Campaign("shadow", "legacy_markdown"), retired: c5Retired(),
        roadmaps: pair.revision.roadmaps,
        registry: c5Registry({ kind: "commit", commit: C5_BASE }),
      });
      const shadowOnly = validateTransaction({ scope: "all", against: C5_BASE, base: shadowOnlyBase, candidate });
      assert(shadowOnly.issues.length === 0 && shadowOnly.authority_transfers.includes(C5_ID), `shadow-only same-ID cutover must pass exact owner/span/debt transfer: ${JSON.stringify(shadowOnly.issues)}`);
      for (const [label, baseRevision] of [
        ["reservation", pair.revision],
        ["shadow_only", shadowOnlyBase],
      ] as const) {
        for (const missing of ["base", "candidate"] as const) {
          const transaction = validateTransaction({
            scope: "all",
            against: C5_BASE,
            base: missing === "base" ? { ...baseRevision, debt: {} } : baseRevision,
            candidate: missing === "candidate" ? { ...candidate, debt: {} } : candidate,
          });
          assert(transaction.issues.some((issue) => issue.code === "E-TRANSACTION-BASE" &&
            issue.logical_path === "debt.matrix"), `${label} cutover requires ${missing} debt.matrix`);
        }
      }
      return true;
    }
    case "against_all_reverse_authority_rejected": {
      const source = c5LegacySource();
      const reservation = c5Reservation(C5_ID, source);
      const base = withBaseRevision(c5ActiveRevision(c5V1("matrix", [c5ReadyRecord(C5_ID)])));
      const candidate = c5WithDebt({
        campaign: c5Campaign("shadow", "legacy_markdown", [reservation]), retired: c5Retired(),
        roadmaps: c5Snapshots({ markdown: source, document: c5V0("matrix", source, C5_ID) }),
        registry: fakeRegistryView(), legacy_title_bindings: [c5Title(reservation, source)],
      });
      const result = validateTransaction({ scope: "all", against: C5_BASE, base, candidate });
      assertIssue(result.issues, "E-CAMPAIGN-TRANSITION", "reverse authority must fail");
      const stateRevision = (
        state: "legacy_markdown" | "shadow" | "authoritative",
        baseRevision: boolean,
      ) => c5WithDebt({
        campaign: c5Campaign(state, "legacy_markdown"), retired: c5Retired(),
        roadmaps: c5Snapshots({
          markdown: source,
          ...(state === "shadow" ? { document: c5V0("matrix", source) } :
            state === "authoritative" ? { document: c5V1("matrix") } : {}),
        }),
        registry: c5Registry(baseRevision ? { kind: "commit", commit: C5_BASE } : { kind: "worktree" }),
      });
      const forward = validateTransaction({
        scope: "all", against: C5_BASE,
        base: stateRevision("legacy_markdown", true), candidate: stateRevision("shadow", false),
      });
      assert(forward.issues.length === 0, `legacy -> shadow one-step transition must pass: ${JSON.stringify(forward.issues)}`);
      for (const [from, to] of [
        ["shadow", "legacy_markdown"],
        ["authoritative", "shadow"],
        ["legacy_markdown", "authoritative"],
      ] as const) {
        const rejected = validateTransaction({
          scope: "all", against: C5_BASE,
          base: stateRevision(from, true), candidate: stateRevision(to, false),
        });
        assertIssue(rejected.issues, "E-CAMPAIGN-TRANSITION", `${from} -> ${to} authority transition must fail`);
      }
      return true;
    }
  }
  return false;
}

function executeC5(id: C5SelfTestCaseId, context?: SelfTestContext): void {
  assert(
    c5CampaignCase(id) || c5RetiredCase(id) || c5IdentityReservationCase(id) ||
      c5TransactionCase(id, context) || c5AgainstCase(id),
    `C5 case ${id} has no executable proof`,
  );
}

const CASE_POLARITY = new Map<PermanentIdSelfTestCaseId, "positive" | "negative">([
  ["id_grammar_accept", "positive"],
  ["id_reserved_tokens", "negative"],
  ["id_numeric_legacy_tokens", "negative"],
  ["id_namespace_mismatch", "negative"],
  ["id_is_opaque_to_consumers", "positive"],
]);

assert(CASE_POLARITY.size === PERMANENT_ID_SELFTEST_CASE_IDS.length, "permanent-ID polarity metadata must cover every case");

const JOIN_CASE_IDS = REQUIRED_IDENTITY_SELFTEST_CASE_IDS.slice(
  PERMANENT_ID_SELFTEST_CASE_IDS.length,
  43,
) as readonly JoinSelfTestCaseId[];

const C5_CASE_IDS = REQUIRED_IDENTITY_SELFTEST_CASE_IDS.slice(43);
type C5SelfTestCaseId = (typeof C5_CASE_IDS)[number];

const POSITIVE_JOIN_CASES = new Set<JoinSelfTestCaseId>([
  "reference_each_kind",
  "citation_inventory_tracks_source_byte_spans",
  "citation_inventory_sorted_by_path_and_span",
  "citation_inventory_multiple_occurrences",
  "citation_inventory_draft_excluded",
  "citation_inventory_nul_binary_excluded",
  "citation_inventory_base_revision_isolated",
  "test_symbol_fact_top_level_direct_test",
  "test_symbol_fact_declared_module_prefix",
  "test_symbol_fact_inline_module_path",
  "test_symbol_fact_attribute_between_test_and_fn",
  "test_symbol_fact_macro_template_excluded",
  "test_symbol_fact_macro_invocation_excluded",
  "test_symbol_fact_comments_and_strings_excluded",
  "test_symbol_fact_undeclared_file_excluded",
  "test_symbol_fact_id_derivation_exact",
  "test_symbol_fact_base_revision_isolated",
]);

function joinCategory(id: JoinSelfTestCaseId): SelfTestCase["category"] {
  if (id.startsWith("citation_") || id.startsWith("test_symbol_")) return "repository-facts";
  if (id.startsWith("identity_")) return "identity-retirement";
  return "references-relations";
}

function joinSubcases(id: JoinSelfTestCaseId): readonly string[] | undefined {
  if (id === "reference_each_kind" || id === "negative_reference_registry_enumeration") {
    return REFERENCE_KIND_REGISTRY;
  }
  if (id === "identity_alias_collision") return ["alias_alias", "alias_first_class"];
  if (id === "test_symbol_fact_macro_template_excluded") return ["brace", "bracket", "parenthesis"];
  if (id === "test_symbol_fact_comments_and_strings_excluded") {
    return ["block_comment", "character", "line_comment", "raw_string", "string"];
  }
  return undefined;
}

const JOIN_SELFTEST_CASES: readonly SelfTestCase[] = Object.freeze(JOIN_CASE_IDS.map((id) => ({
  id,
  category: joinCategory(id),
  run(context: SelfTestContext): SelfTestResult {
    const polarity = POSITIVE_JOIN_CASES.has(id) ? "positive" as const : "negative" as const;
    const subcases = joinSubcases(id);
    try {
      executeJoin(id);
      return { ok: true, polarity, subcases };
    } catch (error) {
      return {
        ok: false,
        polarity,
        issues: [{
          code: "E-SELFTEST-CASE",
          source: "<selftest>",
          logical_path: id,
          message: error instanceof Error ? error.message : String(error),
          exit: 1,
        }],
        subcases,
      };
    }
  },
})));

const POSITIVE_C5_CASES = new Set<C5SelfTestCaseId>([
  "campaign_direct_matrix", "campaign_direct_testing", "campaign_legacy_matrix",
  "campaign_legacy_testing", "campaign_testing_survives_matrix_cutover",
  "campaign_state_transition", "campaign_deselect_active_allowed",
  "campaign_allowlist_exhaustive", "campaign_reservation_owns_id_without_selection",
  "campaign_deselect_keeps_reservation", "retired_new_last_active_matches_against",
  "transaction_complete_tombstone", "transaction_complete_guard_transfer",
  "transaction_deselect_active_allowed", "transaction_legacy_cutover_transfer_selected",
  "transaction_legacy_cutover_transfer_unselected", "transaction_legacy_delivery_without_shadow",
  "transaction_legacy_delivery_with_shadow", "identity_shadow_record_reserves_id",
  "identity_reservation_shadow_coalesces", "against_matrix_v1_debt_allowed",
  "identity_evidence_digest_binary_inputs",
  "against_testing_v1_debt_allowed", "against_per_roadmap_does_not_load_other_base",
  "against_all_testing_legacy_absent_valid", "against_all_testing_shadow_valid",
  "against_all_testing_authoritative_valid", "against_all_base_uses_base_authority_metadata",
  "against_all_wp4m_bootstrap_valid", "against_all_shadow_to_authoritative_transfer",
  "transaction_tombstone_eligible_base_owner_set", "retired_test_symbol_requires_exact_id_and_symbol",
]);

function c5Subcases(id: C5SelfTestCaseId): readonly string[] | undefined {
  if (id === "identity_reservation_shadow_third_owner_rejected") {
    return ["A-A", "A-R", "A-S", "A-G", "A-T", "R-R", "R-R-cross-namespace", "R-S", "R-S-mismatch", "R-G", "R-T", "S-S", "S-G", "S-T", "G-G", "G-T", "T-T", "R-S-third-A", "R-S-third-R", "R-S-third-S", "R-S-third-G", "R-S-third-T", "alias-A", "alias-R", "alias-S", "alias-G", "alias-T", "reversal"];
  }
  if (id === "identity_evidence_digest_binary_inputs") {
    return ["equal_length_views", "raw_mutation", "shared_inputs"];
  }
  if (id === "transaction_tombstone_eligible_base_owner_set") {
    return ["active_record", "current_guard", "legacy_markdown_reservation"];
  }
  if (id === "transaction_tombstone_ineligible_base_owner_rejected") {
    return ["shadow_only", "active_family", "alias", "selection", "preexisting_tombstone"];
  }
  if (id === "retired_unresolved_replacement") {
    return ["gate", "test_symbol", "file_heading", "draft", "duplicate", "external", "bare_title"];
  }
  if (id === "retired_bad_hash_length_case") return ["character", "case", "sha1_length", "sha256_length", "object_format_length"];
  if (id === "retired_preexisting_keeps_base") return ["gate", "test_symbol", "file_heading", "immutable", "reversal"];
  if (id === "retired_test_symbol_requires_exact_id_and_symbol") return ["exact", "id_mutation", "symbol_mutation", "duplicate"];
  if (id === "campaign_active_work_only") return ["decision", "family", "current_guard", "tombstone", "shadow_only", "raw_without_work_shadow", "missing"];
  if (id === "campaign_programmatic_impossible_authority_tuple") return ["campaign", "lifecycle", "transition"];
  if (id === "campaign_state_fields") return ["in_progress_missing_both", "in_progress_missing_pickup", "selected_forbids_pickup"];
  if (id === "campaign_state_transition") return ["selected_to_in_progress", "in_progress_to_selected", "removal", "invalid"];
  if (id === "campaign_allowlist_exhaustive") return ["exact", "missing", "duplicate", "extra"];
  if (id === "campaign_selection_target_kind_matches_owner") {
    return ["active_owner_tag", "valid_owner_invalid_selection", "no_capability", "structural_substitute", "undefined_substitute", "valid_capability"];
  }
  if (id === "campaign_legacy_digest_title_span") {
    return ["title", "range", "digest", "forged_title_capability", "shadow_owner", "shadow_digest", "shadow_range", "shadow_title", "shadow_index", "shadow_span_exhaustiveness"];
  }
  if (id === "identity_reservation_shadow_binding_mismatch") {
    return ["cloned_capability", "owner_array", "sole_reservation", "sole_shadow", "shadow_evidence_mutation", "reservation_evidence_mutation"];
  }
  if (id === "transaction_legacy_delivery_shadow_owner_survives") return ["shadow", "active", "guard", "alias"];
  if (id === "transaction_legacy_delivery_bound_span_survives") return ["moved_complete_slice", "stale_shadow_digest"];
  if (id === "transaction_legacy_delivery_live_repo_citation") return ["repository_citation", "relation_endpoint", "typed_reference"];
  if (id === "transaction_complete_guard_transfer") {
    return [
      "four_exact_debt_atoms", "five_exact_child_ids_and_kinds", "all_child_guards",
      "missing_base_debt", "missing_candidate_debt",
      "same_active_axis", "same_active_axis_value", "same_active_evidence", "same_active_cell",
      "same_active_exclusion", "wrong_provider_kind", "gate_pin", "test_symbol_pin",
      "file_heading_pin", "empty_claim", "draft_heading", "duplicate_provider", "stub_gate",
      "valid_input_reversal", "forged", "empty", "wrong_family_object", "wrong_guard_object",
      "direction_reversal", "overlap", "unrelated_capability", "attached_source_span",
      "unrelated_source_span", "unrelated_owner_regression", "independent_growth",
      "category_hide", "frozen_growth", "lifecycle_guard_retirement", "guard_retirement_forward",
      "guard_retirement_reverse", "guard_restructure_forward", "guard_restructure_reverse",
      "guard_semantic_conversion_forward", "guard_semantic_conversion_reverse",
    ];
  }
  if (id === "transaction_complete_tombstone") return ["complete", "missing_base_debt", "missing_candidate_debt"];
  if (id === "transaction_partial_guard") {
    return ["missing", "wrong_id", "unresolved_pin", "simultaneous_tombstone", "leftover_family", "missing_child_guard", "unused_guard", "future_reuse"];
  }
  if (id === "transaction_full_hash_git_integration") {
    return [
      "scratch_lifecycle", "argv", "unsigned", "sha1", "sha256", "durable_corpus_facts",
      "crlf_byte_citation", "revision_isolation", "retirement_closure", "heading_text_precondition", "wrong_base_revision",
      "candidate_commit_rejected", "abbreviated",
    ];
  }
  if (id === "against_all_testing_authoritative_valid") return ["complete_debt", "missing_base_debt", "missing_candidate_debt"];
  if (id === "against_all_wp4m_bootstrap_valid") {
    return ["valid_capability", "independent_issue", "no_capability", "structural_substitute", "undefined_substitute", "complete", "testing_shadow_reservation_retained", "missing_base_debt", "missing_candidate_debt", "testing_authoritative", "owner_coordinate", "frozen_span_coordinate", "debt_coordinate"];
  }
  if (id === "against_all_shadow_to_authoritative_transfer") {
    return ["reservation_shadow_pair", "shadow_only", "reservation_missing_base_debt", "reservation_missing_candidate_debt", "shadow_missing_base_debt", "shadow_missing_candidate_debt"];
  }
  if (id === "against_all_reverse_authority_rejected") return ["legacy_to_shadow", "shadow_to_authoritative", "authoritative_to_shadow", "shadow_to_legacy", "legacy_to_authoritative_skipped"];
  if (id === "against_all_post_activation_missing_root_rejected") return ["campaign", "retired"];
  return undefined;
}

const C5_SELFTEST_CASES: readonly SelfTestCase[] = Object.freeze(C5_CASE_IDS.map((id) => ({
  id,
  category: id.startsWith("campaign_") ? "campaign" as const : "identity-retirement" as const,
  run(context: SelfTestContext): SelfTestResult {
    const polarity = POSITIVE_C5_CASES.has(id) ? "positive" as const : "negative" as const;
    const subcases = c5Subcases(id);
    try {
      executeC5(id, context);
      return { ok: true, polarity, subcases };
    } catch (error) {
      return {
        ok: false,
        polarity,
        issues: [{
          code: "E-SELFTEST-CASE",
          source: "<selftest>",
          logical_path: id,
          message: error instanceof Error ? error.message : String(error),
          exit: 1,
        }],
        subcases,
      };
    }
  },
})));

export const IDENTITY_SELFTEST_CASES: readonly SelfTestCase[] = Object.freeze(
  [...PERMANENT_ID_SELFTEST_CASE_IDS.map((id) => ({
    id,
    category: "identity-retirement" as const,
    run(context: SelfTestContext): SelfTestResult {
      const polarity = CASE_POLARITY.get(id)!;
      try {
        execute(
          id,
          id === "id_is_opaque_to_consumers"
            ? identityFixtureBundleFromContext(context)
            : undefined,
        );
        return { ok: true, polarity };
      } catch (error) {
        return {
          ok: false,
          polarity,
          issues: [{
            code: "E-SELFTEST-CASE",
            source: "<selftest>",
            logical_path: id,
            message: error instanceof Error ? error.message : String(error),
            exit: 1,
          }],
        };
      }
    },
  })), ...JOIN_SELFTEST_CASES, ...C5_SELFTEST_CASES],
);

export function runIdentitySelfTests(
  fixtureBundle: IdentityFixtureBundle,
  context: SelfTestContext,
): { readonly executed: number } {
  for (const id of PERMANENT_ID_SELFTEST_CASE_IDS) execute(id, fixtureBundle);
  for (const id of JOIN_CASE_IDS) executeJoin(id);
  for (const id of C5_CASE_IDS) executeC5(id, context);
  return { executed: REQUIRED_IDENTITY_SELFTEST_CASE_IDS.length };
}
