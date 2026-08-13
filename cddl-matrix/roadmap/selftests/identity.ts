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
  Reference,
  Relation,
  RoadmapDocument,
  RoadmapDocumentV3,
  SemanticAuthorityRecord,
} from "../model/documents.ts";
import type { RegistryView } from "../adapters/types.ts";
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
  REFERENCE_KIND_REGISTRY,
  validateSemanticRoadmapJoins,
  validateRoadmapReferences,
} from "../references.ts";
import {
  gateFact,
  headingFact,
  scanRoadmapCitations,
  scanRoadmapMarkdownFacts,
  type TrackedTextInput,
} from "../repository_facts.ts";
import { extractRustTestSymbols } from "../rust_symbols.ts";
import { deriveRelationViews, validateRelations } from "../relations.ts";
import {
  identityOwnerClaimKey,
  validateGlobalIdentity,
} from "../identity.ts";
import { buildExpectedChunks, createExpectedByteView, type CompletedRenderIr, type RenderChunk } from "../render_ir.ts";
import { resolveSectionPlan } from "../manifest.ts";
import { renderCanonicalSemanticRecord } from "../adapters/engine.ts";
import { codePointSort } from "../kernel.ts";

export const IDENTITY_ROADMAP_FIXTURE_PATHS = Object.freeze([
  "all-fields/matrix-v3.toml",
  "all-fields/testing-v3.toml",
  "positive/small-matrix-v3.toml",
  "positive/small-testing-v3.toml",
] as const);

export type IdentityRoadmapFixturePath =
  (typeof IDENTITY_ROADMAP_FIXTURE_PATHS)[number];

const IDENTITY_FIXTURE_ROOT = "cddl-matrix/roadmap/fixtures" as RepoPath;

export interface IdentityFixtureBundle {
  readonly file_count: 4;
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
    "identity fixture bundle must contain exactly four roadmap files",
  );
  const snapshots = new Map<IdentityRoadmapFixturePath, Uint8Array>();
  for (const path of IDENTITY_ROADMAP_FIXTURE_PATHS) {
    const value = files.get(path);
    assert(value !== undefined && value.byteLength > 0, `identity fixture bundle is missing ${path}`);
    snapshots.set(path, new Uint8Array(value));
  }
  const bundle: IdentityFixtureBundle = Object.freeze({ file_count: 4 });
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
  "identity_alias_collision",
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
  generated_slot: [],
  part: [],
  reference: [],
  section: ["fixture"],
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

function smallFixtureExpectation(roadmap: RoadmapName): FixtureIndexExpectation {
  const recordId = `${roadmap}.fixture-small-semantic`;
  return {
    roadmap,
    providers: { record: [recordId] },
    payloads: { "semantic:work": [recordId] },
    id_use_roles: { manifest_record: 1, provider: 1 },
    semantic_targets: {},
    reference_uses: {},
    aliases: [],
    subordinate: { ...EMPTY_SUBORDINATE },
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
  providers: { record: MATRIX_RECORD_IDS },
  payloads: {
    "semantic:control": suffixed("matrix.fixture-control-", "abcdefg"),
    "semantic:decision": suffixed("matrix.fixture-choice-", "abcd"),
    "semantic:evidence": suffixed("matrix.fixture-evidence-", "abcdefghijklm"),
    "semantic:matrix_external_closeout": suffixed("matrix.fixture-upstream-", "abc"),
    "semantic:matrix_policy": [
      ...suffixed("matrix.fixture-policy-", "abc"),
      "matrix.fixture-semantic-owner",
    ],
    "semantic:signal": suffixed("matrix.fixture-signal-", "abcdefghijk"),
    "semantic:work": [
      "matrix.fixture-raw-owner",
      ...suffixed("matrix.fixture-task-", "abcdefghij"),
    ],
  },
  id_use_roles: {
    manifest_record: 53,
    parent_record: 1,
    provider: 53,
    reference_source: 12,
    reference_target: 1,
    relation_source: 10,
    relation_target: 10,
    semantic_target: 18,
  },
  semantic_targets: {
    "matrix.fixture-control-a": 2,
    "matrix.fixture-evidence-a": 7,
    "matrix.fixture-evidence-b": 2,
    "matrix.fixture-evidence-c": 3,
    "matrix.fixture-evidence-d": 1,
    "matrix.fixture-signal-f": 1,
    "matrix.fixture-task-b": 2,
  },
  reference_uses: {
    "ref-commit": 2,
    "ref-consumer": 2,
    "ref-feature": 1,
    "ref-file": 12,
    "ref-gate": 6,
    "ref-issue": 17,
    "ref-release": 1,
    "ref-roadmap": 2,
    "ref-spec": 3,
    "ref-test": 2,
  },
  aliases: [],
  subordinate: {
    generated_slot: ["constraint", "counts", "emission", "ops"],
    part: ["fixture-part"],
    reference: [
      "ref-cell",
      "ref-commit",
      "ref-consumer",
      "ref-feature",
      "ref-file",
      "ref-gate",
      "ref-issue",
      "ref-release",
      "ref-roadmap",
      "ref-role",
      "ref-spec",
      "ref-test",
    ],
    section: ["fixture"],
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
  },
  relations: MATRIX_RELATIONS,
  relations_by_source: MATRIX_RELATIONS_BY_SOURCE,
  relations_by_target: MATRIX_RELATIONS_BY_TARGET,
};

const TESTING_RECORD_IDS = [
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
  "testing.fixture-task-ready",
];

const TESTING_ALL_FIELDS_EXPECTATION: FixtureIndexExpectation = {
  roadmap: "testing",
  providers: { record: TESTING_RECORD_IDS },
  payloads: {
    "semantic:control": ["testing.fixture-control-review"],
    "semantic:evidence": ["testing.fixture-evidence-gate"],
    "semantic:signal": ["testing.fixture-signal-escalation"],
    "semantic:testing_cost": [
      "testing.fixture-all-fields-semantic",
      "testing.fixture-cost-historical",
    ],
    "semantic:testing_incident": [
      "testing.fixture-incident-attributed",
      "testing.fixture-incident-historical",
      "testing.fixture-incident-live",
    ],
    "semantic:testing_operational_watch": [
      "testing.fixture-operational-attributed",
      "testing.fixture-operational-retire-pending",
      "testing.fixture-operational-watching",
    ],
    "semantic:testing_system_admission": [
      "testing.fixture-admission-independent",
      "testing.fixture-admission-silent",
    ],
    "semantic:work": [
      "testing.fixture-all-fields-raw",
      "testing.fixture-task-ready",
    ],
  },
  id_use_roles: {
    manifest_record: 15,
    provider: 15,
    reference_source: 9,
    semantic_target: 10,
  },
  semantic_targets: {
    "testing.fixture-admission-independent": 1,
    "testing.fixture-evidence-gate": 6,
    "testing.fixture-incident-attributed": 1,
    "testing.fixture-incident-live": 1,
    "testing.fixture-signal-escalation": 1,
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
    ],
  },
  references_by_kind: {
    file_heading: [
      "control-review-heading",
      "incident-attributed-rule",
      "incident-historical-rule",
      "operational-attributed-rule",
      "operational-retire-rule",
    ],
    gate: [
      "evidence-gate-target",
      "incident-historical-retirement",
      "operational-retire-pin",
      "semantic-cost-gate",
    ],
  },
  relations: [],
  relations_by_source: {},
  relations_by_target: {},
};

const EXPECTED_FIXTURE_INDEXES: Readonly<Record<
  IdentityRoadmapFixturePath,
  FixtureIndexExpectation
>> = {
  "all-fields/matrix-v3.toml": MATRIX_ALL_FIELDS_EXPECTATION,
  "all-fields/testing-v3.toml": TESTING_ALL_FIELDS_EXPECTATION,
  "positive/small-matrix-v3.toml": smallFixtureExpectation("matrix"),
  "positive/small-testing-v3.toml": smallFixtureExpectation("testing"),
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
    if (path === "all-fields/matrix-v3.toml") {
      assert(
        new Set(references.map((reference) => reference.kind)).size === REFERENCE_KIND_REGISTRY.length,
        "matrix all-fields must provide one committed template for every Reference kind",
      );
      committedReferenceTemplates = Object.freeze([...references]);
    }
    const registry = registryViewForReferences(references);
    assert(
      validateRoadmapReferences(result.indexes, registry, {
        source: path,
        providers: MATRIX_ADAPTER.referenceProviders(registry),
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
      const candidates = [...committedReferenceTemplates]
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
            path,
          );
          assert(providerRegistry.issues.length === 0, `${path} mutation provider composition must remain valid`);
          const selectedProvider = providerRegistry.by_kind.get(replacement.kind);
          assert(selectedProvider !== undefined, `${path} replacement kind ${replacement.kind} must have a provider`);
          if (!selectedProvider.resolve(replacement as never, mutatedRegistry).resolved) continue;
          const mutationIssues = validateRoadmapReferences(mutated, mutatedRegistry, {
            source: path,
            providers: adapterProviders,
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
      // A semantic join field closes over the payload kind of the record its target belongs to.
      // Repoint the target at a record of a different payload kind, so the fault injection
      // reaches the join policy itself rather than depending on a structural role happening to
      // cite the same ID.
      const ownerPayload = result.indexes.payload_records.get(target.owner_record_id);
      const foreignOwner = [...result.indexes.payload_records.values()].find((provider) =>
        provider.payload.kind !== ownerPayload?.payload.kind
      );
      assert(
        foreignOwner !== undefined,
        `${path} needs a second payload kind to move a semantic target out of its universe`,
      );
      const wrongKind = new Map(result.indexes.first_class);
      wrongKind.set(semanticUse.id, {
        ...target,
        owner_record_id: foreignOwner.record.id,
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
    if (path === "all-fields/matrix-v3.toml") {
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
      // The nested transition forms (Packet 3A-2) make a wrong-kind target unrepresentable for
      // blocked/armed work, closeouts, and policies; the one remaining matrix citation surface is
      // deferred work's standalone-signal transition list.
      assert(
        mutateSemanticTarget("fixture-task-g\"].payload.transition_ids", (payload) => ({
          ...payload,
          transition_kind: "cadence",
        })).some((value) => value.code === "E-REFERENCE-FORBIDDEN" && value.logical_path.includes("fixture-task-g")),
        "deferred work must reject a cadence target in place of its reopening signal",
      );
      assert(
        mutateSemanticTarget("fixture-task-f\"].payload.control_ids", (payload) => ({
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
  const records = recordIds.map((id, index) => `
[[record]]
id = "${id}"
title = "Record ${index}"
legacy_aliases = ["Legacy ${String.fromCharCode(90 - index)}"]

[record.payload]
kind = "work"
detail_md = """R"""
work_state = "ready"
work_intent = "build_capability"
work_kind = "feature"
risk = "cosmetic"
acceptance_md = """Accepted."""
priority_rationale_md = """Normal."""
`).join("");
  const entries = recordIds.map((id) => `  ${JSON.stringify(id)},\n`).join("");
  return decodeRoadmapSource(bytes(`[document]
schema_version = 3
roadmap = "${roadmap}"
source_path = "cddl-matrix/ROADMAP.md"
projection_path = "cddl-matrix/ROADMAP.md"

[[section]]
section_id = "fixture"
title = "Fixture"
legacy_aliases = ["Legacy Section"]
body_md = """S"""
entries = [
${entries}]
${records}`), "<identity-selftest>", roadmap, false);
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
      logical_path: `record[${JSON.stringify(consumer)}].payload.reference_ids`,
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
      authority: "semantic",
      logical_path: `record[${JSON.stringify(consumer)}].payload`,
    }]]),
  } as unknown as RoadmapIndexes;
  const providerClaims = Object.freeze([
    ...createCoreReferenceProviders(joined.first_class),
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
    guard_role: "generic",
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
      const additionalGuards: CurrentGuard[] = [
        { ...guard, id: brandedRoadmapId("matrix.fixture-extra-a") },
        { ...guard, id: brandedRoadmapId("matrix.fixture-extra-b") },
      ];
      const forward = validateGlobalIdentity({
        documents: [index.identity_inputs, testing.identity_inputs],
        current_guards: [tiedGuardB, tiedGuardA, ...additionalGuards],
      });
      const reversed = validateGlobalIdentity({
        documents: [testing.identity_inputs, index.identity_inputs],
        current_guards: [...additionalGuards].reverse().concat([tiedGuardA, tiedGuardB]),
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
      const expectedBytes = bytes("# Expected heading\nbody ");
      const expectedView = createExpectedByteView([
        expectedBytes.subarray(0, 5),
        expectedBytes.subarray(5),
      ].map((chunk, plan_index): RenderChunk => ({
        plan_index,
        owner: { kind: "part", id: `expected-${plan_index}`, field: "body_md" },
        bytes: chunk,
        consumed_fields: ["body_md"],
      })));
      const expectedFacts = scanRoadmapMarkdownFacts("README.md" as RepoPath, expectedView);
      assert(
        expectedFacts.issues.length === 0 && expectedFacts.headings.length === 1 &&
          expectedFacts.headings[0]!.heading === "Expected heading",
        "immutable Markdown scan did not consume a multi-chunk ExpectedByteView directly",
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

function c5ReadyRecord(id: RoadmapId, workKind: WorkKind = "feature"): SemanticAuthorityRecord {
  return {
    id,
    title: "Lifecycle",
    payload: {
      kind: "work",
      work_state: "ready",
      work_intent: "build_capability",
      work_kind: workKind,
      risk: "compile_failure",
      acceptance_md: bytes("acceptance"),
      priority_rationale_md: bytes("priority"),
    },
  };
}

function c5Guard(id: RoadmapId, gateId = "roadmap_projection_check"): CurrentGuard {
  return {
    id,
    replacement_pin: { kind: "gate", gate_id: gateId, claim_md: bytes("guard") },
    owner_registry: "fixture-guards",
    guard_role: "generic",
  };
}

function c5Document(
  roadmap: RoadmapName,
  records: readonly RoadmapDocumentV3["records"][number][] = [],
  relations: readonly Relation[] = [],
  references: readonly Reference[] = [],
): RoadmapDocumentV3 {
  return {
    document: {
      schema_version: 3,
      roadmap,
      source_path: c5SourcePath(roadmap),
      projection_path: c5Path(roadmap),
    },
    sections: [], records: [...records], parts: [],
    relations: [...relations], references: [...references],
  };
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
) as readonly JoinSelfTestCaseId[];

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
  run(): SelfTestResult {
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
  })), ...JOIN_SELFTEST_CASES],
);

