import type {
  SelfTestCase,
  SelfTestContext,
  SelfTestResult,
} from "../selftest.ts";
import type {
  FixtureRelativePath,
  RepoPath,
  RoadmapId,
  RoadmapName,
} from "../model/core.ts";
import type { RoadmapDocument } from "../model/documents.ts";
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
} from "../indexes.ts";

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
  // Reference, relation, and global-identity modules append their case IDs here.
] as const;

export type RequiredIdentitySelfTestCaseId =
  (typeof REQUIRED_IDENTITY_SELFTEST_CASE_IDS)[number];

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

function bytes(value: string): Uint8Array {
  return new TextEncoder().encode(value);
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
  for (const path of IDENTITY_ROADMAP_FIXTURE_PATHS) {
    const expected = EXPECTED_FIXTURE_INDEXES[path];
    const document = decodeRoadmapSource(fixtureBytes(bundle, path), path, expected.roadmap);
    const result = buildRoadmapIndexes(document);
    assert(
      result.issues.length === 0,
      `${path} must index without issues, got ${JSON.stringify(result.issues)}`,
    );
    assertFixtureIndexes(path, result.indexes, expected);
  }
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

const CASE_POLARITY = new Map<PermanentIdSelfTestCaseId, "positive" | "negative">([
  ["id_grammar_accept", "positive"],
  ["id_reserved_tokens", "negative"],
  ["id_numeric_legacy_tokens", "negative"],
  ["id_namespace_mismatch", "negative"],
  ["id_is_opaque_to_consumers", "positive"],
]);

assert(CASE_POLARITY.size === PERMANENT_ID_SELFTEST_CASE_IDS.length, "permanent-ID polarity metadata must cover every case");

export const IDENTITY_SELFTEST_CASES: readonly SelfTestCase[] = Object.freeze(
  PERMANENT_ID_SELFTEST_CASE_IDS.map((id) => ({
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
  })),
);

export function runIdentitySelfTests(
  fixtureBundle: IdentityFixtureBundle,
): { readonly executed: number } {
  for (const id of PERMANENT_ID_SELFTEST_CASE_IDS) execute(id, fixtureBundle);
  return { executed: PERMANENT_ID_SELFTEST_CASE_IDS.length };
}
