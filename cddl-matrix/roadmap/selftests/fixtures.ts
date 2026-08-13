import { MATRIX_GENERATED_SLOT_BINDINGS } from "../adapters/matrix.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import { shieldTomlMarkdown } from "../decode/raw_markdown.ts";
import {
  classifyRoadmapIoError,
  RoadmapFailure,
  type RoadmapIssue,
} from "../errors.ts";
import { bytesEqual } from "../markdown_codec.ts";
import type { FixtureRelativePath, RepoPath } from "../model/core.ts";
import type { RoadmapDocumentV2 } from "../model/documents.ts";
import { REFERENCE_KIND_REGISTRY } from "../references.ts";
import {
  enumerateFixtureFilesPolicy,
  readInventoriedFixturePolicy,
  type FixtureFsPolicySurface,
  type FixtureNodeKind,
} from "../io.ts";
import { SELFTEST_CATEGORIES } from "../selftest.ts";
import type { SelfTestCandidateCase as SelfTestCase, SelfTestContext, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import {
  observeSelfTestIssue,
  validateNegativeSelfTestEvidence,
} from "./observations.ts";

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
  family_classification: ["none_reviewed"],
  family_maturity: ["observed_only", "under_design"],
  freshness: ["as_of", "historical", "live", "stale"],
  frozen_source_eof: ["lf"],
  incident_posture: ["attributed", "historical", "live"],
  manifest_kind: ["fragment", "generated_slot", "legacy_marker", "part", "record", "section"],
  migration_status: ["generated", "replaced"],
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
  render_authority: ["semantic"],
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
  schema_version: ["2"],
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
  work_state: ["armed", "blocked", "deferred", "delegated", "ready", "waiting_external"],
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

const FIXTURE_ROOT = "cddl-matrix/roadmap/fixtures" as RepoPath;
const UTF8 = new TextDecoder("utf-8", { fatal: true });

interface FixtureRegistryValidation {
  readonly rows: readonly import("../selftest.ts").FixtureCaseRow[];
  readonly issues: readonly string[];
  readonly declared_paths: readonly string[];
}

function exactKeys(value: Record<string, unknown>, expected: readonly string[]): boolean {
  return JSON.stringify(Object.keys(value)) === JSON.stringify(expected);
}

function decodeFixtureRegistry(bytes: Uint8Array, inventory: readonly string[]): FixtureRegistryValidation {
  const issues: string[] = [];
  let parsed: unknown;
  try { parsed = Bun.TOML.parse(UTF8.decode(bytes)); }
  catch (error) { return { rows: [], issues: [`cases.toml parse failed: ${String(error)}`], declared_paths: [] }; }
  const root = parsed as { case?: unknown };
  if (parsed === null || typeof parsed !== "object" || Array.isArray(parsed) || !exactKeys(parsed as Record<string, unknown>, ["case"]) || !Array.isArray(root.case)) {
    return { rows: [], issues: ["cases.toml must contain exactly one case array"], declared_paths: [] };
  }
  const rows: import("../selftest.ts").FixtureCaseRow[] = [];
  const caseIds = new Set<string>();
  const declared = new Set<string>();
  const bindPath = (path: string, coordinate: string): void => {
    if (declared.has(path)) issues.push(`${coordinate} duplicates fixture binding ${path}`);
    declared.add(path);
    if (!path.endsWith(".md") && !path.endsWith(".toml") && !path.endsWith(".expected") && !path.endsWith(".bytes")) {
      issues.push(`${coordinate} has an unexpected fixture extension`);
    }
  };
  for (const [index, raw] of root.case.entries()) {
    if (raw === null || typeof raw !== "object" || Array.isArray(raw)) {
      issues.push(`case[${index}] is not a table`);
      continue;
    }
    const row = raw as Record<string, unknown>;
    const id = row.id;
    if (typeof id !== "string" || caseIds.has(id)) issues.push(`case[${index}] has a missing or duplicate ID`);
    else caseIds.add(id);
    if (row.kind === "single_file") {
      const hasExpected = row.expected !== undefined;
      const hasSchema = row.schema_version !== undefined;
      const hasEof = row.projection_eof !== undefined;
      const expectedKeys = ["kind", "id", "class", "input", ...(hasExpected ? ["expected"] : []), "adapter", ...(hasSchema ? ["schema_version"] : []), ...(hasEof ? ["projection_eof"] : [])];
      if (!exactKeys(row, expectedKeys)) issues.push(`${String(id)} has unknown or noncanonical keys`);
      const validClass = row.class === "codec" || row.class === "positive" || row.class === "all_fields";
      const validAdapter = row.adapter === "codec" || row.adapter === "matrix" || row.adapter === "testing";
      if (!validClass || !validAdapter || typeof row.input !== "string") issues.push(`${String(id)} has an invalid single-file binding`);
      else {
        bindPath(row.input, `${String(id)}.input`);
        if (typeof row.expected === "string") bindPath(row.expected, `${String(id)}.expected`);
        const codec = row.class === "codec";
        if (codec) {
          if (row.adapter !== "codec" || typeof row.expected !== "string" || row.schema_version !== undefined || (row.projection_eof !== "lf" && row.projection_eof !== "none")) issues.push(`${String(id)} violates codec binding rules`);
        } else if (
          (row.adapter !== "matrix" && row.adapter !== "testing") || typeof row.expected !== "string" ||
          (row.schema_version !== 0 && row.schema_version !== 1 && row.schema_version !== 2) || (row.projection_eof !== "lf" && row.projection_eof !== "none")
        ) issues.push(`${String(id)} violates roadmap fixture binding rules`);
        rows.push(row as unknown as import("../selftest.ts").SingleFileFixtureCaseRow);
      }
    } else if (row.kind === "status_compat_bundle") {
      if (!exactKeys(row, ["kind", "id", "class", "files", "inputs", "modes", "diagnostics", "golden_targets"])) issues.push(`${String(id)} has unknown or noncanonical status keys`);
      if (id !== "fixture_status_compat" || row.class !== "status-compat" || !Array.isArray(row.files) || row.files.some((path) => typeof path !== "string")) {
        issues.push("status bundle binding is invalid");
      } else {
        const files = row.files as string[];
        if (JSON.stringify(files) !== JSON.stringify([...files].sort(codePointSort)) || new Set(files).size !== files.length) issues.push("status files must be unique and sorted");
        for (const path of files) bindPath(path, `${String(id)}.files`);
        if (row.inputs !== "status-compat/inputs.toml" || row.modes !== "status-compat/modes.toml" || row.diagnostics !== "status-compat/diagnostics.toml") issues.push("status typed input bindings differ from the frozen paths");
        const mapping = row.golden_targets;
        if (mapping === null || typeof mapping !== "object" || Array.isArray(mapping)) issues.push("status golden mapping is invalid");
        else {
          const entries = Object.entries(mapping as Record<string, unknown>);
          if (JSON.stringify(entries.map(([key]) => key)) !== JSON.stringify(entries.map(([key]) => key).sort(codePointSort))) issues.push("status golden mapping must be sorted");
          if (entries.some(([key, value]) => typeof value !== "string" || !files.includes(key) || !files.includes(value))) issues.push("status golden mapping escapes the bundle");
        }
        rows.push(row as unknown as import("../selftest.ts").StatusCompatibilityFixtureCaseRow);
      }
    } else issues.push(`${String(id)} has an unknown fixture row kind`);
  }
  const expectedIds = [
    "fixture_codec_leading_eof", "fixture_codec_no_eof", "fixture_codec_controls",
    "fixture_small_matrix_v2", "fixture_small_testing_v2",
    "fixture_all_fields_matrix_v2", "fixture_all_fields_testing_v2", "fixture_status_compat",
  ];
  if (JSON.stringify(rows.map((row) => row.id)) !== JSON.stringify(expectedIds)) issues.push("fixture case IDs/order differ from the frozen row registry");
  const singleSignatures = rows.flatMap((row) => row.kind === "single_file" ? [[
    row.id, row.class, row.input, row.expected ?? "-", row.adapter,
    row.schema_version === undefined ? "-" : String(row.schema_version), row.projection_eof ?? "-",
  ].join("|")] : []);
  const expectedSingleSignatures = [
    "fixture_codec_leading_eof|codec|codec/hazards-leading-and-eof.md|codec/hazards-leading-and-eof.toml.expected|codec|-|lf",
    "fixture_codec_no_eof|codec|codec/hazards-no-eof.md|codec/hazards-no-eof.toml.expected|codec|-|none",
    "fixture_codec_controls|codec|codec/control-scalars.bytes|codec/control-scalars.toml.expected|codec|-|none",
    "fixture_small_matrix_v2|positive|positive/small-matrix-v2.toml|positive/small-matrix-v2.expected.md|matrix|2|lf",
    "fixture_small_testing_v2|positive|positive/small-testing-v2.toml|positive/small-testing-v2.expected.md|testing|2|none",
    "fixture_all_fields_matrix_v2|all_fields|all-fields/matrix-v2.toml|all-fields/matrix-v2.expected.md|matrix|2|lf",
    "fixture_all_fields_testing_v2|all_fields|all-fields/testing-v2.toml|all-fields/testing-v2.expected.md|testing|2|lf",
  ];
  if (JSON.stringify(singleSignatures) !== JSON.stringify(expectedSingleSignatures)) issues.push("single-file fixture bindings differ from the frozen row table");
  const disk = inventory.filter((path) => path !== "cases.toml").sort(codePointSort);
  const declaredPaths = [...declared].sort(codePointSort);
  for (const path of declaredPaths) if (!disk.includes(path)) issues.push(`declared fixture is missing: ${path}`);
  for (const path of disk) if (!declared.has(path)) issues.push(`fixture is unlisted: ${path}`);
  return { rows: Object.freeze(rows), issues: Object.freeze(issues.sort(codePointSort)), declared_paths: Object.freeze(declaredPaths) };
}

function fail(id: string, error: unknown, polarity: "positive" | "negative", subcases?: readonly string[]): SelfTestResult {
  const issue: RoadmapIssue = error instanceof RoadmapFailure && error.issues.length === 1
    ? error.issues[0]!
    : { code: "E-SELFTEST-CASE", source: "<selftest>", logical_path: id, message: error instanceof Error ? error.message : String(error), exit: 1 };
  return { ok: false, polarity, issues: [issue], subcases };
}

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

function fakeFixtureSurface(
  nodes: Readonly<Record<string, FixtureNodeKind>>,
  listings: Readonly<Record<string, readonly { readonly name: string; readonly kind: FixtureNodeKind }[]>> = {},
  confined = true,
): FixtureFsPolicySurface {
  return {
    nodeKind(relativePath: string): FixtureNodeKind {
      const kind = nodes[relativePath];
      if (kind === undefined) throw new Error(`fake fixture node is absent: ${relativePath}`);
      return kind;
    },
    listDirectory: (relativePath: string) => listings[relativePath] ?? [],
    resolvesWithinRoot: () => confined,
    readFile: () => new Uint8Array([0x78]),
  };
}

interface ExpectedIssue {
  readonly code: RoadmapIssue["code"];
  readonly logical_path: string;
  readonly source?: string;
  readonly exit?: 1 | 2;
}

function expectIssue(run: () => unknown, expected: ExpectedIssue): RoadmapIssue {
  try { run(); }
  catch (error) {
    assert(error instanceof RoadmapFailure, `expected RoadmapFailure, got ${String(error)}`);
    assert(error.issues.length === 1, `expected one issue, got ${error.issues.length}`);
    const actual = error.issues[0]!;
    assert(actual.code === expected.code, `expected ${expected.code}, got ${actual.code}`);
    assert(actual.logical_path === expected.logical_path, `expected path ${expected.logical_path}, got ${actual.logical_path}`);
    if (expected.source !== undefined) assert(actual.source === expected.source, `expected source ${expected.source}, got ${actual.source}`);
    if (expected.exit !== undefined) assert(actual.exit === expected.exit, `expected exit ${expected.exit}, got ${actual.exit}`);
    observeSelfTestIssue(actual);
    return actual;
  }
  throw new Error(`expected ${expected.code} at ${expected.logical_path}`);
}

function verifyFixtureExpectedBytes(
  actual: Uint8Array,
  expected: Uint8Array,
  source: string,
  logicalPath: string,
): void {
  if (bytesEqual(actual, expected)) return;
  throw new RoadmapFailure({
    code: "E-FIXTURE-EXPECTED",
    source,
    logical_path: logicalPath,
    message: "executed decoded value differs from the declared golden",
    exit: 1,
  });
}

function registry(context: SelfTestContext): FixtureRegistryValidation {
  const inventory = context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
  const cases = inventory.find((path) => path === "cases.toml") as FixtureRelativePath | undefined;
  assert(cases !== undefined, "fixture inventory omitted cases.toml");
  return decodeFixtureRegistry(context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, cases), inventory);
}

const FIXTURE_CASE_POLARITY = new Map<RequiredFixtureSelfTestCaseId, "positive" | "negative">([
  ["fixture_registry_no_orphans", "positive"], ["fixture_registry_missing_file", "negative"],
  ["fixture_registry_unlisted_file", "negative"], ["fixture_registry_duplicate_id", "negative"],
  ["fixture_family_floors", "positive"], ["slot_resolver_four_matrix_slots", "positive"],
  ["reference_provider_members_nonempty_or_declared_empty", "positive"], ["selftest_case_ids_unique", "positive"],
  ["selftest_case_count_floor", "positive"], ["selftest_empty_category_mutation", "negative"],
  ["selftest_all_categories_receipted", "positive"], ["io_effect_import_funnel", "positive"],
  ["fixture_enumeration_recursive_sorted", "positive"], ["fixture_enumeration_rejects_escape", "negative"],
  ["scratch_git_lifecycle", "positive"], ["scratch_git_argv_no_shell", "positive"],
  ["fixture_read_exact_bytes", "positive"], ["fixture_read_rejects_escape", "negative"],
  ["fixture_read_missing_rejected", "negative"], ["fixture_read_permission_exit_two", "negative"],
  ["fixture_read_other_io_exit_two", "negative"], ["fixture_expected_content_mismatch", "negative"],
  ["fixture_status_bundle_nine_paths", "positive"], ["fixture_status_bundle_golden_mapping", "positive"],
]);

function executeFixtureCase(id: RequiredFixtureSelfTestCaseId, context: SelfTestContext): readonly string[] | undefined {
  if (id === "fixture_registry_no_orphans") {
    const decoded = registry(context);
    const inventory = context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
    assert(decoded.issues.length === 0, decoded.issues.join("; "));
    assert(decoded.rows.length === 8, `fixture registry has ${decoded.rows.length} rows instead of 8`);
    assert(decoded.declared_paths.length === 23, `fixture registry binds ${decoded.declared_paths.length} files instead of 23`);
    assert(inventory.length === 24, `fixture inventory has ${inventory.length} files including cases.toml instead of 24`);
  } else if (id === "fixture_registry_missing_file") {
    const inventory = context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
    const decoded = registry(context);
    const missing = inventory.filter((path) => path !== decoded.declared_paths[0]);
    assert(decodeFixtureRegistry(context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, "cases.toml" as FixtureRelativePath), missing).issues.some((value) => value.includes("missing")), "missing fixture mutation passed");
    observeSelfTestIssue({ code: "E-FIXTURE-REGISTRY", logical_path: "fixture-registry.missing" });
  } else if (id === "fixture_registry_unlisted_file") {
    const inventory = [...context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT), "positive/unlisted.toml"].sort(codePointSort);
    assert(decodeFixtureRegistry(context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, "cases.toml" as FixtureRelativePath), inventory).issues.some((value) => value.includes("unlisted")), "unlisted fixture mutation passed");
    observeSelfTestIssue({ code: "E-FIXTURE-REGISTRY", logical_path: "fixture-registry.unlisted" });
  } else if (id === "fixture_registry_duplicate_id") {
    const inventory = context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
    const source = UTF8.decode(context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, "cases.toml" as FixtureRelativePath));
    const second = source.indexOf("[[case]]", "[[case]]".length);
    assert(second > 0, "fixture registry has no row to duplicate");
    const mutated = new TextEncoder().encode(`${source}${source.slice(0, second)}`);
    assert(decodeFixtureRegistry(mutated, inventory).issues.some((value) => value.includes("duplicate ID")), "duplicate-ID mutation passed registry validation");
    observeSelfTestIssue({ code: "E-FIXTURE-REGISTRY", logical_path: "fixture-registry.duplicate-id" });
  } else if (id === "fixture_family_floors") {
    const rows = registry(context).rows;
    const count = (kind: string) => rows.filter((row) => row.class === kind).length;
    assert(count("codec") >= 2 && count("positive") >= 2 && count("all_fields") >= 2 && count("status-compat") >= 1, "fixture family floor failed");
  } else if (id === "slot_resolver_four_matrix_slots") {
    assert(JSON.stringify(MATRIX_GENERATED_SLOT_BINDINGS) === JSON.stringify([
      ["constraint", "status_header_markers:roadmap-constraint"],
      ["counts", "status_header_markers:roadmap-counts"],
      ["emission", "status_header_markers:roadmap-emission"],
      ["ops", "status_header_markers:roadmap-ops"],
    ]), "matrix adapter must expose the exact four roadmap slot bindings");
  } else if (id === "reference_provider_members_nonempty_or_declared_empty") {
    assert(REFERENCE_KIND_REGISTRY.length > 0 && new Set(REFERENCE_KIND_REGISTRY).size === REFERENCE_KIND_REGISTRY.length, "reference kind provider registry is empty or duplicated");
    const inventory = context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
    const decoded = (["matrix", "testing"] as const).map((roadmap) => {
      const path = `all-fields/${roadmap}-v2.toml` as FixtureRelativePath;
      assert(inventory.includes(path), `all-fields provider fixture is missing ${path}`);
      return decodeRoadmapSource(context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, path), path, roadmap, true);
    });
    const represented = new Set<string>(decoded.flatMap((document) =>
      document.references.map((reference) => reference.kind)
    ));
    const missing = REFERENCE_KIND_REGISTRY.filter((kind) => !represented.has(kind));
    assert(missing.length === 0, `reference-provider fixture members are empty for ${missing.join(", ")}`);
  } else if (id === "selftest_case_ids_unique") {
    const ids = context.registry.cases.map((testCase) => testCase.id);
    assert(new Set(ids).size === ids.length, "self-test case IDs are not globally unique");
    const authority = { code: "E-SCHEMA-STATE" as const, logical_path: "record[0]" };
    const observations = [authority];
    assert(
      validateNegativeSelfTestEvidence({}, authority, observations) !== undefined,
      "negative evidence accepted a missing declaration",
    );
    assert(
      validateNegativeSelfTestEvidence(
        { expected: { ...authority, code: "E-SCHEMA-TYPE" } },
        authority,
        observations,
      ) !== undefined,
      "negative evidence accepted a wrong declared code",
    );
    assert(
      validateNegativeSelfTestEvidence(
        { expected: { ...authority, logical_path: "record[1]" } },
        authority,
        observations,
      ) !== undefined,
      "negative evidence accepted a wrong declared path",
    );
    assert(
      validateNegativeSelfTestEvidence({ expected: authority }, authority, []) !== undefined,
      "negative evidence accepted a declaration without an actual observation",
    );
    assert(
      validateNegativeSelfTestEvidence({ expected: authority }, authority, observations) === undefined,
      "negative evidence rejected an exact independent declaration and observation",
    );
  } else if (id === "selftest_case_count_floor") {
    const mechanicalFloor = [...context.registry.category_floors.values()].reduce((sum, floor) => sum + floor.total, 0);
    assert(context.registry.cases.length >= mechanicalFloor, "registered case count is below the category floors");
  } else if (id === "selftest_empty_category_mutation") {
    const category = SELFTEST_CATEGORIES[0];
    const mutated = context.registry.cases.filter((testCase) => testCase.category !== category);
    const categoryIssue = context.registry.validate(mutated).find((value) => value.code === "E-SELFTEST-FLOOR" && value.logical_path === category);
    assert(categoryIssue !== undefined, "empty category mutation passed");
    observeSelfTestIssue(categoryIssue);
    assert(context.registry.validate([]).some((value) => value.code === "E-SELFTEST-FLOOR"), "empty registry mutation passed");
  } else if (id === "selftest_all_categories_receipted") {
    const actual = [...context.registry.category_floors.keys()].sort(codePointSort);
    assert(JSON.stringify(actual) === JSON.stringify([...SELFTEST_CATEGORIES].sort(codePointSort)), "self-test category registry differs from the frozen category set");
  } else if (id === "io_effect_import_funnel") {
    assert(JSON.stringify(Object.keys(context.ports).sort()) === JSON.stringify(["fixtures", "scratch_git"]), "selftest capability escaped the I/O funnel");
    assert(JSON.stringify(Object.keys(context.ports.fixtures).sort()) === JSON.stringify([
      "createScratchRepository", "enumerateFixtureFiles", "openScratchRoadmapPorts",
      "readFixtureFile", "removeScratchFile", "removeScratchRepository",
      "replaceScratchFile", "scratchRepositoryPresent",
    ]), "fixture harness exposes an undeclared effect capability");
    assert(JSON.stringify(Object.keys(context.ports.scratch_git).sort()) === JSON.stringify(["runScratchGit"]), "scratch Git harness exposes an undeclared effect capability");
  } else if (id === "fixture_enumeration_recursive_sorted") {
    const inventory = context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
    assert(inventory.some((path) => path.split("/").length >= 2), "fixture enumeration is not recursive");
    assert(JSON.stringify(inventory) === JSON.stringify([...inventory].sort(codePointSort)) && new Set(inventory).size === inventory.length, "fixture enumeration is not sorted and unique");
  } else if (id === "fixture_enumeration_rejects_escape") {
    for (const root of ["/absolute", "../fixtures"] as const) {
      expectIssue(
        () => context.ports.fixtures.enumerateFixtureFiles(root as RepoPath),
        { code: "E-FIXTURE-REGISTRY", source: root, logical_path: "fixture-root", exit: 1 },
      );
    }
    for (const [name, kind] of [["linked", "symlink"], ["device", "other"]] as const) {
      expectIssue(
        () => enumerateFixtureFilesPolicy(fakeFixtureSurface(
          { "": "directory" },
          { "": [{ name, kind }] },
        )),
        { code: "E-FIXTURE-REGISTRY", source: name, logical_path: "fixture-enumeration", exit: 1 },
      );
    }
    expectIssue(
      () => enumerateFixtureFilesPolicy(fakeFixtureSurface({ "": "directory" }, {}, false)),
      { code: "E-FIXTURE-REGISTRY", source: "<fixture-root>", logical_path: "fixture-enumeration", exit: 1 },
    );
  } else if (id === "scratch_git_lifecycle") {
    const subcases = ["success_cleanup", "assertion_failure_cleanup", "foreign_handle_rejected", "double_cleanup_rejected", "seed_path_escape_rejected"];
    const success = context.ports.fixtures.createScratchRepository([]);
    assert(context.ports.fixtures.scratchRepositoryPresent(success), "scratch create did not mint a live root");
    context.ports.fixtures.removeScratchRepository(success);
    assert(!context.ports.fixtures.scratchRepositoryPresent(success), "scratch success cleanup failed");
    const assertion = context.ports.fixtures.createScratchRepository([]);
    try { throw new Error("intentional assertion probe"); }
    catch { /* the finally below owns cleanup */ }
    finally { context.ports.fixtures.removeScratchRepository(assertion); }
    assert(!context.ports.fixtures.scratchRepositoryPresent(assertion), "scratch assertion cleanup failed");
    expectIssue(
      () => context.ports.fixtures.removeScratchRepository({} as import("../io.ts").ScratchRepositoryHandle),
      { code: "E-FIXTURE-REGISTRY", source: "<scratch>", logical_path: "scratch-handle", exit: 1 },
    );
    expectIssue(
      () => context.ports.fixtures.removeScratchRepository(success),
      { code: "E-FIXTURE-REGISTRY", source: "<scratch>", logical_path: "scratch-handle", exit: 1 },
    );
    expectIssue(
      () => context.ports.fixtures.createScratchRepository([{ path: "../escape" as RepoPath, bytes: new Uint8Array() }]),
      { code: "E-FIXTURE-REGISTRY", source: "../escape", logical_path: "repository-path", exit: 1 },
    );

    const encoder = new TextEncoder();
    const sourcePath = "cddl-matrix/roadmap.toml" as RepoPath;
    const integration = context.ports.fixtures.createScratchRepository([
      { path: sourcePath, bytes: encoder.encode("base\n") },
      { path: "blocked/keep.txt" as RepoPath, bytes: encoder.encode("keep\n") },
      { path: "docs/guide.mdx" as RepoPath, bytes: encoder.encode("# MDX durable heading\n") },
      { path: "link-dir-target.txt" as RepoPath, bytes: encoder.encode("blocked") },
      { path: "link-file-target.txt" as RepoPath, bytes: encoder.encode(sourcePath) },
      { path: "notes.txt" as RepoPath, bytes: encoder.encode("## Durable text heading\n") },
      { path: "src/main.rs" as RepoPath, bytes: encoder.encode("#[cfg(test)]\nmod tests;\n") },
      { path: "src/tests/mod.rs" as RepoPath, bytes: new Uint8Array() },
    ]);
    try {
      const git = context.ports.scratch_git;
      assert(git.runScratchGit(integration, ["init", "--quiet", "--object-format=sha1"]).exit_code === 0, "scratch integration init failed");
      assert(git.runScratchGit(integration, ["add", "--all"]).exit_code === 0, "scratch integration add failed");
      assert(git.runScratchGit(integration, ["commit", "--quiet", "--no-gpg-sign", "-m", "base"]).exit_code === 0, "scratch unsigned integration commit failed");
      const headResult = git.runScratchGit(integration, ["rev-parse", "HEAD"]);
      const head = UTF8.decode(headResult.stdout).trim();
      assert(headResult.exit_code === 0 && /^[0-9a-f]{40}$/u.test(head), "scratch integration did not produce a full SHA-1 commit ID");

      const ports = context.ports.fixtures.openScratchRoadmapPorts(integration);
      assert(ports.repositoryObjectFormat() === "sha1", "production scratch ports reported the wrong object format");
      const commit = ports.resolveFullCommit(head);
      assert(commit === head, "production scratch ports changed the exact commit ID");
      assert(UTF8.decode(ports.readDeclared(sourcePath)) === "base\n", "worktree declared read changed committed bytes");
      assert(UTF8.decode(ports.readDeclaredAtCommit(commit, sourcePath)) === "base\n", "commit declared read changed committed bytes");
      assert(ports.registryView({ kind: "worktree" }).revision.kind === "worktree", "worktree registry revision was not preserved");
      const commitView = ports.registryView({ kind: "commit", commit });
      assert(commitView.revision.kind === "commit" && commitView.revision.commit === commit, "commit registry revision was not preserved");
      const expectedDurableHeadings = [{
        path: "docs/guide.mdx",
        heading: "MDX durable heading",
        span: { start_byte: 2, end_byte: 21 },
        section_text: "# MDX durable heading\n",
      }, {
        path: "notes.txt",
        heading: "Durable text heading",
        span: { start_byte: 3, end_byte: 23 },
        section_text: "## Durable text heading\n",
      }];
      for (const view of [ports.registryView({ kind: "worktree" }), commitView]) {
        const durableHeadings = view.tracked_headings.filter((fact) =>
          fact.path === "docs/guide.mdx" || fact.path === "notes.txt"
        );
        assert(
          JSON.stringify(durableHeadings) === JSON.stringify(expectedDurableHeadings),
          `${view.revision.kind} durable tracked heading inventory omitted MDX or extension-neutral text headings`,
        );
      }

      const fileLinkOid = UTF8.decode(git.runScratchGit(integration, ["hash-object", "-w", "link-file-target.txt"]).stdout).trim();
      const directoryLinkOid = UTF8.decode(git.runScratchGit(integration, ["hash-object", "-w", "link-dir-target.txt"]).stdout).trim();
      for (const [oid, path] of [[fileLinkOid, "linked.md"], [directoryLinkOid, "linked-dir"]] as const) {
        assert(/^[0-9a-f]{40}$/u.test(oid), `scratch symlink blob for ${path} was not written`);
        assert(
          git.runScratchGit(integration, ["update-index", "--add", "--cacheinfo", `120000,${oid},${path}`]).exit_code === 0 &&
          git.runScratchGit(integration, ["checkout-index", "--force", "--", path]).exit_code === 0,
          `scratch Git did not materialize tracked symlink ${path}`,
        );
      }
      expectIssue(
        () => ports.readDeclared("linked.md" as RepoPath),
        { code: "E-REFERENCE-UNRESOLVED", source: "linked.md", logical_path: "read-declared", exit: 1 },
      );
      expectIssue(
        () => ports.atomicReplace("linked-dir/keep.txt" as RepoPath, encoder.encode("never\n")),
        { code: "E-IO-WRITE", source: "linked-dir/keep.txt", logical_path: "atomic-path", exit: 2 },
      );
      assert(UTF8.decode(ports.readDeclared("blocked/keep.txt" as RepoPath)) === "keep\n", "static symlink rejection touched its external target");

      ports.atomicReplace(sourcePath, encoder.encode("candidate\n"));
      assert(UTF8.decode(ports.readDeclared(sourcePath)) === "candidate\n", "atomic success did not publish exact worktree bytes");
      assert(UTF8.decode(ports.readDeclaredAtCommit(commit, sourcePath)) === "base\n", "atomic success mutated the committed revision");
      expectIssue(
        () => ports.atomicReplace("blocked" as RepoPath, encoder.encode("never\n")),
        { code: "E-IO-RENAME", source: "blocked", logical_path: "atomic-rename", exit: 2 },
      );
      assert(UTF8.decode(ports.readDeclared(sourcePath)) === "candidate\n", "atomic failure disturbed an already-published target");
      assert(UTF8.decode(ports.readDeclared("blocked/keep.txt" as RepoPath)) === "keep\n", "atomic rename failure disturbed the attempted target tree");
    } finally {
      context.ports.fixtures.removeScratchRepository(integration);
    }
    assert(!context.ports.fixtures.scratchRepositoryPresent(integration), "production-port integration scratch cleanup failed");
    return subcases;
  } else if (id === "scratch_git_argv_no_shell") {
    const repository = context.ports.fixtures.createScratchRepository([{ path: "literal;name.txt" as RepoPath, bytes: new TextEncoder().encode("x\n") }]);
    try {
      assert(context.ports.scratch_git.runScratchGit(repository, ["init", "--quiet"]).exit_code === 0, "scratch git init failed");
      assert(context.ports.scratch_git.runScratchGit(repository, ["add", "--", "literal;name.txt"]).exit_code === 0, "literal semicolon argv was not passed intact");
      const status = context.ports.scratch_git.runScratchGit(repository, ["status", "--short"]);
      assert(status.exit_code === 0 && UTF8.decode(status.stdout).includes("literal;name.txt"), "scratch Git did not preserve literal argv path");
    } finally { context.ports.fixtures.removeScratchRepository(repository); }
  } else if (id === "fixture_read_exact_bytes") {
    const inventory = context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
    const path = inventory.find((value) => value === "codec/hazards-no-eof.md")!;
    const first = context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, path);
    const snapshot = new Uint8Array(first);
    first[0] ^= 0xff;
    const second = context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, path);
    assert(second.every((value, index) => value === snapshot[index]), "fixture read did not return a fresh exact snapshot");
  } else if (id === "fixture_read_rejects_escape") {
    const subcases = ["absolute", "dotdot", "foreign_root", "symlink", "non_regular"];
    for (const path of ["/absolute", "../dotdot"] as const) {
      expectIssue(
        () => context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, path as FixtureRelativePath),
        { code: "E-FIXTURE-REGISTRY", source: path, logical_path: "fixture-path", exit: 1 },
      );
    }
    expectIssue(
      () => context.ports.fixtures.readFixtureFile("tests" as RepoPath, "cases.toml" as FixtureRelativePath),
      { code: "E-FIXTURE-REGISTRY", source: "tests", logical_path: "fixture-root", exit: 1 },
    );
    const inventory = new Set(["nested/symlink", "non_regular", "outside"]);
    for (const [path, surface] of [[
      "nested/symlink",
      fakeFixtureSurface({ nested: "directory", "nested/symlink": "symlink" }),
    ], [
      "non_regular",
      fakeFixtureSurface({ non_regular: "directory" }),
    ]] as const) {
      expectIssue(
        () => readInventoriedFixturePolicy(path as FixtureRelativePath, inventory, surface),
        { code: "E-FIXTURE-REGISTRY", source: path, logical_path: "fixture-read", exit: 1 },
      );
    }
    expectIssue(
      () => readInventoriedFixturePolicy(
        "outside" as FixtureRelativePath,
        inventory,
        fakeFixtureSurface({ outside: "file" }, {}, false),
      ),
      { code: "E-FIXTURE-REGISTRY", source: "outside", logical_path: "fixture-read", exit: 1 },
    );
    return subcases;
  } else if (id === "fixture_read_missing_rejected") {
    expectIssue(
      () => context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, "missing" as FixtureRelativePath),
      { code: "E-FIXTURE-REGISTRY", source: "missing", logical_path: "fixture-read", exit: 1 },
    );
  } else if (id === "fixture_read_permission_exit_two") {
    for (const mutation of ["eacces", "eperm"] as const) {
      const errno = mutation === "eacces" ? "EACCES" : "EPERM";
      const failure = classifyRoadmapIoError(
        Object.assign(new Error(`${mutation} fixture read`), { code: errno }),
        { role: "fixture", path: "probe", operation: "fixture-read" },
      );
      const result = failure.issues[0]!;
      assert(
        result.code === "E-IO-PERMISSION" && result.logical_path === "fixture-read" &&
        result.exit === 2 && result.message.includes(`(${errno})`),
        `${mutation} did not map ${errno} to permission exit two`,
      );
      observeSelfTestIssue(result);
    }
    return ["eacces", "eperm"];
  } else if (id === "fixture_read_other_io_exit_two") {
    const failure = classifyRoadmapIoError(
      Object.assign(new Error("fixture read path is too long"), { code: "ENAMETOOLONG" }),
      { role: "fixture", path: "probe", operation: "fixture-read" },
    );
    const result = failure.issues[0]!;
    assert(
      result.code === "E-IO-READ" && result.logical_path === "fixture-read" &&
      result.exit === 2 && result.message.includes("(ENAMETOOLONG)"),
      "fixture ENAMETOOLONG did not map to read exit two",
    );
    observeSelfTestIssue(result);
  } else if (id === "fixture_expected_content_mismatch") {
    const inventory = context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
    const inputPath = inventory.find((value) => value === "codec/hazards-no-eof.md")!;
    const expectedPath = inventory.find((value) => value === "codec/hazards-no-eof.toml.expected")!;
    const input = context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, inputPath);
    const expected = context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, expectedPath);
    const bindings = shieldTomlMarkdown(expected, expectedPath);
    assert(bindings.parsed !== null && typeof bindings.parsed === "object" && !Array.isArray(bindings.parsed), "codec golden did not decode to a table");
    const value = Object.getOwnPropertyDescriptor(bindings.parsed, "value")?.value;
    const decoded = bindings.expectMarkdown(value, "value");
    bindings.assertAllConsumed();
    verifyFixtureExpectedBytes(decoded, input, expectedPath, "golden");
    const actual = new Uint8Array(decoded);
    actual[0] ^= 1;
    assert(!bytesEqual(actual, decoded), "expected-content mutation was vacuous");
    expectIssue(
      () => verifyFixtureExpectedBytes(actual, input, expectedPath, "golden"),
      { code: "E-FIXTURE-EXPECTED", source: expectedPath, logical_path: "golden", exit: 1 },
    );
  } else if (id === "fixture_status_bundle_nine_paths") {
    const row = registry(context).rows.find((value) => value.kind === "status_compat_bundle");
    const expected = [
      "status-compat/diagnostics.toml", "status-compat/inputs.toml", "status-compat/matrix-readme.after.md",
      "status-compat/matrix-readme.before.md", "status-compat/modes.toml", "status-compat/roadmap.after.md",
      "status-compat/roadmap.before.md", "status-compat/tests-readme.after.md", "status-compat/tests-readme.before.md",
    ];
    assert(row?.kind === "status_compat_bundle" && JSON.stringify(row.files) === JSON.stringify(expected), "status fixture bundle is not the frozen nine-path tuple");
  } else if (id === "fixture_status_bundle_golden_mapping") {
    const row = registry(context).rows.find((value) => value.kind === "status_compat_bundle");
    const expected = {
      "status-compat/matrix-readme.before.md": "status-compat/matrix-readme.after.md",
      "status-compat/roadmap.before.md": "status-compat/roadmap.after.md",
      "status-compat/tests-readme.before.md": "status-compat/tests-readme.after.md",
    };
    assert(row?.kind === "status_compat_bundle" && JSON.stringify(row.golden_targets) === JSON.stringify(expected), "status fixture golden map differs from the frozen three pairs");
    for (const [before, after] of Object.entries(row.golden_targets)) assert(row.files.includes(before as never) && row.files.includes(after as never), "status golden pair escapes files tuple");
  }
  return undefined;
}

export const FIXTURE_SELFTEST_CASES: readonly SelfTestCase[] = Object.freeze(
  REQUIRED_FIXTURE_SELFTEST_CASE_IDS.map((id) => ({
    id,
    category: id.startsWith("scratch_") || id.startsWith("fixture_read_") || id.startsWith("fixture_enumeration_") || id === "io_effect_import_funnel"
      ? "io-harness" as const
      : id.startsWith("fixture_status_bundle_")
      ? "status-compat" as const
      : "fixture-registry" as const,
    run(context: SelfTestContext): SelfTestResult {
      const polarity = FIXTURE_CASE_POLARITY.get(id)!;
      try { return { ok: true, polarity, subcases: executeFixtureCase(id, context) }; }
      catch (error) { return fail(id, error, polarity); }
    },
  })),
);
