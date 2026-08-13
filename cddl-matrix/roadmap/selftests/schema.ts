import { RoadmapWireError, bytesEqual, encodeMarkdownString } from "../markdown_codec.ts";
import type { RoadmapIssue } from "../errors.ts";
import type { RepoPath, RoadmapName } from "../model/core.ts";
import type { RoadmapDocumentV3 } from "../model/documents.ts";
import type { SelfTestCandidateCase as SelfTestCase, SelfTestCategory, SelfTestContext, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import { compareAllFieldsCoverageTags } from "./fixtures.ts";
import { observeSelfTestIssue } from "./observations.ts";
import { composeRoadmapDocument } from "../compose.ts";
import { decodeMatrixPayload, MATRIX_ENUM_FIELDS, MATRIX_SCHEMA_ROWS } from "../decode/matrix.ts";
import {
  expectExactTable,
  requiredValue,
  schemaFail,
  type DecodeContext,
  type EnumSchemaField,
  type ExactSchemaRow,
  type SchemaDecodeTrace,
} from "../decode/primitives.ts";
import {
  decodeRoadmapSource,
  REFERENCE_SCHEMA_ROWS,
  ROADMAP_ENUM_FIELDS,
  ROADMAP_ROW,
  ROADMAP_SCHEMA_ROWS,
} from "../decode/roadmap.ts";
import {
  decodeSharedSemanticPayload,
  SEMANTIC_ENUM_FIELDS,
  SHARED_SEMANTIC_SCHEMA_ROWS,
} from "../decode/semantic.ts";
import { decodeTestingPayload, TESTING_ENUM_FIELDS, TESTING_SCHEMA_ROWS } from "../decode/testing.ts";
import { childLogicalPath, shieldTomlMarkdown } from "../decode/raw_markdown.ts";

const UTF8 = new TextEncoder();
const text = (value: string): Uint8Array => UTF8.encode(value);
const ZERO_HASH = "0".repeat(64);

export const REQUIRED_SCHEMA_SELFTEST_CASE_IDS = [
  "strict_unknown_top", "strict_unknown_nested_record", "strict_unknown_reference", "strict_unknown_every_table", "strict_unknown_kind", "strict_unknown_enum", "strict_enum_every_field", "strict_missing_discriminator", "strict_generic_state_rejected", "strict_generic_disposition_rejected", "missing_section_entries", "empty_records_floor", "all_fields_identity", "v3_semantic_identity", "v2_unsupported", "v3_retired_keys_rejected", "transition_observable_arm_dependent", "noncanonical_basic_string", "noncanonical_table_order", "noncanonical_set_order", "toml_terminal_newline", "domain_matrix_all_tags", "domain_testing_all_tags", "domain_state_required_forbidden", "domain_defect_regression_required", "domain_missing_system_admission_required", "domain_transition_each_kind", "domain_quantitative_scope_unit_required", "domain_manual_not_auto_boolean", "domain_fired_transition_not_parked", "domain_already_met_transition_rejected", "domain_stale_unknown_visible", "evidence_point_requires_provenance", "evidence_negative_requires_enumeration", "evidence_generator_requires_harness_free", "evidence_timing_join_structural", "evidence_draft_log_rejected", "schema_exact_keys_every_structural_arm", "schema_shared_payload_exact_keys_every_arm", "schema_matrix_payload_exact_keys_every_arm", "schema_testing_payload_exact_keys_every_arm", "schema_reference_exact_keys_every_arm", "schema_canonical_key_order_every_arm", "schema_duplicate_assignment_rejected", "schema_duplicate_table_rejected", "schema_duplicate_nested_payload_rejected", "noncanonical_comment", "noncanonical_inline_table", "schema_priority_band_closed_enum", "schema_observed_at_civil_date", "schema_held_permanent_rejected", "schema_due_on_valid_through_postures",
] as const;

export type RequiredSchemaSelfTestCaseId = (typeof REQUIRED_SCHEMA_SELFTEST_CASE_IDS)[number];

const ALL_SCHEMA_ROWS = [
  ...ROADMAP_SCHEMA_ROWS,
  ...REFERENCE_SCHEMA_ROWS,
  ...SHARED_SEMANTIC_SCHEMA_ROWS,
  ...MATRIX_SCHEMA_ROWS,
  ...TESTING_SCHEMA_ROWS,
] as const;
function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

function expectFailure(run: () => unknown, codes: readonly string[], path?: string): RoadmapWireError {
  try { run(); } catch (error) {
    assert(error instanceof RoadmapWireError, `expected RoadmapWireError, got ${String(error)}`);
    assert(codes.includes(error.issue.code), `expected ${codes.join("|")}, got ${error.issue.code}`);
    if (path !== undefined) assert(error.issue.logical_path === path, `expected ${path}, got ${error.issue.logical_path}`);
    observeSelfTestIssue(error.issue);
    return error;
  }
  throw new Error(`expected ${codes.join("|")}`);
}

function minimalRoadmap(): Uint8Array {
  return text(`[document]
schema_version = 3
roadmap = "matrix"
source_path = "fixture/matrix.toml"
projection_path = "fixture/matrix.md"

[[section]]
section_id = "fixture"
title = "Fixture"
body_md = '''S
'''
entries = [
  "matrix.fixture-minimal",
]

[[record]]
id = "matrix.fixture-minimal"
title = "Fixture record"

[record.payload]
kind = "work"
body_md = '''R
'''
work_state = "ready"
work_intent = "build_capability"
work_kind = "feature"
risk = "cosmetic"
acceptance_md = '''Accepted.
'''
priority_rationale_md = '''Normal.
'''
`);
}

function minimalDocument(): RoadmapDocumentV3 {
  const decoded = decodeRoadmapSource(minimalRoadmap(), "<v3>", "matrix");
  assert(decoded.document.schema_version === 3, "minimal v3 fixture did not decode as v3");
  return decoded;
}

/**
 * A semantic document carrying the subordinate owner kinds, used by the lifecycle-disposition
 * arms. Both subordinate kinds declare the one disposition their kind permits.
 */
function subordinateRoadmap(): string {
  return `[document]\nschema_version = 3\nroadmap = "matrix"\nsource_path = "fixture/subordinate.toml"\nprojection_path = "fixture/subordinate.md"\n\n[[section]]\nsection_id = "fixture"\ntitle = "Fixture"\nbody_md = '''S{{slot:status}}\n'''\nentries = [\n  "matrix.fixture-record",\n  "part",\n]\n\n[section.slots.status]\nbinding = "fixture-status"\n\n[[record]]\nid = "matrix.fixture-record"\ntitle = "Record"\n\n[record.payload]\nkind = "work"\nbody_md = '''R\n'''\nwork_state = "ready"\nwork_intent = "build_capability"\nwork_kind = "feature"\nrisk = "cosmetic"\nacceptance_md = '''Accepted.\n'''\npriority_rationale_md = '''Normal.\n'''\n\n[[part]]\npart_id = "part"\nparent_record_id = "matrix.fixture-record"\nbody_md = '''P\n'''\n`;
}

/**
 * The pending-review posture is authored on an unplaced (non-rendering) record: no body_md and
 * no section placement. This synthetic source is the exact-key corpus target for that row.
 */
function pendingReviewRoadmap(): Uint8Array {
  const record = `\n[[record]]\nid = "matrix.fixture-review-pending"\ntitle = "Pending review"\n\n[record.payload]\nkind = "work"\nwork_state = "pending_review"\nwork_intent = "build_capability"\nwork_kind = "feature"\nrisk = "cosmetic"\nuncertainty_md = '''Review required.\n'''\n`;
  return text(subordinateRoadmap().replace("\n[[part]]", `${record}\n[[part]]`));
}

function decodePayload(body: string, roadmap: RoadmapName): unknown {
  const bindings = shieldTomlMarkdown(text(`[p]\n${body}`), "<payload>");
  const ctx: DecodeContext = { source: bindings.source, bindings };
  const root = expectExactTable(ctx, bindings.parsed, "$", { name: "payload root", required: ["p"] });
  const raw = requiredValue(root, "p");
  const pre = expectExactTable(ctx, raw, "p", { name: "payload kind", required: ["kind"], optional: raw !== null && typeof raw === "object" && !Array.isArray(raw) ? Object.keys(raw).filter((key) => key !== "kind") : [] });
  const kind = requiredValue(pre, "kind");
  assert(typeof kind === "string", "payload kind string");
  const shared = decodeSharedSemanticPayload(ctx, raw, "p");
  const decoded = shared ?? (roadmap === "matrix" ? decodeMatrixPayload(ctx, raw, "p", kind) : decodeTestingPayload(ctx, raw, "p", kind));
  if (decoded === undefined) schemaFail(ctx, "E-SCHEMA-ENUM", "p.kind", `unknown ${roadmap} payload kind`);
  bindings.assertAllConsumed();
  return decoded;
}

const READY = `kind = "work"\nwork_state = "ready"\nwork_intent = "build_capability"\nwork_kind = "feature"\nrisk = "cosmetic"\nacceptance_md = """Accepted."""\npriority_band = "normal"\npriority_rationale_md = """Normal."""\n`;

/** An armed work carrying the nested promotion trigger — the only remaining trigger packaging. */
function predicateTransition(predicate: string): string {
  return `kind = "work"\nwork_state = "armed"\nwork_intent = "optimize"\nwork_kind = "optimization"\nrisk = "cosmetic"\ncontrol_ids = ["matrix.fixture-control"]\n\n[p.promotion_trigger]\nobserver = "operator"\ndimension = "count"\nobservable = "fixture"\naction_on_fire_md = """Act."""\nevaluation = "unknown"\n\n[p.promotion_trigger.predicate]\n${predicate}`;
}

const FIXTURE_ROOT = "cddl-matrix/roadmap/fixtures" as RepoPath;
function readFixture(context: SelfTestContext, relative: string): Uint8Array {
  const inventory = context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
  const path = inventory.find((entry) => entry === relative);
  assert(path !== undefined, `fixture inventory contains ${relative}`);
  return context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, path);
}

function roadmapFixture(context: SelfTestContext, relative: string, roadmap: RoadmapName): RoadmapDocumentV3 {
  const doc = decodeRoadmapSource(readFixture(context, relative), relative, roadmap);
  assert(doc.document.schema_version === 3, `${relative} is v3`);
  return doc as RoadmapDocumentV3;
}

function fixtureIdentity(context: SelfTestContext): void {
  const paths: readonly [string, RoadmapName][] = [
    ["positive/small-matrix-v3.toml", "matrix"], ["positive/small-testing-v3.toml", "testing"],
    ["all-fields/matrix-v3.toml", "matrix"], ["all-fields/testing-v3.toml", "testing"],
  ];
  for (const [path, roadmap] of paths) {
    const bytes = readFixture(context, path);
    const doc = decodeRoadmapSource(bytes, path, roadmap);
    assert(bytesEqual(composeRoadmapDocument(doc), bytes), `${path} canonical identity`);
  }
  const subordinate = text(subordinateRoadmap());
  const subordinateDoc = decodeRoadmapSource(subordinate, "synthetic/subordinate-matrix-v3.toml", "matrix", false);
  assert(bytesEqual(composeRoadmapDocument(subordinateDoc), subordinate), "synthetic subordinate canonical identity");
}

function allFieldsCoverage(context: SelfTestContext): void {
  const matrix = roadmapFixture(context, "all-fields/matrix-v3.toml", "matrix");
  const testing = roadmapFixture(context, "all-fields/testing-v3.toml", "testing");
  assert(
    [matrix.document.roadmap, testing.document.roadmap].sort().join("|") === "matrix|testing",
    "all-fields documents must structurally cover both RoadmapName values",
  );
  const tags = [...matrix.records, ...testing.records].flatMap((record) => record.tags ?? []);
  const comparison = compareAllFieldsCoverageTags(tags);
  assert(comparison.ok, `all-fields closed set: ${JSON.stringify(comparison)}`);
}

function replaceAfter(source: string, anchor: string, before: string, after: string): string {
  const anchorIndex = source.indexOf(anchor);
  assert(anchorIndex !== -1, `mutation anchor not found: ${anchor}`);
  const valueIndex = source.indexOf(before, anchorIndex + anchor.length);
  assert(valueIndex !== -1, `mutation value not found after ${anchor}: ${before}`);
  return `${source.slice(0, valueIndex)}${after}${source.slice(valueIndex + before.length)}`;
}

function fixtureText(context: SelfTestContext, relative: string): string {
  return new TextDecoder().decode(readFixture(context, relative));
}

function canonicalRoadmapFixture(
  context: SelfTestContext,
  relative: string,
  roadmap: RoadmapName,
): ReturnType<typeof decodeRoadmapSource> {
  const decoded = decodeRoadmapSource(readFixture(context, relative), relative, roadmap, false);
  const canonical = composeRoadmapDocument(decoded);
  const reparsed = decodeRoadmapSource(canonical, `${relative}#canonical`, roadmap);
  assert(bytesEqual(composeRoadmapDocument(reparsed), canonical), `${relative} canonical loader identity`);
  return reparsed;
}

type SchemaGroupName = "roadmap" | "reference" | "semantic" | "matrix" | "testing";
type EnumGroupName = "roadmap" | "semantic" | "matrix" | "testing";

interface FixtureSource {
  readonly path: string;
  readonly bytes: Uint8Array;
  readonly kind: "roadmap";
  readonly roadmap?: RoadmapName;
  readonly enum_groups: readonly EnumGroupName[];
}

interface TomlSection {
  readonly path: string;
  readonly header_line: number;
  readonly body_start_line: number;
  end_line: number;
}

interface TomlAssignment {
  readonly key: string;
  readonly start_line: number;
  readonly end_line: number;
  readonly expression: string;
}

interface FixtureMutationTarget {
  readonly fixture: FixtureSource;
  readonly logical_path: string;
}

interface EnumMutationTarget extends FixtureMutationTarget {
  readonly field_path: string;
  readonly key: string;
  readonly indexed: boolean;
  readonly expected_code: "E-SCHEMA-ENUM" | "E-SCHEMA-VERSION";
}

interface RowMutationCounts {
  unknown: number;
  required: number;
  forbidden: number;
}

export interface SchemaFixtureMutationReceipt {
  readonly fixture_reads: number;
  readonly discovery_loads: number;
  readonly mutation_loads: number;
  readonly table_targets: number;
  readonly enum_targets: number;
  readonly unknown_mutations: number;
  readonly required_mutations: number;
  readonly forbidden_mutations: number;
  readonly enum_mutations: number;
}

interface FixtureMutationProof {
  readonly receipt: SchemaFixtureMutationReceipt;
  readonly row_counts: ReadonlyMap<ExactSchemaRow, RowMutationCounts>;
  readonly enum_counts: ReadonlyMap<string, number>;
  readonly observed_issues: readonly RoadmapIssue[];
}

const SCHEMA_ROW_GROUPS: readonly { readonly name: SchemaGroupName; readonly rows: readonly ExactSchemaRow[] }[] = [
  { name: "roadmap", rows: ROADMAP_SCHEMA_ROWS },
  { name: "reference", rows: REFERENCE_SCHEMA_ROWS },
  { name: "semantic", rows: SHARED_SEMANTIC_SCHEMA_ROWS },
  { name: "matrix", rows: MATRIX_SCHEMA_ROWS },
  { name: "testing", rows: TESTING_SCHEMA_ROWS },
];

const ENUM_FIELD_GROUPS: readonly { readonly name: EnumGroupName; readonly fields: readonly EnumSchemaField[] }[] = [
  { name: "roadmap", fields: ROADMAP_ENUM_FIELDS },
  { name: "semantic", fields: SEMANTIC_ENUM_FIELDS },
  { name: "matrix", fields: MATRIX_ENUM_FIELDS },
  { name: "testing", fields: TESTING_ENUM_FIELDS },
];

const ENUM_KEY_OVERRIDES: Readonly<Record<string, string>> = {
  [["roadmap", "relation_kind"].join(":")]: "kind",
  [["roadmap", "reference_kind"].join(":")]: "kind",
  "semantic:shared_semantic_kind": "kind",
  "semantic:decision_permanence": "permanence",
  "semantic:transition_evaluation": "evaluation",
  "matrix:matrix_semantic_kind": "kind",
  "matrix:policy_permanence": "permanence",
  "testing:testing_semantic_kind": "kind",
};

function productionFixtureSources(context: SelfTestContext): { fixtures: readonly FixtureSource[]; reads: number } {
  const fixtures: FixtureSource[] = [];
  let reads = 0;
  const read = (path: string): Uint8Array => {
    reads++;
    return readFixture(context, path);
  };
  const roadmapFixtures = [
    ["positive/small-matrix-v3.toml", "matrix"], ["positive/small-testing-v3.toml", "testing"],
    ["all-fields/matrix-v3.toml", "matrix"], ["all-fields/testing-v3.toml", "testing"],
  ] as const;
  for (const [path, roadmap] of roadmapFixtures) {
    fixtures.push({
      path,
      bytes: read(path),
      kind: "roadmap",
      roadmap,
      enum_groups: roadmap === "matrix" ? ["roadmap", "semantic", "matrix"] : ["roadmap", "semantic", "testing"],
    });
  }
  fixtures.push({
    path: "synthetic/minimal-matrix-v3.toml",
    bytes: minimalRoadmap(),
    kind: "roadmap",
    roadmap: "matrix",
    enum_groups: ["roadmap", "semantic", "matrix"],
  });
  fixtures.push({
    path: "synthetic/pending-review-matrix-v3.toml",
    bytes: pendingReviewRoadmap(),
    kind: "roadmap",
    roadmap: "matrix",
    enum_groups: ["roadmap", "semantic", "matrix"],
  });
  return { fixtures, reads };
}

function loadFixtureSource(fixture: FixtureSource, bytes: Uint8Array, trace?: SchemaDecodeTrace): void {
  decodeRoadmapSource(bytes, fixture.path, fixture.roadmap, false, trace);
}

function resolveHeaderPath(components: readonly string[], currentIndices: ReadonlyMap<string, number>): string {
  let logical = "";
  for (const component of components) {
    const key = logical === "" ? component : `${logical}.${component}`;
    const index = currentIndices.get(key);
    logical = index === undefined ? key : `${key}[${index}]`;
  }
  return logical;
}

function scanTomlSections(bytes: Uint8Array): { raw: string[]; shielded: string[]; sections: readonly TomlSection[] } {
  const raw = new TextDecoder().decode(bytes).split("\n");
  const bindings = shieldTomlMarkdown(bytes, "<fixture-scan>");
  const masked = bytes.slice();
  for (const token of bindings.tokens) {
    for (let index = token.start_byte; index < token.end_byte; index++) {
      if (masked[index] !== 0x0a) masked[index] = 0x20;
    }
  }
  const shielded = new TextDecoder().decode(masked).split("\n");
  assert(raw.length === shielded.length, "Markdown shielding preserves fixture line count");
  const sections: TomlSection[] = [{ path: "$", header_line: -1, body_start_line: 0, end_line: raw.length }];
  const currentIndices = new Map<string, number>();
  const nextIndices = new Map<string, number>();
  for (let line = 0; line < shielded.length; line++) {
    const match = /^\s*(\[\[|\[)([A-Za-z0-9_.-]+)(\]\]|\])\s*$/.exec(shielded[line]!);
    if (match === null) continue;
    sections[sections.length - 1]!.end_line = line;
    const array = match[1] === "[[";
    const components = match[2]!.split(".");
    let path: string;
    if (array) {
      const parent = resolveHeaderPath(components.slice(0, -1), currentIndices);
      const name = components.at(-1)!;
      const key = parent === "" ? name : `${parent}.${name}`;
      const index = nextIndices.get(key) ?? 0;
      nextIndices.set(key, index + 1);
      currentIndices.set(key, index);
      path = `${key}[${index}]`;
    } else {
      path = resolveHeaderPath(components, currentIndices);
    }
    sections.push({ path, header_line: line, body_start_line: line + 1, end_line: raw.length });
  }
  return { raw, shielded, sections };
}

function assignmentsInSection(scan: ReturnType<typeof scanTomlSections>, section: TomlSection): readonly TomlAssignment[] {
  const starts: { key: string; line: number; expression: string }[] = [];
  for (let line = section.body_start_line; line < section.end_line; line++) {
    const match = /^([A-Za-z_][A-Za-z0-9_-]*)\s*=\s*(.*)$/.exec(scan.shielded[line]!);
    if (match !== null) starts.push({ key: match[1]!, line, expression: match[2]! });
  }
  return starts.map((entry, index) => ({
    key: entry.key,
    start_line: entry.line,
    end_line: starts[index + 1]?.line ?? section.end_line,
    expression: entry.expression,
  }));
}

function encodeLines(lines: readonly string[]): Uint8Array {
  return text(lines.join("\n"));
}

function spliceLines(bytes: Uint8Array, start: number, end: number, replacement: readonly string[]): Uint8Array {
  const lines = new TextDecoder().decode(bytes).split("\n");
  lines.splice(start, end - start, ...replacement);
  return encodeLines(lines);
}

function sectionForPath(scan: ReturnType<typeof scanTomlSections>, path: string): TomlSection | undefined {
  return scan.sections.find((section) => section.path === path);
}

function insertAssignment(bytes: Uint8Array, tablePath: string, key: string, expression: string): Uint8Array {
  const scan = scanTomlSections(bytes);
  const section = sectionForPath(scan, tablePath);
  if (section !== undefined) return spliceLines(bytes, section.body_start_line, section.body_start_line, [`${key} = ${expression}`]);
  throw new Error(`fixture table section not found: ${tablePath}`);
}

function replaceAssignmentExpression(bytes: Uint8Array, tablePath: string, key: string, expression: string): Uint8Array {
  const scan = scanTomlSections(bytes);
  const section = sectionForPath(scan, tablePath);
  assert(section !== undefined, `fixture enum table section ${tablePath}`);
  const assignment = assignmentsInSection(scan, section).find((candidate) => candidate.key === key);
  assert(assignment !== undefined, `fixture enum assignment ${tablePath}.${key}`);
  return spliceLines(bytes, assignment.start_line, assignment.end_line, [`${key} = ${expression}`]);
}

function removeSections(bytes: Uint8Array, predicate: (section: TomlSection) => boolean): Uint8Array {
  const scan = scanTomlSections(bytes);
  const lines = [...scan.raw];
  const sections = scan.sections.filter((section) => section.header_line >= 0 && predicate(section)).sort((left, right) => right.header_line - left.header_line);
  assert(sections.length !== 0, "fixture child table target");
  for (const section of sections) lines.splice(section.header_line, section.end_line - section.header_line);
  return encodeLines(lines);
}

function removeRequiredKey(bytes: Uint8Array, tablePath: string, key: string): Uint8Array {
  const scan = scanTomlSections(bytes);
  const section = sectionForPath(scan, tablePath);
  if (section !== undefined) {
    const assignment = assignmentsInSection(scan, section).find((candidate) => candidate.key === key);
    if (assignment !== undefined) return spliceLines(bytes, assignment.start_line, assignment.end_line, []);
  }
  const childPath = tablePath === "$" ? key : `${tablePath}.${key}`;
  const removed = removeSections(bytes, (candidate) => candidate.path === childPath || candidate.path.startsWith(`${childPath}[`) || candidate.path.startsWith(`${childPath}.`));
  return removed;
}

function enumFieldKey(group: EnumGroupName, field: EnumSchemaField): string {
  return ENUM_KEY_OVERRIDES[`${group}:${field.name}`] ?? field.name;
}

function enumFieldId(group: EnumGroupName, field: EnumSchemaField): string {
  return `${group}:${field.name}`;
}

function expressionContainsEnumValue(expression: string, values: readonly string[]): boolean {
  const trimmed = expression.trim();
  return values.some((value) => trimmed === JSON.stringify(value) || trimmed === value || trimmed.includes(JSON.stringify(value)));
}

function enumCandidateMatches(field: EnumSchemaField, section: TomlSection, assignments: readonly TomlAssignment[]): boolean {
  const keys = new Set(assignments.map((assignment) => assignment.key));
  if (field.name === "schema_version") return section.path === "document";
  if (field.name === "relation_kind") return section.path.startsWith("relation[");
  if (field.name === "reference_kind") return section.path.startsWith("reference[");
  if (field.name === "decision_permanence") return keys.has("decision_state");
  if (field.name === "policy_permanence") return keys.has("policy_kind");
  return true;
}

function findEnumTarget(
  fixtures: readonly FixtureSource[],
  group: EnumGroupName,
  field: EnumSchemaField,
): EnumMutationTarget | undefined {
  const key = enumFieldKey(group, field);
  for (const fixture of fixtures) {
    if (!fixture.enum_groups.includes(group)) continue;
    const scan = scanTomlSections(fixture.bytes);
    for (const section of scan.sections) {
      const assignments = assignmentsInSection(scan, section);
      if (!enumCandidateMatches(field, section, assignments)) continue;
      const assignment = assignments.find((candidate) => candidate.key === key && expressionContainsEnumValue(candidate.expression, field.values));
      if (assignment === undefined) continue;
      const indexed = assignment.expression.trim().startsWith("[");
      const fieldPath = childLogicalPath(section.path, key) + (indexed ? "[0]" : "");
      return {
        fixture,
        logical_path: section.path,
        field_path: fieldPath,
        key,
        indexed,
        expected_code: field.name === "schema_version" ? "E-SCHEMA-VERSION" : "E-SCHEMA-ENUM",
      };
    }
  }
  return undefined;
}

function assertRowMutationCoverage(proof: FixtureMutationProof, rows: readonly ExactSchemaRow[]): void {
  for (const row of rows) {
    const counts = proof.row_counts.get(row);
    assert(counts !== undefined, `${row.name}: fixture mutation receipt`);
    assert(counts.unknown === 1, `${row.name}: one production unknown-key mutation`);
    assert(counts.required === row.required.length, `${row.name}: every required key mutated through production loader`);
    assert(counts.forbidden === (row.forbidden?.length ?? 0), `${row.name}: every forbidden key mutated through production loader`);
  }
}

function buildFixtureMutationProof(context: SelfTestContext): FixtureMutationProof {
  const { fixtures, reads } = productionFixtureSources(context);
  const declaredRows = new Set(ALL_SCHEMA_ROWS);
  const rowNames = new Set<string>();
  for (const row of ALL_SCHEMA_ROWS) {
    assert(!rowNames.has(row.name), `duplicate schema row ${row.name}`);
    rowNames.add(row.name);
    const required = new Set(row.required);
    const optional = new Set(row.optional ?? []);
    const forbidden = new Set(row.forbidden ?? []);
    assert(required.size === row.required.length, `${row.name}: duplicate required key`);
    assert(optional.size === (row.optional?.length ?? 0), `${row.name}: duplicate optional key`);
    assert(forbidden.size === (row.forbidden?.length ?? 0), `${row.name}: duplicate forbidden key`);
    for (const key of required) assert(!optional.has(key) && !forbidden.has(key), `${row.name}: overlapping ${key}`);
    for (const key of optional) assert(!forbidden.has(key), `${row.name}: optional/forbidden ${key}`);
  }
  for (const group of ENUM_FIELD_GROUPS) {
    for (const field of group.fields) {
      assert(field.values.length !== 0 && new Set(field.values).size === field.values.length, `${enumFieldId(group.name, field)}: closed distinct values`);
    }
  }
  const rowTargets = new Map<ExactSchemaRow, FixtureMutationTarget>();
  let discoveryLoads = 0;
  for (const fixture of fixtures) {
    const trace: SchemaDecodeTrace = {
      exactTable(schema, logicalPath): void {
        if (declaredRows.has(schema) && !rowTargets.has(schema)) rowTargets.set(schema, { fixture, logical_path: logicalPath });
      },
      enum(): void {},
    };
    loadFixtureSource(fixture, fixture.bytes, trace);
    discoveryLoads++;
  }
  for (const group of SCHEMA_ROW_GROUPS) {
    for (const row of group.rows) assert(rowTargets.has(row), `${group.name}:${row.name} lacks a production fixture table target`);
  }

  const enumTargets = new Map<string, EnumMutationTarget>();
  for (const group of ENUM_FIELD_GROUPS) {
    for (const field of group.fields) {
      const target = findEnumTarget(fixtures, group.name, field);
      assert(target !== undefined, `${enumFieldId(group.name, field)} lacks a production fixture enum target`);
      enumTargets.set(enumFieldId(group.name, field), target);
    }
  }

  const rowCounts = new Map<ExactSchemaRow, RowMutationCounts>();
  const observedIssues: RoadmapIssue[] = [];
  let mutationLoads = 0;
  let unknownMutations = 0;
  let requiredMutations = 0;
  let forbiddenMutations = 0;
  for (const row of ALL_SCHEMA_ROWS) {
    const target = rowTargets.get(row)!;
    const counts: RowMutationCounts = { unknown: 0, required: 0, forbidden: 0 };
    const unknownKey = "fixture_schema_unknown";
    const unknown = insertAssignment(target.fixture.bytes, target.logical_path, unknownKey, "true");
    observedIssues.push(expectFailure(
      () => loadFixtureSource(target.fixture, unknown),
      ["E-SCHEMA-UNKNOWN-KEY"],
      childLogicalPath(target.logical_path, unknownKey),
    ).issue);
    counts.unknown++;
    unknownMutations++;
    mutationLoads++;

    for (const key of row.required) {
      const missing = removeRequiredKey(target.fixture.bytes, target.logical_path, key);
      try {
        observedIssues.push(expectFailure(
          () => loadFixtureSource(target.fixture, missing),
          ["E-SCHEMA-MISSING-KEY"],
          childLogicalPath(target.logical_path, key),
        ).issue);
      } catch (error) {
        throw new Error(`${row.name}.${key} fixture mutation: ${error instanceof Error ? error.message : String(error)}`);
      }
      counts.required++;
      requiredMutations++;
      mutationLoads++;
    }
    for (const key of row.forbidden ?? []) {
      const forbidden = insertAssignment(target.fixture.bytes, target.logical_path, key, "true");
      try {
        observedIssues.push(expectFailure(
          () => loadFixtureSource(target.fixture, forbidden),
          ["E-SCHEMA-FORBIDDEN-KEY"],
          childLogicalPath(target.logical_path, key),
        ).issue);
      } catch (error) {
        throw new Error(`${row.name}.${key} fixture mutation: ${error instanceof Error ? error.message : String(error)}`);
      }
      counts.forbidden++;
      forbiddenMutations++;
      mutationLoads++;
    }
    rowCounts.set(row, counts);
  }

  const enumCounts = new Map<string, number>();
  let enumMutations = 0;
  for (const group of ENUM_FIELD_GROUPS) {
    for (const field of group.fields) {
      const id = enumFieldId(group.name, field);
      const target = enumTargets.get(id)!;
      const expression = target.expected_code === "E-SCHEMA-VERSION" ? "999" : target.indexed ? '["__invalid__"]' : '"__invalid__"';
      const invalid = replaceAssignmentExpression(target.fixture.bytes, target.logical_path, target.key, expression);
      observedIssues.push(expectFailure(
        () => loadFixtureSource(target.fixture, invalid),
        target.expected_code === "E-SCHEMA-VERSION"
          ? ["E-SCHEMA-VERSION", "E-SCHEMA-ENUM"]
          : [target.expected_code],
        target.field_path,
      ).issue);
      enumCounts.set(id, 1);
      enumMutations++;
      mutationLoads++;
    }
  }

  assert(rowCounts.size === ALL_SCHEMA_ROWS.length, "every declared schema row has mutation counters");
  assert(enumCounts.size === ENUM_FIELD_GROUPS.reduce((count, group) => count + group.fields.length, 0), "every declared enum field has mutation counters");
  return {
    row_counts: rowCounts,
    enum_counts: enumCounts,
    observed_issues: Object.freeze(observedIssues),
    receipt: {
      fixture_reads: reads,
      discovery_loads: discoveryLoads,
      mutation_loads: mutationLoads,
      table_targets: rowTargets.size,
      enum_targets: enumTargets.size,
      unknown_mutations: unknownMutations,
      required_mutations: requiredMutations,
      forbidden_mutations: forbiddenMutations,
      enum_mutations: enumMutations,
    },
  };
}

const FIXTURE_MUTATION_PROOFS = new WeakMap<SelfTestContext, FixtureMutationProof>();

function fixtureMutationProof(context: SelfTestContext): FixtureMutationProof {
  const prior = FIXTURE_MUTATION_PROOFS.get(context);
  if (prior !== undefined) return prior;
  const proof = buildFixtureMutationProof(context);
  FIXTURE_MUTATION_PROOFS.set(context, proof);
  return proof;
}

function observeProofIssue(
  proof: FixtureMutationProof,
  predicate: (issue: RoadmapIssue) => boolean,
): void {
  const matched = proof.observed_issues.find(predicate);
  assert(matched !== undefined, "fixture mutation proof lacks its declared representative issue");
  observeSelfTestIssue(matched);
}

function assertNeedlesInOrder(source: string, needles: readonly string[], message: string): void {
  let cursor = 0;
  for (const needle of needles) {
    const index = source.indexOf(needle, cursor);
    assert(index !== -1, `${message}: missing or out-of-order ${needle}`);
    cursor = index + needle.length;
  }
}

function duplicateParseRejected(source: string): void {
  expectFailure(() => decodeRoadmapSource(text(source), "<duplicate>", "matrix"), ["E-TOML-PARSE", "E-TOML-NONCANONICAL"]);
}

function execute(id: RequiredSchemaSelfTestCaseId, context?: SelfTestContext): void {
  switch (id) {
    case "strict_unknown_top": expectFailure(() => decodeRoadmapSource(text(`unknown = 1\n\n${new TextDecoder().decode(minimalRoadmap())}`), "<unknown-top>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"], "unknown"); return;
    case "strict_unknown_nested_record": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap()).replace('title = "Fixture record"', 'title = "Fixture record"\nunknown = 1')), "<unknown-record>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"]); return;
    case "strict_unknown_every_table": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), ALL_SCHEMA_ROWS);
      return;
    }
    case "strict_enum_every_field": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const proof = fixtureMutationProof(context);
      for (const group of ENUM_FIELD_GROUPS) {
        for (const field of group.fields) assert(proof.enum_counts.get(enumFieldId(group.name, field)) === 1, `${enumFieldId(group.name, field)} production enum mutation`);
      }
      observeProofIssue(proof, (issue) => issue.code === "E-SCHEMA-VERSION" || issue.code === "E-SCHEMA-ENUM");
      return;
    }
    case "strict_unknown_enum": expectFailure(() => decodePayload(READY.replace('work_state = "ready"', 'work_state = "unknown"'), "matrix"), ["E-SCHEMA-ENUM"], "p.work_state"); return;
    case "schema_priority_band_closed_enum": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assert(fixtureMutationProof(context).enum_counts.get("semantic:priority_band") === 1, "priority_band production enum mutation");
      return;
    }
    case "strict_unknown_kind": expectFailure(() => decodePayload('kind = "unknown"\n', "matrix"), ["E-SCHEMA-ENUM"]); return;
    case "strict_missing_discriminator": expectFailure(() => decodePayload('body_md = """Missing."""\n', "matrix"), ["E-SCHEMA-MISSING-KEY"]); return;
    case "strict_generic_state_rejected": expectFailure(() => decodePayload(`${READY}state = "ready"\n`, "matrix"), ["E-SCHEMA-UNKNOWN-KEY"]); return;
    case "strict_generic_disposition_rejected": expectFailure(() => decodePayload(`${READY}disposition = "ready"\n`, "matrix"), ["E-SCHEMA-UNKNOWN-KEY"]); return;
    case "missing_section_entries": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap()).replace(/entries = \[\n(?:.*\n)*?\]\n/u, "")), "<missing-entries>", "matrix"), ["E-SCHEMA-MISSING-KEY"]); return;
    case "empty_records_floor": {
      const canonical = new TextDecoder().decode(minimalRoadmap());
      const withoutRecord = canonical
        .replace(/\n\[\[record\]\][\s\S]*$/u, "\n")
        .replace(/entries = \[\n(?:.*\n)*?\]\n/u, "entries = []\n");
      expectFailure(() => decodeRoadmapSource(text(withoutRecord), "<empty-records>", "matrix"), ["E-SCHEMA-MISSING-KEY", "E-SCHEMA-FLOOR"]);
      return;
    }
    case "all_fields_identity":
      assert(context !== undefined, `${id} requires fixture ports`); fixtureIdentity(context); return;
    case "domain_matrix_all_tags": case "domain_testing_all_tags":
      assert(context !== undefined, `${id} requires fixture ports`); allFieldsCoverage(context); return;
    case "noncanonical_comment": expectFailure(() => decodeRoadmapSource(text(`# comment\n${new TextDecoder().decode(minimalRoadmap())}`), "<comment>", "matrix"), ["E-TOML-NONCANONICAL"]); return;
    case "noncanonical_inline_table": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap()).replace('entries = [\n  "matrix.fixture-minimal",\n]', 'entries = ["matrix.fixture-minimal"]\nslots = { status = { binding = "fixture-status" } }')), "<inline-table>", "matrix"), ["E-TOML-NONCANONICAL"]); return;
    // The D7 flip made the literal spelling canonical, so the alternate form to reject is the
    // basic one; both quote forms decode alike, so the canonical-bytes comparison refuses it.
    case "noncanonical_basic_string": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap()).replace("body_md = '''S\n'''", 'body_md = """S\n"""')), "<basic>", "matrix"), ["E-TOML-NONCANONICAL"]); return;
    case "noncanonical_set_order": {
      expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap()).replace('title = "Fixture"', 'title = "Fixture"\nlegacy_aliases = ["z", "a"]')), "<set-order>", "matrix"), ["E-TOML-NONCANONICAL"]);
      return;
    }
    case "noncanonical_table_order": {
      const source = new TextDecoder().decode(minimalRoadmap());
      const section = source.match(/\n\[\[section\]\][\s\S]*?(?=\n\[\[record\]\])/)?.[0];
      const record = source.match(/\n\[\[record\]\][\s\S]*$/u)?.[0];
      assert(section !== undefined && record !== undefined, "canonical fixture blocks");
      const swapped = source.replace(section, "__SECTION__").replace(record, section).replace("__SECTION__", record);
      expectFailure(() => decodeRoadmapSource(text(swapped), "<table-order>", "matrix"), ["E-TOML-NONCANONICAL"]);
      return;
    }
    case "toml_terminal_newline": assert(composeRoadmapDocument(decodeRoadmapSource(minimalRoadmap(), "<terminal>", "matrix")).at(-1) === 0x0a, "one TOML terminal LF"); return;
    case "v3_semantic_identity": {
      const decoded = minimalDocument();
      const composed = composeRoadmapDocument(decoded);
      assert(bytesEqual(composed, minimalRoadmap()), "schema v3 did not preserve canonical wire identity");
      const roundTrip = decodeRoadmapSource(composed, "<v3-round-trip>", "matrix");
      assert(roundTrip.document.schema_version === 3, "schema v3 round-trip changed the document version");
      assert(bytesEqual(composeRoadmapDocument(roundTrip), composed), "schema v3 second composition drifted");
      return;
    }
    case "v2_unsupported":
      expectFailure(
        () => decodeRoadmapSource(
          text(new TextDecoder().decode(minimalRoadmap()).replace("schema_version = 3", "schema_version = 2")),
          "<v2>",
          "matrix",
        ),
        ["E-SCHEMA-VERSION"],
        "document.schema_version",
      );
      return;
    case "v3_retired_keys_rejected": {
      // Every v2 scaffolding key the conversion deleted is now an unknown key, closed-schema style.
      const canonical = new TextDecoder().decode(minimalRoadmap());
      expectFailure(() => decodeRoadmapSource(text(canonical.replace('roadmap = "matrix"', 'authority = "authoritative"\nroadmap = "matrix"')), "<retired-authority>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"], "document.authority");
      expectFailure(() => decodeRoadmapSource(text(canonical.replace('projection_path = "fixture/matrix.md"', 'projection_path = "fixture/matrix.md"\nfrozen_source_sha256 = "' + ZERO_HASH + '"')), "<retired-frozen>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"], "document.frozen_source_sha256");
      expectFailure(() => decodeRoadmapSource(text(canonical.replace('title = "Fixture record"', 'title = "Fixture record"\nprojection_group = "fixture"')), "<retired-projection-group>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"], "record[0].projection_group");
      expectFailure(() => decodeRoadmapSource(text(canonical.replace('title = "Fixture"', 'title = "Fixture"\nrender_authority = "semantic"')), "<retired-render-authority>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"], "section[0].render_authority");
      expectFailure(() => decodeRoadmapSource(text(canonical.replace('kind = "work"', 'kind = "work"\nsummary_md = """R\n"""')), "<retired-summary>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"], "record[0].payload.summary_md");
      expectFailure(() => decodeRoadmapSource(text(`${canonical}\n[[source_span]]\nid = "span"\n`), "<retired-span>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"], "source_span");
      expectFailure(() => decodeRoadmapSource(text(`${canonical}\n[[manifest.entry]]\nkind = "section"\nsection_id = "fixture"\n`), "<retired-manifest>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"], "manifest");
      return;
    }
    case "transition_observable_arm_dependent": {
      const eventTransition = `kind = "work"\nwork_state = "armed"\nwork_intent = "optimize"\nwork_kind = "optimization"\nrisk = "cosmetic"\ncontrol_ids = ["matrix.fixture-control"]\n\n[p.promotion_trigger]\nobserver = "operator"\ndimension = "count"\naction_on_fire_md = """Act."""\nevaluation = "unknown"\n\n[p.promotion_trigger.predicate]\npredicate_kind = "event"\nevent_md = """Event."""\nevidence_ids = ["matrix.fixture-evidence"]\n`;
      const decodedEvent = decodePayload(eventTransition, "matrix");
      assert(decodedEvent !== undefined, "event-condition trigger decodes without a trigger-level observable");
      expectFailure(() => decodePayload(eventTransition.replace('dimension = "count"', 'dimension = "count"\nobservable = "fixture"'), "matrix"), ["E-SCHEMA-FORBIDDEN-KEY"], "p.promotion_trigger.observable");
      const manualTransition = predicateTransition('predicate_kind = "manual"\nreview_procedure_md = """Review."""\nevidence_ids = ["matrix.fixture-evidence"]\n');
      assert(decodePayload(manualTransition, "matrix") !== undefined, "manual-condition trigger keeps its authored observable");
      expectFailure(() => decodePayload(manualTransition.replace('observable = "fixture"\n', ""), "matrix"), ["E-SCHEMA-MISSING-KEY"], "p.promotion_trigger.observable");
      const quantitative = predicateTransition('predicate_kind = "quantitative"\ncomparator = "ge"\nthreshold = 2\nunit = "constructs"\nscope = "fixture"\nmeasurement = 1\nas_of = "2026-08-11"\nevidence_ids = ["matrix.fixture-evidence"]\n');
      assert(decodePayload(quantitative, "matrix") !== undefined, "quantitative predicate carries its optional evidence_ids");
      expectFailure(() => decodePayload(quantitative.replace('observable = "fixture"\n', ""), "matrix"), ["E-SCHEMA-MISSING-KEY"], "p.promotion_trigger.observable");
      return;
    }
    case "domain_defect_regression_required": expectFailure(() => decodePayload(READY.replace('work_kind = "feature"', 'work_kind = "defect"'), "matrix"), ["E-SCHEMA-STATE"]); return;
    case "domain_missing_system_admission_required": expectFailure(() => decodePayload(READY.replace('work_kind = "feature"', 'work_kind = "missing_system"'), "testing"), ["E-SCHEMA-STATE"]); return;
    case "schema_held_permanent_rejected": expectFailure(() => decodePayload('kind = "decision"\ndecision_state = "held"\nrationale_md = """R."""\npermanence = "permanent"\n\n[p.reopening_signal]\nobserver = "operator"\ndimension = "count"\naction_on_fire_md = """Act."""\nevaluation = "unknown"\n\n[p.reopening_signal.predicate]\npredicate_kind = "event"\nevent_md = """Event."""\n', "matrix"), ["E-SCHEMA-ENUM"]); return;
    case "domain_quantitative_scope_unit_required": expectFailure(() => decodePayload(predicateTransition('predicate_kind = "quantitative"\ncomparator = "ge"\nthreshold = 2\nmeasurement = 1\nas_of = "2026-08-11"\n'), "matrix"), ["E-SCHEMA-MISSING-KEY"]); return;
    case "domain_manual_not_auto_boolean": {
      const decoded = decodePayload(predicateTransition('predicate_kind = "manual"\nreview_procedure_md = """Review."""\nevidence_ids = ["matrix.fixture-evidence"]\n'), "matrix");
      assert(decoded !== undefined, "manual predicate remains authored"); return;
    }
    case "domain_state_required_forbidden": {
      expectFailure(() => decodePayload(`${READY}blocker_md = """No."""\n`, "matrix"), ["E-SCHEMA-FORBIDDEN-KEY"]);
      expectFailure(() => decodePayload(READY.replace('acceptance_md = """Accepted."""', 'acceptance_md = """"""'), "matrix"), ["E-SCHEMA-FLOOR"], "p.acceptance_md");
      expectFailure(() => decodePayload(READY.replace('priority_rationale_md = """Normal."""', 'priority_rationale_md = """"""'), "matrix"), ["E-SCHEMA-FLOOR"], "p.priority_rationale_md");
      return;
    }
    case "domain_fired_transition_not_parked": {
      // Deferred work's reopening signal is nested (Phase 4 fold); a deferred record whose nested
      // signal already reads "met" is parking a fired transition.
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/matrix-v3.toml");
      const mutated = replaceAfter(source, 'id = "matrix.fixture-task-g"', 'evaluation = "unmet"', 'evaluation = "met"');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<fired-transition>", "matrix"), ["E-SCHEMA-STATE"], "record.matrix.fixture-task-g.reopening_signal");
      return;
    }
    case "domain_already_met_transition_rejected": {
      // The nested form of the same rule: an armed work's nested promotion trigger whose
      // evaluation already reads "met" cannot be parked.
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/matrix-v3.toml");
      const mutated = replaceAfter(source, "[record.payload.promotion_trigger]", 'evaluation = "unknown"', 'evaluation = "met"');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<already-met-transition>", "matrix"), ["E-SCHEMA-STATE"], "record.matrix.fixture-task-f.promotion_trigger");
      return;
    }
    case "domain_stale_unknown_visible": {
      const decoded = decodePayload('kind = "evidence"\nevidence_kind = "source_read"\nclaim_md = """Claim."""\nevidence_verdict = "unknown"\nfreshness = "stale"\nreference_ids = ["source"]\nobserved_at = "2026-08-11"\nenvironment_md = """Env."""\nunprobed_remainder_md = """Remainder."""\n\n[p.scope]\nsurfaces = ["fixture"]\n', "matrix");
      assert(decoded !== undefined, "stale/unknown is visible semantic state"); return;
    }
    case "evidence_point_requires_provenance": expectFailure(() => decodePayload('kind = "evidence"\nevidence_kind = "source_read"\nclaim_md = """Claim."""\nevidence_verdict = "confirmed"\nfreshness = "as_of"\nreference_ids = ["source"]\nobserved_at = "2026-08-11"\nunprobed_remainder_md = """Remainder."""\n\n[p.scope]\nsurfaces = ["fixture"]\n', "matrix"), ["E-SCHEMA-STATE"]); return;
    case "evidence_negative_requires_enumeration": expectFailure(() => decodePayload('kind = "evidence"\nevidence_kind = "registry_enumeration"\nclaim_md = """None found."""\nevidence_verdict = "confirmed"\nfreshness = "live"\nreference_ids = ["registry"]\nunprobed_remainder_md = """None."""\nrefresh_reference_id = "refresh"\n\n[p.scope]\nsurfaces = ["fixture"]\n', "matrix"), ["E-SCHEMA-STATE"]); return;
    case "evidence_generator_requires_harness_free": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/matrix-v3.toml");
      const mutated = replaceAfter(source, 'id = "matrix.fixture-task-a"', 'evidence_ids = ["matrix.fixture-evidence-a", "matrix.fixture-evidence-c"]', 'evidence_ids = ["matrix.fixture-evidence-a"]');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<generator-evidence>", "matrix"), ["E-SCHEMA-STATE"], "record.matrix.fixture-task-a.evidence_ids");
      return;
    }
    case "evidence_timing_join_structural": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/testing-v3.toml");
      const mutated = replaceAfter(source, 'id = "testing.fixture-cost-historical"', 'evidence_ids = ["testing.fixture-evidence-gate"]', 'evidence_ids = ["testing.fixture-cost-historical"]');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<timing-evidence>", "testing"), ["E-SCHEMA-STATE"], "record.testing.fixture-cost-historical.evidence_ids");
      return;
    }
    case "evidence_draft_log_rejected": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/matrix-v3.toml");
      const mutated = replaceAfter(source, 'id = "ref-file"', 'path = "cddl-matrix/README.md"', 'path = "draft/logs/check-local.log"');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<draft-evidence>", "matrix"), ["E-REFERENCE-FORBIDDEN"], "reference[4].path");
      return;
    }
    case "schema_observed_at_civil_date": expectFailure(() => decodePayload('kind = "testing_cost"\ncost_posture = "historical_observation"\nunit = "ms"\nscope_md = """Scope."""\nvalue_min = 1\nvalue_max = 2\nobserved_at = "2025-02-29"\nenvironment_md = """Env."""\nevidence_ids = ["testing.fixture-evidence"]\n', "testing"), ["E-SCHEMA-TYPE"]); return;
    case "schema_duplicate_assignment_rejected": duplicateParseRejected(new TextDecoder().decode(minimalRoadmap()).replace("schema_version = 3", "schema_version = 3\nschema_version = 3")); return;
    case "schema_duplicate_table_rejected": duplicateParseRejected(new TextDecoder().decode(minimalRoadmap()).replace("\n[[section]]", "\n[document]\n\n[[section]]")); return;
    case "schema_duplicate_nested_payload_rejected":
      duplicateParseRejected(`${new TextDecoder().decode(minimalRoadmap())}\n[record.payload]\nkind = "work"\n`); return;
    case "schema_exact_keys_every_structural_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), [...ROADMAP_SCHEMA_ROWS]);
      canonicalRoadmapFixture(context, "all-fields/matrix-v3.toml", "matrix");
      canonicalRoadmapFixture(context, "all-fields/testing-v3.toml", "testing");
      return;
    }
    case "schema_shared_payload_exact_keys_every_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), SHARED_SEMANTIC_SCHEMA_ROWS);
      canonicalRoadmapFixture(context, "all-fields/matrix-v3.toml", "matrix");
      return;
    }
    case "schema_matrix_payload_exact_keys_every_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), MATRIX_SCHEMA_ROWS);
      const source = fixtureText(context, "all-fields/matrix-v3.toml");
      canonicalRoadmapFixture(context, "all-fields/matrix-v3.toml", "matrix");
      const action = replaceAfter(source, 'id = "matrix.fixture-upstream-a"', 'action_id = "action-a-one"', 'action_id = "action-1"');
      expectFailure(() => decodeRoadmapSource(text(action), "<action-slug>", "matrix"), ["E-ID-GRAMMAR"]);
      const branch = replaceAfter(source, 'id = "matrix.fixture-upstream-a"', 'branch_id = "branch-a-one"', 'branch_id = "branch-1"');
      expectFailure(() => decodeRoadmapSource(text(branch), "<branch-slug>", "matrix"), ["E-ID-GRAMMAR"]);
      return;
    }
    case "schema_testing_payload_exact_keys_every_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), TESTING_SCHEMA_ROWS);
      const source = fixtureText(context, "all-fields/testing-v3.toml");
      canonicalRoadmapFixture(context, "all-fields/testing-v3.toml", "testing");
      const capture = replaceAfter(source, 'id = "testing.fixture-operational-attributed"', 'step_id = "capture"', 'step_id = "capture-1"');
      expectFailure(() => decodeRoadmapSource(text(capture), "<capture-slug>", "testing"), ["E-ID-GRAMMAR"]);
      return;
    }
    case "strict_unknown_reference": {
      const base = new TextDecoder().decode(minimalRoadmap());
      expectFailure(() => decodeRoadmapSource(text(`${base}\n[[reference]]\nid = "fixture"\nsource = "matrix.fixture-minimal"\nkind = "gate"\ngate_id = "fixture"\nunknown = 1\n`), "<unknown-reference>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"], "reference[0].unknown");
      expectFailure(() => decodeRoadmapSource(text(`${base}\n[[reference]]\nid = "fixture-1"\nsource = "matrix.fixture-minimal"\nkind = "gate"\ngate_id = "fixture"\n`), "<reference-id-grammar>", "matrix"), ["E-ID-GRAMMAR"], "reference[0].id");
      return;
    }
    case "schema_reference_exact_keys_every_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), [ROADMAP_ROW.reference_discriminator, ...REFERENCE_SCHEMA_ROWS]);
      canonicalRoadmapFixture(context, "all-fields/matrix-v3.toml", "matrix");
      return;
    }
    case "schema_due_on_valid_through_postures": {
      expectFailure(() => decodePayload('kind = "work"\nwork_state = "blocked"\nwork_intent = "repair"\nwork_kind = "feature"\nrisk = "cosmetic"\nblocker_md = """No."""\n\n[p.unblock_predicate]\nowner_reference_id = "owner"\nevent_md = """Event."""\ncheck_procedure_md = """Check."""\ndue_action_md = """Act."""\ndue_on = "2026-08-11"\nevaluation = "unknown"\n', "matrix"), ["E-SCHEMA-UNKNOWN-KEY"]);
      expectFailure(() => decodePayload('kind = "evidence"\nevidence_kind = "source_read"\nclaim_md = """Claim."""\nevidence_verdict = "confirmed"\nfreshness = "historical"\nreference_ids = ["source"]\nobserved_at = "2026-08-11"\nvalid_through = "2026-08-12"\nenvironment_md = """Env."""\nunprobed_remainder_md = """None."""\n\n[p.scope]\nsurfaces = ["fixture"]\n', "matrix"), ["E-SCHEMA-FORBIDDEN-KEY", "E-SCHEMA-STATE"]);
      return;
    }
    case "schema_canonical_key_order_every_arm": {
      assert(new TextDecoder().decode(composeRoadmapDocument(decodeRoadmapSource(minimalRoadmap(), "<order>", "matrix"))) === new TextDecoder().decode(minimalRoadmap()), "canonical key order");
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), ALL_SCHEMA_ROWS);
      for (const [path, roadmap] of [
        ["positive/small-matrix-v3.toml", "matrix"], ["positive/small-testing-v3.toml", "testing"],
        ["all-fields/matrix-v3.toml", "matrix"], ["all-fields/testing-v3.toml", "testing"],
      ] as const) canonicalRoadmapFixture(context, path, roadmap);
      const matrix = composeRoadmapDocument(decodeRoadmapSource(readFixture(context, "all-fields/matrix-v3.toml"), "<work-order>", "matrix", false));
      const matrixText = new TextDecoder().decode(matrix);
      const workStart = matrixText.indexOf('[[record]]\nid = "matrix.fixture-task-a"');
      const workEnd = matrixText.indexOf("\n[[record]]", workStart + 1);
      assert(workStart !== -1 && workEnd !== -1, "canonical ready-work block");
      assertNeedlesInOrder(matrixText.slice(workStart, workEnd), [
        "evidence_ids = ",
        "acceptance_md = ",
        "priority_band = ",
        "priority_rationale_md = ",
        "control_ids = ",
        "regression_evidence_ids = ",
        "regression_gap_ids = ",
      ], "ready-work key universe");
      return;
    }
    case "domain_transition_each_kind": {
      assert(context !== undefined, `${id} requires fixture ports`);
      allFieldsCoverage(context);
      return;
    }
    default: throw new Error(`unhandled schema self-test ${id}`);
  }
}

function failure(id: string, error: unknown): RoadmapIssue {
  if (error instanceof RoadmapWireError) return error.issue;
  return { code: "E-SELFTEST-CASE", source: "<selftest>", logical_path: id, message: error instanceof Error ? error.message : String(error), exit: 1 };
}

interface SchemaCaseSpec {
  readonly category: SelfTestCategory;
  readonly polarity: "positive" | "negative";
  readonly subcases?: readonly string[];
}

/**
 * One row per registered case: the category it counts toward, its declared polarity, and the exact
 * subcase labels it reports.  The mapped key type keeps the table total over the frozen ID
 * inventory, so a renamed case is a typecheck failure rather than a substring match silently
 * re-filing it under another category.
 */
const SCHEMA_CASES: { readonly [K in RequiredSchemaSelfTestCaseId]: SchemaCaseSpec } = {
  strict_unknown_top: { category: "schema", polarity: "negative" },
  strict_unknown_nested_record: { category: "schema", polarity: "negative" },
  strict_unknown_reference: { category: "schema", polarity: "negative" },
  strict_unknown_every_table: { category: "schema", polarity: "negative" },
  strict_unknown_kind: { category: "schema", polarity: "negative" },
  strict_unknown_enum: { category: "schema", polarity: "negative" },
  strict_enum_every_field: { category: "schema", polarity: "negative" },
  strict_missing_discriminator: { category: "schema", polarity: "negative" },
  strict_generic_state_rejected: { category: "schema", polarity: "negative" },
  strict_generic_disposition_rejected: { category: "schema", polarity: "negative" },
  missing_section_entries: { category: "schema", polarity: "negative" },
  empty_records_floor: { category: "schema", polarity: "negative" },
  all_fields_identity: { category: "schema", polarity: "positive" },
  v3_semantic_identity: { category: "schema", polarity: "positive" },
  v2_unsupported: { category: "schema", polarity: "negative" },
  v3_retired_keys_rejected: { category: "schema", polarity: "negative" },
  transition_observable_arm_dependent: { category: "schema", polarity: "negative" },
  noncanonical_basic_string: { category: "schema", polarity: "negative" },
  noncanonical_table_order: { category: "schema", polarity: "negative" },
  noncanonical_set_order: { category: "schema", polarity: "negative" },
  toml_terminal_newline: { category: "schema", polarity: "positive" },
  domain_matrix_all_tags: { category: "domain-matrix", polarity: "positive" },
  domain_testing_all_tags: { category: "domain-testing", polarity: "positive" },
  domain_state_required_forbidden: { category: "domain-matrix", polarity: "negative" },
  domain_defect_regression_required: { category: "domain-testing", polarity: "negative" },
  domain_missing_system_admission_required: { category: "schema", polarity: "negative" },
  domain_transition_each_kind: { category: "schema", polarity: "positive" },
  domain_quantitative_scope_unit_required: { category: "schema", polarity: "negative" },
  domain_manual_not_auto_boolean: { category: "schema", polarity: "positive" },
  domain_fired_transition_not_parked: { category: "schema", polarity: "negative" },
  domain_already_met_transition_rejected: { category: "schema", polarity: "negative" },
  domain_stale_unknown_visible: { category: "schema", polarity: "positive" },
  evidence_point_requires_provenance: { category: "schema", polarity: "negative" },
  evidence_negative_requires_enumeration: { category: "schema", polarity: "negative" },
  evidence_generator_requires_harness_free: { category: "schema", polarity: "negative" },
  evidence_timing_join_structural: { category: "schema", polarity: "negative" },
  evidence_draft_log_rejected: { category: "schema", polarity: "negative" },
  schema_exact_keys_every_structural_arm: { category: "schema", polarity: "positive" },
  schema_shared_payload_exact_keys_every_arm: { category: "schema", polarity: "positive" },
  schema_matrix_payload_exact_keys_every_arm: { category: "domain-matrix", polarity: "positive" },
  schema_testing_payload_exact_keys_every_arm: { category: "domain-testing", polarity: "positive" },
  schema_reference_exact_keys_every_arm: { category: "schema", polarity: "positive" },
  schema_canonical_key_order_every_arm: { category: "schema", polarity: "positive" },
  schema_duplicate_assignment_rejected: { category: "schema", polarity: "negative" },
  schema_duplicate_table_rejected: { category: "schema", polarity: "negative" },
  schema_duplicate_nested_payload_rejected: { category: "schema", polarity: "negative" },
  noncanonical_comment: { category: "schema", polarity: "negative" },
  noncanonical_inline_table: { category: "schema", polarity: "negative" },
  schema_priority_band_closed_enum: { category: "schema", polarity: "positive" },
  schema_observed_at_civil_date: { category: "schema", polarity: "negative" },
  schema_held_permanent_rejected: { category: "schema", polarity: "negative" },
  schema_due_on_valid_through_postures: { category: "schema", polarity: "negative" },
};

export const SCHEMA_SELFTEST_CASES: readonly SelfTestCase[] = REQUIRED_SCHEMA_SELFTEST_CASE_IDS.map((id) => {
  const spec = SCHEMA_CASES[id];
  return {
    id,
    category: spec.category,
    run(context): SelfTestResult {
      try {
        execute(id, context);
        return {
          ok: true,
          polarity: spec.polarity,
          ...(spec.subcases === undefined ? {} : { subcases: spec.subcases }),
        };
      }
      catch (error) { return { ok: false, polarity: spec.polarity, issues: [failure(id, error)] }; }
    },
  };
});

const FIXTURE_REQUIRED_SCHEMA_CASE_IDS = new Set<RequiredSchemaSelfTestCaseId>([
  "strict_unknown_every_table",
  "all_fields_identity",
  "domain_matrix_all_tags",
  "domain_testing_all_tags",
  "domain_transition_each_kind",
  "domain_fired_transition_not_parked",
  "domain_already_met_transition_rejected",
  "evidence_generator_requires_harness_free",
  "evidence_timing_join_structural",
  "evidence_draft_log_rejected",
  "schema_exact_keys_every_structural_arm",
  "schema_shared_payload_exact_keys_every_arm",
  "schema_matrix_payload_exact_keys_every_arm",
  "schema_testing_payload_exact_keys_every_arm",
  "schema_reference_exact_keys_every_arm",
  "schema_canonical_key_order_every_arm",
  "schema_priority_band_closed_enum",
]);

