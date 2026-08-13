import { RoadmapWireError, bytesEqual, encodeMarkdownString } from "../markdown_codec.ts";
import type { RoadmapIssue } from "../errors.ts";
import type { RepoPath, RoadmapName } from "../model/core.ts";
import type { RoadmapDocumentV1, RoadmapDocumentV2 } from "../model/documents.ts";
import type { SelfTestCandidateCase as SelfTestCase, SelfTestContext, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import { compareAllFieldsCoverageTags } from "./fixtures.ts";
import { observeSelfTestIssue } from "./observations.ts";
import { composeCampaignDocument, composeRetiredIdsDocument, composeRoadmapDocument } from "../compose.ts";
import { decodeCampaignSource, CAMPAIGN_ENUM_FIELDS, CAMPAIGN_SCHEMA_ROWS } from "../decode/campaign.ts";
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
  MANIFEST_SCHEMA_ROWS,
  REFERENCE_SCHEMA_ROWS,
  ROADMAP_ENUM_FIELDS,
  ROADMAP_SCHEMA_ROWS,
  ROADMAP_V2_SCHEMA_ROWS,
} from "../decode/roadmap.ts";
import {
  decodeSharedSemanticPayload,
  SEMANTIC_ENUM_FIELDS,
  SEMANTIC_V2_SCHEMA_ROWS,
  SHARED_SEMANTIC_SCHEMA_ROWS,
} from "../decode/semantic.ts";
import { decodeTestingPayload, TESTING_ENUM_FIELDS, TESTING_SCHEMA_ROWS } from "../decode/testing.ts";
import { decodeRetiredSource, RETIRED_ENUM_FIELDS, RETIRED_SCHEMA_ROWS } from "../decode/retired.ts";
import { childLogicalPath, shieldTomlMarkdown } from "../decode/raw_markdown.ts";
import { syntheticClosedDenominatorV2Source } from "./denominator.ts";
import {
  semanticConversionState,
  validateSemanticConversionDeclaration,
} from "../semantic_conversion.ts";

const UTF8 = new TextEncoder();
const text = (value: string): Uint8Array => UTF8.encode(value);
const ZERO_HASH = "0".repeat(64);

export const REQUIRED_SCHEMA_SELFTEST_CASE_IDS = [
  "strict_unknown_top", "strict_unknown_nested_record", "strict_unknown_reference", "strict_unknown_campaign", "strict_unknown_retired", "strict_unknown_every_table", "strict_unknown_kind", "strict_unknown_enum", "strict_enum_every_field", "strict_missing_discriminator", "strict_generic_state_rejected", "strict_generic_disposition_rejected", "v0_missing_manifest", "v0_empty_records_floor", "truncated_span_read", "campaign_missing_root", "campaign_empty_valid", "campaign_impossible_authority_tuple_rejected", "retired_missing_root", "retired_empty_valid", "v0_all_fields_identity", "v1_all_fields_identity", "v2_semantic_identity", "v2_migration_escape_hatches_rejected", "v2_intrinsic_completion", "v2_unresolved_migration_reference_rejected", "v3_unsupported", "campaign_all_fields_identity", "retired_all_fields_identity", "noncanonical_literal_string", "noncanonical_table_order", "noncanonical_set_order", "toml_terminal_newline", "v0_rejects_semantics", "v0_rejects_authoritative", "v1_raw_requires_frozen_span", "v1_new_raw_rejected", "v1_raw_shadow_nonrendering", "v1_semantic_forbids_raw", "schema_lifecycle_raw_omission_historical", "schema_lifecycle_raw_review_arms", "schema_lifecycle_semantic_requires_reviewed", "schema_lifecycle_cross_kind_rejected", "schema_lifecycle_v0_forbidden", "schema_projection_visibility_document_arm", "schema_projection_visibility_semantic_only_arm", "schema_projection_visibility_forbidden_nonsemantic_arms", "domain_matrix_all_tags", "domain_testing_all_tags", "domain_state_required_forbidden", "domain_defect_regression_required", "domain_missing_system_admission_required", "domain_transition_each_kind", "domain_quantitative_scope_unit_required", "domain_manual_not_auto_boolean", "domain_fired_transition_not_parked", "domain_already_met_signal_rejected", "domain_stale_unknown_visible", "evidence_point_requires_provenance", "evidence_negative_requires_enumeration", "evidence_generator_requires_harness_free", "evidence_timing_join_structural", "evidence_draft_log_rejected", "domain_closed_denominator_rejected", "schema_v0_exact_keys_every_table", "schema_v1_exact_keys_every_structural_arm", "schema_shared_payload_exact_keys_every_arm", "schema_matrix_payload_exact_keys_every_arm", "schema_testing_payload_exact_keys_every_arm", "schema_systematic_exact_keys_every_arm", "schema_reference_exact_keys_every_arm", "schema_campaign_retired_exact_keys_every_arm", "schema_canonical_key_order_every_arm", "schema_duplicate_assignment_rejected", "schema_duplicate_table_rejected", "schema_duplicate_nested_payload_rejected", "schema_campaign_authority_keys", "schema_campaign_reservation_exact_keys", "schema_campaign_selection_binding_keys_forbidden", "noncanonical_comment", "noncanonical_inline_table", "systematic_illegal_cell_rejected", "systematic_illegal_coordinate_is_exclusion", "systematic_unmodelled_coordinate_not_cell", "campaign_inline_legacy_binding_rejected", "schema_priority_band_closed_enum", "schema_campaign_slug_grammars", "schema_observed_at_civil_date", "schema_held_permanent_rejected", "schema_due_on_valid_through_postures",
] as const;

export type RequiredSchemaSelfTestCaseId = (typeof REQUIRED_SCHEMA_SELFTEST_CASE_IDS)[number];

const ALL_SCHEMA_ROWS = [
  ...ROADMAP_SCHEMA_ROWS,
  ...ROADMAP_V2_SCHEMA_ROWS,
  ...MANIFEST_SCHEMA_ROWS,
  ...REFERENCE_SCHEMA_ROWS,
  ...SHARED_SEMANTIC_SCHEMA_ROWS,
  ...SEMANTIC_V2_SCHEMA_ROWS,
  ...MATRIX_SCHEMA_ROWS,
  ...TESTING_SCHEMA_ROWS,
  ...CAMPAIGN_SCHEMA_ROWS,
  ...RETIRED_SCHEMA_ROWS,
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

function minimalRoadmap(version: 0 | 1, roadmap: RoadmapName = "matrix"): Uint8Array {
  const id = `${roadmap}.fixture-minimal`;
  const authority = version === 0 ? "shadow" : "authoritative";
  const frozen = version === 1 ? 'frozen_legacy_span_ids = ["record", "section"]\n' : "";
  const render = version === 1 ? 'render_authority = "raw"\n' : "";
  return text(`[document]\nschema_version = ${version}\nauthority = "${authority}"\nroadmap = "${roadmap}"\nsource_path = "fixture/${roadmap}.toml"\nprojection_path = "fixture/${roadmap}.md"\nfrozen_source_sha256 = "${ZERO_HASH}"\nfrozen_source_byte_length = 2\nfrozen_source_line_count = 1\nfrozen_source_eof = "lf"\n${frozen}\n[[section]]\nsection_id = "fixture"\ntitle = "Fixture"\n${render}source_block_md = """S\n"""\nspan_ids = ["section"]\n\n[[record]]\nid = "${id}"\ntitle = "Fixture record"\nprojection_group = "fixture"\n${render}source_block_md = """R\n"""\nspan_ids = ["record"]\n\n[[manifest.entry]]\nkind = "section"\nsection_id = "fixture"\n\n[[manifest.entry]]\nkind = "record"\nrecord_id = "${id}"\n\n[[source_span]]\nid = "section"\nstart_byte = 0\nend_byte = 1\nsha256 = "${ZERO_HASH}"\nsource_kind = "section"\nowner_id = "fixture"\nowner_field = "source_block_md"\nmigration_status = "raw"\n\n[[source_span]]\nid = "record"\nstart_byte = 1\nend_byte = 2\nsha256 = "${ZERO_HASH}"\nsource_kind = "record"\nowner_id = "${id}"\nowner_field = "source_block_md"\nmigration_status = "raw"\n`);
}

function minimalV2Roadmap(): Uint8Array {
  return text(`[document]
schema_version = 2
authority = "authoritative"
roadmap = "matrix"
source_path = "fixture/matrix.toml"
projection_path = "fixture/matrix.md"
frozen_source_sha256 = "${ZERO_HASH}"
frozen_source_byte_length = 2
frozen_source_line_count = 1
frozen_source_eof = "lf"
projection_layout = "legacy_v1"

[[section]]
section_id = "fixture"
title = "Fixture"
render_authority = "semantic"
body_md = """S
"""

[[section.source_replacement]]
span_id = "section"
replacement_field = "body_md"
review_note_md = """Reviewed section.
"""

[[record]]
id = "matrix.fixture-minimal"
title = "Fixture record"
projection_group = "fixture"
render_authority = "semantic"
projection_visibility = "document"

[record.payload]
kind = "work"
summary_md = """R
"""
work_state = "ready"
work_intent = "build_capability"
work_kind = "feature"
risk = "cosmetic"
family_classification = "none_reviewed"
acceptance_md = """Accepted.
"""
priority_rationale_md = """Normal.
"""

[[record.source_replacement]]
span_id = "record"
replacement_field = "payload.summary_md"
review_note_md = """Reviewed record.
"""

[[manifest.entry]]
kind = "section"
section_id = "fixture"

[[manifest.entry]]
kind = "record"
record_id = "matrix.fixture-minimal"

[[source_span]]
id = "section"
start_byte = 0
end_byte = 1
sha256 = "${ZERO_HASH}"
source_kind = "section"
owner_id = "fixture"
owner_field = "body_md"
migration_status = "replaced"

[[source_span]]
id = "record"
start_byte = 1
end_byte = 2
sha256 = "${ZERO_HASH}"
source_kind = "record"
owner_id = "matrix.fixture-minimal"
owner_field = "payload.summary_md"
migration_status = "replaced"
`);
}

function minimalV2Document(): RoadmapDocumentV2 {
  const decoded = decodeRoadmapSource(minimalV2Roadmap(), "<v2>", "matrix");
  assert(decoded.document.schema_version === 2, "minimal v2 fixture did not decode as v2");
  return decoded as RoadmapDocumentV2;
}

function semanticRecordRoadmap(visibility: "document" | "semantic_only"): string {
  const source = new TextDecoder().decode(minimalRoadmap(1));
  const record = `[[record]]\nid = "matrix.fixture-minimal"\ntitle = "Fixture record"\nprojection_group = "fixture"\nrender_authority = "raw"\nsource_block_md = """R\n"""\nspan_ids = ["record"]`;
  const semantic = `[[record]]\nid = "matrix.fixture-minimal"\ntitle = "Fixture record"\nprojection_group = "fixture"\nrender_authority = "semantic"\nprojection_visibility = "${visibility}"\n\n[record.payload]\nkind = "work"\nsummary_md = """R\n"""\nwork_state = "ready"\nwork_intent = "build_capability"\nwork_kind = "feature"\nrisk = "cosmetic"\nfamily_classification = "none_reviewed"\nacceptance_md = """Accepted.\n"""\npriority_rationale_md = """Normal.\n"""${visibility === "document" ? `\n\n[[record.source_replacement]]\nspan_id = "record"\nreplacement_field = "payload.summary_md"\nreview_note_md = """Reviewed.\n"""` : ""}`;
  const converted = source.replace(record, semantic)
    .replace('frozen_legacy_span_ids = ["record", "section"]', 'frozen_legacy_span_ids = ["section"]')
    .replace('owner_field = "source_block_md"\nmigration_status = "raw"\n', 'owner_field = "source_block_md"\nmigration_status = "raw"\n')
    .replace('owner_id = "matrix.fixture-minimal"\nowner_field = "source_block_md"\nmigration_status = "raw"', `owner_id = "matrix.fixture-minimal"\nowner_field = "payload.summary_md"\nmigration_status = "replaced"`);
  return visibility === "semantic_only"
    ? converted
      .replace('\n\n[[manifest.entry]]\nkind = "record"\nrecord_id = "matrix.fixture-minimal"', "")
      .replace(/\n\n\[\[source_span\]\]\nid = "record"[\s\S]*$/u, "\n")
    : converted;
}

function subordinateRoadmap(
  version: 0 | 1,
  fragment: "raw_omitted" | "raw_pending" | "raw_reviewed" | "raw_independent" | "semantic_reviewed",
  part: "raw_omitted" | "raw_pending" | "raw_reviewed" | "raw_independent" | "semantic_reviewed",
): string {
  const authority = version === 0 ? "shadow" : "authoritative";
  const rawFragment = fragment !== "semantic_reviewed";
  const rawPart = part !== "semantic_reviewed";
  const disposition = (kind: "fragment" | "part", arm: typeof fragment | typeof part): string => {
    if (version === 0 || arm === "raw_omitted") return "";
    if (arm === "raw_pending") return 'lifecycle_disposition = "pending_review"\n';
    if (arm === "raw_independent") return 'lifecycle_disposition = "independent_record"\n';
    return `lifecycle_disposition = "${kind === "fragment" ? "document_prose" : "parent_supporting_prose"}"\n`;
  };
  const frozen = version === 1
    ? `frozen_legacy_span_ids = [${[
      rawFragment ? '"fragment"' : undefined,
      rawPart ? '"part"' : undefined,
      '"record"',
      '"section"',
    ].filter((value) => value !== undefined).join(", ")}]\n`
    : "";
  const render = (semantic: boolean): string => version === 1
    ? `render_authority = "${semantic ? "semantic" : "raw"}"\n`
    : "";
  const fragmentBody = rawFragment
    ? `source_block_md = """F\n"""\nspan_ids = ["fragment"]`
    : `body_md = """F\n"""\n\n[[fragment.source_replacement]]\nspan_id = "fragment"\nreplacement_field = "body_md"\nreview_note_md = """Reviewed fragment.\n"""`;
  const partBody = rawPart
    ? `source_block_md = """P\n"""\nspan_ids = ["part"]`
    : `body_md = """P\n"""\n\n[[part.source_replacement]]\nspan_id = "part"\nreplacement_field = "body_md"\nreview_note_md = """Reviewed part.\n"""`;
  return `[document]\nschema_version = ${version}\nauthority = "${authority}"\nroadmap = "matrix"\nsource_path = "fixture/subordinate.toml"\nprojection_path = "fixture/subordinate.md"\nfrozen_source_sha256 = "${ZERO_HASH}"\nfrozen_source_byte_length = 4\nfrozen_source_line_count = 1\nfrozen_source_eof = "none"\n${frozen}\n[[section]]\nsection_id = "fixture"\ntitle = "Fixture"\n${render(false)}source_block_md = """S\n"""\nspan_ids = ["section"]\n\n[[fragment]]\nfragment_id = "fragment"\nprojection_group = "fixture"\n${render(!rawFragment)}${disposition("fragment", fragment)}${fragmentBody}\n\n[[record]]\nid = "matrix.fixture-record"\ntitle = "Record"\nprojection_group = "fixture"\n${render(false)}source_block_md = """R\n"""\nspan_ids = ["record"]\n\n[[part]]\npart_id = "part"\nparent_record_id = "matrix.fixture-record"\n${render(!rawPart)}${disposition("part", part)}${partBody}\n\n[[manifest.entry]]\nkind = "section"\nsection_id = "fixture"\n\n[[manifest.entry]]\nkind = "fragment"\nfragment_id = "fragment"\n\n[[manifest.entry]]\nkind = "record"\nrecord_id = "matrix.fixture-record"\n\n[[manifest.entry]]\nkind = "part"\npart_id = "part"\n\n[[source_span]]\nid = "section"\nstart_byte = 0\nend_byte = 1\nsha256 = "${ZERO_HASH}"\nsource_kind = "section"\nowner_id = "fixture"\nowner_field = "source_block_md"\nmigration_status = "raw"\n\n[[source_span]]\nid = "fragment"\nstart_byte = 1\nend_byte = 2\nsha256 = "${ZERO_HASH}"\nsource_kind = "fragment"\nowner_id = "fragment"\nowner_field = "${rawFragment ? "source_block_md" : "body_md"}"\nmigration_status = "${rawFragment ? "raw" : "replaced"}"\n\n[[source_span]]\nid = "record"\nstart_byte = 2\nend_byte = 3\nsha256 = "${ZERO_HASH}"\nsource_kind = "record"\nowner_id = "matrix.fixture-record"\nowner_field = "source_block_md"\nmigration_status = "raw"\n\n[[source_span]]\nid = "part"\nstart_byte = 3\nend_byte = 4\nsha256 = "${ZERO_HASH}"\nsource_kind = "part"\nowner_id = "part"\nowner_field = "${rawPart ? "source_block_md" : "body_md"}"\nmigration_status = "${rawPart ? "raw" : "replaced"}\"\n`;
}

function emptyCampaign(): Uint8Array {
  return text('[campaign]\nschema_version = 1\nmatrix_authority = "legacy_markdown"\ntesting_authority = "legacy_markdown"\n');
}

function emptyRetired(): Uint8Array {
  return text("[retired_ids]\nschema_version = 1\n");
}

function decodePayload(body: string, roadmap: RoadmapName, position: "shadow" | "authority" = "shadow"): unknown {
  const bindings = shieldTomlMarkdown(text(`[p]\n${body}`), "<payload>");
  const ctx: DecodeContext = { source: bindings.source, bindings };
  const root = expectExactTable(ctx, bindings.parsed, "$", { name: "payload root", required: ["p"] });
  const raw = requiredValue(root, "p");
  const pre = expectExactTable(ctx, raw, "p", { name: "payload kind", required: ["kind"], optional: raw !== null && typeof raw === "object" && !Array.isArray(raw) ? Object.keys(raw).filter((key) => key !== "kind") : [] });
  const kind = requiredValue(pre, "kind");
  assert(typeof kind === "string", "payload kind string");
  const shared = decodeSharedSemanticPayload(ctx, raw, "p", position);
  const decoded = shared ?? (roadmap === "matrix" ? decodeMatrixPayload(ctx, raw, "p", kind) : decodeTestingPayload(ctx, raw, "p", kind));
  if (decoded === undefined) schemaFail(ctx, "E-SCHEMA-ENUM", "p.kind", `unknown ${roadmap} payload kind`);
  bindings.assertAllConsumed();
  return decoded;
}

const READY = `kind = "work"\nsummary_md = """Ready."""\nwork_state = "ready"\nwork_intent = "build_capability"\nwork_kind = "feature"\nrisk = "cosmetic"\nfamily_classification = "none_reviewed"\nacceptance_md = """Accepted."""\npriority_band = "normal"\npriority_rationale_md = """Normal."""\n`;
const OBSERVED_FAMILY = `kind = "family"\nsummary_md = """Family."""\nfamily_maturity = "observed_only"\ncampaign_state = "designing"\ngoal_md = """Goal."""\nboundary_md = """Boundary."""\nwork_ids = []\nobservation_reference_ids = ["observation"]\naffected_profiles = ["default"]\naffected_faces = ["rust"]\ncontrol_ids = []\ncompletion_owner_reference_id = "completion"\nretirement_owner_reference_id = "retirement"\n`;

function predicateSignal(predicate: string, predicateKind: "quantitative" | "manual" = "quantitative"): string {
  return `kind = "signal"\nsummary_md = """Signal."""\ntransition_kind = "promotion_trigger"\nobserver = "operator"\ndimension = "count"\nobservable = "fixture"\npredicate_kind = "${predicateKind}"\ncurrent_evidence_ids = []\naction_on_fire_md = """Act."""\nevaluation = "unknown"\n\n[p.predicate]\n${predicate}`;
}

const FIXTURE_ROOT = "cddl-matrix/roadmap/fixtures" as RepoPath;
function readFixture(context: SelfTestContext, relative: string): Uint8Array {
  const inventory = context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
  const path = inventory.find((entry) => entry === relative);
  assert(path !== undefined, `fixture inventory contains ${relative}`);
  return context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, path);
}

function roadmapFixture(context: SelfTestContext, relative: string, roadmap: RoadmapName): RoadmapDocumentV1 {
  const doc = decodeRoadmapSource(readFixture(context, relative), relative, roadmap);
  assert(doc.document.schema_version === 1, `${relative} is v1`);
  return doc as RoadmapDocumentV1;
}

function fixtureIdentity(context: SelfTestContext): void {
  const paths: readonly [string, RoadmapName][] = [
    ["positive/minimal-matrix-v0.toml", "matrix"], ["positive/minimal-testing-v0.toml", "testing"],
    ["positive/mixed-matrix-v1.toml", "matrix"], ["positive/mixed-testing-v1.toml", "testing"],
    ["irregular/matrix-v0.toml", "matrix"], ["irregular/testing-v0.toml", "testing"],
    ["all-fields/matrix-v1.toml", "matrix"], ["all-fields/testing-v1.toml", "testing"],
  ];
  for (const [path, roadmap] of paths) {
    const bytes = readFixture(context, path);
    const doc = decodeRoadmapSource(bytes, path, roadmap);
    assert(bytesEqual(composeRoadmapDocument(doc), bytes), `${path} canonical identity`);
  }
}

function allFieldsCoverage(context: SelfTestContext): void {
  const matrix = roadmapFixture(context, "all-fields/matrix-v1.toml", "matrix");
  const testing = roadmapFixture(context, "all-fields/testing-v1.toml", "testing");
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
): RoadmapDocumentV1 | ReturnType<typeof decodeRoadmapSource> {
  const decoded = decodeRoadmapSource(readFixture(context, relative), relative, roadmap, false);
  const canonical = composeRoadmapDocument(decoded);
  const reparsed = decodeRoadmapSource(canonical, `${relative}#canonical`, roadmap);
  assert(bytesEqual(composeRoadmapDocument(reparsed), canonical), `${relative} canonical loader identity`);
  return reparsed;
}

function canonicalCampaignFixture(context: SelfTestContext, relative: string): void {
  const decoded = decodeCampaignSource(readFixture(context, relative), relative, false);
  const canonical = composeCampaignDocument(decoded);
  assert(bytesEqual(composeCampaignDocument(decodeCampaignSource(canonical, `${relative}#canonical`)), canonical), `${relative} canonical campaign identity`);
}

function canonicalRetiredFixture(context: SelfTestContext, relative: string): void {
  const decoded = decodeRetiredSource(readFixture(context, relative), relative, false);
  const canonical = composeRetiredIdsDocument(decoded);
  assert(bytesEqual(composeRetiredIdsDocument(decodeRetiredSource(canonical, `${relative}#canonical`)), canonical), `${relative} canonical retired identity`);
}

type SchemaGroupName = "roadmap" | "manifest" | "reference" | "semantic" | "matrix" | "testing" | "campaign" | "retired";
type EnumGroupName = "roadmap" | "semantic" | "matrix" | "testing" | "campaign" | "retired";

interface FixtureSource {
  readonly path: string;
  readonly bytes: Uint8Array;
  readonly kind: "roadmap" | "campaign" | "retired";
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
  { name: "roadmap", rows: ROADMAP_V2_SCHEMA_ROWS },
  { name: "manifest", rows: MANIFEST_SCHEMA_ROWS },
  { name: "reference", rows: REFERENCE_SCHEMA_ROWS },
  { name: "semantic", rows: SHARED_SEMANTIC_SCHEMA_ROWS },
  { name: "semantic", rows: SEMANTIC_V2_SCHEMA_ROWS },
  { name: "matrix", rows: MATRIX_SCHEMA_ROWS },
  { name: "testing", rows: TESTING_SCHEMA_ROWS },
  { name: "campaign", rows: CAMPAIGN_SCHEMA_ROWS },
  { name: "retired", rows: RETIRED_SCHEMA_ROWS },
];

const ENUM_FIELD_GROUPS: readonly { readonly name: EnumGroupName; readonly fields: readonly EnumSchemaField[] }[] = [
  { name: "roadmap", fields: ROADMAP_ENUM_FIELDS },
  { name: "semantic", fields: SEMANTIC_ENUM_FIELDS },
  { name: "matrix", fields: MATRIX_ENUM_FIELDS },
  { name: "testing", fields: TESTING_ENUM_FIELDS },
  { name: "campaign", fields: CAMPAIGN_ENUM_FIELDS },
  { name: "retired", fields: RETIRED_ENUM_FIELDS },
];

const ENUM_KEY_OVERRIDES: Readonly<Record<string, string>> = {
  [["roadmap", "authority_v0"].join(":")]: "authority",
  [["roadmap", "authority_v1"].join(":")]: "authority",
  [["roadmap", "fragment_lifecycle_disposition"].join(":")]: "lifecycle_disposition",
  [["roadmap", "part_lifecycle_disposition"].join(":")]: "lifecycle_disposition",
  [["roadmap", "manifest_kind"].join(":")]: "kind",
  [["roadmap", "relation_kind"].join(":")]: "kind",
  [["roadmap", "reference_kind"].join(":")]: "kind",
  "semantic:shared_semantic_kind": "kind",
  "semantic:decision_permanence": "permanence",
  "semantic:signal_evaluation": "evaluation",
  "semantic:evidence_stage": "stages",
  "semantic:family_campaign_state": "campaign_state",
  "semantic:family_authority_kind": "authority_kind",
  "semantic:family_cell_spec_legality": "spec_legality",
  "semantic:family_exclusion_spec_legality": "spec_legality",
  "semantic:family_cell_disposition": "cell_disposition",
  "matrix:matrix_semantic_kind": "kind",
  "matrix:policy_permanence": "permanence",
  "testing:testing_semantic_kind": "kind",
  "campaign:reservation_roadmap_path": "roadmap_path",
  "campaign:selection_target_kind": "target_kind",
  "campaign:campaign_cost_posture": "posture",
  "retired:replacement_kind": "kind",
};

function injectV0StructuralFixture(bytes: Uint8Array): Uint8Array {
  const injected = `
[[fragment]]
fragment_id = "fixture-fragment"
projection_group = "fixture"
source_block_md = """Injected fixture fragment.
"""
span_ids = ["section"]

[[legacy_marker]]
marker_id = "fixture-marker"
legacy_aliases = ["fixture-marker-alias"]
source_block_md = """Injected fixture marker.
"""
span_ids = ["section"]

[[part]]
part_id = "fixture-part"
parent_record_id = "matrix.fixture-irregular"
source_block_md = """Injected fixture part.
"""
span_ids = ["section"]
`;
  const source = new TextDecoder().decode(bytes);
  const marker = "\n[[generated_slot]]";
  assert(source.includes(marker), "v0 structural fixture injection point");
  return text(source.replace(marker, `${injected}${marker}`));
}

function injectV1SemanticStructuralFixture(bytes: Uint8Array): Uint8Array {
  const injected = `
[[section]]
section_id = "fixture-semantic-section"
title = "Injected semantic section"
render_authority = "semantic"
body_md = """Injected semantic section.
"""

[[fragment]]
fragment_id = "fixture-semantic-fragment"
projection_group = "fixture"
render_authority = "semantic"
lifecycle_disposition = "document_prose"
body_md = """Injected semantic fragment.
"""

[[legacy_marker]]
marker_id = "fixture-semantic-marker"
legacy_aliases = ["fixture-semantic-marker-alias"]
render_authority = "semantic"
marker_md = """Injected semantic marker.
"""

[[part]]
part_id = "fixture-semantic-part"
parent_record_id = "matrix.fixture-raw-owner"
render_authority = "semantic"
lifecycle_disposition = "parent_supporting_prose"
body_md = """Injected semantic part.
"""
`;
  const source = new TextDecoder().decode(bytes);
  const marker = "\n[[record]]";
  assert(source.includes(marker), "v1 semantic structural fixture injection point");
  return text(source.replace(marker, `${injected}${marker}`));
}

function productionFixtureSources(context: SelfTestContext): { fixtures: readonly FixtureSource[]; reads: number } {
  const fixtures: FixtureSource[] = [];
  let reads = 0;
  const read = (path: string): Uint8Array => {
    reads++;
    return readFixture(context, path);
  };
  const roadmapFixtures = [
    ["positive/minimal-matrix-v0.toml", "matrix"], ["positive/minimal-testing-v0.toml", "testing"],
    ["positive/mixed-matrix-v1.toml", "matrix"], ["positive/mixed-testing-v1.toml", "testing"],
    ["irregular/matrix-v0.toml", "matrix"], ["irregular/testing-v0.toml", "testing"],
    ["all-fields/matrix-v1.toml", "matrix"], ["all-fields/testing-v1.toml", "testing"],
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
    path: "synthetic/minimal-matrix-v2.toml",
    bytes: minimalV2Roadmap(),
    kind: "roadmap",
    roadmap: "matrix",
    enum_groups: ["roadmap", "semantic", "matrix"],
  });
  fixtures.push({
    path: "synthetic/closed-denominator-matrix-v2.toml",
    bytes: syntheticClosedDenominatorV2Source(),
    kind: "roadmap",
    roadmap: "matrix",
    enum_groups: ["roadmap", "semantic", "matrix"],
  });
  for (const path of ["all-fields/campaign-pre-cutover.toml", "all-fields/campaign-matrix-cutover.toml", "all-fields/campaign-both-cut-over.toml"] as const) {
    fixtures.push({ path, bytes: read(path), kind: "campaign", enum_groups: ["campaign"] });
  }
  const retiredPath = "all-fields/retired-ids-v1.toml";
  fixtures.push({ path: retiredPath, bytes: read(retiredPath), kind: "retired", enum_groups: ["retired"] });

  const v0 = fixtures.find((fixture) => fixture.path === "irregular/matrix-v0.toml")!;
  fixtures.push({ ...v0, path: `${v0.path}#injected-structural-arms`, bytes: injectV0StructuralFixture(v0.bytes) });
  const v1 = fixtures.find((fixture) => fixture.path === "all-fields/matrix-v1.toml")!;
  fixtures.push({ ...v1, path: `${v1.path}#injected-semantic-arms`, bytes: injectV1SemanticStructuralFixture(v1.bytes) });
  return { fixtures, reads };
}

function loadFixtureSource(fixture: FixtureSource, bytes: Uint8Array, trace?: SchemaDecodeTrace): void {
  if (fixture.kind === "roadmap") {
    decodeRoadmapSource(bytes, fixture.path, fixture.roadmap, false, trace);
  } else if (fixture.kind === "campaign") {
    decodeCampaignSource(bytes, fixture.path, false, trace);
  } else {
    decodeRetiredSource(bytes, fixture.path, false, trace);
  }
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
  if (tablePath === "manifest") {
    const firstEntry = scan.sections.find((candidate) => candidate.path === "manifest.entry[0]");
    assert(firstEntry !== undefined, "manifest fixture target");
    return spliceLines(bytes, firstEntry.header_line, firstEntry.header_line, ["[manifest]", `${key} = ${expression}`, ""]);
  }
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
  if (tablePath === "manifest" && key === "entry") {
    const removedScan = scanTomlSections(removed);
    const next = removedScan.sections.find((candidate) => candidate.path === "source_span[0]");
    assert(next !== undefined, "manifest empty-table insertion point");
    return spliceLines(removed, next.header_line, next.header_line, ["[manifest]", ""]);
  }
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
  if (field.name === "manifest_kind") return section.path.startsWith("manifest.entry[");
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
    case "strict_unknown_top": expectFailure(() => decodeRoadmapSource(text(`unknown = 1\n\n${new TextDecoder().decode(minimalRoadmap(0))}`), "<unknown-top>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"], "unknown"); return;
    case "strict_unknown_nested_record": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap(0)).replace('span_ids = ["record"]', 'span_ids = ["record"]\nunknown = 1')), "<unknown-record>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"]); return;
    case "strict_unknown_campaign": expectFailure(() => decodeCampaignSource(text(new TextDecoder().decode(emptyCampaign()) + "unknown = 1\n"), "<campaign>"), ["E-SCHEMA-UNKNOWN-KEY"]); return;
    case "strict_unknown_retired": expectFailure(() => decodeRetiredSource(text(new TextDecoder().decode(emptyRetired()) + "unknown = 1\n"), "<retired>"), ["E-SCHEMA-UNKNOWN-KEY"]); return;
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
    case "strict_missing_discriminator": expectFailure(() => decodePayload('summary_md = """Missing."""\n', "matrix"), ["E-SCHEMA-MISSING-KEY"]); return;
    case "strict_generic_state_rejected": expectFailure(() => decodePayload(`${READY}state = "ready"\n`, "matrix"), ["E-SCHEMA-UNKNOWN-KEY"]); return;
    case "strict_generic_disposition_rejected": expectFailure(() => decodePayload(`${READY}disposition = "ready"\n`, "matrix"), ["E-SCHEMA-UNKNOWN-KEY"]); return;
    case "v0_missing_manifest": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap(0)).replace(/\n\[\[manifest\.entry\]\][\s\S]*?(?=\n\[\[source_span\]\])/, "")), "<missing-manifest>", "matrix"), ["E-SCHEMA-MISSING-KEY"]); return;
    case "v0_empty_records_floor": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap(0)).replace(/\n\[\[record\]\][\s\S]*?(?=\n\[\[manifest\.entry\]\])/, "")), "<empty-records>", "matrix"), ["E-SCHEMA-MISSING-KEY", "E-SCHEMA-FLOOR"]); return;
    case "truncated_span_read": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap(0)).replace("start_byte = 1\nend_byte = 2", "start_byte = 1\nend_byte = 3")), "<truncated-span>", "matrix"), ["E-SPAN-BOUNDS"], "source_span[1].end_byte"); return;
    case "campaign_missing_root": expectFailure(() => decodeCampaignSource(text("selection = []\n"), "<campaign>"), ["E-SCHEMA-MISSING-KEY"]); return;
    case "campaign_empty_valid": { const bytes = emptyCampaign(); assert(bytesEqual(composeCampaignDocument(decodeCampaignSource(bytes, "<campaign>")), bytes), "empty campaign identity"); return; }
    case "campaign_impossible_authority_tuple_rejected": {
      for (const [matrixAuthority, testingAuthority] of [
        ["legacy_markdown", "shadow"],
        ["legacy_markdown", "authoritative"],
        ["shadow", "authoritative"],
      ] as const) {
        expectFailure(() => decodeCampaignSource(text(`[campaign]\nschema_version = 1\nmatrix_authority = "${matrixAuthority}"\ntesting_authority = "${testingAuthority}"\n`), "<campaign-authority>"), ["E-SCHEMA-STATE"], "campaign.testing_authority");
      }
      return;
    }
    case "retired_missing_root": expectFailure(() => decodeRetiredSource(text(""), "<retired>"), ["E-SCHEMA-MISSING-KEY"]); return;
    case "retired_empty_valid": { const bytes = emptyRetired(); assert(bytesEqual(composeRetiredIdsDocument(decodeRetiredSource(bytes, "<retired>")), bytes), "empty retired identity"); return; }
    case "v0_all_fields_identity": case "v1_all_fields_identity":
      assert(context !== undefined, `${id} requires fixture ports`); fixtureIdentity(context); return;
    case "domain_matrix_all_tags": case "domain_testing_all_tags":
      assert(context !== undefined, `${id} requires fixture ports`); allFieldsCoverage(context); return;
    case "campaign_all_fields_identity": {
      assert(context !== undefined, `${id} requires fixture ports`);
      for (const path of ["all-fields/campaign-pre-cutover.toml", "all-fields/campaign-matrix-cutover.toml", "all-fields/campaign-both-cut-over.toml"]) {
        const bytes = readFixture(context, path); assert(bytesEqual(composeCampaignDocument(decodeCampaignSource(bytes, path)), bytes), path);
      }
      const bounded = fixtureText(context, "all-fields/campaign-both-cut-over.toml");
      expectFailure(
        () => decodeCampaignSource(text(bounded.replace(
          'implementation_units = ["fixture-implementation"]',
          "implementation_units = []",
        )), "<empty-campaign-cost-set>"),
        ["E-SCHEMA-FLOOR"],
        "selection[1].cost_bound.implementation_units",
      );
      expectFailure(
        () => decodeCampaignSource(text(bounded.replace(
          'implementation_units = ["fixture-implementation"]',
          'implementation_units = ["fixture-implementation", "fixture-implementation"]',
        )), "<duplicate-campaign-cost-set>"),
        ["E-SCHEMA-TYPE"],
        "selection[1].cost_bound.implementation_units",
      );
      expectFailure(
        () => decodeCampaignSource(text(bounded.replace(
          'validation_units = ["fixture-validation"]',
          'validation_units = ["fixture-implementation"]',
        )), "<overlapping-campaign-cost-sets>"),
        ["E-SCHEMA-STATE"],
        "selection[1].cost_bound.validation_units",
      );
      expectFailure(
        () => decodeCampaignSource(text(bounded.replace(
          'assumption_md = """The fixture bounds one reviewed implementation unit and one validation unit."""',
          'assumption_md = """"""',
        )), "<empty-campaign-cost-assumption>"),
        ["E-SCHEMA-FLOOR"],
        "selection[1].cost_bound.assumption_md",
      );
      return;
    }
    case "retired_all_fields_identity": { assert(context !== undefined, `${id} requires fixture ports`); const path = "all-fields/retired-ids-v1.toml"; const bytes = readFixture(context, path); assert(bytesEqual(composeRetiredIdsDocument(decodeRetiredSource(bytes, path)), bytes), path); return; }
    case "noncanonical_comment": expectFailure(() => decodeRoadmapSource(text(`# comment\n${new TextDecoder().decode(minimalRoadmap(0))}`), "<comment>", "matrix"), ["E-TOML-NONCANONICAL"]); return;
    case "noncanonical_inline_table": expectFailure(() => decodeRetiredSource(text(`[retired_ids]\nschema_version = 1\n\n[[retired_ids.entry]]\nid = "matrix.fixture-retired"\nlast_active_at = "${"0".repeat(40)}"\nreplacement = { kind = "gate", gate_id = "fixture", claim_md = """Claim.""" }\n`), "<inline-table>"), ["E-TOML-NONCANONICAL"]); return;
    case "noncanonical_literal_string": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap(0)).replace('source_block_md = """S\n"""', "source_block_md = '''S\n'''")), "<literal>", "matrix"), ["E-CODEC-PLACEHOLDER"]); return;
    case "noncanonical_set_order": {
      expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap(0)).replace('title = "Fixture"', 'title = "Fixture"\nlegacy_aliases = ["z", "a"]')), "<set-order>", "matrix"), ["E-TOML-NONCANONICAL"]);
      expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap(0)).replace('span_ids = ["record"]', 'span_ids = ["record", "record"]')), "<duplicate-set>", "matrix"), ["E-SCHEMA-TYPE"], "record[0].span_ids");
      return;
    }
    case "noncanonical_table_order": {
      const source = new TextDecoder().decode(minimalRoadmap(0));
      const section = source.match(/\n\[\[section\]\][\s\S]*?(?=\n\[\[record\]\])/)?.[0];
      const record = source.match(/\n\[\[record\]\][\s\S]*?(?=\n\[\[manifest\.entry\]\])/)?.[0];
      assert(section !== undefined && record !== undefined, "canonical fixture blocks");
      const swapped = source.replace(section, "__SECTION__").replace(record, section).replace("__SECTION__", record);
      expectFailure(() => decodeRoadmapSource(text(swapped), "<table-order>", "matrix"), ["E-TOML-NONCANONICAL"]);
      return;
    }
    case "toml_terminal_newline": assert(composeRoadmapDocument(decodeRoadmapSource(minimalRoadmap(0), "<terminal>", "matrix")).at(-1) === 0x0a, "one TOML terminal LF"); return;
    case "v0_rejects_authoritative": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap(0)).replace('authority = "shadow"', 'authority = "authoritative"')), "<v0-authority>", "matrix"), ["E-SCHEMA-ENUM"]); return;
    case "v0_rejects_semantics": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap(0)).replace('title = "Fixture record"', 'title = "Fixture record"\nrender_authority = "raw"')), "<v0-semantic>", "matrix"), ["E-SCHEMA-FORBIDDEN-KEY"]); return;
    case "v1_raw_requires_frozen_span": case "v1_new_raw_rejected": expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap(1)).replace('frozen_legacy_span_ids = ["record", "section"]', 'frozen_legacy_span_ids = ["section"]')), "<v1-raw>", "matrix"), ["E-SCHEMA-STATE"]); return;
    case "v1_raw_shadow_nonrendering": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/matrix-v1.toml");
      const decoded = decodeRoadmapSource(text(source), "<raw-shadow>", "matrix", false);
      assert("relations" in decoded, "v1 raw-shadow fixture");
      const record = decoded.records.find((entry) => entry.id === "matrix.fixture-raw-owner");
      assert(record !== undefined && record.render_authority === "raw" && record.semantic_shadow !== undefined, "raw owner retains a nonrendering semantic shadow");
      assert(new TextDecoder().decode(record.source_block_md).startsWith("RAW OWNER"), "raw bytes remain the render authority");
      const mutated = replaceAfter(source, 'id = "matrix.fixture-raw-owner"', 'render_authority = "raw"', 'render_authority = "semantic"');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<raw-shadow-authority>", "matrix"), ["E-SCHEMA-FORBIDDEN-KEY"]);
      return;
    }
    case "v2_semantic_identity": {
      const decoded = minimalV2Document();
      const composed = composeRoadmapDocument(decoded);
      assert(bytesEqual(composed, minimalV2Roadmap()), "schema v2 did not preserve canonical wire identity");
      const roundTrip = decodeRoadmapSource(composed, "<v2-round-trip>", "matrix");
      assert(roundTrip.document.schema_version === 2, "schema v2 round-trip downgraded the document");
      assert(bytesEqual(composeRoadmapDocument(roundTrip), composed), "schema v2 second composition drifted");
      return;
    }
    case "v2_migration_escape_hatches_rejected": {
      const canonical = new TextDecoder().decode(minimalV2Roadmap());
      const semantic = minimalV2Document();
      const reject = (name: string, source: string, codes: readonly string[]): void => {
        const error = expectFailure(() => decodeRoadmapSource(text(source), `<v2-${name}>`, "matrix"), codes);
        assert(error.issue.logical_path.length > 0, `${name} rejection omitted its logical coordinate`);
      };
      const rejectDocument = (name: string, document: RoadmapDocumentV2): void =>
        reject(name, new TextDecoder().decode(composeRoadmapDocument(document)), ["E-SCHEMA-STATE"]);
      reject(
        "semantic-conversion",
        canonical.replace('authority = "authoritative"\n', 'authority = "authoritative"\nsemantic_conversion = "complete"\n'),
        ["E-SCHEMA-FORBIDDEN-KEY"],
      );
      reject(
        "frozen-legacy-spans",
        canonical.replace('authority = "authoritative"\n', 'authority = "authoritative"\nfrozen_legacy_span_ids = []\n'),
        ["E-SCHEMA-FORBIDDEN-KEY"],
      );
      reject(
        "raw-section",
        canonical.replace(
          'render_authority = "semantic"\nbody_md = """S\n"""\n\n[[section.source_replacement]]\nspan_id = "section"\nreplacement_field = "body_md"\nreview_note_md = """Reviewed section.\n"""',
          'render_authority = "raw"\nsource_block_md = """S\n"""\nspan_ids = ["section"]',
        ),
        ["E-SCHEMA-STATE"],
      );
      reject(
        "raw-record",
        canonical.replace(
          'render_authority = "semantic"\nprojection_visibility = "document"\n\n[record.payload]',
          'render_authority = "raw"\nsource_block_md = """R\n"""\nspan_ids = ["record"]\n\n[record.semantic_shadow]',
        ).replace(/\n\n\[\[record\.source_replacement\]\][\s\S]*?review_note_md = """Reviewed record\.\n"""/u, ""),
        ["E-SCHEMA-STATE"],
      );
      rejectDocument("raw-fragment", {
        ...semantic,
        fragments: [{
          fragment_id: "raw-fragment",
          projection_group: "fixture",
          render_authority: "raw",
          lifecycle_disposition: "document_prose",
          source_block_md: text("F\n"),
          span_ids: ["section"],
        }],
      } as unknown as RoadmapDocumentV2);
      rejectDocument("raw-legacy-marker", {
        ...semantic,
        legacy_markers: [{
          marker_id: "raw-marker",
          legacy_aliases: ["Raw marker"],
          render_authority: "raw",
          source_block_md: text("M\n"),
          span_ids: ["section"],
        }],
      } as unknown as RoadmapDocumentV2);
      rejectDocument("raw-part", {
        ...semantic,
        parts: [{
          part_id: "raw-part",
          parent_record_id: "matrix.fixture-minimal",
          render_authority: "raw",
          lifecycle_disposition: "parent_supporting_prose",
          source_block_md: text("P\n"),
          span_ids: ["section"],
        }],
      } as unknown as RoadmapDocumentV2);
      reject(
        "semantic-owner-raw-fields",
        canonical.replace('body_md = """S\n"""', 'body_md = """S\n"""\nsource_block_md = """S\n"""\nspan_ids = ["section"]'),
        ["E-SCHEMA-UNKNOWN-KEY", "E-SCHEMA-FORBIDDEN-KEY"],
      );
      reject(
        "raw-span",
        canonical.replace('migration_status = "replaced"', 'migration_status = "raw"'),
        ["E-SCHEMA-STATE"],
      );
      return;
    }
    case "v2_intrinsic_completion": {
      const document = minimalV2Document();
      const state = semanticConversionState(document);
      assert(state.declared === "intrinsic" && state.effective === "complete", "schema v2 completion is not intrinsic");
      assert(validateSemanticConversionDeclaration(document, false).length === 0, "schema v2 intrinsic completion failed declaration validation");
      const forged = {
        ...document,
        document: { ...document.document, semantic_conversion: "complete" },
      } as RoadmapDocumentV2;
      const issues = validateSemanticConversionDeclaration(forged, false);
      assert(
        issues.length === 1 && issues[0]!.code === "E-SCHEMA-STATE" &&
          issues[0]!.logical_path === "document.semantic_conversion",
        "schema v2 accepted a programmatic migration declaration",
      );
      return;
    }
    case "v2_unresolved_migration_reference_rejected": {
      const source = `${new TextDecoder().decode(minimalV2Roadmap())}
[[reference]]
id = "legacy-debt"
source = "matrix.fixture-minimal"
kind = "unresolved_migration"
local_reference = "legacy raw owner"
uncertainty_md = """This migration reference must not survive v2.
"""
expires_at = "2026-08-12"
`;
      expectFailure(
        () => decodeRoadmapSource(text(source), "<v2-unresolved-migration>", "matrix"),
        ["E-SCHEMA-STATE"],
        "reference[0].kind",
      );
      return;
    }
    case "v3_unsupported":
      expectFailure(
        () => decodeRoadmapSource(
          text(new TextDecoder().decode(minimalV2Roadmap()).replace("schema_version = 2", "schema_version = 3")),
          "<v3>",
          "matrix",
        ),
        ["E-SCHEMA-VERSION"],
        "document.schema_version",
      );
      return;
    case "v1_semantic_forbids_raw": {
      expectFailure(() => decodeRoadmapSource(text(new TextDecoder().decode(minimalRoadmap(1)).replace('render_authority = "raw"\nsource_block_md = """R', 'render_authority = "semantic"\nsource_block_md = """R')), "<semantic-raw>", "matrix"), ["E-SCHEMA-FORBIDDEN-KEY", "E-SCHEMA-UNKNOWN-KEY"]);
      return;
    }
    case "schema_lifecycle_raw_omission_historical": {
      const source = subordinateRoadmap(1, "raw_omitted", "raw_omitted");
      const decoded = decodeRoadmapSource(text(source), "<historical-lifecycle-omission>", "matrix");
      assert(
        decoded.document.schema_version === 1 &&
          decoded.fragments[0] !== undefined && "render_authority" in decoded.fragments[0] &&
          decoded.fragments[0].render_authority === "raw" && decoded.fragments[0].lifecycle_disposition === undefined &&
          decoded.parts[0] !== undefined && "render_authority" in decoded.parts[0] &&
          decoded.parts[0].render_authority === "raw" && decoded.parts[0].lifecycle_disposition === undefined,
        "historical v1 lifecycle omission did not remain loadable and explicit in the model",
      );
      return;
    }
    case "schema_lifecycle_raw_review_arms": {
      for (const [fragment, part] of [
        ["raw_pending", "raw_pending"],
        ["raw_reviewed", "raw_reviewed"],
        ["raw_independent", "raw_independent"],
      ] as const) {
        const source = subordinateRoadmap(1, fragment, part);
        const decoded = decodeRoadmapSource(text(source), `<raw-lifecycle-${fragment}>`, "matrix");
        assert(bytesEqual(composeRoadmapDocument(decoded), text(source)), `${fragment} raw lifecycle arm lost canonical identity`);
      }
      return;
    }
    case "schema_lifecycle_semantic_requires_reviewed": {
      const fragment = subordinateRoadmap(1, "semantic_reviewed", "raw_reviewed");
      const part = subordinateRoadmap(1, "raw_reviewed", "semantic_reviewed");
      decodeRoadmapSource(text(fragment), "<semantic-fragment-reviewed>", "matrix");
      decodeRoadmapSource(text(part), "<semantic-part-reviewed>", "matrix");
      for (const disposition of [undefined, "pending_review", "independent_record"] as const) {
        const mutated = disposition === undefined
          ? fragment.replace('lifecycle_disposition = "document_prose"\n', "")
          : fragment.replace('lifecycle_disposition = "document_prose"', `lifecycle_disposition = "${disposition}"`);
        expectFailure(
          () => decodeRoadmapSource(text(mutated), `<semantic-fragment-${disposition ?? "missing"}>`, "matrix"),
          disposition === undefined ? ["E-SCHEMA-MISSING-KEY"] : ["E-SCHEMA-ENUM"],
          "fragment[0].lifecycle_disposition",
        );
      }
      return;
    }
    case "schema_lifecycle_cross_kind_rejected": {
      const fragment = subordinateRoadmap(1, "raw_reviewed", "raw_reviewed")
        .replace('lifecycle_disposition = "document_prose"', 'lifecycle_disposition = "parent_supporting_prose"');
      expectFailure(() => decodeRoadmapSource(text(fragment), "<fragment-part-disposition>", "matrix"), ["E-SCHEMA-ENUM"], "fragment[0].lifecycle_disposition");
      const part = subordinateRoadmap(1, "raw_reviewed", "raw_reviewed")
        .replace('lifecycle_disposition = "parent_supporting_prose"', 'lifecycle_disposition = "document_prose"');
      expectFailure(() => decodeRoadmapSource(text(part), "<part-fragment-disposition>", "matrix"), ["E-SCHEMA-ENUM"], "part[0].lifecycle_disposition");
      return;
    }
    case "schema_lifecycle_v0_forbidden": {
      const v0 = subordinateRoadmap(0, "raw_omitted", "raw_omitted")
        .replace('fragment_id = "fragment"\n', 'fragment_id = "fragment"\nlifecycle_disposition = "document_prose"\n');
      expectFailure(() => decodeRoadmapSource(text(v0), "<v0-lifecycle>", "matrix"), ["E-SCHEMA-FORBIDDEN-KEY"], "fragment[0].lifecycle_disposition");
      return;
    }
    case "schema_projection_visibility_document_arm": {
      const document = semanticRecordRoadmap("document");
      decodeRoadmapSource(text(document), "<semantic-document>", "matrix");
      expectFailure(() => decodeRoadmapSource(text(document.replace('projection_visibility = "document"\n', "")), "<missing-visibility>", "matrix"), ["E-SCHEMA-MISSING-KEY"]);
      expectFailure(() => decodeRoadmapSource(text(document.replace(/\n\n\[\[record\.source_replacement\]\][\s\S]*?review_note_md = """Reviewed\.\n"""/u, "")), "<document-no-replacement>", "matrix"), ["E-SCHEMA-STATE"]);
      return;
    }
    case "schema_projection_visibility_semantic_only_arm": {
      const semanticOnly = semanticRecordRoadmap("semantic_only");
      const semanticOnlyDocument = decodeRoadmapSource(text(semanticOnly), "<semantic-only>", "matrix");
      const semanticOnlyRoundTrip = decodeRoadmapSource(composeRoadmapDocument(semanticOnlyDocument), "<semantic-only-round-trip>", "matrix");
      assert(semanticOnlyRoundTrip.document.schema_version === 1 && semanticOnlyRoundTrip.records.some((record) => "projection_visibility" in record && record.projection_visibility === "semantic_only"), "semantic-only visibility did not compose/decode round-trip");
      expectFailure(() => decodeRoadmapSource(text(semanticOnly.replace('[record.payload]', '[[record.source_replacement]]\nspan_id = "record"\nreplacement_field = "payload.summary_md"\nreview_note_md = """No.\n"""\n\n[record.payload]')), "<semantic-only-replacement>", "matrix"), ["E-SCHEMA-STATE"]);
      expectFailure(() => decodeRoadmapSource(text(semanticOnly + '\n[[source_span]]\nid = "semantic-only-span"\nstart_byte = 1\nend_byte = 2\nsha256 = "' + ZERO_HASH + '"\nsource_kind = "record"\nowner_id = "matrix.fixture-minimal"\nowner_field = "payload.summary_md"\nmigration_status = "replaced"\n'), "<semantic-only-span>", "matrix"), ["E-SCHEMA-STATE"]);
      return;
    }
    case "schema_projection_visibility_forbidden_nonsemantic_arms": {
      for (const [version, misplaced] of [[1, new TextDecoder().decode(minimalRoadmap(1))], [0, new TextDecoder().decode(minimalRoadmap(0))]] as const) {
        expectFailure(() => decodeRoadmapSource(text(misplaced.replace('source_block_md = """R', 'projection_visibility = "document"\nsource_block_md = """R')), `<misplaced-visibility-v${version}>`, "matrix"), ["E-SCHEMA-FORBIDDEN-KEY"]);
      }
      const shadowMisplaced = new TextDecoder().decode(minimalRoadmap(1)).replace(
        'span_ids = ["record"]',
        'span_ids = ["record"]\n\n[record.semantic_shadow]\nkind = "decision"\nsummary_md = """Shadow.\n"""\nprojection_visibility = "semantic_only"\ndecision_state = "pending"\nquestion_md = """Question.\n"""',
      );
      expectFailure(() => decodeRoadmapSource(text(shadowMisplaced), "<misplaced-shadow-visibility>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"]);
      return;
    }
    case "domain_defect_regression_required": expectFailure(() => decodePayload(READY.replace('work_kind = "feature"', 'work_kind = "defect"'), "matrix", "authority"), ["E-SCHEMA-STATE"]); return;
    case "domain_missing_system_admission_required": expectFailure(() => decodePayload(READY.replace('work_kind = "feature"', 'work_kind = "missing_system"'), "testing", "authority"), ["E-SCHEMA-STATE"]); return;
    case "schema_held_permanent_rejected": expectFailure(() => decodePayload('kind = "decision"\nsummary_md = """Held."""\ndecision_state = "held"\nrationale_md = """R."""\npermanence = "permanent"\ntransition_ids = ["matrix.fixture-signal"]\n', "matrix"), ["E-SCHEMA-ENUM"]); return;
    case "domain_quantitative_scope_unit_required": expectFailure(() => decodePayload(predicateSignal('predicate_kind = "quantitative"\ncomparator = "ge"\nthreshold = 2\nmeasurement = 1\nas_of = "2026-08-11"\n'), "matrix"), ["E-SCHEMA-MISSING-KEY"]); return;
    case "domain_manual_not_auto_boolean": {
      const decoded = decodePayload(predicateSignal('predicate_kind = "manual"\nreview_procedure_md = """Review."""\nevidence_ids = ["matrix.fixture-evidence"]\n', "manual"), "matrix");
      assert(decoded !== undefined, "manual predicate remains authored"); return;
    }
    case "domain_state_required_forbidden": {
      expectFailure(() => decodePayload(`${READY}blocker_md = """No."""\n`, "matrix", "authority"), ["E-SCHEMA-FORBIDDEN-KEY"]);
      expectFailure(() => decodePayload(READY.replace('acceptance_md = """Accepted."""', 'acceptance_md = """"""'), "matrix", "authority"), ["E-SCHEMA-FLOOR"], "p.acceptance_md");
      expectFailure(() => decodePayload(READY.replace('priority_rationale_md = """Normal."""', 'priority_rationale_md = """"""'), "matrix", "authority"), ["E-SCHEMA-FLOOR"], "p.priority_rationale_md");
      return;
    }
    case "domain_fired_transition_not_parked": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/matrix-v1.toml");
      const mutated = replaceAfter(source, 'id = "matrix.fixture-task-f"', 'transition_ids = ["matrix.fixture-signal-b"]', 'transition_ids = ["matrix.fixture-signal-a"]');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<fired-transition>", "matrix"), ["E-SCHEMA-STATE"], "record.matrix.fixture-task-f.transition_ids");
      return;
    }
    case "domain_already_met_signal_rejected": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/matrix-v1.toml");
      const mutated = replaceAfter(source, 'id = "matrix.fixture-signal-b"', 'evaluation = "unmet"', 'evaluation = "met"');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<already-met-signal>", "matrix"), ["E-SCHEMA-STATE"], "record.matrix.fixture-task-f.transition_ids");
      return;
    }
    case "domain_stale_unknown_visible": {
      const decoded = decodePayload('kind = "evidence"\nsummary_md = """Stale."""\nevidence_kind = "source_read"\nclaim_md = """Claim."""\nevidence_verdict = "unknown"\nfreshness = "stale"\nreference_ids = ["source"]\nobserved_at = "2026-08-11"\nenvironment_md = """Env."""\nunprobed_remainder_md = """Remainder."""\n\n[p.scope]\nsurfaces = ["fixture"]\n', "matrix");
      assert(decoded !== undefined, "stale/unknown is visible semantic state"); return;
    }
    case "evidence_point_requires_provenance": expectFailure(() => decodePayload('kind = "evidence"\nsummary_md = """Point."""\nevidence_kind = "source_read"\nclaim_md = """Claim."""\nevidence_verdict = "confirmed"\nfreshness = "as_of"\nreference_ids = ["source"]\nobserved_at = "2026-08-11"\nunprobed_remainder_md = """Remainder."""\n\n[p.scope]\nsurfaces = ["fixture"]\n', "matrix"), ["E-SCHEMA-STATE"]); return;
    case "evidence_negative_requires_enumeration": expectFailure(() => decodePayload('kind = "evidence"\nsummary_md = """Registry."""\nevidence_kind = "registry_enumeration"\nclaim_md = """None found."""\nevidence_verdict = "confirmed"\nfreshness = "live"\nreference_ids = ["registry"]\nunprobed_remainder_md = """None."""\nrefresh_reference_id = "refresh"\n\n[p.scope]\nsurfaces = ["fixture"]\n', "matrix"), ["E-SCHEMA-STATE"]); return;
    case "evidence_generator_requires_harness_free": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/matrix-v1.toml");
      const mutated = replaceAfter(source, 'id = "matrix.fixture-task-a"', 'evidence_ids = ["matrix.fixture-evidence-a", "matrix.fixture-evidence-c"]', 'evidence_ids = ["matrix.fixture-evidence-a"]');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<generator-evidence>", "matrix"), ["E-SCHEMA-STATE"], "record.matrix.fixture-task-a.evidence_ids");
      return;
    }
    case "evidence_timing_join_structural": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/testing-v1.toml");
      const mutated = replaceAfter(source, 'id = "testing.fixture-cost-historical"', 'evidence_ids = ["testing.fixture-evidence-gate"]', 'evidence_ids = ["testing.fixture-cost-historical"]');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<timing-evidence>", "testing"), ["E-SCHEMA-STATE"], "record.testing.fixture-cost-historical.evidence_ids");
      return;
    }
    case "evidence_draft_log_rejected": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/matrix-v1.toml");
      const mutated = replaceAfter(source, 'id = "ref-file"', 'path = "cddl-matrix/README.md"', 'path = "draft/logs/check-local.log"');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<draft-evidence>", "matrix"), ["E-REFERENCE-FORBIDDEN"], "reference[4].path");
      return;
    }
    case "schema_observed_at_civil_date": expectFailure(() => decodePayload('kind = "testing_cost"\nsummary_md = """Cost."""\ncost_posture = "historical_observation"\nunit = "ms"\nscope_md = """Scope."""\nvalue_min = 1\nvalue_max = 2\nobserved_at = "2025-02-29"\nenvironment_md = """Env."""\nevidence_ids = ["testing.fixture-evidence"]\n', "testing"), ["E-SCHEMA-TYPE"]); return;
    case "schema_campaign_slug_grammars": expectFailure(() => decodeCampaignSource(text(`${new TextDecoder().decode(emptyCampaign())}\n[[selection]]\nitem_id = "matrix.fixture-item"\ntarget_kind = "active_id"\nselected_state = "selected"\npriority_class = "High"\nselection_reason_md = """Reason."""\ncycle = "cycle-one"\nremaining_scope_md = """Remain."""\n`), "<slug>"), ["E-SCHEMA-TYPE"]); return;
    case "schema_duplicate_assignment_rejected": duplicateParseRejected(new TextDecoder().decode(minimalRoadmap(0)).replace("schema_version = 0", "schema_version = 0\nschema_version = 0")); return;
    case "schema_duplicate_table_rejected": duplicateParseRejected(new TextDecoder().decode(minimalRoadmap(0)).replace("\n[[section]]", "\n[document]\n\n[[section]]")); return;
    case "schema_duplicate_nested_payload_rejected": {
      const shadow = '\n[record.semantic_shadow]\nkind = "work"\nsummary_md = """Pending."""\nwork_state = "pending_review"\nwork_intent = "build_capability"\nwork_kind = "feature"\nrisk = "cosmetic"\nfamily_classification = "pending"\nuncertainty_md = """Review."""\n\n[record.semantic_shadow]\n';
      duplicateParseRejected(new TextDecoder().decode(minimalRoadmap(1)).replace("\n[[manifest.entry]]", `${shadow}\n[[manifest.entry]]`)); return;
    }
    case "domain_closed_denominator_rejected": expectFailure(() => decodePayload('kind = "family"\nsummary_md = """F."""\nfamily_maturity = "closed_denominator"\n', "matrix"), ["E-SCHEMA-ENUM"]); return;
    case "systematic_illegal_cell_rejected": expectFailure(() => decodePayload(`${OBSERVED_FAMILY}\n[[p.cell]]\nid = "matrix.fixture-cell"\nspec_legality = "illegal"\ncell_disposition = "unknown"\naffected_profiles = ["default"]\naffected_faces = ["rust"]\n\n[[p.cell.coordinate]]\naxis_id = "matrix.fixture-axis"\nvalue_id = "matrix.fixture-value"\n`, "matrix"), ["E-SCHEMA-ENUM"]); return;
    case "systematic_illegal_coordinate_is_exclusion": {
      const decoded = decodePayload(`${OBSERVED_FAMILY}\n[[p.exclusion]]\nid = "matrix.fixture-exclusion"\nspec_legality = "illegal"\nreason_md = """Illegal."""\nowner_reference_id = "owner"\nsource_reference_id = "source"\nliveness_reference_id = "liveness"\n\n[[p.exclusion.coordinate]]\naxis_id = "matrix.fixture-axis"\nvalue_id = "matrix.fixture-value"\n`, "matrix");
      assert(decoded !== undefined, "illegal coordinate decodes only as exclusion"); return;
    }
    case "systematic_unmodelled_coordinate_not_cell": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const source = fixtureText(context, "all-fields/matrix-v1.toml");
      const mutated = replaceAfter(source, 'id = "matrix.fixture-coordinate-a"', 'spec_legality = "legal"', 'spec_legality = "unknown"');
      expectFailure(() => decodeRoadmapSource(text(mutated), "<unmodelled-coordinate>", "matrix"), ["E-SCHEMA-ENUM"]);
      return;
    }
    case "schema_v0_exact_keys_every_table": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), [...ROADMAP_SCHEMA_ROWS.filter((_, index) => [0, 2, 4, 7, 10, 13, 17, 20, 21, 22, 23].includes(index)), ...MANIFEST_SCHEMA_ROWS]);
      canonicalRoadmapFixture(context, "irregular/matrix-v0.toml", "matrix");
      canonicalRoadmapFixture(context, "irregular/testing-v0.toml", "testing");
      return;
    }
    case "schema_v1_exact_keys_every_structural_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), [...ROADMAP_SCHEMA_ROWS.filter((_, index) => [1, 3, 5, 6, 8, 9, 11, 12, 14, 15, 16, 18, 19, 20, 21, 22, 23, 24, 25].includes(index)), ...MANIFEST_SCHEMA_ROWS]);
      canonicalRoadmapFixture(context, "all-fields/matrix-v1.toml", "matrix");
      canonicalRoadmapFixture(context, "all-fields/testing-v1.toml", "testing");
      return;
    }
    case "schema_shared_payload_exact_keys_every_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), SHARED_SEMANTIC_SCHEMA_ROWS);
      canonicalRoadmapFixture(context, "all-fields/matrix-v1.toml", "matrix");
      return;
    }
    case "schema_systematic_exact_keys_every_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), SHARED_SEMANTIC_SCHEMA_ROWS.filter((row) => row.name.includes("family")));
      canonicalRoadmapFixture(context, "all-fields/matrix-v1.toml", "matrix");
      return;
    }
    case "schema_matrix_payload_exact_keys_every_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), MATRIX_SCHEMA_ROWS);
      const source = fixtureText(context, "all-fields/matrix-v1.toml");
      canonicalRoadmapFixture(context, "all-fields/matrix-v1.toml", "matrix");
      const action = replaceAfter(source, 'id = "matrix.fixture-upstream-a"', 'action_id = "action-a-one"', 'action_id = "action-1"');
      expectFailure(() => decodeRoadmapSource(text(action), "<action-slug>", "matrix"), ["E-ID-GRAMMAR"]);
      const branch = replaceAfter(source, 'id = "matrix.fixture-upstream-a"', 'branch_id = "branch-a-one"', 'branch_id = "branch-1"');
      expectFailure(() => decodeRoadmapSource(text(branch), "<branch-slug>", "matrix"), ["E-ID-GRAMMAR"]);
      return;
    }
    case "schema_testing_payload_exact_keys_every_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), TESTING_SCHEMA_ROWS);
      const source = fixtureText(context, "all-fields/testing-v1.toml");
      canonicalRoadmapFixture(context, "all-fields/testing-v1.toml", "testing");
      const capture = replaceAfter(source, 'id = "testing.fixture-operational-attributed"', 'step_id = "capture"', 'step_id = "capture-1"');
      expectFailure(() => decodeRoadmapSource(text(capture), "<capture-slug>", "testing"), ["E-ID-GRAMMAR"]);
      return;
    }
    case "strict_unknown_reference": {
      const base = new TextDecoder().decode(minimalRoadmap(1));
      expectFailure(() => decodeRoadmapSource(text(`${base}\n[[reference]]\nid = "fixture"\nsource = "matrix.fixture-minimal"\nkind = "gate"\ngate_id = "fixture"\nunknown = 1\n`), "<unknown-reference>", "matrix"), ["E-SCHEMA-UNKNOWN-KEY"], "reference[0].unknown");
      expectFailure(() => decodeRoadmapSource(text(`${base}\n[[reference]]\nid = "fixture-1"\nsource = "matrix.fixture-minimal"\nkind = "gate"\ngate_id = "fixture"\n`), "<reference-id-grammar>", "matrix"), ["E-ID-GRAMMAR"], "reference[0].id");
      return;
    }
    case "schema_reference_exact_keys_every_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), [ROADMAP_SCHEMA_ROWS[25], ...REFERENCE_SCHEMA_ROWS]);
      canonicalRoadmapFixture(context, "all-fields/matrix-v1.toml", "matrix");
      return;
    }
    case "schema_campaign_retired_exact_keys_every_arm": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), [...CAMPAIGN_SCHEMA_ROWS, ...RETIRED_SCHEMA_ROWS]);
      canonicalCampaignFixture(context, "all-fields/campaign-pre-cutover.toml");
      canonicalRetiredFixture(context, "all-fields/retired-ids-v1.toml");
      return;
    }
    case "schema_campaign_authority_keys": case "schema_campaign_reservation_exact_keys": {
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), CAMPAIGN_SCHEMA_ROWS);
      canonicalCampaignFixture(context, "all-fields/campaign-pre-cutover.toml");
      canonicalCampaignFixture(context, "all-fields/campaign-matrix-cutover.toml");
      canonicalCampaignFixture(context, "all-fields/campaign-both-cut-over.toml");
      return;
    }
    case "schema_campaign_selection_binding_keys_forbidden": case "campaign_inline_legacy_binding_rejected": {
      assert(context !== undefined, `${id} requires fixture ports`);
      const proof = fixtureMutationProof(context);
      assertRowMutationCoverage(proof, [CAMPAIGN_SCHEMA_ROWS[3], CAMPAIGN_SCHEMA_ROWS[4]]);
      observeProofIssue(proof, (issue) => issue.code === "E-SCHEMA-FORBIDDEN-KEY" && issue.logical_path.includes("selection"));
      return;
    }
    case "schema_due_on_valid_through_postures": {
      expectFailure(() => decodePayload('kind = "signal"\nsummary_md = """Unblock."""\ntransition_kind = "unblock_predicate"\nowner_reference_id = "owner"\nevent_md = """Event."""\ncheck_procedure_md = """Check."""\ndue_action_md = """Act."""\ndue_on = "2026-08-11"\nevaluation = "unknown"\n', "matrix"), ["E-SCHEMA-UNKNOWN-KEY"]);
      expectFailure(() => decodePayload('kind = "evidence"\nsummary_md = """Historical."""\nevidence_kind = "source_read"\nclaim_md = """Claim."""\nevidence_verdict = "confirmed"\nfreshness = "historical"\nreference_ids = ["source"]\nobserved_at = "2026-08-11"\nvalid_through = "2026-08-12"\nenvironment_md = """Env."""\nunprobed_remainder_md = """None."""\n\n[p.scope]\nsurfaces = ["fixture"]\n', "matrix"), ["E-SCHEMA-FORBIDDEN-KEY", "E-SCHEMA-STATE"]);
      return;
    }
    case "schema_canonical_key_order_every_arm": {
      assert(new TextDecoder().decode(composeRoadmapDocument(decodeRoadmapSource(minimalRoadmap(0), "<order>", "matrix"))) === new TextDecoder().decode(minimalRoadmap(0)), "canonical key order");
      assert(context !== undefined, `${id} requires fixture ports`);
      assertRowMutationCoverage(fixtureMutationProof(context), ALL_SCHEMA_ROWS);
      for (const [path, roadmap] of [
        ["positive/minimal-matrix-v0.toml", "matrix"], ["positive/minimal-testing-v0.toml", "testing"],
        ["positive/mixed-matrix-v1.toml", "matrix"], ["positive/mixed-testing-v1.toml", "testing"],
        ["irregular/matrix-v0.toml", "matrix"], ["irregular/testing-v0.toml", "testing"],
        ["all-fields/matrix-v1.toml", "matrix"], ["all-fields/testing-v1.toml", "testing"],
      ] as const) canonicalRoadmapFixture(context, path, roadmap);
      for (const path of ["all-fields/campaign-pre-cutover.toml", "all-fields/campaign-matrix-cutover.toml", "all-fields/campaign-both-cut-over.toml"]) canonicalCampaignFixture(context, path);
      canonicalRetiredFixture(context, "all-fields/retired-ids-v1.toml");
      const matrix = composeRoadmapDocument(decodeRoadmapSource(readFixture(context, "all-fields/matrix-v1.toml"), "<work-order>", "matrix", false));
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
      ], "WP0 ready-work key universe");
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

const POSITIVE_SCHEMA_CASE_IDS: readonly RequiredSchemaSelfTestCaseId[] = [
  "campaign_empty_valid",
  "retired_empty_valid",
  "v0_all_fields_identity",
  "v1_all_fields_identity",
  "v2_semantic_identity",
  "v2_migration_escape_hatches_rejected",
  "v2_intrinsic_completion",
  "campaign_all_fields_identity",
  "retired_all_fields_identity",
  "toml_terminal_newline",
  "v1_raw_shadow_nonrendering",
  "schema_lifecycle_raw_omission_historical",
  "schema_lifecycle_raw_review_arms",
  "domain_matrix_all_tags",
  "domain_testing_all_tags",
  "domain_transition_each_kind",
  "domain_manual_not_auto_boolean",
  "domain_stale_unknown_visible",
  "schema_v0_exact_keys_every_table",
  "schema_v1_exact_keys_every_structural_arm",
  "schema_shared_payload_exact_keys_every_arm",
  "schema_matrix_payload_exact_keys_every_arm",
  "schema_testing_payload_exact_keys_every_arm",
  "schema_systematic_exact_keys_every_arm",
  "schema_reference_exact_keys_every_arm",
  "schema_campaign_retired_exact_keys_every_arm",
  "schema_canonical_key_order_every_arm",
  "schema_campaign_authority_keys",
  "schema_campaign_reservation_exact_keys",
  "systematic_illegal_coordinate_is_exclusion",
  "schema_priority_band_closed_enum",
];

const NEGATIVE_SCHEMA_CASE_IDS: readonly RequiredSchemaSelfTestCaseId[] = [
  "strict_unknown_top",
  "strict_unknown_nested_record",
  "strict_unknown_reference",
  "strict_unknown_campaign",
  "strict_unknown_retired",
  "strict_unknown_every_table",
  "strict_enum_every_field",
  "strict_unknown_kind",
  "strict_unknown_enum",
  "strict_missing_discriminator",
  "strict_generic_state_rejected",
  "strict_generic_disposition_rejected",
  "v0_missing_manifest",
  "v0_empty_records_floor",
  "truncated_span_read",
  "campaign_missing_root",
  "campaign_impossible_authority_tuple_rejected",
  "retired_missing_root",
  "noncanonical_literal_string",
  "noncanonical_table_order",
  "noncanonical_set_order",
  "v0_rejects_semantics",
  "v0_rejects_authoritative",
  "v1_raw_requires_frozen_span",
  "v1_new_raw_rejected",
  "v1_semantic_forbids_raw",
  "v2_unresolved_migration_reference_rejected",
  "v3_unsupported",
  "schema_lifecycle_semantic_requires_reviewed",
  "schema_lifecycle_cross_kind_rejected",
  "schema_lifecycle_v0_forbidden",
  "schema_projection_visibility_document_arm",
  "schema_projection_visibility_semantic_only_arm",
  "schema_projection_visibility_forbidden_nonsemantic_arms",
  "domain_state_required_forbidden",
  "domain_defect_regression_required",
  "domain_missing_system_admission_required",
  "domain_quantitative_scope_unit_required",
  "domain_fired_transition_not_parked",
  "domain_already_met_signal_rejected",
  "evidence_point_requires_provenance",
  "evidence_negative_requires_enumeration",
  "evidence_generator_requires_harness_free",
  "evidence_timing_join_structural",
  "evidence_draft_log_rejected",
  "domain_closed_denominator_rejected",
  "schema_duplicate_assignment_rejected",
  "schema_duplicate_table_rejected",
  "schema_duplicate_nested_payload_rejected",
  "schema_campaign_selection_binding_keys_forbidden",
  "noncanonical_comment",
  "noncanonical_inline_table",
  "systematic_illegal_cell_rejected",
  "systematic_unmodelled_coordinate_not_cell",
  "campaign_inline_legacy_binding_rejected",
  "schema_campaign_slug_grammars",
  "schema_observed_at_civil_date",
  "schema_held_permanent_rejected",
  "schema_due_on_valid_through_postures",
];

const SCHEMA_CASE_POLARITY = new Map<RequiredSchemaSelfTestCaseId, "positive" | "negative">([
  ...POSITIVE_SCHEMA_CASE_IDS.map((id) => [id, "positive"] as const),
  ...NEGATIVE_SCHEMA_CASE_IDS.map((id) => [id, "negative"] as const),
]);
assert(POSITIVE_SCHEMA_CASE_IDS.length + NEGATIVE_SCHEMA_CASE_IDS.length === REQUIRED_SCHEMA_SELFTEST_CASE_IDS.length, "schema case polarity metadata must declare each ID once");
assert(SCHEMA_CASE_POLARITY.size === REQUIRED_SCHEMA_SELFTEST_CASE_IDS.length, "schema case polarity metadata must cover each ID exactly once");
for (const id of REQUIRED_SCHEMA_SELFTEST_CASE_IDS) assert(SCHEMA_CASE_POLARITY.has(id), `missing explicit polarity for ${id}`);

export const SCHEMA_SELFTEST_CASES: readonly SelfTestCase[] = REQUIRED_SCHEMA_SELFTEST_CASE_IDS.map((id) => ({
  id,
  category: id === "domain_state_required_forbidden" || id.includes("matrix")
    ? "domain-matrix" as const
    : id === "domain_defect_regression_required" || id.includes("testing")
    ? "domain-testing" as const
    : id.includes("v0")
    ? "schema-v0" as const
    : "schema-v1" as const,
  run(context): SelfTestResult {
    const polarity = SCHEMA_CASE_POLARITY.get(id)!;
    try {
      execute(id, context);
      return {
        ok: true,
        polarity,
        ...(id === "v2_migration_escape_hatches_rejected"
          ? { subcases: [
            "semantic_conversion", "frozen_legacy_span_ids", "raw_section", "raw_fragment",
            "raw_legacy_marker", "raw_record", "raw_part", "semantic_owner_raw_fields", "raw_span",
          ] }
          : {}),
      };
    }
    catch (error) { return { ok: false, polarity, issues: [failure(id, error)] }; }
  },
}));

const FIXTURE_REQUIRED_SCHEMA_CASE_IDS = new Set<RequiredSchemaSelfTestCaseId>([
  "strict_unknown_every_table",
  "v0_all_fields_identity",
  "v1_all_fields_identity",
  "campaign_all_fields_identity",
  "retired_all_fields_identity",
  "v1_raw_shadow_nonrendering",
  "domain_matrix_all_tags",
  "domain_testing_all_tags",
  "domain_transition_each_kind",
  "domain_fired_transition_not_parked",
  "domain_already_met_signal_rejected",
  "evidence_generator_requires_harness_free",
  "evidence_timing_join_structural",
  "evidence_draft_log_rejected",
  "schema_v0_exact_keys_every_table",
  "schema_v1_exact_keys_every_structural_arm",
  "schema_shared_payload_exact_keys_every_arm",
  "schema_matrix_payload_exact_keys_every_arm",
  "schema_testing_payload_exact_keys_every_arm",
  "schema_systematic_exact_keys_every_arm",
  "schema_reference_exact_keys_every_arm",
  "schema_campaign_retired_exact_keys_every_arm",
  "schema_canonical_key_order_every_arm",
  "schema_campaign_authority_keys",
  "schema_campaign_reservation_exact_keys",
  "schema_campaign_selection_binding_keys_forbidden",
  "campaign_inline_legacy_binding_rejected",
  "schema_priority_band_closed_enum",
  "systematic_unmodelled_coordinate_not_cell",
]);

export function runSchemaDirectSelfTests(context?: SelfTestContext): { executed: number; counts: Readonly<Record<string, number>>; fixture_mutations?: SchemaFixtureMutationReceipt } {
  const counts: Record<string, number> = {};
  for (const id of REQUIRED_SCHEMA_SELFTEST_CASE_IDS) {
    if (context === undefined && FIXTURE_REQUIRED_SCHEMA_CASE_IDS.has(id)) continue;
    execute(id, context);
    counts[id] = (counts[id] ?? 0) + 1;
  }
  const expected = context === undefined ? REQUIRED_SCHEMA_SELFTEST_CASE_IDS.length - FIXTURE_REQUIRED_SCHEMA_CASE_IDS.size : REQUIRED_SCHEMA_SELFTEST_CASE_IDS.length;
  assert(Object.keys(counts).length === expected && Object.values(counts).every((count) => count === 1), "each executed schema case must run exactly once");
  return context === undefined
    ? { executed: expected, counts }
    : { executed: expected, counts, fixture_mutations: fixtureMutationProof(context).receipt };
}
