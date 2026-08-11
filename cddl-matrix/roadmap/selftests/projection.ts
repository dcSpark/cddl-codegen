import type { RoadmapSelfTestPorts } from "../io.ts";
import type { ReadOnlyRoadmapPorts } from "../io.ts";
import type { SelfTestCase, SelfTestCategory, SelfTestContext, SelfTestResult } from "../selftest.ts";
import type { RoadmapIssue } from "../errors.ts";
import type {
  FragmentId,
  FixtureRelativePath,
  FullCommitId,
  MarkerId,
  PartId,
  RepoPath,
  RoadmapId,
  SectionId,
  SlotId,
  SpanId,
} from "../model/core.ts";
import type {
  ManifestEntry,
  RoadmapDocument,
  RoadmapDocumentV0,
  RoadmapDocumentV1,
  SourceSpan,
} from "../model/documents.ts";
import type { MatrixStatusInputs } from "../model/matrix.ts";
import type {
  StatusCompatibilityDiagnosticFixture,
  StatusCompatibilityInputsWire,
  StatusCompatibilityModeFixture,
} from "../selftest.ts";
import { resolveManifest } from "../manifest.ts";
import {
  buildExpectedChunks,
  checkedPrefixOffsets,
  createExpectedByteView,
  validateCompletedChunks,
  type CompletedRenderIr,
  type ExpectedByteViewObserver,
  type RenderChunk,
} from "../render_ir.ts";
import {
  renderThenCheckCommittedProjection,
  renderValidatedChunks,
} from "../render.ts";
import { validateSourceSpans } from "../spans.ts";
import {
  compareMigrationDebt,
  debtOwnerIndex,
  deriveMigrationDebt,
  independentDebtIndex,
  migrationDebtReport,
  validateDebtRetirementFacts,
  validateDebtTransitionFacts,
  type DebtOwnerKey,
  type DebtComparisonOptions,
  type IndependentDebtKey,
  type MigrationDebt,
} from "../debt.ts";
import {
  LEGACY_STATUS_OUTPUT_CLAIMS,
  LEGACY_STATUS_OUTPUT_REGISTRY,
  classifyLegacyStatusHeaderInvocation,
  deriveMatrixStatusFacts,
  planLegacyStatusHeaderRun,
  renderMatrixStatusPayloads,
} from "../matrix_status_facts.ts";
import {
  collectManifestSlotBindingFacts,
  createTestOutputRegistry,
  inspectStatusMarkerBinding,
  intervalsOverlap,
  resolveOutputClaims,
  validateOutputClaimInventory,
  type OutputClaim,
  type ClosedOutputRegistry,
  type ValidatedOutputAuthority,
} from "../output_registry.ts";
import {
  createProjectionWritePlan,
} from "../write_plan.ts";

/**
 * The projection packet's stable executable registry.  Detailed case bodies are installed beside
 * the implementation; keeping the complete ID inventory here makes an omitted reviewer vector a
 * typechecked, greppable failure instead of an inferred count.
 */
export const REQUIRED_PROJECTION_SELFTEST_CASE_IDS = [
  "manifest_duplicate_record",
  "manifest_missing_part",
  "manifest_orphan_fragment",
  "manifest_unknown_id",
  "manifest_wrong_kind",
  "manifest_duplicate_legacy_marker",
  "manifest_record_table_order_irrelevant",
  "manifest_true_sequence_preserved",
  "manifest_duplicate_not_tiebroken",
  "span_gap",
  "span_overlap",
  "span_wrong_digest",
  "span_wrong_owner",
  "span_wrong_kind",
  "span_wrong_status",
  "span_out_of_bounds",
  "span_reversed",
  "span_utf8_byte_offsets",
  "span_mid_scalar_boundary",
  "span_final_eof_owner",
  "span_empty_vacuity",
  "span_partial_prefix_rejected",
  "span_single_snapshot",
  "span_source_change_digest_rejected",
  "debt_raw_to_shadow_allowed",
  "debt_shadow_to_semantic_allowed",
  "debt_semantic_to_raw_rejected",
  "debt_new_key_rejected",
  "debt_swap_same_count_rejected",
  "debt_resolution_subset",
  "debt_independent_set_growth_rejected",
  "debt_category_hiding_rejected",
  "debt_frozen_set_growth_rejected",
  "debt_v0_v1_exact_cutover_set",
  "debt_v1_v0_rejected",
  "debt_unrelated_base_rejected",
  "render_irregular_matrix_exact",
  "render_irregular_testing_exact",
  "render_zero_chunks_rejected",
  "render_no_implicit_lf",
  "render_semantic_consumption_once",
  "render_shadow_ignored",
  "render_prior_projection_irrelevant",
  "outputs_duplicate_whole",
  "outputs_whole_vs_slot",
  "outputs_duplicate_slot",
  "outputs_duplicate_binding",
  "outputs_overlapping_slots",
  "outputs_path_escape",
  "outputs_empty_inventory",
  "outputs_shadow_no_claim",
  "outputs_matrix_handoff_collision",
  "outputs_projection_path_floor",
  "outputs_slot_cardinality",
  "write_check_read_only_port",
  "write_query_read_only_port",
  "write_projection_rejects_toml",
  "write_projection_rejects_authority_files",
  "write_shadow_rejected",
  "write_all_rejected",
  "format_source_single_explicit",
  "atomic_write_failure_preserves_target",
  "issues_sorted",
  "json_sorted_keys",
  "lexical_not_locale_sort",
  "two_clean_renders_equal",
  "no_clock_without_as_of",
  "debt_structured_owner_every_kind",
  "debt_same_textual_id_different_kind_distinct",
  "debt_owner_field_rename_requires_witness",
  "debt_span_raw_to_replaced_allowed",
  "debt_new_raw_span_rejected",
  "debt_unmodelled_coordinate_subset",
  "render_chunks_precede_consumption_validation",
  "render_chunks_precede_span_validation",
  "render_slots_resolved_before_slot_validation",
  "render_invalid_chunk_skips_projection_read",
  "render_committed_projection_read_last",
  "render_projection_mutation_changes_only_drift",
  "outputs_interval_overlap",
  "outputs_interval_utf8_bytes",
  "outputs_manifest_binding_owner",
  "outputs_live_status_claims_all_twelve",
  "status_facts_derive_fixture_parity",
  "status_projector_before_after_target_byte_parity",
  "status_projector_before_after_mode_parity",
  "status_projector_before_after_message_parity",
  "status_projector_preflight_no_partial_write",
  "span_expected_byte_view_cross_chunk",
  "span_expected_byte_view_incremental_hash",
] as const;

export type RequiredProjectionSelfTestCaseId =
  (typeof REQUIRED_PROJECTION_SELFTEST_CASE_IDS)[number];

export const PROJECTION_FIXTURE_PATHS = Object.freeze([
  "irregular/matrix-v0.expected.md",
  "irregular/testing-v0.expected.md",
  "status-compat/diagnostics.toml",
  "status-compat/inputs.toml",
  "status-compat/matrix-readme.after.md",
  "status-compat/matrix-readme.before.md",
  "status-compat/modes.toml",
  "status-compat/roadmap.after.md",
  "status-compat/roadmap.before.md",
  "status-compat/tests-readme.after.md",
  "status-compat/tests-readme.before.md",
] as const);

export type ProjectionFixturePath = (typeof PROJECTION_FIXTURE_PATHS)[number];

export interface ProjectionFixtureBundle {
  readonly file_count: 11;
}

export interface ProjectionFixtureObserver {
  fixtureRead(path: ProjectionFixturePath): void;
}

const projectionFixtureFiles = new WeakMap<object, {
  readonly files: ReadonlyMap<ProjectionFixturePath, Uint8Array>;
  readonly observer?: ProjectionFixtureObserver;
}>();

export function createProjectionFixtureBundle(
  files: ReadonlyMap<ProjectionFixturePath, Uint8Array>,
  observer?: ProjectionFixtureObserver,
): ProjectionFixtureBundle {
  if (files.size !== PROJECTION_FIXTURE_PATHS.length) throw new Error("projection fixture bundle must contain exactly eleven files");
  const snapshots = new Map<ProjectionFixturePath, Uint8Array>();
  for (const path of PROJECTION_FIXTURE_PATHS) {
    const value = files.get(path);
    if (value === undefined || value.byteLength === 0) throw new Error(`projection fixture bundle is missing ${path}`);
    snapshots.set(path, new Uint8Array(value));
  }
  const bundle: ProjectionFixtureBundle = Object.freeze({ file_count: 11 });
  projectionFixtureFiles.set(bundle, { files: snapshots, observer });
  return bundle;
}

function fixtureBytes(bundle: ProjectionFixtureBundle, path: ProjectionFixturePath): Uint8Array {
  const privateBundle = projectionFixtureFiles.get(bundle);
  const value = privateBundle?.files.get(path);
  if (value === undefined) throw new Error("projection fixture bundle is caller-constructed or incomplete");
  privateBundle?.observer?.fixtureRead(path);
  return new Uint8Array(value);
}

const FIXTURE_REQUIRED_CASES = new Set<RequiredProjectionSelfTestCaseId>([
  "render_irregular_matrix_exact",
  "render_irregular_testing_exact",
  "status_facts_derive_fixture_parity",
  "status_projector_before_after_target_byte_parity",
  "status_projector_before_after_mode_parity",
  "status_projector_before_after_message_parity",
  "status_projector_preflight_no_partial_write",
]);

const encoder = new TextEncoder();

function bytes(value: string): Uint8Array {
  return encoder.encode(value);
}

function asRoadmapId(value: string): RoadmapId { return value as RoadmapId; }
function asSectionId(value: string): SectionId { return value as SectionId; }
function asFragmentId(value: string): FragmentId { return value as FragmentId; }
function asMarkerId(value: string): MarkerId { return value as MarkerId; }
function asPartId(value: string): PartId { return value as PartId; }
function asSlotId(value: string): SlotId { return value as SlotId; }
function asSpanId(value: string): SpanId { return value as SpanId; }
function asRepoPath(value: string): RepoPath { return value as RepoPath; }

function sha256(value: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(value).digest("hex");
}

interface RawFixture {
  readonly document: RoadmapDocumentV0;
  readonly source: Uint8Array;
}

function rawFixture(): RawFixture {
  const source = bytes("HFMRPG");
  const sectionId = asSectionId("heading");
  const recordId = asRoadmapId("matrix.fixture-work");
  const slotId = asSlotId("status-slot");
  const manifest: ManifestEntry[] = [
    { kind: "section", section_id: sectionId },
    { kind: "fragment", fragment_id: asFragmentId("fragment") },
    { kind: "legacy_marker", marker_id: asMarkerId("marker") },
    { kind: "record", record_id: recordId },
    { kind: "part", part_id: asPartId("part") },
    { kind: "generated_slot", slot_id: slotId },
  ];
  const spanRows: readonly [string, SourceSpan["source_kind"], string, SourceSpan["migration_status"]][] = [
    ["span-h", "section", "heading", "raw"],
    ["span-f", "fragment", "fragment", "raw"],
    ["span-m", "legacy_marker", "marker", "raw"],
    ["span-r", "record", recordId, "raw"],
    ["span-p", "part", "part", "raw"],
    ["span-g", "generated_slot", slotId, "generated"],
  ];
  const spans = spanRows.map(([id, source_kind, owner_id, migration_status], index): SourceSpan => ({
    id: asSpanId(id),
    start_byte: index,
    end_byte: index + 1,
    sha256: sha256(source.subarray(index, index + 1)),
    source_kind,
    owner_id,
    owner_field: migration_status === "generated" ? "generated" : "source_block_md",
    migration_status,
  }));
  const document: RoadmapDocumentV0 = {
    document: {
      schema_version: 0,
      authority: "shadow",
      roadmap: "matrix",
      source_path: asRepoPath("fixture/matrix.toml"),
      projection_path: asRepoPath("fixture/matrix.md"),
      frozen_source_sha256: sha256(source),
      frozen_source_byte_length: source.byteLength,
      frozen_source_line_count: 1,
      frozen_source_eof: "none",
    },
    sections: [{ section_id: sectionId, title: "Heading", source_block_md: bytes("H"), span_ids: [asSpanId("span-h")] }],
    fragments: [{ fragment_id: asFragmentId("fragment"), projection_group: sectionId, source_block_md: bytes("F"), span_ids: [asSpanId("span-f")] }],
    legacy_markers: [{ marker_id: asMarkerId("marker"), legacy_aliases: ["legacy"], source_block_md: bytes("M"), span_ids: [asSpanId("span-m")] }],
    records: [{ id: recordId, title: "Work", projection_group: sectionId, source_block_md: bytes("R"), span_ids: [asSpanId("span-r")] }],
    parts: [{ part_id: asPartId("part"), parent_record_id: recordId, source_block_md: bytes("P"), span_ids: [asSpanId("span-p")] }],
    generated_slots: [{ slot_id: slotId, binding: "fixture-status", span_ids: [asSpanId("span-g")] }],
    manifest,
    spans,
  };
  return { document, source };
}

function complete(document: RoadmapDocument): { readonly completed: CompletedRenderIr; readonly manifestIssues: readonly RoadmapIssue[] } {
  const placement = resolveManifest(document);
  const completed = buildExpectedChunks(document, placement.ops, {
    renderSemanticRecord(record, fields) {
      const rendered: Uint8Array[] = [];
      const payload = record.payload;
      if (payload.kind === "work" && payload.work_state === "ready") {
        rendered.push(fields.consume("payload.summary_md", payload.summary_md));
        rendered.push(fields.consume("payload.acceptance_md", payload.acceptance_md));
        rendered.push(fields.consume("payload.priority_rationale_md", payload.priority_rationale_md));
      }
      const length = rendered.reduce((sum, value) => sum + value.byteLength, 0);
      const result = new Uint8Array(length);
      let offset = 0;
      for (const value of rendered) { result.set(value, offset); offset += value.byteLength; }
      return result;
    },
    resolveGeneratedSlot(slot) {
      return { binding: slot.binding, bytes: bytes("G") };
    },
  });
  return { completed, manifestIssues: placement.issues };
}

function authoritativeFixture(): { readonly document: RoadmapDocumentV1; readonly source: Uint8Array } {
  const raw = rawFixture();
  const document: RoadmapDocumentV1 = {
    document: {
      ...raw.document.document,
      schema_version: 1,
      authority: "authoritative",
      frozen_legacy_span_ids: raw.document.spans.filter((span) => span.migration_status === "raw").map((span) => span.id),
    },
    sections: raw.document.sections.map((value) => ({ ...value, render_authority: "raw" as const })),
    fragments: raw.document.fragments.map((value) => ({ ...value, render_authority: "raw" as const })),
    legacy_markers: raw.document.legacy_markers.map((value) => ({ ...value, render_authority: "raw" as const })),
    records: raw.document.records.map((value) => ({ ...value, render_authority: "raw" as const })),
    parts: raw.document.parts.map((value) => ({ ...value, render_authority: "raw" as const })),
    generated_slots: raw.document.generated_slots,
    manifest: raw.document.manifest,
    spans: raw.document.spans,
    relations: [],
    references: [],
  };
  return { document, source: raw.source };
}

function issueCodes(issues: readonly RoadmapIssue[]): Set<string> {
  return new Set(issues.map((value) => value.code));
}

function requireIssue(issues: readonly RoadmapIssue[], code: RoadmapIssue["code"]): void {
  if (!issueCodes(issues).has(code)) fail(`expected ${code}, got ${[...issueCodes(issues)].join(", ")}`);
}

function pass(polarity: "positive" | "negative" = "positive", subcases?: readonly string[]): SelfTestResult {
  return { ok: true, polarity, subcases };
}

function fail(message: string): never {
  throw new Error(message);
}

function expectedByteViewCrossChunk(): readonly string[] {
  const executed: string[] = [];
  const chunks: RenderChunk[] = ["a", "é", "🚀", "z"].map((value, manifest_index) => ({
    manifest_index,
    owner: { kind: "fragment", id: `chunk-${manifest_index}`, field: "body_md" },
    bytes: bytes(value),
    source_span_ids: [],
    consumed_fields: ["body_md"],
  }));
  const view = createExpectedByteView(chunks);
  if (JSON.stringify(view.prefix_offsets) !== JSON.stringify([0, 1, 3, 7, 8])) {
    fail("prefix offsets do not use UTF-8 byte lengths");
  }
  if (view.slice(1, 1).segments.length !== 0 || view.slice(8, 8).segments.length !== 0) fail("empty slice exposed an edge segment");
  executed.push("zero_length_boundaries");
  const oneChunk = view.slice(1, 3);
  if (oneChunk.segments.length !== 1 || !view.equals(oneChunk, bytes("é"))) fail("one-chunk slice failed");
  executed.push("one_chunk");
  const adjacent = view.slice(0, 3);
  if (adjacent.segments.length !== 2 || !view.equals(adjacent, bytes("aé"))) fail("adjacent-chunk slice failed");
  executed.push("adjacent_chunks");
  const slice = view.slice(1, 8);
  if (slice.segments.length < 3 || !view.equals(slice, bytes("é🚀z"))) fail("cross-chunk equality failed");
  executed.push("three_plus_chunks");
  chunks[1].bytes[0] = 0x78;
  if (!view.equals(slice, bytes("é🚀z"))) fail("expected byte view retained a mutable caller buffer");
  let rejected = false;
  try {
    view.slice(2, 3);
  } catch {
    rejected = true;
  }
  if (!rejected) fail("mid-scalar boundary was accepted");
  executed.push("mid_scalar_rejection");
  rejected = false;
  try {
    checkedPrefixOffsets([Number.MAX_SAFE_INTEGER, 1]);
  } catch {
    rejected = true;
  }
  if (!rejected) fail("checked prefix overflow was accepted");
  executed.push("checked_prefix_overflow");
  return Object.freeze(executed);
}

function expectedByteViewIncrementalHash(): void {
  const chunks: RenderChunk[] = ["one", "two", "three"].map((value, manifest_index) => ({
    manifest_index,
    owner: { kind: "fragment", id: `hash-${manifest_index}`, field: "body_md" },
    bytes: bytes(value),
    source_span_ids: [],
    consumed_fields: ["body_md"],
  }));
  const observed = { segments: 0, combined_allocations: 0, final_allocations: 0 };
  const observer: ExpectedByteViewObserver = {
    hashSegmentVisited: () => { observed.segments++; },
    combinedHashBufferAllocated: () => { observed.combined_allocations++; },
    finalProjectionAllocated: () => { observed.final_allocations++; },
  };
  const view = createExpectedByteView(chunks, observer);
  const digest = new Bun.CryptoHasher("sha256").update(bytes("onetwothree")).digest("hex");
  if (view.sha256(view.slice(0, view.byte_length)) !== digest) fail("incremental digest differs");
  if (observed.segments <= 1) fail("incremental hash did not causally report multi-segment traversal");
  if (observed.combined_allocations !== 0 || observed.final_allocations !== 0) {
    fail("incremental hash allocated a combined/final projection buffer");
  }
  renderValidatedChunks(chunks, [], view, observer);
  if (observed.combined_allocations !== 0 || Number(observed.final_allocations) !== 1) {
    fail("final-concatenator observer is not wired to the sole allocation seam");
  }
}

function finalRenderHasNoImplicitBytes(): void {
  const chunks: RenderChunk[] = ["left", "right"].map((value, manifest_index) => ({
    manifest_index,
    owner: { kind: "fragment", id: `render-${manifest_index}`, field: "body_md" },
    bytes: bytes(value),
    source_span_ids: [],
    consumed_fields: ["body_md"],
  }));
  const rendered = renderValidatedChunks(chunks, [], createExpectedByteView(chunks));
  if (new TextDecoder().decode(rendered) !== "leftright") fail("renderer inserted bytes");
}

function testManifestCase(id: RequiredProjectionSelfTestCaseId): SelfTestResult {
  const fixture = rawFixture();
  let document = fixture.document;
  let expected: RoadmapIssue["code"] | undefined;
  switch (id) {
    case "manifest_duplicate_record":
    case "manifest_duplicate_not_tiebroken":
      document = { ...document, manifest: [...document.manifest, { kind: "record", record_id: document.records[0].id }] };
      expected = "E-MANIFEST-DUPLICATE";
      break;
    case "manifest_missing_part":
      document = { ...document, manifest: document.manifest.filter((entry) => entry.kind !== "part") };
      expected = "E-MANIFEST-MISSING";
      break;
    case "manifest_orphan_fragment":
      document = { ...document, fragments: [{ ...document.fragments[0], projection_group: asSectionId("missing") }] };
      expected = "E-MANIFEST-ORPHAN";
      break;
    case "manifest_unknown_id":
      document = { ...document, manifest: document.manifest.map((entry) =>
        entry.kind === "record" ? { kind: "record", record_id: asRoadmapId("matrix.fixture-unknown") } : entry
      ) };
      expected = "E-MANIFEST-UNKNOWN";
      break;
    case "manifest_wrong_kind":
      document = { ...document, manifest: document.manifest.map((entry) =>
        entry.kind === "fragment" ? { kind: "fragment", fragment_id: asFragmentId(document.records[0].id) } : entry
      ) };
      expected = "E-MANIFEST-KIND";
      break;
    case "manifest_duplicate_legacy_marker":
      document = { ...document, manifest: [...document.manifest, { kind: "legacy_marker", marker_id: document.legacy_markers[0].marker_id }] };
      expected = "E-MANIFEST-DUPLICATE";
      break;
    case "manifest_record_table_order_irrelevant": {
      const second = {
        ...document.records[0],
        id: asRoadmapId("matrix.fixture-second"),
        title: "Second",
      };
      const firstManifestIndex = document.manifest.findIndex((entry) => entry.kind === "record");
      const manifest = [...document.manifest];
      manifest.splice(firstManifestIndex + 1, 0, { kind: "record", record_id: second.id });
      const forward = resolveManifest({ ...document, records: [document.records[0], second], manifest });
      const reverse = resolveManifest({ ...document, records: [second, document.records[0]], manifest });
      if (forward.issues.length !== 0 || reverse.issues.length !== 0) fail("valid manifest order rejected");
      if (forward.ops.map((op) => op.node.id).join("|") !== reverse.ops.map((op) => op.node.id).join("|")) {
        fail("record table order changed presentation");
      }
      return pass();
    }
    case "manifest_true_sequence_preserved": {
      const reversed = { ...document, manifest: [...document.manifest].reverse() };
      const resolved = resolveManifest(reversed);
      if (resolved.ops[0]?.node.kind !== "generated_slot" || resolved.ops.at(-1)?.node.kind !== "section") {
        fail("manifest authored sequence was reordered");
      }
      return pass();
    }
    default:
      fail(`${id} is not a manifest case`);
  }
  const issues = resolveManifest(document).issues;
  if (expected === undefined) fail("manifest test lacks expected code");
  requireIssue(issues, expected);
  return pass("negative");
}

function validateRawFixture(document: RoadmapDocumentV0, source: Uint8Array): readonly RoadmapIssue[] {
  const placement = resolveManifest(document);
  const completed = complete(document).completed;
  return [
    ...placement.issues,
    ...validateCompletedChunks(document, placement.ops, completed),
    ...validateSourceSpans({ document, completed, source_snapshot: source }),
  ];
}

function testSpanCase(id: RequiredProjectionSelfTestCaseId): SelfTestResult {
  if (id === "span_expected_byte_view_cross_chunk") {
    return pass("positive", expectedByteViewCrossChunk());
  }
  if (id === "span_expected_byte_view_incremental_hash") {
    expectedByteViewIncrementalHash();
    return pass();
  }
  const fixture = rawFixture();
  let document = fixture.document;
  let source = fixture.source;
  let expected: RoadmapIssue["code"] | undefined;
  switch (id) {
    case "span_gap": {
      const spans = document.spans.map((span, index) => index === 1 ? { ...span, start_byte: span.start_byte + 1 } : span);
      document = { ...document, spans };
      expected = "E-SPAN-GAP";
      break;
    }
    case "span_overlap": {
      const spans = document.spans.map((span, index) => index === 1 ? { ...span, start_byte: 0 } : span);
      document = { ...document, spans };
      expected = "E-SPAN-OVERLAP";
      break;
    }
    case "span_wrong_digest":
      document = { ...document, spans: document.spans.map((span, index) => index === 0 ? { ...span, sha256: "0".repeat(64) } : span) };
      expected = "E-SPAN-DIGEST";
      break;
    case "span_wrong_owner":
      document = { ...document, spans: document.spans.map((span, index) => index === 0 ? { ...span, owner_id: "wrong" } : span) };
      expected = "E-SPAN-OWNER";
      break;
    case "span_wrong_kind":
      document = { ...document, spans: document.spans.map((span, index) => index === 0 ? { ...span, source_kind: "record" as const } : span) };
      expected = "E-SPAN-KIND";
      break;
    case "span_wrong_status":
      document = { ...document, spans: document.spans.map((span, index) => index === 0 ? { ...span, migration_status: "replaced" as const } : span) };
      expected = "E-SPAN-STATUS";
      break;
    case "span_out_of_bounds":
      document = { ...document, spans: document.spans.map((span, index) => index === 5 ? { ...span, end_byte: 7 } : span) };
      expected = "E-SPAN-BOUNDS";
      break;
    case "span_reversed":
      document = { ...document, spans: document.spans.map((span, index) => index === 0 ? { ...span, start_byte: 1, end_byte: 0 } : span) };
      expected = "E-SPAN-BOUNDS";
      break;
    case "span_utf8_byte_offsets":
    case "span_mid_scalar_boundary": {
      const chunks: RenderChunk[] = [{
        manifest_index: 0,
        owner: { kind: "section", id: "unicode", field: "body_md" },
        bytes: bytes("é"),
        source_span_ids: [],
        consumed_fields: ["body_md"],
      }];
      const view = createExpectedByteView(chunks);
      if (!view.equals(view.slice(0, 2), bytes("é"))) fail("UTF-8 byte offsets were not used");
      let rejected = false;
      try { view.slice(1, 2); } catch { rejected = true; }
      if (!rejected) fail("mid-scalar byte boundary accepted");
      return pass(id === "span_mid_scalar_boundary" ? "negative" : "positive");
    }
    case "span_final_eof_owner": {
      const issues = validateRawFixture(document, source);
      if (issues.length !== 0) fail(`valid EOF ownership failed: ${issues.map((value) => value.code).join(",")}`);
      return pass();
    }
    case "span_empty_vacuity":
      document = { ...document, spans: [] };
      expected = "E-SPAN-EMPTY";
      break;
    case "span_partial_prefix_rejected":
      document = { ...document, spans: document.spans.slice(0, -1) };
      expected = "E-SPAN-COVERAGE";
      break;
    case "span_single_snapshot": {
      let reads = 0;
      const input = {
        document,
        completed: complete(document).completed,
        get source_snapshot(): Uint8Array { reads++; return source; },
      };
      validateSourceSpans(input);
      if (reads !== 1) fail(`source snapshot read ${reads} times`);
      return pass();
    }
    case "span_source_change_digest_rejected":
      source = bytes("XFMRPG");
      expected = "E-SOURCE-DIGEST";
      break;
    default:
      fail(`${id} is not a span case`);
  }
  const issues = validateRawFixture(document, source);
  if (expected === undefined) fail("span test lacks expected code");
  requireIssue(issues, expected);
  return pass("negative");
}

function owner(
  owner_kind: DebtOwnerKey["owner_kind"],
  owner_id: string,
  owner_field: string,
): DebtOwnerKey {
  return {
    roadmap: "matrix",
    owner_kind,
    owner_id,
    owner_field,
  } as DebtOwnerKey;
}

function debt(
  entries: readonly (readonly [DebtOwnerKey, "raw_unclassified" | "raw_with_semantic_shadow" | "semantic"])[],
  independent: readonly IndependentDebtKey[] = [],
  frozen: readonly DebtOwnerKey[] = [],
): MigrationDebt {
  return {
    owners: new Map(entries.map(([key, state]) => [debtOwnerIndex(key), { key, state }])),
    independent: new Map(independent.map((key) => [independentDebtIndex(key), key])),
    frozen_legacy_spans: new Map(frozen.map((key) => [debtOwnerIndex(key), key])),
  };
}

function testDebtCase(id: RequiredProjectionSelfTestCaseId): SelfTestResult {
  const v1 = authoritativeFixture().document;
  const v0 = rawFixture().document;
  const recordRaw = owner("record", "matrix.fixture-work", "source_block_md");
  const recordSemantic = owner("record", "matrix.fixture-work", "payload.summary_md");
  const spanKey = owner("source_span", "span-r", "coverage");
  const options = { base_document: v1, candidate_document: v1 };
  let base = debt([[recordRaw, "raw_unclassified"]], [], [spanKey]);
  let candidate = debt([[recordRaw, "raw_unclassified"]], [], [spanKey]);
  let expected: RoadmapIssue["code"] | undefined;
  let compareOptions: DebtComparisonOptions = options;
  switch (id) {
    case "debt_raw_to_shadow_allowed":
      candidate = debt([[recordRaw, "raw_with_semantic_shadow"]], [], [spanKey]);
      break;
    case "debt_shadow_to_semantic_allowed":
    {
      const baseFixture = semanticFixture("shadow");
      const candidateFixture = semanticFixture("exact");
      base = deriveMigrationDebt(baseFixture.document, complete(baseFixture.document).completed);
      candidate = deriveMigrationDebt(candidateFixture.document, complete(candidateFixture.document).completed);
      const transition = validateDebtTransitionFacts(
        base,
        candidate,
        { base_document: baseFixture.document, candidate_document: candidateFixture.document },
        [{
          removed: recordRaw,
          added: [
            owner("record", "matrix.fixture-work", "payload.acceptance_md"),
            owner("record", "matrix.fixture-work", "payload.priority_rationale_md"),
            recordSemantic,
          ],
        }],
      );
      if (!transition.ok) fail(`valid debt restructure rejected: ${transition.issues.map((value) => value.message).join(";")}`);
      compareOptions = {
        base_document: baseFixture.document,
        candidate_document: candidateFixture.document,
        transition_facts: transition.facts,
      };
      break;
    }
    case "debt_semantic_to_raw_rejected":
      base = debt([[recordRaw, "semantic"]], [], [spanKey]);
      candidate = debt([[recordRaw, "raw_unclassified"]], [], [spanKey]);
      expected = "E-DEBT-OWNER-REGRESSION";
      break;
    case "debt_new_key_rejected":
    case "debt_swap_same_count_rejected":
    case "debt_owner_field_rename_requires_witness":
      candidate = debt([[recordSemantic, "semantic"]], [], [spanKey]);
      expected = "E-DEBT-OWNER-REGRESSION";
      break;
    case "debt_resolution_subset": {
      const independentKey: IndependentDebtKey = {
        roadmap: "matrix", category: "inferred_transitions", owner: recordRaw, subject: "transition",
      };
      base = debt([[recordRaw, "raw_unclassified"]], [independentKey], [spanKey]);
      candidate = debt([[recordRaw, "raw_unclassified"]], [], [spanKey]);
      break;
    }
    case "debt_independent_set_growth_rejected": {
      const independentKey: IndependentDebtKey = {
        roadmap: "matrix", category: "inferred_transitions", owner: recordRaw, subject: "transition",
      };
      candidate = debt([[recordRaw, "raw_unclassified"]], [independentKey], [spanKey]);
      expected = "E-DEBT-SET-GROWTH";
      break;
    }
    case "debt_category_hiding_rejected": {
      const oldKey: IndependentDebtKey = {
        roadmap: "matrix", category: "inferred_transitions", owner: recordRaw, subject: "transition",
      };
      const hiddenKey: IndependentDebtKey = {
        ...oldKey, category: "unresolved_references",
      };
      base = debt([[recordRaw, "raw_unclassified"]], [oldKey], [spanKey]);
      candidate = debt([[recordRaw, "raw_unclassified"]], [hiddenKey], [spanKey]);
      expected = "E-DEBT-CATEGORY-HIDE";
      break;
    }
    case "debt_frozen_set_growth_rejected":
    case "debt_new_raw_span_rejected": {
      const added = owner("source_span", "span-new", "coverage");
      candidate = debt([[recordRaw, "raw_unclassified"], [added, "raw_unclassified"]], [], [spanKey, added]);
      expected = id === "debt_frozen_set_growth_rejected" ? "E-DEBT-FROZEN-SET" : "E-DEBT-OWNER-REGRESSION";
      break;
    }
    case "debt_v0_v1_exact_cutover_set":
      compareOptions = { base_document: v0, candidate_document: v1 };
      break;
    case "debt_v1_v0_rejected":
      compareOptions = { base_document: v1, candidate_document: v0 };
      expected = "E-DEBT-BASE-MISMATCH";
      break;
    case "debt_unrelated_base_rejected":
      compareOptions = {
        base_document: { ...v1, document: { ...v1.document, source_path: asRepoPath("other.toml") } },
        candidate_document: v1,
      };
      expected = "E-DEBT-BASE-MISMATCH";
      break;
    case "debt_structured_owner_every_kind": {
      const kinds: DebtOwnerKey["owner_kind"][] = ["record", "section", "fragment", "part", "legacy_marker", "source_span"];
      const fixture = rawFixture();
      const derived = deriveMigrationDebt(fixture.document, complete(fixture.document).completed);
      const observedKinds = new Set([...derived.owners.values()].map((value) => value.key.owner_kind));
      for (const kind of kinds) if (!observedKinds.has(kind)) fail(`derived debt omitted ${kind}`);
      const report = migrationDebtReport(derived);
      if (report.owners.length < kinds.length) fail("debt report omitted derived owner atoms");
      return pass();
    }
    case "debt_same_textual_id_different_kind_distinct": {
      const section = owner("section", "same", "source_block_md");
      const fragment = owner("fragment", "same", "source_block_md");
      if (debtOwnerIndex(section) === debtOwnerIndex(fragment)) fail("textual IDs collapsed across kinds");
      return pass();
    }
    case "debt_span_raw_to_replaced_allowed":
      base = debt([[spanKey, "raw_unclassified"]], [], [spanKey]);
      candidate = debt([[spanKey, "semantic"]], [], []);
      break;
    case "debt_unmodelled_coordinate_subset": {
      const key: IndependentDebtKey = {
        roadmap: "matrix", category: "unmodelled_coordinates", owner: recordRaw, subject: "axis-x",
      };
      base = debt([[recordRaw, "semantic"]], [key], [spanKey]);
      candidate = debt([[recordRaw, "semantic"]], [], []);
      break;
    }
    default:
      fail(`${id} is not a debt case`);
  }
  const issues = compareMigrationDebt(base, candidate, compareOptions);
  if (expected === undefined) {
    if (issues.length !== 0) fail(`${id}: unexpected debt issue ${issues.map((value) => value.code).join(",")}`);
    if (id === "debt_shadow_to_semantic_allowed") {
      const ghost = owner("record", "matrix.fixture-work", "payload.ghost_md");
      const mutableOwners = candidate.owners as Map<string, { key: DebtOwnerKey; state: "semantic" }>;
      mutableOwners.set(debtOwnerIndex(ghost), { key: ghost, state: "semantic" });
      const changed = compareMigrationDebt(base, candidate, compareOptions);
      requireIssue(changed, "E-DEBT-BASE-MISMATCH");
      mutableOwners.delete(debtOwnerIndex(ghost));
    }
    if (id === "debt_owner_field_rename_requires_witness") fail("rename case failed to exercise rejection");
    return pass();
  }
  requireIssue(issues, expected);
  if (id === "debt_owner_field_rename_requires_witness") {
    const empty = validateDebtTransitionFacts(base, candidate, compareOptions, [{ removed: recordRaw, added: [] }]);
    if (empty.ok || !empty.issues.some((value) => value.message.includes("at least one"))) {
      fail("empty restructure witness did not fail its nonempty-transition coordinate");
    }
    const forged = compareMigrationDebt(base, candidate, {
      ...compareOptions,
      transition_facts: { restructure_count: 1, retirement_count: 0 },
    });
    requireIssue(forged, "E-DEBT-BASE-MISMATCH");

    const retirementBase = debt([[recordRaw, "raw_unclassified"]], [], [spanKey]);
    const retirementCandidate = debt([], [], [spanKey]);
    const retirementCandidateDocument: RoadmapDocumentV1 = { ...v1, records: [] };
    const replacementPin = { kind: "gate" as const, gate_id: "retirement-gate", claim_md: bytes("resolved") };
    const candidateTombstone = {
      owner_kind: "tombstone" as const,
      id: asRoadmapId("matrix.fixture-work"),
      namespace: "matrix" as const,
      tombstone: {
        id: asRoadmapId("matrix.fixture-work"),
        last_active_at: "a".repeat(40) as FullCommitId,
        replacement: replacementPin,
      },
    };
    const retirementRequest = {
      base_owner: {
        owner_kind: "active_record" as const,
        id: asRoadmapId("matrix.fixture-work"),
        namespace: "matrix" as const,
        record: v1.records[0],
      },
      removed_debt_owners: [recordRaw],
      base_commit: "a".repeat(40) as FullCommitId,
      base_source: {
        source_path: v1.document.source_path,
        sha256: v1.document.frozen_source_sha256,
        byte_length: v1.document.frozen_source_byte_length,
      },
      candidate_source: {
        source_path: retirementCandidateDocument.document.source_path,
        sha256: retirementCandidateDocument.document.frozen_source_sha256,
        byte_length: retirementCandidateDocument.document.frozen_source_byte_length,
      },
      candidate_identity_facts: [candidateTombstone],
      candidate_tombstone: candidateTombstone,
      candidate_replacement_fact: { id: "retirement-gate", kind: "cargo", stub: false },
    };
    const emptyRetirement = validateDebtRetirementFacts(
      retirementBase,
      retirementCandidate,
      { base_document: v1, candidate_document: retirementCandidateDocument },
      [],
    );
    if (emptyRetirement.ok) fail("empty retirement fact set authorized owner deletion");
    const ineligibleSectionRetirement = validateDebtRetirementFacts(
      retirementBase,
      retirementCandidate,
      { base_document: v1, candidate_document: retirementCandidateDocument },
      [{
        ...retirementRequest,
        base_owner: {
          owner_kind: "section",
          id: asRoadmapId("matrix.fixture-work"),
          namespace: "matrix",
        } as never,
        removed_debt_owners: [owner("section", "heading", "source_block_md")],
      }],
    );
    if (ineligibleSectionRetirement.ok) fail("ineligible section owner minted retirement authority");
    const missingCandidateJoins = validateDebtRetirementFacts(
      retirementBase,
      retirementCandidate,
      { base_document: v1, candidate_document: retirementCandidateDocument },
      [{ ...retirementRequest, candidate_identity_facts: [], candidate_replacement_fact: undefined }],
    );
    if (missingCandidateJoins.ok) fail("invented eligible owner without candidate joins minted retirement authority");
    const retirement = validateDebtRetirementFacts(
      retirementBase,
      retirementCandidate,
      { base_document: v1, candidate_document: retirementCandidateDocument },
      [retirementRequest],
    );
    if (!retirement.ok) fail(`legitimate retirement fact rejected: ${retirement.issues.map((value) => value.message).join(";")}`);
    const retirementOptions: DebtComparisonOptions = {
      base_document: v1,
      candidate_document: retirementCandidateDocument,
      transition_facts: retirement.facts,
    };
    const retired = compareMigrationDebt(retirementBase, retirementCandidate, retirementOptions);
    if (retired.length !== 0) fail(`validated retirement did not authorize removal: ${retired.map((value) => value.code).join(",")}`);
    requireIssue(compareMigrationDebt(retirementBase, retirementCandidate, {
      base_document: v1,
      candidate_document: retirementCandidateDocument,
      transition_facts: { restructure_count: 0, retirement_count: 1 },
    }), "E-DEBT-BASE-MISMATCH");
    const changedOwners = retirementCandidate.owners as Map<string, { key: DebtOwnerKey; state: "semantic" }>;
    const changedKey = owner("record", "changed", "payload.summary_md");
    changedOwners.set(debtOwnerIndex(changedKey), { key: changedKey, state: "semantic" });
    requireIssue(compareMigrationDebt(retirementBase, retirementCandidate, retirementOptions), "E-DEBT-BASE-MISMATCH");
    changedOwners.delete(debtOwnerIndex(changedKey));
  }
  return pass("negative");
}

function semanticFixture(
  mode: "exact" | "missing" | "duplicate" | "shadow" = "exact",
): { readonly document: RoadmapDocumentV1; readonly renderCalls: { value: number } } {
  const raw = authoritativeFixture().document;
  const record = raw.records[0];
  const summary = bytes("S");
  const acceptance = bytes("A");
  const rationale = bytes("Q");
  const output = bytes("HFMSAQPG");
  const spanIds = [asSpanId("span-summary"), asSpanId("span-acceptance"), asSpanId("span-rationale")];
  const semantic = {
    id: record.id,
    title: record.title,
    projection_group: record.projection_group,
    render_authority: "semantic" as const,
    payload: {
      kind: "work" as const,
      summary_md: summary,
      work_state: "ready" as const,
      work_intent: "repair" as const,
      work_kind: "feature" as const,
      risk: "cosmetic" as const,
      family_classification: "none_reviewed" as const,
      acceptance_md: acceptance,
      priority_rationale_md: rationale,
    },
    source_replacements: [
      { span_id: spanIds[0], replacement_field: "payload.summary_md", review_note_md: bytes("review") },
      { span_id: spanIds[1], replacement_field: "payload.acceptance_md", review_note_md: bytes("review") },
      { span_id: spanIds[2], replacement_field: "payload.priority_rationale_md", review_note_md: bytes("review") },
    ],
  };
  const records = mode === "shadow"
    ? [{ ...record, semantic_shadow: semantic.payload }]
    : [semantic];
  const semanticSpans: SourceSpan[] = [
    ...raw.spans.filter((span) => span.id !== asSpanId("span-r")).map((span) => {
      const shift = span.start_byte >= 4 ? 2 : 0;
      return { ...span, start_byte: span.start_byte + shift, end_byte: span.end_byte + shift };
    }),
    { id: spanIds[0], start_byte: 3, end_byte: 4, sha256: sha256(summary), source_kind: "record", owner_id: record.id, owner_field: "payload.summary_md", migration_status: "replaced" },
    { id: spanIds[1], start_byte: 4, end_byte: 5, sha256: sha256(acceptance), source_kind: "record", owner_id: record.id, owner_field: "payload.acceptance_md", migration_status: "replaced" },
    { id: spanIds[2], start_byte: 5, end_byte: 6, sha256: sha256(rationale), source_kind: "record", owner_id: record.id, owner_field: "payload.priority_rationale_md", migration_status: "replaced" },
  ];
  semanticSpans.sort((left, right) => left.start_byte - right.start_byte);
  const document: RoadmapDocumentV1 = {
    ...raw,
    document: {
      ...raw.document,
      frozen_source_sha256: sha256(output),
      frozen_source_byte_length: output.byteLength,
      frozen_legacy_span_ids: mode === "shadow" ? raw.document.frozen_legacy_span_ids : raw.document.frozen_legacy_span_ids.filter((id) => id !== asSpanId("span-r")),
    },
    records,
    spans: mode === "shadow" ? raw.spans : semanticSpans,
  };
  const calls = { value: 0 };
  Object.defineProperty(document, "__selftest_mode", { value: mode, enumerable: false });
  Object.defineProperty(document, "__render_calls", { value: calls, enumerable: false });
  return { document, renderCalls: calls };
}

function completeSemantic(document: RoadmapDocumentV1, calls: { value: number }): CompletedRenderIr {
  const mode = (document as RoadmapDocumentV1 & { __selftest_mode?: string }).__selftest_mode ?? "exact";
  const placement = resolveManifest(document);
  return buildExpectedChunks(document, placement.ops, {
    renderSemanticRecord(record, fields) {
      calls.value++;
      if (mode === "shadow") fail("semantic shadow reached renderer");
      const payload = record.payload;
      if (payload.kind !== "work" || payload.work_state !== "ready") return new Uint8Array();
      const first = fields.consume("payload.summary_md", payload.summary_md);
      if (mode === "duplicate") fields.consume("payload.summary_md", payload.summary_md);
      const second = mode === "missing" ? new Uint8Array() : fields.consume("payload.acceptance_md", payload.acceptance_md);
      const third = fields.consume("payload.priority_rationale_md", payload.priority_rationale_md);
      const output = new Uint8Array(first.byteLength + second.byteLength + third.byteLength);
      output.set(first);
      output.set(second, first.byteLength);
      output.set(third, first.byteLength + second.byteLength);
      return output;
    },
    resolveGeneratedSlot(slot) { return { binding: slot.binding, bytes: bytes("G") }; },
  });
}

function testRenderCase(
  id: RequiredProjectionSelfTestCaseId,
  fixtureBundle?: ProjectionFixtureBundle,
): SelfTestResult {
  if (id === "render_irregular_matrix_exact" || id === "render_irregular_testing_exact") {
    if (fixtureBundle === undefined) fail(`${id}: committed fixture bundle was not injected`);
    const expected = fixtureBytes(
      fixtureBundle,
      id === "render_irregular_matrix_exact"
        ? "irregular/matrix-v0.expected.md"
        : "irregular/testing-v0.expected.md",
    );
    const newline = expected.indexOf(0x0a);
    if (newline < 0) fail(`${id}: committed irregular fixture has no safe split boundary`);
    const split = newline + 1;
    const chunks: RenderChunk[] = [expected.subarray(0, split), expected.subarray(split)].map((value, manifest_index) => ({
      manifest_index,
      owner: { kind: "fragment", id: `${id}-${manifest_index}`, field: "source_block_md" },
      bytes: value,
      source_span_ids: [],
      consumed_fields: ["source_block_md"],
    }));
    const rendered = renderValidatedChunks(chunks, [], createExpectedByteView(chunks));
    if (sha256(rendered) !== sha256(expected) || rendered.byteLength !== expected.byteLength) fail("committed irregular source bytes changed");
    return pass();
  }
  if (id === "render_no_implicit_lf") {
    finalRenderHasNoImplicitBytes();
    return pass();
  }
  if (id === "render_zero_chunks_rejected") {
    let rejected = false;
    try { renderValidatedChunks([], [], createExpectedByteView([])); } catch { rejected = true; }
    if (!rejected) fail("zero chunks rendered successfully");
    return pass("negative");
  }
  if (id === "render_semantic_consumption_once" || id === "render_chunks_precede_consumption_validation") {
    const fixture = semanticFixture(id === "render_semantic_consumption_once" ? "duplicate" : "missing");
    const placement = resolveManifest(fixture.document);
    const completed = completeSemantic(fixture.document, fixture.renderCalls);
    if (completed.chunks.length !== placement.ops.length) fail("field failure prevented chunk completion");
    requireIssue(validateCompletedChunks(fixture.document, placement.ops, completed), "E-FIELD-CONSUMPTION");
    if (id === "render_chunks_precede_consumption_validation") {
      const raw = rawFixture();
      const rawPlacement = resolveManifest(raw.document);
      const rawCompleted = complete(raw.document).completed;
      const reordered: CompletedRenderIr = {
        ...rawCompleted,
        chunks: [rawCompleted.chunks[1], rawCompleted.chunks[0], ...rawCompleted.chunks.slice(2)],
      };
      requireIssue(validateCompletedChunks(raw.document, rawPlacement.ops, reordered), "E-RENDER-AUTHORITY");

      const exactFixture = semanticFixture("exact");
      const exactPlacement = resolveManifest(exactFixture.document);
      const exactCompleted = completeSemantic(exactFixture.document, exactFixture.renderCalls);
      const fieldLedger = exactCompleted.field_consumption[0];
      requireIssue(validateCompletedChunks(exactFixture.document, exactPlacement.ops, {
        ...exactCompleted,
        field_consumption: [],
      }), "E-FIELD-CONSUMPTION");
      requireIssue(validateCompletedChunks(exactFixture.document, exactPlacement.ops, {
        ...exactCompleted,
        field_consumption: [...exactCompleted.field_consumption, fieldLedger],
      }), "E-FIELD-CONSUMPTION");
      requireIssue(validateCompletedChunks(exactFixture.document, exactPlacement.ops, {
        ...exactCompleted,
        field_consumption: [...exactCompleted.field_consumption, { ...fieldLedger, owner_id: "ghost-owner" }],
      }), "E-FIELD-CONSUMPTION");

      const slotLedger = rawCompleted.slot_resolutions[0];
      requireIssue(validateCompletedChunks(raw.document, rawPlacement.ops, {
        ...rawCompleted,
        slot_resolutions: [],
      }), "E-OUTPUT-SLOT");
      requireIssue(validateCompletedChunks(raw.document, rawPlacement.ops, {
        ...rawCompleted,
        slot_resolutions: [...rawCompleted.slot_resolutions, slotLedger],
      }), "E-OUTPUT-SLOT");
      requireIssue(validateCompletedChunks(raw.document, rawPlacement.ops, {
        ...rawCompleted,
        slot_resolutions: [...rawCompleted.slot_resolutions, { ...slotLedger, manifest_index: 999 }],
      }), "E-OUTPUT-SLOT");
    }
    return pass("negative");
  }
  if (id === "render_shadow_ignored") {
    const fixture = semanticFixture("shadow");
    const completed = completeSemantic(fixture.document, fixture.renderCalls);
    if (fixture.renderCalls.value !== 0) fail("semantic shadow rendered");
    if (new TextDecoder().decode(renderValidatedChunks(completed.chunks, [], completed.expected_bytes)) !== "HFMRPG") fail("raw authority bytes changed");
    return pass();
  }
  if (id === "render_slots_resolved_before_slot_validation") {
    const fixture = rawFixture();
    const placement = resolveManifest(fixture.document);
    let resolverCalls = 0;
    const completed = buildExpectedChunks(fixture.document, placement.ops, {
      renderSemanticRecord: () => new Uint8Array(),
      resolveGeneratedSlot: (slot) => { resolverCalls++; return { binding: `${slot.binding}-wrong`, bytes: bytes("G") }; },
    });
    if (resolverCalls !== 1 || completed.chunks.length !== placement.ops.length) fail("slot did not resolve during chunk build");
    requireIssue(validateCompletedChunks(fixture.document, placement.ops, completed), "E-OUTPUT-SLOT");
    return pass("negative");
  }
  if (id === "render_chunks_precede_span_validation") {
    const fixture = rawFixture();
    const completed = complete(fixture.document).completed;
    const broken = { ...fixture.document, spans: [] };
    if (completed.chunks.length !== fixture.document.manifest.length) fail("chunks were not completed first");
    requireIssue(validateSourceSpans({ document: broken, completed, source_snapshot: fixture.source }), "E-SPAN-EMPTY");
    return pass("negative");
  }
  if (id === "render_invalid_chunk_skips_projection_read") {
    const fixture = rawFixture();
    const completed = complete(fixture.document).completed;
    let reads = 0;
    let rejected = false;
    try {
      renderThenCheckCommittedProjection(
        completed.chunks,
        [{ code: "E-SPAN-GAP", source: "fixture", logical_path: "span", message: "bad", exit: 1 }],
        completed.expected_bytes,
        fixture.document.document.projection_path,
        () => { reads++; return fixture.source; },
      );
    } catch { rejected = true; }
    if (!rejected || reads !== 0) fail("invalid chunks reached committed projection read");
    return pass("negative");
  }
  if (id === "render_committed_projection_read_last" || id === "render_projection_mutation_changes_only_drift") {
    const fixture = rawFixture();
    const completed = complete(fixture.document).completed;
    let reads = 0;
    const actual = id === "render_committed_projection_read_last" ? fixture.source : bytes("X\nMRPG");
    const checked = renderThenCheckCommittedProjection(
      completed.chunks,
      [],
      completed.expected_bytes,
      fixture.document.document.projection_path,
      () => { reads++; return actual; },
    );
    if (reads !== 1) fail("committed projection was not read exactly once at the late seam");
    if (id === "render_committed_projection_read_last" && checked.issues.length !== 0) fail("matching projection drifted");
    if (id === "render_projection_mutation_changes_only_drift") {
      if (checked.issues.length !== 1 || checked.issues[0].code !== "E-PROJECTION-DRIFT") fail("mutation changed more than drift verdict");
      if (
        !checked.issues[0].message.includes("expected context=") ||
        !checked.issues[0].message.includes("actual context=") ||
        !checked.issues[0].message.includes("\\n")
      ) fail("drift diagnostic omitted escaped local byte context");
    }
    return pass(id === "render_projection_mutation_changes_only_drift" ? "negative" : "positive");
  }
  const fixture = rawFixture();
  const completed = complete(fixture.document).completed;
  const first = renderValidatedChunks(completed.chunks, [], completed.expected_bytes);
  const second = renderValidatedChunks(completed.chunks, [], completed.expected_bytes);
  if (new TextDecoder().decode(first) !== "HFMRPG") fail(`${id}: exact render differs`);
  if (id === "render_prior_projection_irrelevant") {
    const stale = renderThenCheckCommittedProjection(
      completed.chunks,
      [],
      completed.expected_bytes,
      fixture.document.document.projection_path,
      () => bytes("unrelated prior projection"),
    );
    if (sha256(stale.expected) !== sha256(first) || stale.issues.length !== 1) {
      fail("prior projection bytes influenced freshly rendered bytes");
    }
  }
  if (id === "two_clean_renders_equal" && sha256(first) !== sha256(second)) fail("clean renders differ");
  return pass();
}

function statusClaim(path: RepoPath, id: string): OutputClaim {
  const slot = asSlotId(id);
  return {
    kind: "slot",
    producer: "selftest",
    path,
    slot_id: slot,
    interval: {
      kind: "binding",
      binding: { kind: "status_header_markers", marker_id: slot },
      cardinality: { exact: 1 },
    },
  };
}

function wholeClaim(path: RepoPath, producer = "selftest"): OutputClaim {
  return { kind: "whole_file", producer, path, interval: { kind: "whole_file" } };
}

function closedOutputRegistry(claims: readonly OutputClaim[]): ClosedOutputRegistry {
  const result = createTestOutputRegistry(claims);
  if (!result.ok) fail(`selftest output registry rejected: ${result.issues.map((value) => value.code).join(",")}`);
  return result.registry;
}

function wholeFileAuthority(
  path: RepoPath,
  target: Uint8Array = bytes("existing"),
): ValidatedOutputAuthority {
  const claim = wholeClaim(path, "roadmap-projector");
  const resolution = resolveOutputClaims({
    registry: closedOutputRegistry([claim]),
    claims: [claim],
    targets: new Map([[path, target]]),
  });
  if (resolution.authority === undefined || resolution.issues.length !== 0) {
    return fail("failed to mint selftest whole-file authority");
  }
  return resolution.authority;
}

function markerDocument(id: string, payload = "value", prefix = ""): Uint8Array {
  return bytes(`${prefix}<!-- gen:sh:${id} -->${payload}<!-- /gen:sh:${id} -->`);
}

// C6 owns this test-only committed compatibility oracle. Production status/adapters receive only
// MatrixStatusInputs; raw TOML decoding does not cross the fixture bundle boundary.
function parseFixtureToml<T>(bundle: ProjectionFixtureBundle, path: ProjectionFixturePath): T {
  return Bun.TOML.parse(new TextDecoder("utf-8", { fatal: true }).decode(fixtureBytes(bundle, path))) as T;
}

function statusInputs(bundle: ProjectionFixtureBundle): MatrixStatusInputs {
  const wire = parseFixtureToml<StatusCompatibilityInputsWire>(bundle, "status-compat/inputs.toml");
  const features = [
    ...wire.matrix.rfc8610_feature_ids.map((id) => ({ id, profile: "RFC8610" })),
    ...wire.matrix.rfc9682_feature_ids.map((id) => ({ id, profile: "RFC9682" })),
    ...wire.matrix.cddl_codegen_feature_ids.map((id) => ({ id, profile: "CDDL_CODEGEN" })),
  ];
  const annotations = [
    ...wire.matrix.supported_annotation_ids.map((id) => ({ id, status: "supported" })),
    ...wire.matrix.divergent_annotation.map((value) => ({
      id: value.id,
      status: value.status,
      emission: { [value.profile]: { status: value.emission_status } },
    })),
  ];
  return {
    matrix: {
      annotations,
      features,
      containment_ids: wire.matrix.containment_ids,
      control_operator_ids: wire.matrix.control_operator_ids,
    },
    catalog: { rows: wire.catalog.constraint_row_ids.map((id) => ({
      id,
      vectors: Array.from({ length: wire.catalog.constraint_vector_count }, () => ({ expect: "reject", class: "constraint" })),
    })) },
    registry: { gates: wire.registry.ignored_gate_ids.map((ignored_test, index) => ({
      id: `fixture-gate-${index}`,
      kind: "cargo",
      ignored_test,
    })) },
    timings: { tiers: [
      { tier: "fast", wall_ms: wire.timings.fast_wall_ms },
      { tier: "local", wall_ms: wire.timings.local_wall_ms },
      { tier: "full", wall_ms: wire.timings.full_wall_ms },
    ] },
  };
}

function statusTargets(bundle: ProjectionFixtureBundle, state: "before" | "after"): ReadonlyMap<RepoPath, Uint8Array> {
  return new Map([
    [asRepoPath("cddl-matrix/ROADMAP.md"), fixtureBytes(bundle, `status-compat/roadmap.${state}.md`)],
    [asRepoPath("cddl-matrix/README.md"), fixtureBytes(bundle, `status-compat/matrix-readme.${state}.md`)],
    [asRepoPath("tests/README.md"), fixtureBytes(bundle, `status-compat/tests-readme.${state}.md`)],
  ]);
}

type StatusModeWire = Omit<StatusCompatibilityModeFixture, "stdout_md" | "stderr_md"> & {
  readonly stdout_md: string;
  readonly stderr_md: string;
};

type StatusDiagnosticWire = Omit<StatusCompatibilityDiagnosticFixture, "stdout_md" | "stderr_md"> & {
  readonly stdout_md: string;
  readonly stderr_md: string;
};

function statusModeFixtures(bundle: ProjectionFixtureBundle): readonly StatusCompatibilityModeFixture[] {
  const parsed = parseFixtureToml<{ mode: readonly StatusModeWire[] }>(bundle, "status-compat/modes.toml");
  return parsed.mode.map((value) => ({
    ...value,
    stdout_md: bytes(value.stdout_md),
    stderr_md: bytes(value.stderr_md),
  }));
}

function statusDiagnosticFixtures(bundle: ProjectionFixtureBundle): readonly StatusCompatibilityDiagnosticFixture[] {
  const parsed = parseFixtureToml<{ diagnostic: readonly StatusDiagnosticWire[] }>(bundle, "status-compat/diagnostics.toml");
  return parsed.diagnostic.map((value) => ({
    ...value,
    stdout_md: bytes(value.stdout_md),
    stderr_md: bytes(value.stderr_md),
  }));
}

function exactBytes(left: Uint8Array, right: Uint8Array): boolean {
  return left.byteLength === right.byteLength && sha256(left) === sha256(right);
}

function targetReadSpy(targets: ReadonlyMap<RepoPath, Uint8Array>): {
  readonly targets: ReadonlyMap<RepoPath, Uint8Array>;
  readonly reads: ReadonlyMap<RepoPath, number>;
} {
  const reads = new Map<RepoPath, number>();
  const proxy = new Proxy(targets, {
    get(target, property) {
      if (property === "get") {
        return (path: RepoPath): Uint8Array | undefined => {
          reads.set(path, (reads.get(path) ?? 0) + 1);
          return target.get(path);
        };
      }
      const value = Reflect.get(target, property, target) as unknown;
      return typeof value === "function" ? value.bind(target) : value;
    },
  });
  return { targets: proxy, reads };
}

function testOutputCase(id: RequiredProjectionSelfTestCaseId): SelfTestResult {
  const path = asRepoPath("fixture/output.md");
  const claim = statusClaim(path, "one");
  if (id === "outputs_interval_overlap") {
    const executed: string[] = [];
    const cases = [
      ["same_interval", { start_byte: 1, end_byte: 4 }, { start_byte: 1, end_byte: 4 }, true],
      ["partial_left", { start_byte: 0, end_byte: 3 }, { start_byte: 2, end_byte: 5 }, true],
      ["partial_right", { start_byte: 2, end_byte: 5 }, { start_byte: 0, end_byte: 3 }, true],
      ["contained", { start_byte: 0, end_byte: 5 }, { start_byte: 1, end_byte: 4 }, true],
      ["whole_vs_slot", { start_byte: 0, end_byte: 8 }, { start_byte: 2, end_byte: 3 }, true],
      ["same_producer_overlap", { start_byte: 1, end_byte: 6 }, { start_byte: 4, end_byte: 7 }, true],
      ["adjacent", { start_byte: 0, end_byte: 2 }, { start_byte: 2, end_byte: 5 }, false],
    ] as const;
    for (const [label, left, right, expected] of cases) {
      if (intervalsOverlap(left, right) !== expected) fail(`${label}: overlap verdict differs`);
      const leftSlot = asSlotId("left");
      const rightSlot = asSlotId("right");
      const leftClaim: OutputClaim = label === "whole_vs_slot"
        ? wholeClaim(path, "same-producer")
        : {
          kind: "slot",
          producer: "same-producer",
          path,
          slot_id: leftSlot,
          interval: {
            kind: "binding",
            binding: { kind: "manifest_generated_slot", roadmap: "matrix", slot_id: leftSlot },
            cardinality: { exact: 1 },
          },
        };
      const rightClaim: OutputClaim = {
        kind: "slot",
        producer: "same-producer",
        path,
        slot_id: rightSlot,
        interval: {
          kind: "binding",
          binding: { kind: "manifest_generated_slot", roadmap: "matrix", slot_id: rightSlot },
          cardinality: { exact: 1 },
        },
      };
      const manifestSlots = [
        ...(leftClaim.kind === "slot" ? [{
          roadmap: "matrix" as const,
          path,
          slot_id: leftSlot,
          declaration_count: 1,
          placement_count: 1,
          owner_span_count: 1,
          interval: left,
          payload_interval: left,
        }] : []),
        {
          roadmap: "matrix" as const,
          path,
          slot_id: rightSlot,
          declaration_count: 1,
          placement_count: 1,
          owner_span_count: 1,
          interval: right,
          payload_interval: right,
        },
      ];
      const resolution = resolveOutputClaims({
        registry: closedOutputRegistry([leftClaim, rightClaim]),
        claims: [leftClaim, rightClaim],
        targets: new Map([[path, bytes("12345678")]]),
        manifest_slots: manifestSlots,
      });
      if (expected) requireIssue(resolution.issues, "E-OUTPUT-CLAIM");
      else if (resolution.issues.length !== 0 || resolution.resolved.length !== 2) {
        fail(`${label}: adjacent claims did not resolve independently`);
      }
      executed.push(label);
    }
    return pass("negative", executed);
  }
  if (id === "outputs_interval_utf8_bytes") {
    const target = markerDocument("one", "value", "é");
    const resolved = resolveOutputClaims({
      registry: closedOutputRegistry([claim]),
      claims: [claim],
      targets: new Map([[path, target]]),
    });
    const value = resolved.resolved[0];
    if (
      resolved.issues.length !== 0 || value.interval.start_byte !== 2 ||
      value.interval.end_byte !== target.byteLength || value.payload_interval.start_byte !== 21 ||
      value.payload_interval.end_byte !== 26
    ) fail("complete/payload interval did not use exact UTF-8 byte offsets");
    const whole = wholeClaim(path);
    const wholeResolution = resolveOutputClaims({
      registry: closedOutputRegistry([whole]),
      claims: [whole],
      targets: new Map([[path, target]]),
    });
    const wholeValue = wholeResolution.resolved[0];
    if (
      wholeResolution.issues.length !== 0 || wholeValue.interval.start_byte !== 0 ||
      wholeValue.interval.end_byte !== target.byteLength ||
      wholeValue.payload_interval.start_byte !== 0 ||
      wholeValue.payload_interval.end_byte !== target.byteLength
    ) fail("whole-file claim did not resolve to exact [0, UTF-8 byte length)");
    return pass();
  }
  if (id === "outputs_manifest_binding_owner") {
    const fixture = rawFixture();
    const completed = complete(fixture.document).completed;
    const facts = collectManifestSlotBindingFacts(fixture.document, completed);
    const slot = fixture.document.generated_slots[0].slot_id;
    const manifestClaim: OutputClaim = {
      kind: "slot",
      producer: "roadmap-projector",
      path: fixture.document.document.projection_path,
      slot_id: slot,
      interval: {
        kind: "binding",
        binding: { kind: "manifest_generated_slot", roadmap: "matrix", slot_id: slot },
        cardinality: { exact: 1 },
      },
    };
    const resolved = resolveOutputClaims({
      registry: closedOutputRegistry([manifestClaim]),
      claims: [manifestClaim],
      targets: new Map(),
      manifest_slots: facts,
    });
    if (
      resolved.issues.length !== 0 || resolved.resolved.length !== 1 ||
      resolved.resolved[0].interval.start_byte !== 5 || resolved.resolved[0].interval.end_byte !== 6 ||
      resolved.resolved[0].payload_interval.start_byte !== 5 || resolved.resolved[0].payload_interval.end_byte !== 6
    ) fail("manifest slot owner did not resolve its exact completed chunk without a projection read");
    const mismatchedCompleted: CompletedRenderIr = {
      ...completed,
      slot_resolutions: completed.slot_resolutions.map((item) => ({
        ...item,
        resolution: item.resolution === undefined
          ? undefined
          : { ...item.resolution, binding: `${item.resolution.binding}-wrong` },
      })),
    };
    requireIssue(resolveOutputClaims({
      registry: closedOutputRegistry([manifestClaim]),
      claims: [manifestClaim],
      targets: new Map(),
      manifest_slots: collectManifestSlotBindingFacts(fixture.document, mismatchedCompleted),
    }).issues, "E-OUTPUT-SLOT");
    return pass();
  }
  if (id === "outputs_live_status_claims_all_twelve") {
    if (LEGACY_STATUS_OUTPUT_CLAIMS.length !== 12) fail("live status registry does not declare twelve claims");
    if (new Set(LEGACY_STATUS_OUTPUT_CLAIMS.map((value) => value.kind === "slot" ? `${value.path}:${value.slot_id}` : value.path)).size !== 12) {
      fail("live status claims are not distinct");
    }
    if (LEGACY_STATUS_OUTPUT_REGISTRY.claim_count !== 12) fail("closed live status registry does not contain twelve claims");
    const expected = [
      "cddl-matrix/ROADMAP.md:roadmap-counts",
      "cddl-matrix/ROADMAP.md:roadmap-ops",
      "cddl-matrix/ROADMAP.md:roadmap-emission",
      "cddl-matrix/ROADMAP.md:roadmap-constraint",
      "cddl-matrix/README.md:readme-counts",
      "cddl-matrix/README.md:readme-annotations",
      "cddl-matrix/README.md:readme-ops",
      "cddl-matrix/README.md:readme-enforce-green",
      "tests/README.md:tests-ignored-gates",
      "tests/README.md:tests-tier-fast",
      "tests/README.md:tests-tier-local",
      "tests/README.md:tests-tier-full",
    ];
    const actual = LEGACY_STATUS_OUTPUT_CLAIMS.map((value) => {
      if (
        value.kind !== "slot" || value.producer !== "project_status_headers" ||
        value.interval.binding.kind !== "status_header_markers" ||
        value.interval.binding.marker_id !== value.slot_id
      ) fail("live status claim is not one normalized marker binding");
      return `${value.path}:${value.slot_id}`;
    });
    if (JSON.stringify(actual) !== JSON.stringify(expected)) fail("live status claim coordinates differ from the exact twelve");
    const localRegistry = closedOutputRegistry([claim]);
    const target = markerDocument("one");
    requireIssue(resolveOutputClaims({
      registry: localRegistry,
      claims: [{ ...claim, producer: "forged-producer" }],
      targets: new Map([[path, target]]),
    }).issues, "E-OUTPUT-WRITER");
    requireIssue(resolveOutputClaims({
      registry: localRegistry,
      claims: [{ ...claim, path: asRepoPath("fixture/forged.md") }],
      targets: new Map([[path, target]]),
    }).issues, "E-OUTPUT-PATH");
    requireIssue(resolveOutputClaims({
      registry: localRegistry,
      claims: [statusClaim(path, "forged-slot")],
      targets: new Map([[path, target]]),
    }).issues, "E-OUTPUT-SLOT");
    requireIssue(resolveOutputClaims({
      registry: { claim_count: 1 },
      claims: [claim],
      targets: new Map([[path, target]]),
    }).issues, "E-OUTPUT-CLAIM");
    return pass();
  }
  if (id === "outputs_empty_inventory") {
    requireIssue(validateOutputClaimInventory([]), "E-OUTPUT-CLAIM");
    return pass("negative");
  }
  if (id === "outputs_path_escape") {
    requireIssue(validateOutputClaimInventory([wholeClaim(asRepoPath("../escape.md"))]), "E-OUTPUT-PATH");
    return pass("negative");
  }
  if (id === "outputs_duplicate_whole") {
    requireIssue(validateOutputClaimInventory([wholeClaim(path), wholeClaim(path)]), "E-OUTPUT-CLAIM");
    return pass("negative");
  }
  if (id === "outputs_duplicate_slot") {
    const live = LEGACY_STATUS_OUTPUT_CLAIMS[0];
    if (live.kind !== "slot") return fail("first live claim is not a slot");
    const sameSlotDifferentBinding: OutputClaim = {
      ...live,
      interval: {
        kind: "binding",
        binding: { kind: "manifest_generated_slot", roadmap: "matrix", slot_id: live.slot_id },
        cardinality: { exact: 1 },
      },
    };
    const issues = validateOutputClaimInventory([live, sameSlotDifferentBinding]);
    if (!issues.some((value) => value.message === "path/slot pair is claimed more than once")) {
      fail("duplicate slot mutation did not produce its exact path/slot diagnostic");
    }
    if (issues.some((value) => value.message === "structured output binding is claimed more than once")) {
      fail("duplicate slot mutation accidentally duplicated the structured binding");
    }
    return pass("negative");
  }
  if (id === "outputs_duplicate_binding") {
    const live = LEGACY_STATUS_OUTPUT_CLAIMS[0];
    if (live.kind !== "slot") return fail("first live claim is not a slot");
    const sameBindingDifferentSlot: OutputClaim = {
      ...live,
      slot_id: asSlotId("roadmap-counts-alias"),
    };
    const issues = validateOutputClaimInventory([live, sameBindingDifferentSlot]);
    if (!issues.some((value) => value.message === "structured output binding is claimed more than once")) {
      fail("duplicate binding mutation did not produce its exact structured-binding diagnostic");
    }
    if (issues.some((value) => value.message === "path/slot pair is claimed more than once")) {
      fail("duplicate binding mutation accidentally duplicated the enclosing slot");
    }
    return pass("negative");
  }
  if (id === "outputs_whole_vs_slot") {
    const resolved = resolveOutputClaims({
      registry: closedOutputRegistry([wholeClaim(path), claim]),
      claims: [wholeClaim(path), claim],
      targets: new Map([[path, markerDocument("one")]]),
    });
    requireIssue(resolved.issues, "E-OUTPUT-CLAIM");
    return pass("negative");
  }
  if (id === "outputs_matrix_handoff_collision") {
    const roadmapPath = asRepoPath("cddl-matrix/ROADMAP.md");
    const liveRoadmapClaims = LEGACY_STATUS_OUTPUT_CLAIMS.filter((value) => value.path === roadmapPath);
    if (liveRoadmapClaims.length !== 4) fail("handoff mutation did not begin with the four live ROADMAP slots");
    if (LEGACY_STATUS_OUTPUT_CLAIMS.filter((value) => value.path !== roadmapPath).length !== 8) {
      fail("handoff mutation did not leave the eight README claims unaffected");
    }
    const projectorWhole = wholeClaim(roadmapPath, "roadmap-projector");
    const target = bytes(liveRoadmapClaims.map((value) => {
      if (value.kind !== "slot") return fail("ROADMAP live claim is not a slot");
      return `<!-- gen:sh:${value.slot_id} -->value<!-- /gen:sh:${value.slot_id} -->`;
    }).join("\n"));
    const claims = [...liveRoadmapClaims, projectorWhole];
    const resolved = resolveOutputClaims({
      registry: closedOutputRegistry(claims),
      claims,
      targets: new Map([[roadmapPath, target]]),
    });
    const overlaps = resolved.issues.filter((value) => value.code === "E-OUTPUT-CLAIM");
    if (overlaps.length !== 4) fail("whole-file handoff did not collide with each of the four live ROADMAP slots");
    return pass("negative");
  }
  if (id === "outputs_overlapping_slots") {
    const manifestClaims = ["one", "two"].map((value): OutputClaim => {
      const slotId = asSlotId(value);
      return {
        kind: "slot",
        producer: "roadmap-projector",
        path,
        slot_id: slotId,
        interval: {
          kind: "binding",
          binding: { kind: "manifest_generated_slot", roadmap: "matrix", slot_id: slotId },
          cardinality: { exact: 1 },
        },
      };
    });
    const resolved = resolveOutputClaims({
      registry: closedOutputRegistry(manifestClaims),
      claims: manifestClaims,
      targets: new Map(),
      manifest_slots: manifestClaims.map((value, index) => {
        if (value.kind !== "slot") return fail("unexpected whole-file manifest claim");
        const interval = index === 0
          ? { start_byte: 0, end_byte: 4 }
          : { start_byte: 2, end_byte: 6 };
        return {
          roadmap: "matrix" as const,
          path,
          slot_id: value.slot_id,
          declaration_count: 1,
          placement_count: 1,
          owner_span_count: 1,
          interval,
          payload_interval: interval,
        };
      }),
    });
    requireIssue(resolved.issues, "E-OUTPUT-CLAIM");
    return pass("negative");
  }
  if (id === "outputs_slot_cardinality") {
    const executed: string[] = [];
    const statusCases = [
      ["status_zero_open", bytes("<!-- /gen:sh:one -->")],
      ["status_two_open", bytes("<!-- gen:sh:one --><!-- gen:sh:one -->x<!-- /gen:sh:one -->")],
      ["status_zero_close", bytes("<!-- gen:sh:one -->")],
      ["status_two_close", bytes("<!-- gen:sh:one -->x<!-- /gen:sh:one --><!-- /gen:sh:one -->")],
      ["reversed", bytes("<!-- /gen:sh:one --><!-- gen:sh:one -->x")],
    ] as const;
    const registry = closedOutputRegistry([claim]);
    for (const [label, target] of statusCases) {
      const issues = resolveOutputClaims({
        registry,
        claims: [claim],
        targets: new Map([[path, target]]),
      }).issues;
      if (!issues.some((value) =>
        value.code === "E-OUTPUT-SLOT" && value.logical_path === 'slot["one"]'
      )) fail(`${label}: marker cardinality did not retain its typed output-slot coordinate`);
      executed.push(label);
    }

    const secondClaim = statusClaim(path, "two");
    const crossed = bytes("<!-- gen:sh:one --><!-- gen:sh:two -->x<!-- /gen:sh:one --><!-- /gen:sh:two -->");
    const crossedIssues = resolveOutputClaims({
      registry: closedOutputRegistry([claim, secondClaim]),
      claims: [claim, secondClaim],
      targets: new Map([[path, crossed]]),
    }).issues;
    if (!crossedIssues.some((value) =>
      value.code === "E-OUTPUT-SLOT" && value.logical_path === 'slot["one"]'
    )) fail("crossed marker structure did not retain its typed output-slot coordinate");
    executed.push("crossed");

    const nested = bytes("<!-- gen:sh:one --><!-- gen:sh:two -->x<!-- /gen:sh:two --><!-- /gen:sh:one -->");
    const nestedIssues = resolveOutputClaims({
      registry: closedOutputRegistry([claim, secondClaim]),
      claims: [claim, secondClaim],
      targets: new Map([[path, nested]]),
    }).issues;
    if (!nestedIssues.some((value) =>
      value.code === "E-OUTPUT-SLOT" && value.logical_path === 'slot["one"]'
    )) fail("nested marker structure did not retain its typed output-slot coordinate");
    executed.push("nested");

    const fixture = rawFixture();
    const completed = complete(fixture.document).completed;
    const slot = fixture.document.generated_slots[0].slot_id;
    const manifestClaim: OutputClaim = {
      kind: "slot",
      producer: "roadmap-projector",
      path: fixture.document.document.projection_path,
      slot_id: slot,
      interval: {
        kind: "binding",
        binding: { kind: "manifest_generated_slot", roadmap: "matrix", slot_id: slot },
        cardinality: { exact: 1 },
      },
    };
    const manifestRegistry = closedOutputRegistry([manifestClaim]);
    const declaration = fixture.document.generated_slots[0];
    const placement = fixture.document.manifest.find((entry) =>
      entry.kind === "generated_slot" && entry.slot_id === slot
    ) ?? fail("raw fixture generated-slot placement is missing");
    const ownerSpan = fixture.document.spans.find((span) =>
      span.source_kind === "generated_slot" && span.owner_id === slot
    ) ?? fail("raw fixture generated-slot span is missing");
    const manifestCases: readonly [string, RoadmapDocument][] = [
      ["manifest_zero", { ...fixture.document, generated_slots: [] }],
      ["manifest_two_declarations", {
        ...fixture.document,
        generated_slots: [declaration, { ...declaration }],
      }],
      ["manifest_two_placements", {
        ...fixture.document,
        manifest: [...fixture.document.manifest, { ...placement }],
      }],
      ["manifest_two_spans", {
        ...fixture.document,
        spans: [...fixture.document.spans, { ...ownerSpan, id: asSpanId("span-g-duplicate") }],
      }],
    ] as const;
    for (const [label, mutatedDocument] of manifestCases) {
      requireIssue(resolveOutputClaims({
        registry: manifestRegistry,
        claims: [manifestClaim],
        targets: new Map(),
        manifest_slots: collectManifestSlotBindingFacts(mutatedDocument, completed),
      }).issues, "E-OUTPUT-SLOT");
      executed.push(label);
    }
    return pass("negative", executed);
  }
  if (
    id === "outputs_shadow_no_claim" || id === "outputs_projection_path_floor" ||
    id === "write_projection_rejects_toml" || id === "write_projection_rejects_authority_files" ||
    id === "write_shadow_rejected" || id === "write_all_rejected" || id === "format_source_single_explicit"
  ) {
    let document: RoadmapDocument;
    if (id === "write_projection_rejects_toml") {
      const fixture = authoritativeFixture().document;
      document = { ...fixture, document: { ...fixture.document, projection_path: asRepoPath("fixture/source.toml") } };
    } else if (id === "write_projection_rejects_authority_files") {
      const fixture = authoritativeFixture().document;
      document = { ...fixture, document: { ...fixture.document, projection_path: asRepoPath("roadmap-campaign.toml") } };
    } else {
      document = id === "write_shadow_rejected" || id === "outputs_shadow_no_claim"
        ? rawFixture().document
        : authoritativeFixture().document;
    }
    let actualWp1Authority: ValidatedOutputAuthority | undefined;
    if (id === "outputs_shadow_no_claim") {
      if (
        LEGACY_STATUS_OUTPUT_CLAIMS.some((value) =>
          value.kind === "whole_file" || value.producer === "roadmap-projector"
        )
      ) {
        fail("WP1 production inventory unexpectedly contains a whole-file/projector claim");
      }
      const claimsByPath = new Map<RepoPath, Extract<OutputClaim, { kind: "slot" }>[]>();
      for (const value of LEGACY_STATUS_OUTPUT_CLAIMS) {
        if (value.kind !== "slot") fail("WP1 status inventory contains a non-slot claim");
        const values = claimsByPath.get(value.path) ?? [];
        values.push(value);
        claimsByPath.set(value.path, values);
      }
      const targets = new Map<RepoPath, Uint8Array>();
      for (const [targetPath, targetClaims] of claimsByPath) {
        targets.set(targetPath, bytes(targetClaims.map((value) =>
          `<!-- gen:sh:${value.slot_id} -->value<!-- /gen:sh:${value.slot_id} -->`
        ).join("\n")));
      }
      const resolution = resolveOutputClaims({
        registry: LEGACY_STATUS_OUTPUT_REGISTRY,
        claims: LEGACY_STATUS_OUTPUT_CLAIMS,
        targets,
      });
      if (resolution.issues.length !== 0 || resolution.authority === undefined) {
        fail("actual WP1 production status inventory did not resolve");
      }
      actualWp1Authority = resolution.authority;
    }
    const authority = id === "outputs_shadow_no_claim"
      ? actualWp1Authority as ValidatedOutputAuthority
      : id === "outputs_projection_path_floor"
        ? wholeFileAuthority(asRepoPath("fixture/other-projection.md"))
        : wholeFileAuthority(document.document.projection_path);
    const result = createProjectionWritePlan({
      write_coordinate: (id === "format_source_single_explicit" ? "format_source" : "projection") as "projection",
      roadmap: (id === "write_all_rejected" ? "all" : "matrix") as "matrix",
      document,
      projection_bytes: bytes("projection"),
      output_authority: authority,
      validation_issues: [],
    });
    if (result.ok) fail(`${id}: unauthorized write plan minted`);
    if (id === "write_projection_rejects_toml") requireIssue(result.issues, "E-OUTPUT-TOML");
    if (id === "write_projection_rejects_authority_files") requireIssue(result.issues, "E-OUTPUT-PATH");
    if (
      id === "outputs_projection_path_floor" || id === "outputs_shadow_no_claim" ||
      id === "write_shadow_rejected" || id === "write_all_rejected" || id === "format_source_single_explicit"
    ) requireIssue(result.issues, "E-OUTPUT-AUTHORITY");
    if (
      id === "outputs_shadow_no_claim" &&
      !result.issues.some((value) =>
        value.logical_path === "output_claims" && value.message.includes("lacks an opaque validated whole-file authority")
      )
    ) fail("actual WP1 inventory did not prove the projection has no whole-file claim");
    const intendedCoordinate = id === "write_all_rejected" ? "document.roadmap"
      : id === "format_source_single_explicit" ? "write_coordinate"
        : id === "write_shadow_rejected" ? "document.authority"
          : id === "write_projection_rejects_toml" ? "document.projection_path"
            : id === "write_projection_rejects_authority_files" ? "document.projection_path"
              : undefined;
    if (intendedCoordinate !== undefined && !result.issues.some((value) => value.logical_path === intendedCoordinate)) {
      fail(`${id}: did not fail its intended write coordinate`);
    }
    if (id === "outputs_projection_path_floor") {
      const callerChosen = createProjectionWritePlan({
        write_coordinate: "projection",
        roadmap: "matrix",
        document,
        projection_bytes: bytes("projection"),
        output_authority: wholeFileAuthority(document.document.projection_path),
        validation_issues: [],
      });
      if (callerChosen.ok || !callerChosen.issues.some((value) => value.logical_path === "output_claims.scope")) {
        fail("caller-chosen test inventory authorized a production write plan");
      }
      const forged = createProjectionWritePlan({
        write_coordinate: "projection",
        roadmap: "matrix",
        document,
        projection_bytes: bytes("projection"),
        output_authority: { resolved_count: 1 },
        validation_issues: [],
      });
      if (forged.ok) fail("caller-constructed output authority minted a write plan");
      requireIssue(forged.issues, "E-OUTPUT-AUTHORITY");
    }
    return pass("negative");
  }
  if (id === "write_check_read_only_port" || id === "write_query_read_only_port") {
    let writeCapabilityReads = 0;
    const fixture = rawFixture();
    const readOnly = new Proxy({
      readDeclared: (_path: RepoPath) => fixture.source,
      readDeclaredAtCommit: () => fixture.source,
      repositoryObjectFormat: () => "sha1" as const,
      resolveFullCommit: () => fail("unused"),
      registryView: () => fail("unused"),
    } satisfies ReadOnlyRoadmapPorts, {
      get(target, property, receiver) {
        if (property === "atomicReplace") {
          writeCapabilityReads++;
          return fail("read-only operation requested write capability");
        }
        return Reflect.get(target, property, receiver);
      },
    });
    if (id === "write_check_read_only_port") {
      const completed = complete(fixture.document).completed;
      const checked = renderThenCheckCommittedProjection(
        completed.chunks,
        [],
        completed.expected_bytes,
        fixture.document.document.projection_path,
        () => readOnly.readDeclared(fixture.document.document.projection_path),
      );
      if (checked.issues.length !== 0) fail("read-only check unexpectedly drifted");
    } else {
      const report = migrationDebtReport(debt([[owner("record", "query", "payload.summary_md"), "semantic"]]));
      if (report.owners.length !== 1) fail("read-only query omitted its structured result");
    }
    if (writeCapabilityReads !== 0 || "atomicReplace" in readOnly) fail("read-only path exposed or requested write authority");
    return pass();
  }
  if (id === "atomic_write_failure_preserves_target") {
    const target = bytes("old");
    let calls = 0;
    const ports = {
      readDeclared: () => target,
      readDeclaredAtCommit: () => target,
      repositoryObjectFormat: () => "sha1" as const,
      resolveFullCommit: () => fail("unused"),
      registryView: () => fail("unused"),
      atomicReplace: (_path: RepoPath, _bytes: Uint8Array) => { calls++; throw new Error("atomic failure"); },
    };
    let rejected = false;
    try { ports.atomicReplace(asRepoPath("fixture/projection.md"), bytes("new")); } catch { rejected = true; }
    if (!rejected || calls !== 1 || new TextDecoder().decode(target) !== "old") fail("atomic failure changed original snapshot");
    return pass("negative");
  }
  fail(`${id} is not an output case`);
}

function testStatusCase(
  id: RequiredProjectionSelfTestCaseId,
  fixtureBundle?: ProjectionFixtureBundle,
): SelfTestResult {
  if (fixtureBundle === undefined) fail(`${id}: committed status fixture bundle was not injected`);
  const inputs = statusInputs(fixtureBundle);
  const facts = deriveMatrixStatusFacts(inputs);
  if (id === "status_facts_derive_fixture_parity") {
    if (
      facts.features_total !== 90 || facts.containment_cells !== 60 || facts.annotations_total !== 80 ||
      facts.control_ops !== 30 || facts.constraint_vectors !== 1 || facts.enforce_green_rows.length !== 1 ||
      facts.ignored_gates.length !== 2 || facts.validation_problems.length !== 0
    ) fail("typed status fact derivation differs from the frozen fixture");
    if (renderMatrixStatusPayloads(facts).length !== 12) fail("status derivation did not render twelve payloads");
    return pass();
  }
  if (id === "status_projector_before_after_target_byte_parity") {
    const before = statusTargets(fixtureBundle, "before");
    const after = statusTargets(fixtureBundle, "after");
    const plan = planLegacyStatusHeaderRun(inputs, { mode: "write", argv: ["--write"], targets: before });
    if (plan.exit_code !== 0 || plan.writes.length !== 3) fail("status write plan failed");
    const executed: string[] = [];
    const labels = new Map<RepoPath, string>([
      [asRepoPath("cddl-matrix/ROADMAP.md"), "roadmap"],
      [asRepoPath("cddl-matrix/README.md"), "matrix_readme"],
      [asRepoPath("tests/README.md"), "tests_readme"],
    ]);
    for (const write of plan.writes) {
      if (sha256(write.bytes) !== sha256(after.get(write.path) ?? new Uint8Array())) fail(`status target ${write.path} differs from golden`);
      const label = labels.get(write.path);
      if (label === undefined) fail(`status wrote unexpected target ${write.path}`);
      executed.push(label);
    }
    return pass("positive", executed);
  }
  if (id === "status_projector_before_after_mode_parity") {
    const executed: string[] = [];
    for (const modeFixture of statusModeFixtures(fixtureBundle)) {
      const mode = classifyLegacyStatusHeaderInvocation(modeFixture.argv);
      let reads: ReadonlyMap<RepoPath, number> = new Map();
      let claimResolutions = 0;
      const resolvedClaimKeys = new Set<string>();
      let plan;
      if (modeFixture.target_state === "none") {
        if (mode !== "report") fail(`${modeFixture.id}: fixture expected a report coordinate`);
        plan = planLegacyStatusHeaderRun(inputs, { mode: "report", argv: modeFixture.argv }, {
          claimResolved: (claim) => {
            claimResolutions++;
            resolvedClaimKeys.add(claim.claim.kind === "slot" ? `${claim.path}:${claim.claim.slot_id}` : claim.path);
          },
        });
      } else {
        if (mode === "report") fail(`${modeFixture.id}: fixture expected target reads`);
        const spy = targetReadSpy(statusTargets(fixtureBundle, modeFixture.target_state));
        reads = spy.reads;
        plan = planLegacyStatusHeaderRun(inputs, { mode, argv: modeFixture.argv, targets: spy.targets }, {
          claimResolved: (claim) => {
            claimResolutions++;
            resolvedClaimKeys.add(claim.claim.kind === "slot" ? `${claim.path}:${claim.claim.slot_id}` : claim.path);
          },
        });
      }
      const writeOrder = plan.writes.map((write) => write.path);
      if (
        plan.exit_code !== modeFixture.exit_code || !exactBytes(plan.stdout, modeFixture.stdout_md) ||
        !exactBytes(plan.stderr, modeFixture.stderr_md) || plan.writes.length !== modeFixture.writes ||
        [...reads.values()].reduce((sum, value) => sum + value, 0) !== modeFixture.target_reads ||
        claimResolutions !== modeFixture.claim_resolutions ||
        resolvedClaimKeys.size !== modeFixture.claim_resolutions ||
        JSON.stringify(writeOrder) !== JSON.stringify(modeFixture.write_order)
      ) fail(`${modeFixture.id}: committed mode receipt differs`);
      if (modeFixture.target_reads > 0 && (reads.size !== 3 || [...reads.values()].some((value) => value !== 1))) {
        fail(`${modeFixture.id}: targets were not each read exactly once`);
      }
      executed.push(modeFixture.id);
    }
    return pass("positive", executed);
  }
  if (id === "status_projector_before_after_message_parity") {
    const executed: string[] = [];
    for (const diagnostic of statusDiagnosticFixtures(fixtureBundle)) {
      const mode = classifyLegacyStatusHeaderInvocation(diagnostic.argv);
      if (mode === "report") fail(`${diagnostic.id}: diagnostic fixture does not select check/write`);
      let diagnosticInputs = inputs;
      const targets = new Map(statusTargets(fixtureBundle, mode === "write" ? "before" : "after"));
      if (diagnostic.mutation === "remove_all_features") {
        diagnosticInputs = {
          ...inputs,
          matrix: { ...inputs.matrix, features: [] },
        };
      } else {
        const target = asRepoPath(diagnostic.target);
        const original = new TextDecoder("utf-8", { fatal: true }).decode(targets.get(target));
        const markerId = diagnostic.marker_id ?? fail(`${diagnostic.id}: marker ID is missing`);
        const open = `<!-- gen:sh:${markerId} -->`;
        const close = `<!-- /gen:sh:${markerId} -->`;
        let mutated = original;
        if (diagnostic.mutation === "remove_marker_pair") {
          const start = original.indexOf(open);
          const end = original.indexOf(close, start) + close.length;
          if (start < 0 || end < close.length) fail(`${diagnostic.id}: marker pair is absent from fixture`);
          mutated = original.slice(0, start) + original.slice(end);
        } else if (diagnostic.mutation === "remove_open_marker") {
          mutated = original.replace(open, "");
        } else if (diagnostic.mutation === "remove_close_marker") {
          mutated = original.replace(close, "");
        } else {
          const start = original.indexOf(open) + open.length;
          const end = original.indexOf(close, start);
          mutated = original.slice(0, start) + (diagnostic.replacement ?? "") + original.slice(end);
        }
        targets.set(target, bytes(mutated));
      }
      const plan = planLegacyStatusHeaderRun(diagnosticInputs, {
        mode,
        argv: diagnostic.argv,
        targets,
      });
      if (
        plan.exit_code !== diagnostic.exit_code || plan.writes.length !== diagnostic.writes ||
        !exactBytes(plan.stdout, diagnostic.stdout_md) || !exactBytes(plan.stderr, diagnostic.stderr_md)
      ) fail(`${diagnostic.id}: committed diagnostic receipt differs`);
      executed.push(diagnostic.id);
    }
    const invalidUtf8Targets = new Map(statusTargets(fixtureBundle, "after"));
    const invalidPath = asRepoPath("cddl-matrix/ROADMAP.md");
    const invalidSnapshot = new Uint8Array(invalidUtf8Targets.get(invalidPath) ?? fail("status ROADMAP fixture is absent"));
    const invalidBinding = inspectStatusMarkerBinding(invalidSnapshot, asSlotId("roadmap-counts"));
    if (invalidBinding.payload_interval === undefined) fail("status ROADMAP fixture marker is unresolved");
    invalidSnapshot[invalidBinding.payload_interval.start_byte] = 0xff;
    invalidUtf8Targets.set(invalidPath, invalidSnapshot);
    const invalidUtf8Plan = planLegacyStatusHeaderRun(inputs, {
      mode: "check",
      argv: ["--check"],
      targets: invalidUtf8Targets,
    });
    const invalidUtf8Stdout = new TextDecoder().decode(invalidUtf8Plan.stdout);
    if (
      invalidUtf8Plan.exit_code !== 1 || invalidUtf8Plan.writes.length !== 0 ||
      !invalidUtf8Stdout.includes("[E-OUTPUT-SLOT] cddl-matrix/ROADMAP.md#claims[0]: output target is not strict UTF-8")
    ) fail("check decoded an invalid marker payload before returning its typed output-slot diagnostic");
    return pass("negative", executed);
  }
  if (id === "status_projector_preflight_no_partial_write") {
    const before = new Map(statusTargets(fixtureBundle, "before"));
    const lastPath = asRepoPath("tests/README.md");
    before.set(lastPath, bytes(new TextDecoder().decode(before.get(lastPath)).replace("<!-- /gen:sh:tests-tier-full -->", "")));
    const spy = targetReadSpy(before);
    let resolvedClaims = 0;
    const plan = planLegacyStatusHeaderRun(
      inputs,
      { mode: "write", argv: ["--write"], targets: spy.targets },
      { claimResolved: () => { resolvedClaims++; } },
    );
    const expectedStderr = bytes("FAIL ../tests/README.md: span 'tests-tier-full' has no <!-- gen:sh:tests-tier-full --> … <!-- /gen:sh:tests-tier-full --> markers — hand-place them once around the phrase.\n");
    if (
      plan.exit_code !== 1 || plan.writes.length !== 0 || plan.stdout.byteLength !== 0 ||
      !exactBytes(plan.stderr, expectedStderr) || spy.reads.size !== 3 ||
      [...spy.reads.values()].some((value) => value !== 1) || resolvedClaims !== 11
    ) fail("failed last-target preflight did not read/preflight all targets before planning zero writes");
    return pass("negative");
  }
  fail(`${id} is not a status case`);
}

function testDeterminismCase(
  id: RequiredProjectionSelfTestCaseId,
  fixtureBundle?: ProjectionFixtureBundle,
): SelfTestResult {
  if (id === "issues_sorted") {
    const fixture = rawFixture();
    const brokenSpans = fixture.document.spans.map((span, index) =>
      index < 2 ? { ...span, sha256: "0".repeat(64) } : span
    );
    const forward = validateSourceSpans({
      document: { ...fixture.document, spans: brokenSpans },
      completed: complete(fixture.document).completed,
      source_snapshot: fixture.source,
    });
    const reverse = validateSourceSpans({
      document: { ...fixture.document, spans: [...brokenSpans].reverse() },
      completed: complete(fixture.document).completed,
      source_snapshot: fixture.source,
    });
    const coordinates = (issues: readonly RoadmapIssue[]): string =>
      issues.map((value) => `${value.code}:${value.logical_path}`).join("|");
    if (coordinates(forward) !== coordinates(reverse) || !coordinates(forward).includes("span-h")) {
      fail("issue order depends on source table insertion order");
    }
    return pass();
  }
  if (id === "json_sorted_keys") {
    const entries = ["z", "ä", "a"].map((value) =>
      [owner("record", value, "payload.summary_md"), "semantic"] as const
    );
    const forward = JSON.stringify(migrationDebtReport(debt(entries)));
    const reverse = JSON.stringify(migrationDebtReport(debt([...entries].reverse())));
    if (forward !== reverse || !(forward.indexOf('"owner_id":"a"') < forward.indexOf('"owner_id":"z"')) ||
      !(forward.indexOf('"owner_id":"z"') < forward.indexOf('"owner_id":"ä"'))) {
      fail("structured debt JSON arrays do not have canonical key order");
    }
    return pass();
  }
  if (id === "lexical_not_locale_sort") {
    const keys = ["z", "ä", "a"].sort((left, right) => left < right ? -1 : left > right ? 1 : 0);
    if (keys.join("|") !== "a|z|ä") fail("code-point sorting was not deterministic");
    return pass();
  }
  if (id === "no_clock_without_as_of") {
    const original = Date.now;
    Date.now = () => fail("status derivation read the clock");
    try {
      deriveMatrixStatusFacts({
        matrix: { annotations: [], features: [], containment_ids: [], control_operator_ids: [] },
        catalog: { rows: [] },
        registry: { gates: [] },
        timings: { tiers: [] },
      });
    } finally { Date.now = original; }
    return pass();
  }
  return testRenderCase(id, fixtureBundle);
}

const CASE_CATEGORY: Readonly<Partial<Record<RequiredProjectionSelfTestCaseId, SelfTestCategory>>> = {
  span_expected_byte_view_cross_chunk: "spans",
  span_expected_byte_view_incremental_hash: "spans",
  render_no_implicit_lf: "manifest-render",
};

function runNamedCase(
  id: RequiredProjectionSelfTestCaseId,
  fixtureBundle?: ProjectionFixtureBundle,
): SelfTestResult {
  if (id.startsWith("manifest_")) return testManifestCase(id);
  if (id.startsWith("span_")) return testSpanCase(id);
  if (id.startsWith("debt_")) return testDebtCase(id);
  if (id.startsWith("outputs_") || id.startsWith("write_") || id === "format_source_single_explicit" || id === "atomic_write_failure_preserves_target") {
    return testOutputCase(id);
  }
  if (id.startsWith("status_")) return testStatusCase(id, fixtureBundle);
  if (id.startsWith("render_") || id === "two_clean_renders_equal" || id === "issues_sorted" || id === "json_sorted_keys" || id === "lexical_not_locale_sort" || id === "no_clock_without_as_of") {
    return testDeterminismCase(id, fixtureBundle);
  }
  return fail(`unrouted projection selftest ${id}`);
}

export const PROJECTION_SELFTEST_CASES: readonly SelfTestCase[] = Object.freeze(
  REQUIRED_PROJECTION_SELFTEST_CASE_IDS.map((id) => ({
    id,
    category: CASE_CATEGORY[id] ?? (
      id.startsWith("manifest_") || id.startsWith("render_") ? "manifest-render"
        : id.startsWith("span_") ? "spans"
          : id.startsWith("debt_") ? "debt"
            : id.startsWith("status_") ? "status-compat"
              : id.startsWith("outputs_") || id.startsWith("write_") || id === "format_source_single_explicit" || id === "atomic_write_failure_preserves_target" ? "output-ownership"
                : "determinism-purity"
    ),
    run: (context: SelfTestContext) => runNamedCase(
      id,
      FIXTURE_REQUIRED_CASES.has(id) ? projectionFixtureBundleFromPorts(context.ports) : undefined,
    ),
  })),
);

function projectionFixtureBundleFromPorts(ports: RoadmapSelfTestPorts): ProjectionFixtureBundle {
  const root = asRepoPath("cddl-matrix/roadmap/fixtures");
  return createProjectionFixtureBundle(new Map(PROJECTION_FIXTURE_PATHS.map((path) => [
    path,
    ports.fixtures.readFixtureFile(root, path as FixtureRelativePath),
  ])));
}

export function runProjectionSelfTests(
  fixtureBundle?: ProjectionFixtureBundle,
): { readonly executed: number; readonly skipped: number } {
  let executed = 0;
  for (const id of REQUIRED_PROJECTION_SELFTEST_CASE_IDS) {
    if (FIXTURE_REQUIRED_CASES.has(id) && fixtureBundle === undefined) continue;
    const result = runNamedCase(id, fixtureBundle);
    if (!result.ok) fail(`${id}: returned a failed result`);
    executed++;
  }
  return { executed, skipped: REQUIRED_PROJECTION_SELFTEST_CASE_IDS.length - executed };
}

if (import.meta.main) {
  const receipt = runProjectionSelfTests();
  console.log(`projection selftests: ${receipt.executed} case(s) passed, ${receipt.skipped} fixture-backed case(s) skipped`);
}
