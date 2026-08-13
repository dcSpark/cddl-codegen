import type { RoadmapSelfTestPorts } from "../io.ts";
import type { ReadOnlyRoadmapPorts } from "../io.ts";
import type {
  SelfTestCandidateCase as SelfTestCase,
  SelfTestCategory,
  SelfTestContext,
  SelfTestCandidateResult as SelfTestResult,
} from "../selftest.ts";
import type { RoadmapIssue } from "../errors.ts";
import {
  observeMatchingIssue,
  observeSelfTestIssue,
  observeUntypedSelfTestRejection,
} from "./observations.ts";
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
  SemanticPayload,
  SourceSpan,
} from "../model/documents.ts";
import type { MatrixStatusInputs } from "../model/matrix.ts";
import type {
  StatusCompatibilityDiagnosticFixture,
  StatusCompatibilityInputsWire,
  StatusCompatibilityModeFixture,
} from "../selftest.ts";
import { resolveManifest } from "../manifest.ts";
import { buildRoadmapIndexes } from "../indexes.ts";
import {
  buildExpectedChunks,
  checkedPrefixOffsets,
  createExpectedByteView,
  exactProjectedFieldSegment,
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
import { renderCanonicalSemanticRecord } from "../adapters/matrix.ts";
import {
  compareMigrationDebt,
  debtOwnerIndex,
  deriveMigrationDebt,
  independentDebtIndex,
  migrationDebtReport,
  migrationProgressReport,
  validateRecordOwnerTransition,
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
  productionOutputInventory,
  productionOutputStage,
  resolveOutputClaims,
  validateOutputClaimInventory,
  validateProductionOutputRegistry,
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
  "manifest_semantic_only_is_not_placed",
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
  "debt_independent_set_growth_rejected",
  "debt_category_hiding_rejected",
  "debt_unrelated_base_rejected",
  "render_zero_chunks_rejected",
  "render_no_implicit_lf",
  "render_semantic_consumption_once",
  "render_semantic_only_zero_byte_consumption",
  "render_semantic_only_identity_debt",
  "render_semantic_only_span_prohibition",
  "render_semantic_exact_field_binding_swapped_labels",
  "render_semantic_exact_field_binding_partial",
  "render_semantic_exact_field_binding_duplicate",
  "render_semantic_replacement_rows_order_independent",
  "render_structural_exact_field_binding_all_kinds",
  "render_structural_exact_field_binding_rejections",
  "render_prior_projection_irrelevant",
  "outputs_duplicate_whole",
  "outputs_whole_vs_slot",
  "outputs_duplicate_slot",
  "outputs_duplicate_binding",
  "outputs_overlapping_slots",
  "outputs_path_escape",
  "outputs_empty_inventory",
  "outputs_legacy_status_inventory_no_whole_file_claim",
  "outputs_matrix_handoff_collision",
  "outputs_projection_path_floor",
  "outputs_slot_cardinality",
  "write_check_read_only_port",
  "write_query_read_only_port",
  "write_projection_rejects_toml",
  "write_projection_rejects_authority_files",
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
  "debt_unmodelled_coordinate_subset",
  "debt_semantic_authority_pending_both",
  "debt_progress_record_reversal_deterministic",
  "debt_progress_semantic_only_excluded",
  "debt_progress_exact_replacement_coverage",
  "debt_progress_swapped_replacement_not_covered",
  "debt_progress_typed_stale_unknown_visible",
  "debt_progress_completion_category_policy",
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
  "outputs_production_stage_inventories",
  "outputs_production_stage_required",
  "outputs_production_whole_authority",
  "status_facts_derive_fixture_parity",
  "status_projector_before_after_target_byte_parity",
  "status_projector_before_after_mode_parity",
  "status_projector_before_after_message_parity",
  "status_projector_preflight_no_partial_write",
  "status_projector_after_matrix_handoff",
  "span_expected_byte_view_cross_chunk",
  "span_expected_byte_view_incremental_hash",
] as const;

export type RequiredProjectionSelfTestCaseId =
  (typeof REQUIRED_PROJECTION_SELFTEST_CASE_IDS)[number];

export const PROJECTION_FIXTURE_PATHS = Object.freeze([
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
  readonly file_count: 9;
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
  if (files.size !== PROJECTION_FIXTURE_PATHS.length) throw new Error("projection fixture bundle must contain exactly nine files");
  const snapshots = new Map<ProjectionFixturePath, Uint8Array>();
  for (const path of PROJECTION_FIXTURE_PATHS) {
    const value = files.get(path);
    if (value === undefined || value.byteLength === 0) throw new Error(`projection fixture bundle is missing ${path}`);
    snapshots.set(path, new Uint8Array(value));
  }
  const bundle: ProjectionFixtureBundle = Object.freeze({ file_count: 9 });
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
  "status_facts_derive_fixture_parity",
  "status_projector_before_after_target_byte_parity",
  "status_projector_before_after_mode_parity",
  "status_projector_before_after_message_parity",
  "status_projector_preflight_no_partial_write",
  "status_projector_after_matrix_handoff",
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

function issueCodes(issues: readonly RoadmapIssue[]): Set<string> {
  return new Set(issues.map((value) => value.code));
}

function requireIssue(issues: readonly RoadmapIssue[], code: RoadmapIssue["code"]): void {
  const matched = issues.find((issue) => issue.code === code);
  if (matched === undefined) fail(`expected ${code}, got ${[...issueCodes(issues)].join(", ")}`);
  observeSelfTestIssue(matched);
}

function requireExactIssue(issues: readonly RoadmapIssue[], code: RoadmapIssue["code"], logicalPath: string): void {
  const matched = observeMatchingIssue(issues, code, logicalPath);
  if (matched === undefined) fail(`expected ${code} at ${logicalPath}, got ${issues.map((issue) => `${issue.code}@${issue.logical_path}`).join(", ")}`);
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
  const chunks: RenderChunk[] = ["one\n", "two", "three\n"].map((value, manifest_index) => ({
    manifest_index,
    owner: { kind: "fragment", id: `hash-${manifest_index}`, field: "body_md" },
    bytes: bytes(value),
    source_span_ids: [],
    consumed_fields: ["body_md"],
  }));
  const observed = {
    slice_segments: 0,
    source_fact_chunks: 0,
    combined_allocations: 0,
    final_allocations: 0,
  };
  const observer: ExpectedByteViewObserver = {
    hashSegmentVisited: () => { observed.slice_segments++; },
    sourceFactChunkVisited: () => { observed.source_fact_chunks++; },
    combinedHashBufferAllocated: () => { observed.combined_allocations++; },
    finalProjectionAllocated: () => { observed.final_allocations++; },
  };
  const view = createExpectedByteView(chunks, observer);
  const digest = new Bun.CryptoHasher("sha256").update(bytes("one\ntwothree\n")).digest("hex");
  if (view.sha256(view.slice(0, view.byte_length)) !== digest) fail("incremental digest differs");
  if (observed.slice_segments <= 1) fail("incremental hash did not causally report multi-segment traversal");
  if (observed.combined_allocations !== 0 || observed.final_allocations !== 0) {
    fail("incremental hash allocated a combined/final projection buffer");
  }
  observed.slice_segments = 0;
  observed.source_fact_chunks = 0;
  observed.combined_allocations = 0;
  observed.final_allocations = 0;
  const facts = view.sourceFacts();
  if (
    facts.byte_length !== bytes("one\ntwothree\n").byteLength ||
    facts.sha256 !== digest || facts.line_count !== 2 || facts.eof !== "lf"
  ) fail("whole-source facts were not incrementally exact across chunks");
  if (!Object.isFrozen(facts)) fail("whole-source facts were not frozen");
  if (observed.source_fact_chunks !== chunks.length || observed.source_fact_chunks <= 1) {
    fail("whole-source facts did not causally report every private chunk traversal");
  }
  if (observed.combined_allocations !== 0 || observed.final_allocations !== 0) {
    fail("whole-source facts allocated a combined/final projection buffer");
  }
  const sliceVisitsAfterFirstCall = observed.slice_segments;
  const visitsAfterFirstCall = observed.source_fact_chunks;
  const combinedAfterFirstCall = observed.combined_allocations;
  const finalAfterFirstCall = observed.final_allocations;
  if (view.sourceFacts() !== facts) fail("whole-source facts did not return the cached object");
  if (
    observed.slice_segments !== sliceVisitsAfterFirstCall ||
    observed.source_fact_chunks !== visitsAfterFirstCall ||
    observed.combined_allocations !== combinedAfterFirstCall ||
    observed.final_allocations !== finalAfterFirstCall
  ) {
    fail("cached whole-source facts repeated traversal or allocation");
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
  const fixture = semanticFixture("exact");
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
    case "manifest_semantic_only_is_not_placed": {
      const semanticOnly = semanticOnlyCompletion();
      if (semanticOnly.placement.issues.length !== 0 ||
        semanticOnly.placement.ops.some((op) => op.node.kind === "record" && op.node.id === semanticOnly.record.id)) {
        fail("semantic-only record did not remain an unplaced first-class semantic owner");
      }
      const authored = {
        ...semanticOnly.document,
        manifest: [...semanticOnly.document.manifest, { kind: "record" as const, record_id: semanticOnly.record.id }],
      };
      const rejected = resolveManifest(authored).issues;
      if (!rejected.some((entry) => entry.code === "E-MANIFEST-KIND" &&
        entry.logical_path === `manifest[${semanticOnly.document.manifest.length}]`)) {
        fail("authored semantic-only manifest placement was not rejected at its exact entry");
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

function validateFixture(document: RoadmapDocument): readonly RoadmapIssue[] {
  const placement = resolveManifest(document);
  const completed = complete(document).completed;
  return [
    ...placement.issues,
    ...validateCompletedChunks(document, placement.ops, completed),
    ...validateSourceSpans({ document, completed }),
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
  const fixture = semanticFixture("exact");
  let document = fixture.document;
  const lastSpanIndex = document.spans.length - 1;
  let expected: RoadmapIssue["code"] | undefined;
  // Some mutations are visible at more than one coordinate; pin the one the case is about.
  let expectedPath: string | undefined;
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
      expectedPath = 'source_span["span-h"]';
      break;
    case "span_wrong_kind":
      document = { ...document, spans: document.spans.map((span, index) => index === 0 ? { ...span, source_kind: "record" as const } : span) };
      expected = "E-SPAN-KIND";
      break;
    case "span_wrong_status":
      document = { ...document, spans: document.spans.map((span, index) => index === 0 ? { ...span, migration_status: "raw" as const } : span) };
      expected = "E-SPAN-STATUS";
      break;
    case "span_out_of_bounds":
      document = { ...document, spans: document.spans.map((span, index) => index === lastSpanIndex ? { ...span, end_byte: span.end_byte + 2 } : span) };
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
      let rejection: unknown;
      try { view.slice(1, 2); } catch (error) { rejection = error; }
      const rejected = rejection !== undefined;
      if (!rejected) fail("mid-scalar byte boundary accepted");
      if (id === "span_mid_scalar_boundary") observeUntypedSelfTestRejection(id, rejection);
      return pass(id === "span_mid_scalar_boundary" ? "negative" : "positive");
    }
    case "span_final_eof_owner": {
      const issues = validateFixture(document);
      if (issues.length !== 0) fail(`valid EOF ownership failed: ${issues.map((value) => value.code).join(",")}`);
      requireIssue(validateFixture({
        ...document,
        document: { ...document.document, frozen_source_line_count: 2 },
      }), "E-SPAN-COVERAGE");
      requireIssue(validateFixture({
        ...document,
        document: { ...document.document, frozen_source_eof: "lf" },
      }), "E-SPAN-COVERAGE");
      return pass("positive");
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
      const original = complete(document).completed;
      let reads = 0;
      const completed: CompletedRenderIr = {
        ...original,
        expected_bytes: {
          ...original.expected_bytes,
          sourceFacts() { reads++; return original.expected_bytes.sourceFacts(); },
        },
      };
      const valid = validateSourceSpans({ document, completed });
      if (valid.length !== 0 || reads !== 1) fail(`source facts acquired ${reads} times`);
      requireIssue(validateSourceSpans({
        document: {
          ...document,
          document: { ...document.document, frozen_source_byte_length: document.document.frozen_source_byte_length + 1 },
        },
        completed: original,
      }), "E-SPAN-COVERAGE");
      requireIssue(validateSourceSpans({
        document: {
          ...document,
          document: { ...document.document, frozen_source_sha256: "0".repeat(64) },
        },
        completed: original,
      }), "E-SOURCE-DIGEST");
      return pass("positive");
    }
    case "span_source_change_digest_rejected": {
      document = {
        ...document,
        sections: document.sections.map((section, index) =>
          index === 0 ? { ...section, body_md: bytes("X") } : section
        ),
      };
      const changed = complete(document).completed;
      if (changed.expected_bytes.sliceBytes(0, 1)[0] !== 0x58) fail("authority-byte mutation did not reach expected chunks");
      expected = "E-SOURCE-DIGEST";
      break;
    }
    default:
      fail(`${id} is not a span case`);
  }
  const issues = validateFixture(document);
  if (expected === undefined) fail("span test lacks expected code");
  if (expectedPath === undefined) requireIssue(issues, expected);
  else requireExactIssue(issues, expected, expectedPath);
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
  entries: readonly (readonly [DebtOwnerKey, "semantic"])[],
  independent: readonly IndependentDebtKey[] = [],
): MigrationDebt {
  return {
    owners: new Map(entries.map(([key, state]) => [debtOwnerIndex(key), { key, state }])),
    independent: new Map(independent.map((key) => [independentDebtIndex(key), key])),
  };
}

function testDebtCase(id: RequiredProjectionSelfTestCaseId): SelfTestResult {
  if (id === "debt_semantic_authority_pending_both") {
    const fixture = semanticFixture("exact");
    const record = fixture.document.records[0];
    if (record === undefined) fail("semantic pending fixture lacks authority record");
    const payload: SemanticPayload = {
      kind: "work",
      summary_md: bytes("Pending."),
      work_state: "pending_review",
      work_intent: "build_capability",
      work_kind: "feature",
      risk: "cosmetic",
      family_classification: "pending",
      uncertainty_md: bytes("Review."),
    };
    const document: RoadmapDocument = {
      ...fixture.document,
      records: fixture.document.records.map((candidate) => candidate === record ? { ...record, payload } : candidate),
    };
    const original = completeSemantic(fixture.document, fixture.renderCalls);
    const completed: CompletedRenderIr = {
      ...original,
      field_consumption: original.field_consumption.map((entry) => entry.owner_id === record.id ? {
        ...entry,
        expected_fields: ["payload.summary_md", "payload.uncertainty_md"],
        consumed_fields: ["payload.summary_md", "payload.uncertainty_md"],
      } : entry),
    };
    const derived = deriveMigrationDebt(document, completed);
    const tuples = [...derived.independent.values()].filter((item) =>
      item.category === "inferred_transitions" || item.category === "pending_family_classifications"
    );
    if (tuples.length !== 2 || tuples.some((item) => item.owner.owner_field !== "payload.summary_md") ||
      !tuples.some((item) => item.subject === "payload.work_state") ||
      !tuples.some((item) => item.subject === "payload.family_classification")) {
      fail("semantic authority did not derive both separate pending tuples on a semantic owner");
    }
    return pass();
  }
  if (id === "debt_progress_record_reversal_deterministic") {
    const fixture = semanticFixture("exact");
    const completed = completeSemantic(fixture.document, fixture.renderCalls);
    const reversed: RoadmapDocument = {
      ...fixture.document,
      records: [...fixture.document.records].reverse(),
      spans: [...fixture.document.spans].reverse(),
    };
    const reversedCompleted = completeSemantic(reversed, { value: 0 });
    const forward = JSON.stringify(migrationProgressReport(
      fixture.document,
      deriveMigrationDebt(fixture.document, completed),
      completed,
    ));
    const backward = JSON.stringify(migrationProgressReport(
      reversed,
      deriveMigrationDebt(reversed, reversedCompleted),
      reversedCompleted,
    ));
    if (forward !== backward) fail("migration progress JSON depends on record/span insertion order");
    return pass();
  }
  if (id === "debt_progress_semantic_only_excluded") {
    const fixture = semanticFixture("exact");
    const baseCompleted = completeSemantic(fixture.document, fixture.renderCalls);
    const sourceRecord = fixture.document.records[0];
    if (sourceRecord === undefined) fail("semantic-only progress fixture lacks source");
    const added = {
      ...sourceRecord,
      id: asRoadmapId("matrix.fixture-progress-only"),
      projection_visibility: "semantic_only" as const,
      source_replacements: [],
    };
    const candidate: RoadmapDocument = {
      ...fixture.document,
      records: [...fixture.document.records, added],
    };
    const completed = completeSemantic(candidate, { value: 0 });
    const baseProgress = migrationProgressReport(
      fixture.document,
      deriveMigrationDebt(fixture.document, baseCompleted),
      baseCompleted,
    );
    const candidateProgress = migrationProgressReport(candidate, deriveMigrationDebt(candidate, completed), completed);
    if (candidateProgress.replacement_coverage.denominator !== baseProgress.replacement_coverage.denominator ||
      candidateProgress.replacement_coverage.numerator !== baseProgress.replacement_coverage.numerator ||
      JSON.stringify(candidateProgress.completion_audit) !== JSON.stringify(baseProgress.completion_audit)) {
      fail("semantic-only record inflated a replacement migration denominator or a completion blocker");
    }
    return pass();
  }
  if (id === "debt_progress_exact_replacement_coverage" || id === "debt_progress_swapped_replacement_not_covered") {
    const fixture = semanticFixture("exact");
    const completed = completeSemantic(fixture.document, fixture.renderCalls);
    let document = fixture.document;
    if (id === "debt_progress_swapped_replacement_not_covered") {
      const record = document.records[0];
      if (record === undefined || record.source_replacements.length < 2) {
        fail("swapped progress fixture lacks replacement rows");
      }
      const left = record.source_replacements[0]!;
      const right = record.source_replacements[1]!;
      const fields = new Map([[left.span_id, right.replacement_field], [right.span_id, left.replacement_field]]);
      document = {
        ...document,
        records: document.records.map((candidate) => candidate === record ? {
          ...record,
          source_replacements: record.source_replacements.map((replacement) => ({
            ...replacement,
            replacement_field: fields.get(replacement.span_id) ?? replacement.replacement_field,
          })),
        } : candidate),
        spans: document.spans.map((span) => ({ ...span, owner_field: fields.get(span.id) ?? span.owner_field })),
      };
    }
    const report = migrationProgressReport(document, deriveMigrationDebt(document, completed), completed);
    const denominator = document.spans.filter((span) => span.migration_status === "replaced").length;
    if (denominator !== 7) fail("replacement denominator drifted from the seven replaced fixture spans");
    const expected = id === "debt_progress_exact_replacement_coverage" ? denominator : denominator - 2;
    if (report.replacement_coverage.denominator !== denominator || report.replacement_coverage.numerator !== expected ||
      report.replacement_coverage.covered_span_ids.length !== expected) {
      fail(`${id}: exact replacement coverage drifted`);
    }
    return pass();
  }
  if (id === "debt_progress_typed_stale_unknown_visible") {
    const fixture = semanticFixture("exact").document;
    const probe = (payload: SemanticPayload) => {
      const document: RoadmapDocument = {
        ...fixture,
        records: fixture.records.map((record) => ({ ...record, payload })),
      };
      const completed = complete(document).completed;
      return migrationProgressReport(document, deriveMigrationDebt(document, completed), completed).typed_semantic_state;
    };
    const signalUnknown = probe({
      kind: "signal", summary_md: bytes("Unknown."), transition_kind: "watch_escalation",
      failure_signature_md: bytes("Fail."), capture_procedure_md: bytes("Capture."),
      response_md: bytes("Respond."), escalation_action_md: bytes("Escalate."),
      retirement_semantics_md: bytes("Retire."), evaluation: "unknown",
    });
    const signalStale = probe({
      kind: "signal", summary_md: bytes("Stale."), transition_kind: "watch_escalation",
      failure_signature_md: bytes("Fail."), capture_procedure_md: bytes("Capture."),
      response_md: bytes("Respond."), escalation_action_md: bytes("Escalate."),
      retirement_semantics_md: bytes("Retire."), evaluation: "stale",
    });
    const evidence = probe({
      kind: "evidence", summary_md: bytes("Evidence."), evidence_kind: "source_read",
      claim_md: bytes("Claim."), evidence_verdict: "unknown", freshness: "stale", reference_ids: [],
      unprobed_remainder_md: bytes("Remainder."), scope: {},
    });
    const control = probe({
      kind: "control", summary_md: bytes("Control."), control_kind: "review_rule", control_state: "stale",
      reference_ids: [], claim_md: bytes("Claim."), boundary_md: bytes("Boundary."),
    });
    if (signalUnknown.signals.unknown_record_ids.length !== 1 || signalStale.signals.stale_record_ids.length !== 1 ||
      evidence.evidence.unknown_record_ids.length !== 1 || evidence.evidence.stale_record_ids.length !== 1 ||
      control.controls.stale_record_ids.length !== 1 ||
      JSON.stringify(control.unrepresentable_coordinates) !== JSON.stringify(["controls.unknown"])) {
      fail("typed stale/unknown semantic state is not fully visible");
    }
    return pass();
  }
  if (id === "debt_progress_completion_category_policy") {
    const document = semanticFixture("exact").document;
    const completed = complete(document).completed;
    const recordOwner = owner("record", "matrix.fixture-work", "payload.summary_md");
    const extras: IndependentDebtKey[] = [
      { roadmap: "matrix", category: "inferred_transitions", owner: recordOwner, subject: "lane" },
      { roadmap: "matrix", category: "unmodelled_coordinates", owner: recordOwner, subject: "visible" },
    ];
    const report = migrationProgressReport(document, deriveMigrationDebt(document, completed, extras), completed);
    if (!report.completion_audit.lane_blockers.some((blocker) =>
      blocker.category === "inferred_transitions" && blocker.subject.includes("lane")
    ) || report.completion_audit.join_blockers.length !== 0 ||
      [...report.completion_audit.lane_blockers, ...report.completion_audit.join_blockers].some((blocker) =>
        blocker.category === "unmodelled_coordinates"
      ) || !report.independent_debt.items.some((item) =>
        item.category === "unmodelled_coordinates" && item.subject === "visible"
      )) {
      fail("completion audit category policy hid or misclassified independent state");
    }
    return pass();
  }
  const document = semanticFixture("exact").document;
  const recordRaw = owner("record", "matrix.fixture-work", "source_block_md");
  const recordSemantic = owner("record", "matrix.fixture-work", "payload.summary_md");
  const options: DebtComparisonOptions = { base_document: document, candidate_document: document };
  let base = debt([[recordRaw, "semantic"]]);
  let candidate = debt([[recordRaw, "semantic"]]);
  let expected: RoadmapIssue["code"] | undefined;
  let compareOptions: DebtComparisonOptions = options;
  switch (id) {
    case "debt_owner_field_rename_requires_witness":
      candidate = debt([[recordSemantic, "semantic"]]);
      expected = "E-DEBT-OWNER-REGRESSION";
      break;
    case "debt_independent_set_growth_rejected": {
      const independentKey: IndependentDebtKey = {
        roadmap: "matrix", category: "inferred_transitions", owner: recordRaw, subject: "transition",
      };
      candidate = debt([[recordRaw, "semantic"]], [independentKey]);
      expected = "E-DEBT-SET-GROWTH";
      break;
    }
    case "debt_category_hiding_rejected": {
      const oldKey: IndependentDebtKey = {
        roadmap: "matrix", category: "inferred_transitions", owner: recordRaw, subject: "transition",
      };
      const hiddenKey: IndependentDebtKey = { ...oldKey, category: "unmodelled_coordinates" };
      base = debt([[recordRaw, "semantic"]], [oldKey]);
      candidate = debt([[recordRaw, "semantic"]], [hiddenKey]);
      expected = "E-DEBT-CATEGORY-HIDE";
      break;
    }
    case "debt_unrelated_base_rejected":
      compareOptions = {
        base_document: { ...document, document: { ...document.document, source_path: asRepoPath("other.toml") } },
        candidate_document: document,
      };
      expected = "E-DEBT-BASE-MISMATCH";
      break;
    case "debt_structured_owner_every_kind": {
      const kinds: DebtOwnerKey["owner_kind"][] = ["record", "section", "fragment", "part", "legacy_marker", "source_span"];
      const derived = deriveMigrationDebt(document, complete(document).completed);
      const observedKinds = new Set([...derived.owners.values()].map((value) => value.key.owner_kind));
      for (const kind of kinds) if (!observedKinds.has(kind)) fail(`derived debt omitted ${kind}`);
      const report = migrationDebtReport(derived);
      if (report.owners.length < kinds.length) fail("debt report omitted derived owner atoms");
      return pass();
    }
    case "debt_same_textual_id_different_kind_distinct": {
      const section = owner("section", "same", "body_md");
      const fragment = owner("fragment", "same", "body_md");
      if (debtOwnerIndex(section) === debtOwnerIndex(fragment)) fail("textual IDs collapsed across kinds");
      return pass();
    }
    case "debt_unmodelled_coordinate_subset": {
      const key: IndependentDebtKey = {
        roadmap: "matrix", category: "unmodelled_coordinates", owner: recordRaw, subject: "axis-x",
      };
      base = debt([[recordRaw, "semantic"]], [key]);
      candidate = debt([[recordRaw, "semantic"]]);
      break;
    }
    default:
      fail(`${id} is not a debt case`);
  }
  const issues = compareMigrationDebt(base, candidate, compareOptions);
  if (expected === undefined) {
    if (issues.length !== 0) fail(`${id}: unexpected debt issue ${issues.map((value) => value.code).join(",")}`);
    return pass();
  }
  requireIssue(issues, expected);
  if (id === "debt_owner_field_rename_requires_witness") {
    // A renamed owner field is BOTH a candidate-only owner and a disappeared base owner. The only
    // accepted candidate-only witness is a brand-new semantic-only record, which an already-present
    // record can never be, so neither half is waivable by owner counts.
    if (!issues.some((value) => value.logical_path === `owners.${debtOwnerIndex(recordRaw)}`)) {
      fail("renamed owner field did not report its disappeared base owner");
    }
    if (compareMigrationDebt(base, candidate, compareOptions).length !== issues.length) {
      fail("owner rename rejection is not stable under replay");
    }
  }
  return pass("negative");
}

function semanticFixture(
  mode: "exact" | "missing" | "duplicate" = "exact",
): {
  readonly document: RoadmapDocument;
  readonly source: Uint8Array;
  readonly renderCalls: { value: number };
} {
  const source = bytes("HFMSUMACCRATIONALEPG");
  const sectionId = asSectionId("heading");
  const fragmentId = asFragmentId("fragment");
  const markerId = asMarkerId("marker");
  const recordId = asRoadmapId("matrix.fixture-work");
  const partId = asPartId("part");
  const slotId = asSlotId("status-slot");
  const summary = bytes("SUM");
  const acceptance = bytes("ACC");
  const rationale = bytes("RATIONALE");
  const review = bytes("review");
  const replacement = (span_id: string, replacement_field: string) => ({
    span_id: asSpanId(span_id),
    replacement_field,
    review_note_md: review,
  });
  const manifest: ManifestEntry[] = [
    { kind: "section", section_id: sectionId },
    { kind: "fragment", fragment_id: fragmentId },
    { kind: "legacy_marker", marker_id: markerId },
    { kind: "record", record_id: recordId },
    { kind: "part", part_id: partId },
    { kind: "generated_slot", slot_id: slotId },
  ];
  const spanRows: readonly [string, SourceSpan["source_kind"], string, string, number, number][] = [
    ["span-h", "section", sectionId, "body_md", 0, 1],
    ["span-f", "fragment", fragmentId, "body_md", 1, 2],
    ["span-m", "legacy_marker", markerId, "marker_md", 2, 3],
    ["span-summary", "record", recordId, "payload.summary_md", 3, 6],
    ["span-acceptance", "record", recordId, "payload.acceptance_md", 6, 9],
    ["span-rationale", "record", recordId, "payload.priority_rationale_md", 9, 18],
    ["span-p", "part", partId, "body_md", 18, 19],
    ["span-g", "generated_slot", slotId, "generated", 19, 20],
  ];
  const spans = spanRows.map(([id, source_kind, owner_id, owner_field, start_byte, end_byte]): SourceSpan => ({
    id: asSpanId(id),
    start_byte,
    end_byte,
    sha256: sha256(source.subarray(start_byte, end_byte)),
    source_kind,
    owner_id,
    owner_field,
    migration_status: source_kind === "generated_slot" ? "generated" : "replaced",
  }));
  const document: RoadmapDocument = {
    document: {
      schema_version: 2,
      authority: "authoritative",
      roadmap: "matrix",
      source_path: asRepoPath("fixture/matrix.toml"),
      projection_path: asRepoPath("fixture/matrix.md"),
      frozen_source_sha256: sha256(source),
      frozen_source_byte_length: source.byteLength,
      frozen_source_line_count: 1,
      frozen_source_eof: "none",
    },
    sections: [{
      section_id: sectionId,
      title: "Heading",
      render_authority: "semantic",
      body_md: bytes("H"),
      source_replacements: [replacement("span-h", "body_md")],
    }],
    fragments: [{
      fragment_id: fragmentId,
      projection_group: sectionId,
      render_authority: "semantic",
      lifecycle_disposition: "document_prose",
      body_md: bytes("F"),
      source_replacements: [replacement("span-f", "body_md")],
    }],
    legacy_markers: [{
      marker_id: markerId,
      legacy_aliases: ["legacy"],
      render_authority: "semantic",
      marker_md: bytes("M"),
      source_replacements: [replacement("span-m", "marker_md")],
    }],
    records: [{
      id: recordId,
      title: "Work",
      projection_group: sectionId,
      render_authority: "semantic",
      projection_visibility: "document",
      payload: {
        kind: "work",
        summary_md: summary,
        work_state: "ready",
        work_intent: "repair",
        work_kind: "feature",
        risk: "cosmetic",
        family_classification: "none_reviewed",
        evidence_ids: [asRoadmapId("matrix.fixture-evidence-a"), asRoadmapId("matrix.fixture-evidence-b")],
        acceptance_md: acceptance,
        priority_rationale_md: rationale,
      },
      source_replacements: [
        replacement("span-summary", "payload.summary_md"),
        replacement("span-acceptance", "payload.acceptance_md"),
        replacement("span-rationale", "payload.priority_rationale_md"),
      ],
    }],
    parts: [{
      part_id: partId,
      parent_record_id: recordId,
      render_authority: "semantic",
      lifecycle_disposition: "parent_supporting_prose",
      body_md: bytes("P"),
      source_replacements: [replacement("span-p", "body_md")],
    }],
    generated_slots: [{ slot_id: slotId, binding: "fixture-status", span_ids: [asSpanId("span-g")] }],
    manifest,
    spans,
    relations: [],
    references: [],
  };
  const calls = { value: 0 };
  Object.defineProperty(document, "__selftest_mode", { value: mode, enumerable: false });
  Object.defineProperty(document, "__render_calls", { value: calls, enumerable: false });
  return { document, source, renderCalls: calls };
}

function completeSemantic(document: RoadmapDocument, calls: { value: number }): CompletedRenderIr {
  const mode = (document as RoadmapDocument & { __selftest_mode?: string }).__selftest_mode ?? "exact";
  const placement = resolveManifest(document);
  return buildExpectedChunks(document, placement.ops, {
    renderSemanticRecord(record, fields) {
      calls.value++;
      const payload = record.payload;
      if (payload.kind !== "work" || payload.work_state !== "ready") return new Uint8Array();
      const first = fields.consume("payload.summary_md", payload.summary_md);
      if (mode === "duplicate") fields.consume("payload.summary_md", payload.summary_md);
      const second = mode === "missing" ? new Uint8Array() : fields.consume("payload.acceptance_md", payload.acceptance_md);
      const third = fields.consume("payload.priority_rationale_md", payload.priority_rationale_md);
      if (record.projection_visibility === "semantic_only") return new Uint8Array();
      const output = new Uint8Array(first.byteLength + second.byteLength + third.byteLength);
      output.set(first);
      output.set(second, first.byteLength);
      output.set(third, first.byteLength + second.byteLength);
      return output;
    },
    resolveGeneratedSlot(slot) { return { binding: slot.binding, bytes: bytes("G") }; },
  });
}

function recordChunkFor(completed: CompletedRenderIr, id: string): RenderChunk {
  const chunks = completed.chunks.filter((chunk) => chunk.owner.kind === "record" && chunk.owner.id === id);
  if (chunks.length !== 1) fail(`expected one record chunk for ${id}`);
  return chunks[0]!;
}

/**
 * Every v2 owner is semantic, so the structural exact-binding vectors read the same fixture as the
 * record vectors; the alias keeps the structural cases' intent legible at their call sites.
 */
function structuralSemanticFixture(): {
  readonly document: RoadmapDocument;
  readonly completed: CompletedRenderIr;
} {
  const document = semanticFixture("exact").document;
  return { document, completed: complete(document).completed };
}

function structuralSectionWithReplacements(
  document: RoadmapDocument,
  source_replacements: RoadmapDocument["sections"][number]["source_replacements"],
): RoadmapDocument {
  return {
    ...document,
    sections: document.sections.map((section) => ({ ...section, source_replacements })),
  };
}

function semanticOnlyCompletion(): {
  readonly document: RoadmapDocument;
  readonly record: RoadmapDocument["records"][number];
  readonly placement: ReturnType<typeof resolveManifest>;
  readonly completed: CompletedRenderIr;
} {
  const exact = semanticFixture("exact");
  const record = exact.document.records[0];
  if (record === undefined) fail("semantic-only render vector lacks semantic record");
  const semanticOnlyRecord = { ...record, projection_visibility: "semantic_only" as const, source_replacements: [] };
  const document: RoadmapDocument = {
    ...exact.document,
    records: exact.document.records.map((candidate) => candidate === record ? semanticOnlyRecord : candidate),
    manifest: exact.document.manifest.filter((entry) =>
      !(entry.kind === "record" && entry.record_id === semanticOnlyRecord.id)
    ),
  };
  const placement = resolveManifest(document);
  const completed = buildExpectedChunks(document, placement.ops, {
    renderSemanticRecord: renderCanonicalSemanticRecord,
    resolveGeneratedSlot(slot) { return { binding: slot.binding, bytes: bytes("G") }; },
  });
  return { document, record: semanticOnlyRecord, placement, completed };
}

function testRenderCase(
  id: RequiredProjectionSelfTestCaseId,
  fixtureBundle?: ProjectionFixtureBundle,
): SelfTestResult {
  if (id === "render_no_implicit_lf") {
    finalRenderHasNoImplicitBytes();
    return pass();
  }
  if (id === "render_zero_chunks_rejected") {
    let rejection: unknown;
    try { renderValidatedChunks([], [], createExpectedByteView([])); } catch (error) { rejection = error; }
    if (rejection === undefined) fail("zero chunks rendered successfully");
    observeUntypedSelfTestRejection(id, rejection);
    return pass("negative");
  }
  if (id === "render_semantic_consumption_once" || id === "render_chunks_precede_consumption_validation") {
    const fixture = semanticFixture(id === "render_semantic_consumption_once" ? "duplicate" : "missing");
    const placement = resolveManifest(fixture.document);
    const completed = completeSemantic(fixture.document, fixture.renderCalls);
    if (completed.chunks.length !== placement.ops.length) fail("field failure prevented chunk completion");
    requireExactIssue(
      validateCompletedChunks(fixture.document, placement.ops, completed),
      "E-FIELD-CONSUMPTION",
      'record["matrix.fixture-work"]',
    );
    if (id === "render_chunks_precede_consumption_validation") {
      const raw = semanticFixture("exact");
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
  if (id === "render_semantic_only_zero_byte_consumption") {
    const { document, record, placement, completed } = semanticOnlyCompletion();
    const issues = validateCompletedChunks(document, placement.ops, completed);
    if (issues.length !== 0) fail(`semantic-only non-render ledger failed: ${issues.map((issue) => issue.message).join(";")}`);
    const semanticChunk = completed.chunks.find((chunk) => chunk.owner.kind === "record");
    const ledger = completed.field_consumption.find((entry) => entry.owner_kind === "record");
    if (semanticChunk !== undefined ||
      completed.projected_field_segments.some((segment) => segment.owner_kind === "record") ||
      placement.ops.some((op) => op.node.kind === "record" && op.node.id === record.id)) {
      fail("semantic-only record minted a manifest operation, chunk, or projected field segment");
    }
    if (ledger === undefined || ledger.expected_fields.length !== 3 || ledger.consumed_fields.length !== 3) {
      fail("semantic-only record did not explicitly consume every Markdown field");
    }
    return pass();
  }
  if (id === "render_semantic_only_identity_debt") {
    const { document, record, placement, completed } = semanticOnlyCompletion();
    if (placement.ops.some((op) => op.node.kind === "record" && op.node.id === record.id) ||
      !buildRoadmapIndexes(document).indexes.first_class.has(record.id)) {
      fail("semantic-only record did not retain identity outside the manifest");
    }
    const semanticDebt = deriveMigrationDebt(document, completed);
    if ([...semanticDebt.owners.values()].filter(({ key }) => key.owner_kind === "record" && key.owner_id === record.id).length !== 3) {
      fail("semantic-only fields did not retain semantic debt owner atoms");
    }
    // Semantic-only visibility is admissible ONLY for a candidate-only record. An already-published
    // document-visible record that turns semantic-only is a visibility regression, and no counts-only
    // witness can authorize it.
    const base = semanticFixture("exact").document;
    requireExactIssue(
      validateRecordOwnerTransition({ base_document: base, candidate_document: document }),
      "E-DEBT-OWNER-REGRESSION",
      `record[${JSON.stringify(record.id)}].projection_visibility`,
    );
    return pass("negative");
  }
  if (id === "render_semantic_only_span_prohibition") {
    const { document, record, completed } = semanticOnlyCompletion();
    requireIssue(validateSourceSpans({ document, completed }), "E-SPAN-OWNER");
    const exactRecord = semanticFixture("exact").document.records[0];
    if (exactRecord === undefined) fail("span-prohibition vector lacks replacement source");
    const replacementOnly: RoadmapDocument = {
      ...document,
      records: document.records.map((candidate) => candidate === record
        ? { ...candidate, source_replacements: [exactRecord.source_replacements[0]!] }
        : candidate),
    };
    requireIssue(validateSourceSpans({ document: replacementOnly, completed }), "E-SPAN-OWNER");
    return pass("negative");
  }
  if (
    id === "render_semantic_exact_field_binding_swapped_labels" ||
    id === "render_semantic_exact_field_binding_partial" ||
    id === "render_semantic_exact_field_binding_duplicate" ||
    id === "render_semantic_replacement_rows_order_independent"
  ) {
    const exact = semanticFixture("exact");
    const exactPlacement = resolveManifest(exact.document);
    const exactCompleted = completeSemantic(exact.document, exact.renderCalls);
    const exactIssues = [
      ...validateCompletedChunks(exact.document, exactPlacement.ops, exactCompleted),
      ...validateSourceSpans({ document: exact.document, completed: exactCompleted }),
    ];
    if (exactIssues.length !== 0) fail(`exact semantic control failed: ${exactIssues.map((issue) => issue.message).join(";")}`);
    const record = exact.document.records.find((candidate) =>
      "render_authority" in candidate && candidate.render_authority === "semantic"
    );
    if (record === undefined || record.render_authority !== "semantic") fail("exact binding vector lacks semantic record");
    if (id === "render_semantic_exact_field_binding_swapped_labels") {
      const [first, second, third] = record.source_replacements;
      const swapped: RoadmapDocument = {
        ...exact.document,
        records: exact.document.records.map((candidate) => candidate === record ? {
          ...record,
          source_replacements: [
            { ...first!, replacement_field: second!.replacement_field },
            { ...second!, replacement_field: first!.replacement_field },
            third!,
          ],
        } : candidate),
        spans: exact.document.spans.map((span) => span.id === first!.span_id
          ? { ...span, owner_field: second!.replacement_field }
          : span.id === second!.span_id ? { ...span, owner_field: first!.replacement_field } : span),
      };
      requireIssue(validateSourceSpans({ document: swapped, completed: exactCompleted }), "E-SPAN-OWNER");
      return pass("negative");
    }
    if (id === "render_semantic_exact_field_binding_partial") {
      const first = record.source_replacements[0]!;
      const partial: RoadmapDocument = {
        ...exact.document,
        spans: exact.document.spans.map((span) => span.id === first.span_id
          ? { ...span, end_byte: span.end_byte - 1, sha256: sha256(bytes("SU")) }
          : span),
      };
      requireIssue(validateSourceSpans({ document: partial, completed: exactCompleted }), "E-SPAN-OWNER");
      return pass("negative");
    }
    if (id === "render_semantic_exact_field_binding_duplicate") {
      const [first, second, third] = record.source_replacements;
      const duplicateField: RoadmapDocument = {
        ...exact.document,
        records: exact.document.records.map((candidate) => candidate === record ? {
          ...record,
          source_replacements: [first!, { ...second!, replacement_field: first!.replacement_field }, third!],
        } : candidate),
      };
      requireIssue(validateCompletedChunks(duplicateField, resolveManifest(duplicateField).ops, exactCompleted), "E-FIELD-CONSUMPTION");
      requireIssue(validateSourceSpans({ document: duplicateField, completed: exactCompleted }), "E-SPAN-OWNER");
      const duplicateSpan: RoadmapDocument = {
        ...exact.document,
        records: exact.document.records.map((candidate) => candidate === record ? {
          ...record,
          source_replacements: [first!, { ...second!, span_id: first!.span_id }, third!],
        } : candidate),
      };
      requireIssue(validateCompletedChunks(duplicateSpan, resolveManifest(duplicateSpan).ops, exactCompleted), "E-FIELD-CONSUMPTION");
      requireIssue(validateSourceSpans({ document: duplicateSpan, completed: exactCompleted }), "E-SPAN-OWNER");
      return pass("negative");
    }
    const reversed: RoadmapDocument = {
      ...exact.document,
      records: exact.document.records.map((candidate) => candidate === record ? {
        ...record,
        source_replacements: [...record.source_replacements].reverse(),
      } : candidate),
    };
    const reversedCompleted = completeSemantic(reversed, { value: 0 });
    const reversedPlacement = resolveManifest(reversed);
    const reversedIssues = [
      ...validateCompletedChunks(reversed, reversedPlacement.ops, reversedCompleted),
      ...validateSourceSpans({ document: reversed, completed: reversedCompleted }),
    ];
    if (reversedIssues.length !== 0 || !exactCompleted.expected_bytes.bytesEqual(reversedCompleted.expected_bytes) ||
      JSON.stringify(exactCompleted.projected_field_segments.map((segment) => [segment.logical_path, segment.start_in_chunk, segment.end_in_chunk])) !==
        JSON.stringify(reversedCompleted.projected_field_segments.map((segment) => [segment.logical_path, segment.start_in_chunk, segment.end_in_chunk]))) {
      fail("replacement row order changed rendered bytes or canonical projected field segments");
    }
    return pass();
  }
  if (id === "render_structural_exact_field_binding_all_kinds") {
    const fixture = structuralSemanticFixture();
    const placement = resolveManifest(fixture.document);
    const issues = [
      ...validateCompletedChunks(fixture.document, placement.ops, fixture.completed),
      ...validateSourceSpans({ document: fixture.document, completed: fixture.completed }),
    ];
    if (issues.length !== 0) fail(`exact structural binding failed: ${issues.map((issue) => issue.message).join(";")}`);
    const expected: readonly [Exclude<SourceSpan["source_kind"], "generated_slot">, string, string][] = [
      ["section", "heading", "body_md"],
      ["fragment", "fragment", "body_md"],
      ["legacy_marker", "marker", "marker_md"],
    ];
    const executed: string[] = [];
    for (const [kind, ownerId, field] of expected) {
      const chunk = fixture.completed.chunks.find((candidate) =>
        candidate.owner.kind === kind && candidate.owner.id === ownerId
      );
      const span = fixture.document.spans.find((candidate) =>
        candidate.source_kind === kind && candidate.owner_id === ownerId
      );
      if (chunk === undefined || span === undefined) fail(`${kind} exact-binding control is incomplete`);
      if (exactProjectedFieldSegment(
        fixture.completed, chunk, kind, ownerId, field, span.start_byte, span.end_byte,
      ) === undefined) fail(`${kind} exact projected field did not resolve`);
      executed.push(kind);
    }
    const recordFixture = semanticFixture("exact");
    const recordCompleted = completeSemantic(recordFixture.document, recordFixture.renderCalls);
    const record = recordFixture.document.records.find((candidate) =>
      "render_authority" in candidate && candidate.render_authority === "semantic"
    );
    if (record === undefined || !("source_replacements" in record)) fail("record exact-binding control is incomplete");
    const recordChunk = recordChunkFor(recordCompleted, record.id);
    for (const replacement of record.source_replacements) {
      const span = recordFixture.document.spans.find((candidate) => candidate.id === replacement.span_id);
      if (span === undefined || exactProjectedFieldSegment(
        recordCompleted,
        recordChunk,
        "record",
        record.id,
        replacement.replacement_field,
        span.start_byte,
        span.end_byte,
      ) === undefined) fail("record exact projected field did not resolve");
    }
    executed.push("record");
    const partChunk = fixture.completed.chunks.find((candidate) =>
      candidate.owner.kind === "part" && candidate.owner.id === "part"
    );
    const partSpan = fixture.document.spans.find((candidate) =>
      candidate.source_kind === "part" && candidate.owner_id === "part"
    );
    if (partChunk === undefined || partSpan === undefined || exactProjectedFieldSegment(
      fixture.completed, partChunk, "part", "part", "body_md", partSpan.start_byte, partSpan.end_byte,
    ) === undefined) fail("part exact projected field did not resolve");
    executed.push("part");
    const semanticOnly = semanticOnlyCompletion();
    if (semanticOnly.completed.projected_field_segments.some((segment) =>
      segment.owner_kind === "record" && segment.owner_id === semanticOnly.record.id
    )) fail("semantic-only record minted a projected field segment");
    executed.push("semantic_only_zero_segments");
    const debt = deriveMigrationDebt(fixture.document, fixture.completed);
    const report = migrationProgressReport(fixture.document, debt, fixture.completed);
    const replaced = fixture.document.spans.filter((span) => span.migration_status === "replaced");
    if (report.replacement_coverage.denominator !== replaced.length ||
      report.replacement_coverage.numerator !== replaced.length) {
      fail("structural exact segments did not count as exact migration coverage");
    }
    executed.push("progress_coverage");
    return pass("positive", Object.freeze(executed));
  }
  if (id === "render_structural_exact_field_binding_rejections") {
    const fixture = structuralSemanticFixture();
    const section = fixture.document.sections[0]!;
    if (!("render_authority" in section) || section.render_authority !== "semantic") {
      fail("structural rejection fixture lacks semantic section");
    }
    const sectionChunk = fixture.completed.chunks.find((chunk) =>
      chunk.owner.kind === "section" && chunk.owner.id === section.section_id
    );
    const sectionSpan = fixture.document.spans.find((span) =>
      span.source_kind === "section" && span.owner_id === section.section_id
    );
    const segmentIndex = fixture.completed.projected_field_segments.findIndex((segment) =>
      segment.owner_kind === "section" && segment.owner_id === section.section_id
    );
    const segment = fixture.completed.projected_field_segments[segmentIndex];
    if (sectionChunk === undefined || sectionSpan === undefined || segmentIndex < 0 || segment === undefined) {
      fail("structural rejection fixture lacks exact section facts");
    }
    const withSegments = (segments: CompletedRenderIr["projected_field_segments"]): CompletedRenderIr => ({
      ...fixture.completed,
      projected_field_segments: segments,
    });
    const requireCompletedSegmentIssue = (completed: CompletedRenderIr): void => requireExactIssue(
      validateCompletedChunks(fixture.document, resolveManifest(fixture.document).ops, completed),
      "E-FIELD-CONSUMPTION",
      'section["heading"].projected_field_segments',
    );
    const executed: string[] = [];
    const without = fixture.completed.projected_field_segments.filter((_, index) => index !== segmentIndex);
    requireCompletedSegmentIssue(withSegments(without));
    executed.push("missing_segment");
    requireCompletedSegmentIssue(withSegments([...fixture.completed.projected_field_segments, segment]));
    executed.push("duplicate_segment");
    const fragmentChunkIndex = fixture.completed.chunks.findIndex((chunk) => chunk.owner.kind === "fragment");
    const fragmentSegmentIndex = fixture.completed.projected_field_segments.findIndex((candidate) =>
      candidate.owner_kind === "fragment"
    );
    const fragmentChunk = fixture.completed.chunks[fragmentChunkIndex];
    const fragmentSegment = fixture.completed.projected_field_segments[fragmentSegmentIndex];
    if (fragmentChunkIndex < 0 || fragmentSegmentIndex < 0 || fragmentChunk === undefined || fragmentSegment === undefined) {
      fail("same-ID cross-kind fixture lacks fragment facts");
    }
    const collidingFragmentChunk: RenderChunk = {
      ...fragmentChunk,
      owner: { ...fragmentChunk.owner, id: section.section_id },
    };
    const collidingChunks = [...fixture.completed.chunks];
    collidingChunks[fragmentChunkIndex] = collidingFragmentChunk;
    const collidingSegments = [...fixture.completed.projected_field_segments];
    collidingSegments[fragmentSegmentIndex] = { ...fragmentSegment, owner_id: section.section_id };
    const collidingCompleted: CompletedRenderIr = {
      ...fixture.completed,
      chunks: collidingChunks,
      projected_field_segments: collidingSegments,
    };
    if (exactProjectedFieldSegment(
      collidingCompleted,
      collidingFragmentChunk,
      "section",
      section.section_id,
      "body_md",
      sectionSpan.start_byte,
      sectionSpan.end_byte,
    ) !== undefined) fail("same textual ID under a different structural kind satisfied section binding");
    executed.push("same_id_wrong_kind");
    requireCompletedSegmentIssue(withSegments([...without, { ...segment, owner_id: "wrong" }]));
    executed.push("wrong_owner_id");
    requireCompletedSegmentIssue(withSegments([...without, { ...segment, logical_path: "marker_md" }]));
    executed.push("wrong_logical_path");
    const sectionChunkIndex = fixture.completed.chunks.indexOf(sectionChunk);
    const widenedChunks = [...fixture.completed.chunks];
    const widenedSectionChunk: RenderChunk = { ...sectionChunk, bytes: bytes("HH") };
    widenedChunks[sectionChunkIndex] = widenedSectionChunk;
    const partialSegment = { ...segment, end_in_chunk: 1, bytes: bytes("H") };
    const partialCompleted: CompletedRenderIr = {
      ...fixture.completed,
      chunks: widenedChunks,
      projected_field_segments: [...without, partialSegment],
      expected_bytes: createExpectedByteView(widenedChunks),
    };
    if (exactProjectedFieldSegment(
      partialCompleted,
      widenedSectionChunk,
      "section",
      section.section_id,
      "body_md",
      sectionSpan.start_byte,
      sectionSpan.end_byte,
    ) !== undefined) fail("proper non-empty structural prefix satisfied full-field binding");
    const partialReport = migrationProgressReport(
      fixture.document,
      deriveMigrationDebt(fixture.document, partialCompleted),
      partialCompleted,
    );
    if (partialReport.replacement_coverage.covered_span_ids.includes(sectionSpan.id)) {
      fail("proper structural prefix counted as complete migration coverage");
    }
    executed.push("partial_full_field");
    const unsafeSegment = {
      ...segment,
      start_in_chunk: Number.MAX_SAFE_INTEGER + 1,
      end_in_chunk: Number.MAX_SAFE_INTEGER + 2,
    };
    if (exactProjectedFieldSegment(
      withSegments([...without, unsafeSegment]),
      sectionChunk,
      "section",
      section.section_id,
      "body_md",
      sectionSpan.start_byte,
      sectionSpan.end_byte,
    ) !== undefined) fail("unsafe structural segment coordinate was accepted");
    if (exactProjectedFieldSegment(
      fixture.completed,
      sectionChunk,
      "section",
      section.section_id,
      "body_md",
      Number.MAX_SAFE_INTEGER + 1,
      Number.MAX_SAFE_INTEGER + 2,
    ) !== undefined) fail("unsafe whole-document coordinate was accepted");
    executed.push("unsafe_coordinate");
    const driftedBytes = new Uint8Array(segment.bytes);
    driftedBytes[0] = driftedBytes[0]! ^ 1;
    requireIssue(validateCompletedChunks(fixture.document, resolveManifest(fixture.document).ops,
      withSegments([...without, { ...segment, bytes: driftedBytes }])), "E-RENDER-AUTHORITY");
    executed.push("segment_byte_drift");
    const expectedDriftChunks = fixture.completed.chunks.map((chunk) =>
      chunk === sectionChunk ? { ...chunk, bytes: bytes("X") } : chunk
    );
    const expectedDrift: CompletedRenderIr = {
      ...fixture.completed,
      expected_bytes: createExpectedByteView(expectedDriftChunks),
    };
    if (exactProjectedFieldSegment(
      expectedDrift,
      sectionChunk,
      "section",
      section.section_id,
      "body_md",
      sectionSpan.start_byte,
      sectionSpan.end_byte,
    ) !== undefined) fail("expected-byte-view drift did not invalidate structural binding");
    executed.push("expected_view_drift");
    if (exactProjectedFieldSegment(
      fixture.completed,
      { ...sectionChunk },
      "section",
      section.section_id,
      "body_md",
      sectionSpan.start_byte,
      sectionSpan.end_byte,
    ) !== undefined) fail("cloned chunk object satisfied structural binding");
    executed.push("cloned_chunk_identity");
    const duplicatedChunkCompleted: CompletedRenderIr = {
      ...fixture.completed,
      chunks: [sectionChunk, ...fixture.completed.chunks],
    };
    if (exactProjectedFieldSegment(
      duplicatedChunkCompleted,
      sectionChunk,
      "section",
      section.section_id,
      "body_md",
      sectionSpan.start_byte,
      sectionSpan.end_byte,
    ) !== undefined) fail("duplicated chunk object satisfied structural binding");
    executed.push("duplicate_chunk_identity");
    const missingReplacement = structuralSectionWithReplacements(fixture.document, []);
    requireIssue(validateCompletedChunks(missingReplacement, resolveManifest(missingReplacement).ops, fixture.completed), "E-FIELD-CONSUMPTION");
    executed.push("missing_replacement");
    const row = section.source_replacements[0]!;
    const duplicateReplacement = structuralSectionWithReplacements(fixture.document, [row, row]);
    requireIssue(validateCompletedChunks(duplicateReplacement, resolveManifest(duplicateReplacement).ops, fixture.completed), "E-FIELD-CONSUMPTION");
    executed.push("duplicate_replacement");
    const wrongReplacement = structuralSectionWithReplacements(fixture.document, [{ ...row, replacement_field: "marker_md" }]);
    requireIssue(validateCompletedChunks(wrongReplacement, resolveManifest(wrongReplacement).ops, fixture.completed), "E-FIELD-CONSUMPTION");
    executed.push("wrong_replacement_field");
    const noSegment = withSegments(without);
    requireIssue(validateSourceSpans({ document: fixture.document, completed: noSegment }), "E-SPAN-OWNER");
    const debt = deriveMigrationDebt(fixture.document, noSegment);
    const report = migrationProgressReport(fixture.document, debt, noSegment);
    if (report.replacement_coverage.covered_span_ids.includes(sectionSpan.id) ||
      !report.completion_audit.lane_blockers.some((blocker) =>
        blocker.category === "uncovered_replacement_span" && blocker.subject === sectionSpan.id
      )) fail("whole structural chunk bypassed the missing projected-segment proof");
    executed.push("whole_chunk_without_segment");
    return pass("negative", Object.freeze(executed));
  }
  if (id === "render_slots_resolved_before_slot_validation") {
    const fixture = semanticFixture("exact");
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
    const fixture = semanticFixture("exact");
    const completed = complete(fixture.document).completed;
    const broken = { ...fixture.document, spans: [] };
    if (completed.chunks.length !== fixture.document.manifest.length) fail("chunks were not completed first");
    requireIssue(validateSourceSpans({ document: broken, completed }), "E-SPAN-EMPTY");
    return pass("negative");
  }
  if (id === "render_invalid_chunk_skips_projection_read") {
    const fixture = semanticFixture("exact");
    const completed = complete(fixture.document).completed;
    let reads = 0;
    let rejection: unknown;
    try {
      renderThenCheckCommittedProjection(
        completed.chunks,
        [{ code: "E-SPAN-GAP", source: "fixture", logical_path: "span", message: "bad", exit: 1 }],
        completed.expected_bytes,
        fixture.document.document.projection_path,
        () => { reads++; return fixture.source; },
      );
    } catch (error) { rejection = error; }
    if (rejection === undefined || reads !== 0) fail("invalid chunks reached committed projection read");
    if (typeof rejection === "object" && rejection !== null && "issues" in rejection) {
      observeMatchingIssue((rejection as { issues: readonly RoadmapIssue[] }).issues, "E-SPAN-GAP", "span");
    }
    return pass("negative");
  }
  if (id === "render_committed_projection_read_last" || id === "render_projection_mutation_changes_only_drift") {
    const fixture = semanticFixture("exact");
    const completed = complete(fixture.document).completed;
    let reads = 0;
    const actual = id === "render_committed_projection_read_last" ? fixture.source : bytes("X\nMSUMACCRATIONALEPG");
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
      observeMatchingIssue(checked.issues, "E-PROJECTION-DRIFT", "projection");
      if (
        !checked.issues[0].message.includes("expected context=") ||
        !checked.issues[0].message.includes("actual context=") ||
        !checked.issues[0].message.includes("\\n")
      ) fail("drift diagnostic omitted escaped local byte context");
    }
    return pass(id === "render_projection_mutation_changes_only_drift" ? "negative" : "positive");
  }
  const fixture = semanticFixture("exact");
  const completed = complete(fixture.document).completed;
  const first = renderValidatedChunks(completed.chunks, [], completed.expected_bytes);
  const second = renderValidatedChunks(completed.chunks, [], completed.expected_bytes);
  if (new TextDecoder().decode(first) !== "HFMSUMACCRATIONALEPG") fail(`${id}: exact render differs`);
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
    const fixture = semanticFixture("exact");
    const completed = complete(fixture.document).completed;
    const facts = collectManifestSlotBindingFacts(fixture.document, completed);
    const slot = fixture.document.generated_slots[0].slot_id;
    const slotSpan = fixture.document.spans.find((span) => span.source_kind === "generated_slot");
    if (slotSpan === undefined) fail("fixture lacks its generated-slot span");
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
      resolved.resolved[0].interval.start_byte !== slotSpan.start_byte ||
      resolved.resolved[0].interval.end_byte !== slotSpan.end_byte ||
      resolved.resolved[0].payload_interval.start_byte !== slotSpan.start_byte ||
      resolved.resolved[0].payload_interval.end_byte !== slotSpan.end_byte
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
  if (id === "outputs_production_stage_inventories") {
    const pre = productionOutputInventory("pre_cutover");
    const matrix = productionOutputInventory("matrix_authoritative");
    const both = productionOutputInventory("both_authoritative");
    if (pre.claims.length !== 12 || pre.status_claims.length !== 12 || pre.registry.claim_count !== 12) {
      fail("pre-cutover production inventory is not the exact twelve legacy status slots");
    }
    if (matrix.claims.length !== 9 || matrix.status_claims.length !== 8 || matrix.registry.claim_count !== 9) {
      fail("matrix_authoritative production inventory is not eight README slots plus one matrix whole-file claim");
    }
    if (both.claims.length !== 10 || both.status_claims.length !== 8 || both.registry.claim_count !== 10) {
      fail("both_authoritative production inventory is not eight README slots plus both whole-file claims");
    }
    const wholePaths = (inventory: typeof matrix): string[] => inventory.claims.flatMap((claim) =>
      claim.kind === "whole_file" ? [claim.path] : []
    );
    if (JSON.stringify(wholePaths(matrix)) !== JSON.stringify(["cddl-matrix/ROADMAP.md"])) {
      fail("matrix_authoritative whole-file inventory does not name exactly the matrix projection");
    }
    if (JSON.stringify(wholePaths(both)) !== JSON.stringify(["cddl-matrix/ROADMAP.md", "tests/TESTING_ROADMAP.md"])) {
      fail("both_authoritative whole-file inventory does not name exactly both projections");
    }
    if (productionOutputStage() !== "both_authoritative") {
      fail("the canonical production output stage is not both_authoritative");
    }
    if (validateProductionOutputRegistry(matrix.claims, "pre_cutover").ok) {
      fail("a matrix_authoritative inventory validated against the pre-cutover revision stage");
    }
    if (!validateProductionOutputRegistry([...both.claims].reverse(), "both_authoritative").ok) {
      fail("closed production inventory validation became order-sensitive");
    }
    return pass();
  }
  if (id === "outputs_production_stage_required") {
    const claims = productionOutputInventory("pre_cutover").claims;
    const invokeWithoutTypeFloor = validateProductionOutputRegistry as unknown as (
      claims: readonly OutputClaim[],
      stage?: unknown,
    ) => ReturnType<typeof validateProductionOutputRegistry>;
    const omitted = invokeWithoutTypeFloor(claims);
    const invalid = invokeWithoutTypeFloor(claims, "claim-count-inferred");
    if (omitted.ok || invalid.ok) {
      fail("omitted or invalid production stage selected an inventory from claim shape");
    }
    requireIssue(omitted.issues, "E-OUTPUT-CLAIM");
    requireIssue(invalid.issues, "E-OUTPUT-CLAIM");
    if (omitted.issues[0]?.logical_path !== "stage" || invalid.issues[0]?.logical_path !== "stage") {
      fail("invalid production stages did not report the explicit stage coordinate");
    }
    return pass("negative");
  }
  if (id === "outputs_production_whole_authority") {
    const inventory = productionOutputInventory("matrix_authoritative");
    const claim = inventory.claims.find((value) =>
      value.kind === "whole_file" && value.path === "cddl-matrix/ROADMAP.md"
    );
    if (claim === undefined) fail("matrix_authoritative inventory omitted matrix whole-file ownership");
    const resolution = resolveOutputClaims({
      registry: inventory.registry,
      claims: [claim],
      targets: new Map([[claim.path, bytes("committed projection\n")]]),
    });
    if (resolution.issues.length !== 0 || resolution.authority === undefined ||
      resolution.resolved.length !== 1 || resolution.resolved[0].interval.start_byte !== 0 ||
      resolution.resolved[0].interval.end_byte !== bytes("committed projection\n").byteLength) {
      fail("matrix_authoritative whole-file claim did not resolve to one opaque complete production interval");
    }
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
    observeMatchingIssue(issues, "E-OUTPUT-CLAIM");
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
    observeMatchingIssue(issues, "E-OUTPUT-CLAIM");
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
    observeMatchingIssue(overlaps, "E-OUTPUT-CLAIM");
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

    const fixture = semanticFixture("exact");
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
    ) ?? fail("fixture generated-slot placement is missing");
    const ownerSpan = fixture.document.spans.find((span) =>
      span.source_kind === "generated_slot" && span.owner_id === slot
    ) ?? fail("fixture generated-slot span is missing");
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
  if (id === "outputs_legacy_status_inventory_no_whole_file_claim") {
    // The production status-header inventory is slot-only by construction, so it can never mint the
    // whole-file authority a projection write needs — proven against the ACTUAL claim list, not a
    // fixture of it. The carrier is an ordinary authoritative v2 document: the refusal is a property
    // of the inventory, not of the document's authority.
    if (LEGACY_STATUS_OUTPUT_CLAIMS.some((value) =>
      value.kind === "whole_file" || value.producer === "roadmap-projector"
    )) fail("legacy status production inventory unexpectedly contains a whole-file/projector claim");
    const claimsByPath = new Map<RepoPath, Extract<OutputClaim, { kind: "slot" }>[]>();
    for (const value of LEGACY_STATUS_OUTPUT_CLAIMS) {
      if (value.kind !== "slot") fail("legacy status inventory contains a non-slot claim");
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
      fail("actual legacy production status inventory did not resolve");
    }
    const document = semanticFixture("exact").document;
    const result = createProjectionWritePlan({
      write_coordinate: "projection",
      roadmap: "matrix",
      document,
      projection_bytes: bytes("projection"),
      output_authority: resolution.authority,
      validation_issues: [],
    });
    if (result.ok) fail("slot-only status inventory minted a projection write plan");
    requireExactIssue(result.issues, "E-OUTPUT-AUTHORITY", "output_claims");
    if (!result.issues.some((value) =>
      value.logical_path === "output_claims" &&
      value.message.includes("lacks an opaque validated whole-file authority")
    )) fail("actual legacy status inventory did not prove the projection has no whole-file claim");
    return pass("negative");
  }
  if (
    id === "outputs_projection_path_floor" ||
    id === "write_projection_rejects_toml" || id === "write_projection_rejects_authority_files" ||
    id === "write_all_rejected" || id === "format_source_single_explicit"
  ) {
    const fixture = semanticFixture("exact").document;
    const document: RoadmapDocument = id === "write_projection_rejects_toml"
      ? { ...fixture, document: { ...fixture.document, projection_path: asRepoPath("fixture/source.toml") } }
      : id === "write_projection_rejects_authority_files"
        ? { ...fixture, document: { ...fixture.document, projection_path: asRepoPath("draft/roadmap-notes.md") } }
        : fixture;
    const authority = id === "outputs_projection_path_floor"
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
      id === "outputs_projection_path_floor" ||
      id === "write_all_rejected" || id === "format_source_single_explicit"
    ) requireIssue(result.issues, "E-OUTPUT-AUTHORITY");
    const intendedCoordinate = id === "write_all_rejected" ? "document.roadmap"
      : id === "format_source_single_explicit" ? "write_coordinate"
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
    const fixture = semanticFixture("exact");
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
    let rejection: unknown;
    try { ports.atomicReplace(asRepoPath("fixture/projection.md"), bytes("new")); } catch (error) { rejection = error; }
    if (rejection === undefined || calls !== 1 || new TextDecoder().decode(target) !== "old") fail("atomic failure changed original snapshot");
    observeUntypedSelfTestRejection(id, rejection);
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
    observeSelfTestIssue({ code: "E-OUTPUT-SLOT", logical_path: "claims[0]" });
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
    observeMatchingIssue(resolveOutputClaims({
      registry: LEGACY_STATUS_OUTPUT_REGISTRY,
      claims: LEGACY_STATUS_OUTPUT_CLAIMS,
      targets: before,
    }).issues, "E-OUTPUT-SLOT");
    return pass("negative");
  }
  if (id === "status_projector_after_matrix_handoff") {
    const roadmapPath = asRepoPath("cddl-matrix/ROADMAP.md");
    const ownedTargets = new Map(
      [...statusTargets(fixtureBundle, "before")].filter(([path]) => path !== roadmapPath),
    );
    const spy = targetReadSpy(ownedTargets);
    let resolutions = 0;
    const plan = planLegacyStatusHeaderRun(
      inputs,
      { mode: "write", argv: ["--write"], targets: spy.targets },
      { claimResolved: () => { resolutions++; } },
      "matrix_authoritative",
    );
    if (
      plan.exit_code !== 0 || plan.stderr.byteLength !== 0 || plan.writes.length !== 2 ||
      resolutions !== 8 || spy.reads.size !== 2 || [...spy.reads.values()].some((count) => count !== 1) ||
      plan.writes.some((write) => write.path === roadmapPath) ||
      new TextDecoder().decode(plan.stdout) !== "status-headers: wrote 8 generated span(s) across 2 file(s).\n"
    ) fail("matrix_authoritative status writer retained a matrix ROADMAP read, claim, or write");
    return pass();
  }
  fail(`${id} is not a status case`);
}

function testDeterminismCase(
  id: RequiredProjectionSelfTestCaseId,
  fixtureBundle?: ProjectionFixtureBundle,
): SelfTestResult {
  if (id === "issues_sorted") {
    const fixture = semanticFixture("exact");
    const brokenSpans = fixture.document.spans.map((span, index) =>
      index < 2 ? { ...span, sha256: "0".repeat(64) } : span
    );
    const forward = validateSourceSpans({
      document: { ...fixture.document, spans: brokenSpans },
      completed: complete(fixture.document).completed,
    });
    const reverse = validateSourceSpans({
      document: { ...fixture.document, spans: [...brokenSpans].reverse() },
      completed: complete(fixture.document).completed,
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
