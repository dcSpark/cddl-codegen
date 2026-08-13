import type { RoadmapSelfTestPorts } from "../io.ts";
import { documentSlots, placeholderFor } from "../slots.ts";
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
  PartId,
  RepoPath,
  RoadmapId,
  SectionId,
  SlotId,
} from "../model/core.ts";
import type {
  RenderNodeKind,
  RoadmapDocument,
  SemanticPayload,
} from "../model/documents.ts";
import type { MatrixStatusInputs } from "../model/matrix.ts";
import type {
  StatusCompatibilityDiagnosticFixture,
  StatusCompatibilityInputsWire,
  StatusCompatibilityModeFixture,
} from "../selftest.ts";
import { resolveSectionPlan } from "../section_plan.ts";
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
import { renderValidatedChunks } from "../render.ts";
import { PROJECTION_PATH_BY_ROADMAP } from "../projection_paths.ts";
import { stableJsonValue } from "../query.ts";
import { renderCanonicalSemanticRecord } from "../adapters/engine.ts";
import {
  LEGACY_STATUS_OUTPUT_CLAIMS,
  LEGACY_STATUS_OUTPUT_REGISTRY,
  classifyLegacyStatusHeaderInvocation,
  deriveMatrixStatusFacts,
  planLegacyStatusHeaderRun,
  renderMatrixStatusPayloads,
} from "../matrix_status_facts.ts";
import {
  collectSectionSlotBindingFacts,
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
import { sha256 } from "../kernel.ts";

/**
 * The projection packet's stable executable registry.  Detailed case bodies are installed beside
 * the implementation; keeping the complete ID inventory here makes an omitted reviewer vector a
 * typechecked, greppable failure instead of an inferred count.
 */
export const REQUIRED_PROJECTION_SELFTEST_CASE_IDS = [
  "section_entry_duplicate_record",
  "section_entry_missing_part",
  "section_entry_orphan_record",
  "section_entry_unknown_id",
  "section_entry_non_rendering_record",
  "section_record_table_order_irrelevant",
  "section_true_sequence_preserved",
  "section_entry_duplicate_not_tiebroken",
  "section_semantic_only_is_not_placed",
  "span_utf8_byte_offsets",
  "span_mid_scalar_boundary",
  "render_zero_chunks_rejected",
  "render_no_implicit_lf",
  "render_semantic_consumption_once",
  "render_semantic_only_zero_byte_consumption",
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
  "render_chunks_precede_consumption_validation",
  "render_slots_resolved_before_slot_validation",
  "render_undeclared_slot_placeholder_rejected",
  "render_invalid_chunk_rejected_before_bytes",
  "outputs_interval_overlap",
  "outputs_interval_utf8_bytes",
  "outputs_section_slot_binding_owner",
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

const encoder = new TextEncoder();

function bytes(value: string): Uint8Array {
  return encoder.encode(value);
}

function asRoadmapId(value: string): RoadmapId { return value as RoadmapId; }
function asSectionId(value: string): SectionId { return value as SectionId; }
function asPartId(value: string): PartId { return value as PartId; }
function asSlotId(value: string): SlotId { return value as SlotId; }
function asRepoPath(value: string): RepoPath { return value as RepoPath; }

function complete(document: RoadmapDocument): { readonly completed: CompletedRenderIr; readonly planIssues: readonly RoadmapIssue[] } {
  const placement = resolveSectionPlan(document);
  const completed = buildExpectedChunks(document, placement.ops, {
    renderSemanticRecord: renderCanonicalSemanticRecord,
    resolveGeneratedSlot(slot) {
      return { binding: slot.binding, bytes: bytes("G") };
    },
  });
  return { completed, planIssues: placement.issues };
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
  const chunks: RenderChunk[] = ["a", "é", "🚀", "z"].map((value, plan_index) => ({
    plan_index,
    owner: { kind: "part", id: `chunk-${plan_index}`, field: "body_md" },
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
  const chunks: RenderChunk[] = ["one\n", "two", "three\n"].map((value, plan_index) => ({
    plan_index,
    owner: { kind: "part", id: `hash-${plan_index}`, field: "body_md" },
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
  const chunks: RenderChunk[] = ["left", "right"].map((value, plan_index) => ({
    plan_index,
    owner: { kind: "part", id: `render-${plan_index}`, field: "body_md" },
    bytes: bytes(value),
    source_span_ids: [],
    consumed_fields: ["body_md"],
  }));
  const rendered = renderValidatedChunks(chunks, [], createExpectedByteView(chunks));
  if (new TextDecoder().decode(rendered) !== "leftright") fail("renderer inserted bytes");
}

function withEntries(
  document: RoadmapDocument,
  entries: readonly string[],
): RoadmapDocument {
  return { ...document, sections: document.sections.map((section) => ({ ...section, entries })) };
}

function testSectionPlanCase(id: RequiredProjectionSelfTestCaseId): SelfTestResult {
  const fixture = semanticFixture("exact");
  let document = fixture.document;
  const entries = document.sections[0]!.entries;
  let expected: RoadmapIssue["code"] | undefined;
  switch (id) {
    case "section_entry_duplicate_record":
    case "section_entry_duplicate_not_tiebroken":
      document = withEntries(document, [...entries, String(document.records[0].id)]);
      expected = "E-SECTION-DUPLICATE";
      break;
    case "section_entry_missing_part":
      document = withEntries(document, entries.filter((entry) => entry !== "part"));
      expected = "E-SECTION-ORPHAN";
      break;
    case "section_entry_orphan_record":
      document = withEntries(document, entries.filter((entry) => entry !== String(document.records[0].id)));
      expected = "E-SECTION-ORPHAN";
      break;
    case "section_entry_unknown_id":
      document = withEntries(document, entries.map((entry) =>
        entry === String(document.records[0].id) ? "matrix.fixture-unknown" : entry
      ));
      expected = "E-SECTION-UNKNOWN";
      break;
    case "section_entry_non_rendering_record": {
      const record = document.records[0]!;
      if (record.payload.kind !== "work") fail("section-plan vector payload drifted");
      const { body_md: _detail, ...payload } = record.payload;
      document = { ...document, records: [{ ...record, payload: payload as SemanticPayload }] };
      expected = "E-SECTION-KIND";
      break;
    }
    case "section_record_table_order_irrelevant": {
      const second = {
        ...document.records[0],
        id: asRoadmapId("matrix.fixture-second"),
        title: "Second",
      };
      const placed = withEntries(document, [...entries, String(second.id)]);
      const forward = resolveSectionPlan({ ...placed, records: [document.records[0], second] });
      const reverse = resolveSectionPlan({ ...placed, records: [second, document.records[0]] });
      if (forward.issues.length !== 0 || reverse.issues.length !== 0) fail("valid section entries rejected");
      if (forward.ops.map((op) => op.node.id).join("|") !== reverse.ops.map((op) => op.node.id).join("|")) {
        fail("record table order changed presentation");
      }
      return pass();
    }
    case "section_true_sequence_preserved": {
      const reversed = withEntries(document, [...entries].reverse());
      const resolved = resolveSectionPlan(reversed);
      if (resolved.ops[0]?.node.kind !== "section" ||
        resolved.ops[1]?.node.id !== entries.at(-1) ||
        resolved.ops.at(-1)?.node.id !== entries[0]) {
        fail("authored entry sequence was reordered");
      }
      return pass();
    }
    case "section_semantic_only_is_not_placed": {
      const semanticOnly = semanticOnlyCompletion();
      if (semanticOnly.placement.issues.length !== 0 ||
        semanticOnly.placement.ops.some((op) => op.node.kind === "record" && op.node.id === semanticOnly.record.id)) {
        fail("semantic-only record did not remain an unplaced first-class semantic owner");
      }
      const section = semanticOnly.document.sections[0]!;
      const authored = {
        ...semanticOnly.document,
        sections: [{ ...section, entries: [...section.entries, String(semanticOnly.record.id)] }],
      };
      const rejected = resolveSectionPlan(authored).issues;
      if (!rejected.some((entry) => entry.code === "E-SECTION-KIND" &&
        entry.logical_path === `section[${JSON.stringify(String(section.section_id))}].entries[${section.entries.length}]`)) {
        fail("authored non-rendering entry was not rejected at its exact coordinate");
      }
      return pass();
    }
    default:
      fail(`${id} is not a section-plan case`);
  }
  const issues = resolveSectionPlan(document).issues;
  if (expected === undefined) fail("section-plan test lacks expected code");
  requireIssue(issues, expected);
  return pass("negative");
}

function validateFixture(document: RoadmapDocument): readonly RoadmapIssue[] {
  const placement = resolveSectionPlan(document);
  const completed = complete(document).completed;
  return [
    ...placement.issues,
    ...validateCompletedChunks(document, placement.ops, completed),
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
  if (id === "span_utf8_byte_offsets" || id === "span_mid_scalar_boundary") {
    const chunks: RenderChunk[] = [{
      plan_index: 0,
      owner: { kind: "section", id: "unicode", field: "body_md" },
      bytes: bytes("é"),
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
  fail(`${id} is not a span case`);
}

function semanticFixture(
  mode: "exact" | "missing" | "duplicate" = "exact",
): {
  readonly document: RoadmapDocument;
  readonly renderCalls: { value: number };
} {
  const sectionId = asSectionId("heading");
  const recordId = asRoadmapId("matrix.fixture-work");
  const partId = asPartId("part");
  const slotId = asSlotId("status-slot");
  const entries: readonly string[] = [String(recordId), String(partId)];
  const document: RoadmapDocument = {
    document: {
      schema_version: 3,
      roadmap: "matrix",
      source_path: asRepoPath("fixture/matrix.toml"),
      projection_path: asRepoPath("fixture/matrix.md"),
    },
    sections: [{
      section_id: sectionId,
      title: "Heading",
      body_md: bytes(`H${placeholderFor(slotId)}F`),
      entries,
      slots: [{ slot_id: slotId, binding: "fixture-status" }],
    }],
    records: [{
      id: recordId,
      title: "Work",
      payload: {
        kind: "work",
        body_md: bytes("DET"),
        work_state: "ready",
        work_intent: "repair",
        work_kind: "feature",
        risk: "cosmetic",
        evidence_ids: [asRoadmapId("matrix.fixture-evidence-a"), asRoadmapId("matrix.fixture-evidence-b")],
        acceptance_md: bytes("ACC"),
        priority_rationale_md: bytes("RATIONALE"),
      },
    }],
    parts: [{
      part_id: partId,
      parent_record_id: recordId,
      body_md: bytes("P"),
    }],
    relations: [],
    references: [],
  };
  const calls = { value: 0 };
  Object.defineProperty(document, "__selftest_mode", { value: mode, enumerable: false });
  Object.defineProperty(document, "__render_calls", { value: calls, enumerable: false });
  return { document, renderCalls: calls };
}

function completeSemantic(document: RoadmapDocument, calls: { value: number }): CompletedRenderIr {
  const mode = (document as RoadmapDocument & { __selftest_mode?: string }).__selftest_mode ?? "exact";
  const placement = resolveSectionPlan(document);
  return buildExpectedChunks(document, placement.ops, {
    renderSemanticRecord(record, fields) {
      calls.value++;
      const payload = record.payload;
      if (payload.kind !== "work" || payload.work_state !== "ready" || payload.body_md === undefined ||
        payload.acceptance_md === undefined) return new Uint8Array();
      const first = fields.consume("payload.body_md", payload.body_md);
      if (mode === "duplicate") fields.consume("payload.body_md", payload.body_md);
      if (mode !== "missing") fields.consume("payload.acceptance_md", payload.acceptance_md);
      fields.consume("payload.priority_rationale_md", payload.priority_rationale_md);
      return new Uint8Array(first);
    },
    resolveGeneratedSlot(slot) { return { binding: slot.binding, bytes: bytes("G") }; },
  });
}

function recordChunkFor(completed: CompletedRenderIr, id: string): RenderChunk {
  const chunks = completed.chunks.filter((chunk) => chunk.owner.kind === "record" && chunk.owner.id === id);
  if (chunks.length !== 1) fail(`expected one record chunk for ${id}`);
  return chunks[0]!;
}

function semanticOnlyCompletion(): {
  readonly document: RoadmapDocument;
  readonly record: RoadmapDocument["records"][number];
  readonly placement: ReturnType<typeof resolveSectionPlan>;
  readonly completed: CompletedRenderIr;
} {
  const exact = semanticFixture("exact");
  const record = exact.document.records[0];
  if (record === undefined) fail("unplaced render vector lacks a record");
  if (record.payload.kind !== "work") fail("unplaced render vector payload drifted");
  const { body_md: _detail, ...payload } = record.payload;
  const unplacedRecord = { ...record, payload: payload as SemanticPayload };
  const document: RoadmapDocument = {
    ...exact.document,
    records: exact.document.records.map((candidate) => candidate === record ? unplacedRecord : candidate),
    sections: exact.document.sections.map((section) => ({
      ...section,
      entries: section.entries.filter((entry) => entry !== String(unplacedRecord.id)),
    })),
  };
  const placement = resolveSectionPlan(document);
  const completed = buildExpectedChunks(document, placement.ops, {
    renderSemanticRecord: renderCanonicalSemanticRecord,
    resolveGeneratedSlot(slot) { return { binding: slot.binding, bytes: bytes("G") }; },
  });
  return { document, record: unplacedRecord, placement, completed };
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
    const placement = resolveSectionPlan(fixture.document);
    const completed = completeSemantic(fixture.document, fixture.renderCalls);
    if (completed.chunks.length !== placement.ops.length) fail("field failure prevented chunk completion");
    requireExactIssue(
      validateCompletedChunks(fixture.document, placement.ops, completed),
      "E-FIELD-CONSUMPTION",
      'record["matrix.fixture-work"]',
    );
    if (id === "render_chunks_precede_consumption_validation") {
      const raw = semanticFixture("exact");
      const rawPlacement = resolveSectionPlan(raw.document);
      const rawCompleted = complete(raw.document).completed;
      const reordered: CompletedRenderIr = {
        ...rawCompleted,
        chunks: [rawCompleted.chunks[1], rawCompleted.chunks[0], ...rawCompleted.chunks.slice(2)],
      };
      requireIssue(validateCompletedChunks(raw.document, rawPlacement.ops, reordered), "E-RENDER-AUTHORITY");

      const exactFixture = semanticFixture("exact");
      const exactPlacement = resolveSectionPlan(exactFixture.document);
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
        slot_resolutions: [...rawCompleted.slot_resolutions, { ...slotLedger, plan_index: 999 }],
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
      fail("semantic-only record minted a section-plan operation, chunk, or projected field segment");
    }
    if (ledger === undefined || ledger.expected_fields.length !== 2 || ledger.consumed_fields.length !== 2) {
      fail("unplaced record did not explicitly consume every Markdown field");
    }
    return pass();
  }
  if (id === "render_slots_resolved_before_slot_validation") {
    const fixture = semanticFixture("exact");
    const placement = resolveSectionPlan(fixture.document);
    let resolverCalls = 0;
    const completed = buildExpectedChunks(fixture.document, placement.ops, {
      renderSemanticRecord: () => new Uint8Array(),
      resolveGeneratedSlot: (slot) => { resolverCalls++; return { binding: `${slot.binding}-wrong`, bytes: bytes("G") }; },
    });
    if (resolverCalls !== 1 || completed.chunks.length !== placement.ops.length) fail("slot did not resolve during chunk build");
    requireIssue(validateCompletedChunks(fixture.document, placement.ops, completed), "E-OUTPUT-SLOT");
    return pass("negative");
  }
  if (id === "render_undeclared_slot_placeholder_rejected") {
    // The other half of the bijection: prose may not place a slot nothing declared, or the
    // placeholder would survive verbatim into the projection.
    const fixture = semanticFixture("exact");
    const section = fixture.document.sections[0]!;
    const document: RoadmapDocument = {
      ...fixture.document,
      sections: [{
        ...section,
        body_md: bytes(`H${placeholderFor(asSlotId("status-slot"))}F${placeholderFor(asSlotId("absent"))}`),
      }],
    };
    const placement = resolveSectionPlan(document);
    const completed = buildExpectedChunks(document, placement.ops, {
      renderSemanticRecord: () => new Uint8Array(),
      resolveGeneratedSlot(slot) { return { binding: slot.binding, bytes: bytes("G") }; },
    });
    requireIssue(completed.build_issues, "E-OUTPUT-SLOT");
    return pass("negative");
  }
  if (id === "render_invalid_chunk_rejected_before_bytes") {
    // With the committed-projection seam retired, the property left to pin is that a validation
    // issue rejects the render before any final projection bytes are produced.
    const fixture = semanticFixture("exact");
    const completed = complete(fixture.document).completed;
    let rejection: unknown;
    try {
      renderValidatedChunks(
        completed.chunks,
        [{ code: "E-RENDER-AUTHORITY", source: "fixture", logical_path: "span", message: "bad", exit: 1 }],
        completed.expected_bytes,
        {
          hashSegmentVisited: () => {},
          combinedHashBufferAllocated: () => {},
          finalProjectionAllocated: () => fail("invalid chunks reached final projection allocation"),
        },
      );
    } catch (error) { rejection = error; }
    if (rejection === undefined) fail("invalid chunks were rendered");
    if (typeof rejection === "object" && rejection !== null && "issues" in rejection) {
      observeMatchingIssue((rejection as { issues: readonly RoadmapIssue[] }).issues, "E-RENDER-AUTHORITY", "span");
    }
    return pass("negative");
  }
  const fixture = semanticFixture("exact");
  const completed = complete(fixture.document).completed;
  const first = renderValidatedChunks(completed.chunks, [], completed.expected_bytes);
  const second = renderValidatedChunks(completed.chunks, [], completed.expected_bytes);
  if (new TextDecoder().decode(first) !== "HGFDETP") fail(`${id}: exact render differs`);
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
            binding: { kind: "section_slot", roadmap: "matrix", slot_id: leftSlot },
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
          binding: { kind: "section_slot", roadmap: "matrix", slot_id: rightSlot },
          cardinality: { exact: 1 },
        },
      };
      const sectionSlots = [
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
        section_slots: sectionSlots,
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
  if (id === "outputs_section_slot_binding_owner") {
    const fixture = semanticFixture("exact");
    const completed = complete(fixture.document).completed;
    const facts = collectSectionSlotBindingFacts(fixture.document, completed);
    const slot = documentSlots(fixture.document.sections)[0]!.slot_id;
    const sectionClaim: OutputClaim = {
      kind: "slot",
      producer: "roadmap-projector",
      path: fixture.document.document.projection_path,
      slot_id: slot,
      interval: {
        kind: "binding",
        binding: { kind: "section_slot", roadmap: "matrix", slot_id: slot },
        cardinality: { exact: 1 },
      },
    };
    const resolved = resolveOutputClaims({
      registry: closedOutputRegistry([sectionClaim]),
      claims: [sectionClaim],
      targets: new Map(),
      section_slots: facts,
    });
    const resolution = completed.slot_resolutions[0]!;
    const sectionChunkIndex = completed.chunks.findIndex((chunk) =>
      chunk.owner.kind === "section" && chunk.owner.id === resolution.section_id
    );
    const chunkStart = completed.expected_bytes.prefix_offsets[sectionChunkIndex]!;
    const slotStart = chunkStart + resolution.start_in_chunk;
    const slotEnd = chunkStart + resolution.end_in_chunk;
    if (
      resolved.issues.length !== 0 || resolved.resolved.length !== 1 ||
      resolved.resolved[0].interval.start_byte !== slotStart ||
      resolved.resolved[0].interval.end_byte !== slotEnd ||
      resolved.resolved[0].payload_interval.start_byte !== slotStart ||
      resolved.resolved[0].payload_interval.end_byte !== slotEnd
    ) fail("section-slot owner did not resolve its exact completed chunk without a projection read");
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
      registry: closedOutputRegistry([sectionClaim]),
      claims: [sectionClaim],
      targets: new Map(),
      section_slots: collectSectionSlotBindingFacts(fixture.document, mismatchedCompleted),
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
    if (JSON.stringify(wholePaths(matrix)) !== JSON.stringify([PROJECTION_PATH_BY_ROADMAP.matrix])) {
      fail("matrix_authoritative whole-file inventory does not name exactly the matrix projection");
    }
    if (
      JSON.stringify(wholePaths(both)) !==
        JSON.stringify([PROJECTION_PATH_BY_ROADMAP.matrix, PROJECTION_PATH_BY_ROADMAP.testing])
    ) {
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
      value.kind === "whole_file" && value.path === PROJECTION_PATH_BY_ROADMAP.matrix
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
        binding: { kind: "section_slot", roadmap: "matrix", slot_id: live.slot_id },
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
    const sectionClaims = ["one", "two"].map((value): OutputClaim => {
      const slotId = asSlotId(value);
      return {
        kind: "slot",
        producer: "roadmap-projector",
        path,
        slot_id: slotId,
        interval: {
          kind: "binding",
          binding: { kind: "section_slot", roadmap: "matrix", slot_id: slotId },
          cardinality: { exact: 1 },
        },
      };
    });
    const resolved = resolveOutputClaims({
      registry: closedOutputRegistry(sectionClaims),
      claims: sectionClaims,
      targets: new Map(),
      section_slots: sectionClaims.map((value, index) => {
        if (value.kind !== "slot") return fail("unexpected whole-file section claim");
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
    const slot = documentSlots(fixture.document.sections)[0]!.slot_id;
    const sectionClaim: OutputClaim = {
      kind: "slot",
      producer: "roadmap-projector",
      path: fixture.document.document.projection_path,
      slot_id: slot,
      interval: {
        kind: "binding",
        binding: { kind: "section_slot", roadmap: "matrix", slot_id: slot },
        cardinality: { exact: 1 },
      },
    };
    const sectionRegistry = closedOutputRegistry([sectionClaim]);
    const declaringSection = fixture.document.sections[0]!;
    const declaration = (declaringSection.slots ?? [])[0]!;
    const withSection = (section: RoadmapDocument["sections"][number]): RoadmapDocument => ({
      ...fixture.document,
      sections: [section],
    });
    const slotCases: readonly [string, RoadmapDocument][] = [
      ["section_slot_zero", withSection({ ...declaringSection, slots: undefined })],
      ["section_slot_two_declarations", {
        ...fixture.document,
        sections: [declaringSection, { ...declaringSection, section_id: asSectionId("second") }],
      }],
      ["section_slot_two_placements", withSection({
        ...declaringSection,
        body_md: bytes(`H${placeholderFor(declaration.slot_id)}${placeholderFor(declaration.slot_id)}F`),
      })],
    ] as const;
    for (const [label, mutatedDocument] of slotCases) {
      requireIssue(resolveOutputClaims({
        registry: sectionRegistry,
        claims: [sectionClaim],
        targets: new Map(),
        section_slots: collectSectionSlotBindingFacts(mutatedDocument, completed),
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
    const fixtureRendered = (() => {
      const completed = complete(fixture.document).completed;
      return renderValidatedChunks(completed.chunks, [], completed.expected_bytes);
    })();
    const readOnly = new Proxy({
      readDeclared: (_path: RepoPath) => fixtureRendered,
      readDeclaredAtCommit: () => fixtureRendered,
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
      // Check renders in memory only; the one port interaction it may have is the read arm.
      const completed = complete(fixture.document).completed;
      const rendered = renderValidatedChunks(completed.chunks, [], completed.expected_bytes);
      const served = readOnly.readDeclared(fixture.document.document.source_path);
      if (sha256(rendered) !== sha256(new Uint8Array(served))) {
        fail("read-only check render drifted from its port-served snapshot");
      }
    } else {
      const value = stableJsonValue(new Map([["query", { owner_id: "query" }]])) as Record<string, unknown>;
      if (Object.keys(value).length !== 1) fail("read-only query omitted its structured result");
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

function testDeterminismCase(id: RequiredProjectionSelfTestCaseId): SelfTestResult {
  if (id === "issues_sorted") {
    const fixture = semanticFixture("exact");
    const second = { ...fixture.document.records[0]!, id: asRoadmapId("matrix.fixture-second") };
    const orphaned = {
      ...fixture.document,
      records: [...fixture.document.records, second],
      sections: fixture.document.sections.map((section) => ({ ...section, entries: [] })),
    };
    const forward = resolveSectionPlan(orphaned).issues;
    const reverse = resolveSectionPlan({ ...orphaned, records: [...orphaned.records].reverse() }).issues;
    const coordinates = (issues: readonly RoadmapIssue[]): string =>
      issues.map((value) => `${value.code}:${value.logical_path}`).join("|");
    if (coordinates(forward) !== coordinates(reverse) || !coordinates(forward).includes("record")) {
      fail("issue order depends on source table insertion order");
    }
    return pass();
  }
  if (id === "json_sorted_keys") {
    const entries = ["z", "ä", "a"].map((value) => [value, { owner_id: value }] as const);
    const forward = JSON.stringify(stableJsonValue(new Map(entries)));
    const reverse = JSON.stringify(stableJsonValue(new Map([...entries].reverse())));
    if (forward !== reverse || !(forward.indexOf('"a"') < forward.indexOf('"z"')) ||
      !(forward.indexOf('"z"') < forward.indexOf('"ä"'))) {
      fail("structured query JSON objects do not have canonical key order");
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
  fail(`${id} is not a determinism case`);
}

type ProjectionCaseExecutor = (
  id: RequiredProjectionSelfTestCaseId,
  fixtureBundle?: ProjectionFixtureBundle,
) => SelfTestResult;

interface ProjectionCaseSpec {
  readonly category: SelfTestCategory;
  readonly run: ProjectionCaseExecutor;
  /** Set on the cases whose bodies read the committed status-compatibility fixture bundle. */
  readonly fixture?: true;
}

/**
 * Every registered case names its category and its executor here, once.  The mapped key type makes
 * the table total over the frozen ID inventory, so a renamed, added, or removed case is a
 * typecheck failure instead of a silent reroute into whichever executor a name prefix happened to
 * select.
 */
const PROJECTION_CASES: { readonly [K in RequiredProjectionSelfTestCaseId]: ProjectionCaseSpec } = {
  section_entry_duplicate_record: { category: "section-render", run: testSectionPlanCase },
  section_entry_missing_part: { category: "section-render", run: testSectionPlanCase },
  section_entry_orphan_record: { category: "section-render", run: testSectionPlanCase },
  section_entry_unknown_id: { category: "section-render", run: testSectionPlanCase },
  section_entry_non_rendering_record: { category: "section-render", run: testSectionPlanCase },
  section_record_table_order_irrelevant: { category: "section-render", run: testSectionPlanCase },
  section_true_sequence_preserved: { category: "section-render", run: testSectionPlanCase },
  section_entry_duplicate_not_tiebroken: { category: "section-render", run: testSectionPlanCase },
  section_semantic_only_is_not_placed: { category: "section-render", run: testSectionPlanCase },
  span_utf8_byte_offsets: { category: "section-render", run: testSpanCase },
  span_mid_scalar_boundary: { category: "section-render", run: testSpanCase },
  render_zero_chunks_rejected: { category: "section-render", run: testRenderCase },
  render_no_implicit_lf: { category: "section-render", run: testRenderCase },
  render_semantic_consumption_once: { category: "section-render", run: testRenderCase },
  render_semantic_only_zero_byte_consumption: { category: "section-render", run: testRenderCase },
  outputs_duplicate_whole: { category: "output-ownership", run: testOutputCase },
  outputs_whole_vs_slot: { category: "output-ownership", run: testOutputCase },
  outputs_duplicate_slot: { category: "output-ownership", run: testOutputCase },
  outputs_duplicate_binding: { category: "output-ownership", run: testOutputCase },
  outputs_overlapping_slots: { category: "output-ownership", run: testOutputCase },
  outputs_path_escape: { category: "output-ownership", run: testOutputCase },
  outputs_empty_inventory: { category: "output-ownership", run: testOutputCase },
  outputs_legacy_status_inventory_no_whole_file_claim: { category: "output-ownership", run: testOutputCase },
  outputs_matrix_handoff_collision: { category: "output-ownership", run: testOutputCase },
  outputs_projection_path_floor: { category: "output-ownership", run: testOutputCase },
  outputs_slot_cardinality: { category: "output-ownership", run: testOutputCase },
  write_check_read_only_port: { category: "output-ownership", run: testOutputCase },
  write_query_read_only_port: { category: "output-ownership", run: testOutputCase },
  write_projection_rejects_toml: { category: "output-ownership", run: testOutputCase },
  write_projection_rejects_authority_files: { category: "output-ownership", run: testOutputCase },
  write_all_rejected: { category: "output-ownership", run: testOutputCase },
  format_source_single_explicit: { category: "output-ownership", run: testOutputCase },
  atomic_write_failure_preserves_target: { category: "output-ownership", run: testOutputCase },
  issues_sorted: { category: "determinism-purity", run: testDeterminismCase },
  json_sorted_keys: { category: "determinism-purity", run: testDeterminismCase },
  lexical_not_locale_sort: { category: "determinism-purity", run: testDeterminismCase },
  two_clean_renders_equal: { category: "determinism-purity", run: testRenderCase },
  no_clock_without_as_of: { category: "determinism-purity", run: testDeterminismCase },
  render_chunks_precede_consumption_validation: { category: "section-render", run: testRenderCase },
  render_slots_resolved_before_slot_validation: { category: "section-render", run: testRenderCase },
  render_undeclared_slot_placeholder_rejected: { category: "section-render", run: testRenderCase },
  render_invalid_chunk_rejected_before_bytes: { category: "section-render", run: testRenderCase },
  outputs_interval_overlap: { category: "output-ownership", run: testOutputCase },
  outputs_interval_utf8_bytes: { category: "output-ownership", run: testOutputCase },
  outputs_section_slot_binding_owner: { category: "output-ownership", run: testOutputCase },
  outputs_live_status_claims_all_twelve: { category: "output-ownership", run: testOutputCase },
  outputs_production_stage_inventories: { category: "output-ownership", run: testOutputCase },
  outputs_production_stage_required: { category: "output-ownership", run: testOutputCase },
  outputs_production_whole_authority: { category: "output-ownership", run: testOutputCase },
  status_facts_derive_fixture_parity: { category: "status-compat", run: testStatusCase, fixture: true },
  status_projector_before_after_target_byte_parity: { category: "status-compat", run: testStatusCase, fixture: true },
  status_projector_before_after_mode_parity: { category: "status-compat", run: testStatusCase, fixture: true },
  status_projector_before_after_message_parity: { category: "status-compat", run: testStatusCase, fixture: true },
  status_projector_preflight_no_partial_write: { category: "status-compat", run: testStatusCase, fixture: true },
  status_projector_after_matrix_handoff: { category: "status-compat", run: testStatusCase, fixture: true },
  span_expected_byte_view_cross_chunk: { category: "section-render", run: testSpanCase },
  span_expected_byte_view_incremental_hash: { category: "section-render", run: testSpanCase },
};

export const PROJECTION_SELFTEST_CASES: readonly SelfTestCase[] = Object.freeze(
  REQUIRED_PROJECTION_SELFTEST_CASE_IDS.map((id) => {
    const spec = PROJECTION_CASES[id];
    return {
      id,
      category: spec.category,
      run: (context: SelfTestContext) => spec.run(
        id,
        spec.fixture === true ? projectionFixtureBundleFromPorts(context.ports) : undefined,
      ),
    };
  }),
);

function projectionFixtureBundleFromPorts(ports: RoadmapSelfTestPorts): ProjectionFixtureBundle {
  const root = asRepoPath("cddl-matrix/roadmap/fixtures");
  return createProjectionFixtureBundle(new Map(PROJECTION_FIXTURE_PATHS.map((path) => [
    path,
    ports.fixtures.readFixtureFile(root, path as FixtureRelativePath),
  ])));
}

