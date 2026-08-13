import { validateSystematicFamilies, type DenominatorAuthorityAdapter } from "../denominator.ts";
import { validateDecodedRoadmapDocument } from "../adapters/engine.ts";
import { MATRIX_ADAPTER } from "../adapters/matrix.ts";
import { composeRoadmapDocument } from "../compose.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import { buildRoadmapIndexes } from "../indexes.ts";
import type { ReferenceId, RepoPath, RoadmapId, SectionId } from "../model/core.ts";
import type { RoadmapDocumentV2, SemanticAuthorityRecord, SemanticPayload } from "../model/documents.ts";
import type { FamilyPayload } from "../model/systematic.ts";
import { resolveManifest } from "../manifest.ts";
import { buildExpectedChunks, validateCompletedChunks } from "../render_ir.ts";
import type { RegistryView, RoadmapAdapter } from "../adapters/types.ts";
import type { SelfTestCase, SelfTestResult } from "../selftest.ts";
import { observeSelfTestIssue } from "./observations.ts";
import { FIXED_VALUE_CHOICE_MEMBER_AUTHORITY } from "../fixed_value_authority.ts";
import { extractFixedValueSourceFacts } from "../source_facts.ts";

const b = (value: string): Uint8Array => new TextEncoder().encode(`${value}\n`);
const id = (value: string): RoadmapId => value as RoadmapId;
const ref = (value: string): ReferenceId => value as ReferenceId;
const SOURCE = "cddl-matrix/roadmap.toml" as RepoPath;
const sectionBytes = b("S");
const sectionDigest = new Bun.CryptoHasher("sha256").update(sectionBytes).digest("hex");
const FAMILY = id("matrix.fixture-denominator");
const WORK = id("matrix.fixture-denominator-delivery");
const CONTROL = id("matrix.fixture-denominator-control");
const REQ = id("matrix.fixture-denominator-requirement");
const AXIS_A = id("matrix.fixture-denominator-placement");
const AXIS_B = id("matrix.fixture-denominator-modifier");
const A_MEMBER = id("matrix.fixture-denominator-member");
const A_GROUP = id("matrix.fixture-denominator-group");
const B_PLAIN = id("matrix.fixture-denominator-plain");
const B_OPTIONAL = id("matrix.fixture-denominator-optional");
const CELL_IDS = [
  id("matrix.fixture-denominator-coordinate-member-plain"),
  id("matrix.fixture-denominator-coordinate-member-optional"),
  id("matrix.fixture-denominator-coordinate-group-plain"),
] as const;
const EXCLUSION = id("matrix.fixture-denominator-exclusion-group-optional");

function record(recordId: RoadmapId, payload: SemanticPayload): SemanticAuthorityRecord {
  return { id: recordId, title: recordId, projection_group: "fixture" as SectionId, render_authority: "semantic", projection_visibility: "semantic_only", payload, source_replacements: [] };
}

function family(): FamilyPayload {
  const coordinates = [
    [[AXIS_A, A_MEMBER], [AXIS_B, B_PLAIN]],
    [[AXIS_A, A_MEMBER], [AXIS_B, B_OPTIONAL]],
    [[AXIS_A, A_GROUP], [AXIS_B, B_PLAIN]],
  ] as const;
  return {
    kind: "family", summary_md: b("Closed fixture"), family_maturity: "closed_denominator", campaign_state: "closing",
    goal_md: b("Close it."), boundary_md: b("Synthetic only."), work_ids: [WORK], authority_kind: "reviewed_relation",
    authority_reference_id: ref("authority"), derivation_md: b("Derive."), legality_rule_md: b("Three legal."),
    legality_owner_reference_id: ref("legality"), drift_check_reference_id: ref("drift"), mutation_test_reference_id: ref("mutation"),
    affected_profiles: ["default", "preserve"], affected_faces: ["rust", "wasm"], control_ids: [CONTROL],
    completion_owner_reference_id: ref("completion"), retirement_owner_reference_id: ref("retirement"),
    axes: [
      { id: AXIS_A, label: "placement", authority_reference_id: ref("axis-a"), values: [
        { id: A_GROUP, label: "group", source_reference_id: ref("group") },
        { id: A_MEMBER, label: "member", source_reference_id: ref("member") },
      ] },
      { id: AXIS_B, label: "modifier", authority_reference_id: ref("axis-b"), values: [
        { id: B_OPTIONAL, label: "optional", source_reference_id: ref("optional") },
        { id: B_PLAIN, label: "plain", source_reference_id: ref("plain") },
      ] },
    ],
    evidence_requirements: [{ id: REQ, profiles: ["default", "preserve"], faces: ["rust", "wasm"], stages: ["executed"] }],
    cells: CELL_IDS.map((cellId, index) => ({
      id: cellId, spec_legality: "legal", cell_disposition: (["supported", "safely_refused", "deliberately_unsupported"] as const)[index]!,
      affected_profiles: ["default", "preserve"], affected_faces: ["rust", "wasm"],
      coordinates: coordinates[index]!.map(([axis_id, value_id]) => ({ axis_id, value_id })),
      evidence_bindings: ["default", "preserve"].flatMap((profile) => ["rust", "wasm"].map((face) =>
        ({ requirement_id: REQ, profile, face, stage: "executed" as const, outcome: "succeeded" as const, evidence_id: id(`matrix.fixture-denominator-evidence-${["zero", "one", "two"][index]}`) })
      )),
    })),
    exclusions: [{ id: EXCLUSION, spec_legality: "illegal", reason_md: b("Not admitted."), owner_reference_id: ref("ex-owner"), source_reference_id: ref("ex-source"), liveness_reference_id: ref("ex-live"), coordinates: [{ axis_id: AXIS_A, value_id: A_GROUP }, { axis_id: AXIS_B, value_id: B_OPTIONAL }] }],
  };
}

function fixture(): { document: RoadmapDocumentV2; adapter: DenominatorAuthorityAdapter; view: RegistryView } {
  const payload = family();
  const records: SemanticAuthorityRecord[] = [
    record(FAMILY, payload),
    record(WORK, { kind: "work", summary_md: b("Work"), work_state: "ready", work_intent: "build_system", work_kind: "coverage_cell", risk: "false_pass_or_red", family_id: FAMILY, acceptance_md: b("Accepted."), priority_rationale_md: b("Now.") }),
    record(CONTROL, { kind: "control", summary_md: b("Control"), control_kind: "gate", control_state: "live", reference_ids: [ref("ref-control-pin")], claim_md: b("Runs."), boundary_md: b("Synthetic.") }),
    ...CELL_IDS.map((cellId, index) => record(id(`matrix.fixture-denominator-evidence-${["zero", "one", "two"][index]}`), {
      kind: "evidence", summary_md: b("Evidence"), evidence_kind: "gate", claim_md: b("Executed."), evidence_verdict: "confirmed", freshness: "live", reference_ids: [ref(`ref-evidence-pin-${["zero", "one", "two"][index]}`)], refresh_reference_id: ref(`ref-evidence-pin-${["zero", "one", "two"][index]}`), unprobed_remainder_md: b("None."), scope: { cell_ids: [cellId], profiles: ["default", "preserve"], faces: ["rust", "wasm"] },
    })),
  ];
  const gate = (referenceId: string, source = FAMILY) => ({ id: ref(referenceId), source, kind: "gate" as const, gate_id: referenceId });
  const heading = (referenceId: string) => ({ id: ref(referenceId), source: FAMILY, kind: "file_heading" as const, path: "README.md" as RepoPath, heading: referenceId });
  const passage = (referenceId: string) => ({ id: ref(referenceId), source: FAMILY, kind: "spec_passage" as const, document: "fixture", passage: referenceId });
  const references = [
    { id: ref("authority"), source: FAMILY, kind: "roadmap" as const, target_id: FAMILY },
    heading("legality"), gate("drift"), gate("mutation"), gate("completion"), gate("retirement"),
    passage("axis-a"), passage("axis-b"), passage("group"), passage("member"), passage("optional"), passage("plain"),
    heading("ex-owner"), passage("ex-source"), gate("ex-live"), gate("ref-control-pin", CONTROL),
    ...CELL_IDS.map((_, index) => gate(`ref-evidence-pin-${["zero", "one", "two"][index]}`, id(`matrix.fixture-denominator-evidence-${["zero", "one", "two"][index]}`))),
  ];
  const document: RoadmapDocumentV2 = {
    document: { schema_version: 2, authority: "authoritative", roadmap: "matrix", source_path: SOURCE, projection_path: "cddl-matrix/ROADMAP.md" as RepoPath, frozen_source_sha256: sectionDigest, frozen_source_byte_length: sectionBytes.byteLength, frozen_source_line_count: 1, frozen_source_eof: "lf" },
    sections: [{ section_id: "fixture" as SectionId, title: "Fixture", render_authority: "semantic", body_md: sectionBytes, source_replacements: [{ span_id: "fixture-section" as never, replacement_field: "body_md", review_note_md: b("Reviewed") }] }], fragments: [], legacy_markers: [], records, parts: [], generated_slots: [], manifest: [{ kind: "section", section_id: "fixture" as SectionId }], spans: [{ id: "fixture-section" as never, start_byte: 0, end_byte: sectionBytes.byteLength, sha256: sectionDigest, source_kind: "section", owner_id: "fixture", owner_field: "body_md", migration_status: "replaced" }], relations: [], references,
  };
  const derived = {
    axes: payload.axes.map((axis) => ({ id: axis.id, value_ids: axis.values.map((value) => value.id) })),
    candidates: [
      ...payload.cells.map((cell) => ({ coordinates: cell.coordinates, spec_legality: "legal" as const, affected_profiles: cell.affected_profiles, affected_faces: cell.affected_faces, expected_disposition: cell.cell_disposition as "supported" | "safely_refused" | "deliberately_unsupported", expected_outcomes: cell.evidence_bindings?.map(({ requirement_id, profile, face, stage, outcome }) => ({ requirement_id, profile, face, stage, outcome })) })),
      ...payload.exclusions.map((exclusion) => ({ coordinates: exclusion.coordinates, spec_legality: "illegal" as const, affected_profiles: [] as string[], affected_faces: [] as string[] })),
    ],
    evidence_requirements: payload.evidence_requirements,
    legal_cell_floor: 3,
    evidence_binding_floor: 12,
  };
  const adapter = { family_id: FAMILY, authority_kind: "reviewed_relation" as const, authority_reference_id: ref("authority"), derive: () => derived } satisfies DenominatorAuthorityAdapter;
  return { document, adapter, view: {} as RegistryView };
}

export function syntheticClosedDenominatorV2Source(): Uint8Array {
  return composeRoadmapDocument(fixture().document);
}

function issues(document: RoadmapDocumentV2, adapter?: DenominatorAuthorityAdapter): readonly unknown[] {
  const built = buildRoadmapIndexes(document);
  if (built.issues.length > 0) return built.issues;
  return validateSystematicFamilies(built.indexes, {} as RegistryView, new Map(adapter === undefined ? [] : [[FAMILY, adapter]]), SOURCE);
}

function pipelineIssues(document: RoadmapDocumentV2, adapter?: DenominatorAuthorityAdapter): readonly unknown[] {
  const roadmapAdapter = {
    roadmap: "matrix", namespace: "matrix", source_path: SOURCE,
    projection_path: "cddl-matrix/ROADMAP.md" as RepoPath,
    validateExtension() {}, renderSemantic: () => new Uint8Array(), referenceProviders: () => [],
    slotResolvers: () => new Map(), validateFloors() {},
  } satisfies RoadmapAdapter<SemanticPayload>;
  const view = {
    revision: { kind: "worktree" }, production_output_stage: "both_authoritative",
    gates: document.references.filter((value) => value.kind === "gate").map((value) => ({ id: value.gate_id, kind: "fixture", stub: false })),
    tracked_headings: document.references.filter((value) => value.kind === "file_heading").map((value) => ({ path: value.path, heading: value.heading, span: { start_byte: 0, end_byte: 1 } })),
    matrix_features: [], matrix_roles: [], matrix_cells: [], test_symbols: [], roadmap_citations: [], current_guards: [], output_claims: [], matrix_status_inputs: {} as never,
  } as RegistryView;
  return validateDecodedRoadmapDocument(
    document,
    view,
    roadmapAdapter,
    MATRIX_ADAPTER.referenceProviders(view),
    () => {},
    { defer_foreign_roadmap_joins: false, denominator_authorities: new Map(adapter === undefined ? [] : [[FAMILY, adapter]]) },
  ).issues;
}

const SUBCASES = ["valid", "production_empty_registry", "full_pipeline_injected", "full_pipeline_empty_registry", "real_completed_render", "missing_axis_value", "derived_extra_axis_value", "derived_extra_legal_cell", "authored_extra_cell", "legality_flip", "duplicate_coordinate", "unknown_disposition", "disposition_drift", "loose_evidence", "missing_binding", "duplicate_binding", "extra_binding", "outcome_drift", "wrong_evidence_scope", "uncovered_applicability", "affected_face_drift", "as_of_evidence", "stale_evidence", "zero_floor", "nan_floor", "fractional_floor", "stale_control", "missing_exclusion_liveness"] as const;

function run(): void {
  const base = fixture();
  const wire = composeRoadmapDocument(base.document);
  const decoded = decodeRoadmapSource(wire, "<closed-denominator-v2>", "matrix");
  if (decoded.document.schema_version !== 2) {
    throw new Error("closed denominator did not decode/compose as strict schema v2");
  }
  const decodedFamily = (decoded as RoadmapDocumentV2).records.find((value) => value.id === FAMILY);
  if (decodedFamily?.render_authority !== "semantic" ||
    decodedFamily.payload.kind !== "family" || decodedFamily.payload.family_maturity !== "closed_denominator") {
    throw new Error("closed denominator did not decode/compose as strict schema v2");
  }
  const validIssues = issues(base.document, base.adapter);
  if (validIssues.length !== 0) throw new Error(`valid synthetic closed denominator failed: ${JSON.stringify(validIssues)}`);
  if (issues(base.document).length === 0) throw new Error("production empty authority registry accepted closure");
  const pipelineValidIssues = pipelineIssues(base.document, base.adapter);
  if (pipelineValidIssues.length !== 0) throw new Error(`full decoded validation pipeline rejected synthetic closure: ${JSON.stringify(pipelineValidIssues)}`);
  if (pipelineIssues(base.document).length === 0) throw new Error("full decoded validation pipeline accepted closure with production empty registry");
  const placement = resolveManifest(base.document);
  const completed = buildExpectedChunks(base.document, placement.ops, {
    renderSemanticRecord: (record, fields) => MATRIX_ADAPTER.renderSemantic(record, fields),
    resolveGeneratedSlot: () => undefined,
  });
  const completedIssues = [...placement.issues, ...validateCompletedChunks(base.document, placement.ops, completed)];
  if (completedIssues.length !== 0) throw new Error(`real completed-render validation rejected synthetic closure: ${JSON.stringify(completedIssues)}`);
  const mutate = (edit: (doc: RoadmapDocumentV2, adapter: DenominatorAuthorityAdapter) => void): void => {
    const value = fixture();
    edit(value.document, value.adapter);
    if (issues(value.document, value.adapter).length === 0) throw new Error("denominator mutation passed");
  };
  mutate((doc) => { (doc.records[0]!.payload as FamilyPayload).axes[0]!.values = []; });
  mutate((_doc, adapter) => { const original = adapter.derive; (adapter as { derive: DenominatorAuthorityAdapter["derive"] }).derive = (view) => ({ ...original(view), axes: [...original(view).axes, { id: id("matrix.extra-axis"), value_ids: [id("matrix.extra-value")] }] }); });
  mutate((_doc, adapter) => { const original = adapter.derive; (adapter as { derive: DenominatorAuthorityAdapter["derive"] }).derive = (view) => ({ ...original(view), candidates: [...original(view).candidates, { coordinates: [{ axis_id: AXIS_A, value_id: A_GROUP }, { axis_id: AXIS_B, value_id: B_PLAIN }], spec_legality: "legal", affected_profiles: ["default"], affected_faces: ["rust"] }] }); });
  mutate((doc) => { const f = doc.records[0]!.payload as FamilyPayload; f.cells.push({ ...f.cells[0]!, id: id("matrix.extra-cell"), coordinates: [{ axis_id: AXIS_A, value_id: A_GROUP }, { axis_id: AXIS_B, value_id: B_OPTIONAL }] }); });
  mutate((_doc, adapter) => { const original = adapter.derive; (adapter as { derive: DenominatorAuthorityAdapter["derive"] }).derive = (view) => ({ ...original(view), candidates: original(view).candidates.map((value, index) => index === 0 ? { ...value, spec_legality: "illegal" } : value) }); });
  mutate((doc) => { const f = doc.records[0]!.payload as FamilyPayload; f.cells[1]!.coordinates = [...f.cells[0]!.coordinates]; });
  mutate((doc) => { (doc.records[0]!.payload as FamilyPayload).cells[0]!.cell_disposition = "unknown"; });
  mutate((doc) => { (doc.records[0]!.payload as FamilyPayload).cells[0]!.cell_disposition = "safely_refused"; });
  mutate((doc) => { (doc.records[0]!.payload as FamilyPayload).cells[0]!.evidence_ids = [id("matrix.loose")]; });
  mutate((doc) => { (doc.records[0]!.payload as FamilyPayload).cells[0]!.evidence_bindings = []; });
  mutate((doc) => { const binding = (doc.records[0]!.payload as FamilyPayload).cells[0]!.evidence_bindings![0]!; (doc.records[0]!.payload as FamilyPayload).cells[0]!.evidence_bindings!.push({ ...binding }); });
  mutate((doc) => { const bindings = (doc.records[0]!.payload as FamilyPayload).cells[0]!.evidence_bindings!; bindings.push({ ...bindings[0]!, stage: "compiled" }); });
  mutate((doc) => { (doc.records[0]!.payload as FamilyPayload).cells[0]!.evidence_bindings![0]!.outcome = "inapplicable"; });
  mutate((doc) => { const evidence = doc.records.find((value) => value.id === id("matrix.fixture-denominator-evidence-zero"))!.payload; if (evidence.kind === "evidence") evidence.scope.faces = ["wasm"]; });
  mutate((doc) => { (doc.records[0]!.payload as FamilyPayload).evidence_requirements[0]!.profiles = ["default"]; });
  mutate((doc) => { (doc.records[0]!.payload as FamilyPayload).cells[0]!.affected_faces = ["rust"]; });
  mutate((doc) => { const evidence = doc.records.find((value) => value.id === id("matrix.fixture-denominator-evidence-zero"))!.payload; if (evidence.kind === "evidence") { evidence.freshness = "as_of"; evidence.observed_at = "2026-08-12" as never; delete evidence.refresh_reference_id; } });
  mutate((doc) => { const evidence = doc.records.find((value) => value.id === id("matrix.fixture-denominator-evidence-zero"))!.payload; if (evidence.kind === "evidence") { evidence.freshness = "stale"; evidence.observed_at = "2026-08-12" as never; evidence.at_commit = "a".repeat(40) as never; delete evidence.refresh_reference_id; } });
  mutate((_doc, adapter) => { const original = adapter.derive; (adapter as { derive: DenominatorAuthorityAdapter["derive"] }).derive = (view) => ({ ...original(view), legal_cell_floor: 0 }); });
  mutate((_doc, adapter) => { const original = adapter.derive; (adapter as { derive: DenominatorAuthorityAdapter["derive"] }).derive = (view) => ({ ...original(view), legal_cell_floor: Number.NaN }); });
  mutate((_doc, adapter) => { const original = adapter.derive; (adapter as { derive: DenominatorAuthorityAdapter["derive"] }).derive = (view) => ({ ...original(view), evidence_binding_floor: 1.5 }); });
  mutate((doc) => { const control = doc.records.find((value) => value.id === CONTROL)!.payload; if (control.kind === "control") control.control_state = "stale"; });
  mutate((doc) => { doc.references = doc.references.filter((value) => value.id !== ref("ex-live")); });
}

const FIXED_VALUE_ENUM = `
pub enum FixedValue {
  Null,
  Undefined,
  Bool(bool),
  Nint(i128),
  Uint(u64),
  Float(f64),
  Text(String),
  Bytes(Vec<u8>),
}
`;

const FIXED_VALUE_LOWERING = `
fn type2_to_fixed_value(value: &Type2) -> Option<FixedValue> {
  match value {
    _ => Some(FixedValue::Null),
    _ => Some(FixedValue::Undefined),
    _ => Some(FixedValue::Bool(true)),
    _ => Some(FixedValue::Nint(-1)),
    _ => Some(FixedValue::Uint(1)),
    _ => Some(FixedValue::Float(1.0)),
    _ => Some(FixedValue::Text(String::new())),
    _ => Some(FixedValue::Bytes(Vec::new())),
  }
}
`;

const FIXED_ROWS = [
  ["contain.choice-member.prelude.true.fixed-kind", "prelude.true", "t = true / tstr", true],
  ["contain.choice-member.type2.value.bytes.fixed-kind", "type2.value", "t = h'CAFE' / tstr", true],
  ["contain.choice-member.type2.value.float.fixed-kind", "type2.value", "t = 1.5 / tstr", false],
  ["contain.choice-member.type2.value.nint.fixed-kind", "type2.value", "t = -1 / null / tstr", false],
  ["contain.choice-member.prelude.null.fixed-kind", "prelude.null", "t = null / tstr / uint", true],
  ["contain.choice-member.type2.value.text.fixed-kind", "type2.value", 't = "x" / uint', true],
  ["contain.choice-member.type2.value.uint.fixed-kind", "type2.value", "t = 5 / tstr", true],
  ["contain.choice-member.prelude.undefined.fixed-kind", "prelude.undefined", "t = undefined / tstr", true],
] as const;

function fixedValueView(): RegistryView {
  return {
    fixed_value_source: extractFixedValueSourceFacts(FIXED_VALUE_ENUM, FIXED_VALUE_LOWERING),
    matrix_containment: FIXED_ROWS.map(([rowId, feature, example]) => ({ id: rowId, role: "role.choice-member", feature, spec: "allowed", example })),
    matrix_support: FIXED_ROWS.map(([rowId, , , supported]) => ({
      id: rowId,
      status: supported ? "supported" : "unsupported",
      evidence: supported
        ? "probe (cell): cddl-codegen exit 0; compiles=ok; round-trips=ok; wasm round-trips"
        : "probe (cell): cddl-codegen exit 1",
      emission: {},
    })),
  } as unknown as RegistryView;
}

function fixedValueAuthorityMutations(): void {
  const rejects = (action: () => unknown, label: string): void => {
    try { action(); } catch { return; }
    throw new Error(`FixedValue authority mutation passed: ${label}`);
  };
  const sourceMutation = (rust: string, lowering = FIXED_VALUE_LOWERING, label = "source"): void =>
    rejects(() => FIXED_VALUE_CHOICE_MEMBER_AUTHORITY.derive({
      ...fixedValueView(), fixed_value_source: extractFixedValueSourceFacts(rust, lowering),
    }), label);
  sourceMutation(FIXED_VALUE_ENUM.replace("  Bytes(Vec<u8>),\n", ""), FIXED_VALUE_LOWERING.replace("    _ => Some(FixedValue::Bytes(Vec::new())),\n", ""), "enum remove");
  sourceMutation(FIXED_VALUE_ENUM.replace("Bool(bool)", "Boolean(bool)"), FIXED_VALUE_LOWERING.replace("FixedValue::Bool", "FixedValue::Boolean"), "enum rename");
  sourceMutation(FIXED_VALUE_ENUM.replace("Bool(bool)", "Bool(u8)"), FIXED_VALUE_LOWERING, "payload drift");
  sourceMutation(FIXED_VALUE_ENUM.replace("  Bytes(Vec<u8>),", "  Bytes(Vec<u8>),\n  Extra(u8),"), FIXED_VALUE_LOWERING.replace("    _ => Some(FixedValue::Bytes(Vec::new())),", "    _ => Some(FixedValue::Bytes(Vec::new())),\n    _ => Some(FixedValue::Extra(0)),"), "enum add");
  rejects(() => extractFixedValueSourceFacts(FIXED_VALUE_ENUM, FIXED_VALUE_LOWERING.replace("    _ => Some(FixedValue::Float(1.0)),\n", "")), "lowering mismatch");

  const deriveRejects = (edit: (view: { matrix_containment: any[]; matrix_support: any[] }) => void, label: string): void => {
    const view = fixedValueView() as RegistryView & { matrix_containment: any[]; matrix_support: any[] };
    view.matrix_containment = view.matrix_containment.map((value) => ({ ...value }));
    view.matrix_support = view.matrix_support.map((value) => ({ ...value }));
    edit(view);
    rejects(() => FIXED_VALUE_CHOICE_MEMBER_AUTHORITY.derive(view), label);
  };
  deriveRejects((view) => { view.matrix_containment.pop(); view.matrix_support.pop(); }, "row missing");
  deriveRejects((view) => { view.matrix_containment.push({ ...view.matrix_containment[0], id: "contain.choice-member.prelude.false.fixed-kind" }); view.matrix_support.push({ ...view.matrix_support[0], id: "contain.choice-member.prelude.false.fixed-kind" }); }, "row extra");
  for (const field of ["feature", "role", "spec", "example"] as const) {
    deriveRejects((view) => { view.matrix_containment[0][field] = "drift"; }, `${field} drift`);
  }
  deriveRejects((view) => { view.matrix_support[0].status = "unsupported"; }, "support disposition drift");
  deriveRejects((view) => { view.matrix_support[0].evidence = "cddl-codegen exit 0; compiles=ok; round-trips=ok; wasm compiles only"; }, "fake wasm compile-only surface");

  const derived = FIXED_VALUE_CHOICE_MEMBER_AUTHORITY.derive(fixedValueView());
  const outcomes = derived.candidates.flatMap((candidate) => candidate.expected_outcomes ?? []);
  if (derived.candidates.length !== 8 || outcomes.length !== 36 ||
    outcomes.filter((value) => value.outcome === "succeeded").length !== 30 ||
    outcomes.filter((value) => value.outcome === "safely_refused").length !== 2 ||
    outcomes.filter((value) => value.outcome === "inapplicable").length !== 4 ||
    derived.candidates.some((candidate) => candidate.affected_profiles.join() !== "default") ||
    derived.candidates.filter((candidate) => candidate.expected_disposition === "supported").some((candidate) => candidate.affected_faces.join() !== "rust,wasm") ||
    derived.candidates.filter((candidate) => candidate.expected_disposition === "safely_refused").some((candidate) => candidate.affected_faces.join() !== "rust")) {
    throw new Error("FixedValue authority did not derive the exact 8-cell/36-outcome profile and face matrix");
  }
}

export const REQUIRED_DENOMINATOR_SELFTEST_CASE_IDS = ["denominator_v2_synthetic_authority", "denominator_v2_production_empty_registry_rejected", "fixed_value_choice_member_authority_mutations"] as const;
export const DENOMINATOR_SELFTEST_CASES: readonly SelfTestCase[] = [{
  id: "denominator_v2_synthetic_authority", category: "denominator", run(): SelfTestResult {
    try { run(); return { ok: true, polarity: "positive", subcases: SUBCASES }; }
    catch (error) { return { ok: false, polarity: "positive", issues: [{ code: "E-SELFTEST-CASE", source: "<selftest>", logical_path: "denominator_v2_synthetic_authority", message: error instanceof Error ? error.message : String(error), exit: 1 }], subcases: SUBCASES }; }
  },
}, {
  id: "denominator_v2_production_empty_registry_rejected", category: "denominator", run(): SelfTestResult {
    const rejected = issues(fixture().document);
    const expectedPath = `record[${JSON.stringify(FAMILY)}].payload.family_maturity`;
    const expected = { code: "E-SCHEMA-STATE" as const, logical_path: expectedPath };
    if (!rejected.some((value) => (value as { code?: string }).code === "E-SCHEMA-STATE" &&
      (value as { logical_path?: string }).logical_path === expectedPath)) {
      return { ok: false, polarity: "negative", expected, issues: [{ code: "E-SELFTEST-CASE", source: "<selftest>", logical_path: "denominator_v2_production_empty_registry_rejected", message: "empty production registry did not reject closure", exit: 1 }] };
    }
    observeSelfTestIssue({ code: "E-SCHEMA-STATE", logical_path: expectedPath });
    return { ok: true, polarity: "negative", expected };
  },
}, {
  id: "fixed_value_choice_member_authority_mutations", category: "denominator", run(): SelfTestResult {
    const subcases = ["enum_add", "enum_remove", "enum_rename", "payload_drift", "lowering_mismatch", "containment_missing", "containment_extra", "feature_drift", "role_drift", "spec_drift", "example_drift", "support_disposition_drift", "profile_face_exact", "exact_36_outcomes", "fake_wasm_compile_only_rejected"] as const;
    try { fixedValueAuthorityMutations(); return { ok: true, polarity: "positive", subcases }; }
    catch (error) { return { ok: false, polarity: "positive", issues: [{ code: "E-SELFTEST-CASE", source: "<selftest>", logical_path: "fixed_value_choice_member_authority_mutations", message: error instanceof Error ? error.message : String(error), exit: 1 }], subcases }; }
  },
}];
