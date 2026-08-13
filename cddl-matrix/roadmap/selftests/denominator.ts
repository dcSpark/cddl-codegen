import { validateSystematicFamilies, type DenominatorAuthorityAdapter } from "../denominator.ts";
import { validateDecodedRoadmapDocument } from "../adapters/engine.ts";
import { MATRIX_ADAPTER } from "../adapters/matrix.ts";
import { composeRoadmapDocument } from "../compose.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import { buildRoadmapIndexes } from "../indexes.ts";
import type { ReferenceId, RepoPath, RoadmapId, SectionId } from "../model/core.ts";
import type { RoadmapDocumentV3, SemanticAuthorityRecord, SemanticPayload } from "../model/documents.ts";
import type { FamilyPayload } from "../model/systematic.ts";
import { resolveManifest } from "../manifest.ts";
import { buildExpectedChunks, validateCompletedChunks } from "../render_ir.ts";
import type { RegistryView, RoadmapAdapter } from "../adapters/types.ts";
import type { SelfTestCase, SelfTestResult } from "../selftest.ts";
import { observeSelfTestIssue } from "./observations.ts";

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
  return { id: recordId, title: recordId, projection_group: "fixture" as SectionId, payload };
}

function family(): FamilyPayload {
  const coordinates = [
    [[AXIS_A, A_MEMBER], [AXIS_B, B_PLAIN]],
    [[AXIS_A, A_MEMBER], [AXIS_B, B_OPTIONAL]],
    [[AXIS_A, A_GROUP], [AXIS_B, B_PLAIN]],
  ] as const;
  return {
    kind: "family", family_maturity: "closed_denominator", campaign_state: "closing",
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

function fixture(): { document: RoadmapDocumentV3; adapter: DenominatorAuthorityAdapter; view: RegistryView } {
  const payload = family();
  const records: SemanticAuthorityRecord[] = [
    record(FAMILY, payload),
    record(WORK, { kind: "work", work_state: "ready", work_intent: "build_system", work_kind: "coverage_cell", risk: "false_pass_or_red", family_id: FAMILY, acceptance_md: b("Accepted."), priority_rationale_md: b("Now.") }),
    record(CONTROL, { kind: "control", control_kind: "gate", control_state: "live", reference_ids: [ref("ref-control-pin")], claim_md: b("Runs."), boundary_md: b("Synthetic.") }),
    ...CELL_IDS.map((cellId, index) => record(id(`matrix.fixture-denominator-evidence-${["zero", "one", "two"][index]}`), {
      kind: "evidence", evidence_kind: "gate", claim_md: b("Executed."), evidence_verdict: "confirmed", freshness: "live", reference_ids: [ref(`ref-evidence-pin-${["zero", "one", "two"][index]}`)], refresh_reference_id: ref(`ref-evidence-pin-${["zero", "one", "two"][index]}`), unprobed_remainder_md: b("None."), scope: { cell_ids: [cellId], profiles: ["default", "preserve"], faces: ["rust", "wasm"] },
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
  const document: RoadmapDocumentV3 = {
    document: { schema_version: 3, roadmap: "matrix", source_path: SOURCE, projection_path: "cddl-matrix/ROADMAP.md" as RepoPath },
    sections: [{ section_id: "fixture" as SectionId, title: "Fixture", body_md: sectionBytes }], fragments: [], records, parts: [], generated_slots: [], manifest: [{ kind: "section", section_id: "fixture" as SectionId }], relations: [], references,
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

export function syntheticClosedDenominatorSource(): Uint8Array {
  return composeRoadmapDocument(fixture().document);
}

function issues(document: RoadmapDocumentV3, adapter?: DenominatorAuthorityAdapter): readonly unknown[] {
  const built = buildRoadmapIndexes(document);
  if (built.issues.length > 0) return built.issues;
  return validateSystematicFamilies(built.indexes, {} as RegistryView, new Map(adapter === undefined ? [] : [[FAMILY, adapter]]), SOURCE);
}

function pipelineIssues(document: RoadmapDocumentV3, adapter?: DenominatorAuthorityAdapter): readonly unknown[] {
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
  const decoded = decodeRoadmapSource(wire, "<closed-denominator>", "matrix");
  if (decoded.document.schema_version !== 3) {
    throw new Error("closed denominator did not decode/compose as strict schema v3");
  }
  const decodedFamily = decoded.records.find((value) => value.id === FAMILY);
  if (decodedFamily?.payload.kind !== "family" || decodedFamily.payload.family_maturity !== "closed_denominator") {
    throw new Error("closed denominator did not decode/compose as strict schema v3");
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
  const mutate = (edit: (doc: RoadmapDocumentV3, adapter: DenominatorAuthorityAdapter) => void): void => {
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

export const REQUIRED_DENOMINATOR_SELFTEST_CASE_IDS = ["denominator_v2_synthetic_authority", "denominator_v2_production_empty_registry_rejected"] as const;
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
}];
