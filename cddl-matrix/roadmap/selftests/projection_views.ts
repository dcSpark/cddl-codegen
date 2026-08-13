import type { SelfTestCandidateCase as SelfTestCase, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import type { RepoPath, RoadmapId, SectionId, SpanId } from "../model/core.ts";
import type { RoadmapDocumentV2 } from "../model/documents.ts";
import { resolveManifest } from "../manifest.ts";
import { buildExpectedChunks } from "../render_ir.ts";
import { buildProjectionViews, validateContentReachability, validateLegacySpanProvenance } from "../projection_views.ts";
import type { ProjectionLayout } from "../projection_layout.ts";
import { renderCanonicalSemanticRecord } from "../adapters/matrix.ts";
import { TESTING_ADAPTER } from "../adapters/testing.ts";
import { scanRoadmapMarkdownFacts } from "../references.ts";
import { createImmutableByteView } from "../render_ir.ts";
import { liveTestingLegacyProjection, liveTestingProjection, liveTestingV2Document } from "./live_testing.ts";
import { sha256 } from "../kernel.ts";

const UTF8 = new TextEncoder();
const TEXT = new TextDecoder();
const bytes = (value: string): Uint8Array => UTF8.encode(value);
function assert(condition: unknown, message: string): asserts condition { if (!condition) throw new Error(message); }
const pass = (subcases: readonly string[]): SelfTestResult => ({ ok: true, polarity: "positive", subcases });

function fixture(): { document: RoadmapDocumentV2; legacy: Uint8Array } {
  const section = bytes("## Fixture\n\n");
  const record = bytes("- **Visible record.** Detailed prose stays intact.\n");
  const legacy = new Uint8Array(section.byteLength + record.byteLength);
  legacy.set(section); legacy.set(record, section.byteLength);
  const sectionId = "fixture" as SectionId;
  const recordId = "matrix.fixture-visible" as RoadmapId;
  const document: RoadmapDocumentV2 = {
    document: { schema_version: 2, authority: "authoritative", roadmap: "matrix",
      source_path: "fixture/roadmap.toml" as RepoPath, projection_path: "fixture/ROADMAP.md" as RepoPath,
      frozen_source_sha256: sha256(legacy), frozen_source_byte_length: legacy.byteLength,
      frozen_source_line_count: 3, frozen_source_eof: "lf", projection_layout: "curated_v1" },
    sections: [{ section_id: sectionId, title: "Fixture", render_authority: "semantic",
      body_md: section, source_replacements: [{ span_id: "span-section" as SpanId,
        replacement_field: "body_md", review_note_md: bytes("Section source review.\n") }] }],
    fragments: [], legacy_markers: [], parts: [], generated_slots: [],
    records: [{ id: recordId, title: "Visible record", projection_group: sectionId,
      legacy_aliases: ["Legacy item 1"], render_authority: "semantic", projection_visibility: "document",
      payload: { kind: "work", summary_md: record, work_state: "ready", work_intent: "build_system",
        work_kind: "infrastructure", risk: "false_pass_or_red", family_classification: "none_reviewed",
        acceptance_md: bytes("Acceptance detail.\n"), priority_rationale_md: bytes("Priority detail.\n") },
      source_replacements: [{ span_id: "span-record" as SpanId, replacement_field: "payload.summary_md",
        review_note_md: bytes("Record source review.\n") }] }],
    manifest: [{ kind: "section", section_id: sectionId }, { kind: "record", record_id: recordId }],
    spans: [
      { id: "span-section" as SpanId, start_byte: 0, end_byte: section.byteLength, sha256: sha256(section),
        source_kind: "section", owner_id: sectionId, owner_field: "body_md", migration_status: "replaced" },
      { id: "span-record" as SpanId, start_byte: section.byteLength, end_byte: legacy.byteLength,
        sha256: sha256(record), source_kind: "record", owner_id: recordId,
        owner_field: "payload.summary_md", migration_status: "replaced" },
    ],
    relations: [{ source: recordId, kind: "related", target: recordId, note_md: bytes("Relation audit note.\n") }],
    references: [],
  };
  return { document, legacy };
}

function views() {
  const value = fixture();
  const manifest = resolveManifest(value.document);
  assert(manifest.issues.length === 0, "projection-view fixture manifest is invalid");
  const completed = buildExpectedChunks(value.document, manifest.ops, {
    renderSemanticRecord: renderCanonicalSemanticRecord,
    resolveGeneratedSlot: () => undefined,
  });
  assert(completed.build_issues.length === 0, "projection-view fixture failed to build");
  return { ...value, completed, projection: buildProjectionViews(value.document, completed, value.legacy) };
}

function liveTestingViews(document: RoadmapDocumentV2 = liveTestingV2Document()) {
  const manifest = resolveManifest(document);
  assert(manifest.issues.length === 0, "live testing manifest is invalid");
  const completed = buildExpectedChunks(document, manifest.ops, {
    renderSemanticRecord: (record, fields) => TESTING_ADAPTER.renderSemantic(record, fields),
    resolveGeneratedSlot: () => undefined,
  });
  assert(completed.build_issues.length === 0, "live testing render IR failed to build");
  return { document, projection: buildProjectionViews(document, completed, liveTestingLegacyProjection()) };
}

function testingDocumentAtLayout(layout: ProjectionLayout): RoadmapDocumentV2 {
  const document = liveTestingV2Document();
  return { ...document, document: { ...document.document, projection_layout: layout } };
}

export const REQUIRED_PROJECTION_VIEW_SELFTEST_CASE_IDS = [
  "projection_views_layout_and_provenance",
  "projection_views_content_exactly_once",
] as const;

export const PROJECTION_VIEW_SELFTEST_CASES: readonly SelfTestCase[] = Object.freeze([
  {
    id: "projection_views_layout_and_provenance", category: "manifest-render", run(): SelfTestResult {
      const value = views();
      assert(value.projection.issues.length === 0, "valid projection views reported issues");
      const text = TEXT.decode(value.projection.full);
      assert(text.startsWith("<!-- GENERATED FILE: owned by fixture/roadmap.toml;"), "ownership banner is absent");
      assert(text.includes('<a id="roadmap-id-matrix.fixture-visible"></a>\n- **Visible record.**'), "stable anchor is absent or moved from its record");
      assert(!text.includes("Acceptance detail") && !text.includes("source review"), "audit-only prose leaked into full view");
      const facts = scanRoadmapMarkdownFacts(value.document.document.projection_path, createImmutableByteView(value.projection.full));
      assert(facts.issues.length === 0 && JSON.stringify(facts.stable_anchor_ids) === '["matrix.fixture-visible"]', "anchor scanner did not return the exact stable ID");
      assert(value.projection.legacy_span_provenance.length === 2 &&
        value.projection.legacy_span_provenance[0]?.span_id === "span-section" &&
        value.projection.legacy_span_provenance[1]?.span_id === "span-record", "legacy span provenance is incomplete or unstable");
      assert(validateLegacySpanProvenance(value.document, value.projection.legacy_span_provenance).length === 0,
        "exact legacy span provenance was rejected");
      assert(validateLegacySpanProvenance(value.document, value.projection.legacy_span_provenance.slice(1))
        .some((entry) => entry.message.includes("reported 0 times")), "missing span provenance escaped");
      assert(validateLegacySpanProvenance(value.document,
        [...value.projection.legacy_span_provenance, value.projection.legacy_span_provenance[0]!])
        .some((entry) => entry.message.includes("reported 2 times")), "duplicate span provenance escaped");
      const duplicateFacts = scanRoadmapMarkdownFacts(value.document.document.projection_path,
        createImmutableByteView(bytes(`${text}\n<a id="roadmap-id-matrix.fixture-visible"></a>\n`)));
      assert(duplicateFacts.issues.some((entry) => entry.code === "E-ID-DUPLICATE"), "duplicate anchor escaped scanner");
      const malformedFacts = scanRoadmapMarkdownFacts(value.document.document.projection_path,
        createImmutableByteView(bytes('<a id="roadmap-id-matrix.Bad"></a>\n')));
      assert(malformedFacts.issues.length > 0, "malformed anchor escaped scanner");

      const stageTexts = new Map<ProjectionLayout, string>();
      for (const layout of ["legacy_v1", "anchors_v1", "standing_v1", "unnumbered_v1", "curated_v1"] as const) {
        const stage = liveTestingViews(testingDocumentAtLayout(layout));
        assert(stage.projection.issues.length === 0, `${layout} testing projection views reported issues`);
        stageTexts.set(layout, TEXT.decode(stage.projection.full));
      }
      const legacyText = stageTexts.get("legacy_v1")!;
      const anchorsText = stageTexts.get("anchors_v1")!;
      const standingText = stageTexts.get("standing_v1")!;
      const unnumberedText = stageTexts.get("unnumbered_v1")!;
      const curatedText = stageTexts.get("curated_v1")!;
      assert(legacyText === TEXT.decode(liveTestingLegacyProjection()) &&
        !legacyText.includes("<!-- GENERATED FILE:") && !legacyText.includes('id="roadmap-id-'),
      "legacy_v1 did not preserve the exact pre-anchor-layout projection");
      assert(anchorsText.startsWith("<!-- GENERATED FILE: owned by tests/testing-roadmap.toml;") &&
        anchorsText.includes('id="roadmap-id-testing.') &&
        anchorsText.includes("## Next work items, in priority order") &&
        !anchorsText.includes("## Standing-system residuals") &&
        !anchorsText.includes("### Live operational watches"),
      "anchors_v1 changed more than ownership/anchor layout");
      assert(standingText.includes("## Next work items, in priority order") &&
        (standingText.match(/^## Standing-system residuals$/gmu) ?? []).length === 1 &&
        !standingText.includes("### Live operational watches"),
      "standing_v1 did not add only the explicit Standing-system heading after anchors_v1");
      assert((unnumberedText.match(/^## Next work$/gmu) ?? []).length === 1 &&
        !unnumberedText.includes("## Next work items, in priority order") &&
        unnumberedText.includes('\n<a id="roadmap-id-testing.') && unnumberedText.includes("</a>\n- ") &&
        !unnumberedText.includes("### Live operational watches"),
      "unnumbered_v1 did not apply the Next heading/bullet layout without operational regrouping");
      assert(curatedText.includes("### Live operational watches"),
        "curated_v1 did not materialize the operational regrouping");

      const current = liveTestingViews();
      assert(current.projection.issues.length === 0, "current live testing projection views reported issues");
      assert(TEXT.decode(current.projection.full) === TEXT.decode(liveTestingProjection()),
        "current live testing stage escaped its committed exact projection bytes");
      const live = liveTestingViews(testingDocumentAtLayout("curated_v1"));
      assert(live.projection.issues.length === 0, "live testing projection views reported issues");
      const liveText = TEXT.decode(live.projection.full);
      assert(liveText === curatedText, "live testing curated projection is non-deterministic");
      assert((liveText.match(/^## Next work$/gmu) ?? []).length === 1 &&
        !liveText.includes("## Next work items, in priority order"), "Next heading rewrite is absent or non-exact");
      assert((liveText.match(/^## Standing-system residuals$/gmu) ?? []).length === 1,
        "Standing-system heading was not materialized exactly once");
      for (const heading of ["Operational systems, controls, and resource work", "Live operational watches"]) {
        assert((liveText.match(new RegExp(`^### ${heading}$`, "gmu")) ?? []).length === 1,
          `operational heading ${heading} is absent or duplicated`);
      }
      assert(!liveText.includes("### Attributed and historical operating guidance"),
        "empty relocated history bucket still rendered a heading");
      const operationalRecords = live.document.records.filter((record) => record.projection_group === "operational-watches");
      const operationalBuckets = {
        systems: operationalRecords.filter((record) => record.payload.kind !== "testing_operational_watch" &&
          record.payload.kind !== "testing_incident"),
        live: operationalRecords.filter((record) =>
          record.payload.kind === "testing_operational_watch" && record.payload.watch_state === "watching" ||
          record.payload.kind === "testing_incident" && record.payload.incident_posture === "live"),
        history: operationalRecords.filter((record) =>
          record.payload.kind === "testing_operational_watch" && record.payload.watch_state !== "watching" ||
          record.payload.kind === "testing_incident" && record.payload.incident_posture !== "live"),
      };
      assert(operationalBuckets.systems.length === 32 && operationalBuckets.live.length === 4 &&
        operationalBuckets.history.length === 0, "live operational classification counts changed");
      const systemsStart = liveText.indexOf("### Operational systems, controls, and resource work");
      const liveStart = liveText.indexOf("### Live operational watches");
      const nextSection = liveText.indexOf("\n## ", liveStart);
      const retainedMemory = '<a id="roadmap-id-testing.tier-memory.spend-measurements"></a>\n' +
        "- **Spend the measurements.**";
      assert(liveText.includes(retainedMemory),
        "retained tier-memory work is absent or still nested under an unrelated operational record");
      assert(liveText.indexOf(retainedMemory) > systemsStart && liveText.indexOf(retainedMemory) < liveStart,
        "retained tier-memory work escaped the operational systems/resource bucket");
      for (const [kind, records, start, end] of [
        ["systems", operationalBuckets.systems, systemsStart, liveStart],
        ["live", operationalBuckets.live, liveStart, nextSection],
      ] as const) {
        const bucketText = liveText.slice(start, end);
        assert(records.filter((record) => record.projection_visibility === "document")
          .every((record) => bucketText.includes(`id="roadmap-id-${record.id}"`)),
          `operational ${kind} bucket misplaced one or more classified records`);
      }
      const aliases = live.document.records.flatMap((record) => record.legacy_aliases ?? [])
        .filter((alias) => /^Next work [0-9]+$/u.test(alias)).sort();
      assert(aliases.length === 25 && !aliases.includes("Next work 9") && aliases.includes("Next work 26"),
        "live Next-work ordinal inventory changed or filled the intentional ordinal gap");
      for (const record of live.document.records.filter((candidate) =>
        candidate.legacy_aliases?.some((alias) => /^Next work [0-9]+$/u.test(alias)))) {
        const anchor = `<a id="roadmap-id-${record.id}"></a>\n- `;
        assert(liveText.includes(anchor), `Next-work record ${record.id} did not become an anchored bullet`);
      }
      const liveFacts = scanRoadmapMarkdownFacts(live.document.document.projection_path,
        createImmutableByteView(live.projection.full));
      const visibleIds = live.document.records.filter((record) => record.projection_visibility === "document").map((record) => record.id).sort();
      assert(JSON.stringify(liveFacts.stable_anchor_ids) === JSON.stringify(visibleIds) &&
        live.document.records.filter((record) => record.projection_visibility === "semantic_only")
          .every((record) => !liveFacts.stable_anchor_ids.includes(record.id)),
      "live anchors do not exactly exclude semantic-only records");
      const badHeadingDocument: RoadmapDocumentV2 = {
        ...live.document,
        sections: live.document.sections.map((section) => section.section_id === "next-priority"
          ? { ...section, body_md: bytes("## Unexpected heading\n\n") }
          : section),
      };
      assert(liveTestingViews(badHeadingDocument).projection.issues.some((entry) =>
        entry.logical_path === "projection.layout.section.next-priority"),
      "Next-heading source-prefix drift silently disabled its curated transform");
      return pass(["banner", "anchor", "layout_stages", "full_audit_separation", "span_provenance", "span_missing",
        "span_duplicate", "fragment_scan", "fragment_duplicate", "fragment_malformed"]);
    },
  },
  {
    id: "projection_views_content_exactly_once", category: "manifest-render", run(): SelfTestResult {
      const value = views();
      const ledger = value.projection.content_reachability;
      assert(ledger.length === 7 && ledger.filter((entry) => entry.view === "full").length === 2 &&
        ledger.filter((entry) => entry.view === "audit").length === 5, "full/audit authored field partition changed");
      assert(validateContentReachability(value.document, ledger, value.projection.full, value.projection.audit).length === 0,
        "exact ledger was rejected");
      assert(validateContentReachability(value.document, ledger.slice(1), value.projection.full, value.projection.audit)
        .some((entry) => entry.message.includes("assigned 0 times")), "missing field mutation escaped");
      assert(validateContentReachability(value.document, [...ledger, ledger[0]!], value.projection.full, value.projection.audit)
        .some((entry) => entry.message.includes("assigned 2 times")), "duplicate field mutation escaped");
      const altered = ledger.map((entry, index) => index === 0 ? { ...entry, output_start_byte: entry.output_start_byte + 1 } : entry);
      assert(validateContentReachability(value.document, altered, value.projection.full, value.projection.audit)
        .some((entry) => entry.message.includes("not bound")), "mismatched final-view range mutation escaped");
      const audit = TEXT.decode(value.projection.audit);
      assert(audit.includes("Acceptance detail") && audit.includes("source review") &&
        !audit.includes("Visible record.** Detailed prose stays intact"),
      "audit projection omitted audit-only prose or duplicated full prose");
      return pass(["exact", "missing", "duplicate", "mismatched_bytes"]);
    },
  },
]);
