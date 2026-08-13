import type { SelfTestCandidateCase as SelfTestCase, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import type { RepoPath, RoadmapId, SectionId } from "../model/core.ts";
import type { RoadmapDocumentV3 } from "../model/documents.ts";
import { resolveManifest } from "../manifest.ts";
import { buildExpectedChunks } from "../render_ir.ts";
import { buildProjectionViews, validateContentReachability } from "../projection_views.ts";
import { renderCanonicalSemanticRecord } from "../adapters/engine.ts";
import { TESTING_ADAPTER } from "../adapters/testing.ts";
import { scanRoadmapMarkdownFacts } from "../repository_facts.ts";
import { createImmutableByteView } from "../render_ir.ts";
import { liveTestingProjection, liveTestingV3Document } from "./live_testing.ts";

const UTF8 = new TextEncoder();
const TEXT = new TextDecoder();
const bytes = (value: string): Uint8Array => UTF8.encode(value);
function assert(condition: unknown, message: string): asserts condition { if (!condition) throw new Error(message); }
const pass = (subcases: readonly string[]): SelfTestResult => ({ ok: true, polarity: "positive", subcases });

function fixture(): { document: RoadmapDocumentV3 } {
  const section = bytes("## Fixture\n\n");
  const record = bytes("- **Visible record.** Detailed prose stays intact.\n");
  const sectionId = "fixture" as SectionId;
  const recordId = "matrix.fixture-visible" as RoadmapId;
  const document: RoadmapDocumentV3 = {
    document: { schema_version: 3, roadmap: "matrix",
      source_path: "fixture/roadmap.toml" as RepoPath, projection_path: "fixture/ROADMAP.md" as RepoPath },
    sections: [{ section_id: sectionId, title: "Fixture", body_md: section }],
    fragments: [], parts: [], generated_slots: [],
    records: [{ id: recordId, title: "Visible record", projection_group: sectionId,
      legacy_aliases: ["Legacy item 1"],
      payload: { kind: "work", detail_md: record, work_state: "ready", work_intent: "build_system",
        work_kind: "infrastructure", risk: "false_pass_or_red",
        acceptance_md: bytes("Acceptance detail.\n"), priority_rationale_md: bytes("Priority detail.\n") } }],
    manifest: [{ kind: "section", section_id: sectionId }, { kind: "record", record_id: recordId }],
    relations: [{ source: recordId, kind: "related", target: recordId, note_md: bytes("Relation audit note.\n") }],
    references: [],
  };
  return { document };
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
  return { ...value, completed, projection: buildProjectionViews(value.document, completed) };
}

function liveTestingViews(document: RoadmapDocumentV3 = liveTestingV3Document()) {
  const manifest = resolveManifest(document);
  assert(manifest.issues.length === 0, "live testing manifest is invalid");
  const completed = buildExpectedChunks(document, manifest.ops, {
    renderSemanticRecord: (record, fields) => TESTING_ADAPTER.renderSemantic(record, fields),
    resolveGeneratedSlot: () => undefined,
  });
  assert(completed.build_issues.length === 0, "live testing render IR failed to build");
  return { document, projection: buildProjectionViews(document, completed) };
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
      assert(!text.includes("Acceptance detail") && !text.includes("Relation audit note"), "audit-only prose leaked into full view");
      const facts = scanRoadmapMarkdownFacts(value.document.document.projection_path, createImmutableByteView(value.projection.full));
      assert(facts.issues.length === 0 && JSON.stringify(facts.stable_anchor_ids) === '["matrix.fixture-visible"]', "anchor scanner did not return the exact stable ID");
      const duplicateFacts = scanRoadmapMarkdownFacts(value.document.document.projection_path,
        createImmutableByteView(bytes(`${text}\n<a id="roadmap-id-matrix.fixture-visible"></a>\n`)));
      assert(duplicateFacts.issues.some((entry) => entry.code === "E-ID-DUPLICATE"), "duplicate anchor escaped scanner");
      const malformedFacts = scanRoadmapMarkdownFacts(value.document.document.projection_path,
        createImmutableByteView(bytes('<a id="roadmap-id-matrix.Bad"></a>\n')));
      assert(malformedFacts.issues.length > 0, "malformed anchor escaped scanner");

      const live = liveTestingViews();
      assert(live.projection.issues.length === 0, "live testing projection views reported issues");
      const liveText = TEXT.decode(live.projection.full);
      assert(liveText === TEXT.decode(liveTestingProjection()),
        "live testing curated projection escaped its committed exact projection bytes");
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
      assert(operationalBuckets.systems.length === 16 && operationalBuckets.live.length === 4 &&
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
      const placedIds = new Set(live.document.manifest.flatMap((entry) =>
        entry.kind === "record" ? [entry.record_id as string] : []));
      for (const [kind, records, start, end] of [
        ["systems", operationalBuckets.systems, systemsStart, liveStart],
        ["live", operationalBuckets.live, liveStart, nextSection],
      ] as const) {
        const bucketText = liveText.slice(start, end);
        assert(records.filter((record) => placedIds.has(record.id))
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
      const visibleIds = [...placedIds].sort();
      assert(JSON.stringify(liveFacts.stable_anchor_ids) === JSON.stringify(visibleIds) &&
        live.document.records.filter((record) => !placedIds.has(record.id))
          .every((record) => !liveFacts.stable_anchor_ids.includes(record.id)),
      "live anchors do not exactly match manifest-placed records");
      const badHeadingDocument: RoadmapDocumentV3 = {
        ...live.document,
        sections: live.document.sections.map((section) => section.section_id === "next-priority"
          ? { ...section, body_md: bytes("## Unexpected heading\n\n") }
          : section),
      };
      assert(liveTestingViews(badHeadingDocument).projection.issues.some((entry) =>
        entry.logical_path === "projection.layout.section.next-priority"),
      "Next-heading source-prefix drift silently disabled its curated transform");
      return pass(["banner", "anchor", "curated_layout", "full_audit_separation",
        "fragment_scan", "fragment_duplicate", "fragment_malformed"]);
    },
  },
  {
    id: "projection_views_content_exactly_once", category: "manifest-render", run(): SelfTestResult {
      const value = views();
      const ledger = value.projection.content_reachability;
      assert(ledger.length === 5 && ledger.filter((entry) => entry.view === "full").length === 2 &&
        ledger.filter((entry) => entry.view === "audit").length === 3, "full/audit authored field partition changed");
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
      assert(audit.includes("Acceptance detail") && audit.includes("Relation audit note") &&
        !audit.includes("Visible record.** Detailed prose stays intact"),
      "audit projection omitted audit-only prose or duplicated full prose");
      return pass(["exact", "missing", "duplicate", "mismatched_bytes"]);
    },
  },
]);
