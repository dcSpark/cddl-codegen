import type { SelfTestCandidateCase as SelfTestCase, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import type { RepoPath, RoadmapId, SectionId } from "../model/core.ts";
import type { RoadmapDocumentV3 } from "../model/documents.ts";
import { resolveSectionPlan } from "../section_plan.ts";
import { buildExpectedChunks } from "../render_ir.ts";
import { buildProjectionViews, validateContentReachability } from "../projection_views.ts";
import { renderCanonicalSemanticRecord } from "../adapters/engine.ts";
import { recordStatusFacts } from "../payload_descriptors.ts";
import { TESTING_ADAPTER } from "../adapters/testing.ts";
import { scanRoadmapMarkdownFacts } from "../repository_facts.ts";
import { createImmutableByteView } from "../render_ir.ts";
import { liveTestingV3Document } from "./live_testing.ts";

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
    sections: [{ section_id: sectionId, title: "Fixture", body_md: section, entries: [String(recordId)] }],
    parts: [],
    records: [{ id: recordId, title: "Visible record",
      legacy_aliases: ["Legacy item 1"],
      payload: { kind: "work", body_md: record, work_state: "ready", work_intent: "build_system",
        work_kind: "infrastructure", risk: "false_pass_or_red",
        acceptance_md: bytes("Acceptance detail.\n"), priority_rationale_md: bytes("Priority detail.\n") } }],
    relations: [{ source: recordId, kind: "related", target: recordId, note_md: bytes("Relation audit note.\n") }],
    references: [],
  };
  return { document };
}

function views() {
  const value = fixture();
  const plan = resolveSectionPlan(value.document);
  assert(plan.issues.length === 0, "projection-view fixture section plan is invalid");
  const completed = buildExpectedChunks(value.document, plan.ops, {
    renderSemanticRecord: renderCanonicalSemanticRecord,
    resolveGeneratedSlot: () => undefined,
  });
  assert(completed.build_issues.length === 0, "projection-view fixture failed to build");
  return { ...value, completed, projection: buildProjectionViews(value.document, completed) };
}

function liveTestingViews(document: RoadmapDocumentV3 = liveTestingV3Document()) {
  const plan = resolveSectionPlan(document);
  assert(plan.issues.length === 0, "live testing section plan is invalid");
  const completed = buildExpectedChunks(document, plan.ops, {
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
    id: "projection_views_layout_and_provenance", category: "section-render", run(): SelfTestResult {
      const value = views();
      assert(value.projection.issues.length === 0, "valid projection views reported issues");
      const text = TEXT.decode(value.projection.full);
      assert(text.startsWith("<!-- GENERATED FILE: owned by fixture/roadmap.toml;"), "ownership banner is absent");
      assert(
        text.includes('<a id="roadmap-id-matrix.fixture-visible"></a>\n<sub>work · work_state=ready · risk=false_pass_or_red</sub>\n- **Visible record.**'),
        "stable anchor plus typed status line is absent or moved from its record",
      );
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
      // The render itself is the authority: the projection is a gitignored draft/ artifact, so
      // there are no committed projection bytes to compare against — the layout assertions below
      // are the pinned surface.
      const liveText = TEXT.decode(live.projection.full);
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
      const operationalEntries = new Set(live.document.sections
        .filter((section) => String(section.section_id) === "operational-watches")
        .flatMap((section) => [...section.entries]));
      const operationalRecords = live.document.records.filter((record) => operationalEntries.has(String(record.id)));
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
      assert(operationalBuckets.systems.length === 12 && operationalBuckets.live.length === 4 &&
        operationalBuckets.history.length === 0, "live operational classification counts changed");
      const systemsStart = liveText.indexOf("### Operational systems, controls, and resource work");
      const liveStart = liveText.indexOf("### Live operational watches");
      const nextSection = liveText.indexOf("\n## ", liveStart);
      const retainedMemory = '<a id="roadmap-id-testing.tier-memory.spend-measurements"></a>\n' +
        "<sub>work · work_state=blocked · risk=resource_exhaustion</sub>\n" +
        "- **Spend the measurements.**";
      assert(liveText.includes(retainedMemory),
        "retained tier-memory work is absent or still nested under an unrelated operational record");
      assert(liveText.indexOf(retainedMemory) > systemsStart && liveText.indexOf(retainedMemory) < liveStart,
        "retained tier-memory work escaped the operational systems/resource bucket");
      const liveRecordIds = new Set(live.document.records.map((record) => String(record.id)));
      const placedIds = new Set(live.document.sections
        .flatMap((section) => [...section.entries])
        .filter((id) => liveRecordIds.has(id)));
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
      assert(aliases.length === 18 && !aliases.includes("Next work 9") && !aliases.includes("Next work 18") && !aliases.includes("Next work 19") && !aliases.includes("Next work 20") && !aliases.includes("Next work 21") && !aliases.includes("Next work 22") && !aliases.includes("Next work 24") && !aliases.includes("Next work 25") && !aliases.includes("Next work 26") && aliases.includes("Next work 27"),
        "live Next-work ordinal inventory changed or filled the intentional ordinal gap");
      const handStatusLine = (record: (typeof live.document.records)[number]): string => {
        const facts = recordStatusFacts(record.payload);
        return `<sub>${[
          String(facts.kind),
          ...facts.discriminants.map(([path, value]) => `${path}=${value}`),
          ...(facts.risk === undefined ? [] : [`risk=${facts.risk}`]),
        ].join(" · ")}</sub>`;
      };
      for (const record of live.document.records.filter((candidate) =>
        candidate.legacy_aliases?.some((alias) => /^Next work [0-9]+$/u.test(alias)))) {
        const anchor = `<a id="roadmap-id-${record.id}"></a>\n${handStatusLine(record)}\n- `;
        assert(liveText.includes(anchor), `Next-work record ${record.id} did not become an anchored, status-lined bullet`);
      }
      // Status-line uniformity oracle: every section-placed record's anchor is immediately
      // followed by a status line spelling exactly its typed facts (kind, arm discriminants,
      // risk where the arm carries one) — hand-joined here so the oracle is independent of the
      // renderer's own formatting helper.
      for (const record of live.document.records.filter((candidate) => placedIds.has(String(candidate.id)))) {
        const anchorText = `<a id="roadmap-id-${record.id}"></a>\n`;
        const at = liveText.indexOf(anchorText);
        assert(at !== -1, `record ${record.id} lost its stable anchor`);
        const afterAnchor = liveText.slice(at + anchorText.length);
        const following = afterAnchor.slice(0, afterAnchor.indexOf("\n"));
        assert(following.trimStart() === handStatusLine(record) && /^ *$/u.test(following.slice(0, following.length - following.trimStart().length)),
          `record ${record.id} lacks its typed status line behind its anchor`);
      }
      const liveFacts = scanRoadmapMarkdownFacts(live.document.document.projection_path,
        createImmutableByteView(live.projection.full));
      const visibleIds = [...placedIds].sort();
      assert(JSON.stringify(liveFacts.stable_anchor_ids) === JSON.stringify(visibleIds) &&
        live.document.records.filter((record) => !placedIds.has(record.id))
          .every((record) => !liveFacts.stable_anchor_ids.includes(record.id)),
      "live anchors do not exactly match section-placed records");
      const badHeadingDocument: RoadmapDocumentV3 = {
        ...live.document,
        sections: live.document.sections.map((section) => section.section_id === "next-priority"
          ? { ...section, body_md: bytes("## Unexpected heading\n\n") }
          : section),
      };
      assert(liveTestingViews(badHeadingDocument).projection.issues.some((entry) =>
        entry.logical_path === "projection.layout.section.next-priority"),
      "Next-heading source-prefix drift silently disabled its layout transform");
      // Blank-line hygiene is render-side and record-attributed: butt one record's prose against
      // the following section heading and the failure must name the butting owner, not a markdown
      // lint coordinate (first-authoring-run friction finding).
      const butting = fixture();
      const buttingDocument: RoadmapDocumentV3 = {
        ...butting.document,
        sections: [
          ...butting.document.sections,
          { section_id: "second" as SectionId, title: "Second", body_md: bytes("## Second\n\n"), entries: [] },
        ],
      };
      const buttingPlan = resolveSectionPlan(buttingDocument);
      assert(buttingPlan.issues.length === 0, "blank-line fixture section plan is invalid");
      const buttingCompleted = buildExpectedChunks(buttingDocument, buttingPlan.ops, {
        renderSemanticRecord: renderCanonicalSemanticRecord,
        resolveGeneratedSlot: () => undefined,
      });
      assert(buttingCompleted.build_issues.length === 0, "blank-line fixture failed to build");
      const buttingIssues = buildProjectionViews(buttingDocument, buttingCompleted).issues;
      assert(buttingIssues.some((entry) =>
        entry.logical_path === "projection.layout.blank-line.section.second" &&
        entry.message.includes('record "matrix.fixture-visible"')),
      "record prose butting against the next heading did not fail naming the butting record");
      assert(liveTestingViews().projection.issues.length === 0,
        "live render violates its own blank-line hygiene");
      return pass(["banner", "anchor", "status_line", "layout", "full_audit_separation",
        "fragment_scan", "fragment_duplicate", "fragment_malformed", "blank_line"]);
    },
  },
  {
    id: "projection_views_content_exactly_once", category: "section-render", run(): SelfTestResult {
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
