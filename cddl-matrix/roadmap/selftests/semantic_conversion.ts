import { migrationProgressReport, type MigrationDebt, type IndependentDebtKey, type DebtOwnerKey } from "../debt.ts";
import type { RoadmapName, RoadmapId, SpanId } from "../model/core.ts";
import type { RoadmapDocument, RoadmapDocumentV1 } from "../model/documents.ts";
import type { CompletedRenderIr } from "../render_ir.ts";
import {
  semanticConversionCompletionAudit,
  semanticConversionState,
  validateSemanticConversionCompletion,
  validateSemanticConversionDeclaration,
  validateSemanticConversionTransition,
} from "../semantic_conversion.ts";
import type {
  SelfTestCandidateCase as SelfTestCase,
  SelfTestCandidateResult as SelfTestResult,
} from "../selftest.ts";

export const REQUIRED_SEMANTIC_CONVERSION_SELFTEST_CASE_IDS = [
  "completion_rejects_raw_record_owner",
  "completion_rejects_raw_section_owner",
  "completion_rejects_raw_fragment_owner",
  "completion_rejects_raw_part_owner",
  "completion_rejects_raw_legacy_marker_owner",
  "completion_rejects_raw_span",
  "completion_rejects_frozen_span",
  "completion_rejects_semantic_shadow",
  "completion_rejects_uncovered_replacement_span",
  "completion_rejects_raw_subordinate_lifecycle",
  "completion_rejects_inferred_transition",
  "completion_rejects_pending_family_classification",
  "completion_rejects_unrendered_field",
  "completion_allows_typed_stale_unknown_and_unmodelled",
  "completion_reports_unresolved_cross_roadmap_join",
  "completion_accepts_zero_debt",
  "completion_transition_forward_allowed",
  "completion_transition_stable_allowed",
  "completion_transition_reverse_rejected",
  "completion_transition_historical_omission_is_converting",
  "completion_declaration_v0_forbidden",
  "completion_declaration_current_v1_required",
  "completion_declaration_historical_v1_omission_allowed",
] as const;

type MutableV1Meta = RoadmapDocumentV1["document"] & {
  semantic_conversion?: "converting" | "complete";
};

function document(
  state: "converting" | "complete" | "omitted" = "complete",
  roadmap: RoadmapName = "matrix",
): RoadmapDocumentV1 {
  const meta: MutableV1Meta = {
    schema_version: 1,
    authority: "authoritative",
    roadmap,
    source_path: `${roadmap === "matrix" ? "cddl-matrix/roadmap" : "tests/testing-roadmap"}.toml` as never,
    projection_path: `${roadmap === "matrix" ? "cddl-matrix/ROADMAP" : "tests/TESTING_ROADMAP"}.md` as never,
    frozen_source_sha256: "0".repeat(64),
    frozen_source_byte_length: 1,
    frozen_source_line_count: 1,
    frozen_source_eof: "lf",
    frozen_legacy_span_ids: [],
    ...(state === "omitted" ? {} : { semantic_conversion: state }),
  };
  return {
    document: meta,
    sections: [],
    fragments: [],
    legacy_markers: [],
    records: [],
    parts: [],
    generated_slots: [],
    manifest: [],
    spans: [],
    relations: [],
    references: [],
  };
}

function emptyCompleted(): CompletedRenderIr {
  return {
    chunks: [],
    field_consumption: [],
    projected_field_segments: [],
    slot_resolutions: [],
    build_issues: [],
  } as unknown as CompletedRenderIr;
}

function debt(
  owners: readonly { key: DebtOwnerKey; state: "raw_unclassified" | "raw_with_semantic_shadow" | "semantic" }[] = [],
  independent: readonly IndependentDebtKey[] = [],
  frozen: readonly DebtOwnerKey[] = [],
): MigrationDebt {
  const ownerIndex = (key: DebtOwnerKey) => JSON.stringify([key.roadmap, key.owner_kind, key.owner_id, key.owner_field]);
  const independentIndex = (key: IndependentDebtKey) => JSON.stringify([
    key.roadmap, key.category, key.owner.owner_kind, key.owner.owner_id, key.owner.owner_field, key.subject,
  ]);
  return {
    owners: new Map(owners.map((value) => [ownerIndex(value.key), value])),
    independent: new Map(independent.map((value) => [independentIndex(value), value])),
    frozen_legacy_spans: new Map(frozen.map((key) => [ownerIndex(key), key])),
  };
}

const owner = (
  owner_kind: Exclude<DebtOwnerKey["owner_kind"], "source_span">,
): DebtOwnerKey => ({
  roadmap: "matrix",
  owner_kind,
  owner_id: `fixture-${owner_kind}` as never,
  owner_field: owner_kind === "record" ? "source_block_md"
    : owner_kind === "legacy_marker" ? "source_block_md"
    : "source_block_md",
} as DebtOwnerKey);

const spanOwner = (id = "fixture-span"): DebtOwnerKey => ({
  roadmap: "matrix",
  owner_kind: "source_span",
  owner_id: id as SpanId,
  owner_field: "coverage",
});

function independent(category: IndependentDebtKey["category"]): IndependentDebtKey {
  return {
    roadmap: "matrix",
    category,
    owner: owner("record"),
    subject: `fixture-${category}`,
  };
}

function expect(condition: boolean, message: string): void {
  if (!condition) throw new Error(message);
}

function pass(): SelfTestResult {
  return { ok: true, polarity: "positive" };
}

function run(id: string): SelfTestResult {
  const completed = emptyCompleted();
  if (id.startsWith("completion_rejects_raw_") && id.endsWith("_owner")) {
    const kind = id.slice("completion_rejects_raw_".length, -"_owner".length) as
      "record" | "section" | "fragment" | "part" | "legacy_marker";
    const issues = validateSemanticConversionCompletion(
      document(),
      debt([{ key: owner(kind), state: "raw_unclassified" }]),
      completed,
    );
    expect(issues.some((issue) => issue.message.includes("raw_content_owner")), `${id}: raw owner did not block complete`);
    return pass();
  }
  switch (id) {
    case "completion_rejects_raw_span": {
      const value = document();
      value.spans.push({
        id: "fixture-span" as SpanId, start_byte: 0, end_byte: 1, sha256: "0".repeat(64),
        source_kind: "record", owner_id: "matrix.fixture" , owner_field: "source_block_md",
        migration_status: "raw",
      });
      expect(validateSemanticConversionCompletion(value, debt(), completed).some((issue) => issue.message.includes("raw_span")), "raw span did not block complete");
      return pass();
    }
    case "completion_rejects_frozen_span": {
      expect(validateSemanticConversionCompletion(document(), debt([], [], [spanOwner()]), completed)
        .some((issue) => issue.message.includes("frozen_span")), "frozen span did not block complete");
      return pass();
    }
    case "completion_rejects_semantic_shadow": {
      const value = document();
      value.records.push({
        id: "matrix.fixture" as RoadmapId, title: "Fixture", projection_group: "fixture" as never,
        render_authority: "raw", source_block_md: new Uint8Array([1]), span_ids: [],
        semantic_shadow: { kind: "work" } as never,
      });
      expect(validateSemanticConversionCompletion(value, debt(), completed)
        .some((issue) => issue.message.includes("semantic_shadow")), "semantic shadow did not block complete");
      return pass();
    }
    case "completion_rejects_uncovered_replacement_span": {
      const value = document();
      value.spans.push({
        id: "fixture-span" as SpanId, start_byte: 0, end_byte: 1, sha256: "0".repeat(64),
        source_kind: "record", owner_id: "matrix.fixture", owner_field: "payload.summary_md",
        migration_status: "replaced",
      });
      expect(validateSemanticConversionCompletion(value, debt(), completed)
        .some((issue) => issue.message.includes("uncovered_replacement_span")), "uncovered replacement did not block complete");
      return pass();
    }
    case "completion_rejects_raw_subordinate_lifecycle":
    case "completion_rejects_inferred_transition":
    case "completion_rejects_pending_family_classification":
    case "completion_rejects_unrendered_field": {
      const category = categoryMap[id as keyof typeof categoryMap];
      expect(validateSemanticConversionCompletion(document(), debt([], [independent(category)]), completed)
        .some((issue) => issue.message.includes(category)), `${category} did not block complete`);
      return pass();
    }
    case "completion_allows_typed_stale_unknown_and_unmodelled": {
      const value = document();
      value.records.push(
        {
          id: "matrix.fixture-signal" as RoadmapId, title: "Signal", projection_group: "fixture" as never,
          render_authority: "semantic", projection_visibility: "semantic_only", source_replacements: [],
          payload: { kind: "signal", evaluation: "unknown" } as never,
        },
        {
          id: "matrix.fixture-evidence" as RoadmapId, title: "Evidence", projection_group: "fixture" as never,
          render_authority: "semantic", projection_visibility: "semantic_only", source_replacements: [],
          payload: { kind: "evidence", evidence_verdict: "unknown", freshness: "stale" } as never,
        },
        {
          id: "matrix.fixture-control" as RoadmapId, title: "Control", projection_group: "fixture" as never,
          render_authority: "semantic", projection_visibility: "semantic_only", source_replacements: [],
          payload: { kind: "control", control_state: "stale" } as never,
        },
      );
      const audit = semanticConversionCompletionAudit(
        value,
        debt([], [independent("unmodelled_coordinates")]),
        completed,
      );
      const progress = migrationProgressReport(
        value,
        debt([], [independent("unmodelled_coordinates")]),
        completed,
      );
      expect(audit.blockers.length === 0 && JSON.stringify(audit.join_blockers) === "[]" &&
        progress.typed_semantic_state.signals.unknown_record_ids.length === 1 &&
        progress.typed_semantic_state.evidence.unknown_record_ids.length === 1 &&
        progress.typed_semantic_state.evidence.stale_record_ids.length === 1 &&
        progress.typed_semantic_state.controls.stale_record_ids.length === 1,
      "typed stale/unknown/unmodelled state must not block complete");
      return pass();
    }
    case "completion_reports_unresolved_cross_roadmap_join": {
      const audit = semanticConversionCompletionAudit(document(), debt([], [independent("unresolved_references")]), completed);
      expect(audit.blockers.length === 0 && audit.join_blockers.length === 1,
        "unresolved reference must be deferred from lane completion and reported as a cross-roadmap join blocker");
      return pass();
    }
    case "completion_accepts_zero_debt":
      expect(validateSemanticConversionCompletion(document(), debt(), completed).length === 0,
        "zero-debt complete declaration must pass");
      return pass();
    case "completion_transition_forward_allowed":
      expect(validateSemanticConversionTransition(document("converting"), document("complete")).length === 0,
        "converting to complete must be allowed without capability");
      return pass();
    case "completion_transition_stable_allowed":
      expect(validateSemanticConversionTransition(document("complete"), document("complete")).length === 0,
        "stable complete must be allowed");
      return pass();
    case "completion_transition_reverse_rejected":
      expect(validateSemanticConversionTransition(document("complete"), document("converting")).length === 1,
        "complete to converting must be rejected");
      return pass();
    case "completion_transition_historical_omission_is_converting":
      expect(semanticConversionState(document("omitted")).declared === "omitted" &&
        semanticConversionState(document("omitted")).effective === "converting" &&
        validateSemanticConversionTransition(document("complete"), document("omitted")).length === 1,
      "historical omission must be effective converting and may not reverse complete");
      return pass();
    case "completion_declaration_v0_forbidden": {
      const v0 = { ...document(), document: { ...document().document, schema_version: 0, authority: "shadow" } } as unknown as RoadmapDocument;
      expect(validateSemanticConversionDeclaration(v0, false).length === 1, "v0 declaration must be forbidden");
      return pass();
    }
    case "completion_declaration_current_v1_required":
      for (const roadmap of ["matrix", "testing"] as const) {
        expect(validateSemanticConversionDeclaration(document("omitted", roadmap), false).length === 1,
          `${roadmap} current v1 omission must fail`);
      }
      return pass();
    case "completion_declaration_historical_v1_omission_allowed":
      for (const roadmap of ["matrix", "testing"] as const) {
        expect(validateSemanticConversionDeclaration(document("omitted", roadmap), true).length === 0,
          `${roadmap} historical v1 omission must decode as converting`);
      }
      return pass();
  }
  throw new Error(`unhandled semantic conversion case ${id}`);
}

const categoryMap = {
  completion_rejects_raw_subordinate_lifecycle: "raw_subordinate_lifecycles",
  completion_rejects_inferred_transition: "inferred_transitions",
  completion_rejects_pending_family_classification: "pending_family_classifications",
  completion_rejects_unrendered_field: "unrendered_fields",
} as const;

export const SEMANTIC_CONVERSION_SELFTEST_CASES: readonly SelfTestCase[] =
  REQUIRED_SEMANTIC_CONVERSION_SELFTEST_CASE_IDS.map((id) => Object.freeze({
    id,
    category: "debt" as const,
    run: () => run(id),
  }));
