import type { IssueCollector, RoadmapIssue } from "../errors.ts";
import { namespaceOf, validateRoadmapId } from "../ids.ts";
import type { RoadmapIndexes, SemanticPayloadProviderFact } from "../indexes.ts";
import type { RepoPath, RoadmapId, RoadmapName, SlotId } from "../model/core.ts";
import type { RoadmapDocument, SemanticPayload, SemanticRecord } from "../model/documents.ts";
import type { GeneratedSlotResolver, Indexes, RegistryView, RoadmapAdapter } from "./types.ts";
import {
  MATRIX_ADAPTER,
  renderCanonicalSemanticRecord,
  validateDecodedRoadmapDocument,
  type DecodedRoadmapValidationOptions,
  type DecodedRoadmapValidationResult,
} from "./matrix.ts";

const TESTING_SOURCE_PATH = "tests/testing-roadmap.toml" as RepoPath;
const TESTING_PROJECTION_PATH = "tests/TESTING_ROADMAP.md" as RepoPath;
function issue(
  provider: SemanticPayloadProviderFact,
  source: string,
  logicalPath: string,
  message: string,
): RoadmapIssue {
  return {
    code: "E-SCHEMA-STATE",
    source,
    logical_path: `${provider.logical_path}.${logicalPath}`,
    message,
    exit: 1,
  };
}

function payloadAt(indexes: Indexes, id: RoadmapId): SemanticPayload | undefined {
  if ("payload_records" in indexes) {
    return (indexes as Indexes & Pick<RoadmapIndexes, "payload_records">).payload_records.get(id)?.payload;
  }
  return indexes.records.get(id)?.payload;
}

function requirePayloadKind(
  provider: SemanticPayloadProviderFact,
  source: string,
  indexes: Indexes,
  id: RoadmapId,
  field: string,
  predicate: (payload: SemanticPayload) => boolean,
  expected: string,
  out: IssueCollector,
): void {
  const target = payloadAt(indexes, id);
  const deferred = (indexes as Indexes & { readonly deferred_foreign_roadmap_joins?: RoadmapName })
    .deferred_foreign_roadmap_joins;
  const targetNamespace = namespaceOf(id);
  if (target === undefined && deferred !== undefined && targetNamespace !== undefined &&
    targetNamespace !== deferred) return;
  if (target === undefined || !predicate(target)) {
    out.add(issue(provider, source, field, `${id} must resolve to ${expected}`));
  }
}

export function validateTestingPayloadFact(
  provider: SemanticPayloadProviderFact,
  indexes: Indexes,
  out: IssueCollector,
  source: string = TESTING_SOURCE_PATH,
): void {
  const payload = provider.payload;
  if (
    payload.kind !== "testing_operational_watch" && payload.kind !== "testing_incident" &&
    payload.kind !== "testing_cost" && payload.kind !== "testing_system_admission"
  ) return;
  if (payload.kind === "testing_operational_watch") {
    requirePayloadKind(
      provider,
      source,
      indexes,
      payload.escalation_transition_id,
      "escalation_transition_id",
      (target) => target.kind === "signal" && target.transition_kind === "watch_escalation",
      "a watch-escalation signal",
      out,
    );
    return;
  }
  if (payload.kind === "testing_incident") {
    for (const id of payload.evidence_ids) {
      requirePayloadKind(provider, source, indexes, id, "evidence_ids", (target) => target.kind === "evidence", "an evidence record", out);
    }
    return;
  }
  if (payload.kind === "testing_cost") {
    if (payload.cost_posture === "historical_observation") {
      for (const id of payload.evidence_ids) {
        requirePayloadKind(provider, source, indexes, id, "evidence_ids", (target) => target.kind === "evidence", "an evidence record", out);
      }
    }
    return;
  }
  for (const id of payload.evidence_ids) {
    requirePayloadKind(provider, source, indexes, id, "evidence_ids", (target) => target.kind === "evidence", "an evidence record", out);
  }
  if (payload.admission_kind === "independent_recurrence") {
    for (const id of payload.incident_ids) {
      requirePayloadKind(
        provider,
        source,
        indexes,
        id,
        "incident_ids",
        (target) => target.kind === "testing_incident",
        "a testing incident record",
        out,
      );
    }
  } else if (payload.admission_kind === "bounded_denominator") {
    requirePayloadKind(provider, source, indexes, payload.family_id, "family_id", (target) => target.kind === "family", "a systematic family", out);
    requirePayloadKind(
      provider,
      source,
      indexes,
      payload.cost_record_id,
      "cost_record_id",
      (target) => target.kind === "testing_cost",
      "a testing cost record",
      out,
    );
  }
}

export const TESTING_ADAPTER: RoadmapAdapter<SemanticPayload> = Object.freeze({
  roadmap: "testing",
  namespace: "testing",
  source_path: TESTING_SOURCE_PATH,
  projection_path: TESTING_PROJECTION_PATH,
  validateExtension(record: SemanticRecord<SemanticPayload>, indexes: Indexes, out: IssueCollector) {
    validateTestingPayloadFact({
      record,
      payload: record.payload,
      authority: "semantic",
      logical_path: `record[${JSON.stringify(record.id)}].payload`,
    }, indexes, out);
  },
  renderSemantic: renderCanonicalSemanticRecord,
  // Testing has no domain-specific reference universe. Gate/test/file providers are shared
  // joins, and an empty adapter member list is an explicit contract rather than a missing registry.
  referenceProviders(_view: RegistryView) {
    return [];
  },
  // Testing has no generated roadmap slots. Synthetic transport fixtures inject their own
  // resolver at the render-service seam and do not expand production adapter authority.
  slotResolvers(_view: RegistryView, _document: RoadmapDocument): ReadonlyMap<SlotId, GeneratedSlotResolver> {
    return new Map<SlotId, GeneratedSlotResolver>();
  },
  validateFloors(doc: RoadmapDocument, out: IssueCollector) {
    if (doc.document.roadmap !== "testing") {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "document.roadmap",
        message: "testing adapter requires a testing roadmap document",
        exit: 1,
      });
    }
    if (doc.document.source_path !== TESTING_SOURCE_PATH) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "document.source_path",
        message: `testing source path must be ${TESTING_SOURCE_PATH}`,
        exit: 1,
      });
    }
    if (doc.document.projection_path !== TESTING_PROJECTION_PATH) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "document.projection_path",
        message: `testing projection path must be ${TESTING_PROJECTION_PATH}`,
        exit: 1,
      });
    }
    if (doc.records.length === 0 || doc.manifest.length === 0 || doc.spans.length === 0) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "$",
        message: "testing roadmap requires records, manifest placements, and source spans",
        exit: 1,
      });
    }
    if (doc.generated_slots.length !== 0) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "generated_slot",
        message: "testing roadmap declares exactly zero generated slots",
        exit: 1,
      });
    }
  },
});

export const TESTING_GENERATED_SLOT_BINDINGS: readonly never[] = Object.freeze([]);

export function validateTestingRoadmapDocument(
  document: RoadmapDocument,
  view: RegistryView,
  options: DecodedRoadmapValidationOptions = {},
): DecodedRoadmapValidationResult {
  return validateDecodedRoadmapDocument(
    document,
    view,
    TESTING_ADAPTER,
    [
      ...MATRIX_ADAPTER.referenceProviders(view),
      ...TESTING_ADAPTER.referenceProviders(view),
    ],
    validateTestingPayloadFact,
    options,
  );
}
