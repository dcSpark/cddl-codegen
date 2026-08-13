import type { IssueCollector } from "../errors.ts";
import type { SemanticPayloadProviderFact } from "../indexes.ts";
import type { RepoPath, SlotId } from "../model/core.ts";
import type { RoadmapDocument, SemanticPayload, SemanticRecord } from "../model/documents.ts";
import type { GeneratedSlotResolver, Indexes, RegistryView, RoadmapAdapter } from "./types.ts";
import {
  createRoadmapFloorValidator,
  renderCanonicalSemanticRecord,
  requirePayloadKind,
  validateDecodedRoadmapDocument,
  type DecodedRoadmapValidationOptions,
  type DecodedRoadmapValidationResult,
} from "./engine.ts";

const TESTING_SOURCE_PATH = "tests/testing-roadmap.toml" as RepoPath;
const TESTING_PROJECTION_PATH = "tests/TESTING_ROADMAP.md" as RepoPath;
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
    // The nested watch_escalation form is structural; only the standalone-transition citation form
    // still needs its target subtype validated.
    if (payload.escalation_transition_id !== undefined) {
      requirePayloadKind(
        provider,
        source,
        indexes,
        payload.escalation_transition_id,
        "escalation_transition_id",
        (target) => target.kind === "transition" && target.transition_kind === "watch_escalation",
        "a watch-escalation transition",
        out,
      );
    }
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
  }
}

const TESTING_FLOORS = createRoadmapFloorValidator({
  roadmap: "testing",
  source_path: TESTING_SOURCE_PATH,
  projection_path: TESTING_PROJECTION_PATH,
  // The testing roadmap declares no generated slots at all; the empty binding list is what makes
  // that an explicit contract rather than an omission.
  slot_bindings: [],
  slot_inventory_message: "testing roadmap declares exactly zero generated slots",
});

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
  validateFloors: TESTING_FLOORS,
});

export const TESTING_GENERATED_SLOT_BINDINGS: readonly never[] = Object.freeze([]);
