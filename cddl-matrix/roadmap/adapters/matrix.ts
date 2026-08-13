import type { IssueCollector } from "../errors.ts";
import { deriveMatrixStatusFacts, renderMatrixStatusPayloads } from "../matrix_status_facts.ts";
import type { RepoPath, SlotId } from "../model/core.ts";
import type {
  Reference,
  RoadmapDocument,
  SemanticPayload,
  SemanticRecord,
} from "../model/documents.ts";
import type { SemanticPayloadProviderFact } from "../indexes.ts";
import type {
  GeneratedSlotResolver,
  Indexes,
  ReferenceProvider,
  RegistryView,
  RoadmapAdapter,
} from "./types.ts";
import {
  createRoadmapFloorValidator,
  payloadFactIssue,
  renderCanonicalSemanticRecord,
  requirePayloadKind,
  validateDecodedRoadmapDocument,
  type DecodedRoadmapValidationOptions,
  type DecodedRoadmapValidationResult,
} from "./engine.ts";
import { MATRIX_SLOT_FLOORS } from "./matrix_slot_floors.ts";

const issue = payloadFactIssue;

const MATRIX_SOURCE_PATH = "cddl-matrix/roadmap.toml" as RepoPath;
const MATRIX_PROJECTION_PATH = "cddl-matrix/ROADMAP.md" as RepoPath;
const SLOT_BINDINGS = Object.freeze([
  ["constraint" as SlotId, "status_header_markers:roadmap-constraint"],
  ["counts" as SlotId, "status_header_markers:roadmap-counts"],
  ["emission" as SlotId, "status_header_markers:roadmap-emission"],
  ["ops" as SlotId, "status_header_markers:roadmap-ops"],
] as const);

function usesLiveMatrixInlineSlots(doc: RoadmapDocument): boolean {
  if (
    doc.document.source_path !== MATRIX_SOURCE_PATH ||
    doc.document.projection_path !== MATRIX_PROJECTION_PATH ||
    doc.generated_slots.length !== MATRIX_SLOT_FLOORS.length
  ) return false;
  const slots = new Map(doc.generated_slots.map((slot) => [slot.slot_id, slot]));
  return slots.size === MATRIX_SLOT_FLOORS.length && MATRIX_SLOT_FLOORS.every(
    ([slotId, binding, spanId, start, end]) => {
      const slot = slots.get(slotId);
      const span = doc.spans.find((entry) => entry.id === spanId);
      return slot?.binding === binding && slot.span_ids.length === 1 &&
        slot.span_ids[0] === spanId && span?.source_kind === "generated_slot" &&
        span.owner_id === slotId && span.owner_field === "generated" &&
        span.migration_status === "generated" && span.start_byte === start && span.end_byte === end;
    },
  );
}

function exactRegistryProvider(
  kind: "matrix_feature" | "matrix_role" | "matrix_cell",
  members: (view: RegistryView) => readonly { id: string }[],
): ReferenceProvider {
  return Object.freeze({
    kind,
    resolve(reference: Reference, view: RegistryView) {
      if (reference.kind !== kind) {
        return { resolved: false as const, reason: `provider ${kind} received ${reference.kind}` };
      }
      const id = reference.kind === "matrix_feature"
        ? reference.feature_id
        : reference.kind === "matrix_role" ? reference.role_id : reference.cell_id;
      const matches = members(view).filter((member) => member.id === id);
      return matches.length === 1
        ? { resolved: true as const, provider: `${kind}:${id}` }
        : {
            resolved: false as const,
            reason: matches.length === 0
              ? `${kind} ${JSON.stringify(id)} is absent from the injected registry view`
              : `${kind} ${JSON.stringify(id)} has ${matches.length} injected registry claims`,
          };
    },
  });
}

function matrixProviders(): readonly ReferenceProvider[] {
  return Object.freeze([
    exactRegistryProvider("matrix_cell", (view) => view.matrix_cells),
    exactRegistryProvider("matrix_feature", (view) => view.matrix_features),
    exactRegistryProvider("matrix_role", (view) => view.matrix_roles),
  ]);
}

function matrixSlotResolvers(
  view: RegistryView,
  document: RoadmapDocument,
): ReadonlyMap<SlotId, GeneratedSlotResolver> {
  const facts = deriveMatrixStatusFacts(view.matrix_status_inputs);
  if (facts.validation_problems.length > 0) {
    throw new Error(`matrix status inputs fail anti-vacuity: ${facts.validation_problems.join("; ")}`);
  }
  const inlineProductionSlots = usesLiveMatrixInlineSlots(document);
  const payloads = new Map<string, Uint8Array>(
    renderMatrixStatusPayloads(facts)
      .filter((payload) => payload.path === MATRIX_PROJECTION_PATH)
      .map((payload) => {
        if (inlineProductionSlots) {
          // The live roadmap slots own only inline marker interiors while the document retains the
          // frozen production slot contract. Owner authority and joins do not change the
          // surrounding layout owned by other nodes.
          return [`status_header_markers:${payload.slot_id}`, payload.bytes] as const;
        }
        // Compatibility fixtures model a slot as a complete standalone projection line.
        const line = new Uint8Array(payload.bytes.byteLength + 1);
        line.set(payload.bytes);
        line[payload.bytes.byteLength] = 0x0a;
        return [`status_header_markers:${payload.slot_id}`, line] as const;
      }),
  );
  return new Map(SLOT_BINDINGS.map(([slotId, expectedBinding]) => [
    slotId,
    Object.freeze({
      resolve(slot) {
        if (slot.slot_id !== slotId) {
          throw new Error(`resolver ${slotId} cannot resolve slot ${slot.slot_id}`);
        }
        if (slot.binding !== expectedBinding) {
          throw new Error(`slot ${slotId} binding must be ${expectedBinding}`);
        }
        const bytes = payloads.get(expectedBinding);
        if (bytes === undefined || bytes.byteLength === 0) {
          throw new Error(`slot ${slotId} produced no matrix roadmap status bytes`);
        }
        return { binding: expectedBinding, bytes: new Uint8Array(bytes) };
      },
    } satisfies GeneratedSlotResolver),
  ]));
}

export function validateMatrixPayloadFact(
  provider: SemanticPayloadProviderFact,
  indexes: Indexes,
  out: IssueCollector,
  source: string = MATRIX_SOURCE_PATH,
): void {
  const payload = provider.payload;
  if (payload.kind !== "matrix_external_closeout" && payload.kind !== "matrix_policy") return;
  if (payload.kind === "matrix_external_closeout") {
    if (payload.transition_ids.length !== 1) {
      out.add(issue(provider, source, "E-SCHEMA-STATE", "transition_ids", "matrix closeout requires exactly one retirement predicate"));
    }
    for (const id of payload.transition_ids) {
      requirePayloadKind(
        provider,
        source,
        indexes,
        id,
        "transition_ids",
        (target) => target.kind === "signal" && target.transition_kind === "retirement_predicate",
        "one retirement-predicate signal",
        out,
      );
    }
    const actionIds = new Set<string>();
    for (const action of payload.actions) {
      if (actionIds.has(action.action_id)) {
        out.add(issue(provider, source, "E-SCHEMA-STATE", `actions[${JSON.stringify(action.action_id)}]`, "closeout action ID is duplicated"));
      }
      actionIds.add(action.action_id);
    }
    const branchIds = new Set<string>();
    for (const branch of payload.branches) {
      if (branchIds.has(branch.branch_id)) {
        out.add(issue(provider, source, "E-SCHEMA-STATE", `branches[${JSON.stringify(branch.branch_id)}]`, "closeout branch ID is duplicated"));
      }
      branchIds.add(branch.branch_id);
      const branchActionIds = new Set<string>();
      for (const actionId of branch.action_ids) {
        if (branchActionIds.has(actionId)) {
          out.add(issue(
            provider,
            source,
            "E-SCHEMA-STATE",
            `branches[${JSON.stringify(branch.branch_id)}].action_ids`,
            `${actionId} is duplicated within the branch action sequence`,
          ));
        }
        branchActionIds.add(actionId);
        if (!actionIds.has(actionId)) {
          out.add(issue(
            provider,
            source,
            "E-SCHEMA-STATE",
            `branches[${JSON.stringify(branch.branch_id)}].action_ids`,
            `${actionId} does not resolve to a local closeout action`,
          ));
        }
      }
    }
    return;
  }
  const targetId = payload.policy_kind === "maintenance_protocol"
    ? payload.cadence_transition_id
    : payload.reopening_transition_id;
  if (targetId === undefined) {
    if (payload.policy_kind === "boundary" && payload.permanence === "reopenable") {
      out.add(issue(provider, source, "E-SCHEMA-STATE", "reopening_transition_id", "reopenable matrix boundary requires a reopening signal"));
    }
    return;
  }
  const expected = payload.policy_kind === "maintenance_protocol" ? "cadence" : "reopening_signal";
  requirePayloadKind(
    provider,
    source,
    indexes,
    targetId,
    payload.policy_kind === "maintenance_protocol" ? "cadence_transition_id" : "reopening_transition_id",
    (target) => target.kind === "signal" && target.transition_kind === expected,
    `${expected} signal`,
    out,
  );
}

const MATRIX_FLOORS = createRoadmapFloorValidator({
  roadmap: "matrix",
  source_path: MATRIX_SOURCE_PATH,
  projection_path: MATRIX_PROJECTION_PATH,
  slot_bindings: SLOT_BINDINGS,
  slot_inventory_message: "matrix roadmap requires exactly four distinct generated status slots",
});

export const MATRIX_ADAPTER: RoadmapAdapter<SemanticPayload> = Object.freeze({
  roadmap: "matrix",
  namespace: "matrix",
  source_path: MATRIX_SOURCE_PATH,
  projection_path: MATRIX_PROJECTION_PATH,
  validateExtension(record: SemanticRecord<SemanticPayload>, indexes: Indexes, out: IssueCollector) {
    validateMatrixPayloadFact({
      record,
      payload: record.payload,
      authority: "semantic",
      logical_path: `record[${JSON.stringify(record.id)}].payload`,
    }, indexes, out);
  },
  renderSemantic: renderCanonicalSemanticRecord,
  referenceProviders(_view: RegistryView) {
    return [...matrixProviders()];
  },
  slotResolvers(view: RegistryView, document: RoadmapDocument) {
    return matrixSlotResolvers(view, document);
  },
  validateFloors: MATRIX_FLOORS,
});

export const MATRIX_GENERATED_SLOT_BINDINGS = SLOT_BINDINGS;
