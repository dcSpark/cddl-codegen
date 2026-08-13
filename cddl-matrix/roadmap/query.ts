/**
 * The `--query` views: read-only projections of already-validated roadmaps into a stable value
 * shape, plus the canonical JSON/text rendering of that value.  Nothing here validates or writes;
 * `index.ts` owns the command shell that selects roadmaps and prints the result.
 */
import type {
  AsOfDate,
  EvidencePayload,
  QueryView,
  SignalPayload,
} from "./model/core.ts";
import type { FinalizedRoadmap } from "./pipeline.ts";
import { armQueryEntries } from "./payload_descriptors.ts";
import { sha256 } from "./kernel.ts";

const UTF8 = new TextEncoder();

export function stableJsonValue(value: unknown): unknown {
  if (value instanceof Uint8Array) return new TextDecoder().decode(value);
  if (Array.isArray(value)) return value.map(stableJsonValue);
  if (value instanceof Map) {
    return Object.fromEntries([...value.entries()].sort(([a], [b]) => String(a) < String(b) ? -1 : 1)
      .map(([key, entry]) => [String(key), stableJsonValue(entry)]));
  }
  if (value !== null && typeof value === "object") {
    return Object.fromEntries(Object.keys(value as object).sort().map((key) => [
      key,
      stableJsonValue((value as Record<string, unknown>)[key]),
    ]));
  }
  return value;
}

/** Derive only date-bearing query labels; authored qualitative/manual states stay authored. */
function evaluateTemporalPayload(
  payload: SignalPayload | EvidencePayload,
  asOf: AsOfDate | undefined,
): string {
  if (payload.kind === "signal") {
    if (payload.transition_kind !== "cadence") return payload.evaluation;
    if (payload.due_on === undefined) return "unknown";
    if (asOf === undefined) return "unknown_no_as_of";
    return asOf >= payload.due_on ? "due" : "not_due";
  }
  if (payload.freshness !== "as_of") return payload.freshness;
  if (payload.valid_through === undefined) return "unknown";
  if (asOf === undefined) return "unknown_no_as_of";
  return asOf > payload.valid_through ? "stale" : payload.freshness;
}

function groupQueryRows<T extends Record<string, unknown>>(
  rows: readonly T[],
  group: (row: T) => string,
): Readonly<Record<string, readonly T[]>> {
  const result: Record<string, T[]> = {};
  for (const row of rows) (result[group(row)] ??= []).push(row);
  return Object.fromEntries(Object.entries(result).sort(([left], [right]) => left < right ? -1 : left > right ? 1 : 0));
}

export function queryValue(prepared: readonly FinalizedRoadmap[], view: QueryView, asOf: AsOfDate | undefined): unknown {
  const evaluation_as_of = asOf ?? null;
  const payloadRows = prepared.flatMap((item) => item.document.records.flatMap((record) => {
    return [{ roadmap: item.document.document.roadmap, id: record.id, payload: record.payload }];
  })).sort((left, right) => left.roadmap < right.roadmap ? -1 : left.roadmap > right.roadmap ? 1 :
    left.id < right.id ? -1 : left.id > right.id ? 1 : 0);
  switch (view) {
    case "summary":
      return {
        evaluation_as_of,
        roadmaps: prepared.map((item) => ({
          roadmap: item.document.document.roadmap,
          schema_version: item.document.document.schema_version,
          record_count: item.document.records.length,
          projection_byte_length: item.projection.byteLength,
          projection_sha256: sha256(item.projection),
        })),
      };
    case "references":
      return { evaluation_as_of, roadmaps: prepared.map((item) => ({
        roadmap: item.document.document.roadmap,
        references: item.document.references,
      })) };
    case "signals": {
      const rows = payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] => {
        if (payload.kind === "signal") {
          // The row's shape is the arm's own field list (descriptor-derived); evaluation is the
          // one computed value, so it and the discriminant stay explicit here.
          return [{ roadmap, id, transition_kind: payload.transition_kind,
            evaluation: evaluateTemporalPayload(payload, asOf),
            ...Object.fromEntries(armQueryEntries(payload, ["detail_md", "transition_kind", "evaluation"])) }];
        }
        if (payload.kind === "evidence" && payload.freshness === "as_of") {
          return [{
            roadmap, id,
            transition_kind: "evidence_freshness",
            evaluation: evaluateTemporalPayload(payload, asOf),
            evidence_kind: payload.evidence_kind, evidence_verdict: payload.evidence_verdict,
            freshness: payload.freshness, reference_ids: payload.reference_ids,
            observed_at: payload.observed_at ?? null, valid_through: payload.valid_through ?? null,
            scope: payload.scope, claim_md: payload.claim_md,
            unprobed_remainder_md: payload.unprobed_remainder_md,
          }];
        }
        return [];
      });
      return { evaluation_as_of, signals: groupQueryRows(rows, (row) => String(row.transition_kind)) };
    }
    case "actionables": {
      const rows = payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] =>
        payload.kind !== "work" || payload.work_state === "deferred" ? [] : [{
          roadmap, id, work_state: payload.work_state, work_kind: payload.work_kind,
          work_intent: payload.work_intent, consequence: payload.risk,
          admission_basis: payload.work_kind !== "missing_system"
            ? "not_applicable"
            : (payload.admission_ids?.length ?? 0) > 0 ? "admitted" : "missing",
          admission_basis_ids: payload.admission_ids ?? [],
          ...(payload.work_state === "ready" ? { priority_band: payload.priority_band ?? "unbanded",
            priority_rationale_md: payload.priority_rationale_md }
          : payload.work_state === "blocked" ? { blocker_md: payload.blocker_md,
            unblock_transition_ids: payload.transition_ids }
          : payload.work_state === "waiting_external" ? {
            external_owner_reference_id: payload.external_owner_reference_id,
            unblock_transition_ids: payload.transition_ids }
          : payload.work_state === "delegated" ? { return_condition_md: payload.return_condition_md }
          : payload.work_state === "armed" ? { control_ids: payload.control_ids,
            transition_ids: payload.transition_ids }
          : { uncertainty_md: payload.uncertainty_md }),
        }]);
      const costs = payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] =>
        payload.kind !== "testing_cost"
          ? []
          : [{ roadmap, id, ...Object.fromEntries(armQueryEntries(payload, ["detail_md"])) }]);
      const externalCloseouts = payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] =>
        payload.kind !== "matrix_external_closeout"
          ? []
          : [{ roadmap, id, ...Object.fromEntries(armQueryEntries(payload, ["detail_md"])) }]);
      const ready = rows.filter((row) => row.work_state === "ready");
      const armed = rows.filter((row) => row.work_state === "armed");
      const signalsById = new Map(payloadRows.flatMap(({ id, payload }) => payload.kind === "signal"
        ? [[String(id), payload] as const] : []));
      const relations = prepared.flatMap((item) => item.document.relations);
      const blockedOrOwned = rows.filter((row) => ["blocked", "waiting_external", "delegated"].includes(String(row.work_state)))
        .map((row) => {
          const transitionIds = "unblock_transition_ids" in row
            ? row.unblock_transition_ids as readonly string[]
            : [];
          const delegationTargets = row.work_state === "delegated"
            ? relations.filter((relation) => relation.kind === "delegates_to" && relation.source === row.id)
              .map((relation) => relation.target)
            : [];
          return { ...row,
            owner_group: row.work_state === "waiting_external"
              ? String(row.external_owner_reference_id)
              : row.work_state === "delegated" ? delegationTargets.join(",") : "blocked_internal",
            delegation_targets: delegationTargets,
            exact_unblock_predicates: transitionIds.flatMap((id) => {
            const signal = signalsById.get(id);
            return signal === undefined ? [] : [{ id, transition_kind: signal.transition_kind,
              evaluation: signal.evaluation,
              ...(signal.transition_kind === "unblock_predicate"
                ? { event_md: signal.event_md, check_procedure_md: signal.check_procedure_md,
                  due_action_md: signal.due_action_md, owner_reference_id: signal.owner_reference_id }
                : {}) }];
          }) };
        });
      const pendingReview = rows.filter((row) => row.work_state === "pending_review");
      return { evaluation_as_of,
        ready_by_consequence: groupQueryRows(ready, (row) => String(row.consequence)),
        ready_by_admission_basis: groupQueryRows(ready, (row) => String(row.admission_basis)),
        armed_recur_first: armed,
        blocked_external_delegated: groupQueryRows(blockedOrOwned, (row) => String(row.owner_group)),
        pending_review: pendingReview,
        costs, external_closeouts: groupQueryRows(externalCloseouts, (row) => String(row.closeout_state)) };
    }
    case "decisions": {
      const rows = payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] =>
        payload.kind !== "decision" ? [] : [{ roadmap, id, decision_state: payload.decision_state,
          permanence: payload.decision_state === "pending" ? "pending" : payload.permanence,
          transition_ids: "transition_ids" in payload ? payload.transition_ids ?? [] : [],
          ...Object.fromEntries(armQueryEntries(payload, ["detail_md", "decision_state", "permanence", "transition_ids"])) }]);
      return { evaluation_as_of, decisions: groupQueryRows(rows, (row) => String(row.decision_state)) };
    }
    case "watches":
      return { evaluation_as_of,
        live: payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] => {
          if (payload.kind === "testing_operational_watch" && payload.watch_state === "watching") return [{
            roadmap, id, payload_kind: payload.kind,
            ...Object.fromEntries(armQueryEntries(payload, ["detail_md", "watch_state"])),
          }];
          if (payload.kind === "testing_incident" && payload.incident_posture === "live") return [{
            roadmap, id, payload_kind: payload.kind,
            ...Object.fromEntries(armQueryEntries(payload, ["detail_md", "incident_posture"])),
          }];
          return [];
        }),
        attributed_history: payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] => {
          if (payload.kind === "testing_operational_watch" && payload.watch_state !== "watching") return [{
            roadmap, id, payload_kind: payload.kind, posture: payload.watch_state,
            ...Object.fromEntries(armQueryEntries(payload, ["detail_md", "watch_state"])),
          }];
          if (payload.kind === "testing_incident" && payload.incident_posture !== "live") return [{
            roadmap, id, payload_kind: payload.kind, posture: payload.incident_posture,
            ...Object.fromEntries(armQueryEntries(payload, ["detail_md", "incident_posture"])),
          }];
          return [];
        }),
      };
    case "content":
      return { evaluation_as_of, roadmaps: prepared.map((item) => ({
        roadmap: item.document.document.roadmap,
        full_projection_byte_length: item.projection.byteLength,
        audit_markdown: item.projection_views.audit,
        authored_content: item.projection_views.content_reachability,
      })) };
    case "output-owners":
      return { evaluation_as_of, claims: prepared.flatMap((item) => item.registry.output_claims) };
  }
}

export function queryText(value: unknown): Uint8Array {
  const stable = stableJsonValue(value) as Record<string, unknown>;
  const lines = Object.keys(stable).sort().map((key) => `${key}: ${JSON.stringify(stable[key])}`);
  return UTF8.encode(`${lines.join("\n")}\n`);
}
