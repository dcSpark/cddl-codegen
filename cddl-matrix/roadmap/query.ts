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
          families: item.document.records.flatMap((record) => {
            const payload = record.payload;
            if (payload.kind !== "family") return [];
            return [{
              id: record.id,
              denominator_maturity: payload.family_maturity,
              ...(payload.family_maturity === "closed_denominator"
                ? { legal_total: payload.cells.length }
                : { observed_lower_bound: payload.cells.length }),
            }];
          }),
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
          return [{ roadmap, id, transition_kind: payload.transition_kind,
            evaluation: evaluateTemporalPayload(payload, asOf),
            ...(payload.transition_kind === "promotion_trigger" || payload.transition_kind === "reopening_signal"
              ? { observer: payload.observer, dimension: payload.dimension,
                ...(payload.observable === undefined ? {} : { observable: payload.observable }),
                predicate: payload.predicate, action_on_fire_md: payload.action_on_fire_md }
              : payload.transition_kind === "unblock_predicate"
              ? { owner_reference_id: payload.owner_reference_id, event_md: payload.event_md,
                check_procedure_md: payload.check_procedure_md, due_action_md: payload.due_action_md }
              : payload.transition_kind === "watch_escalation"
              ? { failure_signature_md: payload.failure_signature_md,
                capture_procedure_md: payload.capture_procedure_md, response_md: payload.response_md,
                escalation_action_md: payload.escalation_action_md,
                retirement_semantics_md: payload.retirement_semantics_md }
              : payload.transition_kind === "retirement_predicate"
              ? { external_owner_reference_id: payload.external_owner_reference_id,
                external_predicate_md: payload.external_predicate_md,
                verification_md: payload.verification_md, due_action_md: payload.due_action_md }
              : { owner_reference_id: payload.owner_reference_id, event_source: payload.event_source,
                period_or_event_md: payload.period_or_event_md, checklist_md: payload.checklist_md,
                missed_action_md: payload.missed_action_md,
                last_completion_reference_id: payload.last_completion_reference_id ?? null,
                due_on: payload.due_on ?? null, authored_as_of: payload.as_of ?? null }) }];
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
          admission_basis_ids: payload.admission_ids ?? [], family_id: payload.family_id ?? null,
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
        payload.kind !== "testing_cost" ? [] : [{ roadmap, id, cost_posture: payload.cost_posture,
          unit: payload.unit, scope_md: payload.scope_md,
          ...(payload.cost_posture === "live_registry" ? { gate_reference_id: payload.gate_reference_id }
          : { value_min: payload.value_min, value_max: payload.value_max,
            observed_at: payload.observed_at, environment_md: payload.environment_md,
            evidence_ids: payload.evidence_ids }) }]);
      const externalCloseouts = payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] =>
        payload.kind !== "matrix_external_closeout" ? [] : [{ roadmap, id,
          closeout_state: payload.closeout_state, upstream_owner_reference_id: payload.upstream_owner_reference_id,
          current_upstream_state_md: payload.current_upstream_state_md,
          transition_ids: payload.transition_ids, verification_md: payload.verification_md,
          prune_reference_ids: payload.prune_reference_ids ?? [], actions: payload.actions,
          branches: payload.branches,
          ...(payload.closeout_state === "blocked" ? { blocker_md: payload.blocker_md } : {}) }]);
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
          ...(payload.decision_state === "pending" ? { question_md: payload.question_md }
          : payload.decision_state === "held" ? { rationale_md: payload.rationale_md }
          : { rationale_md: payload.rationale_md,
            authority_reference_id: payload.authority_reference_id }) }]);
      return { evaluation_as_of, decisions: groupQueryRows(rows, (row) => String(row.decision_state)) };
    }
    case "families":
      return { evaluation_as_of, families: payloadRows.flatMap(({ roadmap, id, payload }) => {
        if (payload.kind !== "family") return [];
        const dispositions = Object.fromEntries([
          "supported", "safely_refused", "deliberately_unsupported", "unknown",
        ].map((disposition) => [disposition,
          payload.cells.filter((cell) => cell.cell_disposition === disposition).length]));
        return [{ roadmap, id, denominator_maturity: payload.family_maturity,
          campaign_state: payload.campaign_state,
          ...(payload.family_maturity === "closed_denominator"
            ? { legal_total: payload.cells.length }
            : { observed_lower_bound: payload.cells.length }),
          exclusions: payload.exclusions, dispositions,
          explicit_unknown: payload.cells.filter((cell) => cell.cell_disposition === "unknown").length,
          unmodelled_population: payload.family_maturity === "closed_denominator" ? 0 : "unknown_open_denominator",
          closure_owner_reference_id: payload.completion_owner_reference_id,
          ...(payload.family_maturity === "observed_only"
            ? { denominator_authority: "observed_only",
              observation_reference_ids: payload.observation_reference_ids }
            : { denominator_authority: payload.authority_kind,
              authority_reference_id: payload.authority_reference_id,
              derivation_md: payload.derivation_md, legality_rule_md: payload.legality_rule_md,
              legality_owner_reference_id: payload.legality_owner_reference_id,
              denominator_unknowns_md: payload.family_maturity === "under_design"
                ? payload.denominator_unknowns_md ?? null : null,
              ...(payload.family_maturity === "closed_denominator"
                ? { drift_check_reference_id: payload.drift_check_reference_id,
                  mutation_test_reference_id: payload.mutation_test_reference_id }
                : {}) }) }];
      }) };
    case "watches":
      return { evaluation_as_of,
        live: payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] => {
          if (payload.kind === "testing_operational_watch" && payload.watch_state === "watching") return [{
            roadmap, id, payload_kind: payload.kind, signature_md: payload.signature_md, capture_steps: payload.capture_steps,
            response_md: payload.response_md, escalation_transition_id: payload.escalation_transition_id,
            retirement_semantics_md: payload.retirement_semantics_md,
          }];
          if (payload.kind === "testing_incident" && payload.incident_posture === "live") return [{
            roadmap, id, payload_kind: payload.kind, signature_md: payload.signature_md, evidence_ids: payload.evidence_ids,
          }];
          return [];
        }),
        attributed_history: payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] => {
          if (payload.kind === "testing_operational_watch" && payload.watch_state !== "watching") return [{
            roadmap, id, payload_kind: payload.kind, posture: payload.watch_state, signature_md: payload.signature_md,
            capture_steps: payload.capture_steps, response_md: payload.response_md,
            escalation_transition_id: payload.escalation_transition_id,
            retirement_semantics_md: payload.retirement_semantics_md,
            attribution_md: payload.attribution_md,
            operating_rule_reference_id: payload.operating_rule_reference_id,
            ...(payload.watch_state === "retire_pending"
              ? { retirement_reference_id: payload.retirement_reference_id } : {}),
          }];
          if (payload.kind === "testing_incident" && payload.incident_posture !== "live") return [{
            roadmap, id, payload_kind: payload.kind, posture: payload.incident_posture,
            signature_md: payload.signature_md, evidence_ids: payload.evidence_ids,
            attribution_md: payload.attribution_md,
            operating_rule_reference_id: payload.operating_rule_reference_id,
            ...(payload.incident_posture === "historical"
              ? { retirement_reference_id: payload.retirement_reference_id } : {}),
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
