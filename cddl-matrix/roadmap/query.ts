/**
 * The `--query` views: read-only projections of already-validated roadmaps into a stable value
 * shape, plus the canonical JSON/text rendering of that value.  Nothing here validates or writes;
 * `index.ts` owns the command shell that selects roadmaps and prints the result.
 */
import type {
  AsOfDate,
  EvidencePayload,
  NestedCadenceTransition,
  QueryView,
} from "./model/core.ts";
import type { FinalizedRoadmap } from "./pipeline.ts";
import { armOfPayload, armQueryEntries } from "./payload_descriptors.ts";
import { payloadRoadmapCitations } from "./indexes.ts";
import { codePointSort, sha256 } from "./kernel.ts";

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
  payload: EvidencePayload,
  asOf: AsOfDate | undefined,
): string {
  if (payload.freshness !== "as_of") return payload.freshness;
  if (payload.valid_through === undefined) return "unknown";
  if (asOf === undefined) return "unknown_no_as_of";
  return asOf > payload.valid_through ? "stale" : payload.freshness;
}

export interface IndexRow {
  readonly roadmap: string;
  readonly id: string;
  readonly arm: string;
  readonly state: string;
  readonly section: string;
  readonly cited: readonly string[];
  readonly title: string;
}

/** The one placeholder for an absent coordinate, so every index column is always occupied. */
const INDEX_NONE = "-";

/**
 * One row per record: the whole substrate in the form an agent can survey without ingesting the
 * prose. Every column is derived, never authored — `arm` is the payload's own kind, `state` is its
 * descriptor arm's discriminant chain (so a new arm shows up here with no edit to this file),
 * `section` is the section whose `entries` place it (`-` for a record that deliberately does not
 * render), and `cited` is the descriptor-derived list of roadmap records the payload names.
 */
function indexRows(prepared: readonly FinalizedRoadmap[]): readonly IndexRow[] {
  const rows: IndexRow[] = [];
  for (const item of prepared) {
    const sectionOf = new Map<string, string>();
    for (const section of item.document.sections) {
      for (const entry of section.entries) sectionOf.set(entry, section.section_id);
    }
    for (const record of item.document.records) {
      const arm = armOfPayload(record.payload);
      const state = arm.when
        .filter(([path]) => path !== "kind")
        .map(([path, values]) => `${path}=${values.join("|")}`)
        .join(",");
      const cited = payloadRoadmapCitations(record);
      rows.push({
        roadmap: item.document.document.roadmap,
        id: record.id,
        arm: record.payload.kind,
        state: state === "" ? INDEX_NONE : state,
        section: sectionOf.get(record.id) ?? INDEX_NONE,
        cited,
        title: record.title,
      });
    }
  }
  return rows.sort((left, right) =>
    codePointSort(left.roadmap, right.roadmap) || codePointSort(left.id, right.id));
}

/** One tab-separated line per row; the title is JSON-encoded so a row is always exactly one line. */
function indexTextLines(value: unknown): string {
  const rows = (value as { index?: readonly IndexRow[] }).index ?? [];
  return rows.map((row) => [
    row.roadmap,
    row.id,
    row.arm,
    row.state,
    row.section,
    row.cited.length === 0 ? INDEX_NONE : row.cited.join(","),
    JSON.stringify(row.title),
  ].join("\t")).join("\n");
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
    case "transitions": {
      // Standalone transition records are unrepresentable (Phase 4 fold): the qualitative nested
      // kinds ride their owners in the actionables/decisions/watches views, and this view keeps
      // exactly the TEMPORAL rows an as-of date can move — nested cadences (surfaced under their
      // owner's id) and as-of evidence freshness.
      const evaluateCadence = (cadence: NestedCadenceTransition): string => {
        if (cadence.due_on === undefined) return "unknown";
        if (asOf === undefined) return "unknown_no_as_of";
        return asOf >= cadence.due_on ? "due" : "not_due";
      };
      const rows = payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] => {
        const cadence = payload.kind === "work" && payload.work_state === "armed"
          ? payload.cadence
          : payload.kind === "matrix_policy" && payload.policy_kind === "maintenance_protocol"
          ? payload.cadence
          : undefined;
        if (cadence !== undefined) {
          return [{
            roadmap, id, transition_kind: "cadence", evaluation: evaluateCadence(cadence),
            owner_reference_id: cadence.owner_reference_id, event_source: cadence.event_source,
            period_or_event_md: cadence.period_or_event_md, checklist_md: cadence.checklist_md,
            missed_action_md: cadence.missed_action_md,
            last_completion_reference_id: cadence.last_completion_reference_id ?? null,
            due_on: cadence.due_on ?? null, authored_as_of: cadence.as_of ?? null,
          }];
        }
        if (payload.kind === "evidence" && payload.freshness === "as_of") {
          return [{
            roadmap, id,
            transition_kind: "evidence_freshness",
            evaluation: evaluateTemporalPayload(payload, asOf),
            evidence_kind: payload.evidence_kind,
            freshness: payload.freshness, reference_ids: payload.reference_ids,
            observed_at: payload.observed_at ?? null, valid_through: payload.valid_through ?? null,
            scope: payload.scope, claim_md: payload.claim_md,
            unprobed_remainder_md: payload.unprobed_remainder_md,
          }];
        }
        return [];
      });
      return { evaluation_as_of, transitions: groupQueryRows(rows, (row) => String(row.transition_kind)) };
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
            unblock_predicate: payload.unblock_predicate }
          : payload.work_state === "waiting_external" ? {
            external_owner_reference_id: payload.external_owner_reference_id,
            ...(payload.unblock_predicate === undefined ? {} : { unblock_predicate: payload.unblock_predicate }),
            ...(payload.retirement_predicate === undefined ? {} : { retirement_predicate: payload.retirement_predicate }) }
          : payload.work_state === "delegated" ? { return_condition_md: payload.return_condition_md }
          : payload.work_state === "armed" ? { control_ids: payload.control_ids,
            promotion_trigger: payload.promotion_trigger }
          : { uncertainty_md: payload.uncertainty_md }),
        }]);
      const costs = payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] =>
        payload.kind !== "testing_cost"
          ? []
          : [{ roadmap, id, ...Object.fromEntries(armQueryEntries(payload)) }]);
      const externalCloseouts = payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] =>
        payload.kind !== "matrix_external_closeout"
          ? []
          : [{ roadmap, id, ...Object.fromEntries(armQueryEntries(payload)) }]);
      const ready = rows.filter((row) => row.work_state === "ready");
      const armed = rows.filter((row) => row.work_state === "armed");
      const relations = prepared.flatMap((item) => item.document.relations);
      const blockedOrOwned = rows.filter((row) => ["blocked", "waiting_external", "delegated"].includes(String(row.work_state)))
        .map((row) => {
          const delegationTargets = row.work_state === "delegated"
            ? relations.filter((relation) => relation.kind === "delegates_to" && relation.source === row.id)
              .map((relation) => relation.target)
            : [];
          // The unblock predicate now lives nested on the owner (Packet 3A-2); the exact list is
          // its one nested entry rather than a citation join over standalone transition records.
          const nested = "unblock_predicate" in row
            ? row.unblock_predicate as { event_md: Uint8Array; check_procedure_md: Uint8Array; due_action_md: Uint8Array; owner_reference_id: string }
            : undefined;
          return { ...row,
            owner_group: row.work_state === "waiting_external"
              ? String(row.external_owner_reference_id)
              : row.work_state === "delegated" ? delegationTargets.join(",") : "blocked_internal",
            delegation_targets: delegationTargets,
            exact_unblock_predicates: nested === undefined ? [] : [{
              transition_kind: "unblock_predicate",
              event_md: nested.event_md, check_procedure_md: nested.check_procedure_md,
              due_action_md: nested.due_action_md, owner_reference_id: nested.owner_reference_id,
            }] };
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
          ...Object.fromEntries(armQueryEntries(payload, ["decision_state", "permanence"])) }]);
      return { evaluation_as_of, decisions: groupQueryRows(rows, (row) => String(row.decision_state)) };
    }
    case "watches":
      return { evaluation_as_of,
        live: payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] => {
          if (payload.kind === "testing_operational_watch" && payload.watch_state === "watching") return [{
            roadmap, id, payload_kind: payload.kind,
            ...Object.fromEntries(armQueryEntries(payload, ["watch_state"])),
          }];
          if (payload.kind === "testing_incident" && payload.incident_posture === "live") return [{
            roadmap, id, payload_kind: payload.kind,
            ...Object.fromEntries(armQueryEntries(payload, ["incident_posture"])),
          }];
          return [];
        }),
        attributed_history: payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] => {
          if (payload.kind === "testing_operational_watch" && payload.watch_state !== "watching") return [{
            roadmap, id, payload_kind: payload.kind, posture: payload.watch_state,
            ...Object.fromEntries(armQueryEntries(payload, ["watch_state"])),
          }];
          if (payload.kind === "testing_incident" && payload.incident_posture !== "live") return [{
            roadmap, id, payload_kind: payload.kind, posture: payload.incident_posture,
            ...Object.fromEntries(armQueryEntries(payload, ["incident_posture"])),
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
    case "index":
      return { evaluation_as_of, index: indexRows(prepared) };
  }
}

/**
 * The text rendering of a query value. Every view but `index` prints one line per top-level key;
 * `index` is line-oriented by construction — one record per line — so it renders its own rows.
 */
export function queryText(value: unknown, view: QueryView): Uint8Array {
  if (view === "index") {
    const lines = indexTextLines(stableJsonValue(value));
    return UTF8.encode(lines === "" ? "" : `${lines}\n`);
  }
  const stable = stableJsonValue(value) as Record<string, unknown>;
  const lines = Object.keys(stable).sort().map((key) => `${key}: ${JSON.stringify(stable[key])}`);
  return UTF8.encode(`${lines.join("\n")}\n`);
}
