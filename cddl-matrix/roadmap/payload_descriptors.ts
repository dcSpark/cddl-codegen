/**
 * The one per-kind payload field-descriptor table.  Every per-payload-kind field restatement in
 * the tree derives from here: the decode exact-schema rows and discriminators (`decode/semantic.ts`,
 * `decode/matrix.ts`, `decode/testing.ts`), the canonical TOML compose order (`compose.ts`), the
 * canonical Markdown field order (`adapters/engine.ts`), the payload ID-use extraction
 * (`indexes.ts`), the reference-kind and roadmap-join expectations per citing field
 * (`references.ts`), and the per-kind query shapes (`query.ts`).  Adding a payload field is an
 * edit to THIS file: its arm entry supplies acceptance, emission, ordering, join policy, and query
 * membership at once.
 *
 * Shape rules:
 *  - An ARM is one (payload kind, state) row of the closed schema.  Its `fields` list is the
 *    canonical TOML order; the exact-schema row derives from it (required/optional filters keep
 *    field order), so the two can never disagree.
 *  - `forbidden` is authored per arm (the state machine's explicit rejections).
 *  - Discriminator pre-tables derive as: required = the discriminant chain; optional = the union
 *    of every arm's fields AND forbidden keys minus required (so a forbidden key still reaches the
 *    arm row, preserving E-SCHEMA-FORBIDDEN-KEY over E-SCHEMA-UNKNOWN-KEY).
 *  - Nested tables (signal predicates, evidence scope, closeout actions/branches, watch capture
 *    steps) are GROUPS with their own arms; a field of kind `table`/`array_table` points at one.
 */
import type { ExactSchemaRow } from "./decode/primitives.ts";
import type { Reference, SemanticPayload } from "./model/documents.ts";

export type ReferenceKindName = Reference["kind"];

/** Join expectation for a roadmap-ID field (consumed by references.ts). */
export interface RoadmapTargetExpectation {
  readonly payload_kind: SemanticPayload["kind"];
  readonly work_kind?: "regression_gap";
  readonly transition_kinds?: readonly (
    | "promotion_trigger"
    | "reopening_signal"
    | "unblock_predicate"
    | "watch_escalation"
    | "retirement_predicate"
    | "cadence"
  )[];
  readonly control_state?: "live";
}

/** Allowed reference kinds for a reference-ID field: a closed list, or a map keyed by a sibling enum field. */
export type AllowedReferenceKinds =
  | readonly ReferenceKindName[]
  | { readonly by: string; readonly map: Readonly<Record<string, readonly ReferenceKindName[]>> };

export interface NestedGroup {
  readonly arms: readonly PayloadArm[];
}

export type PayloadFieldValue =
  | { readonly t: "kind" }
  | { readonly t: "enum"; readonly values: readonly string[] }
  | { readonly t: "string" }
  | { readonly t: "slug" }
  | { readonly t: "markdown"; readonly nonempty?: true }
  | { readonly t: "number" }
  | { readonly t: "civil_date" }
  | { readonly t: "commit" }
  | { readonly t: "string_set"; readonly nonempty?: true }
  | { readonly t: "roadmap_id"; readonly target: RoadmapTargetExpectation }
  | {
      readonly t: "roadmap_id_set";
      readonly nonempty?: true;
      readonly target: RoadmapTargetExpectation;
    }
  | { readonly t: "reference_id"; readonly allowed: AllowedReferenceKinds }
  | {
      readonly t: "reference_id_set";
      readonly nonempty?: true;
      readonly allowed: AllowedReferenceKinds;
    }
  | { readonly t: "table"; readonly group: NestedGroup }
  | {
      readonly t: "array_table";
      readonly group: NestedGroup;
      /** Decoded payload property name when it differs from the wire key (action -> actions). */
      readonly prop: string;
      /** Element field whose value labels ID-use logical paths (branch["<id>"].…). */
      readonly id_field: string;
      readonly nonempty?: true;
      /** Absent wire key decodes to [] (property always present). */
      readonly default_empty?: true;
      /** Collapse each element table to this single field's value (branch action_id lists). */
      readonly flatten?: string;
    };

export interface PayloadFieldQueryHints {
  readonly rename?: string;
  readonly absent?: "null" | "empty_array";
}

export interface PayloadField {
  readonly name: string;
  readonly presence: "required" | "optional";
  readonly value: PayloadFieldValue;
  readonly query?: PayloadFieldQueryHints;
}

export interface PayloadArm {
  /** The exact-schema row (single frozen instance; decode passes exactly this object). */
  readonly row: ExactSchemaRow;
  readonly fields: readonly PayloadField[];
  /** Discriminant match clauses against a decoded payload (dotted paths supported). */
  readonly when: readonly (readonly [string, readonly string[]])[];
}

// ---------------------------------------------------------------------------------------------
// Closed enum value sets (single source; the decode enum registries reference these).

export const WORK_STATES = ["ready", "blocked", "armed", "deferred", "waiting_external", "delegated", "pending_review"] as const;
export const WORK_INTENTS = ["repair", "add_regression", "build_capability", "build_system", "establish_honest_refusal", "optimize", "change_documentation"] as const;
export const WORK_KINDS = ["defect", "regression_gap", "coverage_cell", "missing_system", "feature", "optimization", "documentation_integrity", "infrastructure"] as const;
export const RISKS = ["silent_wrong_bytes", "invalid_acceptance", "valid_rejection", "wrong_public_api", "compile_failure", "abort_or_panic", "false_pass_or_red", "misleading_docs", "resource_exhaustion", "cosmetic"] as const;
export const PRIORITIES = ["critical", "high", "normal", "low"] as const;
export const EVALUATIONS = ["met", "unmet", "unknown", "stale"] as const;
export const COMPARATORS = ["lt", "le", "eq", "ge", "gt"] as const;
export const EVIDENCE_KINDS = ["regression_pin", "gate", "harness_free_repro", "committed_vector", "execution_probe", "registry_enumeration", "source_read", "spec_read", "consumer_report", "incident", "external_issue", "external_commit", "decision"] as const;
export const EVIDENCE_VERDICTS = ["proposed", "confirmed", "falsified", "unknown", "inapplicable"] as const;
export const FRESHNESS = ["live", "as_of", "historical", "stale"] as const;
export const CONTROL_KINDS = ["gate", "test", "fixture", "review_rule", "consumer_ci", "upstream_issue", "operator_procedure"] as const;
export const CONTROL_STATES = ["live", "proposed", "stale"] as const;
export const DECISION_STATES = ["pending", "held", "decided"] as const;
export const PERMANENCE = ["permanent", "reopenable"] as const;
export const TRANSITION_KINDS = ["promotion_trigger", "reopening_signal", "unblock_predicate", "watch_escalation", "retirement_predicate", "cadence"] as const;
export const PREDICATE_KINDS = ["quantitative", "event", "manual"] as const;
export const SHARED_SEMANTIC_KINDS = ["work", "decision", "signal", "evidence", "control"] as const;
export const CLOSEOUT_STATES = ["waiting", "due", "blocked"] as const;
export const POLICY_KINDS = ["maintenance_protocol", "boundary"] as const;
export const MATRIX_SEMANTIC_KINDS = ["matrix_external_closeout", "matrix_policy"] as const;
export const WATCH_STATES = ["watching", "attributed", "retire_pending"] as const;
export const INCIDENT_POSTURES = ["live", "attributed", "historical"] as const;
export const COST_POSTURES = ["live_registry", "historical_observation"] as const;
export const ADMISSION_KINDS = ["silent_corruption", "independent_recurrence"] as const;
export const TESTING_SEMANTIC_KINDS = ["testing_operational_watch", "testing_incident", "testing_cost", "testing_system_admission"] as const;

// ---------------------------------------------------------------------------------------------
// Reference-kind universes per citing-field class (moved verbatim from references.ts).

export const DURABLE_OWNER_REFERENCE_KINDS = Object.freeze([
  "consumer_report", "external_commit", "external_issue", "external_release", "file_heading",
  "gate", "matrix_feature", "matrix_role", "roadmap", "spec_passage", "test_symbol",
] as const satisfies readonly ReferenceKindName[]);

export const EXTERNAL_OWNER_REFERENCE_KINDS = Object.freeze([
  "consumer_report", "external_commit", "external_issue", "external_release",
] as const satisfies readonly ReferenceKindName[]);

export const AUTHORITATIVE_REFERENCE_KINDS = Object.freeze([
  "file_heading", "gate", "matrix_feature", "roadmap", "spec_passage",
] as const satisfies readonly ReferenceKindName[]);

const UPSTREAM_REFERENCE_KINDS = Object.freeze([
  "external_commit", "external_issue", "external_release",
] as const satisfies readonly ReferenceKindName[]);

export const EVIDENCE_REFERENCE_KINDS_BY_EVIDENCE_KIND: Readonly<Record<string, readonly ReferenceKindName[]>> = Object.freeze({
  regression_pin: ["gate", "test_symbol"],
  gate: ["gate"],
  harness_free_repro: DURABLE_OWNER_REFERENCE_KINDS,
  committed_vector: ["file_heading", "test_symbol"],
  execution_probe: DURABLE_OWNER_REFERENCE_KINDS,
  registry_enumeration: ["file_heading", "gate", "matrix_cell", "matrix_feature", "matrix_role"],
  source_read: ["file_heading"],
  spec_read: ["spec_passage"],
  consumer_report: ["consumer_report"],
  incident: ["file_heading", "roadmap"],
  external_issue: ["external_issue"],
  external_commit: ["external_commit"],
  decision: ["roadmap"],
});

export const CONTROL_REFERENCE_KINDS_BY_CONTROL_KIND: Readonly<Record<string, readonly ReferenceKindName[]>> = Object.freeze({
  gate: ["gate"],
  test: ["matrix_cell", "test_symbol"],
  fixture: ["file_heading", "test_symbol"],
  review_rule: ["file_heading"],
  consumer_ci: ["consumer_report", "gate"],
  upstream_issue: ["external_issue"],
  operator_procedure: ["file_heading"],
});

// ---------------------------------------------------------------------------------------------
// Construction helpers.

function field(
  name: string,
  presence: "required" | "optional",
  value: PayloadFieldValue,
  query?: PayloadFieldQueryHints,
): PayloadField {
  return Object.freeze({ name, presence, value, ...(query === undefined ? {} : { query }) });
}

const kindField = (): PayloadField => field("kind", "required", { t: "kind" });
const detail = (): PayloadField => field("detail_md", "optional", { t: "markdown" });
const md = (name: string, presence: "required" | "optional" = "required", nonempty?: true): PayloadField =>
  field(name, presence, { t: "markdown", ...(nonempty === undefined ? {} : { nonempty }) });
const str = (name: string, presence: "required" | "optional" = "required"): PayloadField =>
  field(name, presence, { t: "string" });
const en = (name: string, values: readonly string[], presence: "required" | "optional" = "required"): PayloadField =>
  field(name, presence, { t: "enum", values });
const refId = (name: string, allowed: AllowedReferenceKinds, presence: "required" | "optional" = "required", query?: PayloadFieldQueryHints): PayloadField =>
  field(name, presence, { t: "reference_id", allowed }, query);
const refSet = (name: string, allowed: AllowedReferenceKinds, presence: "required" | "optional" = "required", nonempty?: true, query?: PayloadFieldQueryHints): PayloadField =>
  field(name, presence, { t: "reference_id_set", allowed, ...(nonempty === undefined ? {} : { nonempty }) }, query);
const idField = (name: string, target: RoadmapTargetExpectation, presence: "required" | "optional" = "required"): PayloadField =>
  field(name, presence, { t: "roadmap_id", target });
const idSet = (name: string, target: RoadmapTargetExpectation, presence: "required" | "optional" = "required", nonempty?: true): PayloadField =>
  field(name, presence, { t: "roadmap_id_set", target, ...(nonempty === undefined ? {} : { nonempty }) });
const date = (name: string, presence: "required" | "optional" = "required", query?: PayloadFieldQueryHints): PayloadField =>
  field(name, presence, { t: "civil_date" }, query);
const num = (name: string): PayloadField => field(name, "required", { t: "number" });
const strSet = (name: string, presence: "required" | "optional" = "required", nonempty?: true): PayloadField =>
  field(name, presence, { t: "string_set", ...(nonempty === undefined ? {} : { nonempty }) });
const slug = (name: string): PayloadField => field(name, "required", { t: "slug" });

function armRow(
  name: string,
  fields: readonly PayloadField[],
  forbidden?: readonly string[],
): ExactSchemaRow {
  const required = fields.filter((entry) => entry.presence === "required").map((entry) => entry.name);
  const optional = fields.filter((entry) => entry.presence === "optional").map((entry) => entry.name);
  return Object.freeze({
    name,
    required: Object.freeze(required),
    ...(optional.length === 0 ? {} : { optional: Object.freeze(optional) }),
    ...(forbidden === undefined || forbidden.length === 0 ? {} : { forbidden: Object.freeze(forbidden) }),
  });
}

function arm(
  rowName: string,
  when: readonly (readonly [string, readonly string[]])[],
  fields: readonly PayloadField[],
  forbidden?: readonly string[],
): PayloadArm {
  return Object.freeze({ row: armRow(rowName, fields, forbidden), fields: Object.freeze(fields), when });
}

/** Discriminator pre-table: the arms' whole key universe (fields plus forbidden) stays optional. */
export function discriminatorRow(
  name: string,
  required: readonly string[],
  arms: readonly PayloadArm[],
): ExactSchemaRow {
  const seen = new Set(required);
  const optional: string[] = [];
  for (const entry of arms) {
    for (const key of [...entry.fields.map((f) => f.name), ...(entry.row.forbidden ?? [])]) {
      if (seen.has(key)) continue;
      seen.add(key);
      optional.push(key);
    }
  }
  return Object.freeze({ name, required: Object.freeze([...required]), optional: Object.freeze(optional) });
}

// ---------------------------------------------------------------------------------------------
// Shared kinds.

const EVIDENCE_TARGET: RoadmapTargetExpectation = { payload_kind: "evidence" };
const CONTROL_TARGET: RoadmapTargetExpectation = { payload_kind: "control" };
const LIVE_CONTROL_TARGET: RoadmapTargetExpectation = { payload_kind: "control", control_state: "live" };
const REGRESSION_GAP_TARGET: RoadmapTargetExpectation = { payload_kind: "work", work_kind: "regression_gap" };
const ADMISSION_TARGET: RoadmapTargetExpectation = { payload_kind: "testing_system_admission" };
const INCIDENT_TARGET: RoadmapTargetExpectation = { payload_kind: "testing_incident" };
const signalTarget = (
  ...kinds: NonNullable<RoadmapTargetExpectation["transition_kinds"]>[number][]
): RoadmapTargetExpectation => ({ payload_kind: "signal", transition_kinds: kinds });

const workCommon = (): PayloadField[] => [
  kindField(),
  detail(),
  en("work_state", WORK_STATES),
  en("work_intent", WORK_INTENTS),
  en("work_kind", WORK_KINDS),
  en("risk", RISKS),
  idSet("evidence_ids", EVIDENCE_TARGET, "optional"),
];
const workTail = (): PayloadField[] => [
  idSet("regression_evidence_ids", EVIDENCE_TARGET, "optional"),
  idSet("regression_gap_ids", REGRESSION_GAP_TARGET, "optional"),
  idSet("admission_ids", ADMISSION_TARGET, "optional"),
];

export const SIGNAL_PREDICATE_GROUP: NestedGroup = Object.freeze({
  arms: [
    arm("quantitative predicate", [["predicate_kind", ["quantitative"]]], [
      en("predicate_kind", PREDICATE_KINDS),
      en("comparator", COMPARATORS),
      num("threshold"),
      str("unit"),
      str("scope"),
      num("measurement"),
      date("as_of"),
      idSet("evidence_ids", EVIDENCE_TARGET, "optional"),
    ]),
    // evidence_ids is OPTIONAL on the event and manual arms (3A-2 re-cut ruling 1): absent is the
    // defined state "no evidence of the event recorded yet" — the honest state of an unfired
    // trigger.  When present it must still be nonempty.
    arm("event predicate", [["predicate_kind", ["event"]]], [
      en("predicate_kind", PREDICATE_KINDS),
      md("event_md"),
      idSet("evidence_ids", EVIDENCE_TARGET, "optional", true),
    ]),
    arm("manual predicate", [["predicate_kind", ["manual"]]], [
      en("predicate_kind", PREDICATE_KINDS),
      md("review_procedure_md"),
      idSet("evidence_ids", EVIDENCE_TARGET, "optional", true),
    ]),
  ],
});

// ---------------------------------------------------------------------------------------------
// Nested signal groups (Packet 3A-2): the six transition kinds' typed contracts, packaged as
// nested tables on the record that owns them.  The nested field's NAME is the transition kind, so
// `kind`/`transition_kind`/`detail_md` have no nested representation; everything else is the same
// arm contract the standalone signal records keep.

const NESTED_TRIGGER_GROUP_ARMS: readonly PayloadArm[] = [
  arm("nested event-condition trigger", [["predicate.predicate_kind", ["event"]]], [
    str("observer"),
    str("dimension"),
    md("action_on_fire_md"),
    en("evaluation", EVALUATIONS),
    field("predicate", "required", { t: "table", group: SIGNAL_PREDICATE_GROUP }),
  ], ["observable"]),
  arm("nested authored-condition trigger", [["predicate.predicate_kind", ["quantitative", "manual"]]], [
    str("observer"),
    str("dimension"),
    str("observable"),
    md("action_on_fire_md"),
    en("evaluation", EVALUATIONS),
    field("predicate", "required", { t: "table", group: SIGNAL_PREDICATE_GROUP }),
  ]),
];

/** Promotion triggers and reopening signals share the trigger contract; the field name fixes the kind. */
export const NESTED_TRIGGER_GROUP: NestedGroup = Object.freeze({ arms: NESTED_TRIGGER_GROUP_ARMS });

/** Pre-table for the trigger group's predicate-first discrimination (mirrors the standalone flow). */
export const NESTED_TRIGGER_PRESENCE_ROW: ExactSchemaRow = discriminatorRow(
  "nested trigger predicate presence",
  ["predicate"],
  NESTED_TRIGGER_GROUP_ARMS,
);

export const NESTED_UNBLOCK_GROUP: NestedGroup = Object.freeze({
  arms: [
    arm("nested unblock predicate", [], [
      refId("owner_reference_id", DURABLE_OWNER_REFERENCE_KINDS),
      md("event_md"),
      md("check_procedure_md"),
      md("due_action_md"),
      en("evaluation", EVALUATIONS),
    ]),
  ],
});

export const NESTED_WATCH_ESCALATION_GROUP: NestedGroup = Object.freeze({
  arms: [
    arm("nested watch escalation", [], [
      md("failure_signature_md"),
      md("capture_procedure_md"),
      md("response_md"),
      md("escalation_action_md"),
      md("retirement_semantics_md"),
      en("evaluation", EVALUATIONS),
    ]),
  ],
});

export const NESTED_RETIREMENT_GROUP: NestedGroup = Object.freeze({
  arms: [
    arm("nested retirement predicate", [], [
      refId("external_owner_reference_id", EXTERNAL_OWNER_REFERENCE_KINDS),
      md("external_predicate_md"),
      md("verification_md"),
      md("due_action_md"),
      en("evaluation", EVALUATIONS),
    ]),
  ],
});

export const NESTED_CADENCE_GROUP: NestedGroup = Object.freeze({
  arms: [
    arm("nested cadence", [], [
      refId("owner_reference_id", DURABLE_OWNER_REFERENCE_KINDS),
      str("event_source"),
      md("period_or_event_md"),
      md("checklist_md"),
      md("missed_action_md"),
      refId("last_completion_reference_id", DURABLE_OWNER_REFERENCE_KINDS, "optional", { absent: "null" }),
      date("due_on", "optional", { absent: "null" }),
      date("as_of", "optional", { rename: "authored_as_of", absent: "null" }),
      en("evaluation", EVALUATIONS),
    ]),
  ],
});

const nestedTable = (name: string, group: NestedGroup, presence: "required" | "optional" = "required"): PayloadField =>
  field(name, presence, { t: "table", group });

const WORK_ARM_LIST: readonly PayloadArm[] = [
  arm("ready work", [["work_state", ["ready"]]], [
    ...workCommon(),
    md("acceptance_md", "required", true),
    en("priority_band", PRIORITIES, "optional"),
    md("priority_rationale_md", "required", true),
    idSet("control_ids", CONTROL_TARGET, "optional"),
    ...workTail(),
  ], ["blocker_md", "transition_ids", "external_owner_reference_id", "return_condition_md", "uncertainty_md", "unblock_predicate", "promotion_trigger", "reopening_signal", "retirement_predicate"]),
  arm("blocked work", [["work_state", ["blocked"]]], [
    ...workCommon(),
    md("acceptance_md", "optional"),
    md("blocker_md"),
    idSet("control_ids", CONTROL_TARGET, "optional"),
    ...workTail(),
    nestedTable("unblock_predicate", NESTED_UNBLOCK_GROUP),
  ], ["priority_band", "priority_rationale_md", "transition_ids", "external_owner_reference_id", "return_condition_md", "uncertainty_md", "promotion_trigger", "reopening_signal", "retirement_predicate"]),
  arm("armed work", [["work_state", ["armed"]]], [
    ...workCommon(),
    md("acceptance_md", "optional"),
    idSet("control_ids", LIVE_CONTROL_TARGET, "required", true),
    ...workTail(),
    nestedTable("promotion_trigger", NESTED_TRIGGER_GROUP),
  ], ["priority_band", "priority_rationale_md", "blocker_md", "transition_ids", "external_owner_reference_id", "return_condition_md", "uncertainty_md", "unblock_predicate", "reopening_signal", "retirement_predicate"]),
  // Deferred work admits BOTH forms at once (re-cut ruling 3): the nested reopening signal and a
  // citation of a standalone (rendered) signal record; the decoder requires at least one.
  arm("deferred work", [["work_state", ["deferred"]]], [
    ...workCommon(),
    md("acceptance_md", "optional"),
    idSet("control_ids", CONTROL_TARGET, "optional"),
    idSet("transition_ids", signalTarget("reopening_signal"), "optional", true),
    ...workTail(),
    nestedTable("reopening_signal", NESTED_TRIGGER_GROUP, "optional"),
  ], ["priority_band", "priority_rationale_md", "blocker_md", "external_owner_reference_id", "return_condition_md", "uncertainty_md", "unblock_predicate", "promotion_trigger", "retirement_predicate"]),
  // Waiting-external work carries exactly one of the two admissible transition contracts.
  arm("waiting external work", [["work_state", ["waiting_external"]]], [
    ...workCommon(),
    md("acceptance_md", "optional"),
    idSet("control_ids", CONTROL_TARGET, "optional"),
    refId("external_owner_reference_id", EXTERNAL_OWNER_REFERENCE_KINDS),
    ...workTail(),
    nestedTable("retirement_predicate", NESTED_RETIREMENT_GROUP, "optional"),
    nestedTable("unblock_predicate", NESTED_UNBLOCK_GROUP, "optional"),
  ], ["priority_band", "priority_rationale_md", "blocker_md", "transition_ids", "return_condition_md", "uncertainty_md", "promotion_trigger", "reopening_signal"]),
  arm("delegated work", [["work_state", ["delegated"]]], [
    ...workCommon(),
    md("acceptance_md", "optional"),
    idSet("control_ids", CONTROL_TARGET, "optional"),
    md("return_condition_md"),
    ...workTail(),
  ], ["priority_band", "priority_rationale_md", "blocker_md", "transition_ids", "external_owner_reference_id", "uncertainty_md", "unblock_predicate", "promotion_trigger", "reopening_signal", "retirement_predicate"]),
  arm("pending review work", [["work_state", ["pending_review"]]], [
    ...workCommon(),
    idSet("control_ids", CONTROL_TARGET, "optional"),
    md("uncertainty_md"),
    ...workTail(),
  ], ["acceptance_md", "priority_band", "priority_rationale_md", "blocker_md", "transition_ids", "external_owner_reference_id", "return_condition_md", "unblock_predicate", "promotion_trigger", "reopening_signal", "retirement_predicate"]),
];

const DECISION_ARM_LIST: readonly PayloadArm[] = [
  arm("pending decision", [["decision_state", ["pending"]]], [
    kindField(),
    detail(),
    en("decision_state", DECISION_STATES),
    md("question_md"),
    nestedTable("unblock_predicate", NESTED_UNBLOCK_GROUP),
  ], ["rationale_md", "authority_reference_id", "permanence", "transition_ids", "reopening_signal"]),
  arm("held decision", [["decision_state", ["held"]]], [
    kindField(),
    detail(),
    en("decision_state", DECISION_STATES),
    md("rationale_md"),
    en("permanence", ["reopenable"]),
    nestedTable("reopening_signal", NESTED_TRIGGER_GROUP),
  ], ["question_md", "authority_reference_id", "transition_ids", "unblock_predicate"]),
  arm("decided permanent decision", [["decision_state", ["decided"]], ["permanence", ["permanent"]]], [
    kindField(),
    detail(),
    en("decision_state", DECISION_STATES),
    md("rationale_md"),
    refId("authority_reference_id", AUTHORITATIVE_REFERENCE_KINDS),
    en("permanence", PERMANENCE),
  ], ["question_md", "transition_ids", "reopening_signal", "unblock_predicate"]),
  arm("decided reopenable decision", [["decision_state", ["decided"]], ["permanence", ["reopenable"]]], [
    kindField(),
    detail(),
    en("decision_state", DECISION_STATES),
    md("rationale_md"),
    refId("authority_reference_id", AUTHORITATIVE_REFERENCE_KINDS),
    en("permanence", PERMANENCE),
    nestedTable("reopening_signal", NESTED_TRIGGER_GROUP),
  ], ["question_md", "transition_ids", "unblock_predicate"]),
];

const triggerWhen = (predicateKinds: readonly string[]): readonly (readonly [string, readonly string[]])[] => [
  ["transition_kind", ["promotion_trigger", "reopening_signal"]],
  ["predicate.predicate_kind", predicateKinds],
];

const SIGNAL_ARM_LIST: readonly PayloadArm[] = [
  arm("event-condition promotion or reopening signal", triggerWhen(["event"]), [
    kindField(),
    detail(),
    en("transition_kind", TRANSITION_KINDS),
    str("observer"),
    str("dimension"),
    md("action_on_fire_md"),
    en("evaluation", EVALUATIONS),
    field("predicate", "required", { t: "table", group: SIGNAL_PREDICATE_GROUP }),
  ], ["observable"]),
  arm("authored-condition promotion or reopening signal", triggerWhen(["quantitative", "manual"]), [
    kindField(),
    detail(),
    en("transition_kind", TRANSITION_KINDS),
    str("observer"),
    str("dimension"),
    str("observable"),
    md("action_on_fire_md"),
    en("evaluation", EVALUATIONS),
    field("predicate", "required", { t: "table", group: SIGNAL_PREDICATE_GROUP }),
  ]),
  arm("unblock predicate", [["transition_kind", ["unblock_predicate"]]], [
    kindField(),
    detail(),
    en("transition_kind", TRANSITION_KINDS),
    refId("owner_reference_id", DURABLE_OWNER_REFERENCE_KINDS),
    md("event_md"),
    md("check_procedure_md"),
    md("due_action_md"),
    en("evaluation", EVALUATIONS),
  ]),
  arm("watch escalation", [["transition_kind", ["watch_escalation"]]], [
    kindField(),
    detail(),
    en("transition_kind", TRANSITION_KINDS),
    md("failure_signature_md"),
    md("capture_procedure_md"),
    md("response_md"),
    md("escalation_action_md"),
    md("retirement_semantics_md"),
    en("evaluation", EVALUATIONS),
  ]),
  arm("retirement predicate", [["transition_kind", ["retirement_predicate"]]], [
    kindField(),
    detail(),
    en("transition_kind", TRANSITION_KINDS),
    refId("external_owner_reference_id", EXTERNAL_OWNER_REFERENCE_KINDS),
    md("external_predicate_md"),
    md("verification_md"),
    md("due_action_md"),
    en("evaluation", EVALUATIONS),
  ]),
  arm("cadence signal", [["transition_kind", ["cadence"]]], [
    kindField(),
    detail(),
    en("transition_kind", TRANSITION_KINDS),
    refId("owner_reference_id", DURABLE_OWNER_REFERENCE_KINDS),
    str("event_source"),
    md("period_or_event_md"),
    md("checklist_md"),
    md("missed_action_md"),
    refId("last_completion_reference_id", DURABLE_OWNER_REFERENCE_KINDS, "optional", { absent: "null" }),
    date("due_on", "optional", { absent: "null" }),
    date("as_of", "optional", { rename: "authored_as_of", absent: "null" }),
    en("evaluation", EVALUATIONS),
  ]),
];

export const EVIDENCE_SCOPE_GROUP: NestedGroup = Object.freeze({
  arms: [
    arm("evidence scope", [], [
      strSet("surfaces", "optional", true),
      strSet("faces", "optional", true),
      strSet("profiles", "optional", true),
      strSet("flags", "optional", true),
      strSet("input_modes", "optional", true),
      strSet("toolchains", "optional", true),
      strSet("executors", "optional", true),
      strSet("tiers", "optional", true),
    ]),
  ],
});

const EVIDENCE_ALLOWED: AllowedReferenceKinds = {
  by: "evidence_kind",
  map: EVIDENCE_REFERENCE_KINDS_BY_EVIDENCE_KIND,
};

const EVIDENCE_ARM_LIST: readonly PayloadArm[] = [
  arm("evidence", [], [
    kindField(),
    detail(),
    en("evidence_kind", EVIDENCE_KINDS),
    md("claim_md"),
    en("evidence_verdict", EVIDENCE_VERDICTS),
    en("freshness", FRESHNESS),
    refSet("reference_ids", EVIDENCE_ALLOWED, "required", true),
    md("command_md", "optional"),
    md("result_md", "optional"),
    field("at_commit", "optional", { t: "commit" }),
    date("observed_at", "optional"),
    date("valid_through", "optional"),
    md("environment_md", "optional"),
    md("unprobed_remainder_md"),
    refId("refresh_reference_id", EVIDENCE_ALLOWED, "optional"),
    str("enumerated_registry", "optional"),
    field("scope", "required", { t: "table", group: EVIDENCE_SCOPE_GROUP }),
  ]),
];

const CONTROL_ARM_LIST: readonly PayloadArm[] = [
  arm("control", [], [
    kindField(),
    detail(),
    en("control_kind", CONTROL_KINDS),
    en("control_state", CONTROL_STATES),
    refSet("reference_ids", { by: "control_kind", map: CONTROL_REFERENCE_KINDS_BY_CONTROL_KIND }, "required", true),
    md("claim_md"),
    md("boundary_md"),
  ]),
];

// ---------------------------------------------------------------------------------------------
// Matrix kinds.

export const CLOSEOUT_ACTION_GROUP: NestedGroup = Object.freeze({
  arms: [arm("matrix closeout action", [], [slug("action_id"), md("action_md")])],
});

export const CLOSEOUT_BRANCH_ACTION_GROUP: NestedGroup = Object.freeze({
  arms: [arm("matrix closeout branch action", [], [slug("action_id")])],
});

export const CLOSEOUT_BRANCH_GROUP: NestedGroup = Object.freeze({
  arms: [
    arm("matrix closeout branch", [], [
      slug("branch_id"),
      md("predicate_md"),
      refSet("prune_reference_ids", UPSTREAM_REFERENCE_KINDS, "optional", true),
      field("action", "required", {
        t: "array_table",
        group: CLOSEOUT_BRANCH_ACTION_GROUP,
        prop: "action_ids",
        id_field: "action_id",
        nonempty: true,
        flatten: "action_id",
      }),
    ]),
  ],
});

const closeoutFields = (state: "waiting" | "due" | "blocked"): PayloadField[] => [
  kindField(),
  detail(),
  en("closeout_state", CLOSEOUT_STATES),
  refId("upstream_owner_reference_id", UPSTREAM_REFERENCE_KINDS),
  md("current_upstream_state_md"),
  ...(state === "blocked" ? [md("blocker_md")] : []),
  md("verification_md"),
  refSet("prune_reference_ids", UPSTREAM_REFERENCE_KINDS, "optional", true, { absent: "empty_array" }),
  nestedTable("retirement_predicate", NESTED_RETIREMENT_GROUP),
  field("action", state === "due" ? "required" : "optional", {
    t: "array_table",
    group: CLOSEOUT_ACTION_GROUP,
    prop: "actions",
    id_field: "action_id",
    default_empty: true,
  }),
  field("branch", "optional", {
    t: "array_table",
    group: CLOSEOUT_BRANCH_GROUP,
    prop: "branches",
    id_field: "branch_id",
    default_empty: true,
  }),
];

const CLOSEOUT_ARM_LIST: readonly PayloadArm[] = [
  arm("waiting matrix closeout", [["closeout_state", ["waiting"]]], closeoutFields("waiting"), ["blocker_md", "transition_ids"]),
  arm("due matrix closeout", [["closeout_state", ["due"]]], closeoutFields("due"), ["blocker_md", "transition_ids"]),
  arm("blocked matrix closeout", [["closeout_state", ["blocked"]]], closeoutFields("blocked"), ["transition_ids"]),
];

const POLICY_ARM_LIST: readonly PayloadArm[] = [
  arm("matrix maintenance policy", [["policy_kind", ["maintenance_protocol"]]], [
    kindField(),
    detail(),
    en("policy_kind", POLICY_KINDS),
    refId("authority_reference_id", AUTHORITATIVE_REFERENCE_KINDS),
    md("protocol_md"),
    nestedTable("cadence", NESTED_CADENCE_GROUP),
  ], ["cadence_transition_id", "reopening_transition_id", "reopening_signal"]),
  arm("matrix permanent boundary", [["policy_kind", ["boundary"]], ["permanence", ["permanent"]]], [
    kindField(),
    detail(),
    en("policy_kind", POLICY_KINDS),
    refId("authority_reference_id", AUTHORITATIVE_REFERENCE_KINDS),
    md("rationale_md"),
    en("permanence", PERMANENCE),
  ], ["cadence_transition_id", "reopening_transition_id", "cadence", "reopening_signal"]),
  arm("matrix reopenable boundary", [["policy_kind", ["boundary"]], ["permanence", ["reopenable"]]], [
    kindField(),
    detail(),
    en("policy_kind", POLICY_KINDS),
    refId("authority_reference_id", AUTHORITATIVE_REFERENCE_KINDS),
    md("rationale_md"),
    en("permanence", PERMANENCE),
    nestedTable("reopening_signal", NESTED_TRIGGER_GROUP),
  ], ["cadence_transition_id", "reopening_transition_id", "cadence"]),
];

// ---------------------------------------------------------------------------------------------
// Testing kinds.

export const CAPTURE_STEP_GROUP: NestedGroup = Object.freeze({
  arms: [arm("watch capture step", [], [slug("step_id"), md("capture_md")])],
});

// A watch carries exactly one of the two escalation forms: the nested watch_escalation, or a
// citation of a standalone (rendered) watch-escalation signal record (re-cut ruling 2).
const watchFields = (state: "watching" | "attributed" | "retire_pending"): PayloadField[] => [
  kindField(),
  detail(),
  en("watch_state", WATCH_STATES),
  md("signature_md"),
  ...(state === "watching" ? [] : [
    md("attribution_md"),
    refId("operating_rule_reference_id", ["file_heading", "gate"]),
  ]),
  md("response_md"),
  idField("escalation_transition_id", signalTarget("watch_escalation"), "optional"),
  ...(state === "retire_pending" ? [refId("retirement_reference_id", ["file_heading", "gate", "test_symbol"])] : []),
  md("retirement_semantics_md"),
  nestedTable("watch_escalation", NESTED_WATCH_ESCALATION_GROUP, "optional"),
  field("capture_step", "required", {
    t: "array_table",
    group: CAPTURE_STEP_GROUP,
    prop: "capture_steps",
    id_field: "step_id",
    nonempty: true,
  }),
];

const WATCH_ARM_LIST: readonly PayloadArm[] = [
  arm("watching operational watch", [["watch_state", ["watching"]]], watchFields("watching"), ["attribution_md", "operating_rule_reference_id", "retirement_reference_id"]),
  arm("attributed operational watch", [["watch_state", ["attributed"]]], watchFields("attributed"), ["retirement_reference_id"]),
  arm("retire-pending operational watch", [["watch_state", ["retire_pending"]]], watchFields("retire_pending")),
];

const incidentFields = (posture: "live" | "attributed" | "historical"): PayloadField[] => [
  kindField(),
  detail(),
  en("incident_posture", INCIDENT_POSTURES),
  md("signature_md"),
  idSet("evidence_ids", EVIDENCE_TARGET, "required", true),
  ...(posture === "live" ? [] : [
    md("attribution_md"),
    refId("operating_rule_reference_id", ["file_heading", "gate"]),
  ]),
  ...(posture === "historical" ? [refId("retirement_reference_id", ["file_heading", "gate", "test_symbol"])] : []),
];

const INCIDENT_ARM_LIST: readonly PayloadArm[] = [
  arm("live testing incident", [["incident_posture", ["live"]]], incidentFields("live"), ["attribution_md", "operating_rule_reference_id", "retirement_reference_id"]),
  arm("attributed testing incident", [["incident_posture", ["attributed"]]], incidentFields("attributed"), ["retirement_reference_id"]),
  arm("historical testing incident", [["incident_posture", ["historical"]]], incidentFields("historical")),
];

const COST_ARM_LIST: readonly PayloadArm[] = [
  arm("live registry testing cost", [["cost_posture", ["live_registry"]]], [
    kindField(),
    detail(),
    en("cost_posture", COST_POSTURES),
    str("unit"),
    md("scope_md"),
    refId("gate_reference_id", ["gate"]),
  ], ["value_min", "value_max", "observed_at", "environment_md", "evidence_ids", "valid_through"]),
  arm("historical testing cost", [["cost_posture", ["historical_observation"]]], [
    kindField(),
    detail(),
    en("cost_posture", COST_POSTURES),
    str("unit"),
    md("scope_md"),
    num("value_min"),
    num("value_max"),
    date("observed_at"),
    md("environment_md"),
    idSet("evidence_ids", EVIDENCE_TARGET, "required", true),
  ], ["gate_reference_id", "valid_through"]),
];

const ADMISSION_ARM_LIST: readonly PayloadArm[] = [
  arm("silent-corruption admission", [["admission_kind", ["silent_corruption"]]], [
    kindField(),
    detail(),
    en("admission_kind", ADMISSION_KINDS),
    md("claim_md"),
    idSet("evidence_ids", EVIDENCE_TARGET, "required", true),
  ], ["incident_ids"]),
  arm("independent-recurrence admission", [["admission_kind", ["independent_recurrence"]]], [
    kindField(),
    detail(),
    en("admission_kind", ADMISSION_KINDS),
    md("claim_md"),
    idSet("evidence_ids", EVIDENCE_TARGET, "required", true),
    idSet("incident_ids", INCIDENT_TARGET, "required", true),
  ]),
];

// ---------------------------------------------------------------------------------------------
// The assembled table, keyed by payload kind.

export const PAYLOAD_KIND_ARMS: Readonly<Record<SemanticPayload["kind"], readonly PayloadArm[]>> = Object.freeze({
  work: WORK_ARM_LIST,
  decision: DECISION_ARM_LIST,
  signal: SIGNAL_ARM_LIST,
  evidence: EVIDENCE_ARM_LIST,
  control: CONTROL_ARM_LIST,
  matrix_external_closeout: CLOSEOUT_ARM_LIST,
  matrix_policy: POLICY_ARM_LIST,
  testing_operational_watch: WATCH_ARM_LIST,
  testing_incident: INCIDENT_ARM_LIST,
  testing_cost: COST_ARM_LIST,
  testing_system_admission: ADMISSION_ARM_LIST,
});

function dottedValue(value: unknown, path: string): unknown {
  let current: unknown = value;
  for (const part of path.split(".")) {
    if (current === null || typeof current !== "object") return undefined;
    current = (current as Record<string, unknown>)[part];
  }
  return current;
}

function armMatches(armSpec: PayloadArm, value: unknown): boolean {
  return armSpec.when.every(([path, values]) => {
    const found = dottedValue(value, path);
    return typeof found === "string" && values.includes(found);
  });
}

/** Select the closed arm a decoded payload inhabits (the table names the payload's own kind). */
export function armOfPayload(payload: SemanticPayload): PayloadArm {
  const arms = PAYLOAD_KIND_ARMS[payload.kind];
  const found = arms.find((candidate) => armMatches(candidate, payload));
  if (found === undefined) {
    throw new Error(`payload of kind ${payload.kind} matches no descriptor arm`);
  }
  return found;
}

/**
 * Select an arm from decoded discriminants alone (drivers call this mid-decode, before the arm's
 * remaining fields exist).  The probe object carries exactly the discriminant chain — and, for
 * trigger signals, the already-decoded nested predicate.
 */
export function armForDiscriminants(
  kind: SemanticPayload["kind"],
  probe: Readonly<Record<string, unknown>>,
): PayloadArm {
  const arms = PAYLOAD_KIND_ARMS[kind];
  const found = arms.find((candidate) => armMatches(candidate, probe));
  if (found === undefined) {
    throw new Error(`discriminants select no ${kind} descriptor arm`);
  }
  return found;
}

/** Select a nested group's arm for a decoded nested value (single-arm groups match trivially). */
export function armOfGroupValue(group: NestedGroup, value: unknown): PayloadArm {
  const found = group.arms.find((candidate) => armMatches(candidate, value));
  if (found === undefined) {
    throw new Error("nested payload value matches no descriptor group arm");
  }
  return found;
}

/** The decoded payload property a field materializes as (array tables may rename). */
export function fieldProperty(entry: PayloadField): string {
  return entry.value.t === "array_table" ? entry.value.prop : entry.name;
}

/**
 * The arm's query-row entries: every field of the payload's arm in order, minus `kind` and the
 * caller's exclusions, honoring the per-field query hints (renames; null / empty-array defaults
 * for absent optionals).  View-specific computed values stay at the call site — this derives the
 * SHAPE, so a new payload field appears in its arm's query rows without touching query code.
 */
export function armQueryEntries(
  payload: SemanticPayload,
  exclude: readonly string[] = [],
): readonly (readonly [string, unknown])[] {
  const arm = armOfPayload(payload);
  const entries: (readonly [string, unknown])[] = [];
  for (const entry of arm.fields) {
    if (entry.value.t === "kind" || exclude.includes(entry.name)) continue;
    const prop = fieldProperty(entry);
    let value: unknown = (payload as unknown as Record<string, unknown>)[prop];
    if (value === undefined) {
      if (entry.query?.absent === "null") value = null;
      else if (entry.query?.absent === "empty_array") value = [];
      else continue;
    }
    entries.push([entry.query?.rename ?? prop, value] as const);
  }
  return entries;
}

// ---------------------------------------------------------------------------------------------
// Discriminator rows (pre-tables of the state machines; names are load-bearing in diagnostics).

export const DISCRIMINATOR_ROWS = Object.freeze({
  work: discriminatorRow("work discriminator", ["kind", "work_state"], WORK_ARM_LIST),
  decision: discriminatorRow("decision discriminator", ["kind", "decision_state"], DECISION_ARM_LIST),
  decided_decision: discriminatorRow(
    "decided decision discriminator",
    ["kind", "decision_state", "permanence"],
    DECISION_ARM_LIST.slice(2),
  ),
  signal: discriminatorRow("signal discriminator", ["kind", "transition_kind"], SIGNAL_ARM_LIST),
  signal_predicate_presence: discriminatorRow(
    "signal predicate presence",
    ["kind", "transition_kind", "predicate"],
    SIGNAL_ARM_LIST.slice(0, 2),
  ),
  signal_predicate: discriminatorRow(
    "signal predicate discriminator",
    ["predicate_kind"],
    SIGNAL_PREDICATE_GROUP.arms,
  ),
  matrix_closeout: discriminatorRow("matrix closeout discriminator", ["kind", "closeout_state"], CLOSEOUT_ARM_LIST),
  matrix_policy: discriminatorRow("matrix policy discriminator", ["kind", "policy_kind"], POLICY_ARM_LIST),
  matrix_boundary: discriminatorRow(
    "matrix boundary discriminator",
    ["kind", "policy_kind", "permanence"],
    POLICY_ARM_LIST.slice(1),
  ),
  testing_watch: discriminatorRow("operational watch discriminator", ["kind", "watch_state"], WATCH_ARM_LIST),
  testing_incident: discriminatorRow("testing incident discriminator", ["kind", "incident_posture"], INCIDENT_ARM_LIST),
  testing_cost: discriminatorRow("testing cost discriminator", ["kind", "cost_posture"], COST_ARM_LIST),
  testing_admission: discriminatorRow("testing admission discriminator", ["kind", "admission_kind"], ADMISSION_ARM_LIST),
} as const satisfies Readonly<Record<string, ExactSchemaRow>>);

// ---------------------------------------------------------------------------------------------
// The exported exact-schema row registries, in their frozen legacy order.  The decode modules
// re-export these under their historical names; the schema selftests key mutation coverage on
// these exact object identities.

export const SHARED_SEMANTIC_SCHEMA_ROW_LIST: readonly ExactSchemaRow[] = Object.freeze([
  ...WORK_ARM_LIST.map((entry) => entry.row),
  ...DECISION_ARM_LIST.slice(0, 3).map((entry) => entry.row),
  DECISION_ARM_LIST[3]!.row,
  SIGNAL_ARM_LIST[0]!.row,
  ...SIGNAL_ARM_LIST.slice(2).map((entry) => entry.row),
  ...SIGNAL_PREDICATE_GROUP.arms.map((entry) => entry.row),
  EVIDENCE_ARM_LIST[0]!.row,
  EVIDENCE_SCOPE_GROUP.arms[0]!.row,
  CONTROL_ARM_LIST[0]!.row,
  SIGNAL_ARM_LIST[1]!.row,
  ...NESTED_TRIGGER_GROUP.arms.map((entry) => entry.row),
  NESTED_UNBLOCK_GROUP.arms[0]!.row,
  NESTED_WATCH_ESCALATION_GROUP.arms[0]!.row,
  NESTED_RETIREMENT_GROUP.arms[0]!.row,
  NESTED_CADENCE_GROUP.arms[0]!.row,
]);

export const MATRIX_SCHEMA_ROW_LIST: readonly ExactSchemaRow[] = Object.freeze([
  ...CLOSEOUT_ARM_LIST.map((entry) => entry.row),
  CLOSEOUT_ACTION_GROUP.arms[0]!.row,
  CLOSEOUT_BRANCH_GROUP.arms[0]!.row,
  CLOSEOUT_BRANCH_ACTION_GROUP.arms[0]!.row,
  ...POLICY_ARM_LIST.map((entry) => entry.row),
]);

export const TESTING_SCHEMA_ROW_LIST: readonly ExactSchemaRow[] = Object.freeze([
  ...WATCH_ARM_LIST.map((entry) => entry.row),
  CAPTURE_STEP_GROUP.arms[0]!.row,
  ...INCIDENT_ARM_LIST.map((entry) => entry.row),
  ...COST_ARM_LIST.map((entry) => entry.row),
  ...ADMISSION_ARM_LIST.map((entry) => entry.row),
]);
