import type {
  ControlPayload,
  DecisionPayload,
  EvidencePayload,
  EvidenceScope,
  SharedSemanticPayload,
  SignalPayload,
  SignalPredicate,
  WorkPayload,
} from "../model/core.ts";
import type { SemanticPayload } from "../model/documents.ts";
import {
  canonicalSet,
  childLogicalPath as p,
  expectArrayOf,
  expectCivilDate,
  expectEnum,
  expectExactTable,
  expectFiniteNumber,
  expectFullCommitId,
  expectMarkdown,
  expectNonemptyArray,
  expectReferenceId,
  expectReferenceIdSet,
  expectRoadmapId,
  expectRoadmapIdSet,
  expectString,
  expectStringSet,
  hasOwn,
  optionalDecoded,
  optionalValue,
  requiredValue,
  schemaFail,
  type DecodeContext,
  type EnumSchemaField,
  type ExactSchemaRow,
} from "./primitives.ts";
import { codePointSort } from "../kernel.ts";

const WORK_STATES = ["ready", "blocked", "armed", "deferred", "waiting_external", "delegated", "pending_review"] as const;
const WORK_INTENTS = ["repair", "add_regression", "build_capability", "build_system", "establish_honest_refusal", "optimize", "change_documentation"] as const;
const WORK_KINDS = ["defect", "regression_gap", "coverage_cell", "missing_system", "feature", "optimization", "documentation_integrity", "infrastructure"] as const;
const RISKS = ["silent_wrong_bytes", "invalid_acceptance", "valid_rejection", "wrong_public_api", "compile_failure", "abort_or_panic", "false_pass_or_red", "misleading_docs", "resource_exhaustion", "cosmetic"] as const;
const PRIORITIES = ["critical", "high", "normal", "low"] as const;
const EVALUATIONS = ["met", "unmet", "unknown", "stale"] as const;
const COMPARATORS = ["lt", "le", "eq", "ge", "gt"] as const;
const EVIDENCE_KINDS = ["regression_pin", "gate", "harness_free_repro", "committed_vector", "execution_probe", "registry_enumeration", "source_read", "spec_read", "consumer_report", "incident", "external_issue", "external_commit", "decision"] as const;
const EVIDENCE_VERDICTS = ["proposed", "confirmed", "falsified", "unknown", "inapplicable"] as const;
const FRESHNESS = ["live", "as_of", "historical", "stale"] as const;
const CONTROL_KINDS = ["gate", "test", "fixture", "review_rule", "consumer_ci", "upstream_issue", "operator_procedure"] as const;

export const SEMANTIC_ENUM_FIELDS: readonly EnumSchemaField[] = [
  { name: "shared_semantic_kind", values: ["work", "decision", "signal", "evidence", "control"] },
  { name: "work_state", values: WORK_STATES },
  { name: "work_intent", values: WORK_INTENTS },
  { name: "work_kind", values: WORK_KINDS },
  { name: "risk", values: RISKS },
  { name: "priority_band", values: PRIORITIES },
  { name: "decision_state", values: ["pending", "held", "decided"] },
  { name: "decision_permanence", values: ["permanent", "reopenable"] },
  { name: "transition_kind", values: ["promotion_trigger", "reopening_signal", "unblock_predicate", "watch_escalation", "retirement_predicate", "cadence"] },
  { name: "signal_evaluation", values: EVALUATIONS },
  { name: "predicate_kind", values: ["quantitative", "event", "manual"] },
  { name: "comparator", values: COMPARATORS },
  { name: "evidence_kind", values: EVIDENCE_KINDS },
  { name: "evidence_verdict", values: EVIDENCE_VERDICTS },
  { name: "freshness", values: FRESHNESS },
  { name: "control_kind", values: CONTROL_KINDS },
  { name: "control_state", values: ["live", "proposed", "stale"] },
];

const DETAIL = ["detail_md"] as const;
const WORK_COMMON = ["kind", "work_state", "work_intent", "work_kind", "risk"] as const;
const WORK_OPTIONALS = ["detail_md", "evidence_ids", "acceptance_md", "priority_band", "priority_rationale_md", "blocker_md", "control_ids", "transition_ids", "external_owner_reference_id", "return_condition_md", "uncertainty_md", "regression_evidence_ids", "regression_gap_ids", "admission_ids"] as const;

export const SHARED_SEMANTIC_SCHEMA_ROWS: readonly ExactSchemaRow[] = [
  { name: "ready work", required: [...WORK_COMMON, "acceptance_md", "priority_rationale_md"], optional: ["detail_md", "evidence_ids", "priority_band", "control_ids", "regression_evidence_ids", "regression_gap_ids", "admission_ids"], forbidden: ["blocker_md", "transition_ids", "external_owner_reference_id", "return_condition_md", "uncertainty_md"] },
  { name: "blocked work", required: [...WORK_COMMON, "blocker_md", "transition_ids"], optional: ["detail_md", "evidence_ids", "acceptance_md", "control_ids", "regression_evidence_ids", "regression_gap_ids", "admission_ids"], forbidden: ["priority_band", "priority_rationale_md", "external_owner_reference_id", "return_condition_md", "uncertainty_md"] },
  { name: "armed work", required: [...WORK_COMMON, "control_ids", "transition_ids"], optional: ["detail_md", "evidence_ids", "acceptance_md", "regression_evidence_ids", "regression_gap_ids", "admission_ids"], forbidden: ["priority_band", "priority_rationale_md", "blocker_md", "external_owner_reference_id", "return_condition_md", "uncertainty_md"] },
  { name: "deferred work", required: [...WORK_COMMON, "transition_ids"], optional: ["detail_md", "evidence_ids", "acceptance_md", "control_ids", "regression_evidence_ids", "regression_gap_ids", "admission_ids"], forbidden: ["priority_band", "priority_rationale_md", "blocker_md", "external_owner_reference_id", "return_condition_md", "uncertainty_md"] },
  { name: "waiting external work", required: [...WORK_COMMON, "transition_ids", "external_owner_reference_id"], optional: ["detail_md", "evidence_ids", "acceptance_md", "control_ids", "regression_evidence_ids", "regression_gap_ids", "admission_ids"], forbidden: ["priority_band", "priority_rationale_md", "blocker_md", "return_condition_md", "uncertainty_md"] },
  { name: "delegated work", required: [...WORK_COMMON, "return_condition_md"], optional: ["detail_md", "evidence_ids", "acceptance_md", "control_ids", "regression_evidence_ids", "regression_gap_ids", "admission_ids"], forbidden: ["priority_band", "priority_rationale_md", "blocker_md", "transition_ids", "external_owner_reference_id", "uncertainty_md"] },
  { name: "pending review work", required: [...WORK_COMMON, "uncertainty_md"], optional: ["detail_md", "evidence_ids", "control_ids", "regression_evidence_ids", "regression_gap_ids", "admission_ids"], forbidden: ["acceptance_md", "priority_band", "priority_rationale_md", "blocker_md", "transition_ids", "external_owner_reference_id", "return_condition_md"] },
  { name: "pending decision", required: ["kind", "decision_state", "question_md", "transition_ids"], optional: DETAIL, forbidden: ["rationale_md", "authority_reference_id", "permanence"] },
  { name: "held decision", required: ["kind", "decision_state", "rationale_md", "permanence", "transition_ids"], optional: DETAIL, forbidden: ["question_md", "authority_reference_id"] },
  { name: "decided permanent decision", required: ["kind", "decision_state", "rationale_md", "authority_reference_id", "permanence"], optional: DETAIL, forbidden: ["question_md", "transition_ids"] },
  { name: "decided reopenable decision", required: ["kind", "decision_state", "rationale_md", "authority_reference_id", "permanence", "transition_ids"], optional: DETAIL, forbidden: ["question_md"] },
  { name: "event-condition promotion or reopening signal", required: ["kind", "transition_kind", "observer", "dimension", "action_on_fire_md", "evaluation", "predicate"], optional: DETAIL, forbidden: ["observable"] },
  { name: "unblock predicate", required: ["kind", "transition_kind", "owner_reference_id", "event_md", "check_procedure_md", "due_action_md", "evaluation"], optional: DETAIL },
  { name: "watch escalation", required: ["kind", "transition_kind", "failure_signature_md", "capture_procedure_md", "response_md", "escalation_action_md", "retirement_semantics_md", "evaluation"], optional: DETAIL },
  { name: "retirement predicate", required: ["kind", "transition_kind", "external_owner_reference_id", "external_predicate_md", "verification_md", "due_action_md", "evaluation"], optional: DETAIL },
  { name: "cadence signal", required: ["kind", "transition_kind", "owner_reference_id", "event_source", "period_or_event_md", "checklist_md", "missed_action_md", "evaluation"], optional: ["detail_md", "last_completion_reference_id", "due_on", "as_of"] },
  { name: "quantitative predicate", required: ["predicate_kind", "comparator", "threshold", "unit", "scope", "measurement", "as_of"], optional: ["evidence_ids"] },
  { name: "event predicate", required: ["predicate_kind", "event_md", "evidence_ids"] },
  { name: "manual predicate", required: ["predicate_kind", "review_procedure_md", "evidence_ids"] },
  { name: "evidence", required: ["kind", "evidence_kind", "claim_md", "evidence_verdict", "freshness", "reference_ids", "unprobed_remainder_md", "scope"], optional: ["detail_md", "command_md", "result_md", "at_commit", "observed_at", "valid_through", "environment_md", "refresh_reference_id", "enumerated_registry"] },
  { name: "evidence scope", required: [], optional: ["surfaces", "faces", "profiles", "flags", "input_modes", "toolchains", "executors", "tiers"] },
  { name: "control", required: ["kind", "control_kind", "control_state", "reference_ids", "claim_md", "boundary_md"], optional: DETAIL },
  { name: "authored-condition promotion or reopening signal", required: ["kind", "transition_kind", "observer", "dimension", "observable", "action_on_fire_md", "evaluation", "predicate"], optional: DETAIL },
] as const;

function common(ctx: DecodeContext, table: object, path: string): { detail_md?: Uint8Array } {
  return {
    ...optionalMarkdown(ctx, table, path, "detail_md"),
  };
}

function optionalMarkdown(ctx: DecodeContext, table: object, path: string, key: string): { [key: string]: Uint8Array } | object {
  if (!hasOwn(table, key)) return {};
  return { [key]: expectMarkdown(ctx, optionalValue(table, key), p(path, key)) };
}

function expectNonemptyMarkdown(ctx: DecodeContext, table: object, path: string, key: string): Uint8Array {
  const value = expectMarkdown(ctx, requiredValue(table, key), p(path, key));
  if (value.length === 0) schemaFail(ctx, "E-SCHEMA-FLOOR", p(path, key), `${key} must be nonempty`);
  return value;
}

function optionalRoadmapSet(ctx: DecodeContext, table: object, path: string, key: string): { [key: string]: import("../model/core.ts").RoadmapId[] } | object {
  if (!hasOwn(table, key)) return {};
  return { [key]: expectRoadmapIdSet(ctx, optionalValue(table, key), p(path, key)) };
}

function decodeWork(ctx: DecodeContext, raw: unknown, path: string): WorkPayload {
  const discriminator = expectExactTable(ctx, raw, path, {
    name: "work discriminator",
    required: ["kind", "work_state"],
    optional: ["work_intent", "work_kind", "risk", ...WORK_OPTIONALS],
  });
  const state = expectEnum(ctx, requiredValue(discriminator, "work_state"), WORK_STATES, p(path, "work_state"));
  const schema = SHARED_SEMANTIC_SCHEMA_ROWS[WORK_STATES.indexOf(state)];
  const table = expectExactTable(ctx, raw, path, schema);
  const workKind = expectEnum(ctx, requiredValue(table, "work_kind"), WORK_KINDS, p(path, "work_kind"));
  const regressionEvidence = optionalDecoded(table, "regression_evidence_ids", path, (value, fieldPath) => expectRoadmapIdSet(ctx, value, fieldPath));
  const regressionGaps = optionalDecoded(table, "regression_gap_ids", path, (value, fieldPath) => expectRoadmapIdSet(ctx, value, fieldPath));
  const admissions = optionalDecoded(table, "admission_ids", path, (value, fieldPath) => expectRoadmapIdSet(ctx, value, fieldPath));
  if (workKind === "defect" && (regressionEvidence?.length ?? 0) + (regressionGaps?.length ?? 0) === 0) {
    schemaFail(ctx, "E-SCHEMA-STATE", path, "defect work requires regression evidence or a regression-gap ID");
  }
  if (state === "ready" && workKind === "missing_system" && (admissions?.length ?? 0) === 0) {
    schemaFail(ctx, "E-SCHEMA-STATE", path, "ready missing-system work requires an admission ID");
  }

  const base = {
    kind: "work" as const,
    ...common(ctx, table, path),
    work_state: state,
    work_intent: expectEnum(ctx, requiredValue(table, "work_intent"), WORK_INTENTS, p(path, "work_intent")),
    work_kind: workKind,
    risk: expectEnum(ctx, requiredValue(table, "risk"), RISKS, p(path, "risk")),
    ...optionalRoadmapSet(ctx, table, path, "evidence_ids"),
    ...(state === "armed" ? {} : optionalRoadmapSet(ctx, table, path, "control_ids")),
    ...(regressionEvidence === undefined ? {} : { regression_evidence_ids: regressionEvidence }),
    ...(regressionGaps === undefined ? {} : { regression_gap_ids: regressionGaps }),
    ...(admissions === undefined ? {} : { admission_ids: admissions }),
  };
  switch (state) {
    case "ready":
      return {
        ...base,
        work_state: state,
        acceptance_md: expectNonemptyMarkdown(ctx, table, path, "acceptance_md"),
        priority_rationale_md: expectNonemptyMarkdown(ctx, table, path, "priority_rationale_md"),
        ...optionalPriority(ctx, table, path),
      };
    case "blocked":
      return { ...base, ...optionalMarkdown(ctx, table, path, "acceptance_md"), work_state: state, blocker_md: expectMarkdown(ctx, requiredValue(table, "blocker_md"), p(path, "blocker_md")), transition_ids: expectRoadmapIdSet(ctx, requiredValue(table, "transition_ids"), p(path, "transition_ids"), true) };
    case "armed":
      return { ...base, ...optionalMarkdown(ctx, table, path, "acceptance_md"), work_state: state, control_ids: expectRoadmapIdSet(ctx, requiredValue(table, "control_ids"), p(path, "control_ids"), true), transition_ids: expectRoadmapIdSet(ctx, requiredValue(table, "transition_ids"), p(path, "transition_ids"), true) };
    case "deferred":
      return { ...base, ...optionalMarkdown(ctx, table, path, "acceptance_md"), work_state: state, transition_ids: expectRoadmapIdSet(ctx, requiredValue(table, "transition_ids"), p(path, "transition_ids"), true) };
    case "waiting_external":
      return { ...base, ...optionalMarkdown(ctx, table, path, "acceptance_md"), work_state: state, transition_ids: expectRoadmapIdSet(ctx, requiredValue(table, "transition_ids"), p(path, "transition_ids"), true), external_owner_reference_id: expectReferenceId(ctx, requiredValue(table, "external_owner_reference_id"), p(path, "external_owner_reference_id")) };
    case "delegated":
      return { ...base, ...optionalMarkdown(ctx, table, path, "acceptance_md"), work_state: state, return_condition_md: expectMarkdown(ctx, requiredValue(table, "return_condition_md"), p(path, "return_condition_md")) };
    case "pending_review":
      return { ...base, work_state: state, uncertainty_md: expectMarkdown(ctx, requiredValue(table, "uncertainty_md"), p(path, "uncertainty_md")) };
  }
}

function optionalPriority(ctx: DecodeContext, table: object, path: string): { priority_band?: (typeof PRIORITIES)[number] } {
  const value = optionalDecoded(table, "priority_band", path, (entry, fieldPath) => expectEnum(ctx, entry, PRIORITIES, fieldPath));
  return value === undefined ? {} : { priority_band: value };
}

function decodeDecision(ctx: DecodeContext, raw: unknown, path: string): DecisionPayload {
  const discriminator = expectExactTable(ctx, raw, path, { name: "decision discriminator", required: ["kind", "decision_state"], optional: ["detail_md", "question_md", "rationale_md", "authority_reference_id", "permanence", "transition_ids"] });
  const state = expectEnum(ctx, requiredValue(discriminator, "decision_state"), ["pending", "held", "decided"] as const, p(path, "decision_state"));
  if (state === "pending") {
    const table = expectExactTable(ctx, raw, path, SHARED_SEMANTIC_SCHEMA_ROWS[7]);
    return { kind: "decision", ...common(ctx, table, path), decision_state: state, question_md: expectMarkdown(ctx, requiredValue(table, "question_md"), p(path, "question_md")), transition_ids: expectRoadmapIdSet(ctx, requiredValue(table, "transition_ids"), p(path, "transition_ids"), true) };
  }
  if (state === "held") {
    const table = expectExactTable(ctx, raw, path, SHARED_SEMANTIC_SCHEMA_ROWS[8]);
    const permanence = expectEnum(ctx, requiredValue(table, "permanence"), ["reopenable"] as const, p(path, "permanence"));
    return { kind: "decision", ...common(ctx, table, path), decision_state: state, rationale_md: expectMarkdown(ctx, requiredValue(table, "rationale_md"), p(path, "rationale_md")), permanence, transition_ids: expectRoadmapIdSet(ctx, requiredValue(table, "transition_ids"), p(path, "transition_ids"), true) };
  }
  const pre = expectExactTable(ctx, raw, path, { name: "decided decision discriminator", required: ["kind", "decision_state", "permanence"], optional: ["detail_md", "question_md", "rationale_md", "authority_reference_id", "transition_ids"] });
  const permanence = expectEnum(ctx, requiredValue(pre, "permanence"), ["permanent", "reopenable"] as const, p(path, "permanence"));
  const table = expectExactTable(ctx, raw, path, SHARED_SEMANTIC_SCHEMA_ROWS[permanence === "permanent" ? 9 : 10]);
  return {
    kind: "decision",
    ...common(ctx, table, path),
    decision_state: state,
    rationale_md: expectMarkdown(ctx, requiredValue(table, "rationale_md"), p(path, "rationale_md")),
    authority_reference_id: expectReferenceId(ctx, requiredValue(table, "authority_reference_id"), p(path, "authority_reference_id")),
    permanence,
    ...(permanence === "reopenable" ? { transition_ids: expectRoadmapIdSet(ctx, requiredValue(table, "transition_ids"), p(path, "transition_ids"), true) } : {}),
  };
}

function decodePredicate(ctx: DecodeContext, raw: unknown, path: string): SignalPredicate {
  const pre = expectExactTable(ctx, raw, path, { name: "signal predicate discriminator", required: ["predicate_kind"], optional: ["comparator", "threshold", "unit", "scope", "measurement", "as_of", "event_md", "evidence_ids", "review_procedure_md"] });
  const kind = expectEnum(ctx, requiredValue(pre, "predicate_kind"), ["quantitative", "event", "manual"] as const, p(path, "predicate_kind"));
  if (kind === "quantitative") {
    const table = expectExactTable(ctx, raw, path, SHARED_SEMANTIC_SCHEMA_ROWS[16]);
    return { predicate_kind: kind, comparator: expectEnum(ctx, requiredValue(table, "comparator"), COMPARATORS, p(path, "comparator")), threshold: expectFiniteNumber(ctx, requiredValue(table, "threshold"), p(path, "threshold")), unit: expectString(ctx, requiredValue(table, "unit"), p(path, "unit")), scope: expectString(ctx, requiredValue(table, "scope"), p(path, "scope")), measurement: expectFiniteNumber(ctx, requiredValue(table, "measurement"), p(path, "measurement")), as_of: expectCivilDate(ctx, requiredValue(table, "as_of"), p(path, "as_of")), ...optionalRoadmapSet(ctx, table, path, "evidence_ids") };
  }
  if (kind === "event") {
    const table = expectExactTable(ctx, raw, path, SHARED_SEMANTIC_SCHEMA_ROWS[17]);
    return { predicate_kind: kind, event_md: expectMarkdown(ctx, requiredValue(table, "event_md"), p(path, "event_md")), evidence_ids: expectRoadmapIdSet(ctx, requiredValue(table, "evidence_ids"), p(path, "evidence_ids"), true) };
  }
  const table = expectExactTable(ctx, raw, path, SHARED_SEMANTIC_SCHEMA_ROWS[18]);
  return { predicate_kind: kind, review_procedure_md: expectMarkdown(ctx, requiredValue(table, "review_procedure_md"), p(path, "review_procedure_md")), evidence_ids: expectRoadmapIdSet(ctx, requiredValue(table, "evidence_ids"), p(path, "evidence_ids"), true) };
}

function decodeSignal(ctx: DecodeContext, raw: unknown, path: string): SignalPayload {
  const pre = expectExactTable(ctx, raw, path, { name: "signal discriminator", required: ["kind", "transition_kind"], optional: ["detail_md", "observer", "dimension", "observable", "action_on_fire_md", "evaluation", "predicate", "owner_reference_id", "event_md", "check_procedure_md", "due_action_md", "failure_signature_md", "capture_procedure_md", "response_md", "escalation_action_md", "retirement_semantics_md", "external_owner_reference_id", "external_predicate_md", "verification_md", "event_source", "period_or_event_md", "checklist_md", "missed_action_md", "last_completion_reference_id", "due_on", "as_of"] });
  const kind = expectEnum(ctx, requiredValue(pre, "transition_kind"), ["promotion_trigger", "reopening_signal", "unblock_predicate", "watch_escalation", "retirement_predicate", "cadence"] as const, p(path, "transition_kind"));
  if (kind === "promotion_trigger" || kind === "reopening_signal") {
    // The trigger condition's home is arm-dependent: an event predicate carries it as
    // predicate.event_md (signal-level observable is forbidden), while manual and quantitative
    // predicates have no nested condition field, so observable is required exactly there.
    const predicate = decodePredicate(
      ctx,
      requiredValue(
        expectExactTable(ctx, raw, path, { name: "signal predicate presence", required: ["kind", "transition_kind", "predicate"], optional: ["detail_md", "observer", "dimension", "observable", "action_on_fire_md", "evaluation"] }),
        "predicate",
      ),
      p(path, "predicate"),
    );
    const table = expectExactTable(
      ctx,
      raw,
      path,
      predicate.predicate_kind === "event" ? SHARED_SEMANTIC_SCHEMA_ROWS[11] : SHARED_SEMANTIC_SCHEMA_ROWS[22],
    );
    return { kind: "signal", ...common(ctx, table, path), transition_kind: kind, observer: expectString(ctx, requiredValue(table, "observer"), p(path, "observer")), dimension: expectString(ctx, requiredValue(table, "dimension"), p(path, "dimension")), ...(predicate.predicate_kind === "event" ? {} : { observable: expectString(ctx, requiredValue(table, "observable"), p(path, "observable")) }), action_on_fire_md: expectMarkdown(ctx, requiredValue(table, "action_on_fire_md"), p(path, "action_on_fire_md")), evaluation: expectEnum(ctx, requiredValue(table, "evaluation"), EVALUATIONS, p(path, "evaluation")), predicate };
  }
  const schemaIndex = { unblock_predicate: 12, watch_escalation: 13, retirement_predicate: 14, cadence: 15 }[kind];
  const table = expectExactTable(ctx, raw, path, SHARED_SEMANTIC_SCHEMA_ROWS[schemaIndex]);
  const shared = { kind: "signal" as const, ...common(ctx, table, path), transition_kind: kind, evaluation: expectEnum(ctx, requiredValue(table, "evaluation"), EVALUATIONS, p(path, "evaluation")) };
  if (kind === "unblock_predicate") return { ...shared, transition_kind: kind, owner_reference_id: expectReferenceId(ctx, requiredValue(table, "owner_reference_id"), p(path, "owner_reference_id")), event_md: expectMarkdown(ctx, requiredValue(table, "event_md"), p(path, "event_md")), check_procedure_md: expectMarkdown(ctx, requiredValue(table, "check_procedure_md"), p(path, "check_procedure_md")), due_action_md: expectMarkdown(ctx, requiredValue(table, "due_action_md"), p(path, "due_action_md")) };
  if (kind === "watch_escalation") return { ...shared, transition_kind: kind, failure_signature_md: expectMarkdown(ctx, requiredValue(table, "failure_signature_md"), p(path, "failure_signature_md")), capture_procedure_md: expectMarkdown(ctx, requiredValue(table, "capture_procedure_md"), p(path, "capture_procedure_md")), response_md: expectMarkdown(ctx, requiredValue(table, "response_md"), p(path, "response_md")), escalation_action_md: expectMarkdown(ctx, requiredValue(table, "escalation_action_md"), p(path, "escalation_action_md")), retirement_semantics_md: expectMarkdown(ctx, requiredValue(table, "retirement_semantics_md"), p(path, "retirement_semantics_md")) };
  if (kind === "retirement_predicate") return { ...shared, transition_kind: kind, external_owner_reference_id: expectReferenceId(ctx, requiredValue(table, "external_owner_reference_id"), p(path, "external_owner_reference_id")), external_predicate_md: expectMarkdown(ctx, requiredValue(table, "external_predicate_md"), p(path, "external_predicate_md")), verification_md: expectMarkdown(ctx, requiredValue(table, "verification_md"), p(path, "verification_md")), due_action_md: expectMarkdown(ctx, requiredValue(table, "due_action_md"), p(path, "due_action_md")) };
  return { ...shared, transition_kind: kind, owner_reference_id: expectReferenceId(ctx, requiredValue(table, "owner_reference_id"), p(path, "owner_reference_id")), event_source: expectString(ctx, requiredValue(table, "event_source"), p(path, "event_source")), period_or_event_md: expectMarkdown(ctx, requiredValue(table, "period_or_event_md"), p(path, "period_or_event_md")), checklist_md: expectMarkdown(ctx, requiredValue(table, "checklist_md"), p(path, "checklist_md")), missed_action_md: expectMarkdown(ctx, requiredValue(table, "missed_action_md"), p(path, "missed_action_md")), ...optionalReference(ctx, table, path, "last_completion_reference_id"), ...optionalDate(ctx, table, path, "due_on"), ...optionalDate(ctx, table, path, "as_of") };
}

function optionalReference(ctx: DecodeContext, table: object, path: string, key: string): object {
  const value = optionalDecoded(table, key, path, (entry, fieldPath) => expectReferenceId(ctx, entry, fieldPath));
  return value === undefined ? {} : { [key]: value };
}

function optionalDate(ctx: DecodeContext, table: object, path: string, key: string): object {
  const value = optionalDecoded(table, key, path, (entry, fieldPath) => expectCivilDate(ctx, entry, fieldPath));
  return value === undefined ? {} : { [key]: value };
}

function optionalString(ctx: DecodeContext, table: object, path: string, key: string): object {
  const value = optionalDecoded(table, key, path, (entry, fieldPath) => expectString(ctx, entry, fieldPath));
  return value === undefined ? {} : { [key]: value };
}

function decodeEvidenceScope(ctx: DecodeContext, raw: unknown, path: string): EvidenceScope {
  const table = expectExactTable(ctx, raw, path, SHARED_SEMANTIC_SCHEMA_ROWS[20]);
  if (Object.keys(table).length === 0) schemaFail(ctx, "E-SCHEMA-FLOOR", path, "evidence scope requires at least one coordinate set");
  return {
    ...optionalStringSet(ctx, table, path, "surfaces"),
    ...optionalStringSet(ctx, table, path, "faces"),
    ...optionalStringSet(ctx, table, path, "profiles"),
    ...optionalStringSet(ctx, table, path, "flags"),
    ...optionalStringSet(ctx, table, path, "input_modes"),
    ...optionalStringSet(ctx, table, path, "toolchains"),
    ...optionalStringSet(ctx, table, path, "executors"),
    ...optionalStringSet(ctx, table, path, "tiers"),
  };
}

function optionalStringSet(ctx: DecodeContext, table: object, path: string, key: string): object {
  if (!hasOwn(table, key)) return {};
  return { [key]: expectStringSet(ctx, optionalValue(table, key), p(path, key), true) };
}

function decodeEvidence(ctx: DecodeContext, raw: unknown, path: string): EvidencePayload {
  const table = expectExactTable(ctx, raw, path, SHARED_SEMANTIC_SCHEMA_ROWS[19]);
  const kind = expectEnum(ctx, requiredValue(table, "evidence_kind"), EVIDENCE_KINDS, p(path, "evidence_kind"));
  const freshness = expectEnum(ctx, requiredValue(table, "freshness"), FRESHNESS, p(path, "freshness"));
  const hasPoint = hasOwn(table, "at_commit") || hasOwn(table, "environment_md");
  if (freshness === "live") {
    if (!hasOwn(table, "refresh_reference_id") || hasOwn(table, "observed_at") || hasPoint || hasOwn(table, "valid_through")) schemaFail(ctx, "E-SCHEMA-STATE", path, "live evidence requires refresh and forbids point provenance/validity");
  } else {
    if (!hasOwn(table, "observed_at") || !hasPoint || hasOwn(table, "refresh_reference_id")) schemaFail(ctx, "E-SCHEMA-STATE", path, `${freshness} evidence requires observed_at plus point provenance and forbids refresh`);
    if (freshness !== "as_of" && hasOwn(table, "valid_through")) schemaFail(ctx, "E-SCHEMA-FORBIDDEN-KEY", p(path, "valid_through"), "valid_through is permitted only for as_of evidence");
  }
  if (kind === "registry_enumeration" && !hasOwn(table, "enumerated_registry")) schemaFail(ctx, "E-SCHEMA-STATE", p(path, "enumerated_registry"), "registry enumeration evidence requires enumerated_registry");
  if (kind !== "registry_enumeration" && hasOwn(table, "enumerated_registry")) schemaFail(ctx, "E-SCHEMA-FORBIDDEN-KEY", p(path, "enumerated_registry"), "enumerated_registry belongs only to registry_enumeration evidence");
  const commandAllowed = ["harness_free_repro", "execution_probe", "registry_enumeration", "source_read"].includes(kind);
  if (!commandAllowed && (hasOwn(table, "command_md") || hasOwn(table, "result_md"))) schemaFail(ctx, "E-SCHEMA-FORBIDDEN-KEY", path, `${kind} evidence forbids command/result fields`);
  if (["harness_free_repro", "execution_probe"].includes(kind) && (!hasOwn(table, "command_md") || !hasOwn(table, "result_md") || !hasPoint)) schemaFail(ctx, "E-SCHEMA-STATE", path, `${kind} evidence requires command, result, and point provenance`);
  const observed = optionalDecoded(table, "observed_at", path, (value, fieldPath) => expectCivilDate(ctx, value, fieldPath));
  const validThrough = optionalDecoded(table, "valid_through", path, (value, fieldPath) => expectCivilDate(ctx, value, fieldPath));
  if (observed !== undefined && validThrough !== undefined && validThrough < observed) schemaFail(ctx, "E-SCHEMA-STATE", p(path, "valid_through"), "valid_through must not precede observed_at");
  return {
    kind: "evidence",
    ...common(ctx, table, path),
    evidence_kind: kind,
    claim_md: expectMarkdown(ctx, requiredValue(table, "claim_md"), p(path, "claim_md")),
    evidence_verdict: expectEnum(ctx, requiredValue(table, "evidence_verdict"), EVIDENCE_VERDICTS, p(path, "evidence_verdict")),
    freshness,
    reference_ids: expectReferenceIdSet(ctx, requiredValue(table, "reference_ids"), p(path, "reference_ids"), true),
    ...optionalMarkdown(ctx, table, path, "command_md"),
    ...optionalMarkdown(ctx, table, path, "result_md"),
    ...(hasOwn(table, "at_commit")
      ? { at_commit: expectFullCommitId(ctx, optionalValue(table, "at_commit"), p(path, "at_commit")) }
      : {}),
    ...(observed === undefined ? {} : { observed_at: observed }),
    ...(validThrough === undefined ? {} : { valid_through: validThrough }),
    ...optionalMarkdown(ctx, table, path, "environment_md"),
    unprobed_remainder_md: expectMarkdown(ctx, requiredValue(table, "unprobed_remainder_md"), p(path, "unprobed_remainder_md")),
    ...optionalReference(ctx, table, path, "refresh_reference_id"),
    ...optionalString(ctx, table, path, "enumerated_registry"),
    scope: decodeEvidenceScope(ctx, requiredValue(table, "scope"), p(path, "scope")),
  };
}

function decodeControl(ctx: DecodeContext, raw: unknown, path: string): ControlPayload {
  const table = expectExactTable(ctx, raw, path, SHARED_SEMANTIC_SCHEMA_ROWS[21]);
  return { kind: "control", ...common(ctx, table, path), control_kind: expectEnum(ctx, requiredValue(table, "control_kind"), CONTROL_KINDS, p(path, "control_kind")), control_state: expectEnum(ctx, requiredValue(table, "control_state"), ["live", "proposed", "stale"] as const, p(path, "control_state")), reference_ids: expectReferenceIdSet(ctx, requiredValue(table, "reference_ids"), p(path, "reference_ids"), true), claim_md: expectMarkdown(ctx, requiredValue(table, "claim_md"), p(path, "claim_md")), boundary_md: expectMarkdown(ctx, requiredValue(table, "boundary_md"), p(path, "boundary_md")) };
}

export function decodeSharedSemanticPayload(
  ctx: DecodeContext,
  raw: unknown,
  path: string,
): SharedSemanticPayload | undefined {
  const pre = expectExactTable(ctx, raw, path, { name: "semantic payload discriminator", required: ["kind"], optional: Object.keys(raw === null || typeof raw !== "object" || Array.isArray(raw) ? {} : raw).filter((key) => key !== "kind") });
  const kind = expectString(ctx, requiredValue(pre, "kind"), p(path, "kind"));
  switch (kind) {
    case "work": return decodeWork(ctx, raw, path);
    case "decision": return decodeDecision(ctx, raw, path);
    case "signal": return decodeSignal(ctx, raw, path);
    case "evidence": return decodeEvidence(ctx, raw, path);
    case "control": return decodeControl(ctx, raw, path);
    default: return undefined;
  }
}

export function assertSharedPayload(payload: SharedSemanticPayload): SemanticPayload {
  return payload;
}
