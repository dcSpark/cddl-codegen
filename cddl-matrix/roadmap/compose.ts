import type {
  Fragment,
  GeneratedSlot,
  ManifestEntry,
  Part,
  RecordNode,
  Reference,
  Relation,
  RoadmapDocument,
  Section,
  SemanticPayload,
} from "./model/documents.ts";
import type {
  ControlPayload,
  DecisionPayload,
  EvidencePayload,
  SemanticPayloadBase,
  SignalPayload,
  WorkPayload,
} from "./model/core.ts";
import type { MatrixExternalCloseoutPayload, MatrixPolicyPayload } from "./model/matrix.ts";
import type {
  TestingCostPayload,
  TestingIncidentPayload,
  TestingOperationalWatchPayload,
  TestingSystemAdmissionPayload,
} from "./model/testing.ts";
import { CanonicalTomlWriter } from "./toml_writer.ts";

const compare = (left: string, right: string): number => (left < right ? -1 : left > right ? 1 : 0);
const sorted = <T>(items: readonly T[], key: (item: T) => string): T[] =>
  [...items].sort((left, right) => compare(key(left), key(right)));

function optionalString(writer: CanonicalTomlWriter, key: string, value: string | undefined): void {
  if (value !== undefined) writer.string(key, value);
}

function optionalMarkdown(writer: CanonicalTomlWriter, key: string, value: Uint8Array | undefined): void {
  if (value !== undefined) writer.markdown(key, value);
}

function optionalStrings(
  writer: CanonicalTomlWriter,
  key: string,
  value: readonly string[] | undefined,
  sort = true,
): void {
  if (value !== undefined) writer.strings(key, value, sort);
}

function semanticCommon(writer: CanonicalTomlWriter, payload: SemanticPayloadBase): void {
  writer.string("kind", payload.kind);
  optionalMarkdown(writer, "detail_md", payload.detail_md);
}

function writeWork(writer: CanonicalTomlWriter, payload: WorkPayload): void {
  semanticCommon(writer, payload);
  writer.string("work_state", payload.work_state);
  writer.string("work_intent", payload.work_intent);
  writer.string("work_kind", payload.work_kind);
  writer.string("risk", payload.risk);
  optionalStrings(writer, "evidence_ids", payload.evidence_ids);
  switch (payload.work_state) {
    case "ready":
      writer.markdown("acceptance_md", payload.acceptance_md);
      if (payload.priority_band !== undefined) writer.string("priority_band", payload.priority_band);
      writer.markdown("priority_rationale_md", payload.priority_rationale_md);
      optionalStrings(writer, "control_ids", payload.control_ids);
      break;
    case "blocked":
      optionalMarkdown(writer, "acceptance_md", payload.acceptance_md);
      writer.markdown("blocker_md", payload.blocker_md);
      optionalStrings(writer, "control_ids", payload.control_ids);
      writer.strings("transition_ids", payload.transition_ids, true);
      break;
    case "armed":
      optionalMarkdown(writer, "acceptance_md", payload.acceptance_md);
      writer.strings("control_ids", payload.control_ids, true);
      writer.strings("transition_ids", payload.transition_ids, true);
      break;
    case "deferred":
      optionalMarkdown(writer, "acceptance_md", payload.acceptance_md);
      optionalStrings(writer, "control_ids", payload.control_ids);
      writer.strings("transition_ids", payload.transition_ids, true);
      break;
    case "waiting_external":
      optionalMarkdown(writer, "acceptance_md", payload.acceptance_md);
      optionalStrings(writer, "control_ids", payload.control_ids);
      writer.strings("transition_ids", payload.transition_ids, true);
      writer.string("external_owner_reference_id", payload.external_owner_reference_id);
      break;
    case "delegated":
      optionalMarkdown(writer, "acceptance_md", payload.acceptance_md);
      optionalStrings(writer, "control_ids", payload.control_ids);
      writer.markdown("return_condition_md", payload.return_condition_md);
      break;
    case "pending_review":
      optionalStrings(writer, "control_ids", payload.control_ids);
      writer.markdown("uncertainty_md", payload.uncertainty_md);
      break;
  }
  optionalStrings(writer, "regression_evidence_ids", payload.regression_evidence_ids);
  optionalStrings(writer, "regression_gap_ids", payload.regression_gap_ids);
  optionalStrings(writer, "admission_ids", payload.admission_ids);
}

function writeDecision(writer: CanonicalTomlWriter, payload: DecisionPayload): void {
  semanticCommon(writer, payload);
  writer.string("decision_state", payload.decision_state);
  if (payload.decision_state === "pending") {
    writer.markdown("question_md", payload.question_md);
    writer.strings("transition_ids", payload.transition_ids, true);
    return;
  }
  writer.markdown("rationale_md", payload.rationale_md);
  if (payload.decision_state === "decided") writer.string("authority_reference_id", payload.authority_reference_id);
  writer.string("permanence", payload.permanence);
  if (payload.transition_ids !== undefined) writer.strings("transition_ids", payload.transition_ids, true);
}

function writeSignal(writer: CanonicalTomlWriter, payload: SignalPayload, prefix: string): void {
  semanticCommon(writer, payload);
  writer.string("transition_kind", payload.transition_kind);
  if (payload.transition_kind === "promotion_trigger" || payload.transition_kind === "reopening_signal") {
    writer.string("observer", payload.observer);
    writer.string("dimension", payload.dimension);
    if (payload.predicate.predicate_kind !== "event") writer.string("observable", payload.observable ?? "");
    writer.markdown("action_on_fire_md", payload.action_on_fire_md);
    writer.string("evaluation", payload.evaluation);
    writer.table(`${prefix}.predicate`);
    const predicate = payload.predicate;
    writer.string("predicate_kind", predicate.predicate_kind);
    if (predicate.predicate_kind === "quantitative") {
      writer.string("comparator", predicate.comparator);
      writer.number("threshold", predicate.threshold);
      writer.string("unit", predicate.unit);
      writer.string("scope", predicate.scope);
      writer.number("measurement", predicate.measurement);
      writer.string("as_of", predicate.as_of);
      optionalStrings(writer, "evidence_ids", predicate.evidence_ids);
    } else if (predicate.predicate_kind === "event") {
      writer.markdown("event_md", predicate.event_md);
      writer.strings("evidence_ids", predicate.evidence_ids, true);
    } else {
      writer.markdown("review_procedure_md", predicate.review_procedure_md);
      writer.strings("evidence_ids", predicate.evidence_ids, true);
    }
    return;
  }
  if (payload.transition_kind === "unblock_predicate") {
    writer.string("owner_reference_id", payload.owner_reference_id);
    writer.markdown("event_md", payload.event_md);
    writer.markdown("check_procedure_md", payload.check_procedure_md);
    writer.markdown("due_action_md", payload.due_action_md);
  } else if (payload.transition_kind === "watch_escalation") {
    writer.markdown("failure_signature_md", payload.failure_signature_md);
    writer.markdown("capture_procedure_md", payload.capture_procedure_md);
    writer.markdown("response_md", payload.response_md);
    writer.markdown("escalation_action_md", payload.escalation_action_md);
    writer.markdown("retirement_semantics_md", payload.retirement_semantics_md);
  } else if (payload.transition_kind === "retirement_predicate") {
    writer.string("external_owner_reference_id", payload.external_owner_reference_id);
    writer.markdown("external_predicate_md", payload.external_predicate_md);
    writer.markdown("verification_md", payload.verification_md);
    writer.markdown("due_action_md", payload.due_action_md);
  } else {
    writer.string("owner_reference_id", payload.owner_reference_id);
    writer.string("event_source", payload.event_source);
    writer.markdown("period_or_event_md", payload.period_or_event_md);
    writer.markdown("checklist_md", payload.checklist_md);
    writer.markdown("missed_action_md", payload.missed_action_md);
    optionalString(writer, "last_completion_reference_id", payload.last_completion_reference_id);
    optionalString(writer, "due_on", payload.due_on);
    optionalString(writer, "as_of", payload.as_of);
  }
  writer.string("evaluation", payload.evaluation);
}

function writeEvidence(writer: CanonicalTomlWriter, payload: EvidencePayload, prefix: string): void {
  semanticCommon(writer, payload);
  writer.string("evidence_kind", payload.evidence_kind);
  writer.markdown("claim_md", payload.claim_md);
  writer.string("evidence_verdict", payload.evidence_verdict);
  writer.string("freshness", payload.freshness);
  writer.strings("reference_ids", payload.reference_ids, true);
  optionalMarkdown(writer, "command_md", payload.command_md);
  optionalMarkdown(writer, "result_md", payload.result_md);
  optionalString(writer, "at_commit", payload.at_commit);
  optionalString(writer, "observed_at", payload.observed_at);
  optionalString(writer, "valid_through", payload.valid_through);
  optionalMarkdown(writer, "environment_md", payload.environment_md);
  writer.markdown("unprobed_remainder_md", payload.unprobed_remainder_md);
  optionalString(writer, "refresh_reference_id", payload.refresh_reference_id);
  optionalString(writer, "enumerated_registry", payload.enumerated_registry);
  writer.table(`${prefix}.scope`);
  optionalStrings(writer, "surfaces", payload.scope.surfaces);
  optionalStrings(writer, "faces", payload.scope.faces);
  optionalStrings(writer, "profiles", payload.scope.profiles);
  optionalStrings(writer, "flags", payload.scope.flags);
  optionalStrings(writer, "input_modes", payload.scope.input_modes);
  optionalStrings(writer, "toolchains", payload.scope.toolchains);
  optionalStrings(writer, "executors", payload.scope.executors);
  optionalStrings(writer, "tiers", payload.scope.tiers);
}

function writeControl(writer: CanonicalTomlWriter, payload: ControlPayload): void {
  semanticCommon(writer, payload);
  writer.string("control_kind", payload.control_kind);
  writer.string("control_state", payload.control_state);
  writer.strings("reference_ids", payload.reference_ids, true);
  writer.markdown("claim_md", payload.claim_md);
  writer.markdown("boundary_md", payload.boundary_md);
}

function writeMatrixCloseout(
  writer: CanonicalTomlWriter,
  payload: MatrixExternalCloseoutPayload,
  prefix: string,
): void {
  semanticCommon(writer, payload);
  writer.string("closeout_state", payload.closeout_state);
  writer.string("upstream_owner_reference_id", payload.upstream_owner_reference_id);
  writer.markdown("current_upstream_state_md", payload.current_upstream_state_md);
  if (payload.closeout_state === "blocked") writer.markdown("blocker_md", payload.blocker_md);
  writer.strings("transition_ids", payload.transition_ids, true);
  writer.markdown("verification_md", payload.verification_md);
  optionalStrings(writer, "prune_reference_ids", payload.prune_reference_ids);
  for (const action of payload.actions) {
    writer.arrayTable(`${prefix}.action`);
    writer.string("action_id", action.action_id);
    writer.markdown("action_md", action.action_md);
  }
  for (const branch of payload.branches) {
    writer.arrayTable(`${prefix}.branch`);
    writer.string("branch_id", branch.branch_id);
    writer.markdown("predicate_md", branch.predicate_md);
    optionalStrings(writer, "prune_reference_ids", branch.prune_reference_ids);
    for (const actionId of branch.action_ids) {
      writer.arrayTable(`${prefix}.branch.action`);
      writer.string("action_id", actionId);
    }
  }
}

function writeMatrixPolicy(writer: CanonicalTomlWriter, payload: MatrixPolicyPayload): void {
  semanticCommon(writer, payload);
  writer.string("policy_kind", payload.policy_kind);
  writer.string("authority_reference_id", payload.authority_reference_id);
  if (payload.policy_kind === "maintenance_protocol") {
    writer.markdown("protocol_md", payload.protocol_md);
    writer.string("cadence_transition_id", payload.cadence_transition_id);
    return;
  }
  writer.markdown("rationale_md", payload.rationale_md);
  writer.string("permanence", payload.permanence);
  optionalString(writer, "reopening_transition_id", payload.reopening_transition_id);
}

function writeTestingWatch(
  writer: CanonicalTomlWriter,
  payload: TestingOperationalWatchPayload,
  prefix: string,
): void {
  semanticCommon(writer, payload);
  writer.string("watch_state", payload.watch_state);
  writer.markdown("signature_md", payload.signature_md);
  if (payload.watch_state !== "watching") {
    writer.markdown("attribution_md", payload.attribution_md);
    writer.string("operating_rule_reference_id", payload.operating_rule_reference_id);
  }
  writer.markdown("response_md", payload.response_md);
  writer.string("escalation_transition_id", payload.escalation_transition_id);
  if (payload.watch_state === "retire_pending") {
    writer.string("retirement_reference_id", payload.retirement_reference_id);
  }
  writer.markdown("retirement_semantics_md", payload.retirement_semantics_md);
  for (const step of payload.capture_steps) {
    writer.arrayTable(`${prefix}.capture_step`);
    writer.string("step_id", step.step_id);
    writer.markdown("capture_md", step.capture_md);
  }
}

function writeTestingIncident(writer: CanonicalTomlWriter, payload: TestingIncidentPayload): void {
  semanticCommon(writer, payload);
  writer.string("incident_posture", payload.incident_posture);
  writer.markdown("signature_md", payload.signature_md);
  writer.strings("evidence_ids", payload.evidence_ids, true);
  if (payload.incident_posture !== "live") {
    writer.markdown("attribution_md", payload.attribution_md);
    writer.string("operating_rule_reference_id", payload.operating_rule_reference_id);
  }
  if (payload.incident_posture === "historical") {
    writer.string("retirement_reference_id", payload.retirement_reference_id);
  }
}

function writeTestingCost(writer: CanonicalTomlWriter, payload: TestingCostPayload): void {
  semanticCommon(writer, payload);
  writer.string("cost_posture", payload.cost_posture);
  writer.string("unit", payload.unit);
  writer.markdown("scope_md", payload.scope_md);
  if (payload.cost_posture === "live_registry") {
    writer.string("gate_reference_id", payload.gate_reference_id);
    return;
  }
  writer.number("value_min", payload.value_min);
  writer.number("value_max", payload.value_max);
  writer.string("observed_at", payload.observed_at);
  writer.markdown("environment_md", payload.environment_md);
  writer.strings("evidence_ids", payload.evidence_ids, true);
}

function writeTestingAdmission(writer: CanonicalTomlWriter, payload: TestingSystemAdmissionPayload): void {
  semanticCommon(writer, payload);
  writer.string("admission_kind", payload.admission_kind);
  writer.markdown("claim_md", payload.claim_md);
  writer.strings("evidence_ids", payload.evidence_ids, true);
  if (payload.admission_kind === "independent_recurrence") {
    writer.strings("incident_ids", payload.incident_ids, true);
  }
}

function writeSemanticPayload(
  writer: CanonicalTomlWriter,
  payload: SemanticPayload,
  prefix: string,
): void {
  switch (payload.kind) {
    case "work": writeWork(writer, payload); break;
    case "decision": writeDecision(writer, payload); break;
    case "signal": writeSignal(writer, payload, prefix); break;
    case "evidence": writeEvidence(writer, payload, prefix); break;
    case "control": writeControl(writer, payload); break;
    case "matrix_external_closeout": writeMatrixCloseout(writer, payload, prefix); break;
    case "matrix_policy": writeMatrixPolicy(writer, payload); break;
    case "testing_operational_watch": writeTestingWatch(writer, payload, prefix); break;
    case "testing_incident": writeTestingIncident(writer, payload); break;
    case "testing_cost": writeTestingCost(writer, payload); break;
    case "testing_system_admission": writeTestingAdmission(writer, payload); break;
  }
}

function writeGeneratedSlot(writer: CanonicalTomlWriter, slot: GeneratedSlot): void {
  writer.arrayTable("generated_slot");
  writer.string("slot_id", slot.slot_id);
  writer.string("binding", slot.binding);
}

function writeManifestEntry(writer: CanonicalTomlWriter, entry: ManifestEntry): void {
  writer.arrayTable("manifest.entry");
  writer.string("kind", entry.kind);
  switch (entry.kind) {
    case "section": writer.string("section_id", entry.section_id); break;
    case "fragment": writer.string("fragment_id", entry.fragment_id); break;
    case "record": writer.string("record_id", entry.record_id); break;
    case "part": writer.string("part_id", entry.part_id); break;
    case "generated_slot": writer.string("slot_id", entry.slot_id); break;
  }
}

function referenceTuple(reference: Reference): string {
  switch (reference.kind) {
    case "roadmap": return reference.target_id;
    case "matrix_feature": return reference.feature_id;
    case "matrix_role": return reference.role_id;
    case "matrix_cell": return reference.cell_id;
    case "gate": return reference.gate_id;
    case "test_symbol": return `${reference.test_id}\0${reference.symbol}`;
    case "file_heading": return `${reference.path}\0${reference.heading}`;
    case "spec_passage": return `${reference.document}\0${reference.passage}`;
    case "external_issue": return `${reference.repository}\0${reference.issue}`;
    case "external_commit": return `${reference.repository}\0${reference.commit}`;
    case "external_release": return `${reference.project}\0${reference.release}`;
    case "consumer_report": return `${reference.consumer}\0${reference.report_reference}`;
  }
}

function writeReference(writer: CanonicalTomlWriter, reference: Reference): void {
  writer.arrayTable("reference");
  writer.string("id", reference.id);
  writer.string("source", reference.source);
  writer.string("kind", reference.kind);
  switch (reference.kind) {
    case "roadmap": writer.string("target_id", reference.target_id); break;
    case "matrix_feature": writer.string("feature_id", reference.feature_id); break;
    case "matrix_role": writer.string("role_id", reference.role_id); break;
    case "matrix_cell": writer.string("cell_id", reference.cell_id); break;
    case "gate": writer.string("gate_id", reference.gate_id); break;
    case "test_symbol": writer.string("test_id", reference.test_id); writer.string("symbol", reference.symbol); break;
    case "file_heading": writer.string("path", reference.path); writer.string("heading", reference.heading); break;
    case "spec_passage": writer.string("document", reference.document); writer.string("passage", reference.passage); break;
    case "external_issue": writer.string("repository", reference.repository); writer.string("issue", reference.issue); break;
    case "external_commit": writer.string("repository", reference.repository); writer.string("commit", reference.commit); break;
    case "external_release": writer.string("project", reference.project); writer.string("release", reference.release); break;
    case "consumer_report": writer.string("consumer", reference.consumer); writer.string("report_reference", reference.report_reference); break;
  }
}

export function composeRoadmapDocument(document: RoadmapDocument): Uint8Array {
  const writer = new CanonicalTomlWriter();
  const meta = document.document;
  writer.table("document");
  writer.integer("schema_version", meta.schema_version);
  writer.string("roadmap", meta.roadmap);
  writer.string("source_path", meta.source_path);
  writer.string("projection_path", meta.projection_path);

  const sections: readonly Section[] = document.sections;
  for (const section of sorted(sections, (value) => value.section_id)) {
    writer.arrayTable("section");
    writer.string("section_id", section.section_id);
    writer.string("title", section.title);
    optionalStrings(writer, "legacy_aliases", section.legacy_aliases);
    writer.markdown("body_md", section.body_md);
  }

  const fragments: readonly Fragment[] = document.fragments;
  for (const fragment of sorted(fragments, (value) => value.fragment_id)) {
    writer.arrayTable("fragment");
    writer.string("fragment_id", fragment.fragment_id);
    writer.string("projection_group", fragment.projection_group);
    optionalString(writer, "title", fragment.title);
    optionalStrings(writer, "legacy_aliases", fragment.legacy_aliases);
    writer.markdown("body_md", fragment.body_md);
  }

  const records: readonly RecordNode[] = document.records;
  for (const record of sorted(records, (value) => value.id)) {
    writer.arrayTable("record");
    writer.string("id", record.id);
    writer.string("title", record.title);
    writer.string("projection_group", record.projection_group);
    optionalStrings(writer, "legacy_aliases", record.legacy_aliases);
    optionalStrings(writer, "tags", record.tags);
    writer.table("record.payload");
    writeSemanticPayload(writer, record.payload, "record.payload");
  }

  const parts: readonly Part[] = document.parts;
  for (const part of sorted(parts, (value) => value.part_id)) {
    writer.arrayTable("part");
    writer.string("part_id", part.part_id);
    writer.string("parent_record_id", part.parent_record_id);
    optionalString(writer, "title", part.title);
    writer.markdown("body_md", part.body_md);
  }

  for (const slot of sorted(document.generated_slots, (value) => value.slot_id)) writeGeneratedSlot(writer, slot);
  for (const entry of document.manifest) writeManifestEntry(writer, entry);
  {
    const relations = [...document.relations].sort((left, right) =>
      compare(`${left.source}\0${left.kind}\0${left.target}`, `${right.source}\0${right.kind}\0${right.target}`),
    );
    for (const relation of relations) {
      writer.arrayTable("relation");
      writer.string("source", relation.source);
      writer.string("kind", relation.kind);
      writer.string("target", relation.target);
      optionalMarkdown(writer, "note_md", relation.note_md);
    }
    const references = [...document.references].sort((left, right) =>
      compare(
        `${left.source}\0${left.kind}\0${referenceTuple(left)}`,
        `${right.source}\0${right.kind}\0${referenceTuple(right)}`,
      ),
    );
    for (const reference of references) writeReference(writer, reference);
  }
  return writer.finish();
}

export type CanonicalTomlDocument = RoadmapDocument;

export function composeCanonicalDocument(document: CanonicalTomlDocument): Uint8Array {
  return composeRoadmapDocument(document);
}
