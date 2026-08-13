import { MATRIX_ADAPTER, validateMatrixRoadmapDocument } from "./adapters/matrix.ts";
import { TESTING_ADAPTER, validateTestingRoadmapDocument } from "./adapters/testing.ts";
import type { RegistryView, RoadmapAdapter } from "./adapters/types.ts";
import { parseRoadmapCli, ROADMAP_CLI_USAGE, RoadmapCliParseError } from "./cli.ts";
import { composeCanonicalDocument } from "./compose.ts";
import {
  deriveMigrationDebt,
  migrationDebtReport,
  migrationProgressReport,
  type MigrationDebt,
} from "./debt.ts";
import { decodeRoadmapSource } from "./decode/roadmap.ts";
import {
  failureFromUnknown,
  isRoadmapFailure,
  renderRoadmapIssues,
  RoadmapFailure,
  sortRoadmapIssues,
  type RoadmapIssue,
} from "./errors.ts";
import { validateGlobalIdentity } from "./identity.ts";
import { buildRoadmapIndexes } from "./indexes.ts";
import {
  type RoadmapCliPorts,
  type RoadmapWritePorts,
  type ReadOnlyRoadmapPorts,
} from "./io.ts";
import { resolveManifest } from "./manifest.ts";
import { RoadmapWireError } from "./markdown_codec.ts";
import type {
  AsOfDate,
  CliRequest,
  EvidencePayload,
  FullCommitId,
  QueryView,
  RepoPath,
  RepositoryRevision,
  RoadmapName,
  RoadmapSelection,
  SignalPayload,
} from "./model/core.ts";
import type { RoadmapDocument, SemanticPayload } from "./model/documents.ts";
import {
  resolveOutputClaims,
  validateProductionOutputRegistry,
} from "./output_registry.ts";
import {
  checkCommittedProjectionBytes,
  renderThenCheckCommittedProjection,
  renderValidatedChunks,
  RenderValidationError,
} from "./render.ts";
import {
  buildExpectedChunks,
  createImmutableByteView,
  validateCompletedChunks,
  type CompletedRenderIr,
} from "./render_ir.ts";
import { buildProjectionViews, type ProjectionViews } from "./projection_views.ts";
import { projectionLayout, projectionLayoutRank, validateProjectionLayoutDeclaration } from "./projection_layout.ts";
import { scanRoadmapMarkdownFacts } from "./references.ts";
import {
  semanticConversionCompletionAudit,
  semanticConversionState,
  validateSemanticConversionCompletion,
  validateSemanticConversionDeclaration,
} from "./semantic_conversion.ts";
import { runSelfTests } from "./selftest.ts";
import { validateSourceSpans } from "./spans.ts";
import { validateTransaction } from "./transaction.ts";
import { applyProjectionWritePlan, createProjectionWritePlan } from "./write_plan.ts";
import { MATRIX_DENOMINATOR_AUTHORITIES } from "./fixed_value_authority.ts";

export { createNodeRoadmapCliPorts } from "./io.ts";

const UTF8 = new TextEncoder();
const MATRIX_SOURCE = "cddl-matrix/roadmap.toml" as RepoPath;
const TESTING_SOURCE = "tests/testing-roadmap.toml" as RepoPath;
const SOURCE_BY_ROADMAP: Readonly<Record<RoadmapName, RepoPath>> = Object.freeze({
  matrix: MATRIX_SOURCE,
  testing: TESTING_SOURCE,
});
const ADAPTER_BY_ROADMAP: Readonly<Record<RoadmapName, RoadmapAdapter<SemanticPayload>>> = Object.freeze({
  matrix: MATRIX_ADAPTER,
  testing: TESTING_ADAPTER,
});

export interface RoadmapCliResult {
  exit_code: 0 | 1 | 2;
  stdout: Uint8Array;
  stderr: Uint8Array;
}

interface ValidatedRoadmapCore {
  readonly document: RoadmapDocument;
  readonly registry: RegistryView;
  readonly completed: CompletedRenderIr;
  readonly debt: MigrationDebt;
  readonly projection_views: ProjectionViews;
}

interface FinalizedRoadmap extends ValidatedRoadmapCore {
  readonly projection: Uint8Array;
}

interface RevisionReader {
  read(path: RepoPath): Uint8Array;
  registry(): RegistryView;
  revision: RepositoryRevision;
}

function sha256(bytes: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(bytes).digest("hex");
}

function failure(issues: readonly RoadmapIssue[]): never {
  throw new RoadmapFailure(sortRoadmapIssues(issues));
}

function issue(
  code: RoadmapIssue["code"],
  source: string,
  logicalPath: string,
  message: string,
  exit: 1 | 2 = 1,
): RoadmapIssue {
  return { code, source, logical_path: logicalPath, message, exit };
}

function strictSource(bytes: Uint8Array, source: RepoPath): void {
  if (bytes.byteLength === 0) {
    failure([issue("E-SOURCE-EMPTY", source, "$", "declared source is empty")]);
  }
}

function worktreeReader(ports: ReadOnlyRoadmapPorts): RevisionReader {
  return {
    revision: { kind: "worktree" },
    read: (path) => ports.readDeclared(path),
    registry: () => ports.registryView({ kind: "worktree" }),
  };
}

function commitReader(ports: ReadOnlyRoadmapPorts, commit: FullCommitId): RevisionReader {
  const revision: RepositoryRevision = { kind: "commit", commit };
  return {
    revision,
    read: (path) => ports.readDeclaredAtCommit(commit, path),
    registry: () => ports.registryView(revision),
  };
}

function decodeAt(name: RoadmapName, reader: RevisionReader): { document: RoadmapDocument; source: Uint8Array } {
  const path = SOURCE_BY_ROADMAP[name];
  const source = new Uint8Array(reader.read(path));
  strictSource(source, path);
  const document = decodeRoadmapSource(source, path, name, true);
  const declarationIssues = validateSemanticConversionDeclaration(
    document,
    reader.revision.kind === "commit",
  );
  const layoutIssues = validateProjectionLayoutDeclaration(
    document,
    reader.revision.kind === "commit",
  );
  if (declarationIssues.length > 0 || layoutIssues.length > 0) failure([...declarationIssues, ...layoutIssues]);
  return { document, source };
}

function domainValidation(
  document: RoadmapDocument,
  view: RegistryView,
  options: Parameters<typeof validateMatrixRoadmapDocument>[2] = {},
) {
  return document.document.roadmap === "matrix"
    ? validateMatrixRoadmapDocument(document, view, options)
    : validateTestingRoadmapDocument(document, view, options);
}

function prepareRoadmapCore(name: RoadmapName, reader: RevisionReader): ValidatedRoadmapCore {
  const { document } = decodeAt(name, reader);
  return prepareDecodedRoadmapCore(name, document, reader.registry());
}

function prepareDecodedRoadmapCore(
  name: RoadmapName,
  document: RoadmapDocument,
  registry: RegistryView,
): ValidatedRoadmapCore {
  const manifest = resolveManifest(document);
  const adapter = ADAPTER_BY_ROADMAP[name];
  const resolvers = adapter.slotResolvers(registry, document);
  const completed = buildExpectedChunks(document, manifest.ops, {
    renderSemanticRecord(record, fields) {
      return adapter.renderSemantic(record, fields);
    },
    resolveGeneratedSlot(slot) {
      return resolvers.get(slot.slot_id)?.resolve(slot, registry);
    },
  });
  const renderIssues = [
    ...manifest.issues,
    ...validateCompletedChunks(document, manifest.ops, completed),
  ];
  if (renderIssues.length > 0) failure(renderIssues);
  const projectionViews = buildProjectionViews(
    document,
    completed,
    renderValidatedChunks(completed.chunks, [], completed.expected_bytes),
  );
  if (projectionViews.issues.length > 0) failure(projectionViews.issues);
  validateProjectionAnchors(document, projectionViews.full);
  // Authoritative v1 references to the roadmap projection resolve against the projection this
  // decoded source just built. The committed projection is prior output and therefore cannot be
  // an input to domain validation. Build/slot resolution deliberately precedes this scan because
  // expected bytes are its authority; neither depends on a successful domain verdict.
  const validationRegistry = document.document.schema_version !== 0 && document.document.authority === "authoritative"
    ? registryWithRoadmapMarkdownFact(
      registry,
      adapter.projection_path,
      createImmutableByteView(projectionViews.full),
    )
    : registry;
  const domain = domainValidation(document, validationRegistry, {
    defer_foreign_roadmap_joins: true,
    denominator_authorities: document.document.roadmap === "matrix" ? MATRIX_DENOMINATOR_AUTHORITIES : undefined,
  });
  const structuralIssues = [...domain.issues];
  if (structuralIssues.length > 0) failure(structuralIssues);

  const spanIssues = validateSourceSpans({ document, completed });
  if (spanIssues.length > 0) failure(spanIssues);
  const debt = deriveMigrationDebt(document, completed);
  const completionIssues = validateSemanticConversionCompletion(document, debt, completed);
  if (completionIssues.length > 0) failure(completionIssues);
  const productionOutput = validateProductionOutputRegistry(
    registry.output_claims,
    registry.production_output_stage,
  );
  if (!productionOutput.ok) failure(productionOutput.issues);
  const projectionIsOwned = productionOutput.claims.some((claim) =>
    claim.kind === "whole_file" && claim.path === document.document.projection_path
  );
  if (document.document.authority === "authoritative" && !projectionIsOwned) {
    failure([issue(
      "E-OUTPUT-AUTHORITY",
      document.document.source_path,
      "document.authority",
      "authoritative roadmap requires its same-revision production whole-file projection claim",
    )]);
  }
  if (document.document.authority === "shadow" && projectionIsOwned) {
    failure([issue(
      "E-OUTPUT-AUTHORITY",
      document.document.source_path,
      "document.authority",
      "shadow roadmap forbids an authoritative whole-file projection claim",
    )]);
  }
  return Object.freeze({
    document,
    registry: validationRegistry,
    completed,
    debt,
    projection_views: projectionViews,
  });
}

function finalizeRoadmap(core: ValidatedRoadmapCore): FinalizedRoadmap {
  return Object.freeze({
    ...core,
    projection: new Uint8Array(core.projection_views.full),
  });
}

function selectedRoadmaps(selection: RoadmapSelection): readonly RoadmapName[] {
  return selection === "all" ? ["matrix", "testing"] : [selection];
}

function roadmapReceipt(prepared: FinalizedRoadmap): string {
  const debt = migrationDebtReport(prepared.debt);
  const independent = Object.values(debt.independent_counts).reduce((sum, value) => sum + value, 0);
  const owners = Object.values(debt.owner_counts).reduce((sum, value) => sum + value, 0);
  const projectionOwner = prepared.registry.output_claims.find((claim) =>
    claim.kind === "whole_file" && claim.path === prepared.document.document.projection_path
  )?.producer ?? "unclaimed";
  const completion = semanticConversionCompletionAudit(
    prepared.document,
    prepared.debt,
    prepared.completed,
  );
  return `source=${prepared.document.document.source_path} schema=${prepared.document.document.schema_version} authority=${prepared.document.document.authority} semantic_conversion_declared=${completion.declared} semantic_conversion_effective=${completion.effective} completion_blockers=${completion.blockers.length} join_blockers=${completion.join_blockers.length} projection_bytes=${prepared.projection.byteLength} projection_sha256=${sha256(prepared.projection)} manifest=${prepared.document.manifest.length} spans=${prepared.document.spans.length} debt_owners=${owners} debt_independent=${independent} output_claims=${prepared.registry.output_claims.length} projection_owner=${projectionOwner}`;
}

function success(stdout: string | Uint8Array): RoadmapCliResult {
  return { exit_code: 0, stdout: typeof stdout === "string" ? UTF8.encode(stdout) : stdout, stderr: new Uint8Array() };
}

function checkRoadmaps(
  selection: RoadmapSelection,
  ports: ReadOnlyRoadmapPorts,
): RoadmapCliResult {
  const receipts: string[] = [];
  for (const name of selectedRoadmaps(selection)) {
    const core = prepareRoadmapCore(name, worktreeReader(ports));
    receipts.push(roadmapReceipt(checkCommittedProjection(core, ports)));
  }
  return success(`CHECK OK\n${receipts.join("\n")}\n`);
}

function checkCommittedProjection(
  core: ValidatedRoadmapCore,
  ports: ReadOnlyRoadmapPorts,
): FinalizedRoadmap {
  let checked: ReturnType<typeof renderThenCheckCommittedProjection>;
  try {
    checked = checkCommittedProjectionBytes(
      core.projection_views.full,
      core.document.document.projection_path,
      () => ports.readDeclared(core.document.document.projection_path),
    );
  } catch (error) {
    if (isRoadmapFailure(error)) {
      failure(error.issues.map((value) => value.code === "E-SOURCE-MISSING"
        ? { ...value, code: "E-PROJECTION-MISSING" as const, source: core.document.document.projection_path }
        : value));
    }
    throw error;
  }
  if (checked.issues.length > 0) failure(checked.issues);
  return Object.freeze({ ...core, projection: checked.expected });
}

function stableJsonValue(value: unknown): unknown {
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

function queryValue(prepared: readonly FinalizedRoadmap[], view: QueryView, asOf: AsOfDate | undefined): unknown {
  const evaluation_as_of = asOf ?? null;
  const payloadRows = prepared.flatMap((item) => item.document.records.flatMap((record) => {
    const payload = "payload" in record ? record.payload : "semantic_shadow" in record ? record.semantic_shadow : undefined;
    return payload === undefined ? [] : [{ roadmap: item.document.document.roadmap, id: record.id, payload }];
  })).sort((left, right) => left.roadmap < right.roadmap ? -1 : left.roadmap > right.roadmap ? 1 :
    left.id < right.id ? -1 : left.id > right.id ? 1 : 0);
  switch (view) {
    case "summary":
      return {
        evaluation_as_of,
        roadmaps: prepared.map((item) => ({
          roadmap: item.document.document.roadmap,
          schema_version: item.document.document.schema_version,
          authority: item.document.document.authority,
          semantic_conversion: semanticConversionState(item.document),
          record_count: item.document.records.length,
          families: item.document.records.flatMap((record) => {
            const payload = "payload" in record ? record.payload : "semantic_shadow" in record ? record.semantic_shadow : undefined;
            if (payload?.kind !== "family") return [];
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
    case "debt":
      return { evaluation_as_of, roadmaps: prepared.map((item) => ({
        roadmap: item.document.document.roadmap,
        semantic_conversion: semanticConversionCompletionAudit(item.document, item.debt, item.completed),
        ...migrationDebtReport(item.debt),
        migration_progress: migrationProgressReport(item.document, item.debt, item.completed),
      })) };
    case "references":
      return { evaluation_as_of, roadmaps: prepared.map((item) => ({
        roadmap: item.document.document.roadmap,
        references: "references" in item.document ? item.document.references : [],
      })) };
    case "signals": {
      const rows = payloadRows.flatMap(({ roadmap, id, payload }): readonly Record<string, unknown>[] => {
        if (payload.kind === "signal") {
          return [{ roadmap, id, transition_kind: payload.transition_kind,
            evaluation: evaluateTemporalPayload(payload, asOf), summary_md: payload.summary_md,
            ...(payload.transition_kind === "promotion_trigger" || payload.transition_kind === "reopening_signal"
              ? { observer: payload.observer, dimension: payload.dimension, observable: payload.observable,
                predicate_kind: payload.predicate_kind, current_evidence_ids: payload.current_evidence_ids,
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
            summary_md: payload.summary_md,
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
          summary_md: payload.summary_md,
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
      const relations = prepared.flatMap((item) => "relations" in item.document ? item.document.relations : []);
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
          summary_md: payload.summary_md,
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
        legacy_span_provenance: item.projection_views.legacy_span_provenance,
      })) };
    case "output-owners":
      return { evaluation_as_of, claims: prepared.flatMap((item) => item.registry.output_claims) };
  }
}

function queryText(value: unknown): Uint8Array {
  const stable = stableJsonValue(value) as Record<string, unknown>;
  const lines = Object.keys(stable).sort().map((key) => `${key}: ${JSON.stringify(stable[key])}`);
  return UTF8.encode(`${lines.join("\n")}\n`);
}

function queryRoadmaps(
  selection: RoadmapSelection,
  view: QueryView,
  asOf: AsOfDate | undefined,
  ports: ReadOnlyRoadmapPorts,
  json = false,
): Uint8Array {
  const prepared = selectedRoadmaps(selection).map((name) => {
    return finalizeRoadmap(prepareRoadmapCore(name, worktreeReader(ports)));
  });
  const value = stableJsonValue(queryValue(prepared, view, asOf));
  return json ? UTF8.encode(`${JSON.stringify(value)}\n`) : queryText(value);
}

function identityFor(prepared: ValidatedRoadmapCore) {
  const indexes = buildRoadmapIndexes(prepared.document).indexes;
  return validateGlobalIdentity({
    documents: [indexes.identity_inputs],
    current_guards: prepared.registry.current_guards,
  });
}

/**
 * The candidate side of a scoped comparison: the selected roadmap's own identity universe over the
 * worktree registry. There is no cross-roadmap lifecycle root, so this is the complete authority.
 */
function scopedCandidateIdentity(prepared: ValidatedRoadmapCore) {
  return { registry: prepared.registry, identity: identityFor(prepared) };
}

function validateOneRoadmapChange(
  selection: RoadmapName,
  against: FullCommitId,
  ports: ReadOnlyRoadmapPorts,
): readonly RoadmapIssue[] {
  const candidate = prepareRoadmapCore(selection, worktreeReader(ports));
  const candidateGlobal = scopedCandidateIdentity(candidate);
  const baseReader = commitReader(ports, against);
  const result = validateTransaction({
    scope: selection,
    against,
    load_base: () => {
      const base = prepareRoadmapCore(selection, baseReader);
      return {
        document: base.document,
        debt: base.debt,
        completed: base.completed,
        identity: identityFor(base),
        registry: base.registry,
      };
    },
    candidate_document: candidate.document,
    candidate_debt: candidate.debt,
    candidate_completed: candidate.completed,
    candidate_registry: candidateGlobal.registry,
    candidate_global_identity: candidateGlobal.identity,
  });
  return result.issues;
}

function validateChange(
  selection: RoadmapSelection,
  against: FullCommitId,
  ports: ReadOnlyRoadmapPorts,
): readonly RoadmapIssue[] {
  return sortRoadmapIssues(selectedRoadmaps(selection).flatMap((name) =>
    validateOneRoadmapChange(name, against, ports)
  ));
}

function registryWithRoadmapMarkdownFact(
  registry: RegistryView,
  path: RepoPath,
  markdown: Parameters<typeof scanRoadmapMarkdownFacts>[1],
): RegistryView {
  const facts = scanRoadmapMarkdownFacts(path, markdown);
  if (facts.issues.length > 0) failure(facts.issues);
  // Replace, rather than append, this projection's facts. Besides keeping revalidation
  // idempotent, this fails closed if an injected registry ever contains stale prior-projection
  // facts: only the selected immutable view can describe the current projection path.
  const citations = [
    ...registry.roadmap_citations.filter((value) => value.source !== path),
    ...facts.citations,
  ].sort((left, right) =>
    left.source < right.source ? -1 : left.source > right.source ? 1 :
      left.span.start_byte - right.span.start_byte || left.span.end_byte - right.span.end_byte ||
      (left.id < right.id ? -1 : left.id > right.id ? 1 : 0)
  );
  const headings = [
    ...registry.tracked_headings.filter((value) => value.path !== path),
    ...facts.headings,
  ].sort((left, right) =>
    left.path < right.path ? -1 : left.path > right.path ? 1 :
      left.span.start_byte - right.span.start_byte || (left.heading < right.heading ? -1 : left.heading > right.heading ? 1 : 0)
  );
  return Object.freeze({
    ...registry,
    tracked_headings: Object.freeze(headings),
    roadmap_citations: Object.freeze(citations),
  });
}

function validateProjectionAnchors(document: RoadmapDocument, projection: Uint8Array): void {
  if (projectionLayoutRank(projectionLayout(document)) < 1) return;
  const facts = scanRoadmapMarkdownFacts(document.document.projection_path, createImmutableByteView(projection));
  if (facts.issues.length > 0) failure(facts.issues);
  const expected = document.records.flatMap((record) =>
    "projection_visibility" in record && record.projection_visibility === "document" ? [record.id] : []
  ).sort();
  if (JSON.stringify(facts.stable_anchor_ids) !== JSON.stringify(expected)) failure([issue(
    "E-ID-DUPLICATE",
    document.document.projection_path,
    "roadmap-anchor",
    `stable anchor inventory must exactly equal document-visible record IDs (expected=${expected.length}, actual=${facts.stable_anchor_ids.length})`,
  )]);
}

function formatSource(path: RepoPath, ports: RoadmapWritePorts): RoadmapCliResult {
  const source = new Uint8Array(ports.readDeclared(path));
  strictSource(source, path);
  let document: RoadmapDocument;
  if (path === MATRIX_SOURCE || path === TESTING_SOURCE) {
    const name: RoadmapName = path === MATRIX_SOURCE ? "matrix" : "testing";
    document = decodeRoadmapSource(source, path, name, false);
    const declarationIssues = [
      ...validateSemanticConversionDeclaration(document, false),
      ...validateProjectionLayoutDeclaration(document, false),
    ];
    if (declarationIssues.length > 0) failure(declarationIssues);
    prepareDecodedRoadmapCore(name, document, ports.registryView({ kind: "worktree" }));
  } else failure([issue("E-CLI-FORMAT-TARGET", "<cli>", "format_source", "format target is not declared", 2)]);
  const canonical = composeCanonicalDocument(document);
  ports.atomicReplace(path, canonical);
  return success(`FORMAT OK source=${path} bytes=${canonical.byteLength} sha256=${sha256(canonical)}\n`);
}

function validateAgainst(candidate: string, ports: ReadOnlyRoadmapPorts): FullCommitId {
  const format = ports.repositoryObjectFormat();
  const length = format === "sha1" ? 40 : 64;
  if (!(new RegExp(`^[0-9a-f]{${length}}$`).test(candidate))) {
    failure([issue(
      "E-GIT-BASE-FORMAT",
      "<git>",
      "against",
      `--against must be exactly ${length} lowercase hexadecimal characters for repository object format ${format}`,
      2,
    )]);
  }
  let resolved: FullCommitId;
  try {
    resolved = ports.resolveFullCommit(candidate);
  } catch (error) {
    if (isRoadmapFailure(error)) throw error;
    failure([issue("E-GIT-BASE-LOOKUP", "<git>", "against", "--against names no commit object with that exact object ID", 2)]);
  }
  if (resolved !== candidate) {
    failure([issue("E-GIT-BASE-LOOKUP", "<git>", "against", "--against names no commit object with that exact object ID", 2)]);
  }
  return resolved;
}

function writeRoadmap(name: RoadmapName, ports: RoadmapWritePorts): RoadmapCliResult {
  const reader = worktreeReader(ports);
  const prepared = prepareRoadmapCore(name, reader);
  const finalized = finalizeRoadmap(prepared);
  const productionOutput = validateProductionOutputRegistry(
    prepared.registry.output_claims,
    prepared.registry.production_output_stage,
  );
  if (!productionOutput.ok) failure(productionOutput.issues);
  const projectionClaim = productionOutput.claims.find((claim) =>
    claim.kind === "whole_file" && claim.path === prepared.document.document.projection_path
  );
  const resolution = projectionClaim === undefined
    ? undefined
    : resolveOutputClaims({
      registry: productionOutput.registry,
      claims: [projectionClaim],
      targets: new Map([[
        prepared.document.document.projection_path,
        ports.readDeclared(prepared.document.document.projection_path),
      ]]),
    });
  const result = createProjectionWritePlan({
    write_coordinate: "projection",
    roadmap: name,
    document: prepared.document,
    projection_bytes: finalized.projection,
    ...(resolution?.authority === undefined ? {} : { output_authority: resolution.authority }),
    validation_issues: resolution?.issues ?? Object.freeze([]),
  });
  if (!result.ok) failure(result.issues);
  applyProjectionWritePlan(result.plan, ports);
  return success(`WRITE OK roadmap=${name} target=${result.plan.target} bytes=${result.plan.bytes.byteLength} sha256=${sha256(result.plan.bytes)}\n`);
}

function resultFromRoadmapCliFailure(error: unknown): RoadmapCliResult {
  let issues: readonly RoadmapIssue[];
  let usage = false;
  if (error instanceof RoadmapCliParseError) {
    issues = [error.issue];
    usage = true;
  } else if (error instanceof RoadmapWireError) {
    issues = [error.issue];
  } else if (error instanceof RenderValidationError) {
    issues = error.issues.length > 0
      ? error.issues
      : [issue("E-RENDER-EMPTY", "<internal>", "render", error.message, 2)];
  } else if (isRoadmapFailure(error)) {
    issues = error.issues;
    usage = issues.some((value) => value.code.startsWith("E-CLI-"));
  } else {
    const converted = failureFromUnknown(error, "runRoadmapCli");
    issues = converted.issues;
  }
  const sorted = sortRoadmapIssues(issues);
  const exit = sorted.some((value) => value.exit === 2) ? 2 : 1;
  const diagnostics = renderRoadmapIssues(sorted);
  const suffix = usage ? UTF8.encode(ROADMAP_CLI_USAGE) : new Uint8Array();
  const stderr = new Uint8Array(diagnostics.byteLength + suffix.byteLength);
  stderr.set(diagnostics);
  stderr.set(suffix, diagnostics.byteLength);
  return {
    exit_code: exit,
    stdout: new Uint8Array(),
    stderr,
  };
}

export interface RoadmapCliDispatchServices {
  readonly run_selftests: typeof runSelfTests;
}

const DEFAULT_DISPATCH_SERVICES: RoadmapCliDispatchServices = Object.freeze({
  run_selftests: runSelfTests,
});

function dispatchRoadmapCliRequest(
  request: CliRequest,
  ports: RoadmapCliPorts,
  services: RoadmapCliDispatchServices,
): RoadmapCliResult {
  switch (request.mode) {
    case "selftest": {
      const result = services.run_selftests(ports.selftest);
      return success(result.stdout);
    }
    case "check": {
      const against = request.against === undefined ? undefined : validateAgainst(request.against, ports.read);
      const selftest = services.run_selftests(ports.selftest);
      if (against !== undefined) {
        const transactionIssues = validateChange(request.roadmap, against, ports.read);
        if (transactionIssues.length > 0) failure(transactionIssues);
      }
      const checked = checkRoadmaps(request.roadmap, ports.read);
      const stdout = new Uint8Array(selftest.stdout.byteLength + checked.stdout.byteLength);
      stdout.set(selftest.stdout);
      stdout.set(checked.stdout, selftest.stdout.byteLength);
      return success(stdout);
    }
    case "write":
      return writeRoadmap(request.roadmap, ports.write);
    case "query":
      return success(queryRoadmaps(request.roadmap, request.view, request.as_of, ports.read, request.json));
    case "format_source":
      return formatSource(request.source_path, ports.write);
  }
}

/** Dispatch one argv vector with an already-constructed narrowed port bundle. */
export function runRoadmapCli(
  argv: readonly string[],
  ports: RoadmapCliPorts,
  services: RoadmapCliDispatchServices = DEFAULT_DISPATCH_SERVICES,
): RoadmapCliResult {
  try {
    return dispatchRoadmapCliRequest(parseRoadmapCli(argv), ports, services);
  } catch (error) {
    return resultFromRoadmapCliFailure(error);
  }
}
