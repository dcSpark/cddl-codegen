import { MATRIX_ADAPTER, validateMatrixRoadmapDocument } from "./adapters/matrix.ts";
import { TESTING_ADAPTER, validateTestingRoadmapDocument } from "./adapters/testing.ts";
import type { RegistryView, RoadmapAdapter } from "./adapters/types.ts";
import { parseRoadmapCli, ROADMAP_CLI_USAGE, RoadmapCliParseError } from "./cli.ts";
import { createImmutableByteView, validateLegacyTitleBinding } from "./campaign.ts";
import { composeCanonicalDocument } from "./compose.ts";
import { deriveMigrationDebt, migrationDebtReport, type MigrationDebt } from "./debt.ts";
import { decodeCampaignSource } from "./decode/campaign.ts";
import { decodeRetiredSource } from "./decode/retired.ts";
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
import type {
  CampaignDocumentV1,
  RetiredIdsDocumentV1,
  RoadmapDocument,
  SemanticPayload,
} from "./model/documents.ts";
import {
  productionOutputStage,
  resolveOutputClaims,
  validateProductionOutputRegistry,
} from "./output_registry.ts";
import {
  renderThenCheckCommittedProjection,
  renderValidatedChunks,
  RenderValidationError,
} from "./render.ts";
import { buildExpectedChunks, validateCompletedChunks, type CompletedRenderIr } from "./render_ir.ts";
import { scanRoadmapMarkdownFacts } from "./references.ts";
import { runSelfTests } from "./selftest.ts";
import { validateSourceSpans } from "./spans.ts";
import {
  validateLifecycleRevision,
  validateTransaction,
  type LifecycleRevisionInput,
} from "./transaction.ts";
import { applyProjectionWritePlan, createProjectionWritePlan } from "./write_plan.ts";

export { createNodeRoadmapCliPorts } from "./io.ts";

const UTF8 = new TextEncoder();
const MATRIX_SOURCE = "cddl-matrix/roadmap.toml" as RepoPath;
const TESTING_SOURCE = "tests/testing-roadmap.toml" as RepoPath;
const CAMPAIGN_SOURCE = "roadmap-campaign.toml" as RepoPath;
const RETIRED_SOURCE = "roadmap-retired-ids.toml" as RepoPath;
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
  return { document: decodeRoadmapSource(source, path, name, true), source };
}

function domainValidation(document: RoadmapDocument, view: RegistryView) {
  return document.document.roadmap === "matrix"
    ? validateMatrixRoadmapDocument(document, view)
    : validateTestingRoadmapDocument(document, view);
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
  const domain = domainValidation(document, registry);
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
  const structuralIssues = [
    ...domain.issues,
    ...manifest.issues,
    ...validateCompletedChunks(document, manifest.ops, completed),
  ];
  if (structuralIssues.length > 0) failure(structuralIssues);

  const spanIssues = validateSourceSpans({ document, completed });
  if (spanIssues.length > 0) failure(spanIssues);
  const debt = deriveMigrationDebt(document, completed);
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
    registry,
    completed,
    debt,
  });
}

function validatePreActivationOutputStage(registry: RegistryView): void {
  const stage = productionOutputStage();
  if (registry.production_output_stage !== stage) {
    failure([issue(
      "E-OUTPUT-CLAIM",
      "<output-registry>",
      "production_output_stage",
      `pre-activation registry stage ${registry.production_output_stage} does not match canonical stage ${stage}`,
    )]);
  }
}

function finalizeRoadmap(core: ValidatedRoadmapCore): FinalizedRoadmap {
  return Object.freeze({
    ...core,
    projection: renderValidatedChunks(core.completed.chunks, [], core.completed.expected_bytes),
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
  return `source=${prepared.document.document.source_path} schema=${prepared.document.document.schema_version} authority=${prepared.document.document.authority} projection_bytes=${prepared.projection.byteLength} projection_sha256=${sha256(prepared.projection)} manifest=${prepared.document.manifest.length} spans=${prepared.document.spans.length} debt_owners=${owners} debt_independent=${independent} output_claims=${prepared.registry.output_claims.length} projection_owner=${projectionOwner}`;
}

function success(stdout: string | Uint8Array): RoadmapCliResult {
  return { exit_code: 0, stdout: typeof stdout === "string" ? UTF8.encode(stdout) : stdout, stderr: new Uint8Array() };
}

function checkRoadmaps(
  selection: RoadmapSelection,
  ports: ReadOnlyRoadmapPorts,
): RoadmapCliResult {
  const reader = worktreeReader(ports);
  if (selection !== "all") {
    const core = prepareRoadmapCore(selection, reader);
    const lifecycle = loadActivatedLifecycle(
      reader,
      selection === "matrix" ? { matrix: core } : { testing: core },
    );
    if (lifecycle !== undefined) {
      const validated = validateLifecycleRevision(lifecycle);
      if (validated.issues.length > 0) failure(validated.issues);
      const lifecycleCore = preparedRoadmapsFromLifecycle(lifecycle, selection)[0];
      if (lifecycleCore === undefined) {
        failure([issue("E-SCHEMA-STATE", core.document.document.source_path, "document", `${selection} lifecycle omitted its selected structured roadmap`)]);
      }
      const checked = lifecycleCore.document.document.authority === "shadow"
        ? finalizeRoadmap(lifecycleCore)
        : checkCommittedProjection(lifecycleCore, ports);
      return success(`CHECK OK\n${roadmapReceipt(checked)}\n`);
    }
    validatePreActivationOutputStage(core.registry);
    return success(`CHECK OK\n${roadmapReceipt(checkCommittedProjection(core, ports))}\n`);
  }
  const lifecycle = loadActivatedLifecycle(reader);
  if (lifecycle !== undefined) {
    const validated = validateLifecycleRevision(lifecycle);
    if (validated.issues.length > 0) failure(validated.issues);
    const receipts = preparedRoadmapsFromLifecycle(lifecycle).map((core) => {
      if (core.document.document.authority === "shadow") {
        return roadmapReceipt(finalizeRoadmap(core));
      }
      return roadmapReceipt(checkCommittedProjection(core, ports));
    });
    return success(`CHECK OK\n${receipts.join("\n")}\n`);
  }
  validatePreActivationOutputStage(reader.registry());
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
    checked = renderThenCheckCommittedProjection(
      core.completed.chunks,
      [],
      core.completed.expected_bytes,
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

function queryValue(prepared: readonly FinalizedRoadmap[], view: QueryView, asOf: AsOfDate | undefined): unknown {
  const evaluation_as_of = asOf ?? null;
  switch (view) {
    case "summary":
      return {
        evaluation_as_of,
        roadmaps: prepared.map((item) => ({
          roadmap: item.document.document.roadmap,
          schema_version: item.document.document.schema_version,
          authority: item.document.document.authority,
          record_count: item.document.records.length,
          projection_byte_length: item.projection.byteLength,
          projection_sha256: sha256(item.projection),
        })),
      };
    case "debt":
      return { evaluation_as_of, roadmaps: prepared.map((item) => ({
        roadmap: item.document.document.roadmap,
        ...migrationDebtReport(item.debt),
      })) };
    case "references":
      return { evaluation_as_of, roadmaps: prepared.map((item) => ({
        roadmap: item.document.document.roadmap,
        references: "references" in item.document ? item.document.references : [],
      })) };
    case "signals":
      return { evaluation_as_of, signals: prepared.flatMap((item) => item.document.records.flatMap((record) => {
        const payload = "payload" in record ? record.payload : "semantic_shadow" in record ? record.semantic_shadow : undefined;
        if (payload?.kind === "signal") {
          return [{ roadmap: item.document.document.roadmap, id: record.id, evaluation: evaluateTemporalPayload(payload, asOf) }];
        }
        if (payload?.kind === "evidence" && payload.freshness === "as_of") {
          return [{
            roadmap: item.document.document.roadmap,
            id: record.id,
            evaluation: evaluateTemporalPayload(payload, asOf),
          }];
        }
        return [];
      })) };
    case "output-owners":
      return { evaluation_as_of, claims: prepared.flatMap((item) => item.registry.output_claims) };
    case "campaign":
      return { evaluation_as_of, campaign: null, state: "not_loaded_without_lifecycle_scope" };
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
  if (view === "campaign") {
    const lifecycle = loadActivatedLifecycle(worktreeReader(ports));
    if (lifecycle === undefined) {
      const value = stableJsonValue({ evaluation_as_of: asOf ?? null, campaign: null, state: "not_activated" });
      return json ? UTF8.encode(`${JSON.stringify(value)}\n`) : queryText(value);
    }
    const validated = validateLifecycleRevision(lifecycle);
    if (validated.issues.length > 0) failure(validated.issues);
    const value = stableJsonValue({ evaluation_as_of: asOf ?? null, campaign: lifecycle.campaign });
    return json ? UTF8.encode(`${JSON.stringify(value)}\n`) : queryText(value);
  }
  const reader = worktreeReader(ports);
  if (selection !== "all") {
    const core = prepareRoadmapCore(selection, reader);
    const lifecycle = loadActivatedLifecycle(
      reader,
      selection === "matrix" ? { matrix: core } : { testing: core },
    );
    if (lifecycle !== undefined) {
      const validated = validateLifecycleRevision(lifecycle);
      if (validated.issues.length > 0) failure(validated.issues);
    } else {
      validatePreActivationOutputStage(core.registry);
    }
    const value = stableJsonValue(queryValue([finalizeRoadmap(core)], view, asOf));
    return json ? UTF8.encode(`${JSON.stringify(value)}\n`) : queryText(value);
  }
  const lifecycle = loadActivatedLifecycle(reader);
  if (lifecycle !== undefined) {
    const validated = validateLifecycleRevision(lifecycle);
    if (validated.issues.length > 0) failure(validated.issues);
    const value = stableJsonValue(queryValue(
      preparedRoadmapsFromLifecycle(lifecycle).map(finalizeRoadmap),
      view,
      asOf,
    ));
    return json ? UTF8.encode(`${JSON.stringify(value)}\n`) : queryText(value);
  }
  validatePreActivationOutputStage(reader.registry());
  const prepared = selectedRoadmaps(selection).map((name) =>
    finalizeRoadmap(prepareRoadmapCore(name, worktreeReader(ports)))
  );
  const value = stableJsonValue(queryValue(prepared, view, asOf));
  return json ? UTF8.encode(`${JSON.stringify(value)}\n`) : queryText(value);
}

function preparedRoadmapsFromLifecycle(
  lifecycle: LifecycleRevisionInput,
  selection: RoadmapSelection = "all",
): readonly ValidatedRoadmapCore[] {
  return selectedRoadmaps(selection).flatMap((name) => {
    const document = lifecycle.roadmaps[name].document;
    return document === undefined
      ? []
      : [prepareDecodedRoadmapCore(name, document, lifecycle.registry)];
  });
}

function identityFor(prepared: ValidatedRoadmapCore) {
  const indexes = buildRoadmapIndexes(prepared.document).indexes;
  return validateGlobalIdentity({
    documents: [indexes.identity_inputs],
    current_guards: prepared.registry.current_guards,
  });
}

function scopedCandidateIdentity(
  reader: RevisionReader,
  selected: RoadmapName,
  prepared: ValidatedRoadmapCore,
) {
  const lifecycle = loadLifecycle(reader, { [selected]: prepared });
  const validated = validateLifecycleRevision(lifecycle);
  return {
    registry: lifecycle.registry,
    identity: Object.freeze({
      ...validated.identity,
      // A scoped comparison cannot authorize lifecycle changes, but its candidate must still be a
      // valid member of the current global authority/identity universe. Carry every current-tree
      // lifecycle issue through the identity result consumed by the scoped transaction.
      issues: validated.issues,
    }),
  };
}

function validateChange(
  selection: RoadmapSelection,
  against: FullCommitId,
  ports: ReadOnlyRoadmapPorts,
): readonly RoadmapIssue[] {
  if (selection === "all") {
    return validateAllChange(against, ports);
  }
  const candidateReader = worktreeReader(ports);
  const candidate = prepareRoadmapCore(selection, candidateReader);
  const candidateGlobal = scopedCandidateIdentity(candidateReader, selection, candidate);
  const baseReader = commitReader(ports, against);
  const result = validateTransaction({
    scope: selection,
    against,
    load_base: () => {
      const base = prepareRoadmapCore(selection, baseReader);
      return { document: base.document, debt: base.debt, identity: identityFor(base), registry: base.registry };
    },
    candidate_document: candidate.document,
    candidate_debt: candidate.debt,
    candidate_registry: candidateGlobal.registry,
    candidate_global_identity: candidateGlobal.identity,
  });
  return result.issues;
}

function sourceIsMissing(error: unknown, path: RepoPath): boolean {
  return isRoadmapFailure(error) && error.issues.length > 0 &&
    error.issues.every((value) => value.code === "E-SOURCE-MISSING" && value.source === path);
}

function readOptionalSource(reader: RevisionReader, path: RepoPath): Uint8Array | undefined {
  try {
    return reader.read(path);
  } catch (error) {
    if (sourceIsMissing(error, path)) return undefined;
    throw error;
  }
}

function loadActivatedLifecycle(
  reader: RevisionReader,
  prepared: Partial<Readonly<Record<RoadmapName, ValidatedRoadmapCore>>> = {},
): LifecycleRevisionInput | undefined {
  const campaignBytes = readOptionalSource(reader, CAMPAIGN_SOURCE);
  const retiredBytes = readOptionalSource(reader, RETIRED_SOURCE);
  if (campaignBytes === undefined && retiredBytes === undefined) return undefined;
  if (campaignBytes === undefined) {
    failure([issue("E-SOURCE-MISSING", CAMPAIGN_SOURCE, "$", "declared source is missing")]);
  }
  if (retiredBytes === undefined) {
    failure([issue("E-SOURCE-MISSING", RETIRED_SOURCE, "$", "declared source is missing")]);
  }
  const campaign = decodeCampaignSource(campaignBytes, CAMPAIGN_SOURCE, true);
  const retired = decodeRetiredSource(retiredBytes, RETIRED_SOURCE, true);
  return assembleLifecycle(reader, campaign, retired, prepared);
}

function loadLifecycle(
  reader: RevisionReader,
  prepared: Partial<Readonly<Record<RoadmapName, ValidatedRoadmapCore>>> = {},
): LifecycleRevisionInput {
  const loaded = loadActivatedLifecycle(reader, prepared);
  if (loaded === undefined) {
    failure([
      issue("E-SOURCE-MISSING", CAMPAIGN_SOURCE, "$", "declared source is missing"),
      issue("E-SOURCE-MISSING", RETIRED_SOURCE, "$", "declared source is missing"),
    ]);
  }
  return loaded;
}

function readLegacyMarkdown(reader: RevisionReader, name: RoadmapName) {
  const path = ADAPTER_BY_ROADMAP[name].projection_path;
  try {
    return createImmutableByteView(reader.read(path));
  } catch (error) {
    if (isRoadmapFailure(error)) {
      const remapped = error.issues.map((value) => value.code === "E-PROJECTION-MISSING"
        ? { ...value, code: "E-SOURCE-MISSING" as const, source: path }
        : value);
      failure(remapped);
    }
    throw error;
  }
}

function readAndValidateShadowMarkdown(
  reader: RevisionReader,
  name: RoadmapName,
  core: ValidatedRoadmapCore,
) {
  const authoritativeMarkdown = readLegacyMarkdown(reader, name);
  const checked = renderThenCheckCommittedProjection(
    core.completed.chunks,
    [],
    core.completed.expected_bytes,
    ADAPTER_BY_ROADMAP[name].projection_path,
    () => authoritativeMarkdown.sliceBytes(0, authoritativeMarkdown.byte_length),
  );
  if (checked.issues.length > 0) failure(checked.issues);
  return authoritativeMarkdown;
}

function registryWithRoadmapMarkdownFacts(
  registry: RegistryView,
  roadmaps: LifecycleRevisionInput["roadmaps"],
): RegistryView {
  const facts = (["matrix", "testing"] as const).map((name) =>
    scanRoadmapMarkdownFacts(ADAPTER_BY_ROADMAP[name].projection_path, createImmutableByteView(roadmaps[name].markdown))
  );
  const factIssues = facts.flatMap((value) => value.issues);
  if (factIssues.length > 0) failure(factIssues);
  const citations = [...registry.roadmap_citations, ...facts.flatMap((value) => value.citations)].sort((left, right) =>
    left.source < right.source ? -1 : left.source > right.source ? 1 :
      left.span.start_byte - right.span.start_byte || left.span.end_byte - right.span.end_byte ||
      (left.id < right.id ? -1 : left.id > right.id ? 1 : 0)
  );
  const headings = [...registry.tracked_headings, ...facts.flatMap((value) => value.headings)].sort((left, right) =>
    left.path < right.path ? -1 : left.path > right.path ? 1 :
      left.span.start_byte - right.span.start_byte || (left.heading < right.heading ? -1 : left.heading > right.heading ? 1 : 0)
  );
  return Object.freeze({
    ...registry,
    tracked_headings: Object.freeze(headings),
    roadmap_citations: Object.freeze(citations),
  });
}

function assembleLifecycle(
  reader: RevisionReader,
  campaign: CampaignDocumentV1,
  retired: RetiredIdsDocumentV1,
  prepared: Partial<Readonly<Record<RoadmapName, ValidatedRoadmapCore>>> = {},
): LifecycleRevisionInput {
  const roadmap = (name: RoadmapName): ValidatedRoadmapCore | undefined => {
    const authority = name === "matrix" ? campaign.campaign.matrix_authority : campaign.campaign.testing_authority;
    if (authority !== "legacy_markdown") return prepared[name] ?? prepareRoadmapCore(name, reader);
    if (readOptionalSource(reader, SOURCE_BY_ROADMAP[name]) === undefined) return undefined;
    failure([issue(
      "E-SCHEMA-STATE",
      SOURCE_BY_ROADMAP[name],
      "document",
      `${name} authority legacy_markdown forbids a roadmap TOML source`,
    )]);
  };
  const matrix = roadmap("matrix");
  const testing = roadmap("testing");
  const markdown = (name: RoadmapName, value: ValidatedRoadmapCore | undefined) => {
    const authority = name === "matrix" ? campaign.campaign.matrix_authority : campaign.campaign.testing_authority;
    if (authority === "authoritative") return value!.completed.expected_bytes;
    return authority === "shadow"
      ? readAndValidateShadowMarkdown(reader, name, value!)
      : readLegacyMarkdown(reader, name);
  };
  const roadmaps: LifecycleRevisionInput["roadmaps"] = {
    matrix: { markdown: markdown("matrix", matrix), ...(matrix === undefined ? {} : { document: matrix.document }) },
    testing: { markdown: markdown("testing", testing), ...(testing === undefined ? {} : { document: testing.document }) },
  };
  const legacyTitleBindings = campaign.legacy_markdown_reservations.flatMap((reservation) => {
    const name = reservation.id.startsWith("matrix.") ? "matrix" : "testing";
    const binding = validateLegacyTitleBinding(reservation, roadmaps[name].markdown);
    return binding === undefined ? [] : [binding];
  });
  const registry = registryWithRoadmapMarkdownFacts(reader.registry(), roadmaps);
  const campaignOutputStage = productionOutputStage(campaign);
  if (registry.production_output_stage !== campaignOutputStage) {
    failure([issue(
      "E-OUTPUT-CLAIM",
      "<output-registry>",
      "production_output_stage",
      `revision registry stage ${registry.production_output_stage} does not match campaign stage ${campaignOutputStage}`,
    )]);
  }
  const productionOutput = validateProductionOutputRegistry(
    registry.output_claims,
    registry.production_output_stage,
  );
  if (!productionOutput.ok) failure(productionOutput.issues);
  return {
    campaign,
    retired,
    roadmaps,
    registry,
    legacy_title_bindings: Object.freeze(legacyTitleBindings),
    debt: {
      ...(matrix === undefined ? {} : { matrix: matrix.debt }),
      ...(testing === undefined ? {} : { testing: testing.debt }),
    },
  };
}

function validateAllChange(against: FullCommitId, ports: ReadOnlyRoadmapPorts): readonly RoadmapIssue[] {
  const candidate = loadLifecycle(worktreeReader(ports));
  const baseReader = commitReader(ports, against);
  const baseLifecycle = loadActivatedLifecycle(baseReader);
  if (baseLifecycle !== undefined) {
    const base = baseLifecycle;
    return validateTransaction({ scope: "all", against, base, candidate }).issues;
  }
  const matrix = prepareRoadmapCore("matrix", baseReader);
  const testingAuthority = candidate.campaign === undefined
    ? undefined
    : candidate.campaign.campaign.testing_authority;
  const testing = testingAuthority === "shadow" ? prepareRoadmapCore("testing", baseReader) : undefined;
  if (testingAuthority === "legacy_markdown" && readOptionalSource(baseReader, TESTING_SOURCE) !== undefined) {
    failure([issue("E-SCHEMA-STATE", TESTING_SOURCE, "document", "testing bootstrap legacy authority forbids a roadmap TOML source")]);
  }
  const roadmaps: LifecycleRevisionInput["roadmaps"] = {
    matrix: { markdown: readAndValidateShadowMarkdown(baseReader, "matrix", matrix), document: matrix.document },
    testing: testing === undefined
      ? { markdown: readLegacyMarkdown(baseReader, "testing") }
      : { markdown: readAndValidateShadowMarkdown(baseReader, "testing", testing), document: testing.document },
  };
  const base: LifecycleRevisionInput = {
    roadmaps,
    registry: registryWithRoadmapMarkdownFacts(baseReader.registry(), roadmaps),
    debt: { matrix: matrix.debt, ...(testing === undefined ? {} : { testing: testing.debt }) },
  };
  return validateTransaction({ scope: "all", against, base, candidate, bootstrap: true }).issues;
}

function formatSource(path: RepoPath, ports: RoadmapWritePorts): RoadmapCliResult {
  const source = new Uint8Array(ports.readDeclared(path));
  strictSource(source, path);
  let document: RoadmapDocument | CampaignDocumentV1 | RetiredIdsDocumentV1;
  if (path === MATRIX_SOURCE) {
    document = decodeRoadmapSource(source, path, "matrix", false);
    const core = prepareDecodedRoadmapCore("matrix", document, ports.registryView({ kind: "worktree" }));
    const lifecycle = loadActivatedLifecycle(worktreeReader(ports), { matrix: core });
    if (lifecycle !== undefined) {
      const validated = validateLifecycleRevision(lifecycle);
      if (validated.issues.length > 0) failure(validated.issues);
    } else {
      validatePreActivationOutputStage(core.registry);
    }
  } else if (path === TESTING_SOURCE) {
    document = decodeRoadmapSource(source, path, "testing", false);
    const core = prepareDecodedRoadmapCore("testing", document, ports.registryView({ kind: "worktree" }));
    const lifecycle = loadActivatedLifecycle(worktreeReader(ports), { testing: core });
    if (lifecycle !== undefined) {
      const validated = validateLifecycleRevision(lifecycle);
      if (validated.issues.length > 0) failure(validated.issues);
    } else {
      validatePreActivationOutputStage(core.registry);
    }
  } else if (path === CAMPAIGN_SOURCE) {
    document = decodeCampaignSource(source, path, false);
    const retiredBytes = ports.readDeclared(RETIRED_SOURCE);
    const retired = decodeRetiredSource(retiredBytes, RETIRED_SOURCE, true);
    const validated = validateLifecycleRevision(assembleLifecycle(worktreeReader(ports), document, retired));
    if (validated.issues.length > 0) failure(validated.issues);
  } else if (path === RETIRED_SOURCE) {
    document = decodeRetiredSource(source, path, false);
    const campaignBytes = ports.readDeclared(CAMPAIGN_SOURCE);
    const campaign = decodeCampaignSource(campaignBytes, CAMPAIGN_SOURCE, true);
    const validated = validateLifecycleRevision(assembleLifecycle(worktreeReader(ports), campaign, document));
    if (validated.issues.length > 0) failure(validated.issues);
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
  const lifecycle = loadActivatedLifecycle(reader);
  if (lifecycle === undefined) {
    const prepared = finalizeRoadmap(prepareRoadmapCore(name, reader));
    validatePreActivationOutputStage(prepared.registry);
    const unavailable = createProjectionWritePlan({
      write_coordinate: "projection",
      roadmap: name,
      document: prepared.document,
      projection_bytes: prepared.projection,
      validation_issues: Object.freeze([]),
    });
    if (!unavailable.ok) failure(unavailable.issues);
    throw new Error("internal: pre-cutover roadmap unexpectedly minted a projection write plan");
  }
  const validatedLifecycle = validateLifecycleRevision(lifecycle);
  if (validatedLifecycle.issues.length > 0) failure(validatedLifecycle.issues);
  const prepared = preparedRoadmapsFromLifecycle(lifecycle).find((core) =>
    core.document.document.roadmap === name
  );
  if (prepared === undefined) {
    failure([issue(
      "E-OUTPUT-AUTHORITY",
      SOURCE_BY_ROADMAP[name],
      "document.authority",
      "projection write requires an activated structured roadmap",
    )]);
  }
  const finalized = finalizeRoadmap(prepared);
  const stage = productionOutputStage(lifecycle.campaign);
  const productionOutput = validateProductionOutputRegistry(prepared.registry.output_claims, stage);
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
