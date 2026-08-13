import {
  canonicalSemanticMarkdownFields,
  MATRIX_ADAPTER,
  MATRIX_GENERATED_SLOT_BINDINGS,
  validateMatrixRoadmapDocument,
} from "../adapters/matrix.ts";
import { composeRoadmapDocument } from "../compose.ts";
import {
  TESTING_ADAPTER,
  validateTestingRoadmapDocument,
} from "../adapters/testing.ts";
import type { FieldConsumer, RegistryView, RoadmapAdapter } from "../adapters/types.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import type { SchemaDecodeTrace } from "../decode/primitives.ts";
import type { IssueCollector, RoadmapIssue } from "../errors.ts";
import {
  buildRoadmapIndexes,
  type RoadmapIndexes,
  type SemanticPayloadProviderFact,
} from "../indexes.ts";
import { resolveManifest } from "../manifest.ts";
import type {
  FixtureRelativePath,
  ReferenceId,
  RepoPath,
  RoadmapId,
  RoadmapName,
  SlotId,
} from "../model/core.ts";
import type {
  GeneratedSlot,
  Reference,
  RoadmapDocument,
  RoadmapDocumentV0,
  RoadmapDocumentV1,
  SemanticPayload,
  SemanticRecord,
} from "../model/documents.ts";
import type { MatrixSemanticPayload, MatrixStatusInputs } from "../model/matrix.ts";
import { renderValidatedChunks } from "../render.ts";
import { buildExpectedChunks, validateCompletedChunks } from "../render_ir.ts";
import {
  collectReferenceProviders,
  createCoreReferenceProviders,
  deriveUnresolvedMigrationAuthority,
  validateCombinedRoadmapReferences,
  validateRoadmapReferences,
  validateSemanticRoadmapJoins,
  type SemanticJoinUniverse,
} from "../references.ts";
import { validateRelations } from "../relations.ts";
import type { SelfTestCandidateCase as SelfTestCase, SelfTestContext, SelfTestCandidateResult as SelfTestResult } from "../selftest.ts";
import {
  liveMatrixAuthoritativeDocument,
  liveMatrixCurrentLegacyProjection,
  liveMatrixLegacyProjection,
  liveMatrixProjection,
  liveMatrixShadowV0Document,
  liveMatrixShadowV0Source,
} from "./live_matrix.ts";
import {
  liveTestingAuthoritativeDocument,
  liveTestingShadowV0Document,
  liveTestingShadowV0Source,
} from "./live_testing.ts";

export const REQUIRED_ADAPTER_SELFTEST_CASE_IDS = [
  "decoder_domain_dispatch_once",
  "adapter_surface_has_no_decode_hook",
  "pipeline_indexes_before_adapter_validation",
  "indexes_created_from_decoded_document",
  "matrix_mixed_v1_preserves_inline_slots",
  "matrix_v0_reconstruction_visibility_arms",
  "testing_v0_reconstruction_visibility_arms",
] as const;

export type RequiredAdapterSelfTestCaseId =
  (typeof REQUIRED_ADAPTER_SELFTEST_CASE_IDS)[number];

export const ADAPTER_SELFTEST_SUBCASES = Object.freeze(["matrix", "testing"] as const);

const FIXTURE_ROOT = "cddl-matrix/roadmap/fixtures" as RepoPath;
const FIXTURE_PATHS = Object.freeze([
  "all-fields/matrix-v1.expected.md",
  "all-fields/matrix-v1.toml",
  "all-fields/matrix-v2.expected.md",
  "all-fields/matrix-v2.toml",
  "all-fields/testing-v1.expected.md",
  "all-fields/testing-v1.toml",
  "all-fields/testing-v2.expected.md",
  "all-fields/testing-v2.toml",
  "positive/mixed-matrix-v1.expected.md",
  "positive/mixed-matrix-v1.toml",
  "positive/mixed-testing-v1.expected.md",
  "positive/mixed-testing-v1.toml",
  "positive/small-matrix-v2.expected.md",
  "positive/small-matrix-v2.toml",
  "positive/small-testing-v2.expected.md",
  "positive/small-testing-v2.toml",
  "status-compat/roadmap.after.md",
  "status-compat/roadmap.before.md",
] as const);
type AdapterFixturePath = (typeof FIXTURE_PATHS)[number];

export interface AdapterFixtureBundle {
  readonly file_count: 18;
}

const fixtureFiles = new WeakMap<object, ReadonlyMap<AdapterFixturePath, Uint8Array>>();

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

function bytesEqual(left: Uint8Array, right: Uint8Array): boolean {
  if (left.byteLength !== right.byteLength) return false;
  for (let index = 0; index < left.byteLength; index++) if (left[index] !== right[index]) return false;
  return true;
}

function sha256(value: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(value).digest("hex");
}

function firstByteDifference(left: Uint8Array, right: Uint8Array): number {
  const shared = Math.min(left.byteLength, right.byteLength);
  for (let index = 0; index < shared; index++) if (left[index] !== right[index]) return index;
  return left.byteLength === right.byteLength ? -1 : shared;
}

function combineBytes(values: readonly Uint8Array[]): Uint8Array {
  const result = new Uint8Array(values.reduce((total, value) => total + value.byteLength, 0));
  let offset = 0;
  for (const value of values) {
    result.set(value, offset);
    offset += value.byteLength;
  }
  return result;
}

export function createAdapterFixtureBundle(
  files: ReadonlyMap<AdapterFixturePath, Uint8Array>,
): AdapterFixtureBundle {
  assert(files.size === FIXTURE_PATHS.length, "adapter fixture bundle must contain exactly eighteen files");
  const snapshots = new Map<AdapterFixturePath, Uint8Array>();
  for (const path of FIXTURE_PATHS) {
    const value = files.get(path);
    assert(value !== undefined && value.byteLength > 0, `adapter fixture bundle is missing ${path}`);
    snapshots.set(path, new Uint8Array(value));
  }
  const bundle: AdapterFixtureBundle = Object.freeze({ file_count: 18 });
  fixtureFiles.set(bundle, snapshots);
  return bundle;
}

function fixtureBytes(bundle: AdapterFixtureBundle, path: AdapterFixturePath): Uint8Array {
  const value = fixtureFiles.get(bundle)?.get(path);
  assert(value !== undefined, `adapter fixture bytes are unavailable for ${path}`);
  return new Uint8Array(value);
}

function fixtureBundleFromContext(context: SelfTestContext): AdapterFixtureBundle {
  const inventory = context.ports.fixtures.enumerateFixtureFiles(FIXTURE_ROOT);
  assert(
    JSON.stringify(inventory) === JSON.stringify([...inventory].sort(codePointSort)),
    "adapter fixture inventory must be code-point sorted",
  );
  assert(new Set(inventory).size === inventory.length, "adapter fixture inventory contains a duplicate");
  const selected = inventory.filter((path) => FIXTURE_PATHS.some((expected) => expected === path));
  assert(
    JSON.stringify(selected) === JSON.stringify(FIXTURE_PATHS),
    `adapter fixture inventory differs: ${JSON.stringify(selected)}`,
  );
  const files = new Map<AdapterFixturePath, Uint8Array>();
  for (const path of FIXTURE_PATHS) {
    const authorized = selected.find((candidate) => candidate === path) as FixtureRelativePath | undefined;
    assert(authorized !== undefined, `missing authorized adapter fixture ${path}`);
    files.set(path, context.ports.fixtures.readFixtureFile(FIXTURE_ROOT, authorized));
  }
  return createAdapterFixtureBundle(files);
}

function decoded(
  bundle: AdapterFixtureBundle,
  path: AdapterFixturePath,
  roadmap: RoadmapName,
  trace?: SchemaDecodeTrace,
): RoadmapDocument {
  return decodeRoadmapSource(fixtureBytes(bundle, path), path, roadmap, true, trace);
}

class Collector implements IssueCollector {
  readonly mutable: RoadmapIssue[] = [];
  get issues(): readonly RoadmapIssue[] {
    return this.mutable;
  }
  add(issue: RoadmapIssue): void {
    this.mutable.push(issue);
  }
}

function semanticPayload(record: RoadmapDocumentV1["records"][number]): SemanticPayload | undefined {
  return record.render_authority === "semantic" ? record.payload : record.semantic_shadow;
}

function productionDocument(document: RoadmapDocumentV1): RoadmapDocumentV1 {
  const matrix = document.document.roadmap === "matrix";
  return {
    ...document,
    document: {
      ...document.document,
      source_path: (matrix ? "cddl-matrix/roadmap.toml" : "tests/testing-roadmap.toml") as RepoPath,
      projection_path: (matrix ? "cddl-matrix/ROADMAP.md" : "tests/TESTING_ROADMAP.md") as RepoPath,
    },
  };
}

function replacePayload(
  document: RoadmapDocumentV1,
  select: (payload: SemanticPayload) => boolean,
  mutate: (payload: SemanticPayload) => SemanticPayload,
): { readonly document: RoadmapDocumentV1; readonly payload: SemanticPayload } {
  let replacement: SemanticPayload | undefined;
  const records = document.records.map((record) => {
    const payload = semanticPayload(record);
    if (replacement !== undefined || payload === undefined || !select(payload)) return record;
    replacement = mutate(payload);
    return record.render_authority === "semantic"
      ? { ...record, payload: replacement }
      : { ...record, semantic_shadow: replacement };
  });
  assert(replacement !== undefined, "payload mutation selector matched no decoded record");
  return { document: { ...document, records }, payload: replacement };
}

function fieldSpy(): {
  readonly consumer: FieldConsumer;
  readonly calls: readonly { path: string; bytes: Uint8Array }[];
} {
  const calls: { path: string; bytes: Uint8Array }[] = [];
  return {
    consumer: {
      consume(path, value) {
        calls.push({ path, bytes: value });
        return new Uint8Array(value);
      },
    },
    calls,
  };
}

function statusCompatibilityInputs(): MatrixStatusInputs {
  const numbered = (prefix: string, count: number, start = 1): readonly string[] =>
    Array.from({ length: count }, (_, index) => `${prefix}-${String(index + start).padStart(2, "0")}`);
  const rfc8610 = numbered("feature-rfc8610", 30);
  const rfc9682 = numbered("feature-rfc9682", 30);
  const cddlCodegen = numbered("feature-cddl-codegen", 30);
  return {
    matrix: {
      annotations: [
        { id: rfc8610[0]!, status: "supported" },
        ...numbered("annotation", 78, 2).map((id) => ({ id, status: "supported" as const })),
        {
          id: "annotation-divergent",
          status: "supported",
          emission: { preserve: { status: "unsupported" } },
        },
      ],
      features: [
        ...rfc8610.map((id) => ({ id, profile: "RFC8610" as const })),
        ...rfc9682.map((id) => ({ id, profile: "RFC9682" as const })),
        ...cddlCodegen.map((id) => ({ id, profile: "CDDL_CODEGEN" as const })),
      ],
      containment_ids: numbered("containment", 60),
      control_operator_ids: numbered("control", 30),
    },
    catalog: { rows: [{
      id: rfc8610[0]!,
      vectors: [{ expect: "reject", class: "constraint" }],
    }] },
    registry: { gates: [
      { id: "fixture-gate-0", kind: "cargo", ignored_test: "manual_gate_a" },
      { id: "fixture-gate-1", kind: "cargo", ignored_test: "manual_gate_b" },
    ] },
    timings: { tiers: [
      { tier: "fast", wall_ms: 1000 },
      { tier: "local", wall_ms: 60000 },
      { tier: "full", wall_ms: 120000 },
    ] },
  };
}

function allFieldsStatusInputs(): MatrixStatusInputs {
  const features = [
    ...Array.from({ length: 95 }, (_, index) => ({ id: `r-${index}`, profile: "RFC8610" })),
    { id: "rfc9682", profile: "RFC9682" },
    ...Array.from({ length: 27 }, (_, index) => ({ id: `c-${index}`, profile: "CDDL_CODEGEN" })),
  ];
  const annotations = Array.from({ length: 293 }, (_, index) => ({
    id: index < 20 ? `row-${index}` : `annotation-${index}`,
    status: "supported",
    ...(index < 6 ? { emission: { preserve: { status: "unsupported" } } } : {}),
  }));
  return {
    matrix: {
      annotations,
      features,
      containment_ids: Array.from({ length: 136 }, (_, index) => `containment-${index}`),
      control_operator_ids: Array.from({ length: 37 }, (_, index) => `control-${index}`),
    },
    catalog: { rows: Array.from({ length: 20 }, (_, index) => ({
      id: `row-${index}`,
      vectors: Array.from({ length: index === 19 ? 1 : 5 }, () => ({ expect: "reject", class: "constraint" })),
    })) },
    registry: { gates: [{ id: "gate", kind: "cargo", ignored_test: "manual" }] },
    timings: { tiers: [
      { tier: "fast", wall_ms: 1000 },
      { tier: "local", wall_ms: 2000 },
      { tier: "full", wall_ms: 3000 },
    ] },
  };
}

function liveMatrixStatusInputs(): MatrixStatusInputs {
  const features = [
    ...Array.from({ length: 95 }, (_, index) => ({ id: `r-${index}`, profile: "RFC8610" })),
    { id: "rfc9682", profile: "RFC9682" },
    ...Array.from({ length: 27 }, (_, index) => ({ id: `c-${index}`, profile: "CDDL_CODEGEN" })),
  ];
  const annotations = Array.from({ length: 301 }, (_, index) => ({
    id: index < 93 ? `row-${index}` : `annotation-${index}`,
    status: "supported",
    ...(index === 0 ? { emission: { preserve: { status: "unsupported" } } } : {}),
  }));
  return {
    matrix: {
      annotations,
      features,
      containment_ids: Array.from({ length: 144 }, (_, index) => `containment-${index}`),
      control_operator_ids: Array.from({ length: 37 }, (_, index) => `control-${index}`),
    },
    catalog: { rows: Array.from({ length: 93 }, (_, index) => ({
      id: `row-${index}`,
      vectors: Array.from({ length: index < 22 ? 2 : 1 }, () => ({
        expect: "reject",
        class: "constraint",
      })),
    })) },
    registry: { gates: Array.from({ length: 20 }, (_, index) => ({
      id: `gate-${index}`,
      kind: "cargo",
      ignored_test: `manual-${index}`,
    })) },
    timings: { tiers: [
      { tier: "fast", wall_ms: 1000 },
      { tier: "local", wall_ms: 2000 },
      { tier: "full", wall_ms: 3000 },
    ] },
  };
}

function registryView(
  bundle: AdapterFixtureBundle,
  document?: RoadmapDocument,
  statusInputs: MatrixStatusInputs = statusCompatibilityInputs(),
): RegistryView {
  const references = document !== undefined && "references" in document ? document.references : [];
  const unique = <T>(values: readonly T[], key: (value: T) => string): readonly T[] =>
    [...new Map(values.map((value) => [key(value), value])).values()];
  return {
    revision: { kind: "worktree" },
    production_output_stage: "pre_cutover",
    gates: unique(references.filter((entry) => entry.kind === "gate").map((entry) => ({ id: entry.gate_id, kind: "cargo", stub: false })), (entry) => entry.id),
    matrix_features: unique(references.filter((entry) => entry.kind === "matrix_feature").map((entry) => ({ id: entry.feature_id })), (entry) => entry.id),
    matrix_roles: unique(references.filter((entry) => entry.kind === "matrix_role").map((entry) => ({ id: entry.role_id })), (entry) => entry.id),
    matrix_cells: unique(references.filter((entry) => entry.kind === "matrix_cell").map((entry) => ({ id: entry.cell_id })), (entry) => entry.id),
    tracked_headings: unique(references.filter((entry) => entry.kind === "file_heading").map((entry) => ({
      path: entry.path,
      heading: entry.heading,
      span: { start_byte: 0, end_byte: 1 },
    })), (entry) => JSON.stringify([entry.path, entry.heading])),
    test_symbols: unique(references.filter((entry) => entry.kind === "test_symbol").map((entry) => ({
      test_id: entry.test_id,
      symbol: entry.symbol,
      source: "src/tests/fixture.rs" as RepoPath,
      span: { start_byte: 0, end_byte: 1 },
      module_path: ["tests", "fixture"],
    })), (entry) => JSON.stringify([entry.test_id, entry.symbol])),
    roadmap_citations: [],
    current_guards: document?.records.some((record) =>
        "payload" in record && record.payload.kind === "evidence" &&
        record.id.startsWith("matrix.evidence.fixed-value-choice-member.")
      )
      ? ["bool", "bytes", "float", "nint", "null", "text", "uint", "undefined"].map((value) => ({
        id: `matrix.fixed-value-choice-member.coordinate-${value}` as RoadmapId,
        guard_role: "family_cell" as const,
        family_root_id: "matrix.systematic.fixed-value-choice-member" as RoadmapId,
        owner_registry: "fixture-fixed-value-closure",
        replacement_pin: {
          kind: "gate" as const,
          gate_id: "roadmap_projection_check",
          claim_md: new Uint8Array(),
        },
      }))
      : [],
    output_claims: [],
    matrix_status_inputs: statusInputs,
  };
}

function requireSemantic(
  document: RoadmapDocument,
  predicate: (payload: SemanticPayload) => boolean,
): SemanticRecord {
  assert(
    document.document.schema_version === 1 || document.document.schema_version === 2,
    "adapter fixture must carry semantic authority",
  );
  const record = document.records.find((candidate) =>
    "render_authority" in candidate &&
    candidate.render_authority === "semantic" && predicate(candidate.payload)
  );
  assert(
    record !== undefined && "render_authority" in record && record.render_authority === "semantic",
    "semantic adapter record is missing",
  );
  return record;
}

function testDecoderDispatch(bundle: AdapterFixtureBundle): void {
  const matrixRows: string[] = [];
  const decodedMatrix = decoded(bundle, "all-fields/matrix-v1.toml", "matrix", {
    exactTable(schema, path) { if (schema.name === "matrix maintenance policy") matrixRows.push(path); },
    enum() {},
  });
  assert(JSON.stringify(matrixRows) === JSON.stringify(["record[24].semantic_shadow"]), "matrix S1 domain arm did not dispatch exactly once at its frozen logical path");
  assert(decodedMatrix.document.schema_version === 1, "matrix all-fields fixture is not v1");
  const matrix = productionDocument(decodedMatrix as RoadmapDocumentV1);
  const matrixPayload = semanticPayload(matrix.records.find((record) => record.id === "matrix.fixture-policy-a" as RoadmapId)!);
  assert(matrixPayload?.kind === "matrix_policy", "matrix S1 branded shadow payload is absent");
  const matrixCallbacks: SemanticPayloadProviderFact[] = [];
  const matrixResult = validateMatrixRoadmapDocument(
    matrix,
    registryView(bundle, matrix, allFieldsStatusInputs()),
    {
      unresolved_migration_authority: validationAuthority(matrix),
      observer: {
        sharedValidationStarted() {},
        domainPayloadValidated(provider) {
          if (provider.payload === matrixPayload) matrixCallbacks.push(provider);
        },
      },
    },
  );
  assert(matrixResult.issues.length === 0, `matrix production wrapper rejected the decoder dispatch fixture: ${JSON.stringify(matrixResult.issues)}`);
  assert(matrixCallbacks.length === 1 && matrixCallbacks[0]?.payload === matrixPayload && matrixCallbacks[0]?.authority === "semantic_shadow", "matrix production domain callback did not receive the exact S1 branded payload object once");

  const testingRows: string[] = [];
  const decodedTesting = decoded(bundle, "all-fields/testing-v1.toml", "testing", {
    exactTable(schema, path) { if (schema.name === "watching operational watch") testingRows.push(path); },
    enum() {},
  });
  assert(JSON.stringify(testingRows) === JSON.stringify(["record[13].semantic_shadow"]), "testing S1 domain arm did not dispatch exactly once at its frozen logical path");
  assert(decodedTesting.document.schema_version === 1, "testing all-fields fixture is not v1");
  const testing = productionDocument(decodedTesting as RoadmapDocumentV1);
  const testingPayload = semanticPayload(testing.records.find((record) => record.id === "testing.fixture-operational-watching" as RoadmapId)!);
  assert(testingPayload?.kind === "testing_operational_watch", "testing S1 branded shadow payload is absent");
  const testingCallbacks: SemanticPayloadProviderFact[] = [];
  const testingResult = validateTestingRoadmapDocument(testing, registryView(bundle, testing), {
    unresolved_migration_authority: validationAuthority(testing),
    observer: {
      sharedValidationStarted() {},
      domainPayloadValidated(provider) {
        if (provider.payload === testingPayload) testingCallbacks.push(provider);
      },
    },
  });
  assert(testingResult.issues.length === 0, `testing production wrapper rejected the decoder dispatch fixture: ${JSON.stringify(testingResult.issues)}`);
  assert(testingCallbacks.length === 1 && testingCallbacks[0]?.payload === testingPayload && testingCallbacks[0]?.authority === "semantic_shadow", "testing production domain callback did not receive the exact S1 branded payload object once");
}

function expectThrows(action: () => void, message: string): void {
  let threw = false;
  try { action(); } catch { threw = true; }
  assert(threw, message);
}

function validateFloors(adapter: RoadmapAdapter<SemanticPayload>, document: RoadmapDocument): readonly RoadmapIssue[] {
  const collector = new Collector();
  adapter.validateFloors(document, collector);
  return collector.issues;
}

function requireFloorIssue(
  adapter: RoadmapAdapter<SemanticPayload>,
  document: RoadmapDocument,
  logicalPath: string,
  message: string,
): void {
  assert(validateFloors(adapter, document).some((entry) => entry.logical_path === logicalPath), message);
}

function testFloors(bundle: AdapterFixtureBundle): void {
  const matrix = productionDocument(decoded(bundle, "all-fields/matrix-v1.toml", "matrix") as RoadmapDocumentV1);
  const testing = productionDocument(decoded(bundle, "all-fields/testing-v1.toml", "testing") as RoadmapDocumentV1);
  assert(validateFloors(MATRIX_ADAPTER, matrix).length === 0, "exact production matrix floors failed");
  assert(validateFloors(TESTING_ADAPTER, testing).length === 0, "exact production testing floors failed");

  for (const [adapter, document, otherRoadmap, source, projection] of [
    [MATRIX_ADAPTER, matrix, "testing", "wrong/matrix.toml", "wrong/MATRIX.md"],
    [TESTING_ADAPTER, testing, "matrix", "wrong/testing.toml", "wrong/TESTING.md"],
  ] as const) {
    requireFloorIssue(adapter, { ...document, document: { ...document.document, roadmap: otherRoadmap } }, "document.roadmap", `${adapter.roadmap} accepted the wrong roadmap floor`);
    requireFloorIssue(adapter, { ...document, document: { ...document.document, source_path: source as RepoPath } }, "document.source_path", `${adapter.roadmap} accepted the wrong source floor`);
    requireFloorIssue(adapter, { ...document, document: { ...document.document, projection_path: projection as RepoPath } }, "document.projection_path", `${adapter.roadmap} accepted the wrong projection floor`);
  }

  requireFloorIssue(MATRIX_ADAPTER, { ...matrix, generated_slots: matrix.generated_slots.slice(1) }, "generated_slot", "matrix accepted a missing slot");
  requireFloorIssue(MATRIX_ADAPTER, { ...matrix, generated_slots: [...matrix.generated_slots, { ...matrix.generated_slots[0]!, slot_id: "extra" as SlotId }] }, "generated_slot", "matrix accepted an extra slot");
  requireFloorIssue(MATRIX_ADAPTER, { ...matrix, generated_slots: matrix.generated_slots.map((slot, index) => index === 0 ? { ...slot, slot_id: "wrong" as SlotId } : slot) }, `generated_slot["constraint"].binding`, "matrix accepted a wrong slot ID");
  requireFloorIssue(MATRIX_ADAPTER, { ...matrix, generated_slots: matrix.generated_slots.map((slot, index) => index === 0 ? { ...slot, binding: "wrong:binding" } : slot) }, `generated_slot["constraint"].binding`, "matrix accepted a wrong slot binding");
  requireFloorIssue(TESTING_ADAPTER, { ...testing, generated_slots: [{ slot_id: "forbidden" as SlotId, binding: "wrong", span_ids: [] }] }, "generated_slot", "testing accepted a generated slot");
}

function testProviders(bundle: AdapterFixtureBundle): void {
  const matrix = productionDocument(decoded(bundle, "all-fields/matrix-v1.toml", "matrix") as RoadmapDocumentV1);
  const built = buildRoadmapIndexes(matrix);
  assert(built.issues.length === 0, `matrix provider fixture failed C4A: ${JSON.stringify(built.issues)}`);
  const authority = deriveUnresolvedMigrationAuthority(built.indexes);
  assert(authority.issues.length === 0 && authority.authority !== undefined, "matrix unresolved-migration authority was not derivable");
  const view = registryView(bundle, matrix);
  const providers = MATRIX_ADAPTER.referenceProviders(view);
  assert(providers.map((provider) => provider.kind).join("|") === "matrix_cell|matrix_feature|matrix_role", "matrix provider order is not code-point deterministic");
  assert(TESTING_ADAPTER.referenceProviders(view).length === 0, "testing domain provider list must be explicitly empty");

  const core = createCoreReferenceProviders(built.indexes.first_class, authority.authority);
  const allKinds = [...core, ...providers].map((provider) => provider.kind).sort(codePointSort);
  assert(allKinds.length === 13 && new Set(allKinds).size === 13, "core plus matrix adapters do not provide exactly one of all 13 reference kinds");
  assert(collectReferenceProviders([...core, ...providers]).issues.length === 0, "valid provider composition was rejected");
  assert(validateRoadmapReferences(built.indexes, view, {
    providers,
    first_class: built.indexes.first_class,
    unresolved_migration_authority: authority.authority,
  }).length === 0, "actual reference validation rejected the exact provider composition");
  for (const provider of providers) {
    const missing = providers.filter((candidate) => candidate !== provider);
    const duplicate = [...providers, provider];
    assert(validateRoadmapReferences(built.indexes, view, { providers: missing, unresolved_migration_authority: authority.authority }).some((entry) => entry.logical_path === `reference-provider.${provider.kind}`), `${provider.kind} provider zero-cardinality mutation passed`);
    assert(validateRoadmapReferences(built.indexes, view, { providers: duplicate, unresolved_migration_authority: authority.authority }).some((entry) => entry.logical_path === `reference-provider.${provider.kind}`), `${provider.kind} provider duplicate mutation passed`);
  }

  const source = "matrix.fixture-provider-source" as RoadmapId;
  const existingFeature = view.matrix_features[0]?.id;
  const existingRole = view.matrix_roles[0]?.id;
  const existingCell = view.matrix_cells[0]?.id;
  assert(existingFeature !== undefined && existingRole !== undefined && existingCell !== undefined, "provider wrong-kind vectors require existing members in all three matrix universes");
  const cases: readonly {
    kind: "matrix_feature" | "matrix_role" | "matrix_cell";
    reference: Reference;
    wrong_reference: Reference;
    one: RegistryView;
    duplicate: RegistryView;
  }[] = [
    {
      kind: "matrix_feature",
      reference: { id: "ref-provider-feature" as ReferenceId, source, kind: "matrix_feature", feature_id: "feature-a" },
      wrong_reference: { id: "ref-provider-feature-wrong" as ReferenceId, source, kind: "matrix_role", role_id: existingRole },
      one: { ...view, matrix_features: [{ id: "feature-a" }] },
      duplicate: { ...view, matrix_features: [{ id: "feature-a" }, { id: "feature-a" }] },
    },
    {
      kind: "matrix_role",
      reference: { id: "ref-provider-role" as ReferenceId, source, kind: "matrix_role", role_id: "role-a" },
      wrong_reference: { id: "ref-provider-role-wrong" as ReferenceId, source, kind: "matrix_cell", cell_id: existingCell },
      one: { ...view, matrix_roles: [{ id: "role-a" }] },
      duplicate: { ...view, matrix_roles: [{ id: "role-a" }, { id: "role-a" }] },
    },
    {
      kind: "matrix_cell",
      reference: { id: "ref-provider-cell" as ReferenceId, source, kind: "matrix_cell", cell_id: "cell-a" },
      wrong_reference: { id: "ref-provider-cell-wrong" as ReferenceId, source, kind: "matrix_feature", feature_id: existingFeature },
      one: { ...view, matrix_cells: [{ id: "cell-a" }] },
      duplicate: { ...view, matrix_cells: [{ id: "cell-a" }, { id: "cell-a" }] },
    },
  ];
  for (const vector of cases) {
    const provider = providers.find((candidate) => candidate.kind === vector.kind);
    assert(provider !== undefined, `${vector.kind} provider is absent`);
    const resolve = provider.resolve as (reference: Reference, registry: RegistryView) => { resolved: boolean };
    assert(resolve(vector.reference, vector.one).resolved, `${vector.kind} rejected exact one membership`);
    assert(!resolve(vector.reference, view).resolved, `${vector.kind} accepted zero membership`);
    assert(!resolve(vector.reference, vector.duplicate).resolved, `${vector.kind} accepted duplicate membership`);
    assert(!resolve(vector.wrong_reference, vector.one).resolved, `${vector.kind} accepted an existing member from another provider universe through wrong-kind dispatch`);
  }

  const controlId = built.indexes.id_providers[0]!.owner_record_id;
  const cellReference: Reference = {
    id: "control-test-cell" as ReferenceId,
    source: controlId,
    kind: "matrix_cell",
    cell_id: "control-test-cell-fixture",
  };
  const symbolReference: Reference = {
    id: "control-test-symbol" as ReferenceId,
    source: controlId,
    kind: "test_symbol",
    test_id: "rust-test:cddl-codegen#tests::fixture::control_test",
    symbol: "tests::fixture::control_test",
  };
  const gateReference: Reference = {
    id: "control-test-gate" as ReferenceId,
    source: controlId,
    kind: "gate",
    gate_id: "control-test-gate-fixture",
  };
  const controlPath = `record[${JSON.stringify(controlId)}].semantic_shadow`;
  const controlProvider: SemanticPayloadProviderFact = {
    record: built.indexes.record_nodes.get(controlId)!,
    authority: "semantic_shadow",
    logical_path: controlPath,
    payload: {
      kind: "control",
      summary_md: new TextEncoder().encode("Mixed direct test control."),
      control_kind: "test",
      control_state: "live",
      reference_ids: [cellReference.id, symbolReference.id],
      claim_md: new TextEncoder().encode("Exact matrix cells and their direct test are owned together."),
      boundary_md: new TextEncoder().encode("No unrelated provider kind is admitted."),
    },
  };
  const mixedControl = {
    ...built.indexes,
    references: new Map<ReferenceId, Reference>([
      [cellReference.id, cellReference],
      [symbolReference.id, symbolReference],
    ]),
    reference_id_uses: [cellReference, symbolReference].map((reference) => ({
      id: reference.id,
      logical_path: `${controlPath}.reference_ids`,
    })),
    payload_records: new Map([...built.indexes.payload_records, [controlId, controlProvider]]),
  } as RoadmapIndexes;
  const mixedView: RegistryView = {
    ...view,
    matrix_cells: [...view.matrix_cells, { id: cellReference.cell_id }],
    test_symbols: [...view.test_symbols, {
      test_id: symbolReference.test_id,
      symbol: symbolReference.symbol,
      source: "src/tests/fixture.rs" as RepoPath,
      span: { start_byte: 0, end_byte: 1 },
      module_path: ["tests", "fixture"],
    }],
    gates: [...view.gates, { id: gateReference.gate_id, kind: "cargo", stub: false }],
  };
  assert(validateRoadmapReferences(mixedControl, mixedView, { providers }).length === 0,
    "control_kind=test rejected its canonical matrix_cell + test_symbol ownership tuple");
  const unrelated = {
    ...mixedControl,
    references: new Map([...mixedControl.references, [gateReference.id, gateReference]]),
    reference_id_uses: [...mixedControl.reference_id_uses, {
      id: gateReference.id,
      logical_path: `${controlPath}.reference_ids`,
    }],
  } as RoadmapIndexes;
  assert(validateRoadmapReferences(unrelated, mixedView, { providers }).some((entry) =>
    entry.code === "E-REFERENCE-FORBIDDEN" && entry.logical_path === `${controlPath}.reference_ids`
  ), "control_kind=test admitted an unrelated gate reference");
}

function testCrossRoadmapJoinSubstrate(bundle: AdapterFixtureBundle): void {
  const matrix = productionDocument(decoded(bundle, "all-fields/matrix-v1.toml", "matrix") as RoadmapDocumentV1);
  const testing = productionDocument(decoded(bundle, "all-fields/testing-v1.toml", "testing") as RoadmapDocumentV1);
  const matrixBuilt = buildRoadmapIndexes(matrix).indexes;
  const testingBuilt = buildRoadmapIndexes(testing).indexes;
  const source = [...matrixBuilt.payload_records.values()].find((provider) => provider.payload.kind === "work");
  const target = [...testingBuilt.payload_records.values()].find((provider) => provider.payload.kind === "evidence");
  assert(source !== undefined && target !== undefined, "cross-roadmap fixture lacks work/evidence endpoints");
  const foreignReference: Reference = {
    id: "cross-roadmap" as ReferenceId,
    source: source.record.id,
    kind: "roadmap",
    target_id: target.record.id,
  };
  const relation = { source: source.record.id, kind: "delegates_to" as const, target: target.record.id };
  const sourcePath = `${source.logical_path}.evidence_ids`;
  const scoped = {
    ...matrixBuilt,
    id_uses: [...matrixBuilt.id_uses,
      { id: target.record.id, logical_path: sourcePath, role: "semantic_target" as const },
      { id: target.record.id, logical_path: "relation[0].target", role: "relation_target" as const },
      { id: target.record.id, logical_path: `reference[${JSON.stringify(foreignReference.id)}].target_id`, role: "reference_target" as const },
    ],
    relations: [...matrixBuilt.relations, relation],
    references: new Map([[foreignReference.id, foreignReference]]),
    reference_id_uses: [],
  } as RoadmapIndexes;
  const combined: SemanticJoinUniverse = {
    first_class: new Map([...matrixBuilt.first_class, ...testingBuilt.first_class]),
    payload_records: new Map([...matrixBuilt.payload_records, ...testingBuilt.payload_records]),
  };
  const scopedSemanticIssues = validateSemanticRoadmapJoins(scoped, scoped, "<scoped>", "matrix");
  assert(scopedSemanticIssues.length === 0,
    `scoped lane rejected an explicitly opposite-namespace semantic join: ${JSON.stringify(scopedSemanticIssues)}`);
  assert(validateRelations(scoped.relations, scoped.first_class, "<scoped>", "matrix").length === 0,
    "scoped lane rejected an explicitly opposite-namespace relation");
  assert(validateRoadmapReferences(scoped, registryView(bundle, matrix), {
    providers: MATRIX_ADAPTER.referenceProviders(registryView(bundle, matrix)),
    defer_foreign_roadmap_joins: "matrix",
  }).length === 0, "scoped lane rejected an explicitly opposite-namespace roadmap reference");
  assert(validateSemanticRoadmapJoins(scoped, combined, "<combined>").length === 0 &&
    validateRelations(scoped.relations, combined.first_class, "<combined>").length === 0 &&
    validateCombinedRoadmapReferences(scoped, combined.first_class, "<combined>").length === 0 &&
    validateRoadmapReferences(scoped, registryView(bundle, matrix), {
      providers: MATRIX_ADAPTER.referenceProviders(registryView(bundle, matrix)),
      first_class: combined.first_class,
    }).length === 0, "combined cross-roadmap universe did not resolve the foreign semantic/relation/reference tuple");
  const absentCombined: SemanticJoinUniverse = {
    first_class: matrixBuilt.first_class,
    payload_records: matrixBuilt.payload_records,
  };
  assert(validateSemanticRoadmapJoins(scoped, absentCombined, "<combined-missing>").some((entry) =>
    entry.code === "E-REFERENCE-UNRESOLVED" && entry.logical_path === sourcePath
  ), "combined cross-roadmap universe accepted a missing foreign semantic target");
  assert(validateRelations(scoped.relations, absentCombined.first_class, "<combined-missing>").some((entry) =>
    entry.code === "E-RELATION-ENDPOINT" && entry.logical_path.endsWith(".target")
  ), "combined cross-roadmap universe accepted a missing foreign relation target");
  assert(validateRoadmapReferences(scoped, registryView(bundle, matrix), {
    providers: MATRIX_ADAPTER.referenceProviders(registryView(bundle, matrix)),
    first_class: absentCombined.first_class,
  }).some((entry) => entry.code === "E-REFERENCE-UNRESOLVED"),
  "combined cross-roadmap universe accepted a missing foreign roadmap reference target");
  assert(validateCombinedRoadmapReferences(scoped, absentCombined.first_class, "<combined-missing>").some((entry) =>
    entry.code === "E-REFERENCE-UNRESOLVED"
  ), "production combined reference seam accepted a missing foreign roadmap target");
  const sameLaneMissing = "matrix.fixture-same-lane-missing" as RoadmapId;
  const sameLane = { ...scoped, id_uses: [{ id: sameLaneMissing, logical_path: sourcePath, role: "semantic_target" }] } as RoadmapIndexes;
  assert(validateSemanticRoadmapJoins(sameLane, sameLane, "<scoped>", "matrix").some((entry) =>
    entry.code === "E-REFERENCE-UNRESOLVED"
  ), "scoped deferral admitted a missing same-lane target");
}

function statusLine(bytes: Uint8Array): Uint8Array {
  const result = new Uint8Array(bytes.byteLength + 1);
  result.set(bytes);
  result[result.byteLength - 1] = 0x0a;
  return result;
}

function markerInterior(source: Uint8Array, markerId: string): Uint8Array {
  const text = new TextDecoder("utf-8", { fatal: true }).decode(source);
  const open = `<!-- gen:sh:${markerId} -->`;
  const close = `<!-- /gen:sh:${markerId} -->`;
  const openAt = text.indexOf(open);
  const contentAt = openAt + open.length;
  const closeAt = text.indexOf(close, contentAt);
  assert(openAt >= 0 && text.indexOf(open, contentAt) < 0, `committed status fixture must contain exactly one ${markerId} open marker`);
  assert(closeAt >= contentAt && text.indexOf(close, closeAt + close.length) < 0, `committed status fixture must contain exactly one ${markerId} close marker`);
  return new TextEncoder().encode(text.slice(contentAt, closeAt));
}

function replaceMarkerInterior(source: string, markerId: string, interior: string): string {
  const open = `<!-- gen:sh:${markerId} -->`;
  const close = `<!-- /gen:sh:${markerId} -->`;
  const start = source.indexOf(open);
  const contentStart = start + open.length;
  const end = source.indexOf(close, contentStart);
  assert(start >= 0 && end >= contentStart, `status before fixture is missing ${markerId}`);
  return `${source.slice(0, contentStart)}${interior}${source.slice(end)}`;
}

function testSlots(bundle: AdapterFixtureBundle): void {
  const view = registryView(bundle);
  const matrixDocument = decoded(bundle, "all-fields/matrix-v1.toml", "matrix");
  const testingDocument = decoded(bundle, "all-fields/testing-v1.toml", "testing");
  const beforeBytes = fixtureBytes(bundle, "status-compat/roadmap.before.md");
  const afterBytes = fixtureBytes(bundle, "status-compat/roadmap.after.md");
  const expected = new Map(MATRIX_GENERATED_SLOT_BINDINGS.map(([slotId, binding]) => [
    binding,
    statusLine(markerInterior(afterBytes, `roadmap-${slotId}`)),
  ]));
  for (const [slotId] of MATRIX_GENERATED_SLOT_BINDINGS) {
    assert(!bytesEqual(markerInterior(beforeBytes, `roadmap-${slotId}`), markerInterior(afterBytes, `roadmap-${slotId}`)), `status before/after oracle does not independently change ${slotId}`);
  }
  const resolvers = MATRIX_ADAPTER.slotResolvers(view, matrixDocument);
  assert([...resolvers.keys()].join("|") === MATRIX_GENERATED_SLOT_BINDINGS.map(([id]) => id).join("|"), "matrix adapter does not expose the four exact deterministic slot resolvers");
  let projected = new TextDecoder("utf-8", { fatal: true }).decode(beforeBytes);
  for (const [index, [slotId, binding]] of MATRIX_GENERATED_SLOT_BINDINGS.entries()) {
    const resolver = resolvers.get(slotId);
    assert(resolver !== undefined, `missing resolver ${slotId}`);
    const slot: GeneratedSlot = { slot_id: slotId, binding, span_ids: [] };
    const first = resolver.resolve(slot, view);
    const second = resolver.resolve(slot, view);
    const wanted = expected.get(binding);
    assert(wanted !== undefined && bytesEqual(first.bytes, wanted), `resolver ${slotId} did not return exact committed compatibility bytes`);
    assert(first.binding === binding && first.bytes.byteLength > 0, `resolver ${slotId} returned a vacuous or wrong binding`);
    assert(first.bytes !== second.bytes && bytesEqual(first.bytes, second.bytes), `resolver ${slotId} did not clone its payload`);
    first.bytes[0] = first.bytes[0]! ^ 0xff;
    assert(bytesEqual(second.bytes, wanted), `resolver ${slotId} leaked caller mutation into later output`);
    assert(second.bytes[second.bytes.byteLength - 1] === 0x0a, `resolver ${slotId} does not own its complete projection line`);
    projected = replaceMarkerInterior(
      projected,
      `roadmap-${slotId}`,
      new TextDecoder("utf-8", { fatal: true }).decode(second.bytes.subarray(0, second.bytes.byteLength - 1)),
    );
    const otherSlot = MATRIX_GENERATED_SLOT_BINDINGS[(index + 1) % MATRIX_GENERATED_SLOT_BINDINGS.length]![0];
    expectThrows(() => resolver.resolve({ ...slot, slot_id: otherSlot }, view), `resolver ${slotId} accepted the wrong slot ID`);
    expectThrows(() => resolver.resolve({ ...slot, binding: `readme_payload:${slotId}` }, view), `resolver ${slotId} accepted a wrong binding kind`);
  }
  assert(projected === new TextDecoder("utf-8", { fatal: true }).decode(afterBytes), "the four resolver marker interiors do not transform committed status-compat before bytes into exact after bytes");
  assert(TESTING_ADAPTER.slotResolvers(view, testingDocument).size === 0, "testing generated-slot registry must be declared empty");
  const emptyStatus: MatrixStatusInputs = {
    matrix: { annotations: [], features: [], containment_ids: [], control_operator_ids: [] },
    catalog: { rows: [] },
    registry: { gates: [] },
    timings: { tiers: [] },
  };
  expectThrows(() => MATRIX_ADAPTER.slotResolvers({ ...view, matrix_status_inputs: emptyStatus }, matrixDocument), "matrix slot resolvers accepted vacuous status inputs");

  const live = liveMatrixFloorProbe();
  const liveIssues = new Collector();
  MATRIX_ADAPTER.validateFloors(live, liveIssues);
  assert(liveIssues.issues.length === 0, `exact live matrix floor probe failed: ${JSON.stringify(liveIssues.issues)}`);
  const liveResolvers = MATRIX_ADAPTER.slotResolvers(view, live);
  for (const slot of live.generated_slots) {
    const resolved = liveResolvers.get(slot.slot_id)?.resolve(slot, view);
    assert(resolved !== undefined && resolved.bytes.at(-1) !== 0x0a, `live inline slot ${slot.slot_id} retained fixture-only LF ownership`);
  }

  const recordProjectionChanged: RoadmapDocumentV0 = {
    ...live,
    records: live.records.map((record, index) => index === 0
      ? { ...record, projection_group: live.sections.at(-1)!.section_id }
      : record),
  };
  const recordProjectionIssues = new Collector();
  MATRIX_ADAPTER.validateFloors(recordProjectionChanged, recordProjectionIssues);
  assert(
    recordProjectionIssues.issues.some((entry) => entry.logical_path === "matrix_v0.structure"),
    "matrix record projection-group-only mutation escaped the complete structure floor",
  );

  const sectionMetadataChanged: RoadmapDocumentV0 = {
    ...live,
    sections: live.sections.map((section, index) => index === 0
      ? { ...section, title: `${section.title} changed` }
      : section),
  };
  const sectionMetadataIssues = new Collector();
  MATRIX_ADAPTER.validateFloors(sectionMetadataChanged, sectionMetadataIssues);
  assert(
    sectionMetadataIssues.issues.some((entry) => entry.logical_path === "matrix_v0.structure"),
    "matrix section-title-only mutation escaped the complete structure floor",
  );

  const fragmentMetadataChanged: RoadmapDocumentV0 = {
    ...live,
    fragments: live.fragments.map((fragment, index) => index === 0
      ? {
        ...fragment,
        title: `${fragment.title ?? "untitled"} changed`,
        projection_group: live.sections.at(-1)!.section_id,
      }
      : fragment),
  };
  const fragmentMetadataIssues = new Collector();
  MATRIX_ADAPTER.validateFloors(fragmentMetadataChanged, fragmentMetadataIssues);
  assert(
    fragmentMetadataIssues.issues.some((entry) => entry.logical_path === "matrix_v0.structure"),
    "matrix fragment-title/group-only mutation escaped the complete structure floor",
  );

  const aliasChanged: RoadmapDocumentV0 = {
    ...live,
    records: live.records.map((record, index) => index === 0
      ? { ...record, legacy_aliases: [...(record.legacy_aliases ?? []), "fixture-alias"] }
      : record),
  };
  const aliasIssues = new Collector();
  MATRIX_ADAPTER.validateFloors(aliasChanged, aliasIssues);
  assert(
    aliasIssues.issues.some((entry) => entry.logical_path === "matrix_v0.structure"),
    "matrix alias-only mutation escaped the complete structure floor",
  );

  const manifestChanged: RoadmapDocumentV0 = {
    ...live,
    manifest: [live.manifest[1]!, live.manifest[0]!, ...live.manifest.slice(2)],
  };
  const manifestIssues = new Collector();
  MATRIX_ADAPTER.validateFloors(manifestChanged, manifestIssues);
  assert(
    manifestIssues.issues.some((entry) => entry.logical_path === "matrix_v0.structure"),
    "matrix manifest-sequence mutation escaped the complete structure floor",
  );

  const [leftRecord, rightRecord] = live.records;
  assert(leftRecord !== undefined && rightRecord !== undefined, "live record floor probe lacks swap controls");
  const wrongIds: RoadmapDocumentV0 = {
    ...live,
    records: live.records.map((record) =>
      record === leftRecord ? { ...record, id: rightRecord.id } :
        record === rightRecord ? { ...record, id: leftRecord.id } : record
    ),
  };
  const wrongIdIssues = new Collector();
  MATRIX_ADAPTER.validateFloors(wrongIds, wrongIdIssues);
  assert(wrongIdIssues.issues.some((entry) => entry.code === "E-SCHEMA-FLOOR" && entry.logical_path.startsWith("record[")), "swapped live record IDs escaped exact production floors");
  for (const slot of wrongIds.generated_slots) {
    const resolved = MATRIX_ADAPTER.slotResolvers(view, wrongIds).get(slot.slot_id)?.resolve(slot, view);
    assert(resolved?.bytes.at(-1) === 0x0a, `wrong-shape live document still selected bare slot mode for ${slot.slot_id}`);
  }

  const [leftPart, rightPart] = live.parts;
  assert(leftPart !== undefined && rightPart !== undefined, "live part floor probe lacks boundary controls");
  const wrongBoundaries: RoadmapDocumentV0 = {
    ...live,
    parts: live.parts.map((part) =>
      part === leftPart ? { ...part, span_ids: [...rightPart.span_ids] } :
        part === rightPart ? { ...part, span_ids: [...leftPart.span_ids] } : part
    ),
  };
  const wrongBoundaryIssues = new Collector();
  MATRIX_ADAPTER.validateFloors(wrongBoundaries, wrongBoundaryIssues);
  assert(wrongBoundaryIssues.issues.some((entry) => entry.code === "E-SCHEMA-FLOOR" && entry.logical_path.startsWith("part[")), "swapped live part boundaries escaped exact production floors");

  const coordinatedBypass: RoadmapDocumentV0 = {
    ...live,
    document: {
      ...live.document,
      frozen_source_sha256: "0".repeat(64),
      frozen_source_byte_length: live.document.frozen_source_byte_length + 1,
      frozen_source_line_count: live.document.frozen_source_line_count + 1,
    },
    sections: live.sections.map((section) => section.section_id === "matrix-side-work"
      ? {
        ...section,
        section_id: "fixture-matrix-side-work-renamed" as RoadmapDocumentV0["sections"][number]["section_id"],
      }
      : section),
    fragments: live.fragments.map((fragment) => fragment.fragment_id === "document-tail"
      ? {
        ...fragment,
        fragment_id: "fixture-document-tail-renamed" as RoadmapDocumentV0["fragments"][number]["fragment_id"],
      }
      : fragment),
    records: live.records.map((record, index) => ({
      ...record,
      id: `matrix.fixture-bypass-${index}` as RoadmapId,
    })),
  };
  const coordinatedBypassIssues = new Collector();
  MATRIX_ADAPTER.validateFloors(coordinatedBypass, coordinatedBypassIssues);
  assert(
    coordinatedBypassIssues.issues.some((entry) =>
      entry.code === "E-SCHEMA-FLOOR" && entry.logical_path === "document.frozen_source_sha256"
    ) && coordinatedBypassIssues.issues.some((entry) =>
      entry.code === "E-SCHEMA-FLOOR" && entry.logical_path.startsWith("record[")
    ),
    "coordinated matrix fingerprint/anchor mutation bypassed production floors",
  );
  for (const slot of coordinatedBypass.generated_slots) {
    const resolved = MATRIX_ADAPTER.slotResolvers(view, coordinatedBypass).get(slot.slot_id)?.resolve(slot, view);
    assert(
      resolved?.bytes.at(-1) === 0x0a,
      `coordinated matrix fingerprint/anchor mutation still selected bare slot mode for ${slot.slot_id}`,
    );
  }

  const liveTesting = liveTestingFloorProbe();
  const liveTestingIssues = new Collector();
  TESTING_ADAPTER.validateFloors(liveTesting, liveTestingIssues);
  assert(
    liveTestingIssues.issues.every((entry) =>
      entry.logical_path === "testing_v0.structure" ||
      entry.logical_path.startsWith("testing_v0.physical_inventory.")
    ),
    `testing live-floor probe failed an explicit pickup floor: ${JSON.stringify(liveTestingIssues.issues)}`,
  );

  const coordinatedTestingBypass: RoadmapDocumentV0 = {
    ...liveTesting,
    document: {
      ...liveTesting.document,
      frozen_source_sha256: "0".repeat(64),
      frozen_source_byte_length: liveTesting.document.frozen_source_byte_length + 1,
      frozen_source_line_count: liveTesting.document.frozen_source_line_count + 1,
    },
    sections: liveTesting.sections.map((section) => section.section_id === "standing-system"
      ? {
        ...section,
        section_id: "fixture-standing-system-renamed" as RoadmapDocumentV0["sections"][number]["section_id"],
      }
      : section),
    fragments: liveTesting.fragments.map((fragment) => fragment.fragment_id === "sources-exhaustive-menu"
      ? {
        ...fragment,
        fragment_id: "fixture-sources-menu-renamed" as RoadmapDocumentV0["fragments"][number]["fragment_id"],
      }
      : fragment),
    records: liveTesting.records.map((record) =>
      record.id === "testing.rule-trailing.directive-classification"
        ? { ...record, id: "testing.rule-trailing.coordinated-bypass" as RoadmapId }
        : record
    ),
  };
  const coordinatedTestingBypassIssues = new Collector();
  TESTING_ADAPTER.validateFloors(coordinatedTestingBypass, coordinatedTestingBypassIssues);
  assert(
    coordinatedTestingBypassIssues.issues.some((entry) =>
      entry.code === "E-SCHEMA-FLOOR" && entry.logical_path === "document.frozen_source_sha256"
    ) && coordinatedTestingBypassIssues.issues.some((entry) =>
      entry.code === "E-SCHEMA-FLOOR" && entry.logical_path === "testing_v0.structure"
    ),
    "coordinated testing fingerprint/anchor mutation bypassed production floors",
  );

  const reclassifiedPart = liveTesting.parts[0];
  assert(reclassifiedPart !== undefined, "testing live-floor probe lacks a part reclassification control");
  const reclassified: RoadmapDocumentV0 = {
    ...liveTesting,
    parts: liveTesting.parts.slice(1),
    fragments: [...liveTesting.fragments, {
      fragment_id: "fixture-reclassified-part" as RoadmapDocumentV0["fragments"][number]["fragment_id"],
      projection_group: "standing-system" as RoadmapDocumentV0["sections"][number]["section_id"],
      title: reclassifiedPart.title,
      source_block_md: reclassifiedPart.source_block_md,
      span_ids: [...reclassifiedPart.span_ids],
    }],
  };
  const reclassifiedIssues = new Collector();
  TESTING_ADAPTER.validateFloors(reclassified, reclassifiedIssues);
  assert(
    reclassifiedIssues.issues.some((entry) => entry.logical_path === "part") &&
      reclassifiedIssues.issues.some((entry) => entry.logical_path === "fragment") &&
      reclassifiedIssues.issues.some((entry) => entry.logical_path === "testing_v0.structure"),
    "testing nested-part-to-fragment reclassification escaped exact production floors",
  );

  const selectedId = "testing.rule-trailing.directive-classification";
  const renamedSelected: RoadmapDocumentV0 = {
    ...liveTesting,
    records: liveTesting.records.map((record) => record.id === selectedId
      ? {
        ...record,
        id: "testing.rule-trailing.directive-renamed" as RoadmapId,
        legacy_aliases: undefined,
      }
      : record),
  };
  const renamedSelectedIssues = new Collector();
  TESTING_ADAPTER.validateFloors(renamedSelected, renamedSelectedIssues);
  assert(
    renamedSelectedIssues.issues.some((entry) =>
      entry.logical_path === `record[${JSON.stringify(selectedId)}]`
    ) && renamedSelectedIssues.issues.some((entry) => entry.logical_path === "testing_v0.structure"),
    "testing RuleTrailing rename/alias deletion escaped exact production floors",
  );
}

function testMixedLiveMatrixInlineSlots(bundle: AdapterFixtureBundle): void {
  const authoritative = liveMatrixAuthoritativeDocument();
  const semanticRecord = authoritative.records.find((record) =>
    record.id === "matrix.additional-tool-annotations"
  );
  const complete = authoritative.document.semantic_conversion === "complete";
  assert(semanticRecord !== undefined && (complete
    ? semanticRecord.render_authority === "semantic" && semanticRecord.projection_visibility === "document" &&
      semanticRecord.source_replacements.length === 1
    : semanticRecord.render_authority === "raw" && semanticRecord.semantic_shadow !== undefined),
  "live semantic conversion owner does not match its declared conversion stage");
  const relationTarget = authoritative.records.find((record) => record.id !== semanticRecord.id);
  assert(relationTarget !== undefined, "packet-1 live semantic conversion lacks a relation target");
  const authorityReferenceId = "packet-one-authority" as ReferenceId;
  const mixed: RoadmapDocumentV1 = {
    ...authoritative,
    relations: [...authoritative.relations, {
      source: semanticRecord.id,
      kind: "related",
      target: relationTarget.id,
      note_md: new TextEncoder().encode("Packet-1 relation must not select fixture slot layout."),
    }],
    references: [...authoritative.references, {
      id: authorityReferenceId,
      source: semanticRecord.id,
      kind: "spec_passage",
      document: "packet-one-selftest",
      passage: "mixed-v1-inline-slot-authority",
    }],
  };
  assert(complete
    ? mixed.records.every((record) => record.render_authority === "semantic")
    : mixed.records.some((record) => record.render_authority === "raw" && record.semantic_shadow !== undefined),
  "live vector record authorities do not match its declared conversion stage");
  assert(
    mixed.relations.length === authoritative.relations.length + 1 &&
      mixed.references.length === authoritative.references.length + 1,
    "packet-1 vector did not add both a relation and a reference",
  );

  const view = registryView(bundle, mixed, liveMatrixStatusInputs());
  const validation = validateMatrixRoadmapDocument(mixed, view);
  assert(validation.issues.length === 0, `packet-1 mixed live document failed validation: ${JSON.stringify(validation.issues)}`);
  const resolvers = MATRIX_ADAPTER.slotResolvers(view, mixed);
  for (const slot of mixed.generated_slots) {
    const resolved = resolvers.get(slot.slot_id)?.resolve(slot, view);
    assert(resolved !== undefined && resolved.bytes.at(-1) !== 0x0a, `mixed-v1 inline slot ${slot.slot_id} retained fixture-only LF ownership`);
  }
  const refreshedGeneratedDigest: RoadmapDocumentV1 = {
    ...mixed,
    spans: mixed.spans.map((span) => span.id === "slot-counts"
      ? { ...span, sha256: "f".repeat(64) }
      : span),
  };
  const refreshedResolvers = MATRIX_ADAPTER.slotResolvers(view, refreshedGeneratedDigest);
  for (const slot of refreshedGeneratedDigest.generated_slots) {
    const resolved = refreshedResolvers.get(slot.slot_id)?.resolve(slot, view);
    assert(resolved !== undefined && resolved.bytes.at(-1) !== 0x0a,
      `refreshed generated digest changed production inline ownership for ${slot.slot_id}`);
  }

  const projection = liveMatrixCurrentLegacyProjection();
  const rendered = renderFixture(mixed, MATRIX_ADAPTER, view);
  const semanticAuthorities = mixed.records.filter((record) => record.render_authority === "semantic").length;
  assert(rendered.semantic_calls === semanticAuthorities && (complete ? semanticAuthorities > 1 :
    mixed.records.some((record) => record.render_authority === "raw" && record.semantic_shadow !== undefined)),
  "live renderer calls do not match the declared conversion stage");
  assert(
    bytesEqual(rendered.bytes, projection),
    `packet-1 mixed-v1 projection differs at byte ${firstByteDifference(rendered.bytes, projection)} ` +
      `(rendered length/digest ${rendered.bytes.byteLength}/${sha256(rendered.bytes)}, ` +
      `live ${projection.byteLength}/${sha256(projection)})`,
  );
  assert(
    rendered.bytes.byteLength === 83_654 &&
      sha256(rendered.bytes) === "0f82fde27a06de8be795ef15f611ae74901771af876bfb65d856de0ef273f8df",
    "packet-1 mixed-v1 projection escaped the frozen live length/digest floor",
  );

  const mixedShadow = liveMatrixShadowV0Document(mixed);
  const shadowIssues = new Collector();
  MATRIX_ADAPTER.validateFloors(mixedShadow, shadowIssues);
  assert(shadowIssues.issues.length === 0, `packet-1 reconstructed v0 shadow failed frozen floors: ${JSON.stringify(shadowIssues.issues)}`);
  assert(
    bytesEqual(composeRoadmapDocument(mixedShadow), liveMatrixShadowV0Source()),
    "packet-1 mixed authority did not reconstruct the exact frozen v0 source",
  );

  const badCoordinates: RoadmapDocumentV1 = {
    ...mixed,
    spans: mixed.spans.map((span) => span.id === "slot-constraint"
      ? { ...span, start_byte: span.start_byte + 1 }
      : span),
  };
  const coordinateResolvers = MATRIX_ADAPTER.slotResolvers(view, badCoordinates);
  for (const slot of badCoordinates.generated_slots) {
    const resolved = coordinateResolvers.get(slot.slot_id)?.resolve(slot, view);
    assert(resolved?.bytes.at(-1) === 0x0a, `malformed production slot coordinates still selected inline mode for ${slot.slot_id}`);
  }

  const badBinding: RoadmapDocumentV1 = {
    ...mixed,
    generated_slots: mixed.generated_slots.map((slot) => slot.slot_id === "constraint"
      ? { ...slot, binding: "status_header_markers:roadmap-counts" }
      : slot),
  };
  const bindingIssues = new Collector();
  MATRIX_ADAPTER.validateFloors(badBinding, bindingIssues);
  assert(
    bindingIssues.issues.some((entry) => entry.logical_path === "generated_slot[\"constraint\"].binding"),
    "malformed production slot binding escaped the declared matrix slot floor",
  );
}

function withRawDocumentVisibleRecord(document: RoadmapDocumentV1): RoadmapDocumentV1 {
  const prefix = document.document.roadmap === "matrix" ? "record-" : "span-record-";
  if (document.records.some((record) => record.render_authority === "raw" &&
    record.semantic_shadow !== undefined && record.span_ids.length === 1 && record.span_ids[0]!.startsWith(prefix))) {
    return document;
  }
  const semantic = document.records.find((record) =>
    record.render_authority === "semantic" && record.projection_visibility === "document" &&
    record.source_replacements.length === 1 && record.source_replacements[0]!.span_id.startsWith(prefix)
  );
  assert(semantic?.render_authority === "semantic", "reconstruction fixture lacks a reversible semantic record");
  const replacement = semantic.source_replacements[0]!;
  assert(
    replacement.replacement_field === "payload.summary_md" || replacement.replacement_field === "payload.detail_md",
    "reconstruction fixture replacement is not a top-level Markdown field",
  );
  const sourceBlock = replacement.replacement_field === "payload.summary_md"
    ? semantic.payload.summary_md
    : semantic.payload.detail_md;
  assert(sourceBlock !== undefined, "reconstruction fixture replacement field is absent");
  const raw = {
    id: semantic.id,
    title: semantic.title,
    projection_group: semantic.projection_group,
    ...(semantic.legacy_aliases === undefined ? {} : { legacy_aliases: semantic.legacy_aliases }),
    ...(semantic.tags === undefined ? {} : { tags: semantic.tags }),
    render_authority: "raw" as const,
    source_block_md: sourceBlock,
    span_ids: [replacement.span_id],
    semantic_shadow: semantic.payload,
  };
  return {
    ...document,
    document: {
      ...document.document,
      semantic_conversion: "converting",
      frozen_legacy_span_ids: [...document.document.frozen_legacy_span_ids, replacement.span_id].sort(),
    },
    records: document.records.map((record) => record === semantic ? raw : record),
    spans: document.spans.map((span) => span.id === replacement.span_id
      ? { ...span, owner_field: "source_block_md", migration_status: "raw" }
      : span),
  };
}

function withSemanticOnlyRecord(document: RoadmapDocumentV1, id: RoadmapId): RoadmapDocumentV1 {
  const record = {
    id,
    title: "Semantic-only reconstruction vector",
    projection_group: document.sections[0]!.section_id,
    render_authority: "semantic" as const,
    projection_visibility: "semantic_only" as const,
    payload: {
      kind: "work" as const,
      summary_md: new TextEncoder().encode("Semantic-only metadata."),
      work_state: "ready" as const,
      work_intent: "build_capability" as const,
      work_kind: "feature" as const,
      risk: "cosmetic" as const,
      family_classification: "none_reviewed" as const,
      acceptance_md: new TextEncoder().encode("Identity remains active."),
      priority_rationale_md: new TextEncoder().encode("No document bytes are owned."),
    },
    source_replacements: [],
  };
  return {
    ...document,
    records: [...document.records, record],
  };
}

function withDocumentVisibleRecord(document: RoadmapDocumentV1): RoadmapDocumentV1 {
  const raw = document.records.find((record) => record.render_authority === "raw" && record.span_ids.length === 1);
  assert(raw !== undefined && raw.render_authority === "raw", "document-visible reconstruction vector lacks a single-span raw record");
  const spanId = raw.span_ids[0]!;
  const converted = {
    id: raw.id,
    title: raw.title,
    projection_group: raw.projection_group,
    ...(raw.legacy_aliases === undefined ? {} : { legacy_aliases: raw.legacy_aliases }),
    ...(raw.tags === undefined ? {} : { tags: raw.tags }),
    render_authority: "semantic" as const,
    projection_visibility: "document" as const,
    payload: {
      kind: "decision" as const,
      summary_md: raw.source_block_md,
      decision_state: "pending" as const,
      question_md: new TextEncoder().encode("Reconstruction-only metadata."),
      transition_ids: [],
    },
    source_replacements: [{
      span_id: spanId,
      replacement_field: "payload.summary_md",
      review_note_md: new TextEncoder().encode("Exact legacy block reviewed."),
    }],
  };
  return {
    ...document,
    document: {
      ...document.document,
      frozen_legacy_span_ids: document.document.frozen_legacy_span_ids.filter((id) => id !== spanId),
    },
    records: document.records.map((record) => record === raw ? converted : record),
    spans: document.spans.map((span) => span.id === spanId ? {
      ...span,
      owner_field: "payload.summary_md",
      migration_status: "replaced" as const,
    } : span),
  };
}

function testMatrixV0ReconstructionVisibilityArms(bundle: AdapterFixtureBundle): void {
  const authoritative = liveMatrixAuthoritativeDocument();
  const semanticOnly = withSemanticOnlyRecord(authoritative, "matrix.fixture-semantic-only" as RoadmapId);
  const rendered = renderFixture(semanticOnly, MATRIX_ADAPTER, registryView(bundle, semanticOnly, liveMatrixStatusInputs()));
  assert(
    bytesEqual(rendered.bytes, liveMatrixCurrentLegacyProjection()),
    "semantic-only matrix record changed the current pre-anchor projection bytes",
  );
  assert(bytesEqual(composeRoadmapDocument(liveMatrixShadowV0Document(semanticOnly)), liveMatrixShadowV0Source()), "matrix v0 reconstruction retained semantic-only record or placement");
  const documentVisible = withDocumentVisibleRecord(withRawDocumentVisibleRecord(authoritative));
  assert(bytesEqual(composeRoadmapDocument(liveMatrixShadowV0Document(documentVisible)), liveMatrixShadowV0Source()), "matrix v0 reconstruction did not restore a document-visible semantic record");
}

function testTestingV0ReconstructionVisibilityArms(): void {
  const authoritative = liveTestingAuthoritativeDocument();
  const semanticOnly = withSemanticOnlyRecord(authoritative, "testing.fixture-semantic-only" as RoadmapId);
  assert(bytesEqual(composeRoadmapDocument(liveTestingShadowV0Document(semanticOnly)), liveTestingShadowV0Source()), "testing v0 reconstruction retained semantic-only record or placement");
  const documentVisible = withDocumentVisibleRecord(withRawDocumentVisibleRecord(authoritative));
  assert(bytesEqual(composeRoadmapDocument(liveTestingShadowV0Document(documentVisible)), liveTestingShadowV0Source()), "testing v0 reconstruction did not restore a document-visible semantic record");
}

function liveMatrixFloorProbe(): RoadmapDocumentV0 {
  return liveMatrixShadowV0Document();
}

function liveTestingFloorProbe(): RoadmapDocumentV0 {
  const empty = new Uint8Array();
  const sections: RoadmapDocumentV0["sections"] = [
    ["declined-boundaries", "Declined (decided, with a reopening signal unless explicitly permanent)"],
    ["deferred-features", "Deferred features (build when a real consumer needs them)"],
    ["next-priority", "Next work items, in priority order"],
    ["north-star", "North star — automated feature coverage"],
    ["operational-watches", "Operational watches"],
    ["pending-maintainer", "Pending maintainer action"],
    ["preamble", "Testing roadmap preamble"],
    ["sources", "Sources"],
    ["standing-system", "Standing-system residuals"],
  ].map(([section_id, title], index) => ({
    section_id: section_id as RoadmapDocumentV0["sections"][number]["section_id"],
    title,
    ...(section_id === "standing-system" ? { legacy_aliases: ["Standing-system residuals"] } : {}),
    source_block_md: empty,
    span_ids: [`fixture-section-span-a${index}` as RoadmapDocumentV0["spans"][number]["id"]],
  }));
  const fragments: RoadmapDocumentV0["fragments"] = [
    [
      "sources-exhaustive-menu",
      "Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`",
    ],
    ["sources-expert-writeups", "Per-dimension expert write-ups: `draft/testing-recommendations/*.md`"],
  ].map(([fragment_id, title], index) => ({
    fragment_id: fragment_id as RoadmapDocumentV0["fragments"][number]["fragment_id"],
    projection_group: "sources" as RoadmapDocumentV0["sections"][number]["section_id"],
    title,
    source_block_md: empty,
    span_ids: [`fixture-fragment-span-a${index}` as RoadmapDocumentV0["spans"][number]["id"]],
  }));

  const records: RoadmapDocumentV0["records"] = [];
  let recordIndex = 0;
  const addRecords = (group: string, count: number, ordinals: readonly number[] = []): void => {
    for (let index = 0; index < count; index++) {
      const ordinal = ordinals[index];
      records.push({
        id: `testing.fixture-a${recordIndex}` as RoadmapId,
        title: `Fixture record ${recordIndex}`,
        projection_group: group as RoadmapDocumentV0["sections"][number]["section_id"],
        ...(ordinal === undefined ? {} : { legacy_aliases: [`Next work ${ordinal}`] }),
        source_block_md: empty,
        span_ids: [`fixture-record-span-a${recordIndex}` as RoadmapDocumentV0["spans"][number]["id"]],
      });
      recordIndex++;
    }
  };
  records.push({
    id: "testing.rule-trailing.directive-classification" as RoadmapId,
    title: "Adopt the parser's `RuleTrailing` anchor and classify that rule-only slot in one delivery — blocked on publishing the reviewed fork revision.",
    projection_group: "pending-maintainer" as RoadmapDocumentV0["sections"][number]["section_id"],
    legacy_aliases: ["B3-002", "B3-005", "T1-09"],
    source_block_md: empty,
    span_ids: ["span-record-rule-trailing-directive-classification" as RoadmapDocumentV0["spans"][number]["id"]],
  });
  addRecords("pending-maintainer", 1);
  addRecords("next-priority", 25, [1, 2, 3, 4, 5, 6, 7, 8, ...Array.from({ length: 17 }, (_, index) => index + 10)]);
  addRecords("standing-system", 61);
  addRecords("deferred-features", 18);
  addRecords("operational-watches", 13);
  addRecords("declined-boundaries", 11);
  assert(records.length === 130, "testing live-floor probe record count drifted");

  const parts: RoadmapDocumentV0["parts"] = Array.from({ length: 57 }, (_, index) => ({
    part_id: `fixture-part-a${index}` as RoadmapDocumentV0["parts"][number]["part_id"],
    parent_record_id: records[index]!.id,
    title: `Fixture part ${index}`,
    source_block_md: empty,
    span_ids: [`fixture-part-span-a${index}` as RoadmapDocumentV0["spans"][number]["id"]],
  }));
  const spans: RoadmapDocumentV0["spans"] = [{
    id: "span-record-rule-trailing-directive-classification" as RoadmapDocumentV0["spans"][number]["id"],
    start_byte: 6_013,
    end_byte: 8_877,
    sha256: "c5a5b506dba80f59781f9024767bd7b6bd14d191981f1923553d12ad65b8d338",
    source_kind: "record",
    owner_id: "testing.rule-trailing.directive-classification",
    owner_field: "source_block_md",
    migration_status: "raw",
  }];
  while (spans.length < 198) {
    const index = spans.length;
    spans.push({
      id: `fixture-ledger-span-a${index}` as RoadmapDocumentV0["spans"][number]["id"],
      start_byte: 20_000 + index,
      end_byte: 20_001 + index,
      sha256: "0".repeat(64),
      source_kind: "section",
      owner_id: sections[0]!.section_id,
      owner_field: "source_block_md",
      migration_status: "raw",
    });
  }
  return {
    document: {
      schema_version: 0,
      authority: "shadow",
      roadmap: "testing",
      source_path: "tests/testing-roadmap.toml" as RepoPath,
      projection_path: "tests/TESTING_ROADMAP.md" as RepoPath,
      frozen_source_sha256: "6e90f1fb06011cefa546d861da0a6525ff1af6fc81bbe51c9ed5f035578b53af",
      frozen_source_byte_length: 306_388,
      frozen_source_line_count: 3_412,
      frozen_source_eof: "lf",
    },
    sections,
    fragments,
    legacy_markers: [],
    records,
    parts,
    generated_slots: [],
    manifest: Array.from({ length: 198 }, () => ({
      kind: "section" as const,
      section_id: sections[0]!.section_id,
    })),
    spans,
  };
}

function markdownSnapshots(value: unknown): readonly { readonly original: Uint8Array; readonly copy: Uint8Array }[] {
  const snapshots: { original: Uint8Array; copy: Uint8Array }[] = [];
  const visit = (candidate: unknown): void => {
    if (candidate instanceof Uint8Array) {
      snapshots.push({ original: candidate, copy: new Uint8Array(candidate) });
      return;
    }
    if (Array.isArray(candidate)) {
      for (const entry of candidate) visit(entry);
      return;
    }
    if (candidate !== null && typeof candidate === "object") {
      for (const key of Object.keys(candidate)) visit((candidate as Record<string, unknown>)[key]);
    }
  };
  visit(value);
  return snapshots;
}

function markdownByteMap(value: unknown, path = "payload"): ReadonlyMap<string, Uint8Array> {
  const fields = new Map<string, Uint8Array>();
  const visit = (candidate: unknown, logicalPath: string): void => {
    if (candidate instanceof Uint8Array) {
      fields.set(logicalPath, candidate);
      return;
    }
    if (Array.isArray(candidate)) {
      candidate.forEach((entry, index) => visit(entry, `${logicalPath}[${index}]`));
      return;
    }
    if (candidate !== null && typeof candidate === "object") {
      for (const key of Object.keys(candidate)) {
        visit((candidate as Record<string, unknown>)[key], `${logicalPath}.${key}`);
      }
    }
  };
  visit(value, path);
  return fields;
}

function expectedCanonicalFieldOrder(value: SemanticPayload): readonly string[] {
  const paths: string[] = [];
  const add = (path: string, bytes: Uint8Array | undefined): void => {
    if (bytes !== undefined) paths.push(`payload.${path}`);
  };
  add("summary_md", value.summary_md);
  add("detail_md", value.detail_md);
  switch (value.kind) {
    case "work":
      add("acceptance_md", value.acceptance_md);
      if (value.work_state === "ready") add("priority_rationale_md", value.priority_rationale_md);
      if (value.work_state === "blocked") add("blocker_md", value.blocker_md);
      if (value.work_state === "delegated") add("return_condition_md", value.return_condition_md);
      if (value.work_state === "pending_review") add("uncertainty_md", value.uncertainty_md);
      break;
    case "decision":
      if (value.decision_state === "pending") add("question_md", value.question_md);
      else add("rationale_md", value.rationale_md);
      break;
    case "signal":
      if (value.transition_kind === "promotion_trigger" || value.transition_kind === "reopening_signal") {
        add("action_on_fire_md", value.action_on_fire_md);
        if (value.predicate.predicate_kind === "event") add("predicate.event_md", value.predicate.event_md);
        if (value.predicate.predicate_kind === "manual") add("predicate.review_procedure_md", value.predicate.review_procedure_md);
      } else if (value.transition_kind === "unblock_predicate") {
        add("event_md", value.event_md);
        add("check_procedure_md", value.check_procedure_md);
        add("due_action_md", value.due_action_md);
      } else if (value.transition_kind === "watch_escalation") {
        add("failure_signature_md", value.failure_signature_md);
        add("capture_procedure_md", value.capture_procedure_md);
        add("response_md", value.response_md);
        add("escalation_action_md", value.escalation_action_md);
        add("retirement_semantics_md", value.retirement_semantics_md);
      } else if (value.transition_kind === "retirement_predicate") {
        add("external_predicate_md", value.external_predicate_md);
        add("verification_md", value.verification_md);
        add("due_action_md", value.due_action_md);
      } else {
        add("period_or_event_md", value.period_or_event_md);
        add("checklist_md", value.checklist_md);
        add("missed_action_md", value.missed_action_md);
      }
      break;
    case "evidence":
      add("claim_md", value.claim_md);
      add("command_md", value.command_md);
      add("result_md", value.result_md);
      add("environment_md", value.environment_md);
      add("unprobed_remainder_md", value.unprobed_remainder_md);
      break;
    case "control":
      add("claim_md", value.claim_md);
      add("boundary_md", value.boundary_md);
      break;
    case "family":
      add("goal_md", value.goal_md);
      add("boundary_md", value.boundary_md);
      if (value.family_maturity === "under_design") {
        add("derivation_md", value.derivation_md);
        add("legality_rule_md", value.legality_rule_md);
        add("denominator_unknowns_md", value.denominator_unknowns_md);
      }
      value.exclusions.forEach((entry, index) => add(`exclusions[${index}].reason_md`, entry.reason_md));
      break;
    case "matrix_external_closeout":
      add("current_upstream_state_md", value.current_upstream_state_md);
      if (value.closeout_state === "blocked") add("blocker_md", value.blocker_md);
      add("verification_md", value.verification_md);
      value.actions.forEach((entry, index) => add(`actions[${index}].action_md`, entry.action_md));
      value.branches.forEach((entry, index) => add(`branches[${index}].predicate_md`, entry.predicate_md));
      break;
    case "matrix_policy":
      if (value.policy_kind === "maintenance_protocol") add("protocol_md", value.protocol_md);
      else add("rationale_md", value.rationale_md);
      break;
    case "testing_operational_watch":
      add("signature_md", value.signature_md);
      if (value.watch_state !== "watching") add("attribution_md", value.attribution_md);
      add("response_md", value.response_md);
      add("retirement_semantics_md", value.retirement_semantics_md);
      value.capture_steps.forEach((entry, index) => add(`capture_steps[${index}].capture_md`, entry.capture_md));
      break;
    case "testing_incident":
      add("signature_md", value.signature_md);
      if (value.incident_posture !== "live") add("attribution_md", value.attribution_md);
      break;
    case "testing_cost":
      add("scope_md", value.scope_md);
      if (value.cost_posture === "historical_observation") add("environment_md", value.environment_md);
      break;
    case "testing_system_admission":
      add("claim_md", value.claim_md);
      break;
    default: {
      const exhaustive: never = value;
      return exhaustive;
    }
  }
  return Object.freeze(paths);
}

function assertCanonicalFieldInventory(payload: SemanticPayload, context: string): void {
  const expectedOrder = expectedCanonicalFieldOrder(payload);
  const expectedBytes = markdownByteMap(payload);
  const actual = canonicalSemanticMarkdownFields(payload);
  assert(actual.map((field) => field.logical_path).join("|") === expectedOrder.join("|"), `${context} canonical FieldConsumer order drifted`);
  assert(actual.length === expectedBytes.size, `${context} canonical field inventory omitted or duplicated Markdown bytes`);
  for (const field of actual) {
    const expected = expectedBytes.get(field.logical_path);
    assert(expected !== undefined && field.bytes === expected && bytesEqual(field.bytes, expected), `${context} ${field.logical_path} did not retain its exact decoded input-byte mapping`);
  }
}

function renderFixture(
  document: RoadmapDocument,
  adapter: RoadmapAdapter<SemanticPayload>,
  view: RegistryView,
): { readonly bytes: Uint8Array; readonly semantic_calls: number } {
  const placement = resolveManifest(document);
  const resolvers = adapter.slotResolvers(view, document);
  let semanticCalls = 0;
  const completed = buildExpectedChunks(document, placement.ops, {
    renderSemanticRecord(record, fields) {
      semanticCalls++;
      return adapter.renderSemantic(record, fields);
    },
    resolveGeneratedSlot(slot) {
      return resolvers.get(slot.slot_id)?.resolve(slot, view);
    },
  });
  const issues = [...placement.issues, ...validateCompletedChunks(document, placement.ops, completed)];
  assert(issues.length === 0, `completed adapter rendering failed: ${JSON.stringify(issues)}`);
  return {
    bytes: renderValidatedChunks(completed.chunks, issues, completed.expected_bytes),
    semantic_calls: semanticCalls,
  };
}

function testGoldenRendering(bundle: AdapterFixtureBundle): void {
  const goldens: {
    roadmap: RoadmapName;
    source_path: AdapterFixturePath;
    expected_path: AdapterFixturePath;
    expected_length: number;
    rendered: Uint8Array;
  }[] = [];
  for (const [roadmap, sourcePath, expectedPath, adapter, statusInputs, expectedLength] of [
    ["matrix", "positive/mixed-matrix-v1.toml", "positive/mixed-matrix-v1.expected.md", MATRIX_ADAPTER, statusCompatibilityInputs(), 93],
    ["testing", "positive/mixed-testing-v1.toml", "positive/mixed-testing-v1.expected.md", TESTING_ADAPTER, statusCompatibilityInputs(), 96],
    ["matrix", "positive/small-matrix-v2.toml", "positive/small-matrix-v2.expected.md", MATRIX_ADAPTER, statusCompatibilityInputs(), 73],
    ["testing", "positive/small-testing-v2.toml", "positive/small-testing-v2.expected.md", TESTING_ADAPTER, statusCompatibilityInputs(), 75],
    ["matrix", "all-fields/matrix-v1.toml", "all-fields/matrix-v1.expected.md", MATRIX_ADAPTER, allFieldsStatusInputs(), 3051],
    ["testing", "all-fields/testing-v1.toml", "all-fields/testing-v1.expected.md", TESTING_ADAPTER, allFieldsStatusInputs(), 697],
    ["matrix", "all-fields/matrix-v2.toml", "all-fields/matrix-v2.expected.md", MATRIX_ADAPTER, allFieldsStatusInputs(), 1737],
    ["testing", "all-fields/testing-v2.toml", "all-fields/testing-v2.expected.md", TESTING_ADAPTER, allFieldsStatusInputs(), 511],
  ] as const) {
    const document = decoded(bundle, sourcePath, roadmap);
    const snapshots = markdownSnapshots(document);
    const rendered = renderFixture(document, adapter, registryView(bundle, document, statusInputs));
    goldens.push({
      roadmap,
      source_path: sourcePath,
      expected_path: expectedPath,
      expected_length: expectedLength,
      rendered: rendered.bytes,
    });
    const semanticAuthorities = document.records.filter((record) =>
      "render_authority" in record && record.render_authority === "semantic"
    ).length;
    assert(rendered.semantic_calls === semanticAuthorities, `${roadmap} semantic shadow reached the renderer or a semantic authority did not render exactly once`);
    const second = renderFixture(document, adapter, registryView(bundle, document, statusInputs));
    assert(bytesEqual(rendered.bytes, second.bytes), `${roadmap} adapter rendering changed on a second render`);
    assert(snapshots.every((entry) => bytesEqual(entry.original, entry.copy)), `${roadmap} adapter mutated decoded Markdown bytes`);
  }

  for (const [roadmap, path, adapter] of [
    ["matrix", "positive/mixed-matrix-v1.toml", MATRIX_ADAPTER],
    ["testing", "positive/mixed-testing-v1.toml", TESTING_ADAPTER],
    ["matrix", "positive/small-matrix-v2.toml", MATRIX_ADAPTER],
    ["testing", "positive/small-testing-v2.toml", TESTING_ADAPTER],
  ] as const) {
    const document = decoded(bundle, path, roadmap);
    const record = requireSemantic(document, (payload) => payload.kind === "work");
    assertCanonicalFieldInventory(record.payload, `${path} semantic work`);
    const expectedOrder = expectedCanonicalFieldOrder(record.payload);
    const expectedInputs = markdownByteMap(record.payload);
    const normal = fieldSpy();
    const normalBytes = adapter.renderSemantic(record, normal.consumer);
    assert(normal.calls.map((call) => call.path).join("|") === expectedOrder.join("|"), `${roadmap} renderer did not call FieldConsumer in frozen ready-work order`);
    assert(normal.calls.every((call) => call.bytes === expectedInputs.get(call.path)), `${roadmap} renderer did not pass each exact decoded byte object to FieldConsumer`);
    const reversedPayload = Object.fromEntries(Object.entries(record.payload).reverse()) as SemanticPayload;
    const reversed = { ...record, payload: reversedPayload } as SemanticRecord;
    assertCanonicalFieldInventory(reversedPayload, `${path} reversed semantic work`);
    const reversedSpy = fieldSpy();
    assert(bytesEqual(normalBytes, adapter.renderSemantic(reversed, reversedSpy.consumer)), `${roadmap} rendering depends on payload property construction order`);
    assert(normal.calls.map((call) => call.path).join("|") === reversedSpy.calls.map((call) => call.path).join("|"), `${roadmap} field traversal order changed with object construction order`);
    const substitutions = new Map<string, Uint8Array>([
      ["payload.summary_md", new TextEncoder().encode(`SUBSTITUTED ${roadmap.toUpperCase()} SUMMARY\n`)],
      ["payload.detail_md", new TextEncoder().encode(`SUBSTITUTED ${roadmap.toUpperCase()} DETAIL\n`)],
    ]);
    const substitutionCalls: { path: string; input: Uint8Array }[] = [];
    const substituted = adapter.renderSemantic(record, {
      consume(logicalPath, input) {
        substitutionCalls.push({ path: logicalPath, input });
        return substitutions.get(logicalPath) ?? new Uint8Array(input);
      },
    });
    assert(substitutionCalls.map((call) => call.path).join("|") === expectedOrder.join("|"), `${roadmap} substitution changed the exact FieldConsumer call sequence`);
    assert(substitutionCalls.every((call) => call.input === expectedInputs.get(call.path)), `${roadmap} substitution did not receive the exact decoded field input`);
    const replacementFields = new Set(record.source_replacements.map((replacement) => replacement.replacement_field));
    const expectedSubstitution = combineBytes(expectedOrder
      .filter((logicalPath) => replacementFields.has(logicalPath))
      .map((logicalPath) => substitutions.get(logicalPath) ?? expectedInputs.get(logicalPath)!));
    assert(bytesEqual(substituted, expectedSubstitution) && !bytesEqual(substituted, normalBytes), `${roadmap} renderer did not append and expose FieldConsumer substitute bytes in exact canonical output order`);
  }

  for (const [roadmap, path, adapter] of [
    ["matrix", "all-fields/matrix-v1.toml", MATRIX_ADAPTER],
    ["testing", "all-fields/testing-v1.toml", TESTING_ADAPTER],
    ["matrix", "all-fields/matrix-v2.toml", MATRIX_ADAPTER],
    ["testing", "all-fields/testing-v2.toml", TESTING_ADAPTER],
  ] as const) {
    const document = decoded(bundle, path, roadmap);
    const built = buildRoadmapIndexes(document);
    assert(built.issues.length === 0, `${roadmap} all-arm rendering fixture failed C4A`);
    for (const provider of built.indexes.payload_records.values()) {
      const context = `${roadmap} ${provider.authority} ${provider.record.id} ${provider.payload.kind}`;
      assertCanonicalFieldInventory(provider.payload, context);
      const reversedPayload = Object.fromEntries(Object.entries(provider.payload).reverse()) as SemanticPayload;
      assertCanonicalFieldInventory(reversedPayload, `${context} reversed construction`);
      assert(
        canonicalSemanticMarkdownFields(provider.payload).map((field) => field.logical_path).join("|") ===
          canonicalSemanticMarkdownFields(reversedPayload).map((field) => field.logical_path).join("|"),
        `${context} frozen canonical order changed after property reversal`,
      );
      if (provider.authority === "semantic_shadow") continue;
      const record = provider.record;
      assert("render_authority" in record && record.render_authority === "semantic", `${context} semantic provider record is not render authority`);
      const expectedOrder = expectedCanonicalFieldOrder(provider.payload);
      const expectedInputs = markdownByteMap(provider.payload);
      const calls = fieldSpy();
      const rendered = adapter.renderSemantic(record, calls.consumer);
      assert(calls.calls.map((call) => call.path).join("|") === expectedOrder.join("|"), `${context} renderer call order differs from frozen per-arm oracle`);
      assert(calls.calls.every((call) => call.bytes === expectedInputs.get(call.path)), `${context} renderer passed a noncanonical path-to-input byte mapping`);
      const replacements = new Set(record.source_replacements.map((entry) => entry.replacement_field));
      const renderedPaths = record.projection_visibility === "semantic_only"
        ? []
        : expectedOrder.filter((logicalPath) => replacements.has(logicalPath));
      assert(bytesEqual(rendered, combineBytes(renderedPaths.map((logicalPath) => expectedInputs.get(logicalPath)!))), `${context} output differs after exact order and input mapping validation`);
    }
  }

  for (const golden of goldens) {
    const expected = fixtureBytes(bundle, golden.expected_path);
    assert(expected.byteLength === golden.expected_length, `${golden.roadmap} committed ${golden.source_path} golden has an unexpected byte length`);
    assert(bytesEqual(golden.rendered, expected), `${golden.roadmap} ${golden.source_path} did not render the exact committed golden after the exhaustive order/input oracle passed`);
  }
}

function validationAuthority(document: RoadmapDocument): ReturnType<typeof deriveUnresolvedMigrationAuthority>["authority"] {
  const built = buildRoadmapIndexes(document);
  assert(built.issues.length === 0, `authority input failed C4A: ${JSON.stringify(built.issues)}`);
  const result = deriveUnresolvedMigrationAuthority(built.indexes);
  assert(result.issues.length === 0, `unresolved-migration authority failed: ${JSON.stringify(result.issues)}`);
  return result.authority;
}

interface AdapterMutationVector {
  readonly name: string;
  readonly roadmap: RoadmapName;
  readonly logical_path: string;
  readonly issue_codes: readonly RoadmapIssue["code"][];
  mutate(document: RoadmapDocumentV1): { readonly document: RoadmapDocumentV1; readonly payload: SemanticPayload };
}

function testDomainMutationTable(bundle: AdapterFixtureBundle): void {
  const matrixPath = (id: string, field: string): string => `record[${JSON.stringify(id)}].semantic_shadow.${field}`;
  const testingPath = (id: string, field: string): string => `record[${JSON.stringify(id)}].semantic_shadow.${field}`;
  const vectors: readonly AdapterMutationVector[] = [
    {
      name: "matrix closeout retirement subtype",
      roadmap: "matrix",
      logical_path: matrixPath("matrix.fixture-upstream-a", "transition_ids"),
      issue_codes: ["E-REFERENCE-FORBIDDEN", "E-SCHEMA-STATE"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "matrix_external_closeout" && payload.closeout_state === "waiting",
        (payload) => {
          assert(payload.kind === "matrix_external_closeout", "matrix closeout vector selected the wrong payload");
          return { ...payload, transition_ids: ["matrix.fixture-signal-k" as RoadmapId] };
        }),
    },
    {
      name: "matrix cadence transition subtype",
      roadmap: "matrix",
      logical_path: matrixPath("matrix.fixture-policy-a", "cadence_transition_id"),
      issue_codes: ["E-REFERENCE-FORBIDDEN", "E-SCHEMA-STATE"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "matrix_policy" && payload.policy_kind === "maintenance_protocol",
        (payload) => {
          assert(payload.kind === "matrix_policy" && payload.policy_kind === "maintenance_protocol", "matrix cadence vector selected the wrong payload");
          return { ...payload, cadence_transition_id: "matrix.fixture-signal-f" as RoadmapId };
        }),
    },
    {
      name: "matrix reopening transition subtype",
      roadmap: "matrix",
      logical_path: matrixPath("matrix.fixture-policy-c", "reopening_transition_id"),
      issue_codes: ["E-REFERENCE-FORBIDDEN", "E-SCHEMA-STATE"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "matrix_policy" && payload.policy_kind === "boundary" && payload.permanence === "reopenable",
        (payload) => {
          assert(payload.kind === "matrix_policy" && payload.policy_kind === "boundary", "matrix reopening vector selected the wrong payload");
          return { ...payload, reopening_transition_id: "matrix.fixture-signal-k" as RoadmapId };
        }),
    },
    {
      name: "matrix branch references existing branch instead of action",
      roadmap: "matrix",
      logical_path: `${matrixPath("matrix.fixture-upstream-a", "branches")}["branch-a-one"].action_ids`,
      issue_codes: ["E-SCHEMA-STATE"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "matrix_external_closeout" && payload.closeout_state === "waiting",
        (payload) => {
          assert(payload.kind === "matrix_external_closeout", "matrix branch vector selected the wrong payload");
          return {
            ...payload,
            branches: payload.branches.map((branch, index) => index === 0
              ? { ...branch, action_ids: [branch.branch_id] }
              : branch),
          };
        }),
    },
    {
      name: "testing watch escalation wrong existing kind",
      roadmap: "testing",
      logical_path: testingPath("testing.fixture-operational-watching", "escalation_transition_id"),
      issue_codes: ["E-REFERENCE-FORBIDDEN", "E-SCHEMA-STATE"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "testing_operational_watch" && payload.watch_state === "watching",
        (payload) => {
          assert(payload.kind === "testing_operational_watch", "testing watch vector selected the wrong payload");
          return { ...payload, escalation_transition_id: "testing.fixture-evidence-gate" as RoadmapId };
        }),
    },
    {
      name: "testing incident evidence wrong existing kind",
      roadmap: "testing",
      logical_path: testingPath("testing.fixture-incident-live", "evidence_ids"),
      issue_codes: ["E-REFERENCE-FORBIDDEN", "E-SCHEMA-STATE"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "testing_incident" && payload.incident_posture === "live",
        (payload) => {
          assert(payload.kind === "testing_incident", "testing incident vector selected the wrong payload");
          return { ...payload, evidence_ids: ["testing.fixture-task-ready" as RoadmapId] };
        }),
    },
    {
      name: "testing cost evidence wrong existing kind",
      roadmap: "testing",
      logical_path: testingPath("testing.fixture-cost-historical", "evidence_ids"),
      issue_codes: ["E-REFERENCE-FORBIDDEN", "E-SCHEMA-STATE"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "testing_cost" && payload.cost_posture === "historical_observation",
        (payload) => {
          assert(payload.kind === "testing_cost" && payload.cost_posture === "historical_observation", "testing cost vector selected the wrong payload");
          return { ...payload, evidence_ids: ["testing.fixture-task-ready" as RoadmapId] };
        }),
    },
    {
      name: "testing admission evidence wrong existing kind",
      roadmap: "testing",
      logical_path: testingPath("testing.fixture-admission-silent", "evidence_ids"),
      issue_codes: ["E-REFERENCE-FORBIDDEN", "E-SCHEMA-STATE"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "testing_system_admission" && payload.admission_kind === "silent_corruption",
        (payload) => {
          assert(payload.kind === "testing_system_admission", "testing admission evidence vector selected the wrong payload");
          return { ...payload, evidence_ids: ["testing.fixture-task-ready" as RoadmapId] };
        }),
    },
    {
      name: "testing admission incident wrong existing kind",
      roadmap: "testing",
      logical_path: testingPath("testing.fixture-admission-independent", "incident_ids"),
      issue_codes: ["E-REFERENCE-FORBIDDEN", "E-SCHEMA-STATE"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "testing_system_admission" && payload.admission_kind === "independent_recurrence",
        (payload) => {
          assert(payload.kind === "testing_system_admission" && payload.admission_kind === "independent_recurrence", "testing admission incident vector selected the wrong payload");
          return { ...payload, incident_ids: ["testing.fixture-evidence-gate" as RoadmapId] };
        }),
    },
    {
      name: "testing admission family wrong existing kind",
      roadmap: "testing",
      logical_path: testingPath("testing.fixture-admission-bounded", "family_id"),
      issue_codes: ["E-REFERENCE-FORBIDDEN", "E-SCHEMA-STATE"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "testing_system_admission" && payload.admission_kind === "bounded_denominator",
        (payload) => {
          assert(payload.kind === "testing_system_admission" && payload.admission_kind === "bounded_denominator", "testing admission family vector selected the wrong payload");
          return { ...payload, family_id: "testing.fixture-task-ready" as RoadmapId };
        }),
    },
    {
      name: "testing admission cost wrong existing kind",
      roadmap: "testing",
      logical_path: testingPath("testing.fixture-admission-bounded", "cost_record_id"),
      issue_codes: ["E-REFERENCE-FORBIDDEN", "E-SCHEMA-STATE"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "testing_system_admission" && payload.admission_kind === "bounded_denominator",
        (payload) => {
          assert(payload.kind === "testing_system_admission" && payload.admission_kind === "bounded_denominator", "testing admission cost vector selected the wrong payload");
          return { ...payload, cost_record_id: "testing.fixture-incident-live" as RoadmapId };
        }),
    },
    {
      name: "shared work family join wrong existing kind",
      roadmap: "matrix",
      logical_path: matrixPath("matrix.fixture-task-a", "family_id"),
      issue_codes: ["E-REFERENCE-FORBIDDEN"],
      mutate: (document) => replacePayload(document,
        (payload) => payload.kind === "work" && payload.work_state === "ready" && payload.work_kind === "defect",
        (payload) => {
          assert(payload.kind === "work" && payload.work_state === "ready", "shared family vector selected the wrong payload");
          return { ...payload, family_id: "matrix.fixture-task-b" as RoadmapId };
        }),
    },
  ];

  for (const vector of vectors) {
    const sourcePath = vector.roadmap === "matrix" ? "all-fields/matrix-v1.toml" : "all-fields/testing-v1.toml";
    const base = productionDocument(decoded(bundle, sourcePath, vector.roadmap) as RoadmapDocumentV1);
    const mutated = vector.mutate(base);
    const view = registryView(bundle, mutated.document, vector.roadmap === "matrix" ? allFieldsStatusInputs() : statusCompatibilityInputs());
    const result = vector.roadmap === "matrix"
      ? validateMatrixRoadmapDocument(mutated.document, view, { unresolved_migration_authority: validationAuthority(mutated.document) })
      : validateTestingRoadmapDocument(mutated.document, view, { unresolved_migration_authority: validationAuthority(mutated.document) });
    const indexed = [...result.indexes.payload_records.values()].find((provider) => provider.payload === mutated.payload);
    assert(indexed?.authority === "semantic_shadow", `${vector.name}: mutated decoded shadow identity did not reach internally built indexes`);
    for (const code of vector.issue_codes) {
      assert(result.issues.some((entry) => entry.code === code && entry.logical_path === vector.logical_path), `${vector.name}: missing ${code} at exact path ${vector.logical_path}; got ${JSON.stringify(result.issues)}`);
    }
  }
}

function testPipeline(bundle: AdapterFixtureBundle): void {
  for (const [roadmap, path, validate] of [
    ["matrix", "all-fields/matrix-v1.toml", validateMatrixRoadmapDocument],
    ["testing", "all-fields/testing-v1.toml", validateTestingRoadmapDocument],
  ] as const) {
    const document = productionDocument(decoded(bundle, path, roadmap) as RoadmapDocumentV1);
    const view = registryView(bundle, document, roadmap === "matrix" ? allFieldsStatusInputs() : statusCompatibilityInputs());
    const shared: RoadmapIndexes[] = [];
    const domain: { provider: SemanticPayloadProviderFact }[] = [];
    const result = validate(document, view, {
      unresolved_migration_authority: validationAuthority(document),
      observer: {
        sharedValidationStarted(indexes) { shared.push(indexes); },
        domainPayloadValidated(provider) { domain.push({ provider }); },
      },
    });
    assert(result.issues.length === 0, `${roadmap} production validation rejected a valid decoded document: ${JSON.stringify(result.issues)}`);
    assert(shared.length === 1 && shared[0] === result.indexes, `${roadmap} shared validation did not receive the internally built indexes exactly once`);
    assert(domain.length === result.indexes.payload_records.size, `${roadmap} did not validate every payload fact`);
    assert([...result.indexes.payload_records.values()].every((provider, index) => domain[index]?.provider === provider), `${roadmap} domain validation changed payload-fact identity or order`);
    assert([...result.indexes.payload_records.values()].some((provider) => provider.authority === "semantic_shadow"), `${roadmap} fixture did not exercise semantic_shadow validation`);
    for (const provider of result.indexes.payload_records.values()) {
      const decodedPayload = "render_authority" in provider.record
        ? provider.record.render_authority === "semantic"
          ? provider.record.payload
          : provider.record.semantic_shadow
        : undefined;
      assert(provider.payload === decodedPayload, `${roadmap} domain callback did not retain decoded payload identity`);
    }

    const duplicate = { ...document, records: [...document.records, document.records[0]!] };
    let sharedCalls = 0;
    let domainCalls = 0;
    const rejected = validate(duplicate, view, {
      observer: {
        sharedValidationStarted() { sharedCalls++; },
        domainPayloadValidated() { domainCalls++; },
      },
    });
    assert(rejected.issues.some((entry) => entry.code.startsWith("E-ID-")), `${roadmap} duplicate provider did not fail in C4A`);
    assert(sharedCalls === 0 && domainCalls === 0, `${roadmap} continued to shared/domain callbacks after a C4A failure`);
  }

  testDomainMutationTable(bundle);
  testCrossRoadmapJoinSubstrate(bundle);
}

function testIndexesFromDecoded(bundle: AdapterFixtureBundle): void {
  for (const [roadmap, path, adapter] of [
    ["matrix", "positive/mixed-matrix-v1.toml", MATRIX_ADAPTER],
    ["testing", "positive/mixed-testing-v1.toml", TESTING_ADAPTER],
  ] as const) {
    const document = decoded(bundle, path, roadmap);
    const built = buildRoadmapIndexes(document);
    assert(built.issues.length === 0, `${roadmap} decoded fixture did not index cleanly`);
    const record = requireSemantic(document, (payload) => payload.kind === "work");
    assert(built.indexes.records.get(record.id) === record, `${roadmap} index copied or replaced the decoded semantic record`);
    assert(built.indexes.payload_records.get(record.id)?.payload === record.payload, `${roadmap} index did not retain the decoded payload identity`);
    const spy = fieldSpy();
    const rendered = adapter.renderSemantic(record, spy.consumer);
    assert(spy.calls.length === 4, `${roadmap} adapter did not consume all four decoded ready-work Markdown fields`);
    assert(rendered.byteLength > record.payload.summary_md.byteLength && bytesEqual(rendered.subarray(0, record.payload.summary_md.byteLength), record.payload.summary_md), `${roadmap} canonical rendering did not begin with decoded summary bytes`);
  }
  testFloors(bundle);
  testGoldenRendering(bundle);
}

function testSurface(bundle: AdapterFixtureBundle): void {
  for (const [name, adapter] of [["matrix", MATRIX_ADAPTER], ["testing", TESTING_ADAPTER]] as const) {
    const keys = Object.keys(adapter).sort(codePointSort);
    assert(!keys.includes("decodeExtension"), `${name} adapter exposes decodeExtension`);
    assert(!keys.includes("decode") && !keys.includes("parse"), `${name} adapter exposes a raw decoder hook`);
    assert(Object.isFrozen(adapter), `${name} adapter surface is mutable`);
  }
  testProviders(bundle);
  testSlots(bundle);
  const matrix = decoded(bundle, "positive/mixed-matrix-v1.toml", "matrix");
  const testing = decoded(bundle, "positive/mixed-testing-v1.toml", "testing");
  assert(requireSemantic(matrix, () => true).payload.kind === "work", "matrix branded payload missing");
  assert(requireSemantic(testing, () => true).payload.kind === "work", "testing branded payload missing");
}

function execute(id: RequiredAdapterSelfTestCaseId, bundle: AdapterFixtureBundle): void {
  switch (id) {
    case "decoder_domain_dispatch_once":
      testDecoderDispatch(bundle);
      return;
    case "adapter_surface_has_no_decode_hook":
      testSurface(bundle);
      return;
    case "pipeline_indexes_before_adapter_validation":
      testPipeline(bundle);
      return;
    case "indexes_created_from_decoded_document":
      testIndexesFromDecoded(bundle);
      return;
    case "matrix_mixed_v1_preserves_inline_slots":
      testMixedLiveMatrixInlineSlots(bundle);
      return;
    case "matrix_v0_reconstruction_visibility_arms":
      testMatrixV0ReconstructionVisibilityArms(bundle);
      return;
    case "testing_v0_reconstruction_visibility_arms":
      testTestingV0ReconstructionVisibilityArms();
      return;
  }
}

function failure(id: RequiredAdapterSelfTestCaseId, error: unknown): RoadmapIssue {
  return {
    code: "E-SELFTEST-CASE",
    source: "<selftest>",
    logical_path: id,
    message: error instanceof Error ? error.message : String(error),
    exit: 1,
  };
}

export const ADAPTER_SELFTEST_CASES: readonly SelfTestCase[] = Object.freeze(
  REQUIRED_ADAPTER_SELFTEST_CASE_IDS.map((id) => ({
    id,
    category: "adapter-pipeline" as const,
    run(context: SelfTestContext): SelfTestResult {
      const subcases = id === "decoder_domain_dispatch_once" ? ADAPTER_SELFTEST_SUBCASES : undefined;
      try {
        execute(id, fixtureBundleFromContext(context));
        return { ok: true, polarity: "positive", subcases };
      } catch (error) {
        return {
          ok: false,
          polarity: "positive",
          issues: [failure(id, error)],
          subcases,
        };
      }
    },
  })),
);

export function runAdapterDirectSelfTests(
  bundle: AdapterFixtureBundle,
): { readonly executed: 7; readonly subcases: readonly ["matrix", "testing"] } {
  for (const id of REQUIRED_ADAPTER_SELFTEST_CASE_IDS) execute(id, bundle);
  return { executed: 7, subcases: ADAPTER_SELFTEST_SUBCASES };
}
