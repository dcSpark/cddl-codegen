import type {
  SelfTestCandidateCase as SelfTestCase,
  SelfTestCandidateResult as SelfTestResult,
} from "../selftest.ts";
import type { RegistryView } from "../adapters/types.ts";
import {
  compareMigrationDebt,
  debtOwnerIndex,
  validateDebtExternalRelocationFacts,
  type DebtExternalRelocationRequest,
  type DebtOwnerKey,
  type MigrationDebt,
  type OwnerDebtState,
} from "../debt.ts";
import type { FullCommitId, PartId, ReferenceId, RepoPath, RoadmapId, SectionId, SpanId } from "../model/core.ts";
import type { RetiredIdsDocumentV1, RoadmapDocumentV2 } from "../model/documents.ts";
import {
  NORTH_STAR_STRUCTURAL_RELOCATION_GUARDS,
  validateNorthStarStructuralRelocations,
  validateWp8TestingRelocation,
  WP8_LAST_ACTIVE_AT,
  WP8_RETAINED_MEMORY_IDS,
  WP8_RETIRED_RELOCATION_GUARDS,
  WP8_RETIRED_STRUCTURAL_PART_IDS,
  WP8_RETIRED_STRUCTURAL_PART_RELOCATION_GUARDS,
} from "../relocation.ts";
import { liveTestingV2Document } from "./live_testing.ts";

const UTF8 = new TextEncoder();
function assert(condition: unknown, message: string): asserts condition { if (!condition) throw new Error(message); }
const pass = (subcases: readonly string[]): SelfTestResult => ({ ok: true, polarity: "positive", subcases });

function retiredDocument(): RetiredIdsDocumentV1 {
  return {
    retired_ids: { schema_version: 1 },
    entries: WP8_RETIRED_RELOCATION_GUARDS.map((guard) => ({
      id: guard.id as RoadmapId,
      last_active_at: WP8_LAST_ACTIVE_AT as FullCommitId,
      replacement: {
        kind: "file_heading" as const,
        path: guard.path as RepoPath,
        heading: guard.heading,
        claim_md: UTF8.encode("Current-state fact is durable at this heading.\n"),
      },
    })),
  };
}

function relocationRegistry(): RegistryView {
  const rows = new Map<string, { path: RepoPath; heading: string; claims: string[] }>();
  for (const guard of [
    ...NORTH_STAR_STRUCTURAL_RELOCATION_GUARDS,
    ...WP8_RETIRED_STRUCTURAL_PART_RELOCATION_GUARDS,
  ]) {
    const key = JSON.stringify([guard.path, guard.heading]);
    const row = rows.get(key) ?? { path: guard.path as RepoPath, heading: guard.heading, claims: [] };
    if (guard.claim_text !== undefined) row.claims.push(guard.claim_text);
    rows.set(key, row);
  }
  return {
    tracked_headings: [...rows.values()].map((row) => ({
      path: row.path,
      heading: row.heading,
      span: { start_byte: 0, end_byte: 1 },
      section_text: `${row.heading}\n${row.claims.join("\n")}\n`,
    })),
  } as unknown as RegistryView;
}

function hasPath(issues: readonly { logical_path: string }[], path: string): boolean {
  return issues.some((entry) => entry.logical_path === path);
}

function mutateRecord(
  document: RoadmapDocumentV2,
  id: string,
  mutate: (record: RoadmapDocumentV2["records"][number]) => RoadmapDocumentV2["records"][number],
): RoadmapDocumentV2 {
  return { ...document, records: document.records.map((record) => String(record.id) === id ? mutate(record) : record) };
}

const OLD_SPAN: DebtOwnerKey = {
  roadmap: "testing",
  owner_kind: "source_span",
  owner_id: "span-fixture-external-old" as SpanId,
  owner_field: "coverage",
};
const NEW_SPAN: DebtOwnerKey = {
  roadmap: "testing",
  owner_kind: "source_span",
  owner_id: "span-fixture-external-new" as SpanId,
  owner_field: "coverage",
};
const BASE_HASH = "1".repeat(64);
const CANDIDATE_HASH = "2".repeat(64);

function externalDocument(which: "base" | "candidate", includeOther = false): RoadmapDocumentV2 {
  const primary = which === "base" ? OLD_SPAN : NEW_SPAN;
  const other = which === "base" ? NEW_SPAN : OLD_SPAN;
  const hash = which === "base" ? BASE_HASH : CANDIDATE_HASH;
  const span = (key: DebtOwnerKey) => ({
    id: key.owner_id as SpanId,
    start_byte: 0,
    end_byte: 1,
    sha256: hash,
    source_kind: "record" as const,
    owner_id: "testing.fixture-external",
    owner_field: "payload.detail_md",
    migration_status: "replaced" as const,
  });
  return {
    document: {
      schema_version: 2,
      authority: "authoritative",
      roadmap: "testing",
      source_path: "tests/testing-roadmap.toml" as RepoPath,
      projection_path: "tests/TESTING_ROADMAP.md" as RepoPath,
      frozen_source_sha256: hash,
      frozen_source_byte_length: 1,
      frozen_source_line_count: 1,
      frozen_source_eof: "lf",
      projection_layout: "curated_v1",
    },
    sections: [], fragments: [], legacy_markers: [], records: [], parts: [], generated_slots: [],
    manifest: [], spans: includeOther ? [span(primary), span(other)] : [span(primary)], relations: [], references: [],
  };
}

function externalDebt(entries: readonly [DebtOwnerKey, OwnerDebtState][]): MigrationDebt {
  return {
    owners: new Map(entries.map(([key, state]) => [debtOwnerIndex(key), { key, state }])),
    independent: new Map(),
    frozen_legacy_spans: new Map(),
  };
}

function externalRequest(overrides: Partial<DebtExternalRelocationRequest> = {}): DebtExternalRelocationRequest {
  return {
    removed: OLD_SPAN,
    added: [NEW_SPAN],
    base_source: {
      source_path: "tests/testing-roadmap.toml" as RepoPath,
      sha256: BASE_HASH,
      byte_length: 1,
    },
    candidate_source: {
      source_path: "tests/testing-roadmap.toml" as RepoPath,
      sha256: CANDIDATE_HASH,
      byte_length: 1,
    },
    replacement_pin: {
      kind: "file_heading",
      path: "tests/README.md" as RepoPath,
      heading: "Coverage",
      claim_md: UTF8.encode("Current-state fact is durable here.\n"),
    },
    candidate_replacement_facts: [{
      path: "tests/README.md" as RepoPath,
      heading: "Coverage",
      span: { start_byte: 0, end_byte: 1 },
    }],
    ...overrides,
  };
}

export const REQUIRED_RELOCATION_SELFTEST_CASE_IDS = [
  "wp8_relocation_mutation_matrix",
  "debt_external_relocation_mutation_matrix",
] as const;

export const RELOCATION_SELFTEST_CASES: readonly SelfTestCase[] = Object.freeze([{
  id: "wp8_relocation_mutation_matrix",
  category: "identity-retirement",
  run(): SelfTestResult {
    const document = liveTestingV2Document();
    const retired = retiredDocument();
    assert(validateWp8TestingRelocation(document, retired).length === 0,
      "exact live WP8 relocation shape was rejected");
    assert(validateNorthStarStructuralRelocations(relocationRegistry()).length === 0,
      "exact structural relocation destinations were rejected");

    const missingRetired = { ...retired, entries: retired.entries.slice(1) };
    assert(hasPath(validateWp8TestingRelocation(document, missingRetired),
      `wp8.retired[${JSON.stringify(WP8_RETIRED_RELOCATION_GUARDS[0]!.id)}]`),
    "missing WP8 tombstone escaped");

    const wrongHash = { ...retired, entries: retired.entries.map((entry, index) => index === 0
      ? { ...entry, last_active_at: "0".repeat(40) as FullCommitId }
      : entry) };
    assert(validateWp8TestingRelocation(document, wrongHash).some((entry) => entry.code === "E-RETIRED-HASH"),
      "mis-pinned WP8 last_active_at escaped");

    const wrongDestination = { ...retired, entries: retired.entries.map((entry, index) => index === 0
      ? { ...entry, replacement: { ...entry.replacement, heading: "Wrong heading" } }
      : entry) } as RetiredIdsDocumentV1;
    assert(validateWp8TestingRelocation(document, wrongDestination)
      .some((entry) => entry.code === "E-RETIRED-REPLACEMENT"), "mis-pinned WP8 destination escaped");

    const retiredId = WP8_RETIRED_RELOCATION_GUARDS[0]!.id as RoadmapId;
    const activeResidual = {
      ...document,
      records: [...document.records, { ...document.records[0]!, id: retiredId }],
    } as RoadmapDocumentV2;
    assert(hasPath(validateWp8TestingRelocation(activeResidual, retired),
      `wp8.active[${JSON.stringify(retiredId)}]`), "active retired record escaped");

    const partId = WP8_RETIRED_STRUCTURAL_PART_IDS[0] as PartId;
    const partResidual = {
      ...document,
      parts: [...document.parts, { ...document.parts[0]!, part_id: partId }],
    } as RoadmapDocumentV2;
    assert(hasPath(validateWp8TestingRelocation(partResidual, retired),
      `wp8.part[${JSON.stringify(partId)}]`), "retired structural part escaped");

    const manifestResidual = {
      ...document,
      manifest: [...document.manifest, { kind: "part" as const, part_id: partId }],
    } as RoadmapDocumentV2;
    assert(hasPath(validateWp8TestingRelocation(manifestResidual, retired),
      `wp8.manifest[${JSON.stringify(partId)}]`), "retired manifest placement escaped");

    const spanResidual = {
      ...document,
      spans: [...document.spans, {
        ...document.spans[0]!,
        id: "span-fixture-wp8-residual" as SpanId,
        source_kind: "part" as const,
        owner_id: String(partId),
        owner_field: "body_md",
      }],
    } as RoadmapDocumentV2;
    assert(validateWp8TestingRelocation(spanResidual, retired)
      .some((entry) => entry.logical_path.includes("wp8.span")), "retired provenance span escaped");

    const relationResidual = {
      ...document,
      relations: [...document.relations, { source: retiredId, kind: "related" as const,
        target: document.records[0]!.id }],
    };
    assert(validateWp8TestingRelocation(relationResidual, retired)
      .some((entry) => entry.logical_path.includes("wp8.relation")), "retired relation endpoint escaped");

    const referenceResidual = {
      ...document,
      references: [...document.references, {
        id: "ref-fixture-wp8-residual" as ReferenceId,
        source: document.records[0]!.id,
        kind: "roadmap" as const,
        target_id: retiredId,
      }],
    };
    assert(validateWp8TestingRelocation(referenceResidual, retired)
      .some((entry) => entry.logical_path.includes("wp8.reference")), "retired typed reference escaped");

    const missingTriad = {
      ...document,
      records: document.records.filter((record) => String(record.id) !== WP8_RETAINED_MEMORY_IDS[1]),
    };
    assert(hasPath(validateWp8TestingRelocation(missingTriad, retired),
      `wp8.retained[${JSON.stringify(WP8_RETAINED_MEMORY_IDS[1])}]`), "missing retained memory triad member escaped");

    const misplacedMemory = mutateRecord(document, WP8_RETAINED_MEMORY_IDS[0], (record) => ({
      ...record,
      projection_group: "standing-system" as SectionId,
    }));
    assert(hasPath(validateWp8TestingRelocation(misplacedMemory, retired), "wp8.retained.placement"),
      "misplaced retained memory work escaped");

    const missingMemoryManifest = {
      ...document,
      manifest: document.manifest.filter((entry) =>
        entry.kind !== "record" || String(entry.record_id) !== WP8_RETAINED_MEMORY_IDS[0]),
    };
    assert(hasPath(validateWp8TestingRelocation(missingMemoryManifest, retired), "wp8.retained.manifest"),
      "missing retained memory manifest placement escaped");

    const missingMemorySpan = {
      ...document,
      spans: document.spans.filter((span) => span.id !== "span-record-tier-memory-spend-measurements"),
    };
    assert(hasPath(validateWp8TestingRelocation(missingMemorySpan, retired), "wp8.retained.span"),
      "missing retained memory span escaped");

    const registry = relocationRegistry();
    const missingHeading = { ...registry, tracked_headings: registry.tracked_headings.filter((fact) =>
      fact.path !== "tests/README.md" || fact.heading !== "Coverage") };
    assert(validateNorthStarStructuralRelocations(missingHeading)
      .some((entry) => entry.logical_path.includes("north-star")), "missing durable destination heading escaped");

    const claimGuard = NORTH_STAR_STRUCTURAL_RELOCATION_GUARDS.find((guard) => guard.claim_text !== undefined)!;
    const missingClaim = { ...registry, tracked_headings: registry.tracked_headings.map((fact) =>
      fact.path === claimGuard.path && fact.heading === claimGuard.heading
        ? { ...fact, section_text: fact.section_text!.replace(claimGuard.claim_text!, "") }
        : fact) };
    assert(validateNorthStarStructuralRelocations(missingClaim)
      .some((entry) => entry.message.includes("missing exact fact text")), "missing durable claim text escaped");

    const partGuard = WP8_RETIRED_STRUCTURAL_PART_RELOCATION_GUARDS[0]!;
    const missingPartClaim = { ...registry, tracked_headings: registry.tracked_headings.map((fact) =>
      fact.path === partGuard.path && fact.heading === partGuard.heading
        ? { ...fact, section_text: fact.section_text!.replace(partGuard.claim_text!, "") }
        : fact) };
    assert(validateNorthStarStructuralRelocations(missingPartClaim)
      .some((entry) => entry.logical_path.includes("wp8.parts")), "missing structural-part destination fact escaped");

    return pass([
      "exact", "missing_tombstone", "wrong_last_active", "wrong_destination", "active_record",
      "retired_part", "manifest", "span", "relation", "reference", "retained_triad", "retained_placement",
      "retained_manifest", "retained_span", "destination_heading", "destination_claim", "part_destination_claim",
    ]);
  },
}, {
  id: "debt_external_relocation_mutation_matrix",
  category: "debt",
  run(): SelfTestResult {
    const baseDocument = externalDocument("base");
    const candidateDocument = externalDocument("candidate");
    const baseDebt = externalDebt([[OLD_SPAN, "semantic"]]);
    const candidateDebt = externalDebt([[NEW_SPAN, "semantic"]]);
    const options = { base_document: baseDocument, candidate_document: candidateDocument };
    const request = externalRequest();
    const valid = validateDebtExternalRelocationFacts(baseDebt, candidateDebt, options, [request]);
    assert(valid.ok, `valid external relocation rejected: ${JSON.stringify(valid.issues)}`);
    assert(compareMigrationDebt(baseDebt, candidateDebt, { ...options, transition_facts: valid.facts }).length === 0,
      "valid external relocation capability did not authorize its exact debt delta");

    const empty = validateDebtExternalRelocationFacts(baseDebt, candidateDebt, options, []);
    assert(!empty.ok && empty.issues.some((entry) => entry.logical_path === "external_relocation"),
      "empty external relocation request set escaped");

    const wrongSource = validateDebtExternalRelocationFacts(baseDebt, candidateDebt, options, [externalRequest({
      base_source: { ...request.base_source, sha256: "3".repeat(64) },
    })]);
    assert(!wrongSource.ok, "wrong external relocation source fingerprint escaped");

    const missingDestination = validateDebtExternalRelocationFacts(baseDebt, candidateDebt, options, [externalRequest({
      candidate_replacement_facts: [],
    })]);
    const duplicateDestination = validateDebtExternalRelocationFacts(baseDebt, candidateDebt, options, [externalRequest({
      candidate_replacement_facts: [request.candidate_replacement_facts[0]!, request.candidate_replacement_facts[0]!],
    })]);
    assert(!missingDestination.ok && !duplicateDestination.ok,
      "missing or duplicate durable destination escaped exact-one resolution");

    const candidateWithRemovedDocument = externalDocument("candidate", true);
    const candidateWithRemovedDebt = externalDebt([[OLD_SPAN, "semantic"], [NEW_SPAN, "semantic"]]);
    assert(!validateDebtExternalRelocationFacts(baseDebt, candidateWithRemovedDebt, {
      base_document: baseDocument,
      candidate_document: candidateWithRemovedDocument,
    }, [request]).ok, "removed owner surviving in the candidate escaped");

    const rawSuccessor = externalDebt([[NEW_SPAN, "raw_unclassified"]]);
    const missingSuccessorDocument = { ...candidateDocument, spans: [] };
    const wrongRoadmapSuccessor = { ...NEW_SPAN, roadmap: "matrix" as const };
    assert(!validateDebtExternalRelocationFacts(baseDebt, rawSuccessor, options, [request]).ok,
      "non-semantic successor escaped");
    assert(!validateDebtExternalRelocationFacts(baseDebt, candidateDebt, {
      base_document: baseDocument,
      candidate_document: missingSuccessorDocument,
    }, [request]).ok, "successor absent from the candidate document escaped");
    assert(!validateDebtExternalRelocationFacts(baseDebt,
      externalDebt([[wrongRoadmapSuccessor, "semantic"]]), options,
      [externalRequest({ added: [wrongRoadmapSuccessor] })]).ok, "cross-roadmap successor escaped");

    const duplicateOverlap = validateDebtExternalRelocationFacts(baseDebt, candidateDebt, options, [request, request]);
    assert(!duplicateOverlap.ok, "duplicate external relocation ownership overlap escaped");

    const clonedCandidate = externalDocument("candidate");
    assert(compareMigrationDebt(baseDebt, candidateDebt, {
      base_document: baseDocument,
      candidate_document: clonedCandidate,
      transition_facts: valid.facts,
    }).some((entry) => entry.logical_path === "transition_facts"),
    "external relocation capability replayed against another candidate document");

    return pass([
      "positive", "empty", "source_fingerprint", "destination_missing", "destination_duplicate",
      "removed_owner_survives", "successor_state", "successor_document", "successor_roadmap",
      "duplicate_overlap", "capability_binding",
    ]);
  },
}]);
