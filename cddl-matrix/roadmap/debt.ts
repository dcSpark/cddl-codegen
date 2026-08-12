import type { RoadmapIssue } from "./errors.ts";
import type { FileHeadingFact, GateFact, TestSymbolFact } from "./adapters/types.ts";
import type {
  FragmentId,
  FullCommitId,
  MarkerId,
  PartId,
  RoadmapId,
  RoadmapName,
  SectionId,
  SpanId,
  RepoPath,
} from "./model/core.ts";
import type {
  ActiveRecordOwnerFact,
  CurrentGuard,
  CurrentGuardOwnerFact,
  IdentityOwnerFact,
  ReplacementPin,
  RoadmapDocument,
  TombstoneOwnerFact,
} from "./model/documents.ts";
import type { CompletedRenderIr, FieldConsumptionLedgerEntry, RenderChunk } from "./render_ir.ts";

export type OwnerDebtState = "raw_unclassified" | "raw_with_semantic_shadow" | "semantic";

export type DebtOwnerKey =
  | { roadmap: RoadmapName; owner_kind: "record"; owner_id: RoadmapId; owner_field: string }
  | {
      roadmap: RoadmapName;
      owner_kind: "section";
      owner_id: SectionId;
      owner_field: "source_block_md" | "body_md";
    }
  | {
      roadmap: RoadmapName;
      owner_kind: "fragment";
      owner_id: FragmentId;
      owner_field: "source_block_md" | "body_md";
    }
  | {
      roadmap: RoadmapName;
      owner_kind: "part";
      owner_id: PartId;
      owner_field: "source_block_md" | "body_md";
    }
  | {
      roadmap: RoadmapName;
      owner_kind: "legacy_marker";
      owner_id: MarkerId;
      owner_field: "source_block_md" | "marker_md";
    }
  | {
      roadmap: RoadmapName;
      owner_kind: "source_span";
      owner_id: SpanId;
      owner_field: "coverage";
    };

export type IndependentDebtCategory =
  | "raw_subordinate_lifecycles"
  | "inferred_transitions"
  | "unresolved_references"
  | "pending_family_classifications"
  | "unrendered_fields"
  | "unmodelled_coordinates";

export interface IndependentDebtKey {
  roadmap: RoadmapName;
  category: IndependentDebtCategory;
  owner: DebtOwnerKey;
  subject: string;
}

export interface MigrationDebt {
  owners: ReadonlyMap<string, { key: DebtOwnerKey; state: OwnerDebtState }>;
  independent: ReadonlyMap<string, IndependentDebtKey>;
  frozen_legacy_spans: ReadonlyMap<string, DebtOwnerKey>;
}

export interface DebtRestructureRequest {
  readonly removed: DebtOwnerKey;
  readonly added: readonly DebtOwnerKey[];
}

export interface ValidatedDebtTransitionFacts {
  readonly restructure_count: number;
  readonly retirement_count: number;
  readonly guard_transfer_count?: number;
}

export type DebtTransitionFactsInput =
  | ValidatedDebtTransitionFacts
  | readonly ValidatedDebtTransitionFacts[];

export type TombstoneEligibleIdentityOwnerFact = Extract<
  IdentityOwnerFact,
  { owner_kind: "active_record" | "current_guard" | "legacy_markdown_reservation" }
>;

export interface DebtDocumentSourceFingerprint {
  readonly source_path: RepoPath;
  readonly sha256: string;
  readonly byte_length: number;
}

export type DebtReplacementResolutionFact = GateFact | TestSymbolFact | FileHeadingFact;

export interface DebtRetirementTransitionRequest {
  readonly base_owner: TombstoneEligibleIdentityOwnerFact;
  readonly removed_debt_owners: readonly DebtOwnerKey[];
  readonly base_commit: FullCommitId;
  readonly base_source: DebtDocumentSourceFingerprint;
  readonly candidate_source: DebtDocumentSourceFingerprint;
  readonly candidate_identity_facts: readonly IdentityOwnerFact[];
  readonly candidate_tombstone: TombstoneOwnerFact;
  readonly candidate_replacement_fact?: DebtReplacementResolutionFact;
}

export interface DebtGuardTransferRequest {
  /** Must name the exact family record object held by `base_document`. */
  readonly base_owner: ActiveRecordOwnerFact;
  /** Must wrap the exact same-ID guard object in `candidate_guards`. */
  readonly candidate_guard: CurrentGuardOwnerFact;
  /** Complete candidate guard registry, including guards for systematic child IDs. */
  readonly candidate_guards: readonly CurrentGuard[];
  /** Complete candidate gate/test-symbol/file-heading provider set. */
  readonly candidate_replacement_facts: readonly DebtReplacementResolutionFact[];
}

export interface DebtComparisonOptions {
  readonly base_document: RoadmapDocument;
  readonly candidate_document: RoadmapDocument;
  /** Opaque facts minted by C3 after restructure checks or C5-supplied retirement coordinates. */
  readonly transition_facts?: DebtTransitionFactsInput;
}

export type DebtTransitionFactResult =
  | { readonly ok: true; readonly facts: ValidatedDebtTransitionFacts; readonly issues: readonly [] }
  | { readonly ok: false; readonly issues: readonly RoadmapIssue[] };

export interface DebtReport {
  readonly owner_counts: Readonly<Record<OwnerDebtState, number>>;
  readonly independent_counts: Readonly<Record<IndependentDebtCategory, number>>;
  readonly owners: readonly { key: DebtOwnerKey; state: OwnerDebtState }[];
  readonly independent: readonly IndependentDebtKey[];
}

const OWNER_RANK: Readonly<Record<OwnerDebtState, number>> = {
  semantic: 0,
  raw_with_semantic_shadow: 1,
  raw_unclassified: 2,
};

const INDEPENDENT_CATEGORIES: readonly IndependentDebtCategory[] = [
  "raw_subordinate_lifecycles",
  "inferred_transitions",
  "unresolved_references",
  "pending_family_classifications",
  "unrendered_fields",
  "unmodelled_coordinates",
];

interface PrivateDebtTransitionFacts {
  readonly base: MigrationDebt;
  readonly candidate: MigrationDebt;
  readonly base_document: RoadmapDocument;
  readonly candidate_document: RoadmapDocument;
  readonly base_signature: string;
  readonly candidate_signature: string;
  readonly base_document_signature: string;
  readonly candidate_document_signature: string;
  readonly removed: ReadonlySet<string>;
  readonly added: ReadonlySet<string>;
}

const validatedDebtTransitions = new WeakMap<object, PrivateDebtTransitionFacts>();

function debtSignature(debt: MigrationDebt): string {
  return JSON.stringify({
    owners: [...debt.owners].map(([index, value]) => [index, debtOwnerIndex(value.key), value.state])
      .sort((left, right) => codePointSort(String(left[0]), String(right[0]))),
    independent: [...debt.independent].map(([index, value]) => [index, independentDebtIndex(value)])
      .sort((left, right) => codePointSort(left[0], right[0])),
    frozen: [...debt.frozen_legacy_spans].map(([index, value]) => [index, debtOwnerIndex(value)])
      .sort((left, right) => codePointSort(left[0], right[0])),
  });
}

function documentTransitionSignature(document: RoadmapDocument): string {
  const replacementRows: string[][] = [];
  const addReplacements = (kind: string, id: string, value: object): void => {
    if (!("source_replacements" in value) || !Array.isArray(value.source_replacements)) return;
    for (const replacement of value.source_replacements) {
      replacementRows.push([kind, id, replacement.span_id, replacement.replacement_field]);
    }
  };
  for (const value of document.sections) addReplacements("section", value.section_id, value);
  for (const value of document.fragments) addReplacements("fragment", value.fragment_id, value);
  for (const value of document.legacy_markers) addReplacements("legacy_marker", value.marker_id, value);
  for (const value of document.records) addReplacements("record", value.id, value);
  for (const value of document.parts) addReplacements("part", value.part_id, value);
  const ownerRows = [
    ...document.sections.map((value) => ["section", value.section_id, "source_block_md" in value ? "source_block_md" : "body_md"]),
    ...document.fragments.map((value) => ["fragment", value.fragment_id, "source_block_md" in value ? "source_block_md" : "body_md"]),
    ...document.legacy_markers.map((value) => ["legacy_marker", value.marker_id, "source_block_md" in value ? "source_block_md" : "marker_md"]),
    ...document.records.flatMap((value) => {
      if ("source_block_md" in value) return [["record", value.id, "source_block_md"]];
      const fields: string[] = [];
      const collect = (candidate: unknown, path: string): void => {
        if (candidate instanceof Uint8Array) {
          if (path.endsWith("_md")) fields.push(path);
          return;
        }
        if (Array.isArray(candidate)) {
          candidate.forEach((item, index) => collect(item, `${path}[${index}]`));
        } else if (candidate !== null && typeof candidate === "object") {
          for (const key of Object.keys(candidate).sort(codePointSort)) {
            collect((candidate as Record<string, unknown>)[key], `${path}.${key}`);
          }
        }
      };
      collect(value.payload, "payload");
      return fields.map((field) => ["record", value.id, value.projection_visibility, field]);
    }),
    ...document.parts.map((value) => ["part", value.part_id, "source_block_md" in value ? "source_block_md" : "body_md"]),
  ].sort((left, right) => codePointSort(JSON.stringify(left), JSON.stringify(right)));
  const spanRows = document.spans.map((span) => [
    span.id,
    span.source_kind,
    span.owner_id,
    span.owner_field,
    span.migration_status,
    span.start_byte,
    span.end_byte,
    span.sha256,
  ]).sort((left, right) => codePointSort(JSON.stringify(left), JSON.stringify(right)));
  replacementRows.sort((left, right) => codePointSort(JSON.stringify(left), JSON.stringify(right)));
  return JSON.stringify({
    document: [
      document.document.roadmap,
      document.document.source_path,
      document.document.projection_path,
      document.document.schema_version,
    ],
    owners: ownerRows,
    spans: spanRows,
    replacements: replacementRows,
  });
}

const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

export function debtOwnerIndex(key: DebtOwnerKey): string {
  return JSON.stringify([key.roadmap, key.owner_kind, key.owner_id, key.owner_field]);
}

export function independentDebtIndex(key: IndependentDebtKey): string {
  return JSON.stringify([
    key.roadmap,
    key.category,
    key.owner.owner_kind,
    key.owner.owner_id,
    key.owner.owner_field,
    key.subject,
  ]);
}

function issue(
  options: DebtComparisonOptions,
  code: Extract<RoadmapIssue["code"], `E-DEBT-${string}`>,
  logical_path: string,
  message: string,
): RoadmapIssue {
  return {
    code,
    source: options.candidate_document.document.source_path,
    logical_path,
    message,
    exit: 1,
  };
}

function addOwner(
  owners: Map<string, { key: DebtOwnerKey; state: OwnerDebtState }>,
  key: DebtOwnerKey,
  state: OwnerDebtState,
): void {
  owners.set(debtOwnerIndex(key), { key: Object.freeze({ ...key }), state });
}

function chunkByOwner(chunks: readonly RenderChunk[]): Map<string, RenderChunk> {
  return new Map(chunks.map((chunk) => [JSON.stringify([chunk.owner.kind, chunk.owner.id]), chunk]));
}

function recordRawState(record: object): OwnerDebtState {
  return "semantic_shadow" in record && record.semantic_shadow !== undefined
    ? "raw_with_semantic_shadow"
    : "raw_unclassified";
}

function ownerForRecord(
  document: RoadmapDocument,
  recordId: RoadmapId,
  owners: ReadonlyMap<string, { key: DebtOwnerKey; state: OwnerDebtState }>,
): DebtOwnerKey {
  const matches = [...owners.values()]
    .filter(({ key }) => key.owner_kind === "record" && key.owner_id === recordId)
    .sort((left, right) => codePointSort(left.key.owner_field, right.key.owner_field));
  return matches.find(({ key }) => key.owner_field === "payload.summary_md")?.key ?? matches[0]?.key ?? {
    roadmap: document.document.roadmap,
    owner_kind: "record",
    owner_id: recordId,
    owner_field: "source_block_md",
  };
}

function addIndependent(
  independent: Map<string, IndependentDebtKey>,
  value: IndependentDebtKey,
): void {
  independent.set(independentDebtIndex(value), Object.freeze({
    ...value,
    owner: Object.freeze({ ...value.owner }),
  }));
}

function unrenderedFromLedger(
  document: RoadmapDocument,
  ledger: FieldConsumptionLedgerEntry,
  owner: DebtOwnerKey,
  independent: Map<string, IndependentDebtKey>,
): void {
  const counts = new Map<string, number>();
  for (const field of ledger.consumed_fields) counts.set(field, (counts.get(field) ?? 0) + 1);
  for (const field of ledger.expected_fields) {
    if ((counts.get(field) ?? 0) === 1) continue;
    addIndependent(independent, {
      roadmap: document.document.roadmap,
      category: "unrendered_fields",
      owner,
      subject: field,
    });
  }
}

/** Derive owner atoms from decoded values and completed render ledgers, never from raw wire data. */
export function deriveMigrationDebt(
  document: RoadmapDocument,
  completed: CompletedRenderIr,
  additionalIndependent: readonly IndependentDebtKey[] = [],
): MigrationDebt {
  const roadmap = document.document.roadmap;
  const owners = new Map<string, { key: DebtOwnerKey; state: OwnerDebtState }>();
  const independent = new Map<string, IndependentDebtKey>();
  const frozen = new Map<string, DebtOwnerKey>();
  const chunks = chunkByOwner(completed.chunks);

  for (const section of document.sections) {
    const raw = "source_block_md" in section;
    addOwner(owners, {
      roadmap,
      owner_kind: "section",
      owner_id: section.section_id,
      owner_field: raw ? "source_block_md" : "body_md",
    }, raw ? "raw_unclassified" : "semantic");
  }
  for (const fragment of document.fragments) {
    const raw = "source_block_md" in fragment;
    const key: DebtOwnerKey = {
      roadmap,
      owner_kind: "fragment",
      owner_id: fragment.fragment_id,
      owner_field: raw ? "source_block_md" : "body_md",
    };
    addOwner(owners, key, raw ? "raw_unclassified" : "semantic");
    if (raw) addIndependent(independent, {
      roadmap,
      category: "raw_subordinate_lifecycles",
      owner: key,
      subject: "raw-fragment-lifecycle",
    });
  }
  for (const marker of document.legacy_markers) {
    const raw = "source_block_md" in marker;
    const key: DebtOwnerKey = {
      roadmap,
      owner_kind: "legacy_marker",
      owner_id: marker.marker_id,
      owner_field: raw ? "source_block_md" : "marker_md",
    };
    addOwner(owners, key, raw ? "raw_unclassified" : "semantic");
    if (raw) addIndependent(independent, {
      roadmap,
      category: "raw_subordinate_lifecycles",
      owner: key,
      subject: "raw-marker-lifecycle",
    });
  }
  for (const part of document.parts) {
    const raw = "source_block_md" in part;
    const key: DebtOwnerKey = {
      roadmap,
      owner_kind: "part",
      owner_id: part.part_id,
      owner_field: raw ? "source_block_md" : "body_md",
    };
    addOwner(owners, key, raw ? "raw_unclassified" : "semantic");
    if (raw) addIndependent(independent, {
      roadmap,
      category: "raw_subordinate_lifecycles",
      owner: key,
      subject: "raw-part-lifecycle",
    });
  }

  for (const record of document.records) {
    if ("source_block_md" in record) {
      addOwner(owners, {
        roadmap,
        owner_kind: "record",
        owner_id: record.id,
        owner_field: "source_block_md",
      }, recordRawState(record));
      continue;
    }
    const chunk = chunks.get(JSON.stringify(["record", record.id]));
    const ledger = completed.field_consumption.find((value) =>
      value.owner_kind === "record" && value.owner_id === record.id
    );
    // Owner atoms describe the decoded semantic fields, even when rendering failed to consume one;
    // the independent unrendered_fields tuple records that separate failure.
    for (const field of ledger?.expected_fields ?? chunk?.consumed_fields ?? []) {
      addOwner(owners, {
        roadmap,
        owner_kind: "record",
        owner_id: record.id,
        owner_field: field,
      }, "semantic");
    }
    if (record.payload.kind === "work" && record.payload.family_classification === "pending") {
      const owner = ownerForRecord(document, record.id, owners);
      addIndependent(independent, {
        roadmap,
        category: "pending_family_classifications",
        owner,
        subject: "payload.family_classification",
      });
    }
    if (
      record.payload.kind === "family" && record.payload.family_maturity === "under_design" &&
      record.payload.denominator_unknowns_md !== undefined
    ) {
      const owner = ownerForRecord(document, record.id, owners);
      addIndependent(independent, {
        roadmap,
        category: "unmodelled_coordinates",
        owner,
        subject: "payload.denominator_unknowns_md",
      });
    }
  }

  for (const span of document.spans) {
    let state: OwnerDebtState = "semantic";
    if (span.migration_status === "raw") {
      const chunk = completed.chunks.find((value) => value.source_span_ids.includes(span.id));
      if (chunk?.owner.kind === "record") {
        const record = document.records.find((value) => value.id === chunk.owner.id);
        state = record !== undefined && "source_block_md" in record
          ? recordRawState(record)
          : "raw_unclassified";
      } else state = "raw_unclassified";
    }
    const key: DebtOwnerKey = {
      roadmap,
      owner_kind: "source_span",
      owner_id: span.id,
      owner_field: "coverage",
    };
    addOwner(owners, key, state);
  }

  for (const ledger of completed.field_consumption) {
    const owner = ledger.owner_kind === "record"
      ? ownerForRecord(document, ledger.owner_id as RoadmapId, owners)
      : [...owners.values()].find(({ key }) =>
        key.owner_kind === ledger.owner_kind && key.owner_id === ledger.owner_id
      )?.key;
    if (owner !== undefined) unrenderedFromLedger(document, ledger, owner, independent);
  }

  if ("references" in document) {
    for (const spanId of document.document.frozen_legacy_span_ids) {
      const key: DebtOwnerKey = {
        roadmap,
        owner_kind: "source_span",
        owner_id: spanId,
        owner_field: "coverage",
      };
      frozen.set(debtOwnerIndex(key), key);
    }
    for (const reference of document.references) {
      if (reference.kind !== "unresolved_migration") continue;
      const owner = ownerForRecord(document, reference.source, owners);
      addIndependent(independent, {
        roadmap,
        category: "unresolved_references",
        owner,
        subject: reference.local_reference,
      });
    }
  } else {
    for (const span of document.spans.filter((value) => value.migration_status === "raw")) {
      const key: DebtOwnerKey = {
        roadmap,
        owner_kind: "source_span",
        owner_id: span.id,
        owner_field: "coverage",
      };
      frozen.set(debtOwnerIndex(key), key);
    }
  }
  for (const value of additionalIndependent) addIndependent(independent, value);

  return Object.freeze({
    owners,
    independent,
    frozen_legacy_spans: frozen,
  });
}

function valueAtPath(value: unknown, path: readonly string[]): unknown {
  let current = value;
  for (const component of path) {
    const field = /^[a-z][a-z0-9_]*/.exec(component)?.[0];
    if (field === undefined || current === null || typeof current !== "object" || !(field in current)) {
      return undefined;
    }
    current = (current as Record<string, unknown>)[field];
    const suffix = component.slice(field.length);
    let offset = 0;
    for (const match of suffix.matchAll(/\[([0-9]+)\]/g)) {
      if (match.index !== offset || !Array.isArray(current)) return undefined;
      current = current[Number(match[1])];
      offset += match[0].length;
    }
    if (offset !== suffix.length) return undefined;
  }
  return current;
}

function documentHasOwner(document: RoadmapDocument, key: DebtOwnerKey): boolean {
  if (key.roadmap !== document.document.roadmap) return false;
  switch (key.owner_kind) {
    case "source_span":
      return key.owner_field === "coverage" && document.spans.some((span) => span.id === key.owner_id);
    case "section": {
      const value = document.sections.find((entry) => entry.section_id === key.owner_id);
      return value !== undefined && key.owner_field in value;
    }
    case "fragment": {
      const value = document.fragments.find((entry) => entry.fragment_id === key.owner_id);
      return value !== undefined && key.owner_field in value;
    }
    case "part": {
      const value = document.parts.find((entry) => entry.part_id === key.owner_id);
      return value !== undefined && key.owner_field in value;
    }
    case "legacy_marker": {
      const value = document.legacy_markers.find((entry) => entry.marker_id === key.owner_id);
      return value !== undefined && key.owner_field in value;
    }
    case "record": {
      const value = document.records.find((entry) => entry.id === key.owner_id);
      if (value === undefined) return false;
      if (key.owner_field === "source_block_md") return "source_block_md" in value;
      if (!key.owner_field.startsWith("payload.") || !("payload" in value)) return false;
      const field = valueAtPath(value, key.owner_field.split("."));
      return field instanceof Uint8Array && key.owner_field.endsWith("_md");
    }
  }
}

function ownerSpans(document: RoadmapDocument, key: DebtOwnerKey): readonly SpanId[] {
  if (key.owner_kind === "source_span") {
    return document.spans.some((span) => span.id === key.owner_id) ? Object.freeze([key.owner_id]) : [];
  }
  return Object.freeze(document.spans.filter((span) =>
    span.source_kind === key.owner_kind && span.owner_id === key.owner_id &&
    span.owner_field === key.owner_field
  ).map((span) => span.id));
}

function replacementMatches(document: RoadmapDocument, key: DebtOwnerKey, spanId: SpanId): boolean {
  if (key.owner_kind === "source_span") return false;
  const values = key.owner_kind === "record" ? document.records
    : key.owner_kind === "section" ? document.sections
      : key.owner_kind === "fragment" ? document.fragments
        : key.owner_kind === "part" ? document.parts
          : document.legacy_markers;
  const value = values.find((candidate) => {
    if (key.owner_kind === "record") return "id" in candidate && candidate.id === key.owner_id;
    if (key.owner_kind === "section") return "section_id" in candidate && candidate.section_id === key.owner_id;
    if (key.owner_kind === "fragment") return "fragment_id" in candidate && candidate.fragment_id === key.owner_id;
    if (key.owner_kind === "part") return "part_id" in candidate && candidate.part_id === key.owner_id;
    return "marker_id" in candidate && candidate.marker_id === key.owner_id;
  });
  if (value === undefined || !("source_replacements" in value) || !Array.isArray(value.source_replacements)) {
    return false;
  }
  return value.source_replacements.filter((replacement) =>
    replacement.span_id === spanId && replacement.replacement_field === key.owner_field
  ).length === 1;
}

/**
 * Validate requested restructures against exact base/candidate owner and replacement-span facts.
 * The returned capability is bound by identity to this comparison; caller-shaped objects are inert.
 */
export function validateDebtTransitionFacts(
  base: MigrationDebt,
  candidate: MigrationDebt,
  options: Pick<DebtComparisonOptions, "base_document" | "candidate_document">,
  requests: readonly DebtRestructureRequest[],
): DebtTransitionFactResult {
  const comparisonOptions: DebtComparisonOptions = options;
  const issues: RoadmapIssue[] = [];
  const removed = new Set<string>();
  const added = new Set<string>();
  for (const [requestIndex, request] of requests.entries()) {
    const logicalPath = `restructure[${requestIndex}]`;
    const removedIndex = debtOwnerIndex(request.removed);
    if (request.added.length === 0) {
      issues.push(issue(comparisonOptions, "E-DEBT-OWNER-REGRESSION", logicalPath, "restructure must add at least one validated semantic owner"));
      continue;
    }
    if (
      removed.has(removedIndex) || !base.owners.has(removedIndex) || candidate.owners.has(removedIndex) ||
      !documentHasOwner(options.base_document, request.removed) ||
      documentHasOwner(options.candidate_document, request.removed)
    ) {
      issues.push(issue(comparisonOptions, "E-DEBT-OWNER-REGRESSION", logicalPath, "removed owner is not one exact base-only document/debt fact"));
      continue;
    }
    const baseSpans = ownerSpans(options.base_document, request.removed);
    if (baseSpans.length === 0) {
      issues.push(issue(comparisonOptions, "E-DEBT-OWNER-REGRESSION", logicalPath, "removed owner has no exact base span fact"));
      continue;
    }
    const requestAdded = new Set<string>();
    const requestRemoved = new Set<string>([removedIndex]);
    let valid = true;
    for (const addedKey of request.added) {
      const addedIndex = debtOwnerIndex(addedKey);
      const candidateOwner = candidate.owners.get(addedIndex);
      const candidateSpans = ownerSpans(options.candidate_document, addedKey);
      if (
        requestAdded.has(addedIndex) || added.has(addedIndex) || base.owners.has(addedIndex) ||
        candidateOwner?.state !== "semantic" || !documentHasOwner(options.candidate_document, addedKey) ||
        addedKey.roadmap !== request.removed.roadmap ||
        addedKey.owner_kind !== request.removed.owner_kind || addedKey.owner_id !== request.removed.owner_id ||
        candidateSpans.length !== 1 || !replacementMatches(options.candidate_document, addedKey, candidateSpans[0])
      ) {
        valid = false;
        break;
      }
      requestAdded.add(addedIndex);
      for (const spanId of candidateSpans) {
        const spanKey: DebtOwnerKey = {
          roadmap: addedKey.roadmap,
          owner_kind: "source_span",
          owner_id: spanId,
          owner_field: "coverage",
        };
        const spanIndex = debtOwnerIndex(spanKey);
        if (base.owners.has(spanIndex) || candidate.owners.get(spanIndex)?.state !== "semantic") {
          valid = false;
          break;
        }
        requestAdded.add(spanIndex);
      }
      if (!valid) break;
    }
    for (const spanId of baseSpans) {
      const spanKey: DebtOwnerKey = {
        roadmap: request.removed.roadmap,
        owner_kind: "source_span",
        owner_id: spanId,
        owner_field: "coverage",
      };
      const spanIndex = debtOwnerIndex(spanKey);
      if (base.owners.has(spanIndex) && !candidate.owners.has(spanIndex)) requestRemoved.add(spanIndex);
    }
    if (!valid) {
      issues.push(issue(comparisonOptions, "E-DEBT-OWNER-REGRESSION", logicalPath, "added owners are not unique semantic candidate-only field/span transitions for the removed owner"));
      continue;
    }
    for (const removedIndex of requestRemoved) removed.add(removedIndex);
    for (const addedIndex of requestAdded) added.add(addedIndex);
  }
  if (issues.length > 0) return Object.freeze({ ok: false, issues: Object.freeze(issues) });
  const facts: ValidatedDebtTransitionFacts = Object.freeze({
    restructure_count: requests.length,
    retirement_count: 0,
  });
  validatedDebtTransitions.set(facts, {
    base,
    candidate,
    base_document: options.base_document,
    candidate_document: options.candidate_document,
    base_signature: debtSignature(base),
    candidate_signature: debtSignature(candidate),
    base_document_signature: documentTransitionSignature(options.base_document),
    candidate_document_signature: documentTransitionSignature(options.candidate_document),
    removed,
    added,
  });
  return Object.freeze({ ok: true, facts, issues: Object.freeze([]) as readonly [] });
}

function fingerprintMatches(
  fingerprint: DebtDocumentSourceFingerprint,
  document: RoadmapDocument,
): boolean {
  return fingerprint.source_path === document.document.source_path &&
    fingerprint.sha256 === document.document.frozen_source_sha256 &&
    fingerprint.byte_length === document.document.frozen_source_byte_length &&
    /^[0-9a-f]{64}$/.test(fingerprint.sha256) &&
    Number.isSafeInteger(fingerprint.byte_length) && fingerprint.byte_length > 0;
}

function pinIndex(pin: ReplacementPin): string {
  const claim = new Bun.CryptoHasher("sha256").update(pin.claim_md).digest("hex");
  if (pin.kind === "gate") return JSON.stringify([pin.kind, pin.gate_id, claim]);
  if (pin.kind === "test_symbol") return JSON.stringify([pin.kind, pin.test_id, pin.symbol, claim]);
  return JSON.stringify([pin.kind, pin.path, pin.heading, claim]);
}

function replacementResolves(pin: ReplacementPin, fact: DebtReplacementResolutionFact | undefined): boolean {
  if (fact === undefined || pin.claim_md.byteLength === 0) return false;
  if (pin.kind === "gate") return "stub" in fact && fact.id === pin.gate_id && !fact.stub;
  if (pin.kind === "test_symbol") {
    return "test_id" in fact && fact.test_id === pin.test_id && fact.symbol === pin.symbol;
  }
  return pin.path !== "draft" && !pin.path.startsWith("draft/") &&
    "heading" in fact && fact.path === pin.path && fact.heading === pin.heading;
}

function replacementResolvesExactlyOnce(
  pin: ReplacementPin,
  facts: readonly DebtReplacementResolutionFact[],
): boolean {
  return facts.filter((fact) => replacementResolves(pin, fact)).length === 1;
}

type SystematicProviderKind =
  | "family_axis"
  | "family_axis_value"
  | "family_evidence_requirement"
  | "family_cell"
  | "family_exclusion";

interface SystematicProvider {
  readonly id: RoadmapId;
  readonly kind: SystematicProviderKind;
  readonly owner_record_id: RoadmapId;
}

function recordPayload(record: ActiveRecordOwnerFact["record"]): unknown {
  if ("payload" in record) return record.payload;
  return record.semantic_shadow;
}

function familySystematicProviders(
  ownerRecordId: RoadmapId,
  payload: unknown,
): readonly SystematicProvider[] | undefined {
  if (payload === null || typeof payload !== "object" || !("kind" in payload) || payload.kind !== "family" ||
    !("axes" in payload) || !Array.isArray(payload.axes) ||
    !("evidence_requirements" in payload) || !Array.isArray(payload.evidence_requirements) ||
    !("cells" in payload) || !Array.isArray(payload.cells) ||
    !("exclusions" in payload) || !Array.isArray(payload.exclusions)) return undefined;
  const providers: SystematicProvider[] = [];
  for (const axis of payload.axes) {
    if (axis === null || typeof axis !== "object" || !("id" in axis) || !("values" in axis) ||
      !Array.isArray(axis.values)) return undefined;
    providers.push({ id: axis.id as RoadmapId, kind: "family_axis", owner_record_id: ownerRecordId });
    for (const value of axis.values) {
      if (value === null || typeof value !== "object" || !("id" in value)) return undefined;
      providers.push({
        id: value.id as RoadmapId,
        kind: "family_axis_value",
        owner_record_id: ownerRecordId,
      });
    }
  }
  for (const value of payload.evidence_requirements) {
    if (value === null || typeof value !== "object" || !("id" in value)) return undefined;
    providers.push({
      id: value.id as RoadmapId,
      kind: "family_evidence_requirement",
      owner_record_id: ownerRecordId,
    });
  }
  for (const value of payload.cells) {
    if (value === null || typeof value !== "object" || !("id" in value)) return undefined;
    providers.push({ id: value.id as RoadmapId, kind: "family_cell", owner_record_id: ownerRecordId });
  }
  for (const value of payload.exclusions) {
    if (value === null || typeof value !== "object" || !("id" in value)) return undefined;
    providers.push({
      id: value.id as RoadmapId,
      kind: "family_exclusion",
      owner_record_id: ownerRecordId,
    });
  }
  providers.sort((left, right) => codePointSort(left.id, right.id) || codePointSort(left.kind, right.kind));
  return Object.freeze(providers);
}

function documentSystematicProviders(document: RoadmapDocument): readonly SystematicProvider[] {
  const providers: SystematicProvider[] = [];
  for (const record of document.records) {
    if (!("render_authority" in record)) continue;
    const payload = "payload" in record ? record.payload : record.semantic_shadow;
    const familyProviders = familySystematicProviders(record.id, payload);
    if (familyProviders !== undefined) providers.push(...familyProviders);
  }
  providers.sort((left, right) => codePointSort(left.id, right.id) || codePointSort(left.kind, right.kind) ||
    codePointSort(left.owner_record_id, right.owner_record_id));
  return Object.freeze(providers);
}

function activeRecordIsEligible(
  owner: Extract<TombstoneEligibleIdentityOwnerFact, { owner_kind: "active_record" }>,
  document: RoadmapDocument,
): boolean {
  if (!document.records.some((record) => record === owner.record && record.id === owner.id)) return false;
  const record = owner.record;
  if ("payload" in record) return record.payload.kind !== "family";
  if ("semantic_shadow" in record && record.semantic_shadow !== undefined) {
    return record.semantic_shadow.kind !== "family";
  }
  return true;
}

function reservationIsEligible(
  owner: Extract<TombstoneEligibleIdentityOwnerFact, { owner_kind: "legacy_markdown_reservation" }>,
  document: RoadmapDocument,
): boolean {
  const reservation = owner.reservation;
  if (
    reservation.id !== owner.id || reservation.work_kind !== owner.work_kind ||
    reservation.roadmap_path !== document.document.projection_path ||
    reservation.whole_source_sha256 !== document.document.frozen_source_sha256 ||
    reservation.source_start_byte < 0 || reservation.source_start_byte >= reservation.source_end_byte ||
    reservation.source_end_byte > document.document.frozen_source_byte_length
  ) return false;
  const exactSpan = document.spans.some((span) =>
    span.start_byte === reservation.source_start_byte && span.end_byte === reservation.source_end_byte &&
    span.sha256 === reservation.source_sha256
  );
  if (!exactSpan) return false;
  const shadow = owner.corroborating_shadow;
  return shadow === undefined || (
    shadow.id === owner.id && shadow.namespace === owner.namespace &&
    shadow.source_path === document.document.source_path && shadow.legacy_span_ids.length > 0 &&
    shadow.legacy_span_ids.every((spanId) => document.spans.some((span) => span.id === spanId))
  );
}

function eligibleBaseLifecycleOwner(
  owner: IdentityOwnerFact,
  document: RoadmapDocument,
): owner is TombstoneEligibleIdentityOwnerFact {
  if (
    owner.owner_kind !== "active_record" && owner.owner_kind !== "current_guard" &&
    owner.owner_kind !== "legacy_markdown_reservation"
  ) return false;
  if (
    owner.namespace !== document.document.roadmap || owner.id !== (owner.owner_kind === "current_guard" ? owner.guard.id : owner.owner_kind === "legacy_markdown_reservation" ? owner.reservation.id : owner.record.id) ||
    !String(owner.id).startsWith(`${owner.namespace}.`)
  ) return false;
  if (owner.owner_kind === "active_record") return activeRecordIsEligible(owner, document);
  if (owner.owner_kind === "legacy_markdown_reservation") return reservationIsEligible(owner, document);
  return owner.guard.id === owner.id && pinIndex(owner.guard.replacement_pin).length > 0;
}

function exactRemovedDebtIndexes(
  base: MigrationDebt,
  candidate: MigrationDebt,
  owner: TombstoneEligibleIdentityOwnerFact,
): readonly string[] {
  return [...base.owners].filter(([, value]) =>
    value.key.roadmap === owner.namespace && value.key.owner_kind === "record" &&
    value.key.owner_id === owner.id && !candidate.owners.has(debtOwnerIndex(value.key))
  ).map(([index]) => index).sort(codePointSort);
}

/**
 * C5-facing retirement seam. It validates lifecycle eligibility and candidate identity/replacement
 * joins itself, then mints the private comparison brand; caller-shaped coordinates are inert.
 */
export function validateDebtRetirementFacts(
  base: MigrationDebt,
  candidate: MigrationDebt,
  options: Pick<DebtComparisonOptions, "base_document" | "candidate_document">,
  requests: readonly DebtRetirementTransitionRequest[],
): DebtTransitionFactResult {
  const comparisonOptions: DebtComparisonOptions = options;
  const issues: RoadmapIssue[] = [];
  const removed = new Set<string>();
  const added = new Set<string>();
  if (requests.length === 0) {
    issues.push(issue(comparisonOptions, "E-DEBT-OWNER-REGRESSION", "retirement", "retirement transition fact set is empty"));
  }
  for (const [requestIndex, request] of requests.entries()) {
    const logicalPath = `retirement[${requestIndex}]`;
    const baseOwner = request.base_owner as IdentityOwnerFact;
    const eligible = eligibleBaseLifecycleOwner(baseOwner, options.base_document);
    const sameIdCandidate = request.candidate_identity_facts.filter((fact) => fact.id === baseOwner.id);
    const tombstone = request.candidate_tombstone;
    const tombstoneJoined = sameIdCandidate.length === 1 && sameIdCandidate[0].owner_kind === "tombstone" &&
      sameIdCandidate[0] === tombstone && tombstone.id === baseOwner.id &&
      tombstone.namespace === baseOwner.namespace && tombstone.tombstone.id === baseOwner.id &&
      tombstone.tombstone.last_active_at === request.base_commit && /^[0-9a-f]{40}(?:[0-9a-f]{24})?$/.test(request.base_commit) &&
      replacementResolves(tombstone.tombstone.replacement, request.candidate_replacement_fact);
    const guardPinJoined = baseOwner.owner_kind !== "current_guard" ||
      pinIndex(baseOwner.guard.replacement_pin) === pinIndex(tombstone.tombstone.replacement);
    const expectedRemoved = eligible ? exactRemovedDebtIndexes(base, candidate, baseOwner) : [];
    const requestedRemoved = request.removed_debt_owners.map(debtOwnerIndex).sort(codePointSort);
    const removalsExact = expectedRemoved.length === requestedRemoved.length &&
      expectedRemoved.every((index, position) => index === requestedRemoved[position]) &&
      request.removed_debt_owners.every((key) =>
        key.owner_kind === "record" && key.owner_id === baseOwner.id && key.roadmap === baseOwner.namespace &&
        base.owners.has(debtOwnerIndex(key)) && !candidate.owners.has(debtOwnerIndex(key)) &&
        documentHasOwner(options.base_document, key) && !documentHasOwner(options.candidate_document, key)
      );
    if (
      !eligible || !fingerprintMatches(request.base_source, options.base_document) ||
      !fingerprintMatches(request.candidate_source, options.candidate_document) || !tombstoneJoined ||
      !guardPinJoined || !removalsExact ||
      (baseOwner.owner_kind !== "current_guard" && expectedRemoved.length === 0) ||
      requestedRemoved.some((index) => removed.has(index))
    ) {
      issues.push(issue(
        comparisonOptions,
        "E-DEBT-OWNER-REGRESSION",
        logicalPath,
        "retirement lacks one eligible lifecycle owner or exact source/tombstone/guard/replacement/debt joins",
      ));
      continue;
    }
    for (const removedIndex of requestedRemoved) {
      removed.add(removedIndex);
      const removedKey = base.owners.get(removedIndex)?.key;
      if (removedKey === undefined) continue;
      for (const spanId of ownerSpans(options.base_document, removedKey)) {
        const spanIndex = debtOwnerIndex({
          roadmap: removedKey.roadmap,
          owner_kind: "source_span",
          owner_id: spanId,
          owner_field: "coverage",
        });
        if (base.owners.has(spanIndex) && !candidate.owners.has(spanIndex)) removed.add(spanIndex);
      }
    }
  }
  if (issues.length > 0) return Object.freeze({ ok: false, issues: Object.freeze(issues) });
  const facts: ValidatedDebtTransitionFacts = Object.freeze({
    restructure_count: 0,
    retirement_count: requests.length,
  });
  validatedDebtTransitions.set(facts, {
    base,
    candidate,
    base_document: options.base_document,
    candidate_document: options.candidate_document,
    base_signature: debtSignature(base),
    candidate_signature: debtSignature(candidate),
    base_document_signature: documentTransitionSignature(options.base_document),
    candidate_document_signature: documentTransitionSignature(options.candidate_document),
    removed,
    added,
  });
  return Object.freeze({ ok: true, facts, issues: Object.freeze([]) as readonly [] });
}

/**
 * C5 family-to-guard seam. Removed debt coordinates and systematic child IDs are derived from the
 * exact base family object; callers cannot nominate either set.
 */
export function validateDebtGuardTransferFacts(
  base: MigrationDebt,
  candidate: MigrationDebt,
  options: Pick<DebtComparisonOptions, "base_document" | "candidate_document">,
  requests: readonly DebtGuardTransferRequest[],
): DebtTransitionFactResult {
  const comparisonOptions: DebtComparisonOptions = options;
  const issues: RoadmapIssue[] = [];
  const removed = new Set<string>();
  const added = new Set<string>();
  if (requests.length === 0) {
    issues.push(issue(comparisonOptions, "E-DEBT-OWNER-REGRESSION", "guard_transfer", "guard-transfer transition fact set is empty"));
  }
  const candidateProviders = documentSystematicProviders(options.candidate_document);
  for (const [requestIndex, request] of requests.entries()) {
    const logicalPath = `guard_transfer[${requestIndex}]`;
    const owner = request.base_owner;
    const guardOwner = request.candidate_guard;
    const baseRecordExact = options.base_document.records.some((record) => record === owner.record) &&
      owner.owner_kind === "active_record" && owner.id === owner.record.id &&
      owner.namespace === options.base_document.document.roadmap;
    const familyProviders = baseRecordExact
      ? familySystematicProviders(owner.id, recordPayload(owner.record))
      : undefined;
    const rootGuards = request.candidate_guards.filter((guard) => guard.id === owner.id);
    const rootGuardExact = guardOwner.owner_kind === "current_guard" && guardOwner.id === owner.id &&
      guardOwner.namespace === owner.namespace && guardOwner.guard.id === owner.id &&
      rootGuards.length === 1 && rootGuards[0] === guardOwner.guard &&
      replacementResolvesExactlyOnce(guardOwner.guard.replacement_pin, request.candidate_replacement_facts);
    const candidateRootAbsent = !options.candidate_document.records.some((record) => record.id === owner.id);
    const childIds = new Set<string>();
    let childrenProtected = familyProviders !== undefined;
    for (const provider of familyProviders ?? []) {
      if (childIds.has(provider.id)) {
        childrenProtected = false;
        break;
      }
      childIds.add(provider.id);
      const active = candidateProviders.filter((candidateProvider) =>
        candidateProvider.id === provider.id && candidateProvider.kind === provider.kind
      );
      const guards = request.candidate_guards.filter((guard) => guard.id === provider.id);
      const activeProtected = active.length === 1 && guards.length === 0;
      const guardProtected = active.length === 0 && guards.length === 1 &&
        replacementResolvesExactlyOnce(guards[0]!.replacement_pin, request.candidate_replacement_facts);
      if (!activeProtected && !guardProtected) {
        childrenProtected = false;
        break;
      }
    }

    const recordRows = [...base.owners].filter(([, value]) =>
      value.key.roadmap === owner.namespace && value.key.owner_kind === "record" &&
      value.key.owner_id === owner.id
    );
    const requestRemoved = new Set<string>();
    let removalsExact = recordRows.length > 0;
    for (const [index, value] of recordRows) {
      if (candidate.owners.has(index) || !documentHasOwner(options.base_document, value.key) ||
        documentHasOwner(options.candidate_document, value.key)) {
        removalsExact = false;
        break;
      }
      requestRemoved.add(index);
      for (const spanId of ownerSpans(options.base_document, value.key)) {
        const spanKey: DebtOwnerKey = {
          roadmap: owner.namespace,
          owner_kind: "source_span",
          owner_id: spanId,
          owner_field: "coverage",
        };
        const spanIndex = debtOwnerIndex(spanKey);
        if (!base.owners.has(spanIndex) || candidate.owners.has(spanIndex) ||
          documentHasOwner(options.candidate_document, spanKey)) {
          removalsExact = false;
          break;
        }
        requestRemoved.add(spanIndex);
      }
      if (!removalsExact) break;
    }
    if ([...requestRemoved].some((index) => removed.has(index))) removalsExact = false;
    if (!baseRecordExact || familyProviders === undefined || !rootGuardExact || !candidateRootAbsent ||
      !childrenProtected || !removalsExact) {
      issues.push(issue(
        comparisonOptions,
        "E-DEBT-OWNER-REGRESSION",
        logicalPath,
        "guard transfer lacks one exact base family, same-ID resolving root guard, complete systematic child protection, or exact derived debt removal",
      ));
      continue;
    }
    for (const index of requestRemoved) removed.add(index);
  }
  if (issues.length > 0) return Object.freeze({ ok: false, issues: Object.freeze(issues) });
  const facts: ValidatedDebtTransitionFacts = Object.freeze({
    restructure_count: 0,
    retirement_count: 0,
    guard_transfer_count: requests.length,
  });
  validatedDebtTransitions.set(facts, {
    base,
    candidate,
    base_document: options.base_document,
    candidate_document: options.candidate_document,
    base_signature: debtSignature(base),
    candidate_signature: debtSignature(candidate),
    base_document_signature: documentTransitionSignature(options.base_document),
    candidate_document_signature: documentTransitionSignature(options.candidate_document),
    removed,
    added,
  });
  return Object.freeze({ ok: true, facts, issues: Object.freeze([]) as readonly [] });
}

function transitionFacts(
  base: MigrationDebt,
  candidate: MigrationDebt,
  options: DebtComparisonOptions,
): PrivateDebtTransitionFacts | undefined {
  if (options.transition_facts === undefined) return undefined;
  const supplied = Array.isArray(options.transition_facts)
    ? options.transition_facts
    : [options.transition_facts];
  if (supplied.length === 0) return undefined;
  const removed = new Set<string>();
  const added = new Set<string>();
  const claimed = new Set<string>();
  for (const capability of supplied) {
    const facts = validatedDebtTransitions.get(capability);
    if (facts?.base !== base || facts.candidate !== candidate ||
      facts.base_document !== options.base_document || facts.candidate_document !== options.candidate_document ||
      facts.base_signature !== debtSignature(base) || facts.candidate_signature !== debtSignature(candidate) ||
      facts.base_document_signature !== documentTransitionSignature(options.base_document) ||
      facts.candidate_document_signature !== documentTransitionSignature(options.candidate_document)) return undefined;
    for (const index of [...facts.removed, ...facts.added]) {
      if (claimed.has(index)) return undefined;
      claimed.add(index);
    }
    for (const index of facts.removed) removed.add(index);
    for (const index of facts.added) added.add(index);
  }
  if (claimed.size === 0) return undefined;
  return {
    base,
    candidate,
    base_document: options.base_document,
    candidate_document: options.candidate_document,
    base_signature: debtSignature(base),
    candidate_signature: debtSignature(candidate),
    base_document_signature: documentTransitionSignature(options.base_document),
    candidate_document_signature: documentTransitionSignature(options.candidate_document),
    removed,
    added,
  };
}

function newSemanticRecord(
  key: DebtOwnerKey,
  base: MigrationDebt,
  candidateState: OwnerDebtState,
  options: DebtComparisonOptions,
): boolean {
  if (key.owner_kind !== "record" || candidateState !== "semantic") return false;
  const candidateRecord = options.candidate_document.records.find((record) => record.id === key.owner_id);
  return candidateRecord !== undefined && !("source_block_md" in candidateRecord) &&
    documentHasOwner(options.candidate_document, key) &&
    !options.base_document.records.some((record) => record.id === key.owner_id) &&
    ![...base.owners.values()].some(({ key: baseKey }) =>
    baseKey.owner_kind === "record" && baseKey.owner_id === key.owner_id
  );
}

function validateMapIndexes(
  debt: MigrationDebt,
  label: string,
  options: DebtComparisonOptions,
  issues: RoadmapIssue[],
): void {
  for (const [index, value] of debt.owners) {
    if (index !== debtOwnerIndex(value.key)) {
      issues.push(issue(options, "E-DEBT-BASE-MISMATCH", `${label}.owners`, "owner index does not match its structured key"));
    }
  }
  for (const [index, value] of debt.independent) {
    if (index !== independentDebtIndex(value) || value.roadmap !== value.owner.roadmap) {
      issues.push(issue(options, "E-DEBT-BASE-MISMATCH", `${label}.independent`, "independent index does not match its structured tuple"));
    }
  }
  for (const [index, value] of debt.frozen_legacy_spans) {
    if (index !== debtOwnerIndex(value) || value.owner_kind !== "source_span" || value.owner_field !== "coverage") {
      issues.push(issue(options, "E-DEBT-BASE-MISMATCH", `${label}.frozen_legacy_spans`, "frozen-span index is not an exact structured source-span key"));
    }
  }
}

/** Compare owner lattice and independent/frozen sets; counts never authorize a transition. */
export function compareMigrationDebt(
  base: MigrationDebt,
  candidate: MigrationDebt,
  options: DebtComparisonOptions,
): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  const baseMeta = options.base_document.document;
  const candidateMeta = options.candidate_document.document;
  if (
    baseMeta.roadmap !== candidateMeta.roadmap ||
    baseMeta.source_path !== candidateMeta.source_path ||
    baseMeta.projection_path !== candidateMeta.projection_path
  ) {
    issues.push(issue(options, "E-DEBT-BASE-MISMATCH", "document", "base and candidate roadmap/source/projection identity differ"));
    return Object.freeze(issues);
  }
  if (baseMeta.schema_version === 1 && candidateMeta.schema_version === 0) {
    issues.push(issue(options, "E-DEBT-BASE-MISMATCH", "document.schema_version", "v1 to v0 migration is forbidden"));
  }
  validateMapIndexes(base, "base", options, issues);
  validateMapIndexes(candidate, "candidate", options, issues);
  const transitions = transitionFacts(base, candidate, options);
  if (options.transition_facts !== undefined && transitions === undefined) {
    issues.push(issue(options, "E-DEBT-BASE-MISMATCH", "transition_facts", "debt transition capability is invalid, caller-constructed, or belongs to another comparison"));
  }

  for (const [index, candidateOwner] of candidate.owners) {
    const baseOwner = base.owners.get(index);
    if (baseOwner !== undefined) {
      if (OWNER_RANK[candidateOwner.state] > OWNER_RANK[baseOwner.state]) {
        issues.push(issue(
          options,
          "E-DEBT-OWNER-REGRESSION",
          `owners.${index}`,
          `owner regressed from ${baseOwner.state} to ${candidateOwner.state}`,
        ));
      }
      continue;
    }
    if (
      !newSemanticRecord(candidateOwner.key, base, candidateOwner.state, options) &&
      !transitions?.added.has(index)
    ) {
      issues.push(issue(
        options,
        "E-DEBT-OWNER-REGRESSION",
        `owners.${index}`,
        `candidate-only owner ${index} lacks a new-record or restructure witness`,
      ));
    }
  }
  for (const [index, baseOwner] of base.owners) {
    if (!candidate.owners.has(index) && !transitions?.removed.has(index)) {
      issues.push(issue(
        options,
        "E-DEBT-OWNER-REGRESSION",
        `owners.${index}`,
        `base owner ${index} disappeared without a validated C5 retirement or restructure transition fact`,
      ));
    }
  }

  for (const [index, value] of candidate.independent) {
    if (base.independent.has(index)) continue;
    const hidden = [...base.independent.values()].find((baseValue) =>
      debtOwnerIndex(baseValue.owner) === debtOwnerIndex(value.owner) &&
      baseValue.subject === value.subject &&
      baseValue.category !== value.category
    );
    issues.push(issue(
      options,
      hidden === undefined ? "E-DEBT-SET-GROWTH" : "E-DEBT-CATEGORY-HIDE",
      `independent.${index}`,
      hidden === undefined
        ? "candidate independent debt tuple was not present at base"
        : `independent debt moved from ${hidden.category} to ${value.category}`,
    ));
  }

  const candidateFrozen = [...candidate.frozen_legacy_spans.keys()];
  const baseFrozen = new Set(base.frozen_legacy_spans.keys());
  if (candidateFrozen.some((index) => !baseFrozen.has(index))) {
    issues.push(issue(options, "E-DEBT-FROZEN-SET", "frozen_legacy_spans", "candidate frozen span set is not a subset of base"));
  }
  if (baseMeta.schema_version === 0 && candidateMeta.schema_version === 1) {
    const exact = candidateFrozen.length === baseFrozen.size && candidateFrozen.every((index) => baseFrozen.has(index));
    if (!exact) {
      issues.push(issue(options, "E-DEBT-FROZEN-SET", "frozen_legacy_spans", "v0 to v1 cutover must preserve the exact raw span set"));
    }
    for (const [index] of base.owners) {
      if (!candidate.owners.has(index)) {
        issues.push(issue(options, "E-DEBT-OWNER-REGRESSION", `owners.${index}`, "v0 to v1 cutover cannot remove an owner"));
      }
    }
  }
  return Object.freeze(issues);
}

export function migrationDebtReport(debt: MigrationDebt): DebtReport {
  const ownerCounts: Record<OwnerDebtState, number> = {
    raw_unclassified: 0,
    raw_with_semantic_shadow: 0,
    semantic: 0,
  };
  const independentCounts = Object.fromEntries(
    INDEPENDENT_CATEGORIES.map((category) => [category, 0]),
  ) as Record<IndependentDebtCategory, number>;
  const owners = [...debt.owners.values()].sort((left, right) =>
    codePointSort(debtOwnerIndex(left.key), debtOwnerIndex(right.key))
  );
  const independent = [...debt.independent.values()].sort((left, right) =>
    codePointSort(independentDebtIndex(left), independentDebtIndex(right))
  );
  for (const owner of owners) ownerCounts[owner.state]++;
  for (const value of independent) independentCounts[value.category]++;
  return Object.freeze({
    owner_counts: Object.freeze(ownerCounts),
    independent_counts: Object.freeze(independentCounts),
    owners: Object.freeze(owners),
    independent: Object.freeze(independent),
  });
}
