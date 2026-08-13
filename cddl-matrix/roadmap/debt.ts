import type { RoadmapIssue } from "./errors.ts";
import type {
  FragmentId,
  MarkerId,
  PartId,
  RoadmapId,
  RoadmapName,
  SectionId,
  SpanId,
} from "./model/core.ts";
import type { RoadmapDocument, SemanticPayload } from "./model/documents.ts";
import {
  exactProjectedFieldSegment,
  type CompletedRenderIr,
  type FieldConsumptionLedgerEntry,
  type RenderChunk,
} from "./render_ir.ts";
import { codePointSort } from "./kernel.ts";

/** Every owner of the sole supported schema is semantic; the state remains a report axis. */
export type OwnerDebtState = "semantic";

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
  | "inferred_transitions"
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
}

export interface DebtComparisonOptions {
  readonly base_document: RoadmapDocument;
  readonly candidate_document: RoadmapDocument;
  readonly base_completed?: CompletedRenderIr;
  readonly candidate_completed?: CompletedRenderIr;
}

export interface DebtReport {
  readonly owner_counts: Readonly<Record<OwnerDebtState, number>>;
  readonly independent_counts: Readonly<Record<IndependentDebtCategory, number>>;
  readonly owners: readonly { key: DebtOwnerKey; state: OwnerDebtState }[];
  readonly independent: readonly IndependentDebtKey[];
}

export interface MigrationProgressBlocker {
  readonly category: "uncovered_replacement_span" | IndependentDebtCategory;
  readonly subject: string;
}

export interface MigrationProgressReport {
  readonly replacement_coverage: {
    readonly denominator: number;
    readonly numerator: number;
    readonly covered_span_ids: readonly SpanId[];
  };
  readonly independent_debt: {
    readonly count: number;
    readonly items: readonly IndependentDebtKey[];
  };
  readonly typed_semantic_state: {
    readonly signals: {
      readonly unknown_record_ids: readonly RoadmapId[];
      readonly stale_record_ids: readonly RoadmapId[];
    };
    readonly evidence: {
      readonly unknown_record_ids: readonly RoadmapId[];
      readonly stale_record_ids: readonly RoadmapId[];
    };
    readonly controls: {
      readonly stale_record_ids: readonly RoadmapId[];
    };
    readonly unrepresentable_coordinates: readonly ["controls.unknown"];
  };
  readonly completion_audit: {
    readonly lane_blockers: readonly MigrationProgressBlocker[];
    /** No independent category defers to a cross-roadmap join; the axis stays a reported zero. */
    readonly join_blockers: readonly MigrationProgressBlocker[];
  };
}

const INDEPENDENT_CATEGORIES: readonly IndependentDebtCategory[] = [
  "inferred_transitions",
  "pending_family_classifications",
  "unrendered_fields",
  "unmodelled_coordinates",
];

export const LANE_BLOCKING_INDEPENDENT_CATEGORIES = Object.freeze([
  "inferred_transitions",
  "pending_family_classifications",
  "unrendered_fields",
] as const satisfies readonly IndependentDebtCategory[]);

export const VISIBLE_NON_BLOCKING_INDEPENDENT_CATEGORIES = Object.freeze([
  "unmodelled_coordinates",
] as const satisfies readonly IndependentDebtCategory[]);

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

function addSemanticPayloadDebt(
  document: RoadmapDocument,
  payload: SemanticPayload,
  owner: DebtOwnerKey,
  independent: Map<string, IndependentDebtKey>,
): void {
  if (payload.kind === "work" && payload.work_state === "pending_review") {
    addIndependent(independent, {
      roadmap: document.document.roadmap,
      category: "inferred_transitions",
      owner,
      subject: "payload.work_state",
    });
  }
  if (payload.kind === "work" && payload.family_classification === "pending") {
    addIndependent(independent, {
      roadmap: document.document.roadmap,
      category: "pending_family_classifications",
      owner,
      subject: "payload.family_classification",
    });
  }
  if (
    payload.kind === "family" && payload.family_maturity === "under_design" &&
    payload.denominator_unknowns_md !== undefined
  ) {
    addIndependent(independent, {
      roadmap: document.document.roadmap,
      category: "unmodelled_coordinates",
      owner,
      subject: "payload.denominator_unknowns_md",
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
  const chunks = chunkByOwner(completed.chunks);

  for (const section of document.sections) {
    addOwner(owners, {
      roadmap, owner_kind: "section", owner_id: section.section_id, owner_field: "body_md",
    }, "semantic");
  }
  for (const fragment of document.fragments) {
    addOwner(owners, {
      roadmap, owner_kind: "fragment", owner_id: fragment.fragment_id, owner_field: "body_md",
    }, "semantic");
  }
  for (const marker of document.legacy_markers) {
    addOwner(owners, {
      roadmap, owner_kind: "legacy_marker", owner_id: marker.marker_id, owner_field: "marker_md",
    }, "semantic");
  }
  for (const part of document.parts) {
    addOwner(owners, {
      roadmap, owner_kind: "part", owner_id: part.part_id, owner_field: "body_md",
    }, "semantic");
  }

  for (const record of document.records) {
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
    addSemanticPayloadDebt(
      document,
      record.payload,
      ownerForRecord(document, record.id, owners),
      independent,
    );
  }

  for (const span of document.spans) {
    addOwner(owners, {
      roadmap, owner_kind: "source_span", owner_id: span.id, owner_field: "coverage",
    }, "semantic");
  }

  for (const ledger of completed.field_consumption) {
    const owner = ledger.owner_kind === "record"
      ? ownerForRecord(document, ledger.owner_id as RoadmapId, owners)
      : [...owners.values()].find(({ key }) =>
        key.owner_kind === ledger.owner_kind && key.owner_id === ledger.owner_id
      )?.key;
    if (owner !== undefined) unrenderedFromLedger(document, ledger, owner, independent);
  }

  for (const value of additionalIndependent) addIndependent(independent, value);

  return Object.freeze({ owners, independent });
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
      if (!key.owner_field.startsWith("payload.")) return false;
      const field = valueAtPath(value, key.owner_field.split("."));
      return field instanceof Uint8Array && key.owner_field.endsWith("_md");
    }
  }
}

/**
 * Record-owner transition rules for one authoritative comparison: a candidate-only record must be
 * semantic-only and own nothing projected, and an existing record cannot change its visibility.
 */
export function validateRecordOwnerTransition(
  options: Pick<DebtComparisonOptions, "base_document" | "candidate_document">,
): readonly RoadmapIssue[] {
  const comparisonOptions: DebtComparisonOptions = options;
  const issues: RoadmapIssue[] = [];
  const baseRecords = new Map(options.base_document.records.map((record) => [record.id, record]));
  for (const candidateRecord of options.candidate_document.records) {
    const baseRecord = baseRecords.get(candidateRecord.id);
    if (baseRecord === undefined) {
      const validSemanticOnly = candidateRecord.projection_visibility === "semantic_only" &&
        candidateRecord.source_replacements.length === 0 &&
        !options.candidate_document.spans.some((span) =>
          span.source_kind === "record" && span.owner_id === candidateRecord.id
        );
      if (!validSemanticOnly) {
        issues.push(issue(
          comparisonOptions,
          "E-DEBT-OWNER-REGRESSION",
          `record[${JSON.stringify(candidateRecord.id)}]`,
          "candidate-only roadmap record must be semantic-only and own no source replacements or spans",
        ));
      }
      continue;
    }
    if (baseRecord.projection_visibility !== candidateRecord.projection_visibility) {
      issues.push(issue(
        comparisonOptions,
        "E-DEBT-OWNER-REGRESSION",
        `record[${JSON.stringify(candidateRecord.id)}].projection_visibility`,
        "an existing semantic record cannot change projection visibility",
      ));
    }
  }
  return Object.freeze(issues);
}

function newSemanticRecord(
  key: DebtOwnerKey,
  base: MigrationDebt,
  candidateState: OwnerDebtState,
  options: DebtComparisonOptions,
): boolean {
  if (key.owner_kind !== "record" || candidateState !== "semantic") return false;
  const candidateRecord = options.candidate_document.records.find((record) => record.id === key.owner_id);
  return candidateRecord !== undefined &&
    candidateRecord.projection_visibility === "semantic_only" &&
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
}

/** Compare owner and independent sets; counts never authorize a transition. */
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
  validateMapIndexes(base, "base", options, issues);
  validateMapIndexes(candidate, "candidate", options, issues);

  for (const [index, candidateOwner] of candidate.owners) {
    if (base.owners.has(index)) continue;
    if (!newSemanticRecord(candidateOwner.key, base, candidateOwner.state, options)) {
      issues.push(issue(
        options,
        "E-DEBT-OWNER-REGRESSION",
        `owners.${index}`,
        `candidate-only owner ${index} lacks a new-record witness`,
      ));
    }
  }
  for (const [index] of base.owners) {
    if (!candidate.owners.has(index)) {
      issues.push(issue(
        options,
        "E-DEBT-OWNER-REGRESSION",
        `owners.${index}`,
        `base owner ${index} disappeared without a validated retirement`,
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

  return Object.freeze(issues);
}

function sourceReplacementsForSpan(
  document: RoadmapDocument,
  span: RoadmapDocument["spans"][number],
): readonly { readonly span_id: SpanId; readonly replacement_field: string }[] {
  const value = span.source_kind === "record"
    ? document.records.find((candidate) => candidate.id === span.owner_id)
    : span.source_kind === "section"
      ? document.sections.find((candidate) => candidate.section_id === span.owner_id)
      : span.source_kind === "fragment"
        ? document.fragments.find((candidate) => candidate.fragment_id === span.owner_id)
        : span.source_kind === "part"
          ? document.parts.find((candidate) => candidate.part_id === span.owner_id)
          : span.source_kind === "legacy_marker"
            ? document.legacy_markers.find((candidate) => candidate.marker_id === span.owner_id)
            : undefined;
  if (value === undefined || !("source_replacements" in value)) return [];
  return value.source_replacements.filter((replacement) =>
    replacement.span_id === span.id && replacement.replacement_field === span.owner_field
  );
}

function hasExactReplacementBinding(
  document: RoadmapDocument,
  completed: CompletedRenderIr,
  span: RoadmapDocument["spans"][number],
): boolean {
  if (span.migration_status !== "replaced") return false;
  const replacements = sourceReplacementsForSpan(document, span);
  if (replacements.length !== 1) return false;
  const chunks = completed.chunks.filter((chunk) =>
    chunk.owner.kind === span.source_kind && chunk.owner.id === span.owner_id &&
    chunk.source_span_ids.filter((spanId) => spanId === span.id).length === 1
  );
  const chunk = chunks[0];
  if (chunks.length !== 1 || chunk === undefined) return false;
  if (span.source_kind === "generated_slot") return false;
  return exactProjectedFieldSegment(
    completed,
    chunk,
    span.source_kind,
    span.owner_id,
    span.owner_field,
    span.start_byte,
    span.end_byte,
  ) !== undefined;
}

function progressBlockerSort(
  left: MigrationProgressBlocker,
  right: MigrationProgressBlocker,
): number {
  return codePointSort(JSON.stringify([left.category, left.subject]), JSON.stringify([right.category, right.subject]));
}

/** Pure, canonical progress view. Typed stale/unknown postures are visible state, never debt. */
export function migrationProgressReport(
  document: RoadmapDocument,
  debt: MigrationDebt,
  completed: CompletedRenderIr,
): MigrationProgressReport {
  const replacementDenominator = document.spans.filter((span) => span.migration_status === "replaced");
  const coveredSpanIds = replacementDenominator.filter((span) =>
    hasExactReplacementBinding(document, completed, span)
  ).map((span) => span.id).sort(codePointSort);
  const covered = new Set(coveredSpanIds);
  const independent = [...debt.independent.values()].sort((left, right) =>
    codePointSort(independentDebtIndex(left), independentDebtIndex(right))
  );
  const signalUnknown = new Set<RoadmapId>();
  const signalStale = new Set<RoadmapId>();
  const evidenceUnknown = new Set<RoadmapId>();
  const evidenceStale = new Set<RoadmapId>();
  const controlStale = new Set<RoadmapId>();
  for (const record of document.records) {
    const payload = record.payload;
    if (payload.kind === "signal") {
      if (payload.evaluation === "unknown") signalUnknown.add(record.id);
      if (payload.evaluation === "stale") signalStale.add(record.id);
    } else if (payload.kind === "evidence") {
      if (payload.evidence_verdict === "unknown") evidenceUnknown.add(record.id);
      if (payload.freshness === "stale") evidenceStale.add(record.id);
    } else if (payload.kind === "control" && payload.control_state === "stale") {
      controlStale.add(record.id);
    }
  }
  const sortedIds = (values: ReadonlySet<RoadmapId>): readonly RoadmapId[] =>
    Object.freeze([...values].sort(codePointSort));

  const laneBlockers: MigrationProgressBlocker[] = [
    ...replacementDenominator.filter((span) => !covered.has(span.id)).map((span) => ({
      category: "uncovered_replacement_span" as const,
      subject: span.id,
    })),
    ...independent.filter((item) =>
      LANE_BLOCKING_INDEPENDENT_CATEGORIES.some((category) => category === item.category)
    ).map((item) => ({
      category: item.category,
      subject: independentDebtIndex(item),
    })),
  ].sort(progressBlockerSort);

  return Object.freeze({
    replacement_coverage: Object.freeze({
      denominator: replacementDenominator.length,
      numerator: coveredSpanIds.length,
      covered_span_ids: Object.freeze(coveredSpanIds),
    }),
    independent_debt: Object.freeze({ count: independent.length, items: Object.freeze(independent) }),
    typed_semantic_state: Object.freeze({
      signals: Object.freeze({ unknown_record_ids: sortedIds(signalUnknown), stale_record_ids: sortedIds(signalStale) }),
      evidence: Object.freeze({ unknown_record_ids: sortedIds(evidenceUnknown), stale_record_ids: sortedIds(evidenceStale) }),
      controls: Object.freeze({ stale_record_ids: sortedIds(controlStale) }),
      unrepresentable_coordinates: Object.freeze(["controls.unknown"] as const),
    }),
    completion_audit: Object.freeze({
      lane_blockers: Object.freeze(laneBlockers),
      join_blockers: Object.freeze([]),
    }),
  });
}

export function migrationDebtReport(debt: MigrationDebt): DebtReport {
  const ownerCounts: Record<OwnerDebtState, number> = { semantic: 0 };
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

/**
 * Migration completion is intrinsic to the wire schema: there is no authored declaration and no
 * "converting" arm. The audit reports the fixed state alongside the live progress blockers.
 */
export interface MigrationCompletionAudit {
  readonly declared: "intrinsic";
  readonly effective: "complete";
  readonly blockers: readonly MigrationProgressBlocker[];
  readonly join_blockers: readonly MigrationProgressBlocker[];
}

export const MIGRATION_COMPLETION_STATE = Object.freeze({
  declared: "intrinsic",
  effective: "complete",
} as const);

export function migrationCompletionAudit(
  document: RoadmapDocument,
  debt: MigrationDebt,
  completed: CompletedRenderIr,
): MigrationCompletionAudit {
  const progress = migrationProgressReport(document, debt, completed);
  return Object.freeze({
    ...MIGRATION_COMPLETION_STATE,
    blockers: progress.completion_audit.lane_blockers,
    join_blockers: progress.completion_audit.join_blockers,
  });
}

/** Intrinsic completion is only credible while no lane blocker survives. */
export function validateMigrationCompletion(
  document: RoadmapDocument,
  debt: MigrationDebt,
  completed: CompletedRenderIr,
): readonly RoadmapIssue[] {
  const audit = migrationCompletionAudit(document, debt, completed);
  return Object.freeze(audit.blockers.map((blocker) => ({
    code: "E-SCHEMA-STATE" as const,
    source: document.document.source_path,
    logical_path: `document.migration_completion.blocker[${JSON.stringify(`${blocker.category}:${blocker.subject}`)}]`,
    message: `intrinsic migration completion forbids ${blocker.category} blocker ${blocker.subject}`,
    exit: 1 as const,
  })));
}
