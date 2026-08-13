import type { IssueCollector, RoadmapIssue } from "../errors.ts";
import { validateRoadmapId } from "../ids.ts";
import type { RoadmapIndexes, SemanticPayloadProviderFact } from "../indexes.ts";
import type { RepoPath, RoadmapId, RoadmapName, SlotId } from "../model/core.ts";
import type {
  RoadmapDocument,
  RoadmapDocumentV0,
  SemanticPayload,
  SemanticRecord,
} from "../model/documents.ts";
import type { GeneratedSlotResolver, Indexes, RegistryView, RoadmapAdapter } from "./types.ts";
import {
  MATRIX_ADAPTER,
  renderCanonicalSemanticRecord,
  validateDecodedRoadmapDocument,
  type DecodedRoadmapValidationOptions,
  type DecodedRoadmapValidationResult,
} from "./matrix.ts";

const TESTING_SOURCE_PATH = "tests/testing-roadmap.toml" as RepoPath;
const TESTING_PROJECTION_PATH = "tests/TESTING_ROADMAP.md" as RepoPath;
const TESTING_PICKUP = Object.freeze({
  sha256: "6e90f1fb06011cefa546d861da0a6525ff1af6fc81bbe51c9ed5f035578b53af",
  byte_length: 306_388,
  line_count: 3_412,
  eof: "lf" as const,
});

const TESTING_V0_COUNTS = Object.freeze({
  sections: 9,
  fragments: 2,
  legacy_markers: 0,
  records: 130,
  parts: 57,
  generated_slots: 0,
  manifest: 198,
  spans: 198,
});

const TESTING_V0_GROUP_COUNTS: Readonly<Record<string, number>> = Object.freeze({
  "pending-maintainer": 2,
  "next-priority": 25,
  "standing-system": 61,
  "deferred-features": 18,
  "operational-watches": 13,
  "declined-boundaries": 11,
});

const TESTING_V0_SECTIONS = Object.freeze([
  ["declined-boundaries", "Declined (decided, with a reopening signal unless explicitly permanent)"],
  ["deferred-features", "Deferred features (build when a real consumer needs them)"],
  ["next-priority", "Next work items, in priority order"],
  ["north-star", "North star — automated feature coverage"],
  ["operational-watches", "Operational watches"],
  ["pending-maintainer", "Pending maintainer action"],
  ["preamble", "Testing roadmap preamble"],
  ["sources", "Sources"],
  ["standing-system", "Standing-system residuals"],
] as const);

const TESTING_V0_FRAGMENTS = Object.freeze([
  [
    "sources-exhaustive-menu",
    "Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`",
  ],
  ["sources-expert-writeups", "Per-dimension expert write-ups: `draft/testing-recommendations/*.md`"],
] as const);

const RULE_TRAILING = Object.freeze({
  id: "testing.rule-trailing.directive-classification" as RoadmapId,
  title: "Adopt the parser's `RuleTrailing` anchor and classify that rule-only slot in one delivery — blocked on publishing the reviewed fork revision.",
  projection_group: "pending-maintainer",
  aliases: Object.freeze(["B3-002", "B3-005", "T1-09"]),
  span_id: "span-record-rule-trailing-directive-classification",
  start_byte: 6_013,
  end_byte: 8_877,
  sha256: "c5a5b506dba80f59781f9024767bd7b6bd14d191981f1923553d12ad65b8d338",
});

const TESTING_V0_STRUCTURE_SHA256 =
  "162d134c14916246d85f47c3b21998c4d01c81f8bd5af4570ffa62a552fc44d1";

const TESTING_V0_PHYSICAL_INVENTORY: Readonly<Record<string, number>> = Object.freeze({
  h1: 1,
  h2: 7,
  h3: 1,
  column_zero_list_nodes: 131,
  nested_list_nodes: 57,
  nested_bold_nodes: 28,
  table_lines: 4,
  table_blocks: 1,
  html_comment_starts: 1,
  fenced_code_blocks: 0,
  inline_code_delimiters: 4_004,
  inline_code_lines: 1_404,
});

function floorIssue(
  doc: RoadmapDocument,
  out: IssueCollector,
  logicalPath: string,
  message: string,
): void {
  out.add({
    code: "E-SCHEMA-FLOOR",
    source: doc.document.source_path,
    logical_path: logicalPath,
    message,
    exit: 1,
  });
}

function isLiveTestingV0(doc: RoadmapDocument): doc is RoadmapDocumentV0 {
  return doc.document.schema_version === 0 &&
    doc.document.source_path === TESTING_SOURCE_PATH &&
    doc.document.projection_path === TESTING_PROJECTION_PATH;
}

function manifestIdentity(entry: RoadmapDocumentV0["manifest"][number]): readonly string[] {
  switch (entry.kind) {
    case "section": return [entry.kind, entry.section_id];
    case "fragment": return [entry.kind, entry.fragment_id];
    case "legacy_marker": return [entry.kind, entry.marker_id];
    case "record": return [entry.kind, entry.record_id];
    case "part": return [entry.kind, entry.part_id];
    case "generated_slot": return [entry.kind, entry.slot_id];
  }
}

function testingV0StructureSha256(doc: RoadmapDocumentV0): string {
  const owners: unknown[][] = [
    ...doc.sections.map((value) => [
      "section", value.section_id, value.title, value.legacy_aliases ?? [], value.span_ids,
    ]),
    ...doc.fragments.map((value) => [
      "fragment", value.fragment_id, value.projection_group, value.title ?? null,
      value.legacy_aliases ?? [], value.span_ids,
    ]),
    ...doc.legacy_markers.map((value) => [
      "legacy_marker", value.marker_id, value.legacy_aliases, value.span_ids,
    ]),
    ...doc.records.map((value) => [
      "record", value.id, value.title, value.projection_group, value.legacy_aliases ?? [], value.span_ids,
    ]),
    ...doc.parts.map((value) => [
      "part", value.part_id, value.parent_record_id, value.title ?? null, value.span_ids,
    ]),
    ...doc.generated_slots.map((value) => [
      "generated_slot", value.slot_id, value.binding, value.span_ids,
    ]),
  ];
  owners.sort((left, right) => String(left[1]) < String(right[1]) ? -1 : String(left[1]) > String(right[1]) ? 1 : 0);
  const spans = [...doc.spans].sort((left, right) =>
    left.start_byte - right.start_byte || (left.id < right.id ? -1 : left.id > right.id ? 1 : 0)
  ).map((value) => [
    value.id, value.start_byte, value.end_byte, value.sha256, value.source_kind,
    value.owner_id, value.owner_field, value.migration_status,
  ]);
  const encoded = new TextEncoder().encode(JSON.stringify({
    owners,
    manifest: doc.manifest.map(manifestIdentity),
    spans,
  }));
  return new Bun.CryptoHasher("sha256").update(encoded).digest("hex");
}

function rawBlocks(doc: RoadmapDocumentV0): readonly Uint8Array[] {
  return [
    ...doc.sections.map((value) => value.source_block_md),
    ...doc.fragments.map((value) => value.source_block_md),
    ...doc.legacy_markers.map((value) => value.source_block_md),
    ...doc.records.map((value) => value.source_block_md),
    ...doc.parts.map((value) => value.source_block_md),
  ];
}

function physicalInventory(doc: RoadmapDocumentV0): Readonly<Record<string, number>> | undefined {
  const totals: Record<string, number> = {
    h1: 0,
    h2: 0,
    h3: 0,
    column_zero_list_nodes: 0,
    nested_list_nodes: 0,
    nested_bold_nodes: 0,
    table_lines: 0,
    table_blocks: 0,
    html_comment_starts: 0,
    fenced_code_blocks: 0,
    inline_code_delimiters: 0,
    inline_code_lines: 0,
  };
  const decoder = new TextDecoder("utf-8", { fatal: true });
  try {
    for (const block of rawBlocks(doc)) {
      const text = decoder.decode(block);
      const lines = text.split("\n");
      totals.h1 += lines.filter((line) => /^# /.test(line)).length;
      totals.h2 += lines.filter((line) => /^## /.test(line)).length;
      totals.h3 += lines.filter((line) => /^### /.test(line)).length;
      totals.column_zero_list_nodes += lines.filter((line) => /^(?:[-+*] |\d+\. )/.test(line)).length;
      totals.nested_list_nodes += lines.filter((line) => /^ +(?:[-+*] |\d+\. )/.test(line)).length;
      totals.nested_bold_nodes += lines.filter((line) => /^ +[-+*] \*\*/.test(line)).length;
      totals.table_lines += lines.filter((line) => /^\s*\|.*\|\s*$/.test(line)).length;
      let inTable = false;
      for (const line of lines) {
        const table = /^\s*\|.*\|\s*$/.test(line);
        if (table && !inTable) totals.table_blocks++;
        inTable = table;
      }
      totals.html_comment_starts += (text.match(/<!--/g) ?? []).length;
      totals.fenced_code_blocks += lines.filter((line) => /^\s*(?:`{3,}|~{3,})/.test(line)).length / 2;
      totals.inline_code_delimiters += (text.match(/`/g) ?? []).length;
      totals.inline_code_lines += lines.filter((line) => line.includes("`")).length;
    }
  } catch {
    return undefined;
  }
  return Object.freeze(totals);
}

function validateLiveTestingV0Floors(doc: RoadmapDocument, out: IssueCollector): void {
  if (!isLiveTestingV0(doc)) return;
  const metadata = [
    ["document.frozen_source_sha256", doc.document.frozen_source_sha256, TESTING_PICKUP.sha256],
    ["document.frozen_source_byte_length", doc.document.frozen_source_byte_length, TESTING_PICKUP.byte_length],
    ["document.frozen_source_line_count", doc.document.frozen_source_line_count, TESTING_PICKUP.line_count],
    ["document.frozen_source_eof", doc.document.frozen_source_eof, TESTING_PICKUP.eof],
  ] as const;
  for (const [logicalPath, actual, expected] of metadata) {
    if (actual !== expected) floorIssue(doc, out, logicalPath, `testing v0 pickup floor must be ${JSON.stringify(expected)}`);
  }

  const counts = [
    ["section", doc.sections.length, TESTING_V0_COUNTS.sections],
    ["fragment", doc.fragments.length, TESTING_V0_COUNTS.fragments],
    ["legacy_marker", doc.legacy_markers.length, TESTING_V0_COUNTS.legacy_markers],
    ["record", doc.records.length, TESTING_V0_COUNTS.records],
    ["part", doc.parts.length, TESTING_V0_COUNTS.parts],
    ["generated_slot", doc.generated_slots.length, TESTING_V0_COUNTS.generated_slots],
    ["manifest", doc.manifest.length, TESTING_V0_COUNTS.manifest],
    ["source_span", doc.spans.length, TESTING_V0_COUNTS.spans],
  ] as const;
  for (const [logicalPath, actual, expected] of counts) {
    if (actual !== expected) floorIssue(doc, out, logicalPath, `testing v0 requires exactly ${expected} ${logicalPath} entries`);
  }

  const sections = new Map<string, RoadmapDocumentV0["sections"][number]>(
    doc.sections.map((section) => [section.section_id, section]),
  );
  for (const [id, title] of TESTING_V0_SECTIONS) {
    const section = sections.get(id);
    const aliases = id === "standing-system" ? ["Standing-system residuals"] : [];
    if (section?.title !== title || JSON.stringify(section.legacy_aliases ?? []) !== JSON.stringify(aliases)) {
      floorIssue(doc, out, `section[${JSON.stringify(id)}]`, `testing v0 requires exact section title and aliases for ${id}`);
    }
  }
  const fragments = new Map<string, RoadmapDocumentV0["fragments"][number]>(
    doc.fragments.map((fragment) => [fragment.fragment_id, fragment]),
  );
  for (const [id, title] of TESTING_V0_FRAGMENTS) {
    const fragment = fragments.get(id);
    if (fragment?.projection_group !== "sources" || fragment.title !== title) {
      floorIssue(doc, out, `fragment[${JSON.stringify(id)}]`, `testing v0 requires exact Sources fragment ${id}`);
    }
  }

  const groupCounts = new Map<string, number>();
  for (const record of doc.records) {
    groupCounts.set(record.projection_group, (groupCounts.get(record.projection_group) ?? 0) + 1);
    const id = validateRoadmapId(record.id, "testing");
    if (!id.ok) floorIssue(doc, out, `record[${JSON.stringify(record.id)}].id`, `testing v0 ID fails permanent policy: ${id.message}`);
  }
  for (const [group, expected] of Object.entries(TESTING_V0_GROUP_COUNTS)) {
    if (groupCounts.get(group) !== expected) {
      floorIssue(doc, out, `record.projection_group[${JSON.stringify(group)}]`, `testing v0 requires exactly ${expected} records in ${group}`);
    }
  }
  if (groupCounts.size !== Object.keys(TESTING_V0_GROUP_COUNTS).length) {
    floorIssue(doc, out, "record.projection_group", "testing v0 contains an unexpected record projection group");
  }

  const numberedAliases = doc.records.flatMap((record) =>
    (record.legacy_aliases ?? []).filter((alias) => /^Next work \d+$/.test(alias))
  ).sort((left, right) => Number(left.slice(10)) - Number(right.slice(10)));
  const expectedNumberedAliases = [
    ...Array.from({ length: 8 }, (_, index) => `Next work ${index + 1}`),
    ...Array.from({ length: 17 }, (_, index) => `Next work ${index + 10}`),
  ];
  if (JSON.stringify(numberedAliases) !== JSON.stringify(expectedNumberedAliases)) {
    floorIssue(doc, out, "record.legacy_aliases.next-work", "testing v0 requires exact Next work aliases 1–8 and 10–26, with no invented 9");
  }

  const selected = doc.records.find((record) => record.id === RULE_TRAILING.id);
  if (
    selected?.title !== RULE_TRAILING.title || selected.projection_group !== RULE_TRAILING.projection_group ||
    JSON.stringify(selected.legacy_aliases ?? []) !== JSON.stringify(RULE_TRAILING.aliases) ||
    JSON.stringify(selected.span_ids) !== JSON.stringify([RULE_TRAILING.span_id])
  ) {
    floorIssue(doc, out, `record[${JSON.stringify(RULE_TRAILING.id)}]`, "testing v0 requires the exact reviewed RuleTrailing ID/title/group/aliases/span binding");
  }
  const selectedSpan = doc.spans.find((span) => span.id === RULE_TRAILING.span_id);
  if (
    selectedSpan?.source_kind !== "record" || selectedSpan.owner_id !== RULE_TRAILING.id ||
    selectedSpan.owner_field !== "source_block_md" || selectedSpan.migration_status !== "raw" ||
    selectedSpan.start_byte !== RULE_TRAILING.start_byte || selectedSpan.end_byte !== RULE_TRAILING.end_byte ||
    selectedSpan.sha256 !== RULE_TRAILING.sha256
  ) {
    floorIssue(doc, out, `source_span[${JSON.stringify(RULE_TRAILING.span_id)}]`, "testing v0 requires the exact reviewed RuleTrailing interval and digest");
  }

  const inventory = physicalInventory(doc);
  for (const [name, expected] of Object.entries(TESTING_V0_PHYSICAL_INVENTORY)) {
    if (inventory?.[name] !== expected) {
      floorIssue(doc, out, `testing_v0.physical_inventory.${name}`, `testing v0 physical inventory requires ${name}=${expected}`);
    }
  }
  if (testingV0StructureSha256(doc) !== TESTING_V0_STRUCTURE_SHA256) {
    floorIssue(doc, out, "testing_v0.structure", "testing v0 owner classification, identities, manifest, or span ledger differs from the reviewed pickup");
  }
}

function issue(
  provider: SemanticPayloadProviderFact,
  source: string,
  logicalPath: string,
  message: string,
): RoadmapIssue {
  return {
    code: "E-SCHEMA-STATE",
    source,
    logical_path: `${provider.logical_path}.${logicalPath}`,
    message,
    exit: 1,
  };
}

function payloadAt(indexes: Indexes, id: RoadmapId): SemanticPayload | undefined {
  if ("payload_records" in indexes) {
    return (indexes as Indexes & Pick<RoadmapIndexes, "payload_records">).payload_records.get(id)?.payload;
  }
  return indexes.records.get(id)?.payload;
}

function requirePayloadKind(
  provider: SemanticPayloadProviderFact,
  source: string,
  indexes: Indexes,
  id: RoadmapId,
  field: string,
  predicate: (payload: SemanticPayload) => boolean,
  expected: string,
  out: IssueCollector,
): void {
  const target = payloadAt(indexes, id);
  const deferred = (indexes as Indexes & { readonly deferred_foreign_roadmap_joins?: RoadmapName })
    .deferred_foreign_roadmap_joins;
  const targetNamespace = id.startsWith("matrix.") ? "matrix" : id.startsWith("testing.") ? "testing" : undefined;
  if (target === undefined && deferred !== undefined && targetNamespace !== undefined &&
    targetNamespace !== deferred) return;
  if (target === undefined || !predicate(target)) {
    out.add(issue(provider, source, field, `${id} must resolve to ${expected}`));
  }
}

export function validateTestingPayloadFact(
  provider: SemanticPayloadProviderFact,
  indexes: Indexes,
  out: IssueCollector,
  source: string = TESTING_SOURCE_PATH,
): void {
  const payload = provider.payload;
  if (
    payload.kind !== "testing_operational_watch" && payload.kind !== "testing_incident" &&
    payload.kind !== "testing_cost" && payload.kind !== "testing_system_admission"
  ) return;
  if (payload.kind === "testing_operational_watch") {
    requirePayloadKind(
      provider,
      source,
      indexes,
      payload.escalation_transition_id,
      "escalation_transition_id",
      (target) => target.kind === "signal" && target.transition_kind === "watch_escalation",
      "a watch-escalation signal",
      out,
    );
    return;
  }
  if (payload.kind === "testing_incident") {
    for (const id of payload.evidence_ids) {
      requirePayloadKind(provider, source, indexes, id, "evidence_ids", (target) => target.kind === "evidence", "an evidence record", out);
    }
    return;
  }
  if (payload.kind === "testing_cost") {
    if (payload.cost_posture === "historical_observation") {
      for (const id of payload.evidence_ids) {
        requirePayloadKind(provider, source, indexes, id, "evidence_ids", (target) => target.kind === "evidence", "an evidence record", out);
      }
    }
    return;
  }
  for (const id of payload.evidence_ids) {
    requirePayloadKind(provider, source, indexes, id, "evidence_ids", (target) => target.kind === "evidence", "an evidence record", out);
  }
  if (payload.admission_kind === "independent_recurrence") {
    for (const id of payload.incident_ids) {
      requirePayloadKind(
        provider,
        source,
        indexes,
        id,
        "incident_ids",
        (target) => target.kind === "testing_incident",
        "a testing incident record",
        out,
      );
    }
  } else if (payload.admission_kind === "bounded_denominator") {
    requirePayloadKind(provider, source, indexes, payload.family_id, "family_id", (target) => target.kind === "family", "a systematic family", out);
    requirePayloadKind(
      provider,
      source,
      indexes,
      payload.cost_record_id,
      "cost_record_id",
      (target) => target.kind === "testing_cost",
      "a testing cost record",
      out,
    );
  }
}

export const TESTING_ADAPTER: RoadmapAdapter<SemanticPayload> = Object.freeze({
  roadmap: "testing",
  namespace: "testing",
  source_path: TESTING_SOURCE_PATH,
  projection_path: TESTING_PROJECTION_PATH,
  validateExtension(record: SemanticRecord<SemanticPayload>, indexes: Indexes, out: IssueCollector) {
    validateTestingPayloadFact({
      record,
      payload: record.payload,
      authority: "semantic",
      logical_path: `record[${JSON.stringify(record.id)}].payload`,
    }, indexes, out);
  },
  renderSemantic: renderCanonicalSemanticRecord,
  // Testing has no domain-specific reference universe. Gate/test/file providers are shared
  // joins, and an empty adapter member list is an explicit contract rather than a missing registry.
  referenceProviders(_view: RegistryView) {
    return [];
  },
  // Testing has no generated roadmap slots. Synthetic transport fixtures inject their own
  // resolver at the render-service seam and do not expand production adapter authority.
  slotResolvers(_view: RegistryView, _document: RoadmapDocument): ReadonlyMap<SlotId, GeneratedSlotResolver> {
    return new Map<SlotId, GeneratedSlotResolver>();
  },
  validateFloors(doc: RoadmapDocument, out: IssueCollector) {
    validateLiveTestingV0Floors(doc, out);
    if (doc.document.roadmap !== "testing") {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "document.roadmap",
        message: "testing adapter requires a testing roadmap document",
        exit: 1,
      });
    }
    if (doc.document.source_path !== TESTING_SOURCE_PATH) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "document.source_path",
        message: `testing source path must be ${TESTING_SOURCE_PATH}`,
        exit: 1,
      });
    }
    if (doc.document.projection_path !== TESTING_PROJECTION_PATH) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "document.projection_path",
        message: `testing projection path must be ${TESTING_PROJECTION_PATH}`,
        exit: 1,
      });
    }
    if (doc.records.length === 0 || doc.manifest.length === 0 || doc.spans.length === 0) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "$",
        message: "testing roadmap requires records, manifest placements, and source spans",
        exit: 1,
      });
    }
    if (doc.generated_slots.length !== 0) {
      out.add({
        code: "E-SCHEMA-FLOOR",
        source: doc.document.source_path,
        logical_path: "generated_slot",
        message: "testing roadmap declares exactly zero generated slots",
        exit: 1,
      });
    }
  },
});

export const TESTING_GENERATED_SLOT_BINDINGS: readonly never[] = Object.freeze([]);

export function validateTestingRoadmapDocument(
  document: RoadmapDocument,
  view: RegistryView,
  options: DecodedRoadmapValidationOptions = {},
): DecodedRoadmapValidationResult {
  return validateDecodedRoadmapDocument(
    document,
    view,
    TESTING_ADAPTER,
    [
      ...MATRIX_ADAPTER.referenceProviders(view),
      ...TESTING_ADAPTER.referenceProviders(view),
    ],
    validateTestingPayloadFact,
    options,
  );
}
