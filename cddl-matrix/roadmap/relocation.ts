import type { RegistryView } from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import type { RetiredIdsDocumentV1, RoadmapDocument } from "./model/documents.ts";

/**
 * These labels are transaction-local structural guards, not RoadmapIds. They keep WP8's split of
 * the former testing north-star prose exact without manufacturing active or tombstoned lifecycle
 * owners for facts whose authority is now a durable current-state document.
 */
export interface StructuralRelocationGuard {
  readonly id: string;
  readonly path: string;
  readonly heading: string;
  readonly claim_text?: string;
}

export const NORTH_STAR_STRUCTURAL_RELOCATION_FLOOR = 17;

export const NORTH_STAR_STRUCTURAL_RELOCATION_GUARDS: readonly StructuralRelocationGuard[] =
  Object.freeze([
    { id: "testing.relocation.north-star.architecture-overview", path: "tests/README.md", heading: "Coverage" },
    { id: "testing.relocation.north-star.decode-conformance", path: "tests/README.md", heading: "Decode-direction conformance (`tests/decode_conformance/` — accept what the spec accepts)" },
    { id: "testing.relocation.north-star.json-wasm-surface-legs", path: "tests/README.md", heading: "json/wasm surface legs" },
    { id: "testing.relocation.north-star.open-struct-map-loose-test-map", path: "tests/README.md", heading: "Open struct-maps (rest rows) — test map (loose-CBOR Phase B)" },
    { id: "testing.relocation.north-star.open-struct-map-ignore-test-map", path: "tests/README.md", heading: "Open struct-maps — the `@ignore` (tolerate-and-drop) flavor — test map" },
    { id: "testing.relocation.north-star.open-array-test-map", path: "tests/README.md", heading: "Open arrays (rest tails) — test map" },
    { id: "testing.relocation.north-star.loose-any-user-contract", path: "docs/docs/current_capacities.mdx", heading: "The `any` type (loose CBOR)" },
    { id: "testing.relocation.north-star.any-cbor-runtime-contract", path: "docs/docs/output_format.mdx", heading: "The `any` type (`AnyCbor` runtime value)" },
    { id: "testing.relocation.north-star.open-struct-map-contract", path: "docs/docs/output_format.mdx", heading: "Open struct-maps (rest rows)" },
    { id: "testing.relocation.north-star.open-struct-map-ignore-contract", path: "docs/docs/output_format.mdx", heading: "Open struct-maps (rest rows)" },
    { id: "testing.relocation.north-star.open-array-contract", path: "docs/docs/output_format.mdx", heading: "Open arrays (rest tails)" },
    { id: "testing.relocation.north-star.open-array-ignore-contract", path: "docs/docs/output_format.mdx", heading: "Open arrays (rest tails)" },
    { id: "testing.relocation.north-star.hash-control-exclusion", path: "docs/docs/current_capacities.mdx", heading: "Unsupported constructs" },
    { id: "testing.relocation.north-star.cbor-any-exclusion", path: "docs/docs/current_capacities.mdx", heading: "Unsupported constructs" },
    { id: "testing.relocation.north-star.non-last-any-choice-exclusion", path: "docs/docs/current_capacities.mdx", heading: "Contextual gaps (supported top-level, unsupported when nested)" },
    { id: "testing.relocation.north-star.newtype-any-exclusion", path: "docs/docs/current_capacities.mdx", heading: "The `any` type (loose CBOR)", claim_text: "`@newtype` on a bare `any` alias is unsupported" },
    { id: "testing.relocation.north-star.control-operator-any-exclusion", path: "docs/docs/current_capacities.mdx", heading: "The `any` type (loose CBOR)", claim_text: "Control operators on bare `any` are also" },
  ] satisfies readonly StructuralRelocationGuard[]);

export const WP8_RETIRED_STRUCTURAL_PART_IDS = Object.freeze([
  "part-freezes-livelock-repo-arithmetic-make-them-impossible-containment",
  "part-negative-premise-stands",
] as const);

export const WP8_LAST_ACTIVE_AT = "6ec64028f86606b37c7be8dc743b76c97e0b4c4e";

export const WP8_RETIRED_STRUCTURAL_PART_RELOCATION_GUARDS: readonly StructuralRelocationGuard[] =
  Object.freeze([
    {
      id: "part-freezes-livelock-repo-arithmetic-make-them-impossible-containment",
      path: "tests/README.md",
      heading: "Gate-level concurrency (registry-declared, opt-in)",
      claim_text: "containment on a shared dev box is an OPERATOR measure",
    },
    {
      id: "part-negative-premise-stands",
      path: "tests/README.md",
      heading: "Gate-level concurrency (registry-declared, opt-in)",
      claim_text: "A future\nspawn path outside all three can silently escape the bounds",
    },
  ] satisfies readonly StructuralRelocationGuard[]);

export const WP8_RETAINED_MEMORY_IDS = Object.freeze([
  "testing.tier-memory.spend-measurements",
  "testing.predicate.tier-memory-durable-measurements",
  "testing.source-observation.tier-memory-spend-measurements",
] as const);

export const WP8_RETAINED_MEMORY_RELOCATION_GUARD: StructuralRelocationGuard = Object.freeze({
  id: "testing.tier-memory.spend-measurements",
  path: "tests/README.md",
  heading: "Gate-level concurrency (registry-declared, opt-in)",
});

export const WP8_RETIRED_RELOCATION_GUARDS: readonly StructuralRelocationGuard[] = Object.freeze([
  { id: "testing.backgrounded-check-ts-full-launched-sub-agent-s", path: "AGENTS.md", heading: "Build & verify" },
  { id: "testing.source-observation.backgrounded-check-ts-full-launched-sub-agent-s", path: "AGENTS.md", heading: "Build & verify" },
  { id: "testing.e0463-t-find-crate-core-scratch-tree-std", path: "AGENTS.md", heading: "Build & verify" },
  { id: "testing.trigger.e0463-t-find-crate-core-scratch-tree-std", path: "AGENTS.md", heading: "Build & verify" },
  { id: "testing.full-suite-flake-attributed-hardened-acquire-scratch-lock", path: "tests/README.md", heading: "Operational incident attribution and evidence capture" },
  { id: "testing.trigger.full-suite-flake-attributed-hardened-acquire-scratch-lock", path: "tests/README.md", heading: "Operational incident attribution and evidence capture" },
  { id: "testing.migration-handoff-s-complete-list-found-survey-negative", path: "docs/docs/output_format.mdx", heading: "Upgrading a crate generated before the `no_std` output" },
  { id: "testing.trigger.migration-handoff-s-complete-list-found-survey-negative", path: "docs/docs/output_format.mdx", heading: "Upgrading a crate generated before the `no_std` output" },
  { id: "testing.shared-cargo-target-dir-across-same-named-scratch", path: "AGENTS.md", heading: "Build & verify" },
  { id: "testing.trigger.shared-cargo-target-dir-across-same-named-scratch", path: "AGENTS.md", heading: "Build & verify" },
  { id: "testing.tier-s-peak-memory-bounded-arithmetic-over-assumed", path: "tests/README.md", heading: "Gate-level concurrency (registry-declared, opt-in)" },
  { id: "testing.source-observation.tier-s-peak-memory-bounded-arithmetic-over-assumed", path: "AGENTS.md", heading: "Build & verify" },
  { id: "testing.verify-ts-warm-up-cargo-test-exit-attributed", path: "AGENTS.md", heading: "Build & verify" },
  { id: "testing.source-observation.verify-ts-warm-up-cargo-test-exit-attributed", path: "AGENTS.md", heading: "Build & verify" },
  { id: "testing.working-tree-gate-run-concurrent-live-implementation-agent", path: "AGENTS.md", heading: "Build & verify" },
  { id: "testing.trigger.working-tree-gate-run-concurrent-live-implementation-agent", path: "AGENTS.md", heading: "Build & verify" },
] satisfies readonly StructuralRelocationGuard[]);

function issue(logicalPath: string, message: string, code: RoadmapIssue["code"]): RoadmapIssue {
  return { code, source: "<structural-relocation>", logical_path: logicalPath, message, exit: 1 };
}

export function validateNorthStarStructuralRelocations(view: RegistryView): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  if (NORTH_STAR_STRUCTURAL_RELOCATION_GUARDS.length !== NORTH_STAR_STRUCTURAL_RELOCATION_FLOOR) {
    issues.push(issue(
      "north-star.floor",
      `north-star relocation inventory requires exactly ${NORTH_STAR_STRUCTURAL_RELOCATION_FLOOR} rows, found ${NORTH_STAR_STRUCTURAL_RELOCATION_GUARDS.length}`,
      "E-SCHEMA-FLOOR",
    ));
  }
  const ids = new Set<string>();
  for (const guard of NORTH_STAR_STRUCTURAL_RELOCATION_GUARDS) {
    const path = `north-star[${JSON.stringify(guard.id)}]`;
    if (ids.has(guard.id)) {
      issues.push(issue(path, "duplicate structural relocation guard ID", "E-ID-DUPLICATE"));
    }
    ids.add(guard.id);
    const matches = view.tracked_headings.filter((fact) =>
      fact.path === guard.path && fact.heading === guard.heading
    );
    if (matches.length !== 1) {
      issues.push(issue(
        path,
        `durable file-heading destination must resolve exactly once, found ${matches.length}: ${guard.path} → ${guard.heading}`,
        "E-REFERENCE-UNRESOLVED",
      ));
    }
    if (guard.claim_text !== undefined) {
      const claimText = guard.claim_text;
      const claimMatches = view.tracked_headings.filter((fact) =>
        fact.path === guard.path && fact.heading === guard.heading &&
        fact.section_text?.includes(claimText) === true
      );
      if (claimMatches.length !== 1) {
        issues.push(issue(path, `durable destination is missing exact fact text ${JSON.stringify(guard.claim_text)}`, "E-REFERENCE-UNRESOLVED"));
      }
    }
  }
  if (WP8_RETIRED_STRUCTURAL_PART_RELOCATION_GUARDS.length !== WP8_RETIRED_STRUCTURAL_PART_IDS.length) {
    issues.push(issue(
      "wp8.parts.floor",
      `structural-part relocation inventory requires exactly ${WP8_RETIRED_STRUCTURAL_PART_IDS.length} rows, found ${WP8_RETIRED_STRUCTURAL_PART_RELOCATION_GUARDS.length}`,
      "E-SCHEMA-FLOOR",
    ));
  }
  for (const guard of WP8_RETIRED_STRUCTURAL_PART_RELOCATION_GUARDS) {
    const path = `wp8.parts[${JSON.stringify(guard.id)}]`;
    const matches = view.tracked_headings.filter((fact) =>
      fact.path === guard.path && fact.heading === guard.heading &&
      (guard.claim_text === undefined || fact.section_text?.includes(guard.claim_text) === true)
    );
    if (matches.length !== 1) {
      issues.push(issue(
        path,
        `durable structural-part destination must resolve exactly once with its fact text, found ${matches.length}: ${guard.path} → ${guard.heading}`,
        "E-REFERENCE-UNRESOLVED",
      ));
    }
  }
  return Object.freeze(issues);
}

/** Require WP8's retirement to remain one indivisible structural shape once any of its IDs appears. */
export function validateWp8TestingRelocation(
  document: RoadmapDocument,
  retired: RetiredIdsDocumentV1,
): readonly RoadmapIssue[] {
  if (document.document.roadmap !== "testing") return Object.freeze([]);
  const expectedIds = new Set(WP8_RETIRED_RELOCATION_GUARDS.map((guard) => guard.id));
  const entries = new Map(retired.entries.map((entry) => [String(entry.id), entry]));
  if (![...expectedIds].some((id) => entries.has(id))) return Object.freeze([]);
  const issues: RoadmapIssue[] = [];
  if (expectedIds.size !== 16 || WP8_RETIRED_RELOCATION_GUARDS.length !== 16) {
    issues.push(issue("wp8.retired.floor", "WP8 retirement inventory requires exactly 16 unique IDs", "E-SCHEMA-FLOOR"));
  }
  for (const guard of WP8_RETIRED_RELOCATION_GUARDS) {
    const entry = entries.get(guard.id);
    const path = `wp8.retired[${JSON.stringify(guard.id)}]`;
    if (entry === undefined) {
      issues.push(issue(path, "WP8 retirement is missing one exact tombstone", "E-SCHEMA-FLOOR"));
    } else if (entry.last_active_at !== WP8_LAST_ACTIVE_AT) {
      issues.push(issue(path, `WP8 tombstone must retain exact last_active_at ${WP8_LAST_ACTIVE_AT}`, "E-RETIRED-HASH"));
    } else if (entry.replacement.kind !== "file_heading" || entry.replacement.path !== guard.path ||
      entry.replacement.heading !== guard.heading) {
      issues.push(issue(path, `WP8 tombstone must pin ${guard.path} → ${guard.heading}`, "E-RETIRED-REPLACEMENT"));
    }
  }
  const activeIds = new Set(document.records.map((record) => String(record.id)));
  for (const id of expectedIds) if (activeIds.has(id)) {
    issues.push(issue(`wp8.active[${JSON.stringify(id)}]`, "retired WP8 ID remains active", "E-TRANSACTION-OWNER"));
  }
  const partIds = new Set(document.parts.map((part) => String(part.part_id)));
  for (const id of WP8_RETIRED_STRUCTURAL_PART_IDS) if (partIds.has(id)) {
    issues.push(issue(`wp8.part[${JSON.stringify(id)}]`, "retired WP8 structural part remains active", "E-TRANSACTION-OWNER"));
  }
  const structuralParts = new Set<string>(WP8_RETIRED_STRUCTURAL_PART_IDS);
  for (const entry of document.manifest) {
    const owner = entry.kind === "record" ? String(entry.record_id) : entry.kind === "part" ? String(entry.part_id) : undefined;
    if (owner !== undefined && (expectedIds.has(owner) || structuralParts.has(owner))) {
      issues.push(issue(`wp8.manifest[${JSON.stringify(owner)}]`, "retired WP8 owner remains placed", "E-MANIFEST-ORPHAN"));
    }
  }
  for (const span of document.spans) if (expectedIds.has(span.owner_id) ||
    structuralParts.has(span.owner_id)) {
    issues.push(issue(`wp8.span[${JSON.stringify(span.id)}]`, "relocated WP8 provenance span remains", "E-SPAN-OWNER"));
  }
  if ("relations" in document) for (const relation of document.relations) if (
    expectedIds.has(String(relation.source)) || expectedIds.has(String(relation.target))
  ) {
    issues.push(issue(`wp8.relation[${JSON.stringify([relation.source, relation.kind, relation.target])}]`, "retired WP8 ID remains a relation endpoint", "E-TRANSACTION-REFERENCE"));
  }
  if ("references" in document) for (const reference of document.references) if (
    expectedIds.has(String(reference.source)) || reference.kind === "roadmap" && expectedIds.has(String(reference.target_id))
  ) {
    issues.push(issue(`wp8.reference[${JSON.stringify(reference.id)}]`, "retired WP8 ID remains a typed reference endpoint", "E-TRANSACTION-REFERENCE"));
  }
  for (const id of WP8_RETAINED_MEMORY_IDS) if (!activeIds.has(id)) {
    issues.push(issue(`wp8.retained[${JSON.stringify(id)}]`, "future tier-memory work triad was removed", "E-SCHEMA-FLOOR"));
  }
  const retainedWork = document.records.find((record) => String(record.id) === WP8_RETAINED_MEMORY_IDS[0]);
  if (retainedWork === undefined || !("projection_visibility" in retainedWork) ||
    retainedWork.projection_visibility !== "document") {
    issues.push(issue("wp8.retained.visibility", "future tier-memory work must remain document-visible", "E-SCHEMA-FLOOR"));
  }
  if (retainedWork === undefined || !("render_authority" in retainedWork) ||
    retainedWork.render_authority !== "semantic" || retainedWork.projection_group !== "operational-watches" ||
    retainedWork.payload.kind !== "work" ||
    !new TextDecoder("utf-8", { fatal: true }).decode(retainedWork.payload.detail_md)
      .startsWith("- **Spend the measurements.**")) {
    issues.push(issue(
      "wp8.retained.placement",
      "future tier-memory work must remain an independent top-level work item in the operational systems/resource bucket",
      "E-SCHEMA-FLOOR",
    ));
  }
  const retainedManifestEntries = document.manifest.filter((entry) =>
    entry.kind === "record" && String(entry.record_id) === WP8_RETAINED_MEMORY_IDS[0]
  );
  if (retainedManifestEntries.length !== 1) {
    issues.push(issue("wp8.retained.manifest", "future tier-memory work must have one exact render placement", "E-SCHEMA-FLOOR"));
  }
  const retainedSpans = document.spans.filter((span) =>
    span.owner_id === WP8_RETAINED_MEMORY_IDS[0] && span.id === "span-record-tier-memory-spend-measurements"
  );
  if (retainedSpans.length !== 1) {
    issues.push(issue("wp8.retained.span", "future tier-memory work must retain one independent record provenance span", "E-SCHEMA-FLOOR"));
  }
  return Object.freeze(issues);
}
