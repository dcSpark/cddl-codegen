import type {
  ByteInterval,
  OutputClaim,
  ResolvedOutputClaim,
} from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import type { RepoPath, RoadmapName, SlotId } from "./model/core.ts";
import type { RoadmapDocument } from "./model/documents.ts";
import type { CompletedRenderIr } from "./render_ir.ts";

export type { ByteInterval, OutputClaim, ResolvedOutputClaim } from "./adapters/types.ts";

export interface ManifestSlotBindingFact {
  readonly roadmap: RoadmapName;
  readonly path: RepoPath;
  readonly slot_id: SlotId;
  readonly declaration_count: number;
  readonly placement_count: number;
  readonly owner_span_count: number;
  readonly interval?: ByteInterval;
  readonly payload_interval?: ByteInterval;
}

export interface OutputResolutionInput {
  readonly registry: ClosedOutputRegistry;
  readonly claims: readonly OutputClaim[];
  readonly targets: ReadonlyMap<RepoPath, Uint8Array>;
  readonly manifest_slots?: readonly ManifestSlotBindingFact[];
  readonly observer?: { claimResolved(claim: ResolvedOutputClaim): void };
}

export interface OutputResolution {
  readonly resolved: readonly ResolvedOutputClaim[];
  readonly issues: readonly RoadmapIssue[];
  readonly authority?: ValidatedOutputAuthority;
}

export interface ClosedOutputRegistry {
  readonly claim_count: number;
}

export interface ValidatedOutputAuthority {
  readonly resolved_count: number;
}

export type ClosedOutputRegistryResult =
  | { readonly ok: true; readonly registry: ClosedOutputRegistry; readonly issues: readonly [] }
  | { readonly ok: false; readonly issues: readonly RoadmapIssue[] };

export type ProductionOutputRegistryValidation =
  | { readonly ok: true; readonly registry: ClosedOutputRegistry; readonly issues: readonly [] }
  | { readonly ok: false; readonly issues: readonly RoadmapIssue[] };

const registeredClaims = new WeakMap<object, readonly OutputClaim[]>();
const productionRegistries = new WeakSet<object>();
const testRegistries = new WeakSet<object>();
const authorizedClaims = new WeakMap<object, {
  readonly resolved: readonly ResolvedOutputClaim[];
  readonly scope: "production" | "test";
}>();

export interface StatusMarkerInspection {
  readonly open_count: number;
  readonly close_count: number;
  readonly open_offsets: readonly number[];
  readonly close_offsets: readonly number[];
  readonly interval?: ByteInterval;
  readonly payload_interval?: ByteInterval;
  readonly ordered: boolean;
}

const encoder = new TextEncoder();

function issue(
  code: "E-OUTPUT-PATH" | "E-OUTPUT-CLAIM" | "E-OUTPUT-WRITER" | "E-OUTPUT-SLOT",
  source: string,
  logical_path: string,
  message: string,
): RoadmapIssue {
  return { code, source, logical_path, message, exit: 1 };
}

function validRepoPath(path: string): boolean {
  if (
    path.length === 0 || path.includes("\0") || path.includes("\\") ||
    path.startsWith("/") || path.endsWith("/")
  ) return false;
  const components = path.split("/");
  return components.every((component) => component !== "" && component !== "." && component !== "..");
}

function bindingIndex(claim: Extract<OutputClaim, { kind: "slot" }>): string {
  return JSON.stringify(claim.interval.binding);
}

function claimIndex(claim: OutputClaim): string {
  return claim.kind === "whole_file"
    ? JSON.stringify([claim.kind, claim.producer, claim.path])
    : JSON.stringify([claim.kind, claim.producer, claim.path, claim.slot_id, claim.interval.binding]);
}

function snapshotClaim(claim: OutputClaim): OutputClaim {
  if (claim.kind === "whole_file") {
    return Object.freeze({
      kind: claim.kind,
      producer: claim.producer,
      path: claim.path,
      interval: Object.freeze({ kind: "whole_file" as const }),
    });
  }
  const binding = claim.interval.binding.kind === "status_header_markers"
    ? Object.freeze({
      kind: "status_header_markers" as const,
      marker_id: claim.interval.binding.marker_id,
    })
    : Object.freeze({
      kind: "manifest_generated_slot" as const,
      roadmap: claim.interval.binding.roadmap,
      slot_id: claim.interval.binding.slot_id,
    });
  return Object.freeze({
    kind: claim.kind,
    producer: claim.producer,
    path: claim.path,
    slot_id: claim.slot_id,
    interval: Object.freeze({
      kind: "binding" as const,
      binding,
      cardinality: Object.freeze({ exact: 1 as const }),
    }),
  });
}

/** Close one trusted producer/path/slot inventory. Resolution requests cannot add to it. */
function createClosedOutputRegistry(
  claims: readonly OutputClaim[],
  scope: "production" | "test",
): ClosedOutputRegistryResult {
  const issues = validateOutputClaimInventory(claims);
  if (issues.length > 0) return Object.freeze({ ok: false, issues });
  const registry: ClosedOutputRegistry = Object.freeze({ claim_count: claims.length });
  registeredClaims.set(registry, Object.freeze(claims.map(snapshotClaim)));
  (scope === "production" ? productionRegistries : testRegistries).add(registry);
  return Object.freeze({ ok: true, registry, issues: Object.freeze([]) as readonly [] });
}

/** Test-only inventory constructor. Authorities minted from it are rejected by write_plan.ts. */
export function createTestOutputRegistry(claims: readonly OutputClaim[]): ClosedOutputRegistryResult {
  return createClosedOutputRegistry(claims, "test");
}

const STATUS_PATHS = Object.freeze({
  roadmap: "cddl-matrix/ROADMAP.md" as RepoPath,
  matrix_readme: "cddl-matrix/README.md" as RepoPath,
  tests_readme: "tests/README.md" as RepoPath,
});

export const LEGACY_STATUS_OUTPUT_CLAIMS: readonly OutputClaim[] = Object.freeze([
  [STATUS_PATHS.roadmap, "roadmap-counts"],
  [STATUS_PATHS.roadmap, "roadmap-ops"],
  [STATUS_PATHS.roadmap, "roadmap-emission"],
  [STATUS_PATHS.roadmap, "roadmap-constraint"],
  [STATUS_PATHS.matrix_readme, "readme-counts"],
  [STATUS_PATHS.matrix_readme, "readme-annotations"],
  [STATUS_PATHS.matrix_readme, "readme-ops"],
  [STATUS_PATHS.matrix_readme, "readme-enforce-green"],
  [STATUS_PATHS.tests_readme, "tests-ignored-gates"],
  [STATUS_PATHS.tests_readme, "tests-tier-fast"],
  [STATUS_PATHS.tests_readme, "tests-tier-local"],
  [STATUS_PATHS.tests_readme, "tests-tier-full"],
].map(([path, id]) => {
  const slotId = id as SlotId;
  return Object.freeze({
    kind: "slot" as const,
    producer: "project_status_headers",
    path: path as RepoPath,
    slot_id: slotId,
    interval: Object.freeze({
      kind: "binding" as const,
      binding: Object.freeze({ kind: "status_header_markers" as const, marker_id: slotId }),
      cardinality: Object.freeze({ exact: 1 as const }),
    }),
  });
}));

function fixedProductionRegistry(): ClosedOutputRegistry {
  const result = createClosedOutputRegistry(LEGACY_STATUS_OUTPUT_CLAIMS, "production");
  if (!result.ok) throw new Error("internal: fixed legacy status inventory is invalid");
  return result.registry;
}

/** The sole WP1 production inventory; it intentionally contains no projection whole-file claim. */
export const LEGACY_STATUS_OUTPUT_REGISTRY = fixedProductionRegistry();

/**
 * Revalidate an injected revision view against WP1's one fixed production ownership inventory.
 * This grants only the closed registry capability; exact byte intervals still require the
 * separately explicit target snapshots accepted by resolveOutputClaims.
 */
export function validateProductionOutputRegistry(
  claims: readonly OutputClaim[],
): ProductionOutputRegistryValidation {
  const inventory = registeredClaims.get(LEGACY_STATUS_OUTPUT_REGISTRY);
  if (inventory === undefined) throw new Error("internal: fixed production output inventory lost provenance");
  const issues: RoadmapIssue[] = [...validateOutputClaimInventory(claims)];
  if (claims.length !== inventory.length) {
    issues.push(issue(
      "E-OUTPUT-CLAIM",
      "<output-registry>",
      "claims",
      `production output inventory has ${claims.length} claims, expected exactly ${inventory.length}`,
    ));
  }
  for (const [index, claim] of claims.entries()) {
    const mismatch = registryMismatchIssue(claim, inventory, index);
    if (mismatch !== undefined) issues.push(mismatch);
  }
  for (const [index, registered] of inventory.entries()) {
    if (!claims.some((claim) => claimIndex(claim) === claimIndex(registered))) {
      issues.push(issue(
        "E-OUTPUT-CLAIM",
        registered.path,
        `inventory[${index}]`,
        "registered production output claim is absent from the injected revision view",
      ));
    }
  }
  if (issues.length > 0) return Object.freeze({ ok: false, issues: Object.freeze(issues) });
  return Object.freeze({
    ok: true,
    registry: LEGACY_STATUS_OUTPUT_REGISTRY,
    issues: Object.freeze([]) as readonly [],
  });
}

function registryMismatchIssue(
  requested: OutputClaim,
  inventory: readonly OutputClaim[],
  index: number,
): RoadmapIssue | undefined {
  if (inventory.some((claim) => claimIndex(claim) === claimIndex(requested))) return undefined;
  const source = requested.path;
  const logicalPath = `claims[${index}]`;
  if (!inventory.some((claim) => claim.path === requested.path)) {
    return issue("E-OUTPUT-PATH", source, logicalPath, "requested output path is not registered");
  }
  if (!inventory.some((claim) => claim.path === requested.path && claim.producer === requested.producer)) {
    return issue("E-OUTPUT-WRITER", source, logicalPath, "requested output producer is not registered for this path");
  }
  if (requested.kind === "slot") {
    return issue("E-OUTPUT-SLOT", source, logicalPath, "requested slot or structured binding is not registered");
  }
  return issue("E-OUTPUT-CLAIM", source, logicalPath, "requested whole-file claim is not registered");
}

export function resolvedWholeFileClaim(
  authority: ValidatedOutputAuthority,
  path: RepoPath,
): ResolvedOutputClaim | undefined {
  return authorizedClaims.get(authority)?.resolved.find((resolved) =>
    resolved.claim.kind === "whole_file" && resolved.path === path
  );
}

export function isProductionOutputAuthority(authority: ValidatedOutputAuthority): boolean {
  return authorizedClaims.get(authority)?.scope === "production";
}

function findAll(haystack: Uint8Array, needle: Uint8Array): number[] {
  const offsets: number[] = [];
  outer: for (let start = 0; start + needle.byteLength <= haystack.byteLength; start++) {
    for (let index = 0; index < needle.byteLength; index++) {
      if (haystack[start + index] !== needle[index]) continue outer;
    }
    offsets.push(start);
  }
  return offsets;
}

function strictUtf8(bytes: Uint8Array): boolean {
  try {
    new TextDecoder("utf-8", { fatal: true }).decode(bytes);
    return true;
  } catch {
    return false;
  }
}

export function inspectStatusMarkerBinding(bytes: Uint8Array, markerId: SlotId): StatusMarkerInspection {
  const openBytes = encoder.encode(`<!-- gen:sh:${markerId} -->`);
  const closeBytes = encoder.encode(`<!-- /gen:sh:${markerId} -->`);
  const opens = findAll(bytes, openBytes);
  const closes = findAll(bytes, closeBytes);
  const payloadStart = opens.length === 1 ? opens[0] + openBytes.byteLength : -1;
  const payloadEnd = closes.length === 1 ? closes[0] : -1;
  const nestedMarkerOffsets = [
    ...findAll(bytes, encoder.encode("<!-- gen:sh:")),
    ...findAll(bytes, encoder.encode("<!-- /gen:sh:")),
  ].filter((offset) => offset >= payloadStart && offset < payloadEnd);
  const ordered = opens.length === 1 && closes.length === 1 && payloadStart <= payloadEnd &&
    nestedMarkerOffsets.length === 0;
  if (!ordered) {
    return Object.freeze({
      open_count: opens.length,
      close_count: closes.length,
      open_offsets: Object.freeze(opens),
      close_offsets: Object.freeze(closes),
      ordered: false,
    });
  }
  return Object.freeze({
    open_count: 1,
    close_count: 1,
    open_offsets: Object.freeze(opens),
    close_offsets: Object.freeze(closes),
    interval: Object.freeze({
      start_byte: opens[0],
      end_byte: closes[0] + closeBytes.byteLength,
    }),
    payload_interval: Object.freeze({
      start_byte: opens[0] + openBytes.byteLength,
      end_byte: closes[0],
    }),
    ordered: true,
  });
}

export function intervalsOverlap(left: ByteInterval, right: ByteInterval): boolean {
  return Math.max(left.start_byte, right.start_byte) < Math.min(left.end_byte, right.end_byte);
}

export function collectManifestSlotBindingFacts(
  document: RoadmapDocument,
  completed: CompletedRenderIr,
): readonly ManifestSlotBindingFact[] {
  const facts: ManifestSlotBindingFact[] = [];
  for (const slotId of new Set(document.generated_slots.map((slot) => slot.slot_id))) {
    const declarations = document.generated_slots.filter((slot) => slot.slot_id === slotId);
    const placements = document.manifest.filter((entry) =>
      entry.kind === "generated_slot" && entry.slot_id === slotId
    );
    const spans = document.spans.filter((span) =>
      span.source_kind === "generated_slot" && span.owner_id === slotId &&
      span.owner_field === "generated" && span.migration_status === "generated"
    );
    const chunks = completed.chunks.filter((chunk) =>
      chunk.owner.kind === "generated_slot" && chunk.owner.id === slotId &&
      chunk.owner.field === "generated"
    );
    const resolutions = completed.slot_resolutions.filter((item) => item.slot.slot_id === slotId);
    const placementIndex = document.manifest.findIndex((entry) =>
      entry.kind === "generated_slot" && entry.slot_id === slotId
    );
    const chunkIndex = chunks.length === 1 ? completed.chunks.indexOf(chunks[0]) : -1;
    const chunkInterval = chunkIndex >= 0
      ? {
        start_byte: completed.expected_bytes.prefix_offsets[chunkIndex],
        end_byte: completed.expected_bytes.prefix_offsets[chunkIndex + 1],
      }
      : undefined;
    const declaration = declarations.length === 1 ? declarations[0] : undefined;
    const span = spans.length === 1 ? spans[0] : undefined;
    const resolutionItem = resolutions.length === 1 ? resolutions[0] : undefined;
    const resolution = resolutionItem?.resolution;
    const resolverIsExact =
      declaration !== undefined && resolutionItem !== undefined && resolution !== undefined &&
      placements.length === 1 && chunks.length === 1 && placementIndex >= 0 &&
      resolutionItem.manifest_index === placementIndex && chunks[0].manifest_index === placementIndex &&
      resolutionItem.slot.binding === declaration.binding &&
      resolutionItem.slot.span_ids.length === declaration.span_ids.length &&
      resolutionItem.slot.span_ids.every((value, index) => value === declaration.span_ids[index]) &&
      resolution.binding === declaration.binding && resolution.bytes.byteLength > 0 &&
      resolution.bytes.byteLength === chunks[0].bytes.byteLength &&
      resolution.bytes.every((value, index) => value === chunks[0].bytes[index]);
    const ownerSpanIsExact =
      declaration !== undefined && span !== undefined && chunkInterval !== undefined &&
      declaration.span_ids.length === 1 && declaration.span_ids[0] === span.id &&
      chunks[0].source_span_ids.length === 1 && chunks[0].source_span_ids[0] === span.id &&
      span.start_byte === chunkInterval.start_byte && span.end_byte === chunkInterval.end_byte;
    const resolvedInterval =
      resolverIsExact && ownerSpanIsExact && chunkInterval !== undefined &&
        chunkInterval.start_byte < chunkInterval.end_byte
        ? Object.freeze(chunkInterval)
        : undefined;
    facts.push(Object.freeze({
      roadmap: document.document.roadmap,
      path: document.document.projection_path,
      slot_id: slotId,
      declaration_count: declarations.length,
      placement_count: placements.length,
      owner_span_count: spans.length,
      ...(resolvedInterval === undefined ? {} : {
        interval: resolvedInterval,
        payload_interval: resolvedInterval,
      }),
    }));
  }
  return Object.freeze(facts);
}

/** Validate the typed inventory before reading or resolving any target bytes. */
export function validateOutputClaimInventory(claims: readonly OutputClaim[]): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  if (claims.length === 0) {
    issues.push(issue("E-OUTPUT-CLAIM", "<output-registry>", "claims", "output claim inventory is empty"));
    return Object.freeze(issues);
  }
  const exactClaims = new Set<string>();
  const slotKeys = new Set<string>();
  const bindings = new Set<string>();
  for (const [index, claim] of claims.entries()) {
    const logicalPath = `claims[${index}]`;
    if (!validRepoPath(claim.path)) {
      issues.push(issue("E-OUTPUT-PATH", claim.path, logicalPath, "output path is not a confined repository-relative path"));
    }
    const exact = claimIndex(claim);
    if (exactClaims.has(exact)) {
      issues.push(issue("E-OUTPUT-CLAIM", claim.path, logicalPath, "output claim is duplicated"));
    }
    exactClaims.add(exact);
    if (claim.kind === "slot") {
      if (claim.interval.cardinality.exact !== 1) {
        issues.push(issue("E-OUTPUT-SLOT", claim.path, logicalPath, "slot cardinality must be exactly one"));
      }
      const binding = claim.interval.binding;
      const bindingSlot = binding.kind === "status_header_markers" ? binding.marker_id : binding.slot_id;
      if (claim.slot_id !== bindingSlot) {
        issues.push(issue("E-OUTPUT-SLOT", claim.path, logicalPath, "enclosing slot_id does not match binding slot ID"));
      }
      const slotKey = JSON.stringify([claim.path, claim.slot_id]);
      if (slotKeys.has(slotKey)) {
        issues.push(issue("E-OUTPUT-CLAIM", claim.path, logicalPath, "path/slot pair is claimed more than once"));
      }
      slotKeys.add(slotKey);
      const bindingKey = JSON.stringify([claim.path, bindingIndex(claim)]);
      if (bindings.has(bindingKey)) {
        issues.push(issue("E-OUTPUT-CLAIM", claim.path, logicalPath, "structured output binding is claimed more than once"));
      }
      bindings.add(bindingKey);
    }
  }
  return Object.freeze(issues);
}

function resolveManifestClaim(
  claim: Extract<OutputClaim, { kind: "slot" }>,
  facts: readonly ManifestSlotBindingFact[],
): ResolvedOutputClaim | RoadmapIssue {
  const binding = claim.interval.binding;
  if (binding.kind !== "manifest_generated_slot") {
    throw new Error("internal: status binding passed to manifest resolver");
  }
  const matches = facts.filter((fact) =>
    fact.roadmap === binding.roadmap && fact.path === claim.path && fact.slot_id === binding.slot_id
  );
  if (
    matches.length !== 1 || matches[0].declaration_count !== 1 || matches[0].placement_count !== 1 ||
    matches[0].owner_span_count !== 1 || matches[0].interval === undefined ||
    matches[0].payload_interval === undefined
  ) {
    return issue(
      "E-OUTPUT-SLOT",
      claim.path,
      `slot[${JSON.stringify(claim.slot_id)}]`,
      "manifest slot must have exactly one declaration, placement, matching owner span, and resolved interval",
    );
  }
  const interval = matches[0].interval;
  const payloadInterval = matches[0].payload_interval;
  if (
    !Number.isSafeInteger(interval.start_byte) || !Number.isSafeInteger(interval.end_byte) ||
    interval.start_byte < 0 || interval.start_byte >= interval.end_byte ||
    payloadInterval.start_byte < interval.start_byte || payloadInterval.end_byte > interval.end_byte ||
    payloadInterval.start_byte >= payloadInterval.end_byte
  ) {
    return issue(
      "E-OUTPUT-SLOT",
      claim.path,
      `slot[${JSON.stringify(claim.slot_id)}]`,
      "manifest slot intervals are invalid or empty",
    );
  }
  return Object.freeze({
    claim,
    path: claim.path,
    interval,
    payload_interval: payloadInterval,
  });
}

/** Resolve every typed claim to exact UTF-8 byte intervals, then reject all same-path overlap. */
export function resolveOutputClaims(input: OutputResolutionInput): OutputResolution {
  const inventory = registeredClaims.get(input.registry);
  const issues: RoadmapIssue[] = [...validateOutputClaimInventory(input.claims)];
  const registryScope = productionRegistries.has(input.registry)
    ? "production"
    : testRegistries.has(input.registry) ? "test" : undefined;
  if (inventory === undefined || registryScope === undefined || input.registry.claim_count !== inventory.length) {
    issues.push(issue("E-OUTPUT-CLAIM", "<output-registry>", "registry", "output registry capability is invalid or caller-constructed"));
  } else {
    for (const [index, claim] of input.claims.entries()) {
      const mismatch = registryMismatchIssue(claim, inventory, index);
      if (mismatch !== undefined) issues.push(mismatch);
    }
  }
  const resolutionClaims = input.claims.map((requested) =>
    inventory?.find((registered) => claimIndex(registered) === claimIndex(requested)) ?? requested
  );
  const resolved: ResolvedOutputClaim[] = [];
  for (const [index, claim] of resolutionClaims.entries()) {
    if (claim.kind === "slot" && claim.interval.binding.kind === "manifest_generated_slot") {
      const result = resolveManifestClaim(claim, input.manifest_slots ?? []);
      if ("code" in result) issues.push(result);
      else {
        resolved.push(result);
        input.observer?.claimResolved(result);
      }
      continue;
    }
    const target = input.targets.get(claim.path);
    if (target === undefined) {
      issues.push(issue("E-OUTPUT-PATH", claim.path, `claims[${index}]`, "output target snapshot is missing"));
      continue;
    }
    const snapshot = new Uint8Array(target);
    if (!strictUtf8(snapshot)) {
      issues.push(issue("E-OUTPUT-SLOT", claim.path, `claims[${index}]`, "output target is not strict UTF-8"));
      continue;
    }
    if (claim.kind === "whole_file") {
      if (snapshot.byteLength === 0) {
        issues.push(issue("E-OUTPUT-CLAIM", claim.path, `claims[${index}]`, "whole-file interval is empty"));
        continue;
      }
      const interval = Object.freeze({ start_byte: 0, end_byte: snapshot.byteLength });
      const result = Object.freeze({ claim, path: claim.path, interval, payload_interval: interval });
      resolved.push(result);
      input.observer?.claimResolved(result);
      continue;
    }
    const statusBinding = claim.interval.binding;
    if (statusBinding.kind !== "status_header_markers") {
      issues.push(issue("E-OUTPUT-SLOT", claim.path, `claims[${index}]`, "unknown slot binding arm"));
      continue;
    }
    const inspected = inspectStatusMarkerBinding(snapshot, statusBinding.marker_id);
    if (
      inspected.open_count !== 1 || inspected.close_count !== 1 || !inspected.ordered ||
      inspected.interval === undefined || inspected.payload_interval === undefined ||
      inspected.payload_interval.start_byte === inspected.payload_interval.end_byte
    ) {
      issues.push(issue(
        "E-OUTPUT-SLOT",
        claim.path,
        `slot[${JSON.stringify(claim.slot_id)}]`,
        `status marker binding has open=${inspected.open_count}, close=${inspected.close_count}, ordered=${inspected.ordered}; expected one nonempty ordered pair`,
      ));
      continue;
    }
    const result = Object.freeze({
      claim,
      path: claim.path,
      interval: inspected.interval,
      payload_interval: inspected.payload_interval,
    });
    resolved.push(result);
    input.observer?.claimResolved(result);
  }

  for (let leftIndex = 0; leftIndex < resolved.length; leftIndex++) {
    for (let rightIndex = leftIndex + 1; rightIndex < resolved.length; rightIndex++) {
      const left = resolved[leftIndex];
      const right = resolved[rightIndex];
      if (left.path === right.path && intervalsOverlap(left.interval, right.interval)) {
        issues.push(issue(
          "E-OUTPUT-CLAIM",
          left.path,
          `overlap[${leftIndex},${rightIndex}]`,
          `resolved intervals [${left.interval.start_byte},${left.interval.end_byte}) and [${right.interval.start_byte},${right.interval.end_byte}) overlap`,
        ));
      }
    }
  }
  if (issues.length > 0 || resolved.length !== input.claims.length) {
    return Object.freeze({ resolved: Object.freeze(resolved), issues: Object.freeze(issues) });
  }
  const authority: ValidatedOutputAuthority = Object.freeze({ resolved_count: resolved.length });
  authorizedClaims.set(authority, {
    resolved: Object.freeze([...resolved]),
    scope: registryScope as "production" | "test",
  });
  return Object.freeze({
    resolved: Object.freeze(resolved),
    issues: Object.freeze(issues),
    authority,
  });
}
