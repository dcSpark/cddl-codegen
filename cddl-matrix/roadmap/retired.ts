import type { RegistryView } from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import type { FullCommitId, RoadmapId } from "./model/core.ts";
import type {
  ReplacementPin,
  RetiredIdV1,
  RetiredIdsDocumentV1,
  TombstoneEligibleBaseOwner,
} from "./model/documents.ts";

const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

export const REPLACEMENT_PIN_KINDS = Object.freeze(["gate", "test_symbol", "file_heading"] as const);

export interface ReplacementResolution {
  readonly resolved: boolean;
  readonly provider?: "gate" | "test_symbol" | "file_heading";
  readonly reason?: string;
}

function issue(
  code: Extract<RoadmapIssue["code"], `E-RETIRED-${string}` | "E-ID-DUPLICATE">,
  logicalPath: string,
  message: string,
): RoadmapIssue {
  return {
    code,
    source: "roadmap-retired-ids.toml",
    logical_path: logicalPath,
    message,
    exit: 1,
  };
}

function replacementIndex(pin: ReplacementPin): string {
  switch (pin.kind) {
    case "gate":
      return JSON.stringify([pin.kind, pin.gate_id, [...pin.claim_md]]);
    case "test_symbol":
      return JSON.stringify([pin.kind, pin.test_id, pin.symbol, [...pin.claim_md]]);
    case "file_heading":
      return JSON.stringify([pin.kind, pin.path, pin.heading, [...pin.claim_md]]);
  }
}

export function retiredEntryIndex(entry: RetiredIdV1): string {
  return JSON.stringify([entry.id, entry.last_active_at, replacementIndex(entry.replacement)]);
}

/** Resolve a durable replacement only from the injected candidate revision registry. */
export function resolveReplacementPin(pin: ReplacementPin, view: RegistryView): ReplacementResolution {
  if (pin.claim_md.byteLength === 0) return { resolved: false, reason: "replacement claim is empty" };
  switch (pin.kind) {
    case "gate": {
      const matches = view.gates.filter((fact) => fact.id === pin.gate_id && !fact.stub);
      return matches.length === 1
        ? { resolved: true, provider: "gate" }
        : { resolved: false, reason: "gate replacement must resolve exactly once to a non-stub gate" };
    }
    case "test_symbol": {
      const matches = view.test_symbols.filter((fact) =>
        fact.test_id === pin.test_id && fact.symbol === pin.symbol
      );
      return matches.length === 1
        ? { resolved: true, provider: "test_symbol" }
        : { resolved: false, reason: "test-symbol replacement requires the exact derived test ID and symbol" };
    }
    case "file_heading": {
      if (pin.path === "draft" || pin.path.startsWith("draft/")) {
        return { resolved: false, reason: "draft paths cannot be durable replacements" };
      }
      const matches = view.tracked_headings.filter((fact) =>
        fact.path === pin.path && fact.heading === pin.heading
      );
      return matches.length === 1
        ? { resolved: true, provider: "file_heading" }
        : { resolved: false, reason: "file-heading replacement must resolve exactly once in tracked non-draft facts" };
    }
  }
}

export interface RetiredValidationResult {
  readonly entries: ReadonlyMap<RoadmapId, RetiredIdV1>;
  readonly issues: readonly RoadmapIssue[];
}

export function validateRetiredIds(
  document: RetiredIdsDocumentV1,
  view: RegistryView,
): RetiredValidationResult {
  const entries = new Map<RoadmapId, RetiredIdV1>();
  const issues: RoadmapIssue[] = [];
  const sortedEntries = [...document.entries].sort((left, right) => codePointSort(left.id, right.id));
  for (const [index, entry] of sortedEntries.entries()) {
    const path = `retired_ids.entry[${index}]`;
    if (entries.has(entry.id)) {
      issues.push(issue("E-ID-DUPLICATE", path, `duplicate retired ID ${JSON.stringify(entry.id)}`));
      continue;
    }
    entries.set(entry.id, entry);
    if (!/^[0-9a-f]{40}(?:[0-9a-f]{24})?$/.test(entry.last_active_at)) {
      issues.push(issue("E-RETIRED-HASH", `${path}.last_active_at`, "last_active_at must be one full lowercase 40- or 64-hex commit ID"));
    }
    const resolution = resolveReplacementPin(entry.replacement, view);
    if (!resolution.resolved) {
      issues.push(issue("E-RETIRED-REPLACEMENT", `${path}.replacement`, resolution.reason ?? "replacement does not resolve"));
    }
  }
  return Object.freeze({
    entries,
    issues: Object.freeze(issues.sort((left, right) =>
      codePointSort(left.logical_path, right.logical_path) || codePointSort(left.code, right.code) ||
      codePointSort(left.message, right.message)
    )),
  });
}

export interface TombstoneOriginFact {
  readonly id: RoadmapId;
  readonly owner_kind: TombstoneEligibleBaseOwner;
}

export interface RetiredTransitionInputs {
  readonly base: RetiredValidationResult;
  readonly candidate: RetiredValidationResult;
  readonly against: FullCommitId;
  readonly eligible_base_origins: readonly TombstoneOriginFact[];
}

export function validateRetiredTransition(inputs: RetiredTransitionInputs): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  const origins = new Map<RoadmapId, TombstoneOriginFact[]>();
  for (const origin of inputs.eligible_base_origins) {
    const group = origins.get(origin.id) ?? [];
    group.push(origin);
    origins.set(origin.id, group);
  }
  for (const [id, base] of inputs.base.entries) {
    const candidate = inputs.candidate.entries.get(id);
    if (candidate === undefined || retiredEntryIndex(base) !== retiredEntryIndex(candidate)) {
      issues.push(issue("E-RETIRED-REUSE", `retired_ids.entry[${JSON.stringify(id)}]`, "a pre-existing tombstone must preserve last_active_at and its exact replacement pin"));
    }
  }
  for (const [id, candidate] of inputs.candidate.entries) {
    if (inputs.base.entries.has(id)) continue;
    const matchingOrigins = origins.get(id) ?? [];
    if (matchingOrigins.length !== 1) {
      issues.push(issue("E-RETIRED-REUSE", `retired_ids.entry[${JSON.stringify(id)}]`, "a new tombstone requires exactly one eligible base owner"));
    }
    if (candidate.last_active_at !== inputs.against) {
      issues.push(issue("E-RETIRED-HASH", `retired_ids.entry[${JSON.stringify(id)}].last_active_at`, "new tombstone last_active_at must equal the explicit --against commit"));
    }
  }
  return Object.freeze(issues.sort((left, right) =>
    codePointSort(left.logical_path, right.logical_path) || codePointSort(left.code, right.code) ||
    codePointSort(left.message, right.message)
  ));
}
