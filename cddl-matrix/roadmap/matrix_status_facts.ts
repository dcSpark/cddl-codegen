import type { ResolvedOutputClaim } from "./adapters/types.ts";
import type { RepoPath, SlotId } from "./model/core.ts";
import type {
  ClassifiedLegacyStatusInvocation,
  LegacyStatusHeaderRunPlan,
  MatrixStatusInputs,
} from "./model/matrix.ts";
import {
  inspectStatusMarkerBinding,
  LEGACY_STATUS_OUTPUT_CLAIMS,
  LEGACY_STATUS_OUTPUT_REGISTRY,
  productionOutputInventory,
  resolveOutputClaims,
  type ProductionOutputStage,
} from "./output_registry.ts";
import { bytesEqual, codePointSort } from "./kernel.ts";

export { LEGACY_STATUS_OUTPUT_CLAIMS, LEGACY_STATUS_OUTPUT_REGISTRY } from "./output_registry.ts";

export interface MatrixStatusFacts {
  readonly features_total: number;
  readonly features_by_profile: ReadonlyMap<string, number>;
  readonly containment_cells: number;
  readonly annotations_total: number;
  readonly control_ops: number;
  readonly divergences: readonly { id: string; profile: string }[];
  readonly divergence_profiles: ReadonlyMap<string, number>;
  readonly constraint_vectors: number;
  readonly enforce_green_rows: readonly string[];
  readonly ignored_gates: readonly string[];
  readonly tier_walls: ReadonlyMap<string, number>;
  readonly validation_problems: readonly string[];
}

export interface MatrixStatusPayload {
  readonly path: RepoPath;
  readonly slot_id: SlotId;
  readonly bytes: Uint8Array;
}

const ROADMAP_PATH = "cddl-matrix/ROADMAP.md" as RepoPath;
const MATRIX_README_PATH = "cddl-matrix/README.md" as RepoPath;
const TESTS_README_PATH = "tests/README.md" as RepoPath;
const TARGET_PATHS = Object.freeze([ROADMAP_PATH, MATRIX_README_PATH, TESTS_README_PATH]);
const PROFILE_ORDER = ["RFC8610", "RFC9682", "CDDL_CODEGEN"] as const;
const WORDS = [
  "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
  "ten", "eleven", "twelve", "thirteen",
] as const;
const encoder = new TextEncoder();
const decoder = new TextDecoder("utf-8", { fatal: true });

function countWord(value: number): string {
  return value >= 0 && value < WORDS.length ? WORDS[value] : String(value);
}

function profileLabel(profile: (typeof PROFILE_ORDER)[number], count: number): string {
  return profile === "CDDL_CODEGEN"
    ? `${count} \`CDDL_CODEGEN\` vendor profile`
    : `${count} ${profile}`;
}

function profileSplit(facts: MatrixStatusFacts): string {
  return PROFILE_ORDER
    .map((profile) => profileLabel(profile, facts.features_by_profile.get(profile) ?? 0))
    .join(" + ");
}

function divergenceClause(facts: MatrixStatusFacts): string {
  const profiles = [...facts.divergence_profiles.keys()].sort(codePointSort);
  if (facts.divergences.length > 0 && profiles.length === 1 && profiles[0] === "preserve") {
    return "all `preserve`-side";
  }
  return profiles
    .map((profile) => `${facts.divergence_profiles.get(profile) ?? 0} \`${profile}\`-side`)
    .join(", ");
}

function humanTierWall(facts: MatrixStatusFacts, tier: string): string {
  const ms = facts.tier_walls.get(tier);
  if (ms === undefined) return "unmeasured";
  const seconds = ms / 1000;
  if (seconds < 60) return `~${Math.round(seconds)}s`;
  const minutes = Math.round((seconds / 60) * 10) / 10;
  return minutes < 10 ? `~${minutes} min` : `~${Math.round(minutes)} min`;
}

/** Purely derive the twelve payload inputs and the live check-mode honesty invariants. */
export function deriveMatrixStatusFacts(inputs: MatrixStatusInputs): MatrixStatusFacts {
  const featuresByProfile = new Map<string, number>();
  for (const feature of inputs.matrix.features) {
    const profile = feature.profile ?? "NONE";
    featuresByProfile.set(profile, (featuresByProfile.get(profile) ?? 0) + 1);
  }
  const divergences: { id: string; profile: string }[] = [];
  for (const annotation of inputs.matrix.annotations) {
    for (const profile of Object.keys(annotation.emission ?? {}).sort(codePointSort)) {
      if (annotation.emission?.[profile]?.status === "unsupported") {
        divergences.push({ id: annotation.id, profile });
      }
    }
  }
  const divergenceProfiles = new Map<string, number>();
  for (const divergence of divergences) {
    divergenceProfiles.set(divergence.profile, (divergenceProfiles.get(divergence.profile) ?? 0) + 1);
  }
  let constraintVectors = 0;
  const enforceRows = new Set<string>();
  for (const row of inputs.catalog.rows) {
    for (const vector of row.vectors) {
      if (vector.expect === "reject" && vector.class === "constraint") {
        constraintVectors++;
        enforceRows.add(row.id);
      }
    }
  }
  const statusById = new Map(inputs.matrix.annotations.map((annotation) => [annotation.id, annotation.status]));
  const ignoredGates = inputs.registry.gates
    .filter((gate) => gate.kind !== "stub" && gate.ignored_test !== undefined)
    .map((gate) => gate.ignored_test as string);
  const tierWalls = new Map<string, number>();
  for (const timing of inputs.timings.tiers) {
    if (timing.wall_ms !== undefined) tierWalls.set(timing.tier, timing.wall_ms);
  }
  const problems: string[] = [];
  const featuresTotal = inputs.matrix.features.length;
  const profileSum = PROFILE_ORDER.reduce(
    (sum, profile) => sum + (featuresByProfile.get(profile) ?? 0),
    0,
  );
  if (profileSum !== featuresTotal) {
    problems.push(`per-profile feature counts sum to ${profileSum} but the features total is ${featuresTotal} — an unmodelled profile leaked in (${[...featuresByProfile.keys()].join(", ")})`);
  }
  for (const profile of featuresByProfile.keys()) {
    if (!PROFILE_ORDER.includes(profile as (typeof PROFILE_ORDER)[number])) {
      problems.push(`feature profile '${profile}' is not in the fixed render order (${PROFILE_ORDER.join(", ")}) — extend PROFILE_ORDER or fix the overlay`);
    }
  }
  for (const rowId of enforceRows) {
    const status = statusById.get(rowId);
    if (status !== "supported") {
      problems.push(`enforce-green row \`${rowId}\` carries a class="constraint" vector but its cddl-codegen annotation is \`${status ?? "absent"}\` (must be supported — the enforce-green label would be dishonest)`);
    }
  }
  if (inputs.matrix.annotations.length < 80) {
    problems.push(`only ${inputs.matrix.annotations.length} annotation rows read (expected >= 80) — the matrix read looks broken/empty`);
  }
  if (featuresTotal < 90) {
    problems.push(`only ${featuresTotal} feature rows read (expected >= 90) — the matrix read looks broken/empty`);
  }
  if (inputs.matrix.containment_ids.length < 60) {
    problems.push(`only ${inputs.matrix.containment_ids.length} containment cells read (expected >= 60) — the matrix read looks broken/empty`);
  }
  if (inputs.matrix.control_operator_ids.length < 30) {
    problems.push(`only ${inputs.matrix.control_operator_ids.length} control operators read (expected >= 30) — the matrix read looks broken/empty`);
  }
  if (constraintVectors < 1) {
    problems.push('no class="constraint" reject vectors found (expected >= 1) — the catalog read looks broken');
  }
  if (enforceRows.size < 1) {
    problems.push("no enforce-green rows found (expected >= 1) — the catalog read looks broken");
  }
  if (ignoredGates.length < 1) {
    problems.push("no manual #[ignore]d gates found in the registry (expected >= 1) — the check.ts import looks broken");
  }
  for (const tier of ["fast", "local", "full"]) {
    if (!tierWalls.has(tier)) {
      problems.push(`tier '${tier}' has no wall_ms row in tests/timings.json — the timings read looks broken (the tier table would render "unmeasured")`);
    }
  }
  return Object.freeze({
    features_total: featuresTotal,
    features_by_profile: featuresByProfile,
    containment_cells: inputs.matrix.containment_ids.length,
    annotations_total: inputs.matrix.annotations.length,
    control_ops: inputs.matrix.control_operator_ids.length,
    divergences: Object.freeze(divergences),
    divergence_profiles: divergenceProfiles,
    constraint_vectors: constraintVectors,
    enforce_green_rows: Object.freeze([...enforceRows]),
    ignored_gates: Object.freeze(ignoredGates),
    tier_walls: tierWalls,
    validation_problems: Object.freeze(problems),
  });
}

/** Render the exact twelve marker payloads in their historical write order. */
export function renderMatrixStatusPayloads(facts: MatrixStatusFacts): readonly MatrixStatusPayload[] {
  const textRows: readonly [RepoPath, string, string][] = [
    [ROADMAP_PATH, "roadmap-counts", `${facts.features_total} features (${profileSplit(facts)}), ${facts.containment_cells} containment cells, and ${facts.annotations_total} cddl-codegen annotations`],
    [ROADMAP_PATH, "roadmap-ops", `all ${facts.control_ops} IANA ops probed`],
    [ROADMAP_PATH, "roadmap-emission", `${facts.divergences.length} divergences, ${divergenceClause(facts)}`],
    [ROADMAP_PATH, "roadmap-constraint", `${facts.constraint_vectors} \`class="constraint"\` enforcement reject vectors over ${facts.enforce_green_rows.length} enforce-green rows`],
    [MATRIX_README_PATH, "readme-counts", `${facts.features_total} features and ${facts.containment_cells} containment cells`],
    [MATRIX_README_PATH, "readme-annotations", `${facts.annotations_total} cddl-codegen support annotations`],
    [MATRIX_README_PATH, "readme-ops", `all ${facts.control_ops} IANA ops probed`],
    [MATRIX_README_PATH, "readme-enforce-green", `${facts.enforce_green_rows.length} rows`],
    [TESTS_README_PATH, "tests-ignored-gates", `the ${countWord(facts.ignored_gates.length)} \`#[ignore]\`d gates ${facts.ignored_gates.map((gate) => `\`${gate}\``).join(" / ")}`],
    [TESTS_README_PATH, "tests-tier-fast", humanTierWall(facts, "fast")],
    [TESTS_README_PATH, "tests-tier-local", humanTierWall(facts, "local")],
    [TESTS_README_PATH, "tests-tier-full", humanTierWall(facts, "full")],
  ];
  return Object.freeze(textRows.map(([path, slotId, value]) => Object.freeze({
    path,
    slot_id: slotId as SlotId,
    bytes: encoder.encode(value),
  })));
}

export function classifyLegacyStatusHeaderInvocation(
  argv: readonly string[],
): ClassifiedLegacyStatusInvocation["mode"] {
  if (argv.includes("--write")) return "write";
  if (argv.includes("--check")) return "check";
  return "report";
}

function reportBytes(facts: MatrixStatusFacts): Uint8Array {
  const lines = [
    "",
    "status-header counts (derived from matrix.json + catalog.toml + check.ts registry)",
    "",
    `  features:            ${facts.features_total}  (${profileSplit(facts)})`,
    `  containment cells:   ${facts.containment_cells}`,
    `  cddl-codegen annos:  ${facts.annotations_total}`,
    `  IANA control ops:    ${facts.control_ops}`,
    `  emission divergences:${facts.divergences.length}  (${divergenceClause(facts)})`,
    ...facts.divergences.map((value) => `      - ${value.id}  [${value.profile}]`),
    `  constraint vectors:  ${facts.constraint_vectors}  over ${facts.enforce_green_rows.length} enforce-green row(s)`,
    `      ${[...facts.enforce_green_rows].sort(codePointSort).join(", ")}`,
    `  manual #[ignore]d gates: ${facts.ignored_gates.length}  (${countWord(facts.ignored_gates.length)})`,
    ...facts.ignored_gates.map((gate) => `      - ${gate}`),
    "",
    "(run with --write to regenerate the spans, --check for the drift gate)",
    "",
  ];
  return encoder.encode(`${lines.join("\n")}\n`);
}

function checkSuccess(facts: MatrixStatusFacts): Uint8Array {
  return encoder.encode(
    `status-header count gate OK — ${facts.features_total} features (${profileSplit(facts)}) · ${facts.containment_cells} containment · ` +
    `${facts.annotations_total} annotations · ${facts.control_ops} IANA ops · ${facts.divergences.length} emission divergence(s) (${divergenceClause(facts)}) · ` +
    `${facts.constraint_vectors} constraint vectors over ${facts.enforce_green_rows.length} enforce-green rows · ` +
    `${facts.ignored_gates.length} manual #[ignore]d gates · all spans in sync\n`,
  );
}

function legacyPath(path: RepoPath): string {
  if (path === ROADMAP_PATH) return "ROADMAP.md";
  if (path === MATRIX_README_PATH) return "README.md";
  return "../tests/README.md";
}

function replacePayloads(
  snapshot: Uint8Array,
  replacements: readonly { start: number; end: number; bytes: Uint8Array }[],
): Uint8Array {
  let result = new Uint8Array(snapshot);
  for (const replacement of [...replacements].sort((left, right) => right.start - left.start)) {
    const next = new Uint8Array(result.byteLength - (replacement.end - replacement.start) + replacement.bytes.byteLength);
    next.set(result.subarray(0, replacement.start));
    next.set(replacement.bytes, replacement.start);
    next.set(result.subarray(replacement.end), replacement.start + replacement.bytes.byteLength);
    result = next;
  }
  return result;
}

/** Pure compatibility planner; it never reads or writes a target and report mode cannot carry one. */
export function planLegacyStatusHeaderRun(
  inputs: MatrixStatusInputs,
  invocation: ClassifiedLegacyStatusInvocation,
  observer?: { claimResolved(claim: ResolvedOutputClaim): void },
  outputStage: ProductionOutputStage = "pre_cutover",
): LegacyStatusHeaderRunPlan {
  const facts = deriveMatrixStatusFacts(inputs);
  if (invocation.mode === "report") {
    return Object.freeze({
      exit_code: 0,
      stdout: reportBytes(facts),
      stderr: new Uint8Array(),
      writes: Object.freeze([]),
    });
  }
  const outputInventory = productionOutputInventory(outputStage);
  const statusClaims = outputInventory.status_claims;
  const ownedSlots = new Set(statusClaims.map((claim) => JSON.stringify([claim.path, claim.slot_id])));
  const payloads = renderMatrixStatusPayloads(facts).filter((payload) =>
    ownedSlots.has(JSON.stringify([payload.path, payload.slot_id]))
  );
  const targetPaths = Object.freeze(TARGET_PATHS.filter((path) =>
    statusClaims.some((claim) => claim.path === path)
  ));
  const inspections = new Map<string, ReturnType<typeof inspectStatusMarkerBinding>>();
  const targetSnapshots = new Map<RepoPath, Uint8Array>();
  const missingTargets: RepoPath[] = [];
  for (const path of targetPaths) {
    const target = invocation.targets.get(path);
    if (target === undefined) missingTargets.push(path);
    else targetSnapshots.set(path, new Uint8Array(target));
  }
  if (missingTargets.length > 0) {
    const line = `FAIL ${legacyPath(missingTargets[0])}: target snapshot is missing\n`;
    return Object.freeze({
      exit_code: 1,
      stdout: invocation.mode === "check" ? encoder.encode(`status-header count gate: 1 problem(s)\n  ${line}`) : new Uint8Array(),
      stderr: invocation.mode === "write" ? encoder.encode(line) : new Uint8Array(),
      writes: Object.freeze([]),
    });
  }
  for (const payload of payloads) {
    const snapshot = targetSnapshots.get(payload.path) as Uint8Array;
    inspections.set(JSON.stringify([payload.path, payload.slot_id]), inspectStatusMarkerBinding(snapshot, payload.slot_id));
  }

  // Resolve the complete status-writer subset for this production output stage before
  // either mode can decide to write. The resolver is deliberately fed only the immutable target
  // snapshots above; callers cannot make a later claim observe different bytes.
  const resolution = resolveOutputClaims({
    registry: outputInventory.registry,
    claims: statusClaims,
    targets: targetSnapshots,
    observer: { claimResolved: (claim) => observer?.claimResolved(claim) },
  });

  if (invocation.mode === "write") {
    for (const payload of payloads) {
      const inspected = inspections.get(JSON.stringify([payload.path, payload.slot_id]));
      // Missing markers and a reversed single pair had a stable historical write diagnostic.
      // Duplicate/crossed/nested pairs did not have defined safe replacement semantics and use the
      // typed structural claim diagnostic below.
      if (inspected === undefined || inspected.open_count === 0 || inspected.close_count === 0) {
        const text = `FAIL ${legacyPath(payload.path)}: span '${payload.slot_id}' has no <!-- gen:sh:${payload.slot_id} --> … <!-- /gen:sh:${payload.slot_id} --> markers — hand-place them once around the phrase.\n`;
        return Object.freeze({
          exit_code: 1,
          stdout: new Uint8Array(),
          stderr: encoder.encode(text),
          writes: Object.freeze([]),
        });
      }
      if (
        inspected.open_count === 1 && inspected.close_count === 1 && !inspected.ordered &&
        inspected.open_offsets[0] > inspected.close_offsets[0]
      ) {
        const text = `FAIL ${legacyPath(payload.path)}: span '${payload.slot_id}' has no <!-- gen:sh:${payload.slot_id} --> … <!-- /gen:sh:${payload.slot_id} --> markers — hand-place them once around the phrase.\n`;
        return Object.freeze({
          exit_code: 1,
          stdout: new Uint8Array(),
          stderr: encoder.encode(text),
          writes: Object.freeze([]),
        });
      }
    }
    if (resolution.issues.length > 0) {
      const first = resolution.issues[0];
      return Object.freeze({
        exit_code: 1,
        stdout: new Uint8Array(),
        stderr: encoder.encode(`FAIL [${first.code}] ${first.source}#${first.logical_path}: ${first.message}\n`),
        writes: Object.freeze([]),
      });
    }
    const writes = targetPaths.map((path) => {
      const snapshot = targetSnapshots.get(path) as Uint8Array;
      const replacements = payloads.filter((payload) => payload.path === path).map((payload) => {
        const interval = inspections.get(JSON.stringify([path, payload.slot_id]))?.payload_interval;
        if (interval === undefined) throw new Error("internal: preflight accepted a missing interval");
        return { start: interval.start_byte, end: interval.end_byte, bytes: payload.bytes };
      });
      return Object.freeze({ path, bytes: replacePayloads(snapshot, replacements) });
    });
    return Object.freeze({
      exit_code: 0,
      stdout: encoder.encode(`status-headers: wrote ${statusClaims.length} generated span(s) across ${targetPaths.length} file(s).\n`),
      stderr: new Uint8Array(),
      writes: Object.freeze(writes),
    });
  }

  const problems = [...facts.validation_problems];
  for (const [payloadIndex, payload] of payloads.entries()) {
    const inspected = inspections.get(JSON.stringify([payload.path, payload.slot_id]));
    if (inspected === undefined) continue;
    if (inspected.open_count !== 1) {
      problems.push(`${legacyPath(payload.path)}: open marker for span '${payload.slot_id}' appears ${inspected.open_count} time(s), expected exactly 1`);
    }
    if (inspected.close_count !== 1) {
      problems.push(`${legacyPath(payload.path)}: close marker for span '${payload.slot_id}' appears ${inspected.close_count} time(s), expected exactly 1`);
    }
    if (inspected.open_count !== 1 || inspected.close_count !== 1) continue;
    const structural = resolution.issues.find((value) =>
      value.code === "E-OUTPUT-SLOT" && value.source === payload.path &&
      (
        value.logical_path === `slot[${JSON.stringify(payload.slot_id)}]` ||
        value.logical_path === `claims[${payloadIndex}]`
      )
    );
    if (structural !== undefined) {
      problems.push(`[${structural.code}] ${structural.source}#${structural.logical_path}: ${structural.message}`);
      continue;
    }
    if (!inspected.ordered || inspected.payload_interval === undefined) {
      problems.push(`${legacyPath(payload.path)}: markers for span '${payload.slot_id}' are reversed or crossed`);
      continue;
    }
    const snapshot = targetSnapshots.get(payload.path) as Uint8Array;
    const actual = snapshot.subarray(
      inspected.payload_interval.start_byte,
      inspected.payload_interval.end_byte,
    );
    if (!bytesEqual(actual, payload.bytes)) {
      problems.push(`${legacyPath(payload.path)}: span '${payload.slot_id}' is stale — has ${JSON.stringify(decoder.decode(actual))}, expected ${JSON.stringify(decoder.decode(payload.bytes))} (run \`bun run project_status_headers.ts --write\`)`);
    }
  }
  for (const outputIssue of resolution.issues.filter((value) => value.code === "E-OUTPUT-CLAIM")) {
    problems.push(`[${outputIssue.code}] ${outputIssue.source}#${outputIssue.logical_path}: ${outputIssue.message}`);
  }
  if (problems.length > 0) {
    return Object.freeze({
      exit_code: 1,
      stdout: encoder.encode(`status-header count gate: ${problems.length} problem(s)\n${problems.map((problem) => `  FAIL ${problem}`).join("\n")}\n`),
      stderr: new Uint8Array(),
      writes: Object.freeze([]),
    });
  }
  return Object.freeze({
    exit_code: 0,
    stdout: checkSuccess(facts),
    stderr: new Uint8Array(),
    writes: Object.freeze([]),
  });
}
