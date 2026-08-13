#!/usr/bin/env bun
/**
 * check.ts — the single entry point for "run everything that verifies this repo".
 *
 *   bun run check.ts            # `local` tier (default) — run before considering work done
 *   bun run check.ts fast       # what CI runs — the absolute-minimum commit gate
 *   bun run check.ts full       # `local` + every manual-only gate (the real "run all tests")
 *
 * CI (`.github/workflows/build.yml`) runs EXACTLY `bun run check.ts fast` — nothing else. The
 * registry below IS the encoding of every gate that verifies this repo; the tiers
 * (fast ⊂ local ⊂ full) are views over it. POLICY: the fast tier stays the absolute minimum (sole
 * maintainer; AI-velocity commits; CI minutes are the scarce resource) — new gates default to
 * `local` or `full`, and promoting anything into `fast` is a maintainer decision. Honest reporting
 * is the point: every run prints the FULL registry as a table (PASS / FAIL / SKIPPED / STUB /
 * not-in-tier + durations), so a gate that did not run is always *visibly* not-run — the failure
 * mode this runner cures (a gate that exists but is in nobody's habit) is impossible to miss.
 *
 * Flags:
 *   --keep-going    run all in-tier gates even after a failure (default: fail fast — later cargo
 *                   gates depend on the build, so stopping early avoids cascade noise).
 *   --skip-missing  downgrade a missing verify.ts oracle from FAIL to SKIPPED(oracle absent).
 *   --refresh-fuzz  re-run fuzz/generate.sh before the fuzz gates even if generated/ exists.
 *   --cache-transparency  enable the flag-gated `verify_cache_transparency` full-tier gate (two verify
 *                   runs — cached vs GATE_CACHE=0 — asserted byte-identical; otherwise SKIPPED).
 *   --only <a,b>    run ONLY these gates, in registry order, among the named tier's in-tier set
 *                   (`--only a,b` or `--only=a,b`). A selected run is NEVER a tier run: the full
 *                   registry still prints with the deselected gates as `NOT RUN (--only)`, the
 *                   SUMMARY header says PARTIAL, the self-log is `check-only-<stamp>.log` (outside
 *                   every tier median), and the last line is a receipt, not the tier verdict. Cite
 *                   it as "gates X, Y ran green" — never as a tier verdict. See "GATE SELECTION".
 *
 * CONCURRENCY: gates run one at a time UNLESS the registry says otherwise. A gate may declare a
 * `concurrent` group; gates in the same group, contiguous in the registry, run as one batch with at
 * most `CHECK_JOBS` (default 4) in flight, slowest-measured first. Everything else is a barrier, so
 * registry order still means what it says and the fmt→clippy→build→test chain stays strictly ordered.
 * Today exactly one group exists: the `#[ignore]`d manual-only heavy gates. A batched gate's output
 * is BUFFERED and emitted as one block on completion — interleaved cargo output is unreadable and
 * would mis-attribute the per-gate cache rollups the timings parser reads. Bound is memory, not
 * cores: `CHECK_JOBS=1` restores fully sequential execution.
 *
 * PEAK RESOURCE: what has to stay bounded is not the gate count but the PRODUCT
 * `(gates in flight) × (rustc per gate) × (per-rustc resident set)` — so each batched gate is handed
 * a memory-derived `CARGO_BUILD_JOBS` (`CHECK_CARGO_JOBS` overrides), and neither factor scales with
 * `nproc`. The sequential path is untouched. A `local`/`full` run also preflights free memory and
 * free scratch space up front: below the memory floor it degrades to sequential, below the disk floor
 * it refuses, because a tier commits to its peak in its first seconds and cannot discover a cap
 * mid-run. `CHECK_SKIP_PREFLIGHT=1` bypasses both floors; `fast` (CI) is untouched.
 *
 * LOGGING: every run tees its FULL output to a timestamped `draft/logs/check-<tier>-<stamp>.log`
 * (`check-only-<stamp>.log` for a `--only` run — a name outside the tier regex on purpose, see
 * "GATE SELECTION"; path printed at start and end) — evidence preservation is the tool's job, not a piping habit.
 * Never pipe a run through `tail`/`grep` as its only capture; cite the printed log path instead.
 *
 * FUZZING: the `full` tier walks the byte-fuzzer's two targets live (`fuzz_bounded_run`), bounded to
 * `FUZZ_BUDGET_S` seconds per target (default 120) and run one libFuzzer process at a time. It is a
 * smoke-walk of the reachable hostile-input surface, not the periodic deep run — that stays manual
 * (`fuzz/README.md`). Deliberately not gate-cached: a randomized exploration is not a pure function
 * of the tree's bytes.
 *
 * NETWORK: local/full runs start with a retried `cargo fetch` warm-up (workspace + fuzz +
 * tests/warmup dep-universe manifest), then force CARGO_NET_OFFLINE=true for every gate — nested-
 * cargo cells resolve from the cargo cache instead of hitting crates.io per cell, which removes
 * the registry-transient flake class outright (tests/README.md § "Offline-after-warmup").
 * CHECK_ONLINE=1 skips the offline forcing; a pre-set CARGO_NET_OFFLINE=true skips the fetch.
 * The fast tier (CI) is untouched.
 *
 * SELF-COMPLETENESS (the systematic catch, TDD): the first gate `self_checks` runs five meta-checks
 * so a new gate that nobody registers fails the run rather than silently not existing:
 *   1. ignored-test classification — every `#[ignore]` test must be registered here as either a
 *      manual gate (run it) or a known-failing stub (never run it, shown as STUB).
 *   2. matrix-script coverage    — every `cddl-matrix/*.ts` (minus lib.ts) must be wired to a gate.
 *   3. CI-is-fast-tier invariant — build.yml must invoke `bun run check.ts fast` and must contain
 *      NO other run step (all CI work flows through the registry's fast tier, so growing CI is an
 *      explicit, reviewed registry edit — not a workflow edit agents make in passing). It says
 *      nothing about the workflow's `paths:` trigger filter, which is not a run step: covering the
 *      trees a fast gate READS is a filter edit, and the promoted doc scanners depend on one.
 *   4. concurrency declarations are well-formed (`cmd`-only, group members contiguous).
 *   5. `requires:` edges are well-formed — the `--only` dependency fence's only enforcement.
 *
 * Meta-checks mutation-verified red-first at landing (repo idiom):
 *   - adding a throwaway `#[ignore]` test           -> meta-check 1 FAILED (unclassified ignore)
 *   - adding a throwaway `cddl-matrix/throwaway.ts`  -> meta-check 2 FAILED (script wired to no gate)
 *   - adding a direct `run: cargo test` step to build.yml -> meta-check 3 FAILED (bypasses registry)
 *   canaries reverted after confirming red.
 */
import {
  existsSync, mkdirSync, readFileSync, readdirSync, readlinkSync, rmSync, statSync,
  unlinkSync, writeFileSync,
} from "node:fs";
import { homedir, tmpdir } from "node:os";
import { basename, join, relative, resolve } from "node:path";
import {
  appendRows, cellCountsFor, compactDur, keptRunKeys, machineId, parseLog, readCells, readDigest,
  readLedger, runDigestUpdate, runKeysInOrder, splitLogName, tierWindow, trimCellLines, trimRows,
  upsert, writeLedger,
  KEEP_RUNS_IN_CELLS, type Digest, type GateRow, type Row, type RunRow,
} from "./cddl-matrix/project_timings.ts";
import { runNoStdCheckGate } from "./cddl-matrix/no_std_check.ts";

const ROOT = import.meta.dir;
const MATRIX = join(ROOT, "cddl-matrix");
const LOGS_DIR = join(ROOT, "draft", "logs");
const LEDGER = join(ROOT, "draft", "timings.jsonl");
const CELLS = join(ROOT, "draft", "timing-cells.jsonl");

// ---- tiers ---------------------------------------------------------------------------------------
const TIERS = ["fast", "local", "full"] as const;
export type Tier = (typeof TIERS)[number];
const rank = (t: Tier) => TIERS.indexOf(t);

// ---- argv ------------------------------------------------------------------------------------------
export interface ParsedArgv { flags: Set<string>; positional: string[]; only: string[] | null }

/**
 * argv -> (flags, positional, `--only` selection).
 *
 * argv is WALKED rather than partitioned by a `startsWith("--")` filter because `--only` is the one
 * flag that takes a value: in the space spelling (`--only a,b`) that value is not a flag, and the
 * filter would hand it to the tier positional — a selection would silently become "unknown tier".
 * Both spellings are accepted (`--only a,b` and `--only=a,b`), and ids may also be split across
 * repeated flags. Every argv consumer in this file goes through here, including the two `fn` gates
 * that read the tier off argv themselves, so there is exactly one place that knows this shape.
 */
export function parseArgv(argv: string[]): ParsedArgv {
  const flags = new Set<string>();
  const positional: string[] = [];
  let only: string[] | null = null;
  const add = (v: string): void => {
    only = [...(only ?? []), ...v.split(",").map(s => s.trim()).filter(Boolean)];
  };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i]!;
    if (a === "--only") {
      flags.add("--only");
      const v = argv[i + 1];
      // A valueless `--only` yields an EMPTY selection rather than swallowing the next flag; the
      // resolver rejects it by name, which is a better message than "unknown flag".
      if (v !== undefined && !v.startsWith("--")) { add(v); i++; } else only ??= [];
      continue;
    }
    if (a.startsWith("--only=")) { flags.add("--only"); add(a.slice("--only=".length)); continue; }
    if (a.startsWith("--")) { flags.add(a); continue; }
    positional.push(a);
  }
  return { flags, positional, only };
}

/** The tier a run is at, read off argv the way `main` reads it. */
export function tierFromArgv(argv: string[]): Tier {
  const p = parseArgv(argv).positional[0];
  return TIERS.includes(p as Tier) ? (p as Tier) : "local";
}

// ---- gate model ----------------------------------------------------------------------------------
// A FOURTH not-run flavour beside `NOT_IN_TIER`, `SKIPPED (earlier failure; fail-fast)` and
// `SKIPPED (reason)`: a gate the run's `--only` selection deliberately left out. Never a reuse of
// SKIPPED — a deliberate omission must not read as an incidental one — and never `not-in-tier`,
// which says the opposite (this gate WOULD have run in a complete run of this tier).
type Status = "PASS" | "FAIL" | "SKIPPED" | "STUB" | "NOT_IN_TIER" | "NOT_RUN_ONLY";
interface Outcome { status: Status; reason?: string }
interface Opts { skipMissing: boolean; refreshFuzz: boolean; cacheTransparency: boolean }
export interface Gate {
  id: string;
  tier: Tier;
  kind: "cmd" | "fn" | "stub";
  desc: string;
  cmd?: string[];            // kind === "cmd"
  cwd?: string;              // kind === "cmd"; defaults to ROOT
  run?: (o: Opts) => Outcome;// kind === "fn"
  ignoredTest?: string;      // maps this gate to a `#[ignore]` test (meta-check 1)
  script?: string;           // cddl-matrix/*.ts this gate drives (meta-check 2)
  /**
   * OPT-IN gate-level concurrency: this gate may run concurrently with the OTHER gates naming the
   * same group. Absent (the default for every gate) means sequential — today's behaviour, byte for
   * byte, including inherited stdout. The registry is the encoding of what verifies this repo, so
   * concurrency is declared here and is as visible as tier membership; nothing infers it.
   *
   * `cmd` gates only, and group members must be CONSECUTIVE in the registry (both enforced by
   * meta-check 4). Consecutiveness is what keeps registry order meaningful: an ungrouped gate is a
   * BARRIER, so `verify` still finishes before `verify_cache_transparency` starts and the
   * fmt→clippy→build→test chain stays strictly ordered. A member separated from its group by a
   * barrier would run alone — a declaration that silently does nothing, which is the failure class
   * meta-check 4 exists to make impossible.
   */
  concurrent?: string;
  /**
   * Gates this one READS THE OUTPUT OF, and therefore cannot be selected without (`--only`).
   *
   * The registry encodes execution ORDER and concurrency; a tier run masks data dependencies by
   * always running whole prefixes, so nothing had to state them until selection existed. A split
   * pair does not fail loudly — `coverage_md_diff` alone passes vacuously against a stale-but-
   * committed COVERAGE.md — which is why the refusal is hard (v1: refuse, never auto-include) and
   * why the `why` is carried here rather than in the error site: the message has to say what the
   * split would silently have asserted.
   *
   * Established by enumerating the registry, not by grep: the pairs are exactly the gates whose
   * verdict depends on a file or cache another gate WRITES in the same run. Everything else either
   * reads only committed files or owns its scratch root. Meta-check 5 keeps the field honest.
   */
  requires?: { gate: string; why: string }[];
}

// ---- process helpers -----------------------------------------------------------------------------
function sh(cmd: string[], cwd = ROOT, env?: Record<string, string>): number {
  const r = Bun.spawnSync(cmd, {
    cwd,
    env: env ? { ...process.env, ...env } : process.env,
    stdout: "inherit",
    stderr: "inherit",
    stdin: "inherit",
  });
  return r.exitCode ?? 1;
}

// ==================================================================================================
// GATE-LEVEL CONCURRENCY
// ==================================================================================================
// Why: measured, the heavy gates leave ~89 % of a 32-core box idle. Each is a single `#[test]`
// looping serially over catalog rows, spawning nested cargo; solo, the tier's second-largest gate
// runs at 16.1 % CPU — 5.1 of 32 cores. A controlled same-session A/B over
// `multifile_matrix_roundtrips` + `wasm_matrix_roundtrips` measured 603 s serial against 338 s
// 2-way parallel — 1.78×, 89 % of the 2× ideal, with only +6 % per-gate inflation.
//
// What bounds the win, and therefore the design: under perfect parallelism a tier's wall is bounded
// by its LONGEST gate, and this set is badly skewed (~636 s down to ~10 s). "Sum ÷ jobs" is not
// achievable, so the pool dispatches LONGEST-FIRST — a short gate scheduled ahead of the longest one
// adds its whole duration to the tail.
//
// What bounds the DEGREE is memory, not cores: concurrent `rustc` is the memory-hungry part and only
// ~6 of 32 GiB is free on the development box in practice. Hence a small default, overridable.
const DEFAULT_JOBS = 4;

/** `CHECK_JOBS=1` restores fully sequential execution — the pool with degree 1 IS the old loop. */
export function parseJobs(raw: string | undefined): { jobs: number; warning?: string } {
  if (raw === undefined || raw.trim() === "") return { jobs: DEFAULT_JOBS };
  const n = Number(raw.trim());
  if (!Number.isInteger(n) || n < 1)
    return { jobs: DEFAULT_JOBS, warning: `CHECK_JOBS='${raw}' is not a positive integer — using ${DEFAULT_JOBS}` };
  return { jobs: n };
}

// ---- the SECOND factor: cargo's own `-j` inside each batched gate --------------------------------
// `CHECK_JOBS` bounds how many GATES overlap. It does not bound how many `rustc` each one spawns, and
// a nested cargo defaults to `-j $(nproc)`. So the quantity that actually consumes memory —
//
//     (gates in flight) × (rustc per gate) × (per-rustc resident set)
//
// — was bounded by nothing, and its second factor scaled with CORE COUNT, which is unrelated to the
// machine's memory. That is the defect this bound closes: a WSL2 box with a 32 GiB cap and 32 cores
// went unresponsive for ~10 minutes under a full tier and had to be power-cycled.
//
// Hence a bound derived from MEMORY, not cores, written into each batched child's `CARGO_BUILD_JOBS`:
//
//     product      = round(MemTotal × RUSTC_MEM_FRACTION ÷ ASSUMED_PEAK_RUSTC_GIB)
//     per-gate -j  = max(1, floor(product ÷ gates in flight))
//
// On a 32 GiB machine that is 8 rustc across the whole batch — `-j2` at the default 4 gates in
// flight — so 8 × 2 GiB = 16 GiB worst case against a 32 GiB cap. The half-of-MemTotal fraction is
// the headroom the arithmetic ignores: page cache (a tier writes tens of GiB of scratch), the parent
// `bun`/`cargo test` processes, an editor's rust-analyzer, and a concurrent session's own gates.
// `round`, not `floor`, because MemTotal reads a little under the machine's nominal cap (31.3 GiB on
// a 32 GiB WSL2 box — the kernel reserves the rest) and flooring would make a 32 GiB machine behave
// like a 28 GiB one; the assumed footprint below carries a 4× margin, so a rounding step at the
// boundary is inside the estimate's own error bar.
//
// Measured on this box (32 GiB cap, `nproc` 32, sampled at 1 s over a 4-gate batch of
// `recombination_wasm_crates_check` + `recombination_crates_execute` +
// `identifier_hazard_crates_compile` + `rust_oracle_fingerprint`, one warm regime):
//
//     CARGO_BUILD_JOBS |  peak rustc | peak Σ rustc RSS | batch wall
//     -----------------+-------------+------------------+-----------
//     32 (unbounded)   |          27 |         2.25 GiB |    63.6 s
//      2 (the default) |           3 |         0.53 GiB |    60.3 s
//      1               |           2 |         0.37 GiB |    60.0 s
//
// Two things that table settles. The bound is close to FREE at the batch level: these gates are
// internally serial loops over catalog rows, so the win came from overlapping GATES, never from
// cargo's own `-j` — only `rust_oracle_fingerprint`, the one member that is a single crate compile,
// slows (17.4 s → 34.0 s at `-j2`), and it is not the batch's tail. And process count is the wrong
// thing to reason about anyway: the largest single rustc resident set seen anywhere — across this
// batch and a whole `local` tier — was **455 MiB**, so bounding COUNT alone would still admit a very
// different peak if a gate ever compiles one large crate instead of many small ones.
/**
 * Assumed worst-case resident set of ONE concurrency slot, in GiB.
 *
 * Named for `rustc` because that is what `CARGO_BUILD_JOBS` divides, but what it must actually cover
 * is the peak of everything one slot holds at once — and compilation is only the first half of that.
 * A nested-cargo gate compiles, then RUNS the binaries it built: the full tier's emitted-test crates
 * carry thousands of `#[test]` functions each, and a test process's own resident set is not sampled
 * anywhere in this file. Treating the constant as "one rustc" measured the cheaper half and budgeted
 * as though it were the whole.
 *
 * Raised from 2 after two whole-machine freezes (~1 h and ~1.5 h, 100% memory and swap, sustained
 * thrashing) under full tiers that every memory check passed — the largest single rustc ever observed
 * here is 455 MiB, so 8 of those is ~3.6 GiB and cannot explain a 31 GiB box going down. The old
 * value was not a small margin over the wrong quantity; it was a comfortable margin over a quantity
 * that was never the binding one. Until a slot's true peak is measured (see `tests/testing-roadmap.toml`),
 * this is deliberately pessimistic: the cost of being too low is a slower tier, and the cost of being
 * too high is a machine that stops responding for an hour and takes every other session with it.
 */
const ASSUMED_PEAK_RUSTC_GIB = 4;
/**
 * Fraction of AVAILABLE memory the batch may commit to concurrent `rustc`; the rest is headroom.
 *
 * Of AVAILABLE, not MemTotal — the distinction is the whole point. MemTotal answers "how big is this
 * machine", which is not the question: the batch cannot use memory another process already holds. A
 * developer box runs an editor's language servers, other agent sessions, and whatever else, so a
 * budget struck against MemTotal silently assumes it owns a half of the machine that is already
 * spoken for. Committing that half anyway is how a tier drives the box into swap, and a thrashing
 * machine does not fail a gate — it stops responding, so nothing in this runner ever observes it.
 * Budgeting against `MemAvailable` makes the bound shrink exactly when the machine is busy, which is
 * when it must.
 */
const RUSTC_MEM_FRACTION = 0.5;
/** Product used when MemTotal is unreadable (non-Linux): the value this machine's memory derives. */
const FALLBACK_RUSTC_PRODUCT = 8;

/** MemTotal in GiB, or `undefined` where `/proc/meminfo` does not exist. Injectable for tests. */
export function memTotalGiB(meminfo?: string): number | undefined {
  try {
    const txt = meminfo ?? readFileSync("/proc/meminfo", "utf8");
    const m = txt.match(/^MemTotal:\s+(\d+)\s*kB$/m);
    return m ? Number(m[1]) / 1024 / 1024 : undefined;
  } catch { return undefined; }
}

/**
 * `CARGO_BUILD_JOBS` for one batched gate, given how many gates share the machine with it.
 *
 * Pure, so the arithmetic is pinned by a test rather than observed only in a full-tier run.
 *
 * Precedence, and why each rung is where it is:
 *  1. `CHECK_CARGO_JOBS` — an explicit operator override wins outright. A 128 GiB box should be able
 *     to raise this, and a sequential-minded operator should be able to pin it to 1.
 *  2. otherwise the memory-derived product above, and then
 *  3. **never above an inherited `CARGO_BUILD_JOBS`** — a `min`, not a replace. Someone who exported
 *     `CARGO_BUILD_JOBS=2` to be gentle to their machine said something the runner must not undo;
 *     someone who exported `CARGO_BUILD_JOBS=32` did not know about the batch, so the bound applies.
 */
export function cargoJobsForBatch(o: {
  gatesInFlight: number;
  memTotalGiB?: number;
  /**
   * `MemAvailable` at the moment this batch starts — the preferred basis, and measured PER BATCH
   * rather than once at startup, because a tier's later batches begin under whatever the earlier
   * ones (and every other process on the box) left behind.
   */
  memAvailGiB?: number;
  override?: string;   // CHECK_CARGO_JOBS
  inherited?: string;  // CARGO_BUILD_JOBS already present in the environment
}): { jobs: number; why: string; warning?: string } {
  let warning: string | undefined;
  const asPositiveInt = (raw: string | undefined, name: string): number | undefined => {
    if (raw === undefined || raw.trim() === "") return undefined;
    const n = Number(raw.trim());
    if (Number.isInteger(n) && n >= 1) return n;
    warning = `${name}='${raw}' is not a positive integer — ignoring it`;
    return undefined;
  };
  const override = asPositiveInt(o.override, "CHECK_CARGO_JOBS");
  if (override !== undefined)
    return { jobs: override, why: `CHECK_CARGO_JOBS=${override} (operator override)`, ...(warning ? { warning } : {}) };

  // `MemAvailable` first, `MemTotal` only as a fallback for a machine that cannot report it. Both
  // are floored, never rounded: `Math.round` rounds a 3.5-slot budget UP to 4, spending headroom the
  // fraction exists to reserve, and it is the wrong direction to be wrong in.
  const basisGiB = o.memAvailGiB ?? o.memTotalGiB;
  const basisName = o.memAvailGiB !== undefined ? "MemAvailable" : "MemTotal";
  const product = basisGiB === undefined
    ? FALLBACK_RUSTC_PRODUCT
    : Math.max(1, Math.floor((basisGiB * RUSTC_MEM_FRACTION) / ASSUMED_PEAK_RUSTC_GIB));
  const derived = Math.max(1, Math.floor(product / Math.max(1, o.gatesInFlight)));
  const memWhy = basisGiB === undefined
    ? `memory unreadable — fallback product ${product}`
    : `${basisGiB.toFixed(1)} GiB ${basisName} × ${RUSTC_MEM_FRACTION} ÷ ${ASSUMED_PEAK_RUSTC_GIB} GiB/slot = ${product} slot(s)`;

  const inherited = asPositiveInt(o.inherited, "CARGO_BUILD_JOBS");
  if (inherited !== undefined && inherited < derived)
    return {
      jobs: inherited,
      why: `${memWhy} ÷ ${o.gatesInFlight} gates = -j${derived}, held down to the inherited CARGO_BUILD_JOBS=${inherited}`,
      ...(warning ? { warning } : {}),
    };
  return {
    jobs: derived,
    why: `${memWhy} ÷ ${o.gatesInFlight} gates = -j${derived}`,
    ...(warning ? { warning } : {}),
  };
}

/**
 * Runaway-hang guard on the JOIN, not a duration assertion on any gate.
 *
 * Nothing here may fail a gate on a number — durations are nondeterministic and this delivery adds
 * no test that fails on one. What this bounds is the pool's own liveness: if the join ever fails to
 * settle after its work is done, the runner must say so loudly and RETURN rather than hang. That
 * failure mode is not hypothetical — two ad-hoc probe scripts written while gathering the evidence
 * for this feature backgrounded a sampler alongside their gates and then used a bare `wait`, which
 * waits for EVERY background job including the never-exiting sampler. Both ran to completion
 * internally — every gate exit-0, every verdict on disk — and then hung without emitting a summary;
 * one went unnoticed for ~5 hours. The default is ~5× the slowest gate on record, so it cannot fire
 * on slowness; if it fires, the runner has a bug and the diagnostic names the gates still in flight.
 */
const DEFAULT_JOIN_TIMEOUT_MS = 3 * 60 * 60 * 1000;

export function parseJoinTimeoutMs(raw: string | undefined): number {
  if (raw === undefined || raw.trim() === "") return DEFAULT_JOIN_TIMEOUT_MS;
  const n = Number(raw.trim());
  return Number.isFinite(n) && n > 0 ? n * 1000 : DEFAULT_JOIN_TIMEOUT_MS;
}

// ---- the THIRD factor: how many nested tool children a gate runs AT ONCE -------------------------
// `CARGO_BUILD_JOBS` bounds how many `rustc` each nested cargo spawns; it says nothing about how
// many nested cargos a gate holds open concurrently. For a `cargo test` gate that count is the
// libtest thread count — `nproc` by default — so the true peak was
// `test threads × CARGO_BUILD_JOBS` compilers plus a spawned test binary per thread, a product the
// slot arithmetic above never modeled. The bound lives in the test helper every nested spawn goes
// through (`tool_cmd`, src/tests/integration_tests.rs), which reads `CDDL_NESTED_TOOL_PERMITS`;
// this helper is the runner's side of it.
//
// The derived value is ONE child per gate, and that is not timidity but the only reading under
// which the slot model is honest: a gate's whole `CARGO_BUILD_JOBS` share is each nested child's
// internal `-j`, so at N children the gate spends N × share slots — permits and jobs multiply, and
// any pair that both track the share overshoots the budget quadratically. One child at `-j share`
// is exactly the "compile then run" slot `ASSUMED_PEAK_RUSTC_GIB` prices. The wall-time this
// leaves on the table is real and deliberately unspent until the sampler below can price it
// (raising permits is safe exactly when measured child peaks say so, and `CHECK_NESTED_PERMITS`
// is the operator's override meanwhile).
export function nestedToolPermitsForGate(o: {
  override?: string;   // CHECK_NESTED_PERMITS
  inherited?: string;  // CDDL_NESTED_TOOL_PERMITS already present in the environment
}): { permits: number; why: string; warning?: string } {
  let warning: string | undefined;
  const asPositiveInt = (raw: string | undefined, name: string): number | undefined => {
    if (raw === undefined || raw.trim() === "") return undefined;
    const n = Number(raw.trim());
    if (Number.isInteger(n) && n >= 1) return n;
    warning = `${name}='${raw}' is not a positive integer — ignoring it`;
    return undefined;
  };
  const override = asPositiveInt(o.override, "CHECK_NESTED_PERMITS");
  if (override !== undefined)
    return { permits: override, why: `CHECK_NESTED_PERMITS=${override} (operator override)`, ...(warning ? { warning } : {}) };
  const derived = 1;
  const inherited = asPositiveInt(o.inherited, "CDDL_NESTED_TOOL_PERMITS");
  if (inherited !== undefined && inherited < derived)
    return {
      permits: inherited,
      why: `1 nested child per gate, held down to the inherited CDDL_NESTED_TOOL_PERMITS=${inherited}`,
      ...(warning ? { warning } : {}),
    };
  return { permits: derived, why: "1 nested child per gate (its whole -j share is that child's)", ...(warning ? { warning } : {}) };
}

// ---- per-run memory sampler (report-only, asserted by NOTHING) -----------------------------------
// The slot arithmetic above budgets an ASSUMED per-slot footprint because no measurement of the
// real one existed: no gate ever sampled concurrent `rustc`, Σ RSS of the run's own process tree
// (test processes included), or how low `MemAvailable` actually went. This sampler is that
// measurement. It reports and records; it never fails anything — peaks and floors are
// nondeterministic, and a gate that fails on a number would be flaky by construction. What the
// numbers buy is the ability to replace pessimistic constants (the 4 GiB slot, the one-permit
// nested bound) with measured ones, and to split the NEXT whole-machine incident into "the memory
// bound was wrong again" vs "memory was healthy, something else saturated".
export interface ProcStat { pid: number; comm: string; ppid: number; rssBytes: number }

/**
 * One `/proc/<pid>/stat` line. The comm field is parenthesized and may itself contain spaces or
 * parens (`(tokio-runtime-w)`, `(a) weird (name)`), so the split point is the LAST `)` — fields
 * after it are whitespace-separated with state at index 0, ppid at 1, rss (pages) at 21.
 */
export function parseProcStat(pid: number, line: string, pageBytes: number): ProcStat | undefined {
  const open = line.indexOf("(");
  const close = line.lastIndexOf(")");
  if (open < 0 || close < open) return undefined;
  const rest = line.slice(close + 1).trim().split(/\s+/);
  const ppid = Number(rest[1]);
  const rssPages = Number(rest[21]);
  if (!Number.isInteger(ppid) || !Number.isFinite(rssPages)) return undefined;
  return { pid, comm: line.slice(open + 1, close), ppid, rssBytes: rssPages * pageBytes };
}

/** Transitive children of `root` (root itself excluded — the runner measures what it SPAWNED). */
export function descendantsOf(root: number, procs: ProcStat[]): ProcStat[] {
  const kids = new Map<number, ProcStat[]>();
  for (const p of procs) {
    const a = kids.get(p.ppid);
    if (a) a.push(p); else kids.set(p.ppid, [p]);
  }
  const out: ProcStat[] = [];
  const stack = [root];
  while (stack.length) {
    for (const c of kids.get(stack.pop()!) ?? []) {
      out.push(c);
      stack.push(c.pid);
    }
  }
  return out;
}

export interface MemPeaks {
  ticks: number;
  readErrors: number;
  /** Peak Σ RSS across the run's descendant tree, and the tree's shape at that tick. */
  peakTreeGiB: number;
  peakTreeProcs: number;
  /** Peak count of concurrent `rustc` processes in the tree (its own tick, not the peak-RSS one). */
  peakRustc: number;
  /** Largest single process ever seen in the tree. */
  maxSingleGiB: number;
  maxSingleComm: string;
  /** Machine-wide MemAvailable floor over the run — the number the budget's basis dips to. */
  memAvailFloorGiB?: number;
}

function startMemSampler(intervalMs = 1000): { stop: () => MemPeaks } {
  const peaks: MemPeaks = {
    ticks: 0, readErrors: 0, peakTreeGiB: 0, peakTreeProcs: 0, peakRustc: 0,
    maxSingleGiB: 0, maxSingleComm: "-",
  };
  // Page size once: x86-64 and most aarch64 kernels use 4096, but 16k/64k-page arm64 kernels
  // exist, and rss in /proc is in PAGES.
  let pageBytes = 4096;
  try {
    const out = Bun.spawnSync(["getconf", "PAGESIZE"]).stdout.toString().trim();
    const n = Number(out);
    if (Number.isInteger(n) && n > 0) pageBytes = n;
  } catch { /* keep the default */ }
  const tick = (): void => {
    try {
      const procs: ProcStat[] = [];
      for (const entry of readdirSync("/proc")) {
        if (!/^\d+$/.test(entry)) continue;
        try {
          const p = parseProcStat(Number(entry), readFileSync(`/proc/${entry}/stat`, "utf8"), pageBytes);
          if (p) procs.push(p);
        } catch { /* the process exited between readdir and read — normal churn, not an error */ }
      }
      const tree = descendantsOf(process.pid, procs);
      let sum = 0;
      let rustc = 0;
      for (const p of tree) {
        sum += p.rssBytes;
        if (p.comm === "rustc") rustc++;
        if (p.rssBytes / 2 ** 30 > peaks.maxSingleGiB) {
          peaks.maxSingleGiB = p.rssBytes / 2 ** 30;
          peaks.maxSingleComm = p.comm;
        }
      }
      const sumGiB = sum / 2 ** 30;
      if (sumGiB > peaks.peakTreeGiB) {
        peaks.peakTreeGiB = sumGiB;
        peaks.peakTreeProcs = tree.length;
      }
      if (rustc > peaks.peakRustc) peaks.peakRustc = rustc;
      const avail = availGiB("mem");
      if (avail !== undefined && (peaks.memAvailFloorGiB === undefined || avail < peaks.memAvailFloorGiB))
        peaks.memAvailFloorGiB = avail;
      peaks.ticks++;
    } catch {
      peaks.readErrors++;
    }
  };
  const timer = setInterval(tick, intervalMs);
  // Belt to the stop()'s braces: an unref'd timer can never hold the event loop open, so even a
  // path that misses stop() cannot recreate the hang-after-success class the join guard exists for.
  (timer as unknown as { unref?: () => void }).unref?.();
  return {
    stop: () => {
      clearInterval(timer);
      tick(); // one final sample, so a run shorter than the interval still reports something
      return peaks;
    },
  };
}

/** The sampler's end-of-run report: a printed block, and a row in the gitignored local ledger. */
function reportMemPeaks(peaks: MemPeaks, tier: Tier): void {
  if (peaks.ticks === 0) {
    console.log(`\nmemory sampler: no samples (${peaks.readErrors} read error(s) — no /proc on this platform?)`);
    return;
  }
  const gib = (n: number | undefined): string => n === undefined ? "?" : n.toFixed(2);
  console.log(
    `\nmemory sampler (report-only, 1 s ticks × ${peaks.ticks}): ` +
    `peak run Σ RSS ${gib(peaks.peakTreeGiB)} GiB over ${peaks.peakTreeProcs} proc(s); ` +
    `peak concurrent rustc ${peaks.peakRustc}; ` +
    `largest single process ${gib(peaks.maxSingleGiB)} GiB (${peaks.maxSingleComm}); ` +
    `machine MemAvailable floor ${gib(peaks.memAvailFloorGiB)} GiB` +
    (peaks.readErrors ? `; ${peaks.readErrors} read error(s)` : ""),
  );
  try {
    mkdirSync(join(ROOT, "draft"), { recursive: true });
    const ledger = join(ROOT, "draft", "memory-peaks.jsonl");
    // Bounded like the other draft ledgers, but self-contained: keep the last N runs at append
    // time rather than joining the log-keyed retention pass — peaks rows carry no cross-file keys,
    // and 200 runs is months of history at one row per run, plenty to re-derive a constant from.
    const rows = existsSync(ledger) ? readFileSync(ledger, "utf8").split("\n").filter(l => l.trim()) : [];
    rows.push(JSON.stringify({ stamp: new Date().toISOString(), tier, ...peaks }));
    writeFileSync(ledger, rows.slice(-200).join("\n") + "\n");
  } catch (e) {
    console.log("memory sampler: ledger append failed (non-fatal — peaks are never a gate): " +
      (e instanceof Error ? e.message : String(e)));
  }
}

/** One unit of parallel work plus the handle the timeout guard would need to reclaim it. */
export interface PoolItem { id: string }

/**
 * Bounded-concurrency pool. **The join awaits exactly the worker promises and nothing else.**
 *
 * `degree` workers pull from one cursor over `items`, so the pool is bounded by construction rather
 * than by counting live promises. `stopAfter` stops the pull WITHOUT cancelling anything already in
 * flight: that is fail-fast's meaning here — no NEW gate starts after a failure, gates already
 * running finish and report their real verdicts, and everything never started is reported as
 * never-run. Cancelling in-flight gates was rejected: it throws away minutes of completed work, and
 * killing a nested cargo/rustc tree is the operation this repo has already been bitten by
 * (a pattern-matched `pkill` took out a concurrent session's live run).
 *
 * The timeout races the join against a TIMER — a promise that cannot itself fail to settle — and the
 * timer is always cleared, because an outstanding `setTimeout` keeps Bun's event loop alive and
 * would reproduce the hang-after-success mode through a different door. `onTimeout` gets the items
 * still in flight so the caller can reclaim them by their OWN handles (never by name pattern).
 */
export async function runPool<T extends PoolItem, R>(
  items: T[],
  degree: number,
  work: (item: T) => Promise<R>,
  o: { timeoutMs?: number; stopAfter?: (r: R, item: T) => boolean; onTimeout?: (inFlight: T[]) => void } = {},
): Promise<Map<string, R>> {
  const results = new Map<string, R>();
  const inFlight = new Set<T>();
  let next = 0;
  let stopped = false;
  const worker = async (): Promise<void> => {
    for (;;) {
      if (stopped || next >= items.length) return;
      const item = items[next++]!;
      inFlight.add(item);
      try {
        const r = await work(item);
        results.set(item.id, r);
        if (o.stopAfter?.(r, item)) stopped = true;
      } finally {
        inFlight.delete(item);
      }
    }
  };
  const width = Math.max(1, Math.min(degree, items.length));
  const joined = Promise.all(Array.from({ length: width }, () => worker())).then(() => "done" as const);
  if (!o.timeoutMs) { await joined; return results; }

  let timer: ReturnType<typeof setTimeout> | undefined;
  const guard = new Promise<"timeout">(res => { timer = setTimeout(() => res("timeout"), o.timeoutMs); });
  try {
    if (await Promise.race([joined, guard]) === "timeout") {
      stopped = true;
      o.onTimeout?.([...inFlight]);
    }
  } finally {
    if (timer !== undefined) clearTimeout(timer);
  }
  return results;
}

/**
 * Registry order, grouped into units of execution: a `concurrent` gate joins the batch immediately
 * before it when that batch carries the same group, and everything else is a batch of one.
 *
 * Pure, and takes the already-tier-filtered non-stub gate list, so the batching rule is pinned by a
 * test rather than observed only in a full-tier run.
 */
export interface Batch { group?: string; gates: Gate[] }

export function planBatches(gates: Gate[]): Batch[] {
  const out: Batch[] = [];
  for (const g of gates) {
    const prev = out[out.length - 1];
    if (g.concurrent !== undefined && prev !== undefined && prev.group === g.concurrent) prev.gates.push(g);
    else out.push({ ...(g.concurrent !== undefined ? { group: g.concurrent } : {}), gates: [g] });
  }
  return out;
}

/**
 * Dispatch order within a batch: slowest measured gate first, registry order as the tiebreak.
 *
 * A HINT, never an assertion — `tests/timings.json` rows are contended and re-baselining, and a gate
 * with no measurement yet simply sorts last. Getting the order wrong costs wall time and nothing
 * else. It matters because the tail is what a skewed batch is bounded by: dispatching the ~636 s
 * gate last would add nearly its whole duration after everything else had drained.
 */
export function longestFirst(gates: Gate[], hint: (id: string) => number | undefined): Gate[] {
  return gates
    .map((g, i) => ({ g, i }))
    .sort((a, b) => (hint(b.g.id) ?? 0) - (hint(a.g.id) ?? 0) || a.i - b.i)
    .map(x => x.g);
}

/** Measured warm durations from the committed digest, as the scheduler's ordering hint. */
function digestHint(): (id: string) => number | undefined {
  try {
    const byId = new Map(readDigest().gates.map(g => [g.gate, g.warm_ms]));
    return id => byId.get(id);
  } catch {
    return () => undefined; // no digest: registry order, which is what the sort falls back to
  }
}

// ---- registry warm-up: fetch once online, then force every gate offline (local/full only) --------
// Each nested-cargo cell (compile matrices, suite wasm legs, replay gates) resolves a fresh temp
// crate against crates.io, so a flaky network/proxy kills otherwise-green runs at a random cell —
// and cargo's built-in transient retry never engages on the proxy-CONNECT-abort flavor (zero
// `spurious network error` lines across full-tier logs that died this way). The fix is removing
// the per-cell network dependency, not retrying it: one retried `cargo fetch` (workspace + fuzz +
// the tests/warmup dep-universe manifest, drift-gated by `warmup_manifest_covers_registry_dep_universe`),
// then CARGO_NET_OFFLINE=true — the env propagates through `cargo test` → nested Command spawns and
// the cddl-matrix scripts, so no cell touches the network. The retry here is honest: a pure fetch
// with no assertions behind it.
function warmupThenOffline(tier: Tier) {
  if (rank(tier) < rank("local")) return; // fast tier is CI: stays online, zero added cost
  if (process.env.CHECK_ONLINE === "1") {
    console.log("warm-up: CHECK_ONLINE=1 — staying online (registry transients possible)");
    return;
  }
  if (process.env.CARGO_NET_OFFLINE === "true") {
    console.log("warm-up: CARGO_NET_OFFLINE already set — skipping fetch, cargo cache assumed warm");
    return;
  }
  const fetches: string[][] = [
    ["cargo", "fetch", "--locked"], // matches the build/clippy gates' --locked
    ["cargo", "fetch", "--manifest-path", "fuzz/Cargo.toml"],
    ["cargo", "fetch", "--manifest-path", "tests/warmup/Cargo.toml"],
  ];
  const ATTEMPTS = 3;
  for (let attempt = 1; ; attempt++) {
    console.log(`warm-up: cargo fetch (workspace + fuzz + dep universe), attempt ${attempt}/${ATTEMPTS}`);
    if (fetches.every(cmd => sh(cmd) === 0)) break;
    if (attempt === ATTEMPTS) {
      console.error(
        "check.ts: warm-up fetch failed after 3 attempts — one online fetch is required to populate " +
        "the cargo cache before gates run offline (CHECK_ONLINE=1 forces the old online behavior)",
      );
      process.exit(2);
    }
  }
  process.env.CARGO_NET_OFFLINE = "true";
  console.log("warm-up: fetched — CARGO_NET_OFFLINE=true for all gates");
}

// ---- run-start scratch sweep: leaked nested-cargo scratch retires itself -------------------------
/**
 * Every nested-cargo gate and in-process test suite mints per-run scratch under `tmpdir()` and relies
 * on end-of-run cleanup that a KILLED or crashed run never reaches, so debris accumulates silently
 * across sessions. Measured once at machine-killing scale: 3316 leaked entries totalling 43 GB filled
 * the root filesystem to 0 bytes free, which killed one session's full tier mid-gate, left a
 * half-written `cddl-matrix/annotations` overlay that failed the NEXT run's `build_matrix_check`, and
 * broke harness task-output plumbing for every concurrent session. So scratch retires itself here,
 * under three bounds and no fourth.
 *
 * **An explicit prefix registry, never a glob.** `tmpdir()` is shared with everything else on the
 * machine, and a `cddl*` glob that matched one unrelated directory would be a data-loss bug reported
 * as a test-runner bug. Every member of `SCRATCH_PREFIXES` is a literal leading substring of a name
 * THIS REPO mints, derived by walking the `temp_dir()`/`tmpdir()` join sites in `src/`, `check.ts`
 * and `cddl-matrix/` — an enumeration of the mint sites, not a grep for a keyword. A new mint site
 * outside these prefixes leaks (the failure direction is debris accumulating, never a stranger's
 * directory deleted), so add its prefix here when you add it.
 *
 * **Age, never emptiness or size.** The longest tier measured runs well under an hour, so a
 * 24-hour-old entry cannot belong to a live run of anything; and the hand remediation the incident
 * needed used exactly this rule. Age is the NEWEST mtime among an entry and its immediate children,
 * because a long-lived `CARGO_TARGET_DIR` root keeps its own mtime while cargo rewrites the files
 * inside it — reading the root alone would retire a target dir that is amortising builds every day.
 *
 * **A live-process guard.** AGENTS.md's pattern-kill rule — "another session's live run matches the
 * same substring" — binds deletion at least as hard as it binds killing, and a scratch root reaches
 * its gate as often through `CARGO_TARGET_DIR` as through argv. So the guard reads `cmdline`,
 * `environ` and `cwd` for every process, and if it cannot read `/proc` at all the sweep does not run:
 * a guard that fails open is not a guard.
 *
 * `.lock` files are never swept. They are `acquire_scratch_lock`'s sibling flocks, they hold no
 * bytes, and unlinking one a live run holds lets a third run acquire a fresh inode while the first
 * still owns the old one — the precise race the lock's own placement comment exists to prevent.
 */
export const SCRATCH_PREFIXES: readonly string[] = [
  "cddl_codegen_",       // src/tests/** (the bulk, incl. every `scratch_name` root), src/api.rs,
                         // cddl-matrix/no_std_check.ts
  "cddl_config_",        // src/tests/config_tests.rs
  "cddl_verify_",        // cddl-matrix/verify.ts — the probe root plus the ~2 GiB compile/wasm targets
  "cddl_verbosity_",     // src/tests/integration_tests.rs
  "cddl_wr_",            // src/tests/integration_tests.rs (wrapper-request sidecar fixtures)
  "cddl_json_gen_dep_",  // src/tests/integration_tests.rs
  "cddl_timing_cells_",  // src/tests/timing_cells.rs
  "cddl_cm_feat_",       // src/cargo_manifest.rs
  "cddl-retention-",     // cddl-matrix/project_timings.ts (retainLogs' own fixtures)
  "gate_cache_audit_",   // cddl-matrix/audit_gate_cache_closure.ts (scratch + forced-miss cache dir)
  "cache_transparency_", // cddl-matrix/cache_transparency.ts
  "no-silent-dir-",      // cddl-matrix/no_silent_directive.ts
  "cddl-pin-cold-fetch-",// cddl-matrix/pin_cold_fetch.ts (scratch bare repo for the SHA probes)
  "cddl-roadmap-selftest-",     // cddl-matrix/roadmap/io.ts (hermetic SHA-1/SHA-256 Git fixtures)
];

/** 24 h: >40× the longest measured tier, so no live run's scratch can reach it. */
export const SCRATCH_MAX_AGE_MS = 24 * 60 * 60 * 1000;

export interface ScratchSweep {
  status: "ran" | "skipped";
  reason?: string;
  removed: string[];
  bytes: number;
  keptFresh: number;
  keptLive: string[];
}

/** Newest mtime among an entry and its immediate children — see the age bound above. */
function newestMtimeMs(path: string): number {
  let newest = 0;
  try { newest = statSync(path).mtimeMs; } catch { return Date.now(); }
  try {
    for (const child of readdirSync(path)) {
      try { newest = Math.max(newest, statSync(join(path, child)).mtimeMs); } catch { /* raced away */ }
    }
  } catch { /* a file, or unreadable: its own mtime stands */ }
  return newest;
}

/** Names appearing in any live process's argv, environment or cwd; `null` if `/proc` is unreadable. */
export function liveScratchNames(procRoot = "/proc"): Set<string> | null {
  let pids: string[];
  try { pids = readdirSync(procRoot).filter(d => /^\d+$/.test(d)); } catch { return null; }
  const blobs: string[] = [];
  for (const pid of pids) {
    for (const f of ["cmdline", "environ"]) {
      try { blobs.push(readFileSync(join(procRoot, pid, f), "utf8")); } catch { /* gone or not ours */ }
    }
    try { blobs.push(readlinkSync(join(procRoot, pid, "cwd"))); } catch { /* gone or not ours */ }
  }
  return new Set(blobs);
}

function duBytes(path: string): number {
  const r = Bun.spawnSync(["du", "-sk", path], { stdout: "pipe", stderr: "ignore" });
  const kib = parseInt((r.stdout?.toString() ?? "").trim().split(/\s+/)[0] ?? "", 10);
  return r.exitCode === 0 && Number.isFinite(kib) ? kib * 1024 : 0;
}

export function sweepScratch(o: {
  root: string;
  liveNames: () => Set<string> | null;
  now?: number;
  maxAgeMs?: number;
  prefixes?: readonly string[];
  dryRun?: boolean;
}): ScratchSweep {
  const now = o.now ?? Date.now();
  const maxAge = o.maxAgeMs ?? SCRATCH_MAX_AGE_MS;
  const prefixes = o.prefixes ?? SCRATCH_PREFIXES;
  const empty: ScratchSweep = { status: "ran", removed: [], bytes: 0, keptFresh: 0, keptLive: [] };

  let names: string[];
  try { names = readdirSync(o.root); } catch { return { ...empty, status: "skipped", reason: `cannot read ${o.root}` }; }
  const candidates = names.filter(n => !n.endsWith(".lock") && prefixes.some(p => n.startsWith(p)));
  if (candidates.length === 0) return empty;

  let keptFresh = 0;
  const stale = candidates.filter(n => {
    if (now - newestMtimeMs(join(o.root, n)) < maxAge) { keptFresh++; return false; }
    return true;
  });
  if (stale.length === 0) return { ...empty, keptFresh };

  // Only once deletion is actually on the table does the guard have to succeed — the quiet case
  // stays quiet on a machine with no `/proc`, and the loud case fails closed.
  const live = o.liveNames();
  if (live === null)
    return { status: "skipped", reason: "the live-process scan failed — refusing to delete scratch without the guard",
             removed: [], bytes: 0, keptFresh, keptLive: [] };
  const named = [...live].join("\0");

  const removed: string[] = [];
  const keptLive: string[] = [];
  let bytes = 0;
  for (const n of stale) {
    if (named.includes(n)) { keptLive.push(n); continue; }
    const path = join(o.root, n);
    bytes += duBytes(path);
    if (!o.dryRun) { try { rmSync(path, { recursive: true, force: true }); } catch { continue; } }
    removed.push(n);
  }
  return { status: "ran", removed, bytes, keptFresh, keptLive };
}

/** Runs the sweep over `tmpdir()` and prints ONE line — silent when nothing qualified. */
function printScratchSweep(): void {
  const s = sweepScratch({ root: tmpdir(), liveNames: () => liveScratchNames() });
  if (s.status === "skipped") {
    if (s.reason && !s.reason.startsWith("cannot read")) console.log(`scratch sweep: ${s.reason}`);
    return;
  }
  if (s.removed.length === 0 && s.keptLive.length === 0) return;
  const live = s.keptLive.length ? `; kept ${s.keptLive.length} named by a live process` : "";
  console.log(
    `scratch sweep: removed ${s.removed.length} stale entr${s.removed.length === 1 ? "y" : "ies"} ` +
    `(older than ${SCRATCH_MAX_AGE_MS / 3_600_000} h) under ${tmpdir()}, ${fmtBytes(s.bytes)} freed${live}`,
  );
}

// ---- resource preflight: a memory cap cannot be discovered mid-run ------------------------------
// A tier runs for tens of minutes and commits to its peak in the first seconds. When it overcommits,
// the machine does not fail a gate — it swaps, and the developer power-cycles it, which destroys the
// run AND everything else on the box. So the floors are checked ONCE, up front, loudly, in the same
// shape `verify.ts`'s `diskHeadroomPreflight` established (hard floor, named cleanup command).
//
// Memory DEGRADES rather than refuses: a sequential tier is slow but correct, and a slow tier beats
// no tier. Only a floor at which even one cargo would thrash refuses outright. Disk REFUSES, because
// going sequential does not create free space and every nested-cargo gate downstream would die on
// ENOSPC tens of minutes in — the ENOSPC entry in `tests/testing-roadmap.toml` is that failure already
// paid for once.
//
// `fast` (what CI runs) is deliberately untouched: it spawns no batch, mints no scratch, and must not
// acquire a way to refuse to start on a runner whose disk this tool cannot reason about.
const MEM_DEGRADE_FLOOR_GIB = 8;   // below this, run the batch sequentially
const MEM_REFUSE_FLOOR_GIB = 2;    // below this, one cargo alone would thrash
const DISK_FLOOR_GIB = 10;         // the scratch a full tier mints is measured in tens of GiB

function availGiB(kind: "mem" | "disk"): number | undefined {
  if (kind === "mem") {
    try {
      const m = readFileSync("/proc/meminfo", "utf8").match(/^MemAvailable:\s+(\d+)\s*kB$/m);
      return m ? Number(m[1]) / 1024 / 1024 : undefined;
    } catch { return undefined; }
  }
  const r = Bun.spawnSync(["df", "-k", "--output=avail", tmpdir()], { stdout: "pipe", stderr: "ignore" });
  const kib = parseInt((r.stdout?.toString() ?? "").trim().split(/\r?\n/).pop() ?? "", 10);
  return r.exitCode === 0 && Number.isFinite(kib) ? kib / 1024 / 1024 : undefined;
}

/** The biggest `cddl*` scratch entries, largest first — a bare number is not actionable, a name is. */
function scratchOffenders(n = 5): string[] {
  const r = Bun.spawnSync(["sh", "-c", `du -sk ${tmpdir()}/cddl* 2>/dev/null | sort -rn | head -${n}`],
    { stdout: "pipe", stderr: "ignore" });
  return (r.stdout?.toString() ?? "").trim().split("\n").filter(Boolean).map(l => {
    const [kib, ...rest] = l.split(/\s+/);
    return `${(Number(kib) / 1024 / 1024).toFixed(1)} GiB  ${rest.join(" ")}`;
  });
}

/**
 * The floor decision, as a pure function of the four measurements — so the refuse and degrade
 * branches are pinned by a test instead of only ever reachable by running a machine out of memory.
 * `undefined` for a measurement means "could not read it", which must never itself refuse a run.
 */
export type PreflightAction = "proceed" | "degrade" | "refuse";
export function preflightDecision(o: {
  tier: Tier; jobs: number; skip: boolean; memAvailGiB?: number; diskAvailGiB?: number;
}): { action: PreflightAction; jobs: number; reason: string } {
  if (rank(o.tier) < rank("local"))
    return { action: "proceed", jobs: o.jobs, reason: `tier=${o.tier} spawns no batch and mints no scratch — floors not applicable` };
  if (o.skip)
    return { action: "proceed", jobs: o.jobs, reason: "CHECK_SKIP_PREFLIGHT=1 — memory/disk floors not checked" };
  if (o.diskAvailGiB !== undefined && o.diskAvailGiB < DISK_FLOOR_GIB)
    return { action: "refuse", jobs: o.jobs, reason: `${o.diskAvailGiB.toFixed(1)} GiB free scratch is under the ${DISK_FLOOR_GIB} GiB floor` };
  if (o.memAvailGiB !== undefined && o.memAvailGiB < MEM_REFUSE_FLOOR_GIB)
    return { action: "refuse", jobs: o.jobs, reason: `${o.memAvailGiB.toFixed(1)} GiB MemAvailable is under the ${MEM_REFUSE_FLOOR_GIB} GiB hard floor` };
  if (o.memAvailGiB !== undefined && o.memAvailGiB < MEM_DEGRADE_FLOOR_GIB && o.jobs > 1)
    return { action: "degrade", jobs: 1, reason: `${o.memAvailGiB.toFixed(1)} GiB MemAvailable is under the ${MEM_DEGRADE_FLOOR_GIB} GiB parallel floor` };
  const seen = [
    o.memAvailGiB !== undefined ? `${o.memAvailGiB.toFixed(1)} GiB MemAvailable` : "MemAvailable unreadable",
    o.diskAvailGiB !== undefined ? `${o.diskAvailGiB.toFixed(1)} GiB free scratch` : "free scratch unmeasurable",
  ].join(", ");
  return { action: "proceed", jobs: o.jobs, reason: `${seen} — floors clear` };
}

/** Measures, applies `preflightDecision`, and either returns the (possibly degraded) jobs or exits 2. */
function resourcePreflight(tier: Tier, jobs: number): number {
  const mem = rank(tier) < rank("local") ? undefined : availGiB("mem");
  const disk = rank(tier) < rank("local") ? undefined : availGiB("disk");
  const d = preflightDecision({
    tier, jobs, skip: process.env.CHECK_SKIP_PREFLIGHT === "1", memAvailGiB: mem, diskAvailGiB: disk,
  });
  if (d.action === "refuse") {
    const isDisk = disk !== undefined && disk < DISK_FLOOR_GIB;
    console.error(
      `HARNESS FAILURE: ${d.reason} — refusing to start a ${tier} tier.\n` +
      (isDisk
        ? `A ${tier} tier mints tens of GiB of nested-cargo scratch, so starting here buys a mid-run ENOSPC ` +
          `death tens of minutes from now instead of a message today. Largest scratch entries:\n` +
          (scratchOffenders().map(s => `  ${s}`).join("\n") || "  (none — the space is used by something else)") +
          `\nClear stale scratch — e.g. \`rm -rf ${tmpdir()}/cddl_codegen_* ${tmpdir()}/cddl_verify_*\` — then re-run.`
        : `Even one cargo would swap, and a machine that swaps under a tier stops responding rather than ` +
          `failing a gate. Close what is holding the memory (an editor's rust-analyzer, another session's ` +
          `gates) and re-run.`) +
      `\nCHECK_SKIP_PREFLIGHT=1 overrides this check.`,
    );
    process.exit(2);
  }
  if (d.action === "degrade")
    console.log(
      `preflight: ${d.reason} — DEGRADING to jobs=1 (sequential). A slow tier beats a locked-up machine; ` +
      `the requested CHECK_JOBS=${jobs} is overridden, and CHECK_SKIP_PREFLIGHT=1 keeps it.`,
    );
  else if (rank(tier) >= rank("local")) console.log(`preflight: ${d.reason}`);
  return d.jobs;
}

// ---- oracle resolution (mirrors cddl-matrix/verify.ts so the preflight can't disagree with it) ---
function resolveRubyCddl(): string | null {
  if (process.env.RUBY_CDDL) return existsSync(process.env.RUBY_CDDL) ? process.env.RUBY_CDDL : null;
  try {
    const r = Bun.spawnSync(["ruby", "-e", "puts Gem.user_dir"], { stdout: "pipe", stderr: "ignore" });
    const cand = join((r.stdout?.toString() ?? "").trim(), "bin", "cddl"); // where the `cddl` gem installs
    if (existsSync(cand)) return cand;
  } catch { /* ruby not installed */ }
  return null;
}
function resolveRustCddl(): string | null {
  const p = process.env.RUST_CDDL ?? resolve(homedir(), "Documents/git/cddl/target/debug/cddl");
  return existsSync(p) ? p : null;
}

// ---- meta-check 1 input: the suite's own #[ignore] list ------------------------------------------
function ignoredTestsFromCargo(): string[] {
  const r = Bun.spawnSync(
    ["cargo", "test", "--all-features", "--all-targets", "--", "--ignored", "--list"],
    { cwd: ROOT, stdout: "pipe", stderr: "inherit" },
  );
  const names: string[] = [];
  for (const line of (r.stdout?.toString() ?? "").split("\n")) {
    const m = line.match(/^(\S+):\s*test\s*$/); // e.g. "tests::integration_tests::ir_conformance_corpus: test"
    if (m) names.push(m[1].split("::").pop()!);
  }
  return names;
}

// ---- the three self-completeness meta-checks (first gate) ----------------------------------------
function runSelfChecks(): Outcome {
  const problems: string[] = [];
  const warnings: string[] = [];

  // 1. ignored-test classification: the cargo #[ignore] set must equal registry(manual) ∪ registry(stub).
  const ignored = new Set(ignoredTestsFromCargo());
  if (ignored.size === 0)
    problems.push("meta-1: `cargo test -- --ignored --list` yielded zero tests (build broken or output drift) — cannot classify");
  const manual = new Set(REGISTRY.filter(g => g.ignoredTest && g.kind !== "stub").map(g => g.ignoredTest!));
  const stubs = new Set(REGISTRY.filter(g => g.kind === "stub").map(g => g.ignoredTest!));
  const classified = new Set<string>([...manual, ...stubs]);
  for (const n of ignored)
    if (!classified.has(n))
      problems.push(`meta-1: #[ignore] test '${n}' is unclassified — register it in check.ts as a manual gate or a known-failing stub`);
  for (const n of classified)
    if (ignored.size > 0 && !ignored.has(n))
      problems.push(`meta-1: registered ignored-test '${n}' is no longer #[ignore] in the suite — update the registry`);

  // 2. matrix-script coverage: every cddl-matrix/*.ts (except the shared lib) must be wired to a gate.
  const scripts = readdirSync(MATRIX).filter(f => f.endsWith(".ts") && f !== "lib.ts");
  const referenced = new Set(REGISTRY.map(g => g.script).filter(Boolean) as string[]);
  for (const s of scripts)
    if (!referenced.has(s))
      problems.push(`meta-2: cddl-matrix/${s} is wired into no gate — add it to a tier in the registry (or justify its exclusion)`);

  // 3. CI-is-fast-tier invariant: build.yml must run exactly `bun run check.ts fast` — the one
  //    load-bearing line — and nothing else. Any other `run:` step means CI work is bypassing the
  //    registry (the fast tier is the sole definition of what CI does; growing it is a maintainer
  //    decision made HERE, not a workflow edit).
  const yml = readFileSync(join(ROOT, ".github/workflows/build.yml"), "utf8");
  // Both YAML step forms count: `- run: …` (run starts the step) and bare `run: …` (after `- name:`).
  const runSteps = yml.match(/^\s*(?:-\s+)?run:.*$/gm) ?? [];
  const isFastInvocation = (s: string) => /^\s*(?:-\s+)?run:\s*bun run check\.ts fast\s*$/.test(s);
  if (!runSteps.some(isFastInvocation))
    problems.push("meta-3: build.yml no longer invokes `bun run check.ts fast` — CI must run the fast tier through this runner");
  for (const s of runSteps)
    if (!isFastInvocation(s))
      problems.push(`meta-3: build.yml has a run step besides \`check.ts fast\` ('${s.trim()}') — CI work must flow through the registry's fast tier (maintainer decision)`);

  // 4. concurrency declarations are well-formed. Both halves guard against a declaration that
  //    silently does nothing — the failure mode a registry-encoded opt-in exists to prevent.
  //    (a) `cmd` only: the buffered-output and per-child-env path is implemented for spawned
  //        commands; an `fn` gate writes straight to the shared stdout and mutates process state, so
  //        declaring one concurrent would interleave output and mislabel cell rows.
  //    (b) consecutive: an ungrouped gate is a barrier, so a member cut off from its group by one
  //        would run alone while claiming otherwise.
  for (const g of REGISTRY)
    if (g.concurrent !== undefined && g.kind !== "cmd")
      problems.push(`meta-4: gate '${g.id}' declares concurrent='${g.concurrent}' but is kind='${g.kind}' — only \`cmd\` gates can be batched (buffered output + per-child env)`);
  {
    const seen = new Set<string>();
    let prev: string | undefined;
    for (const g of REGISTRY) {
      if (g.concurrent !== undefined && g.concurrent !== prev && seen.has(g.concurrent))
        problems.push(`meta-4: concurrency group '${g.concurrent}' is not contiguous in the registry — gate '${g.id}' is separated from the rest of its group by a barrier gate, so it would run ALONE while declaring otherwise`);
      if (g.concurrent !== undefined) seen.add(g.concurrent);
      prev = g.concurrent;
    }
  }

  // 5. `requires:` declarations are well-formed — the field only bites under `--only`, so nothing
  //    else would ever notice a stale one. Each prerequisite must exist, must be runnable, must sit
  //    EARLIER in the registry (a later one could not have produced what this gate reads), and must
  //    be in a tier no deeper than the dependent's, or the pair would be unselectable at the tier
  //    that offers the dependent.
  {
    const pos = new Map(REGISTRY.map((g, i) => [g.id, i]));
    for (const g of REGISTRY)
      for (const r of g.requires ?? []) {
        const dep = REGISTRY.find(x => x.id === r.gate);
        if (!dep || dep.kind === "stub")
          problems.push(`meta-5: gate '${g.id}' requires '${r.gate}', which is not a runnable registry gate`);
        else if (pos.get(dep.id)! > pos.get(g.id)!)
          problems.push(`meta-5: gate '${g.id}' requires '${r.gate}', which runs AFTER it in registry order — a prerequisite cannot follow its dependent`);
        else if (rank(dep.tier) > rank(g.tier))
          problems.push(`meta-5: gate '${g.id}' (${g.tier}) requires '${r.gate}' (${dep.tier}) — the pair would be unselectable at the '${g.tier}' tier`);
      }
  }

  for (const w of warnings) console.log("  WARN " + w);
  if (problems.length) {
    for (const p of problems) console.log("  FAIL " + p);
    return { status: "FAIL", reason: `${problems.length} self-completeness problem(s)` };
  }
  const groups = new Set(REGISTRY.map(g => g.concurrent).filter(Boolean) as string[]);
  console.log(
    `  OK — ${ignored.size} #[ignore] test(s) classified (${manual.size} manual gate(s), ${stubs.size} stub(s)), ` +
      `${scripts.length} matrix script(s) covered, CI runs the fast tier only, ` +
      `${groups.size} concurrency group(s) well-formed, ` +
      `${REGISTRY.reduce((n, g) => n + (g.requires?.length ?? 0), 0)} requires-edge(s) well-formed`,
  );
  return { status: "PASS" };
}

// ---- verify.ts gate: oracle preflight + run ------------------------------------------------------
function runVerify(o: Opts): Outcome {
  const ruby = resolveRubyCddl();
  const rust = resolveRustCddl();
  const missing: string[] = [];
  if (!ruby) missing.push("ruby `cddl`   install: gem install --user-install cddl");
  if (!rust) missing.push("rust `cddl`   build the pinned fork: local-fixes @ ac1b98e in the ~/Documents/git/cddl sibling checkout (or an immutable copy of it), then set RUST_CDDL to it — a stock `cargo install cddl` build is refused by verify.ts's oracle fingerprint");
  if (missing.length) {
    console.log("  oracle preflight FAILED — cddl-matrix/verify.ts needs both oracles:");
    for (const m of missing) console.log("    - " + m);
    if (o.skipMissing) return { status: "SKIPPED", reason: "oracle absent (--skip-missing)" };
    return { status: "FAIL", reason: `missing oracle(s): ${!ruby ? "ruby cddl " : ""}${!rust ? "rust cddl" : ""}`.trim() };
  }
  console.log(`  oracles OK — ruby=${ruby}  rust=${rust}`);
  const exit = sh(["bun", "run", "verify.ts"], MATRIX, { RUST_CDDL: rust!, RUBY_CDDL: ruby! });
  return exit === 0 ? { status: "PASS" } : { status: "FAIL", reason: `verify.ts exit ${exit}` };
}

// ---- gate-cache closure-audit gate: strace preflight, then run the strace'd audit script ---------
// A `fn` gate (not `cmd`) so a strace-less machine shows SKIPPED in the registry SUMMARY table, not
// a PASS whose skip is visible only in the scrollback — the honest-visible-skip rule at the summary
// level. The script keeps its own internal strace skip for direct invocation.
function runClosureAudit(): Outcome {
  if (!Bun.which("strace")) {
    console.log("  strace not found on PATH — install strace to run the gate-cache input-closure audit.");
    return { status: "SKIPPED", reason: "strace absent" };
  }
  // The ONE gate that asks for no cell rows. `strace -f` inflates every duration inside the traced
  // subtree, so rows produced here would measure strace rather than the gate — the same reason a
  // killed run's gate sum is not recorded as a wall time. Unsetting it also means the audit's traced
  // process makes no syscall this feature added, which keeps the audit's own closure argument
  // independent of anything argued here.
  const cells = process.env.CDDL_TIMING_CELLS;
  delete process.env.CDDL_TIMING_CELLS;
  try {
    const exit = sh(["bun", "run", "audit_gate_cache_closure.ts"], MATRIX);
    return exit === 0 ? { status: "PASS" } : { status: "FAIL", reason: `audit_gate_cache_closure.ts exit ${exit}` };
  } finally {
    if (cells !== undefined) process.env.CDDL_TIMING_CELLS = cells;
  }
}

// ---- cache-transparency gate: flag-gated; oracle preflight mirrors runVerify, then run the diff ---
// Flag-gated (`--cache-transparency`) because it costs ~two verify runs (one mostly-cached + one full);
// the roadmap prescribes an occasional manual full-tier diff, not an every-run cost. The oracle
// preflight is identical to `verify` so `--skip-missing` downgrades the same way; the resolved oracles
// are passed through the env, exactly as the cached run needs them.
function runCacheTransparency(o: Opts): Outcome {
  if (!o.cacheTransparency) return { status: "SKIPPED", reason: "pass --cache-transparency" };
  const ruby = resolveRubyCddl();
  const rust = resolveRustCddl();
  const missing: string[] = [];
  if (!ruby) missing.push("ruby `cddl`   install: gem install --user-install cddl");
  if (!rust) missing.push("rust `cddl`   build the pinned fork: local-fixes @ ac1b98e in the ~/Documents/git/cddl sibling checkout (or an immutable copy of it), then set RUST_CDDL to it");
  if (missing.length) {
    console.log("  oracle preflight FAILED — cache_transparency.ts needs both oracles (it runs verify.ts twice):");
    for (const m of missing) console.log("    - " + m);
    if (o.skipMissing) return { status: "SKIPPED", reason: "oracle absent (--skip-missing)" };
    return { status: "FAIL", reason: `missing oracle(s): ${!ruby ? "ruby cddl " : ""}${!rust ? "rust cddl" : ""}`.trim() };
  }
  console.log(`  oracles OK — ruby=${ruby}  rust=${rust}`);
  const exit = sh(["bun", "run", "cache_transparency.ts"], MATRIX, { RUST_CDDL: rust!, RUBY_CDDL: ruby! });
  return exit === 0 ? { status: "PASS" } : { status: "FAIL", reason: `cache_transparency.ts exit ${exit}` };
}

// ---- shared fuzz-crate provisioning: (re)generate `fuzz/generated` iff needed --------------------
// The crates under fuzz are GENERATED and gitignored, so BOTH fuzz gates need them and each is
// self-sufficient: whichever runs first materializes them, which is why neither declares a
// `requires:` edge on the other (a `--only` selection of either one still generates what it needs —
// there is no output of one that the other reads and would otherwise assert against vacuously).
// The once-per-run memo bounds COST only: without it a `--refresh-fuzz` tier run would pay for the
// same regeneration twice. It is set only on SUCCESS, so a failed generate is retried (and re-fails
// loudly) rather than silently skipped by the second gate.
let fuzzGeneratedThisRun = false;
function ensureFuzzGenerated(o: Opts): string | null {
  const gen = join(ROOT, "fuzz", "generated");
  const refresh = o.refreshFuzz && !fuzzGeneratedThisRun;
  if (existsSync(gen) && !refresh) return null;
  console.log(`  ${existsSync(gen) ? "--refresh-fuzz" : "fuzz/generated absent"} -> running fuzz/generate.sh`);
  const g = sh(["bash", "fuzz/generate.sh"], ROOT);
  if (g !== 0) return `fuzz/generate.sh exit ${g}`;
  fuzzGeneratedThisRun = true;
  return null;
}

// ---- fuzz compile-rot gate: (re)generate iff needed, then cargo check the fuzz crate -------------
function runFuzz(o: Opts): Outcome {
  const err = ensureFuzzGenerated(o);
  if (err) return { status: "FAIL", reason: err };
  const exit = sh(["cargo", "check", "--manifest-path", "fuzz/Cargo.toml"], ROOT);
  return exit === 0 ? { status: "PASS" } : { status: "FAIL", reason: `cargo check (fuzz) exit ${exit}` };
}

// ---- bounded fuzz RUN gate: a time-boxed libFuzzer walk of both targets --------------------------
// What it adds over `fuzz_compile_rot`: that gate proves the hostile-input surface stays REACHABLE
// (the crate compiles with every probed type in it); it cannot see a panic that only a live
// libFuzzer input triggers. This gate walks the surface — bounded, so it is a SMOKE-WALK of the
// reachable paths against the committed seed corpora, not the periodic deep run (that stays manual;
// `fuzz/README.md` states the two-layer posture).
//
// NOT gate-cached, deliberately. `run_cached` memoizes nested cargo whose OUTPUT is a pure function
// of the tree's bytes; a fuzz run is a randomized exploration that walks different inputs on every
// invocation over a corpus it mutates as it goes, so a content-hash hit would skip the only thing
// the gate does. The omission is a decision, not an oversight — the gate-cache closure audit traces
// ONE cached cell (`CLOSURE_AUDIT_GATE`) and enumerates no gate set, so nothing else records it.
//
// Sequential over the targets, one libFuzzer process at a time with an explicit RSS cap: the peak
// resident set the tier must bound is one process at `-rss_limit_mb` (2048, libFuzzer's default,
// passed explicitly so the bound is stated rather than inherited) plus the cargo build that precedes
// it — never a fan-out that scales with core count.
const FUZZ_TARGETS = ["from_cbor_bytes", "from_cbor_bytes_recursive"] as const;
const FUZZ_BUDGET_DEFAULT_S = 120;
const FUZZ_RSS_LIMIT_MB = 2048;

/** `FUZZ_BUDGET_S` — per-target `-max_total_time`, in seconds. Garbage falls back, loudly. */
function fuzzBudgetSeconds(raw: string | undefined): { seconds: number; warning?: string } {
  if (raw === undefined || raw.trim() === "") return { seconds: FUZZ_BUDGET_DEFAULT_S };
  const n = Number(raw.trim());
  if (!Number.isInteger(n) || n < 1)
    return {
      seconds: FUZZ_BUDGET_DEFAULT_S,
      warning: `FUZZ_BUDGET_S='${raw}' is not a positive integer — using ${FUZZ_BUDGET_DEFAULT_S}s per target`,
    };
  return { seconds: n };
}

/** The newest libFuzzer artifact for a target, if the run left one — the reproducer to cite. */
function newestFuzzArtifact(target: string): string | null {
  const dir = join(ROOT, "fuzz", "artifacts", target);
  if (!existsSync(dir)) return null;
  const files = readdirSync(dir)
    .map(f => join(dir, f))
    .filter(p => statSync(p).isFile())
    .sort((a, b) => statSync(b).mtimeMs - statSync(a).mtimeMs);
  return files[0] ?? null;
}

function runFuzzBoundedRun(o: Opts): Outcome {
  // Missing tooling is a FAIL, never a skip: this gate exists only in the tier that ships the
  // feature, and a silent skip there voids the guarantee (the `runNoStdCheck` full-tier posture).
  // No local-tier softening arm because the gate is full-only — there is no softer tier to be in.
  const missing: string[] = [];
  const rustup = Bun.which("rustup");
  const nightly = rustup
    ? Bun.spawnSync([rustup, "toolchain", "list"], { stdout: "pipe", stderr: "pipe" })
    : null;
  if (!nightly || nightly.exitCode !== 0 || !/^nightly/m.test(nightly.stdout.toString()))
    missing.push("nightly toolchain   install: rustup toolchain install nightly");
  const cargo = Bun.which("cargo");
  const fuzzVersion = cargo
    ? Bun.spawnSync([cargo, "+nightly", "fuzz", "--version"], { stdout: "pipe", stderr: "pipe" })
    : null;
  if (!fuzzVersion || fuzzVersion.exitCode !== 0)
    missing.push("cargo-fuzz          install: cargo install cargo-fuzz");
  if (missing.length) {
    console.log("  fuzz tooling preflight FAILED — the bounded fuzz run needs both:");
    for (const m of missing) console.log("    - " + m);
    return { status: "FAIL", reason: `missing fuzz tooling: ${missing.length === 2 ? "nightly + cargo-fuzz" : missing[0]!.split("  ")[0]}` };
  }

  const err = ensureFuzzGenerated(o);
  if (err) return { status: "FAIL", reason: err };

  const { seconds, warning } = fuzzBudgetSeconds(process.env.FUZZ_BUDGET_S);
  if (warning) console.log("  WARN " + warning);
  console.log(`  ${FUZZ_TARGETS.length} target(s), sequential, ${seconds}s each (FUZZ_BUDGET_S), rss limit ${FUZZ_RSS_LIMIT_MB} MiB`);
  for (const target of FUZZ_TARGETS) {
    console.log(`  --- fuzzing ${target} ---`);
    const exit = sh(
      ["cargo", "+nightly", "fuzz", "run", target, "--",
        `-max_total_time=${seconds}`, `-rss_limit_mb=${FUZZ_RSS_LIMIT_MB}`],
      ROOT,
    );
    if (exit !== 0) {
      const artifact = newestFuzzArtifact(target);
      return {
        status: "FAIL",
        reason: `${target}: cargo fuzz run exit ${exit}` +
          (artifact ? ` — reproducer ${relative(ROOT, artifact)} (replay: cargo +nightly fuzz run ${target} ${relative(ROOT, artifact)})` : " (no artifact written)"),
      };
    }
  }
  return { status: "PASS" };
}

// ---- no-std drift gate: fresh-generate the profiles, thumb-check each emitted shim ---------------
// A `fn` gate for two reasons. (1) The absent-target outcome is TIER-DEPENDENT — a loud SKIPPED in
// `local`, a hard FAIL in `full` — and `Opts` carries no tier, so the gate reads it the way `main()`
// does: through `tierFromArgv`, the shared argv parser (the self-log re-exec preserves argv
// verbatim, and going through the parser is what keeps a `--only` VALUE from reading as a tier).
// (2) A SKIPPED
// shows in the registry SUMMARY table rather than only in the scrollback, the same honest-visible-skip
// reason `gate_cache_closure_audit` is a `fn` gate.
function runNoStdCheck(): Outcome {
  return runNoStdCheckGate(tierFromArgv(process.argv.slice(2)));
}

// ---- component JS-host gate: transpile with jco and drive the result from node --------------------
// A `fn` gate for exactly the `runNoStdCheck` reason above: its provisioning outcome is TIER-DEPENDENT.
// node, npm and a first run that needs the npm REGISTRY are more fragile than a rustup target, so a
// machine legitimately lacking them must not fail `local` — but a silent skip in the tier that SHIPS
// the feature would void the guarantee `full` exists to give. `CDDL_JCO_REQUIRED=1` is what the Rust
// test reads to turn its loud skip into a panic; passing it through the env (rather than adding an
// `env` field to `Gate`) keeps the registry's shape — and its meta-checks — unchanged.
function runJcoCheck(): Outcome {
  const env = tierFromArgv(process.argv.slice(2)) === "full" ? { CDDL_JCO_REQUIRED: "1" } : undefined;
  const exit = sh(["cargo", "test", "--bin", "cddl-codegen", "component_jco"], ROOT, env);
  return exit === 0 ? { status: "PASS" } : { status: "FAIL", reason: `cargo test component_jco exit ${exit}` };
}

// ---- matrix typecheck gate: tsc --noEmit via the pinned local devDependency ----------------------
// The cddl-matrix scripts run under Bun (no build step), but nothing checks their strict types unless
// `tsc` runs. This gate uses the LOCAL typescript pinned in cddl-matrix/package.json (not a global /
// bunx-downloaded one) so the version is reproducible from the committed lockfile. The runtime stays
// dependency-free — node_modules is dev-only and never imported at run time.
function runMatrixTypecheck(): Outcome {
  const tsc = join(MATRIX, "node_modules", ".bin", "tsc");
  if (!existsSync(tsc)) {
    console.log("  cddl-matrix/node_modules is missing — install the dev-only typecheck deps first:");
    console.log("    (cd cddl-matrix && bun install)");
    return { status: "FAIL", reason: "cddl-matrix/node_modules absent (run `bun install` in cddl-matrix)" };
  }
  const exit = sh([tsc, "--noEmit"], MATRIX);
  return exit === 0 ? { status: "PASS" } : { status: "FAIL", reason: `tsc --noEmit exit ${exit}` };
}

// ==================================================================================================
// THE REGISTRY — one entry per gate. Execution order is registry order; a run at tier T executes
// every non-stub gate whose tier rank <= rank(T). fast ⊂ local ⊂ full holds by construction.
//   - `self_checks` (fast) runs FIRST so a mis-registered gate fails before anything expensive.
//   - the `fast` tier is WHAT CI RUNS (build.yml invokes `bun run check.ts fast`, enforced by
//     meta-check 3). Keep it the absolute minimum: the cheap correctness floor (fmt / clippy /
//     snapshot tests) plus the sub-second drift gates. Promoting a gate into `fast` is a maintainer
//     decision — new gates default to `local` or `full`.
//   - the `local` tier (default) is "run before considering work done": fast + the workspace build
//     and the full `cargo test` suite (corpus + wasm-matrix compile gates + emitted-test execution).
//   - `snapshot_quick` is the fast-tier inner loop; in local/full it is subsumed by the full
//     `cargo test` but stays cheap (~5s) so tier-supersetting holds by construction.
// ==================================================================================================
/**
 * The one concurrency group there is: the `#[ignore]`d, manual-only heavy gates. Named once so the
 * membership is greppable and a typo cannot silently create a second group of one.
 */
const MANUAL_HEAVY = "manual_heavy";

export const REGISTRY: Gate[] = [
  { id: "self_checks", tier: "fast", kind: "fn", run: runSelfChecks,
    desc: "self-completeness meta-checks (ignored-test + matrix-script coverage + CI-is-fast-tier)" },

  // --- fast tier (CI + the inner loop) ---
  { id: "fmt", tier: "fast", kind: "cmd", cmd: ["cargo", "fmt", "--all", "--", "--check"],
    desc: "rustfmt check" },
  { id: "clippy", tier: "fast", kind: "cmd",
    cmd: ["cargo", "clippy", "--locked", "--workspace", "--all-features", "--all-targets", "--", "--deny", "clippy::all", "--deny", "clippy::assertions_on_result_states"],
    desc: "clippy (deny all + assertions_on_result_states)" },
  { id: "snapshot_quick", tier: "fast", kind: "cmd", cmd: ["cargo", "test", "--bin", "cddl-codegen", "snapshot_tests"],
    desc: "golden snapshot tests (in-process, fast)" },

  // --- fast tier: the matrix-drift gates (sub-second file-scanners; project_corpus also runs the
  //     repo's own `cargo run --example ast_roles` AST walk) ---
  { id: "build_matrix_check", tier: "fast", kind: "cmd", cmd: ["bun", "run", "build_matrix.ts", "--check"], cwd: MATRIX,
    script: "build_matrix.ts", desc: "matrix.json matches authored overlay" },
  { id: "project_robustness_check", tier: "fast", kind: "cmd", cmd: ["bun", "run", "project_robustness.ts", "--check"], cwd: MATRIX,
    script: "project_robustness.ts", desc: "robustness fixtures drift gate" },
  { id: "project_wasm_matrix_check", tier: "fast", kind: "cmd", cmd: ["bun", "run", "project_wasm_matrix.ts", "--check"], cwd: MATRIX,
    script: "project_wasm_matrix.ts", desc: "wasm-ABI matrix fixtures drift gate" },
  { id: "project_golden_hex_check", tier: "fast", kind: "cmd", cmd: ["bun", "run", "project_golden_hex.ts", "--check"], cwd: MATRIX,
    script: "project_golden_hex.ts", desc: "golden_hex COVERAGE.md drift gate" },
  { id: "project_corpus", tier: "fast", kind: "cmd", cmd: ["bun", "run", "project_corpus.ts"], cwd: MATRIX,
    script: "project_corpus.ts", desc: "corpus overlay validator + COVERAGE.md rewrite" },
  { id: "coverage_md_diff", tier: "fast", kind: "cmd", cmd: ["git", "diff", "--exit-code", "tests/corpus/COVERAGE.md"], cwd: ROOT,
    requires: [{ gate: "project_corpus", why: "it diffs the COVERAGE.md that gate rewrites; alone it passes vacuously against the committed file" }],
    desc: "tests/corpus/COVERAGE.md is up to date" },

  // --- local tier (default): the heavy correctness gates, NOT run in CI (cost policy) ---
  { id: "build", tier: "local", kind: "cmd", cmd: ["cargo", "build", "--locked", "--workspace", "--all-features", "--all-targets"],
    desc: "workspace build" },
  { id: "test", tier: "local", kind: "cmd", cmd: ["cargo", "test", "--all-features", "--all-targets"],
    desc: "full test suite (incl. corpus + wasm-matrix compile gates)" },
  // Named separately from the `test` sweep above (which also runs it) so a component-face failure is
  // reportable as one: this suite's four-stage WIT-validity gate is the only place the emitted `.wit`
  // is checked against the pinned component-model toolchain, and its wasip2 build smoke the only
  // place the emitted GUEST GLUE is compiled at all — a package whose WIT resolves, encodes and
  // validates can still name a trait method that does not exist. A tier log that names this gate is
  // what makes both visible without reading the full test output. The smoke is nested cargo and is
  // memoized per generated-crate content hash by the gate cache, so an unchanged tree re-runs cheap.
  { id: "component_wit", tier: "local", kind: "cmd", cmd: ["cargo", "test", "--bin", "cddl-codegen", "component_tests"],
    desc: "component face: WIT validity (resolve/encode/validate), wasm-posture purity, wasip2 build smoke" },
  // The CROSS-CRATE seam, named for the same reason as its siblings: import mode is the one part of
  // this face whose inputs come from ANOTHER crate's committed output, so a failure here means a
  // consumer's build breaks on a dependency's artifact rather than on its own spec — a distinction
  // worth reading off the tier log. Mostly in-process generation over scratch dirs plus the same
  // pinned WIT oracle, and ONE nested-cargo cell: a five-crate wasip2 build (one dependency, two
  // consumers and their guests), which is the only place three facts are observable at all — the
  // `with:` map is co-required, an imported resource's borrow lowers to `&T` rather than to a borrow
  // newtype, and a dependency-typed collection parameter lowers ONLY through the accumulator shape.
  // Each is a macro-expansion or type-inference failure, so a package that resolves, encodes and
  // validates says nothing about any of them. That cell is memoized by the gate cache.
  { id: "component_import", tier: "local", kind: "cmd", cmd: ["cargo", "test", "--bin", "cddl-codegen", "component_import"],
    desc: "component face: cross-crate import mode (dep WIT materialization, `with:` map, bytes seam, accumulator, wasip2 build)" },
  // The rust->WIT surface differential, named for the same reason: it is the ONLY gate that asks
  // what the boundary DROPPED. Everything else judges what was emitted against itself — a member
  // missing from both the `.wit` and the glue is a package that resolves, validates and builds.
  // In-process generation plus parsing, so it is cheap; it is separate from `component_wit` so a
  // parity regression and a validity regression never arrive under one name.
  { id: "component_parity", tier: "local", kind: "cmd", cmd: ["cargo", "test", "--bin", "cddl-codegen", "component_parity"],
    desc: "component face: rust pub surface -> WIT surface differential (ledgered)" },
  // The only gate that RUNS the face. Its two siblings above judge emitted bytes; this one loads a
  // real wasm32-wasip2 component into wasmtime and drives it, which is the only way to reach the
  // classes no static reading can: a fallible door returning `Err` instead of trapping (a trap
  // poisons the instance for every later caller), a getter handing back a snapshot rather than an
  // alias, the three distinct states of `option<option<T>>`, byte-equality against the rust crate's
  // own serialization, and the same-handle-as-receiver-and-argument case that has no wasm-face
  // precedent at all. Named separately for the same reason as its siblings: a behavioral regression
  // and a validity regression must never arrive under one name.
  //
  // Nested cargo over a hand-written host crate copied INSIDE the hashed output root, memoized by
  // the gate cache. Measured on the delivering machine: 81 s cold (a fresh scratch root, so
  // wasmtime builds), 3 s warm (cache hit), 9 s on a cache MISS with the scratch root warm — which
  // is why that root is kept between runs rather than deleted.
  { id: "component_host", tier: "local", kind: "cmd", cmd: ["cargo", "test", "--bin", "cddl-codegen", "component_host"],
    desc: "component face: behavioral gate — a real wasip2 component driven through a wasmtime host" },
  // THE acceptance gate for the whole component face: two independently generated crates, built as
  // two components, COMPOSED into one dual-export world, and driven through the flow the feature
  // exists for — mint a dependency object, hand it to a consumer, get one back from a consumer
  // getter, and keep using it through the dependency's own interface. Every gate above stops one
  // step short of that: `component_import_wasip2_build` proves the consumer's imported-resource glue
  // COMPILES, `component_host` proves ONE component behaves. Only here do two crates agree at
  // runtime, which is the claim the face is for.
  //
  // It also pins the instantiate-once finding in both halves: the two-instance topology composes at
  // exit 0 into a world indistinguishable from the correct one, and the first handle crossing then
  // fails. That asymmetry is why the prescription is documented rather than delegated to the
  // composer.
  //
  // Nested cargo over a hand-written host crate copied INSIDE the hashed output root, composing
  // in-process with `wac-graph` (no `wac` binary, whose version would sit outside the lockfile),
  // memoized by the gate cache. Measured on the delivering machine: 91 s cold (a fresh scratch root,
  // so wasmtime and wac-graph build), 4 s warm (cache hit), 7 s on a forced run with the scratch
  // root warm — which is why that root is kept between runs rather than deleted. It is the same cost
  // class as `component_host` above and cheaper on the re-run path, which is what puts it at `local`
  // rather than `full`.
  { id: "component_compose", tier: "local", kind: "cmd", cmd: ["cargo", "test", "--bin", "cddl-codegen", "component_compose"],
    desc: "component face: cross-crate acceptance — two generated components composed and driven through wasmtime" },
  // The face as its REAL audience meets it. Every gate above judges the component from Rust; the
  // motivating consumer is a JS dApp, which reaches it through a TRANSPILER (`jco`) rather than a
  // runtime — and the surface jco synthesizes is not the wasmtime one. A WIT `enum` arrives as a
  // string label and REJECTS the numeric discriminant the wasm-bindgen face takes; a fallible door
  // throws instead of returning; disposal hangs off an own property, not a prototype. None of that is
  // observable above this line, and all of it is a claim `component_differences.mdx` makes to a
  // consumer. Three legs: the single-component surface, the cross-crate wiring (two separately
  // transpiled modules joined by `--map` — the shape the docs prescribe), and a KNOWN-BROKEN pin on
  // `wac`-composed artifacts under jco 1.26.1, whose worse symptom is a silently WRONG object rather
  // than a throw. That third leg loud-skips ALONE when the ambient `wac` is absent or below 0.9.
  //
  // Reused fixtures throughout (`component-host/inputs`, `component-compose/{dep,consumer}`), so a
  // disagreement with the two wasmtime gates is a finding about the HOST, not the emitter.
  //
  // `local` on both counts the placement has to answer. COST: the cheapest gate in this group — no
  // wasmtime, no composer crate, no native host crate; three wasip2 guest builds and a sub-second
  // transpile. Measured on the delivering machine: 17 s cold (fresh scratch root, so `npm ci` and the
  // guest builds run), 4 s warm (cache hit — which pays the six-manifest `cargo generate-lockfile`
  // preflight), 3 s on a forced run with the scratch root warm. PROVISIONING: node, npm and a cold
  // run's npm registry access are more fragile than a rustup target, which is why the gate is a `fn`
  // — it loud-SKIPS at `local` and hard-FAILS at `full` (`CDDL_JCO_REQUIRED=1`), rather than sitting
  // at `full` where the JS face would go unchecked in the tier run dozens of times a day.
  { id: "component_jco", tier: "local", kind: "fn", run: runJcoCheck,
    desc: "component face: JS host — jco-transpiled components driven from node (surface + cross-crate + the wac-composed known-broken pin)" },
  // The one gate that regenerates OVER prior output at corpus breadth — the only path on which the
  // comment-preservation overlay runs at all, and the home of two shipped classes (a comment stranded
  // by a rule deletion into a self-perpetuating `compile_error!` sentinel; a `cddl-codegen:replace`
  // block orphaning an import). `#[ignore]`d because it is 5 generator runs x 91 fixtures rather than
  // because it is fragile, and `local` rather than `full` because it is measured at 40 s: no cargo,
  // no network, six worker threads over generator subprocesses. `--exact` on the full module path so
  // it does not sweep in its `_compiles` sibling, which is `full` and pays nested cargo.
  { id: "regen_over_prior_output_corpus", tier: "local", kind: "cmd",
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "tests::regen_over_prior_tests::regen_over_prior_output_corpus", "--", "--exact", "--ignored", "--nocapture"],
    ignoredTest: "regen_over_prior_output_corpus",
    desc: "corpus-wide regen over prior output: trailing-comment floor + rule-DELETION and user-EDIT regen variants (manual, #[ignore]d)" },
  { id: "insta_orphan", tier: "local", kind: "cmd",
    cmd: ["cargo", "insta", "test", "--unreferenced=reject", "--", "snapshot_tests", "robustness"],
    desc: "snapshot orphan check" },
  { id: "matrix_typecheck", tier: "local", kind: "fn", run: runMatrixTypecheck,
    desc: "tsc --noEmit over the cddl-matrix scripts (pinned local devDependency)" },
  { id: "no_std_check", tier: "local", kind: "fn", run: runNoStdCheck, script: "no_std_check.ts",
    desc: "emitted no-std-check shims: fresh-generate the six profiles, cargo check each shim on thumbv7m-none-eabi" },
  // verify.ts's own gate is `full`-tier, so without this its assert-at-startup self-tests would only
  // ever run inside a ~60-minute manual gate — a millisecond check sitting behind the most expensive
  // thing in the repo. What they cover fails SILENTLY in production: a wrong ruby verdict token, wasm
  // evidence-stage name, or policy-mint classification (among others) is a plausible-looking annotation
  // or hidden triage exemption, not an error, so the window between breaking one and anyone noticing is
  // the thing worth closing. Hermetic and ~30ms — only
  // argv parsing runs ahead of the self-test blocks, so it needs no oracle, no cargo, no network.
  // (`script:` is deliberately absent: meta-check 2's verify.ts mapping belongs to the `verify` gate.)
  //
  // The desc names the CLASS, never a count: verify.ts grows self-test blocks as its legs grow, and a
  // counted description silently became wrong the first time one was added (it read "three" while six
  // ran). `--selftest`'s own output enumerates the live blocks, which is the honest place for it.
  { id: "verify_selftest", tier: "local", kind: "cmd",
    cmd: ["bun", "run", "verify.ts", "--selftest"], cwd: MATRIX,
    desc: "verify.ts's pure startup deciders (evidence-vocabulary classifiers, the component leg's selection + ident mirror, the build-sweep classifier), whose failures are silent in production" },
  { id: "roadmap_projection_check", tier: "fast", kind: "cmd",
    cmd: ["bun", "run", "project_roadmaps.ts", "--roadmap", "all", "--check"], cwd: MATRIX,
    script: "project_roadmaps.ts",
    desc: "roadmap selftests + schema/identity/reference/section validation, canonical-TOML equality and an in-memory render of both roadmap projections (pure committed files, no cargo/network)" },
  // --- THE SUB-SECOND NO-CARGO FILE-SCANNER CLASS, promoted from `local` into `fast` (CI) ---
  // Eight gates: `lint_doc_citations`, `project_decode_conformance`, `project_recombination_check`,
  // the four `query_q*` gates and `project_status_headers`. Maintainer call, 2026-08-03 — the same
  // promotion form as `project_multifile_matrix_check` below ("maintainer call, 2026-07"), taken for
  // the whole class at once: measured 629 ms warm for all eight together, ~4 % of the fast tier's
  // wall, against a proven cost of the split (a HEAD commit shipped CI-green with three of these
  // red locally, surfacing one per session behind fail-fast).
  // `roadmap_projection_check` is a separately approved ninth member (maintainer call,
  // 2026-08-11): it has the same pure committed-file shape, and it is what keeps both roadmap
  // TOML sources valid and canonical and proves both projections still render (the renders
  // themselves are gitignored draft/roadmaps/ artifacts, so there is no committed copy to drift).
  //
  // THE CLASS BOUNDARY, so the next gate addition can answer for itself whether it belongs here:
  // pure reads of COMMITTED files, no cargo, no network, no `cddl-matrix/node_modules`, no `draft/`
  // ledger. `lint_doc_citations` shells `git ls-files -z` and `project_status_headers` imports this
  // registry — both fine on any checkout; neither is a subprocess CI must provision for.
  // Two `local` neighbours below are deliberately OUTSIDE the class, and stay `local`:
  //   - `matrix_typecheck` needs `cddl-matrix/node_modules`, which CI cannot have without a second
  //     `run:` step — and meta-check 3 rejects one by design.
  //   - `no_silent_directive` spawns `cargo build --bin cddl-codegen` plus generator runs; its warm
  //     5 s is a warm-target-dir number and it is not a file scanner at all.
  // `verify_selftest` and `timings_digest_check` are in the class by COST but were not part of the
  // ruling; promoting either is a new maintainer call, never an extension of this one.
  { id: "lint_doc_citations", tier: "fast", kind: "cmd",
    cmd: ["bun", "run", "lint_doc_citations.ts"], cwd: MATRIX,
    script: "lint_doc_citations.ts", desc: "documentation citation existence + positional-citation + MD022 lint" },
  { id: "project_decode_conformance", tier: "fast", kind: "cmd",
    cmd: ["bun", "run", "project_decode_conformance.ts"], cwd: MATRIX,
    script: "project_decode_conformance.ts", desc: "decode-conformance catalog drift gate (matrix.json + catalog.toml, no cargo)" },
  { id: "no_silent_directive", tier: "local", kind: "cmd",
    cmd: ["bun", "run", "no_silent_directive.ts"], cwd: MATRIX,
    script: "no_silent_directive.ts", desc: "comment-DSL directive×shape silent-drop net (generate with/without each directive, FAIL on byte-identical-and-unmentioned)" },
  { id: "project_recombination_check", tier: "fast", kind: "cmd",
    cmd: ["bun", "run", "project_recombination.ts", "--check"], cwd: MATRIX,
    script: "project_recombination.ts", desc: "recombination-fuzzer ingredients drift gate (matrix.json → tests/recomb/ingredients.json, no cargo)" },
  // Sub-second file-scanner promoted to `fast` beside its sibling `project_wasm_matrix_check`
  // (maintainer call, 2026-07: measured ~0.04s wall).
  { id: "project_multifile_matrix_check", tier: "fast", kind: "cmd",
    cmd: ["bun", "run", "project_multifile_matrix.ts", "--check"], cwd: MATRIX,
    script: "project_multifile_matrix.ts", desc: "multifile placement matrix fixtures drift gate" },
  { id: "query_q4_directional", tier: "fast", kind: "cmd", cmd: ["bun", "run", "query_q4_directional.ts", "--check"], cwd: MATRIX,
    script: "query_q4_directional.ts", desc: "Q4 directional-support query + consistency gate (matrix.json + catalog.toml, no cargo)" },
  { id: "query_q1_gaps", tier: "fast", kind: "cmd", cmd: ["bun", "run", "query_q1_gaps.ts", "--check"], cwd: MATRIX,
    script: "query_q1_gaps.ts", desc: "Q1 support-gap query + generated-Limitations drift gate (matrix.json → current_capacities.mdx, no cargo)" },
  { id: "query_q5_completeness", tier: "fast", kind: "cmd", cmd: ["bun", "run", "query_q5_completeness.ts", "--check"], cwd: MATRIX,
    script: "query_q5_completeness.ts", desc: "Q5 matrix-self-completeness query + reconciliation gate (matrix.json + sources/*.abnf/.prelude, no cargo)" },
  { id: "query_q6_diff", tier: "fast", kind: "cmd", cmd: ["bun", "run", "query_q6_diff.ts", "--check"], cwd: MATRIX,
    script: "query_q6_diff.ts", desc: "Q6 profile/version-diff query + profile-set consistency, vacuity & annotation-completeness gate (matrix.json, no cargo)" },
  { id: "project_status_headers", tier: "fast", kind: "cmd",
    cmd: ["bun", "run", "project_status_headers.ts", "--check"], cwd: MATRIX,
    script: "project_status_headers.ts",
    desc: "status-header count spans drift gate (matrix.json + catalog.toml + check.ts registry → README/tests-README, no cargo)" },
  // STRUCTURE only — this gate never asserts a duration. Gate durations are nondeterministic
  // (machine load, cross-session contention), so a drift gate on the numbers would add a flaky gate
  // to the very suite the measurement work exists to make cheaper.
  { id: "timings_digest_check", tier: "local", kind: "cmd",
    cmd: ["bun", "run", "project_timings.ts", "--check"], cwd: MATRIX,
    script: "project_timings.ts",
    desc: "tests/timings.json structural gate (a row per registry gate, no orphan rows) + the digest update rule's pure-function pins" },

  // --- full tier: the manual-only gates (run by memory today; the whole point of this runner) ---
  // Every gate in the run below is `#[ignore]`d, `cmd`-shaped, and owns a scratch root nothing else
  // touches, so they are declared mutually concurrent. The audit behind that (each gate's
  // `temp_dir()` root name, the `acquire_scratch_lock` flock on the ones that share a root across
  // repeat runs, the atomic tmp-write+rename gate cache, cargo's own build-directory lock, and
  // `oracle_fingerprint.json` being read-only in both of its consumers) is what makes the
  // declaration a claim rather than a hope. Two members deliberately excluded, each for its own
  // reason, both of them BARRIERS that end the batch: `verify_cache_transparency` must observe a
  // cache `verify` already warmed, and `gate_cache_closure_audit` is an strace input-closure audit —
  // ambient concurrent file activity is exactly what it must not see.
  //
  // The batch is CONTIGUOUS on purpose (meta-check 4 enforces it): the first ungrouped gate below
  // (`verify`) ends it, so registry order still means what it says.
  { id: "wasm_matrix_roundtrips", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "wasm_matrix_roundtrips", "--", "--ignored"],
    ignoredTest: "wasm_matrix_roundtrips", desc: "wasm-ABI matrix round-trip gate (manual, #[ignore]d)" },
  { id: "multifile_matrix_roundtrips", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "multifile_matrix_roundtrips", "--", "--ignored"],
    ignoredTest: "multifile_matrix_roundtrips", desc: "multifile placement matrix round-trip gate — both generated subcrates, all profiles (manual, #[ignore]d)" },
  { id: "identifier_hazard_crates_compile", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "identifier_hazard_crates_compile", "--", "--ignored"],
    ignoredTest: "identifier_hazard_crates_compile", desc: "identifier-hazard sweep standalone compile gate (manual, #[ignore]d)" },
  { id: "generated_local_out_of_scope_crates_compile", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "generated_local_out_of_scope_crates_compile", "--", "--ignored"],
    ignoredTest: "generated_local_out_of_scope_crates_compile",
    desc: "generated-local reserved-name scope gate — every out-of-scope cell must still COMPILE, per profile (manual, #[ignore]d)" },
  { id: "recombination_crates_execute", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    // Full module path + `--exact`: the sibling `recombination_preserve_crates_execute` gate must not
    // cross-select under cargo's default substring matching (and vice versa).
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "tests::recombination_tests::recombination_crates_execute", "--", "--exact", "--ignored", "--nocapture"],
    ignoredTest: "recombination_crates_execute",
    desc: "recombination fuzzer layer 2 (default profile): batched --emit-tests execution of the ok compositions (manual, #[ignore]d)" },
  { id: "recombination_preserve_crates_execute", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "tests::recombination_tests::recombination_preserve_crates_execute", "--", "--exact", "--ignored", "--nocapture"],
    ignoredTest: "recombination_preserve_crates_execute",
    desc: "recombination fuzzer layer 2 (preserve profile): --preserve-encodings escalation of the ok compositions (manual, #[ignore]d)" },
  { id: "recombination_json_crates_execute", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "tests::recombination_tests::recombination_json_crates_execute", "--", "--exact", "--ignored", "--nocapture"],
    ignoredTest: "recombination_json_crates_execute",
    desc: "recombination fuzzer layer 2 (json profile): serde/schemars escalation of the ok compositions (manual, #[ignore]d)" },
  { id: "recombination_wasm_crates_check", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "tests::recombination_tests::recombination_wasm_crates_check", "--", "--exact", "--ignored", "--nocapture"],
    ignoredTest: "recombination_wasm_crates_check",
    desc: "recombination fuzzer layer 2 (wasm profile): batched --wasm=true cargo check of generated wasm crates (manual, #[ignore]d)" },
  { id: "ir_conformance_corpus", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "ir_conformance_corpus", "--", "--ignored", "--nocapture"],
    ignoredTest: "ir_conformance_corpus", desc: "IR-bug conformance oracle at corpus breadth + decorrelated ruby `cddl` gem sweep (gem REQUIRED — FAILS if absent unless CDDL_RUBY_ORACLE=skip; manual, #[ignore]d)" },
  // The sibling gate's DIRECTORY-INPUT leg: `ir_conformance_corpus` sweeps single files only, so the
  // multi-module emission (root-level test module + scope globs) was outside the oracle's reach. Its
  // filter substring is disjoint from every other cargo-test filter here, so it selects alone.
  { id: "ir_conformance_multifile", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "ir_conformance_multifile", "--", "--ignored", "--nocapture"],
    ignoredTest: "ir_conformance_multifile", desc: "IR-bug conformance oracle, directory-input leg: a multifile placement cell's non-root-module rule judged against the concatenated source spec (cddl git dep — first fetch needs network; manual, #[ignore]d)" },
  { id: "rust_oracle_fingerprint", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "rust_oracle_fingerprint", "--", "--ignored", "--nocapture"],
    ignoredTest: "rust_oracle_fingerprint", desc: "rust CDDL_ORACLE_DEP behavioral fingerprint preflight (shared oracle_fingerprint.json; manual, #[ignore]d)" },
  { id: "decode_conformance_replay", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "decode_conformance_replay", "--", "--ignored", "--nocapture"],
    ignoredTest: "decode_conformance_replay",
    desc: "decode-conformance replay: committed catalog vectors decode (+ preserve byte-identity + json/wasm decode-surface legs), oracle-free (manual, #[ignore]d)" },
  // Name is NOT a superstring of `decode_conformance_replay` — the sibling gate above filters by
  // SUBSTRING, so a `corpus_decode_conformance_replay` would be swept into it. `corpus_decode_replay`
  // substring-matches no other test name and no other cargo-test filter here, so it runs alone.
  { id: "corpus_decode_replay", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "corpus_decode_replay", "--", "--ignored", "--nocapture"],
    ignoredTest: "corpus_decode_replay",
    desc: "corpus (composition-depth) decode-conformance replay: committed corpus_catalog.toml vectors decode (+ preserve byte-identity + json/wasm decode-surface legs), oracle-free (manual, #[ignore]d)" },
  { id: "all_supported_constructs_generate_all_profiles", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "all_supported_constructs_generate_all_profiles", "--", "--ignored"],
    ignoredTest: "all_supported_constructs_generate_all_profiles",
    desc: "supported catalog generates under all 3 profiles (manual, #[ignore]d)" },
  { id: "feature_corpus_roundtrips_nondefault_profiles", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "feature_corpus_roundtrips_nondefault_profiles", "--", "--ignored"],
    ignoredTest: "feature_corpus_roundtrips_nondefault_profiles",
    desc: "corpus emit-tests round-trip under preserve/json (manual, #[ignore]d)" },
  // Corpus-breadth component compilation, which `feature_corpus_compiles` structurally cannot give:
  // that gate hardcodes `crate_subs = ["rust", "wasm"]` and runs a HOST `cargo check` with no
  // `--target`, so the wasip2 component crate is invisible to it — the reason the `ALL_PROFILES`
  // component row filters out of it rather than flowing into it. The `local`-tier build smoke
  // compiles five representative fixtures; this asks the same question of all 89, and the answer
  // differs. `check` rather than `build`: the link is already asserted on the representative
  // fixtures, and the class this breadth catches — glue naming a trait, method or macro the bindings
  // never minted — is a type-check failure. A member of the batch on the same terms as its
  // neighbours: `#[ignore]`d, `cmd`-shaped, and owner of a flocked scratch root nothing else
  // touches. Measured 140 s cold / 97 s warm (a warm run still pays generation per cell).
  { id: "component_corpus_compiles", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "component_corpus_compiles", "--", "--ignored", "--nocapture"],
    ignoredTest: "component_corpus_compiles",
    desc: "component face at corpus breadth: cargo check --target wasm32-wasip2 per fixture (manual, #[ignore]d)" },
  // The same question over an enumeration NOBODY CURATED for the component face: every drivable row
  // of the decode catalog, generated component-only and BUILT for wasip2. It sits beside its corpus
  // sibling above rather than replacing it because the two enumerations are independent by
  // construction — the corpus is hand-authored fixtures, this is the matrix's own row set — and a
  // hand-picked compile corpus cannot be assumed to cover a class just because the class is
  // ordinary: both component-glue compile classes closed in 2026-08 sat in ordinary matrix rows no
  // fixture spelled, and this loop is the by-hand procedure that found them. It has since found a
  // third instance nobody was looking for.
  //
  // `build` rather than `check` because the wasip2 LINK is what the emitted rlib-only manifest makes
  // possible, and the sweep is the only place that link is asserted at breadth.
  //
  // Rows that generate and do not build are held BOTH WAYS by `SWEEP_EXPECTED_BUILD_FAIL` in
  // verify.ts — a listed row that starts building (or stops generating) fails as a stale entry, an
  // unlisted one that stops building fails as a new finding. Each entry is an open defect ledgered
  // in `cddl-matrix/roadmap.toml` § "Findings — open", never a decision to stop looking.
  //
  // Bun, not cargo, and no `#[ignore]` test behind it: the enumeration is the committed catalog,
  // which only the matrix scripts parse. It needs no CDDL oracle (its per-row verdict is a cargo
  // exit code), so unlike `verify` it is a plain `cmd` gate and a member of the batch on the same
  // terms as its neighbours. `script:` is deliberately absent — meta-check 2's verify.ts mapping
  // belongs to the `verify` gate, as it does for `verify_selftest`. Per-cell nested cargo, memoized
  // by the gate cache under its own label. Measured on the delivering machine: 3 m 3 s cold (every
  // cell builds; the first pays the shared wasip2 dependency graph, then ~1 s each), 2 m 6 s warm —
  // a warm run still pays generation, the lockfile preflight and the tree hash per cell.
  { id: "component_build_sweep", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["bun", "run", "verify.ts", "--component-build-sweep"], cwd: MATRIX,
    desc: "component face at matrix breadth: generate + wasip2 BUILD every drivable decode-catalog row, with non-building rows held both ways by verify.ts's SWEEP_EXPECTED_BUILD_FAIL (manual)" },
  // The compile half of the corpus-wide user-EDIT regen leg, split from its `local` sibling because
  // its cost class is nested cargo per fixture rather than a generator subprocess. It is the only
  // gate that asks whether a crate regenerated over a `cddl-codegen:replace` block still BUILDS
  // warning-clean — the orphaned-`use` class is a rustc WARNING, invisible to any assertion about
  // generation exiting 0. Gate-cached per generated-crate content hash, and a member of the batch on
  // the same terms as its neighbours: `#[ignore]`d, `cmd`-shaped, owner of a flocked scratch root
  // nothing else touches.
  // The wrapper-participation grid's compile/link floors. Two gates rather than one because their
  // SUBJECTS differ: the first links a CONSUMER (plus the committed wasm-clean dep pair) for
  // wasm32-unknown-unknown, which is the only place two `#[wasm_bindgen]` classes of one name fail;
  // the second checks the HOST crate a `--wrapper-requests` run produces, whose mints come from a
  // sidecar rather than from its own spec. Both are `#[ignore]`d, `cmd`-shaped, gate-cached per
  // generated-crate content hash, and own flocked scratch roots nothing else touches — the batch's
  // membership terms.
  { id: "wrapper_participation_floors", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "wrapper_participation_mode_floors", "--", "--ignored", "--nocapture"],
    ignoredTest: "wrapper_participation_mode_floors",
    desc: "wrapper-participation grid: per-mode compile/link floors (local cargo check + index/workspace wasm32 link) (manual, #[ignore]d)" },
  { id: "wrapper_participation_host_floor", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "wrapper_participation_requested_host_floor", "--", "--ignored", "--nocapture"],
    ignoredTest: "wrapper_participation_requested_host_floor",
    desc: "wrapper-participation grid: requested-hosted floor — cargo check of the HOST crate a --wrapper-requests run emits (manual, #[ignore]d)" },
  { id: "regen_over_prior_output_corpus_compiles", tier: "full", kind: "cmd", concurrent: MANUAL_HEAVY,
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "regen_over_prior_output_corpus_compiles", "--", "--ignored", "--nocapture"],
    ignoredTest: "regen_over_prior_output_corpus_compiles",
    desc: "regen over a user EDIT at corpus breadth: cargo check the regenerated rust+wasm crates, unused-import/variable clean (manual, #[ignore]d)" },
  { id: "verify", tier: "full", kind: "fn", run: runVerify, script: "verify.ts",
    desc: "cddl-matrix mechanical verify gate (oracle preflight + probe every feature)" },
  // Registered AFTER `verify` so a `full --cache-transparency` run warms the cache via `verify` first,
  // making this gate's cached run A genuinely hit-heavy (registry execution is sequential).
  { id: "verify_cache_transparency", tier: "full", kind: "fn", run: runCacheTransparency, script: "cache_transparency.ts",
    requires: [{ gate: "verify", why: "it audits a cache that gate warms in the same run; alone it fails its >=1-hit vacuity floor, or passes against a stale cache" }],
    desc: "gate-cache OUTPUT-side soundness: verify.ts annotations + report byte-identical cached vs GATE_CACHE=0 (flag-gated --cache-transparency)" },
  { id: "gate_cache_closure_audit", tier: "full", kind: "fn", run: runClosureAudit, script: "audit_gate_cache_closure.ts",
    desc: "gate-cache KEY-side soundness: strace input-closure audit of a cached gate (default multifile_matrix_compiles, CLOSURE_AUDIT_GATE overrides; SKIPPED if strace absent)" },
  { id: "corpus_detect", tier: "full", kind: "cmd", cmd: ["bun", "run", "corpus_detect.ts"], cwd: MATRIX,
    script: "corpus_detect.ts", desc: "corpus_detect featuresIn/rolesIn self-checks" },
  // The tier's ONE deliberately-online gate (warm-up→offline covers every other gate): the whole
  // question is "does the REMOTE serve this rev", which a warm local cargo/git DB answers wrongly
  // and confidently — exactly how the 2026-08 phantom pin passed three cycles of green gates.
  // No gate cache on purpose: its verdict can change with zero tree change (deleted/force-pushed
  // branch). Environment failures exit 2 with text that says "not a pin defect"; see the script
  // header for the two-step probe that keeps the failure classes structurally separate.
  { id: "pin_cold_fetch", tier: "full", kind: "cmd",
    cmd: ["bun", "run", "pin_cold_fetch.ts"], cwd: MATRIX,
    script: "pin_cold_fetch.ts",
    desc: "every git rev asserted by a committed pin-carrying file resolves from its REMOTE (deliberately online; the phantom-pin class)" },
  { id: "fuzz_compile_rot", tier: "full", kind: "fn", run: runFuzz,
    desc: "fuzz crate compile-rot check (generate.sh iff needed, then cargo check)" },
  // AFTER the compile-rot gate on purpose: a crate that does not COMPILE should be reported as
  // compile rot, not as a fuzz run that failed to build. No `requires:` edge — both gates provision
  // `fuzz/generated` themselves (see `ensureFuzzGenerated`), so a `--only fuzz_bounded_run` run
  // asserts exactly what a whole-tier one does rather than passing vacuously.
  { id: "fuzz_bounded_run", tier: "full", kind: "fn", run: runFuzzBoundedRun,
    desc: "bounded libFuzzer RUN of both fuzz targets (FUZZ_BUDGET_S seconds each, default 120; not gate-cached)" },

  // --- tracked-failing IOU stubs: known-failing #[ignore] tests, NEVER executed (shown as STUB) ---
  // (none — the float-under-preserve IOU was delivered; `preserve_encodings_supports_floats` is now
  // an ordinary always-on test.)
];

// ==================================================================================================
// GATE SELECTION (`--only`) — and the fence that keeps a partial run from reading as a tier run
// ==================================================================================================
// The case it serves is structural: the two longest gates in the registry are BARRIERS at the end of
// the `full` tier (one order-constrained, one trace-purity-constrained), so a run that dies or
// fail-fasts leaves exactly them unrun, and covering them costs a whole tier again.
//
// What makes it shippable is that the falsified-claim class ("tier green" manufactured from a partial
// run) is UNREPRESENTABLE in the output rather than policed by discipline. Five closures, each
// against a way a partial run could pass for a whole one:
//   1. the full registry still prints, with deselected in-tier gates in their OWN status word;
//   2. the SUMMARY header itself carries the partiality, so a quoted table cannot lose it;
//   3. the self-log is `check-only-<stamp>.log` — outside `check-(fast|local|full)-`, so the run is
//      excluded BY CONSTRUCTION from tier medians, `--backfill` tier attribution and per-tier
//      retention (per-GATE rows are still emitted: those remain legitimate measurements);
//   4. dependency-splitting selections are refused, naming the prerequisite (`requires`);
//   5. the final line is a paste-able receipt that cannot state a verdict without stating the
//      omission — `RESULT: PASS — all in-tier gates green` stays reserved for complete tiers.
export type SelectionResult = { ok: true; ids: Set<string> } | { ok: false; message: string };

/**
 * Resolve a `--only` selection against a tier, or refuse with the message to print.
 *
 * Selection is among the gates OF THE RUN'S TIER (v1 scope): `full --only verify` selects within
 * full's in-tier set, and naming a gate outside it is a refusal that says which tier to run instead.
 * That keeps one meaning for "in-tier", which every other part of the summary is written against.
 */
export function resolveSelection(only: string[], tier: Tier, registry: Gate[] = REGISTRY): SelectionResult {
  const runnable = registry.filter(g => g.kind !== "stub");
  const known = new Map(runnable.map(g => [g.id, g]));
  if (only.length === 0)
    return { ok: false, message: `check.ts: --only needs at least one gate id (known: ${runnable.map(g => g.id).join(", ")})` };
  const ids = new Set<string>();
  for (const id of only) {
    const g = known.get(id);
    if (!g)
      return { ok: false, message: `check.ts: --only: unknown gate '${id}' (known: ${runnable.map(x => x.id).join(", ")})` };
    if (rank(g.tier) > rank(tier))
      return {
        ok: false,
        message: `check.ts: --only: gate '${id}' is not in the '${tier}' tier — it is a '${g.tier}'-tier gate. ` +
          `Run \`bun run check.ts ${g.tier} --only ${only.join(",")}\`.`,
      };
    ids.add(id);
  }
  for (const id of ids) {
    for (const req of known.get(id)!.requires ?? []) {
      if (ids.has(req.gate)) continue;
      return {
        ok: false,
        message: `check.ts: --only: gate '${id}' requires '${req.gate}' in the same run — ${req.why}. ` +
          `Select both (\`--only ${[...ids, req.gate].join(",")}\`) or drop '${id}'.`,
      };
    }
  }
  return { ok: true, ids };
}

// ==================================================================================================
// RUN
// ==================================================================================================
function fmtDur(ms: number): string {
  if (ms < 1000) return `${Math.round(ms)}ms`;
  const s = ms / 1000;
  if (s < 60) return `${s.toFixed(1)}s`;
  const m = Math.floor(s / 60);
  return `${m}m ${Math.round(s - m * 60)}s`;
}

function statusCell(o: Outcome): string {
  switch (o.status) {
    case "PASS": return "PASS";
    case "FAIL": return "FAIL" + (o.reason ? ` (${o.reason})` : "");
    case "SKIPPED": return "SKIPPED" + (o.reason ? ` (${o.reason})` : "");
    case "STUB": return "STUB (tracked failing)";
    case "NOT_IN_TIER": return "not-in-tier";
    case "NOT_RUN_ONLY": return "NOT RUN (--only)";
  }
}

// ---- gate execution ------------------------------------------------------------------------------
export interface GateOutcome { gate: string; out: Outcome; ms: number }

/**
 * A gate on its own, exactly as every gate ran before this feature: output INHERITED straight to the
 * shared stdout, `CDDL_TIMING_GATE` set on this process. Unchanged byte for byte, because a gate that
 * declares no concurrency group must keep today's behaviour — the default is sequential.
 */
function runGateSequential(g: Gate, opts: Opts, timingCells: boolean): GateOutcome {
  console.log(`\n=== [${g.tier}] ${g.id} — ${g.desc} ===`);
  // A sequential gate needs the memory bound just as much as a batched one — arguably more, since
  // `CHECK_JOBS=1` routes EVERY gate through here. Without this the nested cargo a gate spawns runs
  // at cargo's `-j $(nproc)` default, so the "safest" setting was the least bounded one: the batch
  // path capped its children while the sequential path handed each gate the whole machine. The repo
  // `.cargo/config.toml` floor does not reach these, because a gate's nested cargo runs in a scratch
  // directory outside the repo and cargo discovers config by walking up from the CWD; an exported
  // environment variable is what crosses that boundary.
  //
  // `gatesInFlight: 1` because that is the truth here — one gate alone may spend the whole budget.
  const solo = cargoJobsForBatch({
    gatesInFlight: 1,
    memTotalGiB: memTotalGiB(),
    memAvailGiB: availGiB("mem"),
    override: process.env.CHECK_CARGO_JOBS,
    inherited: process.env.CARGO_BUILD_JOBS,
  });
  process.env.CARGO_BUILD_JOBS = String(solo.jobs);
  // The child-count half of the same bound (see `nestedToolPermitsForGate`): without it, a
  // `cargo test` gate's nested spawns run as concurrently as libtest has threads, and the `-j`
  // above multiplies by that count instead of standing alone.
  const soloPermits = nestedToolPermitsForGate({
    override: process.env.CHECK_NESTED_PERMITS,
    inherited: process.env.CDDL_NESTED_TOOL_PERMITS,
  });
  process.env.CDDL_NESTED_TOOL_PERMITS = String(soloPermits.permits);
  // The REGISTRY gate a cell belongs to — knowable only here. The emitter labels the Rust side
  // has (`feature_corpus_compiles`, …) are three-quarters cell names inside the `test` gate, so a
  // cell row must not present one as a gate id; it carries both, and this is the half that is true.
  if (timingCells) process.env.CDDL_TIMING_GATE = g.id;
  const t0 = performance.now();
  let out: Outcome;
  if (g.kind === "cmd") {
    const e = sh(g.cmd!, g.cwd ?? ROOT);
    out = e === 0 ? { status: "PASS" } : { status: "FAIL", reason: `exit ${e}` };
  } else {
    out = g.run!(opts);
  }
  const ms = performance.now() - t0;
  console.log(`--- ${g.id}: ${out.status}${out.reason ? ` (${out.reason})` : ""}  [${fmtDur(ms)}]`);
  return { gate: g.id, out, ms };
}

/**
 * A batched gate: output CAPTURED and emitted as one block when it finishes.
 *
 * Buffering is not cosmetic. Interleaved output from concurrent cargo processes is unreadable, and it
 * would break the log as a data source: `project_timings.ts`'s parser attributes each
 * `<n> run, <m> cached` rollup to the `=== [tier] <gate> — …` section it appears under, so a rollup
 * line landing under a different gate's header would be a wrong measurement rather than a missing
 * one. One atomic block per gate keeps every rollup inside its own section whatever order the gates
 * finish in.
 *
 * `CDDL_TIMING_GATE` and `CARGO_BUILD_JOBS` go to the CHILD's environment, never `process.env`: the
 * Rust emitter reads the former per row, so one mutable copy on the parent would label every
 * concurrent gate's cells with whichever gate started last — and the latter must not reach the
 * SEQUENTIAL path, which stays byte-identical to its pre-concurrency behaviour (one cargo at
 * `-j nproc` is the historical shape and is not what overcommitted the machine).
 *
 * Both pipes are drained CONCURRENTLY with the exit wait. Awaiting `exited` first deadlocks the
 * instant a gate outgrows the pipe buffer — and these gates emit megabytes — which is the same
 * hang-after-the-work-succeeded shape the join guard exists for, reached through the child instead.
 * Chunks from the two streams append to one buffer, so a gate's stderr stays roughly in place
 * relative to its stdout instead of being appended wholesale after it.
 */
async function runGateBuffered(
  g: Gate, timingCells: boolean, live: Map<string, Bun.Subprocess>, cargoJobs: number,
  nestedPermits: number,
): Promise<GateOutcome & { text: string }> {
  const t0 = performance.now();
  const child = Bun.spawn(g.cmd!, {
    cwd: g.cwd ?? ROOT,
    env: {
      ...process.env,
      // The bound propagates by INHERITANCE to every cargo this gate spawns, nested ones included:
      // the gate is a `cargo test` whose test body spawns a fresh `cargo` per generated crate, and
      // that grandchild reads the same environment.
      CARGO_BUILD_JOBS: String(cargoJobs),
      // Its sibling: how many such nested children the gate may hold open AT ONCE (read by
      // `tool_cmd` in src/tests/integration_tests.rs). `-j` divides one child's compilers; this
      // bounds the child count that `-j` silently multiplied by.
      CDDL_NESTED_TOOL_PERMITS: String(nestedPermits),
      ...(timingCells ? { CDDL_TIMING_GATE: g.id } : {}),
    },
    stdout: "pipe",
    stderr: "pipe",
    // Never `inherit`: several concurrent children sharing one stdin is a race over a resource none
    // of these gates reads.
    stdin: "ignore",
  });
  live.set(g.id, child);
  const chunks: Uint8Array[] = [];
  const pump = async (s: ReadableStream<Uint8Array>): Promise<void> => {
    for await (const c of s) chunks.push(c);
  };
  try {
    const [, , code] = await Promise.all([pump(child.stdout), pump(child.stderr), child.exited]);
    const ms = performance.now() - t0;
    let n = 0;
    for (const c of chunks) n += c.length;
    const buf = new Uint8Array(n);
    let at = 0;
    for (const c of chunks) { buf.set(c, at); at += c.length; }
    let text = new TextDecoder().decode(buf);
    if (text.length && !text.endsWith("\n")) text += "\n";
    return {
      gate: g.id,
      out: code === 0 ? { status: "PASS" } : { status: "FAIL", reason: `exit ${code}` },
      ms,
      text,
    };
  } finally {
    live.delete(g.id);
  }
}

/**
 * Execute an already-tier-filtered, non-stub gate list in registry order, overlapping only the gates
 * that declared they may overlap.
 *
 * Exported as the single execution path so nothing can measure or exercise a *different* runner than
 * the one a tier uses.
 *
 * FAIL-FAST WITH GATES IN FLIGHT, decided here and visible in the returned statuses: a failure stops
 * the pool from starting anything NEW, gates already running finish and report their real verdicts
 * (a PASS earned under load is a PASS, and discarding minutes of finished work to report it as
 * nothing would be strictly less honest), and every gate that never started comes back
 * `SKIPPED (earlier failure; fail-fast)` — so "ran and failed", "ran and passed", and "never ran"
 * remain three distinguishable things in the summary table, which is the whole contract of the
 * always-printed registry summary.
 */
export async function runGates(o: {
  gates: Gate[];
  opts: Opts;
  keepGoing: boolean;
  jobs: number;
  joinTimeoutMs?: number;
  hint?: (id: string) => number | undefined;
  timingCells?: boolean;
  /** Called as each gate finishes, in COMPLETION order — a run killed mid-batch keeps its rows. */
  onDone?: (r: GateOutcome) => void;
}): Promise<GateOutcome[]> {
  const hint = o.hint ?? digestHint();
  const joinTimeoutMs = o.joinTimeoutMs ?? parseJoinTimeoutMs(process.env.CHECK_JOIN_TIMEOUT_S);
  const timingCells = o.timingCells ?? false;
  const done = new Map<string, GateOutcome>();
  const record = (r: GateOutcome): void => { done.set(r.gate, r); o.onDone?.(r); };
  let aborted = false;

  for (const batch of planBatches(o.gates)) {
    if (aborted) break;

    if (batch.gates.length === 1) {
      const r = runGateSequential(batch.gates[0]!, o.opts, timingCells);
      record(r);
      if (r.out.status === "FAIL" && !o.keepGoing) aborted = true;
      continue;
    }

    const order = longestFirst(batch.gates, hint);
    const jobs = Math.max(1, Math.min(o.jobs, order.length));
    // Computed per batch, from the gates ACTUALLY in flight — a batch of two overlapping gates may
    // spend more `-j` each than a batch of four, because the product is what must stay bounded.
    const cargo = cargoJobsForBatch({
      gatesInFlight: jobs,
      memTotalGiB: memTotalGiB(),
      // Re-measured HERE, not carried from the startup preflight: a full tier's last batch can begin
      // an hour after its first, on a machine whose free memory has moved since — and the startup
      // reading being stale in the optimistic direction is precisely the case that hurts.
      memAvailGiB: availGiB("mem"),
      override: process.env.CHECK_CARGO_JOBS,
      inherited: process.env.CARGO_BUILD_JOBS,
    });
    if (cargo.warning) console.log(`>>> check.ts: ${cargo.warning}`);
    const permits = nestedToolPermitsForGate({
      override: process.env.CHECK_NESTED_PERMITS,
      inherited: process.env.CDDL_NESTED_TOOL_PERMITS,
    });
    if (permits.warning) console.log(`>>> check.ts: ${permits.warning}`);
    console.log(
      `\n>>> parallel batch: ${order.length} gate(s) in group '${batch.group}', jobs=${jobs}` +
      `, CARGO_BUILD_JOBS=${cargo.jobs} per gate [${cargo.why}]` +
      `, CDDL_NESTED_TOOL_PERMITS=${permits.permits} per gate [${permits.why}]` +
      ` (dispatch order, slowest measured first: ${order.map(g => g.id).join(", ")})`,
    );
    const live = new Map<string, Bun.Subprocess>();
    await runPool(
      order.map(g => ({ id: g.id, g })),
      jobs,
      async item => {
        console.log(`>>> ${item.id}: started`);
        const r = await runGateBuffered(item.g, timingCells, live, cargo.jobs, permits.permits);
        // ONE write: header, the gate's whole output, footer — nothing from another gate between.
        process.stdout.write(
          `\n=== [${item.g.tier}] ${item.g.id} — ${item.g.desc} ===\n${r.text}` +
          `--- ${item.g.id}: ${r.out.status}${r.out.reason ? ` (${r.out.reason})` : ""}  [${fmtDur(r.ms)}]\n`,
        );
        record({ gate: r.gate, out: r.out, ms: r.ms });
        return r;
      },
      {
        timeoutMs: joinTimeoutMs,
        stopAfter: r => r.out.status === "FAIL" && !o.keepGoing,
        // Reclaim by the handles this pool owns — never by a name pattern, which matches another
        // session's live cargo run as readily as this one's.
        onTimeout: pending => {
          console.log(
            `\n>>> check.ts BUG: the parallel join did not settle within ${fmtDur(joinTimeoutMs)} — ` +
            `still in flight: ${pending.map(p => p.id).join(", ") || "(none)"}. Killing them and ` +
            `continuing to the summary rather than hanging. Raise CHECK_JOIN_TIMEOUT_S if this was ` +
            `a genuinely slow run, and treat it as a runner defect otherwise.`,
          );
          for (const p of pending) live.get(p.id)?.kill();
        },
      },
    );
    // A never-started gate is left OUT of `done` deliberately: the sequential runner has always
    // reported a fail-fast skip without emitting a measurement row for it, and a 0 ms row would be a
    // measurement of nothing. The registry-order pass at the end of this function fills them in.
    if (batch.gates.some(g => !done.has(g.id) || done.get(g.id)!.out.status === "FAIL") && !o.keepGoing)
      aborted = true;
  }

  // Registry order out, with everything the abort never reached reported as never-run.
  return o.gates.map(g =>
    done.get(g.id) ?? { gate: g.id, out: { status: "SKIPPED", reason: "earlier failure; fail-fast" }, ms: 0 });
}

// ---- start-of-run ETA ----------------------------------------------------------------------------
// The one number an agent needs BEFORE committing to a run, and the reason the digest is committed
// at all: it has to be right in a fresh checkout on the first run, before any local ledger exists.
//
// The estimate is a RUN-LEVEL wall median, never a sum of per-gate medians. A sum omits inter-gate
// overhead, and it is unavailable for the runs that matter most — a killed run carries per-gate
// timings but no wall (deliberately: a killed run's gate sum is not a wall time), so summing gates
// would quietly build an estimate out of runs that never finished.

/** Coarse, ETA-shaped: `54s`, `5m32s`, `70m`. Deliberately blunter than fmtDur — this is a forecast. */
function fmtEta(ms: number): string {
  const s = Math.round(ms / 1000);
  if (s < 90) return `${s}s`;
  if (s < 600) return `${Math.floor(s / 60)}m${s % 60}s`;
  return `${Math.round(s / 60)}m`;
}

/**
 * The strongest cache-warmth counter-example this tier's OWN measurements support, or null.
 *
 * The claim being evidenced is "a warm cache does not shortcut this tier", and it is only printed
 * where the data shows it — never hardcoded, because a hardcoded pair rots exactly like the
 * hand-written wall times this feature exists to replace. `cells_run`/`cells_cached` live on GATE
 * rows, so a run's cache warmth is the sum over its gates; the wall comes from its RUN row.
 *
 * Three filters, each of which the claim needs: the run must have COMPLETED and passed (a killed run
 * has no wall), it must be cache-DOMINATED (more hits than misses — otherwise "warm" is a
 * misdescription), and, having picked the most cache-heavy such run, that run must have been no
 * faster than the tier's median. If the warmest run WAS fast, the claim is false for this tier and
 * nothing is printed. Selecting the warmest run first and testing it second is what keeps this
 * honest: picking the warmest run *among the slow ones* would be cherry-picking.
 */
function cacheCounterExample(rows: Row[], tier: Tier, etaMs: number): string | null {
  interface Agg { cached: number; misses: number; wall?: number; pass: boolean }
  const byRun = new Map<string, Agg>();
  for (const r of rows) {
    if (r.tier !== tier) continue;
    const k = r.log ?? r.run;
    const a = byRun.get(k) ?? { cached: 0, misses: 0, pass: false };
    if (r.kind === "gate") { a.cached += r.cells_cached ?? 0; a.misses += r.cells_run ?? 0; }
    else { a.wall = r.wall_ms; a.pass = r.verdict === "pass"; }
    byRun.set(k, a);
  }
  let warmest: Agg | null = null;
  for (const a of byRun.values()) {
    if (!a.pass || a.wall === undefined) continue;
    if (a.cached === 0 || a.cached <= a.misses) continue;
    if (!warmest || a.cached > warmest.cached) warmest = a;
  }
  if (!warmest || warmest.wall! < etaMs) return null;
  return `the most cache-heavy ${tier} run on record hit ${warmest.cached} cached cells ` +
    `(${warmest.misses} run) and still took ${compactDur(warmest.wall!)}`;
}

/**
 * Pure, so the no-baseline branch is pinned by a test rather than reached only in a fresh checkout.
 * The committed digest gates the estimate — it is the number a first run has — while the local
 * ledger, when there is one, contributes the observed SPREAD, which the digest does not store. Both
 * sources are named in the line, because they can legitimately disagree: the digest only moves when
 * the deadband trips, so a spread that no longer brackets the median is the deadband working.
 */
export function etaLines(digest: Digest, rows: Row[], tier: Tier): string[] {
  const row = digest.tiers.find(t => t.tier === tier);
  // Never a fabricated estimate: below the seeding floor the honest output is that there isn't one.
  if (!row || row.wall_ms === undefined || (row.n ?? 0) < 3) return [`  no baseline yet for tier=${tier}`];
  const w = tierWindow(rows, tier);
  const spread = w.length >= 3
    ? `; local ledger's last ${w.length} span ${compactDur(Math.min(...w))}-${compactDur(Math.max(...w))}`
    : "";
  const out = [`  expected ~${fmtEta(row.wall_ms)} (median of ${row.n} passing ${tier} runs, tests/timings.json${spread})`];
  const ce = cacheCounterExample(rows, tier, row.wall_ms);
  if (ce) out.push(`  a warm cache does NOT make this fast: ${ce}`);
  return out;
}

/** Never fatal: a malformed digest or a torn ledger must not turn a green run red. */
function printEta(tier: Tier): void {
  try {
    for (const l of etaLines(readDigest(), readLedger(), tier)) console.log(l);
  } catch (e) {
    console.log(`  no duration baseline available (${e instanceof Error ? e.message : String(e)})`);
  }
}

// ---- log retention -------------------------------------------------------------------------------
// `draft/logs/` grows ~11 MB/day and nobody reads a log twice. Compression was measured (gzip -9:
// 35.6x, zstd -19: 53.9x) and rejected: it preserves a hoard nobody reads. What is worth keeping is
// the DURATIONS, and those are scraped into `draft/timings.jsonl` — which is why the interlock below
// is not a formality.

/**
 * Basenames of `check-*.log` files cited by a COMMITTED file, or null if the scan itself failed.
 *
 * Anchored on the literal `check-` prefix for a measured reason: the natural unanchored form
 * `[A-Za-z0-9._-]+\.log` backtracks across the ~3900 tracked corpus files and takes ~51s, blowing
 * the "retention adds < 1s" budget by two orders of magnitude. With the prefix it is one pass in
 * ~0.16s. `draft/` is gitignored, so `git grep` never sees the logs themselves — only citations.
 *
 * Returns null (rather than an empty set) when git fails, so the caller FAILS CLOSED. An empty set
 * and a broken scan are indistinguishable at the call site, and the difference is deleted evidence.
 */
function citedLogBasenames(): Set<string> | null {
  const r = Bun.spawnSync(["git", "grep", "-hoE", "check-[A-Za-z0-9._-]+\\.log"], {
    cwd: ROOT, stdout: "pipe", stderr: "pipe",
  });
  if ((r.exitCode ?? 1) > 1) return null; // 1 == "no matches", which is a legitimate empty answer
  return new Set(new TextDecoder().decode(r.stdout).split("\n").map(s => s.trim()).filter(Boolean));
}

export interface Retention {
  status: "ran" | "skipped";
  reason?: string;
  deleted: string[];
  bytes: number;
  kept: number;
  citedKept: string[];
  /** Expired but held back because the ledger has no rows recovered from them. */
  unscrapedKept: string[];
}

/**
 * Keep the last `keepPerTier` `check-<class>-*.log` per class; delete the rest. Bounded and
 * predictable, which a time window is not: a quiet fortnight followed by a busy day should not
 * change how much history survives.
 *
 * Scope is deliberately narrow — only names matching `check-(fast|local|full|only)-*.log`.
 * `draft/logs/` also holds hand-written ad-hoc logs, some of which begin with `check-` but name
 * neither a tier nor a selection (`check-optional_fixed_float-crates-….log`); they were written
 * under other conventions and are left alone.
 *
 * `only` is a FOURTH keep-N class, not a tier: a `--only` run's log is deliberately named outside
 * the tier regex so no tier median can absorb it, and the flip side of that exclusion is that
 * nothing would ever delete it. One class per name shape keeps both properties.
 *
 * Two fail-closed interlocks, in the order that keeps the quiet case quiet: expiry candidates are
 * computed FIRST, and a run with nothing to expire (CI, a fresh checkout) returns silently without
 * asserting anything. Only once deletion is actually on the table must (a) the citation scan have
 * succeeded and (b) `draft/timings.jsonl` exist and be non-empty — the logs are the only copy of
 * the duration history until they have been scraped into it.
 *
 * That second interlock is necessary but NOT sufficient, so a third one is per-FILE: a non-empty
 * ledger proves some history was scraped, not that THIS log's was. Nothing otherwise connects the
 * file being unlinked to the rows meant to preserve it, and the gap is not hypothetical — until
 * structured emission lands, the ledger only grows when someone runs `--backfill` by hand, so every
 * run from here produces a log that reaches position 11 and is deleted with its durations never
 * captured. It does not close after emission lands either: if emission breaks, or a run dies before
 * writing its rows, a non-empty ledger still authorises deleting a log nothing captured. So an
 * expiry candidate absent from the ledger is KEPT — the failure direction is logs accumulating,
 * never history vanishing — and once emission makes scraping automatic the guard costs nothing.
 *
 * One escape hatch, or unscraped logs leak forever: a run that died before its first gate finished
 * yields NO rows at all, so it can never appear in the ledger. Such a log is deletable, and the
 * question "does this log carry any timings?" is asked of the parser that would have scraped it
 * rather than of a filename heuristic — the parser is the definition of what a row is, so the two
 * cannot drift apart. It is only asked of expiry candidates, which is ~1 per run in steady state.
 */
export const KEEP_LOGS_PER_TIER = 10;

export function retainLogs(o: {
  logsDir: string;
  ledger: string;
  cited: () => Set<string> | null;
  keepPerTier?: number;
  dryRun?: boolean;
}): Retention {
  const keep = o.keepPerTier ?? KEEP_LOGS_PER_TIER;
  const skip = (reason: string): Retention =>
    ({ status: "skipped", reason, deleted: [], bytes: 0, kept: 0, citedKept: [], unscrapedKept: [] });
  if (!existsSync(o.logsDir)) return { status: "ran", deleted: [], bytes: 0, kept: 0, citedKept: [], unscrapedKept: [] };

  const RE = /^check-(fast|local|full|only)-.+\.log$/;
  const byClass = new Map<string, { f: string; mtime: number; size: number }[]>();
  for (const f of readdirSync(o.logsDir)) {
    const m = RE.exec(f);
    if (!m) continue;
    const st = statSync(join(o.logsDir, f));
    const list = byClass.get(m[1]!) ?? [];
    list.push({ f, mtime: st.mtimeMs, size: st.size });
    byClass.set(m[1]!, list);
  }

  let kept = 0;
  const expired: { f: string; size: number }[] = [];
  for (const list of byClass.values()) {
    // mtime order, filename as a stable tiebreak: 21 of the logs on disk are hand-named and carry
    // no parseable stamp, so ordering by filename alone would rank them arbitrarily against the rest.
    list.sort((a, b) => a.mtime - b.mtime || (a.f < b.f ? -1 : 1));
    const cut = Math.max(0, list.length - keep);
    kept += list.length - cut;
    expired.push(...list.slice(0, cut));
  }
  if (expired.length === 0) return { status: "ran", deleted: [], bytes: 0, kept, citedKept: [], unscrapedKept: [] };

  const cited = o.cited();
  if (cited === null)
    return skip("the `git grep` citation scan failed — refusing to delete logs without the guard");
  if (!existsSync(o.ledger) || statSync(o.ledger).size === 0)
    return skip(
      "draft/timings.jsonl is missing or empty — these logs are the only copy of that history; " +
      "run `bun run project_timings.ts --backfill` in cddl-matrix/ before anything is deleted",
    );

  const scraped = new Set(readLedger(o.ledger).map(r => r.log).filter((s): s is string => !!s));
  /** Does this log carry timings the ledger should hold? Asked of the parser that would scrape it. */
  const carriesTimings = (f: string): boolean => {
    try {
      return (parseLog(readFileSync(join(o.logsDir, f), "utf8"), f)?.gates.length ?? 0) > 0;
    } catch {
      return true; // unreadable: assume it has something to lose, and keep it
    }
  };

  const deleted: string[] = [];
  const citedKept: string[] = [];
  const unscrapedKept: string[] = [];
  let bytes = 0;
  for (const e of expired) {
    // A committed doc citing a gitignored path is dangling-by-construction and cannot fail loudly,
    // so deleting the referent would make the citation wrong everywhere and detectably wrong nowhere.
    if (cited.has(e.f)) { citedKept.push(e.f); kept++; continue; }
    if (!scraped.has(e.f) && carriesTimings(e.f)) { unscrapedKept.push(e.f); kept++; continue; }
    if (!o.dryRun) unlinkSync(join(o.logsDir, e.f));
    deleted.push(e.f);
    bytes += e.size;
  }
  return { status: "ran", deleted, bytes, kept, citedKept, unscrapedKept };
}

function fmtBytes(b: number): string {
  if (b < 1024) return `${b} B`;
  if (b < 1024 ** 2) return `${(b / 1024).toFixed(1)} KB`;
  if (b < 1024 ** 3) return `${(b / 1024 ** 2).toFixed(1)} MB`;
  return `${(b / 1024 ** 3).toFixed(2)} GB`;
}

function printRetention(): void {
  const r = retainLogs({ logsDir: LOGS_DIR, ledger: LEDGER, cited: citedLogBasenames });
  if (r.status === "skipped") { console.log(`  retention: SKIPPED — ${r.reason}`); return; }
  // Naming the kept-because-cited logs keeps the guard's own workload visible: the guard is a bridge
  // for citations that should not exist, and a silent bridge is one nobody ever removes.
  if (r.citedKept.length)
    console.log(
      `  retention: WARNING — kept ${r.citedKept.length} expired log(s) cited by a committed file ` +
      `(${r.citedKept.join(", ")}); the fact belongs in the doc, not the path`,
    );
  // Not a warning — keeping is the correct outcome. It is printed because the backlog is the visible
  // cost of scraping being a manual step, and it should shrink to nothing once emission is automatic.
  if (r.unscrapedKept.length)
    console.log(
      `  retention: kept ${r.unscrapedKept.length} expired log(s) whose timings are not in the ledger yet ` +
      `— run \`bun run project_timings.ts --backfill\` in cddl-matrix/ to release them`,
    );
  if (r.deleted.length)
    console.log(`  retention: deleted ${r.deleted.length} check-*.log (${fmtBytes(r.bytes)} reclaimed), kept ${r.kept} (last ${KEEP_LOGS_PER_TIER} per tier, and per --only class)`);
}

// ---- structured emission ------------------------------------------------------------------------
// A run writes its own measurements to `draft/timings.jsonl` as it goes, so future runs never depend
// on re-parsing prose. The prose parser (`project_timings.ts`) STAYS as the fallback — it is what
// recovers a gate added without emission, or a run that died mid-line — but it is no longer the
// primary path, and none of the output below is visible in the log: prose is unchanged by design.
//
// THE COUPLING THAT MAKES THIS LOAD-BEARING: retention deletes an expired log only once the ledger
// holds a row naming THAT log (`retainLogs`, the per-file interlock). So every row emitted here
// carries `log` = the basename of the run's own log file, spelled exactly as the backfill parser
// spells it. Get that wrong and nothing fails — retention simply classifies every log as unscraped
// and holds it forever, which is the growth problem this feature exists to end, returning silently.

/**
 * The run's own log basename, or null when there is no self-log.
 *
 * `runSelfLogged` re-execs check.ts with `CHECK_SELF_LOG` set, and `main()` only ever runs in that
 * child — with one exception: `--help` runs `main()` directly with the variable unset (it prints and
 * exits before anything measurable). A direct `CHECK_SELF_LOG=… bun run check.ts` is honoured as-is.
 * Null means "emit nothing": a row that cannot name its log cannot release it from retention, and a
 * silently mis-keyed row is worse than an absent one.
 */
export function selfLogBasename(): string | null {
  const p = process.env.CHECK_SELF_LOG;
  return p ? basename(p) : null;
}

/** Run-scoped facts, resolved once at run start and stamped onto every row this run emits. */
export interface RunContext {
  log: string;
  run: string;
  tier: Tier;
  machine: string;
  commit?: string;
  dirty?: boolean;
  rustc?: string;
  gate_cache_enabled: boolean;
}

/** Mirrors `gate_cache.rs`'s `CacheConfig::from_env` — the cache class the rows are bucketed by. */
function gateCacheEnabled(): boolean {
  const v = (process.env.GATE_CACHE ?? "").toLowerCase();
  return v !== "0" && v !== "false";
}

/** Best-effort provenance: a missing git or rustc leaves the field absent, never a fabricated value. */
function gitFacts(): { commit?: string; dirty?: boolean } {
  try {
    const c = Bun.spawnSync(["git", "rev-parse", "--short", "HEAD"], { cwd: ROOT, stdout: "pipe", stderr: "ignore" });
    const s = Bun.spawnSync(["git", "status", "--porcelain"], { cwd: ROOT, stdout: "pipe", stderr: "ignore" });
    if ((c.exitCode ?? 1) !== 0 || (s.exitCode ?? 1) !== 0) return {};
    return { commit: c.stdout.toString().trim(), dirty: s.stdout.toString().trim().length > 0 };
  } catch { return {}; }
}

function rustcVersion(): string | undefined {
  try {
    const r = Bun.spawnSync(["rustc", "-V"], { stdout: "pipe", stderr: "ignore" });
    if ((r.exitCode ?? 1) !== 0) return undefined;
    return r.stdout.toString().trim().split(/\s+/)[1];
  } catch { return undefined; }
}

/**
 * `tier`/`run` are taken from the LOG NAME, not from argv, so a live row and a row the parser would
 * later recover from the same log agree by construction rather than by two code paths staying in
 * sync. The argv tier is only the fallback for a hand-set `CHECK_SELF_LOG` that names no tier.
 */
function runContext(tier: Tier): RunContext | null {
  const log = selfLogBasename();
  if (!log) return null;
  const named = splitLogName(log);
  return {
    log,
    run: named?.run ?? log.replace(/\.log$/, ""),
    tier: named?.tier ?? tier,
    machine: machineId(),
    ...gitFacts(),
    rustc: rustcVersion(),
    gate_cache_enabled: gateCacheEnabled(),
  };
}

/**
 * The run row is written TWICE — once at run start with neither `wall_ms` nor `verdict`, once at the
 * end with both — and the two converge on ONE ledger row because `rowKey` keys a run row on
 * `(log, "#run")`. The start write is what makes a killed run declare what it was (tier, commit,
 * dirty tree) instead of leaving only orphan gate rows; the end write is what gives the tier its
 * wall. A run that never reaches the end keeps the wall-less row, which every consumer already
 * treats as "no wall" — the same shape a killed run's backfilled row has.
 */
export function runRow(c: RunContext, done?: { wall_ms: number; verdict: "pass" | "fail" }): RunRow {
  return {
    v: 1, kind: "run", run: c.run, tier: c.tier, machine: c.machine, log: c.log, src: "run",
    ...(done ? { wall_ms: Math.round(done.wall_ms), verdict: done.verdict } : {}),
    commit: c.commit, dirty: c.dirty, rustc: c.rustc, gate_cache_enabled: c.gate_cache_enabled,
  };
}

/**
 * Field-for-field the backfill parser's gate row, so `--update` consumes both without a branch.
 *
 * `cells` arrives only at end of run, joined from the cell file by `(log, gate)` — the counts cannot
 * be known while the gate is still running. It is what keeps a live row from being strictly WORSE
 * than a backfilled one: `cacheCounterExample` reads `cells_cached` off gate rows, so without this
 * the ETA's warm-cache sentence could only ever be fed by pre-emission history and would fall silent
 * as that aged out.
 */
export function gateRow(
  c: RunContext, gate: string, status: "pass" | "fail" | "skipped", ms: number,
  cells?: { run: number; cached: number },
): GateRow {
  return {
    v: 1, kind: "gate", run: c.run, tier: c.tier, gate, status, ms: Math.round(ms),
    gate_cache_enabled: c.gate_cache_enabled,
    ...(cells ? { cells_run: cells.run, cells_cached: cells.cached } : {}),
    machine: c.machine, log: c.log, src: "run",
  };
}

/** Emission is never fatal: a full disk must not turn a green run red over a measurement. */
function emit(rows: Row[]): void {
  try { appendRows(rows, LEDGER); } catch { /* measurements are not a gate */ }
}

/**
 * End of run: collapse the run row, join the cell counts onto the gate rows, and trim both files.
 *
 * This is the only place check.ts rewrites the ledger rather than appending to it, and the failure
 * direction if a concurrent run appends during the read-modify-write is that run's LAST few gate rows
 * going missing — which retention's per-file interlock then reads as "not scraped" and keeps the log
 * for. Logs accumulating, never history vanishing, is the direction every interlock here fails in.
 *
 * Trimming lives HERE, in the path every run takes, rather than behind a `--trim` mode someone has to
 * remember: a maintenance step that depends on being remembered is the same rot the self-updating
 * digest exists to avoid. The interlock that makes it safe is in `keptRunKeys` — a run whose log is
 * still on disk is never trimmed out, because retention needs that row to release that log.
 */
function finalizeRun(
  c: RunContext, wallMs: number, verdict: "pass" | "fail",
  gates: { gate: string; status: "pass" | "fail" | "skipped"; ms: number }[],
  emitRunRow = true,
): void {
  try {
    const counts = cellCountsFor(readCells(CELLS), c.log);
    const merged = upsert(readLedger(LEDGER), [
      // A `--only` run emits NO run row: a run-level wall is a tier's wall, and a partial run has
      // none to give. Its per-GATE rows stay — those measure the same work a tier run measures, and
      // `windowFor` buckets them by (gate, cache class), never by tier. Everything the ledger and
      // retention key on is the log basename, which the gate rows carry, so a run row's absence
      // costs nothing but the number that would have been a lie.
      ...(emitRunRow ? [runRow(c, { wall_ms: wallMs, verdict })] : []),
      // Re-emitting every gate row the run already appended: same `(log, gate)` key, so each lands on
      // its own earlier row and gains the counts. A killed run keeps the count-less rows it appended.
      ...gates.map(g => gateRow(c, g.gate, g.status, g.ms, counts.get(g.gate))),
    ]);
    const logsOnDisk = new Set(existsSync(LOGS_DIR) ? readdirSync(LOGS_DIR) : []);
    const trimmed = trimRows(merged, keptRunKeys(merged, { logsOnDisk }));
    writeLedger(trimmed, LEDGER);
    if (existsSync(CELLS)) {
      const keepCells = new Set(runKeysInOrder(trimmed).slice(-KEEP_RUNS_IN_CELLS));
      const kept = trimCellLines(readFileSync(CELLS, "utf8").split("\n"), keepCells);
      writeFileSync(CELLS, kept.length ? kept.join("\n") + "\n" : "");
    }
  } catch { /* measurements are not a gate */ }
}

async function main() {
  const argv = process.argv.slice(2);
  const { flags, positional, only } = parseArgv(argv);
  const KNOWN = new Set(["--keep-going", "--skip-missing", "--refresh-fuzz", "--cache-transparency", "--only", "--help"]);
  for (const f of flags)
    if (!KNOWN.has(f)) { console.error(`check.ts: unknown flag '${f}' (known: ${[...KNOWN].join(", ")})`); process.exit(2); }
  if (flags.has("--help")) {
    console.log("usage: bun run check.ts [fast|local|full] [--keep-going] [--skip-missing] [--refresh-fuzz] [--cache-transparency] [--only <gate>[,<gate>]]");
    console.log("  bare invocation runs the `local` tier; CI runs `fast`. See the header of check.ts for details.");
    console.log("  --only runs a SUBSET of the tier's gates and is never a tier verdict: the full registry still");
    console.log("         prints (deselected gates as `NOT RUN (--only)`), the summary says PARTIAL, the log is");
    console.log("         draft/logs/check-only-<stamp>.log, and the last line is a receipt. Cite such a run as");
    console.log("         \"gates X, Y ran green\", never as a tier verdict.");
    console.log(`  env: CHECK_JOBS=<n>  gates in flight within a concurrency batch (default ${DEFAULT_JOBS}; 1 = fully sequential)`);
    console.log("       CHECK_CARGO_JOBS=<n>  CARGO_BUILD_JOBS given to each BATCHED gate (default: memory-derived, see check.ts)");
    console.log("       CHECK_NESTED_PERMITS=<n>  concurrent nested tool children per gate (default 1; read by tool_cmd via CDDL_NESTED_TOOL_PERMITS)");
    console.log("       CHECK_MEM_SAMPLER=0  disable the report-only per-run memory sampler");
    console.log(`       CHECK_SKIP_PREFLIGHT=1  skip the memory (${MEM_DEGRADE_FLOOR_GIB}/${MEM_REFUSE_FLOOR_GIB} GiB) and disk (${DISK_FLOOR_GIB} GiB) floors`);
    console.log("       CHECK_JOIN_TIMEOUT_S=<n>  runaway-hang guard on the parallel join (never a duration assertion)");
    process.exit(0);
  }
  let tier: Tier = "local";
  if (positional.length) {
    if (!TIERS.includes(positional[0] as Tier)) {
      console.error(`check.ts: unknown tier '${positional[0]}' (expected: ${TIERS.join(", ")})`);
      process.exit(2);
    }
    tier = positional[0] as Tier;
  }
  // Refusals BEFORE anything expensive, and before the ETA/retention chatter: a mistyped selection
  // must cost nothing and must say what the legal spellings are.
  let selected: Set<string> | null = null;
  if (only) {
    const r = resolveSelection(only, tier);
    if (!r.ok) { console.error(r.message); process.exit(2); }
    selected = r.ids;
  }
  const keepGoing = flags.has("--keep-going");
  const opts: Opts = { skipMissing: flags.has("--skip-missing"), refreshFuzz: flags.has("--refresh-fuzz"), cacheTransparency: flags.has("--cache-transparency") };
  const { jobs: requestedJobs, warning: jobsWarning } = parseJobs(process.env.CHECK_JOBS);
  if (jobsWarning) console.log(`check.ts: ${jobsWarning}`);
  // Deliberately BEFORE the preflight: space this run recovers is space the disk floor should count,
  // so a machine whose only problem is last week's debris starts instead of refusing.
  printScratchSweep();
  // Before anything expensive: a tier commits to its peak resource in its first seconds and cannot
  // discover a memory cap mid-run.
  const jobs = resourcePreflight(tier, requestedJobs);

  console.log(`\ncheck.ts — tier=${tier}  jobs=${jobs}${keepGoing ? " --keep-going" : ""}${opts.skipMissing ? " --skip-missing" : ""}${opts.refreshFuzz ? " --refresh-fuzz" : ""}${opts.cacheTransparency ? " --cache-transparency" : ""}${selected ? ` --only ${only!.join(",")}` : ""}`);

  // Both of these run BEFORE the warm-up fetch, and that ordering is the point of the ETA: the
  // warm-up can hang on a bad network for minutes and is the most likely place a run is interrupted,
  // so an estimate printed after it is an estimate the interrupted agent never saw.
  // A selected run gets NO tier ETA: the tier's median wall is the duration of a run this one is not.
  if (selected) console.log(`  --only: ${selected.size} selected gate(s) — no tier ETA, because a selected run is not a tier run`);
  else printEta(tier);
  printRetention();

  // Deliberately AFTER retention. Emission creates the ledger if it is absent, and retention's global
  // interlock ("refuse to delete until a backfill has run") reads exactly that absence — so emitting
  // first would let check.ts bootstrap a one-row ledger and quietly weaken the guard on a checkout
  // whose logs have never been scraped. Still before the warm-up, which is where a run is most
  // likely to be interrupted.
  const ctx = runContext(tier);
  if (ctx) {
    // No run row for a selected run, in either write (see `finalizeRun`): the row exists to give a
    // tier its wall, and a partial run has no tier wall to give.
    if (!selected) emit([runRow(ctx)]);
    // Cell rows: the drill-down under a gate row. Plumbed by env var because the Rust side emits
    // from inside libtest, where a printed marker is captured and discarded (see
    // `src/tests/timing_cells.rs`). Set only when this run is already emitting, so a suite run by
    // hand — and CI, and `--help` — stays byte-for-byte what it was.
    process.env.CDDL_TIMING_CELLS = CELLS;
    // The run scoping a cell row cannot know for itself. Without the log basename its rows can be
    // tied back to this run only by guessing at timestamp windows, and the end-of-run join that
    // feeds `cells_cached` onto the gate rows has nothing to key on.
    process.env.CDDL_TIMING_LOG = ctx.log;
  }

  warmupThenOffline(tier);

  const results = new Map<string, { out: Outcome; ms: number }>();
  // The gates that actually executed and appended a row, in order — the exact set finalizeRun
  // re-emits with cell counts joined on. Not derivable from `results`, which also holds the stub and
  // not-in-tier gates that never printed a line and never emitted anything.
  const emitted: { gate: string; status: "pass" | "fail" | "skipped"; ms: number }[] = [];
  const wall0 = performance.now();

  // Report-only; started this late so a preflight refusal or a `--help` costs zero samples, and
  // stopped (with its summary printed) before the registry table so the numbers land in every log.
  const sampler = process.env.CHECK_MEM_SAMPLER === "0" ? null : startMemSampler();

  const inTier: Gate[] = [];
  let deselected = 0;
  for (const g of REGISTRY) {
    if (g.kind === "stub") { results.set(g.id, { out: { status: "STUB" }, ms: 0 }); continue; }
    if (rank(g.tier) > rank(tier)) { results.set(g.id, { out: { status: "NOT_IN_TIER" }, ms: 0 }); continue; }
    // In the tier, left out by the selection — its OWN status word, so the table cannot present a
    // deliberate omission as an incidental skip or as a gate this tier never asks for.
    if (selected && !selected.has(g.id)) {
      results.set(g.id, { out: { status: "NOT_RUN_ONLY" }, ms: 0 });
      deselected++;
      continue;
    }
    inTier.push(g);
  }

  for (const r of await runGates({
    gates: inTier,
    opts,
    keepGoing,
    jobs,
    timingCells: ctx !== null,
    // Emitted as each gate finishes, beside its prose line, and for the same reason it is printed
    // there: a run killed at gate 30 of 42 keeps 30 measurements. Fail-fast skips are excluded — a
    // gate that never ran has no duration to record, and a 0 ms row would be a measurement of nothing.
    onDone: r => {
      if (!ctx) return;
      const status = r.out.status === "PASS" ? "pass" : r.out.status === "FAIL" ? "fail" : "skipped";
      emitted.push({ gate: r.gate, status, ms: r.ms });
      emit([gateRow(ctx, r.gate, status, r.ms)]);
    },
  })) results.set(r.gate, { out: r.out, ms: r.ms });

  if (sampler) reportMemPeaks(sampler.stop(), tier);

  // ---- always-printed full-registry summary --------------------------------------------------
  const wall = performance.now() - wall0;
  const idW = Math.max(...REGISTRY.map(g => g.id.length), 4);
  const tierW = 5;
  const line = "-".repeat(idW + tierW + 10 + 40);
  console.log("\n" + line);
  // The partiality rides in the HEADER, not only in the trailing lines: summary tables get pasted
  // without what follows them, and a selected run's PASS rows are otherwise indistinguishable from a
  // tier run's.
  console.log(selected
    ? `SUMMARY (PARTIAL — --only ${only!.join(",")}) — tier=${tier}  wall=${fmtDur(wall)}`
    : `SUMMARY — tier=${tier}  wall=${fmtDur(wall)}`);
  console.log(line);
  console.log(`${"GATE".padEnd(idW)}  ${"TIER".padEnd(tierW)}  ${"TIME".padEnd(8)}  STATUS`);
  console.log(line);
  for (const g of REGISTRY) {
    const r = results.get(g.id)!;
    const time = r.out.status === "PASS" || r.out.status === "FAIL" || (r.out.status === "SKIPPED" && r.ms > 0)
      ? fmtDur(r.ms) : "-";
    console.log(`${g.id.padEnd(idW)}  ${g.tier.padEnd(tierW)}  ${time.padEnd(8)}  ${statusCell(r.out)}`);
  }
  console.log(line);

  const fails = REGISTRY.filter(g => results.get(g.id)!.out.status === "FAIL").map(g => g.id);

  // Before the digest refresh, never after: `--update` reads the ledger, and this run's own wall is
  // the newest point in the tier window it is about to take a median over.
  if (ctx) finalizeRun(ctx, wall, fails.length ? "fail" : "pass", emitted, !selected);

  // ---- refresh the measured-duration digest, and the spans DERIVED from it ---------------------
  // Runs on EVERY invocation, including a failing one: the rule is a median over a few thousand
  // local rows, so it costs sub-millisecond and there is no reason to defer it behind a bless step
  // that would rot. It is silent unless a deadband tripped, and it can never turn a green run red —
  // durations are nondeterministic, and a failure to write a number is not a failure of the repo.
  //
  // The digest and the tier-time spans it derives (`tests/README.md`'s tier table, projected by
  // `project_status_headers.ts`) are ONE commit or neither: a re-measurement committed alone leaves
  // the NEXT tier run to fail-fast on the `project_status_headers` gate, which cost a full tier
  // iteration three times in one session before the pair was joined here. So a changed digest
  // regenerates the spans in the same flow, and the hint names every file the pair touched.
  let digestChanged = false;
  try {
    digestChanged = runDigestUpdate().changed;
  } catch (e) {
    console.log("check.ts: timings digest refresh failed (non-fatal — durations are never a gate): " +
      (e instanceof Error ? e.message : String(e)));
  }
  if (digestChanged) {
    if (sh(["bun", "run", "project_status_headers.ts", "--write"], MATRIX) === 0)
      console.log("timings: commit tests/timings.json together with the spans it derives — " +
        "tests/README.md, cddl-matrix/README.md");
    else
      console.log("check.ts: STALE SPANS — tests/timings.json was rewritten but " +
        "`project_status_headers.ts --write` failed; run it by hand before committing, or the next " +
        "tier run fail-fasts on the `project_status_headers` gate (non-fatal here — durations are never a gate)");
  }

  // A SELECTED run's last line is the paste-able receipt, and it replaces the verdict rather than
  // merely omitting it: counts and names are fused, so the sentence cannot state what ran without
  // stating what did not. `RESULT: PASS — all in-tier gates green` stays reserved for complete
  // tiers — it is the string AGENTS.md and the timings parser both read as a tier verdict.
  if (selected) {
    const sel = inTier.map(g => ({ id: g.id, status: results.get(g.id)!.out.status }));
    const passed = sel.filter(s => s.status === "PASS").map(s => s.id);
    const failed = sel.filter(s => s.status === "FAIL").map(s => s.id);
    const skipped = sel.filter(s => s.status !== "PASS" && s.status !== "FAIL").map(s => s.id);
    const notes = [
      ...(failed.length ? [`${failed.length} FAILED: ${failed.join(", ")}`] : []),
      ...(skipped.length ? [`${skipped.length} SKIPPED: ${skipped.join(", ")}`] : []),
    ];
    const g = gitFacts();
    const at = g.commit ? `${g.commit}${g.dirty ? "-dirty" : ""}` : "unknown-commit";
    console.log(
      `check.ts --only ${only!.join(",")} @ ${at}: ${passed.length}/${sel.length} selected PASS` +
      `${notes.length ? ` (${notes.join("; ")})` : ""}; ${deselected} in-tier gates NOT RUN — no tier verdict`,
    );
    process.exit(fails.length ? 1 : 0);
  }

  if (fails.length) {
    console.log(`RESULT: FAIL — ${fails.length} gate(s) failed: ${fails.join(", ")}`);
    process.exit(1);
  }
  console.log(`RESULT: PASS — all in-tier gates green (tier=${tier})`);
  process.exit(0);
}

// ---- self-logging: every run tees its FULL output to draft/logs/ ---------------------------------
// The evidence-preservation rule ("full output to a file from the FIRST run") kept being violated
// by hand — transient-failure sightings whose only capture went through `tail`/`grep` were burned
// repeatedly before the class was learned. So the tool does it: a run re-execs itself with output
// piped, pumping every chunk to the terminal AND a timestamped log (reruns can never clobber the
// evidence they're investigating). The path is printed at start and end; cite it instead of piping.
async function runSelfLogged(): Promise<never> {
  const { mkdirSync, openSync, writeSync, closeSync } = await import("node:fs");
  const logsDir = join(ROOT, "draft", "logs");
  mkdirSync(logsDir, { recursive: true });
  const argv = process.argv.slice(2);
  // A SELECTED run is named `check-only-…`, never `check-<tier>-…`, and that is the fence's
  // measurement half rather than a cosmetic choice: `splitLogName` derives a row's tier from this
  // filename, so a 14-minute `--only` run logged as `check-full-…` would enter the full tier's
  // median wall and corrupt the ETA every future full run prints. A name outside the tier regex is
  // excluded from tier medians, from `--backfill`'s tier attribution and from per-tier retention by
  // construction — no consumer has to remember to filter it out.
  const tier = parseArgv(argv).only ? "only" : tierFromArgv(argv);
  const stamp = new Date().toISOString().replace(/\.\d+Z$/, "Z").replace(/:/g, "-");
  const logPath = join(logsDir, `check-${tier}-${stamp}.log`);
  console.log(`check.ts: full log → ${logPath}`);
  const fd = openSync(logPath, "w");
  const child = Bun.spawn([process.argv[0], process.argv[1], ...process.argv.slice(2)], {
    cwd: ROOT,
    env: { ...process.env, CHECK_SELF_LOG: logPath },
    stdout: "pipe",
    stderr: "pipe",
    stdin: "inherit",
  });
  const pump = async (stream: ReadableStream<Uint8Array>, out: NodeJS.WriteStream) => {
    for await (const chunk of stream) {
      out.write(chunk);
      writeSync(fd, chunk);
    }
  };
  await Promise.all([pump(child.stdout, process.stdout), pump(child.stderr, process.stderr)]);
  const exit = await child.exited;
  closeSync(fd);
  console.log(`check.ts: full log at ${logPath}`);
  process.exit(exit);
}

if (import.meta.main) {
  // --help prints and exits; no evidence to preserve, so no log file for it.
  if (process.env.CHECK_SELF_LOG || process.argv.includes("--help")) await main();
  else await runSelfLogged();
}
