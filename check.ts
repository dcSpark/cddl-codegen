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
 *   --refresh-fuzz  re-run fuzz/generate.sh before the fuzz compile-rot check even if generated/ exists.
 *   --cache-transparency  enable the flag-gated `verify_cache_transparency` full-tier gate (two verify
 *                   runs — cached vs GATE_CACHE=0 — asserted byte-identical; otherwise SKIPPED).
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
 * (path printed at start and end) — evidence preservation is the tool's job, not a piping habit.
 * Never pipe a run through `tail`/`grep` as its only capture; cite the printed log path instead.
 *
 * NETWORK: local/full runs start with a retried `cargo fetch` warm-up (workspace + fuzz +
 * tests/warmup dep-universe manifest), then force CARGO_NET_OFFLINE=true for every gate — nested-
 * cargo cells resolve from the cargo cache instead of hitting crates.io per cell, which removes
 * the registry-transient flake class outright (tests/README.md § "Offline-after-warmup").
 * CHECK_ONLINE=1 skips the offline forcing; a pre-set CARGO_NET_OFFLINE=true skips the fetch.
 * The fast tier (CI) is untouched.
 *
 * SELF-COMPLETENESS (the systematic catch, TDD): the first gate `self_checks` runs three meta-checks
 * so a new gate that nobody registers fails the run rather than silently not existing:
 *   1. ignored-test classification — every `#[ignore]` test must be registered here as either a
 *      manual gate (run it) or a known-failing stub (never run it, shown as STUB).
 *   2. matrix-script coverage    — every `cddl-matrix/*.ts` (minus lib.ts) must be wired to a gate.
 *   3. CI-is-fast-tier invariant — build.yml must invoke `bun run check.ts fast` and must contain
 *      NO other run step (all CI work flows through the registry's fast tier, so growing CI is an
 *      explicit, reviewed registry edit — not a workflow edit agents make in passing).
 *
 * Meta-checks mutation-verified red-first at landing (repo idiom):
 *   - adding a throwaway `#[ignore]` test           -> meta-check 1 FAILED (unclassified ignore)
 *   - adding a throwaway `cddl-matrix/throwaway.ts`  -> meta-check 2 FAILED (script wired to no gate)
 *   - adding a direct `run: cargo test` step to build.yml -> meta-check 3 FAILED (bypasses registry)
 *   canaries reverted after confirming red.
 */
import { existsSync, readFileSync, readdirSync, statSync, unlinkSync, writeFileSync } from "node:fs";
import { homedir, tmpdir } from "node:os";
import { basename, join, resolve } from "node:path";
import {
  appendRows, cellCountsFor, compactDur, keptRunKeys, machineId, parseLog, readCells, readDigest,
  readLedger, runKeysInOrder, splitLogName, tierWindow, trimCellLines, trimRows, upsert, writeLedger,
  KEEP_RUNS_IN_CELLS, type Digest, type GateRow, type Row, type RunRow,
} from "./cddl-matrix/project_timings.ts";

const ROOT = import.meta.dir;
const MATRIX = join(ROOT, "cddl-matrix");
const LOGS_DIR = join(ROOT, "draft", "logs");
const LEDGER = join(ROOT, "draft", "timings.jsonl");
const CELLS = join(ROOT, "draft", "timing-cells.jsonl");

// ---- tiers ---------------------------------------------------------------------------------------
const TIERS = ["fast", "local", "full"] as const;
export type Tier = (typeof TIERS)[number];
const rank = (t: Tier) => TIERS.indexOf(t);

// ---- gate model ----------------------------------------------------------------------------------
type Status = "PASS" | "FAIL" | "SKIPPED" | "STUB" | "NOT_IN_TIER";
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
 * Assumed worst-case resident set of ONE `rustc`, in GiB.
 *
 * Deliberately ~4× the 455 MiB largest actually observed. The margin is the point: the full tier's
 * emitted-test crates (thousands of `#[test]` functions each) were NOT sampled, and a single large
 * rustc is exactly the shape a count-based bound cannot see coming.
 */
const ASSUMED_PEAK_RUSTC_GIB = 2;
/** Fraction of MemTotal the batch may commit to concurrent `rustc`; the rest is the headroom above. */
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

  const product = o.memTotalGiB === undefined
    ? FALLBACK_RUSTC_PRODUCT
    : Math.max(1, Math.round((o.memTotalGiB * RUSTC_MEM_FRACTION) / ASSUMED_PEAK_RUSTC_GIB));
  const derived = Math.max(1, Math.floor(product / Math.max(1, o.gatesInFlight)));
  const memWhy = o.memTotalGiB === undefined
    ? `MemTotal unreadable — fallback product ${product}`
    : `${o.memTotalGiB.toFixed(1)} GiB MemTotal × ${RUSTC_MEM_FRACTION} ÷ ${ASSUMED_PEAK_RUSTC_GIB} GiB/rustc = ${product} rustc`;

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

// ---- resource preflight: a memory cap cannot be discovered mid-run ------------------------------
// A tier runs for tens of minutes and commits to its peak in the first seconds. When it overcommits,
// the machine does not fail a gate — it swaps, and the developer power-cycles it, which destroys the
// run AND everything else on the box. So the floors are checked ONCE, up front, loudly, in the same
// shape `verify.ts`'s `diskHeadroomPreflight` established (hard floor, named cleanup command).
//
// Memory DEGRADES rather than refuses: a sequential tier is slow but correct, and a slow tier beats
// no tier. Only a floor at which even one cargo would thrash refuses outright. Disk REFUSES, because
// going sequential does not create free space and every nested-cargo gate downstream would die on
// ENOSPC tens of minutes in — the ENOSPC entry in `tests/TESTING_ROADMAP.md` is that failure already
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

  for (const w of warnings) console.log("  WARN " + w);
  if (problems.length) {
    for (const p of problems) console.log("  FAIL " + p);
    return { status: "FAIL", reason: `${problems.length} self-completeness problem(s)` };
  }
  const groups = new Set(REGISTRY.map(g => g.concurrent).filter(Boolean) as string[]);
  console.log(
    `  OK — ${ignored.size} #[ignore] test(s) classified (${manual.size} manual gate(s), ${stubs.size} stub(s)), ` +
      `${scripts.length} matrix script(s) covered, CI runs the fast tier only, ` +
      `${groups.size} concurrency group(s) well-formed`,
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

// ---- fuzz compile-rot gate: (re)generate iff needed, then cargo check the fuzz crate -------------
function runFuzz(o: Opts): Outcome {
  const gen = join(ROOT, "fuzz", "generated");
  if (!existsSync(gen) || o.refreshFuzz) {
    console.log(`  ${existsSync(gen) ? "--refresh-fuzz" : "fuzz/generated absent"} -> running fuzz/generate.sh`);
    const g = sh(["bash", "fuzz/generate.sh"], ROOT);
    if (g !== 0) return { status: "FAIL", reason: `fuzz/generate.sh exit ${g}` };
  }
  const exit = sh(["cargo", "check", "--manifest-path", "fuzz/Cargo.toml"], ROOT);
  return exit === 0 ? { status: "PASS" } : { status: "FAIL", reason: `cargo check (fuzz) exit ${exit}` };
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
    desc: "tests/corpus/COVERAGE.md is up to date" },

  // --- local tier (default): the heavy correctness gates, NOT run in CI (cost policy) ---
  { id: "build", tier: "local", kind: "cmd", cmd: ["cargo", "build", "--locked", "--workspace", "--all-features", "--all-targets"],
    desc: "workspace build" },
  { id: "test", tier: "local", kind: "cmd", cmd: ["cargo", "test", "--all-features", "--all-targets"],
    desc: "full test suite (incl. corpus + wasm-matrix compile gates)" },
  { id: "insta_orphan", tier: "local", kind: "cmd",
    cmd: ["cargo", "insta", "test", "--unreferenced=reject", "--", "snapshot_tests", "robustness"],
    desc: "snapshot orphan check" },
  { id: "matrix_typecheck", tier: "local", kind: "fn", run: runMatrixTypecheck,
    desc: "tsc --noEmit over the cddl-matrix scripts (pinned local devDependency)" },
  { id: "lint_doc_citations", tier: "local", kind: "cmd",
    cmd: ["bun", "run", "lint_doc_citations.ts"], cwd: MATRIX,
    script: "lint_doc_citations.ts", desc: "documentation citation existence + positional-citation + MD022 lint" },
  { id: "project_decode_conformance", tier: "local", kind: "cmd",
    cmd: ["bun", "run", "project_decode_conformance.ts"], cwd: MATRIX,
    script: "project_decode_conformance.ts", desc: "decode-conformance catalog drift gate (matrix.json + catalog.toml, no cargo)" },
  { id: "no_silent_directive", tier: "local", kind: "cmd",
    cmd: ["bun", "run", "no_silent_directive.ts"], cwd: MATRIX,
    script: "no_silent_directive.ts", desc: "comment-DSL directive×shape silent-drop net (generate with/without each directive, FAIL on byte-identical-and-unmentioned)" },
  { id: "project_recombination_check", tier: "local", kind: "cmd",
    cmd: ["bun", "run", "project_recombination.ts", "--check"], cwd: MATRIX,
    script: "project_recombination.ts", desc: "recombination-fuzzer ingredients drift gate (matrix.json → tests/recomb/ingredients.json, no cargo)" },
  // Sub-second file-scanner promoted to `fast` beside its sibling `project_wasm_matrix_check`
  // (maintainer call, 2026-07: measured ~0.04s wall).
  { id: "project_multifile_matrix_check", tier: "fast", kind: "cmd",
    cmd: ["bun", "run", "project_multifile_matrix.ts", "--check"], cwd: MATRIX,
    script: "project_multifile_matrix.ts", desc: "multifile placement matrix fixtures drift gate" },
  { id: "query_q4_directional", tier: "local", kind: "cmd", cmd: ["bun", "run", "query_q4_directional.ts", "--check"], cwd: MATRIX,
    script: "query_q4_directional.ts", desc: "Q4 directional-support query + consistency gate (matrix.json + catalog.toml, no cargo)" },
  { id: "query_q1_gaps", tier: "local", kind: "cmd", cmd: ["bun", "run", "query_q1_gaps.ts", "--check"], cwd: MATRIX,
    script: "query_q1_gaps.ts", desc: "Q1 support-gap query + generated-Limitations drift gate (matrix.json → current_capacities.mdx, no cargo)" },
  { id: "query_q5_completeness", tier: "local", kind: "cmd", cmd: ["bun", "run", "query_q5_completeness.ts", "--check"], cwd: MATRIX,
    script: "query_q5_completeness.ts", desc: "Q5 matrix-self-completeness query + reconciliation gate (matrix.json + sources/*.abnf/.prelude, no cargo)" },
  { id: "query_q6_diff", tier: "local", kind: "cmd", cmd: ["bun", "run", "query_q6_diff.ts", "--check"], cwd: MATRIX,
    script: "query_q6_diff.ts", desc: "Q6 profile/version-diff query + profile-set consistency, vacuity & annotation-completeness gate (matrix.json, no cargo)" },
  { id: "project_status_headers", tier: "local", kind: "cmd",
    cmd: ["bun", "run", "project_status_headers.ts", "--check"], cwd: MATRIX,
    script: "project_status_headers.ts",
    desc: "status-header count spans drift gate (matrix.json + catalog.toml + check.ts registry → ROADMAP/README/tests-README, no cargo)" },
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
  { id: "verify", tier: "full", kind: "fn", run: runVerify, script: "verify.ts",
    desc: "cddl-matrix mechanical verify gate (oracle preflight + probe every feature)" },
  // Registered AFTER `verify` so a `full --cache-transparency` run warms the cache via `verify` first,
  // making this gate's cached run A genuinely hit-heavy (registry execution is sequential).
  { id: "verify_cache_transparency", tier: "full", kind: "fn", run: runCacheTransparency, script: "cache_transparency.ts",
    desc: "gate-cache OUTPUT-side soundness: verify.ts annotations + report byte-identical cached vs GATE_CACHE=0 (flag-gated --cache-transparency)" },
  { id: "gate_cache_closure_audit", tier: "full", kind: "fn", run: runClosureAudit, script: "audit_gate_cache_closure.ts",
    desc: "gate-cache KEY-side soundness: strace input-closure audit of a cached gate (default multifile_matrix_compiles, CLOSURE_AUDIT_GATE overrides; SKIPPED if strace absent)" },
  { id: "corpus_detect", tier: "full", kind: "cmd", cmd: ["bun", "run", "corpus_detect.ts"], cwd: MATRIX,
    script: "corpus_detect.ts", desc: "corpus_detect featuresIn/rolesIn self-checks" },
  { id: "fuzz_compile_rot", tier: "full", kind: "fn", run: runFuzz,
    desc: "fuzz crate compile-rot check (generate.sh iff needed, then cargo check)" },

  // --- tracked-failing IOU stubs: known-failing #[ignore] tests, NEVER executed (shown as STUB) ---
  { id: "stub_preserve_encodings_supports_floats", tier: "full", kind: "stub",
    ignoredTest: "preserve_encodings_supports_floats",
    desc: "IOU: floats under --preserve-encodings (pre-existing unimplemented! in generation/deserialize.rs)" },
];

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
      override: process.env.CHECK_CARGO_JOBS,
      inherited: process.env.CARGO_BUILD_JOBS,
    });
    if (cargo.warning) console.log(`>>> check.ts: ${cargo.warning}`);
    console.log(
      `\n>>> parallel batch: ${order.length} gate(s) in group '${batch.group}', jobs=${jobs}` +
      `, CARGO_BUILD_JOBS=${cargo.jobs} per gate [${cargo.why}]` +
      ` (dispatch order, slowest measured first: ${order.map(g => g.id).join(", ")})`,
    );
    const live = new Map<string, Bun.Subprocess>();
    await runPool(
      order.map(g => ({ id: g.id, g })),
      jobs,
      async item => {
        console.log(`>>> ${item.id}: started`);
        const r = await runGateBuffered(item.g, timingCells, live, cargo.jobs);
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
 * Keep the last `keepPerTier` `check-<tier>-*.log` per tier; delete the rest. Bounded and
 * predictable, which a time window is not: a quiet fortnight followed by a busy day should not
 * change how much history survives.
 *
 * Scope is deliberately narrow — only names matching `check-(fast|local|full)-*.log`. `draft/logs/`
 * also holds hand-written ad-hoc logs, some of which begin with `check-` but do not name a real tier
 * (`check-optional_fixed_float-crates-….log`); they were written under other conventions and are
 * left alone.
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

  const RE = /^check-(fast|local|full)-.+\.log$/;
  const byTier = new Map<Tier, { f: string; mtime: number; size: number }[]>();
  for (const f of readdirSync(o.logsDir)) {
    const m = RE.exec(f);
    if (!m) continue;
    const st = statSync(join(o.logsDir, f));
    const list = byTier.get(m[1] as Tier) ?? [];
    list.push({ f, mtime: st.mtimeMs, size: st.size });
    byTier.set(m[1] as Tier, list);
  }

  let kept = 0;
  const expired: { f: string; size: number }[] = [];
  for (const list of byTier.values()) {
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
    console.log(`  retention: deleted ${r.deleted.length} check-*.log (${fmtBytes(r.bytes)} reclaimed), kept ${r.kept} (last ${KEEP_LOGS_PER_TIER} per tier)`);
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
): void {
  try {
    const counts = cellCountsFor(readCells(CELLS), c.log);
    const merged = upsert(readLedger(LEDGER), [
      runRow(c, { wall_ms: wallMs, verdict }),
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
  const flags = new Set(argv.filter(a => a.startsWith("--")));
  const positional = argv.filter(a => !a.startsWith("--"));
  const KNOWN = new Set(["--keep-going", "--skip-missing", "--refresh-fuzz", "--cache-transparency", "--help"]);
  for (const f of flags)
    if (!KNOWN.has(f)) { console.error(`check.ts: unknown flag '${f}' (known: ${[...KNOWN].join(", ")})`); process.exit(2); }
  if (flags.has("--help")) {
    console.log("usage: bun run check.ts [fast|local|full] [--keep-going] [--skip-missing] [--refresh-fuzz] [--cache-transparency]");
    console.log("  bare invocation runs the `local` tier; CI runs `fast`. See the header of check.ts for details.");
    console.log(`  env: CHECK_JOBS=<n>  gates in flight within a concurrency batch (default ${DEFAULT_JOBS}; 1 = fully sequential)`);
    console.log("       CHECK_CARGO_JOBS=<n>  CARGO_BUILD_JOBS given to each BATCHED gate (default: memory-derived, see check.ts)");
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
  const keepGoing = flags.has("--keep-going");
  const opts: Opts = { skipMissing: flags.has("--skip-missing"), refreshFuzz: flags.has("--refresh-fuzz"), cacheTransparency: flags.has("--cache-transparency") };
  const { jobs: requestedJobs, warning: jobsWarning } = parseJobs(process.env.CHECK_JOBS);
  if (jobsWarning) console.log(`check.ts: ${jobsWarning}`);
  // Before anything expensive: a tier commits to its peak resource in its first seconds and cannot
  // discover a memory cap mid-run.
  const jobs = resourcePreflight(tier, requestedJobs);

  console.log(`\ncheck.ts — tier=${tier}  jobs=${jobs}${keepGoing ? " --keep-going" : ""}${opts.skipMissing ? " --skip-missing" : ""}${opts.refreshFuzz ? " --refresh-fuzz" : ""}${opts.cacheTransparency ? " --cache-transparency" : ""}`);

  // Both of these run BEFORE the warm-up fetch, and that ordering is the point of the ETA: the
  // warm-up can hang on a bad network for minutes and is the most likely place a run is interrupted,
  // so an estimate printed after it is an estimate the interrupted agent never saw.
  printEta(tier);
  printRetention();

  // Deliberately AFTER retention. Emission creates the ledger if it is absent, and retention's global
  // interlock ("refuse to delete until a backfill has run") reads exactly that absence — so emitting
  // first would let check.ts bootstrap a one-row ledger and quietly weaken the guard on a checkout
  // whose logs have never been scraped. Still before the warm-up, which is where a run is most
  // likely to be interrupted.
  const ctx = runContext(tier);
  if (ctx) {
    emit([runRow(ctx)]);
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

  const inTier: Gate[] = [];
  for (const g of REGISTRY) {
    if (g.kind === "stub") { results.set(g.id, { out: { status: "STUB" }, ms: 0 }); continue; }
    if (rank(g.tier) > rank(tier)) { results.set(g.id, { out: { status: "NOT_IN_TIER" }, ms: 0 }); continue; }
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

  // ---- always-printed full-registry summary --------------------------------------------------
  const wall = performance.now() - wall0;
  const idW = Math.max(...REGISTRY.map(g => g.id.length), 4);
  const tierW = 5;
  const line = "-".repeat(idW + tierW + 10 + 40);
  console.log("\n" + line);
  console.log(`SUMMARY — tier=${tier}  wall=${fmtDur(wall)}`);
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
  if (ctx) finalizeRun(ctx, wall, fails.length ? "fail" : "pass", emitted);

  // ---- refresh the measured-duration digest ---------------------------------------------------
  // Runs on EVERY invocation, including a failing one: the rule is a median over a few thousand
  // local rows, so it costs sub-millisecond and there is no reason to defer it behind a bless step
  // that would rot. It is silent unless a deadband tripped, and it can never turn a green run red —
  // durations are nondeterministic, and a failure to write a number is not a failure of the repo.
  if (sh(["bun", "run", "project_timings.ts", "--update"], MATRIX) !== 0)
    console.log("check.ts: timings digest refresh failed (non-fatal — durations are never a gate)");

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
  const tierArg = process.argv.slice(2).find(a => !a.startsWith("--"));
  const tier = TIERS.includes(tierArg as Tier) ? tierArg : "local";
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
