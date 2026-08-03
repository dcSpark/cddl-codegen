#!/usr/bin/env bun
/**
 * project_timings.ts — how long each check.ts gate actually takes, measured rather than remembered.
 *
 *   bun run project_timings.ts --backfill   -> parse draft/logs/check-*.log into draft/timings.jsonl
 *   bun run project_timings.ts --update     -> recompute tests/timings.json from the ledger (deadband)
 *   bun run project_timings.ts --check      -> self-tests + STRUCTURAL assertions (the registered gate)
 *   bun run project_timings.ts              -> readable report (slowest gates first)
 *
 * Three artifacts, three lifetimes:
 *   - `draft/logs/*.log`      prose, gitignored, deleted on a retention window. Never cite one as
 *                             evidence of record: the directory is gitignored, so such a citation is
 *                             dangling-by-construction and cannot fail loudly.
 *   - `draft/timings.jsonl`   the LOCAL ledger — one JSON object per gate-run, append-only,
 *                             gitignored. Parsers must ignore unknown keys.
 *   - `tests/timings.json`    the COMMITTED digest — one row per registry gate plus one per tier.
 *                             It is what a fresh checkout reads on its first run, before any local
 *                             ledger exists, which is why it is committed at all.
 *
 * WHY DURATIONS ARE NEVER A GATE. They are nondeterministic (machine load, cross-session gate
 * contention), so a drift gate on them would add a flaky gate to the very suite this work exists to
 * make cheaper. `--check` asserts STRUCTURE only: every non-stub registry gate has a digest row, and
 * no digest row names a gate the registry does not have. A duration value is never asserted anywhere.
 *
 * WHY THE DIGEST SELF-UPDATES UNDER A DEADBAND (rather than a remembered weekly bless, which rots):
 * a write happens only when the sliding-window MEDIAN has moved past `max(2s, 20% of stored)`. Four
 * properties carry that, each of which a later "simplification" would destroy — see `decideUpdate`.
 *
 * KNOWN COARSENESS, documented rather than engineered around: a cache-enabled run that misses every
 * cell is genuinely slower than one that hits every cell, and both land in `warm_ms`. The median
 * absorbs it; the per-row `cells_run`/`cells_cached` are retained so a later analysis can bucket more
 * finely. `warm_ms` and `cold_ms` are separate fields and NEVER merge — `cold_ms` stays absent until
 * a `GATE_CACHE=0` run supplies one. Absent is honest; a synthesized number is not.
 *
 * THE PROSE PARSER IS PERMANENT, not a migration step. It is the fallback whenever a gate is added
 * without structured emission, or a run dies mid-line: gate rows recovered from prose beat a hole.
 * Two shapes it must keep handling, both regression-locked by the fixtures in `selfTests`:
 *   - a KILLED run has no SUMMARY table and no tier verdict yet still carries real per-gate timings,
 *     because check.ts prints `--- <gate>: <STATUS>  [<dur>]` incrementally as each gate finishes.
 *     Such a run is recorded WITHOUT a wall time — a killed run's gate sum is not a wall time.
 *   - `^RESULT` is not the tier verdict. Individual gates print their own `RESULT:` lines
 *     (`RESULT: PASS (editorial mapping holds mechanically)` and four others). Only the two long
 *     forms check.ts itself prints are the verdict.
 */
import {
  appendFileSync, existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, statSync,
  utimesSync, writeFileSync,
} from "node:fs";
import { createHash } from "node:crypto";
import { cpus, hostname, tmpdir } from "node:os";
import { join, resolve } from "node:path";
import {
  REGISTRY, SCRATCH_MAX_AGE_MS, SCRATCH_PREFIXES, cargoJobsForBatch, etaLines, gateRow, longestFirst,
  memTotalGiB, parseArgv, parseJobs, parseJoinTimeoutMs, planBatches, preflightDecision, resolveSelection,
  retainLogs, runPool, runRow, sweepScratch,
  type Gate, type RunContext, type SelectionResult,
} from "../check.ts";

const HERE = import.meta.dir;
const ROOT = resolve(HERE, "..");
const LOGS_DIR = join(ROOT, "draft", "logs");
const LEDGER = join(ROOT, "draft", "timings.jsonl");
const CELLS = join(ROOT, "draft", "timing-cells.jsonl");
const DIGEST = join(ROOT, "tests", "timings.json");

const TIERS = ["fast", "local", "full"] as const;
type Tier = (typeof TIERS)[number];
const isTier = (s: string): s is Tier => (TIERS as readonly string[]).includes(s);

// ==================================================================================================
// Duration formatting/parsing — the exact inverse of check.ts's fmtDur
// ==================================================================================================
// fmtDur emits exactly three shapes: `<n>ms` (<1s), `<n.n>s` (<60s), `<m>m <s>s` (>=60s). Sub-second
// precision is genuinely LOST above 60s; this never fabricates it back.
export function parseDur(s: string): number | null {
  let m = /^(\d+(?:\.\d+)?)ms$/.exec(s);
  if (m) return Number(m[1]);
  m = /^(\d+(?:\.\d+)?)s$/.exec(s);
  if (m) return Number(m[1]) * 1000;
  m = /^(\d+)m (\d+)s$/.exec(s);
  if (m) return (Number(m[1]) * 60 + Number(m[2])) * 1000;
  return null;
}

/** Compact form for the one attributable write line (`7m33s`), distinct from check.ts's `7m 33s`. */
export function compactDur(ms: number): string {
  if (ms < 1000) return `${Math.round(ms)}ms`;
  const s = ms / 1000;
  if (s < 60) return `${s.toFixed(1)}s`;
  const m = Math.floor(s / 60);
  return `${m}m${Math.round(s - m * 60)}s`;
}

// ==================================================================================================
// The ledger
// ==================================================================================================
export interface GateRow {
  v: 1;
  kind: "gate";
  run: string;           // the log's stamp — chronological within a tier, but see the ordering note
  tier: Tier;
  gate: string;
  status: "pass" | "fail" | "skipped";
  ms: number;
  gate_cache_enabled: boolean;
  cells_run?: number;
  cells_cached?: number;
  machine: string;
  log?: string;          // basename of the log this row was recovered from (upsert key half)
  src: "backfill" | "run";
}
export interface RunRow {
  v: 1;
  kind: "run";
  run: string;
  tier: Tier;
  wall_ms?: number;      // ABSENT for a killed run — deliberately, see the header
  verdict?: "pass" | "fail";
  machine: string;
  log?: string;
  src: "backfill" | "run";
  // Run-scoped provenance, present only on LIVE rows: the prose parser cannot recover any of it from
  // a log, so a backfilled run row simply omits them. They live on the run row rather than being
  // repeated on every gate row because they are facts about the run, not about a gate.
  commit?: string;
  dirty?: boolean;
  rustc?: string;
  gate_cache_enabled?: boolean;
}
export type Row = GateRow | RunRow;

/**
 * Ledger order IS chronological order, and that is load-bearing: `--update` takes the most recent N
 * rows per (gate, cache class) by POSITION, not by parsing `run` as a date. 21 of the 352 logs on
 * disk are hand-named (`check-local-item23.log`) and carry no timestamp at all, so a date parse would
 * have to either drop them or invent an ordering. Backfill instead walks the logs in mtime order and
 * appends; live emission appends as it goes. Both are chronological by construction.
 */
export function rowKey(r: Row): string {
  // `\0` as an ESCAPE, never a literal NUL in the source: one raw NUL byte makes git classify
  // this whole file as binary, so every future diff renders as `Bin N -> M bytes` and silently
  // stops being reviewable. The separator itself is worth keeping — it cannot occur in a log
  // basename or a gate id, so the composite key is unambiguous.
  return `${r.log ?? r.run}\0${r.kind === "gate" ? r.gate : "#run"}`;
}

export function readLedger(path = LEDGER): Row[] {
  if (!existsSync(path)) return [];
  const out: Row[] = [];
  for (const line of readFileSync(path, "utf8").split("\n")) {
    if (!line.trim()) continue;
    try { out.push(JSON.parse(line) as Row); } catch { /* a torn final line from a killed run: skip */ }
  }
  return out;
}

/**
 * One row per CELL — the drill-down beneath a gate row (`src/tests/timing_cells.rs` emits these).
 *
 * `emitter` and `gate` are deliberately different fields. Three of the labels the gate cache is
 * called with (`feature_corpus_compiles`, `wasm_matrix_compiles`, `multifile_matrix_compiles`) are
 * cells inside the `test` gate, not registry gates — the same trap the prose parser avoids by
 * attributing roll-ups to the section header rather than to their label. `gate` is what check.ts said
 * it was running and is ABSENT for a hand-invoked suite; `emitter` is who wrote the row.
 */
export interface CellRow {
  v: 1;
  kind: "cell";
  log?: string;
  gate?: string;
  emitter: string;
  cell: string;
  outcome: "hit" | "run_pass" | "run_fail" | "ran";
  ms: number;
  ts: number;
}

export function readCells(path = CELLS): CellRow[] {
  if (!existsSync(path)) return [];
  const out: CellRow[] = [];
  for (const line of readFileSync(path, "utf8").split("\n")) {
    if (!line.trim()) continue;
    try { out.push(JSON.parse(line) as CellRow); } catch { /* torn line from a killed cell: skip */ }
  }
  return out;
}

/**
 * `cells_run`/`cells_cached` per REGISTRY GATE for one run — the join that puts a live gate row on
 * equal footing with a backfilled one.
 *
 * It matters that this exists at all: `cacheCounterExample` sums these off GATE rows to decide
 * whether the ETA may claim a warm cache does not shortcut a tier. Live rows carried no counts, so
 * every run from here would have contributed nothing to that claim and it would have gone quiet as
 * the backfilled history aged out — with no gate noticing.
 *
 * Only CACHE-CLASSIFIED outcomes count. A replay gate's `ran` row is uncached work, not a cache miss;
 * counting it would make "how warm was this run" mean two different things depending on which gates
 * ran, and would diverge from the backfill parser, which sees only gate-cache roll-ups.
 */
export function cellCountsFor(cells: CellRow[], log: string): Map<string, { run: number; cached: number }> {
  const out = new Map<string, { run: number; cached: number }>();
  for (const c of cells) {
    if (c.log !== log || !c.gate || c.outcome === "ran") continue;
    const a = out.get(c.gate) ?? { run: 0, cached: 0 };
    if (c.outcome === "hit") a.cached++;
    else a.run++;
    out.set(c.gate, a);
  }
  return out;
}

/** Idempotent upsert on (log, gate). Existing keys keep their POSITION so a re-scrape is byte-stable. */
export function upsert(existing: Row[], incoming: Row[]): Row[] {
  const order: string[] = [];
  const byKey = new Map<string, Row>();
  for (const r of existing) {
    const k = rowKey(r);
    if (!byKey.has(k)) order.push(k);
    byKey.set(k, r);
  }
  for (const r of incoming) {
    const k = rowKey(r);
    if (!byKey.has(k)) order.push(k);
    byKey.set(k, r);
  }
  return order.map(k => byKey.get(k)!);
}

/**
 * Upsert, except that a row a RUN wrote is never replaced by one the prose parser recovered.
 *
 * The two sources key identically on `(log, gate)`, so a plain upsert lets a re-scrape overwrite a
 * live row — and this is not hypothetical: retention's own skip message tells the user to run
 * `--backfill`, so it will happen on any checkout with an unscraped backlog. A live row wins on every
 * axis (exact milliseconds instead of prose rounded to whole seconds above a minute, plus commit,
 * dirty and rustc, plus cell counts joined from the cell file), so the prose row has nothing left to
 * contribute. Rows the run never covered — a gate that finished after the run's last write, or any
 * log no run emitted for — still merge normally.
 */
export function mergeBackfill(existing: Row[], incoming: Row[]): Row[] {
  const live = new Set(existing.filter(r => r.src === "run").map(rowKey));
  return upsert(existing, incoming.filter(r => !live.has(rowKey(r))));
}

/**
 * How much history the local ledger keeps. The digest's windows are 20 runs deep, so 200 is ten times
 * what any consumer reads — kept because the raw rows are the only place a later analysis can bucket
 * more finely than the digest does.
 */
export const KEEP_RUNS_IN_LEDGER = 200;
/**
 * Cells are trimmed harder than gate rows, and on purpose: they are a drill-down for the
 * investigation at hand (~650 rows for a `local` run, ~2500 for a `full` one), while the long-range
 * series lives in the ledger. Trimming them can never strand a log, because retention reads the
 * LEDGER to decide what it may delete and never looks at this file.
 */
export const KEEP_RUNS_IN_CELLS = 50;

/** Run identity in ledger order, which is chronological order (see the note on `rowKey`). */
export function runKeysInOrder(rows: Row[]): string[] {
  const seen = new Set<string>();
  const order: string[] = [];
  for (const r of rows) {
    const k = r.log ?? r.run;
    if (!seen.has(k)) { seen.add(k); order.push(k); }
  }
  return order;
}

/**
 * Which runs survive a trim. Two rules, and the second is the load-bearing one.
 *
 * 1. Keep the most recent `keepRuns`. Unbounded growth in `draft/` is the class the user rejected
 *    when they refused to merely COMPRESS the log hoard, and a feature about measurement has no
 *    business reintroducing it in its own data.
 * 2. Keep EVERY run whose log is still on disk, however old. Retention deletes an expired log only
 *    once the ledger holds a row naming it, so trimming that row out would strand the log forever —
 *    the two mechanisms would deadlock, each waiting on the other, and the symptom would be logs
 *    quietly accumulating with every gate green. Rule 2 is what makes them compose instead. It costs
 *    nothing in steady state: retention keeps 10 logs per tier, far inside the window.
 */
export function keptRunKeys(
  rows: Row[], o: { keepRuns?: number; logsOnDisk: Set<string> },
): Set<string> {
  const order = runKeysInOrder(rows);
  const kept = new Set(order.slice(-(o.keepRuns ?? KEEP_RUNS_IN_LEDGER)));
  for (const k of order) if (o.logsOnDisk.has(k)) kept.add(k);
  return kept;
}

export function trimRows(rows: Row[], kept: Set<string>): Row[] {
  return rows.filter(r => kept.has(r.log ?? r.run));
}

/**
 * Cell rows for runs outside the window, dropped. A row with NO `log` is kept: it came from a suite
 * someone ran by hand with `CDDL_TIMING_CELLS` set, and check.ts deleting another session's
 * deliberately-collected data would be the same failure class as deleting an unscraped log. That is
 * bounded rather than open-ended — such rows exist only where a human opted in per invocation — and
 * the bound is the tail below.
 */
export function trimCellLines(lines: string[], kept: Set<string>, keepUnattributed = 5_000): string[] {
  const out: string[] = [];
  const unattributed: string[] = [];
  for (const line of lines) {
    if (!line.trim()) continue;
    let c: CellRow;
    try { c = JSON.parse(line) as CellRow; } catch { continue; } // torn line: not worth carrying
    if (c.log === undefined) unattributed.push(line);
    else if (kept.has(c.log)) out.push(line);
  }
  return [...unattributed.slice(-keepUnattributed), ...out];
}

export function writeLedger(rows: Row[], path = LEDGER): void {
  mkdirSync(join(path, ".."), { recursive: true });
  writeFileSync(path, rows.map(r => JSON.stringify(r)).join("\n") + (rows.length ? "\n" : ""));
}

/**
 * Append rows as they happen. A live run emits one row per gate AT the moment that gate finishes,
 * which is the whole reason this is an append rather than a write-at-end: a run killed at gate 30 of
 * 42 keeps 30 measurements, exactly as the prose parser recovers 37 of 42 from a killed log.
 *
 * One `appendFileSync` per call, and each row is a single line, so a torn write can cost at most the
 * row being written — `readLedger` already skips an unparseable line.
 */
export function appendRows(rows: Row[], path = LEDGER): void {
  if (!rows.length) return;
  mkdirSync(join(path, ".."), { recursive: true });
  appendFileSync(path, rows.map(r => JSON.stringify(r)).join("\n") + "\n");
}

/** Short STABLE machine id. Not the raw hostname: `tests/timings.json` is committed. */
export function machineId(): string {
  return createHash("sha256").update(`${hostname()}|${cpus().length}`).digest("hex").slice(0, 8);
}

// ==================================================================================================
// The prose parser
// ==================================================================================================
const RE_SECTION = /^=== \[(fast|local|full)\] (\S+) — /;
const RE_GATE = /^--- ([A-Za-z0-9_]+): (PASS|FAIL|SKIPPED)(?: \(.*\))?  \[([^\]]+)\]$/;
const RE_SUMMARY = /^SUMMARY — tier=(\S+)  wall=(.+)$/;
const RE_VERDICT_PASS = /^RESULT: PASS — all in-tier gates green \(tier=(\S+)\)$/;
const RE_VERDICT_FAIL = /^RESULT: FAIL — \d+ gate\(s\) failed:/;
// Both roll-up shapes: verify.ts's unlabelled padded form and the Rust gates' `<label> gate-cache:`.
// The label is NOT used for attribution — see parseLog.
const RE_ROLLUP = /gate-cache\s*:\s*(\d+) run, (\d+) cached\s*$/;

export interface ParsedLog {
  run: string;
  tier: Tier;
  gates: { gate: string; status: "pass" | "fail" | "skipped"; ms: number; cells_run?: number; cells_cached?: number }[];
  wallMs?: number;
  verdict?: "pass" | "fail";
}

/** Split `check-<tier>-<stamp>.log`. Rejects a name whose tier field is not a real tier. */
export function splitLogName(basename: string): { tier: Tier; run: string } | null {
  const m = /^check-([A-Za-z0-9_]+)-(.+)\.log$/.exec(basename);
  if (!m) return null;
  if (!isTier(m[1]!)) return null; // e.g. check-optional_fixed_float-crates-…: an ad-hoc log, not a run
  return { tier: m[1]!, run: m[2]! };
}

/**
 * Gate-cache roll-ups are attributed by the SECTION they appear in (`=== [tier] <gate> — …`), never
 * by their own label. Three of the six labels emitted — `feature_corpus_compiles`,
 * `wasm_matrix_compiles`, `multifile_matrix_compiles` — are CELLS INSIDE the `test` gate and are not
 * gate ids at all; a label→gate map would invent three gates that do not exist. The section header
 * is check.ts's own per-gate delimiter, so it is right by construction for every shape.
 */
export function parseLog(text: string, basename: string): ParsedLog | null {
  const named = splitLogName(basename);
  if (!named) return null;
  const parsed: ParsedLog = { run: named.run, tier: named.tier, gates: [] };
  const cells = new Map<string, { run: number; cached: number }>();
  let section: string | null = null;

  for (const line of text.split("\n")) {
    const sec = RE_SECTION.exec(line);
    if (sec) { section = sec[2]!; continue; }
    const roll = RE_ROLLUP.exec(line);
    if (roll && section) {
      const acc = cells.get(section) ?? { run: 0, cached: 0 };
      acc.run += Number(roll[1]);
      acc.cached += Number(roll[2]);
      cells.set(section, acc);
      continue;
    }
    const g = RE_GATE.exec(line);
    if (g) {
      const ms = parseDur(g[3]!);
      if (ms === null) continue;
      const c = cells.get(g[1]!);
      parsed.gates.push({
        gate: g[1]!,
        status: g[2] === "PASS" ? "pass" : g[2] === "FAIL" ? "fail" : "skipped",
        ms,
        ...(c ? { cells_run: c.run, cells_cached: c.cached } : {}),
      });
      continue;
    }
    const s = RE_SUMMARY.exec(line);
    if (s) { const w = parseDur(s[2]!); if (w !== null) parsed.wallMs = w; continue; }
    if (RE_VERDICT_PASS.test(line)) { parsed.verdict = "pass"; continue; }
    if (RE_VERDICT_FAIL.test(line)) { parsed.verdict = "fail"; continue; }
  }
  return parsed;
}

export interface BackfillResult { rows: Row[]; parsed: number; skipped: { log: string; reason: string }[] }

export function backfill(logsDir = LOGS_DIR, machine = machineId()): BackfillResult {
  const names = existsSync(logsDir)
    ? readdirSync(logsDir).filter(f => f.startsWith("check-") && f.endsWith(".log"))
    : [];
  // mtime order = chronological order; see the note on rowKey.
  const ordered = names
    .map(f => ({ f, mtime: statSync(join(logsDir, f)).mtimeMs }))
    .sort((a, b) => a.mtime - b.mtime || (a.f < b.f ? -1 : 1));

  const rows: Row[] = [];
  const skipped: { log: string; reason: string }[] = [];
  let parsedCount = 0;
  for (const { f } of ordered) {
    const p = parseLog(readFileSync(join(logsDir, f), "utf8"), f);
    if (!p) { skipped.push({ log: f, reason: "filename does not name a real tier (fast|local|full)" }); continue; }
    if (p.gates.length === 0) { skipped.push({ log: f, reason: "no `--- <gate>: <STATUS>  [<dur>]` lines (run died before the first gate finished)" }); continue; }
    parsedCount++;
    rows.push({
      v: 1, kind: "run", run: p.run, tier: p.tier, machine, log: f, src: "backfill",
      ...(p.wallMs !== undefined ? { wall_ms: p.wallMs } : {}),
      ...(p.verdict ? { verdict: p.verdict } : {}),
    });
    for (const g of p.gates)
      rows.push({
        v: 1, kind: "gate", run: p.run, tier: p.tier, gate: g.gate, status: g.status, ms: g.ms,
        // check.ts never sets GATE_CACHE; every gate it times therefore ran cache-enabled. (The
        // `verify_cache_transparency` gate runs GATE_CACHE=0 *inside itself*, which is its own cost,
        // not a cold measurement of any other gate.)
        gate_cache_enabled: true,
        ...(g.cells_run !== undefined ? { cells_run: g.cells_run, cells_cached: g.cells_cached } : {}),
        machine, log: f, src: "backfill",
      });
  }
  return { rows, parsed: parsedCount, skipped };
}

// ==================================================================================================
// The digest and its update rule
// ==================================================================================================
export interface HistoryPoint { date: string; warm_ms: number }
export interface DigestGate {
  gate: string;
  warm_ms?: number;
  cold_ms?: number;
  n?: number;
  measured?: string;
  machine?: string;
  history?: HistoryPoint[];
}
export interface DigestTier {
  tier: Tier;
  wall_ms?: number;
  n?: number;
  measured?: string;
  machine?: string;
  history?: HistoryPoint[];
}
export interface Digest { v: 1; note: string; gates: DigestGate[]; tiers: DigestTier[] }

export const WINDOW = 20;
export const SEED_MIN = 3;
export const UPDATE_MIN = 10;

export function median(xs: number[]): number {
  if (xs.length === 0) throw new Error("median of empty window");
  const s = [...xs].sort((a, b) => a - b);
  const mid = s.length >> 1;
  return s.length % 2 ? s[mid]! : (s[mid - 1]! + s[mid]!) / 2;
}

/**
 * Rounding is cosmetic and must stay cosmetic. Above 1s it snaps to whole seconds (1s is always far
 * below the >=2s deadband, so it can never arm the next write). BELOW 1s it snaps to whole
 * milliseconds instead: snapping a 25 ms gate to 0 would not be cosmetic, it would erase the value.
 */
export function roundValue(ms: number): number {
  return ms >= 1000 ? Math.round(ms / 1000) * 1000 : Math.round(ms);
}

export type Decision =
  | { write: true; value: number; reason: string }
  | { write: false; reason: string; note?: string };

/**
 * The update rule — a PURE function of (stored, window), with no I/O, so it is cheap to pin.
 *
 * Four properties it depends on. Each is load-bearing and each has a test that fails if it is
 * "simplified" away:
 *
 *  - MEDIAN over a sliding window, never a mean. This is the real anti-flap mechanism: for the
 *    median to move 20%, roughly half the window has to shift, so a write needs ~5 runs of sustained
 *    change and one loaded-machine outlier cannot move it at all.
 *  - BOTH a percent and an absolute floor. Percent alone lets a 25 ms gate flap on a 10 ms move;
 *    absolute alone treats a 30s move identically on a 25 ms gate and a 16m gate.
 *  - Compare against STORED, not against the previous run. Run-to-run comparison makes monotonic
 *    drift invisible forever; comparing to stored lets slow creep accumulate until it trips.
 *  - NO quantization of the stored value. Bucketing writes (1s/5s/15s bands) is what *introduces*
 *    boundary flapping rather than preventing it — a 5s bucket spans 10s–120s while the deadband is
 *    only >=5s above 25s, so between 10s and 25s the bucket exceeds the trigger and a write can land
 *    far enough from truth to immediately arm the next one. The deadband alone already guarantees
 *    stability.
 *
 * Consequence worth stating rather than fixing: with `band = max(2s, …)` a 25 ms gate effectively
 * freezes after seeding. That is intended — sub-second gates are noise — and a real 25 ms -> 5s
 * regression still trips the absolute floor.
 */
export function decideUpdate(
  stored: number | undefined,
  window: number[],
  storedMachine: string | undefined,
  machine: string,
): Decision {
  if (window.length === 0) return { write: false, reason: "no rows" };
  // The machine check comes FIRST — ahead of seeding, not just ahead of overwriting. `storedMachine`
  // is the identity of the ROW, not of one field: a row already measured elsewhere is inert here in
  // every direction. Behind the `stored === undefined` branch this guard is defeatable in two passes,
  // because seeding the row's still-empty cache class re-stamps its machine to the local one and
  // thereby unlocks the class the guard was protecting. A row that has never been measured at all
  // (`storedMachine === undefined`) has no identity to defend and seeds normally.
  if (storedMachine !== undefined && storedMachine !== machine)
    return {
      write: false,
      reason: "machine mismatch",
      note: `timings: this row was measured on machine ${storedMachine}, this is ${machine} — not overwriting`,
    };
  const raw = median(window);
  if (stored === undefined) {
    if (window.length < SEED_MIN) return { write: false, reason: `seeding needs n>=${SEED_MIN} (n=${window.length})` };
    return { write: true, value: roundValue(raw), reason: `seeded (n=${window.length})` };
  }
  if (window.length < UPDATE_MIN) return { write: false, reason: `window not yet meaningful (n=${window.length}<${UPDATE_MIN})` };
  const band = Math.max(2000, 0.2 * stored);
  if (Math.abs(raw - stored) < band)
    return { write: false, reason: `within deadband (|${Math.round(raw)}-${stored}| < ${Math.round(band)})` };
  return { write: true, value: roundValue(raw), reason: `deadband tripped (n=${window.length})` };
}

/** Most recent `WINDOW` PASS durations for (gate, cache class), oldest first. */
export function windowFor(rows: Row[], gate: string, cls: "warm" | "cold", limit = WINDOW): number[] {
  const hits: number[] = [];
  for (const r of rows) {
    if (r.kind !== "gate" || r.gate !== gate) continue;
    // PASS only. A FAIL is a truncated or inflated run of the gate, and a SKIPPED gate's ~0ms is not
    // a measurement of the work — averaging either in would make the number a lie in both directions.
    if (r.status !== "pass") continue;
    if ((r.gate_cache_enabled ? "warm" : "cold") !== cls) continue;
    hits.push(r.ms);
  }
  return hits.slice(-limit);
}

/** Most recent `WINDOW` run-level walls for a tier, oldest first. Killed runs carry none by design. */
export function tierWindow(rows: Row[], tier: Tier, limit = WINDOW): number[] {
  const hits: number[] = [];
  for (const r of rows) {
    if (r.kind !== "run" || r.tier !== tier) continue;
    if (r.verdict !== "pass") continue;
    if (r.wall_ms === undefined) continue;
    hits.push(r.wall_ms);
  }
  return hits.slice(-limit);
}

export const registryGateIds = (): string[] => REGISTRY.filter(g => g.kind !== "stub").map(g => g.id);

export function emptyDigest(): Digest {
  return {
    v: 1,
    note:
      "Measured gate durations, maintained by cddl-matrix/project_timings.ts. A row exists for every " +
      "non-stub check.ts registry gate (structural, gate-enforced); its warm_ms appears once the local " +
      "ledger has enough runs, and is rewritten only when the sliding-window median moves past " +
      "max(2s, 20%). cold_ms is absent until a GATE_CACHE=0 run supplies one. No test asserts any " +
      "value here.",
    gates: [],
    tiers: [],
  };
}

export function readDigest(path = DIGEST): Digest {
  if (!existsSync(path)) return emptyDigest();
  const d = JSON.parse(readFileSync(path, "utf8")) as Digest;
  return { ...emptyDigest(), ...d, gates: d.gates ?? [], tiers: d.tiers ?? [] };
}

export function serializeDigest(d: Digest): string {
  return JSON.stringify(
    { v: d.v, note: d.note, gates: [...d.gates].sort((a, b) => (a.gate < b.gate ? -1 : 1)), tiers: d.tiers },
    null,
    2,
  ) + "\n";
}

const today = (): string => new Date().toISOString().slice(0, 10);

function pushHistory(row: { history?: HistoryPoint[] }, warm_ms: number): void {
  const h = row.history ?? [];
  h.push({ date: today(), warm_ms });
  while (h.length > 5) h.shift();
  row.history = h;
}

export interface UpdateReport { writes: string[]; notes: string[]; digest: Digest; changed: boolean }

/**
 * Recompute the digest from the ledger. Two independent reasons the file's bytes may change, and no
 * third: (a) the deadband tripped for some row, or (b) the REGISTRY's gate set changed, which adds or
 * drops a row's existence. (b) is already a reviewed edit; (a) prints one attributable line per write.
 */
export function updateDigest(rows: Row[], prior: Digest, machine = machineId()): UpdateReport {
  const writes: string[] = [];
  // Deduped: the machine guard now fires for both cache classes of every foreign row, so the raw
  // stream would be ~2 identical lines per gate. One line per distinct foreign machine is the
  // signal; the rest is volume.
  const notes = new Set<string>();
  const byGate = new Map(prior.gates.map(g => [g.gate, { ...g }]));
  const next: DigestGate[] = [];

  for (const id of registryGateIds()) {
    const row: DigestGate = byGate.get(id) ?? { gate: id };
    for (const cls of ["warm", "cold"] as const) {
      const field = cls === "warm" ? "warm_ms" : "cold_ms";
      const w = windowFor(rows, id, cls);
      const d = decideUpdate(row[field], w, row.machine, machine);
      if (d.write) {
        const before = row[field];
        row[field] = d.value;
        row.n = w.length;
        row.measured = today();
        row.machine = machine;
        if (cls === "warm") pushHistory(row, d.value);
        writes.push(
          `timings: ${id} ${cls} ${before === undefined ? "(none)" : compactDur(before)} -> ${compactDur(d.value)} ` +
          `(n=${w.length})`,
        );
      } else if (d.note) notes.add(d.note);
    }
    next.push(row);
  }

  const priorTiers = new Map(prior.tiers.map(t => [t.tier, { ...t }]));
  const nextTiers: DigestTier[] = [];
  for (const tier of TIERS) {
    const row: DigestTier = priorTiers.get(tier) ?? { tier };
    const w = tierWindow(rows, tier);
    const d = decideUpdate(row.wall_ms, w, row.machine, machine);
    if (d.write) {
      const before = row.wall_ms;
      row.wall_ms = d.value;
      row.n = w.length;
      row.measured = today();
      row.machine = machine;
      pushHistory(row, d.value);
      writes.push(
        `timings: tier=${tier} wall ${before === undefined ? "(none)" : compactDur(before)} -> ${compactDur(d.value)} ` +
        `(n=${w.length})`,
      );
    } else if (d.note) notes.add(d.note);
    nextTiers.push(row);
  }

  const digest: Digest = { ...prior, gates: next, tiers: nextTiers };
  const changed = serializeDigest(digest) !== serializeDigest(prior);
  return { writes, notes: [...notes], digest, changed };
}

/**
 * The `--update` pass as a callable: read the ledger, recompute, write the digest when it moved, and
 * print one attributable line per write. Exported because check.ts runs this at the end of every tier
 * and needs the `changed` verdict — `tests/timings.json` is the deriving source for the tier-time
 * spans in `tests/README.md`, so a digest write that lands without its spans leaves the NEXT run to
 * fail-fast on `project_status_headers`. The commit hint is deliberately NOT printed here: only the
 * caller knows whether the derived spans were regenerated in the same flow, and therefore which files
 * belong in the one commit.
 */
export function runDigestUpdate(): UpdateReport {
  const u = updateDigest(readLedger(), readDigest());
  for (const n of u.notes) console.log(n);
  if (u.changed) {
    writeFileSync(DIGEST, serializeDigest(u.digest));
    for (const w of u.writes) console.log(w);
    if (!u.writes.length) console.log("timings: tests/timings.json row set now matches the check.ts registry");
  }
  return u;
}

// ==================================================================================================
// --check: self-tests (pure) + structural assertions (never a duration)
// ==================================================================================================
const FIXTURE_COMPLETED = `
check.ts — tier=full

=== [fast] self_checks — self-completeness meta-checks ===
  OK — everything classified
--- self_checks: PASS  [202ms]

=== [local] test — full test suite ===
feature_corpus_compiles gate-cache: 3 run, 11 cached
--- a/tests/corpus/COVERAGE.md
--- test: PASS  [3m 43s]

=== [full] recombination_crates_execute — recombination fuzzer layer 2 ===
recombination default gate-cache: 0 run, 25 cached
--- recombination_crates_execute: PASS  [56.6s]

=== [full] verify — cddl-matrix mechanical verify gate ===
RESULT: PASS (editorial mapping holds mechanically)
gate-cache          : 149 run, 687 cached
--- verify: PASS  [7m 33s]

=== [full] verify_cache_transparency — flag-gated ===
--- verify_cache_transparency: SKIPPED (pass --cache-transparency)  [0ms]

--------------------------------------------------
SUMMARY — tier=full  wall=74m 33s
--------------------------------------------------
RESULT: PASS — all in-tier gates green (tier=full)
`.trimStart();

const FIXTURE_KILLED = `
check.ts — tier=full

=== [fast] self_checks — self-completeness meta-checks ===
--- self_checks: PASS  [202ms]

=== [local] test — full test suite ===
RESULT: PASS (editorial mapping holds mechanically)
--- test: PASS  [3m 43s]

=== [full] verify — cddl-matrix mechanical verify gate ===
[gate-cache] prelude.float32: cached PASS (key 0e34a418)
  [embed] prelude.float32: round-trips when embedded in __probe_holder
`.trimStart();

interface TestResult { name: string; ok: boolean; detail?: string }

/**
 * Replay a series of runs: each step appends a measurement, re-derives the window, and applies the
 * rule — persisting the WRITTEN value as `stored`, which is what "compare against stored, not
 * against the previous run" means operationally.
 */
function replay(series: number[], seed: number, machine = "m0"): number[] {
  let stored: number | undefined = seed;
  const seen: number[] = [];
  const writes: number[] = [];
  for (const x of series) {
    seen.push(x);
    const d = decideUpdate(stored, seen.slice(-WINDOW), machine, machine);
    if (d.write) { stored = d.value; writes.push(d.value); }
  }
  return writes;
}

/**
 * Walk the repo's `temp_dir()` / `tmpdir()` join sites and report the scratch names check.ts's
 * `SCRATCH_PREFIXES` does not cover. The registry has to be an enumeration of the mint sites to be
 * worth anything, and an enumeration written once is a snapshot that rots — so the enumeration is
 * re-derived here on every run of the `timings_digest_check` gate.
 *
 * `unresolved` is a failure, not a note: a join whose name this walker cannot read is exactly where a
 * new spelling would hide, and the cost of guessing wrong is scratch that leaks until a disk fills.
 * `.lock` names are excluded because the sweep excludes them by design (they are
 * `acquire_scratch_lock`'s sibling flocks, and unlinking a held one breaks the mutual exclusion).
 */
export function scratchMintSites(): { uncovered: string[]; unresolved: string[] } {
  const files: string[] = [join(ROOT, "check.ts")];
  const walk = (dir: string) => {
    let entries: string[];
    try { entries = readdirSync(dir); } catch { return; }
    for (const e of entries) {
      if (e === "node_modules" || e === "target" || e === "generated") continue;
      const p = join(dir, e);
      if (statSync(p).isDirectory()) walk(p);
      else if (e.endsWith(".rs") || e.endsWith(".ts")) files.push(p);
    }
  };
  for (const d of ["src", "fuzz", "cddl-matrix"]) walk(join(ROOT, d));

  const uncovered: string[] = [];
  const unresolved: string[] = [];
  const covered = (lit: string) => lit.endsWith(".lock") || SCRATCH_PREFIXES.some(p => lit.startsWith(p));
  const RUST = /temp_dir\(\)\s*\.join\(\s*(?:&\s*)?(?:format!\(\s*)?(?:"((?:[^"\\]|\\.)*)"|([A-Za-z_][A-Za-z0-9_]*)\s*\))/g;
  const TS = /join\(\s*tmpdir\(\)\s*,\s*[`"]([^`"$\n]*)/g;

  for (const f of files) {
    const src = readFileSync(f, "utf8");
    const rel = f.slice(ROOT.length + 1);
    if (f.endsWith(".rs")) {
      for (const m of src.matchAll(RUST)) {
        let lit = m[1];
        if (lit === undefined) {
          // A variable join (`.join(&scratch_name)`) — resolve its `let` in the same file.
          const v = m[2]!;
          const a = new RegExp(`let\\s+${v}\\s*(?::[^=]*)?=\\s*(?:format!\\(\\s*)?"((?:[^"\\\\]|\\\\.)*)"`).exec(src);
          if (!a) { unresolved.push(`${rel}: .join(${v}) — cannot resolve the name`); continue; }
          lit = a[1]!;
        }
        if (!covered(lit)) uncovered.push(`${rel}: ${lit}`);
      }
    } else {
      for (const m of src.matchAll(TS)) if (!covered(m[1]!)) uncovered.push(`${rel}: ${m[1]}`);
    }
  }
  return { uncovered, unresolved };
}

export async function selfTests(): Promise<TestResult[]> {
  const t: TestResult[] = [];
  const ok = (name: string, cond: boolean, detail?: string) => t.push({ name, ok: cond, detail });

  // Hovering ±19% around stored -> 0 writes. Percent-only or absolute-only thresholds both fail here.
  {
    const stored = 100_000;
    const series = Array.from({ length: 60 }, (_, i) => stored * (1 + (i % 2 ? 0.19 : -0.19)));
    const w = replay(series, stored);
    ok("hovering_below_threshold_never_rewrites", w.length === 0, `writes=${JSON.stringify(w)}`);
  }

  // A +30% step held for a full window -> exactly ONE write, then silence. Not a write per run.
  {
    const stored = 100_000;
    const series = Array.from({ length: 60 }, () => 130_000);
    const w = replay(series, stored);
    ok("sustained_shift_writes_once", w.length === 1 && w[0] === 130_000, `writes=${JSON.stringify(w)}`);
  }

  // One 10x run inside a window of 10 -> 0 writes. This is what the MEDIAN buys: a mean would be
  // dragged 90% by that single loaded-machine outlier and would write immediately.
  {
    const stored = 100_000;
    const series = [...Array.from({ length: 9 }, () => 100_000), 1_000_000];
    const w = replay(series, stored);
    ok("single_outlier_does_not_write", w.length === 0, `writes=${JSON.stringify(w)}`);
  }

  // Median sitting exactly on a 1s boundary, jittered ±1ms -> 0 writes. Quantizing the STORED value
  // into buckets is what would break this: a bucket wider than the deadband lets a write land far
  // enough from truth to immediately arm the next one.
  {
    const stored = 12_000;
    const series = Array.from({ length: 60 }, (_, i) => 12_000 + (i % 3) - 1);
    const w = replay(series, stored);
    ok("rounding_boundary_is_stable", w.length === 0, `writes=${JSON.stringify(w)}`);
  }

  // +3%/run creep -> writes once the median is past 20% of STORED, and not before. This is the test
  // that fails if the comparison is ever "simplified" to run-to-run: consecutive medians differ by
  // only ~3%, which never clears the deadband, so a run-to-run rule writes ZERO times forever while
  // the gate silently doubles. Verified by making exactly that change and watching this go red.
  {
    const stored = 100_000;
    const series: number[] = [];
    let cur = stored;
    for (let i = 0; i < 30; i++) { cur *= 1.03; series.push(cur); }
    const w = replay(series, stored);
    ok("slow_drift_eventually_trips", w.length >= 1 && w[0]! >= stored * 1.2,
      `writes=${JSON.stringify(w.map(Math.round))}`);
  }

  // A GATE_CACHE=0 row never contributes to warm_ms, and vice versa. A gate at 700 cache hits and
  // the same gate at 0 are different measurements; storing two fields is the answer to the
  // cache/no-cache question, and averaging them would answer neither.
  {
    const mk = (ms: number, warm: boolean): GateRow => ({
      v: 1, kind: "gate", run: `r${ms}${warm}`, tier: "full", gate: "g", status: "pass", ms,
      gate_cache_enabled: warm, machine: "m0", src: "run",
    });
    const rows: Row[] = [mk(1000, true), mk(2000, true), mk(90_000, false), mk(91_000, false), mk(92_000, false)];
    const warm = windowFor(rows, "g", "warm");
    const cold = windowFor(rows, "g", "cold");
    ok("warm_and_cold_never_merge",
      warm.length === 2 && cold.length === 3 && !warm.some(x => x >= 90_000) && !cold.some(x => x < 90_000),
      `warm=${JSON.stringify(warm)} cold=${JSON.stringify(cold)}`);
  }

  // A row measured on another machine is inert here — for BOTH cache classes, seeding included, and
  // it stays inert however many times `--update` runs. Two passes, because one is not enough to
  // catch the escalation this pins: if the machine check sat behind the `stored === undefined`
  // branch, pass 1 would refuse the populated class but SEED the empty one, re-stamping the row's
  // machine identity to the local machine — which unlocks the protected class on pass 2. The trigger
  // is not "someone bought a second laptop": machineId() folds in `cpus().length`, so a VM or
  // container that comes back with a different core count is a different machine id on the same box.
  {
    const foreign = "machineA";
    const local = "machineB";
    const prior: Digest = {
      ...emptyDigest(),
      gates: [{ gate: "fmt", warm_ms: 800, n: 20, measured: "2026-01-01", machine: foreign }],
    };
    const rows: Row[] = [];
    for (let i = 0; i < 20; i++)
      for (const warm of [true, false])
        rows.push({
          v: 1, kind: "gate", run: `r${i}${warm}`, tier: "fast", gate: "fmt", status: "pass",
          ms: 99_000, gate_cache_enabled: warm, machine: local, src: "run",
        });
    let d = prior;
    for (let pass = 0; pass < 2; pass++) d = updateDigest(rows, d, local).digest;
    const fmt = d.gates.find(g => g.gate === "fmt")!;
    ok("foreign_machine_row_is_never_overwritten_or_reseeded",
      fmt.warm_ms === 800 && fmt.cold_ms === undefined && fmt.machine === foreign,
      `after 2 passes on ${local}: ${JSON.stringify(fmt)}`);
  }

  // STRUCTURAL: a digest row per non-stub registry gate, and no orphan rows. Never a duration.
  {
    const d = readDigest();
    const have = new Set(d.gates.map(g => g.gate));
    const want = registryGateIds();
    const missing = want.filter(g => !have.has(g));
    const orphan = [...have].filter(g => !want.includes(g));
    ok("digest_has_a_row_per_registry_gate", missing.length === 0 && orphan.length === 0,
      `missing=${JSON.stringify(missing)} orphan=${JSON.stringify(orphan)}` +
      (missing.length || orphan.length ? " — run `bun run project_timings.ts --update`" : ""));
  }

  // Parser fixtures. Inline rather than pointing at draft/logs/: those are gitignored and are
  // deleted on a retention window, so a fixture living there would rot exactly when it matters.
  {
    const p = parseLog(FIXTURE_COMPLETED, "check-full-2026-07-25T20-12-44Z.log")!;
    const verify = p.gates.find(g => g.gate === "verify");
    const test = p.gates.find(g => g.gate === "test");
    ok("parser_completed_log_yields_wall_verdict_and_cells",
      p.gates.length === 5 && p.wallMs === 74 * 60_000 + 33_000 && p.verdict === "pass" &&
      verify?.ms === 7 * 60_000 + 33_000 && verify.cells_run === 149 && verify.cells_cached === 687 &&
      test?.cells_run === 3 && test.cells_cached === 11 &&
      p.gates.find(g => g.gate === "verify_cache_transparency")?.status === "skipped",
      JSON.stringify({ n: p.gates.length, wallMs: p.wallMs, verdict: p.verdict, verify, test }));
  }
  {
    const p = parseLog(FIXTURE_KILLED, "check-full-2026-07-25T17-53-25Z.log")!;
    ok("parser_killed_log_yields_gates_but_no_wall_or_verdict",
      p.gates.length === 2 && p.wallMs === undefined && p.verdict === undefined,
      JSON.stringify({ n: p.gates.length, wallMs: p.wallMs, verdict: p.verdict }));
  }
  {
    ok("parser_rejects_a_non_tier_log_name",
      splitLogName("check-optional_fixed_float-crates-20260718T141505Z.log") === null &&
      splitLogName("check-full-2026-07-25T20-12-44Z.log")?.tier === "full");
  }
  {
    ok("parser_inverts_every_fmtDur_shape",
      parseDur("202ms") === 202 && parseDur("56.6s") === 56_600 &&
      parseDur("3m 37s") === 217_000 && parseDur("1m 2s") === 62_000 && parseDur("0ms") === 0 &&
      parseDur("later") === null);
  }

  // The start-of-run ETA never fabricates a number. Below the seeding floor — a fresh checkout whose
  // digest has not been blessed for this tier — the honest output is that there is no baseline, and
  // it must not depend on a local ledger, because a fresh checkout has none.
  {
    const rows: Row[] = [];
    const noRow: Digest = { ...emptyDigest(), tiers: [] };
    const belowFloor: Digest = { ...emptyDigest(), tiers: [{ tier: "full", wall_ms: 100, n: 2 }] };
    const noWall: Digest = { ...emptyDigest(), tiers: [{ tier: "full", n: 40 }] };
    const want = "  no baseline yet for tier=full";
    ok("eta_says_no_baseline_rather_than_fabricating_one",
      etaLines(noRow, rows, "full").join("|") === want &&
      etaLines(belowFloor, rows, "full").join("|") === want &&
      etaLines(noWall, rows, "full").join("|") === want,
      JSON.stringify([etaLines(noRow, rows, "full"), etaLines(belowFloor, rows, "full"), etaLines(noWall, rows, "full")]));
  }
  // A seeded row yields exactly one estimate line, sourced from the RUN-LEVEL wall — never a sum of
  // per-gate medians, which omits inter-gate overhead and is unavailable for killed runs.
  {
    const d: Digest = { ...emptyDigest(), tiers: [{ tier: "fast", wall_ms: 54_000, n: 20 }] };
    const lines = etaLines(d, [], "fast");
    ok("eta_reports_the_blessed_run_level_median",
      lines.length === 1 && lines[0]!.includes("~54s") && lines[0]!.includes("median of 20 passing fast runs"),
      JSON.stringify(lines));
  }

  // ---- retention: the interlocks on an IRREVERSIBLE operation ------------------------------------
  // Every one of these pins a way `check.ts` could delete the only copy of something. They run
  // against a temp directory, never `draft/logs/`.
  {
    const seq = (prefix: string, n: number, t0 = 1_700_000_000): [string, number][] =>
      Array.from({ length: n }, (_, i) => [`${prefix}${String(i).padStart(3, "0")}.log`, t0 + i]);
    // Default body carries NO `--- <gate>: …` line, so it parses to zero gate rows — the shape a run
    // that died before its first gate leaves behind, and the one case retention may delete unscraped.
    const mk = (files: [string, number][], body = "x".repeat(1000)) => {
      const dir = mkdtempSync(join(tmpdir(), "cddl-retention-"));
      const logs = join(dir, "logs");
      mkdirSync(logs);
      for (const [f, mtime] of files) {
        writeFileSync(join(logs, f), body);
        utimesSync(join(logs, f), mtime, mtime); // distinct ascending mtimes: retention orders by mtime
      }
      return { dir, logs, ledger: join(dir, "timings.jsonl") };
    };
    const ledgerFor = (names: string[]): string =>
      names.map(log => JSON.stringify({
        v: 1, kind: "gate", run: log, tier: "fast", gate: "fmt", status: "pass", ms: 700,
        gate_cache_enabled: true, machine: "m0", log, src: "backfill",
      })).join("\n") + "\n";

    // AC: blocked on the backfill having run. The logs are the ONLY copy of the duration history
    // until they have been scraped, so an absent-or-empty ledger must stop the delete, not warn past
    // it. An empty file is treated exactly like a missing one — `touch` is not a backfill.
    {
      const { dir, logs, ledger } = mk(seq("check-fast-", 15));
      const noLedger = retainLogs({ logsDir: logs, ledger, cited: () => new Set() });
      writeFileSync(ledger, "");
      const emptyLedger = retainLogs({ logsDir: logs, ledger, cited: () => new Set() });
      ok("retention_refuses_to_delete_without_a_backfilled_ledger",
        noLedger.status === "skipped" && emptyLedger.status === "skipped" &&
        readdirSync(logs).length === 15 && /timings\.jsonl is missing or empty/.test(noLedger.reason ?? ""),
        JSON.stringify({ noLedger, emptyLedger, onDisk: readdirSync(logs).length }));
      rmSync(dir, { recursive: true });
    }

    // The citation guard FAILS CLOSED. An empty result set and a broken scan are indistinguishable
    // at the call site, and the difference between them is deleted evidence.
    {
      const { dir, logs, ledger } = mk(seq("check-fast-", 15));
      writeFileSync(ledger, '{"v":1}\n');
      const r = retainLogs({ logsDir: logs, ledger, cited: () => null });
      ok("retention_fails_closed_when_the_citation_scan_fails",
        r.status === "skipped" && readdirSync(logs).length === 15 && /citation scan failed/.test(r.reason ?? ""),
        JSON.stringify(r));
      rmSync(dir, { recursive: true });
    }

    // Keep the last 10 per tier; a cited log survives past its window and is NAMED, so the guard's
    // own workload stays visible rather than becoming a silent bridge nobody removes. Ad-hoc logs —
    // including `check-`-prefixed ones whose tier field is not a real tier — are never candidates.
    {
      const files: [string, number][] = [
        ...seq("check-fast-", 15), ...seq("check-local-", 12), ...seq("check-full-", 3),
        ["check-optional_fixed_float-crates-20260718T141505Z.log", 1_600_000_000],
        ["verify-rustname-registration-2026-07-18T22-48-50Z.log", 1_600_000_001],
        ["check-local-item23.log", 1_600_000_002],
      ];
      const { dir, logs, ledger } = mk(files);
      writeFileSync(ledger, '{"v":1}\n');
      const cited = new Set(["check-fast-000.log", "check-local-item23.log"]);
      const r = retainLogs({ logsDir: logs, ledger, cited: () => cited });
      const left = new Set(readdirSync(logs));
      const newestSurvive = (p: string, from: number) =>
        Array.from({ length: 10 }, (_, i) => `${p}${String(from + i).padStart(3, "0")}.log`).every(f => left.has(f));
      ok("retention_keeps_the_last_10_per_tier_and_every_cited_log",
        // fast: 15 -> 5 expired, 1 cited -> 4 deleted. local: 13 (12 + the older item23) -> 3
        // expired, item23 cited -> 2 deleted. full: 3 -> none expired.
        r.status === "ran" && r.deleted.length === 6 && r.bytes === 6000 &&
        r.citedKept.length === 2 && left.has("check-fast-000.log") && left.has("check-local-item23.log") &&
        newestSurvive("check-fast-", 5) && newestSurvive("check-local-", 2) &&
        left.has("check-optional_fixed_float-crates-20260718T141505Z.log") &&
        left.has("verify-rustname-registration-2026-07-18T22-48-50Z.log"),
        JSON.stringify({ deleted: r.deleted, bytes: r.bytes, citedKept: r.citedKept, left: [...left].sort() }));
      rmSync(dir, { recursive: true });
    }

    // A `--only` run's log is named OUTSIDE the tier regex on purpose — no tier median, no
    // `--backfill` tier attribution, no per-tier retention window may absorb a partial run. The flip
    // side of that exclusion is that nothing would ever DELETE such a log, so `only` is its own
    // keep-N class: trimmed exactly like a tier, pooled with none of them.
    {
      const files: [string, number][] = [...seq("check-only-", 13), ...seq("check-fast-", 4)];
      const { dir, logs, ledger } = mk(files);
      writeFileSync(ledger, '{"v":1}\n');
      const r = retainLogs({ logsDir: logs, ledger, cited: () => new Set() });
      const left = new Set(readdirSync(logs));
      ok("retention_trims_only_logs_as_their_own_keep_class",
        r.deleted.length === 3 && r.deleted.every(f => f.startsWith("check-only-")) &&
        [0, 1, 2].every(i => !left.has(`check-only-00${i}.log`)) &&
        left.has("check-only-003.log") &&
        [0, 1, 2, 3].every(i => left.has(`check-fast-00${i}.log`)),
        JSON.stringify({ deleted: r.deleted, left: [...left].sort() }));
      rmSync(dir, { recursive: true });
    }

    // The ledger interlock is PER-FILE, not global. A non-empty ledger proves some history was
    // scraped, not that THIS log's was — and until check.ts emits rows itself the ledger only grows
    // when someone runs `--backfill` by hand, so every run leaves a log that reaches the end of the
    // window with its durations never captured. Absent from the ledger => kept, so the failure
    // direction is logs accumulating rather than history vanishing.
    {
      const body = "=== [fast] fmt — rustfmt check ===\n--- fmt: PASS  [753ms]\n";
      const { dir, logs, ledger } = mk(seq("check-fast-", 15), body);
      writeFileSync(ledger, ledgerFor(["check-fast-000.log", "check-fast-001.log"]));
      const r = retainLogs({ logsDir: logs, ledger, cited: () => new Set() });
      const left = new Set(readdirSync(logs));
      // 5 expired (000..004); only 000/001 are in the ledger, and only they may go.
      const held = ["check-fast-002.log", "check-fast-003.log", "check-fast-004.log"];
      ok("retention_keeps_an_expired_log_absent_from_the_ledger",
        r.deleted.length === 2 && r.deleted.every(f => ["check-fast-000.log", "check-fast-001.log"].includes(f)) &&
        r.unscrapedKept.length === 3 && held.every(f => left.has(f)) && r.kept === 13,
        JSON.stringify({ deleted: r.deleted, unscrapedKept: r.unscrapedKept, kept: r.kept }));

      // ...and the SAME candidate becomes deletable the moment a row naming it appears. This is the
      // half that proves the guard is a gate on the data, not a permanent refusal.
      writeFileSync(ledger, ledgerFor(["check-fast-002.log", "check-fast-003.log", "check-fast-004.log"]));
      const r2 = retainLogs({ logsDir: logs, ledger, cited: () => new Set() });
      ok("retention_releases_a_log_once_the_ledger_holds_its_rows",
        r2.deleted.length === 3 && r2.unscrapedKept.length === 0 &&
        held.every(f => !existsSync(join(logs, f))), JSON.stringify(r2));
      rmSync(dir, { recursive: true });
    }

    // ---- the W5 <-> retention coupling, end to end -----------------------------------------------
    // check.ts emits its rows through `runRow`/`gateRow`; retention releases an expired log only when
    // the ledger holds a row whose `log` names it. Those two facts meet at ONE field, and if they
    // ever stop meeting NOTHING fails: retention just classifies every log as unscraped and holds it
    // forever — the growth problem, back, silently, with every gate still green. So this calls the
    // REAL emitters (never a local copy of their shape, which would pin the copy instead) and asserts
    // the log they produce is the log retention then deletes.
    {
      const c = (log: string): RunContext => ({
        log, run: log.replace(/^check-fast-|\.log$/g, ""), tier: "fast", machine: "m0",
        commit: "abc1234", dirty: false, rustc: "1.96.1", gate_cache_enabled: true,
      });
      const body = "=== [fast] fmt — rustfmt check ===\n--- fmt: PASS  [753ms]\n";
      const { dir, logs, ledger } = mk(seq("check-fast-", 15), body);
      // Exactly what a run appends: the wall-less start row, a gate row, then the completed run row.
      const expiring = ["check-fast-000.log", "check-fast-001.log", "check-fast-002.log",
        "check-fast-003.log", "check-fast-004.log"];
      const emitted: Row[] = [];
      for (const log of expiring) {
        emitted.push(runRow(c(log)));
        emitted.push(gateRow(c(log), "fmt", "pass", 753));
        emitted.push(runRow(c(log), { wall_ms: 54_000, verdict: "pass" }));
      }
      writeFileSync(ledger, emitted.map(r => JSON.stringify(r)).join("\n") + "\n");
      const r = retainLogs({ logsDir: logs, ledger, cited: () => new Set() });
      // EVERY row, not merely one per run. Retention would be satisfied by a single naming row, so
      // asserting only its outcome would let a gate row silently lose the field — and `log` is also
      // half the upsert key, so a gate row without it re-scrapes to a DIFFERENT key and the ledger
      // grows a duplicate of every gate on the next `--backfill`. Both canaries (dropping `log` from
      // gate rows, then from run rows) were run against the retention assertion alone and PASSED;
      // this line is what makes them red.
      const orphan = emitted.filter(x => x.log === undefined || !expiring.includes(x.log));
      ok("emitted_rows_release_their_own_log_from_retention",
        orphan.length === 0 && r.deleted.length === 5 && r.unscrapedKept.length === 0 &&
        expiring.every(f => !existsSync(join(logs, f))),
        JSON.stringify({ orphan, deleted: r.deleted, unscrapedKept: r.unscrapedKept }));
      rmSync(dir, { recursive: true });

      // The start row and the end row are ONE row, not two. `rowKey` keys a run row on
      // (log, "#run"), so the completed row lands on the started one; a mis-keyed pair would double
      // every run in the ledger and, worse, leave a wall-less row that reads as a killed run.
      const one = upsert([runRow(c("check-fast-000.log"))],
        [runRow(c("check-fast-000.log"), { wall_ms: 54_000, verdict: "pass" })]);
      ok("a_runs_start_and_end_rows_converge_on_one_ledger_row",
        one.length === 1 && (one[0] as RunRow).wall_ms === 54_000 && (one[0] as RunRow).verdict === "pass",
        JSON.stringify(one));

      // The cell join, which is what keeps a live gate row from being strictly WORSE than a
      // backfilled one. `cacheCounterExample` reads `cells_cached` off GATE rows to decide whether
      // the ETA may say a warm cache does not shortcut a tier; live rows carried no counts, so every
      // run from here would have contributed nothing to it and the sentence would have gone quiet as
      // the backfilled history aged out, with no gate noticing.
      {
        const log = "check-full-2026-07-26T00-00-00Z.log";
        const cell = (over: Partial<CellRow>): CellRow => ({
          v: 1, kind: "cell", log, gate: "test", emitter: "feature_corpus_compiles",
          cell: "x/default", outcome: "hit", ms: 200, ts: 1, ...over,
        });
        const cells: CellRow[] = [
          cell({}), cell({}), cell({ outcome: "run_pass" }), cell({ outcome: "run_fail" }),
          // A replay gate's uncached work: real time, but NOT a cache miss. Counting it would make
          // "how warm was this run" mean something different depending on which gates ran, and would
          // diverge from the backfill parser, which only ever sees gate-cache roll-ups.
          cell({ gate: "corpus_decode_replay", emitter: "corpus_decode_replay", outcome: "ran" }),
          // Another run's rows, and an unattributed hand-run row: neither belongs to this run.
          cell({ log: "check-full-OTHER.log" }),
          cell({ log: undefined }),
        ];
        const counts = cellCountsFor(cells, log);
        const c2: RunContext = {
          log, run: "2026-07-26T00-00-00Z", tier: "full", machine: "m0", gate_cache_enabled: true,
        };
        const row = gateRow(c2, "test", "pass", 223_000, counts.get("test"));
        ok("cell_rows_join_back_onto_the_gate_row_that_owns_them",
          counts.get("test")?.cached === 2 && counts.get("test")?.run === 2 &&
          counts.get("corpus_decode_replay") === undefined &&
          row.cells_cached === 2 && row.cells_run === 2 &&
          // ...and the field order still matches the backfill parser's rows exactly.
          Object.keys(row).join(",") ===
            "v,kind,run,tier,gate,status,ms,gate_cache_enabled,cells_run,cells_cached,machine,log,src",
          JSON.stringify({ counts: [...counts], row }));
      }

      // Finding 3: `--backfill` must not undo a live row. Retention's own skip message tells the user
      // to run it, so a checkout with an unscraped backlog WILL re-scrape logs that live emission
      // already covered — and the prose row is lossy where the live one is exact (whole seconds above
      // a minute) and carries no commit/dirty/rustc.
      {
        const log = "check-fast-000.log";
        const c3: RunContext = {
          log, run: "000", tier: "fast", machine: "m0", commit: "abc1234", dirty: false,
          rustc: "1.96.1", gate_cache_enabled: true,
        };
        const live: Row[] = [runRow(c3, { wall_ms: 54_321, verdict: "pass" }), gateRow(c3, "fmt", "pass", 757)];
        const prose: Row[] = [
          { v: 1, kind: "run", run: "000", tier: "fast", machine: "m0", log, src: "backfill", wall_ms: 54_000, verdict: "pass" },
          { v: 1, kind: "gate", run: "000", tier: "fast", gate: "fmt", status: "pass", ms: 757, gate_cache_enabled: true, machine: "m0", log, src: "backfill" },
          // A gate the run never emitted for (it died first) still merges — the parser is the fallback.
          { v: 1, kind: "gate", run: "000", tier: "fast", gate: "clippy", status: "pass", ms: 147, gate_cache_enabled: true, machine: "m0", log, src: "backfill" },
        ];
        const merged = mergeBackfill(live, prose);
        const runRowAfter = merged.find(r => r.kind === "run") as RunRow;
        ok("backfill_never_overwrites_a_row_a_run_wrote",
          merged.length === 3 &&
          merged.every(r => (r.kind === "gate" && r.gate === "clippy") ? r.src === "backfill" : r.src === "run") &&
          runRowAfter.wall_ms === 54_321 && runRowAfter.commit === "abc1234" &&
          merged.some(r => r.kind === "gate" && r.gate === "clippy"),
          JSON.stringify(merged));
      }

      // ---- Finding 4: trimming, and the interlock that stops it deadlocking with retention --------
      {
        const mkRun = (n: number): Row[] => {
          const log = `check-fast-${String(n).padStart(3, "0")}.log`;
          return [
            { v: 1, kind: "run", run: String(n), tier: "fast", machine: "m0", log, src: "run", wall_ms: 54_000, verdict: "pass" },
            { v: 1, kind: "gate", run: String(n), tier: "fast", gate: "fmt", status: "pass", ms: 757, gate_cache_enabled: true, machine: "m0", log, src: "run" },
          ];
        };
        const rows: Row[] = Array.from({ length: 250 }, (_, i) => mkRun(i)).flat();

        // Newest survive, oldest go, and the count is exactly the window.
        {
          const kept = keptRunKeys(rows, { keepRuns: 200, logsOnDisk: new Set() });
          const out = trimRows(rows, kept);
          ok("trimming_keeps_the_newest_runs_and_drops_the_oldest",
            kept.size === 200 && out.length === 400 &&
            kept.has("check-fast-249.log") && kept.has("check-fast-050.log") &&
            !kept.has("check-fast-049.log") && !kept.has("check-fast-000.log"),
            JSON.stringify({ kept: kept.size, rows: out.length }));
        }

        // THE risky interaction. Retention deletes an expired log only once the ledger holds a row
        // naming it; trimming that row out would leave the log unreleasable forever and the two
        // mechanisms would deadlock, each waiting on the other, with every gate green throughout.
        // So a run whose log is STILL ON DISK survives however far outside the window it is.
        // Asserted END TO END rather than as a precondition: trim the ledger against what is really
        // on disk, then hand the trimmed ledger to retention and require it to actually DELETE the
        // out-of-window logs. A pin that only checked "the row survived" would still pass if
        // retention later stopped keying on that row.
        {
          const body = "=== [fast] fmt — rustfmt check ===\n--- fmt: PASS  [757ms]\n";
          // 15 logs on disk, named for runs 000-014 — i.e. all far OUTSIDE the 200-run window that
          // ends at 249. Retention expires the oldest 5 and needs their ledger rows to release them.
          const files = Array.from({ length: 15 }, (_, i): [string, number] =>
            [`check-fast-${String(i).padStart(3, "0")}.log`, 1_700_000_000 + i]);
          const { dir, logs, ledger } = mk(files, body);
          const onDisk = new Set(files.map(([f]) => f));
          const kept = keptRunKeys(rows, { keepRuns: 200, logsOnDisk: onDisk });
          const trimmed = trimRows(rows, kept);
          writeFileSync(ledger, trimmed.map(r => JSON.stringify(r)).join("\n") + "\n");
          const r = retainLogs({ logsDir: logs, ledger, cited: () => new Set() });
          ok("trimming_never_strands_a_log_retention_has_not_released",
            // Without the on-disk rule these 15 runs are trimmed away, retention sees no rows naming
            // them, and every one is held as unscraped forever — logs accumulating with all gates green.
            kept.size === 215 && r.deleted.length === 5 && r.unscrapedKept.length === 0 &&
            r.deleted.every(f => onDisk.has(f)),
            JSON.stringify({ kept: kept.size, deleted: r.deleted, unscrapedKept: r.unscrapedKept }));
          rmSync(dir, { recursive: true });
        }

        // Cells follow the ledger's kept runs, and an unattributed row — a suite someone ran by hand
        // with CDDL_TIMING_CELLS set — is never destroyed by a check.ts run that knows nothing of it.
        {
          const line = (log: string | undefined) => JSON.stringify({
            v: 1, kind: "cell", ...(log ? { log } : {}), gate: "test",
            emitter: "feature_corpus_compiles", cell: "x", outcome: "hit", ms: 1, ts: 1,
          });
          const lines = [line("check-fast-249.log"), line("check-fast-000.log"), line(undefined), "{not json", ""];
          const out = trimCellLines(lines, new Set(["check-fast-249.log"]));
          ok("cell_trimming_follows_the_ledger_and_spares_hand_run_rows",
            out.length === 2 && out.some(l => l.includes("249")) &&
            out.some(l => !l.includes("\"log\"")) && !out.some(l => l.includes("000")) &&
            !out.some(l => l.includes("not json")),
            JSON.stringify(out));
        }
      }

      // AC: `--update` consumes emitted rows with no branch for their provenance. The windows are the
      // whole consumer surface, so if a live row's field names or types drifted from the backfilled
      // ones this is where it shows up — as an empty window, which reads as "gate never ran".
      const live: Row[] = [
        runRow(c("check-fast-000.log"), { wall_ms: 54_000, verdict: "pass" }),
        gateRow(c("check-fast-000.log"), "fmt", "pass", 753),
      ];
      ok("emitted_rows_feed_the_update_rule_without_a_branch",
        JSON.stringify(windowFor(live, "fmt", "warm")) === "[753]" &&
        JSON.stringify(windowFor(live, "fmt", "cold")) === "[]" &&
        JSON.stringify(tierWindow(live, "fast")) === "[54000]",
        JSON.stringify({ warm: windowFor(live, "fmt", "warm"), tier: tierWindow(live, "fast") }));
    }

    // The escape hatch, or an unscrapeable log leaks forever: a run that died before its first gate
    // finished yields NO rows, so it can never appear in the ledger. "Does this log carry timings?"
    // is asked of the PARSER that would have scraped it — the parser defines what a row is, so the
    // two cannot drift apart the way a filename or size heuristic would.
    {
      const { dir, logs, ledger } = mk(seq("check-fast-", 15)); // default body: no gate lines at all
      writeFileSync(ledger, ledgerFor(["unrelated.log"]));
      const r = retainLogs({ logsDir: logs, ledger, cited: () => new Set() });
      ok("retention_still_deletes_a_log_that_never_had_rows_to_scrape",
        r.deleted.length === 5 && r.unscrapedKept.length === 0, JSON.stringify(r));
      rmSync(dir, { recursive: true });
    }

    // Nothing expired -> silent success, and the interlocks are never even consulted. This is what
    // keeps CI and a fresh checkout quiet: neither has a ledger, and neither has anything to delete.
    {
      const { dir, logs, ledger } = mk(seq("check-fast-", 4));
      let consulted = false;
      const r = retainLogs({ logsDir: logs, ledger, cited: () => { consulted = true; return new Set(); } });
      ok("retention_is_silent_and_asserts_nothing_when_nothing_expired",
        r.status === "ran" && r.deleted.length === 0 && r.kept === 4 && !consulted, JSON.stringify(r));
      rmSync(dir, { recursive: true });
    }
  }

  // ---- the run-start scratch sweep: the other IRREVERSIBLE operation -----------------------------
  // Deleting under a shared `tmpdir()` is the failure class AGENTS.md's pattern-kill rule names, one
  // level worse: a wrong `pkill` costs another session its run, a wrong `rm -rf` costs it its data.
  // So every bound is pinned here rather than reachable only by leaving a machine dirty for a day.
  // These run against a synthetic root, never the real `tmpdir()`.
  {
    const DAY = 24 * 60 * 60;
    const NOW = 1_800_000_000_000; // ms
    /** A synthetic scratch root: `[name, ageHours, kind]`, `dir` unless a name ends in `.lock`. */
    const mk = (entries: [string, number][]) => {
      const root = mkdtempSync(join(tmpdir(), "cddl-retention-sweep-"));
      for (const [name, ageHours] of entries) {
        const p = join(root, name);
        const t = NOW / 1000 - ageHours * 3600;
        if (name.endsWith(".lock")) writeFileSync(p, "");
        else { mkdirSync(p); writeFileSync(join(p, "payload"), "x".repeat(100)); utimesSync(join(p, "payload"), t, t); }
        utimesSync(p, t, t);
      }
      return root;
    };
    const sweep = (root: string, live: string[] | null, dryRun = false) =>
      sweepScratch({ root, now: NOW, liveNames: () => (live === null ? null : new Set(live)), dryRun });

    // The registry, not a glob. An unregistered name is another program's directory, and the sweep
    // must be incapable of touching it however stale it looks. A `.lock` is `acquire_scratch_lock`'s
    // sibling flock: unlinking one a live run holds lets a third run acquire a fresh inode.
    {
      const root = mk([
        ["cddl_codegen_stale_deadbeef", 48], ["cddl_verify_target_old", 72],
        ["somebody_elses_build_cache", 999], ["cddl-ish-but-unregistered", 999],
        ["cddl_codegen_ir_conformance_00.lock", 999],
      ]);
      const r = sweep(root, []);
      const left = new Set(readdirSync(root));
      ok("scratch_sweep_removes_only_registered_prefixes_and_never_a_lock",
        r.status === "ran" && r.removed.length === 2 &&
        !left.has("cddl_codegen_stale_deadbeef") && !left.has("cddl_verify_target_old") &&
        left.has("somebody_elses_build_cache") && left.has("cddl-ish-but-unregistered") &&
        left.has("cddl_codegen_ir_conformance_00.lock"),
        JSON.stringify({ r, left: [...left] }));
      rmSync(root, { recursive: true });
    }

    // Age, and age read from the NEWEST immediate child. A long-lived `CARGO_TARGET_DIR` keeps its
    // own mtime while cargo rewrites the files inside it, so reading the root alone would retire a
    // target dir that is amortising builds every day — a correctness-neutral bug that silently
    // deletes the whole point of path-keying those roots.
    {
      const root = mk([["cddl_codegen_wasm_matrix_00", 1], ["cddl_codegen_dead_00", 48]]);
      const fresh = join(root, "cddl_codegen_target_00");
      mkdirSync(fresh);
      writeFileSync(join(fresh, "hot"), "x");
      utimesSync(join(fresh, "hot"), NOW / 1000 - 60, NOW / 1000 - 60); // child touched a minute ago
      utimesSync(fresh, NOW / 1000 - 30 * DAY, NOW / 1000 - 30 * DAY);  // root itself a month old
      const r = sweep(root, []);
      ok("scratch_sweep_ages_an_entry_by_its_newest_child",
        r.removed.join() === "cddl_codegen_dead_00" && r.keptFresh === 2 &&
        existsSync(fresh) && existsSync(join(root, "cddl_codegen_wasm_matrix_00")),
        JSON.stringify({ r, left: readdirSync(root) }));
      rmSync(root, { recursive: true });
    }

    // A stale entry NAMED by a live process survives and is counted: a scratch root reaches its gate
    // through `CARGO_TARGET_DIR` as often as through argv, and a run whose clock says a day has
    // passed (a suspended laptop, a long `#[ignore]`d gate) is still a run.
    {
      const root = mk([["cddl_codegen_busy_00", 48], ["cddl_codegen_idle_00", 48]]);
      const r = sweep(root, ["CARGO_TARGET_DIR=/tmp/cddl_codegen_busy_00\0RUST_LOG=info"]);
      ok("scratch_sweep_spares_an_entry_named_by_a_live_process",
        r.removed.join() === "cddl_codegen_idle_00" && r.keptLive.join() === "cddl_codegen_busy_00" &&
        existsSync(join(root, "cddl_codegen_busy_00")),
        JSON.stringify({ r, left: readdirSync(root) }));
      rmSync(root, { recursive: true });
    }

    // The guard FAILS CLOSED. An unreadable `/proc` and "no process names anything" are
    // indistinguishable at the call site, and the difference between them is another session's run.
    {
      const root = mk([["cddl_codegen_stale_00", 48]]);
      const r = sweep(root, null);
      ok("scratch_sweep_fails_closed_when_the_live_process_scan_fails",
        r.status === "skipped" && r.removed.length === 0 && existsSync(join(root, "cddl_codegen_stale_00")) &&
        /live-process scan failed/.test(r.reason ?? ""),
        JSON.stringify(r));
      rmSync(root, { recursive: true });
    }

    // Nothing stale -> silent success, and the guard is never even consulted. CI and a fresh checkout
    // must stay quiet, and a sweep that scanned `/proc` on every invocation to say nothing is waste.
    {
      const root = mk([["cddl_codegen_fresh_00", 1]]);
      let consulted = false;
      const r = sweepScratch({ root, now: NOW, liveNames: () => { consulted = true; return new Set(); } });
      ok("scratch_sweep_is_silent_and_consults_nothing_when_nothing_is_stale",
        r.status === "ran" && r.removed.length === 0 && r.keptFresh === 1 && !consulted, JSON.stringify(r));
      rmSync(root, { recursive: true });
    }

    // The threshold is a documented choice, not an accident: >40x the longest measured tier, so no
    // live run's scratch can reach it even on a machine whose clock jumped.
    ok("the_scratch_age_threshold_is_a_day",
      SCRATCH_MAX_AGE_MS === 86_400_000, String(SCRATCH_MAX_AGE_MS));

    // The registry is only honest while it still covers the mint sites. This walks the `temp_dir()`
    // / `tmpdir()` joins in the repo and fails on any scratch name no registered prefix covers — the
    // failure direction being a new mint site that LEAKS, which is invisible until a disk fills.
    // A site whose name this walker cannot resolve is a FAILURE too: an unreadable site is exactly
    // where a new spelling would hide.
    {
      const { uncovered, unresolved } = scratchMintSites();
      ok("the_scratch_prefix_registry_covers_every_temp_dir_mint_site",
        uncovered.length === 0 && unresolved.length === 0,
        JSON.stringify({ uncovered: uncovered.slice(0, 8), unresolved: unresolved.slice(0, 8) }));
    }
  }

  // ---- `--only` selection, and the argv shape it needs ------------------------------------------
  // The fence's refusal half. Each of these is a way a partial run could quietly assert more than it
  // ran: an id nobody has, a gate this tier never offers, or half of a pair whose other half writes
  // what it reads. They live here for the same reason the retention tests do — pure functions of
  // check.ts, testable without running a gate.
  {
    const msg = (r: SelectionResult): string => (r.ok ? "" : r.message);
    const reg: Gate[] = [
      { id: "alpha", tier: "fast", kind: "cmd", desc: "alpha" },
      { id: "beta", tier: "fast", kind: "cmd", desc: "beta",
        requires: [{ gate: "alpha", why: "it diffs what alpha rewrites" }] },
      { id: "gamma", tier: "full", kind: "cmd", desc: "gamma" },
    ];
    const r1 = resolveSelection(["beta", "alpha"], "fast", reg);
    ok("selection_accepts_an_in_tier_set_carrying_its_prerequisite",
      r1.ok && [...r1.ids].sort().join() === "alpha,beta", JSON.stringify(r1));
    const r2 = resolveSelection(["nope"], "fast", reg);
    ok("selection_refuses_an_unknown_gate_id_naming_the_known_ones",
      !r2.ok && /unknown gate 'nope'/.test(msg(r2)) && /alpha, beta, gamma/.test(msg(r2)), msg(r2));
    const r3 = resolveSelection(["gamma"], "fast", reg);
    ok("selection_refuses_an_out_of_tier_gate_naming_the_tier_to_run",
      !r3.ok && /'gamma' is not in the 'fast' tier/.test(msg(r3)) && /check\.ts full --only gamma/.test(msg(r3)),
      msg(r3));
    const r4 = resolveSelection(["beta"], "fast", reg);
    ok("selection_refuses_a_dependency_split_naming_the_prerequisite",
      !r4.ok && /requires 'alpha'/.test(msg(r4)) && /diffs what alpha rewrites/.test(msg(r4)), msg(r4));
    // The REAL registry's two order-dependent pairs — enumerated from the gates whose verdict reads
    // a file or cache another gate WRITES in the same run. A dropped `requires:` would restore the
    // silent-vacuous-pass this fence exists to refuse, and nothing else would notice.
    const real = [resolveSelection(["coverage_md_diff"], "fast"), resolveSelection(["verify_cache_transparency"], "full")];
    ok("the_registrys_order_dependent_pairs_refuse_to_split",
      real.every(r => !r.ok) && /requires 'project_corpus'/.test(msg(real[0]!)) &&
      /requires 'verify'/.test(msg(real[1]!)),
      JSON.stringify(real.map(msg)));
    // Both spellings, and — the reason argv is walked rather than filtered — the space form's value
    // must not be mistaken for the tier positional.
    const a1 = parseArgv(["full", "--only", "verify,corpus_detect", "--keep-going"]);
    const a2 = parseArgv(["--only=verify", "--only", "corpus_detect", "fast"]);
    ok("argv_parses_both_only_spellings_without_eating_the_tier",
      a1.positional.join() === "full" && a1.only!.join() === "verify,corpus_detect" && a1.flags.has("--keep-going") &&
      a2.positional.join() === "fast" && a2.only!.join() === "verify,corpus_detect",
      JSON.stringify({ a1: { ...a1, flags: [...a1.flags] }, a2: { ...a2, flags: [...a2.flags] } }));
  }

  // ---- check.ts's gate scheduler --------------------------------------------------------------
  // These live here for the same reason `retainLogs`/`etaLines` do: this is the one gate that runs
  // pure self-tests over check.ts's exported helpers, and a scheduler defect is exactly the class
  // that a full-tier run would surface hours later, or not at all (the pool's own liveness cannot be
  // observed from a tier that never fails).
  {
    const g = (id: string, concurrent?: string): Gate =>
      ({ id, tier: "full", kind: "cmd", desc: id, ...(concurrent !== undefined ? { concurrent } : {}) });

    // Default-sequential, and an UNGROUPED gate is a barrier. Both halves matter: the barrier is what
    // keeps `verify` before `verify_cache_transparency` and the fmt→clippy→build→test chain ordered,
    // and it is why a group must be contiguous to mean anything.
    {
      const plan = planBatches([g("a"), g("b", "grp"), g("c", "grp"), g("d"), g("e", "grp")]);
      ok("batches_group_only_contiguous_declared_gates",
        plan.length === 4 &&
        plan[0]!.gates.map(x => x.id).join() === "a" && plan[0]!.group === undefined &&
        plan[1]!.gates.map(x => x.id).join() === "b,c" && plan[1]!.group === "grp" &&
        plan[2]!.gates.map(x => x.id).join() === "d" &&
        plan[3]!.gates.map(x => x.id).join() === "e",
        JSON.stringify(plan.map(b => ({ group: b.group, gates: b.gates.map(x => x.id) }))));
    }
    {
      const plan = planBatches([g("a"), g("b"), g("c")]);
      ok("an_undeclared_registry_is_entirely_sequential",
        plan.length === 3 && plan.every(b => b.gates.length === 1 && b.group === undefined),
        JSON.stringify(plan.map(b => b.gates.map(x => x.id))));
    }

    // Longest-first, because a skewed batch's wall is its tail: dispatching the longest gate last
    // adds nearly its whole duration after everything else has drained. An UNMEASURED gate sorts
    // last and ties fall back to registry order — the hint is an optimisation, never a correctness
    // input, so a missing row must degrade to "run it in the order it was written".
    {
      const hint = (id: string) => ({ big: 600_000, mid: 300_000, tie1: 100_000, tie2: 100_000 } as Record<string, number>)[id];
      const order = longestFirst([g("tie1"), g("new"), g("mid"), g("tie2"), g("big")], hint).map(x => x.id);
      ok("dispatch_is_longest_first_with_registry_order_as_the_tiebreak",
        order.join() === "big,mid,tie1,tie2,new", order.join());
    }

    // The degree bound is what makes this safe on a box with ~6 GiB free, so it is asserted as an
    // observed maximum rather than trusted from the pool's shape.
    {
      const items = Array.from({ length: 12 }, (_, i) => ({ id: `g${i}` }));
      let live = 0;
      let peak = 0;
      const r = await runPool(items, 3, async () => {
        peak = Math.max(peak, ++live);
        await new Promise(res => setTimeout(res, 1));
        live--;
        return "PASS";
      });
      ok("pool_never_exceeds_its_degree_and_runs_everything",
        peak === 3 && r.size === 12, `peak=${peak} results=${r.size}`);
    }

    // Fail-fast with gates in flight: nothing NEW starts, and what was already running is kept. The
    // summary contract depends on the difference — "ran and failed", "ran and passed under load", and
    // "never ran" have to stay three distinguishable things.
    {
      const items = Array.from({ length: 8 }, (_, i) => ({ id: `g${i}` }));
      const started: string[] = [];
      const r = await runPool(items, 2, async item => {
        started.push(item.id);
        await new Promise(res => setTimeout(res, 5));
        return item.id === "g0" ? "FAIL" : "PASS";
      }, { stopAfter: res => res === "FAIL" });
      ok("fail_fast_stops_dispatch_but_keeps_in_flight_results",
        started.length < items.length && r.get("g0") === "FAIL" && r.get("g1") === "PASS" &&
        [...r.keys()].every(k => started.includes(k)),
        `started=${JSON.stringify(started)} results=${JSON.stringify([...r])}`);
    }

    // THE HANG PIN. Two ad-hoc probe scripts written while measuring this feature backgrounded a
    // sampler beside their gates and then used a bare `wait`, which waits for EVERY background job:
    // both finished all their work, wrote every verdict, and then hung without a summary — one for
    // ~5 hours. So the join must await its OWN handles and nothing else, and must still return when
    // a timeout is armed. The never-settling promise and the live interval are exactly the two
    // shapes that would keep an over-broad join (or an uncleared timer) alive.
    {
      const interval = setInterval(() => {}, 1_000);
      const neverSettles = new Promise<void>(() => {});
      void neverSettles;
      const r = await runPool([{ id: "x" }, { id: "y" }], 2, async item => item.id, { timeoutMs: 60_000 });
      clearInterval(interval);
      ok("the_join_awaits_only_its_own_handles", r.size === 2 && r.get("x") === "x" && r.get("y") === "y",
        JSON.stringify([...r]));
    }

    // A join that DOES time out reports the still-in-flight items and returns, rather than hanging
    // forever. This is the guard's whole purpose: a runner bug must be loud and terminating.
    {
      const pending: string[] = [];
      // The abandoned work's own timer is kept SHORT on purpose: it outlives the pool by definition,
      // and bun will not exit while it is pending — which is the event-loop half of the same
      // hang-after-success hazard, seen from the other side.
      const r = await runPool([{ id: "slow" }], 1, async () => {
        await new Promise(res => setTimeout(res, 200));
        return "PASS";
      }, { timeoutMs: 25, onTimeout: inFlight => pending.push(...inFlight.map(i => i.id)) });
      ok("a_stuck_join_names_what_is_in_flight_and_returns",
        pending.join() === "slow" && r.size === 0, `pending=${JSON.stringify(pending)} results=${r.size}`);
    }

    ok("jobs_and_join_timeout_fall_back_to_their_defaults",
      parseJobs(undefined).jobs === 4 && parseJobs("  ").jobs === 4 && parseJobs("1").jobs === 1 &&
      parseJobs("8").jobs === 8 && parseJobs("0").warning !== undefined &&
      parseJobs("nope").jobs === 4 && parseJobs("nope").warning !== undefined &&
      parseJoinTimeoutMs(undefined) === 3 * 60 * 60 * 1000 && parseJoinTimeoutMs("30") === 30_000 &&
      parseJoinTimeoutMs("-1") === 3 * 60 * 60 * 1000,
      JSON.stringify([parseJobs("0"), parseJobs("nope"), parseJoinTimeoutMs("30")]));

    // ---- the PEAK-RESOURCE bound ---------------------------------------------------------------
    // `CHECK_JOBS` bounds the gate count; what overcommits a machine is the PRODUCT of gate count,
    // rustc-per-gate and per-rustc footprint. These pin the arithmetic that keeps the product from
    // scaling with `nproc` — the defect that made a 32-core / 32 GiB box unresponsive under a full
    // tier. Pure inputs, because the alternative is discovering a regression from a locked-up
    // machine.
    {
      // Derived from MEMORY, never cores: the same core count against half the RAM must halve it.
      // The basis is AVAILABLE memory — `at` feeds it as `memAvailGiB`, which is what a real batch
      // passes; `memTotalGiB` survives only as the fallback for a machine that cannot report avail.
      const at = (gib: number, gates: number) => cargoJobsForBatch({ gatesInFlight: gates, memAvailGiB: gib }).jobs;
      ok("cargo_jobs_derive_from_memory_and_shrink_as_gates_overlap",
        at(32, 4) === 1 && at(32, 1) === 4 && at(32, 8) === 1 && at(16, 4) === 1 && at(128, 4) === 4,
        JSON.stringify({ g32x4: at(32, 4), g32x1: at(32, 1), g32x8: at(32, 8), g16x4: at(16, 4), g128x4: at(128, 4) }));

      // The basis is what is FREE, not what the machine has. A busy 32 GiB box with 8 GiB available
      // must budget like an 8 GiB box — this is the bound that two whole-machine freezes went
      // through, because a MemTotal basis reads a loaded machine as an idle one.
      const busy = cargoJobsForBatch({ gatesInFlight: 1, memTotalGiB: 32, memAvailGiB: 8 });
      const idle = cargoJobsForBatch({ gatesInFlight: 1, memTotalGiB: 32, memAvailGiB: 30 });
      ok("the_budget_follows_available_memory_not_machine_size",
        busy.jobs === 1 && idle.jobs === 3 && busy.why.includes("MemAvailable"),
        JSON.stringify({ busy: busy.jobs, idle: idle.jobs, why: busy.why }));

      // MemTotal is the FALLBACK, used only when avail is unreadable — never preferred over it.
      const fellBack = cargoJobsForBatch({ gatesInFlight: 1, memTotalGiB: 32 });
      ok("mem_total_is_only_the_fallback_basis",
        fellBack.jobs === 4 && fellBack.why.includes("MemTotal") && !fellBack.why.includes("MemAvailable"),
        JSON.stringify(fellBack));

      // Floored, never rounded: a 3.5-slot budget is 3. Rounding up spends the headroom the
      // fraction exists to reserve, which is the wrong direction to be wrong in.
      ok("a_fractional_budget_floors_rather_than_rounding_up",
        cargoJobsForBatch({ gatesInFlight: 1, memAvailGiB: 28 }).jobs === 3,
        JSON.stringify(cargoJobsForBatch({ gatesInFlight: 1, memAvailGiB: 28 })));

      // The product is the invariant, so assert it directly rather than trusting the divisions above.
      // Above `gatesInFlight = budget` the floor of one job per gate takes over — a gate handed
      // `-j0` would build nothing — so the bound there is the gate count itself, which `CHECK_JOBS`
      // already governs. What must never happen is the product tracking `nproc`.
      const productAt = (gib: number, gates: number) => at(gib, gates) * gates;
      // Stated as the INVARIANT rather than as numbers, so it keeps meaning what it means when the
      // budget constants move: the budget for a machine is what one gate alone may spend, and the
      // crossover to "one job per gate" sits exactly there. Hardcoding either side re-pins the
      // arithmetic to today's constants and has to be rewritten every time they change — which is
      // how a bound gets loosened by whoever is updating the test to make it pass.
      const budgetAt = (gib: number) => at(gib, 1);
      const holds = (gib: number) => {
        const b = budgetAt(gib);
        const under = [1, 2, 3, 4, 8, 16, 32].filter(g => g <= b).every(g => productAt(gib, g) <= b);
        const over = [1, 2, 3, 4, 8, 16, 32].filter(g => g > b).every(g => productAt(gib, g) === g);
        return under && over;
      };
      ok("the_bounded_product_never_exceeds_the_memory_budget",
        holds(16) && holds(32) && holds(64) && holds(128),
        JSON.stringify({
          budgets: [16, 32, 64, 128].map(budgetAt),
          at32: [1, 2, 3, 4, 8, 16, 32].map(g => productAt(32, g)),
        }));

      // A tiny machine still runs: floor of 1, never 0, or the batch would spawn a cargo that
      // refuses to build.
      ok("cargo_jobs_never_reach_zero",
        at(1, 16) === 1 && at(0.5, 1) === 1, JSON.stringify([at(1, 16), at(0.5, 1)]));

      // Overrides. An operator's explicit `CHECK_CARGO_JOBS` wins outright (a big box must be able to
      // raise it); an inherited `CARGO_BUILD_JOBS` can only hold the derived value DOWN — someone
      // being gentle to their machine said something the runner must not undo, while someone who
      // exported 32 for an unrelated reason never knew about the batch.
      const ov = cargoJobsForBatch({ gatesInFlight: 4, memAvailGiB: 32, override: "12" });
      const low = cargoJobsForBatch({ gatesInFlight: 4, memAvailGiB: 32, inherited: "1" });
      const high = cargoJobsForBatch({ gatesInFlight: 4, memAvailGiB: 32, inherited: "32" });
      const bad = cargoJobsForBatch({ gatesInFlight: 4, memAvailGiB: 32, override: "nope" });
      ok("explicit_override_wins_and_an_inherited_setting_only_holds_it_down",
        ov.jobs === 12 && low.jobs === 1 && high.jobs === 1 && bad.jobs === 1 && bad.warning !== undefined,
        JSON.stringify({ ov: ov.jobs, low: low.jobs, high: high.jobs, bad }));

      // An unreadable MemTotal must not mean "unbounded" — that is exactly the pre-fix behaviour.
      const blind = cargoJobsForBatch({ gatesInFlight: 4, memTotalGiB: undefined, memAvailGiB: undefined });
      ok("an_unmeasurable_machine_still_gets_a_bound", blind.jobs === 2, JSON.stringify(blind));

      // And the real reading is sane on whatever box this runs on (or absent on a non-Linux one).
      const real = memTotalGiB();
      ok("mem_total_reads_as_a_plausible_size_or_not_at_all",
        real === undefined || (real > 0.5 && real < 4096), String(real));
      ok("mem_total_parser_reads_the_kb_unit",
        memTotalGiB("MemTotal:       33886140 kB\nMemFree: 1 kB\n")! > 32 &&
        memTotalGiB("MemTotal:       33886140 kB\nMemFree: 1 kB\n")! < 33,
        String(memTotalGiB("MemTotal:       33886140 kB\n")));
    }

    // ---- the resource preflight ------------------------------------------------------------------
    // A tier commits to its peak resource in its first seconds and cannot discover a memory cap
    // mid-run; the alternative to checking up front is a machine that stops responding and gets
    // power-cycled, which destroys the run and everything else on the box. These pin the branches,
    // because the only other way to reach them is to genuinely run a machine out of memory.
    {
      const p = (o: Partial<Parameters<typeof preflightDecision>[0]>) =>
        preflightDecision({ tier: "full", jobs: 4, skip: false, memAvailGiB: 20, diskAvailGiB: 200, ...o });

      // `fast` is what CI runs: it spawns no batch and mints no scratch, and must NOT acquire a way
      // to refuse to start on a runner whose disk this tool cannot reason about.
      ok("the_fast_tier_is_never_gated_on_a_floor",
        p({ tier: "fast", memAvailGiB: 0.1, diskAvailGiB: 0.1 }).action === "proceed",
        JSON.stringify(p({ tier: "fast", memAvailGiB: 0.1, diskAvailGiB: 0.1 })));

      // Memory DEGRADES: a slow tier beats no tier. Disk REFUSES: going sequential creates no space,
      // and every nested-cargo gate downstream would die on ENOSPC tens of minutes in.
      ok("a_low_memory_machine_degrades_to_sequential_rather_than_refusing",
        p({ memAvailGiB: 5 }).action === "degrade" && p({ memAvailGiB: 5 }).jobs === 1,
        JSON.stringify(p({ memAvailGiB: 5 })));
      ok("a_full_scratch_volume_refuses_before_the_tier_starts",
        p({ diskAvailGiB: 3 }).action === "refuse", JSON.stringify(p({ diskAvailGiB: 3 })));
      ok("memory_so_low_that_even_one_cargo_would_thrash_refuses",
        p({ memAvailGiB: 1 }).action === "refuse", JSON.stringify(p({ memAvailGiB: 1 })));

      // An UNMEASURABLE machine proceeds. A preflight that refused whenever it could not read
      // `/proc/meminfo` would make the runner unusable wherever the measurement is the thing missing.
      ok("an_unmeasurable_machine_proceeds_rather_than_refusing",
        p({ memAvailGiB: undefined, diskAvailGiB: undefined }).action === "proceed",
        JSON.stringify(p({ memAvailGiB: undefined, diskAvailGiB: undefined })));

      // Degrading an ALREADY sequential run is a no-op, not a second announcement.
      ok("an_already_sequential_run_is_not_degraded_again",
        p({ memAvailGiB: 5, jobs: 1 }).action === "proceed", JSON.stringify(p({ memAvailGiB: 5, jobs: 1 })));

      // The override is a real escape hatch — it must survive both floors, or a machine the tool
      // measures wrongly has no way to run at all.
      ok("skip_preflight_overrides_both_floors",
        p({ skip: true, memAvailGiB: 0.1, diskAvailGiB: 0.1 }).action === "proceed" &&
        p({ skip: true, memAvailGiB: 0.1, diskAvailGiB: 0.1 }).jobs === 4,
        JSON.stringify(p({ skip: true, memAvailGiB: 0.1, diskAvailGiB: 0.1 })));

      ok("a_healthy_machine_proceeds_with_the_requested_jobs",
        p({}).action === "proceed" && p({}).jobs === 4, JSON.stringify(p({})));
    }
  }
  return t;
}

// ==================================================================================================
// CLI
// ==================================================================================================
function report(): void {
  const d = readDigest();
  const seeded = d.gates.filter(g => g.warm_ms !== undefined).sort((a, b) => b.warm_ms! - a.warm_ms!);
  console.log("gate durations (warm cache), slowest first — measured, not remembered:");
  for (const g of seeded) console.log(`  ${g.gate.padEnd(46)} ${compactDur(g.warm_ms!).padStart(8)}  (n=${g.n})`);
  const unseeded = d.gates.filter(g => g.warm_ms === undefined).map(g => g.gate);
  if (unseeded.length) console.log(`\nnot yet measured (no passing runs in the local ledger): ${unseeded.join(", ")}`);
  for (const t of d.tiers)
    console.log(`tier=${t.tier.padEnd(5)} wall ${t.wall_ms !== undefined ? compactDur(t.wall_ms) : "(not yet measured)"}${t.n ? `  (n=${t.n})` : ""}`);
}

async function main(): Promise<void> {
  const argv = process.argv.slice(2);
  const mode = argv.find(a => a.startsWith("--")) ?? "";

  if (mode === "--backfill") {
    const r = backfill();
    const before = readLedger();
    const merged = mergeBackfill(before, r.rows);
    writeLedger(merged);
    const liveHeld = new Set(before.filter(x => x.src === "run").map(x => x.log).filter(Boolean));
    if (liveHeld.size)
      console.log(`backfill: left ${liveHeld.size} run(s) already recorded live untouched (a live row beats a re-parsed one)`);
    const gateRows = merged.filter(x => x.kind === "gate").length;
    console.log(`backfill: parsed ${r.parsed} log(s) -> ${gateRows} gate row(s) + ${merged.length - gateRows} run row(s) in draft/timings.jsonl`);
    console.log(`backfill: skipped ${r.skipped.length} log(s)`);
    for (const s of r.skipped) console.log(`  SKIP ${s.log}: ${s.reason}`);
    return;
  }

  if (mode === "--update") {
    // Standalone: nobody has regenerated the derived spans, so the hint names the writer that does.
    // check.ts calls `runDigestUpdate` directly and runs that writer itself.
    if (runDigestUpdate().changed)
      console.log("timings: commit tests/timings.json together with the spans it derives — " +
        "run `bun run project_status_headers.ts --write` and commit both");
    return;
  }

  if (mode === "--check") {
    // A check that THROWS is still a red gate, but a bare stack trace names no check. Catching here
    // costs nothing and keeps the failure attributable to the suite rather than to bun.
    let results: TestResult[];
    try {
      results = await selfTests();
    } catch (e) {
      console.log(`RESULT: FAIL — a timings check threw before reporting: ${e instanceof Error ? e.stack ?? e.message : String(e)}`);
      process.exit(1);
    }
    for (const r of results) console.log(`  ${r.ok ? "ok  " : "FAIL"} ${r.name}${r.ok || !r.detail ? "" : ` — ${r.detail}`}`);
    const bad = results.filter(r => !r.ok);
    if (bad.length) {
      console.log(`RESULT: FAIL — ${bad.length}/${results.length} timings check(s) failed`);
      process.exit(1);
    }
    console.log(`RESULT: PASS — ${results.length} timings check(s); durations asserted: none, by design`);
    return;
  }

  if (mode) { console.error(`project_timings.ts: unknown flag '${mode}' (--backfill | --update | --check)`); process.exit(2); }
  report();
}

if (import.meta.main) await main();
