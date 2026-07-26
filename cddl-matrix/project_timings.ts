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
  existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, statSync, utimesSync,
  writeFileSync,
} from "node:fs";
import { createHash } from "node:crypto";
import { cpus, hostname, tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { REGISTRY, etaLines, retainLogs } from "../check.ts";

const HERE = import.meta.dir;
const ROOT = resolve(HERE, "..");
const LOGS_DIR = join(ROOT, "draft", "logs");
const LEDGER = join(ROOT, "draft", "timings.jsonl");
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

export function writeLedger(rows: Row[], path = LEDGER): void {
  mkdirSync(join(path, ".."), { recursive: true });
  writeFileSync(path, rows.map(r => JSON.stringify(r)).join("\n") + (rows.length ? "\n" : ""));
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
          `(n=${w.length}) — tests/timings.json updated, commit it`,
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
        `(n=${w.length}) — tests/timings.json updated, commit it`,
      );
    } else if (d.note) notes.add(d.note);
    nextTiers.push(row);
  }

  const digest: Digest = { ...prior, gates: next, tiers: nextTiers };
  const changed = serializeDigest(digest) !== serializeDigest(prior);
  return { writes, notes: [...notes], digest, changed };
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

export function selfTests(): TestResult[] {
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

function main(): void {
  const argv = process.argv.slice(2);
  const mode = argv.find(a => a.startsWith("--")) ?? "";

  if (mode === "--backfill") {
    const r = backfill();
    const merged = upsert(readLedger(), r.rows);
    writeLedger(merged);
    const gateRows = merged.filter(x => x.kind === "gate").length;
    console.log(`backfill: parsed ${r.parsed} log(s) -> ${gateRows} gate row(s) + ${merged.length - gateRows} run row(s) in draft/timings.jsonl`);
    console.log(`backfill: skipped ${r.skipped.length} log(s)`);
    for (const s of r.skipped) console.log(`  SKIP ${s.log}: ${s.reason}`);
    return;
  }

  if (mode === "--update") {
    const rows = readLedger();
    const prior = readDigest();
    const u = updateDigest(rows, prior);
    for (const n of u.notes) console.log(n);
    if (u.changed) {
      writeFileSync(DIGEST, serializeDigest(u.digest));
      for (const w of u.writes) console.log(w);
      if (!u.writes.length) console.log("timings: tests/timings.json row set now matches the check.ts registry — commit it");
    }
    return;
  }

  if (mode === "--check") {
    // A check that THROWS is still a red gate, but a bare stack trace names no check. Catching here
    // costs nothing and keeps the failure attributable to the suite rather than to bun.
    let results: TestResult[];
    try {
      results = selfTests();
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

if (import.meta.main) main();
