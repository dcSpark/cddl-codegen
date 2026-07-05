#!/usr/bin/env bun
/**
 * Q5 matrix-self-completeness query (QUERIES.md Q5) — PURE FILE READS, no cargo, no oracles.
 *
 * Answers QUERIES.md Q5: "every construct defined by CDDL profile P (grammar ∪ prelude ∪ control-op
 * registry) that the matrix does NOT model." It is the standing, projectable form of `verify.ts`'s
 * bidirectional reconciliation lint (README.md § "Why it's provably comprehensive") — the same
 * forward/backward completeness check, but read from committed files only so it runs without the
 * oracle toolchain. It reads:
 *   1. `matrix.json` — `features` (`{id, production, alt, profile}`, the modelled construct universe)
 *      and `control_operators` (the IANA registry axis, derived complete-by-construction).
 *   2. `sources/cddl-1-1-update.abnf` — the pinned grammar. The ABNF *alternatives* are NOT embedded in
 *      matrix.json (only each feature's `alt` string is), so the forward lint re-extracts them here with
 *      the SAME block/alt/normalize logic verify.ts uses (kept in sync deliberately — see below).
 *   3. `sources/cddl.prelude` — the pinned standard prelude (RFC 8610 App. D) type names.
 *
 * THE TWO DIRECTIONS (neither alone is sufficient — forward-only admits invented features,
 * backward-only admits coverage holes):
 *   - **forward** (source → feature): every grammar ALTERNATIVE has ≥1 covering feature row. This is
 *     the direction that catches a construct the matrix forgot to model.
 *   - **backward** (feature → source): every feature's `production` resolves to a real first-party
 *     source — an ABNF production, the prelude pseudo-production, the IANA control-op registry, or the
 *     `CDDL_CODEGEN` vendor source (comment-DSL / sentinel). Catches an invented feature with no source.
 *
 * FORWARD LINT — HARD for `type2` ONLY (the honest coverage boundary, ROADMAP.md § 4):
 *   - `type2`'s 12 ABNF alternatives are the closed, hard-gated set (this is the check that caught the
 *     missing `#7` alternative). Q5 is AUTHORITATIVE here: an uncovered `type2` alternative is a real
 *     modelling gap and `--check` hard-fails on it.
 *   - Every OTHER production's alt-coverage is BEST-EFFORT and clearly labelled soft: `normalizeAlt`
 *     does exact-string matching after whitespace/`S` stripping, so it reports `rule` / `type1` /
 *     `genericarg` / `genericparm` / `assignt` / `assigng` as false-uncovered even though feature rows
 *     exist, and `head-number`'s semantics live under the `type2.*` rows so it renders "NOT MODELED"
 *     with zero rows. These soft columns are diagnostics, not gates. Fixing `normalizeAlt` (and adding
 *     the missing variation rows called out in ROADMAP § 4) is a SEPARATE item — this query does NOT
 *     touch it; it reports the soft coverage with the caveat so the noise is never mistaken for a gap.
 *
 * BACKWARD LINT + PRELUDE/CONTROL-OP COMPLETENESS — all HARD:
 *   - every feature resolves to a source (else it is invented) — `--check` hard-fails;
 *   - every prelude name has a `prelude.<name>` feature row — `--check` hard-fails on a gap;
 *   - the control-op axis is complete BY CONSTRUCTION (its rows ARE the IANA registry, imported by
 *     `build_matrix.ts`), so Q5 asserts the shape (37 ops, each with an id) rather than reconciling.
 *
 * Run from cddl-matrix/:
 *   bun run query_q5_completeness.ts          -> the readable completeness report (both directions)
 *   bun run query_q5_completeness.ts type2    -> only productions/features whose id contains "type2"
 *   bun run query_q5_completeness.ts --check   -> hard-fail on any uncovered type2 alt / unresolved
 *                                                 feature source / prelude gap + vacuity floor; exit nonzero
 */
import { readFileSync } from "node:fs";

const HERE = import.meta.dir;
const splitlines = (s: string): string[] => s.split(/\r?\n/);

// --- matrix.json ----------------------------------------------------------------------------------
interface FeatureRow { id: string; production?: string | null; alt?: string | null; profile?: string }
interface CtlRow { id: string; name?: string }
interface MatrixJson { features: FeatureRow[]; control_operators: CtlRow[] }
const matrix = JSON.parse(readFileSync(`${HERE}/matrix.json`, "utf8")) as MatrixJson;
const features = matrix.features;
const control_ops = matrix.control_operators;

// --- pinned sources -------------------------------------------------------------------------------
const abnfText = readFileSync(`${HERE}/sources/cddl-1-1-update.abnf`, "utf8");
const preludeText = readFileSync(`${HERE}/sources/cddl.prelude`, "utf8");
const PRELUDE_PSEUDO = "prelude";
const VENDOR_PRODUCTIONS = new Set(["comment_dsl", "sentinel"]); // CDDL_CODEGEN vendor source

// ABNF production names (LHS of every `name = …` line).
const abnf_productions = new Set<string>();
for (const line of splitlines(abnfText)) {
  const m = line.match(/^([A-Za-z][A-Za-z0-9_-]*)\s*=/);
  if (m) abnf_productions.add(m[1]);
}
// Prelude type names.
const prelude_names: string[] = [];
for (const line of splitlines(preludeText)) {
  const m = line.match(/^([A-Za-z][A-Za-z0-9_.-]*)\s*=/);
  if (m) prelude_names.push(m[1]);
}
const prelude_name_set = new Set(prelude_names);

// Control-op production names (feature.production may name an op, e.g. a `.size`-shaped feature).
const controlop_prod_names = new Set<string>([
  ...control_ops.map(co => co.name ?? ""),
  ...control_ops.map(co => (co.name ?? "").replace(/^\.+/, "")),
]);

// --- ABNF alternative extraction — SAME logic as verify.ts (kept in sync deliberately) -------------
// This forward lint must produce byte-identical alternatives to verify.ts's reconciliation so the two
// never disagree about what "covered" means. normalizeAlt is COPIED AS-IS: fixing its exact-string
// noise is a separate ROADMAP § 4 item, not this query's job (see header).
function stripComment(s: string): string {
  let out = "", inQ = false;
  for (const ch of s) {
    if (ch === '"') inQ = !inQ;
    if (ch === ";" && !inQ) break;
    out += ch;
  }
  return out.replace(/\s+$/, "");
}
function splitTopAlts(s: string): string[] {
  const alts: string[] = [];
  let buf = "", depth = 0, inQ = false;
  for (const ch of s) {
    if (inQ) { buf += ch; if (ch === '"') inQ = false; }
    else if (ch === '"') { inQ = true; buf += ch; }
    else if (ch === "(" || ch === "[" || ch === "{") { depth++; buf += ch; }
    else if (ch === ")" || ch === "]" || ch === "}") { depth--; buf += ch; }
    else if (ch === "/" && depth === 0) { alts.push(buf); buf = ""; }
    else buf += ch;
  }
  if (buf) alts.push(buf);
  return alts.map(a => a.trim()).filter(a => a.length);
}
function productionAlternatives(name: string): string[] | null {
  const out: string[] = [];
  let inBlock = false;
  for (const raw of splitlines(abnfText)) {
    const m = raw.match(/^([A-Za-z][A-Za-z0-9_-]*)\s*=\s*(.*)$/);
    if (m) {
      if (m[1] === name) { inBlock = true; out.push(stripComment(m[2])); }
      else if (inBlock) break;
      continue;
    }
    if (inBlock) {
      const s = raw.trim();
      if (s === "") break;
      out.push(stripComment(s));
    }
  }
  if (!inBlock) return null;
  return splitTopAlts(out.filter(x => x).join(" "));
}
const normalizeAlt = (s: string): string => stripComment(s).replace(/\bS\b/g, "").replace(/\s/g, "");

// type2 is the HARD gate; the rest are soft/best-effort (see header + ROADMAP § 4).
const HARD_PRODUCTION = "type2";
const SOFT_PRODUCTIONS = ["value", "rangeop", "occur", "memberkey", "group", "grpchoice", "grpent",
  "type", "type1", "assignt", "assigng", "rule", "genericparm", "genericarg", "head-number"];
const TYPE2_MIN_ALTERNATIVES = 12; // the pinned cddl-1-1-update.abnf type2 block (verify.ts's floor)

interface AltCoverage { production: string; alternatives: string[]; covered: string[]; uncovered: string[]; rows: number }
function altCoverage(prod: string): AltCoverage {
  const alts = productionAlternatives(prod) ?? [];
  const feat_norms = new Set(features.filter(f => f.production === prod && f.alt).map(f => normalizeAlt(f.alt!)));
  const rows = features.filter(f => f.production === prod).length;
  const covered: string[] = [], uncovered: string[] = [];
  for (const a of alts) (feat_norms.has(normalizeAlt(a)) ? covered : uncovered).push(a);
  return { production: prod, alternatives: alts, covered, uncovered, rows };
}
const type2Cov = altCoverage(HARD_PRODUCTION);
const softCov = SOFT_PRODUCTIONS.map(altCoverage);

// --- BACKWARD: every feature resolves to a first-party source -------------------------------------
type Source = "grammar" | "prelude" | "control-op" | "vendor";
interface Resolution { id: string; production: string | null; source: Source | null }
function resolveSource(f: FeatureRow): Source | null {
  const prod = f.production ?? null;
  if (prod === PRELUDE_PSEUDO) return prelude_name_set.has(f.id.slice("prelude.".length)) ? "prelude" : null;
  if (prod && VENDOR_PRODUCTIONS.has(prod)) return "vendor";
  if (prod && abnf_productions.has(prod)) return "grammar";
  if (prod && controlop_prod_names.has(prod)) return "control-op";
  return null;
}
const resolutions: Resolution[] = features.map(f => ({ id: f.id, production: f.production ?? null, source: resolveSource(f) }));
const unresolved = resolutions.filter(r => r.source === null).sort((a, b) => a.id.localeCompare(b.id));
const bySource: Record<Source, number> = { grammar: 0, prelude: 0, "control-op": 0, vendor: 0 };
for (const r of resolutions) if (r.source) bySource[r.source]++;

// --- PRELUDE completeness (forward, hard) — every prelude name has a prelude.<name> row ------------
const prelude_feature_ids = new Set(features.filter(f => f.production === PRELUDE_PSEUDO).map(f => f.id));
const preludeGaps = prelude_names.filter(n => !prelude_feature_ids.has(`prelude.${n}`)).sort();

// --- gate ------------------------------------------------------------------------------------------
function problems(): string[] {
  const ps: string[] = [];
  // Harness floor: a re-pinned ABNF with a mid-block blank line silently truncates the alternatives
  // list (the vacuous-pass direction). Pin the count exactly as verify.ts does.
  if (type2Cov.alternatives.length < TYPE2_MIN_ALTERNATIVES)
    ps.push(`type2 extraction yielded ${type2Cov.alternatives.length} alternatives (expected >= ${TYPE2_MIN_ALTERNATIVES}) — the ABNF block extraction truncated`);
  // HARD forward: no uncovered type2 alternative.
  for (const a of type2Cov.uncovered)
    ps.push(`type2 alternative not modelled by any feature: ${JSON.stringify(a)}`);
  // HARD backward: every feature resolves to a source.
  for (const r of unresolved)
    ps.push(`feature \`${r.id}\` (production ${JSON.stringify(r.production)}) resolves to NO first-party source (grammar/prelude/control-op/vendor)`);
  // HARD prelude completeness.
  for (const n of preludeGaps)
    ps.push(`prelude name \`${n}\` has no \`prelude.${n}\` feature row`);
  return ps;
}
function vacuityProblems(): string[] {
  const ps: string[] = [];
  if (type2Cov.alternatives.length !== TYPE2_MIN_ALTERNATIVES)
    ps.push(`expected exactly ${TYPE2_MIN_ALTERNATIVES} type2 alternatives, saw ${type2Cov.alternatives.length} — the pinned grammar shape changed (review before re-pinning the floor)`);
  if (control_ops.length !== 37)
    ps.push(`expected 37 IANA control operators, saw ${control_ops.length} — the registry axis read looks broken/incomplete`);
  if (control_ops.some(c => !c.id))
    ps.push(`a control-op row has no id — the registry axis is malformed`);
  if (prelude_names.length <= 20)
    ps.push(`only ${prelude_names.length} prelude names read (expected > 20) — the prelude source read looks broken/empty`);
  if (features.length < 80)
    ps.push(`only ${features.length} features read (expected >= 80) — the matrix read looks broken/empty`);
  return ps;
}

// --- entry -----------------------------------------------------------------------------------------
const argv = process.argv.slice(2);
const isCheck = argv.includes("--check");
const positional = argv.filter(a => !a.startsWith("--"));

if (isCheck) {
  const all = [...problems(), ...vacuityProblems()];
  if (all.length) {
    console.log(`Q5 completeness gate: ${all.length} problem(s)`);
    for (const p of all) console.log(`  FAIL ${p}`);
    process.exit(1);
  }
  console.log(
    `Q5 completeness gate OK — forward: type2 ${type2Cov.covered.length}/${type2Cov.alternatives.length} alternatives covered (HARD, 0 uncovered) · ` +
      `backward: ${resolutions.length} features all resolve (${bySource.grammar} grammar, ${bySource.prelude} prelude, ${bySource.vendor} vendor, ${bySource["control-op"]} control-op) · ` +
      `prelude ${prelude_feature_ids.size}/${prelude_names.length} complete · ${control_ops.length} control ops (complete by construction)`,
  );
  process.exit(0);
}

// --- default (and filtered) run: the readable completeness report ---------------------------------
const filter = positional[0]?.toLowerCase();
const match = (s: string): boolean => !filter || s.toLowerCase().includes(filter);

console.log(`\nQ5 — matrix self-completeness (constructs the profile defines that the matrix does NOT model)`);
console.log(`Sources: grammar (sources/cddl-1-1-update.abnf) ∪ prelude (sources/cddl.prelude) ∪ IANA control-op registry.\n`);

// FORWARD — hard type2 gate.
if (match("type2")) {
  console.log(`### FORWARD (source → feature) — HARD gate: type2 alternatives`);
  console.log(`type2 is the closed, hard-gated set: every ABNF alternative must have >= 1 covering feature.`);
  console.log(`  covered:   ${type2Cov.covered.length}/${type2Cov.alternatives.length}`);
  if (type2Cov.uncovered.length) {
    console.log(`  UNCOVERED (real modelling gaps — --check fails):`);
    for (const a of type2Cov.uncovered) console.log(`    - ${a}`);
  } else {
    console.log(`  uncovered: 0  ✓ (type2 fully modelled)`);
  }
  console.log("");
}

// FORWARD — soft best-effort columns.
const shownSoft = softCov.filter(c => match(c.production));
if (shownSoft.length) {
  console.log(`### FORWARD (source → feature) — SOFT / best-effort: other productions`);
  console.log(`These are DIAGNOSTIC, not gated. \`normalizeAlt\` does exact-string matching, so it reports`);
  console.log(`rule / type1 / genericarg / genericparm / assignt / assigng as false-uncovered though feature`);
  console.log(`rows exist, and head-number renders NOT MODELED because its semantics live under the type2.*`);
  console.log(`rows. Fixing this (tightening normalizeAlt + adding the ROADMAP § 4 variation rows) is a`);
  console.log(`SEPARATE item — Q5 is authoritative ONLY for type2 (above).`);
  const w = Math.max(...shownSoft.map(c => c.production.length), 10);
  for (const c of shownSoft) {
    const tag = c.rows === 0 ? "NOT MODELED (0 rows)" : c.uncovered.length ? `${c.uncovered.length} soft-uncovered (likely normalizeAlt noise)` : "ok";
    console.log(`  ${c.production.padEnd(w)}  ${String(c.covered.length + "/" + c.alternatives.length).padEnd(6)} covered  ${c.rows} row(s)  ${tag}`);
  }
  console.log("");
}

// BACKWARD.
if (!filter || match("backward") || unresolved.some(r => match(r.id))) {
  console.log(`### BACKWARD (feature → source) — HARD: every feature resolves to a first-party source`);
  console.log(`  resolved: ${resolutions.length - unresolved.length}/${resolutions.length}  ` +
    `(grammar ${bySource.grammar}, prelude ${bySource.prelude}, vendor ${bySource.vendor}, control-op ${bySource["control-op"]})`);
  if (unresolved.length) {
    console.log(`  UNRESOLVED (invented features — --check fails):`);
    for (const r of unresolved) if (match(r.id)) console.log(`    - ${r.id} (production ${JSON.stringify(r.production)})`);
  } else {
    console.log(`  unresolved: 0  ✓ (no invented features)`);
  }
  console.log("");
}

// PRELUDE + CONTROL-OP completeness.
if (!filter || match("prelude") || match("control") || match("ctl")) {
  console.log(`### PRELUDE + CONTROL-OP completeness — HARD / by-construction`);
  console.log(`  prelude: ${prelude_feature_ids.size}/${prelude_names.length} names have a prelude.<name> row` +
    (preludeGaps.length ? `  — MISSING: ${preludeGaps.join(", ")}` : `  ✓`));
  console.log(`  control ops: ${control_ops.length} rows — complete BY CONSTRUCTION (the axis IS the IANA`);
  console.log(`               registry, imported by build_matrix.ts; nothing to reconcile, only assert shape).`);
  console.log("");
}

console.log(`Summary: Q5 is AUTHORITATIVE for type2 (${type2Cov.uncovered.length} gap(s)) and for the backward /`);
console.log(`prelude / control-op directions (${unresolved.length + preludeGaps.length} gap(s)); the other productions' forward`);
console.log(`coverage is best-effort until normalizeAlt is tightened (ROADMAP § 4).`);
