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
 *      the shared block/alt/normalize logic verify.ts uses.
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
 * FORWARD LINT — HARD for every enumerated production:
 *   - every pinned ABNF alternative in the enumerated grammar productions is either covered by a feature
 *     row, explicitly delegated with a reason, or modelled under named feature rows;
 *   - every production has a pinned alternative-count floor so extraction truncation cannot pass
 *     vacuously;
 *   - stale delegation strings and missing modelled-under feature ids hard-fail.
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
 *   bun run query_q5_completeness.ts --check   -> hard-fail on any uncovered alt / unresolved feature
 *                                                 source / prelude gap + vacuity floor; exit nonzero
 */
import { readFileSync } from "node:fs";
import { ALT_PRODUCTIONS, grammarAltCoverage } from "./lib";

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

const altCoverageResult = grammarAltCoverage(features, abnfText);
const altCoverage = altCoverageResult.coverage;

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
  ps.push(...altCoverageResult.problems);
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
  ps.push(...altCoverageResult.vacuityProblems);
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
  const totalCovered = ALT_PRODUCTIONS.reduce((n, prod) => {
    const c = altCoverage[prod];
    return n + c.covered.length + c.delegated.length + c.modeled_under.length;
  }, 0);
  const totalAlternatives = ALT_PRODUCTIONS.reduce((n, prod) => n + altCoverage[prod].abnf_alternatives.length, 0);
  console.log(
    `Q5 completeness gate OK — forward: ${totalCovered}/${totalAlternatives} alternatives covered/delegated/modelled-under across ${ALT_PRODUCTIONS.length} productions (HARD, 0 uncovered) · ` +
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

// FORWARD.
const shownForward = ALT_PRODUCTIONS.map(prod => altCoverage[prod]).filter(c => match(c.production));
if (shownForward.length) {
  console.log(`### FORWARD (source → feature) — HARD gate: ABNF alternatives`);
  console.log(`Every enumerated ABNF alternative must be covered by a feature row, delegated with a reason, or modelled under named feature rows.`);
  const w = Math.max(...shownForward.map(c => c.production.length), 10);
  for (const c of shownForward) {
    const accounted = c.covered.length + c.delegated.length + c.modeled_under.length;
    const rows = c.feature_rows.length;
    const extras = [
      c.delegated.length ? `${c.delegated.length} delegated` : null,
      c.modeled_under.length ? `${c.modeled_under.length} modelled-under` : null,
      c.uncovered.length ? `${c.uncovered.length} UNCOVERED` : null,
    ].filter(Boolean).join(", ");
    console.log(`  ${c.production.padEnd(w)}  ${String(accounted + "/" + c.abnf_alternatives.length).padEnd(6)} accounted  ${rows} row(s)  ${extras || "ok"}`);
    for (const d of c.delegated)
      console.log(`    delegated ${d.alt} — ${d.reason}`);
    for (const m of c.modeled_under)
      console.log(`    ${m.alt} — modelled under ${m.featureIds.join(", ")}`);
    for (const a of c.uncovered)
      console.log(`    UNCOVERED ${a}`);
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

const forwardGaps = ALT_PRODUCTIONS.reduce((n, prod) => n + altCoverage[prod].uncovered.length, 0);
console.log(`Summary: Q5 is AUTHORITATIVE for forward grammar coverage across ${ALT_PRODUCTIONS.length} productions`);
console.log(`(${forwardGaps} gap(s)) and for the backward / prelude / control-op directions (${unresolved.length + preludeGaps.length} gap(s)).`);
