#!/usr/bin/env bun
/**
 * Mechanical verification gate for the CDDL master matrix.
 *
 * RECONCILES the authored overlay against the pinned native sources (completeness spine), PROBES every
 * feature's `example` through the three oracles (ruby cddl / rust cddl / cddl-codegen), WRITES
 * annotations/cddl_codegen.toml from those probe results, emits verify_report.json, and exits nonzero
 * on a HARD FAILURE. Authority model: the ruby `cddl` reference decides example validity; the rust
 * `cddl` crate only corroborates (ruby-accepts-but-rust-rejects is a recorded parser limitation).
 *
 * Run from cddl-matrix/:  bun run build_matrix.ts && bun run verify.ts
 */
import { existsSync, mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { constants, homedir, tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { ROOT, loadMatrixInputs, stableJson } from "./lib";

process.chdir(ROOT);

// --- oracle locations (env-overridable; defaults assume the sibling-repo layout) ------------------
const CODEGEN_DIR = resolve(ROOT, ".."); // the cddl-codegen repo this script lives in
const RUST_CDDL = process.env.RUST_CDDL ?? resolve(homedir(), "Documents/git/cddl/target/debug/cddl");
const PRELUDE_PSEUDO = "prelude";
const PROBE_TIMEOUT = 120; // seconds per oracle invocation

// --- F1: language-profile axis --------------------------------------------------------------------
const CDDL_CODEGEN_TARGET_PROFILE = "RFC8610";
const GRAMMAR_PROFILE_RANK: Record<string, number> = { RFC8610: 0, RFC9682: 1 };
const profileNewerThanTarget = (p: string | undefined) =>
  (GRAMMAR_PROFILE_RANK[p || "RFC8610"] ?? 0) > (GRAMMAR_PROFILE_RANK[CDDL_CODEGEN_TARGET_PROFILE] ?? 0);

// Genuine reference-vs-ABNF conflicts kept as `uncertain` rather than hard-failing. Empty today.
const CONFLICT_ALLOWLIST: Record<string, string> = {};

interface Derived {
  valid_a: boolean; valid_b: boolean; spec_valid: boolean; parser_limitation: boolean;
  support: string; support_detail: string; out_of_profile: boolean; status: string;
}
interface ProbeResult extends Derived {
  id: string; production: string | null; profile: string; example: string;
  ruby: number; rust: number; codegen: number;
}
interface ContainmentCorr {
  id: string; spec_declared: string | null; spec_observed: string; ruby: number; rust: number;
  parser_limitation: boolean; contradiction: boolean; example: string;
}
interface AltCoverage {
  abnf_alternatives: string[]; feature_rows: string[]; covered: string[]; uncovered: string[]; modeled: boolean;
}

function resolveRubyCddl(): string | null {
  if (process.env.RUBY_CDDL && existsSync(process.env.RUBY_CDDL)) return process.env.RUBY_CDDL;
  try {
    const r = Bun.spawnSync(["ruby", "-e", "puts Gem.user_dir"], { stdout: "pipe", stderr: "ignore" });
    const cand = join((r.stdout?.toString() ?? "").trim(), "bin", "cddl"); // where the `cddl` gem installs
    if (existsSync(cand)) return cand;
  } catch { /* ruby not installed */ }
  return null;
}
const RUBY_CDDL = resolveRubyCddl();

const splitlines = (t: string): string[] => {
  const a = t.split(/\r\n|\r|\n/);
  if (a.length && a[a.length - 1] === "") a.pop();
  return a;
};

// ==================================================================================================
// 1. LOAD the merged matrix exactly as build_matrix.ts does.
// ==================================================================================================
const { features, roles, contain, encodings, controlOps: control_ops } = loadMatrixInputs();
const feature_ids = new Set(features.map(f => f.id));
const role_ids = new Set(roles.map(r => r.id));
const enc_ids = new Set(encodings.map(e => e.id));

// ==================================================================================================
// 2. RECONCILE against sources/ — BIDIRECTIONAL grammar lint (F2).
// ==================================================================================================
const abnfText = readFileSync(`${ROOT}/sources/cddl-1-1-update.abnf`, "utf8");

// 2a. ABNF production names.
const abnf_productions = new Set<string>();
for (const line of splitlines(abnfText)) {
  const m = line.match(/^([A-Za-z][A-Za-z0-9_-]*)\s*=/);
  if (m) abnf_productions.add(m[1]);
}

// BACKWARD lint: a feature's `production` must resolve to a first-party source.
const controlop_prod_names = new Set<string>([
  ...control_ops.map(co => co.name),
  ...control_ops.map(co => co.name.replace(/^\.+/, "")),
]);
const fabricated: { id: string; production: string | null }[] = [];
for (const f of features) {
  const prod = f.production;
  if (prod === PRELUDE_PSEUDO) continue;
  if (prod && abnf_productions.has(prod)) continue;
  if (prod && controlop_prod_names.has(prod)) continue;
  fabricated.push({ id: f.id, production: prod ?? null });
}

// 2b. Prelude type names.
const preludeText = readFileSync(`${ROOT}/sources/cddl.prelude`, "utf8");
const prelude_names: string[] = [];
for (const line of splitlines(preludeText)) {
  const m = line.match(/^([A-Za-z][A-Za-z0-9_.-]*)\s*=/);
  if (m) prelude_names.push(m[1]);
}
const prelude_name_set = new Set(prelude_names);
const prelude_feature_ids = new Set(features.filter(f => f.production === PRELUDE_PSEUDO).map(f => f.id));

const gaps: Record<string, string>[] = [];
for (const name of prelude_names)
  if (!prelude_feature_ids.has(`prelude.${name}`))
    gaps.push({ kind: "missing_prelude_feature", name, expected_id: `prelude.${name}` });
for (const fid of [...prelude_feature_ids].sort()) {
  const nm = fid.slice("prelude.".length);
  if (!prelude_name_set.has(nm)) gaps.push({ kind: "orphan_prelude_feature", id: fid, name: nm });
}

// 2c. LINK INTEGRITY.
const link_errors: { kind: string; id: string | null; ref: string | null }[] = [];
for (const f of features) {
  for (const eid of f.encodings ?? []) if (!enc_ids.has(eid)) link_errors.push({ kind: "encoding", id: f.id ?? null, ref: eid });
  for (const rid of f.roles ?? []) if (!role_ids.has(rid)) link_errors.push({ kind: "role", id: f.id ?? null, ref: rid });
}
for (const c of contain) {
  if (!role_ids.has(c.role)) link_errors.push({ kind: "containment_role", id: c.id ?? null, ref: c.role ?? null });
  if (!feature_ids.has(c.feature)) link_errors.push({ kind: "containment_feature", id: c.id ?? null, ref: c.feature ?? null });
}

// 2d. PER-ALTERNATIVE completeness for the grammar axis.
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

const ALT_PRODUCTIONS = ["type2", "value", "rangeop", "occur", "memberkey", "group", "grpchoice",
  "grpent", "type", "type1", "assignt", "assigng", "rule", "genericparm", "genericarg", "head-number"];

const alt_coverage: Record<string, AltCoverage> = {};
for (const prod of ALT_PRODUCTIONS) {
  const alts = productionAlternatives(prod);
  const feat_norms = new Set(features.filter(f => f.production === prod && f.alt).map(f => normalizeAlt(f.alt!)));
  const rows = features.filter(f => f.production === prod).map(f => f.id);
  const covered: string[] = [], uncovered: string[] = [];
  for (const a of alts ?? []) (feat_norms.has(normalizeAlt(a)) ? covered : uncovered).push(a);
  alt_coverage[prod] = { abnf_alternatives: alts ?? [], feature_rows: [...rows].sort(), covered, uncovered, modeled: rows.length > 0 };
}
const type2_uncovered: string[] = alt_coverage["type2"].uncovered;

// ==================================================================================================
// 3. PROBE each feature's example through the three oracles.
// ==================================================================================================
const probeDir = mkdtempSync(join(tmpdir(), "cddl_verify_"));
const probeFile = join(probeDir, "probe.cddl");
const ccOut = join(probeDir, "cc_out");

function runExit(cmd: string[], cwd?: string): number {
  const r = Bun.spawnSync(cmd, { cwd, stdout: "ignore", stderr: "ignore", timeout: PROBE_TIMEOUT * 1000 });
  if (r.exitedDueToTimeout) return -1;              // timeout -> -1
  if (r.exitCode !== null) return r.exitCode;       // normal exit (incl. 101 panic)
  const sig = r.signalCode ? (constants.signals as Record<string, number>)[r.signalCode] : undefined;
  return sig != null ? -sig : -1;                   // signal kill -> -signum (returncode convention)
}

function oracles(example: string): [number, number, number] {
  writeFileSync(probeFile, example + "\n");
  const a = RUBY_CDDL ? runExit([RUBY_CDDL, probeFile, "generate", "1"]) : -2;
  const b = runExit([RUST_CDDL, "compile-cddl", "--cddl", probeFile]);
  const c = runExit(["cargo", "run", "-q", "--", `--input=${probeFile}`, `--output=${ccOut}`, "--wasm=false"], CODEGEN_DIR);
  return [a, b, c];
}

function derive(featureId: string, profile: string, rubyExit: number, rustExit: number, codegenExit: number): Derived {
  const valid_a = rubyExit === 0;
  const valid_b = rustExit === 0;
  const spec_valid = valid_a;
  const parser_limitation = valid_a && !valid_b;
  let support: string, support_detail: string;
  if (codegenExit === 0) { support = "supported"; support_detail = "exit 0"; }
  else if (codegenExit === 101) { support = "unsupported"; support_detail = "panic (exit 101)"; }
  else { support = "unsupported"; support_detail = `rejected at parse/lex (exit ${codegenExit})`; }
  const out_of_profile = spec_valid && support !== "supported" && profileNewerThanTarget(profile);
  let status: string;
  if (!spec_valid) status = Object.hasOwn(CONFLICT_ALLOWLIST, featureId) ? "uncertain" : "spec_invalid";
  else if (out_of_profile) status = "out_of_profile";
  else status = support;
  return { valid_a, valid_b, spec_valid, parser_limitation, support, support_detail, out_of_profile, status };
}

const probe_results: ProbeResult[] = [];
for (const f of [...features].sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))) {
  const [a, b, c] = oracles(f.example);
  const profile = f.profile ?? "RFC8610";
  const d = derive(f.id, profile, a, b, c);
  probe_results.push({ id: f.id, production: f.production ?? null, profile, example: f.example, ruby: a, rust: b, codegen: c, ...d });
}

// Containment corroboration (spec oracles only), reference-authority.
const containment_corroboration: ContainmentCorr[] = [];
for (const c of contain.filter(x => x.example).sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))) {
  writeFileSync(probeFile, c.example + "\n");
  const a = RUBY_CDDL ? runExit([RUBY_CDDL, probeFile, "generate", "1"]) : -2;
  const b = runExit([RUST_CDDL, "compile-cddl", "--cddl", probeFile]);
  const observed = a === 0 ? "allowed" : "disallowed";
  const parser_limitation = (a === 0) !== (b === 0);
  const contradiction = observed !== c.spec;
  containment_corroboration.push({
    id: c.id, spec_declared: c.spec ?? null, spec_observed: observed,
    ruby: a, rust: b, parser_limitation, contradiction, example: c.example!,
  });
}

// ==================================================================================================
// 4. WRITE annotations/cddl_codegen.toml from the probe results (execution-grounded).
// ==================================================================================================
const ok = (exit: number) => (exit === 0 ? "ok" : "fail");
const tomlStr = (s: string) => JSON.stringify(s); // JSON string escaping is a valid TOML basic string

const annoLines: string[] = [
  "# cddl-codegen support, keyed by master feature id. EXECUTION-GROUNDED: generated by verify.ts",
  "# from live oracle probes (NOT hand-read from the generator source). Do not edit by hand — re-run",
  "#   bun run build_matrix.ts && bun run verify.ts",
  "# to regenerate. Each row is the result of running the feature's minimal `example` through:",
  "#   ruby  cddl ... generate 1            (spec-validity A, authoritative / reference)",
  "#   rust  cddl compile-cddl              (spec-validity B, corroborating only)",
  "#   cddl-codegen --input=... --wasm=false (support: exit 0=supported, 101=panic/unsupported, else error)",
  "# status: supported | unsupported | out_of_profile | uncertain.",
  "#   out_of_profile = the feature's grammar profile is NEWER than cddl-codegen's TARGET profile AND",
  "#         cddl-codegen rejects it (it is outside what the tool targets, NOT a gap within it).",
  "#   uncertain = spec-valid but a genuine reference-vs-ABNF conflict. A `rust parser limitation`",
  "#         note means the reference (ruby/ABNF) accepts the example but the rust cddl crate rejects",
  "#         it (e.g. lowercase `h'cafe'`); that is corroboration noise, not a support/validity verdict.",
  "#",
  "# TARGET PROFILE: cddl-codegen tracks ~RFC 8610 (the RFC 8610 grammar). It does NOT implement the",
  "#   RFC 9682 grammar additions (the `#7` split; the type-valued tag head-number,",
  '#   `head-number = uint / ("<" type ">")`). Features tagged `profile = "RFC9682"` that cddl-codegen',
  "#   rejects are therefore `out_of_profile`, not `unsupported`. (Control-op extension RFCs",
  "#   9090/9165/9741 are a separate registry axis whose support is probed per operator.)",
  "#",
  "# CONSUMER NOTES (cddl-codegen-specific facts kept OUT of the pure-spec master, recorded here):",
  "#   * `T / null` type choice -> cddl-codegen emits Option<T> (a consumer special-case of the",
  '#     ordinary `type = type1 *("/" type1)` production, NOT a distinct ABNF alternative).',
  "#   * prelude `float` (float16-32 / float64) -> cddl-codegen maps to Rust f64.",
  "",
];
for (const pr of probe_results) {
  let ev = `probe: cddl-codegen ${pr.support_detail ?? "exit " + pr.codegen}; ruby=${ok(pr.ruby)} rust=${ok(pr.rust)}`;
  if (pr.parser_limitation) ev += " (rust parser limitation: reference/ABNF accept)";
  if (pr.status === "out_of_profile")
    ev += ` (out of profile: feature profile ${pr.profile} is newer than cddl-codegen target ${CDDL_CODEGEN_TARGET_PROFILE})`;
  annoLines.push("[[support]]");
  annoLines.push(`id = ${tomlStr(pr.id)}`);
  annoLines.push(`status = ${tomlStr(pr.status)}`);
  annoLines.push(`evidence = ${tomlStr(ev)}`);
  annoLines.push("");
}
writeFileSync(`${ROOT}/annotations/cddl_codegen.toml`, annoLines.join("\n").replace(/\s+$/, "") + "\n");

// ==================================================================================================
// 5. EMIT verify_report.json, print summary, exit nonzero on hard failures.
// ==================================================================================================
const spec_invalid = probe_results.filter(pr => pr.status === "spec_invalid");
const parser_limitations = probe_results.filter(pr => pr.parser_limitation).map(pr => pr.id);
const containment_parser_limitations = containment_corroboration.filter(c => c.parser_limitation).map(c => c.id);
const containment_contradictions = containment_corroboration.filter(c => c.contradiction);
const uncertain = [...new Set(probe_results.filter(pr => pr.status === "uncertain").map(pr => pr.id))].sort();
const out_of_profile = [...new Set(probe_results.filter(pr => pr.status === "out_of_profile").map(pr => pr.id))].sort();

const report = {
  gaps,
  fabricated,
  link_errors,
  type2_uncovered_alternatives: type2_uncovered,
  alternative_coverage: alt_coverage,
  spec_invalid: spec_invalid.map(pr => pr.id),
  out_of_profile,
  parser_limitations: [...parser_limitations].sort(),
  probe_results,
  containment_corroboration,
  containment_contradictions: containment_contradictions.map(c => c.id),
  containment_parser_limitations: [...containment_parser_limitations].sort(),
  target_profile: CDDL_CODEGEN_TARGET_PROFILE,
  summary: {
    features: features.length,
    roles: roles.length,
    containment: contain.length,
    encodings: encodings.length,
    control_ops: control_ops.length,
    abnf_productions: abnf_productions.size,
    prelude_names: prelude_names.length,
    supported: probe_results.filter(pr => pr.status === "supported").length,
    unsupported: probe_results.filter(pr => pr.status === "unsupported").length,
    out_of_profile: out_of_profile.length,
    uncertain: uncertain.length,
    fabricated: fabricated.length,
    gaps: gaps.length,
    link_errors: link_errors.length,
    type2_alternatives: alt_coverage["type2"].abnf_alternatives.length,
    type2_covered: alt_coverage["type2"].covered.length,
    type2_uncovered: type2_uncovered.length,
    spec_invalid: spec_invalid.length,
    parser_limitations: parser_limitations.length,
    containment_contradictions: containment_contradictions.length,
    containment_parser_limitations: containment_parser_limitations.length,
  },
};
writeFileSync(`${ROOT}/verify_report.json`, stableJson(report));

const s = report.summary;
const eq = "=".repeat(80);
console.log(eq);
console.log("CDDL matrix verify gate (ABNF-authority)");
console.log(eq);
console.log(`features probed     : ${s.features}`);
console.log(`target profile      : ${CDDL_CODEGEN_TARGET_PROFILE} (out-of-profile features excluded from gaps)`);
console.log(`ABNF productions    : ${s.abnf_productions}  prelude names: ${s.prelude_names}`);
console.log(`support (codegen)   : supported=${s.supported} unsupported=${s.unsupported} out_of_profile=${s.out_of_profile} uncertain=${s.uncertain}`);
console.log(`reconcile (BIDIRECTIONAL grammar lint):`);
console.log(`  forward  (source->feature): type2 alternatives covered ${s.type2_covered}/${s.type2_alternatives} (uncovered=${s.type2_uncovered})`);
console.log(`  backward (feature->source): fabricated=${s.fabricated} (feature.production resolving to no ABNF/prelude/control-op source)`);
console.log(`  prelude gaps=${s.gaps}  link_errors=${s.link_errors}`);
console.log(`type2 per-alt       : ${s.type2_covered}/${s.type2_alternatives} covered (uncovered=${s.type2_uncovered})`);
console.log(`spec-invalid (ref-rejected examples): ${s.spec_invalid}`);
console.log(`parser limitations (rust): features=${s.parser_limitations} containment=${s.containment_parser_limitations}`);
console.log(`containment         : contradictions=${s.containment_contradictions}`);

console.log("\nALTERNATIVE COVERAGE (type2 gates; others best-effort/logged):");
for (const prod of ALT_PRODUCTIONS) {
  const cov = alt_coverage[prod];
  const nAlt = cov.abnf_alternatives.length, nCov = cov.covered.length;
  const tag = prod === "type2" ? "HARD" : "soft";
  if (!cov.modeled) console.log(`  - ${prod.padEnd(12)} [${tag}] NOT MODELED (0 feature rows) — ${nAlt} ABNF alternative(s)`);
  else console.log(`  - ${prod.padEnd(12)} [${tag}] ${nCov}/${nAlt} alternatives covered${cov.uncovered.length ? "  uncovered: " + cov.uncovered.join("; ") : ""}`);
}

if (fabricated.length) {
  console.log("\nFABRICATED productions (backward lint: not in ABNF, not `prelude`, not control-op registry):");
  for (const x of fabricated) console.log(`  - ${x.id}: production '${x.production}'`);
}
if (gaps.length) { console.log("\nCOMPLETENESS GAPS (prelude):"); for (const g of gaps) console.log(`  - ${JSON.stringify(g)}`); }
if (link_errors.length) { console.log("\nLINK-INTEGRITY ERRORS:"); for (const e of link_errors) console.log(`  - ${e.kind}: ${e.id} -> unknown '${e.ref}'`); }
if (type2_uncovered.length) { console.log("\nTYPE2 PER-ALTERNATIVE GAPS (no covering feature row):"); for (const a of type2_uncovered) console.log(`  - ${a}`); }
if (spec_invalid.length) {
  console.log("\nSPEC-INVALID EXAMPLES (REFERENCE parser rejects an authored example):");
  for (const pr of spec_invalid) console.log(`  - ${pr.id}: ruby=${ok(pr.ruby)} rust=${ok(pr.rust)}  ex=${JSON.stringify(pr.example)}`);
}
if (containment_contradictions.length) {
  console.log("\nCONTAINMENT CONTRADICTIONS (reference-observed spec != declared spec):");
  for (const c of containment_contradictions) console.log(`  - ${c.id}: declared=${c.spec_declared} observed=${c.spec_observed}`);
}
if (parser_limitations.length || containment_parser_limitations.length) {
  console.log("\nPARSER LIMITATIONS (reference/ABNF accept, rust rejects — informational, non-fatal):");
  for (const u of [...parser_limitations].sort()) console.log(`  - ${u}`);
  for (const u of [...containment_parser_limitations].sort()) console.log(`  - ${u} (containment)`);
}

console.log("\nOUT_OF_PROFILE (" + out_of_profile.length + `; profile newer than ${CDDL_CODEGEN_TARGET_PROFILE} and cddl-codegen rejects — excluded from gaps, NOT unsupported):`);
for (const u of out_of_profile) {
  const pr = probe_results.find(p => p.id === u)!;
  console.log(`  - ${u} (profile ${pr.profile}; ${pr.support_detail})`);
}

console.log("\nUNCERTAIN (" + uncertain.length + "):");
for (const u of uncertain) console.log(`  - ${u}`);

console.log("\nwrote annotations/cddl_codegen.toml and verify_report.json");

const hard_fail = fabricated.length || gaps.length || link_errors.length || type2_uncovered.length ||
  spec_invalid.length || containment_contradictions.length;
if (hard_fail) { console.log("\nRESULT: FAIL (hard failure — see above)"); process.exit(1); }
console.log("\nRESULT: PASS");
process.exit(0);
