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

// Features whose generated code REFERENCES user-supplied items (an extern type, a raw-bytes impl, a
// custom ser/deser fn), so the crate cannot compile STANDALONE — by design. The compile-gate would
// false-negative them; they ARE supported (integration-tested where the user code is provided). Exempt =
// compile result ignored, support from generation exit only. Reason cites the integration test that DOES
// cover them, so the exemption isn't blind.
const COMPILE_GATE_EXEMPT: Record<string, string> = {
  "ext.extern": "requires a user-provided extern type; integration-tested in tests/extern-deps",
  "ext.raw_bytes": "requires a user-provided raw-bytes impl; integration-tested in tests/raw-bytes",
  "dsl.custom_serialize": "references a user-provided serialize fn; integration-tested in tests/custom_serialization",
  "dsl.custom_deserialize": "references a user-provided deserialize fn; integration-tested in tests/custom_serialization",
};

interface Derived {
  valid_a: boolean; valid_b: boolean; spec_valid: boolean; parser_limitation: boolean;
  support: string; support_detail: string; out_of_profile: boolean; status: string;
}
interface ProbeResult extends Derived {
  id: string; production: string | null; profile: string; example: string;
  ruby: number; rust: number; codegen: number; compile: number;
}
interface ContainmentCorr {
  id: string; spec_declared: string | null; spec_observed: string; ruby: number; rust: number;
  parser_limitation: boolean; contradiction: boolean; example: string;
  codegen: number; compile: number; support: string | null;  // per-cell cddl-codegen support (the role × feature axis)
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
// CDDL_CODEGEN (vendor) profile: features resolve to the in-repo DSL source, not the ABNF/prelude/registry.
const CDDL_CODEGEN_PSEUDO = new Set(["comment_dsl", "sentinel"]);
const dslSource = readFileSync(`${CODEGEN_DIR}/src/comment_ast.rs`, "utf8") + "\n" +
  readFileSync(`${CODEGEN_DIR}/src/parsing.rs`, "utf8");
const fabricated: { id: string; production: string | null }[] = [];
for (const f of features) {
  const prod = f.production;
  if (prod === PRELUDE_PSEUDO) continue;
  if (prod && CDDL_CODEGEN_PSEUDO.has(prod)) {
    if (f.alt && dslSource.includes(f.alt)) continue;   // resolves to the pinned vendor source
    fabricated.push({ id: f.id, production: prod });     // alt token absent from source -> fabricated
    continue;
  }
  if (prod && abnf_productions.has(prod)) continue;
  if (prod && controlop_prod_names.has(prod)) continue;
  fabricated.push({ id: f.id, production: prod ?? null });
}

// FORWARD lint (CDDL_CODEGEN): every USER-FACING @directive (comment_ast.rs tag("@…")) and *_MARKER
// (parsing.rs) must be modelled by a feature — completeness in the vendor source's direction (mirrors the
// prelude lint). "User-facing" = documented in comment_dsl.mdx: that gate excludes INTERNAL markers
// cddl-codegen injects itself (e.g. _CDDL_CODEGEN_SCOPE_MARKER_, used for module scoping, never written by
// a user and absent from the docs), while still catching a new documented extension the matrix forgot.
const docsText = readFileSync(`${CODEGEN_DIR}/docs/docs/comment_dsl.mdx`, "utf8");
const dslDirectives = [...dslSource.matchAll(/tag\("(@[a-z_]+)"\)/g)].map(m => m[1]);
const dslMarkers = [...dslSource.matchAll(/MARKER[^"]*"(_CDDL_CODEGEN_[^"]+)"/g)].map(m => m[1]);
const cddlCodegenAlts = new Set(features.filter(f => f.profile === "CDDL_CODEGEN").map(f => f.alt));
const cddl_codegen_gaps: { kind: string; name: string }[] = [];
for (const d of [...new Set([...dslDirectives, ...dslMarkers])].sort())
  if (docsText.includes(d) && !cddlCodegenAlts.has(d)) cddl_codegen_gaps.push({ kind: "missing_cddl_codegen_feature", name: d });

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
// Shared cargo target for the compile-gate so the generated crate's deps (cbor_event, …) build ONCE and
// every subsequent `cargo check` is incremental (fits PROBE_TIMEOUT). Warmed before the probe loops.
const COMPILE_TARGET = mkdtempSync(join(tmpdir(), "cddl_verify_target_"));
const COMPILE_WARM_TIMEOUT = 600; // first build (all deps) can exceed the per-probe timeout

function runExit(cmd: string[], cwd?: string, env?: Record<string, string>, timeoutS = PROBE_TIMEOUT): number {
  const r = Bun.spawnSync(cmd, {
    cwd, env: env ? { ...process.env, ...env } : undefined,
    stdout: "ignore", stderr: "ignore", timeout: timeoutS * 1000,
  });
  if (r.exitedDueToTimeout) return -1;              // timeout -> -1
  if (r.exitCode !== null) return r.exitCode;       // normal exit (incl. 101 panic)
  const sig = r.signalCode ? (constants.signals as Record<string, number>)[r.signalCode] : undefined;
  return sig != null ? -sig : -1;                   // signal kill -> -signum (returncode convention)
}

// COMPILE-GATE: `cargo check` the generated crate. The generator exiting 0 is NOT enough — it can emit
// non-compiling Rust (e.g. `x = any` -> `pub type X = Any;`, a type defined nowhere), which the exit-code
// probe over-credits as "supported". Mirrors integration_tests::feature_corpus_compiles (rust-only,
// shared CARGO_TARGET_DIR). Caller invokes only when generation (`cargo run`) succeeded.
function runCompile(timeoutS = PROBE_TIMEOUT): number {
  return runExit(
    ["cargo", "check", "--manifest-path", join(ccOut, "rust", "Cargo.toml")],
    CODEGEN_DIR, { CARGO_TARGET_DIR: COMPILE_TARGET }, timeoutS,
  );
}

// [ruby, rust, codegen-generate, codegen-compile]. compile is -2 when generation didn't succeed (n/a).
function oracles(example: string): [number, number, number, number] {
  writeFileSync(probeFile, example + "\n");
  const a = RUBY_CDDL ? runExit([RUBY_CDDL, probeFile, "generate", "1"]) : -2;
  const b = runExit([RUST_CDDL, "compile-cddl", "--cddl", probeFile]);
  const c = runExit(["cargo", "run", "-q", "--", `--input=${probeFile}`, `--output=${ccOut}`, "--wasm=false"], CODEGEN_DIR);
  const comp = c === 0 ? runCompile() : -2;
  return [a, b, c, comp];
}

function derive(featureId: string, profile: string, rubyExit: number, rustExit: number, codegenExit: number, compileExit: number): Derived {
  const valid_a = rubyExit === 0;
  const valid_b = rustExit === 0;
  // CDDL_CODEGEN is a vendor profile: cddl-codegen IS the spec authority, so the ruby/rust reference
  // oracles don't gate validity (they reject the sentinel typenames — expected). Validity comes from the
  // backward lint (the construct resolves to the in-repo vendor source), so treat it as spec-valid here.
  const isVendor = profile === "CDDL_CODEGEN";
  const spec_valid = isVendor ? true : valid_a;
  const parser_limitation = !isVendor && valid_a && !valid_b;
  // COMPILE-GATED support: "supported" requires generation AND a compiling crate. Generation-only
  // (exit 0 but cargo check fails) is a false positive — recorded distinctly so the gap is visible.
  // EXEMPT features (user-supplied code) skip the compile bit — they can't compile standalone by design.
  const compileExempt = Object.hasOwn(COMPILE_GATE_EXEMPT, featureId);
  let support: string, support_detail: string;
  if (codegenExit === 0 && (compileExempt || compileExit === 0)) {
    support = "supported";
    support_detail = compileExempt ? `exit 0; standalone-compile N/A (${COMPILE_GATE_EXEMPT[featureId]})` : "exit 0; compiles";
  }
  else if (codegenExit === 0) { support = "unsupported"; support_detail = `generates but does not compile (cargo check exit ${compileExit})`; }
  else if (codegenExit === 101) { support = "unsupported"; support_detail = "panic (exit 101)"; }
  else { support = "unsupported"; support_detail = `rejected at parse/lex (exit ${codegenExit})`; }
  const out_of_profile = spec_valid && support !== "supported" && profileNewerThanTarget(profile);
  let status: string;
  if (!spec_valid) status = Object.hasOwn(CONFLICT_ALLOWLIST, featureId) ? "uncertain" : "spec_invalid";
  else if (out_of_profile) status = "out_of_profile";
  else status = support;
  return { valid_a, valid_b, spec_valid, parser_limitation, support, support_detail, out_of_profile, status };
}

// Warm the shared compile target ONCE (deps build here; per-probe `cargo check` is then incremental and
// fits PROBE_TIMEOUT). Without this the first probe's check would eat the whole dep build and risk a
// spurious timeout -> false "does not compile". A trivial valid spec is enough to pull in the deps.
writeFileSync(probeFile, "warm = [uint, tstr]\n");
if (runExit(["cargo", "run", "-q", "--", `--input=${probeFile}`, `--output=${ccOut}`, "--wasm=false"], CODEGEN_DIR) === 0)
  runCompile(COMPILE_WARM_TIMEOUT);

const probe_results: ProbeResult[] = [];
for (const f of [...features].sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))) {
  const [a, b, c, comp] = oracles(f.example);
  const profile = f.profile ?? "RFC8610";
  const d = derive(f.id, profile, a, b, c, comp);
  probe_results.push({ id: f.id, production: f.production ?? null, profile, example: f.example, ruby: a, rust: b, codegen: c, compile: comp, ...d });
}

// Containment corroboration + PER-CELL support. The role × feature axis: a feature's support can DIFFER
// by nesting context (e.g. a literal value is supported as an array member but not as a top-level type).
// ruby/rust corroborate the spec verdict; cddl-codegen is probed per cell for an execution-grounded
// support bit, exactly like the per-feature probe — written to annotations keyed by the containment id.
const containment_corroboration: ContainmentCorr[] = [];
for (const c of contain.filter(x => x.example).sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))) {
  writeFileSync(probeFile, c.example + "\n");
  const a = RUBY_CDDL ? runExit([RUBY_CDDL, probeFile, "generate", "1"]) : -2;
  const b = runExit([RUST_CDDL, "compile-cddl", "--cddl", probeFile]);
  // only probe support where the nesting is spec-valid (ruby accepts); a spec-disallowed cell isn't
  // valid CDDL, so "does cddl-codegen support it" is meaningless.
  const cg = a === 0
    ? runExit(["cargo", "run", "-q", "--", `--input=${probeFile}`, `--output=${ccOut}`, "--wasm=false"], CODEGEN_DIR)
    : -2;
  const cgComp = cg === 0 ? runCompile() : -2;   // compile-gate the cell too (same false-positive class)
  const support = a === 0 ? (cg === 0 && cgComp === 0 ? "supported" : "unsupported") : null;
  const observed = a === 0 ? "allowed" : "disallowed";
  const parser_limitation = (a === 0) !== (b === 0);
  const contradiction = observed !== c.spec;
  containment_corroboration.push({
    id: c.id, spec_declared: c.spec ?? null, spec_observed: observed,
    ruby: a, rust: b, parser_limitation, contradiction, example: c.example!, codegen: cg, compile: cgComp, support,
  });
}

// CONTROL-OP support (item 7). Probe each IANA op's minimal `example` through cddl-codegen (compile-gated)
// -> a [[support]] row keyed by ctl.<name>, the same pattern as features. The op set is IANA-registry-
// authoritative, so ruby/rust are CORROBORATION ONLY (an op a given ruby/rust version lacks is not
// "invalid CDDL"): support is purely the cddl-codegen verdict, supported/unsupported (no out_of_profile —
// the control-op extension RFCs 9090/9165/9741 are a separate axis from the grammar profile).
interface ControlOpSupport { id: string; name: string; support: string; detail: string; ruby: number; rust: number; codegen: number; compile: number; example: string }
const controlop_missing_example = control_ops.filter(co => !co.example).map(co => co.id);
const controlop_support: ControlOpSupport[] = [];
for (const co of [...control_ops].filter(co => co.example).sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))) {
  const [a, b, c, comp] = oracles(co.example!);
  const supported = c === 0 && comp === 0;
  const detail = supported ? "exit 0; compiles"
    : c === 0 ? `generates but does not compile (cargo check exit ${comp})`
    : c === 101 ? "panic (exit 101)"
    : `rejected at parse/lex (exit ${c})`;
  controlop_support.push({ id: co.id, name: co.name, support: supported ? "supported" : "unsupported", detail, ruby: a, rust: b, codegen: c, compile: comp, example: co.example! });
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
  "#   cddl-codegen --input=... --wasm=false (generate the crate) THEN `cargo check` it (the COMPILE-GATE).",
  "#     support = generates AND compiles. exit 0 alone is NOT enough: `x = any` generates `pub type X =",
  "#     Any;` (a type defined nowhere) which fails to compile -> unsupported, not a false 'supported'.",
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
  if (pr.profile === "CDDL_CODEGEN") ev += " (vendor profile: validity by cddl-codegen; ruby/rust informational)";
  if (pr.status === "out_of_profile")
    ev += ` (out of profile: feature profile ${pr.profile} is newer than cddl-codegen target ${CDDL_CODEGEN_TARGET_PROFILE})`;
  annoLines.push("[[support]]");
  annoLines.push(`id = ${tomlStr(pr.id)}`);
  annoLines.push(`status = ${tomlStr(pr.status)}`);
  annoLines.push(`evidence = ${tomlStr(ev)}`);
  annoLines.push("");
}
// PER-CELL support (role × feature), keyed by containment id — same [[support]] table, since a
// containment id is a master id. Lets the matrix say "supported HERE, not THERE" structurally.
annoLines.push("# --- per-cell support (role × feature), keyed by containment id (see containment/*.toml) ---");
annoLines.push("");
for (const c of containment_corroboration.filter(c => c.support !== null)) {
  const compile = c.codegen === 0 ? `; compiles=${c.compile === 0 ? "ok" : "fail"}` : "";
  const ev = `probe (cell): cddl-codegen exit ${c.codegen}${compile}; ruby=${ok(c.ruby)} rust=${ok(c.rust)}`;
  annoLines.push("[[support]]");
  annoLines.push(`id = ${tomlStr(c.id)}`);
  annoLines.push(`status = ${tomlStr(c.support!)}`);
  annoLines.push(`evidence = ${tomlStr(ev)}`);
  annoLines.push("");
}
// PER-CONTROL-OP support (item 7), keyed by ctl.<name>. cddl-codegen is the support authority; ruby/rust
// corroborate (informational) — control ops are IANA-authoritative, so a ruby/rust reject is not invalidity.
annoLines.push("# --- per-control-op support, keyed by ctl.<name> (IANA registry; ruby/rust corroborate only) ---");
annoLines.push("");
for (const co of controlop_support) {
  const ev = `probe (control-op): cddl-codegen ${co.detail}; ruby=${ok(co.ruby)} rust=${ok(co.rust)}`;
  annoLines.push("[[support]]");
  annoLines.push(`id = ${tomlStr(co.id)}`);
  annoLines.push(`status = ${tomlStr(co.support)}`);
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
  cddl_codegen_gaps,
  fabricated,
  link_errors,
  type2_uncovered_alternatives: type2_uncovered,
  alternative_coverage: alt_coverage,
  spec_invalid: spec_invalid.map(pr => pr.id),
  out_of_profile,
  parser_limitations: [...parser_limitations].sort(),
  probe_results,
  controlop_support,
  controlop_missing_example,
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
    controlop_supported: controlop_support.filter(c => c.support === "supported").length,
    controlop_unsupported: controlop_support.filter(c => c.support === "unsupported").length,
    controlop_missing_example: controlop_missing_example.length,
    abnf_productions: abnf_productions.size,
    prelude_names: prelude_names.length,
    supported: probe_results.filter(pr => pr.status === "supported").length,
    unsupported: probe_results.filter(pr => pr.status === "unsupported").length,
    out_of_profile: out_of_profile.length,
    uncertain: uncertain.length,
    fabricated: fabricated.length,
    gaps: gaps.length,
    cddl_codegen_gaps: cddl_codegen_gaps.length,
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
console.log(`control-op support  : supported=${s.controlop_supported} unsupported=${s.controlop_unsupported} (of ${s.control_ops}; missing example=${s.controlop_missing_example})`);
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

if (controlop_missing_example.length) {
  console.log("\nCONTROL-OPS MISSING AN EXAMPLE (add to control_examples.toml):");
  for (const id of controlop_missing_example) console.log(`  - ${id}`);
}

console.log("\nwrote annotations/cddl_codegen.toml and verify_report.json");

const hard_fail = fabricated.length || gaps.length || cddl_codegen_gaps.length || link_errors.length || type2_uncovered.length ||
  spec_invalid.length || containment_contradictions.length || controlop_missing_example.length;
if (hard_fail) { console.log("\nRESULT: FAIL (hard failure — see above)"); process.exit(1); }
console.log("\nRESULT: PASS");
process.exit(0);
