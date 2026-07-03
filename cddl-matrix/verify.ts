#!/usr/bin/env bun
/**
 * Mechanical verification gate for the CDDL master matrix.
 *
 * RECONCILES the authored overlay against the pinned native sources (completeness spine), PROBES every
 * feature's `example` through the three oracles (ruby cddl / rust cddl / cddl-codegen), emits
 * verify_report.json, and — ONLY when the gate passes — rewrites annotations/cddl_codegen.toml from the
 * probe results (a failing run must not leave a poisoned execution-grounded file to commit). Authority
 * model: the ruby `cddl` reference decides example validity; the rust `cddl` crate only corroborates
 * (ruby-accepts-but-rust-rejects is a recorded parser limitation).
 *
 * The cddl-codegen probe is EXECUTION-GATED (TESTING_ROADMAP item 1 / c6): generation runs with
 * `--emit-tests=true` and "supported" requires `cargo test` of the generated crate to PASS — the
 * IR-minted round-trip/reject tests must execute green, not merely compile. So the matrix's
 * "supported" verdict means "round-trips" wherever the type mints a test surface (recorded per probe
 * as `minted`). Shapes with no STANDALONE mint surface — transparent aliases, bounded/newtype-able
 * aliases, named tables/arrays (orphan-rule: no standalone Serialize), pure c-enums — get an EMBED
 * FALLBACK: the probe re-runs with a synthetic record holder wrapping the rule
 * (`__probe_holder = [0, <rule>]`) so the type's ONLY wire path (its embed site) executes, and the
 * evidence reads "round-trips when embedded" (recorded per probe as `embedded`). The fallback only
 * UPGRADES evidence — a synthetic that can't generate (generic rule, panic) or can't round-trip falls
 * back to the compile verdict, so "supported" is never overstated.
 *
 * By DEFAULT the cddl-codegen probe ALSO runs a wasm oracle: it regenerates each example with
 * `--wasm=true --emit-tests=true` and `cargo test`s the generated wasm crate (the emitted
 * `cddl_generated_wasm_tests` module — cross-crate byte differential + wire round-trip + accessor
 * read-back), recorded as additional per-probe evidence (`minted_wasm` / `wasm_roundtrips`). The rust
 * round-trip verdict still gates support; wasm is corroborating evidence. Opt out for a faster run with
 * `--no-wasm` (or `VERIFY_WASM=0`), which roughly halves per-probe cargo work.
 *
 * Exit codes: 0 PASS · 1 hard failure (gate) · 2 HARNESS failure (broken environment / oracle paths /
 * repeated timeouts — verdicts were not trustworthy, so no verdict files were (re)written).
 *
 * Run from cddl-matrix/:  bun run build_matrix.ts && bun run verify.ts
 */
import { existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
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
// custom ser/deser fn), so the crate cannot compile STANDALONE — by design (and a crate that can't
// compile can't run its emitted tests either). The execution-gate would false-negative them; they ARE
// supported (integration-tested where the user code is provided). Exempt = compile/test results ignored,
// support from generation exit only. Reason cites the integration test that DOES cover them, so the
// exemption isn't blind.
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
  ruby: number; rust: number; codegen: number; compile: number; test: number; minted: boolean;
  minted_wasm?: boolean; wasm_roundtrips?: number;
  // Embed-fallback outcome (feature/control-op loops only, when the base probe minted nothing):
  // true = round-trips inside a synthetic record holder; false = holder minted but round-trip failed;
  // undefined = not applicable (base already minted) or the synthetic was un-embeddable/failed to generate.
  embedded?: boolean;
}
interface ContainmentCorr {
  id: string; spec_declared: string | null; spec_observed: string; ruby: number; rust: number;
  parser_limitation: boolean; contradiction: boolean; example: string;
  codegen: number; compile: number; test: number; minted: boolean;
  minted_wasm?: boolean; wasm_roundtrips?: number;
  support: string | null;  // per-cell cddl-codegen support (the role × feature axis)
}
interface AltCoverage {
  abnf_alternatives: string[]; feature_rows: string[]; covered: string[]; uncovered: string[]; modeled: boolean;
}

function resolveRubyCddl(): string | null {
  if (process.env.RUBY_CDDL) {
    if (existsSync(process.env.RUBY_CDDL)) return process.env.RUBY_CDDL;
    // an explicitly pinned oracle that doesn't exist must not silently fall back to gem discovery —
    // the run would probe a different ruby cddl than the operator intended.
    console.error(`HARNESS FAILURE: RUBY_CDDL is set to '${process.env.RUBY_CDDL}' but no such file exists.`);
    process.exit(2);
  }
  try {
    const r = Bun.spawnSync(["ruby", "-e", "puts Gem.user_dir"], { stdout: "pipe", stderr: "ignore" });
    const cand = join((r.stdout?.toString() ?? "").trim(), "bin", "cddl"); // where the `cddl` gem installs
    if (existsSync(cand)) return cand;
  } catch { /* ruby not installed */ }
  return null;
}
const RUBY_CDDL = resolveRubyCddl();
// Validate the rust oracle upfront too: Bun.spawnSync throws ENOENT on a missing binary, which would
// otherwise surface as a raw stack trace minutes into the probe loop.
if (!existsSync(RUST_CDDL)) {
  console.error(`HARNESS FAILURE: rust cddl oracle not found at '${RUST_CDDL}' (set RUST_CDDL or build the sibling repo).`);
  process.exit(2);
}

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
// Structured extraction of the vendor surface (shared by the backward lint below and the forward lint):
// matching the tag("@…")/MARKER constructs rather than substring-searching the whole source means a
// directive that survives only in a comment or unrelated string no longer passes the backward lint.
const dslDirectives = [...dslSource.matchAll(/tag\("(@[a-z_]+)"\)/g)].map(m => m[1]);
const dslMarkers = [...dslSource.matchAll(/MARKER[^"]*"(_CDDL_CODEGEN_[^"]+)"/g)].map(m => m[1]);
// Floor assertion: if a refactor of comment_ast.rs/parsing.rs changes the extractable shape, both lints
// built on these sets would go vacuous (forward) or flag everything (backward) — fail loud instead.
if (dslDirectives.length === 0 || dslMarkers.length === 0) {
  console.error(`HARNESS FAILURE: vendor-source extraction went vacuous (directives=${dslDirectives.length}, markers=${dslMarkers.length}); comment_ast.rs/parsing.rs no longer match the extraction patterns.`);
  process.exit(2);
}
const dslTokens = new Set([...dslDirectives, ...dslMarkers]);
const fabricated: { id: string; production: string | null }[] = [];
for (const f of features) {
  const prod = f.production;
  if (prod === PRELUDE_PSEUDO) continue;
  if (prod && CDDL_CODEGEN_PSEUDO.has(prod)) {
    if (f.alt && dslTokens.has(f.alt)) continue;         // resolves to the pinned vendor source
    fabricated.push({ id: f.id, production: prod });     // alt absent from the extracted surface -> fabricated
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
// Floor assertion for the HARD gate: productionAlternatives stops at the first blank line inside a
// production block, so a re-pinned ABNF with a mid-block blank line would silently TRUNCATE the
// alternatives list and shrink the gate's `uncovered` set (the vacuous-pass direction). Pin the count.
const TYPE2_MIN_ALTERNATIVES = 12; // the pinned cddl-1-1-update.abnf type2 block
if (alt_coverage["type2"].abnf_alternatives.length < TYPE2_MIN_ALTERNATIVES) {
  console.error(`HARNESS FAILURE: type2 extraction yielded ${alt_coverage["type2"].abnf_alternatives.length} alternatives (expected >= ${TYPE2_MIN_ALTERNATIVES}); the ABNF block extraction truncated.`);
  process.exit(2);
}

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

// DEFAULT-ON wasm probe (opt out with `--no-wasm` argv or VERIFY_WASM=0 env): additionally generate
// each example with `--wasm=true --emit-tests=true` and `cargo test` the generated WASM crate, so the
// emitted `cddl_generated_wasm_tests` module (cross-crate byte differential + wire round-trip + accessor
// read-back + boundary acceptance) RUNS as a second execution oracle. It roughly doubles per-probe cargo
// work (own crate build + wasm-bindgen deps); operators who need the faster run opt out. The rust
// round-trip verdict still gates support — the wasm result is recorded as additional evidence only. Its
// own out/target dirs keep it from disturbing the rust probe's compile classification.
const WASM_PROBE =
  !process.argv.includes("--no-wasm") && !["0", "false"].includes((process.env.VERIFY_WASM ?? "").toLowerCase());
const ccOutWasm = join(probeDir, "cc_out_wasm");
const WASM_TARGET = WASM_PROBE ? mkdtempSync(join(tmpdir(), "cddl_verify_wasm_target_")) : "";

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

// A negative exit (timeout / signal kill) is a HARNESS condition, not a probe verdict: derive() would
// classify it as "rejected at parse/lex" and silently flip a genuinely-supported feature to
// "unsupported" under a PASS. Retry once (transient hiccup), then abort the whole run.
let harness_timeouts_retried = 0;
function runProbe(cmd: string[], cwd?: string, env?: Record<string, string>, timeoutS = PROBE_TIMEOUT): number {
  let exit = runExit(cmd, cwd, env, timeoutS);
  if (exit < 0) {
    harness_timeouts_retried++;
    exit = runExit(cmd, cwd, env, timeoutS);
    if (exit < 0) {
      console.error(`HARNESS FAILURE: probe timed out / was killed twice (exit ${exit}): ${cmd.join(" ")}`);
      process.exit(2);
    }
  }
  return exit;
}

// COMPILE (classification only): `cargo check` the generated crate. Run when `cargo test` FAILED, to
// split "generates but does not compile" (e.g. `x = any` -> `pub type X = Any;`, a type defined
// nowhere) from "compiles but the emitted tests fail". Mirrors
// integration_tests::feature_corpus_compiles (rust-only, shared CARGO_TARGET_DIR).
function runCompile(timeoutS = PROBE_TIMEOUT): number {
  return runProbe(
    ["cargo", "check", "--manifest-path", join(ccOut, "rust", "Cargo.toml")],
    CODEGEN_DIR, { CARGO_TARGET_DIR: COMPILE_TARGET }, timeoutS,
  );
}

// EXECUTION-GATE: `cargo test` the generated crate — compiles the lib AND runs the `--emit-tests`
// round-trip/reject module (strictly stronger than `cargo check`). Caller invokes only when
// generation succeeded.
function runTest(timeoutS = PROBE_TIMEOUT): number {
  return runProbe(
    ["cargo", "test", "--manifest-path", join(ccOut, "rust", "Cargo.toml")],
    CODEGEN_DIR, { CARGO_TARGET_DIR: COMPILE_TARGET }, timeoutS,
  );
}

// The generator MERGES into an existing output dir (it never clears it), so a partially-written crate
// from a panicking probe — or any future conditionally-emitted module — would leak into the next
// probe's compile gate. Start every generation from an empty dir.
const cleanOut = () => rmSync(ccOut, { recursive: true, force: true });
function runCodegen(): number {
  cleanOut();
  return runProbe(["cargo", "run", "-q", "--", `--input=${probeFile}`, `--output=${ccOut}`, "--wasm=false", "--emit-tests=true"], CODEGEN_DIR);
}

// WASM oracle (default-on): generate the SAME example with `--wasm=true` into a separate out dir, then
// `cargo test` the wasm crate — which builds the rust crate as a (non-test) path dep AND compiles+runs
// the emitted `cddl_generated_wasm_tests` module. Separate out dir so it never perturbs the rust
// probe's ccOut; separate target so its wasm-bindgen deps don't invalidate the rust compile cache.
const cleanOutWasm = () => rmSync(ccOutWasm, { recursive: true, force: true });
function runCodegenWasm(): number {
  cleanOutWasm();
  return runProbe(["cargo", "run", "-q", "--", `--input=${probeFile}`, `--output=${ccOutWasm}`, "--wasm=true", "--emit-tests=true"], CODEGEN_DIR);
}
function runWasmTest(timeoutS = PROBE_TIMEOUT): number {
  return runProbe(
    ["cargo", "test", "--manifest-path", join(ccOutWasm, "wasm", "Cargo.toml")],
    CODEGEN_DIR, { CARGO_TARGET_DIR: WASM_TARGET }, timeoutS,
  );
}

// The full cddl-codegen probe: generate (with --emit-tests) -> `cargo test`. On the green path a single
// `cargo test` suffices (its success implies the lib compiles, recorded as compile 0); only a test
// FAILURE pays for the extra `cargo check` that classifies it. -2 = not reached (n/a).
// `minted` = the emitted lib actually contains the generated-test module; without it a `cargo test`
// pass is vacuous (transparent aliases / pure c-enums mint nothing, by design — skipped loudly by the
// emitter), so the verdict evidence must not claim "round-trips" for those.
// `minted_wasm` / `wasm_roundtrips` are populated under the WASM_PROBE (default-on; undefined when
// opted out via --no-wasm / VERIFY_WASM=0, so they're then omitted from the JSON report and add no wasm
// evidence to annotations — an opted-out run's output is byte-identical to a pre-wasm-probe run).
// `wasm_roundtrips` is the `cargo test` exit of the generated wasm crate; `minted_wasm` = its lib
// actually contains the generated wasm-test module (else a green `cargo test` is vacuous, same caveat
// as `minted`).
interface CodegenProbe { gen: number; compile: number; test: number; minted: boolean; minted_wasm?: boolean; wasm_roundtrips?: number }
function probeCodegen(): CodegenProbe {
  const gen = runCodegen();
  // A spec that doesn't generate at all has no wasm crate to test either (the wasm probe re-runs the
  // SAME generator with `--wasm=true`), so skip the doomed wasm generation — the rust verdict already
  // records the parse/panic. wasm fields stay undefined -> no (redundant) wasm evidence clause.
  if (gen !== 0) return { gen, compile: -2, test: -2, minted: false };
  const libPath = join(ccOut, "rust", "src", "lib.rs");
  const minted = existsSync(libPath) && readFileSync(libPath, "utf8").includes("mod cddl_generated_tests");
  const test = runTest();
  const compile = test === 0 ? 0 : runCompile();
  return { gen, compile, test, minted, ...wasmProbe() };
}

// The wasm half of a probe (default-on): generate `--wasm=true`, cargo test the wasm crate. Returns {}
// when opted out so the fields stay undefined (report/annotation output matches a pre-wasm-probe run).
function wasmProbe(): { minted_wasm?: boolean; wasm_roundtrips?: number } {
  if (!WASM_PROBE) return {};
  const gen = runCodegenWasm();
  if (gen !== 0) return { minted_wasm: false, wasm_roundtrips: gen };
  const wasmLib = join(ccOutWasm, "wasm", "src", "lib.rs");
  const minted_wasm = existsSync(wasmLib) && readFileSync(wasmLib, "utf8").includes("mod cddl_generated_wasm_tests");
  return { minted_wasm, wasm_roundtrips: runWasmTest() };
}

// The cddl-codegen half of the support verdict, shared by the feature / per-cell / control-op loops so
// the "supported means round-trips" semantics can't drift apart between them.
function codegenVerdict(p: CodegenProbe): { supported: boolean; detail: string } {
  if (p.gen === 0 && p.test === 0)
    return { supported: true, detail: p.minted ? "exit 0; compiles; round-trips" : "exit 0; compiles (no minted round-trip surface)" };
  if (p.gen === 0 && p.compile !== 0)
    return { supported: false, detail: `generates but does not compile (cargo check exit ${p.compile})` };
  if (p.gen === 0)
    return { supported: false, detail: `compiles but emitted round-trip tests fail (cargo test exit ${p.test})` };
  if (p.gen === 101) return { supported: false, detail: "panic (exit 101)" };
  return { supported: false, detail: `rejected at parse/lex (exit ${p.gen})` };
}

// Honest wasm-oracle evidence suffix (default-on). "" when the wasm probe didn't run (opted out / rust
// gen failed / fields undefined) so an opted-out run's annotations are unchanged. `exempt` features reference
// user-supplied code, so — exactly like the rust standalone-compile exemption — the wasm crate can't be
// `cargo test`ed standalone; say N/A, not FAILED. Same fallback discipline as `minted`: a green test with
// nothing minted is "compiles (no minted wasm surface)", NOT a round-trip; a failure is worded by whether
// a module was actually minted (module present -> a round-trip assertion failed; absent -> the crate
// itself failed to compile, mirroring the rust compile verdict).
function wasmEvidence(minted_wasm?: boolean, wasm_roundtrips?: number, exempt = false): string {
  if (wasm_roundtrips === undefined) return "";
  if (exempt) return "; wasm standalone-test N/A (user-supplied code)";
  if (wasm_roundtrips === 0)
    return minted_wasm ? "; wasm round-trips" : "; wasm compiles (no minted wasm surface)";
  return minted_wasm
    ? `; wasm round-trip FAILED (cargo test exit ${wasm_roundtrips})`
    : `; wasm crate failed to compile (cargo test exit ${wasm_roundtrips})`;
}

// --- EMBED FALLBACK for shapes with no STANDALONE mint surface -----------------------------------
// A large class of supported shapes mints NO standalone round-trip test: a transparent alias emits a
// bare `pub type` with no methods; a bounded/newtype-able alias (`g = [2*5 uint]`), a named Table/Array
// struct, and a pure c-enum have no standalone `Serialize` impl (the orphan rule forbids one for a type
// the crate doesn't own the wire contract of standalone). Their ONLY wire path is at an EMBED site —
// where a record field of that type serializes/deserializes through the enclosing struct. So when the
// base probe minted nothing, RE-PROBE with a synthetic record holder that wraps the probed rule
// (`__probe_holder = [0, <rule>]` — the leading literal forces a 2+ element heterogeneous record so the
// existing record minter fires, never a collapsed single-field alias). Evidence then reads "round-trips
// when embedded", which is honest: it round-trips at the only wire path these shapes have.
//
// A standalone mint surface would need generator surface changes nobody has asked for; this closes the
// coverage PROBE-SIDE with zero generator changes. The synthetic re-probe can ONLY UPGRADE evidence: a
// synthetic that fails to generate (a generic rule needing type args, a panic) or fails to mint/round-trip
// falls back to the base compile verdict, LOUDLY logged, and never downgrades a previously-supported probe.
const HOLDER_RULE = "__probe_holder";

// The probed rule = the FIRST rule defined in the example (the identifier before the first `=`, `/=`, or
// `//=`). A generic rule head (`foo<a> = …`) can't be referenced without type arguments, so it is not
// embeddable — detected by the `<…>` after the name and skipped with a recorded reason.
function firstRuleName(example: string): { name: string; generic: boolean } | null {
  for (const raw of example.split(/\r\n|\r|\n/)) {
    const line = raw.trim();
    if (!line || line.startsWith(";")) continue;
    const m = line.match(/^([A-Za-z@_$][A-Za-z0-9@_$.-]*)\s*(<[^=]*>)?\s*(\/\/=|\/=|=)(?![=>])/);
    if (m) return { name: m[1], generic: m[2] !== undefined };
  }
  return null;
}

// Runs ONLY when the base probe generated but minted no standalone surface (gen 0, minted false).
// Returns embedded=true iff the synthetic holder minted AND its `cargo test` round-trips green;
// embedded stays undefined on any skip/failure (kept the compile verdict). Every path logs one loud
// `[embed]` line so a skip or a failed embed is visible in the run output, not silent.
function embedFallback(id: string, example: string, cg: CodegenProbe): boolean | undefined {
  if (cg.gen !== 0 || cg.minted) return undefined; // not applicable — base already mints (or didn't generate)
  const log = (note: string) => console.log(`  [embed] ${id}: ${note}`);
  const rule = firstRuleName(example);
  if (!rule) { log("no parseable rule head; embed skipped (kept compile verdict)"); return undefined; }
  if (rule.generic) { log(`generic rule '${rule.name}' needs type args; not embeddable (kept compile verdict)`); return undefined; }
  writeFileSync(probeFile, `${example}\n${HOLDER_RULE} = [0, ${rule.name}]\n`);
  const gen = runCodegen();
  if (gen !== 0) { log(`synthetic holder failed to generate (cargo run exit ${gen}); kept compile verdict`); return undefined; }
  const libPath = join(ccOut, "rust", "src", "lib.rs");
  const minted = existsSync(libPath) && readFileSync(libPath, "utf8").includes("mod cddl_generated_tests");
  if (!minted) { log("synthetic holder minted no test surface; kept compile verdict"); return undefined; }
  const test = runTest();
  if (test === 0) { log(`round-trips when embedded in ${HOLDER_RULE}`); return true; }
  log(`embedded round-trip FAILED (cargo test exit ${test}); kept compile verdict`);
  return false;
}

// Evidence-text upgrade: when the type round-trips embedded, replace the honest-but-weak compile clause
// with the embedded round-trip clause. Any other embed outcome (skipped / failed) leaves the base
// evidence untouched so support is never overstated.
function embedDetail(detail: string, embedded?: boolean): string {
  if (embedded !== true) return detail;
  return detail.replace("compiles (no minted round-trip surface)", "compiles; round-trips when embedded (synthetic record holder)");
}

// [ruby, rust, codegen probe].
function oracles(example: string): [number, number, CodegenProbe] {
  writeFileSync(probeFile, example + "\n");
  const a = RUBY_CDDL ? runProbe([RUBY_CDDL, probeFile, "generate", "1"]) : -2;
  const b = runProbe([RUST_CDDL, "compile-cddl", "--cddl", probeFile]);
  return [a, b, probeCodegen()];
}

function derive(featureId: string, profile: string, rubyExit: number, rustExit: number, cg: CodegenProbe): Derived {
  const valid_a = rubyExit === 0;
  const valid_b = rustExit === 0;
  // CDDL_CODEGEN is a vendor profile: cddl-codegen IS the spec authority, so the ruby/rust reference
  // oracles don't gate validity (they reject the sentinel typenames — expected). Validity comes from the
  // backward lint (the construct resolves to the in-repo vendor source), so treat it as spec-valid here.
  const isVendor = profile === "CDDL_CODEGEN";
  const spec_valid = isVendor ? true : valid_a;
  const parser_limitation = !isVendor && valid_a && !valid_b;
  // EXECUTION-GATED support: "supported" requires generation AND a passing `cargo test` (the emitted
  // round-trip/reject module). Generation-only, or compiles-but-fails-round-trip, are false positives —
  // each recorded distinctly so the gap class is visible.
  // EXEMPT features (user-supplied code) skip the compile/test bits — they can't compile standalone by design.
  const compileExempt = Object.hasOwn(COMPILE_GATE_EXEMPT, featureId);
  let support: string, support_detail: string;
  if (compileExempt && cg.gen === 0) {
    support = "supported";
    support_detail = `exit 0; standalone-compile N/A (${COMPILE_GATE_EXEMPT[featureId]})`;
  } else {
    const v = codegenVerdict(cg);
    support = v.supported ? "supported" : "unsupported";
    support_detail = v.detail;
  }
  const out_of_profile = spec_valid && support !== "supported" && profileNewerThanTarget(profile);
  let status: string;
  if (!spec_valid) status = Object.hasOwn(CONFLICT_ALLOWLIST, featureId) ? "uncertain" : "spec_invalid";
  else if (out_of_profile) status = "out_of_profile";
  else status = support;
  return { valid_a, valid_b, spec_valid, parser_limitation, support, support_detail, out_of_profile, status };
}

// Warm the shared compile target ONCE (deps + the libtest harness build here; per-probe `cargo test`
// is then incremental and fits PROBE_TIMEOUT). Without this the first probe's test would eat the whole
// dep build and risk a spurious timeout -> false "does not round-trip". A trivial valid spec that MINTS
// a round-trip test is enough to pull in the deps and the test-profile artifacts.
//
// The warm-up doubles as the HARNESS SELF-TEST: it runs a known-good spec through the full
// generate+test pipeline, so any failure here is by definition environmental (the generator itself
// doesn't build, cargo/registry/disk trouble). It MUST abort: cargo exits 101 for a compile error of
// the generator exactly like a per-feature panic, so an unhealthy harness would otherwise record every
// feature as "panic (exit 101)"/unsupported and still print PASS (no hard-fail term inspects probe
// exits). Both halves get the warm timeout — a cold full generator build can exceed PROBE_TIMEOUT.
// The warm spec must also MINT tests (assert below): a warm-up whose `cargo test` runs zero emitted
// tests would silently stop self-testing the execution half of the pipeline.
writeFileSync(probeFile, "warm = [uint, tstr]\n");
cleanOut();
const warmGen = runExit(["cargo", "run", "-q", "--", `--input=${probeFile}`, `--output=${ccOut}`, "--wasm=false", "--emit-tests=true"], CODEGEN_DIR, undefined, COMPILE_WARM_TIMEOUT);
const warmLib = warmGen === 0 ? readFileSync(join(ccOut, "rust", "src", "lib.rs"), "utf8") : "";
const warmTest = warmGen === 0 ? runExit(["cargo", "test", "--manifest-path", join(ccOut, "rust", "Cargo.toml")], CODEGEN_DIR, { CARGO_TARGET_DIR: COMPILE_TARGET }, COMPILE_WARM_TIMEOUT) : -2;
if (warmGen !== 0 || warmTest !== 0 || !warmLib.includes("mod cddl_generated_tests")) {
  console.error(`HARNESS FAILURE: warm-up on a known-good spec failed (generate exit ${warmGen}, cargo test exit ${warmTest}, minted=${warmLib.includes("mod cddl_generated_tests")}). The environment is unhealthy; no probes were run and nothing was written.`);
  process.exit(2);
}

// Warm the WASM target the same way when the wasm probe is on (the default): the first wasm crate build (wasm-bindgen
// + the libtest harness) can exceed PROBE_TIMEOUT, which would false-fail the first per-probe wasm test.
// Doubles as the wasm-oracle self-test — a known-good spec that MINTS a wasm module must round-trip green.
if (WASM_PROBE) {
  writeFileSync(probeFile, "warm = [uint, tstr]\n");
  const wgen = runExit(["cargo", "run", "-q", "--", `--input=${probeFile}`, `--output=${ccOutWasm}`, "--wasm=true", "--emit-tests=true"], CODEGEN_DIR, undefined, COMPILE_WARM_TIMEOUT);
  const wlib = wgen === 0 ? readFileSync(join(ccOutWasm, "wasm", "src", "lib.rs"), "utf8") : "";
  const wtest = wgen === 0 ? runExit(["cargo", "test", "--manifest-path", join(ccOutWasm, "wasm", "Cargo.toml")], CODEGEN_DIR, { CARGO_TARGET_DIR: WASM_TARGET }, COMPILE_WARM_TIMEOUT) : -2;
  if (wgen !== 0 || wtest !== 0 || !wlib.includes("mod cddl_generated_wasm_tests")) {
    console.error(`HARNESS FAILURE: wasm warm-up on a known-good spec failed (generate exit ${wgen}, cargo test exit ${wtest}, minted=${wlib.includes("mod cddl_generated_wasm_tests")}). The --wasm probe environment is unhealthy; no probes were run and nothing was written.`);
    process.exit(2);
  }
}

const probe_results: ProbeResult[] = [];
for (const f of [...features].sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))) {
  const [a, b, cg] = oracles(f.example);
  const profile = f.profile ?? "RFC8610";
  const d = derive(f.id, profile, a, b, cg);
  // Embed fallback (evidence-only): re-probe unmintable shapes wrapped in a synthetic record holder so
  // their embed-site wire path executes. Never changes `d.support` — only enriches the evidence text.
  const embedded = embedFallback(f.id, f.example, cg);
  probe_results.push({ id: f.id, production: f.production ?? null, profile, example: f.example, ruby: a, rust: b, codegen: cg.gen, compile: cg.compile, test: cg.test, minted: cg.minted, minted_wasm: cg.minted_wasm, wasm_roundtrips: cg.wasm_roundtrips, embedded, ...d });
}

// Second harness-health layer (the warm-up catches a broken environment at startup; this catches one
// that degrades mid-run): zero supported features is not a plausible verdict shape for this repo.
if (!probe_results.some(pr => pr.support === "supported")) {
  console.error("HARNESS FAILURE: no feature probed 'supported' — implausible verdict shape; refusing to write verdicts.");
  process.exit(2);
}

// Containment corroboration + PER-CELL support. The role × feature axis: a feature's support can DIFFER
// by nesting context (e.g. a literal value is supported as an array member but not as a top-level type).
// ruby/rust corroborate the spec verdict; cddl-codegen is probed per cell for an execution-grounded
// support bit, exactly like the per-feature probe — written to annotations keyed by the containment id.
// A containment cell without an `example` gets no spec corroboration, no per-cell support probe, and
// no annotation row — a silent coverage hole (control ops already hard-gate the identical situation;
// mirror that here so adding an example-less cell is loud, not a quiet shrink of the probed set).
const containment_missing_example = contain.filter(c => !c.example).map(c => c.id).sort();
const containment_corroboration: ContainmentCorr[] = [];
for (const c of contain.filter(x => x.example).sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))) {
  writeFileSync(probeFile, c.example + "\n");
  const a = RUBY_CDDL ? runProbe([RUBY_CDDL, probeFile, "generate", "1"]) : -2;
  const b = runProbe([RUST_CDDL, "compile-cddl", "--cddl", probeFile]);
  // only probe support where the nesting is spec-valid (ruby accepts); a spec-disallowed cell isn't
  // valid CDDL, so "does cddl-codegen support it" is meaningless.
  const cg = a === 0 ? probeCodegen() : { gen: -2, compile: -2, test: -2, minted: false };
  // execution-gate the cell too (same false-positive class as the feature axis)
  const support = a === 0 ? (codegenVerdict(cg).supported ? "supported" : "unsupported") : null;
  const observed = a === 0 ? "allowed" : "disallowed";
  const parser_limitation = a === 0 && b !== 0;  // directional, matching the feature-level definition (ruby accepts, rust rejects)
  const contradiction = observed !== c.spec;
  containment_corroboration.push({
    id: c.id, spec_declared: c.spec ?? null, spec_observed: observed,
    ruby: a, rust: b, parser_limitation, contradiction, example: c.example!,
    codegen: cg.gen, compile: cg.compile, test: cg.test, minted: cg.minted,
    minted_wasm: cg.minted_wasm, wasm_roundtrips: cg.wasm_roundtrips, support,
  });
}

// CONTROL-OP support (item 7). Probe each IANA op's minimal `example` through cddl-codegen (compile-gated)
// -> a [[support]] row keyed by ctl.<name>, the same pattern as features. The op set is IANA-registry-
// authoritative, so ruby/rust are CORROBORATION ONLY (an op a given ruby/rust version lacks is not
// "invalid CDDL"): support is purely the cddl-codegen verdict, supported/unsupported (no out_of_profile —
// the control-op extension RFCs 9090/9165/9741 are a separate axis from the grammar profile).
interface ControlOpSupport { id: string; name: string; support: string; detail: string; ruby: number; rust: number; codegen: number; compile: number; test: number; minted: boolean; embedded?: boolean; example: string }
const controlop_missing_example = control_ops.filter(co => !co.example).map(co => co.id);
// ruby exit 65 (EX_DATAERR) can mean the example is malformed — but ALSO that ruby's generate mode
// simply can't handle an op it postdates (verified: the RFC-correct `.printf ["%04x", 20]`, which
// the rust crate parses, still exits 65). Neither oracle can separate the two for registry ops it
// doesn't implement, so this CANNOT be a hard gate — it is surfaced as a review list instead: an
// example appearing here is uncorroborated by the reference, so check it against the defining RFC
// by hand before trusting its "unsupported" verdict.
const controlop_uncorroborated: string[] = [];
const controlop_support: ControlOpSupport[] = [];
for (const co of [...control_ops].filter(co => co.example).sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))) {
  const [a, b, cg] = oracles(co.example!);
  // ANY nonzero ruby exit means the reference did not confirm the example is valid CDDL — not just
  // 65 (EX_DATAERR). Keying on 65 alone hid exit-1 cases (malformed controllers, valid examples of
  // ops ruby rejects with a different code) — 3 of the 5 historical malformed forms among them. The
  // exit code is recorded per id so a malformed-example regression stays distinguishable from the
  // unimplemented-op noise this list is expected to carry.
  if (a !== 0) controlop_uncorroborated.push(`${co.id} (ruby exit ${a})`);
  const v = codegenVerdict(cg);
  // Same embed fallback as the feature loop (trivial generalization — control-op examples are single
  // named rules): an op whose annotated type mints no standalone surface (`.cbor`, `.ge` -> a bounded
  // alias) round-trips at its embed site, upgrading the evidence without touching the support verdict.
  const embedded = embedFallback(co.id, co.example!, cg);
  controlop_support.push({ id: co.id, name: co.name, support: v.supported ? "supported" : "unsupported", detail: v.detail, ruby: a, rust: b, codegen: cg.gen, compile: cg.compile, test: cg.test, minted: cg.minted, embedded, example: co.example! });
}

// Same harness-health guard as the feature loop (line ~390), extended to the two loops that run
// AFTER it. A mid-run degradation (the cddl-codegen probe binary going missing / crashing) rewrites
// every per-cell and per-op verdict to "unsupported (panic exit 101)" while the feature-only gate
// above still passes — the exact "failing looks passing" class this file exists to prevent. A real
// run always has some supported cells/ops (18/36 and 9/37 at time of writing), so zero is
// implausible: refuse to write verdicts rather than emit an all-unsupported annotation set.
if (containment_corroboration.length && !containment_corroboration.some(c => c.support === "supported")) {
  console.error("HARNESS FAILURE: no containment cell probed 'supported' — implausible verdict shape; refusing to write verdicts.");
  process.exit(2);
}
if (controlop_support.length && !controlop_support.some(c => c.support === "supported")) {
  console.error("HARNESS FAILURE: no control-op probed 'supported' — implausible verdict shape; refusing to write verdicts.");
  process.exit(2);
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
  "#   cddl-codegen --input=... --wasm=false --emit-tests=true (generate the crate) THEN `cargo test` it",
  "#     (the EXECUTION-GATE: the emitted IR-minted round-trip/reject tests must PASS — strictly",
  "#     stronger than compiling). support = generates AND compiles AND round-trips. exit 0 alone is",
  "#     NOT enough: `x = any` generates `pub type X = Any;` (a type defined nowhere) which fails to",
  "#     compile -> unsupported, not a false 'supported'. A type that mints no STANDALONE test surface",
  "#     (transparent alias / bounded-or-newtype-able alias / named table or array / pure c-enum) is",
  "#     RE-PROBED wrapped in a synthetic record holder (`__probe_holder = [0, <rule>]`) so its embed-",
  "#     site wire path runs: evidence then reads 'round-trips when embedded (synthetic record holder)'.",
  "#     If the synthetic can't generate (a generic rule needing type args) or can't round-trip, the",
  "#     evidence stays 'no minted round-trip surface' — the embed only ever UPGRADES it.",
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
  let ev = `probe: cddl-codegen ${embedDetail(pr.support_detail ?? "exit " + pr.codegen, pr.embedded)}${wasmEvidence(pr.minted_wasm, pr.wasm_roundtrips, Object.hasOwn(COMPILE_GATE_EXEMPT, pr.id))}; ruby=${ok(pr.ruby)} rust=${ok(pr.rust)}`;
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
  const roundtrips = c.test === 0 ? (c.minted ? "ok" : "n/a (nothing minted)") : "fail";
  const compile = c.codegen === 0 ? `; compiles=${c.compile === 0 ? "ok" : "fail"}; round-trips=${roundtrips}` : "";
  const ev = `probe (cell): cddl-codegen exit ${c.codegen}${compile}${wasmEvidence(c.minted_wasm, c.wasm_roundtrips)}; ruby=${ok(c.ruby)} rust=${ok(c.rust)}`;
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
  const ev = `probe (control-op): cddl-codegen ${embedDetail(co.detail, co.embedded)}; ruby=${ok(co.ruby)} rust=${ok(co.rust)}`;
  annoLines.push("[[support]]");
  annoLines.push(`id = ${tomlStr(co.id)}`);
  annoLines.push(`status = ${tomlStr(co.support)}`);
  annoLines.push(`evidence = ${tomlStr(ev)}`);
  annoLines.push("");
}
// Written only after the gate passes (bottom of the script): a hard-failing run must not leave a
// poisoned "EXECUTION-GROUNDED" file on disk for the operator to accidentally commit.
const annoPath = `${ROOT}/annotations/cddl_codegen.toml`;
const annoContent = annoLines.join("\n").replace(/\s+$/, "") + "\n";

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
  controlop_uncorroborated,
  containment_corroboration,
  containment_contradictions: containment_contradictions.map(c => c.id),
  containment_parser_limitations: [...containment_parser_limitations].sort(),
  containment_missing_example,
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
    controlop_uncorroborated: controlop_uncorroborated.length,
    abnf_productions: abnf_productions.size,
    prelude_names: prelude_names.length,
    supported: probe_results.filter(pr => pr.status === "supported").length,
    unsupported: probe_results.filter(pr => pr.status === "unsupported").length,
    // How many probes upgraded from compile-only to embedded-round-trip evidence via the synthetic
    // holder (feature loop + control-op loop). A drop toward zero signals the embed fallback rotted.
    embedded_upgraded: probe_results.filter(pr => pr.embedded === true).length,
    embedded_upgraded_controlops: controlop_support.filter(c => c.embedded === true).length,
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
    containment_missing_example: containment_missing_example.length,
    harness_timeouts_retried,
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
console.log(`embed-fallback      : ${s.embedded_upgraded} feature probe(s) + ${s.embedded_upgraded_controlops} control-op(s) upgraded compile-only -> round-trips-when-embedded`);
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
if (containment_missing_example.length) {
  console.log("\nCONTAINMENT CELLS MISSING AN EXAMPLE (unprobed, uncorroborated — add to containment/*.toml):");
  for (const id of containment_missing_example) console.log(`  - ${id}`);
}
if (controlop_uncorroborated.length) {
  console.log("\nCONTROL-OP EXAMPLES UNCORROBORATED BY THE REFERENCE (nonzero ruby exit — either the example is malformed OR ruby postdates/rejects the op; REVIEW against the defining RFC):");
  for (const id of controlop_uncorroborated) console.log(`  - ${id}`);
}
if (harness_timeouts_retried)
  console.log(`\nNOTE: ${harness_timeouts_retried} probe(s) timed out / were killed and succeeded on retry.`);

console.log("\nwrote verify_report.json");

const hard_fail = fabricated.length || gaps.length || cddl_codegen_gaps.length || link_errors.length || type2_uncovered.length ||
  spec_invalid.length || containment_contradictions.length || controlop_missing_example.length || containment_missing_example.length;
if (hard_fail) { console.log("\nRESULT: FAIL (hard failure — see above; annotations/cddl_codegen.toml left untouched)"); process.exit(1); }
const prevAnno = existsSync(annoPath) ? readFileSync(annoPath, "utf8") : "";
writeFileSync(annoPath, annoContent);
console.log("wrote annotations/cddl_codegen.toml");
if (annoContent !== prevAnno)
  console.log("NOTE: annotations changed — re-run `bun run build_matrix.ts` to refresh matrix.json (CI's --check gates the committed form).");
console.log("\nRESULT: PASS");
process.exit(0);
