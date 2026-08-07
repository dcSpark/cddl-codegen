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
 * The cddl-codegen probe is EXECUTION-GATED: generation runs with
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
 * read-back), recorded as additional per-probe evidence — one field per STAGE of the leg
 * (`wasm_gen` / `minted_wasm` / `wasm_roundtrips`), so the clause names where a failure happened. The rust
 * round-trip verdict still gates support; wasm is corroborating evidence. Opt out for a faster run with
 * `--no-wasm` (or `VERIFY_WASM=0`), which roughly halves per-probe cargo work — but a `--no-wasm` run
 * rewrites every row's evidence WITHOUT the wasm bits (wholesale diff churn), so keep wasm ON for any
 * run whose annotations will be committed.
 *
 * Exit codes: 0 PASS · 1 hard failure (gate) · 2 HARNESS failure (broken environment / oracle paths /
 * repeated timeouts — verdicts were not trustworthy, so no verdict files were (re)written).
 *
 * Run from cddl-matrix/:  bun run build_matrix.ts && bun run verify.ts
 */
import { appendFileSync, existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, utimesSync, writeFileSync } from "node:fs";
import { constants, homedir, tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import {
  ALT_PRODUCTIONS, CORPUS_CATALOG_INTRO, CORPUS_DECODE_FLOOR_ARM_EXEMPT, CORPUS_HOLDER_RULE, CatalogRow,
  CatalogVector, CorpusRule, DECODE_FLOOR_ARM_EXEMPT, DECODE_REJECT_ORACLE_GAP_EXEMPT, GATE_CACHE_SCHEMA, GateCacheEntry, PRELUDE_NAMES, ROOT,
  annotationsHeaderLines, composeCatalog, corpusArmExample, corpusClosureBody, corpusProbeSpec, enumerateCorpusRules,
  gateCacheEnabled, gateCacheKey, grammarAltCoverage, hashTree, loadMatrixInputs, parseCatalog,
  readGateCacheEntry, resolveChoiceArmClasses, rubyGenerateIsBernoulli, stableJson, vectorShapeClass,
  writeGateCacheEntry,
} from "./lib";

process.chdir(ROOT);

const verifyStartedAt = Date.now();
const formatElapsed = (ms: number): string => {
  const totalSeconds = Math.floor(ms / 1000);
  const hours = Math.floor(totalSeconds / 3600);
  const minutes = Math.floor((totalSeconds % 3600) / 60);
  const seconds = totalSeconds % 60;
  const millis = ms % 1000;

  if (hours > 0) return `${hours}h ${minutes}m ${seconds}s`;
  if (minutes > 0) return `${minutes}m ${seconds}s`;
  if (seconds > 0) return `${seconds}.${millis.toString().padStart(3, "0")}s`;
  return `${millis}ms`;
};
// Scratch this run created under tmpdir(), removed by the exit handler below so a run cannot leak
// multi-GB cargo targets. Registered at creation (not hardcoded) because the early-exit guards above
// the creation sites — oracle-resolution, ALL_PROFILES extraction, the disk-headroom preflight — exit
// before any of it exists, and an exit handler must never reference a not-yet-initialised binding.
// The two CARGO_TARGET_DIRs are BUILD artifacts: nothing triages from them, and they are what the
// 2 GiB `diskHeadroomPreflight` floor is measured against, so they go unconditionally. `probeDir`
// holds the generated crate a HARNESS FAILURE names for triage (e.g. the `replay cargo test timed out
// twice (<dir>)` message), so it survives a non-zero exit and is announced — deleting it would leave
// those messages pointing at nothing. It is keep-last-1 (cleanOut/nextForeignOut delete the previous
// generation), so what survives a red run is one crate, not a run's worth.
const scratchTargets: string[] = [];
let scratchProbeDir = "";
process.on("exit", (code) => {
  console.log(`elapsed time        : ${formatElapsed(Date.now() - verifyStartedAt)}`);
  for (const d of scratchTargets) rmSync(d, { recursive: true, force: true });
  if (scratchProbeDir) {
    if (code === 0) rmSync(scratchProbeDir, { recursive: true, force: true });
    else console.log(`probe scratch kept for triage: ${scratchProbeDir}`);
  }
});

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

// Features whose generated code REFERENCES user-supplied items and for which NO definition this
// harness can write would make the crate compile: what is missing is a whole OTHER CRATE, not a type.
// Exempt = compile/test results ignored, support from generation exit only. Reason names what
// structurally blocks a def and cites the gate that does cover the shape, so the exemption isn't
// blind. Every OTHER user-code-referencing row is seeded instead — see `DEF_SPLICE` below.
const COMPILE_GATE_EXEMPT: Record<string, string> = {
  "dsl.rust_name":
    "the directive pins a DEPENDENCY crate's type name, so the generated `use extern_dep::…` needs that whole crate on the path — a local definition cannot supply a foreign crate root; integration-tested in src/tests/rust_name_tests.rs and the extern_import byte-identity pair",
  "dsl.extern_companions":
    "the directive defers the wasm companion classes to a SIBLING WASM CRATE, so the generated `use <path>::<Class>;` needs that crate to exist and a local definition would defeat the deferral it declares; integration-tested in src/tests/extern_companions_tests.rs and the two-crate wasm32 link gate extern_companions_defers_to_sibling_wasm_crate",
};

// --- DEF SPLICE: seed the user-supplied side so extern rows COMPILE instead of being exempt --------
// A `_CDDL_CODEGEN_EXTERN_TYPE_` / `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule, and a
// `@custom_serialize`/`@custom_deserialize` pair, name code the SPEC does not contain. That is a
// reason to write that code, not a reason to stop compiling: the emitted crate is where the
// bare-generic-base (E0107), dep-owned-schema-row (E0433) and undeclared-serialization-module (E0433)
// classes live, and each of them shipped because no gate built the crate they sat in.
//
// Residence is the DOCUMENTED user contract (docs/docs/output_format.mdx § Generated crate roots):
// type definitions and codec fns go into the seed-once thin `rust/src/lib.rs` / `wasm/src/lib.rs`,
// never into `src/generated/**` (clobbered every run, and the extern glue's own `pub use crate::<Name>;`
// would collide E0255). A codec FN additionally needs a hand `use` into the generated scope — the
// remedy comment_dsl.mdx names for a bare (non-path-qualified) codec name — which is the `reexport`
// field, emitted as `pub use crate::{…};` into `generated/mod.rs` so `serialization.rs`'s `use super::*;`
// resolves it.
//
// The templates live in `tests/def_templates/` and are name-parameterized, so ONE definition serves
// every rule name a shape can mint; `src/tests/integration_tests.rs` splices the same files for the
// feature-corpus and wasm-matrix compile floors.
interface SpliceDef {
  template: string;              // tests/def_templates/<template>.<rust|wasm>
  preserveTemplate?: string;     // substituted under --preserve-encodings (encoding-variable signature)
  name?: string;                 // __NAME__ — the rust type the marker rule mints
  ser?: string;                  // __SER__  — the @custom_serialize target
  deser?: string;                // __DESER__ — the @custom_deserialize target
  copy?: boolean;                // add `Copy` to __DERIVES__ (the @copy contract's compile-time assertion)
  reexport?: string[];           // names to `pub use crate::{…};` into generated/mod.rs
}
const DEF_SPLICE: Record<string, { rust: SpliceDef[]; wasm: SpliceDef[] }> = {
  "ext.extern": {
    rust: [{ template: "extern_type", name: "Foo" }],
    wasm: [{ template: "opaque_wrapper", name: "Foo" }],
  },
  "ext.extern.generic": {
    rust: [{ template: "generic_extern", name: "ExtGen" }],
    wasm: [{ template: "generic_extern", name: "ExtGen" }],
  },
  // WITH instances the two faces diverge: rust names the generic BASE (and aliases
  // `pub type ExtGenU64 = ExtGen<u64>;` itself), while wasm — which has no generics — demands a
  // concrete `ExtGenU64` wrapper. That asymmetry is the whole point of the paired cells.
  "ext.extern.generic_instance": {
    rust: [{ template: "generic_extern", name: "ExtGen" }],
    wasm: [{ template: "opaque_wrapper", name: "ExtGenU64" }],
  },
  "ext.raw_bytes": {
    rust: [{ template: "raw_bytes", name: "Rb" }],
    wasm: [{ template: "opaque_wrapper", name: "Rb" }],
  },
  "dsl.copy": {
    rust: [{ template: "raw_bytes", name: "Hash", copy: true }],
    wasm: [{ template: "opaque_wrapper", name: "Hash" }],
  },
  "dsl.raw_bytes_flavor": {
    rust: [{ template: "generic_extern", name: "ExtSet" }],
    wasm: [{ template: "generic_extern", name: "ExtSet" }],
  },
  "dsl.custom_serialize": {
    rust: [{ template: "custom_bytes_codec", preserveTemplate: "custom_bytes_codec_preserve", ser: "my_ser", deser: "my_deser", reexport: ["my_ser", "my_deser"] }],
    wasm: [],
  },
  "dsl.custom_deserialize": {
    rust: [{ template: "custom_bytes_codec", preserveTemplate: "custom_bytes_codec_preserve", ser: "my_ser", deser: "my_deser", reexport: ["my_ser", "my_deser"] }],
    wasm: [],
  },
  // `@custom_encodings sz,str` is INERT without --preserve-encodings, so only the preserve flavor
  // differs from the pair above — and a declaration/call/codec disagreement is exactly a compile error.
  "dsl.custom_encodings": {
    rust: [{ template: "custom_bytes_codec", preserveTemplate: "custom_bytes_codec_declared_preserve", ser: "my_ser", deser: "my_deser", reexport: ["my_ser", "my_deser"] }],
    wasm: [],
  },
  "dsl.custom_wire_major": {
    rust: [
      { template: "raw_bytes", name: "Rb" },
      { template: "custom_text_over_raw_bytes", preserveTemplate: "custom_text_over_raw_bytes_preserve", name: "Rb", ser: "my_ser", deser: "my_deser", reexport: ["my_ser", "my_deser"] },
    ],
    wasm: [{ template: "opaque_wrapper", name: "Rb" }],
  },
};

// A row is un-mintable in the D3 decode catalog whenever its wire is not the one the reference oracle
// would produce — which is true of BOTH families above, for the same reason and by different routes:
// an extern/raw-bytes marker is a typename ruby rejects outright, and a custom codec writes a wire the
// ruby-generated vectors for the REPLACED type do not describe. `mintRow` pins from the union, so
// seeding a def (which makes the crate compile) never silently promotes a row into minting vectors
// that would assert the wrong bytes.
const decodeMintPinReason = (id: string): string | undefined =>
  Object.hasOwn(COMPILE_GATE_EXEMPT, id)
    ? `references user-supplied code; crate cannot compile standalone (${COMPILE_GATE_EXEMPT[id]})`
    : Object.hasOwn(DEF_SPLICE, id)
      ? "references user-supplied code, whose wire the reference oracle does not describe (the crate itself is compile-gated via the def splice)"
      : undefined;

const BASE_DEF_DERIVES = ["Clone", "Debug", "PartialEq", "Eq", "PartialOrd", "Ord", "Hash"];
const JSON_DEF_DERIVES = ["serde::Serialize", "serde::Deserialize", "schemars::JsonSchema"];

function renderDef(kind: "rust" | "wasm", d: SpliceDef, json: boolean, preserve: boolean): string {
  const tpl = preserve && d.preserveTemplate ? d.preserveTemplate : d.template;
  const src = readFileSync(`${CODEGEN_DIR}/tests/def_templates/${tpl}.${kind}`, "utf8");
  // The json flags make generated code delegate a user type's JSON representation to that type, so a
  // faithful fixture carries the derives that contract imposes. Rust side only: the wasm wrapper's
  // json fns go through the rust type's serde.
  const derives = [
    ...BASE_DEF_DERIVES,
    ...(d.copy ? ["Copy"] : []),
    ...(json && kind === "rust" ? JSON_DEF_DERIVES : []),
  ];
  return src
    .replaceAll("__NAME__", d.name ?? "")
    .replaceAll("__DERIVES__", derives.join(", "))
    .replaceAll("__SER__", d.ser ?? "")
    .replaceAll("__DESER__", d.deser ?? "");
}

// The row currently being probed, set by `oracles()` alongside `setCodegenInput` and read by every
// generation entry point below (the default probe, the wasm probe, each emission-profile probe and the
// embed fallback's synthetic re-probe), so a splice can never be applied to one stage and skipped in
// another. Non-feature loops (containment, control ops) set it to null by passing an id no row carries.
let activeSplice: { rust: SpliceDef[]; wasm: SpliceDef[] } | null = null;
function applyDefSplice(out: string, opts: { wasm: boolean; json: boolean; preserve: boolean }) {
  if (!activeSplice) return;
  const reexports: string[] = [];
  let rustDefs = "";
  for (const d of activeSplice.rust) {
    rustDefs += "\n\n" + renderDef("rust", d, opts.json, opts.preserve);
    reexports.push(...(d.reexport ?? []));
  }
  if (rustDefs) appendFileSync(join(out, "rust", "src", "lib.rs"), rustDefs);
  if (reexports.length)
    appendFileSync(join(out, "rust", "src", "generated", "mod.rs"), `\npub use crate::{${reexports.join(", ")}};\n`);
  if (!opts.wasm || activeSplice.wasm.length === 0) return;
  // The wasm crate root doesn't see the `wasm_bindgen` macro `generated/mod.rs` privately `use`s.
  let wasmDefs = "\nuse wasm_bindgen::prelude::wasm_bindgen;\n";
  for (const d of activeSplice.wasm) wasmDefs += "\n\n" + renderDef("wasm", d, opts.json, opts.preserve);
  appendFileSync(join(out, "wasm", "src", "lib.rs"), wasmDefs);
}

// --- EMISSION-PROFILE axis (design rationale: see README.md + ROADMAP.md) ----------------------------------------------
// Second, orthogonal axis on the support verdict: besides the DEFAULT-flags verdict (`status`), a
// default-supported row is ALSO probed under each non-default emission profile — the CLI flag sets that
// drive meaningfully different generation paths (preserve-encodings, json-serde/schema). The single
// source of truth is the `ALL_PROFILES` const in src/tests/mod.rs (shared by the Rust snapshot axis and
// the compile gate), extracted by regex with a floor assertion so a profile added to the Rust test axis
// cannot silently escape the matrix probe. Distinct from the CDDL *language*-profile field the matrix
// calls `profile` (RFC8610/RFC9682) — that's a grammar axis; this is a codegen-flag axis.
interface EmissionProfile { name: string; flags: string[] }
function extractEmissionProfiles(): EmissionProfile[] {
  const modSrc = readFileSync(`${CODEGEN_DIR}/src/tests/mod.rs`, "utf8");
  // Grab the array body between `= &[` and the terminating `];` (the inner flag arrays end with `])`/`],`,
  // never `];`, so the non-greedy match stops only at the const's real end).
  const block = modSrc.match(/const ALL_PROFILES\s*:[^=]*=\s*&\[([\s\S]*?)\];/);
  if (!block) {
    console.error("HARNESS FAILURE: could not locate the ALL_PROFILES const in src/tests/mod.rs — the emission-profile source of truth drifted.");
    process.exit(2);
  }
  const profiles: EmissionProfile[] = [];
  // Each entry is `("name", &[ "flag", ... ])`; the json entry's tuple/flag list spans lines, so match
  // across newlines (`\s*` between `(` and the name; `[^\]]*` over the flag list).
  for (const m of block![1].matchAll(/\(\s*"([a-z_]+)"\s*,\s*&\[([^\]]*)\]/g)) {
    const flags = [...m[2].matchAll(/"([^"]+)"/g)].map(f => f[1]);
    profiles.push({ name: m[1], flags });
  }
  const nonDefault = profiles.filter(p => p.name !== "default");
  // Floor assertion (mirrors the dslDirectives/dslMarkers vacuity guard): a refactor that changes the
  // const's extractable shape must fail loud, not quietly probe zero emission profiles.
  if (!profiles.some(p => p.name === "default") || nonDefault.length < 2) {
    console.error(`HARNESS FAILURE: ALL_PROFILES extraction implausible (default present=${profiles.some(p => p.name === "default")}, non-default profiles=${nonDefault.length}); expected 'default' + >=2 emission profiles. src/tests/mod.rs no longer matches the extraction pattern.`);
    process.exit(2);
  }
  return nonDefault;
}
const EMISSION_PROFILES = extractEmissionProfiles();

// The emission profiles a run actually PROBES. The extraction above stays the live drift gate on
// `ALL_PROFILES` — a profile added to the Rust axis must still survive the regex and the floor — so
// the filter is applied HERE, at the probe/report sites, and never inside the extraction.
//
// `component` (`--component=true`) is excluded. This axis's probe is a RUST-CRATE round-trip
// (generate -> cargo test the generated rust crate), and the component face cannot change that
// verdict: it mints a separate wasip2 crate and leaves every rust and wasm byte identical (asserted
// per corpus fixture on the Rust snapshot axis). Probing it would add roughly a third to this repo's
// most expensive gate for a verdict already recorded under another profile, and would mint
// `emission.component.*` annotation keys every downstream matrix consumer then has to carry.
const PROBED_EMISSION_PROFILES = EMISSION_PROFILES.filter(p => p.name !== "component");

// --smoke=N (dev tooling): probe only the first N features, skip the containment/control-op loops and
// all harness-health guards, TOML-parse-validate the composed annotation content, PRINT it, and write
// NOTHING (neither annotations nor verify_report.json). Lets new probe code run end-to-end quickly
// so the full run isn't its first execution (measured full-run wall time: ~11 min warm-cache on the
// dev machine with wasm + decode-foreign on; hours cold — the shared-target warm-up dominates).
// The `runOracleFingerprint` oracle-IDENTITY check is NOT one of the skipped harness-health guards: it
// still runs on a --smoke run (a smoke run against the wrong oracle would mislead the developer just as
// much as a full run), the same as the `existsSync(RUST_CDDL)` oracle-resolution check.
const smokeArg = process.argv.find(a => a.startsWith("--smoke="));
const SMOKE_N = smokeArg ? parseInt(smokeArg.slice("--smoke=".length), 10) : 0;
const SMOKE = SMOKE_N > 0;

// --mint-decode-foreign (D3, decode-conformance harness): (re)generate the committed decode catalog
// (tests/decode_conformance/catalog.toml) from the matrix's supported rows and EXIT — writes ONLY the
// catalog, never annotations/verify_report.json, and skips the reconcile/probe loops (it inserts itself
// right after the shared-target warm-up and process.exit()s before the normal probe pipeline). See
// runMintDecodeForeign. `--only=id,id,…` re-mints just that subset, preserving every other committed row
// verbatim (parsed back through the same deterministic writer); a named id that has left the supported
// set but still has a committed row is DROPPED (support-boundary removal), not re-minted.
const MINT_DECODE = process.argv.includes("--mint-decode-foreign");
// --mint-decode-corpus (composition-depth decode leg): (re)generate the committed corpus decode catalog
// (tests/decode_conformance/corpus_catalog.toml) from tests/corpus/*.cddl × the shared rule enumerator
// and EXIT — writes ONLY that catalog, never annotations/verify_report.json/the matrix catalog. Same
// structure as runMintDecodeForeign (negative control, two-oracle validation, replay, arm-floor,
// triage/pin posture). `--only=` accepts corpus row ids AND bare fixture stems (expanding to the
// fixture's rows). See runMintDecodeCorpus.
const MINT_DECODE_CORPUS = process.argv.includes("--mint-decode-corpus");
const onlyArg = process.argv.find(a => a.startsWith("--only="));
const MINT_ONLY = onlyArg
  ? new Set(onlyArg.slice("--only=".length).split(",").map(s => s.trim()).filter(s => s.length))
  : null;
const MINT_ACCEPT_REJECTED = "MINT_ACCEPT_REJECTED";

// ASSERT-AT-STARTUP self-tests for the three pure evidence-vocabulary deciders. They run on EVERY
// invocation before any oracle work, because all three fail SILENTLY in production — a wrong verdict
// token, stage name, or policy classification reads as a plausible annotation or hidden triage
// exemption, not as an error. `--selftest` runs ONLY these blocks and exits, for a sub-second
// red/green check without the multi-minute pipeline.
const SELFTEST = process.argv.includes("--selftest");
// (1) The ruby-generate Bernoulli classifier (Change A's deterministic verdict source): a
// mis-classification would silently route a Bernoulli row back onto the flaky `generate` verdict (or a
// deterministic row onto the nondet token), surfacing only as a spurious annotations flip.
{
  const cases: [string, boolean][] = [
    ["x = uint .and (0..9)", true],   // .and narrows the value space -> generate is Bernoulli
    ["x = int .ne 0", true],          // .ne comparison op
    ["t = tstr .size 4", true],       // .size length narrowing
    ["x = bytes .cbor uint", false],  // .cbor is a payload wrapper -> generate is deterministic
    ["x = uint .default 5", false],   // .default is an annotation, not a validity constraint
  ];
  const failures = cases.filter(([ex, want]) => rubyGenerateIsBernoulli(ex) !== want);
  if (failures.length) {
    console.error(
      "HARNESS FAILURE: ruby-generate Bernoulli classifier self-test failed for " +
      failures.map(([ex, want]) => `${JSON.stringify(ex)} (expected ${want})`).join(", ") +
      " — the deterministic ruby-verdict routing is unsafe; refusing to run.",
    );
    process.exit(2);
  }
  if (SELFTEST) console.log(`ruby-generate Bernoulli classifier self-test OK (${cases.length} fixtures)`);
}
// (2) The wasm-oracle evidence composer's STAGE taxonomy: each probe-record shape `wasmProbe()` can
// emit must render a clause naming the stage that produced it. The failure this pins is a stage name
// that is merely wrong rather than absent — a generation-time refusal once rendered as `wasm crate
// failed to compile (cargo test exit 1)`, describing a `cargo test` that never ran, and it nearly
// carried a wasm-support review to the wrong conclusion. Hence the extra assertion that no
// generation-stage clause contains the words `cargo test` at all.
{
  type WasmFields = { minted_wasm?: boolean; wasm_gen?: number; wasm_roundtrips?: number };
  const cases: [WasmFields, boolean, string][] = [
    [{}, false, ""],                                                                       // opted out / rust gen failed
    [{ minted_wasm: false, wasm_gen: 1 }, false, "; wasm generation REFUSED (generator exit 1)"],
    [{ minted_wasm: false, wasm_gen: 101 }, false, "; wasm generation PANICKED (generator exit 101)"],
    [{ minted_wasm: true, wasm_gen: 0, wasm_roundtrips: 0 }, false, "; wasm round-trips"],
    [{ minted_wasm: false, wasm_gen: 0, wasm_roundtrips: 0 }, false, "; wasm compiles (no minted wasm surface)"],
    [{ minted_wasm: true, wasm_gen: 0, wasm_roundtrips: 101 }, false, "; wasm round-trip FAILED (cargo test exit 101)"],
    [{ minted_wasm: false, wasm_gen: 0, wasm_roundtrips: 1 }, false, "; wasm crate failed to compile (cargo test exit 1)"],
    [{ minted_wasm: false, wasm_gen: 1 }, true, "; wasm standalone-test N/A (user-supplied code)"],
  ];
  const failures = cases
    .map(([fields, exempt, want]) => ({ fields, exempt, want, got: wasmEvidence(fields, exempt) }))
    .filter(f => f.got !== f.want);
  const leaked = cases
    .filter(([fields]) => (fields.wasm_gen ?? 0) !== 0)
    .map(([fields]) => wasmEvidence(fields))
    .filter(clause => clause.includes("cargo test"));
  if (failures.length || leaked.length) {
    console.error(
      "HARNESS FAILURE: wasm-evidence stage-taxonomy self-test failed — " +
      failures.map(f => `${JSON.stringify(f.fields)}${f.exempt ? " (exempt)" : ""}: expected ${JSON.stringify(f.want)}, got ${JSON.stringify(f.got)}`)
        .concat(leaked.map(c => `generation-stage clause names \`cargo test\`: ${JSON.stringify(c)}`))
        .join("; ") +
      " — the annotations would attribute failures to the wrong stage; refusing to run.",
    );
    process.exit(2);
  }
  if (SELFTEST) console.log(`wasm-evidence stage-taxonomy self-test OK (${cases.length} fixtures)`);
}
// (3) A random spec-valid candidate may be rejected by the documented duplicate-key policy. It is
// redundant only when its OWN marked Display matches a validated hand policy pin; an unknown reason
// must remain class-less red triage. This must share `--selftest`, not wait for a random mint draw.
policyMintClassifierSelfTest();
if (SELFTEST) console.log("policy mint classifier self-test OK (delimiter + matched/unmatched reason)");
if (SELFTEST) process.exit(0);
// K ruby-generated candidate instances per row (deduped byte-identically before two-oracle validation).
const FOREIGN_K = 10;
// The committed catalog the mint writes and the D4 corroborating oracle reads.
const CATALOG_PATH = resolve(CODEGEN_DIR, "tests", "decode_conformance", "catalog.toml");
// The committed CORPUS catalog (composition-depth leg), sibling of catalog.toml.
const CORPUS_CATALOG_PATH = resolve(CODEGEN_DIR, "tests", "decode_conformance", "corpus_catalog.toml");
const CORPUS_DIR = resolve(CODEGEN_DIR, "tests", "corpus");
// The synthetic holder wrapping a rule with no standalone decode surface (transparent alias / named
// table / c-enum): the field decode routes through cddl-codegen's GENERATED code, not cbor_event's
// blanket impl, so the holder is what actually exercises the decoder for those shapes. Prepended FIRST
// so both oracles root validation at it (rust prints "Root type for validation: <first rule>").
const FOREIGN_HOLDER_RULE = "__probe_holder";

interface Derived {
  valid_a: boolean; valid_b: boolean; spec_valid: boolean; parser_limitation: boolean;
  support: string; support_detail: string; out_of_profile: boolean; status: string;
}
// Per-row outcome under ONE non-default emission profile (rust-only: no ruby/rust/wasm re-run). Same
// generate -> cargo test -> (on fail) cargo check -> embed-fallback pipeline as the default probe, with
// the profile's flags appended. `detail` is embed-upgraded (embedDetail), same as the default axis.
interface EmissionOutcome { status: string; detail: string; gen: number; compile: number; test: number; minted: boolean; embedded?: boolean }
interface ProbeResult extends Derived {
  id: string; production: string | null; profile: string; example: string;
  ruby: number; rust: number; codegen: number; compile: number; test: number; minted: boolean;
  // Deterministic ruby verdict token for the `ruby=` evidence clause (rubyClause; Change A). `ruby` above
  // stays the raw generate exit for the diagnostic report; the annotation clause reads this.
  ruby_clause: string;
  minted_wasm?: boolean; wasm_gen?: number; wasm_roundtrips?: number;
  // Decode-foreign oracle (D4): whether the generated decoder accepted the committed spec-derived
  // vectors, and how many accept vectors were replayed. Undefined when opted out (byte-identical output).
  accepts_foreign?: boolean; foreign_vectors?: number;
  // Embed-fallback outcome (feature/control-op loops only, when the base probe minted nothing):
  // true = round-trips inside a synthetic record holder; false = holder minted but round-trip failed;
  // undefined = not applicable (base already minted) or the synthetic was un-embeddable/failed to generate.
  embedded?: boolean;
  // Per-emission-profile verdicts, populated iff the default verdict is supported (undefined otherwise).
  emission?: Record<string, EmissionOutcome>;
}
interface ContainmentCorr {
  id: string; spec_declared: string | null; spec_observed: string; ruby: number; rust: number;
  ruby_clause: string;  // deterministic ruby verdict token for the `ruby=` clause (rubyClause; Change A)
  parser_limitation: boolean; contradiction: boolean; example: string;
  codegen: number; compile: number; test: number; minted: boolean;
  minted_wasm?: boolean; wasm_gen?: number; wasm_roundtrips?: number;
  accepts_foreign?: boolean; foreign_vectors?: number;  // decode-foreign oracle (D4), iff supported
  support: string | null;  // per-cell cddl-codegen support (the role × feature axis)
  emission?: Record<string, EmissionOutcome>;  // per-emission-profile verdicts (iff support === "supported")
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
// diag2cbor.rb (cbor-diag gem) ships in the same binstub dir as the ruby `cddl` gem. The mint pipes
// ruby's diagnostic-notation `generate` output through it to raw CBOR. Only the mint requires it, so
// its absence is a hard failure THERE (checked in runMintDecodeForeign), not for a normal verify run.
const DIAG2CBOR = RUBY_CDDL ? join(dirname(RUBY_CDDL), "diag2cbor.rb") : null;

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
// Flavor words: arguments some directives accept AFTER the tag (e.g. `@used_as_key hash`/`ord`,
// `@duplicates preserve`/`reject`), parsed as literal match arms in comment_ast.rs. Two arm shapes
// exist: the DemandSet flag form (`"hash" => demand.hash = true`) and the enum-value form
// (`"preserve" => DuplicatesPolicy::Preserve`). These aren't `tag("@…")` directives, so a sibling
// FEATURE row whose alt is `<directive> <flavor…>` (mode-narrowed derive families, argument-required
// policies) resolves to the vendor source only once these words are recognized too. The vocabulary is
// deliberately ONE set across directives (the lint's job is anti-fabrication, not per-directive
// argument grammar — that exactness lives in comment_ast.rs itself and corpus_detect.ts's mirror).
const dslFlavors = new Set([
  ...[...dslSource.matchAll(/"([a-z]+)"\s*=>\s*demand\.\w+\s*=\s*true/g)].map(m => m[1]),
  ...[...dslSource.matchAll(/"([a-z]+)"\s*=>\s*DuplicatesPolicy::/g)].map(m => m[1]),
]);
// Floor assertion: if a refactor of comment_ast.rs/parsing.rs changes the extractable shape, both lints
// built on these sets would go vacuous (forward) or flag everything (backward) — fail loud instead.
if (dslDirectives.length === 0 || dslMarkers.length === 0) {
  console.error(`HARNESS FAILURE: vendor-source extraction went vacuous (directives=${dslDirectives.length}, markers=${dslMarkers.length}); comment_ast.rs/parsing.rs no longer match the extraction patterns.`);
  process.exit(2);
}
const dslTokens = new Set([...dslDirectives, ...dslMarkers]);
// If any vendor row carries a flavored (multi-token) alt but the flavor vocabulary went vacuous, the
// match-arm extraction pattern broke — every such row would false-flag as fabricated; fail loud instead.
if (features.some(f => f.profile === "CDDL_CODEGEN" && f.alt && /\s/.test(f.alt)) && dslFlavors.size === 0) {
  console.error("HARNESS FAILURE: flavored vendor alt present but flavor-word extraction went vacuous; comment_ast.rs no longer matches the flavor match-arm pattern.");
  process.exit(2);
}
const fabricated: { id: string; production: string | null }[] = [];
for (const f of features) {
  const prod = f.production;
  if (prod === PRELUDE_PSEUDO) continue;
  if (prod && CDDL_CODEGEN_PSEUDO.has(prod)) {
    // Resolves to the pinned vendor source when the alt is a bare extracted directive/marker, OR a base
    // directive followed only by recognized flavor words (the `@used_as_key hash`/`ord` argument surface).
    if (f.alt) {
      const [base, ...flavorWords] = f.alt.split(/\s+/);
      if (dslTokens.has(f.alt) ||
          (dslTokens.has(base) && flavorWords.length > 0 && flavorWords.every(w => dslFlavors.has(w))))
        continue;
    }
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
// A directive is modelled by a row whose alt IS the directive (bare form) OR by flavored sibling
// rows whose alt is `<directive> <arg>` — the only possible modelling for an ARGUMENT-REQUIRED
// directive like `@duplicates`, whose bare form is a parse-time panic and so can have no bare row.
const modelled = (d: string) =>
  cddlCodegenAlts.has(d) || [...cddlCodegenAlts].some(a => a !== undefined && a.startsWith(`${d} `));
for (const d of [...new Set([...dslDirectives, ...dslMarkers])].sort())
  if (docsText.includes(d) && !modelled(d)) cddl_codegen_gaps.push({ kind: "missing_cddl_codegen_feature", name: d });

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
const altCoverageResult = grammarAltCoverage(features, abnfText);
const alt_coverage = altCoverageResult.coverage;
const type2_uncovered: string[] = alt_coverage["type2"].uncovered;
const alt_uncovered = ALT_PRODUCTIONS.flatMap(prod => alt_coverage[prod].uncovered.map(alt => ({ production: prod, alt })));
const alt_accounted = ALT_PRODUCTIONS.reduce((n, prod) => {
  const cov = alt_coverage[prod];
  return n + cov.covered.length + cov.delegated.length + cov.modeled_under.length;
}, 0);
const alt_alternatives = ALT_PRODUCTIONS.reduce((n, prod) => n + alt_coverage[prod].abnf_alternatives.length, 0);
if (altCoverageResult.problems.some(p => p.includes("extraction yielded"))) {
  console.error(`HARNESS FAILURE: ABNF alternative extraction truncated:`);
  for (const p of altCoverageResult.problems.filter(p => p.includes("extraction yielded"))) console.error(`  - ${p}`);
  process.exit(2);
}

// --- ORACLE-IDENTITY FINGERPRINT (mechanical rust-oracle pin) -------------------------------------
// `RUST_CDDL` defaults to a LIVE development tree's `target/debug/cddl` (README § "Upstream oracle
// gaps"). Every local branch reports version 0.10.6, so a version string cannot tell the pinned
// `local-fixes` @ ac1b98e build apart from a wrong-branch rebuild — and evidence minted against the
// wrong oracle looks EXACTLY like evidence minted against the pinned one. This behavioral fingerprint
// is the guard the version string can't be: a handful of pinned probe inputs from
// oracle_fingerprint.json whose accept/reject exits are UNIQUE to the local-fixes fixes. The same JSON
// is consumed by `integration_tests::rust_oracle_fingerprint` for the generated-crate
// `CDDL_ORACLE_DEP` crate preflight. Discriminating power (why each probe is here):
//   • WIP branch `non-uint-ranges` (0.10.6 + the non-uint-range fix only, NOT ac1b98e/773b723) fails 1–2;
//   • released/crates.io 0.10.x fails 1–3;
//   • the PAST pin 2c7548e (everything through the radix fix) fails 9–10;
//   • the PAST pin 4e39d09 (everything through the bignum fix) fails 11–12;
//   • the PAST pin 765fd81 (everything through the optional-entry/closed-map fix) fails 5 and 7–8;
//   • an always-accept stub fails every reject-expecting probe (4, 6, 8, 10, 12).
// It is an oracle-IDENTITY check (same category as the `existsSync(RUST_CDDL)` HARNESS FAILURE above),
// so it runs UNCONDITIONALLY at startup on every path (normal probe, --mint-decode-foreign, --smoke),
// before the multi-minute shared-target warm-up — a wrong oracle fails in under a second. Exits ONLY
// zero/nonzero (no exact nonzero code pinned). No env-var escape hatch: evidence minted against a
// non-pinned oracle must not be writable at all.
interface FingerprintProbe {
  name: string; spec: string; mode: "compile" | "validate"; cborHex?: string;
  expectZeroExit: boolean; why: string;
}

function loadOracleFingerprint(): FingerprintProbe[] {
  const path = join(ROOT, "oracle_fingerprint.json");
  let raw: unknown;
  try {
    raw = JSON.parse(readFileSync(path, "utf8"));
  } catch (e) {
    console.error(`HARNESS FAILURE: rust oracle fingerprint probe set could not be read from '${path}': ${e}`);
    process.exit(2);
  }
  const fail = (msg: string): never => {
    console.error(`HARNESS FAILURE: rust oracle fingerprint probe set invalid (${path}): ${msg}`);
    process.exit(2);
  };
  if (!raw || typeof raw !== "object" || !Array.isArray((raw as { probes?: unknown }).probes)) {
    fail("top-level object must contain a probes array");
  }
  const loaded: FingerprintProbe[] = [];
  for (const [i, p] of (raw as { probes: unknown[] }).probes.entries()) {
    if (!p || typeof p !== "object") fail(`probe[${i}] must be an object`);
    const probe = p as Record<string, unknown>;
    const name = probe.name;
    const spec = probe.spec;
    const mode = probe.mode;
    const cborHex = probe.cborHex;
    const expectOk = probe.expectOk;
    const why = probe.why;
    if (typeof name !== "string" || name.length === 0) fail(`probe[${i}] has missing/invalid name`);
    if (typeof spec !== "string") fail(`probe '${name}' has missing/invalid spec`);
    if (mode !== "compile" && mode !== "validate") fail(`probe '${name}' has unknown mode ${JSON.stringify(mode)}`);
    if (mode === "validate" && typeof cborHex !== "string") fail(`probe '${name}' is validate mode but missing cborHex`);
    if (cborHex !== undefined && typeof cborHex !== "string") fail(`probe '${name}' has non-string cborHex`);
    if (typeof expectOk !== "boolean") fail(`probe '${name}' has missing/invalid expectOk`);
    if (typeof why !== "string") fail(`probe '${name}' has missing/invalid why`);
    const parsedMode = mode as "compile" | "validate";
    loaded.push({
      name: name as string,
      spec: spec as string,
      mode: parsedMode,
      cborHex: cborHex as string | undefined,
      expectZeroExit: expectOk as boolean,
      why: why as string,
    });
  }
  if (loaded.length < 5) fail(`probe count ${loaded.length} is below anti-vacuity floor 5`);
  return loaded;
}

const ORACLE_FINGERPRINT: FingerprintProbe[] = loadOracleFingerprint();
// Hoisted so it can be called at startup before its lexical position and on every entry path. Writes its
// own fp.cddl/fp.cbor under probeDir (created by the caller just before invocation). `exit > 0` (not
// `!== 0`) satisfies a nonzero expectation so a timeout/signal kill (negative exit) reads as a MISMATCH,
// the safe direction — a wrong oracle can't slip through on a transient hiccup.
function runOracleFingerprint(dir: string): void {
  const fpCddl = join(dir, "fp.cddl");
  const fpCbor = join(dir, "fp.cbor");
  const failures: string[] = [];
  for (const p of ORACLE_FINGERPRINT) {
    writeFileSync(fpCddl, p.spec.replace(/\n*$/, "\n"));
    let exit: number;
    if (p.mode === "compile") {
      exit = runExit([RUST_CDDL, "compile-cddl", "--cddl", fpCddl]);
    } else {
      writeFileSync(fpCbor, Buffer.from(p.cborHex!, "hex"));
      exit = runExit([RUST_CDDL, "--ci", "validate", "--cddl", fpCddl, "--cbor", fpCbor]);
    }
    const ok = p.expectZeroExit ? exit === 0 : exit > 0;
    if (!ok) {
      failures.push(
        `  - probe '${p.name}': spec ${JSON.stringify(p.spec)}${p.cborHex ? ` cbor 0x${p.cborHex}` : ""}; ` +
        `expected ${p.expectZeroExit ? "ZERO" : "NONZERO"} exit, got ${exit}. ${p.why}`,
      );
    }
  }
  if (failures.length) {
    console.error(`HARNESS FAILURE: rust oracle fingerprint MISMATCH — RUST_CDDL='${RUST_CDDL}' does not behave like the pinned oracle. Failing probe(s):`);
    for (const f of failures) console.error(f);
    console.error("The pinned oracle is the fork's `local-fixes` branch @ ac1b98e in the sibling repo ~/Documents/git/cddl (README § \"Upstream oracle gaps\"). Recover by rebuilding from THAT branch (restore any WIP checkout afterwards) or by pointing RUST_CDDL at an immutable copy of the pinned build. A stock `cargo install cddl` binary is NOT accepted — its released 0.10.x gaps fail this fingerprint by design.");
    process.exit(2);
  }
  console.log(`rust oracle fingerprint OK (${ORACLE_FINGERPRINT.length} probes — local-fixes @ ac1b98e behavior)`);
}

// Disk-headroom preflight (Change B): every probe/mint path generates 100s of throwaway crates into
// tmpdir-backed scratch; a near-full scratch volume degrades into the WIDE-EVIDENCE-FLIP signature (a
// batch of identical bogus verdicts — many rows flipping to the same generic cargo-failure line, none
// reproducing solo) rather than one loud error (the ENOSPC triage lesson, cddl-matrix/README.md
// § "Gotchas"). Hard-fail upfront when headroom is under the 2 GiB floor, naming the stale-scratch
// cleanup for both scratch prefixes. `df -k` is GNU coreutils; if unavailable the check degrades to a
// loud warning rather than blocking the run. This is the oracle-fingerprint's designated sibling — one
// implementation, called once at startup so it covers the normal evidence-writing run,
// --mint-decode-foreign, and --mint-decode-corpus alike.
function diskHeadroomPreflight(context: string): void {
  const FLOOR_KIB = 2 * 1024 * 1024; // 2 GiB in KiB
  const r = Bun.spawnSync(["df", "-k", "--output=avail", tmpdir()], { stdout: "pipe", stderr: "ignore" });
  const availKiB = parseInt((r.stdout?.toString() ?? "").trim().split(/\r?\n/).pop() ?? "", 10);
  if (r.exitCode !== 0 || !Number.isFinite(availKiB)) {
    console.warn(`[${context}] WARNING: could not measure free space on '${tmpdir()}' (df exit ${r.exitCode}) — proceeding without the ENOSPC preflight.`);
    return;
  }
  if (availKiB < FLOOR_KIB) {
    console.error(
      `HARNESS FAILURE: only ${(availKiB / 1024 / 1024).toFixed(1)} GiB free on the scratch volume ('${tmpdir()}'; floor 2 GiB). ` +
      `A near-full scratch degrades into runs of identical bogus verdicts (the ENOSPC wide-evidence-flip lesson, ` +
      `cddl-matrix/README.md § "Gotchas") instead of failing loudly. Clear stale scratch first — e.g. ` +
      `\`rm -rf ${tmpdir()}/cddl_codegen_* ${tmpdir()}/cddl_verify_*\` — then re-run.`,
    );
    process.exit(2);
  }
}

// ==================================================================================================
// 3. PROBE each feature's example through the three oracles.
// ==================================================================================================
const probeDir = mkdtempSync(join(tmpdir(), "cddl_verify_"));
scratchProbeDir = probeDir;
const probeFile = join(probeDir, "probe.cddl");
// Oracle-identity fingerprint runs FIRST (before the shared-target warm-up), on every path — a wrong
// RUST_CDDL fails in under a second instead of minting mixed-oracle evidence minutes in.
runOracleFingerprint(probeDir);
// Its designated sibling: fail fast on a near-full scratch volume before any generation (Change B).
diskHeadroomPreflight("scratch-preflight");
// Every generation gets a FRESH output dir (a monotonic counter suffix; the previous dir is deleted
// so disk use stays keep-last-1). Reusing ONE path for every cell under the shared CARGO_TARGET_DIR
// let cargo's mtime-based fingerprint declare freshly generated sources "fresh" against the PREVIOUS
// cell's build at the same path and reuse its artifacts — `cargo test` then exits 0 without compiling
// the new bytes. That stale-reuse race predates the cache (a transient wrong verdict per run), but the
// cache PERSISTED it: a deterministically-failing exempt cell (dsl.custom_serialize) got a poisoned
// PASS entry, caught live by `verify_cache_transparency`'s A/B diff. Unique per-generation paths make
// cargo fingerprint the leaf crate per cell (deps stay amortized in the shared target — they are
// path-independent), which is the Rust-side gates' per-cell-dir design. The gate-cache tree hash uses
// RELATIVE paths and the key argv is path-normalized, so keys are unchanged by the moving dirs.
let outSeq = 0;
let ccOut = join(probeDir, `cc_out_${outSeq++}`);
const ccWarmOut = join(probeDir, "cc_warm_out");
// Shared cargo target for the compile-gate so the generated crate's deps (cbor_event, …) build ONCE and
// every subsequent `cargo check` is incremental (fits PROBE_TIMEOUT). Warmed before the probe loops.
const COMPILE_TARGET = mkdtempSync(join(tmpdir(), "cddl_verify_target_"));
scratchTargets.push(COMPILE_TARGET);
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
let ccOutWasm = join(probeDir, `cc_out_wasm_${outSeq++}`);
const ccWarmOutWasm = join(probeDir, "cc_warm_out_wasm");
const WASM_TARGET = WASM_PROBE ? mkdtempSync(join(tmpdir(), "cddl_verify_wasm_target_")) : "";
if (WASM_TARGET) scratchTargets.push(WASM_TARGET);

// DEFAULT-ON decode-foreign oracle (D4; opt out with `--no-decode-foreign` argv or
// VERIFY_DECODE_FOREIGN=0 env): for each SUPPORTED probe row whose committed catalog entry still matches
// the matrix example, regenerate from the catalog `spec` (own out dir, shared COMPILE_TARGET — deps are
// already warm), append the same `__foreign_decode_replay` module, and `cargo test` it, feeding
// spec-derived CBOR our code did NOT produce into the generated decoder. CORROBORATES ONLY — it never
// changes a support verdict; a failure is recorded as evidence + rolled into decode_foreign_failures.
// Opted out -> the per-probe fields stay undefined, so the report/annotation output is byte-identical to
// a pre-feature run (the wasm-oracle opt-out pattern).
const DECODE_FOREIGN =
  !process.argv.includes("--no-decode-foreign") &&
  !["0", "false"].includes((process.env.VERIFY_DECODE_FOREIGN ?? "").toLowerCase());
let ccOutForeign = join(probeDir, `cc_out_foreign_${outSeq++}`);
// The committed vectors keyed by matrix row id (empty in the mint path / when opted out / before the
// catalog is first committed). Loaded once; a missing file is not an error here (D6 gates completeness).
const catalogRows: Map<string, CatalogRow> =
  DECODE_FOREIGN && !MINT_DECODE && existsSync(CATALOG_PATH) ? parseCatalog(CATALOG_PATH) : new Map();

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

const GATE_CACHE_ENABLED = gateCacheEnabled();
const gateCacheStats = { run: 0, cached: 0 };

type WarmNeed = { kind: "rust" } | { kind: "wasm" } | { kind: "emission"; profile: EmissionProfile };

function runGenerateLockfile(manifestPath: string): number {
  return runExit(["cargo", "generate-lockfile", "--manifest-path", manifestPath], CODEGEN_DIR);
}

function cacheableCargoPass(
  gate: string,
  cell: string,
  argv: string[],
  manifestPath: string,
  treeRoot: string,
  warmNeed: WarmNeed,
  env: Record<string, string>,
  timeoutS: number,
): number {
  let key: string | null = null;
  let entryBase: Omit<GateCacheEntry, "cell" | "created"> | null = null;
  if (GATE_CACHE_ENABLED && runGenerateLockfile(manifestPath) === 0) {
    const tree = hashTree(treeRoot);
    // Key on a PATH-NORMALIZED argv: the literal command line embeds `treeRoot`, which lives under
    // this run's mkdtemp probeDir — a random path per run, so keying the raw argv would make every
    // key unique to its run (zero cross-run hits, the whole point of the cache). The tree hash
    // already pins the crate bytes; the argv's role in the key is the command SHAPE (subcommand +
    // which crate within the tree), which survives the placeholder. Mirrors the Rust gates'
    // `cwd=<subdir>`-style normalized argv_for_key.
    const argvForKey = argv.map(a => a.split(treeRoot).join("<tree>"));
    const keyParts = gateCacheKey({ gate, argv: argvForKey, tree });
    key = keyParts.key;
    entryBase = { schema: GATE_CACHE_SCHEMA, gate, argv: argvForKey, rustc: keyParts.rustc, tree };
    if (readGateCacheEntry(key, CODEGEN_DIR)) {
      gateCacheStats.cached++;
      console.log(`[gate-cache] ${cell}: cached PASS (key ${key.slice(0, 8)})`);
      return 0;
    }
  }

  ensureWarm(warmNeed);
  touchTree(treeRoot);
  gateCacheStats.run++;
  const exit = runProbe(argv, CODEGEN_DIR, env, timeoutS);
  if (exit === 0 && key && entryBase)
    writeGateCacheEntry(key, { ...entryBase, cell, created: new Date().toISOString() }, CODEGEN_DIR);
  return exit;
}

// Cargo's leaf fingerprint in the shared COMPILE_TARGET is keyed by package name+version, NOT by
// manifest path: when a PASSING `cddl-lib` is built into the target AFTER this cell's sources were
// written — exactly what the LAZY warm-up does on a cache miss (it runs between the cell's
// generation and its `cargo test`) — cargo sees "sources older than the last same-name build",
// declares the cell FRESH, and reuses the other crate's artifacts: `cargo test` exits 0 without
// compiling the cell's bytes (proven by direct experiment: a deterministically-failing crate tests
// green when a same-name passing crate is built after its generation). The eager-warm GATE_CACHE=0
// path is immune (warm-ups predate every cell's generation), which is how verify_cache_transparency
// caught the poisoned PASS this defends against. Touching every tree file right before the nested
// cargo makes the sources NEWER than any same-name fingerprint, forcing the honest rebuild; mtimes
// are not key material, so the content-hash key is unaffected.
function touchTree(root: string): void {
  const now = new Date();
  const walk = (dir: string) => {
    for (const ent of readdirSync(dir, { withFileTypes: true })) {
      const p = join(dir, ent.name);
      if (ent.isDirectory()) {
        if (ent.name !== "target") walk(p);
      } else {
        try { utimesSync(p, now, now); } catch { /* raced/removed file — rebuild honesty is kept by the rest */ }
      }
    }
  };
  walk(root);
}

// COMPILE (classification only): `cargo check` the generated crate. Run when `cargo test` FAILED, to
// split "generates but does not compile" (e.g. `x = any` -> `pub type X = Any;`, a type defined
// nowhere) from "compiles but the emitted tests fail". Mirrors
// integration_tests::feature_corpus_compiles (rust-only, shared CARGO_TARGET_DIR).
function runCompile(cell: string, warmNeed: WarmNeed = { kind: "rust" }, timeoutS = PROBE_TIMEOUT): number {
  const manifest = join(ccOut, "rust", "Cargo.toml");
  const argv = ["cargo", "check", "--manifest-path", manifest];
  return cacheableCargoPass("verify.rust_check", cell, argv, manifest, ccOut, warmNeed, { CARGO_TARGET_DIR: COMPILE_TARGET }, timeoutS);
}

// EXECUTION-GATE: `cargo test` the generated crate — compiles the lib AND runs the `--emit-tests`
// round-trip/reject module (strictly stronger than `cargo check`). Caller invokes only when
// generation succeeded.
function runTest(cell: string, warmNeed: WarmNeed = { kind: "rust" }, timeoutS = PROBE_TIMEOUT): number {
  const manifest = join(ccOut, "rust", "Cargo.toml");
  const argv = ["cargo", "test", "--manifest-path", manifest];
  return cacheableCargoPass("verify.rust_test", cell, argv, manifest, ccOut, warmNeed, { CARGO_TARGET_DIR: COMPILE_TARGET }, timeoutS);
}

// JSON-GEN stage, run ONLY for a def-spliced row under the json emission profile. The `wasm/json-gen`
// crate is a THIRD emitted crate — a `schemars` registrar over the rust types — and no other stage of
// this harness builds it for any row. Two compile-error classes shipped in exactly that crate
// (a bare generic-extern base's E0107 schema row, a dep-owned row's E0433) while the extern exemption
// kept it un-built, so the extern rows are precisely where the floor is worth its wall time. Kept
// bounded to those rows deliberately: the corpus-side json-gen breadth is
// `integration_tests::feature_corpus_compiles`', and widening it to every matrix row would pay a
// schemars build on rows that carry none of this risk.
function runJsonGenCheck(cell: string, timeoutS = PROBE_TIMEOUT): number {
  const manifest = join(ccOut, "wasm", "json-gen", "Cargo.toml");
  if (!existsSync(manifest)) return 0;
  const argv = ["cargo", "check", "--manifest-path", manifest];
  return cacheableCargoPass("verify.json_gen_check", cell, argv, manifest, ccOut, { kind: "rust" }, { CARGO_TARGET_DIR: COMPILE_TARGET }, timeoutS);
}

// The generator MERGES into an existing output dir (it never clears it), so a partially-written crate
// from a panicking probe — or any future conditionally-emitted module — would leak into the next
// probe's compile gate. Start every generation from an empty dir.
// (Bumping to a fresh path is the stale-fingerprint defense — see the `outSeq` comment above.)
const cleanOut = () => { rmSync(ccOut, { recursive: true, force: true }); ccOut = join(probeDir, `cc_out_${outSeq++}`); };
// Fresh-dir bump for the foreignGenCrate flows (decode-foreign probe + the mint paths): delete the
// current dir, allocate the next, return it — callers pass the return straight to foreignGenCrate and
// keep reading the module binding for the replay that follows.
const nextForeignOut = (): string => { rmSync(ccOutForeign, { recursive: true, force: true }); ccOutForeign = join(probeDir, `cc_out_foreign_${outSeq++}`); return ccOutForeign; };
const nextOut = (): string => { cleanOut(); return ccOut; };
// `extraFlags` appends an emission profile's CLI flags (preserve/json) so the SAME generate pipeline
// serves both the default probe (no extra flags) and the per-emission-profile probes.
// The codegen probes' input path: `probeFile` for ordinary single-file examples, or a synthesized
// directory for features carrying `example_extern_stub` — extern-scope directives (`@rust_name`)
// REJECT on exported rules by design, so their only legal probe home is a stub under
// `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/`, which requires directory input. `oracles()` sets this per
// feature (and the feature loop resets it afterwards); the ruby/rust reference oracles keep
// probing the single-file `probeFile` text (informational for the vendor profile either way).
let codegenInput = probeFile;
const probeMultiDir = () => join(probeDir, "probe_multi");
function setCodegenInput(example: string, externStub?: string) {
  if (externStub === undefined) {
    codegenInput = probeFile;
    return;
  }
  const dir = probeMultiDir();
  rmSync(dir, { recursive: true, force: true });
  mkdirSync(join(dir, "_CDDL_CODEGEN_EXTERN_DEPS_DIR_", "extern_dep"), { recursive: true });
  writeFileSync(join(dir, "lib.cddl"), example + "\n");
  writeFileSync(join(dir, "_CDDL_CODEGEN_EXTERN_DEPS_DIR_", "extern_dep", "lib.cddl"), externStub + "\n");
  codegenInput = dir;
}
function runCodegen(extraFlags: string[] = []): number {
  cleanOut();
  const exit = runProbe(["cargo", "run", "-q", "--", `--input=${codegenInput}`, `--output=${ccOut}`, "--wasm=false", "--emit-tests=true", ...extraFlags], CODEGEN_DIR);
  // Seed the user-supplied side (see DEF_SPLICE) BEFORE any compile stage reads the crate. The flags
  // decide the flavor: the encoding-variable signature under preserve, the serde/schemars derives the
  // json contract imposes on user types.
  if (exit === 0)
    applyDefSplice(ccOut, {
      wasm: false,
      json: extraFlags.some(f => f.startsWith("--json-")),
      preserve: extraFlags.includes("--preserve-encodings=true"),
    });
  return exit;
}

// WASM oracle (default-on): generate the SAME example with `--wasm=true` into a separate out dir, then
// `cargo test` the wasm crate — which builds the rust crate as a (non-test) path dep AND compiles+runs
// the emitted `cddl_generated_wasm_tests` module. Separate out dir so it never perturbs the rust
// probe's ccOut; separate target so its wasm-bindgen deps don't invalidate the rust compile cache.
const cleanOutWasm = () => { rmSync(ccOutWasm, { recursive: true, force: true }); ccOutWasm = join(probeDir, `cc_out_wasm_${outSeq++}`); };
function runCodegenWasm(): number {
  cleanOutWasm();
  const exit = runProbe(["cargo", "run", "-q", "--", `--input=${codegenInput}`, `--output=${ccOutWasm}`, "--wasm=true", "--emit-tests=true"], CODEGEN_DIR);
  if (exit === 0) applyDefSplice(ccOutWasm, { wasm: true, json: false, preserve: false });
  return exit;
}
function runWasmTest(cell: string, timeoutS = PROBE_TIMEOUT): number {
  const manifest = join(ccOutWasm, "wasm", "Cargo.toml");
  const argv = ["cargo", "test", "--manifest-path", manifest];
  return cacheableCargoPass("verify.wasm_test", cell, argv, manifest, ccOutWasm, { kind: "wasm" }, { CARGO_TARGET_DIR: WASM_TARGET }, timeoutS);
}

// The full cddl-codegen probe: generate (with --emit-tests) -> `cargo test`. On the green path a single
// `cargo test` suffices (its success implies the lib compiles, recorded as compile 0); only a test
// FAILURE pays for the extra `cargo check` that classifies it. -2 = not reached (n/a).
// `minted` = the emitted lib actually contains the generated-test module; without it a `cargo test`
// pass is vacuous (transparent aliases / pure c-enums mint nothing, by design — skipped loudly by the
// emitter), so the verdict evidence must not claim "round-trips" for those.
// `minted_wasm` / `wasm_gen` / `wasm_roundtrips` are populated under the WASM_PROBE (default-on;
// undefined when opted out via --no-wasm / VERIFY_WASM=0, so they're then omitted from the JSON report
// and add no wasm evidence to annotations — an opted-out run's output is byte-identical to a
// pre-wasm-probe run). Each names ONE stage of the wasm leg, so a failure can be attributed to the
// stage it happened at: `wasm_gen` is the GENERATOR's exit for the `--wasm=true` re-run;
// `wasm_roundtrips` is the `cargo test` exit of a wasm crate that was actually generated (so it is set
// only when `wasm_gen` is 0); `minted_wasm` = that crate's lib actually contains the generated
// wasm-test module (else a green `cargo test` is vacuous, same caveat as `minted`).
interface CodegenProbe { gen: number; compile: number; test: number; minted: boolean; minted_wasm?: boolean; wasm_gen?: number; wasm_roundtrips?: number }
// The RUST-ONLY core of a codegen probe (generate -> cargo test -> classify), shared by the default
// probe and the per-emission-profile probes. `extraFlags` appends a profile's CLI flags; the emission
// probes deliberately DON'T run the wasm oracle (design doc: wasm stays default-profile corroborating
// evidence, keeping added wall time ~2x rust work on the supported subset, not 4x).
function probeCodegenRust(cell: string, extraFlags: string[] = [], emissionProfile?: EmissionProfile): CodegenProbe {
  const gen = runCodegen(extraFlags);
  if (gen !== 0) return { gen, compile: -2, test: -2, minted: false };
  const libPath = join(ccOut, "rust", "src", "generated", "mod.rs");
  const minted = existsSync(libPath) && readFileSync(libPath, "utf8").includes("mod cddl_generated_tests");
  const warmNeed: WarmNeed = emissionProfile ? { kind: "emission", profile: emissionProfile } : { kind: "rust" };
  // The def-spliced rows carry one extra emitted crate under the json profile. A failure there is a
  // `cargo check` failure, so it is reported through `compile` (and `test`, which never ran) — reusing
  // `codegenVerdict`'s existing "generates but does not compile" wording rather than teaching every
  // downstream annotation consumer a new evidence token. Ordered BEFORE `runTest` so a json-gen break
  // is not masked by a green rust round-trip.
  if (activeSplice && emissionProfile?.flags.some(f => f.startsWith("--json-"))) {
    const jsonGen = runJsonGenCheck(cell);
    if (jsonGen !== 0) return { gen, compile: jsonGen, test: jsonGen, minted };
  }
  const test = runTest(cell, warmNeed);
  const compile = test === 0 ? 0 : runCompile(cell, warmNeed);
  return { gen, compile, test, minted };
}
function probeCodegen(cell: string): CodegenProbe {
  const base = probeCodegenRust(cell);
  // A spec that doesn't generate at all has no wasm crate to test either (the wasm probe re-runs the
  // SAME generator with `--wasm=true`), so skip the doomed wasm generation — the rust verdict already
  // records the parse/panic. wasm fields stay undefined -> no (redundant) wasm evidence clause.
  if (base.gen !== 0) return base;
  return { ...base, ...wasmProbe(cell) };
}

// The wasm half of a probe (default-on): generate `--wasm=true`, cargo test the wasm crate. Returns {}
// when opted out so the fields stay undefined (report/annotation output matches a pre-wasm-probe run).
function wasmProbe(cell: string): { minted_wasm?: boolean; wasm_gen?: number; wasm_roundtrips?: number } {
  if (!WASM_PROBE) return {};
  const gen = runCodegenWasm();
  // A generation failure leaves `wasm_roundtrips` UNSET rather than borrowing it to carry the
  // generator's exit: the two are different stages, and one field holding either made a graceful
  // generation-time refusal read as a compile regression in the annotations.
  if (gen !== 0) return { minted_wasm: false, wasm_gen: gen };
  const wasmLib = join(ccOutWasm, "wasm", "src", "generated", "mod.rs");
  const minted_wasm = existsSync(wasmLib) && readFileSync(wasmLib, "utf8").includes("mod cddl_generated_wasm_tests");
  return { minted_wasm, wasm_gen: gen, wasm_roundtrips: runWasmTest(cell) };
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

// Honest wasm-oracle evidence suffix (default-on). Every clause names the STAGE the observation was
// made at, because a reader acts on the stage: a generation-time refusal is the wasm surface declining
// a shape (often deliberate, sometimes a gap to fill), while a compile or round-trip failure is a
// build/emission break. Naming the wrong stage nearly misled a wasm-support review once, when a
// generation refusal was reported as `wasm crate failed to compile (cargo test exit 1)` — a `cargo
// test` that never ran. The taxonomy, in the order tested:
//   - probe didn't run (opted out / rust gen failed / fields undefined)  -> "" (annotations unchanged)
//   - `exempt`: the feature references user-supplied code, so — exactly like the rust
//     standalone-compile exemption — the wasm crate can't be `cargo test`ed standalone: N/A, not FAILED
//   - generation refused / panicked (`wasm_gen` nonzero)  -> a clause naming GENERATION and the
//     GENERATOR's exit; deliberately free of the substring `cargo test`, which is what misread
//   - generated + `cargo test` 0    -> round-trips, or (nothing minted) compiles-only; same `minted`
//     fallback discipline as rust — a green test over an unminted surface is not a round-trip
//   - generated + `cargo test` nonzero -> worded by whether a module was actually minted (present -> a
//     round-trip assertion failed; absent -> the crate itself failed to compile, mirroring the rust
//     compile verdict). Reachable only for a crate that really was generated.
function wasmEvidence(p: { minted_wasm?: boolean; wasm_gen?: number; wasm_roundtrips?: number }, exempt = false): string {
  const { minted_wasm, wasm_gen, wasm_roundtrips } = p;
  if (wasm_gen === undefined) return "";
  if (exempt) return "; wasm standalone-test N/A (user-supplied code)";
  if (wasm_gen !== 0 && wasm_gen !== 101) return `; wasm generation REFUSED (generator exit ${wasm_gen})`;
  if (wasm_gen === 101) return "; wasm generation PANICKED (generator exit 101)";
  if (wasm_roundtrips === undefined) return "";   // unreachable: wasm_gen 0 always sets it
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
function embedFallback(id: string, example: string, cg: CodegenProbe, emissionProfile?: EmissionProfile): boolean | undefined {
  if (cg.gen !== 0 || cg.minted) return undefined; // not applicable — base already mints (or didn't generate)
  const log = (note: string) => console.log(`  [embed] ${id}: ${note}`);
  const extraFlags = emissionProfile?.flags ?? [];
  const rule = firstRuleName(example);
  if (!rule) { log("no parseable rule head; embed skipped (kept compile verdict)"); return undefined; }
  if (rule.generic) { log(`generic rule '${rule.name}' needs type args; not embeddable (kept compile verdict)`); return undefined; }
  writeFileSync(probeFile, `${example}\n${HOLDER_RULE} = [0, ${rule.name}]\n`);
  const gen = runCodegen(extraFlags);
  if (gen !== 0) { log(`synthetic holder failed to generate (cargo run exit ${gen}); kept compile verdict`); return undefined; }
  const libPath = join(ccOut, "rust", "src", "generated", "mod.rs");
  const minted = existsSync(libPath) && readFileSync(libPath, "utf8").includes("mod cddl_generated_tests");
  if (!minted) { log("synthetic holder minted no test surface; kept compile verdict"); return undefined; }
  const test = runTest(id, emissionProfile ? { kind: "emission", profile: emissionProfile } : { kind: "rust" });
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

// EMISSION-PROFILE probe: re-run the row's example through each PROBED non-default
// emission profile (see PROBED_EMISSION_PROFILES), reusing the exact same rust-only pipeline (generate -> cargo test -> classify ->
// embed-fallback-if-unminted) with the profile's flags appended. Runs ONLY when the row's default
// verdict is supported (caller-enforced), so any non-supported entry here is a genuine profile
// divergence. COMPILE_GATE_EXEMPT rows keep exemption semantics per profile (verdict from the
// generation exit only; standalone-compile N/A). The shared `codegenVerdict` keeps the semantics from
// drifting apart from the default axis.
function probeEmissions(id: string, example: string): Record<string, EmissionOutcome> {
  const out: Record<string, EmissionOutcome> = {};
  const exempt = Object.hasOwn(COMPILE_GATE_EXEMPT, id);
  for (const prof of PROBED_EMISSION_PROFILES) {
    writeFileSync(probeFile, example + "\n");
    const cg = probeCodegenRust(`${id} (emission=${prof.name})`, prof.flags, prof);
    if (exempt && cg.gen === 0) {
      out[prof.name] = {
        status: "supported",
        detail: `exit 0; standalone-compile N/A (${COMPILE_GATE_EXEMPT[id]})`,
        gen: cg.gen, compile: cg.compile, test: cg.test, minted: cg.minted,
      };
      continue;
    }
    const v = codegenVerdict(cg);
    const embedded = embedFallback(`${id} (emission=${prof.name})`, example, cg, prof);
    out[prof.name] = {
      status: v.supported ? "supported" : "unsupported",
      detail: embedDetail(v.detail, embedded),
      gen: cg.gen, compile: cg.compile, test: cg.test, minted: cg.minted, embedded,
    };
  }
  return out;
}

// [ruby, rust, codegen probe].
function oracles(cell: string, example: string, externStub?: string): [number, number, CodegenProbe] {
  writeFileSync(probeFile, example + "\n");
  setCodegenInput(example, externStub);
  // Set once per row, alongside the codegen input, so every downstream generation in this row's probe
  // (the emission-profile re-probes and the embed fallback included) seeds the same defs. A cell/
  // control-op id never matches a key, so those loops clear it here.
  activeSplice = DEF_SPLICE[cell] ?? null;
  const a = RUBY_CDDL ? runProbe([RUBY_CDDL, probeFile, "generate", "1"]) : -2;
  const b = runProbe([RUST_CDDL, "compile-cddl", "--cddl", probeFile]);
  return [a, b, probeCodegen(cell)];
}

// `rubySpecValid` is the DETERMINISTIC ruby spec-validity bit (rubyClause().specValid) — NOT the raw
// generate exit, which is a Bernoulli trial for value-narrowing controllers (Change A). Everything else
// is unchanged: valid_a is that bit, spec_valid derives from it (or true for the vendor profile).
function derive(featureId: string, profile: string, rubySpecValid: boolean, rustExit: number, cg: CodegenProbe): Derived {
  const valid_a = rubySpecValid;
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

// ==================================================================================================
// DECODE-CONFORMANCE HARNESS (the fourth gate direction) — feed SPEC-DERIVED
// CBOR our code did NOT produce into the generated decoders and assert acceptance. `--mint-decode-foreign`
// (re)builds the committed catalog (D3); the default-on D4 oracle in the probe loops replays it as
// corroboration. All helpers are hoisted `function`s so the mint (called right after the warm-up) and
// the probe loops (below) can share them regardless of textual order.
// ==================================================================================================
// CatalogVector / CatalogRow, parseCatalog and composeCatalog (with its private foreignTomlStr helper)
// live in ./lib so the drift gate can round-trip the catalog without importing this whole gate module.
interface ForeignOutcome { accepts_foreign?: boolean; foreign_vectors?: number }
interface ReplayVec { hex: string; name: string; expectOk: boolean }
interface ReplayResult { verdicts: Map<string, boolean>; output: string }

// Recover the Display captured by replayInDir's deliberately per-test marker. `name:` (not merely
// `name`) is load-bearing: a candidate test `row_a1` must not borrow `row_a10`'s failure reason.
// Keeping this small parser here, next to the emitted marker, makes policy classification reviewable
// without depending on cargo's surrounding panic formatting.
function markedReplayRejectDisplay(output: string, name: string): string | null {
  const needle = `${MINT_ACCEPT_REJECTED} ${name}: `;
  const start = output.indexOf(needle);
  if (start < 0) return null;
  const from = start + needle.length;
  const end = output.indexOf("\n", from);
  return output.slice(from, end < 0 ? undefined : end);
}

// A random oracle-valid candidate is redundant policy evidence ONLY when its own decoder Display
// matches an already two-oracle-validated hand policy pin. Unknown rejection reasons stay class-less
// triage: this never turns a newly-discovered decoder bug into an invisible policy exemption.
function isKnownPolicyReject(result: ReplayResult, name: string, pins: CatalogVector[]): boolean {
  const display = markedReplayRejectDisplay(result.output, name);
  return display !== null && pins.some(p =>
    p.class === "policy-rejected" && typeof p.expect_err === "string" && p.expect_err.length > 0 &&
    display.includes(p.expect_err!),
  );
}

// Cheap pure control for the marker grammar: a longer decimal suffix must not donate its Display to
// a prefix candidate, while the exact candidate matches only the validated policy door.
function policyMintClassifierSelfTest(): void {
  const pins: CatalogVector[] = [{ hex: "00", source: "hand", expect: "reject", class: "policy-rejected", reason: "synthetic", expect_err: "Duplicate key:" }];
  const output = `${MINT_ACCEPT_REJECTED} row_a10: Duplicate key: 2\n${MINT_ACCEPT_REJECTED} row_a2: unrelated failure\n`;
  const result: ReplayResult = { verdicts: new Map(), output };
  if (markedReplayRejectDisplay(output, "row_a1") !== null ||
      !isKnownPolicyReject(result, "row_a10", pins) ||
      isKnownPolicyReject(result, "row_a2", pins))
    throw new Error("policy mint classifier self-check failed (test-name delimiter or policy reason matching regressed)");
}

// A matrix row id (`occur.optional`, `ctl.size`) -> a valid, unique Rust test-fn ident fragment.
function foreignIdent(id: string): string { return id.replace(/[^A-Za-z0-9]/g, "_"); }

// Mirror of src/utils.rs `convert_to_camel_case` (rule ident -> Rust PascalCase type name): uppercase the
// first char and any char after `_`/`-`, drop `$`/`@`. A mismatch only degrades a standalone row to holder
// mode (the `impl Deserialize for <Name>` grep misses), never a wrong verdict.
function toCamelCase(ident: string): string {
  let out = "", upper = true;
  for (const c of ident) {
    if (c === "_" || c === "-") upper = true;
    else if (c === "$" || c === "@") { /* ignored, as in the generator */ }
    else { out += upper ? c.toUpperCase() : c; upper = false; }
  }
  return out;
}

// Generate the crate from `spec` (default flags, no --wasm, no --emit-tests — replay needs only the lib).
function foreignGenCrate(outDir: string, spec: string): number {
  writeFileSync(probeFile, spec.replace(/\n*$/, "\n"));
  rmSync(outDir, { recursive: true, force: true });
  return runProbe(["cargo", "run", "-q", "--", `--input=${probeFile}`, `--output=${outDir}`, "--wasm=false"], CODEGEN_DIR);
}
// Standalone decode surface = a nominal `impl Deserialize for <typeName>` (in the generated root
// `generated/mod.rs` or `generated/serialization.rs` — the crate root lib.rs is a thin seeded stub).
// Its absence (transparent aliases: Vec/BTreeMap/u64 targets) means decode must be exercised via a holder.
function crateHasDeserialize(outDir: string, typeName: string): boolean {
  const re = new RegExp(`impl Deserialize for ${typeName.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\b`);
  for (const f of ["mod.rs", "serialization.rs"]) {
    const p = join(outDir, "rust", "src", "generated", f);
    if (existsSync(p) && re.test(readFileSync(p, "utf8"))) return true;
  }
  return false;
}

// A #[cfg(test)] replay module appended to the generated root scope (`generated/mod.rs` — where its
// `use super::*;` / `use super::serialization::*;` resolve; the crate root lib.rs is a thin seeded
// stub): one test per vector, decoding a byte-array literal through `<decodeType>::from_cbor_bytes`
// exactly as the emitted tests call it. accept => assert Ok; reject => assert Err.
// Runs `cargo test` (shared warm target) and parses per-test pass/fail. Returns null on a COMPILE failure
// (no test result lines) so callers can tell "decoder rejected a vector" (a verdict) from "crate didn't
// build" (a harness/detection problem).
function replayInDir(cell: string, outDir: string, vecs: ReplayVec[], decodeType: string): ReplayResult | null {
  const libPath = join(outDir, "rust", "src", "generated", "mod.rs");
  const fns = vecs.map(v => {
    const bytes = (v.hex.match(/../g) ?? []).map(b => `0x${b}`).join(", ");
    // is_err-only here (the corroborating D4 oracle only needs "our decoder rejects"); the rejection
    // REASON assert (catalog `expect_err`) lives in the rust replay gate
    // (src/tests/integration_tests.rs::decode_conformance_replay), which owns the durable pin.
    const body = v.expectOk
      ? `match ${decodeType}::from_cbor_bytes(BYTES) { Ok(_) => {}, Err(e) => panic!("${MINT_ACCEPT_REJECTED} ${v.name}: {}", e) }`
      : `assert!(${decodeType}::from_cbor_bytes(BYTES).is_err(), "reject vector must NOT decode");`;
    return `    #[test]\n    fn ${v.name}() {\n        const BYTES: &[u8] = &[${bytes}];\n        ${body}\n    }`;
  }).join("\n");
  const mod = `\n#[cfg(test)]\n#[allow(clippy::all)]\nmod __foreign_decode_replay {\n    use super::*;\n    use super::serialization::*;\n${fns}\n}\n`;
  writeFileSync(libPath, readFileSync(libPath, "utf8") + mod);
  const manifest = join(outDir, "rust", "Cargo.toml");
  const argv = ["cargo", "test", "--manifest-path", manifest, "--", "__foreign_decode_replay"];
  let key: string | null = null;
  let entryBase: Omit<GateCacheEntry, "cell" | "created"> | null = null;
  if (GATE_CACHE_ENABLED && runGenerateLockfile(manifest) === 0) {
    const tree = hashTree(outDir);
    // Same path-normalization as cacheableCargoPass: `outDir` is under the per-run mkdtemp
    // probeDir, so the raw argv would poison the key with a run-unique path (zero cross-run hits).
    const argvForKey = argv.map(a => a.split(outDir).join("<tree>"));
    const keyParts = gateCacheKey({ gate: "verify.decode_foreign_replay", argv: argvForKey, tree });
    key = keyParts.key;
    entryBase = { schema: GATE_CACHE_SCHEMA, gate: "verify.decode_foreign_replay", argv: argvForKey, rustc: keyParts.rustc, tree };
    if (readGateCacheEntry(key, CODEGEN_DIR)) {
      gateCacheStats.cached++;
      console.log(`[gate-cache] ${cell}: cached PASS (key ${key.slice(0, 8)})`);
      // replay stdout is consumed only to recover per-test pass/fail. A cached PASS means libtest
      // reached every appended replay test and they all passed, so the success-path map is exact.
      return { verdicts: new Map(vecs.map(v => [v.name, true])), output: "" };
    }
  }

  ensureWarm({ kind: "rust" });
  touchTree(outDir);   // same-name stale-fingerprint defense — see touchTree's comment
  gateCacheStats.run++;
  const run = () => Bun.spawnSync(
    argv,
    { cwd: CODEGEN_DIR, env: { ...process.env, CARGO_TARGET_DIR: COMPILE_TARGET }, stdout: "pipe", stderr: "pipe", timeout: PROBE_TIMEOUT * 1000 },
  );
  let r = run();
  if (r.exitedDueToTimeout) r = run();  // one transient retry, mirroring runProbe
  if (r.exitedDueToTimeout) { console.error(`HARNESS FAILURE: replay cargo test timed out twice (${outDir}).`); process.exit(2); }
  const out = (r.stdout?.toString() ?? "") + (r.stderr?.toString() ?? "");
  const res = new Map<string, boolean>();
  // the module is appended into `generated/mod.rs`, so its libtest path carries a parent module
  // prefix (`generated::__foreign_decode_replay::…`) — match the marker anywhere after `test `
  for (const m of out.matchAll(/test [\w:]*__foreign_decode_replay::(\w+) \.\.\. (ok|FAILED)/g)) res.set(m[1], m[2] === "ok");
  if ((r.exitCode ?? -1) === 0 && res.size === vecs.length && vecs.every(v => res.get(v.name) === true) && key && entryBase)
    writeGateCacheEntry(key, { ...entryBase, cell, created: new Date().toISOString() }, CODEGEN_DIR);
  if (res.size !== vecs.length) return null;  // compile error / missing tests -> not a verdict
  return { verdicts: res, output: out };
}

// ruby `cddl <spec> generate 1` emits ONE diagnostic-notation instance on stdout; diag2cbor.rb converts
// diag (stdin) -> raw CBOR (stdout). Both are needed to mint candidate vectors.
function rubyGenDiag(spec: string): { diag: string; exit: number } {
  writeFileSync(probeFile, spec.replace(/\n*$/, "\n"));
  const r = Bun.spawnSync([RUBY_CDDL!, probeFile, "generate", "1"], { stdout: "pipe", stderr: "ignore", timeout: PROBE_TIMEOUT * 1000 });
  return { diag: r.stdout?.toString() ?? "", exit: r.exitCode ?? -1 };
}
function diagToHex(diag: string): string | null {
  const r = Bun.spawnSync([DIAG2CBOR!], { stdin: new TextEncoder().encode(diag), stdout: "pipe", stderr: "ignore" });
  if ((r.exitCode ?? -1) !== 0) return null;
  const buf = Buffer.from(r.stdout ?? new Uint8Array());
  return buf.length ? buf.toString("hex") : null;
}
// Both oracles must accept a candidate against `spec` (rust needs --ci to exit nonzero on invalid — the
// startup negative control guards that flag). Returns the two exit codes; accept iff BOTH are 0.
function validateBoth(spec: string, hex: string): { ruby: number; rust: number } {
  writeFileSync(probeFile, spec.replace(/\n*$/, "\n"));
  const cbor = join(probeDir, "foreign_cand.cbor");
  writeFileSync(cbor, Buffer.from(hex, "hex"));
  const ruby = runExit([RUBY_CDDL!, probeFile, "validate", cbor]);
  const rust = runExit([RUST_CDDL, "--ci", "validate", "--cddl", probeFile, "--cbor", cbor]);
  return { ruby, rust };
}

// --- Change A: deterministic ruby verdict for the annotation `ruby=` clause -------------------------
// For a value-narrowing controller (rubyGenerateIsBernoulli), the ruby `generate` exit is a Bernoulli
// trial and must NOT be trusted (draft/ruby-cddl-generate-bernoulli-constraint-controllers.md). rubyClause
// replaces the raw generate exit wherever it fed evidence/status, with a deterministic source. Token
// grammar (the `; ruby=` delimiter downstream splitters key on is preserved; the parenthesized provenance
// is new):
//   ruby=ok | ruby=fail                 — generate exit; NON-Bernoulli examples only (deterministic in practice)
//   ruby=ok(validate) | fail(validate)  — the row's committed spec-valid accept vectors, ALL re-validated by
//                                         ruby against the CATALOG spec (deterministic input ⇒ deterministic
//                                         verdict); ok iff every accept vector validates
//   ruby=nondet(generate)               — Bernoulli example with no committed accept vectors; a STABLE token
//                                         chosen statically (no subprocess), NEVER spec-invalidating (a random
//                                         generate must not flip a row's status on a dice roll)
// `specValid` is what feeds derive()'s spec_valid on the feature axis (and the per-cell / uncorroborated
// gate on the other two loops). Validate uses a DEDICATED probe file so it never clobbers the caller's
// shared probeFile, keeping rubyClause free of loop-ordering hazards.
interface RubyClause { token: string; specValid: boolean }
function rubyValidateHex(spec: string, hex: string): number {
  const specFile = join(probeDir, "ruby_verdict.cddl");
  writeFileSync(specFile, spec.replace(/\n*$/, "\n"));
  const cbor = join(probeDir, "ruby_verdict.cbor");
  writeFileSync(cbor, Buffer.from(hex, "hex"));
  return runExit([RUBY_CDDL!, specFile, "validate", cbor]);
}
function rubyClause(id: string, example: string, generateExit: number): RubyClause {
  if (!rubyGenerateIsBernoulli(example)) {
    const okv = generateExit === 0;
    return { token: okv ? "ok" : "fail", specValid: okv };
  }
  const row = RUBY_CDDL ? catalogRows.get(id) : undefined;
  const accepts =
    row && row.spec !== undefined && row.pinned_reason === undefined
      ? row.vectors.filter(v => v.expect === "accept" && v.class !== "over-acceptance")
      : [];
  if (row?.spec !== undefined && accepts.length > 0) {
    const allOk = accepts.every(v => rubyValidateHex(row.spec!, v.hex) === 0);
    return { token: allOk ? "ok(validate)" : "fail(validate)", specValid: allOk };
  }
  // Classified but no deterministic vectors (e.g. ctl.and / ctl.within — unsupported, no catalog row):
  // a stable token, treated as not-spec-invalidating.
  return { token: "nondet(generate)", specValid: true };
}

// --- D4: the default-on corroborating oracle (called from the three probe loops on supported rows) ---
function decodeForeignProbe(id: string, matrixExample: string): ForeignOutcome {
  if (!DECODE_FOREIGN) return {};                                   // opted out -> byte-identical output
  const row = catalogRows.get(id);
  if (!row || row.pinned_reason !== undefined || row.spec === undefined || row.type_name === undefined)
    return { foreign_vectors: 0 };                                  // no usable committed entry
  if (row.example !== matrixExample) return { foreign_vectors: 0 };  // stale entry (D6 hard-gates drift)
  // EXCLUDE class="over-acceptance" accept vectors: they are spec-INVALID CBOR the decoder wrongly
  // accepts, NOT spec-valid foreign decode evidence. Corroboration is spec-valid decode evidence only,
  // and Q4 cross-checks N against the catalog's spec-valid accept count, so both sides exclude alike.
  const accepts = row.vectors.filter(v => v.expect === "accept" && v.class !== "over-acceptance");
  const rejects = row.vectors.filter(v => v.expect === "reject");
  if (accepts.length === 0) return { foreign_vectors: 0 };
  if (foreignGenCrate(nextForeignOut(), row.spec) !== 0) return { accepts_foreign: false, foreign_vectors: accepts.length };
  const vecs: ReplayVec[] = [
    ...accepts.map((v, i) => ({ hex: v.hex, name: `accept_${i}`, expectOk: true })),
    ...rejects.map((v, i) => ({ hex: v.hex, name: `reject_${i}`, expectOk: false })),
  ];
  // A null replay (no per-test verdict lines) is normally a compile failure, but across 170-ish
  // sequential replay crates it also shows up as a TRANSIENT (an isolated shifting-cell registry/
  // CONNECT abort during dep resolution — the "Registry-fetch transients in nested-cargo cells"
  // operational watch). Regenerate + replay once before recording not-accepts, mirroring the mint
  // paths' retry: without it a single transient flips a row's committed decode-foreign evidence to
  // "FAILED", which the cache-transparency gate then reads as an A/B divergence (both of that
  // gate's first two real runs failed exactly this way, on different rows each time).
  let res = replayInDir(id, ccOutForeign, vecs, row.type_name);   // null (compile fail) -> not-accepts
  if (res === null) {
    console.error(`[decode-foreign] ${id}: replay produced no per-test verdict (compile error or transient registry glitch) — regenerating + retrying once.`);
    if (foreignGenCrate(nextForeignOut(), row.spec) === 0) res = replayInDir(id, ccOutForeign, vecs, row.type_name);
  }
  return { accepts_foreign: res !== null && vecs.every(v => res.verdicts.get(v.name) === true), foreign_vectors: accepts.length };
}
// Evidence suffix (wasmEvidence twin). "" when opted out so an opted-out run's annotations are unchanged.
function decodeForeignEvidence(fo?: ForeignOutcome): string {
  if (!fo || fo.foreign_vectors === undefined) return "";
  if (fo.foreign_vectors === 0) return "; no committed decode vectors (see catalog)";
  return fo.accepts_foreign
    ? `; accepts ${fo.foreign_vectors} foreign spec-derived vector(s)`
    : `; foreign-vector decode FAILED (${fo.foreign_vectors} vector(s))`;
}

// --- D3: mint one row (returns its CatalogRow; accumulates triage/pin-break/dropped notes) -----------
// For a choice row in the arm-coverage floor's scope (`resolveChoiceArmClasses`, shared with the drift
// gate's § 7), a resample-until-covered loop (below) ensures the committed vectors carry >=1 spec-valid
// accept per resolvable arm class, so a randomized draw that misses a whole arm can't under-claim the
// row.
function mintRow(id: string, axis: string, example: string, prev: CatalogRow | undefined,
                 triage: string[], pinBreak: string[], dropped: string[],
                 staleRejectGapExemptions: string[]): CatalogRow {
  const pin = (reason: string): CatalogRow => ({ id, axis, example, pinned_reason: reason, vectors: [] });
  // COMPILE_GATE_EXEMPT rows reference user-supplied code, so their crate GENERATES (exit 0) but can
  // never compile standalone — the replay `cargo test` would fail as a compile error, not a decode
  // verdict. Pin them upfront (same exemption, same reason, as the support probe's compile gate).
  const mintPin = decodeMintPinReason(id);
  if (mintPin !== undefined) return pin(mintPin);
  const rule = firstRuleName(example);
  if (!rule) return pin("no parseable rule head in the example");

  if (foreignGenCrate(nextOut(), example) !== 0) return pin("cddl-codegen cannot generate this construct standalone");
  const typeName = toCamelCase(rule.name);
  let mode: string, spec: string, decodeType: string;
  if (crateHasDeserialize(ccOut, typeName)) {
    mode = "standalone"; spec = example; decodeType = typeName;
  } else {
    if (rule.generic) return pin(`generic rule '${rule.name}' needs type args; not standalone-decodable and not embeddable`);
    spec = `${FOREIGN_HOLDER_RULE} = [0, ${rule.name}]\n${example}`;
    if (foreignGenCrate(nextOut(), spec) !== 0) return pin("synthetic holder failed to generate");
    decodeType = toCamelCase(FOREIGN_HOLDER_RULE);
    if (!crateHasDeserialize(ccOut, decodeType)) return pin("synthetic holder minted no standalone decode surface");
    mode = "holder";
  }
  // ccOut now holds the crate generated from `spec`.

  const seen = new Set<string>();
  const candidates: string[] = [];
  let lastRubyExit = 0;
  for (let i = 0; i < FOREIGN_K; i++) {
    const g = rubyGenDiag(spec);
    if (g.exit !== 0) { lastRubyExit = g.exit; continue; }
    const hex = diagToHex(g.diag);
    if (hex && !seen.has(hex)) { seen.add(hex); candidates.push(hex); }
  }
  const handVecs = (prev?.vectors ?? []).filter(v => v.source === "hand");
  const rejectPins = (prev?.vectors ?? []).filter(v => v.expect === "reject");
  // class="over-acceptance" pins: spec-INVALID CBOR (source="hand") the decoder wrongly ACCEPTS. Held
  // separately from ordinary accept candidates — they are RE-VALIDATED spec-INVALID (both oracles
  // reject, the class="constraint" inverse gate) and committed VERBATIM, never routed through the
  // accept two-oracle gate (which would drop them as "spec-invalid") nor pruned mechanically.
  const overAcceptPins = (prev?.vectors ?? []).filter(v => v.expect === "accept" && v.class === "over-acceptance");
  // overAcceptPins is checked EXPLICITLY (not via handVecs): over-acceptance pins are source="hand" by
  // convention, but the guard must not lean on that unenforced invariant — pinning a row that still
  // holds an over-acceptance pin would silently discard a pin that must survive re-mints VERBATIM.
  if (candidates.length === 0 && handVecs.length === 0 && rejectPins.length === 0 && overAcceptPins.length === 0)
    return pin(`ruby generator cannot mint this construct (last exit ${lastRubyExit})`);

  // Two-oracle validate. Reject-intended and over-acceptance pins take precedence over accept-intended
  // for a shared hex (both are spec-INVALID; an accept candidate must not shadow them).
  const rejectHexes = new Set(rejectPins.map(v => v.hex));
  const overAcceptHexes = new Set(overAcceptPins.map(v => v.hex));
  const excludedHex = (h: string) => rejectHexes.has(h) || overAcceptHexes.has(h);
  const acDedup = new Map<string, { hex: string; source: string }>();
  for (const c of [
    ...candidates.filter(h => !excludedHex(h)).map(h => ({ hex: h, source: "ruby-generate" })),
    ...handVecs.filter(v => v.expect === "accept" && v.class !== "over-acceptance" && !excludedHex(v.hex)).map(v => ({ hex: v.hex, source: "hand" })),
  ]) {
    if (!acDedup.has(c.hex)) acDedup.set(c.hex, c);
  }

  const validatedAccept: { hex: string; source: string }[] = [];
  for (const c of acDedup.values()) {
    const { ruby, rust } = validateBoth(spec, c.hex);
    if (ruby === 0 && rust === 0) validatedAccept.push(c);
    else dropped.push(`${id}/${c.hex} (accept-intended; ruby ${ruby} rust ${rust})`);
  }

  // --- Accept-vector ARM-COVERAGE floor (resample-until-covered) --------------------------------
  // A multi-arm choice row can land with a whole arm UNSAMPLED (the FOREIGN_K draws above are random),
  // silently under-claiming its decode verdict. Reuse the SAME conservative resolver the drift gate's
  // § 7 uses (`resolveChoiceArmClasses` in lib.ts — ONE source of truth; its twin consumer is
  // project_decode_conformance.ts). While any resolvable arm class lacks a validated spec-valid accept
  // vector, draw extra ruby candidates (bounded) and keep the two-oracle-valid ones that cover a MISSING
  // class. On cap exhaustion with a genuinely-uncovered (unledgered) class, exit 1.
  const floor = resolveChoiceArmClasses(example);
  if (floor) {
    // A genuinely-unmintable arm class (a documented oracle gap) is ledgered exempt — don't pursue it
    // (the two-oracle gate can't admit it, so draws would just exhaust the cap). The drift gate's § 7
    // stale-guards the same ledger, so a class that becomes mintable fails there and gets re-minted.
    const required = new Set(floor.classes.filter(c => !Object.hasOwn(DECODE_FLOOR_ARM_EXEMPT, `${id}/${c}`)));
    const coveredClasses = () => new Set(validatedAccept.map(c => vectorShapeClass(c.hex, mode === "holder")));
    const missing = () => [...required].filter(c => !coveredClasses().has(c));
    const ARM_FLOOR_EXTRA_CAP = 60;  // bounded extra `generate` draws before giving up
    let extra = 0;
    while (missing().length && extra < ARM_FLOOR_EXTRA_CAP) {
      extra++;
      const g = rubyGenDiag(spec);
      if (g.exit !== 0) continue;
      const hex = diagToHex(g.diag);
      if (!hex || seen.has(hex) || excludedHex(hex)) continue;
      seen.add(hex);
      const cls = vectorShapeClass(hex, mode === "holder");
      if (!missing().includes(cls)) continue;  // an already-covered (or exempt) class — don't bloat the row
      const { ruby, rust } = validateBoth(spec, hex);
      if (ruby === 0 && rust === 0) validatedAccept.push({ hex, source: "ruby-generate" });
      else dropped.push(`${id}/${hex} (arm-coverage resample for class ${cls}; ruby ${ruby} rust ${rust})`);
    }
    const stillMissing = missing();  // exempt classes already excluded from `required`
    if (stillMissing.length) {
      console.error(
        `ARM-COVERAGE FLOOR: row '${id}' left arm class(es) [${stillMissing.join(", ")}] with NO spec-valid ` +
          `accept vector after ${ARM_FLOOR_EXTRA_CAP} extra generate draws (required classes {${[...required].join(", ")}}). ` +
          `The randomized mint could not cover the arm — investigate (resample bias, a mis-resolved arm, or a genuine ` +
          `oracle gap) and either fix it or add a cited DECODE_FLOOR_ARM_EXEMPT entry in lib.ts. Refusing to mint an ` +
          `under-claiming row.`,
      );
      process.exit(1);
    }
  }
  // Class-aware reject re-validation (the inverse gates):
  //   class="constraint" — spec-INVALID CBOR that violates a constraint the row enforces. Keep iff
  //     BOTH oracles still REJECT it (nonzero exit); a constraint vector that has BECOME spec-valid is
  //     upstream spec drift and is dropped/flagged. NOTE: the rust oracle (cddl 0.10.x) does NOT
  //     enforce the numeric range/eq control ops (`.le/.lt/.gt/.eq/.ne/.ge`) over a `uint` target
  //     (int-target controls ARE enforced — draft/rust-cddl-uint-control-op-gap.md) — it accepts
  //     in-type boundary violations — so a boundary-violating vector on those rows cannot pass this
  //     both-reject gate; those rows stay `enforce = unverified` by design (README.md § "Upstream
//     oracle gaps"). A
  //     constraint vector must be BASE-TYPE-VALID (only the constraint rejects it) — a type-violation
  //     vector is not enforcement evidence. `.size`, `.cbor`, and cut qualify on that standard.
  //   policy-rejected — spec-VALID CBOR both oracles accept, but the documented generated-decoder
  //     policy intentionally rejects. Keep iff BOTH oracles still ACCEPT; decoder acceptance is a
  //     policy regression, never the bug-fixed/unpin flow.
  //   bug/limitation — spec-VALID CBOR our decoder wrongly rejects. Keep iff BOTH oracles still ACCEPT.
  const validatedRejectPins: CatalogVector[] = [];
  for (const p of rejectPins) {
    const { ruby, rust } = validateBoth(spec, p.hex);
    if (p.class === "constraint") {
      // An oracle that does not implement the rule AT ALL cannot join the both-reject consensus. A
      // ledgered exemption (DECODE_REJECT_ORACLE_GAP_EXEMPT, lib.ts) names exactly which oracles
      // accept these bytes anyway, so certification narrows to the remaining oracles plus the cited
      // writeup instead of vanishing. Stale guard, the MINT half (the half that holds the oracles): a
      // ledgered oracle that now REJECTS has closed its gap, so the entry over-claims — reported at
      // the end of the mint so the entry is narrowed (or dropped) in the same change that re-certifies
      // the vector the ordinary way.
      const gap = DECODE_REJECT_ORACLE_GAP_EXEMPT[`${id}/${p.hex}`];
      const exempt = new Set(gap?.oracles ?? []);
      for (const [name, exit] of [["ruby", ruby], ["rust", rust]] as const)
        if (exempt.has(name) && exit !== 0)
          staleRejectGapExemptions.push(
            `${id}/${p.hex}: DECODE_REJECT_ORACLE_GAP_EXEMPT lists \`${name}\` as ACCEPTING these spec-invalid bytes, but ${name} now REJECTS them (exit ${exit}) — that gap closed; drop \`${name}\` from the entry (and the whole entry once no oracle remains), and prune the matching case in ${gap!.writeup}`,
          );
      if ((ruby !== 0 || exempt.has("ruby")) && (rust !== 0 || exempt.has("rust"))) validatedRejectPins.push(p);
      else dropped.push(`${id}/${p.hex} (constraint vector is no longer spec-INVALID per both oracles — upstream spec drift; ruby ${ruby} rust ${rust})`);
    } else {
      if (ruby === 0 && rust === 0) validatedRejectPins.push(p);
      else dropped.push(`${id}/${p.hex} (reject pin no longer spec-valid; ruby ${ruby} rust ${rust})`);
    }
  }
  // over-acceptance pins — the class="constraint" inverse gate (both oracles must still REJECT the
  // spec-INVALID bytes). Kept VERBATIM (class/reason/source preserved); dropped only if it has become
  // spec-VALID (a fix upstream / spec drift), which the log surfaces for human review.
  const validatedOverAccept: CatalogVector[] = [];
  for (const p of overAcceptPins) {
    const { ruby, rust } = validateBoth(spec, p.hex);
    if (ruby !== 0 && rust !== 0) validatedOverAccept.push(p);
    else dropped.push(`${id}/${p.hex} (over-acceptance vector is no longer spec-INVALID per both oracles — upstream spec drift/fix; ruby ${ruby} rust ${rust})`);
  }
  if (validatedAccept.length === 0 && validatedRejectPins.length === 0 && validatedOverAccept.length === 0) {
    // Every candidate was contested (an oracle rejected its own generator's output, or the two
    // disagree) — an oracle-artifact class, not a decoder verdict: nothing validated ever reached our
    // decoder, so there is no vector to commit and no triage to run. Pin mechanically; the per-vector
    // `dropped` log records each contested instance for review.
    return pin("all generated candidates failed two-oracle cross-validation (oracle disagreement — see mint log)");
  }

  const vecs: ReplayVec[] = [
    ...validatedAccept.map((c, i) => ({ hex: c.hex, name: `${foreignIdent(id)}_a${i}`, expectOk: true })),
    ...validatedRejectPins.map((p, i) => ({ hex: p.hex, name: `${foreignIdent(id)}_r${i}`, expectOk: false })),
    // over-acceptance vectors: the decoder is EXPECTED to (still wrongly) accept them — expectOk true.
    ...validatedOverAccept.map((p, i) => ({ hex: p.hex, name: `${foreignIdent(id)}_o${i}`, expectOk: true })),
  ];
  const res = replayInDir(id, ccOut, vecs, decodeType);
  if (res === null) { console.error(`HARNESS FAILURE: replay crate for '${id}' failed to compile (decodeType=${decodeType}, mode=${mode}) — detection bug; refusing to mint.`); process.exit(2); }

  const outVecs: CatalogVector[] = [];
  validatedAccept.forEach((c, i) => {
    const name = `${foreignIdent(id)}_a${i}`;
    if (res.verdicts.get(name) === true) outVecs.push({ hex: c.hex, source: c.source, expect: "accept" });
    else if (isKnownPolicyReject(res, name, validatedRejectPins))
      dropped.push(`${id}/${c.hex} (spec-valid generated candidate rejected by the validated policy door; redundant policy evidence omitted)`);
    else {
      outVecs.push({ hex: c.hex, source: c.source, expect: "reject" });  // class-less: triage-pending
      triage.push(`${id}/${c.hex} (mode=${mode}, type=${decodeType}): spec-valid but decoder REJECTED`);
    }
  });
  validatedRejectPins.forEach((p, i) => {
    outVecs.push(p);  // keep the row either way (re-confirmed pin, or kept for human re-triage)
    if (res.verdicts.get(`${foreignIdent(id)}_r${i}`) !== true)
      pinBreak.push(p.class === "constraint"
        ? `${id}/${p.hex}: constraint vector now DECODES cleanly — the generated decoder does NOT enforce the constraint (enforcement gap); record it in ROADMAP § findings`
        : p.class === "policy-rejected"
          ? `${id}/${p.hex}: policy-rejected vector now DECODES cleanly — cddl-codegen no longer applies the documented narrowing policy; investigate the policy regression (do NOT re-triage/unpin as a bug fix)`
          : `${id}/${p.hex}: committed reject pin now DECODES cleanly — bug fixed or decoder loosened; re-triage/unpin`);
  });
  validatedOverAccept.forEach((p, i) => {
    outVecs.push(p);  // committed VERBATIM (class="over-acceptance", reason, source preserved)
    if (res.verdicts.get(`${foreignIdent(id)}_o${i}`) !== true)
      pinBreak.push(`${id}/${p.hex}: over-acceptance vector now REJECTS — the decoder no longer wrongly accepts the spec-INVALID bytes (the fix landed); promote it to class="constraint" (+ expect_err), move the row id from EXPECTED_ENFORCE_OVERACCEPTS to EXPECTED_ENFORCE_YES in query_q4_directional.ts, and update the ROADMAP § findings entry`);
  });
  return { id, axis, example, spec, mode, type_name: decodeType, vectors: outVecs };
}

function runMintDecodeForeign(): never {
  if (!RUBY_CDDL) { console.error("HARNESS FAILURE: --mint-decode-foreign needs the ruby cddl reference (generate + validate); none resolved."); process.exit(2); }
  if (!DIAG2CBOR || !existsSync(DIAG2CBOR)) { console.error(`HARNESS FAILURE: diag2cbor.rb not found beside the ruby cddl binstub (looked at '${DIAG2CBOR}'); the cbor-diag gem is required to mint.`); process.exit(2); }

  // Negative control (after warm-up): a known-bad instance must be rejected by BOTH oracles (rust via
  // --ci) AND our decoder — else the cross-check is vacated (this is what catches a --ci-flag regression).
  {
    const spec = "n = uint";
    const badHex = "627878";  // text "xx" — invalid for a uint
    const { ruby, rust } = validateBoth(spec, badHex);
    if (ruby === 0 || rust === 0) {
      console.error(`HARNESS FAILURE: decode-conformance negative control was ACCEPTED by an oracle (ruby exit ${ruby}, rust --ci exit ${rust}); the two-oracle cross-check is not rejecting invalid CBOR (check the --ci flag). Refusing to mint.`);
      process.exit(2);
    }
    if (foreignGenCrate(nextOut(), spec) !== 0) { console.error("HARNESS FAILURE: negative-control spec `n = uint` failed to generate."); process.exit(2); }
    const res = replayInDir("mint.neg_control", ccOut, [{ hex: badHex, name: "neg_control", expectOk: false }], toCamelCase("n"));
    if (res === null || res.verdicts.get("neg_control") !== true) {
      console.error("HARNESS FAILURE: decode-conformance negative control — our generated decoder did NOT reject the known-bad instance (or the replay failed to compile). Refusing to mint.");
      process.exit(2);
    }
    console.log("[mint] negative control OK (known-bad instance rejected by ruby, rust --ci, and our decoder).");
  }

  const matrix = JSON.parse(readFileSync(`${ROOT}/matrix.json`, "utf8"));
  const anno: { id: string; status: string }[] = matrix.annotations?.cddl_codegen ?? [];
  const exampleOf = new Map<string, { axis: string; example: string }>();
  for (const f of matrix.features ?? []) if (f.example !== undefined) exampleOf.set(f.id, { axis: "feature", example: f.example });
  for (const c of matrix.containment ?? []) if (c.example !== undefined) exampleOf.set(c.id, { axis: "containment", example: c.example });
  for (const o of matrix.control_operators ?? []) if (o.example !== undefined) exampleOf.set(o.id, { axis: "control_op", example: o.example });
  const supported = [...new Set(anno.filter(r => r.status === "supported").map(r => r.id))].sort();
  if (supported.length < 80) { console.error(`HARNESS FAILURE: matrix.json reports only ${supported.length} supported rows (< 80 floor) — an implausibly small obligation set; refusing to mint.`); process.exit(2); }

  const existing = existsSync(CATALOG_PATH) ? parseCatalog(CATALOG_PATH) : new Map<string, CatalogRow>();
  const outRows = new Map<string, CatalogRow>();
  if (MINT_ONLY) {
    // A `--only` id is valid if it's either a supported row (re-mint it) or an existing catalog row
    // that just left the supported set (DROP it — a support-boundary REMOVAL, e.g. a construct newly
    // rejected at generation). Excluded from BOTH the verbatim-preserve below and `toMint`, such an id
    // simply vanishes from the output — the intended drop. A name that is neither supported nor an
    // existing row is a typo and still hard-fails.
    const unknown = [...MINT_ONLY].filter(id => !supported.includes(id) && !existing.has(id));
    if (unknown.length) { console.error(`HARNESS FAILURE: --only names row(s) that are neither 'supported' in matrix.json nor an existing catalog row: ${unknown.join(", ")}`); process.exit(2); }
    for (const [rid, row] of existing) if (!MINT_ONLY.has(rid)) outRows.set(rid, row);  // preserve verbatim
  }
  const toMint = supported.filter(id => (MINT_ONLY ? MINT_ONLY.has(id) : true));

  const triage: string[] = [];   // spec-valid vectors our decoder REJECTED (new, class-less) -> exit 1
  const pinBreak: string[] = []; // committed reject pins that now decode -> exit 1
  const dropped: string[] = [];  // contested / oracle-artifact vectors dropped (logged, not committed)
  const staleRejectGapExemptions: string[] = [];  // DECODE_REJECT_ORACLE_GAP_EXEMPT entries whose gap closed -> exit 1
  const pinnedRows: string[] = [];
  for (const id of toMint) {
    const meta = exampleOf.get(id);
    if (!meta) { console.error(`HARNESS FAILURE: supported row '${id}' has no example in matrix.json — cannot mint.`); process.exit(2); }
    const row = mintRow(id, meta.axis, meta.example, existing.get(id), triage, pinBreak, dropped, staleRejectGapExemptions);
    if (row.pinned_reason !== undefined) pinnedRows.push(`${id}: ${row.pinned_reason}`);
    outRows.set(id, row);
    const tag = row.pinned_reason !== undefined ? `PINNED (${row.pinned_reason})` : `${row.mode}, ${row.vectors.length} vector(s) [${row.type_name}]`;
    console.log(`[mint] ${id}: ${tag}`);
  }

  const content = composeCatalog([...outRows.values()]);
  try { Bun.TOML.parse(content); }
  catch (e) { console.error(`HARNESS FAILURE: composed catalog.toml does not parse as TOML (${e}) — a writer bug, not a verdict. Refusing to write.`); process.exit(2); }
  mkdirSync(dirname(CATALOG_PATH), { recursive: true });
  writeFileSync(CATALOG_PATH, content);

  const active = [...outRows.values()].filter(r => r.pinned_reason === undefined);
  const nVectors = active.reduce((n, r) => n + r.vectors.length, 0);
  const eq = "=".repeat(80);
  console.log(`\n${eq}`);
  console.log(`DECODE-CONFORMANCE MINT ${MINT_ONLY ? `(--only=${[...MINT_ONLY].sort().join(",")})` : "(full)"}`);
  console.log(eq);
  console.log(`rows written        : ${outRows.size} (minted ${toMint.length}, preserved ${outRows.size - toMint.length})`);
  console.log(`active / pinned     : ${active.length} active, ${outRows.size - active.length} pinned_reason`);
  console.log(`vectors             : ${nVectors} across ${active.length} active row(s)`);
  console.log(`modes               : standalone=${active.filter(r => r.mode === "standalone").length} holder=${active.filter(r => r.mode === "holder").length}`);
  if (pinnedRows.length) { console.log("\nPINNED (mechanically un-mintable):"); for (const p of pinnedRows) console.log(`  - ${p}`); }
  if (dropped.length) { console.log("\nDROPPED VECTORS (contested / oracle artifact — not committed):"); for (const d of dropped) console.log(`  - ${d}`); }
  console.log(`\nwrote ${CATALOG_PATH}`);
  if (triage.length) {
    console.log("\nTRIAGE-PENDING (spec-valid vectors our decoder REJECTED — committed as class-less reject rows; the drift gate stays RED until a human classifies each):");
    for (const t of triage) console.log(`  - ${t}`);
  }
  if (pinBreak.length) {
    console.log("\nPIN RE-CHECK FAILURES (committed reject pins that now decode — re-triage/unpin):");
    for (const p of pinBreak) console.log(`  - ${p}`);
  }
  if (staleRejectGapExemptions.length) {
    console.log("\nSTALE ORACLE-GAP EXEMPTIONS (a ledgered oracle now rejects the bytes — the gap closed):");
    for (const s of staleRejectGapExemptions) console.log(`  - ${s}`);
  }
  if (triage.length || pinBreak.length || staleRejectGapExemptions.length) { console.log("\nRESULT: MINT wrote the catalog but exits 1 (triage pending — see above)."); process.exit(1); }
  console.log("\nRESULT: MINT PASS");
  process.exit(0);
}

// ==================================================================================================
// CORPUS DECODE-CONFORMANCE MINT (`--mint-decode-corpus`) — the composition-depth leg. Structured
// exactly like runMintDecodeForeign (negative control, two-oracle validation, replay, arm-floor,
// triage/pin posture) but the obligation set is tests/corpus/*.cddl × the shared rule enumerator, every
// row is HOLDER mode (spec = `__probe_holder = [0, <rule>]\n<closure>`).
// ==================================================================================================

// A corpus row id: "<fixture stem>.<rule>". Kept greppable and stable.
function corpusRowId(fixture: string, rule: string): string { return `${fixture}.${rule}`; }

// Mint ONE corpus (fixture, rule) row. Always holder mode: the composition-depth value is the generated
// member/field decode path the holder routes through. GENERIC rules and rules referencing user-supplied
// code (custom ser/deser) are pinned (can't be minted standalone). `allRules` is the fixture's full
// enumeration (for the dependency closure). Accumulates triage/pin-break/dropped notes like mintRow.
function mintCorpusRow(fixture: string, rule: CorpusRule, allRules: CorpusRule[], prev: CatalogRow | undefined,
                       triage: string[], pinBreak: string[], dropped: string[]): CatalogRow {
  const id = corpusRowId(fixture, rule.name);
  const axis = "corpus";
  const example = corpusClosureBody(rule.name, allRules);   // fixture-order closure body (the committed `example`)
  const pin = (reason: string): CatalogRow => ({ id, axis, example, fixture, rule: rule.name, pinned_reason: reason, vectors: [] });

  // GENERIC rules (`pair<a,b>` — head carries `<...>`) can't be holder-wrapped bare; their
  // instantiations ARE covered via the referencing rules (`pair_use`, `[pair<uint, tstr>]`).
  if (rule.generic)
    return pin(`generic rule (head carries \`<...>\`); cannot be holder-wrapped bare — instantiations are covered via referencing rules`);
  // Rules whose closure references user-supplied ser/deser fns (`@custom_serialize` / `@custom_deserialize`)
  // generate code calling code that isn't in the crate, so the replay crate cannot compile standalone —
  // pin them upfront (the matrix's COMPILE_GATE_EXEMPT precedent for dsl.custom_serialize/deserialize).
  // `@custom_json` is NOT included: it only affects the --json profile; the default-profile crate compiles.
  if (/@custom_serialize\b|@custom_deserialize\b/.test(example))
    return pin("references user-supplied serialize/deserialize code; crate cannot compile standalone (@custom_serialize/@custom_deserialize)");

  const spec = corpusProbeSpec(rule.name, allRules);
  const genExit = foreignGenCrate(nextOut(), spec);
  if (genExit !== 0) return pin(`cddl-codegen cannot generate this construct standalone (holder+closure; cargo run exit ${genExit})`);
  const decodeType = toCamelCase(CORPUS_HOLDER_RULE);      // "ProbeHolder"
  if (!crateHasDeserialize(ccOut, decodeType)) return pin("synthetic holder minted no standalone decode surface");
  const mode = "holder";
  // ccOut now holds the crate generated from `spec`.

  const seen = new Set<string>();
  const candidates: string[] = [];
  let lastRubyExit = 0;
  for (let i = 0; i < FOREIGN_K; i++) {
    const g = rubyGenDiag(spec);
    if (g.exit !== 0) { lastRubyExit = g.exit; continue; }
    const hex = diagToHex(g.diag);
    if (hex && !seen.has(hex)) { seen.add(hex); candidates.push(hex); }
  }
  const handVecs = (prev?.vectors ?? []).filter(v => v.source === "hand");
  const rejectPins = (prev?.vectors ?? []).filter(v => v.expect === "reject");
  const overAcceptPins = (prev?.vectors ?? []).filter(v => v.expect === "accept" && v.class === "over-acceptance");
  // overAcceptPins is checked EXPLICITLY (not via handVecs): over-acceptance pins are source="hand" by
  // convention, but the guard must not lean on that unenforced invariant — pinning a row that still
  // holds an over-acceptance pin would silently discard a pin that must survive re-mints VERBATIM.
  if (candidates.length === 0 && handVecs.length === 0 && rejectPins.length === 0 && overAcceptPins.length === 0) {
    // The dominant pin cause here is the ruby 0.12.14 inline-composite `.cbor`-controller parse gap
    // (exit 65 — draft/ruby-cddl-inline-composite-control-arg-gap.md; the ir_conformance_corpus
    // RUBY_EXPECTED_FAIL prune condition in cddl-matrix/ROADMAP.md re-mints these when the gem fix ships).
    const gap = lastRubyExit === 65
      ? " — ruby 0.12.14 inline-composite `.cbor`-controller parse gap (draft/ruby-cddl-inline-composite-control-arg-gap.md; re-mint when the gem fix ships)"
      : "";
    return pin(`ruby generator cannot mint this construct (last exit ${lastRubyExit})${gap}`);
  }

  const rejectHexes = new Set(rejectPins.map(v => v.hex));
  const overAcceptHexes = new Set(overAcceptPins.map(v => v.hex));
  const excludedHex = (h: string) => rejectHexes.has(h) || overAcceptHexes.has(h);
  const acDedup = new Map<string, { hex: string; source: string }>();
  for (const c of [
    ...candidates.filter(h => !excludedHex(h)).map(h => ({ hex: h, source: "ruby-generate" })),
    ...handVecs.filter(v => v.expect === "accept" && v.class !== "over-acceptance" && !excludedHex(v.hex)).map(v => ({ hex: v.hex, source: "hand" })),
  ]) {
    if (!acDedup.has(c.hex)) acDedup.set(c.hex, c);
  }

  const validatedAccept: { hex: string; source: string }[] = [];
  // Per-oracle accept tallies over the primary candidate set, kept for the oracle-disagreement pin
  // wording below (a durable, direction-agnostic record of HOW the two oracles split).
  let tallyRubyOk = 0, tallyRustOk = 0, tallyTotal = 0;
  for (const c of acDedup.values()) {
    const { ruby, rust } = validateBoth(spec, c.hex);
    tallyTotal++;
    if (ruby === 0) tallyRubyOk++;
    if (rust === 0) tallyRustOk++;
    if (ruby === 0 && rust === 0) validatedAccept.push(c);
    else dropped.push(`${id}/${c.hex} (accept-intended; ruby ${ruby} rust ${rust})`);
  }

  // Accept-vector ARM-COVERAGE floor (resample-until-covered) — reuse the SAME resolver the drift gate's
  // corpus half uses, applied to the TARGET-FIRST closure example (corpusArmExample) so the root scan
  // lands on the target's RHS. Exemptions
  // come from the CORPUS ledger (CORPUS_DECODE_FLOOR_ARM_EXEMPT — corpus keys must never enter the
  // matrix ledger, whose stale-guard iterates matrix row ids only).
  const floor = resolveChoiceArmClasses(corpusArmExample(rule.name, allRules));
  if (floor) {
    const required = new Set(floor.classes.filter(c => !Object.hasOwn(CORPUS_DECODE_FLOOR_ARM_EXEMPT, `${id}/${c}`)));
    const coveredClasses = () => new Set(validatedAccept.map(c => vectorShapeClass(c.hex, true)));
    const missing = () => [...required].filter(c => !coveredClasses().has(c));
    const ARM_FLOOR_EXTRA_CAP = 60;
    let extra = 0;
    while (missing().length && extra < ARM_FLOOR_EXTRA_CAP) {
      extra++;
      const g = rubyGenDiag(spec);
      if (g.exit !== 0) continue;
      const hex = diagToHex(g.diag);
      if (!hex || seen.has(hex) || excludedHex(hex)) continue;
      seen.add(hex);
      const cls = vectorShapeClass(hex, true);
      if (!missing().includes(cls)) continue;
      const { ruby, rust } = validateBoth(spec, hex);
      if (ruby === 0 && rust === 0) validatedAccept.push({ hex, source: "ruby-generate" });
      else dropped.push(`${id}/${hex} (arm-coverage resample for class ${cls}; ruby ${ruby} rust ${rust})`);
    }
    const stillMissing = missing();
    if (stillMissing.length) {
      console.error(
        `ARM-COVERAGE FLOOR: corpus row '${id}' left arm class(es) [${stillMissing.join(", ")}] with NO spec-valid ` +
          `accept vector after ${ARM_FLOOR_EXTRA_CAP} extra generate draws (required {${[...required].join(", ")}}). ` +
          `Investigate (resample bias, mis-resolved arm, or oracle gap) and fix or add a cited ` +
          `CORPUS_DECODE_FLOOR_ARM_EXEMPT entry in lib.ts (the corpus ledger — NOT the matrix one). ` +
          `Refusing to mint an under-claiming row.`,
      );
      process.exit(1);
    }
  }

  // Class-aware reject re-validation (inverse gates) — policy-rejected is the spec-VALID-but-
  // intentionally-rejected sibling of bug/limitation, while constraint remains spec-invalid.
  const validatedRejectPins: CatalogVector[] = [];
  for (const p of rejectPins) {
    const { ruby, rust } = validateBoth(spec, p.hex);
    if (p.class === "constraint") {
      if (ruby !== 0 && rust !== 0) validatedRejectPins.push(p);
      else dropped.push(`${id}/${p.hex} (constraint vector is no longer spec-INVALID per both oracles — upstream spec drift; ruby ${ruby} rust ${rust})`);
    } else {
      if (ruby === 0 && rust === 0) validatedRejectPins.push(p);
      else dropped.push(`${id}/${p.hex} (reject pin no longer spec-valid; ruby ${ruby} rust ${rust})`);
    }
  }
  const validatedOverAccept: CatalogVector[] = [];
  for (const p of overAcceptPins) {
    const { ruby, rust } = validateBoth(spec, p.hex);
    if (ruby !== 0 && rust !== 0) validatedOverAccept.push(p);
    else dropped.push(`${id}/${p.hex} (over-acceptance vector is no longer spec-INVALID per both oracles — upstream spec drift/fix; ruby ${ruby} rust ${rust})`);
  }
  if (validatedAccept.length === 0 && validatedRejectPins.length === 0 && validatedOverAccept.length === 0) {
    // Durable, self-contained pin wording (not "see mint log" — the log is transient): record the
    // per-oracle accept tallies so the DIRECTION of the disagreement survives in the committed catalog,
    // and point at the durable ledger of upstream oracle gaps rather than a run artifact.
    return pin(
      `all ${tallyTotal} generated candidate(s) failed two-oracle cross-validation ` +
      `(ruby accepted ${tallyRubyOk}/${tallyTotal}, rust --ci accepted ${tallyRustOk}/${tallyTotal} — an upstream oracle ` +
      `disagreement, not a decoder verdict; see cddl-matrix/README.md § "Upstream oracle gaps" and re-mint this row when ` +
      `the cited fix ships)`,
    );
  }

  const vecs: ReplayVec[] = [
    ...validatedAccept.map((c, i) => ({ hex: c.hex, name: `${foreignIdent(id)}_a${i}`, expectOk: true })),
    ...validatedRejectPins.map((p, i) => ({ hex: p.hex, name: `${foreignIdent(id)}_r${i}`, expectOk: false })),
    ...validatedOverAccept.map((p, i) => ({ hex: p.hex, name: `${foreignIdent(id)}_o${i}`, expectOk: true })),
  ];
  // A null replay (no per-test verdict lines) is normally a compile failure, but over 130+ sequential
  // crates sharing one incremental COMPILE_TARGET it also shows up as a TRANSIENT glitch (an isolated
  // re-mint of the same row then passes). Regenerate + replay once before declaring a HARNESS FAILURE,
  // so a real non-compiling construct still fails loudly but a transient does not abort the whole mint.
  let res = replayInDir(id, ccOut, vecs, decodeType);
  if (res === null) {
    console.error(`[mint-corpus] ${id}: replay produced no per-test verdict (compile error or partial output) — regenerating + retrying once.`);
    if (foreignGenCrate(nextOut(), spec) === 0) res = replayInDir(id, ccOut, vecs, decodeType);
  }
  if (res === null) { console.error(`HARNESS FAILURE: corpus replay crate for '${id}' failed to compile TWICE (mode=${mode}) — a non-compiling standalone construct or a detection bug; refusing to mint.`); process.exit(2); }

  const outVecs: CatalogVector[] = [];
  validatedAccept.forEach((c, i) => {
    const name = `${foreignIdent(id)}_a${i}`;
    if (res.verdicts.get(name) === true) outVecs.push({ hex: c.hex, source: c.source, expect: "accept" });
    else if (isKnownPolicyReject(res, name, validatedRejectPins))
      dropped.push(`${id}/${c.hex} (spec-valid generated candidate rejected by the validated policy door; redundant policy evidence omitted)`);
    else {
      outVecs.push({ hex: c.hex, source: c.source, expect: "reject" });  // class-less: triage-pending
      triage.push(`${id}/${c.hex} (mode=${mode}, type=${decodeType}): spec-valid but decoder REJECTED`);
    }
  });
  validatedRejectPins.forEach((p, i) => {
    outVecs.push(p);
    if (res.verdicts.get(`${foreignIdent(id)}_r${i}`) !== true)
      pinBreak.push(p.class === "constraint"
        ? `${id}/${p.hex}: constraint vector now DECODES cleanly — the generated decoder does NOT enforce the constraint (enforcement gap); record it in ROADMAP § findings`
        : p.class === "policy-rejected"
          ? `${id}/${p.hex}: policy-rejected vector now DECODES cleanly — cddl-codegen no longer applies the documented narrowing policy; investigate the policy regression (do NOT re-triage/unpin as a bug fix)`
          : `${id}/${p.hex}: committed reject pin now DECODES cleanly — bug fixed or decoder loosened; re-triage/unpin`);
  });
  validatedOverAccept.forEach((p, i) => {
    outVecs.push(p);
    if (res.verdicts.get(`${foreignIdent(id)}_o${i}`) !== true)
      pinBreak.push(`${id}/${p.hex}: over-acceptance vector now REJECTS — the decoder no longer wrongly accepts the spec-INVALID bytes (the fix landed); promote it to class="constraint" (+ expect_err) and update the ROADMAP § findings entry`);
  });
  return { id, axis, example, fixture, rule: rule.name, spec, mode, type_name: decodeType, vectors: outVecs };
}

function runMintDecodeCorpus(): never {
  if (!RUBY_CDDL) { console.error("HARNESS FAILURE: --mint-decode-corpus needs the ruby cddl reference (generate + validate); none resolved."); process.exit(2); }
  if (!DIAG2CBOR || !existsSync(DIAG2CBOR)) { console.error(`HARNESS FAILURE: diag2cbor.rb not found beside the ruby cddl binstub (looked at '${DIAG2CBOR}'); the cbor-diag gem is required to mint.`); process.exit(2); }
  // The ENOSPC/scratch preflight is the startup `diskHeadroomPreflight("scratch-preflight")` (Change B) —
  // it runs before this mint is dispatched, so no inline copy is needed here.

  // Negative control (same as the foreign mint): a known-bad instance must be rejected by BOTH oracles
  // (rust via --ci) AND our decoder — else the cross-check is vacated.
  {
    const spec = "n = uint";
    const badHex = "627878";  // text "xx" — invalid for a uint
    const { ruby, rust } = validateBoth(spec, badHex);
    if (ruby === 0 || rust === 0) {
      console.error(`HARNESS FAILURE: decode-conformance negative control was ACCEPTED by an oracle (ruby exit ${ruby}, rust --ci exit ${rust}); the two-oracle cross-check is not rejecting invalid CBOR (check the --ci flag). Refusing to mint.`);
      process.exit(2);
    }
    if (foreignGenCrate(nextOut(), spec) !== 0) { console.error("HARNESS FAILURE: negative-control spec `n = uint` failed to generate."); process.exit(2); }
    const res = replayInDir("mint-corpus.neg_control", ccOut, [{ hex: badHex, name: "neg_control", expectOk: false }], toCamelCase("n"));
    if (res === null || res.verdicts.get("neg_control") !== true) {
      console.error("HARNESS FAILURE: decode-conformance negative control — our generated decoder did NOT reject the known-bad instance (or the replay failed to compile). Refusing to mint.");
      process.exit(2);
    }
    console.log("[mint-corpus] negative control OK (known-bad instance rejected by ruby, rust --ci, and our decoder).");
  }

  // Obligation set: every tests/corpus/*.cddl × every enumerated top-level rule. NEVER a hand list.
  const fixtureFiles = readdirSync(CORPUS_DIR).filter(f => f.endsWith(".cddl")).sort();
  if (fixtureFiles.length < 55) { console.error(`HARNESS FAILURE: only ${fixtureFiles.length} corpus fixtures found (< 55 floor) — an implausibly small obligation set; refusing to mint.`); process.exit(2); }
  interface CorpusObligation { id: string; fixture: string; rule: CorpusRule; allRules: CorpusRule[] }
  const obligations: CorpusObligation[] = [];
  const fixtureStems = new Set<string>();
  for (const file of fixtureFiles) {
    const stem = file.replace(/\.cddl$/, "");
    fixtureStems.add(stem);
    const text = readFileSync(join(CORPUS_DIR, file), "utf8");
    const rules = enumerateCorpusRules(text);
    if (rules.length === 0) { console.error(`HARNESS FAILURE: corpus fixture '${stem}.cddl' enumerated ZERO rules — the rule-head regex drifted or the file is empty.`); process.exit(2); }
    for (const rule of rules) {
      // Prelude names must never collide with corpus rule names (a collision would make a reference
      // ambiguous between a prelude type and a fixture rule). Asserted at enumeration time here AND by
      // the drift gate's corpus enumeration (PRELUDE_NAMES, lib.ts — same pinned source).
      if (PRELUDE_NAMES.has(rule.name)) { console.error(`HARNESS FAILURE: corpus fixture '${stem}.cddl' rule '${rule.name}' collides with a prelude type name — reference extraction would be ambiguous.`); process.exit(2); }
      obligations.push({ id: corpusRowId(stem, rule.name), fixture: stem, rule, allRules: rules });
    }
  }
  if (obligations.length < 120) { console.error(`HARNESS FAILURE: only ${obligations.length} corpus (fixture,rule) obligations (< 120 floor) — implausibly small; refusing to mint.`); process.exit(2); }

  const existing = existsSync(CORPUS_CATALOG_PATH) ? parseCatalog(CORPUS_CATALOG_PATH) : new Map<string, CatalogRow>();
  const outRows = new Map<string, CatalogRow>();
  const enumeratedIds = new Set(obligations.map(o => o.id));
  let mintOnlySelected: Set<string> | null = null;
  if (MINT_ONLY) {
    // `--only=` accepts corpus row ids AND bare fixture stems (expanding to the fixture's rows). An id
    // that is neither an enumerated row nor a fixture stem nor an existing catalog row is a typo → hard-fail.
    const selected = new Set<string>();
    const unknown: string[] = [];
    for (const tok of MINT_ONLY) {
      if (enumeratedIds.has(tok)) {
        selected.add(tok);
      } else if (fixtureStems.has(tok)) {
        for (const o of obligations) if (o.fixture === tok) selected.add(o.id);
      } else if (existing.has(tok)) {
        selected.add(tok);   // a row that left the enumerated set — DROP it (excluded below)
      } else {
        unknown.push(tok);
      }
    }
    if (unknown.length) { console.error(`HARNESS FAILURE: --only names token(s) that are neither an enumerated corpus row id, a corpus fixture stem, nor an existing catalog row: ${unknown.join(", ")}`); process.exit(2); }
    // Preserve verbatim every existing row NOT selected. A selected id that is no longer enumerated is
    // simply not re-minted and not preserved → it vanishes (the support-boundary drop).
    for (const [rid, row] of existing) if (!selected.has(rid)) outRows.set(rid, row);
    mintOnlySelected = selected;
  }
  const toMint = obligations.filter(o => (MINT_ONLY ? mintOnlySelected!.has(o.id) : true));

  const triage: string[] = [];
  const pinBreak: string[] = [];
  const dropped: string[] = [];
  const pinnedRows: string[] = [];
  for (const o of toMint) {
    const row = mintCorpusRow(o.fixture, o.rule, o.allRules, existing.get(o.id), triage, pinBreak, dropped);
    if (row.pinned_reason !== undefined) pinnedRows.push(`${o.id}: ${row.pinned_reason}`);
    outRows.set(o.id, row);
    const tag = row.pinned_reason !== undefined ? `PINNED (${row.pinned_reason})` : `${row.mode}, ${row.vectors.length} vector(s) [${row.type_name}]`;
    console.log(`[mint-corpus] ${o.id}: ${tag}`);
  }

  const content = composeCatalog([...outRows.values()], CORPUS_CATALOG_INTRO);
  try { Bun.TOML.parse(content); }
  catch (e) { console.error(`HARNESS FAILURE: composed corpus_catalog.toml does not parse as TOML (${e}) — a writer bug, not a verdict. Refusing to write.`); process.exit(2); }
  mkdirSync(dirname(CORPUS_CATALOG_PATH), { recursive: true });
  writeFileSync(CORPUS_CATALOG_PATH, content);

  const active = [...outRows.values()].filter(r => r.pinned_reason === undefined);
  const nVectors = active.reduce((n, r) => n + r.vectors.length, 0);
  const eq = "=".repeat(80);
  console.log(`\n${eq}`);
  console.log(`CORPUS DECODE-CONFORMANCE MINT ${MINT_ONLY ? `(--only=${[...MINT_ONLY].sort().join(",")})` : "(full)"}`);
  console.log(eq);
  console.log(`fixtures enumerated : ${fixtureStems.size}`);
  console.log(`rows written        : ${outRows.size} (minted ${toMint.length}, preserved ${outRows.size - toMint.length})`);
  console.log(`active / pinned     : ${active.length} active, ${outRows.size - active.length} pinned_reason`);
  console.log(`vectors             : ${nVectors} across ${active.length} active row(s)`);
  if (pinnedRows.length) { console.log("\nPINNED (mechanically un-mintable):"); for (const p of pinnedRows) console.log(`  - ${p}`); }
  if (dropped.length) { console.log("\nDROPPED VECTORS (contested / oracle artifact — not committed):"); for (const d of dropped) console.log(`  - ${d}`); }
  console.log(`\nwrote ${CORPUS_CATALOG_PATH}`);
  if (triage.length) {
    console.log("\nTRIAGE-PENDING (spec-valid vectors our decoder REJECTED — committed as class-less reject rows; the drift gate stays RED until a human classifies each):");
    for (const t of triage) console.log(`  - ${t}`);
  }
  if (pinBreak.length) {
    console.log("\nPIN RE-CHECK FAILURES (committed reject pins that now decode — re-triage/unpin):");
    for (const p of pinBreak) console.log(`  - ${p}`);
  }
  if (triage.length || pinBreak.length) { console.log("\nRESULT: CORPUS MINT wrote the catalog but exits 1 (triage pending — see above)."); process.exit(1); }
  console.log("\nRESULT: CORPUS MINT PASS");
  process.exit(0);
}

let rustWarm = false;
let wasmWarm = false;
const emissionWarm = new Set<string>();

// The warm-ups' OWN spec file. Never probeFile: LAZY warm-ups run MID-CELL (on the first cache
// miss, between a cell's rust generation and its later legs), so writing the warm spec to the
// shared probeFile made the cell's SUBSEQUENT generations (the wasm leg reuses probeFile from
// `oracles()`) silently generate the WARM crate instead of the cell — the first-miss cell's wasm
// evidence was then the warm crate's, and identical warm trees across cells cross-hit each other's
// cache entries under the wrong cell labels (caught live by verify_cache_transparency: a failing
// cell's wasm leg reported the warm crate's PASS). Eager warm-ups (GATE_CACHE=0) predate every
// cell and were immune, which is the A/B asymmetry the gate flagged.
const warmFile = join(probeDir, "warm.cddl");

function ensureRustWarm(): void {
  if (rustWarm) return;
  // Warm the shared compile target ONCE (deps + libtest harness build) and self-test a known-good,
  // minting spec. A separate warm output dir avoids clobbering the freshly generated crate whose cache
  // lookup just missed.
  writeFileSync(warmFile, "warm = [uint, tstr]\n");
  rmSync(ccWarmOut, { recursive: true, force: true });
  const warmGen = runExit(["cargo", "run", "-q", "--", `--input=${warmFile}`, `--output=${ccWarmOut}`, "--wasm=false", "--emit-tests=true"], CODEGEN_DIR, undefined, COMPILE_WARM_TIMEOUT);
  const warmLib = warmGen === 0 ? readFileSync(join(ccWarmOut, "rust", "src", "generated", "mod.rs"), "utf8") : "";
  const warmTest = warmGen === 0 ? runExit(["cargo", "test", "--manifest-path", join(ccWarmOut, "rust", "Cargo.toml")], CODEGEN_DIR, { CARGO_TARGET_DIR: COMPILE_TARGET }, COMPILE_WARM_TIMEOUT) : -2;
  if (warmGen !== 0 || warmTest !== 0 || !warmLib.includes("mod cddl_generated_tests")) {
    console.error(`HARNESS FAILURE: warm-up on a known-good spec failed (generate exit ${warmGen}, cargo test exit ${warmTest}, minted=${warmLib.includes("mod cddl_generated_tests")}). The environment is unhealthy; no probes were run and nothing was written.`);
    process.exit(2);
  }
  rustWarm = true;
}

function ensureWasmWarm(): void {
  if (wasmWarm || !WASM_PROBE) return;
  writeFileSync(warmFile, "warm = [uint, tstr]\n");
  rmSync(ccWarmOutWasm, { recursive: true, force: true });
  const wgen = runExit(["cargo", "run", "-q", "--", `--input=${warmFile}`, `--output=${ccWarmOutWasm}`, "--wasm=true", "--emit-tests=true"], CODEGEN_DIR, undefined, COMPILE_WARM_TIMEOUT);
  const wlib = wgen === 0 ? readFileSync(join(ccWarmOutWasm, "wasm", "src", "generated", "mod.rs"), "utf8") : "";
  const wtest = wgen === 0 ? runExit(["cargo", "test", "--manifest-path", join(ccWarmOutWasm, "wasm", "Cargo.toml")], CODEGEN_DIR, { CARGO_TARGET_DIR: WASM_TARGET }, COMPILE_WARM_TIMEOUT) : -2;
  if (wgen !== 0 || wtest !== 0 || !wlib.includes("mod cddl_generated_wasm_tests")) {
    console.error(`HARNESS FAILURE: wasm warm-up on a known-good spec failed (generate exit ${wgen}, cargo test exit ${wtest}, minted=${wlib.includes("mod cddl_generated_wasm_tests")}). The --wasm probe environment is unhealthy; no probes were run and nothing was written.`);
    process.exit(2);
  }
  wasmWarm = true;
}

function ensureEmissionWarm(prof: EmissionProfile): void {
  if (emissionWarm.has(prof.name)) return;
  writeFileSync(warmFile, "warm = [uint, tstr]\n");
  rmSync(ccWarmOut, { recursive: true, force: true });
  const g = runExit(["cargo", "run", "-q", "--", `--input=${warmFile}`, `--output=${ccWarmOut}`, "--wasm=false", "--emit-tests=true", ...prof.flags], CODEGEN_DIR, undefined, COMPILE_WARM_TIMEOUT);
  const lib = g === 0 ? readFileSync(join(ccWarmOut, "rust", "src", "generated", "mod.rs"), "utf8") : "";
  const t = g === 0 ? runExit(["cargo", "test", "--manifest-path", join(ccWarmOut, "rust", "Cargo.toml")], CODEGEN_DIR, { CARGO_TARGET_DIR: COMPILE_TARGET }, COMPILE_WARM_TIMEOUT) : -2;
  if (g !== 0 || t !== 0 || !lib.includes("mod cddl_generated_tests")) {
    console.error(`HARNESS FAILURE: emission-profile warm-up '${prof.name}' (flags: ${prof.flags.join(" ") || "none"}) failed (generate exit ${g}, cargo test exit ${t}, minted=${lib.includes("mod cddl_generated_tests")}). The '${prof.name}' probe pipeline is unhealthy; no probes were run and nothing was written.`);
    process.exit(2);
  }
  emissionWarm.add(prof.name);
}

function ensureWarm(need: WarmNeed): void {
  if (need.kind === "wasm") ensureWasmWarm();
  else if (need.kind === "emission") ensureEmissionWarm(need.profile);
  else ensureRustWarm();
}

if (!GATE_CACHE_ENABLED) {
  ensureRustWarm();
  // Mint modes run per-row and EXIT, skipping the wasm/emission warm-ups and all probe loops below.
  // They write ONLY their catalog, never annotations/verify_report.json.
  if (MINT_DECODE) runMintDecodeForeign();
  if (MINT_DECODE_CORPUS) runMintDecodeCorpus();
  ensureWasmWarm();
  for (const prof of PROBED_EMISSION_PROFILES) ensureEmissionWarm(prof);
} else {
  // Lazy warm-ups defer the nested-cargo self-tests to the first cache miss (a hit never touches
  // the env those self-tests guard), but the GENERATOR half must still self-test upfront: a
  // generator that doesn't build fails every probe's `cargo run` with exit 101 — recorded as
  // per-feature panics — and a generation failure never reaches a nested cargo step, so no miss
  // would ever hit a warm-up abort. One tiny known-good spec, generation-only (~seconds warm; the
  // generator build it may pay for is needed by every probe anyway).
  writeFileSync(warmFile, "warm = [uint, tstr]\n");
  rmSync(ccWarmOut, { recursive: true, force: true });
  const g = runExit(["cargo", "run", "-q", "--", `--input=${warmFile}`, `--output=${ccWarmOut}`, "--wasm=false", "--emit-tests=true"], CODEGEN_DIR, undefined, COMPILE_WARM_TIMEOUT);
  const lib = g === 0 ? readFileSync(join(ccWarmOut, "rust", "src", "generated", "mod.rs"), "utf8") : "";
  if (g !== 0 || !lib.includes("mod cddl_generated_tests")) {
    console.error(`HARNESS FAILURE: generation self-test on a known-good spec failed (generate exit ${g}, minted=${lib.includes("mod cddl_generated_tests")}). The environment is unhealthy; no probes were run and nothing was written.`);
    process.exit(2);
  }
  if (MINT_DECODE) runMintDecodeForeign();
  if (MINT_DECODE_CORPUS) runMintDecodeCorpus();
}

const probe_results: ProbeResult[] = [];
const sortedFeatures = [...features].sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0));
// --smoke=N probes only the first N features (see the flag comment near the top).
const featureList = SMOKE ? sortedFeatures.slice(0, SMOKE_N) : sortedFeatures;
for (const f of featureList) {
  const [a, b, cg] = oracles(f.id, f.example, f.example_extern_stub);
  const profile = f.profile ?? "RFC8610";
  // Change A: route the ruby verdict through the deterministic source (never the raw Bernoulli generate
  // exit for value-narrowing controllers). specValid feeds derive()'s spec_valid; token feeds evidence.
  const rc = rubyClause(f.id, f.example, a);
  const d = derive(f.id, profile, rc.specValid, b, cg);
  // Embed fallback (evidence-only): re-probe unmintable shapes wrapped in a synthetic record holder so
  // their embed-site wire path executes. Never changes `d.support` — only enriches the evidence text.
  // Not applicable to extern-stub features: the fallback writes single-file `probeFile`, but their
  // codegen input is the synthesized directory, so it would re-probe the unchanged dir and log noise.
  const embedded = f.example_extern_stub !== undefined ? undefined : embedFallback(f.id, f.example, cg);
  // Emission-profile axis: probe preserve/json iff the FINAL default verdict is supported (scoping rule
  // (a) — unsupported-at-default is unsupported everywhere, a derived fact recorded by ABSENCE of keys).
  // status === "supported" captures COMPILE_GATE_EXEMPT and vendor rows (both land status="supported").
  const emission = d.status === "supported" ? probeEmissions(f.id, f.example) : undefined;
  // Decode-foreign oracle (D4): corroborate a supported verdict by replaying the committed spec-derived
  // vectors through the generated decoder. Never changes `d.support`.
  const foreign = d.status === "supported" ? decodeForeignProbe(f.id, f.example) : {};
  probe_results.push({ id: f.id, production: f.production ?? null, profile, example: f.example, ruby: a, ruby_clause: rc.token, rust: b, codegen: cg.gen, compile: cg.compile, test: cg.test, minted: cg.minted, minted_wasm: cg.minted_wasm, wasm_gen: cg.wasm_gen, wasm_roundtrips: cg.wasm_roundtrips, accepts_foreign: foreign.accepts_foreign, foreign_vectors: foreign.foreign_vectors, embedded, emission, ...d });
}
// The later loops (containment, control-op, decode-foreign replay) write `probeFile` and expect the
// codegen probes to read it — reset the input in case the LAST feature above was extern-stub-shaped.
codegenInput = probeFile;

// Second harness-health layer (the warm-up catches a broken environment at startup; this catches one
// that degrades mid-run): zero supported features is not a plausible verdict shape for this repo.
// Skipped under --smoke (a small feature slice may legitimately contain no supported rows).
if (!SMOKE && !probe_results.some(pr => pr.support === "supported")) {
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
// --smoke skips the containment loop entirely (see the flag comment near the top).
const containCells = SMOKE ? [] : contain.filter(x => x.example).sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0));
for (const c of containCells) {
  writeFileSync(probeFile, c.example + "\n");
  const a = RUBY_CDDL ? runProbe([RUBY_CDDL, probeFile, "generate", "1"]) : -2;
  const b = runProbe([RUST_CDDL, "compile-cddl", "--cddl", probeFile]);
  // Change A: the spec-validity gate reads the DETERMINISTIC ruby verdict, not the raw generate exit
  // (byte-identical for the non-Bernoulli cells that populate the containment axis today; the routing
  // is here so a future value-narrowing cell can't gate on a dice roll). rubyValidateHex uses its own
  // probe file, so this never clobbers c.example in probeFile before probeCodegen reads it.
  const rc = rubyClause(c.id, c.example!, a);
  const rubyValid = rc.specValid;
  // only probe support where the nesting is spec-valid (ruby accepts); a spec-disallowed cell isn't
  // valid CDDL, so "does cddl-codegen support it" is meaningless.
  const cg = rubyValid ? probeCodegen(c.id) : { gen: -2, compile: -2, test: -2, minted: false };
  // execution-gate the cell too (same false-positive class as the feature axis)
  const support = rubyValid ? (codegenVerdict(cg).supported ? "supported" : "unsupported") : null;
  const observed = rubyValid ? "allowed" : "disallowed";
  const parser_limitation = rubyValid && b !== 0;  // directional, matching the feature-level definition (ruby accepts, rust rejects)
  const contradiction = observed !== c.spec;
  // Emission-profile axis, same scoping rule (a) as the feature loop: probe preserve/json iff the cell's
  // default per-cell verdict is supported.
  const emission = support === "supported" ? probeEmissions(c.id, c.example!) : undefined;
  const foreign = support === "supported" ? decodeForeignProbe(c.id, c.example!) : {};
  containment_corroboration.push({
    id: c.id, spec_declared: c.spec ?? null, spec_observed: observed,
    ruby: a, ruby_clause: rc.token, rust: b, parser_limitation, contradiction, example: c.example!,
    codegen: cg.gen, compile: cg.compile, test: cg.test, minted: cg.minted,
    minted_wasm: cg.minted_wasm, wasm_gen: cg.wasm_gen, wasm_roundtrips: cg.wasm_roundtrips,
    accepts_foreign: foreign.accepts_foreign, foreign_vectors: foreign.foreign_vectors, support, emission,
  });
}

// CONTROL-OP support. Probe each IANA op's minimal `example` through cddl-codegen (compile-gated)
// -> a [[support]] row keyed by ctl.<name>, the same pattern as features. The op set is IANA-registry-
// authoritative, so ruby/rust are CORROBORATION ONLY (an op a given ruby/rust version lacks is not
// "invalid CDDL"): support is purely the cddl-codegen verdict, supported/unsupported (no out_of_profile —
// the control-op extension RFCs 9090/9165/9741 are a separate axis from the grammar profile).
interface ControlOpSupport { id: string; name: string; support: string; detail: string; ruby: number; ruby_clause: string; rust: number; codegen: number; compile: number; test: number; minted: boolean; embedded?: boolean; example: string; emission?: Record<string, EmissionOutcome>; accepts_foreign?: boolean; foreign_vectors?: number }
const controlop_missing_example = control_ops.filter(co => !co.example).map(co => co.id);
// ruby exit 65 (EX_DATAERR) can mean the example is malformed — but ALSO that ruby's generate mode
// simply can't handle an op it postdates (verified: the RFC-correct `.printf ["%04x", 20]`, which
// the rust crate parses, still exits 65). Neither oracle can separate the two for registry ops it
// doesn't implement, so this CANNOT be a hard gate — it is surfaced as a review list instead: an
// example appearing here is uncorroborated by the reference, so check it against the defining RFC
// by hand before trusting its "unsupported" verdict.
const controlop_uncorroborated: string[] = [];
const controlop_support: ControlOpSupport[] = [];
// --smoke skips the control-op loop entirely (see the flag comment near the top).
const controlOpCells = SMOKE ? [] : [...control_ops].filter(co => co.example).sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0));
for (const co of controlOpCells) {
  const [a, b, cg] = oracles(co.id, co.example!);
  // Change A: the corroboration verdict is deterministic (rubyClause) — a value-narrowing op is
  // corroborated by ruby `validate` over its committed vectors, not by the Bernoulli `generate` exit,
  // so it can no longer land in the review list on a dice roll. A row appears here only when the
  // deterministic verdict is NOT spec-valid: a genuine parse/generate gap (e.g. `.printf` exit 65,
  // not classified) or a classified op whose committed vectors fail to validate (an upstream drift).
  const rc = rubyClause(co.id, co.example!, a);
  if (!rc.specValid) controlop_uncorroborated.push(`${co.id} (ruby ${rc.token}; generate exit ${a})`);
  const v = codegenVerdict(cg);
  // Same embed fallback as the feature loop (trivial generalization — control-op examples are single
  // named rules): an op whose annotated type mints no standalone surface (`.cbor`, `.ge` -> a bounded
  // alias) round-trips at its embed site, upgrading the evidence without touching the support verdict.
  const embedded = embedFallback(co.id, co.example!, cg);
  // Emission-profile axis, same scoping rule (a): probe preserve/json iff the op's default verdict is supported.
  const emission = v.supported ? probeEmissions(co.id, co.example!) : undefined;
  const foreign = v.supported ? decodeForeignProbe(co.id, co.example!) : {};
  controlop_support.push({ id: co.id, name: co.name, support: v.supported ? "supported" : "unsupported", detail: v.detail, ruby: a, ruby_clause: rc.token, rust: b, codegen: cg.gen, compile: cg.compile, test: cg.test, minted: cg.minted, embedded, example: co.example!, emission, accepts_foreign: foreign.accepts_foreign, foreign_vectors: foreign.foreign_vectors });
}

// Same harness-health guard as the feature loop (line ~390), extended to the two loops that run
// AFTER it. A mid-run degradation (the cddl-codegen probe binary going missing / crashing) rewrites
// every per-cell and per-op verdict to "unsupported (panic exit 101)" while the feature-only gate
// above still passes — the exact "failing looks passing" class this file exists to prevent. A real
// run always has some supported cells/ops (18/36 and 9/37 at time of writing), so zero is
// implausible: refuse to write verdicts rather than emit an all-unsupported annotation set.
if (!SMOKE && containment_corroboration.length && !containment_corroboration.some(c => c.support === "supported")) {
  console.error("HARNESS FAILURE: no containment cell probed 'supported' — implausible verdict shape; refusing to write verdicts.");
  process.exit(2);
}
if (!SMOKE && controlop_support.length && !controlop_support.some(c => c.support === "supported")) {
  console.error("HARNESS FAILURE: no control-op probed 'supported' — implausible verdict shape; refusing to write verdicts.");
  process.exit(2);
}

// Emission-axis harness-health guard, same shape as the three default-axis
// guards above: per emission profile, if >=1 row was probed under it but ZERO came back supported, that
// is an implausible verdict shape (a broken profile pipeline degraded mid-run) — refuse to write.
// Skipped under --smoke.
if (!SMOKE) {
  const allEmission: Record<string, EmissionOutcome>[] = [
    ...probe_results.map(p => p.emission),
    ...containment_corroboration.map(c => c.emission),
    ...controlop_support.map(o => o.emission),
  ].filter((e): e is Record<string, EmissionOutcome> => e !== undefined);
  for (const prof of PROBED_EMISSION_PROFILES) {
    const probed = allEmission.filter(e => prof.name in e);
    if (probed.length && !probed.some(e => e[prof.name].status === "supported")) {
      console.error(`HARNESS FAILURE: no row probed 'supported' under emission profile '${prof.name}' — implausible verdict shape; refusing to write verdicts.`);
      process.exit(2);
    }
  }
}

// ==================================================================================================
// 4. WRITE annotations/cddl_codegen.toml from the probe results (execution-grounded).
// ==================================================================================================
const ok = (exit: number) => (exit === 0 ? "ok" : "fail");
const tomlStr = (s: string) => JSON.stringify(s); // JSON string escaping is a valid TOML basic string

// The file header lives in lib.ts (`annotationsHeaderLines`) because it is a machine-regenerated
// region with a drift gate: project_corpus.ts asserts the COMMITTED header matches that template,
// so a hand edit to the rendered file (which the next run here would silently revert) and a
// template edit not mirrored into the committed file both fail loudly at the next fast-tier run.
// The DECODE_FOREIGN parameter is the opt-out discipline: the decode-foreign header paragraph is
// emitted ONLY when the oracle is on, so an opted-out run (--no-decode-foreign /
// VERIFY_DECODE_FOREIGN=0) — whose per-row evidence also omits the clause — is byte-identical to a
// pre-feature run (the wasm-oracle opt-out discipline, applied to the header too).
const annoLines: string[] = [...annotationsHeaderLines(DECODE_FOREIGN)];
// Emit the per-emission-profile dotted keys for one probed row, profiles sorted by name for
// determinism. `emission.<name>.status`/`emission.<name>.evidence` sit inside the row's [[support]]
// table (dotted sub-tables); the detail is already embed-upgraded (see probeEmissions).
function pushEmissionLines(emission?: Record<string, EmissionOutcome>) {
  if (!emission) return;
  for (const name of Object.keys(emission).sort()) {
    const e = emission[name];
    annoLines.push(`emission.${name}.status = ${tomlStr(e.status)}`);
    annoLines.push(`emission.${name}.evidence = ${tomlStr(`probe (emission=${name}): cddl-codegen ${e.detail}`)}`);
  }
}
for (const pr of probe_results) {
  let ev = `probe: cddl-codegen ${embedDetail(pr.support_detail ?? "exit " + pr.codegen, pr.embedded)}${wasmEvidence(pr, Object.hasOwn(COMPILE_GATE_EXEMPT, pr.id))}${decodeForeignEvidence({ accepts_foreign: pr.accepts_foreign, foreign_vectors: pr.foreign_vectors })}; ruby=${pr.ruby_clause} rust=${ok(pr.rust)}`;
  if (pr.parser_limitation) ev += " (rust parser limitation: reference/ABNF accept)";
  if (pr.profile === "CDDL_CODEGEN") ev += " (vendor profile: validity by cddl-codegen; ruby/rust informational)";
  if (pr.status === "out_of_profile")
    ev += ` (out of profile: feature profile ${pr.profile} is newer than cddl-codegen target ${CDDL_CODEGEN_TARGET_PROFILE})`;
  annoLines.push("[[support]]");
  annoLines.push(`id = ${tomlStr(pr.id)}`);
  annoLines.push(`status = ${tomlStr(pr.status)}`);
  annoLines.push(`evidence = ${tomlStr(ev)}`);
  pushEmissionLines(pr.emission);
  annoLines.push("");
}
// PER-CELL support (role × feature), keyed by containment id — same [[support]] table, since a
// containment id is a master id. Lets the matrix say "supported HERE, not THERE" structurally.
annoLines.push("# --- per-cell support (role × feature), keyed by containment id (see containment/*.toml) ---");
annoLines.push("");
for (const c of containment_corroboration.filter(c => c.support !== null)) {
  const roundtrips = c.test === 0 ? (c.minted ? "ok" : "n/a (nothing minted)") : "fail";
  const compile = c.codegen === 0 ? `; compiles=${c.compile === 0 ? "ok" : "fail"}; round-trips=${roundtrips}` : "";
  const ev = `probe (cell): cddl-codegen exit ${c.codegen}${compile}${wasmEvidence(c)}${decodeForeignEvidence({ accepts_foreign: c.accepts_foreign, foreign_vectors: c.foreign_vectors })}; ruby=${c.ruby_clause} rust=${ok(c.rust)}`;
  annoLines.push("[[support]]");
  annoLines.push(`id = ${tomlStr(c.id)}`);
  annoLines.push(`status = ${tomlStr(c.support!)}`);
  annoLines.push(`evidence = ${tomlStr(ev)}`);
  pushEmissionLines(c.emission);
  annoLines.push("");
}
// PER-CONTROL-OP support, keyed by ctl.<name>. cddl-codegen is the support authority; ruby/rust
// corroborate (informational) — control ops are IANA-authoritative, so a ruby/rust reject is not invalidity.
annoLines.push("# --- per-control-op support, keyed by ctl.<name> (IANA registry; ruby/rust corroborate only) ---");
annoLines.push("");
for (const co of controlop_support) {
  const ev = `probe (control-op): cddl-codegen ${embedDetail(co.detail, co.embedded)}${decodeForeignEvidence({ accepts_foreign: co.accepts_foreign, foreign_vectors: co.foreign_vectors })}; ruby=${co.ruby_clause} rust=${ok(co.rust)}`;
  annoLines.push("[[support]]");
  annoLines.push(`id = ${tomlStr(co.id)}`);
  annoLines.push(`status = ${tomlStr(co.support)}`);
  annoLines.push(`evidence = ${tomlStr(ev)}`);
  pushEmissionLines(co.emission);
  annoLines.push("");
}
// Written only after the gate passes (bottom of the script): a hard-failing run must not leave a
// poisoned "EXECUTION-GROUNDED" file on disk for the operator to accidentally commit.
const annoPath = `${ROOT}/annotations/cddl_codegen.toml`;
const annoContent = annoLines.join("\n").replace(/\s+$/, "") + "\n";

// Parse-validate the composed annotation content BEFORE it is written (or, in smoke, printed): a
// writer bug (a mis-emitted dotted key, an unescaped string) must not cost a completed full run
// at the final step, and a malformed file must never land on disk.
try {
  Bun.TOML.parse(annoContent);
} catch (e) {
  console.error(`HARNESS FAILURE: composed annotations/cddl_codegen.toml does not parse as TOML (${e}) — a writer bug, not a probe verdict. Refusing to write.`);
  process.exit(2);
}

// --smoke: print the parse-validated preview and exit WITHOUT writing anything (no annotations, no
// verify_report.json). This is the only path where the composed content is shown but not persisted.
if (SMOKE) {
  console.log(`\n${"=".repeat(80)}`);
  console.log(`SMOKE MODE (--smoke=${SMOKE_N}): probed the first ${featureList.length} feature(s) ONLY.`);
  console.log("Skipped: containment loop, control-op loop, ALL harness-health guards.");
  console.log("TOML-parse-validated annotation preview follows; NOTHING is written to disk.");
  console.log("=".repeat(80));
  console.log(annoContent);
  console.log("=".repeat(80));
  console.log("SMOKE MODE: wrote NO files (no annotations/cddl_codegen.toml, no verify_report.json).");
  console.log("=".repeat(80));
  process.exit(0);
}

// ==================================================================================================
// 5. EMIT verify_report.json, print summary, exit nonzero on hard failures.
// ==================================================================================================
const spec_invalid = probe_results.filter(pr => pr.status === "spec_invalid");
const parser_limitations = probe_results.filter(pr => pr.parser_limitation).map(pr => pr.id);
const containment_parser_limitations = containment_corroboration.filter(c => c.parser_limitation).map(c => c.id);
const containment_contradictions = containment_corroboration.filter(c => c.contradiction);
const uncertain = [...new Set(probe_results.filter(pr => pr.status === "uncertain").map(pr => pr.id))].sort();
const out_of_profile = [...new Set(probe_results.filter(pr => pr.status === "out_of_profile").map(pr => pr.id))].sort();

// EMISSION-PROFILE axis roll-up (features + containment cells + control ops). Every emission entry
// exists only for a default-supported row, so any non-supported entry IS a divergence (default
// supported, profile unsupported).
interface EmissionDivergence { id: string; profile: string; detail: string }
const emission_divergences: EmissionDivergence[] = [];
const collectDivergences = (id: string, emission?: Record<string, EmissionOutcome>) => {
  if (!emission) return;
  for (const name of Object.keys(emission).sort())
    if (emission[name].status !== "supported")
      emission_divergences.push({ id, profile: name, detail: emission[name].detail });
};
for (const pr of probe_results) collectDivergences(pr.id, pr.emission);
for (const c of containment_corroboration) collectDivergences(c.id, c.emission);
for (const co of controlop_support) collectDivergences(co.id, co.emission);
const emissionCounts: Record<string, { supported: number; unsupported: number }> = {};
for (const prof of PROBED_EMISSION_PROFILES) {
  const all = [
    ...probe_results.map(r => r.emission),
    ...containment_corroboration.map(r => r.emission),
    ...controlop_support.map(r => r.emission),
  ].map(e => e?.[prof.name]).filter((e): e is EmissionOutcome => e !== undefined);
  emissionCounts[prof.name] = {
    supported: all.filter(e => e.status === "supported").length,
    unsupported: all.filter(e => e.status !== "supported").length,
  };
}

// DECODE-FOREIGN oracle roll-up (D4): supported rows whose committed spec-derived vectors the generated
// decoder REJECTED. Corroboration only — these never changed a support verdict; surfaced like EMISSION
// DIVERGENCES so a regression is loud. Undefined `accepts_foreign` (opted out / no catalog entry / not
// supported) is not a failure.
interface DecodeForeignFailure { id: string; vectors: number }
const decode_foreign_failures: DecodeForeignFailure[] = [];
const collectForeignFailure = (id: string, af?: boolean, fv?: number) => {
  if (af === false) decode_foreign_failures.push({ id, vectors: fv ?? 0 });
};
for (const pr of probe_results) collectForeignFailure(pr.id, pr.accepts_foreign, pr.foreign_vectors);
for (const c of containment_corroboration) collectForeignFailure(c.id, c.accepts_foreign, c.foreign_vectors);
for (const co of controlop_support) collectForeignFailure(co.id, co.accepts_foreign, co.foreign_vectors);
decode_foreign_failures.sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0));
const foreignAll = [
  ...probe_results.map(r => ({ af: r.accepts_foreign, fv: r.foreign_vectors })),
  ...containment_corroboration.map(r => ({ af: r.accepts_foreign, fv: r.foreign_vectors })),
  ...controlop_support.map(r => ({ af: r.accepts_foreign, fv: r.foreign_vectors })),
];
const decodeForeignCounts = {
  rows_corroborated: foreignAll.filter(r => r.af === true).length,
  rows_no_vectors: foreignAll.filter(r => r.af === undefined && r.fv === 0).length,
  rows_failed: decode_foreign_failures.length,
  vectors_accepted: foreignAll.filter(r => r.af === true).reduce((n, r) => n + (r.fv ?? 0), 0),
};

const report = {
  gaps,
  cddl_codegen_gaps,
  fabricated,
  link_errors,
  type2_uncovered_alternatives: type2_uncovered,
  uncovered_alternatives: alt_uncovered,
  alternative_coverage_problems: altCoverageResult.problems,
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
  emission_profiles: PROBED_EMISSION_PROFILES.map(p => p.name),
  emission_divergences,
  // Conditional so an opted-out run's verify_report.json is byte-identical to a pre-feature run (the
  // per-probe accepts_foreign/foreign_vectors are already omitted when undefined; this omits the roll-up).
  ...(DECODE_FOREIGN ? { decode_foreign_failures } : {}),
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
    alt_productions: ALT_PRODUCTIONS.length,
    alt_alternatives,
    alt_accounted,
    alt_uncovered: alt_uncovered.length,
    type2_alternatives: alt_coverage["type2"].abnf_alternatives.length,
    type2_covered: alt_coverage["type2"].covered.length,
    type2_uncovered: type2_uncovered.length,
    spec_invalid: spec_invalid.length,
    parser_limitations: parser_limitations.length,
    containment_contradictions: containment_contradictions.length,
    containment_parser_limitations: containment_parser_limitations.length,
    containment_missing_example: containment_missing_example.length,
    emission: emissionCounts,
    emission_divergent: emission_divergences.length,
    ...(DECODE_FOREIGN ? { decode_foreign: decodeForeignCounts } : {}),
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
console.log(`emission axis       : ${PROBED_EMISSION_PROFILES.map(p => `${p.name}(supported=${emissionCounts[p.name].supported} unsupported=${emissionCounts[p.name].unsupported})`).join("  ")}  divergent=${s.emission_divergent}`);
console.log(`reconcile (BIDIRECTIONAL grammar lint):`);
console.log(`  forward  (source->feature): ${s.alt_accounted}/${s.alt_alternatives} alternatives accounted across ${s.alt_productions} productions (uncovered=${s.alt_uncovered})`);
console.log(`  backward (feature->source): fabricated=${s.fabricated} (feature.production resolving to no ABNF/prelude/control-op source)`);
console.log(`  prelude gaps=${s.gaps}  link_errors=${s.link_errors}`);
console.log(`type2 per-alt       : ${s.type2_covered}/${s.type2_alternatives} covered (uncovered=${s.type2_uncovered})`);
console.log(`spec-invalid (ref-rejected examples): ${s.spec_invalid}`);
console.log(`parser limitations (rust): features=${s.parser_limitations} containment=${s.containment_parser_limitations}`);
console.log(`containment         : contradictions=${s.containment_contradictions}`);
if (GATE_CACHE_ENABLED) console.log(`gate-cache          : ${gateCacheStats.run} run, ${gateCacheStats.cached} cached`);

console.log("\nALTERNATIVE COVERAGE (hard-gated for every listed production):");
for (const prod of ALT_PRODUCTIONS) {
  const cov = alt_coverage[prod];
  const nAlt = cov.abnf_alternatives.length;
  const nAcct = cov.covered.length + cov.delegated.length + cov.modeled_under.length;
  const extras = [
    cov.delegated.length ? `${cov.delegated.length} delegated` : null,
    cov.modeled_under.length ? `${cov.modeled_under.length} modelled-under` : null,
    cov.uncovered.length ? `uncovered: ${cov.uncovered.join("; ")}` : null,
  ].filter(Boolean).join("  ");
  if (!cov.modeled) console.log(`  - ${prod.padEnd(12)} [HARD] NOT MODELED (0 feature rows) — ${nAlt} ABNF alternative(s)`);
  else console.log(`  - ${prod.padEnd(12)} [HARD] ${nAcct}/${nAlt} alternatives accounted${extras ? "  " + extras : ""}`);
}

// --- SECTION REGISTRY ----------------------------------------------------------------------------
// ONE list is both the failure verdict and the printed evidence, so a category cannot join the
// `RESULT: FAIL` accounting without also getting a console section — which is exactly how
// `cddl_codegen_gaps` once failed a run with "see above" pointing at nothing, the culprit readable
// only in verify_report.json. `hard_fail` is derived by filtering this array; the summary loop below
// iterates the SAME array, so there is no second list to keep in step.
//
// `always` = print the header (with its count) even when empty — the standing counters a reader
// expects to see at zero. `when: false` removes the section entirely (a flag-gated oracle): it is
// neither printed nor eligible to fail the gate. `footer` is a trailing non-item line the section owns.
interface ReportSection {
  key: string;                      // stable identifier; matches the verify_report.json field where one exists
  hard: boolean;                    // does a non-empty `items` fail the gate?
  items: readonly unknown[];
  header: (n: number) => string;
  line?: (item: any) => string;
  always?: boolean;
  when?: boolean;
  footer?: () => string;
}
const SECTIONS: ReportSection[] = [
  {
    key: "fabricated", hard: true, items: fabricated,
    header: () => "\nFABRICATED productions (backward lint: not in ABNF, not `prelude`, not control-op registry):",
    line: (x: typeof fabricated[number]) => `  - ${x.id}: production '${x.production}'`,
  },
  {
    key: "gaps", hard: true, items: gaps,
    header: () => "\nCOMPLETENESS GAPS (prelude):",
    line: g => `  - ${JSON.stringify(g)}`,
  },
  {
    key: "cddl_codegen_gaps", hard: true, items: cddl_codegen_gaps,
    header: () => "\nCOMPLETENESS GAPS (CDDL_CODEGEN comment-DSL: documented directive/marker with no feature row):",
    line: g => `  - ${JSON.stringify(g)}`,
  },
  {
    key: "link_errors", hard: true, items: link_errors,
    header: () => "\nLINK-INTEGRITY ERRORS:",
    line: (e: typeof link_errors[number]) => `  - ${e.kind}: ${e.id} -> unknown '${e.ref}'`,
  },
  {
    key: "type2_uncovered_alternatives", hard: false, items: type2_uncovered,
    header: () => "\nTYPE2 PER-ALTERNATIVE GAPS (no covering feature row):",
    line: (a: string) => `  - ${a}`,
  },
  {
    key: "alternative_coverage_problems", hard: true, items: altCoverageResult.problems,
    header: () => "\nALTERNATIVE COVERAGE GAPS:",
    line: (p: string) => `  - ${p}`,
  },
  {
    key: "spec_invalid", hard: true, items: spec_invalid,
    header: () => "\nSPEC-INVALID EXAMPLES (REFERENCE parser rejects an authored example):",
    line: (pr: typeof spec_invalid[number]) => `  - ${pr.id}: ruby=${ok(pr.ruby)} rust=${ok(pr.rust)}  ex=${JSON.stringify(pr.example)}`,
  },
  {
    key: "containment_contradictions", hard: true, items: containment_contradictions,
    header: () => "\nCONTAINMENT CONTRADICTIONS (reference-observed spec != declared spec):",
    line: (c: typeof containment_contradictions[number]) => `  - ${c.id}: declared=${c.spec_declared} observed=${c.spec_observed}`,
  },
  {
    // One section over two collections: the feature-loop and containment-loop limitations read as one
    // list to a human, distinguished per line rather than by a second heading.
    key: "parser_limitations", hard: false,
    items: [
      ...[...parser_limitations].sort().map(id => ({ id, containment: false })),
      ...[...containment_parser_limitations].sort().map(id => ({ id, containment: true })),
    ],
    header: () => "\nPARSER LIMITATIONS (reference/ABNF accept, rust rejects — informational, non-fatal):",
    line: (u: { id: string; containment: boolean }) => `  - ${u.id}${u.containment ? " (containment)" : ""}`,
  },
  {
    key: "out_of_profile", hard: false, items: out_of_profile, always: true,
    header: n => "\nOUT_OF_PROFILE (" + n + `; profile newer than ${CDDL_CODEGEN_TARGET_PROFILE} and cddl-codegen rejects — excluded from gaps, NOT unsupported):`,
    line: (u: string) => {
      const pr = probe_results.find(p => p.id === u)!;
      return `  - ${u} (profile ${pr.profile}; ${pr.support_detail})`;
    },
  },
  {
    key: "uncertain", hard: false, items: uncertain, always: true,
    header: n => "\nUNCERTAIN (" + n + "):",
    line: (u: string) => `  - ${u}`,
  },
  {
    key: "emission_divergences", hard: false, items: emission_divergences, always: true,
    header: n => "\nEMISSION DIVERGENCES (" + n + "; default-supported but unsupported under a non-default emission profile):",
    line: (dv: EmissionDivergence) => `  - ${dv.id} [emission=${dv.profile}]: ${dv.detail}`,
  },
  {
    key: "decode_foreign_failures", hard: false, items: decode_foreign_failures, always: true, when: DECODE_FOREIGN,
    header: n => "\nDECODE-FOREIGN FAILURES (" + n + "; supported row whose committed spec-derived vectors the generated decoder REJECTED — corroboration only, verdict unchanged):",
    line: (df: DecodeForeignFailure) => `  - ${df.id} (${df.vectors} accept vector(s))`,
    footer: () => `decode-foreign     : corroborated=${decodeForeignCounts.rows_corroborated} row(s) (${decodeForeignCounts.vectors_accepted} vector(s)); no-vectors=${decodeForeignCounts.rows_no_vectors}; failed=${decodeForeignCounts.rows_failed}`,
  },
  {
    key: "controlop_missing_example", hard: true, items: controlop_missing_example,
    header: () => "\nCONTROL-OPS MISSING AN EXAMPLE (add to control_examples.toml):",
    line: (id: string) => `  - ${id}`,
  },
  {
    key: "containment_missing_example", hard: true, items: containment_missing_example,
    header: () => "\nCONTAINMENT CELLS MISSING AN EXAMPLE (unprobed, uncorroborated — add to containment/*.toml):",
    line: (id: string) => `  - ${id}`,
  },
  {
    key: "controlop_uncorroborated", hard: false, items: controlop_uncorroborated,
    header: () => "\nCONTROL-OP EXAMPLES UNCORROBORATED BY THE REFERENCE (nonzero ruby exit — either the example is malformed OR ruby postdates/rejects the op; REVIEW against the defining RFC):",
    line: (id: string) => `  - ${id}`,
  },
];
// Structural self-check: a half-registered category (no key, a duplicate key, or a hard category with
// no per-item printer) would reintroduce the very "fails with nothing to read" shape this registry
// exists to make impossible, so refuse to run rather than emit a verdict nobody can act on.
{
  const seen = new Set<string>();
  for (const s of SECTIONS) {
    if (!s.key) throw new Error("verify.ts SECTIONS: a section has an empty key");
    if (seen.has(s.key)) throw new Error(`verify.ts SECTIONS: duplicate section key '${s.key}'`);
    seen.add(s.key);
    if (s.hard && typeof s.line !== "function")
      throw new Error(`verify.ts SECTIONS: hard section '${s.key}' has no per-item printer — it could fail the gate with nothing to read`);
  }
}

for (const sec of SECTIONS) {
  if (sec.when === false) continue;
  if (!sec.always && sec.items.length === 0) continue;
  console.log(sec.header(sec.items.length));
  const line = sec.line;
  if (line) for (const item of sec.items) console.log(line(item));
  if (sec.footer) console.log(sec.footer());
}

if (harness_timeouts_retried)
  console.log(`\nNOTE: ${harness_timeouts_retried} probe(s) timed out / were killed and succeeded on retry.`);

console.log("\nwrote verify_report.json");

const hard_fail = SECTIONS.some(s => s.hard && s.when !== false && s.items.length > 0);
if (hard_fail) { console.log("\nRESULT: FAIL (hard failure — see above; annotations/cddl_codegen.toml left untouched)"); process.exit(1); }
const prevAnno = existsSync(annoPath) ? readFileSync(annoPath, "utf8") : "";
writeFileSync(annoPath, annoContent);
console.log("wrote annotations/cddl_codegen.toml");
if (annoContent !== prevAnno)
  console.log("NOTE: annotations changed — re-run `bun run build_matrix.ts` to refresh matrix.json (CI's --check gates the committed form).");
console.log("\nRESULT: PASS");
process.exit(0);
