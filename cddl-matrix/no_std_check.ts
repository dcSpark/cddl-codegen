/**
 * no_std_check.ts — the repo-side half of the no_std attribution guarantee.
 *
 * # What it asserts
 *
 * The generated rust crate compiles with `default-features = false` on a target that has no `std` at
 * all. The tool emits a `no-std-check/` shim crate into every output root and the seeded crate root
 * tells consumers to run it; this gate is what makes that instruction TRUE of tool output, so that a
 * red shim in a consumer's tree attributes to their hand-written additions rather than to us.
 *
 * # Why fresh crates and not the fixture trees
 *
 * The local `tests/<dir>/export*` trees carry module-scope `println!` helpers spliced in by the
 * integration harness OUTSIDE `#[cfg(test)]` — harness scaffolding, not tool product — so they would
 * fail here for reasons that are not about the tool. Every profile below is generated fresh.
 *
 * # Why each profile's shim is checked ALONE
 *
 * Cargo unifies features across a whole dependency graph. A check run inside a tree that also holds,
 * say, a test-only oracle dependency can have `std` re-enabled transitively for a shared dep, and the
 * check then passes over a crate that is not `no_std`-clean — this is not hypothetical, it masked a
 * real break in this repo until a sibling cell failed. The emitted shim's empty `[workspace]` table,
 * the `--manifest-path` invocation, and oracle-free fresh generation are what deliver the isolation.
 *
 * # Warning policy
 *
 * Asserting warning-FREE output would be red on day one for reasons that are not product defects, so
 * the verdict is `exit == 0` AND every `warning:` line falling in an allowed class (see
 * `classifyWarning`). Anything else fails, printing the offending lines and the full stderr.
 *
 * # The one inverted cell
 *
 * `--deserialize-depth-limit` output is deliberately NOT no_std-capable (its recursion guard is
 * `thread_local!`-based), so that profile's shim cell asserts a FAILURE carrying the pinned
 * `compile_error!` text instead. Each cell therefore carries its own accept predicate (see
 * `Accept`), and the cache stores cell-SUCCESS rather than cargo-exit-0.
 *
 * # The split-layout profile
 *
 * The `PROFILES` below are single-crate, so their shim has exactly one package to turn `std` off
 * for. A real consumer is a `--config` tree of several packages over one shared runtime crate, where
 * the feature has to FORWARD across every hop or it stops at the first — see `generateSplitProfile`.
 *
 * Standalone-invocable — `check.ts` has no single-gate selector, so the way to run this alone is
 * `bun run cddl-matrix/no_std_check.ts [fast|local|full]` (default `local`).
 */
import { existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync, utimesSync, readdirSync, statSync } from "node:fs";
import { join, resolve, dirname } from "node:path";
import { tmpdir } from "node:os";
import {
  GATE_CACHE_SCHEMA, gateCacheEnabled, gateCacheKey, hashTree, readGateCacheEntry, writeGateCacheEntry,
  type GateCacheEntry,
} from "./lib.ts";

const ROOT = import.meta.dir;
const CODEGEN_DIR = resolve(ROOT, ".."); // the cddl-codegen repo this script lives in
const TARGET = "thumbv7m-none-eabi";
const GATE = "no_std_check";
/** Bump on any change to the VERDICT logic (not to the profiles' bytes, which the tree hash covers). */
const VERDICT_MARKER = "no-std-check-v3";
const CARGO_TIMEOUT_S = 900;

/**
 * The leading substring of the `compile_error!` a `--deserialize-depth-limit` crate carries for
 * `not(feature = "std")` builds. LOCKSTEP with `generation::export::DEPTH_LIMIT_REQUIRES_STD` (the
 * Rust const that composes it into the serialization prelude, whose doc comment names this file) and
 * with the `--deserialize-depth-limit` info block in `docs/docs/command_line_flags.mdx` /
 * the attribution carve-out in `docs/docs/output_format.mdx`.
 *
 * Only the LEADING substring, deliberately: the full sentence carries remediation prose that may be
 * reworded, while these words are the identity a consumer greps and this gate asserts. A Rust-side
 * reword reds the `depth_limit.shim_thumb_expect_fail` cell below — the acceptable direction, since
 * the alternative is a cell that quietly accepts any failure at all.
 */
const DEPTH_LIMIT_REQUIRES_STD_SUBSTRING = "--deserialize-depth-limit output requires the `std` feature";

// ---- outcome ------------------------------------------------------------------------------------
// Mirrors check.ts's `Outcome` without importing it: this script is the standalone entry point, and
// check.ts imports IT (not the other way round) to avoid a cycle through the registry.
type Status = "PASS" | "FAIL" | "SKIPPED";
export interface NoStdOutcome { status: Status; reason?: string }

// ---- scratch ------------------------------------------------------------------------------------
// The `cddl_codegen_` prefix is load-bearing: the repo's scratch cleanup one-liner and the tier
// resource preflight both key on it.
const scratchDirs: string[] = [];
function scratch(tag: string): string {
  const d = mkdtempSync(join(tmpdir(), `cddl_codegen_nostd_${tag}_`));
  scratchDirs.push(d);
  return d;
}
function cleanupScratch(keep: boolean): void {
  for (const d of scratchDirs) {
    if (keep) console.log(`  scratch kept for triage: ${d}`);
    else rmSync(d, { recursive: true, force: true });
  }
}

// ---- process helpers ----------------------------------------------------------------------------
interface Ran { exit: number; stdout: string; stderr: string }

function run(cmd: string[], cwd: string, env?: Record<string, string>, timeoutS = CARGO_TIMEOUT_S): Ran {
  const r = Bun.spawnSync(cmd, {
    cwd,
    env: env ? { ...process.env, ...env } : process.env,
    stdout: "pipe",
    stderr: "pipe",
    timeout: timeoutS * 1000,
  });
  const exit = r.exitedDueToTimeout ? -1 : (r.exitCode ?? -1);
  return { exit, stdout: r.stdout?.toString() ?? "", stderr: r.stderr?.toString() ?? "" };
}

// ---- the allowed-warning classifier -------------------------------------------------------------
/**
 * Three classes, each established by capture from a real run rather than guessed:
 *
 *  1. the cdylib drop — the generated crate declares `crate-type = ["cdylib", "rlib"]`, and cargo
 *     DROPS the cdylib on a no-std target with a warning instead of erroring. Nothing to fix: the
 *     crate-type is right for the crate's normal consumers.
 *  2. the documented `Serialize` trait residue — the usage-derived import prune cannot prove a trait
 *     unused (it is exercised by a method call whose name never mentions it), so a scope importing
 *     `cbor_event::se::Serialize` without calling it warns. Tracked in tests/TESTING_ROADMAP.md's
 *     `unused_imports` entry and encoded in integration_tests' `UNUSED_IMPORT_TRAIT_RESIDUE`. Rustc
 *     renders the path in backticks either fully (`cbor_event::se::Serialize`) or as the bare leaf
 *     (`Serialize`) when it came from a braced group — both are matched, on the LEAF, exactly as the
 *     Rust-side scanner does.
 *  3. cargo's per-crate roll-up (`warning: \`pkg\` (lib) generated N warnings`), which is not a
 *     warning at all but a count of the ones above.
 *
 * Presence is never asserted, only membership: the preserve+canonical profile emits NO residue (its
 * serialization scope does call `Serialize`), so requiring class 2 would fail that profile.
 */
export function classifyWarning(line: string): "cdylib" | "trait-residue" | "rollup" | null {
  const t = line.trim();
  if (!t.startsWith("warning:")) return null;
  if (/^warning: dropping unsupported crate type `\w+` for target `[\w-]+`$/.test(t)) return "cdylib";
  if (/^warning: `[^`]+` \([^)]*\) generated \d+ warnings?/.test(t)) return "rollup";
  if (t.startsWith("warning: unused import")) {
    // Every backtick-quoted path on the line, reduced to its leaf ident — `super::*` -> `*`, which is
    // never allowed. Same reduction as `unused_generated_import_lines` in the Rust suite.
    const leaves = t.split("`").filter((_, i) => i % 2 === 1).map(p => p.split("::").pop() ?? p);
    if (leaves.length > 0 && leaves.every(l => l === "Serialize")) return "trait-residue";
  }
  return null;
}

/** Every `warning:`-leading line that is NOT in an allowed class, trimmed. */
export function disallowedWarnings(stderr: string): string[] {
  return stderr
    .split("\n")
    .filter(l => l.trim().startsWith("warning:") && classifyWarning(l) === null)
    .map(l => l.trim());
}

// ---- gate cache ---------------------------------------------------------------------------------
const GATE_CACHE_ENABLED = gateCacheEnabled();
const stats = { run: 0, cached: 0 };

/** mtimes are not key material, so this cannot affect a verdict — see verify.ts's `touchTree`. */
function touchTree(root: string): void {
  const now = new Date();
  const walk = (dir: string) => {
    for (const ent of readdirSync(dir, { withFileTypes: true })) {
      const p = join(dir, ent.name);
      if (ent.isDirectory()) { if (ent.name !== "target") walk(p); continue; }
      try { if (statSync(p).isFile()) utimesSync(p, now, now); } catch { /* best effort */ }
    }
  };
  try { walk(root); } catch { /* best effort */ }
}

/**
 * What a cell's raw cargo result has to look like for the cell to have SUCCEEDED. Not the same thing
 * as "cargo succeeded": one cell class here asserts a compile FAILURE, and it must cache on that
 * failure exactly as the green cells cache on exit 0 — a cache that only stores exit-0 would rerun
 * the expected-fail cell forever and, worse, would make "cached" mean "was green" for some cells and
 * "was correct" for others.
 */
type CellVerdict = { ok: boolean; why?: string; detail?: string[] };
type Accept = (r: { exit: number; stderr: string }) => CellVerdict;

/**
 * The default: cargo exited 0 and every `warning:` line is in an allowed class. Every cell that
 * asserts a crate BUILDS uses this one.
 */
const acceptClean: Accept = r => {
  if (r.exit !== 0) return { ok: false, why: `cargo exit ${r.exit}` };
  const bad = disallowedWarnings(r.stderr);
  if (bad.length > 0)
    return { ok: false, why: `${bad.length} warning(s) outside the allowed set`, detail: bad };
  return { ok: true };
};

/**
 * The expected-FAIL class, used by exactly one cell: a `--deserialize-depth-limit` crate's shim.
 * That crate's serialization prelude carries a `#[cfg(not(feature = "std"))] compile_error!` because
 * the recursion guard is `thread_local!`-based, and the shim depends on the crate with
 * `default-features = false` — so the check MUST fail, and a green one means the guard silently
 * stopped being std-gated.
 *
 * Success = nonzero exit AND the pinned message present. No warning policy applies: a failing
 * compilation emits whatever resolution noise the missing `std` produces (that noise is the reason
 * the `compile_error!` exists), and asserting anything about it would pin rustc diagnostics.
 */
const acceptDepthLimitRefusal: Accept = r => {
  if (r.exit === 0)
    return {
      ok: false,
      why: "cargo exit 0 — a --deserialize-depth-limit crate must NOT build with default-features = false (its depth guard is thread_local-based); the std gating regressed",
    };
  if (!r.stderr.includes(DEPTH_LIMIT_REQUIRES_STD_SUBSTRING))
    return {
      ok: false,
      why: `the build failed, but not with the pinned compile_error! — stderr does not contain \`${DEPTH_LIMIT_REQUIRES_STD_SUBSTRING}\``,
    };
  return { ok: true };
};

/**
 * One cacheable cargo cell. `manifests` are the manifests THIS cell actually checks — each gets a
 * `cargo generate-lockfile` before the tree is hashed, so a lockfile cargo would write during the
 * run cannot change the tree out from under its own key.
 *
 * `accept` decides both the verdict and what gets cached (see [`Accept`]).
 */
function cacheableCell(
  cell: string, argv: string[], manifests: string[], treeRoot: string, env: Record<string, string>,
  accept: Accept = acceptClean,
): CellVerdict & { cached: boolean; ms: number; stderr?: string } {
  let key: string | null = null;
  let entryBase: Omit<GateCacheEntry, "cell" | "created"> | null = null;
  if (GATE_CACHE_ENABLED && manifests.every(m => run(["cargo", "generate-lockfile", "--manifest-path", m], CODEGEN_DIR).exit === 0)) {
    const tree = hashTree(treeRoot);
    // PATH-NORMALIZED argv: the command line embeds this run's mkdtemp root, a fresh random path
    // every run — keying the raw argv would make every key unique and the cache useless. The tree
    // hash already pins the bytes; argv's job in the key is the command SHAPE. The verdict marker
    // rides here so a change to `classifyWarning` invalidates prior PASSes.
    const argvForKey = [VERDICT_MARKER, ...argv.map(a => a.split(treeRoot).join("<tree>"))];
    const parts = gateCacheKey({ gate: GATE, argv: argvForKey, tree });
    key = parts.key;
    entryBase = { schema: GATE_CACHE_SCHEMA, gate: GATE, argv: argvForKey, rustc: parts.rustc, tree };
    if (readGateCacheEntry(key, CODEGEN_DIR)) {
      stats.cached++;
      console.log(`[gate-cache] ${cell}: cached PASS (key ${key.slice(0, 8)})`);
      return { ok: true, cached: true, ms: 0 };
    }
  }
  touchTree(treeRoot);
  stats.run++;
  const t0 = performance.now();
  const r = run(argv, CODEGEN_DIR, env);
  const ms = performance.now() - t0;
  const verdict = accept(r);
  if (verdict.ok && key && entryBase)
    writeGateCacheEntry(key, { ...entryBase, cell, created: new Date().toISOString() }, CODEGEN_DIR);
  return { ...verdict, stderr: r.stderr, cached: false, ms };
}

// ---- profiles -----------------------------------------------------------------------------------
interface Profile {
  id: string;
  /** Distinct `--lib-name` per profile: cargo's fingerprint is keyed by package name + version, so
   *  three same-named `cddl-lib` crates sharing a target dir can be declared fresh off each other's
   *  artifacts. Distinct names remove that hazard at the root instead of defending against it. */
  libName: string;
  flags: string[];
  cddl: string;
  /** Hand-written consumer code the profile's documented extern contract REQUIRES (profile B only). */
  consumer?: { module: string; source: string; rootAppend: string };
  /** Whether this profile also gets the host-target std-arm hasher tripwire (profile A only —
   *  `MapHashBuilder` exists only under `--preserve-encodings`). */
  hostStdArm?: boolean;
  /** `--deserialize-depth-limit` output: the shim cell INVERTS — the crate's `thread_local!` guard
   *  is std-only, so a `default-features = false` build must fail carrying the pinned
   *  `compile_error!`. The profile trades its `shim_thumb` cell for `shim_thumb_expect_fail` plus a
   *  `host_default_check` proving the flag is inert under the (default-on) `std` feature. */
  depthLimitRefusal?: boolean;
  /** `--emit-tests` output: the emitted `#[cfg(test)]` module is invisible to every shim cell (test
   *  code is not compiled when a crate is built as a DEPENDENCY, and the shim cells `cargo check`),
   *  so this profile adds a host `cargo test --no-default-features --lib` that actually compiles and
   *  RUNS it under the `not(std)` crate root. */
  hostTestNoStd?: boolean;
}

const PROFILES: Profile[] = [
  {
    // Maximises the preserve-encodings surface: the OrderedHashMap public type (hence the
    // `MapHashBuilder` cfg pair), the derivative-derived key traits in all four `@used_as_key`
    // flavors, and a bytes wrapper. NO float member — `--preserve-encodings` aborts generation on
    // one (the tracked `preserve_encodings_supports_floats` stub).
    id: "preserve_canonical",
    libName: "nostd-preserve",
    hostStdArm: true,
    flags: ["--preserve-encodings=true", "--canonical-form=true"],
    cddl: [
      "hash28 = bytes .size 28",
      "key_bare = [ a: uint, b: text ] ; @used_as_key",
      "key_hash = [ c: uint ] ; @used_as_key hash",
      "key_ord = [ d: uint ] ; @used_as_key ord",
      "key_cstyle = 0 / 1 / 2 ; @used_as_key",
      "tbl = { * uint => text }",
      "outer = [ h: hash28, t: tbl, kb: key_bare, kh: key_hash, ko: key_ord, kc: key_cstyle ]",
      "",
    ].join("\n"),
  },
  {
    // The raw-bytes runtime — `RawBytesEncoding` and the `decode_canonical_hex` door its
    // `from_raw_hex` reads through, i.e. BOTH `hex::` call sites — which the snapshot corpus
    // structurally never emits. Note the runtime is CONCATENATED INTO
    // `generated/serialization.rs`; there is no separate `raw_bytes_encoding.rs` or
    // `hex_canonical.rs` in the output to look for.
    id: "raw_bytes",
    libName: "nostd-rawbytes",
    flags: ["--preserve-encodings=true"],
    cddl: ["pubkey = _CDDL_CODEGEN_RAW_BYTES_TYPE_", "sig = [ k: pubkey, m: bytes ]", ""].join("\n"),
    consumer: {
      module: "raw_bytes_consumer",
      // The documented extern contract: a `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule REQUIRES a
      // hand-written definition, so zero-hand-code purity is impossible for this profile. Kept
      // minimal and deliberately no_std-clean so a red cell attributes to the tool, not to this.
      source: [
        "//! Hand-written by the no_std_check gate: the consumer half of the documented",
        "//! `_CDDL_CODEGEN_RAW_BYTES_TYPE_` contract. Deliberately no_std-clean.",
        "extern crate alloc;",
        "",
        "use crate::error::DeserializeError;",
        "use crate::serialization::RawBytesEncoding;",
        "use alloc::vec::Vec;",
        "",
        "#[derive(Clone, Debug)]",
        "pub struct Pubkey(Vec<u8>);",
        "",
        "impl RawBytesEncoding for Pubkey {",
        "    fn to_raw_bytes(&self) -> &[u8] {",
        "        &self.0",
        "    }",
        "",
        "    fn from_raw_bytes(bytes: &[u8]) -> Result<Self, DeserializeError> {",
        "        Ok(Self(bytes.to_vec()))",
        "    }",
        "}",
        "",
      ].join("\n"),
      // Appended to the SEED-ONCE crate root — the documented consumer flow, not a patch to any
      // generated file. Nothing under `src/generated/**` is touched by this gate.
      rootAppend: "\npub mod raw_bytes_consumer;\npub use raw_bytes_consumer::Pubkey;\n",
    },
  },
  {
    // The JSON surface the corpus never emits: `json_schema_gen.rs` (the Registrar runtime and the
    // `$crate::json_schema_gen::Cow` macro), `json_value_ser.rs`, and serde/schemars derives on the
    // generated types — all compiled into the RUST crate, hence inside the thumb check.
    //
    // `any_members` is the `any` × `--json-serde-derives` composition, and it is here for a reason
    // no other profile covers: `any_cbor.rs` is emitted only when the finalized IR contains `any`,
    // and under `--json-serde-derives` the WHOLE of `static/any_cbor_json.rs` is appended to it —
    // eight nested inline `natural_any_cbor_*` adapter modules, all compiled whether or not the spec
    // reaches them. A file-top `use alloc::…` does not reach a nested inline module, so each of those
    // modules carries its own `use super::alloc::…;` BY HAND; a missed one is invisible under `std`
    // (the std prelude supplies the name) and an E0425 the moment the crate is built without it.
    // That is exactly the shape of the first consumer-reported no_std break, and this member set —
    // a plain `any`, an optional `any`, a `[* any]` seq, an optional seq, a `{* K => any}` table and
    // an optional table — is what puts every adapter shape inside the thumb compile.
    id: "json_schema",
    libName: "nostd-json",
    flags: ["--preserve-encodings=true", "--json-serde-derives=true", "--json-schema-export=true"],
    cddl: [
      "inner = [ a: uint, b: text ]",
      "tbl = { * uint => text }",
      "any_members = { 1: any, ? 2: any, 3: [* any], ? 4: [* any], 5: { * uint => any }, ? 6: { * uint => any } }",
      "outer = [ i: inner, t: tbl, ? o: bytes, am: any_members ]",
      "",
    ].join("\n"),
  },
  {
    // The ONE flag whose output is deliberately not no_std-capable. `--deserialize-depth-limit`
    // emits a `thread_local!`-based recursion guard, so the serialization prelude also carries a
    // `#[cfg(not(feature = "std"))] compile_error!` — and the shim, which every export emits and
    // which depends with `default-features = false`, is what a consumer will see it through. Both
    // halves of that contract are cells: the refusal must FIRE with the pinned message (a green
    // shim would mean the std gating silently regressed), and it must be INERT on a normal build
    // (the `std` feature is default-on, so `--deserialize-depth-limit` costs a std consumer
    // nothing). A recursive rule so the guard is actually threaded into a deserializer.
    id: "depth_limit",
    libName: "nostd-depth",
    depthLimitRefusal: true,
    flags: ["--deserialize-depth-limit", "64"],
    cddl: ["tree = [value: uint, children: [* tree]]", ""].join("\n"),
  },
  {
    // `--emit-tests` output, whose std usage (`std::env`/`std::fs` dump hook, `format!`, `vec!`,
    // `eprintln!`) lives in a `#[cfg(test)]` module that the generated crate restores `std` for
    // itself. Two cells for two independent reasons: `shim_thumb` guards the direction where that
    // restore LEAKS out of the test module into crate-scope code (the shim would stop building),
    // and `host_test_nostd` is the only cell in this gate that compiles the test module at all —
    // `#[cfg(test)]` code is not built when a crate is a dependency, and every other cell is a
    // `cargo check`. Preserve-encodings is on so the nested `cddl_encoding_fidelity` module, which
    // carries its own hand-written copy of the restore, is in the compile.
    id: "emit_tests",
    libName: "nostd-emittests",
    hostTestNoStd: true,
    flags: ["--emit-tests=true", "--preserve-encodings=true"],
    cddl: [
      "hash28 = bytes .size 28",
      "inner = [ a: uint, b: text ]",
      "tbl = { * uint => text }",
      "outer = [ h: hash28, i: inner, t: tbl, ? o: bytes ]",
      "",
    ].join("\n"),
  },
];

// ---- the split-layout profile (`--config`) ------------------------------------------------------
/**
 * The layout every other profile in this file cannot reach: a `--config` tree whose crates are
 * SEPARATE cargo packages, joined by a `deps` edge, all sharing one `--export-static-crate` runtime
 * crate.
 *
 * # What only this profile can prove
 *
 * The single-crate profiles above check one package, so `default-features = false` on their shim has
 * exactly one crate to reach. In a split layout it has three, and the feature has to FORWARD across
 * each hop or it stops at the first: a crate whose own `std` is off while its dependency's is on is
 * a crate whose dependency still selected the `std` arm. On `thumbv7m-none-eabi` `std` does not
 * exist at all, so a leaked `std` feature anywhere in the chain is a compile error rather than a
 * silent difference — which is what makes the thumb cell below the reachability proof for the
 * runtime crate's `#[cfg(not(feature = "std"))]` hasher arm through a real consumer topology.
 *
 * Both forwarding classes ride on the one profile: `deps` (leaf → core) and `[runtime].lib-name`
 * (both crates → the co-owned runtime).
 *
 * # The hand-owned half
 *
 * `--export-static-crate` writes into a crate the TOOL DOES NOT OWN: its `[package]` table is
 * seed-once and its crate root is never written at all. So the gate plays the consumer, exactly as
 * `config_tests::a_runtime_table_exports_a_runtime_the_other_flavor_compiles_against` does and as
 * profile B plays the `_CDDL_CODEGEN_RAW_BYTES_TYPE_` consumer: a minimal `[package]` table before
 * generating (so the seed sees a name to keep — it must MATCH `[runtime].lib-name`, which the tool
 * deliberately does not read that manifest to check), and the root afterwards. Afterwards because
 * the module list is the export's, not ours to predict; deliberately no_std-clean and carrying the
 * documented `cfg_attr` line, so a red cell attributes to the tool rather than to this.
 */
const SPLIT_ID = "split_config";
const SPLIT_RUNTIME_PACKAGE = "nostd-runtime";
const SPLIT_RUNTIME_CRATE = "nostd_runtime";

function generateSplitProfile(): string {
  const root = scratch(SPLIT_ID);
  mkdirSync(join(root, "specs"), { recursive: true });
  mkdirSync(join(root, "runtime", "src"), { recursive: true });
  // `core` carries `--preserve-encodings`, so `OrderedHashMap` — and therefore the `MapHashBuilder`
  // cfg pair the whole no_std switch is observable through — is in the compile.
  writeFileSync(join(root, "specs", "core.cddl"),
    ["hash28 = bytes .size 28", "tbl = { * uint => text }", "core_thing = [ h: hash28, t: tbl ]", ""].join("\n"));
  writeFileSync(join(root, "specs", "leaf.cddl"), ["leaf_rec = [ c: core_thing, n: uint ]", ""].join("\n"));
  // Hand-owned half, part 1: the package identity. Written BEFORE generating because the export's
  // `package.name` op is seed-once — it writes only into a manifest that has none.
  writeFileSync(join(root, "runtime", "Cargo.toml"), [
    "# Hand-written by the no_std_check gate: the co-owned runtime crate's identity. Its `name` must",
    "# match `[runtime].lib-name` — the tool derives the dependency from that key and does not read",
    "# this file to check.",
    "[package]", `name = "${SPLIT_RUNTIME_PACKAGE}"`, 'version = "0.0.0"', 'edition = "2024"',
    "publish = false", "", "[workspace]", "",
  ].join("\n"));
  writeFileSync(join(root, "codegen.toml"), [
    "[defaults]", `static-dir = "${join(CODEGEN_DIR, "static")}"`, "wasm = false", "preserve-encodings = true", "",
    "[runtime]", 'export-static-crate = "runtime"', `common-import = "${SPLIT_RUNTIME_CRATE}"`,
    `lib-name = "${SPLIT_RUNTIME_PACKAGE}"`, "",
    "[crates.core]", 'input = "specs/core.cddl"', 'output = "gen/core"', 'lib-name = "nostd-core"', "",
    "[crates.leaf]", 'input = "specs/leaf.cddl"', 'output = "gen/leaf"', 'lib-name = "nostd-leaf"',
    'deps = ["core"]', "",
  ].join("\n"));

  const gen = run(["cargo", "run", "-q", "--bin", "cddl-codegen", "--", "--config", join(root, "codegen.toml")], CODEGEN_DIR);
  if (gen.exit !== 0) {
    console.log(`  ${SPLIT_ID}: GENERATION FAILED (exit ${gen.exit})`);
    console.log(gen.stdout);
    console.log(gen.stderr);
    throw new Error(`generation failed for profile ${SPLIT_ID}`);
  }

  // Hand-owned half, part 2: the crate root. `pub mod` lines only, plus the one documented opt-in
  // line — without it the runtime crate is a plain `std` crate and the thumb cell would fail on a
  // target that has no `std`, which is a fact about this hand file rather than about the forwarding.
  const runtimeSrc = join(root, "runtime", "src");
  const modules = readdirSync(runtimeSrc)
    .filter(f => f.endsWith(".rs") && f !== "lib.rs")
    .map(f => f.slice(0, -3))
    .sort();
  if (modules.length === 0)
    throw new Error(`profile ${SPLIT_ID}: the export wrote no runtime modules — the export half is broken`);
  writeFileSync(join(runtimeSrc, "lib.rs"), [
    "//! Hand-written by the no_std_check gate: the co-owned runtime crate's root, which the tool",
    "//! never writes. `pub mod` lines and the documented opt-in line, nothing else.",
    '#![cfg_attr(not(feature = "std"), no_std)]',
    "",
    ...modules.map(m => `pub mod ${m};`),
    "",
  ].join("\n"));

  // The std-arm tripwire, on the crate that OWNS the cfg pair in this layout: the runtime. Same
  // placement rules as `writeHostConsumer` — inside the hashed tree, before any cell runs.
  const hostDir = join(root, "host-consumer");
  mkdirSync(join(hostDir, "src"), { recursive: true });
  writeFileSync(join(hostDir, "Cargo.toml"), [
    "[package]", 'name = "split-host-consumer"', 'version = "0.0.0"', 'edition = "2024"', "publish = false", "",
    "[dependencies]", `${SPLIT_RUNTIME_PACKAGE} = { path = "../runtime" }`, "", "[workspace]", "",
  ].join("\n"));
  writeFileSync(join(hostDir, "src", "lib.rs"), [
    "//! Written by the no_std_check gate: the std-arm hasher assertion for the SPLIT layout, where",
    "//! the cfg pair lives in the shared runtime crate rather than in a generated one.",
    `use ${SPLIT_RUNTIME_CRATE}::ordered_hash_map::MapHashBuilder;`,
    "",
    "pub fn _h(h: MapHashBuilder) -> std::collections::hash_map::RandomState {",
    "    h",
    "}",
    "",
  ].join("\n"));

  const shim = join(root, "gen", "leaf", "no-std-check", "Cargo.toml");
  if (!existsSync(shim))
    throw new Error(`profile ${SPLIT_ID}: the tool emitted no shim at ${shim} — the emission half is broken`);
  return root;
}

/** Both cells of the split profile. Returns false if either failed. */
function runSplitProfile(cargoTarget: string): boolean {
  const root = generateSplitProfile();
  let ok = true;
  // The consumer at the FAR end of the chain: its shim turns `std` off on `nostd-leaf`, which must
  // reach `nostd-core` and then `nostd-runtime`. Green here is the reachability proof.
  const shimManifest = join(root, "gen", "leaf", "no-std-check", "Cargo.toml");
  const shimCell = cacheableCell(
    `${SPLIT_ID}.shim_thumb`,
    ["cargo", "check", "--manifest-path", shimManifest, "--target", TARGET],
    [shimManifest], root, { CARGO_TARGET_DIR: cargoTarget },
  );
  if (!reportCell(`${SPLIT_ID}.shim_thumb`, shimCell)) ok = false;

  // …and the default direction, so the forwarding cannot be "fixed" by leaving the std arm behind.
  const hostManifest = join(root, "host-consumer", "Cargo.toml");
  const hostCell = cacheableCell(
    `${SPLIT_ID}.host_std_arm`,
    ["cargo", "check", "--manifest-path", hostManifest],
    [hostManifest], root, { CARGO_TARGET_DIR: cargoTarget },
  );
  if (!reportCell(`${SPLIT_ID}.host_std_arm`, hostCell)) ok = false;
  return ok;
}

// ---- target guard -------------------------------------------------------------------------------
function targetInstalled(): boolean {
  // cwd = repo root so `rust-toolchain.toml` selects the pinned toolchain — targets are installed
  // PER TOOLCHAIN, so asking any other one answers a different question.
  const r = run(["rustup", "target", "list", "--installed"], CODEGEN_DIR, undefined, 120);
  return r.exit === 0 && r.stdout.split("\n").some(l => l.trim() === TARGET);
}

function loudSkipMessage(): string[] {
  return [
    "",
    "  ============================================================================",
    `  ${GATE}: SKIPPED — the '${TARGET}' target is not installed`,
    "",
    "  This gate is the repo-side half of the documented no_std attribution guarantee:",
    "  it proves the generated crate builds without `std`, so that a red no-std-check in",
    "  a consumer's tree attributes to their hand-written code. Skipping it means nothing",
    "  in this run checked that.",
    "",
    "  Fix (rustup-managed checkouts): the target is declared in rust-toolchain.toml and",
    "  installs itself — if it is missing here, run `rustup toolchain install` for the",
    "  pinned toolchain, or simply:",
    `      rustup target add ${TARGET}`,
    "",
    "  This is a SKIP in the local tier and a hard FAIL in full: a silent skip would void",
    "  the guarantee with nothing else positioned to notice.",
    "  ============================================================================",
    "",
  ];
}

// ---- the gate ------------------------------------------------------------------------------------
function generateProfile(p: Profile): { out: string; specPath: string } {
  const root = scratch(p.id);
  const specPath = join(root, "spec.cddl");
  writeFileSync(specPath, p.cddl);
  const out = join(root, "out");
  const gen = run([
    "cargo", "run", "-q", "--bin", "cddl-codegen", "--",
    "--input", specPath, "--output", out, "--wasm=false", "--lib-name", p.libName, ...p.flags,
  ], CODEGEN_DIR);
  if (gen.exit !== 0) {
    console.log(`  ${p.id}: GENERATION FAILED (exit ${gen.exit})`);
    console.log(gen.stdout);
    console.log(gen.stderr);
    throw new Error(`generation failed for profile ${p.id}`);
  }
  if (p.consumer) {
    const rustSrc = join(out, "rust", "src");
    writeFileSync(join(rustSrc, `${p.consumer.module}.rs`), p.consumer.source);
    const rootPath = join(rustSrc, "lib.rs");
    writeFileSync(rootPath, readFileSync(rootPath, "utf8") + p.consumer.rootAppend);
  }
  const shim = join(out, "no-std-check", "Cargo.toml");
  if (!existsSync(shim))
    throw new Error(`profile ${p.id}: the tool emitted no shim at ${shim} — the emission half is broken`);
  if (p.hostStdArm) writeHostConsumer(p, out);
  return { out, specPath };
}

/**
 * The std-arm tripwire crate, written INSIDE the profile's output root and BEFORE any cell runs.
 * Both placements are cache correctness, not tidiness: the cache key's only input-of-substance is
 * `hashTree(out)`, so a consumer crate written outside `out` would be invisible to the key (editing
 * its assertion would keep serving a stale PASS), and one written after the shim cell would change
 * the tree between the two cells and miss forever on re-runs.
 *
 * `MapHashBuilder` is a cfg pair — `RandomState` under `feature = "std"`,
 * `hashlink::DefaultHashBuilder` without it — and the thumb cell exercises ONLY the not(std) arm.
 * This signature type-checks exactly while the std arm is still `RandomState`, so a silent hasher
 * flip fails here and nowhere else in the repo.
 */
function writeHostConsumer(p: Profile, out: string): void {
  const dir = join(out, "host-consumer");
  mkdirSync(join(dir, "src"), { recursive: true });
  writeFileSync(join(dir, "Cargo.toml"), [
    "[package]", `name = "${p.libName}-host-consumer"`, 'version = "0.0.0"', 'edition = "2024"', "publish = false", "",
    // A RELATIVE path dep: an absolute one would embed this run's mkdtemp root in a file the tree
    // hash covers, making every run's key unique — the same trap the argv normalization avoids.
    "[dependencies]", `${p.libName} = { path = "../rust" }`, "", "[workspace]", "",
  ].join("\n"));
  const libCode = p.libName.replace(/-/g, "_");
  writeFileSync(join(dir, "src", "lib.rs"), [
    "//! Written by the no_std_check gate: the std-arm hasher assertion.",
    `use ${libCode}::ordered_hash_map::MapHashBuilder;`,
    "",
    "pub fn _h(h: MapHashBuilder) -> std::collections::hash_map::RandomState {",
    "    h",
    "}",
    "",
  ].join("\n"));
}

function reportCell(cell: string, r: CellVerdict & { cached: boolean; ms: number; stderr?: string }): boolean {
  if (r.cached) return true;
  if (r.ok) {
    console.log(`  ${cell}: PASS  [${(r.ms / 1000).toFixed(1)}s]`);
    return true;
  }
  console.log(`  ${cell}: FAIL — ${r.why}  [${(r.ms / 1000).toFixed(1)}s]`);
  for (const l of r.detail ?? []) console.log(`      ${l}`);
  console.log(`  ---- full stderr for ${cell} ----`);
  console.log(r.stderr ?? "");
  console.log(`  ---- end stderr for ${cell} ----`);
  return false;
}

export function runNoStdCheckGate(tier: string): NoStdOutcome {
  console.log(`  target: ${TARGET}   profiles: ${[...PROFILES.map(p => p.id), SPLIT_ID].join(", ")}`);

  if (!targetInstalled()) {
    for (const l of loudSkipMessage()) console.log(l);
    if (tier === "full")
      return { status: "FAIL", reason: `${TARGET} not installed (a silent skip voids the attribution guarantee)` };
    return { status: "SKIPPED", reason: `${TARGET} not installed` };
  }

  const cargoTarget = scratch("target");
  let ok = true;
  try {
    for (const p of PROFILES) {
      const { out } = generateProfile(p);
      const rustManifest = join(out, "rust", "Cargo.toml");
      const shimManifest = join(out, "no-std-check", "Cargo.toml");
      // The shim cell, in one of its two directions. `depthLimitRefusal` inverts the verdict rather
      // than skipping the cell: "this crate must NOT build without std" is a property with the same
      // regression risk as its opposite, and the only way to state it is a cell that fails on green.
      const shimName = p.depthLimitRefusal ? "shim_thumb_expect_fail" : "shim_thumb";
      const shimCell = cacheableCell(
        `${p.id}.${shimName}`,
        ["cargo", "check", "--manifest-path", shimManifest, "--target", TARGET],
        [shimManifest],
        out,
        { CARGO_TARGET_DIR: cargoTarget },
        p.depthLimitRefusal ? acceptDepthLimitRefusal : acceptClean,
      );
      if (!reportCell(`${p.id}.${shimName}`, shimCell)) ok = false;

      if (p.depthLimitRefusal) {
        // The other half of the contract: the refusal is confined to `not(feature = "std")`. `std`
        // is default-on, so a plain host check of the SAME crate must be clean — otherwise the
        // `compile_error!`'s cfg is wrong and the flag broke every ordinary consumer.
        const hostCell = cacheableCell(
          `${p.id}.host_default_check`,
          ["cargo", "check", "--manifest-path", rustManifest],
          [rustManifest],
          out,
          { CARGO_TARGET_DIR: cargoTarget },
        );
        if (!reportCell(`${p.id}.host_default_check`, hostCell)) ok = false;
      }

      if (p.hostTestNoStd) {
        // `--lib` is not tidiness — it is what makes this cell possible. The generated crate
        // declares `crate-type = ["cdylib", "rlib"]`, and a cdylib is LINKED on a host target; with
        // `--no-default-features` the crate is `#![no_std]`, so that link wants a `#[global_allocator]`
        // and a `#[panic_handler]` and fails before any test runs. (On the no-std TARGET cargo drops
        // the cdylib with a warning instead, which is why the shim cells never meet this.) `--lib`
        // builds only the lib TEST binary, which links std through the test module's own restore.
        const testCell = cacheableCell(
          `${p.id}.host_test_nostd`,
          ["cargo", "test", "--manifest-path", rustManifest, "--no-default-features", "--lib"],
          [rustManifest],
          out,
          { CARGO_TARGET_DIR: cargoTarget },
        );
        if (!reportCell(`${p.id}.host_test_nostd`, testCell)) ok = false;
      }

      if (p.hostStdArm) {
        const hostManifest = join(out, "host-consumer", "Cargo.toml");
        const hostCell = cacheableCell(
          `${p.id}.host_std_arm`,
          ["cargo", "check", "--manifest-path", hostManifest],
          [hostManifest],
          out,
          { CARGO_TARGET_DIR: cargoTarget },
        );
        if (!reportCell(`${p.id}.host_std_arm`, hostCell)) ok = false;
      }
    }
    if (!runSplitProfile(cargoTarget)) ok = false;
  } catch (e) {
    console.log(`  ${GATE}: ${(e as Error).message}`);
    ok = false;
  }

  if (GATE_CACHE_ENABLED) console.log(`${GATE} gate-cache: ${stats.run} run, ${stats.cached} cached`);
  cleanupScratch(!ok);
  return ok ? { status: "PASS" } : { status: "FAIL", reason: "one or more no-std-check cells failed" };
}

// ---- standalone entry point ----------------------------------------------------------------------
if (import.meta.main) {
  // Same rule check.ts's `main()` uses: first non-`--` argv token is the tier, default `local`.
  const tier = process.argv.slice(2).find(a => !a.startsWith("--")) ?? "local";
  const outcome = runNoStdCheckGate(tier);
  console.log(`${GATE}: ${outcome.status}${outcome.reason ? ` (${outcome.reason})` : ""}`);
  process.exit(outcome.status === "FAIL" ? 1 : 0);
}
