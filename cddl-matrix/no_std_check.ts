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
const VERDICT_MARKER = "no-std-check-v1";
const CARGO_TIMEOUT_S = 900;

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
 * One cacheable cargo cell. `manifests` are the manifests THIS cell actually checks — each gets a
 * `cargo generate-lockfile` before the tree is hashed, so a lockfile cargo would write during the
 * run cannot change the tree out from under its own key.
 */
function cacheableCell(
  cell: string, argv: string[], manifests: string[], treeRoot: string, env: Record<string, string>,
): { exit: number; cached: boolean; stderr: string; ms: number } {
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
      return { exit: 0, cached: true, stderr: "", ms: 0 };
    }
  }
  touchTree(treeRoot);
  stats.run++;
  const t0 = performance.now();
  const r = run(argv, CODEGEN_DIR, env);
  const ms = performance.now() - t0;
  if (r.exit === 0 && key && entryBase)
    writeGateCacheEntry(key, { ...entryBase, cell, created: new Date().toISOString() }, CODEGEN_DIR);
  return { exit: r.exit, cached: false, stderr: r.stderr, ms };
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
    // The raw-bytes runtime — `RawBytesEncoding`, the `FromHexErrorCore` newtype and BOTH `hex::`
    // call sites — which the snapshot corpus structurally never emits. Note the runtime is
    // CONCATENATED INTO `generated/serialization.rs`; there is no separate `raw_bytes_encoding.rs`
    // in the output to look for.
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
    id: "json_schema",
    libName: "nostd-json",
    flags: ["--preserve-encodings=true", "--json-serde-derives=true", "--json-schema-export=true"],
    cddl: ["inner = [ a: uint, b: text ]", "tbl = { * uint => text }", "outer = [ i: inner, t: tbl, ? o: bytes ]", ""].join("\n"),
  },
];

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

function reportCell(cell: string, r: { exit: number; cached: boolean; stderr: string; ms: number }): boolean {
  if (r.cached) return true;
  const bad = disallowedWarnings(r.stderr);
  if (r.exit === 0 && bad.length === 0) {
    console.log(`  ${cell}: PASS  [${(r.ms / 1000).toFixed(1)}s]`);
    return true;
  }
  if (r.exit !== 0) console.log(`  ${cell}: FAIL — cargo exit ${r.exit}  [${(r.ms / 1000).toFixed(1)}s]`);
  else {
    console.log(`  ${cell}: FAIL — ${bad.length} warning(s) outside the allowed set  [${(r.ms / 1000).toFixed(1)}s]`);
    for (const l of bad) console.log(`      ${l}`);
  }
  console.log(`  ---- full stderr for ${cell} ----`);
  console.log(r.stderr);
  console.log(`  ---- end stderr for ${cell} ----`);
  return false;
}

export function runNoStdCheckGate(tier: string): NoStdOutcome {
  console.log(`  target: ${TARGET}   profiles: ${PROFILES.map(p => p.id).join(", ")}`);

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
      const shimManifest = join(out, "no-std-check", "Cargo.toml");
      const shimCell = cacheableCell(
        `${p.id}.shim_thumb`,
        ["cargo", "check", "--manifest-path", shimManifest, "--target", TARGET],
        [shimManifest],
        out,
        { CARGO_TARGET_DIR: cargoTarget },
      );
      if (!reportCell(`${p.id}.shim_thumb`, shimCell)) ok = false;

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
