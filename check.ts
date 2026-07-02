#!/usr/bin/env bun
/**
 * check.ts — the single entry point for "run everything that verifies this repo".
 *
 *   bun run check.ts            # `ci`   tier (default) — exactly what a PR would face
 *   bun run check.ts quick      # fast inner loop (fmt + clippy + snapshot tests)
 *   bun run check.ts full       # `ci` + every manual-only gate (the real "run all tests")
 *
 * The CI workflow (`.github/workflows/build.yml`) is feature-frozen for cost, so "run everything"
 * lives here instead of in CI. The registry below IS the encoding of every gate that verifies this
 * repo; the tiers (quick ⊂ ci ⊂ full) are views over it. Honest reporting is the point: every run
 * prints the FULL registry as a table (PASS / FAIL / SKIPPED / STUB / not-in-tier + durations), so a
 * gate that did not run is always *visibly* not-run — the current failure mode (a manual gate that
 * exists but is in nobody's habit) becomes impossible to miss.
 *
 * Flags:
 *   --keep-going    run all in-tier gates even after a failure (default: fail fast — later cargo
 *                   gates depend on the build, so stopping early avoids cascade noise).
 *   --skip-missing  downgrade a missing verify.ts oracle from FAIL to SKIPPED(oracle absent).
 *   --refresh-fuzz  re-run fuzz/generate.sh before the fuzz compile-rot check even if generated/ exists.
 *
 * SELF-COMPLETENESS (the systematic catch, TDD): the first gate `self_checks` runs three meta-checks
 * so a new gate that nobody registers fails the run rather than silently not existing:
 *   1. ignored-test classification — every `#[ignore]` test must be registered here as either a
 *      manual gate (run it) or a known-failing stub (never run it, shown as STUB).
 *   2. matrix-script coverage    — every `cddl-matrix/*.ts` (minus lib.ts) must be wired to a gate.
 *   3. build.yml mirror (WARN)   — each ci-tier command string must still appear in build.yml.
 *
 * Meta-checks mutation-verified red-first at landing (repo idiom):
 *   - adding a throwaway `#[ignore]` test           -> meta-check 1 FAILED (unclassified ignore)
 *   - adding a throwaway `cddl-matrix/throwaway.ts`  -> meta-check 2 FAILED (script wired to no gate)
 *   both canaries reverted after confirming red.
 */
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";

const ROOT = import.meta.dir;
const MATRIX = join(ROOT, "cddl-matrix");

// ---- tiers ---------------------------------------------------------------------------------------
const TIERS = ["quick", "ci", "full"] as const;
type Tier = (typeof TIERS)[number];
const rank = (t: Tier) => TIERS.indexOf(t);

// ---- gate model ----------------------------------------------------------------------------------
type Status = "PASS" | "FAIL" | "SKIPPED" | "STUB" | "NOT_IN_TIER";
interface Outcome { status: Status; reason?: string }
interface Opts { skipMissing: boolean; refreshFuzz: boolean }
interface Gate {
  id: string;
  tier: Tier;
  kind: "cmd" | "fn" | "stub";
  desc: string;
  cmd?: string[];            // kind === "cmd"
  cwd?: string;              // kind === "cmd"; defaults to ROOT
  run?: (o: Opts) => Outcome;// kind === "fn"
  ignoredTest?: string;      // maps this gate to a `#[ignore]` test (meta-check 1)
  script?: string;           // cddl-matrix/*.ts this gate drives (meta-check 2)
  mirror?: string;           // verbatim build.yml substring this ci gate replicates (meta-check 3)
}

// ---- process helpers -----------------------------------------------------------------------------
function sh(cmd: string[], cwd = ROOT, env?: Record<string, string>): number {
  const r = Bun.spawnSync(cmd, {
    cwd,
    env: env ? { ...process.env, ...env } : process.env,
    stdout: "inherit",
    stderr: "inherit",
    stdin: "inherit",
  });
  return r.exitCode ?? 1;
}

// ---- oracle resolution (mirrors cddl-matrix/verify.ts so the preflight can't disagree with it) ---
function resolveRubyCddl(): string | null {
  if (process.env.RUBY_CDDL) return existsSync(process.env.RUBY_CDDL) ? process.env.RUBY_CDDL : null;
  try {
    const r = Bun.spawnSync(["ruby", "-e", "puts Gem.user_dir"], { stdout: "pipe", stderr: "ignore" });
    const cand = join((r.stdout?.toString() ?? "").trim(), "bin", "cddl"); // where the `cddl` gem installs
    if (existsSync(cand)) return cand;
  } catch { /* ruby not installed */ }
  return null;
}
function resolveRustCddl(): string | null {
  const p = process.env.RUST_CDDL ?? resolve(homedir(), "Documents/git/cddl/target/debug/cddl");
  return existsSync(p) ? p : null;
}

// ---- meta-check 1 input: the suite's own #[ignore] list ------------------------------------------
function ignoredTestsFromCargo(): string[] {
  const r = Bun.spawnSync(
    ["cargo", "test", "--all-features", "--all-targets", "--", "--ignored", "--list"],
    { cwd: ROOT, stdout: "pipe", stderr: "inherit" },
  );
  const names: string[] = [];
  for (const line of (r.stdout?.toString() ?? "").split("\n")) {
    const m = line.match(/^(\S+):\s*test\s*$/); // e.g. "tests::integration_tests::ir_conformance_corpus: test"
    if (m) names.push(m[1].split("::").pop()!);
  }
  return names;
}

// ---- the three self-completeness meta-checks (first gate) ----------------------------------------
function runSelfChecks(): Outcome {
  const problems: string[] = [];
  const warnings: string[] = [];

  // 1. ignored-test classification: the cargo #[ignore] set must equal registry(manual) ∪ registry(stub).
  const ignored = new Set(ignoredTestsFromCargo());
  if (ignored.size === 0)
    problems.push("meta-1: `cargo test -- --ignored --list` yielded zero tests (build broken or output drift) — cannot classify");
  const manual = new Set(REGISTRY.filter(g => g.ignoredTest && g.kind !== "stub").map(g => g.ignoredTest!));
  const stubs = new Set(REGISTRY.filter(g => g.kind === "stub").map(g => g.ignoredTest!));
  const classified = new Set<string>([...manual, ...stubs]);
  for (const n of ignored)
    if (!classified.has(n))
      problems.push(`meta-1: #[ignore] test '${n}' is unclassified — register it in check.ts as a manual gate or a known-failing stub`);
  for (const n of classified)
    if (ignored.size > 0 && !ignored.has(n))
      problems.push(`meta-1: registered ignored-test '${n}' is no longer #[ignore] in the suite — update the registry`);

  // 2. matrix-script coverage: every cddl-matrix/*.ts (except the shared lib) must be wired to a gate.
  const scripts = readdirSync(MATRIX).filter(f => f.endsWith(".ts") && f !== "lib.ts");
  const referenced = new Set(REGISTRY.map(g => g.script).filter(Boolean) as string[]);
  for (const s of scripts)
    if (!referenced.has(s))
      problems.push(`meta-2: cddl-matrix/${s} is wired into no gate — add it to a tier in the registry (or justify its exclusion)`);

  // 3. build.yml mirror (WARN only): the ci tier duplicates the frozen file, the one allowed drift risk.
  const yml = readFileSync(join(ROOT, ".github/workflows/build.yml"), "utf8");
  for (const g of REGISTRY)
    if (g.mirror && !yml.includes(g.mirror))
      warnings.push(`meta-3: ci gate '${g.id}' command not found verbatim in build.yml — drift? ('${g.mirror}')`);

  for (const w of warnings) console.log("  WARN " + w);
  if (problems.length) {
    for (const p of problems) console.log("  FAIL " + p);
    return { status: "FAIL", reason: `${problems.length} self-completeness problem(s)` };
  }
  console.log(
    `  OK — ${ignored.size} #[ignore] test(s) classified (${manual.size} manual gate(s), ${stubs.size} stub(s)), ` +
      `${scripts.length} matrix script(s) covered, ${warnings.length} mirror warning(s)`,
  );
  return { status: "PASS" };
}

// ---- verify.ts gate: oracle preflight + run ------------------------------------------------------
function runVerify(o: Opts): Outcome {
  const ruby = resolveRubyCddl();
  const rust = resolveRustCddl();
  const missing: string[] = [];
  if (!ruby) missing.push("ruby `cddl`   install: gem install --user-install cddl");
  if (!rust) missing.push("rust `cddl`   install: cargo install cddl   (then set RUST_CDDL=~/.cargo/bin/cddl, or point it at your build)");
  if (missing.length) {
    console.log("  oracle preflight FAILED — cddl-matrix/verify.ts needs both oracles:");
    for (const m of missing) console.log("    - " + m);
    if (o.skipMissing) return { status: "SKIPPED", reason: "oracle absent (--skip-missing)" };
    return { status: "FAIL", reason: `missing oracle(s): ${!ruby ? "ruby cddl " : ""}${!rust ? "rust cddl" : ""}`.trim() };
  }
  console.log(`  oracles OK — ruby=${ruby}  rust=${rust}`);
  const exit = sh(["bun", "run", "verify.ts"], MATRIX, { RUST_CDDL: rust!, RUBY_CDDL: ruby! });
  return exit === 0 ? { status: "PASS" } : { status: "FAIL", reason: `verify.ts exit ${exit}` };
}

// ---- fuzz compile-rot gate: (re)generate iff needed, then cargo check the fuzz crate -------------
function runFuzz(o: Opts): Outcome {
  const gen = join(ROOT, "fuzz", "generated");
  if (!existsSync(gen) || o.refreshFuzz) {
    console.log(`  ${existsSync(gen) ? "--refresh-fuzz" : "fuzz/generated absent"} -> running fuzz/generate.sh`);
    const g = sh(["bash", "fuzz/generate.sh"], ROOT);
    if (g !== 0) return { status: "FAIL", reason: `fuzz/generate.sh exit ${g}` };
  }
  const exit = sh(["cargo", "check", "--manifest-path", "fuzz/Cargo.toml"], ROOT);
  return exit === 0 ? { status: "PASS" } : { status: "FAIL", reason: `cargo check (fuzz) exit ${exit}` };
}

// ==================================================================================================
// THE REGISTRY — one entry per gate. Execution order is registry order; a run at tier T executes
// every non-stub gate whose tier rank <= rank(T). quick ⊂ ci ⊂ full holds by construction.
//   - `self_checks` (quick) runs FIRST so a mis-registered gate fails before anything expensive.
//   - the `ci`-tier gates replicate build.yml's two jobs (test job, then matrix-drift job) IN ORDER;
//     `mirror` pins the verbatim command so meta-check 3 warns on drift.
//   - `snapshot_quick` is the quick-tier inner loop; in ci/full it runs additionally (cheap, ~5s) and
//     is subsumed by the full `cargo test`. The runner's own additions over the raw build.yml union
//     are exactly: self_checks + snapshot_quick (both cheap, both quick-tier).
// ==================================================================================================
const REGISTRY: Gate[] = [
  { id: "self_checks", tier: "quick", kind: "fn", run: runSelfChecks,
    desc: "self-completeness meta-checks (ignored-test + matrix-script coverage + build.yml mirror)" },

  // --- quick tier (fast inner loop) ---
  { id: "fmt", tier: "quick", kind: "cmd", cmd: ["cargo", "fmt", "--all", "--", "--check"],
    mirror: "cargo fmt --all -- --check", desc: "rustfmt check" },
  { id: "clippy", tier: "quick", kind: "cmd",
    cmd: ["cargo", "clippy", "--locked", "--workspace", "--all-features", "--all-targets", "--", "--deny", "clippy::all"],
    mirror: "cargo clippy --locked --workspace --all-features --all-targets", desc: "clippy (deny all)" },
  { id: "snapshot_quick", tier: "quick", kind: "cmd", cmd: ["cargo", "test", "--bin", "cddl-codegen", "snapshot_tests"],
    desc: "golden snapshot tests (in-process, fast)" },

  // --- ci tier: build.yml `test` job, in order ---
  { id: "build", tier: "ci", kind: "cmd", cmd: ["cargo", "build", "--locked", "--workspace", "--all-features", "--all-targets"],
    mirror: "cargo build --locked --workspace --all-features --all-targets", desc: "workspace build" },
  { id: "test", tier: "ci", kind: "cmd", cmd: ["cargo", "test", "--all-features", "--all-targets"],
    mirror: "cargo test --all-features --all-targets", desc: "full test suite (incl. corpus + wasm-matrix compile gates)" },
  { id: "insta_orphan", tier: "ci", kind: "cmd",
    cmd: ["cargo", "insta", "test", "--unreferenced=reject", "--", "snapshot_tests", "robustness"],
    mirror: "cargo insta test --unreferenced=reject -- snapshot_tests robustness", desc: "snapshot orphan check" },

  // --- ci tier: build.yml `matrix-drift` job, in order (cwd = cddl-matrix/) ---
  { id: "build_matrix_check", tier: "ci", kind: "cmd", cmd: ["bun", "run", "build_matrix.ts", "--check"], cwd: MATRIX,
    script: "build_matrix.ts", mirror: "bun run build_matrix.ts --check", desc: "matrix.json matches authored overlay" },
  { id: "project_robustness_check", tier: "ci", kind: "cmd", cmd: ["bun", "run", "project_robustness.ts", "--check"], cwd: MATRIX,
    script: "project_robustness.ts", mirror: "bun run project_robustness.ts --check", desc: "robustness fixtures drift gate" },
  { id: "project_wasm_matrix_check", tier: "ci", kind: "cmd", cmd: ["bun", "run", "project_wasm_matrix.ts", "--check"], cwd: MATRIX,
    script: "project_wasm_matrix.ts", mirror: "bun run project_wasm_matrix.ts --check", desc: "wasm-ABI matrix fixtures drift gate" },
  { id: "project_golden_hex_check", tier: "ci", kind: "cmd", cmd: ["bun", "run", "project_golden_hex.ts", "--check"], cwd: MATRIX,
    script: "project_golden_hex.ts", mirror: "bun run project_golden_hex.ts --check", desc: "golden_hex COVERAGE.md drift gate" },
  { id: "project_corpus", tier: "ci", kind: "cmd", cmd: ["bun", "run", "project_corpus.ts"], cwd: MATRIX,
    script: "project_corpus.ts", mirror: "bun run project_corpus.ts", desc: "corpus overlay validator + COVERAGE.md rewrite" },
  { id: "coverage_md_diff", tier: "ci", kind: "cmd", cmd: ["git", "diff", "--exit-code", "tests/corpus/COVERAGE.md"], cwd: ROOT,
    mirror: "git diff --exit-code tests/corpus/COVERAGE.md", desc: "tests/corpus/COVERAGE.md is up to date" },

  // --- full tier: the manual-only gates (run by memory today; the whole point of this runner) ---
  { id: "wasm_matrix_roundtrips", tier: "full", kind: "cmd",
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "wasm_matrix_roundtrips", "--", "--ignored"],
    ignoredTest: "wasm_matrix_roundtrips", desc: "wasm-ABI matrix round-trip gate (manual, #[ignore]d)" },
  { id: "ir_conformance_corpus", tier: "full", kind: "cmd",
    cmd: ["cargo", "test", "--bin", "cddl-codegen", "ir_conformance_corpus", "--", "--ignored", "--nocapture"],
    ignoredTest: "ir_conformance_corpus", desc: "IR-bug conformance oracle at corpus breadth (manual, #[ignore]d)" },
  { id: "verify", tier: "full", kind: "fn", run: runVerify, script: "verify.ts",
    desc: "cddl-matrix mechanical verify gate (oracle preflight + probe every feature)" },
  { id: "corpus_detect", tier: "full", kind: "cmd", cmd: ["bun", "run", "corpus_detect.ts"], cwd: MATRIX,
    script: "corpus_detect.ts", desc: "corpus_detect featuresIn/rolesIn self-checks" },
  { id: "fuzz_compile_rot", tier: "full", kind: "fn", run: runFuzz,
    desc: "fuzz crate compile-rot check (generate.sh iff needed, then cargo check)" },

  // --- tracked-failing IOU stubs: known-failing #[ignore] tests, NEVER executed (shown as STUB) ---
  { id: "stub_wasm_optional_nullable_field_three_state_fidelity", tier: "full", kind: "stub",
    ignoredTest: "wasm_optional_nullable_field_three_state_fidelity",
    desc: "IOU: wasm optional-nullable field three-state fidelity (generator-side flatten loss)" },
  { id: "stub_wasm_enum_nullable_variant_three_state_fidelity", tier: "full", kind: "stub",
    ignoredTest: "wasm_enum_nullable_variant_three_state_fidelity",
    desc: "IOU: wasm double-nested enum-variant three-state fidelity (as_variant getter skipped)" },
  { id: "stub_preserve_encodings_supports_floats", tier: "full", kind: "stub",
    ignoredTest: "preserve_encodings_supports_floats",
    desc: "IOU: floats under --preserve-encodings (pre-existing unimplemented! in generation.rs)" },
];

// ==================================================================================================
// RUN
// ==================================================================================================
function fmtDur(ms: number): string {
  if (ms < 1000) return `${Math.round(ms)}ms`;
  const s = ms / 1000;
  if (s < 60) return `${s.toFixed(1)}s`;
  const m = Math.floor(s / 60);
  return `${m}m ${Math.round(s - m * 60)}s`;
}

function statusCell(o: Outcome): string {
  switch (o.status) {
    case "PASS": return "PASS";
    case "FAIL": return "FAIL" + (o.reason ? ` (${o.reason})` : "");
    case "SKIPPED": return "SKIPPED" + (o.reason ? ` (${o.reason})` : "");
    case "STUB": return "STUB (tracked failing)";
    case "NOT_IN_TIER": return "not-in-tier";
  }
}

function main() {
  const argv = process.argv.slice(2);
  const flags = new Set(argv.filter(a => a.startsWith("--")));
  const positional = argv.filter(a => !a.startsWith("--"));
  const KNOWN = new Set(["--keep-going", "--skip-missing", "--refresh-fuzz", "--help"]);
  for (const f of flags)
    if (!KNOWN.has(f)) { console.error(`check.ts: unknown flag '${f}' (known: ${[...KNOWN].join(", ")})`); process.exit(2); }
  if (flags.has("--help")) {
    console.log("usage: bun run check.ts [quick|ci|full] [--keep-going] [--skip-missing] [--refresh-fuzz]");
    console.log("  bare invocation runs the `ci` tier. See the header of check.ts for details.");
    process.exit(0);
  }
  let tier: Tier = "ci";
  if (positional.length) {
    if (!TIERS.includes(positional[0] as Tier)) {
      console.error(`check.ts: unknown tier '${positional[0]}' (expected: ${TIERS.join(", ")})`);
      process.exit(2);
    }
    tier = positional[0] as Tier;
  }
  const keepGoing = flags.has("--keep-going");
  const opts: Opts = { skipMissing: flags.has("--skip-missing"), refreshFuzz: flags.has("--refresh-fuzz") };

  console.log(`\ncheck.ts — tier=${tier}${keepGoing ? " --keep-going" : ""}${opts.skipMissing ? " --skip-missing" : ""}${opts.refreshFuzz ? " --refresh-fuzz" : ""}`);

  const results = new Map<string, { out: Outcome; ms: number }>();
  let anyFail = false;
  let aborted = false;
  const wall0 = performance.now();

  // ponytail: sequential v1 — one gate at a time so per-gate durations are honest and a failure's
  // output isn't interleaved. If wall-time ever bites, the independent read-only drift checks
  // (build_matrix / project_* --check, coverage_md_diff) can run in parallel beside the `test` gate;
  // keep the fmt→clippy→build→test cargo chain sequential (later gates depend on the build).
  for (const g of REGISTRY) {
    if (g.kind === "stub") { results.set(g.id, { out: { status: "STUB" }, ms: 0 }); continue; }
    if (rank(g.tier) > rank(tier)) { results.set(g.id, { out: { status: "NOT_IN_TIER" }, ms: 0 }); continue; }
    if (aborted) { results.set(g.id, { out: { status: "SKIPPED", reason: "earlier failure; fail-fast" }, ms: 0 }); continue; }

    console.log(`\n=== [${g.tier}] ${g.id} — ${g.desc} ===`);
    const t0 = performance.now();
    let out: Outcome;
    if (g.kind === "cmd") {
      const e = sh(g.cmd!, g.cwd ?? ROOT);
      out = e === 0 ? { status: "PASS" } : { status: "FAIL", reason: `exit ${e}` };
    } else {
      out = g.run!(opts);
    }
    const ms = performance.now() - t0;
    results.set(g.id, { out, ms });
    console.log(`--- ${g.id}: ${out.status}${out.reason ? ` (${out.reason})` : ""}  [${fmtDur(ms)}]`);
    if (out.status === "FAIL") { anyFail = true; if (!keepGoing) aborted = true; }
  }

  // ---- always-printed full-registry summary --------------------------------------------------
  const wall = performance.now() - wall0;
  const idW = Math.max(...REGISTRY.map(g => g.id.length), 4);
  const tierW = 5;
  const line = "-".repeat(idW + tierW + 10 + 40);
  console.log("\n" + line);
  console.log(`SUMMARY — tier=${tier}  wall=${fmtDur(wall)}`);
  console.log(line);
  console.log(`${"GATE".padEnd(idW)}  ${"TIER".padEnd(tierW)}  ${"TIME".padEnd(8)}  STATUS`);
  console.log(line);
  for (const g of REGISTRY) {
    const r = results.get(g.id)!;
    const time = r.out.status === "PASS" || r.out.status === "FAIL" || (r.out.status === "SKIPPED" && r.ms > 0)
      ? fmtDur(r.ms) : "-";
    console.log(`${g.id.padEnd(idW)}  ${g.tier.padEnd(tierW)}  ${time.padEnd(8)}  ${statusCell(r.out)}`);
  }
  console.log(line);

  const fails = REGISTRY.filter(g => results.get(g.id)!.out.status === "FAIL").map(g => g.id);
  if (fails.length) {
    console.log(`RESULT: FAIL — ${fails.length} gate(s) failed: ${fails.join(", ")}`);
    process.exit(1);
  }
  console.log(`RESULT: PASS — all in-tier gates green (tier=${tier})`);
  process.exit(0);
}

main();
