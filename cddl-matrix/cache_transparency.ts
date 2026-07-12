#!/usr/bin/env bun
/**
 * Gate-cache CACHE-TRANSPARENCY DIFF (soundness gate, check.ts `full` tier: `verify_cache_transparency`,
 * flag-gated by `check.ts --cache-transparency`).
 *
 * The companion of the input-closure audit (`audit_gate_cache_closure.ts`). Where the closure audit
 * checks the KEY side (everything a nested cargo reads is covered by what the key hashes), this
 * checks the OUTPUT side: a cache HIT must be byte-identical to really re-running. It asserts
 * `verify.ts`'s two written artifacts —
 *   - cddl-matrix/annotations/cddl_codegen.toml
 *   - cddl-matrix/verify_report.json
 * — are BYTE-IDENTICAL between a CACHED run (`GATE_CACHE=1`, hits actually taken) and an UNCACHED run
 * (`GATE_CACHE=0`). That is the direct check that the hit path's reconstructions (the synthesized
 * all-true verdict maps / exit-0 short-circuits in `verify.ts`/`lib.ts`) can never leak into output
 * bytes differently than real execution.
 *
 * Registered AFTER the `verify` gate so a `check.ts full --cache-transparency` run warms/refreshes the
 * cache via `verify` first and run A here is genuinely hit-heavy (registry execution is sequential, so
 * the two gates' verify invocations never interleave).
 *
 * Mechanics: run A (`GATE_CACHE=1`, the real `.gate-cache/` a prior full run warmed) — parse its
 * `gate-cache : N run, M cached` summary and REQUIRE M > 0 (an all-miss run proves nothing about the
 * hit path -> exit 2, vacuity floor). Snapshot A's artifact bytes. Run B (`GATE_CACHE=0`). Byte-compare.
 * On mismatch: FAIL, write both versions + a unified diff under the scratch dir, print the path (the
 * diff IS the finding — a reconstruction divergence or an environment flake; re-run once to rule out a
 * transient before treating it as a cache bug). A nonzero verify exit in A or B is a FAIL naming which
 * run (verify writes no verdict files on its harness-exit 2, so a diff would be vacuous); both runs
 * failing is an environment problem -> exit 2. Run B (uncached, ground-truth) artifacts are left in
 * place — identical to what a plain `verify` gate leaves — so the "fold before committing after a full
 * run" rule covers the aftermath unchanged.
 *
 * Both oracles (RUST_CDDL / RUBY_CDDL) must be resolved; the check.ts entry preflights them exactly like
 * the `verify` gate (so `--skip-missing` downgrades identically) and passes them through the env.
 *
 * Exit: 0 PASS (byte-identical) · 1 FAIL (artifacts diverge / a single verify run failed) · 2 HARNESS
 * (vacuity floor M == 0 / both runs failed / oracle env missing).
 *
 * Run from cddl-matrix/ (cost ~2 verify runs — one mostly-cached + one full):
 *   RUST_CDDL=... RUBY_CDDL=... bun run cache_transparency.ts
 */
import { existsSync, mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";

const ROOT = import.meta.dir;
const ANNO_PATH = resolve(ROOT, "annotations", "cddl_codegen.toml");
const REPORT_PATH = resolve(ROOT, "verify_report.json");
const ARTIFACTS: { name: string; path: string }[] = [
  { name: "annotations/cddl_codegen.toml", path: ANNO_PATH },
  { name: "verify_report.json", path: REPORT_PATH },
];

if (!process.env.RUST_CDDL || !existsSync(process.env.RUST_CDDL)) {
  console.error("HARNESS FAILURE: RUST_CDDL is unset or does not exist — the check.ts entry must resolve both oracles and pass them through (run via `check.ts full --cache-transparency`, or set RUST_CDDL/RUBY_CDDL to run the script directly).");
  process.exit(2);
}
if (!process.env.RUBY_CDDL || !existsSync(process.env.RUBY_CDDL)) {
  console.error("HARNESS FAILURE: RUBY_CDDL is unset or does not exist — the check.ts entry must resolve both oracles and pass them through.");
  process.exit(2);
}

const scratch = mkdtempSync(join(tmpdir(), "cache_transparency_"));
const readArtifacts = (): (string | null)[] => ARTIFACTS.map(a => (existsSync(a.path) ? readFileSync(a.path, "utf8") : null));

// Save the pre-existing artifact bytes so a both-runs-failed abort can restore the operator's prior
// state instead of leaving whatever half-run landed.
const preExisting = readArtifacts();

// Stream a verify.ts run: echo its output live (an ~11 min run must not look hung) AND capture stdout
// so the gate-cache summary line can be parsed. Returns { exitCode, stdout }.
async function runVerify(label: string, gateCache: "0" | "1"): Promise<{ exit: number; stdout: string }> {
  console.log(`\n=== cache-transparency run ${label} (GATE_CACHE=${gateCache}) ===`);
  const proc = Bun.spawn(["bun", "run", "verify.ts"], {
    cwd: ROOT,
    env: { ...process.env, GATE_CACHE: gateCache },
    stdout: "pipe",
    stderr: "inherit",
  });
  let stdout = "";
  const decoder = new TextDecoder();
  for await (const chunk of proc.stdout) {
    const text = decoder.decode(chunk, { stream: true });
    stdout += text;
    process.stdout.write(text);
  }
  stdout += decoder.decode();
  const exit = await proc.exited;
  return { exit, stdout };
}

// Parse `gate-cache          : N run, M cached` — verify.ts prints it only when the cache is enabled.
function parseCachedCount(stdout: string): number | null {
  const m = stdout.match(/^gate-cache\s*:\s*(\d+) run, (\d+) cached\s*$/m);
  return m ? parseInt(m[2], 10) : null;
}

function restorePreExisting(): void {
  for (let i = 0; i < ARTIFACTS.length; i++)
    if (preExisting[i] !== null) writeFileSync(ARTIFACTS[i].path, preExisting[i]!);
}

async function main(): Promise<void> {
  // Run A — cached path (real .gate-cache warmed by a prior verify/full run).
  const a = await runVerify("A", "1");
  const cached = parseCachedCount(a.stdout);

  if (a.exit !== 0) {
    // Distinguish a single-run failure (FAIL) from a broken environment (both fail -> exit 2).
    const b = await runVerify("B", "0");
    if (b.exit !== 0) {
      restorePreExisting();
      console.error(`\nHARNESS FAILURE: BOTH verify runs failed (A exit ${a.exit}, B exit ${b.exit}) — broken environment, not a cache finding. Restored pre-existing artifacts.`);
      process.exit(2);
    }
    console.error(`\nFAIL: the cached run A exited ${a.exit} while the uncached run B passed — verify's cached path is not equivalent to a real run.`);
    process.exit(1);
  }

  // Vacuity floor: run A must have taken at least one hit, else it proves nothing about the hit path.
  if (cached === null) {
    console.error("\nHARNESS FAILURE: run A produced no `gate-cache : N run, M cached` summary (is the gate cache disabled?) — cannot establish a hit-heavy cached run.");
    process.exit(2);
  }
  if (cached === 0) {
    console.error(`\nHARNESS FAILURE: run A took 0 cache hits (all-miss) — proves nothing about the hit path. Warm the cache first (run \`check.ts full\`, or \`bun run verify.ts\` once) and retry.`);
    process.exit(2);
  }
  console.log(`\nrun A took ${cached} cache hit(s) — proceeding to the uncached ground-truth run.`);
  const snapshotA = readArtifacts();

  // Run B — uncached ground truth.
  const b = await runVerify("B", "0");
  if (b.exit !== 0) {
    console.error(`\nFAIL: the uncached run B exited ${b.exit} while the cached run A passed — cannot compare (B is the ground truth).`);
    process.exit(1);
  }
  const snapshotB = readArtifacts();

  // Byte-compare each artifact.
  const mismatches: string[] = [];
  for (let i = 0; i < ARTIFACTS.length; i++) {
    const name = ARTIFACTS[i].name;
    if (snapshotA[i] === null || snapshotB[i] === null) {
      mismatches.push(`${name}: MISSING after a run (A ${snapshotA[i] === null ? "absent" : "present"}, B ${snapshotB[i] === null ? "absent" : "present"})`);
      continue;
    }
    if (snapshotA[i] !== snapshotB[i]) {
      const aFile = join(scratch, `A.${i}.${name.replace(/\W+/g, "_")}`);
      const bFile = join(scratch, `B.${i}.${name.replace(/\W+/g, "_")}`);
      writeFileSync(aFile, snapshotA[i]!);
      writeFileSync(bFile, snapshotB[i]!);
      const diff = Bun.spawnSync(["diff", "-u", aFile, bFile], { stdout: "pipe", stderr: "pipe" });
      const diffFile = join(scratch, `diff.${i}.${name.replace(/\W+/g, "_")}.patch`);
      writeFileSync(diffFile, diff.stdout?.toString() ?? "");
      mismatches.push(`${name}: BYTES DIFFER (cached A != uncached B)\n      A: ${aFile}\n      B: ${bFile}\n      unified diff: ${diffFile}`);
    }
  }

  if (mismatches.length) {
    console.error(`\nFAIL: ${mismatches.length} artifact(s) diverge between the cached (A) and uncached (B) run:`);
    for (const m of mismatches) console.error(`  - ${m}`);
    console.error(`\nThe diff IS the finding — a cache reconstruction divergence, or an environment flake. Re-run once to rule out a transient before treating it as a cache bug.`);
    process.exit(1);
  }

  console.log(`\nRESULT: PASS — both artifacts byte-identical between the cached (${cached} hit(s)) and uncached run. The hit path reconstructs verdicts transparently.`);
  process.exit(0);
}

main();
