/**
 * pin_cold_fetch.ts — every git rev asserted by a committed pin-carrying file resolves from the
 * REMOTE, with no local state consulted.
 *
 * # What it asserts
 *
 * Every `git = "<url>" … rev = "<sha>"` pin MENTIONED in the committed pin-carrying surfaces —
 * `static/manifest_changes/*.toml` (the manifest source of truth), the derived `static/Cargo_*.toml`
 * views, and the repo root `Cargo.toml` — names a commit the remote actually serves. Mentions, not
 * the effective last-write-wins fold, deliberately: a superseded entry still asserting an
 * unresolvable rev is a live trap (delete or reorder the shadowing entry and the phantom silently
 * returns), and checking mentions also subsumes any same-path consistency lint — which would in any
 * case be the weaker gate, since the original cycle-2 defect was ONE rev, mentioned consistently
 * everywhere, that was simply never pushed. Unresolvability is the defect; this gate tests it
 * directly and catches both shapes.
 *
 * # Why this gate is deliberately ONLINE (do not make it offline for speed)
 *
 * This being the tier's one online gate is exactly why the defect class survived: the tier's
 * warm-up→offline design means a warm local cargo DB answers "does this rev exist?" wrongly and
 * confidently — a rev that was never pushed resolves fine from the machine that staged it, which is
 * precisely how the 2026-08 phantom pin passed three cycles of green gates before a cold-cache
 * check caught it. The entire question this gate asks is "does the REMOTE serve this rev"; any
 * answer derived from local state (cargo DB, git alternates, a cache of a previous PASS) is the
 * failure mode, not an optimization. For the same reason this gate does NOT use the gate cache: its
 * verdict can change with zero tree change (a force-pushed or deleted branch), so a cached PASS is
 * meaningless.
 *
 * # The two failure classes are structurally separated (different steps, different exit codes)
 *
 * An online gate whose transient failures read as pin defects teaches everyone to re-run, and the
 * gate stops meaning anything — which is how a phantom survives a second time. So:
 *
 *  1. REACHABILITY (per remote): `git ls-remote <url> HEAD`, retried. Failure → exit 2,
 *     "environment" — network/DNS/auth/rate-limit; NOT a pin defect. Expected occasionally (this is
 *     the one gate that needs the network); the remedy is fix connectivity / re-run, never a pin
 *     change. It still fails the run: a pass without the network would let a phantom ship from an
 *     offline box.
 *  2. RESOLUTION (per pin): `git fetch --depth 1 <url> <sha>` into a scratch bare repo. GitHub
 *     serves any reachable full SHA and answers "not our ref" otherwise. On failure, step 1 is
 *     re-run to re-classify: remote now unreachable → exit 2 (environment died mid-run); remote
 *     still reachable → exit 1, PIN DEFECT, naming the rev, every file:line that mentions it, and
 *     stating plainly that the commit does not exist on the remote.
 *
 * git rather than cargo for the probe: git ignores `CARGO_NET_OFFLINE` (which the tier sets after
 * warm-up), needs no synthesized crate or scratch CARGO_HOME, and a depth-1 SHA fetch is the exact
 * question. `GIT_TERMINAL_PROMPT=0` so an auth misconfiguration fails instead of hanging the tier.
 *
 * # Full-SHA policy
 *
 * A `rev = "…"` that is not a full 40-hex SHA fails as a pin defect too: short revs cannot be
 * fetched by SHA-in-want, and an abbreviated pin is one object-collision away from ambiguity.
 *
 * Standalone-invocable: `bun run cddl-matrix/pin_cold_fetch.ts`.
 */
import { mkdtempSync, readFileSync, readdirSync, rmSync } from "node:fs";
import { join, resolve } from "node:path";
import { tmpdir } from "node:os";
import { spawnSync } from "node:child_process";

const ROOT = import.meta.dir;
const CODEGEN_DIR = resolve(ROOT, "..");
const GATE = "pin_cold_fetch";

/** The committed pin-carrying surfaces. A new surface that grows a git pin belongs here. */
function pinSurfaces(): string[] {
  const changesets = readdirSync(join(CODEGEN_DIR, "static", "manifest_changes"))
    .filter(f => f.endsWith(".toml"))
    .map(f => join("static", "manifest_changes", f));
  const derived = readdirSync(join(CODEGEN_DIR, "static"))
    .filter(f => /^Cargo_.*\.toml$/.test(f))
    .map(f => join("static", f));
  return [...changesets, ...derived, "Cargo.toml"];
}

interface Mention { file: string; line: number; url: string; rev: string }

/** Every line-level `git = "<url>" … rev = "<hex>"` pair; malformed rev lines are defects, not skips. */
function collectMentions(): { mentions: Mention[]; malformed: string[] } {
  const mentions: Mention[] = [];
  const malformed: string[] = [];
  for (const rel of pinSurfaces()) {
    const lines = readFileSync(join(CODEGEN_DIR, rel), "utf8").split("\n");
    lines.forEach((text, i) => {
      if (!/\brev\s*=\s*"/.test(text)) return;
      const m = text.match(/\bgit\s*=\s*"([^"]+)"[^\n]*?\brev\s*=\s*"([0-9a-fA-F]+)"/);
      if (m && /^[0-9a-f]{40}$/.test(m[2])) {
        mentions.push({ file: rel, line: i + 1, url: m[1], rev: m[2] });
      } else {
        malformed.push(`${rel}:${i + 1}: ${text.trim()}`);
      }
    });
  }
  return { mentions, malformed };
}

const GIT_ENV = { ...process.env, GIT_TERMINAL_PROMPT: "0" };
function git(args: string[], timeoutMs: number): { ok: boolean; detail: string } {
  const r = spawnSync("git", args, { env: GIT_ENV, timeout: timeoutMs, encoding: "utf8" });
  const detail = [r.stderr?.trim(), r.error ? String(r.error) : ""].filter(Boolean).join(" | ");
  return { ok: r.status === 0, detail };
}

/** Step 1: is the remote reachable at all? Retried — transients are this step's whole job. */
function remoteReachable(url: string): { ok: boolean; detail: string } {
  let last = { ok: false, detail: "" };
  for (let attempt = 1; attempt <= 3; attempt++) {
    last = git(["ls-remote", url, "HEAD"], 30_000);
    if (last.ok) return last;
  }
  return last;
}

function environmentFail(url: string, detail: string): never {
  console.error(`${GATE}: FAIL (environment) — could not reach ${url}`);
  console.error(`  This is NOT a pin defect. This gate is the tier's one deliberately-online gate,`);
  console.error(`  so occasional environment failures here are expected. Check network/DNS/proxy,`);
  console.error(`  GitHub availability/rate limits, and git auth (GIT_TERMINAL_PROMPT is disabled`);
  console.error(`  so a credential prompt fails instead of hanging). Fix the environment and`);
  console.error(`  re-run; do not change any pin in response to this failure.`);
  if (detail) console.error(`  git said: ${detail}`);
  process.exit(2);
}

function main() {
  const { mentions, malformed } = collectMentions();
  if (malformed.length > 0) {
    console.error(`${GATE}: FAIL (pin defect) — rev pin(s) that are not a full 40-hex SHA:`);
    for (const m of malformed) console.error(`  ${m}`);
    process.exit(1);
  }
  if (mentions.length === 0) {
    // Self-check: the surfaces are known to carry at least one pin today; zero means the scan broke.
    console.error(`${GATE}: FAIL — scanned ${pinSurfaces().length} surfaces and found no git pins;`);
    console.error(`  the scan regex or the surface list has drifted from the pin format.`);
    process.exit(1);
  }

  const byPin = new Map<string, Mention[]>();
  for (const m of mentions) {
    const k = `${m.url}#${m.rev}`;
    byPin.set(k, [...(byPin.get(k) ?? []), m]);
  }
  const urls = [...new Set(mentions.map(m => m.url))];

  for (const url of urls) {
    const r = remoteReachable(url);
    if (!r.ok) environmentFail(url, r.detail);
  }

  const scratch = mkdtempSync(join(tmpdir(), "cddl-pin-cold-fetch-"));
  try {
    const init = git(["init", "--bare", "--quiet", scratch], 30_000);
    if (!init.ok) environmentFail("(local git init)", init.detail);
    for (const [key, where] of byPin) {
      const [url, rev] = key.split("#");
      let fetch = git(["-C", scratch, "fetch", "--depth", "1", url, rev], 120_000);
      if (!fetch.ok) fetch = git(["-C", scratch, "fetch", "--depth", "1", url, rev], 120_000); // one retry
      if (!fetch.ok) {
        // Re-classify: did the environment die mid-run, or does the remote not have the commit?
        if (!remoteReachable(url).ok) environmentFail(url, fetch.detail);
        console.error(`${GATE}: FAIL (pin defect) — the remote does not serve rev ${rev}`);
        console.error(`  ${url} is reachable, but fetching this commit fails — the commit does not`);
        console.error(`  exist on the remote (never pushed, or its branch was deleted/force-pushed).`);
        console.error(`  Every green build that resolved it did so from a warm local cargo/git DB;`);
        console.error(`  a fresh checkout cannot build. Re-pin to a commit reachable from a remote`);
        console.error(`  branch (and if the mention is a superseded changeset entry, correct it in`);
        console.error(`  place — static/manifest_changes/README.md, "Correcting a never-valid value").`);
        console.error(`  Mentioned at:`);
        for (const m of where) console.error(`    ${m.file}:${m.line}`);
        if (fetch.detail) console.error(`  git said: ${fetch.detail}`);
        process.exit(1);
      }
      console.log(`  ${rev.slice(0, 12)}… @ ${url} — resolves (${where.length} mention(s))`);
    }
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
  console.log(`${GATE}: PASS — ${byPin.size} unique pin(s), ${mentions.length} mention(s), every rev served by its remote`);
}

main();
