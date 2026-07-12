#!/usr/bin/env bun
/**
 * Gate-cache INPUT-CLOSURE AUDIT (soundness gate, check.ts `full` tier: `gate_cache_closure_audit`).
 *
 * The gate cache (`tests/README.md` § "The gate cache (memoize-and-skip for nested cargo)";
 * `src/tests/gate_cache.rs`; `cddl-matrix/lib.ts` gate-cache half) skips a nested cargo invocation
 * when a sha256 key over its input closure matches a previously-GREEN run. That skip is only sound
 * if the key HASHES everything the nested cargo actually CONSUMES. This gate is the mechanical
 * guardrail this repo uses INSTEAD of the industry "nightly cold run": it traces a real cached gate
 * under `strace -f`, attributes every successful file-content read to the process subtree it
 * happened in, and asserts every read made by a NESTED-CARGO subtree (the work the cache can skip)
 * falls into an allowed class the key provably covers.
 *
 * Allowed read classes for a nested-cargo subtree (FAIL on anything else):
 *   | class                                   | allowed because                                          |
 *   |-----------------------------------------|----------------------------------------------------------|
 *   | under $TMPDIR (mkdtemp scratch / the    | the generated tree is hashed (`hashTree`), `target/`     |
 *   | gate's cddl_codegen_* scratch root)     | excluded by cargo's own correctness contract             |
 *   | under $CARGO_HOME (default ~/.cargo)    | dep sources pinned by the hashed Cargo.lock              |
 *   |                                         | (generate-lockfile before key computation); registry    |
 *   |                                         | files content-addressed by version+checksum              |
 *   | under $RUSTUP_HOME (default ~/.rustup)  | pinned by the `rustc -vV` key component                 |
 *   | system prefixes (/usr /lib* /etc /proc  | machine state (per-checkout-local cache, never shared) — |
 *   | /sys /dev /opt /run /bin /sbin)         | same accepted class as glibc/the linker                  |
 *   | user git config — EXACTLY the two files | cargo consults git config for URL rewriting/transport    |
 *   | $HOME/.gitconfig and $XDG_CONFIG_HOME   | during registry/git-dep access, so it can affect whether |
 *   | (default ~/.config)/git/config — NOT a  | FETCHING succeeds, but not WHAT is built: the key hashes |
 *   | widened $HOME class (/etc/gitconfig is  | the `cargo generate-lockfile` output, and Cargo.lock     |
 *   | already under the system class)         | pins versions AND content checksums, so any bytes a      |
 *   |                                         | rewritten URL could serve are checksum-fenced — given    |
 *   |                                         | the same hashed lockfile the verdict is git-config-      |
 *   |                                         | independent. Stacking: the cache is per-checkout-local,  |
 *   |                                         | never shared, so user-level machine state sits in the    |
 *   |                                         | same acceptance class as CARGO_HOME config and the       |
 *   |                                         | toolchain's system libs. (Empirical: every nested cargo  |
 *   |                                         | reads ~/.gitconfig at startup — 124/124 subtrees on the  |
 *   |                                         | first audited trace.)                                    |
 *
 * A successful read under the REPO CHECKOUT is the headline finding — that is exactly "a cached site
 * grew an unhashed input" (a path-dep append, a build script reading a repo file, a
 * `.cargo/config.toml` materializing). A read under $HOME outside CARGO_HOME/RUSTUP_HOME/the two
 * user git-config files, or any path in no allowed class, likewise FAILs. The fix for a genuine finding is to HASH the input into
 * the key at that call site (never to allowlist it), or — only with a written soundness argument —
 * to justify a new allow class HERE.
 *
 * Traced representative (v1): `multifile_matrix_compiles` (env `CLOSURE_AUDIT_GATE=<test name>`
 * overrides). Chosen because its nested `cargo check` on the generated `wasm/` crate transitively
 * builds the `../rust` PATH DEP — the highest-risk read pattern — and `run_cached`'s
 * `cargo generate-lockfile` preflight is in the traced set too. Extending coverage
 * (`feature_corpus_compiles` for the cargo-test + json-gen shapes, the roundtrip/recombination
 * sites, the TS-side verify.ts sites) is CONFIGURATION via CLOSURE_AUDIT_GATE, not code. NOT yet
 * traced in v1: everything but the configured gate — notably the TS-side cached sites in verify.ts
 * (whose nested cargo runs with cwd = the repo, so a repo cargo config would be an unhashed input);
 * the static `.cargo/config` assert below is the standing guard for that TS-side hole until the
 * strace leg covers it.
 *
 * Mechanics: the trace forces every cell to MISS (fresh `GATE_CACHE_DIR` mkdtemp) so the nested work
 * — incl. the lockfile preflight — actually RUNS and is auditable, without touching the real
 * `.gate-cache/`. Membership: a pid is in a nested-cargo subtree iff its NEAREST ancestor-or-self
 * that execve'd a qualifying cargo (subcommand in {test, check, build, generate-lockfile}) is NOT
 * the traced ROOT command. The root is itself a `cargo test`, so it is excluded by construction; the
 * generator's `cargo run` is not qualifying, so the generator's repo reads (static/, fixtures) are
 * excluded via the same nearest-ancestor rule; rustc/test-binary children inherit their subtree
 * root's membership. Deliberately, the nested `cargo test`'s generated TEST BINARY is INSIDE a
 * nested subtree — an emitted test that reads an external file at runtime is exactly a finding.
 *
 * Scope caveats (documented, not silent): only successful reads with an fd >= 0 and a `-y` path
 * annotation are counted; `O_DIRECTORY`/`O_PATH` opens are excluded (not file-content reads); a
 * resumed open (flags split across the trace) is conservatively counted as a read; ENOENT probes are
 * not flagged. Deterministic output: offenders and per-class census are sorted.
 *
 * Exit: 0 PASS · 1 FAIL (a nested-subtree read in no allowed class) · 2 HARNESS (strace absent —
 * visible SKIPPED — / trace died / a repo cargo config exists / ZERO nested-cargo subtrees traced,
 * the vacuity floor: the audit must refuse to pass if it traced nothing).
 *
 * Run from cddl-matrix/:  bun run audit_gate_cache_closure.ts   (or CLOSURE_AUDIT_GATE=<test>).
 * `--self-test` runs only the embedded parser fixtures and exits (no cargo, no strace).
 */
import { existsSync, mkdtempSync, readdirSync, realpathSync } from "node:fs";
import { homedir, tmpdir } from "node:os";
import { basename, join, resolve } from "node:path";

const ROOT = import.meta.dir;
const CODEGEN_DIR = resolve(ROOT, ".."); // the cddl-codegen repo this script lives in
const GATE = process.env.CLOSURE_AUDIT_GATE ?? "multifile_matrix_compiles";
const QUALIFYING_SUBCOMMANDS = new Set(["test", "check", "build", "generate-lockfile"]);

// ==================================================================================================
// PURE PARSER (unit-tested via --self-test embedded fixtures below)
// ==================================================================================================
export interface ExecEvent { pid: number; argv: string[]; }
export interface ReadEvent { pid: number; path: string; flags: string; }
export interface ProcModel {
  parent: Map<number, number>;
  argv: Map<number, string[]>;
  rootPid: number | null;
}

// Extract the argv array from an execve line: `PID execve("/path", ["a", "b", ...], 0x... ) = 0`.
// The unfinished half (`... <unfinished ...>`) still carries the argv, so we accept it regardless of
// the `= 0` (exec of cargo/rustc effectively always succeeds; a failed exec spawns nothing to audit).
export function parseExec(line: string): ExecEvent | null {
  const pidM = line.match(/^(\d+)\s+(?:<\.\.\.\s+)?execve\(/);
  if (!pidM) return null;
  const arrM = line.match(/execve\("(?:[^"\\]|\\.)*",\s*\[([\s\S]*?)\](?:,|\s*<unfinished)/);
  if (!arrM) return null;
  const argv = [...arrM[1].matchAll(/"((?:[^"\\]|\\.)*)"/g)].map(m => m[1]);
  if (argv.length === 0) return null;
  return { pid: parseInt(pidM[1], 10), argv };
}

// Extract a (parent -> child) edge from a successful clone/fork/vfork/clone3, finished or resumed.
export function parseClone(line: string): { parent: number; child: number } | null {
  const direct = line.match(/^(\d+)\s+(?:clone3?|fork|vfork)\(.*\)\s*=\s*(\d+)$/);
  const resumed = line.match(/^(\d+)\s+<\.\.\.\s+(?:clone3?|fork|vfork)\s+resumed>.*=\s*(\d+)$/);
  const m = direct ?? resumed;
  if (!m) return null;
  const child = parseInt(m[2], 10);
  if (!Number.isFinite(child) || child <= 0) return null;
  return { parent: parseInt(m[1], 10), child };
}

// Extract a successful, path-annotated open/openat/openat2 read. `-y` annotates the returned fd with
// the kernel-resolved absolute path (symlinks resolved), which we prefer over relative-path math. The
// flags string is scanned from the whole syscall (empty for a resumed open, where flags were in the
// unfinished half — then treated conservatively as a content read). Requires the `= <fd></abs/path>`
// annotation; an ENOENT/negative return has none and is skipped.
export function parseRead(line: string): ReadEvent | null {
  if (!/\b(?:openat2|openat|open)\(/.test(line) && !/<\.\.\.\s+(?:openat2|openat|open)\s+resumed>/.test(line))
    return null;
  const pidM = line.match(/^(\d+)\s+/);
  if (!pidM) return null;
  const retM = line.match(/=\s+(\d+)<([^>]*)>\s*$/); // fd >= 0 with -y path annotation
  if (!retM) return null;
  const path = retM[2];
  // Only REAL filesystem paths: an fd annotation can be a pseudo-file (`pipe:[…]`, `socket:[…]`,
  // `anon_inode:[…]` — no leading `/`) or anonymous memory (`/memfd:…`); none is a file-content read.
  if (!path.startsWith("/") || path.startsWith("/memfd:")) return null;
  const flagTokens = [...line.matchAll(/\bO_[A-Z]+\b/g)].map(m => m[0]);
  return { pid: parseInt(pidM[1], 10), path, flags: flagTokens.join("|") };
}

// A content read we scrutinize: exclude directory / O_PATH opens; a write-only open is not a read; a
// resumed open (flags === "") is conservatively included (safe direction for an audit).
export function isContentRead(flags: string): boolean {
  if (flags === "") return true;
  if (flags.includes("O_DIRECTORY") || flags.includes("O_PATH")) return false;
  return flags.includes("O_RDONLY") || flags.includes("O_RDWR");
}

export function isQualifyingCargo(argv: string[] | undefined): boolean {
  if (!argv || argv.length < 2) return false;
  if (argv[0] !== "cargo" && basename(argv[0]) !== "cargo") return false;
  return QUALIFYING_SUBCOMMANDS.has(argv[1]);
}

// The nested-cargo subtree root a pid belongs to: nearest ancestor-or-self that execve'd a qualifying
// cargo, EXCLUDING the traced root pid (itself a `cargo test`, whose subtree legitimately reads the
// whole repo compiling the harness). null => the pid is not inside any nested-cargo subtree.
export function owningNestedCargo(pid: number, m: ProcModel): number | null {
  let cur: number | undefined = pid;
  const seen = new Set<number>();
  while (cur !== undefined && !seen.has(cur)) {
    seen.add(cur);
    if (cur !== m.rootPid && isQualifyingCargo(m.argv.get(cur))) return cur;
    cur = m.parent.get(cur);
  }
  return null;
}

export type PathClass =
  | { allowed: true; label: string }
  | { allowed: false; label: string };

export interface Boundaries {
  tmp: string[]; cargoHome: string[]; rustupHome: string[]; repo: string[]; home: string[];
  // EXACT file paths (not prefixes): $HOME/.gitconfig + $XDG_CONFIG_HOME(default ~/.config)/git/config.
  // Deliberately NOT a widened $HOME class; soundness argument in the header allow-table.
  userGitConfig: string[];
}
const SYSTEM_PREFIXES = ["/usr", "/lib", "/lib64", "/lib32", "/etc", "/proc", "/sys", "/dev", "/opt", "/run", "/bin", "/sbin"];

const underAny = (path: string, prefixes: string[]): boolean =>
  prefixes.some(p => p !== "" && (path === p || path.startsWith(p.endsWith("/") ? p : p + "/")));

// Order matters: tmp/cargo/rustup/user-git-config are checked before `repo`/`home` (they may live
// under $HOME); `repo` before the system + home-generic classes so a repo path never masquerades as
// either. userGitConfig is an EXACT-file match, never a prefix.
export function classifyPath(path: string, b: Boundaries): PathClass {
  if (underAny(path, b.tmp)) return { allowed: true, label: "tmp/scratch" };
  if (underAny(path, b.cargoHome)) return { allowed: true, label: "cargo_home" };
  if (underAny(path, b.rustupHome)) return { allowed: true, label: "rustup_home" };
  if (b.userGitConfig.includes(path)) return { allowed: true, label: "user_git_config" };
  if (underAny(path, b.repo)) return { allowed: false, label: "REPO_CHECKOUT" };
  if (underAny(path, SYSTEM_PREFIXES)) return { allowed: true, label: "system" };
  if (underAny(path, b.home)) return { allowed: false, label: "HOME_OUTSIDE_CARGO_RUSTUP" };
  return { allowed: false, label: "UNCLASSIFIED" };
}

// ==================================================================================================
// SELF-TEST (embedded strace fixtures — the no-new-frameworks equivalent of a unit suite; runs before
// the real trace so a parser regression fails loud without a multi-minute strace run first).
// ==================================================================================================
function selfTest(): void {
  const fail = (msg: string): never => { console.error(`self-test FAILED: ${msg}`); process.exit(2); };
  const eq = (a: unknown, b: unknown, msg: string) => { if (JSON.stringify(a) !== JSON.stringify(b)) fail(`${msg}: got ${JSON.stringify(a)} want ${JSON.stringify(b)}`); };

  // exec parsing (finished + unfinished)
  eq(parseExec(`100 execve("/usr/bin/cargo", ["cargo", "test", "--bin", "x"], 0x7ff /* 40 vars */) = 0`)?.argv,
     ["cargo", "test", "--bin", "x"], "exec finished");
  eq(parseExec(`101 execve("/root/.cargo/bin/cargo", ["cargo", "check"] <unfinished ...>`)?.argv,
     ["cargo", "check"], "exec unfinished");
  eq(parseExec(`102 openat(AT_FDCWD, "x")`), null, "non-exec line");

  // clone parsing (finished + resumed), reject failures
  eq(parseClone(`100 clone(child_stack=NULL, flags=CLONE_VM) = 200`), { parent: 100, child: 200 }, "clone finished");
  eq(parseClone(`100 <... clone3 resumed> => {parent_tid=[201]}, 88) = 201`), { parent: 100, child: 201 }, "clone3 resumed");
  eq(parseClone(`100 clone(...) = -1 EAGAIN (err)`), null, "clone failed");

  // read parsing: only annotated successful fds; flags scanned; ENOENT skipped
  eq(parseRead(`200 openat(AT_FDCWD</cwd>, "/etc/hosts", O_RDONLY|O_CLOEXEC) = 3</etc/hosts>`),
     { pid: 200, path: "/etc/hosts", flags: "O_RDONLY|O_CLOEXEC" }, "openat read");
  eq(parseRead(`200 openat(AT_FDCWD, "/nope", O_RDONLY) = -1 ENOENT (No such file or directory)`), null, "enoent skipped");
  eq(parseRead(`200 <... openat resumed> ) = 7</tmp/scratch/a>`)?.path, "/tmp/scratch/a", "resumed open");
  eq(parseRead(`200 openat(AT_FDCWD, "x", O_RDONLY) = 5<pipe:[12345]>`), null, "pseudo-fd (pipe) skipped");
  eq(parseRead(`200 openat(AT_FDCWD, "x", O_RDONLY) = 5</memfd:foo>`), null, "memfd skipped");

  // content-read filter
  if (!isContentRead("O_RDONLY|O_CLOEXEC")) fail("rdonly is a read");
  if (isContentRead("O_WRONLY|O_CREAT")) fail("wronly is not a read");
  if (isContentRead("O_RDONLY|O_DIRECTORY")) fail("directory open excluded");
  if (!isContentRead("")) fail("resumed (empty flags) conservatively a read");

  // qualifying-cargo detection
  if (!isQualifyingCargo(["cargo", "generate-lockfile", "--manifest-path", "x"])) fail("generate-lockfile qualifies");
  if (!isQualifyingCargo(["/home/u/.cargo/bin/cargo", "check"])) fail("abs-path cargo qualifies");
  if (isQualifyingCargo(["cargo", "run", "--", "--input=x"])) fail("cargo run does NOT qualify");
  if (isQualifyingCargo(["rustc", "--edition=2018"])) fail("rustc does not qualify");

  // membership: root cargo test excluded; nested check included; generator cargo run excluded.
  const model: ProcModel = {
    rootPid: 1,
    parent: new Map([[2, 1], [3, 2], [4, 2], [5, 4], [6, 3]]),
    argv: new Map<number, string[]>([
      [1, ["cargo", "test", "--bin", "cddl-codegen", GATE]], // root (excluded even though `test`)
      [2, ["cddl-codegen", "--test-binary"]],                // the test binary
      [3, ["cargo", "run", "--", "--input=x"]],              // generator — NOT qualifying
      [4, ["cargo", "check"]],                               // nested check — qualifies
      [5, ["rustc", "x"]],                                   // child of nested check — inherits
      [6, ["rustc", "x"]],                                   // child of generator — excluded
    ]),
  };
  eq(owningNestedCargo(1, model), null, "root excluded");
  eq(owningNestedCargo(2, model), null, "test binary not nested");
  eq(owningNestedCargo(3, model), null, "generator cargo run excluded");
  eq(owningNestedCargo(6, model), null, "rustc under generator excluded");
  eq(owningNestedCargo(4, model), 4, "nested check is its own root");
  eq(owningNestedCargo(5, model), 4, "rustc under nested check inherits");

  // classification (allowed vs FAIL classes)
  const b: Boundaries = {
    tmp: ["/tmp"], cargoHome: ["/home/u/.cargo"], rustupHome: ["/home/u/.rustup"],
    repo: ["/home/u/git/cddl-codegen"], home: ["/home/u"],
    userGitConfig: ["/home/u/.gitconfig", "/home/u/.config/git/config"],
  };
  eq(classifyPath("/tmp/cddl_codegen_x/y", b).allowed, true, "tmp allowed");
  eq(classifyPath("/home/u/.cargo/registry/z", b).allowed, true, "cargo_home allowed");
  eq(classifyPath("/home/u/.rustup/toolchains/z", b).allowed, true, "rustup allowed");
  eq(classifyPath("/usr/lib/x.so", b).allowed, true, "system allowed");
  eq(classifyPath("/home/u/.gitconfig", b), { allowed: true, label: "user_git_config" }, "user gitconfig allowed");
  eq(classifyPath("/home/u/.config/git/config", b), { allowed: true, label: "user_git_config" }, "xdg git config allowed");
  eq(classifyPath("/home/u/.gitconfig.bak", b).label, "HOME_OUTSIDE_CARGO_RUSTUP", "gitconfig sibling NOT allowed (exact match only)");
  eq(classifyPath("/home/u/.config/git/config.d/x", b).label, "HOME_OUTSIDE_CARGO_RUSTUP", "git config subpath NOT allowed (exact match only)");
  eq(classifyPath("/home/u/git/cddl-codegen/README.md", b), { allowed: false, label: "REPO_CHECKOUT" }, "repo FAIL");
  eq(classifyPath("/home/u/secrets", b).label, "HOME_OUTSIDE_CARGO_RUSTUP", "home-outside FAIL");
  eq(classifyPath("/var/random", b).label, "UNCLASSIFIED", "unknown FAIL");

  console.log("self-test OK (parser + membership + classification fixtures pass)");
}

// ==================================================================================================
// STREAMING FILE READER (the trace can be multi-GB; never slurp it whole)
// ==================================================================================================
async function forEachLine(path: string, cb: (line: string) => void): Promise<void> {
  const stream = Bun.file(path).stream();
  const decoder = new TextDecoder();
  let buf = "";
  for await (const chunk of stream) {
    buf += decoder.decode(chunk, { stream: true });
    let nl: number;
    while ((nl = buf.indexOf("\n")) >= 0) {
      cb(buf.slice(0, nl));
      buf = buf.slice(nl + 1);
    }
  }
  buf += decoder.decode();
  if (buf.length) cb(buf);
}

// ==================================================================================================
// MAIN
// ==================================================================================================
function realpathOr(p: string): string { try { return realpathSync(p); } catch { return p; } }

// De-dup prefix set: literal + realpath'd (handles /tmp being a symlink, ~/.cargo etc.).
function boundaryPrefixes(p: string): string[] {
  return [...new Set([resolve(p), realpathOr(p)])];
}

// Static companion assert: the repo checkout must contain no `.cargo/config(.toml)?` (outside
// target/). TS-side cached sites run nested cargo with cwd = the repo, so cargo's config discovery
// walks the repo — a repo cargo config would be an unhashed verdict-affecting input the v1 strace leg
// (Rust-side only) cannot see. If this ever fires, HASH the config into the key (all sites), do not
// allowlist. Bounded walk; skips target/ and .git/.
function findRepoCargoConfigs(dir: string, acc: string[]): void {
  let entries;
  try { entries = readdirSync(dir, { withFileTypes: true }); } catch { return; }
  for (const e of entries) {
    if (e.isDirectory()) {
      if (e.name === "target" || e.name === ".git" || e.name === "node_modules") continue;
      if (e.name === ".cargo") {
        for (const f of ["config.toml", "config"]) if (existsSync(join(dir, ".cargo", f))) acc.push(join(dir, ".cargo", f));
        continue; // no cargo config dir nesting to recurse into
      }
      findRepoCargoConfigs(join(dir, e.name), acc);
    }
  }
}

async function main(): Promise<void> {
  selfTest();
  if (process.argv.includes("--self-test")) process.exit(0);

  // Preflight 1: strace resolvable — else a VISIBLE skip (never a silent pass), matching --skip-missing.
  const strace = Bun.which("strace");
  if (!strace) {
    console.log("SKIPPED (strace absent) — install strace to run the gate-cache input-closure audit.");
    process.exit(0);
  }

  // Preflight 2: static repo cargo-config assert.
  const configs: string[] = [];
  findRepoCargoConfigs(CODEGEN_DIR, configs);
  if (configs.length) {
    console.error("HARNESS FAILURE: the repo checkout contains a cargo config, an unhashed verdict-affecting input for the TS-side cached sites (nested cargo runs with cwd = the repo):");
    for (const c of configs.sort()) console.error(`  - ${c}`);
    console.error("Fix: hash the config into the gate-cache key at ALL call sites — do NOT allowlist it here.");
    process.exit(2);
  }

  // Is the traced gate #[ignore]d? (cargo lists an ignored test only under `-- --ignored --list`.)
  const listed = Bun.spawnSync(
    ["cargo", "test", "--bin", "cddl-codegen", GATE, "--", "--ignored", "--list"],
    { cwd: CODEGEN_DIR, stdout: "pipe", stderr: "inherit" },
  );
  const ignoredList = listed.stdout?.toString() ?? "";
  const isIgnored = new RegExp(`(?:^|::)${GATE}: test\\b`, "m").test(ignoredList);

  const scratch = mkdtempSync(join(tmpdir(), "gate_cache_audit_"));
  const straceLog = join(scratch, "closure.strace");
  const gateCacheDir = mkdtempSync(join(tmpdir(), "gate_cache_audit_cachedir_")); // forces MISS everywhere

  const cargoArgs = ["cargo", "test", "--bin", "cddl-codegen", GATE, "--", ...(isIgnored ? ["--ignored"] : [])];
  const straceArgs = [
    strace, "-f", "-e", "trace=%process,openat,openat2,open", "-y", "-s", "4096",
    "-o", straceLog, "--", ...cargoArgs,
  ];
  console.log(`tracing gate '${GATE}'${isIgnored ? " (#[ignore]d -> --ignored)" : ""} under strace -f ...`);
  console.log(`  ${cargoArgs.join(" ")}`);
  console.log(`  GATE_CACHE_DIR=${gateCacheDir} (fresh -> forces every cell to MISS)`);
  const run = Bun.spawnSync(straceArgs, {
    cwd: CODEGEN_DIR,
    env: { ...process.env, GATE_CACHE_DIR: gateCacheDir },
    stdout: "inherit", stderr: "inherit",
  });
  if (!existsSync(straceLog)) {
    console.error(`HARNESS FAILURE: strace produced no log at ${straceLog} (exit ${run.exitCode}).`);
    process.exit(2);
  }
  if (run.exitCode !== 0) {
    // A red gate under trace is a broken environment for the audit, not a closure finding — the audit
    // cannot vouch for the closure of a run that didn't pass. Refuse to pass (do not claim green).
    console.error(`HARNESS FAILURE: traced gate '${GATE}' exited ${run.exitCode} (not a closure finding — the gate itself must be green before its closure can be audited).`);
    process.exit(2);
  }

  // Pass 1: process tree + execve argv + root pid (first pid to appear).
  const model: ProcModel = { parent: new Map(), argv: new Map(), rootPid: null };
  await forEachLine(straceLog, (line) => {
    const pidM = line.match(/^(\d+)\s/);
    if (pidM && model.rootPid === null) model.rootPid = parseInt(pidM[1], 10);
    const ex = parseExec(line);
    if (ex) { model.argv.set(ex.pid, ex.argv); return; }
    const cl = parseClone(line);
    if (cl && !model.parent.has(cl.child)) model.parent.set(cl.child, cl.parent);
  });

  // Vacuity floor: at least one nested-cargo subtree (a qualifying cargo pid other than the root).
  const nestedRoots = new Set<number>();
  for (const [pid, argv] of model.argv)
    if (pid !== model.rootPid && isQualifyingCargo(argv)) nestedRoots.add(pid);
  if (nestedRoots.size === 0) {
    console.error(`HARNESS FAILURE: traced gate '${GATE}' spawned ZERO nested-cargo subtrees (qualifying subcommands: ${[...QUALIFYING_SUBCOMMANDS].sort().join(", ")}). The audit refuses to pass on a vacuous trace — point CLOSURE_AUDIT_GATE at a gate that runs nested cargo.`);
    process.exit(2);
  }

  const b: Boundaries = {
    tmp: boundaryPrefixes(process.env.TMPDIR || tmpdir()),
    cargoHome: boundaryPrefixes(process.env.CARGO_HOME || join(homedir(), ".cargo")),
    rustupHome: boundaryPrefixes(process.env.RUSTUP_HOME || join(homedir(), ".rustup")),
    repo: boundaryPrefixes(CODEGEN_DIR),
    home: boundaryPrefixes(homedir()),
    userGitConfig: [
      ...boundaryPrefixes(join(homedir(), ".gitconfig")),
      ...boundaryPrefixes(join(process.env.XDG_CONFIG_HOME || join(homedir(), ".config"), "git", "config")),
    ],
  };
  // /tmp is also a valid scratch root even when TMPDIR points elsewhere (the Rust gates use
  // std::env::temp_dir()); include it so a differing TMPDIR doesn't spuriously flag /tmp scratch.
  b.tmp = [...new Set([...b.tmp, ...boundaryPrefixes("/tmp")])];

  // Pass 2: classify every content read made from inside a nested-cargo subtree.
  const classCounts = new Map<string, number>();
  const uniquePaths = new Map<string, Set<string>>();
  interface Offender { path: string; label: string; pid: number; ownerArgv: string; }
  const offenders = new Map<string, Offender>();
  let nestedReads = 0;
  await forEachLine(straceLog, (line) => {
    const rd = parseRead(line);
    if (!rd || !isContentRead(rd.flags)) return;
    const owner = owningNestedCargo(rd.pid, model);
    if (owner === null) return; // not inside a nested-cargo subtree
    nestedReads++;
    const cls = classifyPath(rd.path, b);
    classCounts.set(cls.label, (classCounts.get(cls.label) ?? 0) + 1);
    if (!uniquePaths.has(cls.label)) uniquePaths.set(cls.label, new Set());
    uniquePaths.get(cls.label)!.add(rd.path);
    if (!cls.allowed) {
      const ownerArgv = (model.argv.get(owner) ?? []).join(" ");
      const dedup = `${rd.path}\u0000${owner}`;
      if (!offenders.has(dedup)) offenders.set(dedup, { path: rd.path, label: cls.label, pid: rd.pid, ownerArgv });
    }
  });

  // Deterministic census.
  const labels = [...classCounts.keys()].sort();
  const census = labels.map(l => `${l}=${classCounts.get(l)} (${uniquePaths.get(l)!.size} unique)`).join(", ");
  console.log(`\n${"=".repeat(80)}`);
  console.log(`gate-cache input-closure audit — gate '${GATE}'`);
  console.log("=".repeat(80));
  console.log(`nested-cargo subtrees traced : ${nestedRoots.size}`);
  console.log(`reads from nested subtrees    : ${nestedReads}`);
  console.log(`class census                  : ${census || "(none)"}`);

  if (offenders.size) {
    console.error(`\nFAIL: ${offenders.size} nested-cargo read(s) in NO allowed class — a cached site consumed an unhashed input:`);
    // Group by class, headline class first — a REPO_CHECKOUT finding must never be buried under a
    // hundred entries of another class by a flat display cap. Deterministic within a class: sorted
    // by (path, pid); per-class cap.
    const LABEL_ORDER = ["REPO_CHECKOUT", "HOME_OUTSIDE_CARGO_RUSTUP", "UNCLASSIFIED"];
    const labelRank = (l: string) => { const i = LABEL_ORDER.indexOf(l); return i === -1 ? LABEL_ORDER.length : i; };
    const byLabel = new Map<string, Offender[]>();
    for (const o of offenders.values()) {
      if (!byLabel.has(o.label)) byLabel.set(o.label, []);
      byLabel.get(o.label)!.push(o);
    }
    const SHOW_PER_CLASS = 25;
    for (const label of [...byLabel.keys()].sort((a, c) => labelRank(a) - labelRank(c) || (a < c ? -1 : 1))) {
      const group = byLabel.get(label)!.sort((a, c) => (a.path < c.path ? -1 : a.path > c.path ? 1 : a.pid - c.pid));
      console.error(`  [${label}] — ${group.length} offending read(s), ${new Set(group.map(o => o.path)).size} unique path(s):`);
      for (const o of group.slice(0, SHOW_PER_CLASS))
        console.error(`  - ${o.path}\n      pid ${o.pid}, owned by nested cargo: ${o.ownerArgv}`);
      if (group.length > SHOW_PER_CLASS) console.error(`  ... and ${group.length - SHOW_PER_CLASS} more in this class`);
    }
    console.error(`\nFix: hash the offending input into the gate-cache key at that call site (or, only WITH a written soundness argument, justify a new allow class in ${basename(import.meta.path)}).`);
    process.exit(1);
  }

  console.log(`\nRESULT: PASS — every nested-cargo read falls in a key-covered class.`);
  process.exit(0);
}

main();
