/**
 * no_silent_directive.ts — the directive×shape silent-drop net.
 *
 * The comment-DSL directives (`@newtype`, `@duplicates`, …) each change codegen, but the
 * directive×shape cross-product has no systematic coverage — the directive-level analog of the
 * ledgered "escaped flag-interaction" class. Three shipped gaps at the `@newtype` wrapper seam shared
 * one symptom: a written directive produced output BYTE-IDENTICAL to omitting it, with no notice or
 * rejection acknowledging it (a silent drop). This gate is the systematic catch that would have flagged
 * all three without predicting them.
 *
 * Mechanism — for each (base shape, toggled directive) cell in the small fixed corpus below, generate
 * the built binary TWICE (rust-only, into throwaway scratch dirs): once with the base directives only,
 * once with the toggled directive ADDED. A cell is a silent drop — FAIL — iff:
 *   (a) both runs succeeded and produced BYTE-IDENTICAL generated source, AND
 *   (b) the with-directive run's stdout+stderr contains no mention of the directive (no notice, no
 *       rejection) that would acknowledge it.
 * A byte DIFFERENCE (the directive changed output) or a nonzero with-directive exit (the directive was
 * loudly rejected) is a PASS — the directive was honored, not silenced. Legitimate byte-identical
 * accepted no-ops (an explicit `@duplicates preserve` on a non-258 array where preserve is already the
 * default; an explicit `@duplicates reject` on a 258 set where reject is already the default) live on
 * the visible ALLOWLIST below, which doubles as the accepted-no-op inventory — each entry carries a
 * one-line justification.
 *
 * Tier: `local` (never `fast`/CI — CI cost policy). Kept tiny + fast (a handful of one-rule specs).
 * No `--check` mode: this gate has no drift artifact, it just runs.
 */
import { mkdtempSync, rmSync, existsSync, readdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";

const HERE = import.meta.dir;
const CODEGEN_DIR = resolve(HERE, "..");
const STATIC_DIR = join(CODEGEN_DIR, "static");
const BIN = join(CODEGEN_DIR, "target", "debug", "cddl-codegen");

/** A directive×shape cell: the toggled directive is APPENDED to `base` (which may already carry some). */
interface Cell {
  id: string;
  /** the rule body, e.g. `#6.258([* uint])` */
  ruleBody: string;
  /** directives always present (both runs), e.g. `["@newtype"]` */
  base: string[];
  /** the directive under test, ADDED for the second run, e.g. `@duplicates reject` */
  toggled: string;
  /** human shape description for the FAIL message */
  shape: string;
}

// The corpus. The first two cells reproduce shipped wrapper-seam gaps (each byte-identical
// with/without the toggled directive BEFORE the Phase-2.1 fixes, distinct AFTER); the third pins that a
// custom `@newtype <name>` getter is honored on a nominalized two-arm 258 set (Phase 2.2 subsumed the
// gap-3 rejection — bare `@newtype` on a set nominal is now an accepted no-op, allowlisted below);
// the last two are the allowlisted accepted-no-op controls that prove the gate does not simply pass
// everything.
const CORPUS: Cell[] = [
  {
    id: "single_arm_258_newtype_preserve_optout",
    ruleBody: "#6.258([* uint])",
    base: ["@newtype"],
    toggled: "@duplicates preserve",
    shape: "single-arm #6.258 array @newtype wrapper",
  },
  {
    id: "plain_newtype_reject",
    ruleBody: "[* uint]",
    base: ["@newtype"],
    toggled: "@duplicates reject",
    shape: "plain [* a] @newtype wrapper",
  },
  {
    // Phase 2.2: the two-arm 258 idiom nominalizes; a custom `@newtype <name>` getter is honored on the
    // set nominal (adds `pub fn entries(..)`), so toggling it changes output. (Bare `@newtype` on a set
    // nominal is a no-op — no getter, to avoid the Deref shadow — covered by the allowlist cell below.)
    id: "two_arm_258_idiom_newtype_named_getter",
    ruleBody: "#6.258([* uint]) / [* uint]",
    base: [],
    toggled: "@newtype entries",
    shape: "collapsed two-arm 258 set idiom (nominalized)",
  },
  {
    id: "plain_array_preserve",
    ruleBody: "[* uint]",
    base: [],
    toggled: "@duplicates preserve",
    shape: "plain non-258 [* a] array",
  },
  {
    id: "two_arm_258_idiom_reject",
    ruleBody: "#6.258([* uint]) / [* uint]",
    base: [],
    toggled: "@duplicates reject",
    shape: "collapsed two-arm 258 set idiom",
  },
  {
    // `@ignore` (the open struct-map tolerate-and-drop rest-row flavor) is valid ONLY on a `* k => v`
    // rest ENTRY, read from that entry's trailing comment slot. This gate's `buildRule` places the
    // toggled directive at RULE position (`foo = <body> ; @ignore`), where `@ignore` is a
    // misplacement — so this cell documents the LOUD rule-level rejection (honored-not-silenced),
    // exactly the legitimate cell shape for a directive whose only valid slot is not rule position.
    id: "open_struct_rule_position_ignore_rejected",
    ruleBody: "{ 1: uint, * uint => any }",
    base: [],
    toggled: "@ignore",
    shape: "open struct-map at rule-position @ignore (valid only on the rest entry)",
  },
  {
    // Bare `@newtype` on a nominalized 258 set is an ACCEPTED NO-OP: the set already nominalizes into a
    // wrapper, and a bare `@newtype` requests an inherent `get()` that is deliberately suppressed (it
    // would shadow `OrderedSet::get(index)` through `Deref` — E0061). So it is byte-identical with/without
    // the directive; allowlisted below as the documented no-op.
    id: "two_arm_258_idiom_bare_newtype_noop",
    ruleBody: "#6.258([* uint]) / [* uint]",
    base: [],
    toggled: "@newtype",
    shape: "collapsed two-arm 258 set idiom (bare @newtype, no getter)",
  },
];

// Legitimate byte-identical accepted no-ops: `<cellId>` => one-line justification. A cell on this list
// is EXPECTED to be byte-identical with/without its directive and to print no acknowledging notice; it
// is exempted from the FAIL condition (and doubles as the accepted-no-op inventory).
const ALLOWLIST: Record<string, string> = {
  // `@duplicates preserve` is already the default for a plain non-258 array (`Vec`), so writing it is a
  // byte-identical self-documenting no-op — the documented opt-out spelling, not a dropped directive.
  plain_array_preserve:
    "explicit @duplicates preserve on a non-258 array = today's default (Vec); byte-identical no-op",
  // `@duplicates reject` is already the registry default for a 258 set idiom (`OrderedSet`), so writing
  // it is a byte-identical self-documenting no-op; the explicit directive also suppresses the defaulting
  // clause of the collapse notice, so the with-run prints no @duplicates mention.
  two_arm_258_idiom_reject:
    "explicit @duplicates reject on a 258 set idiom = registry default (OrderedSet); byte-identical no-op",
  // A named non-generic 258 set NOMINALIZES with or without `@newtype`; a BARE `@newtype` requests an
  // inherent `get()` that is suppressed on set nominals (it would shadow `OrderedSet::get(index)` through
  // `Deref`), so it adds nothing. A custom `@newtype <name>` getter IS honored (see the positive cell).
  two_arm_258_idiom_bare_newtype_noop:
    "bare @newtype on a nominalized 258 set = no getter (suppressed to avoid the Deref shadow); byte-identical no-op",
};

/** The `@`-prefixed directive token used to detect an acknowledging notice/rejection
 *  (`@duplicates preserve` → `@duplicates`). MUST keep the leading `@`: the generator dumps its full IR
 *  to stdout, and that dump names struct fields `duplicates:` / `newtype:` (no `@`) on EVERY run — so a
 *  bare-keyword match would be vacuous (always "mentioned"). Genuine notices/rejections always spell the
 *  directive with its `@` (`defaulting to @duplicates reject`, `@newtype on rule …`), which the IR dump
 *  never does. */
function directiveKeyword(directive: string): string {
  return "@" + directive.replace(/^@/, "").split(/\s+/)[0];
}

function buildRule(cell: Cell, extra: string[]): string {
  const directives = [...cell.base, ...extra];
  const comment = directives.length ? ` ; ${directives.join(" ")}` : "";
  // A holder embedding the rule exercises member position too (the transparent-alias flatten seam).
  return `foo = ${cell.ruleBody}${comment}\nholder = [f: foo]\n`;
}

interface RunResult { exit: number; output: string; bytes: string | null }

/** Generate `spec` rust-only into a throwaway dir; return exit code, combined stdout+stderr, and the
 *  concatenated generated source (null when the run failed / produced no tree). */
function generate(spec: string): RunResult {
  const dir = mkdtempSync(join(tmpdir(), "no-silent-dir-"));
  try {
    const specPath = join(dir, "in.cddl");
    writeFileSync(specPath, spec);
    const outDir = join(dir, "out");
    const r = Bun.spawnSync(
      [BIN, `--input=${specPath}`, `--output=${outDir}`, `--static-dir=${STATIC_DIR}`, "--wasm=false"],
      { cwd: CODEGEN_DIR, stdout: "pipe", stderr: "pipe" },
    );
    const output = (r.stdout?.toString() ?? "") + (r.stderr?.toString() ?? "");
    const exit = r.exitCode ?? 1;
    let bytes: string | null = null;
    const genDir = join(outDir, "rust", "src", "generated");
    if (exit === 0 && existsSync(genDir)) {
      // Concatenate every generated .rs (sorted) — the byte-identity comparison surface. The static
      // runtime files (ordered_set.rs, serialization.rs, …) are identical between the two runs of a
      // cell, so they never mask a real per-type difference; including them is harmless and keeps the
      // surface a simple whole-tree read.
      const parts: string[] = [];
      for (const f of readdirSync(genDir).sort()) {
        const p = join(genDir, f);
        if (statSync(p).isFile() && f.endsWith(".rs")) parts.push(`// FILE ${f}\n` + readFileSync(p, "utf8"));
      }
      bytes = parts.join("\n");
    }
    return { exit, output, bytes };
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
}

function main(): number {
  if (!existsSync(BIN)) {
    // Build once (offline-safe under the runner's CARGO_NET_OFFLINE); the runner's `build` gate usually
    // has done this already, but the gate must be runnable standalone.
    const b = Bun.spawnSync(["cargo", "build", "-q", "--bin", "cddl-codegen"], { cwd: CODEGEN_DIR, stdout: "inherit", stderr: "inherit" });
    if ((b.exitCode ?? 1) !== 0 || !existsSync(BIN)) {
      console.error("no_silent_directive: could not build cddl-codegen");
      return 2;
    }
  }

  const failures: string[] = [];
  let passes = 0;
  for (const cell of CORPUS) {
    const without = generate(buildRule(cell, []));
    const withD = generate(buildRule(cell, [cell.toggled]));

    if (without.exit !== 0) {
      failures.push(`${cell.id}: BASE spec failed to generate (exit ${without.exit}) — fixture bug:\n${without.output}`);
      continue;
    }

    const keyword = directiveKeyword(cell.toggled);
    const mentioned = withD.output.toLowerCase().includes(keyword.toLowerCase());

    if (withD.exit !== 0) {
      // Loud rejection: the directive was honored (not silenced). Require it name the directive so an
      // UNRELATED failure can't masquerade as "handled".
      if (mentioned) {
        console.log(`  PASS ${cell.id}: '${cell.toggled}' loudly rejected on ${cell.shape}`);
        passes++;
      } else {
        failures.push(`${cell.id}: with-directive run failed (exit ${withD.exit}) but its output never names '${cell.toggled}' — unexpected failure, not a directive rejection:\n${withD.output}`);
      }
      continue;
    }

    const identical = without.bytes !== null && withD.bytes !== null && without.bytes === withD.bytes;
    if (!identical) {
      console.log(`  PASS ${cell.id}: '${cell.toggled}' changed generated output on ${cell.shape}`);
      passes++;
      continue;
    }
    if (mentioned) {
      console.log(`  PASS ${cell.id}: '${cell.toggled}' acknowledged by a notice on ${cell.shape} (byte-identical accepted)`);
      passes++;
      continue;
    }
    if (cell.id in ALLOWLIST) {
      console.log(`  PASS ${cell.id}: allowlisted no-op — ${ALLOWLIST[cell.id]}`);
      passes++;
      continue;
    }
    failures.push(`directive silently ignored: '${cell.toggled}' on ${cell.shape} (${cell.id}) — output byte-identical with/without it and no notice/rejection names it. Either honor the directive, reject it loudly, or (if a genuine no-op) add it to the ALLOWLIST with a justification.`);
  }

  console.log(`\nno_silent_directive: ${passes} passed, ${failures.length} failed (${CORPUS.length} cells)`);
  for (const f of failures) console.error(`  FAIL ${f}`);
  return failures.length === 0 ? 0 : 1;
}

process.exit(main());
