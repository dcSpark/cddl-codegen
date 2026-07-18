#!/usr/bin/env bun
/**
 * Robustness-catalog projection — matrix support verdict -> .cddl fixtures.
 *
 * Projects the matrix's execution-grounded cddl-codegen support verdict into the fixtures that
 * src/tests/robustness_tests.rs drives through a generate-only `catch_unwind` pass:
 *   - tests/matrix_supported/<id>.cddl  — every status="supported" feature/control-op   (expect-ok)
 *   - tests/matrix_panic/<id>.cddl      — every status="unsupported" feature/control-op whose evidence is
 *                                         a generation PANIC (`panic (exit 101)`)         (expect-PANIC)
 *   - tests/matrix_reject/<id>.cddl     — every status="unsupported" feature/control-op whose evidence is
 *                                         NOT a panic, PLUS every status="out_of_profile" row  (expect-reject)
 *
 * Each fixture is the construct's minimal `example` verbatim (the same text verify.ts probed), so the
 * in-process Rust outcome must match the matrix verdict. The panic catalog uses the matrix probe's
 * exact flags (--wasm=false, default profile, generate-only, no cargo check), so it captures ONLY
 * panic-class gaps; compile-class ones (`x = any`, bare `x = int`, `bool` in a type-choice) generate
 * fine here and are out of scope (that's a negative compile-gate's job). NOTE the supported gate
 * (robustness_tests::all_supported_constructs_generate) DELIBERATELY asserts a stronger claim than
 * the grounding probe: it also generates with --wasm=true, which verify.ts never probes — a red
 * there with a green matrix can mean a wasm-emission-only panic, not matrix↔generator drift.
 *
 * The reject catalog is the third generation-outcome scorecard: the rows a verdict marks off-limits but
 * that mint no test elsewhere (parse-rejected control ops, generates-but-doesn't-compile shapes like
 * `prelude.any`, out-of-profile constructs). Its purpose is to catch a parser/codegen change that
 * SILENTLY makes a rejected construct parse — the exact thing a past cddl-fork bump did to 14 control
 * ops — which flips a committed `error (graceful)` row to `ok` and surfaces as a snapshot diff in the
 * default `cargo test` suite instead of waiting for a manual verify.ts run. Because the outcome differs
 * by evidence class (a parse-reject errors gracefully; a generates-but-doesn't-compile row generates
 * fine under generate-only; an out-of-profile panic still panics), the --check cross-check derives the
 * expected label PER ROW from the row's evidence class (below), not uniformly.
 *
 * contain.* (role x feature) cells are projected too. Their support probes use a cell-specific
 * evidence vocabulary (`probe (cell): cddl-codegen exit ...`), but still map onto the same three
 * generate-only outcome catalogs.
 *
 * Run from cddl-matrix/:
 *   bun run project_robustness.ts          -> (re)writes all three fixture dirs from matrix.json
 *   bun run project_robustness.ts --check  -> drift gate: fails if any fixture is stale/missing/orphaned
 */
import { readFileSync, existsSync, readdirSync, mkdirSync, writeFileSync, rmSync } from "node:fs";

const HERE = import.meta.dir;
const SUPPORTED_DIR = `${HERE}/../tests/matrix_supported`;
const PANIC_DIR = `${HERE}/../tests/matrix_panic`;
const REJECT_DIR = `${HERE}/../tests/matrix_reject`;
const CHECK = process.argv.includes("--check");

// The generation-outcome label the reject catalog is EXPECTED to record for a row, derived from the
// row's matrix evidence class (the reject catalog is heterogeneous — its rows fail for different
// reasons, and generate-only observes each differently). Returns null if the evidence shape is one
// this catalog should never contain (a hard drift: the vocabulary drifted; the caller fails loud).
function rejectExpectedLabel(evidence: string): string | null {
  if (evidence.startsWith("probe (cell): cddl-codegen exit 1")) return "error (graceful)";
  // Cell support can be downgraded by post-generation compile/test gaps. A generate-only reject
  // catalog fixture will still record `ok` for those rows, same as feature-level compile gaps.
  if (evidence.startsWith("probe (cell): cddl-codegen exit 0;")) return "ok";
  if (evidence.startsWith("probe (cell): cddl-codegen exit 101")) return "PANIC";
  if (evidence.includes("rejected at parse/lex")) return "error (graceful)";
  // The gap is POST-generation (won't compile, or emitted round-trip tests fail); a generate-only
  // pass legitimately succeeds, so the catalog records `ok`. This is not drift — the reject catalog
  // is generate-only, so it can't observe a compile/round-trip failure and correctly reports `ok`.
  if (
    evidence.includes("generates but does not compile") ||
    evidence.includes("compiles but emitted round-trip tests fail")
  )
    return "ok";
  // A construct that panics the generator even though it's off-profile (e.g. type2.tag_head_type is
  // out_of_profile WITH panic evidence) still panics under generate-only.
  if (evidence.includes("panic (exit 101)")) return "PANIC";
  return null;
}

function isPanicEvidence(evidence: string): boolean {
  return evidence.includes("panic (exit 101)") || evidence.startsWith("probe (cell): cddl-codegen exit 101");
}

interface Ann { id: string; status: string; evidence?: string }
interface Ex { id: string; example: string }
const matrix = JSON.parse(readFileSync(`${HERE}/matrix.json`, "utf8")) as {
  features: Ex[];
  containment: Ex[];
  control_operators: Ex[];
  annotations: { cddl_codegen: Ann[] };
};

// id -> minimal example. Features/control-ops use their support-probe examples; containment cells use
// their role-specific cell examples.
const exampleById = new Map<string, string>(
  [...matrix.features, ...matrix.control_operators, ...matrix.containment].map(f => [f.id, f.example]),
);
// Extern-stub features (`example_extern_stub` — e.g. dsl.rust_name) probe as DIRECTORY input in
// verify.ts (their directive is only legal inside an `_CDDL_CODEGEN_EXTERN_DEPS_DIR_` scope); these
// flat single-file catalogs cannot represent them, so they are skipped with a loud per-run note
// (no-silent-caps) — their generate-path coverage lives in verify.ts's probe and the integration
// suites the annotation evidence cites, not here.
const externStubIds = new Set<string>(
  (matrix.features as { id: string; example_extern_stub?: string }[])
    .filter(f => f.example_extern_stub !== undefined)
    .map(f => f.id),
);

const supported: Ex[] = [];
const panic: Ex[] = [];
const reject: Ex[] = [];
// id -> the generation-outcome label the reject catalog should record (from evidence class); consumed
// by the --check cross-check so it derives the expected label per row, not uniformly.
const rejectExpect = new Map<string, string>();
const droppedNoExample: string[] = [];
const rejectEvidenceDrift: string[] = [];
const skippedExternStub: string[] = [];
for (const a of matrix.annotations.cddl_codegen) {
  if (externStubIds.has(a.id)) {
    skippedExternStub.push(`${a.id} (${a.status})`);
    continue;
  }
  const ex = exampleById.get(a.id);
  if (ex === undefined) {
    // Any annotated row without a projectable example is a silent coverage gap. Surface it.
    droppedNoExample.push(`${a.id} (${a.status})`);
    continue;
  }
  const evidence = a.evidence ?? "";
  if (a.status === "supported") supported.push({ id: a.id, example: ex });
  else if (a.status === "unsupported" && isPanicEvidence(evidence))
    panic.push({ id: a.id, example: ex });
  // Reject catalog: non-panic unsupported rows (parse-rejected, generates-but-doesn't-compile) plus
  // every out_of_profile row (which can itself be panic-class, e.g. type2.tag_head_type).
  else if (
    (a.status === "unsupported" && !isPanicEvidence(evidence)) ||
    a.status === "out_of_profile"
  ) {
    reject.push({ id: a.id, example: ex });
    const expected = rejectExpectedLabel(evidence);
    if (expected === null)
      rejectEvidenceDrift.push(`${a.id} (evidence: "${evidence}")`);
    else rejectExpect.set(a.id, expected);
  }
}
if (rejectEvidenceDrift.length) {
  console.log(
    `ERROR: ${rejectEvidenceDrift.length} reject-catalog row(s) have an evidence shape rejectExpectedLabel() ` +
      `doesn't recognise — the evidence vocabulary drifted; update rejectExpectedLabel in project_robustness.ts: ` +
      rejectEvidenceDrift.join(", "),
  );
  process.exit(1);
}
if (droppedNoExample.length) {
  console.log(
    `ERROR: ${droppedNoExample.length} feature/control-op annotation id(s) have no example to project ` +
      `(coverage gap — add an example to features/*.toml or control_examples.toml): ${droppedNoExample.join(", ")}`,
  );
  process.exit(1);
}
if (skippedExternStub.length)
  console.log(
    `NOTE: ${skippedExternStub.length} extern-stub feature row(s) not projected into the flat ` +
      `catalogs (directory-input probes; covered by verify.ts + the integration suites their ` +
      `evidence cites): ${skippedExternStub.join(", ")}`,
  );
supported.sort((x, y) => (x.id < y.id ? -1 : x.id > y.id ? 1 : 0));
panic.sort((x, y) => (x.id < y.id ? -1 : x.id > y.id ? 1 : 0));
reject.sort((x, y) => (x.id < y.id ? -1 : x.id > y.id ? 1 : 0));

// The fixture is the example verbatim (trimmed + one trailing newline). The filename is the id (dots are
// fine — Rust `file_stem` recovers the id as the catalog label).
const content = (ex: string) => ex.trim() + "\n";

const drift: string[] = [];
function reconcile(dir: string, rows: Ex[], label: string) {
  if (!CHECK) mkdirSync(dir, { recursive: true });
  const want = new Map(rows.map(r => [`${r.id}.cddl`, content(r.example)]));
  const have = existsSync(dir) ? readdirSync(dir).filter(f => f.endsWith(".cddl")) : [];
  for (const f of have)
    if (!want.has(f)) {
      if (CHECK) drift.push(`${label}: orphan fixture \`${f}\` (no longer in the projected set)`);
      else rmSync(`${dir}/${f}`);
    }
  for (const [f, body] of want) {
    const path = `${dir}/${f}`;
    const cur = existsSync(path) ? readFileSync(path, "utf8") : null;
    if (CHECK) {
      if (cur === null) drift.push(`${label}: missing fixture \`${f}\``);
      else if (cur !== body) drift.push(`${label}: \`${f}\` content drift vs matrix example`);
    } else if (cur !== body) writeFileSync(path, body);
  }
}
reconcile(SUPPORTED_DIR, supported, "matrix_supported (expect-ok)");
reconcile(PANIC_DIR, panic, "matrix_panic (expect-PANIC)");
reconcile(REJECT_DIR, reject, "matrix_reject (expect-reject)");

// Cross-check the committed outcome catalog against the matrix verdict class. This gates the drift
// class that actually happened once: a generator change flips a fixture's outcome, the insta catalog
// is re-blessed (observed truth updated), but the matrix verdict it was projected from only changes
// on a manual verify.ts run — leaving two committed artifacts contradicting each other with every
// gate green. A catalog row disagreeing with the matrix's panic class now fails the drift gate.
if (CHECK) {
  const snapPath = `${PANIC_DIR}/snapshots/catalog.snap`;
  if (!existsSync(snapPath)) drift.push("matrix_panic: snapshots/catalog.snap is missing (run the Rust catalog test)");
  else {
    const panicIds = new Set(panic.map(r => r.id));
    const rows = readFileSync(snapPath, "utf8")
      .split("\n")
      .map(l => /^([\w.$-]+) +(ok|error \(graceful\)|PANIC)$/.exec(l))
      .filter((m): m is RegExpExecArray => m !== null);
    // The label vocabulary is defined independently in src/tests/robustness_tests.rs. If it drifts (a
    // relabel + insta re-bless, both CI-green), this regex matches zero rows and the loop below
    // becomes vacuous — the cross-check would "pass" having compared nothing. Assert non-empty, and
    // assert every projected panic-class id is actually present (a missing row is otherwise invisible
    // to both loop arms, which only fire on rows that DID parse).
    if (rows.length === 0) {
      drift.push(
        "catalog↔matrix: parsed 0 rows from catalog.snap — the label format drifted from this regex " +
          "(update project_robustness.ts) or the snapshot is empty",
      );
    } else {
      const catalogIds = new Set(rows.map(([, id]) => id));
      for (const id of panicIds)
        if (!catalogIds.has(id))
          drift.push(
            `catalog↔matrix: matrix panic-class \`${id}\` has no parseable row in the committed catalog.snap ` +
              `(renamed/dropped, or the label regex no longer matches its row)`,
          );
    }
    for (const [, id, label] of rows) {
      if (panicIds.has(id) && label !== "PANIC")
        drift.push(
          `catalog↔matrix: \`${id}\` is matrix panic-class but the committed catalog records \`${label}\` — ` +
            `re-run verify.ts (refresh the verdict), re-project, re-bless`,
        );
      else if (!panicIds.has(id) && label === "PANIC")
        drift.push(
          `catalog↔matrix: the catalog records PANIC for \`${id}\` but the matrix no longer marks it panic-class — re-project and re-bless`,
        );
    }
  }
}

// Same cross-check for the reject catalog, but the expected label is PER ROW (from the row's evidence
// class, computed above into rejectExpect), because the reject catalog is heterogeneous: a parse-reject
// records `error (graceful)`, a generates-but-doesn't-compile row records `ok` under generate-only, an
// out-of-profile panic records `PANIC`. Same anti-vacuity guards as the panic cross-check above.
if (CHECK) {
  const snapPath = `${REJECT_DIR}/snapshots/catalog.snap`;
  if (!existsSync(snapPath)) drift.push("matrix_reject: snapshots/catalog.snap is missing (run the Rust catalog test)");
  else {
    const rows = readFileSync(snapPath, "utf8")
      .split("\n")
      .map(l => /^([\w.$-]+) +(ok|error \(graceful\)|PANIC)$/.exec(l))
      .filter((m): m is RegExpExecArray => m !== null);
    // Anti-vacuity: a label relabel + re-bless (both CI-green) would make this regex match zero rows,
    // silently comparing nothing. Assert non-empty AND that every projected reject id has a row.
    if (rows.length === 0) {
      drift.push(
        "reject catalog↔matrix: parsed 0 rows from catalog.snap — the label format drifted from this regex " +
          "(update project_robustness.ts) or the snapshot is empty",
      );
    } else {
      const catalogIds = new Set(rows.map(([, id]) => id));
      for (const id of rejectExpect.keys())
        if (!catalogIds.has(id))
          drift.push(
            `reject catalog↔matrix: projected \`${id}\` has no parseable row in the committed catalog.snap ` +
              `(renamed/dropped, or the label regex no longer matches its row)`,
          );
    }
    for (const [, id, label] of rows) {
      const want = rejectExpect.get(id);
      if (want === undefined)
        drift.push(
          `reject catalog↔matrix: the catalog records a row for \`${id}\` but the matrix no longer projects it ` +
            `into the reject catalog — re-project and re-bless`,
        );
      else if (label !== want)
        drift.push(
          `reject catalog↔matrix: \`${id}\` should record \`${want}\` (its matrix evidence class) but the committed ` +
            `catalog records \`${label}\` — a rejected construct may have started parsing; investigate, then re-run ` +
            `verify.ts (refresh the verdict), re-project, re-bless`,
        );
    }
  }
}

console.log(
  `robustness projection: ${supported.length} supported (expect-ok), ${panic.length} panic-class (expect-PANIC), ` +
    `${reject.length} reject-class (expect-reject)`,
);
if (CHECK) {
  if (drift.length) {
    console.log(`SNAPSHOT DRIFT (${drift.length}) — run \`bun run project_robustness.ts\` and review:`);
    for (const d of drift) console.log("  -", d);
    process.exit(1);
  }
  console.log("drift check OK: all three fixture dirs match the matrix support verdict");
} else {
  console.log(`wrote tests/matrix_supported/, tests/matrix_panic/, and tests/matrix_reject/`);
}
