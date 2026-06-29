#!/usr/bin/env bun
/**
 * Robustness-catalog projection (testing-roadmap c2 + c3) — matrix support verdict -> .cddl fixtures.
 *
 * Projects the matrix's execution-grounded cddl-codegen support verdict into the fixtures that
 * src/robustness_tests.rs drives through a generate-only `catch_unwind` pass:
 *   - tests/matrix_supported/<id>.cddl  — every status="supported" feature/control-op   (c3, expect-ok)
 *   - tests/matrix_panic/<id>.cddl      — every status="unsupported" feature/control-op whose evidence is
 *                                         a generation PANIC (`panic (exit 101)`)         (c2, expect-PANIC)
 *
 * Each fixture is the construct's minimal `example` verbatim (the same text verify.ts probed), so the
 * in-process Rust outcome must match the matrix verdict. The Rust harness uses --wasm=false, default
 * profile, generate-only (no cargo check) — exactly the matrix probe's flags — so this captures ONLY
 * panic-class gaps. Compile-class ones (`x = any`, bare `x = int`, `bool` in a type-choice) generate
 * fine here and are out of scope (that's a negative compile-gate's job).
 *
 * contain.* (role x feature) cells are deliberately excluded — they carry no standalone probe example
 * and are a different axis (c1's role x feature corpus already covers them).
 *
 * Run from cddl-matrix/:
 *   bun run project_robustness.ts          -> (re)writes both fixture dirs from matrix.json
 *   bun run project_robustness.ts --check  -> drift gate: fails if any fixture is stale/missing/orphaned
 */
import { readFileSync, existsSync, readdirSync, mkdirSync, writeFileSync, rmSync } from "node:fs";

const HERE = import.meta.dir;
const SUPPORTED_DIR = `${HERE}/../tests/matrix_supported`;
const PANIC_DIR = `${HERE}/../tests/matrix_panic`;
const CHECK = process.argv.includes("--check");

interface Ann { id: string; status: string; evidence?: string }
interface Ex { id: string; example: string }
const matrix = JSON.parse(readFileSync(`${HERE}/matrix.json`, "utf8")) as {
  features: Ex[];
  control_operators: Ex[];
  annotations: { cddl_codegen: Ann[] };
};

// id -> minimal example, for features + control-ops only (contain.* cells have no standalone example).
const exampleById = new Map<string, string>(
  [...matrix.features, ...matrix.control_operators].map(f => [f.id, f.example]),
);

const supported: Ex[] = [];
const panic: Ex[] = [];
const droppedNoExample: string[] = [];
for (const a of matrix.annotations.cddl_codegen) {
  const ex = exampleById.get(a.id);
  if (ex === undefined) {
    // contain.* (per-cell role × feature) rows legitimately carry no standalone example; ANY other
    // dropped id is a feature/control-op with no example to project = a silent coverage gap. Surface it.
    if (!a.id.startsWith("contain.")) droppedNoExample.push(`${a.id} (${a.status})`);
    continue;
  }
  if (a.status === "supported") supported.push({ id: a.id, example: ex });
  else if (a.status === "unsupported" && (a.evidence ?? "").includes("panic (exit 101)"))
    panic.push({ id: a.id, example: ex });
}
if (droppedNoExample.length) {
  console.log(
    `ERROR: ${droppedNoExample.length} feature/control-op annotation id(s) have no example to project ` +
      `(coverage gap — add an example to features/*.toml or control_examples.toml): ${droppedNoExample.join(", ")}`,
  );
  process.exit(1);
}
supported.sort((x, y) => (x.id < y.id ? -1 : 1));
panic.sort((x, y) => (x.id < y.id ? -1 : 1));

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
reconcile(SUPPORTED_DIR, supported, "matrix_supported (c3)");
reconcile(PANIC_DIR, panic, "matrix_panic (c2)");

console.log(
  `robustness projection: ${supported.length} supported (expect-ok), ${panic.length} panic-class (expect-PANIC)`,
);
if (CHECK) {
  if (drift.length) {
    console.log(`SNAPSHOT DRIFT (${drift.length}) — run \`bun run project_robustness.ts\` and review:`);
    for (const d of drift) console.log("  -", d);
    process.exit(1);
  }
  console.log("drift check OK: both fixture dirs match the matrix support verdict");
} else {
  console.log(`wrote tests/matrix_supported/ and tests/matrix_panic/`);
}
