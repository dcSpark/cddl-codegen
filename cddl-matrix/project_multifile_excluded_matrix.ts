#!/usr/bin/env bun
/**
 * Generation-only projection for multifile shapes deliberately excluded from the compile matrix.
 *
 * `extern` and `rawbytes` need consumer-provided Rust definitions, so putting them under
 * `tests/matrix_multifile/` would turn the compile floor red for fixture plumbing rather than
 * generator behaviour.  This compact sibling grid instead provides deterministic directory input
 * to `multifile_excluded_shape_matrix_generates`, which calls `api::generated_strings` in-process.
 * It covers every meaningful existing reference position (named field, alias target, unreferenced
 * owner) for plain extern, raw bytes, and extern generic instances with BOTH plain-record and
 * raw-bytes arguments. The generic cell families are the feature-request-07 shape: a non-root
 * alias has to import `Base` and its arguments separately, never a `Base<Args>` expression in a
 * Rust `use` line.
 *
 * Run from cddl-matrix/:
 *   bun run project_multifile_excluded_matrix.ts          -> (re)writes tests/matrix_multifile_excluded/<cell>/*.cddl
 *   bun run project_multifile_excluded_matrix.ts --check  -> fails if any fixture is stale/missing/orphaned
 */
import { existsSync, mkdirSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync } from "node:fs";

const HERE = import.meta.dir;
const DIR = `${HERE}/../tests/matrix_multifile_excluded`;
const CHECK = process.argv.includes("--check");

interface Shape {
  defs: string[];
  ty: string;
}

// These are the exact shapes excluded from the ordinary matrix's standalone compile proof.  The
// generic instances cover BOTH argument classes implicated by feature request 07. Keeping either
// one alone would leave a normalization/flavor branch unobserved: `ExtSet<Plain>` and
// `ExtSetRawBytes<PubKey>` must both decompose to a base + argument import from module `a`.
const SHAPES: Record<string, Shape> = {
  extern: { defs: ["ext = _CDDL_CODEGEN_EXTERN_TYPE_"], ty: "ext" },
  generic_extern_plain: {
    defs: ["plain = [a: uint]", "ext_set<T> = _CDDL_CODEGEN_EXTERN_TYPE_ ; @raw_bytes_flavor"],
    ty: "ext_set<plain>",
  },
  generic_extern_rawbytes: {
    defs: ["pub_key = _CDDL_CODEGEN_RAW_BYTES_TYPE_", "ext_set<T> = _CDDL_CODEGEN_EXTERN_TYPE_ ; @raw_bytes_flavor"],
    ty: "ext_set<pub_key>",
  },
  rawbytes: { defs: ["pub_key = _CDDL_CODEGEN_RAW_BYTES_TYPE_"], ty: "pub_key" },
};
const EXPECTED_SHAPES = ["extern", "generic_extern_plain", "generic_extern_rawbytes", "rawbytes"];

interface Mode {
  b: (shape: Shape) => string;
}
const MODES: Record<string, Mode> = {
  aliased: { b: (shape) => `bal = ${shape.ty}` },
  named: { b: (shape) => `bholder = [field0: ${shape.ty}]` },
  unref: { b: () => "bholder = [field0: uint]" },
};
const EXPECTED_MODES = ["aliased", "named", "unref"];

for (const [label, actual, expected] of [
  ["shape", Object.keys(SHAPES).sort(), EXPECTED_SHAPES],
  ["reference-mode", Object.keys(MODES).sort(), EXPECTED_MODES],
] as const)
  if (JSON.stringify(actual) !== JSON.stringify(expected))
    throw new Error(
      `excluded-shape ${label} set is [${actual.join(", ")}], expected [${expected.join(", ")}] — ` +
        "update the exact membership pin and test assertions in the same reviewed change",
    );

interface Cell {
  dir: string;
  files: Record<string, string>;
}
const cells: Cell[] = [];
for (const shapeName of EXPECTED_SHAPES) {
  const shape = SHAPES[shapeName]!;
  for (const modeName of EXPECTED_MODES) {
    const mode = MODES[modeName]!;
    cells.push({
      dir: `${shapeName}__${modeName}`,
      files: {
        "lib.cddl": "rt = [uint]\n",
        "a.cddl": `; generation-only excluded cell: ${shapeName} x ${modeName} (shape defs, module a)\n${shape.defs.join("\n")}\n`,
        "b.cddl": `; generation-only excluded cell: ${shapeName} x ${modeName} (reference from module b)\n${mode.b(shape)}\n`,
      },
    });
  }
}
// Bytewise comparison, not localeCompare: fixture ordering is a reproducibility property, not a
// host-locale decision.
cells.sort((a, b) => (a.dir < b.dir ? -1 : a.dir > b.dir ? 1 : 0));
const EXPECTED_CELLS = 12; // 4 excluded shapes × {aliased, named, unref}
if (cells.length !== EXPECTED_CELLS)
  throw new Error(`excluded-shape grid produced ${cells.length} cells, expected ${EXPECTED_CELLS}`);

const drift: string[] = [];
if (!CHECK) mkdirSync(DIR, { recursive: true });
const wanted = new Map(cells.map((cell) => [cell.dir, cell.files]));
const haveDirs = existsSync(DIR) ? readdirSync(DIR).filter((name) => statSync(`${DIR}/${name}`).isDirectory()) : [];
for (const dir of haveDirs)
  if (!wanted.has(dir)) {
    if (CHECK) drift.push(`orphan fixture dir \`${dir}/\` (no longer in the projected set)`);
    else rmSync(`${DIR}/${dir}`, { recursive: true });
  }

for (const [dir, files] of wanted) {
  const cellDir = `${DIR}/${dir}`;
  if (!CHECK) mkdirSync(cellDir, { recursive: true });
  const haveFiles = existsSync(cellDir) ? readdirSync(cellDir).filter((name) => statSync(`${cellDir}/${name}`).isFile()) : [];
  for (const file of haveFiles)
    if (!(file in files)) {
      if (CHECK) drift.push(`orphan fixture file \`${dir}/${file}\` (not in the projected set)`);
      else rmSync(`${cellDir}/${file}`);
    }
  for (const [file, body] of Object.entries(files)) {
    const path = `${cellDir}/${file}`;
    const current = existsSync(path) ? readFileSync(path, "utf8") : null;
    if (CHECK) {
      if (current === null) drift.push(`missing fixture \`${dir}/${file}\``);
      else if (current !== body) drift.push(`\`${dir}/${file}\` content drift vs projection`);
    } else if (current !== body) writeFileSync(path, body);
  }
}

console.log(`multifile excluded-shape generation matrix projection: ${cells.length} cells -> tests/matrix_multifile_excluded/`);
if (CHECK) {
  if (drift.length) {
    console.log(`SNAPSHOT DRIFT (${drift.length}) — run \`bun run project_multifile_excluded_matrix.ts\` and review:`);
    for (const item of drift) console.log("  -", item);
    process.exit(1);
  }
  console.log("drift check OK: tests/matrix_multifile_excluded matches the projection");
}
