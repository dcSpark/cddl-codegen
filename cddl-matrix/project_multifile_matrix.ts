#!/usr/bin/env bun
/**
 * multifile placement matrix projection — enumerate {type-shape} × {cross-module reference mode}
 * into two-module DIRECTORY fixtures under tests/matrix_multifile/<shape>__<mode>/.
 *
 * Sibling of project_wasm_matrix.ts. Where the wasm-ABI matrix individuates how a type crosses the
 * wasm-bindgen boundary, this individuates the axis EVERY other construct gate is blind to: MODULE
 * PLACEMENT. The corpus gates, the wasm-ABI matrix and the parity differential all feed the generator
 * SINGLE-file specs, so every construct is only ever verified in root scope. Multifile emission
 * branches on scope: `mark_refs` (intermediate.rs) resolves the import source for the
 * generator-invented structural wrappers (`XList`/`MapKToV`), and each per-module `mod.rs` declares
 * only the submodules whose files it actually emits. This matrix is the systematic catcher for that
 * axis; the emitter invariants it guards (each once a loud `cargo check` failure class): a
 * cross-module anonymous same-shape use imports the structural name from the shape's owner module
 * (the E0432 class), a module declares `pub mod serialization;` only when it emits the file (the
 * E0583 class), and a cross-module named `.cbor` ref imports the inner type (the E0433 class).
 * Placement breakage fails loudly at `cargo check`, so a compile floor is a sufficient oracle. The
 * Rust gate `integration_tests::multifile_matrix_compiles` generates each `--wasm=true` (directory
 * input) and `cargo check`s the wasm crate (which pulls the rust crate as a path dep, so rust-side
 * breakage surfaces through it), so an un-covered placement regression surfaces as a specific red
 * cell; any deliberately-held class goes into that gate's `MULTIFILE_MATRIX_SKIP` ledger (currently:
 * the `collrec` array-structural-wrapper cells — the Array-arm placement class, enumerated after
 * review found the SHAPES hole; see the cddl-matrix/ROADMAP.md finding).
 *
 * Deterministic (sorted cell order; no hash-order). Fixtures go to tests/matrix_multifile/<cell>/{lib,a,b}.cddl.
 *
 * Run from cddl-matrix/:
 *   bun run project_multifile_matrix.ts          -> (re)writes tests/matrix_multifile/<cell>/*.cddl
 *   bun run project_multifile_matrix.ts --check  -> drift gate: fails if any fixture is stale/missing/orphaned
 */
import { readFileSync, existsSync, readdirSync, mkdirSync, writeFileSync, rmSync, statSync } from "node:fs";

const HERE = import.meta.dir;
const DIR = `${HERE}/../tests/matrix_multifile`;
const CHECK = process.argv.includes("--check");

// --- Axis 1: type-shapes. Defs + `ty` copied verbatim (provenance) from project_wasm_matrix.ts's
// `SHAPES` — do NOT import it (that module runs projection on import) — plus `collrec`, which is
// multifile-SPECIFIC (the structural array wrapper only needs placement cross-module; at the wasm
// matrix's root scope the class cannot bite). Included: every shape that HAS
// defs and is self-contained (can compile standalone). `anonForm` is the shape's inline anonymous
// same-shape spelling (the `mark_refs` structural-wrapper class); present iff the anon holder
// `holder = [field0: <anonForm>]` compiles GREEN as a single-file spec — verified once during
// construction (throwaway generate + `cargo check` rust+wasm). All 7 anon-cell candidates
// probed green (coll/collmap/collrec/tag/nullable/bwrap/cborwrap), so all 7 admit an anon cell; a red
// there would be a single-file limitation, not a placement finding, and the shape would carry no
// `anonForm`.
//
// Excluded shapes (present in the wasm matrix, deliberately absent here):
//   - `prim` (`ty: uint`, no defs) — nothing to PLACE in a module; a module needs at least one rule.
//   - `extern` / `rawbytes` — user-supplied types (`_CDDL_CODEGEN_EXTERN_TYPE_` / raw-bytes); can't
//     compile standalone (same permanent exclusion the wasm matrix carries for `extern`).
interface Shape {
  defs: string[]; // named-type definitions -> module `a` (authored dependency order; CDDL is order-free)
  ty: string; // the shape's named rule, referenced cross-module by the `named` mode
  anonForm?: string; // inline anonymous same-shape spelling -> the `anon`/`anonb` modes' `b.cddl` field type
  // `anonb` (anon + ballast) participation: the plain `anon` cell of an alias/table-only module `a`
  // reds on E0583 FIRST (module `a` emits no serialization.rs), masking the b-side E0432
  // anonymous-same-shape import — the CORE `mark_refs` finding. `anonb` adds a ballast record rule to
  // `a` so it emits serialization and the b-side reference verdict surfaces. Set ONLY on the shapes
  // whose plain `anon` cell is E0583-masked (coll, collmap, nullable); the other anon shapes
  // (tag, bwrap, cborwrap) are GREEN in plain `anon` — module `a` already emits serialization, so a
  // ballast variant adds no discrimination and they are excluded from `anonb`.
  anonBallast?: boolean;
}
const SHAPES: Record<string, Shape> = {
  palias: { defs: ["pa = uint"], ty: "pa" },
  talias: { defs: ["ta = text"], ty: "ta" },
  coll: { defs: ["nums = [* uint]"], ty: "nums", anonForm: "[* uint]", anonBallast: true },
  collmap: { defs: ["mp = { * uint => text }"], ty: "mp", anonForm: "{ * uint => text }", anonBallast: true },
  passthru: { defs: ["nums = [* uint]", "pt = nums"], ty: "pt" },
  passthrumap: { defs: ["mp = { * uint => text }", "ptm = mp"], ty: "ptm" },
  struct: { defs: ["st = [a: uint, b: text]"], ty: "st" },
  mstruct: { defs: ["mst = { a: uint, b: text }"], ty: "mst" },
  // cborwrap's anon form references the named `foo` (which lives in module `a`) — a cross-module named
  // ref embedded in an anonymous `.cbor` wrapper. It still individuates the anon-placement class (the
  // `.cbor` wrapper resolution under module scope) and its single-file control is green, so it is kept.
  cborwrap: { defs: ["foo = [a: uint]", "fb = bytes .cbor foo"], ty: "fb", anonForm: "bytes .cbor foo" },
  // Collection of NON-exposable elements: the array whose wasm representation needs a generated
  // structural wrapper (`FooList`), i.e. the Array-arm sibling of `collmap`'s structural-map class —
  // `mark_refs`' Array arm still hard-codes ROOT_SCOPE as the wrapper's import source (the remaining
  // issue-138 half). `coll` ([* uint]) is transparent `Vec<u64>` and can never probe this. Both
  // reference modes are known-red (see MULTIFILE_MATRIX_SKIP + the cddl-matrix/ROADMAP.md finding);
  // the shape was enumerated AFTER review found the hole — the single-file anon control is green
  // (like cborwrap, the anon form references the named `foo` cross-module).
  collrec: { defs: ["foo = [a0: uint]", "recs = [* foo]"], ty: "recs", anonForm: "[* foo]" },
  tag: { defs: ["tg = #6.10(uint)"], ty: "tg", anonForm: "#6.10(uint)" },
  bwrap: { defs: ["bw = bytes .size (0..32)"], ty: "bw", anonForm: "bytes .size (0..32)" },
  cenum: { defs: ["fe = 0 / 1 / 2"], ty: "fe" },
  denum: { defs: ["denum = uint / text"], ty: "denum" },
  nullable: { defs: ["opt = uint / null"], ty: "opt", anonForm: "uint / null", anonBallast: true },
  generic: { defs: ["cont<T0> = [value: T0]", "uc = cont<uint>"], ty: "uc" },
  chain: { defs: ["ca = [* uint]", "cb = ca", "cc = cb"], ty: "cc" },
  cborwrap2: { defs: ["foo = [a: uint]", "fb = bytes .cbor foo", "fb2 = fb"], ty: "fb2" },
};

// --- Axis 2: cross-module reference mode. The shape's defs go in module `a`; module `b` holds one
// `bholder = [field0: <...>]` record. `named` references the shape's named rule cross-module; `anon`
// embeds the shape's inline anonymous same-shape spelling (the `mark_refs` structural class); `anonb`
// is `anon` with a ballast record rule added to module `a` (so `a` emits serialization and E0583 can't
// mask the b-side E0432 import verdict — see `anonBallast` above); `unref` references nothing (module
// `a` still declares the shape — this is where an alias/table-only module could mis-declare
// `pub mod serialization;`, E0583). Root-owner direction (shape in root, referenced from a module) is
// deliberately NOT enumerated: root-module owners probed fine in BOTH directions, so the
// non-root-owner cells above are the discriminating ones. `bholder`/`field0`/`bal0` dodge the
// `R`/`W`/`T` reader/writer/generic letters.
interface Mode {
  // returns module `b`'s content, or null if the shape does not participate in this mode
  b: (s: Shape) => string | null;
  aExtra?: (s: Shape) => string[]; // extra rules appended to module `a`'s defs for this mode
}
const MODES: Record<string, Mode> = {
  anon: { b: (s) => (s.anonForm ? `bholder = [field0: ${s.anonForm}]` : null) },
  anonb: {
    b: (s) => (s.anonBallast && s.anonForm ? `bholder = [field0: ${s.anonForm}]` : null),
    aExtra: () => ["ballast = [bal0: uint]"],
  },
  named: { b: (s) => `bholder = [field0: ${s.ty}]` },
  unref: { b: () => `bholder = [field0: uint]` },
};

// The anon subset is a reviewed fact (which shapes have a single-file-green anon form): a mis-typed
// `anonForm` key would silently drop a shape from `anon` (TS excess-property check catches an unknown
// key at the literal, but not a value dropped by a downstream filter) — pin the derived set so any
// grid shrink/growth is an explicit reviewed edit, exactly like EXPECTED_CELLS below.
const EXPECTED_ANON_SHAPES = ["bwrap", "cborwrap", "coll", "collmap", "collrec", "nullable", "tag"];
const anonShapes = Object.keys(SHAPES)
  .filter((k) => SHAPES[k].anonForm)
  .sort();
if (JSON.stringify(anonShapes) !== JSON.stringify(EXPECTED_ANON_SHAPES))
  throw new Error(
    `anon-form shape set is [${anonShapes.join(", ")}], expected [${EXPECTED_ANON_SHAPES.join(", ")}] — ` +
      `if the change is deliberate (a probe outcome changed), update EXPECTED_ANON_SHAPES in the same commit`,
  );

// Same idiom for the `anonb` subset (exactly the E0583-masked plain-`anon` shapes).
const EXPECTED_ANONB_SHAPES = ["coll", "collmap", "nullable"];
const anonbShapes = Object.keys(SHAPES)
  .filter((k) => SHAPES[k].anonBallast)
  .sort();
if (JSON.stringify(anonbShapes) !== JSON.stringify(EXPECTED_ANONB_SHAPES))
  throw new Error(
    `anonb shape set is [${anonbShapes.join(", ")}], expected [${EXPECTED_ANONB_SHAPES.join(", ")}] — ` +
      `if the change is deliberate (a masking outcome changed), update EXPECTED_ANONB_SHAPES in the same commit`,
  );
for (const k of anonbShapes)
  if (!SHAPES[k].anonForm)
    throw new Error(`SHAPES.${k}: anonBallast without anonForm — anonb reuses the anon spelling, so it needs one`);

// lib.cddl is the root scope (file stem `lib` == ROOT_SCOPE); one trivial rule, constant across cells.
const LIB_CDDL = "rt = [uint]\n";

interface Cell {
  dir: string;
  files: Record<string, string>;
}
const cells: Cell[] = [];
for (const shape of Object.keys(SHAPES).sort()) {
  const s = SHAPES[shape];
  for (const mode of Object.keys(MODES).sort()) {
    const b = MODES[mode].b(s);
    if (b === null) continue; // shape does not participate in this mode (anon without a green form)
    const aRules = [...s.defs, ...(MODES[mode].aExtra?.(s) ?? [])];
    cells.push({
      dir: `${shape}__${mode}`,
      files: {
        "lib.cddl": LIB_CDDL,
        "a.cddl": `; cell: ${shape} x ${mode} (shape defs, module a)\n${aRules.join("\n")}\n`,
        "b.cddl": `; cell: ${shape} x ${mode} (reference from module b)\n${b}\n`,
      },
    });
  }
}
cells.sort((a, b) => (a.dir < b.dir ? -1 : a.dir > b.dir ? 1 : 0));

// Grid shrink/growth must be an explicit, reviewed edit — not the byproduct of a filter change.
const EXPECTED_CELLS = 46; // 18 shapes × {named, unref} = 36 + 7 anon-form shapes × {anon} + 3 anonb shapes × {anonb} -> 46
if (cells.length !== EXPECTED_CELLS)
  throw new Error(
    `multifile grid produced ${cells.length} cells, expected ${EXPECTED_CELLS} — if the change is deliberate, update EXPECTED_CELLS in the same commit`,
  );

const drift: string[] = [];
if (!CHECK) mkdirSync(DIR, { recursive: true });
const wantDirs = new Map(cells.map((c) => [c.dir, c.files]));

// orphan cell dirs (a fixture dir no longer in the projected set)
const haveDirs = existsSync(DIR)
  ? readdirSync(DIR).filter((f) => statSync(`${DIR}/${f}`).isDirectory())
  : [];
for (const d of haveDirs)
  if (!wantDirs.has(d)) {
    if (CHECK) drift.push(`orphan fixture dir \`${d}/\` (no longer in the projected set)`);
    else rmSync(`${DIR}/${d}`, { recursive: true });
  }

for (const [d, files] of wantDirs) {
  const cellDir = `${DIR}/${d}`;
  if (!CHECK) mkdirSync(cellDir, { recursive: true });
  // orphan files within a cell dir (a file not in the projected 3)
  const haveFiles = existsSync(cellDir) ? readdirSync(cellDir).filter((f) => statSync(`${cellDir}/${f}`).isFile()) : [];
  for (const f of haveFiles)
    if (!(f in files)) {
      if (CHECK) drift.push(`orphan fixture file \`${d}/${f}\` (not in the projected set)`);
      else rmSync(`${cellDir}/${f}`);
    }
  for (const [f, body] of Object.entries(files)) {
    const path = `${cellDir}/${f}`;
    const cur = existsSync(path) ? readFileSync(path, "utf8") : null;
    if (CHECK) {
      if (cur === null) drift.push(`missing fixture \`${d}/${f}\``);
      else if (cur !== body) drift.push(`\`${d}/${f}\` content drift vs projection`);
    } else if (cur !== body) writeFileSync(path, body);
  }
}

console.log(`multifile placement matrix projection: ${cells.length} cells (shape x mode) -> tests/matrix_multifile/`);
if (CHECK) {
  if (drift.length) {
    console.log(`SNAPSHOT DRIFT (${drift.length}) — run \`bun run project_multifile_matrix.ts\` and review:`);
    for (const d of drift) console.log("  -", d);
    process.exit(1);
  }
  console.log("drift check OK: tests/matrix_multifile matches the projection");
}
