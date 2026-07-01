#!/usr/bin/env bun
/**
 * wasm-ABI matrix projection — enumerate {wasm-ABI type-shape} × {boundary role} into .cddl fixtures.
 *
 * Sibling of project_robustness.ts. Where the serialization matrix (cddl-matrix) individuates CBOR
 * features, this individuates the *wasm binding* axis those features deliberately don't: how a type
 * crosses the wasm-bindgen boundary (by-value vs by-ref, `.into()`/`.clone()`, wrapper-vs-transparent,
 * accessor return/param types). Each cell = one minimal named-type shape placed in one boundary role.
 * The Rust gate `integration_tests::wasm_matrix_compiles` generates each `--wasm=true` and `cargo
 * check`s the wasm crate, so an un-covered cell surfaces as a specific red cell instead of a
 * production surprise (the enumeration writes the failing test; the emitter gets fixed cell by cell).
 *
 * The crux (see draft/handoff-wasm-abi-matrix.md): wrapper-vs-transparent is a STRUCT-TABLE fact, not
 * an IR-shape fact — `coll` (`nums = [* uint]`, a wrapper struct) and `passthru` (`pt = nums`, a
 * transparent `pub type`) share the identical IR shape but cross the boundary differently. Hence both
 * are distinct shapes here.
 *
 * Deterministic (sorted cell order; no hash-order). Fixtures go to tests/matrix_wasm/<shape>__<role>.cddl.
 *
 * Run from cddl-matrix/:
 *   bun run project_wasm_matrix.ts          -> (re)writes tests/matrix_wasm/*.cddl
 *   bun run project_wasm_matrix.ts --check  -> drift gate: fails if any fixture is stale/missing/orphaned
 */
import { readFileSync, existsSync, readdirSync, mkdirSync, writeFileSync, rmSync } from "node:fs";

const HERE = import.meta.dir;
const DIR = `${HERE}/../tests/matrix_wasm`;
const CHECK = process.argv.includes("--check");

// --- Axis 1: wasm-ABI type-shapes. Each shape supplies its named-type definition(s) plus the type
// name to place in a role. Type names are multi-letter to dodge the single-letter reader/writer/generic
// generics (`R`/`W`/`T`) the deserializer/serializer emit (a rule named `r`/`w` collides -> E0574).
interface Shape {
  defs: string[]; // named-type definitions, authored dependency order (CDDL itself is order-free)
  ty: string; // the type placed in the role
  roles?: string[]; // restrict to these roles (default: all). Used for redundant / non-compiling shapes.
}
const SHAPES: Record<string, Shape> = {
  // transparent, copy scalar
  prim: { defs: [], ty: "uint" },
  palias: { defs: ["pa = uint"], ty: "pa" },
  // transparent, non-copy (String) — the harmless-identity-`.into()` landmine
  talias: { defs: ["ta = text"], ty: "ta" },
  // wrapper struct (Array RustStruct) — by-ref + `.into()`
  coll: { defs: ["nums = [* uint]"], ty: "nums" },
  // wrapper struct (Table RustStruct) — a map-wrapper; a distinct emitter/typedef path from `coll`
  collmap: { defs: ["mp = { * uint => text }"], ty: "mp" },
  // transparent `pub type` -> Vec (the wrapper-vs-transparent distinction; shares IR shape with `coll`)
  passthru: { defs: ["nums = [* uint]", "pt = nums"], ty: "pt" },
  // transparent alias to a *map* typedef — the map/table typedef-resolution path (known-red: E0425)
  passthrumap: { defs: ["mp = { * uint => text }", "ptm = mp"], ty: "ptm" },
  // wrapper struct (Record RustStruct)
  struct: { defs: ["st = [a: uint, b: text]"], ty: "st" },
  // transparent-to-wrapper via `.cbor` (follows the inner wrapper `Foo`)
  cborwrap: { defs: ["foo = [a: uint]", "fb = bytes .cbor foo"], ty: "fb" },
  // c-style enum — Copy, re-exported by value (`pub use`)
  cenum: { defs: ["fe = 0 / 1 / 2"], ty: "fe" },
  // generic instance -> monomorphized wrapper struct
  generic: { defs: ["cont<T0> = [value: T0]", "uc = cont<uint>"], ty: "uc" },
  // --- Depth / representative smoke cells: same boundary logic as a 1-hop shape above, kept only to
  // guard alias-chain *resolution depth* (>1 hop). One role each — full role coverage would only
  // duplicate `passthru`/`cborwrap` accessors (verified: differs from them only by type name).
  chain: { defs: ["ca = [* uint]", "cb = ca", "cc = cb"], ty: "cc", roles: ["array-element"] }, // 2-hop passthru
  cborwrap2: {
    defs: ["foo = [a: uint]", "fb = bytes .cbor foo", "fb2 = fb"],
    ty: "fb2",
    roles: ["array-element"],
  }, // chained transparent-to-wrapper
  // extern user-supplied type — can't `cargo check` standalone (gate SKIP; integration-tested in
  // tests/extern-deps). Fails identically in every role, so one representative cell documents the shape.
  extern: { defs: ["ext = _CDDL_CODEGEN_EXTERN_TYPE_"], ty: "ext", roles: ["array-element"] },
};

// --- Axis 2: boundary roles. Each wraps the shape's type `ty` in a distinct accessor-emitting context.
// `holder` (-> `Holder`) is the wrapper; it avoids the `R`/`W`/`T` generic collision.
interface Role {
  wrap: (ty: string) => string;
}
const ROLES: Record<string, Role> = {
  "array-element": { wrap: (t) => `holder = [* ${t}]` }, // get() -> T, add(elem: T)
  "map-value": { wrap: (t) => `holder = { * uint => ${t} }` }, // get()->Option<T>, insert(value: T)
  "map-key": { wrap: (t) => `holder = { * ${t} => uint }` }, // key param, keys() -> Vec<T>
  "struct-field": { wrap: (t) => `holder = [field0: ${t}]` }, // getter field0()->T, new(field0: T)
  // array-rep 2-field: `{ ? f: T }` (map-rep bareword member key) panics the generator (parsing.rs
  // "unsupported table map key"), so an optional field must use the array representation.
  "struct-field-opt": { wrap: (t) => `holder = [pre: uint, ? field0: ${t}]` }, // getter->Option<T>, set_field0
  "newtype-inner": { wrap: (t) => `holder = ${t} ; @newtype` }, // new(inner: T), getter
};

const cells: { file: string; body: string }[] = [];
for (const shape of Object.keys(SHAPES).sort()) {
  const { defs, ty, roles } = SHAPES[shape];
  for (const role of Object.keys(ROLES).sort()) {
    if (roles && !roles.includes(role)) continue;
    const lines = [`; cell: ${shape} x ${role}`, ...defs, ROLES[role].wrap(ty)];
    cells.push({ file: `${shape}__${role}.cddl`, body: lines.join("\n") + "\n" });
  }
}
cells.sort((a, b) => (a.file < b.file ? -1 : 1));

const drift: string[] = [];
if (!CHECK) mkdirSync(DIR, { recursive: true });
const want = new Map(cells.map((c) => [c.file, c.body]));
const have = existsSync(DIR) ? readdirSync(DIR).filter((f) => f.endsWith(".cddl")) : [];
for (const f of have)
  if (!want.has(f)) {
    if (CHECK) drift.push(`orphan fixture \`${f}\` (no longer in the projected set)`);
    else rmSync(`${DIR}/${f}`);
  }
for (const [f, body] of want) {
  const path = `${DIR}/${f}`;
  const cur = existsSync(path) ? readFileSync(path, "utf8") : null;
  if (CHECK) {
    if (cur === null) drift.push(`missing fixture \`${f}\``);
    else if (cur !== body) drift.push(`\`${f}\` content drift vs projection`);
  } else if (cur !== body) writeFileSync(path, body);
}

console.log(`wasm-ABI matrix projection: ${cells.length} cells (shape x role) -> tests/matrix_wasm/`);
if (CHECK) {
  if (drift.length) {
    console.log(`SNAPSHOT DRIFT (${drift.length}) — run \`bun run project_wasm_matrix.ts\` and review:`);
    for (const d of drift) console.log("  -", d);
    process.exit(1);
  }
  console.log("drift check OK: tests/matrix_wasm matches the projection");
}
