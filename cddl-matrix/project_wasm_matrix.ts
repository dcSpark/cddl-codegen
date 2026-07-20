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
 * The crux: wrapper-vs-transparent is a STRUCT-TABLE fact, not
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
// name to place in a role. Type names are kept multi-letter for historical reasons: a single-letter rule
// `r`/`w` USED to collide (E0574) with the reader/writer generics `R`/`W` the pre-cbor_event-3.x
// serialize/deserialize fns carried (fixed first by collision-proofing the generic names, then made
// structurally impossible when the 3.x de-generification dropped those fn generics; cddl-codegen also
// monomorphizes user generics, so no live `T` param survives either) — swept and pinned by
// `src/tests/identifier_hazard_tests.rs`. Names stay multi-letter to avoid re-minting every wasm fixture.
interface Shape {
  defs: string[]; // named-type definitions, authored dependency order (CDDL itself is order-free)
  ty: string; // the type placed in the role
  roles?: string[]; // restrict to these roles (default: all). Used for redundant / non-compiling shapes.
  skipRoles?: string[]; // emit all roles EXCEPT these (for a role that panics generation on this shape).
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
  // RESTRICTED non-empty list wrapper (`[+ T]` -> core `NonEmptyVec<T>`; the two-type-constraint
  // feature, draft/two-type-constraint-enforcement.md). Distinct wasm-ABI shape from `coll`: the
  // wrapper wraps `core::NonEmptyVec<u64>` and exposes a FAILABLE `try_from(elements: Vec<u64>) ->
  // Result<_, JsError>` (the single checked door; the CBOR decoder routes through the same one)
  // ALONGSIDE an infallible `new(first)` + infallible `add` (a push can never break a min-1 bound),
  // unlike `coll`'s all-infallible `new()`/`add`. Exposable element (`uint`), so `try_from` takes
  // the bare `Vec` BY VALUE (boundary copy, no ownership hazard) — the counterpart to `necollrec`'s
  // borrow+clone door.
  necoll: { defs: ["nums = [+ uint]"], ty: "nums" },
  // RESTRICTED non-empty list wrapper over a NON-exposable (record) element — the design's headline
  // two-wrapper pattern. Mints BOTH the loose builder (`FooList(Vec<cddl_lib::Foo>)`, today's `[* foo]`
  // wrapper) AND the restricted wrapper (`Recs(NonEmptyVec<cddl_lib::Foo>)`), the latter created via
  // `try_from(list: &FooList) -> Result<_, JsError>` which BORROWS the loose wrapper and CLONES its
  // contents (cloning sidesteps the wasm ownership problem; the JS-side `FooList` stays valid). A
  // distinct door from `necoll`'s by-value `try_from(Vec)`, and the exact surface WI-1 review found
  // three wasm-name bugs on (loose-builder minting + non-exposable element accessors).
  necollrec: { defs: ["foo = [a0: uint]", "recs = [+ foo]"], ty: "recs" },
  // RESTRICTED non-empty map wrapper (`{+ k => v}` -> core `NonEmptyMap<K, V>`). The map sibling of
  // `necollrec`: mints the loose `MapU64ToText` builder and the restricted `Mp(NonEmptyMap<u64,
  // String>)` created via `try_from(map: &MapU64ToText) -> Result<_, JsError>` (borrow + clone), with
  // an infallible `new(first_key, first_value)` + infallible `insert`. A distinct emitter/typedef path
  // from `necoll`/`necollrec` (map key/value accessors, `insert` vs `add`), mirroring how `collmap`
  // is distinct from `coll`.
  nemap: { defs: ["mp = { + uint => text }"], ty: "mp" },
  // transparent `pub type` -> Vec (the wrapper-vs-transparent distinction; shares IR shape with `coll`)
  passthru: { defs: ["nums = [* uint]", "pt = nums"], ty: "pt" },
  // transparent alias to a *map* typedef — the map/table typedef-resolution path (known-red: E0425)
  passthrumap: { defs: ["mp = { * uint => text }", "ptm = mp"], ty: "ptm" },
  // wrapper struct (Record RustStruct)
  struct: { defs: ["st = [a: uint, b: text]"], ty: "st" },
  // transparent alias to a Record RustStruct — distinct from `struct` at wasm ctor boundaries where
  // the rust ctor may take the alias wrapper as one argument instead of inlining the record fields.
  ralias: { defs: ["st = [a: uint, b: text]", "ral = st"], ty: "ral" },
  // map-representation Record struct (bareword-keyed map). Wasm emission is byte-identical to `struct`
  // modulo type names (the representation only changes rust-side serialization), so one representative
  // cell suffices — it still compile-gates the map-rep rust code through the wasm crate's path-dependency
  // and executes map-rep serialization in the roundtrips gate; full role coverage would only duplicate
  // `struct`'s cells.
  mstruct: { defs: ["mst = { a: uint, b: text }"], ty: "mst", roles: ["array-element"] },
  // transparent-to-wrapper via `.cbor` (follows the inner wrapper `Foo`)
  cborwrap: { defs: ["foo = [a: uint]", "fb = bytes .cbor foo"], ty: "fb" },
  // CBOR-tag wrapper struct — a distinct wasm-ABI shape: crosses via a wasm `new(inner)` ctor and an
  // inner-value `get()` accessor (plus `From<cddl_lib::Tg>` / cbor bytes), unlike `cborwrap`
  // (transparent-to-wrapper, which resolves to the inner `Foo` wrapper) and the `coll`/`collmap`
  // wrappers (which expose the richer `new`/`add`/`insert` collection API).
  tag: { defs: ["tg = #6.10(uint)"], ty: "tg" },
  // bounded/range wrapper struct — the ONLY `Result`-returning wasm `new`: `new(inner)` enforces the
  // `.size` bound and returns `Result<_, JsError>`, alongside the inner-value `get()`. Pins the
  // failable ctor + getter across roles (a bare `.size` range wraps WITHOUT `@newtype`).
  bwrap: { defs: ["bw = bytes .size (0..32)"], ty: "bw" },
  // c-style enum — Copy, re-exported by value (`pub use`)
  cenum: { defs: ["fe = 0 / 1 / 2"], ty: "fe" },
  // data-carrying type-choice enum -> a `#[wasm_bindgen]` wrapper enum with per-variant ctors; a
  // distinct wasm shape from the Copy c-style enum (`cenum`) and the Record `struct`.
  denum: { defs: ["denum = uint / text"], ty: "denum" },
  // nullable -> `Option<T>` at the boundary. A distinct wasm-ABI shape: `Option<T>` needs
  // `OptionIntoWasmAbi`, which nested positions (map value, optional field) don't satisfy for a
  // non-wrapper inner (known-red). map-key is pruned: a nullable map key is degenerate CDDL, and
  // its wasm bindings don't compile (`Option<u64>` key fails `ErasableGeneric` — E0271/E0277), so
  // un-pruning would only add a permanent SKIP cell. (The generation-time "special-typed map key"
  // assert this prune used to cite is gone — special-class keys now generate; see the
  // special_map_key corpus fixture.)
  nullable: { defs: ["opt = uint / null"], ty: "opt", skipRoles: ["map-key"] },
  // generic instance -> monomorphized wrapper struct
  generic: { defs: ["cont<T0> = [value: T0]", "uc = cont<uint>"], ty: "uc" },
  // --- Anonymous generic-COLLECTION-instance lowerings. The tag-set series gave types a new way to
  // cross the wasm boundary: an anonymous collection instance (`x: gcoll<elem>`) lowers to the
  // STRUCTURAL wrapper class for wrapper-needing elements and to the DIRECT bare-`Vec` exposure for
  // exposable ones (converged with the inline `[* elem]` spelling), while a NAMED instance rule keeps
  // its own-name class. `generic` above is the RECORD-instance sibling (a monomorphized wrapper
  // struct); these are the COLLECTION-instance shapes it doesn't cover.
  // anonymous instance over a NON-exposable (record) element -> lowers to the STRUCTURAL wrapper class
  // (`FooList`); the anonymous `gcoll<foo>` dedups to it, so the wasm surface names `FooList`, not a
  // rule-named `Gcoll…` class.
  gcolla: { defs: ["foo = [a0: uint]", "gcoll<e0> = [* e0]"], ty: "gcoll<foo>" },
  // anonymous instance over an EXPOSABLE (uint) element -> bare `Vec<u64>` exposure (no wrapper class):
  // `insert`/`get`/array accessors cross the bare `Vec`. Its map-key cell (`{ * gcoll<uint> => uint }`)
  // was a keys-list naming divergence (E0425: the keys-list wrapper was MINTED from the still-named
  // `Rust(GcollU64)` table domain as `GcollU64List` while `keys()` REFERENCED the resolved-domain
  // structural `ArrU64List`), fixed by deferring the keys-list mint to after the domain resolution
  // (`finalize_generic_table_keys_lists`) so both name the structural `ArrU64List`; pinned by
  // `exposable_generic_collection_instance_keyed_map_lowers_keys_list_structurally_under_wasm`.
  gcollexp: { defs: ["gcoll<e0> = [* e0]"], ty: "gcoll<uint>" },
  // NAMED-instance-rule boundary control: a `gcn = gcoll<foo>` rule keeps its OWN-name wasm class
  // (`Gcn`), distinct from the anonymous `gcolla` shape's structural-name lowering — the named-vs-anon
  // discriminant the collection-instance convergence turns on.
  gcolln: { defs: ["foo = [a0: uint]", "gcoll<e0> = [* e0]", "gcn = gcoll<foo>"], ty: "gcn" },
  // anonymous generic TABLE instance (the MAP-container sibling of `gcolla`): lowers to the structural
  // keyed wrapper (`MapU64ToText` via a `pub type GtblU64Text = MapU64ToText;` passthrough). A generic
  // table instance under `--wasm` once aborted generation on a duplicate-synthesized-ident collision
  // (the anonymous instance recorded as its own shape's sole owner) — the class its grid row makes red;
  // fixed + pinned by `generic_table_instance_lowers_to_structural_wrapper_under_wasm`.
  gtbla: { defs: ["gtbl<k0, v0> = { * k0 => v0 }"], ty: "gtbl<uint, text>" },
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
  // raw-bytes user-supplied type -> a `RawBytesEncoding` wrapper (`PubKey`) with From/AsRef but NO
  // wasm `new`. Named `pub_key` so the emitted type is `PubKey`, matching the in-repo defs
  // (`tests/external_{rust,wasm}_raw_bytes_def`) the gates append for `rawbytes__*` cells — so unlike
  // `extern`, these cells compile/round-trip rather than being a permanent SKIP. map-key is pruned: a
  // raw-bytes map key needs the user type to be `Ord` (`BTreeMap<PubKey, _>`), which a bare
  // `PubKey([u8; 32])` isn't (E0277) — ill-formed for a user-supplied inner, same shape as nullable's
  // degenerate map-key prune.
  rawbytes: { defs: ["pub_key = _CDDL_CODEGEN_RAW_BYTES_TYPE_"], ty: "pub_key", skipRoles: ["map-key"] },
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
  // array-rep 2-field. bareword-keyed map-rep fields (mandatory and `?`-optional) DO generate, but map-rep
  // field-holder roles emit byte-identical wasm to these array-rep roles (probed), so enumerating them
  // would only duplicate cells — the array representation stays as the single enumerated form. Map-rep
  // struct/optional-field serialization is executed by `tests/core` (its map-rep `Bar` has optional
  // fields) and the `mstruct` representative cell.
  "struct-field-opt": { wrap: (t) => `holder = [pre: uint, ? field0: ${t}]` }, // getter->Option<T>, set_field0
  // the `@newtype` wrapper always exposes a wasm `new(inner)` ctor and an inner-value `get()`
  // accessor (see docs/docs/wasm_differences.mdx § "Tag and @newtype wrappers"); a collection inner
  // crosses as its own collection wrapper class. The role exercises the wrapper boundary conversions.
  "newtype-inner": { wrap: (t) => `holder = ${t} ; @newtype` },
  // type-choice per-variant wasm ctor emission (`generate_type_choices_from_variants`) — a distinct
  // emission path from every container role above: the shape is placed as ONE arm of a type-choice
  // enum, so each shape mints a `Holder::new_<shape>` wasm ctor whose fallibility must match the rust
  // ctor's. The partner arm is `nint` because it is CBOR-disjoint from every shape's type (uint/text/
  // bytes/array/map/tag/null), so a decoder can discriminate the arms — needed by the round-trip gate.
  // A bool fixed-value partner (`false`) PANICS generation (`prelude.false` is in the panic catalog),
  // and the supported fixed values (uint/text literals) overlap prim/cenum/talias arms — so neither
  // can stand in as the disjoint arm.
  "tchoice-variant": { wrap: (t) => `holder = ${t} / nint` },
  // GROUP-choice per-variant wasm ctor emission (`codegen_group_choices` in generation/enums.rs) — the
  // group-choice sibling of `tchoice-variant`, exercising a DISTINCT emitter path (a `//` group choice
  // routes through `codegen_group_choices`' per-variant wasm ctor loop, not the type-choice
  // `generate_type_choices_from_variants` path). Each arm is a single named field, so the shape is
  // placed as `f0` in one arm and the partner `nint` as `f1` in the other; the emitter mints one
  // `Holder::new_<field>` wasm ctor per arm (`new_f0` for the shape, `new_f1` for the partner) — and a
  // shape whose rust ctor is failable (e.g. `bwrap`'s `.size` bound) crosses with the matching by-ref
  // `&Bw` + `.into()` conversion, so this pins the group-choice-arm ctor fallibility/conversion the
  // way `tchoice-variant` pins the type-choice one. The partner arm is `nint` for the same reason as
  // `tchoice-variant`: it is CBOR-disjoint from every shape's element type (uint/text/bytes/array/map/
  // tag/null), so a decoder can discriminate the two 1-element arms — needed by the round-trip gate; a
  // bool fixed-value partner PANICS generation and the supported uint/text literal fixed values overlap
  // prim/cenum/talias arms. ARRAY representation is the single enumerated form: the map-rep spelling
  // (`{ f0: ${t} // f1: nint }`) emits BYTE-IDENTICAL wasm (probed — the representation only changes
  // rust-side serialization), exactly as the `struct-field` role's comment records, so a map-rep role
  // would only duplicate cells (and a fixed-value entry in a map-rep group-choice arm panics generation
  // — the ledgered `contain.group-choice-arm.type2.value.map` PANIC row — so some map-rep templates
  // would not even generate).
  "gchoice-variant": { wrap: (t) => `holder = [ f0: ${t} // f1: nint ]` },
};

// A typo'd role name in `roles`/`skipRoles` would be a silent no-op that shrinks the projected grid
// (the only trace: an orphan-fixture deletion in a diff) — validate every entry against ROLES.
for (const [shape, s] of Object.entries(SHAPES))
  for (const r of [...(s.roles ?? []), ...(s.skipRoles ?? [])])
    if (!(r in ROLES))
      throw new Error(`SHAPES.${shape}: unknown role \`${r}\` (valid: ${Object.keys(ROLES).sort().join(", ")})`);

const cells: { file: string; body: string }[] = [];
for (const shape of Object.keys(SHAPES).sort()) {
  const { defs, ty, roles, skipRoles } = SHAPES[shape];
  for (const role of Object.keys(ROLES).sort()) {
    if (roles && !roles.includes(role)) continue;
    if (skipRoles?.includes(role)) continue;
    const lines = [`; cell: ${shape} x ${role}`, ...defs, ROLES[role].wrap(ty)];
    cells.push({ file: `${shape}__${role}.cddl`, body: lines.join("\n") + "\n" });
  }
}
cells.sort((a, b) => (a.file < b.file ? -1 : a.file > b.file ? 1 : 0));

// Grid shrink/growth must be an explicit, reviewed edit — not the byproduct of a filter change.
const EXPECTED_CELLS = 194; // 24 full shapes × 8 roles − 2 map-key skips (nullable, rawbytes) + 4 single-role shapes (chain, cborwrap2, extern, mstruct)
if (cells.length !== EXPECTED_CELLS)
  throw new Error(
    `wasm-ABI grid produced ${cells.length} cells, expected ${EXPECTED_CELLS} — if the change is deliberate, update EXPECTED_CELLS in the same commit`,
  );

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
