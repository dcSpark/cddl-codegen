#!/usr/bin/env bun
/**
 * multifile placement matrix projection — enumerate {type-shape} × {cross-module reference mode}
 * into two-module DIRECTORY fixtures under tests/matrix_multifile/<shape>__<mode>/.
 *
 * Sibling of project_wasm_matrix.ts. Where the wasm-ABI matrix individuates how a type crosses the
 * wasm-bindgen boundary, this individuates the axis EVERY other construct gate is blind to: MODULE
 * PLACEMENT. The corpus gates, the wasm-ABI matrix and the parity differential all feed the generator
 * SINGLE-file specs, so every construct is only ever verified in root scope. Multifile emission
 * branches on scope: `mark_refs` (intermediate/mod.rs) resolves the import source for the
 * generator-invented structural wrappers (`XList`/`MapKToV`), and each per-module `mod.rs` declares
 * only the submodules whose files it actually emits. This matrix is the systematic catcher for that
 * axis; the emitter invariants it guards (each once a loud `cargo check` failure class): a
 * cross-module anonymous same-shape use imports the structural name from the shape's owner module
 * (the E0432 class), a module declares `pub mod serialization;` only when it emits the file (the
 * E0583 class), and a cross-module named `.cbor` ref imports the inner type (the E0433 class).
 * Two Rust gates consume the fixtures. The always-on compile FLOOR
 * `integration_tests::multifile_matrix_compiles` generates each `--wasm=true` (directory input) and
 * `cargo check`s the wasm crate (which pulls the rust crate as a path dep, so rust-side breakage
 * surfaces through it) — mis-scoped placement fails loudly there as a specific red cell; any
 * deliberately-held class goes into that gate's `MULTIFILE_MATRIX_SKIP` ledger (currently: the
 * `collrec` array-structural-wrapper cells — the Array-arm placement class, enumerated after review
 * found the SHAPES hole; see the cddl-matrix/roadmap.toml finding). The behavioural upgrade
 * `integration_tests::multifile_matrix_roundtrips` (#[ignore]d, check.ts full tier) re-generates
 * each cell `--emit-tests=true` across all profiles and `cargo test`s BOTH generated subcrates, so
 * the cross-module wiring is EXECUTED (round-tripped) rather than only type-checked; its
 * deliberately-red cells live in `MULTIFILE_ROUNDTRIP_SKIP`/`MULTIFILE_ROUNDTRIP_PROFILE_SKIP`.
 *
 * Deterministic (sorted cell order; no hash-order). Fixtures go to tests/matrix_multifile/<cell>/{lib,a,b}.cddl
 * — except the `rootref` cells, whose reference lives in `lib.cddl` and which therefore carry no `b.cddl`.
 *
 * Run from cddl-matrix/:
 *   bun run project_multifile_matrix.ts          -> (re)writes tests/matrix_multifile/<cell>/*.cddl
 *   bun run project_multifile_matrix.ts --check  -> drift gate: fails if any fixture is stale/missing/orphaned
 */
import { readFileSync, existsSync, readdirSync, mkdirSync, writeFileSync, rmSync, statSync } from "node:fs";

const HERE = import.meta.dir;
const DIR = `${HERE}/../tests/matrix_multifile`;
const CHECK = process.argv.includes("--check");
const WASM_SHAPES_PATH = `${HERE}/project_wasm_matrix.ts`;

// This projection must not import its sibling: importing it writes fixtures. Instead parse the one
// bounded declaration we share, preserving the sibling's file-only/no-side-effect posture.
const WASM_ONLY_EXCLUSIONS = ["extern", "prim", "rawbytes"];

function skipTrivia(source: string, i: number): number {
  for (;;) {
    while (/\s/.test(source[i] ?? "")) i++;
    if (source.startsWith("//", i)) { const end = source.indexOf("\n", i + 2); i = end < 0 ? source.length : end + 1; continue; }
    if (source.startsWith("/*", i)) {
      const end = source.indexOf("*/", i + 2);
      if (end < 0) throw new Error("malformed SHAPES declaration: unterminated block comment");
      i = end + 2;
      continue;
    }
    return i;
  }
}

function skipQuoted(source: string, i: number): number {
  const quote = source[i++]!;
  while (i < source.length) {
    if (source[i] === "\\") { i += 2; continue; }
    if (source[i] === quote) return i + 1;
    i++;
  }
  throw new Error("malformed SHAPES declaration: unterminated string");
}

/** Parses exactly `const SHAPES: … = { … };`, without evaluating the sibling projection. */
export function wasmShapeNames(source: string): string[] {
  const declarations = [...source.matchAll(/\bconst\s+SHAPES\s*:/g)];
  if (declarations.length !== 1) throw new Error(`expected exactly one top-level SHAPES declaration, found ${declarations.length}`);
  let i = declarations[0]!.index! + declarations[0]![0].length;
  const equals = source.indexOf("=", i);
  if (equals < 0) throw new Error("malformed SHAPES declaration: missing '='");
  i = skipTrivia(source, equals + 1);
  if (source[i] !== "{") throw new Error("malformed SHAPES declaration: expected object literal");
  const bodyStart = ++i;
  let depth = 1;
  for (; i < source.length && depth; i++) {
    if (source.startsWith("//", i)) { i = skipTrivia(source, i) - 1; continue; }
    if (source.startsWith("/*", i)) { i = skipTrivia(source, i) - 1; continue; }
    if (["'", '"', "`"].includes(source[i]!)) { i = skipQuoted(source, i) - 1; continue; }
    if (source[i] === "{") depth++;
    if (source[i] === "}") depth--;
  }
  if (depth !== 0) throw new Error("malformed SHAPES declaration: unclosed object literal");
  const bodyEnd = i - 1;
  if (source[skipTrivia(source, i)] !== ";") throw new Error("malformed SHAPES declaration: object must end with ';'");

  const names: string[] = [];
  const seen = new Set<string>();
  let nesting = 0;
  for (let p = bodyStart; p < bodyEnd;) {
    p = skipTrivia(source, p);
    if (p >= bodyEnd) break;
    if (["'", '"', "`"].includes(source[p]!)) { p = skipQuoted(source, p); continue; }
    const ch = source[p]!;
    if ("{[(".includes(ch)) { nesting++; p++; continue; }
    if ("}])".includes(ch)) { nesting--; p++; continue; }
    if (nesting === 0) {
      if (ch === ",") { p++; continue; }
      const name = /^([A-Za-z_$][A-Za-z0-9_$]*)\s*:/.exec(source.slice(p));
      if (name) {
        if (seen.has(name[1]!)) throw new Error(`duplicate SHAPES key '${name[1]}'`);
        seen.add(name[1]!);
        names.push(name[1]!);
        p += name[0].length;
        continue;
      }
      throw new Error(`malformed SHAPES declaration: unsupported top-level property syntax near '${source.slice(p, p + 32)}'`);
    }
    p++;
  }
  if (nesting !== 0) throw new Error("malformed SHAPES declaration: unbalanced nested syntax");
  return names.sort();
}

export function assertWasmShapeSuperset(wasmSource: string, localNames: Iterable<string>, exclusions = WASM_ONLY_EXCLUSIONS): void {
  const wasm = new Set(wasmShapeNames(wasmSource));
  const local = new Set(localNames);
  for (const exclusion of exclusions) {
    if (!wasm.has(exclusion)) throw new Error(`WASM_ONLY_EXCLUSIONS contains stale '${exclusion}' (absent from wasm SHAPES)`);
    if (local.has(exclusion)) throw new Error(`WASM_ONLY_EXCLUSIONS contains '${exclusion}', but it is present in multifile SHAPES`);
  }
  const missing = [...wasm].filter(name => !local.has(name) && !exclusions.includes(name)).sort();
  if (missing.length) throw new Error(`multifile SHAPES is missing eligible wasm shape(s): ${missing.join(", ")}`);
}

function supersetSelfTests(): void {
  const source = "const SHAPES: Record<string, Shape> = { alpha: {}, prim: {}, extern: {}, rawbytes: {} };";
  const expectThrow = (label: string, f: () => void): void => {
    try { f(); } catch { return; }
    throw new Error(`SHAPES superset self-test: ${label} was accepted`);
  };
  assertWasmShapeSuperset(source, ["alpha", "extra"], WASM_ONLY_EXCLUSIONS);
  expectThrow("missing eligible wasm shape", () => assertWasmShapeSuperset(source, [], WASM_ONLY_EXCLUSIONS));
  expectThrow("stale exclusion", () => assertWasmShapeSuperset(source, ["alpha"], ["prim", "stale"]));
  expectThrow("malformed declaration", () => wasmShapeNames("const SHAPES: Record<string, Shape> = { alpha: {}"));
  expectThrow("duplicate declaration key", () => wasmShapeNames("const SHAPES: Record<string, Shape> = { alpha: {}, alpha: {} };"));
  expectThrow("unsupported quoted key", () => wasmShapeNames("const SHAPES: Record<string, Shape> = { \"alpha\": {} };"));
}
supersetSelfTests();

// --- Axis 1: type-shapes. Defs + `ty` copied verbatim (provenance) from project_wasm_matrix.ts's
// `SHAPES` — do NOT import it (that module runs projection on import) — plus any multifile-SPECIFIC
// placement-only shapes. Those extras are allowed because their cross-module placement class has no
// wasm root-scope counterpart; `collrec` and `tblrec` are representative examples, not a closed list.
// Included: every shape that HAS
// defs and is self-contained (can compile standalone). `anonForm` is the shape's inline anonymous
// same-shape spelling (the `mark_refs` structural-wrapper class); present iff the anon holder
// `holder = [field0: <anonForm>]` compiles GREEN as a single-file spec — verified once when the
// shape lands (throwaway generate + `cargo check` rust+wasm). All 11 anon-cell candidates probed
// green (the EXPECTED_ANON_SHAPES list below), so all 11 admit an anon cell; a red
// there would be a single-file limitation, not a placement finding, and the shape would carry no
// `anonForm`. (The generic-instance and `@duplicates` directive shapes carry no `anonForm` by
// construction — an instance `ty` IS the anonymous spelling, and the per-rule directive has no
// inline spelling — so they are not anon-cell candidates at all; see their entry comments.)
//
// Excluded shapes (present in the wasm matrix, deliberately absent here):
//   - `prim` (`ty: uint`, no defs) — nothing to PLACE in a module; a module needs at least one rule.
//   - `extern` / `rawbytes` — user-supplied types (`_CDDL_CODEGEN_EXTERN_TYPE_` / raw-bytes); can't
//     compile standalone (same permanent exclusion the wasm matrix carries for `extern`). The
//     exclusion bounds this gate, not the generator — extern shapes still have placement behavior,
//     and its alias-position residue escaped to production once (feature request 07: generic-extern
//     instance alias targets are `Base<Args>` type expressions). Hand-pinned by
//     tests/extern-generic-scoped; a generation-only excluded-shapes leg is recorded recur-first in
//     tests/testing-roadmap.toml ("Multifile reference-POSITION coverage").
interface Shape {
  defs: string[]; // named-type definitions -> module `a` (authored dependency order; CDDL is order-free)
  ty: string; // the shape's named rule, referenced cross-module by the `named` mode
  anonForm?: string; // inline anonymous same-shape spelling -> the `anon`/`anonb` modes' `b.cddl` field type
  // `anonb` (anon + ballast) participation: if the phantom-`pub mod serialization;` class (E0583)
  // ever regresses, the plain `anon` cell of an alias/table-only module `a` would red on it FIRST,
  // masking the b-side structural-import verdict (the E0432 class). `anonb` adds a ballast record
  // rule to `a` so it always emits serialization and the b-side reference verdict stays visible
  // regardless. Set ONLY on the shapes whose module `a` is alias/table-only (coll, collmap,
  // nullable); the other anon shapes' module `a` already emits serialization, so a ballast variant
  // adds no discrimination and they are excluded from `anonb`.
  anonBallast?: boolean;
  // `rootref` participation: the shape's anon spelling, placed in the ROOT scope instead of module
  // `b`. Set ONLY on the WRAPPER-MINTING anon shapes — the ones whose root cell actually makes the
  // root scope resolve a generator-INVENTED structural class (`MapKToV` / `XList` / their
  // `NonEmpty*` twins), which is the whole subject of the mode; a shape whose anon form lowers to a
  // transparent core type (`coll` -> `Vec<u64>`, `necoll` -> `NonEmptyVec<u64>`, `nullable` ->
  // `Option<u64>`, `tag`, `bwrap`) or to a plain user-named rule (`cborwrap`'s `Foo`) puts nothing
  // structural in root's way, so its rootref cell would duplicate `anon` with the holder moved.
  // Membership is a PROBE outcome (each participating shape's root `generated/mod.rs` was read for
  // the structural name it resolves), pinned by EXPECTED_ROOTREF_SHAPES below.
  rootRef?: boolean;
}
const SHAPES: Record<string, Shape> = {
  palias: { defs: ["pa = uint"], ty: "pa" },
  talias: { defs: ["ta = text"], ty: "ta" },
  coll: { defs: ["nums = [* uint]"], ty: "nums", anonForm: "[* uint]", anonBallast: true },
  collmap: { defs: ["mp = { * uint => text }"], ty: "mp", anonForm: "{ * uint => text }", anonBallast: true, rootRef: true },
  // RESTRICTED non-empty list over an EXPOSABLE element (`[+ uint]` -> `NonEmptyVec<u64>`, the
  // two-type-constraint feature). Placement mirrors `coll`: the element is transparent, so module `a`
  // is alias-only (no serialization) and the shape needs no cross-module structural element wrapper —
  // gated here to confirm the `NonEmptyVec` alias imports cross-module the same as a bare `Vec`.
  necoll: { defs: ["nums = [+ uint]"], ty: "nums", anonForm: "[+ uint]", anonBallast: true },
  // RESTRICTED non-empty list over a NON-exposable (record) element (`[+ foo]`) — the `+` analogue of
  // `collrec`. Mints the loose structural wrapper (`FooList`) AND the restricted wrapper cross-module,
  // so it hits the SAME `mark_refs` Array-arm structural-wrapper placement class as `collrec` (the
  // ROOT_SCOPE hard-code — the remaining issue-138 half; cddl-matrix/roadmap.toml § findings). Both
  // reference modes are known-red (MULTIFILE_MATRIX_SKIP, citing that finding); the single-file anon
  // control is green (like `collrec`/`cborwrap`, the anon form references the named `foo` cross-module).
  necollrec: { defs: ["foo = [a0: uint]", "recs = [+ foo]"], ty: "recs", anonForm: "[+ foo]", rootRef: true },
  // RESTRICTED non-empty map (`{+ k => v}` -> `NonEmptyMap<K, V>`) — the `+` analogue of `collmap`.
  // Placement mirrors `collmap` (the map arm is sole-owner-aware), so both reference modes are green;
  // module `a` is table-alias-only, so it carries `anonBallast` like `collmap`.
  nemap: { defs: ["mp = { + uint => text }"], ty: "mp", anonForm: "{ + uint => text }", anonBallast: true, rootRef: true },
  // SYNTHESIZED-NonEmpty facet: an inline `[+ foo]` / `{+ uint => foo}` over a NON-exposable (record)
  // element with NO named collection rule anywhere, so the restricted wrapper synthesizes
  // `NonEmptyFooList` / `NonEmptyMapU64ToFoo` at ROOT (no dedup-to-named — unlike `necollrec`/`nemap`,
  // whose module `a` carries a named `[+ …]`/`{+ …}` rule the inline occurrence dedups to). This is
  // the facet the import tracker got wrong independently of the ledgered restricted cells: it imported
  // the pre-NonEmpty `FooList`/`MapU64ToFoo` spelling while the emitter names the `NonEmpty*` wrapper
  // bare (E0425). `ty: foo` (a plain record) drives the named/aliased/unref modes; the discriminating
  // cell is `anon` (the inline synthesized wrapper referenced cross-module). `wasm_collection_wrapper`
  // resolves the wrapper name+home so both compile green.
  nesyncoll: { defs: ["foo = [a0: uint]"], ty: "foo", anonForm: "[+ foo]", rootRef: true },
  nesynmap: { defs: ["foo = [a0: uint]"], ty: "foo", anonForm: "{ + uint => foo }", rootRef: true },
  // Named NESTED restricted map (the CML issue's "placement half"): the outer `{+ …}` rule's
  // `try_from(&MapU64ToMapU64ToText)` loose source is itself keyed on the inner `{+ …}`'s loose
  // `MapU64ToText` builder, all minted at the rule's emission scope. Witnessed green (the map
  // struct-walk arm registers each loose source + key/value at its emission scope). No `anonForm` —
  // referenced by its named rule (the nested inline spelling would only duplicate `named`).
  nenestmap: { defs: ["nn = { + uint => { + uint => text } }"], ty: "nn" },
  passthru: { defs: ["nums = [* uint]", "pt = nums"], ty: "pt" },
  passthrumap: { defs: ["mp = { * uint => text }", "ptm = mp"], ty: "ptm" },
  struct: { defs: ["st = [a: uint, b: text]"], ty: "st" },
  // transparent alias to a Record struct — like passthru/passthrumap but with a record target, so
  // the alias-target recursion in `mark_refs` descends into a type with its own wasm wrapper (the
  // shape whose wasm-matrix gchoice cell exposed the ctor alias-resolution divergence)
  ralias: { defs: ["st = [a: uint, b: text]", "ral = st"], ty: "ral" },
  mstruct: { defs: ["mst = { a: uint, b: text }"], ty: "mst" },
  // cborwrap's anon form references the named `foo` (which lives in module `a`) — a cross-module named
  // ref embedded in an anonymous `.cbor` wrapper. It still individuates the anon-placement class (the
  // `.cbor` wrapper resolution under module scope) and its single-file control is green, so it is kept.
  cborwrap: { defs: ["foo = [a: uint]", "fb = bytes .cbor foo"], ty: "fb", anonForm: "bytes .cbor foo" },
  // Collection of NON-exposable elements: the array whose wasm representation needs a generated
  // structural wrapper (`FooList`), i.e. the Array-arm sibling of `collmap`'s structural-map class —
  // `mark_refs`' Array arm still hard-codes ROOT_SCOPE as the wrapper's import source (the remaining
  // issue-138 half). `coll` ([* uint]) is transparent `Vec<u64>` and can never probe this. Both
  // reference modes are known-red (see MULTIFILE_MATRIX_SKIP + the cddl-matrix/roadmap.toml finding);
  // the shape was enumerated AFTER review found the hole — the single-file anon control is green
  // (like cborwrap, the anon form references the named `foo` cross-module).
  collrec: { defs: ["foo = [a0: uint]", "recs = [* foo]"], ty: "recs", anonForm: "[* foo]", rootRef: true },
  // Table keyed by a NON-exposable (record) key (`{ * foo => text }`) in a non-root module. The
  // sole-owner table class is emitted in module `a`, and its `keys()` accessor names the root-minted
  // keys-list wrapper (`FooList`) bare — but that wrapper is minted at ROOT_SCOPE and, until the
  // mark_refs keys-list registration lands, never imported into module `a` (E0425). `collmap`
  // (`{ * uint => text }`) has an EXPOSABLE key, so its `keys()` returns a bare `Vec<u64>` and can
  // never probe this class. Module `a` emits serialization (the `foo` record), so no anonBallast is
  // needed (like `collrec`); the single-file anon control is green (root scope has no cross-module
  // import to dangle).
  tblrec: { defs: ["foo = [a0: uint]", "tbl = { * foo => text }"], ty: "tbl", anonForm: "{ * foo => text }", rootRef: true },
  // OPEN TABLE keyed by a NON-exposable (record) typed key — the open-table sibling of `tblrec`, and
  // the placement class its flattening creates. The minted struct's wasm class carries the typed
  // row's `keys()` ITSELF (flattened, not on a container class), so the keys-list wrapper is named
  // bare in the STRUCT's module while the wrapper is minted at ROOT — and, unlike a table's, that
  // wrapper is not an IR Array struct, so no struct-walk arm reaches its body either. `tblrec` cannot
  // probe this: its `keys()` lives on the table's own class, whose emission scope the container arm
  // already tracks. Typed key `foo` (a record, major 4) beside a `text` catch-all (major 3), so the
  // complement is non-empty. No `anonForm`: an open table is a NAMED-RULE concession — an inline
  // anonymous one is refused at recognition — so there is no inline spelling to place.
  otblrec: { defs: ["foo = [a0: uint]", "otbl = { * foo => text, * text => text }"], ty: "otbl" },
  // OPEN TABLE — verbatim wasm SHAPES provenance: `ot = { * bstr => uint, * text => uint }`,
  // `ty: ot`. This all-exposable counterpart to `otblrec` has no inline form (open tables are
  // named-rule-only), but its named/aliased/unref cells place the distinct flattened open-table
  // emission path in a non-root module.
  otbl: { defs: ["ot = { * bstr => uint, * text => uint }"], ty: "ot" },
  tag: { defs: ["tg = #6.10(uint)"], ty: "tg", anonForm: "#6.10(uint)" },
  bwrap: { defs: ["bw = bytes .size (0..32)"], ty: "bw", anonForm: "bytes .size (0..32)" },
  cenum: { defs: ["fe = 0 / 1 / 2"], ty: "fe" },
  denum: { defs: ["denum = uint / text"], ty: "denum" },
  nullable: { defs: ["opt = uint / null"], ty: "opt", anonForm: "uint / null", anonBallast: true },
  generic: { defs: ["cont<T0> = [value: T0]", "uc = cont<uint>"], ty: "uc" },
  // Anonymous generic-COLLECTION-instance lowerings (copied verbatim-with-provenance from the wasm
  // projection's `SHAPES`). No `anonForm`: a generic-instance shape's `ty` IS already an anonymous
  // same-shape spelling (`gcoll<foo>` / `gcoll<uint>` / `gtbl<uint, text>`), so the `anon`/`anonb`
  // modes would only duplicate what `named`/`aliased` already place — there is no separate inline
  // spelling to distinguish. `gcolln`'s `ty` is a NAMED rule (`gcn`), which likewise has no distinct
  // anonymous spelling here beyond the instance itself.
  // anonymous instance over a NON-exposable (record) element -> the structural `FooList` wrapper; the
  // anon-instance cross-module placement is green (the Array arm registers the element ref from the
  // wrapper's root emission scope, like `collrec__anon`).
  gcolla: { defs: ["foo = [a0: uint]", "gcoll<e0> = [* e0]"], ty: "gcoll<foo>" },
  // anonymous instance over an EXPOSABLE (uint) element -> transparent `Vec<u64>`; no structural
  // wrapper crosses, so every reference mode is green (like `coll`, whose element is also transparent).
  gcollexp: { defs: ["gcoll<e0> = [* e0]"], ty: "gcoll<uint>" },
  // NAMED generic-collection-instance rule over a record element (`gcn = gcoll<foo>`) — the
  // generic-instance twin of `collrec` (`recs = [* foo]`): its NAMED cross-module reference dangles on
  // the root-minted structural `FooList` wrapper name (E0432), the residual mark_refs Array-arm
  // structural-WRAPPER-NAME ROOT_SCOPE class (cddl-matrix/roadmap.toml § findings). `gcolln__named` is
  // known-red (MULTIFILE_MATRIX_SKIP + MULTIFILE_ROUNDTRIP_SKIP); `aliased`/`unref` are green (the
  // alias-base mint + `scope_references` type-alias walk cover the base, like `collrec__aliased`).
  gcolln: { defs: ["foo = [a0: uint]", "gcoll<e0> = [* e0]", "gcn = gcoll<foo>"], ty: "gcn" },
  // anonymous generic TABLE instance -> the structural keyed wrapper (`MapU64ToText`); the sole-owner
  // table machinery is scope-aware, so every reference mode is green (like `collmap`).
  gtbla: { defs: ["gtbl<k0, v0> = { * k0 => v0 }"], ty: "gtbl<uint, text>" },
  // `@duplicates reject` OrderedSet ABI class (copied verbatim-with-provenance from the wasm
  // projection's `SHAPES`). Element is EXPOSABLE (`uint`), so — like `coll`/`necoll` — the reject twin
  // is a transparent core-type alias (`OrderedSet<u64>` / `NonEmptyOrderedSet<u64>`) with no
  // structural ELEMENT wrapper minted at root, so no cross-module structural name can dangle: every
  // reference mode is green (probed). No `anonForm`: `@duplicates reject` is a per-RULE directive with
  // no inline anonymous spelling for the named `rset`/`nerset`, and the anonymous-instance shapes'
  // `ty` (`oset<uint>` / `neoset<uint>`) IS already the anonymous same-shape spelling — the `anon`
  // modes would only duplicate what `named`/`aliased` place.
  // named `[*]`/`[+]` reject rules — transparent alias to the uniqueness twin, imported cross-module
  // like a bare `Vec`/`NonEmptyVec` (the `coll`/`necoll` placement class).
  rset: { defs: ["rs = [* uint] ; @duplicates reject"], ty: "rs" },
  nerset: { defs: ["nrs = [+ uint] ; @duplicates reject"], ty: "nrs" },
  // BOUNDED reject set — verbatim wasm SHAPES provenance: `brs = [*2 uint] ; @duplicates reject`,
  // `ty: brs`. Unlike loose/non-empty reject sets this emits the bounds-bearing runtime wrapper;
  // named/aliased/unref cells place both its rule-owned wrapper and alias target across modules.
  brset: { defs: ["brs = [*2 uint] ; @duplicates reject"], ty: "brs" },
  // Anonymous generic bounded reject set — verbatim wasm SHAPES provenance:
  // `boset<e0> = [2*3 e0] ; @duplicates reject`, `ty: boset<uint>`. Its structural
  // bounds-bearing wrapper is reached through the same three reference modes, including the
  // alias-only module that does not otherwise mention its generated structural name.
  brseta: { defs: ["boset<e0> = [2*3 e0] ; @duplicates reject"], ty: "boset<uint>" },
  // anonymous generic reject-set instances over an exposable element — no structural wrapper crosses
  // (like `gcollexp`), so every reference mode is green.
  rseta: { defs: ["oset<e0> = [* e0] ; @duplicates reject"], ty: "oset<uint>" },
  nerseta: { defs: ["neoset<e0> = [+ e0] ; @duplicates reject"], ty: "neoset<uint>" },
  // Reject rules over a NON-exposable (record) element — the reject sibling of `collrec`/`necollrec`.
  // Unlike `rset`/`nerset` (exposable `uint` element, no structural element wrapper), the record
  // element mints a loose `FooList` at ROOT that the rule's `try_from(&FooList)` names bare, so the
  // struct-walk Array arm must register the loose source for reject rules of ANY bounds (keyed on
  // `duplicates == Reject`, LOCKSTEP with `generate_reject_ordered_set_type`'s `loose_list`). `rsetrec`
  // (a plain `[*]` reject rule) probes the gap the non-empty-bound gate left open — E0425 on `FooList`
  // (at the rule's scope) and its element `Foo` (at ROOT) before the fix; `nersetrec` (`[+]`) was
  // already covered by the non-empty bound but is witnessed regardless. No `anonForm` (per-RULE
  // directive, like `rset`/`nerset`).
  rsetrec: { defs: ["foo = [a0: uint]", "rs = [* foo] ; @duplicates reject"], ty: "rs" },
  nersetrec: { defs: ["foo = [a0: uint]", "nrs = [+ foo] ; @duplicates reject"], ty: "nrs" },
  // Anonymous generic reject-set instances over a record element (the `gcolla`/`rseta` pattern): the
  // inline Array arm registers the loose source + element from the wrapper's emission scope, so every
  // reference mode is green — witnessed here to lock the anon-instance reject placement. No `anonForm`
  // (the instance `ty` IS the anonymous same-shape spelling).
  rsetarec: { defs: ["foo = [a0: uint]", "oset<e0> = [* e0] ; @duplicates reject"], ty: "oset<foo>" },
  nersetarec: { defs: ["foo = [a0: uint]", "neoset<e0> = [+ e0] ; @duplicates reject"], ty: "neoset<foo>" },
  // `@duplicates preserve` PairMap ABI class (copied verbatim-with-provenance from the wasm
  // projection's `SHAPES`). No `anonForm`: `@duplicates preserve` is a per-RULE directive with no
  // inline anonymous spelling for the named `pmap`/`nepmap`, and the anonymous-instance shapes' `ty`
  // (`ptbl<uint, text>` / `neptbl<uint, text>`) IS already the anonymous same-shape spelling — the
  // `anon` modes would only duplicate what `named`/`aliased` place.
  // LOOSE `{*}` pair maps (named `pmap` + anon-instance `pmapa`) mirror `collmap`/`gtbla` (the map arm
  // is sole-owner-aware), so every reference mode is green (probed).
  pmap: { defs: ["pm = { * uint => text } ; @duplicates preserve"], ty: "pm" },
  pmapa: { defs: ["ptbl<k0, v0> = { * k0 => v0 } ; @duplicates preserve"], ty: "ptbl<uint, text>" },
  // The RESTRICTED `{+}` NonEmptyPairMap wrappers reach the SAME structural-wrapper ROOT_SCOPE class
  // as `nemap`: the restricted wrapper's `try_from(&MapU64ToText)` names the root-minted loose pair-map
  // builder bare from a non-root module (E0425). `nepmap` (a named `{+}` rule) mints its wrapper in
  // module `a`, so all three modes are red; `nepmapa` (an anon instance) is red only in `aliased` — the
  // alias-base mint walk drops the loose-builder import, while the field-driven `named` mode imports it
  // and `unref` materializes no wrapper (probed). Pinned by MULTIFILE_MATRIX_SKIP /
  // MULTIFILE_ROUNDTRIP_SKIP citing the structural-wrapper ROOT_SCOPE findings entry.
  nepmap: { defs: ["npm = { + uint => text } ; @duplicates preserve"], ty: "npm" },
  nepmapa: { defs: ["neptbl<k0, v0> = { + k0 => v0 } ; @duplicates preserve"], ty: "neptbl<uint, text>" },
  chain: { defs: ["ca = [* uint]", "cb = ca", "cc = cb"], ty: "cc" },
  cborwrap2: { defs: ["foo = [a: uint]", "fb = bytes .cbor foo", "fb2 = fb"], ty: "fb2" },
};

// --- Axis 2: cross-module reference mode. The shape's defs go in module `a`; module `b` holds one
// `bholder = [field0: <...>]` record (or, for `aliased`, one plain alias rule). `named` references
// the shape's named rule cross-module; `aliased` ALIASES it (`bal = <ty>` — a plain rule alias, so
// module `b` emits `pub type Bal = …;` naming the cross-module target with no field reference in
// sight: the `scope_references` type-alias walk, which the field-driven modes can never probe — the
// reference mode a consumer's `policy_id = script_hash`-style domain naming hits, proven E0412 in
// production before the walk landed); `anon`
// embeds the shape's inline anonymous same-shape spelling (the `mark_refs` structural class); `anonb`
// is `anon` with a ballast record rule added to module `a` (so `a` always emits serialization and an
// alias-only-module E0583 regression can't mask the b-side import verdict — see `anonBallast` above);
// `unref` references nothing (module `a` still declares the shape — the cell where an
// alias/table-only module would mis-declare `pub mod serialization;` if the E0583 class regressed).
// `rootref` moves the referencing holder into the ROOT scope (`lib.cddl`) and emits NO module `b` at
// all — the REFERENCING-MODULE axis every other mode holds constant at `b`. Root is where
// `mark_refs` resolves a structural wrapper differently: a wrapper sole-owned by module `a` must be
// imported into root (`use a::MapU64ToText;`), while one the root holder itself owns is minted AT
// root and named bare with no import. Both halves are unreachable from a `b`-side reference.
// Root-owner direction (shape in root, referenced from a module) is
// deliberately NOT enumerated: root-module owners probed fine in BOTH directions, so the
// non-root-owner cells above are the discriminating ones. `bholder`/`rootholder`/`field0`/`bal0`
// dodge the `R`/`W`/`T` reader/writer/generic letters.
interface Mode {
  // returns module `b`'s content, or null if the shape does not participate in this mode.
  // EXACTLY ONE of `b` / `root` is set (asserted below): a `root` mode places its reference in the
  // ROOT scope and the cell carries no `b.cddl` at all.
  b?: (s: Shape) => string | null;
  // returns the rules appended to `lib.cddl` (after the constant root rule), or null if the shape
  // does not participate in this mode.
  root?: (s: Shape) => string | null;
  aExtra?: (s: Shape) => string[]; // extra rules appended to module `a`'s defs for this mode
}
const MODES: Record<string, Mode> = {
  // alias-only module `b` on purpose (no bholder ballast): that IS the production shape (a module
  // of `pub type` domain aliases), and the alias-only-module E0583 class is independently pinned
  // by the alias shapes' `unref` cells, so a red here stays attributable to the alias-walk import.
  aliased: { b: (s) => `bal = ${s.ty}` },
  anon: { b: (s) => (s.anonForm ? `bholder = [field0: ${s.anonForm}]` : null) },
  anonb: {
    b: (s) => (s.anonBallast && s.anonForm ? `bholder = [field0: ${s.anonForm}]` : null),
    aExtra: () => ["ballast = [bal0: uint]"],
  },
  named: { b: (s) => `bholder = [field0: ${s.ty}]` },
  // The referencing-MODULE variation of `anon`: same inline anonymous spelling, placed at ROOT.
  rootref: { root: (s) => (s.rootRef && s.anonForm ? `rootholder = [field0: ${s.anonForm}]` : null) },
  unref: { b: () => `bholder = [field0: uint]` },
};
for (const [name, m] of Object.entries(MODES))
  if (!m.b === !m.root)
    throw new Error(`MODES.${name}: set EXACTLY one of \`b\` / \`root\` — a mode places its reference in module \`b\` or in the root scope, never both/neither`);

// The anon subset is a reviewed fact (which shapes have a single-file-green anon form): a mis-typed
// `anonForm` key would silently drop a shape from `anon` (TS excess-property check catches an unknown
// key at the literal, but not a value dropped by a downstream filter) — pin the derived set so any
// grid shrink/growth is an explicit reviewed edit, exactly like EXPECTED_CELLS below.
const EXPECTED_ANON_SHAPES = ["bwrap", "cborwrap", "coll", "collmap", "collrec", "necoll", "necollrec", "nemap", "nesyncoll", "nesynmap", "nullable", "tag", "tblrec"];
const anonShapes = Object.keys(SHAPES)
  .filter((k) => SHAPES[k].anonForm)
  .sort();
if (JSON.stringify(anonShapes) !== JSON.stringify(EXPECTED_ANON_SHAPES))
  throw new Error(
    `anon-form shape set is [${anonShapes.join(", ")}], expected [${EXPECTED_ANON_SHAPES.join(", ")}] — ` +
      `if the change is deliberate (a probe outcome changed), update EXPECTED_ANON_SHAPES in the same commit`,
  );

// Same idiom for the `anonb` subset (exactly the alias/table-only-module `anon` shapes — see `anonBallast`).
const EXPECTED_ANONB_SHAPES = ["coll", "collmap", "necoll", "nemap", "nullable"];
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

// Same idiom for the `rootref` subset (the WRAPPER-MINTING anon shapes — see `rootRef`). Each entry
// is a probe outcome, recorded as the structural name its ROOT scope has to resolve:
//   collmap    `use a::MapU64ToText;`          — sole-owner map wrapper imported from module `a`
//   collrec    `FooList` minted AT root        — root holder owns the `[* foo]` occurrence
//   necollrec  `FooList` at root + `use a::{Foo, Recs}`
//   nemap      `MapU64ToText` at root + `use a::Mp`
//   nesyncoll  `FooList` + `NonEmptyFooList` minted AT root (the synthesized-wrapper facet)
//   nesynmap   `MapU64ToFoo` + `NonEmptyMapU64ToFoo` minted AT root
//   tblrec     `use a::{Foo, MapFooToText};`   — sole-owner table wrapper + its root keys-list
const EXPECTED_ROOTREF_SHAPES = ["collmap", "collrec", "necollrec", "nemap", "nesyncoll", "nesynmap", "tblrec"];
const rootrefShapes = Object.keys(SHAPES)
  .filter((k) => SHAPES[k].rootRef)
  .sort();
if (JSON.stringify(rootrefShapes) !== JSON.stringify(EXPECTED_ROOTREF_SHAPES))
  throw new Error(
    `rootref shape set is [${rootrefShapes.join(", ")}], expected [${EXPECTED_ROOTREF_SHAPES.join(", ")}] — ` +
      `if the change is deliberate (a probe outcome changed), update EXPECTED_ROOTREF_SHAPES in the same commit`,
  );
for (const k of rootrefShapes)
  if (!SHAPES[k].anonForm)
    throw new Error(`SHAPES.${k}: rootRef without anonForm — rootref reuses the anon spelling, so it needs one`);

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
    const m = MODES[mode];
    // shape does not participate in this mode (anon without a green form; rootref without a
    // wrapper-minting one)
    const b = m.b ? m.b(s) : null;
    const root = m.root ? m.root(s) : null;
    if (b === null && root === null) continue;
    const aRules = [...s.defs, ...(m.aExtra?.(s) ?? [])];
    cells.push({
      dir: `${shape}__${mode}`,
      files: {
        "lib.cddl":
          root === null
            ? LIB_CDDL
            : `${LIB_CDDL}; cell: ${shape} x ${mode} (reference from the ROOT scope)\n${root}\n`,
        "a.cddl": `; cell: ${shape} x ${mode} (shape defs, module a)\n${aRules.join("\n")}\n`,
        ...(b === null ? {} : { "b.cddl": `; cell: ${shape} x ${mode} (reference from module b)\n${b}\n` }),
      },
    });
  }
}
cells.sort((a, b) => (a.dir < b.dir ? -1 : a.dir > b.dir ? 1 : 0));

// Grid shrink/growth must be an explicit, reviewed edit — not the byproduct of a filter change.
const EXPECTED_CELLS = 163; // 46 shapes × {aliased, named, unref} = 138 + 13 anon-form shapes × {anon} + 5 anonb shapes × {anonb} + 7 rootref shapes × {rootref} -> 163
if (cells.length !== EXPECTED_CELLS)
  throw new Error(
    `multifile grid produced ${cells.length} cells, expected ${EXPECTED_CELLS} — if the change is deliberate, update EXPECTED_CELLS in the same commit`,
  );

// The sibling's definitions are the wasm-boundary authority. Every standalone-compilable wasm
// shape belongs here too; the only exclusions are the exact structural ledger above. Multifile-specific
// placement-only shapes are intentionally allowed as local extras.
assertWasmShapeSuperset(readFileSync(WASM_SHAPES_PATH, "utf8"), Object.keys(SHAPES));

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
