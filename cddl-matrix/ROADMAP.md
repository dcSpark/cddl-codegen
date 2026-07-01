# CDDL master matrix — roadmap (remaining work + findings)

Read `README.md` first (the model + current state). This doc is the forward-looking list **and** the
hard-won findings — the two things a future agent can't re-derive from the code. The blow-by-blow of *how*
the done work landed lives in git history (this doc was pruned of it; the project already did the same with
the scale report + cold-critique).

**Status: gate-green.** 92 features (81 RFC8610 + 1 RFC9682 + 10 `CDDL_CODEGEN` vendor profile), all axes
reconciled/deterministic, with compile-gated execution-grounded support **per-feature, per-cell
(role × feature), and per-control-op** (all 37 IANA ops probed). Both flagship projections GENERATE their
hand docs and drift-check: `golden_hex` (encoding axis, Q3) and the `corpus` projection (feature axis Q2 +
per-cell **role × feature** coverage). The north-star — subsuming `tests/corpus/COVERAGE.md` — is **DONE**
(two independent cold reviews: "clear win").

> **North star.** Do the hard per-construct coverage work **once** and *project* it into the **many** docs
> that need it, instead of hand-maintaining (and re-sweating) each. Corpus was the hard flagship;
> `docs/docs/current_capacities.mdx` is next; more follow. The matrix is "good enough" when regenerating a
> real hand doc from it is a **clear win**.

> **Findings ledger (F#)** from the cold critique (full write-up in git history): F1, F2, F4–F7 **done**;
> **F3 partial** (support is compile-gated; the 5-way directional split is deferred — §3); **F8–F11 out of
> scope** (bottom). Only still-open findings are sections below.

## 1. Remaining work — projections & queries

The matrix exists to feed **many** consumers; corpus was just the hard flagship. What's left:

- **`docs/docs/current_capacities.mdx` (Q1)** — cddl-codegen's hand-maintained "what we support" list. Its
  natural query is Q1 ("constructs unsupported but in target profile" → the inverse is the *supported*
  list). The support + profile data already exist in `matrix.json`; this is the right home for the
  profile-filtered support/gap logic (a `query_q1_gaps.ts` prototype was built and removed as premature —
  rebuild it here, against the real doc). Q1 stays a `QUERIES.md` definition-of-done query meanwhile — the
  capability is proven, just not emitted as a standing artifact.
- **Secondary query scripts (Q4/Q5/Q6)** from `QUERIES.md`. Q5 (matrix self-completeness) and Q6
  (profile/version diff) are largely already satisfied by `verify.ts`'s reconciliation + the F6 snapshot —
  they need only thin query scripts. **Q4** (directional support) is blocked on F3 execution (§3).
- **Full role × feature coverage grid.** The corpus projection keys coverage on `(role × feature)` only for
  the cells where support *differs by role* (`prelude.null`, the literal values). A full grid for *every*
  construct is unbuilt — the floor data (`corpus_detect.ts` `rolesIn`, via `examples/ast_roles.rs`) already
  supports it; wire it into `project_corpus.ts` if a consumer wants the complete matrix view.

## 2. Wire the gate into CI

All tooling is **Bun/TypeScript, run manually** from `cddl-matrix/`; `lib.ts` holds the shared loaders +
the stable-JSON serializer. Oracle paths are env-overridable (`RUST_CDDL`, `RUBY_CDDL`; `CODEGEN_DIR`
derives from the repo root).

**Full verification suite (run all to confirm consistency — none wired into CI yet):**
- `bun run build_matrix.ts --check` — snapshot/drift gate: `matrix.json` matches the authored overlay.
- `bun run verify.ts` — reconcile (bidirectional grammar/prelude/vendor lints) + probe **per-feature,
  per-cell, AND per-control-op** support, **compile-gated** (generate + `cargo check`); rewrites
  `annotations/cddl_codegen.toml`. Needs the three oracles (ruby `cddl`, rust `cddl` CLI, `cddl-codegen`) —
  installable: `gem install --user-install cddl`, `cargo install cddl` (set `RUST_CDDL`), cddl-codegen
  builds from this repo. Slow (probes ~156 examples × generate+compile via `cargo`).
- `bun run project_corpus.ts` — **generates `tests/corpus/COVERAGE.md`** + the overlay validator gate
  (canonical-fixture drift + note↔support agreement + `code_anchor` exists in `src/` + floor completeness +
  per-cell role coverage drift + cell-support check H). Builds/runs `examples/ast_roles.rs` (the role
  floor), so it needs the cargo toolchain like `verify.ts`.
- `bun run project_golden_hex.ts` — golden_hex (encoding-axis) projection + drift-check.
- `bun run project_robustness.ts` — projects the support verdict into the robustness-catalog fixtures
  (`tests/matrix_{supported,panic}/*.cddl`); `--check` is the drift gate. Pure
  `matrix.json` read (no cargo/oracles), so it's a fast CI gate.
- `bun run corpus_detect.ts` — runs the `featuresIn` + role-aware (`rolesIn`) self-checks and prints the
  text-scan + role-aware floor diagnostics. The role floor builds/runs `examples/ast_roles.rs` (needs cargo).

Remaining:
- **Wire into CI** so the drift-check, snapshot guard, and reconciliation run automatically. `verify.ts`
  needs the three oracle tools present; have the CI lane provide them or skip that probe gracefully when
  absent.
- **Typecheck enforcement.** The scripts are strict-typed but nothing runs `tsc`. Add a `tsconfig.json` +
  `@types/bun` (dev-only) and a `tsc --noEmit` step beside the gate (needs an ambient `declare module
  "*.toml"` for the `project_golden_hex.ts` import). The one place a (dev) dependency is worth it; the
  runtime stays dependency-free.

## 3. F3 — directional / enforcement support (partial; full split deferred)

Support is no longer one bit: the probe is **compile-gated** — generate the crate AND `cargo check` it, so
"supported" means *accept + compiles* (this caught `x = any`). `QUERIES.md` Q4 still wants the full 5-way
split **{accept, encode, decode, round-trip, enforce-constraint}**, which needs per-feature *execution* of
the compiled code (round-trip + constraint checks), not just compilation — machinery still deferred; Q4 is
blocked until it lands.

**Not a corpus blocker.** The corpus projection consumes the directional ⚠️ distinction
(parsed-but-not-honored: cuts, sockets, float-under-`preserve`) via **hand-asserted overlay notes**, not
execution. F3 is the *upgrade path*: it would make those ⚠️ verdicts execution-grounded and unblock Q4.

## 4. F4 / F5 follow-ons (only when their consumer exists)

- **F5 (encoding precision):** the encoding axis is already major-type-dependent (no impossible cells). The
  golden_hex projection lists uncovered legal cells *globally*; the remaining work is per-*construct*
  legal-cell enumeration so Q3 can say "for construct C, these legal encodings are untested" — link
  `features[].encodings` to the leaf cells each construct can emit and intersect with golden coverage.
- **F4 (tag registry):** deliberately not pinned/enumerated (cddl-codegen is tag-parametric). Revisit only
  if a *tag-semantic* consumer of the master appears.

## 5. Expansion (when relevant)

- **Profiles/versions:** v1 targets the RFC 8610/9682 grammar + the IANA control-op registry (spans RFC
  8610/9090/9165/9741). Add other CDDL profiles (e.g. the modules drafts) if needed, and bump cddl-codegen's
  declared target profile if it updates its `cddl` dependency.
- **More tools:** the master is implementation-agnostic; add `annotations/<other-tool>.toml` if another
  consumer adopts it.

## Findings & gotchas (durable — read before touching the support seam or probe examples)

The hard-won lessons. The recurring rule: **a panic/compile-failure on minimal *valid* CDDL is a finding to
surface, not something to engineer away by making the probe green — and the inverse, don't *invent* a gap
from a degenerate example.**

- **Support seam — TWO root causes, not one.** The drift-check found 7 constructs marked `unsupported` yet
  touched by a working fixture (the probe runs the minimal `example`; cddl-codegen only emits for *named
  composite types*). A first pass wrongly "fixed" all 7 by editing examples — which **hid real gaps**. The
  honest split:
  - **3 were degenerate-example artifacts** — the construct works; the example hit an *orthogonal* bug.
    Fixed with minimal *feature-isolating* examples: `memberkey.bareword`→`m = [name: tstr]`,
    `occur.optional`→`g = [? name: tstr]` (single-field **arrays** dodge the single-field-**map** bug),
    `type2.map`→`foo = { * tstr => int }` (table). Now correctly `supported`.
  - **4 were genuine gaps** — top-level fixed-value / null **types** (`answer = 42`, `version = 5`,
    `marker = "v1"`, `x = null`) panic (`should not expose Fixed type in member`): cddl-codegen uses `Fixed`
    only for serialization as a struct *member*, never as an exposed standalone type. Left `unsupported`,
    **reported as real gaps** (➖ notes in the corpus overlay), not relabelled. The corpus touches them only
    as members — the **contextual** reality (the role × feature axis), held structurally by per-cell support.
- **Anonymous-group limitation (pervasive contextual fact, surfaced by per-cell support).** An INLINE
  anonymous map/array/group nested in a choice / array-element / cbor-payload / generic-arg / map-value /
  occurrence position panics (`parsing.rs` "Anonymous groups not allowed") — it must be **named** (a rule or
  `@name`). The one exception: **tag-content** accepts an inline composite. So `type2.map` is supported as
  tag-content, unsupported inline elsewhere, and works everywhere via a named reference — the per-(feature,
  role) verdict genuinely differs, which is the whole point.
- **Containment cell-example hygiene.** Enabling per-cell support exposed single-field-map cell examples that
  confounded the panic reason. The `type2.map`-in-a-role cells (`array-element` / `cbor-payload` /
  `choice-member` / `generic-arg` / `occurrence-target`) are 2-field, so they panic for the real
  **anonymous-group** reason, not the single-field-map bug. No single-field struct-map cell example remains.
- **Compile-gate (F3 partial) — the support probe requires the generated crate to COMPILE, not just
  cddl-codegen exit 0.** This caught a false positive the exit-code probe had laundered: `x = any` exits 0
  but emits `pub type X = Any;` (undefined type → won't compile), so `prelude.any` is correctly ➖ (root
  cause: `any` absent from `is_identifier_reserved`). 4 user-code features stay `supported` via a documented
  `COMPILE_GATE_EXEMPT` allowlist (they reference user-supplied code, so can't compile standalone —
  `ext.extern`, `ext.raw_bytes`, `dsl.custom_serialize`, `dsl.custom_deserialize`; integration-tested).
- **Role floor (role × feature coverage) — NOT a serde JSON-AST dump.** The `cddl` AST's `Serialize` is gated on
  `target_arch = "wasm32"`, so there's no free serde dump on a native build; `examples/ast_roles.rs`
  hand-walks via the crate's `Visitor` trait instead. Role-detection only needs the crate to PARSE (soft
  caveat) — which it does for the whole corpus by construction. `project_corpus.ts` verifies a role-keyed
  `[[cover]]` against the AST role floor AND the per-cell support verdict (check H), so it can't claim ✅ on
  an unsupported cell.

**Bugs / gaps surfaced as findings (candidate cddl-codegen fixes — the matrix's actual payoff):**
- Top-level fixed-value / null **types** panic (`should not expose Fixed type in member`), though fixed
  values serialize fine as struct members. A singleton-value type is a reasonable feature.
- Single-field **struct** maps panic: `{ a: uint }` → `unsupported table map key`. Mixed struct+table maps
  (`{ a: uint, * k => v }`) unsupported. Inline anonymous nested composites need a name.
- `bool` in a type choice → `E0282` (`bool / tstr`, `uint / bool` fail; `int / tstr` compiles).
- A single-letter rule named `r` collides with the deserializer's reader generic `R` → `E0574`.
- Bare `x = int` / an `int` `.cbor` payload emit an undefined `Int` wrapper (`cannot find type Int`); `int`
  works as a member / array element.
- `float16` / float-choice aliases unsupported (no native Rust f16) while `float32/64` work; floats fail
  under `--preserve-encodings`; generics on plain groups rejected.
- **Exclusive range upper bound is mis-computed** — `a...b` excludes `b` (max valid = b-1), but cddl-codegen
  emits `max = b+1`: `[v: 0...10]` generates `max: Some(11)`, accepting 10 and 11. One-char fix:
  `parsing.rs` `range_end + 1` → `range_end - 1`. (Surfaced by the corpus gap-fill; `exclusive_range.cddl`
  snapshot pins it; `rangeop.exclusive` is ⚠️.)
- **Occurrence-count constraints aren't enforced** on homogeneous arrays — `[+ uint]` (≥1) and `[2*5 uint]`
  (2..5) both emit a bare `Vec<u64>` with no length check (bare `*` is faithfully a `Vec`, so only `+`/`n*m`
  drop a constraint). Surfaced by `occurrence.cddl`.
- **Inline-group splice drops members** — `[(uint, tstr)]` generates a 1-field `InlineGroup { index_0: u64 }`
  (`read_elems(1)`), silently losing `tstr` (inline-group entries aren't flattened into the record). Surfaced
  by `inline_group.cddl`; `grpent.inline_group` is ⚠️.
- **✅ FIXED — One-sided `nint` bound was inverted in the *constructor*** (the deserializer was correct, so
  the two disagreed). For `nint .ge -5`, deserialize checks the raw signed value (`if x < -5`) — right — but
  `new()` checked the stored u64 magnitude after `nint_bounds_to_u64` mapped `(min,max) → (|min+1|,|max+1|)`
  and fed it into the *same* `e < min || e > max` template (`bounds_check_if_block`). The magnitude
  representation `m = -v-1` reverses ordering, so a lower bound on the value must become an *upper* bound on
  the magnitude — the transform kept it as a lower bound. Empirically confirmed: `Bounds::new` **rejected
  in-spec** value `-1` (magnitude 0) and **accepted out-of-spec** value `-6` (magnitude 5), exactly backwards.
  **Fixed** by swapping the endpoints in `nint_bounds_to_u64` (`(min,max) → (|max+1|,|min+1|)`); regression is
  a construct-reject in `tests/core/tests.rs::bounds()` (executes in `core_no_wasm`). Originally surfaced by
  the **c6 `--emit-tests`** work, which skipped `nint` reject targets for this reason.
- **✅ FIXED — Standalone bounded-`nint` newtype did not compile** (sibling of the above, a *different* code
  path). `a = nint .ge -5 ; @newtype` generated `A(u64)` whose `new()`/deserialize both emitted `if inner < -5`
  with `inner: u64` → `error[E0600]: cannot apply unary operator - to type u64`; the crate didn't compile. The
  Wrapper (`@newtype`) bounds path is hand-rolled (not `bounds_check_if_block`) and applied the raw signed
  bound to the u64 magnitude without the nint transform. **Fixed** by applying `nint_bounds_to_u64` to the
  wrapper's `min_max` when the wrapped type is `N64` (`generate_wrapper_struct`); `new()` and deserialize share
  one check block so they stay in agreement. Regression: bounded-nint newtypes in `tests/core` with
  construct-reject + round-trip assertions (`bounds()`, runs in `core_no_wasm`).
- **Two-sided negative range as a record field panics the generator.** `rec2 = [q: -10..-3]` → `internal
  error: entered unreachable code` at `bounds_check_if_block`'s `(None,None)` arm — the negative range's
  bounds don't reach the field bounds check as `(Some,Some)`.
- **Top-level two-sided negative range silently drops its bounds.** `c = -10..-3` emits `pub type C = i64;`
  with no range check at all (bounds lost).
- **✅ FIXED (array position) — wasm boundary dropped the wrapper conversion for a named-alias collection.**
  A named exposable collection alias used as an array element — `nested = [* nums]` where `nums = [* uint]` —
  generated `NestedNum::get(&self) -> Nums { self.0[index].clone() }`: the return type is the wasm wrapper
  `Nums` (naming keeps `AliasIdent::Rust`) but the body yielded the inlined `Vec<u64>` (the boundary fns
  transparently *unwrapped* the alias) → `error[E0308]`; the wasm crate didn't compile (rust did — nothing
  systematically compiled wasm, now fixed by the corpus wasm gate). Fixed by making `to_wasm_boundary` /
  `from_wasm_boundary_clone` respect `AliasIdent::Rust` (convert into the wrapper) while still unwrapping
  `AliasIdent::Reserved`; used `is_copy` (not `is_enum`, which panics on pure type-aliases). Gated by
  `tests/corpus/wasm_nested_alias.cddl` + the new `--wasm=true` `cargo check` in `feature_corpus_compiles`.
- **✅ FIXED (map-value + optional-field positions) — the deeper half of the same class.** The map-wrapper
  *value* position (`m = { * uint => nums }`) and an optional named-alias-collection field (`? f: nums`) also
  failed: `get`/`insert` returned `Option<Nums>` from an `Option<Vec<u64>>` body (E0308), and the optional
  getter hit `E0277`. Root cause was one level deeper than the array case: the map/optional emission decides
  its `.into()` from `directly_wasm_exposable`, whose `Alias` arm *unwrapped* `AliasIdent::Rust` — asking "is
  the inlined inner exposable" (a bare `Vec<u64>` is) instead of "is the *named* alias a wrapper" (it is).
  **Fixed at the predicate** (single source of truth): the wrapper-vs-transparent fact is NOT derivable from
  the conceptual-type shape — a direct named collection (`nums = [* uint]`, a wasm wrapper struct) and a
  *passthrough* alias (`arr2 = arr`, a transparent `pub type Arr2 = Vec<u64>`) both have inner `Array(..)`.
  The source of truth is the generated struct table: `directly_wasm_exposable`'s `Alias` arm now returns
  `false` when `ident` has a `RustStruct` wrapper variant (Array/Table/Record/Wrapper), and otherwise (a
  transparent `pub type`, or a re-exported c-style enum) *recurses into what it aliases* — so `arr2 = arr`
  (→ exposable `Vec<u64>`) is transparent, while `foo_bytes = bytes .cbor foo` (→ the wrapper `Foo`) stays a
  wrapper, unchanged. One edit fixes map get/insert/keys, the optional getter, and flips named-array-alias
  params to by-ref (an ABI-consistency win). An earlier shape-match version (`Primitive|enum → true, _ →
  false`) was **caught in audit** to regress the passthrough case (`arr2 = arr` at a map value → by-ref
  `&Vec<u64>`, which wasm-bindgen rejects — `RefFromWasmAbi`); the recurse-on-no-wrapper form fixes that with
  zero collateral (only `wasm_nested_alias` snapshots move). Chosen over `is_copy` (misclassifies `text`/`bytes`
  aliases). Gated by `tests/corpus/wasm_nested_alias.cddl` (array-element, map-value, optional-field, AND the
  passthrough alias) under the corpus `--wasm=true` `cargo check`.
  - *Related known bug (unrelated to this fix):* `{ ? f: nums }` (a map-rep struct with a bareword member key)
    panics the generator at `parsing.rs:960` — pre-existing (see `draft/cddl-bareword-member-key-bug.md`);
    the fixture uses the array-rep `[ x: uint, ? f: nums ]` to sidestep it.
- **STILL OPEN (wasm, pre-existing — surfaced by the edge-case hunt, not regressions):** two more
  named-alias-at-wasm-boundary gaps that fail identically at HEAD (byte-identical generated wasm before/after
  the map-value fix): (a) a passthrough alias to a **map** typedef used as a wasm map value/element —
  `m2 = amap; amap = { * uint => text }` — emits a dangling `MapU64To…` type reference (`E0425`); (b) a
  named collection **wrapper** used as a map **KEY** — `{ nums => text }` — yields a `Borrow` mismatch
  (`Vec<u64>: Borrow<&Vec<u64>>`, `E0277`). Both need the wasm map key/value typedef-resolution path, distinct
  from the `directly_wasm_exposable` predicate. Good future clear-win candidates now that the wasm gate exists.

## wasm-ABI matrix (systematic wasm-boundary coverage — `project_wasm_matrix.ts`)

The systematic answer to "we find wasm bugs by luck": `cddl-matrix/project_wasm_matrix.ts` enumerates
{wasm-ABI type-shape} × {boundary role} into `tests/matrix_wasm/*.cddl` (one minimal fixture per cell),
and `integration_tests::wasm_matrix_compiles` generates each `--wasm=true` + `cargo check`s the wasm crate.
Coverage is now by-construction: a new wasm-ABI boundary bug surfaces as a specific red cell, not a
production surprise. Standing up the enumeration (69 cells at first cut, 61 green after fixing known-red
#2) reconfirmed the two open reds above **and found two new ones** the ad-hoc hunt had missed. Current
skip-list: 8 red cells (known-red #1 ×5 roles, the two new cenum reds, extern):

- **Red cells (compile-fail), skip-listed in the gate — TDD targets, take off `SKIP` as each is fixed:**
  - **known-red #1 — passthrough alias → map typedef (`E0425`). Still open (deferred, not shallowly
    fixable).** `passthrumap` (`mp = { * uint => text }; ptm = mp`) fails as `array-element`, `map-value`,
    `map-key`, `struct-field`, AND `struct-field-opt` (only `newtype-inner` compiles). Root cause: on the
    wasm side the alias emits `pub type Ptm = MapU64ToText`, but no such wrapper struct exists — the named
    map's wasm wrapper is `Mp` (from the Table rule ident), while `MapU64ToText` (`name_for_wasm_map`) is
    only generated for *inline* anonymous maps, which the alias path never triggers.
    - **Attempted + reverted (dead-end to not repeat):** making the alias base_type keep a reference to
      the named map (`parsing.rs` `None` branch: `Alias(Rust(Mp), Map)` → `Rust(Mp)`) DOES green
      `array-element` + `map-value`, but a named map has a **dual representation** — a transparent
      `pub type Mp = BTreeMap` typedef AND a wasm wrapper struct — and `Rust(Mp)` is not interchangeable
      with an inlined `Map` in the other emitters: it regressed `collmap__newtype-inner` (newtype wraps
      `Mp` → `BTreeMap::serialize` doesn't exist, `E0599`), broke `struct-field`/`map-key` (record/key
      serialization expects the inline `Map`), and turned two red cells into generation panics. Inlining
      the map is load-bearing for rust/newtype/struct serialization; the wasm side is the only place that
      needs the wrapper name. A single shared base_type can't serve both (arrays work only because they're
      transparent on *both* sides).
    - **The real fix** is the wrapper-logic unification the handoff flags as out-of-scope (one
      `has_wasm_wrapper(ident)` predicate that naming + boundary + exposability all derive from), or a
      wasm-only alias-emission path that resolves a `Map` base_type to its named Table wrapper. Left
      skip-listed as the next TDD target.
  - **known-red #2 — collection wrapper as a map KEY (`E0277`). ✅ FIXED.** `coll__map-key`
    (`nums = [* uint]` at `{ * nums => uint }`): `Vec<u64>: Borrow<&Vec<u64>>`. `from_wasm_boundary_ref`
    unwrapped the wrapper alias into its inline array and hit the `&{expr}` fallthrough (`&key`), then the
    map-key `get` appended `.as_ref()` → `&key.as_ref()`. Fixed by treating a non-`directly_wasm_exposable`
    named alias like `Rust(ident)` (return `expr`), so the emitter yields `key.as_ref()`. Off the SKIP list.
  - **NEW red — c-style enum as a map KEY (`E0119`).** `cenum__map-key` (`fe = 0/1/2` at `{ * fe => uint }`)
    emits conflicting `Ord`/`Eq`/`PartialEq`/`PartialOrd` impls on the RUST crate: the enum is already
    `#[derive(..Eq, Ord..)]` and the map-key path derives them again. Compiles fine as array-element /
    map-value — the cell individuates the map-key trigger exactly.
  - **NEW red — `@newtype` over a c-style enum (`E0308`).** `cenum__newtype-inner` (`holder = fe ; @newtype`):
    mismatched types in the generated wrapper body. Newtype over primitive / collection / struct / cbor /
    generic all compile — the enum inner is the trigger.
- **The wrapper-vs-transparent distinction the serialization matrix collapses is a first-class axis here:**
  `coll` (Array wrapper) vs `collmap` (Table wrapper) vs `passthru`/`passthrumap` (transparent `pub type`)
  vs `struct` (Record) vs `cborwrap` (transparent-to-wrapper) vs `cenum` (Copy re-export) vs `generic`
  (monomorphized wrapper) — each a distinct `is_copy × directly_wasm_exposable × has-a-RustStruct` cell.
  Depth/redundant shapes (`chain` 2-hop passthru, `cborwrap2` chained, `extern`) are kept as one
  representative role each (their accessors differ from the 1-hop shape only by type name).

**Oracles (so `verify.ts` runs outside CI):** ruby `cddl` via `gem install --user-install cddl` (verify.ts
auto-resolves it at `Gem.user_dir/bin/cddl`), rust `cddl` via `cargo install cddl` (point `RUST_CDDL` at
`~/.cargo/bin/cddl`), and cddl-codegen builds from this repo. The compile-gate reuses
`integration_tests::feature_corpus_compiles`' pattern (shared `CARGO_TARGET_DIR`, one-time dep warm-up).

## Explicitly out of scope (decided, not overlooked)

Per the `QUERIES.md` query-map, no consumer query needs these (revisit only if a concrete need surfaces):
- **F8 — matching semantics** (prioritized-choice ordering, greedy occurrence, implicit `:` cut): a
  validation concern, not a cddl-codegen serialization concern.
- **F9 — interaction tuples** (A-in-B-in-role-C-with-operator-D): richer-than-binary containment; stretch
  query (Q7) only.
- **F10 / F11** — note-only (over-acceptance denominator; AST cross-check is weak corroboration).

## Pending decision (needs a human call)

- **Over-acceptance denominator:** should coverage be measured against the grammar's full superset, or only
  realistically-implementable constructs?

(Resolved decisions — D3 corpus numerator; cddl-codegen comment-DSL as the `CDDL_CODEGEN` profile; doc
consolidation — are recorded in the code + git history; not re-litigated here.)

## Maintenance

Upstream specs churn (IANA registries, the grammar). Refresh with `sources/fetch.sh` (re-fetches + verifies
against `SHA256SUMS`); a checksum mismatch flags upstream drift to review before re-pinning and regenerating.

## Related

Broader testing work (clear-wins, roadmap items, the trailing-bytes runtime change) lives in
`tests/TESTING_ROADMAP.md` — the matrix is the reference that testing effort *projects from*.
