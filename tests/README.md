# Tests

cddl-codegen is tested in two complementary layers. Keep them distinct — they answer different
questions.

| Layer | File | Question it answers | Speed |
|-------|------|---------------------|-------|
| **Golden snapshots** | `src/tests/snapshot_tests.rs` | "Did the *generated source* change?" | fast (~5s, in-process) |
| **Integration** | `src/tests/integration_tests.rs` | "Does the generated code *compile and round-trip*?" | slow (compiles generated crates) |

Snapshots are the fast inner loop and the primary safety net for refactors; integration tests are
the correctness gate. A refactor that doesn't intend to change output should leave every snapshot
untouched — if one moves, you see exactly what changed.

**CI policy — feature-frozen.** The CI workflow (`.github/workflows/build.yml`) is feature-frozen:
it accepts no new jobs, steps, gates, or expansions of existing runs (CI minutes cost real money —
see `AGENTS.md`). The only changes accepted are fixes to things that break due to refactoring.
Verification systems not already wired into CI run manually/locally, and are documented as manual
runs.

## Golden snapshots (`snapshot_tests.rs`)

Drives the generator as a library (`crate::api`) and snapshots the post-rustfmt generated source
with [`insta`]. No subprocess, no compilation, no `target/` bloat. Three sub-suites:

- **`feature_corpus`** — one tiny CDDL file per language construct in [`tests/corpus/`](corpus),
  generated under every flag profile in `ALL_PROFILES` (`default`, `preserve`, `json`), plus an IR
  dump. A one-feature regression yields a one-file diff. Snapshots are grouped per feature in
  `tests/corpus/snapshots/<feature>/`. The generated `Cargo.toml` and json-gen `main.rs` are
  *skipped* here — they barely vary by construct, so they'd be repeated noise; they're covered by
  `whole_program` and `serialization_prelude` instead.
- **`whole_program`** — the larger integration inputs (`core`, `preserve-encodings`, `canonical`,
  `json`, and the `multifile` directory) each under one known-safe profile, capturing the *full*
  output incl. `Cargo.toml`s. Covers cross-feature interactions, the scope/module path, and the
  edition/deps logic.
- **`cargo_toml_matrix`** — a small curated `input × profile` matrix that snapshots every distinct
  generated `Cargo.toml` dependency combination (the type-conditional `hex`/`wasm-bindgen` deps
  toggled independently). The per-feature corpus skips `Cargo.toml` as near-constant noise, and
  `whole_program` doesn't produce every combination, so this is where they're all pinned.
- **`serialization_prelude`** — the static serialization runtime, snapshotted once per flag
  combination (it ships verbatim into every crate but is assembled differently per flag).

`canonical` is a serialization sub-mode of `preserve` (differs only where maps/sets exist), so it's
covered at whole-program scale rather than duplicated per feature.

### Adding a feature

1. Drop a tiny `tests/corpus/<feature>.cddl` exercising exactly one construct (see existing files).
   The stem must not collide with a `whole_program` label (asserted by the test).
2. `INSTA_UPDATE=always cargo test snapshot_tests` to generate its snapshots.
3. Eyeball the new files under `tests/corpus/snapshots/<feature>/`, then commit them.

### Blessing changes

After an intentional generation change:

```sh
INSTA_UPDATE=always cargo test        # accept all, then review the git diff
# or, with cargo-insta installed:
cargo insta review                    # interactive per-snapshot accept/reject
```

`*.snap` files are committed (they're the golden reference); `*.snap.new` / `*.pending-snap` are
gitignored.

CI also runs `cargo insta test --unreferenced=reject` so a snapshot orphaned by a refactor (one
that stops generating a file) fails the build instead of lingering unnoticed.

## Integration tests (`integration_tests.rs`)

Each test generates a crate via the CLI (`cargo run`), appends hand-written round-trip tests
(`tests/deser_test` + each dir's `tests.rs`), then compiles and runs it — plus a wasm build and a
json-schema build where applicable. Each config (`preserve`, `canonical`, `json`, multifile,
raw-bytes, extern-deps, …) exercises a distinct compile path, so they aren't redundant.
A fixture dir may also ship a `tests_wasm.rs`: its contents are appended into the generated
*wasm* crate and `cargo test`ed there (host target — the wasm-bindgen wrapper types are plain Rust,
so no node/wasm-pack is needed). `tests/core/tests_wasm.rs` (default profile) and
`tests/canonical/tests_wasm.rs` (preserve-encodings/canonical, whose map wrappers wrap
`OrderedHashMap`) execute a representative sample of the wasm-ABI shape axis (the
`project_wasm_matrix.ts` `SHAPES` list): construct through the wasm wrapper API, round-trip
`to_cbor_bytes`/`from_cbor_bytes`, read every accessor back. That's the *behavioral* half the
`wasm_matrix_compiles` gate below can't see — a semantically wrong accessor or boundary conversion
compiles green. The rust-side value round-trips are `--emit-tests`' job; these files own the
boundary.

The three external-macro flags (`--wasm-list-macro`/`--wasm-conversions-macro` and
`--wasm-cbor-json-api-macro`) emit invocations of a *user-supplied* macro, so the output can't
compile standalone and a source snapshot can't judge invocation semantics; `wasm_list_macro_compiles`
and `wasm_cbor_json_api_macro_compiles` compile-gate them against the real macro definitions in
[`tests/wasm-macro-crate`](wasm-macro-crate) (wired in as a path dependency, the same way
extern-deps wires `tests/extern-dep-crate`). Those macros' arms mirror the inline emission, so the
wrong-emission classes a snapshot would bless — swapped args, wrong `needs_into`/`is_copy`, an
unreachable combination, a wrong arity — fail to compile (see the crate's README).

`flag_value_smoke` generate + `cargo check`s a rich extern-free input (`tests/canonical`) under each
documented flag *value* that no named profile exercises (`--annotate-fields=false`,
`--to-from-bytes-methods=false`, `--binary-wrappers=true`) — each selects a whole alternative emit
path. `--canonical-form=true` requires `--preserve-encodings` (on its own it emits a non-compiling
crate); that combination is rejected in `api::with_types` and pinned by
`flag_value_rejects_canonical_without_preserve`.

### Independent conformance oracle (`tests/deser_test_conformance.rs`)

A round-trip only proves our encoder and decoder agree with *each other* — a symmetric bug passes.
For a second oracle whose **decode + constraint-evaluation** path is independent of ours,
`deser_test_conformance.rs` validates our serialized bytes against the source `.cddl` using the `cddl`
crate's validator (`validate_cbor_from_slice`, which decodes with ciborium and evaluates constraints
itself). A **failure is a strong signal** (our bytes don't match the spec the generator was built
from); a **pass is weak** — the validator has known gaps (it does not enforce `uint .size`, and
mishandles `.size`-aliased element types inside arrays) AND it is *not fully decorrelated*: it parses
the `.cddl` with the same dcSpark `cddl` fork at the same pinned rev as the generator's own front end
(`CDDL_ORACLE_DEP`), so a fork-level misparse escapes it. It is therefore a *second* oracle, never the
sole one; decorrelating the parser (anweiss rev / the ruby `cddl` gem in verify.ts / a ciborium
structural differential) is a TESTING_ROADMAP item. Because the validator validates against a spec's first type rule only, the helper
prepends a synthetic root aliasing the rule under test.

It's wired into the `preserve-encodings` fixture (the richest hand-written round-trip surface, and the
one whose whole point — irregular definite/indefinite encodings — most needs an independent structural
check): `run_test` appends the helper and adds the `cddl` git dep to that generated crate. Broadening
to more fixtures is a compile-cost trade-off (the `cddl` dep is heavy), not a limitation of the helper.
See `tests::cddl_crate_conformance` in `tests/preserve-encodings/tests.rs`.

### Spec-anchored golden vectors (`tests/golden_hex*`)

Three fixtures assert exact CBOR bytes hand-derived from RFC 8949 rather than built with any
encoding helper — the only oracle class that catches a *symmetric* encode+decode bug (both sides
wrong in compensating ways round-trips green everywhere else):

- **`tests/golden_hex`** — default flags; RFC 8949 Appendix A known-answer vectors, both
  directions. Coverage map: [`tests/golden_hex/COVERAGE.md`](golden_hex/COVERAGE.md), projected and
  CI-drift-gated by `cddl-matrix/project_golden_hex.ts`.
- **`tests/golden_hex_preserve`** — `--preserve-encodings`; irregular §3 encodings (non-minimal
  header arguments, indefinite/chunked items, map key order) must re-encode byte-identically.
- **`tests/golden_hex_canonical`** — `--canonical-form`; the same irregular inputs must re-encode
  to hand-derived §4.2 minimal bytes (and those bytes must be a canonical fixed point).

The preserve/canonical suites' *other* byte assertions are built with `tests/deser_test`'s
cbor_event helpers — the same `write_*_sz` primitives the generated code encodes with — so these
raw-hex sets are the independent spec anchor for those modes. The projection validates every
golden byte array in all three dirs (two-digit `0x??` literals, exactly one well-formed CBOR item)
and hard-fails otherwise; regenerate + commit `COVERAGE.md` after editing any of them.

## Generated-test harness (`--emit-tests`, `src/emit_tests.rs`)

The generator can emit a `#[cfg(test)] mod cddl_generated_tests` into the generated rust crate:
per-type **round-trip** tests (IR-derived cases — baseline, bound boundaries, one per choice
variant, each optional field present — asserted byte-identical through the full wire cycle, and
— outside preserve-encodings, where wire-populated encoding fields legitimately differ — the
deserialized value asserted `Debug`-equal to the minted original: byte-identity alone is a fixed
point for an information-losing projection serializer, so it can't see that miscompile class) and
**bounded-reject** tests. Values are minted deterministically from each type's IR (no
proptest/`Arbitrary` deps in generated crates); unmintable shapes are skipped with a logged
notice. Two consumers run it in CI: `integration_tests::emit_tests_execute` (the rich
preserve-encodings fixture, with emitted-test count floors) and `feature_corpus_compiles`'
default profile (below). This is the "output is right, not just unchanged" oracle — it caught two
snapshot-blessed miscompiles (`.ne` bounds, preserve-encodings default-field serialization) on
its first corpus sweep. It shares the generator's IR, so IR-level bugs (wrong bounds computed at
parse time) are the spec-anchored oracles' job (`tests/golden_hex/`).

The corpus gate `feature_corpus_compiles` `cargo check`s every `tests/corpus/*.cddl` crate (rust +
wasm + json-gen) under all three profiles, and under the **default profile** additionally
generates with `--emit-tests` and `cargo test`s **both** the rust and the wasm crate — so a corpus
construct must round-trip, not just compile, on both the rust and the wasm side. (`cargo check`
never compiles `#[cfg(test)]` code, so nothing but `cargo test` type-checks or runs the emitted
`cddl_generated_wasm_tests` module below; the preserve/json profiles and json-gen stay check-only.)

Generated output lands in `tests/<dir>/export*/` — disposable, gitignored, and safe to
`git clean -fdx tests` if the ~GBs of build artifacts pile up locally. CI starts clean each run.

### wasm-crate test module (`--emit-tests` + `--wasm=true`, `src/emit_tests_wasm.rs`)

With `--wasm=true`, `--emit-tests` also emits a `#[cfg(test)] mod cddl_generated_wasm_tests` into the
generated **wasm** crate. It's a *second renderer* over the same `emit_tests::MintValue` derivation
surface the rust harness uses (the derivation is the single maintained thing; the two renderers —
rust-API strings vs wasm-wrapper-API strings — read from it). The teeth, per mintable type:

1. **Cross-crate byte differential** — build the value through the wasm wrapper ctor/`new_*` AND,
   independently, through the `cddl_lib::` rust ctor (the wasm crate path-depends on it), then assert
   `to_cbor_bytes()` is byte-equal. A wrong conversion in a wasm `new`/`new_<variant>`/`set_*` can't
   cancel here (the rust build is independent), so this catches the identity-`.into()`-where-a-transform-
   was-needed class — the exact wasm-boundary bug the compile gate can't see.
2. **Wire round-trip** — `from_cbor_bytes(bytes)` then `to_cbor_bytes()` byte-identical.
3. **Accessor read-back against emit-time literals** — primitive getters compared to the exact minted
   literal (not original-vs-back, which lets a wrong getter conversion cancel); enum `kind()`/`as_<var>()`
   pinned to the minted variant. Read on the freshly-*built* wasm value, not the post-wire one, so a
   wire-ambiguous choice (core's uint-`0` vs a fixed `i0` variant) can't false-fail.
4. **Boundary acceptance only** (`wasm_bounds_<type>`) — the accepted boundary value constructs
   (`.ok().is_some()`). The beyond-boundary REJECT direction is deliberately **not** emitted: a wasm
   ctor's error path builds a `JsError` through a wasm-bindgen import that panics under host `cargo
   test`; rejection is already pinned as `RangeCheck` on the wire by the rust `--emit-tests` module, so
   this half only confirms the acceptance plumbing.

wasm-API idioms baked in: `JsError: !Debug`, so a wasm `Result` is unwrapped `.ok().expect(..)`, never
`.unwrap()`; composite ctor params cross as `&Wrapper`; c-style enums cross by value; `@newtype`/tag
wrappers expose no wasm `new` (built by decoding the rust twin's bytes). **Loud skips (never silent):**
wrapper/collection ctor args (block-expr build deferred), flatten points, and the whole module under any
`--wasm-*-macro` flag (those replace the wrapper method surface) — each an `eprintln!`. Mutation-verified
red-first (three `generation.rs` wasm-boundary mutations each turned exactly the intended assertion class
red; see the `src/emit_tests_wasm.rs` header).

Two consumers run it:
- **`integration_tests::emit_wasm_tests_execute`** (default suite, ~10s) — generates the rich `core`
  fixture `--wasm=true --emit-tests=true` and `cargo test`s the **wasm** crate (alongside the
  hand-written `tests_wasm.rs` as a plausibility cross-check), with emitted-test count floors. It
  `cargo test`s only the wasm crate: `core` is not `--emit-tests`-clean on the *rust* side (two
  hand-written source-inspection tests truncate `lib.rs` at the first `#[cfg(test)]`, and its
  wire-ambiguous `TypeChoice` trips the rust value-equality oracle), but the wasm crate builds the rust
  crate as a *non-test* dependency, so none of that compiles here.
- **`integration_tests::wasm_matrix_roundtrips`** (`#[ignore]`d, manual — the round-trip upgrade of the
  wasm-ABI matrix compile gate; see that section below).

Run the manual gate with:

```sh
cargo test --bin cddl-codegen wasm_matrix_roundtrips -- --ignored   # ~1.5 min
```

### IR-bug conformance oracle at breadth (`--emit-tests-conformance` + `integration_tests::ir_conformance_corpus`)

The round-trip harness mints its values from the **same IR** as the code under test, so an IR-level
miscompile — a bound or member computed wrong at *parse* time — mints a spec-violating value and then
asserts it round-trips green (encoder and decoder agree with each other, both from the same bad IR).
Illustrative shape of the class (now fixed): `tests/corpus/exclusive_range.cddl` (`[v: 0...10]`) once
mis-computed the exclusive upper bound, so the minter minted `v = 11` (spec max valid = 9) and the
round-trip passed anyway — an IR-level bound bug invisible to the round-trip harness.

`--emit-tests-conformance` closes that residual. When on, each emitted round-trip case gets one extra
line right after its bytes are computed: `cddl_conformance::validate(&bytes, "<rule>")`, validating
the minted bytes against the type's **source `.cddl` rule** via the `cddl` crate's independent
decode+constraint path (the same validator, and the same shared helpers, as
`deser_test_conformance.rs` — the emitter reuses them, it does not duplicate the validator). The
Rust type name is mapped back to its source rule via `convert_to_snake_case`, gated on the ident
being a real top-level rule (`IntermediateTypes::is_toplevel_rule`) and on the reversal round-tripping
faithfully — a synthesized struct or a lossy name gets no call.

**What it proves / can't.** A conformance failure is a strong signal (our bytes violate the spec the
generator was built from). Same caveats as `deser_test_conformance.rs`: it shares the dcSpark fork's
*parser* with the generator, so it catches wrong **values**, not fork-level misparses; and the minted
values are shallow (None arms, empty tables, depth-capped recursion), so it's breadth across fixtures,
not exhaustive per-type depth.

**The gate** (`integration_tests::ir_conformance_corpus`, `#[ignore]`d — **manual/local only** under
the CI freeze, because it adds the heavy `cddl` dep to every corpus crate):

```sh
cargo test --bin cddl-codegen ir_conformance_corpus -- --ignored --nocapture   # ~1 min
```

For every `tests/corpus/*.cddl` it generates with `--emit-tests --emit-tests-conformance`, appends
`CDDL_ORACLE_DEP` + the shared oracle helpers, copies the fixture in as
`cddl_conformance_source.cddl`, and `cargo test`s the crate under one shared `CARGO_TARGET_DIR` (so
`cddl` compiles once). Two curated lists, each empirically justified:

- **`EXPECTED_FAIL`** — fixtures with a known IR bug whose minted value the oracle *must* reject. Their
  `cargo test` must fail **and** the output must carry the oracle's distinctive message (so it failed
  for the right reason). An expected-fail fixture that *passes* turns the gate RED ("IR bug apparently
  fixed or oracle lost teeth — investigate, then remove from `EXPECTED_FAIL`"). **Currently empty** —
  no corpus fixture mints a spec-violating value at HEAD. The machinery stays armed: the next IR-level
  bug's fixture will trip this list. Its last resident, kept here as the illustrative case, was
  `exclusive_range` (`[v: 0...10]`): the validator rejected the minted `11` as out of range
  `0 <= value < 10`, and it was removed once `parsing.rs` was corrected to emit `max = b-1` and the
  minted value became in-spec.
  `inline_group` (`[(uint, tstr)]`) and `occurrence` (`[+ uint]` / `[2*5 uint]`) are earlier
  residents' siblings that never joined the list: both are **fixed at HEAD** (inline_group emits a
  2-field struct that reads 2 elems; occurrence bounds now live on the ARRAY type — enforced as a
  length check at embed sites and covered by `occurrence_holder`'s minted round-trip + deser-reject
  cases, where they were once misread as element VALUE bounds).
- **`CONFORMANCE_SKIP`** — fixtures excluded from the sweep for a concrete *validator/minter* gap
  (never to hide a real bug): `dsl_custom` (references user-supplied code, can't compile standalone),
  and `sized_int` (validator gap — it cannot evaluate a range with a negative lower bound, `i_8:
  -128..127`, nor `.size` on a signed `int`, `i_64: int .size 8`; our minted values are in-spec).

Any fixture **not** on either list that fails conformance turns the gate RED with the minted bytes +
rule named. A vacuity floor asserts a nonzero number of fixtures actually emitted a conformance call,
so a silent no-op sweep can't pass.

## wasm-ABI matrix (`tests/matrix_wasm/` + `integration_tests::wasm_matrix_compiles`)

A **coverage-by-construction** gate for the generated wasm-bindgen bindings: it compiles the wasm crate
for every cell of a `{wasm-ABI type-shape} × {boundary role}` grid, so any cell whose bindings don't
type-check is a specific red cell. It exists because the wasm ABI — accessor return types, boundary
`.into()`/`.clone()`/by-ref conversions, map typedefs — is a concern the CBOR-serialization suites don't
compile-check by construction: the rust crate can type-check while the generated wasm crate does not, so
without an enumerated gate that class of bug is only caught by whichever fixtures happen to hit it. Gating
the whole grid makes the coverage systematic instead of incidental.

Coverage equals the **type-shape axis**, which is hand-curated: a wasm representation not in `SHAPES` is
not gated. Treat the axis as a living list — when a type reaches the wasm boundary in a representation no
existing shape captures, add a shape (see "Adding / changing cells"), and periodically ask "which
representation are we *not* enumerating?", because a missing shape is a silent hole, not a red cell.

Pipeline (projection → fixtures → gate), the same shape as the robustness projection:

```
cddl-matrix/project_wasm_matrix.ts  ─►  tests/matrix_wasm/<shape>__<role>.cddl  ─►  integration_tests::wasm_matrix_compiles
     enumerate {shape × role}            one minimal fixture per cell             generate --wasm=true, cargo check the wasm crate
```

- **The projection** (`cddl-matrix/project_wasm_matrix.ts`, `bun run`) emits one minimal `.cddl` per
  `(type-shape × boundary role)` cell. Output is deterministic — **never hand-edit `tests/matrix_wasm/`**;
  edit the projection and re-run. `--check` is the drift gate (fails on a stale/missing/orphaned fixture)
  and runs in CI's `matrix-drift` job.
- **The two axes** — the authoritative list + copy-paste CDDL live in the projection's `SHAPES`/`ROLES`:
  - **Type-shape**: how a type crosses the wasm boundary — `prim`, `palias`, `talias`, `coll`/`collmap`
    (array/map wrapper structs), `passthru`/`passthrumap` (transparent `pub type`s), `struct`,
    `cborwrap`/`cborwrap2`, `cenum` (Copy c-style enum), `denum` (data-carrying type-choice enum),
    `nullable` (`Option<T>`), `generic`, `chain`, `extern`. This is the
    `is_copy × directly_wasm_exposable × has-a-wrapper-RustStruct` axis the CBOR feature matrix
    deliberately does *not* individuate (wrapper-vs-transparent is a struct-table fact, not a shape fact
    — see the docstrings in `src/intermediate.rs`).
  - **Role**: where the type sits — `array-element`, `map-value`, `map-key`, `struct-field`,
    `struct-field-opt`, `newtype-inner`. Each drives distinct accessor emission (`get`/`add`/`insert`/
    `keys`, by-value vs by-ref). Struct roles use the **array representation** (`[field0: T]`,
    `[pre: uint, ? field0: T]`) because a map-representation struct with a bareword member key currently
    panics generation (a separate, still-open limitation); consequently map-representation structs and
    optional-fields-inside-maps are outside this grid's scope. A shape may likewise skip a role
    that would only pin a permanent red — `nullable` skips `map-key`: a nullable key is degenerate
    CDDL and its wasm bindings don't compile (`Option<u64>` fails `ErasableGeneric`), see the
    prune comment in the projection.
- **The gate** (`integration_tests::wasm_matrix_compiles`) globs the fixtures, generates each
  `--wasm=true`, and `cargo check`s the wasm crate. The wasm crate path-depends on the rust crate, so
  rust-side type errors surface here too — which means some skip-listed reds are rust-crate generation
  bugs rather than wasm-boundary ones. It follows `feature_corpus_compiles`' shared-target-dir *pattern*
  but uses its **own** scratch + `CARGO_TARGET_DIR` (`cddl_codegen_wasm_matrix`), separate so the two
  tests don't collide when `cargo test` runs them in parallel. The verdict is **compile**: a cell can
  compile green while emitting *semantically* wrong bindings (e.g. an identity `.into()` where a transform
  was needed). Catching those is the job of the **round-trip** upgrade — `integration_tests::wasm_matrix_roundtrips`
  (`#[ignore]`d, manual): same cell enumeration, but each cell is generated `--emit-tests=true` and
  `cargo test`ed so the emitted `cddl_generated_wasm_tests` module (see § "wasm-crate test module" above)
  RUNS its cross-crate byte differential + accessor read-back. It has its own scratch dir
  (`cddl_codegen_wasm_matrix_rt`) and `SKIP` list so it runs beside this always-on compile floor, which
  stays untouched. Run it with `cargo test --bin cddl-codegen wasm_matrix_roundtrips -- --ignored`; a cell
  whose shape mints no wasm surface (loud emitter skip) passes with zero emitted tests, which is a
  legitimate green (the compile gate already pins its ABI compiles).

**Fixing a red cell (the TDD loop).** A red cell is a bug the matrix *wants* fixed. Known reds sit in the
gate's `SKIP` list, each with a comment + a ledger entry in
[`cddl-matrix/ROADMAP.md`](../cddl-matrix/ROADMAP.md) (which shape/role, the exact `E####`, root cause).
To close one:

1. Remove its `<shape>__<role>` entry from `SKIP`.
2. Fix the emitter; `cargo test wasm_matrix_compiles` until green.
3. A `SKIP` cell that starts compiling *fails* the gate (the "resurfaced" guard) — so you can't forget
   step 1 and the list can't rot.

A *new* red cell (red but not in `SKIP`) also fails the gate: fix it, or skip-list it **deliberately**
with a ROADMAP entry — never silently.

**Adding / changing cells.** Edit `SHAPES`/`ROLES` in the projection, `bun run project_wasm_matrix.ts`,
review the new fixtures, run the gate. Prune cells whose emission duplicates an existing one — the
projection already restricts redundant shapes (`chain`, `cborwrap2`, `extern`) to one representative role.

> Sibling system: `tests/matrix_{supported,panic}/` (projected by `cddl-matrix/project_robustness.ts`,
> driven by `src/tests/robustness_tests.rs`) is the same projection→fixtures→gate shape on a different axis —
> "does a construct *generate*?" rather than "does its wasm *compile*?".

## Coverage

The in-process snapshot suite alone covers ~81% of the codebase (generation.rs ~86%). To measure
(requires `cargo install cargo-llvm-cov` + `rustup component add llvm-tools-preview`):

```sh
cargo llvm-cov --summary-only -- snapshot_tests
```

Note: the integration tests run the generator in a subprocess, so llvm-cov (which instruments the
test binary) does not attribute their coverage — the 81% is from snapshots + in-process unit tests
only.

## Known gap

The CDDL standard prelude (`biguint`, `tdate`, `uri`, …) is covered by `tests/corpus/prelude.cddl`.
The float-bearing prelude types (`number`, `time`) are omitted from it because floats don't support
`--preserve-encodings` (a pre-existing `unimplemented!` in `generation.rs`), which the corpus
exercises for every entry.

[`insta`]: https://insta.rs
