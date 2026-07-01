# Tests

cddl-codegen is tested in two complementary layers. Keep them distinct — they answer different
questions.

| Layer | File | Question it answers | Speed |
|-------|------|---------------------|-------|
| **Golden snapshots** | `src/snapshot_tests.rs` | "Did the *generated source* change?" | fast (~5s, in-process) |
| **Integration** | `src/integration_tests.rs` | "Does the generated code *compile and round-trip*?" | slow (compiles generated crates) |

Snapshots are the fast inner loop and the primary safety net for refactors; integration tests are
the correctness gate. A refactor that doesn't intend to change output should leave every snapshot
untouched — if one moves, you see exactly what changed.

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

Generated output lands in `tests/<dir>/export*/` — disposable, gitignored, and safe to
`git clean -fdx tests` if the ~GBs of build artifacts pile up locally. CI starts clean each run.

## wasm-ABI matrix (`tests/matrix_wasm/` + `integration_tests::wasm_matrix_compiles`)

A **coverage-by-construction** gate for the generated wasm-bindgen bindings. The suites above exercise
the wasm ABI only *incidentally* — whatever the hand-picked fixtures happen to use — so a whole class of
boundary bugs (wrong accessor return type, bad `.into()`/`.clone()`/by-ref slips, dangling map typedefs)
was invisible: the *rust* side compiled fine and nothing systematically compiled the wasm crate. This
enumerates a **grid** of `{wasm-ABI type-shape} × {boundary role}` and gates every cell of it, so a bug
in a covered cell surfaces specifically instead of by luck.

The grid is only as complete as its **type-shape axis**, which is hand-curated and *extensible*: a wasm
representation not in `SHAPES` isn't covered. When a new distinct representation appears, add a shape (see
"Adding / changing cells"). This is not hypothetical — the `nullable` shape was added after a holistic
audit generated `opt = uint / null` at a map value and found a live `E0277` (`OptionIntoWasmAbi`) red the
grid was silently missing. Treat the axis as a living list, not a finished one.

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
    `keys`, by-value vs by-ref). Structs use the **array representation** (`[field0: T]`,
    `[pre: uint, ? field0: T]`) to dodge a pre-existing map-rep bareword-member-key generation panic, so
    map-representation structs and optional-fields-inside-maps are not gated here. A shape may also skip a
    role that panics generation on it (`nullable` skips `map-key` — a null key hits a "special-typed map
    key" assert).
- **The gate** (`integration_tests::wasm_matrix_compiles`) globs the fixtures, generates each
  `--wasm=true`, and `cargo check`s the wasm crate (which path-depends on the rust crate, so rust-side
  errors surface too — a couple of skip-listed reds are in fact rust-crate generation bugs the sweep
  caught, not wasm-boundary ones). It follows `feature_corpus_compiles`' shared-target-dir *pattern* but
  uses its **own** scratch + `CARGO_TARGET_DIR` (`cddl_codegen_wasm_matrix`), deliberately separate so
  the two tests don't collide when `cargo test` runs them in parallel. The verdict is **compile** for
  now — note a cell can compile green while emitting *semantically* wrong bindings (e.g. an identity
  `.into()` where a transform was needed); catching those awaits upgrading the verdict to *round-trip*
  once the property round-trip harness lands (see [`TESTING_ROADMAP.md`](TESTING_ROADMAP.md) item 1; the
  wasm-verdict upgrade is the item-2 follow-on that depends on it).

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
> driven by `src/robustness_tests.rs`) is the same projection→fixtures→gate shape on a different axis —
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
