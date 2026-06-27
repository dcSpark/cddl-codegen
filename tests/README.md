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

The CDDL standard prelude (`biguint`, `tdate`, `uri`, …) currently panics the generator
(`is_enum` assertion in `intermediate.rs`), so it has no corpus entry despite being listed as a
documented capability. Add one once the generator supports it.

[`insta`]: https://insta.rs
