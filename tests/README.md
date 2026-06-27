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
with [`insta`]. No subprocess, no compilation, no `target/` bloat. Two sub-suites:

- **`feature_corpus`** — one tiny CDDL file per language construct in [`tests/corpus/`](corpus),
  generated under every applicable flag profile, plus an IR dump. A one-feature regression yields a
  one-file diff. Snapshots are grouped per feature in `tests/corpus/snapshots/<feature>/`.
- **`whole_program`** — the larger integration inputs (`core`, `preserve-encodings`, `canonical`,
  `json`) each under one known-safe profile, to catch cross-feature interactions.

### Profiles

`ALL_PROFILES` in `snapshot_tests.rs` lists the flag axes that change codegen (`default`,
`preserve`, `json`). By default a corpus file is generated under all of them. If a construct isn't
valid under some flag (e.g. fixed-value fields + `--preserve-encodings`, cddl-codegen issue #205),
restrict it with a first-line directive:

```cddl
; snapshot-profiles: default json
my_rule = ...
```

### Adding a feature

1. Drop a tiny `tests/corpus/<feature>.cddl` exercising exactly one construct (see existing files).
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
