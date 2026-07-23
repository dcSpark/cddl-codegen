# Tests

cddl-codegen is tested in two complementary layers. Keep them distinct — they answer different
questions.

## Running everything

`check.ts` at the repo root is the single entry point for "run everything that verifies this repo".
It's a dependency-free Bun script built around a gate **registry** — one entry per verification gate
— with three tiers, each a superset of the previous:

| Tier | Command | What it runs | Wall time (warm) |
|------|---------|--------------|------------------|
| `fast` | `bun run check.ts fast` | what CI runs: fmt + clippy + snapshot tests + the drift gates | ~15s |
| `local` (default) | `bun run check.ts` | `fast` + workspace build + the full `cargo test` suite | ~4 min |
| `full` | `bun run check.ts full` | `local` + every manual-only gate | ~30 min |

`fast` is exactly what CI runs (`build.yml` is a thin `bun run check.ts fast` invoker — see the CI
policy below). `local` is "run before considering work done" — the heavy correctness gates (full
`cargo test`, corpus + wasm-matrix compiles) plus `matrix_typecheck` (`tsc --noEmit` over the
`cddl-matrix` scripts, via a dev-only local `typescript`/`@types/bun` — run `bun install` in
`cddl-matrix/` once; the runtime stays dependency-free) and the decode-conformance catalog +
status-header count and doc-citation drift gates (`project_decode_conformance.ts`,
`project_status_headers.ts`, `lint_doc_citations.ts`) live here, NOT in CI. The doc-citation gate
checks that gap prose's cited pins still exist, rejects positional roadmap/list citations, and enforces
blank lines before headings in the hand docs. The conventions it backs: gap-tracking prose names its
pin by exact identifier ("pinned by/tracked by/gated by `name`"), and a *behavioral* claim ("construct
X panics/rejects") gets a robustness-catalog row FIRST — the panic/reject catalogs flip loudly on a
behavior change, where prose-only claims rot silently. `full` additionally runs the
manual gates (<!-- status-header gate roll-call is generated — regenerate with: cd cddl-matrix && bun run project_status_headers.ts --write --><!-- gen:sh:tests-ignored-gates -->the thirteen `#[ignore]`d gates `wasm_matrix_roundtrips` / `multifile_matrix_roundtrips` / `identifier_hazard_crates_compile` / `recombination_crates_execute` / `recombination_preserve_crates_execute` / `recombination_json_crates_execute` / `recombination_wasm_crates_check` / `ir_conformance_corpus` / `rust_oracle_fingerprint` / `decode_conformance_replay` / `corpus_decode_replay` / `all_supported_constructs_generate_all_profiles` / `feature_corpus_roundtrips_nondefault_profiles`<!-- /gen:sh:tests-ignored-gates -->, `cddl-matrix/verify.ts`, `corpus_detect.ts`, and
the fuzz-crate compile-rot check, plus the two gate-cache soundness gates — the input-closure audit `gate_cache_closure_audit` and the flag-gated `verify_cache_transparency` — see the gate-cache section below) — run it before shipping a feature. Every run ends with the **full registry** printed as a table (`PASS` / `FAIL` /
`SKIPPED(reason)` / `STUB` / `not-in-tier` + per-gate durations), so a gate that didn't run is always
*visibly* not-run. Exit is non-zero on any `FAIL`; the run fails fast by default (`--keep-going` runs
every in-tier gate first). Every run also tees its FULL output to a timestamped
`draft/logs/check-<tier>-<stamp>.log` (path printed at start and end) — evidence preservation is
the tool's job, so never pipe a run through `tail`/`grep` as its only capture; cite the printed
path.

`verify.ts` needs two oracles (ruby `cddl`, rust `cddl`); the runner preflights them and prints
install one-liners on failure (`--skip-missing` downgrades a missing oracle to `SKIPPED`). It is the
slowest single gate but not prohibitive: ~170 examples × generate + `cargo test` × 2 crates —
measured ~10-11 min on the dev machine when every cell runs (a `GATE_CACHE=0` or first/all-miss
run; wasm + decode-foreign on), collapsing to ~4-5 min on a hit-heavy re-run against an unchanged
tree (~715 of ~740 cells proven by key — see the gate-cache section below); hours cold, the
shared-target warm-up dominating. The fuzz
gate re-runs `fuzz/generate.sh` only when `fuzz/generated` is absent or `--refresh-fuzz` is passed.
`--cache-transparency` enables the otherwise-`SKIPPED` `verify_cache_transparency` gate (two verify
runs, cached vs `GATE_CACHE=0`, asserted byte-identical — see the gate-cache section).

> **Fold before committing after a `full` run.** The `verify` gate rewrites
> `cddl-matrix/annotations/cddl_codegen.toml`, and it runs AFTER `build_matrix_check` already
> passed earlier in the same run — so a green full-tier summary does not prove the committed
> `matrix.json` matches the refreshed annotations (evidence strings change whenever decode vectors
> were minted since the last run). Run `bun run build_matrix.ts` from `cddl-matrix/` and re-run
> `bun run check.ts fast` before committing; this exact miss has produced a red-on-HEAD CI drift
> gate twice.

The runner's **first gate is three self-completeness meta-checks**: every `#[ignore]` test must be
registered as a manual gate or a known-failing stub, every `cddl-matrix/*.ts` (minus `lib.ts`) must
be wired to a tier, and `build.yml` must invoke `bun run check.ts fast` with no other run step (so
CI can neither drift away from the fast tier nor grow work that bypasses the registry). This is the
systematic catch for the disease the runner cures — a gate that exists but is in nobody's habit — so
a new manual gate or IOU stub is a conscious registry edit, not a silent omission.

Wall times above are warm-cache, measured on the dev machine, and assume NO gate-cache hits (the
uncached worst case — a touch-everything change); after a run on unchanged trees the heavy gates
mostly skip (see the next section). A cold build adds the one-time dependency + test-binary
compile.

### Offline-after-warmup (nested cargo never touches the network)

Local/full runs start with a retried `cargo fetch` warm-up — the workspace (`--locked`), the fuzz
crate, and `tests/warmup/Cargo.toml` (the dep-universe manifest: the union of every crates-io dep
the generated crates can declare) — then set `CARGO_NET_OFFLINE=true` for every gate. The env
propagates through `cargo test` → the suite's nested `Command` spawns and the cddl-matrix scripts,
so every nested-cargo cell resolves from the cargo cache instead of hitting crates.io per temp
crate. This removes the registry-transient flake class by construction (a flaky network/proxy used
to kill otherwise-green runs at a random cell with `unable to update registry crates-io` /
`curl [56] Proxy CONNECT aborted` — cargo's own transient retry never engages on that flavor), and
drops the per-cell `Updating crates.io index` latency as a side effect. The fast tier (CI) is
untouched.

The warm-up manifest is drift-gated: `warmup_manifest_covers_registry_dep_universe`
(`src/cargo_manifest.rs`) asserts every dep the manifest ops can emit appears there with the same
version req and features (features gate optional transitive deps, which `cargo fetch` only pulls
when enabled). Fixture crates under `tests/` with hand-written manifests are the manual tail: a
fixture-only dep missing from the warm-up manifest fails offline cells loudly with
`no matching package named <dep>` — add it to `tests/warmup/Cargo.toml`. Escape hatches:
`CHECK_ONLINE=1` keeps the run online (no offline forcing); a pre-set `CARGO_NET_OFFLINE=true`
skips the fetch and trusts the cache. The warm-up is the ONE place a network retry is honest (a
pure fetch, no assertions behind it); if it fails all attempts the run stops before any gate.

### The gate cache (memoize-and-skip for nested cargo)

The heavy gates spend nearly all their wall time cargo-compiling/testing GENERATED crates whose
bytes did not change since the last green run, so those nested cargo invocations are memoized:
generation always re-runs (cheap, and it is what computes the impact of a change — never a
change→test map, which rots silently), then each nested cargo step hashes everything it consumes
and skips on a key that matched a previously-passing run. The key is sha256 over the whole
generated output tree (all crates — path deps are inputs — hashed AFTER `cargo generate-lockfile`,
so dependency resolution is pinned into the tree and the skipped build would have used the same
resolution by construction), the full `rustc -vV`, `RUSTFLAGS` as the nested invocation sees it,
the command sequence in path-normalized form (scratch paths are run- or checkout-local, so keys
carry the command SHAPE — subcommand + crate role within the hashed tree — never a literal
scratch path, which would make every key unique to its run), and a schema version. A gate whose
cached closure ALSO asserts something beyond the cargo exit code versions that extra verdict logic
into the key as an explicit argv marker (`feature_corpus_compiles`' `lint=unused-imports-v3`), so
changing what the closure checks re-runs every previously-cached cell instead of laundering old
PASSes past the new check. Soundness
rests on the same enforced determinism
invariant the rest of the repo leans on (byte-identical regeneration; `generated_code_clippy_clean`
already relies on the identical-bytes→identical-verdict form of it): an unchanged key means
re-running would provably reproduce the recorded verdict.

Mechanics: entries live in the gitignored `.gate-cache/` at the repo root (one
`<key>.json` per green verdict — existence is the verdict, the body is for debugging which
component moved); only PASSES are cached (a failing or expected-red cell re-runs every time);
corrupt entries read as misses and self-heal on the next green run; `GATE_CACHE=0` disables
read+write entirely and `GATE_CACHE_DIR` relocates the dir (unit tests use it). Skips are never
silent: each covered gate prints a `[gate-cache] <cell>: cached PASS (key …)` line per hit and a
`N run, M cached` summary. CI is unaffected (the fast tier reaches no cached site, and CI starts
from a clean checkout with no cache dir). There is deliberately NO time-based invalidation of any
kind: the industry "nightly cold run" guardrail compensates for unchecked input closures, and this
repo's stance is mechanical per-run enforcement instead — the closure is reviewed at each call
site, mutation-verified red-first at landing (comment-only fixture edit still hits; a rule rename
misses exactly its cells; a corrupted entry re-runs), and `GATE_CACHE=0` exists for suspicion.

Covered sites: `verify.ts`'s per-example rust/wasm probe tests, failure-classifying checks, and
decode-foreign replays (its warm-ups turn lazy — first miss only — behind an always-run
generation-only self-test, so a generator that doesn't build still aborts the run before any
verdict is written); and, via `src/tests/gate_cache.rs`, one cached unit per cell in
`feature_corpus_compiles`, `wasm_matrix_compiles`, `multifile_matrix_compiles`,
`wasm_matrix_roundtrips`, `multifile_matrix_roundtrips`, and the recombination layer-2 batches.
`decode_conformance_replay` is deliberately NOT cached: its success path parses libtest stdout
into per-vector verdicts and completeness counts, so exit status alone is not the consumed result
(the cached-unit rule: a site qualifies only when the harness consumes nothing but exit codes
from a fixed command sequence). `run_test`-based fixture suites are also uncached in v1 — they
reuse export dirs and already replay warm-incrementally through cargo.

**Soundness gates.** The NO-time-based-invalidation stance rests on two obligations, each with a
mechanical full-tier gate INSTEAD of an industry cold run. `gate_cache_closure_audit`
(`cddl-matrix/audit_gate_cache_closure.ts`) protects the KEY side: it traces a real cached gate
under `strace -f` and asserts every file-content read made by a nested-cargo subtree falls in a
class the key provably covers (the generated tree under `$TMPDIR`, `$CARGO_HOME`, `$RUSTUP_HOME`,
system prefixes, and exactly the two user git-config files cargo consults at startup — fetch-side
only, checksum-fenced by the hashed lockfile, so verdict-inert) — a read under the repo checkout is
exactly "a cached site grew an unhashed input"
and FAILs, naming the path, pid, and owning nested-cargo argv. It traces `multifile_matrix_compiles`
by default (its nested `cargo check` transitively builds the `../rust` path dep — the highest-risk
read pattern); `CLOSURE_AUDIT_GATE=<test name>` extends coverage to the other cached gates as
configuration, not code. It prints a visible `SKIPPED` when `strace` is absent, refuses to pass a
trace with zero nested-cargo subtrees (vacuity floor), and statically asserts the repo carries no
`.cargo/config` (an unhashed input for the TS-side sites whose nested cargo runs with cwd = the
repo). `verify_cache_transparency` (`cddl-matrix/cache_transparency.ts`, flag-gated by
`--cache-transparency`) protects the OUTPUT side: it asserts `verify.ts`'s
`annotations/cddl_codegen.toml` and `verify_report.json` are byte-identical between a cached run
(≥1 hit required — vacuity floor) and a `GATE_CACHE=0` run, the direct check that the hit path's
reconstructed verdicts can never leak into output bytes differently than real execution (this
gate has already earned its keep: it exposed cached-vs-uncached divergences down to single lines,
each attributed and fixed — the defenses below exist because of what it found).

verify.ts carries three defenses against nested-cargo verdicts leaning on state OUTSIDE the
hashed tree, all forced by the shared `CARGO_TARGET_DIR`: cargo's leaf fingerprint there is keyed
by package name+version, NOT manifest path, so a `cddl-lib` built AFTER another cell's sources
were written makes cargo declare those older sources "fresh" and reuse the wrong crate's
artifacts — `cargo test` then exits 0 without compiling the cell's bytes (a lazy warm-up runs in
exactly that window: on a cache miss, between the cell's generation and its `cargo test`; the
eager-warm `GATE_CACHE=0` path never can, which made the poison a pure cached-run asymmetry). The
defenses: every generation gets a fresh, counter-suffixed output dir (keep-last-1 deletion; the
Rust gates' per-cell-dir design); `touchTree` bumps every tree file's mtime right before each
MISSED nested cargo (after any warm-up), so the cell's sources are always newer than any
same-name fingerprint and the rebuild is honest; and the warm-ups write their spec to their OWN
`warm.cddl`, never the cell's probe file — a lazy warm-up runs mid-cell, and a shared spec file
would make the cell's later legs (the wasm probe reuses the spec file) silently generate the WARM
crate instead of the cell. None of the layers moves a key: the tree hash is
content-over-relative-paths and the key argv is path-normalized.

| Layer | File | Question it answers | Speed |
|-------|------|---------------------|-------|
| **Golden snapshots** | `src/tests/snapshot_tests.rs` | "Did the *generated source* change?" | fast (~5s, in-process) |
| **Integration** | `src/tests/integration_tests.rs` | "Does the generated code *compile and round-trip*?" | slow (compiles generated crates) |

Snapshots are the fast inner loop and the primary safety net for refactors; integration tests are
the correctness gate. A refactor that doesn't intend to change output should leave every snapshot
untouched — if one moves, you see exactly what changed.

**CI policy — fast tier only.** CI (`.github/workflows/build.yml`) runs exactly
`bun run check.ts fast` and nothing else (CI minutes cost real money — sole maintainer, AI-velocity
commits). The fast tier of the registry is the single definition of what CI does,
and check.ts's `self_checks` gate fails if the workflow grows any other run step. Keep the fast
tier the absolute minimum: new gates default to `local` or `full`; promoting one into `fast` is a
maintainer decision. Everything heavier than the fast tier runs locally, and is documented as a
local/manual run.

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
  `json`, `json-float`, and the `multifile` directory) each under one known-safe profile, capturing
  the *full* output incl. `Cargo.toml`s. Covers cross-feature interactions, the scope/module path,
  and the edition/deps logic. It's also the home for inputs that need a *profile-limited* snapshot
  (`json-float`: floats can't join the corpus, whose snapshots span all three profiles, and
  preserve-encodings is unimplemented for floats). The same constraint routes a single CONSTRUCT
  into a profile-limited input's `.cddl` rather than the corpus: `tagged_type_choice` (tag over a
  whole type choice) lives in `core` because tagging a type-choice enum is unimplemented under
  preserve-encodings. And it's the home for inputs whose output *can't compile
  standalone* (`extern_deps`/`extern_deps_wasm`/`raw_bytes` reference user-supplied types; their
  behavioral coverage is their integration fixtures) — this suite never compiles, so neither constraint bites here, which
  is why such inputs are pinned here rather than via corpus skip-lists that would weaken the corpus
  invariant that every fixture is fully gated.
- **`cargo_toml_matrix`** — a small curated `input × profile` matrix that snapshots every distinct
  generated `Cargo.toml` dependency combination (the type-conditional `hex`/`wasm-bindgen` deps
  toggled independently). The per-feature corpus skips `Cargo.toml` as near-constant noise, and
  `whole_program` doesn't produce every combination, so this is where they're all pinned. Beyond the
  snapshots it asserts each conditional dep is present *exactly* when its flag/type condition holds —
  the absence half guards the manifest changeset's set-or-**remove** contract (a dep whose condition
  turned off must be removed from an existing manifest, not skipped; see `cargo_manifest.rs` — the
  one deliberate exception is the `--export-static-crate` target's changeset,
  `ops_for_static_runtime`, whose conditional deps are set-or-SKIP because that manifest is co-owned
  with a hand-owned crate whose hand code may need a dep the current flavor doesn't). The manifest's
  one tool-owned NON-dep conditional key — the `--rust-wasm-feature` `[features]` leaf paired with
  the now-optional `wasm-bindgen` dep (set under `--wasm` with `["dep:wasm-bindgen"]` or `[]`
  content by c-style-enum presence, removed without `--wasm`) — is pinned byte-wise by these same
  snapshots; its lifecycle/merge contract (including the CML-shaped regen and the legacy
  feature-list repair) lives in the `feature_gate_*` unit tests beside `ops_for_rust`/`ops_for_wasm`
  in `cargo_manifest.rs`. The
  unconditional keys come from a per-manifest append-only change log (`static/manifest_changes/*.toml`,
  the single source of truth — format and editing rules in `static/manifest_changes/README.md`);
  its fold reader hard-errors on non-contiguous ids or a malformed
  entry, so a key the tool ever managed can never be silently unmentioned (removals become permanent
  tombstones by appending a `remove` entry). Its sibling `manifest_template_drift` pins the derived
  `static/Cargo_*.toml` templates — generated snapshots of the logs, never read at runtime —
  byte-for-byte, failing with `BLESS_MANIFEST_TEMPLATES=1 cargo test manifest_template_drift` when a
  log changes without regenerating them.
- **`serialization_prelude`** — the static serialization runtime, snapshotted once per flag
  combination (it ships verbatim into every crate but is assembled differently per flag).

The module also carries non-snapshot **invariant gates** — each sweeps every file the
`whole_program` inputs generate and asserts a property snapshots can't judge (snapshots pin that
emitted bytes don't *change*, not that they satisfy an invariant; a violation just gets blessed):

- `generated_files_start_with_header` — every generated `.rs` in the tool-owned trees
  (`rust/src/generated/**`, `wasm/src/generated/**`) must LEAD with the codegen provenance banner;
  only blank lines, `//` comments, and crate `#![…]` attributes may precede it. It asserts with the
  same banner constant and path predicate the stamper uses (`generation::CODEGEN_HEADER` /
  `is_header_stamped_path` — the stamping is file-level in `generated_files`, so scope-internal
  ordering can't outrank it), over the `whole_program` inputs plus the wasm-list-macro fixture under
  both its profiles. It exists because `codegen`'s `Scope::raw` hoists raw text above everything in
  insertion order: any raw pushed during generation (the class that put `impl_wasm_list!`
  invocations and merged-root module declarations above the header) beats an end-of-run banner raw.
- `deserialize_converts_error_at_most_once` — a generated error-conversion chain maps to
  `DeserializeError` at most once per read (an emission site prepending the conversion without
  checking whether an earlier chain stage already converted emits a redundant identity `map_err`).
- `ok_pattern_parenthesizes_only_tuples` — a generated `Ok` match pattern parenthesizes its payload
  only when it is a real tuple, matching the `final_expr` shaping on the expression side
  (`Ok((x))` on a single binding is redundant grouping parens).
- `no_anonymous_text_list_wrapper` — text arrays cross the wasm boundary as bare `Vec<String>`
  (supported by wasm-bindgen; strings are copied at the boundary, so the by-value ownership hazard
  that justifies struct `*List` wrappers doesn't apply), so no anonymous `TextList` wrapper may be
  emitted.
- `rust_tree_wasm_bindgen_only_feature_gated` — the RUST tree may carry `wasm_bindgen` only in the
  c-style-enum `#[cfg_attr(feature = …)]` form (`--rust-wasm-feature`), never ungated — the
  corpus-wide placement half of the standalone-compile invariant (the feature-off `cargo check`
  half lives in integration, see § "Integration tests"). Carries a positive control: it FAILS if
  no whole_program input emits the gated form, so the sweep can't silently scan a corpus that
  stopped exercising the construct (the fixture-blind-spot class that once graded the rust crate
  bindgen-free from a fixture lacking any c-style enum).

The emission-hygiene gates pin specific shapes found by review; `generated_code_clippy_clean`
provides the systematic lint axis, while needle gates remain for source-shape classes no rustc or
clippy lint can see.

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

To audit a MASS re-bless (hundreds of snapshots, e.g. a dependency upgrade), classify the changed
lines by frequency instead of eyeballing files:

```sh
git diff tests/corpus/snapshots/ | grep '^[+-]' | grep -v '^[+-][+-]' | sort | uniq -c | sort -rn
```

The intended change classes surface as high-count lines; anything unexpected hides in the
singleton tail, so read that tail line by line — an audit that stops at the common classes proves
nothing about strays.

CI also runs `cargo insta test --unreferenced=reject` so a snapshot orphaned by a refactor (one
that stops generating a file) fails the build instead of lingering unnoticed.

## Preservation-merge fixtures (`tests/preserve-fixtures/` + `src/tests/preserve_fixture_tests.rs`)

The edit-preservation overlay (`comment_preserve::preserve` — user comments, insert blocks,
replace blocks; user docs: `docs/docs/preserving_edits.mdx`) is a pure function of
`(old, new) → merged`, so its behavioral tests are **fixture triples independent of codegen** —
they never churn when the generator changes. Each `tests/preserve-fixtures/<case>/` holds:

- `old.rs` — the prior on-disk file (user comments / tagged blocks / carried sentinel blocks);
- `new.rs` — the freshly generated pristine content;
- exactly one expectation: `expected.rs` (byte-exact merge output) or `error.txt` (a substring
  the hard `PreserveError` must contain — used for malformed-tag cases, authored by hand).

One test (`preserve_fixture_tests::preserve_fixtures`) globs the directory. Byte-exact matching
is deliberate — a misplacement that keeps a substring cannot pass — and on top of the blessed
bytes the harness asserts three properties **independent of the blessed content**, so a wrong
`expected.rs` is hard to bless:

- *idempotent fixed point* (pre-rustfmt): `preserve(expected, new).content == expected` — re-running
  the merge on its own output is a no-op (this also covers block carry-forward across regens);
- *never-silent*: every own-line non-doc user comment and tagged block in `old.rs` appears in the
  output either placed or `escape_for_rust_string`-transformed inside a `compile_error!`;
- `changed == false` ⇒ output byte-identical to `new`.

Bless with `BLESS_PRESERVE_FIXTURES=1 cargo test --bin cddl-codegen preserve_fixtures`, then
review the diff like a snapshot. Blessing never creates `error.txt` cases. The directory's
`.gitattributes` (`* -text`) pins CRLF fixture bytes against checkout conversion; per-case
intents live in `tests/preserve-fixtures/README.md`.

What the pure fixtures CANNOT see — assumptions about real generator output (the header banner,
doc ownership) and the disk-level write / toolchain-formatter seams — is
pinned by exactly three integration tests: `comment_preservation_disk_round_trip` (real pipeline;
injects comments + an insert block + a replace block, regenerates twice, asserts the post-rustfmt
fixed point), `comment_preserve_lexer_round_trip_over_corpus` (lexer assumptions vs everything
the generator emits across flag profiles), and `preserve_markers_survive_rustfmt_fold_roundtrip`
(the formatter seam the first test's mid-function replace block cannot reach: rustfmt folds a
match-TAIL block's markers into trailing position — `} // cddl-codegen:replaces` — so this runs
the tool's exact rustfmt pass over a match-tail block and asserts the result re-parses, staying
meaningful across rustfmt versions because "both spellings parse" is the assertion, while the
`replace_rustfmt_folded_tail_arm_markers` fixture family pins the merge semantics of today's known
folded shape). A fourth seam — the overlay's ORDERING against the usage-derived import prune
(`export()` applies the overlay to the in-memory file map, then re-derives the import set from the
post-overlay content, so an import whose last user a replace block removed vanishes from the final
bytes) — is pinned by `comment_preservation_replace_orphans_import_same_file` and
`comment_preservation_replace_in_descendant_orphans_parent_import` (the cross-file flavor that
makes the re-prune family-wide); that property lives in the export driver, not in the merge, so it
cannot be a fixture. Keep both sets thin; new merge behavior belongs in fixtures.

One generator-output assumption is deliberately NOT pinned by that set, because none of the three
can see its violation: "the generator emits no comment on a row a spec change can delete" (which
subsumes "no trailing comments"). The disk round-trip regenerates the SAME spec — no row is ever
deleted — and the corpus round-trip self-preserves (`preserve(content, content)`), a no-op for any
comment whether or not a real regen would strand it. That blindness shipped a real trap once (the
`extern_interface_check.rs` / `key_demand_assertions.rs` per-row markers — see the corresponding
`TESTING_ROADMAP.md` entry), so the assumption is enforced as an emitter invariant instead —
banner-only sidecar files, pinned by `extern_interface_check_regen_over_deletion_no_trap`
(a real regen-over-prior-output with a rule deletion) and
`extern_interface_check_has_no_trailing_row_comments` (a source-shape floor).

Lexer-level tests (char-vs-lifetime, raw identifiers, in-string `//`) stay inline in
`comment_preserve.rs` — they test `lex`, not the merge.

The overlay's one out-of-crate surface — `--export-static-crate`, which writes the composed static
runtime into a consumer-named crate's `src/` and merges that crate's `Cargo.toml` (the upgrade
path for `--common-import-override` runtime crates) — is pinned by
`export_static_crate_writes_composed_runtime_and_manifest` (integration): the flag-set-pure file
set (non_empty\*/raw_bytes always included, prelude-only serialization.rs carrying its own import
header), insert-block survival across a re-export in that dir, flag-off leaving a same-named dir
untouched, the fresh-manifest seed, and the hand-manifest merge (identity/hand deps survive, a
stale `cbor_event` pin is bumped to what the exported source requires).

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
boundary. `tests/extern-deps-wasm/tests_wasm.rs` extends that behavioral floor across CRATES: it
constructs the consumer's wrappers over the mapped dep's types (`--extern-wasm-crate` — the eight
collection wrappers plus the non-root `nested::NestedHolder`), round-trips to byte-identity, and
value-anchors every getter through the dep's `From`/`AsRef` boundary impls, so a semantically wrong
cross-crate conversion fails rather than merely building.

`rust_wasm_bindgen_feature_gated_crate_compiles_standalone` guards the rust crate's
`--rust-wasm-feature` gate from the one direction no other build can witness: every
workspace-style build enables the feature through the wasm crate's path dep (cargo feature
unification), so only a standalone feature-off `cargo check` of the generated `rust/` proves the
crate compiles without the optional `wasm-bindgen` dep. It also scans the generated rust tree for
any ungated `#[wasm_bindgen…]` (the c-style-enum `cfg_attr` form is the only sanctioned
appearance) — per-fixture here; the corpus-wide placement half is the
`rust_tree_wasm_bindgen_only_feature_gated` invariant gate (snapshot suite, fast tier).

The three external-macro flags (`--wasm-list-macro`/`--wasm-conversions-macro` and
`--wasm-cbor-json-api-macro`) emit invocations of a *user-supplied* macro, so the output can't
compile standalone and a source snapshot can't judge invocation semantics; `wasm_list_macro_compiles`
and `wasm_cbor_json_api_macro_compiles` compile-gate them against the real macro definitions in
[`tests/wasm-macro-crate`](wasm-macro-crate) (wired in as a path dependency, the same way
extern-deps wires `tests/extern-dep-crate`). Those macros' arms mirror the inline emission, so the
wrong-emission classes a snapshot would bless — swapped args, wrong `needs_into`/`is_copy`, an
unreachable combination, a wrong arity — fail to compile (see the crate's README).

`extern_wrapper_index_defers_to_dep` pins the `--extern-wrapper-index` deferral surface (a consumer
skips re-minting collection wrappers a dependency's committed `generated/collections.rs` index says
the dep already owns) over `tests/extern-deps-wasm-index` and the dedicated wasm-clean dep pair
`tests/index-dep-crate{,-wasm}` — dedicated because the shared `tests/extern-dep-crate` pair
intentionally double-defines its `#[wasm_bindgen]` class across both crates (the single-crate
convention `extern_deps` needs) and so can never link for the real wasm target. The
`extern-dep-crate` pair also carries the common `Int` the `--common-import-override` cells
re-export instead of minting: `tests/extern-dep-crate` a single `#[wasm_bindgen]` `Int` serving
both faces (the same single-crate convention), `tests/extern-dep-crate-wasm` a wrapper over it
with the `From`/`AsRef` boundary contract. The same pair backs two further cells:
`common_override_wasm_int` (the PURE override consumer — no `_CDDL_CODEGEN_EXTERN_DEPS_DIR_`, the
`--extern-wasm-crate` key naming the override crate itself, with a content assertion pinning the
`Int` wasm face to the WASM crate because the rust stand-in's `#[wasm_bindgen]` `Int` makes a
wrong-direction re-export compile-indistinguishable) and `dep_owned_named_collection_compiles`
(the pair's `DepWithdrawals`/`DepCerts` — transparent `BTreeMap`/`Vec` aliases plus thin wasm
faces — give the dep-owned named-collection cell a full cross-crate compile). Both are hand mirrors of generated `Int` — the
preserve-encodings `Uint`/`Nint` representation, wire impls, and encoding-insensitive key
semantics — enforced today only by round-tripping through the fixture's own impls (the mirror
drift gate is a recorded `TESTING_ROADMAP.md` item, "Negative failure-SHAPE vectors + the
fixture-`Int` mirror gate"). It is a bespoke
harness rather than `run_test`: it asserts the CLI's stderr warning for an all-extern wrapper
absent from the index, the deferred `use <dep_wasm>::collections::…;` imports (plain `use`, never
re-exported), the local-mint cells (not-in-index and mixed-element), a cross-crate behavioral
round-trip via the fixture's `tests_wasm.rs` (constructing through the DEP's wrapper classes — the
DEFERRED-wrapper sibling of `tests/extern-deps-wasm`'s cross-crate `tests_wasm.rs` above), and the
honest link gate: a real
`cargo build --target wasm32-unknown-unknown` of consumer+dep — the only place duplicate
`#[wasm_bindgen]` classes actually fail — asserted GREEN with the flag and RED
(`duplicate symbol`) without it, with a loud skip (hard assert under CI) when the target isn't
installed. It was the suite's first gate compiling a generated crate for the actual wasm target
(the workspace-mode gates below now do too), so the fixture also deliberately INCLUDES a
control-constrained signed-int member
(`local_thing.c: (int .ne 1)`): its emitted i64-window width guard pins `RangeCheck`'s `i128`
fields on a 32-bit target — the class where `isize` fields overflowed the `i64::MIN`/`MAX`
literals, which 64-bit host builds can never see.

### Workspace mode (`--workspace-dep` / `--wrapper-requests` / `--key-requests`)

One cross-crate system: dep-owned placement of all-one-dep collection wrappers via request
sidecars, plus the map-key-derive channel — the consumer's `borrowed_key_types.rs` sidecar and
dep-side pre-finalize `used_as_key` seeding from both request channels, so a dep type keyed only
by a consumer still derives `Eq/Ord/PartialOrd` (+`Hash` under preserve-encodings). User docs:
`docs/docs/command_line_flags.mdx` and `docs/docs/output_format.mdx` § "Workspace mode". Its
facets, each with its own pins:

- **Key flavors.** A borrow carrying a `@used_as_key` flavor (`hash`/`ord`) requests exactly that
  trait family via an optional third row column with per-flavor compiled self-checks; all-bare
  sidecars keep the two-column form byte-identically. The column/parse legs are covered by the
  `wrapper_requests` unit suite (`key_types_accepts_flavor_column`,
  `key_types_rejects_unknown_flavor`); the compiled cross-crate seam by
  `workspace_key_requests_flavored_contract`: a `@used_as_key hash`-tagged dep extern emits the
  three-column row + per-flavor self-check, the dep's `--key-requests` regen derives exactly the
  named family (a hash-only borrow does NOT force `Ord` through the dep's Ord-refusing field),
  both crates compile against each other, and widening the flavor to `bare` fails the dep compile
  naming `Ord`.
- **Scoped self-check paths.** The self-check asserts each borrowed key at the dep's REAL module
  path — scoped (`wr_dep::sub::module::ScopedKey`) when the type lives in a non-root scope, the
  same path the consumer's own generated `use` lines take — while rows stay the bare
  `(dep, cddl ident)` the dep resolves scope-agnostically (no scope column, so the sidecar format
  is unchanged and root-only sidecars are byte-identical). Pinned by
  `borrowed_key_types_self_check_carries_scoped_dep_path` (emission: scoped path present,
  root-path bug form absent, rows bare), `key_types_skips_scoped_self_check_body` (an OLD parser
  reading a NEW scoped sidecar — the self-check body is skipped wholesale), and
  `workspace_key_requests_scoped_contract` (the two-crate compile contract over
  `tests/workspace-requests/consumer_inputs_scoped` +
  `tests/workspace-requests/dep_inputs_scoped`: scoped emit, dep-side bare-ident resolution
  deriving on the SCOPED rule, both directions compile).
- **Mode-independence.** `--workspace-dep` is honored under `--wasm=false` exactly as under
  `--wasm=true` — the key sidecar and the flag's startup validation apply in both modes (only the
  wasm-side deferral surfaces are wasm-gated) — pinned by the flavored contract's rust-only leg
  (byte-identical sidecar, no `wasm/` tree) and
  `workspace_dep_unknown_is_rejected_under_wasm_false` (an unknown dep exits nonzero in rust-only
  mode, never a silent ignore). `--extern-wrapper-index`'s startup validation is likewise
  mode-independent — an unknown dep or a malformed index line is a hard error under
  `--wasm=false`, even though the deferral it feeds is wasm-gated — pinned by
  `extern_wrapper_index_is_validated_under_wasm_false` (both malformation classes exit nonzero in
  rust-only mode).

The whole surface is pinned by three sibling gates plus the parser's unit suite
(`src/wrapper_requests.rs` — both the strict sidecar grammars and the lenient shape-key
extractor):

- `workspace_dep_defers_to_dep` — consumer side over `tests/workspace-dep-wasm/`: unconditional
  all-one-dep deferral incl. NonEmpty and nested shapes, the byte-frozen `borrowed_collections.rs`
  sidecar format asserted as full-file equality — it is a cross-crate contract — plus
  ownerless/mixed composition with `--extern-wrapper-index` and the rule-declared shadowing
  warning (the AUTHORED-rule TRUE positive: `idx_foo_list = [* idx_foo]` warns and mints locally).
  `workspace_dep_named_table_deferred_keys_list` is its FALSE-positive companion (same fixture
  family, isolated input set): a NAMED table over a dep-owned key in a non-root scope
  (`{ * idx_foo => local_thing }`) synthesizes its `keys()`-list `IdxFooList` at registration time,
  so that keys-list must borrow cleanly — no criterion-9 shadow warning (it is not rule-declared)
  and the deferred `use <dep_wasm>::collections::IdxFooList;` import present in the module holding
  the table class (previously stranded — the issue's warned-shadowed + sidecar-borrowed +
  never-imported three-way contradiction, E0412).
- `workspace_requests_hosts_borrowed_wrappers` + the hard-error tests (including
  `workspace_key_requests_derive_effect_and_hard_errors`, the `--key-requests` intake: derive
  effect, unknown-ident hard error, other-dep row filtering; and
  `workspace_key_requests_flavored_contract`, the flavored `@used_as_key hash` cross-crate
  contract over `tests/workspace-requests/*_flavored`: three-column emit, per-family dep derive,
  both-directions compile, and the bare-widening red proof) +
  `workspace_requests_alias_elements_host` — dep side over `tests/workspace-requests/`: strict
  sidecar intake, union-by-shape with sorted requester attribution, own-spec-shape satisfaction,
  flag-order byte-identity, the criterion-8 hard errors plus the review-hardened classes — the
  stub-fidelity diagnosis for directly-exposable shapes, reserved element idents, the
  shape-nesting depth cap, and the element-resolution appendix on name↔shape mismatches — and
  alias-element hosting: request leaves resolve through the pipeline's `resolve_alias`, the single
  owner of the alias-substitution rule, so requested wrappers over
  `stake_credential = credential`-style aliases, primitive aliases, and externs generate exactly
  what the dep's own spec would. `workspace_requests_hosts_cross_scope_elements` is the host-side
  element-import contract: a hosted wrapper's body names its element wasm classes bare, and the
  requested-collections module computes explicit imports for them instead of relying on the
  root-only `use super::*;` — a cross-scope generated element compiles via its true
  `crate::generated::<scope>::…` import (full `cargo check`), and a scoped extern element is
  imported through its crate-root re-export glue (generation assertion — a bare-stub extern has no
  hand-written runtime to compile against).
  `workspace_requests_cohosted_keys_list_no_self_import` is the keys-list twin of that walk's loose-
  `try_from`-source guards: when a hosted map's `keys()`-list wrapper is itself co-requested into the
  same `requested_collections.rs` (the normal case — borrowing `{* k => v}` also borrows `[* k]`), the
  walk must NOT emit `use crate::generated::<KeysList>;` for it (it is minted in that very file, so
  the root defines no such name — E0432); the map's genuine root-hosted element class is still
  imported, and the wasm crate `cargo check`s (RED pre-fix: E0432 unresolved import).
- `workspace_regen_two_consumer_contract` — the regen-contract gate over `tests/workspace-regen/`:
  an umbrella wasm cdylib linking one dep + TWO consumers, RED with duplicate symbols when both
  consumers mint and GREEN after a reverse-dependency-order holistic regen, then the in-place
  lifecycle — zero-diff unchanged regen, requester churn without preservation traps, last-borrower
  removal, and the new-borrow-before-dep-regen unresolved-import failure. The regen gate runs
  every generation IN PLACE over prior output precisely so the edit-preservation overlay
  participates — it is what caught the sidecar's in-const legend comment trapping on borrow
  removal (since relocated to the file banner, where comments anchor to structure that always
  exists).

### Extern-interface export & `--extern-import` (the machine-generated stub channel)

Every regen emits `extern-interface/<dep>/**` — a committed CDDL projection of the crate's
extern-visible surface (opaque `_CDDL_CODEGEN_EXTERN_TYPE_` rows, truthful transparent spellings,
`@rust_name` pins, `; unexported:` exclude-with-record + reference-closure) that a consumer feeds
back via `--extern-import` in place of a hand-stub tree (user docs:
`docs/docs/output_format.mdx` § the export tree, `docs/docs/integration-other.mdx`,
`docs/docs/command_line_flags.mdx` § `--extern-import`). Its test layers:

- **Renderer floor** (`src/generation/extern_interface.rs` in-file vectors): IR→CDDL spelling per
  shape, never-lossy by construction — an unrenderable shape is a hard `Err` the projection turns
  into an exclusion record, never a guessed spelling; the `RustStructType`/`ConceptualRustType`
  matches carry no `_ =>` arm, so a new IR variant fails compilation until it chooses a spelling.
- **Projection snapshots** (`snapshot_tests.rs`, fast tier): `extern_interface_emit` pins the full
  emitted tree over `tests/extern-interface-emit/` (every projection row, depth-1 exclusion of the
  spec's own extern deps, closure records naming the chain root, prelude refs rendering rather than
  excluding); `extern_interface_emit_is_deterministic` (double-emit byte-compare),
  `extern_interface_emit_same_in_both_modes` (rust-only = wasm — emission is mode-unconditional),
  `extern_interface_emit_empty_surface` (header-only file keeps stable presence),
  `extern_interface_emit_exclusions_and_closure`, and `extern_interface_check_emit` (the
  self-check file's content).
- **Compiled self-check** (`integration_tests.rs`, nested-cargo, local tier): every generated rust
  crate carries `src/generated/extern_interface_check.rs` asserting each exported name is a real,
  correctly-bounded type (`Serialize`, per-type-weakened `Deserialize`, `RawBytesEncoding`,
  existence-`use` for transparent rows) — `extern_interface_check_compiles` is the green half;
  `extern_interface_check_mutation_fails_build` deletes one generated type and requires the dep's
  own build to go RED naming it (the stale/hand-edited-export failure mode, proven not assumed);
  `extern_interface_check_weakens_deserialize_bound` / `extern_interface_check_skips_generic_base`
  pin the two soundness carve-outs.
- **Consumer seam** (`src/tests/extern_import_tests.rs`): the acceptance criterion in two halves —
  `extern_import_matches_hand_stub_byte_for_byte` (seam identity: identical rule text through flag
  vs physical stub lands identical bytes) and `extern_import_matches_pinless_hand_stub_byte_for_byte`
  (migration identity: a pin agreeing with today's derivation changes nothing — the half that
  caught pin==derived emitting `use dep::Foo as Foo;`). Plus the strict-seam vectors (missing
  header / unknown version / unknown `@`-annotation / flag-vs-physical double declaration / empty
  path / malformed value all hard-error; an export whose `; unexported:` records mention DSL tags
  still parses cleanly), the wrapped staleness diagnostic, and single-file-consumer ROOT_SCOPE
  preservation.
- **Transitive floor** (same file, `transitive_*` over `tests/extern-import-transitive/`): depth-1
  export purity (a mid-dep transparent rule referencing a base-dep type is closure-excluded, and
  no base-dep ident appears in any exported body), two-flag composition with right-crate `use`
  targeting, the opaque boundary hiding the dep-of-dep, byte-identity at three-crate scale, and
  `transitive_wasm_sidecars_carry_dep_cddl_idents` — the workspace sidecars above
  (`borrowed_collections.rs` / `borrowed_key_types.rs`) byte-identical through either channel,
  rows still keyed by the dep's ORIGINAL CDDL idents (the `--wrapper-requests`/`--key-requests`
  read-back contract).
- **`@rust_name` floor**: `comment_ast.rs` unit vectors plus `src/tests/rust_name_tests.rs`
  (import-seam aliasing, the wasm full-path bypass site, exported-rule rejection, reserved-name
  pin rejection); the directive is lockstep-mirrored in `cddl-matrix/corpus_detect.ts` and
  registered as the `dsl.rust_name` matrix feature row (compile-exempt: it pins a dependency-crate
  type name, so it cannot compile standalone).

`flag_value_smoke` generate + `cargo check`s a rich extern-free input (`tests/canonical`) under each
documented flag *value* that no named profile exercises (`--annotate-fields=false`,
`--to-from-bytes-methods=false`, `--binary-wrappers=true`) — each selects a whole alternative emit
path. `--canonical-form=true` requires `--preserve-encodings` (on its own it emits a non-compiling
crate); that combination is rejected in `api::with_types` and pinned by
`flag_value_rejects_canonical_without_preserve`.

`generated_code_clippy_clean` runs `cargo clippy` over the generated rust and wasm crates for two
representative profiles (default flags, and `--preserve-encodings --canonical-form`), generated from
the same rich extern-free input as `flag_value_smoke` into its own temp dir (so it can't race the
fixtures' reused `tests/<dir>/export` outputs). What it proves: emitted source is lint-clean for
the covered profiles, modulo the permanent input-dependent allow described below — the
emission-quality class that snapshots and round-trip suites are blind to (they pin bytes and
behavior, not idiomatic-ness; a degenerate `();` statement compiles and round-trips green but
degrades every consumer's `cargo clippy`). What it can't prove: semantic correctness — a
wrong-but-idiomatic deserializer passes.

Both generated crates are denied under `clippy::all` with an empty emission-quality burn-down; the
only allow is permanent and input-dependent: `clippy::disallowed_names` (the fixture's own
`foo`/`bar` rule names become generated parameter names — not a generator defect). The gate also
denies a curated rustc style-lint set (`unused_parens`, `unused_braces`, `unused_allocation`) that
catches redundant emitted grouping/allocation without denying `unused_imports` — that class keeps
the one residue the usage-derived import prune (`import_prune::prune_generated_files`)
deliberately leaves: trait imports (`cbor_event::se::Serialize`), exercised via method calls whose
ident never appears, so name-scanning cannot prove them unused. `unused_variables` is also not
denied here, but has NO legitimate residue — its zero-tolerance owner is `feature_corpus_compiles`'
`unused_generated_variable_lines` scan. Everything else — the concrete
collection/encoding idents, the `super::*`/`error::*` globs (pruned against enumerable
universes), cross-scope type imports, wasm macro/prelude imports, and every private import of a
re-export-only extern-glue file — IS pruned at generation time (the contract lives in
`docs/docs/output_format.mdx`; the warning-severity detector is `feature_corpus_compiles`'
`unused_generated_import_lines` scan). It is intentionally not `-D warnings` (see `tool_cmd`'s doc comment). The wasm leg uses the same deny/allow set as the rust leg; any new
`clippy::all` lint class is hard-red on both profiles and both generated crates.
Tier: check.ts `local` as a plain non-ignored test, kept below the ~90s warm wall-clock threshold.
A warm run measures ~2s, which looks vacuous but is not: regeneration is byte-identical, so cargo's
incremental compilation replays content-hashed lint results instead of re-checking — and any real
content change goes Dirty and re-lints (verified by injecting a `();` `no_effect` canary into the
generated source and watching the gate command fail). Re-prove it that way, not by timing, if the
speed ever raises the suspicion again.

Distinct from this generated-code gate, the fast-tier WORKSPACE clippy gate (check.ts's `clippy`)
denies `clippy::all` PLUS the restriction lint `clippy::assertions_on_result_states` over the
repo's own code: an `assert!(r.is_ok())` / `assert!(r.is_err())` discards the payload that would
attribute the failure, so a Result assert takes the `.expect()` / `match` form whose panic carries
the error (a red run — above all a transient one — must be actionable from its first capture). A
genuine can't-Debug case takes a site-local `#[allow]` with a reason — the visible, reviewable form
of the tradeoff. The `is_ok`/`is_err` fragments in `emit_tests.rs` and the replay harness are
EMITTED text compiled in generated crates, outside this lint's scope.

Fixture-appended tests under `tests/*/tests*.rs` are also outside workspace clippy because they
compile only inside generated crates. A textual sweep in the default integration suite bans fresh
`assert!(...is_ok())` there, gated by `fixture_appended_tests_do_not_assert_is_ok`; positive
Result checks should use `.unwrap()` or `.expect()` so the generated-crate failure includes the
error payload.

Deserialize-error annotation contract (the `error_annotation_*` tests in `tests/core/tests.rs`,
plus `error_annotation_tag_mismatch_single_name` in `tests/preserve-encodings/tests.rs`): every
fallible part of a record's header parsing — container major type, definite-length checks, tag
reads — errors with the type name as the location (`Deserialization failed in Foo because: …`),
the same way field-level failures always have, and a tag mismatch carries the name exactly *once*
(inside the `.annotate(name)` closure the tag check is emitted locationless; a name-carrying form
there would read "Foo.Foo"). The two `_control` cases anchor that field-level and
missing-mandatory-field annotation is not lost when the header code is restructured. The
*enum-direct* tag check — a tag over a whole top-level type choice
(`tagged_type_choice = #6.11(uint / text)`), which deserializes directly with no container rep —
is pinned to the same once-only contract by `error_annotation_tag_mismatch_type_choice_direct`,
and the `generate_tag_check_arms` unit test in `snapshot_tests.rs` renders both arms to pin the
name-carrying `--annotate-fields=false` form that no fixture exercises.

An enum `NoVariantMatched` failure on a directly-deserializing choice — where the `_ => NoVariantMatched`
(and group-choice `NoVariantMatchedWithCauses`) arm sits inside the `.annotate(name)` closure —
carries the name exactly *once*: the arm emits the locationless `DeserializeFailure::…into()` form
and lets the closure supply the name, pinned by `error_annotation_no_variant_single_name`.

The contract now covers the two paths that previously sat outside it — embedded/plain-group
`deserialize()` header scaffolding and newtype wrappers' container reads. A standalone plain group's
header reads sit inside a `.annotate(name)` closure that returns the `(len, read_len)` bindings (the
delegated `deserialize_as_embedded_group` call stays OUTSIDE it — its body is already annotated
per-field, so wrapping it would double-annotate as "Type.Type.field"), and its post-delegation
final-len check gets its own annotate closure; a newtype wrapper's whole deserialize body is wrapped
in one such closure while its `new()`/`TryFrom` range check keeps the name-carrying
`DeserializeError::new` form (no closure wraps those). Pinned at fixture granularity by
`error_annotation_plain_group_header_single_name`,
`error_annotation_wrapper_wrong_container_single_name`, and
`error_annotation_bounded_wrapper_range_single_name` in `tests/core/tests.rs` (with an
encoding-fields sibling `error_annotation_wrapper_and_plain_group_single_name` in
`tests/preserve-encodings/tests.rs`), and at catalog breadth by the replay gate: its
`HEADER_MUTANT_LOCATION_SKIP` ledger is EMPTY at HEAD (stale-guarded), the only known-legitimate
locationless resident being the `from_cbor_bytes` `TrailingData` path (pinned by
`error_display_formatting`'s TrailingData no-location case).

`cargo_manifest_disk_round_trip` and `cargo_manifest_rejects_unparseable_existing` pin the
manifest merge contract on real disk (the only place generation reads prior output — see
`cargo_manifest.rs` and AGENTS.md's determinism note): user edits outside tool-owned keys survive a
regen, the seeded `package.version` stays bumped, tool-owned keys (incl. the version stamp) are
restored, a further regen is a byte-identical fixed point, and an unparseable existing manifest is a
hard error naming the file rather than a clobber. Note for harness authors: because manifests merge
rather than clobber, `run_test` deletes the three manifests in its reused export dirs before
regenerating — its raw-appended `test_deps` would otherwise accumulate across runs.

`getting_started_example` pins the documented first-run experience: it generates from
`example/test.cddl` — the spec `docs/docs/getting_started.mdx` tells a newcomer to run verbatim —
and `cargo check`s both the rust and wasm crates, so that command can't rot silently.

### Independent conformance oracle (`tests/deser_test_conformance.rs`)

A round-trip only proves our encoder and decoder agree with *each other* — a symmetric bug passes.
For a second oracle whose **decode + constraint-evaluation** path is independent of ours,
`deser_test_conformance.rs` validates our serialized bytes against the source `.cddl` using the `cddl`
crate's validator (`validate_cbor_from_slice`, which decodes with ciborium and evaluates constraints
itself). A **failure is a strong signal** (our bytes don't match the spec the generator was built
from); a **pass is weak** — the validator has known gaps (they come and go with the pinned fork
rev: the current ledger is `cddl-matrix/README.md` § "Upstream oracle gaps"; e.g. released 0.10.x
does not enforce control ops over a `uint` target) AND it is *not fully decorrelated*: it parses
the `.cddl` with the same dcSpark `cddl` fork at the same pinned rev as the generator's own front end
(`CDDL_ORACLE_DEP`), so a **fork-level misparse** (grammar/AST bug that corrupts generator IR and this
oracle's spec-interpretation identically) escapes it. `CDDL_ORACLE_DEP` is behaviorally checked before
the corpus gate by `rust_oracle_fingerprint_preflight`, using the same
`cddl-matrix/oracle_fingerprint.json` probe set that `verify.ts` uses for the `RUST_CDDL` binary; a
wrong rev, a stale gap pin, or an emptied probe file fails as a harness error before fixture generation.
That still does not decorrelate parser lineage, so the specific fork-misparse class is covered by the
harness-side ruby `cddl` gem in `ir_conformance_corpus` (below), which shares no parser with the fork.
Because the validator validates against a spec's first type rule only, the helper prepends a synthetic
root aliasing the rule under test.

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

The `opt_set`/`opt_set_holder` vectors in the preserve and canonical suites are the wire anchor for
the **transparent tag-set idiom** (`#6.258([* a]) / [* a]`, REQUEST-08): the preserve suite pins
both arms round-tripping byte-exact (untagged, tagged sz-Two, tagged non-minimal sz-Eight), and the
canonical suite pins that `--canonical-form` normalizes the tag's SIZE (wide → sz-Two) but never its
PRESENCE (tagged stays tagged, untagged stays untagged — presence is not canonicalized). These are
the untagged-arm and size-not-presence directions `--emit-tests` alone cannot reach (it mints from
construction defaults, which are always tagged).

### Transparent tag-set idiom (`#6.N([* a]) / [* a]`) — test map (REQUEST-08)

The tag-258 set-idiom collapse (user doc: `docs/docs/current_capacities.mdx` § "Transparent tag-set
idiom") is verified across the layers:

- **Recognition + IR/source shape** — `src/tests/optional_tag_set_tests.rs` (in-process, fast):
  the collapse to a transparent `Vec`/`NonEmptyVec` alias (arm order irrelevant, any tag number,
  preserve tri-state + non-preserve default-tagged), the near-misses that KEEP the enum (mismatched
  bounds, different element types, both arms tagged, 3+ arms), generic-def instances, the
  reference-site outer-tag PARITY invariant (generic instance vs non-generic equivalent generate
  byte-identically), and a collapsed set used as a type-choice variant discriminating coherently on
  the two-entry `cbor_types()` (`Type::Tag | Type::Array`). Generic-instance field convergence
  (Phase 2.5) is pinned in `src/tests/generic_collection_tests.rs`.
- **Anonymous-instance wasm convergence** (REQUEST-09) — `src/tests/generic_collection_tests.rs`
  (in-process source assertions): a SYNTHESIZED anonymous instance at a field site lowers exactly
  like the inline collection it denotes. A wrapper-needing element (`[pool_owners: set<key_hash>]`)
  lowers to the STRUCTURAL name (`KeyHashList`/`NonEmptyKeyHashList`, not the rule-named
  `SetKeyHash`) with a passthrough alias, so the instance and its inline `[* key_hash]` twin are ONE
  wasm class; a directly-exposable element (`set<uint>`) lowers to a bare by-value `Vec<u64>` with
  no class, its wasm output byte-identical to the inline `[* uint]` equivalent
  (`anonymous_exposable_instance_wasm_matches_inline`). Only a NAMED instance rule
  (`named_set = set<key_hash>`) KEEPS its own class. End-to-end against `--wrapper-requests` in
  `integration_tests::workspace_requests_anonymous_collapsed_set_satisfies_from_own_spec`: the
  structural request is satisfied by own-spec (no criterion-8 #3 collision on the synthesized name),
  and the named-rule boundary still hard-errors naming `NamedSet`.
- **Compile + round-trip** — the `tag_set_idiom` / `tag_set_generic` / `tag_set_near_miss` corpus
  fixtures (`feature_corpus` snapshots + `feature_corpus_compiles`' three-profile compile and the
  default-profile `--emit-tests` byte-exact round-trip of the tagged arm).
- **Wire bytes** — the `opt_set` golden vectors above (both arms + canonical size-not-presence).
- **Matrix cells (choice-member axis)** — `contain.choice-member.type2.tag.set_idiom` /
  `contain.choice-member.type2.tag.set_idiom_near_miss`
  (`cddl-matrix/containment/choice-member.toml`): the bare collapse and the mismatched-bounds
  NON-collapse as execution-probed (role × feature) cells, each with per-wire-arm decode-foreign
  vectors in `tests/decode_conformance/catalog.toml` (arm floor `["4","6"]` pinned in
  `project_decode_conformance.ts` — the untagged major-4 arm is the direction the default encoder
  never emits, so those vectors are the independent decode evidence). The reject-flavored idiom is
  pinned separately by the vendor feature row `dsl.duplicates.reject`.
- **Boundary limitations** — `tests/TESTING_ROADMAP.md` § "Deferred features" (non-idiom
  choice-bodied generic-def crash, alias-of-instance chains, inline choices, and the pre-existing
  multi-tag-per-field encoding-var collision).

### Per-rule duplicates policy (`@duplicates`) — test map

The **`@duplicates reject` flavor** (set/array collections — user doc:
`docs/docs/output_format.mdx` § "Reject-duplicates containers", `docs/docs/comment_dsl.mdx`
§ `@duplicates`) layers a uniqueness twin (`OrderedSet`/`NonEmptyOrderedSet`) onto the tag-set
idiom above, verified across the same layers as the idiom plus the cross-crate ones:

- **Corpus fixture + compile** — `tests/corpus/tag_set_reject.cddl` (five reject shapes + a `holder`
  embedding them incl. an optional field: `int_set` `[*]` idiom, `int_neset` `[+]` idiom, `text_set`,
  `oset_u64` a named generic-instance of a reject generic def, `plain` a non-idiom array) drives the
  `feature_corpus` snapshots and `feature_corpus_compiles`' three-profile compile. Its sibling
  `tests/corpus/tag_set_alias_instance.cddl` pins the ALIAS-of-instantiation shape
  (`required_signers = nonempty_set<...>` with a second anonymous use — the CML regen shape): the
  `pub type` alias to the minted nominal, its resolved-policy self-doc, the opaque
  extern-interface row, and the `typescript_custom_section` TS alias line; its decode-conformance
  rows ride the same catalog as `tag_set_reject`'s below. In-process polarity/seam pins:
  `optional_tag_set_tests::alias_binding_set_nominal_documents_resolved_reject_policy` (doc says
  `NonEmptyOrderedSet` + reject, never the inverted `NonEmptyVec`/preserve texts),
  `snapshot_tests::extern_interface_projects_alias_to_set_nominal_as_opaque`, and
  `optional_tag_set_tests::alias_binding_set_nominal_wasm_surface_flattens_and_names_the_rekey`
  (the flat wasm nominal surface + the JS re-key naming).
- **Preserve-mode floor + reject KATs** — `tests/golden_hex_preserve/tests.rs`: the duplicate-carrying
  `opt_set_{untagged,tagged}_duplicate` / `opt_neset_{untagged,tagged}_duplicate` KATs pin that the
  DEFAULT (`preserve`) accepts and re-emits duplicates byte-exactly (the regression floor `reject`
  narrows from), while `reject_set_untagged` / `reject_set_tagged_wide` pin a duplicate-FREE reject set
  round-tripping byte-exactly and the in-process `reject_set_duplicate_wire_and_api_identical` pins the
  wire door and the API door reporting the same `DuplicateKey`. The std set contract on the runtime
  twins (`insert -> bool`, `contains`, keep-first `Extend`/`FromIterator`, `sort`, `try_opt_from`,
  the `OrderedSet` ↔ `NonEmptyOrderedSet` refinement doors) and the set nominals' emitted
  `try_opt_from` are covered e2e by `reject_set_std_contract_and_refinement_doors` in the same
  suite.
- **Decode-conformance (composition depth)** — the `tests/decode_conformance/corpus_catalog.toml` rows
  `tag_set_reject.{holder,int_set,int_neset,oset,oset_u64,plain,text_set}` (duplicate-free spec-derived
  vectors the generated decoder must accept), replayed by the `corpus_decode_replay` gate.
- **Extern-interface projection** —
  `src/tests/extern_import_tests.rs::extern_import_projects_duplicates_reject_no_cross_crate_skew`: the
  `dep-reject/lib.cddl` → `consumer-reject/lib.cddl` two-crate fixture proves the directive travels on
  the export so the consumer rebuilds the reject door (not a preserve-mode `Vec` that would accept what
  the dep rejects), with a negative-control skew check.
- **Workspace-requests hosting** —
  `src/tests/integration_tests.rs::workspace_requests_hosts_reject_ordered_set_twins`: the
  `tests/workspace-requests/sidecars/reject_borrowed_collections.rs` sidecar drives the dep to host
  `IdxFooOrderedSet` / `NonEmptyIdxFooOrderedSet` in its `requested_collections.rs`.
- **Preserve-encodings byte-fuzzer leg** — `tests/preserve-encodings/input.cddl`'s `oset_p<a>` generic
  reject collapsed set (used by `reject_set_preserve`), regenerated by `fuzz/generate.sh` into the
  `from_cbor_bytes` fixture; currently compile-checked only, with a live-run fold-in tracked by
  `tests/TESTING_ROADMAP.md` § "Byte-fuzzer depth: the tag-set peek path + reject door are wired, but
  only compile-checked".
- **Graceful-rejection matrix** — `src/tests/robustness_tests.rs`:
  `duplicates_directive_rejects_gracefully` (permanent no-policy placements) and
  `duplicates_directive_accepts_live_and_default_noops` (live set/array `reject` plus the accepted
  default no-ops).

The **`@duplicates preserve` flavor** (tables — user doc:
`docs/docs/output_format.mdx` § "Preserve-duplicates tables", `docs/docs/current_capacities.mdx`
§ "Preserve-mode tables", `docs/docs/wasm_differences.mdx` § "Preserve-duplicates tables",
`docs/docs/comment_dsl.mdx` § `@duplicates`) is the TABLE mirror of the reject flavor: a table rule
carrying `@duplicates preserve` swaps its transparent alias to the `Vec<(K, V)>`-backed pair-map twin
(`{*}` → `PairMap`, `{+}` → `NonEmptyPairMap`), the only shape faithful to both entry order and
duplicate keys (driver: byte-exact round-trip of pre-Conway Cardano `transaction_metadata`). Verified
across the same layers:

- **Wire bytes (byte-exact dup round-trip)** — `tests/golden_hex_preserve/tests.rs`:
  `pmap_duplicate_key` and `pmap_duplicate_key_nonminimal_head` (a duplicate-keyed map decodes AND
  re-emits byte-exactly, the second proving per-entry POSITIONAL encoding — a non-minimal head on one
  entry re-emits faithfully), plus the in-process `pair_map_surface_and_nonempty_door` (the pair-map
  read surface — `get` first-match / `get_all` — and the `{+}` NonEmptyPairMap min-1 door). The
  recursive union-keyed table (`{* md => md}`) headline trips a pre-existing, policy-independent
  keys-list synthesis panic, recorded as a residual in `tests/TESTING_ROADMAP.md`.
- **Canonical stable-sort** — `tests/golden_hex_canonical/tests.rs`: `canon_dup_pmap_key_sort` and
  `canon_dup_pmap_nonminimal_head` pin that `--canonical-form` stable-sorts entries by encoded key
  bytes with duplicates adjacent in first-appearance order and minimizes per-entry heads independently
  (the positional sidecar is what lets same-key entries canonicalize separately) — the deterministic
  best-effort for data with no RFC 8949 canonical form.
- **JSON (array-of-pairs)** — `tests/json/tests.rs`: `preserve_pair_map_json` (a preserve table
  serializes as a JSON ARRAY of `[k, v]` pairs — order and duplicates intact — not an object),
  `ne_preserve_pair_map_json_door` (the `{+}` door refuses an empty `[]` with the same min-1 error),
  and `schemas_reject_wrong_shapes` (the emitted `schemars` schema is an array-of-pairs that REJECTS
  an object shape for the field).
- **Core wasm (appending insert)** — `tests/core/tests_wasm.rs::wasm_preserve_pair_map_insert_appends`:
  the pair-map wasm wrapper's `insert` APPENDS (a repeated key grows `len`, never replaces) and returns
  `Option`, the opposite of the reject set's fallible `add`. The emit-tests PairMap synthesis leg
  round-trips it.
- **Robustness pins** — `src/tests/robustness_tests.rs`:
  `duplicates_directive_accepts_live_and_default_noops` (the core lowering: `{*}` → `PairMap`, `{+}` →
  `NonEmptyPairMap`, alongside the live set/array `reject` and the accepted default no-ops),
  `duplicates_preserve_nonempty_table_lowers_to_twin_under_wasm` (the `{+}` NonEmptyPairMap wrapper
  crosses the wasm boundary), `generic_preserve_table_instance_lowers_to_pair_map_under_wasm` (an
  anonymous generic table instance recovers the pair-map flavor from its shape), and
  `duplicates_preserve_pair_map_shape_collision_rejects_gracefully` (the fourth-kind collision detector
  — a preserve table sharing a structural map shape with a genuine inline map or a non-preserve `{+}`
  table is a distinctly-worded graceful rejection).
- **Extern-interface projection** —
  `src/tests/extern_import_tests.rs::extern_import_projects_duplicates_preserve_no_cross_crate_skew`:
  the `dep-preserve/lib.cddl` → `consumer-preserve/lib.cddl` two-crate fixture proves the directive
  travels on the export so the consumer rebuilds the pair-map twins (not a reject-default `BTreeMap`
  that would REJECT the duplicate keys the dep preserves — the mirror skew), with a negative-control
  skew check.
- **Matrix feature rows + decode conformance** — `dsl.duplicates.{reject,preserve}` are registered
  vendor feature rows (flavored siblings, no bare row — the bare directive panics on its missing
  argument; recipe notes: `cddl-matrix/README.md` § "Registering a new vendor (CDDL_CODEGEN)
  feature row"), with minted decode-foreign catalog rows and the `table_preserve.cddl` corpus
  fixture's minted corpus decode rows.
- **wasm-ABI + multifile matrix grid rows (both flavors)** — the per-role grid layer on top of the
  per-fixture pins above: the reject twins as `rset`/`nerset`/`rseta`/`nerseta` and the preserve
  pair-map twins as `pmap`/`nepmap`/`pmapa`/`nepmapa` in `project_wasm_matrix.ts`'s `SHAPES`
  (named-rule + anonymous-instance flavors, each × all 8 boundary roles, compile floor +
  three-profile round-trips) and the same shapes × 3 reference modes in the multifile placement
  matrix. Enumerating them found + fixed the `[*]`-reject wasm-boundary conversion gap (E0308,
  pinned by `newtype_over_plain_reject_ordered_set_converts_wasm_boundary`) and the cross-module
  restricted-wrapper placement class — every collection occurrence resolves its wasm wrapper name +
  home scope through `wasm_collection_wrapper`, and a field referencing a named/dep-owned collection
  rule keeps only the rule ident (the `Alias` arm's structural-wrapper suppression).

### Decode-direction conformance (`tests/decode_conformance/` — accept what the spec accepts)

The fourth gate direction. The three above are all blind to an **over-strict decoder**: round-trips
only decode what they themselves encoded, the conformance oracles validate *our emitted* bytes
(encode side), and the reject tests check that spec-INVALID input is refused. A generated decoder
that rejects spec-VALID CBOR passes all of them — proven twice on this layer's first sweep (below).
This layer feeds SPEC-DERIVED CBOR instances our code did *not* produce into the generated decoders
and asserts they are accepted. Two mechanically-projected obligation sets drive it, each with its own
committed catalog under `tests/decode_conformance/`: the matrix's per-construct BREADTH (the bullets
below) and the corpus fixtures' composition DEPTH (§ "Composition-depth (corpus) leg").

- **The committed corpus** — `tests/decode_conformance/catalog.toml`, machine-produced (same
  artifact class as `cddl-matrix/matrix.json`). The obligation set is PROJECTED from the matrix's
  `supported` rows (features + containment cells + control-ops), never hand-curated: every
  supported row carries ≥1 committed vector or a mechanical `pinned_reason` (no silent skips).
  Vectors are minted by the ruby `cddl` gem's instance generator (`cddl <spec> generate` →
  `diag2cbor.rb`) and committed only after validating against BOTH oracles — ruby `cddl validate`
  AND the rust CLI as `cddl --ci validate` (without `--ci` the rust CLI prints the error but exits
  0; a mint-time negative control feeds both oracles a known-bad instance so that trap can't
  silently vacate the cross-check). Contested vectors are dropped, never committed. A rule with no
  standalone decode surface (transparent alias / named table / c-enum — no nominal
  `impl Deserialize`) is minted in **holder mode**: vectors wrap the rule in
  `__probe_holder = [0, <rule>]` (prepended FIRST — both oracles root validation at a spec's first
  rule) so decoding routes through the *generated* field-decode path rather than cbor_event's
  blanket impls.
- **Refresh flow** — `cd cddl-matrix && bun run verify.ts --mint-decode-foreign` (or
  `--only=<id,…>` to re-mint a subset, preserving the rest byte-identically; an `--only` id that
  has LEFT the supported set but still has a committed row is DROPPED — the support-boundary
  removal flow, e.g. a construct newly rejected at generation — while an id that is neither
  supported nor an existing row still hard-fails as a typo). The mint phase is
  mint-ONLY (writes the catalog, never annotations) and takes "supported" from the committed
  `matrix.json` — so a row whose verdict just flipped needs the plain probe run and a
  `build_matrix.ts` fold BEFORE it can mint; symmetrically, a plain probe run AFTER the mint (plus
  another fold) refreshes the row's decode-foreign evidence clause, which otherwise still reads
  "no committed decode vectors" from the pre-mint probe. Generation is
  randomized, so verdict stability comes from the COMMIT: the deterministic gates below replay
  committed bytes only. `project_decode_conformance.ts` also compares each supported row's committed
  evidence clause with the catalog's spec-valid accept-vector count, excluding
  `class="over-acceptance"`; this catches the proven scoped-mint-after-probe drift where
  `record_array_tagged` minted vectors while its evidence still claimed none. A spec-valid vector the
  decoder rejects is written as a **class-less
  `expect = "reject"` pin and the mint exits 1**; the drift gate stays red until a human triages it
  into `class = "bug"` (ledger it in `cddl-matrix/ROADMAP.md` § findings) or
  `class = "limitation"` (cite `current_capacities.mdx` / the overlay note). `source = "hand"`
  supplement vectors survive re-mints and are re-validated like any candidate.
- **Reject vectors split by class** — two opposite spec-validity claims live under `expect="reject"`:
  - `class = "bug" | "limitation"` — spec-VALID CBOR the decoder WRONGLY rejects (the wrong-rejection
    pins above). Re-validated **spec-VALID** (both oracles accept) at each mint; PRUNED when the gap
    closes.
  - `class = "constraint"` — spec-INVALID CBOR (`source = "hand"`) that VIOLATES a constraint the row
    enforces (an over/under-`.size` string, a numeric-op boundary violation like `11` against
    `int .le 10`, a non-uint `.cbor` payload, a cut-violating map value) and that the generated
    decoder must **durably reject**. Re-validated **spec-INVALID** (both oracles reject — the
    inverse gate) at each mint; NEVER pruned. Two hand-authored fields pin the rejection's identity:
    `reason` names the violated constraint (prose, for humans), and `expect_err` is a substring the
    generated decoder's error Display must contain — the replay gate asserts it, so a decoder that
    rejects for a subtly WRONG reason (a stray length check, an unrelated error path) fails the gate
    instead of passing as it would under a bare `is_err` check. The drift gate REQUIRES `expect_err`
    on `class="constraint"` and forbids it elsewhere; a mint round-trips both fields verbatim.
    Authoring `expect_err`: pick a generous discriminating fragment of the generator-emitted Display
    including the bound and the vector's own found value (both deterministic — same bytes, same
    decoder), e.g. `11 not at most 10`, `not in float range (>=0.5, <=10.5)` — formats in
    `static/error.rs`; if the captured Display does NOT name the violated constraint, that is a
    wrong-reason rejection to investigate, never a string to pin. This class is Q4's
    `enforce = yes (bounded-reject)` evidence (`query_q4_directional.ts` counts `class="constraint"`
    only). NOTE: the numeric range/eq rows carry these vectors only because their probe examples
    target `int` with literal, non-vacuous bounds — the rust corroborating oracle (`cddl` 0.10.x)
    does not enforce these ops over a `uint` target (upstream gap,
    `draft/rust-cddl-uint-control-op-gap.md`), so a `uint`-targeted form can't pass the both-reject
    gate; `query_q4_directional.ts --check` pins the exact green set against such a decay. The
    `rangeop` rows with non-uint endpoints (`.int`/`.nint`/`.float`) sat on a SECOND rust-oracle gap
    (`draft/rust-cddl-float-range-gap.md`: released 0.10.x `validate` blanket-rejects every instance
    of a float or negative-int range); the fork's `885c61c` fix closed it, so those rows carry real
    accept vectors and discriminating rust reject corroboration (the float vectors' `reason` records
    the provenance).
    **Authoring rule — vector SHAPE is load-bearing:** a constraint vector for a `standalone` row is
    a BARE in-type instance of the row's type (`0b`, `fb…`), decodable up to the constraint itself so
    the emitted range/size check is the only possible rejection. A holder-wrapped scalar
    (`8200…` = `[0, x]`) against a standalone row rejects as a TYPE mismatch before any bounds check
    runs — the reason assert would catch that behaviorally (the TYPE-mismatch Display doesn't contain
    the range/size fragment), but `project_decode_conformance.ts` § 6 also bans it STATICALLY at the
    cheap drift-gate tier. The `8200` holder prefix belongs only to `mode = "holder"` rows; a row's
    accept and reject vectors must share their outer CBOR shape. § 6 enforces this mechanically
    (leading major-type class vs the row's accepts, majors 0/1 merged; the holder preamble banned on
    accept-less standalone rows).
- **The vector-class 2×2 (current decoder behavior × spec validity).** `expect` always pins CURRENT
  behavior (what the replay asserts); `class` carries the spec-validity/triage label:

  | | spec-VALID bytes | spec-INVALID bytes |
  |---|---|---|
  | decoder **accepts** | plain `expect="accept"` (no class) | `expect="accept"` + `class="over-acceptance"` |
  | decoder **rejects** | `expect="reject"` + `class="bug"\|"limitation"` | `expect="reject"` + `class="constraint"` (+ `expect_err`) |

  The fourth cell is `class="over-acceptance"` — certified-spec-INVALID CBOR (both oracles reject at
  mint, the same inverse gate as `class="constraint"`) that the generated decoder CURRENTLY (wrongly)
  ACCEPTS: a known silent-acceptance bug with no enforcing fix yet. It is `source="hand"`, requires a
  `reason` (citing the ledgered finding + the promotion flow), is FORBIDDEN `expect_err`, survives
  re-mints VERBATIM, and is re-validated spec-INVALID at each mint (never pruned mechanically). The
  replay gate asserts it STILL decodes Ok ("still wrongly accepts"), so when a fix lands the pin flips
  LOUDLY — the signal to PROMOTE it to `class="constraint"` (+ `expect_err`) and flip the row's Q4
  projection green (the `KNOWN_SILENT_DROP` / `EXPECTED_COMPILE_FAIL` pattern applied to decode). Q4
  projects a carrying row as the honest `enforce = no (over-accepts: M)` (dominating `yes`/`unverified`;
  pinned by `query_q4_directional.ts --check`'s `EXPECTED_ENFORCE_OVERACCEPTS`). A spec-INVALID accept
  vector NEVER counts as spec-valid decode evidence: it is excluded from the verify.ts decode-foreign
  corroboration count, from Q4's foreign-decode count, and from the replay gate's encoding-variant /
  header-mutation / preserve legs. Zero instances at HEAD — the class stays armed for the next
  certified instance. Its retired residents were the widened-occurrence-marker table rows
  `contain.occurrence-target.memberkey.type1.{plus,optional,bounded}_table`: a COUNT-PERMITTING
  marker (`+` / `?` / `n*m`) on a single non-literal arrow map entry table-detected to the same
  unbounded 0..N map as `{ * k => v }`, wrongly accepting out-of-window maps, until `+`/`1*` was
  honored as a `NonEmptyMap` (plus_table's empty-map pin promoted to `class="constraint"`) and the
  `?`/`n*m` spellings became graceful rejections (`tests/matrix_reject/` fixtures) — provenance in
  `cddl-matrix/ROADMAP.md` § findings, the 'Real bounded `?` / `n*m` table cardinality is a
  candidate feature' entry. (The seed instance was the
  no-occurrence type-domain arrow widening `{ tstr => uint }`, whose empty-map instance `8200a0` =
  holder `[0, {}]` was likewise wrongly accepted — it took the flow's OTHER branch, rejected gracefully
  at generation, pinned by `no_occurrence_arrow_map_entry_rejects_gracefully`, its row flipped
  unsupported and the pin dropped with it.) (`8200a0` also remains the seeded-control *accept* on
  `type2.map` — `{ * tstr => int }`, a spec-VALID empty table there.)
- **The replay gate** — `integration_tests::decode_conformance_replay` (`#[ignore]`d, check.ts
  `full` tier, ~6 min): per active row it generates a crate from the committed `spec` and `cargo
  test`s it under two profiles (default + preserve), plus a third json/wasm-surface generation
  (§ "json/wasm surface legs" below). Oracle-free and deterministic — the bytes were spec-cross-validated
  at mint time, so the gate replays commitments, never re-derives them. Three assertion legs run on
  the DEFAULT-profile build, sharing one failure-attribution grammar. Shared across every leg body
  that captures an error Display (the constraint and header-mutant Err arms, both profiles): an
  emitted helper asserts the displayed location chain has no adjacent-duplicate segment — a doubled
  location ("Foo.Foo", the generator double-annotation class) *satisfies* a bare `failed in {name}`
  contains, so without this check the location asserts below cannot see it. Justified exceptions go
  in the stale-guarded, empty-at-HEAD `DOUBLED_LOCATION_SKIP` ledger; the helper ships an emitted
  self-check per replayed crate, counted by the per-crate completeness check so it can't silently
  vanish. The legs:
  - *Base replay* — every accept vector decodes Ok and every reject pin still Errs (**a pin that
    starts decoding green FAILS the gate** — a re-bless can't silently launder a bug). Each
    `class="constraint"` vector additionally asserts the error Display CONTAINS the catalog's
    `expect_err`, pinning the rejection REASON — a wrong-reason rejection fails the gate with the
    captured Display (a vacuity floor keeps ≥ 40 reason asserts live). A `class="over-acceptance"`
    vector emits its own `over_accept_N` test asserting the decoder STILL (wrongly) decodes it Ok; a
    rejection is the pin FLIP (the fix landed), attributed by `classify_over_acceptance_failure` with a
    marker naming the promotion flow, and a completeness guard asserts the emitted `over_accept_*` count
    equals the catalog's over-acceptance vector count. These vectors are excluded from the two legs
    below (spec-invalid bytes evidence nothing about the spec's shape).
  - *Encoding-variant leg* — each accept vector is replayed through mechanically-derived spec-EQUAL
    re-encodings (the shipped `cddl_encoding_fidelity::variants` mutator, reused harness-side:
    indefinite framing, non-minimal int/len widths, chunked strings, reversed maps): a re-encoding
    the decoder REJECTS (over-strict, the motivating class) or mis-decodes to a different value
    fails the gate. `ENCODING_VARIANT_SKIP` (stale-guarded, empty at HEAD) would ledger any
    (row, label) that legitimately fails against a `cddl-matrix/ROADMAP.md` finding; a variant-test
    vacuity floor keeps the leg live.
  - *Header-mutation leg* — each accept vector also derives spec-INVALID reject mutants
    (`header_mutants`, pure byte transforms of the item-under-test's leading CBOR head; holder rows
    mutate past the `82 00` = `[0, _]` preamble): `wrong_major` flips the major type, `trunc_head`
    re-encodes the head with an 8-byte argument then drops its final byte (ill-formed by
    construction). A `wrong_major` flip landing on a major the row's own accept vectors evidence
    (majors 0/1 merged, the drift gate's § 6 merge) is skipped at DERIVATION time: such a mutant is
    ambiguous (possibly spec-valid — `type.choice`'s bstr↔tstr flip lands on the other
    `uint / tstr / bytes` arm), and skipping only the ambiguous flips keeps the row's non-ambiguous
    mutants live where a (row, label)-wide ledger entry would swallow a future over-acceptance.
    Each emitted mutant must be REJECTED **and** the error Display must carry a location naming the
    decoding type (`failed in {type_name}` — the annotation analogue of the base leg's
    `expect_err`, at catalog breadth rather than the fixture-granularity `error_annotation_*`
    tests; a bare `type_name` contains is deliberately NOT used, since single-letter type names
    like `T` would vacuously match "TagMismatch"). Two stale-guarded ledgers hold the honest
    exceptions: `HEADER_MUTANT_ACCEPT_SKIP` — a mutant the row's spec genuinely accepts WITHOUT any
    accept vector evidencing that major (an `any`-typed row, an unsampled choice arm; one resident
    at HEAD: `(prelude.any, wrong_major)`, since `x = any` accepts every major by definition;
    `trunc_head` can never be here, asserted as a hard error) — and
    `HEADER_MUTANT_LOCATION_SKIP` — a rejection carrying no location: EMPTY at HEAD now that
    embedded/plain-group header scaffolding and newtype-wrapper container reads are annotated, with
    the locationless `from_cbor_bytes` `TrailingData` path the only known-legitimate resident (no
    header mutant reaches it here, so the ledger stays empty). A header-mutant vacuity floor keeps
    the leg live.
  - *Failure attribution* — a FAILED replay test's cause is attributed by pure
    marker-classification functions (`classify_constraint_failure` / `classify_variant_failure` /
    `classify_header_mutant_failure` / `classify_over_acceptance_failure`) whose needles own the
    trailing ':' that disambiguates prefix-colliding libtest names (`reject_1` vs `reject_10`,
    `over_accept_1` vs `over_accept_10`); that grammar is pinned unit-side (no crate build) by
    `integration_tests::classify_constraint_failure_disambiguates_prefix_colliding_names` and its
    variant/header-mutant/over-acceptance siblings
    (`classify_variant_failure_owns_the_delimiter_and_maps_each_marker`,
    `classify_header_mutant_failure_disambiguates_prefix_colliding_names`,
    `classify_over_acceptance_failure_disambiguates_prefix_colliding_names`); the header mutator
    itself is pinned by `header_mutants_pin_hand_derived_bytes`.

  Finally it regenerates under `--preserve-encodings=true` and asserts accept vectors decode AND
  re-encode **byte-identically** (the preserve contract is itself decode-direction evidence).
  `PRESERVE_SKIP` (stale-guarded) carries the float class plus the tag-over-a-type-choice preserve
  gap; anything new there is a finding. It stays a hand list on purpose — it is NOT the matrix
  emission axis: the replay specs embed rows as members, so e.g. `prelude.float` skips here while
  its `emission.preserve` verdict (a bare-alias probe) is `supported`.
- **The drift gate** — `cddl-matrix/project_decode_conformance.ts` (check.ts `local` tier, pure
  file reads): matrix-supported ↔ catalog completeness, example-drift staleness (a drifted example
  means the vectors were validated against a spec the matrix no longer describes — re-mint),
  reject-pin class/reason/`expect_err` shape, accept-vector class (no class, or exactly
  `class="over-acceptance"` with a `reason` and no `expect_err`), the § 6 shape rule extended to
  over-acceptance vectors (same-shape as the row's SPEC-VALID accepts, which now EXCLUDE over-acceptance
  from the shape-class set), and the hard-coded **seeded regression controls** — the
  absent-instance vectors (`occur.optional` holder `[0, []]`, `type2.map` holder `[0, {}]`,
  `occur.zero_or_more` holder `[0, []]`) that anchor the over-strict-decoder class TDD-style. It ALSO
  runs the **writer↔reader identity check** (§ 8): `composeCatalog` (in `cddl-matrix/lib.ts`) is the
  SOLE serializer of the hand-authored vector fields, so the gate asserts `compose(parse(catalog.toml))`
  is byte-identical to the committed file — a writer that drops or reorders a field (the silent-strip
  bug class: `class`/`reason` once emitted only under an `expect === "reject"` guard, which would have
  stripped every over-acceptance annotation) goes red before any re-mint corrupts the catalog. A
  synthetic all-fields sample round-trips through `parse∘compose` in the same section, so a dropped
  field is caught even when the committed catalog does not currently exercise it. It ALSO
  runs the **arm-coverage floor** (§ 7): the mint's `generate` is randomized, so a multi-arm CHOICE row
  can land with a whole arm unsampled and its decode verdict silently under-claims (the seed instance:
  `prelude.number` = `int / float` carried only int-headed accepts — the float arm had zero
  decode-direction evidence). For each active choice row whose root RHS statically resolves to arm head
  major-classes (`resolveChoiceArmClasses` in `cddl-matrix/lib.ts` — the ONE resolver the mint's
  resample loop shares), the floor requires ≥1 spec-valid accept vector per resolvable arm class.
  **Majors 0/1 merge into one "int" class**, so `prelude.integer` / `prelude.unsigned` don't flag their
  unsampled plain-uint side (nint already covers int). Two decay pins guard it: `EXPECTED_FLOOR_SCOPE`
  pins the EXACT (row → sorted arm classes) set the resolver fires on (a silent widen/narrow fails
  got/want), and `DECODE_FLOOR_ARM_EXEMPT` (`lib.ts`, stale-guarded) ledgers a genuinely unmintable arm
  class with a citation. Mint side (`verify.ts mintRow`): a **resample-until-covered loop** draws extra
  bounded `generate` batches for any missing class, keeping only two-oracle-valid candidates; on cap
  exhaustion with an unledgered missing class the mint exits 1, naming the row and class. At HEAD the ledger is EMPTY — its one past resident (`prelude.number`'s float arm, unmintable while
  the rust reference rejected a float against the prelude `number` keyword) was re-minted with real
  f32/f64 accept vectors once the fork fix landed at the `ac1b98e` pin; the stale-guard is what forces
  that removal whenever a ledgered gap closes.
- **The verify.ts oracle** — normal `verify.ts` runs replay each supported row's committed vectors
  as a default-on corroborating oracle (`--no-decode-foreign` / `VERIFY_DECODE_FOREIGN=0` opt-out),
  recording an `accepts_foreign` evidence clause in the annotations. Corroboration only — it never
  downgrades a support verdict; failures surface in the report's `decode_foreign_failures`. A
  replay that produces NO per-test verdict (a compile error, or the shifting-cell registry
  transient — the "Registry-fetch transients in nested-cargo cells" watch in
  `tests/TESTING_ROADMAP.md`) regenerates and retries once before recording FAILED, the same
  absorber the mint paths carry, so one transient cannot flip a row's committed evidence clause.

First-sweep payoff — two miscompiles invisible to every self-consistent gate, each caught here by
feeding spec-valid CBOR our code did not produce. Map-representation group-choice single-field
variants emitted **malformed CBOR** (member key dropped) that our decoder symmetrically round-tripped
while rejecting the spec-valid form; that is now **fixed**, and the fix is pinned decode-direction by
the `group.choice` row's accept vectors (a reverted key-dropping decoder mis-decodes the spec-valid
`{"a": n}` foreign bytes and fails the replay gate), with the emitted key-write/key-verify guarded
against an unreviewed re-bless by `integration_tests::corpus_group_choice_map_key_written_and_verified`.
The array-side sibling — `[* (int, tstr)]` silently narrowing the inline-group occurrence to
exactly-once, rejecting the spec-valid `[]` — is now **fixed** too: an occurrence marker on an inline
group is rejected gracefully at generation time. The projected robustness fixtures
(`tests/matrix_reject/contain.occurrence-target.grpent.inline_group.*.cddl`) pin the unsupported cells,
so they project no decode-conformance obligation (no catalog row) rather than a `class="bug"` reject.
The bare-TYPE array-field instance of the same class — `[uint, tstr, * bytes]` narrowing `* bytes` to
one mandatory item, rejecting spec-valid zero- and two-bytes instances — was this sweep's third catch
(mintable only once the fully-fixed rust oracle stopped contesting the candidates) and is rejected
gracefully the same way; `occurrence_on_array_record_field_rejects_gracefully` pins the boundaries
(`+`/bounded/any position reject; `?`, `1*1`, and single-entry homogeneous `[* t]` keep generating).

#### Composition-depth (corpus) leg

The catalog above keys its obligation set on the matrix's minimal per-construct examples — breadth.
The **composition depth** those examples lack lives in the `tests/corpus/*.cddl` fixtures, so a
sibling catalog — `tests/decode_conformance/corpus_catalog.toml` — mints spec-derived decode vectors
for them. Its obligation set is `tests/corpus/*.cddl` **× the shared rule enumerator** (every
top-level rule of every fixture), mechanically derived and never a hand-picked fixture list, so every
(fixture, rule) carries ≥1 committed vector XOR a `pinned_reason` — the same no-silent-skips rule as
the matrix catalog. The enumerator and the per-rule dependency-closure builder live in
`cddl-matrix/lib.ts`, shared by the mint and the drift gate, so the gate re-derives exactly what the
mint derived. Refresh flow: `cd cddl-matrix && bun run verify.ts --mint-decode-corpus` (`--only=`
takes row ids AND bare fixture stems — a stem expands to the fixture's rows — preserving every
unselected row byte-identically; mint-ONLY: it writes this catalog and nothing else, never
annotations or the matrix catalog).

Every active corpus row is **holder mode**: the probe spec is `__probe_holder = [0, <rule>]` plus the
rule's dependency closure (the target rule's span + every fixture rule transitively referenced from
it, in fixture order), `type_name = ProbeHolder`. Holder mode routes decode through the generated
member/field-decode path — the surface composition depth actually exercises — and covers bare-GROUP
rules too: `inner = (a: uint, b: uint)` splices into the holder array, so its vector is the wider
`83 00 …` = `[0, a, b]` rather than the single-item `82 00` (the header-mutation leg strips the same
2-byte preamble and mutates the spliced item at byte 2). The per-rule closure — rather than a
whole-fixture spec — quarantines one un-mintable rule from poisoning its fixture-mates.

Rows that can't be minted mechanically carry a `pinned_reason` instead of vectors, in stable classes:
the ruby generator's **inline-composite `.cbor`-controller parse gap** (gem 0.12.14 exit 65 —
`draft/ruby-cddl-inline-composite-control-arg-gap.md`, re-mint when the gem fix ships), **generic
rules** (a `<…>` head can't be holder-wrapped bare — instantiations are covered via referencing
rules), and **`dsl_custom`** (references user-supplied (de)serialize code — can't compile standalone).
A distinct, decoder-clean class is the **named-rule / parenthesized-choice map-KEY over-rejection** in
the rust oracle (`draft/rust-cddl-named-key-map-gap.md`): its affected table rows (`table_enum_key.*`,
`c_style_enum_map_key.enum_keyed_map`, and the adjacent-signature siblings on `composite_map_key.*`
and `wasm_nested_alias.passthru_tags_map`) keep only
their empty-instance accept vectors, because the rust reference contests every non-empty instance
while the ruby reference and our own decoder accept — an oracle-side drop, not a cddl-codegen gap.

Two gates mirror the matrix legs:
- **Drift gate** — `cddl-matrix/project_decode_conformance.ts` (check.ts `local` tier, pure file
  reads): its corpus half re-derives the glob × enumerator obligation set and asserts completeness
  (vectors XOR `pinned_reason`), staleness (each active row's committed `spec` byte-equals the
  reconstruction from the current fixture via the shared enumerator/closure builder — a drifted
  fixture reads "re-mint"), and the holder `82 00` / wider-`83 00` preamble shape.
- **Replay gate** — `integration_tests::corpus_decode_replay` (`#[ignore]`d, check.ts `full` tier —
  one of the generated `#[ignore]`d-gate roll-call in "Running everything"). It reuses
  `decode_conformance_replay`'s `decode_replay_generate` / `decode_replay_run` helpers and every leg
  verbatim (base accept, `cddl_encoding_fidelity::variants` encoding variants, `header_mutants` header
  mutation at holder offset 2, over-acceptance completeness, the `--preserve-encodings`
  byte-identity leg, and the json/wasm surface legs below), differing only in the catalog path, its
  own scratch target, its own skip-ledger instances, and vacuity floors pinned from actuals. The corpus carries only plain
  accept vectors at HEAD (the enforcement / over-acceptance axes are matrix-owned), so the
  constraint-reason and over-acceptance machinery stays armed but idle (the over-acceptance
  completeness `assert_eq` holds at 0 == 0); `PRESERVE_SKIP` holds only the native-float row
  `homogeneous_array.floats` (`[* float64]`, the `preserve_encodings_supports_floats` gap), the
  json/wasm surface ledgers hold this gate's corpus residents (listed in § "json/wasm surface
  legs"), and every other ledger is empty and stale-guarded.

#### json/wasm surface legs

The two replay gates above pin the RUST CBOR decoder. Two OTHER decode entry points ship with the
generated crate: the `--json-serde-derives` json surface
(`serde_json::from_str` over the serde-derived rust types) and the `--wasm` wrapper surface (the thin
`#[wasm_bindgen]` `from_cbor_bytes` / `from_json` delegators in `create_base_wasm_struct`). A json/wasm
boundary that is over-strict about spec-valid input the rust decoder already accepts would pass every
other gate, so each replay gate runs a **third generation per row** — `--wasm=true
--json-serde-derives=true`, default profile otherwise (NO `--json-schema-export`, NO preserve) — and
two accept-only legs off it (`decode_replay_json_wasm_legs` in `integration_tests.rs`, shared verbatim
by both gates). Only the PLAIN accept vectors are replayed: reject / constraint / over-acceptance /
encoding-variant / header-mutant vectors evidence nothing about these boundaries (the reject direction
is rust-decoder territory, and wasm-side is `JsError`-blocked — see below).

**No external json oracle.** CDDL has no json generation target, so the obligation is defined against
the rust CBOR decoder's accepted values: *every value the rust CBOR decoder accepts from a committed
vector must survive the boundary.*

- **json leg** (`__foreign_decode_replay_json`, appended to the third-generation RUST crate's
  `generated/mod.rs`): per accept vector, `let v = T::from_cbor_bytes(BYTES)` (Ok), `serde_json::to_string(&v)`
  (Ok — the value must be json-SERIALIZABLE), `serde_json::from_str::<T>(&s)` (Ok — the over-strictness
  assert), `assert_eq!(v2.to_cbor_bytes(), v.to_cbor_bytes())` (the `to_cbor_bytes` value-fidelity
  proxy, since generated types don't uniformly derive `PartialEq` — the same proxy the encoding-variant
  leg uses).
- **wasm leg** (`__foreign_decode_replay_wasm`, appended to the WASM crate's `generated/mod.rs`, `cargo
  test`ed on the HOST target like `tests/*/tests_wasm.rs`): per accept vector,
  `T::from_cbor_bytes(BYTES).ok().expect(marker)` (accept direction ONLY — the wasm wrapper builds
  `JsError` on rejection, which PANICS under host `cargo test`, so a wrongful rejection surfaces as the
  loud `WASM_REJECTED` panic rather than an inspectable Err), plus a **cross-crate byte differential**
  (`wv.to_cbor_bytes()` == the rust crate's re-encode of the same bytes — the wasm crate path-deps on
  `../rust`), and where `from_json` is emitted, `T::from_json(&wv.to_json())` Ok with the same
  differential.

**Skip ledgers** (per gate, both REPRODUCTION-guarded like `WASM_SURFACE_SKIP`'s compile check — a
skip row's leg still RUNS, and the entry is consumed only if the leg still fails; a run where every
emitted test passes fails the gate as a stale pin): a row skipped on one leg still runs the other.
`JSON_SURFACE_SKIP` — rows whose json boundary legitimately can't round-trip; each resident cites a
`cddl-matrix/ROADMAP.md` finding, and it also suppresses the wasm `from_json` sub-leg (same serde
path). Resident classes at HEAD (ledgered in § findings):
**`@custom_json`** omitting the serde derives the leg's serde_json usage needs (`dsl.custom_json` /
`dsl_custom.custom_newtype` — can't compile standalone); **non-string map keys** serde_json can't
serialize (`bytes_map_key.*`, `composite_map_key.holder`, corpus); and the **present-null optional
field** whose json round-trip preserves the null the direct CBOR re-encode drops
(`nullable_nested.nullable_optional_field`, corpus). `WASM_SURFACE_SKIP` — rows whose `--wasm`
generation or wasm-crate compile legitimately fails; also cited; sole resident class at HEAD is the
same `@custom_json` gap (the wrapper's `to_json`/`from_json` require the omitted derives). Distinct
from a **mechanical** skip: a type with NO `from_cbor_bytes` wasm wrapper surface (a bare primitive
alias, or a wrapper without the deserialize method — `deserialize_generated` gating) is classified
MECHANICALLY (`wasm_impl_has_fn` scans the generated wasm source for that type's inherent impl),
never hand-listed — a hand list of that class would rot. Loudly-logged, and paired with a "rows DO
exercise the wasm leg" vacuity floor.

**Vacuity floors** (pinned from actuals with ~10% headroom): a json-round-trip assert count floor and a
wasm-accept assert count floor per gate, plus per-crate emitted-test completeness (the run helper
returns `None` — treated as a compile finding — if the emitted test count doesn't match the expected
per-row accept-vector count). **Failure attribution**: grep-stable markers
(`JSON_SERIALIZE_FAILED` / `JSON_REJECTED` / `JSON_VALUE_MISMATCH`; `WASM_REJECTED` /
`WASM_VALUE_MISMATCH` / `WASM_JSON_REJECTED` / `WASM_JSON_VALUE_MISMATCH`) + `classify_json_failure` /
`classify_wasm_failure`, the same trailing-':' prefix-collision grammar as the base-leg classifiers
(pinned unit-side by `classify_json_failure_disambiguates_prefix_colliding_names` and its wasm sibling).

**Out of scope:** the `--wasm-cbor-json-api-macro` escape hatch (it replaces the wrapper surface with a
user-supplied macro; flag-gated, unexercised by these catalogs). The wasm reject direction (the
`JsError`-panic class). And json laxness (serde derives don't re-enforce CDDL bounds — an
enforcement-axis question for a future item, not this accept-direction leg).

### JSON-schema → TypeScript JS-side pipeline (`js_schema_to_ts`, `js_d_ts_merge`, `package_json_pipeline`)

`--json-schema-export` ships a JS toolchain that turns the exported schemas into TypeScript and
merges them into the wasm-pack `.d.ts` (`static/run-json2ts.js` + `static/json-ts-types.js`, wired by
the `--package-json` `package.json`). Three tests cover it, cheapest-in-isolation first:

- **`js_schema_to_ts`** runs the shipped `run-json2ts.js` over committed schema fixtures
  (`tests/json2ts/schemas`) using the pinned `json-schema-to-typescript`, asserting the emitted
  `.d.ts` (JSON-suffixed identifiers, resolved cross-refs, enum → union, the `additionalProperties`
  guard on both a struct and a map type).
- **`js_d_ts_merge`** runs `json-ts-types.js` in isolation over hand-written fixtures — no
  wasm-pack/json2ts needed — asserting it specializes `to_json_value(): any` to the class's JSON
  interface and appends the interface defs.
- **`package_json_pipeline`** is the end-to-end gate: it generates a small extern-free fixture
  (`tests/package-json/input.cddl`) with `--wasm --package-json --json-serde-derives
  --json-schema-export` and runs the SHIPPED `npm run rust:build-nodejs` script VERBATIM — `wasm-pack
  build --target=nodejs` → json-gen `cargo +stable run` → `run-json2ts.js` → `json-ts-types.js` →
  `wasm-pack pack`. Running the script line itself (its `cd`/`;` shell shape, its dependency pins, its
  `cargo +stable`) is the point; replicating the steps in Rust would let the script rot. This is the
  ONLY layer that exercises `#[wasm_bindgen]` macro-expansion → a real wasm-pack `.d.ts` → the JS-side
  merge end-to-end — the systematic wasm gates `cargo check` on the host target and can't see any of
  it. Asserts pin each stage: the layout copy block, a wasm-pack `.d.ts`, a nonempty json-gen
  `schemas/`, `to_json_value(): FooJSON;` + `export interface FooJSON` in the merged `.d.ts` (proving
  the merge ran on real output, not a fixture), and a `.tgz` from `wasm-pack pack`. It builds the
  generated crate with the user's `+stable` toolchain (faithful to the shipped consumer experience),
  so a `+stable` failure here is a real finding about shipped output, not a test bug. Needs
  node+npm+wasm-pack + a rustup `stable` toolchain; skips locally when absent (asserts their presence
  under CI, though CI's fast tier never reaches it). Plain `#[test]`, so it runs in the `local` tier
  like `wasm_json_roundtrip` (~20s warm).

## Generated-test harness (`--emit-tests`, `src/emit_tests.rs`)

The generator can emit a `#[cfg(test)] mod cddl_generated_tests` into the generated rust crate:
per-type **round-trip** tests (IR-derived cases — baseline, bound boundaries, one per choice
variant, each optional field present — asserted byte-identical through the full wire cycle, and
— outside preserve-encodings, where wire-populated encoding fields legitimately differ — the
deserialized value asserted `Debug`-equal to the minted original: byte-identity alone is a fixed
point for an information-losing projection serializer, so it can't see that miscompile class) and
**bounded-reject** tests. Values are minted deterministically from each type's IR (no
proptest/`Arbitrary` deps in generated crates — the repo's determinism ethos would force a fixed
seed anyway, and a fixed-seed sampler is a deterministic enumerator with extra machinery; both
designs share the same per-IR-shape derivation surface in `emit_tests.rs`, which is the single
maintained thing); unmintable shapes are skipped with a logged notice. Two consumers run it in CI: `integration_tests::emit_tests_execute` (the rich
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
A fixture that deliberately reaches a tracked unimplemented path under ONE profile is ledgered
per-profile in the gate's `EXPECTED_GENERATION_FAIL` (`(stem, profile, reason)` — e.g.
`optional_fixed_float`/preserve, which aborts at the native-float preserve stub), stale-guarded
both directions: a listed cell that starts generating fails as "gap closed — remove the pin", an
unlisted generation failure fails normally. The same cells are mirrored where the other corpus
walkers would trip over them: the snapshot suite's `PROFILE_GENERATION_SKIP` (`snapshot_tests.rs` —
no snapshot exists for a profile that never generates) and
`feature_corpus_roundtrips_nondefault_profiles`' `SKIP`, each with its own stale guard.

Because these crates are purely generated (no hand-appended scaffolding), the gate also doubles as
the rustc-warning detector for the usage-derived import prune (`import_prune`): after each nested
cargo invocation it scans stderr (`unused_generated_import_lines`) and fails on ANY `unused import`
warning in the generated crates — collection/encoding idents, `super::*`/`error::*` globs,
cross-scope type imports, and wasm macro/prelude imports — minus a documented trait residue
(`UNUSED_IMPORT_TRAIT_RESIDUE`, the `cbor_event::se::Serialize` trait the name-scan model can't
prove unused). It also fails on ANY `unused variable` warning (`unused_generated_variable_lines`):
a named binding rustc reports unused in a purely-generated crate is generator imprecision (a
count-match arm that should bind `_`), with no trait-residue analogue. This catches a
warning-severity under-prune (or unused-binding emission) the compile-error gates (E0412/E0433,
over-prune only) cannot see. The scan is versioned into the gate-cache key via a
`lint=unused-imports-v3` marker so a change to its verdict re-runs every cached cell.

Generated output lands in `tests/<dir>/export*/` — disposable, gitignored, and safe to
`git clean -fdx tests` if the ~GBs of build artifacts pile up locally. CI starts clean each run.

### Encoding-fidelity oracle (`--emit-tests` × `--preserve-encodings`)

The round-trip harness above feeds `from_cbor_bytes` only the generator's *own* canonical output, so
the "decode an irregular encoding and preserve it" direction — the whole point of
`--preserve-encodings` — went untested at scale (only the hand-picked `tests/golden_hex_preserve/`
KATs covered it). When both flags are set, each round-trip case now also runs an **encoding-fidelity**
block: a self-contained, deterministic CBOR mutator (`static/emit_tests_encoding_fidelity.rs`, spliced
into the emitted test module via `include_str!`) derives seven whole-tree irregular re-encodings of the
minted value's canonical bytes — `widen_step`/`widen_max` (non-minimal header widths), `widen_float`
(a major-type-7 float head re-encoded one IEEE width up, f16→f32→f64 — reachable since the `any`
(`AnyCbor`) mint deliberately includes a float head), `indef_containers`, `chunk_strings`,
`reverse_maps`, and `everything` (all composed) — and asserts each
decodes and re-encodes byte-identically. Whole-tree (not per-position) because a single dropped
encoding-capture fails the whole variant anyway; identity variants are skipped so the loop never
asserts vacuously. With `--canonical-form` also set it adds the canonical **differential** (every
encoding canonicalizes to the same bytes) plus a per-case canonical fixed point — the KATs stay the
spec anchor for *what* the canonical bytes are; this layer buys breadth. Types with user-supplied
`@custom_serialize`/`@custom_deserialize` are excluded (their wire format isn't the generated
serializer's). The emitted mutator ships a `#[test] encoding_mutator_self_check` pinning each mutation
class against hand-derived RFC 8949 bytes *and* pinning `variants()` end-to-end on two inputs — a
composite (int + string + map) and a float-carrying `[5, 1.5]`, the shape the `any` mint produces
(the vacuity guard). Executions: `emit_tests_execute` (local, with a fidelity-assertion floor),
`emit_tests_any_float_execute` (local — generates `tests/any-positions` under
`--preserve-encodings --emit-tests` and runs the crate, proving the `any` mint feeds a real float
head through `widen_float`), and
`feature_corpus_roundtrips_nondefault_profiles` (full tier, corpus × preserve breadth); the canonical
differential runs once at whole-program scale via the `canonical` fixture's `--emit-tests`.

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
   pinned to the minted variant. One arm class reads back via `kind()` only: a nullable-payload arm
   (`opt = uint / null` as an arm) gets no `as_<var>()` self-readback, because that getter flattens
   `Option<Option<T>>` and reads `None` for the minted inner-null — the assertion would be
   unsatisfiable, not informative (read protocol: `docs/docs/wasm_differences.mdx` § nullable values;
   skip site: `emit_tests_wasm.rs`'s `nullable_payload`). Read on the freshly-*built* wasm value, not
   the post-wire one, so a wire-ambiguous choice (core's uint-`0` vs a fixed `i0` variant) can't
   false-fail.
4. **Boundary acceptance only** (`wasm_bounds_<type>`) — the accepted boundary value constructs
   (`.ok().is_some()`). The beyond-boundary REJECT direction is deliberately **not** emitted: a wasm
   ctor's error path builds a `JsError` through a wasm-bindgen import that panics under host `cargo
   test`; rejection is already pinned as `RangeCheck` on the wire by the rust `--emit-tests` module, so
   this half only confirms the acceptance plumbing.

wasm-API idioms baked in: `JsError: !Debug`, so a wasm `Result` is unwrapped `.ok().expect(..)`, never
`.unwrap()`; composite ctor params cross as `&Wrapper`; c-style enums cross by value; every
`@newtype`/tag/bounded wrapper exposes a wasm `new(inner)` ctor (`Result`-returning when the bound makes
it fallible) plus an inner-value getter (`get`, or the `@newtype <name>` rename), so a wrapper ENTRY type
is built through that public `new` — its minted inner rendered by the same ctor-arg machinery and (for a
primitive inner) read back through the getter against the minted literal. A wrapper CTOR ARG is instead
built via its `From<cddl_lib::Native>` impl (a convenience — the wrapper's own `new` is covered by its
top-level entry test); if the inner is unmintable (extern/raw-bytes) the entry type falls back to
decoding the rust twin's bytes with a loud skip of the ctor differential. A wrapper COLLECTION arg
(`FooList`/`FooMap`/`&Nums`) is a `new`/`add`/`insert` block expression. **Loud skips (never silent):**
extern / raw-bytes ctor args (and the same-class wrapper-entry ctor differential), and the whole module under any
`--wasm-*-macro` flag (those replace the wrapper method surface) — each an `eprintln!`. (Optional-nullable
flatten points need no skip: optional fields are not ctor args, so no mint constructs a present-null
state — the three-state surface is covered by the hand-written `tests/nullable-wasm/` fixture.) Mutation-verified
red-first (three `generation/` wasm-boundary mutations each turned exactly the intended assertion class
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
  wasm-ABI matrix compile gate, swept across `ALL_PROFILES` (default / preserve / json); see that
  section below).

Run the manual gate with:

```sh
cargo test --bin cddl-codegen wasm_matrix_roundtrips -- --ignored   # ~8-10 min (258 cells x 3 profiles)
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
not exhaustive per-type depth. One exception to the degenerate baseline: for a CBOR tag whose RFC 8949
content the validator *semantically* enforces (tag 0 = tdate must be an RFC 3339 date-time), the minter
emits a fixed valid literal instead of the generic `"a"` — otherwise a spec-violating baseline would
round-trip byte-identically yet be (correctly) rejected by this oracle. Only tags the validator actually
enforces get a constant (`semantic_tag_content` in `emit_tests.rs`); every other tag mints the baseline.

**The gate** (`integration_tests::ir_conformance_corpus`, `#[ignore]`d — **manual/local only**, kept
out of even the local tier's `cargo test` because it adds the heavy `cddl` dep to every corpus crate):

```sh
cargo test --bin cddl-codegen ir_conformance_corpus -- --ignored --nocapture   # ~1 min
cargo test --bin cddl-codegen rust_oracle_fingerprint -- --ignored --nocapture # preflight only
```

Before the corpus loop, the gate generates a tiny `fingerprint_probe` crate under the same scratch root,
injects `CDDL_ORACLE_DEP`, and executes every shared fingerprint probe through the exact parser and
validator entrypoints the conformance oracle trusts. A mismatch panics with the failing probe names,
the same recovery guidance as the matrix verifier. Then, for every
`tests/corpus/*.cddl`, it generates with `--emit-tests --emit-tests-conformance`, appends
`CDDL_ORACLE_DEP` + the shared oracle helpers, copies the fixture in as
`cddl_conformance_source.cddl`, and `cargo test`s the crate under one shared `CARGO_TARGET_DIR` (so
`cddl` compiles once). The scratch root is keyed by checkout path and wiped at start, so the gate holds
an advisory lock (`acquire_scratch_lock`) for its whole run: a second invocation from the same checkout
waits for the first (printing a grep-stable "waiting for it to finish" message) rather than
`remove_dir_all`ing its crates mid-run — same-checkout concurrent runs serialize while the shared target
cache is preserved. Two curated lists, each empirically justified:

- **`EXPECTED_FAIL`** — fixtures with a known IR bug whose minted value the oracle *must* reject. Their
  `cargo test` must fail **and** the output must carry the oracle's distinctive message (so it failed
  for the right reason). An expected-fail fixture that *passes* turns the gate RED ("IR bug apparently
  fixed or oracle lost teeth — investigate, then remove from `EXPECTED_FAIL`"). Empty whenever no
  corpus fixture mints a spec-violating value; the machinery stays armed — the next IR-level bug's
  fixture will trip this list. Its last resident, kept here as the illustrative case, was
  `exclusive_range` (`[v: 0...10]`): the validator rejected the minted `11` as out of range
  `0 <= value < 10`, and it was removed once `parsing.rs` was corrected to emit `max = b-1` and the
  minted value became in-spec.
  `inline_group` (`[(uint, tstr)]`) and `occurrence` (`[+ uint]` / `[2*5 uint]`) are earlier
  residents' siblings that never joined the list: both are **fixed at HEAD** (inline_group emits a
  2-field struct that reads 2 elems; occurrence bounds now live on the ARRAY type — enforced as a
  length check at embed sites and covered by `occurrence_holder`'s minted round-trip + deser-reject
  cases, where they were once misread as element VALUE bounds).
- **`CONFORMANCE_SKIP`** — fixtures excluded from the sweep for a concrete *validator/minter* gap
  (never to hide a real bug): `dsl_custom` (references user-supplied code, can't compile
  standalone). `sized_int` is a past resident, off the list twice over: its negative-lower-bound
  range stopped being a validator gap at the fork's `885c61c` non-uint-range fix, and its
  `int .size 8` member was dropped when cddl-codegen made `.size`-on-signed-`int` a graceful
  rejection (per the RFC author's clarified semantics — cbor-wg/cddl#32 — the construct means the
  `uint .size` window, which the old `i{8N}` mapping mis-enforced; the rust validator's hard error
  on it remains an upstream over-rejection gap, scoreboard in
  `draft/cddl-size-on-int-divergence.md`).

Any fixture **not** on either list that fails conformance turns the gate RED with the minted bytes +
rule named. A vacuity floor asserts a nonzero number of fixtures actually emitted a conformance call,
so a silent no-op sweep can't pass.

**Which rules get rooted at all.** Both oracle halves share one seam —
`emit_tests.rs::conformance_rule_name` — and it roots every top-level rule EXCEPT a bare-GROUP rule
(`inner = (a: uint, b: uint)`): a group is a reusable fragment, not a validatable instance type, so
both oracles reject any bytes offered against it *by design* (the ruby gem always did; the rust
fork's validator since its `773b723` array-sequence rewrite). Such a rule gets NO conformance call
and NO minted-bytes dump — a design exclusion, not a ledger entry, which is why its absence never
shows up in `DUMP_EXEMPT`. Its *embedders* stay fully judged: `tests/corpus/nested_group.cddl`
pins the shape (`inner` unrooted, its array-splicing sibling `outer` rooted by both halves).

**Decorrelated (ruby `cddl` gem) second oracle.** The rust oracle above shares the dcSpark fork's
parser with the generator, so a **fork misparse** (which corrupts generator IR and the oracle's spec
reading identically) mints well-formed-but-spec-wrong bytes and passes green. To catch that class this
gate re-validates the *same* minted bytes through the ruby `cddl` gem — the RFC author's reference
tool, sharing no parser, decoder, language, or lineage with the fork. The bridge is a dump hook: with
`--emit-tests`, when `CDDL_CODEGEN_DUMP_MINTED` names a directory each round-trip case writes its bytes
to `<rule>__case<i>.cbor` (pure `std`, inert when the var is unset, no CLI flag — see
`emit_tests.rs::roundtrip_body`); the gate points it at a per-fixture dir, then sweeps in sorted order,
invoking `<gem> <synthetic-rooted-spec> validate <case.cbor>` (the gem targets a spec's first rule, so
the same `__cddl_oracle_root = <rule>` trick aims it). The gem is **harness-side only** — never a crate
dep, so shipped output stays ruby-free. Teeth and posture:

- **`RUBY_EXPECTED_FAIL`** — `(fixture, rule, reason)` triples the gem diverges on for a documented,
  non-bug reason (a gem construct gap the fork legitimately supports). Two at HEAD —
  `(cbor_wrapped_group_array, holder)` and `(cbor_bignint_table, holder)`, both the gem's
  inline-composite `.cbor`-controller parse gap (exit 65 poisons the whole spec;
  `draft/ruby-cddl-inline-composite-control-arg-gap.md`). Ledgering is **per (fixture, rule)**, not per
  fixture: a fixture may have one rule the gem can't judge while its *other* rules must still be
  sound. A divergence is *signal*: an unledgered one is either a gem gap to record here **with a
  reason**, or — the class this oracle exists to catch — a fork misparse minting spec-violating bytes.
  **Investigate before ledgering.** A ledgered `(fixture, rule)` that stops diverging while still
  being swept turns the gate RED (stale entry), mirroring `EXPECTED_FAIL`.
- **`GEN_SKIP` vs `RUST_ORACLE_SKIP`** — two distinct exclusions. `GEN_SKIP` (e.g. `dsl_custom`) can't
  be generated standalone at all, so it's skipped entirely. `RUST_ORACLE_SKIP` holds fixtures with a
  *rust*-validator gap that still generate, round-trip, and dump fine: they are generated **without**
  `--emit-tests-conformance` (rust validate half off) while their minted bytes are **still** swept by
  the ruby gem — a rust-validator blind spot must not cost the decorrelated oracle its coverage.
  Empty at HEAD: the sole past resident, `cbor_bignint_table`, came off the list when the fork's
  bignum map-key fix shipped (`cddl-matrix/README.md` § "Upstream oracle gaps" gap #6; its ruby
  half is separately on `RUBY_EXPECTED_FAIL` above, so it keeps the decode-side reference-codec
  differential AND the rust conformance half as its checks).
- **Dump-coverage (`DUMP_EXEMPT`)** — per fixture, every rule the generator *intended* to dump (its
  hook is present in `lib.rs`) must land a `.cbor` on disk. An intended-but-undumped rule fails the
  gate unless ledgered in `DUMP_EXEMPT` **with a justification** — so a dump hook that silently stops
  firing (or a lossy rule name dropping a top-level rule from the sweep) is visible per fixture, not
  only via the corpus-wide case floor. Empty at HEAD: source rule names are always recoverable.
- **Negative control** — after the sweep, one known-good case is truncated (final byte dropped =
  guaranteed malformed) and the gem *must* reject it; a gem invocation that exits 0 regardless of input
  can never pass the gate.
- **Case floor** — a minimum total swept-case count, so a dump hook that silently stops firing fails
  rather than shrinking to a vacuous no-op.
- **Gem REQUIRED (opt-out `CDDL_RUBY_ORACLE=skip`)** — gem discovery mirrors `verify.ts`'s
  `resolveRubyCddl` (`RUBY_CDDL` env pin, fail-loud if the pin is bad; else the gem-dir probe — never
  `$PATH`/`which cddl`, which is the unrelated *rust* `cddl`). The decorrelated oracle must not
  silently, permanently degrade to a no-op just because a machine lacks the gem, so **a missing gem
  FAILS this gate** with install instructions (`gem install --user-install cddl`). To run the gate
  without the decorrelated half — accepting the fork-misparse class goes uncovered — set
  `CDDL_RUBY_ORACLE=skip`, which prints the grep-stable `RUBY ORACLE: SKIPPED (...)` marker and runs
  only the rust + dump-coverage halves.

**Decode-side reference-codec differential (CDDL-blind, dependency-free).** Both cddl oracles above
prove our bytes match the *spec*; neither is a raw structural decode. Piggybacking on the same dumped
`.cbor` files, this gate also decodes every minted case through **two independent CBOR codecs**
(`ciborium` and `minicbor`, harness-side dev-deps — `minicbor` is used nowhere else in the pipeline,
which is its decorrelation value) and requires both to fully consume the bytes (no trailing garbage)
and agree on the decoded structure. What it proves: two decorrelated decoders structurally agree on
our output — a well-formedness regression a spec validator wouldn't see (a validator can accept bytes
a raw decoder chokes on, or vice-versa). What it can't: nothing about spec conformance — that's the two
cddl oracles' job. It has no external dependency, so it runs for `RUST_ORACLE_SKIP` fixtures and even
under `CDDL_RUBY_ORACLE=skip`, with its own case floor (`DIFF_CASE_FLOOR`) and a truncation negative
control (a malformed case must fail both codecs). The one place the codecs legitimately model the same
bytes differently — RFC 8949 §3.4.3 bignum tags 2/3, which `ciborium` folds into integers and
`minicbor` leaves as `Tag(2/3, Bytes)` (our `biguint`/`bignint` prelude types) — is canonicalized by
`fold_bignums` before comparison, so only a genuine structural divergence turns the gate red.

## Static-runtime property layer (`src/tests/any_cbor_tests.rs`)

The `AnyCbor` self-describing CBOR value type (`static/any_cbor_preserve.rs` + its two
per-assembly serialize fragments, `static/any_cbor_non_preserve.rs`) is hand-written runtime
code whose contract — byte-identical re-serialization of ANY well-formed CBOR item under
`--preserve-encodings` — no snapshot can judge, so it gets a dedicated property layer instead
of fixture coverage. `src/tests/any_cbor_tests.rs` `include!`s the static files into one shim
module per static assembly (non-preserve / preserve / preserve+force-canonical — the same
technique the fidelity mutator uses via `include!` in `integration_tests.rs`) and runs under
plain `cargo test` (`cargo test --bin cddl-codegen any_cbor`; in-tier via the local tier's
workspace `cargo test`, no nested cargo, no dedicated gate). Two further shims
(`json_non_preserve` / `json_preserve`) additionally compile the `static/any_cbor_json.rs`
serde fragment and pin the JSON round-trip laws — the exact rendering table
(`docs/docs/output_format.mdx` § AnyCbor JSON), `from_json(to_json(x)) == x` for the
non-preserve variant over finite floats, value-equal-modulo-encodings for preserve, and the
read-side tolerance/error cases.

The core assertion is a **span oracle**: deserialize one item, recover its true byte extent
from `Deserializer::position()` diffs, and require `serialize(deserialize(span)) == span`
byte-identically (preserve variant) / value-fixed-point (non-preserve variant), plus
canonical-serialization fixed points and equal-value-different-encoding → identical canonical
bytes. Corpus: the RFC 8949 appendix-A vectors (`cddl-matrix/sources/appendix_a.json`; one
principled skip — `f818`, `simple(24)`, is ill-formed per RFC 8949 §3.3), hand vectors per
`Sz` width, NaN payloads at every float width (the fork-supplied `float_sz` fidelity), mixed
chunk-width indefinite strings, duplicate map keys (identical AND differently-encoded),
malformed/truncated prefixes (must `Err`, never panic), and seeded-PRNG random structural
values for both variants — the seed prints on failure, so a red run reproduces by pasting the
seed.

Since A2, generated crates whose spec uses `any` also compile these files — the usage-gated
`any_cbor` module assembled in `export.rs` (`--export-static-crate` exports it always, as a
pure function of flags) — so this layer is no longer their only coverage; it remains the only
layer that judges the byte-exactness contract itself. The depth guard is wired through the
single `read` recursion seam via an includer-supplied `any_cbor_recursion_guard!()` macro:
the three per-assembly shims here define it as a no-op, and a fourth `depth_guard` shim
includes the guard runtime with a small baked limit and pins the at/under/over-limit vectors
(over errors `DeserializeFailure::DepthLimitExceeded`, no SIGABRT), with an e2e counterpart
(`integration_tests::deserialize_depth_limit_guards_any_member`) proving a generated
`--deserialize-depth-limit` crate rejects a pathologically deep value in an `any` position.

## wasm-ABI matrix (`tests/matrix_wasm/` + `integration_tests::wasm_matrix_compiles`)

A **coverage-by-construction** gate for the generated wasm-bindgen bindings: it compiles the wasm crate
for every cell of a `{wasm-ABI type-shape} × {boundary role}` grid, so any cell whose bindings don't
type-check is a specific red cell. It exists because the wasm ABI — accessor return types, boundary
`.into()`/`.clone()`/by-ref conversions, map typedefs — is a concern the CBOR-serialization suites don't
compile-check by construction: the rust crate can type-check while the generated wasm crate does not, so
without an enumerated gate that class of bug is only caught by whichever fixtures happen to hit it. Gating
the whole grid makes the coverage systematic instead of incidental.

Coverage equals **both** hand-curated axes — the `SHAPES` type-shape list and the `ROLES` boundary-role
list: a wasm representation not in `SHAPES`, or a boundary position not in `ROLES`, is not gated. Treat
each as a living list — when a type reaches the wasm boundary in a representation no existing shape
captures, add a shape; when the emitter places types in a boundary position no existing role captures,
add a role (see "Adding / changing cells") — and periodically ask "which representation, and which
boundary position, are we *not* enumerating?", because a missing shape *or* role is a silent hole, not a
red cell (the E0599 bounded-wrapper-arm bug lived in the un-enumerated `tchoice-variant` role while its
`bwrap` shape was gated all along).

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
    (array/map wrapper structs), `necoll`/`necollrec`/`nemap` (restricted non-empty wrappers over
    `NonEmptyVec`/`NonEmptyMap` — the failable `try_from` door beside infallible
    `new(first)`+`add`/`insert`; `necoll` takes a bare `Vec` by value, `necollrec`/`nemap` borrow +
    clone their loose builder wrapper), `passthru`/`passthrumap` (transparent `pub type`s), `ralias` (transparent
    alias to a Record struct), `struct`, `mstruct`
    (map-representation Record struct — bareword-keyed map), `cborwrap`/`cborwrap2`, `tag` (a CBOR-tag
    wrapper struct — crosses via a wasm `new(inner)` ctor and an inner-value `get()` accessor, plus
    `From<cddl_lib::Tg>` / cbor bytes), `bwrap` (a bounded/range wrapper struct — the only
    `Result`-returning wasm `new`: `new(inner)` enforces the `.size` bound, alongside `get()`),
    `cenum` (Copy c-style enum), `denum` (data-carrying type-choice enum),
    `nullable` (`Option<T>`), `generic` (a monomorphized RECORD-generic instance),
    `gcolla`/`gcollexp`/`gcolln`/`gtbla` (anonymous generic-COLLECTION/TABLE-instance lowerings —
    wrapper-needing element → structural class, exposable element → bare `Vec`, plus the
    named-instance-rule own-name control), `rset`/`nerset`/`rseta`/`nerseta` (`@duplicates reject`
    uniqueness twins over `OrderedSet`/`NonEmptyOrderedSet` — the FALLIBLE `add` door plus the
    std-set `insert -> bool`/`contains` doors, named-rule and anonymous-instance flavors; a reject
    set NOMINAL class delegates this same surface flat — `len`/`get(index)`/`add`/`insert`/
    `contains`/`try_from`/`try_opt_from` — instead of the two-layer `get() -> companion` shape,
    which only `@duplicates preserve` nominals keep), `pmap`/`nepmap`/`pmapa`/`nepmapa` (`@duplicates preserve` pair-map
    twins over `PairMap`/`NonEmptyPairMap` — the APPENDING `insert` and the `{+}` borrow-clone
    `try_from` door, same two flavors), `chain`, `extern`, `rawbytes` (a user-supplied
    `RawBytesEncoding` type). This is the
    `is_copy × directly_wasm_exposable × has-a-wrapper-RustStruct` axis the CBOR feature matrix
    deliberately does *not* individuate (wrapper-vs-transparent is a struct-table fact, not a shape fact
    — see the docstrings in `src/intermediate/`).
  - **Role**: where the type sits — `array-element`, `map-value`, `map-key`, `struct-field`,
    `struct-field-opt`, `newtype-inner`, `tchoice-variant` (the shape placed as one arm of a
    type-choice enum — the per-variant wasm ctor emission path via
    `generate_type_choices_from_variants`), `gchoice-variant` (the shape placed as one named-field arm
    of a `//` GROUP choice, `[ f0: T // f1: nint ]` — the group-choice sibling of `tchoice-variant`,
    minting one `new_<field>` wasm ctor per arm through the DISTINCT `codegen_group_choices` emitter
    path; array representation only, since the map-rep spelling emits byte-identical wasm). Each drives distinct accessor emission
    (`get`/`add`/`insert`/`keys`/`new_<arm>`, by-value vs by-ref). Struct roles use the **array representation** (`[field0: T]`,
    `[pre: uint, ? field0: T]`); the map representation is covered on the shape axis instead by the
    `mstruct` representative cell. Map-rep field holders (a bareword-keyed map with a mandatory or
    `?`-optional field) are deliberately not enumerated as separate roles because their wasm emission is
    byte-identical to these array-rep roles (the representation only changes rust-side serialization). A
    shape may likewise skip a role
    that would only pin a permanent red — `nullable` skips `map-key`: a nullable key is degenerate
    CDDL and its wasm bindings don't compile (`Option<u64>` fails `ErasableGeneric`), see the
    prune comment in the projection.
- **The gate** (`integration_tests::wasm_matrix_compiles`) globs the fixtures, generates each
  `--wasm=true`, and `cargo check`s the wasm crate. The wasm crate path-depends on the rust crate, so
  rust-side type errors surface here too — which means some skip-listed reds are rust-crate generation
  bugs rather than wasm-boundary ones. A `rawbytes__*` cell resolves `_CDDL_CODEGEN_RAW_BYTES_TYPE_` to a
  user-supplied type (`PubKey`), so before `cargo check` the gate splices the in-repo defs
  (`tests/external_{rust,wasm}_raw_bytes_def`) into the generated rust + wasm crates via
  `append_raw_bytes_defs` — mirroring `run_test`'s external-file append. That's why `rawbytes` compiles for
  real instead of being skipped like `extern` (whose defs live only in `tests/extern-deps`); it costs no
  extra cargo invocation (same per-cell generate + check). It follows `feature_corpus_compiles`' shared-target-dir *pattern*
  but uses its **own** scratch + `CARGO_TARGET_DIR` (`cddl_codegen_wasm_matrix`), separate so the two
  tests don't collide when `cargo test` runs them in parallel. The verdict is **compile**: a cell can
  compile green while emitting *semantically* wrong bindings (e.g. an identity `.into()` where a transform
  was needed). Catching those is the job of the **round-trip** upgrade — `integration_tests::wasm_matrix_roundtrips`
  (`#[ignore]`d, manual): same cell enumeration, but each cell is generated `--emit-tests=true` and
  `cargo test`ed so the emitted `cddl_generated_wasm_tests` module (see § "wasm-crate test module" above)
  RUNS its cross-crate byte differential + accessor read-back. It sweeps every cell across
  `ALL_PROFILES` (default / preserve / json — `--preserve-encodings` and the json flags substantially
  change codegen, so the wasm behavioural verdict must hold under each); the compile floor above stays
  **default-profile only** by cost policy (non-default compile coverage is subsumed by this gate's
  `cargo test` at full tier). It has its own scratch dir (`cddl_codegen_wasm_matrix_rt`) with one
  shared `CARGO_TARGET_DIR` across all profiles/cells and frees each per-cell output dir after its
  verdict. It uses the module-level `WASM_MATRIX_SKIP` (red in every profile) plus a
  `WASM_MATRIX_PROFILE_SKIP` (this gate only — `(profile, cell, reason)`; expected empty at HEAD, as
  every cell round-trips green across all three profiles), each with
  the four-state resurfaced-guard verdict. Every skip/pin ledger validates its keys up front against
  its gate's swept universe, so dead fixture/cell/profile pins fail before heavy work (when adding a
  guard, verify it the way these were: temporarily poison a key and watch the gate fail fast, then
  revert). Run it with
  `cargo test --bin cddl-codegen wasm_matrix_roundtrips --
  --ignored` (~7 min warm); a cell whose shape mints no wasm surface (loud emitter skip) passes with
  zero emitted tests, which is a legitimate green (the compile gate already pins its ABI compiles).

**Wrapper-vs-transparent — route through one predicate.** The recurring wasm-boundary bug source was
naming, boundary conversion, and exposability each *separately* deciding whether an ident is exposed as a
`#[wasm_bindgen]` wrapper struct or a transparent `pub type` — a *struct-table* property, not a
`ConceptualRustType` shape (a named collection `nums = [* uint]` is a wrapper; a passthrough `arr2 = arr`
is transparent — same IR shape). The single source of truth is `IntermediateTypes::has_wasm_wrapper(ident)`;
new decision sites should consult it instead of re-deriving. Gotcha it encodes: an exposable named array
has a wrapper struct *and* is used transparently as `Vec<T>`, so a passthrough-alias emission must gate on
`has_wasm_wrapper(target) && !base_type.directly_wasm_exposable()` (maps are never directly exposable;
exposable arrays are — that split is what keeps `passthrumap` pointing at the wrapper while `passthru`
stays a transparent `Vec`).

**Fixing a red cell (the TDD loop).** A red cell is a bug the matrix *wants* fixed. Known reds sit in the
gate's `WASM_MATRIX_SKIP` list, with the shared reason comment and a ledger entry in
[`cddl-matrix/ROADMAP.md`](../cddl-matrix/ROADMAP.md) (which shape/role, the exact `E####`, root cause).
At HEAD the list holds one permanent resident — `extern__array-element` (references a
user-supplied type, so the cell can't compile standalone; the construct is integration-tested in
`tests/extern-deps`), so any OTHER red appearing is a regression to fix, not a backlog item. The
round-trip gate's `WASM_MATRIX_PROFILE_SKIP` (compile-clean cells red only under some profiles) is
empty at HEAD — every cell round-trips green across all three profiles.
To close one:

1. Remove its `<shape>__<role>` entry from `WASM_MATRIX_SKIP`.
2. Fix the emitter; `cargo test wasm_matrix_compiles` until green.
3. A `WASM_MATRIX_SKIP` cell that starts compiling *fails* the gate (the "resurfaced" guard) — so you can't forget
   step 1 and the list can't rot.

A *new* red cell (red but not in `WASM_MATRIX_SKIP`) also fails the gate: fix it, or skip-list it
**deliberately** with a ROADMAP entry — never silently.

**Adding / changing cells.** Edit `SHAPES`/`ROLES` in the projection, `bun run project_wasm_matrix.ts`,
review the new fixtures, run the gate. Prune cells whose emission duplicates an existing one — the
projection already restricts redundant shapes (`chain`, `cborwrap2`, `extern`, `mstruct`) to one representative role.

> Sibling system: `tests/matrix_{supported,panic,reject}/` (projected by `cddl-matrix/project_robustness.ts`,
> driven by `src/tests/robustness_tests.rs`) is the same projection→fixtures→gate shape on a different axis —
> "does a construct *generate*?" rather than "does its wasm *compile*?". Three generation-outcome
> catalogs, one per matrix verdict class: **supported** (`all_supported_constructs_generate` — must
> generate clean), **panic** (`unsupported_construct_panic_catalog` — tracked-known generator panics),
> and **reject** (`unsupported_construct_reject_catalog` — the rows the matrix marks off-limits that mint
> no other test: parse-rejected control ops, generates-but-doesn't-compile shapes like `prelude.any`, and
> out-of-profile constructs). Containment cells (`contain.*`) are included in this projection; spec-disallowed
> cells without annotation rows are naturally absent, while supported/reject/panic cells get generated
> fixtures and subsume the older hand pins for map-key spelling/arity, group-choice-arm, and
> occurrence-target coverage. The reject catalog's payoff is catching a parser/codegen change that
> *silently* makes a rejected construct parse — the exact regression a past cddl-fork bump caused for 14
> control ops — as a snapshot diff in the default `cargo test` run instead of only on a manual verify.ts
> sweep; `project_robustness.ts --check` independently pins each reject row's expected label to its matrix
> evidence class, so a re-bless can't quietly launder such a flip.

> Sibling system: `src/tests/identifier_hazard_tests.rs` is the same catalog+gate shape on a
> **NAME-shaped** axis a construct enumeration can never catch — collisions between a user-chosen CDDL
> *name* and the Rust the generator *emits* (the axis IS the name). It sweeps a static hazard table
> (`RUST_KEYWORDS` reused from `parsing.rs`, the single-letter names `r`/`w`, and prelude/std type names
> like `Option`/`Vec`/`Int`) × six name positions (rule name in BOTH emitted type shapes — record
> struct and type-choice enum, since the historical generic collision was shape-dependent and a
> struct-only sweep would launder enum-shaped `w` as clean — bareword map key, bareword array key, plain group name,
> `@name` directive value). It is a Rust module rather than a `project_robustness.ts`
> projection **on purpose**: the hazard × position table has no matrix verdict upstream to drift from,
> so a TS layer would only copy a constant into fixtures. Two layers: `identifier_hazard_robustness_catalog`
> (default `cargo test` — the `robustness` substring in its name keeps the `cargo insta test --
> snapshot_tests robustness` orphan gate selecting it) snapshots each cell's *generation* outcome
> (`ok` / `error (graceful)` / `PANIC`, a scorecard — a committed `PANIC` is a tracked-known gap, a NEW
> one is a regression); `identifier_hazard_crates_compile` (`#[ignore]`, check.ts full tier) *compiles*
> the `ok` cells — bundling each position's non-pinned hazards into one crate to avoid ~hundreds of
> `cargo check`s, minus a pinned `EXPECTED_COMPILE_FAIL` set of known does-not-compile cells asserted
> to fail INDIVIDUALLY so a pin flips loudly when its fix lands (currently EMPTY: the shape-dependent
> `r`/`w` generic-collision pins it launched with dissolved when cbor_event 3.x de-generified the
> emitted `serialize`/`deserialize` signatures — no fn type parameters remain to shadow, pinned by
> `emitted_signatures_carry_no_reader_writer_generics`).
> A non-pinned bundle that fails to compile is a NEW hazard finding to add to the pin list (with a
> reason) and report — not to paper over by editing the generator.

> Second sibling, same argument on a **DOCS-CONTRACT** axis: `src/tests/dsl_position_tests.rs`
> hard-asserts the comment-DSL directive × attachment-position grid against
> `docs/docs/comment_dsl.mdx`'s claims (plus error-message-advertised remedies). A directive that
> silently no-ops in an unenumerated position still generates, compiles, and round-trips — invisible
> to every execution-gated probe — so each cell asserts the OBSERVABLE effect as a string-level
> check on the generated source (a renamed field, a `///` comment, a missing serde impl beside a
> positive control; cells whose directive's effect is wasm-side — `@used_as_elem`'s minted list
> wrapper — opt into `--wasm` string generation, still no static dir) or a graceful rejection. Unlike the hazard catalog it is hard-asserted, not a
> blessable snapshot (blessing a decay to silent-drop would defeat the purpose); discovered drops
> are pinned in `KNOWN_SILENT_DROP` (mirroring `EXPECTED_COMPILE_FAIL`) — asserted to STILL be
> dropped so a pin flips loudly when a fix lands, and a pin is a finding to report, not a license
> to re-author the expectation. Pins carry a vacuity hazard Effect cells don't: a pin asserts
> "expectation NOT satisfied", which a MISPLACED directive comment satisfies vacuously (the DSL's
> comma-placement rules are finicky), so a pin is authored only after hand-verifying the placement
> against the docs' comma rules — ideally beside a control cell using the same placement in a
> position where the directive works, isolating *position* as the variable.

### Synthesized-name interaction sweep + duplicate-ident backstop

The generator mints structural wasm-boundary classes whose names derive from user type names — the
loose `{Elem}List` / `Map{K}To{V}` builders, the restricted `NonEmpty*` wrappers, and the table
`keys()` list wrappers. How those names interact with USER rule names (and with each other) is a
NAME-shaped axis the shape catalogs never reach: they mint one rule per shape and never spell a
colliding user name or a named+inline coexistence, so a bug in this class ships as **generation exits
0 but the wasm crate doesn't compile**. Two standing layers own it:

- **Duplicate-ident backstop** (`generation/export.rs::top_level_type_ident` + the scan in
  `generated_files`). Before export, every generated `src/generated/**` file (all three crates) is
  scanned for line-anchored top-level type-namespace definitions (`pub struct`/`enum`/`type`); any
  ident defined twice within one file returns an `Err` at the `generated_files` seam naming the file
  and the duplicated ident(s). This observes the ACTUAL emitted source rather than an IR prediction,
  so it is the backstop for every mint path present and future — turning the silent E0428
  redefinition (a user rule colliding with a synthesized ident) into a loud, graceful generator
  error. The plain F1/F2/F5 families have no IR-level collision scan (only the `NonEmpty*` families
  do, in `intermediate/mod.rs`), so for them the backstop is the sole pinned layer;
  `loose_builder_name_claimed_plain_message_names_ident_and_file` pins its message identity and its
  robustness-catalog row pins the outcome label.
- **`synthesized_name_interaction_sweep`** (`integration_tests.rs`). A table-driven sweep crossing
  each synthesized-name FAMILY (F1 plain list, F2 table builder, F3 `NonEmpty*` list, F4 `NonEmpty*`
  map, F5 table `keys()` list) with each INTERACTION (I-a different-shape rule claims the synthesized
  name; I-b named + inline same-shape coexistence; I-c self-named rule; I-d different-shape claim of a
  needed auxiliary builder — expressible only for F3/F4). The per-cell **invariant: no cell may be
  exit-0 with a non-compiling crate.** Each cell is pinned to either a graceful `Reject(ident)` (an
  IR-scan or backstop rejection whose message names the colliding ident, asserted in-process, no
  cargo) or `Ok` — the generating cells are batched (cell-prefixed rule names, so name-local classes
  can't mask across cells) into ONE crate whose wasm binding is `cargo check`ed (the
  `feature_corpus_compiles` shared-target pattern), with per-cell `present`/`absent` assertions
  pinning the dedup semantics (dedup target defined once, the deduped-away twin never emitted).

Expectations are seeded by the **probe-then-pin** rule: run the generator on the cell's CDDL, inspect
the outcome, then pin the observed-AND-correct behavior. A cell that lands exit-0 + non-compiling is a
NEW instance of the class — fix it if the fix is small and clearly correct, otherwise pin it in a
cited, vacuity-guarded known-bad ledger and REPORT it; never bless it by loosening the row to `Ok`.
The E0425 flavor (an emitter that references a wrapper name no mint path emits) stays owned by the
compile gates (`wasm_matrix_compiles` + the full-tier recombination wasm leg), not this sweep.

### rust↔wasm API-surface parity (`wasm_parity_tests::wasm_api_parity`)

The compile gate above proves the emitted wasm bindings *type-check*; it cannot prove they *exist*. A
member emitted on the rust side of the crate boundary with no wasm counterpart is invisible to every
oracle here — snapshots pin whatever was emitted, the compile gates compile whatever was emitted, and
the wasm test mint is *written against* the surface that exists, so it exercises what's there and
can't demand what's missing. The proven instance is `4e5b837`: wrapper types shipped for years with a
rust `new`/`From` but no wasm ctor/getter — `generate_wrapper_struct` built a `wasm_new` and never
pushed it, caught only by reading the generator. `wasm_api_parity` closes that class structurally.

It parses the emitted `rust/src/generated/mod.rs` and `wasm/src/generated/mod.rs` with `syn` (a
harness-side dev-dep) and asserts a **one-directional rust→wasm** correspondence — only rust members
impose obligations, so wasm-side extras (`kind`/`as_*`/`has_*`/`set_*`/`len`/`insert`/`keys`/
`to_cbor_bytes`/…) are unchecked by design. Four rules:

1. Every rust `pub struct`/`enum` has a wasm counterpart (same-named wasm struct/enum, `pub use`
   re-export, or **public** `pub type` alias).
2. Every rust `pub type` alias has a same-named wasm public alias or wasm type — a **private** wasm
   `type` alias does *not* count (that is exactly the finding class the generator fix below closed).
3. Every rust `pub` field on `T` has a wasm getter of the same name (no setter obligation: wasm emits
   `set_*` only for optional fields). One structural exemption: a field whose type is `Option<X>`/`X`
   with `X` a pub struct defined in the emitted `cbor_encodings.rs` (the preserve profile's
   `pub encodings: Option<XEncoding>` capture fields) is rust-only round-trip metadata, not boundary
   API — no wasm getter obligation. Obligations still come from `mod.rs` only, so the `*Encoding`
   structs themselves never impose any.
4. Every rust inherent `pub fn` on `T` has a wasm inherent fn of the same name **and arity** (`self`
   excluded; return types unchecked — boundary conversions differ by construction). Rules 3–4 run
   only when a same-named wasm struct/enum is *defined*; a `pub use`/alias counterpart is full parity
   under rules 1–2 (a `pub use` *is* the same type; a rust alias has no inherent members).
5. **JS-name visibility.** wasm_bindgen exports no type aliases, so a rust type whose ONLY wasm
   counterpart is a `pub type` alias never reaches JS under its CDDL rule name. Rule 5 resolves the
   alias's target and flags iff the target is a struct/enum *defined* in the wasm mod (a real
   `#[wasm_bindgen]` class) whose name is NOT itself on the rust surface — the (since-fixed)
   usage-dependent JS-class-name class, where a named table rule's wrapper degraded to
   `pub type Mp = MapU64ToText;` pointing at the generator-invented structural class (rule 5 stays
   the live catcher for any recurrence). Carved out (not findings): a target that is
   not wasm-defined (transparent alias to a primitive/std/`Option` type — native in JS); a
   wasm-defined target that IS a rust-surface rule name (a genuine CDDL-level alias on both sides);
   and a **synthesized anonymous generic-collection/table instance alias** (`gcoll<foo>` →
   `GcollFoo`, `gcoll<uint>` → `GcollU64`, `gtbl<uint, text>` → `GtblU64Text`) — the user wrote an
   anonymous instance, not a rule, so it correctly crosses as its inline equivalent's STRUCTURAL
   class (`FooList` / bare `Vec` / `MapU64ToText`, the documented lowering) with no rule name at
   stake. Rules 2 and 5 both skip these. The discriminator is **provenance, not shape**: the
   generator emits a doc marker (`generation::SYNTHESIZED_INSTANCE_ALIAS_DOC`) on synthesized instance
   idents only, and the gate reads it from the rust item's rustdoc — a shape heuristic ("aliases a
   std collection") was rejected because a sole-owner named-table alias (`pub type Mp = MapU64ToText;`)
   is a bare-collection alias too and must STAY gated (else rule 5 goes blind to the degradation bug
   it exists to catch). The marker emission is pinned by `synthesized_instance_alias_marker_provenance`.
   `pub use` counterparts stay JS-visible by design (`#[wasm_bindgen]` c-enums re-exported).

Legitimate rust→wasm asymmetries are baked into those rules, not ledgered: the "`pub use`d Copy
enums", "rust-only trait impls" (only inherent impls are walked — `From`/`AsRef`/`Serialize`/… are
never counted), collection-API-inheritance (a transparent `pub type Nums = Vec<u64>` has no
enumerable members), and tag-over-struct-folding classes all fall out structurally. What it does **not**
check: *semantic* wrongness — an identity `.into()` where a transform was needed — stays
`wasm_matrix_roundtrips`' job; this is a *presence* differential.

Inputs are every `tests/matrix_wasm/*.cddl` cell (even `WASM_MATRIX_SKIP` ones — parity is
parse-only, and their emitted sources parse even when they don't standalone *compile*) plus the two
depth fixtures `tests/core/input.cddl` and `example/test.cddl` (kitchen-sink shapes the minimal cells
don't reach), each swept across `ALL_PROFILES` (default / preserve / json — the flags substantially
change the rust surface). A second corpus axis sweeps every committed `tests/*/input.cddl` fixture
dir under that dir's committed generation profile rows from `integration_tests.rs` (dropping only
flags irrelevant to the emitted `src/generated` API surface, such as `--emit-tests`,
`--wasm=false`, and `--package-json`). A completeness guard enumerates `tests/*/input.cddl` at
runtime and requires every dir to be either in the corpus table or in the exclusion table; the two
excluded dirs are `core` (already swept as a depth fixture across all profiles) and
`wasm-list-macro` (its committed wasm members are emitted as user-macro invocations, invisible to a
`syn` presence differential). Directory-input fixtures such as `tests/multifile/inputs` and
`tests/extern-deps*/inputs` are out of scope for this axis: multifile emission writes per-module
files under `src/generated/`, outside this differential's `mod.rs`-only parse scope, and is covered
by the separate multifile placement sweep. Vacuity guards pin the matrix/depth count plus total
corpus profile rows so the sweep can't silently shrink.

Generation is **in-process** (`api::generated_strings` via `Cli::parse_from`, wrapped in
`catch_unwind` — no subprocess, no scratch dirs) and **parse-only** (no cargo check/test of the
generated crates), so the sweep stays always-on (no `#[ignore]`) in the default `cargo test` /
check.ts local tier. It scopes to `src/generated/mod.rs`; a key-set guard over the returned file map
fails loudly on any `.rs` name outside the per-profile allowlist (the rust base list includes
`key_demand_assertions.rs` — any `@used_as_key`-tagged crate's private compile-time-only
`_demand_*` self-checks, zero pub items so nothing to parse; preserve additionally allows
`cbor_encodings.rs`/`ordered_hash_map.rs`, both optional; the wasm side allows `mod.rs` plus
`collections.rs` — the wrapper re-export index every wasm crate now emits, a `pub use` inventory of
classes already defined in `mod.rs`, so it introduces no boundary API for the differential to
parse), so a future emission surface can't silently escape the differential — it caught
`key_demand_assertions.rs`'s widening to bare roots exactly this way before the file was
classified. One (profile, input)
pair is pinned in `EXPECTED_GENERATION_FAIL`: (preserve, tests/core) — a float member aborts
generation under `--preserve-encodings` (issue #205, the `preserve_encodings_supports_floats` stub)
— with a resurfaced guard both directions (a listed pair that generates fails as "gap closed —
remove the pin"; an unlisted abort fails normally).

Findings reconcile against a `PARITY_EXEMPT` ledger keyed `(profile, input, item, reason)`, the same
`WASM_MATRIX_SKIP` idiom: a finding matching an entry is expected (no failure); an entry matching no
live finding fails as "resurfaced" (a fix landed — remove it); an unexempted finding fails with the
remedy spelled out (fix the emitter, or deliberately ledger it with a reason). The ledger is
**empty** — every finding class the gate has surfaced was fixed at the emitter rather than ledgered:
the named-table wasm alias emitted as a private `type` instead of `pub type` (`generation/`'s
already-generated-map branch now carries `.vis("pub")`, matching its sibling passthrough-alias site);
the preserve-profile wrapper `inner` field emitted `pub` (caught by the profile sweep's first run;
now `pub(crate)` like the default profile's tuple field — deliberately crate-visible so hand
modules outside the generated subtree can reach it, while EXTERNAL code still can't
literal-construct or mutate a wrapper past the bound check `new()` enforces and goes through the
getter; the finding class stays retired because the sweep's `is_pub` matches only
`syn::Visibility::Public`, so `pub(crate)` reads as non-pub to it); and the rule-5 usage-dependent JS-class-name bug, where a named table rule's wrapper
degraded to a `pub type` alias pointing at the generator-invented structural map class (so the CDDL
rule name never reached JS and the shape's class name flipped with unrelated spec content). The fix
(`generation/`'s up-front table-shape ownership pass): a shape owned by a SINGLE named rule now
surfaces its class under the CDDL rule name, with the structural `MapKToV` name a `pub type` alias to
it; same-shape rule PAIRS keep the structural fallback for embedded uses while each named rule still
gets its own class. The corpus-axis landing also surfaced the built-in `int` prelude wrapper gap:
rust exposed `Int::new_uint`, `Int::new_nint`, and `IntError` for `int` map keys, while wasm exposed
only the signed `Int::new(i64)` constructor and mapped parse failures to `JsError`; wasm now emits
the two raw-CBOR-argument constructors and a source-level `pub type IntError = JsError` counterpart.

## multifile placement matrix (`tests/matrix_multifile/` + `integration_tests::multifile_matrix_{compiles,roundtrips}`)

A **coverage-by-construction** gate for the axis every OTHER construct gate is blind to: **module
placement**. The corpus gates, the wasm-ABI matrix, and the parity differential all feed the
generator SINGLE-file specs, so every construct is only ever verified in root scope. Multifile
emission branches on scope — `mark_refs` (`intermediate/mod.rs`) resolves each collection
occurrence's wasm wrapper NAME and HOME scope through `IntermediateTypes::wasm_collection_wrapper`
(the emitter's `for_wasm_member` twin, `table_shape_sole_owners`-aware), while the wrapper/alias
definitions land wherever `types.scope(ident)` puts them — and that region had exactly one hand
fixture
(`tests/multifile`, which covers NAMED cross-module refs but no structural-wrapper-ownership
cells). This sweep enumerates the placement grid, compile-floors it (always-on), and round-trips it
(manual, full tier). Two placement vectors the grid does NOT enumerate are hand-fixture-owned:
the group-choice-VARIANT reference position (`tests/multifile`, see Axis 2 below), and the
extern-shaped type-alias-TARGET position (`tests/extern-generic-scoped` — a generic-EXTERN
instance aliased from a non-root scope decomposes into a base import at the base's declaring
scope plus argument imports, never the whole `Base<Args>` type expression; extern shapes sit in
this grid's SHAPES exclusion, so the compile floor can never enumerate them — see Axis 1).

Pipeline (projection → fixtures → gates), the same two-gate shape as the wasm-ABI matrix:

```
cddl-matrix/project_multifile_matrix.ts  ─►  tests/matrix_multifile/<shape>__<mode>/{lib,a,b}.cddl  ─►  integration_tests::multifile_matrix_compiles
     enumerate {shape × ref-mode}             two-module DIRECTORY fixture per cell                     generate --wasm=true (dir input), cargo check the wasm crate
                                                                                                    ─►  integration_tests::multifile_matrix_roundtrips (#[ignore]d)
                                                                                                        generate --wasm=true --emit-tests=true × ALL_PROFILES, cargo test rust/ + wasm/
```

- **The two-module template.** Each cell is a DIRECTORY fixture. `lib.cddl` (file stem `lib` ==
  `ROOT_SCOPE`) is the root — one trivial rule (`rt = [uint]`), constant across cells; `a.cddl` (scope
  `a`) holds the shape's defs; `b.cddl` (scope `b`) holds the reference. Root-owner direction (shape
  in root, referenced from a module) is deliberately NOT enumerated — root-module owners probed fine
  in both directions, so the non-root-owner cells are the discriminating ones.
- **Axis 1 — type-shape** (`SHAPES`, copied verbatim from `project_wasm_matrix.ts` with a provenance
  comment; NOT imported — that module projects on import; plus the multifile-specific `collrec`,
  `[* <record>]` — the structural array wrapper only needs placement cross-module, so the wasm
  matrix's root-scope grid cannot probe it — and `tblrec`, `{ * <record> => text }` — the
  non-exposable-KEYED table, whose `keys()` accessor names a root-minted keys-list wrapper that
  likewise only dangles cross-module). Every self-contained shape that HAS defs is
  included; `prim` (no defs — nothing to place in a module) and `extern`/`rawbytes` (user-supplied
  types, can't compile standalone) are excluded with header comments. The exclusion bounds the
  GATE, not the generator: extern shapes still have placement behavior (re-export glue routing;
  generic-instance alias decomposition), and its alias-position residue escaped to a production
  regen as feature request 07 — now hand-pinned by `tests/extern-generic-scoped`
  (`extern_generic_scoped` + `extern_generic_scoped_alias_imports`).
- **Axis 2 — cross-module reference mode.** `named` — `b` references the shape's named rule
  (`bholder = [field0: <ty>]`); `aliased` — `b` ALIASES it (`bal = <ty>`, a plain rule alias whose
  emitted `pub type Bal = …;` names the cross-module target with no field reference in sight — the
  `scope_references` type-alias walk, the reference position a consumer's
  `policy_id = script_hash`-style domain aliasing hits, proven E0412 in production before the walk
  landed; module `b` is alias-only on purpose, that being the production shape, with the
  alias-only-module E0583 class independently pinned by the alias shapes' `unref` cells); `anon` —
  `b` embeds the shape's inline anonymous same-shape spelling
  (the `mark_refs` structural class); `anonb` — `anon` plus a ballast record rule in `a`
  (`ballast = [bal0: uint]`), so `a` emits `serialization.rs` and an alias-only-module abort can't
  mask the b-side import verdict (the discrimination that isolates structural-import placement
  regressions); `unref` — `b`
  references nothing (`[field0: uint]`), so an alias/table-only module `a` still gets emitted.
  `named`/`aliased`/`unref` apply to every shape; `anon` exists ONLY for a shape whose anon holder
  `holder = [field0: <anonForm>]` compiles GREEN as a **single-file control** — otherwise the red
  would be a single-file limitation, not a placement finding, and the shape carries no `anonForm`
  (the controls are throwaway, not committed). All 13 candidates
  (`coll`/`collmap`/`collrec`/`tblrec`/`tag`/`nullable`/`bwrap`/`cborwrap`, the restricted
  non-empty shapes `necoll`/`necollrec`/`nemap`, and the synthesized-NonEmpty shapes
  `nesyncoll`/`nesynmap`) probed green. `anonb` applies to
  exactly the anon shapes whose plain `anon` cell would be masked by an alias-only module `a`
  emitting no serialization (`coll`/`collmap`/`nullable`/`necoll`/`nemap`); the other anon shapes'
  module `a` already emits serialization, so nothing masks their b-side verdict and a ballast
  variant adds no discrimination (their `anon` cells are green).
  The field-embedding modes reference the shape from a record-FIELD position; `aliased` is the
  type-alias-TARGET position (added when its import class escaped to production — the second
  position-keyed escape after the group-ctor one below); other reference POSITIONS are
  not enumerated. The one known position-keyed import class — a group-choice VARIANT over a
  foreign-scope Record, whose expanded `new_<variant>` ctor names the record's field types in the
  choice's module (marked by `scope_references` via the shared
  `EnumVariant::group_ctor_record_fields` helper) — is pinned by the hand fixture instead
  (`tests/multifile`: `relay` in `qux.cddl` over `relay_host` in `b/bar.cddl`, test
  `cross_module_group_choice_ctor`, compiled rust+wasm under both fixture profiles); the mode-axis
  extension is recorded recur-first in `tests/TESTING_ROADMAP.md` ("Multifile reference-POSITION
  coverage").
- **The compile floor** (`integration_tests::multifile_matrix_compiles`) globs the cell dirs,
  generates each with DIRECTORY input `--wasm=true`, and `cargo check`s the wasm crate ONLY (which
  path-depends on the rust crate, so rust-side breakage surfaces transitively). Own scratch +
  `CARGO_TARGET_DIR` (`cddl_codegen_multifile_matrix`). Always-on (no `#[ignore]`): it joins the
  default `cargo test` / check.ts local tier. Wall-clock ~35 s (first cold run, shared target warms
  once) / ~30 s warm measured at 43 cells (144 at HEAD).
- **The round-trip gate** (`integration_tests::multifile_matrix_roundtrips`, `#[ignore]`d, check.ts
  **full** tier — the behavioural upgrade, mirroring `wasm_matrix_roundtrips`): same cell
  enumeration, but each cell is generated `--wasm=true --emit-tests=true` across `ALL_PROFILES`
  (default / preserve / json) and `cargo test`ed, so the minted `cddl_generated_tests` /
  `cddl_generated_wasm_tests` modules RUN the cross-module wiring — module `b`'s holder is
  constructed from module `a`'s shape (`Bholder::new(St::new(..))`) and round-tripped, and the wasm
  twin byte-differentials against the fully-qualified `cddl_lib::b::…`/`cddl_lib::a::…` natives.
  (`aliased` cells have no `bholder` — module `b` is a lone `pub type`, whose transparent alias
  mints no standalone test surface, so their round-trip value is module `a`'s own surface plus the
  compile proof that the alias line's cross-module import resolves; the compile floor is those
  cells' discriminating gate.)
  BOTH generated subcrates are `cargo test`ed (`rust/` then `wasm/`): the rust crate's
  `#[cfg(test)]` module is not compiled when it's built merely as the wasm crate's dep, and the
  proven placement classes are rust-side. Own scratch (`cddl_codegen_multifile_rt`) +
  `acquire_scratch_lock`, one shared `CARGO_TARGET_DIR`, each per-cell dir freed after its verdict.
  Loud-skip contract as the wasm round-trip gate: a cell minting no test surface passes with zero
  tests (the emitter eprintln!s the skip; the floor still pins its ABI) — and a minted-module
  vacuity floor (each generated crate's root `generated/mod.rs` is grepped for its test module;
  observed 144/144 rust and 144/144 wasm at the 48-cell grid — the count scales with the cell ×
  profile grid) bounds the aggregate so green can't quietly go vacuous.
  The multifile `--emit-tests` emission itself (root-level test module + `use super::<m>::*;` scope
  globs, without which every multifile cell is E0433-uncompilable) is pinned always-on by the
  in-process `emit_tests_multifile_scope_imports`, so a regression there doesn't wait for full
  tier. Run with `cargo test --bin cddl-codegen multifile_matrix_roundtrips -- --ignored`
  (~4.6 min measured at 48 cells, scaling with the cell count — 144 at HEAD; every run is effectively cold — the scratch root, shared target
  included, is cleared at start and end — with the deps built once up front and the remainder
  dominated by the per-cell-per-profile generate + two nested `cargo test` invocations (3 profiles x the cell count each).
- **Skip ledgers (round-trip gate).** `MULTIFILE_ROUNDTRIP_SKIP: &[(&str, &str)]` (cell stem,
  reason) holds cells red in EVERY profile — currently EMPTY (every cell compiles and round-trips).
  No rustc-error-code class assertion here: the compile floor's `MULTIFILE_MATRIX_SKIP` already pins
  each cell's exact class. `MULTIFILE_ROUNDTRIP_PROFILE_SKIP: &[(&str, &str, &str)]` (profile, cell
  stem, reason) holds profile-specific reds — also EMPTY: the sweep is green across all three
  profiles. Both are four-state (red+listed = expected; red+unlisted = fail; green+listed =
  resurfaced — remove the pin; green+unlisted = pass) with up-front stale-key guards (unknown stem or
  profile fails before any heavy work). Verify a new guard the poison way: pin a green cell →
  resurfaced failure; add a bogus stem → stale-key failure; revert.
- **Skip ledger (compile floor).** `MULTIFILE_MATRIX_SKIP: &[(&str, &[&str], &str)]` (cell stem, expected rustc
  error codes, reason) holds the deliberately-red cells, four-state like `WASM_MATRIX_SKIP`:
  red+listed = expected; red+unlisted = a new placement finding to fix or (deliberately, with a
  ROADMAP entry) pin; green+listed = "resurfaced — remove the pin (a fix landed)"; green+unlisted =
  pass. **Class assertion:** a red+listed cell is NOT satisfied by any redness — the observed rustc
  error-code set (`rustc_error_codes` scans the captured cargo stderr for `error[E####]` headers)
  must EQUAL the pin's declared set, or the gate fails loud with "the cell's failure class changed —
  re-triage the pin" (set equality is the contract, never subset — pin the full honest observed set
  if a cell co-emits multiple codes); a listed cell whose GENERATION aborts is likewise a class
  mismatch (the pin claims a rustc compile error, and a generation abort produces none). Author a
  new pin's codes from the observed evidence, not the expected diagnosis: the gate's red-cell
  failure output prints the captured cargo stderr, whose `error[E####]` headers are exactly the set
  to pin. An up-front stale-key
  guard rejects a listed stem absent from the projected set, and a missing wasm crate is handled
  symmetrically. Verify a new guard the way these were: temporarily poison a key (bogus stem →
  stale-key fail; drop a real pin → the red cell fails with the remedy; pin a green cell → resurfaced;
  change a real pin's error code to a bogus one, e.g. `E9999` → the class-changed message fires),
  watch it fail, revert.

**What it guards today.** Every projected cell compiles and round-trips — both skip ledgers are
empty. Greenness rests on emitter invariants this matrix guards, each once a loud cross-module
failure class: every cross-module collection occurrence imports the SAME wasm wrapper the emitter
names, from the module it is minted in (`scope_references`/`mark_refs` resolve the wrapper name +
home scope through `IntermediateTypes::wasm_collection_wrapper`, the `for_wasm_member` twin, so
import and emission placement cannot disagree — the restricted `[+ T]`/`{+ k => v}`/`@duplicates
preserve` `NonEmpty*` family and the synthesized-NonEmpty facet `nesyncoll`/`nesynmap` all name the
`NonEmpty*` wrapper the emitter uses, not the pre-NonEmpty spelling); each restricted wrapper's loose
`try_from(&Loose)` source is imported at the wrapper's emission scope
(`register_root_non_empty_{list,map}_source`, the non-deferred analogues of the keys-list/deferred
helpers — the E0425 class the `necollrec`/`nemap`/`nepmap` cells guard); and a field referencing a
NAMED collection rule (`recs = [* foo]` / `gcn = gcoll<foo>`, or a DEP-owned `@rust_name`-pinned map)
imports only the rule ident, never a structural wrapper the rule's own class subsumes (the `Alias`
arm suppresses the structural-wrapper import when the alias names a collection rule — the E0432 class
`collrec__named`/`gcolln__named` guard, and the dep-owned flavor
`dep_owned_named_collection_no_local_structural_import` guards, with its cross-crate compile
companion `dep_owned_named_collection_compiles` building both generated crates against the
stand-in dep pair so the pinned-absent dangling import is also caught as the E0432 it would be).
Greenness also rests on four further
emitter invariants this matrix guards: a module
declares `pub mod serialization;` only when that file is written (the module-declaration loop in
`generation/export.rs` shares the `serialize_scopes` predicate with the file-write, so an alias/enum-only
non-root module cannot declare a phantom module — the E0583 class); an anonymous same-shape table
used cross-module imports the structural wrapper from the sole owner's module
(`scope_references`/`mark_refs` consult `IntermediateTypes::table_shape_sole_owners`, the same
helper the wasm emit path uses, so import and emission placement cannot disagree — the E0432
class); a cross-module *named* reference to a `.cbor` wrapper imports the inner named type into
the referencing module (`mark_refs`' Alias arm recurses into the alias target so idents the inlined
serialization names get imported — the E0433 class); and a non-root table class whose KEY is
non-exposable imports the keys-list wrapper its `keys()` accessor names bare — the root-minted
`<Key>List` (`register_root_keys_list`) OR, when that keys-list workspace/index-defers to a
dependency, `use <dep_wasm>::collections::<KeysList>;` (`register_deferred_keys_list`) — with BOTH
homes registered at both the inline-`Map` and the NAMED-`Table` struct-walk arms, mirroring
`codegen_table_type`'s emission condition (the `tblrec` E0425 class, guarded green by all three
`tblrec` cells and exercised end-to-end by `wasm_collections_index`'s record-keyed non-root table;
the deferred flavor for a NAMED table over a dep-owned key — E0412 when its import is stranded —
by `workspace_dep_named_table_deferred_keys_list`). The two gates split the verdict: the
always-on **compile floor** pins that every non-pinned cell's cross-module wiring type-checks (its
four-state class-asserting verdict stays live so any regression re-pins with the observed
error-code evidence), and the full-tier **round-trip gate** executes that wiring across all three
profiles — a green placement cell is semantically verified once both hold (first full sweep:
every non-collrec cell green under default, preserve, and json).

**Adding / changing cells.** Edit `SHAPES`/`MODES` in the projection, `bun run
project_multifile_matrix.ts`, review the new fixtures, run the gate. Output is deterministic — **never
hand-edit `tests/matrix_multifile/`**; `--check` is the drift gate (stale/missing/orphaned dir or
file). `EXPECTED_CELLS`, `EXPECTED_ANON_SHAPES`, and `EXPECTED_ANONB_SHAPES` guard the grid, so a
shrink/growth is an explicit reviewed edit.

## Shape-recombination fuzzer (`tests/recomb/` + `src/tests/recombination_tests.rs`)

Deterministically recombines the matrix's per-feature examples into composed CDDL specs that no
single-example gate samples, and runs them through the generator with escalating oracles. The
motivating gap is proven, not speculative: every other gate samples ONE minimal example per feature
row, and the map-rep group-choice fix found three defects hiding in unsampled shape *variants* of a
single "supported" row. The harness varies exactly the axes that mattered there: multi-member shape
variation inside one construct (a member-kind table: fixed uint/text/bool/null values, keyed
scalars, optional / zero-permitting occurrences, inline groups, filler-typed members — composed 1–3
at a time into struct maps, array records, and both group-choice representations), depth-2 nesting
of constructs in container roles (a role-template table: array element, map key/value, choice
member, group-choice arm, occurrence target, tag content, `.cbor` payload, generic arg, top level),
and — low-weight — identifier choice drawn from `identifier_hazard_tests::hazards()` (never
rediscovered; the hazard sweep owns that axis systematically).

Stage A is a TypeScript projection, `cddl-matrix/project_recombination.ts`: it reduces each matrix
feature's `example` to a reusable hole-fillable expression (primary-rule RHS + auxiliary rules;
irreducible examples are recorded in a `skipped` list with reasons) and projects the containment
legality data, writing the committed `tests/recomb/ingredients.json` (`--check` is the
`project_recombination_check` drift gate, check.ts local tier). Legality semantics: the containment
matrix enumerates only structurally interesting cells and deliberately omits trivial
primitive-as-member cells as implicitly allowed, so the composer treats the projected `disallowed`
pairs as a BLACKLIST (anything unlisted composes) and uses the `legal` (spec="allowed") pairs only
as template↔matrix drift protection — every role template must name a role with at least one
modelled allowed cell.

Stage B is the Rust harness, seeded (fixed `SEED` + splitmix64) and enumeration-deterministic
(systematic cross-products where cheap, seeded sampling where the product explodes; the sweep
asserts two back-to-back enumerations are identical). It is a **corpus generator, not a CI gate**:
the standing harness detects NEW divergence classes; each finding is promoted into the existing
pinned collections after review. Two layers, mirroring the identifier-hazard split:

- `recombination_generation_sweep` (default `cargo test`, check.ts local tier, ~5 s wall —
  classification is parallelized across worker threads; thread count never changes WHAT is swept):
  classifies every composition's generation outcome in-process (`catch_unwind` + the shared
  silenced-hook idiom, extended with a per-worker capturing hook that records the normalized panic
  key `<message> @ <file> @ fn <symbol>` — the panicking production frame's function symbol, from a
  backtrace captured only on a panic, so two bare `unimplemented!()` sites in different functions no
  longer collapse to one class; line numbers stay excluded for refactor resilience). A PANIC whose
  normalized key matches no `KNOWN_PANIC_CLASSES` entry is a NEW
  finding and FAILS the sweep, printing the spec + message + promotion instructions (minimize by
  hand → pin as a matrix row if the matrix can express the cell, else a `tests/robustness/*.cddl`
  fixture → ledger it in `cddl-matrix/ROADMAP.md` § findings → add the ledger entry citing the
  pin). Every ledger entry cites a committed pin AND is asserted actually observed (stale-pin
  guard), and key SHAPE is floor-gated (`ledger_key_shape_floor`, always-on): panic-ledger keys must
  lead with message text — a file/function-only key would silently absorb every future distinct
  panic class at that site — and layer-2 known-bad keys must carry a desc-axis label so a generic
  word cannot absorb unrelated compositions. Graceful rejections are the designed boundary, tallied
  but never findings. Vacuity floors (swept count, ok count) are derived from the executed artifact.
- `recombination_crates_execute` (`#[ignore]`, check.ts full tier — the `recombination_crates_execute`
  gate): batches the sweep's `ok` compositions (~40 rules/batch; per-composition `rc<num>_` rule
  prefixes make names collision-free by construction), generates each batch with
  `--emit-tests=true --wasm=false` (default profile) via the `tool_cmd`/shared-`CARGO_TARGET_DIR`
  pattern of `feature_corpus_compiles`, and `cargo test`s the generated rust crate — executing the
  emitted round-trip/reject tests, not just compiling. A failing batch is re-attributed by rerunning
  members individually; a failing member outside the cited `LAYER2_KNOWN_BAD` ledger (desc-substring
  keyed, vacuity-guarded like the layer-1 ledger) is a NEW finding with the same promotion flow.
  BATCH-MASKING CAVEAT (applies to every layer-2 leg): a green batch is not a per-composition
  guarantee for failure classes whose symptom is a missing CRATE-GLOBAL definition — a batch-mate
  can define the global (the note on `LAYER2_RULES_PER_BATCH`; the proven instance was the
  undefined-`Int` `.cbor`-payload-table cell, since fixed — the reference walk covers emitted type
  aliases, pinned by tests/corpus/int_alias.cddl — but the caveat stands for the next
  crate-global-definition class). Consequence: a known-bad class proven by a STANDALONE repro is
  ledgered even when current batch boundaries compile it green; the mechanical detector (a second
  deterministic batch permutation / singleton mode) is a `tests/TESTING_ROADMAP.md` item.
- `recombination_preserve_crates_execute` (`#[ignore]`, check.ts full tier): the PRESERVE escalation
  of layer 2, driven by the SAME shared runner (`run_layer2_profile`) parameterized with a different
  `Layer2Profile`. Its profile flags are sourced from `src/tests/mod.rs`'s `ALL_PROFILES` by name
  (asserted found, never re-hard-coded), so `classify_all` runs the composition set under
  `--preserve-encodings=true`; the batches then generate `--preserve-encodings=true
  --emit-tests=true --wasm=false` and `cargo test`. Motivation is a proven escaped regression: a
  preserve-only E0308 on tag-wrapped fixed-value members (`[v: #6.1(null)]`) passed every
  default-profile gate and was caught only by review — a preserve batch over the same compositions
  fails loudly on it. Classifying under preserve PANICS for classes that are ok/graceful under
  default (native floats as members; a tag over a type-choice/enum; a tag wrapping `any`); those live
  in `PRESERVE_ONLY_PANIC_CLASSES` (checked after the shared `KNOWN_PANIC_CLASSES` allowlist, each
  citing a `cddl-matrix/ROADMAP.md` § findings entry, vacuity-guarded), and a preserve panic matching
  neither ledger is a NEW finding. Per-profile scratch root + `CARGO_TARGET_DIR`
  (`cddl_codegen_recomb_<profile>_<hash>`) keep profiles from clobbering each other. Exclusion set =
  the shared `LAYER2_KNOWN_BAD` ∪ the profile's own `LAYER2_PRESERVE_KNOWN_BAD`; only the profile's
  own ledger is vacuity-guarded here (a shared entry can legitimately match zero preserve-ok
  compositions because preserve generation panics for that class earlier — the shared ledger's guard
  stays in the default gate). `LAYER2_PRESERVE_KNOWN_BAD` is empty at HEAD — the escalation's first
  sweep's two preserve-only compile classes (tag/`.cbor`-wrapped constrained-int deserialize tuple
  arity; composite map-key move-then-reuse) are fixed, with their preserve compile + round-trip
  pinned by the `tagged_constrained_int` / `composite_map_key` corpus fixtures — so a preserve-only
  compile failure surfaces as a NEW finding. Observed baseline: 1544 classified compositions
  (`ok=856`, `graceful=203`, `panic=485`), 24 batches / 827 executed / 29 shared known-bad
  exclusions in ~45 s. NAMING GOTCHA: the name deliberately does NOT contain the
  `recombination_crates_execute` needle, and both check.ts gate cmds pass `--exact` on the full test
  path so cargo's substring selection can't cross-select.
- `recombination_json_crates_execute` (`#[ignore]`, check.ts full tier): the JSON escalation of
  layer 2, using `ALL_PROFILES["json"]` (`--json-serde-derives=true
  --json-schema-export=true`) plus `--emit-tests=true --wasm=false`, then `cargo test` on the
  generated `rust/` crate. This is the broad shape gate for serde derive / schemars derive compile
  failures while still executing the emitted CBOR tests. `--json-schema-export=true --wasm=false`
  also emits an independent `wasm/json-gen/` crate; this recombination leg deliberately leaves that
  crate to the existing json profile compile/schema gates rather than running it per batch. Both
  json-only ledgers (`JSON_ONLY_PANIC_CLASSES`, `LAYER2_JSON_KNOWN_BAD`) are empty at HEAD — json
  derives do not rewire the panic surface, so classification matches the default profile exactly.
  Observed baseline: 1544 classified compositions (`ok=927`, `graceful=197`, `panic=420`),
  26 batches / 897 executed / 30 shared known-bad exclusions in ~54 s.
- `recombination_wasm_crates_check` (`#[ignore]`, check.ts full tier): the WASM escalation of
  layer 2, using explicit `--wasm=true` for both in-process classification and out-of-process batch
  generation. It does not pass `--emit-tests`: the oracle is `cargo check` on the generated `wasm/`
  crate, which depends on the generated `rust/` crate by path, so rust-side compile failures surface
  through the same command. Both wasm-only ledgers (`WASM_ONLY_PANIC_CLASSES`,
  `LAYER2_WASM_KNOWN_BAD`) are empty at HEAD — tagged tables and alias-only-reachable table wrappers
  generate and check (pinned by the `tagged_table` / `cbor_bignint_table` corpus fixtures) — so a
  wasm-only panic or compile class surfaces as a NEW finding. Observed baseline: 1544 classified
  compositions (`ok=926`, `graceful=197`, `panic=421`), 26 batches / 897 checked / 29 known-bad
  exclusions in ~50 s. This is
  a fuzz-recombination cross-check for wasm generation paths; the wasm-ABI matrix remains the
  systematic per-shape wasm surface owner.

Adding a member kind / role template / construct shape extends the swept surface; re-tune the
executed-artifact floors when doing so deliberately. Changing `SEED` re-rolls every sampled
composition — do it deliberately and re-triage.

## Design rules (review-owned; each with a shipped exemplar)

Two rules govern how guards and graceful-rejection refactors are written. Review is their current
owner; the conditional mechanical layers (built only if a class recurs) are a
`tests/TESTING_ROADMAP.md` item.

- **Invariant-softening refactors keep impossible states loud.** When a panic/assert is converted
  into a graceful rejection, enumerate the states the assert covered and downgrade ONLY the
  reachable, user-triggerable ones; states the assert made impossible stay `unreachable!`. A
  catch-all soft arm silently absorbs the impossible state, and no gate can see that (a mutation
  sweep would at best surface the arm as a survivor that triage then plausibly waves through as
  equivalent — it cannot distinguish "kept loud" from "absorbed"). Shipped exemplar:
  `set_rep_if_plain_group`'s multi-rep match in intermediate/mod.rs (conflicting-rep = graceful
  rejection; non-Record/non-GroupChoice materialization = still `unreachable!`).
- **Vacuity floors witness the guarded artifact, not a proxy for it.** A floor whose count derives
  from an INPUT correlated with the guarded behavior — rather than from the behavior's own
  artifact — is satisfied by any regression that preserves the input (a floor counting catalog
  `expect_err` presence stays green while the emitted assert regresses to a plain `is_err`,
  leaving the pin vacuous). Derive the floor's count from the emitted/executed artifact, or place
  an assert at the emission site itself, outside the branch being guarded. Shipped exemplars:
  `decode_replay_run`'s CONSTRAINT_WRONG_REASON body assert, and the pipeline-boundary
  rejection-drain assert in `api.rs` (both generation exits assert no `record_rejection` survives
  past `finalize`'s drains — a post-drain record site would otherwise be silently swallowed with
  the tool exiting 0; mutation-proven by injecting a post-drain record and observing the snapshot
  suite go red).

## Coverage

The in-process snapshot suite alone covers ~81% of the codebase (generation/ ~86%). To measure
(requires `cargo install cargo-llvm-cov` + `rustup component add llvm-tools-preview`):

```sh
cargo llvm-cov --summary-only -- snapshot_tests
```

Note: the integration tests run the generator in a subprocess, so llvm-cov (which instruments the
test binary) does not attribute their coverage — the 81% is from snapshots + in-process unit tests
only.

## Mutation testing (`cargo-mutants`, manual survey — not a check.ts gate)

Mutates the emit core (`src/generation*`) and scores each mutant against the **behavioral layers
only** — nextest with `-E 'not test(/snapshot_tests::/)'`. Snapshots trivially "kill" almost every
emit-core mutant (any text change fails a snapshot), which measures snapshot *text-sensitivity*,
not whether a human-blessed wrong emission would be caught — the failure mode that actually ships.
Scored behaviorally, the survivor list is a direct map of emit logic no behavioral oracle observes.

All settings (scope, nextest filterset, timeouts) are pinned in `.cargo/mutants.toml`. Requires
`cargo install cargo-mutants cargo-nextest --locked`. Run from the repo root:

```sh
cargo mutants             # fresh sweep (writes mutants.out/, gitignored)
cargo mutants --iterate   # resume: skips mutants already caught/unviable in mutants.out/
```

Never pass `--in-place`: an interrupted in-place run leaves a live mutant applied to
`src/generation/` in the working tree (observed). The default copied-workdir costs one warm-up
build per invocation and keeps the tree clean. Leave the default baseline on (it validates the
unmutated suite green and auto-derives sane per-mutant timeouts; `--baseline=skip` falls back to a
300 s cap that real mutant runs approach).

Measured scale (first survey): **1040 mutants**, ~1.8 min/mutant average (the behavioral suite
shells nested cargo per mutant) — a complete sweep is a **~30 h unattended job**; run it overnight
in `--iterate` chunks. First-survey sample (33 tested: 13 caught, 14 unviable, 6 missed): all 6
misses triaged as *behaviorally equivalent by construction*, not oracle gaps — the
`container_encoding_lookup` arity branch exists only for `clippy::redundant_closure` (both branches
emit semantically identical code), and `encoding_var_is_copy -> false` only adds redundant
`.clone()`s to generated code (the dangerous direction, `-> true`, is caught behaviorally on all
three impls). Expect that equivalent-style class among survivors; the sweep's value is the
survivors that *aren't* in it.

## Known gap

The CDDL standard prelude (`biguint`, `tdate`, `uri`, …) is covered by `tests/corpus/prelude.cddl`.
The float-bearing prelude types (`number`, `time`) are omitted from it because floats don't support
`--preserve-encodings` (a pre-existing `unimplemented!` in `generation/deserialize.rs`), which the corpus
exercises for every entry.

[`insta`]: https://insta.rs
