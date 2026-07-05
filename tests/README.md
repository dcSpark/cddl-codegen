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
| `full` | `bun run check.ts full` | `local` + every manual-only gate | ~10 min |

`fast` is exactly what CI runs (`build.yml` is a thin `bun run check.ts fast` invoker — see the CI
policy below). `local` is "run before considering work done" — the heavy correctness gates (full
`cargo test`, corpus + wasm-matrix compiles) plus `matrix_typecheck` (`tsc --noEmit` over the
`cddl-matrix` scripts, via a dev-only local `typescript`/`@types/bun` — run `bun install` in
`cddl-matrix/` once; the runtime stays dependency-free) and the decode-conformance catalog drift
gate (`project_decode_conformance.ts`) live here, NOT in CI. `full` additionally runs the
manual gates (the five `#[ignore]`d gates `wasm_matrix_roundtrips` / `ir_conformance_corpus` /
`decode_conformance_replay` / `all_supported_constructs_generate_all_profiles` /
`feature_corpus_roundtrips_nondefault_profiles`, `cddl-matrix/verify.ts`, `corpus_detect.ts`, and
the fuzz-crate compile-rot check) — run it before shipping a feature. Every run ends with the **full registry** printed as a table (`PASS` / `FAIL` /
`SKIPPED(reason)` / `STUB` / `not-in-tier` + per-gate durations), so a gate that didn't run is always
*visibly* not-run. Exit is non-zero on any `FAIL`; the run fails fast by default (`--keep-going` runs
every in-tier gate first).

`verify.ts` needs two oracles (ruby `cddl`, rust `cddl`); the runner preflights them and prints
install one-liners on failure (`--skip-missing` downgrades a missing oracle to `SKIPPED`). The fuzz
gate re-runs `fuzz/generate.sh` only when `fuzz/generated` is absent or `--refresh-fuzz` is passed.

The runner's **first gate is three self-completeness meta-checks**: every `#[ignore]` test must be
registered as a manual gate or a known-failing stub, every `cddl-matrix/*.ts` (minus `lib.ts`) must
be wired to a tier, and `build.yml` must invoke `bun run check.ts fast` with no other run step (so
CI can neither drift away from the fast tier nor grow work that bypasses the registry). This is the
systematic catch for the disease the runner cures — a gate that exists but is in nobody's habit — so
a new manual gate or IOU stub is a conscious registry edit, not a silent omission.

Wall times above are warm-cache, measured on the dev machine; a cold build adds the one-time
dependency + test-binary compile.

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
  preserve-encodings is unimplemented for floats) and for inputs whose output *can't compile
  standalone* (`extern_deps`/`raw_bytes` reference user-supplied types; their behavioral coverage is
  their integration fixtures) — this suite never compiles, so neither constraint bites here, which
  is why such inputs are pinned here rather than via corpus skip-lists that would weaken the corpus
  invariant that every fixture is fully gated.
- **`cargo_toml_matrix`** — a small curated `input × profile` matrix that snapshots every distinct
  generated `Cargo.toml` dependency combination (the type-conditional `hex`/`wasm-bindgen` deps
  toggled independently). The per-feature corpus skips `Cargo.toml` as near-constant noise, and
  `whole_program` doesn't produce every combination, so this is where they're all pinned. Beyond the
  snapshots it asserts each conditional dep is present *exactly* when its flag/type condition holds —
  the absence half guards the manifest changeset's set-or-**remove** contract (a dep whose condition
  turned off must be removed from an existing manifest, not skipped; see `cargo_manifest.rs`). The
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
from); a **pass is weak** — the validator has known gaps (it does not enforce `uint .size`, and
mishandles `.size`-aliased element types inside arrays) AND it is *not fully decorrelated*: it parses
the `.cddl` with the same dcSpark `cddl` fork at the same pinned rev as the generator's own front end
(`CDDL_ORACLE_DEP`), so a **fork-level misparse** (grammar/AST bug that corrupts generator IR and this
oracle's spec-interpretation identically) escapes it. That specific gap is now covered by a *lineage*-
decorrelated second sweep — the harness-side ruby `cddl` gem in `ir_conformance_corpus` (below), which
shares no parser with the fork — but this rust oracle's own caveat still holds: it remains a *second*
oracle, never the sole one. Because the validator validates against a spec's first type rule only, the
helper prepends a synthetic root aliasing the rule under test.

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

### Decode-direction conformance (`tests/decode_conformance/` — accept what the spec accepts)

The fourth gate direction. The three above are all blind to an **over-strict decoder**: round-trips
only decode what they themselves encoded, the conformance oracles validate *our emitted* bytes
(encode side), and the reject tests check that spec-INVALID input is refused. A generated decoder
that rejects spec-VALID CBOR passes all of them — proven twice on this layer's first sweep (below).
This layer feeds SPEC-DERIVED CBOR instances our code did *not* produce into the generated decoders
and asserts they are accepted.

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
  `--only=<id,…>` to re-mint a subset, preserving the rest byte-identically). Generation is
  randomized, so verdict stability comes from the COMMIT: the deterministic gates below replay
  committed bytes only. A spec-valid vector the decoder rejects is written as a **class-less
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
    inverse gate) at each mint; NEVER pruned; `reason` names the violated constraint. This is Q4's
    `enforce = yes (bounded-reject)` evidence (`query_q4_directional.ts` counts `class="constraint"`
    only). NOTE: the numeric range/eq rows carry these vectors only because their probe examples
    target `int` with literal, non-vacuous bounds — the rust corroborating oracle (`cddl` 0.10.x)
    does not enforce these ops over a `uint` target (upstream gap,
    `draft/rust-cddl-uint-control-op-gap.md`), so a `uint`-targeted form can't pass the both-reject
    gate; `query_q4_directional.ts --check` pins the exact green set against such a decay.
- **The replay gate** — `integration_tests::decode_conformance_replay` (`#[ignore]`d, check.ts
  `full` tier, ~2 min: per-row crate builds under two profiles). Oracle-free and deterministic:
  per active row it generates from the committed `spec`, asserts every accept vector decodes Ok and
  every reject pin still Errs (**a pin that starts decoding green FAILS the gate** — a re-bless
  can't silently launder a bug), then regenerates under `--preserve-encodings=true` and asserts
  accept vectors decode AND re-encode **byte-identically** (the preserve contract is itself
  decode-direction evidence). `PRESERVE_SKIP` (stale-guarded, like `EXPECTED_FAIL`) carries the
  float class plus the tag-over-a-type-choice preserve gap; anything new there is a finding.
- **The drift gate** — `cddl-matrix/project_decode_conformance.ts` (check.ts `local` tier, pure
  file reads): matrix-supported ↔ catalog completeness, example-drift staleness (a drifted example
  means the vectors were validated against a spec the matrix no longer describes — re-mint),
  reject-pin class/reason shape, and the hard-coded **seeded regression controls** — the
  absent-instance vectors (`occur.optional` holder `[0, []]`, `type2.map` holder `[0, {}]`,
  `occur.zero_or_more` holder `[0, []]`) that anchor the over-strict-decoder class TDD-style.
- **The verify.ts oracle** — normal `verify.ts` runs replay each supported row's committed vectors
  as a default-on corroborating oracle (`--no-decode-foreign` / `VERIFY_DECODE_FOREIGN=0` opt-out),
  recording an `accepts_foreign` evidence clause in the annotations. Corroboration only — it never
  downgrades a support verdict; failures surface in the report's `decode_foreign_failures`.

First-sweep payoff — two miscompiles invisible to every self-consistent gate, each caught here by
feeding spec-valid CBOR our code did not produce. Map-representation group-choice single-field
variants emitted **malformed CBOR** (member key dropped) that our decoder symmetrically round-tripped
while rejecting the spec-valid form; that is now **fixed**, and the fix is pinned decode-direction by
the `group.choice` row's accept vectors (a reverted key-dropping decoder mis-decodes the spec-valid
`{"a": n}` foreign bytes and fails the replay gate), with the emitted key-write/key-verify guarded
against an unreviewed re-bless by `integration_tests::corpus_group_choice_map_key_written_and_verified`.
The array-side sibling — `[* (int, tstr)]` silently narrowing the inline-group occurrence to
exactly-once, rejecting the spec-valid `[]` — is now **fixed** too: an occurrence marker on an inline
group is rejected gracefully at generation time (pinned by
`tests/robustness/inline_group_occurrence.cddl` + `map_inline_group_zero_occurrence.cddl`), so the
matrix verdicts its cell `unsupported` and it projects no decode-conformance obligation (no catalog
row) rather than a `class="bug"` reject.

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

Generated output lands in `tests/<dir>/export*/` — disposable, gitignored, and safe to
`git clean -fdx tests` if the ~GBs of build artifacts pile up locally. CI starts clean each run.

### Encoding-fidelity oracle (`--emit-tests` × `--preserve-encodings`)

The round-trip harness above feeds `from_cbor_bytes` only the generator's *own* canonical output, so
the "decode an irregular encoding and preserve it" direction — the whole point of
`--preserve-encodings` — went untested at scale (only the hand-picked `tests/golden_hex_preserve/`
KATs covered it). When both flags are set, each round-trip case now also runs an **encoding-fidelity**
block: a self-contained, deterministic CBOR mutator (`static/emit_tests_encoding_fidelity.rs`, spliced
into the emitted test module via `include_str!`) derives six whole-tree irregular re-encodings of the
minted value's canonical bytes — `widen_step`/`widen_max` (non-minimal header widths),
`indef_containers`, `chunk_strings`, `reverse_maps`, and `everything` (all composed) — and asserts each
decodes and re-encodes byte-identically. Whole-tree (not per-position) because a single dropped
encoding-capture fails the whole variant anyway; identity variants are skipped so the loop never
asserts vacuously. With `--canonical-form` also set it adds the canonical **differential** (every
encoding canonicalizes to the same bytes) plus a per-case canonical fixed point — the KATs stay the
spec anchor for *what* the canonical bytes are; this layer buys breadth. Types with user-supplied
`@custom_serialize`/`@custom_deserialize` are excluded (their wire format isn't the generated
serializer's). The emitted mutator ships a `#[test] encoding_mutator_self_check` pinning each mutation
class against hand-derived RFC 8949 bytes *and* pinning `variants()` end-to-end on a composite input
(the vacuity guard). Executions: `emit_tests_execute` (local, with a fidelity-assertion floor) and
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
   pinned to the minted variant. Read on the freshly-*built* wasm value, not the post-wire one, so a
   wire-ambiguous choice (core's uint-`0` vs a fixed `i0` variant) can't false-fail.
4. **Boundary acceptance only** (`wasm_bounds_<type>`) — the accepted boundary value constructs
   (`.ok().is_some()`). The beyond-boundary REJECT direction is deliberately **not** emitted: a wasm
   ctor's error path builds a `JsError` through a wasm-bindgen import that panics under host `cargo
   test`; rejection is already pinned as `RangeCheck` on the wire by the rust `--emit-tests` module, so
   this half only confirms the acceptance plumbing.

wasm-API idioms baked in: `JsError: !Debug`, so a wasm `Result` is unwrapped `.ok().expect(..)`, never
`.unwrap()`; composite ctor params cross as `&Wrapper`; c-style enums cross by value; `@newtype`/tag
wrappers expose no wasm `new`, so a wrapper ENTRY type is built by decoding the rust twin's bytes and a
wrapper CTOR ARG via its `From<cddl_lib::Native>` impl, while a wrapper COLLECTION arg
(`FooList`/`FooMap`/`&Nums`) is a `new`/`add`/`insert` block expression. **Loud skips (never silent):**
extern / raw-bytes ctor args, flatten points, and the whole module under any
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
not exhaustive per-type depth. One exception to the degenerate baseline: for a CBOR tag whose RFC 8949
content the validator *semantically* enforces (tag 0 = tdate must be an RFC 3339 date-time), the minter
emits a fixed valid literal instead of the generic `"a"` — otherwise a spec-violating baseline would
round-trip byte-identically yet be (correctly) rejected by this oracle. Only tags the validator actually
enforces get a constant (`semantic_tag_content` in `emit_tests.rs`); every other tag mints the baseline.

**The gate** (`integration_tests::ir_conformance_corpus`, `#[ignore]`d — **manual/local only**, kept
out of even the local tier's `cargo test` because it adds the heavy `cddl` dep to every corpus crate):

```sh
cargo test --bin cddl-codegen ir_conformance_corpus -- --ignored --nocapture   # ~1 min
```

For every `tests/corpus/*.cddl` it generates with `--emit-tests --emit-tests-conformance`, appends
`CDDL_ORACLE_DEP` + the shared oracle helpers, copies the fixture in as
`cddl_conformance_source.cddl`, and `cargo test`s the crate under one shared `CARGO_TARGET_DIR` (so
`cddl` compiles once). The scratch root is keyed by checkout path and wiped at start, so the gate
holds an advisory lock (`acquire_scratch_lock`) for its whole run: a second invocation from the same
checkout waits for the first (printing a grep-stable "waiting for it to finish" message) rather than
`remove_dir_all`ing its crates mid-run — same-checkout concurrent runs serialize while the shared
target cache is preserved. Two curated lists, each empirically justified:

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
  non-bug reason (a gem construct gap the fork legitimately supports; e.g. `nested_group`'s `inner`
  rule, a bare top-level GROUP `inner = (a, b)` cddl-codegen serializes as an array but the gem won't
  validate as an instance type). Ledgering is **per (fixture, rule)**, not per fixture: a fixture may
  have one rule the gem can't judge while its *other* rules must still be sound — a divergence on an
  unledgered sibling rule (e.g. `nested_group`'s `outer`) fails the gate. A divergence is *signal*: an
  unledgered one is either a gem gap to record here **with a reason**, or — the class this oracle
  exists to catch — a fork misparse minting spec-violating bytes. **Investigate before ledgering.** A
  ledgered `(fixture, rule)` that stops diverging while still being swept turns the gate RED (stale
  entry), mirroring `EXPECTED_FAIL`.
- **`GEN_SKIP` vs `RUST_ORACLE_SKIP`** — two distinct exclusions. `GEN_SKIP` (e.g. `dsl_custom`) can't
  be generated standalone at all, so it's skipped entirely. `RUST_ORACLE_SKIP` (e.g. `sized_int`) has
  a *rust*-validator gap but generates, round-trips, and dumps fine — so it's generated **without**
  `--emit-tests-conformance` (rust validate half off) yet its minted bytes are **still** swept by the
  ruby gem. A rust-validator blind spot must not cost the decorrelated oracle its coverage.
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
    `nullable` (`Option<T>`), `generic`, `chain`, `extern`, `rawbytes` (a user-supplied
    `RawBytesEncoding` type). This is the
    `is_copy × directly_wasm_exposable × has-a-wrapper-RustStruct` axis the CBOR feature matrix
    deliberately does *not* individuate (wrapper-vs-transparent is a struct-table fact, not a shape fact
    — see the docstrings in `src/intermediate.rs`).
  - **Role**: where the type sits — `array-element`, `map-value`, `map-key`, `struct-field`,
    `struct-field-opt`, `newtype-inner`. Each drives distinct accessor emission (`get`/`add`/`insert`/
    `keys`, by-value vs by-ref). Struct roles use the **array representation** (`[field0: T]`,
    `[pre: uint, ? field0: T]`); map-representation structs (bareword-keyed maps now generate) and
    optional-fields-inside-maps are simply not yet enumerated as shapes here, so they're outside this
    grid's scope for now. A shape may likewise skip a role
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
  real instead of being SKIP-listed like `extern` (whose defs live only in `tests/extern-deps`); it costs no
  extra cargo invocation (same per-cell generate + check). It follows `feature_corpus_compiles`' shared-target-dir *pattern*
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

> Sibling system: `tests/matrix_{supported,panic,reject}/` (projected by `cddl-matrix/project_robustness.ts`,
> driven by `src/tests/robustness_tests.rs`) is the same projection→fixtures→gate shape on a different axis —
> "does a construct *generate*?" rather than "does its wasm *compile*?". Three generation-outcome
> catalogs, one per matrix verdict class: **supported** (`all_supported_constructs_generate` — must
> generate clean), **panic** (`unsupported_construct_panic_catalog` — tracked-known generator panics),
> and **reject** (`unsupported_construct_reject_catalog` — the rows the matrix marks off-limits that mint
> no other test: parse-rejected control ops, generates-but-doesn't-compile shapes like `prelude.any`, and
> out-of-profile constructs). The reject catalog's payoff is catching a parser/codegen change that
> *silently* makes a rejected construct parse — the exact regression a past cddl-fork bump caused for 14
> control ops — as a snapshot diff in the default `cargo test` run instead of only on a manual verify.ts
> sweep; `project_robustness.ts --check` independently pins each reject row's expected label to its matrix
> evidence class, so a re-bless can't quietly launder such a flip.

> Sibling system: `src/tests/identifier_hazard_tests.rs` is the same catalog+gate shape on a
> **NAME-shaped** axis a construct enumeration can never catch — collisions between a user-chosen CDDL
> *name* and the Rust the generator *emits* (the axis IS the name). It sweeps a static hazard table
> (`RUST_KEYWORDS` reused from `parsing.rs`, the emitted generics `r`/`w`, and prelude/std type names
> like `Option`/`Vec`/`Int`) × six name positions (rule name in BOTH emitted type shapes — record
> struct and type-choice enum, since the generic collision is shape-dependent and a struct-only sweep
> would launder enum-shaped `w` as clean — bareword map key, bareword array key, plain group name,
> `@name` directive value). It is a Rust module rather than a `project_robustness.ts`
> projection **on purpose**: the hazard × position table has no matrix verdict upstream to drift from,
> so a TS layer would only copy a constant into fixtures. Two layers: `identifier_hazard_robustness_catalog`
> (default `cargo test` — the `robustness` substring in its name keeps the `cargo insta test --
> snapshot_tests robustness` orphan gate selecting it) snapshots each cell's *generation* outcome
> (`ok` / `error (graceful)` / `PANIC`, a scorecard — a committed `PANIC` is a tracked-known gap, a NEW
> one is a regression); `identifier_hazard_crates_compile` (`#[ignore]`, check.ts full tier) *compiles*
> the `ok` cells — bundling each position's non-pinned hazards into one crate to avoid ~hundreds of
> `cargo check`s, minus a pinned `EXPECTED_COMPILE_FAIL` set (today: `r` in the struct rule-name, enum
> rule-name, and group-name positions; `w` in the enum rule-name position only — struct-shaped `w`
> genuinely compiles) asserted to fail INDIVIDUALLY so the pins flip loudly when the generic-collision
> fix lands.
> A non-pinned bundle that fails to compile is a NEW hazard finding to add to the pin list (with a
> reason) and report — not to paper over by editing the generator.

## Coverage

The in-process snapshot suite alone covers ~81% of the codebase (generation.rs ~86%). To measure
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
`src/generation.rs` in the working tree (observed). The default copied-workdir costs one warm-up
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
`--preserve-encodings` (a pre-existing `unimplemented!` in `generation.rs`), which the corpus
exercises for every entry.

[`insta`]: https://insta.rs
