# Testing roadmap — what to do next

A curated, opinionated plan for the *next* testing push, so a future effort has a solid starting
point. This is the short list (what we'd actually do, in order), distilled from an independent
multi-agent review (`draft/testing-recommendations/RECOMMENDATIONS.md` — the exhaustive menu) plus
findings from building the current suite. See `tests/README.md` for how the current setup works and
what's already covered; completed work is logged in `tests/CLEAR_WINS_PLAN.md`.

**CI policy — feature-frozen.** `.github/workflows/build.yml` accepts no new jobs, steps, gates, or
expanded runs (CI cost; see `AGENTS.md`). No item below may be wired into CI: new verification
systems run manually/locally. The only CI changes accepted are fixes for breakage caused by
refactoring.

## North star — automated feature coverage

The direction all of this points toward: **an automatic flow that ensures every CDDL construct worth
supporting works properly through the generated library — verified by execution, not by hand.**
Concretely: enumerate the feature space (`cddl-matrix/` already does this), then for each construct
*generate → compile → execute* (round-trip real values, reject invalid ones), and treat every
failure as one of two things — a construct we deliberately don't support (documented, not a gap) or a
bug to fix — closing the loop until coverage is complete and self-checking. "Worth supporting" is
load-bearing: some constructs (`any`, `float16`, socket plugs, …) are design decisions to *exclude*,
not holes to grind toward 100%.

The value is in removing the human from that loop without losing trust: the gates must be strict
enough that "the matrix says this feature is supported" *means* it generates, compiles (rust **and**
wasm), and round-trips. The historical verdict meant only "rust generates and rust-`cargo check`s,"
which is why real bugs (inverted `nint` bound, wasm `get`/`add` type mismatch, JSON-schema turbofish)
shipped green. The Tier-1 items below are the remaining missing pieces of that oracle:

- **Correctness** ✅ (depth + default-profile breadth) — the emitted round-trip harness (item 1)
  turns "output didn't change" into "output is right" for every ctor-mintable type, executed in CI
  via `integration_tests::emit_tests_execute`, across the corpus (`feature_corpus_compiles` runs
  `--emit-tests` + `cargo test`), and across the matrix (`verify.ts` is execution-gated, so the
  matrix "supported" verdict itself means "round-trips" for every minting shape). The remaining
  breadth frontier is the preserve/json profiles and the unminted shapes (minter coverage, item 1).
- **Coverage (compile + round-trip)** ✅ — the generated **wasm** crate is compiled by systematic gates
  (`feature_corpus_compiles` `--wasm=true`; the **wasm-ABI matrix** `wasm_matrix_compiles`, 80 enumerated
  cells) AND round-tripped: `--emit-tests --wasm=true` emits a `cddl_generated_wasm_tests` module
  (`src/emit_tests_wasm.rs`, a cross-crate byte differential + accessor read-back) that runs via
  `emit_wasm_tests_execute` (default suite) and per-cell via `wasm_matrix_roundtrips` (manual). See item 2.
  The residue there is the fidelity stubs + the deferred block-expr collection minter shapes.
- **Fail-loud** ✅ — silent-invalid-output (the `rustfmt` swallow) is now a hard error, so malformed
  emission can never again pass as supported (item 7).

## Recommended next steps, in priority order

### Tier 1 — the strategic investments

1. **Grow the emitted round-trip harness toward matrix-driven execution.** The `--emit-tests`
   module emits both halves per type — IR-derived round-trip cases (baseline, bound boundaries,
   one per choice variant, each optional field present; every case asserted byte-identical through
   the full wire cycle AND — outside preserve-encodings — `Debug`-equal to the minted original, since
   byte-identity alone is a fixed point for information-losing projection miscompiles;
   mutation-verified against a constant-writing serializer) and the bounded reject tests — and is executed in CI by
   `integration_tests::emit_tests_execute` (which also floor-asserts the emitted-test counts so
   emission can't silently shrink). Values are minted **deterministically from each type's IR**
   rather than via emitted `proptest`/`Arbitrary` impls, to keep generated crates dependency-free
   and runs byte-reproducible: the repo's determinism ethos would force a fixed seed anyway, and a
   fixed-seed sampler is a deterministic enumerator with extra machinery — both designs share the
   identical per-IR-shape derivation surface (`emit_tests.rs`), which is the single maintained
   thing; unmintable shapes are skipped with an `eprintln!`, never silently. Validated at landing
   by a full corpus sweep plus the preserve-profile integration gate, which immediately caught
   (and led to fixing) two miscompiles that snapshots + compile gates had blessed: the `.ne`
   unsatisfiable-bound window, and preserve-encodings map records writing default-valued fields
   the header didn't count (corrupt CBOR on any freshly-constructed defaulted value). Remaining:
   - **c6 — matrix-driven execution (the F3 frontier): profiles remaining.** Both halves of the
     flip are in: `feature_corpus_compiles` runs the default profile with `--emit-tests` +
     `cargo test` (CI), and `verify.ts`'s per-feature / per-cell / per-control-op probes are
     execution-gated (manual, per the CI freeze) — the matrix "supported" verdict means
     "round-trips" wherever the type mints a test surface, with a per-probe `minted` bit so
     unminted shapes are labeled "no minted round-trip surface" rather than credited. Remaining:
     the preserve/json profiles once their emitted-test surface is validated at corpus breadth
     (verify.ts probes default flags only — the "supported is silently a default-profile fact"
     decision below).
   - **Encode-fidelity follow-on** for `preserve`/`canonical` (`bytes → T → bytes` byte-identical
     over irregular encodings). Spec-anchored *known-answer* vectors now exist
     (`tests/golden_hex_preserve/`, `tests/golden_hex_canonical/` — hand-derived RFC 8949
     indefinite-length / non-minimal-`Sz` literals, independent of the `cbor_event` helpers); the
     remaining frontier is minted *encodings* at scale (not just hand-picked ones), the only
     at-scale test of those high-stakes flags. (The hand-picked KAT axis itself is closed: value
     anchors, nint vectors, the runtime table-sort pins, and the cross-major `-1`/`256`
     length-first-vs-bytewise discriminator all live in the sibling suites, each
     mutation-verified red.)
   - **Minter coverage:** a standalone mint surface for transparent bounded/newtype-able aliases is
     the highest-leverage remaining minter extension. From the verify.ts flip, a large slice of the
     ~70 supported matrix probes mint nothing standalone — transparent aliases (`x = uint`,
     `x = [* uint]`, tables, bounded aliases like `g = [2*5 uint]`) plus pure c-enums — so their
     execution verdict falls back to compile. A standalone mint would also execute the standalone
     bounds class the `nint` ledger entry tracks (the construct-reject on named bounded-`nint`
     newtypes is already emitted; the residual is the *transparent-alias* bounded surface, which has
     no named struct to hang a `new()`/round-trip on).
   - **Decorrelated conformance parser.** The IR-bug conformance oracle (`--emit-tests-conformance`)
     shares the dcSpark `cddl` fork's *parser* with the generator, so a fork-level misparse escapes it
     (it catches wrong *values*, not misparses) — the same limitation as `deser_test_conformance.rs`.
     Decorrelating the parser (an anweiss rev, the ruby `cddl` gem already wired into
     `cddl-matrix/verify.ts`, or a ciborium structural differential) would harden both oracles at once.
     Same family, still unrecorded elsewhere: a DECODE-side reference-codec differential (our bytes →
     ciborium/minicbor decode → compare structure — the conformance oracle only exercises ciborium via
     the `cddl` crate's validator, and minicbor is used nowhere).

2. **Compile the generated *wasm* crate in the systematic gates.** *(compile foundation built + CI-wired;
   the remaining frontiers are behavioural (round-trip) coverage and the red-cell backlog below.)* The wasm
   bindings are a wasm-ABI concern (`is_copy` × `directly_wasm_exposable` × the boundary-op tables in
   `intermediate.rs`) that the CBOR-serialization feature axis doesn't individuate, so wasm needs its own
   systematic gate: the rust crate can type-check while the generated wasm crate does not (wrong accessor
   return type, bad boundary `.into()`/`.clone()`/by-ref slips, dangling map typedefs).
   - **Foundation (context for the frontiers below).** `feature_corpus_compiles` and the wasm-ABI matrix
     (`wasm_matrix_compiles`, an enumerated `{type-shape × role}` grid) both generate `--wasm=true` and
     `cargo check` the wasm crate on the host target (no `wasm32`/`wasm-pack`; a shared `CARGO_TARGET_DIR`
     amortizes deps). Coverage is by-construction over the enumerated grid; system doc: `tests/README.md`
     § "wasm-ABI matrix". The frontier is now behavioural (round-trip) coverage, not the compile backlog.
   - **Compile backlog: cleared.** Every enumerated cell compiles; the only `SKIP` left in
     `wasm_matrix_compiles` is the permanent `extern__array-element` (user-supplied type, can't compile
     standalone). A red cell reappearing is a regression to fix, not a backlog item. The wrapper-vs-transparent
     fact — the recurring wasm-boundary bug class — now has one source of truth,
     `IntermediateTypes::has_wasm_wrapper(ident)`; route new naming / boundary / exposability decisions
     through it (see `cddl-matrix/ROADMAP.md` § "wasm-ABI matrix — remaining work").
   - **Extending the grid.** Coverage equals the hand-curated shape axis (`SHAPES`); a representation not in
     it is a silent hole, not a red cell — periodically audit for un-enumerated shapes and add them.
     One known hole: `_CDDL_CODEGEN_RAW_BYTES_TYPE_` has no SHAPES entry, and neither raw-bytes fixture
     ships a `tests_wasm.rs` — the raw-bytes wasm boundary is compile-only via `external_wasm_raw_bytes_def`
     (a `rawbytes` shape would gate the compile half systematically; a `tests_wasm.rs` in `tests/raw-bytes/`
     the behavioral half).
   - **Behavioural frontier (compile → round-trip) — landed; residue below.** The compile gate can be
     green while a cell emits a semantically wrong same-type conversion at the wasm boundary (an identity
     `.into()` where a transform was needed). That is now caught: `--emit-tests --wasm=true` emits a
     `cddl_generated_wasm_tests` module (`src/emit_tests_wasm.rs`) that renders each minted value through
     BOTH the wasm wrapper API and the `cddl_lib::` rust API and asserts a cross-crate byte differential +
     accessor read-back — a second renderer over the same `emit_tests::MintValue` derivation surface (no
     duplicate minter). It runs in the default suite (`integration_tests::emit_wasm_tests_execute`, the
     `core` fixture) and per grid cell via `integration_tests::wasm_matrix_roundtrips` (manual, `#[ignore]`d
     under the CI freeze). The first per-cell run paid off immediately: it caught `coll__struct-field`
     (a `[* uint]` wrapper list used as a struct field) minting a bare `vec![..]` against a `&Nums` wasm
     ctor — the emitter shallow-resolved the alias past its wrapper — fixed in `emit_tests_wasm::wasm_arg`
     to loud-skip via the UNRESOLVED type's `directly_wasm_exposable`. Residue:
     - **Deferred wasm minter shapes (loud skips today).** A wrapper/collection ctor arg needs a block-expr
       build (`{ let mut l = FooList::new(); l.add(&e); l }`) and a `@newtype`/tag wrapper ctor arg has no
       wasm `new`; both are currently `eprintln!` skips, so a cell built only from them mints no wasm surface
       and falls back to the compile verdict. A block-expr collection minter is the highest-leverage
       extension — it would give the corpus- and matrix-breadth wasm round-trip gates (below) a mint
       surface for the collection/wrapper cells they currently fall back to compile on.
       Corpus- and matrix-breadth wasm round-trip coverage is otherwise wired: `feature_corpus_compiles`
       `cargo test`s the wasm crate under the default profile (not just `cargo check`), and `verify.ts`
       runs the wasm oracle by default (opt out with `--no-wasm` / `VERIFY_WASM=0`).
     - **Two pre-existing `core`-fixture findings the wasm gate has to route around** (surfaced building the
       wasm execution gate; neither is a wasm bug, both are latent rust-side sharp edges). (1) `core`'s
       hand-written `tests::docs` / `tests::no_alias` source-inspection tests read `lib.rs` and truncate at
       the FIRST `#[cfg(test)]` — brittle now that an emitted module can precede them; a delimiter that
       anchors on the specific hand-written module would be robust. (2) `core`'s `TypeChoice` is wire-ambiguous
       (uint `0` collides with the fixed `i0` variant), so the rust `--emit-tests` value-equality oracle
       false-fails on it. Both are why `emit_wasm_tests_execute` `cargo test`s only the *wasm* crate (which
       builds the rust crate as a non-test dep). The wasm oracle sidesteps (2) by reading accessors on the
       freshly-built value, not the post-wire one.
     - **Known semantic-fidelity gaps** (tracked by `#[ignore]`'d failing tests in `integration_tests.rs`;
       remove `#[ignore]` + write the real assertion when the harness or a fidelity fix lands). wasm-bindgen
       can't represent nested `Option<Option<T>>`, so a nullable value (`T / null` → `Option<T>`) at a
       position that adds its own presence-`Option` is flattened to a single `Option<T>` — the wasm READ
       conflates "absent" with "present-but-null". Native types keep all three states, so CBOR round-trips
       are unaffected; this is purely a wasm read-side loss. The ideal is an unambiguous getter (a presence
       accessor, or exposing the nullable as `Option<wrapper-struct>`, which wasm-bindgen supports on the
       return side). Cases, worst first:
       - **Optional-nullable struct field** (`? field0: (T / null)`) — UNRECOVERABLE (no presence accessor).
         `wasm_optional_nullable_field_three_state_fidelity`.
       - **Double-nested enum variant** — `add_wasm_enum_getters` silently *skips* the `as_variant()` getter
         (build-time `println!` only), so the value is unreadable from wasm.
         `wasm_enum_nullable_variant_three_state_fidelity`.
       - **Recoverable-but-ambiguous** (getter alone is lossy; a second call disambiguates): map `get` (via
         `keys()`), enum single-nested `as_variant()` (via `kind()`) — same flatten, lower priority.
       All flatten via the convention `add_wasm_enum_getters` established ("a bit ambiguous but better than
       nothing"); the round-trip verdict upgrade (this item) is what would catch them mechanically.
   - **CI cost to watch.** Two gates shell nested cargo per cell (`feature_corpus_compiles`;
     `wasm_matrix_compiles`, growing with the extensible axis). The shared `CARGO_TARGET_DIR` amortizes deps;
     if wall-time bites, batch cells into fewer crates, adopt `cargo-nextest`, or gate only changed cells.
   - **Scope note:** the host `cargo check` catches type/signature errors cheaply; `#[wasm_bindgen]`
     macro-expansion / `.d.ts` / JS-surface concerns still need `wasm-pack` and stay the job of the few
     `run_test` fixtures + item 7's `--package-json` run.

### Tier 2 — project-specific, highest signal (when data sourcing is feasible)

3. **Real on-chain CBOR corpus + differential vs the sibling libraries.** *(medium)* This is the
   Cardano `cddl-codegen`; real blocks/txs are the richest fuzz seeds and the existing hand-written
   sibling libs make cross-implementation differential testing nearly free. Higher signal than any
   synthetic data.

4. **Wire-format back-compat + generated-API semver.** *(medium each)* For a serialization tool
   these are correctness contracts the snapshot suite doesn't cover: (a) bytes written by an older
   generator still deserialize under newer generated code (pin a few `(spec, bytes)` vectors);
   (b) `cargo-semver-checks` against a golden generated crate so a renamed field / changed
   signature surfaces as a reviewed API break.

### Tier 3 — validation & process (opportunistic)

5. **`cargo-mutants`** scoped to the emit core (`--file 'src/generation*'`), run manually/locally
   (CI is feature-frozen). The defining technique for a codegen tool — proves the suite *catches*
   wrong-codegen bugs, not just covers lines. Only bites once #1 gives assertions with teeth; needs
   `cargo-nextest` as the runner.
   **Score mutants against the behavioral layers only** (a nextest filter excluding
   `snapshot_tests`): nearly every emit-core mutant changes output text, so under the full suite it
   is trivially "killed" by a snapshot mismatch and the score measures snapshot *sensitivity* —
   which says nothing about whether a human-blessed wrong emission would be caught (the failure
   mode that actually ships). Scored behaviorally, the survivor list is a direct map of emit logic
   no behavioral oracle observes.

6. **`prettyplease` instead of shelling to `rustfmt`.** Removes toolchain-dependent formatting
   churn and the `which` dependency, compiles fast, never bails (it reuses `syn`, already built
   transitively via the proc-macro derives). Lower urgency only because the pinned toolchain already
   mitigates churn.

7. **Output-validate `--json-schema-export` end-to-end.** *(small)* The in-process schema-vs-serde
   check, the `.d.ts` merge, and json-gen's `export_schemas()` execution are covered for scalar
   newtypes (`integration_tests::json` / `json_preserve`), and now for generic-backed newtypes too:
   the turbofish bug — a wrapper over a map/array `@newtype` emitted `T<..>::json_schema` in
   expression position → non-compiling crate — is **fixed** (qualified-path `<T as
   schemars::JsonSchema>::` emission in `generate_wrapper_struct`) and gated by
   `tests/corpus/newtype_generic.cddl` under the `json` profile of `feature_corpus_compiles`.
   The silent-invalid-output hole that let the turbofish ship green — `rustfmt_generated_string`
   swallowing a parse/internal failure and returning the unformatted source at exit 0 — is now **fixed
   too** (fail-loud: propagates `Err`; `Some(3)` "unformatted-but-valid" still `Ok`), so any future
   non-parsing emission is a hard generator error, guarded by
   `snapshot_tests::rustfmt_rejects_unparseable_source` (the north star's "fail-loud" pillar).
   **Still open:** the full `--package-json` run (json-gen `cargo run` → both scripts → wasm-pack).
   `run-json2ts.js` stays covered by `integration_tests::js_schema_to_ts`.

8. **Grammar-fuzz the corpus.** Generate random *valid* CDDL and run it through the generator to
   surface coverage holes and crashes the hand-picked fixtures miss. Laziest source first: recombine
   the matrix's `containment/*.toml` examples (they already enumerate which construct nests in which
   role legally); escalate to an `arbitrary`-derived "supported-CDDL" AST only if that plateaus. Treat
   the fuzzer as a *corpus generator*, not a CI gate — seed it for determinism, then promote any new
   divergence/crash into the snapshot corpus (review once, commit). Complements the real on-chain
   differential (item 3): synthetic breadth vs real-world depth.
   - **Existing `fuzz/` rot residuals (the harness is manual-only under the CI freeze).** The seed
     corpus now harvests all three golden-hex suites (default + preserve + canonical — done in
     `generate.sh`), but two gaps remain: the 22-type probe list in `fuzz_targets/from_cbor_bytes.rs`
     is hand-synced against the generated crate's `impl Deserialize` set with no drift guard (a new
     `input.cddl` rule is silently unfuzzed) — derive or assert it from that set in `generate.sh`; and
     the fuzz crate has no compile-rot check despite a ~5s stable-toolchain `cargo check` (a probe
     rename or scrape-regex break silently rots the repo's sole OOM/stack-overflow oracle) — add it as
     a documented manual/local step, not a workflow job.
   - **Recursive-spec stack-overflow fuzzing (now reachable).** `fuzz/generate.sh` fuzzes only the
     non-recursive preserve spec, so its stack-overflow oracle is structurally blind to unbounded
     recursion. Now that a terminable recursive corpus fixture exists (`tests/corpus/recursive.cddl`)
     and `--deserialize-depth-limit` gives it a *graceful* failure mode, add a recursive spec to the
     fuzz seed set in both configurations: unguarded (the fuzzer should reproduce the SIGABRT the
     depth-limit flag documents and guards) and with `--deserialize-depth-limit` set (a hostile-deep
     input must return `DeserializeFailure::DepthLimitExceeded`, never abort). The
     `deserialize_depth_limit_guards_recursion` integration test pins the graceful path for one
     hand-built hostile vector; the fuzzer is what exercises it at breadth.
   - **`corpus_detect` dsl-prose residual (LOW).** On a directive-leading comment line the detector
     credits every later `@word` via `matchAll`, which doesn't mirror `comment_ast`'s sequential parse
     (`@doc` prose runs to the next `@`; other directives stop at the first non-directive token) — a
     real directive id buried in trailing prose after a leading directive could keep a dsl cover green.
     No current fixture triggers it; the fix must replicate that asymmetric `@doc` grammar, not a naive
     stop-at-first rule.

## Pending decisions (maintainer call — blocks the related test, not on effort)

Surfaced during the clear-wins sweep; each is gated on a behaviour/policy call. Full context + the
rest of the deferred menu (incl. medium next-tasks like a preserve-encodings golden known-answer set
and assertion upgrades needing value choices) live in `tests/CLEAR_WINS_PLAN.md`.

- **MSRV / OS matrix for GENERATED code.** Generated crates pin no MSRV and are only ever built on
  the dev/CI platform; whether to declare + test a minimum rustc (and a second OS) for generated
  output is a policy call. Do-or-decline; currently in no living doc, recorded here so it's a
  decision rather than an oversight.
- **`example/` specs + docs-vs-behavior conformance.** `example/` is consumed by zero tests, and
  `comment_dsl.mdx` / `output_format.mdx` have no behavioral conformance check (comment_dsl has a
  name-level forward lint in `cddl-matrix/verify.ts` only). Decide: wire `example/` into a
  compile-gate and spot-check the documented output claims, or drop/trim `example/`.
- **Snapshot-only corpus policy.** A ~5-line `feature_corpus_compiles` skip-list would unblock
  *snapshot* coverage of the extern (`_CDDL_CODEGEN_EXTERN_TYPE_`) and raw-bytes
  (`_CDDL_CODEGEN_RAW_BYTES_TYPE_`) emit paths — they emit undefined user-supplied types so they
  can't `cargo check` standalone (only compile-tested today via `tests/extern-deps` / `tests/raw-bytes`).
  Introduces a "snapshot-only corpus" concept the docs don't have yet.
- **Re-bless-snapshot coverage gaps** (each needs a fixture/profile change + snapshot re-bless):
  float under the `json` profile; `OrderedHashMap` JSON **serde** (a map-bearing json fixture —
  `tests/json/input.cddl` has none; the `JsonSchema` half is now compile-gated by
  `tests/corpus/newtype_generic.cddl`, item 7 — this remaining serde half is a genuine snapshot
  chore); re-enabling `bool_wrapper` JSON newtype (blocked on generator issue #223).
- **"Supported" is silently a default-profile fact.** verify.ts probes default flags only, and the
  supported-catalog gate (`all_supported_constructs_generate`) runs the default profile — so a
  supported-construct × preserve/json failure is caught only where a corpus fixture isolates that
  construct, and flag-specific failures are a proven class (floats hit `unimplemented!` under
  preserve). Cheapest: run the supported catalog under all three profiles with a small per-profile
  expected-fail list; longer-term, a profile axis on the matrix annotation schema.
- **Tag-drop residue for exotic single-type tag inners (auto-wrap coverage gap).** Top-level tag rules
  auto-wrap into a tag-writing/tag-checking newtype whenever the inner is a primitive/named type
  (`tagged = #6.42(text)`) or a `bytes .cbor T` wrapper (`#6.20(bytes .cbor foo)`) — closing the
  standalone-API tag-drop for every shape the corpus and standard prelude exercise. Three untested,
  exotic inner shapes still fall through to a tag-carrying `pub type` alias whose standalone
  `to/from_cbor_bytes` drops the tag: a `.default`-carrying inner (`#6.n(uint .default 5)`), a range that
  collapses exactly onto a Rust primitive with no residual bound (a ranged inner that *does* carry a bound
  already wraps), and a bare literal inner (`#6.n(5)` — and top-level fixed-value types are a separate
  documented limitation anyway). None appear in any fixture; the cheapest close is to add a corpus fixture
  isolating each and route its `parse_type` arm through `new_wrapper` (mirroring the primitive/`.cbor`
  arms). Parenthesized *non-tag* aliases (`basic = (uint)`) are intentionally left transparent — parens
  have no CBOR effect, so `@newtype` stays the opt-in there.

## Explicitly not worth it (decided, not overlooked)

- Full `2^N` flag powerset / PICT pairwise — the curated named profiles cover the flag
  *combinations* worth testing; revisit only if a flag-interaction bug actually slips through.
  Every individual flag *value* now appears in some profile or test: the five that previously
  didn't are covered by `flag_value_smoke` (`--annotate-fields=false`,
  `--to-from-bytes-methods=false`, `--binary-wrappers=true`), `wasm_cbor_json_api_macro_compiles`
  (`--wasm-cbor-json-api-macro`), and — for `--canonical-form=true` without `--preserve-encodings`,
  which emitted a non-compiling crate — a CLI rejection (`api::with_types`, pinned by
  `flag_value_rejects_canonical_without_preserve`).
- `quickcheck` alongside `proptest`; `goldenfile`/`expect-test` as a second corpus engine;
  `no-panic` lints; coverage instrumentation of *generated* code; `trybuild` for whole-crate
  compile-pass (the corpus `cargo check` is simpler and broader).
- An orphan-fixture-directory meta-test (assert every `tests/<dir>/` is referenced by some gate):
  fixture dirs change rarely and a new gate's author touches the dir listing anyway; the failure
  mode (a committed fixture nothing runs) is caught by review at that rate.

## Sources
- Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`
- Per-dimension expert write-ups: `draft/testing-recommendations/*.md`
