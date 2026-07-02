# Testing roadmap — what to do next

A curated, opinionated plan for the *next* testing push, so a future effort has a solid starting
point. This is the short list (what we'd actually do, in order), distilled from an independent
multi-agent review (`draft/testing-recommendations/RECOMMENDATIONS.md` — the exhaustive menu) plus
findings from building the current suite. See `tests/README.md` for how the current setup works and
what's already covered; completed work is logged in `tests/CLEAR_WINS_PLAN.md`.

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

- **Correctness** — the round-trip harness (item 1): turns "output didn't change" into "output is right."
  **Still open** (the single highest-leverage missing oracle).
- **Coverage (compile)** ✅ — the generated **wasm** crate is now compiled by systematic gates:
  `feature_corpus_compiles` (`--wasm=true`, checks both crates) and the **wasm-ABI matrix**
  (`wasm_matrix_compiles`, 80 enumerated cells). See item 2. The remaining coverage frontier is
  *behavioural* (compile → round-trip), which depends on item 1.
- **Fail-loud** ✅ — silent-invalid-output (the `rustfmt` swallow) is now a hard error, so malformed
  emission can never again pass as supported (item 7).

## Recommended next steps, in priority order

### Tier 1 — the strategic investments

1. **Emit a property round-trip harness into the generated crate.** *(~2–4 days; then free per
   type)* `proptest` + `arbitrary`, behind the `--emit-tests` flag (the flag itself already exists —
   added by the c6 reject half; this item adds the `Arbitrary` + proptest dep on top). The generator is
   the only thing that knows each type's exact fields/bounds/optionality, so it can emit a better
   `Arbitrary` + round-trip test than any hand-written one, for **every** type automatically. This
   is the single highest-leverage missing oracle — it turns "output didn't change" into "output is
   correct." Do this when you want to go deep on correctness.
   - Follow-on once this exists: **encode-fidelity properties** for `preserve`/`canonical`
     (`bytes → T → bytes` byte-identical over arbitrary valid encodings) — the only at-scale test
     of those high-stakes flags.
   - **c6 — matrix-driven execution (the F3 frontier).** The end goal: flip `cddl-matrix/verify.ts`'s
     per-schema `cargo check` → `cargo test` to execute round-trip + reject across the whole feature corpus
     (~3–4× the breadth of hand fixtures). The matrix is the **driver, not the oracle** — it mints no typed
     value, so the round-trip half needs a constraint-aware `Arbitrary` (must honor the generator's own
     `RangeCheck` or it flakes) living in the *generated* crate, plus the proptest dep. The **reject half
     already landed** (`--emit-tests`; `src/emit_tests.rs` — per-bounded-type accept/reject asserts minted
     from compile-time literals, off by default, not yet wired into the suite or `verify.ts`). It surfaced a
     real **generator bug** — the bounded-`nint` constructor bound was inverted, now **fixed** (findings
     ledger in `cddl-matrix/ROADMAP.md`).
     - **Still open:** the round-trip/`Arbitrary` half + proptest dep; then the `verify.ts`
       `cargo check → cargo test` flip; an integration test that runs the emitted module (today the emitter
       has no CI coverage); optionally an `--emit-tests` `nint` construct-reject case (the inverted-bound bug
       it targeted is now fixed directly; a construct-reject would guard against regression + still fails on
       the *standalone* bounded-`nint`-newtype bug the fix didn't cover — see the ledger).
       This flip shares the `verify.ts` / `feature_corpus_compiles` surface with item 2's wasm gate — land
       the two together.

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
   - **Behavioural frontier (compile → round-trip).** The verdict is *compile* only, so a cell can be green
     while emitting a semantically wrong same-type conversion (an identity `.into()` where a transform was
     needed). Upgrade the verdict compile → round-trip once item 1's harness lands (depends on item 1).
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

5. **`cargo-mutants`** scoped to the emit core (`--file 'src/generation*'`), nightly + `--in-diff`
   on PRs. The defining technique for a codegen tool — proves the suite *catches* wrong-codegen
   bugs, not just covers lines. Only bites once #1 gives assertions with teeth; needs
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

## Pending decisions (maintainer call — blocks the related test, not on effort)

Surfaced during the clear-wins sweep; each is gated on a behaviour/policy call. Full context + the
rest of the deferred menu (incl. medium next-tasks like a preserve-encodings golden known-answer set
and assertion upgrades needing value choices) live in `tests/CLEAR_WINS_PLAN.md`.

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
- **cddl-crate conformance oracle** (RECOMMENDATIONS #9 — accept or reject explicitly; it is in
  neither the do-list nor the not-worth-it list today). The `cddl` crate is already a dependency,
  so validating every serialized value against the source `.cddl`
  (`validate_cbor_from_slice(spec, bytes)`) inside the fixtures' `deser_test` helpers would give
  every existing and future round-trip a second, *independent* oracle nearly for free — it fails
  on exactly the enforcement class (occurrence counts, range bounds) that generator+test shared
  assumptions can't catch. Caveats to weigh: the rust validator has known gaps (a fail is a strong
  signal, a pass is weak), and the ruby `cddl` gem (already an oracle in `cddl-matrix/verify.ts`)
  could serve the same role out-of-process at higher cost.
- **Five documented flag values with zero coverage at any oracle level** — no test generates with
  `--annotate-fields=false` (selects a whole different deserialization/error-emission mode, 13+
  branch sites in generation.rs incl. the code's own acknowledged-tricky preserve-encodings
  interaction), `--to-from-bytes-methods=false`, `--binary-wrappers=true`,
  `--wasm-cbor-json-api-macro` (the documented CML invocation path — unlike its two
  snapshot-tested sibling macro flags), or `--canonical-form=true` without `--preserve-encodings`
  (CLI-accepted; docs call standalone use legal). Cheapest acceptance: a generate + `cargo check`
  smoke over `tests/core/input.cddl` per value — if a value doesn't even generate, that answer
  itself converts it into either a CLI rejection or a ledgered gap. Alternative: deprecate/reject
  the untestable values so the "profiles cover the flag space" premise (below) becomes true.
- **"Supported" is silently a default-profile fact.** verify.ts probes default flags only, and the
  supported-catalog gate (`all_supported_constructs_generate`) runs the default profile — so a
  supported-construct × preserve/json failure is caught only where a corpus fixture isolates that
  construct, and flag-specific failures are a proven class (floats hit `unimplemented!` under
  preserve). Cheapest: run the supported catalog under all three profiles with a small per-profile
  expected-fail list; longer-term, a profile axis on the matrix annotation schema.
- **Adversarial CBOR bytes into generated deserializers** (RECOMMENDATIONS #8 + #5 — accept or
  reject explicitly; currently in neither list). The robustness layer covers malformed CDDL into
  the *generator*; hostile bytes into `from_cbor_bytes` — the untrusted input this library's
  consumers actually parse (chain data) — have no systematic coverage, and item 1 will not add any
  (it generates valid values by construction). A panic (vs `Err`), over-allocation, or stack
  overflow on deep nesting in generated code or the `static/` runtime is a DoS in every consumer
  and is invisible to every current and planned layer. Cheapest acceptance: one `cargo-fuzz`
  target over a rich generated crate's `from_cbor_bytes`, seeded from the golden vectors, plus a
  committed crash-replay test; note `catch_unwind`-based harnesses cannot observe stack overflow,
  so the fuzz process boundary is the only oracle for that case.

## Explicitly not worth it (decided, not overlooked)

- Full `2^N` flag powerset / PICT pairwise — the curated named profiles cover the flag
  *combinations* worth testing; revisit only if a flag-interaction bug actually slips through.
  This holds only while every individual flag *value* appears in some profile or test — five
  currently appear in none (see the pending decision above), so the combination dismissal must not
  be read as covering them.
- `quickcheck` alongside `proptest`; `goldenfile`/`expect-test` as a second corpus engine;
  `no-panic` lints; coverage instrumentation of *generated* code; `trybuild` for whole-crate
  compile-pass (the corpus `cargo check` is simpler and broader).

## Sources
- Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`
- Per-dimension expert write-ups: `draft/testing-recommendations/*.md`
