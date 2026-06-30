# Testing roadmap — what to do next

A curated, opinionated plan for the *next* testing push, so a future effort has a solid starting
point. This is the short list (what we'd actually do, in order), distilled from an independent
multi-agent review (`draft/testing-recommendations/RECOMMENDATIONS.md` — the exhaustive menu) plus
findings from building the current suite. See `tests/README.md` for how the current setup works and
what's already covered; completed work is logged in `tests/CLEAR_WINS_PLAN.md`.

## Recommended next steps, in priority order

### Tier 1 — the strategic investment (a focused multi-day effort)

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
     real **generator bug** — the bounded-`nint` constructor bound is inverted (findings ledger in
     `cddl-matrix/ROADMAP.md`).
     - **Still open:** the round-trip/`Arbitrary` half + proptest dep; then the `verify.ts`
       `cargo check → cargo test` flip; an integration test that runs the emitted module (today the emitter
       has no CI coverage); optionally a `nint` reject case (which would also catch the inverted-bound bug).

### Tier 2 — project-specific, highest signal (when data sourcing is feasible)

2. **Real on-chain CBOR corpus + differential vs the sibling libraries.** *(medium)* This is the
   Cardano `cddl-codegen`; real blocks/txs are the richest fuzz seeds and the existing hand-written
   sibling libs make cross-implementation differential testing nearly free. Higher signal than any
   synthetic data.

3. **Wire-format back-compat + generated-API semver.** *(medium each)* For a serialization tool
   these are correctness contracts the snapshot suite doesn't cover: (a) bytes written by an older
   generator still deserialize under newer generated code (pin a few `(spec, bytes)` vectors);
   (b) `cargo-semver-checks` against a golden generated crate so a renamed field / changed
   signature surfaces as a reviewed API break.

### Tier 3 — validation & process (opportunistic)

4. **`cargo-mutants`** scoped to the emit core (`--file 'src/generation*'`), nightly + `--in-diff`
   on PRs. The defining technique for a codegen tool — proves the suite *catches* wrong-codegen
   bugs, not just covers lines. Only bites once #1 gives assertions with teeth; needs
   `cargo-nextest` as the runner.

5. **`prettyplease` instead of shelling to `rustfmt`.** Removes toolchain-dependent formatting
   churn and the `which` dependency, compiles fast, never bails (it reuses `syn`, already built
   transitively via the proc-macro derives). Lower urgency only because the pinned toolchain already
   mitigates churn.

6. **Output-validate `--json-schema-export` end-to-end.** *(small)* The in-process schema-vs-serde
   check, the `.d.ts` merge, and json-gen's `export_schemas()` execution are all covered now
   (`integration_tests::json` / `json_preserve`). **Still open:** the full `--package-json` run
   (json-gen `cargo run` → both scripts → wasm-pack). `run-json2ts.js` stays covered by
   `integration_tests::js_schema_to_ts`.

7. **Grammar-fuzz the corpus.** Generate random *valid* CDDL and run it through the generator to
   surface coverage holes and crashes the hand-picked fixtures miss. Laziest source first: recombine
   the matrix's `containment/*.toml` examples (they already enumerate which construct nests in which
   role legally); escalate to an `arbitrary`-derived "supported-CDDL" AST only if that plateaus. Treat
   the fuzzer as a *corpus generator*, not a CI gate — seed it for determinism, then promote any new
   divergence/crash into the snapshot corpus (review once, commit). Complements the real on-chain
   differential (item 2): synthetic breadth vs real-world depth.

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
  float under the `json` profile; `OrderedHashMap` JSON serde/`JsonSchema` (needs a map-bearing json
  fixture — `tests/json/input.cddl` has none); re-enabling `bool_wrapper` JSON newtype (blocked on
  generator issue #223).

## Explicitly not worth it (decided, not overlooked)

- Full `2^N` flag powerset / PICT pairwise — curated named profiles already cover this; revisit
  only if a flag-interaction bug actually slips through.
- `quickcheck` alongside `proptest`; `goldenfile`/`expect-test` as a second corpus engine;
  `no-panic` lints; coverage instrumentation of *generated* code; `trybuild` for whole-crate
  compile-pass (the corpus `cargo check` is simpler and broader).

## Sources
- Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`
- Per-dimension expert write-ups: `draft/testing-recommendations/*.md`
