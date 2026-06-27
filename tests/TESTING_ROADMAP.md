# Testing roadmap — what to do next

A curated, opinionated plan for the *next* testing push, so a future effort has a solid starting
point. This is the short list (what we'd actually do, in order), distilled from an independent
multi-agent review (`draft/testing-recommendations/RECOMMENDATIONS.md` — the exhaustive menu) plus
findings from building the current suite. See `tests/README.md` for how the current setup works and
what's already covered.

## Recommended next steps, in priority order

### Tier 1 — the strategic investment (a focused multi-day effort)

1. **Emit a property round-trip harness into the generated crate.** *(~2–4 days; then free per
   type)* `proptest` + `arbitrary`, behind a flag (e.g. `--emit-tests`). The generator is the only
   thing that knows each type's exact fields/bounds/optionality, so it can emit a better
   `Arbitrary` + round-trip test than any hand-written one, for **every** type automatically. This
   is the single highest-leverage missing oracle — it turns "output didn't change" into "output is
   correct." Do this when you want to go deep on correctness.
   - Follow-on once this exists: **encode-fidelity properties** for `preserve`/`canonical`
     (`bytes → T → bytes` byte-identical over arbitrary valid encodings) — the only at-scale test
     of those high-stakes flags.

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
   churn and the `which` dependency, compiles fast, never bails (`syn` is already a dep). Lower
   urgency only because the pinned toolchain already mitigates churn.

6. **Large-integer boundary in the wasm/JSON path.** *(small)* `integration_tests::wasm_json_roundtrip`
   (wasm-pack + node, `to_json_value()` vs `JSON.parse(to_json())`) is the only oracle that runs
   bindings in a JS engine, but it stays in the JS safe-integer range. A `u64 > 2^53` is the one
   known gap with teeth: `to_json` emits it full-precision, while the JS path loses precision
   (`JSON.parse`) or throws (`json_compatible` doesn't enable bigint). Add a boundary fixture there —
   to pin whichever behaviour is intended and document the limitation. Broadening that test to more
   type shapes or `preserve`/`canonical` profiles is *not* tracked here: the emitted `to_json_value`
   line is flag-independent, so do it only if a divergence actually surfaces.

7. **Output-validate `--json-schema-export` (today it's only compile-checked).** *(small–medium)* Two
   concrete additions, both able to reuse the `wasm_json_roundtrip` node harness:
   - run the json-gen crate and assert each type's `to_json()` output **validates against its own
     emitted schema** — a real correctness oracle for the feature, not just a "does it build" gate;
   - the second shipped script, `json-ts-types.js` (merges the `.d.ts` into the wasm-pack `pkg`
     output), still has zero coverage — it needs a real `pkg/<lib>.d.ts`, so it'd extend the
     `wasm_json_roundtrip` harness (which already runs wasm-pack) rather than the fixture harness
     below. Note it hardcodes the `cddl_lib_wasm` lib name, so cover the default-name case first.

   `run-json2ts.js` (schema → `.d.ts`) is already covered by `integration_tests::js_schema_to_ts`
   (shipped script over `tests/json2ts/` fixtures, pinned `json-schema-to-typescript`), and those
   fixtures cover its live branches — so the only remaining gaps are `json-ts-types.js` and the full
   end-to-end `--package-json` run (json-gen `cargo run` → both scripts → wasm-pack).

   Why it's worth doing: the suite currently proves the json-gen crate *builds*
   (`integration_tests::json`, `feature_corpus_compiles`) but never runs it or inspects what it emits,
   so a schema can be valid Rust yet wrong JSON Schema and nothing notices

## Explicitly not worth it (decided, not overlooked)

- Full `2^N` flag powerset / PICT pairwise — curated named profiles already cover this; revisit
  only if a flag-interaction bug actually slips through.
- `quickcheck` alongside `proptest`; `goldenfile`/`expect-test` as a second corpus engine;
  `no-panic` lints; coverage instrumentation of *generated* code; `trybuild` for whole-crate
  compile-pass (the corpus `cargo check` is simpler and broader).

## Sources
- Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`
- Per-dimension expert write-ups: `draft/testing-recommendations/*.md`
