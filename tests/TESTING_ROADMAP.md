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

7. **Output-validate `--json-schema-export` (today it's only compile-checked).** *(small–medium)*
   - ✅ *Done.* `integration_tests::json` / `json_preserve` now run
     `tests::schemas_validate_serialization`: every exported type's serde output is validated against
     `schema_for!(T)` (the exact schema json-gen ships) via the `jsonschema` crate. Done in-process
     in Rust rather than the suggested node harness — the emitted schema is literally
     `to_string_pretty(schema_for!(T))` and `to_json()` is `serde_json`, so the Rust check tests the
     same contract (schemars impl vs serde impl agree) without wasm-pack/node/ajv.
   - ✅ *Done.* `json-ts-types.js` (merges the `.d.ts` into the wasm-pack `pkg` output) is covered by
     `integration_tests::js_d_ts_merge` — fixture-based, default `cddl_lib_wasm` lib name, no
     wasm-pack needed (it's pure string-munging). This surfaced and fixed a **live bug**: the script
     keyed off `to_js_value`, but generated output renamed that method to `to_json_value` long ago, so
     the return-type substitution had silently been a no-op.
   - **Still open:** the full end-to-end `--package-json` run (json-gen `cargo run` → both scripts →
     wasm-pack), and running the json-gen crate's own `export_schemas()` (the in-Rust check above
     bypasses it). `run-json2ts.js` stays covered by `integration_tests::js_schema_to_ts`.

8. ✅ **Negative-path / rejection testing.** *Done* (the structural slice). `tests::structural_rejects`
   in `tests/core/tests.rs` pins that malformed CBOR is **rejected**: empty input, too-short array,
   wrong element type, wrong/missing tag — each with an `is_ok()` baseline so a reject can't pass for
   the wrong reason. (Numeric/size-bound rejection was already covered by the `bounds` test.) Room to
   grow if wanted: missing required map key, wrong major type per primitive, indefinite-vs-definite
   mismatches — but the high-value structural shapes are now pinned.

## Explicitly not worth it (decided, not overlooked)

- Full `2^N` flag powerset / PICT pairwise — curated named profiles already cover this; revisit
  only if a flag-interaction bug actually slips through.
- `quickcheck` alongside `proptest`; `goldenfile`/`expect-test` as a second corpus engine;
  `no-panic` lints; coverage instrumentation of *generated* code; `trybuild` for whole-crate
  compile-pass (the corpus `cargo check` is simpler and broader).

## Sources
- Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`
- Per-dimension expert write-ups: `draft/testing-recommendations/*.md`
