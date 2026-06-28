# Testing roadmap — what to do next

A curated, opinionated plan for the *next* testing push, so a future effort has a solid starting
point. This is the short list (what we'd actually do, in order), distilled from an independent
multi-agent review (`draft/testing-recommendations/RECOMMENDATIONS.md` — the exhaustive menu) plus
findings from building the current suite. See `tests/README.md` for how the current setup works and
what's already covered.

> **Recently completed (clear-wins sweep — full per-task log + the complete deferred menu live in
> `tests/CLEAR_WINS_PLAN.md`):** corpus coverage for sized/bounded ints, bool, and fixed/constant
> fields; the full negative-path structural slice (item 8); the JS safe-integer boundary (item 6,
> safe half); json-gen now actually *runs* `export_schemas()` (item 7 residual); `error.rs` Display
> and `RawBytesEncoding` hex coverage; plus several dead/weak tests fixed. What's left below is the
> bigger-ticket work plus a few maintainer decisions (new "Pending decisions" section).

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

6. ✅ **Large-integer boundary in the wasm/JSON path.** *Done.* `wasm_json_roundtrip`
   (`tests/wasm_json/roundtrip.mjs`) pins both halves: the `u64 = 2^53-1` round-trip just below the JS
   safe-integer cliff, and the `u64 > 2^53` contract above it — `to_json()` stays lossless,
   `to_json_value()` *fails loud* (throws rather than silently rounding), and `JSON.parse(to_json())`
   is lossy by JS definition. The blessed contract (keep fail-loud; bigint would break
   `JSON.stringify` and retype every int field) is documented in `docs/docs/wasm_differences.mdx`.
   Broadening to more type shapes / `preserve`/`canonical` stays untracked (the `to_json_value` line
   is flag-independent) — do it only if a divergence surfaces.

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
   - ✅ *Done.* `integration_tests` now `cargo run`s the json-gen crate (was `build`-only), so its
     `export_schemas()` actually executes, and asserts the `schemas/` dir is non-empty (cw16 + a
     surfaced follow-up). Touches json / json_preserve / multifile_json_preserve.
   - **Still open:** the full end-to-end `--package-json` run (json-gen `cargo run` → both scripts →
     wasm-pack). `run-json2ts.js` stays covered by `integration_tests::js_schema_to_ts`.

8. ✅ **Negative-path / rejection testing.** *Done.* `tests::structural_rejects` in
   `tests/core/tests.rs` now pins a comprehensive structural slice — empty input, too-short/over-long
   arrays, wrong major type per slot, wrong/missing tag, duplicate map keys (`DuplicateKey`), missing
   required key (`MandatoryFieldMissing`), and indefinite/definite length-framing errors
   (`DefiniteLenMismatch`/`EndingBreakMissing`/`BreakInDefiniteLen`) — each with an `is_ok()` baseline.
   `DeserializeError`/`Key` Display formatting is also asserted (cw14). (Numeric/size bounds were
   already covered by `bounds`.) **One thing remains, and it's a decision not a task:** trailing bytes
   after a complete value are currently *accepted* (`from_cbor_bytes` never checks cursor==len) — see
   "Pending decisions."

## Pending decisions (maintainer call — blocks the related test, not on effort)

Surfaced during the clear-wins sweep; each is gated on a behaviour/policy call. Full context + the
rest of the deferred menu (incl. medium next-tasks like a preserve-encodings golden known-answer set
and assertion upgrades needing value choices) live in `tests/CLEAR_WINS_PLAN.md`.

- **Trailing-bytes contract.** `from_cbor_bytes` accepts extra bytes after a complete value. Reject
  (stricter; matches most CBOR expectations) or keep? Affects every generated crate — asserting
  `is_err()` today would fail. Decide, then pin the test.
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
