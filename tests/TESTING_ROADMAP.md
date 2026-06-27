# Testing roadmap — what to do next

A curated, opinionated plan for the *next* testing push, so a future effort has a solid starting
point. This is the short list (what we'd actually do, in order), distilled from an independent
multi-agent review (`draft/testing-recommendations/RECOMMENDATIONS.md` — the exhaustive menu) plus
findings from building the current suite. See `tests/README.md` for how the current setup works.

## Where we are (don't redo these)

The foundation is solid and independently validated:

- **Golden snapshots of generated source** (`snapshot_tests.rs`): per-construct feature corpus +
  whole-program inputs + `cargo_toml_matrix` (dep/edition combos) + `serialization_prelude` +
  per-feature IR dumps, across `default`/`preserve`/`json` profiles.
- **Snapshots test the real shipped path** — `export()` and the snapshot producer share one
  `generated_files` function (verified byte-identical output).
- **Orphan-snapshot detection in CI** (`cargo insta test --unreferenced=reject`).
- **Panic-robustness catalog** (`robustness_tests.rs`): malformed/edge inputs run through
  `catch_unwind`, outcome (ok/error/panic) snapshotted.
- **Integration correctness gate** (`integration_tests.rs`): generate → compile → CBOR round-trip,
  per flag combo (incl. JSON round-trip).
- Pinned toolchain (`rust-toolchain.toml`); CI = fmt + clippy + build + test + orphan check;
  measured coverage ≈ 81% (generation.rs ≈ 86%).

## Recommended next steps, in priority order

### Tier 1 — quick wins (~1 day total; grab before any deeper work)

1. **Compile the per-construct corpus, not just snapshot it.** *(~½–1 day)*
   The one real hole left: the feature corpus is snapshot-only, so it could pin *non-compiling*
   generated code as "correct." Add a `cargo check` over the corpus crates sharing one
   `CARGO_TARGET_DIR`. Note for this repo: corpus inputs using `int` need the extern `Int` defs
   (see `tests/external_rust_defs`), so either provide them or keep an `int`-free check subset.

2. **Golden hex known-answer vectors.** *(~a few hours)* `hex-literal`, ~a dozen hand-verified
   `(value ↔ exact bytes)` pairs anchored to RFC 8949 / the CDDL spec. The only cheap guard
   against a *symmetric* encode+decode bug that round-trip tests structurally cannot see.

3. **Generate-twice determinism test.** *(trivial)* `assert_eq!(emit(x), emit(x))`. We're already
   all-`BTreeMap` (no hash-order nondeterminism), so this is cheap insurance + a guard against a
   future `HashMap` creeping in.

### Tier 2 — the strategic investment (a focused multi-day effort)

4. **Emit a property round-trip harness into the generated crate.** *(~2–4 days; then free per
   type)* `proptest` + `arbitrary`, behind a flag (e.g. `--emit-tests`). The generator is the only
   thing that knows each type's exact fields/bounds/optionality, so it can emit a better
   `Arbitrary` + round-trip test than any hand-written one, for **every** type automatically. This
   is the single highest-leverage missing oracle — it turns "output didn't change" into "output is
   correct." Do this when you want to go deep on correctness.
   - Follow-on once #4 exists: **encode-fidelity properties** for `preserve`/`canonical`
     (`bytes → T → bytes` byte-identical over arbitrary valid encodings) — the only at-scale test
     of those high-stakes flags.

### Tier 3 — project-specific, highest signal (when data sourcing is feasible)

5. **Real on-chain CBOR corpus + differential vs the sibling libraries.** *(medium)* This is the
   Cardano `cddl-codegen`; real blocks/txs are the richest fuzz seeds and the existing hand-written
   sibling libs make cross-implementation differential testing nearly free. Higher signal than any
   synthetic data.

6. **Wire-format back-compat + generated-API semver.** *(medium each)* For a serialization tool
   these are correctness contracts the snapshot suite doesn't cover: (a) bytes written by an older
   generator still deserialize under newer generated code (pin a few `(spec, bytes)` vectors);
   (b) `cargo-semver-checks` against a golden generated crate so a renamed field / changed
   signature surfaces as a reviewed API break.

### Tier 4 — validation & process (opportunistic)

7. **`cargo-mutants`** scoped to the emit core (`--file 'src/generation*'`), nightly + `--in-diff`
   on PRs. The defining technique for a codegen tool — proves the suite *catches* wrong-codegen
   bugs, not just covers lines. Only bites once #2/#4 give assertions with teeth; needs
   `cargo-nextest` as the runner.

8. **`prettyplease` instead of shelling to `rustfmt`.** Removes toolchain-dependent formatting
   churn and the `which` dependency, compiles fast, never bails (`syn` is already a dep). Lower
   urgency only because the pinned toolchain already mitigates churn.

## Explicitly not worth it (decided, not overlooked)

- Full `2^N` flag powerset / PICT pairwise — curated named profiles already cover this; revisit
  only if a flag-interaction bug actually slips through.
- `quickcheck` alongside `proptest`; `goldenfile`/`expect-test` as a second corpus engine;
  `no-panic` lints; coverage instrumentation of *generated* code; `trybuild` for whole-crate
  compile-pass (plain `cargo check` from #1 is simpler and broader).

## Sources
- Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`
- Per-dimension expert write-ups: `draft/testing-recommendations/*.md`
