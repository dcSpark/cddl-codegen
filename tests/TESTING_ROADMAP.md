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
wasm), and round-trips — today the verdict only means "rust generates and rust-`cargo check`s," which
is why real bugs (inverted `nint` bound, wasm `get`/`add` type mismatch, JSON-schema turbofish) shipped
green. The Tier-1 items below are the missing pieces of that oracle:

- **Correctness** — the round-trip harness (item 1): turns "output didn't change" into "output is right."
- **Coverage** — a wasm compile-gate (item 2): a whole output mode no systematic gate compiles today.
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

2. **Compile the generated *wasm* crate in the systematic gates.** *(corpus gate landed + first bug fixed;
   matrix axis + wider coverage still open)* Historically every automated compile-gate generated with
   `--wasm=false` and only `cargo check`ed the `rust/` crate, so generated wasm had **coverage by accident,
   not by construction** — a whole class of defects (wasm wrappers that don't type-check: wrong accessor
   return type, bad boundary `.into()`/`.clone()`/`.copied()` slips, by-value-vs-by-ref ABI mismatches) was
   invisible because the *rust* side compiled fine. The wasm bindings are a wasm-ABI concern (`is_copy` ×
   `directly_wasm_exposable` × the boundary-op tables in `intermediate.rs`), which the CBOR-serialization
   feature axis doesn't individuate — so even a "fully covered" matrix had this hole by construction.
   - **✅ Done (the oracle):** `feature_corpus_compiles` now generates `--wasm=true` and `cargo check`s BOTH
     the `rust` and (when emitted) `wasm` crates on the host target — no `wasm32`/`wasm-pack` needed
     (`wasm-bindgen` is a normal dep; the shared `CARGO_TARGET_DIR` amortizes it). It caught, and the emitter
     was fixed for, the named-alias-collection class at the array-element, map-value, optional-field, and
     passthrough-alias positions (see ledger). Gated by `tests/corpus/wasm_nested_alias.cddl`.

   - **⚠️ The gap this leaves — and the system to close it (this is the real wasm work).** The compile-gate
     is an *oracle* ("does THIS fixture's wasm compile?"), but **nothing enumerates which fixtures are
     needed.** Every wasm-ABI bug found so far lived in an un-covered cell of a finite cross-product and was
     found by ad-hoc exploration — which does not converge (fix-one-per-stumble is not TDD). A green gate
     today means "the hand-picked cells compile," *not* "wasm codegen is correct." The fix is a **systematic
     wasm-ABI matrix** — the same idea as `cddl-matrix`'s feature enumeration, but on the axis the
     serialization matrix deliberately does NOT individuate: the **wasm-ABI representation** of a type.
     - **Type-shape axis** (`is_copy` × `directly_wasm_exposable` × has-a-wrapper-`RustStruct` — NOT a CBOR
       distinction): primitive; named primitive alias (transparent `pub type`); named collection (wrapper
       struct); passthrough alias → collection (transparent → `Vec`); passthrough alias → wrapper (transparent
       → wrapper, e.g. `.cbor`/tagged); struct; c-style enum; optional; generic instance; extern.
     - **Role axis** (already modelled by `cddl-matrix`'s `roles.toml`): array element, map key, map value,
       struct field (mandatory + optional), newtype inner — each drives distinct accessor emission
       (`get`/`add`/`insert`/`keys`, param vs return, by-value vs by-ref).
     - **The projection:** a `project_wasm_matrix.ts` (sibling of `project_corpus.ts` / `project_robustness.ts`)
       emits ONE minimal CDDL fixture per (type-shape × role) cell, runs each through the wasm compile-gate
       (and later the round-trip harness for *behaviour*), and reports a per-cell verdict. This subsumes the
       former "give `verify.ts` a wasm verdict axis" + "add more wasm fixtures" bullets: not "add a few
       fixtures" but **enumerate the cross-product and gate every cell**, so coverage is by-construction and
       new wasm-ABI bugs surface as specific red cells instead of production surprises. Until this exists,
       treat every ad-hoc wasm fix as incurring a debt: it needs its matrix cell, or the class recurs.
       **Concrete cell taxonomy + build plan (copy-paste CDDL per type-shape, the projection template,
       gate wiring, landmines): `draft/handoff-wasm-abi-matrix.md`.**
   - **First red cells for that matrix (known-failing — do NOT ad-hoc fix; fix as matrix targets):**
     (a) passthrough alias to a **map** typedef at a wasm map value/element (`m2 = amap; amap = {…}`) →
     dangling `MapU64To…` type ref (`E0425`); (b) a named collection **wrapper** used as a map **key** →
     `Borrow` mismatch (`E0277`). Both pre-existing (byte-identical at HEAD); see the ledger.
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

## Explicitly not worth it (decided, not overlooked)

- Full `2^N` flag powerset / PICT pairwise — curated named profiles already cover this; revisit
  only if a flag-interaction bug actually slips through.
- `quickcheck` alongside `proptest`; `goldenfile`/`expect-test` as a second corpus engine;
  `no-panic` lints; coverage instrumentation of *generated* code; `trybuild` for whole-crate
  compile-pass (the corpus `cargo check` is simpler and broader).

## Sources
- Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`
- Per-dimension expert write-ups: `draft/testing-recommendations/*.md`
