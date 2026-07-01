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
- **Fail-loud** — make silent-invalid-output (`rustfmt` swallow) a hard error, so malformed emission
  can never again pass as supported.

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

2. **Compile the generated *wasm* crate in the systematic gates — today nothing does.** *(structural;
   ~½–1 day for the corpus gate, more for the matrix axis)* Every automated compile-gate generates with
   `--wasm=false` and only `cargo check`s the `rust/` crate: `integration_tests::feature_corpus_compiles`,
   `cddl-matrix/verify.ts` (the per-feature gate — `verify.ts:267/305/328`, documented at `:372`), and
   `project_robustness.ts`. The **only** thing that ever compiles wasm is the handful of hand-picked
   `run_test` integration fixtures (core/preserve/json/canonical/multifile/rust-wasm-split/wasm_json), so
   generated wasm has **coverage by accident, not by construction**. Consequence: an entire class of
   defects — *generated wasm wrappers that don't type-check* (wrong accessor return type, bad boundary
   `.into()`/`.clone()`/`.copied()`, by-value-vs-by-ref ABI slips, generic-arg leaks) — is invisible to
   every systematic gate, because the *rust* side compiles fine and rust is all we check. This is a whole
   output mode with no systematic compile net.
   - **Concrete proof it bites (pre-existing bug).** `nums = [* uint]` (a named, wasm-*exposable* array)
     used as a list element — `nested_num = [* nums]` — emits, with the flag **off**, a wasm wrapper whose
     `get(&self) -> Nums { self.0[index].clone() }` returns `Vec<u64>` typed as the wrapper struct `Nums` →
     two `error[E0308]`s; the wasm crate does not compile. The rust crate compiles clean. Surfaced only
     incidentally by the `--wasm-list-macro` audit (unrelated to that flag); nothing in the corpus/matrix
     would ever have caught it.
   - **Why the feature matrix misses it too.** cddl-matrix *does* model "array of arrays"
     (`contain.array-element.type2.array`), but (a) its verdict is a rust-only `cargo check`, and (b) it
     individuates features by **serialization semantics** ("a distinct wire-encoding / constraint"),
     whereas this is a **wasm-binding distinction** — "exposable inner (inlined as `Vec<u64>`) vs named
     wrapper struct" is a wasm ABI decision (`is_copy` × `directly_wasm_exposable` × the boundary-op tables
     in `intermediate.rs`: `to_wasm_boundary` / `from_wasm_boundary_clone` / `for_wasm_param` /
     `for_wasm_return`), not a CBOR one. The serialization-oriented axis doesn't enumerate the wasm ABI
     branches, so even a "fully covered" matrix has this hole by construction.
   - **The fix, in leverage order:**
     1. **Add a wasm compile-gate to `feature_corpus_compiles`** *(cheapest, highest leverage)*: also
        generate the corpus with `--wasm=true` and `cargo check` the `wasm/` crate. `cargo check` on the
        **host** target catches type/signature errors like the E0308 above **without** the `wasm32` target
        or `wasm-pack` — `wasm-bindgen` is just a normal dependency, and the shared `CARGO_TARGET_DIR`
        already amortizes its build. Lights up wasm type-checking across the *entire* corpus at once and
        catches the whole class going forward. It will go **red** the moment a fixture exercises the bad
        shape — which is the point (same "the test surfaces the bug" dynamic as the c6 reject half and the
        inverted `nint` bound, item 1). Reuse the existing `COMPILE_SKIP` list for fixtures whose wasm can't
        stand alone (extern / raw-bytes user-supplied types — same reason they're rust-skipped; ties into
        the "snapshot-only corpus policy" pending decision below).
     2. **Give `cddl-matrix/verify.ts` a wasm verdict axis** *(the systematic, at-scale version)*: a second
        per-feature support value (`wasm-compiles`) alongside the rust `cargo check`, so the matrix reports
        wasm codegen support per feature and per role — not just rust. (1) is the fast down-payment across
        the corpus; this is the full net across the feature space. Shares the `verify.ts` /
        `feature_corpus_compiles` surface with item 1's `cargo check → cargo test` round-trip flip —
        coordinate the two edits.
     3. **Add fixtures for the wasm-binding distinctions the serialization axis doesn't individuate**: the
        wasm analogue of the feature corpus — one case per branch of the boundary-op decision tables.
        Minimum set: exposable vs non-exposable **array** element; exposable vs non-exposable **map**
        value; nested exposable arrays (`[* [* uint]]` and the *named* `nums = [* uint]; [* nums]` that
        triggers the bug); `Copy` vs non-`Copy` element in `get`/`add`/`insert`/`keys` positions; c-style
        enum element (the `push(elem)` vs `push(elem.into())` re-export edge documented on
        `ConceptualRustType::wasm_list_macro_needs_into`). `tests/wasm-list-macro/input.cddl` already
        covers several list-element branches — generalize that idea to maps and to the non-macro path.
   - **Cross-cut with the underlying generator bug.** (1)+(3) *expose* the `[* named_exposable_array]`
     defect; the **fix** lives in the emitter — `generate_array_type`'s wasm `get`/`add` for an
     exposable-array element picks the wrapper type (`Nums`) as the return but the inlined `Vec<_>` as the
     body. Log it as a generator bug in its own right; the test work above is what makes it (and its
     siblings across the ABI table) *stay* fixed rather than regress silently.
   - **Why it's `--wasm=false` today (the tradeoff being accepted, not overlooked).** The rust-only gate is
     faster (no `wasm-bindgen` dep tree) and needs no wasm toolchain — a reasonable default for a
     *serialization* tool, but it silently declared wasm codegen out of scope for systematic testing.
     `cargo check` (not `build`, not `wasm-pack`) on the host recovers most of the signal for little cost;
     the remaining wasm-only concerns (`#[wasm_bindgen]` macro-expansion errors, `.d.ts`/JS surface) still
     need `wasm-pack`, and stay the job of the few `run_test` fixtures + item 7's `--package-json` run.

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
   **Still open:** (a) make a rustfmt parse/internal failure **fatal** (propagate `Err` from
   `rustfmt_generated_string`) — the turbofish shipped green at exit 0 *only* because rustfmt parse
   errors are swallowed, so making it fatal kills the whole silent-invalid-output class (see also item
   6 and the north star's "fail-loud"); (b) the full `--package-json` run (json-gen `cargo run` → both
   scripts → wasm-pack). `run-json2ts.js` stays covered by `integration_tests::js_schema_to_ts`.

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
