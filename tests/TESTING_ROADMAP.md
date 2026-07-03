# Testing roadmap — what to do next

A curated, opinionated plan for the *next* testing push, so a future effort has a solid starting
point. This is the short list (what we'd actually do, in order), distilled from an independent
multi-agent review (`draft/testing-recommendations/RECOMMENDATIONS.md` — the exhaustive menu) plus
findings from building the current suite. See `tests/README.md` for how the current setup works and
what's already covered.

**CI policy — fast tier only.** CI runs exactly `bun run check.ts fast` (the absolute-minimum
commit gate; cost policy — see `AGENTS.md`), enforced by check.ts's `self_checks`. No item below
goes into the fast tier: new verification systems land in check.ts's `local` or `full` tier and run
locally. Promoting anything into `fast` is a maintainer decision.

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
shipped green. That oracle now exists as a set of live systems: the emitted round-trip harness
executes across the default suite, the corpus, and the matrix (whose "supported" verdict is
execution-gated, with an embed-site fallback for shapes that mint no standalone surface); the
generated wasm crate is compiled AND round-tripped by the same systematic gates; and malformed
emission is a hard generator error. Their living documentation is `tests/README.md` (suite-side)
and `cddl-matrix/README.md` (probe-side). One north-star frontier remains: the matrix's own verdict
is still a default-profile claim — the "Profile axis" pending decision below.

## Recommended next steps, in priority order

1. **Complete the `cargo-mutants` sweep and triage the survivors.** The system is built and its
   invocation pinned (`.cargo/mutants.toml` + `tests/README.md` § "Mutation testing": emit-core
   scope, behavioral-only scoring via a nextest filterset excluding `snapshot_tests` — snapshot
   "kills" measure text-sensitivity, not whether wrong emission is caught behaviorally), but only a
   ~3% sample has been swept (33/1040 mutants; a full sweep is a measured ~30 h unattended job).
   Remaining: run `cargo mutants --iterate` to completion (overnight chunks; resumes from
   `mutants.out/`, skipping already-caught/unviable mutants), then triage every miss into one of
   (a) a missing behavioral assertion → add the test, (b) a roadmap entry naming the uncovered
   emit logic, or (c) behaviorally-equivalent-by-construction → exclude via config with a comment.
   The sample's 6 misses were all class (c) (style-only: the clippy-appeasement arity branch in
   `container_encoding_lookup`, redundant-`.clone()` emission from `encoding_var_is_copy ->
   false`), so if that class dominates the full survivor list, add `exclude_re` entries for those
   functions to keep the score meaningful. Whether a scoped variant (e.g. `--in-diff`) earns a
   `full`-tier check.ts gate is a decision to make AFTER the first complete sweep establishes the
   baseline survivor map.

2. **`prettyplease` instead of shelling to `rustfmt`.** Removes toolchain-dependent formatting
   churn and the `which` dependency, compiles fast, never bails (it reuses `syn`, already built
   transitively via the proc-macro derives). Lower urgency only because the pinned toolchain already
   mitigates churn.

3. **Output-validate `--json-schema-export` end-to-end.** *(small)* Still open: the full
   `--package-json` run (json-gen `cargo run` → both scripts → wasm-pack). This is also the only
   layer that would exercise `#[wasm_bindgen]` macro-expansion / `.d.ts` / JS-surface concerns —
   the systematic wasm gates `cargo check` on the host target and can't see them; today the few
   hand-written `run_test` fixtures' wasm-pack builds are the sole coverage. The rest of the json
   surface is covered: the in-process schema-vs-serde check, the `.d.ts` merge, and json-gen's
   `export_schemas()` execution for scalar AND generic-backed newtypes (`integration_tests::json`
   / `json_preserve`; `tests/corpus/newtype_generic.cddl` under the `json` profile of
   `feature_corpus_compiles`), plus `run-json2ts.js` via `integration_tests::js_schema_to_ts`.

4. **Grammar-fuzz the corpus.** Generate random *valid* CDDL and run it through the generator to
   surface coverage holes and crashes the hand-picked fixtures miss. Laziest source first: recombine
   the matrix's `containment/*.toml` examples (they already enumerate which construct nests in which
   role legally); escalate to an `arbitrary`-derived "supported-CDDL" AST only if that plateaus. Treat
   the fuzzer as a *corpus generator*, not a CI gate — seed it for determinism, then promote any new
   divergence/crash into the snapshot corpus (review once, commit). Complements a real-world on-chain
   corpus differential (see `draft/testing-recommendations/RECOMMENDATIONS.md`): synthetic breadth vs
   real-world depth.

5. **Small independent residuals (low).**
   - **wasm write-side present-null construction** *(unrequested)*. The read-side three-state
     fidelity gap is closed (presence accessors `has_<field>()` / map `has(key)`; oracle:
     `tests/nullable-wasm/`; read protocols in `docs/docs/wasm_differences.mdx`). The remaining
     asymmetry is on the WRITE side: wasm setters/constructors always wrap the argument in an outer
     `Some`, so a JS caller can produce absent and present-value but not present-null. Revisit only
     when a consumer asks.
   - **`corpus_detect` dsl-prose residual.** On a directive-leading comment line the detector
     credits every later `@word` via `matchAll`, which doesn't mirror `comment_ast`'s sequential parse
     (`@doc` prose runs to the next `@`; other directives stop at the first non-directive token) — a
     real directive id buried in trailing prose after a leading directive could keep a dsl cover green.
     No current fixture triggers it; the fix must replicate that asymmetric `@doc` grammar, not a naive
     stop-at-first rule.
   - **Local-tier wall-clock to watch.** `feature_corpus_compiles` and `wasm_matrix_compiles` shell
     nested cargo per cell in the default `cargo test` suite (check.ts `local` tier, not CI); the
     shared `CARGO_TARGET_DIR` amortizes deps. If wall-time bites: batch cells into fewer crates,
     adopt `cargo-nextest` as the suite runner, or gate only changed cells.

## Pending decisions (maintainer call — blocks the related test, not on effort)

Surfaced during the clear-wins sweep; each is gated on a behaviour/policy call.

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
  can't `cargo check` standalone (extern is exercised only via `tests/extern-deps`; raw-bytes via
  `tests/raw-bytes` — wasm boundary executed by its `tests_wasm.rs` — and the wasm-matrix
  `rawbytes__*` cells, which splice the in-repo defs). The gap is *snapshot* coverage of those emit
  paths; introduces a "snapshot-only corpus" concept the docs don't have yet.
- **Stubbed `no_key_group` comment-dsl test.** The only test of the no-key-group emit path is
  commented out (`tests/comment-dsl/tests.rs`); writing a real assertion first needs identifying
  which CDDL construct actually routes through that path (and whether it's reachable at all).
- **Re-bless-snapshot coverage gaps** (each needs a fixture/profile change + snapshot re-bless):
  float under the `json` profile; `OrderedHashMap` JSON **serde** (a map-bearing json fixture —
  `tests/json/input.cddl` has none; the `JsonSchema` half is now compile-gated by
  `tests/corpus/newtype_generic.cddl`, see the `--json-schema-export` item above — this remaining
  serde half is a genuine snapshot chore); re-enabling `bool_wrapper` JSON newtype (blocked on
  generator issue #223).
- **Profile axis on the matrix annotation schema.** The generation-breadth and round-trip-breadth
  halves of "supported is a per-profile fact" are now covered by manual gates — the supported
  catalog generates under all three profiles (`all_supported_constructs_generate_all_profiles`,
  with a per-profile expected-fail list), and the corpus `--emit-tests` round-trip suite runs under
  preserve/json (`feature_corpus_roundtrips_nondefault_profiles`). What remains is the matrix's OWN
  verdict: `verify.ts` probes the default flags only, so a matrix `status = "supported"` annotation
  is still a default-profile claim. The frontier is a per-profile axis on the matrix annotation
  schema (a construct's verdict recorded per profile), so a supported-construct × preserve/json
  regression surfaces as a matrix-drift diff rather than only inside those two manual gates. This
  also subsumes carrying `verify.ts`'s embed-site fallback (see `cddl-matrix/README.md`) per
  profile. Flag-specific failures are a proven class (floats hit `unimplemented!` under preserve —
  see the `preserve_encodings_supports_floats` stub).
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
