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
and `cddl-matrix/README.md` (probe-side). The verdict is no longer a default-profile-only claim:
every default-`supported` row is also probed per non-default emission profile (`preserve`, `json`)
and against foreign spec-derived decode vectors, both recorded in the committed annotations.

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

3. **Grammar-fuzz the corpus.** Generate random *valid* CDDL and run it through the generator to
   surface coverage holes and crashes the hand-picked fixtures miss. Laziest source first: recombine
   the matrix's `containment/*.toml` examples (they already enumerate which construct nests in which
   role legally); escalate to an `arbitrary`-derived "supported-CDDL" AST only if that plateaus. Treat
   the fuzzer as a *corpus generator*, not a CI gate — seed it for determinism, then promote any new
   divergence/crash into the snapshot corpus (review once, commit). Complements a real-world on-chain
   corpus differential (see `draft/testing-recommendations/RECOMMENDATIONS.md`): synthetic breadth vs
   real-world depth.
   The value of shape recombination is proven, not speculative: every gate samples ONE minimal
   example per feature row, and the map-rep group-choice fix found three defects hiding in
   unsampled shape variants of a single "supported" row — a multi-field arm whose enum dispatch
   keyed on the wrong CBOR type (didn't even self-round-trip), a fixed-value-entry arm that
   panicked generation, and a non-fixed-key arm that silently dropped the key. All three would
   have surfaced mechanically from recombining member shapes inside one construct. Identifier
   space is a fuzz dimension too, not just grammar shapes — the proven instance is now an
   enumerated sweep, not a fuzz finding: `src/tests/identifier_hazard_tests.rs` verdicts hazardous
   names × positions × emitted type shape (it caught the shape-dependent `r`/`w` generic collision
   and the reserved-name panics, both since fixed). A grammar fuzzer should draw identifiers from
   that hazard table rather than rediscover it. Recombination is only as complete as the role set it
   draws from; the containment matrix now includes the group-choice-arm `//` role, map-key
   spelling/arity cells, and occurrence-target marker-kind cells, so those examples are available as
   structured fuzzer ingredients.

4. **Small independent residuals (low).**
   - **Top-level fixed-value / bare-literal rules panic generation** (`foo = 5`, and equally
     `#6.n(5)` — the tag is irrelevant). Generation aborts in `intermediate.rs`
     (`should not expose Fixed type in member, only needed for serializaiton: Fixed(Uint(5))`)
     because a `Fixed` conceptual type has no standalone member representation. Support it (a
     one-field wrapper that stores nothing and re-emits the constant on serialize) or reject it
     gracefully; either flips the panic. Distinct from tag handling — a bare literal never reaches
     the tag-wrapping arms. (The two tag-inner variants that DID silently drop the tag through a
     `pub type` alias — a `.default`-carrying inner `#6.n(uint .default 5)` and a range that
     collapses exactly onto a rust primitive `#6.n(uint .le 255)` — now auto-wrap like the
     primitive/`.cbor` arms; pinned by `tests/corpus/tagged_default_inner.cddl` and
     `tests/corpus/tagged_ranged_inner.cddl`. Literal-headed range inners — `#6.5(3..10)` — wrap
     too, pinned by `top_level_ranges` in `tests/core`.)
   - **wasm write-side present-null construction** *(unrequested)*. The read-side three-state
     fidelity gap is closed (presence accessors `has_<field>()` / map `has(key)`; oracle:
     `tests/nullable-wasm/`; read protocols in `docs/docs/wasm_differences.mdx`). The remaining
     asymmetry is on the WRITE side: wasm setters/constructors always wrap the argument in an outer
     `Some`, so a JS caller can produce absent and present-value but not present-null. Revisit only
     when a consumer asks.
   - **`bool_wrapper` JSON newtype** — blocked on generator bug #223 (bool newtype does not
     compile); the commented-out rule in `tests/json/input.cddl` carries the issue link. Unblocks
     only by fixing the generator.
   - **Don't emit a conformance assertion for a bare-group rule (restore per-rule rust oracle).** A
     top-level group rule (`inner = (a: uint, b: uint)`) is not a validatable instance type — both
     conformance oracles reject it (the ruby gem always did; the rust fork's validator now does too,
     since the `773b723` array-sequence rewrite stopped leniently accepting the array-serialized
     mint). Today `nested_group` is handled by fixture-level `RUST_ORACLE_SKIP`, which also drops the
     rust-oracle half of its sibling `outer = [x: uint, inner, y: uint]` (still covered by round-trip
     + ruby + the structural differential). Emitting NO conformance call for a rule whose conceptual
     type is a bare group — rather than emitting one the oracle can't answer — would let `nested_group`
     leave `RUST_ORACLE_SKIP` and restore rust-oracle coverage of `outer`. Small `emit_tests.rs`
     change gated by the existing `ir_conformance_corpus` gate.
   - **Tracked-gap prose must cite its pin — and a lint should assert cited pins exist (low).**
     Prose that tracks a live gap ("tracked by the `#[ignore]`'d fidelity tests", "construct X
     panics the generator") rots silently when the gap closes: the fix retires the test/pin, but
     nothing ties the sentence to it, so the claim outlives its referent (three instances were
     hand-caught, not gate-caught, during the wasm-ABI grid extension: the roadmap bullet tracking
     the read-fidelity gap, a `tests/core/tests_wasm.rs` comment citing the retired `#[ignore]`
     stubs, and a `project_wasm_matrix.ts` role comment claiming `{ ? f: T }` panics — it
     generates; a fourth followed in the same audit: the wasm emitter's documented loud-skip lists
     carried a "flatten points" entry with no corresponding skip site in the code, in any revision). Two mechanical halves: (1) convention — a gap-tracking sentence names its pin by
     exact identifier via the already-pervasive "pinned by/tracked by/gated by `name`" phrasing,
     and a *behavioral* claim ("X panics/rejects") gets a robustness-catalog row FIRST (the
     panic/reject catalogs flip loudly on a behavior change; the `{ ? f: T }` claim rotted
     precisely because it lived only in prose); (2) a drift-gate lint that extracts
     `pinned by`/`tracked by`/`gated by` citations from the gap-tracking docs (`*ROADMAP*.md`,
     `tests/README.md`, `cddl-matrix/README.md`) and asserts each cited identifier still exists in
     the tree — the same shape as the `cddl-matrix/verify.ts` DSL-name forward lint, and the
     narrow, greppable complement to the declined docs-vs-behavior snippet harness (bottom of this
     doc): identifier existence, not prose semantics.
   - **Markdown-structure lint over the hand docs (low).** A ROADMAP findings-bullet prune ate the
     blank line separating the deleted bullet from the next `##` heading — valid-but-mangled
     markdown caught only by in-session review, a class orthogonal to the citation lint above
     (structure, not referents). An MD022-class check (blank line before headings) over
     `*ROADMAP*.md` / `tests/README.md` / `cddl-matrix/README.md` is a one-liner in the drift
     gates; worth wiring only if hand-doc surgery keeps being a per-feature step (it currently is).
   - **Local-tier wall-clock to watch.** `feature_corpus_compiles` and `wasm_matrix_compiles` shell
     nested cargo per cell in the default `cargo test` suite (check.ts `local` tier, not CI); the
     shared `CARGO_TARGET_DIR` amortizes deps. If wall-time bites: batch cells into fewer crates,
     adopt `cargo-nextest` as the suite runner, or gate only changed cells.
   - **One unattributed full-suite flake (2026-07-06) — capture before rerunning.** The `test` gate
     (`cargo test --all-features --all-targets`) failed once with exit 101 on a doc/TS-only change,
     then passed on an immediate identical rerun and a second full run; the failing test's name was
     lost (the output was truncated before the `failures:` list, and reruns were green). Nothing can
     attribute a transient failure after the fact, so the discipline on recurrence is: save the full
     output (`2>&1 | tee`) BEFORE any rerun, then triage. Likeliest transient class: the nested-cargo
     integration gates above (shared `CARGO_TARGET_DIR`; a racing cargo lock/link step). A second
     sighting with a captured name turns this bullet into a real fix item.

5. **Assert the rejection REASON for `class="constraint"` replay vectors (behavioral layer over the
   standing shape lint).** The replay gate asserts only that a reject vector `Err`s — it cannot tell
   a bounds rejection from a CBOR type mismatch, so a malformed constraint vector passes as vacuous
   enforcement evidence (the holder-wrapped `8200…` scalars on the first rangeop mint were caught
   only by review). The STRUCTURAL layer is built: `project_decode_conformance.ts` § 6 (local-tier
   drift gate; promoting to `fast`/CI is a maintainer call) fails any constraint vector whose
   leading CBOR major-type class differs from its row's accepts, or a holder-preamble scalar on a
   standalone row — that alone catches the shipped escape class at drift-gate time. What remains is
   the behavioral proof the lint can't give: a decoder that rejects for a subtly WRONG reason (a
   stray length check, an unrelated error path) still satisfies both `Err` and the shape lint. For
   `class="constraint"` vectors, assert the decoder's error Display names the violated check
   (`RangeCheck`/`RangeCheckFloat`/`.size`-style text) rather than a type/length mismatch — the
   replay harness already captures per-vector errors, so this is a match on strings the generator
   itself emits, oracle-free and deterministic.

6. **Extend the decode-conformance corpus along two axes: encoding variants, then composition
   depth.**
   - **Encoding variants (medium — cheap and self-contained).** Every committed accept vector is
     definite-length, minimal-width CBOR by mint construction (ruby `generate` → `diag2cbor.rb`),
     so spec-EQUAL re-encodings of accepted instances — indefinite container lengths, non-minimal
     int/len widths — are systematically unsampled in the decode direction. The gap is proven: the
     keyed map-rep group-choice deserializer rejected indefinite-length maps (`bf 6161 05 ff`) and
     only a manual review caught it, because no gate feeds encoding-variant bytes to the decoders.
     The remedy needs no oracle: derive the variants mechanically from each committed accept
     vector's bytes (re-encode container headers indefinite, widen int/len sizes) inside the
     replay gate and assert they decode to the same value — deterministic, pure-byte transforms,
     `full` tier alongside the existing replay.
   - **Composition depth (low).** The shipped decode-direction harness (`tests/README.md`
     § "Decode-direction conformance") keys its obligation set on the matrix's minimal
     per-construct examples — breadth, not depth. The corpus fixtures (`tests/corpus/*.cddl`) add
     the composition depth those minimal examples lack; extending the harness to mint vectors for
     them should go through the corpus projection (so coverage stays mechanically checkable), not
     a hand-picked fixture list. json/wasm decode surfaces are likewise unminted. The breadth
     layer's first sweep caught two decoder bugs (the map-rep group-choice key-drop miscompile —
     since fixed, pinned by the row's accept vectors — and inline-group occurrence narrowing —
     since fixed as a graceful rejection, pinned by projected `tests/matrix_reject/contain.occurrence-target.grpent.inline_group.*.cddl`
     fixtures and unsupported-row decode catalog absence), so depth is not the current bottleneck.

7. **Over-acceptance pins: a catalog vector class for spec-INVALID CBOR the decoder wrongly
   ACCEPTS (low, but the gap is proven).** The decode-conformance catalog can only express two
   truths — "must accept" and "durably rejects" (`class="constraint"`) — so a known
   silent-acceptance bug has NO standing pin. The motivating instance is since fixed (the
   `uint .size 2` member decode truncating 65536 to 0 via a bare `as u16` — every narrowing cast on
   the deserialize path is now width-guarded, pinned by `signed_ints_width_rejects` /
   `width_collapse_rejects` and the row's committed constraint vector; ROADMAP § findings), but the
   SYSTEMIC gap it exposed stands: while that bug was live, the catalog had nowhere to hold the
   certified-invalid vector, so the row's projection collapsed to `enforce = unverified` —
   indistinguishable at the Q4 level from "vector not yet minted" — and the bug was held only in
   prose. The missing system is the `KNOWN_SILENT_DROP` / `EXPECTED_COMPILE_FAIL` pattern applied
   to decode: a pinned vector class (e.g. `class="over-acceptance"`) for certified-spec-invalid
   bytes the decoder CURRENTLY accepts, replayed as "still wrongly accepts" so the pin flips loudly
   when a fix lands (prompting promotion to `class="constraint"` and the Q4 enforce-green pin). Q4
   could then project the stronger honest fact `enforce = no (over-accepts)` instead of
   `unverified`, and the `EXPECTED_ENFORCE_UNVERIFIED` pin in `query_q4_directional.ts` (empty
   today) would hold genuinely-unminted rows only. This is also the F10 "over-acceptance
   denominator" pending call made concrete — resolve them together.

8. **Widen the rust↔wasm API-surface parity differential (low).** The one-directional rust→wasm
   presence differential now exists and runs always-on
   (`src/tests/wasm_parity_tests.rs::wasm_api_parity`, documented in `tests/README.md` § "rust↔wasm
   API-surface parity"): it parses the emitted `mod.rs` of both crates over the wasm-matrix cells +
   the two depth fixtures, swept across all three emission profiles, and asserts every rust pub
   type/field/inherent-fn has a wasm counterpart or a ledgered exemption. Two axes it deliberately
   does NOT yet sweep:
   - **Corpus-wide input axis.** It covers the matrix cells + two depth fixtures, not the whole
     snapshot corpus (`tests/*/input.cddl` under their per-dir flags). Extending it there should
     reuse each dir's committed flag set so coverage stays mechanically checkable.
   - **JS-name-visibility axis.** Rules 1–2 accept a *public alias* as the wasm counterpart, which is
     rust-source-level parity only — wasm_bindgen exports no type aliases, so an alias-only
     counterpart means the CDDL rule name never reaches JS. The proven (hand-caught) instance: a
     named table rule's wrapper takes the structural `MapKToV` name when the same shape was already
     minted for an embedded/resolved use, so the JS class name flips between `Mp` and `MapU64ToText`
     depending on unrelated spec content (`cddl-matrix/ROADMAP.md` § findings has the ledger entry +
     candidate emitter fix). The gate extension: classify each rust type's counterpart as
     defined-`#[wasm_bindgen]`-type vs `pub use` vs alias-only, and require every supported rule name
     to be JS-visible or ledgered — turning the naming wart from a hand observation into a red cell.

- MSRV declaration / OS matrix for GENERATED code: the templates' `edition = "2024"` already
  hard-floors the effective MSRV at rustc 1.85 with a self-explanatory compile error, and generated
  output has no platform-conditional code an OS matrix would exercise. Revisit only if a consumer
  reports an MSRV or platform break (dep-driven MSRV creep is the one real vector).
- A docs-vs-behavior conformance harness for `comment_dsl.mdx` / `output_format.mdx` (snippet
  extraction + output spot-checks): emitted output is already pinned by the snapshot corpus and
  DSL-name drift by the `cddl-matrix/verify.ts` forward lint; a doc-snippet system is heavy
  machinery for prose drift that review catches at its actual rate. NB this decision covers PROSE
  drift only — the separate class of a directive silently no-oping in an unenumerated attachment
  position (`@name` was dropped on arrow keys once and bareword keys once, both found by hand) is
  a real class with its own standing system: the directive × attachment-position sweep
  `src/tests/dsl_position_tests.rs`, hard-asserted against `comment_dsl.mdx`'s claims. (The `example/`-half of this
  decision — gating the getting-started command — was accepted and shipped as
  `integration_tests::getting_started_example`.)
- Full `2^N` flag powerset / PICT pairwise — the curated named profiles cover the flag
  *combinations* worth testing, so the full powerset stays out of scope. One escaped interaction
  earned its own standing gate rather than the whole powerset: `--common-import-override` ×
  `--preserve-encodings=false` targeting a preserve-flavored common crate emitted
  `CBORReadLen::new(Len)` against a `new(LenSz)` runtime (E0308). The extern-deps surface is now
  probed under both preserve flavors — `integration_tests::extern_deps` (preserve) and
  `integration_tests::extern_deps_non_preserve` (non-preserve, compiled against the preserve-flavored
  `extern-dep-crate` stand-in) — so that specific cell is pinned without enumerating the rest.
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
