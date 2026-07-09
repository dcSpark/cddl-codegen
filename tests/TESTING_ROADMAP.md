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
and against foreign spec-derived decode vectors, both recorded in the committed annotations — and
the decode-direction evidence is itself identity-carrying, not a bare Ok/Err count: constraint
rejections are reason-asserted (the catalog's `expect_err` pins), every accept vector also
replays under mechanically-derived spec-equal re-encodings, and header-mutation reject mutants of
each accept vector (wrong-major-type / truncated-header byte transforms) must reject WITH an error
location naming the decoding type (`failed in {type_name}`) — the annotation contract at catalog
breadth. The location evidence is itself validated, not just present: every captured Display's
location chain must have no adjacent-duplicate segment (a doubled "Foo.Foo" *satisfies* a bare
`failed in Foo` contains, so presence alone could not see the double-annotation class;
`DOUBLED_LOCATION_SKIP`, empty at HEAD, ledgers any justified exception).

## Recommended next steps, in priority order

1. (pending maintainer action) **Complete the `cargo-mutants` sweep and triage the survivors.** The system is built and its
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

2. (pending maintainer decision) **`prettyplease` instead of shelling to `rustfmt`.** Removes toolchain-dependent formatting
   churn and the `which` dependency, compiles fast, never bails (it reuses `syn`, already built
   transitively via the proc-macro derives). Lower urgency only because the pinned toolchain already
   mitigates churn.

3. **Grammar-fuzzer escalations.** The lazy-first shape-recombination fuzzer is shipped
   (`tests/README.md` § "Shape-recombination fuzzer": `cddl-matrix/project_recombination.ts` →
   `tests/recomb/ingredients.json` → `recombination_generation_sweep` (default suite) +
   `recombination_crates_execute` (full tier), with cited known-class ledgers and a promotion flow
   into the pinned collections). Residuals, in escalation order:
   - **`arbitrary`-derived "supported-CDDL" AST generation** — only if recombination plateaus (its
     first sweep surfaced six new panic-class families, so the plateau is not near; re-evaluate when
     a sweep over an extended member-kind/template table stops minting findings).
   - **Batch-masking detector for the layer-2 sweeps.** The ~40-rule batching means a green batch
     is not a per-composition guarantee for failure classes whose symptom is a missing CRATE-GLOBAL
     definition: `Int` is a crate-global extern emitted iff any rule registers a reference, so a
     batch-mate that registers it compiles the whole batch green. Proven instance:
     `bytes .cbor { * tstr => int }` fails standalone under default yet rode green default batches
     until the wasm leg's different batch boundaries isolated it — caught by review, not by any
     gate (the filing rule + caveat now live on `LAYER2_RULES_PER_BATCH`). The mechanical layer,
     worth building on the SECOND masked instance: run each layer-2 sweep under a second
     DETERMINISTIC batch permutation (a different fixed grouping — determinism preserved, most
     pair-masks broken, wall-clock ×2 on full-tier gates), or an occasional singleton mode
     (batch size 1 — the exhaustive oracle, but hours of wall-clock). Either also owns the
     misfiled-ledger-contradiction class (an entry claiming profile-specificity while masked in its
     home profile — how this instance actually surfaced). Related but decided: the observed-baseline
     comments next to each gate's floors are informational and review-maintained (a stale one
     shipped once); if one misleads triage again, replace them with exact pinned counts asserted by
     the gate, accepting the churn on every ingredient addition — the floors stay the enforced
     artifact either way.
   - **Real-world corpus differential** (see `draft/testing-recommendations/RECOMMENDATIONS.md`):
     synthetic breadth vs real-world depth — recombination does not replace it.

4. **Small independent residuals (low).**
   - **Top-level fixed-value / bare-literal rules are rejected, not yet supported** (`foo = 5`,
     `foo = "text"`, `foo = true`/`null`, and equally `#6.n(5)` — the tag is irrelevant). These
     resolve to a standalone `Fixed` conceptual type, which has no member/standalone Rust
     representation, so they are rejected gracefully at rule registration
     (`intermediate::register_type_alias`, surfaced as `Err` at `finalize`) instead of panicking
     `for_rust_member`; pinned as `error (graceful)` by the `tests/matrix_reject/` rows
     `value.number` / `value.text` / `type2.value` / `prelude.true` / `prelude.false` /
     `prelude.nil` / `prelude.null` and the hand fixture `tests/robustness/tagged_literal.cddl`.
     The open FEATURE is full support — a one-field wrapper that stores nothing and re-emits the
     constant on serialize. The auto-wrapping model already exists for the tag-inner variants that
     stay supported and must not be caught by the rejection: a `.default`-carrying inner
     `#6.n(uint .default 5)` and a range that collapses exactly onto a rust primitive
     `#6.n(uint .le 255)` auto-wrap (pinned by `tests/corpus/tagged_default_inner.cddl` and
     `tests/corpus/tagged_ranged_inner.cddl`); literal-headed range inners — `#6.5(3..10)` — wrap
     too, pinned by `top_level_ranges` in `tests/core`. (Separately, `foo = undefined` still panics
     — a distinct gap, unsupported cddl-prelude `#7.23`, not the `Fixed`-member path; ledgered by
     `tests/matrix_panic/prelude.undefined.cddl`.)
   - **wasm write-side present-null construction** *(unrequested)*. The read-side three-state
     fidelity gap is closed (presence accessors `has_<field>()` / map `has(key)`; oracle:
     `tests/nullable-wasm/`; read protocols in `docs/docs/wasm_differences.mdx`). The remaining
     asymmetry is on the WRITE side: wasm setters/constructors always wrap the argument in an outer
     `Some`, so a JS caller can produce absent and present-value but not present-null. Revisit only
     when a consumer asks.
   - **Local-tier wall-clock to watch.** `feature_corpus_compiles`, `wasm_matrix_compiles`, and
     `multifile_matrix_compiles` shell nested cargo per cell in the default `cargo test` suite
     (check.ts `local` tier, not CI); the shared `CARGO_TARGET_DIR` amortizes deps
     (`multifile_matrix_compiles` measured ~35 s cold / ~30 s warm for its 43 cells). If wall-time
     bites: batch cells into fewer crates, adopt `cargo-nextest` as the suite runner, or gate only
     changed cells.
   - **Full-suite flake, now attributed: `acquire_scratch_lock_serializes` — recurrence needs the
     errno.** The `test` gate failed with exit 101 twice (2026-07-06 unattributed — output truncated
     before the `failures:` list; 2026-07-08 captured in full): the second sighting names
     `integration_tests::acquire_scratch_lock_serializes`, panicking on its release-assert ("the
     lock should be acquirable once the first handle is dropped"). No repro in 60 isolated
     single-test runs, so it is load/parallelism-dependent (the failing run had
     `feature_corpus_compiles`/`wasm_matrix_compiles` shelling parallel nested cargo, whose own
     target-dir flocks pressure the kernel lock accounting). The old assert was `.is_ok()`, which
     conflates the two very different failures — `WouldBlock` (lock outlived its handle = a real
     advisory-flock semantics break) vs a syscall `Error` (e.g. ENOLCK under load = transient
     environment); the assert now `match`es and panics with the concrete error + raw os errno, so
     the NEXT sighting self-attributes. Keep the capture discipline (save full output before any
     rerun); once a recurrence lands with an errno, either harden the test against that transient
     (retry-on-ENOLCK) or escalate a genuine WouldBlock as a std/kernel finding.
   - **Mechanical layers for the two review-owned design rules in `tests/README.md` § "Design
     rules" (invariant-softening, vacuity-floor witness) — build only if a class recurs.** The
     vacuity-floor detector is a scoped mutation sweep over the harness's emission helpers — a
     surviving mutant in an emission arm IS the vacuity made visible — but each such mutant needs
     the full-tier gate to kill it, so it stays manual-only and unbuilt until a second instance
     justifies the cost. The invariant-softening class has no mechanical detector (a mutation sweep
     cannot distinguish "kept loud" from "absorbed"); a recurrence there is the signal to design
     one, not evidence that review can be skipped.

5. **Extend the decode-conformance corpus along the composition-depth axis.** (Two sibling axes are
   already delivered by the replay gate's default leg, both deriving from each accept vector's bytes
   via pure-byte transforms harness-side, no oracle: the encoding-variant axis — spec-EQUAL
   re-encodings (indefinite framing / non-minimal widths / chunked strings / reversed maps) via the
   shipped `cddl_encoding_fidelity::variants` mutator — and the header-mutation reject axis —
   wrong-major-type and truncated-header mutants (`header_mutants`) asserting decode-Err AND an error
   location naming the rule (`failed in {type_name}`), the annotation analogue of the
   `class="constraint"` rejection-reason (`expect_err`) pin, lifting the per-type annotation contract
   from fixture granularity (the `error_annotation_*` tests) to catalog breadth. The remaining depth
   axis is below.)
   - **Composition depth (low).** The shipped decode-direction harness (`tests/README.md`
     § "Decode-direction conformance") keys its obligation set on the matrix's minimal
     per-construct examples — breadth, not depth. The corpus fixtures (`tests/corpus/*.cddl`) add
     the composition depth those minimal examples lack; extending the harness to mint vectors for
     them should go through the corpus projection (so coverage stays mechanically checkable), not
     a hand-picked fixture list. json/wasm decode surfaces are likewise unminted. The breadth
     layer's first sweep caught two decoder bugs (the map-rep group-choice key-drop miscompile —
     since fixed, pinned by the row's accept vectors — and inline-group occurrence narrowing —
     since fixed as a graceful rejection, pinned by projected
     `contain.occurrence-target.grpent.inline_group.{plus_array,optional_array,bounded_array,zero_map}`
     reject rows and unsupported-row decode catalog absence), so depth is not the current bottleneck.

6. **Over-acceptance pins: a catalog vector class for spec-INVALID CBOR the decoder wrongly
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

7. **Burn down the generated-code clippy allow-list (`generated_code_clippy_clean`).** The gate
   denies `clippy::all` on the generated rust crate but carries eight `-A` escapes so the generator's
   current emission passes; each is an emission-quality shape to stop emitting, and every `-A` removed
   proves the fix generator-wide (the gate exercises the default and `preserve+canonical` profiles).
   `clippy::disallowed_names` is NOT in this list — it is a permanent input-dependent allow (the
   fixture's own `foo`/`bar` rule names become generated parameter names). The eight to retire, each
   fixed by adjusting the emitted shape rather than the lint config, with approximate default-profile
   counts:
   - `unnecessary_cast` (13×) — same-type `as` casts on the deserialize path (`… as u64` where the
     source is already `u64`); emit the cast only when the source and target types differ.
   - `collapsible_if` (9×) — nested `if { if … }` in emitted length/optional-field guards that
     clippy wants joined with `&&`.
   - `needless_borrows_for_generic_args` (5×) — a `&expr` passed where the callee takes the value by
     a generic bound (drop the borrow).
   - `useless_conversion` (3×) — `.into()` / `From` to the same type (`u64` → `u64`).
   - `write_with_newline` (2×) — `write!(… "\n")` that should be `writeln!`.
   - `derivable_impls` (2×, preserve profile) — a hand-emitted `impl Default` clippy can derive.
   - `type_complexity` (1×, preserve profile) — a very complex inline type worth a `type` alias.
   - `unnecessary_lazy_evaluations` (1×, preserve profile) — `.unwrap_or_else(|| None)`-style closures
     that should be the eager form.
   Removing an entry from the gate's `-A` list is the pin: it flips red until the generator stops
   minting that shape, then stays green. The gate already holds `clippy::no_effect` (the retired
   `();` shape) and every unlisted lint hard-red, so a NEW emission-quality regression fails
   immediately. Second axis once the list shrinks: extend the gate to the generated *wasm* crate —
   it currently generates with `--wasm=false`, so the wasm-binding emission path has no lint gate
   at all (the same "compiles green but lint-worthy" class the rust half existed to catch).
   Third axis: deny a curated **rustc** style-lint set (`-D unused_parens`, candidates from the
   `unused` group) alongside `clippy::all`. The gate can't use a blanket `-D warnings` (generated
   code legitimately over-imports, so `unused_imports` must stay a warning), but per-lint denies
   don't have that problem, and the class is real: a redundant-parens emission (`Ok((x))` on a
   single-binding match pattern) shipped and was caught by review, not by any gate — rustc's
   `unused_parens` would have flagged it. The review-found shapes are pinned individually by the
   `snapshot_tests` emission-hygiene needle gates (`deserialize_converts_error_at_most_once`,
   `ok_pattern_parenthesizes_only_tuples`); the lint axis is what would catch the *next* shape in
   the class without a needle per bug. (The doubled-`map_err` shape is the counterexample that
   keeps the needle gates load-bearing even after the lint axis lands: no rustc/clippy lint flags
   a repeated identity error-conversion, so that one stays needle-only.)

- (very very very low priority) MSRV declaration / OS matrix for GENERATED code: the templates' `edition = "2024"` already
  hard-floors the effective MSRV at rustc 1.85 with a self-explanatory compile error, and generated
  output has no platform-conditional code an OS matrix would exercise. Revisit only if a consumer
  reports an MSRV or platform break (dep-driven MSRV creep is the one real vector).
- A docs-vs-behavior conformance harness for `comment_dsl.mdx` / `output_format.mdx` (snippet
  extraction + output spot-checks): emitted output is already pinned by the snapshot corpus and
  DSL-name drift by the `cddl-matrix/verify.ts` forward lint; a doc-snippet system is heavy
  machinery for prose drift that review catches at its actual rate. The decline has narrowed since
  it was made: identifier-existence drift (a cited pin that no longer exists) is now mechanically
  covered by the `lint_doc_citations` gate, so what stays declined is prose SEMANTICS only — a
  sentence whose cited pin exists but whose claim about it is wrong. NB this decision covers PROSE
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
