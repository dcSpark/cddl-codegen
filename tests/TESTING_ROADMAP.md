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
rejections are reason-asserted (the catalog's `expect_err` pins), every spec-valid accept vector
also replays under mechanically-derived spec-equal re-encodings, and header-mutation reject mutants
of each (wrong-major-type / truncated-header byte transforms) must reject WITH an error
location naming the decoding type (`failed in {type_name}`) — the annotation contract at catalog
breadth. A certified silent-acceptance bug is pinned rather than hidden: its spec-INVALID instance
lands as a `class="over-acceptance"` accept vector replayed as "still wrongly accepts" and projected
`enforce = no (over-accepts)` until a fix flips it loudly (no live instances at HEAD — the machinery
stays armed). Both promotion branches have now fired on the widened-occurrence-marker table class:
`contain.occurrence-target.memberkey.type1.plus_table`'s `+` marker was honored as a `NonEmptyMap`, so
its over-acceptance vector was promoted to `class="constraint"` and the row moved to enforce-green,
while the `?`/`n*m` spellings (and the seed instance, the no-occurrence type-domain arrow widening)
took the other branch — closed by graceful rejection at generation, dropping their pins with their rows. The location evidence is itself
validated, not just present: every captured Display's
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
   `tests/recomb/ingredients.json` → `recombination_generation_sweep` (default suite) + the
   profile-parameterized layer-2 gates `recombination_crates_execute` /
   `recombination_preserve_crates_execute` / `recombination_json_crates_execute` /
   `recombination_wasm_crates_check` (full tier), with cited known-class ledgers, the
   `ledger_key_shape_floor` key hygiene gate, and a promotion flow into the pinned collections).
   Residuals, in escalation order:
   - **Embed-site leg for alias-classifying roots — two proven escapes.** A composition whose ROOT
     rule classifies as a plain alias (`rcN = bytes .cbor {…}`, `bytes .cbor (int .ne N)`) emits its
     wrapper (de)serialize code only at an embed/USE site — a bare alias rule emits none — so the
     layer-2 sweeps never compile that emission surface for alias roots (embedding exists only when
     such a template lands INNER under another outer, e.g. the ledgered
     `arr_mid inner=cbor_payload` case). Both escapes were preserve-only compile bugs found by
     review/fixture-TDD in the session that fixed their ledgered `tag_content` siblings (tag rules
     AUTO-WRAP into a struct, so those failed standalone and were ledgered; alias roots stayed
     green), each verified standalone-green/embedded-red against its pre-fix tree: the
     `.cbor`-payload constrained-int E0308 (dropped `final_exprs`) and the `.cbor` inline-table
     owned-serializer `.end()` E0308 — both since fixed and pinned by the embedding holder members
     of the `tagged_constrained_int` / `cbor_bignint_table` corpus fixtures. Mechanical shape, worth
     building on the THIRD instance: mint each alias-classifying root also as an embedded variant
     (`rcN_embed = [e: rcN]` — the matrix probe's embed-holder pattern), scoped to alias roots to
     bound the layer-2 wall-clock cost.
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

4. **Synthesized-name residual: the referenced-but-never-minted (E0425) flavor.** The generator mints
   structural wasm idents (loose `{Elem}List` / `Map{K}To{V}` builders, restricted `NonEmpty*`
   wrappers, table `keys()` list wrappers) whose interactions with USER rule names and with EACH OTHER
   are a NAME-shaped axis the shape catalogs (wasm-ABI matrix, corpus, recombination) never spell. The
   two flavors of the "generation exits 0 but the wasm crate doesn't compile" class that live at
   generation time are owned by standing gates: the duplicate-ident (E0428) redefinition flavor by the
   generation-time backstop at the `generated_files` seam, and the dedup/shape-mismatch (E0277) flavor
   by `synthesized_name_interaction_sweep` (families × interactions → graceful rejection with the
   colliding ident named, or a compiling batched wasm crate; see tests/README.md § "Synthesized-name
   interaction sweep + duplicate-ident backstop"). The remaining flavor is E0425: an emitter that
   REFERENCES a synthesized wrapper name no mint path emits. It stays owned by the compile gates
   (`wasm_matrix_compiles` + the full-tier recombination wasm leg). If it recurs outside their reach,
   escalate to a scoped generation-time invariant — "every synthesized wrapper name an emitter
   references must be in the minted set" — the reference-side complement of the emission-side
   duplicate-ident backstop. (A fourth, CROSS-CRATE flavor — duplicate symbols at link under
   `--extern-wrapper-index`, invisible to any in-crate layer — is owned by that flag's
   deferral-boundaries entry among the small residuals below.)

5. **Small independent residuals (low).**
   - **Reason-keyed rejection evidence for the reject catalogs — one proven near-miss recorded, no
     machinery yet.** The robustness/matrix reject catalogs snapshot the OUTCOME label only
     (`error (graceful)`), so a fixture stays green when a NEW, earlier rejection absorbs the one it
     pins: `finalize` short-circuits on prior rejections, so any parse-time rejection added upstream
     of a finalize-time check silently retargets every fixture whose spec happens to also match the
     new guard — the label is identical, the provenance is wrong, and the originally-pinned boundary
     goes unexercised. Proven near-miss (caught by implementation-time reading, not by any gate):
     the no-occurrence-arrow rejection would have absorbed `tests/robustness/float_table_key{,_composite}.cddl`
     (their occur-less spellings matched the new guard before the float-key finalize check ever
     ran); the fixtures were respelled `*` and their headers document the pattern. The mechanical
     layer, if the class recurs: extend the reject-catalog snapshots from bare labels to a stable
     rejection-reason fingerprint per fixture (e.g. a distinctive message substring, as the decode
     catalog's `expect_err` pins already do for decode errors), so a provenance swap flips the
     snapshot loudly. Same recur-first policy as the invariant-softening/vacuity design rules below;
     meanwhile the working rule for new rejection work is the float_table_key header comment: check
     which existing reject fixtures the new guard can reach, and respell or reason-assert them.
   - **Doubled doc-comment markers in emitted docs — one proven instance recorded, no machinery
     yet.** The `codegen` builder fork prefixes EVERY newline-separated line of a doc string with
     the marker itself, so an emission-site string embedding a literal marker to "continue" a
     multi-line doc ships doubled markers (rustdoc renders the extra slashes as literal text).
     Proven instance: the wasm NonEmpty wrapper doc rewording embedded literal markers after its
     `\n`s and landed at HEAD inside a commit whose snapshot suite was left stale-red; the next
     bless folded the bug into ~660 golden snapshots and only post-bless diff REVIEW caught it —
     the snapshot layer pins bytes, so it flags a change but blesses a defect just as happily, and
     `generated_code_clippy_clean` cannot see it (no clippy lint fires on a doc comment's text).
     Mechanical layer if the class recurs: a catalog-wide scan asserting no doubled-marker line in
     any `*.snap` (cheap, pure grep). Meanwhile the working rules are: bare `\n` in `.doc()`
     strings (the builder owns the markers), and the § "Blessing changes" discipline — review the
     blessed diff, never accept blind, ESPECIALLY when snapshots were already red before your
     change (the diff then contains someone else's unreviewed delta interleaved with yours).
   - **Emitted-shape lint classes OUTSIDE `clippy::all` are beyond `generated_code_clippy_clean`'s
     reach — one proven instance recorded, no machinery yet.** Review of the wasm burn-down
     retirement (dropping the identity `.into()`) exposed `Holder::new(val.clone())` in a wasm
     ctor — a clone of an owned, last-use argument (the boundary ops clone every non-Copy expr
     regardless of the call site's ownership). `clippy::redundant_clone` would flag it but is
     nursery-tier, so the gate's `-D clippy::all` cannot see the class; behavior and bytes are
     unaffected (allocation cost only). Disposition mirrors the curated rustc style-deny precedent
     (`unused_parens` et al.): evaluate specific beyond-`all` lints one at a time, adding a per-lint
     deny only if it is currently green-able on both profiles (nursery lints carry known false
     positives). Act on a second instance or a consumer report, not before.
   - **Bounded occurrences beyond `+` (`n*m` / `*n` / `n*` with n ≥ 2) — half runtime-checked,
     half rejected, neither type-enforced.** Current state is asymmetric by representation:
     ARRAYS (`[2*5 foo]`, `[*3 foo]`) are supported via serialize/deserialize-time length checks
     on a bare `Vec<T>` — correct on the wire but bypassable at the API (the exact class the
     two-type design's "Problem" section names; `is_non_empty_array`'s doc comment marks the
     boundary) — while bounded TABLE markers reject gracefully at parsing's detection arm, pinned
     by the `tests/matrix_reject/` rows
     `contain.occurrence-target.memberkey.type1.bounded_table` /
     `contain.occurrence-target.memberkey.bareword.zero_bounded_map` (the `HomogenousMap` doc
     comment guarantees only `*` and `+`/`1*` bounds ever reach generation). The open FEATURE is
     type-level enforcement for both: sibling statics `BoundedVec`/`BoundedMap` following the
     bounds-general API rule in `draft/two-type-constraint-enforcement.md` § "Support for more
     complex occurrences" (an operation is checked iff it can cross a bound; value-`&mut` is
     unconditionally safe; one `TryFrom` door reporting the decoder's own
     `RangeCheck { min, max }`; per-shape siblings rather than redefining `NonEmpty*` as
     const-generic aliases — fallibility cannot follow a const param). Implementation retraces
     the shipped `+` seams: parsing's bounds plumbing (`GroupParsingType::HomogenousArray`/
     `HomogenousMap` already carry `(Option<i128>, Option<i128>)`), member/alias type selection
     beside `is_non_empty_array`/`is_non_empty_map`, `static/` runtime + json/schemars companions
     gated by a usage predicate (the `uses_non_empty_map` precedent), wasm Min/Max-suffixed
     wrappers per the design doc's naming convention (which join the synthesized-name interaction
     families of the sweep item above), deserialize collect-then-`try_into`.
     Tests-first encoding: the table reject rows flip to enforce-green through the same promotion
     flow `plus_table` took (over-acceptance vector → `class="constraint"` reason-asserted
     rejection); arrays gain decode vectors pinning boundary counts (accept at min/max,
     reason-asserted reject at min−1/max+1) plus wasm-ABI matrix shapes. Matrix-side row-flip
     detail — including the `{ k => v }`-as-bounds-`(1, 1)` revisit — lives in the two
     candidate-feature entries in `cddl-matrix/ROADMAP.md` (the "Real bounded `?` / `n*m` table
     cardinality" entry and its two-type sibling). Build when a real spec needs it (a consumer
     request or a corpus fixture), not before.
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
     (`multifile_matrix_compiles` measured ~35 s cold / ~30 s warm at 43 cells; 59 at HEAD). If wall-time
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
     (retry-on-ENOLCK) or escalate a genuine WouldBlock as a std/kernel finding. A THIRD probable
     sighting (2026-07-09, `test` gate, local tier, under the same nested-cargo load profile) was
     burned exactly the way this discipline warns about: the run was piped through `tail`, so no
     `failures:` list or errno survived and two immediate full-suite reruns were green —
     unattributable, evidence value zero. The discipline is load-bearing for one-command runners
     too: pipe `check.ts` to a FILE, never through `tail`, even for the local tier.
   - **Mechanical layers for the two review-owned design rules in `tests/README.md` § "Design
     rules" (invariant-softening, vacuity-floor witness) — build only if a class recurs.** The
     vacuity-floor detector is a scoped mutation sweep over the harness's emission helpers — a
     surviving mutant in an emission arm IS the vacuity made visible — but each such mutant needs
     the full-tier gate to kill it, so it stays manual-only and unbuilt until a second instance
     justifies the cost. The invariant-softening class has no mechanical detector (a mutation sweep
     cannot distinguish "kept loud" from "absorbed"); a recurrence there is the signal to design
     one, not evidence that review can be skipped. The vacuity-floor class also has a first
     on-record instance in a NEEDLE gate (a flavor the harness-scoped mutation sweep would not
     cover): during the collapsible_if burn-down, the retargeted
     `corpus_special_map_key_supported` needle initially counted its indefinite gates by the proxy
     pattern `if matches!(` — any unrelated future matches-gate would pad the count and mask an
     ungated Special peek — and review fixed it to witness the gate's own
     `, cbor_event::Len::Indefinite)` pattern (mutation testing cannot see this flavor either: the
     mask needs an unrelated emission to APPEAR, which no mutant simulates). A second proxy-witness
     needle instance is the signal to design a detector for that flavor too, same policy.
   - **`--extern-wrapper-index` deferral boundaries — NonEmpty structural wrappers are not defer
     candidates.** The deferral (pinned by `extern_wrapper_index_defers_to_dep`) covers only the
     loose list/map wrapper emitters (`generate_array_type` / `codegen_table_type`):
     `generate_non_empty_array_type` / `generate_non_empty_map_type` never consult
     `try_defer_wrapper`, so a dep index listing a `NonEmptyVec`/`NonEmptyMap`-backed wrapper plus
     a consumer spelling the same anonymous nonempty list/table shape over that dep's extern
     element re-mints it (duplicate-symbol link error, exactly the class the flag exists to remove);
     conversely `codegen_table_type` defers a structural map even when it was requested as a
     NonEmpty wrapper's `try_from` source, which would dangle that source type. Both need the
     opt-in flag AND an exotic cross-crate nonempty shape no consumer has spelled; extend
     `try_defer_wrapper` into the NonEmpty emitters (and exempt `try_from`-source mints) when one
     does. Related name-interaction note: a USER rule claiming a dep-indexed structural name is
     never deferred (the rule-declared guard), so it duplicate-symbols at link — the CROSS-CRATE
     flavor of the synthesized-name interaction class, which the shipped in-crate layers cannot
     see (the duplicate-ident backstop scans one crate's own files;
     `synthesized_name_interaction_sweep` spells no dep-index cells — see `tests/README.md`
     § "Synthesized-name interaction sweep + duplicate-ident backstop"), so it is owned HERE: a
     dep-index cell joins the deferral-profile leg below when this entry's recur-first trigger
     fires. The CLASS here — a per-wrapper emission MODE
     (local vs deferred) crossed with the wrapper-shape space — is an axis no existing honesty rule
     sweeps (the wasm-ABI matrix's SHAPES/ROLES cover what types look like and where they sit; the
     third honesty axis covers flag × input mode; neither enumerates flag × shape). These two gaps
     were found by reading the emitters during review, not by any gate. Mechanical layer on the
     SECOND read-caught or consumer-reported instance of the class: a deferral-profile leg over the
     wasm-ABI matrix's extern-capable shapes — each shape probed once with a dep index listing its
     structural name and once without, asserting deferred-import vs local-mint and a wasm32 link —
     rather than accreting per-shape hand fixtures.
   - **Extern-deps wasm-boundary surface: compile-gated, not behavior/packaging-gated.** The
     split-dep cell (`integration_tests::extern_deps_wasm`, `--extern-wasm-crate`) proves the
     generated wasm crate *builds* against a mapped dep wasm crate; the sibling index fixture
     narrows this: `extern_wrapper_index_defers_to_dep` (`tests/extern-deps-wasm-index`) now runs a
     cross-crate behavioral wasm round-trip (`tests_wasm.rs` through deferred dep classes) AND a
     real `wasm32-unknown-unknown` link build (the only gate compiling any generated crate for the
     actual wasm target). For the ORIGINAL fixture three layers above "it builds"
     stay unprobed: (a) wasm-side behavioral round-trips through the cross-crate wrappers — the
     fixture ships no `tests_wasm.rs`, so a semantically wrong boundary conversion (`get`/`add`
     through the dep's `From` impls) compiles green; the existing behavioral floor
     (`tests/core`/`tests/canonical` `tests_wasm.rs`) never crosses crates. Cheapest close: a
     `tests_wasm.rs` in `tests/extern-deps-wasm` constructing `ExternCrateFooList`/the table
     wrapper — and the `nested` module's non-root-use wrappers (`ExternCrateBarList`, the
     `NestedItem`/`ExternCrateBar`-keyed maps), whose rust-side round-trip exists but whose wasm
     accessors don't — through the wasm API and round-tripping. (b) `wasm-pack`/bindgen-CLI packaging —
     `cargo build` cannot see duplicate exported JS class names when the dep wasm crate and the
     consumer both export a like-named wrapper (the generate-locally policy makes this reachable);
     no gate runs bindgen-CLI over the extern fixture. (c) json-gen against extern-dep types —
     `gen_json_schema!` now emits the dep's rust path, but no gate generates or executes
     `--json-schema-export` with extern deps (needs the dep in the json-gen manifest and a
     `schemars::JsonSchema` impl on the dep type, both user responsibilities by design).

6. **Extend the decode-conformance corpus along the composition-depth axis.** (Two sibling axes are
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
  `extern-dep-crate` stand-in) — plus the wasm-boundary cell `integration_tests::extern_deps_wasm`
  (`--extern-wasm-crate` against the split `extern-dep-crate`/`extern-dep-crate-wasm` pair; extern
  types as list elements and table keys/values, from root AND non-root use sites — the non-root
  `nested` cells pin the wrapper-element imports registered from the wrapper's emission scope, which
  root use sites would mask) — so those specific cells are pinned without enumerating the rest.
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
- (low) Assertions on `export()`'s stderr diagnostics — the legacy-root warning and the
  comment-preservation stale-file scan (an orphaned `.rs` under a generated tree) print via
  `eprintln!` in-process, which `cargo test` can't capture without a subprocess harness. The
  behaviors' *output-byte* halves are pinned (seed-once, overlay tests); only the warning text
  itself is unasserted. If either diagnostic grows logic, run the CLI as a subprocess (the
  `run_test` pattern) and assert on captured stderr.

## Sources
- Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`
- Per-dimension expert write-ups: `draft/testing-recommendations/*.md`
