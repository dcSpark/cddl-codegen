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

How to read this file: work is grouped by what unblocks it, not by a single priority number.
"Pending maintainer action" needs a human decision or a long unattended run; "Next work items" are
buildable now; "Standing-system residuals" are recur-first ledger entries (each names its trigger
and the mechanical layer to build when the trigger fires — building early is deliberate
over-engineering); "Deferred features" wait on a real consumer need; "Operational watches" are
known noise sources with a capture discipline; "Declined" records what we decided NOT to build and
the signal that would reopen the decision.

## North star — automated feature coverage

The direction all of this points toward: **an automatic flow that ensures every CDDL construct worth
supporting works properly through the generated library — verified by execution, not by hand.**
Concretely: enumerate the feature space (`cddl-matrix/` already does this), then for each construct
*generate → compile → execute* (round-trip real values, reject invalid ones), and treat every
failure as one of two things — a construct we deliberately don't support (documented, not a gap) or a
bug to fix — closing the loop until coverage is complete and self-checking. "Worth supporting" is
load-bearing: some constructs (`#`/`cbor-any`, `float16`, socket plugs, …) are design decisions to
*exclude*, not holes to grind toward 100% — and the exclude list is revisable: `any` (the prelude
name) moved OFF it 2026-07-23 (maintainer-ruled feature), and the loose-CBOR family that followed
it is shipped CURRENT STATE, not roadmap — first-class `any` on every surface, open struct-maps
(trailing `* K => V` rest rows, capture and `@ignore` tolerate-and-drop flavors), and open arrays
(final-position `* t` rest tails, both flavors). Per-layer test maps: `tests/README.md` § "Open
struct-maps (rest rows)", § "Open struct-maps — the `@ignore` (tolerate-and-drop) flavor", and
§ "Open arrays (rest tails)"; user-facing contracts: `docs/docs/current_capacities.mdx` /
`output_format.mdx` / `comment_dsl.mdx` / `wasm_differences.mdx`. The family's v1 boundaries
(non-final/multiple/plain-group rest rows, `+`/bounded occurrences, and the two key domains a row
cannot key on — float-containing, which has no total order, and `null`-admitting, which the row's
key dispatch cannot tell from an indefinite map's break) are graceful rejections whose
candidate-feature entries live in `cddl-matrix/ROADMAP.md` § findings, not here. The permanent exclusions around it stay: `#` (`Type2::Any`), `cbor-any`, `@newtype` and
control operators on `any`, and non-last bare `any` choice arms (forced-backtracking catch-all unions
put the catch-all last, so an earlier one is unreachable dead code) — see the `any` type-choice entry
in `cddl-matrix/ROADMAP.md`. One deferred coverage item remains: the wasm emit-tests minter has no
`any` ctor path (`src/emit_tests_wasm.rs`, `Any => None`), so a minted `any` value gets no wasm-side
round-trip differential (the rust leg's mint covers it); lifting it follows demand, since the wasm
`AnyCbor` wrapper is byte-oriented and exposes no value-destructuring constructor.

The value is in removing the human from that loop without losing trust: the gates must be strict
enough that "the matrix says this feature is supported" *means* it generates, compiles (rust **and**
wasm), and round-trips. The historical verdict meant only "rust generates and rust-`cargo check`s,"
which is why real bugs (inverted `nint` bound, wasm `get`/`add` type mismatch, JSON-schema turbofish)
shipped green. That oracle now exists as a set of live systems — execution-gated support verdicts at
matrix/corpus/suite breadth, per-non-default-profile probes and foreign spec-derived decode vectors,
identity-carrying decode evidence (reason-asserted rejections, spec-equal re-encoding replays,
header mutants asserting an error location that names the decoding type), and an armed
over-acceptance promotion flow whose both branches have fired. All of that is *current state*,
documented in `tests/README.md` (suite-side; § "Decode-direction conformance" for the evidence
contract and its validation layers) and `cddl-matrix/README.md` (probe-side; § "Directional support
evidence"). The json/wasm decode surfaces carry the same obligation now (the json/wasm surface legs
on both replay gates — `tests/README.md` § "json/wasm surface legs"). What remains on this axis lives
in the sections below (the fuzzer escalations, the recur-first residuals), not here.

## Pending maintainer action

- **Complete the `cargo-mutants` sweep and triage the survivors.** The system is built and its
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

- **`prettyplease` instead of shelling to `rustfmt`.** Removes toolchain-dependent formatting
  churn and the `which` dependency, compiles fast, never bails (it reuses `syn`, already built
  transitively via the proc-macro derives). No longer just a churn mitigation: shelling to rustfmt
  is a proven correctness exposure — rust-lang/rustfmt#5703 (an internal error on over-width
  `pub(crate)` tuple fields, fatal under `export.rs`'s deliberate non-0/3-exit-is-fatal contract)
  aborted consumer regens outright and forced a `#[rustfmt::skip]` + hand-canonical-layout
  workaround into shipped output — now at TWO emission sites (the wasm wrappers and the rust-crate
  default-profile newtype tuple field), sharing one guard owner
  (`push_overwidth_guarded_tuple_field`, `src/generation/mod.rs`; pinned both directions per site by
  `integration_overwidth_wasm_wrapper_field_gets_rustfmt_skip` /
  `integration_overwidth_rust_newtype_field_gets_rustfmt_skip`, and held across the whole hazard
  range by `integration_tuple_field_width_ladder_never_aborts_rustfmt`, whose doc comment records
  the measured 6-char fatal WINDOW — rendered type lengths 86..=91 abort while both 85 and 92
  format clean — that makes a dense ladder, not a huge-name fixture, the only meaningful gate
  shape). A SECOND proven instance of the
  same exposure class: rustfmt's comment-ownership misattribution (the open upstream family of
  rust-lang/rustfmt#6347 / #4654 / #3527) folds an own-line comment after a match's last arm onto
  that arm's `}` as a trailing comment — which rewrote the preserve overlay's own emitted
  `// cddl-codegen:` markers into a form the overlay's next-run scan could not read, breaking
  run-twice=run-once until the overlay grew an unfold pre-pass (pinned by
  `preserve_markers_survive_rustfmt_fold_roundtrip` and the
  `replace_rustfmt_folded_tail_arm_markers` fixture family;
  probed edition-independent and idempotent-on-folded-output, so the folded form is a
  genuine fixed point under current stable). `prettyplease` has no
  internal-error/bail class and does not re-own comments, so adopting it retires both workarounds
  (grep `rustfmt::skip` in
  `src/generation/` — the over-width branch now lives once, in
  `push_overwidth_guarded_tuple_field` — and remove it in the same change; the unfold pre-pass must
  STAY for files already committed in the folded form) — weigh that retirement
  plus the exposure against the emitted-token-stability constraints in AGENTS.md (the snapshot
  corpus and comment-preservation overlay key on the formatter's exact output, so the swap
  re-blesses broadly and must hold the overlay's idempotent-fixed-point property). The swap's
  overlay risk now has a mechanical acceptance suite waiting for it:
  `preserve_fixtures_rustfmt_cycle_stability` sweeps every expected-case preserve fixture through
  `rustfmt_generated_string` and asserts the post-format on-disk fixed point, so pointing that
  seam at prettyplease turns the corpus into the swap's per-structure acceptance run on day one
  (while the width ladder above largely dissolves under the swap — prettyplease has no
  internal-error class, so the exit-contract tripwire it leans on stops being where formatter
  surprises surface).

- **Mint decode-conformance + confirm matrix coverage for the tag-258 reject-default flip.** The
  well-known-tag registry (`parsing::well_known_tag_default_duplicates`) defaults a no-directive
  tag-258 set to `@duplicates reject`. The registry's DEFAULT path (as opposed to an explicit
  `; @duplicates reject`, already covered by `tag_set_reject`) currently has **no duplicate-REJECT
  wire-vector coverage** in the decode-conformance catalog: the new `tests/corpus/tag_set_default.cddl`
  fixture pins the emitted source shape (`feature_corpus` snapshots) and its `cargo check` compiles,
  but no spec-derived reject vector exists for it yet (accept vectors are minted; the flip's other
  confirm legs — the `contain.choice-member.type2.tag.set_idiom` matrix cell re-verifying
  post-flip with a minted wasm surface, and the golden-suite integration gates compiling + KAT-running
  the `@duplicates preserve` opt-outs — have both run green and are retired from this entry).
  The remaining piece is the wire-level duplicate-REJECT pin, which is BLOCKED on a catalog-model
  extension: a duplicate-bearing set is spec-VALID CDDL (`[* uint]` permits duplicates; only
  the tag-258 registry semantics narrow it), so `class="constraint"`'s re-validated
  spec-INVALID invariant cannot hold and `class="over-acceptance"` is its inverse. Needs a new
  vector class (spec-VALID, policy-rejected: both oracles accept, our decoder rejects BY
  DESIGN, `expect_err` pinning the `DuplicateKey` door) threaded through the mint + replay
  gate before the hand vector can exist. Until then the reject default's wire behavior is
  pinned in-process only (`reject_set_duplicate_wire_and_api_identical` covers the shared
  door). Heavy-tier when reopened: coordinate the run (`/tmp` scratch and disk contention —
  see the ENOSPC entry — so get an explicit go-ahead while another session is active).

## Next work items, in priority order

1. **The two write paths inside `export()`'s write tail that still have no direct case.** The tail
   is one implementation now (`generation::write_tail`), driven directly by
   `src/tests/write_tail_tests.rs` — a synthetic file map and a temp dir, no CDDL, no
   `IntermediateTypes`, no `GenerationScope` — which is where the seed-once roots, the manifest
   changeset merge, the family-wide post-overlay import re-prune, never-silent comment handling,
   run-twice = run-once, the no-prior-output bound, the stale-file scan and the byte-inertness of
   the diagnostic reads are pinned. Two of the tail's writes are still exercised only incidentally,
   by whichever e2e cell happens to reach them:
   - **the composed runtime statics**, which take the per-file `write_rs_with_preserve` route —
     the ONE overlay path that is not map-level, so it is the one whose preservation cannot be
     inferred from the map-level cases;
   - **the `--export-static-crate` target's writes**, whose new-static-file notice is an existence
     check against a HAND-OWNED crate the tool otherwise never touches.
   Each is one `WriteTailPlan` field away (`composed_runtime_files` / `static_crate`), so build them
   as two more cases in that module rather than as e2e cells: the point of the extraction is that a
   write path's contract no longer needs a spec to state it.

2. **Grammar-fuzzer escalations.** The lazy-first shape-recombination fuzzer is shipped
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
     such a template lands INNER under another outer, e.g. the
     `arr_mid inner=cbor_payload` case). Both escapes were preserve-only compile bugs found by
     review/fixture-TDD in the session that fixed their ledgered `tag_content` siblings (tag rules
     AUTO-WRAP into a struct, so those failed standalone and were ledgered; alias roots stayed
     green), each verified standalone-green/embedded-red against its pre-fix tree: the
     `.cbor`-payload constrained-int E0308 (dropped `final_exprs`) and the `.cbor` inline-table
     owned-serializer `.end()` E0308 — both since fixed and pinned by the embedding holder members
     of the `tagged_constrained_int` / `cbor_bignint_table` corpus fixtures. Mechanical shape, worth
     building on the THIRD instance: mint each alias-classifying root also as an embedded variant
     (`rcN_embed = [e: rcN]` — the matrix probe's embed-holder pattern), scoped to alias roots to
     bound the layer-2 wall-clock cost. Note the POPULATION shrank on 2026-08-04 without the trigger
     moving: both recorded instances were `.cbor` roots, and a `.cbor` rule body now force-wraps into
     a struct that emits its (de)serialize code standalone, so the sweeps compile that surface
     directly. What still classifies as a plain alias root is the named collection (tagged or not),
     the scalar re-alias and the `T / null` collapse — that is the class a third instance would have
     to come from, and it stands at two.
     The class's SEMANTIC flavor is DELIVERED (2026-08-04) and is not a matrix leg. It was: a bare
     `.cbor` alias root's STANDALONE (de)serialization was the target type's own
     (`y = bytes .cbor int` → `pub type Y = Int`), so the root's `to_cbor_bytes` wrote the bare
     inner form and its `from_cbor_bytes` accepted that form while rejecting the spec's actual
     byte-string one — silently, in a crate that compiled everywhere, while embed sites wrapped
     correctly. A `.cbor` rule body now force-wraps into the wrapper struct the `@newtype` spelling
     produces, so the class is closed at the source rather than detected: the broader invariant it
     was an instance of — no wire-affecting property of a `RustType` may survive on a root that
     emits a transparent alias — is a hard refusal in
     `IntermediateTypes::assert_no_wire_facts_survive_a_transparent_alias`, which fires for any
     FUTURE encoding operation at the one registration seam that could carry one (its one carve-out,
     tags on a named collection / `T / null` collapse, is ledgered in `cddl-matrix/ROADMAP.md`
     § Findings). The emitted-code half — the part no generator assert can speak for — is executed by
     `integration_tests::cbor_rule_body_standalone_codec_agrees_with_its_embed_site` (`local` tier),
     which runs the wrapped-form/bare-form/embed-agreement table on a scalar and a record payload
     under the plain and preserve profiles. The embed-site COMPILE leg above keeps its own
     third-instance trigger unchanged: it is a different instrument on a different failure class,
     and it still stands at two instances.
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
     (batch size 1 — the exhaustive oracle, but hours of wall-clock). Both costs are FIRST-run
     prices now: layer-2 batches are gate-cached per generated tree, so a permutation's or
     singleton's re-runs skip every batch whose composition didn't change — the detector got
     materially cheaper to adopt. Either also owns the
     misfiled-ledger-contradiction class (an entry claiming profile-specificity while masked in its
     home profile — how this instance actually surfaced). (The observed-baseline comments beside
     these gates' floors have their own rot class — see the standing residual below.)
   - **Real-world corpus differential** (see `draft/testing-recommendations/RECOMMENDATIONS.md`):
     synthetic breadth vs real-world depth — recombination does not replace it.

3. **Duplicates-policy residuals.** Both `@duplicates` flavors are shipped on every boundary —
   `reject` (set/array uniqueness twins) and `preserve` (table pair-map twins), covering rust,
   preserve-encodings, canonical, JSON/schemars, wasm, extern-interface projection, and the
   `dsl.duplicates.{reject,preserve}` matrix feature rows. Current state lives in
   `tests/README.md` § "Per-rule duplicates policy (`@duplicates`) — test map" (the per-layer
   pin inventory) and the user docs (`docs/docs/output_format.mdx`, `current_capacities.mdx`,
   `wasm_differences.mdx`, `comment_dsl.mdx`). What remains:
   - **Set-nominalization residuals (Delivery 2 of the set-architecture rethink —
     `d177516`/`816969f`/`13b63a2`/`7daca67`).** Named, generic-instance, and inline `#6.258` sets
     are now nominal wrapper types owning their `{tag, len, elem}` encodings. Three follow-ups
     outlive the delivery:
     - *`From` between same-shape set instantiations.* `set<xs_int>` and `set<[* uint]>` (where
       `xs_int = [* uint]`) mint DISTINCT nominals over the same wire shape by the
       spelling-is-identity rule; a member-wise-encoding `From` between two such structurally
       identical nominals would let a consumer convert without a rebuild, but it needs
       structurally-identical-nominal-pair DETECTION that does not exist yet. Niche — CML writes
       sets through one generic `set<a0>`, so it never mints a same-shape pair; build it when a real
       spec mints the pair. The set ergonomics that DID ship (`Deref`/`DerefMut`, both
       `IntoIterator`s, `From`/`TryFrom` Vec, and the std set contract — `insert`/`contains`/
       `Extend`/`FromIterator`/`sort`, the twin refinement doors, `try_opt_from`) are inventoried
       in the user docs (`docs/docs/output_format.mdx` § "Nominal tag-258 set types").
     - *Dedicated collision message for the generic-instantiation naming family.* An inline
       shape-derived nominal name colliding with a user rule or a structurally-different generic
       instantiation is refused with a set-specific message
       (`inline_258_nominal_name_collision_is_rejected`). The GENERIC-INSTANTIATION family
       (`SetKeyHash` from `set<key_hash>` vs a user `set_key_hash` rule) still falls through to the
       generic duplicate-top-level-ident backstop, whose "list/map wrapper families" text is
       misleading for a set nominal — a dedicated per-kind sibling (the decided per-naming-family
       detector structure) is message polish, deferred until a real collision makes the generic
       message actively wrong.
     - *Member-position `@duplicates` extension.* The inline opt-out is hoist-to-named-rule (no new
       DSL surface); a member-position `; @duplicates` directive on an inline set was DEFERRED until
       a real spec demands it. It looks one-line (the member-position rejection seam in `parsing.rs`
       is ready) but carries hidden machinery: a per-site policy on a named-rule or generic-instance
       reference forks one shared type into two, so it must be restricted to inline shapes, and even
       there the shape-deduped synthesized names would need the policy folded into type identity
       plus policy-variant name disambiguation. Every directive is permanent surface (parser,
       LOCKSTEP `corpus_detect` mirror, docs, wasm matrix); the deferral costs no architectural
       rework (the rejection seam stays), and CML-shaped CDDL never hits it (Cardano writes sets
       through the named generic `set<a0>`, where the rule-level directive already works). Reopening
       signal: a third-party author who won't restructure a spec to hoist.

     The wire-level duplicate-REJECT decode vector for the 258 reject default (the spec-VALID,
     policy-rejected vector class blocked on a catalog-model extension) is the separate
     "Mint decode-conformance + confirm matrix coverage for the tag-258 reject-default flip" entry
     above; it now applies to the nominal-set inner too, which routes duplicates through the SAME
     `OrderedSet::try_from` / `DuplicateKey` door, so no second entry is needed.
   - (The per-role wasm-ABI/multifile grid rows for both flavors are delivered — the
     `rset`/`nerset`/`rseta`/`nerseta` and `pmap`/`nepmap`/`pmapa`/`nepmapa` `SHAPES` entries;
     inventory in `tests/README.md` § "Per-rule duplicates policy (`@duplicates`) — test map".
     The recursive-union-VALUED table this fixture exercises (`md = mdmap / [* md] / …`,
     `mdmap = { * text => md } ; @duplicates preserve`) carries no residual: re-probed 2026-08-01 at
     `0a0f969e`, its standalone holder generates at exit 0 and the emitted rust and wasm crates both
     `cargo check` clean under BOTH the default and `--preserve-encodings` profiles, and
     `table_preserve.mdmap` sits in `tests/decode_conformance/corpus_catalog.toml` with accept
     vectors and no `pinned_reason`. The recursion-in-the-DOMAIN sibling carries none either: the
     union-rooted ordering that used to abort now defers its keys-list mint to finalize, and both
     rootings hold a compile-and-wire floor in the `key_`/`ukey_`/`upres_` blocks of
     `tests/recursive-collection-ref/input.cddl`.)

4. **Lint-provocation shapes for `generated_code_clippy_clean` (partially systematic at best).**
   The gate itself already exists and denies `clippy::all` over the generated rust and wasm crates
   on three cases (`generated_code_clippy_clean`, local tier; documented in `tests/README.md`) —
   yet lint classes still arrive consumer-reported when the gate's rich input is provocation-POOR
   for the shape that mints them. The gate's third case answers that for shapes the rich fixture
   cannot host at all: a minimal scratch-written spec under `--preserve-encodings
   --annotate-fields=false` covering verify-only fixed bool/null in member position and in all
   three arm positions, which is where a `clippy::no_effect` `();` and a `clippy::bool_comparison`
   `x != true` both shipped inside the deny set while the gate stayed green. The gate's rich input
   (`tests/canonical/input.cddl`) carries the
   identity-op provocations (`clippy_neg_bounded` — a record-field bounded `nint` whose deserialize
   RangeCheck exercises the no-`as i128`-cast path; `clippy_wrapped_map` — a `@newtype` over a map
   whose rust `From` impl must be `new(inner)`; `clippy_enum_record` — a record with a c-style enum
   field whose wasm ctor/getter cross by value), so a regression re-minting any of those
   `clippy::unnecessary_cast` / `clippy::useless_conversion` shapes is hard-red in both profiles.
   The still-open work item is the THRESHOLD/shape-dependent default-warn lints the input still
   can't provoke: it has no `/` choice asymmetric enough for `large_enum_variant`'s 200-byte
   default (a synthetic provocation needs a ~30-uint-field record variant), and the static
   `DeserializeError` sits just under `result_large_err`'s ~128-byte threshold on this input while
   exceeding it in CML's CI. Add a deliberately lint-provoking asymmetric choice so the NEXT
   default-warn lint of that class — e.g. one a toolchain bump introduces — goes red in-repo
   instead of in a consumer's CI; those two lints are already permanently silenced by the generated
   root's allow, so for them the shape merely pins that the allow keeps covering what it was added
   for. Known residual that keeps this "partial": threshold-adjacent lints fire spec- and
   layout-dependently, so the gate certifies "no lint fires on shapes we provoke", never "no
   consumer CI will trip"; the consumer-report channel stays load-bearing for that remainder, which
   is why those two lints are allowed at the generated root rather than chased per-spec.

5. **One rustfmt-seam error leg still has no witness, and it is the one that needs a
   subprocess-scoped test harness.** The seam's non-0/3-exit-is-fatal contract — which both the
   width ladder (`integration_tuple_field_width_ladder_never_aborts_rustfmt`) and the
   preserve-fixture rustfmt sweep (`preserve_fixtures_rustfmt_cycle_stability`) cite as their
   entire assertion mechanism — now has direct witnesses for its two former holes: non-UTF-8
   formatter output is an `Err(InvalidData)` rather than an `Ok` carrying the UNFORMATTED input
   (`rustfmt_non_utf8_output_is_an_error`, with an echoing-stub control so the failing leg cannot
   pass for being stubbed at all), and an unspawnable formatter binary is a clean `Err` rather than
   an unwrap backtrace (`rustfmt_unspawnable_binary_is_an_error_not_a_panic`). Both drive
   `rustfmt_source_with`, which takes the binary path as an argument precisely so a stub needs no
   `RUSTFMT`/`PATH` mutation — those are process-global and every concurrent formatter call in the
   same test process would pick the stub up. What that mechanism cannot reach is `rustfmt_path()`'s
   OWN `Err`: no `RUSTFMT` set AND `which` failing, which requires a `PATH` with no rustfmt on it —
   process-global again, and racing the tests that spawn `cargo`. The layer that would cover it is a
   subprocess-scoped harness: re-exec one test in a child process with a controlled environment,
   assert its exit and stderr. Reopening signal, on the axis the harness's cost is amortized over:
   the COUNT of error legs that need a controlled process-wide environment to drive, measurable by
   whoever next writes one — today it is one, so a SECOND is what makes the harness worth building
   rather than the leg worth skipping.

6. **Positional-diversity fold family for the preserve-fixture corpus — the authoring work that
   gives the rustfmt-cycle sweep discovery power.**
   `preserve_fixtures_rustfmt_cycle_stability` holds the post-rustfmt on-disk fixed point over
   every fixture, but only over fold positions the corpus expresses — its own delivery record is
   explicit that it would not have caught its motivating escape, because no pre-escape fixture
   held a marker at a foldable tail. The gap is NOT in the unfold pre-pass:
   `unfold_trailing_markers` cuts on any non-own-line `cddl-codegen:`-tagged comment regardless
   of the construct it trails, so the pre-pass generalizes over positions by construction. What
   has no standing witness per position is the WHOLE chain — rustfmt folds the marker, the
   pre-pass unfolds it, the merge re-applies the block, and the on-disk form reaches its fixed
   point — where indentation, marker geometry, and block-body reconstruction genuinely differ by
   construct. The family: fixtures placing tagged blocks at each grammar-level trailing position
   rustfmt can fold a comment onto — last match arm is in-corpus (the `*_rustfmt_folded_tail_*`
   family); last statement of a block, if/else chain tails, struct-literal tail, last enum
   variant, and the closing brace of a nested module/impl are not — across the three block
   flavors. Authored as ordinary preserve fixtures, the sweep covers each automatically with zero
   harness change (that compounding is the shipped design). Honest bound to keep with the entry:
   this enumerates trailing-position folds only; rustfmt comment mechanisms that are not
   trail-folds (the proven import-reorder marker-glue class recorded in the standing-system
   residuals below) are a different family, and no finite fixture set anticipates a future
   formatter's novel comment re-owning — the sweep's version-bump tripwire is the instrument for
   those.

7. **Cross-version preserve vectors beyond the std→alloc rewrite.** The preserve corpus is a
   SAME-VERSION suite by construction: every case's `old.rs` and `new.rs` agree on generated code
   bytes except where the fixture deliberately drifts one item, which is the shape a re-run of one
   tool version produces. A tool UPGRADE is the other shape — it adds tokens and rewrites others
   across a whole file at once, so every anchor shifts and each item the rewrite touched drifts —
   and no vector expressed it until the no_std false positive arrived from a consumer with a
   `compile_error!` sitting three lines above the comment it claimed was lost
   (`cross_version_std_to_alloc_rewrite_self_cancels` /
   `cross_version_rewrite_traps_only_the_reworded_line`). Those two cover the rewrite class the
   upgrade actually performed; what has no witness is the same whole-file-shift shape crossing the
   overlay's other units — an insert block, a replace block's needle, and a `keep` run whose
   anchoring statement the upgrade rewrote — where the recorded original is matched by TOKENS and
   no text-presence escape hatch exists. Authored as ordinary preserve fixtures, so the corpus and
   its rustfmt-cycle sweep absorb them with zero harness change. Reopening signal, measurable by a
   consumer that already has the problem with two runs and a diff: an upgrade regen emits
   `cddl-codegen:unpreserved-comment` or a trapped block that a second, otherwise-unchanged regen
   does NOT reproduce — a block that self-clears is a false positive by construction, which is
   exactly how the no_std one was identified.

8. **A container construct the conceptual type visitor walks FLAT has no combinatorial wasm-compile
    coverage — its placement behaviour rests on one hand cell per construct.** Most of the IR's
    containers are `Map`/`Array` nodes a walk meets as composites; a few are assembled from inner
    types stored separately, so every walk that reasons about containers has to be told about them
    one site at a time. Today there are two — the open struct-map rest row (`* K => V`) and the open
    array rest tail (`* T`), both re-assembled through `RestRow::container_type` and minted
    explicitly in `generation/mod.rs` because the conceptual visitor never sees them as composites.
    Each is pinned cross-module by exactly one hand fixture cell (`tests/multifile`'s `open_flat` /
    `open_nested` / `open_tail` in `qux.cddl`, whose key/value/element live in `a`, `a/c/foo` and
    `b/bar`, compiled wasm-side under both fixture profiles) rather than by a grid.
    - **What one hand cell per construct does not buy, established by the escape that produced this
      entry.** A rest row whose inner types lived in another file scope emitted two unresolved-name
      classes at once: the module's rest accessor returned a wrapper class minted at the crate ROOT
      and imported nowhere, and that root wrapper's own `insert`/`get`/`keys` named the inner types
      bare with no import there either. Every ingredient had a green fixture of its own — rest rows,
      cross-scope references, nested maps — and no cell crossed them, so the wasm crate did not
      compile while every gate stayed green.
    - **The cheap system is a matrix of shape PAIRS compiled wasm-side** — generated once per pair,
      `cargo check`ed, no vectors. `tests/matrix_multifile` (shape × reference-mode, wasm-checked per
      cell) is the nearest existing machinery, but its SHAPES axis enumerates self-contained RULE
      shapes only; a construct that exists only INSIDE a record has no row it can occupy, which is
      why the two above are hand cells. Extending that axis to record-embedded shapes is the
      concrete build.
    - **Reopening signal:** a THIRD construct joins the flat-walked set — countable in-tree as the
      explicit container mints in `generation/mod.rs` that exist because the conceptual visitor
      cannot see the composite (two today, both listed above). At three, per-construct hand cells
      stop being cheaper than the grid row, and the cross-axis coverage the grid gives for free
      (placement × reference mode × profile) is coverage three constructs are each doing without.

9. **A member-expression `.cbor` STRIPS its inner alias from the IR, so the declared spelling is
    lost one layer above where the spelling rule operates.** `holder = [j: bytes .cbor
    stake_credential]` emits `pub j: Credential` — not `StakeCredential` — while the tag form of the
    same shape (`f: #6.9(stake_credential)`) keeps `Alias(StakeCredential, Rust(Credential))` and
    therefore keeps its declared field type. Both forms attach an encoding operation at the member's
    own type expression; only `.cbor` drops the alias node.
    - **Not fixable in the emitter, which is why it is here and not a spelling bug.** The
      declared-spelling rule (`docs/docs/output_format.mdx` § "Type spelling at member positions")
      names a member position's type from the IR it is handed; if parsing already replaced
      `Alias(StakeCredential, …)` with the bare target, every downstream position — field,
      constructor parameter, accessor, encoding sidecar, call target — agrees on `Credential`, and
      that self-consistency is why nothing fails. The fix is in `parsing.rs`, at whatever builds the
      member's `RustType` for a `.cbor` controller over a typename.
    - **What it costs today: nothing observable, which is the argument for recording rather than
      building.** The output compiles and round-trips; the only loss is the spec author's chosen name
      at a shape that no committed fixture uses (found by probing the tag/`.cbor` asymmetry while
      ruling on the ownership carve-out, not by any gate).
    - **Reopening signal, measurable by a consumer who already has the problem:** a spec declares a
      `bytes .cbor <alias>` member and the generated field's type is not the alias — i.e. their
      public API loses a name they wrote. That is one grep of their own generated source, and it does
      not require anyone to recognise it as a parse-layer issue.

10. **The generated-local collision class is refused, not mangled — and the refusal's shape scope
    comes from a bounded probe matrix, so a position that matrix never touched can still ship an
    uncompilable crate.** A field whose emitted identifier is one of the fixed locals the generated
    serialization bodies bind now rejects at parse time (`parsing::GENERATED_LOCAL_RESERVED`, seven
    names each carrying the shape × profile × error class it was measured to break), with `; @name
    <other>` as the remedy and the CBOR wire key untouched. The emitter-local vocabulary is held
    LOCKSTEP by `identifier_hazard_tests::generated_local_registry_covers_emitter_locals` (a new
    emitter local fails until verdicted into the reserved set or into
    `GENERATED_LOCAL_PROBED_SAFE`), and `generated_local_out_of_scope_crates_compile` (`#[ignore]`,
    full tier) compiles every out-of-scope cell per profile so a too-narrow scope cannot hide.
    - **Residual: mangling is still the general fix.** The shipped behavior takes the name away from
      the spec author instead of renaming the generator's own local, so a spec that genuinely wants
      a field called `raw` must carry a `; @name` comment. **Reopening signal, measurable by a party
      who already has the problem:** a consumer whose CDDL is machine-generated or vendored from
      upstream — where a `; @name` comment cannot be added at all — is refused by this check. They
      cannot apply the remedy, which is the only condition under which the cheap fix is not a fix.
    - **Residual: the scope is only as wide as the probe.** Membership and per-name
      `ReservedScope` were measured over five shapes (array-rep / map-rep / tagged record /
      embedded plain group / group-choice arm) × three profiles × two field types, plus a
      `--wasm=true` pass. NOT probed: a field whose type is a named rule or newtype, `.cbor`-payload
      and bounded-type member positions, and the `--json-schema-export` / `--component` faces.
      **Reopening signal:** a generated crate fails `cargo check` with a shadowed-binding error
      (E0599/E0308/E0124) on a field whose name is in `GENERATED_LOCAL_PROBED_SAFE`, or on a
      reserved name used in a shape outside its declared scope — the compile error names the
      binding, so the report arrives pre-diagnosed and says exactly which row to widen.

11. **The wasm face's door vocabulary is hand-listed, and no mechanism derives it from the rust
    surface it mirrors.** `wasm_door_vocabulary_matches_the_posture_that_owes_it`
    (`src/tests/wasm_parity_tests.rs`) pins the six flag-conditional door members —
    `to_cbor_bytes`, `from_cbor_bytes`, `to_canonical_cbor_bytes`, `to_json`, `to_json_value`,
    `from_json` — per posture and in both directions, against a table it carries in source. A
    SEVENTH member, added to the emitter and not to that table, is caught by nothing: the table is
    the vocabulary, and it is written by hand.
    - **Why the parity gate cannot host the generalization.** `wasm_api_parity` exists to report
      "a rust member with no wasm counterpart", but its rust-side walk reads INHERENT impls only
      (`syn::Item::Impl(im) if im.trait_.is_none()`) and documents the exclusion of trait impls as a
      deliberate structural exemption. Every door member's rust-side home is outside that walk —
      three are trait methods on the generated runtime's `ToCBORBytes`/`Serialize`/`Deserialize`,
      three are backed by serde derives — so each contributes no rust-side row and none can be
      reported missing there. The exemption is right for `From`/`AsRef` and wrong for exactly this
      family, whose members exist ONLY as trait methods on the rust side and ONLY as inherent fns on
      the wasm side. Widening the walk to trait impls would drown the differential in the
      `From`/`AsRef` noise the exemption exists to remove; the generalization wants its own
      mechanism, keyed on the trait set the wasm face promises to mirror.
    - **Reopening signal:** a consumer reporting a wasm member absent that the rust crate has. That
      reaches us pre-diagnosed (the observable is an `undefined` method on a JS class whose rust
      counterpart they can point at), and the count of silently-missing members is the dimension
      along which a hand-listed vocabulary stops being maintainable.

12. **Run the WHOLE input-robustness catalog out of process, not only a listed abort-prone subset.**
    Today `input_robustness_catalog` spawns a process per input named in `ABORT_PRONE_INPUTS` and
    runs every other input in-process inside `catch_unwind`. That list is hand-maintained, so an
    input whose generation newly starts to abort — a non-unwinding crash `catch_unwind` cannot see —
    takes the test binary down instead of recording an `ABORTED (signal <n>)` row, and the failure
    reads as an unexplained harness death rather than as the catalog's own finding. The breadth is
    not built because a spawn per input costs a process launch each across the whole catalog, on the
    `local` tier, to cover a class that has produced exactly one member.
    - **Reopening signal**: a second aborting input found by any route OTHER than that list — a user
      or a fuzz run reporting exit 134 with no diagnostic, from a spec the list does not contain.
      The exit code is the observable and it belongs to whoever ran the tool, and the count of abort
      shapes a hand-list misses is the dimension along which the subset-versus-everything choice
      actually costs. The one shape that motivated the lane
      (`tests/robustness/recursive_collection_holder.cddl`) no longer aborts — the recursive-type
      boundary repairs it — so the signal is not met by the catalog's own contents.

13. **A maintainer ruling to force: the convenience `to_cbor_bytes()` door turns `float16`'s loud
    serialize error into a panic.** A `float16` member's carrier is `f32`, and a carrier value that
    is not f16-exact cannot be written at the one head the type declares — so `Serialize` returns
    `Err` (`InvalidLenPassed`, the declared-width refusal working as designed: rounding to fit
    would be a silent value mutation), and `docs/docs/current_capacities.mdx` documents both the
    error and the door. The tension is the door itself: `ToCBORBytes::to_cbor_bytes()` UNWRAPS,
    on the pre-existing design premise that generated serialization is total — a premise
    declared-width floats are the first type class to break. A library panic on a user-supplied
    value is loud, in-policy, and documented; whether it should instead be a `Result`-returning
    door (or a validating construction path for the f16 carrier) is an API-shape decision with
    breaking-change surface across every generated crate, so it is the maintainer's to take, not
    an implementer's — already flagged to the maintainer at delivery (T1-01, cycle 2). The
    fixture pin for the current contract is `float_heads_inexact_float16_fails_serialize_loudly`
    (`tests/core/tests.rs`). Reopening signal: the maintainer takes the ruling, or a consumer
    reports the panic from production data (measurable by the party holding the inexact value).

14. **A registration-class base axis for the reference-context sweep family — generation floor,
    not directive preservation.** Rule classes differ in what their ident REGISTERS (a struct
    under its own name; an alias to an instantiation canonical, as the named set-nominal binding;
    an extern; a transparent collection alias), and a reference context that assumes one class
    aborts on another: the twice-aliased set-idiom instance panicked at a member site because the
    set-nominal binding is the one class whose ident names no struct, and the repair walk looked
    one `Alias` box too shallow (fixed and pinned by
    `generic_collection_tests::alias_of_instance_chains_generate`, 30 cells). No existing sweep
    could have seen it: `directive_referencing_context_sweep`'s bases are DIRECTIVE families (a
    directive-less instance binding is not a base), and the recombination roles include a
    generic-arg position but no rule-level alias hop. The system this asks for is the sweep
    sibling with bases = registration classes, contexts = the existing reference-context rows
    (member, element, re-alias hop, map value, tag head, …), verdict = generation succeeds and
    the crate compiles — the 30-cell pin is one row of it, hand-built. Reopening signal, on the
    dimension the cost grows along: a SECOND generation abort on a directive-less shape whose
    trigger is a reference context over a registration class those cells do not span — the abort
    is exit-101-loud and belongs to whoever generates the shape, so the reporter exists by
    construction (the first member of the class is the one already fixed, so the signal is not
    met by this entry's own record).

15. **A fixture SHAPE evicted over a known defect has no stale-guard, so the fix never re-adds
    it.** Skip-listed gate rows are ledgered with citations and stale-guards; an eviction — a
    shape REMOVED from a fixture because it trips a known bug — is recorded only in prose, which
    nothing re-probes. Proven cost: `tests/corpus/tag_set_generic.cddl` dropped its bytes
    instance when the bytes-element list doors failed the wasm compile (E0271), and the corpus
    wasm-face compile floor — the exact gate that owns the class — went blind to it for the
    defect's whole life; the shape came back only because the burndown re-probed the row's
    premises by hand. What to build, when the class recurs: a small eviction ledger (fixture,
    evicted spelling, defect citation) with a guard that generates each evicted spelling and
    fails loudly the day it heals, the same stale-guard contract skip rows already carry.
    Reopening signal: a second eviction — a fixture edit that removes a spelling citing a defect
    rather than restructuring the fixture — measurable by its author at the moment of eviction,
    which is when the ledger row would be written anyway.

## Standing-system residuals (recur-first)

Each entry here is a ledger record for a proven-once failure class: what happened, which standing
system (or working rule) owns it meanwhile, and the trigger — usually a SECOND instance — that
justifies building the named mechanical layer. Building before the trigger fires is deliberate
over-engineering; deleting an entry without either building the layer or recording why the class is
dead loses the lesson. When a trigger DOES fire, land the DETECTOR half of the named layer before
the FIX half, and let the detector's arming-run measurement re-test the entry's premises before
implementing its sketched remedy: residue predictions and escalation sketches are recorded against
premises nobody has tested — three falsified-premise instances are on record: the extreme-value
entry's "generation already uses `_sz` where the `i64` limit bites" premise (falsified by the
`FixedValue::to_bytes` bug its own entry had named as a mere range cap); the unused-imports entry's
"near zero" residue prediction (104 warning cells at the arming run, from a generated-only-view
class its sketched glob-EDGE escalation could not have seen); and the optional-fixed-FLOAT findings
entry's "data-lossy but non-crashing" premise (the silent drop was real only for a float ALONGSIDE
other dynamic-length optionals — an ISOLATED one FAILED generation loudly, so the entry's
certify-or-fix fork mis-modeled exactly the shape an enumeration naturally picks; delivered by
`tests/corpus/optional_fixed_float.cddl` + the float presence-field arms, with the preserve residue
since delivered too — a float's head width is an encoding variable, pinned by
`preserve_encodings_supports_floats`). A fourth is on record and is the reason a cited GATE counts as
a premise too: two entries named `all_supported_constructs_generate_all_profiles` as the disk-writing
gate their remedy would extend, and it writes nothing — it drives `api::generated_strings` in-process
over `tests/matrix_supported/`. Probe the mechanism of any gate a remedy is built on, not its name.

**Entries whose trigger has FIRED are work items, not deferrals, and they are listed here so the
section's recur-first premise stays honest.** Cited count-free and by exact title, because a
hand-maintained tally of this list is itself the rot class `cddl-matrix/ROADMAP.md` § Maintenance
records — this one went stale twice in a single day of deliveries before the number came out. The
full record of each stays in place below: "A "no gate demands this" premise probed against ONE gate
is not evidence about a gate in another TIER" (the mechanical half is a maintainer call — it edits
`check.ts` itself); and "`--extern-wrapper-index` deferral-boundaries".

- **A gate exists in the registry but nowhere in `tests/README.md`, and a hand-written count of
  gates disagrees with the registry — no mechanism ties the two.** Two instances, so the trigger has
  fired on the count half and is one short on the catalogue half. *Instance 1 (count):* the
  concurrency section read "the thirteen `#[ignore]`d manual-only heavy gates" while the runner
  printed `parallel batch: 15 gate(s) in group 'manual_heavy'` — and the SAME file two paragraphs
  earlier carries the correct 15 inside a `<!-- gen:sh:tests-ignored-gates -->` generated block, so
  the doc disagreed with itself across a generated/hand boundary. Fixed by deleting the derived count
  rather than correcting it, the remedy this repo already chose for the identical class in prose
  refusal counts. *Instance 2 (catalogue):* `pin_cold_fetch` shipped as a `full`-tier gate and was
  described in `check.ts` and `static/manifest_changes/README.md` but never in `tests/README.md`,
  where its siblings are catalogued; added by hand on discovery. **Owner meanwhile:** review, plus
  the existing `project_status_headers.ts` generated roll-call, which covers only the `#[ignore]`d
  RUST gates — the script-shaped gates (`verify.ts`, `corpus_detect.ts`, the fuzz compile-rot check,
  `pin_cold_fetch`, the two gate-cache gates) are prose and unguarded, which is exactly where the miss
  landed. **The layer, when built:** a fast-tier lint asserting (a) every registry gate id appears in
  `tests/README.md`, and (b) no prose integer adjacent to a gate-group name disagrees with the
  registry's own membership — or, simpler and preferred on the (b) half, that no such integer is
  written at all. Cheap, no cargo. **Trigger:** a third instance, OR the next gate added to the
  registry by anyone who is not also editing `tests/README.md` in the same commit.

- **`lint_doc_citations` accepts "a docs section by heading" as a durable citation, but a section
  citation only proves the SECTION still exists — not that the claim cited from it does.** One
  instance, self-inflicted and caught in the same session that caused it. `current_capacities.mdx`
  cited a findings SECTION for the claim that `t = true / null / tstr` fails under
  `--preserve-encodings`; the delivery that fixed that defect deleted the ENTRY inside the section
  while the section itself survived, so the citation kept resolving, the lint stayed green, and a
  user-facing doc asserted a defect that no longer existed. The entry-title form the lint prefers
  would have dangled loudly on the same edit. Note this is NOT the positional-citation class the lint
  already bans (`item <N>`, which silently retargets): this one is an ACCEPTED durable form whose
  granularity is coarser than the fact it carries. **Owner meanwhile:** review, and the working rule
  that a citation should name the narrowest stable referent that dies with the claim — an entry
  title, a test/gate name, a pin — never the section enclosing it. **The layer, when built:** extend
  the lint so a citation into a doc that has entry-level headings must name one, rather than the
  enclosing section; a section citation stays legal only where the section IS the referent (a
  whole-mechanism reference). **Trigger:** a second instance, or any delivery that prunes entries
  from a section other docs cite into.

- **A REPRESENTATION-CHANGING directive that goes live on a new container can ship without its
  extern-interface projection — the cross-crate skew class, invisible to every single-crate
  gate.** Proven instance (orchestrator code-read, not any gate): `@duplicates preserve` on
  tables was live for two commits while the projection helper only travelled `reject`, and the
  helper's own doc-comment actively misled ("table preserve never reaches here" — stale from the
  phase-1 refusal era); a dep exporting a preserve table would have had its consumer silently
  rebuild a reject-default `BTreeMap`. Fixed by the shape-aware `duplicates_annotation`, which
  now derives the projection from the SAME `is_reject_ordered_set`/`is_preserve_pair_map`
  predicates that drive representation — the coupling that makes the drift structural rather
  than remembered — plus the `dep-preserve`/`consumer-preserve` two-crate fixture with its
  negative-control skew leg. Standing rule meanwhile: a delivery that makes a directive change
  ANYTHING a dep EMITS — representation, materialized names, or annotation semantics — must land
  the extern-interface projection AND its two-crate fixture in the same change (reviewers: diff
  `extern_interface.rs`'s annotation helpers against the new predicate).
  The wider vocabulary is measured, not cautious: the forward direction's near-miss (delivery 4)
  was `@no_alias` honored on collection rules — a MATERIALIZATION change, no representation
  touched — where the projection had to move in the honoring commit (`f9f69a8d`, with its
  two-crate vector) or every consumer would have imported a `pub type` the dep no longer emits.
  It did not ship, because the delivery's review mandated the projection check ad hoc — but the
  old vocabulary ("changes a rule's REPRESENTATION") would not have demanded it, which is exactly
  how the class survives a diligent implementer. Mechanical layer for the forward direction, to
  build if a second emission-changing surface ships without its projection: a marker registry in
  the writer-vocabulary style — enumerate the per-ident records and config predicates in
  `IntermediateTypes` that alter the dep's emitted rust surface, and hold each against a total
  verdict (consulted by `extern_interface.rs`, or classified inapplicable with its reason), the
  way `EXTERN_INTERFACE_WRITER_VOCABULARY` is total over the emitter's `@…` literals.
  The trigger for a mechanical layer — an extern-projection sweep that enumerates
  every representation-changing config predicate and auto-checks a dep/consumer pair per
  directive — is a SECOND representation-changing surface shipping without its projection.
  The class has a CONVERSE direction, proven by a second reading-found instance (2026-08-01,
  delivery 3): the projection EMITTING a directive onto a form the consumer's parse REFUSES.
  `@raw_bytes_flavor` was projected verbatim onto the param-less rendering of a generic extern
  base — a spelling that flavors nothing — and the moment the non-generic-extern refusal landed
  beside it, every consumer of a flavored dep would have hard-failed; no committed fixture was
  such a consumer, so both tiers stayed green while the break shipped in the working tree
  (caught in-delivery by reasoning about the seam, fixed by dropping the tag from the
  projection, pinned by `extern_import_flavored_generic_base_projects_without_the_tag`). What
  makes the converse direction structural rather than remembered is that a REFUSAL delivery has
  no reason to look at the projection: the writer already exists and no representation changed.
  The mechanical layer covering BOTH directions is DELIVERED — the writer-vocabulary registry, its
  per-annotation consumer acceptance vectors, and the LOCKSTEP source scan that holds the two total
  over the emitter's `@…` literals; current state in `tests/README.md` § "Extern-interface export &
  `--extern-import` (the machine-generated stub channel)". What remains unmechanized is the DEP-SIDE
  half:
  the registry proves this crate's parse accepts what its writer emits, and says nothing about a
  counterparty crate at a different version doing so. Reopening signal, measurable by whoever
  pays it: a consumer failing to regenerate against a dependency's committed export whose
  annotations this crate's own parse accepts — the version-skew shape the single-repo registry
  cannot see.
- **An emission branch whose generated OUTPUT no fixture exercises is invisible to every gate —
  it can rot to uncompilable and hide behavioral bugs in both directions.** First of a family of
  three, each recorded separately below because each needs a different detector, and all three
  answering "a fixture exists, so why did it not witness this?" with a different mode: NEVER
  EXECUTED (here), EXECUTED BUT UNASSERTED ("Value-anchor rot on field addition"), and EXECUTED
  AGAINST THE WRONG SHAPE ("A fixture that models an OBSOLETE user contract stays green while the
  real contract path is broken"). Proven instance
  (plan-review-caught during the cbor_event 3.2.0 upgrade, not by any gate): the
  nullable-`Special` null-peek (the `Optional`-with-Special arm in `generation/deserialize.rs`)
  had ZERO checked-in output — no snapshot, export, or suite fixture contained its emission — and
  had decayed to emitting a receiver-less `match cbor_type()?` (E0425, uncompilable) around a
  hardcoded 1-byte rewind that both ACCEPTED malformed two-byte simples (`f8 f5` read as
  `Some(true)`) and REJECTED valid 9-byte floats. Compile, round-trip, and conformance gates were
  all blind because no input ever reached the branch. Standing coverage now: the
  `nullable_specials` fixtures (core + preserve suites) execute the emission in both directions.
  Working rule meanwhile: a new emission branch lands WITH a fixture whose generated output
  exercises it (compiling the generator is not evidence the emitted string compiles). The
  mechanical layer on a second instance: a generator branch-coverage sweep — e.g. llvm-cov over
  the corpus + suite generation runs, red on emission arms no fixture executes — the only layer
  that catches the class without knowing each branch by name.
- **Logic the tool EMITS as source text is outside every layer that reasons about the tool's own
  code, so its only oracle is a hand vector chosen to DISTINGUISH a wrong implementation — and a
  vector that merely EXERCISES the logic certifies nothing.** Proven instance (orchestrator review,
  not any gate): the emitted schema-document closure check carries a `decode_schema_ref_name` whose
  comment justifies unescaping `~1` before `~0` — the order matters only for a name holding a
  LITERAL `~1`, which encodes to `~01` — while its green-path vector was a schema name
  (`Odd<K>/~name`) that decodes correctly under BOTH orders. So the comment's whole justification
  was unwitnessed: the three escape classes were covered, the ordering they were cited to protect
  was not. Confirmed by simulating `schemars`' `encode_ref_name` against both orders, then closed by
  making the name carry a literal `~1` (`Odd<K>/~1name` → `Odd%3CK%3E~1~01name`, which the wrong
  order decodes to `Odd<K>//name`), which costs no new nested-cargo cell. Why no standing layer sees
  the class: the `cargo-mutants` sweep (§ "Pending maintainer action") is scoped to the tool's own
  functions, and logic living inside an emitted string constant is not a function it can mutate —
  so mutation scoring structurally cannot measure emitted-runtime behaviour, and the emitted surface
  is growing (the three name-injectivity panics, the closure walk, the decode). Working rule
  meanwhile: a hand vector for emitted logic states which WRONG implementation it distinguishes, and
  a comment that justifies an ordering or precedence choice names the vector that would fail without
  it. Mechanical layer on a SECOND instance: mutation testing for the emitted runtime — perturb each
  emitted-source constant textually (swap adjacent `.replace` calls, invert a comparison, drop a
  branch), regenerate one fixture per mutant, and require some gate to go red; expensive enough
  (a nested cargo run per mutant) that it earns its cost only once the class has recurred.
- **Value-anchor rot on field addition: a delivery that grows an existing type's fields leaves the
  type's existing hand-vector sweeps decoding the new data but asserting nothing about it — the
  EXECUTED-but-unasserted member of the fixture-blindness family, whose never-executed first
  member is "An emission branch whose generated OUTPUT no fixture exercises is invisible to every
  gate".** Proven instance (review-caught, not by any gate): the float presence-field delivery gave
  `array_opt_fields` (`tests/core`) real `x`/`z` presence fields, and the fixture's existing decode
  sweep — which already fed x/z-present bytes through `from_cbor_bytes`, and whose own comment
  states the discipline ("decode-accepts alone proved nothing — pin every field") — kept passing
  without asserting the new fields or exercising their serialize side in that long-optional-chain
  shape; closed by extending the sweep's anchors + round-trip in the review commit. The
  pending `cargo-mutants` sweep (§ "Pending maintainer action") covers this class only PARTIALLY:
  a generator mutant breaking the new behavior dies to any OTHER fixture asserting the same arm
  (here the corpus fixture's emitted tests), so mutation scoring cannot see that one hand suite's
  anchors went vacuous for its own distinct shape. The working rule is current state in
  `tests/README.md` § "Hand-vector suites", as the fourth of that section's assertion shapes.
  Mechanical layer on a SECOND instance: a per-suite anchor-completeness check (each
  round-tripped type's public fields ⊆ the fields its suite's asserts mention — buildable as a
  grep-level floor over `tests/*/tests.rs`), accepting its enumeration cost then, not before.
- **The corpus-wide regen-over-prior-output sweep reaches two emission profiles of four, and its
  user-EDIT leg reaches one.** `regen_over_prior_output_corpus` sweeps the static floor and the
  rule-DELETION variant under the default and `--preserve-encodings` profiles (the latter for
  `cbor_encodings.rs`, a per-rule surface the default never emits); the `json` profile's `json-gen`
  tree and derive attributes, and the `component` face's WIT-side glue, are unswept, as is the
  user-EDIT leg outside the default profile. Each added profile costs one more generation per fixture
  for legs 1-2 (measured ~15 s on the delivering machine) and THREE for the edit leg, plus nested
  cargo for its compile gate — paid on every `local` tier run. **Owner meanwhile:** the emitter
  invariant itself (no generated comment on a row a spec change can delete), which is
  profile-independent in its reasoning even where the sweep is not in its reach, plus the two swept
  profiles' floors. **Trigger:** a `cddl-codegen:unpreserved-comment` sentinel reported against a
  regenerated tree — by a consumer or by any gate — in a file neither swept profile emits; the
  profile that produced it is then the one to add, not all of them.
- **Stale "known limitation" prose surviving its fix — a finding ledgered in TWO homes where the
  fixing commit prunes only one.** First instance (read-caught during the facade-pin delivery, not
  by any gate): the extern-only-scope undeclared-module finding was ledgered both in
  `cddl-matrix/ROADMAP.md` and as a hand "Known limitation" paragraph in
  `docs/docs/comment_dsl.mdx`; the fix pruned the roadmap entry and minted the pin
  (`integration_extern_only_scope_declared_in_root`) but the user-docs paragraph survived — through
  a later docs pass over the same file — until the facade compile pin
  (`facade_composition_compiles`) delivery pruned it. SECOND instance SET, a DIFFERENT home
  (read-caught during the findings-sweep delivery): the corpus findings ledger
  (`cddl-matrix/annotations/corpus/cddl_codegen.toml`) carried four failure-claim `[[finding]]`s /
  sibling `[[note]]`s whose defects had been fixed out from under them (the `bool`-in-type-choice
  E0282, the single-letter-`r` E0574, the inline-group data-loss "Bug", and the top-level
  fixed-value "panics" prose) — these render into `tests/corpus/COVERAGE.md`, a GENERATED span
  `lint_doc_citations` never scans, so that gate could never have caught them. THIRD instance, a
  distinct rot mode — a NEGATIVE-INVENTORY scan claim (caught at item pickup during the tag-set
  matrix-cell delivery): a `cddl-matrix/ROADMAP.md` bullet asserted "no current row's example
  matches the recognized shape" as its coverage-hole premise, and the claim was silently falsified
  the SAME DAY it shipped when the `dsl.duplicates.{reject,preserve}` registration (`5485b2c`)
  landed a matching example (`#6.258([* uint]) / [* uint] ; @duplicates reject`; harmless there —
  that row's verdict was minted with the collapse live, so nothing was actually stale — but the
  prose was wrong from that commit on). The class property: a point-in-time inventory claim
  DANGLES NOTHING — every artifact it names keeps resolving — so no citation lint can ever fire;
  and mechanically re-checking arbitrary prose inventory claims against the live matrix is the
  same claim-semantics boundary check `I` deliberately stops at. No mechanical layer proposed; the
  guard is pickup discipline, which worked here: a roadmap item's scan/inventory premises are
  claims to re-verify against the live tree when the item is picked up (AGENTS.md's
  premise-probing rule — the re-scan is one grep), and the delivery prunes the whole bullet,
  premise included. Authoring rule for future bullets: scope any inventory claim to its evidence
  ("scanned at <sha>") or phrase it as the check to re-run, never as a standing fact.
  FOURTH instance, in a home the first three did not reach — a session HANDOFF file, where the
  claim is about VERIFICATION STATE rather than about the tree: a `draft/` worklist's carry-forward
  bullet read "no cycle-1 commit has been seen by a `full`-tier gate", true when written and
  falsified as soon as the tier was run. The next cycle's orchestrator propagated the file's line
  into its own report in preference to the fresher fact it had been given directly, which is the
  rot mode worth naming: a handoff doc is READ as current state by construction, so a stale
  standing fact in one is not merely wrong, it OUTRANKS newer evidence in the reader's head. The
  authoring rule above already covers it verbatim (a tier verdict is a point-in-time inventory
  claim, and "PASSED at `<sha>`" satisfies it while "has never run" does not), so this is the rule
  firing rather than a gap — but note it applies to `draft/` handoff files, which no gate scans and
  which are gitignored, so review is the only reader that can enforce it. The
  SECOND-instance mechanical layer
  is now BUILT for that home: the findings-claims arm (`project_corpus.ts` check `I`, local tier)
  fails a failure-claim finding that names no resolvable tracking pin — current state, including
  its accepted looseness (a resolving citation does not validate claim SEMANTICS, which stay
  review-owned; any backtick token matching `src/tests/` text satisfies resolution — tighten on the
  first vacuous pass observed), is documented in `cddl-matrix/README.md` § "Gotchas". Still future:
  the USER-DOCS home has no arm — a parallel limitation-claims arm on `lint_doc_citations` (every
  known-limitation-classed paragraph outside a generated span carries a resolvable tracking
  citation, or folds into a projected span — the matrix north star's "more docs follow") remains
  unbuilt, since the two homes have different scanners and neither arm sees the other's prose. That
  half needs its own signal rather than inheriting the parent's second-instance trigger, which four
  recorded instances have already met: on the axis its cost grows along, it is **the count of
  known-limitation paragraphs in `docs/docs/*.mdx` that name no tracking artifact** — measured by the
  next author who greps `docs/` before shipping a fix, which the working rule below already requires
  them to do, so the count arrives as a by-product of work someone is doing anyway. Build the arm
  when that grep returns a set too large to walk by hand; until then the grep IS the arm.
  Working rules meanwhile: a limitation recorded in user docs names its tracking artifact (the
  reject/panic fixture, pin, or ledger entry) in the same paragraph; a fixing session greps `docs/`
  for the limitation's key phrases before shipping; and the prose that CLOSES a stale finding
  attributes the fix by REPRODUCING against the candidate commit's parent (a throwaway worktree +
  the probe is minutes), never by narrative plausibility — the findings sweep's first closure draft
  attributed both compile-bug closures to a plausible-but-wrong commit, falsified only by a
  worktree reproduction at that commit's parent before the wrong story shipped. The
  reproduce-at-parent rule is REVIEW-OWNED by explicit decision, not pending a mechanical layer:
  whether a reproduction happened leaves no machine-checkable artifact, so a detector could only
  check that prose CLAIMS verification — the same claim-semantics boundary check I stops at (same
  disposition class as the declined docs-conformance harness; the reopening signal is a false
  attribution SHIPPING past review, not one being caught by it). Bisect/reproduction mechanics for
  whoever next needs them — chiefly the `git bisect run` exit-convention POLARITY trap (exit 0
  always means the OLD term, so a fix-hunting script that exits 0 on "works" is inverted and the
  search returns its own fixed endpoint), plus the specific-error-keyed verdict shape and the
  cheaper reproduce-at-parent/pickaxe-first alternatives — live in
  `draft/bisect-verdict-discipline.md`.
- **A finding's DEFECT-SIDE attribution recorded by narrative plausibility, not execution — the
  RECORDING-side twin of the closure-side reproduce-at-parent rule above.** First instance
  (caught at item pickup, cycle 10 of the burndown effort): the present-null-optional finding
  (recorded `63feb259`, retired `78aab6d3`) attributed the dropped state to the CBOR re-encode
  and the faithful behaviour to the json round-trip; worktree execution AT THE RECORDING COMMIT
  showed the reverse on both surfaces (CBOR re-encoded `820082190338f6` byte-exact; the json
  detour came back `820081190338`). The inversion was structural, not a typo's risk profile: the
  surfacing gate (`corpus_decode_replay`'s json leg) proves the two decode surfaces DISAGREE — a
  SYMMETRIC fact — and the prose then named a culprit the gate never identified. No existing
  layer could have caught it: the `JSON_SURFACE_SKIP` guard asserts only that the cell STILL
  FAILS, which stayed true (the disagreement was real, whichever side owned it), so an inverted
  attribution stays green indefinitely; and the findings-claims arm (`project_corpus.ts` check
  `I`) checks citation RESOLUTION, not claim semantics — the same boundary the entry above
  records it stopping at. Cost while it stood: two burndown triage passes carried the row with
  its fix shape pointed at the WRONG surface ("fix serialize for nullable-inner optionals"), and
  only the pickup re-probe rule (AGENTS.md) prevented a delivery built against it. Working rule,
  now, in place of a mechanical layer: **a finding that attributes a defect to ONE of several
  surfaces or directions quotes each surface's executed probe output — bytes in, bytes out, per
  surface, at the recording sha — in the entry; a disagreement gate licenses recording the
  disagreement, and only an executed per-surface probe licenses naming the culprit.** (Symmetric
  evidence for a symmetric fact; the closure-side rule above already demands the same discipline
  when a finding is retired.) The mechanical layer, if the class recurs: skip-ledger rows carry a
  machine-checkable failure SIGNATURE (which leg diverges, and the divergent bytes) that the
  replay guard executes every run, so a recorded attribution is re-asserted continuously instead
  of trusted from its recording day. Reopening signal: a SECOND finding whose recorded
  surface/direction attribution is falsified at pickup — measured by the pickup party, who is
  already re-probing premises under AGENTS.md's rule, so the observation arrives as a by-product
  of work being done anyway.
- **A "no gate demands this" premise probed against ONE gate is not evidence about a gate in
  another TIER — and a full-tier gate is where such a premise survives longest, because CI runs
  `fast` only.** Proven instance: the `@no_json_schema_export` delivery deferred its cddl-matrix
  feature row on a premise probed against `project_corpus` (fast tier), where it genuinely holds —
  check D demands a cover only for an id the corpus actually exercises, and an unregistered id has
  no universe entry at all, so it cannot be demanded. The same premise is FALSE for
  `cddl-matrix/verify.ts`'s forward completeness lint, which treats any directive documented in
  `docs/docs/comment_dsl.mdx` as surveyed surface and hard-fails (`missing_cddl_codegen_feature`)
  while it carries no `features/cddl_codegen.toml` row. `verify` is full-tier; no tier run that
  cycle reached it, so the directive shipped over a red gate from the commit that introduced it
  (`9ae0e552` — tag and docs both present, row absent) through four successors, surfaced by
  orchestrator code-read rather than by any run. The standing system that owns this already exists
  and was simply not run (AGENTS.md § "Build & verify": `check.ts full` before shipping a feature);
  what is missing is the AUTHORING half. Working rule meanwhile: **a "no gate covers X" premise
  names the gates it was probed against AND their tiers**, so the unprobed remainder is visible in
  the claim itself instead of implied — the same scope-the-claim-to-its-evidence discipline the
  point-in-time-inventory-claim entry in this section prescribes. Mechanical layer, on a SECOND
  instance: a `check.ts` listing mode that prints the gates a given tier does NOT run, so a
  deferral rationale can enumerate what it is not evidence about instead of asserting a universal.
  **That trigger has FIRED — second instance, cycle 3 Lane B**, in a new tier pair and a new
  direction: not a deferral rationale but a DELEGATION. A lane restricted to `fast` (heavy tiers
  being serialized centrally) added `tests/emit-tests-bounded-key/`, whose registry is enforced by
  `wasm_api_parity_axes_and_pins_are_live` — a plain `#[test]`, hence `local`. `fast`'s only cargo
  TEST invocation is `cargo test --bin cddl-codegen snapshot_tests`, a SUBSTRING filter, so every
  `#[test]` outside that module is invisible to it. (`fast` does run `cargo fmt`/`cargo clippy`;
  since `clippy --all-targets` type-checks test code, the precise asymmetry is that `fast` catches a
  new `#[test]` that fails to COMPILE and never one that FAILS.) The implementer had no signal and
  shipped the
  fixture without its registry row, costing a fail-fast `local` run that skipped twelve later gates.
  That one fact is now current state where an author will meet it — `tests/README.md` § "Hand-vector
  suites" ends with the fixture-dir obligation, the enforcing test, and its tier — which retires the
  instance without retiring the class.
  Two corrections to this entry's own sketch, measured while the trigger fired: (a) the tier pair
  that bites is `fast`/`local`, not only `fast`/`full`, and the `fast`/`local` gap is the one an
  agent-delegation protocol runs into daily; (b) the sketched layer is PARTLY BUILT already — every
  tier run's summary already prints its `not-in-tier` rows — so what is actually missing is
  narrower than "a listing mode": a way to answer "which tier enforces the registry governing THIS
  PATH" without running anything, which is the question a delegation needs answered before it
  starts. The AUTHORING half is now in force (AGENTS.md § delegation: name the enforcing tier for
  any registry-governed tree a delegation writes into); the mechanical half is a maintainer call,
  deliberately not taken mid-cycle because it edits `check.ts` itself.
  **DEAD END, recorded with its proof, because it is the move anyone reaches for first: do NOT add
  a hand-authored path-glob per registry gate.** The data needed splits in two, and only one half
  exists. `gate -> tier` is already machine-readable in `check.ts`'s registry. `gate -> governed
  paths` exists NOWHERE — `wasm_api_parity_axes_and_pins_are_live` governs `tests/*/input.cddl`, but
  nothing declares that; it is implicit in a `read_dir` inside the test body. Declaring it by hand
  manufactures a SECOND registry describing the first, which can drift from the gate it describes
  while every path in it still resolves — a fresh instance of the precise class this section
  already records four times over (the `KNOWN_PANIC_CLASSES` citation entry and the
  point-in-time-inventory entry). A layer built that way would be a new instance of the failure it
  is meant to prevent, and the drift would be invisible to any resolution check. Deriving the map
  instead of declaring it does not rescue the approach either: the gates that govern a tree
  enumerate it AT RUNTIME, so deriving means running them, which is what the layer exists to avoid.
  UNBUILT SKETCH, not a validated design — offered only so the next attempt starts past the dead
  end: invert the question. The risk materialises only when a NEW FILE appears under a governed
  tree, which is a git-visible event, so a new-file trigger ("you added `tests/<newdir>/input.cddl`;
  the registries enumerating that tree are X, Y, tier Z") needs only the LIST of enumerating gates —
  no per-gate glob, hence no second registry to drift — and fires exactly when the delegation risk
  is real rather than requiring someone to think to ask. Nothing about this was prototyped; its own
  premise (that the set of tree-enumerating gates is small and stable enough to list) is unprobed.
- **A ruling whose premise is "gate X stays green" must be probed in the state the ruling CREATES,
  not the state that precedes it.** This is the scope-the-claim discipline above applied to the
  ORDER of a probe rather than its breadth, and it is a ruling-protocol rule rather than a quirk of
  any one gate. Proven instance: an orchestrator approved blessing
  `tests/matrix_panic/snapshots/catalog.snap` ahead of a `cddl-matrix/verify.ts` re-grounding, on
  the premise that the snapshot records only the observed generation outcome and encodes no matrix
  class, so `project_robustness.ts --check` compares directory PLACEMENT and would stay green. That
  premise was established by running the gate with the snapshot still UNBLESSED — the one state in
  which the disagreement it checks for cannot exist. The gate ALSO cross-checks the committed
  catalog against the matrix verdict class (it is the drift class the check was built for, and its
  own message states the required order: re-run verify.ts, re-project, re-bless), and it is
  `fast`-tier, so the bless would have turned CI red for fourteen cells. It was found by RUNNING the
  gate in the post-bless state, not by re-reading it — which is the tell: a pre-change green says
  nothing about a post-change gate whose input the change is what alters. Standing consequence, now
  settled: **a catalog bless is the LAST step of a re-grounding, never a step that precedes it.**
  Authoring rule: a ruling resting on a gate verdict names the STATE the verdict was measured in,
  and if that state is not the one the ruling produces, the verdict is not evidence for the ruling.
  Mechanical layer, on a SECOND instance where the pre/post distinction is not visible from the
  gate's own message: have the drift check name both inputs it compares in its GREEN line, so a
  reader of a passing run can tell which comparisons actually ran.
- **Observed-baseline comments beside gate floors rot silently — TWO instances, two homes, two
  distinct rot modes; the floors/consts stay the enforced artifact.** These comments are
  informational and review-maintained by design (replacing them with exact gate-asserted counts
  buys churn on every ingredient addition), so no gate can see them go stale. First instance: a
  layer-2 recombination gate's observed-baseline comment shipped stale. Second instance, a
  different rot mode (a tally with two incompatible READINGS): `corpus_decode_replay`'s floors
  comment carried a skip-ledger tally ("6 rows on `JSON_SURFACE_SKIP`") that was accurate as a
  pinning-time observation but reads as a live count — two later ledger changes each treated it one
  way or the other (an addition left it alone as history; a retirement hand-bumped it as a live
  tally), landing a number that matched neither reading. Both caught by review, not a gate.
  Working rule: floor comments state pinning-time observations in explicitly past-tense framing
  ("AT THAT confirm run …"), never present-tense tallies — the consts themselves are the live
  ledger. Mechanical layer, on a stale baseline comment actually misleading triage: replace the
  comments with exact pinned counts asserted by the gate, accepting the churn.
- **Extreme-value boundary correctness of generation-time encode paths: a value that FITS the
  type can still encode wrong at the type's boundary, and no fixture minted from "supported
  shapes" ever lands there.** Proven instance (read-caught during the `FixedValue` i128/u64
  widening, not by any test): `FixedValue::to_bytes` — the generation-time canonical-ordering
  encoder for fixed map keys — called cbor_event's `write_negative_integer(*i as i64)`, which
  produced wrong bytes for `i64::MIN` on the then-current cbor_event 2.4.0 (`-i64::MIN` overflowed
  i64; the 3.x endpoint computes in i128), so a
  canonical spec with a fixed `-9223372036854775808` map key would have gotten wrong key
  ordering silently. `i64::MIN` is representable in the old `isize` field, so this was a
  correctness hole at a representable boundary — a different class from the representability
  widening the nint entry in `cddl-matrix/ROADMAP.md` scopes (that entry even named this call
  site, but as a range cap, and its "generation already uses `_sz` where the `i64` limit bites"
  premise was exactly what the bug falsified). Standing coverage now:
  `nint_to_bytes_canonical_across_boundaries` pins hard-coded canonical bytes across the full
  magnitude ladder + `i64::MIN` through `to_bytes`, and
  `i64_min_fixed_value_emits_width_correct_nint` pins the emitted serialize line under default +
  preserve profiles. What no gate yet does: thread each numeric extreme (`i64::MIN`/`MAX`,
  `u64::MAX`, the `±2^63±1` off-by-ones) through every POSITION class systematically — fixed
  value, fixed map key under `--canonical`, range/bounds windows, deserialize mismatch arms — in
  a generated crate whose emitted tests execute. A SECOND read-caught extreme-value bug in any
  encode/compare path is the trigger to build that boundary-vector sweep as a corpus/matrix
  axis rather than hand-pinning per instance.
- **A fixture that models an OBSOLETE user contract stays green while the real contract path is
  broken — the wrong-shape member of the fixture-blindness family headed by "An emission branch
  whose generated OUTPUT no fixture exercises is invisible to every gate".** Proven instance
  (consumer-reported, not by any gate): every raw-bytes fixture appended
  the user-supplied `_CDDL_CODEGEN_RAW_BYTES_TYPE_` definition INTO `rust/src/generated/mod.rs` —
  the pre-thin-root layout no real user can have (that subtree is clobbered every regen) — so bare
  references resolved locally and all raw-bytes gates stayed green while the extern re-export glue
  (`pub use crate::Name;`) covered only `RustStructType::Extern`, not `RawBytesType`; regenerating
  CML's cip36 (`public_key = _CDDL_CODEGEN_RAW_BYTES_TYPE_` referenced only through `pub type`
  aliases, which have no other resolution path) failed E0412 at five sites. Standing coverage now:
  the fix routes raw-bytes defs through the real contract (appended to the user-owned thin
  `lib.rs`, same as extern-type defs), and the glue's own `pub use crate::<Name>;` line makes any
  regression to the masked routing fail loudly (E0255 duplicate definition) rather than silently
  re-masking. The WASM half of the incident was the class's second occurrence, caught by
  interrogating this entry's own first write-up rather than by any gate: the wasm glue covered
  only `Extern` wrappers while the fixture appended the wasm raw-bytes def into
  `wasm/src/generated/mod.rs` (the same no-real-user-contract residence), proven by a
  cip36-shaped scratch repro — rust crate compiled via the landed rust glue, wasm crate failed 3×
  E0425 `PubKey` (the consumer's rust E0412 had masked the wasm failure, since the regen never got
  past the rust crate). Both halves
  are now standing coverage: glue covers `Extern | RawBytesType` in BOTH crates, both def
  routings model the real crate-root contract (thin `lib.rs`), and regression to the masked
  residence collides E0255 with the glue's own re-export in either crate. The formerly-latent alias-walk sibling
  (`scope_references` walked `rust_structs` only, never `type_aliases`, so a CROSS-scope alias to
  a non-extern type under-imported its target — E0412) got its failing case on a production
  project and is fixed: the type-alias walk marks each emitted alias's `base_type` (and, wasm-side,
  the `resolved_wasm_alias_target` wrapper — a helper shared with the wasm alias emitter so the
  emitted target and its import cannot drift), with standing coverage from the placement matrix's
  `aliased` reference mode (every INCLUDED shape × `bal = <ty>` in module `b`; the extern-shape
  exclusion's alias-position residue — the generic-extern-instance `Base<Args>` class, feature
  request 07 — is hand-pinned by `tests/extern-generic-scoped`, see "Multifile reference-POSITION
  coverage" below). Working rule meanwhile: a
  fixture that injects hand-written code into
  `generated/**` carries a comment justifying the residence — and the justification must name a
  contract a real user can follow, or explicitly flag itself as a modeled-obsolete shape citing
  this entry (the `use super::*;` serializer-helper appends qualify). Mechanical layer on the
  NEXT instance: an audit sweep enumerating every test append-site that writes into a
  `generated/**` path and failing any not on a justified allowlist — the wasm half already being
  the class's second occurrence is the argument for building it on any further sighting rather
  than deferring again.
- **Panic-site un-shadowing: converting a shallow panic to a graceful rejection exposes deeper
  panic sites for inputs that previously died early — and the outcome catalogs, which record
  CATEGORY only by design, cannot see a PANIC→PANIC site shift.** Proven instance: converting
  `parse_type`'s fall-through panic made `ctl.bits` (which died there, on its `&(...)` rule)
  travel past the recorded rejection into the control-operator catch-all — still PANIC, so its
  `matrix_panic` catalog row never flipped, and the shift was visible only because the
  conversion's fixture-flip prediction was diffed against a pre-conversion PER-SITE baseline
  probe. Standing coverage: the recombination sweep's ledger keys are message+site-scoped, so a
  SWEPT composition moving to an unledgered deeper class fails loudly as a new finding; and the
  cycle-4 working rule — before converting a panic site, baseline-probe every fixture the site
  owns (panic site + message, not category), and re-probe after — caught this instance. The
  trigger for a mechanical layer (a site-fingerprint sidecar per `matrix_panic` fixture, diffed
  by the catalog test alongside the category) is a SECOND instance where an un-shadowed deeper
  site goes unnoticed past its converting commit; building it now would trade the catalogs'
  deliberate refactor-stability for a fingerprint that churns on every panic-message edit.
- **Three defect classes that no IN-PROCESS test can see, sharing one remedy: extend the corpus's
  INPUT.** `api::generated_strings` is the library API every in-process suite drives — the snapshot
  corpus, the robustness/panic/reject catalogs, `wasm_api_parity`, and
  `all_supported_constructs_generate_all_profiles`. It returns emitted source as STRINGS —
  post-rustfmt (its own doc comment; the merge step formats every emitted file, so a
  rustfmt-fatal emission IS an in-process generation error) — but it never runs `export()`'s
  disk-write tail, and it never hands anything to rustc. So a defect landing at either of those
  seams — or landing
  only under a profile no committed vehicle generates the shape under — is invisible to all of them
  at once, and a catalog row recording `ok` discharges exactly one claim: generation exited 0.
  The remedy for all three is an INPUT extension, not a new layer. `tests/corpus/*.cddl` is the input
  of `feature_corpus_compiles`, which shells the real CLI per cell (so `export()` runs, rustfmt
  included — a rustfmt-rejected emission is a non-zero exit and a red cell) and then `cargo check`s
  the rust + wasm + json-gen crates under all three profiles, with
  `feature_corpus_roundtrips_nondefault_profiles` (full tier) executing the emitted round-trips under
  preserve and json. Both gates are described as current state in `tests/README.md` § "Generated-test
  harness (`--emit-tests`, `src/emit_tests.rs`)". So the work each class implies is: promote the
  shape into `tests/corpus/` — never build an export-test or per-profile harness beside it.
  **Correction to this entry's own earlier remedy, probed rather than reasoned.** Two of these
  classes previously named `all_supported_constructs_generate_all_profiles` as "a disk-writing gate
  that already exists, limited only by taking `supported.cddl` as its input". Both halves are false:
  it reads `tests/matrix_supported/`, and it calls `api::generated_strings` in-process against an
  `--output` path it never writes (its own doc comment says "generation-only … no compile"). Routing
  rows through it would have reproduced precisely the blindness these entries exist to record — which
  is why a cited gate's mechanism is probed before a remedy is built on it.
  The three classes stay distinct in WHERE the failure lands, and each keeps its own trigger:
  - ***rustc, after a PANIC→`ok` flip: a panic fix lands on the FIRST site that dispatches on a
    shape, not the last — the sites behind it ask the same question and each answers it in its own
    code.*** Proven instance: the `cbor_types` `_ => panic!()` over a nominal reference to a
    collection typedef (`RustStructType::Table`/`Array`, which emit a rust typedef and no impls).
    Fixing the panic arm let the shape GENERATE and produced a crate rustc rejected, because
    `generate_serialize`/`generate_deserialize` emitted `.serialize()`/`::deserialize()` on a
    `BTreeMap`, and — under `--preserve-encodings` ONLY — `encoding_fields_impl` minted no encoding
    sidecar for a member whose serialize read one. Three further sites, one of them profile-gated,
    all reachable from the single input the panic fix unblocked. The standing rule that covers this
    today is IN FORCE and current state in `tests/README.md` (§ "Running everything", the
    catalogs-are-generate-only paragraph): `cargo check` the emitted crate under EVERY profile when a
    fix turns an abort into generated code, plus a per-shape integration fixture —
    `tests/recursive-collection-ref/` is that fixture, run both profiles by
    `recursive_collection_ref` / `recursive_collection_ref_preserve`. Trigger for the mechanical
    layer (a promotion path from a flipped PANIC row into `tests/corpus/`, or a compile leg on
    robustness `ok` rows): a SECOND instance where a robustness row flips PANIC→`ok` and the emitted
    crate does not compile. Building it now would put a per-profile nested cargo build behind a
    catalog whose whole value is running in seconds on every `cargo test`.
  - ***rustfmt, so the code never reaches rustc at all.*** Not a duplicate of the class above: there
    the emitted crate reached rustc and was rejected, so any compile leg would catch it; here
    generation itself fails at the rustfmt merge step, so no compile leg can even receive a crate.
    One premise correction this entry now carries, probed rather than reasoned (an earlier version
    asserted the opposite on the strength of a dangling doc reference): the rustfmt pass runs
    IN-PROCESS too — `generated_strings` is post-rustfmt — so any vehicle that generates the shape
    under the failing profile witnesses this class as a generation error; the residual blindness
    is the same two-part bound as the siblings' (a vehicle must exist under the failing profile at
    all, and an `ok`-flip after a fix certifies only generation). Proven instance (since
    fixed on the type-match dispatch sites and given exactly this entry's remedy — promoted into
    `tests/corpus/group_choice_fixed_special.cddl`, whose cells shell the real CLI so the rustfmt
    seam is witnessed for the shape): a bool or null fixed value in a map-representation
    group-choice arm emitted Rust that rustfmt rejects (`expected pattern, found '='`) under
    `--preserve-encodings`. It reproduced against the parent of the fixed-value-arm support commit,
    so it predated that change; making the default profile generate the uint spelling is what put a
    reader in front of it. The class is not empty: the brute-force-dispatch sibling
    (`t = true / null / tstr`) still fails at the same seam and is ledgered in
    `cddl-matrix/ROADMAP.md` § findings — a robustness row for it would record an error today, but
    an `ok`-flip after a fix would again certify only generation. Reopening signal, on the
    magnitude axis the deferred cost grows along: **the count of `ok`-recording robustness rows
    whose emitted crate fails at export** — measurable by a consumer whose generation aborts at
    rustfmt while our catalog records the shape as `ok`, a party that already has the problem and
    needs nothing from us to notice it. That count is zero today, so the signal is unmet rather
    than pre-satisfied.
  - ***another profile, with no flip, no abort and no fix widening a reachable set — the shape always
    generated.*** A fixture certifies only the PROFILES it is generated under, so a shape added to a
    default-profile fixture is unexamined everywhere else. Proven instance (since fixed and given
    exactly this entry's remedy — promoted into `tests/corpus/cbor_enum_payload.cddl`, all-profile
    by construction, with the preserve round-trip carried by `tests/preserve-encodings/input.cddl`):
    `bytes .cbor <c-style enum>` compiled and round-tripped under the default profile —
    `cbor_payload_leaves` in `tests/core/tests.rs` pins exactly that — while emitting a crate rustc
    rejected under `--preserve-encodings`, because the inlined try-each-variant sequence reused the
    enclosing `final_exprs` as its `Ok(..)` match pattern while the `.cbor` arm had pushed a value
    expression (`StringEncoding::from(<var>_bytes_encoding)`) into them. Established pre-existing by
    regenerating the same probe spec with the production files stashed, so nothing about it was a
    consequence of the change that put a reader in front of it. Two properties made it invisible
    rather than merely unpinned. First, it reached **rustc** (a call in pattern position is
    syntactically a valid tuple-struct pattern, so rustfmt formats the file happily and generation
    exits 0 — the defect was a resolution failure, not a parse failure). Second, `tests/core` is
    default-profile on both of its integration gates and in the snapshot registry, so no committed
    vehicle generated the shape under `--preserve-encodings` at all — which is exactly what
    promoting a shape into `tests/corpus/` fixes, and did. Trigger: **the count of shapes pinned
    green by a default-profile fixture that fail under another profile.** A consumer measures it
    without anything from us — they flip a flag on a spec they already have and their build breaks
    on a shape our docs list as supported. The count returned to zero when the instance above was
    fixed; the next instance means default-profile fixtures are systematically over-claiming, and
    the input extension stops being optional.

- **Parallel-constructor fixture diversity: a parser over external input must span the ident-class
  matrix of REAL specs, not the feature spec's mental model.** Proven instance: the
  `--wrapper-requests` shape parser hand-built bare `Rust(ident)` element leaves instead of
  resolving through the pipeline's alias rule, and every fixture used plain struct elements — so
  the first REAL spec fed through it (conway, where alias domain naming like
  `policy_id = script_hash` is the pervasive idiom) panicked `is_enum`'s registered-struct
  invariant on 5 of the motivating workspace's 18 committed requests. Two layers closed it: the
  MECHANICAL one is `IntermediateTypes::resolve_alias` — the single owner of the alias-substitution
  rule, called by both `new_type` and the requests parser, so a leaf built outside the pipeline
  cannot drift from pipeline resolution again (plus `workspace_requests_alias_elements_host`
  pinning the named-alias / primitive-alias / extern element classes). The residual this entry
  records is the FIXTURE principle no gate enforces: any NEW parser that reconstructs IR values
  from external content (sidecars, index files, future request formats) must ship fixtures
  spanning every ident class the IR distinguishes — registered struct, rule alias, primitive
  alias, extern, generic instance — because a parallel constructor's coverage is independent of
  the pipeline's, and synthetic fixtures inherit the feature author's blind spots. A second
  instance (a new external-input parser shipping struct-only fixtures) is the trigger to make
  this mechanical, e.g. a checklist gate over `parse_*` functions that take external strings.
  An independent first-principles review subsequently ran the full ident-class × shape matrix over
  the requests path (confirming the fix and surfacing four hardening findings, all closed with
  pinned hard errors: the stub-fidelity diagnosis for directly-exposable shapes, reserved element
  idents, the nesting depth cap, and the element-resolution mismatch appendix). The same review
  cycle also caught the PREDICATE flavor of this class mid-implementation: the reserved-ident
  pre-check first shipped as a hand-mirrored copy of `RustIdent::new`'s two assert predicates, and
  review replaced it with `RustIdent::reserved_reason` — the reservation rule's single owner that
  `new` itself asserts through. Working rule reinforced: when a fix needs the same decision an
  existing function already makes, extract the decision, never mirror it.
- **Synthesized-name residual: the referenced-but-never-minted (E0425) flavor.** The generator mints
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
  deferral-boundaries entry below.)
- **Multifile reference-POSITION coverage: two position-keyed escapes down, one enumerated position
  still missing.** Two cross-module import bugs were invisible to `tests/matrix_multifile` because
  its field-embedding modes (`named`/`anon`/`anonb`/`unref`) all reference the shape as a `bholder`
  record FIELD, while each bug lived in a different reference *position* with its own
  import-collection path: the group-ctor class (a GROUP choice over a foreign-scope Record expands
  that record's field types into `new_<variant>` ctor params, unmarked by `scope_references`;
  fixed via `EnumVariant::group_ctor_record_fields`, vector held by the hand fixture
  `tests/multifile` `relay`/`relay_host`, test `cross_module_group_choice_ctor`), and the
  type-alias-target class (a plain alias rule's `pub type` line names its cross-scope target,
  unwalked — E0412, hit in production; fixed by the `scope_references` type-alias walk). The second
  escape triggered this entry's mechanical layer: the position axis now exists as the `aliased`
  mode (`bal = <ty>`, every INCLUDED shape). A third production escape then hit the same `aliased`
  position from the SHAPE side — inside the matrix's extern/rawbytes EXCLUSION, not a missing row:
  a generic-EXTERN instance named by a non-root rule aliases a `Base<Args>` TYPE-EXPRESSION ident
  (`RustIdent::new_generic_with_base`), which the new walk fed to `set_ref` verbatim — a `<…>` in
  the scope's `use` list, rustfmt abort, consumer regen dead (feature request 07, the day after
  the walk landed). Fixed by decomposing in the walk (base imported at the base extern's declaring
  scope via the single-owner `GenericInstance::extern_base_ident`, each argument walked); pinned
  by the hand fixture `tests/extern-generic-scoped` (compile+round-trip proof
  `extern_generic_scoped`; content pin `extern_generic_scoped_alias_imports`, incl. the
  no-`<`-in-`use`-lines invariant). The exclusion itself stays — the matrix's compile floor cannot
  build extern shapes standalone — so extern-shaped placement coverage is hand-fixture-owned.
  Mechanical layer if the excluded region bites again (recur-first): the compile floor forces the
  exclusion, but a GENERATION-ONLY leg (the in-process `api::generated_strings` probe the content
  pin already uses) has no such constraint — an excluded-shapes × reference-modes generation sweep
  needs no hand defs. What remains future-facing is the `gcvariant` mode
  (`bholder = [<ty> // 1, uint]`) over a curated shape subset (the Record-resolving shapes
  `struct`/`mstruct`/`ralias` are the discriminating cells), same participation-pin idiom as
  `anonForm`/`EXPECTED_ANON_SHAPES` — the group-ctor class still rests on the single hand vector
  rather than a matrix row. Its own signal, since the two positions this entry already delivered
  have met the parent's escape-driven one: **the count of Record-resolving shapes reachable through
  a group-choice ctor that the single hand vector does not cover**, which is what the next author
  touching `EnumVariant::group_ctor_record_fields` reads off the shape list — one vector standing in
  for three discriminating cells is tolerable, and stops being so the moment a fourth shape resolves
  through that path.
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
- **Armed-but-idle harness arms (empty-at-HEAD ledgers, zero-count vector classes, per-row-kind
  gate branches) have untested first-use paths — three same-review instances recorded, no
  machinery yet.** The decode-conformance family deliberately keeps machinery armed for residents
  that don't exist at HEAD (the over-acceptance flow, the exempt ledgers, the stale guards), so a
  scoping or preservation bug in such an arm is invisible to every gate until the first real
  resident arrives — and then fires as a false red or a silent data loss. Three instances, all in
  the corpus-decode-leg delivery, all caught by in-session diff review and none by a gate, all
  since fixed: (1) the matrix arm-floor stale-exempt guard iterated a SHARED exempt ledger
  against only the MATRIX uncovered set, so the first corpus-keyed entry — which the corpus
  error messages direct users to add — would have falsely failed the matrix gate (fixed:
  per-catalog ledgers, `CORPUS_DECODE_FLOOR_ARM_EXEMPT`, each stale-guarded against its own
  set); (2) both mints' zero-candidates pin guard ignored over-acceptance pins (zero committed
  at HEAD), so the first ruby-generation gap on a row carrying one would have silently discarded
  a pin that must survive re-mints VERBATIM (fixed in both mints, with the
  don't-lean-on-`source="hand"` comment); (3) the corpus drift half skipped the
  example-reconstruction staleness check for PINNED rows, leaving a fixture edit that invalidates
  a pin's justification green until the next re-mint (fixed: pinned rows check the example half).
  The shipped exemplar of the fix pattern is the drift gate's § 8 synthetic all-fields sample —
  but it covers only the writer/reader schema, not ledgers, mint buckets, or per-row-kind gate
  branches. The mechanical layer on the NEXT instance (especially one that survives review):
  extend the drift gate's self-check section with synthetic residents and synthetic
  perturbations — a fake entry per empty-at-HEAD ledger asserting exactly its own guard (and no
  sibling's) reacts; a pure preservation check of the mint bucket logic over a synthetic
  prior row carrying every vector class; and a perturbation sweep asserting each committed field's
  drift flips the gate red for each row KIND (active and pinned). Meanwhile the working rule:
  an armed-but-idle arm lands with its first-use self-check in the same change, or the review
  explicitly walks the empty arm. A FOURTH instance is on record from the json/wasm
  decode-surface-leg delivery, in a distinct CLAIMED-GUARD flavor and a new HOME:
  `JSON_SURFACE_SKIP` (a replay-gate rust-side skip ledger, not a drift-gate arm) shipped with its
  code comment and doc claiming the reproduction guard ("a listed row that now round-trips fails
  the gate") while the leg driver consumed entries WITHOUT running the leg — the guard was
  asserted, never wired; the `WASM_SURFACE_SKIP` sibling in the same change had the real guard.
  Caught by in-session diff review (the review walked the arm — the working rule holding), fixed
  in the same series. Two consequences for the mechanical layer: the flavor to detect is "ledger
  entry consumed with zero reproduction attempt", and the layer as sketched above is
  DRIFT-gate-scoped — the replay gates' rust-side skip ledgers need a sibling arm (a synthetic
  always-passing resident driven through the leg driver, asserting exactly the stale-pin failure
  fires and no other). Count: four review-caught instances, zero escapes; the build trigger stays
  the first instance that survives review, with the layer owing BOTH homes when built. Related,
  smaller catch from the same review with its own
  trivial layer: a machine-written catalog `pinned_reason` cited a run artifact ("see mint
  log") — catalog prose is outside `lint_doc_citations`' scan surface, so non-durable citations
  there rot silently; fixed by wording review (the mint now emits self-contained tallies + a
  durable ledger pointer), and extending that lint's scan to catalog reason strings is the
  mechanical layer if it recurs.
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
  reach — the wasm-boundary clone-of-owned class stays ledgered, no machinery yet.** The
  boundary ops (`from_wasm_boundary_clone`) clone every non-Copy expr regardless of the call
  site's ownership, so a wasm ctor over an owned last-use arg emits `Holder::new(val.clone())` — a
  redundant clone `clippy::redundant_clone` would flag but is nursery-tier, so the gate's `-D
  clippy::all` cannot see it; behavior and bytes are unaffected (allocation cost only). One
  neighboring FLAVOR of this class is now retired at the source and needs no ledger: the rust
  `From`/`TryFrom` wrapper impls used to route their owned `inner` through the same boundary ops
  (`new(inner.clone().into())`), but they now construct via `new(inner)` directly — the `inner`
  is by-value and already `new()`'s exact param type, so both the clone and the `.into()` were
  identity (also a `clippy::useless_conversion` the gate DOES see, provoked by `clippy_wrapped_map`
  in `generated_code_clippy_clean`'s input). The wasm-boundary clone-of-owned flavor above stays
  ledgered with unchanged disposition, mirroring the curated rustc style-deny precedent
  (`unused_parens` et al.): evaluate specific beyond-`all` lints one at a time, adding a per-lint
  deny only if it is currently green-able on every one of the gate's cases (nursery lints carry
  known false positives). Act on a second instance or a consumer report, not before.
- **`unused_imports` on generated crates — residual trait-import class the name-scan model cannot
  reach.** The rustc-warning DETECTOR is live and BROAD — the generated-code unused-import scan
  (`unused_generated_import_lines`) inside `feature_corpus_compiles` fails on ANY `unused import`
  warning in the purely-generated crates, and (v3) its sibling `unused_generated_variable_lines`
  fails on ANY `unused variable` warning too, cache-key-versioned by the `lint=unused-imports-v3`
  marker (current state in `tests/README.md`'s description of that gate). The variables half was
  added on a consumer-reported instance (the constant count-arm `Some(x) => 1` binding): the corpus
  held the provoking shape all along (33 committed-snapshot instances), so the escape mechanism was
  a missing ASSERTED CLASS, not input poverty — `generated_code_clippy_clean` had kept
  `unused_variables` at warn, lumping it with `unused_imports`, whose stay-warn rationale (the
  legitimate trait residue below) has no variables analogue. That gate now denies
  `-D unused_variables` across all four of its cases and all three generated crates, so the class
  has two owners; `unused_imports` stays at warn there, because the trait residue is real. A second known DETECTOR blind
  spot, proven by the path-tail instance below: the scan lives only in `feature_corpus_compiles`
  cells, which never generate under the cross-crate workspace flags, so prune imprecision visible
  only in `--wrapper-requests`/`--workspace-dep` output (the requested-collections sidecar) reaches
  consumers before any gate — a detector-coverage cousin of the input-poverty sub-class ledgered in
  the flag-powerset entry (there the swept input lacks the shape; here the scanning gate lacks the
  flag). That family is now scanned where the crate under cargo is 100% generated:
  `assert_no_unused_generated_warnings` runs both scans over the nested cargo stderr the
  workspace-requests gates already capture — `workspace_requests_hosts_cross_scope_elements`
  (facet-1 wasm check), `workspace_requests_cohosted_keys_list_no_self_import` (wasm check) and
  `workspace_requests_hosts_borrowed_wrappers` (both wasm32 builds) — at no added cargo cost.
  Wiring the REMAINING capture sites (the `extern_deps` family, which reaches cargo through
  `run_test`) is blocked behind two things, in order. First an EMISSION defect the wiring probe
  proved: a generated root `src/generated/mod.rs` emits `use serialization::*;` that rustc reports
  unused — verbatim `warning: unused import: `serialization::*``, at
  `tests/extern-deps/export/rust/src/generated/mod.rs:81`,
  `tests/extern-deps-non-preserve/…:55`, `tests/extern-deps-wasm/…:63` and, decisively, plain
  `tests/multifile/export/rust/src/generated/mod.rs:59`, so the class is MULTIFILE-shaped rather
  than cross-crate-flag-specific. It reproduces at both capture sites of each fixture (the rust
  `cargo test` and the wasm `cargo build`/`test`) and is committed in the fixture export trees, so
  it is greppable without running anything. This is consumer-visible build noise, not an internal
  fixture wart: every consumer of a multifile-shaped spec gets the same `unused import` warning on
  every build of their regenerated crate. Probed against the 13 enumerated
  workspace-requests/extern-deps gates' capture sites; NOT probed against consumer regens or
  against other multifile fixtures. Two things in the same stderr are NOT this finding: the
  documented `Serialize` trait residue (already exempted by the scan) and the warnings from
  `tests/extern-dep-crate/src/*`, which is hand-written stand-in code rather than generator output.
  Second, once the emission is fixed, the `run_test` sites need a generated-files-only restriction
  before the scan can be wired there at all — those crates carry hand-appended `tests.rs`/
  `deser_test` modules and path deps on hand-written crates, whose warnings the raw line scan
  cannot tell from the generator's. Proven instance of exactly the blind spot the scanning covers
  (2026-07-22, consumer-reported, fixed class-level): the used-ident scan counted `::`-path-tail
  segments (`cml_chain::assets::Coin` counting `assets`), which collide with the parent's `pub mod`
  defs, so `super_glob_needed` conservatively kept the sidecar's dead `use super::*;`; the
  path-tail exclusion in `collect_idents_in_tokens` removed the false-positive class (unit + map-
  level twin tests pin both directions). The entry's earlier escalation trigger
  (a 26-warning consumer report + chain/cip36 regen) fired and is delivered — what the prune now
  removes (the `super::*` and `<common>::error::*` globs against a fully-enumerable universe, the
  wasm prelude names, the `--wasm-*-macro` leaf names, the cross-scope type idents
  `scope_references` over-imports) is the shipped contract in `docs/docs/output_format.mdx`, and the
  protector/disqualifier model it removes them by, with its pins, is current state in
  `tests/README.md` § "Extern-interface export & `--extern-import` (the machine-generated stub
  channel)". Still future-facing: the `cbor_event::se::Serialize` TRAIT
  import the non-canonical serialization prelude emits — a trait is exercised by a method call
  whose ident never appears, so name-scan cannot prove it unused; the detector's
  `UNUSED_IMPORT_TRAIT_RESIDUE` skips it. What a consumer sees is one rustc `unused import` warning
  on every build of a crate whose flavor does not need it: `push_base_serialize_imports` pushes the
  import whenever `!(preserve_encodings && canonical_form)`, while specs whose emitted impls name
  the trait only by full path (`impl cbor_event::se::Serialize for …`) never exercise it — reported
  from a downstream migration against a cip25-shaped non-preserve crate. The design that would
  retire it, recorded so it is not re-derived: a trait's only ident-invisible use is METHOD-CALL
  position, so `cbor_event::se::Serialize` is justified in a module family iff the family contains a
  bare `Serialize` ident (UFCS, a bound, a trait object — all of which the scan already sees, since
  a path TAIL like `cbor_event::se::Serialize` is correctly skipped) OR an ident `serialize`
  IMMEDIATELY PRECEDED BY A LONE `.`. The `.`-precedence is the whole load: every generated
  `impl … Serialize for X` block DEFINES `fn serialize`, so counting the bare ident would justify
  the import in every file and the naive version is useless;
  `.serialize_as_embedded_group(..)` is a different ident, so token-level matching needs no special
  case, and `collect_idents_in_tokens` already tracks `prev1`/`prev2` for the path-tail skip, which
  makes the detection itself a small local addition. The COST is not there: `is_candidate` keys on
  the import's leaf ident against `ALLOWLIST ∪ PruneConfig::extra_candidates`, and justification is
  a single `used` ident set per file, unioned across a module family through the super-glob
  protector logic — a method-call-justified candidate needs a SECOND ident set threaded through
  `used_by_path` and through every protector-union and disqualifier path beside the existing one.
  That is a structural change to a pass whose entire design is conservative-keep, and its failure
  direction is the bad one: over-prune is a consumer COMPILE ERROR where under-prune is only a
  warning, and the gate that would catch an over-prune is `feature_corpus_compiles`, so the change
  is only worth making in a cycle that can run it. Retiring the detector's
  `UNUSED_IMPORT_TRAIT_RESIDUE` exemption (and its self-test) is the other half of the same change —
  the residue and the exemption go together or not at all. Reopening signal: a second unremovable
  trait joins the residue, or a consumer for whom the warning is more than cosmetic (it breaks a
  `-D warnings` build, or it masks a real unused import in the same crate). Also watched,
  warning-severity only (never a compile error):
  the one deliberately-conservative keep the disqualifiers do not cover — an intermediate module
  between the ancestor and a deeper protector that consumes the ancestor's copy for everything
  below it; replace the per-descendant approximation with exact resolution modelling only on a
  real warning report from the live arm.
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
  needle instance is the signal to design a detector for that flavor too, same policy. A SECOND
  needle-vacuity instance is on record in a distinct STALE-LITERAL flavor:
  `corpus_occurrence_bounds_enforced`'s negative needle pinned the exact emitted spelling of the
  element-value RangeCheck cast, so the i128 widening of `RangeCheck` (which renamed every
  emission from `as isize` to `as i128`) would have left it matching nothing — green forever
  while witnessing nothing — had the widening session's exhaustive grep of the old spelling not
  caught it (the needle was retargeted to `found: x as i128` in the same change). Reading, not a
  gate: a vacuous negative needle passes by construction, and the harness-scoped mutation sweep
  is blind to this flavor too (no mutant renames emitted text). The mechanical layer if THIS
  flavor recurs: pair every negative needle over emitted text with a positive LIVENESS witness
  asserting the same literal still occurs where it is legitimate (e.g. a constrained-int
  serialization snapshot DOES contain `found: x as i128` beside the occurrence cell's does-not),
  so an emission rename flips the liveness half red instead of leaving the negative half
  vacuous. Meanwhile the working rule when renaming emitted text: grep the test tree for needles
  pinning the old spelling in the same change.
- **`--extern-wrapper-index` deferral-boundaries — build the deferral-profile leg (DUE):
  per-wrapper emission MODE × wrapper shape × reference POSITION, with a per-mode compile floor.**
  The axis: every wrapper a spec implies has an emission MODE (local vs index-deferred vs
  workspace-borrowed under `--workspace-dep` vs requested-hosted under `--wrapper-requests`)
  crossed with the wrapper-shape space, and no existing honesty rule sweeps it — established by
  enumerating the registries, not by grep (scope: the full check.ts gate registry — no gate
  generates with any mode flag except `test` via the hand fixtures; `ALL_PROFILES` names no mode;
  the wasm-ABI/multifile SHAPES×ROLES pass only `--wasm=true`; and the rust↔wasm parity
  differential scopes out directory-input fixtures, which every dep-owning mode configuration
  requires — so parity is structurally unable to observe any of it). The ~20 committed mode pins
  are incident-shaped, not enumeration-shaped: each pins the exact cell of a past escape, and the
  seam the leg fills is the difference between that list and the grid. Leg spec: each
  extern-capable shape probed once per mode, crossed with reference POSITION (inline-anonymous vs
  NAMED-rule declaration vs named-rule REFERENCE from another rule's member — the flavor that
  takes `set_ref`, which inline pins never touch — plus non-root declaring scopes), asserting the
  mode outcome AND the shape row's IMPLIED COMPANIONS per mode (a table's synthesized keys-list:
  imported-from-dep vs root-minted vs co-hosted; a restricted wrapper's loose `try_from` source,
  list and MAP flavors; nested inner wrappers; the sole-owner structural alias), floored per mode:
  consumer wasm `cargo check` for local; + wasm32-unknown-unknown link with the dep wasm crate
  for the deferring modes (duplicate `#[wasm_bindgen]` symbols are visible ONLY at wasm32 link —
  the `extern_wrapper_index_defers_to_dep` RED leg proves check/test alone cannot see them);
  `cargo check` of the HOST crate for requested-hosted (one arming instance was observable only
  there). Two participation facts the grid must encode rather than assume: the grid is NOT a full
  cross-product — `@duplicates reject` set wrappers can be HOSTED but can never DEFER
  (`generate_reject_ordered_set_type` consults no defer seam, and no consumer generation path can
  record a reject borrow — so two consumers of one dep both mint the set class and would
  duplicate-symbol in one cdylib, the exact class workspace mode closes for lists/maps; inference
  from call-site enumeration, no fixture), while a reject rule's loose `try_from` SOURCE is
  defer-capable — "reject wrapper local + loose source deferred" is a live uncelled combination;
  and the index is NAME-only, which is flavor-SAFE rather than flavor-blind, because the structural
  name carries the container (`PairMapKToV` vs `MapKToV`) — a cross-flavor index match is
  unrepresentable, not merely unlikely, so the grid needs no hazard cell for it. The flavor's two
  cross-crate carriers are each pinned end to end (the sidecar shape column by
  `workspace_requests_hosts_preserve_pair_map_twins`; the paste-able manual-override rule line by
  `preserve_map_defer_hint_is_paste_able`), which makes a preserve table an ordinary SHAPE ROW of
  this grid, owed one cell per mode like any other shape. Arming evidence — the recur-first trigger is over-met,
  and every instance was found by reading the emitters, reported by a consumer, or found by a
  probe, never by a gate: (1) NAMED-table × workspace-borrowed (a synthesized keys-list whose
  deferred import only the inline-map reference position ever registered: E0412 stranding plus a
  false criterion-9 shadow warning; pinned by `workspace_dep_named_table_deferred_keys_list`);
  (2) requested-hosted × co-hosted-keys-list (the host importing from root a class it mints
  itself: E0432; pinned by `workspace_requests_cohosted_keys_list_no_self_import`); (3) a RED
  found by probing the named-REFERENCE position under index mode, closed by this entry's first
  cut (see the ruling below) — probe scope: default
  profile, root scope, array + table flavors; not probed: preserve/json, non-root scopes, the
  wasm32 link): a user rule whose ident equals a dep-indexed structural name
  (`idx_foo_list = [* idx_foo]`), referenced ONLY by rule name from a record field, generates at
  exit 0 with EMPTY stderr and a wasm crate that fails `cargo check` with E0425 (twice, exit
  101) — the mint side records the deferral, but a named-rule reference routes through the
  alias-suppression arm and plain `set_ref`, which never consult the deferred map, so no import
  is ever routed; the same probe's TABLE flavor (`map_u64_to_idx_foo = {* uint => idx_foo}`) is
  screened by `exists_in_rust`, mints locally, and silently re-exports a name the dep's index
  also lists — the duplicate-symbol-at-link configuration. Instances (1)–(3) are ALL failures of
  a companion or a position, never of the primary wrapper — which is why the leg crosses POSITION
  and asserts COMPANIONS rather than probing primary shapes alone, and why it doubles as the
  regression net for accidental-provider removals ("who else relied on this walk path?" answered
  mechanically). Closed context the leg builds on — the NonEmpty defer boundary:
  `generate_non_empty_array_type` / `generate_non_empty_map_type` consult `try_defer_wrapper`,
  their loose `try_from`-source mints defer normally, and the source's conversion-internal import
  is routed at the restricted class's emission scope
  (`register_deferred_non_empty_{list,map}_source`, the same follow-the-class pattern as the R3d
  keys-list registration); pinned by `extern_wrapper_index_defers_to_dep`'s
  `[+ idx_foo]`/`NonEmptyIdxFooList` cell plus its order-hostile deferred-source cells
  (`abc_bars = [+ idx_bar]` walked first; the inline `only_nb_baz: [+ idx_baz]` twin); the
  MAP-side source routing is the same helper pattern but has no dedicated cell in any mode
  (the map-side source and the reject-set combination now have cells — see the first cut below).
  The DECISION this entry rode — USER rules claiming a dep-indexed structural name — is RULED and
  SHIPPED as **honest defer**: the named-reference path consults the deferred map exactly as the
  inline path already did, so the two reference positions agree and the exit-0/E0425 branch is
  gone; the array/defer flavor warns that the authored rule and the dependency's class are
  UNIFIED on the wasm surface (the consumer's rule name now resolves to the dep's class, with the
  rust-side alias kept); and the table flavor — screened by `exists_in_rust`, so it keeps the
  consumer's own class — warns that both crates then export the same `#[wasm_bindgen]` name.
  Delivered as this entry's FIRST CUT, pinned by
  `extern_wrapper_index_named_rule_reference_unifies_with_dep` (both flavors, both warning texts,
  consumer wasm `cargo check` floor) and `extern_wrapper_index_deferred_try_from_sources` (the
  map-side deferred source, its sole-owner-screened variant, and the reject-set × deferred-loose-
  source combination), over `tests/extern-deps-index-named/inputs_named/lib.cddl` and
  `tests/extern-deps-index-named/inputs_sources/lib.cddl`.
  **What remains of this entry is the sweep proper**: the table-driven, participation-aware grid,
  batched per (mode, floor), whose value — grid completeness — is not a property of any cell
  subset, with the per-mode floors above and the compile/link floors at full tier. The first cut
  de-risked its design (it forced both the participation table and the DECISION), so the sweep
  starts from a settled grid rather than a cold one. One probe-seeded cell the first cut surfaced
  and did NOT close, because it is a different branch than the one ruled on: a rule-declared
  wrapper whose constituents include a CONSUMER-owned type takes the R3c "mixed → local, silent"
  path BEFORE any rule-declared consideration, so a nested rule like
  `arr_idx_foo_list = [* idx_foo_list]` mints locally and silently under a name the dep's index
  also lists — the same duplicate-symbol configuration the table flavor now warns about, reached
  by a path that warns nothing (probe scope: default profile, root scope, index mode; not probed
  under workspace mode). (The cross-crate
  duplicate-symbol flavor of the synthesized-name interaction class stays owned HERE: the
  duplicate-ident backstop scans one crate's own files and
  `synthesized_name_interaction_sweep` spells no dep-index cells — see `tests/README.md`
  § "Synthesized-name interaction sweep + duplicate-ident backstop".)
- **Extern-deps wasm-boundary surface: packaging- and json-gen-gaps beyond the behavioral floor.**
  The split-dep cell (`integration_tests::extern_deps_wasm`, `--extern-wasm-crate`) drives the
  generated wasm crate's cross-crate wrappers behaviorally: `tests/extern-deps-wasm/tests_wasm.rs`
  constructs `Everything` and the non-root `nested::NestedHolder` through the wasm API (all eight
  collection wrappers — `ExternCrateFooList`/the table wrapper plus the `nested` module's
  non-root-use `ExternCrateBarList`, `NestedItemList`, and the `NestedItem`/`ExternCrateBar`-keyed
  maps whose keys() re-mint the element list), CBOR round-trips, and value-anchors every getter
  across the boundary (element fields via `.as_ref()` to the dep rust types, map lookups, keys()
  readback), so a semantically wrong boundary conversion (`get`/`add` through the dep's `From`
  impls, an identity `.into()` where a transform was needed) now fails rather than compiling green.
  This mirrors the behavioral floor the sibling index fixture already has
  (`extern_wrapper_index_defers_to_dep` over `tests/extern-deps-wasm-index`, which additionally
  links for the real `wasm32-unknown-unknown` target). Two layers above "it builds and round-trips"
  stay unprobed: (a) `wasm-pack`/bindgen-CLI packaging — `cargo build`/`cargo test` cannot see
  duplicate exported JS class names when the dep wasm crate and the consumer both export a
  like-named wrapper (the generate-locally policy makes this reachable); no gate runs bindgen-CLI
  over the extern fixture. (b) json-gen against extern-dep types — `add_schemas` SKIPS
  dep-owned rows (a dep type's schema root is owned by the dep's own json-gen run, and this crate's
  json-gen manifest depends only on the own rust crate), pinned in-memory by
  `snapshot_tests::json_gen_extern_schema_rows`. COMPILING emitted json-gen crates at breadth is
  owned by `cddl-matrix/ROADMAP.md`'s extern-compile-breadth entry; the residual owned HERE is
  execution — no gate RUNS `export_schemas()` from a json-gen crate built from a spec carrying
  extern deps (such a crate now compiles precisely because the dep rows are skipped; the unprobed
  layer is the runtime over the surviving in-crate rows in that configuration).
- **A generic RAW-BYTES base is refused, where a generic EXTERN base is recorded-and-skipped — the
  asymmetry is the decision, and only one direction of it is swept.** A raw-bytes type is exactly
  its own bytes and carries no element type a parameter could name, so
  `foo<T> = _CDDL_CODEGEN_RAW_BYTES_TYPE_` is a parse-time graceful rejection
  (`robustness_tests::generic_raw_bytes_base_rejects_gracefully`, plus the fast-tier
  `snapshot_tests::extern_interface_check_refuses_generic_raw_bytes_base` beside the two skip
  tests); an extern base names an arbitrary hand-written type that MAY legitimately be
  parameterized, so its generic-ness is recorded and the extern-interface self-check and the
  json-gen schema-row emitter skip it. What is NOT swept is the rest of the marker×directive
  product at a generic base — `@copy`, `@raw_bytes_flavor` and `@extern_companions` each have their
  own reachability at these two branches, and each is pinned only where someone thought to look.
  Reopening signal: a second marker-branch directive found accepted-and-inert on a generic base
  (the count of hand-pinned marker directives is the magnitude that grows here, not the number of
  consumers).
- **Twin in-repo implementations of one semantic decision drifting apart (emission spellings,
  detection walkers, cross-language scanner mirrors) — single-owner extraction is the fix
  pattern; only the directive-SET flavor has a firing detector so far.** The in-repo sibling of
  the parallel-constructor residual's predicate lesson above ("when a fix needs the same decision
  an existing function already makes, extract the decision, never mirror it"): two implementations
  of one decision, both living in this repo, drift apart while every behavioral oracle stays
  green — semantically-equivalent drift has no oracle but reading (decode conformance sees equal
  accept/reject sets, the mutation sweep sees no behavioral delta, the compile gates stay green;
  pending-system coverage check done at recording time: the grammar-fuzzer escalations own only
  the ACTIVE missing-import flavor of the walker instance — a recombination shape table extended
  to nested container positions would compile-fail it, E0433 — and NO pending item sees the
  semantically-equivalent flavor). Three read-caught instances, each fixed by single-owner
  extraction and pinned: (1) `generate_wrapper_struct` hand-built its range-check
  `cond`/`failure_expr` beside the shared helpers and drifted three spellings apart (`!= N`
  collapse, zero-min lower-leg elision, an inline float-payload duplicate) —
  `bounds_check_if_block` + `range_check_err` (`non_negative`/`location` params) are now the
  single owner, pinned by the `bounds_spellings` corpus fixture; (2) `uses_non_empty_{vec,map}`
  hand-walked the `RustStructType` variants, mirroring `visit_all_rust_types`' position
  enumeration, and drifted two legs short (named-array `element_type`, table `domain`) — masked
  by the transparent alias every top-level array/table rule co-registers; now folded onto the
  canonical visitor, pinned by `nonempty_nested_positions`; (3) cross-language:
  `corpus_detect.ts`'s `DSL_TAGS` scanner is a documented hand mirror of `comment_ast.rs`'s
  directive grammar and drifted twice (no `@used_as_elem`; pre-flavor `@used_as_key` grammar),
  with its hand-picked selfCheck vectors drifting WITH it — the layers that fired
  (`verify.ts`'s docs-lint, `project_corpus`'s content-drift check A) trigger only via adjacent
  artifacts (a docs mention, a `[[cover]]` entry), not the mirror itself. A fourth, BYTE-COUPLED
  flavor was read-caught before it fired (D1's core/alloc rewrite, 2026-07-29):
  `composed_runtime_static_files`' preserve flavoring `.replace()`s a byte-exact import line that
  lives in `static/non_empty_map.rs`, so rewriting that line's path would have silently no-op'd
  the substitution — distinct from the instances above in failure mode: not silent (the mangled
  flavor is an E0432 in every preserve corpus-compile cell, `local` tier) but loud-LATE and far
  from the causing edit, and invisible to any grep of either file alone. Fixed as a synchronized
  edit with a move-together comment on BOTH sides; the instance-level tripwire, due on the next
  touch of that composition code, is a must-hit replace (assert the pattern occurred before
  substituting — the selfCheck-lockstep analog for byte mirrors, turning the no-op into a
  generation-time panic that every snapshot regeneration sees at `fast` tier). Directive-SET
  drift now has a firing detector: `corpus_detect.ts`'s selfCheck lockstep tripwire demands set equality
  between the mirror's `MIRRORED_DIRECTIVES` and the authority's `tag("@…")` literals at import
  time, so `project_corpus` (fast tier, CI) fires the moment a directive is added or removed on
  either side. Remaining work, each piece behind its trigger:
  - *On the next read-caught instance of the class:* build the mechanical layer — a
    same-construct differential sweep (emit one bounded/flagged construct per site class — member
    ctor/setter, wrapper `new()`/deserialize, primitive-deserialize `.and_then`, collection len
    check — and diff the check conditions + failure payloads, ledgering deliberate site-specific
    differences), plus the cheap grep half: a snapshot-wide wart scan (same-N `< N || > N`, dead
    `< 0` over unsigned/`len()` exprs) in the spirit of the doubled-doc-marker scan above.
  - *On the next `comment_ast` grammar change of ANY form (not just a new directive):* do the AST
    floor instead of hand-extending `DSL_TAGS` again — parse the comments with the real
    `comment_ast` via a small `examples/` binary, as the role floor already does with
    `ast_roles.rs`. The floor is the only fix for the tripwire's blind spot, ARG-GRAMMAR drift
    within an unchanged directive set (the proven `@used_as_key` flavor shape: no set delta,
    changed consumption, a new panic path), because hand-picked selfCheck vectors drift WITH the
    mirror by construction. The floor's scope also includes the mirror's flavor-set→sibling-id
    crediting (`dsl.used_as_key.{hash,ord,hash_ord}`, mirroring `DemandSet`'s bare/flavored
    exclusivity) — hand-mirrored consumption accreted without any `comment_ast` grammar change,
    which is why this trigger has correctly not fired yet.
  - *Its own reviewed change, never a drive-by:* unify the zero-min RangeCheck payload — the same
    authored zero-min bound reports `min: Some(0)` in wrapper payloads vs `min: None` at member
    sites because the drop-the-redundant-zero decision itself has multiple owners
    (`RustType::with_bounds` normalizes the member channel at IR build; the wrapper's `min_max`
    channel bypasses it; parsing's occurrence filter and `classify_sign_arm`'s uint-arm filter
    spell the same decision in their own domains). A user-visible error-message change.
  - *Non-item, recorded so it isn't re-litigated as a coverage gap:*
    `bounds_check_expr_non_negative`'s Array/Map leg and the member-side zero-min elision are
    defensive-only — every zero lower bound is dropped upstream by the owners above before
    emission, so the corner is unreachable end-to-end and a fixture cannot pin it.
- **Tree-wide sweeps over gitignored regeneration targets count DEAD files as product — one
  proven near-miss, no machinery (and none possible tool-side).** Proven instance (D1's Phase-0
  inventory, caught by classification discipline, not by any gate): a long-lived checkout's
  `tests/*/export*/` trees held 151 stale `.rs` at `<crate>/src/*.rs` from the pre-thin-root
  layout — nothing compiles them (the thin root binds only `src/generated/**`), and a sweep that
  counted them concluded the emitted rust crate has a `std::io` surface, which it does not. The
  near-miss was writing that phantom surface into a delivery inventory as fact and sizing work
  against it. No tool diagnostic can own the class: the crate `src/` level is USER territory by
  the ownership contract (hand modules beside the seed-once `lib.rs` are legitimate, and the tool
  cannot tell a user's file from its own pre-layout-change leftovers), and the trees are
  gitignored, so no repo gate sees them either. Working rule (the fix that worked): classify
  swept files by the CURRENT write-site key set (`src/generation/export.rs`'s write sites are the
  authority), never by presence on disk, and state the unmatched remainder as non-product in the
  sweep's scope note; hygiene half — delete-and-regenerate (or `git clean`) the export trees
  before any tree-wide sweep on a checkout old enough to predate a layout change. Mechanical
  layer on a SECOND distorted-analysis instance: a sweep-preflight that regenerates the swept
  trees first, so the on-disk state IS the current write-site set.
- **Run-local values in gate-cache key material — one proven instance, no machinery yet.** The
  TS-side cache keys hashed the literal nested-cargo argv, which embeds the run's `mkdtempSync`
  probe dir — every key was unique to its run, so verify.ts could never hit its cache across runs
  (fixed by path-normalizing the key argv; the normalized form is documented in `tests/README.md`
  § "The gate cache (memoize-and-skip for nested cargo)"). Caught by merge-review reading, not by
  any gate — and `verify_cache_transparency`'s ≥1-hit vacuity floor is BLIND to this class by
  construction: identical generated trees hit WITHIN a run (the probe dir is constant inside one
  run), so the hit count stays above zero even when no cross-run hit is possible. Mechanical layer
  if the class recurs (a second run-local value — an env var, a timestamp, a counter — leaking
  into key material): a key path-independence self-test (hash one fixed byte tree under two
  different scratch roots in two separate invocations; assert `gateCacheKey` / the Rust
  `gate_cache_key` produce identical keys), plus a cross-run hit floor that only counts hits whose
  entries PREDATE the running process (within-run hits excluded, so the floor actually measures
  cross-run keyability).
- **Control bytes in committed source files — one proven instance, no machinery yet.** A committed
  TS script embedded a literal 0x00 byte in a template-string separator; grep/diff/review tooling
  then classifies the whole file as binary — silently exempting it from every text-shaped check
  and from readable review — while `tsc` parses it happily, so `matrix_typecheck` cannot see the
  class (fixed by spelling the byte as a backslash-u0000 escape). Working rule meanwhile: escape
  sequences, never literal control bytes, in source text. Mechanical layer if the class recurs: a
  tracked-source text-cleanliness lint — every tracked `*.ts`/`*.rs`/`*.toml`/`*.md` must be valid
  UTF-8 with no control bytes outside tab/LF/CR (a one-command scan; natural sibling of
  `lint_doc_citations` in the local tier).
- **Gate-script health is in no cache or staleness key — a fresh worktree/cold run is the only
  detector, and warm runs launder pre-split rot.** Proven instance: `fuzz/generate.sh` predated the
  thin-root split and still read generated serialization from the pre-split `src/serialization.rs`
  and appended the custom-serialization helpers into the seed-once crate root `lib.rs`; a COLD
  regeneration (fresh worktree, or `--refresh-fuzz`) therefore died at the probe-list step while warm
  machines never re-ran the script (`fuzz_compile_rot` re-runs `generate.sh` only when
  `fuzz/generated` is absent — see `tests/README.md` § the fuzz gate), so pre-split state kept the
  gate green. Fixed by pointing the probe lists at `src/generated/serialization.rs`, appending the
  helpers into the clobbered `generated/mod.rs` (with `rm -f` of the stale root `lib.rs` and
  `--no-preserve-comments`, mirroring `integration_tests::preserve_encodings`' `run_test`), and adding
  a pointed existence check inside `gen_probe_list` so the NEXT layout move fails with a diagnosis
  rather than an opaque grep failure under `set -e`. Working rule meanwhile: after any generated-crate
  LAYOUT change, cold-run the regeneration scripts that consume generated layout (`rm -rf
  fuzz/generated` or `--refresh-fuzz` before the run), since no gate re-runs them on a warm tree.
  Mechanical layer on a SECOND regeneration-script rot instance: a periodic cold-regen leg (or a
  script-consumed-path staleness key that forces `generate.sh` to re-run when the generated layout
  changes) — the only layer that catches script rot without a human remembering to cold-run.
- **A hand-maintained source→dependency mapping in a manifest changeset can silently under-assert:
  the tool exports source referencing a crate that no changeset op guarantees.** Proven instance
  (consumer-reported, not caught by any gate): the pre-crate-shaped `--export-static-dir` wrote the
  composed runtime source into a consumer's shared crate while owning no manifest at all, so a
  cbor_event major bump updated every generated crate's manifest EXCEPT the shared runtime's, which
  kept the old pin — caught only because the 2.x→3.x API break fails compilation loudly; a
  source-compatible-looking skew would have shipped silently. Fixed structurally by the
  crate-shaped `--export-static-crate` (the export merges the target's `Cargo.toml`;
  regression-pinned by `export_static_crate_writes_composed_runtime_and_manifest`'s hand-manifest
  leg), and the warm-up dep-universe gate sweeps `ops_for_static_runtime` alongside the other three
  op sets. The RESIDUAL: the mapping from composed runtime files to dep ops (`cbor_event`/`hex`
  always; `hashlink`/`serde`/`schemars` per flag) is hand-derived from grepping the static
  files, and nothing re-checks it — a `static/*.rs` runtime file gaining a reference to a new
  external crate (or to an existing one under a flag combination whose op condition doesn't match)
  would under-assert again, and only the consumer's build would notice. Working rule meanwhile: a
  change touching the static runtime files or `composed_runtime_static_files` re-greps the composed
  file set for foreign-crate path roots and reconciles `ops_for_static_runtime` in the same commit.
  Mechanical layer on a SECOND instance: a bidirectional drift gate over the maximal-flavor
  composed file set — every known dep name appearing as a path root in the composed text must be
  asserted by a static-runtime op, and every asserted dep must appear in the text — a natural
  sibling of `warmup_manifest_covers_registry_dep_universe`, accepting the source-scan heuristics
  then, not before.
- **A dependency swap can pass compile-parity and every existing gate while silently changing
  behavior our public API re-exposes — one proven near-miss, pins landed for the instance.**
  Proven instance (caught by orchestrator differential probing, not by any test): the
  linked-hash-map→hashlink swap plan carried an `insert`→`replace` "compatibility" forwarding
  built on a twice-source-read premise that linked-hash-map's `insert` replaces in place. It
  moves the key to the back — identically to hashlink — so the forwarding would have CHANGED
  shipped wire-order behavior while presenting as preservation, and the swap's real divergence
  was elsewhere (`Entry::or_insert` on an occupied entry: hashlink refreshes to back,
  linked-hash-map doesn't). Every existing layer is structurally blind to the class: round-trip
  and golden-hex oracles never MUTATE through the dep surface (decode→mutate→re-serialize is
  where order semantics bite); snapshot pins hold emitted TEXT, which a backing swap legitimately
  churns; and mutation scoring runs against the CURRENT dep, so it measures coverage of today's
  baseline, never a baseline change. We therefore pinned the incumbent's semantics at the wire
  level BEFORE swapping — `ordered_hash_map_{insert_overwrite_moves_to_back,
  or_insert_keeps_position,from_iter_duplicate_keys,entry_match_shapes}` in
  `tests/preserve-encodings/tests.rs`, committed green on the old backing in their own commit, so
  the swap had to reproduce bytes rather than bless new ones — instead of trusting source reads
  (two independent ones agreed and were both wrong; only an executable differential probe —
  identical op sequences on both crates, iteration-order traces asserted equal — broke the
  premise). Residual: the pinned set covers the CONSUMER-PROBED mutation surface (insert,
  or_insert, from_iter, entry match shapes); other Deref-reachable mutation ops
  (`remove`/`extend`/`pop_*`) are differential-probed equal but unpinned, and other re-exposed
  dep surfaces (the `cbor_event` types in every deserialize signature) rely on the round-trip
  oracles alone, which do cover their read/write behavior but not order-adjacent mutation — no
  such surface exists there today. Working rule meanwhile: a change to the VERSION FLOOR or
  IDENTITY of a dependency whose types our public API re-exposes (a Deref target, a re-exported
  type, a type embedded in public signatures) first lands wire-level pins of the incumbent's
  consumer-reachable behavior in a separate commit, then swaps under them — and parity premises
  between two crates are established by executable differential probes, never by source reads.
  Mechanical layer on a SECOND swap/bump instance of this class (candidates: the cbor_event
  fork's crates.io upstreaming flip, a hashlink 0.13 floor raise): a curated dep-semantics pin
  suite per re-exposed surface, enumerated from the public-API type graph (which dep types appear
  in `pub` signatures/Deref targets), so the pin obligation is discovered by a gate rather than
  remembered by a rule.
- **A fixture cell's verdict can be an accident of cargo feature unification: harness-spliced
  hand code needing a feature a dep reshape disabled stays green whenever a SIBLING dep in the
  same fixture graph re-enables that feature — so the red arrives on a later graph change, not on
  the change that broke the code.** Proven instance (D2, the alloc-mode dep reshape): two
  committed hand fragments (`tests/custom_serialization`, `tests/custom_serialization_preserve`,
  spliced into fixture crates by the integration harness) boxed `hex::FromHexError` as
  `Box<dyn Error>` — an impl hex gates on ITS `std` feature, which the reshape's
  `default-features = false` dropped. The non-preserve fragment failed three `local`-tier cells
  immediately (`core_no_wasm`/`core_with_wasm`/`emit_wasm_tests_execute` — the net that works,
  and the third instance of the owning-family lesson: the owning family of a DEP RESHAPE includes
  every fixture whose hand code uses that dep, a set not enumerable from the ops table, so the
  reshape delivery now sweeps the committed hand-file set as part of its battery). The preserve
  twin stayed green PRE-fix for a reason that had nothing to do with the code: its fixture graph
  carries the `cddl` conformance-oracle dep, whose own transitive `hex` uses default features, and
  cargo unifies features per graph — so `hex/std` was back on and the missing impl restored
  (`cargo tree` evidence in the fix-forward log). Both fragments were fixed the consumer way (a
  local newtype with verbatim-delegating `Debug`/`Display` + `core::error::Error` — the in-repo
  migration exemplar cited by the no_std workstream's D5 notes) rather than by re-enabling the
  dep's defaults, precisely so the fixtures model the post-reshape world; but the masked-pass
  CLASS remains: any spliced hand fragment whose compile needs a reshaped-away feature is
  invisible while some oracle-carrying sibling re-unifies it. Coverage boundary, stated so the
  deferral is scoped: the no_std drift gate (workstream D3) structurally covers the TOOL-PRODUCT
  half — fresh standalone crates checked through `default-features = false` shims with no oracle
  in the graph — and never the fixture-hand-code half, because its profiles deliberately exclude
  the harness appendages. Machinery on a trigger, not now (the class has exactly the two members,
  both fixed): a `cargo check --lib`-only cell per spliced-hand-fragment fixture family —
  lib-only checking keeps dev/oracle deps out of the unification graph, so the fragment must
  compile on the fixture's own declared features. *Reopening signal:* a third instance — a
  fixture cell flipped red by a dep-graph or oracle-version change with no edit to the fixture
  itself, or a reshape delivery's hand-file sweep finding a fragment that compiles only under
  unification. Measurable by exactly the party who hits it (the tier runner reading the E0277
  against an unchanged fixture), on the dimension the cost grows (the count of spliced hand
  fragments using reshaped-dep surfaces).
- **A profile-driven compile gate covers the compositions its PROFILES' SPECS reach, and a runtime
  file emitted on an IR PREDICATE × a FLAG is reached only by their conjunction — the first
  consumer-reported no_std break lived exactly in that hole.** Every profile of the no_std drift gate
  is chosen for a runtime surface the snapshot corpus never emits (`tests/README.md` § "The no_std
  drift gate (`no_std_check`, local tier)"); none of their CDDL contained `any`. `any_cbor.rs` is
  emitted only when the finalized IR holds `any`, and the whole of `static/any_cbor_json.rs` — eight
  nested inline `natural_any_cbor_*` serde adapters — is appended to it only under
  `--json-serde-derives`. Both flags were already in the `json_schema` profile, so the miss reads
  as flag coverage while being composition coverage: no profile's SPEC put the file in any thumb
  compile, and the consumer met the E0425 first. Owned meanwhile by that profile's extended CDDL —
  the `any_members` rule described in that section, which puts all eight adapters in the compile —
  plus the working rule that a new conditionally-assembled `static/` fragment states which profile's
  spec reaches it. Machinery
  on a trigger, not now (one instance, and the enumeration it would need — every emission predicate
  in `generation/export.rs`'s fragment assembly, paired against every profile's generated file set —
  is a second model of the assembly that can drift from it): a coverage assertion that each
  conditionally-assembled static fragment appears in at least one profile's generated tree.
  *Reopening signal:* a SECOND no_std break in a `static/` fragment that no profile's compile
  reaches — measurable by the party who already has the problem (a consumer's red shim, or a tier
  runner's red cell naming a file no cell had compiled), on the dimension the cost grows (the count
  of conditionally-assembled fragments outside every profile's spec).
- **The alloc imports of nested inline modules are hand-carried by design, and two of the eight
  `natural_any_cbor_*` adapters were missed — silent under `std`, six E0425s without it.** The
  alloc-import injector deliberately does not scan nested inline `mod X { … }` bodies: a file-top
  import added for a nested usage would be unused at file scope and still would not resolve inside
  the module, so each such module carries its own `use super::alloc::…;`. The four map adapters
  carried theirs; `natural_any_cbor_seq` and `natural_any_cbor_opt_seq` named `Vec` without one.
  Fixed minimally — the two hand imports, pointing at the canonical rationale comment on
  `natural_any_cbor_btreemap` — rather than by teaching the injector to inject into nested bodies,
  because the nested-module universe in tool output is CLOSED and enumerated (those eight adapters
  plus `--emit-tests`' `cddl_generated_tests`, which opens `use super::*;` and needs nothing), while
  a nested-aware injector buys concrete new hazards for it: per-scope dedup against a consumer's
  `cddl-codegen:insert` block already binding the same name inside the module (E0252 if injected
  blindly, which is what CML's tree would have met), ownership matching extended past the column-0
  exact-match strip rule to indented lines, per-level `super` counting, and insertion-point rules
  inside nested bodies interacting with the reserved-marker hard stops. What converts "correctness
  depends on someone noticing every new nested adapter" into a red tier is the `json_schema`
  profile's `shim_thumb` cell above, which now compiles every adapter shape. *Reopening signal* for
  the nested-aware injector: a SECOND miss of the hand pattern — a red `json_schema`-profile cell,
  or a consumer filing, on an alloc name inside a nested inline module. Measurable by the party who
  already has the problem (the tier runner or the consumer reading the E0425), on the dimension the
  cost grows (recurrence of the class the hand pattern must cover). A new nested adapter added WITH
  its imports correct does not fire it — the signal is the pattern being missed, not the set growing.
- **The gate-cache closure audit is structurally blind to GATE-AUTHORED inputs parked in scratch:
  its allowed-read table admits everything under `$TMPDIR` on the justification "the generated tree
  is hashed", which is true only while every file a cached cell reads under scratch actually lives
  inside a hashed root.** A gate that writes its own input files (hand consumer crates, appended
  modules) BESIDE the hashed tree violates the justification without violating the audit: the reads
  classify as `tmp/scratch, allowed`, the key never covers the bytes, and an edit to that input
  serves the stale PASS forever. Near-miss instance (D3, caught by the implementing agent before
  landing — no shipped gate ever had the bug): `no_std_check.ts`'s first draft wrote the host
  std-arm consumer crate outside the profile's hashed output root; the audit, had it traced that
  gate, would have passed it. What shipped instead is the discipline applied (both gate-written
  crates inside the hashed root, relative path deps so hashed bytes stay run-independent) plus the
  rule stated in `tests/README.md` § "The gate cache" — review-owned, like the design rules.
  Machinery on a trigger, not now (one cache-participating gate writes gate-authored inputs today,
  and it complies): extend the audit's model from "scratch is allowed wholesale" to "a scratch READ
  must fall under some root the traced gate hashed" — a per-gate hashed-root manifest the audit
  consumes, turning the discipline into a traced assertion. *Reopening signal:* a second
  cache-participating gate that writes gate-authored input files into its scratch root — measurable
  by that gate's author/reviewer at landing (the red-first mutation check the cache section already
  requires makes the question "is this file inside the hashed root?" unavoidable to ask), on the
  dimension the cost grows (the count of such gates, each a fresh chance to repeat the draft bug
  with no audit backstop).
- **Comment-residue false matches in text scans over emitted/prior `.rs` output — one proven
  instance, no machinery yet.** Any diagnostic or decision that SCANS generated (or user-edited
  prior) Rust text for a code pattern shares one trap: the comment-preservation overlay and the
  users of its block grammar leave the scanned pattern behind as COMMENT bytes — a
  `cddl-codegen:replaces` recorded original is the exact deleted line `//`-commented, and
  unpreserved-comment blocks / doc comments can quote code verbatim — so a substring `contains`
  scan reads deleted code as live. Proven instance (development-red-caught, not by any shipped
  gate): the extern-glue re-export survival scan's first cut used a substring match and read a
  replace block's recorded `// pub use crate::<Name>;` original as surviving glue — corrected to
  whole-trimmed-line equality before landing, with the residue case pinned by
  `extern_reexport_diagnostic_skips_replace_deleted_glue`'s chain assertions (overlay really
  deleted the live line AND the warning subtracted the name). Working rule meanwhile: a scan over
  emitted/prior `.rs` text matches live tokens (whole-trimmed-line or token-aware), never
  substrings, and its test includes a comment-residue adversarial fixture (the pattern present
  ONLY as a recorded original / comment). One untested premise worth probing when next in the
  area: `missing_reexports`' whole-ident scan over the USER's seed-once `lib.rs` — whether a
  commented-out `// pub use generated::<Name>;` line counts as "provided" (same residue class,
  opposite direction: a false NEGATIVE of the warning). Mechanical layer on a SECOND instance: an
  adversarial-fixture family enumerating every output-text-scanning decision point (the
  bounded-exception diagnostics list in AGENTS.md is the seed inventory) with one residue fixture
  each, rather than trusting each new scan to remember the rule.
- **A directive-dependent emitter edge case whose triggering SHAPE is in no hand fixture is
  invisible to every combinatorial layer — the recombination fuzzer skips ALL `dsl.*` features
  wholesale ("DSL comment in root RHS; not a clean reusable expression"), so no fuzz composition
  ever carries a directive.** Proven instance (probe-caught in review, then independently
  confirmed by reading — never a gate failure): the `definite_info` additive-length combine
  emitted a malformed `"{n} + "` expression when the conditional expression is EMPTY, a branch
  reachable only by an all-mandatory-fields open struct-map whose rest row is `@ignore`
  (tolerate-and-drop: no optionals, no captured-rest term) — no pre-existing spec could reach it,
  and only the delivery's own hand-enumerated fixtures (`details_map` in the
  `open_struct_map_ignore` snapshot, `baz` in the compiled e2e) happened to cover the degenerate
  combination. The standing discipline that catches the SHIPPED form is exactly that
  hand-enumeration: a new directive's spec must fixture the degenerate shape combos
  (all-mandatory, zero-field, empty-collection) alongside the happy paths — reviewers should
  demand the degenerate rows in the fixture list. Mechanical layer on the SECOND
  fuzzer-invisible directive-emitter instance: a directive AXIS for the recombination fuzzer —
  instead of skipping `dsl.*` ingredients, mint directive-BEARING variants of the applicable
  shape templates (rest-row `@ignore`/`@duplicates`/`@name` on map-record templates, rule-level
  `@duplicates` on collection templates), letting the existing generation sweep + layer-2
  compile gates see directive × shape products no hand fixture enumerates.
- **A hand-enumerated ban-pattern list whose AUTHORITY lives in gitignored plan files lags the
  id scheme it bans — no in-repo lockstep is possible (the tracked side cannot read the
  authority in CI), so the list drifts one delivery-phase behind by construction.** Proven
  instance (read-caught in orchestrator review; the tier stayed green): `lint_doc_citations`'s
  ephemeral-reference patterns were spelled for one phase's ids (`PROBE-B<n>`, `ruling R<n>`) —
  the next phase's spellings (`PROBE-C<n>`, the `C-R<n>` ruling form) passed the gate, and the
  tree stayed clean only because an implementing agent self-policed its own citations out in a
  follow-up commit. Fixed by construction where possible: the patterns are now phase-GENERIC
  (`PROBE-[A-Z][0-9]`, `[A-Z]-R[0-9]`, `[a-z][0-9]*-spec`), so letter-roll drift is dead.
  Residual class: a genuinely NEW id FORM introduced by a future plan document (a spelling no
  current regex family anticipates). Working rule meanwhile: authoring a new plan/spec document
  that mints an id scheme adds that scheme's spelling to `EPHEMERAL_PATTERNS` (with canary) in
  the same session, and delegation prompts keep repeating the self-contained-comments rule.
  Mechanical layer on the SECOND uncaught-form instance: a local-only canary harvest — when the
  gitignored plan directory EXISTS (developer checkouts; skipped cleanly in CI), scan its
  `*-spec.md` files for id-like tokens (the probe-index/ruling tables are structured enough to
  extract) and fail if any harvested id is matched by NO ephemeral pattern — lockstep with the
  authority exactly where the authority is present.
- **A rule-position directive is still silently LOST in the multi-line group-rule spelling — the
  fix is built and reviewed on both sides, and adoption is blocked on a maintainer push of the fork
  rev (ruled against 2026-08-04).** Current behavior at the pinned fork rev (`ac1b98ec`): for
  `grp = (\n a: uint\n) ; @x` the trailing comment is MISBOUND to the *following* rule's
  `comments_before_rule` (orphaned when the group rule is last) — a position nothing reads — so the
  directive silently does nothing. Two mechanism corrections established by the fork work, which
  earlier versions of this entry (and `parsing.rs`'s doc blocks) had wrong: the pest-bridge's
  `comments_after_rule: None` literals are PRE-MERGE defaults (comment binding is a source-position
  trivia merge that runs afterwards — the slot stays empty at the pinned rev because the merge emits
  no anchor for it, not because of the hardcode), and the supposed second lossy spelling (a last
  entry whose trailing slot is "already occupied" by a field-position `@name`) DOES NOT EXIST — a
  CDDL comment runs to end of line, so that one-line form comments out the closing paren and fails
  to parse; slot contention is unrepresentable. The fix: fork commit
  `a7ed0784e89689784ff78ed0e85c7434a3528937` (branch `local-fixes`, UNPUSHED — the durable carrier)
  emits a `RuleTrailing` anchor filled as a strict fallback, so population is ADDITIVE by
  construction (the single-line spelling keeps binding to the last entry's slot; nothing
  double-counts); the codegen side (reader merge in `group_rule_pin_metadata`, per-directive
  vectors both spellings, `no_silent_directive` cells) is prepared and verified both ways — green
  under a local path override, red at the committed pin exactly on the multi-line halves — in the
  re-parked burndown row (T1-09, `draft/burndown2/`; NB `draft/` is checkout-local). Owned
  meanwhile by `group_rule_pin_metadata`'s doc comment and the `@no_json_schema_export` docs
  section, both naming the single-line spelling as the supported one. Reopening signal, unchanged
  in substance: a consumer reports a directive lost in a multi-line group spelling their generator
  cannot reformat, or the fork is bumped for any other reason (at which point adopting `a7ed0784`
  and the prepared codegen side is the whole work).

- **On a SPLICED plain group, the rule slot accepts-and-ignores the thirteen directives with a
  field-position meaning.** Found 2026-08-04 while preparing the multi-line group sweep cells: the
  rule-position slot of a spliced group rule is consumed by exactly the four directives with NO
  field-position meaning (`@rust_name`, `@no_json_schema_export`, `@custom_json`, `@used_as_key`);
  the other thirteen (`@doc`, `@newtype`, `@no_alias`, `@copy`, `@used_as_elem`, `@ignore`,
  `@duplicates`, `@raw_bytes_flavor`, the custom-codec family, `@extern_companions`) are accepted
  and inert there — a probe sweep produced 13 genuine silent-drop cells, which were NOT allowlisted
  (an allowlist absorbing real drops is the dishonesty the sweep exists to prevent). The honest fix
  is a refusal at the slot for directives the group-rule position cannot honor — a new rejection
  seam with its own message and vectors, not a sweep entry. The never-spliced flavor already
  refuses loudly (`finalize`'s never-spliced report); this is the SPLICED flavor's gap. Reopening
  signal: an author reports writing one of the thirteen on a spliced group rule and being surprised
  by the silence — or the multi-line sweep cells land (with the fork bump above), at which point
  the sweep shape that measured the 13 drops can be committed as the red-first vector set.

- **The recombination member-kind table does not span the tagged-optional shape, and one exit-0
  uncompilable crate escaped through the gap.** The `#6.n(T / null)`-under-`--preserve-encodings`
  emitter bug (2-binder pattern against the 3-element tuple a crossed tag contributes — E0308 in
  the generated crate at generator exit 0; fixed in `60ed8bb1`, record: burndown2 T2-23) was
  reachable at its parent through `inner = uint / null` + `t = #6.10(inner)` — the exact remedy
  the anonymous-choice-under-preserve refusal advertises — yet no committed fixture and no
  recombination composition spelled it, so `feature_corpus_roundtrips_nondefault_profiles` and the
  preserve layer-2 sweep were both blind to it. The cheap systematic layer: add the tagged-optional
  member kind (a tag head over a `T / null` collapse, spelled via the named-rule remedy) to the
  recombination ingredient table so the layer-2 preserve sweep compiles it per profile. Reopening
  signal, on the axis the cost grows along: a SECOND exit-0-uncompilable crate in a supported
  composition the member-kind table does not span (each instance is a loud build failure for
  whoever generates the shape, so the reporter exists by construction).

- **The referencing-context axis is measured one wrapping DEEP and at one honored base shape per
  directive; stacked wrappings and second base shapes are not.** The sweep that closed the
  directive-through-a-reference class — `src/tests/referencing_context_tests.rs`'s
  `directive_referencing_context_sweep` — crosses every `KNOWN_RULE_METADATA_TAGS` member with ten
  single-level wrapping contexts and found no drop at any of them (current state and design rules
  are in `tests/README.md`, in the third sibling block of the docs-contract sweep's
  description). What it does not reach is a reference TWO
  contexts deep — a tag head over a rule-body `.cbor` alias, a generic argument whose argument is
  itself a re-alias — and a directive's OTHER honored base shapes: the custom pair alone is honored
  on a scalar alias and on a self-carrying extern alias, and the sweep measures one per row. Both are
  crosses of axes that already exist rather than new mechanisms (`CONTEXTS × CONTEXTS` for the first,
  a second `BASES` row per directive for the second), and both are deferred on the same evidence:
  every fixed instance of this class was a REGISTRATION seam that all of a directive's base shapes
  reach and that one level of wrapping already exposes, so the product buys repetition of a seam the
  sweep already covers.
  Reopening signal for building either cross: another referencing-context drop fixed BY HAND — the
  same observable whose firing built the sweep, and one the sweep's own clean result puts at zero
  today, so the entry cannot already meet it.

- **The arm-position axis's classifier is a compile-time forcing function, and losing it is the
  regression to watch for.** The axis itself is now enumerated: `no_silent_directive` sweeps a
  directive on a NON-LAST arm as two shapes of its own — `multi_choice_non_last_arm` and
  `option_collapse_non_last_arm` — so every directive in `KNOWN_RULE_METADATA_TAGS` is measured
  there instead of one standing in for thirteen. The rejections stay pinned where they were:
  `dsl_position_tests`' `type-choice-non-last-arm` cell with `type-choice-non-last-arm-allowed`
  pinning the `@name`/`@doc` exclusion, `no_silent_directive`'s
  `type_choice_non_last_arm_used_as_key` and `option_collapse_non_last_arm_no_alias` hand cells with
  their rule-slot placement controls, and
  `robustness_tests::option_collapse_reads_rule_position_directives` for the vocabulary at the rule
  slot. What no sweep can assert is the EXCLUSION set — which directives a variant position
  legitimately consumes — because that is a statement about future fields, not about present ones:
  `comment_ast::RuleMetadata::non_variant_directives`'s exhaustive destructuring makes a new
  `RuleMetadata` field fail to compile until its author classifies it as rule-level or variant-legal,
  and its sibling `all_directives` (the never-spliced-group refusal's list) inherits that same
  forcing function by building on it. Replacing either destructuring with a hand list silently
  removes the property.
  Reopening signal for adding a runtime assertion of the exclusion set: a `RuleMetadata` field lands
  bound to `_` in that destructuring without a variant-position cell proving it is variant-legal.

- **The schema document's name injectivity is enforced ROW-side, so the residue is everything the row
  set cannot see.** The document is written by a program we emit, so its content is a property of the
  RUN rather than of the emitted source, and the row-side guard that closes every collision with a row
  on the losing side — the runtime `add_schema` helper's name ledger, kept-its-own-name check and
  inline-branch conflict check, firing in the consumer's own `cargo run` — is current state in
  `tests/README.md` § "JSON-schema document — Rust-side coverage (`run_test`'s per-fixture assertions
  + the emitted name guard)". Mint the one cell that section records as unminted — an extra
  `--json-schema-root` root on the LOSING side of a collision — if a consumer reports a collision they
  introduced through that flag. What remains — four holes, each needing its own mechanism:
  - **A collision whose LOSER has no row and whose `schema_id`s match** is a silent merge nothing can
    see: the ledger only holds rows, and the merge makes both returned refs equal the shared name, so
    the kept-its-own-name check reads clean. Reaching it needs a way to enumerate what a row pulls in
    transitively — a post-pass over the finished document comparing `$defs` cardinality against the
    reachable type set, or an upstream `schemars` setting that rejects two `TypeId`s resolving to one
    `schema_id`. The upstream lever is report-and-wait (`schemars` is a crates.io dependency, not a
    fork we pin like the `cddl` parser), so the document post-pass is the schedulable one.
  - **A cross-crate collision whose `schema_id`s MATCH** — two crates' `add_schemas` threaded into
    one `SchemaGenerator` — escapes the ledger, which belongs to the registrar one crate's
    `add_schemas` opens. The
    tool now EMITS that composition (`--json-schema-dep`) and DERIVES it from a config's crate
    graph, so this is no longer a hypothetical layout: what remains uncovered is only the ids-match
    merge, because dep calls are emitted FIRST
    and therefore an ids-DIFFER cross-crate collision hands the consumer's row `<name>2` and trips
    the kept-its-own-name check on the side whose owner can change it. That last step is a
    measurement rather than an inference, pinned by
    `config_tests::a_derived_thread_links_and_a_collision_blames_the_consumer` (what that cell builds
    and asserts: `tests/README.md` § "Config-file front end (`src/tests/config_tests.rs`)").
    Widening the ledger to span crates
    still means either a published ledger type in the static runtime (a new cross-crate API surface)
    or making `add_schemas` take the ledger, which would break the composition-point signature a
    consumer was told to call — so the ids-match merge is where a cross-crate report would have to
    land before either is worth doing.
  - **A `schema_name()` that `schemars` percent-encodes into its `$ref`** (anything outside
    `[A-Za-z0-9_]`, e.g. the static runtime's `OrderedHashMap<K, V>`) skips the kept-its-own-name
    check entirely: `schemars`' `encode_ref_name` lives in a private module (`mod encoding` in
    `schemars-1.2.1/src/lib.rs`, its items `pub fn` but unreachable), so reconstructing the expected
    ref means duplicating an upstream encoder — a false panic in a consumer's build is worse than a
    missed check. The closing mechanism is now half-built rather than hypothetical: the emitted
    closure check carries a `decode_schema_ref_name` (percent-decode, then the JSON-Pointer escapes),
    so the kept-its-own-name check could compare `decode(assigned)` against the name instead of
    skipping. What it still needs before it can be trusted to panic is a vector per escape class,
    since decoding a name the encoder spelled differently than assumed would fail a build that is
    fine: `tests/json-extern`'s hand-written `schema_name()` covers three of them (`~`, `/`, a
    percent-escaped byte — `tests/README.md` § "JSON-schema document — Rust-side coverage
    (`run_test`'s per-fixture assertions + the emitted name guard)"), leaving a multi-byte UTF-8 name.
  - **A collision between two types that BOTH lack rows** — each reached only transitively from some
    row'd type — is seen by neither check: the ledger holds rows, and "kept its own name" is asked
    only of a registered type. With distinct `schema_id`s both are emitted and one takes `<name>2`
    by first-encounter order, so an unrelated spec edit (a rule that sorts earlier, a reordered
    struct field) can silently SWAP which published TypeScript name belongs to which type. The
    byte-identity assertion cannot see it — it compares two runs of ONE spec, never two versions of
    a spec. Same closing mechanism as the no-row-loser hole above (a post-pass over the finished
    document); making the ordering irrelevant rather than merely loud would mean a published-name
    scheme that does not depend on traversal order at all — a breaking rename of published names in
    its own right, so it needs a consumer asking for name stability across spec edits. A cheaper
    *tell* is worth recording beside that post-pass, as a candidate rather than a commitment, because
    the post-pass is not implementable from inside the emitted crate at all (nothing there can
    enumerate the reachable type set): schemars assigns a collision suffix by walking `for i in 2..`
    over the names it has already used (`schemars-1.2.1/src/generate.rs`, the `find_ref` name
    assignment — verified at that pinned version, which `static/Cargo_json_gen.toml` is what pins),
    so a document holding both `<base>` and `<base>2` where no registered row publishes `<base>2` is
    the observable signature of the collision. It carries a real false-positive class — a genuine
    Rust type named `Foo2` — so it would need an opt-out, which is most of why it is not obviously
    worth shipping.

    **This hole is MEASURED, not hypothetical — by the consumer, in their corpus, not by us.** CML
    reported that the `AssetBundle`/`AssetBundle2` pair we predicted would trip the name ledger trips
    nothing: neither is a row. They are `$defs` entries reached through fields (`Value.multiasset`,
    `TransactionBody.mint`) from a hand-written generic substituted in by a `replace` block, and
    because that generic `#[derive(JsonSchema)]`s, the ids DIFFER — so both bodies are emitted and
    which instantiation keeps the bare name is traversal order. Milder than the ids-match merge
    recorded above (no wrong body; both types exist) and worse in one specific way: it silently
    renames a *published TypeScript type* on an unrelated spec edit, which for a published package is
    an API break.

    It is recorded rather than built for a reason that is now probed rather than assumed: a precise
    detector needs the schema_id -> assigned-name mapping, and `schemars::SchemaGenerator` exposes no
    such accessor — `definitions()` / `definitions_mut()` / `take_definitions()` all return only
    `Map<String, Value>` (read at `schemars-1.2.1/src/generate.rs`). From the finished document alone
    the `{base}{i}` rename is indistinguishable from a type genuinely named `Foo2`, so the
    `$defs`-shaped heuristic above would fire on legitimate corpora and train people to ignore it.
    The consumer's own committed-`.d.ts` CI diff is the precise detector for this class and is
    already the standing recommendation.

    What a consumer can do TODAY, and why that further lowers the priority: give each instantiation a
    row with `--json-schema-root` (`--json-schema-root=my_crate::AssetBundle<u64>` and `…<i64>` —
    generic arguments are inside the flag's accepted charset). Registering both makes them two rows
    publishing one `schema_name()` with different `std::any::type_name`s, which the ledger reports;
    registering only the instantiation that LOST the bare name makes the kept-its-own-name check
    report the `<name>2` it was assigned. So the row-side guard is opt-in-able for exactly the shape
    it cannot see on its own. (Reasoned from `static/json_schema_gen.rs`'s `add_schema` and `parse_json_schema_root`'s
    charset — not run.) Reopening signal for building anything more: a public schemars accessor for
    the assigned-name map, or a consumer reporting a rename their `.d.ts` diff did not catch.

  The document's reference CLOSURE is checked in two places — our own per-fixture assertion and the
  emitted `export_schemas()` walking the CONSUMER's document before it writes, both current state in
  `tests/README.md` § "JSON-schema document — Rust-side coverage (`run_test`'s per-fixture assertions
  + the emitted name guard)" — and neither closes the holes above. Two bounds worth keeping in view.
  The emitted check resolves the definitions map through the generator's
  `definitions_path` setting and SKIPS if that resolves to nothing, so a schemars default whose
  reference namespace stops matching the emitted document shape turns the check off rather than
  reddening every consumer build. It is silent IN A CONSUMER'S run, but not undetectable here: the
  emitter writes the `$defs` key literally while `run_test`'s own closure assertion matches
  `#/$defs/` literally too, so the same schemars change that trips the skip reddens our suite's
  assertion first, at local tier. What no layer covers is the window between an upstream release and
  our next local run — a vector pinning the emitted document against a bumped schemars is what would
  close that, and it is the same missing system as the dependency version-RANGE entry below. And
  closure is not injectivity: a document can resolve every reference and still publish one type's
  shape under another's name.

- **A directive's documented sentence can stay true while its consumer-visible MEANING changes
  underneath it, because the artifact it controls changed shape.** `@no_json_schema_export` shipped
  meaning "no registration row for this type", pinned at the row level by
  `snapshot_tests::json_gen_extern_schema_rows` and the accepted/rejected table in
  `robustness_tests::no_json_schema_export_misuse_rejects_gracefully`. Both pins still pass verbatim
  after the JSON surface became one document per crate — and yet the directive now means something
  materially different to a consumer: it drops the ROOT DECLARATION, not the definition, so a
  suppressed type that a published type references is now DECLARED in the shipped `.d.ts` where it
  was previously a dangling `TS2304`. Nothing could have caught that, because every pin was on the
  emitted intermediate and none was on the surface the directive's own documentation names ("not
  part of the published JSON-schema surface"). The gap generalizes past this directive and past
  JSON: a pin on generated source answers "did the emitter branch", never "does the shipped artifact
  say what the directive promises". Note this is a DIFFERENT class from the
  directive×rule-shape reachability product entry, whose verdict is {effect visible, loudly
  rejected} on a byte-compare of generated source — that sweep reads "effect visible" for this
  directive both before and after the change. Owned meanwhile by the end-to-end pin in
  `integration_tests::package_json_pipeline` (a `@no_json_schema_export` rule that a row'd type
  embeds, asserted `$defs`-present and `.d.ts`-declared), which exists precisely so the directive's
  promise is checked where a consumer reads it. Mechanical layer, on the second instance: for each
  directive that names a SHIPPED artifact in its documentation, require one pin on that artifact
  rather than on the emitted source — the directive inventory already exists
  (`KNOWN_RULE_METADATA_TAGS`, mirrored by `corpus_detect.ts`), so the addition is a per-directive
  datum naming its shipped surface, alongside the witness-profile datum the reachability sweep
  needs. Trigger: a second directive whose meaning shifts under an artifact-shape change, or a
  consumer reporting that a directive did not do what its documentation says while every gate stayed
  green.
  **Scope note, from a near-miss in a different flavor:** this entry and its mechanical layer are
  DIRECTIVE-scoped, and the same "documented promise nobody pins" class reaches CLI flags and the
  emitted runtime too, where no equivalent inventory exists to hang a per-item datum on. The
  near-miss (review-caught before shipping, so no instance is on record): the `--json-schema-export`
  docs stated that a document failing the closure check "never reaches `schemas/`", which is true of
  the failing document and false about the directory — `export_schemas()` creates `schemas/` before
  the check and, on failure, writes nothing, so an EARLIER export's document stays on disk and a
  pipeline ignoring the non-zero exit compiles it. Fixed in the prose rather than by a pin, because
  a filesystem-effect claim has no artifact inventory to enumerate. If a flag-level instance actually
  ships, the layer to build is the same shape one rung out: a per-flag datum naming the artifact its
  documentation makes claims about.
- **A citation that RESOLVES while naming the wrong thing is invisible to every citation lint, and
  the claim it makes is the half a human reads.** The `KNOWN_PANIC_CLASSES` form of this is closed:
  each entry pairs a panic-message substring with prose naming the fixture that owns the class, and
  `known_panic_classes_cite_fixtures_that_produce_them` (always-on, `local` tier) now generates each
  `tests/**/*.cddl` fixture an entry names and requires at least one to produce the entry's substring, so a mis-citation is a red test rather than a mislead during triage — the
  instance on record cited a fixture whose actual class was the other anonymous-composite BRACKET,
  which would have sent a reader to the wrong parse site (current state in `tests/README.md` §
  "Shape-recombination fuzzer"). What remains is the
  SECOND FORM of the same class, in a population that detector cannot reach — the cited thing
  is a GATE and the claim is that the gate is EVIDENCE for a property. Instance (read-caught in
  orchestrator review, never committed): a delivery report cited `check.ts fast` as proof that an
  `--emit-tests` map-key spelling was byte-identical, but the snapshot corpus holds zero `__i as …`
  key expressions, so no run of that tier could ever have witnessed the property either way. The
  class property is identical — the citation RESOLVES (the gate exists, it was green), so nothing
  that checks resolution can fire, and only the claim is false — but the population is prose in
  reports and handoff docs, which no gate scans, so the ledger detector above does not extend to
  it and this is NOT its second-instance trigger firing. What DOES transfer is the cheap
  instrument, which is the same shape as the detector: before citing a gate as evidence for
  emitted TEXT, grep that gate's corpus for the text. Which corpus covers what is a `tests/README.md`
  fact, and the emit-tests key-rendering answer now lives there (§ "Authoring standard for a
  bounded-domain emit-tests fixture"); the general rule is that a tier verdict evidences a property
  only where that tier's corpus contains an instance of it.
- **A matrix cell's `note` names the panic site it aborts at, and nothing re-derives that name from
  a run either.** Recorded as a second INSTANCE of the class above, in a different artifact, so a
  cross-artifact evaluation has both in one place. The `note` fields of the `…type2.map` containment
  cells (`cddl-matrix/containment/{array-element,cbor-payload,choice-member,generic-arg,map-value,
  occurrence-target}.toml`, mirrored verbatim into `matrix.json`) attribute the abort to
  `parsing.rs 'Anonymous groups not allowed'`. Every one of them actually reached the
  `"TODO: non-table types as types"` site — the same wrong-BRACKET confusion as the ledger instance,
  authored independently in a different file and a different language. The named site is now gone
  entirely (those cells reject gracefully) and the notes were rewritten with that re-grounding;
  what survives the rewrite is the gap, which is that a hand-authored site
  name inside an annotation is checked by nothing. Whether the two instances are ONE detectable class
  is the open question and the reason this is recorded rather than built: the ledger's detector keys
  on a Rust test's `catch_unwind` capture, while a cell note is TOML consumed by TypeScript, so a
  single detector spanning both is a claim to establish, not to assume.
- **Nothing asserts how MANY times a rejection message is emitted, and nested composites emit
  theirs twice.** `a = [x: [* 5]]` prints its refusal twice because the parse walk visits a nested
  composite twice; `a = [x: { uint => tstr }]` duplicates the same way, so the class predates the
  guards that surfaced it. Behaviour is correct and the exit code is right — only the reading is
  degraded, which is why the robustness/reject catalogs (they record an outcome LABEL, never the
  message) cannot see it. Meanwhile the working rule is that a rejection's text is reviewed by
  reading it once, at authoring time. The mechanical layer is a multiplicity assertion over the
  catalogs' captured stderr, paired with a `contains` de-dup in `record_rejection`. Trigger, on the
  axis the cost grows along — the repetition FACTOR, measurable by whoever next authors a fixture:
  a catalog fixture whose refusal repeats a message THREE or more times (a deeper nest multiplies
  it again), at which point a user cannot tell one refusal from several.
- **A containment note's stated REASON can go false without any gate noticing — the note carries a
  behavioural claim, and only its `spec` and (via the annotation) its support verdict are checked.**
  Proven instance, since fixed: six containment notes justified their two-field examples with "a
  single-field struct hits an orthogonal cddl-codegen panic", a panic that was gone — `m = { k: 5 }`
  and every other shape those notes were attached to generate exit 0, and
  `cddl-matrix/annotations/corpus/cddl_codegen.toml` separately recorded single-field struct maps as
  supported, so the note and the annotation had been contradicting each other in the same repo. The
  notes now state the file's two-field example CONVENTION instead of a causal claim about a defect,
  and the absence of the panic is pinned by `tests/corpus/single_field_map.cddl` — the shape a note
  must take when its reason would otherwise be an unowned behavioural claim. Nothing connected them
  and nothing does now: `project_corpus`'s note↔support check (E) reads the corpus nuance overlay's
  `[[note]]` entries, not containment `note` prose, and `verify.ts` corroborates only the `example`
  against `spec`. The class is bounded — a containment note is prose, and most of it (grammar
  anchors, sibling relationships) is not machine-checkable — so the meanwhile rule is narrower than
  a checker: a note states the SHAPE and its sibling relationships, and any behavioural claim it
  makes names the fixture or annotation that owns the claim, so the claim rots where a gate already
  looks. Trigger for the mechanical layer (a lint that flags behavioural vocabulary — "panics",
  "unsupported", "rejects" — in a containment note whose id carries no annotation asserting it): a
  SECOND note found stating a behaviour the annotations contradict, which the next author to reuse
  a note's reasoning measures at the moment they inherit it.
- **An `--emit-tests` gate that asserts only "the round-trip is green" cannot tell an INTENDED
  minted value from a wrong one that happens to land in-window.** Proven by the bounded-map-key
  minter: choosing a `nint` key base in VALUE space emitted that base as a u64 MAGNITUDE, so
  `{* nint .le -5 => uint}` minted magnitude 18446744073709551611 — wire value about -1.8e19, which
  SATISFIES `<= -5`, so the emitted round-trip passed. A false green, and the harder half to
  notice: the sign-mirror `{* nint .ge -5 => uint}` failed loudly on the identical wrong base, so a
  fixture carrying only ONE sign of the window would have certified the defect in whichever
  direction it was pointed. The conformance oracle (`--emit-tests-conformance`) cannot see this
  class either — the minted value is genuinely spec-VALID, merely degenerate — which is why it
  needs a layer of its own rather than a wider oracle. The standing rule that covers this today is
  IN FORCE and lives in `tests/README.md` § "Authoring standard for a bounded-domain emit-tests
  fixture" (both signs/endpoints of the window; the gate pins the minted SPELLING, compared
  whitespace-stripped, not just the round-trip verdict), with
  `tests/emit-tests-bounded-key/` as that fixture. What remains future here is only the mechanical
  layer: a mint-intent assertion — derive each bounded mint's expected
  literal from the same `MintValue` the renderer consumes and diff the two — so that no bounded
  mint can be wrong-but-passing. Trigger, on the axis the cost grows along: the COUNT of
  minted-value sites whose STORAGE space differs from the CDDL value space, measurable by whoever
  next adds a bounded mint. Today that count is one (the `N64` magnitude, transformed through
  `nint_bounds_to_u64`); a SECOND such site means the working rule is being carried by hand in more
  than one place, and hand-carrying is what produced this instance.
- **An emitter that DROPS an overloadable parameter by building a fresh config is invisible to the
  lint that catches one which hardcodes the default.** The hardcoding half is covered:
  `snapshot_tests::emitter_overload_no_bare_default_tokens` (fast tier, current state in
  `tests/README.md` § "Snapshot tests") fails any overload-scoped emitter fn whose emitted literal
  spells a bare `raw`/`serializer`, and its arming run cleared both axes — the four historical
  deserialize leaves stay fixed, and the one serializer leaf it found (`end_len`'s `is_end` branch,
  reachable only with the default name today because every overload site sets `is_end(false)`) was
  fixed in the same change. What that lint cannot see is the OTHER shape the same four leaves
  produced: a helper that builds a fresh `DeserializeConfig::new(..)` / `SerializeConfig::new(..)`
  instead of threading the caller's, so every literal below it correctly spells an accessor that
  resolves to the default. The emitted text is indistinguishable from correct; only the config's
  provenance is wrong. A mechanical layer would be a construction lint — a config built inside an
  overload-scoped fn must inherit rather than start fresh — with the legitimate fresh-config
  boundaries (the array element, the map key/value, the canonical map key) as its allowlist.
  Deferred because it would have no work to do on the current tree: all four of today's fresh-config
  constructions inside an overload-scoped fn sit at exactly those boundaries and each re-threads the
  overload explicitly (`elem_config.deserializer_name_overload = config.deserializer_name_overload;`
  and its two map twins; the canonical map key's own `buf`). Reopening signal, on the axis the cost
  grows along: the COUNT of fresh-config constructions inside overload-scoped fns, measurable by
  whoever next adds one — today it is four, all at the named boundaries, so a fifth that is NOT one
  of them is the instance that makes the boundary list worth writing. The lint's OTHER gap — a
  helper receiving the name under a parameter spelling the scoping rule does not know, which would
  put every literal below it out of scope — is closed rather than deferred:
  `snapshot_tests::emitter_overload_lint_scopes_every_name_param` (fast tier) scans the same emitter
  sources for name-typed `serializ` parameters and fails any spelled outside the rule's vocabulary,
  which the scoping rule and the guard now share as one const pair (current state in
  `tests/README.md` § "Snapshot tests").

- **A cargo FEATURE on a generated crate's dependency can change what emitted code MEANS, and no
  axis varies dependency features — compounded by every generated `Serialize` only ever being
  exercised through `serde_json`, the one serializer that cannot observe dishonesty in the serde
  data model.** Proven instance (consumer-reported, not any gate): under
  `serde_json/arbitrary_precision`, `serde_json::Number`'s `Serialize` emits a private
  `$serde_json::private::Number` token struct that only serde_json's own serializer collapses, so
  every `any`-carrying member's `to_json_value()` shipped a token object to JS at every magnitude —
  while `to_json()` looked perfect, because `to_json()` IS serde_json. Two independent blind spots
  produced it. Cargo unifies features across a build graph, so the feature arrives from a crate the
  generated manifest never names; the manifest merge even asserts it survives regeneration (a
  correct behaviour — the feature is legitimate), so the tool knowingly preserves a configuration
  nothing tested. And the assertion "the emitted `Serialize` is honest in the serde data model" is
  unobservable from serde_json: a second serializer is the only oracle that can see it, which is why
  the fixture takes `ciborium` (pure Rust, no C deps, no special case for the token) rather than
  adding another serde_json assertion. Owned meanwhile — one fixture, one feature, one alternate
  serializer, with liveness assertions in BOTH the rust and wasm crates so the feature-unification
  indirection cannot silently stop working and leave every other assertion in it vacuous — pinned by
  `json_arbitrary_precision`. Mechanical layer, when the trigger fires: a dependency-feature axis
  over the existing fixture corpus — for each generated-crate dependency, the set of its features
  that can change emitted-code semantics, generated once per (fixture × feature) cell and
  round-tripped through a non-serde_json serializer, which needs a per-dependency feature datum
  beside the manifest changeset that already knows every dependency the emitter references
  (`cargo_manifest.rs`'s `ops_for_*`). Building it now would be over-engineering: exactly one such
  feature is known to change semantics. Trigger: a consumer reporting a SECOND generated-crate
  behaviour that changes with a cargo feature they did not put in their own manifest — the party who
  already has the problem measures it as "my spec and my manifest are unchanged and my generated code
  behaves differently", which is what makes it reportable at all, and it lies on the axis the cost
  grows along, since the deferred layer's value scales with how many such features exist rather than
  with how many consumers hit the one we know about. A second semantics-changing feature is now
  known but is TOOL-OWNED and deliberate rather than unification-borne, so it does not fire this
  trigger: D1 ships `derivative` with `use_core`, which changes every emitted `#[derivative(…)]`
  expansion from `::std::` to `::core::` paths — landed only after probing the COMPLETE emitted
  attribute-form set (7 bodies × 3 derive lines) for equivalence on host and a no-std target.
  Recorded so the count of known semantics-changing dependency features reads two, not one, when
  this axis is next weighed.

- **A DOCUMENTED flag pairing can be broken while every fixture stays green, because a fixture
  satisfies the pairing's preconditions by accident.** Proven instance (found by reading the fixture,
  not by any gate; consumer-reported first): `command_line_flags.mdx`'s own `--extern-wasm-crate`
  example (`--common-import-override=cml_core --extern-wasm-crate=cml_core=cml_core_wasm`) panicked
  at startup validation for every PURE consumer — one whose override crate is not also a declared
  extern dep — from the `Int`-override feature's ship until the report, because `extern_deps_wasm`'s
  override crate doubles as a declared dep and so satisfied the key check by accident. The exact cell
  the doc example describes was never executed. Standing coverage now: `common_override_wasm_int`
  pins the pure-consumer cell. Working rule meanwhile: a flag pairing added to the user docs names
  the cell that executes it, and a cell whose fixture satisfies a precondition incidentally is not
  that cell. Mechanical layer, on a SECOND documented pairing found broken: a sweep executing each
  `command_line_flags.mdx` example invocation verbatim against a minimal spec that assumes only what
  the example's own prose states, asserting generation exits zero. It is the documented-VALID
  complement of the class-level validation-smoke sweep in § Declined's flag-powerset entry (which
  asserts documented-INVALID values reject), and the two should be built together if either is.
- **A documented failure SHAPE that no cell exercises is prose — the cross-crate `Int` channel has
  three, plus a hand mirror nothing cross-checks.** The shipped
  `Int`-under-`--common-import-override` coverage proves the positive paths and asserts row-ABSENCE
  on the degraded path, but none of the three failure shapes documented in
  `command_line_flags.mdx`'s `--common-import-override` section is compile-proven by a standing cell:
  (a) the consumer's `_borrowed_key_types_self_check` going RED when the common crate's `Int` lacks
  the demanded flavor (needs a de-flavored fixture variant); (b) the unresolved-import failure when
  the common crate exposes no `Int`/`IntError` (the existence contract's loud half); (c) `E0277` at
  the consumer's own map sites when the override is not a `--workspace-dep` (today only the no-row
  half is asserted, by `int_key_via_common_import_override_sidecar`). Related mirror exposure, to
  close alongside (a): `tests/extern-dep-crate`'s hand-written `Int` mirrors generated `Int`'s
  preserve-encodings wire format and encoding-insensitive key semantics, but `extern_deps` serializes
  AND deserializes through the fixture's own impls, so a generated-`Int` divergence passes silently —
  unlike the `tests/wasm-macro-crate` mirror, which compilation enforces. The cheap drift gate:
  cross-serialize one vector between a generated `Int` (any no-override cell's output) and the
  fixture's, asserting byte equality both ways. Reopening signal on the axis the cost grows along,
  measurable by the party who has the problem: a consumer hitting any of the three failure shapes and
  reporting that the error they got was not the one the docs describe — or a generated-`Int` wire
  change landing while `extern_deps` stays green.
- **`dsl_position_tests` is string-level by design, so a cell can be GREEN while the emitted crate is
  broken.** Proven instance (caught by a hand-run end-to-end repro during review, not by any gate):
  the `@custom_json` record-struct cell first landed passing while the generated struct kept its
  serde derives against a now-unskipped `encodings` field (E0277). The landed mitigation is paired
  anchors — the cell asserts the derive line AND the skip attribute, on both the directive and
  control sides — which covers known failure modes only. Working rule meanwhile: a cell whose
  expectation is compile-relevant asserts every attribute the compile outcome depends on, not just
  the one the directive names. Mechanical layer on a SECOND green-but-uncompilable cell: a
  `cargo check` probe over the compile-relevant cells only. Weigh it against the sweep's whole
  premise of cheapness (it runs in ~1 s today; a compile leg is nested-cargo-priced and would need
  the gate cache), which is why the probe is scoped to those cells rather than to the grid.
- **Nothing asserts that two IR sites cannot mint the SAME `RustIdent`: `register_rust_struct` ends
  in a bare map insert, so a second claimant silently overwrites the first.** Proven instance
  (reported by a consumer as a panic, root-caused here): a multi-arm group-choice arm registers its
  record under the arm's own name while parsing, so an arm named for an existing rule took that
  rule's slot — then, if the arm was embeddable, `remove_rust_struct` DELETED the rule outright and
  generation panicked at the first lookup of it. The louder half was not the panic: for a
  non-embeddable arm the overwrite STOOD, so the rule vanished from the emitted crate, references to
  it retargeted onto the arm's record, and two arms sharing a name emitted one struct carrying the
  other's fields AND the other's fixed tag — a wrong-wire-shape crate that compiled clean with no
  diagnostic. Which of the four outcomes fired was decided purely by topological rule order, so an
  unrelated reference edge added elsewhere in the spec flipped a working spec to a broken one.
  Standing coverage now: `arm_ident_collision` (order-independent — rule idents are all scope-marked
  before the parse loop, and two arms reject symmetrically), the structural-equivalence door that
  keeps benign same-shape name reuse generating, and the
  `group_choice_arm_ident_collision_rejects_gracefully` /
  `identical_group_choice_arms_in_different_rules_share_one_struct` /
  `embeddable_group_choice_arm_may_share_a_rule_name` trio. That detector is arm-specific by
  construction: the OTHER synthesizing sites (collection wrappers, generic instances, the
  `prelude_*` rules) mint into the same namespace and are guarded, if at all, by their own bespoke
  per-kind detectors. Working rule meanwhile: a change that mints a `RustIdent` from anything other
  than a rule name states what it collides with and how that is detected. The mechanical layer — a
  debug-time uniqueness assertion inside `register_rust_struct`, overwrite = panic against an
  allowlist of the legitimate re-registration seams (`set_rep_if_plain_group` and the `finalize`
  resolution passes rewrite in place by design) — should be armed as a DETECTOR first and its
  allowlist sized from that arming run, since "how many overwrites are legitimate today" is exactly
  the premise nobody has measured. Reopening signal, on the dimension the cost actually grows: a
  THIRD minting site needing its own bespoke collision detector (two are on record — this one and
  the wasm wrapper-name family), or any consumer reporting a declared rule absent from, or
  wrong-shaped in, its generated crate.

- **A message's LEVEL classification is review-owned, and two ways of getting it wrong are
  invisible.** The verbosity delivery classified 63 emission sites across six macros
  (`src/log.rs`), and nothing mechanical checks that any of them landed at the right level. Two
  distinct blind spots, one proven and one structural. **Proven once:** `err!` shipped with zero
  call sites and no tool could say so — `#[macro_export]` exempts a macro from `unused_macros`, so
  a level nobody emits at is indistinguishable from one that is merely rare. Caught by
  orchestrator review of the Phase 1 diff; fixed by converting `main.rs`'s terminal error path,
  which is where it belonged anyway. **Structural:** a warning-class message parked at `info` or
  above is silently invisible at the default level, and the only mechanical watcher covers a
  fraction of the surface — `cddl-matrix/no_silent_directive.ts` needs the `@`-directive notices to
  stay default-visible, but every other diagnostic (the `Recursive type:` notice, `export()`'s four
  warnings, the ~22 `--emit-tests` skip notices) has no such consumer. What keeps this a residual
  rather than a work item is that the one watcher fails SAFE: its FAIL condition is
  byte-identical-AND-unmentioned, so hiding a notice makes it fail loudly rather than pass
  vacuously — a suppressed acknowledgement cannot make that gate go quiet. Working rule meanwhile: a
  new emission site states its level and stream in the same review as its message, and the
  measurement to make is per-level line counts on a fixture that triggers the class (the delivery's
  own before/after used `tests/core/input.cddl`, which emits ZERO diagnostics — so it could not have
  shown a mis-streamed warning, and a second fixture with a recursive type and a `#6.258` set was
  needed to see the stderr half at all). The mechanical layer, when justified: a source-scanning
  test asserting no `info!`/`debug!`/`trace!` format string opens with `warning:`, plus a
  call-site-count floor per macro. Reopening signal, on the dimension the cost actually grows: a
  SECOND consumer depending on a specific message being default-visible (there is one today, and its
  fail-safe direction is what makes the count the right instrument rather than any measure of how
  many sites exist).

- **A `wac`-composed component transpiled by jco loses cross-instance resource identity, and the
  worse half of that is SILENT.** jco 1.26.1 allocates a separate handle table per component
  *instance* for the same resource type and emits no transfer between them for a JS-held handle, so
  on the dual-export world `component_compose` drives correctly through wasmtime: a dependency-minted
  handle passed into a consumer function throws `Resource error: Not a valid "…" resource` (scalar
  borrow and accumulator alike), and — the half that costs — a consumer getter returning a
  dependency-typed `own` handle resolves the index in the wrong table and returns a **different
  object**, with no error anywhere. A second, cheaper symptom sits on the same artifact: both
  packages' single interface is named `types`, so the root typings emit `export * as types` twice and
  fail `tsc --strict` with `TS2300`. It is a defect in the transpiler, not in what this tool emits —
  the identical artifact is correct through wasmtime, and a single generated crate with *two*
  interfaces transpiles and drives correctly, so the boundary is component instances rather than
  interfaces. Owned meanwhile by two things: the known-broken pin in
  `tests/component-jco/js/composed.test.mjs` (gate `component_jco`), whose failure messages each name
  the work a fix creates, and the consumer instruction in `docs/docs/component_differences.mdx`,
  cited by "Do not transpile a composed artifact", which prescribes per-component transpile plus
  `--map` instead. **Retirement observable: that leg FAILING** — a green composed leg means the
  defect is still there, and a red one is the good news. When it fires the work is one commit:
  replace the three known-broken assertions with positive ones, drop the hazard section, and
  re-point the packaging recipe if composing becomes the better shape. Read the gate's own log
  before concluding either way — the leg **loud-skips alone** when the ambient `wac` is absent or
  below 0.9, so a green run whose log does not name a `wac` version is silence, not evidence.

## Deferred features (build when a real consumer needs them)

- **A workspace-mode `wasm32-wasip2` build gate for the generated rust crate, once the `cdylib`
  crate-type question is ruled.** The generated `rust/Cargo.toml` declares
  `crate-type = ["cdylib", "rlib"]` for the wasm face, and building that incidental cdylib for
  `wasm32-wasip2` can SIGSEGV the sysroot's `wasm-component-ld`/LLD — spec-dependent and
  reproduced, not inferred (`tests/component-multifile`'s spec crashes it; `component-core` links
  clean). The user-facing account and the `["rlib"]` workaround live on the `--component` flag's
  documentation, and the component build gates sidestep it by building only the component
  package. What is deliberately not built is the decision the gate waits on — dropping or
  feature-gating the cdylib crate-type in the generated manifest under `--component` — because
  that changes existing output semantics for every consumer, wasm-face ones included, and is a
  maintainer call. Whichever way it is ruled, the landing carries this gate: a workspace-mode
  wasip2 build fixture, red-first against today's crash.
  - **Reopening signal:** a consumer reporting the linker SIGSEGV from their own workspace's
    wasip2 build — the flag doc names the exact failure signature, so the report arrives
    pre-diagnosed, and its existence is the evidence that the documented workaround is not being
    found or is not enough.

- **Probe the component face's JS surface on the axes the JS-host gate left untouched.** The
  `component_jco` gate drives one jco version (1.26.1) and one node version (22) over the default
  encoding posture, with no JSON doors, in node only — which is what its two reused fixtures carry.
  Four axes are therefore documented as unprobed in `docs/docs/component_differences.mdx`, cited by
  "Not probed on this face", each with the probe that would settle it: `--json-serde-derives`'
  `to-json`/`from-json` doors (generate one of the existing fixtures with the flag and drive both
  doors from node); the browser build of `@bytecodealliance/preview2-shim` (the same drivers under a
  headless browser); `--preserve-encodings`' `to-canonical-cbor-bytes` (the surface fixture at that
  posture, asserting byte-exactness across the JS boundary); and other jco/node versions (a second
  pinned lockfile, or a second node in the gate's provisioning preflight). Each is a
  fixture-and-lockfile change rather than new machinery, so what is deferred is coverage breadth and
  the cold-run cost that buys it — none of the four sits on the path the motivating consumer takes.
  - **Reopening signal:** a consumer reporting a JS-side failure on one of those axes — the party
    running the browser build, the preserve posture or a newer jco is the only one who can see it
    first, and the report names which axis to probe. The gate's exact pins are what make such a
    report actionable rather than ambiguous: a failure at a version the gate does not pin is a
    version finding, while a failure at 1.26.1 on node 22 is a regression the gate should already
    have caught.

- **Make the BOTH-set custom pair on a named struct rule mean "generated impls that delegate to
  the named functions" (symmetric delegation).** Today the record-rule both-set spelling
  suppresses the type's generated impls, rewrites embed-site deserializes to the named reader,
  and never references the named writer — accepted, pinned as-is by the `record-rule-both-set`
  control cell in `dsl_position_tests`, and documented as unspecified. The coherent end-state is
  thin generated `Serialize`/`Deserialize` impls delegating to the two named fns plus the missing
  serialize-side `Root(Rust(ident))` arm (the deserialize side already has one), applied to every
  named struct kind — then the pair means the same thing on a record as on an alias,
  `from_cbor_bytes` agrees with embed sites, and the opaque extern-interface projection stays
  sound. Deferred rather than shipped because it changes public behavior for a spelling with zero
  known users; the single-half spellings (which were outright broken) already reject.
  - **Reopening signal:** a consumer asks for a tool-generated struct with hand-owned wire logic
    without going full `_CDDL_CODEGEN_EXTERN_TYPE_` (losing the generated struct/API is the cost
    that makes them ask) — the request itself is the signal, and it names the type shape to
    design against.

- **Let a TABLE rule's own comment slot carry the custom pair.** The pair on
  `t = { * k => v }` is refused today (the `table-rule` cells in `dsl_position_tests`; the
  comment-DSL rejected-positions list), because a table lowers to a transparent map alias with no
  single codec slot to override: honoring it means wrapping the WHOLE map's wire form, a different
  contract from the per-key/per-value overrides that already work. Mechanically it means threading
  `rule_metadata` through `AliasInfo` — read by doc-lookup, the extern-interface projection and
  alias suppression, so each needs its own re-audit — which is why the refusal shipped first and
  names the three spellings that do work (key rule, value rule, `_CDDL_CODEGEN_EXTERN_TYPE_`).
  - **Reopening signal:** a consumer asks for a whole-map custom wire form that per-key/per-value
    pairs cannot express (e.g. a map spelled as something other than a CBOR map on the wire) —
    until then the per-position pairs cover the known shapes.

- **A crate entry that generates only a JSON-schema document (the aggregate package).** An npm
  package composing several generated crates needs one schema document over their union, but is
  often not itself a generated crate — so it carries a hand-written json-gen crate transcribing what
  this tool already emits for a `--json-schema-dep`-only run, registration-order reasoning and all.
  Measured, the emission side is already there: an input-less run (an empty `--input` directory)
  emits a `wasm/json-gen/` whose `add_schemas` holds the threaded calls in flag order and nothing
  else, and a fixture over two generated crates builds it and gets a document carrying a root from
  each. What blocks it is the SURPLUS such a run also writes — a vestigial rust crate that
  `static/manifest_changes/json_gen.toml` then makes the json-gen crate depend on
  (`dependencies.<lib> = { path = "../../rust" }`, an unconditional entry in an append-only log).
  Cargo auto-adopts that path dependency as a workspace member, so its `package.name` collides with
  the hand-written umbrella crate the aggregate exists to serve; renaming escapes the collision only
  by changing the document's title and filename, both published surface.
  What to build, in order: lift the `--json-schema-dep` / `--json-gen-dep` / `--json-schema-root`
  validations out of `with_types`, which an input-less run never enters; make the `../../rust`
  manifest op conditional. Only then land the aggregate path on top. The step this list used to
  open with is already done and is what makes the rest safe: `GenerationScope::export`'s write tail
  is one parameterised implementation (`generation::write_tail`), so the no-prior-output contract
  and its diagnostic-only reads have exactly one home and direct coverage
  (`src/tests/write_tail_tests.rs`). A second write loop beside it is still the shape to avoid — the
  snapshot corpus reaches `generated_strings`, not `export` — but an aggregate path now has a
  `WriteTailPlan` to fill in rather than a write loop to clone. Reopening signal (magnitude, consumer-side): the count
  of hand-written umbrella exporter files a consumer maintains rising above one, which is what a
  project publishing a second npm package over generated crates produces.

- **Finish the cross-flavor accommodation in the shared static runtime.** A runtime written by
  `--export-static-crate` is meant to serve crates at REDUCED flavors — the preludes carry
  deliberate `From<cbor_event::Len> for CBORReadLen` accommodations for exactly that, and the
  config file's `[runtime]` carrier derivation (`docs/docs/config_file.mdx` § "One shared runtime
  crate") is shaped by how far they reach. Two gaps stop them short of `--preserve-encodings`, both
  measured by exporting a runtime at one flavor and `cargo check`ing a consumer at another:
  (1) under `--preserve-encodings` the exported `non_empty_map.rs` substitutes `OrderedHashMap` for
  the inner `BTreeMap`, so a non-preserve consumer whose spec holds a `{+ K => V}` emits
  `NonEmptyMap::try_from(<BTreeMap>)` against a runtime implementing only
  `TryFrom<OrderedHashMap>` (E0277); (2) under `--canonical-form` the runtime's `AnyCbor` implements
  only the two-argument `Serialize`, so a non-preserve consumer using CDDL `any` finds no
  one-argument `serialize` (E0599). What to build: a `TryFrom<BTreeMap>` bridge on the
  preserve-flavored `NonEmptyMap` and a non-preserve `AnyCbor::serialize` shim under canonical, each
  pinned as a leg of the existing cross-flavor compile gate
  (`config_tests::a_runtime_table_exports_a_runtime_the_other_flavor_compiles_against`, whose
  mutation leg asserts the first gap is currently real and is the assertion to invert). Closing both
  makes `preserve-encodings` a genuine MAXIMUM axis, so a CML-shaped config — one full-flavor crate
  and one reduced — derives its carrier with no `flavor-from` declaration at all. `canonical-form`
  stays an equality axis regardless: the `fit_sz` / `to_len_sz` / `SerializeEmbeddedGroup` arity
  differences are a different calling convention, not a missing accommodation. Reopening signal
  (magnitude, consumer-side — the cost grows WITHIN one consumer as its reduced-flavor crates'
  specs grow, not across consumers): a reduced-flavor crate in a shared-runtime workspace acquires
  a `{+ K => V}` or an `any` in its spec, which is the point at which a `flavor-from` declaration
  that compiles today stops compiling.
- **Consumer-side auto-deferral of reject-set wrappers (`--wrapper-requests`).** The dep-side
  hosting leg is complete: a `@duplicates reject` array shape in a committed
  `borrowed_collections.rs` sidecar round-trips the request grammar and emits the
  `OrderedSet`/`NonEmptyOrderedSet` twin wrapper in the dep
  (`workspace_requests_hosts_reject_ordered_set_twins`). What is NOT built: a CONSUMER's own
  generation writing a reject shape into its request sidecar automatically
  (`generate_reject_ordered_set_type` does not call `try_defer_wrapper` — threading it needs a
  signature change across three call sites inside the deferral-placement logic, which is delicate
  enough that it warrants its own reviewed change rather than riding a doc-and-closure commit).
  Consequence and why it is safe to defer: a consumer that hits the shape emits the wrapper
  locally, which is at worst the documented cross-crate duplicate-symbol link class — loud at
  link time, never a silent data-behavior skew. Reopening signal: a real multi-crate consumer
  puts a reject set behind a wrapper-request boundary (hand-authoring the sidecar row is the
  interim workaround). Note the well-known-tag registry
  (`parsing::well_known_tag_default_duplicates`) WIDENS who hits this: every no-directive
  tag-258 set is now the reject shape, not just rules with an explicit `; @duplicates reject` —
  same loud failure class, larger audience, so the reopening signal fires sooner.
- **Extern-interface export dialect v2 candidates.** Each bumps the seam header
  (`_CDDL_CODEGEN_EXTERN_INTERFACE_ v1` — unknown versions hard-error, pinned by
  `extern_import_unknown_version_hard_errors`), so batch them when one gets a real consumer:
  (1) per-variant name pins for c-style enums — the type-level `@rust_name` pin covers the enum
  name but variant names stay consumer-derived, so a variant-naming-rule change reintroduces the
  cross-version skew class on exactly that row (reopening signal: a consumer compile break naming
  a dep enum variant after a codegen naming change); (2) synthesized idents for anonymous generic
  instances — `[* pair<uint,tstr>]` in a dep today closure-excludes the referencing rule
  (reopening signal: a real dep's export carries that `; unexported:` record and a consumer needs
  the rule); (3) transparent spellings for the renderer's known lossy-IR case — a two-sided
  `float32` window has no faithful CDDL form (a literal float range re-parses as `float64`), so
  such rules exclude-with-record (reopening signal: same, for a float32-windowed alias);
  (4) generic PARAMETERS on an exported extern base — the renderer spells every rule body as a bare
  marker, so `ext_set<t0> = _CDDL_CODEGEN_EXTERN_TYPE_` projects param-less and a consumer can
  reference the base but never instantiate it. That is why `@raw_bytes_flavor` does not ride the
  seam: it flavors a generic INSTANCE, so on the param-less form it is unhonorable, and re-parsing
  it is exactly the spelling the non-generic-extern rejection refuses (pinned by
  `extern_import_flavored_generic_base_projects_without_the_tag`). Carrying the params would restore
  both halves at once. Reopening signal: a consumer that needs to instantiate a dependency's generic
  extern over `--extern-import` — its own spec cannot spell the instantiation, so the block is
  visible to the party that has it, not inferred by us.
- **CDDL module directives (draft-ietf-cbor-cddl-modules) with the draft's real inlining
  semantics, and `as`-namespacing.** Both forms are currently recognized and refused loudly
  (`module_directive_import_aborts` / `module_directive_include_aborts`;
  `dotted_rule_name_rejects_gracefully` for cddlc `as`-expansion output) — deliberate: the extern
  channel shipped on the dep-side export instead (rationale: the dep's extern surface is
  `f(spec, DSL, flags, tool version)`, which spec-text resolution cannot see; and the draft's
  `;# import` means inline-and-generate, so extern semantics would contradict it — keep the `;#`
  namespace clean for a real implementation). Reopening signals: a consumer needs third-party
  module consumption (RFC extracts) or cross-module name reuse; the latter is the deep one
  (scope-qualified `RustIdent` through the IR, dep_graph ordering, emitted-test glob imports,
  JSON schema naming).
- **no_std generated crates.** cbor_event supports no_std since 3.0.0, so a CLI flag (`cli.rs` +
  docs) emitting no_std-compatible crates is possible; the static runtime and generated code would
  need their own std-usage sweep (collections, error impls). The mechanical layer when a consumer
  asks: a no_std cross-compile gate over one generated crate.
- **Dependency version-RANGE resolution is untested: generated crates float on semver
  `cbor_event = "3.3.0"`, and nothing gates what that range actually resolves to over time.** In
  check.ts runs the nested-cargo cells now resolve OFFLINE from the cargo cache (the
  registry-transient watch below), so the float enters one step removed — through the warm-up
  fetch, which pulls the newest semver-compatible releases into the cache — but the exposure is
  the same: a new upstream release changes what every nested-cargo cell — and every downstream
  consumer — builds against, with no gate noticing until something breaks. The
  3.2.0 upgrade itself arrived as exactly such a version event (the 2.4.0-era prediction this
  entry re-captures), absorbed deliberately with flip vectors; an UNPLANNED 3.x release would be
  absorbed silently. The mechanical layer when a release actually bites: a pinned-latest or
  `--minimal-versions`-style resolve check over one generated crate, red when the resolved
  `cbor_event` version drifts from the one the vectors were blessed against.
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
  families of the residual entry above), deserialize collect-then-`try_into`.
  Tests-first encoding: the table reject rows flip to enforce-green through the same promotion
  flow `plus_table` took (over-acceptance vector → `class="constraint"` reason-asserted
  rejection); arrays gain decode vectors pinning boundary counts (accept at min/max,
  reason-asserted reject at min−1/max+1) plus wasm-ABI matrix shapes. Matrix-side row-flip
  detail — including the `{ k => v }`-as-bounds-`(1, 1)` revisit — lives in the two
  candidate-feature entries in `cddl-matrix/ROADMAP.md` (the "Real bounded `?` / `n*m` table
  cardinality" entry and its two-type sibling).
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
  too, pinned by `top_level_ranges` in `tests/core`. (Separately, `foo = undefined` is refused
  gracefully — a distinct gap, unsupported cddl-prelude `#7.23` with no `FixedValue`, not the
  `Fixed`-member path; pinned by `undefined_prelude_rejects_gracefully_in_every_position` and
  `tests/matrix_reject/prelude.undefined.cddl`, with the representation deferral ledgered in
  `cddl-matrix/ROADMAP.md` § findings.)
- **wasm write-side present-null construction** *(unrequested)*. The read-side three-state
  fidelity gap is closed (presence accessors `has_<field>()` / map `has(key)`; oracle:
  `tests/nullable-wasm/`; read protocols in `docs/docs/wasm_differences.mdx`). The remaining
  asymmetry is on the WRITE side: wasm setters/constructors always wrap the argument in an outer
  `Some`, so a JS caller can produce absent and present-value but not present-null. Revisit only
  when a consumer asks.
- **Transparent tag-set idiom — recognized-shape boundary (REQUEST-08).** The collapse of a two-arm
  tagged-or-untagged collection choice into one transparent optionally-tagged alias
  (user doc: `docs/docs/current_capacities.mdx` § "Transparent tag-set idiom") is narrow by design;
  recognition is pinned by `optional_tag_set_tests`. Its full test map — the tag-set corpus
  fixtures, the `opt_set` golden wire vectors, and the in-process recognition/parity tests — lives
  in `tests/README.md` § "Transparent tag-set idiom". The boundary shapes below stay unsupported,
  each with its reopening signal (a real consumer spec hitting it):
  - *Non-idiom choice-BODIED generic defs are refused, not supported.* The idiom is the ONLY
    choice-bodied generic def the generator can monomorphize — the collapse fires at parse time,
    before the generic machinery, and produces one struct to substitute into. Arms that do not
    satisfy the collapse condition (e.g. mismatched bounds `xs<a0> = #6.258([+ a0]) / [* a0]`) would
    mint a union enum the machinery has no way to thread parameters through, so the definition is
    rejected at parse naming the idiom (`robustness_tests::unsupported_generic_def_bodies_reject_
    gracefully`). Remedy when the refusal bites: teach the generic machinery to substitute into an
    enum's arms, which is the same work a generic GROUP-choice def needs.
  - *Inline/anonymous two-arm choices are not recognized.* Recognition lives at the
    `parse_type_choices` named-rule seam, so an inline `[x: #6.258([* uint]) / [* uint]]` stays a
    two-variant enum. Remedy when it bites: run the recognition on anonymous choices too.
- **Opt-in RFC 8949 §6.1-faithful total CBOR→JSON rendering (a `to_json_rfc8949`-style lossy
  flavor).** §6.1 is self-described "non-normative advice", and its non-injective rows are lossy
  by its own admission: byte strings become base64url JSON strings indistinguishable from text,
  tag numbers are ignored, non-finite floats and unknown simples become "a single substitute
  value, such as a JSON null", and its integer-key stringification carries an acknowledged
  "danger of key collision". The crate's `any` JSON surfaces refuse silent substitution instead —
  the tagged `AnyCbor` codec (serde's externally-tagged enum form,
  `docs/docs/output_format.mdx` § "JSON representation") is total but structural, and the
  natural-JSON surface (the open-struct-maps workstream) implements §6.1's injective subset and
  **errors** on the substitute-value rows — because generated JSON feeds a symmetric `from_json`
  and typed consumers, where a silent substitute is write-back corruption (`NaN` → `null` →
  a different value re-encoded; bytes → string → re-read as text), not display fuzz. A
  never-fails *natural* view is still legitimately useful for display/diagnostic consumers; it
  must simply be opt-in by name so totality-by-substitution is a visible choice. Mechanical
  layer when a consumer asks: the same recursive walk with §6.1's leaf policy, a never-fails
  property over the `AnyCbor` fuzz corpus, and vectors pinning each substitution row. Reopening
  signal: a real consumer needs a total natural-JSON view and neither the tagged codec nor the
  fallible natural `to_json` fits.
- **EDN text-notation round-trip (`to_edn` / `from_edn`, draft-ietf-cbor-edn-literals).** EDN is
  the text notation that *can* be byte-faithful where JSON structurally cannot (encoding
  indicators, NaN payload bits — JSON's number model collapses both), making it the natural
  third channel: JSON as the value-level lossy view by charter, CBOR as the authoritative
  bytes, EDN as the human-readable byte-faithful text form. The maintainer's `cbor_event_edn`
  library already handles the hard edge cases (NaN payloads included) and is the natural base.
  Deferred because the EDN spec is an evolving IETF draft — a generated-crate surface built
  against it now risks churn with every draft revision. Reopening signal: the draft stabilizes
  (late-stage or RFC) or a real consumer needs byte-faithful text round-trips sooner.
  Mechanical layer: a property over the `AnyCbor` fuzz corpus asserting
  `from_edn(to_edn(x))` byte-identity against the existing span oracle
  (`src/tests/any_cbor_tests.rs`).
- **Run the TS-projection leg over every `--json-schema-export` fixture, not one.**
  `assert_schema_projects_to_legal_ts` type-checks the document a fixture's json-gen crate actually
  wrote, and `open_struct_map_json_e2e` is the only caller — chosen because its rest rows are where
  a schema that is exactly right still projects to TypeScript that does not compile. Every other
  `--json-schema-export` fixture is covered for its schema and not for its projection, so a shape
  none of those rows has (a `oneOf` a consumer's `tsc` chokes on, a `$ref` cycle json2ts unrolls) is
  invisible until a consumer's npm build finds it. Deferred on cost, not value: the leg is an `npm
  install` plus a `tsc` run per fixture, and `run_test` has 27 call sites passing that flag. The
  cheap build when it is worth it is a work dir whose `node_modules` is installed once and shared
  (under `acquire_scratch_lock`, since `cargo test` runs the fixtures in parallel), leaving each
  fixture a node + `tsc` run of a few seconds. Reopening signal: a second projection defect reaches a
  consumer through a fixture the leg does not run on — or the flag's call-site count grows past
  roughly forty, at which point the shared-install build is cheaper than deciding case by case which
  fixture deserves the leg.
- **Type-check the MERGED `.d.ts` with `tsc --noEmit`.** The type-checker oracle covers the json2ts
  output alone (`js_schema_to_ts`, and `assert_schema_projects_to_legal_ts` over real emitted
  documents); the merged file — wasm-pack's bindings with the JSON interfaces
  appended and each `to_json_value()` specialized, produced by `js_d_ts_merge` and
  `package_json_pipeline` — is still asserted on *substrings* of the emitted TypeScript, so it can
  only catch the wrongness it was told to look for. A real type-checker over the merged file is the
  oracle that catches the class as a class: a specialized method naming a type the append step never
  wrote, a declaration duplicated between the two halves, a malformed union. Deferred on cost, not
  on value: it adds TypeScript's install weight to `package_json_pipeline`, already the heaviest
  JS-side gate. Two traps to build it with, so the follow-up does not rediscover them: (1)
  `--skipLibCheck` makes the check VACUOUS here, because the file under test *is* a `.d.ts` — the
  flag must stay off, which also means every type the file references must resolve; and (2)
  TypeScript must be ≥ 5.2 with `--target esnext`, because wasm-bindgen emits `Symbol.dispose`
  members that earlier targets do not know — a trap specific to this half, since the json2ts output
  carries no wasm-bindgen surface. Reopening signal: a shipped merged `.d.ts` breaks a consumer's
  build in a way the substring asserts could not see.
- **Propagating a deliberately-unpublished type's intent to the JSON → TS scripts.** A type whose
  CDDL rule carries `@no_json_schema_export` still mints a wasm class with `to_json_value(): any`
  (the directive removes the registration row, not the derives), so when nothing published
  references it either, `json-ts-types.js` fails on it and the consumer restates the class by hand
  in `--allow-untyped`. The tool cannot compute that set at GENERATION time, and the reason is the
  design constraint to build against: whether a rowless type lands in `$defs` is decided by
  `schemars` at json-gen *runtime*, and a hand-written `JsonSchema` impl can introduce references
  the IR cannot see — so any generation-time list is an approximation that goes stale in both
  directions, against a stale-entry check that is a hard error by design. The mechanism worth
  building instead reports from where BOTH facts are known: have the json-gen crate emit, at
  `export_schemas()` time, which deliberately-unrowed types ended up absent from the finished
  document, and have `json-ts-types.js` consume that instead of a hand-maintained flag value. Note
  the shapes it must cover and the ones it must not: a record, a type or group choice, and a
  `@newtype` wrapper mint a class declaring the JSON method; an `_CDDL_CODEGEN_EXTERN_TYPE_`, a
  collection wrapper and a c-style enum do not. Reopening signal: a consumer whose
  deliberately-unpublished set is large enough that restating it on the script command line is a
  maintenance burden.
- **Guarantee `<$defs key>JSON` is the emitted declaration name.** `run-json2ts.js` sets each
  definition's `title` to `<key>JSON`, but `json-schema-to-typescript` normalizes titles into
  identifiers, so a key that is not a fixed point of that normalization is published under a name
  `json-ts-types.js` cannot key on (`Blake2b256` → `Blake2B256JSON`) and its class reads as untyped.
  The constraint that shapes any fix: post-compile identifier renaming is not available, because it
  also rewrites matching words inside doc comments and string-literal unions — which is exactly why
  the script sets titles BEFORE compiling. The candidate design worth recording removes
  normalization from the contract rather than detecting it: compile with synthetic per-definition
  titles that are provably normalization fixed points and provably absent from the source document,
  then map them back to `<key>JSON` in the emitted text. What exists meanwhile is diagnosis only —
  `json-ts-types.js`'s failure compares modulo normalization and names the declaration that does
  exist (pinned by `integration_tests::js_d_ts_merge`'s case 2), so the consumer is not told to
  publish a type they already published; the class is still untyped in the shipped `.d.ts`, and no
  `--allow-untyped` entry fixes that. Reopening signal: a consumer's type name is not a fixed point
  of the normalization and its class does expose JSON methods.
- **json2ts artifact doc comments naming `undefined`.** The emitted `.d.ts` carries comments like
  "This interface was referenced by `undefined`'s JSON-Schema definition via the `patternProperty`
  …" — `undefined` because the parent definition's title is managed by `run-json2ts.js` rather than
  present in the source document. Purely cosmetic, and pre-existing. No reopening signal is needed:
  fold it into whichever change next touches that script's output, where it is nearly free.
- **A file listing `--json-schema-root` values, instead of one flag per root.** The repeatable flag
  is what shipped, on the grounds that it matches every other repeatable flag and that the asking
  consumer's eight entries do not justify a new file format. A file-listing variant stays purely
  additive (same emission, a different way to spell the list), so it is a cost question a real
  consumer settles: build it when someone's root list is large enough that a command line stops being
  the right place for it. Note what such a file must NOT become — the paths are Rust, not CDDL, and
  routing them through the spec is the category error the flag exists to avoid.
- **Assert a hand-authored JSON schema against the type's actual serialization.** Supplying the
  schema body needs neither new spec syntax nor a hand-written impl: `@custom_json` suppresses the
  `serde`/`schemars` derives while the type still gets a registration row, and `custom_schema_impl!`
  writes the `JsonSchema` impl around a JSON file, so an authored body lands in the document as an
  ordinary local `$defs` entry through the normal route (`docs/docs/comment_dsl.mdx` § Writing the
  `JsonSchema` impl the directive promises). The missing half is the one with the value: a
  hand-written schema drifts from the serialization it claims to describe and nothing notices — the
  reported consumer failure is a hand-maintained schema that had been wrong in two places for years.
  The assertion SHAPE already exists, hand-authored per fixture: `tests/json/tests.rs`'s
  `schemas_validate_serialization` validates a concrete value of every exported type against
  `schema_for!(T)`, with `schemas_reject_wrong_shapes` as the over-permissive counterpart (a
  degenerate always-true schema passes every positive check). What it needs in order to generalize
  into a consumer's crate is per-type sample values MINTED rather than hand-listed — the
  `--emit-tests` IR-derived value minting is the candidate. A spec-level `@json_schema = "<file>"`
  spelling is the cosmetic half and should not ship first: without the assertion it relocates the
  hazard instead of removing it. Reopening signal: the sample-value minting gets reused for this, or
  a consumer reports a shipped schema that contradicted its own serialization.
- **Composing registration rows across generation passes that share one output crate.** Two
  cddl-codegen passes targeting one output directory each own the json-gen crate's row set
  outright, so there is no supported way to have one json-gen crate publish both passes' types.
  Lowest value of the JSON-schema asks by the asking consumer's own ranking: a committed-`.d.ts`
  CI diff covers the requirement they actually have, and the emitted `add_schemas` is already `pub`,
  so a hand-written `export_schemas()` in the crate root can call both passes' registrars today.
  (Scope: this records the ask as understood from the delivery's response notes; the requester's own
  statement of it is not in-tree.) Reopening signal: a consumer whose layout genuinely forces two
  passes into one json-gen crate and for whom the hand-written composition is not enough.
- **A machine-readable ownership record for the co-owned `features.std` list.** The human half of
  the co-ownership contract is asserted in-band — the `# cddl-codegen:` comment block above the key
  (`STD_OWNERSHIP_COMMENT`, `cargo_manifest.rs`), re-written every run so it cannot rot — but a
  TOOL wanting to partition the list's entries by provenance (tool-computed vs consumer-added) has
  only line-prefix parsing to go on. The structural carrier already exists: a list under the
  manifest's `[package.metadata.cddl-codegen]` table (the `generated-with` stamp's home) would
  state the tool's current entry set with no new public surface and stay freely revocable. Not
  built because it has zero consumers today, and duplicating the list without one buys only bytes.
  Reopening signal (consumer-side, the party who'd measure it): a consumer-side tool or lint that
  needs the tool/hand partition of `std` entries and says so — a feature request naming the tooling
  it would unblock.

## Operational watches

- **A stale-pin guard cannot distinguish "the gap closed" from "the vector that proved the gap went
  blunt" — and a wholesale re-mint of RANDOMLY GENERATED vectors produces the second while reporting
  the first.** Proven 2026-07-30 (native-float preserve delivery): adding one corpus rule required a
  catalog row, and `verify.ts --mint-decode-corpus` was run in its FULL-refresh form, which re-rolled
  every row's ruby-generated vectors (3817-line diff for one new row). `corpus_decode_replay` then
  failed with `ENCODING_VARIANT_SKIP names (table_preserve.md, reverse_maps) … the gap closed —
  remove the entry (stale pin)`. It had not: that pin exists because reversing a `@duplicates
  preserve` pair-map's entry order changes the VALUE, and the re-rolled vector set had replaced
  HEAD's multi-entry maps (`8200a2…`, `8200a3…`) with the empty map `8200a0`, which reverses to
  itself. Taking the guard at its word would have deleted a live order-collapse tripwire AND left the
  coverage silently gone — the guard's message names exactly one of its two possible causes, and it
  names the wrong one whenever the data moved. The mint's own `--only=<id|fixture>` form is the fix
  and the working rule: **re-mint the row you changed, never the catalog** (108-line diff, other rows'
  vectors preserved verbatim, `table_preserve.md` still carrying multi-entry maps). The mechanical
  layer on a second instance: have the mint refuse a full refresh that REDUCES any row's vector-shape
  diversity (it already classifies vectors by shape — `vectorShapeClass` — so an empty-map-only row
  where the committed one had multi-entry maps is detectable at mint time, before the pin ever goes
  stale). Trigger: a second pin retired or nearly-retired by vector churn rather than by a fix. (The
  order-sensitivity pins that produced the proven instance are gone — those exemptions are now
  derived from each row's `spec` and carry no stale guard, precisely because "this vector's
  reordering was the identity" is not evidence about the row. Every remaining stale-guarded pin is
  still exposed to the class, which is why the watch stands.)

- **A migration handoff's "complete list found by survey" is a NEGATIVE premise, and the first
  consumer falsified one twice.** The no_std handoff's hand-action survey ended "Nothing else,"
  and CML's executed migration found two more required classes (public struct fields typed
  `LinkedHashMap<…>`; `Entry::or_default()` call sites) plus a three-manifests-wider stale-dep
  sweep. Both misses are instances of standing AGENTS.md rules, recorded here because they
  happened in a SURVEY DOC rather than in code: the field-annotation half was surveyed with a
  grep truncated through `| head -8` and presented as complete (the evidence-lost-to-tail class —
  the pipe ate the `witness_builder.rs` hits), and `or_default` was simply not in the grep
  vocabulary, so absence-of-hits read as absence-of-thing (the vocabulary-bounded-negative
  class — a survey's completeness is established by enumerating the CATEGORY registry, not by
  the searches one thought of). The systematic fix is committed where the next consumer will
  meet it: the four-category migration-search recipe in `docs/docs/output_format.mdx`
  § "Upgrading a crate generated before the `no_std` output" (including resolve-`.entry(`-by-type,
  since the inherent method shadows `Deref` and no text search settles the receiver). Watch, not
  work item: the next handoff doc's survey section should cite that recipe as its method — a
  survey that instead presents its own grep list as complete is this entry recurring.

- **`lint_doc_citations` crashes rather than reports when a tracked file is deleted but unstaged.**
  It walks `git ls-files`, which lists a path git still tracks even after the working-tree file is
  gone, and `readText` is typed `string | null` as though absence were handled while `readFileSync`
  throws — so the gate dies with a bun stack trace and an ENOENT path instead of a citation verdict.
  Seen while re-projecting a matrix re-grounding: `project_robustness.ts` moved fifteen fixtures
  between `tests/matrix_*` directories, and the gate crashed until the moves were staged. Diagnosis
  only, deliberately not fixed: the recovery is `git add`, the gate is correct once the tree is
  staged, and it never misreports a verdict — it fails to produce one. The cost is triage time for
  whoever meets it mid-re-projection and reads an ENOENT on a fixture path as evidence that the
  re-projection deleted something it should not have. Reopening signal: a second run where the crash
  is read as a projection defect rather than an unstaged tree, or any use of this gate somewhere a
  dirty tree is normal (a pre-commit hook), where the crash would be the common case rather than the
  exception.
  Second sighting 2026-08-01 (the prelude fixture moves, `matrix_panic` → `matrix_reject`):
  correctly diagnosed at the cost of triage time, so the signal — the crash being READ AS a
  projection defect — has still not fired; the sighting is recorded because the burndown's
  catalog-flipping deliveries now produce cited-fixture moves routinely, which is the dimension
  the triage cost grows along. Same sighting added one observation the original entry lacks:
  the citation grammar also rejects GLOB spellings (`prelude.eb*.cddl`), which is why moved-
  fixture prose cites one fixture "and its siblings" — decide globs on merit at the same touch
  if the crash is ever promoted to a fix.

- **A shared `CARGO_TARGET_DIR` across same-named scratch crates masks compile failures as
  cached passes.** Proven 2026-08-01 (the same-chain `.cbor` refusal delivery): two scratch
  probes generated crates with the same package name into different directories and
  `cargo check`ed them under one shared target dir; the second check reused the first's
  fingerprint and reported clean for a crate that does not compile — the probe's verdict was
  the CACHE's, not the crate's. The gate-cache machinery is not implicated (it keys on
  generated-crate content hashes); this is raw cargo fingerprint reuse, and it can flip a
  red-first probe green. Working rule: compile probes of scratch crates use a per-crate target
  dir (or distinct package names); a probe that must share a target dir for warm-cache speed
  asserts something the cache cannot fake (`cargo clean -p <name>` first, or check the error
  output of a build it forced). Watch, not work item: the next false-green scratch probe traced
  to fingerprint reuse is the second instance; if one appears inside a REGISTERED gate rather
  than an ad-hoc probe, that gate's cell keying is the defect and it graduates to a work item.

- **The config run's convergence warning can name a crate that already converged.** `Convergence`
  snapshots each consumed sidecar's bytes at the START of the run and compares them after, so any
  sidecar that CHANGED during the run marks its reader stale. That is right for the edge the check
  exists for (a dependency reads its consumer's committed sidecar, and the consumer generates
  later), and wrong for a hand-written `[crates.<n>.wrapper-requests]` / `.key-requests` entry
  pointing at a crate that sorts EARLIER: the owner rewrote the sidecar before the reader read it,
  so the reader saw fresh bytes and is not a run behind, but the before/after comparison cannot
  tell the two apart. Reasoned from the code, not reproduced; strictly diagnostic (stderr text,
  exit status and generated bytes unaffected), and self-clearing — the redundant re-run it asks for
  converges and the second run is silent. The fix, if it is ever worth one, is to snapshot each
  sidecar when its READER reaches it rather than at run start, which needs the capture to
  interleave with the generation loop instead of bracketing it. Reopening signal: the warning names
  the same crate on two consecutive runs of an unchanged config — a false positive that does not
  clear is a different defect from this one, and the person seeing it can measure it without
  reading any of this. The blast radius stays confined to the warning: the committed-state verdict
  that shares the run's exit path (`Config::committed_verdict`) reads the tree rather than
  bracketing the run, so it has no before/after comparison for this to reach, and a spurious warning
  cannot become a spurious build failure.

- **The extern-interface export is a public interchange format.** Once a consumer regenerates
  against a dep's committed `extern-interface/<dep>/**`, its dialect (header line, marker rows,
  `@rust_name` pins, `; unexported:` records) is cross-crate API: any change to what the emitter
  writes must version through the seam header, and a dep-tool-newer-than-consumer combination must
  always hard-error rather than misread (the consumer's strict `@`-token whitelist +
  version-header scan is the enforcement point; `extern_import_unknown_version_hard_errors` and
  `extern_import_unknown_annotation_hard_errors` pin it). Watch: a new comment-DSL directive
  widens the consumer whitelist automatically (it reads `comment_ast`'s tag set), so an OLD
  consumer tool meeting a NEW directive in an export still hard-errors — that asymmetry is the
  designed behavior, not a bug report.
- **Exclusion-record reasons are informational, not interchange.** Consumers never parse the text
  after `; unexported: <ident> — `; wording may change freely. If tooling ever wants to act on
  exclusion REASONS, that's a dialect v2 field, not a regex over prose.
- **Nested-cargo scratch retention: `/tmp/cddl*` leaks until the disk fills.** The nested-cargo
  gates and the in-process test suites mint per-run scratch dirs under the system temp dir
  (`cddl_codegen_test_*`, `cddl_codegen_corpus_compile_*`, `cddl_verify_*` — the verify wasm
  target dirs are ~2 GB each) and rely on end-of-run cleanup that a killed or crashed run never
  reaches, so debris accumulates silently across sessions. Observed 2026-07-19: 3316 leaked
  `/tmp/cddl*` entries totaling 43 GB filled the root filesystem to 0 bytes free, which then
  cascaded — one session's full-tier run died mid-gate (the closure-audit strace could not write,
  and the death left a half-updated `cddl-matrix/annotations` overlay that failed the NEXT run's
  `build_matrix_check`), and harness task-output plumbing broke for every concurrent session.
  Manual remediation (delete only entries older than a day, sparing paths named by live
  processes' cmdlines — the pattern-kill warning in AGENTS.md applies to deletion too) recovered
  35 GB. Scratch is self-retiring since: every `check.ts` run sweeps registry-named, day-old,
  live-process-unguarded entries before its disk preflight (`tests/README.md` § "Run-start scratch
  sweep"), and the sweep's first run on the development box removed 8284 entries / 42 GiB — the same
  saturation, re-formed. What the sweep cannot reach bounds what is left to watch: debris younger
  than the 24 h threshold (a single day of crashed full-tier runs can leak tens of GiB inside the
  window), and scratch minted by a `cargo test` invocation that never goes through `check.ts`. So
  still treat unexplained ENOSPC/mid-gate deaths as possible scratch saturation and check before
  re-attributing — but read the run's own sweep line first, because a run that swept and still
  refused on the disk floor is reporting a different problem.
  - **Reopening signal** (for a bounded named scratch root the tiers reuse and truncate, which
    retires debris by construction rather than by age): a disk-floor refusal or an ENOSPC gate death
    on a machine whose SAME run's sweep line reported nothing removed. Both halves are in the one
    log the operator already has, so the report arrives self-diagnosing.
  Two corroborating
  sightings from the concurrent session, same saturation window: a first-ever generation run
  exiting 1 whose only capture went through `tail -3` (evidence burned — the exact failure mode
  the evidence-preservation rule names; green on immediate rerun, attribution now unrecoverable
  but consistent with a full `/tmp`), and a local-tier run failing three nested-cargo gates on
  os error 28 mid-saturation (all green on the post-remediation rerun; its full log was then
  destroyed with its worktree — the per-checkout `draft/logs/` lifetime note in AGENTS.md).
- **A tier's PEAK MEMORY is bounded by arithmetic over ASSUMED constants — the sampler now
  measures the real peak, and replacing the assumptions with its measurements is the open work.**
  The sibling of the disk entry above, and the proven class: a full disk fails a gate, an
  overcommitted memory cap takes the whole machine — three times now (a ~10-minute freeze that
  produced the first bound, then two ~1–1.5 h freezes at 100 % memory and swap, 2026-08-04, that
  went through every check the first fix installed). The second incident's root-cause (`d5d43bba`)
  closed three budget holes the arithmetic had: the basis was MemTotal where the question is
  MemAvailable (now re-measured per batch), the sequential gate path set no bound at all (so
  `CHECK_JOBS=1` — the cautious setting — routed everything through the unbounded path), and bare
  `cargo test`/`build` invocations outside the runner had no bound anywhere (now floored at
  `[build] jobs = 4` by the repo `.cargo/config.toml`). Two further holes closed since, both
  found by asking what still multiplies concurrency after those fixes: the slot COUNT model was
  wrong, not just the slot-size constant — `CARGO_BUILD_JOBS` divides one nested cargo's
  compilers while the number of nested cargos a `cargo test` gate holds open is the libtest
  thread count (`nproc`), a `threads × jobs` product no arithmetic modeled — bounded now by
  `tool_cmd`'s counting semaphore (`CDDL_NESTED_TOOL_PERMITS`, runner-exported per gate, safe
  default for bare runs); and the `.cargo/config.toml` floor never reached a bare invocation's
  NESTED cargos (config discovery walks up from the scratch CWD and finds nothing; cargo does not
  export its jobs to test children), so `tool_cmd` also defaults `CARGO_BUILD_JOBS` to the same
  floor when the environment has neither. The measurement system this entry used to name as
  missing exists now: every run samples its own descendant tree and reports peak Σ RSS
  (test processes included), peak concurrent `rustc`, the largest single process and the
  MemAvailable floor — printed per run and appended to `draft/memory-peaks.jsonl`, reported and
  never asserted (peaks are nondeterministic; a gate that fails on a number would be flaky by
  construction). What remains, in order of leverage:
  - **Spend the measurements.** The 4 GiB slot constant and the one-permit nested bound are
    deliberately pessimistic prices for unmeasured quantities, and the pessimism costs real wall
    time (nested children serialize per gate). Once accumulated `memory-peaks.jsonl` rows from
    real tiers bound a slot's true peak, re-derive both constants from the measurement and record
    the basis where the constants live.
  - **Freezes are a LIVELOCK, and no in-repo arithmetic can make them impossible — containment is
    an operator measure.** The incident signature (hours at 100 % memory and swap, and the kernel
    log carrying NO oom-kill for the window) is the near-OOM state where reclaim keeps barely
    succeeding on file-backed pages, so the OOM killer never fires and the machine thrashes
    indefinitely. This narrows an earlier claim of this entry's family: a saturated machine is
    unobservable *by passive gates*, but kernel-enforced containment — an early-OOM daemon, or a
    cgroup memory cap with `memory.oom.group` around the tier — converts the freeze into a killed
    process inside a gate: attributable, loud, and over in seconds. It also covers the class the
    arithmetic structurally cannot: MemAvailable per batch is a snapshot, not a reservation, so
    concurrent sessions (and co-tenants like editors and language servers) can jointly overcommit
    a box whose every individual budget was correct. Machine-level, so it is an operator action
    recorded here rather than a gate to build.
  - **The negative premise stands watch.** A future spawn path outside `tool_cmd`, the runner's
    env exports and cargo config discovery would regress the bounds silently; the sampler's
    per-run peaks are what would show it (a peak far above the budget's arithmetic is the tell),
    which is another reason the reports must keep being produced even while nobody is debugging.
- **A tier's DISK BANDWIDTH is unmeasured by anything — the scratch floor is a capacity check,
  not a rate one, and no incident has ever been attributable either way.** The third sibling of
  the two entries above, narrower than both: it names a candidate factor, not a proven one. Up to
  `CHECK_JOBS` gates each drive a nested cargo against its own target dir — separate directories,
  one device — and nothing measures or bounds the write RATE; the device's sustained ceiling is
  an ENVIRONMENT property the tier does not observe, lowest on virtual-disk hosts (WSL2 mounts
  `/` and `/tmp` on one virtual device, far below bare metal). This entry first shipped
  (2026-08-04) reading a whole-machine freeze under ~3 GB/s sustained IO as a bandwidth
  saturation; the same day's root-cause landed the attribution elsewhere — the freezes were
  MEMORY (swap thrashing; three concrete budget holes, closed by `d5d43bba`), with the disk
  traffic as their symptom — so what survives here is exactly the unmeasured-factor claim, plus
  the two facts any future incident inherits: a saturated device is a WHOLE-MACHINE stall, never
  a gate failure, so no PASSIVE gate can observe it (the signal is external — a human at a dead
  terminal; kernel-enforced containment, the operator measure the sibling memory entry records,
  is the one thing that can turn a stall into an in-gate failure), and the post-`d5d43bba` budget
  logs its memory basis per batch — and the sibling entry's sampler now records per-run peaks and
  the MemAvailable floor — so the
  NEXT stall can at least be split into "memory bound was wrong again" vs "memory healthy,
  something else saturated". Candidate instruments if that second reading ever occurs, recorded
  with costs rather than built (no portable measurement of a device's sustained-write ceiling
  exists, and a fixed cap is what `CHECK_JOBS` already is): an IO axis on the sibling memory
  entry's sampler (which exists and walks `/proc` per tick already) — per-run `/proc/diskstats`
  write-rate deltas on the same ticks, recorded beside the
  durations, reported and never asserted — which is what would turn the next incident's
  attribution from forensics into a lookup; and only downstream of such an attribution, a
  rate-aware degrade (an environment-derived `CHECK_JOBS` default for virtual-disk hosts).
  Build signal: a whole-machine stall with the memory budget demonstrably healthy (its per-batch
  basis lines green in the run's own log) — the first bandwidth-attributed incident there has
  ever been, which buys the sampler's IO axis at minimum.
- **Gate-cache residual costs.** The nested-cargo gates (`feature_corpus_compiles`,
  `wasm_matrix_compiles`, `multifile_matrix_compiles`, the layer-2 recombination sweeps, and
  `verify.ts`) memoize per generated-tree content hash (the gate cache), so re-run wall-clock is a
  first-run price; what remains always-run:
  - **Residual all-hit cost** — an all-hit `feature_corpus_compiles` still measures ~2.5 min of
    always-run work (177× generation + 177× `cargo generate-lockfile`); dedupe lockfile resolution
    per identical generated `Cargo.toml` within a run if that bites.
  - **Coverage extensions** — the `run_test` fixture suites (uncached: reused export dirs already
    replay warm-incrementally through cargo, and their external path-dep closure is larger) and
    classified deterministic FAILs (expected-red skip-listed cells and unsupported `verify.ts`
    probes re-run every time; caching a *classified* compile-fail is the same soundness argument,
    but transient-env failures must stay uncacheable — needs a careful failure taxonomy first).
  - **Closure-audit traced-set extension** — the input-closure audit gate
    (`gate_cache_closure_audit`; `tests/README.md` § "The gate cache" / "Soundness gates") traces
    ONE representative cached gate per run (`multifile_matrix_compiles` by default, the
    highest-risk path-dep read pattern), parameterized by `CLOSURE_AUDIT_GATE`. Widen coverage as
    configuration when a new read pattern warrants it — the cargo-test + json-gen shapes
    (`feature_corpus_compiles`), the roundtrip and recombination sites, and the TS-side `verify.ts`
    cached sites (whose nested cargo runs with cwd = the repo, guarded meanwhile by the audit's
    static `.cargo/config` assert) — each run adding a full trace's wall-clock, so extend
    deliberately rather than tracing every site every run.
  - The pre-cache remedies stay valid if the UNCACHED path ever bites (a touch-everything change
    pays full price): batch cells into fewer crates, or adopt `cargo-nextest` as the suite runner
    (`multifile_matrix_compiles` measured ~35 s cold / ~30 s warm at 43 cells; 144 at HEAD).
- **Registry-fetch transients in nested-cargo cells.** Nested-cargo cells used to resolve deps
  from crates.io per temp crate, so a flaky network/proxy (a proxy aborting ~1-in-6 CONNECTs to
  index.crates.io, a class cargo's own transient retry never engages on) killed otherwise-green
  runs at a random cell with `unable to update registry crates-io` / `curl [56] Proxy CONNECT
  aborted` — the shifting-cell + registry-error signature, distinguishable from a real red cell
  at a glance. Hardened by construction in check.ts: local/full runs warm-fetch once (retried)
  and force `CARGO_NET_OFFLINE=true` for every gate, so no cell touches the network
  (`tests/README.md` § "Offline-after-warmup"; the warm-up dep-universe manifest is drift-gated by
  `warmup_manifest_covers_registry_dep_universe`). What remains under watch:
  - **Surfaces still online**: standalone script/gate runs outside check.ts (a bare
    `bun run verify.ts`, an isolated `cargo test --bin cddl-codegen <gate>` confirm) — prefix
    `CARGO_NET_OFFLINE=true` there after any warm run; and the warm-up fetch itself (retried,
    hard-stop before any gate if it can't fetch, `CHECK_ONLINE=1` to opt back into online runs).
  - **verify.ts evidence absorber stays**: a transient null replay would flip a row's
    decode-foreign clause to "FAILED", which `verify_cache_transparency` reads as an A/B
    divergence — absorbed by decodeForeignProbe's regenerate-retry-once (the same retry the mint
    paths carry); the transparency gate is the standing detector if a null-replay class outruns
    the retry.
  - **A sighting under an offline gate run is a NEW class**, not this one — an offline cell
    cannot hit the registry; investigate it, don't re-ledger it here. A
    `no matching package named <dep>` failure offline is the known manual tail: a tests/ fixture
    crate grew a dep outside the warm-up manifest — add it to `tests/warmup/Cargo.toml`.
  Fail-fast discipline is unchanged and general: a failed run plus a one-gate retry is NOT a tier
  pass — fail-fast SKIPS every downstream gate, so a tier-level claim needs the tier re-run (the
  gate cache keeps already-passed cells cheap).
- **`verify_cache_transparency` A/B split on an emission-profile EMBED cell — intermittent, distinct
  from the ruby and registry classes.** Observed 2026-07-13: one `cache_transparency.ts` leg diverged
  on `prelude.tstr`'s `emission.preserve.evidence` — the synthetic-holder embed round-trip
  (`embedFallback`) landed `embedded=true` ("round-trips when embedded") in one run and `embedded=false`
  ("no minted round-trip surface") in another, same input. A back-to-back offline re-run
  (`CARGO_NET_OFFLINE=true`) was byte-identical (714 cache hits), so it is INTERMITTENT, not a
  deterministic cache-reconstruction bug: the `minted` bit of the embed holder's generated
  `generated/mod.rs` varies run-to-run (a transparent-alias-under-preserve cell whose holder sometimes
  mints a standalone test surface, sometimes not — the residual tail of the fresh-output-dir /
  warm-up-clobber class the `72cf4ce`/`a796b7c`/`dbcead7` fixes addressed for the base probe, not yet
  the emission-embed path). The tell that separates it from a ruby flake (now deterministic — README
  § "Gotchas") and the registry-transient class (shifting cell + `unable to update registry`): a SINGLE
  emission-embed evidence line flipping on the `embedded`/`minted` bit, reproducing on neither an
  offline re-run nor a solo probe. Re-run once (offline) to confirm transience before hand-reverting.
  The mechanical fix, when the rate warrants: make the emission-embed generation use a fresh output dir
  per cell (mirror the base-probe fix) so a prior cell's minted `mod.rs` can't be read for the current
  holder.
- **Full-suite flake, attributed and hardened: `acquire_scratch_lock_serializes` — watch only for
  a recurrence that outlives the retry deadline.** Five sightings (2026-07-06 through 2026-07-17),
  every one in the `test` gate under parallel nested-cargo load, none reproducible isolated (60
  isolated runs at the second sighting). The FIFTH sighting was the first fully-attributed capture
  (the pipe-to-file discipline finally paying for itself — the third and fourth sightings were
  burned through `tail`/`grep` and carried zero evidence), and it landed on the `match` split's
  **`WouldBlock` arm**, not a syscall errno: the release-assert saw the lock still held
  immediately after dropping the holder. That attributes the class: `flock` locks are per
  open-file-description, and a concurrent `Command` spawn on another test thread forks a child
  that inherits a DUPLICATE of the holder's descriptor until its exec closes the CLOEXEC fds —
  during that fork-to-exec window the duplicate keeps the lock alive, so an INSTANTANEOUS
  post-drop `try_lock` can transiently observe `WouldBlock` exactly and only under the suite's
  constant-subprocess-spawn load profile. The gates' real acquisition path is the BLOCKING
  `lock()`, which waits out that window by construction, so production serialization was never at
  risk — only the test's instantaneous assert raced it. Hardening shipped with the attribution:
  the release assert retries `WouldBlock` on a bounded 5 s deadline (a transient fork-to-exec
  hold clears as soon as the concurrent child execs; a hold that outlives the deadline still
  panics as a genuine release-on-drop semantics break), and the syscall-`Error` arm still reports
  the raw errno for the never-yet-seen ENOLCK-class transient. Standing discipline unchanged and
  proven load-bearing: pipe every `check.ts` run to a FILE from the FIRST invocation — a
  deadline-outliving recurrence is a real kernel/std finding and needs its full log.
- **`verify.ts` warm-up `cargo test exit -15` (2026-07-19): ATTRIBUTED on the first sighting — not
  a flake, do not watch for it.** One session's `verify.ts` run (json warm-up; logged signature
  "generate exit 0, cargo test exit -15, minted=true") died because a CONCURRENT session ran
  `pkill -f cddl_verify` to clean up what it believed were its own killed run's orphans — but its
  killed `check.ts full` had died in `corpus_decode_replay`, BEFORE its verify gate ever ran, so
  the `/tmp/cddl_verify_*` nested-cargo processes it pattern-matched belonged to the other
  session's LIVE run (exit -15 = pkill's default SIGTERM; window and scratch naming match). A
  recurrence of this signature under multi-session load should FIRST be checked against
  process-kill activity in the other session before any harness attribution. The standing
  discipline it feeds is in `AGENTS.md` (never pattern-kill by tool-generic substring on a shared
  machine).
- **A backgrounded `check.ts full` launched from a SUB-AGENT's turn dies before it can finish —
  ATTRIBUTED to the launching topology, not a wall-clock ceiling. Do not watch for it; the standing
  rule is in `AGENTS.md`.** Four 2026-07-24/25 sightings, all from orchestrating sub-agents, all the
  same signature (task "killed", entire process tree gone, self-log stops mid-gate with no
  tier-level `RESULT` line and no error): ~53 min and ~60–65 min inside `gate_cache_closure_audit`
  (two runs, 2026-07-24), then 60 min and ~68 min inside `verify` (two runs, 2026-07-25). What
  settles it: the SAME tier on the same machine, launched from the MAIN session, ran 74 min to a
  clean `RESULT: PASS — all in-tier gates green`, and a shorter (~35 min) sub-agent background
  run completed fine — so the constraint is the sub-agent turn's lifetime, not a resource or
  wall-clock limit on the run itself. Consequence: a ~75-min full tier can NEVER complete from a
  sub-agent's turn, and delegating one is a guaranteed loss of a multi-minute run. Signature to
  distinguish from a hang: process tree GONE + log mtime stale (a hang keeps live processes).
  Distinguish from cross-session pkill (the entry above): no kill activity in any concurrent
  session, and exit is a task-kill, not `-15` in the run's own log. Recovery when one has already
  died: enumerate completed gates from the self-log (`grep '^--- '`), run the remainder isolated
  (the audit gate alone ≈16 min: `bun run audit_gate_cache_closure.ts` in `cddl-matrix/`;
  `corpus_detect.ts` and the fuzz `cargo check` are seconds), and claim the tier green only by
  COMPLETE gate enumeration — never from the partial log.

## Declined (decided, with the reopening signal)

- **Making the TYPE-CHOICE path reject a duplicated explicit `@name` the way the group-choice arm
  path does.** The two enum-producing paths now differ in policy: two group-choice arms of one rule
  given the same `; @name` are a graceful rejection (`reject_group_choice_arm_variant_name_collision`
  — an authored name is public API of the generated crate, so renaming one would ship a name nobody
  wrote), while a type choice in the same situation (`foo = 0 ; @name mainnet / 1 ; @name mainnet`)
  silently emits `Mainnet` and `Mainnet2`. The divergence is deliberate and rests on what the two
  produce, not on principle: the arm path was emitting a REPEATED variant — a generated crate that
  does not compile (`E0428`) — so it had to change and the only question was which way, whereas the
  type-choice path emits code that compiles and works today. Tightening it is therefore a pure
  behavior change on working output, and it would break any spec whose author has (knowingly or not)
  built against the `Mainnet2` spelling — a cost paid by existing consumers to buy consistency no
  consumer has asked for. Note the asymmetry does NOT extend to derived names: those carry no
  authorial intent and take a numeric suffix on BOTH paths (`foo = [ x: uint // x: text ]` gives
  `Foo::X` / `Foo::X2`), which is the behavior `append_number_if_duplicate` exists for. Reopening
  signal: a consumer reporting that a variant they EXPLICITLY `@name`d was emitted under a different
  name — the harm this would prevent, observable by the party who has it, and distinct from the
  derived-name suffixing that is working as designed.
- **MSRV declaration / OS matrix for GENERATED code.** The templates' `edition = "2024"` already
  hard-floors the effective MSRV at rustc 1.85 with a self-explanatory compile error, and generated
  output has no platform-conditional code an OS matrix would exercise. Revisit only if a consumer
  reports an MSRV or platform break (dep-driven MSRV creep is the one real vector).
- **A docs-vs-behavior conformance harness for `comment_dsl.mdx` / `output_format.mdx`** (snippet
  extraction + output spot-checks): emitted output is already pinned by the snapshot corpus and
  DSL-name drift by the `cddl-matrix/verify.ts` forward lint; a doc-snippet system is heavy
  machinery for prose drift that review catches at its actual rate. The decline has narrowed since
  it was made: identifier-existence drift (a cited pin that no longer exists) is now mechanically
  covered by the `lint_doc_citations` gate, so what stays declined is prose SEMANTICS only — a
  sentence whose cited pin exists but whose claim about it is wrong. A THIRD prose home joins
  `docs/` and the generated `COVERAGE.md` span: a FIXTURE HEADER commenting on a SIBLING fixture.
  `tests/robustness/choice_cbor_ref_arm.cddl` described the rule-body spelling as "the remaining
  unsupported spelling … unaffected by rule ordering" — a claim its sibling
  `cbor_ref_rule_body.cddl` contradicted in its own header from the commit that fixed both, and
  which the catalog snapshot (`ok`) had already recorded as false. Fixture prose is outside
  `lint_doc_citations`' scan surface for the same reason catalog prose is, and the claim named no
  artifact that could dangle, so this stays in the declined class. It is recorded as evidence the
  decline is calibrated rather than as a gap: review caught it one cycle after it shipped, which is
  the "actual rate" the decline is priced at, and the reopening signal for this class is a false
  claim SHIPPING PAST review — not one being caught by it. Authoring rule that would have prevented
  it at zero cost: a fixture header states what THIS fixture pins, and refers to a sibling only by
  name and role, never by asserting the sibling's support status — the catalog snapshot is where
  that status lives, and it is already gate-enforced. NB this decision covers PROSE
  drift only — the separate class of a directive silently no-oping in an unenumerated attachment
  position (`@name` was dropped on arrow keys once and bareword keys once, both found by hand) is
  a real class with its own standing system: the directive × attachment-position sweep
  `src/tests/dsl_position_tests.rs`, hard-asserted against `comment_dsl.mdx`'s claims. (The `example/`-half of this
  decision — gating the getting-started command — was accepted and shipped as
  `integration_tests::getting_started_example`.) A SECOND carve-out (2026-07-25): SYNTACTICALLY
  INVALID directive illustrations are not prose-semantics drift either, and review's "actual
  rate" proved one delivery late for them — the open-arrays doc pass shipped an inline example in
  the one-line directive-before-closing-bracket spelling (the CDDL comment swallows the `]`) two
  bullets BELOW the doc's own warning against exactly that spelling; caught in orchestrator
  review, one delivery after the same trap class was first documented for the map-brace flavor.
  The proven spelling class is now mechanically banned: `lint_doc_citations` scans
  `docs/docs/*.mdx` inline spans and fenced examples for a `; @<directive>` comment sharing its
  line with a closing `}`/`]` (self-checked canary; the warning bullet's deliberate
  counterexamples are allowlisted by exact span text). What STAYS declined is the general
  snippet-extraction harness; its reopening signal is now concrete: a docs example invalid for a
  reason the narrow spelling ban cannot see (any non-directive syntax error class recurring in an
  illustration).
  A THIRD instance class on record (2026-08-04), counted against the decline without reopening
  it: a FALSE UNIVERSAL. `current_capacities.mdx`'s tag-rule section claimed the auto-wrap holds
  for "*every* single-type inner" while the collection and `T / null` inners registered
  transparent aliases (the T1-13 standalone-tag-drop defect, per the tier-1 close-out's board
  record) — a false claim that SHIPPED past review and stood until the `register_type_alias`
  wire-facts assert's carve-out enumeration falsified it mechanically. It does NOT reopen the
  snippet harness because that harness could not have caught it either: the counterexamples were
  unenumerated instances of a universal quantifier, never spelled as snippets — snippet
  extraction validates the examples a doc gives, not the quantifier over the ones it doesn't.
  The layer that actually catches this class is a behavior-side seam assert whose domain IS the
  quantifier's (the wire-facts assert enumerates every registration, so the doc's "every"
  now has a mechanical counterpart), which is where such universals should be anchored: a doc
  universal about generated-code behavior earns a seam assert or a sweep whose axis is the
  quantified domain, and the prose then describes the mechanism rather than substituting for it.
  A FOURTH instance class on record (2026-08-05), counted against the decline without reopening
  it — and the sharpest, because here the doc and the behavior AGREED: `output_format.mdx`'s
  trailing-bytes paragraph stated "(Nested/embedded decoding — e.g. `bytes .cbor T` — is
  unaffected; only the top-level entry point enforces this.)" — a TRUE sentence about a DEFECT
  (the `.cbor` embed's missing exhaustion check, the cycle-13 fuzz find, fixed in `cff85166`).
  Any docs-vs-behavior conformance harness passes it by construction, since there is no
  divergence to detect; what was wrong was the boundary itself, stated as if decided when no
  decision record existed. The layer that caught it is a SPEC-side oracle over the behavior —
  the byte-fuzzer's preserve-fidelity leg (`fuzz_bounded_run`), whose contract comes from RFC
  8610 §3.8.4 + the preserve round-trip promise rather than from our own prose — and that layer
  is now standing. The authoring rule this instance adds, same zero-cost class as the
  fixture-header rule above: a docs sentence that states a LIMITATION or asymmetric boundary
  names its provenance — the decided-posture record (a README gotcha, a matrix out-of-scope row)
  or the findings/roadmap entry that owns it — so an anchorless boundary sentence is visible at
  review as an undecision wearing decided clothes. Reopening signal for that rule, measurable by
  whoever fixes the next such defect: a second anchorless boundary sentence found to have
  documented a defect as intended, after this rule was in force.
- **Full `2^N` flag powerset / PICT pairwise** — the curated named profiles cover the flag
  *combinations* worth testing, so the full powerset stays out of scope. Escaped interactions earn
  their own standing cells rather than the whole powerset — four so far (the Fourth is recorded
  after the Third below, with a now-armed trigger). First:
  `--common-import-override` × `--preserve-encodings=false` targeting a preserve-flavored common
  crate emitted `CBORReadLen::new(Len)` against a `new(LenSz)` runtime (E0308). Second:
  `--workspace-dep` × `--wasm=false` silently ignored the flag — validation included — fixed to
  mode-independent honoring and pinned by `workspace_dep_unknown_is_rejected_under_wasm_false`
  plus the flavored contract's rust-only byte-identity leg. Its read-caught sibling, the
  `--extern-wrapper-index` validation skip under `--wasm=false`, is likewise fixed to
  mode-independent validation (deferral still wasm-gated) and pinned by
  `extern_wrapper_index_is_validated_under_wasm_false`. Third, an EMISSION-path flavor (not
  validation-inertness): `--preserve-encodings=true` × `--annotate-fields=false` — no profile
  combines them, and `flag_value_smoke`'s annotate=false case runs a spec with no encoding-less
  fixed member, so the combination's fixed-value deserialize paths were unreachable by every gate.
  Two bugs lived there: bool/null fixed members emitted a bare `()` with no statement terminator
  (non-parsing output), and uint/nint/text fixed MAP values bound their inline `{field}_encoding`
  over the arm's outer accumulator (E0308 assign-to-shadow) — the first found by reading during
  the optional-fixed-value delivery, the second at the arming run of the standing cell that now
  owns the combination (`preserve_no_annotate_fixed_members_generate`, generating both fixed-member
  corpus fixtures under the pair). Both fixed. Fourth, the same input-poverty mechanism as the
  Third on a different emit path: `--annotate-fields=false` (non-preserve) × a value-bounded
  `nint` MEMBER — the N64 bounds `.and_then` relied solely on the site's `error_convert`, which
  is empty under annotate=false, so the closure inferred the reader's native `cbor_event::Error`
  and the emitted crate failed E0277. `flag_value_smoke` swept the flag the whole time but its
  input (`tests/canonical/input.cddl`) had no bounded nint, so the path was unreachable by every
  gate until the `generated_code_clippy_clean` provocation shape (`clippy_neg_bounded`) entered
  that shared input and turned the smoke red. Fixed (convert-at-most-once, pinned by
  `deserialize_converts_error_at_most_once`; the shape stays in the smoke's input permanently).
  Cross-check at discovery: no cddl-matrix item covers this axis — the matrix enumerates INPUT
  surface under the named profiles, and the corpus already held the provoking shape
  (`bounds_spellings.cddl`'s `m_nint_range`, verified red-reproducing under annotate=false at the
  pre-fix rev) — the unswept dimension was the FLAG value. Recur-first: this is the SECOND
  input-poverty instance (a smoke sweeping a flag over an input too shape-poor to reach the
  flag's divergent emission paths), so the trigger for that sub-class is ARMED; the named layer
  is an `--annotate-fields=false` compile-floor leg over the feature corpus (or its
  deserialize-shape subset) inside `feature_corpus_compiles`' machinery — the corpus is the
  shape-rich input the canonical smoke is not, and gate-cache memoization bounds the added
  nested-cargo cost. Build it on the next escaped annotate=false instance OR when touching that
  gate's profile set for another reason, whichever comes first. Recur-first lesson from the first
  three: a THIRD validating
  flag turning up mode-inert is the trigger
  to build the class-level validation-smoke sweep — each clap flag with documented startup
  validation invoked once with a deliberately invalid value under each `--wasm` mode, asserting
  nonzero exit — rather than accreting per-flag standing cells. The extern-deps surface is now
  probed under both preserve flavors — `integration_tests::extern_deps` (preserve) and
  `integration_tests::extern_deps_non_preserve` (non-preserve, compiled against the preserve-flavored
  `extern-dep-crate` stand-in) — plus the wasm-boundary cell `integration_tests::extern_deps_wasm`
  (`--extern-wasm-crate` against the split `extern-dep-crate`/`extern-dep-crate-wasm` pair; extern
  types as list elements and table keys/values, from root AND non-root use sites — the non-root
  `nested` cells pin the wrapper-element imports registered from the wrapper's emission scope, which
  root use sites would mask) — so those specific cells are pinned without enumerating the rest.
  Adjacent but on an orthogonal axis to the First interaction's runtime-flavor mismatch (that one is
  a `CBORReadLen` construction seam; this one is a derive seam): the cross-crate `Int` **key**-flavor
  channel under `--common-import-override` is pinned by `int_key_via_common_import_override_sidecar`
  (consumer — the `borrowed_key_types.rs` `(<override>, "int")` row and its self-check compiling
  against the `extern-dep-crate` fixture's key-capable `Int`) and the `int` leg of
  `workspace_key_requests_derive_effect_and_hard_errors` (dep — an `int` `--key-requests` row emits a
  key-flavored `Int` a spec never references, while a `uint` row still hard-errors), with the
  `extern_deps`/`extern_deps_wasm` cells and the pure-consumer cell `common_override_wasm_int`
  (override crate NOT also a declared extern dep) covering the plain `Int`/`IntError` re-export path.
  Every individual flag *value* now appears in some profile or test: the five that previously
  didn't are covered by `flag_value_smoke` (`--annotate-fields=false`,
  `--to-from-bytes-methods=false`, `--binary-wrappers=true`), `wasm_cbor_json_api_macro_compiles`
  (`--wasm-cbor-json-api-macro`), and — for `--canonical-form=true` without `--preserve-encodings`,
  which emitted a non-compiling crate — a CLI rejection (`api::with_types`, pinned by
  `flag_value_rejects_canonical_without_preserve`).
- **`quickcheck` alongside `proptest`; `goldenfile`/`expect-test` as a second corpus engine;
  `no-panic` lints; coverage instrumentation of *generated* code; `trybuild` for whole-crate
  compile-pass** (the corpus `cargo check` is simpler and broader).
- **An orphan-fixture-directory meta-test** (assert every `tests/<dir>/` is referenced by some gate):
  fixture dirs change rarely and a new gate's author touches the dir listing anyway; the failure
  mode (a committed fixture nothing runs) is caught by review at that rate.
- **Restructuring `features.std` into a tool-owned sub-feature (a `_cddl-codegen-std` key
  referenced from a consumer-owned `std`).** Declined because the key layout cannot change the
  SEMANTICS, and its two legibility benefits are each dominated by a cheaper reversible channel.
  The semantics are forced to exactly union-plus-prune by three constraints, whatever the keys:
  the `--rust-dep`/`--std-forward-dep` family is assert-only (a dropped flag carries no name to
  tombstone, and `merge_dep_spec` never removes a field the tool's current spec doesn't set, so
  its `default-features = false` outlives the flag un-retractably — retracting the forward while
  defaults-off persists would hand a `std` consumer the path dep's alloc build silently); a
  forward to an absent dependency is a manifest cargo rejects (the prune); and hand forwards are
  uncomputable even in principle (the dependency's feature NAME is unknowable without a registry
  lookup — itertools spells it `use_std`), so they must union-survive. With semantics fixed, the
  split would deliver only a visible ownership boundary — the human form of which the asserted
  `# cddl-codegen:` comment block delivers (and states the RULES, which a key partition cannot),
  and the machine form of which the metadata-record deferral above delivers strictly better —
  while the split alone is irreversible: a cargo feature name on published generated crates is
  public API, and removing one later is a semver break. Every candidate reopening observable
  routes to a different remedy, which is the decline: consumer confusion recurring AFTER the
  asserted comment ships reopens the comment's TEXT; a machine consumer appearing fires the
  metadata deferral; a hand entry lost under the merge is a merge BUG. Nothing observable routes
  to the split itself.
- **An ownership journal making a dropped `--std-forward-dep` converge (safe retraction).**
  Dropping the flag while keeping its `--rust-dep` leaves a stale-but-consistent pair today — the
  dep keeps `default-features = false`, the `<pkg>/std` forward union-survives beside it — safe,
  documented on the flag, fixable by hand. Making the tool retract it requires journaling
  provenance in the manifest (which run wrote the forward AND the `default-features` field, since
  the tool cannot otherwise tell its own defaults-off from the user's), a qualitatively new use of
  the bounded existing-manifest read in which a PRIOR run's flag set changes THIS run's asserted
  content. Declined: a new invariant surface to buy convergence of a documented harmless
  staleness. Reopening signal: a consumer report in which the surviving pair is a defect for them
  — they dropped the flag to STOP forwarding and the persistence harms their build — rather than
  the documented safe leftover.
- **`@wrapper_name` — a user-set name for a synthesized collection wrapper.** Declined in favor of
  container-encoded structural names, which is what the wrapper-name collision class needed: a name
  carrying its container (`PairMapKToV` vs `MapKToV`) makes a same-shape/different-flavor claim
  unrepresentable rather than merely diagnosable. The SAME-flavor sibling-crate collision — two
  crates minting one structural name — splits in two, and only one half is still open.
  When both sides name **one concept** (a crate re-exporting a sibling's type and minting that
  type's companion classes over it), renaming is the wrong remedy in principle: it mints a SECOND JS
  class for one concept, with no type identity against the sibling's own API, so a value obtained
  from the sibling cannot be handed back to it. That half is **served** by the
  `@extern_companions` directive (docs: Comment DSL § `@extern_companions`; its link-level
  acceptance is pinned by `extern_companions_defers_to_sibling_wasm_crate`), which references the
  sibling's class instead of minting one — the collision becomes unrepresentable rather than
  renamed-around. What is left for `@wrapper_name` is the genuinely-**two-concepts** collision: two
  crates whose same-spelled local types are different domain concepts that happen to derive one
  structural name, where re-spelling a key or value alias in one spec is the current answer. The
  design surface a future build must settle before it is cheap: two same-shape rows carrying
  different names, against a structural name that is currently the identity the deferral index, the
  request sidecars and the wrapper dedup all key on; user-authored names entering the rule-ident
  collision detectors, which reason about generated spellings only; and name-keyed deferral matching
  seeing a name no other crate can derive from the shape. Reopening signal: a consumer reports a
  same-flavor sibling collision between types that are **distinct domain concepts on the two sides**
  — so neither crate can reference the other's class, and the alias-respelling workaround would
  misname one of them rather than merely restate it. (The one-concept flavor no longer fires this:
  it has a remedy that keeps the shared name.)
- **Checking `[runtime] flavor-from`'s safety condition rather than stating it.** The `[runtime]`
  carrier derivation is a maintainer-CLOSED area (`AGENTS.md`), so this records the one build that
  would be defensible IF that area reopens — not an intent to build it. Shape: error only on a
  VIOLATED condition in a flavor-mismatched crate (a `{+ K => V}` or `any` construct in its spec, a
  `--deserialize-depth-limit` divergence), never on the mismatch itself, which the once-per-run
  carrier note already describes. The depth-limit divergence is the sharp one — it compiles, and
  silently guards one crate's `any` values at another crate's limit. Cost the check carries: the
  flag→poisoned-construct map becomes load-bearing and a stale map yields a false "safe", so it owes
  a lockstep guard tying its construct list to the runtime's flavor-conditional surface. The same
  divergence class is reachable WITHOUT config mode — hand invocations pairing
  `--export-static-crate` with `--common-import-override` consumers — and there it has less signage
  still: no carrier note exists, only the MATCH contract on the flag's own doc. A check on that path
  is a structurally different build, not a scope extension of the config-mode one: no single hand
  invocation sees both flag sets (the consumer knows only the runtime's path, used verbatim), so it
  would need the exported runtime to carry a machine-readable flavor record that consumer generation
  reads as an explicit cross-crate input (the `--extern-import` class — another crate's committed
  export, not this run's prior output). That half is not behind the maintainer-CLOSED gate (it never
  touches the carrier derivation) but carries the same lockstep-map cost. Reopening signal: a
  consumer report of the silent depth-limit mis-guarding in a shared-runtime workspace — reached via
  `flavor-from` or via hand-flag `--export-static-crate` — or of a mismatched crate's spec gaining a
  poisoned construct with the resulting failure attributed downstream as a consumer bug.

## Sources
- Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`
- Per-dimension expert write-ups: `draft/testing-recommendations/*.md`
