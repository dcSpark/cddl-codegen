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
(non-final/multiple/plain-group rest rows, `+`/bounded occurrences, non-uint/text/any key domains)
are graceful rejections whose candidate-feature entries live in `cddl-matrix/ROADMAP.md`
§ findings, not here. The permanent exclusions around it stay: `#` (`Type2::Any`), `cbor-any`, `@newtype` and
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
  workaround into shipped output (`WasmWrapper::push_inner_field`; pinned by
  `integration_overwidth_wasm_wrapper_field_gets_rustfmt_skip`). A SECOND proven instance of the
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
  `src/generation/` and remove the over-width branch in the same change; the unfold pre-pass must
  STAY for files already committed in the folded form) — weigh that retirement
  plus the exposure against the emitted-token-stability constraints in AGENTS.md (the snapshot
  corpus and comment-preservation overlay key on the formatter's exact output, so the swap
  re-blesses broadly and must hold the overlay's idempotent-fixed-point property).

- **Consider promoting the sub-second drift/count gates from `local` into `fast` (CI).** The
  local-tier placement of `project_recombination_check`, `project_decode_conformance`, the
  `query_q*` gates, `project_status_headers`, and `lint_doc_citations` is the registry's DEFAULT
  for new gates, not a considered cost decision — their older siblings
  (`build_matrix_check`/`project_robustness_check`/`project_wasm_matrix_check`/
  `project_golden_hex_check`/`project_corpus`, the same no-cargo file-scanner class) are already
  `fast`, and all of these run in well under a second. Proven cost of the split: a HEAD commit
  (the `@raw_bytes_flavor` registration) shipped CI-green with THREE local drift gates red
  (`tests/recomb/ingredients.json` stale, the Q6 `VENDOR_FEATURE_COUNT` pin, the status-header
  count spans), and check.ts's fail-fast meant only the first showed per run — the rot surfaced
  one layer at a time in an unrelated later session. The same commit also left the FULL-tier
  `ir_conformance_corpus` deterministically red (its extern fixture ledgered in `COMPILE_SKIP`
  but not the gate's then-twin `GEN_SKIP` — since unified onto `COMPILE_SKIP` as single owner),
  invisible until an unrelated session's next manual full-tier run — the full-tier flavor of the
  same skipped-tier cost, with no promotion remedy (full stays manual by design; the mitigations
  are the single-owner unification and running full before shipping a feature). Promotion is a maintainer decision (CI
  policy, § above); until then the mitigating discipline is unchanged: run `local` before
  considering matrix-surface work done, and expect stacked reds behind fail-fast when de-rotting.

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

- **A gate-SELECTION flag for check.ts (`--only <gate>[,<gate>]`), and the policy fence it has to
  ship with.** Today the only selection axis is the tier, so a tree on which all but a handful of
  gates are already established green has exactly one way to cover the handful: re-run the whole
  tier. That case is real and recurring — a session ended with 37 registry gates PASS on its HEAD
  and exactly four (`verify`, `gate_cache_closure_audit`, `corpus_detect`, `fuzz_compile_rot`)
  never run, and the gate cache does not help, because the tier's wall time is dominated by exactly
  those uncached cells. Why it is a maintainer call rather than an ergonomic patch: check.ts's design
  premise is that a gate which did not run is VISIBLY not-run, and a selection flag is the obvious
  way to manufacture a "tier green" claim out of a partial run — the same falsified-claim class the
  fail-fast rule in `AGENTS.md` already exists to prevent. So it should ship only with the fence
  built in: a selected run still prints the FULL registry with every unselected gate marked not-run,
  and its final line must NOT be the tier verdict (`RESULT: PASS — all in-tier gates green`), which
  stays reserved for a complete tier.

## Next work items, in priority order

1. **Grammar-fuzzer escalations.** The lazy-first shape-recombination fuzzer is shipped
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
     The class also has a SEMANTIC flavor beyond the compile one, read-confirmed during the
     Int-extern alias-reference delivery (tests/corpus/int_alias.cddl): a bare `.cbor` alias root's
     STANDALONE (de)serialization is the target type's own (`y = bytes .cbor int` → `pub type Y =
     Int`, whose impls read/write a plain int — the byte-string wrapping exists only at embed
     sites), so a consumer using the alias type directly gets spec-divergent bytes with no error.
     No standing layer sees it: the compile gates can't (it compiles), and the decode-conformance
     corpus leg deliberately validates through synthetic embed holders (`__probe_holder`), which
     exercise the wrapped path. The mechanical detector, if the embed-site leg above gets built or
     a consumer hits the flavor first: a standalone-vs-embedded decode differential on
     alias-classifying roots — decode each such root's spec-derived vectors through the root
     type's OWN impls beside the holder leg, red where the two accept/byte sets diverge (rather
     than certifying standalone semantics that the probe holders never witnessed).
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

2. **Byte-fuzzer depth: the tag-set peek path + reject door are wired, but only compile-checked.**
   The `from_cbor_bytes` fuzz crate generates from `tests/preserve-encodings/input.cddl`, which now
   carries a `@duplicates reject` collapsed tag-set field minted as a GENERIC instance
   (`oset_p<a> = #6.258([* a]) / [* a] ; @duplicates reject`, used by `reject_set_preserve`) — so
   both the optionally-tagged deserialize peek path (peek the major type, conditionally consume +
   validate the tag, then the collection body — the one hand-rolled branch the tag-set collapse
   added) AND the reject uniqueness door (the collected Vec routed through `OrderedSet::try_from`,
   raising `DuplicateKey`) are in the fuzzed probe set, along with the generic/non-generic
   convergence. `fuzz_compile_rot` (full tier) proves the surface stays REACHABLE (the crate
   compiles with the field), but the actual hostile-input exploration — a tag head with a truncated
   body, a tag-of-a-tag, a non-array after the tag under indefinite lengths, a duplicate on either
   wire arm — only happens on a manual `cargo +nightly fuzz run from_cbor_bytes`. The residual is a
   scheduling one: wire a periodic / pre-ship fuzz RUN (bounded corpus, time-boxed) so the reachable
   surface is actually walked, not merely compiled — the compile-rot gate cannot see a panic that
   only a live libFuzzer input triggers.

3. **Identifier-length realism (a fixture-corpus dimension, recur-first).** Every fixture corpus
   uses short synthetic names, so any emission-width-driven failure class is structurally
   unreachable by every gate — proven by escape: consumer-scale names (CML's
   `MapTransactionIndexTo…AuxiliaryData` wrappers, fully-qualified extern paths) pushed wasm
   wrapper tuple-field lines past rustfmt's `max_width`, tripping rust-lang/rustfmt#5703's fatal
   internal error and aborting regen — first surfaced as hand-placed `#[rustfmt::skip]` blocks in
   CML's committed generated output (the hand-patched-consumer-output tell; see
   `cddl-matrix/ROADMAP.md` § "wasm-ABI & multifile placement matrices"). The instance is fixed
   and pinned both directions (`integration_overwidth_wasm_wrapper_field_gets_rustfmt_skip`), but
   only for the wasm tuple-field site; other emission sites that concatenate names into one line
   (named struct fields — today verified to wrap safely after the colon — enum variants, fn
   signatures, `impl` headers) are unswept against the formatter internal-error class. The cheap
   mechanical layer, to build on the next width-class instance rather than speculatively: one
   long-identifier stress fixture (names sized so common emission sites exceed 100 columns) swept
   across the emission profiles, whose whole gate is "generation succeeds" — `export.rs`'s
   non-0/3-rustfmt-exit-is-fatal contract already turns any formatter internal error into a
   generation failure, so no new assertion machinery is needed.

4. **Duplicates-policy residuals.** Both `@duplicates` flavors are shipped on every boundary —
   `reject` (set/array uniqueness twins) and `preserve` (table pair-map twins), covering rust,
   preserve-encodings, canonical, JSON/schemars, wasm, extern-interface projection, and the
   `dsl.duplicates.{reject,preserve}` matrix feature rows. Current state lives in
   `tests/README.md` § "Per-rule duplicates policy (`@duplicates`) — test map" (the per-layer
   pin inventory) and the user docs (`docs/docs/output_format.mdx`, `current_capacities.mdx`,
   `wasm_differences.mdx`, `comment_dsl.mdx`). What remains:
   - **Recursive-map-KEY limitation (pre-existing, orthogonal).** A table whose DOMAIN is a
     not-yet-registered recursive UNION (`{ * transaction_metadata => transaction_metadata }`, the
     exact Cardano shape) panics in `register_rust_struct`'s keys-list synthesis
     (`name_as_wasm_array_ct` → `is_enum` asserts on the un-registered union) — WITHOUT any
     directive, so it is a general recursive-table-key gap, not a duplicates-policy one. The
     golden_hex headline keys the recursive metadatum map by `tstr` (recursion in the map VALUE) to
     sidestep it. The tempting one-line route — relax `is_enum`'s assert to a graceful `false` — is
     still WRONG, though its old blocker is gone: the assert
     (`self.generic_instances.contains_key(ident)`, `src/intermediate/mod.rs`) was once also the
     "`any` in member/element position" panic class, but the loose-CBOR delivery retired that class
     by intercepting `any` at `new_type` before it ever reaches the assert
     (`tests/robustness/any_member.cddl` is now an `ok` fixture). What remains is the assert's own
     job: it guards genuinely-unregistered generic instances, and a graceful `false` would silently
     misclassify every such ident instead of failing loudly. Closing the
     recursive-key gap therefore needs the deeper route — defer the keys-list synthesis past the
     recursive registration cycle (so the domain is classifiable when `name_as_wasm_array_ct` runs)
     — which is out of scope for a duplicates-policy packet. That deferral seam now EXISTS:
     `finalize_generic_table_keys_lists` (intermediate/mod.rs, from the wasm-ABI matrix's
     generic-instance-keyed-map fix) already defers the keys-list mint past `finalize`'s domain
     resolution for GENERIC-INSTANCE domains, naming from the final domain — extending the same
     defer-to-finalize route to recursively-registered union domains is the concrete pickup, with
     the positive `any`-member fixture (`tests/robustness/any_member.cddl`, an `ok` row) as the
     boundary that must stay intact. The union-KEYED shape is not the real
     Cardano driver anyway (metadata keys are int/text/bytes; the recursion is in the VALUE, covered
     by the tstr-keyed headline).
   - **Cross-crate wrapper-request hosting of a preserve table.** `requests.rs` threads
     `rt.is_preserve_pair_map()` (correct-by-construction), but an inline cross-crate wrapper request
     carries no directive, so a preserve table hosted purely via `--wrapper-requests` from a consumer
     is untested (the named-rule and generic-instance paths ARE covered). A cross-crate preserve
     wrapper-request fixture would close it.
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
     Corpus-mint note that outlives the delivery: `table_preserve.mdmap` is a `pinned_reason` row —
     its standalone holder synthesis trips the pre-existing recursive-union-valued-table
     `cbor_types` panic (the cddl-matrix findings entry) — while the fixture's
     `holder`/`pmap`/`nepmap`/`pmap_txt`/`md` rows minted with vectors.)

5. **Rustfmt-stability sweep over the preserve-fixture corpus.** The rustfmt match-tail
   comment-fold escape (the second rustfmt exposure instance in the `prettyplease` entry above)
   was found by a manual repro, not by any standing layer — the overlay's fixtures all exercised
   `preserve(old, new)` on pre-rustfmt text, and the one standing test that DOES drive the real
   rustfmt seam twice (`comment_preservation_disk_round_trip`) injects a replace block that swaps
   a mid-function statement, a position rustfmt never folds — so "the tool's own rustfmt pass
   rewrites a marker placement the next run's scan can't read" had no systematic witness; the
   delivered `preserve_markers_survive_rustfmt_fold_roundtrip` closes only the one hand-picked
   shape.
   The general property is cheap to sweep because the corpus already enumerates every supported
   overlay structure: for each `tests/preserve-fixtures/<case>/expected.rs`, run
   `rustfmt_generated_string(expected)` and assert `preserve(formatted, new)` still succeeds with
   the same user content surviving (reuse the harness's never-silent unit check; error-expectation
   cases are exempt — they have no `expected.rs`). That makes "the on-disk, post-rustfmt form of
   every overlay structure re-parses" a standing property over all current AND future fixtures, so
   the next formatter canonicalization quirk (or a `prettyplease` swap) fails a sweep instead of a
   consumer regen. Cost: one in-process rustfmt call per expected-case (~50 fixtures, sub-second
   each) — fits the default suite next to `preserve_fixtures`.
   (The cddl-matrix roadmap was checked for overlap: its pending items cover CDDL *input* surface
   — role/feature/encoding grids — and none would have caught this class; output post-processing
   round-trips are this roadmap's domain.)

6. **Lint-provocation shapes for `generated_code_clippy_clean` (partially systematic at best).**
   The gate itself already exists and denies `clippy::all` over the generated rust and wasm crates
   on two profiles (`generated_code_clippy_clean`, local tier; documented in `tests/README.md`) —
   yet lint classes still arrive consumer-reported when the gate's rich input is provocation-POOR
   for the shape that mints them. The gate's input (`tests/canonical/input.cddl`) now carries the
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
`tests/corpus/optional_fixed_float.cddl` + the float presence-field arms, residue owned by
`preserve_encodings_supports_floats`).

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
  negative-control skew leg. Standing rule meanwhile: a delivery that makes a directive change a
  rule's REPRESENTATION must land the extern-interface projection AND its two-crate fixture in
  the same change (reviewers: diff `extern_interface.rs`'s annotation helpers against the new
  predicate). The trigger for a mechanical layer — an extern-projection sweep that enumerates
  every representation-changing config predicate and auto-checks a dep/consumer pair per
  directive — is a SECOND representation-changing surface shipping without its projection.
- **A preserve and a non-preserve map of the SAME `K`/`V` shape in one spec conflict on the single
  minted wasm map-wrapper's backing container under `--wasm`.** `name_for_wasm_map` mints ONE wrapper
  class per `MapKToV` shape, but the preserve (`@duplicates preserve` → `PairMap`) and non-preserve
  (`BTreeMap`/`OrderedHashMap`) flavors need incompatible inner types, so the same-shape/mixed-policy
  pair cannot share it. There are now TWO instances of the class: **tables** (pre-existing — the
  `@duplicates` coexistence note in `docs/docs/comment_dsl.mdx`, "A preserve table and a non-preserve
  map/table of the identical key/value shape cannot coexist under `--wasm`", is the graceful table-side
  rejection), and **open-struct-map rest rows** (a preserve rest row and a
  non-preserve rest row of the same `K`/`V` shape trip it through the SAME `map_shape_is_preserve_owned`
  seam that mints the rest wrapper). The rest-row
  instance is currently UNGUARDED (no fixture needs it, and the shape-clean `open-struct-map` snapshot
  fixture deliberately stays off the wasm-parity axis to avoid it). Standing rule meanwhile: keep the
  preserve/json open-map e2e fixtures off the wasm-parity sweep. Trigger for the mechanical layer — a
  real spec that puts a preserve map/table AND a non-preserve one of the identical `K`/`V` shape under
  `--wasm` (rest-row OR table). The layer is per-container-flavor wrapper minting (a distinct wrapper
  keyed on the backing-container flavor, so both faces coexist) OR a rest-row-side graceful collision
  detector — a parallel per-kind sibling of the existing wasm wrapper-name collision detectors (the
  AGENTS.md "parallel per-kind siblings, not one generic detector" rule), distinctly worded like the
  table-side coexistence rejection above.
- **An emission branch whose generated OUTPUT no fixture exercises is invisible to every gate —
  it can rot to uncompilable and hide behavioral bugs in both directions.** Proven instance
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
- **Value-anchor rot on field addition: a delivery that grows an existing type's fields leaves the
  type's existing hand-vector sweeps decoding the new data but asserting nothing about it — the
  EXECUTED-but-unasserted sibling of the never-executed emission-branch class above.** Proven
  instance (review-caught, not by any gate): the float presence-field delivery gave
  `array_opt_fields` (`tests/core`) real `x`/`z` presence fields, and the fixture's existing decode
  sweep — which already fed x/z-present bytes through `from_cbor_bytes`, and whose own comment
  states the discipline ("decode-accepts alone proved nothing — pin every field") — kept passing
  without asserting the new fields or exercising their serialize side in that long-optional-chain
  shape; closed by extending the sweep's anchors + round-trip in the review commit. The
  pending `cargo-mutants` sweep (§ "Pending maintainer action") covers this class only PARTIALLY:
  a generator mutant breaking the new behavior dies to any OTHER fixture asserting the same arm
  (here the corpus fixture's emitted tests), so mutation scoring cannot see that one hand suite's
  anchors went vacuous for its own distinct shape. Working rule meanwhile: a change that adds
  fields to a type appearing in an existing hand-vector suite extends that suite's value anchors
  in the same change, and the review walks the suite's assert list against the type's new field
  list. Mechanical layer on a SECOND instance: a per-suite anchor-completeness check (each
  round-tripped type's public fields ⊆ the fields its suite's asserts mention — buildable as a
  grep-level floor over `tests/*/tests.rs`), accepting its enumeration cost then, not before.
- **Regenerating over prior output with a rule DELETION is exercised as a gate for only two files,
  not corpus-wide — an emitted-comment-on-a-deletable-row trap in any OTHER generated file would
  ship unseen.** The comment-preservation overlay participates only when `export()` runs over
  prior on-disk output (it is applied to the in-memory file map ahead of the write loop), and its
  one corpus-scale gate (`comment_preserve_lexer_round_trip_over_corpus`) does SELF-preserve
  (`preserve(content, content)`), which is a no-op for any trailing comment regardless of whether a
  real regen would strand it — so it cannot see the sentinel-trap class at all. Proven instance
  (feature-requests 03/04, reproduced before fixing): `extern_interface_check.rs` and
  `key_demand_assertions.rs` emitted a per-row `// <cddl>` comment on every row; deleting the rule
  behind any row stranded that comment into a self-perpetuating `compile_error!` sentinel on the
  next in-place regen (and rustfmt's import reordering separately GLUED markers onto neighbouring
  rows). Standing coverage now: `extern_interface_check_regen_over_deletion_no_trap` (regenerates in
  place with a deletion, asserts neither sidecar gains a trap) and
  `extern_interface_check_has_no_trailing_row_comments` (a source-shape floor on the one file);
  emitters made banner-only. But those pin the two KNOWN files by name — a third generated file that
  grows a per-row comment is invisible again. Working rule meanwhile: no generated file emits a
  comment on a row a spec change can delete (comments live in fixed banners). Mechanical layer on a
  second instance: a corpus-wide regen-with-deletion leg (generate each corpus fixture, regenerate a
  rule-deleted variant IN PLACE, fail on any `cddl-codegen:unpreserved-comment` in the tool-owned
  trees) — the only layer that catches the class without knowing each file by name. Two shaping
  notes for that leg. First, a CHEAPER static floor can land ahead of it (or alongside): a
  corpus-wide scan of freshly generated tool-owned trees for a trailing `//` on any code row —
  single generation, pure text scan, no regen — which catches the trap SOURCE before any deletion
  exists, and is also the layer that falsifies a stale code-behavior premise loudly (the overlay's
  "generator emits no trailing comments" comment stayed load-bearing-but-false for as long as the
  markers shipped, because the two comment-preservation integration tests structurally cannot see
  that violation — `tests/README.md` § the preserve-fixtures section records which assumption is
  therefore enforced as an emitter invariant instead). Second, the leg only exercises
  `key_demand_assertions.rs` where a fixture carries a `@used_as_key` tag — the deletion-variant
  fixture set must include at least one, or that file's rows are vacuously green.
  SECOND instance on record (2026-07-22, consumer-reported, so the corpus-wide leg's trigger has
  FIRED and the leg is now DUE) — the regen-over-EDITED-output flavor: the import prune ran only
  over freshly-generated content while the overlay applied later, per file, at the disk-write seam,
  so a `cddl-codegen:replace` block that removed an import's last user shipped the orphaned `use`
  (CML's regen unused-import residue). Fixed by ordering — `export()` applies the overlay to the
  in-memory file map, then reruns the prune family-wide over the post-overlay map — with the two
  known shapes pinned (`comment_preservation_replace_orphans_import_same_file`,
  `comment_preservation_replace_in_descendant_orphans_parent_import`): the same
  known-shapes-by-name posture as the deletion instance's per-file pins, and the same residual
  blindness to a third shape. Consequence for the named layer's design: the corpus-wide regen leg
  must carry BOTH variant families — the rule-DELETED variant (fail on any
  `cddl-codegen:unpreserved-comment` in the tool-owned trees) AND a user-EDIT variant (a canonical
  replace block injected over a corpus fixture's own output) asserting the regenerated crate stays
  rustc-warning-clean (the `feature_corpus_compiles` unused-import/variable scan applied to the
  regen) and reaches a byte-identical fixed point on the following run. Cross-checked against the
  cddl-matrix ROADMAP at this instance too: its pending items enumerate CDDL input surface;
  regen-over-prior-output interaction stays this roadmap's domain.
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
  ("scanned at <sha>") or phrase it as the check to re-run, never as a standing fact. The
  SECOND-instance mechanical layer
  is now BUILT for that home: the findings-claims arm (`project_corpus.ts` check `I`, local tier)
  fails a failure-claim finding that names no resolvable tracking pin — current state, including
  its accepted looseness (a resolving citation does not validate claim SEMANTICS, which stay
  review-owned; any backtick token matching `src/tests/` text satisfies resolution — tighten on the
  first vacuous pass observed), is documented in `cddl-matrix/README.md` § "Gotchas". Still future:
  the USER-DOCS home has no arm — a parallel limitation-claims arm on `lint_doc_citations` (every
  known-limitation-classed paragraph outside a generated span carries a resolvable tracking
  citation, or folds into a projected span — the matrix north star's "more docs follow") remains
  unbuilt, since the two homes have different scanners and neither arm sees the other's prose.
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
  broken — the executed-but-wrong-shape sibling of the never-executed emission-branch class
  above.** Proven instance (consumer-reported, not by any gate): every raw-bytes fixture appended
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
  rather than a matrix row.
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
  deny only if it is currently green-able on both profiles (nursery lints carry known false
  positives). Act on a second instance or a consumer report, not before.
- **`unused_imports` on generated crates — residual trait-import class the name-scan model cannot
  reach.** The rustc-warning DETECTOR is live and BROAD — the generated-code unused-import scan
  (`unused_generated_import_lines`) inside `feature_corpus_compiles` fails on ANY `unused import`
  warning in the purely-generated crates, and (v3) its sibling `unused_generated_variable_lines`
  fails on ANY `unused variable` warning too, cache-key-versioned by the `lint=unused-imports-v3`
  marker (current state in `tests/README.md`'s description of that gate). The variables half was
  added on a consumer-reported instance (the constant count-arm `Some(x) => 1` binding): the corpus
  held the provoking shape all along (33 committed-snapshot instances), so the escape mechanism was
  a missing ASSERTED CLASS, not input poverty — `generated_code_clippy_clean` deliberately kept
  `unused_variables` at warn, lumping it with `unused_imports`, whose stay-warn rationale (the
  legitimate trait residue below) has no variables analogue. Follow-up on the next touch of that
  gate: add `-D unused_variables` to its rustc deny set (imports stay warn there — the trait
  residue is real); until then the corpus scan is the class owner. A second known DETECTOR blind
  spot, proven by the path-tail instance below: the scan lives only in `feature_corpus_compiles`
  cells, which never generate under the cross-crate workspace flags, so prune imprecision visible
  only in `--wrapper-requests`/`--workspace-dep` output (the requested-collections sidecar) reaches
  consumers before any gate — a detector-coverage cousin of the input-poverty sub-class ledgered in
  the flag-powerset entry (there the swept input lacks the shape; here the scanning gate lacks the
  flag). Named layer: apply the same unused-import/variable stderr scan to the nested `cargo check`
  output the workspace-requests/extern-deps e2e gates already capture — near-zero added cost, and
  it closes that family's blind spot without new cells. Proven instance of exactly that blind spot
  (2026-07-22, consumer-reported, fixed class-level): the used-ident scan counted `::`-path-tail
  segments (`cml_chain::assets::Coin` counting `assets`), which collide with the parent's `pub mod`
  defs, so `super_glob_needed` conservatively kept the sidecar's dead `use super::*;`; the
  path-tail exclusion in `collect_idents_in_tokens` removed the false-positive class (unit + map-
  level twin tests pin both directions). The entry's earlier escalation trigger
  (a 26-warning consumer report + chain/cip36 regen) fired and is delivered: `import_prune` now
  prunes the `use super::*;` and `use <common>::error::*;` globs when a fully-enumerable universe
  proves them unused, computes each file's protector set via the super-glob EDGE graph
  (`reachable_via_super`) rather than every structural descendant (which is what closed the root
  `mod.rs` over-breadth — a sub-scope `serialization.rs` no longer protects the root), adds the
  source-glob disqualifier (a descendant that globs the module the ancestor imports X from, the
  `--common-import-override` `serialization::*` shape), and name-scan-prunes an extended candidate
  set (the wasm prelude names, the `--wasm-*-macro` leaf names, and the cross-scope type idents
  `scope_references` over-imports). Still future-facing: the `cbor_event::se::Serialize` TRAIT
  import the non-canonical serialization prelude emits — a trait is exercised by a method call
  whose ident never appears, so name-scan cannot prove it unused; the detector's
  `UNUSED_IMPORT_TRAIT_RESIDUE` skips it. A per-file trait-need model (scan for the `.serialize(`
  method call / a bare `Serialize` bound) would remove it, but is deferred until the class recurs
  with a second unremovable trait. Also watched, warning-severity only (never a compile error):
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
  crossed with the wrapper-shape space, and no existing honesty rule sweeps it (the wasm-ABI
  matrix's SHAPES/ROLES cover what types look like and where they sit; the third honesty axis
  covers flag × input mode; neither enumerates flag × shape). Leg spec: each extern-capable shape
  probed once per mode (dep index listing its structural name / absent / workspace-dep configured /
  requested by a consumer sidecar), each probe crossed with reference POSITION (inline-anonymous vs
  NAMED-rule — the named-rule flavor takes struct-walk arms inline pins never touch), asserting the
  mode outcome (deferred-import vs local-mint vs borrowed+sidecar-recorded vs hosted+indexed) AND
  the shape row's IMPLIED COMPANIONS per mode (a table's synthesized keys-list:
  imported-from-dep vs root-minted vs co-hosted-locally), with a compile floor per mode (wasm32
  link consumer-side; `cargo check` of the host crate for the requested-hosted mode — one arming
  instance was observable only there). The user-rule cross-crate cell and the map-side NonEmpty
  source-routing cell join the leg rather than accreting per-shape hand fixtures. The leg doubles
  as the regression net for accidental-provider removals — a fix suppressing a walk path other
  positions silently relied on, a class fix-review reach-analysis asks about ("who else relied on
  this path?") but only a mode × shape × position compile floor answers mechanically. Arming
  evidence (the recur-first trigger is met; every instance was found by reading the emitters or
  reported by a consumer, never by a gate): after the NonEmpty first instance (below), one
  consumer-reported delivery produced two companion-wrapper cells — NAMED-table ×
  workspace-borrowed (a synthesized keys-list whose deferred import only the inline-map reference
  position ever registered: E0412 stranding plus a false criterion-9 shadow warning, entered when
  an alias-recursion suppression removed the walk route that had been registering the import;
  pinned by `workspace_dep_named_table_deferred_keys_list`) and requested-hosted ×
  co-hosted-keys-list (the host importing from root a class it mints itself: E0432; pinned by
  `workspace_requests_cohosted_keys_list_no_self_import`). Both were companion-wrapper failures at
  the named/hosted flavor, not primary-shape failures — which is why the leg crosses POSITION and
  asserts COMPANIONS rather than probing primary shapes alone. Closed context the leg builds on —
  the NonEmpty defer boundary: `generate_non_empty_array_type` / `generate_non_empty_map_type`
  consult `try_defer_wrapper`, their loose `try_from`-source mints defer normally (eager
  decisions, order-independent), and the source's conversion-internal import — invisible to the
  field walk — is routed at the restricted class's emission scope
  (`register_deferred_non_empty_{list,map}_source`, the same follow-the-class pattern as the R3d
  keys-list registration); pinned by `extern_wrapper_index_defers_to_dep`'s
  `[+ idx_foo]`/`NonEmptyIdxFooList` cell (whole-wrapper deferral) plus its order-hostile
  deferred-source cells: the unreferenced named rule `abc_bars = [+ idx_bar]` (walked first, only
  loose use in another module — RED as duplicate-symbol if the source re-mints, RED as unresolved
  `IdxBarList` if the import isn't routed) and the inline `only_nb_baz: [+ idx_baz]` twin (no
  loose use anywhere); the map-side source routing is the same helper pattern but has no dedicated
  cell until the leg lands. A separate open DECISION rides this entry: USER rules claiming a
  dep-indexed structural name split by flavor under `--extern-wrapper-index`, and neither flavor is
  fully right: a rule whose ident EQUALS the structural name passes the shipped name-identity
  guard for ARRAYS and silently DEFERS — the user's own class is suppressed in favor of the dep's
  (probed live during the workspace-mode delivery; same-shape so no link error, but the consumer's
  crate silently stops exporting its own authored class) — while TABLE rules are screened by
  `exists_in_rust` and stay local, where they duplicate-symbol at link if the dep also ships the
  name — the CROSS-CRATE flavor of the synthesized-name interaction class, which the shipped
  in-crate layers cannot see (the duplicate-ident backstop scans one crate's own files;
  `synthesized_name_interaction_sweep` spells no dep-index cells — see `tests/README.md`
  § "Synthesized-name interaction sweep + duplicate-ident backstop"), so it is owned HERE.
  Workspace mode already threads real rule provenance (`rule_declared`, from the
  `RustStructType::{Array,Table}` call sites — reading TRUE authorship via
  `is_synthesized_collection`, so a table rule's parse-time-synthesized keys-list never
  masquerades as rule-declared) and warns + keeps the user's class
  (`workspace_dep_defers_to_dep`'s shadowing cell); the open decision is whether the INDEX path
  should adopt the same guard — a behavior change to shipped semantics (today's array-flavor defer
  is at least link-clean), so it needs its own red-first cell, not a drive-by.
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
- **Generic RAW-BYTES base (`foo<T> = _CDDL_CODEGEN_RAW_BYTES_TYPE_`) emits the same uncompilable
  self-check / schema row an extern generic base did before its parse-time fix.** The
  `generic_extern_bases` record populated at parse time (which flips both the extern-interface
  `ExternCheckKind::None` decision and the json-gen schema-row skip) covers ONLY the EXTERN marker;
  the `RAW_BYTES_MARKER` parse branch drops its generic params the same way but is not recorded, so a
  generic raw-bytes base still emits `_assert_raw_bytes::<crate::generated::Foo>()` and a
  schema-registration row naming the bare `cddl_lib::Foo` (E0107 if the user's hand type is
  `Foo<T>`). Repro: `foo<T> = _CDDL_CODEGEN_RAW_BYTES_TYPE_` + any second rule, generate with
  `--json-schema-export=true --wasm=false`. Whether a generic raw-bytes type is meaningful at all
  (raw bytes carry no element type) is the open question — the honest fix is likely to REJECT the
  construct at parse time rather than record-and-skip it; deferred until a real spec needs it. When
  built, extend the parse-time record (or the rejection) to the raw-bytes marker and pin both call
  sites, mirroring `extern_interface_check_skips_generic_base_without_instances`.
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
  artifacts (a docs mention, a `[[cover]]` entry), not the mirror itself. Directive-SET drift now
  has a firing detector: `corpus_detect.ts`'s selfCheck lockstep tripwire demands set equality
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
  always; `linked-hash-map`/`serde`/`schemars` per flag) is hand-derived from grepping the static
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
- **A catalog row's per-gate ledger obligations are enforced only while the row is ACTIVE, so a
  pin→activate transition surfaces latent ledger gaps at a DISTANCE from the change that created
  them.** Proven instance (a real full-tier failure, but far from its cause): the `dsl.ignore`
  feature row and `dsl_ignore.ignored` corpus row shipped PINNED (decode vectors deferred to the
  close-out mint), so neither replay gate ever built their specs — the missing
  `PRESERVE_SKIP` entries (the `@ignore`-under-preserve rejection is BY DESIGN, needing the
  designed-rejection skip) stayed invisible through both WPs' green local tiers and failed the
  tier only when the close-out mint activated the rows. The annotation ALREADY encoded the fact
  the whole time (`emission.preserve.status = "unsupported"` landed with the feature row).
  Mechanical layer, cheap and one-directional (build on the second instance of any
  pin-masked-obligation flavor, or fold into the next replay-gate touch): a static cross-check
  in both replay gates — every catalog row (ACTIVE or PINNED) whose matrix annotation says
  `emission.preserve.status = "unsupported"` must have a `PRESERVE_SKIP` entry, checked without
  generating anything, so the gap fires at the WP commit that adds the row, not at activation.
  (Full derivation of `PRESERVE_SKIP` from the annotation verdict is NOT possible: the float
  rows' annotations say preserve-SUPPORTED — the matrix probe's bare-alias shape misses the
  float deserialize stub that the replay's example spec hits — so the hand ledger keeps owning
  the reasons; the cross-check is subset-direction only.) Confirmed since: each replay gate's
  `PRESERVE_SKIP` stale-guard asserts every listed id names an ACTIVE (vectored) row, so a
  `PRESERVE_SKIP` entry for a still-PINNED row FAILS the gate outright — a designed-rejection skip
  therefore CANNOT be pre-landed against a pinned row. The static annotation-side cross-check (which
  reads catalog + annotation without touching the skip ledger) is thus the only shape that fires at
  pin time; the alternative that keeps the ledger honest same-commit is to land the row ACTIVE with a
  hand-derived accept vector so its skip entry is immediately valid.

- **A rule-position directive reaches only the rule SHAPES whose parse path happens to carry a
  marking site, and a shape that bypasses every site is a SILENT drop — no gate covers the
  directive×rule-shape reachability product.** Two proven instances at the same seam, one of them
  twice: `parse_rule`'s `Rule::Group` arm reaches neither `parse_type` nor `parse_type_choices`, so
  `@rust_name` was silently dropped on plain groups until a marking site was added there, and
  `@no_json_schema_export` shipped inheriting exactly the same hole (a SPLICED plain group does
  register a rust struct and therefore does get a schema-registration row, so the directive was live
  and dead at once). Caught only by an orchestrator hand-probing a shape no fixture contained.
  `cddl-matrix/no_silent_directive.ts` cannot see this class for a JSON-surface directive: it
  generates rust-only, where the suppressed artifact does not exist, so every cell is byte-identical
  and allowlisted regardless of whether the marking site fires. Owned meanwhile by the
  twin-pair convention in `snapshot_tests::json_gen_extern_schema_rows` (each annotated rule paired
  with an unannotated same-shape control, under the flags where the effect is visible) plus the
  accepted/rejected shape table in
  `robustness_tests::no_json_schema_export_misuse_rejects_gracefully`. Mechanical layer, on the
  THIRD instance (or the next directive whose valid placement spans more than one parse path):
  enumerate the rule shapes once — type rule (single-choice, multi-choice, tagged, parenthesized),
  plain group, generic definition, generic instance, extern, raw-bytes — and drive every
  rule-position directive across the product, asserting each cell is one of {effect visible,
  loudly rejected}, never {accepted and inert}. The shape list is the expensive part and it is
  already written down: the sweep table in the `@no_json_schema_export` delivery's review thread.
  Four design points, each the answer to a way the obvious build would inherit the blindness it
  exists to remove:
  - **Each directive needs a WITNESS PROFILE — the minimal flag set under which its surface
    exists — and the sweep must generate under it, not under one fixed profile.** This is the whole
    reason `no_silent_directive` could not have caught the motivating bug: it hardcodes
    `--wasm=false` and nothing else, so a directive whose only surface is the json-gen crate is
    invisible to it no matter which cells its corpus holds. A per-directive profile is one small
    datum (`@no_json_schema_export` -> `--json-schema-export`; `@rust_name` -> the default; the
    `@duplicates` flavors -> the default) and it is what makes the verdict meaningful.
  - **Derive the directive axis from `KNOWN_RULE_METADATA_TAGS`, not from a hand list.** That
    constant is already the authoritative vocabulary and already carries a lockstep tripwire with
    `corpus_detect.ts`'s `MIRRORED_DIRECTIVES`. Deriving from it makes a NEW directive appear in the
    product automatically and demand classification — the forcing function a hand-picked corpus
    cannot provide, and the actual root cause of this class surviving two instances. The shape axis
    stays hand-enumerated; shapes change far more slowly than directives.
  - **Invalid (directive, shape) pairs need no validity matrix — they self-classify.**
    `@raw_bytes_flavor` on a record is already a loud rejection, which is a PASSING verdict under
    the three-way rule. So the product can be swept whole, and the only hand-curated artifact is the
    allowlist of legitimately inert cells, which doubles as the honest inventory exactly as
    `no_silent_directive`'s does today.
  - **Cost is generation-only.** Roughly (directives × shapes) × 2 one-rule generations, no nested
    cargo — the axis sizes put that near 300 generator invocations, so it belongs in `local`/`full`
    and should reuse `no_silent_directive`'s scratch-dir and byte-compare scaffolding rather than
    growing a second copy of it. The cleanest shape is to generalize that gate in place: it already
    owns the {byte-identical, acknowledged, allowlisted} verdict logic, and it is the file whose
    blindness this item is about.

- **The `T / null` -> `Option<T>` collapse reads a different metadata slot from every other
  type-rule path, so a rule-position directive written on such a rule is silently dropped.**
  `parse_type_choices`' optional-inner branch builds its `RuleMetadata` from the INNER type1's own
  `comments_after_type`, while its sibling branch (and `parse_type`) merge the rule-position slots a
  trailing `; @x` actually lands in. So `opt = null / uint ; @duplicates reject` generates exit-0
  with the directive dropped — confirmed empirically as the CURRENT behaviour for `@duplicates`, and
  the same for `@ignore` and `@no_json_schema_export`, all three of which have a rejection at that
  site that therefore never fires. Unlike the group-rule parser entry above this one IS fixable
  here: the comment is present in the AST, just read from the wrong slot. Owned meanwhile by
  nothing — the drop is invisible. The fix is to merge the same rule-position slots the non-optional
  branch merges before the branch splits, which turns each of the three directives into the loud
  rejection its site already spells; do it behind a vector per directive, since a spec relying on
  today's silence would start failing (correctly) at generation. Trigger: any consumer report of a
  directive "doing nothing" on a nullable rule, or the next directive added with a rule-position
  rejection.

- **A group rule's trailing comment is DISCARDED by the CDDL parser in two spellings, so every
  rule-position directive on a plain group is silently dropped there.** The parser leaves every
  AST comment slot `None` (verified by dumping the AST) when a group rule's closing paren is on its
  own line (`grp = (\n a: uint\n) ; @x`), and when the last group entry's own trailing slot is
  already occupied by a field-position `@name` — the two positions share that slot, and only one
  comment survives. Affects `@rust_name` and `@no_json_schema_export` identically; no extraction in
  `parsing.rs` can recover what the parser never recorded. Owned meanwhile by
  `group_rule_pin_metadata`'s doc comment and the `@no_json_schema_export` docs section, both of
  which name the single-line spelling as the supported one. The fix is upstream — and upstream here
  is the **dcSpark fork** of `cddl` pinned by git rev in `Cargo.toml` (version 0.10.6 at that rev),
  not the crates.io crate, so the patch lands in a repo we control. It would have the parser bind the
  comment to `Rule::Group`'s `comments_after_rule`, which today has exactly two construction sites
  (`pest_bridge.rs`) and both hardcode `None` — which is why reading that field from `parsing.rs` is
  dead code rather than a workaround. Design constraint the fix must respect, so it is not
  rediscovered: for the SUPPORTED single-line spelling the comment already binds to the last group
  entry's trailing slot, so populating `comments_after_rule` must be ADDITIVE — only for the
  spellings nothing else captures — or `group_rule_pin_metadata` double-counts and a field-position
  `@name` on the last entry stops renaming its field. Adopting the bump also needs a vector per
  affected directive, since a spec relying on today's silence starts behaving differently
  (correctly) after it. Build a local workaround only if a consumer
  spec cannot use the single-line form — a pre-parse source scan that attributes a trailing comment
  to the group rule by line position would be a second, drift-prone comment parser, so it needs a
  real consumer to justify it.

- **The schema document's name injectivity is enforced ROW-side, so the residue is everything the row
  set cannot see.** The json-gen crate's `schemas/<lib>.schema.json` is written by a program we emit,
  so its content is a property of the RUN, not of the emitted source, and every cheap verdict
  ("generates", "compiles", "the `.d.ts` type-checks") is satisfied by a document publishing one
  type's shape under another type's name. The emitted `add_schema` helper's name ledger, its
  kept-its-own-name check and its inline-branch conflict check close that for every collision with a
  row on the losing side, in the consumer's own `cargo run` (message wording and wiring pinned by
  `snapshot_tests::json_gen_extern_schema_rows`,
  `integration_tests::json_schema_name_merge_fails` and `..._stolen_fails`). A `--json-schema-root`
  extra root is emitted as an ordinary row through the same helper, so it inherits all three checks
  by construction; that inheritance is asserted by reading the emitter, not by a fixture with an
  extra root on the LOSING side of a collision, which would cost another nested-cargo failure cell —
  mint one if a consumer reports a collision they introduced through the flag. Three holes remain,
  each needing its own mechanism:
  - **A collision whose LOSER has no row and whose `schema_id`s match** is a silent merge nothing can
    see: the ledger only holds rows, and the merge makes both returned refs equal the shared name, so
    the kept-its-own-name check reads clean. Reaching it needs a way to enumerate what a row pulls in
    transitively — a post-pass over the finished document comparing `$defs` cardinality against the
    reachable type set, or an upstream `schemars` setting that rejects two `TypeId`s resolving to one
    `schema_id`. The upstream lever is report-and-wait (`schemars` is a crates.io dependency, not a
    fork we pin like the `cddl` parser), so the document post-pass is the schedulable one.
  - **A cross-crate collision** — two crates' `add_schemas` threaded into one `SchemaGenerator` —
    escapes the ledger, which is a local of one crate's `add_schemas`. Widening it means either a
    published ledger type in the static runtime (a new cross-crate API surface) or making
    `add_schemas` take the ledger, which would break the composition-point signature a consumer was
    told to call. Neither is worth doing before a consumer actually composes two documents; the
    kept-its-own-name check still fires whenever the loser is a row of the crate being checked.
  - **A `schema_name()` that `schemars` percent-encodes into its `$ref`** (anything outside
    `[A-Za-z0-9_]`, e.g. the static runtime's `OrderedHashMap<K, V>`) skips the kept-its-own-name
    check entirely: `schemars`' `encode_ref_name` is private, so reconstructing the expected ref means
    duplicating an upstream encoder — a false panic in a consumer's build is worse than a missed
    check. Closing it needs the encoder exposed upstream, or a decode step in the emitted helper
    validated against a vector per escape class.
  - **A collision between two types that BOTH lack rows** — each reached only transitively from some
    row'd type — is seen by neither check: the ledger holds rows, and "kept its own name" is asked
    only of a registered type. With distinct `schema_id`s both are emitted and one takes `<name>2`
    by first-encounter order, so an unrelated spec edit (a rule that sorts earlier, a reordered
    struct field) can silently SWAP which published TypeScript name belongs to which type. The
    byte-identity assertion cannot see it — it compares two runs of ONE spec, never two versions of
    a spec. Same closing mechanism as the no-row-loser hole above (a post-pass over the finished
    document); making the ordering irrelevant rather than merely loud would mean a published-name
    scheme that does not depend on traversal order at all — a breaking rename of published names in
    its own right, so it needs a consumer asking for name stability across spec edits.

  The document's reference CLOSURE — every `$ref` resolving to a `$defs` key of the same document —
  is asserted for every `--json-schema-export` fixture by `integration_tests::run_test`, which is OUR
  suite only: a consumer's own run still has no closure check, and the shape that needs one is a
  hand-written impl returning a bare `Schema::new_ref("SomeType")` (the `@json_schema = "<file>"`
  ask). Emitting the closure walk into the generated `export_schemas()` is the layer that would reach
  them. Trigger: a consumer report of a wrong-bodied published schema, a document composed from two
  crates, or the hand-authored-schema ask landing.

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

## Deferred features (build when a real consumer needs them)

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
  such rules exclude-with-record (reopening signal: same, for a float32-windowed alias).
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
  `cbor_event = "3.2.0"`, and nothing gates what that range actually resolves to over time.** In
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
  too, pinned by `top_level_ranges` in `tests/core`. (Separately, `foo = undefined` still panics
  — a distinct gap, unsupported cddl-prelude `#7.23`, not the `Fixed`-member path; ledgered by
  `tests/matrix_panic/prelude.undefined.cddl`.)
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
  in `tests/README.md` § "Transparent tag-set idiom". Four boundary shapes stay unsupported, each
  with its reopening signal (a real consumer spec hitting it):
  - *Non-idiom choice-BODIED generic defs still crash generation.* A collection-bodied generic def
    whose two arms do NOT satisfy the collapse condition (e.g. mismatched bounds
    `xs<a0> = #6.258([+ a0]) / [* a0]`, instanced + used) panics during generation
    (`Option::unwrap()` on `None` at the `Rust(rust_ident)` arm of `encoding_fields` in
    `generation/mod.rs`) rather than rejecting gracefully. Pre-existing and out of REQUEST-08's
    scope: the collapse fires at parse time BEFORE the generic machinery, so the RECOGNIZED idiom
    never reaches this; only a non-collapsing choice-bodied generic def does. Remedy when it bites:
    parse-time graceful rejection of a choice-bodied generic def that is not the idiom.
  - *Alias-of-instance chains don't compile.* `bar = xs_int` where `xs_int = xs<uint>` is itself a
    generic collection instance emits `pub type Bar = XsInt;` plus use-site `self.x.serialize()` /
    `Bar::deserialize()` — methods a transparent `Vec` alias lacks (verified: generates, does not
    compile). Phase 2.5's field-convergence walk resolves a DIRECT instance field and a nested
    collection-of-instance ELEMENT (`[* xs_int]` is inlined correctly, both directions), but an
    alias-of-an-instance is a second hop it does not follow. Remedy when it bites: extend the walk
    (or `resolve_alias`) to chase an alias whose target is itself a structural collection instance.
    A REJECT-instance-ELEMENT flavor of the same boundary is now concretely pinned: embedding a
    named array rule whose element is a reject-flavored instance (`outer = [* oset<uint>]` used as
    a member) inlines the array loop but leaves `element.serialize()` /
    `OsetU64::deserialize()` calls on the impl-less transparent `OrderedSet` alias — the
    decode-conformance row `tag_set_reject_anon_generic.outer` is `pinned_reason`-pinned on it
    (the fixture itself compiles; only an EMBEDDING of the rule materializes the calls).
  - *Inline/anonymous two-arm choices are not recognized.* Recognition lives at the
    `parse_type_choices` named-rule seam, so an inline `[x: #6.258([* uint]) / [* uint]]` stays a
    two-variant enum. Remedy when it bites: run the recognition on anonymous choices too.
- **Named bytes-element collection aliases generate a wasm wrapper class that fails E0271
  (pre-existing, not REQUEST-08-specific).** A named collection over byte-string elements
  (`Vec<Vec<u8>>` / `NonEmptyVec<Vec<u8>>`) mints a `#[wasm_bindgen]` list-wrapper class exposing the
  `Vec<u8>` element across the wasm ABI, and the generated wasm crate does not compile:
  `error[E0271]: type mismatch resolving <Vec<u8> as ErasableGeneric>::Repr == JsValue`
  (wasm-bindgen's `VectorFromWasmAbi` needs JsValue-convertible elements; a bare `Vec<u8>` element is
  not). Reproduced two ways, both independent of the tag-set feature: (A) worktree binary, a
  non-generic collapsed `nes = #6.258([+ bytes]) / [+ bytes]` + holder — wasm crate → E0271; and
  (C) the MASTER binary at `3bdcbd3` (pre-series), a single-arm `nes = #6.258([+ bytes])` + holder —
  wasm crate → same E0271. So the wasm wrapper for ANY named bytes-element collection alias has never
  compiled; the tag-set corpus fixture was merely the first corpus entry to exercise it under the
  wasm profile (the `tag_set_generic` fixture now omits its bytes instance; the RUST-side bytes
  collapse — incl. per-element `StringEncoding` preservation, the `@raw_bytes_flavor`-moot finding —
  is pinned in-process by `optional_tag_set_tests::bytes_element_set_collapses_with_elem_encodings`,
  which does not compile wasm). Reopening signal: a consumer wanting a bytes-element set across the
  wasm boundary — CML's raw-bytes sets used extern HAND wrappers (its `NonemptySetRawBytes` wasm
  glue), which is exactly why this never surfaced there. Remedy when it bites: in the wasm
  list-wrapper minting machinery, expose the byte-string element through its own wrapper class or a
  `js_sys::Uint8Array`-based ABI instead of a bare `Vec<u8>` element.

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
- **Type-check the merged `.d.ts` with `tsc --noEmit`.** The JS-side pipeline
  (`js_schema_to_ts` / `js_d_ts_merge` / `package_json_pipeline`) asserts on *substrings* of the
  emitted TypeScript, so it can only catch the wrongness it was told to look for; a real
  type-checker over the merged file is the oracle that catches the class as a class (dangling
  names, duplicate declarations, malformed unions). Deferred on cost, not on value: it adds a
  TypeScript dependency and its install weight to `package_json_pipeline`, already the heaviest
  JS-side gate. Two traps to build it with, so the follow-up does not rediscover them: (1)
  `--skipLibCheck` makes the check VACUOUS here, because the file under test *is* a `.d.ts` —
  the flag must stay off, which also means every type the file references must resolve; and (2)
  TypeScript must be ≥ 5.2 with `--target esnext`, because wasm-bindgen emits `Symbol.dispose`
  members that earlier targets do not know. Reopening signal: a shipped `.d.ts` breaks a consumer's
  build in a way the substring asserts could not see.
- **A file listing `--json-schema-root` values, instead of one flag per root.** The repeatable flag
  is what shipped, on the grounds that it matches every other repeatable flag and that the asking
  consumer's eight entries do not justify a new file format. A file-listing variant stays purely
  additive (same emission, a different way to spell the list), so it is a cost question a real
  consumer settles: build it when someone's root list is large enough that a command line stops being
  the right place for it. Note what such a file must NOT become — the paths are Rust, not CDDL, and
  routing them through the spec is the category error the flag exists to avoid.
- **Assert a hand-authored JSON schema against the type's actual serialization.** Supplying a
  schema body by hand needs no new spec syntax: `@custom_json` suppresses the `serde`/`schemars`
  derives while the type still gets a registration row, so a hand-written
  `JsonSchema::json_schema` returning the real body (e.g.
  `serde_json::from_str(include_str!("…"))`) lands in the document as an ordinary local `$defs`
  entry through the normal route. The missing half is the one with the value: a hand-written schema
  drifts from the serialization it claims to describe and nothing notices — the reported consumer
  failure is a hand-maintained schema that had been wrong in two places for years. The assertion
  SHAPE already exists, hand-authored per fixture: `tests/json/tests.rs`'s
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
- **Threading a dependency's `add_schemas` into a consumer's document.** Genuinely blocked on the
  requester, not on design: the tool does not know a dependency's *json-gen crate name or path*.
  `--extern-wasm-crate` / `--workspace-dep` establish the rust and wasm crate identities; the
  json-gen crate is a third artifact whose existence is not implied (a dependency may have none at
  all), and wiring it would need a new dependency edge in the consumer's json-gen manifest written
  against a layout convention the tool would have to invent. What unblocks it, all workspace-layout
  facts rather than design choices: how a consumer's json-gen crate should FIND a dep's json-gen
  crate (a flag carrying `<dep-rust-crate>=<dep-json-gen-crate-name>@<path>`, or a convention plus
  an opt-in flag), and whether a dependency with no json-gen crate is a hard error or a skip. What
  it is worth is now narrower than when it was asked: with one document per crate, anything a
  consumer's own types reference is already in the consumer's document through the closure, so the
  residue is only a dependency's UNREFERENCED roots — and those are already expressible as
  `--json-schema-root=<dep_crate>::<Type>` plus a hand-added dependency in the merged
  `wasm/json-gen/Cargo.toml`. The entry stays because that route makes the consumer restate each root
  by hand, where threading would import the dependency's own list; it remains blocked on the same
  workspace-layout facts. `add_schemas` is emitted `pub` precisely so this stays additive.

## Operational watches

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
  35 GB. The missing system: scratch dirs should be self-retiring — a run-start sweep of stale
  entries under a recognizable namespace (age-based, live-process-guarded), or a bounded named
  scratch root the tiers reuse and truncate. Until built, treat unexplained ENOSPC/mid-gate
  deaths as possible scratch-debris saturation and sweep before re-attributing. Two corroborating
  sightings from the concurrent session, same saturation window: a first-ever generation run
  exiting 1 whose only capture went through `tail -3` (evidence burned — the exact failure mode
  the evidence-preservation rule names; green on immediate rerun, attribution now unrecoverable
  but consistent with a full `/tmp`), and a local-tier run failing three nested-cargo gates on
  os error 28 mid-saturation (all green on the post-remediation rerun; its full log was then
  destroyed with its worktree — the per-checkout `draft/logs/` lifetime note in AGENTS.md).
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
  sentence whose cited pin exists but whose claim about it is wrong. NB this decision covers PROSE
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
- **Documented-example execution sweep for `command_line_flags.mdx`.** A documented flag pairing
  can be broken while every fixture stays green when the fixtures satisfy its preconditions by
  accident: the doc's own `--extern-wasm-crate` example
  (`--common-import-override=cml_core --extern-wasm-crate=cml_core=cml_core_wasm`) panicked at
  startup validation for every PURE consumer (override crate not also a declared extern dep) from
  the `Int`-override feature's ship until a consumer report, because `extern_deps_wasm`'s override
  crate doubles as a declared dep and so satisfied the key check by accident — the exact cell the
  doc example describes was never executed (found by reading the fixture, not by any gate; now
  pinned by `common_override_wasm_int`). Systematic layer: a sweep that executes each
  `command_line_flags.mdx` example invocation verbatim against a minimal spec assuming only what
  the example's own prose states, asserting generation exits zero — so a doc-example/validation
  divergence fails a gate instead of shipping prose-only. Complements the class-level
  validation-smoke sweep above (that one asserts documented-INVALID values reject; this one asserts
  documented-VALID pairings run).
- **Negative failure-SHAPE vectors + the fixture-`Int` mirror gate for the cross-crate `Int`
  channel.** The shipped `Int`-under-`--common-import-override` coverage proves the positive paths
  and asserts row-ABSENCE on the degraded path, but none of the three documented failure shapes is
  compile-proven by a standing cell: (a) the consumer's `_borrowed_key_types_self_check` going RED
  when the common crate's `Int` lacks the demanded flavor (needs a de-flavored fixture variant);
  (b) the unresolved-import failure when the common crate exposes no `Int`/`IntError` (the
  existence contract's loud half); (c) `E0277` at the consumer's own map sites when the override is
  not a `--workspace-dep` (today only the no-row half is asserted, by
  `int_key_via_common_import_override_sidecar`). Each is the documented contract in
  `command_line_flags.mdx`'s `--common-import-override` section — a contract whose failure shape no
  gate exercises is prose-only. Related mirror exposure to close with (a): the
  `tests/extern-dep-crate` hand-written `Int` mirrors generated `Int`'s preserve-encodings wire
  format and encoding-insensitive key semantics, but the `extern_deps` round-trips serialize AND
  deserialize through the fixture's own impls, so a generated-`Int` divergence passes silently —
  unlike the `tests/wasm-macro-crate` mirror, which compilation enforces. A cheap drift gate:
  cross-serialize one vector between a generated `Int` (any no-override cell's output) and the
  fixture's, asserting byte equality both ways. The sweep is
  string-level by design, so a cell can be GREEN while the emitted crate is broken: the
  `@custom_json` record-struct cell first landed passing while the generated struct kept its
  serde derives against a now-unskipped `encodings` field (E0277) — caught only by a hand-run
  end-to-end repro during review, not by any gate. The landed mitigation is paired anchors
  (the cell asserts the derive line AND the skip attribute, on both the directive and control
  sides), which covers known failure modes only; a `cargo check` probe over the cells whose
  expectation is compile-relevant would close the class. Weigh against the sweep's cheapness
  (it currently runs in ~1s; a compile leg is nested-cargo-priced and would need the gate
  cache).
- **`quickcheck` alongside `proptest`; `goldenfile`/`expect-test` as a second corpus engine;
  `no-panic` lints; coverage instrumentation of *generated* code; `trybuild` for whole-crate
  compile-pass** (the corpus `cargo check` is simpler and broader).
- **An orphan-fixture-directory meta-test** (assert every `tests/<dir>/` is referenced by some gate):
  fixture dirs change rarely and a new gate's author touches the dir listing anyway; the failure
  mode (a committed fixture nothing runs) is caught by review at that rate.
- **Assertions on `export()`'s stderr diagnostics** — the legacy-root warning and the
  comment-preservation stale-file scan (an orphaned `.rs` under a generated tree) print via
  `eprintln!` in-process, which `cargo test` can't capture without a subprocess harness. The
  behaviors' *output-byte* halves are pinned (seed-once, overlay tests); only the warning text
  itself is unasserted. If either diagnostic grows logic, run the CLI as a subprocess (the
  `run_test` pattern) and assert on captured stderr.

## Sources
- Full exhaustive menu (24 ranked items + blind spots): `draft/testing-recommendations/RECOMMENDATIONS.md`
- Per-dimension expert write-ups: `draft/testing-recommendations/*.md`
