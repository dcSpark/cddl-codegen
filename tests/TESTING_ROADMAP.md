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
load-bearing: some constructs (`any`, `float16`, socket plugs, …) are design decisions to *exclude*,
not holes to grind toward 100%.

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
  transitively via the proc-macro derives). Lower urgency only because the pinned toolchain already
  mitigates churn.

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
     home profile — how this instance actually surfaced). Related but decided: the observed-baseline
     comments next to each gate's floors are informational and review-maintained (a stale one
     shipped once); if one misleads triage again, replace them with exact pinned counts asserted by
     the gate, accepting the churn on every ingredient addition — the floors stay the enforced
     artifact either way.
   - **Real-world corpus differential** (see `draft/testing-recommendations/RECOMMENDATIONS.md`):
     synthetic breadth vs real-world depth — recombination does not replace it.

2. **A flavored-borrow workspace fixture (`@used_as_key hash`/`ord` across the `--key-requests`
   channel).** The flavor legs of the map-key-derive channel are pinned at the unit layer
   (`key_types_accepts_flavor_column` / `key_types_rejects_unknown_flavor` in
   `src/wrapper_requests.rs`, plus the emitter's conditional three-column form), and the all-bare
   workspace fixtures prove byte-identity — but no workspace fixture yet runs the flavored
   contract end-to-end: a consumer whose spec tags a dep extern with a flavor emits the
   three-column row + per-flavor self-check, the dep's `--key-requests` regen derives exactly the
   named family (a hash-only borrow must NOT force `Ord` through the dep's fields), and both
   crates compile against each other. The compiled cross-crate seam is what the unit tests
   structurally can't see — the same rationale that justified
   `workspace_key_requests_derive_effect_and_hard_errors` for the bare channel. Extend one of the
   existing `tests/workspace-*` fixtures (or mint a sibling) rather than a new gate shape; user
   docs for the format are `docs/docs/output_format.mdx` § Workspace mode and
   `docs/docs/comment_dsl.mdx` § `@used_as_key`.

## Standing-system residuals (recur-first)

Each entry here is a ledger record for a proven-once failure class: what happened, which standing
system (or working rule) owns it meanwhile, and the trigger — usually a SECOND instance — that
justifies building the named mechanical layer. Building before the trigger fires is deliberate
over-engineering; deleting an entry without either building the layer or recording why the class is
dead loses the lesson.

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
  reach — one proven instance recorded, no machinery yet.** Review of the wasm burn-down
  retirement (dropping the identity `.into()`) exposed `Holder::new(val.clone())` in a wasm
  ctor — a clone of an owned, last-use argument (the boundary ops clone every non-Copy expr
  regardless of the call site's ownership). `clippy::redundant_clone` would flag it but is
  nursery-tier, so the gate's `-D clippy::all` cannot see the class; behavior and bytes are
  unaffected (allocation cost only). Disposition mirrors the curated rustc style-deny precedent
  (`unused_parens` et al.): evaluate specific beyond-`all` lints one at a time, adding a per-lint
  deny only if it is currently green-able on both profiles (nursery lints carry known false
  positives). Act on a second instance or a consumer report, not before.
- **`unused_imports` on the pruned collection-type imports — no rustc-warning gate yet.** The
  usage-derived prune (`import_prune::prune_generated_files`, run once over the whole file map in
  `generation/export.rs`'s `generated_files`) removes the four blindly-pushed collection imports
  (`BTreeMap`/`OrderedHashMap`/`NonEmptyVec`/`NonEmptyMap`) that a file's module family — the file
  plus its strict path-descendant modules, the complete set of possible consumers of its private
  imports — names nowhere. An over-prune that removes a needed import is caught LOUD by the
  existing nested-cargo compile gates (E0412/E0433 — exactly what caught the first per-file
  prototype); the uncovered direction is warning-severity residue: a future under-prune, or the
  model's one remaining imprecision — a file is protected by ALL its descendants, including ones
  that never actually glob-chain (`use super::*;` at each level) back to it, so a descendant using
  a type through its own direct import can keep an ancestor's copy alive. Expected residue at
  today's emission shapes is near zero (children reach parents via `use super::*;` uniformly). The
  mechanical layer is a nested-cargo check failing on an `unused_imports` rustc warning whose ident
  is one of the four allowlisted names (`cargo build --message-format=json` filtered to
  `unused_imports` diagnostics naming an allowlist ident); it would also measure the actual residue.
  Build it if it slots into an existing nested-cargo gate in ≤ ~30 lines. Exact glob-EDGE tracking
  (protect only via descendants that actually glob-chain to the file) replaces the descendant
  closure only on a real warning report — not before.
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
- **`--extern-wrapper-index` deferral-boundaries — per-wrapper emission MODE × wrapper shape is an
  unswept axis.** The NonEmpty defer boundary is now closed: `generate_non_empty_array_type` /
  `generate_non_empty_map_type` consult `try_defer_wrapper`, their loose `try_from`-source mints
  defer normally (eager decisions, order-independent), and the source's conversion-internal import —
  invisible to the field walk — is routed at the restricted class's emission scope
  (`register_deferred_non_empty_{list,map}_source`, the same follow-the-class pattern as the R3d
  keys-list registration). Pinned by `extern_wrapper_index_defers_to_dep`'s
  `[+ idx_foo]`/`NonEmptyIdxFooList` cell (whole-wrapper deferral) plus its order-hostile
  deferred-source cells: the unreferenced named rule `abc_bars = [+ idx_bar]` (walked first, only
  loose use in another module — RED as duplicate-symbol if the source re-mints, RED as unresolved
  `IdxBarList` if the import isn't routed) and the inline `only_nb_baz: [+ idx_baz]` twin (no loose
  use anywhere). The map-side source routing is the same helper pattern but has no dedicated cell —
  it joins the deferral-profile leg below. Two related gaps stay open. (1) USER rules claiming a
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
  `RustStructType::{Array,Table}` call sites) and warns + keeps the user's class
  (`workspace_dep_defers_to_dep`'s shadowing cell); the open decision is whether the INDEX path
  should adopt the same guard — a behavior change to shipped semantics (today's array-flavor defer
  is at least link-clean), so it needs its own red-first cell, not a drive-by. (2) The CLASS behind
  all of these — a per-wrapper emission MODE (local vs index-deferred vs workspace-borrowed under
  `--workspace-dep`) crossed with the wrapper-shape space — is an axis no existing honesty rule
  sweeps (the wasm-ABI matrix's SHAPES/ROLES cover what types look like and where they sit; the third
  honesty axis covers flag × input mode; neither enumerates flag × shape). Each has been found by
  reading the emitters, not by any gate. Mechanical layer on the SECOND read-caught or
  consumer-reported instance of the class (the NonEmpty cell above being the first, now pinned by
  hand): a deferral-profile leg over the wasm-ABI matrix's extern-capable shapes — each shape probed
  once per mode (dep index listing its structural name / absent / workspace-dep configured),
  asserting deferred-import vs local-mint vs borrowed+sidecar-recorded and a wasm32 link (the
  user-rule cross-crate cell and the map-side NonEmpty source-routing cell join it) — rather than
  accreting per-shape hand fixtures.
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
  over the extern fixture. (b) json-gen against extern-dep types — `gen_json_schema!` now emits the
  dep's rust path, but no gate generates or executes `--json-schema-export` with extern deps (needs
  the dep in the json-gen manifest and a `schemars::JsonSchema` impl on the dep type, both user
  responsibilities by design).
- **Twin in-repo implementations of one semantic decision drifting apart (emission spellings,
  detection walkers) — two same-cycle read-caught instances recorded, single-owner extraction is
  the fix pattern, no detector machinery yet.** Both instances are the in-repo sibling of the
  parallel-constructor residual's predicate lesson above ("when a fix needs the same decision an
  existing function already makes, extract the decision, never mirror it") — but where that entry's
  scope is parsers over EXTERNAL input, this class is two implementations of one decision both
  living in `src/`, drifting apart while every behavioral oracle stays green. (1)
  `generate_wrapper_struct` hand-built its range-check `cond`/`failure_expr` beside the shared
  `bounds_check_if_block`/`range_check_err` helpers and drifted three spellings apart (`!= N`
  collapse, zero-min lower-leg elision, an inline float-payload duplicate) — semantically
  IDENTICAL emissions, so decode-conformance (equal accept/reject sets), the mutation sweep (no
  behavioral delta to detect), and the compile gates were all blind by construction; caught only
  by review reading. Fixed by making `bounds_check_if_block` + `range_check_err` the single owner
  (`non_negative`/`location` params), pinned by the `bounds_spellings` corpus fixture. (2)
  `uses_non_empty_{vec,map}` hand-walked the `RustStructType` variants, mirroring
  `visit_all_rust_types`' position enumeration, and drifted two legs short (named-array
  `element_type`, table `domain`) — MASKED by the transparent alias every top-level array/table
  rule co-registers, so even the nested-cargo compile gates that own the active flavor of this
  bug (a missing `NonEmptyVec`/`NonEmptyMap` import, E0433) could never fire; also read-caught.
  Fixed by folding the canonical visitor, corner shapes pinned by `nonempty_nested_positions`.
  Pending-system coverage check (done at recording time): the grammar-fuzzer escalations own only
  the ACTIVE missing-import flavor (a recombination shape table extended to nested container
  positions would compile-fail it); NO pending item sees semantically-equivalent drift — that
  flavor has no oracle but reading. Two residuals recorded with the instances: (a) the same
  authored zero-min bound still REPORTS differently by site (`min: Some(0)` in wrapper
  RangeCheck payloads vs `min: None` at member sites) because the drop-the-redundant-zero
  decision itself has multiple owners — `RustType::with_bounds` normalizes at IR build (member
  channel) while the wrapper's `min_max` channel bypasses it, and parsing's occurrence filter
  plus `classify_sign_arm`'s uint-arm filter spell the same decision in their own domains;
  unifying the payload is a user-visible error-message change needing its own reviewed change,
  not a drive-by. (b) `bounds_check_expr_non_negative`'s Array/Map leg and the member-side
  zero-min elision are DEFENSIVE-ONLY today — every zero lower bound is dropped upstream by (a)'s
  owners before emission, so the corner is unreachable end-to-end and a fixture cannot pin it;
  recorded here so it isn't re-litigated as a coverage gap. Mechanical layer on the NEXT
  read-caught instance of the class: a same-construct differential sweep — emit one
  bounded/flagged construct per site class (member ctor/setter, wrapper `new()`/deserialize,
  primitive-deserialize `.and_then`, collection len check) and diff the check conditions +
  failure payloads, ledgering deliberate site-specific differences — plus the cheap grep half, a
  snapshot-wide wart scan (same-N `< N || > N`, dead `< 0` over unsigned/`len()` exprs) in the
  spirit of the doubled-doc-marker scan above.
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

## Deferred features (build when a real consumer needs them)

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

## Operational watches

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
    (`multifile_matrix_compiles` measured ~35 s cold / ~30 s warm at 43 cells; 62 at HEAD).
- **Registry-fetch transients in nested-cargo cells.** The same nested-cargo gates as the
  gate-cache watch above (`feature_corpus_compiles`, `wasm_matrix_compiles`,
  `multifile_matrix_compiles`) each resolve `cbor_event` from crates.io per temp cell, so a flaky
  network/proxy fails otherwise-green local-tier runs with `unable to update registry crates-io`
  on a DIFFERENT cell each run (three consecutive local runs, 2026-07-12, all this signature; every
  affected gate green in isolation; root-caused the same day to the local proxy aborting roughly
  1-in-6 CONNECTs to index.crates.io). The shifting-cell + registry-error signature distinguishes
  it from a real red cell at a glance; isolated re-run of the named gate is the confirm, and
  `CARGO_NET_OFFLINE=true bun run check.ts` is the clean-confirm when the rate makes consecutive
  online runs impractical (deps are already in cargo home after any warm run). The class also
  reaches verify.ts EVIDENCE (not just gate exits): a transient null replay flips a row's
  decode-foreign clause to "FAILED", which `verify_cache_transparency` reads as an A/B divergence —
  absorbed since 2026-07-12 by decodeForeignProbe's regenerate-retry-once (the same retry the mint
  paths carry); the transparency gate itself is the standing detector if the class outruns the
  retry. If the rate bites harder, the mechanical hardening is removing the per-cell network
  dependency: run nested `cargo check`/`test` with `--offline` after one warm-up fetch (the shared
  `CARGO_TARGET_DIR` and cargo home already hold the deps), or vendor the handful of
  generated-crate deps.
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
  `integration_tests::getting_started_example`.)
- **Full `2^N` flag powerset / PICT pairwise** — the curated named profiles cover the flag
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
- **A compile leg for compile-relevant `dsl_position_tests` Effect cells.** The sweep is
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
