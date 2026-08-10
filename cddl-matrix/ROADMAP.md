# CDDL master matrix — roadmap (remaining work + findings)

Read `README.md` first (the model + current state, including the durable gotchas and the
upstream-oracle-gap state). This doc is strictly the FUTURE state: matrix-side work, the
open-findings ledger, upstream close-outs, and pending decisions. The blow-by-blow of *how* done work landed lives in git history (this
doc is actively pruned of it; the project already did the same with the scale report + cold-critique).
Running the gates is not a roadmap concern either: `check.ts` at the repo root is the self-enforcing
gate registry + entry point, `tests/README.md` § "Running everything" is the prose overview, each
script's header docstring is the per-gate detail, and `QUERIES.md` documents the Q1–Q6 query scripts.

One authoring rule for this file: a sentence asserting COVERAGE — "gate X exercises Y", or the
negative "nothing exercises Z" — must name the pin/gate it rests on (for a negative claim, the
enumerated registry or mechanism it was established against). A named identifier dangles greppably
when its referent moves and `lint_doc_citations` asserts it resolves; an unnamed coverage claim
reads as fact indefinitely — a "the json-gen runner gate is single-file-only" sentence here
outlived the contrary always-on test (`multifile_json_preserve`, executing json-gen over
cross-module input since 2023) until a pickup probe caught it. Pickup re-probing stays the
enforcement (a prose rule cannot enforce itself); naming the pin is what gives the probe a target
and the reader a checkable referent.

**Status: gate-green.** <!-- status-header counts are generated — regenerate with: cd cddl-matrix && bun run project_status_headers.ts --write --><!-- gen:sh:roadmap-counts -->123 features (95 RFC8610 + 1 RFC9682 + 27 `CDDL_CODEGEN` vendor profile), 136 containment cells, and 293 cddl-codegen annotations<!-- /gen:sh:roadmap-counts -->, all axes reconciled/deterministic, with
execution-gated support **per-feature, per-cell (role × feature), and per-control-op** (<!-- gen:sh:roadmap-ops -->all 37 IANA ops probed<!-- /gen:sh:roadmap-ops -->):
"supported" requires the generated crate's `--emit-tests`
round-trip/reject tests to PASS (`cargo test`), falling back to the compile verdict only for shapes that
mint no test surface (recorded honestly in the evidence). The orthogonal **emission axis is filled**
(every default-supported row carries a `preserve`/`json` verdict; <!-- gen:sh:roadmap-emission -->1 divergences, all `preserve`-side<!-- /gen:sh:roadmap-emission --> —
see § findings) and supported rows carry decode-foreign corroboration clauses (plus <!-- gen:sh:roadmap-constraint -->105 `class="constraint"` enforcement reject vectors over 85 enforce-green rows<!-- /gen:sh:roadmap-constraint --> — the enforcement
axis carries three temporarily unverified fixed-byte rows and NO certified over-acceptances at HEAD:
every other supported row with a rejectable constraint projects `enforce = yes (bounded-reject)`;
the pinned rust-cddl validator panic prevents the two fixed-byte member rows and their top-level
feature row from gaining independently certified reject vectors until upstream gap #17 closes.
`+`/`1*` is honored as a non-empty container and the other count-permitting table markers
are rejected gracefully (§ findings); the green, three-row unverified, and over-accepts (empty) sets are
each pinned exactly by `query_q4_directional.ts --check` (the over-acceptance vector class stays armed
for the next certified instance).
Four projections GENERATE their hand docs and drift-check: `golden_hex` (encoding axis, Q3), the
`corpus` projection (feature axis Q2 + per-cell **role × feature** coverage), `query_q1_gaps.ts`
(the `## Limitations` section of `docs/docs/current_capacities.mdx`, Q1), and
`project_status_headers.ts` (the countable status-header prose in these docs + `tests/README.md`,
including this paragraph's own counts). **Every consumer query
Q1–Q6 is answered by a standing script** (`QUERIES.md` § "Definition of done").

> **North star.** Do the hard per-construct coverage work **once** and *project* it into the **many** docs
> that need it, instead of hand-maintaining (and re-sweating) each. `docs/docs/current_capacities.mdx`'s
> **Limitations** section projects Q1's support gaps (`query_q1_gaps.ts`); more docs follow. The matrix is
> "good enough" when regenerating a real hand doc from it is a **clear win**.

> **Findings ledger (F#)**: F-numbered items are the cold critique's ledger (full write-up in git
> history). The only ones still cited are **F8–F11, out of scope** (bottom); the machinery the rest
> produced is current state in `README.md` (execution-gated support, § "Directional support
> evidence", § "Upstream oracle gaps").

## Matrix-side work — the model and the projections it renders

The matrix exists to feed **many** consumers; corpus was just the hard flagship. What is left on the
matrix's own side — as opposed to in the generator, which is the findings ledger below — is of two
kinds: a defect in a projection (buildable now) and known incompletenesses of the coverage MODEL
(deferred, so each names the observable that would reopen it).

- **Enumerate the named-group REFERENCE kind (`grpent.groupname`) across its container PLACEMENTS
  and member MODIFIERS — two explicit axes, not a "remaining roles" sweep.** Buildable now, and the
  payoff is measured: every defect yet found in this kind's cells was caught by per-delivery ad-hoc
  probing rather than any projection, because each sat on an axis the grid does not model. The two
  TABLE roles (`{ * coords => uint }` / `{ * uint => coords }`) — one aborted on a raw unwrap, the
  other emitted non-conformant CBOR at exit 0 — are refused
  (`plain_group_table_domain_rejects_gracefully_at_both_spellings`). The MODIFIER axis is live
  too, each flavor breaking DIFFERENTLY: the KEYED struct-map
  member and its TAGGED flavor emitted silent non-conformant wire at exit 0, its OPTIONAL flavor
  panicked, and a single-entry map group-choice arm emitted a serializer and a deserializer that
  disagreed with each other — all refused
  (`plain_group_keyed_map_member_rejects_gracefully_at_every_spelling`). The ALIAS-INDIRECTED
  modifier proves the axis in the other direction: in ARRAY placements it is
  SUPPORTED and byte-equal to the direct reference
  (`alias_to_plain_group_in_array_positions_matches_the_direct_reference`), and reaching that
  verdict took four rep-stamp sites plus two emission sites — one of which had been emitting an
  array header counting elements against a body written in items, at exit 0 in a crate that
  compiles. The `?`-OPTIONAL modifier is refused, because
  a splice writes no marker of its own and so leaves the array's LENGTH as the only evidence of
  presence, which an embedded decoder that length-checks just its own members cannot read
  (`optional_plain_group_array_field_rejects_gracefully_at_every_spelling`; real support is the
  occurrence/bounds design owned by the findings entries "Honor non-final and `+`/bounded
  count-permitting occurrences on heterogeneous ARRAY-record fields" and "Decode-disambiguate a
  non-final `?` optional array-record field whose CBOR major types OVERLAP a later field's" —
  cited below as "the occurrence/bounds family" — and its guard covered a TAGGED
  spelling that had been shipping a codec whose own decoder rejected its own bytes). So enumerate
  the product: placement (array
  element, array-rep field, struct-map member, table key, table value, map/array group-choice arm,
  TYPE-choice arm) × modifier (bare, keyed, `?`-optional, count-permitting rest tail — `*` in
  final position, distinct from the sole-element homogeneous cell
  `contain.occurrence-target.grpent.groupname` — tagged, alias-indirected), with truthful
  verdicts — accept rows where the splice is conformant (array placements, including every
  alias-indirected one) or a wrapping restores it (the named-array remedies), and reject rows
  carrying the refusal evidence. The TYPE-choice-arm placement and the rest-tail modifier joined
  the product late: each carried a live defect (the
  final-position `* kv` rest-tail panic and the plain group in a TYPE-choice arm — both refused,
  see below) while sitting on an axis VALUE the product as first stated did not list — this
  entry's own opening lesson (a defect on an axis the grid does not model) applied to the grid's
  own draft, and the reason the axis values above are enumerated exhaustively rather than by
  example. The group-choice arm's silently dropped `?` sat
  INSIDE the stated product (group-choice arm × `?`-optional), which is exactly the cell class the
  enumeration exists to mint, and is refused
  (`occurrence_on_single_entry_group_choice_arm_rejects_gracefully`): honoring it needs a zero-case
  variant the sibling arms' length checks can tell apart on the wire, which is the
  occurrence/bounds family's scope. The same defect also marks where this grid STOPS being the
  instrument. Probing widened it twice past the cell: to every occurrence marker an ARRAY arm can carry
  (`*`, `+`, `n*m`, all byte-identical to the unmarked arm at exit 0, not just `?`), and off the
  named-group reference kind entirely — an ordinary keyed member arm (`[ x: uint // ? a: tstr ]`)
  dropped its marker byte-identically too. So the guard reads the ENTRY's occurrence rather than its
  type, and the grid would have minted one cell of a defect whose real denominator is arm × marker
  over every entry kind. It also narrowed in one direction: in a MAP
  arm the byte identity is CORRECT for every lower-bound-≥1 marker, since unique keys mean a second
  repetition of a fixed-key alternative would duplicate its keys, so `+` / `2*3` / `2*` admit count
  1 and nothing else. Those keep generating — the f18d764 collapse boundary, asked of the one shared
  predicate (`inline_group_occurrence_flattens`) rather than restated, so the arm seam and the
  inline-group splice cannot come to disagree — and only the zero-permitting markers refuse there.
  A grid row must therefore carry the REP, not just the marker. The rest-tail modifier is settled
  the same way — refused, because a rest tail collects one
  value per remaining array element and a plain group is not one, having no type of its own to
  collect (`plain_group_array_rest_tail_rejects_gracefully_at_every_spelling`; the splicing tail
  that would consume the group's arity per repetition is the occurrence/bounds family's scope). Its
  cell is a rep-carrying row too, and the row's own probing shows why: the ARRAY tail refuses,
  the SOLE-element homogeneous cell (`[* kv]`) generates, the pure TABLE form (`{ * kv => uint }`)
  refuses on its own delivered message — and the MAP REST ROW, the array tail's exact twin, refuses
  on a THIRD delivered message, one per offending slot
  (`plain_group_map_rest_row_rejects_gracefully_at_every_spelling`), because a map entry holds
  exactly one item in each of its two slots. That is the same argument the pure table form's message
  carries, so the rest row's cell separates from the table's on the fixed prefix alone, not on the
  reason. One marker, four verdicts, separating on container and on whether a fixed
  prefix precedes it: no cell of this product is predictable from its neighbours, which is the
  argument for minting all of them rather than the ones a probe happened to reach. The TYPE-choice-arm
  PLACEMENT is the row where the grid's own verdict vocabulary is settled ahead
  of the cells: every cell of it is a reject row, and permanently, because a type choice denotes
  exactly ONE data item — telling the arms apart on the wire is the whole of what a choice decoder
  does — while a plain group can only be spliced and a splice has no one-item form
  (`plain_group_type_choice_arm_rejects_gracefully_at_every_spelling`). That closes the branch
  "stamp the group's Array rep on the choice walk's arms the way the array
  PLACEMENTS do": the array placements splice into a container whose length already scales with the
  group's arity and absorbs the extra items, and an arm has no such container, so there is nothing to
  stamp a rep ONTO. The array framing the refusal names is therefore the semantics rather than a
  workaround, which makes this row cheap to mint (one verdict, one evidence class) and the row worth
  minting anyway: it was three DIFFERENT failures from that one root — the `rust_struct` expect/unwrap
  in `src/intermediate/rust_type.rs`, the plain-group registry assert in `src/intermediate/mod.rs`
  reached earlier under `--wasm`, and at RULE position under the `/ null` collapse an exit-0
  emission of `pub type U = Option<Kv>;` over a `Kv` defined nowhere, a non-compiling crate reported
  as success. A grid whose cells carry only "panics" would have flattened the third, which is the one
  that matters most. NB this defect class is also one synthetic instance shy of the
  "Grammar-derived legality denominator" item's reopening signal (a generation defect in a
  grid-BLANK cell): a
  consumer-reported spec breaking in such a cell fires that signal outright; this entry is the cheap
  targeted slice that does not wait for it.
- **Enumerate the fixed-value KINDS in the bare TYPE-CHOICE arm role (`role.choice-member`).**
  Buildable now, same shape as the float enumeration that exposed it: the delivered float cells
  cover the member and group-choice-arm positions, and a due-diligence probe of the THIRD
  arm position found `t = 1.5 / tstr` and `t = -1 / null / tstr` refused for an unspellable derived
  variant identifier (`F1.5`, `U-1` — § findings, "No auto-naming scheme for a DERIVED variant
  identifier…") while the uint/text kinds are fine — exactly the known-NON-uniform kind axis
  measured on two of three positions. Add choice-member cells per fixed kind with truthful verdicts
  (uint and text as accept rows; float and nint as reject rows carrying the graceful-refusal
  evidence, which is what they stay until a derived-name scheme lands), so the kind × position
  product stops relying on per-delivery diligence for its last column.
- **Grammar-derived legality denominator for the role × feature grid.** The grid rendered in
  `tests/corpus/COVERAGE.md` § "Role × feature containment grid" takes its denominator from two
  *observed* sets — the cells the containment relation models, plus the cells the snapshot corpus
  exercises — so a cell that neither touches renders blank and is invisible *as a cell*. Derive the
  denominator instead from the grammar (a `role → admitted-production` relation over RFC 8610's ABNF,
  the same source each `roles.toml` entry already cites), so that "nothing has an opinion about this
  legal nesting" becomes a rendered state rather than the absence of one, and the corpus stops being
  the thing that decides which cells exist.
  **Reopening signal:** a generation defect (panic, refusal, or miscompile) on valid CDDL reported
  against a `(role × feature)` cell the grid renders BLANK — measurable by the reporter, who holds the
  spec that broke and can look its cell up in the published grid. The two panics that motivated the
  grid itself do not meet this signal: both sat in cells the corpus exercises and nothing models,
  which the grid now names `·`.
- **Per-VALUE encoding reachability.** A head argument that is fixed by the construct's own definition
  is not a judgment call: a prelude construct declares the exact cell its head lands in, and
  `build_matrix.ts` re-derives that from the pinned prelude, so `bigfloat` (tag 5, at every value)
  claims `enc.major6.imm` and nothing else. What remains is the case where the argument **follows the
  value**: `enc.major0.ai27` needs a uint ≥ 2^32, `enc.major2.ai26` a byte string ≥ 2^16 bytes. Those
  cells are genuinely reachable, just not at every value, and separating the writable ones from the
  merely-legal ones means a per-(construct, cell) value predicate — a relation far larger than the
  parent→leaf one it would sit on. So `tests/golden_hex/COVERAGE.md` lists them and says in its own
  prose that reachability is the reader's call. *Reopening signal:* an author working the *untested
  and emittable* column hand-dismisses more listed cells as value-unreachable than they can write
  vectors for. Measured by the person already doing the work, on the axis this deferral's cost grows
  along — how far the column overstates the actionable set. It is not already met: the width cells
  are writable up to `ai26` (a 64 KiB literal) and only `ai27` is out of reach, so today an author
  writes more than they dismiss.
- **Narrowing at emission, in the golden fixture floor.** The per-construct section of
  `tests/golden_hex/COVERAGE.md` marks a construct exercised when a golden rule NAMES it, corrected
  for the prelude's plain aliases (derivable, so applied rather than disclaimed). Not corrected: a
  construct the fixture reaches only through a WIDER type that cddl-codegen narrows when it emits.
  `one_float = [v: float]` asserts each value at its shortest form, so `prelude.float16`'s cell is
  exercised in fact while no rule names `float16`, and the row reads ✗ with its cell listed
  untested. Closing it means
  modelling which arm of a union the generator picks at emission — a claim about the generator, not
  about spec structure, so it cannot be derived from the pinned prelude and must not be guessed; the
  honest fixes are a golden rule that names the narrow type, or an emission model. *Reopening
  signal:* a vector written to close one of these ✗ rows turns out to assert bytes an existing vector
  already asserts — wasted authoring work, measured by the author who did it, and growing with the
  number of skewed rows rather than with the number of consumers. The entry records that the case
  exists; it records nobody having paid for it yet, so the signal can still fire.
- **Tag registry — deliberately not pinned or enumerated.** cddl-codegen is tag-parametric, so the tag
  NUMBER is a parameter of the `type2.tag` feature rather than a construct of its own, and the few
  codegen-distinct tags are already prelude rows; the reasoning is current state in `README.md`
  § "What is a feature?". *Reopening signal:* a **tag-semantic** consumer of the master appears — one
  that must emit per-tag behaviour (datetime, bignum, …) rather than a parametric wrap/unwrap. That is
  the only reader for whom the parametric model under-describes the space, and it is measurable by
  them the moment they try to key anything off a tag number.

## Expansion (when relevant)

- **Profiles/versions:** v1 targets the RFC 8610/9682 grammar + the IANA control-op registry (spans RFC
  8610/9090/9165/9741). The CDDL modules draft is now a pinned source
  (`sources/draft-ietf-cbor-cddl-modules-06.txt`; version-pinned URL — bump filename+URL+sum together)
  and cddl-codegen has a defined, tested stance on its directives: recognized and refused loudly
  (`module_directive_import_aborts`/`module_directive_include_aborts`), with cddlc `as`-expansion
  output rejected at the dotted-ident seam (`dotted_rule_name_rejects_gracefully`). Feature rows for
  the draft's constructs become worthwhile only if real module support lands
  (`tests/TESTING_ROADMAP.md` § Deferred features tracks that decision); bump cddl-codegen's
  declared target profile if it updates its `cddl` dependency.
- **More tools:** the master is implementation-agnostic; add `annotations/<other-tool>.toml` if another
  consumer adopts it.
- **Intra-alternative variation rows (the enforcement-blind-spot class):** when a new variation axis
  appears *inside* one ABNF alternative (a new controller value class, a new literal lexeme, a new
  head-type × sign combination), enumerate it as a ROW with its `class="constraint"` boundary-violation
  reject vector FIRST — before trusting a green. Silent-acceptance bugs are visible only to the
  enforcement axis, and that axis reaches exactly as far as row/example enumeration — an enumeration gap
  here is an enforcement blind spot, not just coverage accounting (the over-acceptance gotcha in
  `README.md` § "Gotchas"). Precedent rows to model on: `rangeop.{inclusive,exclusive}.{int,nint,float}`,
  `occur.bounded.{lower,upper}`, `value.number.{hex,bin,hexfloat}`, `ctl.ne.{zero,one}`, `ctl.size.uint`
  (Q4 pins the exact enforce-green set). The vector CLASS follows the decoder's current behavior:
  when it correctly rejects the boundary violation, the row lands `class="constraint"` (the rows
  above); when it currently WRONGLY ACCEPTS it, the same enumeration lands the violation as a
  certified `class="over-acceptance"` pin instead, which flips to `class="constraint"` once the fix
  lands. Both branches have been walked end to end on one row — the worked cycle is in `README.md`
  § "Gotchas", so it is not retold here. The same rule applies to DISPATCH variations, not just
  enforcement — precedent row: `contain.group-choice-arm.grpent.member.record_array_tagged`
  (`t = [ a: tg // b: tstr ]`, `tg = #6.10([x: uint])`), which pins the TAG head of a struct-level
  tagged record as an array-rep group-choice arm member. The arm discriminant routes a non-embedded
  record arm through `RustType::cbor_types`, whose struct-tag branch dispatches on Tag; before this
  row, every enumerated record-arm gate (`contain.group-choice-arm.grpent.member.record_array` and
  the wasm-matrix `struct`/`generic`/`ralias` gchoice cells) was untagged, so an alternative
  discriminant that hand-mapped `record.rep` (always Array/Map, never Tag) would have gone green
  through every existing gate while mis-dispatching tagged-record arms — this row is what makes
  that mis-mapping fail loudly. A controller OPERAND's value classes are the same axis on the
  generation side: the `.default` operand kinds (`true`/`false`/`null`/`nil` — prelude constants
  arriving as typenames, a lexeme class no example enumerated) reached a parse-time `panic!` on
  valid CDDL while every gate stayed green, because the one committed `.default` example spelled a
  `0`. Delivered enumeration: `tests/corpus/default_scalar_kinds.cddl` + the fallible
  `type2_to_fixed_value` lowering (a non-literal operand is a recorded rejection, so the panic
  class cannot return through a shape nobody enumerated).
- **A supported control's HOST-PLACEMENT spelling space — one support example proves ONE point of
  it.** Distinct from the intra-alternative axis above (variation inside the controller): a
  control also varies by where its TARGET sits — occurrence (mandatory vs optional member),
  representation (map vs array member), and carriage (spelled at the member vs carried through a
  rule-position alias) — and every breadth layer (the per-op support probe, `wasm_matrix_compiles`,
  `feature_corpus_compiles`, the component build sweep) walks committed one-spelling-per-op inputs,
  so a second point of that product can be broken, or panic, while all of them stay green.
  Proven across one whole product (2026-08, the `.default` coherence cycle): the single committed
  spelling (optional map member, uint) hid FOUR defects at other points — a mandatory-defaulted
  member emitted exit-0 E0061 glue on the wasm AND component faces, an array-rep defaulted member
  emitted exit-0 E0308 rust, a bool operand panicked, and the head-refusal message named a head it
  refuses. All were found by hand boundary-probing at pickup, none by a gate. The rule going
  forward: when a supported control or feature is found broken at a SECOND point of its spelling
  space, the closing change enumerates that op's full product into corpus fixtures rather than
  fixing the one probed point — `.default` is the delivered model (`default_value`,
  `default_mandatory`, `default_array_rep`, `default_scalar_kinds` jointly cover
  occurrence × representation × operand kind × carriage, and flow into every corpus-input gate on
  all faces). Enumerating every op's product up front is deferred: the supported-op count times
  the product is large, and one probed point per op is the measured-adequate posture for the ops
  with no breakage history. Reopening signal for the mechanical version (a product-enumeration
  authoring rule enforced per supported op): a SECOND op found broken at an unenumerated
  host-placement point — measurable by whoever hits the break, and lying on the dimension the
  deferred cost grows along (the count of ops with proven multi-point breakage; `.default` is one).
- **A REFUSED construct's spelling space — a refusal probed on one spelling is not a refusal of
  the class.** The reject-side sibling of the two supported-op entries above: every layer that
  certifies a rejection (the matrix row's one `example`, the reject-catalog projection built from
  it, and the `*_rejects_gracefully` pin's hand-written vectors) walks committed
  one-spelling-per-row inputs, so a sibling spelling of the refused class can generate WRONGLY at
  exit 0 while every layer stays green — the worst outcome, because the refusal's existence reads
  as the class being handled. Proven 2026-08-08 (B3-021 pickup): the incremental-extension guard
  keyed on the REPEAT statement's own alternate flag, and every certifying layer spelled the
  base-first ordering — so the extension-FIRST orderings (`a /= tstr` before `a = int`, legal
  CDDL, three shapes) silently generated a type modelling one arm at exit 0, found only by hand
  ordering-probes at pickup and retired by keying the guard on the name's whole statement set,
  with both orders pinned. The rule going forward: when a refusal's trigger is a property of a
  STATEMENT SET or a placement (ordering, statement count, host position) rather than of one
  statement, its rejection pin must enumerate the trigger space's orderings — the delivered
  `incremental_choice_extension_rejects_gracefully` (both orders, per class) is the model.
  Reopening signal for the mechanical version (an ordering/permutation leg in the reject-catalog
  projector): a SECOND guard found bypassable at an unenumerated point of its trigger space —
  measurable by whoever hits the bypass, on the dimension the deferred cost grows along (the
  count of guards with proven ordering-bypass history; the incremental-extension guard is one).

## Findings — open (candidate cddl-codegen fixes; the matrix's actual payoff)

Every entry here is a defect or a missing capability in **this** generator that a probe, gate, or
fuzzer sweep surfaced — that is what the section means, and what the many `§ findings` citations in
`src/tests/` and `tests/README.md` resolve to. Two neighbouring classes live elsewhere on purpose:
what the generator does TODAY, including the boundaries it keeps permanently and the upstream-oracle
gap state, is current state in `README.md` (§ "Gotchas", § "Upstream oracle gaps"); prunes that wait
on an external release are § "Upstream close-outs (waiting on external releases)". New findings are
ledgered here (that's what the probe/gate error messages point at).

One evidence rule for this section: an entry claiming a GENERATOR defect carries a harness-free
repro — the tool run alone, into a scratch dir — before it is written. Evidence read off committed
fixture trees does not qualify, because those trees pass through the test harness, which appends its
own content INTO generated files, so a harness-written emission is indistinguishable there from a
generator-written one. One entry was retracted on exactly that (2026-08-07): its "greppable in the
fixture trees" emission defect — an unused `use serialization::*;` — was such an append, and a
fresh-generation probe found the generator emits that glob nowhere. The retraction record is in
`tests/TESTING_ROADMAP.md`'s unused-imports entry. Two corollaries, each learned from a shipped
imprecision: a verified remedy's MECHANISM gloss is part of the claim — write only what the probe
executed (a 2026-08 entry glossed its executed rename-the-rule remedy as "`@name` renames a rule",
which the parser refuses; the gloss outlived the entry into a delegation spec before a parse probe
caught it) — and edits to this section delete or move entries WHOLE, bold header through final
line (a partial deletion once merged the following entry's body into the previous one's
continuation lines, headerless and invisible as an entry; no lint can tell that merge from a long
entry, so the atomicity is the rule).

- **A group RULE whose body carries multiple group choices is refused with two verified remedies —
  the honoring design (a choice of bodies) is the missing capability, not the guard.**
  `pg = (a: uint // f: bytes)` is valid CDDL (a group rule's body is
  `grpchoice *(S "//" S grpchoice)`), and today it is a graceful refusal at the `api::with_types`
  pre-scan, naming the rule, its choice count and two remedies verified to generate and build
  (`multi_choice_group_rule_body_rejects_gracefully_at_every_placement`): write the alternatives
  where the choice is actually made, as the referencing container's own group choices
  (`h = [ x: uint // a: uint // f: bytes ]`), or split the body into single-choice group rules
  referenced as separate arms (`pga = (a: uint)`, `pgf = (f: bytes)`, `h = [ x: uint // pga // pgf ]`).
  Rule position is the seam because rule position is where the shape is decided: the refusal fires
  on the DEFINITION alone, once per offending rule however many references exist, which is also
  where the abort it replaced fired. The missing capability is the honoring design, not the guard: a
  reference in group-choice context would concatenate the alternatives, while every other placement
  would have to mint a CHOICE OF BODIES whose arms a decoder can tell apart — the same
  naming/registration problem a named group-choice rule's arms already solve, which is public API of
  the generated crate and wants a deliberate convention rather than an ad-hoc one. Until it exists,
  the two remedies are the supported route and the refusal points at them. Reopening signal: a spec
  in which one group rule's alternatives have to be re-spelled at more than a handful of reference
  sites — the hand-expanded arm lists a spec author keeps in sync grow as references × alternatives
  INSIDE one spec, so that count is the dimension to watch rather than a second consumer appearing.
  Not already met: today's evidence is synthetic probes, each with at most one reference site.
- **No auto-naming scheme for a DERIVED variant identifier that the fixed-value minter cannot spell
  — such FLOAT, NINT and keyword-minting TEXT choice arms are refused instead of named.** A choice
  arm with no member key takes its variant name from the value's LEXEME, which fails two ways:
  `1.5` → `F1.5` and `-1` → `U-1` are not identifier-shaped, and `"self"` → `Self` is
  identifier-shaped but a keyword (the emitter writes minted names verbatim and never raw-escapes,
  and `Self` is Rust's only capitalized keyword, so it is the sole reachable one today).
  Today that is a graceful refusal at parse naming the rule, the arm and the
  arm-position `@name` remedy (pinned by
  `lexeme_derived_arm_variant_name_rejects_gracefully_at_both_naming_sites`, with
  `tests/robustness/choice_float_arm_variant_name.cddl`, `choice_nint_arm_variant_name.cddl` and
  `tests/robustness/choice_keyword_arm_variant_name.cddl` holding the catalog outcome), across both
  naming consumers:
  bare and c-style type choices, nested anonymous choices, and the bare-member group-choice spelling
  (`t = [ true // 1.5 ]`). The NAMED-member group-choice spelling is unaffected — its variant comes
  from the member key, which is what the green cells
  `contain.group-choice-arm.type2.value.float_array` / `.float_same_major_array` and the member cell
  `contain.array-element.value.number.float` measure. The missing capability is the scheme itself:
  a sanitized derived name (`F1_5` / `FNeg1`, or a positional one) is public API of the generated
  crate, so it wants a deliberate, documented convention rather than an ad-hoc escape, and shipping
  one retroactively renames variants for anyone who reached the refusal and wrote `@name` instead.
  Until then `@name` is the supported route and the refusal points at it. Reopening signal: a spec
  in which the arms needing a hand-written `@name` outnumber the ones that do not, or a single crate
  carrying more than a handful of them — the hand-naming cost grows with that count inside one
  consumer, so that is the dimension to watch rather than a second consumer appearing. It is not
  already met: today the evidence is synthetic probes only, and a synthetic probe writes no
  `@name`.
- **No opt-in NOMINAL wrapper over a custom pair, so a pair's wire has no standalone entry point.**
  A pair-carrying transparent alias mints no rust type (that is what gives one CDDL name one wire
  form: a `pub type` there would carry the aliased type's built-in codec as a standalone wire
  contradicting the one every embed site writes). The consequence is that the pair's wire is
  reachable only THROUGH an embedding — a holder field, a table entry, an arm — and never as
  `Inner::to_cbor_bytes()`. A consumer who wants that entry point today hand-writes a wrapper over
  the codec functions. Deliberately not built as a default: nominalizing would break the in-memory
  transparency that is the alias-of-marker spelling's documented point (a value IS the hand-written
  type), and it is the remedy `comment_dsl.mdx` § "Positions that are rejected" prescribes for the
  extern/raw-bytes marker, tag-head, tag-258, enum and `@newtype` refusals — each of which needs the
  alias to stay transparent. A directive requesting the wrapper explicitly would sit ON TOP of the
  one-wire-form contract rather than against it, since an opted-in nominal type owns its whole wire
  in both directions. **Reopening signal:** a single consumer crate carrying more than a handful of
  such hand-written standalone wrappers — the hand-maintenance cost grows with that count inside one
  crate, so that is the dimension to watch rather than a second consumer appearing. It is not
  already met: the entry records that the hand-written route exists, not that anyone has been
  observed paying for it.
- **Say each rejection once, so the count of messages is the count of problems.** A single
  offending construct can report the same rejection twice: `a = [{x: int}]`, `a = [[int]]` and
  `x = bytes .cbor ({a: int, c: uint})` each print their message two times, while the same defect in
  a map-value position (`m = { outer: { a: int, c: uint } }`) and the older group-choice rejections
  print it once. A SINGLE-ELEMENT inline array is the same instance seen from the type-choice side:
  `x = [1.5 / tstr]` prints its rejection twice and `x = [tstr / tstr]` its arm-dedup warning twice,
  while the two-entry form `x = [1.5 / tstr, z: uint]` prints once — probed 2026-08-06 on an
  UNMODIFIED binary (default profile, `--wasm=false`). The duplication is positional — the parse
  walk visits an array-element / `.cbor`-controller type2 twice, and a `panic!` at such a site
  masks the second visit by aborting on the first, so any abort→rejection conversion should expect
  to make a pre-existing double-walk observable.
  `record_rejection` is a bare `Vec::push` and `rejections_error` joins the vector, so nothing
  dedups. The fix is NOT simply deduping that join, and that is the part worth recording: two
  genuinely distinct rules can hit one site and produce byte-identical messages (several rejection
  texts name the construct class, not the offending rule), so a blind dedup would silently collapse
  two real problems into one report — trading a cosmetic defect for a correctness one. The shape
  that works is to make the message identify its site (the `rejection_site` helper already exists
  for exactly this) and dedup on the identified pair, or to suppress the second VISIT rather than
  the second message. Reopening signal, on the axis the cost grows: a spec whose rejection output
  makes the number of DISTINCT problems unreadable — three or more copies of one message, or
  duplicates interleaved with genuinely different rejections — reported by someone reading that
  output to fix their own spec.
- **Incremental GROUP-choice extension (`//=`) stays refused; honoring it is blocked on the
  choice-of-bodies design, not on this seam.** The type half is done — `merge_incremental_type_choice_extensions`
  splices every `/=` statement's arms into the first statement's before `ParentVisitor` is built, so
  a `/=` chain generates byte-identically to the folded type-choice rule
  (`incremental_type_choice_extension_equals_the_folded_spelling`), in any statement order and
  across input files. `//=` cannot take the same route: merging group-choice arms into a plain-group
  rule mints exactly the multi-choice group-rule shape refused above, whose honoring design is that
  entry's, not this one's. So the refusal here (`repeated_rule_definition_rejections`, pinned by
  `incremental_choice_extension_rejects_gracefully`) is order-independent and will remain correct
  however that design lands; what changes on the day it lands is that this refusal's arm is deleted
  and the merge generalized to group statements. On implementation: flip the `assigng.extend` reject
  row (verify.ts re-probe) and re-mint its decode rows. Reopening signal — it is the
  choice-of-bodies entry's signal, since nothing here can move first.
- **Honor non-final and bounded count-permitting occurrences on heterogeneous ARRAY-record
  fields.** A **final-position** `* t` after ≥1 fixed member is the loose open-array rest tail;
  final `+ t` / `1* t` is its `NonEmptyVec` twin with a first-element construction door (both
  supported by `contain.occurrence-target.grpent.member.{zero_array,plus_array}`). Still rejected
  gracefully: bounded (`n*m`) final tails and any **non-final / middle** `*` / `+` member
  (`[uint, * bytes, tstr]`); those boundaries are pinned by
  `occurrence_on_array_record_field_rejects_gracefully`. The rejection avoids the silent
  exactly-once narrowing that makes a generated decoder reject spec-valid repetition counts —
  invisible to round-trip tests, surfaced only by spec-derived decode vectors. Real support for the
  middle-position case shares an occurrence-decoding design with the unresolved non-final `?`
  overlap below. RFC 8610's greedy PEG occurrence semantics do not permit reserving a mandatory
  suffix or backtracking; repeated occurrences additionally need a representation and residue
  policy of their own.
- **Real bounded `?` / `n*m` table cardinality is a candidate feature.** A count-permitting occurrence
  marker on a single non-literal arrow map entry never silently widens to an unbounded `*` table —
  the widening the rejections below exist to prevent, since `HomogenousMap` — unlike
  `HomogenousArray` — carries no bounds, so a decoder built from the widened form wrongly accepts
  out-of-window maps.
  `+` / `1*` is honored as a `NonEmptyMap<K, V>` whose single `TryFrom` door rejects the empty map
  identically at the API and the wire (`4fa3041`; enforcement model:
  `docs/docs/output_format.mdx` § "Non-empty containers"). That empty-map rejection is pinned by the
  `contain.occurrence-target.memberkey.type1.plus_table` `class="constraint"` decode vector, projected
  `enforce = yes`. The other count-permitting markers — the `?`, `n*m`, `*n`, `n*`, and `0*n` spellings
  such as `{ ? tstr => uint }` and `{ 2*3 tstr => uint }` — are **rejected gracefully**, pinned by
  `contain.occurrence-target.memberkey.type1.optional_table` and
  `contain.occurrence-target.memberkey.type1.bounded_table` in `tests/matrix_reject/` and by
  `no_occurrence_arrow_map_entry_rejects_gracefully`. Honoring `?` / `n*m` as real map-size bounds (a
  bounds slot on `HomogenousMap` mirroring `HomogenousArray`'s, plus the `MinN`/`MaxN`/`BoundedMap`
  shapes the design doc phases after `NonEmptyMap`) is the remaining candidate feature. If that bounds
  slot lands, also revisit the rejected no-occurrence spelling `{ k => v }` — it becomes implementable
  as bounds `(1, 1)`, so flip its reject row (`contain.map-key.memberkey.type1.tstr_arrow_nooccur`) on
  merit rather than keeping the rejection out of inertia.
- **Bounded table cardinality remains the type-level constraint residue beyond `+`.** Ordinary and
  preserve homogeneous ARRAY windows now use the `BoundedVec` sibling and Min/Max wasm wrappers;
  bounded `@duplicates reject` arrays remain runtime-checked `OrderedSet` values until a compound
  bounded-unique representation is designed; do not reopen that
  delivered representation. The rejected `?`/`n*m` table spellings owned by "Real bounded `?` /
  `n*m` table cardinality" would require a `BoundedMap` sibling. Rust cannot vary method fallibility
  by const parameter, so it remains separate from `NonEmptyMap`; reopen when a table consumer needs
  bounded cardinality rather than a parser refusal.
  - **An open table's min-1 typed row** (`t = { + K_t => V_t, * K_r => V_r }`): the one min-1 shape
    that does NOT use the unrepresentable model, because the container it bounds is one `pub` member
    of a struct whose OTHER member is an unbounded sibling. It is enforced at a seeded door
    (`new(first_key, first_value)`, so a constructed value satisfies it) plus one check after each
    reader's loop, raising the very `RangeCheck` `NonEmptyMap`'s `TryFrom` raises — but clearing the
    field afterwards is representable. Making it unrepresentable means giving that member the
    `NonEmptyMap`/`NonEmptyPairMap` type, which ripples through the JSON, wasm, WIT and
    extern-interface faces of a shape that currently reuses the rest row's container spelling
    verbatim. Reopening signal: a consumer reporting a value that reached the wire — or a
    cross-crate boundary — with an empty typed row, i.e. the bypass observed rather than argued.
  - **Atomic hand-over** (value windows: `uint .le N`, `.size` ranges on bytes/text): private-field
    newtypes whose `TryFrom` door replaces today's ctor/deserialize checks (the
    `value_bounds_check_line` emission sites).
  - **Static-representable** (`bytes .size 32` → `[u8; 32]`, exact `n*n T` → `[T; n]`): the
    representation itself carries the constraint — the `uint .size 1` → `u8` mapping is the shipped
    precedent — independently of the `TryFrom` door, which stays as the ergonomic entry point.
  Each class lands tests-first when picked up; the `+` case's fixture surface (the `nev_*` rules in
  `tests/core/input.cddl`, the `tests/robustness/non_empty_*` collision/dedup pins) is the template,
  and any wasm-boundary shape a new class mints must be enumerated in the wasm-ABI/multifile matrix
  `SHAPES` in the same change (the "Keep EVERY matrix axis honest" rule).
- Zero-permitting occurrences (`*` / `0*n` / `*n`) on a keyed struct-map field are **rejected
  gracefully** (pinned by `contain.occurrence-target.memberkey.bareword.{zero_map,zero_bounded_map}`
  in `tests/matrix_reject/`) rather than silently narrowed to a mandatory field. `+` / `n*m` with a
  lower bound ≥ 1 still generate a mandatory field — under unique map keys they collapse to
  exactly-one, so mandatory is the honored semantics (a deliberate boundary). Real support for `*`
  (an `Option<T>` field, like `?`) is a candidate feature.
- An occurrence marker on an inline (parenthesized) group — `a = [* (int, tstr)]` — is **rejected
  gracefully** (pinned by `contain.occurrence-target.grpent.inline_group.{plus_array,optional_array,bounded_array,zero_map}`
  in `tests/matrix_reject/`) rather than silently narrowed to an exactly-once record. Boundaries
  kept: `[1*1 (…)]` still generates (exactly-once IS the semantics, so flattening the group away is
  sound); on the MAP side `+` / `n*m` with a lower bound ≥ 1 still collapse to a mandatory field;
  and the parenthesized table `{ * (int => tstr) }` still generates a `BTreeMap`. The rejection
  recommends naming the group, and the named form works end to end (`pair = (int, tstr)`,
  `a = [* pair]`). Real `Vec<Synthesized>` / `Option`-style support for zero-permitting markers is a
  candidate feature; flipping a row to `ok` must not decay back to silent narrowing (unsupported
  rows carry no decode-conformance row; `project_decode_conformance.ts` enforces that boundary).
- **`cbor-any` is a decided permanent exclusion, not a candidate feature.** Its
  `#6.55799(any)` self-describe tag is a property of a complete serialized CBOR stream, not an
  ordinary value a generated wrapper can hold. Keep its role-neutral graceful refusal and the
  `tests/TESTING_ROADMAP.md` § North star exclusion; do not reopen it without an explicit maintainer
  decision.
- **Honor a non-final `?` optional array-record field whose CBOR major types OVERLAP a reachable
  follower's.** `a = [ ? f0: uint, f1: uint ]` generates at exit 0, but `A` gets no decoder: the
  refusal is loud and propagates honestly through every consumer, and both `--emit-tests` minters
  skip the type with a named line. Position alone is not the gap: a type-disjoint non-final `?`
  (`[ ? f0: uint, f1: tstr ]`) works today. RFC 8610 Appendix A makes occurrence matching greedy
  and non-backtracking, so `[uint]` is invalid for the overlapping spelling: the optional consumes
  that item and the mandatory successor is then missing. A decoder must not reserve the suffix and
  reinterpret `[uint]` as an absent optional. Real support needs an API/serialization design that
  cannot mint that unreachable absent assignment, plus a decoder which preserves the same greedy
  rule for both definite and indefinite arrays. **Reopening signal:** a spec author reports a rule
  they need to decode on the wire, not merely a shape a fuzzer composed.
- **Real support for the anonymous nested MAP in a type position** (`a = [{x: int, y: uint}]`, and
  its map-value / `.cbor`-payload / `/`-choice / generic-argument / occurrence-target /
  group-choice-arm siblings). Every one of those shapes rejects gracefully
  under both profiles, naming the map's supported named form (`m = {x: int, y: uint}`, referenced by
  `m`); that rejection is pinned by `inline_map_member_rejects_gracefully` and its keyless spelling
  by `tests/robustness/inline_map_keyless_member.cddl`. But the
  ARRAY sibling goes further: a `@name` comment on the type2 mints the struct in place. The map side
  has no such door because `Type2::Map`'s member-position walk never reads rule metadata. Giving it
  one would close the last asymmetry between the two anonymous-composite families. Reopening signal:
  the count of anonymous nested maps a consumer must hoist to named rules to compile ONE committed
  spec reaches 5 — hoisting is a mechanical per-occurrence edit, so the cost grows inside a single
  spec rather than across consumers, and the consumer can count it from their own rejection output.
- **Real support for an inline group as a group-choice arm's sole entry**
  (`t = [ (uint, tstr) // bytes ]`, and the map-rep `t = { (a: uint) // b: tstr }`). Both reject
  gracefully, pointing at the equivalent named form
  (`pair = (uint, tstr)`, then `t = [ pair // bytes ]`) — pinned by
  `inline_group_choice_arm_rejects_gracefully`, with the map rep's outcome category held by
  `tests/robustness/inline_group_choice_arm_map.cddl`. Support means minting the arm's struct from
  the parenthesized group the way a named plain group already is. Lower value than the
  anonymous-nested-MAP entry because the remedy is fully equivalent here (naming the group changes nothing on the
  wire), so the reopening signal is the same magnitude one on a higher threshold: the count of
  inline group-choice arms a consumer must name to compile ONE committed spec reaches 10.
- Float-family table key domains are **rejected gracefully** at generation — a key domain that is
  (or recursively contains) a float compiles to a `BTreeMap<f64, _>` (or an `OrderedHashMap` bounded
  `K: Hash + Eq + Ord` under `--preserve-encodings`), and floats implement none of `Eq`/`Ord`/`Hash`,
  so every such crate failed to build (E0277). The rejection covers the direct family (`{ float64 =>
  uint }`, `{ float32 => uint }`, `{ number => uint }`, `{ time => uint }`) and float keys hidden
  behind a resolved generic instance (`{ gen<float64> => uint }`), checked at the one finalize seam
  that sees resolved instances. Pinned by `tests/robustness/float_table_key.cddl` (direct) and
  `tests/robustness/float_table_key_composite.cddl` (composite generic). Remedy: an integer/text/bytes
  key domain. Real float-key support (e.g. an ordered-float wrapper) is a candidate feature.
  Reopening signal: a consumer reports a spec whose table keys are floats in a wire format they do
  not control — i.e. the integer/text/bytes remedy is not theirs to apply.
- Bytes/nint/float fixed map keys are **rejected gracefully** — only uint and text fixed keys are
  implemented on the struct-map record path (pinned by
  `contain.map-key.memberkey.value.{nint,float}_colon_single`,
  `contain.map-key.memberkey.value.{nint,float}_colon_multi`, and the matching group-choice-arm rows
  `contain.group-choice-arm.memberkey.value.{nint,float}_map` in `tests/matrix_reject/`). The
  printed remedy differs by kind: for nint the table `{ * nint => v }` in its own rule keeps
  generating; for FLOAT the table form is itself rejected (the float-family table-key boundary
  entry — floats have no total order), so the float message advertises an integer/text key instead
  of the dead-end table (asserted by the `float arrow key` remedy check in
  `src/tests/robustness_tests.rs`). Real nint key support is the candidate feature (float key
  support is owned by the float-table-key boundary entry's ordered-float question); flipping either
  row to `ok` requires real support, not a decay back to the old `group_entry_to_field_name`
  panics.
  Fixed-literal arrow classification shares `type2_to_fixed_value` with control-operand lowering
  instead of duplicating the candidate-kind list. That shared lowering does **not** widen the
  record-key representation: it is deliberately allowlisted to uint/text, so bytes (as well as the
  nint/float/special kinds above) remain graceful refusals until a dedicated key representation is
  designed. `fixed_key_arrow_single_entry_routes_to_record_path` pins the allowlist seam.
- **Real nint support is ONE cross-cutting candidate feature — its per-shape gaps are enumeration
  cells of the matrix, not separate tasks.** Nint intersects every containment role (fixed map
  keys — rejected gracefully, its own entry; table domains and `@newtype` bounds — work; bare values, json,
  preserve-encodings — partial), so probing any role re-surfaces a nint cell and per-finding
  sessions keep landing small nint conversions without moving the support boundary. Two facts for
  whoever picks this up: (1) `cbor_event` is NOT a blocker — the crate ships full-range endpoints
  (`write_negative_integer_sz` / `negative_integer_sz`, `i128`; since 3.x the non-`_sz` endpoints
  also REJECT out-of-i64-range values cleanly, header unconsumed, instead of silently wrapping)
  and generation uses the `_sz` endpoints wherever the `i64` limit bites (`i64::MIN`,
  preserve-encodings, and `FixedValue::to_bytes` — full-range `_sz` since the `FixedValue`
  widening, pinned by `nint_to_bytes_canonical_across_boundaries`); (2) the IR-side limiters are
  gone — `FixedValue::Nint` is `i128` and can represent the whole CBOR nint range — so the
  remaining full-range limiter is UPSTREAM: the `cddl` crate parses int literals as
  `isize`/`usize`, so a literal in -2^64..-(2^63+1) cannot reach us from CDDL text. Until a
  consumer reports a spec they do not control that needs a full-range nint shape, new
  nint shapes land as graceful rejections + enumeration cells; when one does, the work is the
  runtime/emitted-type design plus the upstream literal-width question, not IR plumbing — then
  flip the pinned rejection rows (record path first, then the group-choice arm).
- **The wasm extern re-export glue demands a wrapper the wasm crate never uses.** Every in-crate
  `_CDDL_CODEGEN_EXTERN_TYPE_` / `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule gets `pub use crate::<Name>;`
  emitted into the wasm crate's generated module, and the "Own-spec extern re-export contract"
  stderr line asks the consumer to supply that name — whether or not the wasm face references it.
  A raw-bytes type reached only as a generic ARGUMENT is the shape where the two come apart:
  `tests/corpus/extern_generic_raw_bytes.cddl` binds `ext_set<pub_key>`, so the wasm face names
  `ExtSetPubKey` and never `PubKey`, and a consumer who dutifully writes the `PubKey` wrapper gets
  `warning: unused import: crate::PubKey` at a generated location for their trouble. The rust side
  does not have the problem here only by accident (its `pub type ExtSetPubKey = ExtSetRawBytes<PubKey>;`
  names it). Everything about the fixture ALREADY compiles against seeded definitions on both faces
  under all three profiles — the warning is the whole of what is left, and
  `integration_tests::COMPILE_SKIP` holds that one fixture for exactly it.
  Making the wasm re-export usage-conditional is the obvious fix and is NOT a test change: it
  narrows a documented consumer obligation, and under-emitting it is an E0432 in someone else's
  crate, so the emitter must be sure a name is unreachable rather than merely unreferenced by the
  shapes a fixture happens to use.
  **Reopening signal:** a consumer reports the unused-import warning (or silences it) on a wrapper
  the tool asked them to write — they can see it in their own build, which we cannot, and it is the
  same observable whether they hit it once or on twenty types.
- **Corpus/matrix extern EXECUTION, and the multifile matrix's structural extern exclusion.** The
  COMPILE side of extern breadth is delivered and is now current state, documented where it is
  enforced rather than here: the matrix rows in `README.md` § "User-code rows are SEEDED, not
  exempted", the corpus/wasm-matrix side in `tests/README.md` (`CORPUS_DEF_SPLICE`,
  `append_extern_defs`, and the two per-gate skip lists that remain, each with its own reason).
  Two deliberate holes are left, each with an owner rather than a plan here:
  - EXECUTION of user-code cells — running the emitted round-trip module rather than compiling it —
    is `tests/TESTING_ROADMAP.md`'s extern-deps wasm-boundary entry. A seeded codec round-trips
    because the seed says so, which is why `feature_corpus_roundtrips_nondefault_profiles` skips
    those stems on an execution-side reason of its own rather than inheriting the compile floor's.
  - The multifile matrix never enumerates extern/rawbytes shapes at all
    (`project_multifile_matrix.ts`'s `SHAPES`), so its exclusion is structural rather than a skip
    list. Extern PLACEMENT across scopes is covered by `tests/extern-generic-scoped` and
    `facade_composition_compiles`.
  **Reopening signal for the multifile half:** a cross-module extern break that
  `facade_composition_compiles`' single hand-curated composition does not reach — i.e. a real
  consumer's multi-scope spec fails to build on a shape that gate does not model. The magnitude that
  grows here is the number of SCOPES a consumer's spec splits an extern surface across, not the
  number of consumers.

## Upstream close-outs (waiting on external releases)

Not cddl-codegen work: each entry is a PRUNE that becomes due when an external release ships a fix
the matrix currently carries fork-pinned provenance for (the gap state itself is current state in
`README.md` § "Upstream oracle gaps"). They are separated from the findings ledger above because
nothing here is a candidate fix — the code change already exists upstream or on the fork, and what
remains is deleting the notes that explain why we do not have it yet.

- When a release ships the `90f66ff` prelude-`number` float fix (README gap #7): prune the
  fix-provenance notes (README gap #7, the `prelude-number-float-accepts` /
  `prelude-number-tstr-rejects` fingerprint probes' provenance wording). Separately citable while
  in the neighborhood: bare floats against `time` (`#6.1(number)`) validate WITHOUT the tag-1
  wrapper (tag-leniency laxity, pre-existing and unchanged by the fix) — a candidate upstream
  report, not yet filed. A sibling ruby time-family laxity surfaced by the corpus decode mint:
  the generator mints calendar-INVALID `tdate`s (`0("5906-11-31t22:32:49-05:59")` — November 31
  does not exist) and the validator accepts them, while the local-fixes rust oracle rejects
  (RFC 3339 date validity — arguably the correct reading); repro + impact under "Adjacent
  observations" in `draft/rust-cddl-named-key-map-gap.md` (local note) — the same
  candidate-upstream-report disposition, not yet filed.
- When a release ships the `707c038` float-key/null member-key fix (README gap #10): prune the
  fix-provenance notes (README gap #10, the `float-key-accepts` / `null-key-rejects` fingerprint
  probes' provenance wording).
- When a rust `cddl` release ships the uint-target control-op fix (upstream PR submitted): prune
  README gap #1 and `draft/rust-cddl-uint-control-op-gap.md`.
- When a release ships the `773b723` array-sequence fix and the `Cargo.toml` pin moves back to
  crates.io: prune the fix-provenance notes (README gaps #2/#4, the draft repro table, the vector
  `reason` provenance).
- When a release ships the `885c61c` non-uint-endpoint range fix (upstream PR pending — PR material
  in `draft/rust-cddl-float-range-gap.md`): prune the fix-provenance notes (README gap #3, that
  draft, and the rangeop vectors' `reason` provenance).
- When a release ships the `2c7548e` radix-literal lexer fix (upstream PR pending — fix +
  35-test suite on the fork; `draft/rust-cddl-radix-int-literal-gap.md`): prune the fix-provenance
  notes (README gap #5, that draft). Separately track the WG spec question the fix spawned
  (cbor-wg/cddl#33, filed: radix-mantissa floats the unordered ABNF technically derives). Future
  radix-POSITION rows (occurrence bounds `0x2*0x4`, tag heads `#6.0x20`, …) additionally wait on a
  ruby oracle fix — ruby corroborates radix in value position only
  (`draft/radix-oracle-deviations-verdict.md`).
- `.size` on a signed `int` — semantics CLARIFIED by the RFC author (cbor-wg/cddl#32): a control
  distributes over `int = uint / nint`, and an undefined application (`.size` on `nint`) is a
  per-value non-match, so `int .size N` matches exactly the `uint .size N` window. cddl-codegen
  REJECTS the construct gracefully (the old `i{8N}` mapping mis-enforced the clarified window in
  both directions, and the rust oracle hard-errors on it, so an aligned implementation would be
  uncertifiable — pinned by `size_on_signed_int_rejects_gracefully`; scoreboard + detail in
  `draft/cddl-size-on-int-divergence.md`). Remaining upstream waits: (1) the rust CLI's hard error
  is a citable OVER-rejection bug — candidate `local-fixes` fork fix + upstream PR; once fixed,
  supporting `int .size N` as the uint window becomes certifiable (a `ctl.size.int` row becomes
  mintable) and the rejection can be revisited. (2) When the clarification lands as erratum/spec
  text, cite it in the `current_capacities.mdx` note.
- When a ruby `cddl` gem release parses inline/composite type2 controllers (`bytes .cbor [coords]`
  — gem 0.12.14 exit-65s at parse, so the whole containing spec becomes unjudgeable; repro +
  upstream steps in `draft/ruby-cddl-inline-composite-control-arg-gap.md`): remove the
  `ir_conformance_corpus` gate's `RUBY_EXPECTED_FAIL` entries for `cbor_wrapped_group_array` and
  `cbor_bignint_table` (their stale-ledger guards flip red by themselves once the divergence
  disappears) and prune that draft.
  Until then, rows/fixtures needing ruby corroboration for a control-arg construct must name the
  controller type — same caveat class as the ruby radix-position deviations
  (`draft/radix-oracle-deviations-verdict.md`).
- When a release ships the `c2ebf9f` bignum map-key/value-tag fix (upstream PR pending — fix +
  30-test suite on the fork; README gap #6): prune the fix-provenance notes (README gap #6, the
  `RUST_ORACLE_SKIP` past-resident note, the two `bignint-*` fingerprint probes' provenance
  wording).
- When the pinned rust `cddl` validator accepts `validate_cbor_from_slice("x = undefined",
  &[0xf7], None)` (README gap #16): the exact-signature stale probe in
  `ir_conformance_corpus` will fail first. Confirm the fixed behavior is the intended RFC 8949
  undefined value, remove the sole `(fixed_singletons, undefined_value)`
  `RUST_ORACLE_RULE_SKIP` triple so all eight emitted rust calls run again, and replace the
  stale-rejection preflight with an acceptance probe for the new pinned behavior. Then run
  `bun run check.ts full --only ir_conformance_corpus`, update the current-state coverage/cost
  wording in `tests/README.md` and README gap #16, and delete this close-out entry. A changed
  rejection signature is not a fix: investigate it and retain or revise the ledger only with a
  new exact probe; never promote this one-rule gap to fixture-level `RUST_ORACLE_SKIP`.
- When ciborium's generic value decoder preserves CBOR `undefined` (`f7`) distinctly instead of
  returning `Value::Null`, `reference_codec_differential_self_check` should fail stale. Teach
  `ciborium_value_to_tree` the new variant, remove the sole
  `CborTree::Undefined => CborTree::Null` normalization, retain `f7` as an ordinary equality
  self-check, update `tests/README.md` and README gap #16's reference-codec wording, and run
  `bun run check.ts full --only ir_conformance_corpus`. The independent Rust `cddl` decoder may
  still collapse `f7`; do not remove its per-rule oracle accommodation unless its own exact probe
  accepts.
- When a rust `cddl` release fixes fixed-byte CBOR validation (README gap #17): first confirm that
  `h'CAFE'` and raw UTF-8 fixed-byte rules validate without a panic, then remove the three pinned
  decode-foreign rows and re-mint them with
  `bun run verify.ts --mint-decode-foreign --only=value.bytes,contain.array-element.value.bytes,contain.map-value.value.bytes`.
  Re-run the full verify pass so their evidence gains normal two-oracle corroboration, and mint
  reason-bearing constraint rejects so all three rows move from Q4's unverified set to enforce-green.
  Then prune README gap #17 and `draft/b3-024c-rust-cddl-fixed-bytes-validator.md`. This closes an
  independent-certification gap only; do not demote the execution-gated support rows while the external release is
  being evaluated. Also verify that upstream `B16ByteString` display no longer treats decoded bytes
  as UTF-8; re-run the fixed-member and recombination regressions before simplifying the generator's
  lazy byte-safe diagnostic renderer. Until then, generated execution is the equality-enforcement
  pin; the missing independent oracle certification is explicit rather than inferred as `n/a`.
- When a rust `cddl` fix ships TAG-typed map-key validation (README gap #8 — OPEN at the pinned
  `ac1b98e` rev; differential repro, suspected `src/validator/cbor.rs` site, and prune steps in
  `draft/rust-cddl-tag-map-key-gap.md`, local note; no upstream issue filed yet): re-mint the row
  it blocks (`--mint-decode-foreign --only=contain.map-key.type2.tag` — its `pinned_reason`
  disappears once candidates survive the two-oracle gate), re-run the full `verify.ts` in the same
  change so the row's evidence picks up the corroboration clause, and prune README gap #8 + that
  draft.
- When a rust `cddl` fix ships NAMED-RULE / parenthesized-choice map-KEY validation (README gap
  #11 — OPEN at the pinned `ac1b98e` rev; differential grid, adjacent nested-map-VALUE and
  multi-entry-composite-array-key observations, and prune steps in
  `draft/rust-cddl-named-key-map-gap.md`, local note; no upstream issue filed yet): re-mint the
  corpus decode rows it blocks
  (`--mint-decode-corpus --only=c_style_enum_map_key.enum_keyed_map,table_enum_key.enum_keyed,table_enum_key.enum_key_holder`
  — the empty-instance-only rows pick up non-empty vectors, and the pinned/active flip-flop the
  gap causes stops, once candidates survive the two-oracle gate; if the fix also covers the
  adjacent observations,
  re-mint `wasm_nested_alias.passthru_tags_map` and the `composite_map_key` fixture in the same
  change), and prune README gap #11 + that draft.
- When a release ships the `3d56d8e` optional-entry/closed-map/JSON-type-domain-key fix (upstream
  PR pending — fix + regression tests + 21/12-cell differential grid on the fork; README gap #9):
  prune the fix-provenance notes (README gap #9 and the two `optional-entry`/`closed-map`
  fingerprint probes' provenance wording). The fork checkout's `future-issues/` files three
  still-open adjacent map-matching gaps found during that fix (a fourth, the float-key/null
  copy-paste, is since fork-fixed — README gap #10) — bundle them into the upstream conversation
  when convenient.
- When the rust oracle starts distinguishing the float prelude names at all (README gap #12 — OPEN
  at the pinned `ac1b98e` rev: all six names collapse to one "is it a float" test). The ruby gem
  0.12.14 already implements the shortest-form partition head-independently, so this is a
  one-oracle divergence and every affected `DECODE_REJECT_ORACLE_GAP_EXEMPT` entry names `rust`
  alone. Written up for filing, tracked by
  `cddl-matrix/upstream-reports/rust-cddl-float-name-blindness.md` — a committed, paste-ready
  report. Not filed. When a fix lands: drop `rust` from the affected entries (lib.ts) — the mint's
  stale guard names each one as soon as that oracle starts rejecting — remove the entries left
  naming no oracle, re-mint the five constrained float rows so their vectors carry ordinary
  two-oracle certification (`--mint-decode-foreign --only=prelude.float16,prelude.float16-32,prelude.float32,prelude.float32-64,prelude.float64`),
  and prune README gap #12, the family-(e) paragraph in `query_q4_directional.ts`, and the writeup
  itself. If instead the answer is that all six names denote the same set, the exemptions are wrong
  rather than stale: the shortest-form partition reopens, and the writeup's own closing section
  says exactly what moves.
- File the prepared typed-tag report for README gap #14:
  `cddl-matrix/upstream-reports/rust-cddl-tag-fixed-payload-acceptance.md` is committed and
  paste-ready, but no upstream issue has been filed. The catalog exemption remains deliberately
  exact: when rust begins rejecting `81cbf4`, remove
  `contain.group-choice-arm.type2.tag.fixed_array/81cbf4` from
  `DECODE_REJECT_ORACLE_GAP_EXEMPT`, re-mint that row so the hand vector returns to ordinary
  two-oracle certification, run the normal probe/fold, and prune README gap #14 plus the Q4
  family-(d) exception and the report. If only the adjacent `81ccf5` wrong-tag probe changes, update
  the report but keep the payload exemption until its own mint stale guard fires. If upstream rules
  the current acceptance correct, follow the report's explicit reversal branch instead of retaining
  the exemption.

## wasm-ABI & multifile placement matrices — remaining work

Current state — the grid, the three always-on axes (compile floor, round-trip, rust↔wasm API-surface
parity) with their emission-profile sweeps, the minted wasm test surface and its loud-skip list, and
the multifile placement sweep — is documented in `tests/README.md` (§ "wasm-ABI matrix", § "wasm-crate
test module", § "rust↔wasm API-surface parity", § "multifile placement matrix") plus the
annotations-table row in `README.md` (`verify.ts`'s default-on `--wasm` probe); the recombination fuzzer's wasm leg
(`recombination_wasm_crates_check`, `tests/README.md` § "Shape-recombination fuzzer") is the
composition-space cross-check that complements this matrix's curated per-shape grid. What remains:

- **Carry root-position references into the NON-structural reference positions.** The referencing
  module is now an axis: the `rootref` mode places the anon spelling in `lib.cddl` over the
  wrapper-minting shapes (`EXPECTED_ROOTREF_SHAPES`), so both halves of `mark_refs`' root behaviour
  are gated — a wrapper sole-owned by a scope module IMPORTED into root, and a wrapper the root
  holder owns minted AT root and named bare. What that leaves unenumerated is the same reference
  POSITIONS the b-side modes distinguish: a root-position reference to a NAMED rule and a
  root-position type-ALIAS target. They are deferred rather than built because root resolves a
  user-named rule through the ordinary scope import the `named`/`aliased` cells already exercise from
  `b`, so the added rows would repeat a seam rather than reach a new one — whereas the structural
  wrapper genuinely has a root-only spelling (the bare, import-free one), which is why that subset
  was the one built. Reopening signal, on the axis that made `aliased` worth adding: a root-position
  dangling-import class (E0412/E0425/E0432) fixed BY HAND after a consumer's regen — an observable
  its owner reports by construction, and one the seven green `rootref` cells put at zero today.
- **Keep EVERY matrix axis honest (periodic).** Grid coverage equals the hand-curated lists the
  projections carry — `SHAPES` × `ROLES` in `project_wasm_matrix.ts`, `SHAPES` × reference MODE in
  `project_multifile_matrix.ts` — and a hole in ANY of them is silent, not a red cell. A wasm
  representation not in `SHAPES` is an un-gated shape; an emitter path that places types in a
  boundary position not in `ROLES`, or a reference position not in the mode list, is an un-gated
  position. **The standing rule: a generator change that gives types a NEW way to cross the wasm
  boundary, or a NEW position to sit in, adds the shape/role/mode in the same change.** Beyond that
  rule the axes need a periodic sweep, and the sweep is worth running as four repeatable TELLS rather
  than as a re-read of the lists — every hole that has actually bitten (E0412/E0425/E0599 classes,
  one of them escaping to a production consumer's regen) was found by one of these, never by asking
  "is this list complete?":
  - **Fix-review reach analysis** — after a fix, ask which OTHER rule families the changed code path
    can reach. The shapes it reaches and the grid does not are the missing rows.
  - **A steering comment in a test** — a test that deliberately picks the easy variant ("uses an
    exposable key so no wrapper is minted") is a coverage hole wearing a disguise. Grepping for such
    steering is a cheap sweep arm.
  - **Hand-patched generated output in a consumer** — hand-added `use` lines, visibility flips, or
    `#[rustfmt::skip]` blocks living in a consumer's committed `src/generated/**`. Every regen
    deletes them, so each one is a workaround for a generator hole nobody filed; read the consumer's
    generated-output diffs for hand edits (read them — never regenerate a consumer to find them).
  - **The EXCLUSION comments, not just the missing rows** — a shape excluded because the compile
    floor cannot build it standalone (`extern` / `rawbytes`) bounds the GATE, not the generator, and
    a consumer regen has already broken inside exactly such an exclusion. An exclusion justified by
    the gate's execution model is never evidence the generator has nothing to get wrong there.
  One residual is recorded rather than enumerated, in `tests/TESTING_ROADMAP.md`: the
  `gcvariant` mode extension over the Record-resolving shapes plus a generation-only leg over the
  excluded shapes (§ "Multifile reference-POSITION coverage"). (Input REALISM — consumer-scale
  identifier length, a dimension no axis here enumerates — is not a residual here: the
  over-width-field class every short-named fixture kept unreachable is held by a standing
  cddl-codegen gate, the dense width ladder
  `integration_tuple_field_width_ladder_never_aborts_rustfmt`, whose step-1 rungs cannot let the
  measured 6-char rustfmt#5703 fatal window hide between fixtures.) One mechanical upgrade is armed:
  the multifile `SHAPES` list has drifted from the wasm projection's once, caught by doc-coherence
  review rather than by a gate — a SECOND such near-miss is the signal to make
  `project_multifile_matrix.ts --check` assert its `SHAPES` is a superset of the wasm projection's
  (minus its documented exclusions) instead of relying on review.
- **Third honesty axis — flag-gated EMISSION SURFACES × input mode (periodic, same footing as the
  SHAPES/ROLES rule above).** SHAPES/ROLES cover what types look like and where they sit; a whole
  flag-gated emission surface can still be built against single-file assumptions and break only
  under DIRECTORY input, invisible to every single-file gate. Proven instance: the `--emit-tests`
  test modules landed at the generated root naming submodule types bare, so EVERY multifile
  `--emit-tests` crate was E0433-uncompilable — no standing gate generated that flag under
  directory input, so the class surfaced only when the round-trip upgrade's recon exercised it
  (fixed; pinned always-on by `emit_tests_multifile_scope_imports`, executed at matrix breadth by
  `multifile_matrix_roundtrips`). The rule going forward: a NEW flag-gated emission surface must
  state its input-mode posture in the same change — exercised under directory input, or recorded
  single-file-only with the reason. Postures already settled, each with the pin that holds it:
  - `--emit-tests-conformance` × directory input is executed by `ir_conformance_multifile` — a
    multifile placement cell's non-root-module rule judged against the concatenated source spec, the
    spec-on-disk contract `docs/docs/command_line_flags.mdx` states for the flag.
  - The `--wasm-*-macro` modes × directory input are compile-gated by
    `wasm_macros_multifile_compiles` (per-module macro imports, invocations minted inside the
    submodule's own file, and the scoped rust path each invocation carries), which puts the
    input-mode axis under the compile-only verdict that is those surfaces' permanent posture
    (§ "Explicitly out of scope") rather than adding a behavioral row.
  - EXECUTION of the generated json-gen crate (`wasm/json-gen`) over cross-module types is covered
    by `multifile_json_preserve`, whose `run_test` leg runs the json-gen binary over the
    four-submodule `tests/multifile/inputs` tree and asserts the emitted document.

  A directory `--input` holding ONE `.cddl` file is not multifile in the sense this axis means —
  `api::with_types` splits scopes only when >1 file is found — so a single-file input directory
  discharges no input-mode posture. NOT on this list:
  `--common-import-override`, `--extern-wasm-crate`, `--extern-wrapper-index`, and the
  workspace-mode set `--workspace-dep`/`--wrapper-requests`/`--key-requests` are exercised
  under directory input by construction — their pins (`integration_tests::extern_deps*`,
  `extern_wrapper_index_defers_to_dep`, `workspace_dep_defers_to_dep`,
  `workspace_requests_hosts_borrowed_wrappers`, `workspace_regen_two_consumer_contract`, and the
  `Int`-under-override surfaces' `int_key_via_common_import_override_sidecar` + the `int` leg of
  `workspace_key_requests_derive_effect_and_hard_errors`) are
  directory-input fixtures, and the extern-deps
  mechanism they extend only exists under directory input. (`--no-synthesized-rust-collection-aliases`
  is emission-only alias suppression with no per-module placement logic; its pin is single-file,
  a deliberate posture.)

  A SIBLING axis with the same silent-hole character — per-wrapper emission
  MODE (local vs index-deferred vs workspace-borrowed vs requested-hosted) × wrapper shape ×
  reference POSITION, which neither SHAPES/ROLES nor this input-mode rule enumerates — is swept in
  the rust suite instead, by the wrapper-participation grid (`tests/README.md`
  § "The wrapper-participation grid"): a table-driven grid with always-on generation assertions and
  per-(mode, floor) compile/link gates. It stays there rather than moving here because every mode
  configuration requires directory input, which this differential scopes out by construction.

## Explicitly out of scope (decided, not overlooked)

Per the `QUERIES.md` query-map, no consumer query needs these (revisit only if a concrete need surfaces):
- **F8 — matching semantics** (prioritized-choice ordering, greedy occurrence, implicit `:` cut): a
  validation concern, not a cddl-codegen serialization concern.
- **F9 — interaction tuples** (A-in-B-in-role-C-with-operator-D): richer-than-binary containment; stretch
  query (Q7) only.
- **F10 / F11** — note-only (over-acceptance denominator; AST cross-check is weak corroboration).
- **Behavioral wasm-surface rows for the `--wasm-*-macro` modes** — those flags replace the whole
  wrapper method surface with user-supplied macro definitions, so an assertion over that surface
  would judge the FIXTURE's macro bodies rather than the generator's output. The compile verdict
  plus a loud skip (`tests/README.md` § "wasm-crate test module") is the permanent posture, decided
  2026-08-03. Reopening signal: a consumer-reported behavioral defect in a macro-mode wasm surface
  that the compile verdict passed.
- **Behavioral emitted-test cells for extern / raw-bytes ctor args** — compile-verdict-permanent,
  decided 2026-08-04, same class as the `--wasm-*-macro` posture. This is about whether the
  AUTO-MINTER (`emit_tests`) learns these classes, NOT about the feature going untested: hand
  fixtures already cover raw-bytes behaviorally on both sides (`tests/raw-bytes/tests.rs` and
  `tests/raw-bytes/tests_wasm.rs` round-trip through `RawBytesEncoding`, and the extern-generic
  fixtures round-trip `from_raw_bytes`) — state that distinction whenever this posture is cited.
  An extern type has no contract a harness could construct against; raw-bytes has a knowable trait
  door (`RawBytesEncoding::from_raw_bytes`) but no knowable accepted LENGTH (the in-repo `PubKey`
  takes exactly 32 bytes), so an emitted mint would be runtime-red against a correct generator.
  A harness-side opt-in hook is not an available middle ground: the def-file
  splice runs AFTER generation while the mint decision is taken DURING generation off the IR
  struct variant (`emit_tests::mint_struct` yields no `MintValue` for `RustStructType::Extern`
  beyond the reserved `Int`, nor for `RustStructType::RawBytesType`), so the only route is a
  user-supplied-hint FEATURE (DSL surface, red-first vectors, docs,
  matrix registration), which the "no test-harness metadata in production
  specs" reasoning rejects. Reopening signals: a consumer asking for minted extern/raw-bytes
  values (the request arrives carrying the valid-bytes knowledge the hint needs), or a
  consumer-reported wasm-boundary defect on such a ctor arg that the compile verdict passed.
- **Extern-interface seam sentinels** — the `; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1` header and
  `; unexported:` records that `--extern-import` input files carry beyond the two `ext.*` sentinels
  (strictly parsed at the seam; comments to the grammar). They are tool-interchange rather than
  user-authored CDDL, and their strictness is enforced where it binds — the consumer seam's
  hard-error pins `extern_import_unknown_version_hard_errors` and
  `extern_import_unknown_annotation_hard_errors` (`src/tests/extern_import_tests.rs`). A matrix row
  would restate that enforcement as model surface for constructs no user authors. Reopening signal:
  a concrete matrix consumer query that needs vendor-sentinel rows.
- **A containment-matrix row for the INLINE nested `.cbor` self-composition**
  (`bytes .cbor (bytes .cbor T)`, beside the four existing `contain.cbor-payload.*` rows). Declined
  because a row's whole job — keeping the construct's support claim measured — is already done at
  every layer a row would feed: the corpus cell (`ctl.cbor` × `cbor-payload` exercised, COVERAGE.md),
  the active decode-corpus vectors judged by both oracles
  (`cbor_payload_inline_nested` rows), and the recombination sweep's standing composition count
  (its `cbor_payload` template parenthesizes its hole, so the self-composition is swept as `ok`).
  No completeness lint obliges the row — all four projection gates pass without it — while its
  marginal cost is a full annotations sweep with no additional discriminating power. Reopening
  signal: a concrete matrix consumer query that needs the self-composition row — the same signal
  class as the seam-sentinel decline above.

## Pending decisions (need a human call)

- **Over-acceptance denominator:** the `class="over-acceptance"` vector class pins each CERTIFIED
  instance (the numerator; rows are enumerated per the "Intra-alternative variation rows" rule as
  instances surface). Still undecided is the DENOMINATOR for any completeness claim about that axis:
  should over-acceptance coverage be measured against the grammar's full superset, or only
  realistically-implementable constructs? (Affects only how completeness is reported — the pins and
  their Q4 projection are denominator-independent.)

(Resolved decisions — D3 corpus numerator; cddl-codegen comment-DSL as the `CDDL_CODEGEN` profile; doc
consolidation — are recorded in the code + git history; not re-litigated here.)

## Maintenance

Upstream specs churn (IANA registries, the grammar). Refresh with `sources/fetch.sh` (re-fetches + verifies
against `SHA256SUMS`); a checksum mismatch flags upstream drift to review before re-pinning and regenerating.

Hand-counted prose lists in this doc — a family-count header over a findings-ledger list, and any
sibling of one; cited here count-free and instance-free on
purpose, since a hard-coded example count is itself this rot class and one went stale exactly that
way — are maintained by review: pruning or adding a
family must update the count and keep the entry in the list whose framing matches its failure
stage (generation-failure vs layer-2 compile/round-trip). If a count or a mis-homed entry slips
through review again, fold these counts into `project_status_headers.ts`'s generated-counter
system (the same mechanism that already generates the status-header counts) instead of adding
another review rule. The class is not confined to this doc — one armed instance on record: the
wasm-ABI shape-family delivery (2026-07-21) grew the multifile skip ledger across three reviewed
commits while `tests/README.md`'s "Twelve cells" pinned-cell prose (count + enumeration) went
stale, caught only by a dedicated post-delivery doc audit, not by any of the six per-commit
reviews. A second armed instance is on record and it did NOT land in prose: the recombination
sweep's baseline outcome counts, carried as a source comment in `src/tests/recombination_tests.rs`,
drifted 42 `ok` compositions from a fresh measurement while every gate stayed green — the sweep's
vacuity floors sit far enough under the real numbers that the drift could not fire them. So the
trigger is the class, not the venue, and it sits on the axis the cost grows along: **the count of
hand-written MEASURED quantities that nothing re-measures**. Any such number a delivery has to
hand-correct is the signal to derive it from its source rather than re-audit it — a generated span
where the quantity is structural (`MULTIFILE_MATRIX_SKIP`'s count and enumeration), a committed
self-measuring datum where it is measured (the shape `tests/timings.json` already uses for
durations; the recombination sweep's own datum is DELIVERED as `tests/recombination-counts.json`,
held exactly by `recombination_generation_sweep` — see `tests/README.md`'s recombination section).

## Related

Broader testing work (clear-wins, roadmap items, the trailing-bytes runtime change) lives in
`tests/TESTING_ROADMAP.md` — the matrix is the reference that testing effort *projects from*.
