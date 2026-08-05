# CDDL master matrix — roadmap (remaining work + findings)

Read `README.md` first (the model + current state, including the durable gotchas and the
upstream-oracle-gap state). This doc is strictly the FUTURE state: matrix-side work, the
open-findings ledger, upstream close-outs, and pending decisions. The blow-by-blow of *how* done work landed lives in git history (this
doc is actively pruned of it; the project already did the same with the scale report + cold-critique).
Running the gates is not a roadmap concern either: `check.ts` at the repo root is the self-enforcing
gate registry + entry point, `tests/README.md` § "Running everything" is the prose overview, each
script's header docstring is the per-gate detail, and `QUERIES.md` documents the Q1–Q6 query scripts.

**Status: gate-green.** <!-- status-header counts are generated — regenerate with: cd cddl-matrix && bun run project_status_headers.ts --write --><!-- gen:sh:roadmap-counts -->120 features (95 RFC8610 + 1 RFC9682 + 24 `CDDL_CODEGEN` vendor profile), 132 containment cells, and 286 cddl-codegen annotations<!-- /gen:sh:roadmap-counts -->, all axes reconciled/deterministic, with
execution-gated support **per-feature, per-cell (role × feature), and per-control-op** (<!-- gen:sh:roadmap-ops -->all 37 IANA ops probed<!-- /gen:sh:roadmap-ops -->):
"supported" requires the generated crate's `--emit-tests`
round-trip/reject tests to PASS (`cargo test`), falling back to the compile verdict only for shapes that
mint no test surface (recorded honestly in the evidence). The orthogonal **emission axis is filled**
(every default-supported row carries a `preserve`/`json` verdict; <!-- gen:sh:roadmap-emission -->2 divergences, all `preserve`-side<!-- /gen:sh:roadmap-emission --> —
see § findings) and supported rows carry decode-foreign corroboration clauses (plus <!-- gen:sh:roadmap-constraint -->69 `class="constraint"` enforcement reject vectors over 49 enforce-green rows<!-- /gen:sh:roadmap-constraint --> — the enforcement
axis carries NO unverified rows and NO certified over-acceptances at HEAD: every supported row with a
rejectable constraint projects `enforce = yes (bounded-reject)` — the widened-occurrence-marker table
class is CLOSED (`+`/`1*` is honored as a non-empty container and the other count-permitting markers
are rejected gracefully, § findings); the green, unverified (empty), and over-accepts (empty) sets are
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

> **Findings ledger (F#)** from the cold critique (full write-up in git history): everything in scope is
> closed — the resulting machinery is current state in `README.md` (execution-gated support,
> § "Directional support evidence", § "Upstream oracle gaps") — except **F8–F11, out of scope** (bottom).
> Only still-open findings are sections below.

## Matrix-side work — the model and the projections it renders

The matrix exists to feed **many** consumers; corpus was just the hard flagship. What is left on the
matrix's own side — as opposed to in the generator, which is the findings ledger below — is of two
kinds: a defect in a projection (buildable now) and known incompletenesses of the coverage MODEL
(deferred, so each names the observable that would reopen it).

- **The fixed-value ARM position has no enforcement classification, and no evidence that would
  justify one.** `query_q4_directional.ts` classifies a fixed value in a MEMBER position as
  enforcement-bearing (`cellCarriesConstraint`, by the cell's role × feature), so a supported member
  cell landing without a reject vector reads `unverified (no reject vector)` and trips the
  unverified-set pin. Three roles are deliberately outside that classification because their
  rejection story is not value equality: `role.map-key` (a fixed key is looked up; the wrong key
  rejects as a missing required member), and `role.group-choice-arm` / `role.choice-member` (a fixed
  value selects an alternative, so the rejection is "no arm matched"). The arm cells themselves are
  now enumerated (`contain.group-choice-arm.type2.value.nint_array`,
  `contain.group-choice-arm.type2.tag.fixed_array`,
  `contain.choice-member.prelude.true.same_major_brute`, beside the fixed-key and fixed-member arm
  cells that predate them), and every one of them carries accept vectors only: enumerating the
  positions was never the same act as going and getting the rejection evidence, so classifying the
  two arm roles today would still assert evidence nobody holds.
  - **Reopening signal:** an arm-position reject vector landing in the catalog — the moment one
    exists, the arm rejection HAS a vector class, and leaving the role unclassified would let the
    next arm cell land vectorless with no pin drifting, which is the member-side defect all over
    again.
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
  is no longer a judgment call: a prelude construct declares the exact cell its head lands in, and
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

- **A component-face emission leg (the wasm probe's sibling), execution-verified.** The emission
  axis deliberately excludes the `component` profile because this pipeline's verdict is a
  rust-crate round-trip and `--component` leaves every rust byte identical (`README.md`, the
  annotations-table row) — true, and silent about the component face's OWN per-feature surface,
  which does vary: cddl-codegen's corpus compile gate ledgers shapes whose component glue does
  not compile (`EXPECTED_COMPILE_FAIL`, gate `component_corpus_compiles`), so a matrix row can
  read `supported` while the feature's component glue would not build. Compile breadth is already
  owned by that gate; what a matrix leg would add is exactly what the wasm leg added over compile
  checks: per-feature EXECUTION verdicts — build the emitted component crate per feature, drive
  its round-trip under a host, and thread `component_gen`/`minted_component`/`component_roundtrips`
  stage fields into the evidence with stage-named clauses, per the wasm probe's taxonomy.
  - **Reopening signal:** the first component-face defect that `component_corpus_compiles`
    passes — glue that compiles but miscompiles a round-trip — found by any downstream party.
    That is the defect class execution-gating exists for, it is measurable by whoever hits it,
    and the wasm leg's own history (compile-only verdicts masking miscompiles) is why the signal
    sits on this dimension rather than on ledger growth.

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
  that mis-mapping fail loudly.

## Findings — open (candidate cddl-codegen fixes; the matrix's actual payoff)

Every entry here is a defect or a missing capability in **this** generator that a probe, gate, or
fuzzer sweep surfaced — that is what the section means, and what the many `§ findings` citations in
`src/tests/` and `tests/README.md` resolve to. Two neighbouring classes live elsewhere on purpose:
what the generator does TODAY, including the boundaries it keeps permanently and the upstream-oracle
gap state, is current state in `README.md` (§ "Gotchas", § "Upstream oracle gaps"); prunes that wait
on an external release are § "Upstream close-outs (waiting on external releases)". New findings are
ledgered here (that's what the probe/gate error messages point at).

- **A transparent alias carrying a `@custom_serialize`/`@custom_deserialize` PAIR has two wire forms
  in one crate: every embed site routes through the pair, its own standalone codec does not.** Probed
  2026-08-04 while force-wrapping the `.cbor` rule bodies (which closed the structurally identical
  `.cbor` instance). `inner = uint ; @custom_serialize cs @custom_deserialize cd` emits
  `pub type Inner = u64;` — the pair replaces the codec of the type the alias RESOLVES to, at each
  position that reaches it, and mints no type of its own — so `Inner::to_cbor_bytes()` /
  `Inner::from_cbor_bytes()` are `u64`'s built-in codec while `[f: inner]` writes and reads `cs`/`cd`.
  Silent, and the crate compiles. Deliberately NOT fixed here: the transparent spelling is the
  DOCUMENTED contract for the pair (`comment_dsl.mdx` prescribes it as the remedy for the extern /
  raw-bytes marker, tag-rule and `@newtype` refusals — wrapping the alias would break each of those),
  and the extern-interface seam already treats such an alias as hazardous (`; unexported:` row). The
  invariant assert added with the `.cbor` fix therefore scopes to the `RustType`'s OWN `encodings`
  vec, never to `AliasInfo`-carried codec metadata. Documented as a caution in `comment_dsl.mdx`
  § "Reaching an annotated alias through another rule". **Reopening signal:** a consumer reporting
  that a pair-carrying alias's standalone `to_cbor_bytes`/`from_cbor_bytes` produced or accepted the
  built-in wire where their spec says the pair's — i.e. someone calling the standalone entry point at
  all, which is what nothing yet says anyone does.

- **A tagged PRESERVE table's standalone codec drops the tag.** The narrowed remnant of the class
  T1-13 closed (recorded 2026-08-04, narrowed the same day). Every tagged rule body now force-wraps
  — collections, the optional-tag idiom in both flavors, and `T / null` — so its standalone
  `to/from_cbor_bytes` write and require the tag exactly as its embed sites do. ONE combination
  cannot: `t = #6.n({* k => v}) ; @duplicates preserve`, whose inner is the `PairMap` vec-of-pairs
  twin. A wrapper cannot hold that inner — the register-side duplicates threading is scoped to
  `Array` deliberately, because the synthesized structural map wasm wrapper class wraps `BTreeMap`
  (probed: wrapping it fails the wasm crate with E0425 on a missing `PairMapU64ToText`), while a
  preserve-table ALIAS works under wasm only because the named rule itself becomes the `PairMap`
  class. Refusing the shape is not available either: it generates and compiles today, so a refusal
  would be a support regression. So `TaggedPreserveTable::to_cbor_bytes()` still writes a bare map
  while every embed site writes `write_tag(n)` first — the one-type-two-wire-forms shape, surviving
  in exactly one spelling. It is carved out of
  `IntermediateTypes::assert_no_wire_facts_survive_a_transparent_alias` BY NAME (a tag op on a `Map`
  base carrying `Preserve`, nothing else), so the exemption cannot silently widen. Wiring the
  PairMap-aware synthesized wasm wrapper class retires the carve-out, this entry, the
  `@duplicates`-on-`@newtype`-table parse rejection, and the two test carriers that depend on it
  (`declared_spelling_tests::encoding_operation_ownership_decides_whether_the_spelling_survives`'s
  SEAL half and `robustness_tests::stacked_tag_encoding_members_are_depth_disambiguated`'s flavor B,
  both of which say so in place). **Reopening signal:** the wasm per-kind wrapper work landing — or,
  before it, any spec in this repo that spells a tagged preserve table and uses it standalone rather
  than only from a holder.

- **Three control-operator arms ABORT (exit 101) where every sibling refuses gracefully.** Found
  2026-08-03 by the refused-name × resolution-context closure sweep
  (`src/tests/refused_name_closure_tests.rs`) enumerating the control-operator arms as separate
  contexts. `x = <name> .default 1` aborts at `RustType::default` for any head that did not resolve
  to a rust primitive matching the default's value class — which includes `tdate` (a SUPPORTED
  prelude name with no primitive) and, more sharply, every REFUSED prelude name, whose inert
  `Fixed(Null)` placeholder means a graceful rejection the walk had already RECORDED is destroyed by
  the abort before `finalize` can report it. `x = uint..10` (a typename as a range bound) and
  `x = uint .cbor uint` (`.cbor` on a non-`bytes` head) abort for every head, supported names
  included — refusing both shapes is right, but with a message, not a panic. Consumer-visible as an
  unexplained exit 101 on an ordinary spec typo. Each is a `PANIC` row of the input-robustness
  catalog (`tests/robustness/ctl_default_unmapped_head.cddl`,
  `tests/robustness/rangeop_typename_start.cddl`, `tests/robustness/ctl_cbor_non_bytes_head.cddl`)
  and the ten `.default` cells are pinned in that sweep's `KNOWN_CLOSURE_BREACH`. The matrix's own
  `ctl.cbor` / `ctl.default` / `rangeop.*` cells all spell heads that work, so the grid cannot see
  this boundary today.

- **A type-choice ARM or member declared as `bytes .cbor <alias>` changes NAME, not wire, once the
  alias survives to the arm.** Measured 2026-08-03 while fixing the wire half of the same seam (the
  alias's `@custom_serialize`/`@custom_deserialize` pair, which the arm now routes through in both
  directions). On a PAIRLESS alias the emitted CBOR operations are byte-identical before and after —
  `write_unsigned_integer` / `raw.bytes()` in the same order — but every NAME derived from the arm
  moves to the alias's: the rust enum variant (`ArmChoice::U64` → `ArmChoice::ScalarAlias`), its
  constructor and accessor (`new_uint`/`as_uint` → `new_scalar_alias`/`as_scalar_alias`), the wasm
  kind enum, and the WIT function names (`new-uint` → `new-scalar-alias`, the WIT TYPES staying
  `u64`). `output_format.mdx` § "Type spelling at member positions" is what makes that the correct
  spelling, and the same measurement found json-gen and the JSON schema byte-identical. Recorded
  here because it is an API-surface move a consumer regenerating across this change will see with no
  wire change to explain it.

- **`@duplicates reject` on a TABLE rule emits a `--component` guest crate that cannot compile.**
  The generated component glue lowers a WIT `list<tuple<k, v>>` parameter back to the rust member
  through `try_into()` whenever `wit::wit_param_despecialized` says the projection dropped a
  `TryFrom` door. That predicate reads `RustType::duplicates_reject()`, which inspects the policy
  flag WITHOUT looking at the container — so a MAP carrying `reject` (a policy that is a documented
  accepted no-op for tables: a loose table is key-unique by construction) reads as despecialized and
  the guest gets `cddl_lib::Holder::new(f.try_into().map_err(err)?)` for a plain `BTreeMap`.
  Probed: `tbl = { * uint => text } ; @duplicates reject` + `holder = [f: tbl]` under
  `--component=true`, generation exit 0 with empty stderr; `BTreeMap<u64, String>: TryFrom<Vec<(u64,
  String)>>` is unsatisfied (rustc E0277, `From<[(u64, String); _]>` is the only near miss it
  offers). The sibling predicate `wit_param_validates` reads the same flag one function above and
  its own doc comment already states the correct rule — "A plain table is NOT in this class: a
  `BTreeMap` carries no invariant a `list<tuple<K, V>>` can violate" — so the fix is to make BOTH
  predicates' `duplicates_reject()` reading container-aware (the `reject` twin is an ARRAY shape;
  `is_reject_ordered_set` already carries exactly that guard) rather than to special-case the glue.
  Two-seams context, because it decides how far the fix reaches: the ANONYMOUS inline table's row
  slot (`parsing::apply_inline_table_row_metadata`) deliberately does NOT store an explicit `reject`
  for this reason, so the inline spelling is clean today and only the NAMED table's rule slot
  (`register_rust_struct`'s `HomogenousMap` arm, which stores whatever the policy says) reaches the
  defect — a fix at the predicates lets the inline seam store the policy faithfully and drop that
  carve-out. This is the ships-noncompiling-output class, so it takes no reopening signal: the
  tool must not emit a crate that cannot build, and an exit-0/empty-stderr run that does is the
  exact thing the honesty invariants exist to make loud. What is owed is fix-or-refuse now (the
  refusal being a loud rejection of `@duplicates reject` on a table under `--component`), with the
  feature-shaped half — honoring a WIT projection of the preserve/reject twins generally — parked
  only if its own signal is honest. No gate covers it: `component_corpus_compiles` builds the
  component corpus, and no corpus spec pairs a table with `@duplicates reject`, so the fixing
  commit owes that fixture.

- **Say each rejection once, so the count of messages is the count of problems.** A single
  offending construct can report the same rejection twice: `a = [{x: int}]`, `a = [[int]]` and
  `x = bytes .cbor ({a: int, c: uint})` each print their message two times, while the same defect in
  a map-value position (`m = { outer: { a: int, c: uint } }`) and the older group-choice rejections
  print it once. The duplication is positional — the parse walk visits an array-element / `.cbor`-
  controller type2 twice — and it is not new behaviour: the walk always did this, and the `panic!`
  that used to sit at those sites aborted on the FIRST visit, so nothing ever reached the second.
  Converting them to record-and-continue is what made a pre-existing double-walk observable, which
  is the general shape to expect from any further abort→rejection conversion.
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
- **Real incremental choice extension (`/=` type-choice, `//=` group-choice) is a candidate
  feature.** Extending an already-defined ident is rejected gracefully at the `api.rs` pre-scan
  (pinned by `incremental_choice_extension_rejects_gracefully`; the initial-definition-via-`/=`
  spelling keeps generating), to avoid the silent last-wins arm drop the old parse produced. Real
  support means merging the arms, and the plausible route is an AST-level pre-pass at that same
  seam: concatenate the extension rules' `type_choices`/group choices into the base rule before
  `parse_rule` ever sees them, so the existing `parse_type_choices` path does the rest. Known
  hazards to design for (why the rejection shipped instead of a quick merge): the `ParentVisitor`
  is built over the ORIGINAL AST, so synthesized/merged nodes must keep resolving for the
  comment-DSL walks (`get_comment_after` ascends node identity); per-arm `RuleMetadata` from each
  statement's comments must merge, not drop; and a cross-module extension (base and `/=` statement
  in different scope-marked files) needs an ownership decision before it can generate. On
  implementation: flip the `assignt.extend`/`assigng.extend` reject rows (verify.ts re-probe),
  re-mint their decode rows, and retire the rejection + this entry.
- **Honor non-final and `+`/bounded count-permitting occurrences on heterogeneous ARRAY-record
  fields.** A **final-position** `* t` after ≥1 fixed member is now an open-array rest tail
  (captured `Vec`, or dropped under `@ignore`; user doc: `docs/docs/output_format.mdx` § "Open
  arrays"), enumerated supported by `contain.occurrence-target.grpent.member.zero_array`. Still
  rejected gracefully: a `+` or bounded (`n*m`) final tail —
  `contain.occurrence-target.grpent.member.plus_array` — and any **non-final / middle** `*` member
  (`[uint, * bytes, tstr]`); those boundaries are pinned by
  `occurrence_on_array_record_field_rejects_gracefully`. The rejection avoids the silent
  exactly-once narrowing that makes a generated decoder reject spec-valid repetition counts —
  invisible to round-trip tests, surfaced only by spec-derived decode vectors. Real support for the
  middle-position case is one decode-disambiguation design shared with the findings entry
  "Decode-disambiguate a non-final `?` optional array-record field whose CBOR major types OVERLAP a
  later field's": the emitter already peeks major types for optional fields; the missing signal is
  the remaining definite-length COUNT (repeats = len − fixed), plus the shared
  residue/indefinite-length policy decisions that entry spells out.
- **Real bounded `?` / `n*m` table cardinality is a candidate feature.** A count-permitting occurrence
  marker on a single non-literal arrow map entry no longer silently widens to an unbounded `*` table
  (the removed bug: the table-detection arm ignored the entry occurrence and `HomogenousMap` — unlike
  `HomogenousArray` — carried no bounds, so the generated decoder wrongly accepted out-of-window maps).
  Now `+` / `1*` is honored as a `NonEmptyMap<K, V>` whose single `TryFrom` door rejects the empty map
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
- **Extending type-level ("two-type") constraint enforcement beyond the `+` occurrence is a
  candidate feature family.** The shipped model (`static/non_empty.rs` / `static/non_empty_map.rs`;
  user-facing contract in `docs/docs/output_format.mdx` § "Non-empty containers") makes the invalid
  state unrepresentable and funnels construction through a single `TryFrom` door — but only for the
  lower-bound-exactly-1 container shape. The remaining constraint classes still enforce via runtime
  checks that a `pub` field or direct mutation can bypass (the bypassability the `+` work removed):
  - **Bounded containers** (`[2*5 T]` / `*n` arrays — runtime-checked today — and the rejected
    `?`/`n*m` table spellings owned by the sibling entry above): `BoundedVec`/`BoundedMap` statics
    as SIBLINGS of the non-empty types, not a generalization the non-empty types alias into — Rust
    cannot vary a method's fallibility by const parameter, so a `MIN=1, MAX=∞` instantiation would
    force `Result` onto the shipped infallible `push`/`insert`. The mechanical `Min{N}`/`Max{N}`
    wasm naming and the same conversion contract still slot in without redesign; the bounds-general
    API rule (a length-changing operation is checked iff it can cross a bound; value-level `&mut`
    is unrestricted at every bound shape) and the pickup plan live in
    `draft/two-type-constraint-enforcement.md` § "Support for more complex occurrences" and the
    bounded-occurrence residual in `tests/TESTING_ROADMAP.md`.
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
  `SHAPES` in the same change (the axis-honesty rule below).
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
- **A wasm-face alias-hop cycle ENTERED at the plain-typename rule panics.** `hop_alias = hop_arr`
  with `hop_arr = [* hop_alias]` aborts generation under `--wasm=true` at `is_enum`'s
  registered-or-generic assertion (`intermediate/mod.rs`); with `--wasm=false` the same spec
  generates and compiles. Rule ORDERING is the ingredient, not the shape: spelling the same cycle so
  the collection sorts first (`x = [* y]` with `y = x`) generates on both faces, because the
  collection's element reference then resolves through the alias table instead of staying a nominal
  `Rust(ident)` naming an `Array` struct. That is the same ingredient
  `tests/robustness/collection_rule_cycle_entry.cddl` isolates on the TABLE side, where the three
  generation sites that dispatch on such a reference were taught to recurse into the collection's
  structural type — the wasm NAME derivation is a fourth site the same fix never reached. Pinned as
  a tracked-known `PANIC` row by `tests/robustness/recursive_alias_hop_collection_entry.cddl`;
  flipping that row to `ok` is the fix, and it is independent of the recursive-type boundary (it
  reproduces identically with `; @newtype` written by hand, and reproduced before the boundary
  existed). **Reopening signal**, on the magnitude axis: a consumer's committed spec contains ≥1
  alias-hop cycle whose plain-typename rule sorts first — i.e. the count of rules its owner must
  reorder or rename to generate a wasm face reaches 1. Today that count is 0 in every consumer spec;
  the repro is a synthetic probe.
- **A fixed-value inner under the `T / null` Option-collapse (`true / null`) has no member
  REPRESENTATION, so a spec that pins one arm of a nullable to a constant must be hand-rewritten.**
  A two-arm choice with a null arm never becomes an enum: the collapse lowers it to an `Option<T>`
  (the same lowering that makes `t = null / tstr` an `Option<String>`). With `T` itself a fixed
  value there is nothing to put in the `T` slot — a `Fixed` is unstored, carrying meaning only as a
  member whose value the schema pins — so BOTH collapse sites refuse the shape gracefully rather
  than building an unrenderable `Optional(Fixed(..))`: the rule-level site (`parse_type_choices`)
  names the rule by its source spelling, and the member-level one (`rust_type`'s two-arm null
  checks, reached by `[v: true / null]`, `{? k: true / null}`, `{* uint => (true / null)}`) is
  role-generic. Both quote the offending value back in CDDL form and offer the one remedy probed to
  work — widening the fixed arm, `bool / null` lowering to `Option<bool>` — with the standing caveat
  that widening drops the constraint, so it is a different spec. Pinned by
  `tests/robustness/choice_fixed_null_collapse.cddl` and its `_member` sibling, which are hand
  fixtures because the recombination sweep cannot spell the shape (its ingredient list carries
  `prelude.null` as a choice-member but no special-literal filler for the other arm). Probe scope:
  every fixed KIND the collapse can carry — bool (`true`/`false`), uint, nint, float, text, and the
  degenerate `null / null`, which gets its own sentence because it has no non-`null` arm to widen —
  each exit 1 under BOTH the default and `--preserve-encodings` profiles. Not probed: the
  `--wasm=true` emission path (the refusal is recorded in the parse walk, before any emission
  profile is consulted, so profile-independence there is reasoning rather than measurement).
  `true / false` — no null arm, so no collapse — is a DIFFERENT path that generates an ordinary
  `pub enum T { True, False }`, and is not this item.
  What remains open is REPRESENTATION: a fixed-value inner carries only PRESENCE, so a
  presence-shaped lowering (a newtype whose `Option` is the whole information content) would make
  the shape generate instead of refuse. It stays deferred because a refusal naming the shape and a
  working remedy costs its owner one rewritten rule, while a presence lowering adds a
  representation to every downstream surface (rust, wasm, json) that nobody has yet asked for. The
  shape is still uncatalogued as a matrix cell. **Reopening signal**, on the magnitude axis: a spec
  brought to us contains a fixed-value/null two-arm choice, i.e. the count of rules its owner must
  hand-rewrite to keep generating reaches 1; today the evidence is synthetic probes only.
- **Enumerate the FLOAT fixed-value kind, in both the arm and the member position.** The kind axis
  and the position axis are separate cells, but they share one reason to be enumerated at all: the
  result is known to be NON-uniform (probed kinds keep behaving differently from `uint`), so these
  are rows with a known payoff rather than a speculative sweep. Float is the kind left: it used to
  wait on the preserve-mode float stub, and that stub is retired — floats preserve their head width
  in every position — so both rows are buildable now, with `tests/corpus/optional_fixed_float.cddl`
  as the member-position precedent. The remaining kinds are done or deliberately excluded: bool and
  null are covered arm-side by `tests/corpus/group_choice_fixed_special.cddl`, nint and the
  tag-wrapped form by `contain.group-choice-arm.type2.value.nint_array` /
  `contain.group-choice-arm.type2.tag.fixed_array`, the same-major-type pairing by
  `contain.choice-member.prelude.true.same_major_brute`, and the member position by
  `contain.array-element.value.number.nint`; `undefined` and the byte-string literal have no member
  representation at all (their own entries below).
  The kind axis has
  a second dimension the cells must spell deliberately: the DISPATCH PATH. A choice's arms reach
  either the type-match dispatch or (when arms share a CBOR major type) the brute-force
  try-each-arm path, and the two emit independently, so a kind row probed on one path certifies
  nothing about the other. A float arm is a major-7 special, so it shares its major with `true` /
  `false` / `null`: the arm-side row must be spelled to state which path its example takes rather
  than inheriting the type-match assumption the integer kinds' rows record.
  The `n*m` marker on the cardinality boundary is omitted rather than deferred — the refusal
  message names `*` / `+` / `?` / `n*m` from one site, so a fourth row would model the same code
  path the three markers already reach.
- **`undefined` has no member REPRESENTATION, so a spec that constrains a position to it must be
  hand-rewritten.** `undefined` is refused gracefully in every position — `[v: undefined, x: uint]`,
  `{ k: undefined, j: uint }` and the rule body `x = undefined` all exit 1 naming the type, under the
  default and `--preserve-encodings` profiles alike (pinned by
  `undefined_prelude_rejects_gracefully_in_every_position` and
  `tests/robustness/undefined_member.cddl`). That refusal is the correct posture and is not the
  deferred work; what is deferred is REPRESENTING the constraint. `undefined` differs from its
  major-type-7 siblings `true` / `false` / `null` in exactly one way that matters here: those have a
  `FixedValue` and so ride the member-position flip (`README.md` § "Gotchas"), reading and verifying
  the constant on deserialize without storing it, while `undefined` has none — giving it one is the
  work. The widening a user can reach for today is not equivalent: `any` carries an arbitrary CBOR
  item (`undefined` included) but constrains nothing, which is a different spec. The cells
  `contain.array-element.prelude.undefined` and `contain.map-value.prelude.undefined` keep the member
  half visible beside the rule-body row. Reopening signal on the magnitude axis: a spec brought to us
  contains an `undefined` member, i.e. the count of members its owner must hand-rewrite to keep
  generating reaches 1; today the entry's evidence is synthetic probes only, and a synthetic probe
  costs nobody a rewrite.
- **The `eb*` expected-conversion tags advertise a rendering, not a type, so a spec that uses one
  must widen it by hand.** `eb64url` (`#6.21(any)`), `eb64legacy` (`#6.22`) and `eb16` (`#6.23`)
  each wrap an ARBITRARY CBOR item in a tag whose whole content is advice to a consumer rendering
  that item as text; the payload is `any` and the tag constrains nothing a generated type could
  hold. All three are refused gracefully in every position — `x = eb64url`, `[v: eb64url, x: uint]`
  and `{ k: eb64url, j: uint }` alike exit 1 naming the type and its tag under the default and
  `--preserve-encodings` profiles (pinned by
  `any_content_prelude_tags_reject_gracefully_in_every_position`,
  `tests/robustness/eb64url_member.cddl` and the `tests/matrix_reject/prelude.eb64url.cddl` row
  and its two siblings).
  That refusal is the correct posture and is not the deferred work; what is deferred is
  REPRESENTING the tag. With `any` first-class the mechanical route would be the prelude-expansion
  path the supported tags already ride (`eb64url` → `#6.21(any)`, as `encoded-cbor` →
  `#6.24(bstr)` does today), and the open design question is what the emitted type does with the
  advice once the payload is an opaque item. The widening a user can reach for today is not
  equivalent: `any` carries the item but drops the tag and the conversion it advertises, which is
  a different spec. Their fourth sibling `cbor-any` (`#6.55799(any)`) shares the refusal but not
  this entry — its support is a decided permanent exclusion (`tests/TESTING_ROADMAP.md` § North
  star's exclude list), the self-describe tag being a property of a byte stream rather than of a
  value. Reopening signal on the magnitude axis: a spec brought to us uses one of the three names,
  i.e. the count of rules its owner must widen to `any` to keep generating reaches 1; today the
  entry's evidence is synthetic probes only, and a synthetic probe costs nobody a rewrite.
- **A byte-string literal has no fixed-MEMBER representation, so a spec that pins a member to a
  literal must be hand-rewritten.** `[v: h'0102', x: uint]`, the unkeyed `[h'0102', x: uint]`,
  `{ k: h'0102', j: uint }` and the UTF-8 spelling `[v: 'text', x: uint]` are all refused gracefully
  (exit 1, naming the construct) in both profiles, as is the rule body `x = h'0102'` — position no
  longer changes the outcome, only the wording. Member identity is pinned by
  `unsupported_member_type2_rejects_gracefully` and
  `tests/robustness/bytes_member.cddl`. The deferred work is the REPRESENTATION: `FixedValue` has no
  bytes variant, so the fixed-member path the uint / text / bool kinds already use — verify on
  deserialize, store nothing — has nothing to verify against. Widening the member to `bytes`
  generates but stops constraining the value, so it is a different spec, not a workaround. The cells
  `contain.array-element.value.bytes` and `contain.map-value.value.bytes` model both positions. NB
  two spellings never reach the generator at all — upstream, the rust `cddl` parser rejects `b64'…'`
  outright and rejects LOWERCASE hex digits inside `h'…'` (`magic = h'cafe'` dies at parse as
  `Invalid base16 encoding` in every position, though ABNF `HEXDIG` is case-insensitive and the ruby
  reference accepts it — the evidence class `tests/matrix_reject/value.bytes.cddl` records). That
  gap caps the represent fork's value: real-world magic constants are typically spelled in lowercase
  hex, so a `FixedValue::Bytes` delivery generates nothing for them until the fork accepts the
  spelling — the upstream parser fix is part of the feature's real cost, and belongs in the same
  delivery or its docs. Reopening signal on the
  magnitude axis: a spec brought to us contains a byte-string fixed member, i.e. the count of members
  its owner must hand-rewrite reaches 1 — today only synthetic probes reach the site.
- **Decode-disambiguate a non-final `?` optional array-record field whose CBOR major types OVERLAP
  a later field's.** `a = [ ? f0: uint, f1: uint ]` generates at exit 0 and the crate builds, but
  `f0` gets no decoder: the refusal is recorded (`dont_generate_deserialize`, loud
  `Not generating A::deserialize()`) and propagates honestly through every consumer — containing
  enums of both flavors lose their own `Deserialize` transitively, and both `--emit-tests` minters
  skip the type with a named line. Position per se is NOT the gap: the emitter peek-disambiguates
  optional array-record fields against every reachable follower's `cbor_types`, so a type-disjoint
  non-final `?` (`[ ? f0: uint, f1: tstr ]`) works today. Real support shares ONE design with the
  middle-position `*` occurrence entry above: add the remaining definite-length COUNT to the
  existing PEEK signal (repeats = len − fixed), decide the genuinely ambiguous residue (count+peek
  admitting ≥2 assignments — the wire does not carry WHICH assignment produced the bytes, the same
  loss a type choice's overlapping arms have; that one is settled as first match, and this residue
  should settle the same way), and decide the
  indefinite-length story (no count signal there; peek-only makes acceptance encoding-dependent).
  **Reopening signal:** a spec author reports a rule they cannot decode at all — i.e. the refusal
  reaches a type they need on the wire, not merely a shape a fuzzer composed.
- **Make a SAME-CHAIN nested `bytes .cbor` payload GENERATE.** A `.cbor` payload whose own target
  carries a second `.cbor` control in the SAME op chain — `bytes .cbor (bytes .cbor uint)`, and
  equally the named-alias spelling `innerc = bytes .cbor uint` / `b: bytes .cbor innerc`, which the
  `.cbor` single-alias strip flattens into the identical chain — is **refused gracefully at parse
  time**, keyed on two `CBORBytes` in one `RustType`'s chain (exactly the broken set), at both seams
  that can apply the operation: the rule-BODY registration and `rust_type_from_type1`. Pinned by
  `nested_cbor_payload_rejects_gracefully` plus the outcome rows
  `tests/robustness/cbor_payload_same_chain_inline.cddl` and
  `tests/robustness/cbor_payload_same_chain_alias.cddl`. The refusal is what the
  no-uncompilable-crate invariant demands; SUPPORT is what remains.
  Support needs the payload-framing NAMES to become depth-aware, the way the tag path already
  threads a `tag_depth`: a `cbor_depth` threaded the same way. That is the whole fix, and it is
  sized by what breaks without it — the serializer emits two `<var>_inner_se = Serializer::new_vec()`
  bindings at one name, so the inner `finalize()` moves the binding the outer write then uses
  (`error[E0382]`); the deserializer's depth-agnostic names collide the same way (two `<var>_bytes`
  frames, a same-block `inner_de` shadow the sequel statements would capture); and under
  `--preserve-encodings` the encoding-sidecar minting recurses under one name and declares the
  struct field twice (E0124/E0062).
  The boundary the refusal draws is narrow, and everything outside it stays supported: nesting
  through a NAME-CHANGING recursion gets a fresh fn scope and therefore its own buffer — the
  long-standing named-struct form (`cbor_in_cbor` in `tests/core/input.cddl`), the `; @newtype`
  boundary the rejection message advertises (`inner = bytes .cbor uint ; @newtype`, referenced as
  `bytes .cbor inner`, emits the same two-level wire shape and builds), and, since the CBORBytes arm
  reads its byte string from the stream that reached it rather than a hardcoded `raw`, a payload's
  own collection elements/values (`bytes .cbor [* bytes .cbor uint]` and the map-value twin; pinned
  by `tests/corpus/cbor_payload_nested.cddl`, `cbor_payload_nested_payloads` in
  `tests/core/tests.rs` against hand-derived RFC 8949 bytes, and `cbor_nested_payloads` in
  `tests/preserve-encodings/input.cddl`).
  **Reopening signal:** a CDDL specification a consumer must implement but does not control (a
  published wire format, not one they can rewrite) contains one or more `bytes .cbor` members whose
  payload type itself carries a `.cbor` control, directly or through a named alias. That is a `grep`
  over a spec they already hold, and the count of such members is the size of the hand-written
  serialization they would have to maintain alongside the generated crate.
  The recombination sweep does not supply this signal today, but it is one character from doing so,
  and that is the cheap way to reach the shape rather than a reason it cannot be reached. Its
  `cbor_payload` builder composes `bytes .cbor {h}` unparenthesized, so a self-composition spells
  `bytes .cbor bytes .cbor uint` — which is not merely unparsed by our front end but **illegal
  CDDL**: RFC 8610's grammar is `type1 = type2 [S (rangeop / ctlop) S type2]`, so a control
  operator's right-hand side is a `type2`, and `bytes .cbor uint` is a `type1`. The parse rejection
  is the `cddl` crate behaving correctly, not a front-end gap. Parenthesizing that builder
  (`bytes .cbor ({h})`) makes the RHS a `type2` and the self-composition legal, which would put the
  shape in front of the sweep's classifier — worth doing deliberately, with the composition churn
  that implies, rather than treating the shape as unreachable. With the refusal in place the sweep
  would classify it `error (graceful)` and route it nowhere, so what that buys is a standing count
  of how often the composition arises across the sweep's own vocabulary, next to the consumer-side
  count above.
- **Real support for the anonymous nested MAP in a type position** (`a = [{x: int, y: uint}]`, and
  its map-value / `.cbor`-payload / `/`-choice / generic-argument / occurrence-target /
  group-choice-arm siblings). The abort is gone — every one of those shapes now rejects gracefully
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
  (`t = [ (uint, tstr) // bytes ]`, and the map-rep `t = { (a: uint) // b: tstr }`). Both now reject
  gracefully in place of the former abort, pointing at the equivalent named form
  (`pair = (uint, tstr)`, then `t = [ pair // bytes ]`) — pinned by
  `inline_group_choice_arm_rejects_gracefully`, with the map rep's outcome category held by
  `tests/robustness/inline_group_choice_arm_map.cddl`. Support means minting the arm's struct from
  the parenthesized group the way a named plain group already is. Lower value than the anonymous-map
  entry above because the remedy is fully equivalent here (naming the group changes nothing on the
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
  key domain. Real float-key support (e.g. an ordered-float wrapper) is a candidate feature only if a
  consumer justifies it.
- Nint/float fixed map keys are **rejected gracefully** — only uint and text fixed keys are
  implemented on the struct-map record path (pinned by
  `contain.map-key.memberkey.value.{nint,float}_colon_single`,
  `contain.map-key.memberkey.value.{nint,float}_colon_multi`, and the matching group-choice-arm rows
  `contain.group-choice-arm.memberkey.value.{nint,float}_map` in `tests/matrix_reject/`). The
  printed remedy differs by kind: for nint the table `{ * nint => v }` in its own rule keeps
  generating; for FLOAT the table form is itself rejected (the float-family table-key boundary
  above — floats have no total order), so the float message advertises an integer/text key instead
  of the dead-end table (asserted by the `float arrow key` remedy check in
  `src/tests/robustness_tests.rs`). Real nint key support is the candidate feature (float key
  support is owned by the float-table-key boundary entry's ordered-float question); flipping either
  row to `ok` requires real support, not a decay back to the old `group_entry_to_field_name`
  panics.
- **A whole-TABLE custom codec has no spelling.** `@custom_serialize`/`@custom_deserialize` on a
  table rule is refused (the table lowers to a transparent map alias that owns no codec to
  override), and the advertised remedies reach only the parts: the row's KEY rule, the row's VALUE
  rule, or `_CDDL_CODEGEN_EXTERN_TYPE_` for the whole type by hand. Honoring the pair on the rule
  itself means threading `rule_metadata` through `AliasInfo`, which doc-lookup, the
  extern-interface projection and alias suppression all read — each needs its own re-audit, which
  is why the refusal ships first. Reopening signal, measurable by whoever has the problem: a spec
  whose table needs ONE codec over the whole map (a framing the key and value codecs cannot
  compose — a length prefix, a whole-map digest, a non-map wire form) and whose author reports
  reaching for the extern escape to get it.
- **Real nint support is ONE cross-cutting candidate feature — its per-shape gaps are enumeration
  cells of the matrix, not separate tasks.** Nint intersects every containment role (fixed map
  keys — rejected gracefully above; table domains and `@newtype` bounds — work; bare values, json,
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
  consumer justifies the feature, new
  nint shapes land as graceful rejections + enumeration cells; when one does, the work is the
  runtime/emitted-type design plus the upstream literal-width question, not IR plumbing — then
  flip the pinned rejection rows (record path first, then the group-choice arm).
- **A CBOR tag over a type-choice enum is unimplemented under `--preserve-encodings`** — a non-float
  preserve gap, now bounded by a refusal rather than a crash. `t = #6.10(int / tstr)` (and the
  group-choice spellings, and the all-fixed one this profile denies the C-style lowering) is
  **refused gracefully in `IntermediateTypes::finalize`**, on the struct-KIND walk, keyed on exactly
  the predicate the tagged-enum serialize path's `assert!(!cli.preserve_encodings)` has — that assert
  is reached from precisely two places, the `TypeChoice` and `GroupChoice` arms of the rust-struct
  dispatch, and stays in place as the guard that re-earns the retired panic-ledger entry. Pinned by
  `tagged_anonymous_choice_rejects_gracefully_under_preserve`.
  SUPPORT is what remains, and its shape is why it is parked: the tag belongs to the enum RULE while
  the encoding metadata preserve records is per-VARIANT, so support means giving the tag a home of
  its own on the enum (the serialize path's standing `TODO: how to even store these?`). Everything
  around it already works and is what the refusal points at: tags over structs/arrays/maps; the same
  choice NAMED and tagged by name (`inner = int / tstr`, `t = #6.10(inner)` — a tagged wrapper over
  the enum, verified to build and round-trip byte-exact through a generated preserve crate, including
  a non-minimal `d8 0a` tag head and an indefinite-length text arm); and a tag over a NAMED c-style
  enum at a member (`t1: #6.42(myenum)` with `myenum = 0 / 1 / 2`), whose encoding rides the owner's
  sidecar through the inlined dispatch (`tests/corpus/cbor_enum_payload.cddl`). Without
  `--preserve-encodings` the anonymous form generates.
  **Reopening signal:** a consumer's own wire format (one they do not control) tags an anonymous
  choice AND that consumer needs `--preserve-encodings` — i.e. they must re-emit bytes they decoded.
  Both halves are things they can check against a spec and a build flag they already hold; either
  alone is served today, by the naming remedy or by the default profile. Recorded by the
  decode-conformance replay gate's preserve leg (skip-listed in `PRESERVE_SKIP`, stale-guarded), by
  the emission axis (`contain.tag-content.type.choice` → `emission.preserve = unsupported`) alongside
  `prelude.number` / `prelude.time` and the two float-range wrapper rows
  `rangeop.{inclusive,exclusive}.float`, and by `EXPECTED_GENERATION_FAIL` in the wasm API parity
  sweep, which pins `tests/core`'s `tagged_type_choice` on the preserve leg.
- **Extern compile coverage at BREADTH — every extern corpus/matrix cell is still compile-exempt.**
  Every extern corpus row is compile-gate-exempt (`COMPILE_GATE_EXEMPT` — extern references
  user-supplied code) and the multifile matrix carries the same permanent extern exclusion. The
  recorded cost is closed at the hand-fixture level: the extern-only-scope undeclared-module break
  (a non-root scope whose rules are ALL `_CDDL_CODEGEN_EXTERN_TYPE_` got its `mod.rs` emitted with
  the re-export glue but no `pub mod <scope>;` in the generated root — E0432 wherever another scope
  referenced its types) is fixed (declarations derive from the post-glue scope map), string-pinned
  by `integration_extern_only_scope_declared_in_root`, and the full multi-scope composition —
  extern-only scope, hand definition, crate-root re-export — now `cargo check`s in a gate
  (`facade_composition_compiles`, the documented facade consumer built over exactly this shape).
  What remains is BREADTH: that gate compiles ONE hand-curated shape, so an extern break in any
  OTHER corpus/matrix shape still ships unseen. The mechanical catch is the def-splice the compile
  gate already does for `rawbytes` cells (`append_raw_bytes_defs`): seed a trivial extern
  definition + crate-root re-export so extern cells stop being compile-exempt — shared machinery
  with the extern half of "Mint the two remaining unminted wasm-surface classes" below.
  This entry OWNS the compile side of the emitted-check/schema-row class (execution is
  `tests/TESTING_ROADMAP.md`'s extern-deps wasm-boundary entry); two requirements, each justified
  by a shipped compile-error class no gate compiled (since fixed, content-pinned by
  `json_gen_extern_schema_rows` and `extern_interface_check_skips_generic_base_without_instances`):
  (1) the def-splice compile scope must include the emitted `wasm/json-gen` crate under the json
  profile — the bare generic-extern-BASE rows (E0107) and extern-DEP-owned schema rows (E0433)
  were plain compile errors in a crate the extern exemption kept un-built; and (2) the vendor
  extern markers need their intra-alternative variation rows enumerated FIRST (the expansion rule
  above applied to the `CDDL_CODEGEN` profile): a generic extern base WITH vs WITHOUT instances,
  and the generic raw-bytes base (`foo<T> = _CDDL_CODEGEN_RAW_BYTES_TYPE_` — still broken, repro
  ledgered in `tests/TESTING_ROADMAP.md`; the likely honest fix is parse-time rejection, which
  lands it as a reject row rather than a compile cell).

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

## wasm-ABI & multifile placement matrices — remaining work

Current state — the grid, the three always-on axes (compile floor, round-trip, rust↔wasm API-surface
parity) with their emission-profile sweeps, the minted wasm test surface and its loud-skip list, and
the multifile placement sweep — is documented in `tests/README.md` (§ "wasm-ABI matrix", § "wasm-crate
test module", § "rust↔wasm API-surface parity", § "multifile placement matrix") plus `README.md` §
annotations (`verify.ts`'s default-on `--wasm` probe); the recombination fuzzer's wasm leg
(`recombination_wasm_crates_check`, `tests/README.md` § "Shape-recombination fuzzer") is the
composition-space cross-check that complements this matrix's curated per-shape grid. What remains:

- **Enumerate the multifile matrix's referencing-MODULE axis (root-position references).** Every
  reference mode places the referencing `bholder` in module `b`, so a reference FROM THE ROOT scope
  (`lib.cddl`) — which takes `mark_refs`' distinct emission-scope/no-import paths — is systematically
  unenumerated. Proven consequential: before the shared `wasm_collection_wrapper` helper, a
  root-scope anonymous use of a map shape sole-owned by a scope module named the structural class
  bare with no import (never witnessed red; corrected incidentally by the shared-helper change, so
  the root side of that path has no red→green vector). Candidate: a `rootref` mode (the `bholder`
  lands in `lib.cddl`) over the wrapper-minting shape subset, with the same participation-pin idiom
  (`EXPECTED_*` lists) the `anon` mode uses.
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
  identifier length, a dimension no axis here enumerates — is no longer a residual: the
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
  single-file-only with the reason. Known surfaces never generated under directory input today, to
  exercise (a multifile cell/profile) or record as a deliberate posture when next touched:
  `--emit-tests-conformance` (the corpus fixtures are all single-file), the `--wasm-*-macro` modes
  (their compile gates use single-file inputs), and EXECUTION of the generated json-gen crate
  (`wasm/json-gen`) against cross-module types (the multifile `json`-profile sweep compiles
  rust+wasm; the json-gen runner gate is single-file-only). NOT on this list:
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
  a deliberate posture.) A SIBLING axis with the same silent-hole character — per-wrapper emission
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
  decided 2026-08-04, same class as the macro-mode posture above. This is about whether the
  AUTO-MINTER (`emit_tests`) learns these classes, NOT about the feature going untested: hand
  fixtures already cover raw-bytes behaviorally on both sides (`tests/raw-bytes/tests.rs` and
  `tests/raw-bytes/tests_wasm.rs` round-trip through `RawBytesEncoding`, and the extern-generic
  fixtures round-trip `from_raw_bytes`) — state that distinction whenever this posture is cited.
  An extern type has no contract a harness could construct against; raw-bytes has a knowable trait
  door (`RawBytesEncoding::from_raw_bytes`) but no knowable accepted LENGTH (the in-repo `PubKey`
  takes exactly 32 bytes), so an emitted mint would be runtime-red against a correct generator.
  Why an earlier ruling flipped, recorded so this reads as consistent rather than a reversal: the
  2026-08-03 opt-in-hook ruling rested on a cost premise `6ce3b6e0` falsified — the def-file
  splice runs AFTER generation while the mint decision is taken DURING generation off the IR
  struct variant (`emit_tests::mint_struct` yields no `MintValue` for `RustStructType::Extern`
  beyond the reserved `Int`, nor for `RustStructType::RawBytesType`), so no harness-side hook can
  exist and what remained was a user-supplied-hint FEATURE (DSL surface, red-first vectors, docs,
  matrix registration), which the original ruling's own "no test-harness metadata in production
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

One deliberate NON-firing on record, so the next delivery inherits the judgement instead of
re-deriving it: the Q4 enforce-green enumeration (`README.md` § "Directional support evidence
(Q4)") absorbed the 2026-07-28 fixed-value member-cell growth (28 → 43 rows) as ONE grouped family
clause — the TOTAL lives in the generated `gen:sh:readme-enforce-green` span, and no existing hand
count had to be corrected, so by this section's own trigger the enumeration stays hand prose: each
family clause carries certification rationale (which oracle gaps shape "certified" for that
family) that no generated span can derive. The quantities to watch there are the family-INTERNAL
counts ("the 15 fixed-value MEMBER cells", "the eight `rangeop` rows"): the first delivery that
must hand-correct one of those is this trigger firing for that enumeration.

## Related

Broader testing work (clear-wins, roadmap items, the trailing-bytes runtime change) lives in
`tests/TESTING_ROADMAP.md` — the matrix is the reference that testing effort *projects from*.
