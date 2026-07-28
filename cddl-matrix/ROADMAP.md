# CDDL master matrix — roadmap (remaining work + findings)

Read `README.md` first (the model + current state, including the durable gotchas and the
upstream-oracle-gap state). This doc is strictly the FUTURE state: remaining work, the open-findings
ledger, and pending decisions. The blow-by-blow of *how* done work landed lives in git history (this
doc is actively pruned of it; the project already did the same with the scale report + cold-critique).
Running the gates is not a roadmap concern either: `check.ts` at the repo root is the self-enforcing
gate registry + entry point, `tests/README.md` § "Running everything" is the prose overview, each
script's header docstring is the per-gate detail, and `QUERIES.md` documents the Q1–Q6 query scripts.

**Status: gate-green.** <!-- status-header counts are generated — regenerate with: cd cddl-matrix && bun run project_status_headers.ts --write --><!-- gen:sh:roadmap-counts -->117 features (95 RFC8610 + 1 RFC9682 + 21 `CDDL_CODEGEN` vendor profile), 98 containment cells, and 249 cddl-codegen annotations<!-- /gen:sh:roadmap-counts -->, all axes reconciled/deterministic, with
execution-gated support **per-feature, per-cell (role × feature), and per-control-op** (<!-- gen:sh:roadmap-ops -->all 37 IANA ops probed<!-- /gen:sh:roadmap-ops -->):
"supported" requires the generated crate's `--emit-tests`
round-trip/reject tests to PASS (`cargo test`), falling back to the compile verdict only for shapes that
mint no test surface (recorded honestly in the evidence). The orthogonal **emission axis is filled**
(every default-supported row carries a `preserve`/`json` verdict; <!-- gen:sh:roadmap-emission -->7 divergences, all `preserve`-side<!-- /gen:sh:roadmap-emission --> —
see § findings) and supported rows carry decode-foreign corroboration clauses (plus <!-- gen:sh:roadmap-constraint -->46 `class="constraint"` enforcement reject vectors over 28 enforce-green rows<!-- /gen:sh:roadmap-constraint --> — the enforcement
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

## Remaining work

The matrix exists to feed **many** consumers; corpus was just the hard flagship. What's left:

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
- **Extern-interface seam sentinels — decide whether they're matrix surface.** `--extern-import`
  input files carry vendor constructs beyond the two `ext.*` sentinels: the
  `; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1` header and `; unexported:` records (strictly parsed at
  the seam; comments to the grammar). They are tool-interchange rather than user-authored CDDL, so
  rows may be out of scope — but they ARE consumed input surface. A deliberate yes/no belongs
  here rather than silent omission.

## F4 / F5 follow-ons (only when their consumer exists)

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
  `one_float = [v: float]` asserts `fb…` doubles, so `prelude.float64`'s cell is exercised in fact
  while no rule names `float64`, and the row reads ✗ with its cell listed untested. Closing it means
  modelling which arm of a union the generator picks at emission — a claim about the generator, not
  about spec structure, so it cannot be derived from the pinned prelude and must not be guessed; the
  honest fixes are a golden rule that names the narrow type, or an emission model. *Reopening
  signal:* a vector written to close one of these ✗ rows turns out to assert bytes an existing vector
  already asserts — wasted authoring work, measured by the author who did it, and growing with the
  number of skewed rows rather than with the number of consumers. The entry records that the case
  exists; it records nobody having paid for it yet, so the signal can still fire.
- **F4 (tag registry):** deliberately not pinned/enumerated (cddl-codegen is tag-parametric). Revisit only
  if a *tag-semantic* consumer of the master appears.

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
  lands — a worked FULL cycle: `contain.occurrence-target.memberkey.type1.plus_table` was enumerated
  with an out-of-window empty-map over-acceptance pin while the table-marker-widening bug stood, then
  promoted to `class="constraint"` (`enforce = yes`) when `+` became a `NonEmptyMap` (`4fa3041`), so
  both branches of the rule are demonstrated, not just the acceptance side. The same rule applies to DISPATCH variations, not just
  enforcement — precedent row: `contain.group-choice-arm.grpent.member.record_array_tagged`
  (`t = [ a: tg // b: tstr ]`, `tg = #6.10([x: uint])`), which pins the TAG head of a struct-level
  tagged record as an array-rep group-choice arm member. The arm discriminant routes a non-embedded
  record arm through `RustType::cbor_types`, whose struct-tag branch dispatches on Tag; before this
  row, every enumerated record-arm gate (`contain.group-choice-arm.grpent.member.record_array` and
  the wasm-matrix `struct`/`generic`/`ralias` gchoice cells) was untagged, so an alternative
  discriminant that hand-mapped `record.rep` (always Array/Map, never Tag) would have gone green
  through every existing gate while mis-dispatching tagged-record arms — this row is what makes
  that mis-mapping fail loudly.

## Findings — open (the ledger of candidate fixes; the matrix's actual payoff)

The durable gotchas and the upstream-oracle-gap state are CURRENT state and live in `README.md`
(§ "Gotchas", § "Upstream oracle gaps"); this section holds only what is still to do. New findings
are ledgered here (that's what the probe/gate error messages point at).

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

**Upstream close-outs (waiting on external releases):**
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

**Bugs / gaps surfaced as findings (candidate cddl-codegen fixes):**
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
  middle-position case needs decode lookahead: a repeated-item run bounded by the following fields'
  types, i.e. peek-type disambiguation.
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
- **Give a NON-TERMINABLE recursive type a boundary instead of an advisory notice.** A cycle whose
  every path back to itself closes through a mandatory, non-collection member has infinite size in
  Rust: `mdmap = { * text => mdmap }` (the self-reference is the map VALUE, so the emitted typedef is
  `pub type Mdmap = BTreeMap<String, Mdmap>`) and `md = mdrec / int` with `mdrec = { a: md }` both
  generate at exit 0 and then fail `cargo check`. Terminable recursion is supported and unaffected
  (`current_capacities.mdx` § "Recursive types"). The generator already detects the cycle — dep_graph
  prints `Recursive type: … code will possibly need to be edited by hand to use Box/etc` — so what is
  missing is not detection but a boundary: an advisory notice leaves a spec that needs hand-editing
  indistinguishable at the CLI from one that does not, and the hand edit lands in `src/generated/**`,
  which every regeneration clobbers. Deliverable shape: box the cycle-closing member automatically,
  or promote the notice to a rejection naming that member. Reopening signal: a consumer's committed
  spec contains one such cycle — i.e. the count of cycle-closing members they must re-Box after each
  regeneration reaches 1. Today that count is 0 in every consumer spec; the two repros above are
  synthetic probes minted while fixing an unrelated defect, and a synthetic probe costs nobody a
  re-edit.
- **A BOOL or NULL fixed value in a map-rep group-choice arm emits invalid Rust under
  `--preserve-encodings`.** `t = { a: true // b: tstr }` and `t = { a: null // b: tstr }` generate
  and `cargo check` clean under the default profile, but under `--preserve-encodings` the emitted
  source is rejected by the rustfmt post-pass with `expected pattern, found '='`, which `export`
  turns into a fatal "rustfmt failed on the generated source" error — a generator bug, not a
  formatting preference. The uint and TEXT kinds (`a: 0`, `a: "v"`) are clean in both profiles, so
  the break is specific to the value kinds whose preserve-mode encoding sidecar differs. This
  predates the arm gaining default-profile support (reproduced against the parent commit), and the
  support is what makes it reachable in practice rather than hidden behind an earlier abort.
  Unpinned, and the reason is itself the gap: the failure lives in the rustfmt/export seam, which
  `api::generated_strings` never runs, so no in-process test can see it — pinning it needs a
  disk-write (`generate_to_disk`) vector, which no current robustness layer mints. Reopening signal
  on the magnitude axis: the count of fixed-value KINDS a consumer cannot use in a preserve crate is
  2 of the 5 spellable today (bool, null), measurable by anyone generating such a spec.
- **Enumerate the fixed-value group-choice arm's remaining VALUE KINDS.** The arm is supported in
  every profile now, but the committed vectors are `uint 0` plus the three kinds probed on delivery
  (text clean, bool and null broken under preserve — the entry above). Nint and float constants, and
  the tag-wrapped forms in the ARRAY reps, are unprobed in both directions. This is enumeration work
  with a known NON-uniform result rather than a speculative sweep, which is what makes it worth
  rows: two of the four kinds probed so far behave differently from `uint`. Reopening signal: the
  next kind that a consumer finds differs — the ratio is 1-in-2 so far, so the cost of not
  enumerating is paid per spec, not per consumer.
- Fixed-value member containment still has unenumerated variants beyond the precedent rows
  `contain.array-element.prelude.{true,null}`, `contain.map-value.prelude.false`, and
  `contain.array-element.type2.tag.{fixed_null,fixed_bool}`. Remaining candidates include map-value
  tag-wrapped fixed members and the other fixed prelude constants across array/map member positions,
  so the emission axis probes each fixed-value kind × wrapping position × profile instead of relying
  on the hand fixtures (`tests/robustness/fixed_bool_member.cddl`,
  `tests/corpus/fixed_bool_member.cddl`). This is the "Intra-alternative variation rows" expansion
  rule (this doc) applied to the fixed-value member role: enumerate the wrapped variants as rows
  BEFORE trusting a green.
  The variation axis is CARDINALITY as well as wrapping, and that half is what a
  position-keyed enumeration misses: a fixed value is supported in exactly-once placement and
  refused under a count-permitting occurrence, so every fixed-value kind needs rows on both sides
  of that boundary. Proven by the shapes this rule would have named in advance — `[* 5]` and its
  map sibling `{ * uint => 5 }` both reached generation as PANICs, and the map one had no row at
  all until it was found by hand next to the array one.
- **A nint fixed-value mismatch reports the CBOR wire representation, not the authored value** —
  `Key` (static/error.rs) has no signed variant, so the emitted check for `? neg: -3` renders
  `FixedValueMismatch { found: Key::Uint((neg_value + 1).unsigned_abs()), expected: Key::Uint(2) }`:
  a user sees "Expected fixed value 2" for a spec that says `-3` (read-caught during the
  optional-fixed-value delivery; the mandatory path shares the spelling, so this is the fixed-nint
  error-rendering convention, not an optional-path bug). Behavior is correct — only the message
  misleads. Candidate fix: a signed `Key` variant (or signed rendering for the nint arm) so the
  message names the authored value; any reason-asserted decode vector pinning the current "2"
  spelling flips loudly with it. Cosmetic until a reason-asserted reject vector or a consumer
  report makes it user-visible; enumerating a wrong-value reject vector for a nint fixed member is
  what would surface it systematically (the vector author confronts the rendered message at
  pin-authoring time).
- **A bare `any` type-choice arm in a NON-LAST position** (`a = any / tstr`) is a permanent graceful
  rejection, recorded here so the decision is not re-litigated: a bare `any` accepts every CBOR
  item, so any arm after it is unreachable dead code
  ("`any` arm makes later arms unreachable — move it last"). A LAST-position bare `any` arm is
  supported (forced-backtracking dispatch); a tagged `any` arm (`#6.n(any)`) is not a catch-all and
  is allowed in any position. Non-last rejection pinned by `tests/robustness/choice_any_arm.cddl`,
  last-position support by `tests/robustness/choice_last_any_arm.cddl`. The matrix has no
  containment cell for the shape, which is the coverage gap the fuzzer exists to find.
- **Five compile/round-trip-class families remaining from the recombination fuzzer's layer-2 sweeps**
  (`recombination_crates_execute`: generation is ok, but the generated crate fails `cargo test`
  under `--emit-tests`, default profile). Generation-outcome catalogs cannot see these, so each
  class is held in the sweep's `LAYER2_KNOWN_BAD` cited ledger (desc-keyed, vacuity-guarded — a
  fixed class flips loudly) with THIS entry as its pin — except the one entry below marked
  UNPINNED, whose pinning composition left the sweep's reach; each is a candidate cddl-codegen fix:
  - **A non-final `?` optional field in an array record breaks compilation** (E0599:
    `from_cbor_bytes` trait bounds unsatisfied — the `Deserialize` impl is not emitted):
    `a = [ ? f0: uint, f1: uint ]` and any `[? x, …more…]` variant. Optional-LAST array fields
    (`[uint, ? bytes]`) compile and round-trip — the gap is the position, needing decode lookahead
    like the count-permitting occurrence entry above.
  - **An array-rep group-choice arm containing a `?` optional member breaks compilation** (E0599:
    the arm struct's `deserialize_as_embedded_group` is not emitted): `t = [ ? f0: uint, f1: uint // tstr ]`.
  - **Wire-ambiguous type-choice arms cannot round-trip variant identity** (emitted round-trip
    asserts `variant N in == variant N out`, but first-match decoding maps every overlapping value
    to the earliest matching arm): duplicate/equivalent arms (`tstr / tstr`, `text / tstr`), a
    subsuming arm (`uint / tstr / bytes / tstr`, `int .ne 0 / tstr / tstr`,
    `[ ga: -10...10 / tstr // tstr ]`), and payload/type overlap (`bytes .cbor uint / tstr / bytes`
    — a valid-CBOR byte string matches the `.cbor` arm first). Candidate fix: reject duplicate arms
    at generation, or document first-match semantics and have `--emit-tests` skip
    variant-identity asserts for ambiguous choices.
  - **The `--emit-tests` minter does not respect `.ne` on a table key domain**: for a
    `*`-spelled table (`gen<{ * int .ne 0 => uint }>`, verified at HEAD) it mints key `0`, which
    the (correct) emitted decoder rejects with a `RangeCheck` — a minter-side gap, not a decoder
    bug. UNPINNED at HEAD, the one exception to this family's ledger rule: its `LAYER2_KNOWN_BAD`
    pin retired because the fuzzer's pinning composition (the no-occurrence spelling
    `gen<{ int .ne 0 => uint }>`) rejects gracefully at generation under the no-occurrence
    arrow-entry rejection (`5ef7ed0`; its generic-instantiation reach is pinned by
    `generic_arg_no_occurrence_table_rejects_gracefully`), while the
    sweep's `map_key` template has no `*`-spelled variant to re-reach the minter. Re-pin by adding
    a `*`-spelled map-key template (or a hand `--emit-tests` fixture over
    `{ * int .ne 0 => uint }`) when this gap is picked up.
  - **An emitted-test baseline decode failure on a nested shape**: a
    `bytes .cbor float64` member fails its baseline re-decode (`Expected(Special, Text)` at the
    following field — a `.cbor` float payload mis-frames the buffer; still to minimize when picked
    up).
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
- Four comment-DSL candidate fixes surfaced by the `src/tests/dsl_position_tests.rs` position sweep
  (held in its `KNOWN_SILENT_DROP` pin list — pinned, not fixed; the pins flip loudly when a fix
  lands): (1) `@name` at a MEMBER-position anonymous inline group never reaches the naming site
  (`get_comment_after(type2)` ascends only through Type1/TypeChoice), so the "Anonymous groups not
  allowed" rejection fires despite its message advertising `@name` as the remedy — the remedy DOES
  work in choice-member position; either make the member-position comment reachable or scope the
  rejection message's advertised remedy to the positions that can honor it. (2) `@doc` on a fixed-value (dataless C-style enum) type-choice
  variant is captured into the IR but never emitted — data-carrying variants render the `///` fine.
  (3) `@raw_bytes_flavor` on a NON-generic `_CDDL_CODEGEN_EXTERN_TYPE_` rule is silently accepted
  as a no-op — the extern-only validity gate rejects only non-extern rules while the docs scope the
  tag to extern GENERICS, and a non-generic extern has no instances to flavor; either hard-error
  the non-generic case or document the inertness deliberately. (4) `@used_as_elem` in field
  position is silently dropped — the field-trailing comment binds to the field's
  `trailing_comments`, which the rule-level metadata detector never reads, so no wrapper is minted;
  either make the field comment reachable or hard-error it, so the tag never silently no-ops.
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
- `float16` / float-choice aliases unsupported while `float32/64` work; generics on
  plain groups rejected. The historical blocker rationale ("no native Rust f16") is RETIRED as of
  2026-07-23: the dcSpark `cbor_event` fork the main crate now pins ships lossless software
  f16/f32↔f64 conversion plus width-carrying endpoints (`float_sz` / `write_float_sz` /
  `smallest_float_sz`, NaN payloads preserved) — already exercised by the `AnyCbor` runtime type's
  property layer (`src/tests/any_cbor_tests.rs`), and the generated-crate template now git-deps that
  same fork rev (the `_sz` float endpoints are present in generated crates). The remaining float work
  is entirely generator-side: the preserve-mode `unimplemented!` stubs in generation/deserialize.rs
  and the `float16`→`F32` alias folding in parsing. Under
  `--preserve-encodings` the float gap is positional, and the emission axis
  records it honestly: a bare `float`/`float32`/`float64` alias still generates and compiles
  (`emission.preserve = supported`, but compile-only evidence — the synthetic embed holder panics
  generation, so floats **as members** are the broken shape), while the choice-carrying prelude types
  `number` / `time` panic outright (`emission.preserve = unsupported`). An OPTIONAL fixed FLOAT member
  (`[? f: 2.5, …]`, `{? amount: 1.5}`) rides this same class: default and json generate the `bool`
  presence field (`tests/corpus/optional_fixed_float.cddl`, hand vectors in `tests/core/tests.rs`'s
  `opt_fixed_member_float`), but `--preserve-encodings` aborts at the float deserialize stub. Its
  preserve leg is ledgered in `feature_corpus_compiles`'s `EXPECTED_GENERATION_FAIL`,
  `feature_corpus_roundtrips_nondefault_profiles`'s `SKIP`, and the decode replay `PRESERVE_SKIP`.
  Landing the `preserve_encodings_supports_floats` encoding-var work retires all these preserve-float
  member gaps together.
- **A CBOR tag over a type-choice enum is unimplemented under `--preserve-encodings`** — a non-float
  preserve gap: `t = #6.10(int / tstr)` panics generation at the tagged-enum serialize path's explicit
  `assert!(!cli.preserve_encodings)` (its own `TODO: how to even store these?` — the per-variant encoding
  metadata has no home on the enum). Tags over structs/arrays/maps preserve fine. Surfaced by the
  decode-conformance replay gate's preserve leg (skip-listed there in `PRESERVE_SKIP`, stale-guarded)
  and now recorded on the emission axis (`contain.tag-content.type.choice` →
  `emission.preserve = unsupported`), alongside `prelude.number` / `prelude.time` and the two
  float-range wrapper rows `rangeop.{inclusive,exclusive}.float` (the wrapper wraps an f64 member,
  hitting the same native-float-under-preserve `unimplemented!`).
- Two float-adjacent **deliberate graceful rejections — boundaries to keep, not gaps to close
  blindly**: `.ne` over a float (the integer min>max exclusion hack has no principled float
  encoding) and a decimal bound on an integer-primitive head (`uint .le 10.5` — silently flooring
  it onto the int head would mis-enforce). Both route through `record_rejection`; pinned alongside
  the float-window enforcement in the `tests/core` `float_bounds` fixtures.
- **A `@custom_json` type produces a non-compiling json/wasm surface standalone (the same
  can't-compile-standalone class as `dsl_custom`).** `@custom_json` intentionally omits the serde
  derives on the rust type (the user is expected to supply custom json (de)serialize code), but the
  generated code still assumes serde is present on the json/wasm surfaces: the wasm wrapper emits
  `to_json` / `from_json` gated only on `--json-serde-derives` (`create_base_wasm_struct`), which
  require `T: serde::Serialize + Deserialize`, so a `--wasm --json-serde-derives` build fails
  `E0277` (`cddl_lib::Cj: serde::Serialize is not satisfied`); and any `serde_json::to_string(&T)` over
  the rust type likewise won't compile. Surfaced by the json/wasm decode-surface legs on the
  `dsl.custom_json` matrix row (`cj = uint ; @newtype @custom_json`), which is on both `JSON_SURFACE_SKIP`
  and `WASM_SURFACE_SKIP` citing this entry. Candidate fix: gate the wasm `to_json`/`from_json` emission
  (and treat the rust json boundary) on whether the type actually carries serde derives, delegating to
  the user's custom-json hook otherwise — the json analogue of how `@custom_serialize` is already
  threaded. Until then it is a standalone-compile limitation, not a round-trip bug.
- **A NON-STRING map key can't cross the `--json-serde-derives` json boundary.** JSON object keys must
  be strings; `serde_json` stringifies integer keys but hard-errors on a byte-string or composite
  (array/map) key (`serde_json::to_string` returns `Err`, "key must be a string"). So a map keyed by
  `bytes` or a composite type — spec-valid CBOR the decoder accepts — is not json-serializable through
  the generated serde derives. Surfaced by the json/wasm decode-surface legs of `corpus_decode_replay`
  on `bytes_map_key.bkeys`, `bytes_map_key.bytes_key_holder`, and `composite_map_key.holder`, which are
  on that gate's `JSON_SURFACE_SKIP` citing this entry (which also suppresses their wasm `from_json`
  sub-leg). Largely a JSON-format limitation rather than a codegen bug; a candidate mitigation is a
  generated serde impl that hex/base64-encodes non-string keys into json strings (and reverses it), at
  the cost of a non-obvious wire mapping — decide before building.
- **A present-null OPTIONAL field round-trips differently through json than through CBOR re-encode.**
  For `[pre: uint, ? field0: (uint / null)]`, the accept vector `[0, [824, null]]` (the optional field
  PRESENT and null) decodes fine, but the direct CBOR re-encode DROPS the null (`v.to_cbor_bytes()` =
  `[0, [824]]` — present-null normalized to absent), while the json round-trip PRESERVES it
  (`serde_json` → `from_str` → `[0, [824, null]]`). The two decode surfaces disagree about the
  present-null-vs-absent distinction for an optional field whose inner type is itself nullable. Surfaced
  by the json/wasm decode-surface legs of `corpus_decode_replay` on
  `nullable_nested.nullable_optional_field` (on that gate's `JSON_SURFACE_SKIP` citing this entry). The
  candidate fix needs a call on which representation is canonical (a faithful codec should preserve the
  distinction — the CBOR re-encode dropping the present null is the likelier defect) and a matching
  serde/serialize alignment; until then the row's json round-trip can't assert `to_cbor_bytes` fidelity.
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
- **Keep BOTH matrix axes honest (periodic).** Grid coverage equals the hand-curated `SHAPES` ×
  `ROLES` lists in `project_wasm_matrix.ts` — and a hole in *either* axis is silent, not a red cell. A
  wasm representation not in `SHAPES` is an un-gated shape; equally, an emitter path that places types
  in a boundary position not in `ROLES` is a silent hole (the E0599 bounded-wrapper-arm bug lived
  exactly there — it needed the per-variant `tchoice-variant` role to surface, while `bwrap` was a
  `SHAPES` entry all along). A generator change that gives types a NEW way to cross the wasm boundary,
  or a NEW position to sit in, must add the shape or role in the same change. The standing questions "which representation, and
  which boundary position, are we *not* enumerating?" deserve a periodic sweep regardless — and the
  rule extends to the MULTIFILE matrix's own `SHAPES` list (`project_multifile_matrix.ts`), which
  has a proven instance: the collection-of-records shape (`[* <record>]`, the only array whose wasm
  representation needs a generated structural wrapper) was missing from `SHAPES`, so the Array-arm
  placement class stayed invisible to every gate until review of the Map-arm fix asked what the new
  alias recursion could reach — now enumerated as the `collrec` cells (all green: `collrec__anon`/
  `collrec__aliased` under the emission-scope element registration, `collrec__named` once the `Alias`
  arm stopped minting a structural-wrapper import the named rule's own class subsumes).
  A SECOND multifile-`SHAPES` instance is on record with a different discovery signature — an
  adjacent test STEERING AROUND the hole rather than a fix's review: the non-exposable-KEYED table
  (`{ * <record> => text }`, whose wasm `keys()` names a root-minted keys-list wrapper) was in
  neither `SHAPES` list while `wasm_collections_index` deliberately used exposable `uint` keys to
  dodge the then-unfixed keys-list import dangle — a steering comment in a test is a coverage hole
  wearing a disguise, and grepping for such steering is a cheap arm of the periodic sweep.
  Enumerated as the `tblrec` cells (red, E0425-pinned) and fixed (`register_root_keys_list` at both
  `mark_refs` walk arms) in the same change series; all three cells green at HEAD.
  A THIRD multifile-`SHAPES` instance repeated the steering-comment tell verbatim: every reject-set
  shape (`rset`/`nerset`/`rseta`/`nerseta`) carried a deliberately-EXPOSABLE element — their own
  projector comment says so — while a RECORD-element reject rule emits a `try_from(&<Elem>List)`
  loose source the struct-walk arm only registered under the non-empty bound, leaving a plain
  `[*] reject` rule cross-module a live E0425 no gate could see, found by fix-review reach-analysis
  ("which OTHER rule families emit a loose source?") over the restricted-wrapper delivery.
  Enumerated as the `rsetrec`/`nersetrec`/`rsetarec`/`nersetarec` cells (`rsetrec` red-proven,
  then fixed by keying the struct-walk gate on `duplicates == Reject`, LOCKSTEP with
  `generate_reject_ordered_set_type`'s `loose_list` decision); all green at HEAD.
  A second proven instance on the wasm matrix itself: the alias-to-record shape (`ral = st`) was
  missing from `SHAPES`, so the group-choice wasm-ctor alias-resolution divergence was un-gated for
  plain aliases (only its `.cbor`-wrapper sibling `cborwrap` had a cell) until the fix's review
  asked which other alias shapes the divergent `resolve_alias_shallow` could reach — now enumerated
  as the `ralias` cells (green). The `collrec` and `ralias` instances were found by review asking
  "what else can this code path reach?", which is exactly the question the periodic sweep
  mechanizes; the `tblrec` instance adds the steering-comment tell to it. One near-miss on
  the multifile half of this rule is on record: `ralias` initially landed only in the wasm
  projection's `SHAPES`, silently breaking the multifile list's "every self-contained shape with
  defs" claim until a doc-coherence review caught it — a SECOND such near-miss is the signal to
  make `project_multifile_matrix.ts --check` assert its `SHAPES` is a superset of the wasm
  projection's (minus its documented exclusions) instead of relying on review.
  A first instance on the multifile matrix's MODE axis is also on record (the axes-honesty rule's
  fourth quadrant — until it, every instance was SHAPES-side or wasm-ROLES-side): all four
  then-extant reference modes embed the shape from a record-FIELD position, so the
  group-choice-VARIANT
  position — whose ctor expands a foreign-scope Record's field types into `new_<variant>` params
  in both passes, an import class `scope_references` never marked (E0412/E0425) — was invisible to
  the whole sweep. Its discovery signature is a third tell, distinct from fix-review reach-analysis
  and steering comments: HAND-PATCHED GENERATED OUTPUT IN A CONSUMER (CML's committed
  `src/generated/**` carried hand-added `use` lines that every regen deleted — a workaround
  living in generated output is a coverage hole wearing the same disguise as a steering comment,
  and grepping consumers' generated-output diffs for hand edits is the corresponding cheap sweep
  arm). Fixed via the shared `EnumVariant::group_ctor_record_fields` helper (emitters + import
  walk); the vector is hand-pinned (`tests/multifile` `relay`, test
  `cross_module_group_choice_ctor`). The mode-axis extension recorded recur-first against a second
  instance has since had its trigger FIRE: the type-alias-TARGET position (a plain alias rule's
  `pub type` line naming a cross-scope target — another position `scope_references` never walked,
  E0412) escaped to a production consumer's regen, discovered not by any sweep tell but as the
  consumer failure the sweep exists to prevent. The position axis now exists as the `aliased` mode
  (`bal = <ty>`, every included shape — fixed by the `scope_references` type-alias walk); the
  `gcvariant` extension over the Record-resolving shapes remains the recorded remainder in
  `tests/TESTING_ROADMAP.md` ("Multifile reference-POSITION coverage").
  That mode's first day in production then proved a NEW axis surface: the SHAPES list's
  EXCLUSIONS. The `extern`/`rawbytes` shapes are excluded because the compile floor cannot build
  them standalone — but the exclusion bounds the gate, not the generator, and the walk's first
  consumer regen broke exactly there: a generic-EXTERN instance's alias target is a `Base<Args>`
  TYPE-EXPRESSION ident, emitted verbatim into the scope's `use` list (invalid Rust, rustfmt
  abort; feature request 07 — same consumer-failure discovery signature as the `aliased` escape
  itself). Fixed by decomposing in the walk (base imported at its declaring scope via the
  single-owner `GenericInstance::extern_base_ident`, arguments walked); the residue is hand-pinned
  (`tests/extern-generic-scoped`) rather than enumerated, since the constraint forcing the
  exclusion is the compile floor's, and a generation-only excluded-shapes leg is recorded
  recur-first in `tests/TESTING_ROADMAP.md` ("Multifile reference-POSITION coverage"). Sweep
  lesson: an exclusion justified by the gate's execution model is not evidence the generator has
  nothing to get wrong there — the periodic "what are we not enumerating?" question must read the
  exclusion comments too, not just hunt missing rows.
  Two further instances of the hand-patched-consumer-output tell landed together in the
  wasm-wrapper visibility + rustfmt-skip delivery: CML replace-overlay blocks whose entire payload
  flipped a wasm tuple field's visibility (retired by the uniform `pub(crate)` policy —
  `WasmWrapper::push_inner_field`, pinned by `integration_collection_wrapper_fields_are_pub_crate`),
  and seven hand-placed `#[rustfmt::skip]` blocks working around rust-lang/rustfmt#5703 fatally
  aborting regen on over-width fields (retired by the generator emitting the skip itself, pinned by
  `integration_overwidth_wasm_wrapper_field_gets_rustfmt_skip`). The second instance exposed a hole
  in a dimension none of the matrix axes enumerate — input REALISM (consumer-scale identifier
  length; every fixture's short names kept the over-width class unreachable) — recorded recur-first
  in `tests/TESTING_ROADMAP.md` ("Identifier-length realism").
  A further instance: a consumer's hand `pub use cml_core::Int;` (papering over the alias-walk
  dangling-name bug) plus a planned multi-block replace-overlay workaround spanning the whole
  `Int` surface, retired by routing `Int`/`IntError` through `--common-import-override` — the
  re-export plus the `borrowed_key_types.rs` `int` key-flavor row (pinned by
  `extern_deps`, `extern_deps_wasm`, and the pure-consumer cell `common_override_wasm_int` for the
  re-export,
  `int_key_via_common_import_override_sidecar` and the `int`
  leg of `workspace_key_requests_derive_effect_and_hard_errors` for the flavor channel).
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
  MODE (local vs index-deferred vs workspace-borrowed vs requested-hosted) × wrapper shape, which
  neither SHAPES/ROLES nor this input-mode rule enumerates — is recorded in
  `tests/TESTING_ROADMAP.md`'s `--extern-wrapper-index` deferral-boundaries entry, whose
  mechanical layer (a deferral-profile leg over the extern-capable shapes) is now DUE: its
  recur-first trigger fired on a consumer-reported pair of companion-wrapper cells (the
  named-table workspace keys-list and the co-hosted requested keys-list — details and the leg's
  axis refinements live in that entry).
- **Mint the two remaining unminted wasm-surface classes (or declare them permanent).** Extern /
  raw-bytes ctor args (user-supplied types with no generated conversion) and the `--wasm-*-macro`
  modes (they replace the whole wrapper method surface) fall back to the compile verdict with loud
  skips today (the list: `tests/README.md` § "wasm-crate test module"). Minting the former means
  extending the def-splice the compile gate already does for `rawbytes` cells
  (`append_raw_bytes_defs`) to ctor-arg minting; the latter needs the user macro definitions in scope
  (the `tests/wasm-macro-crate` pattern). Either close them or record compile-verdict fallback as the
  permanent posture and prune this item.

## Explicitly out of scope (decided, not overlooked)

Per the `QUERIES.md` query-map, no consumer query needs these (revisit only if a concrete need surfaces):
- **F8 — matching semantics** (prioritized-choice ordering, greedy occurrence, implicit `:` cut): a
  validation concern, not a cddl-codegen serialization concern.
- **F9 — interaction tuples** (A-in-B-in-role-C-with-operator-D): richer-than-binary containment; stretch
  query (Q7) only.
- **F10 / F11** — note-only (over-acceptance denominator; AST cross-check is weak corroboration).

## Pending decision (needs a human call)

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

Hand-counted prose lists in this doc (e.g. the findings ledger's panic-class and
compile/round-trip-class family-count headers — cited here count-free on purpose: a hard-coded
example count is itself this rot class, and one went stale exactly that way) are maintained by
review: pruning or adding a
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
durations; the sweep's own datum is specified in `tests/TESTING_ROADMAP.md`).

## Related

Broader testing work (clear-wins, roadmap items, the trailing-bytes runtime change) lives in
`tests/TESTING_ROADMAP.md` — the matrix is the reference that testing effort *projects from*.
