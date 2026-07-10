# CDDL master matrix — roadmap (remaining work + findings)

Read `README.md` first (the model + current state, including the durable gotchas and the
upstream-oracle-gap state). This doc is strictly the FUTURE state: remaining work, the open-findings
ledger, and pending decisions. The blow-by-blow of *how* done work landed lives in git history (this
doc is actively pruned of it; the project already did the same with the scale report + cold-critique).
Running the gates is not a roadmap concern either: `check.ts` at the repo root is the self-enforcing
gate registry + entry point, `tests/README.md` § "Running everything" is the prose overview, each
script's header docstring is the per-gate detail, and `QUERIES.md` documents the Q1–Q6 query scripts.

**Status: gate-green.** <!-- status-header counts are generated — regenerate with: cd cddl-matrix && bun run project_status_headers.ts --write --><!-- gen:sh:roadmap-counts -->106 features (95 RFC8610 + 1 RFC9682 + 10 `CDDL_CODEGEN` vendor profile), 94 containment cells, and 234 cddl-codegen annotations<!-- /gen:sh:roadmap-counts -->, all axes reconciled/deterministic, with
execution-gated support **per-feature, per-cell (role × feature), and per-control-op** (<!-- gen:sh:roadmap-ops -->all 37 IANA ops probed<!-- /gen:sh:roadmap-ops -->):
"supported" requires the generated crate's `--emit-tests`
round-trip/reject tests to PASS (`cargo test`), falling back to the compile verdict only for shapes that
mint no test surface (recorded honestly in the evidence). The orthogonal **emission axis is filled**
(every default-supported row carries a `preserve`/`json` verdict; <!-- gen:sh:roadmap-emission -->6 divergences, all `preserve`-side<!-- /gen:sh:roadmap-emission --> —
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

- **Full role × feature coverage grid.** The corpus projection keys coverage on `(role × feature)` only for
  the cells where support *differs by role* (`prelude.null`, the literal values). A full grid for *every*
  construct is unbuilt — the floor data (`corpus_detect.ts` `rolesIn`, via `examples/ast_roles.rs`) already
  supports it; wire it into `project_corpus.ts` if a consumer wants the complete matrix view.
- **Ruby-oracle flake absorber for evidence-writing verify runs — build on the next committed bite.**
  The ruby oracle non-deterministically flips a verdict on identical input (documented on
  `control_examples.toml`'s header; observed on `ctl.and`, corroboration-only either way), so a full
  `verify.ts` run can hand an unrelated change a spurious one-line annotation evidence flip. No gate
  can see it (the evidence is probe-written by design); the working rule is review discipline — the
  annotations diff of an evidence-writing run must name exactly the rows the change touches, and a
  flake flip is hand-reverted (done once, on the occurrence-marker enumeration change). If a spurious
  flip ships committed, or the hand-revert dance recurs, build the mechanical layer in the probe
  itself: retry a ruby-fail verdict once on identical input and record the retry outcome (absorbing
  the observed spurious-fail direction at source — no read of the previously committed annotations,
  preserving the no-prior-output-dependence invariant).

## F4 / F5 follow-ons (only when their consumer exists)

- **F5 (encoding precision):** the encoding axis is already major-type-dependent (no impossible cells). The
  golden_hex projection lists uncovered legal cells *globally*; the remaining work is per-*construct*
  legal-cell enumeration so Q3 can say "for construct C, these legal encodings are untested" — link
  `features[].encodings` to the leaf cells each construct can emit and intersect with golden coverage.
- **F4 (tag registry):** deliberately not pinned/enumerated (cddl-codegen is tag-parametric). Revisit only
  if a *tag-semantic* consumer of the master appears.

## Expansion (when relevant)

- **Profiles/versions:** v1 targets the RFC 8610/9682 grammar + the IANA control-op registry (spans RFC
  8610/9090/9165/9741). Add other CDDL profiles (e.g. the modules drafts) if needed, and bump cddl-codegen's
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

**Upstream close-outs (waiting on external releases):**
- When a rust `cddl` release (or the fork's `local-fixes` branch) ships the prelude-`number` float-
  validation fix (`is_ident_float_data_type` omits `Token::NUMBER` while `is_ident_integer_data_type`
  includes it, so a bare float against `number` wrongly fails validation; one-line fix + repro in
  `draft/rust-cddl-number-float-gap.md`): the decode-conformance arm-coverage floor can then mint a real
  float `prelude.number` accept vector. FIRST flip the `prelude-number-float-rejects` probe in
  `cddl-matrix/oracle_fingerprint.json` (it deliberately pins this gap OPEN, and the shared file gates
  both `verify.ts`'s `RUST_CDDL` binary preflight and `ir_conformance_corpus`'s `CDDL_ORACLE_DEP` crate
  preflight, so a fixed oracle is refused before either consumer can run stale). Then remove
  `"prelude.number/7"` from `DECODE_FLOOR_ARM_EXEMPT`
  (`cddl-matrix/lib.ts`) — or let `project_decode_conformance.ts` § 7's stale-guard force it after
  `verify.ts --mint-decode-foreign --only=prelude.number` — then prune the README gap entry and that
  draft.
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
- When a rust `cddl` fix ships bignint-keyed map validation (README gap #6 — OPEN at the pinned
  `2c7548e` rev; differential repro + prune steps in
  `draft/rust-cddl-bignint-key-validator-gap.md`): remove `cbor_bignint_table` from
  `ir_conformance_corpus`'s `RUST_ORACLE_SKIP` (re-arming its rust conformance half), prune README
  gap #6, and delete that draft.
- When a rust `cddl` fix ships TAG-typed map-key validation (README gap #8 — OPEN at the pinned
  `2c7548e` rev; differential repro, suspected `src/validator/cbor.rs` site, and prune steps in
  `draft/rust-cddl-tag-map-key-gap.md`, local note; no upstream issue filed yet): re-mint the row
  it blocks (`--mint-decode-foreign --only=contain.map-key.type2.tag` — its `pinned_reason`
  disappears once candidates survive the two-oracle gate), re-run the full `verify.ts` in the same
  change so the row's evidence picks up the corroboration clause, and prune README gap #8 + that
  draft.
- When a rust `cddl` fix ships optional-entry empty-map validation (README gap #9 — OPEN at the
  pinned `2c7548e` rev; `validate` over-rejects the spec-VALID empty map against `{ ? tstr => uint }`;
  repro + suspected site + close-out detail in `draft/rust-cddl-optional-entry-empty-map-gap.md`):
  the close-out is now just **prune README gap #9 + that draft**. It no longer gates a decode re-mint:
  `{ ? tstr => uint }` is a **rejected** spelling (the count-permitting table-marker boundary — the
  `?` / `n*m` markers are gracefully rejected, `contain.occurrence-target.memberkey.type1.optional_table`
  now lives in `tests/matrix_reject/`, not the decode catalog), so there is no committed decode row for
  the gap to feed a legal empty-map accept vector into. The gap survives only as a fingerprint/oracle
  note until the fix ships.
- The `cbor_event` close-outs (f16 mis-decode, length-prefix over-allocation) are entries in the
  list below — each names its prune/re-mint steps.

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
- **Honor count-permitting occurrences on heterogeneous ARRAY-record fields as `Vec` fields.**
  Generation rejects them gracefully (`[uint, tstr, * bytes]`, any marker but `?`/`1*1`, any
  position — the array analogue of the keyed-map zero-permitting guard, in the same parsing.rs
  field loop; boundaries pinned by `occurrence_on_array_record_field_rejects_gracefully`), to
  avoid the silent exactly-once narrowing that generated decoders rejecting spec-valid repetition
  counts — invisible to round-trip tests, surfaced only by spec-derived decode vectors (the
  `773b723` oracle-masking lesson: the row's candidates died on two-oracle disagreement until the
  fully-fixed `2c7548e` oracle). The unsupported surface is enumerated by
  `contain.occurrence-target.grpent.member.{zero,plus}_array`. Real support needs decode
  lookahead: a repeated-item run bounded by the following fields' types — middle-position repeats
  (`[uint, * bytes, tstr]`) need peek-type disambiguation.
- **cbor_event 2.4.0 mis-decodes HALF-PRECISION (f9) floats — dependency-level, fix deferred
  upstream like the over-allocation entry.** Its Special decoder's `0x19` arm casts the raw 16-bit
  pattern to f64 (`Special::Float(f as f64)` — `f9 4200` = 3.0 decodes as 16896.0) instead of
  decoding the half-float bits the way the `0x1a` arm does `f32::from_bits`. Blast radius: EVERY
  f16-encoded value read by any generated decoder is silently corrupted; it became VISIBLE only on a
  fixed-value member, where the wrong value fails the equality check (ruby's generator minted
  `[3.0]` as `81 f9 4200`, both oracles accept it, our decoder rejects with FixedValueMismatch
  found=16896). Recorded on the `value.number.hexfloat` catalog row as its `class="bug"` reject pin
  (spec-valid, wrongly rejected; the mint re-validates it and it is pruned when a fixed cbor_event
  ships). A second, GREEN-but-corrupted instance sits on the `prelude.time` catalog row: its
  `c1 f9 068e` accept vector replays Ok while the decoded value is the mis-cast one — an accept
  vector asserts Ok only, and this row's preserve byte-identity leg (which would expose the f16
  re-encode) is on the replay gate's `PRESERVE_SKIP` for the unrelated float-generation gap, so
  nothing pins the wrong value. When a fixed cbor_event ships, that vector's decoded value silently
  changes; the repro/prune steps in the local note above cover both instances. Containment while the
  bug stands: **f9 item heads are banned from decode-catalog accept vectors** — the mint drops every
  f9-headed accept candidate (logged as DROPPED) and `project_decode_conformance.ts` fails a committed
  one — because such a vector is GREEN-but-corrupted by construction (Ok-only assert; the
  encoding-variant mutator copies float heads verbatim; the float class is preserve-skipped), i.e.
  decode evidence that pins nothing about the decoded value. The ban is item-HEAD-scoped, so the
  tag-nested `prelude.time` instance above (`c1`-headed) stays ledgered here rather than lint-caught.
  Prune the ban (mint + drift-gate sides together) with this entry when a fixed cbor_event ships. Same disposition as the length-prefix over-allocation entry below: generated crates
  depend on crates.io `cbor_event` directly, so the fix belongs upstream, not in `static/`. Known
  upstream already: issue #16 reports the wrong data, and open PR #18 carries the exact arm fix but
  gates the crate on the nightly-only `f16` primitive, so it cannot ship for stable consumers as
  written — the stable-severable decode fix to propose there, plus the standalone repro and the
  prune/re-mint steps for when a fix ships, are in `draft/cbor-event-f16-decode-fix.md` (local
  note) — bundle it with the over-allocation report in one upstream conversation.
- **Cross-module ARRAY structural-wrapper placement breaks both reference modes of a
  collection-of-records shape** — the Array-arm half of the issue-138 TODO in `mark_refs`
  (intermediate.rs), left in place when the Map arm went sole-owner-aware. `recs = [* foo]` with
  record element `foo` in module `a`: the wasm representation needs a generated structural wrapper
  (`FooList`) — unlike `[* uint]`, which is transparent `Vec<u64>`; that is why the original `coll`
  shape could never probe this class. Pinned by the `collrec` cells in `tests/matrix_multifile/`
  (`MULTIFILE_MATRIX_SKIP` with the exact E-codes; carried into the round-trip gate's
  `MULTIFILE_ROUNDTRIP_SKIP` — the wasm crate never compiles, so `cargo test` can never pass),
  enumerated AFTER review found the `SHAPES` hole (the axis-honesty rule
  below applied late — the cells were never green, so no gate had ever seen the shape):
  - `collrec__anon` (E0425): the anonymous use mints `FooList` at the crate root, and the wrapper's
    accessors name the element type `Foo` bare without importing it from `a`. Candidate fix: mint
    the array wrapper in the element's module, or import the element type where the wrapper is
    minted (the map side answered the analogous placement question with `table_shape_sole_owners`).
  - `collrec__named` (E0432): the `mark_refs` alias-target recursion (the E0433 inner-type-import
    fix) descends into the alias target, where the Array arm adds a ROOT-scope import of the
    structural `FooList` — but a NAMED collection alias mints only its own wrapper (`Recs`), so the
    structural name exists nowhere. Before that recursion landed, the same cell failed RUST-side
    (E0433, the inlined-alias-import class) — the recursion traded an earlier error for this one,
    never a green-to-red. Candidate fix: don't mint structural-wrapper imports when recursing
    through an alias target (the alias's own wrapper subsumes them — e.g. recurse the target with
    the wasm special-casing off), paired with Array-arm owner-resolution for the anon case.

  The **two-type-constraint restricted wasm wrappers** (`[+ T]` → `NonEmptyVec`, `{+ k => v}` →
  `NonEmptyMap`) reach this SAME structural-wrapper ROOT_SCOPE class cross-module — the loose builder
  (`FooList` / `MapU64ToText`) is root-minted and the restricted wrapper's `try_from(&Loose)` (or the
  anon dedup-to-named reference) names it, the element type, or the dedup-target rule bare from a
  non-root module. E0425 in every case, pinned by `MULTIFILE_MATRIX_SKIP`/`MULTIFILE_ROUNDTRIP_SKIP`:
  - `necollrec__{anon,named,unref}` — the `+` analogue of `collrec` (`recs = [+ foo]`, non-exposable
    record element): the restricted wrapper compounds `collrec`'s root-minted `FooList`/`Foo` dangle.
  - `nemap__{anon,anonb,named,unref}` — the MAP-side manifestation the loose-only `collmap` never
    exposed (it is green cross-module): the restricted `Mp::try_from(&MapU64ToText)` reintroduces a
    bare reference to the root-minted loose builder.
  - `necoll__{anon,anonb}` — even the exposable-element list (`[+ uint]`, whose restricted wrapper's
    `try_from(Vec)` needs no loose twin) dangles on the ANON path: the anonymous `[+ uint]` dedups to
    the named rule in the shape's module but the cross-module import is not emitted (`named`/`unref`
    stay green). The same Array-arm owner-resolution fix covers all three shapes.
- Mixed struct+table maps (`{ a: uint, * k => v }`) unsupported — a map is detected as EITHER a struct or a
  homogenous table, never both. Inline anonymous nested composites need a name.
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
    `?`/`n*m` table spellings owned by the sibling entry above): a `BoundedVec<T, MIN, MAX>`-shaped
    `static/` generic (with the non-empty type as its `MIN=1, MAX=∞` case) and mechanical
    `Min{N}`/`Max{N}` wasm naming slot into the same conversion contract without redesign.
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
- Map-representation group-choice arm with a fixed-value entry panics:
  `contain.group-choice-arm.type2.value.map` (`t = { a: 0 // b: tstr }`) reaches generation and aborts at
  `generation.rs:2467` (`assert_eq!(";" vs "")`). This is a new valid-CDDL surface for fixed values in a
  map-rep arm, tracked as a known PANIC row in `tests/matrix_panic/`.
- Fixed-value member containment still has unenumerated variants beyond the precedent rows
  `contain.array-element.prelude.{true,null}`, `contain.map-value.prelude.false`, and
  `contain.array-element.type2.tag.{fixed_null,fixed_bool}`. Remaining candidates include map-value
  tag-wrapped fixed members and the other fixed prelude constants across array/map member positions,
  so the emission axis probes each fixed-value kind × wrapping position × profile instead of relying
  on the hand fixtures (`tests/robustness/fixed_bool_member.cddl`,
  `tests/corpus/fixed_bool_member.cddl`). This is the "Intra-alternative variation rows" expansion
  rule (this doc) applied to the fixed-value member role: enumerate the wrapped variants as rows
  BEFORE trusting a green.
- **An OPTIONAL fixed value with no encoding variation (bool / null) in member position fails
  generation** — independent of the mandatory case, which round-trips
  (`tests/corpus/fixed_bool_member.cddl`). `[x: uint, ? v: true]` / `{? k: true, x: uint}` (and the
  same shapes with `? v: 5` / `? v: null`, so this is not bool-specific): the serialize length
  emits `Len(1 + )` — an empty count because the encoding-less optional contributes no length
  term — which rustfmt rejects, and under `--preserve-encodings` the annotate path asserts
  issue-205 (`is_fixed_value` in the optional type-check branch). Loud either way (Err / assert),
  never silently wrong — a GENERATION-failure class, so it lives here rather than in the layer-2
  `LAYER2_KNOWN_BAD` family list below. Candidate fix: treat an encoding-less optional fixed value
  as a present/absent flag in the length computation (mirroring the mandatory fixed-value handling).
- Array-representation group-choice arm with an inline group panics:
  `contain.group-choice-arm.grpent.inline_group.array` (`t = [ (uint, tstr) // bytes ]`) aborts at
  `parsing.rs:1710` (`inline group entries are not implemented`). This is a distinct inline-group arm
  limitation, tracked as a known PANIC row in `tests/matrix_panic/`.
- **Five panic-class families remaining from the recombination fuzzer's sweeps**
  (`src/tests/recombination_tests.rs`; each pinned as a `tests/robustness/` PANIC row and cited in
  the sweep's `KNOWN_PANIC_CLASSES` ledger — the matrix has no containment cells for these shapes,
  which is itself the coverage gap the fuzzer exists to find):
  - An inline map carrying a GROUP CHOICE as a member/element type (`a = [{ x: uint // y: tstr }]`)
    panics parsing (`group choices in inlined map types not allowed`) — a distinct, earlier site
    than the choice-free "Anonymous groups not allowed" panic. Pinned by
    `tests/robustness/inline_group_choice_member.cddl`.
  - `any` in member/element position (`a = [any]`, `{ k: any }`) panics intermediate.rs's
    `generic_instances` assert — distinct from the top-level `x = any` compile-class gap
    (`tests/matrix_reject/prelude.any.cddl`). Pinned by `tests/robustness/any_member.cddl`.
  - A type-choice arm with no storable representation panics `Option::unwrap()` on `None`
    (intermediate.rs): `a = any / tstr` (the `any` extern arm). The sibling anonymous
    array-of-plain-group arm (`a = [coords] / tstr`) is storable — it promotes the plain group
    to an Array-rep Record struct and generates a proper enum variant (pinned by
    `tests/robustness/choice_group_array_arm.cddl`, an `ok` fixture). Panic pinned by
    `tests/robustness/choice_any_arm.cddl`.
  - A bare fixed value as a zero-or-more occurrence target (`a = [* 5]`, equally `true`/`"v1"`/`null`)
    reaches `for_rust_member`'s `should not expose Fixed type in member` panic — the
    registration-time graceful rejection that owns the top-level shapes never sees this position.
    Pinned by `tests/robustness/fixed_value_occurrence.cddl`.
  - A tag wrapping a PRELUDE CONSTANT (`t = #6.11(true)`) hits the same `Fixed` panic: the literal
    inner (`#6.5(5)`, `tests/robustness/tagged_literal.cddl`) is rejected gracefully, but a prelude
    constant resolves through the prelude alias on a path the guard does not classify. Pinned by
    `tests/robustness/tagged_prelude_constant.cddl`.
- **Six compile/round-trip-class families remaining from the recombination fuzzer's layer-2 sweeps**
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
  - **A tagged fixed value inside a map-rep group-choice arm emits a call to a non-fn struct**
    (E0618 `expected function`): `t = { ga: #6.11(42) // fb: tstr }`.
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
- Array-representation group-choice arm with an anonymous map panics:
  `contain.group-choice-arm.type2.map.array` (`t = [ {a: int, b: uint} // tstr ]`) aborts at
  `parsing.rs:1592` (`TODO: non-table types as types`). This belongs to the anonymous-composite family but
  has its own panic site, tracked as a known PANIC row in `tests/matrix_panic/`.
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
- Two comment-DSL candidate fixes surfaced by the `src/tests/dsl_position_tests.rs` position sweep
  (held in its `KNOWN_SILENT_DROP` pin list — pinned, not fixed; the pins flip loudly when a fix
  lands): (1) `@name` at a MEMBER-position anonymous inline group never reaches the naming site
  (`get_comment_after(type2)` ascends only through Type1/TypeChoice), so the "Anonymous groups not
  allowed" panic fires despite its message advertising `@name` as the remedy — the remedy DOES work
  in choice-member position; either make the member-position comment reachable or scope the panic
  message's advertised remedy. (2) `@doc` on a fixed-value (dataless C-style enum) type-choice
  variant is captured into the IR but never emitted — data-carrying variants render the `///` fine.
- **Real nint support is ONE cross-cutting candidate feature — its per-shape gaps are enumeration
  cells of the matrix, not separate tasks.** Nint intersects every containment role (fixed map
  keys — rejected gracefully above; table domains and `@newtype` bounds — work; bare values, json,
  preserve-encodings — partial), so probing any role re-surfaces a nint cell and per-finding
  sessions keep landing small nint conversions without moving the support boundary. Two facts for
  whoever picks this up: (1) upstream `cbor_event` issue #9 is NOT the blocker it appears — the
  crate already ships full-range endpoints (`write_negative_integer_sz` / `negative_integer_sz`,
  `i128`) and generation already uses them on the paths where the `i64` limit bites (`i64::MIN`,
  preserve-encodings); (2) the actual full-range limiters are local: `FixedValue::Nint(isize)` in
  the IR cannot represent -2^64..-(2^63+1), and the remaining plain `write_negative_integer(i64)`
  call sites (e.g. `FixedValue::to_bytes`) cap fixed-value encoding at the `i64` range. Until a
  consumer justifies the feature, new nint shapes land as graceful rejections + enumeration cells;
  when one does, do the IR widening and the `_sz` sweep as one change, then flip the pinned
  rejection rows (record path first, then the group-choice arm).
- Bare `x = int` / an `int` `.cbor` payload emit an undefined `Int` wrapper (`cannot find type Int`); `int`
  works as a member / array element. Third instance from the recombination fuzzer's layer-2 run: an
  `int`-VALUED table under a `.cbor` payload (`x = bytes .cbor { tstr => int }`) dangles the same
  undefined `Int` (held in the sweep's `LAYER2_KNOWN_BAD` ledger citing this entry), and its
  `{ * tstr => int }` sibling is a fourth. The sibling carries a harness lesson: `Int` is a
  crate-global extern emitted iff any rule registers a reference, so the sweep's BATCHING can mask
  the dangle when a batch-mate registers it — the class stayed green in the default gate for that
  reason and surfaced only under the wasm leg's different batch boundaries (the batch-masking note
  on `LAYER2_RULES_PER_BATCH` in `src/tests/recombination_tests.rs`).
- `float16` / float-choice aliases unsupported (no native Rust f16) while `float32/64` work; generics on
  plain groups rejected. Under `--preserve-encodings` the float gap is positional, and the emission axis
  records it honestly: a bare `float`/`float32`/`float64` alias still generates and compiles
  (`emission.preserve = supported`, but compile-only evidence — the synthetic embed holder panics
  generation, so floats **as members** are the broken shape), while the choice-carrying prelude types
  `number` / `time` panic outright (`emission.preserve = unsupported`).
- **A CBOR tag over a type-choice enum is unimplemented under `--preserve-encodings`** — a non-float
  preserve gap: `t = #6.10(int / tstr)` panics generation at the tagged-enum serialize path's explicit
  `assert!(!cli.preserve_encodings)` (its own `TODO: how to even store these?` — the per-variant encoding
  metadata has no home on the enum). Tags over structs/arrays/maps preserve fine. Surfaced by the
  decode-conformance replay gate's preserve leg (skip-listed there in `PRESERVE_SKIP`, stale-guarded)
  and now recorded on the emission axis (`contain.tag-content.type.choice` →
  `emission.preserve = unsupported`), alongside `prelude.number` / `prelude.time` and the two
  float-range wrapper rows `rangeop.{inclusive,exclusive}.float` (the wrapper wraps an f64 member,
  hitting the same native-float-under-preserve `unimplemented!`).
- **A CBOR tag wrapping `any` panics generation under `--preserve-encodings`** — `t = #6.11(any)`
  reaches generation (unlike bare `[any]` / `{ k: any }`, which panic earlier at the shared
  `generic_instances` assert in intermediate.rs under both profiles) and unwraps `None` in
  `encoding_fields_impl` (generation.rs) building the tag's encoding field, because `any` carries no
  encoding metadata to attach one. Default-profile the same spec panics at that earlier
  `generic_instances` assert (the any-in-member family, pinned by `tests/robustness/any_member.cddl`),
  so this is a preserve-only divergence surfaced by the recombination fuzzer's preserve layer-2 sweep
  (held in its `PRESERVE_ONLY_PANIC_CLASSES` ledger citing this entry; the `any` construct is
  unsupported in these positions regardless). Candidate fix belongs with the broader `any`-support
  question, not the preserve path specifically.
- Two float-adjacent **deliberate graceful rejections — boundaries to keep, not gaps to close
  blindly**: `.ne` over a float (the integer min>max exclusion hack has no principled float
  encoding) and a decimal bound on an integer-primitive head (`uint .le 10.5` — silently flooring
  it onto the int head would mis-enforce). Both route through `record_rejection`; pinned alongside
  the float-window enforcement in the `tests/core` `float_bounds` fixtures.
- **Untrusted length-prefix over-allocation (DoS — dependency-level, fix deferred upstream).**
  `cbor_event` 2.4.0's definite-length string branches pre-size from the untrusted length header
  (`vec![0; len as usize]` at three `de.rs` sites), so an 11-byte input claiming a ~2 GB text string
  drives a single ~2 GB allocation before any payload byte is read — an OOM abort in every consumer
  parsing untrusted chain data. Generated crates are standalone and depend on crates.io `cbor_event`
  directly (`static/Cargo_rust.toml`), so the fix needs a published/forked `cbor_event`, not a
  codegen change. The exact 3-hunk patch, standalone repro, and repoint steps are in
  `draft/cbor-event-overallocation-fix.md`; the ready-to-submit upstream PR text is in
  `draft/cbor-event-upstream-pr.md` — bundle it with the f16 report above in one upstream
  conversation. Surfaced by `fuzz/`; it has no cargo-test crash-replay because an OOM kills the test
  process — the fuzz process boundary is the only oracle for this class. Related, unowned axis:
  generated crates float on semver `cbor_event = "2.4.0"`, so nothing tests the version RANGE
  consumers actually resolve — the upstream fix will arrive as exactly such a version event; a
  `--minimal-versions`-style or pinned-latest check of a generated crate would own it.

## wasm-ABI & multifile placement matrices — remaining work

Current state — the grid, the three always-on axes (compile floor, round-trip, rust↔wasm API-surface
parity) with their emission-profile sweeps, the minted wasm test surface and its loud-skip list, and
the multifile placement sweep — is documented in `tests/README.md` (§ "wasm-ABI matrix", § "wasm-crate
test module", § "rust↔wasm API-surface parity", § "multifile placement matrix") plus `README.md` §
annotations (`verify.ts`'s default-on `--wasm` probe); the recombination fuzzer's wasm leg
(`recombination_wasm_crates_check`, `tests/README.md` § "Shape-recombination fuzzer") is the
composition-space cross-check that complements this matrix's curated per-shape grid. What remains:

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
  alias recursion could reach — now enumerated as the `collrec` cells (red, pinned; § findings).
  A second proven instance on the wasm matrix itself: the alias-to-record shape (`ral = st`) was
  missing from `SHAPES`, so the group-choice wasm-ctor alias-resolution divergence was un-gated for
  plain aliases (only its `.cbor`-wrapper sibling `cborwrap` had a cell) until the fix's review
  asked which other alias shapes the divergent `resolve_alias_shallow` could reach — now enumerated
  as the `ralias` cells (green). Both instances were found by review asking "what else can this
  code path reach?", which is exactly the question the periodic sweep mechanizes. One near-miss on
  the multifile half of this rule is on record: `ralias` initially landed only in the wasm
  projection's `SHAPES`, silently breaking the multifile list's "every self-contained shape with
  defs" claim until a doc-coherence review caught it — a SECOND such near-miss is the signal to
  make `project_multifile_matrix.ts --check` assert its `SHAPES` is a superset of the wasm
  projection's (minus its documented exclusions) instead of relying on review.
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
  (their compile gates use single-file inputs), `--common-import-override` (the `extern_deps` pins
  are single-file), and EXECUTION of the generated json-gen crate (`wasm/json-gen`) against
  cross-module types (the multifile `json`-profile sweep compiles rust+wasm; the json-gen runner
  gate is single-file-only).
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

Hand-counted prose lists in this doc (e.g. the findings ledger's "Five panic-class families" /
"Six compile/round-trip-class families" headers) are maintained by review: pruning or adding a
family must update the count and keep the entry in the list whose framing matches its failure
stage (generation-failure vs layer-2 compile/round-trip). If a count or a mis-homed entry slips
through review again, fold these counts into `project_status_headers.ts`'s generated-counter
system (the same mechanism that already generates the status-header counts) instead of adding
another review rule.

## Related

Broader testing work (clear-wins, roadmap items, the trailing-bytes runtime change) lives in
`tests/TESTING_ROADMAP.md` — the matrix is the reference that testing effort *projects from*.
