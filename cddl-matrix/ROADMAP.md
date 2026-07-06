# CDDL master matrix — roadmap (remaining work + findings)

Read `README.md` first (the model + current state, including the durable gotchas and the
upstream-oracle-gap state). This doc is strictly the FUTURE state: remaining work, the open-findings
ledger, and pending decisions. The blow-by-blow of *how* done work landed lives in git history (this
doc is actively pruned of it; the project already did the same with the scale report + cold-critique).
Running the gates is not a roadmap concern either: `check.ts` at the repo root is the self-enforcing
gate registry + entry point, `tests/README.md` § "Running everything" is the prose overview, each
script's header docstring is the per-gate detail, and `QUERIES.md` documents the Q1–Q6 query scripts.

**Status: gate-green.** <!-- status-header counts are generated — regenerate with: cd cddl-matrix && bun run project_status_headers.ts --write --><!-- gen:sh:roadmap-counts -->106 features (95 RFC8610 + 1 RFC9682 + 10 `CDDL_CODEGEN` vendor profile), 82 containment cells, and 222 cddl-codegen annotations<!-- /gen:sh:roadmap-counts -->, all axes reconciled/deterministic, with
execution-gated support **per-feature, per-cell (role × feature), and per-control-op** (<!-- gen:sh:roadmap-ops -->all 37 IANA ops probed<!-- /gen:sh:roadmap-ops -->):
"supported" requires the generated crate's `--emit-tests`
round-trip/reject tests to PASS (`cargo test`), falling back to the compile verdict only for shapes that
mint no test surface (recorded honestly in the evidence). The orthogonal **emission axis is filled**
(every default-supported row carries a `preserve`/`json` verdict; <!-- gen:sh:roadmap-emission -->6 divergences, all `preserve`-side<!-- /gen:sh:roadmap-emission --> —
see § findings) and supported rows carry decode-foreign corroboration clauses (plus <!-- gen:sh:roadmap-constraint -->44 `class="constraint"` enforcement reject vectors over 26 enforce-green rows<!-- /gen:sh:roadmap-constraint --> — the enforcement
axis is FULLY green: every supported row with a rejectable constraint projects
`enforce = yes (bounded-reject)`, with BOTH the green set and the (now-empty) unverified set pinned
exactly by `query_q4_directional.ts --check`).
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

## 1. Remaining work — projections & queries

The matrix exists to feed **many** consumers; corpus was just the hard flagship. What's left:

- **Full role × feature coverage grid.** The corpus projection keys coverage on `(role × feature)` only for
  the cells where support *differs by role* (`prelude.null`, the literal values). A full grid for *every*
  construct is unbuilt — the floor data (`corpus_detect.ts` `rolesIn`, via `examples/ast_roles.rs`) already
  supports it; wire it into `project_corpus.ts` if a consumer wants the complete matrix view.

## 2. F4 / F5 follow-ons (only when their consumer exists)

- **F5 (encoding precision):** the encoding axis is already major-type-dependent (no impossible cells). The
  golden_hex projection lists uncovered legal cells *globally*; the remaining work is per-*construct*
  legal-cell enumeration so Q3 can say "for construct C, these legal encodings are untested" — link
  `features[].encodings` to the leaf cells each construct can emit and intersect with golden coverage.
- **F4 (tag registry):** deliberately not pinned/enumerated (cddl-codegen is tag-parametric). Revisit only
  if a *tag-semantic* consumer of the master appears.

## 3. Expansion (when relevant)

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
  (Q4 pins the exact enforce-green set).

## Findings — open (the ledger of candidate fixes; the matrix's actual payoff)

The durable gotchas and the upstream-oracle-gap state are CURRENT state and live in `README.md`
(§ "Gotchas", § "Upstream oracle gaps"); this section holds only what is still to do. New findings
are ledgered here (that's what the probe/gate error messages point at).

**Upstream close-outs (waiting on external releases):**
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
- The `cbor_event` close-outs (f16 mis-decode, length-prefix over-allocation) are entries in the
  list below — each names its prune/re-mint steps.

**Bugs / gaps surfaced as findings (candidate cddl-codegen fixes):**
- **Incremental choice extension (`/=` type-choice, `//=` group-choice) silently drops every arm but the
  last.** `parse_rule` re-registers the rule ident on each statement, so the LAST definition wins and the
  generated type models only the final extension arm — `a = int` / `a /= tstr` generates a `tstr`-only
  type, dropping the `int` base arm (parsing.rs documents the `is_type_choice_alternate ignored` skip;
  the `//=` group case is the same shape). Surfaced by the `773b723` array-sequence oracle fix: the old
  rust oracle rejected the base-arm instances ITSELF (its own array/occurrence bug), so the mint's
  two-oracle gate never presented them and the rows minted only the extension-arm accepts — masking the
  drop. Against the fixed oracle, `assignt.extend` / `assigng.extend` re-mint with base-arm accept
  candidates that ruby+rust both accept but our decoder REJECTS; committed as `class="limitation"` reject
  pins (spec-valid, wrongly rejected — pruned when `/=`/`//=` extension is implemented, or rejected
  loudly at generation instead of silently narrowing). A concrete instance of the lesson that an oracle
  bug can hide a codegen gap by rejecting the discriminating vector before our decoder ever sees it.
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
  ships). Same disposition as the length-prefix over-allocation entry below: generated crates
  depend on crates.io `cbor_event` directly, so the fix belongs upstream, not in `static/`. Known
  upstream already: issue #16 reports the wrong data, and open PR #18 carries the exact arm fix but
  gates the crate on the nightly-only `f16` primitive, so it cannot ship for stable consumers as
  written — the stable-severable decode fix to propose there, plus the standalone repro and the
  prune/re-mint steps for when a fix ships, are in `draft/cbor-event-f16-decode-fix.md` (local
  note) — bundle it with the over-allocation report in one upstream conversation.
- **A named table rule's JS class name is usage-dependent** — the wasm wrapper takes the structural
  `MapKToV` name whenever the same map shape was already minted for an embedded/resolved use, leaving
  the CDDL name as only a `pub type` alias (which wasm_bindgen does not export to JS).
  `mp = { * uint => text }` surfaces to JS as class `Mp` when referenced only as a named field
  (`collmap__struct-field`), but as `MapU64ToText` when e.g. a `@newtype` holder resolves the same
  shape first (`collmap__newtype-inner`; same for `standalone_text` in `tests/core`) — so adding an
  unrelated same-shape embedded map elsewhere in a spec silently RENAMES the JS class. Deterministic,
  but consumer-facing and spec-nonlocal (the alias form is pinned by the `core`/`special_map_key`
  wasm snapshots; the behavior is documented honestly in `docs/docs/wasm_differences.mdx` § Tables).
  Hand-caught reading generated output during the parity-gate scoping — `wasm_api_parity`
  deliberately accepts the public alias as rust-source-level parity, so no gate flags it today; the
  systematic catcher is the JS-name-visibility layer recorded on
  `tests/TESTING_ROADMAP.md` item 8 (distinguish a defined `#[wasm_bindgen]` counterpart from an
  alias-only one). Candidate fix: prefer the rule name for the wrapper when a single named table rule
  owns the shape (the struct-field role's existing behavior), aliasing the structural name to it;
  same-shape rule pairs still need the structural fallback.
- Mixed struct+table maps (`{ a: uint, * k => v }`) unsupported — a map is detected as EITHER a struct or a
  homogenous table, never both. Inline anonymous nested composites need a name.
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
- Array-representation group-choice arm with an inline group panics:
  `contain.group-choice-arm.grpent.inline_group.array` (`t = [ (uint, tstr) // bytes ]`) aborts at
  `parsing.rs:1710` (`inline group entries are not implemented`). This is a distinct inline-group arm
  limitation, tracked as a known PANIC row in `tests/matrix_panic/`.
- Array-representation group-choice arm with an anonymous map panics:
  `contain.group-choice-arm.type2.map.array` (`t = [ {a: int, b: uint} // tstr ]`) aborts at
  `parsing.rs:1592` (`TODO: non-table types as types`). This belongs to the anonymous-composite family but
  has its own panic site, tracked as a known PANIC row in `tests/matrix_panic/`.
- Nint/float fixed map keys are **rejected gracefully** — only uint and text fixed keys are
  implemented on the struct-map record path (pinned by
  `contain.map-key.memberkey.value.{nint,float}_colon_*` and the matching group-choice-arm rows
  `contain.group-choice-arm.memberkey.value.{nint,float}_map` in `tests/matrix_reject/`). The
  printed remedy — a table `{ * nint => v }` in its own rule — keeps generating. Real nint/float key
  support is the candidate feature; flipping either row to `ok` requires real support, not a decay
  back to the old `group_entry_to_field_name` panics.
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
  works as a member / array element.
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

## wasm-ABI matrix — remaining work (`project_wasm_matrix.ts`)

The system itself (what it is, the axes, how to run/extend it) is documented in `tests/README.md` §
"wasm-ABI matrix". Every enumerated cell compiles — `integration_tests::wasm_matrix_compiles`' `SKIP`
is expected to hold only the permanent `extern__array-element` (it references
a user-supplied type, so it can't compile standalone; integration-tested in `tests/extern-deps`). A red
cell reappearing is a **regression to fix**, not a backlog item. A third, always-on axis runs beside
compile and round-trip: the rust→wasm API-surface parity differential
(`wasm_parity_tests::wasm_api_parity` over the same cells + two depth fixtures — `tests/README.md` §
"rust↔wasm API-surface parity"; its remaining sweeps are `tests/TESTING_ROADMAP.md` item 8). The
remaining frontiers HERE are extending the grid and the behavioural compile→round-trip upgrade (both
below).

**Extending the grid.** Coverage equals the hand-curated type-shape axis (`SHAPES`); a representation not
in it is a silent hole, not a red cell. Periodically ask "which wasm representation are we *not*
enumerating?" and add a shape.

**Behavioural upgrade — remaining.** The wasm-side minted round-trip surface has landed (the emitted
`cddl_generated_wasm_tests` module, `integration_tests::wasm_matrix_roundtrips`, and `verify.ts`'s
default-on `--wasm` probe — documented in `README.md` § annotations and `tests/README.md` § "wasm-ABI
matrix"). Remaining:
- **Unminted wasm shapes** — a `@newtype`/tag/bounded wrapper ENTRY type builds through its public wasm
  `new(inner)`; wrapper-collection ctor args build via a block-expr `new`/`add`/`insert`, and
  `@newtype`/tag/table/array wrapper ctor args via their `From<cddl_lib::Native>` impl. So only extern /
  raw-bytes ctor args (user-supplied types with no generated conversion) — including a wrapper entry
  whose inner is that class, which falls back to a `from_cbor_bytes` build with a loud-skipped ctor
  differential — and the `--wasm-*-macro` modes remain **loud skips** (`eprintln!` — the list in
  `tests/README.md` § "wasm-crate test module"); a cell built entirely from those mints no wasm surface
  and falls back to the compile verdict. (Flatten points are not on this list: optional fields are not ctor args, so
  no mint ever constructs a present-null state — verified against the `nullable__*` cells, which all
  mint or skip only for the unrelated transparent-alias reason.)

**Oracles (`verify.ts` is manual-only):** ruby `cddl` via `gem install --user-install cddl` (verify.ts
auto-resolves it at `Gem.user_dir/bin/cddl`), rust `cddl` via `cargo install cddl` (point `RUST_CDDL` at
`~/.cargo/bin/cddl`), and cddl-codegen builds from this repo. The compile-gate reuses
`integration_tests::feature_corpus_compiles`' pattern (shared `CARGO_TARGET_DIR`, one-time dep warm-up).
The default `RUST_CDDL` is the `local-fixes` sibling checkout — pin it to an immutable copy before a
multi-probe run (`README.md` § "Upstream oracle gaps" has the details and why).

## Explicitly out of scope (decided, not overlooked)

Per the `QUERIES.md` query-map, no consumer query needs these (revisit only if a concrete need surfaces):
- **F8 — matching semantics** (prioritized-choice ordering, greedy occurrence, implicit `:` cut): a
  validation concern, not a cddl-codegen serialization concern.
- **F9 — interaction tuples** (A-in-B-in-role-C-with-operator-D): richer-than-binary containment; stretch
  query (Q7) only.
- **F10 / F11** — note-only (over-acceptance denominator; AST cross-check is weak corroboration).

## Pending decision (needs a human call)

- **Over-acceptance denominator:** should coverage be measured against the grammar's full superset, or only
  realistically-implementable constructs?

(Resolved decisions — D3 corpus numerator; cddl-codegen comment-DSL as the `CDDL_CODEGEN` profile; doc
consolidation — are recorded in the code + git history; not re-litigated here.)

## Maintenance

Upstream specs churn (IANA registries, the grammar). Refresh with `sources/fetch.sh` (re-fetches + verifies
against `SHA256SUMS`); a checksum mismatch flags upstream drift to review before re-pinning and regenerating.

## Related

Broader testing work (clear-wins, roadmap items, the trailing-bytes runtime change) lives in
`tests/TESTING_ROADMAP.md` — the matrix is the reference that testing effort *projects from*.
