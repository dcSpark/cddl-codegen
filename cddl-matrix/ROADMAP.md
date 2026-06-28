# CDDL master matrix — roadmap (what's remaining)

Read `README.md` first. This is the forward-looking list. Status today: **gate-green** — 92 features
(81 RFC8610 + 1 RFC9682 + 10 `CDDL_CODEGEN` vendor profile), all axes reconciled, deterministic, with
**compile-gated execution-grounded support per-feature, per-cell (role × feature), AND per-control-op**
(all 37 IANA ops probed). **Both flagship projections now GENERATE their hand docs:** **golden_hex** (Q3)
and the **corpus feature-axis** projection — `project_corpus.ts` generates `tests/corpus/COVERAGE.md`
(`corpus_detect.ts` floor + `annotations/corpus/` overlay). The north-star target (subsuming COVERAGE.md)
is **DONE** — judged a clear win by two independent cold reviews. What follows is everything *not* yet
done, roughly in priority order.

> **North star.** cddl-matrix exists to do the hard per-construct coverage work **once** and *project* it
> into the **many** docs/consumers that need it — instead of hand-building (and re-sweating) each from
> scratch. The flagship, hardest target is **replacing `tests/corpus/COVERAGE.md`** (the original,
> heavily-invested doc this effort was created to subsume) — the main test that the matrix is really up to
> the job. It is **not the only** consumer: `docs/docs/current_capacities.mdx` (cddl-codegen's
> hand-maintained support list) is another we'd eventually generate from the matrix, and more will follow.
> `tests/golden_hex` (Q3) was the deliberate **warm-up** — a smaller, clean-keyed problem that proved the
> *derive → join an authored overlay → drift-check* shape before tackling corpus. The matrix is "good
> enough" when regenerating a real hand doc from it is a **clear win**.

> **The `F#` labels** below are findings from the independent cold critique (a fresh agent given only
> the problem + specs, no codebase — full write-up in git history). **This is the canonical status
> ledger.** Already **done**: F7 (the query contract), F1 (profile axis), F2 (semantic-individuation +
> grammar-as-lint), F4 (tags modeled parametrically — not pinned/enumerated), F5 (encoding axis is
> major-type-dependent, no over-count), F6 (synthesis snapshot guard). **Partially landed**: F3 — support
> is now **compile-gated** (accept *and* the generated crate compiles, not just an exit-code accept bit;
> this caught the `x = any` false positive), but the full 5-way directional split {accept, encode, decode,
> round-trip, enforce} is still deferred. **Out of scope**: F8–F11. Only the still-open findings appear below.

## 1. Wire the projections — replace the hand-built coverage docs (the north star)

The master exists to be *projected from*; the corpus replacement is the proof of that thesis (if it can't
subsume a doc we sweated over, the premise is unproven).

**DONE — the thesis is proven.** Both flagship projections now generate their hand docs: `golden_hex`
(encoding axis) and the corpus feature-axis projection (`project_corpus.ts` → `tests/corpus/COVERAGE.md`,
subsumed; judged a clear win by two independent cold reviews). The build-order/findings below are retained
as the historical record of *how* corpus was subsumed; the remaining open items are role-aware coverage
(build-order item 6) and the secondary projection targets + queries.

**Done — the golden_hex (encoding-axis / Q3) projection.** `project_golden_hex.ts` generates
`tests/golden_hex/COVERAGE.md` from `matrix.json` + `sources/appendix_a.json` + `tests.rs`, joining the
human rationale in `annotations/golden_hex/cddl_codegen.toml`. The join is a **drift check**: it fails on
a note that contradicts the derived coverage, a stale note, or any uncovered-and-unexplained vector. This
realizes Q3 at the encoding-cell level (per-construct attribution is the F5 expansion, item 4).

### ➡ The main remaining work: subsume `tests/corpus/COVERAGE.md` (Q2, feature axis)

This is the north-star target. The plan comes from a **gap analysis** of the hand doc against the matrix —
decomposing COVERAGE.md by *where each piece must come from*. The key correction over a naive read: the
**per-projection overlay** (`annotations/corpus/cddl_codegen.toml`, exactly like golden_hex's
`reason`/`code_anchor`/`status` fields) absorbs most of the "non-derivable" content, so it is **not** new
machinery.

**Already in the matrix:** ✅/➖ support per spec feature (the `status` annotation).

**Absorbed by the authored overlay** (the half the overlay was built for — `bun`-joined + drift-checked):
- **⚠️ parsed-but-not-honored** rows (cuts dropped, sockets stripped, float-under-`preserve`) → a
  hand-asserted note `status`.
- **"where to fix"** pointers (handler fn / quoted `panic!` / unmatched AST variant) → `code_anchor`.
- **"notable findings"** + per-row nuance (`int` is special-cased, the misleading panic message) → `reason`.
- *Tradeoff:* overlay status is hand-asserted, not execution-grounded — acceptable (same as golden_hex's
  `redundant`/`out_of_scope`); tighten to execution only if F3 (item 3) lands.

**Genuinely new work the overlay can't conjure** (it only annotates ids that already exist):
1. **The coverage numerator — the ✅-vs-➕ split** ("does a *fixture* exercise feature X"). golden_hex
   *derived* its numerator from the asserted bytes; the corpus analog is to derive which feature ids each
   `tests/corpus/*.cddl` exercises by parsing the fixture (reuse the AST→feature mapping `verify.ts` already
   computes for its lint). **Design fork to settle first — this is the one place corpus is genuinely harder
   than golden_hex:** (D1) pure mechanical "construct appears in some fixture's AST" — fully derived but
   over-credits incidental constructs (every fixture uses uints); (D2) an authored fixture→*intended*-feature
   map — matches COVERAGE.md's "this fixture *isolates* construct C" but is hand-asserted; (D3) hybrid —
   derive "appears" as the floor, overlay names the canonical isolating fixture, the join drift-checks it
   really contains the construct. **Decided: D3** — keeps corpus as solid as golden_hex (the editorial
   "isolated by fixture F" is authored but mechanically guarded, so it can't lie).
2. **Control-op support data.** COVERAGE.md has a whole §3.8 control-operator table; the matrix carries the
   37 IANA ops but **zero have a per-tool support annotation** (the 14-op hole Q1 already flagged). Subsuming
   the table needs that base annotation — probed by `verify.ts` (preferred: keeps it execution-grounded) or
   hand-asserted. Also covers the RFC 9165 ops COVERAGE.md lists as "process later" (currently ➖).
3. **Non-spec rows — decided: model the DSL as its own profile.** The comment-DSL (`@name`/`@doc`/…) and
   EXTERN/RAW_BYTES sentinels are cddl-codegen inventions, not RFC 8610 — but instead of walling them off,
   treat them **as if they were their own RFC**: a named profile **`CDDL_CODEGEN`** with ordinary feature
   rows, run through the *same* flow (feature ids → bidirectional lint → support annotations → the D3
   coverage join), so the projection needs **zero** non-spec branch. The profile tag preserves reusability —
   a non-cddl-codegen consumer filters `profile = CDDL_CODEGEN` out as out-of-profile, exactly as it filters
   RFC 9682. **Condition (keeps the backward-lint invariant intact):** these features must resolve to a real
   pinned source like any other — point the lint at the in-repo `src/comment_ast.rs` (the `@`-handlers) +
   `docs/docs/comment_dsl.mdx`; same-repo means "pinned" for free and structurally can't drift from the impl.
   Net: **less** special-casing than the alternatives — every DSL-bearing consumer (corpus,
   `current_capacities.mdx`, …) gets the DSL for free instead of a per-consumer hand-section. *(Rejected:*
   *(a) hand-author each extensions section — recurs as a special case per consumer; (b) a bespoke non-spec*
   *annotation axis — a second mechanism for what the profile axis already does.)* **Amends** README's
   "implementation-agnostic / pure spec" framing to "a union of named profiles, some vendor-specific, tagged
   so other consumers can exclude them" — update the README to say so.

**Build order:**
1. ~~settle the numerator fork~~ **done — D3**.
2. ~~the fixture↔id coverage key (D3 floor + canonical-fixture overlay) + drift-check~~ **done** —
   `corpus_detect.ts` (`featuresIn`, the "appears" floor; over-credits ~3–10×, proving D3 needed),
   `annotations/corpus/cddl_codegen.toml` (33 canonical-fixture assignments lifted from COVERAGE.md), and
   `project_corpus.ts` (drift-checks every assignment + floor completeness).
   ~~the **nuance overlay**~~ **done** — `[[note]]` (3 ⚠️ partial + 13 ➖ with "where to fix" anchors) and
   `[[finding]]` (6 doc-level notes) in the same overlay file, golden_hex `status`/`reason`/`code_anchor`
   style. Drift-checked three ways: note `status` must agree with the matrix support verdict (caught
   `memberkey.cut` mis-marked partial — the explicit `^` cut is genuinely ➖); ids must be real; and every
   `code_anchor` must still exist in `src/` (the COVERAGE.md self-invalidating-evidence rule made
   mechanical — caught a guessed group-socket anchor).
   ~~stand up the `CDDL_CODEGEN` profile~~ **done** — `features/cddl_codegen.toml` models the 8 comment-DSL
   directives + 2 sentinel typenames (`profile = "CDDL_CODEGEN"`). `verify.ts` extended: a backward lint
   resolves each to its `alt` token in the pinned in-repo source (`src/comment_ast.rs` / `src/parsing.rs`);
   a forward lint requires every **documented** (`comment_dsl.mdx`) directive/marker to be modelled —
   which caught the internal `_CDDL_CODEGEN_SCOPE_MARKER_` and correctly excluded it; and vendor features
   are spec-valid by cddl-codegen's authority, not the ruby/rust oracles (which reject the sentinel
   typenames — expected). All 10 probe `supported`; 8 `dsl.*` got canonical-fixture cover entries; `ext.*`
   are ➕ (no corpus fixture — integration-tested). `corpus_detect` maps `@dir`→`dsl.*` / sentinels→`ext.*`.
   README amended (vendor-profile invariant). **Step 2 complete.**
3. ~~reconcile the support seam~~ **done** (see Findings — it split two ways; do NOT just edit examples to
   green the probe).
4. ~~finish `project_corpus.ts` into the **renderer** (mechanical, golden_hex shape) and diff it against the
   existing hand doc **before** deleting any hand parts~~ **done — the north-star target is subsumed.**
   `project_corpus.ts` now generates `tests/corpus/COVERAGE.md` (profile → production → id; `markFeature`
   joins cover/note/support). Shipped via: render to a `*.generated.md` draft → diff vs the hand doc → two
   independent cold reviews (1st: "not yet a win", which drove the compile-gate + items 6/7 scoping; 2nd:
   "clear win" after the fixes) → flip `OUT` to the live `COVERAGE.md`. Role-aware coverage in the render
   still awaits item 6 (disclosed in the doc, not faked).
5. ~~per-cell support (role × feature)~~ **done** — `verify.ts` now probes each containment cell's example
   through cddl-codegen → an execution-grounded support bit **per (feature, role)**, keyed by the
   containment id in the same `[[support]]` table (28 cells). This is the matrix's core "supported HERE,
   not THERE" capability — the **CONTEXT axis** (`roles × containment`), which is v1. **Correction:** I
   earlier mis-called the value/null contextual split "F3" — F3 is the orthogonal *operation* axis
   (accept/encode/decode/round-trip/enforce); this is the *nesting/context* axis, and per-cell support now
   holds it **structurally** (replacing the prose-note flatten). Added the value/null × member/choice cells:
   `value.number`/`value.text`/`type2.value` supported as array-elements, `prelude.null` supported as a
   choice-member — all `unsupported` at top-level. Example payoff: `type2.map` is supported as tag-content
   but unsupported inline in choice/array/cbor/generic/map-value (the anonymous-group limit — see Findings).
6. **Step 3 (remaining) — role-aware corpus coverage.** The corpus floor (`corpus_detect` `featuresIn`) is a
   text scan: it detects THAT a construct appears, not WHICH role. To key corpus coverage on `(feature,
   role)` — e.g. "null is covered as a choice-member by `nullable.cddl`" vs the uncovered top-level cell (the
   `nullable.cddl` mis-naming) — replace the ad-hoc scan with a real **AST walk via the `cddl` crate**
   (=0.9.1, the exact one cddl-codegen builds with) through a small `cargo run` JSON-AST dump: walk the AST
   and read each construct's enclosing role directly. Caveat (parse-coverage) is soft — the crate parses
   *CDDL* not CBOR, and since cddl-codegen uses it, anything in our corpus parses by construction;
   role-detection only needs the crate to PARSE (see nesting), not for cddl-codegen to support it.
7. ~~**Control-op support.** Probe each of the 37 IANA control operators through cddl-codegen → a per-op
   support annotation keyed by `ctl.<name>`~~ **done.** `control_examples.toml` carries a minimal `example`
   per op (the byte-pinned CSV can't), joined by `lib.ts`; `verify.ts` probes all 37 (compile-gated;
   ruby/rust corroboration-only since the IANA registry is authoritative — no `spec_invalid` for ops a
   given ruby/rust version lacks; a hard-fail guards any op missing an example). Result: **9 supported**
   (`.size .cbor .default .eq .ne .le .lt .ge .gt`) / **28 unsupported** (panic or parse-reject) — matches
   the hand doc's §3.8 exactly. `project_corpus.ts` renders the table from this data (the `noteSkipped`/❓
   "deferred" framing is gone).
- **Support seam — TWO root causes, not one.** The drift-check found 7 constructs marked `unsupported`
  yet touched by a working fixture (probe runs the minimal `example`; cddl-codegen only emits for *named
  composite types*). The honest reckoning (a first pass wrongly "fixed" all 7 by editing examples — which
  **hid real gaps**; corrected):
  - **3 were degenerate-example artifacts** — the construct works; the example hit an *orthogonal* bug.
    Fixed with minimal *feature-isolating* examples: `memberkey.bareword`→`m = [name: tstr]`,
    `occur.optional`→`g = [? name: tstr]` (single-field **arrays** dodge the single-field-**map** bug),
    `type2.map`→`foo = { * tstr => int }` (table). Now correctly `supported`.
  - **4 were genuine gaps** — top-level fixed-value / null **types** (`answer = 42`, `version = 5`,
    `marker = "v1"`, `x = null`) panic (`should not expose Fixed type in member`): cddl-codegen uses `Fixed`
    only for serialization as a struct *member*, never as an exposed standalone type. Kept minimal, left
    `unsupported`, and **reported as real gaps** (➖ notes in the corpus overlay) — not relabelled by editing
    the example. `type2.value`, `value.number`, `value.text`, `prelude.null`. The corpus touches them only
    as members — the **contextual** reality (the role × feature axis), now held structurally by per-cell
    support (build-order item 5), NOT "F3" as I first mislabelled it.
- **Lesson (baked into the overlay header + the C-seam check):** a panic on minimal *valid* CDDL is a
  finding to surface, not something to engineer away by making the probe green.
- **Anonymous-group limitation (pervasive contextual fact, surfaced by per-cell support).** An INLINE
  anonymous map/array/group nested in a choice / array-element / cbor-payload / generic-arg / map-value /
  occurrence position panics (`parsing.rs` "Anonymous groups not allowed") — it must be **named** (a rule
  or `@name`). The one exception: **tag-content** accepts an inline composite. So `type2.map` is supported
  as tag-content, unsupported inline elsewhere, and works everywhere via a named reference — the per-(feature,
  role) verdict genuinely differs, which is the whole point.
- **Containment cell-example hygiene (DONE).** Enabling per-cell support exposed single-field-map cell
  examples that confounded the panic reason. Fixed the **3 with wrong verdicts** (`map-value.type.choice`/
  `type2.tag`, `tag-content.type2.map` → `supported` at 2 fields) and made the **5 `type2.map`-in-a-role
  cells** (`array-element` / `cbor-payload` / `choice-member` / `generic-arg` / `occurrence-target`)
  2-field, so they panic for the real **anonymous-group** reason (verdicts unchanged — `unsupported`). No
  single-field struct-map cell example remains. (`map-key.type2.array` already panicked at the
  anonymous-group site — correctly reasoned, left as-is.)
- **Bugs/gaps recorded as findings (candidate cddl-codegen fixes):** top-level fixed-value/null types
  (above); single-field **struct** maps panic (`{ a: uint }` → `unsupported table map key`); mixed
  struct+table maps unsupported (`{ a: uint, * k => v }`); inline anonymous nested composites need a name.
- **Compile-gate (F3 partial) — the support probe now requires the generated crate to COMPILE, not just
  cddl-codegen exit 0.** This caught a real false positive the exit-code probe had laundered: `x = any`
  exits 0 but emits `pub type X = Any;` (undefined type → won't compile), so `prelude.any` is now correctly
  ➖ (root cause: `any` absent from `is_identifier_reserved`). Triage of the 9 flips it produced split into:
  1 real gap (`any`); 4 degenerate examples fixed to compiling feature-isolating forms (`prelude.int`→
  member-form, `type.choice`→drop `bool`, `genericarg.type1`→rename the `r` rule, the cbor-payload tag
  cell→`uint` payload); and 4 **user-code** features kept `supported` via a documented `COMPILE_GATE_EXEMPT`
  allowlist (they reference user-supplied code, so can't compile standalone — `ext.extern`, `ext.raw_bytes`,
  `dsl.custom_serialize`, `dsl.custom_deserialize`; integration-tested). **New cddl-codegen bugs surfaced
  and recorded as findings** (candidate fixes; each disclosed, none laundered): `bool` in a type choice →
  `E0282`; a single-letter rule `r` collides with the deserializer's reader generic `R` → `E0574`; bare
  `x = int` / `int` `.cbor` payload emit an undefined `Int` wrapper; `float16`/float-choice aliases
  unsupported (no native f16) while `float32/64` work; floats fail under `--preserve-encodings`; generics
  on plain groups rejected. **Lesson reinforced (both directions):** a compile failure on minimal *valid*
  CDDL is a finding to surface — and the inverse, don't *invent* a gap from a degenerate example (each
  example fix was verified to leave the construct genuinely working in normal use before relabelling).
- **Oracles are now installable/installed** (so `verify.ts` runs outside CI): ruby `cddl` via
  `gem install --user-install cddl` (verify.ts auto-resolves it at `Gem.user_dir/bin/cddl`), rust `cddl`
  via `cargo install cddl` (point `RUST_CDDL` at `~/.cargo/bin/cddl`), and cddl-codegen builds from this
  repo. The compile-gate reuses `integration_tests::feature_corpus_compiles`' pattern (shared
  `CARGO_TARGET_DIR`, one-time dep warm-up).

### Other projection targets (after corpus)

The matrix is meant to feed **many** consumers; corpus is just the hard flagship. The next real hand doc to
subsume:
- **`docs/docs/current_capacities.mdx`** — cddl-codegen's hand-maintained "what we support" list. Its
  natural query is **Q1** ("constructs unsupported but in target profile" → the inverse is the *supported*
  list). The support + profile data already exist in `matrix.json`; this conversion is the right home for
  the profile-filtered support/gap logic (a `query_q1_gaps.ts` prototype was built and removed as premature
  — no consumer yet; rebuild it here, against the real doc, when this lands). Q1 stays a `QUERIES.md`
  definition-of-done query in the meantime — the capability is proven, just not emitted as a standing artifact.

### Secondary queries (after corpus)

**Q4/Q5/Q6 query scripts** from `QUERIES.md`. Q5 (matrix self-completeness) and Q6 (profile/version diff)
are largely already satisfied by `verify.ts`'s reconciliation + the F6 snapshot — they need only thin query
scripts. **Q4** (directional support) is blocked on item 3 (F3 execution).

## 2. Wire the gate into CI

All tooling is **Bun/TypeScript, run manually** from `cddl-matrix/`; `lib.ts` holds the shared loaders +
the stable-JSON serializer. Oracle paths are env-overridable (`RUST_CDDL`, `RUBY_CDDL`; `CODEGEN_DIR`
derives from the repo root).

**Full verification suite (run all to confirm consistency — none are wired into CI yet):**
- `bun run build_matrix.ts --check` — snapshot/drift gate: `matrix.json` matches the authored overlay.
- `bun run verify.ts` — reconcile (bidirectional grammar/prelude/vendor lints) + probe **per-feature,
  per-cell, AND per-control-op** support, **compile-gated** (generate + `cargo check`); rewrites
  `annotations/cddl_codegen.toml`. Needs the three oracles present (ruby `cddl`, rust `cddl` CLI,
  `cddl-codegen`) — installable: `gem install --user-install cddl`, `cargo install cddl` (set `RUST_CDDL`),
  cddl-codegen builds from this repo. Slow (probes ~156 examples × generate+compile via `cargo`).
- `bun run project_corpus.ts` — **generates `tests/corpus/COVERAGE.md`** + the overlay validator gate
  (canonical-fixture drift + note↔support agreement + `code_anchor` exists in `src/` + floor completeness).
- `bun run project_golden_hex.ts` — golden_hex (encoding-axis) projection + drift-check.
- `bun run corpus_detect.ts` — runs the `featuresIn` detector's self-check (and prints the floor diagnostic).

Remaining:
- **Wire both into CI** so the drift-check, snapshot guard, and reconciliation run automatically.
  `verify.ts` needs the three oracle tools present (ruby `cddl`, rust `cddl` CLI, `cddl-codegen`); have
  the CI lane provide them or skip that probe gracefully when absent.
- **Typecheck enforcement.** The scripts are strict-typed but nothing runs `tsc` yet — the types are only
  validated ad hoc. Add a `tsconfig.json` + `@types/bun` (dev-only) and a `tsc --noEmit` step beside the
  gate. (Needs an ambient `declare module "*.toml"` for the `project_golden_hex.ts` import.) This is the
  one place a (dev) dependency is worth it; the runtime stays dependency-free.

## 3. F3 — directional / enforcement support (partially landed; full split still deferred)

**Update:** support is no longer one bit. The probe is now **compile-gated** — it generates the crate AND
`cargo check`s it, so "supported" means *accept + compiles* (this caught `x = any`). That's the first two
rungs of the ladder. `QUERIES.md` Q4 still wants the full 5-way split **{accept, encode, decode,
round-trip, enforce-constraint}**, which needs per-feature *execution* of the compiled code (round-trip +
constraint checks), not just compilation — real machinery still deferred. Q4 is blocked until that lands.

**Not a corpus blocker.** The corpus projection (item 1) consumes the directional ⚠️ distinction
(parsed-but-not-honored: cuts, sockets, float-under-`preserve`) via **hand-asserted overlay notes**, not
execution — so F3 stays deferred. F3 is the *upgrade path*: it would let those ⚠️ verdicts be
execution-grounded instead of authored, and unblocks Q4.

## 4. F4 / F5 follow-ons (only when their consumer exists)

- **F5 (encoding precision):** the encoding axis is already major-type-dependent (no impossible cells).
  The golden-vector projection (item 1) is now wired and lists uncovered legal cells *globally*; the
  remaining F5 work — per-*construct* legal-cell enumeration so Q3 can say "for construct C, these legal
  encodings are untested" — is now unblocked. Link `features[].encodings` to the leaf cells each construct
  can emit and intersect with golden coverage.
- **F4 (tag registry):** deliberately not pinned/enumerated (cddl-codegen is tag-parametric). Revisit
  only if a *tag-semantic* consumer of the master appears.

## 5. Expansion (when relevant)

- **Profiles/versions:** v1 targets the RFC 8610/9682 grammar + the IANA control-op registry (which
  already spans RFC 8610/9090/9165/9741). Add other CDDL profiles (e.g. the modules drafts) if needed,
  and bump cddl-codegen's declared target profile if it updates its `cddl` dependency.
- **More tools:** the master is implementation-agnostic; add `annotations/<other-tool>.toml` if another
  consumer adopts it.

## Explicitly out of scope (decided, not overlooked)

Per the `QUERIES.md` query-map, no consumer query needs these, so they're out of v1 (revisit only if a
concrete need surfaces):
- **F8 — matching semantics** (prioritized-choice ordering, greedy occurrence, implicit `:` cut). More a
  validation concern than a cddl-codegen serialization concern; not needed by any query yet.
- **F9 — interaction tuples** (A-in-B-in-role-C-with-operator-D): a richer-than-binary containment;
  stretch query (Q7) only.
- **F10 / F11** — note-only (over-acceptance denominator; AST cross-check is weak corroboration).

## Pending decisions (need a human call)

- The judgment-call critique findings — e.g. the **over-acceptance denominator**: should coverage be
  measured against the grammar's full superset, or only realistically-implementable constructs?
- (Resolved) **Corpus numerator fork** — chose **D3 (hybrid)**: derive "appears" mechanically as the floor,
  overlay names the canonical isolating fixture per construct, the join drift-checks the claim. Keeps corpus
  as solid as golden_hex. (Alternatives were D1 pure-derived/over-credits, D2 authored-map/can-drift.)
- (Resolved) **Non-spec rows** — model cddl-codegen's comment-DSL + sentinels as their own profile
  (`CDDL_CODEGEN`), a vendor "RFC" run through the same flow (pinned in-repo source + lint + features), not a
  hand-section. Amends README's "pure spec" framing to "union of named profiles, some vendor-tagged" (item 1).
- (Resolved this session) Doc consolidation is done: `README.md` is the entry; `ROADMAP.md` /
  `QUERIES.md` / `sources/MANIFEST.md` are the rest. The session-process docs (the scale report and the
  cold-critique write-up) were folded into these and removed — recoverable from git history if needed.

## Maintenance

Upstream specs churn (IANA registries, the grammar). Refresh with `sources/fetch.sh` (re-fetches +
verifies against `SHA256SUMS`); a checksum mismatch flags upstream drift to review before re-pinning and
regenerating.

## Related

Broader testing work (clear-wins, roadmap items, the trailing-bytes runtime change) lives in
`tests/TESTING_ROADMAP.md` — the matrix is the reference that testing effort *projects from*.
