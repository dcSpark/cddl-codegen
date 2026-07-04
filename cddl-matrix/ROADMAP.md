# CDDL master matrix — roadmap (remaining work + findings)

Read `README.md` first (the model + current state). This doc is the forward-looking list **and** the
hard-won findings — the two things a future agent can't re-derive from the code. The blow-by-blow of *how*
the done work landed lives in git history (this doc was pruned of it; the project already did the same with
the scale report + cold-critique).

**Status: gate-green.** 92 features (81 RFC8610 + 1 RFC9682 + 10 `CDDL_CODEGEN` vendor profile), all axes
reconciled/deterministic, with execution-gated support **per-feature, per-cell (role × feature), and
per-control-op** (all 37 IANA ops probed): "supported" requires the generated crate's `--emit-tests`
round-trip/reject tests to PASS (`cargo test`), falling back to the compile verdict only for shapes that
mint no test surface (recorded honestly in the evidence). Both flagship projections GENERATE their
hand docs and drift-check: `golden_hex` (encoding axis, Q3) and the `corpus` projection (feature axis Q2 +
per-cell **role × feature** coverage). The north-star — subsuming `tests/corpus/COVERAGE.md` — is **DONE**
(two independent cold reviews: "clear win").

> **North star.** Do the hard per-construct coverage work **once** and *project* it into the **many** docs
> that need it, instead of hand-maintaining (and re-sweating) each. Corpus was the hard flagship;
> `docs/docs/current_capacities.mdx` is next; more follow. The matrix is "good enough" when regenerating a
> real hand doc from it is a **clear win**.

> **Findings ledger (F#)** from the cold critique (full write-up in git history): F1, F2, F4–F7 **done**;
> **F3 partial** (support is execution-gated — round-trip + reject tests run per probe; the directional
> encode/decode split is deferred — §3); **F8–F11 out of scope** (bottom). Only still-open findings are
> sections below.

## 1. Remaining work — projections & queries

The matrix exists to feed **many** consumers; corpus was just the hard flagship. What's left:

- **`docs/docs/current_capacities.mdx` (Q1)** — cddl-codegen's hand-maintained "what we support" list. Its
  natural query is Q1 ("constructs unsupported but in target profile" → the inverse is the *supported*
  list). The support + profile data already exist in `matrix.json`; this is the right home for the
  profile-filtered support/gap logic (a `query_q1_gaps.ts` prototype was built and removed as premature —
  rebuild it here, against the real doc). Q1 stays a `QUERIES.md` definition-of-done query meanwhile — the
  capability is proven, just not emitted as a standing artifact.
- **Secondary query scripts (Q4/Q5/Q6)** from `QUERIES.md`. Q5 (matrix self-completeness) and Q6
  (profile/version diff) are largely already satisfied by `verify.ts`'s reconciliation + the F6 snapshot —
  they need only thin query scripts. **Q4** (directional support) is blocked on F3's remaining
  encode/decode split (§3).
- **Full role × feature coverage grid.** The corpus projection keys coverage on `(role × feature)` only for
  the cells where support *differs by role* (`prelude.null`, the literal values). A full grid for *every*
  construct is unbuilt — the floor data (`corpus_detect.ts` `rolesIn`, via `examples/ast_roles.rs`) already
  supports it; wire it into `project_corpus.ts` if a consumer wants the complete matrix view.

## 2. Running the verification suite

**CI policy — fast tier only.** CI runs exactly `bun run check.ts fast` (cost policy; see
`AGENTS.md`); the fast tier includes this directory's drift gates. The **(CI)** markers below mean
"in check.ts's fast tier"; everything else runs locally (check.ts `local`/`full`), and promoting a
gate into `fast` is a maintainer decision.

All tooling is **Bun/TypeScript, run manually** from `cddl-matrix/`; `lib.ts` holds the shared loaders +
the stable-JSON serializer. Oracle paths are env-overridable (`RUST_CDDL`, `RUBY_CDDL`; `CODEGEN_DIR`
derives from the repo root).

**One entry point.** `bun run check.ts full` (from the repo root) runs every gate below in tier order
— the CI-wired drift checks plus the manual `verify.ts` / `corpus_detect.ts` probes — with oracle
preflight and a PASS/FAIL summary; it is the "run all to confirm consistency" path. The per-script
commands below stay the reference for running one gate in isolation.

**Full verification suite (run all to confirm consistency).** The fast, pure-`matrix.json` drift checks
are **wired into CI** via check.ts's fast tier (`build_matrix.ts --check`,
`project_robustness.ts --check`, `project_wasm_matrix.ts --check`, `project_golden_hex.ts --check`,
and `project_corpus.ts`); the heavy `verify.ts` oracle probe runs manually (check.ts `full` tier)
and stays out of CI.
- `bun run build_matrix.ts --check` — snapshot/drift gate: `matrix.json` matches the authored overlay. **(CI)**
- `bun run project_wasm_matrix.ts --check` — drift gate for the wasm-ABI matrix fixtures
  (`tests/matrix_wasm/*.cddl`); pure `SHAPES`/`ROLES` projection, no cargo/oracles. **(CI)**
- `bun run verify.ts` — reconcile (bidirectional grammar/prelude/vendor lints) + probe **per-feature,
  per-cell, AND per-control-op** support, **execution-gated** (generate with `--emit-tests=true` +
  `cargo test`; a failing test run pays one extra `cargo check` to classify does-not-compile vs
  round-trip-fails); rewrites `annotations/cddl_codegen.toml`. By **default** it also runs the **wasm
  oracle** — regenerate each example `--wasm=true --emit-tests=true` and `cargo test` the generated wasm
  crate (the emitted `cddl_generated_wasm_tests` module: cross-crate byte differential + wire round-trip
  + accessor read-back), threaded into the per-feature / per-cell evidence as `minted_wasm` /
  `wasm_roundtrips`. The rust round-trip verdict still gates support; wasm is corroborating evidence, so
  it never downgrades a verdict. Opt out for a faster run with `--no-wasm` (or `VERIFY_WASM=0`), which
  roughly halves the per-probe cargo work. Needs the three oracles (ruby `cddl`, rust `cddl` CLI,
  `cddl-codegen`) — installable: `gem install --user-install cddl`, `cargo install cddl` (set
  `RUST_CDDL`), cddl-codegen builds from this repo. Slow (probes ~156 examples × generate + `cargo test`
  ×2 crates via `cargo`).
- `bun run project_corpus.ts` — **generates `tests/corpus/COVERAGE.md`** + the overlay validator gate
  (canonical-fixture drift + note↔support agreement + `code_anchor` exists in `src/` + floor completeness +
  per-cell role coverage drift + cell-support check H). Builds/runs `examples/ast_roles.rs` (the role
  floor), so it needs the cargo toolchain like `verify.ts`. **(CI)**
- `bun run project_golden_hex.ts` — golden_hex (encoding-axis) projection + drift-check; `--check` is
  the CI mode (fails on a stale committed COVERAGE.md without rewriting it). **(CI)**
- `bun run project_robustness.ts` — projects the support verdict into the robustness-catalog fixtures
  (`tests/matrix_{supported,panic,reject}/*.cddl`); `--check` is the drift gate (and cross-checks each
  committed generation-outcome catalog against the matrix verdict — for the reject catalog, per row by
  evidence class). Pure `matrix.json` read (no cargo/oracles), so it's a fast CI gate. **(CI)**
- `bun run corpus_detect.ts` — runs the `featuresIn` + role-aware (`rolesIn`) self-checks and prints the
  text-scan + role-aware floor diagnostics. The role floor builds/runs `examples/ast_roles.rs` (needs cargo).

## 3. F3 — directional / enforcement support (execution-gated; encode/decode split deferred)

Support is execution-gated: the probe generates with `--emit-tests=true` AND runs `cargo test`, so
"supported" means *accept + compiles + the IR-minted round-trip and bounded-reject tests pass* wherever the
type mints a test surface (a per-probe `minted` bit keeps the evidence honest — `x = any` mints nothing
AND fails to compile, so it stays unsupported). Shapes with no STANDALONE surface — transparent aliases,
bounded/newtype-able aliases, named tables/arrays, pure c-enums — are re-probed wrapped in a synthetic
record holder (`__probe_holder = [0, <rule>]`, per-probe `embedded` bit) so their embed-site wire path (the
only one they have) runs, and the evidence reads "round-trips when embedded"; the embed only UPGRADES
evidence, never downgrades a verdict. Of `QUERIES.md` Q4's 5-way split **{accept, encode, decode,
round-trip, enforce-constraint}**, round-trip and (bounds-class) enforce-constraint are now grounded for
both minting and embed-covered shapes; still deferred: the **encode vs decode** direction (a round-trip
conflates them — splitting needs per-direction reference vectors, not just self-consistency). The embed
fallback already carries into the preserve/json profiles: the **emission axis** (`verify.ts` re-probes
every default-`supported` row under each non-default codegen profile through the same rust-only
generate → `cargo test` → embed pipeline, recorded as `emission.<name>.*` annotation keys — see
`cddl-matrix/README.md`). Q4 stays blocked on the encode-vs-decode directional split alone.

**Not a corpus blocker.** The corpus projection consumes the directional ⚠️ distinction
(parsed-but-not-honored: cuts, sockets, float-under-`preserve`) via **hand-asserted overlay notes**, not
execution — those stay hand-asserted even now: cut/socket semantics are validation concerns a round-trip
can't observe. Float-under-`preserve` is now execution-grounded on the emission axis (`verify.ts` probes
each default-`supported` row under `preserve`/`json`), but the corpus projection's ⚠️ note stays
hand-asserted because it is a per-corpus-fixture annotation, not a per-master-row verdict.

## 4. F4 / F5 follow-ons (only when their consumer exists)

- **F5 (encoding precision):** the encoding axis is already major-type-dependent (no impossible cells). The
  golden_hex projection lists uncovered legal cells *globally*; the remaining work is per-*construct*
  legal-cell enumeration so Q3 can say "for construct C, these legal encodings are untested" — link
  `features[].encodings` to the leaf cells each construct can emit and intersect with golden coverage.
- **F4 (tag registry):** deliberately not pinned/enumerated (cddl-codegen is tag-parametric). Revisit only
  if a *tag-semantic* consumer of the master appears.

## 5. Expansion (when relevant)

- **Profiles/versions:** v1 targets the RFC 8610/9682 grammar + the IANA control-op registry (spans RFC
  8610/9090/9165/9741). Add other CDDL profiles (e.g. the modules drafts) if needed, and bump cddl-codegen's
  declared target profile if it updates its `cddl` dependency.
- **More tools:** the master is implementation-agnostic; add `annotations/<other-tool>.toml` if another
  consumer adopts it.
- **Representability gaps (features the matrix cannot even mark as uncovered):** the grammar lint is
  hard-gated only for `type2`'s 12 alternatives; the soft alt-coverage for the other productions is
  currently too noisy to promote (`normalizeAlt` exact-string matching reports `rule`/`type1`/
  `genericarg`/`genericparm` as false-uncovered although feature rows exist, and `head-number`'s
  semantics live under `type2.*` rows so it renders "NOT MODELED"). Separately, some *variations inside*
  one ABNF alternative have no feature row at all: one-sided occurrence bounds (`n*`/`*m` — prose-only
  in `occur.bounded`'s desc; only `2*5` is probed anywhere) and the `uint` radix forms `0x`/`0b` +
  `hexfloat`. Until rows exist, Q5 ("everything the matrix does not model") is authoritative only for
  `type2` — tighten `normalizeAlt` and add the variation rows before claiming more.

## Findings & gotchas (durable — read before touching the support seam or probe examples)

The hard-won lessons. The recurring rule: **a panic/compile-failure on minimal *valid* CDDL is a finding to
surface, not something to engineer away by making the probe green — and the inverse, don't *invent* a gap
from a degenerate example.**

- **Support seam — TWO root causes, not one.** The drift-check found 7 constructs marked `unsupported` yet
  touched by a working fixture (the probe runs the minimal `example`; cddl-codegen only emits for *named
  composite types*). A first pass wrongly "fixed" all 7 by editing examples — which **hid real gaps**. The
  honest split:
  - **3 were degenerate-example artifacts** — the construct works; the example hit an *orthogonal* bug.
    Fixed with minimal *feature-isolating* examples: `memberkey.bareword`→`m = [name: tstr]`,
    `occur.optional`→`g = [? name: tstr]` (single-field **arrays**, chosen when single-field **maps**
    still panicked — that bug is fixed, so these could move to map forms on a future `verify.ts` refresh),
    `type2.map`→`foo = { * tstr => int }` (table). Now correctly `supported`.
  - **4 were genuine gaps** — top-level fixed-value / null **types** (`answer = 42`, `version = 5`,
    `marker = "v1"`, `x = null`) panic (`should not expose Fixed type in member`): cddl-codegen uses `Fixed`
    only for serialization as a struct *member*, never as an exposed standalone type. Left `unsupported`,
    **reported as real gaps** (➖ notes in the corpus overlay), not relabelled. The corpus touches them only
    as members — the **contextual** reality (the role × feature axis), held structurally by per-cell support.
- **Anonymous-group limitation (pervasive contextual fact, surfaced by per-cell support).** An INLINE
  anonymous map/array/group nested in a choice / array-element / cbor-payload / generic-arg / map-value /
  occurrence position panics (`parsing.rs` "Anonymous groups not allowed") — it must be **named** (a rule or
  `@name`). The one exception: **tag-content** accepts an inline composite. So `type2.map` is supported as
  tag-content, unsupported inline elsewhere, and works everywhere via a named reference — the per-(feature,
  role) verdict genuinely differs, which is the whole point.
- **Containment cell-example hygiene.** The `type2.map`-in-a-role cells (`array-element` / `cbor-payload` /
  `choice-member` / `generic-arg` / `occurrence-target`) use 2-field map examples so any panic is
  attributable to the real **anonymous-group** reason (an inline map inside a role needs a name), with no
  single-field-map shape to confound it.
- **Execution-gate (F3) — the support probe requires the generated crate's emitted tests to PASS, not just
  cddl-codegen exit 0.** The compile layer caught a false positive the exit-code probe had laundered:
  `x = any` exits 0 but emits `pub type X = Any;` (undefined type → won't compile), so `prelude.any` is
  correctly ➖ (root cause: `any` absent from `is_identifier_reserved`). The test layer (`--emit-tests` +
  `cargo test`) runs the IR-minted round-trip/reject module per probe; a "compiles but emitted round-trip
  tests fail" verdict is its distinct false-positive class. 4 user-code features stay `supported` via a
  documented `COMPILE_GATE_EXEMPT` allowlist (they reference user-supplied code, so can't compile — or
  test — standalone: `ext.extern`, `ext.raw_bytes`, `dsl.custom_serialize`, `dsl.custom_deserialize`;
  integration-tested).
- **Role floor (role × feature coverage) — NOT a serde JSON-AST dump.** The `cddl` AST's `Serialize` is gated on
  `target_arch = "wasm32"`, so there's no free serde dump on a native build; `examples/ast_roles.rs`
  hand-walks via the crate's `Visitor` trait instead. Role-detection only needs the crate to PARSE (soft
  caveat) — which it does for the whole corpus by construction. `project_corpus.ts` verifies a role-keyed
  `[[cover]]` against the AST role floor AND the per-cell support verdict (check H), so it can't claim ✅ on
  an unsupported cell.

**Bugs / gaps surfaced as findings (candidate cddl-codegen fixes — the matrix's actual payoff):**
- Top-level fixed-value / null **types** panic (`should not expose Fixed type in member`), though fixed
  values serialize fine as struct members. A singleton-value type is a reasonable feature.
- Mixed struct+table maps (`{ a: uint, * k => v }`) unsupported — a map is detected as EITHER a struct or a
  homogenous table, never both. Inline anonymous nested composites need a name.
- Zero-permitting occurrences (`*` / `0*n` / `*n`) on a keyed struct-map field are **rejected gracefully**
  (pinned by `tests/robustness/map_field_zero_occurrence.cddl`) rather than silently narrowed to a mandatory
  field — the narrowing generated decoders that reject valid CBOR omitting the entry, invisible to
  round-trip tests. `+` / `n*m` with a lower bound ≥ 1 still generate a mandatory field: under unique map
  keys they collapse to exactly-one, so mandatory is the honored semantics. Real support for `*` (an
  `Option<T>` field, like `?`) is a candidate feature; surfaced while testing the single-field struct-map fix.
- A single-letter rule named `r` collides with the deserializer's reader generic `R` → `E0574`.
- Bare `x = int` / an `int` `.cbor` payload emit an undefined `Int` wrapper (`cannot find type Int`); `int`
  works as a member / array element.
- `float16` / float-choice aliases unsupported (no native Rust f16) while `float32/64` work; floats fail
  under `--preserve-encodings`; generics on plain groups rejected.
- **Two-sided negative range as a record field panics the generator.** `rec2 = [q: -10..-3]` → `internal
  error: entered unreachable code` at `bounds_check_if_block`'s `(None,None)` arm — the negative range's
  bounds don't reach the field bounds check as `(Some,Some)`.
- **Top-level two-sided negative range silently drops its bounds.** `c = -10..-3` emits `pub type C = i64;`
  with no range check at all (bounds lost).
- **Untrusted length-prefix over-allocation (DoS — dependency-level, fix deferred upstream).**
  *Validated:* the 11-byte input `[0x7b, 00,00,00,00, 80,00,e8,00, 2e,f6]` — a text string whose 8-byte
  length header claims `0x80_00e800` = 2,147,543,040 bytes — drives a single **~2 GB allocation before any
  payload byte is read**, an OOM abort in every consumer parsing untrusted chain data. Reproduced
  standalone (allocation-recording global allocator, no libfuzzer): the largest single `alloc` equals the
  claimed length exactly. *Root cause:* `cbor_event` 2.4.0 `src/de.rs` definite-length string branches
  pre-size from the untrusted length — `let mut bytes = vec![0; len as usize]; read_exact(..)` at
  `bytes_sz` (~486), `text_sz` definite (~545), and `text_sz` indefinite-chunk (~534). The **indefinite
  `bytes_sz`** branch is already safe (`by_ref().take(len).read_to_end`, which grows with available input);
  the fix ports that pattern to the three unsafe sites (+ an `Error::NotEnough` on short read). *Why not
  fixed here:* generated crates are standalone and depend on crates.io `cbor_event` directly
  (`static/Cargo_rust.toml`), so the fix needs a published/forked `cbor_event`, not a codegen change — and
  reimplementing bounded string reads in `static/` (all `Sz` variants + indefinite chunks + preserve-mode
  encoding capture, across 8+ emit sites) is heavy and risky for a serialization runtime. *Disposition
  (maintainer call):* ledger + upstream report; the exact 3-hunk patch, standalone repro, and repoint steps
  are in `draft/cbor-event-overallocation-fix.md`, and the ready-to-submit upstream PR text is in
  `draft/cbor-event-upstream-pr.md`. Surfaced by `fuzz/`; it has no cargo-test crash-replay because an
  OOM/stack-overflow kills the test process — the fuzz process boundary is the only oracle for this class.
  Related, unowned axis: generated crates float on semver `cbor_event = "2.4.0"`
  (`static/Cargo_rust.toml`), so nothing tests the version RANGE consumers actually resolve — the
  upstream fix above will arrive as exactly such a version event; a `--minimal-versions`-style or
  pinned-latest check of a generated crate would own it.
  Sibling panic in the same fuzz session — the collection-loop `assert_eq!(special, Break)` abort on
  `0x81 0xf6` — **was** codegen-owned and is fixed (graceful `Err`; regression in `tests/core`
  `structural_rejects`).

## wasm-ABI matrix — remaining work (`project_wasm_matrix.ts`)

The system itself (what it is, the axes, how to run/extend it) is documented in `tests/README.md` §
"wasm-ABI matrix". Every enumerated cell now compiles — the only entry left on
`integration_tests::wasm_matrix_compiles`' `SKIP` is the permanent `extern__array-element` (it references
a user-supplied type, so it can't compile standalone; integration-tested in `tests/extern-deps`). A red
cell reappearing is a **regression to fix**, not a backlog item. The remaining frontiers are extending the
grid and the behavioural compile→round-trip upgrade (both below).

**Wrapper-vs-transparent — route through one predicate.** The recurring wasm-boundary bug source was
naming, boundary conversion, and exposability each *separately* deciding whether an ident is exposed as a
`#[wasm_bindgen]` wrapper struct or a transparent `pub type` — a *struct-table* property, not a
`ConceptualRustType` shape (a named collection `nums = [* uint]` is a wrapper; a passthrough `arr2 = arr`
is transparent — same IR shape). The single source of truth is `IntermediateTypes::has_wasm_wrapper(ident)`;
new decision sites should consult it instead of re-deriving. Gotcha it encodes: an exposable named array
has a wrapper struct *and* is used transparently as `Vec<T>`, so a passthrough-alias emission must gate on
`has_wasm_wrapper(target) && !base_type.directly_wasm_exposable()` (maps are never directly exposable;
exposable arrays are — that split is what keeps `passthrumap` pointing at the wrapper while `passthru`
stays a transparent `Vec`).

**Extending the grid.** Coverage equals the hand-curated type-shape axis (`SHAPES`); a representation not
in it is a silent hole, not a red cell. Periodically ask "which wasm representation are we *not*
enumerating?" and add a shape. Un-modelled but distinct (add when a consumer or regression appears): tagged
`#6.n(...)` types; map-representation structs (the struct roles currently use array-rep — bareword-keyed
map structs now generate, so adding map-rep struct shapes is unblocked).

**Behavioural upgrade — remaining state.** The wasm-side minted surface has landed: `--emit-tests
--wasm=true` emits a `cddl_generated_wasm_tests` module (`src/emit_tests_wasm.rs`) that constructs through
the wrapper API, round-trips, and reads accessors back against the minted literals, cross-checked against
an independent `cddl_lib::` rust build (the byte differential). It runs per-cell via
`integration_tests::wasm_matrix_roundtrips` (manual, `#[ignore]`d — check.ts full tier) and, in `verify.ts`,
via a **default-on** `--wasm` probe (opt out with `--no-wasm` / `VERIFY_WASM=0`) that `cargo test`s the
generated wasm crate and threads `minted_wasm` / `wasm_roundtrips` into the per-feature and per-cell
evidence. Remaining:
- **Unminted wasm shapes** — wrapper-collection ctor args build via a block-expr `new`/`add`/`insert`
  and `@newtype`/tag/table/array wrapper ctor args via their `From<cddl_lib::Native>` impl, so only
  extern / raw-bytes ctor args (user-supplied types with no generated conversion), flatten points, and
  the `--wasm-*-macro` modes remain **loud skips** (`eprintln!` — the list in `tests/README.md`
  § "wasm-crate test module"); a cell built entirely from those mints no wasm surface and falls back
  to the compile verdict.
- **Fidelity gaps** — the wasm read-side flatten losses (optional-nullable field, double-nested enum
  variant) remain generator-side, tracked by the `#[ignore]`'d fidelity tests; a presence-accessor fix,
  not a test-surface change, closes them.

**Oracles (`verify.ts` is manual-only):** ruby `cddl` via `gem install --user-install cddl` (verify.ts
auto-resolves it at `Gem.user_dir/bin/cddl`), rust `cddl` via `cargo install cddl` (point `RUST_CDDL` at
`~/.cargo/bin/cddl`), and cddl-codegen builds from this repo. The compile-gate reuses
`integration_tests::feature_corpus_compiles`' pattern (shared `CARGO_TARGET_DIR`, one-time dep warm-up).

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
