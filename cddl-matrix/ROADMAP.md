# CDDL master matrix — roadmap (remaining work + findings)

Read `README.md` first (the model + current state). This doc is the forward-looking list **and** the
hard-won findings — the two things a future agent can't re-derive from the code. The blow-by-blow of *how*
the done work landed lives in git history (this doc was pruned of it; the project already did the same with
the scale report + cold-critique).

**Status: gate-green.** 92 features (81 RFC8610 + 1 RFC9682 + 10 `CDDL_CODEGEN` vendor profile), all axes
reconciled/deterministic, with compile-gated execution-grounded support **per-feature, per-cell
(role × feature), and per-control-op** (all 37 IANA ops probed). Both flagship projections GENERATE their
hand docs and drift-check: `golden_hex` (encoding axis, Q3) and the `corpus` projection (feature axis Q2 +
per-cell **role × feature** coverage). The north-star — subsuming `tests/corpus/COVERAGE.md` — is **DONE**
(two independent cold reviews: "clear win").

> **North star.** Do the hard per-construct coverage work **once** and *project* it into the **many** docs
> that need it, instead of hand-maintaining (and re-sweating) each. Corpus was the hard flagship;
> `docs/docs/current_capacities.mdx` is next; more follow. The matrix is "good enough" when regenerating a
> real hand doc from it is a **clear win**.

> **Findings ledger (F#)** from the cold critique (full write-up in git history): F1, F2, F4–F7 **done**;
> **F3 partial** (support is compile-gated; the 5-way directional split is deferred — §3); **F8–F11 out of
> scope** (bottom). Only still-open findings are sections below.

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
  they need only thin query scripts. **Q4** (directional support) is blocked on F3 execution (§3).
- **Full role × feature coverage grid.** The corpus projection keys coverage on `(role × feature)` only for
  the cells where support *differs by role* (`prelude.null`, the literal values). A full grid for *every*
  construct is unbuilt — the floor data (`corpus_detect.ts` `rolesIn`, via `examples/ast_roles.rs`) already
  supports it; wire it into `project_corpus.ts` if a consumer wants the complete matrix view.

## 2. Wire the gate into CI

All tooling is **Bun/TypeScript, run manually** from `cddl-matrix/`; `lib.ts` holds the shared loaders +
the stable-JSON serializer. Oracle paths are env-overridable (`RUST_CDDL`, `RUBY_CDDL`; `CODEGEN_DIR`
derives from the repo root).

**Full verification suite (run all to confirm consistency).** The fast, pure-`matrix.json` drift checks
are **wired into CI** (`.github/workflows/build.yml` `matrix-drift` job runs `build_matrix.ts --check`,
`project_robustness.ts --check`, `project_wasm_matrix.ts --check`, and `project_corpus.ts`); only the
heavy `verify.ts` oracle probe is still run manually (see Remaining).
- `bun run build_matrix.ts --check` — snapshot/drift gate: `matrix.json` matches the authored overlay. **(CI)**
- `bun run project_wasm_matrix.ts --check` — drift gate for the wasm-ABI matrix fixtures
  (`tests/matrix_wasm/*.cddl`); pure `SHAPES`/`ROLES` projection, no cargo/oracles. **(CI)**
- `bun run verify.ts` — reconcile (bidirectional grammar/prelude/vendor lints) + probe **per-feature,
  per-cell, AND per-control-op** support, **compile-gated** (generate + `cargo check`); rewrites
  `annotations/cddl_codegen.toml`. Needs the three oracles (ruby `cddl`, rust `cddl` CLI, `cddl-codegen`) —
  installable: `gem install --user-install cddl`, `cargo install cddl` (set `RUST_CDDL`), cddl-codegen
  builds from this repo. Slow (probes ~156 examples × generate+compile via `cargo`).
- `bun run project_corpus.ts` — **generates `tests/corpus/COVERAGE.md`** + the overlay validator gate
  (canonical-fixture drift + note↔support agreement + `code_anchor` exists in `src/` + floor completeness +
  per-cell role coverage drift + cell-support check H). Builds/runs `examples/ast_roles.rs` (the role
  floor), so it needs the cargo toolchain like `verify.ts`. **(CI)**
- `bun run project_golden_hex.ts` — golden_hex (encoding-axis) projection + drift-check.
- `bun run project_robustness.ts` — projects the support verdict into the robustness-catalog fixtures
  (`tests/matrix_{supported,panic}/*.cddl`); `--check` is the drift gate. Pure
  `matrix.json` read (no cargo/oracles), so it's a fast CI gate. **(CI)**
- `bun run corpus_detect.ts` — runs the `featuresIn` + role-aware (`rolesIn`) self-checks and prints the
  text-scan + role-aware floor diagnostics. The role floor builds/runs `examples/ast_roles.rs` (needs cargo).

Remaining:
- **Wire `verify.ts` into CI** (the fast drift/snapshot gates already run — see above). `verify.ts` needs
  the three oracle tools present; have the CI lane provide them or skip that probe gracefully when absent.
- **Typecheck enforcement.** The scripts are strict-typed but nothing runs `tsc`. Add a `tsconfig.json` +
  `@types/bun` (dev-only) and a `tsc --noEmit` step beside the gate (needs an ambient `declare module
  "*.toml"` for the `project_golden_hex.ts` import). The one place a (dev) dependency is worth it; the
  runtime stays dependency-free.

## 3. F3 — directional / enforcement support (partial; full split deferred)

Support is no longer one bit: the probe is **compile-gated** — generate the crate AND `cargo check` it, so
"supported" means *accept + compiles* (this caught `x = any`). `QUERIES.md` Q4 still wants the full 5-way
split **{accept, encode, decode, round-trip, enforce-constraint}**, which needs per-feature *execution* of
the compiled code (round-trip + constraint checks), not just compilation — machinery still deferred; Q4 is
blocked until it lands.

**Not a corpus blocker.** The corpus projection consumes the directional ⚠️ distinction
(parsed-but-not-honored: cuts, sockets, float-under-`preserve`) via **hand-asserted overlay notes**, not
execution. F3 is the *upgrade path*: it would make those ⚠️ verdicts execution-grounded and unblock Q4.

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
    `occur.optional`→`g = [? name: tstr]` (single-field **arrays** dodge the single-field-**map** bug),
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
- **Containment cell-example hygiene.** Enabling per-cell support exposed single-field-map cell examples that
  confounded the panic reason. The `type2.map`-in-a-role cells (`array-element` / `cbor-payload` /
  `choice-member` / `generic-arg` / `occurrence-target`) are 2-field, so they panic for the real
  **anonymous-group** reason, not the single-field-map bug. No single-field struct-map cell example remains.
- **Compile-gate (F3 partial) — the support probe requires the generated crate to COMPILE, not just
  cddl-codegen exit 0.** This caught a false positive the exit-code probe had laundered: `x = any` exits 0
  but emits `pub type X = Any;` (undefined type → won't compile), so `prelude.any` is correctly ➖ (root
  cause: `any` absent from `is_identifier_reserved`). 4 user-code features stay `supported` via a documented
  `COMPILE_GATE_EXEMPT` allowlist (they reference user-supplied code, so can't compile standalone —
  `ext.extern`, `ext.raw_bytes`, `dsl.custom_serialize`, `dsl.custom_deserialize`; integration-tested).
- **Role floor (role × feature coverage) — NOT a serde JSON-AST dump.** The `cddl` AST's `Serialize` is gated on
  `target_arch = "wasm32"`, so there's no free serde dump on a native build; `examples/ast_roles.rs`
  hand-walks via the crate's `Visitor` trait instead. Role-detection only needs the crate to PARSE (soft
  caveat) — which it does for the whole corpus by construction. `project_corpus.ts` verifies a role-keyed
  `[[cover]]` against the AST role floor AND the per-cell support verdict (check H), so it can't claim ✅ on
  an unsupported cell.

**Bugs / gaps surfaced as findings (candidate cddl-codegen fixes — the matrix's actual payoff):**
- Top-level fixed-value / null **types** panic (`should not expose Fixed type in member`), though fixed
  values serialize fine as struct members. A singleton-value type is a reasonable feature.
- Single-field **struct** maps panic: `{ a: uint }` → `unsupported table map key`. Mixed struct+table maps
  (`{ a: uint, * k => v }`) unsupported. Inline anonymous nested composites need a name.
- `bool` in a type choice → `E0282` (`bool / tstr`, `uint / bool` fail; `int / tstr` compiles).
- A single-letter rule named `r` collides with the deserializer's reader generic `R` → `E0574`.
- Bare `x = int` / an `int` `.cbor` payload emit an undefined `Int` wrapper (`cannot find type Int`); `int`
  works as a member / array element.
- `float16` / float-choice aliases unsupported (no native Rust f16) while `float32/64` work; floats fail
  under `--preserve-encodings`; generics on plain groups rejected.
- **Exclusive range upper bound is mis-computed** — `a...b` excludes `b` (max valid = b-1), but cddl-codegen
  emits `max = b+1`: `[v: 0...10]` generates `max: Some(11)`, accepting 10 and 11. One-char fix:
  `parsing.rs` `range_end + 1` → `range_end - 1`. (Surfaced by the corpus gap-fill; `exclusive_range.cddl`
  snapshot pins it; `rangeop.exclusive` is ⚠️.)
- **Occurrence-count constraints aren't enforced** on homogeneous arrays — `[+ uint]` (≥1) and `[2*5 uint]`
  (2..5) both emit a bare `Vec<u64>` with no length check (bare `*` is faithfully a `Vec`, so only `+`/`n*m`
  drop a constraint). Surfaced by `occurrence.cddl`.
- **Inline-group splice drops members** — `[(uint, tstr)]` generates a 1-field `InlineGroup { index_0: u64 }`
  (`read_elems(1)`), silently losing `tstr` (inline-group entries aren't flattened into the record). Surfaced
  by `inline_group.cddl`; `grpent.inline_group` is ⚠️.
- **Two-sided negative range as a record field panics the generator.** `rec2 = [q: -10..-3]` → `internal
  error: entered unreachable code` at `bounds_check_if_block`'s `(None,None)` arm — the negative range's
  bounds don't reach the field bounds check as `(Some,Some)`.
- **Top-level two-sided negative range silently drops its bounds.** `c = -10..-3` emits `pub type C = i64;`
  with no range check at all (bounds lost).

## wasm-ABI matrix — remaining work (`project_wasm_matrix.ts`)

The system itself (what it is, the axes, how to run/extend it) is documented in `tests/README.md` §
"wasm-ABI matrix". This section is the remaining backlog: the open red cells skip-listed in
`integration_tests::wasm_matrix_compiles`, and the durable fix that would clear the largest group. Each
open cell is a TDD target — take it off `SKIP`, fix the emitter, green. In priority order:

- **`passthrumap` — passthrough alias to a map typedef (`E0425`) — wants the predicate unification.**
  `mp = { * uint => text }; ptm = mp` fails as `array-element`, `map-value`, `map-key`, `struct-field`,
  `struct-field-opt` (only `newtype-inner` compiles): the wasm alias emits `pub type Ptm = MapU64ToText`,
  but the named map's wasm wrapper is `Mp` (from the Table rule ident) — `MapU64ToText`
  (`name_for_wasm_map`) is only generated for *inline* anonymous maps, which the alias path never hits.
  Ruled-out dead-end: making the alias base_type a `Rust(Mp)` reference (`parsing.rs` `None` branch) greens
  array-element/map-value but breaks the rest — a named map has a *dual representation* (a transparent
  `pub type Mp = BTreeMap` **and** a wrapper struct), and `Rust(Mp)` isn't interchangeable with an inlined
  `Map` in the newtype/struct/key serializers (regresses `collmap__newtype-inner` → `E0599`). A single
  shared base_type can't serve both crates (arrays only work because they're transparent on *both*).
  - **Durable fix — the `has_wasm_wrapper(ident)` predicate unification.** The wrapper-vs-transparent fact
    is a *struct-table* property, not a `ConceptualRustType` shape (a named collection `nums = [* uint]` is
    a wrapper; a passthrough `arr2 = arr` is a transparent `pub type` — same IR shape). Today naming,
    boundary conversion, and exposability each decide it separately, and their disagreements have been the
    recurring source of wasm boundary bugs. Route them all through one `has_wasm_wrapper(ident)` source of
    truth; that clears `passthrumap` and forecloses the class. (Narrower alternative: a wasm-only
    alias-emission path resolving a `Map` base_type to its named Table wrapper.)
  - **Where the name is lost (the exact site).** `ptm = mp` resolves `mp` via `new_type` to
    `Alias(mp, Map)`, but `parsing.rs`'s plain-typename rule branch (the `None` control-op arm) then
    *strips* the alias (`concrete_type.conceptual_type = *ty`) so `ptm`'s stored `base_type` is a bare
    `Map` — the link to `mp` is gone before the emitters run. Rust is fine (`for_rust_member(Map)` →
    `BTreeMap<..>`, transparent, correct); wasm is broken (`for_wasm_member(Map)` → the inline-only
    `MapU64ToText`). Narrow-fix shape (additive, base_type untouched so serialization is unaffected):
    at the strip site capture the target ident when `has_wasm_wrapper(target)`, store it on `AliasInfo`,
    and have the wasm alias emission prefer it (`pub type Ptm = Mp`). Must verify **all** passthrumap
    roles + both profiles — the `Rust(Mp)` dead-end greened only array-element/map-value, so per-role
    checking is mandatory (the wasm *usage* of `ptm` in each role must still resolve).

**Extending the grid.** Coverage equals the hand-curated type-shape axis (`SHAPES`); a representation not
in it is a silent hole, not a red cell. Periodically ask "which wasm representation are we *not*
enumerating?" and add a shape. Un-modelled but distinct (add when a consumer or regression appears): tagged
`#6.n(...)` types; map-representation structs — the latter blocked on the bareword-member-key generation
panic that forces the struct roles to use array-rep, so fixing that panic is a prerequisite.

**Behavioural upgrade.** The gate's verdict is *compile*, so a cell can be green while emitting a
semantically wrong same-type conversion. Upgrade the verdict compile → round-trip once the property
harness lands (`tests/TESTING_ROADMAP.md` item 1).

**Oracles (so `verify.ts` runs outside CI):** ruby `cddl` via `gem install --user-install cddl` (verify.ts
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
