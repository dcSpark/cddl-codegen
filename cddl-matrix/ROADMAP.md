# CDDL master matrix — roadmap (remaining work + findings)

Read `README.md` first (the model + current state). This doc is the forward-looking list **and** the
hard-won findings — the two things a future agent can't re-derive from the code. The blow-by-blow of *how*
the done work landed lives in git history (this doc was pruned of it; the project already did the same with
the scale report + cold-critique).

**Status: gate-green.** 92 features (81 RFC8610 + 1 RFC9682 + 10 `CDDL_CODEGEN` vendor profile),
80 containment cells, and 206 cddl-codegen annotations, all axes reconciled/deterministic, with
execution-gated support **per-feature, per-cell (role × feature), and per-control-op** (all 37 IANA ops
probed): "supported" requires the generated crate's `--emit-tests`
round-trip/reject tests to PASS (`cargo test`), falling back to the compile verdict only for shapes that
mint no test surface (recorded honestly in the evidence). The orthogonal **emission axis is filled**
(every default-supported row carries a `preserve`/`json` verdict; 3 divergences, all `preserve`-side —
see § findings) and supported rows carry decode-foreign corroboration clauses (0 failures; plus 10
`class="constraint"` enforcement reject vectors over 9 enforce-green rows — the enforcement axis is
FULLY green: every row
with a rejectable constraint projects `enforce = yes (bounded-reject)`, pinned by
`query_q4_directional.ts --check`).
Three projections GENERATE their hand docs and drift-check: `golden_hex` (encoding axis, Q3), the
`corpus` projection (feature axis Q2 + per-cell **role × feature** coverage), and `query_q1_gaps.ts`
(the `## Limitations` section of `docs/docs/current_capacities.mdx`, Q1). **Every consumer query
Q1–Q6 is answered by a standing script** (`QUERIES.md` § "Definition of done"). The north-star —
subsuming `tests/corpus/COVERAGE.md` — is **DONE** (two independent cold reviews: "clear win").

> **North star.** Do the hard per-construct coverage work **once** and *project* it into the **many** docs
> that need it, instead of hand-maintaining (and re-sweating) each. Corpus was the hard flagship;
> `docs/docs/current_capacities.mdx`'s **Limitations** section now projects Q1's support gaps too
> (`query_q1_gaps.ts`); more docs follow. The matrix is "good enough" when regenerating a real hand doc
> from it is a **clear win**.

> **Findings ledger (F#)** from the cold critique (full write-up in git history): F1, F2, F4–F7 **done**;
> **F3 done** (support is execution-gated — round-trip + reject tests run per probe; per-direction
> reference vectors exist for the decode direction via the decode-conformance harness, encode via the
> conformance oracles; the 5-way Q4 answer is projected by `query_q4_directional.ts` — see `README.md`
> § "Directional support evidence"; the enforcement axis carries `class="constraint"` reject
> vectors so EVERY supported enforcement row projects `enforce = yes (bounded-reject)` — the numeric
> range/eq ops via `int`-targeted probe examples that sidestep the rust oracle's uint-target gap,
> which remains an upstream report — see § findings); **F8–F11
> out of scope** (bottom). Only still-open findings are sections below.

## 1. Remaining work — projections & queries

The matrix exists to feed **many** consumers; corpus was just the hard flagship. What's left:

- **Full role × feature coverage grid.** The corpus projection keys coverage on `(role × feature)` only for
  the cells where support *differs by role* (`prelude.null`, the literal values). A full grid for *every*
  construct is unbuilt — the floor data (`corpus_detect.ts` `rolesIn`, via `examples/ast_roles.rs`) already
  supports it; wire it into `project_corpus.ts` if a consumer wants the complete matrix view.
- **Directive × attachment-position cells for the `dsl.*` features (fourth proven instance of the
  same under-enumeration).** Each comment-DSL feature (`dsl.name`, `dsl.newtype`, `dsl.doc`,
  `dsl.custom_serialize/deserialize/json`) is verdicted from ONE example in ONE attachment
  position, but the mechanism is per-position code (`group_entry_to_field_name` has a separate
  metadata read per MemberKey arm), so a directive can silently no-op in an unenumerated position —
  proven twice for `@name` alone: silently dropped on arrow-keyed entries (fixed when integer arrow
  keys landed) and again on bareword-keyed entries (fixed with the Rust-keyword rejection, whose
  `@name` remedy only works because of that fix — a remedy we would have printed while it was
  broken). Enumerate directive × position (rule, bareword key, value key, arrow key, group-choice
  arm, array member) cells where the docs claim the directive applies, asserting an OBSERVABLE
  effect in the output (a renamed field, a doc comment) or a clean error — silently-ignored is the
  failure mode being swept for. Distinct from the REJECTED doc-snippet prose harness
  (`tests/TESTING_ROADMAP.md` § "Explicitly not worth it"): that decision was about docs drifting
  from behavior; this is behavior enumeration — the snapshot corpus pins positions it samples, and
  both `@name` drops were in positions it did not.

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
- `bun run query_q4_directional.ts` — **answers `QUERIES.md` Q4**: projects the per-direction F3 evidence
  into the 5-way `{accept, encode, decode, round-trip, enforce-constraint}` table, grouped by axis
  (a positional id-substring arg filters to the "for construct C" form). Pure `matrix.json` +
  `tests/decode_conformance/catalog.toml` reads — no cargo/oracles, like `project_decode_conformance.ts`.
  `--check` runs the consistency invariants + vacuity floor as a gate (check.ts `local` tier).
- `bun run query_q1_gaps.ts` — **answers `QUERIES.md` Q1**: the in-profile support gaps grouped by axis
  (feature / control-op / contextual containment-cell), plus the inverse supported summary and the
  out-of-profile rows bucketed separately (the F1 profile split). Pure `matrix.json` read — no
  cargo/oracles. `--write` regenerates the marker-delimited `## Limitations` section of
  `docs/docs/current_capacities.mdx`; `--check` runs the drift + consistency + vacuity gate (check.ts
  `local` tier).
- `bun run query_q5_completeness.ts` — **answers `QUERIES.md` Q5**: the standing, projectable form of
  `verify.ts`'s bidirectional reconciliation. Forward (source → feature) is HARD-authoritative for
  `type2`'s 12 alternatives only; the other productions' alt-coverage is best-effort/soft (labelled with
  the `normalizeAlt` caveat, § 4). Backward (feature → source), prelude completeness, and control-op
  completeness are all hard. Reads `matrix.json` + `sources/cddl-1-1-update.abnf` + `sources/cddl.prelude`
  — no cargo/oracles. `--check` hard-fails on any uncovered `type2` alternative / unresolved feature
  source / prelude gap, plus a vacuity floor (check.ts `local` tier).
- `bun run query_q6_diff.ts` — **answers `QUERIES.md` Q6**: no args prints the per-profile view (features
  each profile introduces + cddl-codegen's support split); two args `old.json new.json` (e.g. a
  `git show REF:cddl-matrix/matrix.json` snapshot) prints a structural diff — added/removed ids per axis
  array + changed annotation statuses. Pure `matrix.json` read — no cargo/oracles. `--check` (no-args
  mode) runs the profile-set consistency + vacuity gate (check.ts `local` tier).

## 3. F4 / F5 follow-ons (only when their consumer exists)

- **F5 (encoding precision):** the encoding axis is already major-type-dependent (no impossible cells). The
  golden_hex projection lists uncovered legal cells *globally*; the remaining work is per-*construct*
  legal-cell enumeration so Q3 can say "for construct C, these legal encodings are untested" — link
  `features[].encodings` to the leaf cells each construct can emit and intersect with golden coverage.
- **F4 (tag registry):** deliberately not pinned/enumerated (cddl-codegen is tag-parametric). Revisit only
  if a *tag-semantic* consumer of the master appears.

## 4. Expansion (when relevant)

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
  anonymous map/array/group nested in a choice / array-element / cbor-payload / generic-arg / map-value
  position panics (`parsing.rs` "Anonymous groups not allowed") — it must be **named** (a rule or
  `@name`). The one exception: **tag-content** accepts an inline composite. So `type2.map` is supported as
  tag-content, unsupported inline elsewhere, and works everywhere via a named reference — the per-(feature,
  role) verdict genuinely differs, which is the whole point. An inline parenthesized group carrying an
  occurrence marker (`[* (int, tstr)]`) is a distinct path: it is **rejected gracefully** (not a panic, and
  no longer a silent narrowing — see the findings section below), with the same "name the group" remedy.
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

**Oracle-coverage gap (upstream, NOT a cddl-codegen bug — the matrix no longer sits on it):**
- The rust `cddl` CLI oracle (0.10.x) does **not enforce control operators over a `uint` target**
  during `validate` — it accepts a boundary violation like `0x0b` (11) against `x = uint .le 10`.
  Scope: the numeric range/equality ops (`.le` / `.lt` / `.gt` / `.eq` / `.ne` / `.ge`) and ALSO
  `.size` / `.bits` over uint targets (found while fixing upstream — wider than the original repro
  matrix in `draft/rust-cddl-uint-control-op-gap.md`; our `ctl.size` constraint vector targets a
  bstr, so it is unaffected). The gap is **target-type-specific**: the identical controls over `int`
  ARE enforced. An upstream PR is submitted; until it merges, the sibling checkout's `local-fixes`
  branch (`~/Documents/git/cddl`, commit `cdba2b4`) carries the fix — rebuild and point `RUST_CDDL`
  at it to give future `verify.ts` runs an enforcing oracle (the matrix's evidence does not depend
  on it: the probes are `int`-targeted). Prune this entry and the draft note when the fix ships in
  a release. A second, distinct oracle disagreement surfaced by the decode-vector mint: for
  `a = [1*1 (int, tstr)]` (`contain.occurrence-target.grpent.inline_group.exactly_once_array`) ruby
  accepts the generated instances while the pinned rust oracle rejects them, so the row is honestly
  pinned vectorless in `tests/decode_conformance/catalog.toml` — un-pin and re-mint if a rust `cddl`
  release starts validating exactly-once inline-group occurrences. A `uint .size N` probe variant
  stays unswept (the one-example-per-op enumeration gap,
  § 4 variation rows). The six ops' probe examples (`control_examples.toml`) target `int` with
  literal, non-vacuous bounds (`x = int .le 10`, `.ge 5`, …) precisely so the decode-conformance
  catalog's both-oracles-reject gate can certify an in-type boundary-violating `class="constraint"`
  vector per row — over `int` both oracles reject each violation, cddl-codegen's generated decoder
  rejects it too (the emitted `RangeCheck`, executed by the replay gate), and all six rows project
  `enforce = yes (bounded-reject)` alongside `ctl.size` / `ctl.cbor` / `memberkey.cut`. The `int`
  targeting (and the non-vacuous `.ge 5` bound — `.ge 0` over a base type admitting no in-type
  violation would leave only a type-violation vector, which tests the base type, not the constraint)
  is load-bearing: `query_q4_directional.ts --check` pins the exact enforce-green set so a decay of
  the examples back to `uint`/vacuous forms fails the gate instead of silently shedding enforcement
  evidence.

**Bugs / gaps surfaced as findings (candidate cddl-codegen fixes — the matrix's actual payoff):**
- Top-level fixed-value / null **types** panic (`should not expose Fixed type in member`), though fixed
  values serialize fine as struct members. A singleton-value type is a reasonable feature.
- Mixed struct+table maps (`{ a: uint, * k => v }`) unsupported — a map is detected as EITHER a struct or a
  homogenous table, never both. Inline anonymous nested composites need a name.
- Zero-permitting occurrences (`*` / `0*n` / `*n`) on a keyed struct-map field are **rejected gracefully**
  (pinned by `contain.occurrence-target.memberkey.bareword.zero_map` and
  `contain.occurrence-target.memberkey.bareword.zero_bounded_map` in `tests/matrix_reject/`) rather than
  silently narrowed to a mandatory field — the narrowing generated decoders that reject valid CBOR
  omitting the entry, invisible to round-trip tests. `+` / `n*m` with a lower bound ≥ 1 still generate a
  mandatory field: under unique map keys they collapse to exactly-one, so mandatory is the honored
  semantics. Real support for `*` (an `Option<T>` field, like `?`) is a candidate feature; surfaced while
  testing the single-field struct-map fix.
- An occurrence marker on an inline (parenthesized) group — `a = [* (int, tstr)]` — is **rejected
  gracefully** (pinned by `contain.occurrence-target.grpent.inline_group.{plus_array,optional_array,bounded_array,zero_map}`
  in `tests/matrix_reject/`) rather than silently narrowed to an exactly-once record. The narrowing
  generated decoders that reject valid CBOR carrying any other repetition count (the spec-valid empty
  array `[]`, 2+ repetitions), invisible to round-trip tests. Boundaries: on the ARRAY side any
  non-exactly-once marker rejects (`*` / `+` / `?` / `2*5` all admit a count the flattened record can't
  represent), while `[1*1 (…)]` still generates — exactly-once IS the semantics, so flattening the group
  away is sound. On the MAP side the f18d764 collapse boundary is kept: a zero-permitting marker
  (`{ * (k: int) }`, which the inline-group wrapper hid from the keyed-field fix) rejects, but `+` /
  `n*m` with a lower bound ≥ 1 still generate a mandatory field (under unique map keys they collapse to
  exactly-one). The parenthesized table `{ * (int => tstr) }` still generates a `BTreeMap` — the flatten
  preserves it and table detection fires on the inner `k => v`. The rejection message recommends naming
  the group, and that named form now works end to end for `pair = (int, tstr)`, `a = [* pair]`. Real
  `Vec<Synthesized>` / `Option`-style support for zero-permitting markers is a candidate feature;
  flipping the row to `ok` must not decay back to silent narrowing. Unsupported rows carry no
  decode-conformance row; `project_decode_conformance.ts` enforces that boundary.
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
- **Emitted `serialize`/`deserialize` fn generics are collision-proofed against the defined-ident
  set.** A rule named `r`/`w` camel-cases to a type `R`/`W` that would shadow the hardcoded
  `fn deserialize<R: BufRead + Seek>` / `fn serialize<'se, W: Write>` parameters, producing
  non-compiling crates (E0574 struct-shape deserialize; E0599 enum-shape `R::U64`/`W::U64` variant
  paths). To avoid this, generation computes the deterministic set of camel-cased idents the crate
  defines (`IntermediateTypes::defined_rust_idents`, a `BTreeSet`) once at the start of `generate()`
  and picks each generic name as the first non-colliding candidate from a fixed sequence
  (`W`, `WSer`, `WSer0`… / `R`, `RDe`, `RDe0`…, via `pick_generic_name`), threading the choice
  through `make_{serialization,deserialization}_function` — the two sites that declare the generics;
  the bodies reference `Serializer<_>`/`Deserializer<_>` and the user type by name, so nothing else
  changes. The static-runtime trait DECLARATIONS keep `R`/`W` (Rust matches impl generics by
  position, not name). Default output is byte-identical — the bare `W`/`R` win whenever no collision
  exists, so the snapshot corpus does not churn. Pinned by
  `generic_names_are_collision_proofed_against_rw_idents` (fast tier, string-level) and the
  now-green `r`/`w` bundles of `identifier_hazard_crates_compile` (`EXPECTED_COMPILE_FAIL` is empty).
  The sweep's shape axis (struct AND type-choice enum for the rule-name position) stays — it exists
  because a struct-only pass had laundered enum-shaped `w` as clean.
- **Reserved-name rule/group definitions are rejected gracefully, not via panic.** A rule or plain
  group whose camel-cased name is a reserved Rust std/prelude type (`option`→`Option`, `box`→`Box`,
  `fn`→`Fn`, `self`/`Self`→`Self`) or a CDDL keyword (`true`/`false`, prelude type names) is caught by
  a pre-scan of user-chosen rule/group names in `api::with_types`
  (`intermediate::reserved_ident_rejection`, mirroring the two `assert!` guards in `RustIdent::new`)
  and surfaced through the normal `record_rejection` channel with a message that cites the rule and
  explains the remedy (rename the identifier: unlike a struct field a `; @name` comment renames, a
  rule/group name IS the emitted type name). The pre-scan runs and aborts BEFORE any
  `rule_ident`/`RustIdent::new` call, because a reserved name can also be *referenced* by another
  rule — any such reference reaches the same assert before `finalize` could drain the rejection. The
  asserts in `RustIdent::new` stay as a backstop for synthesized/internal idents (which never route
  through the pre-scan). `int` is excluded identically to `RustIdent::new`: it names the project's own
  pre-registered extern struct, so `group-name int` still aborts on the plain-group representation
  assert (`assert_eq!(found_rep, Some(rep))`) — a distinct collision, not a reserved-name reject.
  Pinned by `reserved_rule_name_rejects_gracefully_not_panics` and the `PANIC → error (graceful)`
  cells of `identifier_hazard_robustness_catalog`.
- Nint/float fixed map keys on a struct-map record are **rejected gracefully** (pinned by
  `contain.map-key.memberkey.value.{nint,float}_colon_*` in `tests/matrix_reject/`) rather than
  panicking generation — only uint
  and text fixed keys are implemented (the map-key write path and, under `--preserve-encodings`,
  `key_encoding_field`), so `neg = { -1: uint }` used to abort with `unsupported map key type for
  Neg.key__1: Nint(-1)`. The key is now classified at parsing BEFORE field naming, which also
  converts two adjacent panics into the same graceful rejection: an arrow key mixed into a record
  map (`{ -1 => uint, 1: uint }`) and a non-fixed key mixed in (`{ uint => tstr, 1: uint }`), both of
  which formerly panicked at `group_entry_to_field_name` (parsing.rs:1278) before any key check could
  run. Classifying first also makes the `--preserve-encodings` panic site (`key_encoding_field`'s
  `unimplemented!`) unreachable for rejected specs. Boundaries kept: uint/text keys (`{ 1: uint }`,
  `{ "a": uint }`) and the printed remedy — a table `{ * nint => v }` in its own rule — keep
  generating. The group-choice-arm rejection is pinned by
  `contain.group-choice-arm.memberkey.value.{nint,float}_map` and matches the record path. Real nint/float
  key support stays the candidate feature; flipping either row to `ok` requires real support, not a decay
  back to a panic.
- A single-entry fixed-value ARROW key `{ k => v }` **routes to the record path**, byte-identical to
  the colon spelling `{ k: v }` (RFC 8610: a literal-key `k => v` is the same wire entry as `k: v`).
  Table detection now requires a NON-fixed key type — resolved through aliases, so an aliased literal
  domain (`one = 1; { one => uint }`) diverts too — so the `ConceptualRustType::Fixed` table domain
  that panicked `for_rust_member` ("should not expose Fixed type in member", intermediate.rs ~1876)
  for every key kind is gone. On the record path each key kind gets f49d862's classification: uint and
  text generate (byte-converging with the colon spelling), while nint/float/bool and aliased-literal
  domains reject gracefully. The `{ "a" => uint }` and multi-field mixed forms additionally needed a
  `Type2::TextValue` field-naming case (converging on the same field name as `{ "a": uint }`). Pinned
  by `contain.map-key.memberkey.type1.{uint,text}_arrow_*` in `tests/matrix_supported/` and the
  `fixed_key_arrow_single_entry_routes_to_record_path` unit test (byte-exact convergence + the graceful
  rejections). A decay back to table-detecting fixed domains would re-expose the panic.
- A bareword map/array key that is a Rust keyword (`kw = { if: uint }`, `{ true: uint }`, `[if: uint]`,
  and `If` which snake_cases to `if`) is **rejected at parse time** — the emitted (snake_cased) field
  identifier is checked against the Rust keyword list in `parse_record_from_group_choice`, citing the
  rule and the offending field and pointing at the `@name` remedy (a `; @name <other>` directive that
  renames the field while leaving the CBOR wire key as the bareword text). Making the remedy work
  required fixing `@name` being silently dropped on bareword-keyed entries (the Bareword arm of
  `group_entry_to_field_name` ignored comment metadata, unlike the Value/Type1 arms). Formerly the
  keyword field emitted invalid Rust caught only by the rustfmt gate as a "generator bug" error.
  Pinned by `tests/robustness/map_bareword_keyword_key.cddl` (`error (graceful)` — the row was already
  a non-panic Err via the rustfmt gate; the fixture pins the error stays graceful) and the
  `bareword_keyword_field_name_rejects_gracefully` unit test (the parse-time rejection and the
  generating remedy).
- **Real nint support is ONE cross-cutting candidate feature — its per-shape gaps are cells of the
  § 1 enumeration items, not separate tasks.** Nint intersects every containment role (fixed map
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
  `emission.preserve = unsupported`), alongside `prelude.number` / `prelude.time` — the only three
  emission divergences the fill run found, all `preserve`-side.
- **All-negative range as a record field panics the generator.** `rec2 = [q: -10..-3]` → `internal
  error: entered unreachable code` at `bounds_check_if_block`'s `(None,None)` arm. Mechanism (probed):
  an int-typed field's deserializer branches per CBOR sign (uint arm / nint arm) and each arm gets its
  own partitioned bounds check — the straddling `[q: -10..3]` generates correctly with TWO RangeChecks,
  and all-positive `[q: 3..10]` with one — but an all-negative range leaves the uint arm's partition
  with an empty window, which reaches `bounds_check_if_block` as the unrepresentable `(None,None)`
  instead of an always-reject. The fix seam is the sign-branch bounds partition in generation, not
  parsing (the straddle case proves the range plumbing upstream is fine).
- **A top-level two-sided range alias silently drops its bounds — ANY sign, not just negative.**
  `c = -10..-3` emits `pub type C = i64;` and equally `e = 3..10` emits `pub type E = u64;` — a bare
  alias with no range check anywhere, silently accepting out-of-range values (worse than the field
  panic: spec-invalid data round-trips as valid). Bounds ARE enforced on the `@newtype` wrapper path
  (the nint magnitude/bound fixes) and on the field path (previous entry aside), so the gap is
  specifically the plain top-level alias emission; candidate fix: auto-wrap bounded aliases like the
  tagged-inner `.default`/range cases already auto-wrap, or reject with a `@newtype` remedy. Both
  entries are one enumeration hole: the range features are verdicted from one example in one position
  and one sign — position (top-level alias / field) × bound-sign (negative / straddling / positive)
  are unswept axes of the same § 1 under-enumeration class.
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
**Pin the rust oracle to an immutable copy for a long run**: `verify.ts`'s default `RUST_CDDL` path is
a sibling checkout's `target/debug/cddl`, which is an ACTIVE development tree (it carries `local-fixes`
for upstream bugs) — a rebuild mid-probe-loop would mint mixed-oracle evidence, so `cp` the binary
somewhere immutable and point `RUST_CDDL` there before a multi-probe run.

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
