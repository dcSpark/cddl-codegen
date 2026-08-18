# CDDL master matrix (canonical feature space)

A machine-readable enumeration of the CDDL feature space, anchored to the spec's own published artifacts.
It is the single source of truth that other matrices *project from*: `tests/corpus/COVERAGE.md` is the
projection onto the **feature** axis; `tests/golden_hex/COVERAGE.md` is the projection onto the
**encoding** axis. Per-tool support (cddl-codegen's, or any other consumer's) lives in `annotations/`,
keyed by id — so the master itself stays reusable.

The master is a **union of named profiles**, each feature tagged with the one that introduces it: mostly
public spec (`RFC8610`/`RFC9682`), plus one **vendor profile** — `CDDL_CODEGEN`, cddl-codegen's own
comment-DSL (`@name`/`@doc`/…) and sentinel typenames, modelled "as if its own RFC" so it rides the same
pipeline with no special-casing. The `profile` tag is what keeps the master reusable: a *non*-cddl-codegen
consumer filters `CDDL_CODEGEN` out as out-of-profile, exactly as it filters a newer RFC. Vendor features
resolve to a pinned in-repo source (`src/comment_ast.rs` + `docs/docs/comment_dsl.mdx`) via the same
bidirectional lint as spec features — so "not pure RFC" does not mean "unanchored."

> **Entry points (in order):** *this README* (the model + current state, incl. the gotchas and
> upstream-oracle-gap state) → [`roadmap.toml`](roadmap.toml)
> (what's left: remaining work + the open-findings ledger) → [`QUERIES.md`](QUERIES.md) (the
> consumer-query contract). The matrix is **fully scaled and gate-green**: <!-- status-header counts are generated — regenerate with: cd cddl-matrix && bun run project_status_headers.ts --write --><!-- gen:sh:readme-counts -->123 features and 150 containment cells<!-- /gen:sh:readme-counts -->
> across all axes (incl. the `CDDL_CODEGEN` vendor profile), with <!-- gen:sh:readme-annotations -->307 cddl-codegen support annotations<!-- /gen:sh:readme-annotations -->,
> **execution-gated** support **per-feature, per-cell (role × feature), AND per-control-op**
> (<!-- gen:sh:readme-ops -->all 37 IANA ops probed<!-- /gen:sh:readme-ops -->) — "supported" means the
> generated crate's emitted round-trip tests *pass* (`--emit-tests` + `cargo test`) wherever the
> shape mints a standalone or synthetic embedded test surface; the explicitly-labelled fallback for
> shapes that mint neither is a passing compile verdict. A second, orthogonal **emission axis** re-probes every default-`supported`
> row under each non-default codegen profile (`preserve`, `json`) and records a per-profile verdict
> (`emission.<name>.status`) — the axis is **filled**: a row with no `emission` keys is one whose
> default verdict is not `supported`, hence unsupported under every profile (a derived fact, since
> only default-`supported` rows are probed). These verdicts are load-bearing, not just reporting:
> the Rust gate `all_supported_constructs_generate_all_profiles` derives its expected per-profile
> generation failures from `emission.<profile>.status = "unsupported"` (no second hand list) —
> except the flavor whose evidence records "generates but does not compile" (a compile-level
> divergence the generation-only gate cannot observe; the gate skips the expectation so a
> regression to refusal still lands in its non-expected failures), and
> `project_corpus.ts` renders them as per-row profile caveats in `tests/corpus/COVERAGE.md`. **Four projections *generate* their hand docs:**
> `golden_hex` (encoding axis), the corpus feature-axis projection — `project_corpus.ts` generates
> `tests/corpus/COVERAGE.md` (the original north-star target, now subsumed; `corpus_detect.ts` +
> `annotations/corpus/`) — `query_q1_gaps.ts`, which generates the `## Limitations` section of
> `docs/docs/current_capacities.mdx`, and `project_status_headers.ts`, which drift-checks the countable
> status-header prose (feature/annotation/op/divergence/constraint counts) as spans in this README
> and `tests/README.md` (the matrix roadmap's counts render as generated slots when its projection
> is written). A fifth projection feeds a consumer that is not a hand doc:
> `project_recombination.ts` distils every feature `example` + the containment legality data into
> `tests/recomb/ingredients.json` (drift-gated by check.ts `project_recombination_check`), the
> ingredient set the shape-recombination fuzzer composes from (`tests/README.md`
> § "Shape-recombination fuzzer"). **Every consumer query Q1–Q6 is answered by a standing script**
> (`QUERIES.md` § "Definition of done").

## What is a feature?

A **feature** is a **serialization-relevant semantic distinction** — a construct that a code generator
has to make a distinct decision about (a distinct code path, a distinct wire-encoding, a distinct
constraint to enforce). It is **NOT merely a grammar production**: some productions are too coarse for
codegen (one `type2` alternative covers the entire tag registry; `ctlop = "." id` collapses every
control operator into one rule), and some are too fine to mean anything for serialization (the lexical
productions `S`/`WS`/`CRLF`). We therefore individuate **semantically**, then *check* that individuation
against the formal artifacts — rather than mechanically transcribing one row per production.

Because of that, the ABNF grammar is used as a **bidirectional completeness lint, not the spine.** The
real feature space is the union of the first-party sources:

> **grammar ∪ prelude ∪ control-op registry.**

`verify.ts` reconciles the authored features against that union in **both directions** (see "provably
comprehensive" below): forward (every grammar alternative is covered by some feature) and backward
(every feature resolves to some real source). Neither direction alone is sufficient — forward-only
admits invented features; backward-only admits coverage holes.

**Why the CBOR tag registry is *not* a fourth source (a decided scope boundary).** A generic codegen
would need to individuate tags (tag 0/2/24 are different tasks). cddl-codegen, however, is **tag-parametric**:
`#6.n(T)` emits the same wrap/unwrap code for any `n` (it does not implement per-tag *semantics* like
datetime or bignum decoding), so the tag *number* is a parameter of one feature (`type2.tag`), not a
distinct construct. The few tags that *are* codegen-distinct here (because the prelude assigns them a
named type + inner shape — `tdate=#6.0`, `time=#6.1`, `biguint=#6.2`, `bignint=#6.3`,
`encoded-cbor=#6.24`, `uri=#6.32`, `b64url`, `b64legacy`, …) are **already prelude features**. So the
tag axis is fully modeled by `type2.tag` (parametric) + those prelude entries. The full IANA
[CBOR Tags registry](https://www.iana.org/assignments/cbor-tags/) is the external reference for the
whole tag space; we deliberately **do not pin or enumerate it** — it is large and churning, and (unlike
the closed control-op set we derive features from) nothing in the matrix is derived from it.

Each feature also carries a **`profile`** — the RFC that introduced its **capability**, not merely the
grammar line. A pure grammar-line refactor keeps the older profile (RFC 9682 split `#7` into its own
production, but `#7.n` was always expressible via RFC 8610's generic `#DIGIT`, so `type2.major7` is
`RFC8610`); tag `RFC9682` only for a genuinely new capability (the type-valued tag head-number,
inexpressible in 8610). This makes *out-of-profile* distinguishable from *unsupported*: a feature a tool
rejects only because it post-dates the tool's target profile is not a true support gap.

## The model — three layers

**1. Native sources (`sources/`), pinned verbatim — never transcribed.** The authoritative upstream
artifacts, kept byte-identical so they stay diffable / re-syncable against upstream:
- `cddl.abnf` + `cddl-1-1-update.abnf` — grammar (RFC 8610 App. B / RFC 9682, the errata-corrected form)
- `cddl.prelude` — standard prelude (RFC 8610 App. D)
- `cddl-control-operators.csv` — IANA registry (the authoritative, cross-RFC control-op axis)
- `appendix_a.json` — CBOR encoding examples (cbor/test-vectors)

**2. Authored overlay (TOML) — the parts that exist in no upstream artifact:**

| file | what | axis |
|------|------|------|
| `features/*.toml` | one row per **serialization-relevant construct** (+ `profile` + stable ids); ABNF is a lint, not the spine — see "What is a feature?". A row whose directive is legal ONLY in an extern-deps scope (e.g. `@rust_name`) carries `example_extern_stub` alongside `example`: `verify.ts` synthesizes a directory input (the example as `lib.cddl`, the stub under `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/extern_dep/`) so generation-exit is honestly probed for a directive no single-file example can legally use | constructs + variations (A/B) |
| `roles.toml` | nesting container-contexts (array-element, map-key, tag-content, …) | nesting (C) |
| `containment/*.toml` | `role × feature → spec-allowed?` — **where nesting/variation gaps live**; each cell carries an `example` that `verify.ts` also probes for **per-cell tool support** | nesting (C) |
| `encodings.toml` | CBOR major-type × form grid (RFC 8949); **legality is major-type-dependent** (e.g. ints have no indefinite form), not a free orthogonal axis. Two layers, related by authored data: a PARENT row (`enc.major<N>`, the id `features[].encodings` links to) names its leaf cells in `cells`; a LEAF row declares none. That relation is what makes the legality answer **per-construct** — expanding a construct's declared refs through it is how `tests/golden_hex/COVERAGE.md` § "Per-construct legal encodings" says *for construct C, these legal encodings are untested* rather than only listing uncovered cells grid-wide | encoding (D) |
| `control_examples.toml` | minimal probe `example` per IANA control op (the CSV is byte-pinned, so examples live here); joined onto the control-op axis by `lib.ts` and probed by `verify.ts` for **per-control-op support** | control-op |
| `annotations/<tool>.toml` | per-consumer support, keyed by master id (NOT part of the spec master). A `[[support]]` row keyed by a **feature** id is the top-level verdict; keyed by a **containment** id it is the **per-cell (role × feature)** verdict; keyed by a **`ctl.<name>`** id it is the **per-control-op** verdict — so the master records "supported *here*, not *there*" (e.g. `type2.map` supported as tag-content, unsupported inline as a choice/array member). **Support is execution-gated**: `verify.ts` generates with `--emit-tests=true` and requires the crate's IR-minted round-trip/reject tests to *pass* (`cargo test`), not just that cddl-codegen exits 0 — catching non-compiling emissions (the historical canonical catch: `x = any` → `pub type X = Any;`, a type defined nowhere — that row is execution-verified `supported` since first-class `any` landed 2026-07-23) AND compiles-but-miscompiled round-trips. Types that mint no STANDALONE test surface (transparent aliases, bounded/newtype-able aliases, named tables/arrays, pure c-enums) are re-probed wrapped in a synthetic record holder (`__probe_holder = [0, <rule>]`) so their embed-site wire path runs — the only wire path these shapes have — and the evidence reads "round-trips when embedded" (per-probe `embedded` bit). If the synthetic can't generate (a generic rule needing type args) or can't round-trip, the probe keeps the compile verdict; the embed only ever UPGRADES evidence, never downgrades a verdict. **Emission axis:** a default-`supported` row is ALSO re-probed under each PROBED non-default codegen profile (`preserve`, `json` — the flag sets from `src/tests/mod.rs`'s `ALL_PROFILES`; the `component` row is not probed, because this pipeline's verdict is a rust-crate round-trip and `--component` leaves every rust byte identical) via the identical rust-only pipeline (no ruby/rust/wasm re-run), recorded as dotted `emission.<name>.status`/`emission.<name>.evidence` keys. Absence of `emission` keys means the row's default verdict is not `supported`, so it is unsupported under every profile — a *derived* fact, never hand-authored; only a passing `verify.ts` run fills the axis. **Wasm probe:** a **default-on** `--wasm` probe (opt out with `--no-wasm` / `VERIFY_WASM=0`) additionally `cargo test`s the generated wasm crate — the emitted `cddl_generated_wasm_tests` module constructs through the wrapper API, round-trips, and cross-checks against an independent `cddl_lib::` rust build (see `tests/README.md` § "wasm-crate test module") — and threads `wasm_gen` / `minted_wasm` / `wasm_roundtrips` into the per-feature and per-cell evidence. One field per STAGE of the leg, and the rendered clause names the stage it observed (`wasm generation REFUSED/PANICKED (generator exit N)` vs `wasm crate failed to compile`/`wasm round-trip FAILED (cargo test exit N)`), so a wasm surface gracefully declining a shape at generation is not read as a build regression — the reason the generation clauses deliberately never contain the words `cargo test`, which is what misled a wasm-support review when one field carried both exits. An assert-at-startup self-test (the ruby-Bernoulli classifier's sibling) checks the taxonomy before any oracle work, gated by `verify_selftest` on every `local` tier run: a wrong stage name is a plausible-looking annotation, so it must fail loud on the cheap tier rather than wait for the full-tier `verify` gate to notice. **Decode-foreign clause:** a supported row's evidence also records whether the generated decoder ACCEPTS the committed spec-derived vectors from `tests/decode_conformance/catalog.toml` (instances our code did not produce — `; accepts N foreign spec-derived vector(s)`; see `tests/README.md` § "Decode-direction conformance"). Corroboration only, default-on (`--no-decode-foreign` opts out), never changes a verdict. **Component-execution clause:** a BOUNDED selection of feature rows (`COMPONENT_PROBE_ROWS` in `verify.ts`) additionally has its `--component` face EXECUTED: the row's catalog `spec` is regenerated with `--component=true --wasm=false`, the emitted `component/` package is built for `wasm32-wasip2`, and the catalog's own vectors are driven through the real component under wasmtime by a generic host (`component-probe-host/`, dynamic `Val` API — one binary drives every row, no `bindgen!` per world). The copied scratch host runs its own `cargo test` before its executable is built, so synthetic controls pin the defensive dynamic-result protocol arms the generated components cannot reach. Each accept vector must decode AND re-encode to the bytes the row's OWN rust crate produces — a live oracle computed per run, never a committed hex, because this leg's charter is cross-face agreement and the encoding gates own the bytes themselves — and each reject vector must come back as `Err`, never a trap, with the instance still usable afterwards (a re-check vector runs after the rejects and is what observes it). Stage fields `component_gen` / `minted_component` / `component_roundtrips` render one stage-named clause, same discipline as the wasm leg's, and the same opt-out (`--no-component` / `VERIFY_COMPONENT=0` leaves every field undefined and the file byte-identical). Corroboration only: it never changes a verdict. The selection is bounded because each row pays a wasip2 build plus a native oracle build plus a host run where the wasm leg pays one `cargo test`, and compile BREADTH is already owned by `component_corpus_compiles` (every corpus fixture) and `component_build_sweep` (every drivable catalog row — `tests/README.md` § "component build sweep over the decode catalog"); rows are chosen one per translation class the projection has to get right (validates / despecializes / boundary / control, each named at its entry) and growing the set is one edit to that const, guarded by a startup self-test that refuses a row the committed catalog cannot drive. **The set grows when a component-face defect turns up in a construct class the table does not name** — the classes are listed at the const, so a consumer reporting a defect against the emitted component face maps onto one or exposes a gap; that is the dimension the deferred cost lives on (unrepresented classes), not the number of rows or of consumers. | — |

**3. Generated view — `matrix.json`** (produced by `build_matrix.ts`) joins the overlay with the native
sources into one universal artifact for downstream/cross-language consumption. Imported axes are
*derived*, not authored: control operators come straight from the IANA CSV with ids `ctl.<name>` (with
their probe `example` joined from `control_examples.toml`). It is
regenerated, never hand-edited. `build_matrix.ts` also runs the **drift-check** (every annotation id must
resolve to a real master id). Run it with `bun run build_matrix.ts`; `lib.ts` holds the shared loaders
+ the byte-exact JSON serializer.

> **Editing this README or the roadmap?** This README remains hand-authored. The matrix roadmap IS
> `roadmap.toml` — the authored TOML is the only committed form; format it with `bun run
> project_roadmaps.ts --format-source cddl-matrix/roadmap.toml` and validate with `bun run
> project_roadmaps.ts --roadmap all --check` (the projections render in memory). A human-review
> markdown render can be generated with `bun run project_roadmaps.ts --roadmap matrix --write`;
> it lands in the gitignored `draft/roadmaps/` directory and must never be committed. The testing
> roadmap follows the same source contract documented in `tests/README.md`. These documents are linted: the sibling script
> `lint_doc_citations.ts` (check.ts `fast` tier; not matrix tooling — it lives here for the shared
> tsc coverage, as does `no_std_check.ts`, the no_std drift gate, which additionally shares
> `lib.ts`'s gate-cache helpers; see `tests/README.md` § "The no_std drift gate") asserts every
> "pinned by/tracked by/gated by `name`" citation in the hand docs still
> resolves in the tree, bans positional "…item `<N>`" citations, bans numbered section headings in
> the hand docs (a numbered heading invites `§ <N>` citations, which silently retarget on
> renumbering), rejects references to the deleted roadmap projections outside their explicit
> compatibility seams, and requires a blank line before headings. Alongside it, `lint_tracked_text.ts`
> (also fast-tier and dependency-free) reads only tracked paths, strictly rejects invalid UTF-8/control
> bytes in authored text, and rejects doubled indentation-leading rustdoc markers in tracked snapshots.

> **Selection state is not tracked in the repository.** Which roadmap entries are picked up, by whom,
> and in what order is plan-internal state, and it lives in the gitignored `draft/` directory — the
> containment boundary that keeps ephemeral work-packet vocabulary out of committed files. No
> committed file records it and no gate reads it, so nothing under `draft/` may be cited as durable
> evidence. `cddl-matrix/roadmap.toml` records only durable facts about the work itself: its state,
> its consequence, and the observable that would change it.

Why this shape: the (construct × variation × nesting × encoding) cross-product is intractable and mostly
meaningless. Normalizing keeps each fact in one place — e.g. "tags are supported but **nested** tags are
not" is not a bespoke row; it's `containment(tag-content, type2.tag)` with a cddl-codegen annotation of
`unsupported`. The combinatorial concern collapses into one relation.

## Why it's *provably* comprehensive (not just "we tried hard")

Comprehensiveness is established by a **bidirectional lint** against the first-party sources (the ABNF
is a lint, not the closed-world spine — see "What is a feature?"):
- **forward** (source → feature): every enumerated ABNF alternative has ≥1 covering feature row, an
  explicit delegation reason, or a named "modelled under" feature row — completeness checkable against
  the ABNF in `sources/` and hard-gated in `verify.ts`/`query_q5_completeness.ts`;
- **backward** (feature → source): every feature's `production` resolves to a real ABNF production, the
  `prelude` pseudo-production, or the IANA control-op registry — so no feature is invented with no
  source (hard-gated in `verify.ts`);
- cross-checked against a parser's AST variants (the `cddl` crate's `Type2`/`Type1`/`MemberKey`/… enums
  mirror the productions — a second, weaker corroboration, since the AST is one reading of the same grammar);
- **control operators** are derived from the **IANA registry** (authoritative & cross-RFC), so that axis is
  complete by construction and stays current as new RFCs register operators.

## Directional support evidence (Q4)

On top of the execution-gated per-row verdict (see the `annotations/` row above),
`query_q4_directional.ts` projects each row into `QUERIES.md` Q4's 5-way answer **{accept, encode,
decode, round-trip, enforce-constraint}** from pure file reads (`matrix.json` +
`tests/decode_conformance/catalog.toml` — no cargo/oracles); `--check` is its consistency + vacuity
gate (`check.ts` local tier).

**The encode/decode asymmetry is durable and load-bearing.** **Decode** has INDEPENDENT per-construct
evidence: the catalog's `expect="accept"` foreign vectors are spec-derived CBOR our own encoder never
produced (executed by the `decode_conformance_replay` gate; see `tests/README.md` § "Decode-direction
conformance") — exactly the direction a round-trip conflates away. **Encode** has NO independent
per-construct oracle — its independent evidence is corpus-level only (`golden_hex`,
`ir_conformance_corpus`) — so the query reports encode strictly as the round-trip half
(`encode = yes iff round-trip = yes`) and never claims a stronger per-construct encode fact.
(The suite-side sibling catalog `tests/decode_conformance/corpus_catalog.toml` — the
composition-depth leg over the corpus fixtures, same tests/README section — deliberately feeds
NEITHER Q4 nor the annotations: its rows are (fixture, rule) obligations, not matrix rows, so
matrix-scoped counts and evidence clauses never include it.)

**The enforcement axis (`enforce-constraint`) is grounded by `class="constraint"` reject vectors** in
`catalog.toml`: spec-INVALID CBOR whose ONLY invalidity is the constraint the row enforces (an
over/under-`.size` string, a non-uint `.cbor` payload, a cut-violating map value, an out-of-window or
excluded-endpoint number, NaN against a float window — each a valid instance of its base type),
certified spec-invalid at mint and durably rejected by the generated decoder — for the pinned
REASON: each vector's `expect_err` substring is asserted against the decoder's error Display by the
replay gate, so the rejection names the violated constraint, not just any `Err`. The green set is
<!-- gen:sh:readme-enforce-green -->98 rows<!-- /gen:sh:readme-enforce-green -->: `ctl.size`, `ctl.cbor`, `memberkey.cut`, the six numeric range/eq ops (`ctl.{le,lt,gt,eq,ne,ge}`)
plus their boundary-value rows `ctl.ne.{zero,one}` (the `(1,-1)` / degenerate `(2,0)` NE encodings),
`ctl.size.uint` (65536 over the u16-collapsed window, rejected by the width-guarded member decode —
the guard that replaced the silent truncation this row's vector exposed; pinned by the
`signed_ints_width_rejects` / `width_collapse_rejects` execution fixtures),
the eight `rangeop` rows (`rangeop.{inclusive,exclusive}` plus their head-type × sign variation
rows `.int`/`.nint`/`.float`), the three occurrence-bound rows `occur.bounded{,.lower,.upper}`
(out-of-count arrays against the generated bounded collection conversion door — `BoundedVec` or
`BoundedOrderedSet` for reject sets), and the three fixed-value lexeme
rows `value.number.{hexfloat,hex,bin}` (wrong-value instances against the fixed 3.0 / 16 / 10,
rejected as FixedValueMismatch — hex/bin carry hand pins including `[0]`, the silent-zero
radix-conversion trap), the fixed-selector MEMBER-equality family
(`contain.{array-element,map-value,occurrence-target}.…` — literal `prelude.{true,false,null}` and
`type2.value`/`value.*` members, including nint and present-wrong optional forms, plus explicit
`type2.tag.fixed_{bool,null}` array/map members). Each preserves the outer container, tag where
present, key order, and siblings while changing only the fixed payload. The optional cells are
PRESENT-wrong — absence is a legal accept shape — and the `null` cells pin `ExpectedNull`; the other
fixed values pin `FixedValueMismatch`.

The remaining fixed selectors use different, equally enforceable rejection semantics: the
ARM-selection family covers group-choice arms (bareword/fixed map keys, fixed map/array members,
and the explicit tag-11 fixed-payload arm) plus `tstr / null` and the same-major `true / null /
tstr` choice. Their wrong selector leaves no legal alternative; `tstr / null` remains an
arm-selection constraint even though the Rust lowering uses `Option`. The REQUIRED-KEY lookup family
covers single/multi `memberkey.value` colon uint/text rows and their literal `memberkey.type1` arrow
counterparts. Its hand vectors omit only the required key: an indefinite map retains the valid
sibling where one exists, bypassing the definite-cardinality precheck so the decoder reaches
`MandatoryFieldMissing` rather than an unrelated unknown-key or length error. The
tag-11 arm vector is rejected by Ruby but accepted by the pinned rust oracle; its exact narrow rust
exemption and reversible RFC 8610 §3.6 argument are in
`upstream-reports/rust-cddl-tag-fixed-payload-acceptance.md`. Upstream rust-oracle
gaps shape what "certified" means per family
(`query_q4_directional.ts --check` pins the exact green set and the four-row fixed-byte unverified
set, as well as the empty over-acceptance set, so
a decay fails loudly rather than
silently dropping enforcement evidence): the numeric ops' probe examples target `int` with literal,
non-vacuous bounds (`x = int .le 10`, `.ge 5`, …) because the rust oracle does not enforce these ops
over a `uint` target, so `uint`-targeted or
vacuously-bounded forms cannot pass the both-oracles-reject gate; the non-uint-endpoint range
rows (`.int`/`.nint`/`.float`) carry rust corroboration only against the local-fixes oracle @
`885c61c` — the released 0.10.x CLI blanket-rejects EVERY instance of a float or negative-int
range, valid or not (gap #3 in § "Upstream oracle gaps" below; the fork fix is what let those six
rows' accept side mint and made their rejects discriminating); and `ctl.size.uint`'s reject
certification leans on RUBY plus the local-fixes oracle, which rejects the holder-wrapped violation
discriminatingly (the released 0.10.x CLI misvalidates any control-op-carrying rule referenced as
an array entry — gap #4 below; that fork fix is also what let the row's accept side mint).
`ctl.default` is `n/a` (it governs an absent field — no rejectable instance).

**What counts as enforcement-bearing is classified, not inferred from the green set.** A row with no
reject vector reads `unverified (no reject vector)` when it carries a constraint and `n/a (no
constraint)` when it does not, and the difference is what makes a missing vector visible instead of
looking like "nothing to enforce here". For CONTAINMENT cells Q4 classifies three semantic kinds by
the cell's own role × feature out of `matrix.json`: member equality for ordinary fixed-value member
features, arm selection for fixed group-/type-choice alternatives, and required-key lookup for
supported literal map keys. These role/feature families are automatic, so a new supported vectorless
selector drifts the exact unverified-set pin. The tagged `type2.tag` rows and literal-arrow map-key
rows are coarse-feature exceptions: their explicit inventories are stale-checked for their current
role and feature, rather than inferred from an id suffix or example text. That guard deliberately
catches moved/deleted known exceptions; it does not pretend to auto-discover a future unrelated
exception.

**A certified over-acceptance projects `enforce = no (over-accepts: M)`** — the fifth enforce value,
dominating `yes`/`unverified`/`n/a`. Its evidence is a `class="over-acceptance"` accept vector:
spec-INVALID CBOR (both oracles reject at mint, the same inverse gate as `class="constraint"`) the
generated decoder CURRENTLY (wrongly) ACCEPTS. At HEAD the set is **empty** — asserted exactly by
`query_q4_directional.ts --check`'s `EXPECTED_ENFORCE_OVERACCEPTS` pin (`[]`), the decay twin of the
green/unverified sets — but the vector class stays armed for the next certified instance. Why the
class exists at all, and the worked precedent that exercised both of its exits, are in § "Gotchas"
(the over-acceptance bullet) — an over-acceptance is by construction the class a round-trip cannot
see, so its rationale belongs next to that blindness rather than here.

Cut/socket *semantics* stay hand-asserted overlay notes in the corpus projection
(⚠️ parsed-but-not-honored): they are validation concerns a round-trip cannot observe.

## Upstream oracle gaps (rust `cddl` CLI)

None of these are cddl-codegen bugs, and the matrix no longer sits on any of them — but they shape
what "certified" means per vector family (§ Q4 above) and what the `upstream-closeouts` section in
`roadmap.toml` waits on. The sibling checkout's
`local-fixes` branch (`~/Documents/git/cddl`, commit
`ac1b98e` — also the `Cargo.toml` pinned rev, so the generated-crate conformance oracle AND
cddl-codegen's own parser share it) carries the fixes whose entries below read FIXED; the entries
that read OPEN remain open at that rev. In particular, the tag-typed and named-rule map-key gaps
keep affected decode-foreign rows vectorless or empty-instance-only, while the float-class and
typed-tag gaps need narrow one-oracle certification. `RUST_CDDL` defaults to that build, giving
`verify.ts` runs an enforcing oracle. Because every local branch reports version 0.10.6, a
version string cannot tell the pinned build apart from a wrong-branch rebuild, so the shared
behavioral fingerprint in `cddl-matrix/oracle_fingerprint.json` refuses wrong oracles: `verify.ts`
checks the `RUST_CDDL` binary at startup (const `ORACLE_FINGERPRINT`), and
`ir_conformance_corpus` checks the generated-crate `CDDL_ORACLE_DEP` crate before corpus work. The
file holds a handful of pinned probe inputs whose accept/reject exits are unique to the local-fixes
fixes; a gap close-out flips one probe file and both consumers refuse a stale or unexpectedly fixed
oracle consistently. A wrong-branch rebuild — or a stock `cargo install cddl` — therefore fails loudly
at startup (`HARNESS FAILURE`) in under a second instead of minting mixed evidence. The binary
fingerprint runs at STARTUP only, so **pin the oracle to an immutable copy for a long run**: a rebuild
MID-run still swaps the oracle silently (that default path is an ACTIVE development tree), which is
exactly what the `cp`-the-binary-somewhere-immutable-and-point-`RUST_CDDL`-there practice prevents.

1. **uint-target control-op under-enforcement** (released 0.10.x): `validate` does not enforce
   control operators over a `uint` target — it accepts a boundary violation like `0x0b` (11) against
   `x = uint .le 10`. Scope: the numeric range/eq ops (`.le`/`.lt`/`.gt`/`.eq`/`.ne`/`.ge`) AND
   `.size`/`.bits` over uint targets; the gap is target-type-specific — the identical controls over
   `int` ARE enforced. Upstream PR submitted.
   This is why the six ops' probe examples (`control_examples.toml`) target `int` with literal,
   non-vacuous bounds (§ Q4), and why `ctl.size.uint`'s rust corroboration leans on the local-fixes
   build.
2. **group-in-array sequence/occurrence misvalidation** (released 0.10.x): a multi-entry group in an
   array (parenthesized inline OR named reference) validated correctly ONLY as the sole entry with
   no occurrence indicator — inner entries were checked at group-local indices as if absolute array
   positions, and occurrence bounds were compared against total array item count instead of
   repetition count. Ruby accepts all the spec-valid instances. FIXED in `local-fixes` @ `773b723`;
   `contain.occurrence-target.grpent.inline_group.exactly_once_array` is re-minted with real accept
   vectors against that build. The named repeated-group cell
   (`contain.occurrence-target.grpent.groupname`) no longer has a cddl-codegen decode row: RFC 8610
   requires flat group concatenation there, while cddl-codegen cannot yet represent that occurrence
   without silently substituting nested arrays, so the generator now rejects the cell and the
   supported-catalog projection excludes it. The upstream validator fix remains real; it certifies
   the spec shape independently rather than making this generator support it.
3. **non-uint-endpoint range blanket rejection** (released 0.10.x — a 0.10.0 regression): every
   instance validated against a range whose endpoints are not uint is rejected, valid or invalid
   (`invalid cddl range. upper and lower values must be uint types`) — float ranges (`0.5..10.5`)
   AND negative / sign-spanning int ranges (`-10..10`, `-10..-3`); `0..10` validates correctly.
   `compile-cddl` accepts the spec; only `validate` blanket-rejects. FIXED in `local-fixes` @
   `885c61c` (integer windows compare as i128 across any uint/int endpoint mix; float windows match
   floats only, via a NaN-safe accept-form check); the six
   `rangeop.{inclusive,exclusive}.{int,nint,float}` variation rows that sat on it with NO accept
   vector are re-minted with real accept vectors against that build, and their `class="constraint"`
   rejects are now corroborated discriminatingly by rust instead of leaning on ruby alone.
4. **control-op-carrying rule as array entry misvalidated** (released 0.10.x): the WHOLE array is
   checked against the rule (`h = [x]`, `x = uint .size 2`, instance `[4786]` rejects; an
   uncontrolled `x = uint` in the same position validates fine). Subsumed by the same `773b723`
   array-sequence fix, so `ctl.size.uint` now carries real holder-wrapped accept vectors.
5. **radix integer literals mis-lexed** (released 0.10.x — a PARSER gap, unlike 1–4, so it hit
   cddl-codegen's own AST too, not just the validate oracle): the pest grammar had no `0x`/`0b`
   uint alternatives — `0x10` mis-matched the exponent-optional `hexfloat` rule and died as
   "Invalid hexfloat", while `0b1010` mis-lexed as `0` + identifier `b1010` (the unchecked CLI
   entry then silently validated the wrong two-entry shape). FIXED in `local-fixes` @ `2c7548e`
   (radix uint alternatives in every `uint_value` position — occurrence bounds, tag heads, member
   keys, ranges, control-op args — case-insensitive per RFC 8610 §3.1; `hexfloat` now requires the
   `p` exponent; adjacent strictness: leading-zero decimals like `042` are parse errors, `1e5`
   floats now parse). The `value.number.{hex,bin}` rows flipped `supported` on that pin (minted
   accept vectors + hand wrong-fixed-value constraint pins). **Ruby oracle caveat:**
   ruby corroborates radix literals in VALUE position only — its own radix handling in other
   positions (occurrence bounds, tag heads, simple values) is broken
   (`cddl-matrix/upstream-reports/ruby-cddl-radix-position-deviations.md`), so future radix-position rows can't lean on ruby.
6. **bignum-typed map keys rejected wholesale + bignum VALUE tags unchecked** (released 0.10.x): the
   member-key walk had no predicate for the prelude bignum types (`biguint`/`bignint`/`bigint`), so
   `validate` rejected EVERY map whose KEY domain is one of them ("expected object value of type
   bignint, got object") — empty or not, spec-valid entries or not, `.cbor`-wrapped or bare. The
   fix surfaced a second, opposite-polarity gap: the `Value::Tag` arm only inspected tags 0/1, so a
   bignum VALUE "validated" under ANY tag (`[bignint]` accepted `2(h'01')` and even `3(1)`). FIXED
   in `local-fixes` @ `c2ebf9f` (bignum keys match at all four member-key sites; bignum tags are
   enforced in both positions per the RFC 8610 prelude — `bignint` = tag 3 only, `biguint` = tag 2
   only, `bigint` either; a mismatched tag now fails discriminatingly with "map requires entry key
   of type bignint"; 30-case suite on the fork, 12/12 differential agreement vs the ruby gem). The
   fix re-armed the rust conformance-oracle half for `tests/corpus/cbor_bignint_table.cddl` —
   formerly the sole `RUST_ORACLE_SKIP` resident (its ruby half is separately unjudgeable — the
   gem's inline-composite controller parse gap — so it stays on `RUBY_EXPECTED_FAIL`). Two
   fingerprint probes (`bignint-key-empty-map-accepts`, `bignint-wrong-tag-rejects`) pin the fixed
   behavior, refusing a stale pre-fix (old-pin) oracle build.
7. **prelude `number` rejected a float** (released 0.10.x): `validate` rejected every CBOR float
   against the built-in `number` type (`expected type number, got Float(…)`), while ints against
   `number` — and floats against the equivalent inline `int / float` or a user alias — validated
   fine. Root cause: the validator lexes `number` to a reserved token rather than expanding the
   prelude text, and the float classifier omitted that token where its integer sibling included it
   (`number = int / float` accepts both). FIXED in `local-fixes` @ `90f66ff` (a `NumericKind`
   int/float/both classifier replaces the asymmetric per-type checks, in both the CBOR and JSON
   validators). The fix unblocked the one class the decode-conformance **arm-coverage floor**
   (§ "Decode-direction conformance" in `tests/README.md`) had ledgered in `DECODE_FLOOR_ARM_EXEMPT`
   (`lib.ts`): `prelude.number`'s float arm is re-minted with real f32/f64 accept vectors
   and the exemption ledger is empty again. Two fingerprint probes
   (`prelude-number-float-accepts`, `prelude-number-tstr-rejects`) pin the fixed behavior,
   refusing a stale pre-fix (old-pin) oracle build. A stale claim in the retired repro note is
   corrected for the record: bare floats against `time` (`#6.1(number)`) were ALREADY accepted
   before this fix (tag-leniency laxity, unchanged by the bump), not newly accepted by it.
8. **tag-typed map-key over-rejection** (OPEN at `ac1b98e`, NOT fork-fixed — re-confirmed still
   open at that rev by direct probe): `validate` evaluates a
   TAGGED Type1 member key against the WHOLE map instead of the entry keys — spec-valid
   `{24(5): "x"}` against `m = { * #6.24(uint) => tstr }` rejects with
   `expected tagged data #6.24(uint), got Map(...)`, while the untagged `{ * uint => tstr }`
   control validates fine (the member-key walk repositions onto entry keys for the key classes it
   special-cases, but not for `Type2::TaggedData`, so the tag type is visited with the map as the
   current value); ruby accepts. Spelling-independent (re-confirmed under the `*` respell during
   the no-occurrence-arrow close-out). This is what keeps `contain.map-key.type2.tag`
   `pinned_reason`-vectorless in the decode catalog — every ruby-generated candidate dies on the
   two-oracle gate — so decode-foreign corroboration for tagged map keys is structurally
   unavailable; the row's support verdict is unaffected (execution-gated green). No upstream
   issue filed yet. Differential repro + suspected `src/validator/cbor.rs` site + close-out steps:
   `cddl-matrix/upstream-reports/rust-cddl-tag-map-key.md` (local note).
9. **optional-entry empty-map over-rejection + closed-map check skipped when nothing consumed**
   (released 0.10.x): `validate` rejected the spec-VALID EMPTY map against a map whose sole entry
   carries a `?` occurrence (`m = { ? tstr => uint }`, instance `{}`) — `?` permits absence, so
   the empty map is a legal 0-entry instance — because the CBOR validator's shared
   `Occur::Optional | None` member-key arm demanded a matching entry unconditionally (the `0*1`
   spelling, `*` tables, and `?`-marked literal keys took other arms and behaved). Fixing it
   surfaced two interlocking gaps, shipped together in `local-fixes` @ `3d56d8e` so no instance
   moves from reject to accept: the closed-map rule was SKIPPED whenever no group member consumed
   any key (`validated_keys = None` meant "don't check" — `{ ? k: uint }` wrongly ACCEPTED
   `{"a": 1}`, the laxity the old local note recorded as an adjacent observation), and the JSON
   validator had no type-domain string-key matching at all for `?`/occurrence-less entries. Fixed
   behavior verified by a 21-CBOR/12-JSON differential grid vs the ruby gem plus fork regression
   tests. No matrix consequence beyond the ledger: cddl-codegen rejects the `{ ? tstr => uint }`
   spelling gracefully (the count-permitting table-marker boundary), so
  `contain.occurrence-target.memberkey.type1.optional_table` is now a supported bounded-table row;
  its 0..=1 decode vectors are minted and its over-maximum constraint vector is pinned. Two fingerprint probes
   (`optional-entry-empty-map-accepts`, `closed-map-unexpected-key-rejects`) pin the fixed
   behavior, refusing a stale pre-fix (old-pin) oracle build. Of the four adjacent map-matching
   gaps found during the fix, one (the float-key/null copy-paste) is since fork-fixed — gap #10
   below; the other three stay filed in the fork checkout's `future-issues/` (extension-member
   openness, consumed-key re-matching, mixed key domains).
10. **float-typed map keys matched `null` keys** (released 0.10.x): three find-based member-key
   sites in the CBOR validator guarded on the float classifier but searched the map with a NULL
   predicate copy-pasted from the null branch above — both verdicts inverted against
   `m = { float => uint }`: the spec-valid `{1.5: 1}` REJECTED ("map requires entry key of type
   float") while the spec-invalid `{null: 1}` ACCEPTED. The filter-based `* float => k` partition
   path was already correct; only the occurrence-less, `?`, and find-fallback paths were wrong.
   FIXED in `local-fixes` @ `707c038` (`matches!(k, Value::Null)` → `matches!(k, Value::Float(_))`
   at the three sites). No matrix row sat on this gap (it was a `future-issues/` filing from the
   gap #9 close-out, never a catalog blocker). Two fingerprint probes (`float-key-accepts`,
   `null-key-rejects`) pin the fixed behavior — the reject probe also refuses an always-accept
   stub — so a stale pre-fix (old-pin) oracle build is refused by both fingerprint consumers.
11. **named-rule / parenthesized-choice map keys over-rejected, plus a named-composite-array
   key-and-value facet** (OPEN at `ac1b98e`, NOT fork-fixed — found by corpus decode-conformance
   work): `validate` WRONGLY REJECTS
   every non-empty map instance whose KEY domain is a NAMED-RULE reference (`{ * fe => uint }` +
   `fe = 0 / 1 / 2` — and equally `fe = 0`, `fe = uint`, `fe = uint / nint`, `fe = tstr`) or a
   PARENTHESIZED CHOICE (`{ * (0 / 1 / 2) => uint }`), while ruby accepts; the same keys spelled
   INLINE (`{ * uint => uint }`, `{ * 0 => uint }`) validate fine, and the empty map validates for
   this scalar/choice-key facet (no key to mis-match). Error shape: `unexpected key Integer(Integer(0))` from the
   map-key arm — the same neighborhood as gap #8 (TAG-typed map keys), likely the same
   key-matching site not resolving typename refs/choices. **It is worse than non-resolution: the
   site binds the key to the WRONG RULE.** A `{* epoch => label}` member (`epoch = uint`,
   `label = text`) reports `key of type text required, got Integer(Integer(0))` — `text` is
   `label`'s type, i.e. the VALUE's rule answering for the KEY. The same instance validates when the
   map is reached through one more typename indirection (a synthetic
   `__cddl_oracle_root = <rule>` root makes it pass), so the rejection also depends on how the
   containing rule is entered — both worth stating in the upstream report. This is what strips the
   corpus decode
   rows `c_style_enum_map_key.enum_keyed_map`, `table_enum_key.enum_keyed` and
   `table_enum_key.enum_key_holder` down to their EMPTY-map instances (every non-empty ruby
   candidate dies `ruby=0 rust=1` on the two-oracle gate; a mint whose random draws miss the
   empty instance pins the row `pinned_reason`-vectorless instead, with the per-oracle tallies in
   the pin wording — the two states flip-flop across re-mints while the gap is open; MEASURED
   2026-07-30 on `alias_positions`, whose subject makes it maximally exposed: re-minting the fixture
   UNCHANGED demoted `alias_positions.nested` from live vectors to `pinned` for net -37 catalog
   lines, and which rows fall varies per run, so a re-mint of a named-key-heavy fixture is a LOSSY
   operation rather than an idempotent one while this gap is open). It also claims one MATRIX row
   outright: `contain.occurrence-target.memberkey.type1.open_struct_named_key` (`* k => any` with
   `k = uint / text`) minted `pinned_reason`-vectorless on its very first mint — every ruby candidate
   died `ruby=0 rust=1` — while its inline-key siblings `..._bytes_key` and `..._nint_key` minted
   real vectors from the identical rest-row shape (`..._nint_key` is the CLEAN half of that control:
   `..._bytes_key` lost its own zero-rest-entry candidates to the distinct string-key gap #13
   below). That contrast IS the gap stated as a controlled pair, and it costs the row only its
   decode-foreign corroboration: its support verdict is
   execution-gated green, exactly like gap #8's resident `contain.map-key.type2.tag` above. And it is
   why `alias_positions` — the corpus fixture whose whole subject is aliased (i.e. named-rule) map
   keys — rides `ir_conformance_corpus`'s `RUST_ORACLE_SKIP` with its ruby half left judging: its
   maps sit in MEMBER position, where the empty-map instance that spares the three rows above is not
   what the minter draws, so five of its six rules carry the signature. A TWO-ROW map sharpens the
   wrong-rule binding further: for `open_table = { * bstr => uint, * md => md }` (`md = uint / text`)
   the validator reports `map requires entry key of type uint` / `text` — the NAMED-RULE row's arm
   types — against a minted `h'00'` key that plainly matches the first row's inline `bstr`, so the
   presence of one named-rule row poisons keys belonging to a DIFFERENT, inline-typed row of the
   same map (`open_table` is the second `RUST_ORACLE_SKIP` resident on this evidence; ruby accepts
   all three rules' minted values). **Separate facet, caught by the scheduled Cycle-3 full-tier
   `ir_conformance_corpus` gate:** `{ * entry => entry }` with `entry = [n: uint]` rejects even the
   valid empty map in `holder = [m: self_map]` (`[{}]` = `81a0`) as `expected array type, got
   Map([])`; ruby accepts. It occurs when both key and value domains resolve to named composite
   arrays, whether the names are the same or distinct; scalar aliases, inline domains, and either
   one-named/one-inline array control validate. `preserve_pair_map_self_encoding` consequently has
   one narrow `RUST_ORACLE_RULE_SKIP` for `holder`, while standalone `entry`, ruby, dumps, and
   structural checks stay live (`self_map` is a transparent alias, hence no standalone rust call).
   This is the same validator neighborhood and possibly related, not a demonstrated common root.
   No upstream issue filed yet. Differential grid, two adjacent same-neighborhood observations
   (nested-map VALUES in a `*` table; multi-entry composite-array-key tables), and close-out steps:
   `cddl-matrix/upstream-reports/rust-cddl-named-key-map.md` (local note).
12. **float prelude names not distinguished at all** (OPEN at `ac1b98e`, NOT fork-fixed):
    `validate` collapses all six float prelude names into a single "is this a float" test — every
    major-7 float instance validates against `float16`, `float32`, `float64`, `float16-32`,
    `float32-64` and `float` alike (36 of 36 probes accept, NaN included), while a non-float in the
    same position is correctly rejected by all six. The names denote six DISJOINT value classes —
    the partition by shortest lossless form (RFC 8610 § 2.2.3: the `#7.x` notation "is about a set
    of values at the data model level"; § 3.3: "representable as") — so `1.1`, a value no narrower
    form represents, ought not to validate against `float16`. Unlike gaps #1–#11 this one has no
    half the matrix can lean on: it makes rust unable to certify ANY float-class violation, which
    is why all seven `class="constraint"` vectors on the five constrained float rows carry a
    `DECODE_REJECT_ORACLE_GAP_EXEMPT` entry (lib.ts) naming `rust`. The ruby gem — written by RFC
    8610's author — implements the same partition head-independently and REJECTS all seven, so the
    certification narrows to that oracle plus the written argument rather than vanishing; the
    divergence is written up for filing in `upstream-reports/rust-cddl-float-name-blindness.md`,
    whose closing section states what moves if the answer is "the validator is right". No upstream
    issue filed yet.

13. **string-typed rest-row keys in a MIXED map demand an entry — `*` read as `+`** (OPEN at
    `ac1b98e`, found by the cycle-13 containment-cell decode-foreign mint): in a map
    mixing DECLARED entries with a `*`-occurrence rest row, a `bytes`- or `tstr`-typed rest KEY
    domain makes `validate` require at least one rest entry — the spec-valid declared-entries-only
    instance (`{1: 692}` against `m = { 1: uint, * bytes => any }`) fails
    `map requires entry key of type bytes`, while the SAME instance passes with `uint`/`nint` rest
    domains, the rest-entry-bearing instance passes, and the pure table keeps its empty-map pass
    (ruby accepts throughout; RFC 8610 §3.2 makes `*` zero-or-more). Disjoint differential grid
    from gap #11 — no named rule, no choice, no tag in the trigger — though plausibly the same
    member-key matching site; full grid and upstream-report sketch in
    `cddl-matrix/upstream-reports/rust-cddl-string-key-mixed-map-rest-occurrence.md`. Catalog cost today: the matrix
    row `contain.occurrence-target.memberkey.type1.open_struct_bytes_key` minted only
    rest-entry-bearing accept vectors (its two zero-rest-entry candidates died `ruby=0 rust=1` on
    the two-oracle gate), and it inherits gap #11's lossy-re-mint hazard: a re-mint drawing only
    zero-rest-entry candidates would pin the row `pinned_reason`-vectorless. No upstream issue
    filed yet.

14. **typed tag number and wrapped fixed payload both accepted against the wrong declaration**
    (OPEN at the pinned oracle behavior): against
    `t = [ v: #6.11(true) // label: tstr ]`, rust accepts both `81cbf4`
    (`[#6.11(false)]`, wrong wrapped fixed value) and `81ccf5` (`[#6.12(true)]`, wrong tag number),
    while ruby rejects both; both validators accept the declared tag-11/true arm and the text arm.
    The catalog needs only the first vector to prove fixed-selector arm enforcement, and the
    generated decoder rejects it with `Expected fixed value true found false`, but rust cannot join
    its spec-invalidity consensus. The exact
    `contain.group-choice-arm.type2.tag.fixed_array/81cbf4` vector therefore carries a
    `DECODE_REJECT_ORACLE_GAP_EXEMPT` entry naming rust; `81ccf5` is an adjacent diagnostic, not a
    second catalog exemption. The reversible RFC 8610 §3.6 argument, four discriminating probes,
    complete commands, and careful binary-provenance statement are in the committed paste-ready
    `upstream-reports/rust-cddl-tag-fixed-payload-acceptance.md`. No upstream issue filed yet.

15. **incremental `/=` chains lose the plain `=` statement's arm when resolved through a
    REFERENCE** (OPEN at `ac1b98e`): a rule defined incrementally (`extended = uint` +
    `extended /= text`) validates every arm when it is the validation ROOT of a BASE-FIRST chain
    (first rule / CLI default), but a REFERENCE to it (`r = extended`) resolves to the `/=`
    alternates only — `00` (uint 0) is rejected with `expected type text, got
    Integer(Integer(0))` while `6178` (`"x"`) passes; both statement orders lose the `=` arm in
    the referenced form, and an extension-FIRST chain loses it even at root position.
    Base-first root-position validation and the ruby gem both accept the uint instance, so the
    bytes are spec-valid (RFC 8610 §3.9 arm accumulation). Surfaced 2026-08-08 by the first corpus fixture spelling
    `/=` (`tests/corpus/assignt_extend.cddl`): the generated conformance harness always
    references the tested rule through its synthetic `__cddl_oracle_root = <rule>` alias, so
    every incremental-chain fixture hits the gap; both incremental fixtures ride
    `RUST_ORACLE_SKIP` (`assignt_extend`, whose enforcing oracle stays the ruby gem, and
    `assignt_extend_ext_first`, whose ext-first spelling the gem separately crashes on — its
    conformance rides the equivalence pin instead; the split's reasoning lives with the
    fixtures and ledgers). Minimal repro, discriminating probes and commands in
    `upstream-reports/rust-cddl-incremental-extension-reference-resolution.md`. No upstream
    issue filed yet.

16. **`undefined` validates as null** (OPEN at pinned `ac1b98ec07184236517da4511b1bbea239e35190`):
    `validate_cbor_from_slice("x = undefined", &[0xf7], None)` rejects the valid CBOR undefined
    value with `expected type undefined, got Null`. cddl-codegen's `fixed_singletons` fixture still
    generates normally and retains its eight emitted rust conformance calls; only the exact
    `undefined_value` call is neutralized in the gate's scratch generated module. Its other seven
    rust calls, ordinary round trips, dumps, ruby sweep, and reference-codec differential remain
    enforced. The latter normalizes ciborium's f7-to-null value-model collapse against minicbor's
    explicit undefined only after both fully decode the bytes, pinned by its f7 self-check.
    `ir_conformance_corpus` directly pins this rejection and exact signature during its
    oracle preflight, so an acceptance or signature change fails rather than silently preserving the
    one-rule ledger. The corpus decode catalog's exact-vector
    `CORPUS_DECODE_ACCEPT_ORACLE_GAP_EXEMPT` entry retains only its `undefined_value`/`f7` accept
    while ruby accepts and rust keeps that exact failure; the mint stale-guards the exit/signature and
    the catalog gate stale-guards the resident vector. This is an oracle limitation, not a generated-code behavior: the fixture's own
    codec writes and verifies `f7` and rejects null/other special values.

17. **fixed byte-string values panic in the CBOR validator** (OPEN at `ac1b98e`): the pinned
    parser and compiler accept fixed-byte specifications such as `magic = h'CAFE'` and
    `a = [v: h'0102', x: uint]`, but `cddl --ci validate` exits 101 at
    `src/validator/cbor.rs:4840` for their spec-valid byte-string instances. Ruby cddl 0.12.14
    accepts the same instances. The four matrix rows (`value.bytes`,
    `contain.array-element.value.bytes`, `contain.map-value.value.bytes`, and
    `contain.choice-member.type2.value.bytes.fixed-kind`) consequently keep
    pinned, vectorless decode-foreign rows; this removes independent decode corroboration and leaves
    their fixed-equality enforcement `unverified` in Q4 until reject vectors can be independently
    certified. Generated execution separately establishes their supported status and exact
    wrong-value rejection, including the
    Rust/WASM/JSON/WIT/component projection coverage. A standalone full verify PASSed in 29m32s
    with 109 supported / 13 unsupported / 1 out-of-profile rows, 177 decode-foreign rows and 1513
    vectors with 0 failures, and 13/13 component probes; decode-foreign never controls status.
    The same revision has an independent AST-rendering defect: `Display` for `B16ByteString` at
    `src/ast/mod.rs:1314-1319` treats decoded binary bytes as UTF-8, so `Type::to_string()` fails or
    panics on `h'CAFE'`. Generator diagnostics therefore use a lazy byte-safe renderer instead of
    the upstream `Display` path. Re-check both defects when updating the pinned parser/oracle.
    Repro and exact commands:
    `cddl-matrix/upstream-reports/rust-cddl-fixed-bytes-validator.md` (local note).

18. **exact-zero fixed map members are read as exactly-one by rust and ignored by ruby** (OPEN at
    pinned `ac1b98e` / Ruby cddl 0.12.14): for `exact = { 0*0 t: uint }`, the spec-valid empty map
    `a0` passes Ruby but rust rejects it as `object missing key: "t"`; the spec-invalid map containing
    the forbidden key (`a1617400`, `{ "t": 0 }`) passes BOTH validators. RFC 8610 §3.2 defines
    `n*m` as a cardinality range, so `0*0` permits exactly zero occurrences. Consequently
    `contain.occurrence-target.memberkey.bareword.zero_exact_map` stays a vectorless pinned
    decode-foreign row: neither the absent accept direction nor the forbidden-present reject
    direction can satisfy the ordinary two-oracle certification rule. This does not weaken its
    supported verdict: direct generated-runtime tests execute absent CBOR/JSON round trips and
    structured forbidden-key rejection across closed/open, typed/`any`, `@ignore`, bounded,
    non-empty, duplicate-preserving, preserve, canonical, wasm, and component surfaces. The exact-
    vector accept-side framework is `DECODE_ACCEPT_ORACLE_GAP_EXEMPT` (separate matrix/corpus maps);
    it retains a vector only while every non-exempt oracle accepts and a named failing oracle still
    matches its exit/signature. The reject-side framework is `DECODE_REJECT_ORACLE_GAP_EXEMPT`.
    Neither is a row-wide exemption. Re-mint after either validator is fixed; the row's
    durable `pinned_reason` names both current failures and automatically disappears once one
    candidate passes both oracles.

## Gotchas (read before touching the support seam or probe examples)

The recurring rule: **a panic/compile-failure on minimal *valid* CDDL is a finding to surface
(ledger it in `roadmap.toml`'s `findings-open` section), not something to engineer away by making the probe green —
and the inverse, don't *invent* a gap from a degenerate example.**

- **Support seam — a probe verdict is only as good as its example.** The probe runs the cell's
  minimal `example`, and cddl-codegen only emits for *named composite types* — so a degenerate
  example can die on an *orthogonal* limitation and misattribute the gap, while editing an example
  to green a probe can equally hide a real gap. The discipline: examples are minimal and
  *feature-isolating* (`memberkey.bareword` → `m = [name: tstr]`, `occur.optional` →
  `g = [? name: tstr]` — single-field **arrays**, chosen when single-field **maps** still panicked;
  that bug is since fixed, so these could move to map forms on a future `verify.ts` refresh;
  `type2.map` → `foo = { * tstr => int }`, a table), and a genuine gap stays `unsupported` with a
  ➖ note in the corpus overlay, never relabelled to green the drift-check.
- **Anonymous-group limitation (pervasive contextual fact, surfaced by per-cell support).** An INLINE
  anonymous map/array/group nested in a choice / array-element / cbor-payload / generic-arg / map-value
  position is **rejected gracefully** unless it has a representable nominal. A named RULE works in
  every position; the `@name` naming route works where a heterogeneous anonymous ARRAY or
  fixed-field **MAP** record is
  the whole type of the group entry the comment sits on — the choice-member position and the member
  ones (array-element, map-value, occurrence-target), pinned by `dsl_position_tests` cells. TAG
  wrappers are transparent to it: `#6.42([ … ]) ; @name Foo` and `#6.42({ … }) ; @name Foo` mint the
  struct and wrap it in the tag, for any number of nested tags, because a tag mints no type of its
  own and the composite behind it is therefore the only referent the name can have — emitting exactly
  what the named-rule spelling emits. Source equality across rust/preserve/json/wasm/component is
  pinned by `anonymous_map_member_name_emits_the_named_rule_remedy_across_faces`; the compiled
  preserve and wasm fixture is `tests/comment-dsl`. It stays out of reach where the composite belongs
  to some other construct the comment cannot be attributed to (a `bytes .cbor [ … ]` payload, a
  generic argument, a map key, a choice arm), pinned by
  `tagged_anon_array_member_name_walks_only_operator_free_single_choice_tag_layers`; homogeneous
  inline tables remain structural maps rather than records, and two-row open tables remain
  named-rule-only because their container owner and keys list derive from that rule.
  Where the door does not reach, the `@name` written for it is now REFUSED rather than dropped: on a
  SINGLE-ENTRY group-choice arm the entry-trailing slot is honored exactly where the door consumes
  it and is a hard error otherwise, naming the arm's own slot (the one after the `//`), so an author
  whose name landed one slot over is told which slot works instead of getting silence
  (`robustness_tests::name_on_a_single_entry_arm_entry_slot_is_honored_or_refused_never_silent`).
  The refusal seam does not re-derive which member types the door covers — it observes the door's
  own effect (the member's parsed type IS the struct the name mints), so the two cannot drift.
  The one
  exception: **tag-content** accepts an inline composite. So `type2.map` is supported as
  tag-content, unsupported inline elsewhere, and works everywhere via a named reference — the
  per-(feature, role) verdict genuinely differs, which is the whole point. An inline parenthesized
  group carrying an occurrence marker (`[* (int, tstr)]`) is a distinct path: it is rejected
  gracefully (not a panic — `roadmap.toml`'s `findings-open` section). Naming the group alone does
  not change RFC 8610's flat repetition semantics; the supported nested-array remedy first gives it
  an array type (`pair = (int, tstr)`, `pair-item = [pair]`), then repeats that type
  (`a = [* pair-item]`).
- **Supported fixed values are nominal at the TOP level and inline at MEMBER position.** A named
  scalar/text/bytes/bool/null/undefined fixed rule (`x = true`, `x = undefined`, `x = 5`,
  `x = "v"`, `x = h'CAFE'`, or `x = 'raw'`) is a singleton type with a
  standalone codec; the same constants are also supported as array elements and map values:
  a fixed/null choice nominalizes the fixed arm, and only bare `null / null` is one-state — tagged
  or `bytes .cbor` null beside bare null remains two distinct wire arms.
  `a = [v: true, x: uint]`, `m = { k: 5, j: uint }`, the bare unkeyed `a = [5, x: uint]`, and the
  optional keyed forms `[x: uint, ? v: h'0102', label: tstr]` / `{ ? k: 'raw', j: uint }` all use
  the same zero-storage verification model. Fixed bytes exercise same-major and mixed text/bytes
  choices, `.default` over `bytes`, tags, `.cbor`, preserve replay, and canonical minimization.
  Their extern-interface spelling is canonical uppercase hex, and they project through Rust, wasm,
  JSON, WIT, and component surfaces. Fixed **map keys** are the remaining representation boundary:
  only uint and text literal keys
  generate; bytes keys are rejected gracefully. The parser separately accepts uppercase hex and
  raw UTF-8 but still rejects lowercase `h'cafe'` and `b64'…'`. `undefined` is a supported unit
  fixed value (`0xf7`) with no inner encoding sidecar; it participates in nominal fixed/null choices
  without collapsing into null. Cardinality is a
  third, independent splitter: an occurrence marker over an UNKEYED fixed value (`[* 5]`, `[? 5]`,
  `[+ 5]`, `{ * uint => 5 }`) is unsupported while the keyed optional forms above generate — which is
  why `type2.value` × `role.occurrence-target` renders ◐ in the grid.
- **The expected-conversion prelude names are fixed tagged-`AnyCbor` wrappers, not text codecs.**
  `eb64url`, `eb64legacy`, and `eb16` retain and require tags 21, 22, and 23 around one arbitrary
  CBOR item; their Rust, wasm, JSON, WIT, and component projections use the ordinary tagged-`any`
  surfaces and expose no base64/base16 rendering API. `cbor-any` is a separate stream marker and
  remains the permanent graceful refusal described below.
- **Containment cell-example hygiene.** The `type2.map`-in-a-role cells (`array-element` /
  `cbor-payload` / `choice-member` / `generic-arg` / `occurrence-target`) use 2-field map examples so
  any panic is attributable to the real **anonymous-group** reason (an inline map inside a role needs
  a name), with no single-field-map shape to confound it.
- **Some `unsupported` rows are PERMANENT by decision, not pending — don't re-litigate them.** Four
  graceful rejections are boundaries the matrix keeps deliberately, so a row flipping green would be
  a regression, not progress. (1) A bare `any` type-choice arm in a NON-LAST position
  (`a = any / tstr`): a bare `any` accepts every CBOR item, so any arm after it is unreachable dead
  code, and the rejection says so ("`any` arm makes later arms unreachable — move it last"); a
  LAST-position bare `any` arm IS supported (forced-backtracking dispatch), and a tagged `any` arm
  (`#6.n(any)`) is not a catch-all and is allowed in any position (pinned by
  `tests/robustness/choice_any_arm.cddl` and `tests/robustness/choice_last_any_arm.cddl`). (2) `.ne`
  over a float — the integer min>max exclusion hack has no principled float encoding. (3) A decimal
  bound on an integer-primitive head (`uint .le 10.5`) — silently flooring it onto the int head would
  mis-enforce. (4) `cbor-any` (`#6.55799(any)`) self-describes a complete CBOR stream rather than a
  value, so it remains a role-neutral graceful refusal (pinned by
  `cbor_any_prelude_tag_rejects_gracefully_in_every_position`). Items (2) and (3) route through
  `record_rejection` and are pinned alongside the float-window enforcement in the `tests/core`
  `float_bounds` fixtures. Everything else in this section's neighbourhood is a candidate fix, and
  lives in `roadmap.toml`'s `findings-open` section.
- **`@custom_json` is a CONTRACT, and its can't-compile-STANDALONE class is the correct record of
  it.** The directive suppresses the serde/schemars derives on the type it names precisely so the
  spec author owns its JSON form, while every JSON surface the tool emits over that type keeps
  being emitted — a derived container's own derives, the wasm wrapper's
  `to_json`/`to_json_value`/`from_json` door, the `--component` guest's `to-json`/`from-json`
  members, and (under `--json-schema-export`) json-gen's registration row. Each resolves to one of
  exactly three impls the author is on the hook for (`serde::Serialize`, `serde::Deserialize`,
  `schemars::JsonSchema`), so a build failure is always the named trait on the named type
  (`E0277`, loud and local at the first build), and supplying them builds the whole surface green:
  `tests/json` splices the hand-written impls of `tests/external_json_impls` into the generated
  crate and builds the rust, wasm and json-gen crates, standalone type and derived container
  alike (pinned by `custom_wrapper_in_derived_container`). Gating those surfaces on "does this
  type carry serde derives" would delete the door the directive exists to publish — a hand-owned
  JSON form the wasm and schema faces can still expose. So the `dsl.custom_json` row
  (`cj = uint ; @newtype @custom_json`) sitting on both `JSON_SURFACE_SKIP` and
  `WASM_SURFACE_SKIP` is the right record: those legs compile the row's crate with no hand-written
  impls anywhere, which is exactly the state the contract says cannot build. The promise is stated
  where the directive is documented (`docs/docs/comment_dsl.mdx` § "@custom_json"). The boundary
  would stop being a contract — and go back to `roadmap.toml`'s `findings-open` section as a defect — if a
  `@custom_json` type's emitted JSON surface ever demanded anything BEYOND those three impls: a
  bound no hand impl can discharge, or a reference only an edit to a generated file could satisfy.
- **A NON-STRING map key does not cross the `--json-serde-derives` json boundary — a kept
  boundary, not a pending fix.** JSON object keys must be strings; `serde_json` stringifies
  integer keys but hard-errors on a byte-string or composite (array/map) key
  (`serde_json::to_string` returns `Err`, "key must be a string"), so a map keyed by `bytes` or a
  composite type — spec-valid CBOR the decoder accepts — is not json-serializable through the
  generated derives. The same class reaches an open table's typed row and an open struct-map's
  rest row through their own hand-written JSON faces, where the posture is decided: text/uint/nint
  keys image as natural strings and bytes/composite keys STRICT-FAIL loudly at `to_json`
  (`OpenTableKeyImageError`, and the rest-row strict failure —
  `docs/docs/output_format.mdx` § "Open tables (a typed row plus a catch-all)" and § "Typed key
  domains in JSON"), refusing a lossy encoding on injectivity grounds. The affected replay rows
  sit on the two replay gates' `JSON_SURFACE_SKIP` ledgers citing this entry (which also
  suppresses their wasm `from_json` sub-leg, the same serde path), and the bare-`bstr` cells stay
  in the catalog as the pin for the un-remedied spelling's loud error. The remedy a real consumer
  takes is a key type with a string-producing `Serialize` (a `@newtype`/raw-bytes hex impl —
  `tests/open-table-json-e2e` executes exactly this). A generated hex/base64 key stringification
  was considered and not built: it is a lossy, non-obvious wire mapping, and the strict-fail
  posture above refuses lossy key encodings for the same injectivity reason — building it means
  overturning that rationale, or accepting that two map positions publish different key
  conventions, which is a consumer-driven decision to make before any code, not a fix this
  boundary is waiting on.
- **User-code rows are SEEDED, not exempted.** A row whose generated code names user-supplied items
  (an extern / raw-bytes type, a `@custom_serialize`/`@custom_deserialize` pair) gets that code
  written for it — `DEF_SPLICE` in `verify.ts` appends a name-parameterized definition from
  `tests/def_templates/` into the seed-once thin `rust/src/lib.rs` / `wasm/src/lib.rs` (the
  documented residence; `src/generated/**` would collide E0255 with the tool's own re-export glue),
  and a codec fn additionally gets the hand `use` that comment_dsl.mdx names as the remedy for a
  bare codec name. Those rows then run the ordinary compile/test verdict on both faces, plus — under
  the json emission profile, for these rows only — the emitted `wasm/json-gen` crate, which no other
  stage of this harness builds. The cost of NOT compiling them is recorded: the exemption let an
  extern-only-scope undeclared-module break ship unseen (since fixed — string-pinned by
  `integration_extern_only_scope_declared_in_root`, compile-pinned at the hand-fixture level by
  `facade_composition_compiles`), and seeding the first defs immediately surfaced a second one of
  the same class, where a spec of ONLY marker rules emitted an undeclared `serialization` module its
  own self-check named (`marker_only_root_declares_the_serialization_module`).
  Exactly 2 rows stay in `COMPILE_GATE_EXEMPT`, and for a reason a definition cannot answer — what
  is missing is a whole OTHER CRATE, not a type: `dsl.rust_name` (the pinned name lives in a
  dependency crate the generated `use extern_dep::…` needs on the path) and `dsl.extern_companions`
  (the directive DEFERS the wasm companions to a sibling wasm crate, so defining them locally would
  defeat the deferral it declares). Both are integration-tested where the other crate exists.
  A user-code row is also pinned in the D3 decode catalog, seeded or not, because the reference
  oracle does not describe its wire — an extern typename ruby rejects outright, and a custom codec
  writes bytes the ruby-generated vectors for the REPLACED type never describe.
- **The execution gate is what makes a `supported` verdict mean more than "the tool exited 0".**
  `prelude.any` was the canonical execution-gate catch — `x = any` exited 0
  but emitted `pub type X = Any;`, an undefined type — and the gate held it ➖ until first-class
  `any` support landed (2026-07-23); the row is now execution-verified `supported` with the wasm
  and JSON emission legs green too (all three surfaces shipped the same day; the evidence records
  the wasm crate compiling and the json emission round-tripping when embedded).
- **Role floor (role × feature coverage) — NOT a serde JSON-AST dump.** The `cddl` AST's `Serialize`
  is gated on `target_arch = "wasm32"`, so there's no free serde dump on a native build;
  `examples/ast_roles.rs` hand-walks via the crate's `Visitor` trait instead. Role-detection only
  needs the crate to PARSE (soft caveat) — which it does for the whole corpus by construction.
  `project_corpus.ts` verifies a role-keyed `[[cover]]` against the AST role floor AND the per-cell
  support verdict (check H), so it can't claim ✅ on an unsupported cell. The same floor is taken over
  the WHOLE corpus and joined onto the containment relation into `tests/corpus/COVERAGE.md`
  § "Role × feature containment grid": one row per construct, one column per `roles.toml` role in
  grammar order, carrying the matrix's own per-cell verdicts — ✅ every probed shape in the cell is
  supported, ➖ none is, **◐ the probed shapes disagree** (a support boundary *inside* one cell), `?`
  a spec-allowed row still awaiting a `verify.ts` grounding run, ✗ modelled only as spec-disallowed —
  plus `·` for a cell the corpus exercises and no containment row models, and blank for neither. The
  `◐` mark is what makes an intra-cell boundary legible rather than averaged away: `type2.value` ×
  `role.occurrence-target` reads ◐ because two keyed optional fixed-value forms generate where four
  unkeyed occurrence forms refuse — cardinality, not wrapping, splits that cell. **Do not cross-check
  the `·` cells against their ➖ siblings**: the floor is feature-granular ("an array appears in
  array-element role") and a containment row is shape-granular (its example is an *anonymous inline*
  array), so the two sides are different shapes and never a contradiction. The grid is informational
  and kept honest by the `coverage_md_diff` gate, not by a verdict; its denominator is observed
  (modelled ∪ exercised), which is why a blank cell claims nothing at all — see
  `matrix.over-acceptance-denominator` for the grammar-derived denominator that would make "nothing
  has an opinion here" a rendered state.
- **Failure-claim findings must carry a resolvable pin (check I).** A corpus-overlay `[[finding]]`
  that states a defect ("Bug —"/"Gap —" or "Candidate cddl-codegen fix") must name at least one
  backtick-quoted tracking artifact that resolves against the tree (a `tests/…` file or a
  `src/tests/` symbol), or `project_corpus.ts` hard-fails. Findings render into COVERAGE.md — a
  generated span `lint_doc_citations` never scans — so this arm is what keeps a fixed-but-unpruned
  finding from rotting silently (the class is ledgered in `tests/testing-roadmap.toml`'s
  "stale known-limitation prose" residual). Claim SEMANTICS stay review-owned: a resolving citation
  does not prove the claim is still true.
- **Over-acceptance / silent corruption is invisible to round-trip tests by construction** — the
  encoder only produces in-width/in-window values, so a decoder that accepts (or silently wraps)
  spec-invalid data round-trips green. Only an enforcement vector (a spec-invalid instance the
  decoder must reject) can see this class, so a constraint fix lands TOGETHER with its enforcing
  matrix rows/fixtures — a row with no reject vector re-hides the very bug it exists to catch.
  Proven twice: the silent `as u16` width truncation (65536 decoded "successfully" as 0, caught only
  by the `ctl.size.uint` boundary vector; every narrowing decode cast is now width-guarded, pinned
  by the `signed_ints_width_rejects` / `width_collapse_rejects` execution fixtures — one deliberate
  consequence, pinned there: `int` members reject spec-valid CBOR outside i64 as a representability
  rejection instead of silently mis-decoding it) and the float range/control windows (NaN-safe
  accept-form checks + bounds-enforcing wrapper newtypes for top-level ranges, pinned by the
  `float_bounds` / `top_level_float_ranges` fixtures and the `rangeop.inclusive.float` /
  `rangeop.exclusive.float` constraint vectors).
  For a CERTIFIED-but-unfixed silent-acceptance bug (no enforcing fix yet), the catalog's
  `class="over-acceptance"` vector class is the standing pin: spec-INVALID CBOR (both oracles reject)
  the decoder CURRENTLY wrongly accepts, replayed by `decode_conformance_replay`'s over-acceptance leg
  as "still wrongly accepts" (`over_accept_N`) so the pin flips LOUDLY when a fix lands, and projected
  by `query_q4_directional.ts` as the honest `enforce = no (over-accepts: M)` (dominating `yes`/
  `unverified`) instead of hiding the hole. No instances at HEAD (the set is empty) — but the class
  stays armed. Its worked precedent is the former widened-occurrence table class: a single
  non-literal arrow map entry once table-detected every occurrence spelling to the same unbounded
  0..N `BTreeMap` as `{ * k => v }` (`HomogenousMap` carried no bounds), so an out-of-window map
  (`8200a0` = empty) was wrongly accepted against `+`, and
  `contain.occurrence-target.memberkey.type1.plus_table` carried the pin. The fix made `+`/`1*` a
  `NonEmptyMap` and every other unique-key window — including `?`, `n*m`, and omitted exact-once — a
  checked `BoundedMap`; their boundary vectors are now `class="constraint"` and the rows are Q4
  enforce-green. Bounded `@duplicates preserve` tables use the same inclusive-window contract over
  entry-ordered `BoundedPairMap`, so duplicate keys count as separate entries.
- **Constraint-vector SHAPE is load-bearing: a `class="constraint"` vector for a `standalone` row
  must be a bare in-type instance of the row's type** — decodable all the way up to the constraint
  itself, so the emitted range/size check is the ONLY thing that can reject it. A holder-wrapped
  scalar (`[0, 11]` = `82000b`) against a standalone row rejects as a TYPE mismatch before any bounds
  check runs — vacuous enforcement evidence indistinguishable from the real thing (and the Q4 pin
  fixes the row *set*, not vector quality). This exact decay shipped once and was caught only by
  review. TWO gates now catch it. Structural: `project_decode_conformance.ts` § 6 (local-tier drift
  gate) fails a constraint vector whose leading CBOR major-type class differs from its row's accepts
  (majors 0/1 merged — int-family instances span both signs), and bans the `8200` holder preamble on
  an accept-less standalone row. Behavioral: each `class="constraint"` vector carries an `expect_err`
  substring, and the rust replay gate asserts the decoder's error Display CONTAINS it — pinning the
  rejection REASON, so a TYPE-mismatch (or any wrong-reason) rejection no longer passes as it would
  under a bare `is_err` check. The authoring rule remains: holder shapes belong ONLY to
  `mode = "holder"` rows, and a row's accept and reject vectors share their outer CBOR shape.
- **The `ruby=` evidence clause is a DETERMINISTIC verdict, not the raw `generate` exit.** For a rule
  carrying a value-space-NARROWING control operator (`.eq`/`.ne`/`.lt`/`.le`/`.gt`/`.ge`/`.and`/
  `.within`/`.size`), the ruby `cddl generate` mode draws a RANDOM instance of the target type and
  self-validates it — a Bernoulli trial whose exit flips `ok`/`fail` on identical input across runs
  (a random uint rarely lands in `.and (0..9)`; root cause in
  `cddl-matrix/upstream-reports/ruby-cddl-bernoulli-constraint-controllers.md`). `verify.ts` therefore never derives
  a verdict from `generate` for those ops (classified statically by controller op-name —
  `lib.ts` `rubyGenerateIsBernoulli`, self-tested at startup, gated by `verify_selftest`;
  `bun run verify.ts --selftest` runs that check, its wasm-evidence sibling, the policy-mint
  classifier, the unknown-flag process boundary, and the scratch-cwd toolchain pin standalone in
  tens of milliseconds). The
  clause reports one of three deterministic tokens, all preserving the `; ruby=` delimiter downstream
  splitters key on: `ruby=ok|fail` from `generate` for NON-narrowing examples; `ruby=ok(validate)|
  fail(validate)` for a narrowing op WITH committed spec-valid accept vectors — ruby `validate` over
  those vectors (deterministic input ⇒ deterministic verdict), the authoritative source; and
  `ruby=nondet(generate)` for a narrowing op with NO committed vectors (`ctl.and`, `ctl.within`) — a
  STABLE token chosen without a subprocess, never spec-invalidating (a dice roll must not flip a row's
  status). Control-op-axis ruby is corroboration-only, but a narrowing FEATURE row's `spec_valid` reads
  the same deterministic source, so no classified row can hard-fail a run on a draw.
- **Verifier invocation and compiler identity fail closed.** `verify.ts` rejects every unrecognized
  `--` token at startup (rather than falling through to an ordinary sweep); its accepted flags are
  the declared mode flags plus `--smoke=`, `--only=`, and `--probe-only=`. It parses the repository's
  `rust-toolchain.toml` once and forces that `RUSTUP_TOOLCHAIN` into every nested cargo/rustc process,
  including the `lib.ts` `rustc -vV` cache-key probe, so a cache entry identifies the compiler that
  actually built its scratch crate.
- **The component probe's Rust→WIT resource lookup is executable lockstep, not a copied convention.**
  `verify.ts` must mirror the generator's Rust-ident-to-kebab conversion to find the resource that a
  row minted. Its startup self-test parses the exact literal assertion table in
  `src/utils.rs::convert_to_kebab_case_table` and requires every TypeScript fixture to be present with
  the same expectation; an empty table, a malformed or duplicate row, a missing fixture, or a value
  mismatch fails before component work begins. The local-tier `verify_selftest` gate runs this check,
  preventing a drifted lookup from silently understating the component execution evidence as
  "unminted."
- **A WIDE evidence flip means check disk, not the oracle.** A single ruby flake dirties ONE row's
  `ruby=` clause; the distinct tell of the ENOSPC class is MANY rows flipping in one run to the SAME
  generic cargo-failure line (`cargo test exit 101`), none reproducing when probed solo. The cause is a
  near-full scratch volume (`/tmp` backing the 100s of throwaway probe crates): once headroom runs out,
  generations fail identically instead of loudly. `verify.ts` runs `diskHeadroomPreflight` at startup
  (the oracle-fingerprint's sibling) — a 2 GiB `df` floor on the scratch volume, hard-failing with the
  stale-scratch cleanup (`rm -rf $TMPDIR/cddl_codegen_* $TMPDIR/cddl_verify_*`) named — so the
  low-headroom case fails fast on every probe/mint path. It also stops contributing to the problem: a
  process-exit handler deletes the two `mkdtemp` `CARGO_TARGET_DIR`s (`cddl_verify_target_*`,
  `cddl_verify_wasm_target_*` — the multi-GB ones) on every path including the early guards, and
  deletes the probe dir too on a clean exit, keeping it and printing its path on a non-zero exit so the
  `HARNESS FAILURE` messages that name a generated crate still point at one. Consequence for triage:
  leftover `$TMPDIR/cddl_verify_*` now means a run that was KILLED (a signal skips exit handlers) or a
  red run's one kept probe dir — not the normal case. The triage lesson still generalizes: before
  trusting (or hand-reverting) a wide evidence diff, check `df` and clear stale scratch.
- **A failure category joins the verdict by joining the OUTPUT — one registry, not two lists.**
  `verify.ts`'s `SECTIONS` and `project_corpus.ts`'s `CHECKS` are each a single array from which both
  the hard-fail verdict (`hard` entries with non-empty `items`) and the printed console sections are
  derived, so registering a category is one edit and a category cannot fail a run with nothing to
  read. This is the structural form of a defect that shipped: `cddl_codegen_gaps` sat in `verify.ts`'s
  hard-fail expression with no print block, and a red run said "see above" pointing at nothing — the
  culprit readable only in `verify_report.json`. Adding a category means adding a registry entry
  (`key`, `hard`, `items`, headings, per-item printer); a module-scope self-check refuses to run on a
  duplicate/empty key or a hard entry with no printer, and `project_corpus.ts` additionally asserts
  every registered check reached the console. Purely informational output (`verify.ts`'s
  `harness_timeouts_retried` note; `project_corpus.ts`'s phantom/unreferenced/detector-blind lines and
  the `ℹ️  C. SUPPORT SEAM` block) stays outside the registries — it bears no verdict, and putting
  non-verdict rows in the structure that defines the verdict is what makes such a list drift.

## Registering a new vendor (CDDL_CODEGEN) feature row

The end-to-end gate chain, in order. Each step's gate fails loudly if skipped, but nothing else
records the ORDER (walked empirically for the `@used_as_elem` registration):

1. Add the feature row in `features/cddl_codegen.toml`, then `bun run build_matrix.ts`.
2. Full `bun run verify.ts` — writes the row's verdicts into `annotations/cddl_codegen.toml`.
   **If the row's example references user-supplied code** (custom-codec fns, extern/raw-bytes
   types), add its `DEF_SPLICE` entry in `verify.ts` BEFORE this step — reusing a
   `tests/def_templates/` template where one fits, adding one where none does — and validate it
   against a scratch generate + `cargo check` first. Minted without it, the standalone-compile probe
   fails and the row verdicts `unsupported`, costing a second full run to correct (walked both ways:
   the `dsl.custom_encodings` registration paid the second ~12 min run; `dsl.custom_wire_major`
   pre-declared and minted `supported` first try). `COMPILE_GATE_EXEMPT` is the fallback for the
   narrow case where no definition can help because a whole OTHER CRATE is missing; its entry must
   name what structurally blocks a def, and the row then verdicts from the generation exit alone.
3. `bun run project_robustness.ts` — mints `tests/matrix_supported/<id>.cddl`.
4. `bun run verify.ts --mint-decode-foreign --only=<id>` — mints the decode catalog vectors.
5. Full `verify.ts` again — satisfies the decode-foreign evidence clause.
6. `build_matrix.ts` again, then the projections: `project_status_headers.ts --write`, a corpus
   `[[cover]]` entry + `project_corpus.ts`, `project_recombination.ts`, `query_q1_gaps.ts --write`,
   and a `tests/decode_conformance/catalog.toml` row (for an extern-referencing/user-code feature,
   a `pinned_reason` row per the `ext.extern` precedent replaces steps 4–5's minted vectors —
   `project_decode_conformance` fails until one exists either way). Do not stop at the gates a
   quick subset validates: the `dsl.rust_name` registration shipped with exactly this step
   half-done, and four drift gates (coverage_md, decode-conformance, recombination, status
   headers) failed on the next tier run in another session.
7. Bump `query_q6_diff.ts`'s `VENDOR_FEATURE_COUNT` pin.
8. Expect the regenerated recombination ingredients to SHIFT the sweep's deterministic composition
   indices: newly-explored compositions can surface latent panic classes that predate the feature
   (the `dsl.rust_name` regen surfaced two — both robustness-pinned before the projection learned
   to skip `example_extern_stub` rows as non-self-contained fillers, which reverted the shift).
   That is the fuzzer working, not a regression from the feature — run the sweep AND the layer-2
   execute gates, and walk the promotion/stale-pin protocol both directions before shipping the
   registration (an index shift can also strand a `LAYER2_KNOWN_BAD` pin whose composition
   disappears).

If the feature is a comment-DSL tag, `corpus_detect.ts` batches its comment-owner blocks through
`examples/comment_dsl.rs`. That helper calls the real `comment_ast::metadata_from_comments` grammar
and projects the accepted `RuleMetadata` through an exhaustive field match, so neither directive
vocabulary nor argument grammar is reimplemented in TypeScript. Add parser/self-check vectors for a
new semantic flavor, but do not add a scanner table.

The parser-backed detector and the feature registry are **independent mechanisms with different
triggers**, so extending one never implies the other. The detector follows accepted metadata; the
`features/*.toml` row is gated by *documentation* instead: `verify.ts`'s forward completeness lint
treats a directive documented in `docs/docs/comment_dsl.mdx` as surveyed surface and HARD-FAILS
(`missing_cddl_codegen_feature`) while it has no row. A documented directive therefore cannot sit
without a row, even though the row costs a multi-minute verdict mint against the external ruby/rust
oracles — deferring just the mint leaves the `verify` gate red, and since `verify` is full-tier while
CI runs `fast` only, nothing else reports it. The corpus FIXTURE is the one genuinely optional piece:
with a row but no fixture the id renders ➕ supported-untested, which `project_corpus`'s check D
accepts (it demands a cover only for an id the corpus actually exercises).

A FLAVORED sibling row — a multi-token `alt` (`@used_as_key hash`: an existing directive plus
argument words) — engages two recognition surfaces, walked empirically for the
`dsl.used_as_key.{hash,ord,hash_ord}` registration: `verify.ts`'s backward (FABRICATED) lint resolves
the alt only when every trailing word is in the flavor vocabulary extracted from `comment_ast.rs`'s
match arms (two arm shapes:
the DemandSet flag form `"hash" => demand.hash = true` and the enum-value form
`"preserve" => DuplicatesPolicy::Preserve`; a fabricated flavor word still flags, and a vacuity
guard fails loud if the extraction pattern rots). Separately, the parser-backed `comment_dsl`
projection must credit the narrowed sibling id from the merged semantic value (`DemandSet` or
`DuplicatesPolicy`), with matching self-check vectors, or `project_corpus`'s coverage floors drift.

An ARGUMENT-REQUIRED directive (`@duplicates`: the bare spelling is a parse-time panic) registers
as flavored sibling rows ONLY — no bare base row, and the vendor-count pin moves by the number of
flavors. The forward (completeness) lint accepts this: a documented directive is modelled either
by a row whose `alt` IS the directive or by rows whose `alt` starts with `<directive> ` — walked
empirically for the `dsl.duplicates.{reject,preserve}` registration.

## Registering a CONTAINMENT cell

Same discipline, different obligation chain — walked empirically for the dynamic-row occurrence
family (`open_struct*`, `open_table*`), whose three `2*3` cells prove that row-local bounds do not
turn into enclosing-map bounds. In order:

1. Add the `[[contain]]` rows in `containment/<axis>.toml`, then `bun run build_matrix.ts`.
2. Full `bun run verify.ts` — mints each cell's `[[support]]` verdict. A supported dynamic row must
   execute its complete carrier path; generation alone cannot prove the count survives a cross-face
   boundary.
3. `bun run project_robustness.ts` — supported cells mint `tests/matrix_supported/<id>.cddl`,
   rejected cells mint `tests/matrix_reject/<id>.cddl`.
4. A rejected cell's row must ALSO land in the committed reject catalog:
   `INSTA_UPDATE=always cargo test --bin cddl-codegen unsupported_construct_reject_catalog`
   re-blesses `tests/matrix_reject/snapshots/catalog.snap` (the catalog↔matrix clause of
   `project_robustness_check` demands the row).
5. Supported cells need decode rows in `tests/decode_conformance/catalog.toml`: self-contained
   examples take REAL minted vectors (`verify.ts --mint-decode-foreign --only=<id>,...`);
   user-code examples take a `pinned_reason` row. A mint then requires the full `verify.ts`
   AGAIN — the evidence clause must record `accepts N foreign vector(s)`, and the drift check
   says so verbatim ("mint BEFORE probe, or re-probe after").
6. `build_matrix.ts`, then the projections: `project_corpus.ts`, `project_status_headers.ts
   --write`, `query_q1_gaps.ts --write` (the generated Limitations block moves with the cell
   arithmetic), and the `--check` trio (`q6`, `decode_conformance`, `build_matrix`).
7. Expect the cells' examples to meet the REPLAY oracles for the first time on the next full
   tier, and route each finding to its designed ledger rather than a weakened contract:
   `JSON_SURFACE_SKIP` (a value class that cannot cross the json boundary — cite its owning
   record: the non-string-map-key gotcha above for the kept boundary, a `roadmap.toml`
   `findings-open` entry for a defect), `RUST_ORACLE_SKIP` (a rust-validator gap, ruby keeps judging — cite the gap number),
   `ENCODING_VARIANT_SKIP` (a real DECODER gap over a genuinely spec-equal re-encoding — cite the
   findings entry). All three are stale-guarded. A reordering variant whose value-equality premise
   is false for the TYPE is not ledgered at all: an `@duplicates preserve` pair-map's exemption is
   derived from the row's own `spec`, and the loose-container control rows that keep it honest fall
   out of the same rule rather than needing to be maintained beside it.

## Evidence/id convention

`id`s and evidence use **spec anchors (production / RFC section) and grep-able code anchors, never line
numbers** — same robustness rule as the COVERAGE.md docs.

## Conventions (decided)
- **Home:** top-level `cddl-matrix/` (reusable beyond this repo's tests, hence not under `tests/`).
- **Format:** TOML authored overlay + generated `matrix.json`; native sources pinned & committed under `sources/`.
- **Verification:** `build_matrix.ts --check` snapshots the synthesis (fails if `matrix.json` is stale
  vs the authored overlay — the editorial join under test, not just the inputs' checksums); `verify.ts`
  is the reproducible gate (reconcile + triangulated probes). Both are Bun/TypeScript (`bun run …`).
  `verify.ts`'s oracles resolve as: ruby `cddl` (`gem install --user-install cddl`; auto-resolved at
  `Gem.user_dir/bin/cddl`) and a rust `cddl` CLI via `RUST_CDDL` (defaults to the `local-fixes`
  sibling checkout — § "Upstream oracle gaps" explains why, and why to pin an immutable copy before a
  multi-probe run). Evidence-writing verify runs REQUIRE the pinned-behavior oracle: a stock
  `cargo install cddl` build is refused at startup by the `runOracleFingerprint` behavioral fingerprint
  (its released-CLI gaps fail the pinned probes by design), so pointing `RUST_CDDL` at
  `~/.cargo/bin/cddl` no longer produces a degraded-but-workable run — supply the `local-fixes` @
  `ac1b98e` build (or an immutable copy of it) instead. Its generated-crate compile gate reuses
  `integration_tests::feature_corpus_compiles`' shared-target pattern, with the dep warm-up LAZY
  (run before the first cache miss, behind an always-run generation self-test) because the
  per-probe `cargo test`/`check`/replay steps are memoized by generated-tree content hash (the
  gate cache — `tests/README.md` § "The gate cache (memoize-and-skip for nested cargo)"). A cached
  probe records byte-identical evidence/annotations to an executed one — the cache may never change
  `verify_report.json` or annotation content, only skip re-proving it; `GATE_CACHE=0` forces every
  probe to run, and the invariant is mechanically enforced by the full-tier
  `verify_cache_transparency` gate (`check.ts full --cache-transparency`: two verify runs, cached
  vs `GATE_CACHE=0`, byte-diffed).

## Scope (v1)
RFC 8610 backbone in its authoritative current form: 9682 grammar + 8610 prelude + the IANA control-op
registry (which already spans 8610/9090/9165/9741). Encoding axis from RFC 8949. CDDL-modules / future
drafts are out of v1.
