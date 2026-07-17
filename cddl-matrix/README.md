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
> upstream-oracle-gap state) → [`ROADMAP.md`](ROADMAP.md)
> (what's left: remaining work + the open-findings ledger) → [`QUERIES.md`](QUERIES.md) (the
> consumer-query contract). The matrix is **fully scaled and gate-green**: <!-- status-header counts are generated — regenerate with: cd cddl-matrix && bun run project_status_headers.ts --write --><!-- gen:sh:readme-counts -->107 features and 94 containment cells<!-- /gen:sh:readme-counts -->
> across all axes (incl. the `CDDL_CODEGEN` vendor profile), with <!-- gen:sh:readme-annotations -->235 cddl-codegen support annotations<!-- /gen:sh:readme-annotations -->,
> **execution-gated** support **per-feature, per-cell (role × feature), AND per-control-op**
> (<!-- gen:sh:readme-ops -->all 37 IANA ops probed<!-- /gen:sh:readme-ops -->) — "supported" means the
> generated crate's emitted round-trip tests *pass* (`--emit-tests` + `cargo test`), not merely that
> it generates and compiles. A second, orthogonal **emission axis** re-probes every default-`supported`
> row under each non-default codegen profile (`preserve`, `json`) and records a per-profile verdict
> (`emission.<name>.status`) — the axis is **filled**: a row with no `emission` keys is one whose
> default verdict is not `supported`, hence unsupported under every profile (a derived fact, since
> only default-`supported` rows are probed). These verdicts are load-bearing, not just reporting:
> the Rust gate `all_supported_constructs_generate_all_profiles` derives its expected per-profile
> generation failures from `emission.<profile>.status = "unsupported"` (no second hand list), and
> `project_corpus.ts` renders them as per-row profile caveats in `tests/corpus/COVERAGE.md`. **Four projections *generate* their hand docs:**
> `golden_hex` (encoding axis), the corpus feature-axis projection — `project_corpus.ts` generates
> `tests/corpus/COVERAGE.md` (the original north-star target, now subsumed; `corpus_detect.ts` +
> `annotations/corpus/`) — `query_q1_gaps.ts`, which generates the `## Limitations` section of
> `docs/docs/current_capacities.mdx`, and `project_status_headers.ts`, which drift-checks the countable
> status-header prose (feature/annotation/op/divergence/constraint counts) as spans in this README,
> `ROADMAP.md`, and `tests/README.md`. A fifth projection feeds a consumer that is not a hand doc:
> `project_recombination.ts` distils every feature `example` + the containment legality data into
> `tests/recomb/ingredients.json` (drift-gated by check.ts `project_recombination_check`), the
> ingredient set the shape-recombination fuzzer composes from (`tests/README.md`
> § "Shape-recombination fuzzer"). **Every consumer query Q1–Q6 is answered by a standing script**
> (`QUERIES.md` § "Definition of done"). Any "seed / only `type2` worked" or "renderer is the
> remaining piece" framing below this line is historical.

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

**Why the CBOR tag registry is *not* a fourth source (F4 decision).** A generic codegen would need to
individuate tags (tag 0/2/24 are different tasks). cddl-codegen, however, is **tag-parametric**:
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
| `features/*.toml` | one row per **serialization-relevant construct** (+ `profile` + stable ids); ABNF is a lint, not the spine — see "What is a feature?" | constructs + variations (A/B) |
| `roles.toml` | nesting container-contexts (array-element, map-key, tag-content, …) | nesting (C) |
| `containment/*.toml` | `role × feature → spec-allowed?` — **where nesting/variation gaps live**; each cell carries an `example` that `verify.ts` also probes for **per-cell tool support** | nesting (C) |
| `encodings.toml` | CBOR major-type × form grid (RFC 8949); **legality is major-type-dependent** (e.g. ints have no indefinite form), not a free orthogonal axis | encoding (D) |
| `control_examples.toml` | minimal probe `example` per IANA control op (the CSV is byte-pinned, so examples live here); joined onto the control-op axis by `lib.ts` and probed by `verify.ts` for **per-control-op support** | control-op |
| `annotations/<tool>.toml` | per-consumer support, keyed by master id (NOT part of the spec master). A `[[support]]` row keyed by a **feature** id is the top-level verdict; keyed by a **containment** id it is the **per-cell (role × feature)** verdict; keyed by a **`ctl.<name>`** id it is the **per-control-op** verdict — so the master records "supported *here*, not *there*" (e.g. `type2.map` supported as tag-content, unsupported inline as a choice/array member). **Support is execution-gated**: `verify.ts` generates with `--emit-tests=true` and requires the crate's IR-minted round-trip/reject tests to *pass* (`cargo test`), not just that cddl-codegen exits 0 — catching non-compiling emissions (`x = any` → `pub type X = Any;`, a type defined nowhere) AND compiles-but-miscompiled round-trips. Types that mint no STANDALONE test surface (transparent aliases, bounded/newtype-able aliases, named tables/arrays, pure c-enums) are re-probed wrapped in a synthetic record holder (`__probe_holder = [0, <rule>]`) so their embed-site wire path runs — the only wire path these shapes have — and the evidence reads "round-trips when embedded" (per-probe `embedded` bit). If the synthetic can't generate (a generic rule needing type args) or can't round-trip, the probe keeps the compile verdict; the embed only ever UPGRADES evidence, never downgrades a verdict. **Emission axis:** a default-`supported` row is ALSO re-probed under each non-default codegen profile (`preserve`, `json` — the flag sets from `src/tests/mod.rs`'s `ALL_PROFILES`) via the identical rust-only pipeline (no ruby/rust/wasm re-run), recorded as dotted `emission.<name>.status`/`emission.<name>.evidence` keys. Absence of `emission` keys means the row's default verdict is not `supported`, so it is unsupported under every profile — a *derived* fact, never hand-authored; only a passing `verify.ts` run fills the axis. **Wasm probe:** a **default-on** `--wasm` probe (opt out with `--no-wasm` / `VERIFY_WASM=0`) additionally `cargo test`s the generated wasm crate — the emitted `cddl_generated_wasm_tests` module constructs through the wrapper API, round-trips, and cross-checks against an independent `cddl_lib::` rust build (see `tests/README.md` § "wasm-crate test module") — and threads `minted_wasm` / `wasm_roundtrips` into the per-feature and per-cell evidence. **Decode-foreign clause:** a supported row's evidence also records whether the generated decoder ACCEPTS the committed spec-derived vectors from `tests/decode_conformance/catalog.toml` (instances our code did not produce — `; accepts N foreign spec-derived vector(s)`; see `tests/README.md` § "Decode-direction conformance"). Corroboration only, default-on (`--no-decode-foreign` opts out), never changes a verdict. | — |

**3. Generated view — `matrix.json`** (produced by `build_matrix.ts`) joins the overlay with the native
sources into one universal artifact for downstream/cross-language consumption. Imported axes are
*derived*, not authored: control operators come straight from the IANA CSV with ids `ctl.<name>` (with
their probe `example` joined from `control_examples.toml`). It is
regenerated, never hand-edited. `build_matrix.ts` also runs the **drift-check** (every annotation id must
resolve to a real master id). Run it with `bun run build_matrix.ts`; `lib.ts` holds the shared loaders
+ the byte-exact JSON serializer.

> **Editing this README or `ROADMAP.md`?** They are themselves linted: the sibling script
> `lint_doc_citations.ts` (check.ts `local` tier; not matrix tooling — it lives here for the shared
> tsc coverage) asserts every "pinned by/tracked by/gated by `name`" citation in the hand docs still
> resolves in the tree, bans positional "…item `<N>`" citations, bans numbered section headings in
> the hand docs (a numbered heading invites `§ <N>` citations, which silently retarget on
> renumbering), and requires a blank line before headings.

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
<!-- gen:sh:readme-enforce-green -->28 rows<!-- /gen:sh:readme-enforce-green -->: `ctl.size`, `ctl.cbor`, `memberkey.cut`, the six numeric range/eq ops (`ctl.{le,lt,gt,eq,ne,ge}`)
plus their boundary-value rows `ctl.ne.{zero,one}` (the `(1,-1)` / degenerate `(2,0)` NE encodings),
`ctl.size.uint` (65536 over the u16-collapsed window, rejected by the width-guarded member decode —
the guard that replaced the silent truncation this row's vector exposed; pinned by the
`signed_ints_width_rejects` / `width_collapse_rejects` execution fixtures),
the eight `rangeop` rows (`rangeop.{inclusive,exclusive}` plus their head-type × sign variation
rows `.int`/`.nint`/`.float`), the three occurrence-bound rows `occur.bounded{,.lower,.upper}`
(out-of-count arrays against the generated `Vec` length check), and the three fixed-value lexeme
rows `value.number.{hexfloat,hex,bin}` (wrong-value instances against the fixed 3.0 / 16 / 10,
rejected as FixedValueMismatch — hex/bin carry hand pins including `[0]`, the silent-zero
radix-conversion trap from `draft/rust-cddl-radix-int-literal-gap.md` § post-implementation
findings). Upstream rust-oracle
gaps shape what "certified" means per family
(`query_q4_directional.ts --check` pins the exact green set — and the now-empty unverified set — so
a decay fails loudly rather than
silently dropping enforcement evidence): the numeric ops' probe examples target `int` with literal,
non-vacuous bounds (`x = int .le 10`, `.ge 5`, …) because the rust oracle does not enforce these ops
over a `uint` target (`draft/rust-cddl-uint-control-op-gap.md`), so `uint`-targeted or
vacuously-bounded forms cannot pass the both-oracles-reject gate; the non-uint-endpoint range
rows (`.int`/`.nint`/`.float`) carry rust corroboration only against the local-fixes oracle @
`885c61c` — the released 0.10.x CLI blanket-rejects EVERY instance of a float or negative-int
range, valid or not (gap #3 in § "Upstream oracle gaps" below; the fork fix is what let those six
rows' accept side mint and made their rejects discriminating); and `ctl.size.uint`'s reject
certification leans on RUBY plus the local-fixes oracle, which rejects the holder-wrapped violation
discriminatingly (the released 0.10.x CLI misvalidates any control-op-carrying rule referenced as
an array entry — gap #4 below; that fork fix is also what let the row's accept side mint).
`ctl.default` is `n/a` (it governs an absent field — no rejectable instance).

**A certified over-acceptance projects `enforce = no (over-accepts: M)`** — the fifth enforce value,
dominating `yes`/`unverified`/`n/a`. Its evidence is a `class="over-acceptance"` accept vector:
spec-INVALID CBOR (both oracles reject at mint, the same inverse gate as `class="constraint"`) the
generated decoder CURRENTLY (wrongly) ACCEPTS. At HEAD the set is **empty** — asserted exactly by
`query_q4_directional.ts --check`'s `EXPECTED_ENFORCE_OVERACCEPTS` pin (`[]`), the decay twin of the
green/unverified sets — but the vector class stays armed for the next certified instance. The worked
precedent is the widened-occurrence-marker table class (`ROADMAP.md` § findings): a COUNT-PERMITTING
occurrence marker (`+` / `?` / `n*m`) on a single non-literal arrow map entry once table-detected to
the same unbounded 0..N `BTreeMap` as `{ * k => v }` (`HomogenousMap` carried no bounds), so
`contain.occurrence-target.memberkey.type1.plus_table` carried a certified out-of-window empty-map pin
while the bug stood. It demonstrates BOTH branches the pins encode: the `+` marker was FIXED (honored
as `NonEmptyMap`), so its over-acceptance vector was **promoted** to `class="constraint"` (+
`expect_err`) and the row moved to the enforce-green set; the `?` / `n*m` spellings were closed by
graceful rejection at generation, dropping their rows and pins — like the seed instance, the
no-occurrence type-domain arrow widening `contain.map-key.memberkey.type1.tstr_arrow_nooccur`.

Cut/socket *semantics* stay hand-asserted overlay notes in the corpus projection
(⚠️ parsed-but-not-honored): they are validation concerns a round-trip cannot observe.

## Upstream oracle gaps (rust `cddl` CLI)

None of these are cddl-codegen bugs, and the matrix no longer sits on any of them — but they shape
what "certified" means per vector family (§ Q4 above) and what the close-out steps in `ROADMAP.md`
§ findings wait on. The sibling checkout's `local-fixes` branch (`~/Documents/git/cddl`, commit
`ac1b98e` — also the `Cargo.toml` pinned rev, so the generated-crate conformance oracle AND
cddl-codegen's own parser share it) carries fixes for gaps 1–7 and 9–10 (gaps 8 and 11 are OPEN at
that rev — gap 8 keeps one containment row's decode-foreign minting `pinned_reason`-vectorless, and
gap 11 keeps three corpus decode rows empty-instance-only); `RUST_CDDL` defaults to that build,
giving `verify.ts` runs an enforcing oracle. Because every local branch reports version 0.10.6, a
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
   `int` ARE enforced. Upstream PR submitted; full repro in `draft/rust-cddl-uint-control-op-gap.md`.
   This is why the six ops' probe examples (`control_examples.toml`) target `int` with literal,
   non-vacuous bounds (§ Q4), and why `ctl.size.uint`'s rust corroboration leans on the local-fixes
   build.
2. **group-in-array sequence/occurrence misvalidation** (released 0.10.x): a multi-entry group in an
   array (parenthesized inline OR named reference) validated correctly ONLY as the sole entry with
   no occurrence indicator — inner entries were checked at group-local indices as if absolute array
   positions, and occurrence bounds were compared against total array item count instead of
   repetition count. Ruby accepts all the spec-valid instances. FIXED in `local-fixes` @ `773b723`;
   the two catalog rows that sat on it honestly
   (`contain.occurrence-target.grpent.inline_group.exactly_once_array` and
   `contain.occurrence-target.grpent.groupname`) are re-minted with real accept vectors against that
   build. Full repro table: `draft/rust-cddl-group-occurrence-array-count-gap.md`.
3. **non-uint-endpoint range blanket rejection** (released 0.10.x — a 0.10.0 regression): every
   instance validated against a range whose endpoints are not uint is rejected, valid or invalid
   (`invalid cddl range. upper and lower values must be uint types`) — float ranges (`0.5..10.5`)
   AND negative / sign-spanning int ranges (`-10..10`, `-10..-3`); `0..10` validates correctly.
   `compile-cddl` accepts the spec; only `validate` blanket-rejects. FIXED in `local-fixes` @
   `885c61c` (integer windows compare as i128 across any uint/int endpoint mix; float windows match
   floats only, via a NaN-safe accept-form check); the six
   `rangeop.{inclusive,exclusive}.{int,nint,float}` variation rows that sat on it with NO accept
   vector are re-minted with real accept vectors against that build, and their `class="constraint"`
   rejects are now corroborated discriminatingly by rust instead of leaning on ruby alone. Repro +
   fix provenance: `draft/rust-cddl-float-range-gap.md`.
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
   (`draft/radix-oracle-deviations-verdict.md`), so future radix-position rows can't lean on ruby.
   Repro + fix provenance: `draft/rust-cddl-radix-int-literal-gap.md`.
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
   (`lib.ts`): `prelude.number`'s float arm is re-minted with real f32/f64 accept vectors (the `f9`
   half-precision ban stays — that's the separate cbor_event mis-decode, `ROADMAP.md` § findings)
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
   `draft/rust-cddl-tag-map-key-gap.md` (local note).
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
   `contain.occurrence-target.memberkey.type1.optional_table` is a reject fixture in
   `tests/matrix_reject/` with no decode-catalog row to re-mint. Two fingerprint probes
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
11. **named-rule / parenthesized-choice map keys over-rejected** (OPEN at `ac1b98e`, NOT
   fork-fixed — found by the first corpus decode-conformance mint): `validate` WRONGLY REJECTS
   every non-empty map instance whose KEY domain is a NAMED-RULE reference (`{ * fe => uint }` +
   `fe = 0 / 1 / 2` — and equally `fe = 0`, `fe = uint`, `fe = uint / nint`, `fe = tstr`) or a
   PARENTHESIZED CHOICE (`{ * (0 / 1 / 2) => uint }`), while ruby accepts; the same keys spelled
   INLINE (`{ * uint => uint }`, `{ * 0 => uint }`) validate fine, and the empty map validates
   everywhere (no key to mis-match). Error shape: `unexpected key Integer(Integer(0))` from the
   map-key arm — the same neighborhood as gap #8 (TAG-typed map keys), likely the same
   key-matching site not resolving typename refs/choices. This is what strips the corpus decode
   rows `c_style_enum_map_key.enum_keyed_map`, `table_enum_key.enum_keyed` and
   `table_enum_key.enum_key_holder` down to their EMPTY-map instances (every non-empty ruby
   candidate dies `ruby=0 rust=1` on the two-oracle gate; a mint whose random draws miss the
   empty instance pins the row `pinned_reason`-vectorless instead, with the per-oracle tallies in
   the pin wording — the two states flip-flop across re-mints while the gap is open). No
   upstream issue filed yet. Differential grid, two adjacent same-neighborhood observations
   (nested-map VALUES in a `*` table; multi-entry composite-array-key tables), and close-out
   steps: `draft/rust-cddl-named-key-map-gap.md` (local note).

## Gotchas (read before touching the support seam or probe examples)

The recurring rule: **a panic/compile-failure on minimal *valid* CDDL is a finding to surface
(ledger it in `ROADMAP.md` § findings), not something to engineer away by making the probe green —
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
  position panics (`parsing.rs` "Anonymous groups not allowed") — it must be **named**. A named RULE
  works in every position; the `@name` naming route the panic message advertises works only where the
  comment can reach the naming site — verified for the choice-member position, while at member
  positions (array-element, map-value) the directive never arrives and the panic stands (a pinned
  `KNOWN_SILENT_DROP` finding — see the comment-DSL entry in `ROADMAP.md` § findings). The one
  exception: **tag-content** accepts an inline composite. So `type2.map` is supported as
  tag-content, unsupported inline elsewhere, and works everywhere via a named reference — the
  per-(feature, role) verdict genuinely differs, which is the whole point. An inline parenthesized
  group carrying an occurrence marker (`[* (int, tstr)]`) is a distinct path: it is rejected
  gracefully (not a panic — `ROADMAP.md` § findings), with the same "name the group" remedy.
- **Containment cell-example hygiene.** The `type2.map`-in-a-role cells (`array-element` /
  `cbor-payload` / `choice-member` / `generic-arg` / `occurrence-target`) use 2-field map examples so
  any panic is attributable to the real **anonymous-group** reason (an inline map inside a role needs
  a name), with no single-field-map shape to confound it.
- **Execution-gate exemptions.** 4 user-code features stay `supported` via the documented
  `COMPILE_GATE_EXEMPT` allowlist — they reference user-supplied code, so they can't compile (or
  test) standalone: `ext.extern`, `ext.raw_bytes`, `dsl.custom_serialize`, `dsl.custom_deserialize`
  (integration-tested instead). The exemption has a recorded cost: it let an extern-only-scope
  compile break ship unseen (`ROADMAP.md` § findings, the extern-only undeclared-module entry). `prelude.any` is correctly ➖: `x = any` exits 0 but emits
  `pub type X = Any;` — an undefined type (root cause: `any` absent from `is_identifier_reserved`).
- **Role floor (role × feature coverage) — NOT a serde JSON-AST dump.** The `cddl` AST's `Serialize`
  is gated on `target_arch = "wasm32"`, so there's no free serde dump on a native build;
  `examples/ast_roles.rs` hand-walks via the crate's `Visitor` trait instead. Role-detection only
  needs the crate to PARSE (soft caveat) — which it does for the whole corpus by construction.
  `project_corpus.ts` verifies a role-keyed `[[cover]]` against the AST role floor AND the per-cell
  support verdict (check H), so it can't claim ✅ on an unsupported cell.
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
  `float_bounds` / `top_level_float_ranges` fixtures and the `rangeop.*.float` constraint vectors).
  For a CERTIFIED-but-unfixed silent-acceptance bug (no enforcing fix yet), the catalog's
  `class="over-acceptance"` vector class is the standing pin: spec-INVALID CBOR (both oracles reject)
  the decoder CURRENTLY wrongly accepts, replayed by `decode_conformance_replay`'s over-acceptance leg
  as "still wrongly accepts" (`over_accept_N`) so the pin flips LOUDLY when a fix lands, and projected
  by `query_q4_directional.ts` as the honest `enforce = no (over-accepts: M)` (dominating `yes`/
  `unverified`) instead of hiding the hole. No instances at HEAD (the set is empty) — but the class
  stays armed. Its worked precedent is the widened-occurrence-marker table class (`ROADMAP.md`
  § findings): a COUNT-PERMITTING occurrence marker (`+` / `?` / `n*m`) on a single non-literal arrow
  map entry once table-detected to the same unbounded 0..N `BTreeMap` as `{ * k => v }` (`HomogenousMap`
  carried no bounds), so an out-of-window map (`8200a0` = empty) was wrongly accepted against `+`, and
  `contain.occurrence-target.memberkey.type1.plus_table` carried the pin. The FIX (honoring `+` as a
  `NonEmptyMap`, `4fa3041`) flipped it loudly: the vector was promoted to `class="constraint"` (+
  `expect_err "0 not at least 1"`) and the row moved to Q4's enforce-green pin. The `?` / `n*m`
  spellings, and the seed instance (the no-occurrence type-domain arrow widening `{ tstr => uint }`,
  pinned by `no_occurrence_arrow_map_entry_rejects_gracefully`), took the flow's OTHER exit — rejected
  gracefully at generation, their rows flipped unsupported and their pins dropped with them.
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
  `draft/ruby-cddl-generate-bernoulli-constraint-controllers.md`). `verify.ts` therefore never derives
  a verdict from `generate` for those ops (classified statically by controller op-name —
  `lib.ts` `rubyGenerateIsBernoulli`, self-tested at startup / `bun run verify.ts --selftest`). The
  clause reports one of three deterministic tokens, all preserving the `; ruby=` delimiter downstream
  splitters key on: `ruby=ok|fail` from `generate` for NON-narrowing examples; `ruby=ok(validate)|
  fail(validate)` for a narrowing op WITH committed spec-valid accept vectors — ruby `validate` over
  those vectors (deterministic input ⇒ deterministic verdict), the authoritative source; and
  `ruby=nondet(generate)` for a narrowing op with NO committed vectors (`ctl.and`, `ctl.within`) — a
  STABLE token chosen without a subprocess, never spec-invalidating (a dice roll must not flip a row's
  status). Control-op-axis ruby is corroboration-only, but a narrowing FEATURE row's `spec_valid` reads
  the same deterministic source, so no classified row can hard-fail a run on a draw.
- **A WIDE evidence flip means check disk, not the oracle.** A single ruby flake dirties ONE row's
  `ruby=` clause; the distinct tell of the ENOSPC class is MANY rows flipping in one run to the SAME
  generic cargo-failure line (`cargo test exit 101`), none reproducing when probed solo. The cause is a
  near-full scratch volume (`/tmp` backing the 100s of throwaway probe crates): once headroom runs out,
  generations fail identically instead of loudly. `verify.ts` runs `diskHeadroomPreflight` at startup
  (the oracle-fingerprint's sibling) — a 2 GiB `df` floor on the scratch volume, hard-failing with the
  stale-scratch cleanup (`rm -rf $TMPDIR/cddl_codegen_* $TMPDIR/cddl_verify_*`) named — so the
  low-headroom case fails fast on every probe/mint path. The triage lesson still generalizes: before
  trusting (or hand-reverting) a wide evidence diff, check `df` and clear stale scratch.

## Registering a new vendor (CDDL_CODEGEN) feature row

The end-to-end gate chain, in order. Each step's gate fails loudly if skipped, but nothing else
records the ORDER (walked empirically for the `@used_as_elem` registration):

1. Add the feature row in `features/cddl_codegen.toml`, then `bun run build_matrix.ts`.
2. Full `bun run verify.ts` — writes the row's verdicts into `annotations/cddl_codegen.toml`.
3. `bun run project_robustness.ts` — mints `tests/matrix_supported/<id>.cddl`.
4. `bun run verify.ts --mint-decode-foreign --only=<id>` — mints the decode catalog vectors.
5. Full `verify.ts` again — satisfies the decode-foreign evidence clause.
6. `build_matrix.ts` again, then the projections: `project_status_headers.ts --write`, a corpus
   `[[cover]]` entry + `project_corpus.ts`, `project_recombination.ts`, `query_q1_gaps.ts --write`.
7. Bump `query_q6_diff.ts`'s `VENDOR_FEATURE_COUNT` pin.

If the directive is a comment-DSL tag, `corpus_detect.ts`'s `MIRRORED_DIRECTIVES` lockstep
tripwire also fires until the mirror (plus selfCheck vectors for any new grammar) is extended —
or better, move the dsl channel onto the AST floor instead (`tests/TESTING_ROADMAP.md`, the
twin-implementation drift entry).

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
