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
> consumer-query contract). The matrix is **fully scaled and gate-green**: <!-- status-header counts are generated — regenerate with: cd cddl-matrix && bun run project_status_headers.ts --write --><!-- gen:sh:readme-counts -->106 features and 83 containment cells<!-- /gen:sh:readme-counts -->
> across all axes (incl. the `CDDL_CODEGEN` vendor profile), with <!-- gen:sh:readme-annotations -->223 cddl-codegen support annotations<!-- /gen:sh:readme-annotations -->,
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
> resolves in the tree, bans positional "…item `<N>`" citations, and requires a blank line before
> headings.

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

**The enforcement axis (`enforce-constraint`) is grounded by `class="constraint"` reject vectors** in
`catalog.toml`: spec-INVALID CBOR whose ONLY invalidity is the constraint the row enforces (an
over/under-`.size` string, a non-uint `.cbor` payload, a cut-violating map value, an out-of-window or
excluded-endpoint number, NaN against a float window — each a valid instance of its base type),
certified spec-invalid at mint and durably rejected by the generated decoder — for the pinned
REASON: each vector's `expect_err` substring is asserted against the decoder's error Display by the
replay gate, so the rejection names the violated constraint, not just any `Err`. The green set is
<!-- gen:sh:readme-enforce-green -->26 rows<!-- /gen:sh:readme-enforce-green -->: `ctl.size`, `ctl.cbor`, `memberkey.cut`, the six numeric range/eq ops (`ctl.{le,lt,gt,eq,ne,ge}`)
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
generated decoder CURRENTLY (wrongly) ACCEPTS. One row carries it today —
`contain.map-key.memberkey.type1.tstr_arrow_nooccur` (the no-occurrence type-domain arrow widening,
ROADMAP § findings) — asserted exactly by `query_q4_directional.ts --check`'s
`EXPECTED_ENFORCE_OVERACCEPTS` pin, the decay twin of the green/unverified sets. When the decoder is
fixed the replay pin flips, the vector is promoted to `class="constraint"`, and the row moves to the
green set.

Cut/socket *semantics* stay hand-asserted overlay notes in the corpus projection
(⚠️ parsed-but-not-honored): they are validation concerns a round-trip cannot observe.

## Upstream oracle gaps (rust `cddl` CLI)

None of these are cddl-codegen bugs, and the matrix no longer sits on any of them — but they shape
what "certified" means per vector family (§ Q4 above) and what the close-out steps in `ROADMAP.md`
§ findings wait on. The sibling checkout's `local-fixes` branch (`~/Documents/git/cddl`, commit
`2c7548e` — also the `Cargo.toml` pinned rev, so the generated-crate conformance oracle AND
cddl-codegen's own parser share it) carries fixes for gaps 1–5 (gaps 6–7 are OPEN at that rev — gap 6
gates a corpus fixture's conformance-oracle half, not any matrix row; gap 7 blocks ONE arm-coverage-floor
class, ledgered honestly); `RUST_CDDL` defaults to that build,
giving `verify.ts` runs an enforcing oracle. Because every local branch reports version 0.10.6, a
version string cannot tell the pinned build apart from a wrong-branch rebuild, so `verify.ts` refuses to
run against a wrong oracle via the startup behavioral fingerprint `runOracleFingerprint` (const
`ORACLE_FINGERPRINT`): a handful of pinned probe inputs whose accept/reject exits are unique to the
local-fixes fixes, checked before the probe loop on every path (normal probe, `--mint-decode-foreign`,
`--smoke`). A wrong-branch rebuild — or a stock `cargo install cddl` — therefore fails loudly at startup
(`HARNESS FAILURE`) in under a second instead of minting mixed evidence. The fingerprint runs at STARTUP
only, so **pin the oracle to an immutable copy for a long run**: a rebuild MID-run still swaps the oracle
silently (that default path is an ACTIVE development tree), which is exactly what the `cp`-the-binary-
somewhere-immutable-and-point-`RUST_CDDL`-there practice prevents.

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
6. **bignint-keyed map validation over-rejection** (OPEN at `local-fixes` @ `2c7548e`, the pinned
   rev): `validate_cbor_from_slice` rejects EVERY map whose KEY domain is the prelude `bignint`
   ("expected object value of type bignint, got object") — empty or not, spec-valid entries or not,
   `.cbor`-wrapped or bare; bignint VALUES validate fine, so the gap is specific to the key-domain
   position. No matrix row sits on it (no bignint-key-domain cell exists), but it blinds the
   generated-crate conformance oracle for `tests/corpus/cbor_bignint_table.cddl`, which therefore
   rides `ir_conformance_corpus`'s `RUST_ORACLE_SKIP` (its ruby half is separately unjudgeable —
   the gem's inline-composite controller parse gap — so the decode-side reference-codec
   differential is its structural check). Differential repro + prune steps:
   `draft/rust-cddl-bignint-key-validator-gap.md`.
7. **prelude `number` rejects a float** (OPEN at `2c7548e`, NOT fork-fixed): `validate` rejects every
   CBOR float against the built-in `number` type (`expected type number, got Float(…)`), while ints
   against `number` — and floats against the equivalent inline `int / float` or a user alias — validate
   fine. Root cause is a one-line asymmetry: `is_ident_float_data_type` omits `Token::NUMBER` where its
   sibling `is_ident_integer_data_type` includes it (`number = int / float` accepts both). TWO
   consumers sit on this gap: the decode-conformance **arm-coverage floor** (§ "Decode-direction
   conformance" in `tests/README.md`) — `prelude.number`'s float arm cannot be minted through the
   two-oracle accept gate, so its class is carried in `DECODE_FLOOR_ARM_EXEMPT` (`lib.ts`), the floor's
   stale-guarded exemption ledger — and the oracle fingerprint's `prelude-number-float-rejects` probe
   (`ORACLE_FINGERPRINT`, `verify.ts`), which deliberately pins the gap OPEN so an oracle where it is
   silently fixed cannot mint against the stale exemption. Both flip together via the close-out steps
   in `ROADMAP.md` § findings (fingerprint probe first — a fixed oracle is refused at startup until
   it is consciously re-pinned). Repro + the one-line fork-fix + prune steps:
   `draft/rust-cddl-number-float-gap.md`.

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
  (integration-tested instead). `prelude.any` is correctly ➖: `x = any` exits 0 but emits
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
  `unverified`) instead of hiding the hole. The seed instance is the no-occurrence type-domain arrow
  widening (`contain.map-key.memberkey.type1.tstr_arrow_nooccur` — `{ tstr => uint }` table-detected
  to 0..N, its empty-map instance `8200a0` wrongly accepted; § "Directional support evidence (Q4)",
  ROADMAP § findings). When the fix lands the vector is promoted to `class="constraint"` (+ `expect_err`)
  and the row moves to Q4's enforce-green pin.
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
  (its five released-CLI gaps fail the pinned probes by design), so pointing `RUST_CDDL` at
  `~/.cargo/bin/cddl` no longer produces a degraded-but-workable run — supply the `local-fixes` @
  `2c7548e` build (or an immutable copy of it) instead. Its generated-crate compile gate reuses
  `integration_tests::feature_corpus_compiles`' shared-target pattern (one-time dep warm-up).

## Scope (v1)
RFC 8610 backbone in its authoritative current form: 9682 grammar + 8610 prelude + the IANA control-op
registry (which already spans 8610/9090/9165/9741). Encoding axis from RFC 8949. CDDL-modules / future
drafts are out of v1.
