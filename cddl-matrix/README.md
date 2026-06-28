# CDDL master matrix (canonical feature space)

A machine-readable, **implementation-agnostic** enumeration of the CDDL feature space, anchored to the
spec's own published artifacts. It is the single source of truth that other matrices *project from*:
`tests/corpus/COVERAGE.md` is the projection onto the **feature** axis; `tests/golden_hex/COVERAGE.md`
is the projection onto the **encoding** axis. Per-tool support (cddl-codegen's, or any other consumer's)
lives in `annotations/`, keyed by id — so the master itself stays pure spec and reusable.

> **Start at [`OVERVIEW.md`](OVERVIEW.md)** (entry point: vision, architecture, current state) and
> [`ROADMAP.md`](ROADMAP.md) (what's left). This file is the original model write-up, kept as deeper
> reference. **The matrix has since been fully scaled** (81 features across all axes, gate-verified) —
> any "seed / only `type2` worked" framing below this line is historical, superseded by OVERVIEW.

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

`verify.py` reconciles the authored features against that union in **both directions** (see "provably
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
| `containment/*.toml` | `role × feature → spec-allowed?` — **where nesting/variation gaps live** | nesting (C) |
| `encodings.toml` | CBOR major-type × form grid (RFC 8949); **legality is major-type-dependent** (e.g. ints have no indefinite form), not a free orthogonal axis | encoding (D) |
| `annotations/<tool>.toml` | per-consumer support, keyed by master id (NOT part of the spec master) | — |

**3. Generated view — `matrix.json`** (produced by `build_matrix.py`) joins the overlay with the native
sources into one universal artifact for downstream/cross-language consumption. Imported axes are
*derived*, not authored: control operators come straight from the IANA CSV with ids `ctl.<name>`. It is
regenerated, never hand-edited. `build_matrix.py` also runs the **drift-check** (every annotation id must
resolve to a real master id). The script is a PROTOTYPE; the production form is a Rust test wired into the
suite (serde `toml` + `serde_json`, no Python dependency).

Why this shape: the (construct × variation × nesting × encoding) cross-product is intractable and mostly
meaningless. Normalizing keeps each fact in one place — e.g. "tags are supported but **nested** tags are
not" is not a bespoke row; it's `containment(tag-content, type2.tag)` with a cddl-codegen annotation of
`unsupported`. The combinatorial concern collapses into one relation.

## Why it's *provably* comprehensive (not just "we tried hard")

Comprehensiveness is established by a **bidirectional lint** against the first-party sources (the ABNF
is a lint, not the closed-world spine — see "What is a feature?"):
- **forward** (source → feature): every **`type2` ABNF alternative** has ≥1 covering feature row —
  completeness checkable against the ABNF in `sources/`, hard-gated in `verify.py` (this is the check
  that caught the missing `#7` alternative; per-alternative coverage for the other productions is
  computed and logged best-effort);
- **backward** (feature → source): every feature's `production` resolves to a real ABNF production, the
  `prelude` pseudo-production, or the IANA control-op registry — so no feature is invented with no
  source (hard-gated in `verify.py`);
- cross-checked against a parser's AST variants (the `cddl` crate's `Type2`/`Type1`/`MemberKey`/… enums
  mirror the productions — a second, weaker corroboration, since the AST is one reading of the same grammar);
- **control operators** are derived from the **IANA registry** (authoritative & cross-RFC), so that axis is
  complete by construction and stays current as new RFCs register operators.

## Evidence/id convention

`id`s and evidence use **spec anchors (production / RFC section) and grep-able code anchors, never line
numbers** — same robustness rule as the COVERAGE.md docs.

## Conventions (decided)
- **Home:** top-level `cddl-matrix/` (reusable beyond this repo's tests, hence not under `tests/`).
- **Format:** TOML authored overlay + generated `matrix.json`; native sources pinned & committed under `sources/`.
- **Verification:** `build_matrix.py --check` snapshots the synthesis (fails if `matrix.json` is stale
  vs the authored overlay — the editorial join under test, not just the inputs' checksums); `verify.py`
  is the reproducible gate (reconcile + triangulated probes). Both to be reimplemented as Rust CI tests.

## Scope (v1)
RFC 8610 backbone in its authoritative current form: 9682 grammar + 8610 prelude + the IANA control-op
registry (which already spans 8610/9090/9165/9741). Encoding axis from RFC 8949. CDDL-modules / future
drafts are out of v1.
