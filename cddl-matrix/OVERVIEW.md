# CDDL master matrix — overview (start here)

This directory is a **canonical, machine-readable, implementation-agnostic enumeration of the CDDL
feature space**. It is *not* test code; it is a reference artifact that test/coverage/support tooling
**projects from**. New here? Read this file, then `ROADMAP.md` for what's left. Everything else is
deeper reference (see the reading guide at the bottom).

## Problem

`cddl-codegen` generates Rust serialize/deserialize code from CDDL schemas (CDDL = RFC 8610, a
data-definition language for CBOR/JSON). To reason about *coverage* and *support* we kept asking the
same question in different shapes — "which CDDL constructs does the generator support? which are
tested? which CBOR wire-encodings are exercised?" — and answering each ad-hoc, against our own informal
feature list. That list was circular (it only described what we already supported) and had no way to
show what we *don't* support. We needed one authoritative enumeration of *everything CDDL can express*,
that multiple concerns can derive from.

## Vision — one source of truth, many projections

**"The master"** = the authored spec-truth overlay (`features/`, `roles.toml`, `containment/`,
`encodings.toml`) plus the control-operator axis derived from the IANA registry; **`matrix.json`** is
its *generated serialization* (never hand-edited). Specific concerns are **projections** — joins onto
the master by feature id:
- **generator support** — captured today in `annotations/cddl_codegen.toml`, an *authored per-tool
  overlay* (the input); the joined *support view* is the projection.
- **input-construct test coverage** (`tests/corpus/COVERAGE.md`) and **CBOR wire-encoding coverage**
  (`tests/golden_hex/COVERAGE.md`) — these docs exist but are **not yet generated from the master**
  (still hand-maintained, so they can drift; wiring them is `ROADMAP.md` item 1).
- any future tool/consumer — add an `annotations/<tool>.toml`.

It is built to answer concrete questions (full list + "definition of done" in `QUERIES.md`):
Q1 *what does tool X not support that's in its profile* · Q2 *what's supported but untested* ·
Q3 *which wire-encodings are uncovered* · Q4 *directional support (accept/encode/decode/round-trip/
enforce)* · Q5 *what does the matrix itself fail to model* · Q6 *what changed across a profile/version
bump*. Those queries are the **granularity forcing-function**: a design decision is load-bearing only
if some query needs it.

## Architecture — three layers

1. **Native sources (`sources/`), pinned verbatim, never transcribed.** The authoritative upstream
   artifacts — the ABNF grammar (`cddl.abnf` = RFC 8610, `cddl-1-1-update.abnf` = RFC 9682), the
   standard prelude (`cddl.prelude`), the IANA control-operator registry (`cddl-control-operators.csv`),
   CBOR encoding examples (`appendix_a.json`) — kept byte-identical, checksummed (`SHA256SUMS`), and
   re-fetchable (`fetch.sh`). Provenance in `sources/MANIFEST.md`.
2. **Authored overlay (TOML)** — the parts that exist in no upstream artifact: `features/*.toml`
   (constructs), `roles.toml` (nesting contexts), `containment/*.toml` (`role × feature → allowed?`),
   `encodings.toml` (CBOR forms), and `annotations/<tool>.toml` (per-tool support, keyed by feature id).
3. **Generated view — `matrix.json`** (by `build_matrix.py`): joins the overlay with the sources
   (control operators are *derived* from the IANA CSV, not authored). Regenerated, never hand-edited.

Modeled as a small **normalized relational schema** rather than one giant table, so the
(construct × variation × nesting × encoding) cross-product never materializes — e.g. "tags supported
but nested tags not" is one `containment` row, not a bespoke feature.

## Key decisions (and why)

- **A "feature" is a serialization-relevant *semantic distinction*, not a grammar production.** Some
  productions are too coarse for codegen, some are pure lexical noise. The ABNF is therefore a
  **bidirectional completeness lint** (forward: every grammar alternative is covered by a feature;
  backward: every feature resolves to a real source), **not the spine.** The real feature space is
  *grammar ∪ prelude ∪ control-op registry*.
- **`profile` = the RFC that introduced the *capability*, not the grammar line.** A pure grammar
  refactor (RFC 9682 split `#7` into its own production, but `#7.n` was always expressible) keeps the
  older profile. This lets us distinguish *out-of-profile* (a tool rejects a construct only because it
  post-dates the tool's target) from a true *unsupported* gap.
- **Correctness by triangulated oracles, never one implementation.** Spec-validity is decided by the
  ABNF (authority) + the spec-author's reference parser (`cddl` Ruby gem); a third-party Rust parser
  only corroborates (a rust-only rejection is a recorded *parser limitation*, not invalidity).
  Tool-support is **execution-grounded** — probed by *running* cddl-codegen per feature, not reading
  its code. Disagreements are surfaced as `uncertain`, never auto-resolved.
- **Tags are modeled parametrically.** cddl-codegen wraps/unwraps `#6.n(T)` uniformly for any `n`
  (it doesn't implement per-tag semantics), so `type2.tag` is one parametric feature; the
  codegen-distinct tags are already prelude features (`tdate`, `biguint`, …). The IANA tag registry is
  referenced, not pinned/enumerated.
- **The encoding axis is major-type-dependent, not orthogonal.** `encodings.toml` carries forms per
  major type (an int has no indefinite-length form; an array does), so impossible cells (int ×
  indefinite) can't be expressed.
- **Reproducibility covers the synthesis, not just the inputs.** Sources are checksum-pinned, *and*
  `build_matrix.py --check` fails if `matrix.json` is stale vs the authored overlay (the editorial
  join is under test — the same snapshot discipline the `insta` crate applies to generated Rust source
  elsewhere in this repo).

## What works today (verified, reproducible)

Run `python3 build_matrix.py && python3 verify.py` (and `build_matrix.py --check`). Current gate: **PASS**.
- **81 feature rows** (prelude **40/40**; `type2` **12/12** ABNF alternatives) **+ 37 control-operator
  rows** (a separate axis derived from the IANA registry — *not* among the 81) + **9 roles**,
  **32 containment** cells, **54 encodings**, **81 annotations** (one per feature).
- Support (execution-grounded, one annotation per feature): **53 supported / 27 unsupported / 1
  out-of-profile / 0 uncertain.**
- Reconcile (bidirectional lint): backward clean (`fabricated=0` — every feature traces to a source),
  prelude `gaps=0`, `link-integrity=0`; snapshot `--check` clean; byte-identical across runs.
- **Forward completeness is hard-gated for `type2` only** (12/12 alternatives). Other productions are
  checked best-effort, so the gate prints some `[soft] … uncovered` / `NOT MODELED` lines — these are
  1-to-many matcher mismatches (the construct *is* modeled), **not real holes** — the textual matcher
  just can't align a 1-to-many production→feature mapping.
- **26 of the 32 containment** cells are oracle-corroborated (0 contradictions); the other 6 are
  spec-declared only.
- One recorded, non-fatal **rust parser limitation** (`value.bytes` — lowercase hex; reference + ABNF
  accept it).

The matrix has also been through an **independent cold critique** (a fresh agent given only the problem
+ specs, no codebase); its findings are addressed or explicitly deferred — see the findings ledger in
`ROADMAP.md` (the full critique write-up is in git history).

## Conventions & scope

- **Evidence/ids** use stable spec anchors (production / RFC section) and grep-able code anchors —
  **never line numbers** (same rule as the `COVERAGE.md` docs).
- **Format:** TOML authored overlay + generated `matrix.json`; native sources pinned & committed under
  `sources/`. `build_matrix.py` / `verify.py` are Python prototypes — to be reimplemented as a Rust CI
  test (see `ROADMAP.md`).
- **Scope (v1):** the RFC 8610 backbone in its authoritative form — RFC 9682 grammar + RFC 8610 prelude
  + the IANA control-op registry (spanning RFC 8610/9090/9165/9741) + RFC 8949 encodings. CDDL-modules
  and future drafts are out of v1.

## Glossary & how to run

**Where this lives.** `cddl-matrix/` is at the repo top level (reusable beyond this repo's tests).
Paths under `sources/`, `features/`, `annotations/` are local to it; paths like
`tests/corpus/COVERAGE.md` belong to the consuming `cddl-codegen` repo.

**Local terms** (several are printed by the `verify.py` gate):
- **role** — a *nesting context* a construct can occupy (e.g. `array-element`, `map-key`,
  `tag-content`); 9 in `roles.toml`.
- **containment** — a `role × feature → allowed?` fact (e.g. *a tag inside a tag* is one row). A
  *contradiction* = the spec oracle's observed allow/disallow disagrees with the declared value.
- **hard-gated** — a check whose failure fails the gate (vs **soft** = logged only).
- **fabricated** — a feature whose `production` traces to no real source (backward-lint failure).
- **gaps** — a prelude type with no covering feature (forward-lint failure).
- **link-integrity** — a `feature.encodings`/`roles` reference resolving to no real id.
- **the master** — the authored overlay (spec-truth); `matrix.json` is its generated serialization.

**To run the gate** you need three oracles (exact paths in the `verify.py` header): the spec-author's
Ruby `cddl` gem (reference validity), a built Rust `cddl` CLI (corroborating validity), and a built
`cddl-codegen` (support probe). Then `python3 build_matrix.py && python3 verify.py`.

**To add a feature**, copy a `[[feature]]` from `features/type2.toml` (the schema template): `id`,
`production`, `alt`, `rfc`, `profile`, `title`, `desc`, `example` (+ optional `roles`/`encodings`). The
`example` must be valid CDDL — it is parsed and probed. Then regenerate and re-verify.

## Reading guide

| you want… | read |
|-----------|------|
| this — vision, architecture, current state | `README.md` (this file) |
| what's left to do + the critique findings ledger | `ROADMAP.md` |
| what the matrix must answer + definition-of-done | `QUERIES.md` |
| source provenance / how to refresh specs | `sources/MANIFEST.md` |
| the build + the verification gate | `build_matrix.py`, `verify.py` |
