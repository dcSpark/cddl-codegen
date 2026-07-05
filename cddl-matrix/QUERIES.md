# Consumer queries & success contract (the granularity forcing-function)

The master matrix is not an end in itself — it exists to answer specific questions for downstream
concerns (generator support, test coverage, wire-encoding coverage, other tools). This file states
those questions **concretely**. Their purpose is twofold:
1. **Definition of done** — the matrix is "good enough" when it can answer Q1–Q6 mechanically from
   `matrix.json` + annotations.
2. **Granularity test** — each query *dictates* the resolution the schema must carry. A critique finding
   (ledger in `ROADMAP.md`) is **load-bearing iff some query needs it**; if no query needs it, it's
   optional. This is how we avoid both over- and under-investing.

## The queries the master must answer

### Q1 — Actionable support gaps for a tool
"List every construct that tool X does **not** support **but that is in X's target CDDL profile**."
- The everyday "what should we implement next" / "what's our real gap" question.
- **Requires:** per-tool support annotations (have) **+ a profile axis** so out-of-profile features are
  excluded from "gaps." → **needs F1.** Without F1 this query returns false positives (e.g. RFC 9682
  features against an RFC 8610-targeted tool).
- **Answered by `query_q1_gaps.ts`**, a pure `matrix.json` read: default run prints the in-profile gaps
  grouped by axis (feature / control-op / contextual containment-cell) with each construct's example +
  a human note, plus the inverse supported summary and the out-of-profile rows bucketed separately (the
  F1 profile split). A positional arg filters by id substring (the "for tool X, for construct C" form).
  `--write` regenerates the marker-delimited `## Limitations` section of
  `docs/docs/current_capacities.mdx`; `--check` runs the drift gate (in-memory regenerate + byte-compare
  the block) plus consistency invariants and a vacuity floor (`check.ts` local tier).

### Q2 — Test-coverage gaps
"List every construct tool X supports that **no test exercises**." (This is the `tests/corpus` coverage
projection.)
- **Requires:** support annotation (have) + a test-coverage annotation keyed by the same feature id.
- Granularity must match how tests are written (per construct/variation), which is what F2 formalizes.

### Q3 — Wire-encoding coverage gaps
"For each construct, list the CBOR wire-encodings it can legally take that **no golden vector covers**."
(The `golden_hex` projection.)
- **Requires:** the encoding axis as a **per-construct legality relation**, not a free orthogonal axis
  (an int has no indefinite-length form; only mt2–5 do). → **needs F5.** Without F5 this query lists
  impossible cells (int × indefinite) and hides real gaps.

### Q4 — Directional / enforcement support for a construct
"For construct C and tool X, report support across {accept, encode, decode, round-trip,
enforce-constraint}."
- E.g. indefinite-length is commonly *decode-accept but encode-never*; `.size`/cut *enforcement* is
  where generators cut corners.
- **Requires:** the support signal split into ≥5 directional facts, not one bit. → **needs F3.** A
  single accept/reject/crash bit cannot answer this. (F3 is grounded — round-trip + reject execution
  gates, decode-direction vectors via the decode-conformance harness, encode via the conformance
  oracles; see `README.md` § "Directional support evidence".) **Answered by `query_q4_directional.ts`**, which projects those
  per-direction evidence bits into the 5-way answer from `matrix.json` + `catalog.toml` (pure file
  reads): default run prints the table grouped by axis (positional arg filters by id for the "for
  construct C" form); `--check` runs the consistency invariants + vacuity floor as a gate. It reports
  encode honestly as the round-trip half (no independent per-construct encode oracle) while decode
  carries its independent per-construct foreign vectors — the asymmetry Q4 exists to expose.

### Q5 — Matrix self-completeness (per profile)
"List every construct defined by CDDL profile P (grammar ∪ prelude ∪ control-op registry) that the
matrix does **not** model."
- The provable-comprehensiveness check.
- **Requires:** reconciliation against all first-party sources **with the grammar as a bidirectional
  lint** (every production alt → ≥1 feature, every feature → ≥1 source entry). → **needs F2.** (Tags
  are covered by the F4 decision — modeled parametrically + via prelude-named tags — so the CBOR tag
  registry is *referenced*, not pinned/enumerated as a source.)

### Q6 — Profile / version diff
"What changed — in the feature set or in tool X's support — when moving from CDDL profile P to P+1, or
from tool version V to V+1?"
- **Requires:** the profile axis (F1) **+ a pinned, diffable snapshot of the synthesised matrix** so a
  change is a reviewable diff. → **needs F1 + F6.**

### Q7 (stretch) — Hazardous nesting interactions
"For construct C, list known-hazardous compositions (C inside D inside role R with operator E) and tool
X's support for them."
- **Requires:** a small interaction-tuple relation beyond binary containment. → **needs F9.** Marked
  stretch: only worth it once Q1–Q4 are answerable and real interaction bugs are observed.

## What this tells us about scope (findings → load-bearing?)

| finding | needed by | load-bearing? |
|---------|-----------|---------------|
| **F1** profile axis | Q1, Q6 | **Yes** — Q1 (the everyday gap query) is wrong without it |
| **F2** semantic individuation + grammar-as-lint | Q2, Q5 | **Yes** — Q5 (completeness) needs the bidirectional lint |
| **F3** directional/enforcement support split | Q4 | **Yes** — but only to the depth Q4 names (5 facts) |
| **F4** tag modeling | Q5 | **Resolved by decision** — tags modeled parametrically (`type2.tag`) + via prelude-named tags; CBOR tag registry *referenced*, not pinned/enumerated (cddl-codegen is tag-parametric) |
| **F5** encoding legality | Q3 | **Yes** — Q3 is meaningless without per-construct encoding legality |
| **F6** snapshot the synthesis | Q6 | **Yes** — and it's cheap; do it last (after schema settles) |
| **F7** this contract | — | done (this file) |
| **F8** matching semantics | *no query above needs it* | **Deferred** — no consumer query requires prioritized-choice/cut modeling yet; revisit only if a real deserialize-disambiguation need surfaces |
| **F9** interaction tuples | Q7 (stretch) | **Deferred** — stretch query only |
| **F10/F11** | — | **Note-only** — no query affected |

## Definition of done
The matrix is "done (v1)" when Q1–Q6 are each answerable by a small script over `matrix.json` +
annotations, with: an explicit target profile per feature (F1), bidirectional grammar reconciliation +
tags (F2/F4), a per-construct encoding legality relation (F5), a ≥5-way directional support signal (F3),
and a pinned/diffable synthesised snapshot (F6). F8/F9/F10/F11 are explicitly out of v1 scope (ledger
in `ROADMAP.md`).
