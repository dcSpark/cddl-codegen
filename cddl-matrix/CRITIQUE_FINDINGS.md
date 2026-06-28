# Master-matrix critique findings (independent cold review)

## Provenance / why this exists
Every prior review of this matrix had our codebase + rationale in front of it, so it could only check
"is the chosen approach executed well?" — not "is the approach right?" To debias, a fresh agent was
given **only** an anonymized problem statement + the authoritative specs (CDDL/CBOR RFCs, grammar,
prelude, IANA registry, encoding examples) in an isolated directory — **no access to this codebase** —
and asked to be adversarial. This file records its findings and our merit-grading of each, so scope
decisions are made deliberately.

## Verdict
The headline ("the load-bearing decision — anchoring the feature axis to the ABNF grammar — is a
fundamental flaw") is **about half right, and the valuable half is real.** It is softened by what the
cold reviewer could not see: our *implementation* already deviates from the strawman it attacks (we
exclude lexical productions like `S`/`WS`/`CRLF`, we split `occur` into `?`/`*`/`+`/`n*m`, and we import
the prelude + control-op registry). But that gap is itself a finding: our **stated principle**
("grammar-anchored, one row per production alternative") never matched our **real** individuation, and
we never defined "construct." Several specific gaps it found we genuinely do **not** handle.

Crucially, the reviewer's own closing reaffirms the machinery is sound — the corrections below are
**additive, not a rebuild** (see "Reaffirmed sound").

## Implementation status (this session)
- **F7 — DONE** (`QUERIES.md`): the consumer-query/success contract. It also resolved scope: F8/F9 map
  only to deferred/stretch queries, so they're out of v1 *without* needing a probe; F10/F11 note-only.
- **F1 — DONE**: per-feature `profile` axis, refined to mean **capability-introducing RFC** (not
  grammar line). 80 RFC8610 / 1 RFC9682; `verify.py` classifies `out_of_profile` (1: `type2.tag_head_type`).
- **F2 — DONE**: "what is a feature?" defined (serialization-relevant semantic distinction); ABNF
  demoted to a **bidirectional** completeness lint in `verify.py` + `README.md`.
- **F6 — DONE**: `build_matrix.py --check` snapshots the synthesis (fails if `matrix.json` is stale vs
  the authored overlay) — the editorial join is now under test, not just the inputs' checksums.
- **F3 — DEFERRED** (not premature scaffolding): a v1 split could only populate `accept`; the other
  four facts (encode/decode/round-trip/enforce) need per-feature *execution* we're not building yet.
  Do it when execution/corpus wiring lands; Q4 is blocked until then.
- **F4 — DONE (by decision, not enumeration)**: cddl-codegen is **tag-parametric** (`#6.n(T)` is one
  feature `type2.tag` regardless of `n`); the codegen-distinct tags are already prelude features
  (`tdate`/`biguint`/`encoded-cbor`/`uri`/…). So the tag axis is fully modeled; the IANA CBOR-tags
  registry is referenced (URL) but deliberately **not pinned/enumerated** (large, churning, nothing
  derived from it). Rationale recorded in `README.md`. (The critic's tag-explosion concern reasoned
  about a *generic* codegen; it's moot for this tool.)
- **F5 — DONE (already dependency-correct; documented)**: the over-count concern is moot — `encodings.toml`
  already carries **major-type-dependent `forms`** (`enc.major0`/int has no indefinite form; `enc.major2`/
  `enc.major4` do), and no feature links an int to an indefinite form. So encoding legality is
  `feature → major-type → forms` (no impossible cells), with specific-cell links where a construct is
  restricted (e.g. `float16` → `enc.major7.float16`). The link-integrity check (F6 area) guards the
  links. `appendix_a.json` is treated as a **lossy seed corpus**, not a coverage oracle. The Q3
  enumeration (legal-cells-minus-covered) is a *query-time expansion* of this data — built when the
  golden-vector projection exists; building it now would be scaffolding-for-later (YAGNI).
- **F8 / F9 / F10 / F11 — OUT of v1** (per F7's query map).

Gate state after this session: PASS — 53 supported / 27 unsupported / 1 out_of_profile / 0 uncertain;
reconcile clean (bidirectional); link-integrity + per-alternative + snapshot guards active; deterministic.

## Grading legend
- **VALID-UNHANDLED** — real, and we don't address it. Act on it.
- **PARTIAL** — real principle, but our implementation already covers part of it; the correction is to
  make it explicit / close the residual.
- **JUDGMENT** — a defensible nuance or a call that depends on our purpose; note, decide case-by-case.

---

## Findings

### F1 — No language version/profile axis  ·  VALID-UNHANDLED  ·  **P1**
"Pure spec" is not a fixed point: base RFC 8610, the RFC 9682 *update* grammar, and the extension RFCs
(9090/9165/9741 operators) are **different feature sets**. Without an explicit target-version/profile,
"out-of-profile" is conflated with "unsupported."
- **Evidence (already bit us):** `type2.tag_head_type` (`#6.<type>` head-number) is a **9682 addition,
  absent from base 8610**; we recorded it `unsupported`, but relative to cddl-codegen's ~8610 target it
  is arguably *out-of-profile*. `cddl.abnf` vs `cddl-1-1-update.abnf` also differ on `#7` splitting,
  string-literal escapes (`SESC`/`text`/`bytes`), and empty-`cddl`.
- **Correction:** make profile a first-class column on features (which RFC/grammar introduces it); let
  per-tool overlays distinguish *out-of-profile* from *unsupported*. State the master's target profile.

### F2 — "Construct" undefined; grammar should be a lint, not the spine  ·  PARTIAL  ·  **P1**
The unit "construct" is never defined; the grammar is simultaneously too coarse where codegen needs
resolution (one `type2` alt covers the whole tag registry; the spine collapses all control operators
into `ctlop = "." id`) and too fine where it has no meaning (lexical productions).
- **What we already do:** exclude lexical productions; split `occur`; cover common constructs via the
  prelude + registry — i.e. we already individuate semantically, ad-hoc.
- **Correction:** define "construct" as a **serialization-relevant semantic distinction**; demote the
  ABNF to a **bidirectional completeness lint** (every alternative maps to ≥1 feature *and* every
  feature maps to ≥1 production/registry/prelude entry). Keeps provability with the right polarity.

### F3 — "Support" is not one bit  ·  VALID-UNHANDLED  ·  **P2**
parse / encode / decode / round-trip / **constraint-enforcement** (`.size`, cuts, ranges) are distinct
coverages; our exit-code probe collapses them. Directionality is real: `appendix_a.json` `roundtrip:false`
cases (indefinite length) are *decode-accept-but-encode-never*.
- **Correction:** split the support/coverage signal into at least {accept, encode, decode, round-trip,
  enforce}; record directionality. (Sharper version of our own "probe is coarse" note.)

### F4 — CBOR tag registry not pinned / tags not individuated  ·  PARTIAL  ·  **P2**
We pinned the control-op registry but tags hide behind one `type2.tag` row, though tag 0/2/24/32 are
different codegen tasks (the same logic that made us pin the op registry applies to the tag registry).
- **Mitigation already present:** common tags ARE individuated via prelude (`tdate=#6.0`,
  `biguint=#6.2`, `bignint=#6.3`, …).
- **Correction:** pin the IANA CBOR tag registry; individuate at least the codegen-distinct tag classes
  (time, bignum, embedded-CBOR, generic/other).

### F5 — Encoding axis is *dependent*, not orthogonal; `appendix_a.json` is a value seed  ·  PARTIAL  ·  **P2**
Indefinite-length framing applies only to major types 2–5; float widths only to mt7; integer minimality
only to ints — so encoding legality is a dependent product over the construct, not a free axis. And
`appendix_a.json` enumerates CBOR *values*, not CDDL *constructs* (no entry for "tag wrapping a
`.size`'d bstr," "map with a cut key," etc.).
- **What we already do:** `encodings.toml` lists forms *per major type* (captures part of the dependency).
- **Correction:** give the encoding axis its own legality/containment relation; treat `appendix_a.json`
  as a lossy seed corpus with known holes, not a coverage oracle. Note `.size`/`.cbor`/`.det` couple
  encoding back into the operator table.

### F6 — Checksums secure the inputs, not the synthesis  ·  VALID-UNHANDLED  ·  **P1 (cheap)**
`sources/` is checksum-pinned, but the **editorial join** (authored feature/containment tables, the
mapping of prelude/registry/encoding onto rows) is the hand-written, error-prone part where "downstream
inherits any error" actually bites — and it is unpinned.
- **Correction:** snapshot + diff the generated tables (apply the same insta discipline this repo uses
  on generated source). Put the riskiest artifact — the join — under test.

### F7 — No consumer-query / success contract  ·  VALID-UNHANDLED  ·  **P2**
The brief never states the concrete questions a downstream concern asks the master, or what makes it
"done" — which is exactly the test of whether the chosen granularity is adequate.
- **Correction:** write the target queries (e.g. "list constructs decoded but not canonically
  encodable"; "every construct unsupported by tool X but in-profile"); use them to validate granularity
  *before* investing further.

### F8 — Matching semantics unmodeled  ·  JUDGMENT  ·  **P3**
Prioritized-choice ordering (`a / a b` ≠ `a b / a`), greedy occurrence, and the `:`-shortcut's implicit
cut (RFC 8610 §3.5.4) are codegen-relevant and invisible in our model. *Open question:* how much
cddl-codegen's serialization actually depends on these vs. them mattering mainly for validation — check
before investing.

### F9 — Binary containment under-models deep interactions  ·  JUDGMENT  ·  **P3**
`feature × role` boolean can't express "A inside B inside role C with operator D," where real generator
bugs live. Fix is a small set of known-hazardous interaction tuples (not a return to the cross-product).

### F10 — "Single parser rejects ⇒ parser limitation" = over-acceptance bias  ·  JUDGMENT  ·  note
Pushes the feature set toward the grammar's superset that no tool implements. For *our* purpose ("does
tool X support this?"), recording spec-valid-but-unsupported is honest; the concern only bites if the
grammar superset becomes the coverage *denominator*. Decide what the denominator is.

### F11 — AST cross-check is weak (shared common cause)  ·  JUDGMENT  ·  note
A parser's AST is one reading of the *same* grammar, so it isn't an independent enumeration and will
diverge exactly on the grammar's ambiguous/disambiguation-by-prose cases. Treat AST agreement as weak
corroboration, not robustness.

---

## Reaffirmed sound (after a genuine attempt to break it)
- Spec/impl separation (pure-spec master + id-keyed per-tool overlays) — *needs the F1 version axis bolted on.*
- Pinning + refetch + drift-check — good hygiene (F6 only adds: also pin the synthesis).
- Surfacing disagreements as "uncertain" rather than auto-resolving — honest; just track the bucket's content.
- Importing the control-op registry verbatim — exactly right (canonical open set); F4 says apply the same to tags.
- Avoiding the full nesting cross-product — correct; F9 only enriches it.

## Open decisions for the maintainer (→ scope discussion)
1. **Adopt F1 (version/profile axis)?** Highest-leverage, foundational. (Recommend yes.)
2. **Adopt F2 (semantic-individuation principle + grammar-as-lint)?** Makes explicit what we do ad-hoc. (Recommend yes.)
3. **Adopt F6 (snapshot the synthesis)?** Cheap, on-brand correctness win. (Recommend yes.)
4. **F3 / F4 / F5 / F7** — real but larger; do now, defer, or note?
5. **F8 / F9 / F10 / F11** — judgment calls; which (if any) do you buy?
