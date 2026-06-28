# CDDL master matrix — scale report

Generated after the scale step (`python3 build_matrix.py`) and the mechanical verify gate
(`python3 verify.py`). The build/drift-check and the verify probes are reproducible and **both pass**;
this report is honest about what is mechanically verified vs only flagged. After the cleanup-and-
reverify pass (ABNF-authority semantics + two new checker capabilities) and the F1/F2 corrections
(a per-feature **language-profile axis** and a **bidirectional** grammar lint), the UNCERTAIN list is
**empty**; **1 feature is `out_of_profile`** (`type2.tag_head_type`, the RFC 9682 type-valued tag
head-number — a genuine new capability outside cddl-codegen's RFC 8610 target) and one informational
rust parser-limitation remains (non-fatal).

The gate now runs under **ABNF-authority** semantics (RFC 9682 ABNF is the grammar authority; the
reference *ruby* parser is authoritative for example validity; the rust `cddl` crate is corroboration
only). A construct the ABNF/reference permits but the rust crate rejects is a recorded **parser
limitation**, not a spec-invalidity and not a hard fail.

## 1. Coverage per axis

### Features (grammar + prelude) — 81 rows

By ABNF production (`features/*.toml`):

| production | rows | file |
|------------|-----:|------|
| `prelude`    | 40 | `features/prelude.toml` |
| `type2`      | 14 | `features/type2.toml`, `features/generics.toml` |
| `memberkey`  | 4  | `features/composition.toml` |
| `occur`      | 4  | `features/composition.toml` |
| `value`      | 3  | `features/type2.toml` |
| `grpent`     | 3  | `features/composition.toml` |
| `genericarg` | 2  | `features/generics.toml` |
| `rule`       | 2  | `features/generics.toml` |
| `rangeop`    | 2  | `features/type_and_ranges.toml` |
| `genericparm`| 1  | `features/generics.toml` |
| `type`       | 1  | `features/type_and_ranges.toml` |
| `group`      | 1  | `features/composition.toml` |
| `grpchoice`  | 1  | `features/composition.toml` |
| `type1`      | 1  | `features/type_and_ranges.toml` |
| `assignt`    | 1  | `features/type_and_ranges.toml` |
| `assigng`    | 1  | `features/type_and_ranges.toml` |
| **total**    | **81** | |

`verify.py` reconciles these against `sources/cddl-1-1-update.abnf` (55 ABNF productions seen) and
`sources/cddl.prelude`. Every `feature.production` is a real ABNF production (or the recognized
`prelude` pseudo-production): **0 fabricated**.

### Prelude — 40/40 (complete)

Exactly one `prelude.<name>` feature per standard prelude type (RFC 8610 App. D). The reconcile spine
confirms **0 missing** and **0 orphan** prelude features. Every example is VALID by the reference
(ruby) oracle.

### Control operators — 37/37 (complete by construction)

Derived directly from `sources/cddl-control-operators.csv` (IANA registry) with ids `ctl.<name>`;
**not authored**. Spans RFC 8610 / 9090 / 9165 / 9741. Complete and current by construction.

### Encodings — 54 rows (CBOR major-type × length/ai-form grid, RFC 8949)

9 PARENT rows (`enc.major0..7`, `enc.major7.float`) + 45 per-cell rows. Each cell cites its head byte
and, where available, a concrete decoded vector from `sources/appendix_a.json`.

### Containment (role × feature → spec-allowed) — 32 cells, 9 roles

| role | cells |
|------|------:|
| `role.array-element` | 7 |
| `role.map-value` | 5 |
| `role.cbor-payload` | 4 |
| `role.tag-content` | 4 |
| `role.choice-member` | 3 |
| `role.map-key` | 3 |
| `role.occurrence-target` | 3 |
| `role.generic-arg` | 2 |
| `role.top-level` | 1 |
| **total** | **32** |

26 of these carry isolating examples corroborated by the spec oracles: **0 contradictions, 0 parser
limitations** (reference-observed allow/disallow == declared spec on every cell). The genericarg
*attach-sites* — `~typename`, `&groupname`, grpent-groupname — are modeled by wiring
`role.generic-arg` onto `type2.unwrap` / `type2.choice_from_group` / `grpent.groupname` as feature
roles, alongside the existing `type2.typename` site, rather than as new containment cells.

### Per-tool annotations — cddl-codegen, 81/81

`annotations/cddl_codegen.toml` is regenerated from the verify probes (execution-grounded, not
authored): **53 supported, 27 unsupported, 1 out_of_profile, 0 uncertain.** The one `out_of_profile`
row is `type2.tag_head_type` (the RFC 9682 type-valued tag head-number — a genuinely new capability)
that cddl-codegen — whose **target profile is ~RFC 8610** (stated in the file header) — rejects: it is
outside what the tool targets, NOT a gap within it. (`type2.major7` is `unsupported`, a real within-8610
gap: the `#7` capability predates the RFC 9682 grammar split — see §3a.) Deterministic (byte-identical
across two runs). cddl-codegen-specific
facts that do not belong in the pure-spec master (the `T / null -> Option<T>` consumer special-case;
`float -> f64`) are preserved as header notes in that file.

## 2. What is mechanically VERIFIED (the gate PASSES)

- **Build/drift-check OK**: 81 features, 9 roles, 32 containment, 54 encodings, 37 control-ops; every
  annotation id resolves to a real master id.
- **Bidirectional grammar reconcile (F2)**: the ABNF is a **completeness lint, not the spine**.
  *Backward* (feature → source): every feature's `production` resolves to a real ABNF production, the
  `prelude` pseudo-production, or the IANA control-op registry — `fabricated=0`. *Forward* (source →
  feature): every `type2` alternative is covered by ≥1 feature (see the per-alternative bullet). Prelude
  `gaps=0`. The real feature space is grammar ∪ prelude ∪ control-op registry (∪ tag registry, later).
- **Language-profile axis (F1)**: every feature carries a `profile` = the RFC that introduced its
  **capability** (not merely the grammar line — a pure production split keeps the older profile):
  **80 RFC8610, 1 RFC9682** (`type2.tag_head_type`, the type-valued tag head-number, inexpressible in
  8610). cddl-codegen's target profile is `RFC8610`; a feature whose profile is newer AND which
  cddl-codegen rejects is classified `out_of_profile` (excluded from "gaps"), not `unsupported`.
  **out_of_profile = 1.** Control operators mirror their introducing RFC into `profile` in `matrix.json`.
- **Link integrity (NEW check)**: every `feature.encodings` and `feature.roles` entry, and every
  containment `role`/`feature`, resolves to a defined master id — `link_errors=0`. (Previously a
  typo'd/dangling encoding or role link would have passed silently.)
- **type2 per-ALTERNATIVE completeness (the forward lint)**: the 12 `type2` alternatives are parsed
  straight from `sources/cddl-1-1-update.abnf` and each is asserted to have ≥1 covering feature row —
  **12/12 covered**. This is the structural check the old per-production-*name* reconcile could not do
  (it is exactly what missed the `#7` gap). Best-effort alternative coverage for the other alternation
  productions is computed and logged (see §4), so nothing passes silently.
- **Probe (ABNF-authority)**: all 81 feature examples run through all three oracles. `spec_valid` is
  the reference (ruby) verdict; rust is corroboration. **spec-invalid examples (reference rejects an
  authored example) = 0**; **rust parser limitations = 1** (`value.bytes`, recorded/non-fatal).
- **Containment corroboration**: 26 examples, **0 contradictions, 0 parser limitations**.
- **Determinism**: `annotations/cddl_codegen.toml` + `verify_report.json` byte-identical across two
  runs.

`verify.py` exits **0 (PASS)**. Hard failures are reserved for: fabricated productions, prelude gaps,
link-integrity errors, an uncovered `type2` alternative, a reference-rejected example, or a
containment contradiction — none present.

## 3. Consolidated UNCERTAIN list (for human review)

### 3a. Uncertain items: NONE. Out-of-profile items: 1 (the F1 correction)

- **`type2.tag_head_type`** — now classified **`out_of_profile`** (was `unsupported`). The RFC 9682
  *type-valued tag number* (`head-number = uint / ("<" type ">")`, example `t = #6.<n>(int)` / `n =
  uint`) is valid CDDL (ruby + rust accept). cddl-codegen exits `1` with a **lexer error** ("invalid
  digit found in string") — its *bundled, older* cddl-crate lexer cannot tokenize the newer type-valued
  head-number. Because this construct's `profile` is `RFC9682` (an addition present only in
  `cddl-1-1-update.abnf`) and cddl-codegen targets `RFC8610`, the rejection is **out of profile**, not a
  support gap within the tool's target — this is exactly the conflation F1 corrects.
- **`type2.major7`** — stays **`unsupported`** (a genuine within-RFC 8610 gap; cddl-codegen panics on
  `foo = #7.20`, exit 101). The RFC 9682 "`#7` split" only gave major 7 its own grammar *line*; the
  *capability* (`#7.n` simple values/floats) was always expressible via RFC 8610's generic `#DIGIT`
  alternative. Per the **capability rule** (profile = the RFC that introduced the capability, not the
  grammar line), its `profile` is `RFC8610`, so its rejection is a real support gap, not out-of-profile.
  (The row is kept for per-alternative coverage of the RFC 9682 grammar; only its profile changed.)
- **Classifier rule (F1)**: on spec-valid input, a nonzero cddl-codegen exit is `out_of_profile` iff the
  feature's grammar profile is newer than the target (`RFC8610`) on the grammar-version lineage,
  otherwise `unsupported` (panic `101` vs parse/lex-reject `other` recorded as the failure mode in the
  annotation evidence). There is still no "uncertain" support bucket.

### 3b. Recorded, NOT uncertain — rust parser limitation (1)

- **`value.bytes`** — example `magic = h'cafe'`. The reference (ruby) oracle and the ABNF accept it
  (`BCHAR` admits lowercase a–f); the rust `cddl` crate rejects lowercase hex (`h'CAFE'` is accepted).
  Under ABNF-authority this is a **rust parser limitation**, recorded in `verify_report.json`
  (`parser_limitations`) — **not** a spec disagreement, **not** uncertain, **not** a gate failure.
  cddl-codegen also panics here (codegen=101), so its annotation status is a clean `unsupported`.

### 3c. Resolved in this pass (previously flagged)

All previous §3 oracle disagreements, the HIGH finding, and the MEDIUM/LOW findings were fixed; each
touched example was re-verified VALID under **both** oracles (ruby + rust) and now isolates its
construct:

- **Former oracle disagreements (3) → resolved.** `value.bytes` is now a recorded parser limitation
  (§3b), not a hard fail (the gate no longer ANDs the two parsers). `type2.typename` example →
  `foo = bar` / `bar = uint` (self-contained, isolating; was `foo = bar / bar<uint>` with `bar`
  undefined). `type2.unwrap` example → `bar = [uint]` / `foo = ~bar` (was `foo = [~bar]`, `bar`
  undefined).
- **HIGH — missing `type2` `#7` alternative → resolved.** Added `type2.major7` for
  `"#" "7" ["." head-number]` (example `foo = #7.20`); `type2.major` now carries a genuinely non-7
  example `foo = #1.2` and `encodings = [enc.major0..enc.major7]` (the `#DIGIT` sigil ranges over all
  eight majors, not just 7). `type2` is now per-alternative complete (12/12), mechanically enforced.
- **MEDIUM — `type2.tag` re-anchored** to RFC 9682 ABNF
  (`alt = "#" "6" ["." head-number] "(" type ")"`); the head-number-as-type variation is covered by
  the new `type2.tag_head_type` row (§3a).
- **MEDIUM — `type.choice.null` removed** from the master `features` (it is the same
  `type = type1 *("/" type1)` production as `type.choice`, a consumer special-case, not a distinct
  ABNF alternative). The `T / null -> Option<T>` fact is preserved as a header note in
  `annotations/cddl_codegen.toml`.
- **MEDIUM — `/=` / `//=` now modeled.** Added `assignt.extend` (`"/="`, example `a = int` /
  `a /= tstr`) and `assigng.extend` (`"//="`, example `tcpopts = (1: int)` / `tcpopts //= (2: tstr)`)
  for the incremental type-/group-choice-extension operators. (cddl-codegen support: probe-determined.)
- **MEDIUM — `grpent.member`** example → `g = [int]` (the absent-memberkey / positional path, no
  longer indistinguishable from `memberkey.bareword`).
- **MEDIUM — `genericparm.type` / `genericparm.group` / `genericarg.type` production mislabels fixed**:
  the two genericparm rule-heads are now `production = "rule"`; the typename-instantiation row is now
  `production = "type2"` (it IS the `typename [genericarg]` type2 alternative — this also resolves the
  former dual-anchoring, since it and `type2.typename` now share one production).
- **MEDIUM — genericarg attach-sites** represented: `role.generic-arg` is wired onto `type2.unwrap`
  and `type2.choice_from_group` (joining `type2.typename` and `grpent.groupname`), covering all four
  instantiation sites.
- **MEDIUM — link-integrity blind spot closed**: the gate now reconciles `feature.encodings` /
  `feature.roles` / containment refs against master ids (§2).
- **LOW — `grpent.inline_group`** example → `g = [(uint, tstr)]` (isolated from occurrence) and RFC
  anchor → §2.2.1 (Groups). `prelude.float16/32/64` now link the exact `enc.major7.float16/32/64`
  cells; `float16-32` / `float32-64` link their two exact cells; `false/true/bool/nil/null/undefined`
  link the precise `enc.major7.simple_imm` cell (not the whole `enc.major7` family). `enc.major7.float`
  note no longer leaks the cddl-codegen `float -> f64` fact (moved to annotations).
  `enc.major7.float32` note now uses the exact appendix value `3.4028234663852886e+38`.

## 4. Completeness — what is closed, what is open

- **Closed**: per-alternative completeness for `type2` (12/12, mechanically gated); the `#7`
  alternative; the `/=` / `//=` operators; genericarg attach-sites; `type2.tag` head-number-as-type;
  the link-integrity and per-alternative checker blind spots (both now mechanically checked).
- **Best-effort coverage observations (logged, NOT gaps).** Only `type2` is the HARD per-alternative
  gate; the soft matcher logs the rest so nothing passes silently. The following log as
  "uncovered"/"not modeled" by the *textual* matcher but are in fact modeled — the mismatch is a
  1-to-many or sub-part mapping the matcher cannot align, not a missing construct:
  - `type1` — its single alternative `type2 [S (rangeop / ctlop) S type2]` maps to THREE rows
    (`rangeop.inclusive`, `rangeop.exclusive`, `type1.ctlop`).
  - `rule` — modeled via `genericparm.type` / `genericparm.group` (the optional `[genericparm]` on the
    rule head); the base `"="` assignment is implicit in every rule example.
  - `assignt` / `assigng` — the `"="` base alternative is the implicit ordinary assignment used by
    every example; only the `"/="` / `"//="` extensions get dedicated rows.
  - `genericparm` / `genericarg` — the `<...>` bracket alternative is modeled by the repetition /
    type1-argument sub-rows (`*.multiple`, `genericarg.type1`).
  - `head-number` — folded into `type2.tag` (uint head) + `type2.tag_head_type` (type head); no
    `production = "head-number"` rows by design.
- **Open (LOW, defensible)**: `optcom = S ["," S]` has no dedicated row (a pure separator, folded into
  `grpchoice.sequence`) — a documented divergence from one-row-per-alternative, not a capability gap.

## 5. Verified vs flagged — bottom line

- **Verified (reproducible, passing):** build/drift-check, link-integrity (0 errors), **bidirectional
  grammar reconcile** (backward `fabricated=0`, forward type2 per-alternative 12/12), **profile axis**
  (80 RFC8610 / 1 RFC9682, in `matrix.json`), prelude 40/40, control-ops 37/37, encodings 54,
  containment 32 (26 corroborated, 0 contradictions, 0 parser limitations), 81 execution-grounded
  annotations (53 supported / 27 unsupported / **1 out_of_profile** / **0 uncertain**), byte-identical
  determinism. **Gate exits 0 (PASS).**
- **Flagged (human review):** **0 uncertain items.** **1 out_of_profile** (`type2.tag_head_type` — the
  RFC 9682 type-valued tag head-number, a genuine new capability outside cddl-codegen's RFC 8610 target; see §3a).
  One **recorded rust parser limitation** (`value.bytes` — reference/ABNF accept lowercase hex, the rust
  crate rejects it; non-fatal, informational). Nothing is silently resolved.
