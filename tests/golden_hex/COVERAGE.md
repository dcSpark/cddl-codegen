# Golden known-answer vectors — coverage map

> **GENERATED** by `cddl-matrix/project_golden_hex.ts` — do not hand-edit. The mechanical half
> (covered / which test) is derived from the asserted bytes in `tests.rs`; the rationale half is
> authored in `cddl-matrix/annotations/golden_hex/cddl_codegen.toml` and joined in. Regenerate after
> changing either. CI fails on drift (a note that contradicts reality) or any ➕ row.

Tracks which RFC 8949 Appendix A vectors the golden-vector test exercises, vs the full CBOR
encoding grid. Each ✅ asserts **both** directions (`to_cbor_bytes()` == spec bytes, and spec bytes
round-trip back). They catch a *symmetric* encode+decode bug that round-trip tests structurally can't.

Reference: RFC 8949 Appendix A — <https://www.rfc-editor.org/rfc/rfc8949#appendix-A>. Both are pinned
offline under `cddl-matrix/sources/`: the full text as `rfc8949.txt`, the vectors as `appendix_a.json`.

**Framing nuance:** cddl-codegen only emits serialization for *named composite types*, so each
primitive vector is tested as the single element of a one-element array record (`foo = [v: <prim>]`)
— the asserted bytes are `0x81` + the primitive encoding. Composite vectors (`Triple`→`0x83…`,
maps→`0xa…`, `TaggedPair`→`0xd82a…`) assert the RFC bytes exactly. Coverage is matched on the
primitive encoding regardless of the `0x81` framing.

## Legend

| mark | meaning |
|------|---------|
| ✅ | covered — the exact RFC encoding is asserted by the named test |
| ✅* | path covered — a sibling vector on the same encoding path is tested; this exact vector is redundant |
| ➕ | **coverable but untested** — maps to a real generator path, no test and no rationale (actionable gap) |
| ➖ | N/A — generator never emits this under default flags (see reason) |

## Summary

- Appendix A vectors: **82** — ✅ 41 covered · ✅* 24 redundant · ➖ 17 N/A · ➕ 0 unexplained
- Legal **leaf** cells: **45** — 20 covered, 25 unexercised:
  - 6 **never emitted** under default flags (indefinite-length, float16/32, extended-simple, break)
  - 19 **emittable but no Appendix A vector lands here** (e.g. wide-argument length/count heads) — not a generator gap, just outside the App-A example set
- Golden tests: 49 default-flags · sibling sets: 63 preserve + 30 canonical (below)

**Sibling golden sets (not in this grid):** the encodings the default-flags set can never
exercise — the ➖ `.indef` cells, the non-minimal float spellings of a value whose shortest form is narrower, and non-minimal header arguments — have their own spec-anchored
KATs: `tests/golden_hex_preserve/tests.rs` (irregular RFC 8949 §3 encodings must re-encode
byte-identically under `--preserve-encodings`) and `tests/golden_hex_canonical/tests.rs` (the
same irregular inputs must re-encode to hand-derived §4.2 minimal bytes under `--canonical-form`).
This projection validates their byte-literal/well-formedness contract and counts them, but the
Appendix A join above stays default-flags-only by design. Everything else uncovered is either
redundant or has no canonical RFC vector.

### Major type 0 — unsigned integer

| RFC bytes | decoded | | test / note |
|-----------|---------|---|-------------|
| `00` | 0 | ✅ | `triple_boundary_23_24` (+5) |
| `01` | 1 | ✅ | `triple_inline` (+10) |
| `0a` | 10 | ✅ | `array_25_count_header` (+2) |
| `17` | 23 | ✅ | `triple_boundary_23_24` (+1) |
| `1818` | 24 | ✅ | `triple_boundary_23_24` (+1) |
| `1819` | 25 | ✅ | `array_25_count_header` |
| `1864` | 100 | ✅ | `triple_one_two_byte` |
| `1903e8` | 1000 | ✅ | `triple_one_two_byte` |
| `1a000f4240` | 1000000 | ✅ | `triple_four_byte` |
| `1b000000e8d4a51000` | 1000000000000 | ✅ | `triple_eight_byte` |
| `1bffffffffffffffff` | 18446744073709551615 | ✅ | `triple_u64_max` |

### Major type 1 — negative integer

| RFC bytes | decoded | | test / note |
|-----------|---------|---|-------------|
| `20` | -1 | ✅ | `nint_minus_1` |
| `29` | -10 | ✅* | same nint inline path (head 0x2N) as -1, covered by nint_minus_1 |
| `3863` | -100 | ✅ | `nint_minus_100` |
| `3903e7` | -1000 | ✅ | `nint_minus_1000` |
| `3bffffffffffffffff` | -18446744073709551616 | ✅ | `nint_min_8byte` |

### Major type 2 — byte string

| RFC bytes | decoded | | test / note |
|-----------|---------|---|-------------|
| `40` | h'' | ✅ | `bytes_empty` |
| `4401020304` | h'01020304' | ✅ | `bytes_four` |
| `5f42010243030405ff` | (_ h'0102', h'030405') | ➖ | generator emits definite-length byte strings under default flags; indefinite is covered by the sibling preserve/canonical KATs (tests/golden_hex_preserve, tests/golden_hex_canonical) |

### Major type 3 — text string

| RFC bytes | decoded | | test / note |
|-----------|---------|---|-------------|
| `60` | `""` | ✅ | `text_empty` |
| `6161` | a | ✅ | `text_single` (+1) |
| `62225c` | "\ | ✅* | ASCII escapes, same tstr immediate path as "a"/"IETF" (text_single / text_ietf) |
| `62c3bc` | ü | ✅ | `text_utf8_2byte` |
| `63e6b0b4` | 水 | ✅ | `text_utf8_3byte` |
| `6449455446` | IETF | ✅ | `text_ietf` |
| `64f0908591` | 𐅑 | ✅ | `text_utf8_4byte` |
| `7f657374726561646d696e67ff` | streaming | ➖ | generator emits definite-length text strings under default flags; indefinite is covered by the sibling preserve/canonical KATs |

### Major type 4 — array

| RFC bytes | decoded | | test / note |
|-----------|---------|---|-------------|
| `80` | [] | ✅ | `array_empty` |
| `826161a161626163` | ["a", {"b": "c"}] | ✅* | composition of covered array + map immediate paths |
| `826161bf61626163ff` | ["a", {"b": "c"}] | ✅* | outer definite array covered; embeds an indefinite inner map (see enc.major5.indef, never emitted) |
| `83010203` | [1, 2, 3] | ✅ | `triple_inline` (+1) |
| `8301820203820405` | [1, [2, 3], [4, 5]] | ✅* | array-in-array path covered by nested_arrays ([7,[2,3]]); the 3-element form is the same path |
| `83018202039f0405ff` | [1, [2, 3], [4, 5]] | ✅* | outer definite array covered by nested_arrays; embeds an indefinite inner array (see enc.major4.indef, never emitted) |
| `83019f0203ff820405` | [1, [2, 3], [4, 5]] | ✅* | outer definite array covered by nested_arrays; embeds an indefinite inner array (see enc.major4.indef) |
| `98190102030405060708090a0b0c0d0e0f101112131415161718181819` | [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,… | ✅ | `array_25_count_header` |
| `9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff` | [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,… | ➖ | generator emits definite-length arrays under default flags; indefinite is covered by the sibling preserve/canonical KATs |
| `9f01820203820405ff` | [1, [2, 3], [4, 5]] | ➖ | generator emits definite-length arrays under default flags; indefinite is covered by the sibling preserve/canonical KATs |
| `9f018202039f0405ffff` | [1, [2, 3], [4, 5]] | ➖ | generator emits definite-length arrays under default flags; indefinite is covered by the sibling preserve/canonical KATs |
| `9fff` | [] | ➖ | generator emits definite-length arrays under default flags; indefinite is covered by the sibling preserve/canonical KATs |

### Major type 5 — map

| RFC bytes | decoded | | test / note |
|-----------|---------|---|-------------|
| `a0` | {} | ✅ | `map_empty` |
| `a201020304` | {1: 2, 3: 4} | ✅ | `map_variable` |
| `a26161016162820203` | {"a": 1, "b": [2, 3]} | ✅* | text-key map(2) covered by map_text_keys; the array-valued entry is composition of covered paths |
| `a56161614161626142616361436164614461656145` | {"a": "A", "b": "B", "c": "C", "d": "D"… | ✅* | 5-entry map, same map immediate-count path as map_text_keys |
| `bf61610161629f0203ffff` | {"a": 1, "b": [2, 3]} | ➖ | generator emits definite-length maps under default flags; indefinite is covered by the sibling preserve/canonical KATs |
| `bf6346756ef563416d7421ff` | {"Fun": true, "Amt": -2} | ➖ | generator emits definite-length maps under default flags; indefinite is covered by the sibling preserve/canonical KATs |

### Major type 6 — tag

| RFC bytes | decoded | | test / note |
|-----------|---------|---|-------------|
| `c074323031332d30332d32315432303a30343a30305a` | 0("2013-03-21T20:04:00Z") | ✅* | tag 0 datetime SEMANTICS not modeled (codegen wraps #6.n parametrically); the major-6 immediate path is covered by the bignum tags (c2/c3) |
| `c11a514b67b0` | 1(1363896240) | ✅* | tag 1 epoch semantics not modeled; major-6 immediate path covered by bignum tags |
| `c1fb41d452d9ec200000` | 1(1363896240.5) | ✅* | tag 1 epoch-float semantics not modeled; major-6 immediate path covered by bignum tags |
| `c249010000000000000000` | 18446744073709551616 | ✅ | `bignum_unsigned` |
| `c349010000000000000000` | -18446744073709551617 | ✅ | `bignum_negative` |
| `d74401020304` | 23(h'01020304') | ✅* | tag 23 semantics not modeled; major-6 immediate path covered by bignum tags |
| `d818456449455446` | 24(h'6449455446') | ✅* | tag 24 (encoded-cbor) semantics not modeled; the 1-byte-arg tag path (head 0xd8) is covered by tag_pair (#6.42) |
| `d82076687474703a2f2f7777772e6578616d706c652e636f6d` | 32("http://www.example.com") | ✅* | tag 32 (uri) semantics not modeled; same 1-byte-arg tag path (head 0xd8) as tag_pair (#6.42) |

### Major type 7 — simple / float

| RFC bytes | decoded | | test / note |
|-----------|---------|---|-------------|
| `f0` | simple(16) | ➖ | unassigned simple value 16 is not a CDDL construct the generator emits; simple-value immediate cell is covered by false/true/null/undefined |
| `f4` | false | ✅ | `boolean_false` |
| `f5` | true | ✅ | `boolean_true` |
| `f6` | null | ✅ | `null_value` |
| `f7` | undefined | ✅ | `undefined_value` |
| `f818` | simple(24) | ➖ | extended simple values are not a CDDL construct the generator emits |
| `f8ff` | simple(255) | ➖ | extended simple values are not a CDDL construct the generator emits |
| `f90000` | 0.0 | ✅* | same f16 path (head 0xf9) as 1.5, covered by float_half |
| `f90001` | 5.960464477539063e-08 | ✅* | same f16 path as 1.5; a subnormal payload is not a distinct generator path |
| `f90400` | 6.103515625e-05 | ✅* | same f16 path as 1.5, covered by float_half |
| `f93c00` | 1.0 | ✅* | same f16 path as 1.5, covered by float_half |
| `f93e00` | 1.5 | ✅ | `float_half` |
| `f97bff` | 65504.0 | ✅* | same f16 path as 1.5, covered by float_half |
| `f97c00` | Infinity | ✅ | `float_infinity` |
| `f97e00` | NaN | ✅ | `float_nan` |
| `f98000` | -0.0 | ✅* | same f16 path as 1.5; the sign bit is not a distinct generator path |
| `f9c400` | -4.0 | ✅* | same f16 path as 1.5, covered by float_half |
| `f9fc00` | -Infinity | ✅ | `float_neg_infinity` |
| `fa47c35000` | 100000.0 | ✅ | `float_single` |
| `fa7f7fffff` | 3.4028234663852886e+38 | ✅* | same f32 path (head 0xfa) as 100000.0, covered by float_single |
| `fa7f800000` | Infinity | ➖ | Infinity's shortest lossless form is the half `f97c00` (covered by float_infinity), so a write never selects the single-precision spelling; reads accept it  [`smallest_float_sz`] |
| `fa7fc00000` | NaN | ➖ | the canonical quiet NaN's shortest lossless form is the half `f97e00` (covered by float_nan), so a write never selects the single-precision spelling; reads accept it  [`smallest_float_sz`] |
| `faff800000` | -Infinity | ➖ | -Infinity's shortest lossless form is the half `f9fc00` (covered by float_neg_infinity), so a write never selects the single-precision spelling; reads accept it  [`smallest_float_sz`] |
| `fb3ff199999999999a` | 1.1 | ✅ | `float_double` |
| `fb7e37e43c8800759c` | 1.0e+300 | ✅* | same f64/double path (head 0xfb) as 1.1, covered by float_double |
| `fb7ff0000000000000` | Infinity | ➖ | same as the single-precision Infinity spelling: the shortest lossless form is `f97c00`, so a write never selects the double  [`smallest_float_sz`] |
| `fb7ff8000000000000` | NaN | ➖ | same as the single-precision NaN spelling: the shortest lossless form is `f97e00`, so a write never selects the double  [`smallest_float_sz`] |
| `fbc010666666666666` | -4.1 | ✅ | `float_negative` |
| `fbfff0000000000000` | -Infinity | ➖ | same as the single-precision -Infinity spelling: the shortest lossless form is `f9fc00`, so a write never selects the double  [`smallest_float_sz`] |

## Per-construct legal encodings

Q3 narrowed to one construct at a time: for each of the **48** feature rows that
declare an `encodings` list, the leaf cells that construct can legally take, and which of them no
golden vector asserts. Each declared ref is expanded through the master's parent→leaf relation
(`cddl-matrix/encodings.toml` — a PARENT row names its leaves in `cells`; a leaf ref is itself).

**The fixture floor comes first.** `input.cddl` is what the golden vectors are generated from, so a
construct it never mentions has nothing asserted about it at all — **30** of the
48 rows are in that position, marked ✗ below, and their whole legal set is
untested. This matters because coverage is derived as a union over every `kat!` in the file: without
the floor, a construct would be credited for a cell some *other* construct's vector happened to land
in (a fixture with no `bigfloat` in it would still report bigfloat's tag cell as covered).

**For a ✓ construct the covered/uncovered split is still CELL-keyed**, not vector-keyed: a cell counts
as covered when some asserted item's head lands in it, which need not be an item of *this* construct.
`type2.tag` reads `enc.major6.imm` covered because the bignum vectors (`c2`/`c3`) land there. So a ✓
row's *covered* is an upper bound on what is asserted about that construct specifically.

**What "legal" means here, and what it does not.** *Legal* = the leaf cells beneath the encoding
rows the construct **declares**. A construct whose CBOR head is fixed by its own definition declares
the single cell that head lands in — `bigfloat = #6.5(…)` is tag 5 at every value, so it declares
`enc.major6.imm` alone, and the master's drift gate re-derives that from the pinned prelude rather
than trusting the list. What remains is deliberately **not** a claim that cddl-codegen emits each
cell under default flags — the *never emitted* column carries that, from the same cell-keyed
`out_of_scope` notes as the summary — and **not** a claim that a cell is reachable at **every**
value where the head argument follows the value: a `uint` reaches `enc.major0.ai27` only at values
≥ 2^32, and a `bstr` reaches `enc.major2.ai26` only at lengths ≥ 2^16.

**Scope limit of the floor — one case, and it is narrower than it looks.** The floor is
`corpus_detect.ts`'s text scan, which matches a construct by NAME, then corrected for the prelude's
plain aliases (`bytes = bstr`, `text = tstr`, `null = nil`, derived from the pinned prelude): the
fixture writing `tstr` credits `prelude.text` too, because they are one construct under two
spellings. What is NOT corrected is a construct the fixture reaches by writing a **wider type that
the generator narrows at emission**: `one_float = [v: float]` asserts each value at its shortest
form, so `prelude.float16`'s cell is exercised in fact, yet no rule names `float16` and it reads ✗. That is a
claim about what cddl-codegen picks when it emits a union, not about spec structure — not derivable
from the prelude, and deliberately not guessed. For those rows read ✗ as *no golden rule names this
construct*, which is the honest fact, not as *nothing resembling it is asserted anywhere*.

Reported, never fatal: this gate's non-zero exit stays reserved for note drift and ➕ (uncovered
Appendix A vector with no rationale). The coverage this narrows is the same `cellsCovered` behind
the summary's *emittable but no Appendix A vector lands here* line, which is already a deliberate
non-failure — failing here would re-litigate that threshold from a different direction.

**These totals do not sum to the summary's, and making them agree would be a regression.** The
summary asks whether ANY vector reaches a cell; this section asks whether one reaches it *for this
construct*, so a globally covered cell is still untested here. An earlier draft had the two
agreeing exactly — that was an artifact of crediting every construct for every other construct's
vectors, and restoring the agreement would mean restoring the over-credit.

- Exercised by `input.cddl`: **18** of 48 (✗ rows below have their full legal set untested)
- Constructs with at least one untested-and-emittable cell: **40** of 48
- Both counts are **conservative in one known direction**: a construct the fixture reaches only
  through a wider type the generator narrows at emission (`float` → the `float64` cell) counts as
  ✗ here, so *exercised* is a floor and *with gaps* is a ceiling. Cited alone, they overstate the
  gap by that margin.

| construct | in fixture | legal | covered | never emitted | untested and emittable |
|-----------|:---------:|-------|---------|---------------|------------------------|
| `prelude.b64legacy` | ✗ | 1 | 0 | 0 | `enc.major6.ai24` |
| `prelude.b64url` | ✗ | 1 | 0 | 0 | `enc.major6.ai24` |
| `prelude.bigfloat` | ✗ | 1 | 0 | 0 | `enc.major6.imm` |
| `prelude.bigint` | ✗ | 1 | 0 | 0 | `enc.major6.imm` |
| `prelude.bignint` | ✓ | 1 | 1 | 0 | — |
| `prelude.biguint` | ✓ | 1 | 1 | 0 | — |
| `prelude.bool` | ✓ | 1 | 1 | 0 | — |
| `prelude.bstr` | ✓ | 6 | 1 | 1 | `enc.major2.ai24`, `enc.major2.ai25`, `enc.major2.ai26`, `enc.major2.ai27` |
| `prelude.bytes` | ✓ | 6 | 1 | 1 | `enc.major2.ai24`, `enc.major2.ai25`, `enc.major2.ai26`, `enc.major2.ai27` |
| `prelude.cbor-any` | ✗ | 1 | 0 | 0 | `enc.major6.ai25` |
| `prelude.decfrac` | ✗ | 1 | 0 | 0 | `enc.major6.imm` |
| `prelude.eb16` | ✗ | 1 | 0 | 0 | `enc.major6.imm` |
| `prelude.eb64legacy` | ✗ | 1 | 0 | 0 | `enc.major6.imm` |
| `prelude.eb64url` | ✗ | 1 | 0 | 0 | `enc.major6.imm` |
| `prelude.encoded-cbor` | ✗ | 1 | 0 | 0 | `enc.major6.ai24` |
| `prelude.false` | ✗ | 1 | 0 | 0 | `enc.major7.simple_imm` |
| `prelude.float` | ✓ | 3 | 3 | 0 | — |
| `prelude.float16` | ✗ | 1 | 0 | 0 | `enc.major7.float16` |
| `prelude.float16-32` | ✗ | 2 | 0 | 0 | `enc.major7.float16`, `enc.major7.float32` |
| `prelude.float32` | ✗ | 1 | 0 | 0 | `enc.major7.float32` |
| `prelude.float32-64` | ✗ | 2 | 0 | 0 | `enc.major7.float32`, `enc.major7.float64` |
| `prelude.float64` | ✗ | 1 | 0 | 0 | `enc.major7.float64` |
| `prelude.int` | ✗ | 10 | 0 | 0 | `enc.major0.ai24`, `enc.major0.ai25`, `enc.major0.ai26`, `enc.major0.ai27`, `enc.major0.imm`, `enc.major1.ai24`, `enc.major1.ai25`, `enc.major1.ai26`, `enc.major1.ai27`, `enc.major1.imm` |
| `prelude.integer` | ✗ | 15 | 0 | 0 | `enc.major0.ai24`, `enc.major0.ai25`, `enc.major0.ai26`, `enc.major0.ai27`, `enc.major0.imm`, `enc.major1.ai24`, `enc.major1.ai25`, `enc.major1.ai26`, `enc.major1.ai27`, `enc.major1.imm`, `enc.major6.ai24`, `enc.major6.ai25`, `enc.major6.ai26`, `enc.major6.ai27`, `enc.major6.imm` |
| `prelude.mime-message` | ✗ | 1 | 0 | 0 | `enc.major6.ai24` |
| `prelude.nil` | ✓ | 1 | 1 | 0 | — |
| `prelude.nint` | ✓ | 5 | 4 | 0 | `enc.major1.ai26` |
| `prelude.null` | ✓ | 1 | 1 | 0 | — |
| `prelude.number` | ✗ | 13 | 0 | 0 | `enc.major0.ai24`, `enc.major0.ai25`, `enc.major0.ai26`, `enc.major0.ai27`, `enc.major0.imm`, `enc.major1.ai24`, `enc.major1.ai25`, `enc.major1.ai26`, `enc.major1.ai27`, `enc.major1.imm`, `enc.major7.float16`, `enc.major7.float32`, `enc.major7.float64` |
| `prelude.regexp` | ✗ | 1 | 0 | 0 | `enc.major6.ai24` |
| `prelude.tdate` | ✗ | 1 | 0 | 0 | `enc.major6.imm` |
| `prelude.text` | ✓ | 6 | 1 | 1 | `enc.major3.ai24`, `enc.major3.ai25`, `enc.major3.ai26`, `enc.major3.ai27` |
| `prelude.time` | ✗ | 1 | 0 | 0 | `enc.major6.imm` |
| `prelude.true` | ✗ | 1 | 0 | 0 | `enc.major7.simple_imm` |
| `prelude.tstr` | ✓ | 6 | 1 | 1 | `enc.major3.ai24`, `enc.major3.ai25`, `enc.major3.ai26`, `enc.major3.ai27` |
| `prelude.uint` | ✓ | 5 | 5 | 0 | — |
| `prelude.undefined` | ✓ | 1 | 1 | 0 | — |
| `prelude.unsigned` | ✗ | 10 | 0 | 0 | `enc.major0.ai24`, `enc.major0.ai25`, `enc.major0.ai26`, `enc.major0.ai27`, `enc.major0.imm`, `enc.major6.ai24`, `enc.major6.ai25`, `enc.major6.ai26`, `enc.major6.ai27`, `enc.major6.imm` |
| `prelude.uri` | ✗ | 1 | 0 | 0 | `enc.major6.ai24` |
| `type2.array` | ✓ | 6 | 2 | 1 | `enc.major4.ai25`, `enc.major4.ai26`, `enc.major4.ai27` |
| `type2.major` | ✗ | 45 | 0 | 6 | `enc.major0.ai24`, `enc.major0.ai25`, `enc.major0.ai26`, `enc.major0.ai27`, `enc.major0.imm`, `enc.major1.ai24`, `enc.major1.ai25`, `enc.major1.ai26`, `enc.major1.ai27`, `enc.major1.imm`, `enc.major2.ai24`, `enc.major2.ai25`, `enc.major2.ai26`, `enc.major2.ai27`, `enc.major2.imm`, `enc.major3.ai24`, `enc.major3.ai25`, `enc.major3.ai26`, `enc.major3.ai27`, `enc.major3.imm`, `enc.major4.ai24`, `enc.major4.ai25`, `enc.major4.ai26`, `enc.major4.ai27`, `enc.major4.imm`, `enc.major5.ai24`, `enc.major5.ai25`, `enc.major5.ai26`, `enc.major5.ai27`, `enc.major5.imm`, `enc.major6.ai24`, `enc.major6.ai25`, `enc.major6.ai26`, `enc.major6.ai27`, `enc.major6.imm`, `enc.major7.float16`, `enc.major7.float32`, `enc.major7.float64`, `enc.major7.simple_imm` |
| `type2.major7` | ✗ | 6 | 0 | 2 | `enc.major7.float16`, `enc.major7.float32`, `enc.major7.float64`, `enc.major7.simple_imm` |
| `type2.map` | ✓ | 6 | 1 | 1 | `enc.major5.ai24`, `enc.major5.ai25`, `enc.major5.ai26`, `enc.major5.ai27` |
| `type2.tag` | ✓ | 5 | 2 | 0 | `enc.major6.ai25`, `enc.major6.ai26`, `enc.major6.ai27` |
| `type2.tag_head_type` | ✗ | 5 | 0 | 0 | `enc.major6.ai24`, `enc.major6.ai25`, `enc.major6.ai26`, `enc.major6.ai27`, `enc.major6.imm` |
| `value.bytes` | ✓ | 6 | 1 | 1 | `enc.major2.ai24`, `enc.major2.ai25`, `enc.major2.ai26`, `enc.major2.ai27` |
| `value.number` | ✓ | 13 | 12 | 0 | `enc.major1.ai26` |
| `value.text` | ✗ | 6 | 0 | 1 | `enc.major3.ai24`, `enc.major3.ai25`, `enc.major3.ai26`, `enc.major3.ai27`, `enc.major3.imm` |

## Consistency (join drift check)

- ✅ All notes resolve to a real vector/cell and agree with the derived coverage. No drift.
