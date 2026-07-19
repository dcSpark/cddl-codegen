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

- Appendix A vectors: **82** — ✅ 38 covered · ✅* 16 redundant · ➖ 28 N/A · ➕ 0 unexplained
- Legal **leaf** cells: **45** — 18 covered, 27 unexercised:
  - 8 **never emitted** under default flags (indefinite-length, float16/32, extended-simple, break)
  - 19 **emittable but no Appendix A vector lands here** (e.g. wide-argument length/count heads) — not a generator gap, just outside the App-A example set
- Golden tests: 39 default-flags · sibling sets: 30 preserve + 15 canonical (below)

**Sibling golden sets (not in this grid):** the encodings the default-flags set can never
exercise — the ➖ `.indef` cells and non-minimal header arguments — have their own spec-anchored
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
| `01` | 1 | ✅ | `triple_inline` (+7) |
| `0a` | 10 | ✅ | `array_25_count_header` (+1) |
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
| `f0` | simple(16) | ➖ | unassigned simple value 16 is not a CDDL construct the generator emits; simple-value immediate cell is covered by false/true/null |
| `f4` | false | ✅ | `boolean_false` |
| `f5` | true | ✅ | `boolean_true` |
| `f6` | null | ✅ | `null_value` |
| `f7` | undefined | ➖ | cddl-codegen has no `undefined` construct; the simple-value immediate cell is otherwise covered by false/true/null |
| `f818` | simple(24) | ➖ | extended simple values are not a CDDL construct the generator emits |
| `f8ff` | simple(255) | ➖ | extended simple values are not a CDDL construct the generator emits |
| `f90000` | 0.0 | ➖ | generator maps CDDL `float` to Rust f64 and does NOT shorten (Inf/NaN encode as double, not half), so half-precision is never emitted  [`Special::Float`] |
| `f90001` | 5.960464477539063e-08 | ➖ | generator maps CDDL `float` to Rust f64 and does NOT shorten (Inf/NaN encode as double, not half), so half-precision is never emitted  [`Special::Float`] |
| `f90400` | 6.103515625e-05 | ➖ | generator maps CDDL `float` to Rust f64 and does NOT shorten (Inf/NaN encode as double, not half), so half-precision is never emitted  [`Special::Float`] |
| `f93c00` | 1.0 | ➖ | generator maps CDDL `float` to Rust f64 and does NOT shorten (Inf/NaN encode as double, not half), so half-precision is never emitted  [`Special::Float`] |
| `f93e00` | 1.5 | ➖ | generator maps CDDL `float` to Rust f64 and does NOT shorten (Inf/NaN encode as double, not half), so half-precision is never emitted  [`Special::Float`] |
| `f97bff` | 65504.0 | ➖ | generator maps CDDL `float` to Rust f64 and does NOT shorten (Inf/NaN encode as double, not half), so half-precision is never emitted  [`Special::Float`] |
| `f97c00` | Infinity | ➖ | generator maps CDDL `float` to Rust f64 and does NOT shorten (Inf/NaN encode as double, not half), so half-precision is never emitted  [`Special::Float`] |
| `f97e00` | NaN | ➖ | generator maps CDDL `float` to Rust f64 and does NOT shorten (Inf/NaN encode as double, not half), so half-precision is never emitted  [`Special::Float`] |
| `f98000` | -0.0 | ➖ | generator maps CDDL `float` to Rust f64 and does NOT shorten (Inf/NaN encode as double, not half), so half-precision is never emitted  [`Special::Float`] |
| `f9c400` | -4.0 | ➖ | generator maps CDDL `float` to Rust f64 and does NOT shorten (Inf/NaN encode as double, not half), so half-precision is never emitted  [`Special::Float`] |
| `f9fc00` | -Infinity | ➖ | generator maps CDDL `float` to Rust f64 and does NOT shorten (Inf/NaN encode as double, not half), so half-precision is never emitted  [`Special::Float`] |
| `fa47c35000` | 100000.0 | ➖ | generator never emits single-precision (same no-shortening reason as float16)  [`Special::Float`] |
| `fa7f7fffff` | 3.4028234663852886e+38 | ➖ | generator never emits single-precision (same no-shortening reason as float16)  [`Special::Float`] |
| `fa7f800000` | Infinity | ➖ | generator never emits single-precision (same no-shortening reason as float16)  [`Special::Float`] |
| `fa7fc00000` | NaN | ➖ | generator never emits single-precision (same no-shortening reason as float16)  [`Special::Float`] |
| `faff800000` | -Infinity | ➖ | generator never emits single-precision (same no-shortening reason as float16)  [`Special::Float`] |
| `fb3ff199999999999a` | 1.1 | ✅ | `float_double` |
| `fb7e37e43c8800759c` | 1.0e+300 | ✅* | same f64/double path (head 0xfb) as 1.1, covered by float_double |
| `fb7ff0000000000000` | Infinity | ✅ | `float_infinity` |
| `fb7ff8000000000000` | NaN | ✅ | `float_nan` |
| `fbc010666666666666` | -4.1 | ✅ | `float_negative` |
| `fbfff0000000000000` | -Infinity | ✅ | `float_neg_infinity` |

## Consistency (join drift check)

- ✅ All notes resolve to a real vector/cell and agree with the derived coverage. No drift.
