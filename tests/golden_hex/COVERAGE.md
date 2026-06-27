# Golden known-answer vectors — coverage map

Tracks which RFC 8949 Appendix A example encodings are exercised by the golden-vector test, so a
future revisit can see at a glance what's covered and what isn't.

- **Test:** `tests/golden_hex/` (`input.cddl` + `tests.rs`), wired as the `golden_hex` integration
  test in `src/integration_tests.rs`. Each vector asserts **both** directions: `to_cbor_bytes()`
  equals the spec bytes, and the spec bytes deserialize + re-encode back to themselves.
- **Why these matter:** they catch a *symmetric* encode+decode bug (both sides wrong in a
  compensating way) that round-trip tests structurally cannot see, because the expected bytes are
  anchored to the RFC, not to the generator's output.
- **RFC reference:** RFC 8949 Appendix A "Examples of Encoded CBOR Data Items" (Table 6) —
  <https://www.rfc-editor.org/rfc/rfc8949#appendix-A>. For offline use, drop a copy at
  `draft/golden-vectors/rfc8949.txt` (gitignored): `curl -O https://www.rfc-editor.org/rfc/rfc8949.txt`.

## Framing nuance (read this before reading the table)

cddl-codegen only emits serialization methods for *named composite types*, not bare primitive
aliases. So each primitive vector is tested as the single element of a one-element **array record**
(`foo = [v: <primitive>]`), and the actual bytes are `0x81` + the primitive's encoding. The
primitive encoding itself is what's being checked; the `0x81` is framing.

Exceptions that produce the RFC bytes *exactly* (no extra framing):
- `Triple::new(1,2,3)` → `0x83010203` = the RFC `[1, 2, 3]` row verbatim.
- `IntMap` / `TextMap` → bare `0xa2…` map (maps are composite, no wrapper).
- `TaggedPair` → bare `0xd82a…` (tag is composite).

## Legend

| mark | meaning |
|------|---------|
| ✅ | covered — the encoding path is exercised by the named test |
| ➕ | **coverable & recommended** — maps to a real generator path, but no vector yet |
| ➖ | N/A — generator never emits this under default flags (so not a gap) |

## Coverage vs RFC 8949 Appendix A (Table 6)

### Major type 0 — unsigned integer
| RFC value | RFC bytes | status | test / note |
|-----------|-----------|--------|-------------|
| 0 | `0x00` | ✅ | `triple_boundary_23_24` (a=0) |
| 1 | `0x01` | ✅ | `triple_inline` |
| 10 | `0x0a` | ✅ | `map_int_keys` (value 10) |
| 23 | `0x17` | ✅ | `triple_boundary_23_24` (b=23) |
| 24 | `0x1818` | ✅ | `triple_boundary_23_24` (c=24) |
| 25 | `0x1819` | ➖ | same 1-byte path as 24 (redundant) |
| 100 | `0x1864` | ✅ | `triple_one_two_byte` |
| 1000 | `0x1903e8` | ✅ | `triple_one_two_byte` |
| 1000000 | `0x1a000f4240` | ✅ | `triple_four_byte` |
| 1000000000000 | `0x1b000000e8d4a51000` | ✅ | `triple_eight_byte` |
| 18446744073709551615 (u64::MAX) | `0x1bffffffffffffffff` | ✅ | `triple_u64_max` |
| 18446744073709551616 | `0xc249010000000000000000` | ✅ | `bignum_unsigned` (inner) — biguint, tag 2 + bstr |

### Major type 1 — negative integer
| RFC value | RFC bytes | status | test / note |
|-----------|-----------|--------|-------------|
| -1 | `0x20` | ✅ | `nint_minus_1` |
| -10 | `0x29` | ➖ | same inline path as -1 (redundant) |
| -100 | `0x3863` | ✅ | `nint_minus_100` |
| -1000 | `0x3903e7` | ✅ | `nint_minus_1000` |
| -18446744073709551616 | `0x3bffffffffffffffff` | ✅ | `nint_min_8byte` (`OneNint::new(u64::MAX)`) — 8-byte nint boundary |
| -18446744073709551617 | `0xc349010000000000000000` | ✅ | `bignum_negative` (inner) — bignint, tag 3 + bstr |

### Major type 2 — byte string
| RFC value | RFC bytes | status | test / note |
|-----------|-----------|--------|-------------|
| h'' | `0x40` | ✅ | `bytes_empty` (inner) |
| h'01020304' | `0x4401020304` | ✅ | `bytes_four` (inner) |

### Major type 3 — text string
| RFC value | RFC bytes | status | test / note |
|-----------|-----------|--------|-------------|
| "" | `0x60` | ✅ | `text_empty` (inner) |
| "a" | `0x6161` | ✅ | `text_single` (inner) |
| "IETF" | `0x6449455446` | ✅ | `text_ietf` (inner) |
| "\"\\" | `0x62225c` | ➖ | ASCII escapes, same path as "a"/"IETF" |
| "ü" | `0x62c3bc` | ✅ | `text_utf8_2byte` (inner) — 2-byte UTF-8 |
| "水" | `0x63e6b0b4` | ✅ | `text_utf8_3byte` (inner) — 3-byte UTF-8 |
| "𐅑" | `0x64f0908591` | ✅ | `text_utf8_4byte` (inner) — 4-byte UTF-8 / astral |

### Major type 4 — array
| RFC value | RFC bytes | status | test / note |
|-----------|-----------|--------|-------------|
| [] | `0x80` | ✅ | `array_empty` (inner) — variable array `[* uint]`, empty |
| [1, 2, 3] | `0x83010203` | ✅ | `triple_inline` (exact, no wrapper); also `array_three` (inner) |
| [1, [2, 3], [4, 5]] | `0x8301820203820405` | ✅* | `nested_arrays` covers array-in-array (`[7,[2,3]]`); the 3-element form is the same path |
| [1, …, 25] | `0x9819…` | ✅ | `array_25_count_header` (inner) — exercises the `0x98` (1-byte count) header |

### Major type 5 — map
| RFC value | RFC bytes | status | test / note |
|-----------|-----------|--------|-------------|
| {} | `0xa0` | ✅ | `map_empty` (inner) — variable map `{ * uint => uint }`, empty |
| {1: 2, 3: 4} | `0xa201020304` | ✅ | `map_variable` (inner, exact); also `map_int_keys` (fixed map(2)) |
| {"a": 1, "b": [2, 3]} | `0xa26161016162820203` | ✅* | `map_text_keys` covers text-key map(2); array-valued entry not tested |
| ["a", {"b": "c"}] | `0x826161a161626163` | ➖ | composition of covered paths |
| {"a":"A", … "e":"E"} | `0xa56161…` | ➖ | 5-entry map, same path |

### Major type 6 — tag
| RFC value | RFC bytes | status | test / note |
|-----------|-----------|--------|-------------|
| (any tag) | `0xd82a…` (our tag 42) | ✅* | `tag_pair` exercises the tag-with-1-byte-arg encoding (`0xd8 0x2a`) wrapping an array |
| 0("…date…") | `0xc074…` | ➖ | inline-tag (≤23) arg encoding; different *value*, same major-type-6 machinery |
| 1(1363896240) | `0xc11a514b67b0` | ➖ | epoch tag; specific tag semantics not modeled by codegen |
| 23(h'01020304') | `0xd74401020304` | ➖ | inline tag 23 |
| 24(h'6449455446') | `0xd818456449455446` | ➖ | tag 24, same 1-byte-arg path as our tag 42 |
| 32("http://…") | `0xd82076…` | ➖ | tag 32, same path |

### Major type 7 — simple values & float
| RFC value | RFC bytes | status | test / note |
|-----------|-----------|--------|-------------|
| false | `0xf4` | ✅ | `boolean_false` (inner) |
| true | `0xf5` | ✅ | `boolean_true` (inner) |
| null | `0xf6` | ✅ | `null_value` (inner) |
| undefined | `0xf7` | ➖ | cddl-codegen does not support `undefined` |
| simple(16) | `0xf0` | ➖ | not a CDDL construct the generator emits |
| simple(255) | `0xf8ff` | ➖ | as above |
| 1.1 (double) | `0xfb3ff199999999999a` | ✅ | `float_double` (inner) — chosen because it has no shorter form |
| -4.1 (double) | `0xfbc010666666666666` | ✅ | `float_negative` (inner) |
| Infinity (double) | `0xfb7ff0000000000000` | ✅ | `float_infinity` (inner) |
| -Infinity (double) | `0xfbfff0000000000000` | ✅ | `float_neg_infinity` (inner) |
| NaN (double) | `0xfb7ff8000000000000` | ✅ | `float_nan` (inner) |
| 1.0e+300 (double) | `0xfb7e37e43c8800759c` | ➖ | redundant — same double path as 1.1 / -4.1 |
| 0.0, 1.0, 1.5, … (half `0xf9…`) | `0xf9…` | ➖ | generator maps `float` → **f64**; verified it does not shorten (Inf/NaN encode as double, not half) |
| 100000.0, 3.40e38, … (single `0xfa…`) | `0xfa…` | ➖ | generator never emits single-precision (same: no shortening) |

### Indefinite-length / streaming (the `_` rows)
| RFC value | RFC bytes | status | note |
|-----------|-----------|--------|------|
| `(_ h'0102', h'030405')`, `(_ "strea","ming")`, `[_ …]`, `{_ …}` (all streaming rows) | `0x5f…`, `0x7f…`, `0x9f…`, `0xbf…` | ➖ | generator emits **definite-length** under default flags. Belongs in a separate vector set run under `--preserve-encodings` (which can round-trip indefinite-length inputs). |

## Summary

- **Covered (✅):** every applicable RFC 8949 Appendix A encoding path under default flags —
  uint ladder (incl. u64::MAX), nint (incl. 8-byte boundary), bignum (tags 2/3), bstr, tstr
  (incl. 2/3/4-byte UTF-8), fixed + variable arrays (incl. the `0x98` count header) + nesting,
  fixed + variable maps (text & int keys), tag (1-byte arg), bool/null, and f64 (normal, negative,
  ±Infinity, NaN). **No ➕ "coverable but untested" rows remain.**
- **Deliberately out of scope (➖):** half/single-precision floats, `undefined`/simple values, and
  all indefinite-length encodings — none are emitted by the generator under default flags. The
  indefinite-length rows are the natural seed for a future `--preserve-encodings` vector set (the
  only remaining frontier for golden vectors, and a distinct effort since it tests a non-default
  encoding mode).
