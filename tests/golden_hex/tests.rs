// Golden known-answer vectors. Every `expected` byte string below is hand-derived from
// RFC 8949 (Appendix A: the canonical CBOR encoding examples), NOT copied from generator output.
// That independence is the point: a *symmetric* encode+decode bug — both sides wrong in a
// compensating way — passes every round-trip test but fails here, because `expected` is anchored
// to the spec rather than to the code under test.
//
// Each vector asserts both directions:
//   * encode — `value.to_cbor_bytes()` must equal the spec bytes exactly, and
//   * decode — the spec bytes must deserialize and re-encode back to themselves.
//
// Coverage spans every CBOR major type the generator emits under default flags:
//   0 unsigned, 1 negative, 2 bytes, 3 text, 4 array, 5 map, 6 tag, 7 simple (bool/null/undefined) + float.
//
// AUTHORING CONVENTION: write `expected` bytes as two-digit `0x??` literals only (no decimal, no
// single-digit hex). cddl-matrix/project_golden_hex.ts extracts these arrays to build the coverage
// map and hard-fails on any other spelling.
//
// Coverage map vs the full RFC 8949 Appendix A table (what's covered, what isn't, and why):
//   tests/golden_hex/COVERAGE.md
#[cfg(test)]
mod golden_hex {
    use super::*;
    use serialization::Deserialize;

    macro_rules! kat {
        ($name:ident, $t:ty, $value:expr, $bytes:expr) => {
            #[test]
            fn $name() {
                let value: $t = $value;
                let expected: &[u8] = $bytes;
                assert_eq!(
                    value.to_cbor_bytes(),
                    expected,
                    "encode KAT mismatch (left = generated, right = RFC 8949)"
                );
                assert_eq!(
                    <$t>::from_cbor_bytes(expected).unwrap().to_cbor_bytes(),
                    expected,
                    "decode KAT mismatch (spec bytes did not round-trip)"
                );
            }
        };
    }

    // ---- major type 0: unsigned integer ladder (RFC 8949 §A) ----
    // 0..=23 inline, 24 → 0x18, 256+ → 0x19, 65536+ → 0x1a, 2^32+ → 0x1b, u64::MAX → 0x1b ff*8.
    kat!(triple_inline, Triple, Triple::new(1, 2, 3), &[0x83, 0x01, 0x02, 0x03]);
    kat!(
        triple_boundary_23_24,
        Triple,
        Triple::new(0, 23, 24),
        &[0x83, 0x00, 0x17, 0x18, 0x18]
    );
    kat!(
        triple_one_two_byte,
        Triple,
        Triple::new(100, 1000, 0),
        &[0x83, 0x18, 0x64, 0x19, 0x03, 0xe8, 0x00]
    );
    kat!(
        triple_four_byte,
        Triple,
        Triple::new(1_000_000, 0, 0),
        &[0x83, 0x1a, 0x00, 0x0f, 0x42, 0x40, 0x00, 0x00]
    );
    kat!(
        triple_eight_byte,
        Triple,
        Triple::new(1_000_000_000_000, 0, 0),
        &[0x83, 0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00, 0x00, 0x00]
    );
    kat!(
        triple_u64_max,
        Triple,
        Triple::new(u64::MAX, 0, 0),
        &[0x83, 0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00]
    );

    // ---- major type 1: negative integer ----
    // field `n: u64` encodes the value -(n+1); the resulting values (-1, -100, -1000) and their
    // bytes are RFC 8949 §A examples.
    kat!(nint_minus_1, OneNint, OneNint::new(0), &[0x81, 0x20]);
    kat!(nint_minus_100, OneNint, OneNint::new(99), &[0x81, 0x38, 0x63]);
    kat!(
        nint_minus_1000,
        OneNint,
        OneNint::new(999),
        &[0x81, 0x39, 0x03, 0xe7]
    );
    // 8-byte nint boundary: -(u64::MAX + 1) = -18446744073709551616 (encoder does -(n as i128 + 1)).
    kat!(
        nint_min_8byte,
        OneNint,
        OneNint::new(u64::MAX),
        &[0x81, 0x3b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
    );

    // ---- major type 2: byte string ----
    kat!(fixed_bytes_cafe, FixedBytes, FixedBytes::new_value_bytes_cafe(), &[0x42, 0xca, 0xfe]);
    kat!(
        bytes_four,
        OneBytes,
        OneBytes::new(vec![0x01, 0x02, 0x03, 0x04]),
        &[0x81, 0x44, 0x01, 0x02, 0x03, 0x04]
    );
    kat!(bytes_empty, OneBytes, OneBytes::new(vec![]), &[0x81, 0x40]);

    // ---- major type 3: text string ----
    kat!(
        text_ietf,
        OneText,
        OneText::new("IETF".to_string()),
        &[0x81, 0x64, 0x49, 0x45, 0x54, 0x46]
    );
    kat!(
        text_single,
        OneText,
        OneText::new("a".to_string()),
        &[0x81, 0x61, 0x61]
    );
    kat!(text_empty, OneText, OneText::new(String::new()), &[0x81, 0x60]);
    // UTF-8 multibyte: CBOR text length is the BYTE count, not the char count.
    kat!(
        text_utf8_2byte, // "ü" U+00FC
        OneText,
        OneText::new("\u{00fc}".to_string()),
        &[0x81, 0x62, 0xc3, 0xbc]
    );
    kat!(
        text_utf8_3byte, // "水" U+6C34
        OneText,
        OneText::new("\u{6c34}".to_string()),
        &[0x81, 0x63, 0xe6, 0xb0, 0xb4]
    );
    kat!(
        text_utf8_4byte, // "𐅑" U+10151 (astral / surrogate pair in UTF-16)
        OneText,
        OneText::new("\u{10151}".to_string()),
        &[0x81, 0x64, 0xf0, 0x90, 0x85, 0x91]
    );

    // ---- major type 4: array framing + nesting ----
    kat!(point_pair, Point, Point::new(1, 2), &[0x82, 0x01, 0x02]);
    kat!(
        nested_arrays,
        Nested,
        Nested::new(7, Point::new(2, 3)),
        &[0x82, 0x07, 0x82, 0x02, 0x03]
    );
    // variable-length array (the `*` occurrence), wrapped in a 1-element record.
    kat!(array_empty, ArrHolder, ArrHolder::new(vec![]), &[0x81, 0x80]);
    kat!(
        array_three,
        ArrHolder,
        ArrHolder::new(vec![1, 2, 3]),
        &[0x81, 0x83, 0x01, 0x02, 0x03]
    );
    // 25 elements → exercises the 0x98 array header (count in a following 1-byte uint8).
    kat!(
        array_25_count_header,
        ArrHolder,
        ArrHolder::new((1u64..=25).collect()),
        &[
            0x81, 0x98, 0x19, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b,
            0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x18,
            0x18, 0x19
        ]
    );

    // ---- major type 5: map (definite length, declaration order under default flags) ----
    kat!(
        map_text_keys,
        TextMap,
        TextMap::new(1, 2),
        &[0xa2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x02]
    );
    kat!(
        map_int_keys,
        IntMap,
        IntMap::new(10, 20),
        &[0xa2, 0x01, 0x0a, 0x02, 0x14]
    );
    // variable-length map (the `*` occurrence), wrapped; BTreeMap → keys in sorted order.
    kat!(
        map_empty,
        MapHolder,
        MapHolder::new(std::collections::BTreeMap::new()),
        &[0x81, 0xa0]
    );
    kat!(
        map_variable,
        MapHolder,
        MapHolder::new(std::collections::BTreeMap::from([(1u64, 2u64), (3, 4)])),
        &[0x81, 0xa2, 0x01, 0x02, 0x03, 0x04]
    );

    // ---- major type 6: tag (42 → 0xd8 0x2a) wrapping an array ----
    kat!(
        tag_pair,
        TaggedPair,
        TaggedPair::new(1, 2),
        &[0xd8, 0x2a, 0x82, 0x01, 0x02]
    );
    // bignum: RFC 8949 tag 2 (unsigned) / tag 3 (negative), 9-byte magnitude 0x010000000000000000.
    // These are 18446744073709551616 (2^64) and -(2^64 + 1) = -18446744073709551617.
    kat!(
        bignum_unsigned,
        BigUint,
        BigUint::new(vec![0x01, 0, 0, 0, 0, 0, 0, 0, 0].into()),
        &[0x81, 0xc2, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
    );
    kat!(
        bignum_negative,
        BigNint,
        BigNint::new(vec![0x01, 0, 0, 0, 0, 0, 0, 0, 0].into()),
        &[0x81, 0xc3, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
    );

    // ---- major type 7: simple values + float ----
    kat!(boolean_true, OneBool, OneBool::new(true), &[0x81, 0xf5]);
    kat!(boolean_false, OneBool, OneBool::new(false), &[0x81, 0xf4]);
    kat!(undefined_value, OneUndefined, OneUndefined::new(), &[0x81, 0xf7]);
    kat!(null_value, Nullable, Nullable::new(None), &[0x81, 0xf6]);
    kat!(
        nullable_present,
        Nullable,
        Nullable::new(Some(0)),
        &[0x81, 0x00]
    );
    // 1.1 has no exact half/single representation, so it pins the f64/double encoding (0xfb).
    kat!(
        float_double,
        OneFloat,
        OneFloat::new(1.1),
        &[0x81, 0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a]
    );
    kat!(
        float_negative,
        OneFloat,
        OneFloat::new(-4.1),
        &[0x81, 0xfb, 0xc0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66]
    );
    // Float specials — byte-compared (NaN != NaN, but the encodings are fixed bit patterns). RFC
    // 8949 Appendix A lists each of these at all three widths; a float write is the shortest form
    // that preserves the value (§4.1 preferred serialization), which for all three is the half.
    kat!(
        float_infinity,
        OneFloat,
        OneFloat::new(f64::INFINITY),
        &[0x81, 0xf9, 0x7c, 0x00]
    );
    kat!(
        float_neg_infinity,
        OneFloat,
        OneFloat::new(f64::NEG_INFINITY),
        &[0x81, 0xf9, 0xfc, 0x00]
    );
    kat!(
        float_nan,
        OneFloat,
        OneFloat::new(f64::NAN),
        &[0x81, 0xf9, 0x7e, 0x00]
    );
    // A half-precision value the shortest-form rule reaches from the other direction: 1.5 is
    // f16-exact, so an `f9`-headed instance both decodes and re-encodes as itself.
    kat!(
        float_half,
        OneFloat,
        OneFloat::new(1.5),
        &[0x81, 0xf9, 0x3e, 0x00]
    );
    // …and a single-precision one: 100000.0 is f32-exact but outside binary16's range.
    kat!(
        float_single,
        OneFloat,
        OneFloat::new(100000.0),
        &[0x81, 0xfa, 0x47, 0xc3, 0x50, 0x00]
    );

    // ---- open struct-map (loose CBOR "rest row"), default flags ----
    // A fixed key `1: uint` plus a trailing `* uint => uint`: unknown entries land in `.rest`. The
    // map header COUNTS the declared field plus every rest entry (1 + rest.len()); the wire is the
    // declared field first, then rest entries in ascending key order (BTreeMap). Bytes are hand-
    // derived from the CBOR grammar, independent of the generator. (Helper builders keep the `kat!`
    // value argument free of a `);` token, which the coverage extractor uses as the call terminator.)
    fn open_map_rest(entries: &[(u64, u64)]) -> OpenMap {
        let mut v = OpenMap::new(5);
        for &(k, val) in entries {
            v.insert_rest(k, val).unwrap();
        }
        v
    }
    // Empty rest: exactly the closed one-entry map {1: 5} — adding a rest row is wire-compatible.
    kat!(open_map_empty_rest, OpenMap, open_map_rest(&[]), &[0xa1, 0x01, 0x05]);
    // One captured entry {1: 5, 7: 9} — count 2 (0xa2), rest key 7 after the declared key 1.
    kat!(
        open_map_one_rest,
        OpenMap,
        open_map_rest(&[(7, 9)]),
        &[0xa2, 0x01, 0x05, 0x07, 0x09]
    );
    // Two captured entries {1: 5, 7: 9, 8: 10} — count 3 (0xa3); rest keys emit in ascending order
    // (7 before 8), after the declared field.
    kat!(
        open_map_two_rest_ordered,
        OpenMap,
        open_map_rest(&[(7, 9), (8, 10)]),
        &[0xa3, 0x01, 0x05, 0x07, 0x09, 0x08, 0x0a]
    );

    // ---- open struct-map, @ignore (tolerate-and-drop) flavor, default flags ----
    // These are NOT `kat!`s: the `kat!` macro asserts an identity round-trip, but an `@ignore` type
    // DROPS unknown entries, so decoding wire that carries them does not round-trip to itself — it
    // round-trips to the declared-only bytes. Every byte string is hand-derived from the CBOR
    // grammar, spec-anchored, independent of the generator.
    //
    // decode-with-unknowns → re-serialize → declared-only golden bytes: the wire {1: 5, 7: 9}
    // (count 2, declared key 1 plus one unknown uint entry 7) decodes, drops the unknown entry, and
    // re-emits the closed one-entry map {1: 5} (count 1).
    #[test]
    fn ignore_map_drops_unknown_on_reencode() {
        let wire: &[u8] = &[0xa2, 0x01, 0x05, 0x07, 0x09];
        let declared_only: &[u8] = &[0xa1, 0x01, 0x05];
        let decoded = IgnoreMap::from_cbor_bytes(wire).expect("wire with an unknown entry must decode");
        assert_eq!(
            decoded.to_cbor_bytes(),
            declared_only,
            "@ignore must DROP unknown entries and re-emit declared-only bytes"
        );
    }

    // Two unknown entries {1: 5, 7: 9, 8: 10} all drop → declared-only {1: 5}; the stream advances
    // correctly across both dropped entries (the typed `uint` range rejects a non-uint value, so the
    // nested-container drop-through is pinned by the `* uint => any` e2e fixture, not here).
    #[test]
    fn ignore_map_drops_multiple_unknown() {
        let wire: &[u8] = &[0xa3, 0x01, 0x05, 0x07, 0x09, 0x08, 0x0a];
        let declared_only: &[u8] = &[0xa1, 0x01, 0x05];
        let decoded = IgnoreMap::from_cbor_bytes(wire).expect("wire with unknown entries must decode");
        assert_eq!(decoded.to_cbor_bytes(), declared_only);
    }

    // empty-rest identity: wire with NO unknown entries is byte-identical to the closed one-entry
    // map on both directions — decode→re-encode is the input, and constructing declared-only encodes
    // to the same bytes. Adding an `@ignore` rest row to a spec stays wire-compatible for clean data.
    #[test]
    fn ignore_map_empty_rest_identity() {
        let closed: &[u8] = &[0xa1, 0x01, 0x05];
        assert_eq!(
            IgnoreMap::from_cbor_bytes(closed).unwrap().to_cbor_bytes(),
            closed,
            "empty-rest @ignore decode must round-trip byte-identically to the closed map"
        );
        assert_eq!(
            IgnoreMap::new(5).to_cbor_bytes(),
            closed,
            "an @ignore struct encodes exactly the closed one-entry map"
        );
    }

    // ---- open array (loose CBOR "rest tail"), default flags ----
    // Fixed members `uint, tstr` plus a trailing `* uint`: trailing elements land in `.rest`. The array
    // header COUNTS the declared members plus every tail element (2 + rest.len()); the wire is the two
    // declared members, then the tail in order. Bytes are hand-derived from the CBOR grammar,
    // independent of the generator. (The helper keeps the `kat!` value free of a `);` token, which the
    // coverage extractor uses as the call terminator.)
    fn open_list_tail(tail: &[u64]) -> OpenList {
        let mut v = OpenList::new(5, "hi".to_owned());
        for &e in tail {
            v.rest.push(e);
        }
        v
    }
    // Empty tail: exactly the closed two-element array [5, "hi"] — adding a rest tail is wire-compatible.
    kat!(
        open_list_empty_tail,
        OpenList,
        open_list_tail(&[]),
        &[0x82, 0x05, 0x62, 0x68, 0x69]
    );
    // One tail element [5, "hi", 7] — count 3 (0x83), the trailing 7 after the two declared members.
    kat!(
        open_list_one_tail,
        OpenList,
        open_list_tail(&[7]),
        &[0x83, 0x05, 0x62, 0x68, 0x69, 0x07]
    );
    // Two tail elements [5, "hi", 7, 8] — count 4 (0x84); the tail emits in Vec order after the
    // declared members.
    kat!(
        open_list_two_tail,
        OpenList,
        open_list_tail(&[7, 8]),
        &[0x84, 0x05, 0x62, 0x68, 0x69, 0x07, 0x08]
    );

    // ---- open array, @ignore (tolerate-and-drop) flavor, default flags ----
    // NOT `kat!`s: an `@ignore` tail DROPS trailing elements, so decoding wire that carries them does
    // not round-trip to itself — it re-emits the declared-prefix bytes. Every byte string is
    // hand-derived from the CBOR grammar, spec-anchored, independent of the generator.
    //
    // decode-with-trailing → re-serialize → declared-prefix golden bytes: the wire [5, 7, 8] (count 3,
    // declared uint 5 plus two trailing uints) decodes, drops the trailing elements, and re-emits the
    // one-element array [5] (count 1).
    #[test]
    fn ignore_list_drops_trailing_on_reencode() {
        let wire: &[u8] = &[0x83, 0x05, 0x07, 0x08];
        let declared_only: &[u8] = &[0x81, 0x05];
        let decoded =
            IgnoreList::from_cbor_bytes(wire).expect("wire with trailing elements must decode");
        assert_eq!(
            decoded.to_cbor_bytes(),
            declared_only,
            "@ignore must DROP trailing array elements and re-emit the declared prefix only"
        );
    }

    // empty-tail identity: wire with NO trailing elements is byte-identical to the one-element array on
    // both directions — decode→re-encode is the input, and constructing declared-only encodes to the
    // same bytes. Adding an `@ignore` rest tail to a spec stays wire-compatible for clean data.
    #[test]
    fn ignore_list_empty_tail_identity() {
        let closed: &[u8] = &[0x81, 0x05];
        assert_eq!(
            IgnoreList::from_cbor_bytes(closed).unwrap().to_cbor_bytes(),
            closed,
            "empty-tail @ignore decode must round-trip byte-identically to the closed array"
        );
        assert_eq!(
            IgnoreList::new(5).to_cbor_bytes(),
            closed,
            "an @ignore struct encodes exactly the closed one-element array"
        );
    }
}
