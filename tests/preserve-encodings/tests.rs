#[cfg(test)]
mod tests {
    use super::*;
    use cbor_event::{de::Deserializer, Sz, StringLenSz};
    use serialization::Deserialize;

    fn deser_test<T: Deserialize + ToCBORBytes>(orig: &T) {
        let orig_bytes = orig.to_cbor_bytes();
        print_cbor_types("orig", &orig_bytes);
        let mut deserializer = Deserializer::from(orig_bytes.clone());
        let deser = T::deserialize(&mut deserializer).unwrap();
        print_cbor_types("deser", &deser.to_cbor_bytes());
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
        assert_eq!(deserializer.position(), orig_bytes.len());
    }

    /// Assert that `bytes` is REJECTED, and that the rejection is the one the vector claims — the
    /// discriminated form of a bare `assert!(T::from_cbor_bytes(&bytes).is_err())`.
    ///
    /// A bare `is_err()` accepts ANY failure, so a hand-derived vector that fails EARLIER than the
    /// boundary it was written to prove (one byte off; a wrong major type reached before the check
    /// ever runs) stays green while the pinned boundary goes unexercised — outcome right,
    /// provenance wrong, invisible to every gate by construction. Pinning a distinctive substring
    /// of the message makes the provenance part of what the test asserts, and prints the real
    /// message when it moves. A substring only discriminates failures whose messages DIFFER: two
    /// defects sharing one message remain indistinguishable to it.
    ///
    /// Spelled IDENTICALLY in every fixture `tests.rs` that uses it, deliberately: each such file
    /// is appended standalone into its own generated crate, so there is no module a shared
    /// definition could live in and no import that could reach one.
    fn assert_decode_reject_reason<T: Deserialize>(bytes: &[u8], reason_substring: &str) {
        let err = T::from_cbor_bytes(bytes)
            .map(|_| ())
            .expect_err("expected this input to be REJECTED, but it decoded successfully");
        let msg = err.to_string();
        assert!(
            msg.contains(reason_substring),
            "rejected for the WRONG reason: expected a message containing \
             {reason_substring:?}, got: {msg}"
        );
    }

    // Second, independent oracle: validate our *encoder output* against the source .cddl via the
    // `cddl` crate (see tests/deser_test_conformance.rs). Round-trips only prove our encoder and
    // decoder agree with each other; this proves the bytes actually match the spec — catching an
    // encoder that emits spec-non-conformant bytes a symmetrically-buggy decoder would accept. The
    // preserve fixture is the richest hand-written round-trip surface (tags, nested tables,
    // type-choices, and — the point of the flag — irregular definite/indefinite encodings), and is
    // wired for the oracle (cddl dep + spec on disk); broadening to other fixtures is a compile-cost
    // trade-off, not a limitation of the helper.
    #[test]
    fn cddl_crate_conformance() {
        let spec = cddl_oracle_load_spec("/../../input.cddl");
        let check = |rule: &str, bytes: &[u8]| assert_cddl_conforms(&spec, rule, bytes);

        // foo = #6.11([uint, text, bytes]) — both the regular (definite/minimal) and an irregular
        // (indefinite array) encoding must conform: the whole point of --preserve-encodings is that
        // both are valid CBOR for the same value, and the independent validator agrees.
        let mut foo = Foo::new(436, String::from("jfkdsjfd"), vec![1, 1, 1]);
        check("foo", &foo.to_cbor_bytes());
        let mut foo_enc = FooEncoding::default();
        foo_enc.len_encoding = LenEncoding::Indefinite;
        foo.encodings = Some(foo_enc);
        check("foo", &foo.to_cbor_bytes());

        // bar = { ... } (struct-map).
        check("bar", &Bar::new(Foo::new(9, String::from("abc"), vec![6, 4]), None).to_cbor_bytes());

        // @newtype wrappers serialize as their inner shape: wrapper_list = [* uint], wrapper_table = { * uint => uint }.
        check("wrapper_list", &WrapperList::new(vec![5, 4, 3, 2, 1]).to_cbor_bytes());
        let mut wt = OrderedHashMap::new();
        wt.insert(1, 0);
        check("wrapper_table", &WrapperTable::new(wt).to_cbor_bytes());

        // NOTE: types whose array/map elements are a `.size`-aliased type (e.g. table_arr_members's
        // `arr: [*u32]` where `u32 = uint .size 4`) are deliberately NOT checked here — the `cddl`
        // validator mishandles that construct (rejects a valid `[1,3,6]` as "expected uint, got
        // Array"). That is a validator gap, not an encoder bug, and is exactly why a pass is a weak
        // signal and this oracle is a *second* one, never the sole one.

        // Teeth for the oracle itself: a bare int is not a #6.11(...) tagged array, so the validator
        // must reject it — proving assert_cddl_conforms above isn't passing against a no-op validator
        // or a mis-named root rule.
        assert_cddl_rejects(&spec, "foo", &[0x00]);
    }

    #[test]
    fn struct_array() {
        let mut foo = Foo::new(436, String::from("jfkdsjfd"), vec![1, 1, 1]);
        deser_test(&foo);
        let definite_bytes = foo.to_cbor_bytes();
        let mut encoding = FooEncoding::default();
        encoding.len_encoding = LenEncoding::Indefinite;
        foo.encodings = Some(encoding);
        deser_test(&foo);
        let indefinite_bytes = foo.to_cbor_bytes();
        assert!(definite_bytes != indefinite_bytes);
        assert_eq!(definite_bytes[0], 0xc6u8 + 11 - 6);
        assert_eq!(definite_bytes[1], 0x83u8);
        assert_eq!(indefinite_bytes[1], ARR_INDEF);
        assert_eq!(*indefinite_bytes.last().unwrap(), BREAK);
        // last bit of the the [1, 1, 1]
        assert_eq!(*definite_bytes.last().unwrap(), 1u8);
    }

    #[test]
    fn struct_map() {
        let mut bar = Bar::new(Foo::new(9, String::from("abc"), vec![6, 4]), None);
        // quick test without key 5
        deser_test(&bar);
        let mut bar_encoding = BarEncoding::default();
        bar_encoding.len_encoding = LenEncoding::Indefinite;
        bar.encodings = Some(bar_encoding.clone());
        deser_test(&bar);
        bar_encoding.len_encoding = LenEncoding::Definite(cbor_event::Sz::Inline);
        // full test with key 5 (but without key "derp")
        bar.key_5 = Some("text".into());
        bar.encodings = Some(bar_encoding.clone());
        let definite_bytes = bar.to_cbor_bytes();
        bar_encoding.len_encoding = LenEncoding::Indefinite;
        bar.encodings = Some(bar_encoding);
        deser_test(&bar);
        let indefinite_bytes = bar.to_cbor_bytes();
        let default_indef_bytes = vec![
            vec![MAP_INDEF],
                cbor_string("foo"),
                    cbor_tag(13),
                        bar.foo.to_cbor_bytes(),
                vec![0x01u8],
                    vec![NULL],
                vec![0x05u8],
                    cbor_string("text"),
                cbor_string("five"),
                    vec![0x05u8],
            vec![BREAK]
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        assert_eq!(indefinite_bytes, default_indef_bytes);
        let canonical_bytes = vec![
            map_def(4),
                vec![0x01u8],
                    vec![NULL],
                vec![0x05u8],
                    cbor_string("text"),
                cbor_string("foo"),
                    cbor_tag(13),
                        bar.foo.to_cbor_bytes(),
                cbor_string("five"),
                    vec![0x05u8],
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let mut bar_canonical = Bar::from_cbor_bytes(&canonical_bytes).unwrap();
        deser_test(&bar_canonical);
        assert_eq!(bar_canonical.encodings.as_ref().unwrap().len_encoding, LenEncoding::Canonical);
        assert_eq!(bar_canonical.encodings.as_ref().unwrap().orig_deser_order, vec![2, 3, 0, 4]);
        // get rid of other info and it should be identical
        let str_3_encodings = vec![
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Len(Sz::Eight),
            StringLenSz::Indefinite(vec![(2, Sz::One), (1, Sz::Four)])
        ];
        let str_4_encodings = vec![
            StringLenSz::Indefinite(vec![(4, Sz::Inline)]),
            StringLenSz::Len(Sz::Two),
            StringLenSz::Indefinite(vec![(1, Sz::Inline), (1, Sz::Four), (1, Sz::One), (1, Sz::Two)]),
        ];
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        for def_enc in def_encodings {
            for (str_3, str_4) in str_3_encodings.iter().zip(str_4_encodings.iter()) {
                for (has_5, has_derp) in [(false, false), (false, true), (true, false), (true, true)] {
                    let len = if has_5 && has_derp {
                        5
                    } else if has_5 || has_derp {
                        4
                    } else {
                        3
                    };
                    let keys = [
                        [
                            cbor_str_sz("foo", str_3.clone()),
                                cbor_tag_sz(13, def_enc),
                                bar.foo.to_cbor_bytes(),
                        ].into_iter().flatten().collect::<Vec<u8>>(),
                        if has_5 {
                            [
                                cbor_int(5, def_enc),
                                    cbor_str_sz("text", str_4.clone()),
                            ].into_iter().flatten().collect::<Vec<u8>>()
                        } else {
                            vec![]
                        },
                        [
                            cbor_int(1, def_enc),
                                vec![NULL],
                        ].into_iter().flatten().collect::<Vec<u8>>(),
                        if has_derp {
                            [
                                cbor_str_sz("derp", str_4.clone()),
                                    cbor_int(2, def_enc),
                            ].into_iter().flatten().collect::<Vec<u8>>()
                        } else {
                            vec![]
                        },
                        [
                            cbor_str_sz("five", str_4.clone()),
                                cbor_int(5, def_enc),
                        ].into_iter().flatten().collect::<Vec<u8>>(),
                    ];
                    // just a subset of permutations to not take forever
                    for key_order in [[0, 1, 2, 3, 4], [4, 3, 2, 1, 0], [3, 1, 0, 4, 2], [0, 2, 4, 1, 3], [2, 0, 3, 4, 1]] {
                        let mut irregular_encoding = map_sz(len, def_enc);
                        for i in 0..5 {
                            irregular_encoding.extend_from_slice(&keys[key_order[i]]);
                        }
                        print_cbor_types("irregular_encoding", &irregular_encoding);
                        let irregular_bar = Bar::from_cbor_bytes(&irregular_encoding).unwrap();
                        print_cbor_types("irregular_bar.to_cbor_bytes()", &irregular_bar.to_cbor_bytes());
                        assert_eq!(irregular_bar.to_cbor_bytes(), irregular_encoding);
                    }
                }
            }
        }
    }

    // Optional fixed-value members under --preserve-encodings: the presence `bool` coexists with the
    // fixed value's own width metadata. A NON-CANONICAL encoding of the constant (a fixed `5` spelled
    // as a wider uint) must round-trip byte-identically, proving the encoding var is captured and
    // replayed for the fixed value exactly as for a normal member.
    #[test]
    fn opt_fixed_member_preserve() {
        // absent -> presence false, no fixed element on the wire
        let absent = OptFixedArr::from_cbor_bytes(
            &[
                arr_def(2),
                cbor_int(9, Sz::Inline),
                cbor_string("hi"),
            ]
            .concat(),
        )
        .unwrap();
        assert!(!absent.ufix);
        deser_test(&absent);
        // present, with `? ufix: 5` spelled as a 2-byte-wide uint (Sz::Two, 0x19 0x00 0x05):
        // the width is preserved so re-serialize reproduces the exact bytes.
        let non_canonical = [
            arr_def(3),
            cbor_int(9, Sz::Inline),
            cbor_int(5, Sz::Two),
            cbor_string("hi"),
        ]
        .concat();
        let present = OptFixedArr::from_cbor_bytes(&non_canonical).unwrap();
        assert!(present.ufix);
        assert_eq!(present.to_cbor_bytes(), non_canonical);
        deser_test(&present);
        // wrong constant rejects
        let wrong = [
            arr_def(3),
            cbor_int(9, Sz::Inline),
            cbor_int(6, Sz::Inline),
            cbor_string("hi"),
        ]
        .concat();
        assert_decode_reject_reason::<OptFixedArr>(&wrong, "Expected fixed value 5 found 6");

        // map rep: a fixed map value with a non-canonical width preserves byte-identically too
        let map_nc = [
            map_def(2),
            cbor_string("a"),
            cbor_int(9, Sz::Inline),
            cbor_string("m_uint"),
            cbor_int(5, Sz::Two),
        ]
        .concat();
        let m = OptFixedMap::from_cbor_bytes(&map_nc).unwrap();
        assert!(m.m_uint && !m.m_text && !m.m_bool && !m.m_null && !m.m_nint);
        assert_eq!(m.to_cbor_bytes(), map_nc);
    }

    #[test]
    fn table_arr_members() {
        // a more complex test of these encodings is done in the canonical unit tests
        let mut table = OrderedHashMap::new();
        table.insert(0, "zero".into());
        table.insert(32, "thirty two".into());
        let orig = TableArrMembers::new(
            vec![1, 3, 6],
            vec![Foo::new(0, String::from("Zero"), vec![])],
            table.clone(),
        );
        deser_test(&orig);
        let expected = vec![
            map_def(3),
                cbor_string("arr"),
                    arr_def(3),
                        vec![0x01, 0x03, 0x06],
                cbor_string("arr2"),
                    arr_def(1),
                        cbor_tag(11),
                            arr_def(3),
                                vec![0x00],
                                cbor_string("Zero"),
                                vec![0x40],
                cbor_string("table"),
                    map_def(2),
                        vec![0x00],
                            cbor_string("zero"),
                        vec![0x18, 0x20],
                            cbor_string("thirty two"),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        assert_eq!(orig.to_cbor_bytes(), expected);
        let indef_other_order = vec![
            vec![MAP_INDEF],
                cbor_string("arr2"),
                    vec![ARR_INDEF],
                        cbor_tag(11),
                            vec![ARR_INDEF],
                                vec![0x00],
                                cbor_string("Zero"),
                                vec![0x40],
                            vec![BREAK],
                    vec![BREAK],
                cbor_string("table"),
                    vec![MAP_INDEF],
                        vec![0x18, 0x20],
                            cbor_string("thirty two"),
                        vec![0x00],
                            cbor_string("zero"),
                    vec![BREAK],
                cbor_string("arr"),
                    vec![ARR_INDEF],
                        vec![0x01, 0x03, 0x06],
                    vec![BREAK],
            vec![BREAK],
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let mut other_order = TableArrMembers::from_cbor_bytes(&indef_other_order).unwrap();
        assert_eq!(other_order.to_cbor_bytes(), indef_other_order);
        deser_test(&other_order);
        
        assert!(orig.encodings.is_none());

        let other_order_encodings = other_order.encodings.unwrap();
        assert_eq!(other_order_encodings.orig_deser_order, vec![1, 2, 0]);
        assert_eq!(other_order_encodings.len_encoding, LenEncoding::Indefinite);
        assert_eq!(other_order_encodings.arr_encoding, LenEncoding::Indefinite);
        assert_eq!(other_order_encodings.arr2_encoding, LenEncoding::Indefinite);
        assert_eq!(other_order.arr2[0].encodings.as_ref().unwrap().len_encoding, LenEncoding::Indefinite);
        assert_eq!(other_order_encodings.table_encoding, LenEncoding::Indefinite);
    }

    #[test]
    fn deeply_nested() {
        let str_3_encodings = vec![
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Len(Sz::Eight),
            StringLenSz::Indefinite(vec![(1, Sz::Two), (2, Sz::Four)])
        ];
        let str_4_encodings = vec![
            StringLenSz::Indefinite(vec![(4, Sz::Inline)]),
            StringLenSz::Len(Sz::Two),
            StringLenSz::Indefinite(vec![(1, Sz::Eight), (1, Sz::Inline), (1, Sz::Inline), (1, Sz::Two)]),
        ];
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        for def_enc in def_encodings {
            for (str_3, str_4) in str_3_encodings.iter().zip(str_4_encodings.iter()) {
                let irregular_bytes = vec![
                    arr_sz(1, def_enc),
                        vec![MAP_INDEF],
                            cbor_tag_sz(14, def_enc),
                                cbor_bytes_sz(vec![0xBA, 0xAD, 0xF0, 0x0D], str_4.clone()),
                                    map_sz(1, def_enc),
                                        cbor_int(10, def_enc),
                                            map_sz(1, def_enc),
                                                cbor_tag_sz(9, def_enc),
                                                    cbor_int(0, def_enc),
                                                        arr_sz(2, def_enc),
                                                            cbor_tag_sz(18, def_enc),
                                                                arr_sz(0, def_enc),
                                                            cbor_tag_sz(18, def_enc),
                                                                vec![ARR_INDEF],
                                                                    cbor_str_sz("test", str_4.clone()),
                                                                    cbor_str_sz("XYZ", str_3.clone()),
                                                                    cbor_str_sz("ABC", str_3.clone()),
                                                                vec![BREAK],
                            cbor_tag_sz(14, def_enc),
                                cbor_bytes_sz(vec![0xAA, 0xBB, 0xCC], str_3.clone()),
                                    vec![MAP_INDEF],
                                        cbor_int(5, def_enc),
                                            vec![MAP_INDEF],
                                            vec![BREAK],
                                        cbor_int(3, def_enc),
                                            map_sz(1, def_enc),
                                                cbor_tag_sz(9, def_enc),
                                                    cbor_int(2, def_enc),
                                                        vec![ARR_INDEF],
                                                            cbor_tag_sz(18, def_enc),
                                                                arr_sz(1, def_enc),
                                                                    cbor_str_sz("cbor", str_4.clone()),
                                                        vec![BREAK],
                                    vec![BREAK],
                        vec![BREAK],
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular = DeeplyNested::from_cbor_bytes(&irregular_bytes).unwrap();
                assert_eq!(irregular_bytes, irregular.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn string64() {
        let str_24_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Four),
            StringLenSz::Indefinite(vec![(12, Sz::Two), (12, Sz::One)]),
            StringLenSz::Indefinite(vec![(0, Sz::Inline), (4, Sz::Inline), (20, Sz::Four), (0, Sz::Eight)]),
        ];
        for str_enc in str_24_encodings {
            let irregular_bytes = cbor_str_sz("-*=[0123456789ABCDEF]=*-", str_enc);
            let irregular = String64::from_cbor_bytes(&irregular_bytes).unwrap();
            assert_eq!(irregular_bytes, irregular.to_cbor_bytes());
        }
        let _ = String64::from_cbor_bytes(&cbor_str_sz(&(0..64).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Two))).unwrap();
        assert_decode_reject_reason::<String64>(&cbor_str_sz(&(0..65).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Two)), "65 not in range 0 - 64");
    }

    #[test]
    fn string1632() {
        let str_24_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Four),
            StringLenSz::Indefinite(vec![(12, Sz::Two), (12, Sz::One)]),
            StringLenSz::Indefinite(vec![(0, Sz::Inline), (4, Sz::Inline), (20, Sz::Four), (0, Sz::Eight)]),
        ];
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        for str_enc in &str_24_encodings {
            for def_enc in &def_encodings {
                let irregular_bytes = vec![
                    cbor_tag_sz(7, *def_enc),
                        cbor_str_sz("-*=[0123456789ABCDEF]=*-", str_enc.clone()),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular = String1632::from_cbor_bytes(&irregular_bytes).unwrap();
                assert_eq!(irregular_bytes, irregular.to_cbor_bytes());
            }
        }
        let _ = String1632::from_cbor_bytes(&vec![
            cbor_tag_sz(7, Sz::One),
            cbor_str_sz(&(0..16).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::One)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>()).unwrap();
        let _ = String1632::from_cbor_bytes(&vec![
            cbor_tag_sz(7, Sz::Two),
            cbor_str_sz(&(0..32).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Two)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>()).unwrap();
        assert_decode_reject_reason::<String1632>(&vec![
            cbor_tag_sz(7, Sz::Inline),
            cbor_str_sz(&(0..15).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Inline)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>(), "15 not in range 16 - 32");
        assert_decode_reject_reason::<String1632>(&vec![
            cbor_tag_sz(7, Sz::Eight),
            cbor_str_sz(&(0..33).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Eight)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>(), "33 not in range 16 - 32");
    }

    #[test]
    fn type_choice() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_11_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Indefinite(vec![(5, Sz::Two), (6, Sz::One)]),
            StringLenSz::Indefinite(vec![(2, Sz::Inline), (0, Sz::Inline), (9, Sz::Four)]),
        ];
        for str_enc in &str_11_encodings {
            for def_enc in &def_encodings {
                let irregular_bytes_0 = cbor_int(0, *def_enc);
                let irregular_bytes_hello_world = cbor_str_sz("hello world", str_enc.clone());
                let irregular_bytes_uint = cbor_int(10, *def_enc);
                let irregular_bytes_text = cbor_str_sz("abcdefghijk", str_enc.clone());
                let irregular_bytes_tagged_arr = vec![
                    cbor_tag_sz(16, *def_enc),
                        arr_sz(2, *def_enc),
                            cbor_int(1, *def_enc),
                            cbor_int(3, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_0 = TypeChoice::from_cbor_bytes(&irregular_bytes_0).unwrap();
                assert_eq!(irregular_bytes_0, irregular_0.to_cbor_bytes());
                let irregular_hello_world = TypeChoice::from_cbor_bytes(&irregular_bytes_hello_world).unwrap();
                assert_eq!(irregular_bytes_hello_world, irregular_hello_world.to_cbor_bytes());
                let irregular_uint = TypeChoice::from_cbor_bytes(&irregular_bytes_uint).unwrap();
                assert_eq!(irregular_bytes_uint, irregular_uint.to_cbor_bytes());
                let irregular_text = TypeChoice::from_cbor_bytes(&irregular_bytes_text).unwrap();
                assert_eq!(irregular_bytes_text, irregular_text.to_cbor_bytes());
                let irregular_tagged_arr = TypeChoice::from_cbor_bytes(&irregular_bytes_tagged_arr).unwrap();
                assert_eq!(irregular_bytes_tagged_arr, irregular_tagged_arr.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn overlapping_inlined() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_11_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Indefinite(vec![(5, Sz::Two), (6, Sz::One)]),
            StringLenSz::Indefinite(vec![(2, Sz::Inline), (0, Sz::Inline), (9, Sz::Four)]),
        ];
        for def_enc in &def_encodings {
            // one
            let irregular_bytes_one = vec![
                arr_sz(1, *def_enc),
                    cbor_int(0, *def_enc),
            ].into_iter().flatten().clone().collect::<Vec<u8>>();
            let irregular_one = OverlappingInlined::from_cbor_bytes(&irregular_bytes_one).unwrap();
            assert_eq!(irregular_bytes_one, irregular_one.to_cbor_bytes());
            assert!(matches!(irregular_one, OverlappingInlined::One { .. }));
            // two
            let irregular_bytes_two = vec![
                vec![ARR_INDEF],
                    cbor_int(0, *def_enc),
                    cbor_int(u64::MAX as i128, Sz::Eight),
                vec![BREAK],
            ].into_iter().flatten().clone().collect::<Vec<u8>>();
            let irregular_two = OverlappingInlined::from_cbor_bytes(&irregular_bytes_two).unwrap();
            assert_eq!(irregular_bytes_two, irregular_two.to_cbor_bytes());
            assert!(matches!(irregular_two, OverlappingInlined::Two { .. }));
            for str_enc in &str_11_encodings {
                // three
                let irregular_bytes_three = vec![
                    arr_sz(3, *def_enc),
                        cbor_int(0, *def_enc),
                        cbor_int(0, *def_enc),
                        cbor_str_sz("overlapping", str_enc.clone()),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_three = OverlappingInlined::from_cbor_bytes(&irregular_bytes_three).unwrap();
                assert_eq!(irregular_bytes_three, irregular_three.to_cbor_bytes());
                assert!(matches!(irregular_three, OverlappingInlined::Three { .. }));
            }
        }
    }

    #[test]
    fn non_overlapping_type_choice_some() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_11_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Indefinite(vec![(5, Sz::Two), (6, Sz::One)]),
            StringLenSz::Indefinite(vec![(2, Sz::Inline), (0, Sz::Inline), (9, Sz::Four)]),
        ];
        for str_enc in &str_11_encodings {
            for def_enc in &def_encodings {
                let irregular_bytes_uint = cbor_int(0, *def_enc);
                let irregular_bytes_nint = cbor_int(-9, *def_enc);
                let irregular_bytes_text = cbor_str_sz("abcdefghijk", str_enc.clone());
                let irregular_uint = NonOverlappingTypeChoiceSome::from_cbor_bytes(&irregular_bytes_uint).unwrap();
                assert_eq!(irregular_bytes_uint, irregular_uint.to_cbor_bytes());
                let irregular_nint = NonOverlappingTypeChoiceSome::from_cbor_bytes(&irregular_bytes_nint).unwrap();
                assert_eq!(irregular_bytes_nint, irregular_nint.to_cbor_bytes());
                let irregular_text = NonOverlappingTypeChoiceSome::from_cbor_bytes(&irregular_bytes_text).unwrap();
                assert_eq!(irregular_bytes_text, irregular_text.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn non_overlapping_type_choice_all() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_11_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Indefinite(vec![(5, Sz::Two), (6, Sz::One)]),
            StringLenSz::Indefinite(vec![(2, Sz::Inline), (0, Sz::Inline), (9, Sz::Four)]),
        ];
        for str_enc in &str_11_encodings {
            for def_enc in &def_encodings {
                let irregular_bytes_uint = cbor_int(0, *def_enc);
                let irregular_bytes_nint = cbor_int(-9, *def_enc);
                let irregular_bytes_text = cbor_str_sz("abcdefghijk", str_enc.clone());
                let irregular_bytes_bytes = cbor_bytes_sz(vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], str_enc.clone());
                let irregular_bytes_hello_world = vec![
                    cbor_tag_sz(13, *def_enc),
                        cbor_str_sz("hello world", str_enc.clone())
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_arr = vec![
                    arr_sz(2, *def_enc),
                        cbor_int(1, *def_enc),
                        cbor_int(3, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_map = vec![
                    map_sz(2, *def_enc),
                        cbor_str_sz("11111111111", str_enc.clone()),
                            cbor_int(1, *def_enc),
                        cbor_str_sz("33333333333", str_enc.clone()),
                            cbor_int(3, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_uint = NonOverlappingTypeChoiceAll::from_cbor_bytes(&irregular_bytes_uint).unwrap();
                assert_eq!(irregular_bytes_uint, irregular_uint.to_cbor_bytes());
                let irregular_nint = NonOverlappingTypeChoiceAll::from_cbor_bytes(&irregular_bytes_nint).unwrap();
                assert_eq!(irregular_bytes_nint, irregular_nint.to_cbor_bytes());
                let irregular_text = NonOverlappingTypeChoiceAll::from_cbor_bytes(&irregular_bytes_text).unwrap();
                assert_eq!(irregular_bytes_text, irregular_text.to_cbor_bytes());
                let irregular_bytes = NonOverlappingTypeChoiceAll::from_cbor_bytes(&irregular_bytes_bytes).unwrap();
                assert_eq!(irregular_bytes_bytes, irregular_bytes.to_cbor_bytes());
                let irregular_hello_world = NonOverlappingTypeChoiceAll::from_cbor_bytes(&irregular_bytes_hello_world).unwrap();
                assert_eq!(irregular_bytes_hello_world, irregular_hello_world.to_cbor_bytes());
                let irregular_arr = NonOverlappingTypeChoiceAll::from_cbor_bytes(&irregular_bytes_arr).unwrap();
                assert_eq!(irregular_bytes_arr, irregular_arr.to_cbor_bytes());
                let irregular_map = NonOverlappingTypeChoiceAll::from_cbor_bytes(&irregular_bytes_map).unwrap();
                assert_eq!(irregular_bytes_map, irregular_map.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn overlap_basic_embed() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_32_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Indefinite(vec![(16, Sz::Two), (16, Sz::One)]),
            StringLenSz::Indefinite(vec![(10, Sz::Inline), (0, Sz::Inline), (22, Sz::Four)]),
        ];
        for str_enc in &str_32_encodings {
            for def_enc in &def_encodings {
                let irregular_bytes_identity = vec![
                    arr_sz(1, *def_enc),
                        cbor_int(0, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_x = vec![
                    arr_sz(2, *def_enc),
                        cbor_int(1, *def_enc),
                        cbor_bytes_sz(vec![170; 32], str_enc.clone()),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_identity = OverlapBasicEmbed::from_cbor_bytes(&irregular_bytes_identity).unwrap();
                assert_eq!(irregular_bytes_identity, irregular_identity.to_cbor_bytes());
                let irregular_x = OverlapBasicEmbed::from_cbor_bytes(&irregular_bytes_x).unwrap();
                assert_eq!(irregular_bytes_x, irregular_x.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn non_overlap_basic_embed() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_5_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Indefinite(vec![(3, Sz::Two), (2, Sz::One)]),
            StringLenSz::Indefinite(vec![(0, Sz::Eight), (1, Sz::Inline), (0, Sz::Inline), (4, Sz::Four), (0, Sz::Inline)]),
        ];
        for str_enc in &str_5_encodings {
            for def_enc in &def_encodings {
                let irregular_bytes_first = vec![
                    arr_sz(2, *def_enc),
                        cbor_int(10, *def_enc),
                        cbor_int(0, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_second = vec![
                    arr_sz(2, *def_enc),
                        cbor_str_sz("world", str_enc.clone()),
                        cbor_int(1, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_first = NonOverlapBasicEmbed::from_cbor_bytes(&irregular_bytes_first).unwrap();
                assert_eq!(irregular_bytes_first, irregular_first.to_cbor_bytes());
                let irregular_second = NonOverlapBasicEmbed::from_cbor_bytes(&irregular_bytes_second).unwrap();
                assert_eq!(irregular_bytes_second, irregular_second.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn non_overlap_basic_embed_multi_fields() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_5_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Indefinite(vec![(3, Sz::Two), (2, Sz::One)]),
            StringLenSz::Indefinite(vec![(0, Sz::Eight), (1, Sz::Inline), (0, Sz::Inline), (4, Sz::Four), (0, Sz::Inline)]),
        ];
        for str_enc in &str_5_encodings {
            for def_enc in &def_encodings {
                let irregular_bytes_first = vec![
                    arr_sz(2, *def_enc),
                        cbor_int(10, *def_enc),
                        cbor_int(11, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_second = vec![
                    arr_sz(2, *def_enc),
                        cbor_str_sz("HELLO", str_enc.clone()),
                        cbor_int(0, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_first = NonOverlapBasicEmbedMultiFields::from_cbor_bytes(&irregular_bytes_first).unwrap();
                assert_eq!(irregular_bytes_first, irregular_first.to_cbor_bytes());
                let irregular_second = NonOverlapBasicEmbedMultiFields::from_cbor_bytes(&irregular_bytes_second).unwrap();
                assert_eq!(irregular_bytes_second, irregular_second.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn non_overlap_basic_embed_mixed() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_5_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Indefinite(vec![(3, Sz::Two), (2, Sz::One)]),
            StringLenSz::Indefinite(vec![(0, Sz::Eight), (1, Sz::Inline), (0, Sz::Inline), (4, Sz::Four), (0, Sz::Inline)]),
        ];
        for str_enc in &str_5_encodings {
            for def_enc in &def_encodings {
                let irregular_bytes_first = vec![
                    arr_sz(2, *def_enc),
                        cbor_int(10, *def_enc),
                        cbor_int(0, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_second = vec![
                    arr_sz(2, *def_enc),
                        cbor_str_sz("world", str_enc.clone()),
                        cbor_int(1, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_first = NonOverlapBasicEmbedMixed::from_cbor_bytes(&irregular_bytes_first).unwrap();
                assert_eq!(irregular_bytes_first, irregular_first.to_cbor_bytes());
                let irregular_second = NonOverlapBasicEmbedMixed::from_cbor_bytes(&irregular_bytes_second).unwrap();
                assert_eq!(irregular_bytes_second, irregular_second.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn non_overlap_basic_embed_mixed_explicit() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_5_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Indefinite(vec![(3, Sz::Two), (2, Sz::One)]),
            StringLenSz::Indefinite(vec![(0, Sz::Eight), (1, Sz::Inline), (0, Sz::Inline), (4, Sz::Four), (0, Sz::Inline)]),
        ];
        for str_enc in &str_5_encodings {
            for def_enc in &def_encodings {
                let irregular_bytes_first = vec![
                    arr_sz(2, *def_enc),
                        cbor_int(10, *def_enc),
                        cbor_int(0, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_second = vec![
                    arr_sz(2, *def_enc),
                        cbor_str_sz("MiXeD", str_enc.clone()),
                        cbor_int(1, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_third = vec![
                    arr_sz(2, *def_enc),
                        cbor_bytes_sz(vec![0x00, 0x01, 0x02, 0x03, 0x04], str_enc.clone()),
                        cbor_int(1, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_first = NonOverlapBasicEmbedMixedExplicit::from_cbor_bytes(&irregular_bytes_first).unwrap();
                assert_eq!(irregular_bytes_first, irregular_first.to_cbor_bytes());
                let irregular_second = NonOverlapBasicEmbedMixedExplicit::from_cbor_bytes(&irregular_bytes_second).unwrap();
                assert_eq!(irregular_bytes_second, irregular_second.to_cbor_bytes());
                let irregular_third = NonOverlapBasicEmbedMixedExplicit::from_cbor_bytes(&irregular_bytes_third).unwrap();
                assert_eq!(irregular_bytes_third, irregular_third.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn non_overlap_basic_not_basic() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_5_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Indefinite(vec![(3, Sz::Two), (2, Sz::One)]),
            StringLenSz::Indefinite(vec![(0, Sz::Eight), (1, Sz::Inline), (0, Sz::Inline), (4, Sz::Four), (0, Sz::Inline)]),
        ];
        for str_enc in &str_5_encodings {
            for def_enc in &def_encodings {
                let irregular_bytes_group = vec![
                    arr_sz(2, *def_enc),
                        cbor_int(0, *def_enc),
                        cbor_str_sz("hello", str_enc.clone()),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_group_arr = vec![
                    vec![ARR_INDEF],
                        arr_sz(2, *def_enc),
                            cbor_int(1, *def_enc),
                            cbor_str_sz("world", str_enc.clone()),
                    vec![BREAK],
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_group_tagged = vec![
                    arr_sz(2, *def_enc),
                        cbor_tag_sz(11, *def_enc),
                            cbor_int(3, *def_enc),
                            cbor_str_sz(" test", str_enc.clone()),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_group_bytes = vec![
                    arr_sz(1, *def_enc),
                        cbor_bytes_sz(irregular_bytes_group.clone(), StringLenSz::Len(Sz::Two)),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_group = NonOverlapBasicNotBasic::from_cbor_bytes(&irregular_bytes_group).unwrap();
                assert_eq!(irregular_bytes_group, irregular_group.to_cbor_bytes());
                let irregular_group_arr = NonOverlapBasicNotBasic::from_cbor_bytes(&irregular_bytes_group_arr).unwrap();
                assert_eq!(irregular_bytes_group_arr, irregular_group_arr.to_cbor_bytes());
                let irregular_group_tagged = NonOverlapBasicNotBasic::from_cbor_bytes(&irregular_bytes_group_tagged).unwrap();
                assert_eq!(irregular_bytes_group_tagged, irregular_group_tagged.to_cbor_bytes());
                let irregular_group_bytes = NonOverlapBasicNotBasic::from_cbor_bytes(&irregular_bytes_group_bytes).unwrap();
                assert_eq!(irregular_bytes_group_bytes, irregular_group_bytes.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn enums() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let enum_values = vec![3, 1, 4];
        for def_enc in &def_encodings {
            for enum_value in &enum_values {
                let irregular_bytes = vec![
                    arr_sz(2, *def_enc),
                        // CEnum
                        cbor_int(*enum_value, *def_enc),
                        // TypeChoice
                        cbor_int(0, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular = Enums::from_cbor_bytes(&irregular_bytes).unwrap();
                assert_eq!(irregular_bytes, irregular.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn group_choice() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_6_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Indefinite(vec![(3, Sz::Two), (3, Sz::One)]),
            StringLenSz::Indefinite(vec![(2, Sz::Inline), (0, Sz::Inline), (4, Sz::Four)]),
        ];
        for str_enc in &str_6_encodings {
            for def_enc in &def_encodings {
                let irregular_bytes_3 = vec![
                    arr_sz(1, *def_enc),
                        cbor_int(3, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_tagged_2 = vec![
                    arr_sz(1, *def_enc),
                        cbor_tag_sz(10, *def_enc),
                            cbor_int(2, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_foo = vec![
                    vec![ARR_INDEF],
                        cbor_tag_sz(11, *def_enc),
                            arr_sz(3, *def_enc),
                                cbor_int(9, *def_enc),
                                cbor_str_sz("potato", str_enc.clone()),
                                cbor_bytes_sz(vec![0xF0, 0x0D, 0xF0, 0x0D, 0xF0, 0x0D], str_enc.clone()),
                    vec![BREAK],
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_inlined = vec![
                    arr_sz(2, *def_enc),
                        cbor_int(0, *def_enc),
                        cbor_int(10, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_plain = vec![
                    arr_sz(2, *def_enc),
                        cbor_tag_sz(13, *def_enc),
                            cbor_int(17, *def_enc),
                        cbor_tag_sz(9, *def_enc),
                            cbor_str_sz("carrot", str_enc.clone()),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_3 = GroupChoice::from_cbor_bytes(&irregular_bytes_3).unwrap();
                assert_eq!(irregular_bytes_3, irregular_3.to_cbor_bytes());
                let irregular_tagged_2 = GroupChoice::from_cbor_bytes(&irregular_bytes_tagged_2).unwrap();
                assert_eq!(irregular_bytes_tagged_2, irregular_tagged_2.to_cbor_bytes());
                let irregular_foo = GroupChoice::from_cbor_bytes(&irregular_bytes_foo).unwrap();
                assert_eq!(irregular_bytes_foo, irregular_foo.to_cbor_bytes());
                let irregular_inlined = GroupChoice::from_cbor_bytes(&irregular_bytes_inlined).unwrap();
                assert_eq!(irregular_bytes_inlined, irregular_inlined.to_cbor_bytes());
                let irregular_plain = GroupChoice::from_cbor_bytes(&irregular_bytes_plain).unwrap();
                assert_eq!(irregular_bytes_plain, irregular_plain.to_cbor_bytes());
            }
        }
    }

    #[test]
    fn cbor_in_cbor() {
        let str_3_encodings = vec![
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Len(Sz::Eight),
            StringLenSz::Indefinite(vec![(2, Sz::One), (1, Sz::Four)])
        ];
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        for def_enc in def_encodings.iter() {
            for str_enc in str_3_encodings.iter() {
                let irregular_foo_bytes = vec![
                    cbor_tag_sz(11, *def_enc),
                            arr_sz(3, *def_enc),
                                cbor_int(5, *def_enc),
                                cbor_str_sz("???", str_enc.clone()),
                                cbor_bytes_sz(vec![0xAB, 0xCD, 0xEF], str_enc.clone())
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let foo_bytes_enc = StringLenSz::Indefinite(vec![(5, Sz::Inline), ((irregular_foo_bytes.len() - 5) as u64, Sz::Eight)]);
                let irregular_bytes = vec![
                    arr_sz(3, *def_enc),    
                        cbor_bytes_sz(irregular_foo_bytes.clone(), foo_bytes_enc.clone()),
                        cbor_bytes_sz(cbor_int(5, *def_enc), StringLenSz::Len(*def_enc)),
                        cbor_tag_sz(20, *def_enc),
                            cbor_bytes_sz(irregular_foo_bytes, foo_bytes_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular = CborInCbor::from_cbor_bytes(&irregular_bytes).unwrap();
                assert_eq!(irregular_bytes, irregular.to_cbor_bytes());
            }
        }
    }

    // The INLINE nested spelling: both `.cbor` depths on ONE chain, so each level's byte-string
    // `StringEncoding` lands in a sidecar member of the SAME owning name and the depth suffix is all
    // that separates them (`pair_bytes_encoding` / `pair_bytes2_encoding`). Driving the levels
    // INDEPENDENTLY is the point — a shared member, or an outer final expr reading the inner level's
    // encoding, still round-trips whenever the two levels happen to be encoded alike, so every
    // combination of (outer head, inner head) is walked and each must re-encode byte-identically.
    #[test]
    fn cbor_inline_nested_payloads() {
        // Each byte-string head gets its own spelling: minimal, deliberately over-wide, and
        // indefinite chunking (which a level can only reproduce from its own recorded encoding).
        let bstr_enc = |bytes: &[u8], which: usize| -> StringLenSz {
            match which {
                0 => StringLenSz::Len(Sz::Inline),
                1 => StringLenSz::Len(Sz::Eight),
                _ if bytes.len() >= 2 => StringLenSz::Indefinite(vec![
                    (1, Sz::Inline),
                    ((bytes.len() - 1) as u64, Sz::Four),
                ]),
                _ => StringLenSz::Indefinite(vec![(bytes.len() as u64, Sz::Two)]),
            }
        };
        for def_enc in [Sz::Inline, Sz::One, Sz::Four].iter() {
            for outer in 0..3 {
                for inner in 0..3 {
                    // pair = bytes .cbor (bytes .cbor uint), value 5
                    let pair_val = cbor_int(5, *def_enc);
                    let pair_l1 = cbor_bytes_sz(pair_val.clone(), bstr_enc(&pair_val, inner));
                    let pair = cbor_bytes_sz(pair_l1.clone(), bstr_enc(&pair_l1, outer));
                    // triple = bytes .cbor (bytes .cbor (bytes .cbor uint)), value 7 — the level-3
                    // vector: its middle level takes the third spelling so no two levels agree.
                    let tri_val = cbor_int(7, *def_enc);
                    let tri_l1 = cbor_bytes_sz(tri_val.clone(), bstr_enc(&tri_val, inner));
                    let tri_l2 = cbor_bytes_sz(tri_l1.clone(), bstr_enc(&tri_l1, (outer + inner + 1) % 3));
                    let tri = cbor_bytes_sz(tri_l2.clone(), bstr_enc(&tri_l2, outer));
                    let irregular_bytes = vec![
                        arr_sz(3, *def_enc),
                            pair,
                            tri,
                            cbor_str_sz("z", StringLenSz::Len(*def_enc)),
                    ].into_iter().flatten().collect::<Vec<u8>>();
                    let irregular = CborInlineNestedPayloads::from_cbor_bytes(&irregular_bytes).unwrap();
                    assert_eq!(irregular.pair, 5);
                    assert_eq!(irregular.triple, 7);
                    assert_eq!(irregular.tail, "z");
                    assert_eq!(irregular_bytes, irregular.to_cbor_bytes());
                }
            }
        }
    }

    #[test]
    fn signed_ints() {
        use std::cmp::min;
        let umins = [0i128, u8::MIN as i128, u16::MIN as i128, u32::MIN as i128, u64::MIN as i128];
        let umaxs = [23i128, u8::MAX as i128, u16::MAX as i128, u32::MAX as i128, u64::MAX as i128];
        let imins = [-24i128, i8::MIN as i128, i16::MIN as i128, i32::MIN as i128, i64::MIN as i128];
        let imaxs = [-1i128, i8::MAX as i128, i16::MAX as i128, i32::MAX as i128, i64::MAX as i128];
        let def_encodings = [Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        for i in 0..5 {
            let i_8 = min(1, i);
            let i_16 = min(2, i);
            let i_32 = min(3, i);
            let i_64 = min(4, i);
            let irregular_bytes_min = vec![
                vec![ARR_INDEF],
                    // uints
                    cbor_int(umins[i_8], def_encodings[i]),
                    cbor_int(umins[i_16], def_encodings[i]),
                    cbor_int(umins[i_32], def_encodings[i]),
                    cbor_int(umins[i_64], def_encodings[i]),
                    // ints
                    cbor_int(imins[i_8], def_encodings[i]),
                    cbor_int(imins[i_16], def_encodings[i]),
                    cbor_int(imins[i_32], def_encodings[i]),
                    cbor_int(imins[i_64], def_encodings[i]),
                    // nint
                    cbor_int(-1 - umins[i_64], def_encodings[i]),
                    // u64 max const
                    cbor_int(u64::MAX as i128, Sz::Eight),
                    // i64 min const
                    cbor_int(i64::MIN as i128, Sz::Eight),
                vec![BREAK],
            ].into_iter().flatten().clone().collect::<Vec<u8>>();
            let irregular_min = SignedInts::from_cbor_bytes(&irregular_bytes_min).unwrap();
            assert_eq!(irregular_bytes_min, irregular_min.to_cbor_bytes());
            let irregular_bytes_max = vec![
                arr_sz(11, def_encodings[i]),
                    // uints
                    cbor_int(umaxs[i_8], def_encodings[i]),
                    cbor_int(umaxs[i_16], def_encodings[i]),
                    cbor_int(umaxs[i_32], def_encodings[i]),
                    cbor_int(umaxs[i_64], def_encodings[i]),
                    // ints
                    cbor_int(imaxs[i_8], def_encodings[i]),
                    cbor_int(imaxs[i_16], def_encodings[i]),
                    cbor_int(imaxs[i_32], def_encodings[i]),
                    cbor_int(imaxs[i_64], def_encodings[i]),
                    // nint
                    cbor_int(-1 - umaxs[i_64], def_encodings[i]),
                    // u64 max const
                    cbor_int(u64::MAX as i128, Sz::Eight),
                    // i64 min const
                    cbor_int(i64::MIN as i128, Sz::Eight),
            ].into_iter().flatten().clone().collect::<Vec<u8>>();
            let irregular_max = SignedInts::from_cbor_bytes(&irregular_bytes_max).unwrap();
            assert_eq!(irregular_bytes_max, irregular_max.to_cbor_bytes());
        }
    }

    #[test]
    fn defaults() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_3_encodings = vec![
            StringLenSz::Len(Sz::Eight),
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Indefinite(vec![(1, Sz::Two), (2, Sz::One)]),
            StringLenSz::Indefinite(vec![(2, Sz::Inline), (0, Sz::Inline), (1, Sz::Four)]),
        ];
        let bools = [(false, true), (true, false), (true, true)];
        for str_enc in &str_3_encodings {
            for def_enc in &def_encodings {
                for ((key_1_present, key_1_default), (key_2_present, key_2_default)) in bools.iter().zip(bools.iter()) {
                    let value_1: u64 = if *key_1_default { 1337 } else { 2 };
                    let value_2 = if *key_2_default { "two" } else { "one" };
                    let irregular_bytes = vec![
                        vec![MAP_INDEF],
                            if *key_1_present {
                                vec![
                                    cbor_int(1, *def_enc),
                                        cbor_int(value_1 as i128, Sz::Two),
                                ].into_iter().flatten().clone().collect::<Vec<u8>>()
                            } else {
                                vec![]
                            },
                            if *key_2_present {
                                vec![
                                    cbor_int(2, *def_enc),
                                        cbor_str_sz(value_2, str_enc.clone()),
                                ].into_iter().flatten().clone().collect::<Vec<u8>>()
                            } else {
                                vec![]
                            },
                        vec![BREAK],
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular = MapWithDefaults::from_cbor_bytes(&irregular_bytes).unwrap();
                    assert_eq!(irregular_bytes, irregular.to_cbor_bytes());
                    assert_eq!(irregular.key_1, value_1);
                    assert_eq!(irregular.key_2, value_2);
                }
            }
        }
    }

    #[test]
    fn array_opt_fields() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_12_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Indefinite(vec![(5, Sz::Two), (7, Sz::One)]),
            StringLenSz::Indefinite(vec![(3, Sz::Inline), (0, Sz::Inline), (9, Sz::Four)]),
        ];
        for str_enc in &str_12_encodings {
            for def_enc in &def_encodings {
                let e_values = [
                    None,
                    Some(NonOverlappingTypeChoiceSome::U64 {
                        uint: 5,
                        uint_encoding: Some(*def_enc),
                    }),
                    Some(NonOverlappingTypeChoiceSome::N64 {
                        n64: 4,
                        n64_encoding: Some(*def_enc),
                    }),
                    Some(NonOverlappingTypeChoiceSome::Text {
                        text: "twelve chars".to_owned(),
                        text_encoding: str_enc.clone().into(),
                    }),
                ];
                for e in &e_values {
                    for a in [false, true] {
                        for b in [false, true] {
                            for d in [false, true] {
                                // TODO: preserve-encodings remembering optional fixed values. Issue: https://github.com/dcSpark/cddl-codegen/issues/205
                                // for x in [false, true] {
                                //     for z in [false, true] {
                                        let mut components: Vec<Vec<u8>> = vec![vec![ARR_INDEF]];
                                        // if x {
                                        //     components.push(vec![0xf5]);
                                        // }
                                        if a {
                                            components.push(cbor_int(0, *def_enc));
                                        }
                                        if b {
                                            components.push(cbor_str_sz("hello, world", str_enc.clone()));
                                        }
                                        // c
                                        components.push(cbor_int(-10, *def_enc));
                                        if d {
                                            components.push(cbor_str_sz("cddl-codegen", str_enc.clone()));
                                        }
                                        // y
                                        components.push(cbor_tag_sz(10, *def_enc));
                                        components.push(cbor_int(1, *def_enc));
                                        if let Some(e) = &e {
                                            components.push(e.to_cbor_bytes());
                                        }
                                        // if z {
                                        //     //components.push(vec![NULL]);
                                        // }
                                        components.push(vec![BREAK]);
                                        let irregular_bytes = components.into_iter().flatten().clone().collect::<Vec<u8>>();
                                        let irregular = ArrayOptFields::from_cbor_bytes(&irregular_bytes).unwrap();
                                        assert_eq!(irregular_bytes, irregular.to_cbor_bytes());
                                //     }
                                // }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn bounds() {
        // here we're just making sure that the code compiles + checks bounds with the preserve-encodings codegen
        // all members here have their round-trip checked in other tests.
        enum OOB {
            Below,
            Lower,
            Upper,
            Above,
        }
        let make_bounds = |w_out: OOB, x_out: OOB, y_out: OOB, z_out: OOB, a_out: OOB, b_out: OOB| {
            let cbor = vec![
                arr_def(6),
                    cbor_int(match w_out {
                        OOB::Below => -1001,
                        OOB::Lower => -1000,
                        OOB::Upper => 1000,
                        OOB::Above => 1001,
                    }, cbor_event::Sz::Two),
                    cbor_int(match x_out {
                        OOB::Below => panic!(),
                        OOB::Lower => panic!(),
                        OOB::Upper => 7,
                        OOB::Above => 8,
                    }, cbor_event::Sz::Inline),
                    cbor_int(match y_out {
                        OOB::Below => -6,
                        OOB::Lower => -5,
                        OOB::Upper => panic!(),
                        OOB::Above => panic!(),
                    }, cbor_event::Sz::Inline),
                    cbor_string(match z_out {
                        OOB::Below => "ab",
                        OOB::Lower => "abc",
                        OOB::Upper => "abcdefghijklmn",
                        OOB::Above => "abcdefghijklmno",
                    }),
                    vec![ARR_INDEF],
                        match a_out {
                            OOB::Below => vec![],
                            OOB::Lower => vec![0x00],
                            OOB::Upper => vec![0x00, 0x01, 0x02],
                            OOB::Above => vec![0x00, 0x01, 0x02, 0x03],
                        },
                    vec![BREAK],
                    vec![MAP_INDEF],
                        match b_out {
                            OOB::Below => panic!(),
                            OOB::Lower => panic!(),
                            OOB::Upper => vec![0x00, 0x00, 0x01, 0x01, 0x02, 0x02],
                            OOB::Above => vec![0x00, 0x00, 0x01, 0x01, 0x02, 0x02, 0x03, 0x03],
                        },
                    vec![BREAK],
            ].into_iter().flatten().clone().collect::<Vec<u8>>();
            Bounds::from_cbor_bytes(&cbor)
        };
        let good1 = make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Lower, OOB::Lower, OOB::Upper).unwrap();
        deser_test(&good1);
        let good2 = make_bounds(OOB::Upper, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Upper, OOB::Upper).unwrap();
        deser_test(&good2);
        // w oob
        assert!(make_bounds(OOB::Below, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Upper, OOB::Upper).is_err());
        assert!(make_bounds(OOB::Above, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Upper, OOB::Upper).is_err());
        // x oob
        assert!(make_bounds(OOB::Lower, OOB::Above, OOB::Lower, OOB::Upper, OOB::Upper, OOB::Upper).is_err());
        // y oob
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Below, OOB::Upper, OOB::Upper, OOB::Upper).is_err());
        // z oob
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Below, OOB::Upper, OOB::Upper).is_err());
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Above, OOB::Upper, OOB::Upper).is_err());
        // a oob
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Below, OOB::Upper).is_err());
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Above, OOB::Upper).is_err());
        // b oob
        assert!(make_bounds(OOB::Lower, OOB::Upper, OOB::Lower, OOB::Upper, OOB::Upper, OOB::Above).is_err());

        // type and group choices share the same deserialization code so we only check the API
        BoundsTypeChoice::new_bytes(vec![0; 64]).unwrap();
        assert!(BoundsTypeChoice::new_bytes(vec![0; 65]).is_err());
        BoundsGroupChoice::new_a(0, "four".to_owned()).unwrap();
        assert!(BoundsGroupChoice::new_a(0, "hello".to_owned()).is_err());
        deser_test(&BoundsGroupChoice::new_c(Hash::new(vec![]).unwrap(), Hash::new(vec![]).unwrap()));
    }

    #[test]
    fn sign_bounds() {
        // Same per-sign-arm partition as the default-mode fixture, but under --preserve-encodings
        // BOTH arms go through the classification (no i64-special nint case), so this pins the
        // preserve-only panics: le_pos / ge_pos / ne_pos used to `unreachable!()` at generation.
        // Fields: all_neg -10..-3, upto_zero -10..0, le_neg int .le -3, le_pos int .le 10,
        // ge_pos int .ge 3, ne_pos int .ne 5, ne_neg int .ne -5, straddle -10..3,
        // ne_one int .ne 1, ne_zero int .ne 0.
        let base: [i128; 10] = [-5, -5, -5, 10, 3, 4, -4, 0, 0, 1];
        let make = |idx: usize, v: i128| {
            let mut vals = base;
            vals[idx] = v;
            let mut cbor = arr_def(10);
            for x in vals.iter() {
                cbor.extend(cbor_int(*x, cbor_event::Sz::Eight));
            }
            SignBounds::from_cbor_bytes(&cbor)
        };
        let baseline = SignBounds::new(-5, -5, -5, 10, 3, 4, -4, 0, 0, 1).unwrap();
        deser_test(&baseline);
        make(0, -5).unwrap();

        // all_neg (-10..-3): rejects ANY uint and both out-of-window sides; accepts endpoints.
        assert!(make(0, 5).is_err());
        assert!(make(0, -2).is_err());
        assert!(make(0, -11).is_err());
        make(0, -3).unwrap();
        make(0, -10).unwrap();

        // upto_zero (-10..0): upper endpoint 0 constraining.
        make(1, 0).unwrap();
        make(1, -10).unwrap();
        assert!(make(1, 1).is_err());
        assert!(make(1, -11).is_err());

        // le_neg (int .le -3): rejects any uint.
        assert!(make(2, 5).is_err());
        assert!(make(2, -2).is_err());
        make(2, -3).unwrap();
        make(2, -10).unwrap();

        // le_pos (int .le 10): nint arm VACUOUS — must accept a large negative (panicked before).
        make(3, -999999).unwrap();
        make(3, 10).unwrap();
        assert!(make(3, 11).is_err());

        // ge_pos (int .ge 3): nint arm EMPTY — rejects every negative (panicked before).
        make(4, 3).unwrap();
        make(4, 100).unwrap();
        assert!(make(4, 2).is_err());
        assert!(make(4, -1).is_err());

        // ne_pos (int .ne 5): excluded value non-negative → only the uint arm checks it (panicked before).
        make(5, -5).unwrap();
        make(5, 4).unwrap();
        make(5, 6).unwrap();
        assert!(make(5, 5).is_err());

        // ne_neg (int .ne -5): excluded value negative → only the nint arm checks it.
        make(6, 5).unwrap();
        make(6, -4).unwrap();
        make(6, -6).unwrap();
        assert!(make(6, -5).is_err());

        // straddle (-10..3): survivor — must stay byte-identical to pre-fix and behave correctly.
        make(7, -10).unwrap();
        make(7, 3).unwrap();
        make(7, 0).unwrap();
        assert!(make(7, -11).is_err());
        assert!(make(7, 4).is_err());

        // ne_one (int .ne 1): the excluded-value boundary where the (N+1, N-1) exclusion encoding's
        // max hits 0 — a per-side partition of (2, 0) once emitted `x < 2`, silently rejecting 0.
        // Under preserve BOTH arms classify, so this pins the boundary in the partitioned uint arm.
        make(8, 0).unwrap(); // the value the mis-check rejected
        make(8, 2).unwrap();
        make(8, -1).unwrap(); // nint arm is unconstrained by a non-negative exclusion
        assert!(make(8, 1).is_err());

        // ne_zero (int .ne 0): encoding (1, -1) has a bound on each side of the sign split; only 0
        // may reject.
        make(9, 1).unwrap();
        make(9, -1).unwrap();
        assert!(make(9, 0).is_err());
    }

    #[test]
    fn top_level_ranges() {
        // Literal-headed top-level range rules wrap into a bounds-enforcing struct (mirroring the
        // `int .op`-headed top-level wrappers). Same behavior as the default-mode fixture; pinned
        // here to exercise the preserve-encodings wrapper machinery on a literal-headed range and to
        // confirm a tagged rule still writes its tag under preserve.

        // top_level_neg_range = -10..-3, an i64 wrapper (its deserializer reads BOTH sign arms then
        // checks the whole window).
        let neg = |v: i128| TopLevelNegRange::from_cbor_bytes(&cbor_int(v, cbor_event::Sz::Eight));
        assert!(neg(5).is_err()); // any uint is out of an all-negative window
        assert!(neg(-11).is_err());
        assert!(neg(-2).is_err());
        neg(-3).unwrap();
        neg(-10).unwrap();
        deser_test(&TopLevelNegRange::new(-3).unwrap());
        deser_test(&TopLevelNegRange::new(-10).unwrap());

        // top_level_pos_range = 3..10, a u64 wrapper.
        let pos = |v: i128| TopLevelPosRange::from_cbor_bytes(&cbor_int(v, cbor_event::Sz::Eight));
        assert!(pos(2).is_err());
        assert!(pos(11).is_err());
        pos(3).unwrap();
        pos(10).unwrap();
        deser_test(&TopLevelPosRange::new(3).unwrap());
        deser_test(&TopLevelPosRange::new(10).unwrap());

        // top_level_tagged_range = #6.5(3..10): the wrapper writes tag 5 and requires it, plus the
        // window. A fresh value carries no captured encoding, so the tag/int are canonical inline.
        let tagged_ok = TopLevelTaggedRange::new(7).unwrap();
        let tagged_bytes = tagged_ok.to_cbor_bytes();
        assert_eq!(tagged_bytes[0], 0xc5); // major type 6 (tag) with argument 5
        assert_eq!(
            tagged_bytes,
            [cbor_tag(5), cbor_int(7, cbor_event::Sz::Inline)].concat()
        );
        deser_test(&tagged_ok);
        TopLevelTaggedRange::from_cbor_bytes(&tagged_bytes).unwrap();
        // untagged input is rejected (a bare `pub type = u64` alias would have accepted it)
        assert_decode_reject_reason::<TopLevelTaggedRange>(
            &cbor_int(7, cbor_event::Sz::Inline),
            "expected `Tag' byte received `UnsignedInteger'",
        );
        // out-of-window tagged input is rejected
        assert_decode_reject_reason::<TopLevelTaggedRange>(
            &[cbor_tag(5), cbor_int(11, cbor_event::Sz::Inline)].concat(),
            "11 not in range 3 - 10",
        );
        // wrong tag is rejected
        assert_decode_reject_reason::<TopLevelTaggedRange>(
            &[cbor_tag(4), cbor_int(7, cbor_event::Sz::Inline)].concat(),
            "Expected tag 5, found 4",
        );
    }

    #[test]
    fn used_as_key() {
        // this is just here to make sure this compiles (i.e. Hash/Eq traits are derived)
        let mut set_foo: std::collections::HashSet<Foo> = std::collections::HashSet::new();
        set_foo.insert(Foo::new(0, "text".to_owned(), vec![]));
        let mut set_non_overlap: std::collections::HashSet<NonOverlappingTypeChoiceSome> = std::collections::HashSet::new();
        set_non_overlap.insert(NonOverlappingTypeChoiceSome::new_uint(0));
    }

    #[test]
    fn enum_opt_embed_fields() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_3_encodings = vec![
            StringLenSz::Len(Sz::Eight),
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Indefinite(vec![(1, Sz::Two), (2, Sz::One)]),
            StringLenSz::Indefinite(vec![(2, Sz::Inline), (0, Sz::Inline), (1, Sz::Four)]),
        ];
        for str_enc in &str_3_encodings {
            for def_enc in &def_encodings {
                for opt_present in [false, true] {
                    // a
                    let irregular_bytes_a = vec![
                        vec![ARR_INDEF],
                            cbor_int(1, *def_enc),
                        vec![BREAK],
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_a = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_a).unwrap();
                    assert_eq!(irregular_bytes_a, irregular_a.to_cbor_bytes());
                    assert!(matches!(irregular_a, EnumOptEmbedFields::Ea { .. }));
                    // b (Some)
                    let irregular_bytes_b1 = vec![
                        vec![ARR_INDEF],
                            cbor_int(1, *def_enc),
                            cbor_str_sz("foo", str_enc.clone()),
                            cbor_int(5, *def_enc),
                        vec![BREAK],
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_b1 = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_b1).unwrap();
                    assert_eq!(irregular_bytes_b1, irregular_b1.to_cbor_bytes());
                    assert!(matches!(irregular_b1, EnumOptEmbedFields::Eb { .. }));
                    // b (None)
                    let irregular_bytes_b2 = vec![
                        arr_sz(2, *def_enc),
                            cbor_int(1, *def_enc),
                            cbor_int(5, *def_enc),
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_b2 = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_b2).unwrap();
                    assert_eq!(irregular_bytes_b2, irregular_b2.to_cbor_bytes());
                    assert!(matches!(irregular_b2, EnumOptEmbedFields::Eb { .. }));
                    // c
                    let irregular_bytes_c = vec![
                        vec![ARR_INDEF],
                            cbor_int(1, *def_enc),
                            cbor_int(u64::MAX as i128, Sz::Eight),
                            cbor_int(7, *def_enc),
                        vec![BREAK],
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_c = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_c).unwrap();
                    assert_eq!(irregular_bytes_c, irregular_c.to_cbor_bytes());
                    assert!(matches!(irregular_c, EnumOptEmbedFields::Ec { .. }));
                    // d (Some)
                    let irregular_bytes_d1 = vec![
                        arr_sz(3, *def_enc),
                            cbor_int(1, *def_enc),
                            cbor_int(0, *def_enc),
                            cbor_str_sz("bar", str_enc.clone()),
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_d1 = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_d1).unwrap();
                    assert_eq!(irregular_bytes_d1, irregular_d1.to_cbor_bytes());
                    assert!(matches!(irregular_d1, EnumOptEmbedFields::Ed { .. }));
                    // d (None)
                    let irregular_bytes_d2 = vec![
                        vec![ARR_INDEF],
                            cbor_int(1, *def_enc),
                            cbor_int(u64::MAX as i128, Sz::Eight),
                        vec![BREAK],
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_d2 = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_d2).unwrap();
                    assert_eq!(irregular_bytes_d2, irregular_d2.to_cbor_bytes());
                    assert!(matches!(irregular_d2, EnumOptEmbedFields::Ed { .. }));
                    // e (Some)
                    let irregular_bytes_e1 = vec![
                        vec![ARR_INDEF],
                            cbor_int(1, *def_enc),
                            cbor_int(0, *def_enc),
                            cbor_bytes_sz(vec![0x00, 0x01, 0x02], str_enc.clone()),
                            cbor_int(u64::MAX as i128, Sz::Eight),
                        vec![BREAK],
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_e1 = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_e1).unwrap();
                    assert_eq!(irregular_bytes_e1, irregular_e1.to_cbor_bytes());
                    assert!(matches!(irregular_e1, EnumOptEmbedFields::Ee { .. }));
                    // e (None)
                    let irregular_bytes_e2 = vec![
                        arr_sz(3, *def_enc),
                            cbor_int(1, *def_enc),
                            cbor_int(u64::MAX as i128, Sz::Eight),
                            cbor_int(0, *def_enc),
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_e2 = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_e2).unwrap();
                    assert_eq!(irregular_bytes_e2, irregular_e2.to_cbor_bytes());
                    assert!(matches!(irregular_e2, EnumOptEmbedFields::Ee { .. }));
                    // f (Some)
                    let irregular_bytes_f1 = vec![
                        arr_sz(3, *def_enc),
                            cbor_int(1, *def_enc),
                            cbor_int(u64::MAX as i128, Sz::Eight),
                            cbor_tag_sz(11, *def_enc),
                                cbor_int(11, *def_enc),
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_f1 = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_f1).unwrap();
                    assert_eq!(irregular_bytes_f1, irregular_f1.to_cbor_bytes());
                    assert!(matches!(irregular_f1, EnumOptEmbedFields::Ef { .. }));
                    // f (None)
                    let irregular_bytes_f2 = vec![
                        vec![ARR_INDEF],
                            cbor_int(1, *def_enc),
                            cbor_tag_sz(11, *def_enc),
                                cbor_int(11, *def_enc),
                        vec![BREAK],
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_f2 = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_f2).unwrap();
                    assert_eq!(irregular_bytes_f2, irregular_f2.to_cbor_bytes());
                    assert!(matches!(irregular_f2, EnumOptEmbedFields::Ef { .. }));
                    // g (Some)
                    let irregular_bytes_g1 = vec![
                        vec![ARR_INDEF],
                            cbor_int(1, *def_enc),
                            arr_sz(3, *def_enc),
                                cbor_int(0, *def_enc),
                                cbor_int(3, *def_enc),
                                cbor_str_sz("xyz", str_enc.clone()),
                            cbor_tag_sz(13, *def_enc),
                                cbor_int(13, *def_enc),
                        vec![BREAK],
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_g1 = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_g1).unwrap();
                    assert_eq!(irregular_bytes_g1, irregular_g1.to_cbor_bytes());
                    assert!(matches!(irregular_g1, EnumOptEmbedFields::Eg { .. }));
                    // g (None)
                    let irregular_bytes_g2 = vec![
                        arr_sz(2, *def_enc),
                            cbor_int(1, *def_enc),
                            cbor_tag_sz(13, *def_enc),
                                cbor_int(13, *def_enc),
                    ].into_iter().flatten().clone().collect::<Vec<u8>>();
                    let irregular_g2 = EnumOptEmbedFields::from_cbor_bytes(&irregular_bytes_g2).unwrap();
                    assert_eq!(irregular_bytes_g2, irregular_g2.to_cbor_bytes());
                    assert!(matches!(irregular_g2, EnumOptEmbedFields::Eg { .. }));
                }
            }
        }
    }

    #[test]
    fn plain_arrays() {
        let plain = Plain::new(10, String::from("wiorurri34h").into());
        let plain_arrays = PlainArrays::new(
            plain.clone(),
            plain.clone(),
            vec![plain.clone(), plain.clone()]
        );
        deser_test(&plain_arrays);
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_11_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Indefinite(vec![(5, Sz::Two), (6, Sz::One)]),
            StringLenSz::Indefinite(vec![(2, Sz::Inline), (0, Sz::Inline), (9, Sz::Four)]),
        ];
        for str_enc in &str_11_encodings {
            for def_enc in &def_encodings {
                // need to make sure they are actually inlined!
                let irregular_bytes = vec![
                    arr_sz(4, *def_enc),
                        // embedded
                        cbor_tag_sz(13, *def_enc),
                            cbor_int(10, *def_enc),
                        cbor_tag_sz(9, *def_enc),
                            cbor_str_sz("wiorurri34h", str_enc.clone()),
                        // single
                        arr_def(2),
                            cbor_tag(13),
                                cbor_int(10, *def_enc),
                            cbor_tag_sz(9, *def_enc),
                            cbor_str_sz("wiorurri34h", str_enc.clone()),
                        // multiple
                        arr_def(4),
                            cbor_tag_sz(13, *def_enc),
                                cbor_int(10, *def_enc),
                            cbor_tag_sz(9, *def_enc),
                                cbor_str_sz("wiorurri34h", str_enc.clone()),
                            cbor_tag_sz(13, *def_enc),
                                cbor_int(10, *def_enc),
                            cbor_tag_sz(9, *def_enc),
                                cbor_str_sz("wiorurri34h", str_enc.clone()),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let from_bytes = PlainArrays::from_cbor_bytes(&irregular_bytes).unwrap();
                assert_eq!(from_bytes.to_cbor_bytes(), irregular_bytes);
            }
        }
    }

    #[test]
    fn custom_serialization() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_8_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Indefinite(vec![(3, Sz::Two), (5, Sz::One)]),
            StringLenSz::Indefinite(vec![(0, Sz::Four), (4, Sz::Inline), (0, Sz::Inline), (4, Sz::Inline), (0, Sz::One)]),
        ];
        for def_enc in &def_encodings {
            let bytes_special_enc = StringLenSz::Indefinite(vec![(1, *def_enc); 4]);
            for str_enc in &str_8_encodings {
                let irregular_bytes = vec![
                    arr_sz(5, *def_enc),
                        cbor_bytes_sz(vec![0xCA, 0xFE, 0xF0, 0x0D], bytes_special_enc.clone()),
                        cbor_bytes_sz(vec![0x03, 0x01, 0x04, 0x01], bytes_special_enc.clone()),
                        cbor_str_sz("baadd00d", str_enc.clone()),
                        cbor_tag(9),
                            cbor_bytes_sz(vec![0xDE, 0xAD, 0xBE, 0xEF], bytes_special_enc.clone()),
                        cbor_tag(9),
                            cbor_str_sz("10241024", StringLenSz::Len(*def_enc))
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let from_bytes = StructWithCustomSerialization::from_cbor_bytes(&irregular_bytes).unwrap();
                assert_eq!(from_bytes.to_cbor_bytes(), irregular_bytes);
            }
        }
    }

    #[test]
    fn custom_serialization_rejects_wrong_tag() {
        // the custom read hook owns the WHOLE tagged2 field (nothing upstream checks the tag),
        // and the write hook re-encodes tag 9 unconditionally — so accepting any other tag would
        // break the preserve-fidelity invariant the fuzz harness asserts over this exact type
        // (accepted input must re-encode byte-identically: tag-10 in, tag-9 out).
        let bad_tag_bytes = vec![
            arr_def(5),
                cbor_bytes_sz(vec![0xCA, 0xFE, 0xF0, 0x0D], StringLenSz::Indefinite(vec![(1, Sz::Inline); 4])),
                cbor_bytes_sz(vec![0x03, 0x01, 0x04, 0x01], StringLenSz::Indefinite(vec![(1, Sz::Inline); 4])),
                cbor_str_sz("baadd00d", StringLenSz::Len(Sz::Inline)),
                cbor_tag(9),
                    cbor_bytes_sz(vec![0xDE, 0xAD, 0xBE, 0xEF], StringLenSz::Indefinite(vec![(1, Sz::Inline); 4])),
                cbor_tag(10), // tagged2 is #6.9(uint) — tag 10 must be REJECTED
                    cbor_str_sz("10241024", StringLenSz::Len(Sz::Inline))
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let err = StructWithCustomSerialization::from_cbor_bytes(&bad_tag_bytes).unwrap_err();
        assert!(
            format!("{:?}", err).contains("TagMismatch"),
            "wrong tag must fail as TagMismatch, got {:?}",
            err
        );
    }

    #[test]
    fn custom_serialization_fresh_value_hex_boundary_len() {
        // a FRESH value (StringEncoding::Canonical) whose `overridden` field is 12..=23 bytes:
        // the hex text is 2n chars, so sizing the text header from the BYTE length picks a
        // width that cannot hold 2n (canonical(16) = Inline, but the text is 32 chars) and
        // to_cbor_bytes panics with InvalidLenPassed. The hook must size for the text length.
        let v = StructWithCustomSerialization::new(
            vec![0xCA, 0xFE, 0xF0, 0x0D],
            vec![0x03, 0x01, 0x04, 0x01],
            vec![0xAB; 16],
            vec![0xDE, 0xAD, 0xBE, 0xEF],
            1024,
        );
        let bytes = v.to_cbor_bytes();
        let back = StructWithCustomSerialization::from_cbor_bytes(&bytes).unwrap();
        assert_eq!(back.to_cbor_bytes(), bytes);
        assert_eq!(back.overridden, vec![0xAB; 16]);
    }

    // The named record's declared form is `[uint]`, but its pair owns a text item. It carries no
    // external encoding tuple: record sidecars stay self-carrying while both direct and holder APIs
    // delegate to the same pair. The generated array wire must remain unacceptable to the reader.
    #[test]
    fn custom_record_rule_delegates_direct_and_embedded() {
        let record = CustomRecord::new(42);
        let direct = cbor_string("42");
        assert_eq!(record.to_cbor_bytes(), direct);
        assert_eq!(CustomRecord::from_cbor_bytes(&direct).unwrap().value, 42);
        // the custom reader must reject the record's generated array wire
        assert_decode_reject_reason::<CustomRecord>(
            &[arr_def(1), cbor_int(42, Sz::One)].concat(),
            "expected `Text' byte received `Array'",
        );

        let holder = CustomRecordHolder::new(record);
        let embedded = [arr_def(1), cbor_string("42")].concat();
        assert_eq!(holder.to_cbor_bytes(), embedded);
        assert_eq!(
            CustomRecordHolder::from_cbor_bytes(&embedded)
                .unwrap()
                .nested
                .value,
            42
        );
    }

    // The MAP-rep twin of `custom_serialization`. A map-rep field's serialize is built from ONE
    // config that also serves the member-key write, and that config used to be built WITHOUT the
    // field's @custom_serialize — so the custom WRITER was dropped while @custom_deserialize kept
    // being honored on the read side. Under --preserve-encodings the drop is doubly visible: the
    // value's encoding sidecar is handed to the custom writer as an argument, so a default writer
    // loses both the custom wire shape AND the recorded widths. Every field carries a non-minimal
    // encoding (including its own member-KEY header, which the same config derives) so the re-encode
    // equality below is a real fidelity assertion rather than a shape check.
    #[test]
    fn map_custom_serialization() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_8_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Indefinite(vec![(3, Sz::Two), (5, Sz::One)]),
            StringLenSz::Indefinite(vec![(0, Sz::Four), (4, Sz::Inline), (0, Sz::Inline), (4, Sz::Inline), (0, Sz::One)]),
        ];
        for def_enc in &def_encodings {
            let bytes_special_enc = StringLenSz::Indefinite(vec![(1, *def_enc); 4]);
            let key_enc = StringLenSz::Len(*def_enc);
            for str_enc in &str_8_encodings {
                // wire order here is DECLARATION order; orig_deser_order replays whatever it saw
                let irregular_bytes = vec![
                    map_sz(5, *def_enc),
                        cbor_str_sz("chunked", key_enc.clone()),
                            cbor_bytes_sz(vec![0xCA, 0xFE, 0xF0, 0x0D], bytes_special_enc.clone()),
                        cbor_str_sz("hexed", key_enc.clone()),
                            cbor_str_sz("baadd00d", str_enc.clone()),
                        cbor_str_sz("aliased", key_enc.clone()),
                            cbor_bytes_sz(vec![0x03, 0x01, 0x04, 0x01], bytes_special_enc.clone()),
                        cbor_str_sz("tagged", key_enc.clone()),
                            cbor_tag(9),
                            cbor_str_sz("10241024", StringLenSz::Len(*def_enc)),
                        cbor_str_sz("plain", key_enc.clone()),
                            cbor_int(7, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let from_bytes = MapStructWithCustomSerialization::from_cbor_bytes(&irregular_bytes).unwrap();
                assert_eq!(from_bytes.to_cbor_bytes(), irregular_bytes);
            }
        }
        // a FRESH value (no encoding sidecar) must still go through the custom writers: `chunked` comes
        // out indefinite-chunked and `hexed` as hex TEXT, which is exactly what the custom readers
        // demand back — under the default writers this round-trip errors rather than merely
        // differing.
        let fresh = MapStructWithCustomSerialization::new(
            vec![0xCA, 0xFE, 0xF0, 0x0D],
            vec![0xBA, 0xAD, 0xD0, 0x0D],
            vec![0x03, 0x01, 0x04, 0x01],
            1024,
            7,
        );
        let fresh_bytes = fresh.to_cbor_bytes();
        let back = MapStructWithCustomSerialization::from_cbor_bytes(&fresh_bytes).unwrap();
        assert_eq!(back.to_cbor_bytes(), fresh_bytes);
        assert_eq!(back.hexed, vec![0xBA, 0xAD, 0xD0, 0x0D]);
    }

    // Fidelity is a contract on the custom READERS too: accept only wire the paired writer can
    // re-emit from the decoded value plus the returned encoding variables. `hex::decode` tolerates
    // uppercase digits and `u64::from_str` a leading `+`/zeros, but the writers emit lowercase hex
    // and canonical decimal, and no encoding variable carries case or digit spelling — so each such
    // input used to be ACCEPTED and re-encoded to different bytes, exactly the violation the byte
    // fuzzer's preserve-fidelity oracle asserts against (found by `fuzz_bounded_run`; the first
    // vector below is the hand-derived libFuzzer artifact). The readers now refuse; the
    // lowercase/canonical-decimal controls in the two tests above stay green.
    #[test]
    fn custom_codec_readers_reject_wire_they_cannot_reemit() {
        // the libFuzzer artifact: [keyed: {} (indefinite), valued: {0 => "ffffCf"} (indefinite)]
        // — mixed-case hex in the table VALUE position, re-encoded as "ffffcf" before the fix
        let crash = vec![
            0x9f, // array(indefinite)
            0xbf, 0xff, // keyed: {} (indefinite)
            0xbf, // valued: map(indefinite)
            0x00, // 0 =>
            0x66, 0x66, 0x66, 0x66, 0x66, 0x43, 0x66, // text(6) "ffffCf"
            0xff, // break (valued)
            0xff, // break (array)
        ];
        assert_decode_reject_reason::<CustomTablePositions>(&crash, "hex codec: non-lowercase hex text cannot re-encode to itself");
        // the same hole from the table KEY position (read_hex_table_string's other reach)
        let upper_key = vec![
            arr_def(2),
                map_def(1),
                    cbor_string("CAFE"),
                        cbor_int(1, Sz::Inline),
                map_def(0),
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<u8>>();
        assert_decode_reject_reason::<CustomTablePositions>(&upper_key, "hex codec: non-lowercase hex text cannot re-encode to itself");
        // and from the struct-field positions (read_hex_string, read_tagged_uint_str)
        let struct_bytes = |hexed: &str, tagged: &str| {
            vec![
                map_sz(5, Sz::Inline),
                    cbor_string("chunked"),
                        cbor_bytes_sz(vec![0xCA], StringLenSz::Indefinite(vec![(1, Sz::Inline)])),
                    cbor_string("hexed"),
                        cbor_string(hexed),
                    cbor_string("aliased"),
                        cbor_bytes_sz(vec![0x03], StringLenSz::Indefinite(vec![(1, Sz::Inline)])),
                    cbor_string("tagged"),
                        cbor_tag(9),
                        cbor_string(tagged),
                    cbor_string("plain"),
                        cbor_int(7, Sz::Inline),
            ]
            .into_iter()
            .flatten()
            .collect::<Vec<u8>>()
        };
        // control first: lowercase hex + canonical decimal is accepted and re-encodes to itself
        let ok = struct_bytes("baad", "1024");
        assert_eq!(
            MapStructWithCustomSerialization::from_cbor_bytes(&ok)
                .unwrap()
                .to_cbor_bytes(),
            ok
        );
        // uppercase hex at the struct-field slot
        assert_decode_reject_reason::<MapStructWithCustomSerialization>(&struct_bytes("BAAD", "1024"), "hex codec: non-lowercase hex text cannot re-encode to itself");
        // decimal spellings `u64::from_str` accepts but `to_string` never re-emits
        assert_decode_reject_reason::<MapStructWithCustomSerialization>(&struct_bytes("baad", "+1024"), "uint-as-text codec: non-canonical decimal text cannot re-encode to itself");
        assert_decode_reject_reason::<MapStructWithCustomSerialization>(&struct_bytes("baad", "01024"), "uint-as-text codec: non-canonical decimal text cannot re-encode to itself");
        // The STRING-ENCODING dimension of the same contract, on the pair with NO encoding slot at
        // all (read_custom_record replaces the record's array with a text item, so the record's
        // self-carrying sidecars describe nothing about it): the second libFuzzer artifact —
        // indefinite text `7f 61 31 ff` ("1" in one chunk) — was accepted and re-encoded as the
        // definite `61 31`. Only the writer's exact re-emission may be accepted: definite text,
        // canonical length width, canonical decimal content.
        assert_decode_reject_reason::<CustomRecord>(&[0x7f, 0x61, 0x31, 0xff], "record-as-text codec: indefinite text cannot re-encode to itself"); // the artifact
        assert_decode_reject_reason::<CustomRecord>(&[0x78, 0x01, 0x31], "record-as-text codec: non-canonical text length width cannot re-encode to itself"); // non-canonical width
        assert_decode_reject_reason::<CustomRecord>(&[0x62, 0x2b, 0x31], "record-as-text codec: non-canonical decimal text cannot re-encode to itself"); // "+1"
        assert_decode_reject_reason::<CustomRecord>(&[0x62, 0x30, 0x31], "record-as-text codec: non-canonical decimal text cannot re-encode to itself"); // "01"
        // control: the writer's own spelling round-trips byte-identically
        let rec = CustomRecord::from_cbor_bytes(&[0x61, 0x31]).unwrap();
        assert_eq!(rec.value, 1);
        assert_eq!(rec.to_cbor_bytes(), vec![0x61, 0x31]);
    }

    // A table's KEY domain and VALUE range are the two positions a type-level custom pair reaches
    // that no struct field does: both go through the table loop rather than a record field's
    // config. `hex_table_str` writes bytes as hex TEXT, which the default `bytes` writer never
    // produces and `read_hex_table_string` refuses, so a custom fn dropped in either position is a
    // round-trip FAILURE below rather than a cosmetic difference. Every head width here is
    // non-minimal in at least one sweep cell, so the equality is a fidelity assertion: the widths
    // ride the per-entry sidecars THROUGH the custom fns and back.
    #[test]
    fn custom_table_positions() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        let str_encodings = vec![
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Len(Sz::One),
            StringLenSz::Indefinite(vec![(2, Sz::Two), (2, Sz::One)]),
        ];
        for def_enc in &def_encodings {
            for str_enc in &str_encodings {
                let irregular_bytes = vec![
                    arr_sz(2, *def_enc),
                        // keyed: { * hex_table_str => uint } — the KEY rides the custom pair
                        map_sz(2, *def_enc),
                            cbor_str_sz("cafe", str_enc.clone()), // h'CAFE' written as hex text
                                cbor_int(1, *def_enc),
                            cbor_str_sz("f00d", str_enc.clone()), // h'F00D'
                                cbor_int(2, *def_enc),
                        // valued: { * uint => hex_table_str } — the VALUE rides the custom pair
                        map_sz(2, *def_enc),
                            cbor_int(7, *def_enc),
                                cbor_str_sz("baad", str_enc.clone()), // h'BAAD'
                            cbor_int(8, *def_enc),
                                cbor_str_sz("d00d", str_enc.clone()), // h'D00D'
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let from_bytes = CustomTablePositions::from_cbor_bytes(&irregular_bytes).unwrap();
                // the custom READER's output is what lands in the map: decoded bytes, not the text
                assert_eq!(from_bytes.keyed.get(&vec![0xCA, 0xFE]).copied(), Some(1));
                assert_eq!(from_bytes.valued.get(&7), Some(&vec![0xBA, 0xAD]));
                assert_eq!(from_bytes.to_cbor_bytes(), irregular_bytes);
            }
        }
        // a FRESH value (no sidecar at all) must still go through the custom WRITERS in both
        // positions — the wire below is hex TEXT, which is exactly what the custom readers demand
        // back, so under the default `bytes` writers this round-trip errors instead of differing.
        let mut keyed = OrderedHashMap::new();
        keyed.insert(vec![0xCA, 0xFEu8], 1u64);
        let mut valued = OrderedHashMap::new();
        valued.insert(7u64, vec![0xBA, 0xADu8]);
        let fresh = CustomTablePositions::new(keyed, valued);
        let fresh_bytes = fresh.to_cbor_bytes();
        assert_eq!(
            fresh_bytes,
            vec![
                arr_def(2),
                    map_def(1),
                        cbor_string("cafe"),
                            cbor_int(1, Sz::Inline),
                    map_def(1),
                        cbor_int(7, Sz::Inline),
                            cbor_string("baad"),
            ].into_iter().flatten().clone().collect::<Vec<u8>>()
        );
        let back = CustomTablePositions::from_cbor_bytes(&fresh_bytes).unwrap();
        assert_eq!(back.to_cbor_bytes(), fresh_bytes);
        assert_eq!(back.keyed.get(&vec![0xCA, 0xFE]).copied(), Some(1));
        assert_eq!(back.valued.get(&7), Some(&vec![0xBA, 0xAD]));
    }

    // The preserve sidecar SHAPE for both table positions. `*_key_encodings` and `*_value_encodings`
    // are alike keyed by the DECODED KEY, which is what makes a custom KEY codec's two legs meet:
    // the encoding the custom reader hands back is filed under the VALUE it decoded to, never under
    // the wire text it consumed. Non-minimal widths on both custom-written strings keep the recorded
    // encodings distinguishable from the `Canonical` default a missing sidecar entry would yield.
    #[test]
    fn custom_table_positions_sidecar_shape() {
        use serialization::StringEncoding;
        let wire = vec![
            arr_sz(2, Sz::Inline),
                map_sz(1, Sz::Inline),
                    cbor_str_sz("cafe", StringLenSz::Len(Sz::One)), // 4-char text, 1-byte len head
                        cbor_int(1, Sz::Inline),
                map_sz(1, Sz::Inline),
                    cbor_int(7, Sz::Inline),
                        cbor_str_sz("baad", StringLenSz::Indefinite(vec![(2, Sz::Two), (2, Sz::One)])),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let v = CustomTablePositions::from_cbor_bytes(&wire).unwrap();
        let encs = v.encodings.as_ref().unwrap();
        // the `get` argument pins the sidecar's KEY type (the decoded `hex_table_str`, i.e. bytes)
        // and the annotation pins its VALUE type
        let key_enc: Option<&StringEncoding> = encs.keyed_key_encodings.get(&vec![0xCA, 0xFE]);
        assert_eq!(key_enc, Some(&StringEncoding::Definite(Sz::One)));
        // the VALUE sidecar is keyed by the entry's uint KEY, not by the value's own bytes
        let value_enc: Option<&StringEncoding> = encs.valued_value_encodings.get(&7u64);
        assert_eq!(
            value_enc,
            Some(&StringEncoding::Indefinite(vec![(2, Sz::Two), (2, Sz::One)]))
        );
        assert_eq!(v.to_cbor_bytes(), wire);
    }

    // The reader agrees with the writer in both positions: the bytes the DEFAULT `bytes` writer
    // would emit (a CBOR byte string) are REJECTED, so a custom writer dropped from either the key
    // domain or the value range cannot go unnoticed as a merely-cosmetic difference. The control
    // isolates the position as the variable — the same shapes with hex TEXT parse.
    #[test]
    fn custom_table_positions_reject_default_shape() {
        let custom_shaped = vec![
            arr_def(2),
                map_def(1),
                    cbor_string("cafe"),
                        cbor_int(1, Sz::Inline),
                map_def(1),
                    cbor_int(7, Sz::Inline),
                        cbor_string("baad"),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        assert!(
            CustomTablePositions::from_cbor_bytes(&custom_shaped).is_ok(),
            "control: the custom writers' own hex-TEXT shape parses"
        );
        let default_shaped_key = vec![
            arr_def(2),
                map_def(1),
                    cbor_bytes_sz(vec![0xCA, 0xFE], StringLenSz::Len(Sz::Inline)),
                        cbor_int(1, Sz::Inline),
                map_def(1),
                    cbor_int(7, Sz::Inline),
                        cbor_string("baad"),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        // the custom reader must reject the default writer's shape in the table KEY domain
        assert_decode_reject_reason::<CustomTablePositions>(&default_shaped_key, "expected `Text' byte received `Bytes'");
        let default_shaped_value = vec![
            arr_def(2),
                map_def(1),
                    cbor_string("cafe"),
                        cbor_int(1, Sz::Inline),
                map_def(1),
                    cbor_int(7, Sz::Inline),
                        cbor_bytes_sz(vec![0xBA, 0xAD], StringLenSz::Len(Sz::Inline)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        // the custom reader must reject the default writer's shape in the table VALUE range
        assert_decode_reject_reason::<CustomTablePositions>(&default_shaped_value, "expected `Text' byte received `Bytes'");
    }

    #[test]
    fn wrapper_table() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        for def_enc in &def_encodings {
            let irregular_bytes = vec![
                map_sz(3, *def_enc),
                    cbor_int(5, *def_enc),
                        cbor_int(4, *def_enc),
                    cbor_int(3, *def_enc),
                        cbor_int(2, *def_enc),
                    cbor_int(1, *def_enc),
                        cbor_int(0, *def_enc),
            ].into_iter().flatten().clone().collect::<Vec<u8>>();
            let from_bytes = WrapperTable::from_cbor_bytes(&irregular_bytes).unwrap();
            assert_eq!(from_bytes.to_cbor_bytes(), irregular_bytes);
        }
    }

    #[test]
    fn wrapper_list() {
        let def_encodings = vec![Sz::Inline, Sz::One, Sz::Two, Sz::Four, Sz::Eight];
        for def_enc in &def_encodings {
            let irregular_bytes = vec![
                arr_sz(5, *def_enc),
                    cbor_int(5, *def_enc),
                    cbor_int(4, *def_enc),
                    cbor_int(3, *def_enc),
                    cbor_int(2, *def_enc),
                    cbor_int(1, *def_enc),
            ].into_iter().flatten().clone().collect::<Vec<u8>>();
            let from_bytes = WrapperList::from_cbor_bytes(&irregular_bytes).unwrap();
            assert_eq!(from_bytes.to_cbor_bytes(), irregular_bytes);
        }
    }

    #[test]
    fn width_collapse_rejects() {
        // Exact-width collapses (uint .size 2 -> u16, int .size 1 -> i8, int .size 8 -> i64)
        // carry no residual bounds; preserve-mode reads come back WIDER (u64 from
        // unsigned_integer_sz, i128 from negative_integer_sz), so each narrowing cast must be
        // width-guarded — a bare `x as u16`/`as i8`/`as i64` silently truncated (65536 decoded
        // "successfully" as 0), invisible to round-trips by construction.
        let make = |u16v: i128, i8v: i128, i64v: i128| {
            let cbor = [
                arr_def(3),
                cbor_int(u16v, Sz::Eight),
                cbor_int(i8v, Sz::Eight),
                cbor_int(i64v, Sz::Eight),
            ]
            .concat();
            WidthCollapse::from_cbor_bytes(&cbor)
        };
        // Boundary (exactly-representable) values decode.
        make(65535, 0, 0).unwrap();
        make(0, 127, 0).unwrap();
        make(0, -128, 0).unwrap();
        make(0, 0, i64::MAX as i128).unwrap();
        make(0, 0, i64::MIN as i128).unwrap();
        // One-past-width values must REJECT (pre-fix: silently truncate-decoded).
        assert!(make(65536, 0, 0).is_err());
        assert!(make(0, 128, 0).is_err());
        assert!(make(0, -129, 0).is_err());
        assert!(make(0, 0, (i64::MAX as i128) + 1).is_err());
        assert!(make(0, 0, (i64::MIN as i128) - 1).is_err());
        // The guarded path still round-trips (including encoding preservation via deser_test).
        deser_test(&WidthCollapse::new(65535, -128, i64::MIN));
    }

    // A tag mismatch on a tagged record must name the type EXACTLY ONCE. The record's header
    // parsing (including the tag check) sits inside the `.annotate("Foo")` closure; if the tag
    // check built its error via the location-carrying form (`DeserializeError::new("Foo", ..)`),
    // the closure's map_err would PREPEND "Foo" again (static/error.rs annotate concatenates
    // "location.loc"), reading "Foo.Foo". Inside an annotated closure the emitted tag-check error
    // must therefore be the locationless form, while the named form is kept when no closure exists
    // (annotate_fields=false) so the name is never lost.
    //
    // foo = #6.11([uint, text, bytes]); 0xcc is tag 12 (major 6, value 12) — wrong tag, so
    // `raw.tag_sz()?` succeeds and the `tag != 11` check raises TagMismatch.
    #[test]
    fn error_annotation_tag_mismatch_single_name() {
        let err = Foo::from_cbor_bytes(&[0xccu8]).unwrap_err().to_string();
        assert!(
            err.contains("Foo"),
            "tag-mismatch error must name the type, got: {err}"
        );
        assert!(
            !err.contains("Foo.Foo"),
            "tag-mismatch error must not double-annotate, got: {err}"
        );
    }

    // Preserve-profile sibling of tests/core's newtype-wrapper / plain-group annotation pins: the
    // preserve profile carries encoding fields (`enc_fields`), exercising the `Ok(Self { inner,
    // encodings: Some(..) })` branch of the wrapped body that the default profile can't. Both header
    // errors must name their type EXACTLY ONCE.
    //
    // wrapper_list = [* uint] (@newtype) reads an array; plain = (d: #6.13(uint), e: tagged_text)
    // decodes standalone as a 2-element array. A bare uint (0x00) trips each container read inside
    // its `.annotate(<T>)` closure.
    #[test]
    fn error_annotation_wrapper_and_plain_group_single_name() {
        let wrapper_err = WrapperList::from_cbor_bytes(&[0x00u8])
            .unwrap_err()
            .to_string();
        assert!(
            wrapper_err.contains("WrapperList"),
            "wrapper wrong-container error must name the wrapper, got: {wrapper_err}"
        );
        assert!(
            !wrapper_err.contains("WrapperList.WrapperList"),
            "wrapper wrong-container error must not double-annotate, got: {wrapper_err}"
        );

        let plain_err = Plain::from_cbor_bytes(&[0x00u8]).unwrap_err().to_string();
        assert!(
            plain_err.contains("Plain"),
            "plain-group header error must name the group, got: {plain_err}"
        );
        assert!(
            !plain_err.contains("Plain.Plain"),
            "plain-group header error must not double-annotate, got: {plain_err}"
        );
    }

    // WI-1: a `[+ uint]` field under --preserve-encodings must keep its length-encoding metadata in
    // the parent's encoding struct (keyed off the FIELD), so an indefinite-length encoding of the
    // NonEmptyVec array round-trips byte-identically — the container swap must not swallow it.
    #[test]
    fn non_empty_vec_preserves_encoding() {
        let pts = NonEmptyVec::try_from(vec![1u64, 2, 3]).unwrap();
        let mut nev = NevPreserve::new(pts);
        deser_test(&nev);
        let definite = nev.to_cbor_bytes();
        let mut enc = NevPreserveEncoding::default();
        enc.pts_encoding = LenEncoding::Indefinite;
        nev.encodings = Some(enc);
        deser_test(&nev);
        let indefinite = nev.to_cbor_bytes();
        assert!(
            definite != indefinite,
            "indefinite [+ uint] encoding must differ from the definite one"
        );
        assert_eq!(
            *indefinite.last().unwrap(),
            BREAK,
            "indefinite inner array must terminate with BREAK"
        );
    }

    // WI-2: a `{+ k => v}` field under --preserve-encodings must keep its length/key-order encoding
    // metadata in the parent's encoding struct (keyed off the FIELD `m`), so an indefinite-length
    // encoding of the NonEmptyMap round-trips byte-identically — the container swap must not swallow
    // it. (The `NemPreserveEncoding` struct carries `m_encoding`/`m_key_encodings`/`m_value_encodings`
    // exactly as a bare table field would.)
    #[test]
    fn non_empty_map_preserves_encoding() {
        let m = NonEmptyMap::try_from(OrderedHashMap::from_iter([
            (1u64, "a".to_string()),
            (2u64, "b".to_string()),
        ]))
        .unwrap();
        let mut nem = NemPreserve::new(m);
        deser_test(&nem);
        let definite = nem.to_cbor_bytes();
        let mut enc = NemPreserveEncoding::default();
        enc.m_encoding = LenEncoding::Indefinite;
        nem.encodings = Some(enc);
        deser_test(&nem);
        let indefinite = nem.to_cbor_bytes();
        assert!(
            definite != indefinite,
            "indefinite {{+ k => v}} encoding must differ from the definite one"
        );
        assert_eq!(
            *indefinite.last().unwrap(),
            BREAK,
            "indefinite inner map must terminate with BREAK"
        );
    }

    #[test]
    fn nullable_specials() {
        // Preserve-mode flavor of the `T / null` null-peek flip vectors (see the core suite's
        // twin): the peek must rewind by the ACTUAL width `special()` consumed, not a hardcoded 1
        // byte. Both major-type-7 flavors are in the fixture — the encoding-less `bool` and the
        // width-carrying `float64` — because the rewind width and the encoding var are independent
        // failure modes: a float slot must ALSO come back through the Optional wrapper with its
        // head width intact.
        //
        // The float slot carries 1.0, whose three wire widths (`f9 3c00` / `fa 3f800000` /
        // `fb 3ff0000000000000`, RFC 8949 Appendix A) are the same value — so a re-encode that
        // reproduces the input width can only be reading it off the encoding var, not deriving it.
        for float_wire in [
            &[0xf9u8, 0x3c, 0x00][..],
            &[0xfa, 0x3f, 0x80, 0x00, 0x00],
            &[0xfb, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        ] {
            let some: Vec<u8> = [arr_def(2), vec![0xf5], float_wire.to_vec()].concat();
            let d = NullableSpecials::from_cbor_bytes(&some).unwrap();
            assert_eq!(d.b, Some(true));
            assert_eq!(d.f, Some(1.0));
            assert_eq!(d.to_cbor_bytes(), some);
        }
        // both slots null: the float's encoding var has nothing to record, and re-serialize must
        // not invent a width (i.e. must not emit a float at all).
        let none: Vec<u8> = [arr_def(2), vec![0xf6, 0xf6]].concat();
        let d = NullableSpecials::from_cbor_bytes(&none).unwrap();
        assert!(d.b.is_none());
        assert!(d.f.is_none());
        assert_eq!(d.to_cbor_bytes(), none);
        // malformed two-byte simple in the nullable-bool slot (`f8 f5`): the 2.4.0 peek consumed
        // 2 bytes, rewound 1, and re-read the PAYLOAD byte f5 as `true` — accepting malformed
        // input. This and the RFC 8949 §3.3 non-well-formed simples all reject; an f9 float in
        // the bool slot rejects as a type mismatch (not a mis-decode). Each vector keeps a VALID
        // float in the second slot so the rejection is attributable to the first one rather than
        // to the array arity.
        for (bad_bool, expect) in [
            (&[0xf8u8, 0xf5][..], "Expected Special::Bool, received Unassigned(245)"),
            (&[0xfc][..], "non-well-formed encoding of simple value 28"),
            (&[0xfd][..], "non-well-formed encoding of simple value 29"),
            (&[0xfe][..], "non-well-formed encoding of simple value 30"),
            (&[0xf8, 0x1f][..], "non-well-formed encoding of simple value 31"),
            (&[0xff][..], "Expected Special::Bool, received Break"),
            (&[0xf9, 0x42, 0x00][..], "Expected Special::Bool, received Float(3.0)"),
        ] {
            let bad: Vec<u8> =
                [arr_def(2), bad_bool.to_vec(), vec![0xf9, 0x3c, 0x00]].concat();
            assert_decode_reject_reason::<NullableSpecials>(&bad, expect);
        }
        // ...and the mirror, in the FLOAT slot. Two mechanisms, one outcome, kept apart by the
        // pinned reasons: a well-formed non-float Special (`true`, a Break) is a TYPE mismatch,
        // while the non-well-formed simples — the reserved `0xfc-0xfe` codepoints AND the two-byte
        // form below 32 (`f8 18`) — are rejected at READ, before anything types them.
        for (bad_float, expect) in [
            (&[0xf5u8][..], "Expected Special::Float, received Bool(true)"),
            (&[0xf8, 0x18][..], "non-well-formed encoding of simple value 24"),
            (&[0xfc][..], "non-well-formed encoding of simple value 28"),
            (&[0xff][..], "Expected Special::Float, received Break"),
        ] {
            let bad: Vec<u8> = [arr_def(2), vec![0xf5], bad_float.to_vec()].concat();
            assert_decode_reject_reason::<NullableSpecials>(&bad, expect);
        }
    }

    // The recorded head width describes the value that was READ, so it stops applying once there is
    // no such value — and a float takes the SAME two fallbacks an integer argument width takes, which
    // is the part that reads like "preserve does not preserve" if you meet it without the contract
    // above it. Pinned here because the float fallback is load-bearing in a way the integer one is
    // not: `write_float_sz` ERRORS (`InvalidLenPassed`) on a width that cannot represent the value
    // exactly, so without the widening this would be a runtime serialize failure, not just a
    // non-minimal head.
    #[test]
    fn float_width_falls_back_like_an_integer_width() {
        // [b: bool / null, f: float64 / null] — `f` is a mutable Option<f64>.
        let read: Vec<u8> = [arr_def(2), vec![0xf5], vec![0xf9, 0x3e, 0x00]].concat();
        let mut d = NullableSpecials::from_cbor_bytes(&read).unwrap();
        assert_eq!(d.f, Some(1.5));
        // baseline: the read value keeps its half-width head (the contract this test's siblings pin)
        assert_eq!(d.to_cbor_bytes(), read);
        // REPLACED value that the recorded f16 width cannot represent -> the head widens to the
        // narrowest width that CAN, rather than erroring out of serialize
        d.f = Some(1.1);
        assert_eq!(
            d.to_cbor_bytes(),
            [
                arr_def(2),
                vec![0xf5],
                vec![0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a],
            ]
            .concat(),
            "a value the recorded width cannot hold must widen, not fail"
        );
        // ...and a value that still FITS the recorded width keeps it (the fallback is not a reset:
        // 1.0 would encode minimally as f16 anyway, so use a value whose own minimal width is
        // NARROWER than the one recorded, and assert the recorded one survives)
        let read_wide: Vec<u8> = [
            arr_def(2),
            vec![0xf5],
            vec![0xfb, 0x3f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        ]
        .concat();
        let mut d = NullableSpecials::from_cbor_bytes(&read_wide).unwrap();
        d.f = Some(1.0); // minimal width f16, but the recorded width is f64 and still fits
        assert_eq!(
            d.to_cbor_bytes(),
            [
                arr_def(2),
                vec![0xf5],
                vec![0xfb, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
            ]
            .concat(),
            "a recorded width that still represents the new value must be kept, not minimized"
        );
        // a FRESH value carries no recorded width and writes the shortest that represents it
        let fresh = NullableSpecials::new(Some(true), Some(1.5));
        assert_eq!(
            fresh.to_cbor_bytes(),
            [arr_def(2), vec![0xf5], vec![0xf9, 0x3e, 0x00]].concat()
        );
    }

    // An optional fixed FLOAT member is the third encoding shape among the optional fixed kinds:
    // encoding-FUL like uint/text (its head width is metadata) but Special-classed like bool/null
    // (so the presence probe peeks a major-type-7 head). This pins that the two mechanisms compose —
    // presence bit AND width — which neither `opt_fixed_member_preserve` (encoding-ful, not Special)
    // nor the bool/null arms there (Special, not encoding-ful) can reach.
    #[test]
    fn opt_fixed_float_member_preserve() {
        // absent -> presence false, no float element on the wire, nothing recorded
        let absent: Vec<u8> =
            [arr_def(2), cbor_int(9, Sz::Inline), cbor_string("hi")].concat();
        let d = OptFixedArrFloat::from_cbor_bytes(&absent).unwrap();
        assert!(!d.ffix);
        assert_eq!(d.to_cbor_bytes(), absent);
        // present at each of 2.5's two wire widths (`f9 4100` / `fb 4004000000000000`): the VALUE is
        // fixed by the spec, so only a recorded width can reproduce the input bytes.
        for float_wire in [
            &[0xf9u8, 0x41, 0x00][..],
            &[0xfb, 0x40, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        ] {
            let present: Vec<u8> = [
                arr_def(3),
                cbor_int(9, Sz::Inline),
                float_wire.to_vec(),
                cbor_string("hi"),
            ]
            .concat();
            let d = OptFixedArrFloat::from_cbor_bytes(&present).unwrap();
            assert!(d.ffix);
            assert_eq!(d.to_cbor_bytes(), present);
            deser_test(&d);
        }
        // a DIFFERENT float in the slot is a FixedValueMismatch, not a silently-accepted value —
        // without this the width vectors above would pass against a decoder that never checks it
        let wrong: Vec<u8> = [
            arr_def(3),
            cbor_int(9, Sz::Inline),
            vec![0xf9, 0x3c, 0x00],
            cbor_string("hi"),
        ]
        .concat();
        assert_decode_reject_reason::<OptFixedArrFloat>(&wrong, "Expected fixed value 2.5 found 1");
    }

    // --- OrderedHashMap mutation semantics, pinned at the WIRE level ---
    //
    // A table's iteration order IS its serialized key order (the emitted map loop walks
    // `inner.iter()` verbatim), so any change in the backing insertion-ordered map's mutation
    // semantics silently rewrites the bytes of a re-serialized value. Under --preserve-encodings
    // that is a fidelity break, not a cosmetic one: a value decoded from the wire and written back
    // after an in-place edit must keep every key where the wire put it.
    //
    // These tests pin the semantics the wrapper is contractually required to have,
    // independently of WHICH insertion-ordered map backs it. They assert only through
    // `OrderedHashMap`'s own surface and `to_cbor_bytes()` — no backing-crate path is named — so
    // they stay meaningful across a swap of that backing. Written against the current backing and
    // green on it, so a later swap that changes any of them is caught as a behavior change
    // rather than blessed as new bytes. One per method of the entry surface, since the inherent
    // `entry()` shadows `Deref`: that surface is the whole surface a consumer can reach.
    //
    // Values are all < 24 so every uint is a single Sz::Inline byte, keeping the expected byte
    // vectors readable; `WrapperTable` ( `{ * uint => uint }` @newtype ) serializes as exactly the
    // bare map, with `encodings: None` giving the minimal definite length.

    // Pin 1: overwriting an EXISTING key with `insert` moves it to the BACK of the order (and takes
    // the new value). Asserted on the bytes: K2 leaves its original slot and reappears last.
    #[test]
    fn ordered_hash_map_insert_overwrite_moves_to_back() {
        let mut table: OrderedHashMap<u64, u64> = OrderedHashMap::new();
        table.insert(1, 10);
        table.insert(2, 11);
        table.insert(3, 12);
        let before = WrapperTable::new(table.clone()).to_cbor_bytes();
        assert_eq!(
            before,
            [
                map_def(3),
                cbor_int(1, Sz::Inline),
                cbor_int(10, Sz::Inline),
                cbor_int(2, Sz::Inline),
                cbor_int(11, Sz::Inline),
                cbor_int(3, Sz::Inline),
                cbor_int(12, Sz::Inline),
            ]
            .concat()
        );
        // overwrite K2: value becomes 19 AND the key moves behind K3
        table.insert(2, 19);
        let after_value = WrapperTable::new(table.clone());
        let after = after_value.to_cbor_bytes();
        assert_eq!(
            after,
            [
                map_def(3),
                cbor_int(1, Sz::Inline),
                cbor_int(10, Sz::Inline),
                cbor_int(3, Sz::Inline),
                cbor_int(12, Sz::Inline),
                cbor_int(2, Sz::Inline),
                cbor_int(19, Sz::Inline),
            ]
            .concat()
        );
        assert!(
            before != after,
            "overwriting a key must be visible on the wire"
        );
        // the mutated bytes are themselves valid input and round-trip byte-identically
        let back = WrapperTable::from_cbor_bytes(&after).unwrap();
        assert_eq!(back.to_cbor_bytes(), after);
        deser_test(&after_value);
    }

    // Pin 2: `entry(k).or_insert(..)` must NOT move an existing key — the entry API is how a
    // consumer accumulates into a decoded table (`*t.entry(k).or_insert(0) += n`), and a
    // position-refreshing `or_insert` would reorder the re-serialized map behind the caller's back.
    // A key absent from the map is appended at the end, as a plain insert would be.
    #[test]
    fn ordered_hash_map_or_insert_keeps_position() {
        let mut table: OrderedHashMap<u64, u64> = OrderedHashMap::new();
        table.insert(1, 10);
        table.insert(2, 11);
        table.insert(3, 12);
        // existing key: value incremented in place, position untouched
        *table.entry(2).or_insert(0) += 1;
        // absent key: inserted with the default and appended last
        *table.entry(4).or_insert(14) += 0;
        let value = WrapperTable::new(table);
        assert_eq!(
            value.to_cbor_bytes(),
            [
                map_def(4),
                cbor_int(1, Sz::Inline),
                cbor_int(10, Sz::Inline),
                cbor_int(2, Sz::Inline),
                cbor_int(12, Sz::Inline),
                cbor_int(3, Sz::Inline),
                cbor_int(12, Sz::Inline),
                cbor_int(4, Sz::Inline),
                cbor_int(14, Sz::Inline),
            ]
            .concat()
        );
        deser_test(&value);
    }

    // Pin 3: `from_iter` over a sequence containing a DUPLICATE key behaves as repeated `insert`s —
    // the duplicate keeps the LAST value and sits at the BACK. Pinned both as an iteration trace
    // (what a consumer walking the table sees) and on the wire (what the map serializes to), since
    // the two must not be allowed to drift apart.
    #[test]
    fn ordered_hash_map_from_iter_duplicate_keys() {
        let table = OrderedHashMap::from_iter([(1u64, 10u64), (2, 11), (1, 19), (3, 12)]);
        let trace: Vec<(u64, u64)> = table.iter().map(|(k, v)| (*k, *v)).collect();
        assert_eq!(trace, vec![(2, 11), (1, 19), (3, 12)]);
        let value = WrapperTable::new(table);
        assert_eq!(
            value.to_cbor_bytes(),
            [
                map_def(3),
                cbor_int(2, Sz::Inline),
                cbor_int(11, Sz::Inline),
                cbor_int(1, Sz::Inline),
                cbor_int(19, Sz::Inline),
                cbor_int(3, Sz::Inline),
                cbor_int(12, Sz::Inline),
            ]
            .concat()
        );
        deser_test(&value);
    }

    // Pin 4: the entry view's MATCH shapes. `entry()` hands back a two-variant enum over an occupied
    // and a vacant half, so code that destructures an entry — rather than going through `or_insert`
    // — compiles and behaves as it reads. This is the shape hand-written consumers use to accumulate
    // into a decoded table, so it is exercised here inside a real generated crate rather than left
    // to a consumer to discover; the occupied arm's in-place `get_mut` must leave the key's wire
    // position alone for the same reason `or_insert` must.
    #[test]
    fn ordered_hash_map_entry_match_shapes() {
        use crate::generated::ordered_hash_map::Entry;
        let mut table: OrderedHashMap<u64, u64> = OrderedHashMap::new();
        table.insert(1, 10);
        table.insert(2, 11);
        table.insert(3, 12);
        match table.entry(2) {
            Entry::Occupied(mut occupied) => *occupied.get_mut() += 1,
            Entry::Vacant(_) => panic!("key 2 was inserted above — its entry must be occupied"),
        }
        match table.entry(4) {
            Entry::Occupied(_) => panic!("key 4 was never inserted — its entry must be vacant"),
            Entry::Vacant(vacant) => {
                vacant.insert(14);
            }
        }
        let value = WrapperTable::new(table);
        assert_eq!(
            value.to_cbor_bytes(),
            [
                map_def(4),
                cbor_int(1, Sz::Inline),
                cbor_int(10, Sz::Inline),
                cbor_int(2, Sz::Inline),
                cbor_int(12, Sz::Inline),
                cbor_int(3, Sz::Inline),
                cbor_int(12, Sz::Inline),
                cbor_int(4, Sz::Inline),
                cbor_int(14, Sz::Inline),
            ]
            .concat()
        );
        deser_test(&value);
    }
    // Pin 5: `entry(k).or_default()` must not move an existing key. This is the accumulate idiom a
    // consumer reaches for when `V: Default` (`*t.entry(k).or_default() += n`); the backing crate
    // has no `or_default` at all, so the wrapper's is the only one, and the obvious delegation
    // (`or_insert(V::default())` on the backing entry) would refresh the key's position. Driven from
    // DECODED bytes rather than a built table: the value under test is one that came off the wire,
    // which is exactly the case where a moved key rewrites bytes the caller never edited.
    #[test]
    fn ordered_hash_map_or_default_keeps_position() {
        let bytes = [
            map_def(3),
            cbor_int(1, Sz::Inline),
            cbor_int(10, Sz::Inline),
            cbor_int(2, Sz::Inline),
            cbor_int(11, Sz::Inline),
            cbor_int(3, Sz::Inline),
            cbor_int(12, Sz::Inline),
        ]
        .concat();
        let decoded = WrapperTable::from_cbor_bytes(&bytes).unwrap();
        assert_eq!(decoded.to_cbor_bytes(), bytes);
        let mut table = decoded.get().clone();
        // existing key: the default is not applied, and re-serializing gives back the SAME bytes
        assert_eq!(*table.entry(2).or_default(), 11);
        assert_eq!(
            WrapperTable::new(table.clone()).to_cbor_bytes(),
            bytes,
            "or_default on an occupied entry must leave the wire bytes untouched"
        );
        // absent key: `V::default()` is inserted and appended last, as a plain insert would be
        assert_eq!(*table.entry(4).or_default(), 0);
        let value = WrapperTable::new(table);
        assert_eq!(
            value.to_cbor_bytes(),
            [
                map_def(4),
                cbor_int(1, Sz::Inline),
                cbor_int(10, Sz::Inline),
                cbor_int(2, Sz::Inline),
                cbor_int(11, Sz::Inline),
                cbor_int(3, Sz::Inline),
                cbor_int(12, Sz::Inline),
                cbor_int(4, Sz::Inline),
                cbor_int(0, Sz::Inline),
            ]
            .concat()
        );
        deser_test(&value);
    }

    // Pin 6: `entry(k).and_modify(..)` mutates an occupied entry in place, leaving its position
    // alone, and passes a vacant entry through untouched so a trailing `or_insert` still appends.
    // Same decoded-value framing as pin 5.
    #[test]
    fn ordered_hash_map_and_modify_keeps_position() {
        let bytes = [
            map_def(3),
            cbor_int(1, Sz::Inline),
            cbor_int(10, Sz::Inline),
            cbor_int(2, Sz::Inline),
            cbor_int(11, Sz::Inline),
            cbor_int(3, Sz::Inline),
            cbor_int(12, Sz::Inline),
        ]
        .concat();
        let decoded = WrapperTable::from_cbor_bytes(&bytes).unwrap();
        let mut table = decoded.get().clone();
        // occupied, read-only closure: it sees the stored value, and nothing about the wire moves
        let mut saw = None;
        table.entry(2).and_modify(|v| saw = Some(*v));
        assert_eq!(saw, Some(11));
        assert_eq!(
            WrapperTable::new(table.clone()).to_cbor_bytes(),
            bytes,
            "and_modify on an occupied entry must leave the wire bytes untouched"
        );
        // occupied, mutating closure: the new value lands in the key's ORIGINAL slot
        table.entry(2).and_modify(|v| *v += 1);
        // vacant: the closure never runs and the chained `or_insert` appends at the back
        table
            .entry(4)
            .and_modify(|_| panic!("key 4 was never inserted — its entry must be vacant"))
            .or_insert(14);
        let value = WrapperTable::new(table);
        assert_eq!(
            value.to_cbor_bytes(),
            [
                map_def(4),
                cbor_int(1, Sz::Inline),
                cbor_int(10, Sz::Inline),
                cbor_int(2, Sz::Inline),
                cbor_int(12, Sz::Inline),
                cbor_int(3, Sz::Inline),
                cbor_int(12, Sz::Inline),
                cbor_int(4, Sz::Inline),
                cbor_int(14, Sz::Inline),
            ]
            .concat()
        );
        deser_test(&value);
    }

    // ---- the six float prelude names under --preserve-encodings ---------------------------------
    // Same six VALUE classes as the default profile (see tests/core/tests.rs), plus the one thing
    // only this profile has: a RECORDED head. Reads accept ANY head and judge the decoded value, so
    // a recorded head is NOT confined to the class's own width — an `fb`-headed 1.5 is a `float16`
    // that must replay as `fb`. A fresh value falls back to its shortest lossless form, which for a
    // member IS the class's declared width.

    // 1.5 — shortest form `f9`, so a `float16` value.
    const F9_1_5: &[u8] = &[0xf9, 0x3e, 0x00];
    const FA_1_5: &[u8] = &[0xfa, 0x3f, 0xc0, 0x00, 0x00];
    const FB_1_5: &[u8] = &[0xfb, 0x3f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    // 1e10 — binary32-exact, outside binary16's range, so shortest form `fa`: a `float32` value.
    const FA_1E10: &[u8] = &[0xfa, 0x50, 0x15, 0x02, 0xf9];
    const FB_1E10: &[u8] = &[0xfb, 0x42, 0x02, 0xa0, 0x5f, 0x20, 0x00, 0x00, 0x00];
    // 1.1 — needs the full binary64 mantissa, so shortest form `fb`: a `float64` value.
    const FB_1_1: &[u8] = &[0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a];

    fn float_heads_bytes(items: &[&[u8]]) -> Vec<u8> {
        let mut v = arr_def(6);
        for i in items {
            v.extend_from_slice(i);
        }
        v
    }

    #[test]
    fn float_heads_replay_every_recorded_head_including_wider_than_the_class() {
        // `float_heads = [h: float16, u: float16-32, w: float32-64, s: float32, d: float64,
        //  f: float]`. Each row feeds every member a value ITS class contains, at a head that
        // carries it — including heads wider than the class's own width, which is precisely what a
        // conforming preferred-serialization peer is free NOT to have used.
        for (h, u, w, sfa, f) in [
            (F9_1_5, F9_1_5, FA_1E10, FA_1E10, F9_1_5),
            (FA_1_5, FA_1_5, FB_1E10, FB_1E10, FA_1_5),
            (FB_1_5, FB_1_5, FB_1E10, FA_1E10, FB_1_5),
        ] {
            let bytes = float_heads_bytes(&[h, u, w, sfa, FB_1_1, f]);
            let d = FloatHeads::from_cbor_bytes(&bytes).unwrap();
            assert_eq!((d.h, d.u, d.s), (1.5f32, 1.5f32, 1e10f32));
            assert_eq!((d.w, d.d, d.f), (1e10f64, 1.1f64, 1.5f64));
            assert_eq!(d.to_cbor_bytes(), bytes, "a recorded head replays exactly");
        }
    }

    #[test]
    fn float_replays_a_non_minimal_recorded_head() {
        // A RECORDED head is data and outranks the shortest-form write rule, so 1.5 read at `#7.26`
        // or `#7.27` re-encodes at the head it arrived on and not at `#7.25`. Asserted on the
        // unconstrained `float` member, where the two rules are visibly distinguishable.
        for recorded in [FA_1_5, FB_1_5] {
            let bytes =
                float_heads_bytes(&[F9_1_5, F9_1_5, FA_1E10, FA_1E10, FB_1_1, recorded]);
            let d = FloatHeads::from_cbor_bytes(&bytes).unwrap();
            assert_eq!(d.f, 1.5f64);
            assert_eq!(
                d.to_cbor_bytes(),
                bytes,
                "a non-minimal recorded head on `float` replays verbatim",
            );
            // ... and dropping the record falls back to the shortest form, so the two rules are
            // distinguishable rather than accidentally agreeing on this value.
            let mut fresh = d.clone();
            fresh.encodings = None;
            assert_eq!(
                fresh.to_cbor_bytes(),
                float_heads_bytes(&[F9_1_5, F9_1_5, FA_1E10, FA_1E10, FB_1_1, F9_1_5]),
                "with no record, a float writes its shortest lossless form",
            );
        }
    }

    #[test]
    fn float_heads_reject_values_outside_their_class() {
        let in_class: Vec<&[u8]> = vec![F9_1_5, F9_1_5, FA_1E10, FA_1E10, FB_1_1, F9_1_5];
        FloatHeads::from_cbor_bytes(&float_heads_bytes(&in_class)).unwrap();
        // (member index, a value that member's class does NOT contain, the rejection it must
        // produce). The reason column is the runtime's own spelling of each row's `why`, kept
        // beside it as a comment: naming the width class BOTH sides is what keeps a row from
        // passing on some other failure that happens to reach the same outcome.
        for (idx, bad, expect) in [
            // 1e10 is a float32, not a float16
            (0usize, FA_1E10, "Expected a float16 value, found a float32 value"),
            // 1.1 is a float64, not a float16
            (0, FB_1_1, "Expected a float16 value, found a float64 value"),
            // 1.1 is a float64, not a float16-32
            (1, FB_1_1, "Expected a float16 - float32 value, found a float64 value"),
            // an fb-headed 1.5 is still a float16, not a float32-64
            (2, FB_1_5, "Expected a float32 - float64 value, found a float16 value"),
            // an fa-headed 1.5 is still a float16, not a float32
            (3, FA_1_5, "Expected a float32 value, found a float16 value"),
            // 1.1 is a float64, not a float32
            (3, FB_1_1, "Expected a float32 value, found a float64 value"),
            // an fb-headed 1.5 is still a float16, not a float64
            (4, FB_1_5, "Expected a float64 value, found a float16 value"),
            // 1e10 is a float32, not a float64
            (4, FA_1E10, "Expected a float64 value, found a float32 value"),
        ] {
            let mut items = in_class.clone();
            items[idx] = bad;
            assert_decode_reject_reason::<FloatHeads>(&float_heads_bytes(&items), expect);
        }
    }

    #[test]
    fn float_heads_fresh_values_write_the_shortest_form() {
        // No recorded width anywhere: every member writes its value's shortest lossless form, which
        // for a member of a constrained class IS that class's declared width — no separate rule.
        let value = FloatHeads::new(1.5, 1.5, 1.1, 1e10, 1.1, 1.5);
        let b = value.to_cbor_bytes();
        assert_eq!(&b[1..4], F9_1_5, "float16");
        assert_eq!(&b[4..7], F9_1_5, "float16-32 holding a float16 value");
        assert_eq!(b[7], 0xfb, "float32-64 holding a float64 value");
        assert_eq!(&b[16..21], FA_1E10, "float32");
        assert_eq!(b[21], 0xfb, "float64");
        assert_eq!(b[30], 0xf9, "float takes the shortest form too");
    }

    #[test]
    fn float_heads_non_member_fails_serialize_loudly() {
        // 1.5 is a `float16` value, so it is not a `float32` one: no head at which writing it into
        // `s` is right, and the write fails rather than emitting bytes our own reader refuses.
        let value = FloatHeads::new(1.5, 1.5, 1.1, 1.5, 1.1, 1.5);
        let mut buf = cbor_event::se::Serializer::new_vec();
        assert!(cbor_event::se::Serialize::serialize(&value, &mut buf).is_err());
    }

    #[test]
    fn float_heads_preserve_nan_payloads_at_every_width() {
        let f9_nan: &[u8] = &[0xf9, 0x7e, 0x01];
        let fa_nan: &[u8] = &[0xfa, 0x7f, 0xc0, 0x00, 0x01];
        let fb_nan: &[u8] = &[0xfb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01];
        let bytes = float_heads_bytes(&[f9_nan, f9_nan, fa_nan, fa_nan, fb_nan, fb_nan]);
        let d = FloatHeads::from_cbor_bytes(&bytes).unwrap();
        assert!(d.h.is_nan() && d.u.is_nan() && d.s.is_nan());
        assert_eq!(d.to_cbor_bytes(), bytes);
    }

    // `fixed_special_type_choice_brute = true / null / tstr` — two fixed specials in one choice.
    // bool and null share CBOR major type 7, so no type-match dispatch can separate the arms and
    // the enum is emitted through the BRUTE-FORCE try-each-arm path instead: one probe closure per
    // arm, whose `Ok` value the emitter matches to pick the variant. A fixed bool/null arm binds
    // NOTHING there (no value, no encoding sidecar), and the empty binding used to emit an
    // unterminated `()` ahead of the appended `Ok(())` — invalid Rust, so the whole spec failed
    // generation at rustfmt with zero files written. That third emission site is guarded now;
    // these are the EXECUTION assertions the compile-only fix never made, one per arm in both
    // directions plus the no-variant-matched floor.
    #[test]
    fn fixed_special_brute_force_arms_round_trip_both_directions() {
        // encode: each arm writes exactly its own wire form and nothing else.
        assert_eq!(
            FixedSpecialTypeChoiceBrute::new_true().to_cbor_bytes(),
            vec![0xf5],
        );
        assert_eq!(
            FixedSpecialTypeChoiceBrute::new_null().to_cbor_bytes(),
            vec![0xf6],
        );
        assert_eq!(
            FixedSpecialTypeChoiceBrute::new_text(String::from("hi")).to_cbor_bytes(),
            cbor_string("hi"),
        );
        // decode: each wire form picks its own variant. The bool and null bytes differ only in the
        // low 5 bits of one major-7 head, which is precisely what the brute-force dispatch must
        // resolve by TRYING the arms rather than by classifying the major type.
        assert!(matches!(
            FixedSpecialTypeChoiceBrute::from_cbor_bytes(&[0xf5]).unwrap(),
            FixedSpecialTypeChoiceBrute::True,
        ));
        assert!(matches!(
            FixedSpecialTypeChoiceBrute::from_cbor_bytes(&[0xf6]).unwrap(),
            FixedSpecialTypeChoiceBrute::Null,
        ));
        match FixedSpecialTypeChoiceBrute::from_cbor_bytes(&cbor_string("hi")).unwrap() {
            FixedSpecialTypeChoiceBrute::Text { text, .. } => assert_eq!(text, "hi"),
            other => panic!("a text arm decoded as {other:?}"),
        }
        // full round trips through the shared helper (byte-identity + full consumption).
        deser_test(&FixedSpecialTypeChoiceBrute::new_true());
        deser_test(&FixedSpecialTypeChoiceBrute::new_null());
        deser_test(&FixedSpecialTypeChoiceBrute::new_text(String::from("hi")));
        // preserve contract: the two fixed arms carry no encoding state, but the text arm's
        // `StringEncoding` still must survive the brute-force probe closure — an irregular head
        // replays byte-identically rather than being normalized away by the retry machinery.
        for sz in [
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Len(Sz::Eight),
            StringLenSz::Indefinite(vec![(1, Sz::Inline), (1, Sz::Four)]),
        ] {
            let bytes = cbor_str_sz("hi", sz);
            let d = FixedSpecialTypeChoiceBrute::from_cbor_bytes(&bytes).unwrap();
            assert_eq!(d.to_cbor_bytes(), bytes);
        }
        // no variant matched: `false` is the sharp one — it is a bool, so an arm that merely read
        // a bool instead of VERIFYING the constant would accept it as `True`.
        // Each row pins the CAUSE its own arm reported, not just the outer "no variant"
        // verdict: the verdict is identical for every non-matching input, so on its own it cannot
        // tell "the True arm verified the constant and refused" from "the True arm was never
        // reached".
        for (bad, expect) in [
            // false: bool-typed, wrong constant
            (&[0xf4u8][..], "Expected fixed value true found false"),
            // uint
            (&[0x00][..], "No variant matched"),
            // empty bytes
            (&[0x40][..], "No variant matched"),
            // a float — major 7 like the two fixed arms, but neither of them
            (&[0xf9, 0x3c, 0x00][..], "Expected Special::Bool, received Float(1.0)"),
        ] {
            assert_decode_reject_reason::<FixedSpecialTypeChoiceBrute>(bad, expect);
        }
    }
}
