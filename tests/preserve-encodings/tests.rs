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
        assert!(OptFixedArr::from_cbor_bytes(&wrong).is_err());

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
        assert!(String64::from_cbor_bytes(&cbor_str_sz(&(0..65).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Two))).is_err());
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
        assert!(String1632::from_cbor_bytes(&vec![
            cbor_tag_sz(7, Sz::Inline),
            cbor_str_sz(&(0..15).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Inline)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>()).is_err());
        assert!(String1632::from_cbor_bytes(&vec![
            cbor_tag_sz(7, Sz::Eight),
            cbor_str_sz(&(0..33).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Eight)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>()).is_err());
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
        assert!(
            TopLevelTaggedRange::from_cbor_bytes(&cbor_int(7, cbor_event::Sz::Inline)).is_err()
        );
        // out-of-window tagged input is rejected
        assert!(TopLevelTaggedRange::from_cbor_bytes(
            &[cbor_tag(5), cbor_int(11, cbor_event::Sz::Inline)].concat()
        )
        .is_err());
        // wrong tag is rejected
        assert!(TopLevelTaggedRange::from_cbor_bytes(
            &[cbor_tag(4), cbor_int(7, cbor_event::Sz::Inline)].concat()
        )
        .is_err());
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
        // twin — bool-only: preserve-encodings does not support floats): the peek must rewind by
        // the ACTUAL width `special()` consumed, not a hardcoded 1 byte.
        let some: Vec<u8> = [arr_def(1), vec![0xf5]].concat();
        let d = NullableSpecials::from_cbor_bytes(&some).unwrap();
        assert_eq!(d.b, Some(true));
        assert_eq!(d.to_cbor_bytes(), some);
        let none: Vec<u8> = [arr_def(1), vec![0xf6]].concat();
        let d = NullableSpecials::from_cbor_bytes(&none).unwrap();
        assert!(d.b.is_none());
        // malformed two-byte simple in the nullable-bool slot (`f8 f5`): the 2.4.0 peek consumed
        // 2 bytes, rewound 1, and re-read the PAYLOAD byte f5 as `true` — accepting malformed
        // input. This and the RFC 8949 §3.3 non-well-formed simples all reject; an f9 float in
        // the bool slot rejects as a type mismatch (not a mis-decode).
        for bad in [
            &[0x81u8, 0xf8, 0xf5][..],
            &[0x81, 0xfc],
            &[0x81, 0xfd],
            &[0x81, 0xfe],
            &[0x81, 0xf8, 0x1f],
            &[0x81, 0xff],
            &[0x81, 0xf9, 0x42, 0x00],
        ] {
            assert!(NullableSpecials::from_cbor_bytes(bad).is_err());
        }
    }

    // --- OrderedHashMap mutation semantics, pinned at the WIRE level ---
    //
    // A table's iteration order IS its serialized key order (the emitted map loop walks
    // `inner.iter()` verbatim), so any change in the backing insertion-ordered map's mutation
    // semantics silently rewrites the bytes of a re-serialized value. Under --preserve-encodings
    // that is a fidelity break, not a cosmetic one: a value decoded from the wire and written back
    // after an in-place edit must keep every key where the wire put it.
    //
    // These three tests pin the semantics the wrapper is contractually required to have,
    // independently of WHICH insertion-ordered map backs it. They assert only through
    // `OrderedHashMap`'s own surface and `to_cbor_bytes()` — no backing-crate path is named — so
    // they stay meaningful across a swap of that backing. Written against the current backing and
    // green on it, so a later swap that changes any of the three is caught as a behavior change
    // rather than blessed as new bytes.
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
}
