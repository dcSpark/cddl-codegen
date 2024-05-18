#[cfg(test)]
mod tests {
    use super::*;
    use cbor_event::{de::Deserializer, Sz, StringLenSz};
    use serialization::Deserialize;

    fn deser_test<T: Deserialize + ToCBORBytes>(orig: &T) {
        print_cbor_types("orig", &orig.to_cbor_bytes());
        let deser = T::deserialize(&mut Deserializer::from(std::io::Cursor::new(orig.to_cbor_bytes()))).unwrap();
        print_cbor_types("deser", &deser.to_cbor_bytes());
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
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
                        ].into_iter().flatten().copied().collect::<Vec<u8>>(),
                        if has_5 {
                            [
                                cbor_int(5, def_enc),
                                    cbor_str_sz("text", str_4.clone()),
                            ].into_iter().flatten().copied().collect::<Vec<u8>>()
                        } else {
                            vec![]
                        },
                        [
                            cbor_int(1, def_enc),
                                vec![NULL],
                        ].into_iter().flatten().copied().collect::<Vec<u8>>(),
                        if has_derp {
                            [
                                cbor_str_sz("derp", str_4.clone()),
                                    cbor_int(2, def_enc),
                            ].into_iter().flatten().copied().collect::<Vec<u8>>()
                        } else {
                            vec![]
                        },
                        [
                            cbor_str_sz("five", str_4.clone()),
                                cbor_int(5, def_enc),
                        ].into_iter().flatten().copied().collect::<Vec<u8>>(),
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
        assert!(BoundsTypeChoice::new_bytes(vec![0; 64]).is_ok());
        assert!(BoundsTypeChoice::new_bytes(vec![0; 65]).is_err());
        assert!(BoundsGroupChoice::new_a(0, "four".to_owned()).is_ok());
        assert!(BoundsGroupChoice::new_a(0, "hello".to_owned()).is_err());
        deser_test(&BoundsGroupChoice::new_c(Hash::new(vec![]).unwrap(), Hash::new(vec![]).unwrap()));
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
}
