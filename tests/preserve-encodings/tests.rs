#[cfg(test)]
mod tests {
    use super::*;

    fn deser_test<T: Deserialize + ToBytes>(orig: &T) {
        print_cbor_types("orig", &orig.to_bytes());
        let deser = T::deserialize(&mut Deserializer::from(std::io::Cursor::new(orig.to_bytes()))).unwrap();
        print_cbor_types("deser", &deser.to_bytes());
        assert_eq!(orig.to_bytes(), deser.to_bytes());
    }

    #[test]
    fn struct_array() {
        let mut foo = Foo::new(436, String::from("jfkdsjfd"), vec![1, 1, 1]);
        deser_test(&foo);
        let definite_bytes = foo.to_bytes();
        let mut encoding = FooEncoding::default();
        encoding.len_encoding = LenEncoding::Indefinite;
        foo.encodings = Some(encoding);
        deser_test(&foo);
        let indefinite_bytes = foo.to_bytes();
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
        let definite_bytes = bar.to_bytes();
        bar_encoding.len_encoding = LenEncoding::Indefinite;
        bar.encodings = Some(bar_encoding);
        deser_test(&bar);
        let indefinite_bytes = bar.to_bytes();
        let default_indef_bytes = vec![
            vec![MAP_INDEF],
                cbor_string("foo"),
                    cbor_tag(13),
                        bar.foo.to_bytes(),
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
                        bar.foo.to_bytes(),
                cbor_string("five"),
                    vec![0x05u8],
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let mut bar_canonical: Bar = from_bytes(canonical_bytes.clone()).unwrap();
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
                                bar.foo.to_bytes(),
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
                        let irregular_bar = Bar::from_bytes(irregular_encoding.clone()).unwrap();
                        print_cbor_types("irregular_bar.to_bytes()", &irregular_bar.to_bytes());
                        assert_eq!(irregular_bar.to_bytes(), irregular_encoding);
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
        assert_eq!(orig.to_bytes(), expected);
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
        let mut other_order: TableArrMembers = from_bytes(indef_other_order.clone()).unwrap();
        assert_eq!(other_order.to_bytes(), indef_other_order);
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
                let irregular = DeeplyNested::from_bytes(irregular_bytes.clone()).unwrap();
                assert_eq!(irregular_bytes, irregular.to_bytes());
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
            let irregular = String64::from_bytes(irregular_bytes.clone()).unwrap();
            assert_eq!(irregular_bytes, irregular.to_bytes());
        }
        let _ = String64::from_bytes(cbor_str_sz(&(0..64).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Two))).unwrap();
        assert!(String64::from_bytes(cbor_str_sz(&(0..65).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Two))).is_err());
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
                let irregular = String1632::from_bytes(irregular_bytes.clone()).unwrap();
                assert_eq!(irregular_bytes, irregular.to_bytes());
            }
        }
        let _ = String1632::from_bytes(vec![
            cbor_tag_sz(7, Sz::One),
            cbor_str_sz(&(0..16).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::One)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>()).unwrap();
        let _ = String1632::from_bytes(vec![
            cbor_tag_sz(7, Sz::Two),
            cbor_str_sz(&(0..32).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Two)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>()).unwrap();
        assert!(String1632::from_bytes(vec![
            cbor_tag_sz(7, Sz::Inline),
            cbor_str_sz(&(0..15).map(|_| "?").collect::<String>(), StringLenSz::Len(Sz::Inline)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>()).is_err());
        assert!(String1632::from_bytes(vec![
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
                let irregular_0 = TypeChoice::from_bytes(irregular_bytes_0.clone()).unwrap();
                assert_eq!(irregular_bytes_0, irregular_0.to_bytes());
                let irregular_hello_world = TypeChoice::from_bytes(irregular_bytes_hello_world.clone()).unwrap();
                assert_eq!(irregular_bytes_hello_world, irregular_hello_world.to_bytes());
                let irregular_uint = TypeChoice::from_bytes(irregular_bytes_uint.clone()).unwrap();
                assert_eq!(irregular_bytes_uint, irregular_uint.to_bytes());
                let irregular_text = TypeChoice::from_bytes(irregular_bytes_text.clone()).unwrap();
                assert_eq!(irregular_bytes_text, irregular_text.to_bytes());
                let irregular_tagged_arr = TypeChoice::from_bytes(irregular_bytes_tagged_arr.clone()).unwrap();
                assert_eq!(irregular_bytes_tagged_arr, irregular_tagged_arr.to_bytes());
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
                let irregular_3 = GroupChoice::from_bytes(irregular_bytes_3.clone()).unwrap();
                assert_eq!(irregular_bytes_3, irregular_3.to_bytes());
                let irregular_tagged_2 = GroupChoice::from_bytes(irregular_bytes_tagged_2.clone()).unwrap();
                assert_eq!(irregular_bytes_tagged_2, irregular_tagged_2.to_bytes());
                let irregular_foo = GroupChoice::from_bytes(irregular_bytes_foo.clone()).unwrap();
                assert_eq!(irregular_bytes_foo, irregular_foo.to_bytes());
                let irregular_inlined = GroupChoice::from_bytes(irregular_bytes_inlined.clone()).unwrap();
                assert_eq!(irregular_bytes_inlined, irregular_inlined.to_bytes());
                let irregular_plain = GroupChoice::from_bytes(irregular_bytes_plain.clone()).unwrap();
                assert_eq!(irregular_bytes_plain, irregular_plain.to_bytes());
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
                    arr_sz(2, *def_enc),    
                        cbor_bytes_sz(irregular_foo_bytes, foo_bytes_enc),
                        cbor_bytes_sz(cbor_int(5, *def_enc), StringLenSz::Len(*def_enc)),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular = CborInCbor::from_bytes(irregular_bytes.clone()).unwrap();
                assert_eq!(irregular_bytes, irregular.to_bytes());
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
                    cbor_int(umins[i_8], def_encodings[i]),
                    cbor_int(umins[i_16], def_encodings[i]),
                    cbor_int(umins[i_32], def_encodings[i]),
                    cbor_int(umins[i_64], def_encodings[i]),
                    cbor_int(imins[i_8], def_encodings[i]),
                    cbor_int(imins[i_16], def_encodings[i]),
                    cbor_int(imins[i_32], def_encodings[i]),
                    cbor_int(imins[i_64], def_encodings[i]),
                vec![BREAK],
            ].into_iter().flatten().clone().collect::<Vec<u8>>();
            let irregular_min = SignedInts::from_bytes(irregular_bytes_min.clone()).unwrap();
            assert_eq!(irregular_bytes_min, irregular_min.to_bytes());
            let irregular_bytes_max = vec![
                arr_sz(8, def_encodings[i]),
                    cbor_int(umaxs[i_8], def_encodings[i]),
                    cbor_int(umaxs[i_16], def_encodings[i]),
                    cbor_int(umaxs[i_32], def_encodings[i]),
                    cbor_int(umaxs[i_64], def_encodings[i]),
                    cbor_int(imaxs[i_8], def_encodings[i]),
                    cbor_int(imaxs[i_16], def_encodings[i]),
                    cbor_int(imaxs[i_32], def_encodings[i]),
                    cbor_int(imaxs[i_64], def_encodings[i]),
            ].into_iter().flatten().clone().collect::<Vec<u8>>();
            let irregular_max = SignedInts::from_bytes(irregular_bytes_max.clone()).unwrap();
            assert_eq!(irregular_bytes_max, irregular_max.to_bytes());
        }
    }
}