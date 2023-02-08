#[cfg(test)]
mod tests {
    use super::*;
    use cbor_event::{Sz, StringLenSz};

    fn deser_test_orig<T: Deserialize + Serialize>(orig: &T) {
        print_cbor_types("orig (original enc)", &orig.to_cbor_bytes());
        let deser = T::deserialize(&mut Deserializer::from(std::io::Cursor::new(orig.to_cbor_bytes()))).unwrap();
        print_cbor_types("deser", &deser.to_cbor_bytes());
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
    }

    fn deser_test_canonical<T: Deserialize + Serialize>(orig: &T) {
        print_cbor_types("orig (canonical)", &orig.to_canonical_cbor_bytes());
        let deser = T::deserialize(&mut Deserializer::from(std::io::Cursor::new(orig.to_canonical_cbor_bytes()))).unwrap();
        print_cbor_types("deser", &deser.to_canonical_cbor_bytes());
        assert_eq!(orig.to_canonical_cbor_bytes(), deser.to_canonical_cbor_bytes());
    }

    #[test]
    fn struct_array() {
        let non_canonical_bytes = vec![
            cbor_tag_sz(11, Sz::Eight),
                vec![ARR_INDEF],
                    vec![0x09],
                    cbor_string("jdskfjdsfkjad"),
                    vec![0x42, 0xB4, 0xD3],
                vec![BREAK]
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let foo = Foo::from_cbor_bytes(&non_canonical_bytes).unwrap();
        assert_eq!(foo.to_cbor_bytes(), non_canonical_bytes);
        let canonical_bytes = vec![
            cbor_tag(11),
                arr_def(3),
                    vec![0x09],
                    cbor_string("jdskfjdsfkjad"),
                    vec![0x42, 0xB4, 0xD3],
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        assert_eq!(foo.to_canonical_cbor_bytes(), canonical_bytes);
        deser_test_canonical(&foo);
        deser_test_orig(&foo);
    }

    #[test]
    fn struct_map() {
        // tests for indefinite inside both map/array structs (original test)
        let non_canonical_bytes = vec![
            vec![MAP_INDEF],
                cbor_string("foo"),
                    cbor_tag_sz(13, Sz::One),
                        cbor_tag_sz(11, Sz::Two),
                            vec![ARR_INDEF],
                                vec![0x09],
                                cbor_string("jdskfjdsfkjad"),
                                vec![0x42, 0xB4, 0xD3],
                            vec![BREAK],
                vec![0x01u8],
                    vec![0x15],
                cbor_string("derp"),
                    vec![0x013u8],
                cbor_string("five"),
                    vec![0x05u8],
            vec![BREAK]
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let bar = Bar::from_cbor_bytes(&non_canonical_bytes).unwrap();
        assert_eq!(bar.to_cbor_bytes(), non_canonical_bytes);
        let canonical_bytes = vec![
            map_def(4),
                vec![0x01u8],
                    vec![0x15],
                cbor_string("foo"),
                    cbor_tag(13),
                        cbor_tag(11),
                            arr_def(3),
                                vec![0x09],
                                cbor_string("jdskfjdsfkjad"),
                                vec![0x42, 0xB4, 0xD3],
                cbor_string("derp"),
                    vec![0x013u8],
                cbor_string("five"),
                    vec![0x05u8],
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        assert_eq!(bar.to_canonical_cbor_bytes(), canonical_bytes);
        deser_test_canonical(&bar);
        deser_test_orig(&bar);

        // tests for all other possible encodings (new tests after complete encoding preservation)
        let canonical_bytes_all = vec![
            map_def(5),
                vec![0x01u8],
                    vec![NULL],
                vec![0x05],
                    cbor_string("XYZ"),
                cbor_string("foo"),
                    cbor_tag(13),
                        cbor_tag(11),
                            arr_def(3),
                                vec![0x09],
                                cbor_string("abcd"),
                                vec![0x44, 0xB4, 0xD3, 0x09, 0x00],
                cbor_string("derp"),
                    vec![0x013u8],
                cbor_string("five"),
                    vec![0x05u8],
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let str_3_encodings = vec![
            StringLenSz::Len(Sz::Inline),
            StringLenSz::Len(Sz::Eight),
            StringLenSz::Indefinite(vec![(0, Sz::Inline), (2, Sz::One), (1, Sz::Four), (0, Sz::Eight)])
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
                    let mut canonical_bar = Bar::from_cbor_bytes(&canonical_bytes_all).unwrap();
                    if !has_5 {
                        canonical_bar.key_5 = None;
                    }
                    if !has_derp {
                        canonical_bar.derp = None;
                    }
                    let keys = [
                        [
                            cbor_str_sz("foo", str_3.clone()),
                                cbor_tag_sz(13, def_enc),
                                cbor_tag_sz(11, def_enc),
                                arr_sz(3, def_enc),
                                    cbor_int(9, def_enc),
                                    cbor_str_sz("abcd", str_4.clone()),
                                    cbor_bytes_sz(vec![0xB4, 0xD3, 0x09, 0x00], str_4.clone()),
                        ].into_iter().flatten().copied().collect::<Vec<u8>>(),
                        if has_5 {
                            [
                                cbor_int(5, def_enc),
                                    cbor_str_sz("XYZ", str_3.clone()),
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
                                    cbor_int(19, def_enc),
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
                        print_cbor_types("irregular_bar.to_canonical_cbor_bytes()", &irregular_bar.to_canonical_cbor_bytes());
                        assert_eq!(irregular_bar.to_canonical_cbor_bytes(), canonical_bar.to_cbor_bytes());
                    }
                }
            }
        }
    }

    #[test]
    fn table_arr_members() {
        let non_canonical_bytes = vec![
            vec![MAP_INDEF],
                cbor_str_sz("arr2", StringLenSz::Indefinite(vec![(2, Sz::Inline), (0, Sz::Inline), (2, Sz::Four)])),
                    vec![ARR_INDEF],
                        cbor_tag_sz(11, Sz::Four),
                            vec![ARR_INDEF],
                                cbor_int(0, Sz::Eight),
                                cbor_str_sz("Zero", StringLenSz::Len(Sz::Four)),
                                cbor_bytes_sz(vec![], StringLenSz::Indefinite(vec![(0, Sz::Inline), (0, Sz::Four)])),
                            vec![BREAK],
                    vec![BREAK],
                cbor_str_sz("table", StringLenSz::Len(Sz::One)),
                    map_sz(3, Sz::Four),
                        cbor_int(16, Sz::Eight),
                            cbor_str_sz("Sixteen", StringLenSz::Indefinite(vec![(3, Sz::Inline), (4, Sz::Four)])),
                        cbor_int(4, Sz::Two),
                            cbor_str_sz("Four", StringLenSz::Len(Sz::Two)),
                        cbor_int(8, Sz::Four),
                            cbor_str_sz("Eight", StringLenSz::Len(Sz::Inline)),
                cbor_str_sz("arr", StringLenSz::Indefinite(vec![(3, Sz::Four)])),
                    vec![ARR_INDEF],
                        cbor_int(1, Sz::Eight),
                        cbor_int(3, Sz::Inline),
                        cbor_int(6, Sz::Four),
                    vec![BREAK],
            vec![BREAK],
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        print_cbor_types("non_canonical_bytes", &non_canonical_bytes);
        let table = TableArrMembers::from_cbor_bytes(&non_canonical_bytes).unwrap();
        assert_eq!(table.to_cbor_bytes(), non_canonical_bytes);
        let canonical_bytes = vec![
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
                    map_def(3),
                        vec![0x04],
                            cbor_string("Four"),
                        vec![0x08],
                            cbor_string("Eight"),
                        vec![16u8],
                            cbor_string("Sixteen"),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        print_cbor_types("canonical_bytes", &canonical_bytes);
        assert_eq!(table.to_canonical_cbor_bytes(), canonical_bytes);
        deser_test_canonical(&table);
        deser_test_orig(&table);
    }

    #[test]
    fn deeply_nested() {
        let canonical_bytes = vec![
            arr_def(1),
                map_def(2),
                    cbor_tag(14),
                            cbor_bytes_sz(vec![0xAA, 0xBB, 0xCC], StringLenSz::Len(Sz::Inline)),
                                map_def(2),
                                    cbor_int(3, Sz::Inline),
                                        map_def(1),
                                            cbor_tag(9),
                                                cbor_int(2, Sz::Inline),
                                                    arr_def(1),
                                                        cbor_tag(18),
                                                            arr_def(1),
                                                                cbor_string("cbor"),
                                    cbor_int(5, Sz::Inline),
                                        map_def(0),
                    cbor_tag(14),
                        cbor_bytes_sz(vec![0xBA, 0xAD, 0xF0, 0x0D], StringLenSz::Len(Sz::Inline)),
                            map_def(1),
                                cbor_int(10, Sz::Inline),
                                    map_def(1),
                                        cbor_tag(9),
                                            cbor_int(0, Sz::Inline),
                                                arr_def(2),
                                                    cbor_tag(18),
                                                        arr_def(0),
                                                    cbor_tag(18),
                                                        arr_def(3),
                                                            cbor_string("test"),
                                                            cbor_string("XYZ"),
                                                            cbor_string("ABC"),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
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
                assert_eq!(canonical_bytes, irregular.to_canonical_cbor_bytes());
            }
        }
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
                let canonical_bytes_0 = cbor_int(0, Sz::Inline);
                let irregular_bytes_hello_world = cbor_str_sz("hello world", str_enc.clone());
                let canonical_bytes_hello_world = cbor_string("hello world");
                let irregular_bytes_uint = cbor_int(10, *def_enc);
                let canonical_bytes_uint = cbor_int(10, Sz::Inline);
                let irregular_bytes_text = cbor_str_sz("abcdefghijk", str_enc.clone());
                let canonical_bytes_text = cbor_string("abcdefghijk");
                let irregular_bytes_tagged_arr = vec![
                    cbor_tag_sz(16, *def_enc),
                        arr_sz(2, *def_enc),
                            cbor_int(1, *def_enc),
                            cbor_int(3, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let canonical_bytes_tagged_arr = vec![
                    cbor_tag(16),
                        arr_def(2),
                            cbor_int(1, Sz::Inline),
                            cbor_int(3, Sz::Inline),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_0 = TypeChoice::from_cbor_bytes(&irregular_bytes_0).unwrap();
                assert_eq!(irregular_bytes_0, irregular_0.to_cbor_bytes());
                assert_eq!(canonical_bytes_0, irregular_0.to_canonical_cbor_bytes());
                let irregular_hello_world = TypeChoice::from_cbor_bytes(&irregular_bytes_hello_world).unwrap();
                assert_eq!(irregular_bytes_hello_world, irregular_hello_world.to_cbor_bytes());
                assert_eq!(canonical_bytes_hello_world, irregular_hello_world.to_canonical_cbor_bytes());
                let irregular_uint = TypeChoice::from_cbor_bytes(&irregular_bytes_uint).unwrap();
                assert_eq!(irregular_bytes_uint, irregular_uint.to_cbor_bytes());
                assert_eq!(canonical_bytes_uint, irregular_uint.to_canonical_cbor_bytes());
                let irregular_text = TypeChoice::from_cbor_bytes(&irregular_bytes_text).unwrap();
                assert_eq!(irregular_bytes_text, irregular_text.to_cbor_bytes());
                assert_eq!(canonical_bytes_text, irregular_text.to_canonical_cbor_bytes());
                let irregular_tagged_arr = TypeChoice::from_cbor_bytes(&irregular_bytes_tagged_arr).unwrap();
                assert_eq!(irregular_bytes_tagged_arr, irregular_tagged_arr.to_cbor_bytes());
                assert_eq!(canonical_bytes_tagged_arr, irregular_tagged_arr.to_canonical_cbor_bytes());
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
                let canonical_bytes_3 = vec![
                    arr_def(1),
                        cbor_int(3, Sz::Inline),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_tagged_2 = vec![
                    vec![ARR_INDEF],
                        cbor_tag_sz(10, *def_enc),
                            cbor_int(2, *def_enc),
                    vec![BREAK],
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let canonical_bytes_tagged_2 = vec![
                    arr_def(1),
                        cbor_tag(10),
                            cbor_int(2, Sz::Inline),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_foo = vec![
                    arr_sz(1, *def_enc),
                        cbor_tag_sz(11, *def_enc),
                            vec![ARR_INDEF],
                                cbor_int(9, *def_enc),
                                cbor_str_sz("potato", str_enc.clone()),
                                cbor_bytes_sz(vec![0xF0, 0x0D, 0xF0, 0x0D, 0xF0, 0x0D], str_enc.clone()),
                            vec![BREAK],
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let canonical_bytes_foo = vec![
                    arr_def(1),
                        cbor_tag(11),
                            arr_def(3),
                                cbor_int(9, Sz::Inline),
                                cbor_string("potato"),
                                cbor_bytes_sz(vec![0xF0, 0x0D, 0xF0, 0x0D, 0xF0, 0x0D], StringLenSz::Len(Sz::Inline)),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_inlined = vec![
                    arr_sz(2, *def_enc),
                        cbor_int(0, *def_enc),
                        cbor_int(10, *def_enc),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let canonical_bytes_inlined = vec![
                    arr_def(2),
                        cbor_int(0, Sz::Inline),
                        cbor_int(10, Sz::Inline),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_bytes_plain = vec![
                    arr_sz(2, *def_enc),
                        cbor_tag_sz(13, *def_enc),
                            cbor_int(17, *def_enc),
                        cbor_tag_sz(9, *def_enc),
                            cbor_str_sz("carrot", str_enc.clone()),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let canonical_bytes_plain = vec![
                    arr_def(2),
                        cbor_tag(13),
                            cbor_int(17, Sz::Inline),
                        cbor_tag(9),
                            cbor_string("carrot"),
                ].into_iter().flatten().clone().collect::<Vec<u8>>();
                let irregular_3 = GroupChoice::from_cbor_bytes(&irregular_bytes_3).unwrap();
                assert_eq!(irregular_bytes_3, irregular_3.to_cbor_bytes());
                assert_eq!(canonical_bytes_3, irregular_3.to_canonical_cbor_bytes());
                let irregular_tagged_2 = GroupChoice::from_cbor_bytes(&irregular_bytes_tagged_2).unwrap();
                assert_eq!(irregular_bytes_tagged_2, irregular_tagged_2.to_cbor_bytes());
                assert_eq!(canonical_bytes_tagged_2, irregular_tagged_2.to_canonical_cbor_bytes());
                let irregular_foo = GroupChoice::from_cbor_bytes(&irregular_bytes_foo).unwrap();
                assert_eq!(irregular_bytes_foo, irregular_foo.to_cbor_bytes());
                assert_eq!(canonical_bytes_foo, irregular_foo.to_canonical_cbor_bytes());
                let irregular_inlined = GroupChoice::from_cbor_bytes(&irregular_bytes_inlined).unwrap();
                assert_eq!(irregular_bytes_inlined, irregular_inlined.to_cbor_bytes());
                assert_eq!(canonical_bytes_inlined, irregular_inlined.to_canonical_cbor_bytes());
                let irregular_plain = GroupChoice::from_cbor_bytes(&irregular_bytes_plain).unwrap();
                assert_eq!(irregular_bytes_plain, irregular_plain.to_cbor_bytes());
                assert_eq!(canonical_bytes_plain, irregular_plain.to_canonical_cbor_bytes());
            }
        }
    }

    #[test]
    fn cbor_in_cbor() {
        let canonical_foo_bytes = vec![   
            cbor_tag(11),
                arr_def(3),
                    cbor_int(5, Sz::Inline),
                    cbor_string("???"),
                    cbor_bytes_sz(vec![0xAB, 0xCD, 0xEF], StringLenSz::Len(Sz::Inline)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let canonical_bytes = vec![
            arr_def(2),    
                cbor_bytes_sz(canonical_foo_bytes, StringLenSz::Len(Sz::Inline)),
                cbor_bytes_sz(cbor_int(5, Sz::Inline), StringLenSz::Len(Sz::Inline)),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
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
                let irregular = CborInCbor::from_cbor_bytes(&irregular_bytes).unwrap();
                assert_eq!(irregular_bytes, irregular.to_cbor_bytes());
                assert_eq!(canonical_bytes, irregular.to_canonical_cbor_bytes());
            }
        }
    }
}
