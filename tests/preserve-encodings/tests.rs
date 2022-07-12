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
        foo.encoding = LenEncoding::Indefinite;
        deser_test(&foo);
        let indefinite_bytes = foo.to_bytes();
        assert!(definite_bytes != indefinite_bytes);
        assert_eq!(definite_bytes[0], 0x83u8);
        assert_eq!(indefinite_bytes[0], ARR_INDEF);
        assert_eq!(*indefinite_bytes.last().unwrap(), BREAK);
        // last bit of the the [1, 1, 1]
        assert_eq!(*definite_bytes.last().unwrap(), 1u8);
    }

    #[test]
    fn struct_map() {
        let mut bar = Bar::new(Foo::new(9, String::from("abc"), vec![6, 4]), None);
        // quick test without key 5
        deser_test(&bar);
        bar.encoding = LenEncoding::Indefinite;
        deser_test(&bar);
        bar.encoding = LenEncoding::Definite(cbor_event::Sz::Inline);
        // full test with key 5 (but without key "derp")
        bar.key_5 = Some("text".into());
        let definite_bytes = bar.to_bytes();
        bar.encoding = LenEncoding::Indefinite;
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
        assert_eq!(bar_canonical.encoding, LenEncoding::Canonical);
        assert_eq!(Some(vec![2, 3, 0, 4]), bar_canonical.orig_deser_order);
        // get rid of other info and it should be identical
        bar_canonical.encoding = LenEncoding::Indefinite;
        bar_canonical.orig_deser_order = None;
        //assert_eq!(bar, bar_canonical);
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
        let mut table = linked_hash_map::LinkedHashMap::new();
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
        
        assert_eq!(orig.orig_deser_order, None);
        assert_eq!(orig.encoding, LenEncoding::Canonical);
        assert_eq!(orig.arr_encoding, LenEncoding::Canonical);
        assert_eq!(orig.arr2_encoding, LenEncoding::Canonical);
        assert_eq!(orig.arr2[0].encoding, LenEncoding::Canonical);
        assert_eq!(orig.table_encoding, LenEncoding::Canonical);

        assert_eq!(other_order.orig_deser_order, Some(vec![1, 2, 0]));
        assert_eq!(other_order.encoding, LenEncoding::Indefinite);
        assert_eq!(other_order.arr_encoding, LenEncoding::Indefinite);
        assert_eq!(other_order.arr2_encoding, LenEncoding::Indefinite);
        assert_eq!(other_order.arr2[0].encoding, LenEncoding::Indefinite);
        assert_eq!(other_order.table_encoding, LenEncoding::Indefinite);
        
        other_order.orig_deser_order = None;
        other_order.encoding = LenEncoding::Canonical;
        other_order.arr_encoding = LenEncoding::Canonical;
        other_order.arr2_encoding = LenEncoding::Canonical;
        other_order.arr2[0].encoding = LenEncoding::Canonical;
        other_order.table_encoding = LenEncoding::Canonical;
        other_order.table = table;
        //assert_eq!(orig, other_order);
        other_order.orig_deser_order = Some(vec![0, 1, 2]);
        assert_eq!(orig.to_bytes(), other_order.to_bytes());
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
}