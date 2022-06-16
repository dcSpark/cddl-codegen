#[cfg(test)]
mod tests {
    use super::*;

    fn deser_test<T: Deserialize + ToBytes>(orig: &T, force_canonical: bool) {
        print_cbor_types("orig", &orig.to_bytes(force_canonical));
        let deser = T::deserialize(&mut Deserializer::from(std::io::Cursor::new(orig.to_bytes(force_canonical)))).unwrap();
        print_cbor_types("deser", &deser.to_bytes(force_canonical));
        assert_eq!(orig.to_bytes(force_canonical), deser.to_bytes(force_canonical));
    }

    #[test]
    fn struct_array() {
        let non_canonical_bytes = vec![
            vec![ARR_INDEF],
                vec![0x09],
                cbor_string("jdskfjdsfkjad"),
                vec![0x42, 0xB4, 0xD3],
            vec![BREAK]
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let foo: Foo = from_bytes(non_canonical_bytes.clone()).unwrap();
        assert_eq!(foo.to_bytes(false), non_canonical_bytes);
        let canonical_bytes = vec![
            arr_def(3),
                vec![0x09],
                cbor_string("jdskfjdsfkjad"),
                vec![0x42, 0xB4, 0xD3],
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        assert_eq!(foo.to_bytes(true), canonical_bytes);
        deser_test(&foo, true);
        deser_test(&foo, false);
    }

    #[test]
    fn struct_map() {
        // tests for indefinite inside both map/array structs (original test)
        let non_canonical_bytes = vec![
            vec![MAP_INDEF],
                cbor_string("foo"),
                    cbor_tag(13),
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
        let bar: Bar = from_bytes(non_canonical_bytes.clone()).unwrap();
        assert_eq!(bar.to_bytes(false), non_canonical_bytes);
        let canonical_bytes = vec![
            map_def(4),
                vec![0x01u8],
                    vec![0x15],
                cbor_string("foo"),
                    cbor_tag(13),
                    arr_def(3),
                        vec![0x09],
                        cbor_string("jdskfjdsfkjad"),
                        vec![0x42, 0xB4, 0xD3],
                cbor_string("derp"),
                    vec![0x013u8],
                cbor_string("five"),
                    vec![0x05u8],
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        assert_eq!(bar.to_bytes(true), canonical_bytes);
        deser_test(&bar, true);
        deser_test(&bar, false);

        // tests for all other possible encodings (new tests after complete encoding preservation)
        let canonical_bytes_all = vec![
            map_def(5),
                vec![0x01u8],
                    vec![NULL],
                vec![0x05],
                    cbor_string("XYZ"),
                cbor_string("foo"),
                    cbor_tag(13),
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
                    let mut canonical_bar = Bar::from_bytes(canonical_bytes_all.clone()).unwrap();
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
                        let irregular_bar = Bar::from_bytes(irregular_encoding.clone()).unwrap();
                        print_cbor_types("irregular_bar.to_bytes(false)", &irregular_bar.to_bytes(false));
                        assert_eq!(irregular_bar.to_bytes(false), irregular_encoding);
                        print_cbor_types("irregular_bar.to_bytes(true)", &irregular_bar.to_bytes(true));
                        assert_eq!(irregular_bar.to_bytes(true), canonical_bar.to_bytes(false));
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
        let table: TableArrMembers = from_bytes(non_canonical_bytes.clone()).unwrap();
        assert_eq!(table.to_bytes(false), non_canonical_bytes);
        let canonical_bytes = vec![
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
                    map_def(3),
                        vec![0x04],
                            cbor_string("Four"),
                        vec![0x08],
                            cbor_string("Eight"),
                        vec![16u8],
                            cbor_string("Sixteen"),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        print_cbor_types("canonical_bytes", &canonical_bytes);
        assert_eq!(table.to_bytes(true), canonical_bytes);
        deser_test(&table, true);
        deser_test(&table, false);
    }
}
