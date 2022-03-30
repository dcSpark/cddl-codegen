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
    }

    #[test]
    fn table() {
        let non_canonical_bytes = vec![
            vec![MAP_INDEF],
                vec![16u8],
                cbor_string("Sixteen"),
                vec![0x04],
                cbor_string("Four"),
                vec![0x08],
                cbor_string("Eight"),
            vec![BREAK]
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let table: Table = from_bytes(non_canonical_bytes.clone()).unwrap();
        assert_eq!(table.to_bytes(false), non_canonical_bytes);
        let canonical_bytes = vec![
            map_def(3),
                vec![0x04],
                cbor_string("Four"),
                vec![0x08],
                cbor_string("Eight"),
                vec![16u8],
                cbor_string("Sixteen"),
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        assert_eq!(table.to_bytes(true), canonical_bytes);
        deser_test(&table, true);
        deser_test(&table, false);
    }

    #[test]
    fn table_arr_members() {
        let non_canonical_bytes = vec![
            vec![MAP_INDEF],
                cbor_string("arr2"),
                vec![ARR_INDEF],
                    vec![ARR_INDEF],
                        vec![0x00],
                        cbor_string("Zero"),
                        vec![0x40],
                    vec![BREAK],
                vec![BREAK],
                cbor_string("arr"),
                vec![ARR_INDEF],
                    vec![0x01, 0x03, 0x06],
                vec![BREAK],
            vec![BREAK],
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let table: TableArrMembers = from_bytes(non_canonical_bytes.clone()).unwrap();
        assert_eq!(table.to_bytes(false), non_canonical_bytes);
        let canonical_bytes = vec![
            map_def(2),
                cbor_string("arr"),
                arr_def(3),
                    vec![0x01, 0x03, 0x06],
                cbor_string("arr2"),
                arr_def(1),
                    arr_def(3),
                        vec![0x00],
                        cbor_string("Zero"),
                        vec![0x40],
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        assert_eq!(table.to_bytes(true), canonical_bytes);
        deser_test(&table, true);
        deser_test(&table, false);
    }
}
