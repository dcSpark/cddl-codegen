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
        foo.definite_encoding = false;
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
        bar.definite_encoding = false;
        deser_test(&bar);
        bar.definite_encoding = true;
        // full test with key 5 (but without key "derp")
        bar.key_5 = Some("text".into());
        let definite_bytes = bar.to_bytes();
        bar.definite_encoding = false;
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
        assert_eq!(bar_canonical.definite_encoding, true);
        assert_eq!(Some(vec![2, 3, 0, 4]), bar_canonical.orig_deser_order);
        // get rid of other info and it should be identical
        bar_canonical.definite_encoding = false;
        bar_canonical.orig_deser_order = None;
        assert_eq!(bar, bar_canonical);
    }

    #[test]
    fn table_arr_members() {
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
        assert_eq!(orig.definite_encoding, true);
        assert_eq!(orig.arr_definite_encoding, true);
        assert_eq!(orig.arr2_definite_encoding, true);
        assert_eq!(orig.arr2[0].definite_encoding, true);
        assert_eq!(orig.table_definite_encoding, true);

        assert_eq!(other_order.orig_deser_order, Some(vec![1, 2, 0]));
        assert_eq!(other_order.definite_encoding, false);
        assert_eq!(other_order.arr_definite_encoding, false);
        assert_eq!(other_order.arr2_definite_encoding, false);
        assert_eq!(other_order.arr2[0].definite_encoding, false);
        assert_eq!(other_order.table_definite_encoding, false);
        
        other_order.orig_deser_order = None;
        other_order.definite_encoding = true;
        other_order.arr_definite_encoding = true;
        other_order.arr2_definite_encoding = true;
        other_order.arr2[0].definite_encoding = true;
        other_order.table_definite_encoding = true;
        other_order.table = table;
        assert_eq!(orig, other_order);
        other_order.orig_deser_order = Some(vec![0, 1, 2]);
        assert_eq!(orig.to_bytes(), other_order.to_bytes());
    }
}