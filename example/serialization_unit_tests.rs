#[cfg(test)]
mod tests {
    use super::*;
    use serialization::*;

    fn print_cbor_types(obj_name: &str, vec: Vec<u8>) {
        use cbor_event::Type;
        let mut raw = Deserializer::from(vec);
        println!("{} = {{", obj_name);
        loop {
            match raw.cbor_type() {
                Err(_) => break,
                Ok(Type::UnsignedInteger) => println!("UINT({})", raw.unsigned_integer().unwrap()),
                Ok(Type::NegativeInteger) => println!("NINT({})", raw.negative_integer().unwrap()),
                Ok(Type::Bytes) => println!("BYTES({:?})", raw.bytes().unwrap()),
                Ok(Type::Text) => println!("TEXT({})", raw.text().unwrap()),
                Ok(Type::Array) => println!("ARRAY({:?})", raw.array().unwrap()),
                Ok(Type::Map) => println!("MAP({:?})", raw.map().unwrap()),
                Ok(Type::Tag) => println!("TAG({})", raw.tag().unwrap()),
                Ok(Type::Special) => println!("SPECIAL({:?})", raw.special().unwrap()),
            }
        }
        println!("}}");
    }

    fn deser_test<T: Deserialize + ToBytes>(orig: T) {
        print_cbor_types("orig", orig.to_bytes());
        let deser = T::deserialize(&mut Deserializer::from(orig.to_bytes())).unwrap();
        print_cbor_types("deser", deser.to_bytes());
        assert_eq!(orig.to_bytes(), deser.to_bytes());
    }

    #[test]
    fn foo() {
        deser_test(Foo::new(436, String::from("jfkdsjfd"), vec![1, 1, 1]));
    }

    #[test]
    fn foo2_some() {
        deser_test(Foo2::new(
            143546,
            Some(TaggedText::new(String::from("afdjfkjsiefefe"))),
        ));
    }

    #[test]
    fn foo2_none() {
        deser_test(Foo2::new(143546, None));
    }

    #[test]
    fn bar() {
        deser_test(Bar::new(
            &Foo::new(436, String::from("jfkdf"), vec![6, 4]),
            None,
        ));
    }

    #[test]
    fn plain() {
        deser_test(Plain::new(
            7576,
            &TaggedText::new(String::from("wiorurri34h")),
        ));
    }

    #[test]
    fn outer() {
        deser_test(Outer::new(
            2143254,
            &Plain::new(7576, &TaggedText::new(String::from("wiorurri34h"))),
        ));
    }

    #[test]
    fn table() {
        let mut orig = Table::new();
        orig.insert(8, String::from("Eight"));
        orig.insert(16, String::from("Sixteen"));
        orig.insert(32, String::from("Thirty Two"));
        deser_test(orig);
    }

    #[test]
    fn table_arr_members() {
        let mut tab = Mapu64ToString::new();
        tab.insert(43266556, String::from("2k2j343"));
        tab.insert(213543254546565, String::from("!!fjdj"));
        let mut foos = Foos::new();
        foos.add(&Foo::new(0, String::from("Zero"), vec![]));
        foos.add(&Foo::new(2, String::from("Two"), vec![2, 2]));
        deser_test(TableArrMembers::new(&tab, vec![0, 1, 2, 3, 4, 5], &foos));
    }

    #[test]
    fn type_choice_0() {
        deser_test(TypeChoice::new_i0());
    }

    #[test]
    fn type_choice_hello_world() {
        deser_test(TypeChoice::new_helloworld());
    }

    #[test]
    fn type_choice_uint() {
        deser_test(TypeChoice::new_u64(53435364));
    }

    #[test]
    fn type_choice_text() {
        deser_test(TypeChoice::new_text(String::from(
            "jdfidsf83j3  jkrjefdfk !!",
        )));
    }

    #[test]
    fn type_choice_bytes() {
        deser_test(TypeChoice::new_bytes(vec![0x00, 0x01, 0xF7, 0xFF]));
    }

    #[test]
    fn type_choice_tagged_arr() {
        deser_test(TypeChoice::new_arr_u64(vec![1, 2, 3, 4]));
    }

    #[test]
    fn group_choice_foo() {
        deser_test(GroupChoice::new_foo(&Foo::new(0, String::new(), vec![])));
    }

    #[test]
    fn group_choice_0() {
        deser_test(GroupChoice::new_group_choice1(37));
    }

    #[test]
    fn group_choice_plain() {
        deser_test(GroupChoice::new_plain(&Plain::new(
            354545,
            &TaggedText::new(String::from("fdsfdsfdg")),
        )));
    }
}
