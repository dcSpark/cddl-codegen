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
    fn foo() {
        deser_test(&Foo::new(436, String::from("jfkdsjfd"), vec![1, 1, 1]));
    }

    #[test]
    fn foo2_some() {
        deser_test(&Foo2::new(143546, Some(TaggedText::new(String::from("afdjfkjsiefefe")))));
    }

    #[test]
    fn foo2_none() {
        deser_test(&Foo2::new(143546, None));
    }

    #[test]
    fn bar() {
        deser_test(&Bar::new(&Foo::new(436, String::from("jfkdf"), vec![6, 4]), None));
    }

    #[test]
    fn plain() {
        deser_test(&Plain::new(7576, &TaggedText::new(String::from("wiorurri34h"))));
    }

    #[test]
    fn outer() {
        deser_test(&Outer::new(2143254, &Plain::new(7576, &TaggedText::new(String::from("wiorurri34h")))));
    }

    #[test]
    fn table() {
        let mut orig = Table::new();
        orig.insert(8, String::from("Eight"));
        orig.insert(16, String::from("Sixteen"));
        orig.insert(32, String::from("Thirty Two"));
        deser_test(&orig);
    }

    #[test]
    fn table_arr_members() {
        let mut tab = MapStringToString::new();
        tab.insert(String::from("43266556"), String::from("2k2j343"));
        tab.insert(String::from("213543254546565"), String::from("!!fjdj"));
        let mut foos = Foos::new();
        foos.add(&Foo::new(0, String::from("Zero"), vec![]));
        foos.add(&Foo::new(2, String::from("Two"), vec![2, 2]));
        let mut u64s = U64s::new();
        u64s.add(0);
        u64s.add(1);
        u64s.add(2);
        u64s.add(3);
        u64s.add(4);
        u64s.add(6);
        deser_test(&TableArrMembers::new(&tab, &u64s, &foos));
    }

    #[test]
    fn type_choice_0() {
        deser_test(&TypeChoice::new_i0());
    }

    #[test]
    fn type_choice_hello_world() {
        deser_test(&TypeChoice::new_helloworld());
    }
    
    #[test]
    fn type_choice_uint() {
        deser_test(&TypeChoice::new_u64(53435364));
    }

    #[test]
    fn type_choice_text() {
        deser_test(&TypeChoice::new_text(String::from("jdfidsf83j3  jkrjefdfk !!")));
    }

    #[test]
    fn type_choice_bytes() {
        deser_test(&TypeChoice::new_bytes(vec![0x00, 0x01, 0xF7, 0xFF]));
    }

    #[test]
    fn type_choice_tagged_arr() {
	let mut u64s = U64s::new();
        u64s.add(1);
        u64s.add(2);
        u64s.add(3);
        u64s.add(4);
        deser_test(&TypeChoice::new_arr_u64(&u64s));
    }

    #[test]
    fn group_choice_foo() {
        deser_test(&GroupChoice::new_foo(0, String::new(), vec![]));
    }

    #[test]
    fn group_choice_0() {
        deser_test(&GroupChoice::new_group_choice1(37));
    }

    #[test]
    fn group_choice_plain() {
        deser_test(&GroupChoice::new_plain(354545, &TaggedText::new(String::from("fdsfdsfdg"))));
    }
}
