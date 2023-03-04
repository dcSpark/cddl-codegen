#[cfg(test)]
mod tests {
    use super::*;
    use cbor_event::de::Deserializer;
    use serialization::Deserialize;

    fn deser_test<T: Deserialize + ToCBORBytes>(orig: &T) {
        print_cbor_types("orig", orig.to_cbor_bytes());
        let deser = T::deserialize(&mut Deserializer::from(orig.to_cbor_bytes())).unwrap();
        print_cbor_types("deser", deser.to_cbor_bytes());
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
    }

    #[test]
    fn foo() {
        deser_test(&Foo::new(436, String::from("jfkdsjfd"), vec![1, 1, 1]));
    }

    #[test]
    fn foo2_some() {
        deser_test(&Foo2::new(143546, Some(String::from("afdjfkjsiefefe"))));
    }

    #[test]
    fn foo2_none() {
        deser_test(&Foo2::new(143546, None));
    }

    #[test]
    fn bar() {
        deser_test(&Bar::new(
            Foo::new(436, String::from("jfkdf"), vec![6, 4]),
            None,
        ));
    }

    #[test]
    fn plain() {
        deser_test(&Plain::new(7576, String::from("wiorurri34h")));
    }

    #[test]
    fn outer() {
        deser_test(&Outer::new(
            2143254,
            Plain::new(7576, String::from("wiorurri34h")),
        ));
    }

    #[test]
    fn type_choice_0() {
        deser_test(&TypeChoice::I0);
    }

    #[test]
    fn type_choice_hello_world() {
        deser_test(&TypeChoice::Helloworld);
    }

    #[test]
    fn type_choice_uint() {
        deser_test(&TypeChoice::U64(53435364));
    }

    #[test]
    fn type_choice_text() {
        deser_test(&TypeChoice::Text(String::from("jdfidsf83j3  jkrjefdfk !!")));
    }

    #[test]
    fn type_choice_bytes() {
        deser_test(&TypeChoice::Bytes(vec![0x00, 0x01, 0xF7, 0xFF]));
    }

    #[test]
    fn type_choice_tagged_u64() {
        deser_test(&TypeChoice::U64(5));
    }

    #[test]
    fn group_choice_foo() {
        deser_test(&GroupChoice::Foo(Foo::new(0, String::new(), vec![])));
    }

    #[test]
    fn group_choice_0() {
        deser_test(&GroupChoice::GroupChoice1(37));
    }

    #[test]
    fn group_choice_plain() {
        deser_test(&GroupChoice::Plain(Plain::new(
            354545,
            String::from("fdsfdsfdg"),
        )));
    }
}
