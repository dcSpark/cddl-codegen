#[cfg(test)]
mod tests {
    use super::*;
    use cbor_event::de::Deserializer;
    use serialization::Deserialize;
    use std::{print, println};

    fn print_cbor_types(obj_name: &str, vec: Vec<u8>) {
        use cbor_event::Type;
        let mut raw = cbor_event::de::Deserializer::from(vec);
        let mut lens = Vec::new();
        let consume_elem = |lens: &mut Vec<cbor_event::LenSz>| {
            if let Some(len) = lens.last_mut() {
                if let cbor_event::LenSz::Len(n, _) = len {
                    *n -= 1;
                }
            }
        };
        let reduce_depth = |lens: &mut Vec<cbor_event::LenSz>| {
            while let Some(cbor_event::LenSz::Len(0, _)) = lens.last() {
                lens.pop();
                println!("{}}}", "\t".repeat(lens.len()));
            }
        };
        println!("{} = {{", obj_name);
        loop {
            print!("{}", "\t".repeat(lens.len()));
            match raw.cbor_type() {
                Err(_) => break,
                Ok(Type::UnsignedInteger) => {
                    let (x, sz) = raw.unsigned_integer_sz().unwrap();
                    println!("UINT({}, {:?})", x, sz);
                    consume_elem(&mut lens);
                    reduce_depth(&mut lens);
                }
                Ok(Type::NegativeInteger) => {
                    let (x, sz) = raw.negative_integer_sz().unwrap();
                    println!("NINT({}, {:?})", x, sz);
                    consume_elem(&mut lens);
                    reduce_depth(&mut lens);
                }
                Ok(Type::Bytes) => {
                    let (x, sz) = raw.bytes_sz().unwrap();
                    println!("BYTES({:?}, {:?})", x, sz);
                    consume_elem(&mut lens);
                    reduce_depth(&mut lens);
                }
                Ok(Type::Text) => {
                    let (x, sz) = raw.text_sz().unwrap();
                    println!("TEXT(\"{}\", {:?})", x, sz);
                    consume_elem(&mut lens);
                    reduce_depth(&mut lens);
                }
                Ok(Type::Array) => {
                    let len = raw.array_sz().unwrap();
                    println!("ARRAY({:?}) {{", len);
                    consume_elem(&mut lens);
                    lens.push(len);
                    if let cbor_event::LenSz::Len(0, _sz) = len {
                        reduce_depth(&mut lens);
                    }
                }
                Ok(Type::Map) => {
                    let len = raw.map_sz().unwrap();
                    println!("MAP({:?}) {{", len);
                    consume_elem(&mut lens);
                    lens.push(match len {
                        cbor_event::LenSz::Len(n, sz) => cbor_event::LenSz::Len(2 * n, sz),
                        cbor_event::LenSz::Indefinite => cbor_event::LenSz::Indefinite,
                    });
                    if let cbor_event::LenSz::Len(0, _sz) = len {
                        reduce_depth(&mut lens);
                    }
                }
                Ok(Type::Tag) => {
                    let (tag, sz) = raw.tag_sz().unwrap();
                    println!("TAG({}, {:?})", tag, sz);
                }
                Ok(Type::Special) => {
                    let special = raw.special().unwrap();
                    println!("SPECIAL({:?})", special);
                    if special == cbor_event::Special::Break {
                        if let Some(cbor_event::LenSz::Indefinite) = lens.last() {
                            lens.pop();
                            reduce_depth(&mut lens);
                        } else {
                            panic!("unexpected break");
                        }
                    } else {
                        consume_elem(&mut lens);
                        reduce_depth(&mut lens);
                    }
                }
            }
        }
        println!("}}");
    }

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
