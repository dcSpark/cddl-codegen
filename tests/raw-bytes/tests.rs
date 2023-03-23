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
        let orig_bytes = orig.to_cbor_bytes();
        print_cbor_types("orig", orig_bytes);
        let mut deserializer = Deserializer::from(orig_bytes.clone());
        let deser = T::deserialize(&mut deserializer).unwrap();
        print_cbor_types("deser", deser.to_cbor_bytes());
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
        assert_eq!(deserializer.as_ref().position(), orig_bytes.len() as u64);
    }

    #[test]
    fn foo() {
        deser_test(&Foo::new(PubKey([0; 32])));
    }
}
