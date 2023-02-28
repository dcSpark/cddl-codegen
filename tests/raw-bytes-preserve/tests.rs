#[cfg(test)]
mod tests {
    use super::*;
    use cbor_event::{Sz, StringLenSz, de::Deserializer};
    use serialization::Deserialize;

    #[test]
    fn foo() {
        let str_32_encodings = vec![
            StringLenSz::Len(Sz::One),
            StringLenSz::Len(Sz::Eight),
            StringLenSz::Indefinite(vec![(4, Sz::One), (28, Sz::Four)])
        ];
        for str_enc in str_32_encodings.iter() {
            let irregular_bytes = vec![
                arr_sz(2, Sz::One),
                    cbor_int(0, Sz::Inline),
                        cbor_bytes_sz(vec![0xFF; 32], str_enc.clone())
            ].into_iter().flatten().clone().collect::<Vec<u8>>();
            let irregular = Foo::from_cbor_bytes(&irregular_bytes).unwrap();
            assert_eq!(irregular_bytes, irregular.to_cbor_bytes());
        }
    }

    #[test]
    #[should_panic]
    fn foo_too_big() {
        let irregular_bytes = vec![
            arr_sz(2, Sz::One),
                cbor_int(0, Sz::Inline),
                    cbor_bytes_sz(vec![0x00; 33], StringLenSz::Len(Sz::One),)
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let irregular = Foo::from_cbor_bytes(&irregular_bytes).unwrap();
    }

    #[test]
    #[should_panic]
    fn foo_too_small() {
        let irregular_bytes = vec![
            arr_sz(2, Sz::One),
                cbor_int(0, Sz::Inline),
                    cbor_bytes_sz(vec![0x00; 31], StringLenSz::Len(Sz::One),)
        ].into_iter().flatten().clone().collect::<Vec<u8>>();
        let irregular = Foo::from_cbor_bytes(&irregular_bytes).unwrap();
    }
}
