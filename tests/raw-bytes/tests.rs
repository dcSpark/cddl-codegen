#[cfg(test)]
mod tests {
    use super::*;
    use crate::RawBytesEncoding;
    use cbor_event::de::Deserializer;
    use serialization::Deserialize;

    fn deser_test<T: Deserialize + ToCBORBytes>(orig: &T) {
        let orig_bytes = orig.to_cbor_bytes();
        print_cbor_types("orig", &orig_bytes);
        let mut deserializer = Deserializer::from(std::io::Cursor::new(orig_bytes.clone()));
        let deser = T::deserialize(&mut deserializer).unwrap();
        print_cbor_types("deser", &deser.to_cbor_bytes());
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
        assert_eq!(deserializer.as_ref().position(), orig_bytes.len() as u64);
    }

    #[test]
    fn foo() {
        deser_test(&Foo::new(PubKey([0; 32])));
    }

    #[test]
    fn raw_bytes_encoding_round_trip() {
        // PubKey wraps a fixed [u8; 32]; build a recognizable byte pattern.
        let mut bytes = [0u8; 32];
        for (i, b) in bytes.iter_mut().enumerate() {
            *b = i as u8;
        }
        let key = PubKey(bytes);
        // to_raw_hex is the trait default: hex::encode over to_raw_bytes (lowercase).
        let hex = key.to_raw_hex();
        assert_eq!(hex.len(), 64);
        assert_eq!(
            hex,
            "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        );
        // from_raw_hex is the trait default: hex::decode then from_raw_bytes.
        let decoded = PubKey::from_raw_hex(&hex).unwrap();
        assert_eq!(key.to_raw_bytes(), decoded.to_raw_bytes());
    }

    #[test]
    fn raw_bytes_encoding_reject() {
        // Non-hex characters: hex::decode fails -> InvalidStructure branch of from_raw_hex.
        assert!(PubKey::from_raw_hex("zz").is_err());
        // Odd-length hex string is likewise an invalid hex encoding.
        assert!(PubKey::from_raw_hex("abc").is_err());
        // Valid hex but wrong decoded length: from_raw_bytes try_into ([u8; 32]) fails.
        assert!(PubKey::from_raw_hex("00").is_err());
        assert!(PubKey::from_raw_bytes(&[0u8; 31]).is_err());
    }
}
