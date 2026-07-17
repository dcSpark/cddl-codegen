#[cfg(test)]
mod tests {
    use super::*;
    use crate::serialization::{Deserialize, RawBytesEncoding};
    use cbor_event::de::Deserializer;

    fn deser_test<T: Deserialize + ToCBORBytes>(orig: &T) {
        let orig_bytes = orig.to_cbor_bytes();
        let mut deserializer = Deserializer::from(std::io::Cursor::new(orig_bytes.clone()));
        let deser = T::deserialize(&mut deserializer).unwrap();
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
        assert_eq!(deserializer.as_ref().position(), orig_bytes.len() as u64);
    }

    #[test]
    fn using_flavored_round_trip() {
        // `keys` exercises the flavored alias `ExtSetPubKey = ExtSetRawBytes<PubKey>` (raw-bytes
        // element framing); `plains` exercises the plain alias `ExtSetPlain = ExtSet<Plain>`
        // (trait-bound element serialization). Both co-emit from one struct — the feature's point.
        let keys: ExtSetPubKey = ExtSetRawBytes::new(vec![
            PubKey::from_raw_bytes(&[0u8; 32]).unwrap(),
            PubKey::from_raw_bytes(&[7u8; 32]).unwrap(),
        ]);
        let plains: ExtSetPlain = ExtSet::new(vec![
            Plain::new(3, "hi".to_string()),
            Plain::new(9, "yo".to_string()),
        ]);
        deser_test(&UsingFlavored::new(keys, plains));
    }
}
