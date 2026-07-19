#[cfg(test)]
mod extern_generic_scoped_tests {
    use crate::serialization::{Deserialize, RawBytesEncoding, ToCBORBytes};
    use cbor_event::de::Deserializer;

    fn deser_test<T: Deserialize + ToCBORBytes>(orig: &T) {
        let orig_bytes = orig.to_cbor_bytes();
        let mut deserializer = Deserializer::from(orig_bytes.clone());
        let deser = T::deserialize(&mut deserializer).unwrap();
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
        assert_eq!(deserializer.position(), orig_bytes.len());
    }

    // `signers` exercises the flavored alias `RequiredSigners = ExtSetRawBytes<PubKey>` (raw-bytes
    // element framing, base extern in the `crypto` scope); `things` exercises the plain alias
    // `MySet = ExtSet<Plain>` whose argument `Plain` is a ROOT-scope record referenced only as a
    // generic argument. Both aliases live in the `transaction` scope — the shape that aborted
    // generation before feature request 07's fix.
    #[test]
    fn tx_round_trip() {
        let signers: crate::generated::transaction::RequiredSigners =
            crate::ExtSetRawBytes::new(vec![
                crate::PubKey::from_raw_bytes(&[0u8; 32]).unwrap(),
                crate::PubKey::from_raw_bytes(&[7u8; 32]).unwrap(),
            ]);
        let things: crate::generated::transaction::MySet = crate::ExtSet::new(vec![
            crate::generated::Plain::new(3),
            crate::generated::Plain::new(9),
        ]);
        deser_test(&crate::generated::transaction::Tx::new(signers, things));
    }
}
