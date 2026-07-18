pub trait SerializeEmbeddedGroup {
    fn serialize_as_embedded_group<'a>(
        &self,
        serializer: &'a mut Serializer,
    ) -> cbor_event::Result<&'a mut Serializer>;
}

pub trait ToCBORBytes {
    fn to_cbor_bytes(&self) -> Vec<u8>;
}

impl<T: cbor_event::se::Serialize> ToCBORBytes for T {
    fn to_cbor_bytes(&self) -> Vec<u8> {
        let mut buf = Serializer::new_vec();
        self.serialize(&mut buf).unwrap();
        buf.finalize()
    }
}
