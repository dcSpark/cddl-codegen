pub trait SerializeEmbeddedGroup {
    fn serialize_as_embedded_group<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
    ) -> cbor_event::Result<&'a mut Serializer<W>>;
}

pub trait ToBytes {
  fn to_bytes(&self) -> Vec<u8>;
}

impl<T: cbor_event::se::Serialize> ToBytes for T {
  fn to_bytes(&self) -> Vec<u8> {
      let mut buf = Serializer::new_vec();
      self.serialize(&mut buf).unwrap();
      buf.finalize()
  }
}