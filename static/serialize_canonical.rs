pub trait Serialize {
    fn serialize<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
        force_canonical: bool,
    ) -> cbor_event::Result<&'a mut Serializer<W>>;
}

impl<T: cbor_event::se::Serialize> Serialize for T {
    fn serialize<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
        _force_canonical: bool,
    ) -> cbor_event::Result<&'a mut Serializer<W>> {
        <T as cbor_event::se::Serialize>::serialize(self, serializer)
    }
}

// we should probably just generate this directly at the top of serialization.rs
pub trait SerializeEmbeddedGroup {
    fn serialize_as_embedded_group<'a, W: Write + Sized>(
        &self,
        serializer: &'a mut Serializer<W>,
        force_canonical: bool,
    ) -> cbor_event::Result<&'a mut Serializer<W>>;
}


pub trait ToBytes {
    fn to_bytes(&self, force_canonical: bool) -> Vec<u8>;
}

impl<T: Serialize> ToBytes for T {
    fn to_bytes(&self, force_canonical: bool) -> Vec<u8> {
        let mut buf = Serializer::new_vec();
        self.serialize(&mut buf, force_canonical).unwrap();
        buf.finalize()
    }
}
