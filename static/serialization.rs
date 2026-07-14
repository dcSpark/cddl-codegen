// same as cbor_event::de::Deserialize but with our DeserializeError
pub trait Deserialize {
    fn deserialize<R: BufRead + Seek>(
        raw: &mut Deserializer<R>,
    ) -> Result<Self, DeserializeError> where Self: Sized;

    fn from_cbor_bytes(data: &[u8]) -> Result<Self, DeserializeError> where Self: Sized {
        let mut raw = Deserializer::from(std::io::Cursor::new(data));
        let value = Self::deserialize(&mut raw)?;
        // Reject leftover bytes after a complete value instead of silently ignoring them: otherwise a
        // truncated/corrupt or accidentally-concatenated buffer would deserialize as Ok.
        if raw.as_ref().position() != data.len() as u64 {
            return Err(DeserializeFailure::CBOR(cbor_event::Error::TrailingData).into());
        }
        Ok(value)
    }
}

impl<T: cbor_event::de::Deserialize> Deserialize for T {
    fn deserialize<R: BufRead + Seek>(raw: &mut Deserializer<R>) -> Result<T, DeserializeError> {
        T::deserialize(raw).map_err(DeserializeError::from)
    }
}
