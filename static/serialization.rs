// same as cbor_event::de::Deserialize but with our DeserializeError
pub trait Deserialize {
    fn deserialize(
        raw: &mut Deserializer,
    ) -> Result<Self, DeserializeError> where Self: Sized;

    // cbor_event's Deserializer owns its buffer, so this copies `data` once (O(n))
    fn from_cbor_bytes(data: &[u8]) -> Result<Self, DeserializeError> where Self: Sized {
        let mut raw = Deserializer::from(data.to_vec());
        let value = Self::deserialize(&mut raw)?;
        // Reject leftover bytes after a complete value instead of silently ignoring them: otherwise a
        // truncated/corrupt or accidentally-concatenated buffer would deserialize as Ok.
        // Hand-rolled rather than cbor_event's deserialize_complete(): that helper is bounded to
        // cbor_event's own Deserialize trait and error type, while this trait must surface the
        // annotated DeserializeError.
        if !raw.as_slice().is_empty() {
            return Err(DeserializeFailure::CBOR(cbor_event::Error::TrailingData).into());
        }
        Ok(value)
    }
}

impl<T: cbor_event::de::Deserialize> Deserialize for T {
    fn deserialize(raw: &mut Deserializer) -> Result<T, DeserializeError> {
        T::deserialize(raw).map_err(DeserializeError::from)
    }
}
