pub trait RawBytesEncoding {
    fn to_raw_bytes(&self) -> &[u8];

    fn from_raw_bytes(bytes: &[u8]) -> Result<Self, DeserializeError>
    where
        Self: Sized;

    fn to_raw_hex(&self) -> String {
        hex::encode(self.to_raw_bytes())
    }

    fn from_raw_hex(hex_str: &str) -> Result<Self, DeserializeError>
    where
        Self: Sized,
    {
        // The accepted grammar is CANONICAL hex — bare, even-length, lowercase — which is exactly
        // what `to_raw_hex` above emits, so the pair round-trips on the encoding and not merely on
        // the bytes. `decode_canonical_hex` (same composed module) owns that grammar for this
        // surface and for the emitted JSON bytes newtypes alike; its doc comment carries the
        // contract, including that the uppercase input this surface once normalized is now
        // rejected.
        let bytes = decode_canonical_hex(hex_str)
            .map_err(|e| DeserializeFailure::InvalidStructure(Box::new(e)))?;
        Self::from_raw_bytes(bytes.as_ref())
    }
}
