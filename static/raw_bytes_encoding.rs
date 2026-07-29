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
        // The accepted grammar is BARE hex digits: a leading `0x`/`0X` is rejected as an invalid
        // character at index 1, which is what this surface has always answered and what
        // `to_raw_hex` (unprefixed, lowercase) round-trips against. The check is explicit because
        // the backing decoder strips such a prefix and accepts the rest; without it the read side
        // would quietly admit a spelling the write side never emits. A bare `"0x"` is the edge that
        // makes the length test load-bearing — stripped, it decodes to an empty byte string.
        let raw = hex_str.as_bytes();
        if raw.len() >= 2 && raw[0] == b'0' && (raw[1] == b'x' || raw[1] == b'X') {
            return Err(DeserializeFailure::InvalidStructure(Box::new(
                hex::FromHexError::InvalidHexCharacter {
                    c: raw[1] as char,
                    index: 1,
                },
            ))
            .into());
        }
        let bytes =
            hex::decode(hex_str).map_err(|e| DeserializeFailure::InvalidStructure(Box::new(e)))?;
        Self::from_raw_bytes(bytes.as_ref())
    }
}
