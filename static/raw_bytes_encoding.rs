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
        let bytes = hex::decode(hex_str)
            .map_err(|e| DeserializeFailure::InvalidStructure(Box::new(e)))?;
        Self::from_raw_bytes(bytes.as_ref())
    }
}
