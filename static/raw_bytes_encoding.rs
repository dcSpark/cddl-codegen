/// `hex::FromHexError` re-wrapped so it can be boxed as
/// `DeserializeFailure::InvalidStructure(Box<dyn core::error::Error>)`.
///
/// `hex`'s own `Error` impl is `#[cfg(feature = "std")]` and the crate provides no
/// `core::error::Error` impl, so the raw error does not satisfy the boxed bound once the runtime
/// stops depending on `std`. This newtype supplies that impl locally — the same shape the emitted
/// deserializers already rely on for the crate's own `DeserializeError`.
///
/// Both `Debug` and `Display` are hand-written and delegate VERBATIM to the inner error rather than
/// being derived: `DeserializeFailure` derives `Debug` and its `Display` arm renders the boxed error
/// inline, so a derived `Debug` would insert a `FromHexErrorCore(..)` layer into every rendered
/// message. The wrapper is deliberately invisible in output.
///
/// `pub(crate)`: it is only ever constructed inside `from_raw_hex`'s default body and immediately
/// erased to `dyn core::error::Error`, so it never appears in a public signature.
pub(crate) struct FromHexErrorCore(hex::FromHexError);

impl core::fmt::Debug for FromHexErrorCore {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(&self.0, f)
    }
}

impl core::fmt::Display for FromHexErrorCore {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(&self.0, f)
    }
}

impl core::error::Error for FromHexErrorCore {}

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
            .map_err(|e| DeserializeFailure::InvalidStructure(Box::new(FromHexErrorCore(e))))?;
        Self::from_raw_bytes(bytes.as_ref())
    }
}
