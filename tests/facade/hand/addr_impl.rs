// Hand-written definition of the `address` extern, copied to `src/addr_impl.rs` of the generated
// crate and re-exported at the crate root by the facade `lib.rs` (resolving the extern glue's
// `pub use crate::Address;`). Under --preserve-encodings=true the generated code still calls the
// extern through the plain `cbor_event::se::Serialize` / `crate::serialization::Deserialize` traits
// (the extern carries no encoding metadata, so preserve does not reshape these impls — the same
// shape the default-profile docs example uses).
use crate::error::DeserializeError;
use crate::serialization::Deserialize;

#[derive(Clone, Debug)]
pub struct Address(pub u64);

impl cbor_event::se::Serialize for Address {
    fn serialize<'se, W: std::io::Write>(
        &self,
        serializer: &'se mut cbor_event::se::Serializer<W>,
    ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer<W>> {
        serializer.write_unsigned_integer(self.0)
    }
}

impl Deserialize for Address {
    fn deserialize<R: std::io::BufRead + std::io::Seek>(
        raw: &mut cbor_event::de::Deserializer<R>,
    ) -> Result<Self, DeserializeError> {
        Ok(Self(raw.unsigned_integer()?))
    }
}
