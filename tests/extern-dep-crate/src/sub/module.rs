pub type UintTypedef = u64;

// A second extern type (declared `_CDDL_CODEGEN_EXTERN_TYPE_` in the dep spec's `sub/module.cddl`),
// used by the consumer's `nested` module as a wrapper element and map key/value. Serializes as a bare
// uint. Derives Ord/Hash so it can be an `OrderedHashMap` key (the extern-map-KEY cell). Mirrors
// `ExternCrateFoo`: `cbor_event::se::Serialize` (→ the crate's `ToCBORBytes` blanket) plus the crate's
// own `serialization::Deserialize` implemented directly (the blanket `Deserialize` impl only covers
// types implementing cbor_event's `Deserialize`, whose `deserialize` has no `Seek` bound).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExternCrateBar {
    inner: u64,
}

impl ExternCrateBar {
    pub fn new(inner: u64) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> u64 {
        self.inner
    }
}

impl cbor_event::se::Serialize for ExternCrateBar {
    fn serialize<'se>(
        &self,
        serializer: &'se mut cbor_event::se::Serializer,
    ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> {
        serializer.write_unsigned_integer(self.inner)
    }
}

impl crate::serialization::Deserialize for ExternCrateBar {
    fn deserialize(
        raw: &mut cbor_event::de::Deserializer,
    ) -> Result<Self, crate::error::DeserializeError> {
        Ok(Self {
            inner: raw.unsigned_integer()?,
        })
    }
}
