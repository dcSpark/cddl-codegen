// `AnyCbor` serialize impl for the preserve + NON-canonical assembly, where generated types
// implement `cbor_event::se::Serializer`'s `Serialize` (no `force_canonical` argument) and always
// replay stored encodings. Delegates to the mode-independent workhorse with `force_canonical =
// false`. (Paired with `any_cbor_preserve.rs`; the canonical assembly uses
// `any_cbor_preserve_force_canonical.rs` instead.)
impl cbor_event::se::Serialize for AnyCbor {
    fn serialize<'se>(
        &self,
        serializer: &'se mut cbor_event::se::Serializer,
    ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> {
        self.serialize_ref(serializer, false)
    }
}

/// Compare an arbitrary generated map-key type to a fixed key by CBOR value rather than its replay
/// encoding. In particular `0x01` and `0x1801` reach the same verdict. This assembly's generated
/// keys implement cbor-event's one-argument serialization trait.
pub fn cbor_value_eq_serialized<T: cbor_event::se::Serialize>(
    value: &T,
    expected: &AnyCbor,
) -> Result<bool, DeserializeError> {
    let mut serializer = cbor_event::se::Serializer::new_vec();
    cbor_event::se::Serialize::serialize(value, &mut serializer)?;
    <AnyCbor as Deserialize>::from_cbor_bytes(&serializer.finalize())
        .map(|found| found.value_eq(expected))
}
