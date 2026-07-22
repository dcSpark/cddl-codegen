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
