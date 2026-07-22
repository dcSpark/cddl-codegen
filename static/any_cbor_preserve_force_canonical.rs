// `AnyCbor` serialize impl for the preserve + canonical assembly, where generated types implement
// the crate-local `Serialize` trait (the one taking `force_canonical: bool`). Delegates to the
// mode-independent workhorse, threading `force_canonical` through so `to_canonical_cbor_bytes`
// (the local trait's default method) produces deterministic encoding. (Paired with
// `any_cbor_preserve.rs`; the non-canonical assembly uses
// `any_cbor_preserve_non_force_canonical.rs` instead.)
impl Serialize for AnyCbor {
    fn serialize<'a>(
        &self,
        serializer: &'a mut cbor_event::se::Serializer,
        force_canonical: bool,
    ) -> cbor_event::Result<&'a mut cbor_event::se::Serializer> {
        self.serialize_ref(serializer, force_canonical)
    }
}
