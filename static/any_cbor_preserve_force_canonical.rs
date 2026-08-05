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

// A preserve + canonical shared runtime also serves a reduced (non-canonical) consumer, whose
// generated code calls cbor_event's one-argument trait. Replay stored encodings there; the local
// two-argument impl above remains the canonical caller's dispatch point.
impl cbor_event::se::Serialize for AnyCbor {
    fn serialize<'se>(
        &self,
        serializer: &'se mut cbor_event::se::Serializer,
    ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer> {
        self.serialize_ref(serializer, false)
    }
}
