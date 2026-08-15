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

/// Compare an arbitrary generated map-key type to a fixed key by CBOR value rather than its replay
/// encoding. In the canonical preserve assembly generated keys implement this crate's local
/// two-argument `Serialize` trait, not cbor-event's trait. Passing `false` deliberately replays
/// their stored encoding before `AnyCbor::value_eq` erases representation detail.
pub fn cbor_value_eq_serialized<T: Serialize>(
    value: &T,
    expected: &AnyCbor,
) -> Result<bool, DeserializeError> {
    let mut serializer = cbor_event::se::Serializer::new_vec();
    Serialize::serialize(value, &mut serializer, false)?;
    <AnyCbor as Deserialize>::from_cbor_bytes(&serializer.finalize())
        .map(|found| found.value_eq(expected))
}
