#[cfg(test)]
mod tests {
    use super::*;
    use crate::serialization::{Deserialize, ToCBORBytes};
    use cbor_event::de::Deserializer;

    // Round-trips `big_thing = [e: my_extern, s: my_set]`, where `my_set = ext_set<uint>` is the
    // concrete generic-extern instance `ExtSet<u64>`. The json-gen crate (built + run by the
    // harness) is what proves the schema rows compile: the generic BASE row is skipped, while the
    // plain extern (`MyExtern`) and the instance (`MySet`) rows compile against the hand-written
    // `schemars::JsonSchema` impls.
    #[test]
    fn big_thing_round_trip() {
        let orig = BigThing::new(MyExtern::new(42), ExtSet::new(vec![1u64, 2, 3]));
        let bytes = orig.to_cbor_bytes();
        let deser = BigThing::deserialize(&mut Deserializer::from(bytes.clone())).unwrap();
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
    }
}
