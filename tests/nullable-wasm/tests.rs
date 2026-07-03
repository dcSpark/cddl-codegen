// Rust-side oracle for the wasm three-state fidelity fixture. The native types keep all three
// states (absent / present-null / present-value) unconditionally — that is WHY the wasm flatten is a
// pure read-side loss and CBOR round-trips are unaffected. These tests pin that: each state is
// representable natively, the three are pairwise distinct, and each survives a CBOR round trip. The
// wasm-accessor side (that `has_*`/presence restores the distinction across the boundary) is asserted
// in tests_wasm.rs.
#[cfg(test)]
mod nullable_wasm_tests {
    use super::*;
    use cbor_event::de::Deserializer;
    use serialization::Deserialize;

    fn cbor_round_trip<T: Deserialize + ToCBORBytes>(orig: &T) {
        let orig_bytes = orig.to_cbor_bytes();
        let mut deserializer = Deserializer::from(std::io::Cursor::new(orig_bytes.clone()));
        let deser = T::deserialize(&mut deserializer).unwrap();
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
        assert_eq!(deserializer.as_ref().position(), orig_bytes.len() as u64);
    }

    #[test]
    fn optional_nullable_field_three_states_native() {
        // absent: outer None
        let absent = NullableOptionalField::new(1);
        assert_eq!(absent.field0, None);
        // present-null: Some(None)
        let mut present_null = NullableOptionalField::new(1);
        present_null.field0 = Some(None);
        // present-value: Some(Some(v))
        let mut present_value = NullableOptionalField::new(1);
        present_value.field0 = Some(Some(7));

        // pairwise distinct at the native level
        assert_ne!(absent.field0, present_null.field0);
        assert_ne!(absent.field0, present_value.field0);
        assert_ne!(present_null.field0, present_value.field0);

        // and each survives a CBOR round trip
        cbor_round_trip(&absent);
        cbor_round_trip(&present_null);
        cbor_round_trip(&present_value);
    }

    #[test]
    fn nullable_map_value_three_states_native() {
        let mut map = NullableMapValue::new();
        map.insert(10, None); // present-null
        map.insert(20, Some(7)); // present-value
                                 // key 30 absent

        assert_eq!(map.get(&30), None); // absent
        assert_eq!(map.get(&10), Some(&None)); // present-null
        assert_eq!(map.get(&20), Some(&Some(7))); // present-value
    }

    #[test]
    fn nullable_enum_variant_three_states_native() {
        // A different variant, this-variant-inner-null, and this-variant-with-value are all distinct.
        let other = NullableEnum::new_text("x".to_string());
        let inner_null = NullableEnum::new_opt_u64(None);
        let value = NullableEnum::new_opt_u64(Some(7));
        cbor_round_trip(&other);
        cbor_round_trip(&inner_null);
        cbor_round_trip(&value);
        // inner-null vs value serialize differently (null vs the uint)
        assert_ne!(inner_null.to_cbor_bytes(), value.to_cbor_bytes());
    }
}
