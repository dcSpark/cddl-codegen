// float JSON coverage (see input.cddl for why this is not part of tests/json/): serde round-trip
// plus the same serde-vs-schema agreement checks tests/json/tests.rs runs for its types.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn float_holder() {
        let holder = FloatHolder::new(1.5);
        let json = serde_json::to_string_pretty(&holder).unwrap();
        let from_json: FloatHolder = serde_json::from_str(&json).unwrap();
        assert_eq!(json, serde_json::to_string_pretty(&from_json).unwrap());
        assert_eq!(from_json.f, 1.5);
    }

    #[test]
    fn f64_json_round_trip_is_bit_exact() {
        // Adversarial f64 values whose shortest-decimal serialization, when re-parsed by
        // serde_json's DEFAULT (fast, lossy) f64 path, lands 1 ULP off the original bits.
        // Provenance: brute-forced xorshift bit patterns against serde_json 1.0.57 default
        // features (the exact version the generated manifest pins) — each round-trips to a
        // neighbouring bit pattern without `float_roundtrip`. Encoded as raw bits so the
        // literal is the exact value, independent of decimal-parse rounding in this file.
        for &bits in &[
            0x305f_050c_368d_cc74u64, // 1.0715660391465826e-75 (parses back 1 ULP low)
            0x8f8e_a9d3_4942_8d8eu64, // -9.643915712060552e-234 (parses back 1 ULP high)
            0x353c_fc38_7dfa_e6b8u64, // 3.0261999441573203e-52  (parses back 1 ULP high)
        ] {
            let x = f64::from_bits(bits);
            let json = serde_json::to_string(&FloatHolder::new(x)).unwrap();
            let back: FloatHolder = serde_json::from_str(&json).unwrap();
            assert_eq!(
                back.f.to_bits(),
                x.to_bits(),
                "f64 lost bits through json round-trip: {x:e} (0x{bits:016x}) -> {json} -> \
                 0x{:016x}; the generated serde_json dep needs the `float_roundtrip` feature",
                back.f.to_bits(),
            );
        }
    }

    #[test]
    fn schema_validates_serialization() {
        let schema = serde_json::to_value(schemars::schema_for!(FloatHolder)).unwrap();
        let validator = jsonschema::validator_for(&schema).unwrap();
        let value = serde_json::to_value(FloatHolder::new(1.5)).unwrap();
        let errors: Vec<String> = validator.iter_errors(&value).map(|e| e.to_string()).collect();
        assert!(
            errors.is_empty(),
            "FloatHolder: {value} does not validate against its own emitted schema: {errors:?}"
        );
        // negative half: an over-permissive schema must still reject a wrong-shaped field
        assert!(!validator.is_valid(&serde_json::json!({"f": "nope"})));
    }
}
