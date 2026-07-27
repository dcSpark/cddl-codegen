// The wasm crate's half of the feature-liveness pin. `roundtrip.mjs` asserts what the JS side sees,
// but it would pass VACUOUSLY if `serde_json/arbitrary_precision` never reached the wasm crate's
// dependency graph — and that graph is a separate cargo resolve from the rust crate's, reached only
// because the harness copies the `arbitrary-precision-crate` path dep into BOTH manifests. So assert
// it here, in the crate wasm-pack actually builds, using the defect itself as the detector: a bare
// `serde_json::Value` holding a number reaches a non-serde_json serializer as a MAP exactly when the
// feature is on.
#[cfg(test)]
mod wasm_arbitrary_precision_feature {
    #[test]
    fn arbitrary_precision_is_live_in_the_wasm_crate() {
        let mut bytes = Vec::new();
        ciborium::into_writer(&serde_json::Value::from(1000u64), &mut bytes).unwrap();
        let probe: ciborium::Value = ciborium::from_reader(&bytes[..]).unwrap();
        assert!(
            matches!(probe, ciborium::Value::Map(_)),
            "serde_json/arbitrary_precision is NOT enabled in the WASM crate's build graph, so \
             roundtrip.mjs is testing nothing — check that the `arbitrary-precision-crate` path \
             dependency is still copied into the wasm Cargo.toml (got {probe:?})"
        );
    }
}
