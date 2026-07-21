// Behavioral floor for the built-in `int` crossing the wasm boundary under `--common-import-override`
// with a SPLIT rust/wasm dep whose crate is NOT a declared extern dependency. The wasm crate's mere
// compilation (the harness `cargo build`s it, routing `Int`'s wasm face through
// `extern_dep_crate_wasm::Int`) is the acceptance test for the relaxed `--extern-wasm-crate`
// validation; this rust-side test additionally proves the re-exported common `Int` round-trips as a
// struct field (both the `delta_coin` alias cell and the bare `int` cell).
#[cfg(test)]
mod tests {
    use super::*;
    // under --common-import-override the serialization traits live in the dep crate
    use extern_dep_crate::serialization::{Deserialize, ToCBORBytes};

    #[test]
    fn int_holder_roundtrips_through_common_import_int() {
        let holder = IntHolder::new(Int::new(-5), Int::new(9));
        let bytes = holder.to_cbor_bytes();
        let back = IntHolder::from_cbor_bytes(&bytes).expect("must deserialize its own bytes");
        assert_eq!(
            back.to_cbor_bytes(),
            bytes,
            "wire round-trip must be byte-identical"
        );
        assert_eq!(back.delta.to_string(), "-5");
        assert_eq!(back.plain.to_string(), "9");
    }
}
