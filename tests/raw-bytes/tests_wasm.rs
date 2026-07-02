// Wasm-boundary EXECUTION coverage for the raw-bytes fixture (see tests/core/tests_wasm.rs for the
// rationale). `wasm_matrix_compiles`' `rawbytes__*` cells prove the wasm-ABI shape type-checks; this
// executes the boundary on the host target: build `Foo` through the wasm API, round-trip
// `to_cbor_bytes` -> `from_cbor_bytes` byte-identically (differential vs the `cddl_lib::` rust twin),
// and read the `pub_key` accessor back. `PubKey` (a `RawBytesEncoding` wrapper) exposes NO wasm `new`,
// so it's built from the native type via `From<cddl_lib::PubKey>`; the native value itself is minted
// through the `RawBytesEncoding::from_raw_bytes` trait ctor (the tuple field is private cross-crate).
//
// `from_cbor_bytes` returns `Result<_, JsError>` and `JsError: !Debug`, hence `.ok().expect(..)`.

use cddl_lib::serialization::RawBytesEncoding;

#[test]
fn wasm_raw_bytes_roundtrip_and_accessor() {
    // A recognizable 32-byte pattern (PubKey wraps a fixed [u8; 32]).
    let mut bytes = [0u8; 32];
    for (i, b) in bytes.iter_mut().enumerate() {
        *b = i as u8;
    }

    // Build through the wasm API: native raw-bytes -> wasm PubKey (no `new`) -> Foo::new(&PubKey).
    let pk: PubKey = cddl_lib::PubKey::from_raw_bytes(&bytes)
        .expect("valid 32-byte key")
        .into();
    let foo = Foo::new(&pk);

    // Differential vs the rust twin: identical CBOR bytes for the same value.
    let native = cddl_lib::Foo::new(cddl_lib::PubKey::from_raw_bytes(&bytes).unwrap());
    assert_eq!(
        foo.to_cbor_bytes(),
        cddl_lib::serialization::ToCBORBytes::to_cbor_bytes(&native)
    );

    // Byte-identical round trip across the wasm boundary.
    let back = Foo::from_cbor_bytes(&foo.to_cbor_bytes())
        .ok()
        .expect("Foo round-trip");
    assert_eq!(back.to_cbor_bytes(), foo.to_cbor_bytes());

    // Accessor read-back: the pub_key bytes survive the wire.
    assert_eq!(back.pub_key().as_ref().to_raw_bytes(), &bytes[..]);
}
