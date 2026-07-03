//! Adversarial CBOR bytes into a RECURSIVE generated crate's deserializers, built WITH
//! `--deserialize-depth-limit` (see fuzz/generate.sh).
//!
//! Target: the crate generated from `tests/corpus/recursive.cddl` (`fuzz/generated/recursive/rust`,
//! renamed `recursive_lib` in Cargo.toml to avoid clashing with the preserve crate's `cddl-lib`
//! package). Its recursive-descent deserializer would overflow the stack (SIGABRT) on hostile-deep
//! input; the depth limit turns that into a graceful `DeserializeFailure::DepthLimitExceeded`.
//!
//! Two oracles per input (identical to `from_cbor_bytes`):
//!  1. no panic / abort / stack overflow — observed by the fuzz process boundary, which is the only
//!     oracle that can see a stack overflow. This is precisely the oracle the depth limit exists to
//!     satisfy: the hostile-deep seed vector (from
//!     integration_tests::deserialize_depth_limit_guards_recursion) must return
//!     `DepthLimitExceeded` rather than abort, and this boundary is what proves it never aborts;
//!  2. preserve round-trip fidelity — any input `from_cbor_bytes` ACCEPTS must re-encode
//!     byte-identically (the `--preserve-encodings` contract).

#![no_main]

use libfuzzer_sys::fuzz_target;

macro_rules! probe {
    ($data:expr, $($t:ty),+ $(,)?) => {$(
        if let Ok(v) = <$t as recursive_lib::serialization::Deserialize>::from_cbor_bytes($data) {
            let reencoded = recursive_lib::serialization::ToCBORBytes::to_cbor_bytes(&v);
            assert_eq!(
                reencoded.as_slice(),
                $data,
                "preserve-encodings fidelity violated for {}",
                stringify!($t)
            );
        }
    )+};
}

// Derived `probe_all` — see the sibling from_cbor_bytes.rs note and fuzz/generate.sh.
include!(concat!(env!("CARGO_MANIFEST_DIR"), "/generated/recursive_probe_list.in"));

fuzz_target!(|data: &[u8]| {
    probe_all(data);
});
