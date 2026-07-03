//! Adversarial CBOR bytes into generated deserializers (TESTING_ROADMAP "adversarial CBOR" item).
//!
//! Target: the preserve-encodings fixture's generated crate (`fuzz/generated/rust`, regenerate
//! with ./generate.sh) — the richest deserialization surface (encoding-preserving paths, nested
//! tables, type/group choices, bounds, defaults, custom serialization).
//!
//! Two oracles per input:
//!  1. no panic / abort / stack overflow — observed by the fuzz process boundary, which is the
//!     only oracle that can see stack overflow (catch_unwind cannot);
//!  2. preserve round-trip fidelity — any input from_cbor_bytes ACCEPTS must re-encode
//!     byte-identically (that is the --preserve-encodings contract), so the fuzzer doubles as the
//!     encode-fidelity oracle over minted irregular encodings.

#![no_main]

use libfuzzer_sys::fuzz_target;

macro_rules! probe {
    ($data:expr, $($t:ty),+ $(,)?) => {$(
        if let Ok(v) = <$t as cddl_lib::serialization::Deserialize>::from_cbor_bytes($data) {
            let reencoded = cddl_lib::serialization::ToCBORBytes::to_cbor_bytes(&v);
            assert_eq!(
                reencoded.as_slice(),
                $data,
                "preserve-encodings fidelity violated for {}",
                stringify!($t)
            );
        }
    )+};
}

// `probe_all` is DERIVED by fuzz/generate.sh from the generated crate's `impl Deserialize for <T>`
// set — so a new rule in the fuzzed spec is fuzzed with zero manual edits here (drift is impossible,
// not merely detected). generate.sh floors the extracted count so a rotted regex can't silently
// shrink the set. Included in item position (the well-supported use of include!).
include!(concat!(env!("CARGO_MANIFEST_DIR"), "/generated/probe_list.in"));

fuzz_target!(|data: &[u8]| {
    probe_all(data);
});
