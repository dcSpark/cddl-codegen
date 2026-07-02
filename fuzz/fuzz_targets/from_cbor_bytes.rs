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

fuzz_target!(|data: &[u8]| {
    probe!(
        data,
        cddl_lib::Foo,
        cddl_lib::Bar,
        cddl_lib::TableArrMembers,
        cddl_lib::Enums,
        cddl_lib::DeeplyNested,
        cddl_lib::String64,
        cddl_lib::String1632,
        cddl_lib::TypeChoice,
        cddl_lib::NonOverlappingTypeChoiceAll,
        cddl_lib::GroupChoice,
        cddl_lib::PlainArrays,
        cddl_lib::CborInCbor,
        cddl_lib::SignedInts,
        cddl_lib::MapWithDefaults,
        cddl_lib::ArrayOptFields,
        cddl_lib::Bounds,
        cddl_lib::BoundsGroupChoice,
        cddl_lib::OverlappingInlined,
        cddl_lib::EnumOptEmbedFields,
        cddl_lib::StructWithCustomSerialization,
        cddl_lib::WrapperTable,
        cddl_lib::WrapperList,
    );
});
