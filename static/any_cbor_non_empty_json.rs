// Appended to any_cbor.rs only when the same generated crate also contains non_empty.rs.
// The restricted open-array tail needs a natural-JSON `any` sequence adapter whose input type is
// NonEmptyVec rather than Vec; keep that cross-runtime reference out of the always-compiled base
// fragment so the independently-tested AnyCbor runtime stays self-contained.
pub mod natural_any_cbor_non_empty_seq {
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};
    use super::alloc::vec::Vec;
    use super::super::non_empty::NonEmptyVec;

    pub fn serialize<S>(value: &NonEmptyVec<AnyCbor>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(value.iter().map(NaturalAnyCborSer))
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<NonEmptyVec<AnyCbor>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let elems = <Vec<NaturalAnyCborDe> as serde::Deserialize>::deserialize(deserializer)?;
        NonEmptyVec::try_from(elems.into_iter().map(|e| e.0).collect::<Vec<_>>())
            .map_err(serde::de::Error::custom)
    }
}
