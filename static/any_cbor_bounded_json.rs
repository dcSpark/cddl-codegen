// Appended to any_cbor.rs only when bounded.rs is present. A bounded `any` array must keep both
// contracts: natural JSON walks each AnyCbor element through the fallible natural adapter, then the
// exact occurrence window is re-established through BoundedVec's one TryFrom<Vec<_>> door.
pub mod natural_any_cbor_bounded_seq {
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};
    use super::alloc::vec::Vec;
    use super::super::bounded::BoundedVec;

    pub fn serialize<S, const MIN: u64, const MAX: u64>(
        value: &BoundedVec<AnyCbor, MIN, MAX>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(value.iter().map(NaturalAnyCborSer))
    }

    pub fn deserialize<'de, D, const MIN: u64, const MAX: u64>(
        deserializer: D,
    ) -> Result<BoundedVec<AnyCbor, MIN, MAX>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let elems = <Vec<NaturalAnyCborDe> as serde::Deserialize>::deserialize(deserializer)?;
        BoundedVec::try_from(elems.into_iter().map(|e| e.0).collect::<Vec<_>>())
            .map_err(serde::de::Error::custom)
    }
}

pub mod natural_any_cbor_opt_bounded_seq {
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};
    use super::alloc::vec::Vec;
    use super::super::bounded::BoundedVec;

    pub fn serialize<S, const MIN: u64, const MAX: u64>(
        value: &Option<BoundedVec<AnyCbor, MIN, MAX>>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match value {
            Some(value) => serializer.collect_seq(value.iter().map(NaturalAnyCborSer)),
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, D, const MIN: u64, const MAX: u64>(
        deserializer: D,
    ) -> Result<Option<BoundedVec<AnyCbor, MIN, MAX>>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let elems = <Option<Vec<NaturalAnyCborDe>> as serde::Deserialize>::deserialize(deserializer)?;
        elems
            .map(|elems| {
                BoundedVec::try_from(elems.into_iter().map(|e| e.0).collect::<Vec<_>>())
                    .map_err(serde::de::Error::custom)
            })
            .transpose()
    }
}
