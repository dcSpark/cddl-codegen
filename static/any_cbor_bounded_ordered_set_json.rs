// Appended to any_cbor.rs only when ordered_set.rs is present. A bounded unique `any` array
// must walk values naturally, then pass the loose vector through BoundedOrderedSet's single door.
pub mod natural_any_cbor_bounded_ordered_set {
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};
    use super::alloc::vec::Vec;
    use super::super::ordered_set::BoundedOrderedSet;

    pub fn serialize<S, const MIN: u64, const MAX: u64>(
        value: &BoundedOrderedSet<AnyCbor, MIN, MAX>, serializer: S,
    ) -> Result<S::Ok, S::Error>
    where S: serde::Serializer {
        serializer.collect_seq(value.iter().map(NaturalAnyCborSer))
    }

    pub fn deserialize<'de, D, const MIN: u64, const MAX: u64>(
        deserializer: D,
    ) -> Result<BoundedOrderedSet<AnyCbor, MIN, MAX>, D::Error>
    where D: serde::Deserializer<'de> {
        let elems = <Vec<NaturalAnyCborDe> as serde::Deserialize>::deserialize(deserializer)?;
        BoundedOrderedSet::try_from(elems.into_iter().map(|e| e.0).collect::<Vec<_>>())
            .map_err(serde::de::Error::custom)
    }
}

pub mod natural_any_cbor_opt_bounded_ordered_set {
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};
    use super::alloc::vec::Vec;
    use super::super::ordered_set::BoundedOrderedSet;

    pub fn serialize<S, const MIN: u64, const MAX: u64>(
        value: &Option<BoundedOrderedSet<AnyCbor, MIN, MAX>>, serializer: S,
    ) -> Result<S::Ok, S::Error>
    where S: serde::Serializer {
        match value {
            Some(value) => serializer.collect_seq(value.iter().map(NaturalAnyCborSer)),
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, D, const MIN: u64, const MAX: u64>(
        deserializer: D,
    ) -> Result<Option<BoundedOrderedSet<AnyCbor, MIN, MAX>>, D::Error>
    where D: serde::Deserializer<'de> {
        let elems = <Option<Vec<NaturalAnyCborDe>> as serde::Deserialize>::deserialize(deserializer)?;
        elems.map(|elems| {
            BoundedOrderedSet::try_from(elems.into_iter().map(|e| e.0).collect::<Vec<_>>())
                .map_err(serde::de::Error::custom)
        }).transpose()
    }
}
