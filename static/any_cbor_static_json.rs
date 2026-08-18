// Natural JSON adapters for `[AnyCbor; N]`. Serde's generic array support keeps wide arrays
// available, while staging through Vec retains a single fallible cardinality handover.
pub mod natural_any_cbor_static_seq {
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};
    use super::alloc::vec::Vec;

    pub fn serialize<S, const N: usize>(
        value: &[AnyCbor; N],
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(value.iter().map(NaturalAnyCborSer))
    }

    pub fn deserialize<'de, D, const N: usize>(deserializer: D) -> Result<[AnyCbor; N], D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let elems = <Vec<NaturalAnyCborDe> as serde::Deserialize>::deserialize(deserializer)?;
        elems
            .into_iter()
            .map(|e| e.0)
            .collect::<Vec<_>>()
            .try_into()
            .map_err(|values: Vec<_>| serde::de::Error::custom(super::alloc::format!("array length {} does not equal {N}", values.len())))
    }
}

pub mod natural_any_cbor_opt_static_seq {
    use super::{AnyCbor, NaturalAnyCborDe, NaturalAnyCborSer};
    use super::alloc::vec::Vec;

    pub fn serialize<S, const N: usize>(
        value: &Option<[AnyCbor; N]>,
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

    pub fn deserialize<'de, D, const N: usize>(
        deserializer: D,
    ) -> Result<Option<[AnyCbor; N]>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        <Option<Vec<NaturalAnyCborDe>> as serde::Deserialize>::deserialize(deserializer)?
            .map(|elems| {
                elems
                    .into_iter()
                    .map(|e| e.0)
                    .collect::<Vec<_>>()
                    .try_into()
                    .map_err(|values: Vec<_>| serde::de::Error::custom(super::alloc::format!("array length {} does not equal {N}", values.len())))
            })
            .transpose()
    }
}
