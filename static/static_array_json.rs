// Generic serde adapters for `[T; N]`. The pinned serde implements array traits only through 32,
// while generated exact CDDL arrays may be wider. JSON stays an ordinary sequence and this is the
// sole loose-Vec -> static-array cardinality handover.
pub mod static_array {
    use super::alloc::vec::Vec;

    pub fn serialize<S, T, const N: usize>(value: &[T; N], serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
        T: serde::Serialize,
    {
        serializer.collect_seq(value)
    }

    pub fn deserialize<'de, D, T, const N: usize>(deserializer: D) -> Result<[T; N], D::Error>
    where
        D: serde::Deserializer<'de>,
        T: serde::Deserialize<'de>,
    {
        <Vec<T> as serde::Deserialize>::deserialize(deserializer)?
            .try_into()
            .map_err(|elements: Vec<_>| {
                serde::de::Error::custom(super::alloc::format!("array length {} does not equal {N}", elements.len()))
            })
    }
}

pub mod static_array_opt {
    use super::alloc::vec::Vec;

    pub fn serialize<S, T, const N: usize>(
        value: &Option<[T; N]>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
        T: serde::Serialize,
    {
        match value {
            Some(value) => serializer.collect_seq(value),
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, D, T, const N: usize>(
        deserializer: D,
    ) -> Result<Option<[T; N]>, D::Error>
    where
        D: serde::Deserializer<'de>,
        T: serde::Deserialize<'de>,
    {
        <Option<Vec<T>> as serde::Deserialize>::deserialize(deserializer)?
            .map(|elements| {
                elements.try_into().map_err(|elements: Vec<_>| {
                    serde::de::Error::custom(super::alloc::format!(
                        "array length {} does not equal {N}",
                        elements.len()
                    ))
                })
            })
            .transpose()
    }
}

// A collection whose ELEMENT is a static array also cannot rely on serde's array trait: deriving
// `Serialize` for `Vec<[T; N]>` asks for `Serialize` on the wide inner array. Keep its JSON shape
// as a list of lists and do the checked handover independently for every element.
pub mod static_array_seq {
    use super::alloc::vec::Vec;

    pub fn serialize<S, T, const N: usize>(
        value: &Vec<[T; N]>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
        T: serde::Serialize,
    {
        serializer.collect_seq(value.iter().map(|array| array.as_slice()))
    }

    pub fn deserialize<'de, D, T, const N: usize>(deserializer: D) -> Result<Vec<[T; N]>, D::Error>
    where
        D: serde::Deserializer<'de>,
        T: serde::Deserialize<'de>,
    {
        <Vec<Vec<T>> as serde::Deserialize>::deserialize(deserializer)?
            .into_iter()
            .map(|elements| {
                elements.try_into().map_err(|elements: Vec<_>| {
                    serde::de::Error::custom(super::alloc::format!(
                        "array length {} does not equal {N}",
                        elements.len()
                    ))
                })
            })
            .collect()
    }
}
