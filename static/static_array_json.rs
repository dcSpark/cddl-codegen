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

// Recursive exact-array JSON support. The direct adapters above retain their established emitted
// spelling; this descriptor family owns only a static array whose element is another static array
// (at any depth), the shape for which serde would otherwise request a trait on a wide native array.
pub trait RecursiveSerialize<T> {
    fn serialize<S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer;
}

pub trait RecursiveDeserialize<T> {
    fn deserialize<'de, D>(deserializer: D) -> Result<T, D::Error>
    where
        D: serde::Deserializer<'de>;
}

struct SerializeAs<'a, Shape, T>(&'a T, core::marker::PhantomData<Shape>);

impl<Shape, T> serde::Serialize for SerializeAs<'_, Shape, T>
where
    Shape: RecursiveSerialize<T>,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Shape::serialize(self.0, serializer)
    }
}

struct DeserializeAs<Shape, T>(T, core::marker::PhantomData<Shape>);

impl<'de, Shape, T> serde::Deserialize<'de> for DeserializeAs<Shape, T>
where
    Shape: RecursiveDeserialize<T>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Shape::deserialize(deserializer).map(|value| Self(value, core::marker::PhantomData))
    }
}

impl<T: serde::Serialize> RecursiveSerialize<T> for Leaf {
    fn serialize<S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        value.serialize(serializer)
    }
}

impl<T> RecursiveDeserialize<T> for Leaf
where
    for<'de> T: serde::Deserialize<'de>,
{
    fn deserialize<'de, D>(deserializer: D) -> Result<T, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        T::deserialize(deserializer)
    }
}

impl<Inner, T, const N: usize> RecursiveSerialize<[T; N]> for Exact<Inner, N>
where
    Inner: RecursiveSerialize<T>,
{
    fn serialize<S>(value: &[T; N], serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(value.iter().map(|value| {
            SerializeAs::<Inner, T>(value, core::marker::PhantomData)
        }))
    }
}

impl<Inner, T, const N: usize> RecursiveDeserialize<[T; N]> for Exact<Inner, N>
where
    Inner: RecursiveDeserialize<T>,
{
    fn deserialize<'de, D>(deserializer: D) -> Result<[T; N], D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let elements = <alloc::vec::Vec<DeserializeAs<Inner, T>> as serde::Deserialize>::deserialize(deserializer)?
            .into_iter()
            .map(|element| element.0)
            .collect::<alloc::vec::Vec<_>>();
        elements.try_into().map_err(|elements: alloc::vec::Vec<_>| {
            serde::de::Error::custom(alloc::format!(
                "array length {} does not equal {N}",
                elements.len()
            ))
        })
    }
}

impl<Inner, T> RecursiveSerialize<alloc::vec::Vec<T>> for Loose<Inner>
where
    Inner: RecursiveSerialize<T>,
{
    fn serialize<S>(value: &alloc::vec::Vec<T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(value.iter().map(|value| {
            SerializeAs::<Inner, T>(value, core::marker::PhantomData)
        }))
    }
}

impl<Inner, T> RecursiveDeserialize<alloc::vec::Vec<T>> for Loose<Inner>
where
    Inner: RecursiveDeserialize<T>,
{
    fn deserialize<'de, D>(deserializer: D) -> Result<alloc::vec::Vec<T>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        <alloc::vec::Vec<DeserializeAs<Inner, T>> as serde::Deserialize>::deserialize(deserializer)
            .map(|elements| elements.into_iter().map(|element| element.0).collect())
    }
}

impl<Inner, T> RecursiveSerialize<Option<T>> for Optional<Inner>
where
    Inner: RecursiveSerialize<T>,
{
    fn serialize<S>(value: &Option<T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match value {
            Some(value) => Inner::serialize(value, serializer),
            None => serializer.serialize_none(),
        }
    }
}

impl<Inner, T> RecursiveDeserialize<Option<T>> for Optional<Inner>
where
    Inner: RecursiveDeserialize<T>,
{
    fn deserialize<'de, D>(deserializer: D) -> Result<Option<T>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        <Option<DeserializeAs<Inner, T>> as serde::Deserialize>::deserialize(deserializer)
            .map(|value| value.map(|value| value.0))
    }
}

pub fn serialize_recursive<Shape, T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
where
    Shape: RecursiveSerialize<T>,
    S: serde::Serializer,
{
    Shape::serialize(value, serializer)
}

pub fn deserialize_recursive<'de, Shape, T, D>(deserializer: D) -> Result<T, D::Error>
where
    Shape: RecursiveDeserialize<T>,
    D: serde::Deserializer<'de>,
{
    Shape::deserialize(deserializer)
}

// The outer Option of an optional-and-nullable record member is its PRESENCE bit, not part of the
// recursive descriptor: `? f: (T / null)` stores `Option<Option<T>>`, while the descriptor owns
// the inner nullable `Option<T>`. serde does not call a field callback for a missing key (the
// accompanying `#[serde(default)]` supplies outer `None`), so every value decoded here is present
// and must be wrapped in `Some`, including JSON null's descriptor result of `None`.
pub fn serialize_optional_recursive<Shape, T, S>(
    value: &Option<T>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    Shape: RecursiveSerialize<T>,
    S: serde::Serializer,
{
    match value {
        Some(value) => Shape::serialize(value, serializer),
        // This is a safe standalone fallback. The field annotation skips outer None, so generated
        // optional-and-nullable members do not normally reach this branch.
        None => serializer.serialize_none(),
    }
}

pub fn deserialize_optional_recursive<'de, Shape, T, D>(
    deserializer: D,
) -> Result<Option<T>, D::Error>
where
    Shape: RecursiveDeserialize<T>,
    D: serde::Deserializer<'de>,
{
    Shape::deserialize(deserializer).map(Some)
}
