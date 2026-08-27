// Appended only when ordered_set.rs is present. A duplicate-reject collection must walk its
// elements through the recursive descriptor, then cross its concrete `TryFrom<Vec<_>>` door once.
impl<Inner, T> RecursiveSerialize<super::ordered_set::OrderedSet<T>> for RejectSet<Inner>
where
    Inner: RecursiveSerialize<T>,
{
    fn serialize<S>(value: &super::ordered_set::OrderedSet<T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(value.iter().map(|value| {
            SerializeAs::<Inner, T>(value, core::marker::PhantomData)
        }))
    }
}

impl<Inner, T> RecursiveDeserialize<super::ordered_set::OrderedSet<T>> for RejectSet<Inner>
where
    Inner: RecursiveDeserialize<T>,
    T: Ord,
{
    fn deserialize<'de, D>(deserializer: D) -> Result<super::ordered_set::OrderedSet<T>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let elements: alloc::vec::Vec<T> = <alloc::vec::Vec<DeserializeAs<Inner, T>> as serde::Deserialize>::deserialize(deserializer)?
            .into_iter()
            .map(|element| element.0)
            .collect();
        super::ordered_set::OrderedSet::try_from(elements).map_err(serde::de::Error::custom)
    }
}

impl<Inner, T> RecursiveSerialize<super::ordered_set::NonEmptyOrderedSet<T>>
    for RejectSetNonEmpty<Inner>
where
    Inner: RecursiveSerialize<T>,
{
    fn serialize<S>(value: &super::ordered_set::NonEmptyOrderedSet<T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(value.iter().map(|value| {
            SerializeAs::<Inner, T>(value, core::marker::PhantomData)
        }))
    }
}

impl<Inner, T> RecursiveDeserialize<super::ordered_set::NonEmptyOrderedSet<T>>
    for RejectSetNonEmpty<Inner>
where
    Inner: RecursiveDeserialize<T>,
    T: Ord,
{
    fn deserialize<'de, D>(deserializer: D) -> Result<super::ordered_set::NonEmptyOrderedSet<T>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let elements: alloc::vec::Vec<T> = <alloc::vec::Vec<DeserializeAs<Inner, T>> as serde::Deserialize>::deserialize(deserializer)?
            .into_iter()
            .map(|element| element.0)
            .collect();
        super::ordered_set::NonEmptyOrderedSet::try_from(elements).map_err(serde::de::Error::custom)
    }
}

impl<Inner, T, const MIN: u64, const MAX: u64>
    RecursiveSerialize<super::ordered_set::BoundedOrderedSet<T, MIN, MAX>>
    for RejectSetBounded<Inner, MIN, MAX>
where
    Inner: RecursiveSerialize<T>,
{
    fn serialize<S>(value: &super::ordered_set::BoundedOrderedSet<T, MIN, MAX>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(value.iter().map(|value| {
            SerializeAs::<Inner, T>(value, core::marker::PhantomData)
        }))
    }
}

impl<Inner, T, const MIN: u64, const MAX: u64>
    RecursiveDeserialize<super::ordered_set::BoundedOrderedSet<T, MIN, MAX>>
    for RejectSetBounded<Inner, MIN, MAX>
where
    Inner: RecursiveDeserialize<T>,
    T: Ord,
{
    fn deserialize<'de, D>(deserializer: D) -> Result<super::ordered_set::BoundedOrderedSet<T, MIN, MAX>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let elements: alloc::vec::Vec<T> = <alloc::vec::Vec<DeserializeAs<Inner, T>> as serde::Deserialize>::deserialize(deserializer)?
            .into_iter()
            .map(|element| element.0)
            .collect();
        super::ordered_set::BoundedOrderedSet::try_from(elements).map_err(serde::de::Error::custom)
    }
}
