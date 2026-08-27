// Appended only when `include_bounded_vec`; cddl-matrix/no_std_check.ts's `json_schema` profile
// reaches this fragment with a wide exact array under a bounded occurrence.
impl<Inner, T, const MIN: u64, const MAX: u64> RecursiveSerialize<super::bounded::BoundedVec<T, MIN, MAX>>
    for Bounded<Inner, MIN, MAX>
where
    Inner: RecursiveSerialize<T>,
{
    fn serialize<S>(value: &super::bounded::BoundedVec<T, MIN, MAX>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(value.iter().map(|value| {
            SerializeAs::<Inner, T>(value, core::marker::PhantomData)
        }))
    }
}

impl<Inner, T, const MIN: u64, const MAX: u64> RecursiveDeserialize<super::bounded::BoundedVec<T, MIN, MAX>>
    for Bounded<Inner, MIN, MAX>
where
    Inner: RecursiveDeserialize<T>,
{
    fn deserialize<'de, D>(deserializer: D) -> Result<super::bounded::BoundedVec<T, MIN, MAX>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let elements: alloc::vec::Vec<T> = <alloc::vec::Vec<DeserializeAs<Inner, T>> as serde::Deserialize>::deserialize(deserializer)?
            .into_iter()
            .map(|element| element.0)
            .collect();
        super::bounded::BoundedVec::try_from(elements).map_err(serde::de::Error::custom)
    }
}
