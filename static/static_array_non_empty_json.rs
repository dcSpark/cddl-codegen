// Appended only when `include_non_empty_vec`; cddl-matrix/no_std_check.ts's `json_schema` profile
// reaches this fragment with a wide exact array under `[+ ...]`.
impl<Inner, T> RecursiveSerialize<super::non_empty::NonEmptyVec<T>> for NonEmpty<Inner>
where
    Inner: RecursiveSerialize<T>,
{
    fn serialize<S>(value: &super::non_empty::NonEmptyVec<T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(value.iter().map(|value| {
            SerializeAs::<Inner, T>(value, core::marker::PhantomData)
        }))
    }
}

impl<Inner, T> RecursiveDeserialize<super::non_empty::NonEmptyVec<T>> for NonEmpty<Inner>
where
    Inner: RecursiveDeserialize<T>,
{
    fn deserialize<'de, D>(deserializer: D) -> Result<super::non_empty::NonEmptyVec<T>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let elements: alloc::vec::Vec<T> = <alloc::vec::Vec<DeserializeAs<Inner, T>> as serde::Deserialize>::deserialize(deserializer)?
            .into_iter()
            .map(|element| element.0)
            .collect();
        super::non_empty::NonEmptyVec::try_from(elements).map_err(serde::de::Error::custom)
    }
}
