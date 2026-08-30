impl<Inner, K, V> RecursiveSerialize<super::non_empty_map::NonEmptyMap<K, V>> for NonEmptyMap<Inner>
where K: Ord + serde::Serialize, Inner: RecursiveSerialize<V> {
    fn serialize<S>(value: &super::non_empty_map::NonEmptyMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer { serialize_recursive_object_map::<K, V, Inner, S>(value.iter(), value.len(), serializer) }
}
impl<Inner, K, V> RecursiveDeserialize<super::non_empty_map::NonEmptyMap<K, V>> for NonEmptyMap<Inner>
where K: Ord + for<'de> serde::Deserialize<'de>, Inner: RecursiveDeserialize<V> {
    fn deserialize<'de, D>(deserializer: D) -> Result<super::non_empty_map::NonEmptyMap<K, V>, D::Error>
    where D: serde::Deserializer<'de> {
        let staged: alloc::collections::BTreeMap<K, V> = <alloc::collections::BTreeMap<K, RecursiveDeserializeAs<Inner, V>> as serde::Deserialize>::deserialize(deserializer)?
            .into_iter().map(|(k, v)| (k, v.into_inner())).collect();
        super::non_empty_map::NonEmptyMap::try_from(staged).map_err(serde::de::Error::custom)
    }
}
