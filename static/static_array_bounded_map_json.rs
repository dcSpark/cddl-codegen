impl<Inner, K, V, const MIN: u64, const MAX: u64>
    RecursiveSerialize<super::bounded_map::BoundedMap<K, V, MIN, MAX>> for BoundedMap<Inner, MIN, MAX>
where K: Ord + serde::Serialize, Inner: RecursiveSerialize<V> {
    fn serialize<S>(value: &super::bounded_map::BoundedMap<K, V, MIN, MAX>, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer { serialize_recursive_object_map::<K, V, Inner, S>(value.iter(), value.len(), serializer) }
}
impl<Inner, K, V, const MIN: u64, const MAX: u64>
    RecursiveDeserialize<super::bounded_map::BoundedMap<K, V, MIN, MAX>> for BoundedMap<Inner, MIN, MAX>
where K: Ord + for<'de> serde::Deserialize<'de>, Inner: RecursiveDeserialize<V> {
    fn deserialize<'de, D>(deserializer: D) -> Result<super::bounded_map::BoundedMap<K, V, MIN, MAX>, D::Error>
    where D: serde::Deserializer<'de> {
        let staged: alloc::collections::BTreeMap<K, V> = <alloc::collections::BTreeMap<K, RecursiveDeserializeAs<Inner, V>> as serde::Deserialize>::deserialize(deserializer)?
            .into_iter().map(|(k, v)| (k, v.into_inner())).collect();
        super::bounded_map::BoundedMap::try_from(staged).map_err(serde::de::Error::custom)
    }
}
