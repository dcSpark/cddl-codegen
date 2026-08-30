// Pair maps use an array of `[key, value]` pairs, so recursive descriptors may own either side.
fn serialize_recursive_pairs<'a, K: 'a, V: 'a, Key, Value, S>(
    entries: impl Iterator<Item = (&'a K, &'a V)>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    Key: RecursiveSerialize<K>,
    Value: RecursiveSerialize<V>,
    S: serde::Serializer,
{
    serializer.collect_seq(entries.map(|(key, value)| (
        RecursiveSerializeAs::<Key, K>::new(key),
        RecursiveSerializeAs::<Value, V>::new(value),
    )))
}

fn deserialize_recursive_pairs<'de, K, V, Key, Value, D>(
    deserializer: D,
) -> Result<alloc::vec::Vec<(K, V)>, D::Error>
where
    Key: RecursiveDeserialize<K>,
    Value: RecursiveDeserialize<V>,
    D: serde::Deserializer<'de>,
{
    <alloc::vec::Vec<(RecursiveDeserializeAs<Key, K>, RecursiveDeserializeAs<Value, V>)> as serde::Deserialize>::deserialize(deserializer)
        .map(|entries| entries.into_iter().map(|(k, v)| (k.into_inner(), v.into_inner())).collect())
}

impl<Key, Value, K, V> RecursiveSerialize<super::pair_map::PairMap<K, V>> for PairMap<Key, Value>
where Key: RecursiveSerialize<K>, Value: RecursiveSerialize<V> {
    fn serialize<S>(value: &super::pair_map::PairMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer { serialize_recursive_pairs::<K, V, Key, Value, S>(value.iter(), serializer) }
}
impl<Key, Value, K, V> RecursiveDeserialize<super::pair_map::PairMap<K, V>> for PairMap<Key, Value>
where Key: RecursiveDeserialize<K>, Value: RecursiveDeserialize<V> {
    fn deserialize<'de, D>(deserializer: D) -> Result<super::pair_map::PairMap<K, V>, D::Error>
    where D: serde::Deserializer<'de> { deserialize_recursive_pairs::<K, V, Key, Value, D>(deserializer).map(super::pair_map::PairMap::from) }
}

impl<Key, Value, K, V> RecursiveSerialize<super::pair_map::NonEmptyPairMap<K, V>> for NonEmptyPairMap<Key, Value>
where Key: RecursiveSerialize<K>, Value: RecursiveSerialize<V> {
    fn serialize<S>(value: &super::pair_map::NonEmptyPairMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer { serialize_recursive_pairs::<K, V, Key, Value, S>(value.iter(), serializer) }
}
impl<Key, Value, K, V> RecursiveDeserialize<super::pair_map::NonEmptyPairMap<K, V>> for NonEmptyPairMap<Key, Value>
where Key: RecursiveDeserialize<K>, Value: RecursiveDeserialize<V> {
    fn deserialize<'de, D>(deserializer: D) -> Result<super::pair_map::NonEmptyPairMap<K, V>, D::Error>
    where D: serde::Deserializer<'de> { super::pair_map::NonEmptyPairMap::try_from(deserialize_recursive_pairs::<K, V, Key, Value, D>(deserializer)?).map_err(serde::de::Error::custom) }
}

impl<Key, Value, K, V, const MIN: u64, const MAX: u64>
    RecursiveSerialize<super::pair_map::BoundedPairMap<K, V, MIN, MAX>> for BoundedPairMap<Key, Value, MIN, MAX>
where Key: RecursiveSerialize<K>, Value: RecursiveSerialize<V> {
    fn serialize<S>(value: &super::pair_map::BoundedPairMap<K, V, MIN, MAX>, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer { serialize_recursive_pairs::<K, V, Key, Value, S>(value.iter(), serializer) }
}
impl<Key, Value, K, V, const MIN: u64, const MAX: u64>
    RecursiveDeserialize<super::pair_map::BoundedPairMap<K, V, MIN, MAX>> for BoundedPairMap<Key, Value, MIN, MAX>
where Key: RecursiveDeserialize<K>, Value: RecursiveDeserialize<V> {
    fn deserialize<'de, D>(deserializer: D) -> Result<super::pair_map::BoundedPairMap<K, V, MIN, MAX>, D::Error>
    where D: serde::Deserializer<'de> { super::pair_map::BoundedPairMap::try_from(deserialize_recursive_pairs::<K, V, Key, Value, D>(deserializer)?).map_err(serde::de::Error::custom) }
}
