// Recursive JSON support for object-shaped maps. The key stays on serde's ordinary JSON member
// name path; only the value is wrapped, which is the boundary certified by the IR preclaim.
fn serialize_recursive_object_map<'a, K: 'a, V: 'a, Inner, S>(
    entries: impl Iterator<Item = (&'a K, &'a V)>,
    len: usize,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    K: serde::Serialize,
    Inner: RecursiveSerialize<V>,
    S: serde::Serializer,
{
    use serde::ser::SerializeMap as _;
    let mut out = serializer.serialize_map(Some(len))?;
    for (key, value) in entries {
        out.serialize_entry(key, &RecursiveSerializeAs::<Inner, V>::new(value))?;
    }
    out.end()
}

impl<Inner, K, V> RecursiveSerialize<alloc::collections::BTreeMap<K, V>> for Map<Inner>
where
    K: Ord + serde::Serialize,
    Inner: RecursiveSerialize<V>,
{
    fn serialize<S>(value: &alloc::collections::BTreeMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer {
        serialize_recursive_object_map::<K, V, Inner, S>(value.iter(), value.len(), serializer)
    }
}

impl<Inner, K, V> RecursiveDeserialize<alloc::collections::BTreeMap<K, V>> for Map<Inner>
where
    K: Ord + for<'de> serde::Deserialize<'de>,
    Inner: RecursiveDeserialize<V>,
{
    fn deserialize<'de, D>(deserializer: D) -> Result<alloc::collections::BTreeMap<K, V>, D::Error>
    where D: serde::Deserializer<'de> {
        <alloc::collections::BTreeMap<K, RecursiveDeserializeAs<Inner, V>> as serde::Deserialize>::deserialize(deserializer)
            .map(|entries| entries.into_iter().map(|(k, v)| (k, v.into_inner())).collect())
    }
}
