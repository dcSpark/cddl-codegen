impl<K: Ord + serde::Serialize, V: serde::Serialize, const MIN: u64, const MAX: u64> serde::Serialize for BoundedMap<K, V, MIN, MAX> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> { self.0.serialize(serializer) }
}
impl<'de, K: Ord + serde::Deserialize<'de>, V: serde::Deserialize<'de>, const MIN: u64, const MAX: u64> serde::Deserialize<'de> for BoundedMap<K, V, MIN, MAX> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        BoundedMap::try_from(BTreeMap::<K, V>::deserialize(deserializer)?).map_err(serde::de::Error::custom)
    }
}
