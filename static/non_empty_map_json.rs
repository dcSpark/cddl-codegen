impl<K: Ord + serde::Serialize, V: serde::Serialize> serde::Serialize for NonEmptyMap<K, V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // a NonEmptyMap is a plain JSON object — serialize as the loose map would
        self.0.serialize(serializer)
    }
}

impl<'de, K: Ord + serde::Deserialize<'de>, V: serde::Deserialize<'de>> serde::de::Deserialize<'de>
    for NonEmptyMap<K, V>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        // never derive a structural Deserialize (it would bypass the invariant): deserialize the
        // loose map and route through the same TryFrom door as every other construction path.
        let map = <BTreeMap<K, V> as serde::de::Deserialize>::deserialize(deserializer)?;
        NonEmptyMap::try_from(map).map_err(serde::de::Error::custom)
    }
}
