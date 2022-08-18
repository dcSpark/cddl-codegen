impl<K, V> serde::Serialize for OrderedHashMap<K, V> where
    K : Hash + Eq + Ord + serde::Serialize,
    V: serde::Serialize {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer {
        let map = self.iter().collect::<std::collections::BTreeMap<_, _>>();
        map.serialize(serializer)
    }
}

impl<'de, K, V> serde::de::Deserialize<'de> for OrderedHashMap<K, V> where
    K: Hash + Eq + Ord + serde::Deserialize<'de>,
    V: serde::Deserialize<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where
    D: serde::de::Deserializer<'de> {
        let map = <std::collections::BTreeMap<_, _> as serde::de::Deserialize>::deserialize(deserializer)?;
        Ok(Self(map.into_iter().collect()))
    }
}

