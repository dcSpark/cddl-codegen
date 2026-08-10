impl<K: serde::Serialize, V: serde::Serialize> serde::Serialize for PairMap<K, V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // A duplicate-permitting map CANNOT be a JSON object (object keys must be unique — a JSON
        // object silently collapses duplicates). Serialize as an ARRAY of `[k, v]` pairs instead,
        // which preserves both order and duplicates. serde renders a `[(K, V)]` slice as exactly
        // that: `[[k, v], [k, v], ...]`.
        self.as_slice().serialize(serializer)
    }
}

impl<'de, K: serde::Deserialize<'de>, V: serde::Deserialize<'de>> serde::de::Deserialize<'de>
    for PairMap<K, V>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        // the array-of-pairs shape from `Serialize` above
        let entries = <Vec<(K, V)> as serde::de::Deserialize>::deserialize(deserializer)?;
        Ok(PairMap::from(entries))
    }
}

impl<K: serde::Serialize, V: serde::Serialize, const MIN: u64, const MAX: u64> serde::Serialize
    for BoundedPairMap<K, V, MIN, MAX>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: serde::Serializer {
        self.as_slice().serialize(serializer)
    }
}

impl<'de, K: serde::Deserialize<'de>, V: serde::Deserialize<'de>, const MIN: u64, const MAX: u64>
    serde::de::Deserialize<'de> for BoundedPairMap<K, V, MIN, MAX>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: serde::de::Deserializer<'de> {
        BoundedPairMap::try_from(<Vec<(K, V)> as serde::de::Deserialize>::deserialize(deserializer)?)
            .map_err(serde::de::Error::custom)
    }
}

impl<K: serde::Serialize, V: serde::Serialize> serde::Serialize for NonEmptyPairMap<K, V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.as_slice().serialize(serializer)
    }
}

impl<'de, K: serde::Deserialize<'de>, V: serde::Deserialize<'de>> serde::de::Deserialize<'de>
    for NonEmptyPairMap<K, V>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        // never derive a structural Deserialize (it would bypass the min-1 invariant): deserialize
        // the loose array-of-pairs and route through the same TryFrom door as every other path.
        let entries = <Vec<(K, V)> as serde::de::Deserialize>::deserialize(deserializer)?;
        NonEmptyPairMap::try_from(entries).map_err(serde::de::Error::custom)
    }
}
