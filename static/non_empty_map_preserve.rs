use alloc::collections::BTreeMap;

// A preserve-flavored shared runtime can also serve a reduced consumer: generated reduced code
// stages `{+ K => V}` in a BTreeMap, while this runtime stores an OrderedHashMap to retain wire
// order. This fragment is appended AFTER non_empty_map.rs receives its preserve rewrite, so its
// BTreeMap bridge is deliberately not rewritten into a duplicate OrderedHashMap implementation.
impl<K: Ord + core::hash::Hash + Eq, V> TryFrom<BTreeMap<K, V>> for NonEmptyMap<K, V> {
    type Error = DeserializeError;

    fn try_from(map: BTreeMap<K, V>) -> Result<Self, Self::Error> {
        // A BTreeMap already has unique keys, so collecting only transfers its deterministic key
        // order into the preserve runtime's insertion-ordered storage. Delegate range validation to
        // the native door so an empty source keeps the exact RangeCheck contract.
        Self::try_from(map.into_iter().collect::<OrderedHashMap<K, V>>())
    }
}
