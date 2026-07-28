use core::hash::Hash;

/// Mirrors the hasher choice the generated runtime makes (`static/ordered_hash_map.rs`): the backing
/// map defaults its hash-builder parameter to a non-std one, so std's `RandomState` is named
/// explicitly. Only the surface this fixture's dep-crate role needs is mirrored — the entry view,
/// `take()` and `FromIterator` are deliberately absent.
pub type MapHashBuilder = std::collections::hash_map::RandomState;

#[derive(Clone, Debug, Default, Hash, Ord, Eq, PartialEq, PartialOrd)]
pub struct OrderedHashMap<K, V>(hashlink::LinkedHashMap<K, V, MapHashBuilder>)
where
    K: Hash + Eq + Ord;

impl<K, V> std::ops::Deref for OrderedHashMap<K, V>
where
    K: Hash + Eq + Ord,
{
    type Target = hashlink::LinkedHashMap<K, V, MapHashBuilder>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K, V> std::ops::DerefMut for OrderedHashMap<K, V>
where
    K: Hash + Eq + Ord,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<K, V> OrderedHashMap<K, V>
where
    K: Hash + Eq + Ord,
{
    pub fn new() -> Self {
        Self(hashlink::LinkedHashMap::with_hasher(MapHashBuilder::new()))
    }
}
