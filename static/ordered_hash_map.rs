use core::hash::Hash;

#[derive(Clone, Debug, Hash, Ord, Eq, PartialEq, PartialOrd)]
pub struct OrderedHashMap<K, V>(linked_hash_map::LinkedHashMap<K, V>) where
    K : Hash + Eq + Ord;

// An empty ordered map is always constructible, so `Default` must not require `K: Default` /
// `V: Default` the way `#[derive(Default)]` over a generic struct would. This mirrors
// `std::collections::BTreeMap`/`HashMap` (empty-map default, no element bounds) so a table whose
// KEY type isn't `Default` — e.g. a generated `@used_as_key` enum under `--preserve-encodings`,
// where tables become `OrderedHashMap` and enum keys don't derive `Default` — can still be
// `Default::default()`ed
impl<K, V> Default for OrderedHashMap<K, V> where K : Hash + Eq + Ord {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> std::ops::Deref for OrderedHashMap<K, V> where K : Hash + Eq + Ord {
    type Target = linked_hash_map::LinkedHashMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K, V> std::ops::DerefMut for OrderedHashMap<K, V> where K : Hash + Eq + Ord {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<K, V> OrderedHashMap<K, V> where K : Hash + Eq + Ord {
    pub fn new() -> Self {
        Self(linked_hash_map::LinkedHashMap::new())
    }

    /// Consume the wrapper, yielding the backing insertion-ordered map.
    pub fn take(self) -> linked_hash_map::LinkedHashMap<K, V> {
        self.0
    }
}

impl<K, V> FromIterator<(K, V)> for OrderedHashMap<K, V> where K : Hash + Eq + Ord {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self(linked_hash_map::LinkedHashMap::from_iter(iter))
    }
}
