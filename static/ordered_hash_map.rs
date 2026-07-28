use core::hash::{BuildHasher, Hash};

/// The hash builder backing `OrderedHashMap`: std's `RandomState` (randomly seeded SipHash, so keys
/// chosen by whoever wrote the CBOR can't drive a table into its worst case). The backing map takes
/// its hash builder as a type parameter and defaults it to a non-std one, so naming the choice here
/// keeps it deliberate. It is a public alias rather than a written-out path because it appears in
/// the signatures below — anything that names those names this too, so the alias is the one place
/// the choice can later change without renaming a public type.
pub type MapHashBuilder = std::collections::hash_map::RandomState;

/// The two halves of [`Entry`], re-exported so code matching on an entry can name them without
/// depending on the backing map crate itself.
pub use hashlink::linked_hash_map::{OccupiedEntry, VacantEntry};

#[derive(Clone, Debug, Hash, Ord, Eq, PartialEq, PartialOrd)]
pub struct OrderedHashMap<K, V>(hashlink::LinkedHashMap<K, V, MapHashBuilder>) where
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
    type Target = hashlink::LinkedHashMap<K, V, MapHashBuilder>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K, V> std::ops::DerefMut for OrderedHashMap<K, V> where K : Hash + Eq + Ord {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// A view into a single entry of an `OrderedHashMap`, identical to the backing map's own entry view
/// EXCEPT that `or_insert`/`or_insert_with` do not move an occupied entry to the back of the
/// insertion order (the backing crate's versions do, refreshing it LRU-style).
///
/// A table's iteration order IS its serialized key order, so under `--preserve-encodings` a moved
/// entry would silently rewrite the bytes of a value that was only read off the wire and
/// incremented. Reading or accumulating through an entry therefore leaves the order alone; only an
/// explicit `insert` of a new value re-positions a key.
pub enum Entry<'a, K, V, S = MapHashBuilder> {
    Occupied(OccupiedEntry<'a, K, V, S>),
    Vacant(VacantEntry<'a, K, V, S>),
}

impl<'a, K, V, S> Entry<'a, K, V, S> {
    /// The value for this entry, inserting `default` first if it is vacant. An occupied entry keeps
    /// its position; a vacant one is appended at the back, exactly as a fresh `insert` would be.
    pub fn or_insert(self, default: V) -> &'a mut V where K : Hash, S : BuildHasher {
        match self {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(default),
        }
    }

    /// `or_insert` with the default computed only when the entry is vacant.
    pub fn or_insert_with<F: FnOnce() -> V>(self, default: F) -> &'a mut V where K : Hash, S : BuildHasher {
        match self {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(default()),
        }
    }

    /// The key this entry was looked up by, whether or not it is present.
    pub fn key(&self) -> &K {
        match self {
            Entry::Occupied(entry) => entry.key(),
            Entry::Vacant(entry) => entry.key(),
        }
    }
}

impl<K, V> OrderedHashMap<K, V> where K : Hash + Eq + Ord {
    pub fn new() -> Self {
        Self(hashlink::LinkedHashMap::with_hasher(MapHashBuilder::new()))
    }

    /// Consume the wrapper, yielding the backing insertion-ordered map.
    pub fn take(self) -> hashlink::LinkedHashMap<K, V, MapHashBuilder> {
        self.0
    }

    /// Shadows the `Deref`'d entry view with the order-preserving [`Entry`] above — every other
    /// method reaches the backing map through `Deref` unchanged.
    pub fn entry(&mut self, key: K) -> Entry<'_, K, V, MapHashBuilder> {
        match self.0.entry(key) {
            hashlink::linked_hash_map::Entry::Occupied(entry) => Entry::Occupied(entry),
            hashlink::linked_hash_map::Entry::Vacant(entry) => Entry::Vacant(entry),
        }
    }
}

impl<K, V> FromIterator<(K, V)> for OrderedHashMap<K, V> where K : Hash + Eq + Ord {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self(hashlink::LinkedHashMap::from_iter(iter))
    }
}
