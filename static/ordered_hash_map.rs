use core::hash::Hash;

#[derive(Clone, Debug, Default, Hash, Ord, Eq, PartialEq, PartialOrd)]
pub struct OrderedHashMap<K, V>(linked_hash_map::LinkedHashMap<K, V>)
where
    K: Hash + Eq + Ord;

impl<K, V> core::ops::Deref for OrderedHashMap<K, V>
where
    K: Hash + Eq + Ord,
{
    type Target = linked_hash_map::LinkedHashMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K, V> core::ops::DerefMut for OrderedHashMap<K, V>
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
        Self(linked_hash_map::LinkedHashMap::new())
    }
}
