use alloc::collections::{BTreeMap, BTreeSet};

use super::error::{DeserializeError, DeserializeFailure};

/// A deterministic map whose inclusive CDDL occurrence window is part of its type.
///
/// Every loose-carrier construction route goes through `TryFrom`; mutable operations deliberately
/// expose only values, never the carrier, so a valid window cannot be invalidated after construction.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundedMap<K: Ord, V, const MIN: u64, const MAX: u64>(BTreeMap<K, V>);

impl<K: Ord, V, const MIN: u64, const MAX: u64> BoundedMap<K, V, MIN, MAX> {
    fn range_error(found: usize) -> DeserializeError {
        DeserializeFailure::RangeCheck {
            found: found as i128,
            min: Some(MIN as i128),
            max: if MAX == u64::MAX { None } else { Some(MAX as i128) },
        }.into()
    }
    fn valid_len(len: usize) -> bool {
        let len = len as u64;
        MIN <= MAX && len >= MIN && len <= MAX
    }
    pub fn len(&self) -> usize { self.0.len() }
    pub fn is_empty(&self) -> bool { self.0.is_empty() }
    pub fn get(&self, key: &K) -> Option<&V> { self.0.get(key) }
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> { self.0.get_mut(key) }
    pub fn contains_key(&self, key: &K) -> bool { self.0.contains_key(key) }
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> { self.0.iter() }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> { self.0.iter_mut() }
    pub fn keys(&self) -> impl Iterator<Item = &K> { self.0.keys() }
    pub fn values(&self) -> impl Iterator<Item = &V> { self.0.values() }
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> { self.0.values_mut() }
    pub fn into_inner(self) -> BTreeMap<K, V> { self.0 }

    /// Replacing an existing key is legal even at `MAX`; only a new key can grow the map.
    pub fn insert(&mut self, key: K, value: V) -> Result<Option<V>, DeserializeError> {
        if !self.0.contains_key(&key) && self.0.len() as u64 >= MAX {
            return Err(Self::range_error(self.0.len().saturating_add(1)));
        }
        Ok(self.0.insert(key, value))
    }
    pub fn remove(&mut self, key: &K) -> Result<Option<V>, DeserializeError> {
        if self.0.contains_key(key) && self.0.len() as u64 <= MIN {
            return Err(Self::range_error(self.0.len().saturating_sub(1)));
        }
        Ok(self.0.remove(key))
    }
    /// Count new keys before mutating, keeping the receiver unchanged on failure.
    pub fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) -> Result<(), DeserializeError> {
        let rows: Vec<(K, V)> = iter.into_iter().collect();
        let mut new_keys = BTreeSet::new();
        for (key, _) in &rows {
            if !self.0.contains_key(key) { new_keys.insert(key); }
        }
        let next = self.0.len().saturating_add(new_keys.len());
        if !Self::valid_len(next) { return Err(Self::range_error(next)); }
        self.0.extend(rows);
        Ok(())
    }
}

impl<K: Ord, V, const MAX: u64> BoundedMap<K, V, 0, MAX> {
    pub fn new() -> Self { Self(BTreeMap::new()) }
}

impl<K: Ord, V, const MIN: u64, const MAX: u64> TryFrom<BTreeMap<K, V>> for BoundedMap<K, V, MIN, MAX> {
    type Error = DeserializeError;
    fn try_from(map: BTreeMap<K, V>) -> Result<Self, Self::Error> {
        if Self::valid_len(map.len()) { Ok(Self(map)) } else { Err(Self::range_error(map.len())) }
    }
}
impl<K: Ord, V, const MIN: u64, const MAX: u64> TryFrom<Vec<(K, V)>> for BoundedMap<K, V, MIN, MAX> {
    type Error = DeserializeError;
    fn try_from(rows: Vec<(K, V)>) -> Result<Self, Self::Error> { Self::try_from(rows.into_iter().collect::<BTreeMap<_, _>>()) }
}
impl<K: Ord, V, const MIN: u64, const MAX: u64> From<BoundedMap<K, V, MIN, MAX>> for BTreeMap<K, V> {
    fn from(value: BoundedMap<K, V, MIN, MAX>) -> Self { value.0 }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn checked_door_and_mutators_preserve_the_window() {
        assert!(BoundedMap::<u64, u64, 2, 3>::try_from(BTreeMap::new()).is_err());
        let mut inner = BTreeMap::new();
        inner.insert(1, 10);
        inner.insert(2, 20);
        let mut map = BoundedMap::<u64, u64, 2, 3>::try_from(inner).unwrap();
        assert!(map.remove(&1).is_err());
        map.insert(3, 30).unwrap();
        assert_eq!(map.insert(3, 31).unwrap(), Some(30));
        assert!(map.insert(4, 40).is_err());
        *map.get_mut(&3).unwrap() = 32;
        assert_eq!(map.get(&3), Some(&32));
        assert!(map.extend([(4, 40)]).is_err());
        assert!(!map.contains_key(&4));
    }

    #[test]
    fn zero_seed_and_invalid_const_window_are_refused() {
        assert!(BoundedMap::<u64, u64, 0, 1>::new().is_empty());
        assert!(BoundedMap::<u64, u64, 2, 1>::try_from(BTreeMap::new()).is_err());
    }
}
