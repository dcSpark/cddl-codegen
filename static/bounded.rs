use super::error::{DeserializeError, DeserializeFailure};

/// A `Vec<T>` whose inclusive CDDL occurrence window is part of its Rust type.
///
/// `TryFrom<Vec<T>>` is the only loose-to-tight door.  Consequently decoding and ordinary API
/// construction report the same `RangeCheck`, and callers can edit elements without receiving a
/// mutable `Vec` that could invalidate the cardinality invariant.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundedVec<T, const MIN: u64, const MAX: u64>(Vec<T>);

impl<T, const MIN: u64, const MAX: u64> BoundedVec<T, MIN, MAX> {
    fn range_error(found: usize) -> DeserializeError {
        DeserializeFailure::RangeCheck {
            found: found as i128,
            min: Some(MIN as i128),
            max: if MAX == u64::MAX { None } else { Some(MAX as i128) },
        }
        .into()
    }

    fn valid_len(len: usize) -> bool {
        let len = len as u64;
        MIN <= MAX && len >= MIN && len <= MAX
    }

    pub fn len(&self) -> usize { self.0.len() }
    pub fn is_empty(&self) -> bool { self.0.is_empty() }
    pub fn get(&self, index: usize) -> Option<&T> { self.0.get(index) }
    pub fn iter(&self) -> core::slice::Iter<'_, T> { self.0.iter() }
    pub fn iter_mut(&mut self) -> core::slice::IterMut<'_, T> { self.0.iter_mut() }
    pub fn as_slice(&self) -> &[T] { self.0.as_slice() }
    pub fn as_mut_slice(&mut self) -> &mut [T] { self.0.as_mut_slice() }
    pub fn into_inner(self) -> Vec<T> { self.0 }

    pub fn push(&mut self, value: T) -> Result<(), DeserializeError> {
        if self.0.len() as u64 == MAX { return Err(Self::range_error(self.0.len().saturating_add(1))); }
        self.0.push(value);
        Ok(())
    }

    pub fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) -> Result<(), DeserializeError> {
        let extra: Vec<T> = iter.into_iter().collect();
        let len = self.0.len().saturating_add(extra.len());
        if len as u64 > MAX { return Err(Self::range_error(len)); }
        self.0.extend(extra);
        Ok(())
    }

    pub fn pop(&mut self) -> Result<T, DeserializeError> {
        if (self.0.len() as u64) <= MIN { return Err(Self::range_error(self.0.len().saturating_sub(1))); }
        Ok(self.0.pop().expect("length checked before BoundedVec::pop"))
    }

    pub fn remove(&mut self, index: usize) -> Result<T, DeserializeError> {
        if (self.0.len() as u64) <= MIN { return Err(Self::range_error(self.0.len().saturating_sub(1))); }
        Ok(self.0.remove(index))
    }

    pub fn truncate(&mut self, len: usize) -> Result<(), DeserializeError> {
        if (len as u64) < MIN { return Err(Self::range_error(len)); }
        self.0.truncate(len);
        Ok(())
    }
}

// An empty seed is statically meaningful only at the zero-minimum instantiation. Keeping this
// inherent method off every other `MIN` makes an impossible construction route unnameable, instead
// of advertising a constructor that can only fail at runtime.
impl<T, const MAX: u64> BoundedVec<T, 0, MAX> {
    pub fn new() -> Self { Self(Vec::new()) }
}

impl<T, const MIN: u64, const MAX: u64> TryFrom<Vec<T>> for BoundedVec<T, MIN, MAX> {
    type Error = DeserializeError;
    fn try_from(vec: Vec<T>) -> Result<Self, Self::Error> {
        if Self::valid_len(vec.len()) { Ok(Self(vec)) } else { Err(Self::range_error(vec.len())) }
    }
}

impl<T, const MIN: u64, const MAX: u64> From<BoundedVec<T, MIN, MAX>> for Vec<T> {
    fn from(value: BoundedVec<T, MIN, MAX>) -> Self { value.0 }
}
impl<T, const MIN: u64, const MAX: u64> AsRef<[T]> for BoundedVec<T, MIN, MAX> {
    fn as_ref(&self) -> &[T] { self.0.as_slice() }
}
impl<T, const MIN: u64, const MAX: u64> core::ops::Index<usize> for BoundedVec<T, MIN, MAX> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output { &self.0[index] }
}
impl<T, const MIN: u64, const MAX: u64> core::ops::IndexMut<usize> for BoundedVec<T, MIN, MAX> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output { &mut self.0[index] }
}
impl<'a, T, const MIN: u64, const MAX: u64> IntoIterator for &'a BoundedVec<T, MIN, MAX> {
    type Item = &'a T;
    type IntoIter = core::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter { self.0.iter() }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn checked_door_and_mutators_preserve_both_bounds() {
        assert!(BoundedVec::<u64, 2, 5>::try_from(vec![1]).is_err());
        let mut value = BoundedVec::<u64, 2, 5>::try_from(vec![1, 2]).unwrap();
        assert!(value.pop().is_err());
        value.push(3).unwrap();
        value.push(4).unwrap();
        value.push(5).unwrap();
        assert!(value.push(6).is_err());
        assert_eq!(value.as_mut_slice()[0], 1);
    }

    #[test]
    fn zero_min_seed_is_infallible_and_invalid_const_window_is_refused() {
        let empty: BoundedVec<u64, 0, 1> = BoundedVec::new();
        assert!(empty.is_empty());
        assert!(BoundedVec::<u64, 2, 1>::try_from(vec![1, 2]).is_err());
    }
}
