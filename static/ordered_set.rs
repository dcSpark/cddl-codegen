use super::error::{DeserializeError, DeserializeFailure, Key};

/// A `Vec<T>` guaranteed to hold no duplicate elements — the uniqueness twin of `Vec<T>` for a
/// `[* T]` collection rule carrying `@duplicates reject` (the tag-258 set idiom's strict flavor).
///
/// Insertion-ordered and NEVER sorted: `reject` only narrows which inputs are ACCEPTED, it must not
/// touch the bytes of what is accepted, so an accepted set re-emits byte-exactly in its original wire
/// order (the same reason the name follows the qualified-set family `IndexSet`/`LinkedHashSet`, not a
/// bare `Set` that would falsely claim std-set ordering — and would collide with the generated
/// `pub type Set<T> = …` alias the tag-258 idiom mints).
///
/// Uniqueness is enforced ONCE, at the single `TryFrom<Vec<T>>` door, and thereafter cannot be
/// broken: there is no public field, no `&mut Vec<T>`, and `push` is checked (an already-present
/// element is refused). Construction from a `Vec` that holds a duplicate fails with
/// `DeserializeFailure::DuplicateKey(Key::Uint(i))` where **`i` is the zero-based INDEX of the
/// duplicate element** — deterministic and actionable regardless of the element type — so the API
/// door and the wire door report identical errors.
///
/// Staging a not-yet-unique collection is done with a plain `Vec<T>` (`push`/`extend`/`collect`) and
/// a final `OrderedSet::try_from(vec)` — the loose `Vec` is the builder, so no builder type is needed.
///
/// For working WITH a set, the std set contract is also provided (`HashSet`/`BTreeSet`/`IndexSet`):
/// `insert -> bool` (`false` = already present), `contains`, `Extend` and `FromIterator` (both
/// dedup keep-first, so `extend` is union and `collect` is a normalizing conversion), and `sort`.
/// `try_opt_from` is the empty-means-absent constructor for optional fields. The strict `push -> Result`
/// stays as the duplicate-is-a-bug door (the one the decoder uses).
//
// careful: the incremental doors (`push`/`insert`/`contains`/`Extend`/`FromIterator`) are linear
// `contains` scans (`T: PartialEq`, zero extra storage, O(n) per element). The bulk `TryFrom<Vec<T>>`
// door — the one the CBOR decoder feeds untrusted input through — is the hybrid `scan_unique`
// (`T: Ord`): linear below `SORTED_SCAN_MIN_LEN`, sorted-index O(n log n) above, so a large
// adversarial input cannot buy quadratic deep comparisons.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrderedSet<T>(Vec<T>);

impl<T> OrderedSet<T> {
    /// An empty set — always valid (uniqueness is vacuous).
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Append an element, unless it is already present (which would break uniqueness). On refusal the
    /// `Err`'s `DuplicateKey` carries the INDEX the element would have occupied (the current length);
    /// the element itself is not returned and the collection is left unchanged.
    ///
    /// The would-be index this reports is the SAME index the `TryFrom<Vec<T>>` door (`scan_unique`)
    /// reports for the identical duplicate: a `Vec` grown by appending has its first repeat sitting at
    /// exactly the length the collection held when that repeat was appended, so the strict-door
    /// (`push`) and the bulk-door (`try_from`) index conventions agree by construction.
    ///
    /// This is the strict, duplicate-is-a-bug door. For the std set contract ("already present" is a
    /// benign no-op, not an error), use [`insert`](Self::insert), which returns `bool`.
    pub fn push(&mut self, value: T) -> Result<(), DeserializeError>
    where
        T: PartialEq,
    {
        if self.0.contains(&value) {
            return Err(DeserializeFailure::DuplicateKey(Key::Uint(self.0.len() as u64)).into());
        }
        self.0.push(value);
        Ok(())
    }

    /// Insert an element, returning `true` if it was newly added and `false` if it was already present
    /// (the set is left unchanged). This is the std set contract (`HashSet`/`BTreeSet`/`IndexSet`
    /// `insert`): a duplicate is a benign no-op, not an error, so a set union is a plain `insert` loop
    /// with nothing to discard. For duplicate-is-a-bug strictness use [`push`](Self::push).
    pub fn insert(&mut self, value: T) -> bool
    where
        T: PartialEq,
    {
        if self.0.contains(&value) {
            false
        } else {
            self.0.push(value);
            true
        }
    }

    /// Whether the set already contains `value` (std set contract).
    pub fn contains(&self, value: &T) -> bool
    where
        T: PartialEq,
    {
        self.0.contains(value)
    }

    /// Sort the set in place by element `Ord` (the `IndexSet::sort` precedent — sorting a set of
    /// unique elements cannot create a duplicate, so the uniqueness invariant is preserved). This
    /// CHANGES the byte order the set re-emits: the wire-order round-trip guarantee applies to an
    /// UNTOUCHED decoded set, and deliberately sorting is opting out of it.
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.0.sort();
    }

    /// The empty-means-absent constructor for an optional set field: an empty input is `Ok(None)` (the
    /// field is absent), and a non-empty input goes through the `TryFrom<Vec<T>>` uniqueness door
    /// wrapped in `Some`. Only the duplicate failure surfaces as `Err` — unlike `TryFrom::try_from(v).ok()`,
    /// which would silently swallow it.
    pub fn try_opt_from(vec: Vec<T>) -> Result<Option<Self>, DeserializeError>
    where
        T: Ord,
    {
        if vec.is_empty() {
            Ok(None)
        } else {
            Self::try_from(vec).map(Some)
        }
    }

    /// Remove and return the last element (`None` if empty). Cannot create a duplicate, so unchecked.
    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    /// Remove and return the element at `index`. Cannot create a duplicate, so unchecked.
    pub fn remove(&mut self, index: usize) -> T {
        self.0.remove(index)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.0.get(index)
    }

    pub fn iter(&self) -> core::slice::Iter<'_, T> {
        self.0.iter()
    }

    pub fn as_slice(&self) -> &[T] {
        self.0.as_slice()
    }

    /// Escape hatch to the loose `Vec<T>` — mutate freely, then `try_into` back through the door.
    pub fn into_inner(self) -> Vec<T> {
        self.0
    }
}

impl<T> Default for OrderedSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Below this length `scan_unique` uses the plain linear `contains` scan; at or above it, the
/// sorted-index scan. The crossover is empirical, not asymptotic: for cheap-to-compare elements
/// (32-byte hashes) the linear scan's zero allocation and early-exit comparisons win up to a few
/// dozen elements (measured ~21ns vs ~50ns at n=10, with the sorted scan already ahead by n=100),
/// while expensive comparisons (large near-identical elements — the adversarial decode input) push
/// the crossover *below* 10. 32 sits between those two crossovers: typical protocol sets (a handful
/// of keys/certificates) keep the allocation-free path, and everything large enough for O(n²) to
/// compound gets the O(n log n) scan.
const SORTED_SCAN_MIN_LEN: usize = 32;

/// Scan a `Vec` for the first duplicate; on success return the checked collection. Shared by both
/// twins' doors so the uniqueness scan and its `DuplicateKey(index)` error can never drift.
///
/// The reported index is the scan-order-first second occurrence (see `OrderedSet::push` for why the
/// doors' index conventions agree). The sorted path reproduces it exactly: sorting index keys by
/// `(element, original index)` puts every equal run in ascending index order, so each equal-adjacent
/// pair's later index is a second occurrence, and the minimum over those is the first one in scan
/// order.
fn scan_unique<T: Ord>(vec: &[T]) -> Result<(), DeserializeError> {
    let dup = if vec.len() < SORTED_SCAN_MIN_LEN {
        (1..vec.len()).find(|&i| vec[..i].contains(&vec[i]))
    } else {
        let mut idx: Vec<usize> = (0..vec.len()).collect();
        idx.sort_unstable_by(|&a, &b| vec[a].cmp(&vec[b]).then(a.cmp(&b)));
        idx.windows(2)
            .filter(|w| vec[w[0]].cmp(&vec[w[1]]).is_eq())
            .map(|w| w[1])
            .min()
    };
    match dup {
        Some(i) => Err(DeserializeFailure::DuplicateKey(Key::Uint(i as u64)).into()),
        None => Ok(()),
    }
}

/// An insertion-ordered, duplicate-free `Vec<T>` whose inclusive CDDL occurrence window is part
/// of its Rust type.  This is the compound carrier for bounded `@duplicates reject` arrays.
///
/// Like the other ordered-set twins it preserves accepted input order.  Unlike `OrderedSet`, every
/// mutation is checked against both the uniqueness and cardinality invariants; no mutable slice or
/// loose carrier is exposed.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundedOrderedSet<T, const MIN: u64, const MAX: u64>(Vec<T>);

impl<T, const MIN: u64, const MAX: u64> BoundedOrderedSet<T, MIN, MAX> {
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
    pub fn as_slice(&self) -> &[T] { self.0.as_slice() }
    pub fn into_inner(self) -> Vec<T> { self.0 }

    pub fn contains(&self, value: &T) -> bool where T: PartialEq { self.0.contains(value) }

    pub fn push(&mut self, value: T) -> Result<(), DeserializeError>
    where T: PartialEq {
        if self.0.contains(&value) {
            return Err(DeserializeFailure::DuplicateKey(Key::Uint(self.0.len() as u64)).into());
        }
        if self.0.len() as u64 == MAX {
            return Err(Self::range_error(self.0.len().saturating_add(1)));
        }
        self.0.push(value);
        Ok(())
    }

    pub fn pop(&mut self) -> Result<T, DeserializeError> {
        if (self.0.len() as u64) <= MIN {
            return Err(Self::range_error(self.0.len().saturating_sub(1)));
        }
        Ok(self.0.pop().expect("length checked before BoundedOrderedSet::pop"))
    }

    pub fn remove(&mut self, index: usize) -> Result<T, DeserializeError> {
        if (self.0.len() as u64) <= MIN {
            return Err(Self::range_error(self.0.len().saturating_sub(1)));
        }
        Ok(self.0.remove(index))
    }
}

impl<T, const MAX: u64> BoundedOrderedSet<T, 0, MAX> {
    pub fn new() -> Self { Self(Vec::new()) }
}

impl<T: Ord, const MIN: u64, const MAX: u64> TryFrom<Vec<T>> for BoundedOrderedSet<T, MIN, MAX> {
    type Error = DeserializeError;
    fn try_from(vec: Vec<T>) -> Result<Self, Self::Error> {
        // Retain OrderedSet's duplicate-first door precedence: malformed uniqueness is reported
        // identically before the cardinality window is considered.
        scan_unique(&vec)?;
        if Self::valid_len(vec.len()) { Ok(Self(vec)) } else { Err(Self::range_error(vec.len())) }
    }
}

impl<T, const MIN: u64, const MAX: u64> From<BoundedOrderedSet<T, MIN, MAX>> for Vec<T> {
    fn from(value: BoundedOrderedSet<T, MIN, MAX>) -> Self { value.0 }
}
impl<T, const MIN: u64, const MAX: u64> AsRef<[T]> for BoundedOrderedSet<T, MIN, MAX> {
    fn as_ref(&self) -> &[T] { self.0.as_slice() }
}
impl<T, const MIN: u64, const MAX: u64> core::ops::Index<usize> for BoundedOrderedSet<T, MIN, MAX> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output { &self.0[index] }
}
impl<'a, T, const MIN: u64, const MAX: u64> IntoIterator for &'a BoundedOrderedSet<T, MIN, MAX> {
    type Item = &'a T;
    type IntoIter = core::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter { self.0.iter() }
}

impl<T: Ord> TryFrom<Vec<T>> for OrderedSet<T> {
    type Error = DeserializeError;

    fn try_from(vec: Vec<T>) -> Result<Self, Self::Error> {
        scan_unique(&vec)?;
        Ok(Self(vec))
    }
}

impl<T> From<OrderedSet<T>> for Vec<T> {
    fn from(v: OrderedSet<T>) -> Self {
        v.0
    }
}

impl<T> AsRef<[T]> for OrderedSet<T> {
    fn as_ref(&self) -> &[T] {
        self.0.as_slice()
    }
}

impl<T> core::ops::Index<usize> for OrderedSet<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<'a, T> IntoIterator for &'a OrderedSet<T> {
    type Item = &'a T;
    type IntoIter = core::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

/// std SET `Extend`: a duplicate is IGNORED (keep-first), the same semantics `HashSet`/`IndexSet`
/// settled decades ago — so `dst.extend(src)` is a set union. For duplicate-is-a-bug strictness on a
/// single element use [`OrderedSet::push`] instead; `Extend` never surfaces the duplicate.
impl<T: PartialEq> Extend<T> for OrderedSet<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for value in iter {
            self.insert(value);
        }
    }
}

/// std SET `FromIterator`: collect DEDUPS keep-first (`IndexSet::from_iter` semantics). This is the
/// order-preserving normalizing conversion — `vec.into_iter().collect::<OrderedSet<_>>()` — and, for
/// the non-empty twin, `…collect::<OrderedSet<_>>().try_into()` composes it with the min-1 refinement
/// door. There is deliberately no `FromIterator` on `NonEmptyOrderedSet` (an empty iterator is
/// unrepresentable there).
impl<T: PartialEq> FromIterator<T> for OrderedSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut set = Self::new();
        set.extend(iter);
        set
    }
}

/// A `Vec<T>` guaranteed to hold at least one element AND no duplicates — the uniqueness twin of
/// `NonEmptyVec<T>` for a `[+ T]` collection rule carrying `@duplicates reject`.
///
/// Its single `TryFrom<Vec<T>>` door composes BOTH invariants: it fails with the same
/// `DeserializeFailure::RangeCheck { min: Some(1), .. }` as `NonEmptyVec` for an empty input, and the
/// same `DuplicateKey(Key::Uint(i))` (duplicate's INDEX) as `OrderedSet` for a repeated element.
/// Order-preserving and never sorted, for the same byte-exact-round-trip reason as `OrderedSet`.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonEmptyOrderedSet<T>(Vec<T>);

impl<T> NonEmptyOrderedSet<T> {
    /// Construct from a single element — always valid (length 1, trivially unique).
    pub fn new(first: T) -> Self {
        Self(vec![first])
    }

    /// Append an element, unless it is already present (uniqueness). Growing can never violate the
    /// `>= 1` lower bound, so the only failure is a duplicate. The would-be index reported here agrees
    /// with the `TryFrom<Vec<T>>` door's for the same duplicate (see `OrderedSet::push`). This is the
    /// strict, duplicate-is-a-bug door; for the std set contract use [`insert`](Self::insert).
    pub fn push(&mut self, value: T) -> Result<(), DeserializeError>
    where
        T: PartialEq,
    {
        if self.0.contains(&value) {
            return Err(DeserializeFailure::DuplicateKey(Key::Uint(self.0.len() as u64)).into());
        }
        self.0.push(value);
        Ok(())
    }

    /// Insert an element, returning `true` if newly added and `false` if already present (std set
    /// contract). Growing can never break the `>= 1` bound, so this is infallible even though the
    /// strict [`push`](Self::push) reports a duplicate as an error.
    pub fn insert(&mut self, value: T) -> bool
    where
        T: PartialEq,
    {
        if self.0.contains(&value) {
            false
        } else {
            self.0.push(value);
            true
        }
    }

    /// Whether the set already contains `value` (std set contract).
    pub fn contains(&self, value: &T) -> bool
    where
        T: PartialEq,
    {
        self.0.contains(value)
    }

    /// Sort the set in place by element `Ord` (`IndexSet::sort` precedent). Sorting can create neither
    /// a duplicate nor emptiness, so both invariants hold; it CHANGES the re-emitted byte order (the
    /// wire-order round-trip guarantee applies to an UNTOUCHED decoded set).
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.0.sort();
    }

    /// The empty-means-absent constructor for an optional non-empty set field: an empty input is
    /// `Ok(None)` (the field is absent — the min-1 `RangeCheck` deliberately does NOT fire), and a
    /// non-empty input goes through the `TryFrom<Vec<T>>` door (uniqueness) wrapped in `Some`. Only the
    /// duplicate failure surfaces as `Err`.
    pub fn try_opt_from(vec: Vec<T>) -> Result<Option<Self>, DeserializeError>
    where
        T: Ord,
    {
        if vec.is_empty() {
            Ok(None)
        } else {
            Self::try_from(vec).map(Some)
        }
    }

    /// Remove and return the last element, unless doing so would empty the collection (refused at
    /// length 1 so the non-emptiness invariant can never break). Removal cannot create a duplicate.
    pub fn pop(&mut self) -> Result<T, DeserializeError> {
        if self.0.len() <= 1 {
            return Err(DeserializeFailure::RangeCheck { found: 0, min: Some(1), max: None }.into());
        }
        Ok(self.0.pop().unwrap())
    }

    /// Remove and return the element at `index`, unless doing so would empty the collection.
    pub fn remove(&mut self, index: usize) -> Result<T, DeserializeError> {
        if self.0.len() <= 1 {
            return Err(DeserializeFailure::RangeCheck { found: 0, min: Some(1), max: None }.into());
        }
        Ok(self.0.remove(index))
    }

    /// Number of elements. Never zero.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Always `false` — a `NonEmptyOrderedSet` holds at least one element by construction. Present so
    /// the `len`/`is_empty` pair stays consistent (and clippy-clean).
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.0.get(index)
    }

    /// The first element — always present.
    pub fn first(&self) -> &T {
        &self.0[0]
    }

    pub fn iter(&self) -> core::slice::Iter<'_, T> {
        self.0.iter()
    }

    pub fn as_slice(&self) -> &[T] {
        self.0.as_slice()
    }

    /// Escape hatch to the loose `Vec<T>` — mutate freely, then `try_into` back through the door.
    pub fn into_inner(self) -> Vec<T> {
        self.0
    }
}

impl<T: Ord> TryFrom<Vec<T>> for NonEmptyOrderedSet<T> {
    type Error = DeserializeError;

    fn try_from(vec: Vec<T>) -> Result<Self, Self::Error> {
        if vec.is_empty() {
            return Err(DeserializeFailure::RangeCheck { found: 0, min: Some(1), max: None }.into());
        }
        scan_unique(&vec)?;
        Ok(Self(vec))
    }
}

impl<T> From<NonEmptyOrderedSet<T>> for Vec<T> {
    fn from(v: NonEmptyOrderedSet<T>) -> Self {
        v.0
    }
}

impl<T> AsRef<[T]> for NonEmptyOrderedSet<T> {
    fn as_ref(&self) -> &[T] {
        self.0.as_slice()
    }
}

impl<T> core::ops::Index<usize> for NonEmptyOrderedSet<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<'a, T> IntoIterator for &'a NonEmptyOrderedSet<T> {
    type Item = &'a T;
    type IntoIter = core::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

/// std SET `Extend`: a duplicate is IGNORED (keep-first), same as `OrderedSet`; growing can never
/// break the `>= 1` bound. `dst.extend(src)` is a union.
impl<T: PartialEq> Extend<T> for NonEmptyOrderedSet<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for value in iter {
            self.insert(value);
        }
    }
}

/// The refinement door OrderedSet → NonEmptyOrderedSet: elements are ALREADY unique, so the only check
/// is non-emptiness — an empty `OrderedSet` fails with the same `RangeCheck { min: Some(1), .. }`
/// (`0 not at least 1`) as every other min-1 door.
impl<T> TryFrom<OrderedSet<T>> for NonEmptyOrderedSet<T> {
    type Error = DeserializeError;

    fn try_from(set: OrderedSet<T>) -> Result<Self, Self::Error> {
        if set.0.is_empty() {
            return Err(DeserializeFailure::RangeCheck { found: 0, min: Some(1), max: None }.into());
        }
        Ok(Self(set.0))
    }
}

/// The widening door NonEmptyOrderedSet → OrderedSet: dropping the min-1 refinement is infallible
/// (uniqueness is retained, and every non-empty set is a valid possibly-empty set).
impl<T> From<NonEmptyOrderedSet<T>> for OrderedSet<T> {
    fn from(set: NonEmptyOrderedSet<T>) -> Self {
        OrderedSet(set.0)
    }
}

#[cfg(test)]
mod bounded_ordered_set_tests {
    use super::*;

    #[test]
    fn bounded_unique_door_and_mutators_preserve_both_invariants() {
        let duplicate = BoundedOrderedSet::<u64, 2, 5>::try_from(vec![7, 7]).unwrap_err();
        assert!(matches!(duplicate.failure(), DeserializeFailure::DuplicateKey(Key::Uint(1))));
        BoundedOrderedSet::<u64, 2, 5>::try_from(vec![1]).unwrap_err();
        BoundedOrderedSet::<u64, 2, 5>::try_from(vec![1, 2, 3, 4, 5, 6]).unwrap_err();
        let mut value = BoundedOrderedSet::<u64, 2, 5>::try_from(vec![1, 2]).unwrap();
        value.pop().unwrap_err();
        assert!(value.push(2).is_err());
        assert_eq!(value.as_slice(), [1, 2]);
        value.push(3).unwrap();
        value.push(4).unwrap();
        value.push(5).unwrap();
        assert!(value.push(6).is_err());
        assert_eq!(value.as_slice(), [1, 2, 3, 4, 5]);
    }

    #[test]
    fn zero_seed_and_invalid_window_are_checked_doors() {
        assert!(BoundedOrderedSet::<u64, 0, 0>::new().is_empty());
        BoundedOrderedSet::<u64, 2, 1>::try_from(vec![1, 2]).unwrap_err();
    }
}
