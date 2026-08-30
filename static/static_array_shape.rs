// Shared marker vocabulary for the recursive exact-array JSON descriptor. This fragment carries
// no serde/schemars reference, so schema-only and serde-only runtime assemblies name identical
// shapes without dragging the other optional dependency into the crate.
pub struct Leaf;
pub struct NaturalAny;
pub struct Exact<Inner, const N: usize>(core::marker::PhantomData<Inner>);
pub struct Loose<Inner>(core::marker::PhantomData<Inner>);
pub struct NonEmpty<Inner>(core::marker::PhantomData<Inner>);
pub struct Bounded<Inner, const MIN: u64, const MAX: u64>(core::marker::PhantomData<Inner>);
// `@duplicates reject` has three distinct checked carriers. Exact reject windows deliberately
// use RejectSetBounded rather than Exact: `[T; N]` cannot retain the uniqueness invariant.
pub struct RejectSet<Inner>(core::marker::PhantomData<Inner>);
pub struct RejectSetNonEmpty<Inner>(core::marker::PhantomData<Inner>);
pub struct RejectSetBounded<Inner, const MIN: u64, const MAX: u64>(core::marker::PhantomData<Inner>);
// Object maps adapt their VALUE only: JSON member names retain the native map-key convention.
// Pair maps are positional `[[key, value], …]`, so both descriptors participate.
pub struct Map<Value>(core::marker::PhantomData<Value>);
pub struct NonEmptyMap<Value>(core::marker::PhantomData<Value>);
pub struct BoundedMap<Value, const MIN: u64, const MAX: u64>(core::marker::PhantomData<Value>);
pub struct PairMap<Key, Value>(core::marker::PhantomData<(Key, Value)>);
pub struct NonEmptyPairMap<Key, Value>(core::marker::PhantomData<(Key, Value)>);
pub struct BoundedPairMap<Key, Value, const MIN: u64, const MAX: u64>(core::marker::PhantomData<(Key, Value)>);
pub struct Optional<Inner>(core::marker::PhantomData<Inner>);
