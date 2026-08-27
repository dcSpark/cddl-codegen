// Appended only when ordered_set.rs is present. These schemas describe the JSON list boundary,
// not the carrier's opaque Rust representation, and retain its uniqueness/range invariants.
fn reject_set_schema<Inner, T>(generator: &mut schemars::SchemaGenerator) -> schemars::Schema
where
    Inner: RecursiveSchema<T>,
{
    let item = Inner::schema(generator);
    schemars::json_schema!({ "type": "array", "items": item.to_value(), "uniqueItems": true })
}

impl<Inner, T> RecursiveSchema<super::ordered_set::OrderedSet<T>> for RejectSet<Inner>
where
    Inner: RecursiveSchema<T>,
{
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        self::reject_set_schema::<Inner, T>(generator)
    }
}

impl<Inner, T> RecursiveSchema<super::ordered_set::NonEmptyOrderedSet<T>> for RejectSetNonEmpty<Inner>
where
    Inner: RecursiveSchema<T>,
{
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = self::reject_set_schema::<Inner, T>(generator);
        schema.insert("minItems".to_owned(), 1.into());
        schema
    }
}

impl<Inner, T, const MIN: u64, const MAX: u64>
    RecursiveSchema<super::ordered_set::BoundedOrderedSet<T, MIN, MAX>>
    for RejectSetBounded<Inner, MIN, MAX>
where
    Inner: RecursiveSchema<T>,
{
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = self::reject_set_schema::<Inner, T>(generator);
        schema.insert("minItems".to_owned(), MIN.into());
        if MAX != u64::MAX {
            schema.insert("maxItems".to_owned(), MAX.into());
        }
        schema
    }
}
