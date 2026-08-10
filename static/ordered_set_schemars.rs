impl<T: schemars::JsonSchema> schemars::JsonSchema for OrderedSet<T> {
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        format!("OrderedSet<{}>", T::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // shape matches the loose Vec (a JSON array); the uniqueness invariant is enforced at TryFrom.
        // Deliberately NOT refined with `uniqueItems: true`: the sibling NonEmptyVec schemars impl sets
        // the convention of delegating to the Vec schema without invariant refinements (its `>= 1`
        // bound is likewise not surfaced as `minItems`) — the door is the single source of the
        // invariant, and json2ts emits the same array type either way.
        Vec::<T>::json_schema(generator)
    }
    fn inline_schema() -> bool {
        Vec::<T>::inline_schema()
    }
}

impl<T: schemars::JsonSchema> schemars::JsonSchema for NonEmptyOrderedSet<T> {
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        format!("NonEmptyOrderedSet<{}>", T::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // shape matches the loose Vec; uniqueness + non-emptiness are enforced at TryFrom.
        Vec::<T>::json_schema(generator)
    }
    fn inline_schema() -> bool {
        Vec::<T>::inline_schema()
    }
}

impl<T: schemars::JsonSchema, const MIN: u64, const MAX: u64> schemars::JsonSchema
    for BoundedOrderedSet<T, MIN, MAX>
{
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        format!("BoundedOrderedSet<{}, {MIN}, {MAX}>", T::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = Vec::<T>::json_schema(generator);
        schema.insert("uniqueItems".to_owned(), true.into());
        schema.insert("minItems".to_owned(), MIN.into());
        if MAX != u64::MAX { schema.insert("maxItems".to_owned(), MAX.into()); }
        schema
    }
    fn inline_schema() -> bool { Vec::<T>::inline_schema() }
}
