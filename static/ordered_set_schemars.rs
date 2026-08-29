impl<T: schemars::JsonSchema> schemars::JsonSchema for OrderedSet<T> {
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        format!("OrderedSet<{}>", T::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // Shape matches the loose Vec (a JSON array), plus the uniqueness invariant the JSON
        // deserializer enforces through OrderedSet::try_from.
        let mut schema = Vec::<T>::json_schema(generator);
        schema.insert("uniqueItems".to_owned(), true.into());
        schema
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
        // Shape matches the loose Vec, plus the uniqueness and non-empty invariants the JSON
        // deserializer enforces through NonEmptyOrderedSet::try_from. NonEmptyVec follows this
        // same `minItems` convention.
        let mut schema = Vec::<T>::json_schema(generator);
        schema.insert("uniqueItems".to_owned(), true.into());
        schema.insert("minItems".to_owned(), 1.into());
        schema
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
