impl<T: schemars::JsonSchema> schemars::JsonSchema for OrderedSet<T> {
    fn schema_name() -> ::std::borrow::Cow<'static, str> {
        format!("OrderedSet<{}>", T::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // shape matches the loose Vec (a JSON array); the uniqueness invariant is enforced at TryFrom.
        // A `uniqueItems: true` refinement rides the JSON-parity work packet.
        Vec::<T>::json_schema(generator)
    }
    fn inline_schema() -> bool {
        Vec::<T>::inline_schema()
    }
}

impl<T: schemars::JsonSchema> schemars::JsonSchema for NonEmptyOrderedSet<T> {
    fn schema_name() -> ::std::borrow::Cow<'static, str> {
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
