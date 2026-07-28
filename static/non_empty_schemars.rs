impl<T: schemars::JsonSchema> schemars::JsonSchema for NonEmptyVec<T> {
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        format!("NonEmptyVec<{}>", T::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // shape matches the loose Vec (a JSON array); the >= 1 invariant is enforced at TryFrom
        Vec::<T>::json_schema(generator)
    }
    fn inline_schema() -> bool {
        Vec::<T>::inline_schema()
    }
}
