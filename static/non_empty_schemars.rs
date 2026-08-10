impl<T: schemars::JsonSchema> schemars::JsonSchema for NonEmptyVec<T> {
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        format!("NonEmptyVec<{}>", T::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = Vec::<T>::json_schema(generator);
        schema.insert("minItems".to_owned(), 1.into());
        schema
    }
    fn inline_schema() -> bool {
        Vec::<T>::inline_schema()
    }
}
