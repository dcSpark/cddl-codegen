impl<K: Ord + schemars::JsonSchema, V: schemars::JsonSchema> schemars::JsonSchema for NonEmptyMap<K, V> {
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        format!("NonEmptyMap<{}, {}>", K::schema_name(), V::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // Shape matches the loose map (a JSON object), plus the same non-empty door the
        // deserializer reaches through TryFrom.
        let mut schema = BTreeMap::<K, V>::json_schema(generator);
        schema.insert("minProperties".to_owned(), 1.into());
        schema
    }
    fn inline_schema() -> bool {
        BTreeMap::<K, V>::inline_schema()
    }
}
