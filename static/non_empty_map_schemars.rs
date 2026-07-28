impl<K: Ord + schemars::JsonSchema, V: schemars::JsonSchema> schemars::JsonSchema for NonEmptyMap<K, V> {
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        format!("NonEmptyMap<{}, {}>", K::schema_name(), V::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // shape matches the loose map (a JSON object); the >= 1 invariant is enforced at TryFrom
        BTreeMap::<K, V>::json_schema(generator)
    }
    fn inline_schema() -> bool {
        BTreeMap::<K, V>::inline_schema()
    }
}
