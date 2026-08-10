impl<K: Ord + schemars::JsonSchema, V: schemars::JsonSchema, const MIN: u64, const MAX: u64> schemars::JsonSchema for BoundedMap<K, V, MIN, MAX> {
    fn schema_name() -> alloc::borrow::Cow<'static, str> { format!("BoundedMap<{}, {}, {MIN}, {MAX}>", K::schema_name(), V::schema_name()).into() }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = BTreeMap::<K, V>::json_schema(generator);
        schema.insert("minProperties".to_owned(), MIN.into());
        if MAX != u64::MAX { schema.insert("maxProperties".to_owned(), MAX.into()); }
        schema
    }
    fn inline_schema() -> bool { BTreeMap::<K, V>::inline_schema() }
}
