impl<K, V> schemars::JsonSchema for OrderedHashMap<K, V> where
    K: Hash + Eq + Ord + schemars::JsonSchema,
    V: schemars::JsonSchema {
    fn schema_name() -> alloc::borrow::Cow<'static, str> { format!("OrderedHashMap<{}, {}>", K::schema_name(), V::schema_name()).into() }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        alloc::collections::BTreeMap::<K, V>::json_schema(generator)
    }
    fn inline_schema() -> bool { alloc::collections::BTreeMap::<K, V>::inline_schema() }
}

