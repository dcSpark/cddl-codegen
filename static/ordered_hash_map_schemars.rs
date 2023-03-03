impl<K, V> schemars::JsonSchema for OrderedHashMap<K, V>
where
    K: Hash + Eq + Ord + schemars::JsonSchema,
    V: schemars::JsonSchema,
{
    fn schema_name() -> String {
        format!("OrderedHashMap<{}, {}>", K::schema_name(), V::schema_name())
    }
    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        core::collections::BTreeMap::<K, V>::json_schema(gen)
    }
    fn is_referenceable() -> bool {
        std::collections::BTreeMap::<K, V>::is_referenceable()
    }
}
