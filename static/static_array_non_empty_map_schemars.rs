impl<Inner, K, V> RecursiveSchema<super::non_empty_map::NonEmptyMap<K, V>> for NonEmptyMap<Inner>
where K: Ord + schemars::JsonSchema, Inner: RecursiveSchema<V> {
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = recursive_object_map_schema::<K, V, Inner>(generator);
        schema.insert("minProperties".to_owned(), 1.into()); schema
    }
}
