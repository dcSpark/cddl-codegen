impl<Inner, K, V, const MIN: u64, const MAX: u64>
    RecursiveSchema<super::bounded_map::BoundedMap<K, V, MIN, MAX>> for BoundedMap<Inner, MIN, MAX>
where K: Ord + schemars::JsonSchema, Inner: RecursiveSchema<V> {
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = recursive_object_map_schema::<K, V, Inner>(generator);
        schema.insert("minProperties".to_owned(), MIN.into());
        if MAX != u64::MAX { schema.insert("maxProperties".to_owned(), MAX.into()); } schema
    }
}
