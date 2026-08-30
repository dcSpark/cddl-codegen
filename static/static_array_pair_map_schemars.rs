fn recursive_pair_map_schema<K, V, Key, Value>(generator: &mut schemars::SchemaGenerator) -> schemars::Schema
where Key: RecursiveSchema<K>, Value: RecursiveSchema<V> {
    let key = Key::schema(generator);
    let value = Value::schema(generator);
    let pair = schemars::json_schema!({
        "type": "array",
        "items": [key.to_value(), value.to_value()],
        "minItems": 2,
        "maxItems": 2,
    });
    schemars::json_schema!({ "type": "array", "items": pair.to_value() })
}
impl<Key, Value, K, V> RecursiveSchema<super::pair_map::PairMap<K, V>> for PairMap<Key, Value>
where Key: RecursiveSchema<K>, Value: RecursiveSchema<V> {
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema { recursive_pair_map_schema::<K, V, Key, Value>(generator) }
}
impl<Key, Value, K, V> RecursiveSchema<super::pair_map::NonEmptyPairMap<K, V>> for NonEmptyPairMap<Key, Value>
where Key: RecursiveSchema<K>, Value: RecursiveSchema<V> {
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = recursive_pair_map_schema::<K, V, Key, Value>(generator);
        schema.insert("minItems".to_owned(), 1.into()); schema
    }
}
impl<Key, Value, K, V, const MIN: u64, const MAX: u64>
    RecursiveSchema<super::pair_map::BoundedPairMap<K, V, MIN, MAX>> for BoundedPairMap<Key, Value, MIN, MAX>
where Key: RecursiveSchema<K>, Value: RecursiveSchema<V> {
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        let mut schema = recursive_pair_map_schema::<K, V, Key, Value>(generator);
        schema.insert("minItems".to_owned(), MIN.into());
        if MAX != u64::MAX { schema.insert("maxItems".to_owned(), MAX.into()); } schema
    }
}
