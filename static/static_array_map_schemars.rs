// Recursive schemas for object-shaped maps. Start from the key's ordinary map schema so its JSON
// member-name rule stays intact, then replace the value slot without asking schemars for `V`.
fn recursive_object_map_schema<K, V, Inner>(generator: &mut schemars::SchemaGenerator) -> schemars::Schema
where
    K: schemars::JsonSchema,
    Inner: RecursiveSchema<V>,
{
    let mut schema = <alloc::collections::BTreeMap<K, ()> as schemars::JsonSchema>::json_schema(generator);
    let value = Inner::schema(generator).to_value();
    if let Some(patterns) = schema
        .get_mut("patternProperties")
        .and_then(|patterns| patterns.as_object_mut())
    {
        for schema in patterns.values_mut() {
            *schema = value.clone();
        }
    } else {
        schema.insert("additionalProperties".to_owned(), value);
    }
    schema
}

impl<Inner, K, V> RecursiveSchema<alloc::collections::BTreeMap<K, V>> for Map<Inner>
where K: Ord + schemars::JsonSchema, Inner: RecursiveSchema<V> {
    fn schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        recursive_object_map_schema::<K, V, Inner>(generator)
    }
}
