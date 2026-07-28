impl<K: schemars::JsonSchema, V: schemars::JsonSchema> schemars::JsonSchema for PairMap<K, V> {
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        format!("PairMap<{}, {}>", K::schema_name(), V::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // shape matches the JSON representation (an array of `[k, v]` pairs), NOT an object: a
        // duplicate-permitting map has no object form. The `Vec<(K, V)>` schema is exactly that.
        Vec::<(K, V)>::json_schema(generator)
    }
    fn inline_schema() -> bool {
        Vec::<(K, V)>::inline_schema()
    }
}

impl<K: schemars::JsonSchema, V: schemars::JsonSchema> schemars::JsonSchema
    for NonEmptyPairMap<K, V>
{
    fn schema_name() -> alloc::borrow::Cow<'static, str> {
        format!("NonEmptyPairMap<{}, {}>", K::schema_name(), V::schema_name()).into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // shape matches the array-of-pairs JSON; the >= 1 invariant is enforced at TryFrom.
        Vec::<(K, V)>::json_schema(generator)
    }
    fn inline_schema() -> bool {
        Vec::<(K, V)>::inline_schema()
    }
}
