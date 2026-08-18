// Generic schema adapter for `[T; N]`, matching the serde static-array handover.
pub fn static_array_schema<T: schemars::JsonSchema, const N: usize>(
    generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    let mut schema = generator.subschema_for::<Vec<T>>();
    schema.insert("minItems".to_owned(), (N as u64).into());
    schema.insert("maxItems".to_owned(), (N as u64).into());
    schema
}

/// Optional direct `[T; N]`: serde writes `None` as JSON `null`, so the schema must retain the
/// exact-array branch and add a null branch rather than making the field look array-only.
pub fn static_array_opt_schema<T: schemars::JsonSchema, const N: usize>(
    generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    let array = self::static_array_schema::<T, N>(generator);
    schemars::json_schema!({
        "anyOf": [array.to_value(), { "type": "null" }],
    })
}

/// Schema for `Vec<[T; N]>`: the outer occurrence is unconstrained, while every inner array has
/// the exact native cardinality.
pub fn static_array_seq_schema<T: schemars::JsonSchema, const N: usize>(
    generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    let item = self::static_array_schema::<T, N>(generator);
    let mut schema = generator.subschema_for::<Vec<T>>();
    schema.insert("items".to_owned(), item.into());
    schema
}
