
/// The JSON Schema of a captured rest row's flattened open region, for a TYPED (non-`any`) range —
/// pointed at by the rest field's `#[schemars(schema_with = …)]` with the row's key and value types
/// turbofished in.
///
/// Parity contract: a rest row's flattened JSON is produced by the GENERATED flatten fns, which
/// render the same object for every rest container, so its schema belongs to the rest-row position
/// (key domain × value type) and not to whichever container holds the entries. Delegating to
/// `BTreeMap<K, V>` is that schema — it is what the loose container (`BTreeMap` /
/// `OrderedHashMap`, which forwards here too) contributes for the same K and V, so a
/// `@duplicates preserve` row and its non-preserve twin are schema-indistinguishable by
/// construction: uint keys yield `patternProperties "^\d+$"`, text keys `additionalProperties`.
pub fn typed_rest_map_schema<K: schemars::JsonSchema, V: schemars::JsonSchema>(
    generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    <alloc::collections::BTreeMap<K, V> as schemars::JsonSchema>::json_schema(generator)
}
