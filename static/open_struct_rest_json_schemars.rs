
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

/// The JSON Schema of a captured rest row's flattened open region whose key domain is a GENERAL
/// typed `K` — an open object over the range, naming no constraint on the member names at all.
///
/// `BTreeMap<K, V>` is the wrong instrument here, and deliberately not reused: schemars derives a
/// map's member-name constraint from `K`'s own VALUE schema, while a rest row's member names are
/// `K`'s key-string IMAGE (text verbatim, uint/nint decimal). The two coincide only for the
/// primitive `uint`/`text` domains — which keep [`typed_rest_map_schema`] for exactly that reason.
/// For anything else the `BTreeMap` schema is wrong in both directions: a `K` whose schema is a
/// PATTERNED string publishes `patternProperties` over a pattern that describes `K`'s values rather
/// than its key images, plus `additionalProperties: false`, which CLOSES the open region the rest row
/// exists to advertise. The exact admissible member-name set (decimal spellings plus `K`'s admissible
/// texts) has no JSON-Schema expression, so an open object is the honest answer.
///
/// It also asks nothing of `K`: a `@no_json_schema_export` extern key would make a `K`-bounded helper
/// an `E0277` inside a generated file.
pub fn general_key_rest_map_schema<V: schemars::JsonSchema>(
    generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    schemars::json_schema!({
        "type": "object",
        "additionalProperties": (generator.subschema_for::<V>().to_value()),
    })
}
