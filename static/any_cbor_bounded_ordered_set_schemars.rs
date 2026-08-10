// Appended to any_cbor.rs only when ordered_set.rs is present. Natural JSON values are arbitrary,
// while the compound carrier additionally publishes both occurrence and uniqueness invariants.
pub fn natural_any_cbor_bounded_ordered_set_schema<const MIN: u64, const MAX: u64>(
    _generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    let mut schema = schemars::json_schema!({ "type": "array", "items": {}, "uniqueItems": true });
    schema.insert("minItems".to_owned(), MIN.into());
    if MAX != u64::MAX {
        schema.insert("maxItems".to_owned(), MAX.into());
    }
    schema
}
