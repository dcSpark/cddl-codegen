// Appended to any_cbor.rs only when bounded.rs is present. This is the natural JSON schema for a
// BoundedVec<AnyCbor, MIN, MAX>: arbitrary JSON values as items, but the exact CDDL occurrence
// window is still visible to schema consumers.
pub fn natural_any_cbor_bounded_seq_schema<const MIN: u64, const MAX: u64>(
    _generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    let mut schema = schemars::json_schema!({ "type": "array", "items": {} });
    schema.insert("minItems".to_owned(), MIN.into());
    if MAX != u64::MAX {
        schema.insert("maxItems".to_owned(), MAX.into());
    }
    schema
}
