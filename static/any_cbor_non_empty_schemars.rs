// Appended to any_cbor.rs only when non_empty.rs is present. This schema describes the NATURAL
// JSON form of a NonEmptyVec<AnyCbor> tail, not AnyCbor's tagged value codec.
pub fn natural_any_cbor_non_empty_seq_schema(
    _generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    schemars::json_schema!({ "type": "array", "items": {}, "minItems": 1 })
}
