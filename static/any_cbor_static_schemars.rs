// Natural JSON schema for `[AnyCbor; N]`: arbitrary JSON values, exact cardinality.
pub fn natural_any_cbor_static_seq_schema<const N: usize>(
    _generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    let mut schema = schemars::json_schema!({ "type": "array", "items": {} });
    schema.insert("minItems".to_owned(), (N as u64).into());
    schema.insert("maxItems".to_owned(), (N as u64).into());
    schema
}

/// Optional natural `[AnyCbor; N]`: the serde adapter writes `None` as JSON null while `Some`
/// retains the exact array branch.
pub fn natural_any_cbor_opt_static_seq_schema<const N: usize>(
    _generator: &mut schemars::SchemaGenerator,
) -> schemars::Schema {
    let array = self::natural_any_cbor_static_seq_schema::<N>(_generator);
    schemars::json_schema!({
        "anyOf": [array.to_value(), { "type": "null" }],
    })
}
