
/// The JSON Schema of an OPEN TABLE (`t = { * K_t => V_t, * K_r => V_r }`): one open object whose
/// members may hold either row's range.
///
/// The two subschemas are passed in already resolved rather than turbofished as type parameters,
/// because a range may be CDDL `any` — whose published schema is the permissive natural rendering's
/// (`natural_any_cbor_schema`), not `AnyCbor`'s own tagged codec schema. The caller picks per region
/// and this stays one helper.
///
/// It asks nothing of EITHER key type, which is not a convenience but the contract: an open table's
/// member names are `K_t`'s serde image and `K_r`'s key image, neither of which any `K`-derived
/// schema describes (see `general_key_rest_map_schema`, whose reasoning this shares). Naming the
/// keys would also make a `@no_json_schema_export` key type an `E0277` inside a generated file — the
/// exemption the rest row's KEY domain already enjoys, extended to both of an open table's.
/// Two rows whose ranges publish the SAME schema collapse to that schema rather than an
/// `anyOf` of it with itself: the union states nothing the branch does not, and a reader (human or
/// json2ts) reads the duplicated branch as a mistake. A pure function of the two rendered
/// subschemas, so it stays deterministic.
pub fn open_table_schema(
    typed_range: schemars::Schema,
    captured_range: schemars::Schema,
) -> schemars::Schema {
    if typed_range == captured_range {
        return schemars::json_schema!({
            "type": "object",
            "additionalProperties": (typed_range.to_value()),
        });
    }
    schemars::json_schema!({
        "type": "object",
        "additionalProperties": {
            "anyOf": [typed_range.to_value(), captured_range.to_value()],
        },
    })
}
