// Manual `schemars::JsonSchema` for `AnyCbor`, matching the `any_cbor_json.rs` rendering: a
// self-referential `oneOf` over the twelve single-key tagged-object forms. Precedent for a manual
// impl on a runtime type: `ordered_hash_map_schemars.rs`. The schema flows into the json-gen crate
// and through `run-json2ts.js`; the json2ts acceptance of this self-referential schema is a named
// pre-ship gate (DESIGN §6).
//
// Routed through `schemars::json_schema!` (which uses schemars' own bundled serde_json), so this
// compiles under `--json-schema-export` even when `--json-serde-derives` is off (serde_json is only
// a direct dep under the serde flag).
impl schemars::JsonSchema for AnyCbor {
    fn schema_name() -> ::std::borrow::Cow<'static, str> {
        "AnyCbor".into()
    }

    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // Self-reference for the recursive positions (array items, map pair elements, tag value).
        // `subschema_for::<AnyCbor>` registers AnyCbor in the generator's definitions and returns a
        // `$ref` schema. AnyCbor is mid-generation at this point, so the generator returns a ref
        // rather than inlining — `inline_schema` returning false guarantees the recursion terminates.
        let any = generator.subschema_for::<AnyCbor>().to_value();
        schemars::json_schema!({
            "oneOf": [
                { "type": "object", "additionalProperties": false, "required": ["uint"],
                  "properties": { "uint": { "type": "integer", "minimum": 0 } } },
                { "type": "object", "additionalProperties": false, "required": ["nint"],
                  "properties": { "nint": { "type": ["integer", "string"] } } },
                { "type": "object", "additionalProperties": false, "required": ["bytes"],
                  "properties": { "bytes": { "type": "string" } } },
                { "type": "object", "additionalProperties": false, "required": ["text"],
                  "properties": { "text": { "type": "string" } } },
                { "type": "object", "additionalProperties": false, "required": ["array"],
                  "properties": { "array": { "type": "array", "items": (any.clone()) } } },
                { "type": "object", "additionalProperties": false, "required": ["map"],
                  "properties": { "map": { "type": "array",
                      "items": { "type": "array", "minItems": 2, "maxItems": 2,
                          "prefixItems": [(any.clone()), (any.clone())] } } } },
                { "type": "object", "additionalProperties": false, "required": ["tag"],
                  "properties": { "tag": { "type": "array", "minItems": 2, "maxItems": 2,
                      "prefixItems": [ { "type": "integer", "minimum": 0 }, (any.clone()) ] } } },
                { "type": "object", "additionalProperties": false, "required": ["bool"],
                  "properties": { "bool": { "type": "boolean" } } },
                { "type": "object", "additionalProperties": false, "required": ["null"],
                  "properties": { "null": { "type": "null" } } },
                { "type": "object", "additionalProperties": false, "required": ["undefined"],
                  "properties": { "undefined": { "type": "null" } } },
                { "type": "object", "additionalProperties": false, "required": ["unassigned"],
                  "properties": { "unassigned": { "type": "integer", "minimum": 0, "maximum": 255 } } },
                { "type": "object", "additionalProperties": false, "required": ["float"],
                  "properties": { "float": { "type": ["number", "string"] } } }
            ]
        })
    }

    fn inline_schema() -> bool {
        false
    }
}

/// The `#[schemars(schema_with = "…::natural_any_cbor_schema")]` companion to the
/// `natural_any_cbor` serde adapter (loose-CBOR Phase B, ruling R1): the schema a GENERATED type
/// puts on an `any`-typed field/arm. It is the permissive "any JSON value" schema (an empty schema
/// accepts everything) because the natural rendering of an arbitrary CBOR value can be any JSON
/// value — json2ts turns it into TS `unknown`. This is deliberately DISTINCT from `AnyCbor`'s own
/// tagged `oneOf` schema above, which describes the tagged value-codec surface (the `AnyCbor` wasm
/// wrapper's `to_json`), and is unchanged (ruling R2).
pub fn natural_any_cbor_schema(_generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
    schemars::json_schema!({})
}

/// Companion to `natural_any_cbor_seq` (a `Vec<AnyCbor>` member, `[* any]`): an array whose items
/// are the permissive "any JSON value" schema.
pub fn natural_any_cbor_seq_schema(_generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
    schemars::json_schema!({ "type": "array", "items": {} })
}

/// Companion to `natural_any_cbor_btreemap` (a `{* K => any}` table member with a stringifiable
/// key): an object whose additional-property values are the permissive "any JSON value" schema.
pub fn natural_any_cbor_map_schema(_generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
    schemars::json_schema!({ "type": "object", "additionalProperties": {} })
}
