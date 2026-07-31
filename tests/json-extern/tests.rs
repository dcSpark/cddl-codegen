#[cfg(test)]
mod tests {
    use super::*;
    use crate::serialization::{Deserialize, ToCBORBytes};
    use cbor_event::de::Deserializer;

    // Round-trips `big_thing = [e: my_extern, s: my_set]`, where `my_set = ext_set<uint>` is the
    // concrete generic-extern instance `ExtSet<u64>`. The json-gen crate (built + run by the
    // harness) is what proves the schema rows compile: the generic BASE row is skipped, while the
    // plain extern (`MyExtern`) and the instance (`MySet`) rows compile against the hand-written
    // `schemars::JsonSchema` impls.
    // `@no_json_schema_export` compile proof. `NoSchemaExtern` has no `schemars::JsonSchema` impl,
    // so the json-gen crate (built + run by the harness) only compiles because the directive
    // suppressed its schema-registration row — and `HandJsonParent`'s row is suppressed too, which is
    // what lets `@custom_json` drop the derives over a field type that has no schema. The assertion
    // is the harness's json-gen build; this round-trip pins that the CBOR side is untouched by the
    // directive.
    #[test]
    fn hand_json_parent_round_trip() {
        let orig = HandJsonParent::new(NoSchemaExtern::new(7));
        let bytes = orig.to_cbor_bytes();
        let deser = HandJsonParent::deserialize(&mut Deserializer::from(bytes.clone())).unwrap();
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
    }

    // A `@no_json_schema_export` extern as a rest row's KEY domain, under --json-schema-export. The
    // json-gen build is again the compile proof (a K-bounded schema helper would be an E0277 there);
    // this pins the resulting shape: the flattened region is an OPEN object over the RANGE, and the
    // key type contributes nothing to the document at all — which is exactly why the helper can ask
    // nothing of K.
    #[test]
    fn no_json_schema_export_key_publishes_an_open_object() {
        let schema = serde_json::to_value(schemars::schema_for!(NoSchemaKeyRest)).unwrap();
        assert_eq!(
            schema
                .pointer("/additionalProperties/type")
                .and_then(serde_json::Value::as_str),
            Some("integer"),
            "the open region is typed by the range: {schema}"
        );
        assert!(
            schema.pointer("/properties/key_1").is_some(),
            "the declared field survives the flatten merge: {schema}"
        );
        assert!(
            !serde_json::to_string(&schema).unwrap().contains("NoSchemaExtern"),
            "the key type must not be referenced by the published schema: {schema}"
        );
    }

    #[test]
    fn big_thing_round_trip() {
        let orig = BigThing::new(MyExtern::new(42), ExtSet::new(vec![1u64, 2, 3]));
        let bytes = orig.to_cbor_bytes();
        let deser = BigThing::deserialize(&mut Deserializer::from(bytes.clone())).unwrap();
        assert_eq!(orig.to_cbor_bytes(), deser.to_cbor_bytes());
    }
}
