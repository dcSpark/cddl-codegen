// Open struct-map (loose CBOR "rest row") FLATTENED-JSON end-to-end vectors.
// The captured rest entries render at the SAME JSON object level as the declared fields (serde
// `flatten`); to_json is fallible on data (R3: a complex `any` key/value, or a rest key colliding
// with a declared field's JSON name, errors — never a silent substitute/duplicate); from_json is
// symmetric (declared names bind first, every other key lands in rest — loose JSON parsing for free).
#[cfg(test)]
mod open_struct_map_json {
    use super::*;
    use crate::generated::any_cbor::AnyCbor;

    #[test]
    fn foo_flatten_round_trips() {
        // Foo = { 1: uint, 2: text, * uint => any }: rest keys sit at the top object level.
        let mut foo = Foo::new(7, "hello".to_string());
        foo.rest.insert(99, AnyCbor::new_uint(5));
        foo.rest.insert(100, AnyCbor::new_text("world".to_string()));
        let json = serde_json::to_string(&foo).unwrap();
        assert!(json.contains("\"key_1\":7"), "declared field: {json}");
        assert!(json.contains("\"99\":5"), "rest uint key flattened: {json}");
        assert!(json.contains("\"100\":\"world\""), "rest text value flattened: {json}");
        // from_json -> to_json recovers the same JSON (value-equal modulo encodings, R4 numeric key).
        let back: Foo = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
    }

    #[test]
    fn from_json_binds_declared_first_rest_gets_remainder() {
        // Loose JSON parsing: declared names bind first, every other key -> rest.
        let json = r#"{"key_1":1,"key_2":"hi","42":7,"43":8}"#;
        let foo: Foo = serde_json::from_str(json).unwrap();
        assert_eq!(foo.key_1, 1);
        assert_eq!(foo.key_2, "hi");
        assert_eq!(foo.rest.get(&42), Some(&AnyCbor::new_uint(7)));
        assert_eq!(foo.rest.get(&43), Some(&AnyCbor::new_uint(8)));
    }

    #[test]
    fn declared_name_collision_errors_on_write() {
        // A rest key stringifying to a declared field's JSON name would shadow it -> to_json errors
        // (R3). Baz's `any` key domain lets a TEXT key "key_1" collide with the declared `key_1`.
        let mut baz = Baz::new(1);
        baz.rest
            .insert(AnyCbor::new_text("key_1".to_string()), AnyCbor::new_uint(9));
        let r = serde_json::to_string(&baz);
        let e = r.expect_err("a rest key colliding with a declared JSON name must error");
        assert!(
            format!("{e}").contains("declared field"),
            "the error must name the declared-field collision, got: {e}"
        );
    }

    #[test]
    fn any_key_domain_round_trips_and_complex_key_errors() {
        // Baz = { 1: uint, * any => any }: text/uint keys stringify (R3); a bytes key has no natural
        // JSON key form -> to_json errors.
        let mut baz = Baz::new(3);
        baz.rest
            .insert(AnyCbor::new_text("aaa".to_string()), AnyCbor::new_uint(1));
        baz.rest
            .insert(AnyCbor::new_uint(12), AnyCbor::new_text("v".to_string()));
        let json = serde_json::to_string(&baz).unwrap();
        assert!(json.contains("\"aaa\":1"), "any text key: {json}");
        assert!(json.contains("\"12\":\"v\""), "any uint key decimal: {json}");
        // R4: the JSON text key "12" prefers the numeric reading -> uint 12.
        let back: Baz = serde_json::from_str(&json).unwrap();
        assert_eq!(
            back.rest.get(&AnyCbor::new_uint(12)),
            Some(&AnyCbor::new_text("v".to_string()))
        );

        let mut bad_key = Baz::new(1);
        bad_key
            .rest
            .insert(AnyCbor::new_bytes(vec![1, 2, 3]), AnyCbor::new_uint(0));
        assert!(
            serde_json::to_string(&bad_key).is_err(),
            "a complex (bytes) any key must error on to_json"
        );
    }

    #[test]
    fn bytes_valued_any_rest_errors_on_write() {
        // A bytes-VALUED any rest entry has no natural JSON image -> to_json errors (the natural walk, RFC 8949 §6.1).
        let mut baz = Baz::new(1);
        baz.rest
            .insert(AnyCbor::new_uint(5), AnyCbor::new_bytes(vec![0xde, 0xad]));
        assert!(
            serde_json::to_string(&baz).is_err(),
            "a bytes value must error on to_json"
        );
    }

    #[test]
    fn typed_rest_round_trips() {
        // Typed = { 1: uint, * uint => text }: fully typed, deterministic both ways.
        let mut t = Typed::new(1);
        t.rest.insert(50, "fifty".to_string());
        let json = serde_json::to_string(&t).unwrap();
        assert!(json.contains("\"50\":\"fifty\""), "typed rest flattened: {json}");
        let back: Typed = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
    }

    #[test]
    fn pair_map_duplicates_error_on_write() {
        // The @duplicates preserve PairMap twin: duplicate keys stringify identically, so a pair-list
        // rest holding ACTUAL duplicates makes to_json error via the two-rest-key collision check
        // (loud, data-dependent) — while a no-duplicate pair-list flattens fine.
        let mut ok = Dupp::new(1);
        ok.rest.insert(7, AnyCbor::new_uint(1));
        ok.rest.insert(8, AnyCbor::new_uint(2));
        let json = serde_json::to_string(&ok).unwrap();
        assert!(
            json.contains("\"7\":1") && json.contains("\"8\":2"),
            "a no-duplicate pair-list flattens fine: {json}"
        );

        let mut dup = Dupp::new(1);
        dup.rest.insert(7, AnyCbor::new_uint(1));
        dup.rest.insert(7, AnyCbor::new_uint(2)); // ACTUAL duplicate key 7
        let e = serde_json::to_string(&dup)
            .expect_err("duplicate pair-list keys must make to_json error");
        assert!(
            format!("{e}").contains("stringify identically"),
            "the error must name the identical-key collision, got: {e}"
        );
    }
}
