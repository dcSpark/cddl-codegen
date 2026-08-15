// Open struct-map (loose CBOR "rest row") FLATTENED-JSON end-to-end vectors.
// The captured rest entries render at the SAME JSON object level as the declared fields (serde
// `flatten`); to_json is fallible on data (R3: a complex `any` key/value, or a rest key colliding
// with a declared field's JSON name, errors — never a silent substitute/duplicate); from_json is
// symmetric (declared names bind first, every other key lands in rest — loose JSON parsing for free).
// The published SCHEMA of that flattened region belongs to the rest-row position, not to the
// container holding the entries, so the `@duplicates preserve` twin publishes its loose twin's
// schema (`typed_pair_map_rest_publishes_the_same_schema_as_its_loose_twin`).
#[cfg(test)]
mod open_struct_map_json {
    use super::*;
    use crate::generated::any_cbor::AnyCbor;

    #[test]
    fn foo_flatten_round_trips() {
        // Foo = { 1: uint, 2: text, * uint => any }: rest keys sit at the top object level.
        let mut foo = Foo::new(7, "hello".to_string());
        foo.insert_rest(99, AnyCbor::new_uint(5)).unwrap();
        foo.insert_rest(100, AnyCbor::new_text("world".to_string()))
            .unwrap();
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
        assert_eq!(foo.rest().get(&42), Some(&AnyCbor::new_uint(7)));
        assert_eq!(foo.rest().get(&43), Some(&AnyCbor::new_uint(8)));
    }

    #[test]
    fn declared_name_collision_errors_on_write() {
        // A rest key stringifying to a declared field's JSON name would shadow it -> to_json errors
        // (R3). Baz's `any` key domain lets a TEXT key "key_1" collide with the declared `key_1`.
        let mut baz = Baz::new(1);
        baz.insert_rest(
            AnyCbor::new_text("key_1".to_string()),
            AnyCbor::new_uint(9),
        )
        .unwrap();
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
        baz.insert_rest(
            AnyCbor::new_text("aaa".to_string()),
            AnyCbor::new_uint(1),
        )
        .unwrap();
        baz.insert_rest(
            AnyCbor::new_uint(12),
            AnyCbor::new_text("v".to_string()),
        )
        .unwrap();
        let json = serde_json::to_string(&baz).unwrap();
        assert!(json.contains("\"aaa\":1"), "any text key: {json}");
        assert!(json.contains("\"12\":\"v\""), "any uint key decimal: {json}");
        // R4: the JSON text key "12" prefers the numeric reading -> uint 12.
        let back: Baz = serde_json::from_str(&json).unwrap();
        assert_eq!(
            back.rest().get(&AnyCbor::new_uint(12)),
            Some(&AnyCbor::new_text("v".to_string()))
        );

        let mut bad_key = Baz::new(1);
        bad_key
            .insert_rest(
                AnyCbor::new_bytes(vec![1, 2, 3]),
                AnyCbor::new_uint(0),
            )
            .unwrap();
        assert!(
            serde_json::to_string(&bad_key).is_err(),
            "a complex (bytes) any key must error on to_json"
        );
    }

    #[test]
    fn bytes_valued_any_rest_errors_on_write() {
        // A bytes-VALUED any rest entry has no natural JSON image -> to_json errors (the natural walk, RFC 8949 §6.1).
        let mut baz = Baz::new(1);
        baz.insert_rest(
            AnyCbor::new_uint(5),
            AnyCbor::new_bytes(vec![0xde, 0xad]),
        )
        .unwrap();
        assert!(
            serde_json::to_string(&baz).is_err(),
            "a bytes value must error on to_json"
        );
    }

    #[test]
    fn typed_rest_round_trips() {
        // Typed = { 1: uint, * uint => text }: fully typed, deterministic both ways.
        let mut t = Typed::new(1);
        t.insert_rest(50, "fifty".to_string()).unwrap();
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
        ok.insert_rest(7, AnyCbor::new_uint(1)).unwrap();
        ok.insert_rest(8, AnyCbor::new_uint(2)).unwrap();
        let json = serde_json::to_string(&ok).unwrap();
        assert!(
            json.contains("\"7\":1") && json.contains("\"8\":2"),
            "a no-duplicate pair-list flattens fine: {json}"
        );

        let mut dup = Dupp::new(1);
        dup.insert_rest(7, AnyCbor::new_uint(1)).unwrap();
        dup.insert_rest(7, AnyCbor::new_uint(2)).unwrap(); // ACTUAL duplicate key 7
        let e = serde_json::to_string(&dup)
            .expect_err("duplicate pair-list keys must make to_json error");
        assert!(
            format!("{e}").contains("stringify identically"),
            "the error must name the identical-key collision, got: {e}"
        );
    }

    #[test]
    fn typed_pair_map_rest_publishes_the_same_schema_as_its_loose_twin() {
        // A rest row's flattened JSON is produced by the generated flatten fns, identically for every
        // container — so the published schema of the open region belongs to the rest-row position
        // (key domain x value type), not to the container. `Dupt` (@duplicates preserve, PairMap) and
        // `Typed` (loose container) declare the same fixed key and the same `* uint => text` row, so
        // their schemas must agree everywhere but the type name.
        let strip = |schema: schemars::Schema| {
            let mut value = serde_json::to_value(schema).unwrap();
            value.as_object_mut().unwrap().remove("title");
            value
        };
        let typed = strip(schemars::schema_for!(Typed));
        let dupt = strip(schemars::schema_for!(Dupt));
        assert_eq!(
            dupt, typed,
            "the PairMap-backed rest row must publish its loose twin's schema"
        );
        // The open region is there, keyed by the uint domain's decimal pattern, valued by the range.
        assert_eq!(
            dupt.pointer("/patternProperties/^\\d+$/type")
                .and_then(serde_json::Value::as_str),
            Some("string"),
            "the flattened rest region must be a uint-keyed index signature over the range: {dupt}"
        );
        // ...and none of the container's own array-of-pairs keywords leaked onto the object schema.
        for junk in ["items", "prefixItems"] {
            assert!(
                dupt.get(junk).is_none(),
                "the object schema must carry no `{junk}` keyword: {dupt}"
            );
        }
    }
}

// TYPED key domains on a flattened rest row: a rest key images through the `any` domain's convention
// applied to K's OWN CBOR bytes, and reads back numeric-first with a FALLBACK to the text reading.
// The contract these pin (T1-T4): the JSON fixed point is TOTAL for every K (T1); the CBOR/value
// fixed point holds except for a text key whose content is a canonical decimal AND whose K also
// admits the numeric reading (T2, the `any` domain's documented ambiguity, now scoped by K); to_json
// is loudly fallible on data (T3); and a member name every reading of K refuses is a hard parse
// error, never a capture (T4).
#[cfg(test)]
mod open_struct_map_typed_key_json {
    use super::*;
    use crate::generated::serialization::{Deserialize, ToCBORBytes};

    fn uint_key(x: u64) -> Md {
        Md::Int(Int::new_uint(x))
    }

    fn nint_key(x: u64) -> Md {
        Md::Int(Int::new_nint(x))
    }

    #[test]
    fn typed_key_kinds_round_trip_and_are_recoverable() {
        // (1) uint-, text- and nint-keyed entries all image (decimal / verbatim / decimal), the JSON
        // is a fixed point, and every key comes back as the same `Md` (T1 + T2).
        let mut t = Tkk::new(1);
        t.insert_rest(uint_key(9), Md::Text("nine".to_string()))
            .unwrap();
        t.insert_rest(nint_key(4), Md::Text("minus five".to_string()))
            .unwrap(); // CBOR nint -5
        t.insert_rest(Md::Text("word".to_string()), Md::Text("w".to_string()))
            .unwrap();
        let json = serde_json::to_string(&t).unwrap();
        assert!(json.contains("\"9\":"), "uint key images as decimal: {json}");
        assert!(json.contains("\"-5\":"), "nint key images as decimal: {json}");
        assert!(json.contains("\"word\":"), "text key images verbatim: {json}");
        let back: Tkk = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
        assert_eq!(
            back.rest().get(&uint_key(9)),
            Some(&Md::Text("nine".to_string()))
        );
        assert_eq!(
            back.rest().get(&nint_key(4)),
            Some(&Md::Text("minus five".to_string()))
        );
        assert_eq!(
            back.rest().get(&Md::Text("word".to_string())),
            Some(&Md::Text("w".to_string()))
        );
    }

    #[test]
    fn typed_values_ride_their_own_serde() {
        // (2) The decided posture: a typed RANGE renders through V's own derive (serde's
        // externally-tagged union form), NOT through the `any` domain's natural walk. This is the
        // anti-vector for "natural VALUE rendering".
        let mut t = Tkk::new(1);
        t.insert_rest(uint_key(9), Md::Text("hi".to_string()))
            .unwrap();
        let json = serde_json::to_string(&t).unwrap();
        assert!(
            json.contains("\"9\":{\"Text\":\"hi\"}"),
            "a typed value renders tagged, through its own serde: {json}"
        );

        // (3) ...which also means a `bytes` ARM of a union value renders as the derive's array of
        // byte numbers, not as the hex a typed bytes FIELD would use (a union arm never reaches the
        // bytes-field convention). Surprising but symmetric, and pinned so it cannot drift silently.
        let mut b = Tkk::new(1);
        b.insert_rest(uint_key(7), Md::Bytes(vec![0xde, 0xad]))
            .unwrap();
        let json = serde_json::to_string(&b).unwrap();
        assert!(
            json.contains("\"7\":{\"Bytes\":[222,173]}"),
            "a bytes union arm renders as the derive's array form: {json}"
        );
    }

    #[test]
    fn from_json_binds_declared_first_then_typed_rest() {
        // (4) Loose JSON parsing over a typed key domain: declared names bind first, every other
        // member lands in `rest`.
        let json = r#"{"key_1":3,"42":{"Text":"a"},"beta":{"Text":"b"}}"#;
        let t: Tkk = serde_json::from_str(json).unwrap();
        assert_eq!(t.key_1, 3);
        assert_eq!(t.rest().get(&uint_key(42)), Some(&Md::Text("a".to_string())));
        assert_eq!(
            t.rest().get(&Md::Text("beta".to_string())),
            Some(&Md::Text("b".to_string()))
        );
    }

    #[test]
    fn text_key_that_looks_numeric_rebinds_as_uint() {
        // (5) The `any` domain's documented ambiguity, inherited by a K that admits BOTH readings:
        // the member name "12" prefers the numeric reading, so the key's TYPE changes across
        // to_json -> from_json while the JSON stays a fixed point (T1 holds, T2's carve-out). The
        // write-side collision check is what keeps this bounded: a row can hold text "12" OR uint 12,
        // never both, so the rebinding never merges two entries.
        let mut t = Tkk::new(1);
        t.insert_rest(Md::Text("12".to_string()), Md::Text("v".to_string()))
            .unwrap();
        let json = serde_json::to_string(&t).unwrap();
        let back: Tkk = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
        assert_eq!(
            back.rest().get(&uint_key(12)),
            Some(&Md::Text("v".to_string()))
        );
        assert_eq!(back.rest().get(&Md::Text("12".to_string())), None);
    }

    #[test]
    fn non_canonical_decimal_member_name_stays_text() {
        // (6) Only a CANONICAL decimal spelling takes the numeric reading — the same filter the `any`
        // domain uses — so "012" is a text key in both directions.
        let mut t = Tkk::new(1);
        t.insert_rest(Md::Text("012".to_string()), Md::Text("v".to_string()))
            .unwrap();
        let json = serde_json::to_string(&t).unwrap();
        assert!(json.contains("\"012\":"), "{json}");
        let back: Tkk = serde_json::from_str(&json).unwrap();
        assert_eq!(
            back.rest().get(&Md::Text("012".to_string())),
            Some(&Md::Text("v".to_string()))
        );
    }

    #[test]
    fn text_only_key_domain_round_trips_a_numeric_member_name() {
        // (7) THE vector that pins the fallback ruling. `Tk` is a text-only K, so the numeric reading
        // of "12" is refused — under a strict prefer-numeric rule this is a parse error, i.e.
        // `to_json` would emit a document our own reader rejects. The fallback to the text reading is
        // what makes the JSON fixed point TOTAL (T1) for every K.
        let mut t = Tonly::new(1);
        t.insert_rest(Tk::new("12".to_string()), 5).unwrap();
        let json = serde_json::to_string(&t).unwrap();
        assert!(json.contains("\"12\":5"), "{json}");
        let back: Tonly = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
        assert_eq!(back.rest().get(&Tk::new("12".to_string())), Some(&5));
    }

    #[test]
    fn a_member_name_every_reading_refuses_is_a_parse_error() {
        // (8) T4 (refinement on read): `Uk` is uint-only, so "abc" has no reading at all -> a hard
        // parse error, never a capture. The message accounts for BOTH readings, so a uint-only K does
        // not read as if the numeric reading had never been tried.
        let e = serde_json::from_str::<Uonly>(r#"{"key_1":1,"abc":5}"#)
            .expect_err("a member name no reading of K admits must be a parse error");
        let msg = format!("{e}");
        assert!(
            msg.contains("is not a valid key")
                && msg.contains("as uint/nint:")
                && msg.contains("as text:"),
            "the error must account for both readings, got: {msg}"
        );
        // ...and the numeric reading IS taken when the name is a canonical decimal.
        let ok: Uonly = serde_json::from_str(r#"{"key_1":1,"7":5}"#).unwrap();
        assert_eq!(ok.rest().get(&Uk::new(7)), Some(&5));
    }

    #[test]
    fn a_member_name_outside_the_key_bounds_is_a_parse_error() {
        // (9) The JSON side of the CBOR refinement semantics: `Bt` is `text .size (1..4)`, so a
        // 5-character member name fails K's own deserialize and the whole parse fails — it is never
        // silently captured under some other reading.
        let e = serde_json::from_str::<Bonly>(r#"{"key_1":1,"abcde":5}"#)
            .expect_err("an out-of-bounds member name must be a parse error");
        assert!(
            format!("{e}").contains("is not a valid key"),
            "got: {e}"
        );
        let ok: Bonly = serde_json::from_str(r#"{"key_1":1,"abcd":5}"#).unwrap();
        assert_eq!(ok.rest().get(&Bt::new("abcd".to_string()).unwrap()), Some(&5));
    }

    #[test]
    fn a_key_with_no_member_name_image_errors_on_write() {
        // (10) T3: a key whose CBOR head is not uint/nint/text has no member-name image -> to_json
        // errors, NAMING the kind (no substitutes, RFC 8949 6.1).
        let mut t = Tkk::new(1);
        t.insert_rest(Md::Bytes(vec![1, 2, 3]), Md::Text("v".to_string()))
            .unwrap();
        let e = serde_json::to_string(&t).expect_err("a bytes-shaped key must error on to_json");
        assert!(
            format!("{e}").contains("Bytes"),
            "the error must name the CBOR kind, got: {e}"
        );
    }

    #[test]
    fn two_typed_keys_imaging_identically_error_on_write() {
        // (11) T3: uint 12 and text "12" are DIFFERENT CBOR keys with the SAME member-name image, so
        // the flattened object cannot hold both. Same rule (and same message) the `any` domain has.
        let mut t = Tkk::new(1);
        t.insert_rest(uint_key(12), Md::Text("a".to_string()))
            .unwrap();
        t.insert_rest(Md::Text("12".to_string()), Md::Text("b".to_string()))
            .unwrap();
        let e = serde_json::to_string(&t).expect_err("colliding images must error on to_json");
        assert!(
            format!("{e}").contains("stringify identically"),
            "got: {e}"
        );
    }

    #[test]
    fn a_typed_key_shadowing_a_declared_json_name_errors_on_write() {
        // (12) T3: a rest key imaging to a declared field's JSON name would shadow it (most JSON
        // parsers are last-wins), so to_json errors naming the declared-field collision.
        let mut t = Tkk::new(1);
        t.insert_rest(Md::Text("key_1".to_string()), Md::Text("v".to_string()))
            .unwrap();
        let e = serde_json::to_string(&t).expect_err("a declared-name collision must error");
        assert!(format!("{e}").contains("declared field"), "got: {e}");
    }

    #[test]
    fn a_rest_key_equal_to_a_declared_cbor_key_is_rejected_before_write() {
        // (13) `key_1` and the rest member name "1" are distinct JSON spellings, but both represent
        // CBOR uint(1). The native checked door must reject before mutation/write, and flattened JSON
        // must re-enter that same validator after turning its member name back into `Md`.
        let mut t = Tkk::new(9);
        let before = ToCBORBytes::to_cbor_bytes(&t);
        let error = t
            .insert_rest(uint_key(1), Md::Text("hi".to_string()))
            .expect_err("a rest key CBOR-equal to declared key 1 must be rejected");
        assert!(
            matches!(error.failure(), crate::generated::error::DeserializeFailure::DuplicateKey(_)),
            "ordinary declared-key collision retains DuplicateKey: {error}"
        );
        assert_eq!(
            t.rest().len(),
            0,
            "rejected native insertion is atomic"
        );
        assert!(
            ToCBORBytes::to_cbor_bytes(&t) == before,
            "rejected native insertion cannot change wire bytes"
        );
        t.insert_rest(uint_key(2), Md::Text("ok".to_string()))
            .unwrap();
        let bytes = ToCBORBytes::to_cbor_bytes(&t);
        assert_eq!(Tkk::from_cbor_bytes(&bytes).unwrap().rest().len(), 1);
        let json_error = serde_json::from_str::<Tkk>(r#"{"key_1":9,"1":{"Text":"hi"}}"#)
            .expect_err("flattened JSON cannot launder a CBOR-key collision");
        assert!(
            format!("{json_error}").contains("Duplicate key"),
            "JSON must report the native key validation failure: {json_error}"
        );
    }

    #[test]
    fn typed_key_pair_map_flattens_and_rejects_actual_duplicates() {
        // (14) A no-duplicate typed-K `@duplicates preserve` row flattens exactly like its loose twin.
        let mut ok = Tkkp::new(1);
        ok.insert_rest(uint_key(7), Md::Text("a".to_string())).unwrap();
        ok.insert_rest(Md::Text("z".to_string()), Md::Text("b".to_string())).unwrap();
        let json = serde_json::to_string(&ok).unwrap();
        assert!(
            json.contains("\"7\":{\"Text\":\"a\"}") && json.contains("\"z\":{\"Text\":\"b\"}"),
            "{json}"
        );

        // (15) ...and a row holding ACTUAL duplicates has no JSON image at all: duplicates image
        // identically BY DEFINITION, so they hit the same captured-vs-captured check. `@duplicates
        // preserve` is CBOR-only fidelity, and a typed K changes nothing about that.
        let mut dup = Tkkp::new(1);
        dup.insert_rest(uint_key(7), Md::Text("a".to_string())).unwrap();
        dup.insert_rest(uint_key(7), Md::Text("b".to_string())).unwrap();
        let e = serde_json::to_string(&dup).expect_err("actual duplicates must error on to_json");
        assert!(
            format!("{e}").contains("stringify identically"),
            "got: {e}"
        );

        let before = ToCBORBytes::to_cbor_bytes(&ok);
        let collision = ok
            .insert_rest(uint_key(1), Md::Text("blocked".to_string()))
            .expect_err("preserve pair maps retain rest/rest duplicates, never declared/rest ones");
        assert!(matches!(
            collision.failure(),
            crate::generated::error::DeserializeFailure::DuplicateKey(_)
        ));
        assert_eq!(ToCBORBytes::to_cbor_bytes(&ok), before, "preserve rejection is atomic");
    }

    #[test]
    fn a_primitive_typed_key_reads_like_its_bare_sibling() {
        // A primitive typed K (`uint .size 1` — typed only because the bound keeps it off the peeked
        // path) states its image directly: decimal both ways, exactly what the bare `uint` domain
        // does, so which side of the CBOR routing rule a row falls on never changes its JSON. The
        // bound is still K's, so an out-of-range member name is a parse error.
        let mut p = Pk::new(1);
        p.insert_rest(200, 5).unwrap();
        let json = serde_json::to_string(&p).unwrap();
        assert!(json.contains("\"200\":5"), "{json}");
        let back: Pk = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
        assert!(
            serde_json::from_str::<Pk>(r#"{"key_1":1,"300":5}"#).is_err(),
            "a member name outside the key's own range must fail the parse"
        );
        let mut collision = Pk::new(1);
        let before = ToCBORBytes::to_cbor_bytes(&collision);
        let error = collision
            .insert_rest(1, 9)
            .expect_err("a sized uint key is CBOR-value-equal to declared uint(1)");
        assert!(matches!(
            error.failure(),
            crate::generated::error::DeserializeFailure::DuplicateKey(_)
        ));
        assert_eq!(collision.rest().len(), 0);
        assert_eq!(ToCBORBytes::to_cbor_bytes(&collision), before);
    }

    #[test]
    fn a_bytes_key_domain_has_no_json_image_in_either_direction() {
        // A `bytes` K's wire form is a byte string, which has no member-name image at all — so this
        // row's flattened region is a pure error surface, loudly, on both faces. Compiled rather than
        // only snapshotted: the write side of this route has no `to_cbor_bytes` to lean on.
        let mut b = Bk::new(1);
        b.insert_rest(vec![0xde, 0xad], 5).unwrap();
        let e = serde_json::to_string(&b).expect_err("a bytes key must error on to_json");
        assert!(format!("{e}").contains("Bytes"), "got: {e}");
        let e = serde_json::from_str::<Bk>(r#"{"key_1":1,"dead":5}"#)
            .expect_err("no member name reads back as a bytes key");
        assert!(format!("{e}").contains("is not a valid key"), "got: {e}");
        // ...and the CBOR face is untouched by the JSON one.
        let bytes = ToCBORBytes::to_cbor_bytes(&b);
        assert_eq!(Bk::from_cbor_bytes(&bytes).unwrap().rest(), b.rest());
    }

    #[test]
    fn tagged_uint_key_is_distinct_from_the_bare_declared_uint() {
        // The Rust value is `1` in both positions, but the rest key serializes as tag(24, 1),
        // which is a distinct CBOR data-model value from the record's bare uint(1) key.
        let mut value = Tagged::new(7);
        value
            .insert_rest(1, 9)
            .expect("tagged uint(1) must not collide with bare declared uint(1)");
        let bytes = ToCBORBytes::to_cbor_bytes(&value);
        assert!(
            Tagged::from_cbor_bytes(&bytes).is_ok(),
            "the accepted tagged key remains readable"
        );
    }

    #[test]
    fn typed_key_pair_map_publishes_the_same_schema_as_its_loose_twin() {
        // (16) The published schema of the flattened region belongs to the rest-row POSITION (key
        // domain x value type), never to the container holding the entries — so the PairMap twin and
        // its loose twin agree everywhere but the type name.
        let strip = |schema: schemars::Schema| {
            let mut value = serde_json::to_value(schema).unwrap();
            value.as_object_mut().unwrap().remove("title");
            value
        };
        assert_eq!(
            strip(schemars::schema_for!(Tkkp)),
            strip(schemars::schema_for!(Tkk)),
            "the PairMap-backed typed-K rest row must publish its loose twin's schema"
        );
    }

    #[test]
    fn a_typed_key_rest_row_publishes_an_open_object() {
        // (17) The honest schema of a general typed key domain: an OPEN object over the range merged
        // with the declared properties. The member names are K's key-string IMAGE, which no K-derived
        // schema describes, so nothing may constrain them here.
        let schema = serde_json::to_value(schemars::schema_for!(Tkk)).unwrap();
        assert_eq!(
            schema.pointer("/type").and_then(serde_json::Value::as_str),
            Some("object"),
            "{schema}"
        );
        assert!(
            schema.pointer("/properties/key_1").is_some(),
            "the declared field survives the flatten merge: {schema}"
        );
        assert!(
            schema.pointer("/additionalProperties/$ref").is_some(),
            "the open region is an additionalProperties over the range: {schema}"
        );
        for junk in ["patternProperties", "items", "prefixItems"] {
            assert!(
                schema.get(junk).is_none(),
                "the object schema must carry no `{junk}` keyword: {schema}"
            );
        }
    }

    #[test]
    fn a_text_key_domain_flattens_beside_a_declared_member_of_another_type() {
        // (19) `Trow = { name: text, * text => uint }`: every string is a valid member name, so the
        // declared name binds first and the remainder lands in rest — the same split the numeric
        // domains make, over a key space that contains the declared name itself.
        let mut trow = Trow::new("ada".to_string());
        trow.insert_rest("era".to_string(), 3).unwrap();
        let json = serde_json::to_string(&trow).unwrap();
        assert!(json.contains("\"name\":\"ada\""), "declared field: {json}");
        assert!(json.contains("\"era\":3"), "rest text key flattened: {json}");
        let back: Trow = serde_json::from_str(&json).unwrap();
        assert_eq!(back.name, "ada");
        assert_eq!(back.rest().get("era"), Some(&3));
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
    }

    #[test]
    fn a_text_key_domain_publishes_an_unpatterned_open_region() {
        // (20) The schema half of the vector above, and the pre-existing member of the class the TS
        // projection leg covers: a `text` domain constrains no member NAME, so the region is an
        // `additionalProperties` over the range with no pattern beside it — and the declared member's
        // type is not the range's, which is what TypeScript's index signature cannot say exactly.
        let schema = serde_json::to_value(schemars::schema_for!(Trow)).unwrap();
        assert!(
            schema.pointer("/properties/name").is_some(),
            "the declared field survives the flatten merge: {schema}"
        );
        assert!(
            schema.get("patternProperties").is_none(),
            "a text domain admits every name, so nothing may pattern them: {schema}"
        );
        assert_eq!(
            schema
                .pointer("/additionalProperties/type")
                .and_then(serde_json::Value::as_str),
            Some("integer"),
            "the open region is typed by the RANGE: {schema}"
        );
    }

    #[test]
    fn a_key_whose_own_schema_is_constrained_still_publishes_an_open_object() {
        // (18) The anti-vector for the K-free schema helper. `Uk`'s own `JsonSchema` is a constrained
        // NUMBER, and `BTreeMap<Uk, u64>`'s schema would turn that into `patternProperties "^\d+$"`
        // plus `additionalProperties: false` — constraining the member names by K's VALUE schema
        // (which has nothing to do with the key image) and CLOSING the open region the rest row exists
        // to advertise. This goes red the day the helper is "simplified" back to `BTreeMap<K, V>`.
        let schema = serde_json::to_value(schemars::schema_for!(Uonly)).unwrap();
        assert!(
            schema.get("patternProperties").is_none(),
            "K's own value schema must not constrain the member names: {schema}"
        );
        assert_ne!(
            schema.get("additionalProperties"),
            Some(&serde_json::Value::Bool(false)),
            "the flattened rest region must stay OPEN: {schema}"
        );
        assert!(
            schema
                .pointer("/additionalProperties/type")
                .and_then(serde_json::Value::as_str)
                .is_some(),
            "the open region is typed by the RANGE: {schema}"
        );
    }
}
