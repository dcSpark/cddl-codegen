#[cfg(test)]
mod tests {
    use super::*;
    use cbor_event::de::Deserializer;
    use serialization::Deserialize;

    #[test]
    fn bytes_wrapper() {
        let bytes = vec![0xBA, 0xAD, 0xF0, 0x0D];
        let hex = format!("\"{}\"", hex::encode(&bytes));
        let from_bytes = BytesWrapper::new(bytes.clone());
        let from_hex: BytesWrapper = serde_json::from_str(&hex).unwrap();
        assert_eq!(hex, serde_json::to_string_pretty(&from_bytes).unwrap());
        assert_eq!(hex, serde_json::to_string_pretty(&from_hex).unwrap());
    }

    #[test]
    fn str_wrapper() {
        let text = "hello, world";
        let json_str = format!("\"{text}\"");
        let from_str = StrWrapper::new(text.to_owned());
        let from_json: StrWrapper = serde_json::from_str(&json_str).unwrap();
        assert_eq!(json_str, serde_json::to_string_pretty(&from_str).unwrap());
        assert_eq!(json_str, serde_json::to_string_pretty(&from_json).unwrap());
    }

    fn json_wrapper_test<W, V>(value: V)
        where W: TryFrom<V> + serde::Serialize + for <'de> serde::Deserialize<'de>,
              V: std::fmt::Display,
              <W as std::convert::TryFrom<V>>::Error: std::fmt::Debug
    {
        let json_str = value.to_string();
        let from_value = W::try_from(value).unwrap();
        let from_json: W = serde_json::from_str(&json_str).unwrap();
        assert_eq!(json_str, serde_json::to_string_pretty(&from_value).unwrap());
        assert_eq!(json_str, serde_json::to_string_pretty(&from_json).unwrap());
    }

    #[test]
    fn u8_wrapper() {
        json_wrapper_test::<U8Wrapper, u8>(u8::MIN);
        json_wrapper_test::<U8Wrapper, u8>(u8::MAX);
    }

    #[test]
    fn u64_wrapper() {
        json_wrapper_test::<U64Wrapper, u64>(u64::MIN);
        json_wrapper_test::<U64Wrapper, u64>(u64::MAX);
    }

    #[test]
    fn i16_wrapper() {
        json_wrapper_test::<I16Wrapper, i16>(i16::MIN);
        json_wrapper_test::<I16Wrapper, i16>(i16::MAX);
    }

    #[test]
    fn i64_wrapper() {
        json_wrapper_test::<I64Wrapper, i64>(i64::MIN);
        json_wrapper_test::<I64Wrapper, i64>(i64::MAX);
    }

    #[test]
    fn nint_wrapper() {
        json_wrapper_test::<NintWrapper, u64>(u64::MIN);
        json_wrapper_test::<NintWrapper, u64>(u64::MAX);
    }

    // #[test]
    // fn bool_wrapper() {
    //     json_wrapper_test::<BoolWrapper, bool>(false);
    //     json_wrapper_test::<BoolWrapper, bool>(true);
    // }

    #[test]
    fn struct_wrapper() {
        let json_str = u64::MAX.to_string();
        let from_value = StructWrapper::from(U64Wrapper::from(u64::MAX));
        let from_json: StructWrapper = serde_json::from_str(&json_str).unwrap();
        assert_eq!(json_str, serde_json::to_string_pretty(&from_value).unwrap());
        assert_eq!(json_str, serde_json::to_string_pretty(&from_json).unwrap());
    }

    #[test]
    fn custom_wrapper() {
        let json_str = "\"1234\"";
        let from_value = CustomWrapper::from(1234u64);
        let from_json: CustomWrapper = serde_json::from_str(&json_str).unwrap();
        assert_eq!(json_str, serde_json::to_string_pretty(&from_value).unwrap());
        assert_eq!(json_str, serde_json::to_string_pretty(&from_json).unwrap());
    }

    // `@custom_json` in CONTAINED position. `CustomHolder` derives serde and schemars, and each
    // derive re-demands that trait on its `CustomWrapper` member — so this test's mere COMPILATION
    // is the proof that the hand-written impls the directive promises satisfy the contained flavor,
    // which is the one a real consumer always hits. What it then asserts is that the container's
    // derive routes the member through those hand impls unchanged: the member renders as the
    // quoted-decimal string the standalone `custom_wrapper` test pins, never as the number the
    // derives would have produced on their own.
    #[test]
    fn custom_wrapper_in_derived_container() {
        let json_str = r#"{"c":"1234"}"#;
        let holder = CustomHolder::new(CustomWrapper::from(1234u64));
        assert_eq!(serde_json::to_string(&holder).unwrap(), json_str);
        let from_json: CustomHolder = serde_json::from_str(json_str).unwrap();
        assert_eq!(serde_json::to_string(&from_json).unwrap(), json_str);
        // the member's own rejection door still applies through the container
        assert_json_reject::<CustomHolder>(r#"{"c":1234}"#, "invalid type");
        assert_json_reject::<CustomHolder>(r#"{"c":"abc"}"#, "invalid u64 as string");
    }

    // The bare `Int` serializes as the signed decimal *string* (see its serde impls): a JSON number
    // can't hold the full RFC 8949 §3.1 range (-2^64..=2^64-1), and the derived enum form would leak
    // the nint encoding (`{"Nint":4}` actually meaning -5). Known-answer pairs (Int <-> exact JSON),
    // incl. the RFC's own -500 example and both range extremes.
    #[test]
    fn int_json() {
        for (int, json) in [
            (Int::new_uint(0), "\"0\""),
            (Int::new_uint(u64::MAX), "\"18446744073709551615\""), // 2^64 - 1, uint ceiling
            (Int::new_nint(0), "\"-1\""),                          // nint 0 == -1
            (Int::new_nint(499), "\"-500\""),                      // RFC 8949 §3.1 worked example
            (Int::new_nint(u64::MAX), "\"-18446744073709551616\""), // -2^64, nint floor
        ] {
            assert_eq!(serde_json::to_string(&int).unwrap(), json);
            let from_json: Int = serde_json::from_str(json).unwrap();
            assert_eq!(serde_json::to_string(&from_json).unwrap(), json);
        }
    }

    // map JSON serde: `Table` is BTreeMap under the plain json profile and OrderedHashMap under
    // json_preserve — the same test compiles against both, covering both maps' serde impls
    #[test]
    fn table_holder() {
        let mut t = Table::new();
        t.insert("a".to_owned(), 1);
        t.insert("b".to_owned(), 2);
        let holder = TableHolder::new(t);
        let json = serde_json::to_string_pretty(&holder).unwrap();
        let from_json: TableHolder = serde_json::from_str(&json).unwrap();
        assert_eq!(json, serde_json::to_string_pretty(&from_json).unwrap());
        assert_eq!(from_json.t.get("a"), Some(&1));
        assert_eq!(from_json.t.get("b"), Some(&2));
    }

    // Negative half of the wrapper tests above (which never feed one invalid input): each reject
    // is asserted on an error-message SUBSTRING, not bare is_err(), so a rejection can't pass for
    // the wrong reason (e.g. a parse hiccup instead of the bound). The accept baselines are the
    // positive tests above, which accept the same shapes with one dimension fixed.
    fn assert_json_reject<W>(json: &str, needle: &str)
    where
        W: for<'de> serde::Deserialize<'de>,
    {
        // no unwrap_err(): the wrapper types don't all implement Debug
        let err = match serde_json::from_str::<W>(json) {
            Ok(_) => panic!("rejecting {json}: expected an error, but it deserialized"),
            Err(e) => e.to_string(),
        };
        assert!(
            err.contains(needle),
            "rejecting {json}: expected an error containing {needle:?}, got: {err}"
        );
    }

    #[test]
    fn json_rejects() {
        // out-of-range values for bounded / sized numeric newtypes
        assert_json_reject::<U8Wrapper>("256", "expected u8"); // u8_wrapper = uint .lt 256
        assert_json_reject::<U8Wrapper>("-1", "expected u8");
        assert_json_reject::<I16Wrapper>("40000", "expected i16"); // i16_wrapper = int .size 2
        assert_json_reject::<I16Wrapper>("-40000", "expected i16");
        assert_json_reject::<U64Wrapper>("-1", "expected u64");
        // Int's JSON form is the signed decimal STRING; one-past-the-extremes must reject
        // (u64::MAX + 1 and -(2^64) - 1, the exact boundaries int_json pins from the inside)
        assert_json_reject::<Int>("\"18446744073709551616\"", "invalid Int");
        assert_json_reject::<Int>("\"-18446744073709551617\"", "invalid Int");
        assert_json_reject::<Int>("\"abc\"", "invalid Int");
        // bad hex for the bytes newtype: non-hex characters, and an odd digit count
        assert_json_reject::<BytesWrapper>("\"zzzz\"", "invalid hex bytes");
        assert_json_reject::<BytesWrapper>("\"abc\"", "invalid hex bytes");
        // custom (@custom_json) wrapper parses a u64 out of a string
        assert_json_reject::<CustomWrapper>("\"abc\"", "invalid u64 as string");
        assert_json_reject::<CustomWrapper>("\"-1\"", "invalid u64 as string");
        // wrong JSON type entirely, for every JSON shape the fixture emits
        assert_json_reject::<U8Wrapper>("\"5\"", "invalid type");
        assert_json_reject::<StrWrapper>("5", "invalid type");
        assert_json_reject::<BytesWrapper>("5", "invalid type");
        assert_json_reject::<CustomWrapper>("1234", "invalid type");
        assert_json_reject::<Int>("5", "invalid type");
        assert_json_reject::<NintWrapper>("\"5\"", "invalid type");
        assert_json_reject::<StructWrapper>("\"5\"", "invalid type");
    }

    // --- `hex`-dependency wire pins, at the JSON bytes-wrapper surface ---
    //
    // A `bytes` newtype (`bytes_wrapper`) emits hand-written serde impls that call the hex crate's
    // encode/decode DIRECTLY (the `json_hex_bytes` branch of the wrapper emitter), so that
    // dependency's behavior IS the JSON wire format of every bytes newtype this tool emits, on both
    // sides. The `bytes_wrapper` round-trip above only ever feeds back what the serializer produced,
    // so the ACCEPTED INPUT GRAMMAR is observed nowhere else — and unlike the `RawBytesEncoding`
    // surface, the emitted deserializer discards the hex error entirely (it substitutes serde's
    // `invalid_value` / "invalid hex bytes"), so a rejection's WORDING is ours and only the accept /
    // reject VERDICT is the dependency's. That verdict is what these pins hold.
    //
    // Companion to the pins at the `RawBytesEncoding` surface in `tests/raw-bytes/tests.rs`; same
    // rule (tests/README.md hand-vector shape 5: pin a re-exposed dependency's behavior BEFORE the
    // baseline moves). Asserted only through serde — no `hex::` path is named — so they survive a
    // swap and must be flipped in its own diff.
    //
    // The accepted grammar is CANONICAL hex — bare, even-length, lowercase, exactly what the
    // serialize half emits — and both surfaces get it from the same runtime door
    // (`decode_canonical_hex` in the composed `serialization.rs`), which is why the two cannot
    // drift. Accepting uppercase was a leniency this surface HAD and no longer has: it was removed
    // by maintainer ruling in exchange for the round-trip property pinned below.
    #[test]
    fn bytes_wrapper_hex_wire_pins() {
        // Wire pin 5: serialize emits a PLAIN LOWERCASE hex string — no `0x` prefix, no uppercase —
        // and deserialize accepts that exact form.
        let value = BytesWrapper::new(vec![0xBA, 0xAD, 0xF0, 0x0D]);
        assert_eq!(serde_json::to_string(&value).unwrap(), "\"baadf00d\"");
        let from_lower: BytesWrapper = serde_json::from_str("\"baadf00d\"").unwrap();
        assert_eq!(serde_json::to_string(&from_lower).unwrap(), "\"baadf00d\"");
        // ...and its uppercase twin is now REJECTED (this half of pin 5 flipped with the canonical
        // grammar). The JSON surface is verdict-only: the emitted deserializer discards the hex
        // error and substitutes one wording for every refusal, so there is no character or index to
        // pin here — the raw-bytes twin pins those.
        assert_json_reject::<BytesWrapper>("\"BAADF00D\"", "invalid hex bytes");
        // MIXED case, likewise rejected — a lowercase prefix buys the rest no leniency.
        assert_json_reject::<BytesWrapper>("\"baADf00d\"", "invalid hex bytes");
        // Wire pin 3 (JSON half): a `0x` / `0X`-prefixed hex string is REJECTED — the accepted JSON
        // grammar for a bytes newtype is bare hex digits only. This is the pin a more lenient hex
        // implementation flips from reject to accept, silently widening a shipped JSON input
        // grammar while serialization keeps emitting the unprefixed form.
        assert_json_reject::<BytesWrapper>("\"0xbaadf00d\"", "invalid hex bytes");
        assert_json_reject::<BytesWrapper>("\"0XBAADF00D\"", "invalid hex bytes");
    }

    /// The canonical round-trip property at the JSON surface: for every ACCEPTED hex string, the
    /// value it deserializes to re-serializes to that same string byte for byte. Strictly stronger
    /// than the `bytes_wrapper` round-trip elsewhere in this file, which only ever feeds back what
    /// the serializer produced: this says the accepted grammar and the emitted grammar are the SAME
    /// grammar, so no accepted spelling exists that serialization would not itself have written.
    /// Any read-side widening (uppercase, a `0x` prefix) falsifies exactly this.
    #[test]
    fn bytes_wrapper_hex_canonical_round_trip() {
        for s in ["\"\"", "\"00\"", "\"baadf00d\"", "\"0123456789abcdef\""] {
            let value: BytesWrapper = serde_json::from_str(s)
                .unwrap_or_else(|e| panic!("{s} must be accepted canonical hex: {e}"));
            assert_eq!(serde_json::to_string(&value).unwrap(), s);
        }
    }

    // Each type emits a hand-written `impl JsonSchema` alongside its
    // serde impl, and json-gen ships those schemas. The two are generated independently, so they can
    // silently disagree — a gap the round-trip tests above can't see (they only check that serde is
    // self-consistent). Validate a concrete value of every exported type against `schema_for!(T)`
    // (the exact schema json-gen writes out) so a schema that contradicts the serialization fails CI.
    #[test]
    fn schemas_validate_serialization() {
        fn check(schema: schemars::Schema, value: serde_json::Value, label: &str) {
            let schema = serde_json::to_value(schema).unwrap();
            let validator = jsonschema::validator_for(&schema)
                .unwrap_or_else(|e| panic!("{label}: emitted schema is not a valid JSON Schema: {e}"));
            let errors: Vec<String> = validator.iter_errors(&value).map(|e| e.to_string()).collect();
            assert!(
                errors.is_empty(),
                "{label}: {value} does not validate against its own emitted schema: {errors:?}"
            );
        }
        macro_rules! check {
            ($ty:ty, $val:expr) => {
                check(schemars::schema_for!($ty), serde_json::to_value(&$val).unwrap(), stringify!($ty))
            };
        }
        check!(BytesWrapper, BytesWrapper::new(vec![0xBA, 0xAD, 0xF0, 0x0D]));
        check!(StrWrapper, StrWrapper::new("hello, world".to_owned()));
        check!(U8Wrapper, U8Wrapper::new(u8::MAX).unwrap());
        check!(U64Wrapper, U64Wrapper::new(u64::MAX));
        check!(I16Wrapper, I16Wrapper::new(i16::MIN).unwrap());
        check!(I64Wrapper, I64Wrapper::new(i64::MIN).unwrap());
        check!(IntWrapper, IntWrapper::new(Int::new_nint(499)));
        check!(NintWrapper, NintWrapper::new(u64::MAX));
        check!(StructWrapper, StructWrapper::new(U64Wrapper::new(u64::MAX)));
        check!(CustomWrapper, CustomWrapper::new(1234));
        // the CONTAINED flavor: the holder's DERIVED schema delegates the member to the
        // hand-written `JsonSchema` impl, so the published shape agrees with what the hand-written
        // `Serialize` actually writes
        check!(CustomHolder, CustomHolder::new(CustomWrapper::new(1234)));
        check!(Int, Int::new_uint(u64::MAX));
        check!(Int, Int::new_nint(499));
        let mut table = Table::new();
        table.insert("a".to_owned(), 1);
        check!(TableHolder, TableHolder::new(table));
        // WI-4: the PairMap field's emitted schema is an array-of-`[k, v]`-pairs (mirroring the
        // OrderedSet/NonEmptyVec array delegation) — a duplicate-keyed value validates against it.
        check!(
            PreservePmapJson,
            PreservePmapJson::new(PairMap::<u64, String>::from(vec![
                (1u64, "a".to_string()),
                (1u64, "b".to_string()),
            ]))
        );
    }

    // The negative counterpart of schemas_validate_serialization, which is positive-only: a schema
    // that degrades to something too PERMISSIVE (the degenerate always-true `{}` accepts every
    // instance) still passes every positive check. So for each covered type, validate one
    // deliberately wrong-shaped value against the emitted schema and assert validation FAILS —
    // over-permissive schema drift (the likelier decay direction for hand-written impls) becomes a
    // CI failure instead of staying invisible.
    #[test]
    fn schemas_reject_wrong_shapes() {
        fn check_rejects(schema: schemars::Schema, value: serde_json::Value, label: &str) {
            let schema = serde_json::to_value(schema).unwrap();
            let validator = jsonschema::validator_for(&schema)
                .unwrap_or_else(|e| panic!("{label}: emitted schema is not a valid JSON Schema: {e}"));
            assert!(
                !validator.is_valid(&value),
                "{label}: wrong-shaped {value} unexpectedly validates against the emitted schema — the schema is over-permissive"
            );
        }
        macro_rules! check_rejects {
            ($ty:ty, $val:expr) => {
                check_rejects(schemars::schema_for!($ty), $val, stringify!($ty))
            };
        }
        // string-shaped JSON forms reject a number...
        check_rejects!(BytesWrapper, serde_json::json!(5));
        check_rejects!(StrWrapper, serde_json::json!(5));
        check_rejects!(CustomWrapper, serde_json::json!(1234));
        check_rejects!(IntWrapper, serde_json::json!(5));
        check_rejects!(Int, serde_json::json!(5));
        // ...and number-shaped forms reject a string
        check_rejects!(U8Wrapper, serde_json::json!("nope"));
        check_rejects!(U64Wrapper, serde_json::json!("nope"));
        check_rejects!(I16Wrapper, serde_json::json!("nope"));
        check_rejects!(I64Wrapper, serde_json::json!("nope"));
        check_rejects!(NintWrapper, serde_json::json!("nope"));
        check_rejects!(StructWrapper, serde_json::json!("nope"));
        // ...and object-shaped forms reject a wrong-typed field
        check_rejects!(TableHolder, serde_json::json!({"t": {"a": "nope"}}));
        // the contained `@custom_json` member's hand-written schema is string-shaped, so a
        // number-valued member fails the holder's derived schema (an over-permissive delegation —
        // the likeliest decay for a hand impl reached through a derive — would accept it)
        check_rejects!(CustomHolder, serde_json::json!({"c": 1234}));
        // WI-4: a PairMap's array-of-pairs schema rejects an OBJECT for the field (the object form a
        // naive map JSON would use) — proving the schema is the honest array shape, not object-shaped.
        check_rejects!(PreservePmapJson, serde_json::json!({"xs": {"1": "a"}}));
    }

    // WI-1: NonEmptyVec (`[+ uint]`) serializes as a plain JSON array, and JSON deserialize routes
    // through the same TryFrom door — an empty array is rejected there too.
    #[test]
    fn non_empty_vec_json() {
        let nev = NevJson::new(NonEmptyVec::try_from(vec![1u64, 2, 3]).unwrap());
        let json = serde_json::to_string(&nev).unwrap();
        assert!(
            json.contains("[1,2,3]"),
            "NonEmptyVec must serialize as a plain JSON array, got: {json}"
        );
        // JSON deserialize routes through TryFrom (round-trips back to the same JSON)
        let back: NevJson = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
        // an empty array for a `[+ uint]` field is rejected AT the TryFrom door on JSON deserialize
        assert!(serde_json::from_str::<NevJson>(r#"{"xs":[]}"#).is_err());
    }

    // WI-2: NonEmptyMap (`{+ tstr => uint}`) serializes as a plain JSON object, and JSON deserialize
    // routes through the same TryFrom door — an empty object is rejected there too.
    #[test]
    fn non_empty_map_json() {
        // build flavor-agnostically via `NonEmptyMap::new` (the inner map type — `BTreeMap` under
        // `json`, `OrderedHashMap` under `json_preserve` — is never named), so this compiles under
        // both profiles without a `TryFrom<map>` whose collect target would be ambiguous.
        let nem = NemJson::new(NonEmptyMap::new("a".to_string(), 1u64));
        let json = serde_json::to_string(&nem).unwrap();
        assert!(
            json.contains("{\"a\":1}"),
            "NonEmptyMap must serialize as a plain JSON object, got: {json}"
        );
        // JSON deserialize routes through TryFrom (round-trips back to the same JSON)
        let back: NemJson = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
        // an empty object for a `{+ tstr => uint}` field is rejected AT the TryFrom door on deserialize
        assert!(serde_json::from_str::<NemJson>(r#"{"xs":{}}"#).is_err());
    }

    // WI-3: an `@duplicates reject` set (`OrderedSet<u64>`) serializes as a plain JSON array, and
    // JSON deserialize routes through the SAME `TryFrom` uniqueness door the CBOR/API paths use — a
    // duplicate-carrying array is refused there (never silently deduped), while a duplicate-free
    // array round-trips byte-exactly.
    #[test]
    fn reject_set_json() {
        let rs = RejectSetJson::new(OrderedSet::try_from(vec![1u64, 2, 3]).unwrap());
        let json = serde_json::to_string(&rs).unwrap();
        assert!(
            json.contains("[1,2,3]"),
            "OrderedSet must serialize as a plain JSON array, got: {json}"
        );
        // accept round-trip: a duplicate-free array round-trips back to the same JSON
        let back: RejectSetJson = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
        // a duplicate-carrying array is rejected AT the TryFrom door on JSON deserialize
        assert!(
            serde_json::from_str::<RejectSetJson>(r#"{"xs":[1,1]}"#).is_err(),
            "a duplicate-carrying JSON array must be refused at the OrderedSet TryFrom door"
        );
    }

    // WI-4: a `@duplicates preserve` table (`PairMap<u64, String>`) serializes as a JSON ARRAY of
    // `[k, v]` pairs — NOT an object, which would silently collapse the duplicate key — preserving both
    // order and duplicates. JSON deserialize reads that array-of-pairs back with the duplicate intact.
    #[test]
    fn preserve_pair_map_json() {
        let pm = PairMap::<u64, String>::from(vec![
            (1u64, "a".to_string()),
            (1u64, "b".to_string()),
            (2u64, "c".to_string()),
        ]);
        let holder = PreservePmapJson::new(pm);
        let json = serde_json::to_string(&holder).unwrap();
        assert!(
            json.contains(r#""xs":[[1,"a"],[1,"b"],[2,"c"]]"#),
            "PairMap must serialize as a JSON array of [k, v] pairs (order + duplicates intact), got: {json}"
        );
        // round-trip preserves order AND the duplicate key
        let back: PreservePmapJson = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&back).unwrap(), json);
        assert_eq!(back.xs.len(), 3, "the duplicate-keyed entry survives the JSON round-trip");
        assert_eq!(
            back.xs.get_all(&1).into_iter().map(String::as_str).collect::<Vec<_>>(),
            vec!["a", "b"],
            "both key-1 entries survive in first-appearance order"
        );
    }

    // WI-4: the `{+ …}` NonEmptyPairMap door refuses an empty `[]` on the JSON path (the same min-1
    // error the CBOR decoder raises for `{+ …}`), while a non-empty array-of-pairs — duplicates
    // included — is accepted.
    #[test]
    fn ne_preserve_pair_map_json_door() {
        assert!(
            serde_json::from_str::<NePreservePmapJson>(r#"{"ys":[]}"#).is_err(),
            "an empty array for a non-empty (`+`) preserve field must be refused at the NonEmptyPairMap door"
        );
        let ok: NePreservePmapJson =
            serde_json::from_str(r#"{"ys":[[1,"a"],[1,"b"]]}"#).expect("a non-empty array-of-pairs is accepted");
        assert_eq!(ok.ys.len(), 2, "the door keeps duplicate keys");
    }

    #[test]
    fn bounded_preserve_pair_map_json_and_schema_door() {
        let holder: BoundedPreservePmapJson = serde_json::from_str(
            r#"{"zs":[[1,"a"],[1,"b"],[2,"c"]]}"#,
        )
        .expect("a bounded array-of-pairs accepts duplicate keys in entry order");
        let json = serde_json::to_string(&holder).unwrap();
        assert!(
            json.contains(r#""zs":[[1,"a"],[1,"b"],[2,"c"]]"#),
            "BoundedPairMap must retain duplicate pair order: {json}"
        );
        assert_eq!(holder.zs.keys().copied().collect::<Vec<_>>(), vec![1, 1, 2]);
        assert!(
            serde_json::from_str::<BoundedPreservePmapJson>(r#"{"zs":[[1,"a"]]}"#).is_err(),
            "below-min JSON input must be rejected by BoundedPairMap::try_from"
        );
        assert!(
            serde_json::from_str::<BoundedPreservePmapJson>(r#"{"zs":[[1,"a"],[2,"b"],[3,"c"],[4,"d"]]}"#).is_err(),
            "above-max JSON input must be rejected by BoundedPairMap::try_from"
        );

        let schema = serde_json::to_value(schemars::schema_for!(BoundedPreservePmapJson)).unwrap();
        let text = schema.to_string();
        assert!(text.contains("minItems") && text.contains("maxItems"), "bounded pair schema must bound the pair array: {text}");
        assert!(!text.contains("minProperties") && !text.contains("maxProperties"), "bounded pair schema must not pretend to be an object: {text}");
        let validator = jsonschema::validator_for(&schema).unwrap();
        assert!(validator.is_valid(&serde_json::json!({"zs":[[1,"a"],[1,"b"]]})));
        assert!(!validator.is_valid(&serde_json::json!({"zs":[[1,"a"]]})));
        assert!(!validator.is_valid(&serde_json::json!({"zs":[[1,"a",0],[2,"b"]]})));
    }
}
