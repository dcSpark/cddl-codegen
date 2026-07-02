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

    // Negative half of the wrapper tests above (which never feed one invalid input): each reject
    // is asserted on an error-message SUBSTRING, not bare is_err(), so a rejection can't pass for
    // the wrong reason (e.g. a parse hiccup instead of the bound). The is_ok() baselines are the
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

    // Item 7a (TESTING_ROADMAP.md): each type emits a hand-written `impl JsonSchema` alongside its
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
        check!(Int, Int::new_uint(u64::MAX));
        check!(Int, Int::new_nint(499));
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
    }
}
