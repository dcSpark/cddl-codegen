// Open-array rest-tail JSON end-to-end vectors. The captured tail renders as an
// ORDINARY JSON array under the field name; to_json on an `any` tail is fallible on data (RFC 8949
// §6.1's injective subset — a non-injective node like a byte string errors, never a silent
// substitute); only a loose empty tail is skipped on write and defaulted on read.
#[cfg(test)]
mod open_array_json {
    use super::*;
    use crate::generated::any_cbor::AnyCbor;
    use crate::generated::bounded::BoundedVec;

    #[test]
    fn typed_tail_renders_as_array_and_round_trips() {
        // Cap = [uint, tstr, * uint]: the tail is an ordinary JSON array of numbers under `rest`.
        let mut cap = Cap::new(7, "hi".to_string());
        cap.rest = vec![2, 3];
        let json = serde_json::to_string(&cap).unwrap();
        assert!(json.contains("\"rest\":[2,3]"), "typed tail as JSON array: {json}");
        let back: Cap = serde_json::from_str(&json).unwrap();
        assert_eq!(back.index_0, 7);
        assert_eq!(back.index_1, "hi");
        assert_eq!(back.rest, vec![2, 3]);
    }

    #[test]
    fn empty_tail_equals_closed_struct_json() {
        // No trailing elements: the `rest` field is skipped on write (empty tail ≡ closed-struct JSON),
        // and an absent `rest` key defaults to empty on read.
        let cap = Cap::new(7, "hi".to_string());
        let json = serde_json::to_string(&cap).unwrap();
        assert!(!json.contains("rest"), "empty tail omits the field: {json}");
        let back: Cap = serde_json::from_str(r#"{"index_0":7,"index_1":"hi"}"#).unwrap();
        assert!(back.rest.is_empty(), "absent rest defaults to empty");
    }

    #[test]
    fn any_tail_renders_naturally_and_round_trips() {
        // CapAny = [uint, * any]: the `any` elements render NATURALLY (a uint 5 as the JSON number 5,
        // not a tagged `{"uint":5}`), reusing the homogeneous `[* any]` member surface.
        let mut cap = CapAny::new(7);
        cap.rest = vec![AnyCbor::new_uint(5), AnyCbor::new_text("x".to_string())];
        let json = serde_json::to_string(&cap).unwrap();
        assert!(json.contains("\"rest\":[5,\"x\"]"), "any tail natural: {json}");
        let back: CapAny = serde_json::from_str(&json).unwrap();
        assert_eq!(back.rest.len(), 2);
    }

    #[test]
    fn any_tail_non_injective_node_errors_loudly() {
        // A byte string has no injective natural-JSON rendering (RFC 8949 §6.1) -> to_json errors
        // rather than inventing a spelling. Loud, by design.
        let mut cap = CapAny::new(7);
        cap.rest = vec![AnyCbor::new_bytes(vec![1, 2, 3])];
        assert!(
            serde_json::to_string(&cap).is_err(),
            "a bytes tail element must error on to_json (non-injective node)"
        );
    }

    #[test]
    fn required_tail_is_required_non_empty_json_and_schema_honest() {
        let r = Required::new(7, 2);
        let json = serde_json::to_string(&r).unwrap();
        assert!(json.contains("\"rest\":[2]"), "required tail is never skipped: {json}");
        assert!(serde_json::from_str::<Required>(r#"{"index_0":7,"rest":[]}"#).is_err());
        assert!(serde_json::from_str::<Required>(r#"{"index_0":7}"#).is_err());
        let schema = schemars::schema_for!(Required);
        assert_eq!(
            serde_json::to_value(schema).unwrap()["properties"]["rest"]["minItems"],
            1
        );
    }

    #[test]
    fn required_any_tail_stays_natural_and_rejects_empty_json() {
        let r = RequiredAny::new(7, AnyCbor::new_uint(5));
        let json = serde_json::to_string(&r).unwrap();
        assert!(json.contains("\"rest\":[5]"), "natural any JSON, not tagged AnyCbor: {json}");
        assert!(serde_json::from_str::<RequiredAny>(r#"{"index_0":7,"rest":[]}"#).is_err());
    }

    #[test]
    fn bounded_tail_is_required_json_with_honest_min_and_max_schema() {
        let bounded = Bounded::new(7, BoundedVec::try_from(vec![2, 3]).unwrap());
        let json = serde_json::to_string(&bounded).unwrap();
        assert!(json.contains("\"rest\":[2,3]"), "bounded tail is required: {json}");
        let back: Bounded = serde_json::from_str(&json).unwrap();
        assert_eq!(back.rest.as_slice(), &[2, 3]);
        for bad in [
            r#"{"index_0":7,"rest":[2]}"#,
            r#"{"index_0":7,"rest":[2,3,4,5]}"#,
            r#"{"index_0":7}"#,
        ] {
            assert!(serde_json::from_str::<Bounded>(bad).is_err(), "{bad} must reject");
        }
        let schema = serde_json::to_value(schemars::schema_for!(Bounded)).unwrap();
        assert_eq!(schema["properties"]["rest"]["minItems"], 2);
        assert_eq!(schema["properties"]["rest"]["maxItems"], 3);
    }

    #[test]
    fn middle_bounded_segment_reuses_json_schema_and_checked_constructor() {
        let value = MiddleBounded::new(
            7,
            "x".to_owned(),
            BoundedVec::try_from(vec![2, 3]).unwrap(),
        );
        let json = serde_json::to_string(&value).unwrap();
        assert!(json.contains("\"rest\":[2,3]"), "middle carrier remains JSON-visible: {json}");
        assert!(json.contains("\"index_2\":\"x\""), "suffix remains an ordinary field: {json}");
        let back: MiddleBounded = serde_json::from_str(&json).unwrap();
        assert_eq!(back.rest.as_slice(), &[2, 3]);
        assert_eq!(back.index_2, "x");
        let schema = serde_json::to_value(schemars::schema_for!(MiddleBounded)).unwrap();
        assert_eq!(schema["properties"]["rest"]["minItems"], 2);
        assert_eq!(schema["properties"]["rest"]["maxItems"], 3);
        assert!(serde_json::from_str::<MiddleBounded>(
            r#"{"index_0":7,"rest":[2],"index_2":"x"}"#
        )
        .is_err());
    }

    #[test]
    fn exact_middle_same_major_reuses_json_schema_and_checked_constructor() {
        let value = ExactMiddle::new(7, 9, BoundedVec::try_from(vec![2, 3]).unwrap());
        let json = serde_json::to_string(&value).unwrap();
        assert!(json.contains("\"rest\":[2,3]"), "exact carrier remains JSON-visible: {json}");
        assert!(json.contains("\"index_2\":9"), "same-major suffix remains positional: {json}");
        let back: ExactMiddle = serde_json::from_str(&json).unwrap();
        assert_eq!(back.rest.as_slice(), &[2, 3]);
        assert_eq!(back.index_2, 9);
        let schema = serde_json::to_value(schemars::schema_for!(ExactMiddle)).unwrap();
        assert_eq!(schema["properties"]["rest"]["minItems"], 2);
        assert_eq!(schema["properties"]["rest"]["maxItems"], 2);
        assert!(serde_json::from_str::<ExactMiddle>(
            r#"{"index_0":7,"rest":[2],"index_2":9}"#
        )
        .is_err());
    }

    #[test]
    fn exact_segments_json_round_trip_every_named_carrier() {
        let value = ExactSegments::new(
            7,
            BoundedVec::try_from(vec![vec![0xaa], vec![0xbb]]).unwrap(),
            BoundedVec::try_from(Vec::<u64>::new()).unwrap(),
            BoundedVec::try_from(vec![2, 3, 4]).unwrap(),
            "end".to_owned(),
        );
        let json = serde_json::to_value(&value).unwrap();
        assert_eq!(json["chunks"], serde_json::json!([[170], [187]]));
        assert_eq!(json["absent"], serde_json::json!([]));
        assert_eq!(json["values"], serde_json::json!([2, 3, 4]));
        let back: ExactSegments = serde_json::from_value(json).unwrap();
        assert_eq!(back.chunks.as_slice(), &[vec![0xaa], vec![0xbb]]);
        assert!(back.absent.as_slice().is_empty());
        assert_eq!(back.values.as_slice(), &[2, 3, 4]);
        assert_eq!(back.suffix, "end");

        assert!(serde_json::from_str::<ExactSegments>(
            r#"{"prefix":7,"chunks":[[170]],"absent":[],"values":[2,3,4],"suffix":"end"}"#
        )
        .is_err());
    }

    #[test]
    fn bounded_any_tail_stays_natural_and_non_injective_values_fail_loudly() {
        let bounded = BoundedAny::new(
            7,
            BoundedVec::try_from(vec![AnyCbor::new_uint(5)]).unwrap(),
        );
        assert!(
            serde_json::to_string(&bounded).unwrap().contains("\"rest\":[5]"),
            "bounded any tail must use natural JSON"
        );
        let invalid = BoundedAny::new(
            7,
            BoundedVec::try_from(vec![AnyCbor::new_bytes(vec![1, 2, 3])]).unwrap(),
        );
        assert!(
            serde_json::to_string(&invalid).is_err(),
            "bounded any must not silently substitute a non-injective JSON representation"
        );
    }
}
