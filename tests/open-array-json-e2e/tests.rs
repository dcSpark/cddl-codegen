// Open-array rest-tail JSON end-to-end vectors. The captured tail renders as an
// ORDINARY JSON array under the field name; to_json on an `any` tail is fallible on data (RFC 8949
// §6.1's injective subset — a non-injective node like a byte string errors, never a silent
// substitute); only a loose empty tail is skipped on write and defaulted on read.
#[cfg(test)]
mod open_array_json {
    use super::*;
    use crate::generated::any_cbor::AnyCbor;
    use crate::generated::bounded::BoundedVec;
    use crate::generated::non_empty::NonEmptyVec;
    use crate::generated::ordered_set::{BoundedOrderedSet, NonEmptyOrderedSet, OrderedSet};

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
    fn recursively_nested_exact_arrays_round_trip_and_retain_every_bound() {
        let nested = [[1u64; 64], [2u64; 64]];
        let holder = WideNestedHolder::new(nested);
        let json = serde_json::to_string(&holder).unwrap();
        let back: WideNestedHolder = serde_json::from_str(&json).unwrap();
        assert_eq!(back.nested[0][0], 1);
        assert_eq!(back.nested[1][63], 2);

        let schema = serde_json::to_value(schemars::schema_for!(WideNestedHolder)).unwrap();
        let nested = &schema["properties"]["nested"];
        assert_eq!(nested["minItems"], 2);
        assert_eq!(nested["maxItems"], 2);
        assert_eq!(nested["items"]["minItems"], 64);
        assert_eq!(nested["items"]["maxItems"], 64);

        assert!(serde_json::from_str::<WideNestedHolder>(r#"{"nested":[[1],[2]]}"#).is_err());
        assert!(serde_json::from_str::<WideNestedHolder>(
            r#"{"nested":[[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64],[2]]}"#
        ).is_err());

        let deep = [[[1u64; 64], [2u64; 64]], [[3u64; 64], [4u64; 64]]];
        let deep_holder = WideNestedDeepHolder::new(deep);
        let deep_json = serde_json::to_string(&deep_holder).unwrap();
        let deep_back: WideNestedDeepHolder = serde_json::from_str(&deep_json).unwrap();
        assert_eq!(deep_back.nested[1][0][63], 3);
        let deep_schema =
            serde_json::to_value(schemars::schema_for!(WideNestedDeepHolder)).unwrap();
        let deep = &deep_schema["properties"]["nested"];
        assert_eq!(deep["minItems"], 2);
        assert_eq!(deep["items"]["minItems"], 2);
        assert_eq!(deep["items"]["items"]["minItems"], 64);
        assert_eq!(deep["items"]["items"]["maxItems"], 64);
    }

    #[test]
    fn recursive_exact_array_optional_field_and_type_choice_use_the_same_adapter() {
        let nested = [[1u64; 64], [2u64; 64]];
        let mut optional = WideNestedOptional::new(7);
        optional.nested = Some(nested);
        let json = serde_json::to_string(&optional).unwrap();
        let back: WideNestedOptional = serde_json::from_str(&json).unwrap();
        assert_eq!(back.nested.unwrap()[1][0], 2);

        let choice = WideNestedChoice::new_wide_nested(nested);
        let choice_json = serde_json::to_string(&choice).unwrap();
        let choice_back: WideNestedChoice = serde_json::from_str(&choice_json).unwrap();
        assert!(matches!(choice_back, WideNestedChoice::WideNested(_)));
    }

    #[test]
    fn optional_nullable_exact_arrays_keep_three_json_states_and_schema_bounds() {
        let wide = [7u64; 64];
        let absent = WideNullableOptional::new(1);
        assert_eq!(serde_json::to_string(&absent).unwrap(), r#"{"prefix":1}"#);
        assert_eq!(
            serde_json::from_str::<WideNullableOptional>(r#"{"prefix":1}"#)
                .unwrap()
                .value,
            None
        );

        let present_null: WideNullableOptional =
            serde_json::from_str(r#"{"prefix":1,"value":null}"#).unwrap();
        assert_eq!(present_null.value, Some(None));
        assert_eq!(
            serde_json::to_string(&present_null).unwrap(),
            r#"{"prefix":1,"value":null}"#
        );

        let mut present_value = WideNullableOptional::new(1);
        present_value.value = Some(Some(wide));
        let value_json = serde_json::to_string(&present_value).unwrap();
        assert!(value_json.contains(r#""value":[7,7"#), "exact value stays list-shaped: {value_json}");
        assert_eq!(
            serde_json::from_str::<WideNullableOptional>(&value_json)
                .unwrap()
                .value
                .unwrap()
                .unwrap()[63],
            7
        );
        assert!(serde_json::from_str::<WideNullableOptional>(r#"{"prefix":1,"value":[7]}"#).is_err());

        let restricted = BoundedVec::try_from(vec![wide, wide]).unwrap();
        let absent_restricted = WideRestrictedNullableOptional::new(1);
        assert_eq!(
            serde_json::to_string(&absent_restricted).unwrap(),
            r#"{"prefix":1}"#
        );
        assert_eq!(
            serde_json::from_str::<WideRestrictedNullableOptional>(r#"{"prefix":1}"#)
                .unwrap()
                .value,
            None
        );
        let present_null_restricted: WideRestrictedNullableOptional =
            serde_json::from_str(r#"{"prefix":1,"value":null}"#).unwrap();
        assert_eq!(present_null_restricted.value, Some(None));
        assert_eq!(
            serde_json::to_string(&present_null_restricted).unwrap(),
            r#"{"prefix":1,"value":null}"#
        );
        let mut restricted_value = WideRestrictedNullableOptional::new(1);
        restricted_value.value = Some(Some(restricted));
        let restricted_json = serde_json::to_string(&restricted_value).unwrap();
        serde_json::from_str::<WideRestrictedNullableOptional>(&restricted_json).unwrap();
        assert!(serde_json::from_str::<WideRestrictedNullableOptional>(
            &format!(r#"{{"prefix":1,"value":[{}]}}"#, wide_json(7))
        )
        .is_err());
        assert!(serde_json::from_str::<WideRestrictedNullableOptional>(
            &format!(r#"{{"prefix":1,"value":[{0},{0},{0},{0}]}}"#, wide_json(7))
        )
        .is_err());

        let schema = serde_json::to_value(schemars::schema_for!(WideRestrictedNullableOptional)).unwrap();
        assert!(schema["required"]
            .as_array()
            .map_or(true, |required| !required.iter().any(|field| field == "value")));
        let branches = schema["properties"]["value"]["anyOf"].as_array().unwrap();
        assert!(branches.iter().any(|branch| branch["type"] == "null"));
        let list = branches.iter().find(|branch| branch["type"] == "array").unwrap();
        assert_eq!(list["minItems"], 2);
        assert_eq!(list["maxItems"], 3);
        assert_eq!(list["items"]["minItems"], 64);
        assert_eq!(list["items"]["maxItems"], 64);
    }

    #[test]
    fn optional_nullable_exact_natural_any_stays_natural_and_loud() {
        let absent = ShortAnyNullableOptional::new(1);
        assert_eq!(serde_json::to_string(&absent).unwrap(), r#"{"prefix":1}"#);
        assert_eq!(
            serde_json::from_str::<ShortAnyNullableOptional>(r#"{"prefix":1}"#)
                .unwrap()
                .value,
            None
        );
        let mut direct = ShortAnyNullableOptional::new(1);
        direct.value = Some(Some(core::array::from_fn(|_| AnyCbor::new_uint(7))));
        let direct_json = serde_json::to_string(&direct).unwrap();
        assert!(direct_json.contains(r#""value":[7,7]"#), "natural any stays untagged: {direct_json}");
        assert!(matches!(
            serde_json::from_str::<ShortAnyNullableOptional>(&direct_json).unwrap().value,
            Some(Some(values)) if values[0].as_uint() == Some(7)
        ));
        assert_eq!(
            serde_json::from_str::<ShortAnyNullableOptional>(r#"{"prefix":1,"value":null}"#)
                .unwrap()
                .value,
            Some(None)
        );

        let mut nested = NestedShortAnyNullableOptional::new(1);
        nested.value = Some(Some([
            core::array::from_fn(|_| AnyCbor::new_uint(5)),
            core::array::from_fn(|_| AnyCbor::new_uint(6)),
        ]));
        assert!(serde_json::to_string(&nested).unwrap().contains("[[5,5],[6,6]]"));

        direct.value = Some(Some(core::array::from_fn(|_| AnyCbor::new_bytes(vec![1]))));
        assert!(serde_json::to_string(&direct).is_err(), "non-injective any stays loud");
    }

    #[test]
    fn recursively_nested_exact_any_stays_natural_and_loud() {
        let nested = [
            core::array::from_fn(|_| AnyCbor::new_uint(5)),
            core::array::from_fn(|_| AnyCbor::new_text("x".to_owned())),
        ];
        let holder = WideNestedAnyHolder::new(nested.clone());
        let json = serde_json::to_string(&holder).unwrap();
        assert!(json.contains("[[5,5"), "nested any must be natural: {json}");
        let back: WideNestedAnyHolder = serde_json::from_str(&json).unwrap();
        assert_eq!(back.nested[0][0].as_uint(), Some(5));

        let choice = WideNestedAnyChoice::new_wide_nested_any(nested);
        let choice_back: WideNestedAnyChoice =
            serde_json::from_str(&serde_json::to_string(&choice).unwrap()).unwrap();
        assert!(matches!(
            choice_back,
            WideNestedAnyChoice::WideNestedAny(_)
        ));

        let bad = WideNestedAnyHolder::new([
            core::array::from_fn(|_| AnyCbor::new_bytes(vec![1])),
            core::array::from_fn(|_| AnyCbor::new_uint(2)),
        ]);
        assert!(serde_json::to_string(&bad).is_err(), "non-injective any must stay loud");
    }

    #[test]
    fn recursive_sequence_carriers_round_trip_through_their_checked_doors_and_keep_bounds() {
        let exact = [7u64; 64];
        let loose = WideSeqHolder::new(vec![exact]);
        assert_eq!(
            serde_json::from_str::<WideSeqHolder>(&serde_json::to_string(&loose).unwrap())
                .unwrap()
                .xs[0][0],
            7
        );

        let mut optional = WideSeqOptional::new(1);
        optional.xs = Some(vec![exact]);
        assert!(serde_json::from_str::<WideSeqOptional>(&serde_json::to_string(&optional).unwrap())
            .unwrap()
            .xs
            .is_some());

        let nonempty = WideNonemptyHolder::new(NonEmptyVec::new(exact));
        let bounded = WideBoundedHolder::new(BoundedVec::try_from(vec![exact, exact]).unwrap());
        let min_only = WideMinHolder::new(BoundedVec::try_from(vec![exact, exact]).unwrap());
        let max_only = WideMaxHolder::new(BoundedVec::try_from(vec![exact]).unwrap());
        for value in [
            serde_json::to_value(&nonempty).unwrap(),
            serde_json::to_value(&bounded).unwrap(),
            serde_json::to_value(&min_only).unwrap(),
            serde_json::to_value(&max_only).unwrap(),
        ] {
            assert_eq!(value["xs"][0].as_array().unwrap().len(), 64);
        }
        assert_eq!(
            serde_json::from_str::<WideNonemptyHolder>(&serde_json::to_string(&nonempty).unwrap())
                .unwrap()
                .xs
                .len(),
            1
        );
        assert_eq!(
            serde_json::from_str::<WideBoundedHolder>(&serde_json::to_string(&bounded).unwrap())
                .unwrap()
                .xs
                .len(),
            2
        );
        for bad in [
            r#"{"xs":[]}"#,
            r#"{"xs":[[7]]}"#,
        ] {
            assert!(serde_json::from_str::<WideNonemptyHolder>(bad).is_err(), "{bad} must reject");
        }
        assert!(serde_json::from_str::<WideBoundedHolder>(
            &format!(r#"{{"xs":[{}]}}"#, wide_json(7))
        )
        .is_err());
        assert!(serde_json::from_str::<WideBoundedHolder>(
            &format!(r#"{{"xs":[{}, {}, {}, {}]}}"#, wide_json(7), wide_json(7), wide_json(7), wide_json(7))
        )
        .is_err());

        let mixed = WideMixedHolder::new(vec![NonEmptyVec::new(
            BoundedVec::try_from(vec![exact, exact]).unwrap(),
        )]);
        assert_eq!(
            serde_json::from_str::<WideMixedHolder>(&serde_json::to_string(&mixed).unwrap())
                .unwrap()
                .xs[0]
                .as_slice()[0]
                .as_slice()
                .len(),
            2
        );

        let bounded_schema = serde_json::to_value(schemars::schema_for!(WideBoundedHolder)).unwrap();
        assert_eq!(bounded_schema["properties"]["xs"]["minItems"], 2);
        assert_eq!(bounded_schema["properties"]["xs"]["maxItems"], 3);
        assert_eq!(bounded_schema["properties"]["xs"]["items"]["minItems"], 64);
        assert_eq!(bounded_schema["properties"]["xs"]["items"]["maxItems"], 64);
        let min_schema = serde_json::to_value(schemars::schema_for!(WideMinHolder)).unwrap();
        assert_eq!(min_schema["properties"]["xs"]["minItems"], 2);
        assert!(min_schema["properties"]["xs"].get("maxItems").is_none());
        let max_schema = serde_json::to_value(schemars::schema_for!(WideMaxHolder)).unwrap();
        assert_eq!(max_schema["properties"]["xs"]["minItems"], 0);
        assert_eq!(max_schema["properties"]["xs"]["maxItems"], 3);
        let mixed_schema = serde_json::to_value(schemars::schema_for!(WideMixedHolder)).unwrap();
        let mixed = &mixed_schema["properties"]["xs"];
        assert!(mixed.get("minItems").is_none());
        assert_eq!(mixed["items"]["minItems"], 1);
        assert_eq!(mixed["items"]["items"]["minItems"], 2);
        assert_eq!(mixed["items"]["items"]["maxItems"], 3);
        assert_eq!(mixed["items"]["items"]["items"]["minItems"], 64);

        let nullable = WideNullableSeqHolder::new(vec![Some(exact), None]);
        let nullable_json = serde_json::to_string(&nullable).unwrap();
        assert!(nullable_json.contains(",null]"), "nullable element retains JSON null: {nullable_json}");
        let nullable_back: WideNullableSeqHolder = serde_json::from_str(&nullable_json).unwrap();
        assert!(nullable_back.xs[0].is_some() && nullable_back.xs[1].is_none());
        let nullable_schema = serde_json::to_value(schemars::schema_for!(WideNullableSeqHolder)).unwrap();
        assert!(nullable_schema["properties"]["xs"]["items"]["anyOf"]
            .as_array()
            .unwrap()
            .iter()
            .any(|branch| branch["type"] == "null"));

        let choice = WideSeqChoice::new_arr_wide(vec![exact]);
        assert!(matches!(
            serde_json::from_str::<WideSeqChoice>(&serde_json::to_string(&choice).unwrap()).unwrap(),
            WideSeqChoice::ArrWide(_)
        ));
        let wrapped = WideSeqNewtype::new(vec![exact]);
        assert_eq!(
            serde_json::from_str::<WideSeqNewtype>(&serde_json::to_string(&wrapped).unwrap())
                .unwrap()
                .get()[0][0],
            7
        );
        let direct_wrapped = WideNewtype::new(exact);
        assert_eq!(
            serde_json::from_str::<WideNewtype>(&serde_json::to_string(&direct_wrapped).unwrap())
                .unwrap()
                .get()[0],
            7
        );
        let direct_any = WideAnyNewtype::new(core::array::from_fn(|_| AnyCbor::new_uint(7)));
        assert!(serde_json::to_string(&direct_any).unwrap().starts_with("[7,7"));
        let _ = schemars::schema_for!(WideNewtype);
        let _ = schemars::schema_for!(WideAnyNewtype);
    }

    #[test]
    fn recursive_natural_any_sequence_carriers_stay_natural_and_loud() {
        let any = core::array::from_fn(|_| AnyCbor::new_uint(7));
        let loose = WideAnySeqHolder::new(vec![any.clone()]);
        let nonempty = WideAnyNonemptyHolder::new(NonEmptyVec::new(any.clone()));
        let bounded = WideAnyBoundedHolder::new(BoundedVec::try_from(vec![any.clone()]).unwrap());
        assert!(serde_json::to_string(&loose).unwrap().contains("[[7,7"));
        assert!(serde_json::to_string(&nonempty).unwrap().contains("[[7,7"));
        assert!(serde_json::to_string(&bounded).unwrap().contains("[[7,7"));
        assert!(serde_json::from_str::<WideAnyNonemptyHolder>(r#"{"xs":[]}"#).is_err());
        assert!(serde_json::from_str::<WideAnyBoundedHolder>(r#"{"xs":[]}"#).is_err());
        let bad = WideAnyBoundedHolder::new(BoundedVec::try_from(vec![core::array::from_fn(|_| {
            AnyCbor::new_bytes(vec![1])
        })]).unwrap());
        assert!(serde_json::to_string(&bad).is_err(), "natural any remains non-injective-loud");
    }

    #[test]
    fn recursive_reject_sets_keep_order_reject_duplicates_and_publish_bounds() {
        let first = [7u64; 64];
        let second = [8u64; 64];
        let holder = WideRejectHolder::new(
            OrderedSet::try_from(vec![first, second]).unwrap(),
            NonEmptyOrderedSet::try_from(vec![first]).unwrap(),
            BoundedOrderedSet::try_from(vec![first, second]).unwrap(),
            BoundedOrderedSet::try_from(vec![first, second]).unwrap(),
        );
        let json = serde_json::to_string(&holder).unwrap();
        assert!(json.contains("\"loose\":[[7,7"), "reject set stays list-shaped: {json}");
        let back: WideRejectHolder = serde_json::from_str(&json).unwrap();
        assert_eq!(back.loose.as_slice(), &[first, second], "insertion order round-trips");

        let duplicate = format!(
            r#"{{"loose":[{0},{0}],"nonempty":[{0}],"bounded":[{0},{0}],"exact":[{0},{0}]}}"#,
            wide_json(7)
        );
        let error = serde_json::from_str::<WideRejectHolder>(&duplicate)
            .expect_err("duplicate reject input must not normalize");
        assert!(error.to_string().contains("Duplicate key"), "duplicate-specific error: {error}");
        for (reason, invalid, expected) in [
            (
                "nonempty underflow",
                format!(r#"{{"loose":[],"nonempty":[],"bounded":[{0},{0}],"exact":[{0},{0}]}}"#, wide_json(7)),
                "0 not at least 1",
            ),
            (
                "bounded underflow",
                format!(r#"{{"loose":[],"nonempty":[{0}],"bounded":[{0}],"exact":[{0},{0}]}}"#, wide_json(7)),
                "1 not in range 2 - 3",
            ),
            (
                "bounded overflow",
                format!(r#"{{"loose":[],"nonempty":[{0}],"bounded":[{0},{1},{2},{3}],"exact":[{0},{1}]}}"#, wide_json(7), wide_json(8), wide_json(9), wide_json(10)),
                "4 not in range 2 - 3",
            ),
            (
                "exact underflow",
                format!(r#"{{"loose":[],"nonempty":[{0}],"bounded":[{0},{1}],"exact":[{0}]}}"#, wide_json(7), wide_json(8)),
                "1 not in range 2 - 2",
            ),
            (
                "exact overflow",
                format!(r#"{{"loose":[],"nonempty":[{0}],"bounded":[{0},{1}],"exact":[{0},{1},{2}]}}"#, wide_json(7), wide_json(8), wide_json(9)),
                "3 not in range 2 - 2",
            ),
        ] {
            let error = serde_json::from_str::<WideRejectHolder>(&invalid)
                .expect_err("the checked set carrier must reject its authored range");
            assert!(
                error.to_string().contains(expected),
                "{reason} must retain the carrier's range error: {error}"
            );
        }

        let schema = serde_json::to_value(schemars::schema_for!(WideRejectHolder)).unwrap();
        for field in ["loose", "nonempty", "bounded", "exact"] {
            assert_eq!(schema["properties"][field]["uniqueItems"], true, "{field} uniqueness");
            assert_eq!(schema["properties"][field]["items"]["minItems"], 64, "{field} inner min");
            assert_eq!(schema["properties"][field]["items"]["maxItems"], 64, "{field} inner max");
        }
        assert_eq!(schema["properties"]["nonempty"]["minItems"], 1);
        assert_eq!(schema["properties"]["bounded"]["minItems"], 2);
        assert_eq!(schema["properties"]["bounded"]["maxItems"], 3);
        assert_eq!(schema["properties"]["exact"]["minItems"], 2);
        assert_eq!(schema["properties"]["exact"]["maxItems"], 2);
    }

    #[test]
    fn recursive_reject_sets_compose_through_optional_wrapper_choice_and_natural_any() {
        let first = [7u64; 64];
        let optional = WideRejectOptional::new(1);
        assert_eq!(
            serde_json::to_string(&optional).unwrap(),
            r#"{"prefix":1,"xs":null}"#
        );
        let absent_nullable = WideRejectNullableOptional::new(1);
        assert_eq!(serde_json::to_string(&absent_nullable).unwrap(), r#"{"prefix":1}"#);
        assert_eq!(
            serde_json::from_str::<WideRejectNullableOptional>(r#"{"prefix":1}"#)
                .unwrap()
                .xs,
            None,
            "absent remains the outer None"
        );
        let nullable: WideRejectNullableOptional = serde_json::from_str(r#"{"prefix":1,"xs":null}"#).unwrap();
        assert_eq!(nullable.xs, Some(None), "present null remains distinct from absence");
        assert_eq!(
            serde_json::to_string(&nullable).unwrap(),
            r#"{"prefix":1,"xs":null}"#
        );
        let second = [8u64; 64];
        let checked: BoundedOrderedSet<[u64; 64], 2, 3> =
            BoundedOrderedSet::try_from(vec![first, second]).unwrap();
        let mut present_value = WideRejectNullableOptional::new(1);
        present_value.xs = Some(Some(checked));
        let value_json = serde_json::to_string(&present_value).unwrap();
        assert!(value_json.contains(r#""xs":[[7,7"#), "present value remains a list: {value_json}");
        let value_back: WideRejectNullableOptional = serde_json::from_str(&value_json).unwrap();
        assert_eq!(
            value_back.xs.unwrap().unwrap().as_slice(),
            &[first, second],
            "present value returns through BoundedOrderedSet"
        );
        let nullable_schema =
            serde_json::to_value(schemars::schema_for!(WideRejectNullableOptional)).unwrap();
        assert!(nullable_schema["required"]
            .as_array()
            .map_or(true, |required| !required.iter().any(|field| field == "xs")));
        let nullable_branches = nullable_schema["properties"]["xs"]["anyOf"].as_array().unwrap();
        assert!(nullable_branches.iter().any(|branch| branch["type"] == "null"));
        let nullable_array = nullable_branches
            .iter()
            .find(|branch| branch["type"] == "array")
            .expect("nullable reject set retains an array branch");
        assert_eq!(nullable_array["uniqueItems"], true);
        assert_eq!(nullable_array["minItems"], 2);
        assert_eq!(nullable_array["maxItems"], 3);
        assert_eq!(nullable_array["items"]["minItems"], 64);
        assert_eq!(nullable_array["items"]["maxItems"], 64);
        let newtype = WideRejectNewtype::new(OrderedSet::try_from(vec![first]).unwrap());
        assert!(serde_json::to_string(&newtype).unwrap().starts_with("[[7,7"));
        let choice = WideRejectChoice::new_wide_reject_loose(OrderedSet::try_from(vec![first]).unwrap());
        assert!(matches!(serde_json::from_str::<WideRejectChoice>(&serde_json::to_string(&choice).unwrap()).unwrap(), WideRejectChoice::WideRejectLoose(_)));

        let natural = WideRejectAnyHolder::new(OrderedSet::try_from(vec![core::array::from_fn(|_| AnyCbor::new_uint(7))]).unwrap());
        assert!(serde_json::to_string(&natural).unwrap().contains("[[7,7"), "exact any stays natural");
        let duplicate_natural = format!(r#"{{"xs":[{0},{0}]}}"#, wide_json(7));
        let duplicate_natural_error = serde_json::from_str::<WideRejectAnyHolder>(&duplicate_natural)
            .expect_err("natural exact-any reject set must not normalize duplicates");
        assert!(
            duplicate_natural_error.to_string().contains("Duplicate key"),
            "natural exact-any duplicate uses the set carrier error: {duplicate_natural_error}"
        );
        let non_injective = WideRejectAnyHolder::new(OrderedSet::try_from(vec![core::array::from_fn(|_| AnyCbor::new_bytes(vec![1]))]).unwrap());
        assert!(serde_json::to_string(&non_injective).is_err(), "non-injective exact any remains loud");
    }

    fn wide_json(value: u64) -> String {
        format!("[{}]", vec![value.to_string(); 64].join(","))
    }

    #[test]
    fn dynamic_rows_compose_recursive_exact_arrays_without_changing_json_field_shape() {
        let wide = [7u64; 64];

        let loose = WideTailLoose::new(1);
        assert_eq!(serde_json::to_string(&loose).unwrap(), r#"{"prefix":1}"#);
        assert!(serde_json::from_str::<WideTailLoose>(r#"{"prefix":1}"#)
            .unwrap()
            .rest
            .is_empty());
        let mut loose = WideTailLoose::new(1);
        loose.rest = vec![wide];
        let loose_json = serde_json::to_string(&loose).unwrap();
        assert!(loose_json.contains(r#""rest":[[7,7"#), "loose row stays list-shaped: {loose_json}");
        assert_eq!(serde_json::from_str::<WideTailLoose>(&loose_json).unwrap().rest[0][63], 7);

        let nonempty = WideTailNonempty::new(1, wide);
        let bounded = WideTailBounded::new(1, BoundedVec::try_from(vec![wide, wide]).unwrap());
        let exact = WideTailExact::new(1, [wide, wide]);
        for value in [
            serde_json::to_value(&nonempty).unwrap(),
            serde_json::to_value(&bounded).unwrap(),
            serde_json::to_value(&exact).unwrap(),
        ] {
            assert_eq!(value["rest"][0].as_array().unwrap().len(), 64);
        }
        assert!(serde_json::from_str::<WideTailNonempty>(r#"{"prefix":1,"rest":[]}"#).is_err());
        assert!(serde_json::from_str::<WideTailNonempty>(r#"{"prefix":1}"#).is_err());
        assert!(serde_json::from_str::<WideTailBounded>(
            &format!(r#"{{"prefix":1,"rest":[{}]}}"#, wide_json(7))
        )
        .is_err(), "bounded below must enter its checked carrier door");
        assert!(serde_json::from_str::<WideTailBounded>(
            &format!(
                r#"{{"prefix":1,"rest":[{0},{1},{2},{3}]}}"#,
                wide_json(7), wide_json(8), wide_json(9), wide_json(10)
            )
        )
        .is_err(), "bounded above must enter its checked carrier door");
        assert!(serde_json::from_str::<WideTailBounded>(r#"{"prefix":1}"#).is_err());
        assert!(serde_json::from_str::<WideTailExact>(
            &format!(r#"{{"prefix":1,"rest":[{}]}}"#, wide_json(7))
        )
        .is_err(), "exact below must enter its checked static-array door");
        assert!(serde_json::from_str::<WideTailExact>(r#"{"prefix":1}"#).is_err());

        let mut middle = WideTailMiddle::new(1, "end".to_owned());
        middle.rest = vec![wide];
        let middle_json = serde_json::to_string(&middle).unwrap();
        assert!(middle_json.contains(r#""rest":[[7,7"#) && middle_json.contains(r#""suffix":"end"#));
        assert_eq!(serde_json::from_str::<WideTailMiddle>(&middle_json).unwrap().suffix, "end");

        let reject = OrderedSet::try_from(vec![wide]).unwrap();
        let mut nested_reject = WideTailReject::new(1);
        nested_reject.rest = vec![reject];
        let reject_json = serde_json::to_string(&nested_reject).unwrap();
        assert!(reject_json.contains(r#""rest":[[[7,7"#), "nested reject row stays list-shaped: {reject_json}");
        let reject_schema = serde_json::to_value(schemars::schema_for!(WideTailReject)).unwrap();
        assert_eq!(reject_schema["properties"]["rest"]["items"]["uniqueItems"], true);
        assert_eq!(reject_schema["properties"]["rest"]["items"]["items"]["minItems"], 64);
        assert_eq!(reject_schema["properties"]["rest"]["items"]["items"]["maxItems"], 64);
        assert!(serde_json::from_str::<WideTailReject>(
            &format!(r#"{{"prefix":1,"rest":[[{0},{0}]]}}"#, wide_json(7))
        )
        .is_err(), "nested reject must not normalize an actual duplicate");

        let natural: ShortAnyTail = core::array::from_fn(|_| AnyCbor::new_uint(5));
        let mut natural_row = WideTailNaturalAny::new(1);
        natural_row.rest = vec![vec![natural]];
        let natural_json = serde_json::to_string(&natural_row).unwrap();
        assert!(natural_json.contains(r#""rest":[[[5,5"#), "exact any stays natural: {natural_json}");
        assert_eq!(
            serde_json::from_str::<WideTailNaturalAny>(&natural_json)
                .unwrap()
                .rest[0][0][0]
                .as_uint(),
            Some(5),
            "natural exact-any row must decode through the recursive adapter too"
        );
        assert!(serde_json::to_string(&WideTailNaturalAny {
            prefix: 1,
            rest: vec![vec![core::array::from_fn(|_| AnyCbor::new_bytes(vec![1]))]],
        })
        .is_err(), "non-injective exact any stays loud");

        for (name, schema, min, max) in [
            ("nonempty", serde_json::to_value(schemars::schema_for!(WideTailNonempty)).unwrap(), 1, None),
            ("bounded", serde_json::to_value(schemars::schema_for!(WideTailBounded)).unwrap(), 2, Some(3)),
            ("exact", serde_json::to_value(schemars::schema_for!(WideTailExact)).unwrap(), 2, Some(2)),
        ] {
            let rest = &schema["properties"]["rest"];
            assert_eq!(rest["minItems"], min, "{name} outer min");
            if let Some(max) = max {
                assert_eq!(rest["maxItems"], max, "{name} outer max");
            }
            assert_eq!(rest["items"]["minItems"], 64, "{name} inner min");
            assert_eq!(rest["items"]["maxItems"], 64, "{name} inner max");
        }
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
    fn exact_middle_same_major_uses_static_carrier_and_checked_json_handover() {
        let value = ExactMiddle::new(7, 9, [2, 3]);
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
            [vec![0xaa], vec![0xbb]],
            [],
            [2, 3, 4],
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

    #[test]
    fn wide_typed_static_array_json_and_schema_remain_supported() {
        let value = WideHolder::new([7; 64]);
        let json = serde_json::to_string(&value).expect("wide static arrays serialize naturally");
        let back: WideHolder = serde_json::from_str(&json).expect("wide static arrays deserialize");
        assert_eq!(back.wide, value.wide);
        let schema = serde_json::to_value(schemars::schema_for!(WideHolder)).unwrap();
        assert_eq!(schema["properties"]["wide"]["minItems"], 64);
        assert_eq!(schema["properties"]["wide"]["maxItems"], 64);
        assert!(serde_json::from_str::<WideHolder>(r#"{"wide":[7]}"#).is_err());
    }

    #[test]
    fn optional_wide_static_array_keeps_absent_and_present_json_forms() {
        let absent = WideOptional::new(1);
        let absent_json = serde_json::to_string(&absent).unwrap();
        assert!(absent_json.contains(r#""wide":null"#));
        let present: WideOptional = serde_json::from_str(&format!(
            r#"{{"prefix":1,"wide":[{}]}}"#,
            vec!["7"; 64].join(",")
        ))
        .unwrap();
        assert_eq!(present.wide, Some([7; 64]));
        let explicit_null: WideOptional =
            serde_json::from_str(r#"{"prefix":1,"wide":null}"#).unwrap();
        assert_eq!(explicit_null.wide, None);

        let schema = serde_json::to_value(schemars::schema_for!(WideOptional)).unwrap();
        let branches = schema["properties"]["wide"]["anyOf"]
            .as_array()
            .expect("optional static-array schema must be array-or-null");
        let array = branches
            .iter()
            .find(|branch| branch["type"] == "array")
            .expect("array branch retained");
        assert_eq!(array["minItems"], 64);
        assert_eq!(array["maxItems"], 64);
        assert!(branches.iter().any(|branch| branch["type"] == "null"));

        let absent_any = WideAnyOptional::new(1);
        assert!(serde_json::to_string(&absent_any)
            .unwrap()
            .contains(r#""wide":null"#));
        let mut present_any = WideAnyOptional::new(1);
        present_any.wide = Some(std::array::from_fn(|_| AnyCbor::new_uint(7)));
        assert!(serde_json::to_string(&present_any)
            .unwrap()
            .contains(r#""wide":[7,7"#));
        let explicit_null_any: WideAnyOptional =
            serde_json::from_str(r#"{"prefix":1,"wide":null}"#).unwrap();
        assert_eq!(explicit_null_any.wide, None);
        let any_schema = serde_json::to_value(schemars::schema_for!(WideAnyOptional)).unwrap();
        let any_branches = any_schema["properties"]["wide"]["anyOf"]
            .as_array()
            .expect("optional natural static-array schema must be array-or-null");
        let any_array = any_branches
            .iter()
            .find(|branch| branch["type"] == "array")
            .expect("natural array branch retained");
        assert_eq!(any_array["minItems"], 64);
        assert_eq!(any_array["maxItems"], 64);
        assert!(any_branches.iter().any(|branch| branch["type"] == "null"));
    }

    #[test]
    fn nested_and_variant_wide_static_arrays_keep_json_and_schema_support() {
        let nested = WideListHolder::new(vec![[8; 64]]);
        let nested_json = serde_json::to_string(&nested).unwrap();
        let nested_back: WideListHolder = serde_json::from_str(&nested_json).unwrap();
        assert_eq!(nested_back.xs, nested.xs);
        let nested_schema = serde_json::to_value(schemars::schema_for!(WideListHolder)).unwrap();
        assert_eq!(nested_schema["properties"]["xs"]["items"]["minItems"], 64);
        assert_eq!(nested_schema["properties"]["xs"]["items"]["maxItems"], 64);

        let choice = WideChoice::new_wide([9; 64]);
        let choice_json = serde_json::to_string(&choice).unwrap();
        let choice_back: WideChoice = serde_json::from_str(&choice_json).unwrap();
        assert!(matches!(choice_back, WideChoice::Wide(values) if values == [9; 64]));
        // The derive itself is the schema compile proof for this newtype variant; the nested
        // assertion above verifies the adapter's concrete min/max schema shape.
        let _ = schemars::schema_for!(WideChoice);

        let any_choice = WideAnyChoice::new_arr_any(std::array::from_fn(|_| AnyCbor::new_uint(9)));
        let any_choice_json = serde_json::to_string(&any_choice).unwrap();
        assert!(
            any_choice_json.contains("[9,9"),
            "exact-any choice must render naturally, not as tagged AnyCbor: {any_choice_json}"
        );
        let any_choice_back: WideAnyChoice = serde_json::from_str(&any_choice_json).unwrap();
        assert!(matches!(any_choice_back, WideAnyChoice::ArrAny(values) if values[0].as_uint() == Some(9)));
        let any_choice_schema = serde_json::to_string(&schemars::schema_for!(WideAnyChoice)).unwrap();
        assert!(
            any_choice_schema.contains("\"minItems\":64")
                && any_choice_schema.contains("\"maxItems\":64"),
            "exact-any choice schema retains static cardinality: {any_choice_schema}"
        );
    }
}
