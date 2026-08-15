use super::serialization::{Deserialize, ToCBORBytes};
use super::*;

const ABSENT: &[u8] = &[0xa1, 0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07];
const FOREIGN_PRESENT: &[u8] = &[
    0xa5, 0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07, 0x69, b'z', b'e', b'r',
    b'o', b'_', b's', b't', b'a', b'r', 0x01, 0x6c, b'z', b'e', b'r', b'o', b'_', b'b', b'o',
    b'u', b'n', b'd', b'e', b'd', 0x61, b'x', 0x68, b'm', b'a', b'x', b'_', b'o', b'n', b'l',
    b'y', 0xf5, 0x65, b'f', b'i', b'x', b'e', b'd', 0x05,
];

#[test]
fn zero_permitting_keyed_map_fields_decode_and_serialize_as_optional() {
    let absent = ZeroPermittingMap::from_cbor_bytes(ABSENT).expect("absent fixed keys are valid");
    assert_eq!(absent.required, 7);
    assert_eq!(absent.zero_star, None);
    assert_eq!(absent.zero_bounded, None);
    assert_eq!(absent.max_only, None);
    assert!(!absent.fixed);
    assert_eq!(absent.to_cbor_bytes(), ABSENT, "None must omit every optional entry");

    let present = ZeroPermittingMap::from_cbor_bytes(FOREIGN_PRESENT)
        .expect("a foreign producer's present fixed keys are valid");
    assert_eq!(present.required, 7);
    assert_eq!(present.zero_star, Some(1));
    assert_eq!(present.zero_bounded.as_deref(), Some("x"));
    assert_eq!(present.max_only, Some(true));
    assert!(present.fixed, "fixed-value presence uses its established bool carrier");
    let reencoded = present.to_cbor_bytes();
    assert_eq!(reencoded[0], 0xa5, "Some values must write all five map entries");
    let redecoded = ZeroPermittingMap::from_cbor_bytes(&reencoded)
        .expect("the serializer's present fields must remain decodable");
    assert_eq!(redecoded.zero_star, Some(1));
    assert_eq!(redecoded.zero_bounded.as_deref(), Some("x"));
    assert_eq!(redecoded.max_only, Some(true));
    assert!(redecoded.fixed);
}

#[test]
fn zero_permitting_keyed_map_fields_compile_and_round_trip_through_json() {
    let absent = ZeroPermittingMap::from_cbor_bytes(ABSENT).unwrap();
    let json = serde_json::to_string(&absent).expect("json serialize");
    assert!(json.contains("\"zero_star\":null"), "the established optional JSON projection must remain usable");
    let back: ZeroPermittingMap = serde_json::from_str(&json).expect("json deserialize");
    assert_eq!(back.to_cbor_bytes(), ABSENT);
}

#[test]
fn exact_zero_keyed_map_fields_are_forbidden_on_cbor_and_checked_open_construction() {
    let closed_absent = ZeroExactClosed::from_cbor_bytes(&[0xa1, 0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07])
        .expect("the omitted exact-zero key is valid");
    assert_eq!(closed_absent.required, 7);
    let closed_forbidden = ZeroExactClosed::from_cbor_bytes(&[
        0xa2, 0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07,
        0x69, b'f', b'o', b'r', b'b', b'i', b'd', b'd', b'e', b'n', 0x01,
    ]).expect_err("the exact-zero key must not decode");
    assert!(matches!(closed_forbidden.failure(), DeserializeFailure::ForbiddenKey(_)));
    let closed_indefinite = ZeroExactClosed::from_cbor_bytes(&[
        0xbf, 0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07,
        0x69, b'f', b'o', b'r', b'b', b'i', b'd', b'd', b'e', b'n', 0x01, 0xff,
    ]).expect_err("an indefinite map still rejects the forbidden fixed key");
    assert!(matches!(closed_indefinite.failure(), DeserializeFailure::ForbiddenKey(_)));

    let mut allowed = std::collections::BTreeMap::new();
    allowed.insert("other".to_owned(), 9);
    let mut open = ZeroExactOpen::new(7, allowed).expect("an allowed complete rest map constructs");
    assert_eq!(open.to_cbor_bytes()[0], 0xa2);
    open.insert_rest("second".to_owned(), 10)
        .expect("the checked insertion door accepts an allowed key");
    let insert_err = open
        .insert_rest("forbidden".to_owned(), 1)
        .expect_err("the checked insertion door rejects the forbidden key");
    assert!(matches!(insert_err.failure(), DeserializeFailure::ForbiddenKey(_)));
    let mut forbidden = std::collections::BTreeMap::new();
    forbidden.insert("forbidden".to_owned(), 1);
    let err = ZeroExactOpen::new(7, forbidden).expect_err("construction checks the complete rest map");
    assert!(matches!(err.failure(), DeserializeFailure::ForbiddenKey(_)));

    let closed_json_error = serde_json::from_str::<ZeroExactClosed>(r#"{"required":7,"forbidden":1}"#)
        .expect_err("the exact-zero JSON property must be rejected by its named sentinel");
    assert!(
        closed_json_error.to_string().contains("Forbidden key: \"forbidden\""),
        "the serde error must display the shared structured failure: {closed_json_error}"
    );
    // Retain serde's established closed-record behavior for names that are NOT exact-zero
    // constraints. The sentinel must not become a broad `deny_unknown_fields` policy.
    serde_json::from_str::<ZeroExactClosed>(r#"{"required":7,"other":1}"#)
        .expect("an unrelated unknown JSON property keeps the established accepted behavior");
    let open_json_error = serde_json::from_str::<ZeroExactOpen>(r#"{"required":7,"forbidden":1}"#)
        .expect_err("the flattened exact-zero property must be rejected");
    assert!(
        open_json_error.to_string().contains("Forbidden key: \"forbidden\""),
        "open JSON must render the shared structured failure: {open_json_error}"
    );
}

#[test]
fn exact_zero_multiple_typed_any_and_ignore_rows_keep_value_level_constraints() {
    let absent = ZeroExactNamedMulti::from_cbor_bytes(&[
        0xa1, 0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07,
    ]).expect("multiple omitted exact-zero keys are valid");
    assert_eq!(absent.required, 7);
    let uint_error = ZeroExactNamedMulti::from_cbor_bytes(&[
        0xa2, 0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07, 0x18, 41, 0x01,
    ]).expect_err("the uint exact-zero key rejects");
    assert!(matches!(uint_error.failure(), DeserializeFailure::ForbiddenKey(Key::Uint(41))));
    let text_error = ZeroExactNamedMulti::from_cbor_bytes(&[
        0xa2, 0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07, 0x6d, b'o', b'r', b'i',
        b'g', b'i', b'n', b'a', b'l', b'_', b't', b'e', b'x', b't', 0x01,
    ]).expect_err("the text exact-zero key rejects");
    assert!(matches!(text_error.failure(), DeserializeFailure::ForbiddenKey(Key::Str(key)) if key == "original_text"));

    let nonminimal_forbidden_text = [
        0x78, 0x09, b'f', b'o', b'r', b'b', b'i', b'd', b'd', b'e', b'n',
    ];
    let typed_wire = [
        0xa2, 0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07,
        0x78, 0x09, b'f', b'o', b'r', b'b', b'i', b'd', b'd', b'e', b'n', 0x01,
    ];
    let typed_error = ZeroExactTypedOpen::from_cbor_bytes(&typed_wire)
        .expect_err("a non-minimal typed union key compares by CBOR value");
    assert!(matches!(typed_error.failure(), DeserializeFailure::ForbiddenKey(_)));
    let typed_key = RestKey::from_cbor_bytes(&nonminimal_forbidden_text).unwrap();
    let typed_rest = [(typed_key.clone(), 1)].into_iter().collect();
    assert!(matches!(
        ZeroExactTypedOpen::new(7, typed_rest).unwrap_err().failure(),
        DeserializeFailure::ForbiddenKey(_)
    ));
    let mut typed = ZeroExactTypedOpen::new(7, [(RestKey::Text("other".to_owned()), 1)].into_iter().collect()).unwrap();
    assert!(matches!(typed.insert_rest(typed_key, 1).unwrap_err().failure(), DeserializeFailure::ForbiddenKey(_)));

    let any_key = any_cbor::AnyCbor::from_cbor_bytes(&nonminimal_forbidden_text).unwrap();
    let any_rest = [(any_key.clone(), 1)].into_iter().collect();
    assert!(matches!(
        ZeroExactAnyOpen::new(7, any_rest).unwrap_err().failure(),
        DeserializeFailure::ForbiddenKey(_)
    ));
    let mut any_open = ZeroExactAnyOpen::new(
        7,
        [(any_cbor::AnyCbor::new_text("other".to_owned()), 1)]
            .into_iter()
            .collect(),
    )
    .unwrap();
    assert!(matches!(
        any_open.insert_rest(any_key, 1).unwrap_err().failure(),
        DeserializeFailure::ForbiddenKey(_)
    ));

    let any_wire_error = ZeroExactAnyOpen::from_cbor_bytes(&typed_wire)
        .expect_err("an any-domain rest row must not capture the non-minimal forbidden key");
    assert!(matches!(
        any_wire_error.failure(),
        DeserializeFailure::ForbiddenKey(_)
    ));

    let ignored_allowed = ZeroExactIgnore::from_cbor_bytes(&[
        0xa2, 0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07, 0x65, b'o', b't',
        b'h', b'e', b'r', 0x01,
    ])
    .expect("an unrelated key still follows the ignore row's tolerate-and-drop contract");
    assert_eq!(
        ignored_allowed.to_cbor_bytes(),
        [0xa1, 0x68, b'r', b'e', b'q', b'u', b'i', b'r', b'e', b'd', 0x07]
    );
    let ignored_forbidden = ZeroExactIgnore::from_cbor_bytes(&typed_wire)
        .expect_err("the forbidden arm must run before an @ignore rest catch-all");
    assert!(matches!(
        ignored_forbidden.failure(),
        DeserializeFailure::ForbiddenKey(_)
    ));
}

#[test]
fn exact_zero_restricted_rest_insertions_preserve_carrier_windows() {
    use super::bounded_map::BoundedMap;
    use super::pair_map::BoundedPairMap;

    let bounded = BoundedMap::<String, u64, 1, 2>::try_from(
        [("allowed".to_owned(), 1)]
            .into_iter()
            .collect::<std::collections::BTreeMap<_, _>>(),
    )
    .unwrap();
    let mut bounded = ZeroExactBoundedOpen::new(7, bounded).unwrap();
    bounded.insert_rest("second".to_owned(), 2).unwrap();
    assert_eq!(bounded.rest().len(), 2);
    assert!(bounded.insert_rest("third".to_owned(), 3).is_err(), "MAX must remain checked");
    let forbidden = bounded
        .insert_rest("forbidden".to_owned(), 4)
        .expect_err("forbidden keys must be rejected even when the carrier is full");
    assert!(matches!(forbidden.failure(), DeserializeFailure::ForbiddenKey(_)));
    assert_eq!(bounded.rest().len(), 2, "failed insertions leave the checked carrier intact");

    let pairs = BoundedPairMap::<String, u64, 1, 2>::try_from(vec![("allowed".to_owned(), 1)])
        .unwrap();
    let mut pairs = ZeroExactPairOpen::new(7, pairs).unwrap();
    pairs.insert_rest("allowed".to_owned(), 2).unwrap();
    assert_eq!(pairs.rest().len(), 2, "preserve carrier keeps a duplicate pair");
    assert!(pairs.insert_rest("third".to_owned(), 3).is_err(), "pair max counts every pair");
    let forbidden = pairs
        .insert_rest("forbidden".to_owned(), 4)
        .expect_err("forbidden pair must be rejected before append");
    assert!(matches!(forbidden.failure(), DeserializeFailure::ForbiddenKey(_)));
    assert_eq!(pairs.rest().len(), 2);
}

#[test]
fn exact_zero_checked_doors_cover_non_empty_and_loose_pair_rest_carriers() {
    use super::non_empty_map::NonEmptyMap;
    use super::pair_map::PairMap;

    let mut non_empty = ZeroExactNonEmptyOpen::new(
        7,
        NonEmptyMap::new("allowed".to_owned(), 1),
    )
    .expect("the complete non-empty rest crosses the checked constructor");
    non_empty
        .insert_rest("second".to_owned(), 2)
        .expect("the non-empty carrier retains its ordinary insertion surface");
    let non_empty_forbidden = non_empty
        .insert_rest("forbidden".to_owned(), 3)
        .expect_err("the checked door rejects before an infallible NonEmptyMap insertion");
    assert!(matches!(
        non_empty_forbidden.failure(),
        DeserializeFailure::ForbiddenKey(_)
    ));
    assert_eq!(
        non_empty.rest().len(),
        2,
        "a forbidden non-empty insertion must leave the carrier unchanged"
    );

    let mut pairs = ZeroExactLoosePairOpen::new(
        7,
        PairMap::from(vec![("allowed".to_owned(), 1)]),
    )
    .expect("the complete loose pair-map crosses the checked constructor");
    pairs
        .insert_rest("allowed".to_owned(), 2)
        .expect("the loose pair map retains duplicate entries");
    let pair_forbidden = pairs
        .insert_rest("forbidden".to_owned(), 3)
        .expect_err("the checked door rejects before append");
    assert!(matches!(
        pair_forbidden.failure(),
        DeserializeFailure::ForbiddenKey(_)
    ));
    assert_eq!(
        pairs.rest().len(),
        2,
        "a forbidden loose pair insertion must leave duplicate-preserving state unchanged"
    );
}

#[test]
fn exact_zero_json_sentinels_cover_multiple_generated_names_without_schema_leakage() {
    for (json, display) in [
        (r#"{"required":7,"no_uint":1}"#, "Forbidden key: 41"),
        (
            r#"{"required":7,"no_text":1}"#,
            "Forbidden key: \"original_text\"",
        ),
    ] {
        let error = serde_json::from_str::<ZeroExactNamedMulti>(json)
            .expect_err("each generated exact-zero property must route through its sentinel");
        assert!(
            error.to_string().contains(display),
            "expected shared ForbiddenKey display {display:?}, got {error}"
        );
    }
    assert!(
        serde_json::from_str::<ZeroExactNamedMulti>(r#"{"required":7,"ordinary":1}"#).is_ok(),
        "unrelated closed-record properties retain serde's existing ignored-unknown behavior"
    );
    let schema = serde_json::to_value(schemars::schema_for!(ZeroExactNamedMulti)).unwrap();
    let properties = schema
        .get("properties")
        .expect("schemars root object exposes declared properties");
    assert!(properties.get("no_uint").is_none() && properties.get("no_text").is_none());
    assert!(
        properties
            .as_object()
            .unwrap()
            .keys()
            .all(|name| !name.starts_with("__cddl_exact_zero_")),
        "private sentinels must be skipped from JSON Schema"
    );
}
