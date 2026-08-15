#[test]
fn wasm_optional_getters_preserve_zero_permitting_map_field_presence() {
    let absent: ZeroPermittingMap = cddl_lib::ZeroPermittingMap::new(7).into();
    assert_eq!(absent.zero_star(), None);
    assert_eq!(absent.zero_bounded(), None);
    assert_eq!(absent.max_only(), None);
    assert!(!absent.fixed());

    let present: ZeroPermittingMap = {
        let mut native = cddl_lib::ZeroPermittingMap::new(7);
        native.zero_star = Some(1);
        native.zero_bounded = Some("x".to_owned());
        native.max_only = Some(true);
        native.fixed = true;
        native.into()
    };
    assert_eq!(present.zero_star(), Some(1));
    assert_eq!(present.zero_bounded().as_deref(), Some("x"));
    assert_eq!(present.max_only(), Some(true));
    assert!(present.fixed());
}

#[test]
fn wasm_exact_zero_open_rest_observes_only_validated_native_state() {
    let mut allowed = MapTextToU64::new();
    allowed.insert("allowed".to_owned(), 9);
    let wasm = ZeroExactOpen::new(7, &allowed)
        .expect("the wasm constructor crosses the complete rest through the native checked door");
    assert_eq!(wasm.rest().len(), 1, "wasm getter exposes the immutable validated rest view");

    let mut forbidden = MapTextToU64::new();
    forbidden.insert("forbidden".to_owned(), 1);
    let forbidden_native: std::collections::BTreeMap<String, u64> = forbidden.into();
    assert!(
        cddl_lib::ZeroExactOpen::new(7, forbidden_native).is_err(),
        "a forbidden complete wrapper must fail before it can reach the wasm value"
    );
}
