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

// `rest()` intentionally mints an owned wrapper snapshot. The record-level `insert_rest` door is
// therefore the only way a wasm caller mutates the parent. Exercise that distinction against both
// map policies in the generated WASM crate itself: a source-shape assertion cannot prove which
// owner a mutable wrapper updates.
#[test]
fn wasm_open_rest_parent_mutation_is_not_a_snapshot_mutation() {
    let mut ordinary = WasmMutationOpen::new(7);
    let mut detached = ordinary.rest();
    detached.insert("detached".to_owned(), 1);
    assert_eq!(
        ordinary.rest().len(),
        0,
        "mutating a getter snapshot must not mutate the parent record"
    );

    ordinary
        .insert_rest("parent".to_owned(), 2)
        .expect("record mutation door updates the ordinary parent map");
    assert_eq!(ordinary.rest().get("parent".to_owned()), Some(2));
    let ordinary_bytes = ordinary.to_cbor_bytes();
    let ordinary_decoded = WasmMutationOpen::from_cbor_bytes(&ordinary_bytes)
        .ok()
        .expect("the same wasm boundary decodes the parent serialization");
    assert_eq!(ordinary_decoded.rest().get("parent".to_owned()), Some(2));
    let mut ordinary_decoded = ordinary_decoded;
    ordinary_decoded
        .insert_rest("after_decode".to_owned(), 3)
        .expect("a decoded parent retains its record mutation door");
    let ordinary_redecoded = WasmMutationOpen::from_cbor_bytes(&ordinary_decoded.to_cbor_bytes())
        .ok()
        .expect("a decode-mutate-serialize cycle remains readable on the wasm boundary");
    assert_eq!(ordinary_redecoded.rest().get("parent".to_owned()), Some(2));
    assert_eq!(ordinary_redecoded.rest().get("after_decode".to_owned()), Some(3));

    let mut preserve = WasmMutationPairOpen::new(7);
    let mut detached = preserve.rest();
    detached.insert("same".to_owned(), 1);
    assert_eq!(
        preserve.rest().len(),
        0,
        "a preserve getter snapshot is detached too"
    );

    preserve
        .insert_rest("same".to_owned(), 2)
        .expect("first parent pair insertion");
    preserve
        .insert_rest("same".to_owned(), 3)
        .expect("preserve parent insertion appends an equal key");
    assert_eq!(
        preserve.rest().len(),
        2,
        "the duplicate-preserving parent carrier retains both pairs"
    );
    assert_eq!(preserve.rest().get("same".to_owned()), Some(2));
    let preserve_bytes = preserve.to_cbor_bytes();
    let preserve_decoded = WasmMutationPairOpen::from_cbor_bytes(&preserve_bytes)
        .ok()
        .expect("the same wasm boundary decodes both preserved entries");
    assert_eq!(preserve_decoded.rest().len(), 2);
    assert_eq!(preserve_decoded.rest().get("same".to_owned()), Some(2));
    let mut preserve_decoded = preserve_decoded;
    preserve_decoded
        .insert_rest("same".to_owned(), 4)
        .expect("a decoded preserve parent appends through its own door");
    let preserve_redecoded = WasmMutationPairOpen::from_cbor_bytes(&preserve_decoded.to_cbor_bytes())
        .ok()
        .expect("a decoded preserve parent keeps all pairs after mutation");
    assert_eq!(preserve_redecoded.rest().len(), 3);
    assert_eq!(preserve_redecoded.rest().get("same".to_owned()), Some(2));
}

#[test]
fn wasm_open_rest_checked_parent_mutation_keeps_exact_zero_and_max_invariants() {
    use cddl_lib::bounded_map::BoundedMap;

    let native = cddl_lib::ZeroExactBoundedOpen::new(
        7,
        BoundedMap::<String, u64, 1, 2>::try_from(
            [("allowed".to_owned(), 1)]
                .into_iter()
                .collect::<std::collections::BTreeMap<_, _>>(),
        )
        .expect("one entry is inside the bounded window"),
    )
    .expect("the initial key is allowed");
    let mut bounded: ZeroExactBoundedOpen = native.into();
    bounded
        .insert_rest("second".to_owned(), 2)
        .expect("a valid bounded parent insertion succeeds");
    assert_eq!(bounded.rest().len(), 2);

    // Host `cargo test` cannot construct a wasm-bindgen `JsError` (its imported JS constructor
    // panics off wasm32). Catch that terminal bridge panic: reaching it proves the native checked
    // door returned an error, and inspecting the same wrapper afterward proves the failed mutation
    // was atomic. On wasm32 the identical path returns `Err(JsError)` to JavaScript instead.
    assert!(
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _ = bounded.insert_rest("forbidden".to_owned(), 3);
        }))
        .is_err(),
        "exact-zero validation is delegated to the native parent door"
    );
    assert_eq!(bounded.rest().len(), 2, "forbidden insertion is atomic");
    assert!(
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _ = bounded.insert_rest("overflow".to_owned(), 3);
        }))
        .is_err(),
        "the bounded carrier rejects an overflow through the parent door"
    );
    assert_eq!(bounded.rest().len(), 2, "overflow insertion is atomic");
}
