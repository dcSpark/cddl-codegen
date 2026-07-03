// Wasm-boundary read-fidelity coverage for the three-state (absent / present-null / present-value)
// accessors. wasm-bindgen can't represent Option<Option<T>>, so a nullable value at a
// presence-adding position flattens to Option<T> on read: the plain getter reports None for BOTH an
// absent slot and a present-but-null one. The additive presence accessors (`has_<field>()`, map
// `has(key)`) restore the lost bit WITHOUT changing any existing getter signature; the single-nested
// enum variant needs no new accessor because `kind()` + `as_variant()` is already unambiguous.
//
// The wasm WRITE surface is fully three-state-expressive (this is a read-side flattening, not a
// write-side one): a nullable setter/insert takes the inner `Option<T>` and wraps it in the outer
// presence-`Option`, so `set_field0(None)` stores `Some(None)` (present-null) and `set_field0(Some(v))`
// stores `Some(Some(v))` (present-value); skipping the setter leaves the ctor's `None` (absent).
// Likewise map `insert(k, None)` stores a present-null entry, `insert(k, Some(v))` a present-value one,
// and an un-inserted key is absent. So each state is constructed BOTH through the rust API AND through
// the wasm write surface below, and read back through the wasm accessors; we assert the three
// observations are PAIRWISE DISTINCT (the distinguishability the two retired `#[ignore]` stubs stood
// in for) and that the wasm-written present-null is observed as present-null, pinning the corrected
// write-side claim in docs/docs/wasm_differences.mdx.

#[test]
fn wasm_optional_nullable_field_three_state_fidelity() {
    // absent: outer None
    let absent: NullableOptionalField = cddl_lib::NullableOptionalField::new(1).into();
    // present-null: Some(None)
    let present_null: NullableOptionalField = {
        let mut n = cddl_lib::NullableOptionalField::new(1);
        n.field0 = Some(None);
        n.into()
    };
    // present-value: Some(Some(7))
    let present_value: NullableOptionalField = {
        let mut n = cddl_lib::NullableOptionalField::new(1);
        n.field0 = Some(Some(7));
        n.into()
    };

    // Read protocol: (has=false) -> absent; (has=true, get=None) -> present-null; (has=true, get=Some) -> value.
    let obs_absent = (absent.has_field0(), absent.field0());
    let obs_null = (present_null.has_field0(), present_null.field0());
    let obs_value = (present_value.has_field0(), present_value.field0());

    assert_eq!(obs_absent, (false, None));
    assert_eq!(obs_null, (true, None));
    assert_eq!(obs_value, (true, Some(7)));

    // Pairwise distinct through the wasm accessors — the plain `field0()` alone cannot separate
    // absent from present-null (both None); with `has_field0()` all three are distinguishable.
    assert_ne!(obs_absent, obs_null);
    assert_ne!(obs_absent, obs_value);
    assert_ne!(obs_null, obs_value);

    // Now mint the same three states through the WASM WRITE surface (not the rust API): the setter
    // wraps its inner-Option argument in the outer presence-Option, so present-null IS constructible
    // from wasm — `set_field0(None)` -> Some(None). (Contradicts the old "no wasm setter can mint
    // present-null" claim; see docs/docs/wasm_differences.mdx "Write-side semantics".)
    let w_absent = NullableOptionalField::new(1); // never call the setter -> absent
    let mut w_null = NullableOptionalField::new(1);
    w_null.set_field0(None); // present-null via the wasm setter
    let mut w_value = NullableOptionalField::new(1);
    w_value.set_field0(Some(7)); // present-value via the wasm setter

    assert_eq!((w_absent.has_field0(), w_absent.field0()), (false, None));
    assert_eq!((w_null.has_field0(), w_null.field0()), (true, None));
    assert_eq!((w_value.has_field0(), w_value.field0()), (true, Some(7)));
}

#[test]
fn wasm_nullable_map_value_three_state_fidelity() {
    // Build one map holding a present-null entry and a present-value entry; a third key is absent.
    let map: NullableMapValue = {
        let mut m = cddl_lib::NullableMapValue::new();
        m.insert(10, None); // present-null
        m.insert(20, Some(7)); // present-value
                               // key 30 absent
        m.into()
    };

    // Read protocol: (has=false) -> absent key; (has=true, get=None) -> present-null; (has=true, get=Some) -> value.
    let obs_absent = (map.has(30), map.get(30));
    let obs_null = (map.has(10), map.get(10));
    let obs_value = (map.has(20), map.get(20));

    assert_eq!(obs_absent, (false, None));
    assert_eq!(obs_null, (true, None));
    assert_eq!(obs_value, (true, Some(7)));

    assert_ne!(obs_absent, obs_null);
    assert_ne!(obs_absent, obs_value);
    assert_ne!(obs_null, obs_value);

    // Same states, but built entirely through the WASM insert (not the rust API): `insert(k, None)`
    // stores a present-null entry, `insert(k, Some(v))` a present-value one, an un-inserted key is
    // absent. Confirms the wasm write surface can mint present-null (pins the corrected doc claim).
    let mut wm = NullableMapValue::new();
    wm.insert(10, None); // present-null via wasm insert
    wm.insert(20, Some(7)); // present-value via wasm insert
                            // key 30 never inserted -> absent
    assert_eq!((wm.has(30), wm.get(30)), (false, None));
    assert_eq!((wm.has(10), wm.get(10)), (true, None));
    assert_eq!((wm.has(20), wm.get(20)), (true, Some(7)));
}

// Regression guard for the `has_<field>` name-collision skip (generation.rs). `has_name_collision`
// has an optional-nullable `field0` (whose flattening getter would synthesize `has_field0()`) AND a
// sibling field literally named `has_field0`. The generator must SKIP the synthesized accessor so the
// wasm crate compiles with a single `pub fn has_field0` (the sibling field's own getter). That this
// test module compiles at all is the proof; the assertion just exercises the surviving getters.
#[test]
fn wasm_has_field_name_collision_compiles() {
    let v: HasNameCollision = cddl_lib::HasNameCollision::new(true).into();
    // `has_field0()` here is the SIBLING FIELD's getter (a bool), not a synthesized presence accessor.
    assert!(v.has_field0());
    // `field0` (the optional-nullable field) still has its flattening getter; absent by default.
    assert_eq!(v.field0(), None);
}

#[test]
fn wasm_enum_nullable_variant_three_state_fidelity() {
    // Single-nested nullable variant: recoverable via kind() (no new accessor). Three states:
    // a DIFFERENT variant, this-variant-inner-null, and this-variant-with-value.
    let other: NullableEnum = cddl_lib::NullableEnum::new_text("x".to_string()).into();
    let inner_null: NullableEnum = cddl_lib::NullableEnum::new_opt_u64(None).into();
    let value: NullableEnum = cddl_lib::NullableEnum::new_opt_u64(Some(7)).into();

    // Read protocol: kind() resolves the variant; then for the OptU64 variant, as_opt_u64()==None
    // means inner-null and Some means value (kind() already told us it IS the variant).
    let is_opt = |e: &NullableEnum| matches!(e.kind(), NullableEnumKind::OptU64);
    let obs_other = (is_opt(&other), other.as_opt_u64());
    let obs_null = (is_opt(&inner_null), inner_null.as_opt_u64());
    let obs_value = (is_opt(&value), value.as_opt_u64());

    assert_eq!(obs_other, (false, None)); // different variant
    assert_eq!(obs_null, (true, None)); // this variant, inner null
    assert_eq!(obs_value, (true, Some(7))); // this variant, value

    assert_ne!(obs_other, obs_null);
    assert_ne!(obs_other, obs_value);
    assert_ne!(obs_null, obs_value);
}
