// Wasm-boundary read-fidelity coverage for the three-state (absent / present-null / present-value)
// accessors. wasm-bindgen can't represent Option<Option<T>>, so a nullable value at a
// presence-adding position flattens to Option<T> on read: the plain getter reports None for BOTH an
// absent slot and a present-but-null one. The additive presence accessors (`has_<field>()`, map
// `has(key)`) restore the lost bit WITHOUT changing any existing getter signature; the single-nested
// enum variant needs no new accessor because `kind()` + `as_variant()` is already unambiguous.
//
// Each state is constructed through the RUST API (the wasm wrapper carries From<cddl_lib::T>) so the
// absent-vs-present-null pair — which no wasm setter/constructor can mint (setters always wrap in an
// outer Some) — is actually exercised. We then read each state back through the wasm accessors and
// assert the three observations are PAIRWISE DISTINCT: that distinguishability is the property the two
// retired `#[ignore]` stubs stood in for.

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
