//! In-bin unit tests for the `static/bounded_map.rs` runtime, hosted via the same
//! `include!`-a-static-file pattern as `ordered_set_runtime_tests` (the runtime ships into generated
//! crates, so nothing else in THIS crate compiles it).
//!
//! The heavy decode-conformance replay owns the end-to-end rejection-reason contract. This cheap
//! lockstep test pins the four unique-key table boundaries whose generated decode door is
//! `BoundedMap`: omitted exact-once, bounded below/above, and optional above. It catches a runtime
//! diagnostic/catalog-oracle drift without rebuilding every decode-conformance crate.

// The included runtime files legitimately trigger lints a generated crate silences at crate level:
// `dead_code` (only the checked door is exercised here), `upper_case_acronyms` (`error.rs`'s `CBOR`
// variant), and `assertions_on_result_states` (the included file's own small `#[cfg(test)]` module
// uses `is_err`; this crate's check.ts Clippy profile denies that harness-only spelling).
#[allow(dead_code, clippy::upper_case_acronyms)]
mod error {
    include!("../../static/error.rs");
}
#[allow(dead_code, clippy::assertions_on_result_states)]
mod bounded_map {
    include!("../../static/bounded_map.rs");
}

use std::collections::BTreeMap;

use bounded_map::BoundedMap;
use error::DeserializeError;

fn runtime_reason(err: DeserializeError) -> String {
    err.to_string()
        .strip_prefix("Deserialization: ")
        .expect("an unannotated BoundedMap error must use the standard Display prefix")
        .to_owned()
}

fn catalog_expect_err<'a>(catalog: &'a toml::Value, row_id: &str, hex: &str) -> &'a str {
    let rows = catalog
        .get("row")
        .and_then(toml::Value::as_array)
        .expect("catalog.toml has [[row]] entries");
    let row = rows
        .iter()
        .find(|row| row.get("id").and_then(toml::Value::as_str) == Some(row_id))
        .unwrap_or_else(|| panic!("catalog.toml is missing row {row_id}"));
    let vector = row
        .get("vector")
        .and_then(toml::Value::as_array)
        .and_then(|vectors| {
            vectors
                .iter()
                .find(|vector| vector.get("hex").and_then(toml::Value::as_str) == Some(hex))
        })
        .unwrap_or_else(|| panic!("catalog row {row_id} is missing vector {hex}"));
    vector
        .get("expect_err")
        .and_then(toml::Value::as_str)
        .unwrap_or_else(|| panic!("catalog row {row_id} vector {hex} is missing expect_err"))
}

fn assert_pin(catalog: &toml::Value, row_id: &str, hex: &str, runtime_reason: String) {
    assert_eq!(
        catalog_expect_err(catalog, row_id, hex),
        runtime_reason,
        "catalog rejection-reason pin drifted from the BoundedMap runtime for {row_id} vector {hex}"
    );
}

#[test]
fn decode_catalog_bounded_table_reason_pins_match_runtime() {
    let catalog: toml::Value =
        toml::from_str(include_str!("../../tests/decode_conformance/catalog.toml"))
            .expect("decode-conformance catalog is valid TOML");

    let exact = BoundedMap::<u64, u64, 1, 1>::try_from(BTreeMap::new()).unwrap_err();
    assert_pin(
        &catalog,
        "contain.map-key.memberkey.type1.tstr_arrow_nooccur",
        "8200a0",
        runtime_reason(exact),
    );

    let below = BoundedMap::<u64, u64, 2, 3>::try_from(BTreeMap::from([(1, 1)])).unwrap_err();
    assert_pin(
        &catalog,
        "contain.occurrence-target.memberkey.type1.bounded_table",
        "8200a1616101",
        runtime_reason(below),
    );

    let above =
        BoundedMap::<u64, u64, 2, 3>::try_from(BTreeMap::from([(1, 1), (2, 2), (3, 3), (4, 4)]))
            .unwrap_err();
    assert_pin(
        &catalog,
        "contain.occurrence-target.memberkey.type1.bounded_table",
        "8200a4616101616202616303616404",
        runtime_reason(above),
    );

    let optional =
        BoundedMap::<u64, u64, 0, 1>::try_from(BTreeMap::from([(1, 1), (2, 2)])).unwrap_err();
    assert_pin(
        &catalog,
        "contain.occurrence-target.memberkey.type1.optional_table",
        "8200a2616101616202",
        runtime_reason(optional),
    );
}
