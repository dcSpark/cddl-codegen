// This fixture hook makes `integration_tests::bounded_dynamic_map_rows_wasm_compile` run
// `cargo test` for the wasm crate, so its generated `cddl_generated_wasm_tests` module is compiled
// and executed rather than merely left behind a cargo-build floor.
#[test]
fn bounded_dynamic_map_rows_emit_tests_execute() {}
