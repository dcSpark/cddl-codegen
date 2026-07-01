// Smoke test for run_test's tests_wasm.rs hook: proves the file's contents actually land in the
// generated wasm crate and execute. A generated wasm crate ships no #[test]s of its own, so
// without this a broken hook would `cargo test` zero tests and pass vacuously (which it silently
// did before the append was wired up). Substantive wasm behavioural coverage belongs to
// tests/TESTING_ROADMAP.md item 2 — keep this one trivial.
#[test]
fn tests_wasm_hook_is_wired() {
    let foo = Foo::new(42, String::from("wasm-hook"), vec![0xCA, 0xFE]);
    let bytes = foo.to_cbor_bytes();
    assert!(!bytes.is_empty());
    let back = match Foo::from_cbor_bytes(&bytes) {
        Ok(f) => f,
        Err(_) => panic!("Foo::from_cbor_bytes failed on to_cbor_bytes output"),
    };
    assert_eq!(back.index_0(), 42);
    assert_eq!(back.index_1(), "wasm-hook");
    assert_eq!(back.index_2(), vec![0xCA, 0xFE]);
}
