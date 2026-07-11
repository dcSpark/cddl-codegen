// Behavioral floor for the `--extern-wrapper-index` deferral feature, run in the generated WASM
// crate (native `cargo test`; wasm-bindgen impls compile fine off-target). It drives the consumer's
// wasm API entirely through the DEFERRED wrapper classes — `IdxFooList`, `MapU64ToIdxFoo`, and (as a
// map keys-list) `IdxFooList` again — which are imported from `index_dep_crate_wasm::collections`,
// NOT defined here. Constructing them via the dep's classes, feeding them into `Everything::new`,
// round-tripping through CBOR, and reading them back off the getters exercises the whole cross-crate
// boundary the feature relies on (`From<Vec<..>>` / `From<OrderedHashMap<..>>` in both directions),
// plus R3d: `foo_keyed.keys()` builds the deferred `IdxFooList` via `.into()` rather than
// tuple-struct syntax.
#[cfg(test)]
mod extern_wrapper_index_roundtrip {
    use super::*;

    #[test]
    fn deferred_wrappers_construct_roundtrip_and_read_back() {
        // (a) DEFERRED list wrapper, built from the dep's rust type via `From<Vec<..>>`.
        let foos = IdxFooList::from(vec![
            index_dep_crate::IdxFoo::new(1),
            index_dep_crate::IdxFoo::new(2),
        ]);
        assert_eq!(foos.len(), 2);

        // (a) DEFERRED map wrapper.
        let mut foo_table = MapU64ToIdxFoo::new();
        foo_table.insert(
            7,
            &index_dep_crate_wasm::IdxFoo::from(index_dep_crate::IdxFoo::new(3)),
        );
        assert_eq!(foo_table.len(), 1);

        // (b) LOCALLY-minted all-extern map (its wrapper is not in the dep index); its keys-list IS
        // deferred, so its keys() is the R3d `.into()` path.
        let mut foo_to_foo = MapIdxFooToIdxFoo::new();
        foo_to_foo.insert(
            &index_dep_crate_wasm::IdxFoo::from(index_dep_crate::IdxFoo::new(8)),
            &index_dep_crate_wasm::IdxFoo::from(index_dep_crate::IdxFoo::new(9)),
        );
        let ftf_keys: IdxFooList = foo_to_foo.keys();
        assert_eq!(ftf_keys.len(), 1);

        // (c) MIXED map minted locally; its keys-list wrapper IS deferred.
        let mut foo_keyed = MapIdxFooToLocalThing::new();
        foo_keyed.insert(
            &index_dep_crate_wasm::IdxFoo::from(index_dep_crate::IdxFoo::new(4)),
            &LocalThing::new(10, "x".to_owned()),
        );
        // R3d: keys() returns the DEFERRED `IdxFooList`, constructed via `.into()`.
        let keyed_keys: IdxFooList = foo_keyed.keys();
        assert_eq!(keyed_keys.len(), 1);

        // Round-trip the whole record through CBOR and read the deferred list back off the getter.
        let everything = Everything::new(&foos, &foo_table, &foo_to_foo, &foo_keyed);
        let bytes = everything.to_cbor_bytes();
        let back = Everything::from_cbor_bytes(&bytes).expect("must deserialize its own bytes");
        assert_eq!(back.to_cbor_bytes(), bytes);
        assert_eq!(back.foos().len(), 2);
        assert_eq!(back.foo_keyed().keys().len(), 1);
    }
}
