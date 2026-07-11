// Behavioral floor for the extern-deps-across-the-wasm-boundary surface, executed INSIDE the
// generated WASM crate (native `cargo test`; the wasm-bindgen impls compile fine off-target). The
// harness appends this file into the crate-root `generated/mod.rs`, so the root collection wrappers
// and the mapped dep boundary types (`ExternCrateFoo`, `sub::module::ExternCrateBar`, the `nested`
// wasm wrappers) are all in scope via `use super::*`. Driving `Everything` and `nested::NestedHolder`
// entirely through the wasm API — construct through the wrappers, CBOR round-trip, then read every
// getter back and value-anchor it — proves the cross-crate boundary conversions (`.clone().into()` /
// the dep's `From`/`AsRef` impls, in both directions) are semantically CORRECT, not merely that the
// crate builds. Without this file the wasm stage only `cargo build`s, so a wrong boundary conversion
// (a swapped `get`/`add`, an identity `.into()` where a transform was needed) would compile green.
#[cfg(test)]
mod extern_deps_wasm_roundtrip {
    use super::*;

    // The dep boundary wrappers expose only From/Into/AsRef — no getters — so to read element field
    // values back we cross to the dep RUST types via `.as_ref()`: `extern_dep_crate::ExternCrateFoo`
    // has index_0()/index_1()/index_2(), `extern_dep_crate::sub::module::ExternCrateBar` has inner().
    // The in-crate `NestedItem` wasm wrapper exposes x() directly.
    fn foo(i0: u64, i1: &str, i2: Vec<u8>) -> ExternCrateFoo {
        ExternCrateFoo::from(extern_dep_crate::ExternCrateFoo::new(i0, i1.to_owned(), i2))
    }

    fn bar(inner: u64) -> ExternCrateBar {
        ExternCrateBar::from(extern_dep_crate::sub::module::ExternCrateBar::new(inner))
    }

    #[test]
    fn everything_roundtrips_through_extern_dep_collections() {
        // list built by mutation (add); table built by mutation (insert)
        let mut foos = ExternCrateFooList::new();
        foos.add(&foo(4, "a", vec![0x04]));
        foos.add(&foo(5, "b", vec![0x05]));
        assert_eq!(foos.len(), 2);

        let mut table = MapU64ToExternCrateFoo::new();
        assert!(table.insert(0, &foo(2, "v", vec![0x02])).is_none());
        assert!(table.insert(9, &foo(3, "w", vec![0x03])).is_none());
        assert_eq!(table.len(), 2);

        let everything = Everything::new(&foo(7, "single", vec![0xde, 0xad]), &foos, &table);
        let bytes = everything.to_cbor_bytes();
        let back = Everything::from_cbor_bytes(&bytes).expect("must deserialize its own bytes");
        assert_eq!(
            back.to_cbor_bytes(),
            bytes,
            "wire round-trip must be byte-identical"
        );

        // single (control cell: extern-dep type as a direct member)
        assert_eq!(back.single().as_ref().index_0(), 7);
        assert_eq!(back.single().as_ref().index_1(), "single");
        assert_eq!(back.single().as_ref().index_2(), vec![0xde, 0xad]);

        // list cell: len + per-element field values across the boundary
        assert_eq!(back.foos().len(), 2);
        assert_eq!(back.foos().get(0).as_ref().index_0(), 4);
        assert_eq!(back.foos().get(0).as_ref().index_1(), "a");
        assert_eq!(back.foos().get(0).as_ref().index_2(), vec![0x04]);
        assert_eq!(back.foos().get(1).as_ref().index_0(), 5);
        assert_eq!(back.foos().get(1).as_ref().index_1(), "b");
        assert_eq!(back.foos().get(1).as_ref().index_2(), vec![0x05]);

        // table cell: len, exact key set, looked-up value fields, and a miss
        assert_eq!(back.table().len(), 2);
        assert_eq!(back.table().keys(), vec![0u64, 9u64]);
        assert_eq!(back.table().get(0).unwrap().as_ref().index_1(), "v");
        assert_eq!(back.table().get(0).unwrap().as_ref().index_2(), vec![0x02]);
        assert_eq!(back.table().get(9).unwrap().as_ref().index_0(), 3);
        assert_eq!(back.table().get(9).unwrap().as_ref().index_2(), vec![0x03]);
        assert!(back.table().get(1).is_none());
    }

    #[test]
    fn nested_holder_roundtrips_in_crate_and_extern_collections() {
        // in-crate list via mutation (add)
        let mut in_crate_items = NestedItemList::new();
        in_crate_items.add(&NestedItem::new(5));
        in_crate_items.add(&NestedItem::new(6));

        // in-crate map keyed by NestedItem
        let mut in_crate_keyed = MapNestedItemToU64::new();
        assert!(in_crate_keyed.insert(&NestedItem::new(1), 10).is_none());
        // in-crate map valued by NestedItem
        let mut in_crate_valued = MapU64ToNestedItem::new();
        assert!(in_crate_valued.insert(20, &NestedItem::new(2)).is_none());

        // extern list via From<Vec<..>> (the other construction path the wrappers offer)
        let ext_items = ExternCrateBarList::from(vec![
            extern_dep_crate::sub::module::ExternCrateBar::new(7),
            extern_dep_crate::sub::module::ExternCrateBar::new(8),
        ]);
        // extern map keyed by ExternCrateBar (the extern-map-KEY cell)
        let mut ext_keyed = MapExternCrateBarToU64::new();
        assert!(ext_keyed.insert(&bar(3), 30).is_none());
        // extern map valued by ExternCrateBar
        let mut ext_valued = MapU64ToExternCrateBar::new();
        assert!(ext_valued.insert(40, &bar(4)).is_none());

        let holder = nested::NestedHolder::new(
            &in_crate_items,
            &in_crate_keyed,
            &in_crate_valued,
            &ext_items,
            &ext_keyed,
            &ext_valued,
        );
        let bytes = holder.to_cbor_bytes();
        let back =
            nested::NestedHolder::from_cbor_bytes(&bytes).expect("must deserialize its own bytes");
        assert_eq!(
            back.to_cbor_bytes(),
            bytes,
            "wire round-trip must be byte-identical"
        );

        // in-crate list elements (x() on the wasm wrapper)
        assert_eq!(back.in_crate_items().len(), 2);
        assert_eq!(back.in_crate_items().get(0).x(), 5);
        assert_eq!(back.in_crate_items().get(1).x(), 6);

        // in-crate map keyed by NestedItem: value lookup + keys() readback (keys() is a NestedItemList)
        assert_eq!(back.in_crate_keyed().get(&NestedItem::new(1)), Some(10));
        assert_eq!(back.in_crate_keyed().keys().len(), 1);
        assert_eq!(back.in_crate_keyed().keys().get(0).x(), 1);

        // in-crate map valued by NestedItem
        assert_eq!(back.in_crate_valued().keys(), vec![20u64]);
        assert_eq!(back.in_crate_valued().get(20).unwrap().x(), 2);

        // extern list elements (inner() via as_ref() to the dep rust type)
        assert_eq!(back.ext_items().len(), 2);
        assert_eq!(back.ext_items().get(0).as_ref().inner(), 7);
        assert_eq!(back.ext_items().get(1).as_ref().inner(), 8);

        // extern map keyed by ExternCrateBar: value lookup + keys() readback (keys() is an ExternCrateBarList)
        assert_eq!(back.ext_keyed().get(&bar(3)), Some(30));
        assert_eq!(back.ext_keyed().keys().len(), 1);
        assert_eq!(back.ext_keyed().keys().get(0).as_ref().inner(), 3);

        // extern map valued by ExternCrateBar
        assert_eq!(back.ext_valued().keys(), vec![40u64]);
        assert_eq!(back.ext_valued().get(40).unwrap().as_ref().inner(), 4);
    }
}
