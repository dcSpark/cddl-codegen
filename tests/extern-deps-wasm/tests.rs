// Behavioral floor for the extern-deps-across-the-wasm-boundary surface. The wasm crate's mere
// compilation (the harness `cargo build`s it) is the acceptance test for `--extern-wasm-crate`;
// these rust-side tests additionally prove the cross-crate extern type serializes correctly when it
// appears as a LIST element and a TABLE value — the exact cells the wasm wrappers wrap and the cells
// the pre-fix `cddl_lib::…` inner-storage path made non-compiling.
#[cfg(test)]
mod tests {
    use super::*;
    // NestedHolder/NestedItem live in the non-root `nested` module (the wrapper-element-import pin);
    // reference them by path since the appended tests sit in the crate-root generated scope.
    use super::nested::{NestedHolder, NestedItem};
    use extern_dep_crate::sub::module::ExternCrateBar;
    // under --common-import-override the serialization traits live in the dep crate
    use extern_dep_crate::serialization::{Deserialize, ToCBORBytes};

    fn mk_everything() -> Everything {
        let mut table = OrderedHashMap::new();
        table.insert(0u64, ExternCrateFoo::new(2, "v".to_owned(), vec![0x02]));
        table.insert(9u64, ExternCrateFoo::new(3, "w".to_owned(), vec![0x03]));
        Everything::new(
            ExternCrateFoo::new(7, "single".to_owned(), vec![0xde, 0xad]),
            vec![
                ExternCrateFoo::new(4, "a".to_owned(), vec![0x04]),
                ExternCrateFoo::new(5, "b".to_owned(), vec![0x05]),
            ],
            table,
        )
    }

    #[test]
    fn everything_roundtrips_through_extern_dep_collections() {
        let e = mk_everything();
        let bytes = e.to_cbor_bytes();
        let back = Everything::from_cbor_bytes(&bytes).expect("must deserialize its own bytes");
        assert_eq!(
            back.to_cbor_bytes(),
            bytes,
            "wire round-trip must be byte-identical"
        );
        // value anchors through the extern-dep fields so a compensating encode+decode bug can't
        // pass on identity alone
        assert_eq!(back.single.index_0(), 7);
        assert_eq!(back.foos.len(), 2);
        assert_eq!(back.foos[1].index_1(), "b");
        assert_eq!(back.table.get(&9u64).unwrap().index_2(), vec![0x03]);
    }

    #[test]
    fn extern_dep_list_and_map_elements_are_embedded_verbatim() {
        // each extern-dep element must be the dep crate's own encoding (delegation, not a re-impl),
        // embedded byte-for-byte in the outer encoding — for both the list and the table cell
        let e = mk_everything();
        let bytes = e.to_cbor_bytes();
        let list_elem = extern_dep_crate::serialization::ToCBORBytes::to_cbor_bytes(&e.foos[0]);
        assert!(
            bytes.windows(list_elem.len()).any(|w| w == list_elem),
            "extern-dep list element bytes not embedded verbatim"
        );
        let map_val =
            extern_dep_crate::serialization::ToCBORBytes::to_cbor_bytes(e.table.get(&0u64).unwrap());
        assert!(
            bytes.windows(map_val.len()).any(|w| w == map_val),
            "extern-dep table value bytes not embedded verbatim"
        );
    }

    fn mk_nested_holder() -> NestedHolder {
        let mut in_crate_keyed = OrderedHashMap::new();
        in_crate_keyed.insert(NestedItem::new(1), 10u64);
        let mut in_crate_valued = OrderedHashMap::new();
        in_crate_valued.insert(20u64, NestedItem::new(2));
        let mut ext_keyed = OrderedHashMap::new();
        ext_keyed.insert(ExternCrateBar::new(3), 30u64);
        let mut ext_valued = OrderedHashMap::new();
        ext_valued.insert(40u64, ExternCrateBar::new(4));
        NestedHolder::new(
            vec![NestedItem::new(5), NestedItem::new(6)],
            in_crate_keyed,
            in_crate_valued,
            vec![ExternCrateBar::new(7), ExternCrateBar::new(8)],
            ext_keyed,
            ext_valued,
        )
    }

    // The `nested` module's cells exercise in-crate AND extern element types used ONLY as wrapper
    // elements / map keys / map values from a non-root scope — the shapes whose root wasm-module
    // imports the wrapper-element-ref fix registers. Round-tripping them proves the fix is
    // behavioral, not just a compile-only import addition.
    #[test]
    fn nested_holder_roundtrips_in_crate_and_extern_collections() {
        let h = mk_nested_holder();
        let bytes = h.to_cbor_bytes();
        let back = NestedHolder::from_cbor_bytes(&bytes).expect("must deserialize its own bytes");
        assert_eq!(
            back.to_cbor_bytes(),
            bytes,
            "wire round-trip must be byte-identical"
        );
        // value anchors across every cell so a compensating encode+decode bug can't pass on identity
        assert_eq!(back.in_crate_items[1].x, 6);
        assert_eq!(*back.in_crate_keyed.get(&NestedItem::new(1)).unwrap(), 10);
        assert_eq!(back.in_crate_valued.get(&20u64).unwrap().x, 2);
        assert_eq!(back.ext_items[0].inner(), 7);
        assert_eq!(*back.ext_keyed.get(&ExternCrateBar::new(3)).unwrap(), 30);
        assert_eq!(back.ext_valued.get(&40u64).unwrap().inner(), 4);
    }
}
