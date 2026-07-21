// Behavioral floor for a consumer referencing DEP-OWNED named collection rules by name across the
// wasm boundary. The rust crate's `cargo test` (this file) and the wasm crate's `cargo build` (the
// harness builds it) are the cross-crate COMPILE acceptance — the upgrade over the pure
// generation-output assertion in `dep_owned_named_collection_no_local_structural_import`: the dangling
// local structural import that assertion pins would be an E0432 the compile now also catches. The
// round-trip additionally proves the dep-owned `DepWithdrawals`/`DepCerts` (transparent `BTreeMap`/`Vec`
// aliases in the dep crate) serialize correctly as `ConsumerHolder` fields.
#[cfg(test)]
mod tests {
    use super::consumer::ConsumerHolder;
    use extern_dep_crate::serialization::{Deserialize, ToCBORBytes};
    use extern_dep_crate::ExternCrateFoo;

    #[test]
    fn consumer_holder_roundtrips_dep_owned_named_collections() {
        let mut wd = std::collections::BTreeMap::new();
        wd.insert(0u64, ExternCrateFoo::new(1, "a".to_owned(), vec![0x01]));
        wd.insert(9u64, ExternCrateFoo::new(2, "b".to_owned(), vec![0x02]));
        let cs = vec![
            ExternCrateFoo::new(3, "c".to_owned(), vec![0x03]),
            ExternCrateFoo::new(4, "d".to_owned(), vec![0x04]),
        ];
        let holder = ConsumerHolder::new(wd, cs);
        let bytes = holder.to_cbor_bytes();
        let back = ConsumerHolder::from_cbor_bytes(&bytes).expect("must deserialize its own bytes");
        assert_eq!(
            back.to_cbor_bytes(),
            bytes,
            "wire round-trip must be byte-identical"
        );
        // value anchors through the dep-owned collection fields so a compensating bug can't pass on
        // identity alone
        assert_eq!(back.wd.len(), 2);
        assert_eq!(back.wd.get(&9u64).unwrap().index_1(), "b");
        assert_eq!(back.cs.len(), 2);
        assert_eq!(back.cs[1].index_0(), 4);
    }
}
