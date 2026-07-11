// Behavioral floor for the extern-deps surface (`--common-import-override` +
// `_CDDL_CODEGEN_EXTERN_DEPS_DIR_`) — the one surface only this fixture exercises. Compilation
// alone left the whole feature behaviorally unverified: a codegen regression in how
// extern-dep-typed fields serialize (wrong tag, dropped field, encoding not threaded across the
// crate boundary) compiled green and shipped (mutation-proven: a dep crate whose serializer and
// deserializer disagreed on the tag kept the gate green before these tests existed).
#[cfg(test)]
mod tests {
    use super::*;
    // under --common-import-override the serialization traits live in the dep crate
    use extern_dep_crate::serialization::{Deserialize, ToCBORBytes};

    fn mk_everything(extern_foo: ExternCrateFoo) -> Everything {
        let mut foo_map = OrderedHashMap::new();
        foo_map.insert(
            Foo::new(1, "k".to_owned(), vec![]),
            Foo::new(2, "v".to_owned(), vec![]),
        );
        let mut table = OrderedHashMap::new();
        table.insert(
            0u64,
            Bar::new(
                vec![Foo::new(3, "t".to_owned(), vec![])],
                None,
                OrderedHashMap::new(),
            ),
        );
        Everything::new(
            Foo::new(7, "f".to_owned(), vec![0xde, 0xad]),
            extern_foo,
            // extern_foos: [* extern_crate_foo] — the list-of-extern-dep-type field
            vec![
                ExternCrateFoo::new(10, "l0".to_owned(), vec![0xa0]),
                ExternCrateFoo::new(11, "l1".to_owned(), vec![0xa1]),
            ],
            vec![Bar::new(
                vec![Foo::new(4, "b".to_owned(), vec![5])],
                Some(4),
                foo_map,
            )],
            table,
            Baz::new(6),
            Qux::new("q".to_owned()),
        )
    }

    #[test]
    fn everything_roundtrips_through_extern_dep_types() {
        let e = mk_everything(ExternCrateFoo::new(3, "ext".to_owned(), vec![9]));
        let bytes = e.to_cbor_bytes();
        let back = Everything::from_cbor_bytes(&bytes).expect("must deserialize its own bytes");
        assert_eq!(
            back.to_cbor_bytes(),
            bytes,
            "wire round-trip must be byte-identical"
        );
        // value anchors through the extern-dep field so a compensating encode+decode bug
        // (both sides wrong the same way about WHICH data goes where) can't pass on identity
        assert_eq!(back.extern_crate_foo.index_0(), 3);
        assert_eq!(back.extern_crate_foo.index_1(), "ext");
        assert_eq!(back.extern_crate_foo.index_2(), vec![9]);
        // the list-of-extern-dep-type field round-trips element-for-element
        assert_eq!(back.extern_foos.len(), 2);
        assert_eq!(back.extern_foos[1].index_0(), 11);
        assert_eq!(back.extern_foos[0].index_2(), vec![0xa0]);
        // the embedded field is exactly the dep crate's own encoding (delegation, not a re-impl)
        let extern_bytes =
            extern_dep_crate::serialization::ToCBORBytes::to_cbor_bytes(&back.extern_crate_foo);
        assert!(
            bytes.windows(extern_bytes.len()).any(|w| w == extern_bytes),
            "extern-dep field bytes not embedded verbatim in the everything encoding"
        );
    }

    #[test]
    fn extern_dep_encodings_thread_across_the_crate_boundary() {
        // an ExternCrateFoo decoded from IRREGULAR bytes — tag 11 with a non-minimal 1-byte
        // argument (0xd8 0x0b), indefinite array, non-minimal index_0 (0x18 0x01) — must
        // re-encode byte-identically when embedded in a generated struct: the dep crate's
        // stored encodings have to survive the crate boundary
        let irregular: Vec<u8> = vec![
            0xd8, 0x0b, // tag(11), 1-byte argument (minimal would be 0xcb)
            0x9f, // array(indefinite)
            0x18, 0x01, // 1 as uint8 (minimal would be 0x01)
            0x61, 0x61, // "a"
            0x41, 0x02, // bytes [2]
            0xff, // break
        ];
        let extern_foo: ExternCrateFoo =
            extern_dep_crate::serialization::Deserialize::from_cbor_bytes(&irregular)
                .expect("dep crate must accept its own irregular encoding");
        let bytes = mk_everything(extern_foo).to_cbor_bytes();
        assert!(
            bytes.windows(irregular.len()).any(|w| w == irregular),
            "irregular extern-dep encoding not preserved across the crate boundary"
        );
        let back = Everything::from_cbor_bytes(&bytes).unwrap();
        assert_eq!(
            back.to_cbor_bytes(),
            bytes,
            "round-trip must stay byte-identical"
        );
    }
}
