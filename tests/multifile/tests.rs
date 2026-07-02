// Round-trips exercising cross-module type resolution: types living in nested scope modules
// (a::c::foo, a, b::bar, qux) referencing each other and being composed from the crate root
// (`everything`, plus the extern type).
//
// This same file is compiled under BOTH `multifile` (default flags) and `multifile_json_preserve`
// (preserve-encodings + json), so it must stay profile-agnostic: no `encodings` access, and no
// naming the generated map type (`BTreeMap` under default flags, `OrderedHashMap` under
// preserve-encodings) — empty map prototypes are minted by decoding hand-written CBOR instead.
// Map-struct field order on the wire also differs between the profiles, so byte-level hex pins
// are limited to array structs (fixed field order, identical default encodings in both).
#[cfg(test)]
mod tests {
    use super::*;
    use crate::a::c::foo::Foo;
    use crate::a::Baz;
    use crate::b::bar::{Bar, BarTypedef};
    use crate::qux::Qux;
    use cbor_event::de::Deserializer;
    use serialization::Deserialize;

    fn deser_test<T: Deserialize + ToCBORBytes>(orig: &T) {
        let orig_bytes = orig.to_cbor_bytes();
        print_cbor_types("orig", &orig_bytes);
        let mut deserializer = Deserializer::from(std::io::Cursor::new(orig_bytes.clone()));
        let deser = T::deserialize(&mut deserializer).unwrap();
        print_cbor_types("deser", &deser.to_cbor_bytes());
        assert_eq!(orig_bytes, deser.to_cbor_bytes());
        assert_eq!(deserializer.as_ref().position(), orig_bytes.len() as u64);
    }

    fn make_foo(index_0: u64) -> Foo {
        Foo::new(index_0, String::from("ab"), vec![1, 2])
    }

    fn make_external_foo() -> ExternalFoo {
        ExternalFoo::new(0, String::from("external"), vec![8])
    }

    // Minimal bar decoded from hand-written CBOR: {1: null, "five": 5, "foo": 1337([]),
    // "foo_map": {}}. Also the only profile-agnostic way to mint an empty foo_map.
    fn empty_bar() -> Bar {
        let bytes = [
            map_def(4),
            cbor_int(1, cbor_event::Sz::Inline),
            vec![NULL],
            cbor_string("five"),
            cbor_int(5, cbor_event::Sz::Inline),
            cbor_string("foo"),
            cbor_tag_sz(1337, cbor_event::Sz::Two),
            arr_def(0),
            cbor_string("foo_map"),
            map_def(0),
        ]
        .concat();
        Bar::from_cbor_bytes(&bytes).unwrap()
    }

    // Minimal everything decoded from hand-written outer CBOR structure: [foo, external_foo,
    // [], {}, baz, qux]. Also the only profile-agnostic way to mint an empty table.
    fn empty_everything() -> Everything {
        let bytes = [
            arr_def(6),
            make_foo(9).to_cbor_bytes(),
            make_external_foo().to_cbor_bytes(),
            arr_def(0),
            map_def(0),
            Baz::new(7).to_cbor_bytes(),
            Qux::new(String::from("hi")).to_cbor_bytes(),
        ]
        .concat();
        Everything::from_cbor_bytes(&bytes).unwrap()
    }

    fn make_bar() -> Bar {
        // Bar (scope b) references Foo (scope a::c::foo) directly, in an array and as map key+value.
        let mut bar = Bar::new(vec![make_foo(9)], Some(1), empty_bar().foo_map);
        bar.foo_map.insert(make_foo(9), make_foo(10));
        bar
    }

    // Types defined in nested scope modules encode exactly like their single-file equivalents:
    // hand-written RFC 8949 hex, NOT copied from generator output.
    #[test]
    fn nested_module_types_bytes() {
        // foo = [uint, text, bytes] (a/c/foo.cddl): [9, "ab", h'0102']
        assert_eq!(
            make_foo(9).to_cbor_bytes(),
            vec![0x83, 0x09, 0x62, 0x61, 0x62, 0x42, 0x01, 0x02],
        );
        // baz = [0, uint] (a/mod.cddl): the fixed literal 0 is emitted on the wire
        assert_eq!(Baz::new(7).to_cbor_bytes(), vec![0x82, 0x00, 0x07]);
        // qux = [2, text] (qux.cddl): fixed literal 2 + "hi"
        assert_eq!(
            Qux::new(String::from("hi")).to_cbor_bytes(),
            vec![0x82, 0x02, 0x62, 0x68, 0x69],
        );
        deser_test(&make_foo(9));
        deser_test(&Baz::new(7));
        deser_test(&Qux::new(String::from("hi")));
    }

    // The hand-written CBOR prototypes decode into the expected cross-scope values (an acceptance
    // oracle whose input bytes can't have come from the generator).
    #[test]
    fn hand_written_cbor_accepted() {
        let bar = empty_bar();
        assert!(bar.foo.is_empty());
        assert_eq!(bar.key_1, None); // wire null -> None
        assert!(bar.foo_map.is_empty());
        assert_eq!(bar.derp, None);
        assert_eq!(bar.key_5, None);
        assert!(bar.baz.is_none());
        deser_test(&bar);
        let everything = empty_everything();
        assert_eq!(everything.foo.index_0, 9);
        assert_eq!(everything.external_foo.index_1, "external");
        assert!(everything.bars.is_empty());
        assert!(everything.table.is_empty());
        assert_eq!(everything.baz.index_1, 7);
        assert_eq!(everything.qux.index_1, "hi");
        deser_test(&everything);
    }

    // bar (b/bar.cddl) is a map struct in scope b whose fields resolve to types from two OTHER
    // scopes: Foo (a::c::foo, tagged #6.1337 in an array, and as foo_map key/value) and Baz (a).
    #[test]
    fn cross_module_bar() {
        // minimal: optionals absent, key 1 = null
        let mut bar = Bar::new(vec![make_foo(9)], None, empty_bar().foo_map);
        deser_test(&bar);
        // full: every optional present, incl. baz from scope a
        bar.key_1 = Some(1);
        bar.derp = Some(1000);
        bar.key_5 = Some(String::from("text"));
        bar.baz = Some(Baz::new(7));
        bar.foo_map.insert(make_foo(9), make_foo(10));
        deser_test(&bar);
        // decoded VALUES land in the right cross-scope types/fields (not just byte equality)
        let decoded = Bar::from_cbor_bytes(&bar.to_cbor_bytes()).unwrap();
        assert_eq!(decoded.foo[0].index_0, 9);
        assert_eq!(decoded.baz.unwrap().index_1, 7);
        assert_eq!(decoded.foo_map.get(&make_foo(9)).unwrap().index_0, 10);
    }

    // everything (lib.cddl) composes every scope from the crate root: a single-level nested mod
    // (foo), a multi-level nested mod (baz), a root-dir mod (qux), scope-b types in an array
    // (bars) and as map values through a typedef (table), plus the extern type.
    #[test]
    fn cross_module_everything() {
        // BarTypedef = bar (a cross-file typedef) must resolve to the same rust type as Bar
        let bar_typedef: BarTypedef = make_bar();
        let mut everything = Everything::new(
            make_foo(9),
            make_external_foo(),
            vec![make_bar(), make_bar()],
            empty_everything().table,
            Baz::new(7),
            Qux::new(String::from("hi")),
        );
        everything.table.insert(4, bar_typedef);
        everything.table.insert(16, make_bar());
        deser_test(&everything);
        let decoded = Everything::from_cbor_bytes(&everything.to_cbor_bytes()).unwrap();
        assert_eq!(decoded.foo.index_0, 9);
        assert_eq!(decoded.external_foo.index_1, "external");
        assert_eq!(decoded.bars.len(), 2);
        assert_eq!(decoded.bars[1].foo[0].index_2, vec![1, 2]);
        assert_eq!(decoded.table.len(), 2);
        assert_eq!(
            decoded.table.get(&4).unwrap().to_cbor_bytes(),
            make_bar().to_cbor_bytes(),
        );
        assert_eq!(decoded.baz.index_1, 7);
        assert_eq!(decoded.qux.index_1, "hi");
    }
}
