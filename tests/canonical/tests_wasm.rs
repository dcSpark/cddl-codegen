// Wasm-boundary EXECUTION coverage for the preserve-encodings/canonical wasm crate (see
// tests/core/tests_wasm.rs for the rationale). Under `--preserve-encodings` the map wrappers wrap
// `OrderedHashMap` instead of `BTreeMap` — a distinct boundary emission path only this fixture
// executes — and every accessor read here crosses a wrapper carrying encoding state.
//
// Canonical BYTE assertions stay in tests.rs (the byte-vector suite); this file's verdict is the
// boundary: construct via the wasm API, round-trip, read every accessor back.

// shapes: collmap (OrderedHashMap-backed MapU64ToText) + coll (FooList) in the roles map
// get/insert/keys/len and array get/add/len, round-tripped through the containing struct.
#[test]
fn wasm_preserve_map_and_list_wrappers() {
    let mut table = MapU64ToText::new();
    assert_eq!(table.insert(1, String::from("one")), None);
    assert_eq!(table.insert(2, String::from("two")), None);
    // inserting an existing key returns the displaced value across the boundary
    assert_eq!(table.insert(2, String::from("two2")), Some(String::from("two")));
    assert_eq!(table.len(), 2);

    let mut foos = FooList::new();
    assert_eq!(foos.len(), 0);
    foos.add(&Foo::new(1, String::from("a"), vec![0xAB]));
    assert_eq!(foos.len(), 1);

    let wrapper = TableArrMembers::new(vec![100, 200], &foos, &table);
    let back = TableArrMembers::from_cbor_bytes(&wrapper.to_cbor_bytes())
        .ok()
        .expect("TableArrMembers round-trip");
    assert_eq!(back.arr(), vec![100, 200]);
    let foos = back.arr2();
    assert_eq!(foos.len(), 1);
    assert_eq!(foos.get(0).index_0(), 1);
    assert_eq!(foos.get(0).index_1(), "a");
    assert_eq!(foos.get(0).index_2(), vec![0xAB]);
    let table = back.table();
    assert_eq!(table.len(), 2);
    assert_eq!(table.get(1), Some(String::from("one")));
    assert_eq!(table.get(2), Some(String::from("two2")));
    assert_eq!(table.get(3), None);
    assert_eq!(table.keys(), vec![1, 2]);
}

// shape: struct (map representation) under preserve-encodings: optional set_-style fields and a
// REQUIRED-nullable field (`1: uint / null` -> Option<u64>, unambiguous), all accessors read back
// through the encoding-carrying wrapper.
#[test]
fn wasm_preserve_map_struct_fields() {
    let foo = Foo::new(3, String::from("c"), vec![5]);
    let mut bar = Bar::new(&foo, Some(9));
    bar.set_derp(7);
    bar.set_key_5(String::from("five"));
    let back = Bar::from_cbor_bytes(&bar.to_cbor_bytes())
        .ok()
        .expect("Bar round-trip");
    assert_eq!(back.foo().index_0(), 3);
    assert_eq!(back.foo().index_1(), "c");
    assert_eq!(back.foo().index_2(), vec![5]);
    assert_eq!(back.derp(), Some(7));
    assert_eq!(back.key_1(), Some(9));
    assert_eq!(back.key_5(), Some(String::from("five")));

    let bare = Bar::new(&foo, None);
    let back = Bar::from_cbor_bytes(&bare.to_cbor_bytes())
        .ok()
        .expect("Bar round-trip (absent optionals)");
    assert_eq!(back.derp(), None);
    assert_eq!(back.key_1(), None);
    assert_eq!(back.key_5(), None);
}
