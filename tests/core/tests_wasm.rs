// Wasm-boundary EXECUTION coverage. `wasm_matrix_compiles` proves the wasm-ABI shape axis
// (cddl-matrix/project_wasm_matrix.ts SHAPES) *type-checks*; these tests execute a representative
// sample of those shapes on the host target: construct through the wasm wrapper API, round-trip
// `to_cbor_bytes` -> `from_cbor_bytes`, and read every accessor back. That's the verdict a compile
// gate can't reach — an identity `.into()` where a transform was needed, or an accessor reading
// the wrong field, compiles green and only execution can see it. The rust-crate round-trips are
// `--emit-tests`' job; the unique value here is the BOUNDARY (accessor semantics + conversions).
//
// `from_cbor_bytes` returns `Result<_, JsError>` and `JsError: !Debug`, hence `.ok().expect(..)`.

// Smoke test for run_test's tests_wasm.rs hook: proves the file's contents actually land in the
// generated wasm crate and execute. A generated wasm crate ships no #[test]s of its own, so
// without this a broken hook would `cargo test` zero tests and pass vacuously (which it silently
// did before the append was wired up). It doubles as the `struct` (array-representation) shape:
// prim/text/bytes fields by value.
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

// shape: struct (map representation) + role struct-field-opt (set_-style optional fields) + a
// REQUIRED-nullable field (`1: uint / null` -> Option<u64>, where None unambiguously means "null
// on the wire" — the ambiguous optional-nullable flatten is a tracked #[ignore] gap in
// integration_tests.rs and deliberately not asserted here).
#[test]
fn wasm_map_struct_optional_and_nullable_fields() {
    let foo = Foo::new(9, String::from("f"), vec![1, 2]);
    let mut bar = Bar::new(&foo, Some(4), 1.5);
    bar.set_derp(77);
    bar.set_key_5(String::from("five"));
    let back = Bar::from_cbor_bytes(&bar.to_cbor_bytes())
        .ok()
        .expect("Bar round-trip");
    assert_eq!(back.foo().index_0(), 9);
    assert_eq!(back.foo().index_1(), "f");
    assert_eq!(back.foo().index_2(), vec![1, 2]);
    assert_eq!(back.derp(), Some(77));
    assert_eq!(back.one(), Some(4));
    assert_eq!(back.key_5(), Some(String::from("five")));
    assert_eq!(back.float(), 1.5);

    // optionals absent + nullable null: every getter must answer None, not garbage
    let bare = Bar::new(&foo, None, 0.5);
    let back = Bar::from_cbor_bytes(&bare.to_cbor_bytes())
        .ok()
        .expect("Bar round-trip (absent optionals)");
    assert_eq!(back.derp(), None);
    assert_eq!(back.one(), None);
    assert_eq!(back.key_5(), None);
    assert_eq!(back.float(), 0.5);
}

// shapes: coll (FooList, a Vec-backed wrapper struct) + collmap (MapTextToText) in the roles
// array get/add/len and map get/insert/keys/len. List/map wrappers expose no CBOR methods of
// their own, so the wire round-trip goes through the containing struct (TableArrMembers).
#[test]
fn wasm_list_and_map_wrappers() {
    let mut foos = FooList::new();
    assert_eq!(foos.len(), 0);
    foos.add(&Foo::new(1, String::from("one"), vec![]));
    foos.add(&Foo::new(2, String::from("two"), vec![0xFF]));
    assert_eq!(foos.len(), 2);

    let mut tab = MapTextToText::new();
    assert_eq!(tab.insert(String::from("k1"), String::from("v1")), None);
    assert_eq!(tab.insert(String::from("k2"), String::from("v2")), None);
    // inserting an existing key returns the displaced value across the boundary
    assert_eq!(
        tab.insert(String::from("k2"), String::from("v2b")),
        Some(String::from("v2"))
    );
    assert_eq!(tab.len(), 2);

    let wrapper = TableArrMembers::new(&tab, vec![10, 20], &foos);
    let back = TableArrMembers::from_cbor_bytes(&wrapper.to_cbor_bytes())
        .ok()
        .expect("TableArrMembers round-trip");
    assert_eq!(back.arr(), vec![10, 20]);
    let foos = back.arr2();
    assert_eq!(foos.len(), 2);
    assert_eq!(foos.get(0).index_0(), 1);
    assert_eq!(foos.get(0).index_1(), "one");
    assert_eq!(foos.get(0).index_2(), Vec::<u8>::new());
    assert_eq!(foos.get(1).index_0(), 2);
    assert_eq!(foos.get(1).index_1(), "two");
    assert_eq!(foos.get(1).index_2(), vec![0xFF]);
    let tab = back.tab();
    assert_eq!(tab.len(), 2);
    assert_eq!(tab.get(String::from("k1")), Some(String::from("v1")));
    assert_eq!(tab.get(String::from("k2")), Some(String::from("v2b")));
    assert_eq!(tab.get(String::from("nope")), None);
    let keys = tab.keys();
    assert_eq!(keys.len(), 2);
    assert_eq!(keys.get(0), "k1");
    assert_eq!(keys.get(1), "k2");
}

// shape: cenum — a Copy c-style enum crosses the boundary by value as the re-exported
// (`pub use`) rust enum itself, not a wrapper.
#[test]
fn wasm_c_style_enum_boundary() {
    let enums = Enums::new(CEnum::I4, &TypeChoice::new_text(String::from("hi")));
    let back = Enums::from_cbor_bytes(&enums.to_cbor_bytes())
        .ok()
        .expect("Enums round-trip");
    assert_eq!(back.c_enum(), CEnum::I4);
    assert!(matches!(back.type_choice().kind(), TypeChoiceKind::Text));
    assert_eq!(back.type_choice().as_text(), Some(String::from("hi")));
}

// shape: denum — a data-carrying type-choice enum crosses as a wrapper with per-variant ctors,
// a kind() discriminant, and as_*() getters that must answer None for the wrong variant.
#[test]
fn wasm_data_enum_ctors_kind_and_getters() {
    let back = TypeChoice::from_cbor_bytes(&TypeChoice::new_uint(7).to_cbor_bytes())
        .ok()
        .expect("TypeChoice::U64 round-trip");
    assert!(matches!(back.kind(), TypeChoiceKind::U64));
    assert_eq!(back.as_uint(), Some(7));
    assert_eq!(back.as_text(), None);
    assert_eq!(back.as_bytes(), None);
    assert_eq!(back.as_arr_u64(), None);

    let back = TypeChoice::from_cbor_bytes(&TypeChoice::new_bytes(vec![1, 2, 3]).to_cbor_bytes())
        .ok()
        .expect("TypeChoice::Bytes round-trip");
    assert!(matches!(back.kind(), TypeChoiceKind::Bytes));
    assert_eq!(back.as_bytes(), Some(vec![1, 2, 3]));
    assert_eq!(back.as_uint(), None);

    let back = TypeChoice::from_cbor_bytes(&TypeChoice::new_arr_u64(vec![5, 6]).to_cbor_bytes())
        .ok()
        .expect("TypeChoice::ArrU64 round-trip");
    assert!(matches!(back.kind(), TypeChoiceKind::ArrU64));
    assert_eq!(back.as_arr_u64(), Some(vec![5, 6]));

    // fixed-value variants carry no payload: kind() is the only observable, and the payload
    // getters of other variants must stay None
    let back = TypeChoice::from_cbor_bytes(&TypeChoice::new_i0().to_cbor_bytes())
        .ok()
        .expect("TypeChoice::I0 round-trip");
    assert!(matches!(back.kind(), TypeChoiceKind::I0));
    assert_eq!(back.as_uint(), None);
    let back = TypeChoice::from_cbor_bytes(&TypeChoice::new_helloworld().to_cbor_bytes())
        .ok()
        .expect("TypeChoice::Helloworld round-trip");
    assert!(matches!(back.kind(), TypeChoiceKind::Helloworld));
    assert_eq!(back.as_text(), None);
}

// denum sibling: a group-choice enum, including a variant embedding a plain group (Plain).
#[test]
fn wasm_group_choice_enum_boundary() {
    let back =
        GroupChoice::from_cbor_bytes(&GroupChoice::new_foo(3, String::from("g"), vec![7]).to_cbor_bytes())
            .ok()
            .expect("GroupChoice::Foo round-trip");
    assert!(matches!(back.kind(), GroupChoiceKind::Foo));
    let foo = back.as_foo().expect("as_foo on the Foo variant");
    assert_eq!(foo.index_0(), 3);
    assert_eq!(foo.index_1(), "g");
    assert_eq!(foo.index_2(), vec![7]);
    assert!(back.as_group_choice1().is_none());
    assert!(back.as_plain().is_none());

    let back = GroupChoice::from_cbor_bytes(&GroupChoice::new_group_choice1(11).to_cbor_bytes())
        .ok()
        .expect("GroupChoice::GroupChoice1 round-trip");
    assert!(matches!(back.kind(), GroupChoiceKind::GroupChoice1));
    assert_eq!(back.as_group_choice1(), Some(11));

    // `e` (tagged_text = #6.42(text)) auto-wraps into the TaggedText tag wrapper, so it crosses the
    // boundary as an opaque wrapper (from_cbor_bytes/to_cbor_bytes only). Build it from the native
    // type via `.into()` and assert its wire form (tag 42 + text) survives losslessly.
    let e: TaggedText = cddl_lib::TaggedText::from(String::from("tagged")).into();
    let expected_e = e.to_cbor_bytes();
    let back = GroupChoice::from_cbor_bytes(&GroupChoice::new_plain(2, &e).to_cbor_bytes())
        .ok()
        .expect("GroupChoice::Plain round-trip");
    assert!(matches!(back.kind(), GroupChoiceKind::Plain));
    let plain = back.as_plain().expect("as_plain on the Plain variant");
    assert_eq!(plain.d(), 2);
    assert_eq!(plain.e().to_cbor_bytes(), expected_e);
}

// shape: nullable (`T / null` -> Option<T>) in a REQUIRED field — both states survive the wire
// unambiguously. opt_text is also a talias boundary: tagged_text auto-wraps into the opaque
// TaggedText tag wrapper, so the field crosses as `Option<TaggedText>`.
#[test]
fn wasm_nullable_field_both_states() {
    let x: TaggedText = cddl_lib::TaggedText::from(String::from("x")).into();
    let expected_x = x.to_cbor_bytes();
    let back = Foo2::from_cbor_bytes(&Foo2::new(1, Some(x)).to_cbor_bytes())
        .ok()
        .expect("Foo2 round-trip (present)");
    assert_eq!(back.index_0(), 1);
    assert_eq!(back.opt_text().map(|t| t.to_cbor_bytes()), Some(expected_x));

    let back = Foo2::from_cbor_bytes(&Foo2::new(2, None).to_cbor_bytes())
        .ok()
        .expect("Foo2 round-trip (null)");
    assert_eq!(back.index_0(), 2);
    assert!(back.opt_text().is_none());
}

// shapes: cborwrap (`bytes .cbor foo`) — `foo_bytes` (FooBytes = Foo) stays a transparent passthru
// alias, so it crosses as the inner Foo wrapper; `tagged_foo_bytes` (#6.20(bytes .cbor foo)) now
// auto-wraps into the opaque TaggedFooBytes tag wrapper. Both nested-CBOR re-encodings must be
// lossless.
#[test]
fn wasm_cbor_in_cbor_boundary() {
    let inner = Foo::new(5, String::from("in"), vec![9]);
    let tagged: TaggedFooBytes =
        cddl_lib::TaggedFooBytes::from(cddl_lib::Foo::new(6, String::from("tag"), vec![])).into();
    let expected_tagged = tagged.to_cbor_bytes();
    let back = CborInCbor::from_cbor_bytes(&CborInCbor::new(&inner, 42, &tagged).to_cbor_bytes())
        .ok()
        .expect("CborInCbor round-trip");
    assert_eq!(back.foo_bytes().index_0(), 5);
    assert_eq!(back.foo_bytes().index_1(), "in");
    assert_eq!(back.foo_bytes().index_2(), vec![9]);
    assert_eq!(back.uint_bytes(), 42);
    assert_eq!(back.tagged_foo_bytes().to_cbor_bytes(), expected_tagged);
}

// shape: newtype-inner (@newtype). The wasm wrapper exposes NO constructor (a boundary gap: only
// from_cbor_bytes can build one from JS), so construct from the raw inner encoding instead:
// uint 42 = 0x18 0x2a per RFC 8949.
#[test]
fn wasm_newtype_custom_getter() {
    let w = WrapperInt::from_cbor_bytes(&[0x18, 0x2a])
        .ok()
        .expect("WrapperInt::from_cbor_bytes");
    assert_eq!(w.custom_getter(), 42);
    assert_eq!(w.to_cbor_bytes(), vec![0x18, 0x2a]);
}

// shapes: prim + palias — aliased primitives (uint .size N / ranges) cross by value with the
// alias resolved to the rust primitive in the accessor signatures; extreme values survive.
#[test]
fn wasm_primitive_alias_boundaries() {
    let s = SignedInts::new(
        u8::MAX,
        u16::MAX,
        u32::MAX,
        u64::MAX,
        i8::MIN,
        i16::MIN,
        i32::MIN,
        i64::MIN,
        999,
    );
    let back = SignedInts::from_cbor_bytes(&s.to_cbor_bytes())
        .ok()
        .expect("SignedInts round-trip");
    assert_eq!(back.u_8(), u8::MAX);
    assert_eq!(back.u_16(), u16::MAX);
    assert_eq!(back.u_32(), u32::MAX);
    assert_eq!(back.u_64(), u64::MAX);
    assert_eq!(back.i_8(), i8::MIN);
    assert_eq!(back.i_16(), i16::MIN);
    assert_eq!(back.i_32(), i32::MIN);
    assert_eq!(back.i_64(), i64::MIN);
    assert_eq!(back.n_64(), 999);
}

// shape: extern (_CDDL_CODEGEN_EXTERN_TYPE_ with the user-supplied wasm wrapper from
// tests/external_wasm_defs) in required + optional struct fields.
#[test]
fn wasm_extern_type_boundary() {
    let ext = ExternalFoo::new(1, String::from("ext"), vec![2]);
    let mut externs = Externs::new(&ext);
    let back = Externs::from_cbor_bytes(&externs.to_cbor_bytes())
        .ok()
        .expect("Externs round-trip (opt absent)");
    assert_eq!(back.req().index_0(), 1);
    assert_eq!(back.req().index_1(), "ext");
    assert_eq!(back.req().index_2(), vec![2]);
    assert!(back.opt().is_none());

    externs.set_opt(&ExternalFoo::new(3, String::from("opt"), vec![]));
    let back = Externs::from_cbor_bytes(&externs.to_cbor_bytes())
        .ok()
        .expect("Externs round-trip (opt present)");
    let opt = back.opt().expect("opt was set");
    assert_eq!(opt.index_0(), 3);
    assert_eq!(opt.index_1(), "opt");
}

// shape: generic — a monomorphized generic instance (extern_generic<external_foo>) crossing as
// its wasm-side alias (ExternGenericExternalFoo, from tests/external_wasm_defs).
#[test]
fn wasm_generic_instance_boundary() {
    let g = UsingExternGeneric::new(&ExternalFoo::new(8, String::from("g"), vec![1]));
    let back = UsingExternGeneric::from_cbor_bytes(&g.to_cbor_bytes())
        .ok()
        .expect("UsingExternGeneric round-trip");
    assert_eq!(back.foo().index_0(), 8);
    assert_eq!(back.foo().index_1(), "g");
    assert_eq!(back.foo().index_2(), vec![1]);
}
