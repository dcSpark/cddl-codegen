//! The behavioral claims about the component boundary, one `#[test]` per assertion class so a
//! failure names the class it falsifies. Every one of them is a fact about a RUNNING component that
//! no gate reading emitted bytes can reach.
//!
//! The oracle throughout is the generated rust crate itself (`cddl_lib`), linked here as a path dep:
//! "the boundary agrees with the library it wraps" is checkable, where "the boundary produced some
//! bytes" is not.
//!
//! Two shapes recur and are worth naming once:
//!
//! * **A fallible door returns `Err`, it never traps.** The outer `wasmtime::Result` is the trap
//!   channel and the inner `Result<_, String>` is the door; `?` on the outer one is therefore part
//!   of the assertion. And because a trap POISONS the instance (every later call fails with "cannot
//!   enter component instance"), the real claim is that the instance is STILL USABLE afterwards —
//!   which is what each rejection here goes on to check. The trap TEXT is deliberately never pinned.
//! * **Handles crossing the boundary are snapshots.** A getter mints a fresh `own` over a clone, and
//!   a parameter is materialized into an owned value before any `borrow_mut`, so aliasing is
//!   impossible by construction rather than merely unlikely.

use cddl_lib::serialization::{Deserialize as _, ToCBORBytes as _};
use component_host::{api, load};
use wasmtime::component::ResourceAny;
use wasmtime::{Result, Store};

// --- the fixture value, spelled once in both worlds -----------------------------------------------

const ID: u64 = 42;
const NAME: &str = "alpha";
/// `int::nint(4)` is -5: CBOR's nint encoding stores |x + 1|, and the WIT variant carries the wire
/// spelling rather than a signed integer.
const DELTA_NINT: u64 = 4;

fn digest_bytes() -> Vec<u8> {
    (0u8..32).collect()
}

fn tags() -> Vec<String> {
    vec!["t1".to_owned(), "t2".to_owned()]
}

fn props() -> Vec<(String, u64)> {
    vec![("x".to_owned(), 1), ("y".to_owned(), 2)]
}

/// A map `{"a": 1}` — a valid single CBOR item for the `any`-typed `meta` field.
fn meta() -> Vec<u8> {
    vec![0xa1, 0x61, 0x61, 0x01]
}

/// The same value built natively. This is the oracle for the byte-equality class.
fn native_record() -> cddl_lib::Record {
    let mut root = cddl_lib::Node::new("root".to_owned());
    root.children = Some(vec![cddl_lib::Node::new("kid".to_owned())]);
    cddl_lib::Record::new(
        ID,
        NAME.to_owned(),
        cddl_lib::Hash::new(digest_bytes()).unwrap(),
        cddl_lib::Int::new_nint(DELTA_NINT),
        cddl_lib::non_empty::NonEmptyVec::try_from(tags()).unwrap(),
        props().into_iter().collect(),
        cddl_lib::Color::I1,
        cddl_lib::Value::new_text("choice".to_owned()),
        root,
        cddl_lib::any_cbor::AnyCbor::from_cbor_bytes(&meta()).unwrap(),
    )
}

/// The component handles for that same value, plus the ones its constructor borrowed — kept alive so
/// the survival checks can use them.
struct Handles {
    record: ResourceAny,
    hash: ResourceAny,
    choice: ResourceAny,
    root: ResourceAny,
}

fn mk_node(
    store: &mut Store<component_host::Ctx>,
    api: &api::Guest,
    label: &str,
    kids: &[ResourceAny],
) -> Result<ResourceAny> {
    let node = api.node().call_constructor(&mut *store, label)?;
    if !kids.is_empty() {
        api.node().call_set_children(&mut *store, node, kids)?;
    }
    Ok(node)
}

fn mk_handles(store: &mut Store<component_host::Ctx>, api: &api::Guest) -> Result<Handles> {
    let hash = api
        .hash()
        .call_constructor(&mut *store, &digest_bytes())?
        .expect("a 32-byte digest must construct");
    let choice = api.value().call_new_text(&mut *store, "choice")?;
    let kid = mk_node(store, api, "kid", &[])?;
    let root = mk_node(store, api, "root", &[kid])?;
    let record = api
        .record()
        .call_constructor(
            &mut *store,
            ID,
            NAME,
            hash,
            api::Int::Nint(DELTA_NINT),
            &tags(),
            &props(),
            api::Color::I1,
            choice,
            root,
            &meta(),
        )?
        .expect("valid arguments must construct");
    Ok(Handles {
        record,
        hash,
        choice,
        root,
    })
}

// --- the linker requirement -----------------------------------------------------------------------

/// The negative control for `component_host::load`: without `wasmtime-wasi` the reactor's `wasi:*`
/// imports are unresolvable, so instantiation fails before any assertion can run. Without this the
/// linker line above reads as precaution.
#[test]
fn wasi_is_required_in_the_linker() {
    let err = match component_host::load_without_wasi() {
        Ok(_) => panic!("an empty linker must not satisfy the reactor's wasi imports"),
        Err(e) => format!("{e:?}"),
    };
    assert!(
        err.contains("wasi:"),
        "expected an unresolved `wasi:*` import, got: {err}"
    );
}

// --- class 1: construction + accessor read-back ---------------------------------------------------

#[test]
fn construction_and_accessor_read_back() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();
    let hs = mk_handles(store, api)?;
    let r = api.record();

    assert_eq!(r.call_id(&mut *store, hs.record)?, ID);
    assert_eq!(r.call_name(&mut *store, hs.record)?, NAME);
    assert!(matches!(
        r.call_delta(&mut *store, hs.record)?,
        api::Int::Nint(DELTA_NINT)
    ));
    assert_eq!(r.call_tags(&mut *store, hs.record)?, tags());
    assert_eq!(r.call_props(&mut *store, hs.record)?, props());
    assert_eq!(r.call_kind(&mut *store, hs.record)?, api::Color::I1);
    assert_eq!(r.call_meta(&mut *store, hs.record)?, meta());
    // The optional fields the constructor never took are absent, which is the state the rust `new`
    // leaves them in.
    assert_eq!(r.call_aliases(&mut *store, hs.record)?, None);
    assert_eq!(r.call_extra(&mut *store, hs.record)?, None);
    assert_eq!(r.call_note(&mut *store, hs.record)?, None);

    // Composite getters hand back fresh owned handles; read through them.
    let digest = r.call_digest(&mut *store, hs.record)?;
    assert_eq!(api.hash().call_get(&mut *store, digest)?, digest_bytes());
    let choice = r.call_choice(&mut *store, hs.record)?;
    assert_eq!(api.value().call_kind(&mut *store, choice)?, api::ValueKind::Text);
    assert_eq!(
        api.value().call_as_text(&mut *store, choice)?,
        Some("choice".to_owned())
    );
    assert_eq!(api.value().call_as_uint(&mut *store, choice)?, None);
    let root = r.call_root(&mut *store, hs.record)?;
    assert_eq!(api.node().call_label(&mut *store, root)?, "root");
    let kids = api
        .node()
        .call_children(&mut *store, root)?
        .expect("the root was constructed with one child");
    assert_eq!(kids.len(), 1);
    assert_eq!(api.node().call_label(&mut *store, kids[0])?, "kid");
    assert_eq!(api.node().call_children(&mut *store, kids[0])?, None);

    // The setters the OPTIONAL fields carry round-trip too.
    r.call_set_note(&mut *store, hs.record, "hello")?;
    assert_eq!(r.call_note(&mut *store, hs.record)?, Some("hello".to_owned()));
    r.call_set_aliases(&mut *store, hs.record, &["a".to_owned()])?
        .expect("a non-empty alias list must be accepted");
    assert_eq!(
        r.call_aliases(&mut *store, hs.record)?,
        Some(vec!["a".to_owned()])
    );

    // The choice's OTHER two arms, so `kind` and `as-<variant>` are exercised across the whole enum
    // rather than on the one arm the fixture value happens to use.
    let by_uint = api.value().call_new_uint(&mut *store, 7)?;
    assert_eq!(
        api.value().call_kind(&mut *store, by_uint)?,
        api::ValueKind::Uint
    );
    assert_eq!(api.value().call_as_uint(&mut *store, by_uint)?, Some(7));
    assert_eq!(api.value().call_as_bytes(&mut *store, by_uint)?, None);
    let by_bytes = api.value().call_new_bytes(&mut *store, &[1, 2, 3])?;
    assert_eq!(
        api.value().call_kind(&mut *store, by_bytes)?,
        api::ValueKind::Bytes
    );
    assert_eq!(
        api.value().call_as_bytes(&mut *store, by_bytes)?,
        Some(vec![1, 2, 3])
    );

    // The c-style enum in the INBOUND direction too: `kind` is mandatory, so a second record is
    // what carries a different case across, and reading it back is what proves the mapping is not
    // one-way.
    for color in [api::Color::I0, api::Color::I1, api::Color::I2] {
        let hash = api
            .hash()
            .call_constructor(&mut *store, &digest_bytes())?
            .unwrap();
        let choice = api.value().call_new_uint(&mut *store, 1)?;
        let root = mk_node(store, api, "root", &[])?;
        let other = api
            .record()
            .call_constructor(
                &mut *store,
                ID,
                NAME,
                hash,
                api::Int::Uint(1),
                &tags(),
                &props(),
                color,
                choice,
                root,
                &meta(),
            )?
            .expect("valid arguments must construct");
        assert_eq!(r.call_kind(&mut *store, other)?, color);
    }
    Ok(())
}

// --- class 2: byte round-trip, byte-EQUAL against native serialization ----------------------------

#[test]
fn byte_roundtrip_is_byte_equal_to_native_serialization() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();
    let hs = mk_handles(store, api)?;

    let via_component = api.record().call_to_cbor_bytes(&mut *store, hs.record)?;
    let native = native_record().to_cbor_bytes();
    assert_eq!(
        via_component, native,
        "`to-cbor-bytes` across the boundary must be byte-identical to the rust crate's own \
         serialization of the same value"
    );

    // The other direction: native bytes in, accessors out, and re-serializing reproduces them.
    let back = api
        .record()
        .call_from_cbor_bytes(&mut *store, &native)?
        .expect("bytes the rust crate wrote must deserialize");
    assert_eq!(api.record().call_id(&mut *store, back)?, ID);
    assert_eq!(api.record().call_name(&mut *store, back)?, NAME);
    assert_eq!(api.record().call_tags(&mut *store, back)?, tags());
    assert_eq!(api.record().call_meta(&mut *store, back)?, meta());
    assert_eq!(
        api.record().call_to_cbor_bytes(&mut *store, back)?,
        native,
        "re-serializing a deserialized handle must reproduce the same bytes"
    );

    // The same property one level down, on each leaf resource class: a bounded wrapper, a recursive
    // record and a type choice.
    assert_eq!(
        api.hash().call_to_cbor_bytes(&mut *store, hs.hash)?,
        cddl_lib::Hash::new(digest_bytes()).unwrap().to_cbor_bytes()
    );
    let mut native_root = cddl_lib::Node::new("root".to_owned());
    native_root.children = Some(vec![cddl_lib::Node::new("kid".to_owned())]);
    assert_eq!(
        api.node().call_to_cbor_bytes(&mut *store, hs.root)?,
        native_root.to_cbor_bytes()
    );
    assert_eq!(
        api.value().call_to_cbor_bytes(&mut *store, hs.choice)?,
        cddl_lib::Value::new_text("choice".to_owned()).to_cbor_bytes()
    );
    Ok(())
}

// --- class 3: fallible doors return Err, never trap, and leave the instance usable ----------------

#[test]
fn fallible_doors_return_err_and_leave_the_instance_usable() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();

    // A fallible CONSTRUCTOR: the bounded wrapper's window.
    let short = api
        .hash()
        .call_constructor(&mut *store, &vec![0u8; 31])?
        .expect_err("31 bytes must be rejected");
    let long = api
        .hash()
        .call_constructor(&mut *store, &vec![0u8; 33])?
        .expect_err("33 bytes must be rejected");
    // The error string is the rust crate's own `Display`, not a boundary-invented message.
    assert_eq!(
        short,
        cddl_lib::Hash::new(vec![0u8; 31]).unwrap_err().to_string()
    );
    assert!(!long.is_empty());
    // The instance survived both rejections — the property a trap would destroy.
    assert!(api
        .hash()
        .call_constructor(&mut *store, &digest_bytes())?
        .is_ok());

    // A fallible RECORD constructor, fallible for a different reason: the despecialized NonEmpty
    // list re-enters its `TryFrom` door at the consuming constructor.
    let hash = api
        .hash()
        .call_constructor(&mut *store, &digest_bytes())?
        .unwrap();
    let choice = api.value().call_new_text(&mut *store, "choice")?;
    let root = mk_node(store, api, "root", &[])?;
    let empty: Vec<String> = vec![];
    let rejected = api
        .record()
        .call_constructor(
            &mut *store,
            ID,
            NAME,
            hash,
            api::Int::Uint(1),
            &empty,
            &props(),
            api::Color::I0,
            choice,
            root,
            &meta(),
        )?
        .expect_err("an empty `tags` list must be rejected by the NonEmpty door");
    assert_eq!(
        rejected,
        cddl_lib::non_empty::NonEmptyVec::<String>::try_from(vec![])
            .unwrap_err()
            .to_string()
    );

    // A fallible SETTER, at the same door — the one member with no rust constructor between the
    // caller and the field.
    let hs = mk_handles(store, api)?;
    let set_err = api
        .record()
        .call_set_aliases(&mut *store, hs.record, &empty)?
        .expect_err("`set-aliases` must reject an empty list too");
    assert!(!set_err.is_empty());
    // ...and the record is untouched and still usable after the rejection.
    assert_eq!(api.record().call_aliases(&mut *store, hs.record)?, None);
    assert_eq!(api.record().call_id(&mut *store, hs.record)?, ID);

    // The bounded reject set crosses WIT as a list, but the component must restore BOTH bounds and
    // uniqueness through BoundedOrderedSet's one fallible door. Every rejection is an inner Err,
    // never an outer wasmtime error/trap, and the same instance accepts a valid value afterwards.
    let below = api
        .record()
        .call_set_unique(&mut *store, hs.record, &vec![1])?
        .expect_err("a one-element bounded set must reject below its minimum");
    assert_eq!(
        below,
        cddl_lib::ordered_set::BoundedOrderedSet::<u64, 2, 3>::try_from(vec![1])
            .unwrap_err()
            .to_string()
    );
    let above = api
        .record()
        .call_set_unique(&mut *store, hs.record, &vec![1, 2, 3, 4])?
        .expect_err("a four-element bounded set must reject above its maximum");
    assert_eq!(
        above,
        cddl_lib::ordered_set::BoundedOrderedSet::<u64, 2, 3>::try_from(vec![1, 2, 3, 4])
            .unwrap_err()
            .to_string()
    );
    let duplicate = api
        .record()
        .call_set_unique(&mut *store, hs.record, &vec![1, 1])?
        .expect_err("a duplicate bounded set member must reject");
    assert_eq!(
        duplicate,
        cddl_lib::ordered_set::BoundedOrderedSet::<u64, 2, 3>::try_from(vec![1, 1])
            .unwrap_err()
            .to_string()
    );
    assert_eq!(api.record().call_unique(&mut *store, hs.record)?, None);
    api.record()
        .call_set_unique(&mut *store, hs.record, &vec![4, 2, 9])?
        .expect("a distinct in-window bounded set must be accepted after errors");
    assert_eq!(
        api.record().call_unique(&mut *store, hs.record)?,
        Some(vec![4, 2, 9]),
        "accepted bounded-set values retain their insertion order across the component boundary"
    );
    Ok(())
}

// --- class 4: option nesting --------------------------------------------------------------------

/// `option<option<string>>` has three observable, distinct states. The wasm face's flatten /
/// `has_*` workaround is not needed here, and this is what says so.
#[test]
fn nested_option_has_three_distinct_states() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();
    let hs = mk_handles(store, api)?;
    let r = api.record();

    // absent: the field itself is not present
    assert_eq!(r.call_nested(&mut *store, hs.record)?, None);

    // present, null -> `some(none)`
    r.call_set_nested(&mut *store, hs.record, None)?;
    assert_eq!(r.call_nested(&mut *store, hs.record)?, Some(None));

    // present, with a value -> `some(some(v))`
    r.call_set_nested(&mut *store, hs.record, Some("hello"))?;
    assert_eq!(
        r.call_nested(&mut *store, hs.record)?,
        Some(Some("hello".to_owned()))
    );

    // The three states are distinct on the WIRE too, so this is a boundary fact rather than a
    // host-side coincidence of `Option`.
    let mut absent = native_record();
    let mut null = native_record();
    null.nested = Some(None);
    let mut valued = native_record();
    valued.nested = Some(Some("hello".to_owned()));
    absent.nested = None;
    let bytes: Vec<Vec<u8>> = [absent, null, valued]
        .iter()
        .map(|v| v.to_cbor_bytes())
        .collect();
    assert_ne!(bytes[0], bytes[1]);
    assert_ne!(bytes[1], bytes[2]);
    Ok(())
}

// --- class 5: returned collections are snapshots --------------------------------------------------

#[test]
fn returned_values_are_snapshots_in_both_directions() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();
    let hs = mk_handles(store, api)?;
    let n = api.node();

    // RETURN direction: mutating what a getter handed back must not reach the parent.
    let root1 = api.record().call_root(&mut *store, hs.record)?;
    n.call_set_children(&mut *store, root1, &[])?;
    assert_eq!(n.call_children(&mut *store, root1)?, Some(vec![]));
    let root2 = api.record().call_root(&mut *store, hs.record)?;
    let kids = n
        .call_children(&mut *store, root2)?
        .expect("the record's own root still has its child");
    assert_eq!(kids.len(), 1);

    // One level deeper: the elements of a returned collection are snapshots too.
    let kid = kids[0];
    n.call_set_children(&mut *store, kid, &[root2])?;
    let fresh_root = api.record().call_root(&mut *store, hs.record)?;
    let fresh_kids = n.call_children(&mut *store, fresh_root)?.unwrap();
    assert_eq!(n.call_children(&mut *store, fresh_kids[0])?, None);

    // PARAMETER direction: the handle the record borrowed at construction is not aliased into it.
    // Emptying the LENT handle's children must leave the record's stored copy at its one child —
    // `is_some()` alone would not say that, since an aliased read would report `some([])`.
    n.call_set_children(&mut *store, hs.root, &[])?;
    assert_eq!(n.call_children(&mut *store, hs.root)?, Some(vec![]));
    let after = api.record().call_root(&mut *store, hs.record)?;
    let after_kids = n.call_children(&mut *store, after)?.expect(
        "mutating the handle the constructor BORROWED cleared the stored value — the parameter was \
         aliased rather than copied",
    );
    assert_eq!(
        after_kids.len(),
        1,
        "the record's stored root followed the lent handle's mutation — the constructor parameter \
         was aliased rather than copied"
    );
    Ok(())
}

// --- class 6: the same-handle re-entrancy case ----------------------------------------------------

/// The canonical ABI lets a caller lend the same handle twice into one call, so `x.set-children([x])`
/// is expressible (`ResourceAny` is `Copy`) and type-legal for any collection-mediated recursive
/// type. Glue holding two `RefCell` guards at once compiles clean in debug AND release and traps
/// only on this call — and a trap poisons the instance, so in a composed topology one aliased call
/// kills a shared dependency for every consumer.
///
/// The claim is the ABSENCE of a trap. The guest-side panic text is deliberately NOT pinned: the
/// message wording is not a contract, and a trap surfaces host-side only as an opaque
/// `wasm 'unreachable' instruction executed` anyway.
#[test]
fn the_same_handle_as_receiver_and_argument_does_not_trap() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();
    let n = api.node();

    let kid = mk_node(store, api, "kid", &[])?;
    let x = mk_node(store, api, "x", &[kid])?;

    // `?` on the outer `wasmtime::Result` is the assertion: a trap fails here.
    n.call_set_children(&mut *store, x, &[x])?;

    // No trap, so `x` is still usable — the property that dies with a poisoned instance.
    assert_eq!(n.call_label(&mut *store, x)?, "x");
    let kids = n.call_children(&mut *store, x)?.unwrap();
    assert_eq!(kids.len(), 1);
    // The stored child is a SNAPSHOT of `x` as it was at call time: label "x", one child "kid".
    assert_eq!(n.call_label(&mut *store, kids[0])?, "x");
    let grandkids = n.call_children(&mut *store, kids[0])?.unwrap();
    assert_eq!(grandkids.len(), 1);
    assert_eq!(n.call_label(&mut *store, grandkids[0])?, "kid");

    // Not an aliased cycle but a value copy, and the DEPTH is what proves it: a second
    // `x.set-children([x])` stores the state x reached after the FIRST one, so the tree grows one
    // finite level (x → x' → x'' → kid) instead of closing on itself. An aliased store would have
    // no such level — every level would be the same node.
    n.call_set_children(&mut *store, x, &[x])?;
    let l1 = n.call_children(&mut *store, x)?.unwrap();
    assert_eq!(l1.len(), 1);
    let l2 = n.call_children(&mut *store, l1[0])?.unwrap();
    assert_eq!(n.call_label(&mut *store, l2[0])?, "x");
    let l3 = n.call_children(&mut *store, l2[0])?.unwrap();
    assert_eq!(
        n.call_label(&mut *store, l3[0])?,
        "kid",
        "the second lend did not store a snapshot of the state the first one produced"
    );
    assert_eq!(
        n.call_children(&mut *store, l3[0])?,
        None,
        "the copied tree does not bottom out — the stored child aliases its parent"
    );

    // Everything else in the instance is still usable, which is the property a trap would have
    // taken away from every later caller.
    let hs = mk_handles(store, api)?;
    assert_eq!(api.record().call_id(&mut *store, hs.record)?, ID);
    Ok(())
}

// --- class 7: the any-cbor edges ------------------------------------------------------------------

#[test]
fn any_cbor_round_trips_and_re_encodes_canonically() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();
    let hs = mk_handles(store, api)?;
    let r = api.record();

    // Canonical single items round-trip byte-exactly, across every major type.
    for (name, bytes) in [
        ("uint", vec![0x01u8]),
        ("nint", vec![0x24]),
        ("bytes", vec![0x42, 0x01, 0x02]),
        ("text", vec![0x62, 0x68, 0x69]),
        ("array", vec![0x82, 0x01, 0x02]),
        ("map", vec![0xa1, 0x61, 0x61, 0x01]),
        ("tag", vec![0xd8, 0x18, 0x01]),
        ("bool", vec![0xf5]),
        ("null", vec![0xf6]),
    ] {
        r.call_set_extra(&mut *store, hs.record, &bytes)?
            .unwrap_or_else(|e| panic!("{name} must be accepted: {e}"));
        assert_eq!(
            r.call_extra(&mut *store, hs.record)?,
            Some(bytes),
            "{name} must round-trip byte-exactly"
        );
    }

    // THE caveat, under this (non-preserve) posture: the getter returns a CANONICAL RE-ENCODING of
    // the stored value, never a replay of the caller's bytes. Indefinite-length in, definite out —
    // and byte-exact against the RUST crate's serialization, which is the thing it is promised to
    // agree with.
    let indefinite = vec![0x9fu8, 0x01, 0x02, 0xff];
    r.call_set_extra(&mut *store, hs.record, &indefinite)?
        .expect("an indefinite-length array is valid CBOR");
    let read_back = r
        .call_extra(&mut *store, hs.record)?
        .expect("the field was just set");
    assert_ne!(read_back, indefinite);
    assert_eq!(read_back, vec![0x82, 0x01, 0x02]);
    assert_eq!(
        read_back,
        cddl_lib::any_cbor::AnyCbor::from_cbor_bytes(&indefinite)
            .unwrap()
            .to_cbor_bytes()
    );
    Ok(())
}

#[test]
fn any_cbor_takes_exactly_one_item_and_rejects_the_rest_without_trapping() -> Result<()> {
    let mut h = load()?;
    let (store, api) = h.split();
    let hs = mk_handles(store, api)?;
    let r = api.record();

    // The ONE-ITEM rule: two concatenated items are rejected outright, never truncated to the first.
    let trailing = r
        .call_set_extra(&mut *store, hs.record, &vec![0x01, 0x02])?
        .expect_err("two concatenated items must be rejected");
    assert!(!trailing.is_empty());
    assert_eq!(
        r.call_extra(&mut *store, hs.record)?,
        None,
        "a rejected `set-extra` must not have written the first item"
    );

    for (name, bytes) in [
        ("empty", vec![]),
        ("truncated array", vec![0x82u8, 0x01]),
        ("dangling break", vec![0xff]),
        ("reserved major/ai", vec![0x1c]),
    ] {
        // `?` on the outer result is the point: invalid CBOR crosses as an error RETURN, not a trap.
        let e = r
            .call_set_extra(&mut *store, hs.record, &bytes)?
            .expect_err("invalid CBOR must be rejected");
        assert!(!e.is_empty(), "{name} must produce a non-empty error string");
        // The instance is still usable after every rejection.
        assert_eq!(r.call_id(&mut *store, hs.record)?, ID);
        assert_eq!(r.call_meta(&mut *store, hs.record)?, meta());
    }

    // The FREE function on the alias — an interface-level door, reached with no receiver — agrees
    // about both halves.
    for (bytes, want) in [
        (vec![0x01u8], api::AnyCborKind::Uint),
        (vec![0x24], api::AnyCborKind::Nint),
        (vec![0x42, 0x01, 0x02], api::AnyCborKind::Bytes),
        (vec![0x62, 0x68, 0x69], api::AnyCborKind::Text),
        (vec![0x82, 0x01, 0x02], api::AnyCborKind::Array),
        (vec![0xa1, 0x61, 0x61, 0x01], api::AnyCborKind::Map),
        (vec![0xd8, 0x18, 0x01], api::AnyCborKind::Tag),
        (vec![0xf5], api::AnyCborKind::Bool),
        (vec![0xf6], api::AnyCborKind::Null),
    ] {
        let got = api
            .call_cbor_kind(&mut *store, &bytes)?
            .unwrap_or_else(|e| panic!("{bytes:02x?} must decode: {e}"));
        assert_eq!(got, want, "for {bytes:02x?}");
    }
    for bytes in [vec![], vec![0x82u8, 0x01], vec![0x01, 0x02], vec![0xff]] {
        let e = api
            .call_cbor_kind(&mut *store, &bytes)?
            .expect_err("invalid bytes must produce Err, not a trap and not a synthetic enum case");
        assert!(!e.is_empty());
    }
    assert_eq!(
        api.call_cbor_kind(&mut *store, &vec![0x01])?.unwrap(),
        api::AnyCborKind::Uint
    );
    Ok(())
}
