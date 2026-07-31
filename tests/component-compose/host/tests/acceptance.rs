//! THE acceptance test for the component face's cross-crate story: two independently generated
//! crates, built as two components, composed into one world, and driven through the flow the whole
//! feature exists to make possible — mint a dependency object, hand it to a consumer, get a
//! dependency object back, and keep using it through the dependency's own interface.
//!
//! Every other component gate stops short of this. The WIT gates judge a projection, the build gates
//! judge a compile, and `component_host` judges one component's behavior. Only here are TWO crates
//! made to agree at runtime, which is the claim the face is for: without it the consumer's
//! dependency types would be its own private resources, structurally identical to the dependency's
//! and interchangeable with nothing.
//!
//! Two shapes recur and are worth naming once:
//!
//! * **The handle that comes back is a live resource, and the value it carries is a copy.** The seam
//!   is CBOR bytes, so the consumer's getter mints a fresh object in the DEPENDENCY instance's table
//!   from the dependency's own `from-cbor-bytes`. That is what makes it addressable by the
//!   dependency's exported interface — including its setter — and it is also why it is not the same
//!   object the host passed in. Both halves are asserted; conflating them is the misreading this
//!   comment exists to prevent.
//! * **Nothing enforces instantiate-once at compose time.** The two-instance topology composes at
//!   exit 0 and decodes to a world indistinguishable from the correct one. The mistake is only
//!   observable at the first handle crossing, so the negative control asserts both halves: the
//!   composition succeeds, and the crossing does not.

use chain::serialization::ToCBORBytes as _;
use component_compose_host::{
    chain as chain_api, compose, exported_interfaces, load, load_bytes, wallet as wallet_api,
    artifacts, Ctx, Topology, CHAIN_IFACE, WALLET_IFACE,
};
use wasmtime::component::ResourceAny;
use wasmtime::{Result, Store};

const HEAD_NAME: &str = "ada";
const HEAD_AMOUNT: u64 = 1000;
const MEMO: &str = "acceptance";
const ENTRY_NAMES: [&str; 2] = ["first", "second"];

/// The same ledger built natively out of both generated rust crates. This is the oracle for the
/// byte-differential class: "the composed boundary agrees with the libraries it wraps" is checkable,
/// where "the boundary produced some bytes" is not.
fn native_ledger() -> wallet::Ledger {
    let mut head = chain::Token::new(HEAD_NAME.to_owned());
    head.amount = Some(HEAD_AMOUNT);
    let entries = ENTRY_NAMES
        .iter()
        .map(|n| chain::Token::new((*n).to_owned()))
        .collect();
    wallet::Ledger::new(head, entries, MEMO.to_owned())
}

/// Mint the head token on the DEPENDENCY's exported interface, amount set through its setter.
fn mk_head(store: &mut Store<Ctx>, chain: &chain_api::Guest) -> Result<ResourceAny> {
    let token = chain.token().call_constructor(&mut *store, HEAD_NAME)?;
    chain
        .token()
        .call_set_amount(&mut *store, token, HEAD_AMOUNT)?;
    Ok(token)
}

/// Fill the accumulator the consumer's repeated position takes, one element at a time. `push` is
/// where the per-element CBOR seam runs, and it is fallible for the same reason every other crossing
/// is: the far side's deserializer can reject what this side serialized.
fn mk_entries(
    store: &mut Store<Ctx>,
    chain: &chain_api::Guest,
    wallet: &wallet_api::Guest,
) -> Result<ResourceAny> {
    let list = wallet.token_list().call_constructor(&mut *store)?;
    for name in ENTRY_NAMES {
        let token = chain.token().call_constructor(&mut *store, name)?;
        wallet
            .token_list()
            .call_push(&mut *store, list, token)?
            .expect("a token minted on the composed dependency instance must push");
    }
    Ok(list)
}

/// Build the consumer's ledger out of dependency objects, in both parameter shapes at once.
fn mk_ledger(
    store: &mut Store<Ctx>,
    chain: &chain_api::Guest,
    wallet: &wallet_api::Guest,
) -> Result<ResourceAny> {
    let head = mk_head(store, chain)?;
    let entries = mk_entries(store, chain, wallet)?;
    let ledger = wallet
        .ledger()
        .call_constructor(&mut *store, head, entries, MEMO)?
        .expect("valid arguments must construct");
    Ok(ledger)
}

// --- the composed artifact ------------------------------------------------------------------------

/// The composed world exports BOTH interfaces.
///
/// This is what makes the acceptance flow expressible at all, and it is the property `wac plug`
/// destroys: plugging satisfies the consumer's import from the dependency's export and then drops
/// that export, leaving a world a host cannot mint a dependency object in. Read back out of the
/// encoded bytes rather than out of the graph, so the claim is about the artifact a host is handed.
#[test]
fn the_composed_world_exports_both_the_dependency_and_the_consumer() -> Result<()> {
    let (chain_wasm, wallet_wasm) = artifacts();
    let bytes = compose(&chain_wasm, &wallet_wasm, Topology::Once)?;
    let mut expected = vec![CHAIN_IFACE.to_owned(), WALLET_IFACE.to_owned()];
    expected.sort();
    assert_eq!(
        exported_interfaces(&bytes)?,
        expected,
        "the composed world must export both packages' interfaces — a single-export world is the \
         `wac plug` shape, which a host cannot mint dependency objects through"
    );
    Ok(())
}

// --- the acceptance flow --------------------------------------------------------------------------

/// **THE acceptance assertion.** A dependency object minted by the host crosses into the consumer,
/// comes back out of a consumer getter, and is still usable through the DEPENDENCY's own exported
/// interface — including its setter, whose effect reads back.
///
/// That last step is what distinguishes a live resource in the dependency instance's table from a
/// value the host happens to be holding: a setter call only reaches an object the dependency
/// instance owns. Under two dependency instances this same call cannot even be lowered, which is why
/// the negative control below is the other half of this claim rather than a separate curiosity.
///
/// The value that comes back IS a copy — the seam is CBOR bytes, so the getter deserializes a fresh
/// object rather than returning the one the constructor borrowed. Asserted here in both directions
/// so neither half can be misread: the handle differs from the one passed in, and the borrowed
/// original is untouched by a mutation applied to the returned one.
#[test]
fn a_dependency_object_crosses_into_the_consumer_and_comes_back_live() -> Result<()> {
    let mut h = load(Topology::Once)?;
    let (store, chain, wallet) = h.split();

    let head = mk_head(store, chain)?;
    let entries = mk_entries(store, chain, wallet)?;
    let ledger = wallet
        .ledger()
        .call_constructor(&mut *store, head, entries, MEMO)?
        .expect("valid arguments must construct");

    // Out of a CONSUMER getter, typed as the DEPENDENCY's resource.
    let returned = wallet
        .ledger()
        .call_head(&mut *store, ledger)?
        .expect("the head crossing must not fail for a value this run serialized");

    // Readable through the dependency's own interface: the seam round-tripped the value.
    assert_eq!(chain.token().call_name(&mut *store, returned)?, HEAD_NAME);
    assert_eq!(
        chain.token().call_amount(&mut *store, returned)?,
        Some(HEAD_AMOUNT),
        "the optional field must survive the crossing as Some, not collapse to None"
    );

    // WRITABLE through the dependency's own interface, with the effect visible on a later read: the
    // handle addresses an object the dependency instance owns.
    chain.token().call_set_amount(&mut *store, returned, 7)?;
    assert_eq!(
        chain.token().call_amount(&mut *store, returned)?,
        Some(7),
        "a setter driven through the dependency's exported interface must reach the object the \
         consumer's getter handed back — that is what makes it a live resource rather than a value"
    );

    // ... and it is a distinct object from the one the constructor borrowed, because the seam is a
    // copy. The original still reads its own amount.
    assert_ne!(
        returned, head,
        "the getter mints a fresh handle: the seam is CBOR bytes, not an alias"
    );
    assert_eq!(
        chain.token().call_amount(&mut *store, head)?,
        Some(HEAD_AMOUNT),
        "mutating the returned object must not reach back through the seam to the borrowed one"
    );
    Ok(())
}

/// The REPEATED position, end to end: dependency objects pushed one at a time into the accumulator,
/// and read back as owned dependency handles from the consumer's getter.
///
/// The accumulator exists because `list<borrow<imported-resource>>` is legal WIT whose Rust lowering
/// does not compile. That much a build gate can see. What only a run can see is that the elements
/// arrive in order, with their values intact, and come back addressable on the dependency instance.
#[test]
fn a_dependency_typed_collection_crosses_through_the_accumulator() -> Result<()> {
    let mut h = load(Topology::Once)?;
    let (store, chain, wallet) = h.split();

    let list = mk_entries(store, chain, wallet)?;
    assert_eq!(
        wallet.token_list().call_len(&mut *store, list)?,
        ENTRY_NAMES.len() as u32,
        "each push must land exactly one element"
    );

    let head = mk_head(store, chain)?;
    let ledger = wallet
        .ledger()
        .call_constructor(&mut *store, head, list, MEMO)?
        .expect("valid arguments must construct");
    let back = wallet
        .ledger()
        .call_entries(&mut *store, ledger)?
        .expect("the entries crossing must not fail for values this run serialized");

    assert_eq!(back.len(), ENTRY_NAMES.len());
    for (handle, expected) in back.iter().zip(ENTRY_NAMES) {
        assert_eq!(
            chain.token().call_name(&mut *store, *handle)?,
            expected,
            "the accumulator must preserve element ORDER across the seam, not just element count"
        );
    }
    Ok(())
}

/// The composed boundary's bytes are the native crates' bytes.
///
/// The strongest available statement that the seam is lossless: every dependency value in this
/// ledger reached the consumer's rust struct by being serialized on one side of a component boundary
/// and deserialized on the other, and the result still encodes identically to a ledger assembled
/// natively with no boundary in it at all.
#[test]
fn the_composed_boundary_agrees_with_the_native_crates_bytes() -> Result<()> {
    let mut h = load(Topology::Once)?;
    let (store, chain, wallet) = h.split();
    let ledger = mk_ledger(store, chain, wallet)?;
    assert_eq!(
        wallet.ledger().call_to_cbor_bytes(&mut *store, ledger)?,
        native_ledger().to_cbor_bytes(),
        "a value assembled ACROSS the composed boundary must encode identically to the same value \
         assembled natively — anything else means the seam changed it"
    );
    Ok(())
}

// --- the negative control -------------------------------------------------------------------------

/// Two dependency instances compose at exit 0 and produce a world INDISTINGUISHABLE from the correct
/// one.
///
/// This is the compose-time half of the instantiate-once finding, and it is the reason the
/// prescription has to be documented rather than delegated to the composer: nothing in the toolchain
/// refuses the mistake, and no inspection of the artifact's world reveals it. The size differs by a
/// few hundred bytes of instantiation metadata, which is not a signal anyone would read.
#[test]
fn two_dependency_instances_compose_and_are_indistinguishable() -> Result<()> {
    let (chain_wasm, wallet_wasm) = artifacts();
    let good = compose(&chain_wasm, &wallet_wasm, Topology::Once)?;
    let bad = compose(&chain_wasm, &wallet_wasm, Topology::TwoInstances)?;
    // Non-vacuity, asserted here rather than inferred from the sibling test below: the two really
    // are different compositions. They differ only in instantiation metadata — a few hundred bytes
    // on this fixture — which is precisely the point, and a `Topology` that had stopped producing a
    // second instance would make everything else in this test pass for the wrong reason.
    assert_ne!(
        bad, good,
        "the two topologies must produce DIFFERENT artifacts — if they do not, this control is \
         comparing a composition against itself"
    );
    assert_eq!(
        exported_interfaces(&bad)?,
        exported_interfaces(&good)?,
        "the two-instance topology must be shown to produce the SAME world — if it ever stops \
         doing so, the composer has grown the check this control exists because it lacks"
    );
    // Loading it is part of the claim: the mistake survives instantiation too, so it cannot be
    // caught at load time either.
    load_bytes(&bad)?;
    Ok(())
}

/// ... and the runtime half: the first handle crossing fails.
///
/// Resource types are generative per instantiation, so the token minted through the world's exported
/// dependency interface (the SECOND instance) is a different type from the one the consumer was
/// wired to (the first), despite both being spelled `cddl:chain/types@0.1.0`. The failure surfaces
/// as an `Err` on the outer `wasmtime::Result` — the trap channel — at the first call that tries to
/// pass one across.
///
/// The message text is wasmtime's, not ours, and it is asserted loosely: the claim is that the
/// crossing is REFUSED and that the refusal names resource types, not that a diagnostic string is
/// frozen.
#[test]
fn a_handle_from_the_wrong_dependency_instance_cannot_cross() -> Result<()> {
    let mut h = load(Topology::TwoInstances)?;
    let (store, chain, wallet) = h.split();

    // Minting on either side is fine — the topology is only wrong where the two meet.
    let token = chain.token().call_constructor(&mut *store, HEAD_NAME)?;
    let list = wallet.token_list().call_constructor(&mut *store)?;

    let err = wallet
        .token_list()
        .call_push(&mut *store, list, token)
        .expect_err(
            "a handle minted on a SECOND dependency instance must not be accepted by a consumer \
             wired to the first — if it is, resource identity is not generative and the \
             single-instantiation prescription is unnecessary",
        );
    let text = format!("{err:#}").to_lowercase();
    assert!(
        text.contains("resource"),
        "the refusal must name what went wrong (wasmtime says `mismatched resource types`), got: \
         {err:#}"
    );
    Ok(())
}
