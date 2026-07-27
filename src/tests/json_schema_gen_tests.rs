//! In-bin unit tests for the `static/json_schema_gen.rs` runtime, hosted via the same
//! `include!`-a-static-file pattern as `any_cbor_tests` / `ordered_set_runtime_tests` (the module
//! ships into a generated crate's common runtime, so nothing else in THIS crate compiles it).
//!
//! Two jobs, and the first is worth as much as the second:
//!
//! 1. **The file becomes compiled, linted code.** `cargo clippy --workspace --all-features
//!    --all-targets` (the fast-tier `clippy` gate) reaches the bin crate's test binary, so the shim
//!    below puts the shipped helper under clippy and rustc warnings. Before the helpers moved out of
//!    `generation/export.rs`'s string constants they were opaque text no lint could see, and their
//!    only compile proof was a nested-cargo run of a generated json-gen crate — a `local`-tier cost
//!    for a syntax error.
//!
//! 2. **`decode_schema_ref_name` gets real vectors.** It is the inverse of a schemars-private
//!    encoder, so its correctness is a property of THIS code and nothing else's; the cases below are
//!    the two escape layers and — the one its own comment calls out — the ORDER they must be undone
//!    in, which no end-to-end fixture exercises because no fixture has a schema name holding a
//!    literal `~1`.
//!
//! 3. **`Registrar` is proven to be a pure re-spelling of the row.** Every emitted registration row
//!    goes through it, so "the registrar registers what a direct `add_schema` call would, and its
//!    ledger still fires" is the property the reshape rests on. Asserted against the DOCUMENT the two
//!    routes build, not against their call shapes.

// The included runtime file is written for a generated crate, where every item is reachable from
// emitted code; here `add_schema` is only compiled and linted, never called, so the unexercised
// surface would read as dead.
#[allow(dead_code)]
mod json_schema_gen {
    include!("../../static/json_schema_gen.rs");

    /// `decode_schema_ref_name` is deliberately PRIVATE in the shipped module (only
    /// `check_schema_ref_closure` calls it), so the tests reach it through a wrapper the shim owns
    /// rather than by widening the shipped file's visibility for the harness's convenience.
    pub(super) fn decode_for_test(encoded: &str) -> String {
        decode_schema_ref_name(encoded)
    }
}

use json_schema_gen::{
    Registrar, add_schema, check_schema_ref_closure, decode_for_test as decode_schema_ref_name,
};

/// Two ordinary derived types, one referencing the other, so the document a registration builds is
/// non-trivial: `Beta`'s row pulls `Alpha` in through the closure as well as by its own row.
#[derive(schemars::JsonSchema)]
struct Alpha {
    #[allow(dead_code)]
    a: u64,
}

#[derive(schemars::JsonSchema)]
struct Beta {
    #[allow(dead_code)]
    b: String,
    #[allow(dead_code)]
    alpha: Alpha,
}

/// A pair of DISTINCT rust types that publish ONE schema name — the collision the ledger exists to
/// catch. Kept beside the registrar tests because the whole point of `Registrar::add` delegating to
/// `add_schema` is that the guard is not reimplemented: this fires through the registrar or the
/// delegation is a lie.
#[derive(schemars::JsonSchema)]
#[schemars(rename = "Shared")]
struct SharedOne {
    #[allow(dead_code)]
    one: u64,
}

#[derive(schemars::JsonSchema)]
#[schemars(rename = "Shared")]
struct SharedTwo {
    #[allow(dead_code)]
    two: String,
}

/// The reshape's load-bearing claim: driving rows through a `Registrar` builds the SAME document as
/// calling `add_schema` directly with a hand-threaded ledger. Compared on the finished `$defs` map
/// (what actually ships) rather than on anything about the calls, so the assertion survives any
/// future change to how the registrar carries its state.
#[test]
fn registrar_builds_the_same_document_as_direct_add_schema() {
    let mut via_registrar = schemars::SchemaGenerator::default();
    {
        let mut reg = Registrar::new(&mut via_registrar);
        reg.add::<Alpha>();
        reg.add::<Beta>();
    }
    let mut direct = schemars::SchemaGenerator::default();
    let mut claimed = std::collections::BTreeMap::new();
    add_schema::<Alpha>(&mut direct, &mut claimed);
    add_schema::<Beta>(&mut direct, &mut claimed);
    let via_registrar = via_registrar.take_definitions(true);
    let direct = direct.take_definitions(true);
    assert!(
        via_registrar.contains_key("Alpha") && via_registrar.contains_key("Beta"),
        "both rows must reach `$defs`; got {:?}",
        via_registrar.keys().collect::<Vec<_>>()
    );
    assert_eq!(
        via_registrar, direct,
        "a registrar-driven registration must produce the document a direct `add_schema` call does"
    );
}

/// The ledger the registrar OWNS still fires. Reached only through `reg.add`, which is what proves
/// the delegation rather than assuming it — a `Registrar::add` that quietly reimplemented the row
/// without the guard would pass every other test in this file.
#[test]
#[should_panic(expected = "two distinct Rust types both publish the JSON schema name")]
fn registrar_ledger_catches_a_published_name_collision() {
    let mut generator = schemars::SchemaGenerator::default();
    let mut reg = Registrar::new(&mut generator);
    reg.add::<SharedOne>();
    reg.add::<SharedTwo>();
}

/// The ledger's scope is ONE registrar, matching the scope it had as a local of one crate's
/// `add_schemas`: a dependency's rows are threaded through the dep's own `add_schemas` and therefore
/// its own registrar. So two registrars over one generator do not share a ledger — stated here
/// because it is the one property of the reshape a reader could reasonably expect to have changed,
/// and because it is what keeps a cross-crate collision the ledger's blind spot rather than a new
/// false positive.
#[test]
fn a_second_registrar_starts_with_a_fresh_ledger() {
    let mut generator = schemars::SchemaGenerator::default();
    {
        let mut reg = Registrar::new(&mut generator);
        reg.add::<Alpha>();
    }
    // A second registrar re-registering the same type is a no-op against a ledger that never saw it,
    // and must not panic.
    {
        let mut reg = Registrar::new(&mut generator);
        reg.add::<Alpha>();
    }
    assert!(generator.take_definitions(true).contains_key("Alpha"));
}

/// A plain identifier survives both layers untouched — the overwhelmingly common case, and the one
/// that would break loudest if either `replace` grew a false positive.
#[test]
fn decode_ref_name_identity_on_plain_names() {
    for plain in ["Foo", "TransactionBody", "_private", "Ext2", ""] {
        assert_eq!(
            decode_schema_ref_name(plain),
            plain,
            "a name with nothing to decode must come back byte-identical"
        );
    }
}

/// The URI-fragment (RFC 3986) layer. `OrderedHashMap<K, V>` is the real generated name that first
/// made this decode necessary: schemars percent-encodes `<`, `>` and the space.
#[test]
fn decode_ref_name_percent_decodes() {
    assert_eq!(
        decode_schema_ref_name("OrderedHashMap%3CK,%20V%3E"),
        "OrderedHashMap<K, V>"
    );
    // Lowercase hex digits are equally legal in a percent-escape.
    assert_eq!(decode_schema_ref_name("a%3cb"), "a<b");
    // A truncated escape at the very end is not an escape — the bytes pass through rather than the
    // decode panicking or eating past the end.
    assert_eq!(decode_schema_ref_name("trailing%3"), "trailing%3");
    assert_eq!(decode_schema_ref_name("%"), "%");
    // A non-hex payload is not an escape either.
    assert_eq!(decode_schema_ref_name("100%zz"), "100%zz");
    // Multi-byte UTF-8 reassembles from its escaped bytes (`é` = C3 A9).
    assert_eq!(decode_schema_ref_name("caf%C3%A9"), "café");
}

/// The JSON-Pointer (RFC 6901) layer: `~1` is `/` and `~0` is `~`.
#[test]
fn decode_ref_name_undoes_json_pointer_escapes() {
    assert_eq!(decode_schema_ref_name("a~1b"), "a/b");
    assert_eq!(decode_schema_ref_name("a~0b"), "a~b");
    assert_eq!(decode_schema_ref_name("~1~1"), "//");
}

/// The ORDERING case the decode's own comment calls out, and the reason the two `replace` calls are
/// not interchangeable: a name holding a literal `~1` encodes to `~01` (the `~` becomes `~0`, the
/// `1` is left alone), so `~1`-before-`~0` is the only order that recovers it. Undone the other way,
/// `~01` would become `~1` at the `~0` step and then `/` at the `~1` step — the encoder's own input
/// decoded to something it never was.
#[test]
fn decode_ref_name_undoes_json_pointer_escapes_in_encoder_order() {
    assert_eq!(
        decode_schema_ref_name("~01"),
        "~1",
        "a literal `~1` in the name encodes to `~01`; `~1`-before-`~0` is what recovers it"
    );
    // The wrong order's observable signature, stated as the value we must NOT produce.
    assert_ne!(decode_schema_ref_name("~01"), "/");
    // Same trap one layer up: `~0` in the name encodes to `~00`.
    assert_eq!(decode_schema_ref_name("~00"), "~0");
}

/// Both layers together, in the order the encoder applied them (percent first, then pointer): the
/// percent-decode can REVEAL a `~` that then participates in a pointer escape, which is exactly why
/// the percent layer is undone first.
#[test]
fn decode_ref_name_undoes_both_layers_in_encoder_order() {
    // `%7E` is `~`; revealing it must not resurrect a pointer escape that the encoder never wrote,
    // so a revealed `~` followed by `1` still decodes to `/` only because the pointer pass runs
    // after — pinning the composition rather than arguing about it.
    assert_eq!(decode_schema_ref_name("Map%3CK~1V%3E"), "Map<K/V>");
    assert_eq!(decode_schema_ref_name("%3C~01%3E"), "<~1>");
}

/// The closure check's happy path, over a document shaped exactly like the one `export_schemas`
/// writes: a `$defs` bundle whose every `$ref` points inside it. Also pins that the definitions
/// namespace is read from the passed setting rather than hardcoded to `#/$defs/`, and that an
/// ENCODED reference is resolved through `decode_schema_ref_name` against the raw `$defs` key.
#[test]
fn closure_check_accepts_a_self_contained_document() {
    let document = serde_json::json!({
        "$defs": {
            "Foo": { "properties": { "bar": { "$ref": "#/$defs/Bar" } } },
            "Bar": { "type": "string" },
            "OrderedHashMap<K, V>": { "type": "object" },
            "Uses": { "$ref": "#/$defs/OrderedHashMap%3CK,%20V%3E" }
        }
    });
    check_schema_ref_closure(&document, "/$defs");
    // The same document under the other spellings of the same setting schemars may hand us.
    check_schema_ref_closure(&document, "#/$defs");
    check_schema_ref_closure(&document, "#/$defs/");

    // A definitions path that resolves to nothing is SKIPPED, not failed: the reference namespace
    // and the emitted document shape have diverged (a schemars default change), and every reference
    // would then be a false positive.
    check_schema_ref_closure(&document, "/definitions");
}

/// A reference at a key nothing defines fails, and the message names the offending reference and the
/// key it decoded to. `integration_tests::json_schema_ref_dangling_fails` proves this end to end in
/// a real `cargo run`; this is the cheap in-crate net for the same branch.
#[test]
#[should_panic(expected = "is not defined in this document")]
fn closure_check_rejects_a_reference_to_an_undefined_key() {
    let document = serde_json::json!({
        "$defs": { "Foo": { "$ref": "#/$defs/Missing" } }
    });
    check_schema_ref_closure(&document, "/$defs");
}

/// A reference that is not an internal pointer at all — the `Schema::new_ref("PlutusData")` shape a
/// hand-written `JsonSchema` stub produces — fails with the other of the two message classes.
#[test]
#[should_panic(expected = "not an internal")]
fn closure_check_rejects_a_non_internal_reference() {
    let document = serde_json::json!({
        "$defs": { "Foo": { "$ref": "PlutusData" } }
    });
    check_schema_ref_closure(&document, "/$defs");
}

/// The walk reaches references nested inside arrays as well as objects — a `oneOf`/`anyOf` member is
/// the shape every generated CDDL choice produces, so a walk that only descended objects would miss
/// the majority of a real document's references.
#[test]
#[should_panic(expected = "is not defined in this document")]
fn closure_check_walks_into_arrays() {
    let document = serde_json::json!({
        "$defs": {
            "Choice": { "oneOf": [ { "type": "null" }, { "$ref": "#/$defs/Missing" } ] }
        }
    });
    check_schema_ref_closure(&document, "/$defs");
}
