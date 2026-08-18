//! In-bin unit tests for the `static/json_schema_gen.rs` runtime, hosted via the same
//! `include!`-a-static-file pattern as `any_cbor_tests` / `ordered_set_runtime_tests` (the module
//! ships into a generated crate's common runtime, so nothing else in THIS crate compiles it).
//!
//! Four jobs, and the first is worth as much as the other three:
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
//!    in. `tests/json-extern` reaches the private encoder end-to-end with a name that carries a
//!    literal `~1` AND multi-byte UTF-8; the direct vectors below keep malformed inputs and each
//!    inverse layer independently legible.
//!
//! 3. **`Registrar` is proven to be a pure re-spelling of the row.** Every emitted registration row
//!    goes through it, so "the registrar registers what a direct `add_schema` call would, and its
//!    ledger still fires" is the property the reshape rests on. Asserted against the DOCUMENT the two
//!    routes build, not against their call shapes.
//!
//! 4. **`custom_schema_impl!` is expanded here, not only in a generated crate.** The macro writes the
//!    `schemars::JsonSchema` impl a `@custom_json` type needs; a macro body is text until something
//!    expands it, so without an invocation in this crate its only compile proof would again be a
//!    nested-cargo run. Both arms are invoked below, and the reference-namespace retarget they
//!    delegate to gets its own vectors against `custom_schema_body` directly — including the case the
//!    tool's own emitted generator can never reach, a document whose namespace is not `#/$defs`.

// The included runtime file is written for a generated crate, where every item is reachable from
// emitted code; here `add_schema` is only compiled and linted, never called, so the unexercised
// surface would read as dead.
//
// `pub(crate)` and `#[macro_use]`, both for `custom_schema_impl!`. The macro's expansion names
// `$crate::json_schema_gen::custom_schema_body`, and `$crate` inside this test binary is the bin
// crate root — so the module is aliased there (`main.rs`) and must be visible enough to alias.
// `#[macro_use]` is what lets the invocations below name the macro at all: rustc refuses an absolute
// path (`crate::custom_schema_impl!`) to a `macro_export` macro that a macro expansion defined, and
// `include!` makes this one exactly that. In a generated crate the file is a real module and the
// absolute path documented on the macro is the one that works.
#[allow(dead_code)]
#[macro_use]
pub(crate) mod json_schema_gen {
    include!("../../static/json_schema_gen.rs");

    /// `decode_schema_ref_name` is deliberately PRIVATE in the shipped module (only
    /// `check_schema_ref_closure` calls it), so the tests reach it through a wrapper the shim owns
    /// rather than by widening the shipped file's visibility for the harness's convenience.
    pub(super) fn decode_for_test(encoded: &str) -> String {
        decode_schema_ref_name(encoded)
    }

    /// Same arrangement for the two private normalisation helpers that `add_schema`,
    /// `check_schema_ref_closure` and `retarget_defs_references` all read the reference namespace
    /// through.
    pub(super) fn definitions_ref_prefix_for_test(definitions_path: &str) -> String {
        definitions_ref_prefix(definitions_path)
    }

    pub(super) fn definitions_pointer_for_test(definitions_path: &str) -> &str {
        definitions_pointer(definitions_path)
    }
}

use json_schema_gen::{
    Registrar, add_schema, check_schema_ref_closure, custom_schema_body,
    decode_for_test as decode_schema_ref_name, definitions_pointer_for_test as definitions_pointer,
    definitions_ref_prefix_for_test as definitions_ref_prefix, retarget_defs_references,
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

/// The two-argument arm of `custom_schema_impl!`, over a hand-authored body carrying every reference
/// shape at once: the authoring convention's `#/$defs/<Name>` (which the retarget owns) plus the
/// three the retarget must leave alone. The file lives under `tests/`, reached by a path relative to
/// THIS file — which is also the assertion behind the macro's fact 4, since a resolution relative to
/// the DEFINING file (`static/json_schema_gen.rs`) would not find it.
#[allow(dead_code)]
struct Custom;

custom_schema_impl!(Custom, "../../tests/json-schema-custom/unit/Custom.json");

/// A body that references only definitions the document really has, for the closure-passing vector.
#[allow(dead_code)]
struct Closed;

custom_schema_impl!(Closed, "../../tests/json-schema-custom/unit/Closed.json");

/// The three-argument arm, on the shape the two-argument one cannot serve: the type at the
/// invocation is not a bare ident, so the published name has to be stated.
#[allow(dead_code)]
struct Wrapped<T>(T);

custom_schema_impl!(
    Wrapped<u64>,
    "../../tests/json-schema-custom/unit/WrappedU64.json",
    "WrappedU64"
);

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

/// Distinct inline types may share a display name: at a transitive boundary neither is a `$defs`
/// claimant, so `Registrar::claim_reachable` must leave both out of the ledger. Rows still use
/// `add`, whose inline-root publication has its separate conflicting-body check.
struct InlineSharedOne;

impl schemars::JsonSchema for InlineSharedOne {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed("InlineShared")
    }

    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        <u64 as schemars::JsonSchema>::json_schema(generator)
    }

    fn inline_schema() -> bool {
        true
    }
}

struct InlineSharedTwo;

impl schemars::JsonSchema for InlineSharedTwo {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed("InlineShared")
    }

    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        <String as schemars::JsonSchema>::json_schema(generator)
    }

    fn inline_schema() -> bool {
        true
    }
}

/// A non-row type and a later row that claim the same ref-unsafe published name. They are distinct
/// schema identities, so schemars assigns the row a suffixed name rather than merging the schemas.
/// The first type is deliberately registered outside `Registrar`, as a type reached transitively
/// through another row would be in a generated crate.
struct UnsafeNameWinner;

impl schemars::JsonSchema for UnsafeNameWinner {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed("Café/~1")
    }

    fn schema_id() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed("UnsafeNameWinner")
    }

    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        <u64 as schemars::JsonSchema>::json_schema(generator)
    }
}

struct UnsafeNameClaimant;

impl schemars::JsonSchema for UnsafeNameClaimant {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed("Café/~1")
    }

    fn schema_id() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed("UnsafeNameClaimant")
    }

    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        <String as schemars::JsonSchema>::json_schema(generator)
    }
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

/// A reachability claim is a ledger-only preflight: it catches the same name collision as a row,
/// but must not call `subschema_for` or otherwise materialize a definition before the real root
/// reaches it.
#[test]
fn registrar_reachable_claim_is_definition_free_and_uses_the_ledger() {
    let mut generator = schemars::SchemaGenerator::default();
    {
        let mut reg = Registrar::new(&mut generator);
        reg.claim_reachable::<Alpha>();
    }
    assert!(
        generator.definitions().is_empty(),
        "a reachability claim must not materialize a $defs entry"
    );

    let mut generator = schemars::SchemaGenerator::default();
    let mut reg = Registrar::new(&mut generator);
    reg.claim_reachable::<SharedOne>();
    let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        reg.claim_reachable::<SharedTwo>();
    }));
    assert!(
        panic.is_err(),
        "reachability claims must share the row name-injectivity ledger"
    );
}

#[test]
fn registrar_reachable_claim_skips_inline_types() {
    let mut generator = schemars::SchemaGenerator::default();
    let mut reg = Registrar::new(&mut generator);
    reg.claim_reachable::<InlineSharedOne>();
    reg.claim_reachable::<InlineSharedTwo>();
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

/// The kept-its-own-name guard must cover ref-unsafe names too. The winner is registered directly
/// with schemars BEFORE the registrar exists, so the registrar's name ledger cannot observe it; its
/// sole row is the claimant below. Their explicit distinct schema IDs make schemars hand the
/// claimant an encoded `Café/~12` ref. If this panics from the ledger instead, or if it does not
/// panic at all, this vector has failed to exercise check B.
#[test]
#[should_panic(expected = "but the document assigned it \"Café/~12\"")]
fn kept_own_name_guard_catches_a_percent_encoded_stolen_name() {
    assert_ne!(
        <UnsafeNameWinner as schemars::JsonSchema>::schema_id(),
        <UnsafeNameClaimant as schemars::JsonSchema>::schema_id(),
        "the two types must take schemars' suffixed-name path rather than merge"
    );
    let mut generator = schemars::SchemaGenerator::default();
    let winner = generator.subschema_for::<UnsafeNameWinner>();
    assert_eq!(
        winner.get("$ref").and_then(|reference| reference.as_str()),
        Some("#/$defs/Caf%C3%A9~1~01"),
        "the test must exercise schemars' private UTF-8 + JSON-Pointer encoder, not a guessed ref"
    );
    let mut reg = Registrar::new(&mut generator);
    reg.add::<UnsafeNameClaimant>();
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

/// A generator whose definitions live somewhere other than `$defs`. Unreachable through the tool's
/// own emitted `export_schemas()`, which always builds a `SchemaGenerator::default()` — but
/// `add_schemas` takes the generator as a PARAMETER, so a consumer composing several crates' rows
/// supplies their own, and every helper here reads the namespace off it rather than assuming.
fn generator_with_definitions_path(definitions_path: &'static str) -> schemars::SchemaGenerator {
    schemars::generate::SchemaSettings::default()
        .with(|settings| settings.definitions_path = definitions_path.into())
        .into_generator()
}

/// Every spelling schemars' settings may carry for ONE namespace normalises to one JSON pointer. The
/// three readers (the row guard's stolen-name check, the closure check, the hand-authored-body
/// retarget) share this, so a disagreement here is a disagreement between them on one document.
#[test]
fn definitions_pointer_normalises_every_spelling_of_one_setting() {
    for spelling in ["/$defs", "#/$defs", "/$defs/", "#/$defs/"] {
        assert_eq!(
            definitions_pointer(spelling),
            "/$defs",
            "`{spelling}` must normalise to the pointer form"
        );
    }
    // Not hardcoded to `$defs`: a non-default namespace normalises the same way.
    assert_eq!(definitions_pointer("#/definitions/"), "/definitions");
    // schemars' own default is already the normal form, so the common case is a pure pass-through.
    assert_eq!(
        definitions_pointer(
            &schemars::SchemaGenerator::default()
                .settings()
                .definitions_path
        ),
        "/$defs"
    );
}

/// The reference form is DERIVED from the pointer form, which is what makes the two incapable of
/// disagreeing: one `#`, one `/`, whatever the setting was spelled as.
#[test]
fn definitions_ref_prefix_is_derived_from_the_pointer() {
    for spelling in ["/$defs", "#/$defs", "/$defs/", "#/$defs/"] {
        assert_eq!(definitions_ref_prefix(spelling), "#/$defs/");
    }
    assert_eq!(definitions_ref_prefix("/definitions"), "#/definitions/");
    // Stated as the relationship rather than as two independent strings.
    for spelling in ["/$defs", "#/definitions/", "/anything"] {
        assert_eq!(
            definitions_ref_prefix(spelling),
            format!("#{}/", definitions_pointer(spelling))
        );
    }
}

/// The retarget rewrites the authoring convention's prefix and nothing else, at every depth and
/// through arrays. The three untouched shapes are the point of the vector: a bare relative name, an
/// `http(s)://` URL and a pointer into another document are exactly what `check_schema_ref_closure`
/// exists to REPORT, so rewriting one would turn a named failure into a differently-dangling
/// reference that still fails, further from its cause.
#[test]
fn retarget_rewrites_only_the_authoring_prefix() {
    let mut body: serde_json::Value = serde_json::from_str(include_str!(
        "../../tests/json-schema-custom/unit/Custom.json"
    ))
    .unwrap();
    retarget_defs_references(&mut body, "/definitions");
    let properties = &body["properties"];
    assert_eq!(properties["alpha"]["$ref"], "#/definitions/Alpha");
    // Through an array, and at a depth the top-level walk does not reach directly.
    assert_eq!(
        properties["many"]["prefixItems"][0]["$ref"],
        "#/definitions/Alpha"
    );
    assert_eq!(
        properties["many"]["prefixItems"][1]["$ref"],
        "#/definitions/Beta"
    );
    // The deliberate non-rewrites, byte-identical to what the file holds.
    assert_eq!(properties["bare"]["$ref"], "PlutusData");
    assert_eq!(
        properties["remote"]["$ref"],
        "https://example.invalid/schema.json#/$defs/Alpha"
    );
    assert_eq!(
        properties["other_document"]["$ref"],
        "sibling.schema.json#/$defs/Alpha"
    );
    // Non-`$ref` content is untouched as well.
    assert_eq!(body["title"], "hand-authored");
}

/// Under the namespace the authoring convention already names, the retarget is the identity — which
/// is why a hand-authored file that never leaves the default generator behaves as if the retarget
/// were not there, and why adding it changed no existing document.
#[test]
fn retarget_is_the_identity_under_the_default_namespace() {
    let source = include_str!("../../tests/json-schema-custom/unit/Custom.json");
    let original: serde_json::Value = serde_json::from_str(source).unwrap();
    for spelling in ["/$defs", "#/$defs", "/$defs/", "#/$defs/"] {
        let mut body: serde_json::Value = serde_json::from_str(source).unwrap();
        retarget_defs_references(&mut body, spelling);
        assert_eq!(
            body, original,
            "the default namespace, spelled `{spelling}`, must rewrite nothing"
        );
    }
}

/// `custom_schema_body` end to end on the case the tool's own emitted generator can NEVER reach: a
/// document whose namespace is not `#/$defs`. That case is the entire justification for the retarget
/// existing, since without it a hand-authored file's references would be the one part of such a
/// document pointing at a namespace it does not have.
#[test]
fn custom_schema_body_retargets_onto_a_non_defs_namespace() {
    let generator = generator_with_definitions_path("/definitions");
    let schema = custom_schema_body(
        &generator,
        "Closed.json",
        include_str!("../../tests/json-schema-custom/unit/Closed.json"),
    );
    assert_eq!(
        schema.to_value()["properties"]["alpha"]["$ref"],
        "#/definitions/Alpha"
    );
}

/// The same body under a default generator, as the control: the returned schema IS the file's
/// document, so the helper adds nothing of its own beyond the retarget.
#[test]
fn custom_schema_body_returns_the_authored_document_unchanged_by_default() {
    let generator = schemars::SchemaGenerator::default();
    let source = include_str!("../../tests/json-schema-custom/unit/Custom.json");
    let schema = custom_schema_body(&generator, "Custom.json", source);
    assert_eq!(
        schema.to_value(),
        serde_json::from_str::<serde_json::Value>(source).unwrap()
    );
}

/// A file that is not JSON at all fails naming the file, not the byte offset alone — the author's
/// only handle on WHICH of their hand-authored files broke, since the expansion has no other
/// identity at runtime.
#[test]
#[should_panic(expected = "PlutusData.json is not valid JSON")]
fn custom_schema_body_rejects_a_file_that_is_not_json() {
    let generator = schemars::SchemaGenerator::default();
    custom_schema_body(&generator, "PlutusData.json", "{ not json");
}

/// Valid JSON that is not a SCHEMA. `schemars::Schema` is an object or a boolean and nothing else,
/// so a bare array or number is the reachable mistake here (a file holding the type's example VALUE
/// rather than its schema), and it gets the second of the two messages.
#[test]
#[should_panic(expected = "PlutusData.json is not a valid JSON schema")]
fn custom_schema_body_rejects_a_document_that_is_not_a_schema() {
    let generator = schemars::SchemaGenerator::default();
    custom_schema_body(&generator, "PlutusData.json", "[1, 2, 3]");
}

/// The two-argument arm publishes the TYPE TOKEN's own spelling, with no path qualifier or
/// surrounding module leaking in, and leaves the type referable.
#[test]
fn custom_schema_impl_derives_the_published_name_from_the_type_token() {
    assert_eq!(
        <Custom as schemars::JsonSchema>::schema_name().into_owned(),
        "Custom"
    );
    assert!(
        !<Custom as schemars::JsonSchema>::inline_schema(),
        "an inlined type is never a `$defs` entry, so one hand-authored file could not reference \
         another's"
    );
    // The `json_schema()` member the arm writes is `custom_schema_body` over the included file.
    let mut generator = schemars::SchemaGenerator::default();
    assert_eq!(
        <Custom as schemars::JsonSchema>::json_schema(&mut generator).to_value(),
        serde_json::from_str::<serde_json::Value>(include_str!(
            "../../tests/json-schema-custom/unit/Custom.json"
        ))
        .unwrap()
    );
}

/// The three-argument arm takes the name as an EXPRESSION, which is what lets a generic vary its
/// published name per instantiation — the only way an author can keep `schema_id()` (which defaults
/// to the name) distinct across instantiations, and therefore the only way out of the silent MERGE
/// `add_schema`'s ledger panics on.
#[test]
fn custom_schema_impl_takes_a_stated_name_for_a_type_that_is_not_an_ident() {
    assert_eq!(
        <Wrapped<u64> as schemars::JsonSchema>::schema_name().into_owned(),
        "WrappedU64"
    );
    assert!(!<Wrapped<u64> as schemars::JsonSchema>::inline_schema());
    let mut generator = schemars::SchemaGenerator::default();
    assert_eq!(
        <Wrapped<u64> as schemars::JsonSchema>::json_schema(&mut generator).to_value(),
        serde_json::json!({ "type": "integer", "format": "uint64", "minimum": 0 })
    );
}

/// The property the whole macro exists to deliver, asserted against a DOCUMENT built the way
/// `export_schemas()` builds one: a hand-authored type registered beside generated siblings lands in
/// `$defs` under its published name, its authored reference resolves to a sibling's entry, and the
/// document passes the closure check that runs before anything is written.
#[test]
fn a_macro_written_type_is_a_defs_entry_whose_references_close() {
    let mut generator = schemars::SchemaGenerator::default();
    {
        let mut reg = Registrar::new(&mut generator);
        reg.add::<Alpha>();
        reg.add::<Closed>();
        reg.add::<Wrapped<u64>>();
    }
    let definitions_path = generator.settings().definitions_path.to_string();
    let document = serde_json::json!({ "$defs": generator.take_definitions(true) });
    let defs = document["$defs"].as_object().unwrap();
    assert!(
        defs.contains_key("Closed") && defs.contains_key("WrappedU64"),
        "both arms' types must be published `$defs` entries; got {:?}",
        defs.keys().collect::<Vec<_>>()
    );
    assert_eq!(
        defs["Closed"]["properties"]["alpha"]["$ref"], "#/$defs/Alpha",
        "the authored reference must name the sibling's `$defs` entry"
    );
    check_schema_ref_closure(&document, &definitions_path);
}
