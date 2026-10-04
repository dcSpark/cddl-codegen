use crate::intermediate::Representation;

// You can find all the types included in the Rust std lib below
// .rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/all.html
// However, we're particularly interested in avoiding collisions with the prelude
// So we only disallow types that can be found in the prelude
// https://doc.rust-lang.org/std/prelude/index.html

pub const STD_TYPES: &[&str] = &[
    "Copy",
    "Send",
    "Sized",
    "Sync",
    "Unpin",
    "Drop",
    "Fn",
    "FnMut",
    "FnOnce",
    "Box",
    "ToOwned",
    "Clone",
    "PartialEq",
    "PartialOrd",
    "Eq",
    "Ord",
    "AsRef",
    "AsMut",
    "Into",
    "From",
    "Default",
    "Iterator",
    "Extend",
    "IntoIterator",
    "DoubleEndedIterator",
    "ExactSizeIterator",
    "Self",
    "Option",
    "Some",
    "None",
    "Result",
    "Ok",
    "Err",
    "String",
    "ToString",
    "Vec",
    "TryFrom",
    "TryInto",
    "FromIterator",
];

/// Types and traits brought into generated modules by the runtime `error` and `serialization`
/// modules. A rule/group whose camel-cased name matches one collides with a runtime item, so
/// `intermediate::reserved_ident_rejection` refuses it. The set is uniform across profiles and
/// drift-guarded against the static sources by `runtime_types_match_static_sources`.
pub const RUNTIME_TYPES: &[&str] = &[
    "Key",
    "DeserializeFailure",
    "DeserializeError",
    "Deserialize",
    "CBORReadLen",
    "DeserializeEmbeddedGroup",
    "SerializeEmbeddedGroup",
    "ToCBORBytes",
    "Serialize",
    "LenEncoding",
    "StringEncoding",
    "TagPresenceEncoding",
    "RawBytesEncoding",
    "DepthGuard",
];

/// Types that generated modules import BY NAME whenever the spec uses the construct that needs them:
/// the cddl-codegen runtime carriers (`use …::ordered_hash_map::OrderedHashMap;`,
/// `use …::ordered_set::{BoundedOrderedSet, NonEmptyOrderedSet, OrderedSet};`, …, and the wasm/WIT
/// `AnyCbor` face) and the dependency-crate items (`use alloc::collections::BTreeMap;`,
/// `use cbor_event::se::Serializer;`, `use wasm_bindgen::prelude::{JsError, JsValue, …};`). A
/// rule/group of the same name collides with the import (E0255/E0252) or, in a file that reaches
/// user types only through `use super::*;`, silently resolves to the imported item. Like
/// `RUNTIME_TYPES` the set is uniform across profiles, so `intermediate::reserved_ident_rejection`
/// refuses the name even where a profile never imports it. Drift-guarded by
/// `imported_types_cover_generated_named_imports`.
pub const IMPORTED_TYPES: &[&str] = &[
    // cddl-codegen runtime carriers (`static/*.rs`)
    "OrderedHashMap",
    "NonEmptyVec",
    "BoundedVec",
    "BoundedMap",
    "NonEmptyMap",
    "OrderedSet",
    "NonEmptyOrderedSet",
    "BoundedOrderedSet",
    "PairMap",
    "NonEmptyPairMap",
    "BoundedPairMap",
    "AnyCbor",
    // dependency-crate items
    "BTreeMap",
    "Serializer",
    "Deserializer",
    "JsError",
    "JsValue",
];

/// Rust strict and reserved keywords (plus the 2024 additions `gen`/`try`). A struct field named by
/// any of these is invalid Rust; `parse_record_from_group_choice` rejects such fields with the
/// `@name` remedy rather than emitting source that only the rustfmt gate would catch.
pub(crate) const RUST_KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "dyn", "else", "enum", "extern", "false", "fn",
    "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
    "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe",
    "use", "where", "while", "async", "await", "try", "abstract", "become", "box", "do", "final",
    "macro", "override", "priv", "typeof", "unsized", "virtual", "yield", "gen",
];

/// Field names reserved because the generated `serialize`/`deserialize` bodies bind a FIXED local (or
/// take a parameter) by the same name, and the field's own local then collides with it — the crate
/// generates at exit 0 and does not compile, two build steps from the CDDL line that caused it.
///
/// **Evidence-based membership**: every entry breaks at least one shape × profile, measured by
/// generating the shape and `cargo check`ing the emitted rust crate; the second element records what
/// broke. Emitter locals that were swept and did NOT break anywhere stay OUT (refusing a name that
/// works is a gratuitous break) — they are listed in `GENERATED_LOCAL_PROBED_SAFE` instead, so a
/// newly-added emitter local joins the sweep rather than being guessed at.
///
/// Probe scope (2026-08-01, `b5b6283b`): five shapes — array-rep record, map-rep record, tagged
/// record (`#6.42([…])`), embedded plain group, group-choice arm — × three profiles — default,
/// `--preserve-encodings`, `--preserve-encodings --canonical-form` — × two field types (`bytes`,
/// `uint`), plus a `--wasm=true` pass over the same matrix (which surfaced no name the rust-only
/// pass had not). NOT probed: `--json-serde-derives`/`--json-schema-export`, `--component`, a field
/// whose type is a named rule/newtype, `.cbor`-payload and bounded-type member positions.
///
/// **Uniform across PROFILES, scoped by SHAPE.** No FLAG may rescue a refused field — a name that
/// breaks under `--preserve-encodings` is refused under the default profile too, because the spec
/// author does not choose their consumer's flags (`tag` and `len_encoding` compile by default and
/// break the moment `--preserve-encodings` is passed). But a name is only reserved where the emitter
/// actually BINDS the colliding local: the loop counter `read` exists only in a map-record
/// deserializer and the tag read only in a tagged type's, so refusing them in an array record would
/// break working specs for a collision that shape cannot have. The `tag: 0` group-choice
/// discriminant — this repo's own `tests/core` and `tests/preserve-encodings` fixtures, and an
/// idiom across real specs — is exactly such a spec, and it keeps generating. Each entry's
/// `ReservedScope` states where it applies; `generated_local_hazard_robustness_catalog` pins both
/// halves (refused inside the scope, accepted outside it).
///
/// LOCKSTEP with `identifier_hazard_tests::generated_local_registry_covers_emitter_locals`, which
/// re-derives the emitter-local vocabulary from the emitter sources and fails until a new local is
/// verdicted into this list or into `GENERATED_LOCAL_PROBED_SAFE`. The scope-wide compile probe
/// derives its complete spelling denominator from those two halves, so that verdict automatically
/// enters every legal named/newtype/`.cbor`/bounded-member and output-face cell.
pub(crate) const GENERATED_LOCAL_RESERVED: &[(&str, ReservedScope, &str)] = &[
    (
        "len",
        ReservedScope::Any,
        "the array/map length read (`let len = raw.array_sz()?`): array-rep, map-rep and tagged \
         records under every profile (E0308), plus the embedded-plain-group and group-choice-arm \
         shapes under `--preserve-encodings` (the minted `len_encoding` companion also collides \
         with the container's own — E0062/E0124/E0599)",
    ),
    (
        "len_encoding",
        ReservedScope::Any,
        "the container's own length-encoding local and `<Type>Encoding::len_encoding` field: all \
         five probed shapes under `--preserve-encodings` (E0308)",
    ),
    (
        "orig_deser_order",
        ReservedScope::MapRep,
        "the map-record deserializer's `let mut orig_deser_order = Vec::new()`: map-rep records \
         under `--preserve-encodings` (E0308/E0599); array records emit no such local",
    ),
    (
        "raw",
        ReservedScope::Any,
        "the deserializer parameter itself (`fn deserialize(raw: &mut Deserializer)`): array-rep, \
         map-rep and tagged records and embedded plain groups under every profile (E0599 — every \
         later field's read calls a `Deserializer` method on the shadowed binding)",
    ),
    (
        "read",
        ReservedScope::MapRep,
        "the map-record deserialize loop counter (`let mut read = 0`): map-rep records under every \
         profile (E0308/E0599); array records emit no such local",
    ),
    (
        "tag",
        ReservedScope::Tagged,
        "the tag read in a tagged type's deserializer (`let tag = raw.tag()?`): `#6.n(…)` records \
         under `--preserve-encodings` (E0062/E0124/E0308/E0599); an untagged record emits no tag \
         read, which is why the `tag: 0` group-choice discriminant is untouched",
    ),
    (
        "text_key",
        ReservedScope::MapRep,
        "the map-record unknown-key path's `let text_key`: map-rep records under \
         `--preserve-encodings` (E0308/E0599); array records emit no such local",
    ),
];

/// Where a `GENERATED_LOCAL_RESERVED` entry applies — the shapes whose emitted body binds the
/// colliding local. Never a PROFILE condition: see the registry's doc comment.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum ReservedScope {
    /// Every record's serialization body binds it.
    Any,
    /// Only a map-representation record's body.
    MapRep,
    /// Only a record that is the body of a tagged type (`#6.n([…])` / `#6.n({…})`).
    Tagged,
}

impl ReservedScope {
    pub(crate) fn applies(self, rep: Representation, tagged: bool) -> bool {
        match self {
            Self::Any => true,
            Self::MapRep => rep == Representation::Map,
            Self::Tagged => tagged,
        }
    }
}

/// Emitter locals that WERE swept (same matrix as `GENERATED_LOCAL_RESERVED`) and broke nothing, so
/// they are deliberately NOT reserved. This list exists for the LOCKSTEP test, not for the parser:
/// it is the "probed non-colliding" verdict that lets the test tell a name we have judged apart from
/// a name a new emitter just introduced.
#[cfg(test)]
#[allow(dead_code)]
pub(crate) const GENERATED_LOCAL_PROBED_SAFE: &[&str] = &[
    "_depth_guard",
    "_e",
    // The canonical profile's `force_canonical` parameter, renamed where the serialize body never
    // forwards it (`make_serialization_function_over`): such a body writes only a width-less
    // special and references no field, so no field name can collide with it.
    "_force_canonical",
    "_k",
    "_rest_elem",
    "_rest_key",
    "_rest_value",
    "buf",
    "byte",
    "bytes",
    // THE EXACT-ZERO OPEN-REST INSERTION FAMILY — `candidate` and `entry_key`. Both are bound only
    // inside the native `insert_<rest>` method emitted for a record with a forbidden fixed key and
    // a captured map row. Swept 2026-08-15 over ordinary fields named for each local in the five
    // registry shapes × default / preserve / canonical, plus exact-zero open records whose fixed
    // field and captured-row `@name` each use the name under the same three profiles and wasm on/off:
    // every generated crate compiled. The insertion body reaches user-controlled fields only as
    // `self.<field>`; `entry_key` is its parameter and `candidate` is a detached clone, so neither
    // can shadow a bare field binding.
    "candidate",
    // THE OPEN-TABLE JSON FAMILY — `captured_range`, `ks`, `out`, `seen`, `typed_range` (the other
    // four point back here). All five are bound ONLY by `emit_open_table_json`, i.e. only in an open
    // table's hand-written `Serialize`/`Deserialize`/`JsonSchema`, and only under a json flag.
    //
    // Swept 2026-08-02 over the five registry shapes — array-rep record, map-rep record, tagged
    // record (`#6.4n([…])`), embedded plain group, group-choice arm — plus a `bytes`-typed map-rep
    // shape, × default / `--preserve-encodings` / `--preserve-encodings --canonical-form` × json
    // flags OFF and ON (the profile axis the registry's original sweep left unprobed, and the one
    // these locals actually live in), plus the OPEN TABLE shape itself with each name `@name`d onto
    // the typed row and onto the catch-all row, × the same three profiles × json off/on. 42 bundled
    // crates, each name in its own rule, every one `cargo check` clean.
    //
    // Two structural reasons behind that result, both worth stating because they bound what a future
    // emitter change could break. An open table has ZERO fixed fields, so the only user-controlled
    // name reaching these bodies is a ROW's (`@name`) — and every reference to a row's field is
    // qualified (`self.<row>` on the write side, `out.<row>` on the read side), so a local can never
    // shadow one: the swept crates really do emit `out.seen.insert(…)` beside `let mut seen`, and
    // `self.ks.iter()` beside `while let Some(ks)`. `typed_range`/`captured_range` are bound inside
    // the `json_schema` body, which references no field at all.
    "captured_range",
    "deser_order",
    "deser_variant",
    "deserializer",
    "e",
    "elem",
    "element",
    "elements",
    "encs",
    // Exact-zero open-rest insertion local; verdict + sweep evidence at `candidate` above.
    "entry_key",
    "errs",
    "f",
    "field",
    "field_index",
    "first",
    "first_key",
    "first_value",
    "force_canonical",
    "generator",
    // The open table's `deser_order` fallback closure parameter (`(0..self.<row>.len()).map(|i| 2 *
    // i)`) and the canonical merge's enumerate binding. Swept 2026-08-01 over the array-rep /
    // map-rep / tagged-record / embedded-plain-group / group-choice-arm / open-struct-map /
    // open-array / optional-field shapes × default / `--preserve-encodings` /
    // `--preserve-encodings --canonical-form` × wasm off and on: every crate compiles. The closure
    // body references no field, so a field named `i` (reached as `self.i`) cannot be shadowed by it.
    "i",
    "index",
    "initial_position",
    "inner",
    "k",
    "key",
    "key_order",
    // Open-table JSON local; verdict + sweep evidence at `captured_range` above.
    "ks",
    "list",
    "map",
    "native",
    "opt",
    // Open-table JSON local; verdict + sweep evidence at `captured_range` above.
    "out",
    "pairs",
    "present",
    "read_len",
    "rest_entries",
    "rest_i",
    "rest_key",
    "rest_value",
    "ret",
    "s",
    // Open-table JSON local; verdict + sweep evidence at `captured_range` above.
    "seen",
    "serializer",
    "special",
    "string",
    "tag_sz",
    "ty",
    // Open-table JSON local; verdict + sweep evidence at `captured_range` above.
    "typed_range",
    "unknown_key",
    "v",
    "value",
    "variant_deser",
    "wrapper",
    "x",
];

/// A graceful-rejection message if `field_name` (already RESOLVED — post-`@name`, post-snake_case)
/// is a reserved generated-local name, else `None`. The message names the field, the rule and the
/// reserved word, and points at the `; @name <other>` remedy, which renames the Rust field WITHOUT
/// touching the CBOR wire key (probed: a bareword key `rawx` with `; @name payload` emits
/// `serializer.write_text("rawx")` and a `payload` struct field). Array-rep keys never reach the
/// wire at all, so `@name` is unconditionally safe there.
pub(crate) fn generated_local_field_rejection(
    field_name: &str,
    source_name: &str,
    rep: Representation,
    tagged: bool,
) -> Option<String> {
    let (word, _, evidence) = GENERATED_LOCAL_RESERVED
        .iter()
        .find(|(word, scope, _)| *word == field_name && scope.applies(rep, tagged))?;
    Some(format!(
        "rule `{source_name}`: field `{field_name}` is a reserved name — the generated \
         serialization code binds its own `{word}`: {evidence}. The field's local would shadow it \
         and the emitted crate would not compile. Rename the field with a `; @name <other>` comment \
         directive on that entry — the CBOR wire key is unchanged (a bareword/text key stays the \
         same text; array positions never put the name on the wire)."
    ))
}
