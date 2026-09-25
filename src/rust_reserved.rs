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
