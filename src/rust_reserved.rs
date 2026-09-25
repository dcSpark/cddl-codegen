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
