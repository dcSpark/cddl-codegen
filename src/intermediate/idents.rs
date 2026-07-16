use crate::{cli::Cli, rust_reserved::STD_TYPES, utils::is_identifier_reserved};

use super::{IntermediateTypes, RustType};

// to resolve ambiguities between raw (from CDDL) and already-formatted
// for things like type aliases, etc, we use these wrapper structs

// raw unchanged cddl identifier
#[derive(Clone, Debug)]
pub struct CDDLIdent(String);

impl CDDLIdent {
    pub fn new<T: Into<String>>(raw: T) -> Self {
        Self(raw.into())
    }
}

// impl<'a> From<&'a CDDLIdent> for &'a str {
//     fn from(ident: &'a CDDLIdent) -> &'a str {
//         &ident.0
//     }
// }
// why does this not compile?
// impl From<&CDDLIdent> for &str {
//     fn from(ident: &CDDLIdent) -> &str {
//         &ident.0
//     }
// }
// since it doesn't compile, using this for now:
impl std::fmt::Display for CDDLIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Why a token cannot become a [`RustIdent`] — see [`RustIdent::reserved_reason`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReservedIdentKind {
    RustTypeName,
    CddlKeyword,
}

// formatted code-generation identifier exactly as how it would be in the rust code
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct RustIdent(String);

impl RustIdent {
    /// Why `token` cannot become a `RustIdent`, or `None` if it can. The ONE owner of the
    /// reservation rule: `new` asserts on it (internal invariant — pipeline callers never feed
    /// reserved tokens), and callers that take idents from EXTERNAL text (the
    /// `--wrapper-requests` shape parser) pre-check with it so their input surfaces the
    /// feature's own hard error instead of the assert.
    pub fn reserved_reason(token: &str) -> Option<ReservedIdentKind> {
        if STD_TYPES.contains(&&super::convert_to_camel_case(token)[..]) {
            Some(ReservedIdentKind::RustTypeName)
        } else if token != "int" && is_identifier_reserved(token) {
            // int is special here since it refers to our own rust struct, not a primitive
            Some(ReservedIdentKind::CddlKeyword)
        } else {
            None
        }
    }

    // this should not be created directly, but instead via IntermediateTypes::new_type()
    // except for defining new cddl rules, since those should not be reserved identifiers
    pub fn new(cddl_ident: CDDLIdent) -> Self {
        // Message texts are recombination-sweep panic-class keys — keep them if refactoring.
        match Self::reserved_reason(&cddl_ident.0) {
            Some(ReservedIdentKind::RustTypeName) => {
                panic!("Cannot use reserved Rust type name: \"{}\"", cddl_ident.0)
            }
            Some(ReservedIdentKind::CddlKeyword) => {
                panic!("Cannot use reserved CDDL keyword: \"{}\"", cddl_ident.0)
            }
            None => {}
        }

        Self(super::convert_to_camel_case(&cddl_ident.0))
    }

    pub fn new_generic(
        generic_ident: &RustIdent,
        generic_args: &[RustType],
        types: &IntermediateTypes,
        cli: &Cli,
    ) -> Self {
        Self(format!(
            "{}<{}>",
            generic_ident,
            generic_args
                .iter()
                .map(|a| a.for_rust_member(types, false, cli))
                .collect::<Vec<String>>()
                .join(", ")
        ))
    }
}

impl std::fmt::Display for RustIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for RustIdent {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

// identifier for enum (group/type choice) variants
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum VariantIdent {
    // Custom is used for cases like primitives, 0-ary fields, etc that need custom names
    Custom(String),
    // whereas with RustStruct we can directly use a rust-defined type as a name
    RustStruct(RustIdent),
}

impl VariantIdent {
    pub fn new_custom<T: Into<String>>(name: T) -> Self {
        VariantIdent::Custom(name.into())
    }

    pub fn new_rust(ident: RustIdent) -> Self {
        VariantIdent::RustStruct(ident)
    }
}

impl std::fmt::Display for VariantIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariantIdent::Custom(name) => write!(f, "{name}"),
            VariantIdent::RustStruct(ident) => ident.fmt(f),
        }
    }
}

// identifier referring to a type alias
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum AliasIdent {
    // type definition defined in the cddl standard prelude
    Reserved(String),
    // user-made type alias
    Rust(RustIdent),
}

impl AliasIdent {
    pub fn new(ident: CDDLIdent) -> Self {
        if ident.0 == "int" || super::is_identifier_user_defined(&ident.0) {
            AliasIdent::Rust(RustIdent::new(ident))
        } else {
            AliasIdent::Reserved(ident.0)
        }
    }
}

impl From<RustIdent> for AliasIdent {
    fn from(ident: RustIdent) -> AliasIdent {
        AliasIdent::Rust(ident)
    }
}

impl std::fmt::Display for AliasIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AliasIdent::Reserved(name) => write!(f, "{name}"),
            AliasIdent::Rust(ident) => ident.fmt(f),
        }
    }
}
