//! The guest crate's glue: the `wit_bindgen::generate!` call site and the per-type `Guest` /
//! `Guest<Resource>` impls that bridge the WIT surface to the generated rust crate.
//!
//! # Contract
//!
//! This module CONSUMES [`super::wit::WitPackage`] and never re-derives a name from the IR. The
//! projection already decided every WIT name, every ownership position and every rust-side operation;
//! a second derivation here would drift silently from both the emitted `.wit` and the rust↔WIT parity
//! gate. What it does read from the IR is STRUCTURE the projection deliberately does not carry — is
//! this ident a record or a `@newtype` wrapper, does its rust `new` return a `Result` — which is a
//! question about the rust crate, not about the WIT.
//!
//! # The two load-bearing emission invariants
//!
//! **Re-entrancy.** No emitted body ever holds two `RefCell` guards at once. Every parameter is
//! materialized to an owned rust value — cloned through its borrow, guard dropped at the end of that
//! statement — BEFORE any `borrow_mut` of `self`. This is not style: the canonical ABI lets a caller
//! pass the same handle as both receiver and argument (`x.set-children([x])`), collection-mediated
//! recursion makes that type-legal for any self-referential CDDL type, and the two-guard form
//! compiles clean in debug AND release while trapping at runtime on exactly that call — and a trap
//! poisons the whole component instance, so in a composed topology one aliased call kills a shared
//! dependency component for every consumer.
//!
//! **Clone-at-boundary getters.** A getter mints a FRESH owned handle over a CLONE of the field: a
//! snapshot, never an alias into the parent. A handle that aliased the parent's interior would let a
//! caller mutate a field it never asked for, and would reintroduce the two-guard shape through the
//! back door.
//!
//! # One file, not one file per scope
//!
//! Everything lands in `component/src/generated/mod.rs`, unlike the rust and wasm faces which mirror
//! the spec's module scopes on disk. Three reasons, and none of them is convenience: the component
//! crate is a `cdylib` whose entire surface is the WIT world, so its rust module structure is visible
//! to nobody; `wit_bindgen::generate!` mints ONE type tree at ONE invocation site, so a split would
//! buy cross-module paths back to that site for no gain; and `export!` must be invoked in the module
//! that expanded `generate!`. The per-scope map is plumbed anyway (`GenerationScope::component_scopes`)
//! so a later phase can split without touching the write loop.

use super::wit::{
    WitConstructor, WitEnum, WitFunc, WitFuncOp, WitInterface, WitMember, WitMemberOp, WitPackage,
    WitParam, WitResource, WitType, WitTypeDef, WitTypeRef,
};
use crate::cli::Cli;
use crate::intermediate::{IntermediateTypes, ModuleScope, RustIdent, RustStructType};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;

/// The reserved prelude extern carrying the full CBOR integer range. Spelled the way the other
/// sites that special-case it are (see [`super::wit`]'s own const) so one grep finds them all.
const INT_EXTERN_IDENT: &str = "Int";

/// Rust keywords `wit_bindgen` escapes with a trailing `_` when it lowers a WIT name to a rust
/// identifier. The emitted glue has to spell the SAME escape or the trait method it is implementing
/// does not exist — so this list mirrors `wit-bindgen-rust`'s `to_rust_ident`, not rust's full
/// reserved set (the two agree on everything a WIT identifier can spell: a WIT name is lower-kebab,
/// so `Self`, `crate` and the raw-identifier forms are unreachable).
const RUST_KEYWORDS: &[&str] = &[
    "as", "async", "await", "become", "box", "break", "const", "continue", "do", "dyn", "else",
    "enum", "extern", "false", "final", "fn", "for", "if", "impl", "in", "let", "loop", "macro",
    "match", "mod", "move", "mut", "override", "priv", "pub", "ref", "return", "self", "static",
    "struct", "super", "trait", "true", "try", "type", "typeof", "unsafe", "unsized", "use",
    "virtual", "where", "while", "yield",
];

/// The whole guest crate's generated glue, as one rust file's worth of source.
///
/// Infallible for the same reason the projection is: anything phase 1 cannot render was already
/// EXCLUDED AND RECORDED upstream, so what arrives here is by construction emittable.
pub(crate) fn component_glue(
    types: &IntermediateTypes,
    cli: &Cli,
    no_deserialize: &BTreeSet<RustIdent>,
) -> String {
    let package = super::wit::project(types, cli, no_deserialize);
    Emitter {
        types,
        cli,
        aliases: package
            .interfaces
            .iter()
            .map(|(scope, iface)| (scope.clone(), interface_alias(&iface.name)))
            .collect(),
        package: &package,
    }
    .emit()
}

/// The rust module alias one WIT interface is reached through.
///
/// Aliased rather than glob-imported because a WIT package's interfaces share no type namespace: two
/// interfaces may each define a `foo`, and `use exports::…::a::*; use exports::…::b::*;` would then
/// be an E0659 ambiguity at a name neither the spec author nor this emitter chose. The alias is a
/// pure function of the interface name, which the package-level collision detector already proved
/// unique.
fn interface_alias(iface_name: &str) -> String {
    format!("wit_{}", kebab_to_snake(iface_name))
}

/// A WIT identifier in the rust form `wit_bindgen` gives it for a value position (function, module,
/// parameter): words joined with `_`, keywords suffixed.
fn kebab_to_snake(name: &str) -> String {
    let snake = name.replace('-', "_");
    if RUST_KEYWORDS.contains(&snake.as_str()) {
        format!("{snake}_")
    } else {
        snake
    }
}

/// A WIT identifier in the rust form `wit_bindgen` gives it for a TYPE position: each word's first
/// letter uppercased, joined with nothing. (`any-cbor-kind` → `AnyCborKind`, `i0` → `I0`.)
fn kebab_to_camel(name: &str) -> String {
    name.split('-')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first) => first.to_ascii_uppercase().to_string() + chars.as_str(),
                None => String::new(),
            }
        })
        .collect()
}

/// The guest REP struct wrapping one generated rust type. Keyed by the RUST ident rather than the
/// WIT name because rust idents are unique across the whole IR while a WIT type name is unique only
/// within its interface — two scopes may each define a `foo`, and both reps live in this one file.
fn rep_name(ident: &RustIdent) -> String {
    format!("Wit{ident}")
}

/// A conversion expression plus whether it evaluates to a `Result` the caller must unwrap.
///
/// Fallibility is carried rather than baked in as a `?` because the same conversion is emitted both
/// at statement level (where `?` is legal) and inside a `map` closure (where it is not).
struct Conv {
    expr: String,
    fallible: bool,
}

impl Conv {
    fn plain(expr: impl Into<String>) -> Self {
        Self {
            expr: expr.into(),
            fallible: false,
        }
    }

    /// The expression as a value, unwrapping in place. Only valid at statement level.
    fn unwrapped(&self) -> String {
        if self.fallible {
            format!("{}?", self.expr)
        } else {
            self.expr.clone()
        }
    }
}

struct Emitter<'a, 'b> {
    types: &'a IntermediateTypes<'b>,
    cli: &'a Cli,
    package: &'a WitPackage,
    /// Defining scope → the rust module alias its interface is reached through. Precomputed so a
    /// cross-interface reference resolves without a second walk: `wit_bindgen` defines a `use`d type
    /// once, in the interface that DECLARES it, and every other interface's spelling is an alias of
    /// that one type.
    aliases: BTreeMap<ModuleScope, String>,
}

impl Emitter<'_, '_> {
    // ---------------------------------------------------------------------------------------------
    // Paths into the two crates the glue straddles
    // ---------------------------------------------------------------------------------------------

    /// The generated rust crate's path prefix for the shared runtime modules (`serialization`,
    /// `any_cbor`).
    fn runtime(&self) -> String {
        self.cli.common_import_component()
    }

    /// The fully-qualified path of a generated rust type, as the component crate spells it.
    fn rust_path(&self, ident: &RustIdent) -> String {
        super::rust_crate_struct_from_wasm(self.types, ident, self.cli)
    }

    /// The trait carrying `to_cbor_bytes` in this flag posture. Under
    /// `--preserve-encodings --canonical-form` the blanket `ToCBORBytes` impl is not composed into the
    /// runtime at all and the method lives on `Serialize` instead — the same fork the wasm face makes,
    /// spelled the same way so the two cannot drift apart silently.
    fn to_bytes_trait(&self) -> &'static str {
        if self.cli.preserve_encodings && self.cli.canonical_form {
            "Serialize"
        } else {
            "ToCBORBytes"
        }
    }

    /// The rust crate's `Int`, found by scanning the IR for the reserved prelude extern rather than
    /// by constructing its ident, so the path picks up whatever scope the run actually put it in.
    fn int_path(&self) -> String {
        self.types
            .rust_structs()
            .keys()
            .find(|ident| ident.as_ref() == INT_EXTERN_IDENT)
            .map(|ident| self.rust_path(ident))
            .unwrap_or_else(|| format!("{}::{INT_EXTERN_IDENT}", self.cli.lib_name_code()))
    }

    /// Whether the generated rust crate's own `new` for this ident returns a `Result`.
    ///
    /// The rust face decides this in TWO places, and a glue that consulted only one emits
    /// `let inner = Rec::new(..);` for a `Result` — a type error in generated code that no WIT gate
    /// can see. A `@newtype` wrapper's bound rides the IR (`can_new_fail`, marked at finalization);
    /// a RECORD's is derived per emission from its mandatory fields, and
    /// `emit_tests::record_ctor_can_fail` is the already-shared mirror of `records.rs`'s own
    /// `new_can_fail`, so this reads it rather than minting a third copy of the rule.
    fn rust_new_can_fail(&self, ident: &RustIdent) -> bool {
        if self.types.can_new_fail(ident) {
            return true;
        }
        match self.types.rust_struct(ident).map(|s| s.variant()) {
            Some(RustStructType::Record(record)) => crate::emit_tests::record_ctor_can_fail(record),
            _ => false,
        }
    }

    fn alias_for(&self, r: &WitTypeRef) -> &str {
        self.aliases
            .get(&r.scope)
            .map(String::as_str)
            .expect("every projected type's scope has an interface")
    }

    // ---------------------------------------------------------------------------------------------
    // The WIT-side rust types `generate!` mints
    // ---------------------------------------------------------------------------------------------

    /// One WIT type at a use site, in the rust spelling `wit_bindgen` gives it. `param` selects the
    /// OWNERSHIP: a composite in argument position arrives as `<T>Borrow<'_>`, and in return position
    /// as the owned handle. `iface` is the alias of the interface being emitted, needed for the
    /// per-interface synthesized types (`int`, `any-cbor-kind`), which are DISTINCT rust types in
    /// every interface that declares them.
    fn wit_rust_type(&self, ty: &WitType, iface: &str, param: bool) -> String {
        match ty {
            WitType::Bool => "bool".to_owned(),
            WitType::U8 => "u8".to_owned(),
            WitType::U16 => "u16".to_owned(),
            WitType::U32 => "u32".to_owned(),
            WitType::U64 => "u64".to_owned(),
            WitType::S8 => "i8".to_owned(),
            WitType::S16 => "i16".to_owned(),
            WitType::S32 => "i32".to_owned(),
            WitType::S64 => "i64".to_owned(),
            WitType::F32 => "f32".to_owned(),
            WitType::F64 => "f64".to_owned(),
            WitType::Str => "String".to_owned(),
            WitType::List(inner) => format!("Vec<{}>", self.wit_rust_type(inner, iface, param)),
            WitType::Tuple(inner) => {
                let parts: Vec<String> = inner
                    .iter()
                    .map(|t| self.wit_rust_type(t, iface, param))
                    .collect();
                if parts.len() == 1 {
                    format!("({},)", parts[0])
                } else {
                    format!("({})", parts.join(", "))
                }
            }
            WitType::Option(inner) => {
                format!("Option<{}>", self.wit_rust_type(inner, iface, param))
            }
            WitType::Handle(r) => {
                let camel = kebab_to_camel(&r.name);
                if param {
                    format!("{}::{camel}Borrow<'_>", self.alias_for(r))
                } else {
                    format!("{}::{camel}", self.alias_for(r))
                }
            }
            WitType::Enum(r) => format!("{}::{}", self.alias_for(r), kebab_to_camel(&r.name)),
            WitType::Int => format!("{iface}::Int"),
            // A transparent alias over raw CBOR item bytes: the same `Vec<u8>` on both sides of the
            // macro, so no alias spelling is needed and none is emitted.
            WitType::AnyCbor => "Vec<u8>".to_owned(),
            WitType::AnyCborKind => format!("{iface}::AnyCborKind"),
        }
    }

    // ---------------------------------------------------------------------------------------------
    // WIT value -> rust value (parameter direction)
    // ---------------------------------------------------------------------------------------------

    /// Convert an owned WIT-side value into the rust crate's own type.
    ///
    /// Every composite arm CLONES THROUGH the borrow and drops its guard inside the expression it is
    /// generated into — that is the mechanical form of the re-entrancy invariant, and it is why a
    /// handle is never bound to a local `Ref`.
    fn wit_to_rust(&self, ty: &WitType, expr: &str, iface: &str) -> Conv {
        match ty {
            WitType::Bool
            | WitType::U8
            | WitType::U16
            | WitType::U32
            | WitType::U64
            | WitType::S8
            | WitType::S16
            | WitType::S32
            | WitType::S64
            | WitType::F32
            | WitType::F64
            | WitType::Str => Conv::plain(expr),
            WitType::Handle(r) => Conv::plain(format!(
                "{expr}.get::<{}>().0.borrow().clone()",
                rep_name(&r.ident)
            )),
            WitType::Enum(r) => Conv::plain(format!(
                "{}_from_wit({expr})",
                kebab_to_snake(&convert_ident_to_snake(&r.ident))
            )),
            WitType::Int => Conv::plain(format!("int_from_wit_{}({expr})", bridge_suffix(iface))),
            WitType::AnyCbor => Conv {
                expr: format!(
                    "<{}::any_cbor::AnyCbor as {}::serialization::Deserialize>::from_cbor_bytes(&{expr}).map_err(err)",
                    self.runtime(),
                    self.runtime()
                ),
                fallible: true,
            },
            // Never a parameter: the discriminant enum is a return type only.
            WitType::AnyCborKind => Conv::plain(expr),
            WitType::Option(inner) => {
                let inner_conv = self.wit_to_rust(inner, "v", iface);
                if inner_conv.expr == "v" {
                    Conv::plain(expr)
                } else if inner_conv.fallible {
                    Conv {
                        expr: format!("{expr}.map(|v| {}).transpose()", inner_conv.expr),
                        fallible: true,
                    }
                } else {
                    Conv::plain(format!("{expr}.map(|v| {})", inner_conv.expr))
                }
            }
            WitType::Tuple(inner) => {
                let names: Vec<String> = (0..inner.len()).map(|i| format!("t{i}")).collect();
                let convs: Vec<Conv> = inner
                    .iter()
                    .zip(&names)
                    .map(|(t, n)| self.wit_to_rust(t, n, iface))
                    .collect();
                let fallible = convs.iter().any(|c| c.fallible);
                let parts: Vec<String> = convs.iter().map(|c| c.unwrapped()).collect();
                let tuple = if parts.len() == 1 {
                    format!("({},)", parts[0])
                } else {
                    format!("({})", parts.join(", "))
                };
                let destructure = if names.len() == 1 {
                    format!("({},)", names[0])
                } else {
                    format!("({})", names.join(", "))
                };
                if fallible {
                    Conv {
                        expr: format!(
                            "(|| -> Result<_, String> {{ let {destructure} = {expr}; Ok({tuple}) }})()"
                        ),
                        fallible: true,
                    }
                } else if tuple == destructure {
                    // Every element crosses unchanged, so the destructure-and-rebuild would be a
                    // no-op the reader has to check before believing.
                    Conv::plain(expr)
                } else {
                    Conv::plain(format!("{{ let {destructure} = {expr}; {tuple} }}"))
                }
            }
            WitType::List(inner) => {
                // A list's TARGET rust type is whatever the consuming position wants — `Vec`,
                // `BTreeMap`, `OrderedHashMap`, `PairMap`. `collect()` reaches all of them through
                // `FromIterator` and lets inference pick, which is why the element conversion is
                // written and the container is not.
                let (pattern, element) = match &**inner {
                    // A table row arrives as a tuple, and destructuring it in the closure head reads
                    // better than a nested block — and matches the shape both `Vec<(K, V)>` and a
                    // map's `iter()` present.
                    WitType::Tuple(parts) => {
                        let names: Vec<String> =
                            (0..parts.len()).map(|i| format!("x{i}")).collect();
                        let convs: Vec<Conv> = parts
                            .iter()
                            .zip(&names)
                            .map(|(t, n)| self.wit_to_rust(t, n, iface))
                            .collect();
                        let fallible = convs.iter().any(|c| c.fallible);
                        let rendered: Vec<String> = convs.iter().map(|c| c.unwrapped()).collect();
                        let head = if names.len() == 1 {
                            format!("({},)", names[0])
                        } else {
                            format!("({})", names.join(", "))
                        };
                        let body = if rendered.len() == 1 {
                            format!("({},)", rendered[0])
                        } else {
                            format!("({})", rendered.join(", "))
                        };
                        (
                            head,
                            Conv {
                                expr: body,
                                fallible,
                            },
                        )
                    }
                    other => ("x".to_owned(), self.wit_to_rust(other, "x", iface)),
                };
                // An element that crosses unchanged still has to be re-`collect`ed: the TARGET is
                // whatever container the consuming position wants, which is rarely the `Vec` the WIT
                // side hands over. Only the per-element `map` is dropped.
                if element.expr == pattern {
                    return Conv::plain(format!("{expr}.into_iter().collect()"));
                }
                if element.fallible {
                    Conv {
                        expr: format!(
                            "{expr}.into_iter().map(|{pattern}| Ok({})).collect::<Result<Vec<_>, String>>()",
                            element.expr
                        ),
                        fallible: true,
                    }
                } else {
                    Conv::plain(format!(
                        "{expr}.into_iter().map(|{pattern}| {}).collect()",
                        element.expr
                    ))
                }
            }
        }
    }

    // ---------------------------------------------------------------------------------------------
    // rust value -> WIT value (return direction)
    // ---------------------------------------------------------------------------------------------

    /// Convert a rust-crate value into its WIT-side spelling. `by_ref` says whether `expr` is a
    /// reference (inside a closure over an `iter()`) or a place expression (a field read).
    ///
    /// Every arm that carries a composite CLONES: a handle minted here wraps a fresh `RefCell` over a
    /// copy of the field, so it is a snapshot the caller may mutate freely without reaching back into
    /// the parent.
    fn rust_to_wit(&self, ty: &WitType, expr: &str, iface: &str, by_ref: bool) -> String {
        let deref = |e: &str| {
            if by_ref {
                format!("*{e}")
            } else {
                e.to_owned()
            }
        };
        let by_reference = |e: &str| {
            if by_ref {
                e.to_owned()
            } else {
                format!("&{e}")
            }
        };
        match ty {
            WitType::Bool
            | WitType::U8
            | WitType::U16
            | WitType::U32
            | WitType::U64
            | WitType::S8
            | WitType::S16
            | WitType::S32
            | WitType::S64
            | WitType::F32
            | WitType::F64 => deref(expr),
            WitType::Str => format!("{expr}.clone()"),
            WitType::Handle(r) => format!(
                "{}::{}::new({}(RefCell::new({expr}.clone())))",
                self.alias_for(r),
                kebab_to_camel(&r.name),
                rep_name(&r.ident)
            ),
            WitType::Enum(r) => format!(
                "{}_to_wit({})",
                kebab_to_snake(&convert_ident_to_snake(&r.ident)),
                by_reference(expr)
            ),
            WitType::Int => format!(
                "int_to_wit_{}({})",
                bridge_suffix(iface),
                by_reference(expr)
            ),
            WitType::AnyCbor => format!(
                "<{}::any_cbor::AnyCbor as {}::serialization::{}>::to_cbor_bytes({})",
                self.runtime(),
                self.runtime(),
                self.to_bytes_trait(),
                by_reference(expr)
            ),
            WitType::AnyCborKind => {
                format!(
                    "any_cbor_kind_to_wit_{}({})",
                    bridge_suffix(iface),
                    by_reference(expr)
                )
            }
            WitType::Option(inner) => format!(
                "{expr}.as_ref().map(|x| {})",
                self.rust_to_wit(inner, "x", iface, true)
            ),
            WitType::Tuple(inner) => {
                let names: Vec<String> = (0..inner.len()).map(|i| format!("t{i}")).collect();
                let parts: Vec<String> = inner
                    .iter()
                    .zip(&names)
                    .map(|(t, n)| self.rust_to_wit(t, n, iface, true))
                    .collect();
                let head = if names.len() == 1 {
                    format!("({},)", names[0])
                } else {
                    format!("({})", names.join(", "))
                };
                let body = if parts.len() == 1 {
                    format!("({},)", parts[0])
                } else {
                    format!("({})", parts.join(", "))
                };
                format!("{{ let {head} = {}; {body} }}", by_reference(expr))
            }
            WitType::List(inner) => {
                // `iter()` rather than a container-specific door: every rust collection this face can
                // reach (`Vec`, `NonEmptyVec`, `BTreeMap`, `OrderedHashMap`, `OrderedSet`, `PairMap`)
                // has one, and the `|(k, v)|` head below destructures both a `&(K, V)` element and a
                // map's `(&K, &V)` pair identically under default binding modes.
                match &**inner {
                    WitType::Tuple(parts) => {
                        let names: Vec<String> =
                            (0..parts.len()).map(|i| format!("x{i}")).collect();
                        let rendered: Vec<String> = parts
                            .iter()
                            .zip(&names)
                            .map(|(t, n)| self.rust_to_wit(t, n, iface, true))
                            .collect();
                        let head = if names.len() == 1 {
                            format!("({},)", names[0])
                        } else {
                            format!("({})", names.join(", "))
                        };
                        let body = if rendered.len() == 1 {
                            format!("({},)", rendered[0])
                        } else {
                            format!("({})", rendered.join(", "))
                        };
                        format!("{expr}.iter().map(|{head}| {body}).collect()")
                    }
                    other => format!(
                        "{expr}.iter().map(|x| {}).collect()",
                        self.rust_to_wit(other, "x", iface, true)
                    ),
                }
            }
        }
    }

    // ---------------------------------------------------------------------------------------------
    // Emission
    // ---------------------------------------------------------------------------------------------

    fn emit(&self) -> String {
        let mut out = String::new();
        let has_resource = self.package.interfaces.values().any(|iface| {
            iface
                .types
                .iter()
                .any(|def| matches!(def, WitTypeDef::Resource(_)))
        });
        if has_resource {
            out.push_str("use core::cell::RefCell;\n\n");
        }
        // `path` is resolved against CARGO_MANIFEST_DIR, i.e. the component crate root — NOT against
        // the file holding the macro. So the literal is the bare `wit` tail even though this file
        // sits two directories below it.
        let _ = write!(
            out,
            "wit_bindgen::generate!({{\n    path: \"{}\",\n    world: \"{}\",\n}});\n\n",
            wit_dir_tail(),
            self.package.world
        );
        for (scope, iface) in &self.package.interfaces {
            let _ = writeln!(
                out,
                "use exports::{}::{}::{} as {};",
                kebab_to_snake(&self.package.id.namespace),
                kebab_to_snake(&self.package.id.name),
                kebab_to_snake(&iface.name),
                self.aliases
                    .get(scope)
                    .expect("every interface got an alias")
            );
        }
        out.push('\n');

        // ONE `Component` implementing every interface's `Guest` trait, covered by ONE `export!`.
        // That is the shape the component model wants: a world's exports are implemented by a single
        // guest type, and a second `export!` would emit a second set of canonical-ABI symbols.
        out.push_str("struct Component;\n\n");
        for iface in self.package.interfaces.values() {
            out.push_str(&self.emit_guest_impl(iface));
        }
        out.push_str("export!(Component);\n\n");

        out.push_str(
            "/// Every fallible door on this face reports through the rust crate's own error \
             `Display`,\n/// so a WIT `result<_, string>` carries the same text a native caller \
             would see.\nfn err<E: core::fmt::Display>(e: E) -> String {\n    e.to_string()\n}\n\n",
        );

        for iface in self.package.interfaces.values() {
            let alias = self
                .aliases
                .get(&iface.scope)
                .expect("every interface got an alias");
            for def in &iface.types {
                match def {
                    WitTypeDef::Enum(e) => out.push_str(&self.emit_enum_bridges(e, alias)),
                    WitTypeDef::IntVariant => out.push_str(&self.emit_int_bridges(alias)),
                    WitTypeDef::AnyCborKind => out.push_str(&self.emit_any_cbor_kind_bridge(alias)),
                    WitTypeDef::AnyCborAlias | WitTypeDef::Resource(_) => {}
                }
            }
        }

        for iface in self.package.interfaces.values() {
            for def in &iface.types {
                if let WitTypeDef::Resource(resource) = def {
                    out.push_str(&self.emit_resource(resource, iface));
                }
            }
        }
        out
    }

    /// The interface-level `Guest` trait: the resource associated types plus the interface's free
    /// functions, which land HERE and on no `Guest<Resource>` trait.
    fn emit_guest_impl(&self, iface: &WitInterface) -> String {
        let alias = self
            .aliases
            .get(&iface.scope)
            .expect("every interface got an alias");
        let mut out = format!("impl {alias}::Guest for Component {{\n");
        for def in &iface.types {
            if let WitTypeDef::Resource(resource) = def {
                let _ = writeln!(
                    out,
                    "    type {} = {};",
                    kebab_to_camel(&resource.name),
                    rep_name(&resource.ident)
                );
            }
        }
        for func in &iface.funcs {
            out.push_str(&self.emit_free_func(func, alias));
        }
        out.push_str("}\n\n");
        out
    }

    fn emit_free_func(&self, func: &WitFunc, alias: &str) -> String {
        let params: Vec<String> = func
            .params
            .iter()
            .map(|p| {
                format!(
                    "{}: {}",
                    kebab_to_snake(&p.name),
                    self.wit_rust_type(&p.ty, alias, true)
                )
            })
            .collect();
        let ret = self.signature_return(func.result.as_ref(), func.fallible, alias);
        let mut out = format!(
            "\n    fn {}({}){} {{\n",
            kebab_to_snake(&func.name),
            params.join(", "),
            ret
        );
        match func.op {
            // `any-cbor` is a TRANSPARENT alias, so the argument is arbitrary caller bytes carrying
            // no validity invariant: the decode is the check, and an invalid item leaves through the
            // error string rather than as a synthetic enum case with no rust counterpart.
            WitFuncOp::AnyCborKind => {
                let arg = kebab_to_snake(&func.params[0].name);
                let _ = writeln!(
                    out,
                    "        <{rt}::any_cbor::AnyCbor as {rt}::serialization::Deserialize>::from_cbor_bytes(&{arg})\n            \
                     .map(|v| any_cbor_kind_to_wit_{suffix}(&v.kind()))\n            .map_err(err)",
                    rt = self.runtime(),
                    suffix = bridge_suffix(alias)
                );
            }
        }
        out.push_str("    }\n");
        out
    }

    /// The ` -> …` tail of an emitted signature. A WIT `result<_, string>` is `Result<(), String>`
    /// in rust, and a `func()` returning nothing has no tail at all.
    fn signature_return(&self, ok: Option<&WitType>, fallible: bool, iface: &str) -> String {
        let ok = ok.map(|ty| self.wit_rust_type(ty, iface, false));
        match (ok, fallible) {
            (Some(ok), true) => format!(" -> Result<{ok}, String>"),
            (Some(ok), false) => format!(" -> {ok}"),
            (None, true) => " -> Result<(), String>".to_owned(),
            (None, false) => String::new(),
        }
    }

    /// The two-direction bridge for a c-style enum.
    ///
    /// A pair of explicit `match`es rather than a cast or a re-export: the WIT enum's rust type is
    /// MINTED BY `generate!` and is a genuinely different type from the crate's own enum, so the wasm
    /// face's `pub use` trick has no counterpart here. Each case's rust variant comes from the
    /// projection, so the pairing is never re-derived from a name.
    fn emit_enum_bridges(&self, e: &WitEnum, alias: &str) -> String {
        let rust = self.rust_path(&e.ident);
        let wit = format!("{alias}::{}", kebab_to_camel(&e.name));
        let base = kebab_to_snake(&convert_ident_to_snake(&e.ident));
        let mut out = format!("fn {base}_to_wit(v: &{rust}) -> {wit} {{\n    match v {{\n");
        for case in &e.cases {
            let _ = writeln!(
                out,
                "        {rust}::{} => {wit}::{},",
                case.rust_variant,
                kebab_to_camel(&case.name)
            );
        }
        out.push_str("    }\n}\n\n");
        let _ = write!(
            out,
            "fn {base}_from_wit(v: {wit}) -> {rust} {{\n    match v {{\n"
        );
        for case in &e.cases {
            let _ = writeln!(
                out,
                "        {wit}::{} => {rust}::{},",
                kebab_to_camel(&case.name),
                case.rust_variant
            );
        }
        out.push_str("    }\n}\n\n");
        out
    }

    /// The `int` variant's bridge, minted PER INTERFACE: `generate!` declares a separate rust type
    /// for every interface that names it, so one shared bridge would not type-check across two.
    ///
    /// The rust→WIT direction has to match the ARM SHAPE, which `--preserve-encodings` changes from a
    /// tuple to named fields; the WIT→rust direction goes through the posture-independent
    /// `new_uint`/`new_nint` constructors and needs no fork.
    fn emit_int_bridges(&self, alias: &str) -> String {
        let rust = self.int_path();
        let wit = format!("{alias}::Int");
        let alias = bridge_suffix(alias);
        let (uint_pat, nint_pat) = if self.cli.preserve_encodings {
            (
                format!("{rust}::Uint {{ value, .. }}"),
                format!("{rust}::Nint {{ value, .. }}"),
            )
        } else {
            (
                format!("{rust}::Uint(value)"),
                format!("{rust}::Nint(value)"),
            )
        };
        format!(
            "fn int_to_wit_{alias}(v: &{rust}) -> {wit} {{\n    \
             match v {{\n        \
             {uint_pat} => {wit}::Uint(*value),\n        \
             {nint_pat} => {wit}::Nint(*value),\n    }}\n}}\n\n\
             fn int_from_wit_{alias}(v: {wit}) -> {rust} {{\n    \
             match v {{\n        \
             {wit}::Uint(value) => {rust}::new_uint(value),\n        \
             {wit}::Nint(value) => {rust}::new_nint(value),\n    }}\n}}\n\n"
        )
    }

    /// The 12-case discriminant bridge, per interface for the same reason the `int` one is.
    fn emit_any_cbor_kind_bridge(&self, alias: &str) -> String {
        let rust = format!("{}::any_cbor::AnyCborKind", self.runtime());
        let wit = format!("{alias}::AnyCborKind");
        let alias = bridge_suffix(alias);
        let mut out =
            format!("fn any_cbor_kind_to_wit_{alias}(v: &{rust}) -> {wit} {{\n    match v {{\n");
        for (wit_case, rust_variant) in super::wit::ANY_CBOR_KIND_CASES {
            let _ = writeln!(
                out,
                "        {rust}::{rust_variant} => {wit}::{},",
                kebab_to_camel(wit_case)
            );
        }
        out.push_str("    }\n}\n\n");
        out
    }

    /// One resource: the guest REP struct plus its `Guest<Resource>` impl.
    fn emit_resource(&self, resource: &WitResource, iface: &WitInterface) -> String {
        let alias = self
            .aliases
            .get(&iface.scope)
            .expect("every interface got an alias");
        let rep = rep_name(&resource.ident);
        let rust = self.rust_path(&resource.ident);
        // `pub` on both the struct and its field: `Borrow::get::<T>()` hands the rep back by
        // reference, and every cross-resource materialization in this file reads `.0` through it.
        let mut out = format!("pub struct {rep}(pub RefCell<{rust}>);\n\n");
        let _ = writeln!(
            out,
            "impl {alias}::Guest{} for {rep} {{",
            kebab_to_camel(&resource.name)
        );
        if let Some(ctor) = &resource.constructor {
            out.push_str(&self.emit_constructor(resource, ctor, alias));
        }
        for member in &resource.members {
            out.push_str(&self.emit_member(resource, member, alias));
        }
        out.push_str("}\n\n");
        out
    }

    /// A resource `constructor`, which `wit_bindgen` lowers to `fn new(..) -> Self` (or
    /// `Result<Self, E>` — the guest REP type, NOT the owned handle a fallible STATIC returns; one
    /// template cannot serve both, which is why statics are emitted separately below).
    fn emit_constructor(
        &self,
        resource: &WitResource,
        ctor: &WitConstructor,
        alias: &str,
    ) -> String {
        let rep = rep_name(&resource.ident);
        let rust = self.rust_path(&resource.ident);
        let params: Vec<String> = ctor
            .params
            .iter()
            .map(|p| {
                format!(
                    "{}: {}",
                    kebab_to_snake(&p.name),
                    self.wit_rust_type(&p.ty, alias, true)
                )
            })
            .collect();
        let ret = if ctor.fallible {
            " -> Result<Self, String>"
        } else {
            " -> Self"
        };
        let mut out = format!("\n    fn new({}){ret} {{\n", params.join(", "));
        let (lines, args) = self.materialize(&ctor.params, alias);
        for line in lines {
            let _ = writeln!(out, "        {line}");
        }
        // Whether the rust `new` returns a `Result` is a question about the RUST crate, not about
        // the WIT: the WIT constructor is ALSO fallible when a despecialized collection has to be
        // re-validated here, which the rust `new` knows nothing about. So it is read off the rust
        // face's own rule rather than off `ctor.fallible`.
        let call = format!("{rust}::new({})", args.join(", "));
        let build = if self.rust_new_can_fail(&resource.ident) {
            let _ = writeln!(out, "        let inner = {call}.map_err(err)?;");
            format!("{rep}(RefCell::new(inner))")
        } else {
            let _ = writeln!(out, "        let inner = {call};");
            format!("{rep}(RefCell::new(inner))")
        };
        if ctor.fallible {
            let _ = writeln!(out, "        Ok({build})");
        } else {
            let _ = writeln!(out, "        {build}");
        }
        out.push_str("    }\n");
        out
    }

    /// Bind every parameter to an owned rust value, one statement each.
    ///
    /// This IS the re-entrancy invariant in code: each borrow taken to clone a handle's interior is
    /// released at the end of its own `let`, so no two guards are ever live — and the caller may then
    /// `borrow_mut` self even when self is one of the arguments.
    fn materialize(&self, params: &[WitParam], iface: &str) -> (Vec<String>, Vec<String>) {
        let mut lines = Vec::new();
        let mut args = Vec::new();
        for param in params {
            let name = kebab_to_snake(&param.name);
            let conv = self.wit_to_rust(&param.ty, &name, iface);
            if param.validates && matches!(param.ty, WitType::List(_)) {
                // A despecialized collection (`[+ T]`'s `NonEmptyVec`, `@duplicates reject`'s
                // `OrderedSet`) crosses as a plain list, so its single `TryFrom` door has to be
                // re-entered here — at exactly the point the rust crate's own decoder enters it. The
                // `Vec<_>` binding is what makes `collect()` pick the door's input type.
                lines.push(format!("let {name}: Vec<_> = {};", conv.unwrapped()));
                args.push(format!("{name}.try_into().map_err(err)?"));
            } else if conv.expr == name && !conv.fallible {
                // The parameter already IS the rust value (a primitive, a string, a byte list): a
                // rebinding would be a no-op a reader has to check before believing.
                args.push(name);
            } else {
                lines.push(format!("let {name} = {};", conv.unwrapped()));
                args.push(name);
            }
        }
        (lines, args)
    }

    fn emit_member(&self, resource: &WitResource, member: &WitMember, alias: &str) -> String {
        let rep = rep_name(&resource.ident);
        let rust = self.rust_path(&resource.ident);
        let own = format!("{alias}::{}", kebab_to_camel(&resource.name));
        let mut params: Vec<String> = Vec::new();
        if !member.is_static {
            params.push("&self".to_owned());
        }
        for p in &member.params {
            params.push(format!(
                "{}: {}",
                kebab_to_snake(&p.name),
                self.wit_rust_type(&p.ty, alias, true)
            ));
        }
        // A `from-cbor-bytes` static returns the OWNING resource, which the projection cannot name
        // from inside a member without carrying its own owner — so the emitter fills it in, exactly
        // as the WIT renderer does.
        let ret = match member.op {
            WitMemberOp::FromCborBytes => {
                if member.fallible {
                    format!(" -> Result<{own}, String>")
                } else {
                    format!(" -> {own}")
                }
            }
            _ => self.signature_return(member.result.as_ref(), member.fallible, alias),
        };
        let mut out = format!(
            "\n    fn {}({}){ret} {{\n",
            kebab_to_snake(&member.name),
            params.join(", ")
        );
        let body = self.member_body(member, &rep, &rust, &own, alias);
        for line in body {
            let _ = writeln!(out, "        {line}");
        }
        out.push_str("    }\n");
        out
    }

    fn member_body(
        &self,
        member: &WitMember,
        rep: &str,
        rust: &str,
        own: &str,
        alias: &str,
    ) -> Vec<String> {
        let mut lines = Vec::new();
        match &member.op {
            // Getters read through ONE shared borrow and clone out of it. Minting a handle inside the
            // closure is safe — it constructs a NEW `RefCell`, it never borrows an existing one.
            WitMemberOp::Getter { field } | WitMemberOp::RestGetter { field } => {
                lines.push("let me = self.0.borrow();".to_owned());
                let ty = member
                    .result
                    .as_ref()
                    .expect("a getter always returns something");
                lines.push(self.rust_to_wit(ty, &format!("me.{field}"), alias, false));
            }
            // An optional FIXED-value field stores only whether it was present, which is a `bool` on
            // both sides and needs no conversion.
            WitMemberOp::PresenceGetter { field } => {
                lines.push(format!("self.0.borrow().{field}"));
            }
            WitMemberOp::PresenceSetter { field } => {
                let arg = kebab_to_snake(&member.params[0].name);
                lines.push(format!("self.0.borrow_mut().{field} = {arg};"));
            }
            // Setters exist only on OPTIONAL fields and take the BARE type — the getter reports
            // absence, the setter sets presence — so the assignment wraps in `Some`.
            WitMemberOp::Setter { field } => {
                let param = &member.params[0];
                let (materialized, args) = self.materialize(std::slice::from_ref(param), alias);
                // Every argument guard is released by these statements, BEFORE the `borrow_mut`.
                lines.extend(materialized);
                lines.push(format!("self.0.borrow_mut().{field} = Some({});", args[0]));
                if member.fallible {
                    lines.push("Ok(())".to_owned());
                }
            }
            // The rust getter returns the inner value by reference when it is not `Copy` and by value
            // when it is; `.clone()` normalizes both to an owned value, so one template serves both.
            WitMemberOp::WrapperGet { getter } => {
                lines.push("let me = self.0.borrow();".to_owned());
                lines.push(format!("let inner = me.{getter}().clone();"));
                let ty = member
                    .result
                    .as_ref()
                    .expect("a wrapper getter always returns something");
                lines.push(self.rust_to_wit(ty, "inner", alias, false));
            }
            WitMemberOp::ToCborBytes => {
                lines.push(format!(
                    "<{rust} as {rt}::serialization::{tr}>::to_cbor_bytes(&self.0.borrow())",
                    rt = self.runtime(),
                    tr = self.to_bytes_trait()
                ));
            }
            WitMemberOp::FromCborBytes => {
                let arg = kebab_to_snake(&member.params[0].name);
                lines.push(format!(
                    "<{rust} as {rt}::serialization::Deserialize>::from_cbor_bytes(&{arg})",
                    rt = self.runtime()
                ));
                lines.push(format!("    .map(|v| {own}::new({rep}(RefCell::new(v))))"));
                lines.push("    .map_err(err)".to_owned());
            }
        }
        lines
    }
}

/// A rust ident in `snake_case`, for minting a bridge function name off a type name.
fn convert_ident_to_snake(ident: &RustIdent) -> String {
    crate::utils::convert_to_snake_case(ident.as_ref())
}

/// The `generate!` macro's `path` value: the WIT directory's tail, because the macro resolves it
/// against `CARGO_MANIFEST_DIR` (the component crate root) and not against the file it sits in.
fn wit_dir_tail() -> &'static str {
    super::layout::COMPONENT_WIT_DIR
        .strip_prefix(super::layout::COMPONENT_DIR)
        .and_then(|rest| rest.strip_prefix('/'))
        .unwrap_or(super::layout::COMPONENT_WIT_DIR)
}

/// The interface-distinguishing half of a module alias, for minting the per-interface bridge
/// function names (`int_to_wit_<iface>`). Derived from the alias rather than carried separately so
/// the two can never name different interfaces.
fn bridge_suffix(alias: &str) -> &str {
    alias.strip_prefix("wit_").unwrap_or(alias)
}
