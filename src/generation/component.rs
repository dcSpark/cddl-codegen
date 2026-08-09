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

use super::enums::{EnumVariantInRust, enum_rule_tag_encoding_name};
use super::wit::{
    ImportedDepType, WitAccumulator, WitAccumulatorRef, WitConstructor, WitEnum, WitFunc,
    WitFuncOp, WitInterface, WitMember, WitMemberOp, WitPackage, WitParam, WitResource, WitType,
    WitTypeDef, WitTypeRef,
};
use crate::cli::Cli;
use crate::component_wit_deps::DepWitPackages;
use crate::intermediate::{
    EnumVariant, IntermediateTypes, ModuleScope, Representation, RustIdent, RustStructType,
};
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
    dep_wits: &DepWitPackages,
) -> String {
    let package = super::wit::project(types, cli, no_deserialize, dep_wits);
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

/// The rust module alias an IMPORTED dependency interface is reached through.
///
/// A different prefix family from [`interface_alias`] on purpose: a dependency's interface may be
/// named exactly like one of this package's (both `types`, the default), and the two are genuinely
/// different rust modules — `wit_bindgen` puts an EXPORTED interface under `exports::…` and an
/// IMPORTED one at the crate root, which is the visible form of the transitive-import rule. Keying
/// the alias on the dep name as well makes the two nameable side by side.
fn imported_interface_alias(dep: &str, iface_name: &str) -> String {
    format!(
        "wit_dep_{}_{}",
        kebab_to_snake(&dep.replace('-', "_")),
        kebab_to_snake(iface_name)
    )
}

/// Whether `wit_bindgen::generate!` mints a `Guest` trait for this interface — i.e. whether the
/// interface has anything a guest must IMPLEMENT.
///
/// A resource or a free function mints the trait; a declaration of pure VALUE types does not. So an
/// interface whose only projected type is a c-style enum gets a module carrying that enum and no
/// trait in it, and `impl <iface>::Guest for Component {}` there names something that does not exist
/// (E0405).
///
/// Both halves of the condition are load-bearing and each was probed against `wit-bindgen` 0.57
/// rather than reasoned about: an interface with a free function and NO resource DOES mint the
/// trait (the free functions land on it), so "has a resource" alone would suppress the impl that
/// this face's `any-cbor` free functions need.
pub(crate) fn interface_has_guest(iface: &WitInterface) -> bool {
    !iface.funcs.is_empty() || declares_resource(iface)
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

/// The body of a `map` closure over a list whose ELEMENT conversion is fallible, for a
/// `collect::<Result<Vec<_>, String>>()`.
///
/// A tuple element is REBUILT from parts each of which was already unwrapped with `?`, so the tuple
/// literal is a plain value and owes the closure its `Ok(..)`. Every other element's conversion IS
/// the `Result` — wrapping it would collect a `Vec<Result<..>>` inside a `Result`, which is a type
/// error in the emitted crate at the consuming call. The two cases are distinguished by the ELEMENT
/// TYPE rather than by inspecting the expression text, because the tuple branch is the only producer
/// of the rebuilt form.
fn wrap_fallible_element(element: &str, inner: &WitType) -> String {
    if matches!(inner, WitType::Tuple(_)) {
        format!("Ok({element})")
    } else {
        element.to_owned()
    }
}

/// The guest REP struct wrapping one generated rust type. Keyed by the RUST ident rather than the
/// WIT name because rust idents are unique across the whole IR while a WIT type name is unique only
/// within its interface — two scopes may each define a `foo`, and both reps live in this one file.
fn rep_name(ident: &RustIdent) -> String {
    format!("Wit{ident}")
}

/// Whether the interface declares anything the guest must implement — resources of either kind, or
/// free functions. See [`interface_has_guest`] for why both halves are load-bearing.
fn declares_resource(iface: &WitInterface) -> bool {
    iface
        .types
        .iter()
        .any(|def| matches!(def, WitTypeDef::Resource(_) | WitTypeDef::Accumulator(_)))
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

    fn alias_for(&self, r: &WitTypeRef) -> String {
        if let Some(dep_type) = self.imported(r) {
            return imported_interface_alias(&dep_type.dep, &dep_type.interface);
        }
        self.aliases
            .get(&r.scope)
            .cloned()
            .expect("every projected type's scope has an interface")
    }

    /// The dependency-type record behind a type reference, or `None` for a type this package defines.
    /// Every place the imported and exported shapes differ asks this — the two are the same WIT
    /// spelling with two different rust lowerings, and the projection is the only thing that knows
    /// which is which.
    fn imported(&self, r: &WitTypeRef) -> Option<&ImportedDepType> {
        self.package.imported.get(&r.ident)
    }

    /// The rust path of the DEPENDENCY's own type behind an imported handle — the native value the
    /// consumer's structs actually hold. Read through the same crate-boundary resolver the rust and
    /// wasm faces use, so a `@rust_name` pin and the dep's crate name are applied once.
    fn imported_rust_path(&self, r: &WitTypeRef) -> String {
        self.rust_path(&r.ident)
    }

    /// The accumulator behind a reference. Looked up rather than carried inline so the DECLARATION
    /// stays the single owner of the element shape — the reference is minted at the parameter it
    /// replaced, and only the declaration knows what the guest must store.
    fn accumulator(&self, r: &WitAccumulatorRef) -> &WitAccumulator {
        self.package.interfaces[&r.scope]
            .types
            .iter()
            .find_map(|def| match def {
                WitTypeDef::Accumulator(a) if a.name == r.name => Some(a),
                _ => None,
            })
            .expect("every accumulator reference is minted together with its declaration")
    }

    /// The guest REP struct of an accumulator. Keyed by INTERFACE NAME as well as by the WIT name:
    /// an accumulator's name is unique only within its interface (two interfaces may each carry a
    /// `token-list`), while every rep in this one file shares one rust namespace. The interface name
    /// is what the package-level collision detector already proved unique.
    fn acc_rep_name(&self, r: &WitAccumulatorRef) -> String {
        format!(
            "WitAcc{}{}",
            kebab_to_camel(&self.package.interfaces[&r.scope].name),
            kebab_to_camel(&r.name)
        )
    }

    /// The rust-CRATE type a WIT value becomes once [`Self::wit_to_rust`] has converted it — the
    /// element type an accumulator's `Vec` has to be declared with.
    ///
    /// Spelled rather than inferred because it lands in a STRUCT FIELD, the one position rust has no
    /// inference for. It mirrors `wit_to_rust`'s result arm for arm, which is a parallel derivation
    /// and therefore a drift risk; it is bounded by the fact that a mismatch is a compile error in
    /// the emitted crate, which the cross-crate wasip2 build gate exercises.
    fn native_rust_type(&self, ty: &WitType) -> String {
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
            WitType::Handle(r) => {
                if self.imported(r).is_some() {
                    self.imported_rust_path(r)
                } else {
                    self.rust_path(&r.ident)
                }
            }
            WitType::Enum(r) => self.rust_path(&r.ident),
            WitType::Int => self.int_path(),
            WitType::AnyCbor => format!("{}::any_cbor::AnyCbor", self.runtime()),
            WitType::AnyCborKind => format!("{}::any_cbor::AnyCborKind", self.runtime()),
            WitType::Option(inner) => format!("Option<{}>", self.native_rust_type(inner)),
            WitType::List(inner) => format!("Vec<{}>", self.native_rust_type(inner)),
            WitType::Tuple(parts) => {
                let parts: Vec<String> = parts.iter().map(|t| self.native_rust_type(t)).collect();
                if parts.len() == 1 {
                    format!("({},)", parts[0])
                } else {
                    format!("({})", parts.join(", "))
                }
            }
            // A nested accumulator hands over the `Vec` it settled, so the containing one stores
            // exactly that.
            WitType::Accumulator(a) => format!(
                "Vec<{}>",
                self.native_rust_type(&self.accumulator(a).element)
            ),
        }
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
                let alias = self.alias_for(r);
                if !param {
                    return format!("{alias}::{camel}");
                }
                // C-P4: `borrow<t>` lowers to two different rust shapes. An EXPORTED resource gets a
                // `TBorrow<'_>` newtype (the guest owns the rep, so the borrow carries a door onto
                // it); an IMPORTED one is a plain `&T`, because the guest owns nothing to look into
                // — the handle is the whole value. One template cannot serve both.
                if self.imported(r).is_some() {
                    format!("&{alias}::{camel}")
                } else {
                    format!("{alias}::{camel}Borrow<'_>")
                }
            }
            // An accumulator is a resource THIS package exports, so its borrow takes the exported
            // template — the `TBorrow<'_>` newtype, not the `&T` an imported handle lowers to.
            WitType::Accumulator(a) => {
                let camel = kebab_to_camel(&a.name);
                let alias = self
                    .aliases
                    .get(&a.scope)
                    .expect("every accumulator's interface has an alias");
                if param {
                    format!("{alias}::{camel}Borrow<'_>")
                } else {
                    format!("{alias}::{camel}")
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
            // An IMPORTED handle has no guest-side rep to clone out of: the value lives in the
            // DEPENDENCY's component instance, and the only thing that crosses is bytes. So the
            // conversion is the CBOR seam — one serialize on the far side, one deserialize here —
            // and it is fallible because the far side's encoding and this crate's linked dependency
            // can disagree (§7 precondition 2). Costs one serialize + copy + deserialize per value
            // per crossing, which is the price of sharing one dependency instance across consumers.
            WitType::Handle(r) if self.imported(r).is_some() => Conv {
                expr: format!(
                    "<{rust} as {rt}::serialization::Deserialize>::from_cbor_bytes(&{expr}.to_cbor_bytes()).map_err(err)",
                    rust = self.imported_rust_path(r),
                    rt = self.runtime()
                ),
                fallible: true,
            },
            WitType::Handle(r) => Conv::plain(format!(
                "{expr}.get::<{}>().0.borrow().clone()",
                rep_name(&r.ident)
            )),
            // The accumulator already ran the per-element seam, once per `push`/`insert`, so the
            // consuming door only clones the settled collection and re-`collect`s it into whatever
            // container the rust position wants. INFALLIBLE for exactly that reason — which is why
            // moving the cost to the filling member is what buys the honest signature here.
            //
            // The guard is taken and dropped inside this one expression, as every other composite
            // arm's is: the re-entrancy invariant.
            WitType::Accumulator(a) => Conv::plain(format!(
                "{expr}.get::<{}>().0.borrow().clone().into_iter().collect()",
                self.acc_rep_name(a)
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
                            "{expr}.into_iter().map(|{pattern}| {}).collect::<Result<Vec<_>, String>>()",
                            wrap_fallible_element(&element.expr, inner)
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
    ///
    /// Returns a [`Conv`] rather than a bare expression because ONE arm can fail: minting an
    /// IMPORTED handle runs the CBOR seam, and a `from-cbor-bytes` on the far side is fallible. The
    /// fallibility is carried rather than unwrapped in place for the same reason the parameter
    /// direction carries it — the same conversion is emitted at statement level and inside a `map`
    /// closure, and only one of those may spell `?`.
    fn rust_to_wit(&self, ty: &WitType, expr: &str, iface: &str, by_ref: bool) -> Conv {
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
            | WitType::F64 => Conv::plain(deref(expr)),
            WitType::Str => Conv::plain(format!("{expr}.clone()")),
            // The return half of the CBOR seam: this crate holds the DEPENDENCY's native value and
            // owes the caller a handle into the dependency's component instance, so the value is
            // serialized here and re-read by the dependency's own `from-cbor-bytes`. Fallible for the
            // reason the parameter half is, and the reason getters on dependency-typed fields carry a
            // `result<…, string>` on this face at all.
            WitType::Handle(r) if self.imported(r).is_some() => Conv {
                expr: format!(
                    "{alias}::{camel}::from_cbor_bytes(&<{rust} as {rt}::serialization::{tr}>::to_cbor_bytes({value})).map_err(err)",
                    alias = self.alias_for(r),
                    camel = kebab_to_camel(&r.name),
                    rust = self.imported_rust_path(r),
                    rt = self.runtime(),
                    tr = self.to_bytes_trait(),
                    value = by_reference(expr)
                ),
                fallible: true,
            },
            WitType::Handle(r) => Conv::plain(format!(
                "{}::{}::new({}(RefCell::new({expr}.clone())))",
                self.alias_for(r),
                kebab_to_camel(&r.name),
                rep_name(&r.ident)
            )),
            // Never a return: an accumulator exists only because a PARAMETER needed one, and a
            // collection return keeps `list<own t>`. Spelled for totality.
            WitType::Accumulator(_) => Conv::plain(deref(expr)),
            WitType::Enum(r) => Conv::plain(format!(
                "{}_to_wit({})",
                kebab_to_snake(&convert_ident_to_snake(&r.ident)),
                by_reference(expr)
            )),
            WitType::Int => Conv::plain(format!(
                "int_to_wit_{}({})",
                bridge_suffix(iface),
                by_reference(expr)
            )),
            WitType::AnyCbor => Conv::plain(format!(
                "<{}::any_cbor::AnyCbor as {}::serialization::{}>::to_cbor_bytes({})",
                self.runtime(),
                self.runtime(),
                self.to_bytes_trait(),
                by_reference(expr)
            )),
            WitType::AnyCborKind => Conv::plain(format!(
                "any_cbor_kind_to_wit_{}({})",
                bridge_suffix(iface),
                by_reference(expr)
            )),
            WitType::Option(inner) => {
                let inner_conv = self.rust_to_wit(inner, "x", iface, true);
                if inner_conv.fallible {
                    // `map(..).transpose()` turns `Option<Result<T, E>>` into the `Result<Option<T>, E>`
                    // the signature wants, exactly as the parameter direction does.
                    Conv {
                        expr: format!("{expr}.as_ref().map(|x| {}).transpose()", inner_conv.expr),
                        fallible: true,
                    }
                } else {
                    Conv::plain(format!("{expr}.as_ref().map(|x| {})", inner_conv.expr))
                }
            }
            WitType::Tuple(inner) => {
                let names: Vec<String> = (0..inner.len()).map(|i| format!("t{i}")).collect();
                let convs: Vec<Conv> = inner
                    .iter()
                    .zip(&names)
                    .map(|(t, n)| self.rust_to_wit(t, n, iface, true))
                    .collect();
                let fallible = convs.iter().any(|c| c.fallible);
                let parts: Vec<String> = convs.iter().map(|c| c.unwrapped()).collect();
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
                if fallible {
                    Conv {
                        expr: format!(
                            "(|| -> Result<_, String> {{ let {head} = {}; Ok({body}) }})()",
                            by_reference(expr)
                        ),
                        fallible: true,
                    }
                } else {
                    Conv::plain(format!("{{ let {head} = {}; {body} }}", by_reference(expr)))
                }
            }
            WitType::List(inner) => {
                // `iter()` rather than a container-specific door: every rust collection this face can
                // reach (`Vec`, `NonEmptyVec`, `BTreeMap`, `OrderedHashMap`, `OrderedSet`, `PairMap`)
                // has one, and the `|(k, v)|` head below destructures both a `&(K, V)` element and a
                // map's `(&K, &V)` pair identically under default binding modes.
                let (head, element) = match &**inner {
                    WitType::Tuple(parts) => {
                        let names: Vec<String> =
                            (0..parts.len()).map(|i| format!("x{i}")).collect();
                        let convs: Vec<Conv> = parts
                            .iter()
                            .zip(&names)
                            .map(|(t, n)| self.rust_to_wit(t, n, iface, true))
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
                    other => ("x".to_owned(), self.rust_to_wit(other, "x", iface, true)),
                };
                if element.fallible {
                    Conv {
                        expr: format!(
                            "{expr}.iter().map(|{head}| {}).collect::<Result<Vec<_>, String>>()",
                            wrap_fallible_element(&element.expr, inner)
                        ),
                        fallible: true,
                    }
                } else {
                    Conv::plain(format!(
                        "{expr}.iter().map(|{head}| {}).collect()",
                        element.expr
                    ))
                }
            }
        }
    }

    // ---------------------------------------------------------------------------------------------
    // Emission
    // ---------------------------------------------------------------------------------------------

    /// The `(dep, interface)` pairs this package imports, deduplicated and in a deterministic order.
    /// Derived from the RESOLVED types rather than from the flag list, so a declared dependency
    /// nothing actually references contributes no alias and no `with:` row.
    fn imported_interfaces(&self) -> BTreeSet<(String, String)> {
        self.package
            .imported
            .values()
            .filter(|dep_type| self.package.imported_packages.contains_key(&dep_type.dep))
            .map(|dep_type| (dep_type.dep.clone(), dep_type.interface.clone()))
            .collect()
    }

    /// The `with:` keys, one per imported interface — the exact `use` paths the emitted WIT names,
    /// so the two can never disagree about a version or a spelling.
    fn with_entries(&self) -> BTreeSet<String> {
        self.package
            .imported
            .values()
            .filter(|dep_type| self.package.imported_packages.contains_key(&dep_type.dep))
            .map(|dep_type| dep_type.use_path.clone())
            .collect()
    }

    fn emit(&self) -> String {
        let mut out = String::new();
        let has_resource = self.package.interfaces.values().any(declares_resource);
        if has_resource {
            out.push_str("use core::cell::RefCell;\n\n");
        }
        // `path` is resolved against CARGO_MANIFEST_DIR, i.e. the component crate root — NOT against
        // the file holding the macro. So the literal is the bare `wit` tail even though this file
        // sits two directories below it.
        // C-P1: a materialized `wit/deps` tree is NECESSARY but not SUFFICIENT. With the dep
        // package present the WIT resolves, encodes and validates, and the macro still panics with
        // ``missing `with` mapping for the key `<dep-package>/<iface>@<ver>` `` — `wit_bindgen`
        // refuses to decide silently whether a foreign package's bindings are generated here or
        // taken from another crate. So the same derivation that produces the deps copy emits one
        // `with:` row per imported interface, and the keys come out of the DEPENDENCY's own WIT
        // rather than being reconstructed (they have to match it byte for byte).
        let with = self.with_entries();
        let with = if with.is_empty() {
            String::new()
        } else {
            format!(
                "    with: {{\n{}    }},\n",
                with.iter()
                    .map(|key| format!("        \"{key}\": generate,\n"))
                    .collect::<String>()
            )
        };
        let _ = write!(
            out,
            "wit_bindgen::generate!({{\n    path: \"{}\",\n    world: \"{}\",\n{with}}});\n\n",
            wit_dir_tail(),
            self.package.world
        );
        // Everything below the `generate!` invocation exists to serve a `Guest` impl, so a package
        // with none of them emits the invocation ALONE — and that is not a tidiness choice, it is
        // what compiles. `generate!` mints an `export!` macro only for a world that HAS exports, and
        // a `Guest` trait only for an interface that has something to implement (see
        // [`interface_has_guest`]); a spec whose every rule resolves through (an alias, a named
        // collection) projects no interface at all, and one whose only projected type is a c-style
        // enum projects an interface of pure value declarations. The unconditional block was
        // `cannot find macro export` on the first and E0405 on the second. What is left once the
        // impls are gone — the guest type, its `export!`, the `err` funnel every fallible door
        // reports through, the enum/`int`/`any-cbor-kind` bridges and the interface `use` aliases
        // they are spelled against — has no possible caller in a package with no resource and no
        // free function, so it would be dead code in a file the user cannot edit. The WIT surface is
        // untouched either way: a world's exports live in the component type section `generate!`
        // emits regardless of `export!`.
        let guest_interfaces: Vec<&WitInterface> = self
            .package
            .interfaces
            .values()
            .filter(|iface| interface_has_guest(iface))
            .collect();
        if guest_interfaces.is_empty() {
            return out;
        }

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
        // An IMPORTED interface's module is at the CRATE ROOT, not under `exports::` — that split is
        // the visible form of WIT's transitive-import rule (the world exports what the guest
        // implements and imports what it merely names).
        for (dep, interface) in self.imported_interfaces() {
            let package = &self.package.imported_packages[&dep];
            let (namespace, name) = package_segments(&package.package_id);
            let _ = writeln!(
                out,
                "use {}::{}::{} as {};",
                kebab_to_snake(&namespace),
                kebab_to_snake(&name),
                kebab_to_snake(&interface),
                imported_interface_alias(&dep, &interface)
            );
        }
        out.push('\n');

        // ONE `Component` implementing every implementable interface's `Guest` trait, covered by ONE
        // `export!`. That is the shape the component model wants: a world's exports are implemented
        // by a single guest type, and a second `export!` would emit a second set of canonical-ABI
        // symbols.
        out.push_str("struct Component;\n\n");
        for iface in &guest_interfaces {
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
                    // A `<name>-kind` enum needs no bridge function: it is only ever RETURNED, by
                    // the one member (`kind`) that matches the rust DATA enum inline.
                    // An accumulator needs none either: it is a rust type of this file's own, so
                    // there is no `generate!`-minted counterpart to bridge to.
                    WitTypeDef::Kind(_)
                    | WitTypeDef::AnyCborAlias
                    | WitTypeDef::Resource(_)
                    | WitTypeDef::Accumulator(_) => {}
                }
            }
        }

        for iface in self.package.interfaces.values() {
            for def in &iface.types {
                match def {
                    WitTypeDef::Resource(resource) => {
                        out.push_str(&self.emit_resource(resource, iface))
                    }
                    WitTypeDef::Accumulator(acc) => {
                        out.push_str(&self.emit_accumulator(acc, iface))
                    }
                    _ => {}
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
            match def {
                WitTypeDef::Resource(resource) => {
                    let _ = writeln!(
                        out,
                        "    type {} = {};",
                        kebab_to_camel(&resource.name),
                        rep_name(&resource.ident)
                    );
                }
                WitTypeDef::Accumulator(acc) => {
                    let _ = writeln!(
                        out,
                        "    type {} = {};",
                        kebab_to_camel(&acc.name),
                        self.acc_rep_name(&WitAccumulatorRef {
                            scope: acc.scope.clone(),
                            name: acc.name.clone(),
                        })
                    );
                }
                _ => {}
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
            // `any-cbor` is bytes on the wire, so the JSON door decodes FIRST and renders second —
            // and both halves can fail, which is why the WIT declares a `result`. The rendering half
            // fails for a reason the type system cannot express: `AnyCbor`'s serde impl reports
            // "key must be a string" for a non-string-keyed map.
            WitFuncOp::CborToJson => {
                let arg = kebab_to_snake(&func.params[0].name);
                let _ = writeln!(
                    out,
                    "        <{rt}::any_cbor::AnyCbor as {rt}::serialization::Deserialize>::from_cbor_bytes(&{arg})\n            \
                     .map_err(err)\n            .and_then(|v| serde_json::to_string_pretty(&v).map_err(err))",
                    rt = self.runtime()
                );
            }
            WitFuncOp::CborFromJson => {
                let arg = kebab_to_snake(&func.params[0].name);
                let _ = writeln!(
                    out,
                    "        serde_json::from_str::<{rt}::any_cbor::AnyCbor>(&{arg})\n            \
                     .map_err(err)\n            \
                     .map(|v| <{rt}::any_cbor::AnyCbor as {rt}::serialization::{tr}>::to_cbor_bytes(&v))",
                    rt = self.runtime(),
                    tr = self.to_bytes_trait()
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

    /// One ACCUMULATOR: the guest REP struct — a `RefCell<Vec<..>>` of the rust-crate element type —
    /// plus its `Guest<Resource>` impl.
    ///
    /// The whole point of the shape lives in the filling member: it runs the CBOR seam ONCE PER
    /// ELEMENT, so by the time the collection reaches the constructor or setter that consumes it,
    /// there is nothing left to convert and nothing left to fail. That is what moves the fallibility
    /// off the consuming door and onto `push`/`insert`, and it is why the accumulator exists at all —
    /// an imported resource may only be borrowed in a NON-REPEATED parameter position, because
    /// wit-bindgen's Rust backend miscompiles every repeated one (E0506, measured unfixed through
    /// 0.60.0). See `wit::WitAccumulator`.
    ///
    /// The re-entrancy invariant holds here the way it holds everywhere else on this face: the
    /// element is materialized to an owned rust value in its own statement, and only then is the
    /// accumulator's own `RefCell` borrowed mutably.
    fn emit_accumulator(&self, acc: &WitAccumulator, iface: &WitInterface) -> String {
        let alias = self
            .aliases
            .get(&iface.scope)
            .expect("every interface got an alias");
        let rep = self.acc_rep_name(&WitAccumulatorRef {
            scope: acc.scope.clone(),
            name: acc.name.clone(),
        });
        let element = self.native_rust_type(&acc.element);
        // The cause, restated in the emitted crate for the same reason the emitted WIT restates it:
        // a reader meeting the indirection here needs to know it is forced, and a future toolchain
        // fix needs a stated trigger to revisit the shape.
        let mut out = format!(
            "/// The carrier the WIT resource `{}` is backed by: a dependency-typed collection\n\
             /// PARAMETER, already converted. An imported resource may only be borrowed in a\n\
             /// NON-REPEATED parameter position — wit-bindgen's Rust backend miscompiles every\n\
             /// repeated one (E0506), measured unfixed through 0.60.0 — so the caller fills this\n\
             /// one element at a time and the CBOR seam runs once per `{}`.\n\
             pub struct {rep}(pub RefCell<Vec<{element}>>);\n\n",
            acc.name,
            acc.filler()
        );
        let _ = writeln!(
            out,
            "impl {alias}::Guest{} for {rep} {{",
            kebab_to_camel(&acc.name)
        );
        let _ = write!(
            out,
            "\n    fn new() -> Self {{\n        {rep}(RefCell::new(Vec::new()))\n    }}\n"
        );

        // The filler's parameters, as ordinary `WitParam`s so they go through the SAME
        // materialization the hand-written doors do — including its re-entrancy discipline.
        let synthetic = |name: &str, ty: &WitType| WitParam {
            name: name.to_owned(),
            rust_name: name.to_owned(),
            ty: ty.clone(),
            validates: false,
            rust_type: None,
        };
        let params = match acc.row() {
            Some((key, value)) => vec![synthetic("k", key), synthetic("v", value)],
            None => vec![synthetic("v", &acc.element)],
        };
        let signature: Vec<String> = params
            .iter()
            .map(|p| format!("{}: {}", p.name, self.wit_rust_type(&p.ty, alias, true)))
            .collect();
        let ret = if acc.fallible {
            " -> Result<(), String>"
        } else {
            ""
        };
        let _ = write!(
            out,
            "\n    fn {}(&self, {}){ret} {{\n",
            acc.filler(),
            signature.join(", ")
        );
        let (lines, args) = self.materialize(&params, alias);
        for line in lines {
            let _ = writeln!(out, "        {line}");
        }
        let pushed = if args.len() == 1 {
            args[0].clone()
        } else {
            format!("({})", args.join(", "))
        };
        let _ = writeln!(out, "        self.0.borrow_mut().push({pushed});");
        if acc.fallible {
            out.push_str("        Ok(())\n");
        }
        out.push_str("    }\n");

        let _ = write!(
            out,
            "\n    fn {}(&self) -> u32 {{\n        self.0.borrow().len() as u32\n    }}\n",
            super::wit::ACCUMULATOR_LEN_MEMBER
        );
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
            let despecialized = param
                .rust_type
                .as_ref()
                .is_some_and(|ty| super::wit::wit_param_despecialized(ty, self.types));
            if despecialized {
                // A despecialized collection (`[+ T]`'s `NonEmptyVec`, `@duplicates reject`'s
                // `OrderedSet`) crosses as a plain list, so its single `TryFrom` door has to be
                // re-entered here — at exactly the point the rust crate's own decoder enters it. The
                // `Vec<_>` binding is what makes `collect()` pick the door's input type.
                //
                // Routed off the rust TYPE, never off "validates and is a list": that reading also
                // caught a plain bounded array, whose identity `TryFrom<Vec<T>>` compiles while
                // checking nothing, and a bounded map, for which no such `TryFrom` exists at all.
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

    /// The value-window checks a SETTER owes, emitted against the BOUNDARY value and BEFORE any
    /// conversion of it.
    ///
    /// Both halves of that position are forced, not chosen. It is a setter's job and no other
    /// member's: every other door that takes a bounded parameter hands it to a rust constructor that
    /// range-checks it and whose `Result` the guest already unwraps, while a setter writes the field
    /// directly. And it must precede the conversion, because a `.len()` check on a
    /// `collect()`-bound local is E0282 in generated code — the container type is pinned only by the
    /// consuming assignment, which comes after. That is also exactly where the wasm face puts its
    /// own check (`records.rs` emits `value_bounds_check_line` on the wasm parameter, ahead of
    /// `from_wasm_boundary_clone`), so the two faces stay structurally parallel.
    ///
    /// Reads the CONDITION out of `bounds.rs`, the single owner of every value-window spelling in
    /// this project; only the `Err(..)` construction is component-specific.
    fn boundary_bounds_checks(&self, params: &[WitParam]) -> Vec<String> {
        params
            .iter()
            .filter_map(|param| {
                super::bounds::component_bounds_check_line(
                    param.rust_type.as_ref()?,
                    &kebab_to_snake(&param.name),
                    &self.runtime(),
                )
            })
            .collect()
    }

    /// A conversion in RETURN position, as the final expression of a member body.
    ///
    /// The two axes are independent: the CONVERSION may fail (an imported handle's CBOR seam) and the
    /// MEMBER may be declared fallible (which the projection does whenever any part of the signature
    /// touches an imported type). So the `?` comes from the conversion and the `Ok(..)` from the
    /// declaration, and a member that is fallible for one part of its signature still wraps a
    /// conversion that cannot fail.
    fn returned(&self, conv: &Conv, fallible: bool) -> String {
        match (conv.fallible, fallible) {
            // The conversion already evaluates to the `Result` the signature declares — `Ok(x?)`
            // would be the same value spelled twice, and `clippy::needless_question_mark` in the
            // user's own crate.
            (true, _) => conv.expr.clone(),
            (false, true) => format!("Ok({})", conv.expr),
            (false, false) => conv.expr.clone(),
        }
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
        // The members that MINT the owning resource — `from-cbor-bytes`, `from-raw-bytes`,
        // `from-json` and a choice's `new-<variant>` — cannot name it from inside the member without
        // carrying their own owner, so the emitter fills it in, exactly as the WIT renderer does.
        let ret = match member.op {
            WitMemberOp::FromCborBytes
            | WitMemberOp::FromRawBytes
            | WitMemberOp::FromJson
            | WitMemberOp::NewVariant { .. } => {
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
        let body = self.member_body(member, &resource.ident, &rep, &rust, &own, alias);
        for line in body {
            let _ = writeln!(out, "        {line}");
        }
        out.push_str("    }\n");
        out
    }

    /// The IR variants (and representation) behind a choice resource, or `None` for anything else.
    ///
    /// STRUCTURE read from the IR, which is exactly what this module is allowed to read: the
    /// projection decided every WIT name, and what it deliberately does not carry is how the rust
    /// enum's ARMS are spelled — a question about the rust crate that `--preserve-encodings` changes
    /// and that has exactly one owner (`enums::EnumVariantInRust`).
    fn choice_variants(
        &self,
        ident: &RustIdent,
    ) -> Option<(&[EnumVariant], Option<Representation>, Option<usize>)> {
        let rust_struct = self.types.rust_struct(ident)?;
        match rust_struct.variant() {
            RustStructType::TypeChoice { variants } => Some((variants, None, rust_struct.tag())),
            RustStructType::GroupChoice { variants, rep } => {
                Some((variants, Some(*rep), rust_struct.tag()))
            }
            _ => None,
        }
    }

    /// The `<name>-kind` enum a choice's `kind` member returns, found by the choice's rust ident.
    fn kind_enum(&self, ident: &RustIdent) -> Option<&WitEnum> {
        self.package
            .interfaces
            .values()
            .flat_map(|iface| &iface.types)
            .find_map(|def| match def {
                WitTypeDef::Kind(e) if &e.ident == ident => Some(e),
                _ => None,
            })
    }

    fn member_body(
        &self,
        member: &WitMember,
        ident: &RustIdent,
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
                let conv = self.rust_to_wit(ty, &format!("me.{field}"), alias, false);
                lines.push(self.returned(&conv, member.fallible));
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
            // absence, the setter sets presence — so the assignment wraps in `Some`. A field the
            // projection marked `plain_storage` (a `.default`-carrying member, whose default fills
            // the absent case) is stored WITHOUT that presence-`Option`, so its assignment must not
            // wrap. The bit comes from the projection, which is also what made the getter's result
            // type bare; re-deriving it here from the IR could disagree with that.
            WitMemberOp::Setter {
                field,
                plain_storage,
            } => {
                let param = &member.params[0];
                // The value window, checked on the boundary value before anything else touches it —
                // a setter is the one door with no rust constructor between the caller and the
                // field, so without this the WIT's `result<_, string>` would promise a check the
                // glue never performs.
                lines.extend(self.boundary_bounds_checks(std::slice::from_ref(param)));
                let (materialized, args) = self.materialize(std::slice::from_ref(param), alias);
                // Every argument guard is released by these statements, BEFORE the `borrow_mut`.
                lines.extend(materialized);
                let assigned = if *plain_storage {
                    args[0].clone()
                } else {
                    format!("Some({})", args[0])
                };
                lines.push(format!("self.0.borrow_mut().{field} = {assigned};"));
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
                let conv = self.rust_to_wit(ty, "inner", alias, false);
                lines.push(self.returned(&conv, member.fallible));
            }
            WitMemberOp::ToCborBytes => {
                lines.push(format!(
                    "<{rust} as {rt}::serialization::{tr}>::to_cbor_bytes(&self.0.borrow())",
                    rt = self.runtime(),
                    tr = self.to_bytes_trait()
                ));
            }
            // The canonical re-encoding door. `Serialize` is named LITERALLY rather than through
            // `to_bytes_trait()`: `to_canonical_cbor_bytes` is declared on that trait and on no
            // other, and the row exists only in the posture whose runtime composes it — so the trait
            // that owns the method is the honest spelling, and the projection's gate is what keeps
            // the two in step.
            WitMemberOp::ToCanonicalCborBytes => {
                lines.push(format!(
                    "<{rust} as {rt}::serialization::Serialize>::to_canonical_cbor_bytes(&self.0.borrow())",
                    rt = self.runtime()
                ));
            }
            // The JSON seam, over the serde impls the rust face DERIVES under the same flag. The
            // `&*` is load-bearing: `serde_json`'s parameter is generic, so the auto-deref that lets
            // the cbor seam pass a `Ref` to a `&Self` parameter does not apply.
            WitMemberOp::ToJson => {
                lines.push(
                    "serde_json::to_string_pretty(&*self.0.borrow()).map_err(err)".to_owned(),
                );
            }
            WitMemberOp::FromJson => {
                let arg = kebab_to_snake(&member.params[0].name);
                lines.push(format!("serde_json::from_str::<{rust}>(&{arg})"));
                lines.push(format!("    .map(|v| {own}::new({rep}(RefCell::new(v))))"));
                lines.push("    .map_err(err)".to_owned());
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
            // The RAW-bytes seam, and deliberately not the cbor one: the contract a
            // `_CDDL_CODEGEN_RAW_BYTES_TYPE_` imposes on the user's type is `RawBytesEncoding`, and
            // nothing requires `Serialize` of it — glue naming that trait would not compile.
            // `to_raw_bytes` hands back a borrow, so the copy to an owned `Vec` is this face's, not
            // the trait's.
            WitMemberOp::ToRawBytes => {
                lines.push(format!(
                    "<{rust} as {rt}::serialization::RawBytesEncoding>::to_raw_bytes(&self.0.borrow()).to_vec()",
                    rt = self.runtime()
                ));
            }
            WitMemberOp::FromRawBytes => {
                let arg = kebab_to_snake(&member.params[0].name);
                lines.push(format!(
                    "<{rust} as {rt}::serialization::RawBytesEncoding>::from_raw_bytes(&{arg})",
                    rt = self.runtime()
                ));
                lines.push(format!("    .map(|v| {own}::new({rep}(RefCell::new(v))))"));
                lines.push("    .map_err(err)".to_owned());
            }
            // A choice's discriminant, derived by matching the rust DATA enum — never by naming the
            // rust `<Name>Kind`, which is emitted only under `cli.wasm` and is therefore absent from
            // exactly the posture this face targets. The arm spelling comes from the single owner the
            // wasm face's own `kind()` reads, so a preserve/non-preserve arm-shape fork can never
            // exist here in one spelling and there in another.
            WitMemberOp::VariantKind => {
                let (variants, variant_rep, tag) = self
                    .choice_variants(ident)
                    .expect("a `kind` member is only projected for a choice");
                let rule_tag_encoding =
                    enum_rule_tag_encoding_name(self.types, variants, variant_rep, tag, self.cli);
                let kind = self
                    .kind_enum(ident)
                    .expect("a `kind` member is projected beside its `<name>-kind` enum");
                let wit_kind = format!("{alias}::{}", kebab_to_camel(&kind.name));
                lines.push("let me = self.0.borrow();".to_owned());
                lines.push("match &*me {".to_owned());
                for (variant, case) in variants.iter().zip(&kind.cases) {
                    let arm = EnumVariantInRust::new(
                        self.types,
                        variant,
                        variant_rep,
                        tag,
                        rule_tag_encoding.as_deref(),
                        self.cli,
                    );
                    lines.push(format!(
                        "    {rust}::{}{} => {wit_kind}::{},",
                        variant.name,
                        arm.capture_ignore_all(),
                        kebab_to_camel(&case.name)
                    ));
                }
                lines.push("}".to_owned());
            }
            // `as-<variant>`: the payload as a SNAPSHOT (every composite arm of `rust_to_wit`
            // clones), `None` on every other arm.
            WitMemberOp::AsVariant { rust_variant } => {
                let (variants, variant_rep, tag) = self
                    .choice_variants(ident)
                    .expect("an `as-` member is only projected for a choice");
                let variant = variants
                    .iter()
                    .find(|v| v.name.to_string() == *rust_variant)
                    .expect("the projection named a variant of this choice");
                let rule_tag_encoding =
                    enum_rule_tag_encoding_name(self.types, variants, variant_rep, tag, self.cli);
                let arm = EnumVariantInRust::new(
                    self.types,
                    variant,
                    variant_rep,
                    tag,
                    rule_tag_encoding.as_deref(),
                    self.cli,
                );
                let Some(WitType::Option(payload)) = member.result.as_ref() else {
                    unreachable!("an `as-` member always returns an option of its payload");
                };
                let conv = self.rust_to_wit(payload, &arm.names[0], alias, true);
                lines.push("let me = self.0.borrow();".to_owned());
                // A fallible payload conversion puts the `?` inside the arm and the `Ok` outside the
                // whole `match`, so both arms still produce the same `Option<…>` and only the
                // function's return type changes.
                lines.push(if member.fallible {
                    "Ok(match &*me {".to_owned()
                } else {
                    "match &*me {".to_owned()
                });
                lines.push(format!(
                    "    {rust}::{}{} => Some({}),",
                    variant.name,
                    arm.capture_ignore_encodings(),
                    conv.unwrapped()
                ));
                // Emitted only when there IS another arm: a one-variant choice's `_` arm is
                // unreachable, and rustc warns on it in generated code the user cannot edit.
                if variants.len() > 1 {
                    lines.push("    _ => None,".to_owned());
                }
                lines.push(if member.fallible {
                    "})".to_owned()
                } else {
                    "}".to_owned()
                });
            }
            // `new-<variant>`: a STATIC, so it returns the owned HANDLE rather than the rep type a
            // fallible constructor returns. Parameters go through `materialize`, so the re-entrancy
            // invariant holds here exactly as it does in a constructor.
            WitMemberOp::NewVariant {
                rust_ctor,
                rust_can_fail,
            } => {
                let (materialized, args) = self.materialize(&member.params, alias);
                lines.extend(materialized);
                let call = format!("{rust}::{rust_ctor}({})", args.join(", "));
                if *rust_can_fail {
                    lines.push(format!("let inner = {call}.map_err(err)?;"));
                } else {
                    lines.push(format!("let inner = {call};"));
                }
                let build = format!("{own}::new({rep}(RefCell::new(inner)))");
                if member.fallible {
                    lines.push(format!("Ok({build})"));
                } else {
                    lines.push(build);
                }
            }
        }
        lines
    }
}

/// The namespace and name of a WIT package id (`cddl:chain@0.1.0` -> `("cddl", "chain")`), which are
/// the first two segments of the rust module path `wit_bindgen` puts an imported interface at.
///
/// Read off the DEPENDENCY's own id string rather than re-parsed into `WitPackageId`: the id is
/// carried verbatim from the dependency's WIT precisely so nothing here re-derives it, and a version
/// suffix is the only part this split has to drop.
fn package_segments(package_id: &str) -> (String, String) {
    let base = package_id.split('@').next().unwrap_or(package_id);
    match base.split_once(':') {
        Some((namespace, name)) => (namespace.to_owned(), name.to_owned()),
        None => (String::new(), base.to_owned()),
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
