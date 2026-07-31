//! The WIT face of the generated workspace: the naming rules and the pre-generation detectors that
//! decide whether a spec CAN be projected to a WIT package at all.
//!
//! # Contract
//!
//! This module is a pure IR→WIT projection, structurally a sibling of [`super::extern_interface`]:
//! it walks the FINALIZED IR, renders deterministically out of `BTreeMap`/`BTreeSet`s, and anything
//! it cannot render is EXCLUDED AND RECORDED rather than panicked on. It must not depend on the
//! guest-crate emitters — the dependency runs one way, `component.rs` → `wit.rs` — because the
//! rust↔WIT parity gate and the cross-crate export both consume the projection, and a
//! re-derivation at the second consumer is a silent-drift source.
//!
//! # Two reference walks, and why they must agree
//!
//! [`collect_projected_refs`] answers "which SCOPES does this type reach" for the cycle detector;
//! [`map_conceptual`] answers "what does this occurrence SPELL in WIT" and collects its references
//! as a by-product. Both resolve a transparent alias AND a NAMED COLLECTION **through**, never
//! recording the resolved-through ident: `rec` in `b` holding a `names` in `a` whose element lives
//! in `c` records the edge `b → c`, which is the `use` the projection emits, and not `b → a`, which
//! it does not. A collection RULE is skipped by the cycle walk for the same reason [`project`]
//! skips it — it contributes no WIT type, so it owns no interface and can be no end of an edge.
//!
//! Agreement is load-bearing in the FALSE direction rather than the missing one: an edge the
//! detector invents rejects a spec whose emitted WIT would have resolved perfectly, and a user
//! cannot work around a refusal that names scopes the WIT never links.

use crate::cli::Cli;
use crate::intermediate::{
    AliasIdent, ConceptualRustType, EnumVariant, EnumVariantData, IntermediateTypes, ModuleScope,
    Primitive, ROOT_SCOPE, Representation, RestKind, RestSemantics, RustField, RustIdent,
    RustRecord, RustStruct, RustStructType, RustType,
};
use crate::utils::convert_to_kebab_case;
use std::collections::{BTreeMap, BTreeSet};

/// Every word the WIT lexer treats as a keyword across the toolchain versions this project supports
/// — deliberately a **superset** of any single version's set, because the set has moved in BOTH
/// directions and escaping a non-keyword is accepted everywhere.
///
/// Provenance, read out of the lexers rather than from memory (the `match` on the lexed word in
/// `wit-parser`'s `src/ast/lex.rs`):
///
/// - **wit-parser 0.247 / 0.252** (the pinned floor and its successor) — the whole list below except
///   `float32`/`float64`.
/// - **wit-parser 0.240** (standing in for the wasm-tools ≤ 1.231-era consumer floor) — the same
///   list except `map`, PLUS `float32`/`float64`, which it lexes as the `f32`/`f64` keywords under
///   its `require_f32_f64` compatibility switch and 0.247 has since dropped.
///
/// So `map` is a keyword only at the newer end and `float32`/`float64` only at the older end. A name
/// escaped as `%map` parses at both; escaping the union is the only choice compatible in both
/// directions. Bump this list when the pinned `wit-parser` moves — by UNION, never by replacement.
///
/// `_` is a keyword at every version and is deliberately ABSENT: `convert_to_kebab_case` maps
/// underscores to word separators, so a bare `_` cannot be produced, and `%_` is not a valid escaped
/// identifier anyway (an id must have at least one word).
///
pub(crate) const WIT_KEYWORDS: &[&str] = &[
    "as",
    "async",
    "bool",
    "borrow",
    "char",
    "constructor",
    "enum",
    "error-context",
    "export",
    "f32",
    "f64",
    "flags",
    "float32",
    "float64",
    "from",
    "func",
    "future",
    "import",
    "include",
    "interface",
    "list",
    "map",
    "option",
    "own",
    "package",
    "record",
    "resource",
    "result",
    "s16",
    "s32",
    "s64",
    "s8",
    "static",
    "stream",
    "string",
    "tuple",
    "type",
    "u16",
    "u32",
    "u64",
    "u8",
    "use",
    "variant",
    "with",
    "world",
];

/// A WIT identifier as it is WRITTEN: `%`-prefixed iff it is a keyword.
///
/// Applied at RENDER time only. The `%` is WIT syntax, not part of the name — it does not reach the
/// generated Rust bindings, and both the name-collision detector and the rust↔WIT parity gate
/// compare the UNESCAPED spelling — so escaping must never be baked into
/// [`convert_to_kebab_case`].
pub(crate) fn wit_escape(name: &str) -> String {
    if WIT_KEYWORDS.contains(&name) {
        format!("%{name}")
    } else {
        name.to_owned()
    }
}

/// A WIT package identifier: `<namespace>:<name>@<version>`.
///
/// Parsed from `--wit-package`, or derived from `--lib-name` when the flag is absent. The default
/// cannot be a clap `default_value` because it depends on another flag's value, which is why the
/// flag's field is an `Option<String>` and this type is minted by [`Cli::wit_package`].
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub(crate) struct WitPackageId {
    pub namespace: String,
    pub name: String,
    pub version: String,
}

impl WitPackageId {
    /// The default for a `--lib-name`: `cddl:<kebab lib-name>@0.1.0`.
    pub(crate) fn default_for_lib_name(lib_name: &str) -> Self {
        Self {
            namespace: "cddl".to_owned(),
            name: convert_to_kebab_case(lib_name),
            version: "0.1.0".to_owned(),
        }
    }

    /// Parse a `--wit-package` value: `<ns>:<name>` with an optional `@<version>`.
    ///
    /// Shape-checked here rather than at first use so a typo is a clap error naming the flag, not a
    /// WIT parse failure three stages later against a file the user did not write. The version is
    /// checked as dot-separated non-empty numeric-led parts rather than against full semver: WIT
    /// accepts pre-release/build metadata, and a stricter check here would reject values the
    /// resolver takes.
    pub(crate) fn parse(s: &str) -> Result<Self, String> {
        let (id, version) = match s.split_once('@') {
            Some((id, version)) => (id, version),
            None => (s, "0.1.0"),
        };
        let (namespace, name) = id.split_once(':').ok_or_else(|| {
            format!(
                "--wit-package value must be <namespace>:<name>[@<version>] (e.g. \
                 `cddl:my-lib@0.1.0`), got: {s:?}"
            )
        })?;
        for (label, part) in [("namespace", namespace), ("name", name)] {
            if part.is_empty() {
                return Err(format!(
                    "--wit-package {label} is empty in {s:?}: both sides of the `:` are required, \
                     as in `cddl:my-lib@0.1.0`"
                ));
            }
            if let Some(c) = part
                .chars()
                .find(|c| !matches!(c, 'a'..='z' | '0'..='9' | '-'))
            {
                return Err(format!(
                    "invalid character {c:?} in the --wit-package {label} {part:?}: a WIT package \
                     identifier is kebab-case ASCII ([a-z0-9-]), as in `cddl:my-lib@0.1.0`"
                ));
            }
            if part.starts_with('-') || part.ends_with('-') || part.contains("--") {
                return Err(format!(
                    "malformed --wit-package {label} {part:?}: a WIT identifier is `-`-separated \
                     words, so it can neither begin nor end with `-` nor carry an empty word"
                ));
            }
            if part.starts_with(|c: char| c.is_ascii_digit()) {
                return Err(format!(
                    "--wit-package {label} {part:?} begins with a digit: a WIT identifier's first \
                     word must start with a letter"
                ));
            }
        }
        if version.is_empty() {
            return Err(format!(
                "--wit-package version is empty in {s:?}: drop the `@` to take the default \
                 `@0.1.0`, or spell a version as in `cddl:my-lib@0.1.0`"
            ));
        }
        // The `x.y.z` core, checked before any pre-release (`-`) or build (`+`) metadata, which WIT
        // accepts and which nothing here needs to interpret.
        let core = version
            .split(['-', '+'])
            .next()
            .expect("split always yields at least one part");
        let parts: Vec<&str> = core.split('.').collect();
        if parts.len() != 3
            || parts
                .iter()
                .any(|p| p.is_empty() || !p.chars().all(|c| c.is_ascii_digit()))
        {
            return Err(format!(
                "malformed --wit-package version {version:?}: WIT versions are semver, so the \
                 leading part must be `<major>.<minor>.<patch>` (e.g. `0.1.0`), optionally \
                 followed by `-<pre-release>` or `+<build>`"
            ));
        }
        Ok(Self {
            namespace: namespace.to_owned(),
            name: name.to_owned(),
            version: version.to_owned(),
        })
    }
}

impl std::fmt::Display for WitPackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}@{}", self.namespace, self.name, self.version)
    }
}

// =================================================================================================
// The projection value
//
// `component.rs` consumes this, never the rendered text and never the IR — so every member carries
// BOTH its WIT name and the rust ident/expression it bridges to. A name re-derived at the emitter
// is a silent-drift source between the two faces and between the emitter and the parity gate.
// =================================================================================================

/// A shape the phase-1 projection cannot render. Never surfaced to the user as an error: the walk
/// converts it to an EXCLUSION record (R5), so a spec carrying a phase-2 type class generates a WIT
/// WITHOUT that type rather than failing.
#[derive(Clone, Debug)]
pub(crate) enum WitError {
    /// The type's own shape has no phase-1 WIT projection.
    Unprojectable { shape: String },
}

impl std::fmt::Display for WitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WitError::Unprojectable { shape } => write!(f, "{shape}"),
        }
    }
}

fn unprojectable(shape: impl Into<String>) -> WitError {
    WitError::Unprojectable {
        shape: shape.into(),
    }
}

type ProjectResult<T> = Result<T, WitError>;

/// The whole WIT package as a value: one interface per exported `ModuleScope`, plus the exclusion
/// records that make the phase-1 type coverage visible in the emitted file instead of silent.
#[derive(Clone, Debug)]
pub(crate) struct WitPackage {
    pub id: WitPackageId,
    /// The world name (`<kebab lib-name>-world`), UNESCAPED.
    pub world: String,
    pub interfaces: BTreeMap<ModuleScope, WitInterface>,
    /// Types kept out of the projection, keyed by rust ident — rendered as `// unexported:` rows in
    /// the interface of the scope that owns them.
    pub excluded: BTreeMap<RustIdent, WitExclusion>,
}

/// One WIT `interface`, i.e. one exported module scope, i.e. one input file.
#[derive(Clone, Debug)]
pub(crate) struct WitInterface {
    /// The interface name, UNESCAPED (`a::c` → `a-c`; the root scope → `types`).
    pub name: String,
    pub scope: ModuleScope,
    /// `use <interface>.{<type>, …};` — the cross-interface edges, by target interface NAME.
    pub uses: BTreeMap<String, BTreeSet<String>>,
    /// Type definitions, in render order (sorted by WIT name).
    pub types: Vec<WitTypeDef>,
    /// Free functions (the `any-cbor` introspection door), in render order.
    pub funcs: Vec<WitFunc>,
}

/// A type kept OUT of the projection. `root` names the head of the reference chain, so a
/// transitively-excluded type points at the original cause rather than at its neighbour.
#[derive(Clone, Debug)]
pub(crate) struct WitExclusion {
    pub scope: ModuleScope,
    pub reason: String,
    pub root: String,
}

/// One item in an interface's type namespace.
#[derive(Clone, Debug)]
pub(crate) enum WitTypeDef {
    Resource(WitResource),
    Enum(WitEnum),
    /// `enum <name>-kind { … }` — the discriminant of a type/group choice, one case per variant.
    ///
    /// Structurally identical to [`WitTypeDef::Enum`] and deliberately a DISTINCT variant: a c-style
    /// enum's `ident` names a rust enum the guest bridges both directions, while a kind enum's
    /// `ident` names the CHOICE it discriminates and has no rust counterpart at all — the rust
    /// `<Name>Kind` is emitted only under `cli.wasm`, which the component face never requires. The
    /// guest derives the discriminant by matching the rust DATA enum, so it must never be handed the
    /// c-style enum's bridge template.
    Kind(WitEnum),
    /// The fixed `variant int { uint(u64), nint(u64) }` — emitted into every interface that uses
    /// it. Its shape is not derived from the IR (the `Int` prelude extern carries none), so it has
    /// no payload.
    IntVariant,
    /// `type any-cbor = list<u8>;`
    AnyCborAlias,
    /// `enum any-cbor-kind { … }` — the 12 cases of the static runtime's `AnyCborKind`.
    AnyCborKind,
}

impl WitTypeDef {
    /// The item's WIT name, UNESCAPED — the key the collision detector and the parity gate compare.
    pub fn name(&self) -> &str {
        match self {
            WitTypeDef::Resource(r) => &r.name,
            WitTypeDef::Enum(e) | WitTypeDef::Kind(e) => &e.name,
            WitTypeDef::IntVariant => INT_TYPE_NAME,
            WitTypeDef::AnyCborAlias => ANY_CBOR_TYPE_NAME,
            WitTypeDef::AnyCborKind => ANY_CBOR_KIND_TYPE_NAME,
        }
    }
}

/// A class-backed type: a WIT `resource` over the rust crate's struct.
#[derive(Clone, Debug)]
pub(crate) struct WitResource {
    /// UNESCAPED WIT name.
    pub name: String,
    /// The rust type this resource wraps (`RefCell<<crate>::<ident>>` in the guest).
    pub ident: RustIdent,
    pub constructor: Option<WitConstructor>,
    pub members: Vec<WitMember>,
}

/// A c-style enum: a WIT `enum` VALUE type. The WIT-side rust type is minted by
/// `wit_bindgen::generate!`, so the guest needs a per-case bridge — hence each case carries the
/// rust variant ident it maps to.
#[derive(Clone, Debug)]
pub(crate) struct WitEnum {
    pub name: String,
    pub ident: RustIdent,
    pub cases: Vec<WitEnumCase>,
}

#[derive(Clone, Debug)]
pub(crate) struct WitEnumCase {
    /// UNESCAPED WIT name.
    pub name: String,
    /// The rust enum variant ident (`Color::<rust_variant>`). See [`WitMemberOp`] for why the
    /// rust-side half of every pair is carried and why it reads as dead until the emitters land.
    #[allow(dead_code)]
    pub rust_variant: String,
}

/// A resource `constructor`. Fallible constructors lower to `fn new(..) -> Result<Self, E>` (Ok =
/// the guest REP type) while a fallible STATIC lowers to `Result<Handle, E>` — one emitter template
/// cannot serve both, which is why the two are separate shapes here.
#[derive(Clone, Debug)]
pub(crate) struct WitConstructor {
    pub params: Vec<WitParam>,
    pub fallible: bool,
}

/// One `func` on a resource.
#[derive(Clone, Debug)]
pub(crate) struct WitMember {
    /// UNESCAPED WIT name.
    pub name: String,
    /// `static func` rather than a method.
    pub is_static: bool,
    pub params: Vec<WitParam>,
    /// The `ok` type, or `None` for `result<_, string>` / a `func()` returning nothing.
    pub result: Option<WitType>,
    pub fallible: bool,
    /// What the guest glue must DO — the half `component.rs` cannot re-derive from a name.
    pub op: WitMemberOp,
}

/// The rust-side operation a WIT member bridges to.
///
/// `#[allow(dead_code)]` on the payloads (here and on [`WitParam::rust_name`],
/// [`WitEnumCase::rust_variant`], [`WitFunc::op`]): this is the half of the projection the RENDERER
/// never reads and the guest emitter reads exclusively. It is carried anyway — that is the whole
/// point of projecting to a VALUE rather than to text — because an emitter that re-derived a rust
/// name from a WIT name would drift silently from both this module and the parity gate. The
/// attributes come off with `component.rs`.
#[derive(Clone, Debug)]
#[allow(dead_code)]
pub(crate) enum WitMemberOp {
    /// Read `self.0.borrow().<field>` and clone it across the boundary.
    Getter {
        field: String,
    },
    /// Write `self.0.borrow_mut().<field>`; the value is materialized BEFORE the `borrow_mut`
    /// (the re-entrancy invariant).
    Setter {
        field: String,
    },
    /// Read the `bool` presence flag a mandatory-less fixed-value field stores.
    PresenceGetter {
        field: String,
    },
    /// Write that same presence flag.
    PresenceSetter {
        field: String,
    },
    /// A `@newtype` wrapper's inner-value getter (`self.0.borrow().<getter>()`).
    WrapperGet {
        getter: String,
    },
    /// The open-struct rest row's captured content.
    RestGetter {
        field: String,
    },
    ToCborBytes,
    FromCborBytes,
    /// `to-canonical-cbor-bytes`, through `Serialize::to_canonical_cbor_bytes`.
    ///
    /// Projected ONLY under `--preserve-encodings --canonical-form`, which is the one posture where
    /// the composed runtime carries that method at all (it is declared on the `Serialize` trait, and
    /// `Serialize` is composed from `serialization_preserve_force_canonical.rs`). Every other posture
    /// composes `ToCBORBytes`, which declares `to_cbor_bytes` and nothing else.
    ToCanonicalCborBytes,
    /// `to-json`, through `serde_json::to_string_pretty` over the rust type's serde impl.
    ToJson,
    /// `from-json` STATIC, through `serde_json::from_str`.
    FromJson,
    /// A raw-bytes bridge's `to-raw-bytes`, through `RawBytesEncoding` — NOT the cbor seam.
    ToRawBytes,
    /// A raw-bytes bridge's `from-raw-bytes` STATIC, through the same trait.
    FromRawBytes,
    /// A choice's `kind`: match the rust DATA enum and report the `<name>-kind` case.
    ///
    /// The rust `<Name>Kind` enum is emitted only under `cli.wasm`, so the guest may never name it;
    /// the arm spelling comes from the same owner the wasm face's `kind()` uses.
    VariantKind,
    /// A choice's `as-<variant>`: `Some(payload)` on the matching arm, `None` otherwise. Carries the
    /// RUST variant ident so the emitter looks the variant up rather than un-kebabbing a WIT name.
    AsVariant {
        rust_variant: String,
    },
    /// A choice's `new-<variant>` STATIC, bridging the rust enum's own `new_<variant>`.
    ///
    /// `rust_can_fail` is the rust ctor's OWN fallibility, which is not the member's: the member is
    /// also fallible when a despecialized parameter has to be re-validated at the boundary, and the
    /// rust ctor knows nothing about that. Carried rather than re-derived (R3) — the emitter would
    /// otherwise have to re-walk the variant's fields to decide whether to `?`.
    NewVariant {
        rust_ctor: String,
        rust_can_fail: bool,
    },
}

/// One parameter of a constructor / method / free function.
#[derive(Clone, Debug)]
pub(crate) struct WitParam {
    /// UNESCAPED WIT name.
    pub name: String,
    /// The rust field/argument name this parameter feeds. Positional for constructors, but carried
    /// anyway so the emitter never re-kebabs a name back. See [`WitMemberOp`].
    #[allow(dead_code)]
    pub rust_name: String,
    pub ty: WitType,
    /// Whether THIS parameter's WIT→rust conversion is the one that can fail — i.e. whether the
    /// projection despecialized a type whose invariant has to be re-checked at the boundary (see
    /// [`wit_param_validates`]).
    ///
    /// Carried per parameter rather than re-derived, for the same reason every other rust-side half
    /// is: the owning signature's `fallible` bit says only that SOMETHING here can fail, and a guest
    /// emitter that guessed WHICH would either drop a required `TryFrom` door or wrap an infallible
    /// conversion in one that does not exist.
    pub validates: bool,
    /// The rust type this parameter's value has to become, or `None` for a SYNTHESIZED parameter
    /// that corresponds to no IR type at all (`bytes` on `from-cbor-bytes`/`from-raw-bytes`,
    /// `present` on a presence setter, `v` on the free `cbor-kind` function).
    ///
    /// Carried because the guest emitter has two independent decisions to make here and neither is
    /// derivable from `validates` alone: whether the type was DESPECIALIZED (re-enter its `TryFrom`
    /// door — see [`wit_param_despecialized`]) and whether it carries a VALUE WINDOW (emit the range
    /// check). `validates` is the union of those and of CDDL `any`, so routing on it conflates them
    /// — which is how a plain bounded array reached the `TryFrom` door, where `Vec<T>`'s identity
    /// `TryFrom` compiles while checking nothing. R3: the projection carries the rust fact rather
    /// than the emitter re-deriving it.
    ///
    /// WHERE the window is checked stays a per-SITE decision the emitter owns rather than a property
    /// of the parameter: a constructor or a `new-<variant>` static delegates to a rust constructor
    /// that range-checks and whose `Result` the guest already unwraps, so only a SETTER — which
    /// writes the field directly, with nothing between the caller and the invariant — emits one.
    pub rust_type: Option<RustType>,
}

/// An interface-level free function (no `self`).
#[derive(Clone, Debug)]
pub(crate) struct WitFunc {
    pub name: String,
    pub params: Vec<WitParam>,
    pub result: Option<WitType>,
    pub fallible: bool,
    /// See [`WitMemberOp`].
    #[allow(dead_code)]
    pub op: WitFuncOp,
}

#[derive(Clone, Debug)]
pub(crate) enum WitFuncOp {
    /// `AnyCbor::from_cbor_bytes(v)?.kind()`.
    AnyCborKind,
    /// `AnyCbor::from_cbor_bytes(v)?` rendered through its serde impl.
    CborToJson,
    /// The inverse: parse the JSON representation of a CBOR item and hand back its bytes.
    CborFromJson,
}

/// A WIT type at a USE site. Ownership (`own` vs `borrow`) is a POSITION property applied at render
/// — never stored — because the same field type appears in both directions.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub(crate) enum WitType {
    Bool,
    U8,
    U16,
    U32,
    U64,
    S8,
    S16,
    S32,
    S64,
    F32,
    F64,
    Str,
    List(Box<WitType>),
    Tuple(Vec<WitType>),
    Option(Box<WitType>),
    /// A handle to a resource this package defines.
    Handle(WitTypeRef),
    /// A c-style enum value type this package defines.
    Enum(WitTypeRef),
    Int,
    AnyCbor,
    /// The `any-cbor-kind` enum. Synthesized per interface like [`WitType::AnyCbor`], so it needs no
    /// [`WitTypeRef`] — nothing ever `use`s it across an interface boundary.
    AnyCborKind,
}

/// A reference to a named type, carrying the scope that DEFINES it so the `use` graph is computable
/// without a second IR walk.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub(crate) struct WitTypeRef {
    pub scope: ModuleScope,
    /// UNESCAPED WIT name.
    pub name: String,
    pub ident: RustIdent,
}

/// The WIT name of the full-range CBOR integer variant.
const INT_TYPE_NAME: &str = "int";
/// The WIT name of the transparent CBOR-item alias.
const ANY_CBOR_TYPE_NAME: &str = "any-cbor";
/// The WIT name of its discriminant enum.
const ANY_CBOR_KIND_TYPE_NAME: &str = "any-cbor-kind";
/// The free function projecting `AnyCbor::kind()`.
const ANY_CBOR_KIND_FUNC_NAME: &str = "cbor-kind";
/// The free functions projecting `AnyCbor`'s serde impls, under `--json-serde-derives`.
const ANY_CBOR_TO_JSON_FUNC_NAME: &str = "cbor-to-json";
const ANY_CBOR_FROM_JSON_FUNC_NAME: &str = "cbor-from-json";
/// The WIT member names of the JSON seam every class-backed type the tool DEFINES carries.
const TO_JSON_MEMBER_NAME: &str = "to-json";
const FROM_JSON_MEMBER_NAME: &str = "from-json";
/// The interface name the ROOT scope (`lib`) projects to. `lib` is a cargo-layout word with no
/// meaning at the WIT boundary, and `types` is what a hand-written package calls the interface
/// holding a package's types — the spike's spelling. A spec whose own scope converts to `types`
/// collides with it, which the package-level collision check reports.
const ROOT_INTERFACE_NAME: &str = "types";
/// The 12 cases of the static runtime's `AnyCborKind`, as (WIT case, rust variant) pairs. Read out
/// of `static/any_cbor_*.rs`; the pairing is what lets the guest glue bridge both directions
/// without re-deriving a name.
pub(crate) const ANY_CBOR_KIND_CASES: &[(&str, &str)] = &[
    ("uint", "UInt"),
    ("nint", "NInt"),
    ("bytes", "Bytes"),
    ("text", "Text"),
    ("array", "Array"),
    ("map", "Map"),
    ("tag", "Tag"),
    ("bool", "Bool"),
    ("null", "Null"),
    ("undefined", "Undefined"),
    ("unassigned", "Unassigned"),
    ("float", "Float"),
];
/// The ident of the reserved prelude extern for the full CBOR integer range. Spelled the same way
/// the four other sites that special-case it do (`rust_type.rs`, `extern_interface.rs`,
/// `emit_tests.rs`, `emit_tests_wasm.rs`) so one grep finds them all.
const INT_EXTERN_IDENT: &str = "Int";

/// A type staged for inclusion, with the named types it references (for the reference closure) and
/// whether it pulled in the per-interface `int` / `any-cbor` definitions.
struct StagedType {
    scope: ModuleScope,
    /// One rust type may mint MORE than one WIT item: a choice mints its `resource` and the
    /// `<name>-kind` enum that discriminates it. They stage and unstage together — a kind enum whose
    /// resource was excluded would name nothing.
    defs: Vec<WitTypeDef>,
    refs: BTreeSet<RustIdent>,
    uses_int: bool,
    uses_any_cbor: bool,
}

// =================================================================================================
// The projection walk
// =================================================================================================

/// Walk the FINALIZED IR into a [`WitPackage`]. INFALLIBLE by construction (R5): a type whose shape
/// has no phase-1 projection — or which references one — is EXCLUDED AND RECORDED, and the package
/// still renders.
///
/// The `match` over `RustStructType` is EXHAUSTIVE (no `_ =>` arm) so a new IR variant forces an
/// explicit projection decision at compile time rather than silently vanishing from the WIT.
pub(crate) fn project(
    types: &IntermediateTypes,
    cli: &Cli,
    no_deserialize: &BTreeSet<RustIdent>,
) -> WitPackage {
    let mut staged: BTreeMap<RustIdent, StagedType> = BTreeMap::new();
    let mut excluded: BTreeMap<RustIdent, WitExclusion> = BTreeMap::new();
    // The bases of generic externs, computed once. `generic_extern_base_idents` — not the narrower
    // `generic_instance_bases` — is the correct set here for the reason its own doc gives: neither
    // the parse-time record nor the instance-derived one subsumes the other. (The wasm glue's
    // narrower call site is not a model to copy.)
    let generic_extern_bases = types.generic_extern_base_idents();

    for (ident, rust_struct) in types.rust_structs() {
        let scope = types.scope(ident);
        if !scope.export() {
            continue;
        }
        // The reserved `int` prelude extern is not a type of its own here: it projects to the `int`
        // VARIANT at each use site (B3a), so listing it as an unexported extern would be a false
        // record of a type the WIT actually carries.
        if ident.to_string() == INT_EXTERN_IDENT {
            continue;
        }
        // A named collection (`names = [+ text]`, `attrs = {* text => uint}`) is RESOLVED THROUGH at
        // its use sites rather than surfaced — the same rule the CDDL alias row takes, and what
        // keeps the wasm-posture purity invariant reachable. It is neither included nor excluded:
        // the WIT is complete without it.
        if matches!(
            rust_struct.variant(),
            RustStructType::Array { .. } | RustStructType::Table { .. }
        ) {
            continue;
        }
        // A generic extern BASE (`Foo` of `Foo<Bar>`) is skipped for the same reason: it names no
        // concrete type, so a bridging resource over it would be a resource over nothing. Only its
        // INSTANCES are bridged, each under the instance ident. Neither included nor excluded — the
        // WIT is complete without it — and a rule that references the base BARE is excluded at the
        // reference instead, where the reason can name the shape.
        if generic_extern_bases.contains(ident) {
            continue;
        }
        let name = wit_type_name(ident);
        let mut refs = BTreeSet::new();
        let mut ctx = TypeCtx {
            types,
            cli,
            refs: &mut refs,
            uses_int: false,
            uses_any_cbor: false,
            resolving: BTreeSet::new(),
            generic_extern_bases: &generic_extern_bases,
        };
        let projected = project_struct(
            &name,
            ident,
            rust_struct,
            !no_deserialize.contains(ident),
            &mut ctx,
        );
        let (uses_int, uses_any_cbor) = (ctx.uses_int, ctx.uses_any_cbor);
        match projected {
            Ok(defs) => {
                staged.insert(
                    ident.clone(),
                    StagedType {
                        scope: scope.clone(),
                        defs,
                        refs,
                        uses_int,
                        uses_any_cbor,
                    },
                );
            }
            Err(e) => {
                excluded.insert(
                    ident.clone(),
                    WitExclusion {
                        scope: scope.clone(),
                        reason: e.to_string(),
                        root: ident.to_string(),
                    },
                );
            }
        }
    }

    // Reference closure to fixpoint: a resource whose signature names an excluded type would dangle,
    // so it is excluded too, naming the chain ROOT rather than its immediate neighbour. Monotone
    // (types only ever leave `staged`), so it terminates; deterministic (`BTreeMap` iteration, first
    // offending reference in `BTreeSet` order).
    loop {
        let next = staged.iter().find_map(|(ident, st)| {
            st.refs.iter().find(|r| !staged.contains_key(*r)).map(|r| {
                let root = excluded
                    .get(r)
                    .map(|e| e.root.clone())
                    .unwrap_or_else(|| r.to_string());
                (ident.clone(), root)
            })
        });
        let Some((ident, root)) = next else {
            break;
        };
        let st = staged.remove(&ident).expect("just found in the same map");
        excluded.insert(
            ident,
            WitExclusion {
                scope: st.scope,
                reason: format!("references excluded {root}"),
                root,
            },
        );
    }

    // Assemble the interfaces. Every exported scope carrying a staged OR excluded type gets one; an
    // interface with nothing in it is still legal WIT and is still emitted (R6).
    let mut interfaces: BTreeMap<ModuleScope, WitInterface> = BTreeMap::new();
    for st in staged.values() {
        ensure_interface(&mut interfaces, &st.scope);
    }
    for exc in excluded.values() {
        ensure_interface(&mut interfaces, &exc.scope);
    }
    for st in staged.values() {
        let iface = interfaces
            .get_mut(&st.scope)
            .expect("every staged scope was ensured above");
        iface.types.extend(st.defs.iter().cloned());
        if st.uses_int {
            iface.types.push(WitTypeDef::IntVariant);
        }
        if st.uses_any_cbor {
            iface.types.push(WitTypeDef::AnyCborAlias);
            iface.types.push(WitTypeDef::AnyCborKind);
            iface.funcs.push(any_cbor_kind_func());
            // The JSON door onto the same transparent alias. It belongs HERE and not on a resource
            // because `any-cbor` IS the alias — there is no resource to hang a method on — and it
            // exists per interface for the same reason the `cbor-kind` introspection door does.
            iface.funcs.extend(any_cbor_json_funcs(cli));
        }
        // Cross-interface `use`: a type defined in ANOTHER exported scope must be imported by name.
        for referenced in &st.refs {
            let target = types.scope(referenced);
            if target == &st.scope {
                continue;
            }
            iface
                .uses
                .entry(interface_name(target))
                .or_default()
                .insert(wit_type_name(referenced));
        }
    }
    // The per-interface `int` / `any-cbor` definitions are pushed once per USING type above, so
    // deduplicate them (and give every interface a stable render order) here.
    for iface in interfaces.values_mut() {
        iface.types.sort_by(|a, b| a.name().cmp(b.name()));
        // BOTH sides must be synthesized: a user type that happens to convert to `any-cbor` is a
        // COLLISION for the detector to report, and collapsing it here would silently swallow the
        // very thing the detector exists to catch.
        iface
            .types
            .dedup_by(|a, b| synthesized_type(a) && synthesized_type(b) && a.name() == b.name());
        iface.funcs.sort_by(|a, b| a.name.cmp(&b.name));
        iface.funcs.dedup_by(|a, b| a.name == b.name);
    }

    WitPackage {
        id: cli.wit_package(),
        world: world_name(&cli.lib_name),
        interfaces,
        excluded,
    }
}

/// Whether a type definition is one of the fixed, per-interface SYNTHESIZED ones (`int`,
/// `any-cbor`, `any-cbor-kind`) — the only ones that may legitimately be staged more than once and
/// therefore the only ones deduplication may collapse. A repeated USER type name is a collision the
/// detector reports, never something to silently drop.
fn ensure_interface(interfaces: &mut BTreeMap<ModuleScope, WitInterface>, scope: &ModuleScope) {
    interfaces
        .entry(scope.clone())
        .or_insert_with(|| WitInterface {
            name: interface_name(scope),
            scope: scope.clone(),
            uses: BTreeMap::new(),
            types: Vec::new(),
            funcs: Vec::new(),
        });
}

fn synthesized_type(def: &WitTypeDef) -> bool {
    matches!(
        def,
        WitTypeDef::IntVariant | WitTypeDef::AnyCborAlias | WitTypeDef::AnyCborKind
    )
}

fn any_cbor_kind_func() -> WitFunc {
    WitFunc {
        name: ANY_CBOR_KIND_FUNC_NAME.to_owned(),
        params: vec![WitParam {
            name: "v".to_owned(),
            rust_name: "v".to_owned(),
            ty: WitType::AnyCbor,
            // The bytes are decoded through `AnyCbor::from_cbor_bytes`, which is exactly the
            // boundary re-check this flag marks.
            validates: true,
            rust_type: None,
        }],
        result: Some(WitType::AnyCborKind),
        fallible: true,
        op: WitFuncOp::AnyCborKind,
    }
}

/// The two JSON doors onto the `any-cbor` alias, or nothing off `--json-serde-derives`.
///
/// Both are FALLIBLE for reasons the type system cannot carry: `cbor-to-json` decodes arbitrary
/// caller bytes first (the same decode-is-the-check reasoning `cbor-kind` states), and then renders
/// through `AnyCbor`'s serde impl, which the runtime documents as able to fail at a non-string map
/// key. `cbor-from-json` parses caller-supplied text.
fn any_cbor_json_funcs(cli: &Cli) -> Vec<WitFunc> {
    if !cli.json_serde_derives {
        return Vec::new();
    }
    vec![
        WitFunc {
            name: ANY_CBOR_TO_JSON_FUNC_NAME.to_owned(),
            params: vec![WitParam {
                name: "v".to_owned(),
                rust_name: "v".to_owned(),
                ty: WitType::AnyCbor,
                validates: true,
                rust_type: None,
            }],
            result: Some(WitType::Str),
            fallible: true,
            op: WitFuncOp::CborToJson,
        },
        WitFunc {
            name: ANY_CBOR_FROM_JSON_FUNC_NAME.to_owned(),
            params: vec![WitParam {
                name: "json".to_owned(),
                rust_name: "json".to_owned(),
                ty: WitType::Str,
                validates: true,
                rust_type: None,
            }],
            result: Some(WitType::AnyCbor),
            fallible: true,
            op: WitFuncOp::CborFromJson,
        },
    ]
}

/// The WIT name of a rust type: its ident, kebab-converted. Deliberately the RUST ident rather than
/// the CDDL source rule name — the rust↔WIT parity gate compares against the rust surface, and
/// `@name` renames the rust ident, which is what makes the rename the documented collision remedy.
fn wit_type_name(ident: &RustIdent) -> String {
    convert_to_kebab_case(ident.as_ref())
}

/// The interface name of a module scope: its path segments kebab-converted and joined with `-`
/// (`a::c` → `a-c`). Flattening is non-injective, which is why the package-level collision check
/// exists rather than a cleverer join.
fn interface_name(scope: &ModuleScope) -> String {
    if scope == &*ROOT_SCOPE {
        return ROOT_INTERFACE_NAME.to_owned();
    }
    scope
        .components()
        .iter()
        .map(|c| convert_to_kebab_case(c))
        .collect::<Vec<_>>()
        .join("-")
}

/// The world name for a `--lib-name`.
fn world_name(lib_name: &str) -> String {
    format!("{}-world", convert_to_kebab_case(lib_name))
}

/// The per-occurrence state a type mapping threads: what it referenced, whether it pulled in a
/// synthesized definition, and which named collections it is currently resolving THROUGH (a
/// self-referential collection alias would otherwise recurse forever — a stack overflow is a panic,
/// and this module never panics).
struct TypeCtx<'a, 'b> {
    types: &'a IntermediateTypes<'b>,
    /// The flag posture, because two SEAMS are flag-gated: `to-canonical-cbor-bytes` exists only
    /// where the runtime composes it, and the JSON pair only under `--json-serde-derives`. Carried
    /// on the walk rather than threaded through every projection signature, for the same reason
    /// `refs` is.
    cli: &'a Cli,
    refs: &'a mut BTreeSet<RustIdent>,
    uses_int: bool,
    uses_any_cbor: bool,
    resolving: BTreeSet<RustIdent>,
    /// The generic-extern bases [`project`] skips, so a reference to one is excluded WITH a reason
    /// naming the shape rather than falling through the closure as an unexplained missing type.
    generic_extern_bases: &'a BTreeSet<RustIdent>,
}

/// Project one IR struct into its WIT type definition(s). A choice mints two (its `resource` and
/// its `<name>-kind` enum); everything else mints exactly one.
fn project_struct(
    name: &str,
    ident: &RustIdent,
    rust_struct: &RustStruct,
    deserializable: bool,
    ctx: &mut TypeCtx,
) -> ProjectResult<Vec<WitTypeDef>> {
    match rust_struct.variant() {
        RustStructType::Record(record) => Ok(vec![WitTypeDef::Resource(project_record(
            name,
            ident,
            record,
            deserializable,
            ctx,
        )?)]),
        RustStructType::Wrapper {
            wrapped,
            min_max,
            float_min_max,
        } => Ok(vec![WitTypeDef::Resource(project_wrapper(
            name,
            ident,
            rust_struct,
            wrapped,
            min_max.is_some() || float_min_max.is_some(),
            deserializable,
            ctx,
        )?)]),
        RustStructType::CStyleEnum { variants } => Ok(vec![WitTypeDef::Enum(
            project_c_style_enum(name, ident, variants),
        )]),
        // Resolved through at use sites; never reached (filtered in `project`), but spelled out so
        // the match stays exhaustive without an `_ =>` arm.
        RustStructType::Array { .. } | RustStructType::Table { .. } => Err(unprojectable(
            "a named collection is resolved through at its use sites, never surfaced",
        )),
        RustStructType::TypeChoice { variants } => {
            project_choice(name, ident, variants, None, deserializable, ctx)
        }
        RustStructType::GroupChoice { variants, rep } => {
            project_choice(name, ident, variants, Some(*rep), deserializable, ctx)
        }
        // The reserved `Int` extern and every generic-extern BASE are filtered before this point;
        // every OTHER extern gets the bridging resource.
        RustStructType::Extern => Ok(vec![WitTypeDef::Resource(project_extern_bridge(
            name,
            ident,
            deserializable,
            ctx.cli,
        ))]),
        RustStructType::RawBytesType => Ok(vec![WitTypeDef::Resource(project_raw_bytes_bridge(
            name, ident,
        ))]),
    }
}

/// A hand-written extern type → a resource carrying ONLY the bytes seam.
///
/// No constructor and no getters: the tool knows nothing about the user's type beyond the contract
/// it already imposes on it, which is exactly `Serialize` (+ `Deserialize` where the crate reads
/// one) — the two traits the seam bridges. Emitting a bridge rather than excluding is what keeps a
/// CONTAINING record projectable: exclusion is transitive, so one extern would take every type that
/// reaches it out of the WIT.
///
/// The cbor seam and NOTHING else — in particular no JSON seam even under `--json-serde-derives`.
/// See [`json_members`]: the extern contract imposes the cbor traits and never serde.
fn project_extern_bridge(
    name: &str,
    ident: &RustIdent,
    deserializable: bool,
    cli: &Cli,
) -> WitResource {
    WitResource {
        name: name.to_owned(),
        ident: ident.clone(),
        constructor: None,
        members: bytes_members(deserializable, cli),
    }
}

/// A raw-bytes type → a resource carrying the RAW-bytes seam, and no cbor seam at all.
///
/// The deviation from the uniform bridging shape is forced by the contract: a
/// `_CDDL_CODEGEN_RAW_BYTES_TYPE_` is required to implement `RawBytesEncoding` and NOTHING requires
/// `Serialize` of it — the generated crate reads it through `to_raw_bytes()`/`from_raw_bytes()` at
/// every site and the emitted extern-interface self-check asserts `RawBytesEncoding` alone. So a
/// `to-cbor-bytes` bridge would name a trait impl that need not exist, which is a compile error in
/// generated code — the same class the `no_deserialize` fork exists to prevent.
///
/// Both halves are unconditional: `RawBytesEncoding` is ONE trait declaring both methods, so a type
/// satisfying the contract has both, and the `deserializable` verdict (a rust-face decision about
/// generated `Deserialize` impls) says nothing about a type the tool generates no impls for.
fn project_raw_bytes_bridge(name: &str, ident: &RustIdent) -> WitResource {
    WitResource {
        name: name.to_owned(),
        ident: ident.clone(),
        constructor: None,
        members: vec![
            WitMember {
                name: "to-raw-bytes".to_owned(),
                is_static: false,
                params: Vec::new(),
                result: Some(WitType::List(Box::new(WitType::U8))),
                fallible: false,
                op: WitMemberOp::ToRawBytes,
            },
            WitMember {
                name: "from-raw-bytes".to_owned(),
                is_static: true,
                params: vec![WitParam {
                    name: "bytes".to_owned(),
                    rust_name: "bytes".to_owned(),
                    ty: WitType::List(Box::new(WitType::U8)),
                    validates: false,
                    rust_type: None,
                }],
                // The `ok` type is the OWNING resource, filled in by the renderer and the emitter
                // exactly as `from-cbor-bytes`'s is.
                result: None,
                fallible: true,
                op: WitMemberOp::FromRawBytes,
            },
        ],
    }
}

/// A record → a `resource`. The surface MIRRORS the wasm face (the maintainer's
/// parallel-with-documented-deltas ruling): the constructor takes the mandatory non-fixed fields,
/// every non-fixed field gets a bare getter, and only OPTIONAL fields get a `set-` setter.
fn project_record(
    name: &str,
    ident: &RustIdent,
    record: &RustRecord,
    deserializable: bool,
    ctx: &mut TypeCtx,
) -> ProjectResult<WitResource> {
    let mut params = Vec::new();
    let mut members = Vec::new();
    let mut ctor_fallible = false;
    for field in &record.fields {
        let field_name = convert_to_kebab_case(&field.name);
        if field.rust_type.is_fixed_value() {
            // A mandatory fixed value carries no information and gets no accessor (the rust and wasm
            // faces agree). An OPTIONAL one stores its presence as a `bool`, which is real state.
            if field.optional {
                members.push(WitMember {
                    name: field_name.clone(),
                    is_static: false,
                    params: Vec::new(),
                    result: Some(WitType::Bool),
                    fallible: false,
                    op: WitMemberOp::PresenceGetter {
                        field: field.name.clone(),
                    },
                });
                members.push(WitMember {
                    name: format!("set-{field_name}"),
                    is_static: false,
                    params: vec![WitParam {
                        name: "present".to_owned(),
                        rust_name: "present".to_owned(),
                        ty: WitType::Bool,
                        validates: false,
                        rust_type: None,
                    }],
                    result: None,
                    fallible: false,
                    op: WitMemberOp::PresenceSetter {
                        field: field.name.clone(),
                    },
                });
            }
            continue;
        }
        let ty = map_rust_type(&field.rust_type, ctx)?;
        let validates = wit_param_validates(&field.rust_type, ctx.types);
        members.push(WitMember {
            name: field_name.clone(),
            is_static: false,
            params: Vec::new(),
            result: Some(if field.optional {
                WitType::Option(Box::new(ty.clone()))
            } else {
                ty.clone()
            }),
            fallible: false,
            op: WitMemberOp::Getter {
                field: field.name.clone(),
            },
        });
        if field.optional {
            // The setter takes the BARE type, as the wasm face's does — it sets a value, it never
            // clears one. The getter reports absence, so the two are deliberately asymmetric.
            members.push(WitMember {
                name: format!("set-{field_name}"),
                is_static: false,
                params: vec![WitParam {
                    name: field_name,
                    rust_name: field.name.clone(),
                    ty,
                    validates,
                    rust_type: Some(field.rust_type.clone()),
                }],
                result: None,
                fallible: validates,
                op: WitMemberOp::Setter {
                    field: field.name.clone(),
                },
            });
        } else {
            params.push(WitParam {
                name: field_name,
                rust_name: field.name.clone(),
                ty,
                validates,
                rust_type: Some(field.rust_type.clone()),
            });
            ctor_fallible |= validates;
        }
    }
    // An open struct's rest row: a getter over the captured content, mirroring the wasm face. An
    // `@ignore` row stores nothing, so it has no accessor at all.
    if let Some(rest) = &record.rest
        && rest.semantics == RestSemantics::Capture
    {
        let ty = match &rest.kind {
            RestKind::MapEntries { domain, range, .. } => {
                WitType::List(Box::new(WitType::Tuple(vec![
                    map_rust_type(domain, ctx)?,
                    map_rust_type(range, ctx)?,
                ])))
            }
            RestKind::ArrayTail { element } => {
                WitType::List(Box::new(map_rust_type(element, ctx)?))
            }
        };
        members.push(WitMember {
            name: convert_to_kebab_case(&rest.field_name),
            is_static: false,
            params: Vec::new(),
            result: Some(ty),
            fallible: false,
            op: WitMemberOp::RestGetter {
                field: rest.field_name.clone(),
            },
        });
    }
    members.extend(bytes_members(deserializable, ctx.cli));
    members.extend(json_members(ctx.cli));
    Ok(WitResource {
        name: name.to_owned(),
        ident: ident.clone(),
        constructor: Some(WitConstructor {
            params,
            fallible: ctor_fallible,
        }),
        members,
    })
}

/// A `@newtype` wrapper → a `resource` with a constructor and the inner-value getter. The getter's
/// name follows the rust one exactly (`@newtype <name>` renames both), and a SET NOMINAL emits no
/// bare `get` on the rust side, so neither does the WIT.
fn project_wrapper(
    name: &str,
    ident: &RustIdent,
    rust_struct: &RustStruct,
    wrapped: &RustType,
    bounded: bool,
    deserializable: bool,
    ctx: &mut TypeCtx,
) -> ProjectResult<WitResource> {
    let config = rust_struct.config();
    let getter = match config.newtype_getter.as_ref() {
        Some(Some(custom)) => Some(custom.clone()),
        _ if config.set_nominal => None,
        _ => Some("get".to_owned()),
    };
    let ty = map_rust_type(wrapped, ctx)?;
    let mut members = Vec::new();
    if let Some(getter) = getter {
        members.push(WitMember {
            name: convert_to_kebab_case(&getter),
            is_static: false,
            params: Vec::new(),
            result: Some(ty.clone()),
            fallible: false,
            op: WitMemberOp::WrapperGet { getter },
        });
    }
    members.extend(bytes_members(deserializable, ctx.cli));
    members.extend(json_members(ctx.cli));
    Ok(WitResource {
        name: name.to_owned(),
        ident: ident.clone(),
        constructor: Some(WitConstructor {
            params: vec![WitParam {
                name: "inner".to_owned(),
                rust_name: "inner".to_owned(),
                ty,
                // NOT `|| bounded`: a bound lives in the rust `new`'s own signature (`can_new_fail`),
                // so the guest calls a fallible constructor rather than a despecialization door.
                validates: wit_param_validates(wrapped, ctx.types),
                rust_type: Some(wrapped.clone()),
            }],
            fallible: bounded || wit_param_validates(wrapped, ctx.types),
        }),
        members,
    })
}

/// A c-style enum → a WIT `enum` VALUE type. Each case carries its rust variant ident: the WIT enum
/// type is minted by `wit_bindgen::generate!` and is a DISTINCT rust type from the crate's enum, so
/// the guest needs a per-case bridge in both directions and must not re-derive the pairing.
fn project_c_style_enum(name: &str, ident: &RustIdent, variants: &[EnumVariant]) -> WitEnum {
    WitEnum {
        name: name.to_owned(),
        ident: ident.clone(),
        cases: variants
            .iter()
            .map(|v| {
                let rust_variant = v.name.to_string();
                WitEnumCase {
                    name: convert_to_kebab_case(&rust_variant),
                    rust_variant,
                }
            })
            .collect(),
    }
}

/// The suffix a choice's discriminant enum carries. A user rule that converts to `<choice>-kind` is
/// a COLLISION the interface-level detector reports, exactly as one converging on `any-cbor` is.
const KIND_TYPE_SUFFIX: &str = "-kind";

/// A type or group choice → a `resource` with no constructor, one `new-<variant>` STATIC per
/// variant, a `kind` discriminant and one `as-<variant>` per variant that carries data — plus the
/// `<name>-kind` enum those three families are spelled from.
///
/// A choice has no single constructor (there is nothing to construct *without* picking an arm), so
/// the statics replace it. `<variant>` is `convert_to_kebab_case(variant.name_as_var())` for all
/// THREE families from one call, because the rust `new_<name_as_var()>` is what the guest bridges to
/// and a second spelling would drift from it.
fn project_choice(
    name: &str,
    ident: &RustIdent,
    variants: &[EnumVariant],
    rep: Option<Representation>,
    deserializable: bool,
    ctx: &mut TypeCtx,
) -> ProjectResult<Vec<WitTypeDef>> {
    let kind_name = format!("{name}{KIND_TYPE_SUFFIX}");
    let kind_ref = WitTypeRef {
        scope: ctx.types.scope(ident).clone(),
        name: kind_name.clone(),
        ident: ident.clone(),
    };
    let mut cases = Vec::new();
    let mut members = Vec::new();
    for variant in variants {
        let var = convert_to_kebab_case(&variant.name_as_var());
        cases.push(WitEnumCase {
            name: var.clone(),
            rust_variant: variant.name.to_string(),
        });
        let (params, payload) = choice_variant_shape(ident, variant, rep, ctx)?;
        let rust_can_fail = variant_ctor_can_fail(ident, variant, rep, ctx.types);
        // The MEMBER is fallible for a second, independent reason the rust ctor knows nothing
        // about: a despecialized parameter (`[+ T]`, `@duplicates reject`) has to re-enter its
        // `TryFrom` door here. So `rust_can_fail` implies member-fallible, never the reverse.
        let fallible = rust_can_fail || params.iter().any(|p| p.validates);
        members.push(WitMember {
            name: format!("new-{var}"),
            is_static: true,
            params,
            // The `ok` type is the OWNING resource, filled in by the renderer and the emitter for
            // the same reason `from-cbor-bytes`'s is.
            result: None,
            fallible,
            op: WitMemberOp::NewVariant {
                rust_ctor: format!("new_{}", variant.name_as_var()),
                rust_can_fail,
            },
        });
        // A FIXED-value arm carries no payload, so there is nothing for `as-` to hand back; `kind`
        // still reports it, which is the whole answer for such an arm.
        if let Some(payload) = payload {
            members.push(WitMember {
                name: format!("as-{var}"),
                is_static: false,
                params: Vec::new(),
                result: Some(WitType::Option(Box::new(payload))),
                fallible: false,
                op: WitMemberOp::AsVariant {
                    rust_variant: variant.name.to_string(),
                },
            });
        }
    }
    members.push(WitMember {
        name: "kind".to_owned(),
        is_static: false,
        params: Vec::new(),
        result: Some(WitType::Enum(kind_ref)),
        fallible: false,
        op: WitMemberOp::VariantKind,
    });
    members.extend(bytes_members(deserializable, ctx.cli));
    members.extend(json_members(ctx.cli));
    Ok(vec![
        WitTypeDef::Resource(WitResource {
            name: name.to_owned(),
            ident: ident.clone(),
            // No constructor: an arm has to be chosen, and the `new-<variant>` statics are how.
            constructor: None,
            members,
        }),
        WitTypeDef::Kind(WitEnum {
            name: kind_name,
            ident: ident.clone(),
            cases,
        }),
    ])
}

/// One choice arm's `new-<variant>` PARAMETERS and its `as-<variant>` PAYLOAD type (`None` for a
/// fixed-value arm, which has neither).
///
/// The parameter list mirrors the rust enum's own `new_<variant>` exactly, because that is what the
/// guest calls: a group-choice arm naming a RECORD takes the record's mandatory non-fixed FIELDS
/// (the rust ctor builds the record itself), an INLINED arm takes its non-fixed fields, and every
/// other arm takes the variant's own type under the variant's name.
fn choice_variant_shape(
    ident: &RustIdent,
    variant: &EnumVariant,
    rep: Option<Representation>,
    ctx: &mut TypeCtx,
) -> ProjectResult<(Vec<WitParam>, Option<WitType>)> {
    match &variant.data {
        EnumVariantData::RustType(ty) => {
            let ctor_fields = rep.and_then(|_| variant.group_ctor_record_fields(ctx.types, ident));
            let params = match &ctor_fields {
                Some(fields) => {
                    let mut params = Vec::new();
                    for field in fields {
                        params.push(field_param(&field.name, &field.rust_type, ctx)?);
                    }
                    params
                }
                None if ty.is_fixed_value() => Vec::new(),
                None => vec![WitParam {
                    name: convert_to_kebab_case(&variant.name_as_var()),
                    rust_name: variant.name_as_var(),
                    ty: map_rust_type(ty, ctx)?,
                    validates: wit_param_validates(ty, ctx.types),
                    rust_type: Some(ty.clone()),
                }],
            };
            let payload = if ty.is_fixed_value() {
                None
            } else {
                Some(map_rust_type(ty, ctx)?)
            };
            Ok((params, payload))
        }
        EnumVariantData::Inlined(record) => {
            let non_fixed: Vec<&RustField> = record
                .fields
                .iter()
                .filter(|f| !f.rust_type.is_fixed_value())
                .collect();
            // The wasm face ASSERTS `<= 1` here; this module never asserts (R2), so the shape its
            // assert guards against leaves as an exclusion record naming itself.
            if non_fixed.len() > 1 {
                return Err(unprojectable(format!(
                    "the inlined group-choice arm `{}` carries {} non-fixed fields, and an embedded \
                     arm's payload has no single WIT type",
                    variant.name,
                    non_fixed.len()
                )));
            }
            let mut params = Vec::new();
            for field in &non_fixed {
                params.push(field_param(
                    &field.name,
                    field.to_embedded_rust_type().as_ref(),
                    ctx,
                )?);
            }
            let payload = match non_fixed.first() {
                Some(field) => Some(map_rust_type(field.to_embedded_rust_type().as_ref(), ctx)?),
                None => None,
            };
            Ok((params, payload))
        }
    }
}

/// One `WitParam` for a rust FIELD feeding a positional rust constructor argument.
fn field_param(rust_name: &str, ty: &RustType, ctx: &mut TypeCtx) -> ProjectResult<WitParam> {
    Ok(WitParam {
        name: convert_to_kebab_case(rust_name),
        rust_name: rust_name.to_owned(),
        ty: map_rust_type(ty, ctx)?,
        validates: wit_param_validates(ty, ctx.types),
        rust_type: Some(ty.clone()),
    })
}

/// Whether the RUST enum's own `new_<variant>` returns a `Result`.
///
/// Mirrors `generation::enums`'s three per-shape rules verbatim, because a glue that guessed wrong
/// binds a `Result` where the rep expects the value — a type error in generated code no WIT gate can
/// see. In particular it is `has_value_bounds()` and NOT `needs_bounds_check_if_inlined()` on the
/// direct-payload arm: both ctors receive an already-constructed value, so a named bounded type's
/// own fallible `new` already ran upstream.
fn variant_ctor_can_fail(
    ident: &RustIdent,
    variant: &EnumVariant,
    rep: Option<Representation>,
    types: &IntermediateTypes,
) -> bool {
    match &variant.data {
        EnumVariantData::RustType(ty) => {
            match rep.and_then(|_| variant.group_ctor_record_fields(types, ident)) {
                Some(fields) => fields.iter().any(|f| f.rust_type.has_value_bounds()),
                None => !ty.is_fixed_value() && ty.has_value_bounds(),
            }
        }
        EnumVariantData::Inlined(record) => record
            .fields
            .iter()
            .any(|f| f.rust_type.needs_bounds_check_if_inlined(types)),
    }
}

/// The bytes seam every class-backed type carries. The `to-` half is emitted unconditionally — NOT
/// gated on `--to-from-bytes-methods` — because the cross-crate seam and the extern bridging rows
/// both depend on it (a per-face flag-semantics delta, plan §3).
///
/// The `from-` half is gated on `deserializable`, mirroring the wasm face's own
/// `if gen_scope.deserialize_generated(ident)` fork. A spec CAN reach a type the rust face declines
/// to give a `Deserialize` impl (an array struct whose optional field is CBOR-ambiguous with what
/// follows it), and a WIT declaring `from-cbor-bytes` for such a type forces glue that names a trait
/// impl which does not exist — a compile error in generated code.
fn bytes_members(deserializable: bool, cli: &Cli) -> Vec<WitMember> {
    let mut members = vec![WitMember {
        name: "to-cbor-bytes".to_owned(),
        is_static: false,
        params: Vec::new(),
        result: Some(WitType::List(Box::new(WitType::U8))),
        fallible: false,
        op: WitMemberOp::ToCborBytes,
    }];
    // The canonical re-encoding door, in the ONE posture whose composed runtime declares it. The
    // gate is the same pair the guest's `to_bytes_trait()` forks on, and it has to be: under
    // `--preserve-encodings --canonical-form` the blanket `ToCBORBytes` impl is not composed at all
    // and both methods live on `Serialize`, while every other posture composes a `ToCBORBytes` that
    // declares `to_cbor_bytes` alone. A row emitted outside that posture would name a trait method
    // the runtime does not have.
    if cli.preserve_encodings && cli.canonical_form {
        members.push(WitMember {
            name: "to-canonical-cbor-bytes".to_owned(),
            is_static: false,
            params: Vec::new(),
            result: Some(WitType::List(Box::new(WitType::U8))),
            fallible: false,
            op: WitMemberOp::ToCanonicalCborBytes,
        });
    }
    if deserializable {
        members.push(WitMember {
            name: "from-cbor-bytes".to_owned(),
            is_static: true,
            params: vec![WitParam {
                name: "bytes".to_owned(),
                rust_name: "bytes".to_owned(),
                ty: WitType::List(Box::new(WitType::U8)),
                validates: false,
                rust_type: None,
            }],
            // The `ok` type is the OWNING resource, which a member cannot name without carrying its
            // own owner; `op` already says so, and the renderer fills it in.
            result: None,
            fallible: true,
            op: WitMemberOp::FromCborBytes,
        });
    }
    members
}

/// The JSON seam, under `--json-serde-derives` — the sibling of [`bytes_members`] and deliberately
/// NOT part of it, because the two seams are owed by different sets of types.
///
/// `bytes_members` is called by the extern bridge as well, and legitimately: the extern contract the
/// tool already imposes on a user's type IS `Serialize` (+ `Deserialize`), which is exactly what the
/// cbor seam bridges, and the emitted `extern_interface_check.rs` asserts it. NOTHING imposes serde
/// on an extern — the self-check asserts the cbor traits and `RawBytesEncoding`, never
/// `serde::Serialize` — so a `to-json` on a bridging resource would name a trait impl that need not
/// exist. That is the compile-error-in-generated-code class the `no_deserialize` fork and the
/// raw-bytes seam split both exist to prevent, reached a third time. The seam therefore goes only to
/// the types the tool DEFINES (records, `@newtype` wrappers, choices), whose serde impls it derives
/// itself under the same flag.
///
/// Both halves are FALLIBLE. `from-json` obviously so; `to-json` because the wasm face's own
/// `to_json` is `Result<String, JsError>` and serialization genuinely can fail — the runtime's
/// `AnyCbor` serde fragment documents a "key must be a string" failure for a non-string-keyed table,
/// which reaches any type that holds one.
fn json_members(cli: &Cli) -> Vec<WitMember> {
    if !cli.json_serde_derives {
        return Vec::new();
    }
    vec![
        WitMember {
            name: TO_JSON_MEMBER_NAME.to_owned(),
            is_static: false,
            params: Vec::new(),
            result: Some(WitType::Str),
            fallible: true,
            op: WitMemberOp::ToJson,
        },
        WitMember {
            name: FROM_JSON_MEMBER_NAME.to_owned(),
            is_static: true,
            params: vec![WitParam {
                name: "json".to_owned(),
                rust_name: "json".to_owned(),
                ty: WitType::Str,
                validates: false,
                rust_type: None,
            }],
            // The `ok` type is the OWNING resource, filled in by the renderer and the emitter
            // exactly as `from-cbor-bytes`'s is.
            result: None,
            fallible: true,
            op: WitMemberOp::FromJson,
        },
    ]
}

/// Map one `RustType` occurrence to its WIT spelling.
fn map_rust_type(ty: &RustType, ctx: &mut TypeCtx) -> ProjectResult<WitType> {
    map_conceptual(&ty.conceptual_type, ctx)
}

/// Map one `ConceptualRustType` occurrence. EXHAUSTIVE with no `_ =>` arm: a new IR variant must be
/// an explicit projection decision, because silently falling through would emit a WIT that lies
/// about the rust surface.
fn map_conceptual(ty: &ConceptualRustType, ctx: &mut TypeCtx) -> ProjectResult<WitType> {
    match ty {
        ConceptualRustType::Primitive(p) => Ok(map_primitive(*p)),
        ConceptualRustType::Fixed(_) => Err(unprojectable(
            "a fixed value carries no data and has no WIT type",
        )),
        ConceptualRustType::Any => {
            ctx.uses_any_cbor = true;
            Ok(WitType::AnyCbor)
        }
        ConceptualRustType::Rust(ident) => map_named(ident, ctx),
        ConceptualRustType::Alias(AliasIdent::Rust(ident), base) => {
            // A CDDL type alias is RESOLVED THROUGH — never surfaced (plan §4). The exception is an
            // alias whose ident also names a projected struct (a named collection registers both),
            // which resolves through the STRUCT so the two paths agree.
            if ctx.types.rust_structs().contains_key(ident) {
                map_named(ident, ctx)
            } else {
                map_conceptual(base, ctx)
            }
        }
        ConceptualRustType::Alias(AliasIdent::Reserved(_), base) => map_conceptual(base, ctx),
        ConceptualRustType::Optional(inner) => {
            Ok(WitType::Option(Box::new(map_rust_type(inner, ctx)?)))
        }
        ConceptualRustType::Array(inner) => Ok(WitType::List(Box::new(map_rust_type(inner, ctx)?))),
        // Never `map<K, V>`: its key domain (int/char/bool/string) cannot carry a hash-keyed chain
        // map, and consumer tooling at the floor rejects it outright.
        ConceptualRustType::Map(key, value) => Ok(WitType::List(Box::new(WitType::Tuple(vec![
            map_rust_type(key, ctx)?,
            map_rust_type(value, ctx)?,
        ])))),
    }
}

/// Map a reference to a named rust type. Named collections resolve THROUGH; the reserved `Int`
/// extern becomes the `int` variant; everything class-backed becomes a handle or an enum and is
/// recorded as a reference for the closure and the `use` graph.
fn map_named(ident: &RustIdent, ctx: &mut TypeCtx) -> ProjectResult<WitType> {
    if ident.to_string() == INT_EXTERN_IDENT {
        ctx.uses_int = true;
        return Ok(WitType::Int);
    }
    let Some(rust_struct) = ctx.types.rust_struct(ident) else {
        return Err(unprojectable(format!(
            "references `{ident}`, which names no generated type"
        )));
    };
    let type_ref = || WitTypeRef {
        scope: ctx.types.scope(ident).clone(),
        name: wit_type_name(ident),
        ident: ident.clone(),
    };
    // A generic extern BASE names no concrete type, so there is nothing for a handle to point at.
    // Reported here rather than left to the reference closure: the closure can only say "references
    // excluded <ident>", and the base is not excluded — it is skipped, like a named collection.
    if ctx.generic_extern_bases.contains(ident)
        && matches!(rust_struct.variant(), RustStructType::Extern)
    {
        return Err(unprojectable(format!(
            "references the generic extern base `{ident}`, which names no concrete type — only its \
             instances (`{ident}<…>`) are bridged"
        )));
    }
    match rust_struct.variant() {
        RustStructType::Record(_)
        | RustStructType::Wrapper { .. }
        | RustStructType::TypeChoice { .. }
        | RustStructType::GroupChoice { .. }
        // An extern and a raw-bytes type are class-backed too: each has a bridging resource, so a
        // reference to one is an ordinary handle. Their SEAMS differ (cbor vs raw bytes) but that is
        // a property of the resource, invisible at a use site.
        | RustStructType::Extern
        | RustStructType::RawBytesType => {
            let r = type_ref();
            ctx.refs.insert(ident.clone());
            Ok(WitType::Handle(r))
        }
        RustStructType::CStyleEnum { .. } => {
            let r = type_ref();
            ctx.refs.insert(ident.clone());
            Ok(WitType::Enum(r))
        }
        RustStructType::Array { element_type, .. } => resolve_through(ident, ctx, |ctx| {
            Ok(WitType::List(Box::new(map_rust_type(element_type, ctx)?)))
        }),
        RustStructType::Table { domain, range, .. } => resolve_through(ident, ctx, |ctx| {
            Ok(WitType::List(Box::new(WitType::Tuple(vec![
                map_rust_type(domain, ctx)?,
                map_rust_type(range, ctx)?,
            ]))))
        }),
    }
}

/// Resolve a named collection through to its element spelling, refusing to re-enter one already on
/// the stack. A self-referential collection alias would otherwise recurse until the stack ends —
/// and a stack overflow is a panic, which this module does not do.
fn resolve_through(
    ident: &RustIdent,
    ctx: &mut TypeCtx,
    f: impl FnOnce(&mut TypeCtx) -> ProjectResult<WitType>,
) -> ProjectResult<WitType> {
    if !ctx.resolving.insert(ident.clone()) {
        return Err(unprojectable(format!(
            "references `{ident}`, a collection whose element type resolves back to itself"
        )));
    }
    let out = f(ctx);
    ctx.resolving.remove(ident);
    out
}

fn map_primitive(p: Primitive) -> WitType {
    match p {
        Primitive::Bool => WitType::Bool,
        Primitive::F32 => WitType::F32,
        Primitive::F64 => WitType::F64,
        Primitive::U8 => WitType::U8,
        Primitive::U16 => WitType::U16,
        Primitive::U32 => WitType::U32,
        Primitive::U64 => WitType::U64,
        Primitive::I8 => WitType::S8,
        Primitive::I16 => WitType::S16,
        Primitive::I32 => WitType::S32,
        Primitive::I64 => WitType::S64,
        // `nint` is a `u64` on the rust surface too (the magnitude of a negative number), so the WIT
        // matches it rather than inventing a signed spelling the rust side does not have.
        Primitive::N64 => WitType::U64,
        Primitive::Str => WitType::Str,
        Primitive::Bytes => WitType::List(Box::new(WitType::U8)),
    }
}

/// Whether a WIT parameter of this rust type must be VALIDATED at the boundary — i.e. whether the
/// projection dropped a constraint the rust type carries in its own type system.
///
/// Three sources: a value bound (`uint .size 2`, a float window), which makes the rust constructor
/// fallible too; a DESPECIALIZED collection — `[+ T]`'s `NonEmptyVec` and `@duplicates reject`'s
/// `OrderedSet` both become a plain `list<t>` in WIT, so the invariant their single `TryFrom` door
/// enforces has to be re-checked where the list is consumed; and CDDL `any`, which crosses as the
/// transparent `any-cbor` BYTE alias and comes back through `AnyCbor::from_cbor_bytes` at the
/// consuming door — the argument is arbitrary caller-supplied bytes carrying no type-system
/// invariant, so that decode IS the re-check and it has to be able to fail. A plain table is NOT in
/// this class: a `BTreeMap` carries no invariant a `list<tuple<K, V>>` can violate.
fn wit_param_validates(ty: &RustType, types: &IntermediateTypes) -> bool {
    if ty.has_value_bounds() || ty.is_type_enforced_non_empty() || ty.duplicates_reject() {
        return true;
    }
    // A field referencing a named `[+ …]` rule by a bare `Rust(ident)` (rather than through the
    // registered alias that carries the bounds) still despecializes, so read the bound off the
    // struct as well.
    if let ConceptualRustType::Rust(ident) = ty.conceptual_type.resolve_alias_shallow()
        && let Some(rust_struct) = types.rust_struct(ident)
        && matches!(
            rust_struct.variant(),
            RustStructType::Array {
                bounds: Some((Some(1), None)),
                ..
            } | RustStructType::Table {
                bounds: Some((Some(1), None)),
                ..
            }
        )
    {
        return true;
    }
    match &ty.conceptual_type {
        ConceptualRustType::Optional(inner) | ConceptualRustType::Array(inner) => {
            wit_param_validates(inner, types)
        }
        ConceptualRustType::Map(key, value) => {
            wit_param_validates(key, types) || wit_param_validates(value, types)
        }
        ConceptualRustType::Any => true,
        ConceptualRustType::Primitive(_)
        | ConceptualRustType::Fixed(_)
        | ConceptualRustType::Rust(_)
        | ConceptualRustType::Alias(_, _) => false,
    }
}

/// Whether the projection DESPECIALIZED this parameter's type — dropped an invariant the RUST type
/// enforces in its own type system (`[+ T]`'s `NonEmptyVec`, `@duplicates reject`'s `OrderedSet`),
/// so the single `TryFrom` door that owns the invariant has to be re-entered where the WIT list is
/// consumed.
///
/// Strictly narrower than [`wit_param_validates`], and deliberately its own function rather than a
/// reading of that one: `validates` is the UNION of despecialization, a value window and CDDL `any`,
/// and only the first has a `TryFrom` door at all. A plain bounded array (`[2*5 uint]`) is a
/// `Vec<T>` on both sides, so routing it through `try_into` resolves to the identity
/// `TryFrom<Vec<T>>` (`Error = Infallible`) — it compiles, and it checks nothing. A bounded MAP is
/// worse: `BTreeMap<K, V>` has no `TryFrom<Vec<(K, V)>>` at all, so the same conflation emitted glue
/// that did not compile.
pub(crate) fn wit_param_despecialized(ty: &RustType, types: &IntermediateTypes) -> bool {
    if ty.is_type_enforced_non_empty() || ty.duplicates_reject() {
        return true;
    }
    // A field referencing a named `[+ …]` rule by a bare `Rust(ident)` (rather than through the
    // registered alias that carries the bounds) still despecializes — the same second reading
    // `wit_param_validates` takes off the struct, for the same reason.
    if let ConceptualRustType::Rust(ident) = ty.conceptual_type.resolve_alias_shallow()
        && let Some(rust_struct) = types.rust_struct(ident)
        && matches!(
            rust_struct.variant(),
            RustStructType::Array {
                bounds: Some((Some(1), None)),
                ..
            } | RustStructType::Table {
                bounds: Some((Some(1), None)),
                ..
            }
        )
    {
        return true;
    }
    false
}

// =================================================================================================
// Rendering
// =================================================================================================

/// Render a projected package to its `.wit` files, keyed by path relative to `<output>`.
///
/// One file today (`component/wit/world.wit`): WIT resolves a whole DIRECTORY as one package, so the
/// split into files is presentational, and one file keeps the cross-interface `use` edges readable
/// in the order the projection computed them.
pub(crate) fn render(package: &WitPackage) -> BTreeMap<String, String> {
    let mut out = String::new();
    out.push_str(&format!("package {};\n", package.id));
    for iface in package.interfaces.values() {
        out.push('\n');
        out.push_str(&format!("interface {} {{\n", wit_escape(&iface.name)));
        for (target, names) in &iface.uses {
            let names = names
                .iter()
                .map(|n| wit_escape(n))
                .collect::<Vec<_>>()
                .join(", ");
            out.push_str(&format!("  use {}.{{{names}}};\n", wit_escape(target)));
        }
        // Exclusion records, sorted by ident: the direct analog of the extern-interface export's
        // `; unexported:` rows, and the carrier a cross-crate consumer reads a reason from.
        for (ident, exc) in &package.excluded {
            if exc.scope == iface.scope {
                out.push_str(&format!("  // unexported: {ident} — {}\n", exc.reason));
            }
        }
        for def in &iface.types {
            out.push_str(&render_type_def(def));
        }
        for func in &iface.funcs {
            out.push_str(&format!(
                "  {}: func({}){};\n",
                wit_escape(&func.name),
                render_params(&func.params),
                render_result(func.result.as_ref(), func.fallible)
            ));
        }
        out.push_str("}\n");
    }
    out.push('\n');
    out.push_str(&format!("world {} {{\n", wit_escape(&package.world)));
    for iface in package.interfaces.values() {
        out.push_str(&format!("  export {};\n", wit_escape(&iface.name)));
    }
    out.push_str("}\n");

    let mut files = BTreeMap::new();
    files.insert(
        format!("{}/world.wit", crate::generation::layout::COMPONENT_WIT_DIR),
        out,
    );
    files
}

fn render_type_def(def: &WitTypeDef) -> String {
    let mut out = String::new();
    match def {
        WitTypeDef::Resource(resource) => {
            out.push_str(&format!("  resource {} {{\n", wit_escape(&resource.name)));
            if let Some(ctor) = &resource.constructor {
                let ret = if ctor.fallible {
                    format!(" -> result<{}, string>", wit_escape(&resource.name))
                } else {
                    String::new()
                };
                out.push_str(&format!(
                    "    constructor({}){ret};\n",
                    render_params(&ctor.params)
                ));
            }
            for member in &resource.members {
                let statik = if member.is_static { "static " } else { "" };
                out.push_str(&format!(
                    "    {}: {statik}func({}){};\n",
                    wit_escape(&member.name),
                    render_params(&member.params),
                    render_member_result(member, &resource.name)
                ));
            }
            out.push_str("  }\n");
        }
        WitTypeDef::Enum(e) | WitTypeDef::Kind(e) => {
            out.push_str(&format!("  enum {} {{\n", wit_escape(&e.name)));
            for case in &e.cases {
                out.push_str(&format!("    {},\n", wit_escape(&case.name)));
            }
            out.push_str("  }\n");
        }
        WitTypeDef::IntVariant => {
            out.push_str(&format!("  variant {INT_TYPE_NAME} {{\n"));
            out.push_str("    uint(u64),\n");
            // Mirrors the rust crate's own `Int::new_nint` doc, deliberately verbatim: the bias is
            // the single most surprising fact about this type and a second wording would drift.
            out.push_str(
                "    // a negative `x` here would be `|x + 1|` due to CBOR's `nint` encoding e.g. \
                 to represent -5, pass in 4\n",
            );
            out.push_str("    nint(u64),\n");
            out.push_str("  }\n");
        }
        WitTypeDef::AnyCborAlias => {
            out.push_str(&format!("  type {ANY_CBOR_TYPE_NAME} = list<u8>;\n"));
        }
        WitTypeDef::AnyCborKind => {
            out.push_str(&format!("  enum {ANY_CBOR_KIND_TYPE_NAME} {{\n"));
            for (case, _) in ANY_CBOR_KIND_CASES {
                out.push_str(&format!("    {},\n", wit_escape(case)));
            }
            out.push_str("  }\n");
        }
    }
    out
}

fn render_params(params: &[WitParam]) -> String {
    params
        .iter()
        .map(|p| format!("{}: {}", wit_escape(&p.name), render_type(&p.ty, true)))
        .collect::<Vec<_>>()
        .join(", ")
}

/// A member's return spelling. The members that mint the OWNING resource — `from-cbor-bytes`,
/// `from-raw-bytes`, `from-json` and a choice's `new-<variant>` — cannot name it from inside the
/// member without carrying their own owner, so the renderer fills it in from the resource it is
/// rendering.
fn render_member_result(member: &WitMember, owner: &str) -> String {
    let ok = match (&member.op, &member.result) {
        (
            WitMemberOp::FromCborBytes
            | WitMemberOp::FromRawBytes
            | WitMemberOp::FromJson
            | WitMemberOp::NewVariant { .. },
            _,
        ) => Some(wit_escape(owner)),
        (_, Some(ty)) => Some(render_type(ty, false)),
        (_, None) => None,
    };
    render_arrow(ok, member.fallible)
}

fn render_result(ok: Option<&WitType>, fallible: bool) -> String {
    render_arrow(ok.map(|ty| render_type(ty, false)), fallible)
}

/// The ` -> …` tail of a function signature, INCLUDING the arrow — empty when the function returns
/// nothing and cannot fail, which WIT spells by omitting the arrow rather than by a unit type.
fn render_arrow(ok: Option<String>, fallible: bool) -> String {
    match (ok, fallible) {
        (Some(ok), true) => format!(" -> result<{ok}, string>"),
        (Some(ok), false) => format!(" -> {ok}"),
        (None, true) => " -> result<_, string>".to_owned(),
        (None, false) => String::new(),
    }
}

/// Render a type at a use site. `param` selects the OWNERSHIP: every parameter position borrows a
/// composite (`borrow<t>`, `list<borrow<t>>`) and every return position mints a fresh `own`. The
/// canonical ABI CONSUMES an `own` handle passed as an argument, so getting this backwards would
/// silently destroy caller-held objects — and `borrow` in return position is rejected at resolve,
/// so the mirrored mistake fails the validity gate loudly.
fn render_type(ty: &WitType, param: bool) -> String {
    match ty {
        WitType::Bool => "bool".to_owned(),
        WitType::U8 => "u8".to_owned(),
        WitType::U16 => "u16".to_owned(),
        WitType::U32 => "u32".to_owned(),
        WitType::U64 => "u64".to_owned(),
        WitType::S8 => "s8".to_owned(),
        WitType::S16 => "s16".to_owned(),
        WitType::S32 => "s32".to_owned(),
        WitType::S64 => "s64".to_owned(),
        WitType::F32 => "f32".to_owned(),
        WitType::F64 => "f64".to_owned(),
        WitType::Str => "string".to_owned(),
        WitType::List(inner) => format!("list<{}>", render_type(inner, param)),
        WitType::Tuple(inner) => format!(
            "tuple<{}>",
            inner
                .iter()
                .map(|t| render_type(t, param))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        WitType::Option(inner) => format!("option<{}>", render_type(inner, param)),
        WitType::Handle(r) => {
            if param {
                format!("borrow<{}>", wit_escape(&r.name))
            } else {
                wit_escape(&r.name)
            }
        }
        WitType::Enum(r) => wit_escape(&r.name),
        WitType::Int => INT_TYPE_NAME.to_owned(),
        WitType::AnyCbor => ANY_CBOR_TYPE_NAME.to_owned(),
        WitType::AnyCborKind => ANY_CBOR_KIND_TYPE_NAME.to_owned(),
    }
}

/// The emitted `.wit` files for a spec, keyed by path relative to `<output>`.
///
/// INFALLIBLE, exactly like `extern_interface_files`: everything unrenderable is excluded-with-record
/// (R5), so a spec carrying a phase-2 type class still regenerates cleanly and the gap is visible in
/// the emitted file rather than as a generation failure.
///
/// `no_deserialize` is the set of idents the rust face declined to give a `Deserialize` impl — a
/// GENERATION-time verdict, so it arrives from the caller rather than being re-derived here (see
/// [`bytes_members`]).
pub(crate) fn wit_files(
    types: &IntermediateTypes,
    cli: &Cli,
    no_deserialize: &BTreeSet<RustIdent>,
) -> BTreeMap<String, String> {
    render(&project(types, cli, no_deserialize))
}

/// Strong-uniqueness collisions in the WIT surface, one message per collision.
///
/// WIT compares names after stripping the `[method]`/`[static]`/`[constructor]` prefixes, and an
/// interface is one flat namespace, so three levels can collide: interface names against the world
/// name at PACKAGE level, type names AND free-function names within one INTERFACE, and member names
/// (including a member equal to the resource's own name) within one RESOURCE. All three fall out of one walk of the
/// projection, which is why they are one function with three message shapes rather than three
/// sibling detectors — the AGENTS.md parallel-sibling ruling is about the wasm WRAPPER-name family,
/// whose members have genuinely different inputs.
///
/// The resource-level check is the one that cannot be delegated to the validity gate: a
/// `transaction.transaction` collision survives `wit-parser` resolve AND `wit_component::encode`,
/// failing only at component validation — so without this detector the user's first sighting is a
/// wasm-level error naming a mangled `[method]` symbol.
pub(crate) fn wit_name_collisions(
    types: &IntermediateTypes,
    cli: &Cli,
    no_deserialize: &BTreeSet<RustIdent>,
) -> Vec<String> {
    // Projected against the REAL no-deserialize verdict, which is why this runs at GENERATION time
    // rather than at IR finalization beside `wit_scope_cycles`. Finalization would have to project
    // with an empty set — the superset of members — and that over-reports for real: a type that gets
    // no `Deserialize` impl AND carries a field named `from_cbor_bytes` would be rejected for a
    // collision between a getter and a static the tool never emits.
    let package = project(types, cli, no_deserialize);
    let mut msgs = Vec::new();

    // 1 — package level. Interfaces and the world share one namespace, and the scope flattening
    // (`a::c` → `a-c`) is not injective.
    let mut package_names: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for iface in package.interfaces.values() {
        package_names
            .entry(iface.name.clone())
            .or_default()
            .push(format!("the interface for scope `{}`", iface.scope));
    }
    package_names
        .entry(package.world.clone())
        .or_default()
        .push(format!("the world (from --lib-name `{}`)", cli.lib_name));
    for (name, owners) in &package_names {
        if owners.len() < 2 {
            continue;
        }
        msgs.push(format!(
            "WIT package name collision under --component: {owners} all convert to the WIT \
             identifier `{name}`. A WIT package's interfaces and its world share ONE namespace, and \
             the flattening of a nested module scope (`a::c` → `a-c`) is not injective, so the \
             emitted package would declare that name twice and would not resolve. Rename an input \
             file so the scopes differ after conversion, or move the world off the name with \
             --lib-name.",
            owners = owners.join(" and ")
        ));
    }

    // 2 — interface level. One flat namespace per interface, holding the TYPES **and** the free
    // FUNCTIONS: `wit-parser` refuses a package where an interface declares both under one name
    // ("name `x` is defined more than once"), at RESOLVE. So the synthesized `any-cbor` doors belong
    // in this map beside the types — otherwise a rule converging on one of their names reaches the
    // user as a parse failure against a file they did not write, which is exactly the sighting this
    // detector exists to replace.
    for iface in package.interfaces.values() {
        let mut type_names: BTreeMap<&str, Vec<String>> = BTreeMap::new();
        for def in &iface.types {
            type_names.entry(def.name()).or_default().push(match def {
                WitTypeDef::Resource(r) => format!("the type `{}`", r.ident),
                WitTypeDef::Enum(e) => format!("the type `{}`", e.ident),
                WitTypeDef::Kind(e) => format!("the discriminant enum of the choice `{}`", e.ident),
                WitTypeDef::IntVariant => "the `int` variant".to_owned(),
                WitTypeDef::AnyCborAlias => "the `any-cbor` alias".to_owned(),
                WitTypeDef::AnyCborKind => "the `any-cbor-kind` enum".to_owned(),
            });
        }
        for func in &iface.funcs {
            type_names
                .entry(func.name.as_str())
                .or_default()
                .push(format!("the free function `{}`", func.name));
        }
        for (name, owners) in &type_names {
            if owners.len() < 2 {
                continue;
            }
            msgs.push(format!(
                "WIT type name collision under --component: {owners} all convert to the WIT \
                 identifier `{name}` in interface `{iface}`. A WIT interface is ONE flat namespace, \
                 so the emitted interface would declare that name twice and would not resolve. \
                 Rename one of the colliding rules in the CDDL spec itself — a rule's name never \
                 reaches the wire, so the rename changes no encoding. (`@name` cannot rename a \
                 top-level rule, so it is not the remedy here.)",
                owners = owners.join(" and "),
                iface = iface.name
            ));
        }
    }

    // 3 — resource level. Members share one namespace WITH the resource's own name.
    for iface in package.interfaces.values() {
        for def in &iface.types {
            let WitTypeDef::Resource(resource) = def else {
                continue;
            };
            let mut member_names: BTreeMap<&str, Vec<String>> = BTreeMap::new();
            member_names
                .entry(resource.name.as_str())
                .or_default()
                .push("the resource's own name".to_owned());
            for member in &resource.members {
                member_names
                    .entry(member.name.as_str())
                    .or_default()
                    .push(format!("the member `{}`", member.name));
            }
            for (name, owners) in &member_names {
                if owners.len() < 2 {
                    continue;
                }
                msgs.push(format!(
                    "WIT resource member collision under --component: {owners} in resource \
                     `{resource}` (interface `{iface}`) all convert to the WIT identifier `{name}`. \
                     WIT compares member names after stripping the \
                     `[method]`/`[static]`/`[constructor]` prefixes, and a resource may not carry a \
                     member named after the resource itself, so the emitted package RESOLVES and \
                     even ENCODES but fails component validation. Rename the field with the `@name` \
                     comment-DSL directive, which renames the generated accessor without touching \
                     the spec's wire format.",
                    owners = owners.join(" and "),
                    resource = resource.name,
                    iface = iface.name
                ));
            }
        }
    }

    msgs
}

/// Why `name` cannot be a WIT identifier, or `None` if it can.
///
/// The component face converts `--lib-name` into the default WIT package name and the world name,
/// and `--lib-name` has no `value_parser` — a cargo package name may legally begin with a digit,
/// which [`convert_to_kebab_case`] refuses with an `assert!`. Flag problems are graceful errors in
/// this tool, never panics, so `api::validate_flag_combinations` consults this BEFORE any converter
/// runs.
pub(crate) fn wit_identifier_problem(name: &str) -> Option<String> {
    let snake = crate::utils::convert_to_snake_case(name);
    let words: Vec<&str> = snake.split('_').filter(|w| !w.is_empty()).collect();
    let Some(first) = words.first() else {
        return Some("it converts to an empty WIT identifier".to_owned());
    };
    if first.starts_with(|c: char| c.is_ascii_digit()) {
        return Some(format!(
            "it converts to the digit-led WIT identifier `{first}…`, and a WIT identifier's first \
             word must start with a letter"
        ));
    }
    if let Some(c) = snake
        .chars()
        .find(|c| !matches!(c, 'a'..='z' | '0'..='9' | '_'))
    {
        return Some(format!(
            "it carries the character {c:?}, and a WIT identifier is ASCII kebab-case ([a-z0-9-])"
        ));
    }
    None
}

/// Cross-scope reference CYCLES, one message per non-trivial strongly-connected component.
///
/// Each exported `ModuleScope` becomes one WIT `interface`, and a type in one interface reaching a
/// type in another is a `use` — which WIT requires to be ACYCLIC. The rule is interface-level and
/// therefore strictly stronger than a type-level one: `A.x → B.y` together with `B.z → A.w` is no
/// type cycle at all, but it is an interface cycle. cddl-codegen accepts such a spec today on the
/// rust face (rust modules may reference each other freely), so this is a `--component`-only
/// restriction and it must be reported as one.
///
/// Deliberately NOT built on `dep_graph::find_references`: that walks the AST rather than the IR,
/// and its cycle handling is a warn-and-continue over RULES, which is the wrong granularity twice
/// over.
pub(crate) fn wit_scope_cycles(types: &IntermediateTypes, _cli: &Cli) -> Vec<String> {
    // scope -> scope -> one witness edge (`referencing type`, `referenced type`), kept for the
    // message. `BTreeMap` throughout: the message text is generated output and must be reproducible.
    let mut edges: BTreeMap<ModuleScope, BTreeMap<ModuleScope, (RustIdent, RustIdent)>> =
        BTreeMap::new();
    let generic_extern_bases = types.generic_extern_base_idents();
    for (ident, rust_struct) in types.rust_structs() {
        if types.source_rule_name(ident).is_none() {
            continue;
        }
        // A named collection is skipped for the same reason `project` skips it: it surfaces no WIT
        // type, so it owns nothing in any interface and can be neither end of a `use` edge. Its
        // element's scope is reached from each USE site instead, through `collect_projected_refs`.
        // Keeping the rule's own edges here invents a `<collection scope> -> <element scope>` edge
        // the emitted WIT never carries, which is enough to close a false cycle.
        if matches!(
            rust_struct.variant(),
            RustStructType::Array { .. } | RustStructType::Table { .. }
        ) {
            continue;
        }
        let from = types.scope(ident);
        if !from.export() {
            continue;
        }
        for referenced in struct_rule_refs(rust_struct.variant(), types, &generic_extern_bases) {
            let to = types.scope(&referenced);
            if !to.export() || to == from {
                continue;
            }
            edges
                .entry(from.clone())
                .or_default()
                .entry(to.clone())
                .or_insert_with(|| (ident.clone(), referenced.clone()));
        }
    }

    let mut msgs = Vec::new();
    for component in strongly_connected_components(&edges) {
        if component.len() < 2 {
            continue;
        }
        let scopes = component
            .iter()
            .map(|scope| format!("`{scope}`"))
            .collect::<Vec<_>>()
            .join(", ");
        let mut witnesses = Vec::new();
        for from in &component {
            for to in &component {
                if let Some((referencing, referenced)) =
                    edges.get(from).and_then(|targets| targets.get(to))
                {
                    witnesses.push(format!(
                        "`{referencing}` in `{from}` references `{referenced}` in `{to}`"
                    ));
                }
            }
        }
        msgs.push(format!(
            "WIT interface cycle under --component: the scopes {scopes} reference each other \
             ({witnesses}). Each scope becomes one WIT interface, and WIT requires interfaces \
             linked with `use` to be acyclic, so the emitted package would not resolve. Note the \
             rule is INTERFACE-level, not type-level — no single type has to be recursive for this \
             to fire. Move a type so the scopes are acyclic (a leaf scope both sides reference is \
             the usual fix), or put the spec in a single file so every type lands in one interface.",
            witnesses = witnesses.join("; ")
        ));
    }
    msgs
}

/// The rules a struct references DIRECTLY — its own member types only, never through another
/// struct. The scope graph needs direct edges; a walk that recursed into referenced structs would
/// build the transitive closure, whose witness edges no longer name a reference the user wrote.
///
/// The match is EXHAUSTIVE with no `_ =>` arm (module discipline): a new `RustStructType` variant
/// must be a compile-time decision here, because silently contributing no edges would turn a real
/// cycle into a spec that generates unresolvable WIT.
fn struct_rule_refs(
    variant: &RustStructType,
    types: &IntermediateTypes,
    generic_extern_bases: &BTreeSet<RustIdent>,
) -> BTreeSet<RustIdent> {
    let mut out = BTreeSet::new();
    let mut walk = |ty: &RustType| {
        collect_projected_refs(&ty.conceptual_type, types, &mut BTreeSet::new(), &mut out)
    };
    match variant {
        RustStructType::Record(record) => {
            for field in &record.fields {
                walk(&field.rust_type);
            }
            if let Some(rest) = &record.rest {
                match &rest.kind {
                    RestKind::MapEntries { domain, range, .. } => {
                        walk(domain);
                        walk(range);
                    }
                    RestKind::ArrayTail { element } => walk(element),
                }
            }
        }
        RustStructType::Table { domain, range, .. } => {
            walk(domain);
            walk(range);
        }
        RustStructType::Array { element_type, .. } => walk(element_type),
        RustStructType::TypeChoice { variants }
        | RustStructType::GroupChoice { variants, .. }
        | RustStructType::CStyleEnum { variants } => {
            for variant in variants {
                match &variant.data {
                    EnumVariantData::RustType(ty) => walk(ty),
                    EnumVariantData::Inlined(record) => {
                        for field in &record.fields {
                            walk(&field.rust_type);
                        }
                    }
                }
            }
        }
        RustStructType::Wrapper { wrapped, .. } => walk(wrapped),
        // A hand-written extern and a raw-bytes type have no IR-visible members to reference.
        RustStructType::Extern | RustStructType::RawBytesType => {}
    }
    // A generic extern BASE owns no WIT type ([`project`] skips it), so it can be no end of a `use`
    // edge — the same rule a named collection gets, applied at the far end instead of the near one.
    // Dropping it from the result is exactly equivalent to resolving through it: an `Extern` has no
    // members, so it contributes nothing further of its own.
    out.retain(|ident| !generic_extern_bases.contains(ident));
    out
}

/// The rules one occurrence references, AS THE PROJECTION SEES THEM.
///
/// Modeled on `extern_interface::collect_rule_refs`, with the two deliberate differences the WIT
/// face forces, both of them the same rule: what the projection RESOLVES THROUGH is resolved through
/// here too, because an ident the emitted WIT never names owns nothing and can be no end of a `use`
/// edge. That covers a TRANSPARENT alias (whose real target is its base type) and a NAMED COLLECTION
/// (whose real target is its element/domain/range — see [`map_named`]'s `resolve_through`). An alias
/// that backs a projected NON-collection type is recorded as itself.
///
/// `resolving` is the same re-entry guard `resolve_through` carries, for the same reason: a
/// self-referential collection would otherwise recurse until the stack ends, and a stack overflow is
/// a panic.
fn collect_projected_refs(
    ty: &ConceptualRustType,
    types: &IntermediateTypes,
    resolving: &mut BTreeSet<RustIdent>,
    out: &mut BTreeSet<RustIdent>,
) {
    match ty {
        ConceptualRustType::Rust(ident) => collect_named_ref(ident, types, resolving, out),
        ConceptualRustType::Alias(AliasIdent::Rust(ident), base) => {
            if types.rust_structs().contains_key(ident) && types.source_rule_name(ident).is_some() {
                collect_named_ref(ident, types, resolving, out);
            } else {
                collect_projected_refs(base, types, resolving, out);
            }
        }
        // A reserved alias (`uint`, `text`, …) is a WIT primitive: it names no rule.
        ConceptualRustType::Alias(AliasIdent::Reserved(_), _) => {}
        ConceptualRustType::Optional(inner) | ConceptualRustType::Array(inner) => {
            collect_projected_refs(&inner.conceptual_type, types, resolving, out)
        }
        ConceptualRustType::Map(key, value) => {
            collect_projected_refs(&key.conceptual_type, types, resolving, out);
            collect_projected_refs(&value.conceptual_type, types, resolving, out);
        }
        // `any` projects to the self-contained `any-cbor` alias; primitives and fixed values to WIT
        // primitives. None of them names a rule.
        ConceptualRustType::Primitive(_)
        | ConceptualRustType::Fixed(_)
        | ConceptualRustType::Any => {}
    }
}

/// Record a reference to a NAMED rust type, resolving through a collection exactly as [`map_named`]
/// does so the two walks agree about which scope the emitted `use` edge actually points at.
fn collect_named_ref(
    ident: &RustIdent,
    types: &IntermediateTypes,
    resolving: &mut BTreeSet<RustIdent>,
    out: &mut BTreeSet<RustIdent>,
) {
    if types.source_rule_name(ident).is_none() {
        return;
    }
    let element = match types.rust_struct(ident).map(RustStruct::variant) {
        Some(RustStructType::Array { element_type, .. }) => vec![element_type.clone()],
        Some(RustStructType::Table { domain, range, .. }) => vec![domain.clone(), range.clone()],
        _ => {
            out.insert(ident.clone());
            return;
        }
    };
    if !resolving.insert(ident.clone()) {
        return;
    }
    for ty in &element {
        collect_projected_refs(&ty.conceptual_type, types, resolving, out);
    }
    resolving.remove(ident);
}

/// The strongly-connected components of a directed graph, as sorted node lists in a deterministic
/// order.
///
/// Computed by mutual reachability over the transitive closure rather than by Tarjan: the graph has
/// one node per module scope, i.e. one per input file, so the cubic closure is free — and the
/// closure form is short enough to be read for correctness at a glance, which matters more here
/// than an asymptote nothing will reach.
fn strongly_connected_components<N: Clone + Ord, E>(
    edges: &BTreeMap<N, BTreeMap<N, E>>,
) -> Vec<Vec<N>> {
    let nodes: BTreeSet<N> = edges
        .iter()
        .flat_map(|(from, targets)| std::iter::once(from).chain(targets.keys()))
        .cloned()
        .collect();
    // reach[a] = every node a can get to in one or more steps.
    let mut reach: BTreeMap<N, BTreeSet<N>> = nodes
        .iter()
        .map(|node| {
            let direct = edges
                .get(node)
                .map(|targets| targets.keys().cloned().collect())
                .unwrap_or_default();
            (node.clone(), direct)
        })
        .collect();
    loop {
        let mut grew = false;
        for node in &nodes {
            let expanded: BTreeSet<N> = reach[node]
                .iter()
                .flat_map(|next| reach[next].iter().cloned())
                .collect();
            let entry = reach.get_mut(node).expect("node came from the same map");
            for target in expanded {
                grew |= entry.insert(target);
            }
        }
        if !grew {
            break;
        }
    }
    let mut components: Vec<Vec<N>> = Vec::new();
    let mut assigned: BTreeSet<N> = BTreeSet::new();
    for node in &nodes {
        if assigned.contains(node) {
            continue;
        }
        let component: Vec<N> = nodes
            .iter()
            .filter(|other| {
                *other == node || (reach[node].contains(other) && reach[other].contains(node))
            })
            .cloned()
            .collect();
        assigned.extend(component.iter().cloned());
        components.push(component);
    }
    components
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wit_escape_escapes_the_union_of_the_keyword_sets() {
        // a keyword at every probed version
        assert_eq!(wit_escape("bool"), "%bool");
        // a keyword only at the NEWER end (wit-parser 0.247+), tolerated escaped at the older one
        assert_eq!(wit_escape("map"), "%map");
        // a keyword only at the OLDER end (wit-parser 0.240's require_f32_f64 compatibility arm)
        assert_eq!(wit_escape("float32"), "%float32");
        assert_eq!(wit_escape("float64"), "%float64");
        // ordinary names are untouched
        assert_eq!(wit_escape("transaction"), "transaction");
        assert_eq!(wit_escape("to-cbor-bytes"), "to-cbor-bytes");
        // `record` is a keyword and is exactly the kind of name a CDDL rule carries
        assert_eq!(wit_escape("record"), "%record");
    }

    #[test]
    fn wit_keywords_is_sorted_and_deduplicated() {
        // The list is maintained by UNION across toolchain versions, so it is read and edited by
        // hand; sortedness is what keeps a merge from silently duplicating an entry.
        let sorted: Vec<&str> = {
            let mut v = WIT_KEYWORDS.to_vec();
            v.sort_unstable();
            v.dedup();
            v
        };
        assert_eq!(sorted, WIT_KEYWORDS.to_vec());
        // `_` is deliberately absent — see the const's doc.
        assert!(!WIT_KEYWORDS.contains(&"_"));
    }

    #[test]
    fn wit_package_id_default_and_parse() {
        assert_eq!(
            WitPackageId::default_for_lib_name("cml-chain").to_string(),
            "cddl:cml-chain@0.1.0"
        );
        // --lib-name is a cargo package name, so it can carry casing the WIT identifier cannot
        assert_eq!(
            WitPackageId::default_for_lib_name("CMLChain").to_string(),
            "cddl:cml-chain@0.1.0"
        );
        assert_eq!(
            WitPackageId::parse("acme:widgets@1.2.3")
                .unwrap()
                .to_string(),
            "acme:widgets@1.2.3"
        );
        // the version is optional and defaults to the same 0.1.0 the derived default carries
        assert_eq!(
            WitPackageId::parse("acme:widgets").unwrap().to_string(),
            "acme:widgets@0.1.0"
        );
        // pre-release / build metadata rides through: WIT accepts it, we interpret none of it
        assert_eq!(
            WitPackageId::parse("acme:widgets@0.1.0-rc.1")
                .unwrap()
                .to_string(),
            "acme:widgets@0.1.0-rc.1"
        );
    }

    #[test]
    fn wit_package_id_rejects_malformed_values() {
        for bad in [
            "widgets",          // no namespace
            ":widgets",         // empty namespace
            "acme:",            // empty name
            "Acme:widgets",     // uppercase is not a WIT identifier
            "acme:wid_gets",    // underscores are not WIT word separators
            "acme:-widgets",    // leading separator
            "acme:widgets-",    // trailing separator
            "acme:wid--gets",   // empty word
            "acme:0widgets",    // digit-led identifier
            "acme:widgets@",    // empty version
            "acme:widgets@1",   // not <major>.<minor>.<patch>
            "acme:widgets@1.2", // ditto
            "acme:widgets@a.b.c",
        ] {
            assert!(
                WitPackageId::parse(bad).is_err(),
                "--wit-package {bad:?} should have been rejected"
            );
        }
    }

    /// The edge payload the real graph carries is the witness pair; the SCC computation ignores it,
    /// so these graphs carry `()` and stay readable.
    fn graph(
        edges: &[(&'static str, &'static str)],
    ) -> BTreeMap<&'static str, BTreeMap<&'static str, ()>> {
        let mut out: BTreeMap<&str, BTreeMap<&str, ()>> = BTreeMap::new();
        for (from, to) in edges {
            out.entry(from).or_default().insert(to, ());
        }
        out
    }

    #[test]
    fn strongly_connected_components_finds_mutual_reachability_not_just_two_cycles() {
        // a -> b -> c -> a, plus an acyclic tail a -> d. The three-node cycle must come back as ONE
        // component: a detector that only looked for two-node back-edges would miss it, and that is
        // exactly the shape a three-file spec produces.
        let mut components = strongly_connected_components(&graph(&[
            ("a", "b"),
            ("a", "d"),
            ("b", "c"),
            ("c", "a"),
        ]));
        components.sort();
        assert_eq!(components, vec![vec!["a", "b", "c"], vec!["d"]]);
    }

    #[test]
    fn strongly_connected_components_is_trivial_on_an_acyclic_graph() {
        let components = strongly_connected_components(&graph(&[("a", "b"), ("b", "c")]));
        assert!(
            components.iter().all(|c| c.len() == 1),
            "an acyclic graph has no non-trivial SCC, got {components:?}"
        );
    }

    #[test]
    fn strongly_connected_components_separates_two_independent_cycles() {
        // Two disjoint cycles must be two components, not one merged blob — the message names the
        // scopes in ONE cycle, and merging them would send the user looking for a reference that
        // does not exist.
        let mut components = strongly_connected_components(&graph(&[
            ("a", "b"),
            ("b", "a"),
            ("c", "d"),
            ("d", "c"),
        ]));
        components.sort();
        assert_eq!(components, vec![vec!["a", "b"], vec!["c", "d"]]);
    }
}
