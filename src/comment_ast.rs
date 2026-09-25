use std::collections::BTreeSet;

use nom::{
    IResult, Parser,
    bytes::complete::{take_while, take_while1},
    multi::many0,
};

/// The comparison/hash trait "flavor" a `@used_as_key` tag demands. Fields OR-merge (like the other
/// boolean metadata flags), so two comment lines — or two flavor words on one tag — union. Demand is
/// therefore a monotone union: a flavor can only ADD derives on top of internal demand, never remove.
///
/// - `bare`: bare `@used_as_key` — today's mode-dependent full internal bundle
///   (`Eq/PartialEq/Ord/PartialOrd`, plus `Hash` under `--preserve-encodings`).
/// - `hash`: `@used_as_key hash` — `Hash, Eq, PartialEq` (mode-INdependent: external downstream
///   `HashMap` demand exists regardless of the encoding flags).
/// - `ord`: `@used_as_key ord` — `Ord, PartialOrd, Eq, PartialEq` (mode-independent).
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DemandSet {
    pub bare: bool,
    pub hash: bool,
    pub ord: bool,
}

impl DemandSet {
    /// The demand of a bare `@used_as_key` tag.
    pub const BARE: DemandSet = DemandSet {
        bare: true,
        hash: false,
        ord: false,
    };

    pub fn union(self, other: DemandSet) -> DemandSet {
        DemandSet {
            bare: self.bare || other.bare,
            hash: self.hash || other.hash,
            ord: self.ord || other.ord,
        }
    }
}

/// Field-wise OR of two optional demand sets (None = no `@used_as_key` tag at all).
fn merge_key_demand(a: Option<DemandSet>, b: Option<DemandSet>) -> Option<DemandSet> {
    match (a, b) {
        (Some(x), Some(y)) => Some(x.union(y)),
        (x @ Some(_), None) => x,
        (None, y) => y,
    }
}

/// The per-rule duplicate-handling policy a `@duplicates` directive selects for a set/array/table
/// collection rule.
///
/// - `Preserve`: accept duplicate entries on the wire and re-emit them byte-exactly (the contract is
///   preservation, not merely "allow"). This is today's default for the tag-258 set idiom.
/// - `Reject`: duplicates are a `DeserializeFailure::DuplicateKey` on decode AND unconstructable
///   through the API. This is today's default for tables.
///
/// Unlike the boolean flags, the two values are mutually exclusive — a rule has at most one policy —
/// so a SECOND `@duplicates` on the same rule is the duplicate-key panic (like `@name`/`@rust_name`),
/// not a union.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DuplicatesPolicy {
    Preserve,
    Reject,
}

/// The `@extern_companions <path>=<Class>[,<Class>…]` declaration on a LOCALLY-declared marker rule
/// (`_CDDL_CODEGEN_EXTERN_TYPE_` or `_CDDL_CODEGEN_RAW_BYTES_TYPE_` — both name a type this crate
/// does not define, and both have their companions minted from their own ident): the sibling wasm
/// crate (or module path) where the named STRUCTURAL companion classes of this
/// type already exist, so the generator references them instead of minting duplicates.
///
/// `path_prefix` is emitted verbatim as the `use <prefix>::<Class>;` head; `classes` are the exact
/// generator-derived structural class names that defer (`TransactionMetadatumList`,
/// `MapFooToBar`, …). Only LISTED names defer — an unlisted structural companion of the same type
/// still mints locally, which is what lets a consumer borrow one family and own another.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternCompanions {
    pub path_prefix: String,
    pub classes: BTreeSet<String>,
}

/// One codec-visible encoding VARIABLE a `@custom_encodings` declaration names, in the order the
/// custom codec's signature takes them. The vocabulary is the subset of the generator's own encoding
/// variable types that a hand-written codec can meaningfully own — see
/// `docs/docs/comment_dsl.mdx` § "Declaring the wire's encoding variables" for the table and for the
/// aggregate / `TagPresenceEncoding` kinds deliberately left undeclarable.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum EncodingKind {
    /// `sz` → `Option<cbor_event::Sz>`: how an int (or a tag) head was sized.
    Sz,
    /// `str` → `StringEncoding`: how a text/bytes header was written (definite width, or the
    /// indefinite chunk lengths).
    Str,
    /// `len` → `LenEncoding`: how a container's length header was written.
    Len,
}

impl EncodingKind {
    /// The `@custom_encodings` token that spells this kind (the parser's vocabulary, surfaced for
    /// the rejection messages so they can never drift from what is accepted).
    pub fn token(self) -> &'static str {
        match self {
            Self::Sz => "sz",
            Self::Str => "str",
            Self::Len => "len",
        }
    }

    /// Every kind, in the order the docs table lists them. The parser's accepted vocabulary.
    pub const ALL: &'static [EncodingKind] = &[Self::Sz, Self::Str, Self::Len];
}

/// The CBOR major type a `@custom_wire_major` declaration names — the second member of the
/// wire-facts declaration family (`@custom_encodings` is the first). Both exist for the same reason:
/// a `@custom_serialize`/`@custom_deserialize` pair OWNS the wire, so the generator's inference over
/// the type the codec replaces answers about a wire nobody writes. Where `@custom_encodings` declares
/// the codec's framing VARIABLES, this declares the one fact a dispatching reader needs before any
/// deserializer runs: which of the eight majors the codec's first data item is.
///
/// The vocabulary is surfaced from the enum (`ALL` / `token()`) so the rejection messages can never
/// drift from what parses.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum WireMajor {
    Uint,
    Nint,
    Bytes,
    Text,
    Array,
    Map,
    Tag,
    Simple,
}

impl WireMajor {
    /// The `@custom_wire_major` token that spells this major.
    pub fn token(self) -> &'static str {
        match self {
            Self::Uint => "uint",
            Self::Nint => "nint",
            Self::Bytes => "bytes",
            Self::Text => "text",
            Self::Array => "array",
            Self::Map => "map",
            Self::Tag => "tag",
            Self::Simple => "simple",
        }
    }

    /// Every major, in CBOR major-type order (0..7). The parser's accepted vocabulary.
    pub const ALL: &'static [WireMajor] = &[
        Self::Uint,
        Self::Nint,
        Self::Bytes,
        Self::Text,
        Self::Array,
        Self::Map,
        Self::Tag,
        Self::Simple,
    ];
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct RuleMetadata {
    pub name: Option<String>,
    /// `@rust_name`: pins the FINAL derived Rust type name for a rule living in an extern-deps
    /// (`_CDDL_CODEGEN_EXTERN_DEPS_DIR_`) scope. Unlike `@name` (which renames fields/variants, never
    /// the top-level rule type), this renames the type — but ONLY across the crate boundary: a
    /// consumer imports the dependency's real type under this pinned name (`use dep::Pinned as
    /// Derived;`) instead of re-deriving the name from the CDDL ident with its own (possibly newer)
    /// codegen version. This is what kills the cross-version naming-skew class. Rejected on any
    /// exported rule (see `parsing::handle_rust_name_pin`); a pin that camel-cases to a reserved Rust
    /// type is rejected exactly as a derived name would be.
    pub rust_name: Option<String>,
    /// None = not newtype, Some(None) = getter under the default name `get`,
    /// Some(Some(name)) = getter renamed to `name`
    pub newtype: Option<Option<String>>,
    pub no_alias: bool,
    /// None = no `@used_as_key` tag; `Some(demand)` = tagged with the given flavor(s) (bare when no
    /// flavor word follows the tag). See [`DemandSet`].
    pub key_demand: Option<DemandSet>,
    /// `@used_as_elem`: mint the loose-list wasm wrapper (`FooList = [* foo]` equivalent) for this
    /// rule's type as if the spec contained an inline `[* foo]` usage, so a downstream crate can
    /// import the canonical wrapper class from THIS crate. See `IntermediateTypes::mark_used_as_elem`.
    pub used_as_elem: bool,
    /// `@copy`: valid ONLY on a `_CDDL_CODEGEN_EXTERN_TYPE_` or `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule.
    /// Declares that the referenced (externally-defined) rust type derives `Copy`, so the generator
    /// stops emitting a defensive `.clone()` at every boundary that moves the value (map-key
    /// deserialize loops, wasm getters/accessors). The declaring crate emits a compile-time `Copy`
    /// assertion for the type (see `export.rs`), so a false `@copy` fails THAT crate's own build with
    /// a named error — never a distant consumer's. It rides the extern-interface seam, so
    /// `--extern-import` consumers inherit it: `@copy` describes the BASE type, which the projection's
    /// param-less rendering preserves faithfully (unlike `@raw_bytes_flavor` below). On any other
    /// placement it is a graceful parse-time rejection (never silently ignored). See
    /// `IntermediateTypes::is_copy_extern`.
    pub copy: bool,
    /// `@raw_bytes_flavor`: valid ONLY on a `_CDDL_CODEGEN_EXTERN_TYPE_` GENERIC rule — a
    /// non-generic extern is a graceful parse-time rejection, since a base with no parameters has no
    /// instances to flavor. When a generic instance of the tagged extern has any argument that
    /// resolves to a `_CDDL_CODEGEN_RAW_BYTES_TYPE_`, the monomorphized alias references the
    /// convention-named `<ExternName>RawBytes` flavor instead of the plain name. Opt-in (never
    /// automatic): a wrapper bound solely on `RawBytesEncoding` compiles today under the plain name,
    /// so auto-flavoring would silently break working output. It does NOT ride the extern-interface
    /// seam: the projection drops generic parameters, so the tag would land on the very spelling the
    /// rejection above refuses (see `extern_interface.rs`'s annotation assembly). See
    /// `IntermediateTypes::mark_raw_bytes_flavor`.
    pub raw_bytes_flavor: bool,
    /// `@ignore`: valid ONLY on a recognized open struct-map rest row (`* k => v` after fixed keys).
    /// Selects the tolerate-and-DROP flavor — unknown map entries are typed-deserialized and then
    /// discarded (no struct field is emitted, and serialize writes only the declared members), the
    /// documented-lossy counterpart to the default capture flavor. Bare and argument-less, so it
    /// OR-merges like the other boolean flags. On any other placement it is a graceful parse-time
    /// rejection (never silently ignored), and it is rejected together with `--preserve-encodings`
    /// (a preserve crate's byte-exact round-trip contract cannot hold for a deliberately-lossy type).
    /// See `parsing::recognize_rest_row`.
    pub ignore: bool,
    /// `@duplicates preserve|reject`: the per-rule duplicate-handling policy for a set/array/table
    /// collection rule. `None` = no directive (today's per-container defaults apply, unchanged). Only
    /// valid on collection rules; on any other placement it is a graceful parse-time rejection (never
    /// silently ignored). See [`DuplicatesPolicy`].
    pub duplicates: Option<DuplicatesPolicy>,
    pub custom_json: bool,
    /// `@no_json_schema_export`: suppress this rule's schema-registration row in the json-gen crate
    /// (`--json-schema-export`) — and NOTHING else. The `serde`/`schemars` derives stay (a parent
    /// that embeds the type still needs `JsonSchema` on it), CBOR serialization / the wasm surface /
    /// the extern-interface export and self-check are untouched, and with `--json-schema-export` off
    /// the directive is simply inert (one spec, many flag sets). Orthogonal to — and legally
    /// combinable with — `@custom_json` ("I supply the JSON impls, and this type is not a published
    /// schema root"). Bare and argument-less, so it OR-merges like the other boolean flags. On a rule
    /// that registers no rust struct at all it is a graceful rejection (never silently ignored). See
    /// `IntermediateTypes::is_no_json_schema_export`.
    pub no_json_schema_export: bool,
    pub custom_serialize: Option<String>,
    pub custom_deserialize: Option<String>,
    /// `@custom_encodings <kind>[,<kind>…]` / `@custom_encodings none`: the codec-visible encoding
    /// variables of the wire a `@custom_serialize`/`@custom_deserialize` pair writes and reads.
    /// `None` = no declaration (the replaced type's INFERRED demand drives the signature, as it
    /// always has); `Some(kinds)` = the declaration drives it instead, positionally, everywhere
    /// (the codec's trailing serialize args, its deserialize return tuple, and the encoding-struct
    /// slots the generated code stores them in). `Some(vec![])` is the explicit `none` spelling —
    /// "this codec's wire has no framing", an assertion rather than an omission.
    ///
    /// Valid ONLY beside BOTH halves of the pair, in the same position (a type-level alias rule's
    /// comment, or a field's comment): with one half the other direction is generated code deriving
    /// inference, which declared slots would contradict by construction. Everywhere else it is a
    /// graceful rejection. Inert without `--preserve-encodings` (no encoding variables exist at
    /// all — "one spec, many flag sets", like `@extern_companions` without `--wasm`). See
    /// [`EncodingKind`] and `generation::declared_encoding_fields`.
    pub custom_encodings: Option<Vec<EncodingKind>>,
    /// `@custom_wire_major <major>`: the CBOR major type the wire a `@custom_serialize`/
    /// `@custom_deserialize` pair writes and reads STARTS with. The second member of the wire-facts
    /// declaration family (see [`WireMajor`]).
    ///
    /// Valid ONLY beside BOTH halves of the pair, in the same position (the `@custom_encodings`
    /// both-halves contract, and for the same reason: with one half the other direction is generated
    /// code whose real major the declaration would contradict). REQUIRED when the rule keys an OPEN
    /// TABLE's typed row, or when a variable middle ARRAY boundary must see the codec-owned head —
    /// there the generator must know the claimed major before any deserializer runs, and
    /// `cbor_types()` answers about the REPLACED type's wire, which the codec has taken over. A rule
    /// carrying it that neither consumer reaches is a graceful rejection (no-silent-directive):
    /// consumed somewhere is enough, so one alias may prove either boundary and also appear at a
    /// field.
    pub custom_wire_major: Option<WireMajor>,
    /// `@extern_companions <path>=<Class>[,<Class>…]`: valid ONLY on a LOCALLY-scoped, non-generic
    /// `_CDDL_CODEGEN_EXTERN_TYPE_` or `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule. Declares that the named
    /// structural wasm companion classes
    /// of that type already exist in a sibling wasm crate, so the generator emits
    /// `use <path>::<Class>;` and references them instead of minting its own `#[wasm_bindgen]`
    /// duplicates (which duplicate-symbol at link when both crates enter one cdylib). Only listed
    /// classes defer. Inert without `--wasm` (the classes it names are a wasm-boundary concern). On
    /// any other placement — a rule this crate GENERATES, or a DEP-scoped one, which
    /// `--extern-wrapper-index` / `--workspace-dep` already own — it is a graceful parse-time
    /// rejection, never silently ignored. See [`ExternCompanions`] and
    /// `IntermediateTypes::extern_companions`.
    pub extern_companions: Option<ExternCompanions>,
    pub comment: Option<String>,
}

/// The matrix-facing projection of comment metadata.  This deliberately lives beside the parser:
/// a matrix feature id is credited only after the real grammar has accepted and merged a directive.
///
/// Every directive's id derives from its `directives!` spelling; the field-to-directive
/// classification is the exhaustive destructure in [`RuleMetadata::directives`].
#[allow(dead_code)] // consumed by the library-linked `comment_dsl` example, not the bin crate
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatrixDslFacts {
    pub ids: Vec<String>,
    pub key_demand: Option<DemandSet>,
    pub newtype_getter: Option<Option<String>>,
    pub duplicates: Option<DuplicatesPolicy>,
    pub custom_encodings: Option<Vec<EncodingKind>>,
    pub custom_wire_major: Option<WireMajor>,
    pub extern_companions: Option<ExternCompanions>,
    pub doc: Option<String>,
}

/// The field-wise merge rules, WITHOUT the cross-field [`RuleMetadata::verify`]: flags OR, a
/// single-valued field set on both sides is the duplicate-key panic, and `@used_as_key` demand
/// unions. Both merge paths use it — [`merge_metadata`] (comment line into comment line) and
/// [`rule_metadata`]'s fold (directive into directive within one line) — so the rules exist once.
/// The fold must not verify per step: `@newtype @no_alias @name a @name b` reports the duplicate
/// `@name` (the per-directive rule) rather than the cross-field conflict, as it always has.
fn merge_fields(r1: &RuleMetadata, r2: &RuleMetadata) -> RuleMetadata {
    macro_rules! exclusive {
        ($field:ident) => {
            match (r1.$field.as_ref(), r2.$field.as_ref()) {
                (Some(val1), Some(val2)) => panic!(
                    concat!(
                        "Key \"",
                        stringify!($field),
                        "\" specified twice: {:?} {:?}"
                    ),
                    val1, val2
                ),
                (val @ Some(_), _) => val.cloned(),
                (_, val) => val.cloned(),
            }
        };
    }
    RuleMetadata {
        name: exclusive!(name),
        rust_name: exclusive!(rust_name),
        newtype: exclusive!(newtype),
        no_alias: r1.no_alias || r2.no_alias,
        key_demand: merge_key_demand(r1.key_demand, r2.key_demand),
        used_as_elem: r1.used_as_elem || r2.used_as_elem,
        copy: r1.copy || r2.copy,
        raw_bytes_flavor: r1.raw_bytes_flavor || r2.raw_bytes_flavor,
        ignore: r1.ignore || r2.ignore,
        duplicates: exclusive!(duplicates),
        custom_json: r1.custom_json || r2.custom_json,
        no_json_schema_export: r1.no_json_schema_export || r2.no_json_schema_export,
        custom_serialize: exclusive!(custom_serialize),
        custom_deserialize: exclusive!(custom_deserialize),
        custom_encodings: exclusive!(custom_encodings),
        custom_wire_major: exclusive!(custom_wire_major),
        extern_companions: exclusive!(extern_companions),
        comment: exclusive!(comment),
    }
}

pub fn merge_metadata(r1: &RuleMetadata, r2: &RuleMetadata) -> RuleMetadata {
    let merged = merge_fields(r1, r2);
    merged.verify();
    merged
}

fn single(set: impl FnOnce(&mut RuleMetadata)) -> RuleMetadata {
    let mut metadata = RuleMetadata::default();
    set(&mut metadata);
    metadata
}

/// Declares the rule-metadata directive vocabulary ONCE: each row is a [`Directive`] variant, its
/// `@`-spelling, and the parser for the argument text that follows the spelling. The macro derives
/// the enum, [`Directive::ALL`] (dispatch order), [`Directive::spelling`], the argument dispatch,
/// and [`KNOWN_RULE_METADATA_TAGS`] from those rows, so none of them can fall out of lockstep.
///
/// `cddl-matrix/no_silent_directive.ts` and `cddl-matrix/verify.ts` read the vocabulary from the
/// `Variant = "@spelling"` rows of the `directives!` invocation below; keep that row shape.
macro_rules! directives {
    ($($variant:ident = $spelling:literal => $args:expr,)*) => {
        /// One rule-metadata directive. Declaration order is dispatch order (see
        /// [`whitespace_then_directive`]) and the order [`RuleMetadata::directives`] reports.
        #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
        pub enum Directive {
            $($variant,)*
        }

        impl Directive {
            /// Every directive, in declaration (= dispatch) order.
            pub const ALL: &'static [Directive] = &[$(Directive::$variant,)*];

            /// The `@`-token an author writes.
            pub fn spelling(self) -> &'static str {
                match self {
                    $(Directive::$variant => $spelling,)*
                }
            }

            /// Parse this directive's argument text (the input right after its spelling) into the
            /// single-directive metadata it contributes.
            fn parse_args(self, input: &str) -> IResult<&str, RuleMetadata> {
                match self {
                    $(Directive::$variant => ($args)(input),)*
                }
            }
        }

        /// The complete `@`-token vocabulary the rule-metadata DSL recognizes, surfaced as data for
        /// the extern-interface strict `@`-scan (`api::scan_extern_import_seam`), which hard-errors
        /// on any `@`-token outside this set. Because dispatch prefix-matches, the scan treats a
        /// known tag as a PREFIX of the scanned token — `@namefoo` credits `@name` in both places.
        /// Derived from the `directives!` rows, so it cannot drift from what parses.
        ///
        /// Adding a directive is the START of a checklist, not the whole of it: once the directive
        /// is also DOCUMENTED in `docs/docs/comment_dsl.mdx`, `cddl-matrix/verify.ts`'s forward
        /// completeness lint hard-fails until it has a `features/cddl_codegen.toml` row and a
        /// minted verdict, and that lint is FULL-tier — so a local/fast tier stays green while the
        /// full tier is red. The whole chain (feature row, decode-catalog row, ingredients, the
        /// vendor-count pin) is written down in `cddl-matrix/README.md` § "Registering a new vendor
        /// (CDDL_CODEGEN) feature row"; read it before deferring any part of it.
        pub const KNOWN_RULE_METADATA_TAGS: &[&str] = &[$($spelling,)*];
    };
}

directives! {
    Name = "@name" => |i| word_arg(i, |m, w| m.name = Some(w)),
    RustName = "@rust_name" => |i| word_arg(i, |m, w| m.rust_name = Some(w)),
    Newtype = "@newtype" => newtype_args,
    NoAlias = "@no_alias" => |i| flag(i, |m| m.no_alias = true),
    UsedAsKey = "@used_as_key" => used_as_key_args,
    UsedAsElem = "@used_as_elem" => |i| flag(i, |m| m.used_as_elem = true),
    Copy = "@copy" => |i| flag(i, |m| m.copy = true),
    RawBytesFlavor = "@raw_bytes_flavor" => |i| flag(i, |m| m.raw_bytes_flavor = true),
    Ignore = "@ignore" => |i| flag(i, |m| m.ignore = true),
    Duplicates = "@duplicates" => duplicates_args,
    CustomJson = "@custom_json" => |i| flag(i, |m| m.custom_json = true),
    NoJsonSchemaExport = "@no_json_schema_export" => |i| flag(i, |m| m.no_json_schema_export = true),
    CustomSerialize = "@custom_serialize" => |i| word_arg(i, |m, w| m.custom_serialize = Some(w)),
    CustomDeserialize = "@custom_deserialize" => |i| word_arg(i, |m, w| m.custom_deserialize = Some(w)),
    CustomEncodings = "@custom_encodings" => custom_encodings_args,
    CustomWireMajor = "@custom_wire_major" => custom_wire_major_args,
    ExternCompanions = "@extern_companions" => extern_companions_args,
    Doc = "@doc" => doc_args,
}

impl Directive {
    /// Whether a type-choice VARIANT position legitimately consumes this directive: `@name` names
    /// the variant and `@doc` documents it (see `parsing::create_variants_from_type_choices`, which
    /// reads exactly those two fields and discards the rest). Exhaustive on purpose — a new
    /// directive fails to compile here until its author classifies it, the forcing function a
    /// hand-maintained exclusion list cannot provide.
    pub fn is_variant_legal(self) -> bool {
        match self {
            Directive::Name | Directive::Doc => true,
            Directive::RustName
            | Directive::Newtype
            | Directive::NoAlias
            | Directive::UsedAsKey
            | Directive::UsedAsElem
            | Directive::Copy
            | Directive::RawBytesFlavor
            | Directive::Ignore
            | Directive::Duplicates
            | Directive::CustomJson
            | Directive::NoJsonSchemaExport
            | Directive::CustomSerialize
            | Directive::CustomDeserialize
            | Directive::CustomEncodings
            | Directive::CustomWireMajor
            | Directive::ExternCompanions => false,
        }
    }
}

impl RuleMetadata {
    fn verify(&self) {
        if self.newtype.is_some() && self.no_alias {
            // this would make no sense anyway as with newtype we're already not making an alias
            panic!("cannot use both @newtype and @no_alias on the same alias");
        }
    }

    /// Every directive set on this metadata, in [`Directive::ALL`] order. The ONE field-to-directive
    /// classification: the exhaustive destructure makes a new `RuleMetadata` field fail to compile
    /// until its author maps it to a [`Directive`], and every directive list below derives from it.
    pub fn directives(&self) -> Vec<Directive> {
        let Self {
            name,
            rust_name,
            newtype,
            no_alias,
            key_demand,
            used_as_elem,
            copy,
            raw_bytes_flavor,
            ignore,
            duplicates,
            custom_json,
            no_json_schema_export,
            custom_serialize,
            custom_deserialize,
            custom_encodings,
            custom_wire_major,
            extern_companions,
            comment,
        } = self;
        let mut found: Vec<Directive> = [
            (name.is_some(), Directive::Name),
            (rust_name.is_some(), Directive::RustName),
            (newtype.is_some(), Directive::Newtype),
            (*no_alias, Directive::NoAlias),
            (key_demand.is_some(), Directive::UsedAsKey),
            (*used_as_elem, Directive::UsedAsElem),
            (*copy, Directive::Copy),
            (*raw_bytes_flavor, Directive::RawBytesFlavor),
            (*ignore, Directive::Ignore),
            (duplicates.is_some(), Directive::Duplicates),
            (*custom_json, Directive::CustomJson),
            (*no_json_schema_export, Directive::NoJsonSchemaExport),
            (custom_serialize.is_some(), Directive::CustomSerialize),
            (custom_deserialize.is_some(), Directive::CustomDeserialize),
            (custom_encodings.is_some(), Directive::CustomEncodings),
            (custom_wire_major.is_some(), Directive::CustomWireMajor),
            (extern_companions.is_some(), Directive::ExternCompanions),
            (comment.is_some(), Directive::Doc),
        ]
        .into_iter()
        .filter_map(|(set, directive)| set.then_some(directive))
        .collect();
        found.sort();
        found
    }

    /// The `@`-spellings of every rule-level directive set on this metadata, EXCLUDING the ones a
    /// type-choice VARIANT position legitimately consumes ([`Directive::is_variant_legal`]).
    ///
    /// Exists for the non-last-arm rejections in `parsing` (`parse_type_choices` and the inline
    /// `T / null` lowering).
    pub fn non_variant_directives(&self) -> Vec<&'static str> {
        self.directives()
            .into_iter()
            .filter(|directive| !directive.is_variant_legal())
            .map(Directive::spelling)
            .collect()
    }

    /// Every directive set on this metadata, sorted by spelling.
    ///
    /// Exists for the refusals that report what an author wrote into a slot where NOTHING is
    /// honored (e.g. the never-spliced plain-group refusal in `IntermediateTypes::finalize`), so
    /// unlike the variant case the list must be total.
    pub fn all_directives(&self) -> Vec<&'static str> {
        let mut found: Vec<&'static str> = self
            .directives()
            .into_iter()
            .map(Directive::spelling)
            .collect();
        found.sort_unstable();
        found
    }

    /// Project accepted metadata into the cddl-matrix DSL feature ids and the argument-bearing
    /// facts whose spelling used to be duplicated by `corpus_detect.ts`.  This is an authority
    /// boundary, not another parser: callers receive facts only after `metadata_from_comments`
    /// has run the real `nom` grammar and its merge/verification rules.
    ///
    /// A directive's feature id is `dsl.<spelling without @>`, refined by its argument for the two
    /// directives whose matrix rows are per-argument (`@used_as_key` flavors, `@duplicates`
    /// policies).
    #[allow(dead_code)] // see MatrixDslFacts: examples link lib.rs while tests compile main.rs too
    pub fn matrix_dsl_facts(&self) -> MatrixDslFacts {
        let mut ids: Vec<String> = self
            .directives()
            .into_iter()
            .map(|directive| {
                let base = format!("dsl.{}", &directive.spelling()[1..]);
                let refinement = match directive {
                    Directive::UsedAsKey => {
                        self.key_demand
                            .and_then(|demand| match (demand.hash, demand.ord) {
                                (true, true) => Some("hash_ord"),
                                (true, false) => Some("hash"),
                                (false, true) => Some("ord"),
                                (false, false) => None,
                            })
                    }
                    Directive::Duplicates => self.duplicates.map(|policy| match policy {
                        DuplicatesPolicy::Preserve => "preserve",
                        DuplicatesPolicy::Reject => "reject",
                    }),
                    _ => None,
                };
                match refinement {
                    Some(refinement) => format!("{base}.{refinement}"),
                    None => base,
                }
            })
            .collect();
        ids.sort_unstable();
        MatrixDslFacts {
            ids,
            key_demand: self.key_demand,
            newtype_getter: self.newtype.clone(),
            duplicates: self.duplicates,
            custom_encodings: self.custom_encodings.clone(),
            custom_wire_major: self.custom_wire_major,
            extern_companions: self.extern_companions.clone(),
            doc: self.comment.clone(),
        }
    }
}

/// An argument-less directive (`@copy`, `@no_alias`, …): consumes nothing after its spelling.
fn flag(input: &str, set: impl FnOnce(&mut RuleMetadata)) -> IResult<&str, RuleMetadata> {
    Ok((input, single(set)))
}

/// A directive taking one whitespace-free word (`@name`, `@rust_name`, `@custom_serialize`,
/// `@custom_deserialize`). Lenient, as it always was: a missing word is a nom error, which
/// `metadata_from_comments` swallows with the rest of the line; and the word is NOT cut at `@`.
fn word_arg(
    input: &str,
    set: impl FnOnce(&mut RuleMetadata, String),
) -> IResult<&str, RuleMetadata> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, word) = take_while1(|ch: char| !ch.is_whitespace())(input)?;
    Ok((input, single(|metadata| set(metadata, word.to_string()))))
}

/// The strict-vocabulary directives' REQUIRED argument: skip whitespace, PANIC with `missing` when
/// the comment ends or the next directive starts, else take one token cut at whitespace or `@`.
/// Panicking (not a nom error) is the point: `metadata_from_comments` swallows nom errors, so a
/// soft failure would silently drop the whole line's metadata — the distant-failure class these
/// directives exist to kill.
fn required_arg<'a>(input: &'a str, missing: &str) -> IResult<&'a str, &'a str> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    if input.is_empty() || input.starts_with('@') {
        panic!("{missing}");
    }
    take_while1(|ch: char| !ch.is_whitespace() && ch != '@')(input)
}

/// The backtick-quoted, ` / `-joined token list a strict directive's rejection message names.
fn vocabulary<T: Copy>(all: &[T], token: impl Fn(T) -> &'static str) -> String {
    all.iter()
        .map(|item| format!("`{}`", token(*item)))
        .collect::<Vec<_>>()
        .join(" / ")
}

/// A syntactic rust identifier: the shape `@newtype`'s optional getter argument must have, since it
/// is emitted verbatim as a method name. Deliberately syntactic only — a keyword getter (`match`)
/// still reaches the compiler, which names it precisely; what this bounds is the token that would
/// otherwise reach `rustfmt` as unparseable source.
fn is_rust_ident(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) if first.is_alphabetic() || first == '_' => {}
        _ => return false,
    }
    chars.all(|ch| ch.is_alphanumeric() || ch == '_')
}

fn newtype_args(input: &str) -> IResult<&str, RuleMetadata> {
    // to get around type annotations
    fn parse_newtype(input: &str) -> IResult<&str, RuleMetadata> {
        let (input, _) = take_while(char::is_whitespace)(input)?;
        let (input, getter) = take_while1(|ch| !char::is_whitespace(ch) && ch != '@')(input)?;
        let getter = getter.trim();
        // `@newtype` is the one directive whose argument is both OPTIONAL and free-form, so it is
        // the one that can capture text the author never meant as an argument. A CDDL comment runs
        // to end of line, which makes the second `;` in `tk = text ; @newtype ; my comment` comment
        // CONTENT: an unbounded token read takes `;` as the getter and emits `pub fn ;(&self)`,
        // which surfaces as a rustfmt parse failure blaming the generator. Bounding the token to a
        // rust identifier and PANICking otherwise (matching `@used_as_key`/`@duplicates`'
        // unknown-argument handling) names the cause at the cause.
        if !is_rust_ident(getter) {
            panic!(
                "@newtype: invalid getter name {getter:?}; expected a rust identifier \
                 (`@newtype inner`) or bare `@newtype`. A CDDL comment runs to end of line, so a \
                 second `;` on the line is comment CONTENT and is read as the getter — put prose in \
                 `@doc`."
            );
        }
        Ok((input, single(|m| m.newtype = Some(Some(getter.to_owned())))))
    }
    match parse_newtype(input) {
        Ok(ret) => Ok(ret),
        Err(_) => Ok((input.trim_start(), single(|m| m.newtype = Some(None)))),
    }
}

fn used_as_key_args(input: &str) -> IResult<&str, RuleMetadata> {
    // Parse the optional flavor words (`hash`, `ord`) that follow, up to the next `@tag` or end of
    // the comment. Strict vocabulary: any other word is a PANIC. The comment parser otherwise swallows
    // nom errors (`metadata_from_comments`) and `many0` ignores leftovers, so a soft parse failure here
    // would silently drop the whole line's metadata and regress the tagged type to no key derives — the
    // exact distant-failure class this DSL exists to kill. Panicking (matching the duplicate-key panics)
    // makes a typo/prose loud instead. This intentionally rejects today-legal trailing prose
    // (`@used_as_key marks the tx-out`); prose belongs in `@doc`.
    let mut demand = DemandSet::default();
    let mut any_flavor = false;
    let mut rest = input;
    loop {
        let (after_ws, _) = take_while(char::is_whitespace)(rest)?;
        if after_ws.is_empty() || after_ws.starts_with('@') {
            rest = after_ws;
            break;
        }
        let (after_word, word) = take_while1(|ch| !char::is_whitespace(ch) && ch != '@')(after_ws)?;
        match word {
            "hash" => demand.hash = true,
            "ord" => demand.ord = true,
            other => panic!(
                "@used_as_key: unknown flavor {other:?}; expected `hash` and/or `ord`, or bare \
                 `@used_as_key`. (Trailing prose is not allowed after `@used_as_key` — put it in `@doc`.)"
            ),
        }
        any_flavor = true;
        rest = after_word;
    }
    if !any_flavor {
        demand.bare = true;
    }
    Ok((rest, single(|m| m.key_demand = Some(demand))))
}

fn duplicates_args(input: &str) -> IResult<&str, RuleMetadata> {
    // `@duplicates` requires exactly one argument from a strict vocabulary; see `required_arg`.
    let (rest, word) = required_arg(
        input,
        "@duplicates: missing required argument; expected `preserve` or `reject` \
         (e.g. `@duplicates reject`).",
    )?;
    let policy = match word {
        "preserve" => DuplicatesPolicy::Preserve,
        "reject" => DuplicatesPolicy::Reject,
        other => panic!(
            "@duplicates: unknown argument {other:?}; expected `preserve` or `reject`. \
             (Trailing prose is not allowed after `@duplicates` — put it in `@doc`.)"
        ),
    };
    Ok((rest, single(|m| m.duplicates = Some(policy))))
}

fn custom_wire_major_args(input: &str) -> IResult<&str, RuleMetadata> {
    // Exactly one REQUIRED argument from a strict vocabulary (the `@custom_encodings` contract, and
    // panicking for the same reason: a soft failure would drop the pair AND its declared major,
    // re-arming the silent-normalization trap this family exists to disarm).
    let vocabulary = vocabulary(WireMajor::ALL, WireMajor::token);
    let (rest, arg) = required_arg(
        input,
        &format!(
            "@custom_wire_major: missing required argument; expected exactly one of {vocabulary} \
             (e.g. `@custom_wire_major text`)."
        ),
    )?;
    match WireMajor::ALL.iter().find(|m| m.token() == arg) {
        Some(major) => Ok((rest, single(|m| m.custom_wire_major = Some(*major)))),
        None => panic!(
            "@custom_wire_major: unknown major {arg:?}; expected exactly one of {vocabulary} (e.g. \
             `@custom_wire_major text`). The eight tokens are the eight CBOR major types."
        ),
    }
}

fn custom_encodings_args(input: &str) -> IResult<&str, RuleMetadata> {
    // Exactly one REQUIRED argument from a strict vocabulary, whitespace-free (house style). A
    // missing or malformed argument is a PANIC: a soft failure would drop the pair AND its
    // declaration, re-arming the very silent-normalization trap this directive exists to disarm.
    let vocabulary = vocabulary(EncodingKind::ALL, EncodingKind::token);
    let (rest, arg) = required_arg(
        input,
        &format!(
            "@custom_encodings: missing required argument; expected a comma-separated list of \
             {vocabulary} with no whitespace (e.g. `@custom_encodings sz,str`), or the keyword \
             `none` for a wire with no framing."
        ),
    )?;
    if arg == "none" {
        return Ok((rest, single(|m| m.custom_encodings = Some(Vec::new()))));
    }
    let kinds = arg
        .split(',')
        .map(|token| match EncodingKind::ALL.iter().find(|k| k.token() == token) {
            Some(kind) => *kind,
            None => panic!(
                "@custom_encodings: unknown kind {token:?} in {arg:?}; expected a comma-separated \
                 list of {vocabulary} with no whitespace (e.g. `@custom_encodings sz,str`), or the \
                 keyword `none` for a wire with no framing. A trailing comma, an empty entry, or a \
                 space after a comma reaches here as an unknown kind. The aggregate (per-element \
                 `Vec`/`BTreeMap`) and tag-presence encoding types are deliberately not declarable."
            ),
        })
        .collect();
    Ok((rest, single(|m| m.custom_encodings = Some(kinds))))
}

/// Whether `path` is a `::`-separated chain of rust identifiers — the shape the `use <path>::<Class>;`
/// head must have, since it is emitted verbatim into the generated wasm crate. Deliberately syntactic
/// only (like `@newtype`'s getter bound): whether the crate exists is the CONSUMER'S COMPILE to
/// decide, which is this directive's whole trust-and-compile contract. What this bounds is the token
/// that would otherwise reach `rustfmt` as unparseable source.
fn is_rust_path(path: &str) -> bool {
    !path.is_empty() && path.split("::").all(is_rust_ident)
}

fn extern_companions_args(input: &str) -> IResult<&str, RuleMetadata> {
    // Exactly one REQUIRED argument, in a strict shape (see `required_arg`): a soft failure would
    // silently re-mint the very classes the directive exists to suppress, whose only symptom is a
    // `rust-lld: duplicate symbol` in a DIFFERENT crate's link. Loud at the cause.
    let (rest, arg) = required_arg(
        input,
        "@extern_companions: missing required argument; expected \
         `<use_path_prefix>=<Class>[,<Class>…]` (e.g. \
         `@extern_companions cml_chain_wasm=TransactionMetadatumList`).",
    )?;
    let Some((path_prefix, class_list)) = arg.split_once('=') else {
        panic!(
            "@extern_companions: malformed argument {arg:?}; expected \
             `<use_path_prefix>=<Class>[,<Class>…]` — the `=` separating the sibling crate path from \
             the comma-separated class names is required, and neither side may contain whitespace."
        );
    };
    if !is_rust_path(path_prefix) {
        panic!(
            "@extern_companions: invalid use-path prefix {path_prefix:?}; expected a rust path \
             (`cml_chain_wasm`, `cml_chain_wasm::auxdata`) — it is emitted verbatim as the head of \
             `use <prefix>::<Class>;`."
        );
    }
    let mut classes = BTreeSet::new();
    for class in class_list.split(',') {
        if !is_rust_ident(class) {
            panic!(
                "@extern_companions: invalid companion class name {class:?} in {arg:?}; expected a \
                 comma-separated list of rust type identifiers naming the classes that ALREADY \
                 exist in {path_prefix} (e.g. `TransactionMetadatumList`). A trailing comma or an \
                 empty entry reaches here as an empty name."
            );
        }
        classes.insert(class.to_owned());
    }
    let companions = ExternCompanions {
        path_prefix: path_prefix.to_owned(),
        classes,
    };
    Ok((rest, single(|m| m.extern_companions = Some(companions))))
}

/// `@doc`: everything up to the next `@` (or the end of the comment), trimmed.
fn doc_args(input: &str) -> IResult<&str, RuleMetadata> {
    let (input, doc) = take_while1(|c| c != '@')(input)?;
    Ok((input, single(|m| m.comment = Some(doc.trim().to_string()))))
}

/// Skip whitespace, then parse ONE directive: the first [`Directive::ALL`] spelling that prefixes
/// the input, then its arguments. First-match is order-independent because no spelling is a prefix
/// of another (`no_directive_spelling_prefixes_another` pins that).
fn whitespace_then_directive(input: &str) -> IResult<&str, RuleMetadata> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    for &directive in Directive::ALL {
        if let Some(rest) = input.strip_prefix(directive.spelling()) {
            return directive.parse_args(rest);
        }
    }
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::Tag,
    )))
}

fn rule_metadata(input: &str) -> IResult<&str, RuleMetadata> {
    let (input, parsed) = many0(whitespace_then_directive).parse(input)?;
    let merged = parsed
        .iter()
        .fold(RuleMetadata::default(), |acc, one| merge_fields(&acc, one));
    merged.verify();
    Ok((input, merged))
}

impl<'a> From<Option<&'a cddl::ast::Comments<'a>>> for RuleMetadata {
    fn from(comments: Option<&'a cddl::ast::Comments<'a>>) -> RuleMetadata {
        match comments {
            None => RuleMetadata::default(),
            Some(c) => metadata_from_comments(&c.0),
        }
    }
}

pub fn metadata_from_comments(comments: &[&str]) -> RuleMetadata {
    let mut result = RuleMetadata::default();
    for comment in comments {
        if let Ok(comment_metadata) = rule_metadata(comment) {
            result = merge_metadata(&result, &comment_metadata.1);
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_comment_name() {
        assert_eq!(
            rule_metadata("@name foo"),
            Ok((
                "",
                RuleMetadata {
                    name: Some("foo".to_string()),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn parse_comment_newtype() {
        assert_eq!(
            rule_metadata("@newtype"),
            Ok((
                "",
                RuleMetadata {
                    newtype: Some(None),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn parse_comment_newtype_getter_before() {
        assert_eq!(
            rule_metadata("@newtype custom_getter @used_as_key"),
            Ok((
                "",
                RuleMetadata {
                    newtype: Some(Some("custom_getter".to_owned())),
                    key_demand: Some(DemandSet::BARE),
                    ..Default::default()
                }
            ))
        );
    }

    /// The getter bound is syntactic, so it must not narrow the spellings that legitimately reach it.
    #[test]
    fn parse_comment_newtype_getter_underscore_ident() {
        let md = rule_metadata("@newtype _inner").unwrap().1;
        assert_eq!(md.newtype, Some(Some("_inner".to_owned())));
    }

    /// A CDDL comment runs to end of line, so the `;` in `; @newtype ; my comment` is comment CONTENT.
    /// Unbounded, the optional getter reads it and emits `pub fn ;(&self)` — invalid rust that surfaces
    /// as a rustfmt parse failure blaming the generator, a whole pipeline away from the spec line that
    /// caused it. Pinned loud at the cause instead.
    #[test]
    #[should_panic(expected = "@newtype: invalid getter name \";\"")]
    fn parse_comment_newtype_trailing_comment_is_not_a_getter() {
        let _ = rule_metadata("@newtype    ; my comment");
    }

    #[test]
    fn parse_comment_newtype_getter_after() {
        assert_eq!(
            rule_metadata("@used_as_key @newtype custom_getter"),
            Ok((
                "",
                RuleMetadata {
                    newtype: Some(Some("custom_getter".to_owned())),
                    key_demand: Some(DemandSet::BARE),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn parse_comment_newtype_and_name() {
        assert_eq!(
            rule_metadata("@newtype @name foo"),
            Ok((
                "",
                RuleMetadata {
                    name: Some("foo".to_string()),
                    newtype: Some(None),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn parse_comment_newtype_and_name_and_used_as_key() {
        assert_eq!(
            rule_metadata("@newtype @used_as_key @name foo"),
            Ok((
                "",
                RuleMetadata {
                    name: Some("foo".to_string()),
                    newtype: Some(None),
                    key_demand: Some(DemandSet::BARE),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn parse_comment_used_as_key() {
        assert_eq!(
            rule_metadata("@used_as_key"),
            Ok((
                "",
                RuleMetadata {
                    key_demand: Some(DemandSet::BARE),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn parse_comment_used_as_key_hash() {
        assert_eq!(
            rule_metadata("@used_as_key hash").unwrap().1.key_demand,
            Some(DemandSet {
                bare: false,
                hash: true,
                ord: false
            })
        );
    }

    #[test]
    fn parse_comment_used_as_key_ord() {
        assert_eq!(
            rule_metadata("@used_as_key ord").unwrap().1.key_demand,
            Some(DemandSet {
                bare: false,
                hash: false,
                ord: true
            })
        );
    }

    #[test]
    fn parse_comment_used_as_key_hash_ord() {
        assert_eq!(
            rule_metadata("@used_as_key hash ord").unwrap().1.key_demand,
            Some(DemandSet {
                bare: false,
                hash: true,
                ord: true
            })
        );
    }

    // Flavor-word order does not matter (both fold into the same union).
    #[test]
    fn parse_comment_used_as_key_ord_hash_order_independent() {
        assert_eq!(
            rule_metadata("@used_as_key ord hash").unwrap().1.key_demand,
            rule_metadata("@used_as_key hash ord").unwrap().1.key_demand,
        );
    }

    // A flavored tag stops at the next `@tag` — it must not swallow a following tag as a flavor word.
    #[test]
    fn parse_comment_used_as_key_hash_then_newtype() {
        let md = rule_metadata("@used_as_key hash @newtype custom_getter")
            .unwrap()
            .1;
        assert_eq!(
            md.key_demand,
            Some(DemandSet {
                bare: false,
                hash: true,
                ord: false
            })
        );
        assert_eq!(md.newtype, Some(Some("custom_getter".to_owned())));
    }

    // Two comment lines union their flavors (field-wise OR merge).
    #[test]
    fn merge_metadata_unions_key_demand_flavors() {
        let hash = RuleMetadata {
            key_demand: Some(DemandSet {
                hash: true,
                ..Default::default()
            }),
            ..Default::default()
        };
        let ord = RuleMetadata {
            key_demand: Some(DemandSet {
                ord: true,
                ..Default::default()
            }),
            ..Default::default()
        };
        assert_eq!(
            merge_metadata(&hash, &ord).key_demand,
            Some(DemandSet {
                bare: false,
                hash: true,
                ord: true
            })
        );
    }

    #[test]
    #[should_panic(expected = "unknown flavor")]
    fn parse_comment_used_as_key_unknown_flavor_panics() {
        let _ = rule_metadata("@used_as_key hsah");
    }

    // Today-legal trailing prose after `@used_as_key` is now a hard error (prose belongs in `@doc`).
    #[test]
    #[should_panic(expected = "unknown flavor")]
    fn parse_comment_used_as_key_trailing_prose_panics() {
        let _ = rule_metadata("@used_as_key marks the tx-out");
    }

    #[test]
    fn parse_comment_used_as_elem() {
        assert_eq!(
            rule_metadata("@used_as_elem"),
            Ok((
                "",
                RuleMetadata {
                    used_as_elem: true,
                    ..Default::default()
                }
            ))
        );
    }

    // `@used_as_elem` and `@used_as_key` are independent flags that can co-occur, in either order.
    #[test]
    fn parse_comment_used_as_elem_and_key() {
        assert_eq!(
            rule_metadata("@used_as_elem @used_as_key"),
            Ok((
                "",
                RuleMetadata {
                    key_demand: Some(DemandSet::BARE),
                    used_as_elem: true,
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn parse_comment_used_as_key_and_elem_inverse() {
        assert_eq!(
            rule_metadata("@used_as_key @used_as_elem"),
            Ok((
                "",
                RuleMetadata {
                    key_demand: Some(DemandSet::BARE),
                    used_as_elem: true,
                    ..Default::default()
                }
            ))
        );
    }

    // Ordering with a value-carrying tag (@newtype's optional getter) must not swallow @used_as_elem.
    #[test]
    fn parse_comment_newtype_getter_before_used_as_elem() {
        assert_eq!(
            rule_metadata("@newtype custom_getter @used_as_elem"),
            Ok((
                "",
                RuleMetadata {
                    newtype: Some(Some("custom_getter".to_owned())),
                    used_as_elem: true,
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn parse_comment_used_as_elem_before_newtype_getter() {
        assert_eq!(
            rule_metadata("@used_as_elem @newtype custom_getter"),
            Ok((
                "",
                RuleMetadata {
                    newtype: Some(Some("custom_getter".to_owned())),
                    used_as_elem: true,
                    ..Default::default()
                }
            ))
        );
    }

    // Merging two comment lines OR-folds the flag, matching @used_as_key's merge semantics.
    #[test]
    fn merge_metadata_ors_used_as_elem() {
        let lhs = RuleMetadata {
            used_as_elem: true,
            ..Default::default()
        };
        let rhs = RuleMetadata::default();
        assert!(merge_metadata(&lhs, &rhs).used_as_elem);
        assert!(merge_metadata(&rhs, &lhs).used_as_elem);
        assert!(!merge_metadata(&rhs, &rhs).used_as_elem);
    }

    #[test]
    fn parse_comment_raw_bytes_flavor() {
        assert!(
            rule_metadata("@raw_bytes_flavor")
                .unwrap()
                .1
                .raw_bytes_flavor
        );
    }

    // `@raw_bytes_flavor` is an independent flag that co-occurs with other tags, in either order,
    // without swallowing them (mirrors `@used_as_elem`'s ordering coverage).
    #[test]
    fn parse_comment_raw_bytes_flavor_and_name() {
        let md = rule_metadata("@raw_bytes_flavor @name foo").unwrap().1;
        assert!(md.raw_bytes_flavor);
        assert_eq!(md.name, Some("foo".to_string()));
        let inverse = rule_metadata("@name foo @raw_bytes_flavor").unwrap().1;
        assert!(inverse.raw_bytes_flavor);
        assert_eq!(inverse.name, Some("foo".to_string()));
    }

    // Merging two comment lines OR-folds the flag, matching the other boolean tags' merge semantics.
    #[test]
    fn merge_metadata_ors_raw_bytes_flavor() {
        let lhs = RuleMetadata {
            raw_bytes_flavor: true,
            ..Default::default()
        };
        let rhs = RuleMetadata::default();
        assert!(merge_metadata(&lhs, &rhs).raw_bytes_flavor);
        assert!(merge_metadata(&rhs, &lhs).raw_bytes_flavor);
        assert!(!merge_metadata(&rhs, &rhs).raw_bytes_flavor);
    }

    #[test]
    fn parse_comment_copy() {
        assert!(rule_metadata("@copy").unwrap().1.copy);
    }

    // `@ignore` is a bare no-arg flag (the open struct-map tolerate-and-drop rest-row flavor).
    #[test]
    fn parse_comment_ignore() {
        assert!(rule_metadata("@ignore").unwrap().1.ignore);
    }

    // `@ignore` is an independent flag that co-occurs with other tags, in either order, without
    // swallowing them (mirrors `@copy`'s ordering coverage). Here it pairs with `@name`, which a rest
    // row also accepts — the two are read together off the same entry-trailing slot.
    #[test]
    fn parse_comment_ignore_and_name() {
        let md = rule_metadata("@ignore @name foo").unwrap().1;
        assert!(md.ignore);
        assert_eq!(md.name, Some("foo".to_string()));
        let inverse = rule_metadata("@name foo @ignore").unwrap().1;
        assert!(inverse.ignore);
        assert_eq!(inverse.name, Some("foo".to_string()));
    }

    // Merging two comment lines OR-folds the flag, matching the other boolean tags' merge semantics.
    #[test]
    fn merge_metadata_ors_ignore() {
        let lhs = RuleMetadata {
            ignore: true,
            ..Default::default()
        };
        let rhs = RuleMetadata::default();
        assert!(merge_metadata(&lhs, &rhs).ignore);
        assert!(merge_metadata(&rhs, &lhs).ignore);
        assert!(!merge_metadata(&rhs, &rhs).ignore);
    }

    // `@copy` is an independent flag that co-occurs with other tags, in either order, without
    // swallowing them (mirrors `@raw_bytes_flavor`'s ordering coverage).
    #[test]
    fn parse_comment_copy_and_name() {
        let md = rule_metadata("@copy @name foo").unwrap().1;
        assert!(md.copy);
        assert_eq!(md.name, Some("foo".to_string()));
        let inverse = rule_metadata("@name foo @copy").unwrap().1;
        assert!(inverse.copy);
        assert_eq!(inverse.name, Some("foo".to_string()));
    }

    // Merging two comment lines OR-folds the flag, matching the other boolean tags' merge semantics.
    #[test]
    fn merge_metadata_ors_copy() {
        let lhs = RuleMetadata {
            copy: true,
            ..Default::default()
        };
        let rhs = RuleMetadata::default();
        assert!(merge_metadata(&lhs, &rhs).copy);
        assert!(merge_metadata(&rhs, &lhs).copy);
        assert!(!merge_metadata(&rhs, &rhs).copy);
    }

    // `@duplicates` parses both values into the strict `DuplicatesPolicy` enum.
    #[test]
    fn parse_comment_duplicates_preserve() {
        assert_eq!(
            rule_metadata("@duplicates preserve").unwrap().1.duplicates,
            Some(DuplicatesPolicy::Preserve)
        );
    }

    #[test]
    fn parse_comment_duplicates_reject() {
        assert_eq!(
            rule_metadata("@duplicates reject").unwrap().1.duplicates,
            Some(DuplicatesPolicy::Reject)
        );
    }

    // `@duplicates` consumes exactly its one argument, so a directive AFTER it is still parsed and the
    // longer/other tags are not swallowed (mirrors the ordering coverage of the other arg-taking tags).
    #[test]
    fn parse_comment_duplicates_and_name() {
        let md = rule_metadata("@duplicates reject @name foo").unwrap().1;
        assert_eq!(md.duplicates, Some(DuplicatesPolicy::Reject));
        assert_eq!(md.name, Some("foo".to_string()));
        let inverse = rule_metadata("@name foo @duplicates preserve").unwrap().1;
        assert_eq!(inverse.duplicates, Some(DuplicatesPolicy::Preserve));
        assert_eq!(inverse.name, Some("foo".to_string()));
    }

    // A second `@duplicates` on the same rule is a hard error (the duplicate-key panic, like `@name`) —
    // the two values are mutually exclusive, so unioning them makes no sense.
    #[test]
    #[should_panic(expected = "\"duplicates\" specified twice")]
    fn parse_comment_duplicates_duplicate_panics() {
        let _ = rule_metadata("@duplicates reject @duplicates preserve");
    }

    // Two comment lines carrying `@duplicates` also collide through the merge path (field-wise), not
    // only within a single line.
    #[test]
    #[should_panic(expected = "\"duplicates\" specified twice")]
    fn merge_metadata_duplicates_twice_panics() {
        let a = RuleMetadata {
            duplicates: Some(DuplicatesPolicy::Reject),
            ..Default::default()
        };
        let b = RuleMetadata {
            duplicates: Some(DuplicatesPolicy::Preserve),
            ..Default::default()
        };
        let _ = merge_metadata(&a, &b);
    }

    // An unknown argument is a hard error (matching `@used_as_key`'s unknown-flavor loudness), never a
    // silent metadata drop.
    #[test]
    #[should_panic(expected = "unknown argument")]
    fn parse_comment_duplicates_unknown_arg_panics() {
        let _ = rule_metadata("@duplicates allow");
    }

    // A missing argument is also a hard error — `@duplicates` has no meaningful bare form.
    #[test]
    #[should_panic(expected = "missing required argument")]
    fn parse_comment_duplicates_missing_arg_panics() {
        let _ = rule_metadata("@duplicates");
    }

    // A following directive counts as "missing argument" (the arg vocabulary never matches a `@tag`).
    #[test]
    #[should_panic(expected = "missing required argument")]
    fn parse_comment_duplicates_missing_arg_before_tag_panics() {
        let _ = rule_metadata("@duplicates @newtype");
    }

    #[test]
    fn parse_comment_newtype_and_name_inverse() {
        assert_eq!(
            rule_metadata("@name foo @newtype"),
            Ok((
                "",
                RuleMetadata {
                    name: Some("foo".to_string()),
                    newtype: Some(None),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn parse_comment_name_noalias() {
        assert_eq!(
            rule_metadata("@no_alias @name foo"),
            Ok((
                "",
                RuleMetadata {
                    name: Some("foo".to_string()),
                    no_alias: true,
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn parse_comment_newtype_and_custom_json() {
        assert_eq!(
            rule_metadata("@custom_json @newtype"),
            Ok((
                "",
                RuleMetadata {
                    newtype: Some(None),
                    custom_json: true,
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    #[should_panic]
    fn parse_comment_noalias_newtype() {
        let _ = rule_metadata("@no_alias @newtype");
    }

    #[test]
    fn parse_comment_custom_serialize_deserialize() {
        assert_eq!(
            rule_metadata("@custom_serialize foo @custom_deserialize bar"),
            Ok((
                "",
                RuleMetadata {
                    custom_serialize: Some("foo".to_string()),
                    custom_deserialize: Some("bar".to_string()),
                    ..Default::default()
                }
            ))
        );
    }

    // can't have all since @no_alias and @newtype are mutually exclusive
    #[test]
    fn parse_comment_all_except_no_alias() {
        assert_eq!(
            rule_metadata(
                "@newtype @name baz @custom_serialize foo @custom_deserialize bar @used_as_key @used_as_elem @custom_json @doc this is a doc comment"
            ),
            Ok((
                "",
                RuleMetadata {
                    name: Some("baz".to_string()),
                    newtype: Some(None),
                    key_demand: Some(DemandSet::BARE),
                    used_as_elem: true,
                    custom_json: true,
                    custom_serialize: Some("foo".to_string()),
                    custom_deserialize: Some("bar".to_string()),
                    comment: Some("this is a doc comment".to_string()),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn parse_comment_rust_name() {
        assert_eq!(
            rule_metadata("@rust_name PlutusData").unwrap().1.rust_name,
            Some("PlutusData".to_string())
        );
    }

    // `@rust_name` (renames the TOP-LEVEL type across the crate boundary) and `@name` (renames a
    // field/variant) are independent single-ident tags that co-occur, in either order, without one
    // swallowing the other. `@rust_name` must NOT be mistaken for `@name` by the parser.
    #[test]
    fn parse_comment_rust_name_and_name() {
        let md = rule_metadata("@name field_alias @rust_name TypeAlias")
            .unwrap()
            .1;
        assert_eq!(md.name, Some("field_alias".to_string()));
        assert_eq!(md.rust_name, Some("TypeAlias".to_string()));
        let inverse = rule_metadata("@rust_name TypeAlias @name field_alias")
            .unwrap()
            .1;
        assert_eq!(inverse.name, Some("field_alias".to_string()));
        assert_eq!(inverse.rust_name, Some("TypeAlias".to_string()));
    }

    // A second `@rust_name` on the same rule is a hard error (the duplicate-key panic, matching `@name`).
    #[test]
    #[should_panic(expected = "\"rust_name\" specified twice")]
    fn parse_comment_rust_name_duplicate_panics() {
        let _ = rule_metadata("@rust_name Foo @rust_name Bar");
    }

    // Two comment lines carrying `@rust_name` also collide through the merge path (field-wise, like
    // `@name`), not only within a single line.
    #[test]
    #[should_panic(expected = "\"rust_name\" specified twice")]
    fn merge_metadata_rust_name_twice_panics() {
        let a = RuleMetadata {
            rust_name: Some("Foo".to_string()),
            ..Default::default()
        };
        let b = RuleMetadata {
            rust_name: Some("Bar".to_string()),
            ..Default::default()
        };
        let _ = merge_metadata(&a, &b);
    }

    // `@no_json_schema_export`: the bare no-arg directive parses standalone, and — because it is
    // argument-less — a neighbouring directive on the same line is still reachable in BOTH orders (the
    // prefix-match dispatch has no sibling that shadows it). `@custom_json` is the deliberate neighbour:
    // the two are orthogonal and legally combinable ("I supply the JSON impls, and this type is not a
    // published schema root"), so the pair must parse to both flags rather than conflict.
    #[test]
    fn parse_comment_no_json_schema_export() {
        assert_eq!(
            rule_metadata("@no_json_schema_export"),
            Ok((
                "",
                RuleMetadata {
                    no_json_schema_export: true,
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            rule_metadata("@custom_json @no_json_schema_export"),
            Ok((
                "",
                RuleMetadata {
                    custom_json: true,
                    no_json_schema_export: true,
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            rule_metadata("@no_json_schema_export @custom_json"),
            Ok((
                "",
                RuleMetadata {
                    custom_json: true,
                    no_json_schema_export: true,
                    ..Default::default()
                }
            ))
        );
        // `@no_alias` shares the `@no` prefix but is not a prefix OF this tag (nor vice versa), so
        // neither shadows the other in the `alt` regardless of their relative order.
        assert_eq!(
            rule_metadata("@no_alias @no_json_schema_export"),
            Ok((
                "",
                RuleMetadata {
                    no_alias: true,
                    no_json_schema_export: true,
                    ..Default::default()
                }
            ))
        );
    }

    // `@extern_companions` parses its one required argument into the strict `ExternCompanions` shape:
    // a `use`-path prefix and the set of class names that already exist there.
    #[test]
    fn parse_comment_extern_companions() {
        assert_eq!(
            rule_metadata("@extern_companions cml_chain_wasm=TransactionMetadatumList")
                .unwrap()
                .1
                .extern_companions,
            Some(ExternCompanions {
                path_prefix: "cml_chain_wasm".to_owned(),
                classes: ["TransactionMetadatumList".to_owned()]
                    .into_iter()
                    .collect(),
            })
        );
    }

    // The class list is comma-separated and order-insensitive (a `BTreeSet`, like every other
    // order-insensitive multi-value directive), and the prefix may be a `::`-qualified module path since
    // it is emitted verbatim as the `use` head.
    #[test]
    fn parse_comment_extern_companions_multiple_classes_and_qualified_prefix() {
        let md = rule_metadata("@extern_companions cml_chain_wasm::auxdata=MdList,MapMdToMd")
            .unwrap()
            .1
            .extern_companions
            .unwrap();
        assert_eq!(md.path_prefix, "cml_chain_wasm::auxdata");
        assert_eq!(
            md.classes,
            ["MapMdToMd".to_owned(), "MdList".to_owned()]
                .into_iter()
                .collect()
        );
        assert_eq!(
            rule_metadata("@extern_companions d=MapMdToMd,MdList")
                .unwrap()
                .1
                .extern_companions
                .unwrap()
                .classes,
            md.classes
        );
    }

    // The argument is consumed, so a directive AFTER it is still parsed and neither swallows the other
    // (mirrors the ordering coverage of the other arg-taking tags).
    #[test]
    fn parse_comment_extern_companions_and_copy() {
        let md = rule_metadata("@extern_companions dep_wasm=FooList @copy")
            .unwrap()
            .1;
        assert!(md.extern_companions.is_some());
        assert!(md.copy);
        let inverse = rule_metadata("@copy @extern_companions dep_wasm=FooList")
            .unwrap()
            .1;
        assert!(inverse.extern_companions.is_some());
        assert!(inverse.copy);
    }

    // A second `@extern_companions` is the duplicate-key panic (like `@duplicates`/`@rust_name`): one
    // extern type's companions live in ONE sibling crate, so unioning two declarations would be
    // ambiguous about which prefix a class comes from.
    #[test]
    #[should_panic(expected = "\"extern_companions\" specified twice")]
    fn parse_comment_extern_companions_duplicate_panics() {
        let _ = rule_metadata("@extern_companions a=FooList @extern_companions b=BarList");
    }

    #[test]
    #[should_panic(expected = "\"extern_companions\" specified twice")]
    fn merge_metadata_extern_companions_twice_panics() {
        let one = RuleMetadata {
            extern_companions: Some(ExternCompanions {
                path_prefix: "a".to_owned(),
                classes: ["FooList".to_owned()].into_iter().collect(),
            }),
            ..Default::default()
        };
        let _ = merge_metadata(&one, &one);
    }

    // Every malformed argument is a HARD ERROR, never a silent metadata drop: silently dropping this
    // directive re-mints the very classes it exists to suppress, and the only symptom is a
    // `rust-lld: duplicate symbol` in a different crate's link.
    #[test]
    #[should_panic(expected = "missing required argument")]
    fn parse_comment_extern_companions_missing_arg_panics() {
        let _ = rule_metadata("@extern_companions");
    }

    #[test]
    #[should_panic(expected = "missing required argument")]
    fn parse_comment_extern_companions_missing_arg_before_tag_panics() {
        let _ = rule_metadata("@extern_companions @newtype");
    }

    #[test]
    #[should_panic(expected = "malformed argument")]
    fn parse_comment_extern_companions_no_equals_panics() {
        let _ = rule_metadata("@extern_companions cml_chain_wasm");
    }

    #[test]
    #[should_panic(expected = "invalid use-path prefix")]
    fn parse_comment_extern_companions_bad_prefix_panics() {
        let _ = rule_metadata("@extern_companions cml-chain-wasm=FooList");
    }

    // An empty prefix is the same class of typo as a hyphenated one (`=FooList`), and is caught by the
    // same path bound rather than slipping through as `use ::FooList;`.
    #[test]
    #[should_panic(expected = "invalid use-path prefix")]
    fn parse_comment_extern_companions_empty_prefix_panics() {
        let _ = rule_metadata("@extern_companions =FooList");
    }

    // A trailing comma reaches the class loop as an EMPTY name — the spelling a hand-edited list is
    // likeliest to grow — so it is named as such rather than silently dropped.
    #[test]
    #[should_panic(expected = "invalid companion class name")]
    fn parse_comment_extern_companions_trailing_comma_panics() {
        let _ = rule_metadata("@extern_companions dep_wasm=FooList,");
    }

    // A CDDL comment runs to end of line, so trailing prose after the argument is comment CONTENT that
    // `many0` simply stops at — it must not be swallowed into the class list (the `@newtype` getter
    // trap's shape). The directive still parses, with only its own token consumed.
    #[test]
    fn parse_comment_extern_companions_trailing_prose_is_not_an_argument() {
        let md = rule_metadata("@extern_companions dep_wasm=FooList borrowed from the sibling")
            .unwrap()
            .1
            .extern_companions
            .unwrap();
        assert_eq!(md.path_prefix, "dep_wasm");
        assert_eq!(
            md.classes,
            ["FooList".to_owned()]
                .into_iter()
                .collect::<std::collections::BTreeSet<_>>()
        );
    }

    // Boolean flags OR-merge across comment LINES too (the `metadata_from_comments` path), like
    // `@copy`/`@used_as_elem`.
    #[test]
    fn merge_metadata_ors_no_json_schema_export() {
        let lhs = RuleMetadata {
            no_json_schema_export: true,
            ..Default::default()
        };
        let rhs = RuleMetadata::default();
        assert!(merge_metadata(&lhs, &rhs).no_json_schema_export);
        assert!(merge_metadata(&rhs, &lhs).no_json_schema_export);
        assert!(merge_metadata(&lhs, &lhs).no_json_schema_export);
    }

    /// Dispatch takes the FIRST spelling that prefixes the input (`whitespace_then_directive`), which is
    /// order-independent only while no spelling is a prefix of another.
    #[test]
    fn no_directive_spelling_prefixes_another() {
        for a in Directive::ALL {
            for b in Directive::ALL {
                assert!(
                    a == b || !b.spelling().starts_with(a.spelling()),
                    "{} is a prefix of {}: dispatch order would decide which one parses",
                    a.spelling(),
                    b.spelling()
                );
            }
        }
    }

    /// Each directive's canonical spelling parses to metadata that reports exactly that directive:
    /// the spelling (dispatch), the argument parser, and the field classification in
    /// `RuleMetadata::directives` agree. The exhaustive match forces a row for a new directive.
    #[test]
    fn every_directive_round_trips_through_its_field() {
        fn canonical(directive: Directive) -> &'static str {
            match directive {
                Directive::Name => "@name foo",
                Directive::RustName => "@rust_name Foo",
                Directive::Newtype => "@newtype",
                Directive::NoAlias => "@no_alias",
                Directive::UsedAsKey => "@used_as_key",
                Directive::UsedAsElem => "@used_as_elem",
                Directive::Copy => "@copy",
                Directive::RawBytesFlavor => "@raw_bytes_flavor",
                Directive::Ignore => "@ignore",
                Directive::Duplicates => "@duplicates reject",
                Directive::CustomJson => "@custom_json",
                Directive::NoJsonSchemaExport => "@no_json_schema_export",
                Directive::CustomSerialize => "@custom_serialize ser",
                Directive::CustomDeserialize => "@custom_deserialize de",
                Directive::CustomEncodings => "@custom_encodings sz",
                Directive::CustomWireMajor => "@custom_wire_major text",
                Directive::ExternCompanions => "@extern_companions dep_wasm=FooList",
                Directive::Doc => "@doc prose",
            }
        }
        for &directive in Directive::ALL {
            let (rest, metadata) = rule_metadata(canonical(directive)).unwrap();
            assert_eq!(rest, "", "{directive:?}");
            assert_eq!(metadata.directives(), vec![directive]);
            assert_eq!(metadata.all_directives(), vec![directive.spelling()]);
        }
        assert_eq!(
            KNOWN_RULE_METADATA_TAGS,
            Directive::ALL
                .iter()
                .map(|d| d.spelling())
                .collect::<Vec<_>>()
        );
    }
}
