extern crate nom;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
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

/// The `@extern_companions <path>=<Class>[,<Class>…]` declaration on a LOCALLY-marked extern rule:
/// the sibling wasm crate (or module path) where the named STRUCTURAL companion classes of this
/// extern type already exist, so the generator references them instead of minting duplicates.
///
/// `path_prefix` is emitted verbatim as the `use <prefix>::<Class>;` head; `classes` are the exact
/// generator-derived structural class names that defer (`TransactionMetadatumList`,
/// `MapFooToBar`, …). Only LISTED names defer — an unlisted structural companion of the same type
/// still mints locally, which is what lets a consumer borrow one family and own another.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternCompanions {
    pub path_prefix: String,
    pub classes: std::collections::BTreeSet<String>,
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
    /// a named error — never a distant consumer's. Rides the extern-interface seam like
    /// `@raw_bytes_flavor`, so `--extern-import` consumers inherit it. On any other placement it is a
    /// graceful parse-time rejection (never silently ignored). See `IntermediateTypes::is_copy_extern`.
    pub copy: bool,
    /// `@raw_bytes_flavor`: valid ONLY on a `_CDDL_CODEGEN_EXTERN_TYPE_` generic rule. When a
    /// generic instance of the tagged extern has any argument that resolves to a
    /// `_CDDL_CODEGEN_RAW_BYTES_TYPE_`, the monomorphized alias references the convention-named
    /// `<ExternName>RawBytes` flavor instead of the plain name. Opt-in (never automatic): a wrapper
    /// bound solely on `RawBytesEncoding` compiles today under the plain name, so auto-flavoring
    /// would silently break working output. See `IntermediateTypes::mark_raw_bytes_flavor`.
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
    /// `@extern_companions <path>=<Class>[,<Class>…]`: valid ONLY on a LOCALLY-scoped
    /// `_CDDL_CODEGEN_EXTERN_TYPE_` rule. Declares that the named structural wasm companion classes
    /// of this extern type already exist in a sibling wasm crate, so the generator emits
    /// `use <path>::<Class>;` and references them instead of minting its own `#[wasm_bindgen]`
    /// duplicates (which duplicate-symbol at link when both crates enter one cdylib). Only listed
    /// classes defer. Inert without `--wasm` (the classes it names are a wasm-boundary concern). On
    /// any other placement — a non-extern rule, or a DEP-scoped extern, which
    /// `--extern-wrapper-index` / `--workspace-dep` already own — it is a graceful parse-time
    /// rejection, never silently ignored. See [`ExternCompanions`] and
    /// `IntermediateTypes::extern_companions`.
    pub extern_companions: Option<ExternCompanions>,
    pub comment: Option<String>,
}

macro_rules! merge_metadata_fields {
    ($lhs:expr, $rhs:expr, $field_name:literal) => {
        match ($lhs.as_ref(), $rhs.as_ref()) {
            (Some(val1), Some(val2)) => {
                panic!(
                    concat!("Key \"", $field_name, "\" specified twice: {:?} {:?}"),
                    val1, val2
                )
            }
            (val @ Some(_), _) => val.cloned(),
            (_, val) => val.cloned(),
        }
    };
}

pub fn merge_metadata(r1: &RuleMetadata, r2: &RuleMetadata) -> RuleMetadata {
    let merged = RuleMetadata {
        name: merge_metadata_fields!(r1.name, r2.name, "name"),
        rust_name: merge_metadata_fields!(r1.rust_name, r2.rust_name, "rust_name"),
        newtype: merge_metadata_fields!(r1.newtype, r2.newtype, "newtype"),
        no_alias: r1.no_alias || r2.no_alias,
        key_demand: merge_key_demand(r1.key_demand, r2.key_demand),
        used_as_elem: r1.used_as_elem || r2.used_as_elem,
        copy: r1.copy || r2.copy,
        raw_bytes_flavor: r1.raw_bytes_flavor || r2.raw_bytes_flavor,
        ignore: r1.ignore || r2.ignore,
        duplicates: merge_metadata_fields!(r1.duplicates, r2.duplicates, "duplicates"),
        custom_json: r1.custom_json || r2.custom_json,
        no_json_schema_export: r1.no_json_schema_export || r2.no_json_schema_export,
        custom_serialize: merge_metadata_fields!(
            r1.custom_serialize,
            r2.custom_serialize,
            "custom_serialize"
        ),
        custom_deserialize: merge_metadata_fields!(
            r1.custom_deserialize,
            r2.custom_deserialize,
            "custom_deserialize"
        ),
        extern_companions: merge_metadata_fields!(
            r1.extern_companions,
            r2.extern_companions,
            "extern_companions"
        ),
        comment: merge_metadata_fields!(r1.comment, r2.comment, "comment"),
    };
    merged.verify();
    merged
}

enum ParseResult {
    NewType(Option<String>),
    Name(String),
    RustName(String),
    DontGenAlias,
    UsedAsKey(DemandSet),
    UsedAsElem,
    Copy,
    RawBytesFlavor,
    Ignore,
    Duplicates(DuplicatesPolicy),
    CustomJson,
    NoJsonSchemaExport,
    CustomSerialize(String),
    CustomDeserialize(String),
    ExternCompanionsTag(ExternCompanions),
    Comment(String),
}

macro_rules! merge_parse_fields {
    ($base:expr, $new:expr, $field_name:literal) => {
        match $base.as_ref() {
            Some(old) => {
                panic!(
                    concat!("Key \"", $field_name, "\" specified twice: {:?} {:?}"),
                    old, $new
                )
            }
            None => {
                $base = Some($new.to_owned());
            }
        }
    };
}

impl RuleMetadata {
    fn from_parse_results(results: &[ParseResult]) -> RuleMetadata {
        let mut base = RuleMetadata::default();
        for result in results {
            match result {
                ParseResult::Name(name) => merge_parse_fields!(base.name, name, "name"),
                ParseResult::RustName(rust_name) => {
                    merge_parse_fields!(base.rust_name, rust_name, "rust_name")
                }
                ParseResult::NewType(newtype) => {
                    merge_parse_fields!(base.newtype, newtype, "newtype")
                }
                ParseResult::DontGenAlias => {
                    base.no_alias = true;
                }

                ParseResult::UsedAsKey(demand) => {
                    base.key_demand = Some(base.key_demand.unwrap_or_default().union(*demand));
                }
                ParseResult::UsedAsElem => {
                    base.used_as_elem = true;
                }
                ParseResult::Copy => {
                    base.copy = true;
                }
                ParseResult::RawBytesFlavor => {
                    base.raw_bytes_flavor = true;
                }
                ParseResult::Ignore => {
                    base.ignore = true;
                }
                ParseResult::Duplicates(policy) => {
                    merge_parse_fields!(base.duplicates, policy, "duplicates")
                }
                ParseResult::CustomJson => {
                    base.custom_json = true;
                }
                ParseResult::NoJsonSchemaExport => {
                    base.no_json_schema_export = true;
                }
                ParseResult::CustomSerialize(custom_serialize) => {
                    merge_parse_fields!(base.custom_serialize, custom_serialize, "custom_serialize")
                }
                ParseResult::CustomDeserialize(custom_deserialize) => merge_parse_fields!(
                    base.custom_deserialize,
                    custom_deserialize,
                    "custom_deserialize"
                ),
                ParseResult::ExternCompanionsTag(companions) => {
                    merge_parse_fields!(base.extern_companions, companions, "extern_companions")
                }
                ParseResult::Comment(comment) => {
                    merge_parse_fields!(base.comment, comment, "comment")
                }
            }
        }
        base.verify();
        base
    }

    fn verify(&self) {
        if self.newtype.is_some() && self.no_alias {
            // this would make no sense anyway as with newtype we're already not making an alias
            panic!("cannot use both @newtype and @no_alias on the same alias");
        }
    }

    /// The `@`-spellings of every rule-level directive set on this metadata, EXCLUDING the two a
    /// type-choice VARIANT position legitimately consumes (`@name` names the variant, `@doc`
    /// documents it — see `parsing::create_variants_from_type_choices`, which reads exactly those
    /// two fields and discards the rest).
    ///
    /// Exists for one caller: the non-last-arm rejection in `parsing::parse_type_choices`. The
    /// exhaustive destructuring below is load-bearing — a new `RuleMetadata` field fails to compile
    /// here until its author decides whether it is rule-level (add it) or variant-legal (bind it to
    /// `_`), which is the forcing function a hand-maintained list cannot provide.
    pub fn non_variant_directives(&self) -> Vec<&'static str> {
        let Self {
            name: _,
            comment: _,
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
            extern_companions,
        } = self;
        let mut found = Vec::new();
        if rust_name.is_some() {
            found.push("@rust_name");
        }
        if newtype.is_some() {
            found.push("@newtype");
        }
        if *no_alias {
            found.push("@no_alias");
        }
        if key_demand.is_some() {
            found.push("@used_as_key");
        }
        if *used_as_elem {
            found.push("@used_as_elem");
        }
        if *copy {
            found.push("@copy");
        }
        if *raw_bytes_flavor {
            found.push("@raw_bytes_flavor");
        }
        if *ignore {
            found.push("@ignore");
        }
        if duplicates.is_some() {
            found.push("@duplicates");
        }
        if *custom_json {
            found.push("@custom_json");
        }
        if *no_json_schema_export {
            found.push("@no_json_schema_export");
        }
        if custom_serialize.is_some() {
            found.push("@custom_serialize");
        }
        if custom_deserialize.is_some() {
            found.push("@custom_deserialize");
        }
        if extern_companions.is_some() {
            found.push("@extern_companions");
        }
        found
    }
}

fn tag_name(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@name")(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, name) = take_while1(|ch| !char::is_whitespace(ch))(input)?;

    Ok((input, ParseResult::Name(name.to_string())))
}

fn tag_rust_name(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@rust_name")(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, rust_name) = take_while1(|ch| !char::is_whitespace(ch))(input)?;

    Ok((input, ParseResult::RustName(rust_name.to_string())))
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

fn tag_newtype(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@newtype")(input)?;
    // to get around type annotations
    fn parse_newtype(input: &str) -> IResult<&str, ParseResult> {
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
        Ok((input, ParseResult::NewType(Some(getter.to_owned()))))
    }
    match parse_newtype(input) {
        Ok(ret) => Ok(ret),
        Err(_) => Ok((input.trim_start(), ParseResult::NewType(None))),
    }
}

fn tag_no_alias(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@no_alias")(input)?;

    Ok((input, ParseResult::DontGenAlias))
}

fn tag_used_as_key(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@used_as_key")(input)?;
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
    Ok((rest, ParseResult::UsedAsKey(demand)))
}

fn tag_used_as_elem(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@used_as_elem")(input)?;

    Ok((input, ParseResult::UsedAsElem))
}

fn tag_copy(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@copy")(input)?;

    Ok((input, ParseResult::Copy))
}

fn tag_raw_bytes_flavor(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@raw_bytes_flavor")(input)?;

    Ok((input, ParseResult::RawBytesFlavor))
}

fn tag_ignore(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@ignore")(input)?;

    Ok((input, ParseResult::Ignore))
}

fn tag_duplicates(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@duplicates")(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    // `@duplicates` requires exactly one argument from a strict vocabulary. A missing or unknown
    // argument is a PANIC (matching `@used_as_key`'s unknown-flavor handling): the comment parser
    // otherwise swallows nom errors (`metadata_from_comments`), so a soft failure here would silently
    // drop the whole line's metadata — the exact distant-failure class this DSL exists to kill.
    if input.is_empty() || input.starts_with('@') {
        panic!(
            "@duplicates: missing required argument; expected `preserve` or `reject` \
             (e.g. `@duplicates reject`)."
        );
    }
    let (rest, word) = take_while1(|ch| !char::is_whitespace(ch) && ch != '@')(input)?;
    let policy = match word {
        "preserve" => DuplicatesPolicy::Preserve,
        "reject" => DuplicatesPolicy::Reject,
        other => panic!(
            "@duplicates: unknown argument {other:?}; expected `preserve` or `reject`. \
             (Trailing prose is not allowed after `@duplicates` — put it in `@doc`.)"
        ),
    };
    Ok((rest, ParseResult::Duplicates(policy)))
}

fn tag_custom_json(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@custom_json")(input)?;

    Ok((input, ParseResult::CustomJson))
}

fn tag_no_json_schema_export(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@no_json_schema_export")(input)?;

    Ok((input, ParseResult::NoJsonSchemaExport))
}

fn tag_custom_serialize(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@custom_serialize")(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, custom_serialize) = take_while1(|ch| !char::is_whitespace(ch))(input)?;

    Ok((
        input,
        ParseResult::CustomSerialize(custom_serialize.to_string()),
    ))
}

fn tag_custom_deserialize(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@custom_deserialize")(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, custom_deserialize) = take_while1(|ch| !char::is_whitespace(ch))(input)?;

    Ok((
        input,
        ParseResult::CustomDeserialize(custom_deserialize.to_string()),
    ))
}

/// Whether `path` is a `::`-separated chain of rust identifiers — the shape the `use <path>::<Class>;`
/// head must have, since it is emitted verbatim into the generated wasm crate. Deliberately syntactic
/// only (like `@newtype`'s getter bound): whether the crate exists is the CONSUMER'S COMPILE to
/// decide, which is this directive's whole trust-and-compile contract. What this bounds is the token
/// that would otherwise reach `rustfmt` as unparseable source.
fn is_rust_path(path: &str) -> bool {
    !path.is_empty() && path.split("::").all(is_rust_ident)
}

fn tag_extern_companions(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@extern_companions")(input)?;
    let (input, _) = take_while(char::is_whitespace)(input)?;
    // Exactly one REQUIRED argument, in a strict shape. A missing or malformed argument is a PANIC
    // (matching `@duplicates`/`@used_as_key`): the comment parser otherwise swallows nom errors
    // (`metadata_from_comments`), so a soft failure would silently drop the whole line's metadata —
    // here that means silently re-minting the very classes the directive exists to suppress, whose
    // only symptom is a `rust-lld: duplicate symbol` in a DIFFERENT crate's link. Loud at the cause.
    if input.is_empty() || input.starts_with('@') {
        panic!(
            "@extern_companions: missing required argument; expected \
             `<use_path_prefix>=<Class>[,<Class>…]` (e.g. \
             `@extern_companions cml_chain_wasm=TransactionMetadatumList`)."
        );
    }
    let (rest, arg) = take_while1(|ch| !char::is_whitespace(ch) && ch != '@')(input)?;
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
    let mut classes = std::collections::BTreeSet::new();
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
    Ok((
        rest,
        ParseResult::ExternCompanionsTag(ExternCompanions {
            path_prefix: path_prefix.to_owned(),
            classes,
        }),
    ))
}

fn tag_comment(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@doc")(input)?;
    let (input, comment) = take_while1(|c| c != '@')(input)?;

    Ok((input, ParseResult::Comment(comment.trim().to_string())))
}

fn whitespace_then_tag(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, result) = alt((
        tag_name,
        tag_rust_name,
        tag_newtype,
        tag_no_alias,
        tag_used_as_key,
        tag_used_as_elem,
        tag_copy,
        tag_raw_bytes_flavor,
        tag_ignore,
        tag_duplicates,
        tag_custom_json,
        // No prefix relation with any sibling tag (nom `tag` = prefix match): `@no_alias` is not a
        // prefix of `@no_json_schema_export` (they diverge at `a` vs `j`) and vice versa, so the
        // `alt` order between the two is free.
        tag_no_json_schema_export,
        tag_custom_serialize,
        tag_custom_deserialize,
        tag_extern_companions,
        tag_comment,
    ))
    .parse(input)?;

    Ok((input, result))
}

fn rule_metadata(input: &str) -> IResult<&str, RuleMetadata> {
    let (input, parse_results) = many0(whitespace_then_tag).parse(input)?;

    Ok((input, RuleMetadata::from_parse_results(&parse_results)))
}

/// The complete `@`-token vocabulary the rule-metadata DSL recognizes — the `tag("@…")` literals in
/// `whitespace_then_tag`'s `alt`, surfaced as data for the extern-interface strict `@`-scan
/// (`api::scan_extern_import_seam`), which hard-errors on any `@`-token outside this set. Because the
/// tags prefix-match (nom `tag`), the scan treats a known tag as a PREFIX of the scanned token —
/// `@namefoo` credits `@name` in both places. Keep in lockstep with the `tag_*` fns above (and
/// `cddl-matrix/corpus_detect.ts`'s `MIRRORED_DIRECTIVES` mirror). Not `tag("@…")`-wrapped, so it does
/// NOT feed corpus_detect's `tag("@…")`-literal drift tripwire.
///
/// Adding a directive here is the START of a checklist, not the whole of it: once the directive is
/// also DOCUMENTED in `docs/docs/comment_dsl.mdx`, `cddl-matrix/verify.ts`'s forward completeness
/// lint hard-fails until it has a `features/cddl_codegen.toml` row and a minted verdict, and that
/// lint is FULL-tier — so a local/fast tier stays green while the full tier is red. The whole chain
/// (feature row, decode-catalog row, ingredients, the vendor-count pin) is written down in
/// `cddl-matrix/README.md` § "Registering a new vendor (CDDL_CODEGEN) feature row"; read it before
/// deferring any part of it.
pub const KNOWN_RULE_METADATA_TAGS: &[&str] = &[
    "@name",
    "@rust_name",
    "@newtype",
    "@no_alias",
    "@used_as_key",
    "@used_as_elem",
    "@copy",
    "@raw_bytes_flavor",
    "@ignore",
    "@duplicates",
    "@custom_json",
    "@no_json_schema_export",
    "@custom_serialize",
    "@custom_deserialize",
    "@extern_companions",
    "@doc",
];

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

#[test]
fn parse_comment_name() {
    assert_eq!(
        rule_metadata("@name foo"),
        Ok((
            "",
            RuleMetadata {
                name: Some("foo".to_string()),
                rust_name: None,
                newtype: None,
                no_alias: false,
                key_demand: None,
                used_as_elem: false,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                name: None,
                rust_name: None,
                newtype: Some(None),
                no_alias: false,
                key_demand: None,
                used_as_elem: false,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                name: None,
                rust_name: None,
                newtype: Some(Some("custom_getter".to_owned())),
                no_alias: false,
                key_demand: Some(DemandSet {
                    bare: true,
                    hash: false,
                    ord: false
                }),
                used_as_elem: false,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                name: None,
                rust_name: None,
                newtype: Some(Some("custom_getter".to_owned())),
                no_alias: false,
                key_demand: Some(DemandSet {
                    bare: true,
                    hash: false,
                    ord: false
                }),
                used_as_elem: false,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                rust_name: None,
                newtype: Some(None),
                no_alias: false,
                key_demand: None,
                used_as_elem: false,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                rust_name: None,
                newtype: Some(None),
                no_alias: false,
                key_demand: Some(DemandSet {
                    bare: true,
                    hash: false,
                    ord: false
                }),
                used_as_elem: false,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                name: None,
                rust_name: None,
                newtype: None,
                no_alias: false,
                key_demand: Some(DemandSet {
                    bare: true,
                    hash: false,
                    ord: false
                }),
                used_as_elem: false,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                name: None,
                rust_name: None,
                newtype: None,
                no_alias: false,
                key_demand: None,
                used_as_elem: true,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                name: None,
                rust_name: None,
                newtype: None,
                no_alias: false,
                key_demand: Some(DemandSet {
                    bare: true,
                    hash: false,
                    ord: false
                }),
                used_as_elem: true,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                name: None,
                rust_name: None,
                newtype: None,
                no_alias: false,
                key_demand: Some(DemandSet {
                    bare: true,
                    hash: false,
                    ord: false
                }),
                used_as_elem: true,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                name: None,
                rust_name: None,
                newtype: Some(Some("custom_getter".to_owned())),
                no_alias: false,
                key_demand: None,
                used_as_elem: true,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                name: None,
                rust_name: None,
                newtype: Some(Some("custom_getter".to_owned())),
                no_alias: false,
                key_demand: None,
                used_as_elem: true,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                rust_name: None,
                newtype: Some(None),
                no_alias: false,
                key_demand: None,
                used_as_elem: false,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                rust_name: None,
                newtype: None,
                no_alias: true,
                key_demand: None,
                used_as_elem: false,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                name: None,
                rust_name: None,
                newtype: Some(None),
                no_alias: false,
                key_demand: None,
                used_as_elem: false,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: true,
                no_json_schema_export: false,
                custom_serialize: None,
                custom_deserialize: None,
                extern_companions: None,
                comment: None,
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
                name: None,
                rust_name: None,
                newtype: None,
                no_alias: false,
                key_demand: None,
                used_as_elem: false,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: false,
                no_json_schema_export: false,
                custom_serialize: Some("foo".to_string()),
                custom_deserialize: Some("bar".to_string()),
                extern_companions: None,
                comment: None,
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
                rust_name: None,
                newtype: Some(None),
                no_alias: false,
                key_demand: Some(DemandSet {
                    bare: true,
                    hash: false,
                    ord: false
                }),
                used_as_elem: true,
                copy: false,
                raw_bytes_flavor: false,
                ignore: false,
                duplicates: None,
                custom_json: true,
                no_json_schema_export: false,
                custom_serialize: Some("foo".to_string()),
                custom_deserialize: Some("bar".to_string()),
                extern_companions: None,
                comment: Some("this is a doc comment".to_string()),
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
// prefix-match `alt` has no sibling that shadows it). `@custom_json` is the deliberate neighbour:
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
