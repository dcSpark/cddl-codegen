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
    /// `@raw_bytes_flavor`: valid ONLY on a `_CDDL_CODEGEN_EXTERN_TYPE_` generic rule. When a
    /// generic instance of the tagged extern has any argument that resolves to a
    /// `_CDDL_CODEGEN_RAW_BYTES_TYPE_`, the monomorphized alias references the convention-named
    /// `<ExternName>RawBytes` flavor instead of the plain name. Opt-in (never automatic): a wrapper
    /// bound solely on `RawBytesEncoding` compiles today under the plain name, so auto-flavoring
    /// would silently break working output. See `IntermediateTypes::mark_raw_bytes_flavor`.
    pub raw_bytes_flavor: bool,
    pub custom_json: bool,
    pub custom_serialize: Option<String>,
    pub custom_deserialize: Option<String>,
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
        raw_bytes_flavor: r1.raw_bytes_flavor || r2.raw_bytes_flavor,
        custom_json: r1.custom_json || r2.custom_json,
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
    RawBytesFlavor,
    CustomJson,
    CustomSerialize(String),
    CustomDeserialize(String),
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
                ParseResult::RawBytesFlavor => {
                    base.raw_bytes_flavor = true;
                }
                ParseResult::CustomJson => {
                    base.custom_json = true;
                }
                ParseResult::CustomSerialize(custom_serialize) => {
                    merge_parse_fields!(base.custom_serialize, custom_serialize, "custom_serialize")
                }
                ParseResult::CustomDeserialize(custom_deserialize) => merge_parse_fields!(
                    base.custom_deserialize,
                    custom_deserialize,
                    "custom_deserialize"
                ),
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

fn tag_newtype(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@newtype")(input)?;
    // to get around type annotations
    fn parse_newtype(input: &str) -> IResult<&str, ParseResult> {
        let (input, _) = take_while(char::is_whitespace)(input)?;
        let (input, getter) = take_while1(|ch| !char::is_whitespace(ch) && ch != '@')(input)?;
        Ok((input, ParseResult::NewType(Some(getter.trim().to_owned()))))
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

fn tag_raw_bytes_flavor(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@raw_bytes_flavor")(input)?;

    Ok((input, ParseResult::RawBytesFlavor))
}

fn tag_custom_json(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = tag("@custom_json")(input)?;

    Ok((input, ParseResult::CustomJson))
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
        tag_raw_bytes_flavor,
        tag_custom_json,
        tag_custom_serialize,
        tag_custom_deserialize,
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
pub const KNOWN_RULE_METADATA_TAGS: &[&str] = &[
    "@name",
    "@rust_name",
    "@newtype",
    "@no_alias",
    "@used_as_key",
    "@used_as_elem",
    "@raw_bytes_flavor",
    "@custom_json",
    "@custom_serialize",
    "@custom_deserialize",
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
                comment: None,
            }
        ))
    );
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: true,
                custom_serialize: None,
                custom_deserialize: None,
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
                raw_bytes_flavor: false,
                custom_json: false,
                custom_serialize: Some("foo".to_string()),
                custom_deserialize: Some("bar".to_string()),
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
                raw_bytes_flavor: false,
                custom_json: true,
                custom_serialize: Some("foo".to_string()),
                custom_deserialize: Some("bar".to_string()),
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
