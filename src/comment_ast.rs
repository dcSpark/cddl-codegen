extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    multi::many0,
    IResult,
};

#[derive(Clone, Default, Debug, PartialEq)]
pub struct RuleMetadata {
    pub name: Option<String>,
    /// None = not newtype, Some(Some) = generate getter, Some(None) = no getter
    pub newtype: Option<Option<String>>,
    pub no_alias: bool,
    pub used_as_key: bool,
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
        newtype: merge_metadata_fields!(r1.newtype, r2.newtype, "newtype"),
        no_alias: r1.no_alias || r2.no_alias,
        used_as_key: r1.used_as_key || r2.used_as_key,
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
    DontGenAlias,
    UsedAsKey,
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
                ParseResult::NewType(newtype) => {
                    merge_parse_fields!(base.newtype, newtype, "newtype")
                }
                ParseResult::DontGenAlias => {
                    base.no_alias = true;
                }

                ParseResult::UsedAsKey => {
                    base.used_as_key = true;
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

    Ok((input, ParseResult::UsedAsKey))
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
        tag_newtype,
        tag_no_alias,
        tag_used_as_key,
        tag_custom_json,
        tag_custom_serialize,
        tag_custom_deserialize,
        tag_comment,
    ))(input)?;

    Ok((input, result))
}

fn rule_metadata(input: &str) -> IResult<&str, RuleMetadata> {
    let (input, parse_results) = many0(whitespace_then_tag)(input)?;

    Ok((input, RuleMetadata::from_parse_results(&parse_results)))
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

#[test]
fn parse_comment_name() {
    assert_eq!(
        rule_metadata("@name foo"),
        Ok((
            "",
            RuleMetadata {
                name: Some("foo".to_string()),
                newtype: None,
                no_alias: false,
                used_as_key: false,
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
                newtype: Some(None),
                no_alias: false,
                used_as_key: false,
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
                newtype: Some(Some("custom_getter".to_owned())),
                no_alias: false,
                used_as_key: true,
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
                newtype: Some(Some("custom_getter".to_owned())),
                no_alias: false,
                used_as_key: true,
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
                newtype: Some(None),
                no_alias: false,
                used_as_key: false,
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
                newtype: Some(None),
                no_alias: false,
                used_as_key: true,
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
                newtype: None,
                no_alias: false,
                used_as_key: true,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
                comment: None,
            }
        ))
    );
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
                no_alias: false,
                used_as_key: false,
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
                newtype: None,
                no_alias: true,
                used_as_key: false,
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
                newtype: Some(None),
                no_alias: false,
                used_as_key: false,
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
                newtype: None,
                no_alias: false,
                used_as_key: false,
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
        rule_metadata("@newtype @name baz @custom_serialize foo @custom_deserialize bar @used_as_key @custom_json @doc this is a doc comment"),
        Ok((
            "",
            RuleMetadata {
                name: Some("baz".to_string()),
                newtype: Some(None),
                no_alias: false,
                used_as_key: true,
                custom_json: true,
                custom_serialize: Some("foo".to_string()),
                custom_deserialize: Some("bar".to_string()),
                comment: Some("this is a doc comment".to_string()),
            }
        ))
    );
}
