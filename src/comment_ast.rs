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
    pub is_newtype: bool,
    pub no_alias: bool,
    pub used_as_key: bool,
    pub custom_json: bool,
    pub custom_serialize: Option<String>,
    pub custom_deserialize: Option<String>,
}

pub fn merge_metadata(r1: &RuleMetadata, r2: &RuleMetadata) -> RuleMetadata {
    let merged = RuleMetadata {
        name: match (r1.name.as_ref(), r2.name.as_ref()) {
            (Some(val1), Some(val2)) => {
                panic!("Key \"name\" specified twice: {:?} {:?}", val1, val2)
            }
            (val @ Some(_), _) => val.cloned(),
            (_, val) => val.cloned(),
        },
        is_newtype: r1.is_newtype || r2.is_newtype,
        no_alias: r1.no_alias || r2.no_alias,
        used_as_key: r1.used_as_key || r2.used_as_key,
        custom_json: r1.custom_json || r2.custom_json,
        custom_serialize: match (r1.custom_serialize.as_ref(), r2.custom_serialize.as_ref()) {
            (Some(val1), Some(val2)) => {
                panic!(
                    "Key \"custom_serialize\" specified twice: {:?} {:?}",
                    val1, val2
                )
            }
            (val @ Some(_), _) => val.cloned(),
            (_, val) => val.cloned(),
        },
        custom_deserialize: match (
            r1.custom_deserialize.as_ref(),
            r2.custom_deserialize.as_ref(),
        ) {
            (Some(val1), Some(val2)) => {
                panic!(
                    "Key \"custom_deserialize\" specified twice: {:?} {:?}",
                    val1, val2
                )
            }
            (val @ Some(_), _) => val.cloned(),
            (_, val) => val.cloned(),
        },
    };
    merged.verify();
    merged
}

enum ParseResult {
    NewType,
    Name(String),
    DontGenAlias,
    UsedAsKey,
    CustomJson,
    CustomSerialize(String),
    CustomDeserialize(String),
}

impl RuleMetadata {
    fn from_parse_results(results: &[ParseResult]) -> RuleMetadata {
        let mut base = RuleMetadata::default();
        for result in results {
            match result {
                ParseResult::Name(name) => match base.name.as_ref() {
                    Some(old_name) => {
                        panic!("Key \"name\" specified twice: {:?} {:?}", old_name, name)
                    }
                    None => {
                        base.name = Some(name.to_string());
                    }
                },
                ParseResult::NewType => {
                    base.is_newtype = true;
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
                    match base.custom_serialize.as_ref() {
                        Some(old) => {
                            panic!(
                                "Key \"custom_serialize\" specified twice: {:?} {:?}",
                                old, custom_serialize
                            )
                        }
                        None => {
                            base.custom_serialize = Some(custom_serialize.to_string());
                        }
                    }
                }
                ParseResult::CustomDeserialize(custom_deserialize) => {
                    match base.custom_deserialize.as_ref() {
                        Some(old) => {
                            panic!(
                                "Key \"custom_deserialize\" specified twice: {:?} {:?}",
                                old, custom_deserialize
                            )
                        }
                        None => {
                            base.custom_deserialize = Some(custom_deserialize.to_string());
                        }
                    }
                }
            }
        }
        base.verify();
        base
    }

    fn verify(&self) {
        if self.is_newtype && self.no_alias {
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

    Ok((input, ParseResult::NewType))
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
                is_newtype: false,
                no_alias: false,
                used_as_key: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                is_newtype: true,
                no_alias: false,
                used_as_key: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                is_newtype: true,
                no_alias: false,
                used_as_key: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                is_newtype: true,
                no_alias: false,
                used_as_key: true,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                is_newtype: false,
                no_alias: false,
                used_as_key: true,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                is_newtype: true,
                no_alias: false,
                used_as_key: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                is_newtype: false,
                no_alias: true,
                used_as_key: false,
                custom_json: false,
                custom_serialize: None,
                custom_deserialize: None,
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
                is_newtype: true,
                no_alias: false,
                used_as_key: false,
                custom_json: true,
                custom_serialize: None,
                custom_deserialize: None,
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
                is_newtype: false,
                no_alias: false,
                used_as_key: false,
                custom_json: false,
                custom_serialize: Some("foo".to_string()),
                custom_deserialize: Some("bar".to_string()),
            }
        ))
    );
}

// can't have all since @no_alias and @newtype are mutually exclusive
#[test]
fn parse_comment_all_except_no_alias() {
    assert_eq!(
        rule_metadata("@newtype @name baz @custom_serialize foo @custom_deserialize bar @used_as_key @custom_json"),
        Ok((
            "",
            RuleMetadata {
                name: Some("baz".to_string()),
                is_newtype: true,
                no_alias: false,
                used_as_key: true,
                custom_json: true,
                custom_serialize: Some("foo".to_string()),
                custom_deserialize: Some("bar".to_string()),
            }
        ))
    );
}
