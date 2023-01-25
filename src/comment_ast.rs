extern crate nom;
use nom::{
    IResult,
    bytes::complete::{tag, take_while1, take_while}, branch::alt, multi::many0,
};

#[derive(Default, Debug, PartialEq)]
pub struct RuleMetadata {
    pub name: Option<String>,
    pub is_newtype: bool,
    pub no_alias: bool,
}

pub fn merge_metadata(r1: &RuleMetadata, r2: &RuleMetadata) -> RuleMetadata {
    RuleMetadata {
        name: match (r1.name.as_ref(), r2.name.as_ref()) {
            (Some(val1), Some(val2)) => panic!("Key \"name\" specified twice: {:?} {:?}", val1, val2),
            (val@Some(_), _) => val.cloned(),
            (_, val) => val.cloned()
        },
        is_newtype: r1.is_newtype || r2.is_newtype,
        no_alias: r1.no_alias || r2.no_alias,
    }
}

enum ParseResult {
    NewType,
    Name(String),
    DontGenAlias,
}

impl RuleMetadata {
    fn from_parse_results(results: &[ParseResult]) -> RuleMetadata {
        let mut base = RuleMetadata::default();
        for result in results {
            match result {
                ParseResult::Name(name) => {
                    match base.name.as_ref() {
                        Some(old_name) => panic!("Key \"name\" specified twice: {:?} {:?}", old_name, name),
                        None => { base.name = Some(name.to_string()); }
                    }
                },
                ParseResult::NewType => { base.is_newtype = true; },
                ParseResult::DontGenAlias => { base.no_alias = true; },
            }
        }
        base
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

fn whitespace_then_tag(input: &str) -> IResult<&str, ParseResult> {
    let (input, _) = take_while(char::is_whitespace)(input)?;
    let (input, result) = alt((tag_name, tag_newtype, tag_no_alias))(input)?;

    Ok((input, result))
}

fn rule_metadata(input: &str) -> IResult<&str, RuleMetadata> {
    let (input, parse_results) = many0(whitespace_then_tag)(input)?;
    

    Ok((input, RuleMetadata::from_parse_results(&parse_results)))
}


impl <'a> From<Option<&'a cddl::ast::Comments<'a>>> for RuleMetadata {
    fn from(comments: Option<&'a cddl::ast::Comments<'a>>) -> RuleMetadata {
        match comments {
            None => RuleMetadata::default(),
            Some(c) => metadata_from_comments(&c.0)
        }
    }
}

pub fn metadata_from_comments(comments: &[&str]) -> RuleMetadata {
    let mut result = RuleMetadata::default();
    for comment in comments {
        if let Ok(comment_metadata) = rule_metadata(comment) {
            result = merge_metadata(&result, &comment_metadata.1);
        }
    };
    result
}

#[test]
fn parse_comment_name() {
    assert_eq!(rule_metadata("@name foo"), Ok(("", RuleMetadata {
        name: Some("foo".to_string()),
        is_newtype: false,
        no_alias: false,
    })));
}

#[test]
fn parse_comment_newtype() {
    assert_eq!(rule_metadata("@newtype"), Ok(("", RuleMetadata {
        name: None,
        is_newtype: true,
        no_alias: false,
    })));
}

#[test]
fn parse_comment_newtype_and_name() {
    assert_eq!(rule_metadata("@newtype @name foo"), Ok(("", RuleMetadata {
        name: Some("foo".to_string()),
        is_newtype: true,
        no_alias: false,
    })));
}

#[test]
fn parse_comment_newtype_and_name_inverse() {
    assert_eq!(rule_metadata("@name foo @newtype"), Ok(("", RuleMetadata {
        name: Some("foo".to_string()),
        is_newtype: true,
        no_alias: false,
    })));
}

#[test]
fn parse_comment_all() {
    assert_eq!(rule_metadata("@no_alias @name foo @newtype"), Ok(("", RuleMetadata {
        name: Some("foo".to_string()),
        is_newtype: true,
        no_alias: true,
    })));
}
