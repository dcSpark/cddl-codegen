extern crate nom;
use nom::{
  IResult,
  bytes::complete::{tag, take_while1, take_while},
};

#[derive(Default, Debug, PartialEq)]
pub struct RuleMetadata {
  pub name: Option<String>,
}

fn merge_metadata(r1: &RuleMetadata, r2: &RuleMetadata) -> RuleMetadata {
    RuleMetadata {
        name: match (r1.name.as_ref(), r2.name.as_ref()) {
            (Some(val1), Some(val2)) => panic!("Key \"name\" specified twice: {:?} {:?}", val1, val2),
            (val@Some(_), _) => val.cloned(),
            (_, val) => val.cloned()
        }
    }
}

fn rule_metadata(input: &str) -> IResult<&str, RuleMetadata> {
  let (input, _) = take_while(char::is_whitespace)(input)?;
  let (input, _) = tag("@name")(input)?;
  let (input, _) = take_while(char::is_whitespace)(input)?;
  let (input, name) = take_while1(|ch| !char::is_whitespace(ch))(input)?;

  Ok((input, RuleMetadata { name: Some(name.to_string()) }))
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
  assert_eq!(rule_metadata("@name asdf"), Ok(("", RuleMetadata {
    name: Some("asdf".to_string()),
  })));
}