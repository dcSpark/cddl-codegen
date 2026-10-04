use super::source_rule_name_of;
use crate::intermediate::{IntermediateTypes, RustIdent};
use cddl::ast::Type2;

pub(super) fn reject_unsupported_rule_body(
    types: &mut IntermediateTypes,
    type_name: &RustIdent,
    x: &Type2,
) {
    // Unsupported `type2` as a rule body (a bare major-type constraint `#N.M`, a `~name`
    // unwrap, a `&group` / `&( ... )` choice-from-group, the `any` type `#`, …). None has
    // a storable representation at the rule level — reject gracefully, naming the rule by
    // its SOURCE spelling and the offending construct (with an honest hint where one
    // exists), instead of panicking. `finalize` drains the recorded rejection into a
    // graceful `Err` before any generation runs.
    let source_name = source_rule_name_of(types, type_name);
    let (construct, hint) = match x {
        Type2::Unwrap { .. } => (
            "an unwrap (`~name`)".to_string(),
            " — inline the referenced rule's definition manually".to_string(),
        ),
        Type2::DataMajorType { .. } => (
            "a bare major-type constraint (`#N` / `#N.M`)".to_string(),
            String::new(),
        ),
        Type2::Any { .. } => ("the `any` type (`#`)".to_string(), String::new()),
        Type2::ChoiceFromGroup { .. } => (
            "a choice-from-group (`&groupname`)".to_string(),
            String::new(),
        ),
        Type2::ChoiceFromInlineGroup { .. } => (
            "a choice-from-inline-group (`&( ... )`)".to_string(),
            String::new(),
        ),
        other => (format!("this type2 construct ({other:?})"), String::new()),
    };
    types.record_rejection(format!(
        "rule `{source_name}`: {construct} is unsupported as a rule body{hint}"
    ));
}
