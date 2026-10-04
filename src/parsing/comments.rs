use cddl::ast::parent::ParentVisitor;
use cddl::ast::{CDDLType, Comments, GroupEntry, MemberKey, Rule, Type2};

pub(super) fn combine_comments<'a>(
    a: &'a Option<Comments>,
    b: &'a Option<Comments>,
) -> Option<Vec<&'a str>> {
    match (
        a.as_ref().map(|comment| comment.0.clone()),
        b.as_ref().map(|comment| comment.0.clone()),
    ) {
        (Some(a), Some(b)) => Some([a, b].concat()),
        (opt_a, opt_b) => opt_a.or(opt_b),
    }
}

fn get_comments_if_group_parent<'a>(
    parent_visitor: &'a ParentVisitor<'a, 'a>,
    cddl_type: &CDDLType<'a, 'a>,
    child: Option<&CDDLType<'a, 'a>>,
    comments_after_group: &Option<Comments<'a>>,
) -> Option<Comments<'a>> {
    if let Some(CDDLType::Group(_)) = child {
        return comments_after_group.clone();
    }
    get_comment_after(
        parent_visitor,
        cddl_type.parent(parent_visitor).unwrap(),
        Some(cddl_type),
    )
}

fn get_comments_if_type_parent<'a>(
    parent_visitor: &'a ParentVisitor<'a, 'a>,
    cddl_type: &CDDLType<'a, 'a>,
    child: Option<&CDDLType<'a, 'a>>,
    comments_after_type: &Option<Comments<'a>>,
) -> Option<Comments<'a>> {
    if let Some(CDDLType::Type(_)) = child {
        return comments_after_type.clone();
    }
    get_comment_after(
        parent_visitor,
        cddl_type.parent(parent_visitor).unwrap(),
        Some(cddl_type),
    )
}

/// Gets the comment(s) that come after a type by parsing the CDDL AST
///
/// (implementation detail) sometimes getting the comment after a type requires walking up the AST
/// This happens when whether or not the type has a comment after it depends in which structure it is embedded in
///    For example, CDDLType::Group has no "comment_after_group" type
///    However, when part of a Type2::Array, it does have a "comment_after_group" embedded inside the Type2
///
/// Note: we do NOT merge comments when the type is coincidentally the last node inside its parent structure
///    For example, the last CDDLType::GroupChoice inside a CDDLType::Group will not return its parent's comment
pub(super) fn get_comment_after<'a>(
    parent_visitor: &'a ParentVisitor<'a, 'a>,
    cddl_type: &CDDLType<'a, 'a>,
    child: Option<&CDDLType<'a, 'a>>,
) -> Option<Comments<'a>> {
    match cddl_type {
        CDDLType::CDDL(_) => None,
        CDDLType::Rule(t) => match t {
            Rule::Type {
                comments_after_rule,
                ..
            } => comments_after_rule.clone(),
            Rule::Group {
                comments_after_rule,
                ..
            } => comments_after_rule.clone(),
        },
        CDDLType::TypeRule(_) => get_comment_after(
            parent_visitor,
            cddl_type.parent(parent_visitor).unwrap(),
            Some(cddl_type),
        ),
        CDDLType::GroupRule(_) => get_comment_after(
            parent_visitor,
            cddl_type.parent(parent_visitor).unwrap(),
            Some(cddl_type),
        ),
        CDDLType::Group(_) => match cddl_type.parent(parent_visitor).unwrap() {
            parent @ CDDLType::GroupEntry(_) => {
                get_comment_after(parent_visitor, parent, Some(cddl_type))
            }
            parent @ CDDLType::Type2(_) => {
                get_comment_after(parent_visitor, parent, Some(cddl_type))
            }
            parent @ CDDLType::MemberKey(_) => {
                get_comment_after(parent_visitor, parent, Some(cddl_type))
            }
            _ => None,
        },
        // TODO: handle child by looking up the group entry in group_entries
        // the expected behavior of this may instead be to combine_comments based off its parents
        // which is a slippery slope in complexity
        CDDLType::GroupChoice(_) => None,
        CDDLType::GenericParams(_) => None,
        CDDLType::GenericParam(t) => {
            if let Some(CDDLType::Identifier(_)) = child {
                return t.comments_after_ident.clone();
            }
            None
        }
        CDDLType::GenericArgs(_) => None,
        CDDLType::GenericArg(t) => {
            if let Some(CDDLType::Type1(_)) = child {
                return t.comments_after_type.clone();
            }
            None
        }
        CDDLType::GroupEntry(t) => match t {
            GroupEntry::ValueMemberKey {
                trailing_comments, ..
            } => trailing_comments.clone(),
            GroupEntry::TypeGroupname {
                trailing_comments, ..
            } => trailing_comments.clone(),
            GroupEntry::InlineGroup {
                comments_after_group,
                ..
            } => {
                if let Some(CDDLType::Group(_)) = child {
                    return comments_after_group.clone();
                }
                None
            }
        },
        CDDLType::Identifier(_) => None, // TODO: recurse up for GenericParam
        CDDLType::Type(_) => None,
        CDDLType::TypeChoice(t) => t.comments_after_type.clone(),
        CDDLType::Type1(t) => {
            // Find the trailing comment that follows this Type1's type2. It can live in a few places:
            if let Some(CDDLType::Type2(_)) = child {
                if let Some(op) = &t.operator {
                    // a control/range operator sits between the type2 and the comment
                    return op.comments_before_operator.clone();
                }
                if t.comments_after_type.is_some() {
                    return t.comments_after_type.clone();
                }
                // No operator and no comment of its own: the comment belongs to an enclosing node.
                // For a type-choice element, cddl attaches it to the parent TypeChoice (which wraps a
                // single Type1), so fall through and ascend to find it there.
            };
            if t.operator.is_none() {
                get_comment_after(
                    parent_visitor,
                    cddl_type.parent(parent_visitor).unwrap(),
                    Some(cddl_type),
                )
            } else {
                None
            }
        }
        CDDLType::Type2(t) => match t {
            Type2::ParenthesizedType {
                comments_after_type,
                ..
            } => get_comments_if_type_parent(parent_visitor, cddl_type, child, comments_after_type),
            Type2::Map {
                comments_after_group,
                ..
            } => {
                get_comments_if_group_parent(parent_visitor, cddl_type, child, comments_after_group)
            }
            Type2::Array {
                comments_after_group,
                ..
            } => {
                get_comments_if_group_parent(parent_visitor, cddl_type, child, comments_after_group)
            }
            Type2::Unwrap { .. } => get_comment_after(
                parent_visitor,
                cddl_type.parent(parent_visitor).unwrap(),
                Some(cddl_type),
            ),
            Type2::ChoiceFromInlineGroup {
                comments_after_group,
                ..
            } => {
                get_comments_if_group_parent(parent_visitor, cddl_type, child, comments_after_group)
            }
            Type2::ChoiceFromGroup { .. } => get_comment_after(
                parent_visitor,
                cddl_type.parent(parent_visitor).unwrap(),
                Some(cddl_type),
            ),
            Type2::TaggedData {
                comments_after_type,
                ..
            } => get_comments_if_type_parent(parent_visitor, cddl_type, child, comments_after_type),
            _ => None,
        },
        CDDLType::Operator(t) => {
            if let Some(CDDLType::RangeCtlOp(_)) = child {
                return t.comments_after_operator.clone();
            }
            if let Some(CDDLType::Type2(t2)) = child {
                // "comments_before_operator" is associated with the 1st type2 and not the second (t.type2)
                if std::ptr::eq(*t2, &t.type2) {
                    return None;
                } else {
                    return t.comments_before_operator.clone();
                }
            }
            None
        }
        CDDLType::Occurrence(t) => t.comments.clone(),
        CDDLType::Occur(_) => None,
        CDDLType::Value(_) => None,
        CDDLType::ValueMemberKeyEntry(_) => None,
        CDDLType::TypeGroupnameEntry(_) => None,
        CDDLType::MemberKey(MemberKey::NonMemberKey {
            comments_after_type_or_group,
            ..
        }) => comments_after_type_or_group.clone(),
        CDDLType::MemberKey(_) => None,
        CDDLType::NonMemberKey(_) => get_comment_after(
            parent_visitor,
            cddl_type.parent(parent_visitor).unwrap(),
            Some(cddl_type),
        ),
        _ => None,
    }
}
