//! Source-buffer guards for directives absent from the pinned parser's AST slots.
//! Keep source traversal, span slicing and diagnostic order intact.

use super::directives::{inline_group_occurrence_directive_message, quoted_directive_list};
use crate::comment_ast::{RuleMetadata, merge_metadata, metadata_from_comments};
use crate::intermediate::{CDDLIdent, RustIdent};
use cddl::ast::{GroupEntry, Occur, Rule, Type2, TypeGroupnameEntry};
use std::collections::BTreeMap;

/// A graceful-rejection message for the ONE group-rule spelling the pinned `cddl` parser cannot
/// bind a rule-position directive in — a trailing comment after a closing paren that sits on its
/// own line (`grp = (\n a: uint\n) ; @rust_name Foo`) — else `None`. First offence in source order
/// (rules are walked in source order, so the choice is deterministic), mirroring
/// `api::scan_module_directives`, which is also a pre-IR whole-buffer scan that stops at the first
/// bad line.
///
/// Why a source-buffer scan rather than an AST read: there IS no AST slot to read. The parser's
/// comment binding is a source-position trivia merge, and `GroupEntry::InlineGroup` emits no
/// trailing anchor of the group rule's own on that line, so the comment is merged into the
/// FOLLOWING rule's `comments_before_rule` — or reaches no slot at all when the group rule is the
/// document's last. Nothing reads either position, so honoring the spelling is impossible at this
/// pin and the only honest alternative to silence is refusal. `Rule::Group`'s span is what makes the
/// scan exact: it ends just past the closing `)`, BEFORE the trailing comment, for every group-rule
/// spelling (`Rule::Type`'s span, by contrast, INCLUDES its trailing comment — which is also why
/// type rules are never scanned here: their multi-line trailing comment lands in
/// `comments_after_type` and is honored).
///
/// ALL directives refuse uniformly, including the four `group_rule_pin_metadata`'s callers honor in
/// the bound position (`@rust_name`, `@no_json_schema_export`, `@custom_json`, `@used_as_key`): the
/// parser delivers none of them here, so there is nothing to sort. A comment that parses to
/// `RuleMetadata::default()` is prose and is left alone.
pub(crate) fn multiline_group_trailing_directive_rejection(
    cddl: &cddl::ast::CDDL,
    buffer: &str,
) -> Option<String> {
    cddl.rules.iter().find_map(|rule| match rule {
        Rule::Group { span, .. } => {
            multiline_group_trailing_directive_offence(&rule.name(), *span, buffer)
        }
        Rule::Type { .. } => None,
    })
}

/// The pinned parser drops a comment on the line after a repeated inline group's closing `)` and
/// before its enclosing array's `]`: it reaches neither the inline group's `comments_after_group`
/// slot nor its `OptionalComma`. The ordinary parser seam still reads both slots (so a future parser
/// fix is honored as a graceful rejection there); this source-span guard covers the current orphaned
/// spelling before IR construction can silently lose its directives.
pub(crate) fn inline_group_occurrence_trailing_directive_rejection(
    cddl: &cddl::ast::CDDL,
    buffer: &str,
) -> Option<String> {
    cddl.rules.iter().find_map(|rule| {
        let Rule::Type { rule, .. } = rule else {
            return None;
        };
        let [choice] = rule.value.type_choices.as_slice() else {
            return None;
        };
        if choice.type1.operator.is_some() {
            return None;
        }
        let Type2::Array { group, .. } = &choice.type1.type2 else {
            return None;
        };
        let [group_choice] = group.group_choices.as_slice() else {
            return None;
        };
        let [
            (
                GroupEntry::InlineGroup {
                    occur: Some(occur),
                    span: inline_span,
                    ..
                },
                _,
            ),
        ] = group_choice.group_entries.as_slice()
        else {
            return None;
        };
        let metadata = trailing_comment_metadata(buffer.get(inline_span.1..group_choice.span.1)?);
        let directives = metadata.all_directives();
        if directives.is_empty() {
            return None;
        }
        let found = quoted_directive_list(&directives, ", ");
        if matches!(
            occur.occur,
            Occur::Exact {
                lower: Some(1),
                upper: Some(1),
                ..
            }
        ) {
            return Some(inline_group_exact_once_directive_message(
                &rule.name.to_string(),
                &found,
            ));
        }
        let item_ident = RustIdent::new(CDDLIdent::new(format!("{}Item", rule.name)));
        Some(inline_group_occurrence_directive_message(
            &rule.name.to_string(),
            item_ident.as_ref(),
            &found,
        ))
    })
}

fn inline_group_exact_once_directive_message(source: &str, found: &str) -> String {
    format!(
        "rule `{source}`: the `1*1` inline group's entry carries {found}, but that occurrence \
         flattens directly into the owner record and synthesizes no separate item type. Directives \
         and documentation that apply to the outer rule belong after the closing `]` (for example \
         `{source} = [1*1 (a: uint, b: tstr)] ; @doc <text>`); to configure a separately named \
         type, define a named plain group and use that rule instead."
    )
}

/// The same pinned-parser hole exists for a direct named plain-group occurrence (`[* pair]`): a
/// comment after `pair` and before the enclosing `]` is absent from its ordinary entry slots. Keep
/// this source guard restricted to a group name declared in this input, so it cannot broaden the
/// rejection to ordinary homogeneous array elements that do not use the flat-group carrier.
pub(crate) fn named_plain_group_occurrence_trailing_directive_rejection(
    cddl: &cddl::ast::CDDL,
    buffer: &str,
) -> Option<String> {
    let mut plain_group_sources = cddl
        .rules
        .iter()
        .filter_map(|rule| match rule {
            Rule::Group { rule, .. } => Some(rule.name.to_string()),
            Rule::Type { .. } => None,
        })
        .map(|name| (name.clone(), name))
        .collect::<BTreeMap<_, _>>();
    let simple_aliases = cddl
        .rules
        .iter()
        .filter_map(|rule| {
            let Rule::Type { rule, .. } = rule else {
                return None;
            };
            let [choice] = rule.value.type_choices.as_slice() else {
                return None;
            };
            if choice.type1.operator.is_some() {
                return None;
            }
            let Type2::Typename {
                ident,
                generic_args,
                ..
            } = &choice.type1.type2
            else {
                return None;
            };
            generic_args
                .is_none()
                .then(|| (rule.name.to_string(), ident.to_string()))
        })
        .collect::<BTreeMap<_, _>>();
    // Alias resolution is closed and monotonic: only aliases whose chain reaches an authored plain
    // group are admitted, so a normal homogeneous named-type array remains outside this guard.
    loop {
        let newly_resolved = simple_aliases
            .iter()
            .filter_map(|(alias, target)| {
                (!plain_group_sources.contains_key(alias))
                    .then(|| {
                        plain_group_sources
                            .get(target)
                            .map(|source| (alias.clone(), source.clone()))
                    })
                    .flatten()
            })
            .collect::<Vec<_>>();
        if newly_resolved.is_empty() {
            break;
        }
        plain_group_sources.extend(newly_resolved);
    }
    cddl.rules.iter().find_map(|rule| {
        let Rule::Type { rule, .. } = rule else {
            return None;
        };
        let [choice] = rule.value.type_choices.as_slice() else {
            return None;
        };
        if choice.type1.operator.is_some() {
            return None;
        }
        let Type2::Array { group, .. } = &choice.type1.type2 else {
            return None;
        };
        let [group_choice] = group.group_choices.as_slice() else {
            return None;
        };
        let [(
            GroupEntry::TypeGroupname {
                ge:
                    TypeGroupnameEntry {
                        occur: Some(occur),
                        name,
                        ..
                    },
                ..
            },
            _,
        )] = group_choice.group_entries.as_slice()
        else {
            return None;
        };
        let group_source = plain_group_sources.get(&name.to_string())?;
        if matches!(
            occur.occur,
            Occur::Exact {
                lower: Some(1),
                upper: Some(1),
                ..
            }
        )
        {
            return None;
        }
        let metadata = trailing_comment_metadata(buffer.get(name.span.1..group_choice.span.1)?);
        let directives = metadata.all_directives();
        if directives.is_empty() {
            return None;
        }
        let found = quoted_directive_list(&directives, ", ");
        let owner = rule.name.to_string();
        let group_source = group_source.clone();
        Some(format!(
            "rule `{owner}`: the repeated named plain-group occurrence `{group_source}` carries {}, \
             but that entry declares no field or independently configurable type. Directives and \
             documentation that apply to the outer rule belong after the closing `]` (for example \
             `{owner} = [* {group_source}] ; @doc <text>`); directives for the repeated item belong \
             on the named plain-group rule `{group_source} = (…)`. Remove `@name` here: it cannot \
             rename either generated surface.",
            found
        ))
    })
}

/// The four-part detection condition for one group rule, split out so it is unit-testable against a
/// hand-built (span, buffer) pair. `span` is the rule's own span into `buffer`.
fn multiline_group_trailing_directive_offence(
    name: &str,
    span: cddl::ast::Span,
    buffer: &str,
) -> Option<String> {
    // (1) the span's final character is the group's closing paren.
    let through_paren = buffer.get(..span.1)?;
    if !through_paren.ends_with(')') {
        return None;
    }
    let paren = span.1 - 1;
    // (2) only whitespace precedes that `)` on its line — i.e. the paren is on its OWN line. The
    //     single-line spelling and the paren-on-last-entry-line spelling both fail here, which is
    //     exactly right: the parser binds their trailing comment to the last entry's slot.
    let line_start = through_paren[..paren].rfind('\n').map_or(0, |nl| nl + 1);
    if !through_paren[line_start..paren]
        .chars()
        .all(char::is_whitespace)
    {
        return None;
    }
    // (3) the first non-whitespace character after the span, on that same line, starts a comment.
    let rest_of_line = buffer.get(span.1..)?.lines().next().unwrap_or("");
    let comment = rest_of_line.trim_start().strip_prefix(';')?;
    // (4) that comment carries at least one directive. The parser hands comment text over WITHOUT
    //     its leading `;` everywhere else, so the `;` is stripped above and `comment` is spelled
    //     exactly as `metadata_from_comments` sees it in a bound position — the scan and the
    //     honoring sites therefore agree on what a directive IS.
    let metadata = metadata_from_comments(&[comment]);
    if metadata == RuleMetadata::default() {
        return None;
    }
    let tags = metadata.all_directives();
    Some(multiline_group_trailing_directive_message(name, &tags))
}

/// The merged metadata of every `;` comment line in `text`, a source slice after a rule's body.
fn trailing_comment_metadata(text: &str) -> RuleMetadata {
    text.lines()
        .filter_map(|line| line.trim_start().strip_prefix(';'))
        .map(|comment| metadata_from_comments(&[comment]))
        .fold(RuleMetadata::default(), |acc, found| {
            merge_metadata(&acc, &found)
        })
}

/// The one multi-line group-rule refusal message. `tags` is the non-empty directive list, in the
/// stable order `RuleMetadata::all_directives` produces; the first is reused as the example
/// spelling. Pinned by the `robustness_tests` vectors
/// (`multiline_group_rule_trailing_directive_is_refused_not_dropped` and the
/// `KNOWN_RULE_METADATA_TAGS` sweep beside it), which assert the rule ident, the directive spelling
/// and BOTH remedies as substrings; do not reword it.
fn multiline_group_trailing_directive_message(name: &str, tags: &[&str]) -> String {
    let found = quoted_directive_list(tags, ", ");
    format!(
        "group rule `{name}`: a trailing comment on a multi-line group rule's closing-paren line \
         cannot carry a directive — the pinned CDDL parser binds that comment to the FOLLOWING rule \
         (or drops it when the group rule is last), so {found} would be silently lost. Refused \
         rather than dropped. Two spellings put the directive where the parser binds it to this \
         rule: write the whole group on ONE line (`{name} = (…) ; {example} …`), or keep the \
         closing paren on the LAST ENTRY's line. A prose (non-directive) trailing comment is \
         accepted in this position.",
        example = tags.first().copied().unwrap_or("@rust_name")
    )
}
