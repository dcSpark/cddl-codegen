use super::{EXTERN_MARKER, RAW_BYTES_MARKER, rule_is_scope_marker};

/// Every DEFINING statement of every rule name, grouped by AST-level name (`$a` is its own name,
/// distinct from `a`) and kept in source order both between names and within a name.
///
/// Scope markers and the two tool-inserted marker rules are skipped: they are not user rules, and a
/// scope marker's name is synthesized per input file, so it can never legitimately repeat.
///
/// The two seams that care about repeated names — `merge_incremental_type_choice_extensions` and
/// `repeated_rule_definition_rejections` — share this grouping so they cannot disagree about which
/// statements belong to a name (a merge that skipped a statement the guard counted, or the reverse,
/// would either drop an arm silently or refuse a spelling it had just merged).
fn defining_statements_by_name(cddl: &cddl::ast::CDDL) -> Vec<(String, Vec<usize>)> {
    let mut order: Vec<String> = Vec::new();
    let mut by_name: std::collections::BTreeMap<String, Vec<usize>> =
        std::collections::BTreeMap::new();
    for (idx, cddl_rule) in cddl.rules.iter().enumerate() {
        if rule_is_scope_marker(cddl_rule).is_some() {
            continue;
        }
        let name = cddl_rule.name();
        if matches!(name.as_str(), EXTERN_MARKER | RAW_BYTES_MARKER) {
            continue;
        }
        let entry = by_name.entry(name.clone()).or_default();
        if entry.is_empty() {
            order.push(name);
        }
        entry.push(idx);
    }
    order
        .into_iter()
        .map(|name| {
            let indices = by_name.remove(&name).unwrap();
            (name, indices)
        })
        .collect()
}

/// Whether a name's statement set can be MERGED into one type rule by
/// `merge_incremental_type_choice_extensions`.
///
/// Three conditions, each for its own reason:
/// - every statement is a `Rule::Type` — a group statement contributes group-choice arms, not
///   type-choice arms, and merging them would mint the multi-choice plain-group shape
///   `mark_plain_group` asserts against (see `multi_choice_group_def_rejection`);
/// - no statement carries `generic_params` — the merged body would have to substitute arguments
///   into arms declared under different parameter lists, which the generic machinery models
///   nowhere;
/// - at most ONE statement is a non-alternate (`=`) definition — two `=` statements for one name is
///   a REDEFINITION, not an extension, and combining them would silently invent a choice the spec
///   never wrote. The checked parser refuses that spelling before it can reach us, so this
///   condition is what keeps the refusal correct rather than accidental if the parser ever changes.
fn incremental_type_choice_merge_applies(rules: &[cddl::ast::Rule], indices: &[usize]) -> bool {
    let mut non_alternates = 0usize;
    for &idx in indices {
        match &rules[idx] {
            cddl::ast::Rule::Type { rule, .. } => {
                if rule.generic_params.is_some() {
                    return false;
                }
                if !rule.is_type_choice_alternate {
                    non_alternates += 1;
                }
            }
            cddl::ast::Rule::Group { .. } => return false,
        }
    }
    non_alternates <= 1
}

/// Incremental type-choice extension (`a = int` + `a /= tstr`) resolved at the AST level, BEFORE
/// any parent identity is computed or any rule is walked: the later statements' type-choice arms are
/// appended to the FIRST statement's, in source order, and the later statements are removed from
/// `cddl.rules`. Downstream the result is indistinguishable from the folded spelling
/// (`a = int / tstr`) — one `TypeRule`, one arm list — which is what makes the two byte-identical
/// under every profile and every face instead of needing an incremental path of their own.
///
/// **Arm order is STATEMENT source order**, carrier first. That is what makes the pass
/// order-INSENSITIVE in the only sense that matters: whichever operator a statement carries, its
/// arms land where the statement is written. The extension-FIRST spelling (`a /= tstr` then
/// `a = int`) therefore means `a = tstr / int`, not `a = int / tstr` — the `=` statement is not
/// promoted to the front, because a reader's arm order is the order on the page.
///
/// Runs at the `api::with_types` seam between the span-based multiline-group refusal (which reads
/// the SOURCE BUFFER, so it must see the un-spliced rule set) and `ParentVisitor::new` (which is
/// therefore built over the MERGED AST, so parent identity never sees a duplicate name).
///
/// Names this pass does NOT merge — a group statement involved, generics involved, two `=`
/// statements — are left in place for `repeated_rule_definition_rejections` to refuse. Between the
/// two, no repeated name reaches `parse_rule`.
///
/// Directive semantics come out of the splice unchanged rather than invented: the rule-position slot
/// is the LAST arm's trailing comment, which after the merge is the LAST STATEMENT's last arm; every
/// arm keeps its own `@name`/`@doc` for the variant it becomes; and any other directive on a
/// non-last arm hits the existing non-last-arm rejection in `parse_type_choices`, whose message
/// ("move it to the last arm") is already accurate for the merged rule.
pub fn merge_incremental_type_choice_extensions(cddl: &mut cddl::ast::CDDL) {
    let mut removed = vec![false; cddl.rules.len()];
    for (_, indices) in defining_statements_by_name(cddl) {
        if indices.len() < 2 || !incremental_type_choice_merge_applies(&cddl.rules, &indices) {
            continue;
        }
        // Cloned rather than moved out: `cddl.rules` is a `Vec` we are about to compact, so taking
        // the arms by value would need a placeholder in the hole. `TypeChoice` is `Clone` in the
        // pinned fork and borrows the same source buffer, so the clone is shallow.
        let mut extension_arms: Vec<cddl::ast::TypeChoice> = Vec::new();
        for &idx in &indices[1..] {
            match &cddl.rules[idx] {
                cddl::ast::Rule::Type { rule, .. } => {
                    extension_arms.extend(rule.value.type_choices.iter().cloned());
                }
                // Unreachable: `incremental_type_choice_merge_applies` returned true, which requires
                // every statement to be a type rule.
                cddl::ast::Rule::Group { .. } => unreachable!(
                    "a group statement survived the type-choice merge applicability check"
                ),
            }
            removed[idx] = true;
        }
        match &mut cddl.rules[indices[0]] {
            cddl::ast::Rule::Type { rule, .. } => rule.value.type_choices.extend(extension_arms),
            cddl::ast::Rule::Group { .. } => {
                unreachable!("a group statement survived the type-choice merge applicability check")
            }
        }
    }
    if removed.iter().any(|r| *r) {
        let mut keep = removed.iter().map(|r| !r);
        cddl.rules.retain(|_| keep.next().unwrap());
    }
}

/// Graceful-rejection messages — in source order of each name's FIRST definition — for every rule
/// name that still has more than one defining statement after
/// `merge_incremental_type_choice_extensions` has run.
///
/// The classification keys on the name's whole STATEMENT SET, not on the flag of whichever statement
/// happens to come second. That is what makes the refusals order-INDEPENDENT: keying on the repeat's
/// own flag meant an extension-FIRST spelling (`g //= (2: tstr)` then `g = (1: int)`) had a plain
/// `=` statement as its repeat, carried no alternate flag, and so slipped through at exit 0 while
/// `parse_rule` silently dropped every arm but the last — the exact defect the guard exists to
/// prevent, reachable by reordering two lines.
///
/// One message per NAME (not per extra statement): the refusal is a property of the definition set,
/// so a three-statement chain says it once.
///
/// The four classes, in the order they are tested:
/// - a group-alternate (`//=`) statement anywhere in the set. Honoring `//=` means merging the arms
///   into a plain-group rule carrying 2+ group choices, which is exactly the shape
///   `multi_choice_group_def_rejection` refuses (`mark_plain_group` asserts a single group choice) —
///   so this stays a refusal, in BOTH orders, with the remedy that models the same shape.
/// - a MIXED set (type statements and group statements for one name). There is no single rule the
///   arms could merge into: a type-choice arm and a group-choice arm are different things.
/// - GENERICS involved. A merged body would have to substitute arguments into arms declared under
///   different parameter lists.
/// - anything else — a defensive arm, loud. Reachable only if the checked parser ever admits two
///   plain `=` definitions of one name (today `CDDL::from_slice` refuses that outright), which is a
///   redefinition rather than an extension and must never fall through to `parse_rule`'s last-wins
///   registration.
///
/// Remedies are the supported spellings that model the same shape and are asserted to generate in
/// `incremental_choice_extension_rejects_gracefully`.
pub fn repeated_rule_definition_rejections(cddl: &cddl::ast::CDDL) -> Vec<String> {
    let mut messages = Vec::new();
    for (name, indices) in defining_statements_by_name(cddl) {
        if indices.len() < 2 {
            continue;
        }
        let statements = indices.iter().map(|&idx| &cddl.rules[idx]);
        let mut has_group_alternate = false;
        let mut has_type = false;
        let mut has_group = false;
        let mut has_generics = false;
        for cddl_rule in statements {
            match cddl_rule {
                cddl::ast::Rule::Type { rule, .. } => {
                    has_type = true;
                    has_generics |= rule.generic_params.is_some();
                }
                cddl::ast::Rule::Group { rule, .. } => {
                    has_group = true;
                    has_generics |= rule.generic_params.is_some();
                    has_group_alternate |= rule.is_group_choice_alternate;
                }
            }
        }
        messages.push(if has_group_alternate {
            format!(
                "rule `{name}`: incremental group-choice extension (`//=`) is not supported — \
                 re-defining an already-defined identifier with `//=` silently drops every arm but the \
                 last, generating a group that models only the final extension arm. A plain group rule \
                 cannot itself carry a group choice, so give each arm its own named group and select \
                 between them at the use site, e.g. `{name}_a = (...)`, `{name}_b = (...)`, \
                 `t = [ {name}_a // {name}_b ]`."
            )
        } else if has_type && has_group {
            format!(
                "rule `{name}`: `{name}` is defined both as a TYPE and as a GROUP — incremental \
                 extension combines statements of one kind, and a type-choice arm and a group-choice \
                 arm are not the same thing, so there is no single rule these statements could merge \
                 into. Either fold the type statements into one type-choice rule \
                 (`{name} = <arm1> / <arm2>`), or give each group its own named rule and select \
                 between them at the use site (`{name}_a = (...)`, `{name}_b = (...)`, \
                 `t = [ {name}_a // {name}_b ]`)."
            )
        } else if has_generics {
            format!(
                "rule `{name}`: incremental extension of (or with) a GENERIC rule is not supported — \
                 merging two statements gives a type-choice body, and a generic definition's body \
                 must be a shape that registers a struct to substitute arguments into, which a type \
                 choice is not. Give each arm its own named rule and choose between them at the use \
                 site (`{name}_a<t> = [t]`, then `x = {name}_a<int> / tstr`), or — if the arms need \
                 no parameters at all — fold them into one non-generic type-choice rule \
                 (`{name} = <arm1> / <arm2>`)."
            )
        } else {
            format!(
                "rule `{name}`: `{name}` is defined more than once with `=` — that is a \
                 redefinition, not an incremental extension (`/=`), and combining the definitions \
                 would invent a type choice the spec never wrote while keeping only one of them \
                 would silently discard the rest. Write one definition, folding the alternatives \
                 into a type choice if that is what was meant (`{name} = <arm1> / <arm2>`)."
            )
        });
    }
    messages
}
