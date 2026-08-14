use crate::cli::Cli;
use cddl::ast::parent::ParentVisitor;
use cddl::{ast::*, token};
use std::collections::{BTreeMap, BTreeSet};

use crate::comment_ast::{DuplicatesPolicy, RuleMetadata, merge_metadata, metadata_from_comments};
use crate::intermediate::{
    AliasIdent, AliasInfo, CBOREncodingOperation, CDDLIdent, ConceptualRustType, EnumVariant,
    EnumVariantData, FixedValue, FloatWindow, GenericDef, GenericInstance, IntermediateTypes,
    ModuleScope, PlainGroupInfo, Primitive, Representation, RestKind, RestRow, RestSemantics,
    RustField, RustIdent, RustRecord, RustStruct, RustStructType, RustType, VariantIdent,
    reserved_pin_rejection,
};
use crate::utils::{
    append_number_if_duplicate, convert_to_camel_case, convert_to_snake_case,
    is_identifier_user_defined, is_valid_rust_ident,
};

#[derive(Clone, Debug)]
#[allow(clippy::upper_case_acronyms)]
enum ControlOperator {
    Range((Option<i128>, Option<i128>)),
    /// A NaN-safe float value window (`float64 .le 10.5`, `0.5..10.5`, `float .le 10`). Carries
    /// per-side exclusivity because float space is dense (no ±1 collapse like the integer window).
    RangeFloat(FloatWindow),
    CBOR(RustType),
    Default(FixedValue),
}

pub const SCOPE_MARKER: &str = "_CDDL_CODEGEN_SCOPE_MARKER_";
pub const EXTERN_DEPS_DIR: &str = "_CDDL_CODEGEN_EXTERN_DEPS_DIR_";
pub const EXTERN_MARKER: &str = "_CDDL_CODEGEN_EXTERN_TYPE_";
pub const RAW_BYTES_MARKER: &str = "_CDDL_CODEGEN_RAW_BYTES_TYPE_";

/// Some means it is a scope marker, containing the scope
pub fn rule_is_scope_marker(cddl_rule: &cddl::ast::Rule) -> Option<ModuleScope> {
    match cddl_rule {
        Rule::Type {
            rule:
                TypeRule {
                    name: Identifier { ident, .. },
                    value,
                    ..
                },
            ..
        } => {
            if value.type_choices.len() == 1 && ident.starts_with(SCOPE_MARKER) {
                match &value.type_choices[0].type1.type2 {
                    Type2::TextValue { value, .. } => Some(ModuleScope::new(
                        value.as_ref().split("::").map(String::from).collect(),
                    )),
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn parse_rule(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    cddl_rule: &cddl::ast::Rule,
    cli: &Cli,
) {
    match cddl_rule {
        cddl::ast::Rule::Type { rule, .. } => {
            let rust_ident = RustIdent::new(CDDLIdent::new(rule.name.to_string()));
            if matches!(
                rule.name.to_string().as_str(),
                EXTERN_MARKER | RAW_BYTES_MARKER
            ) {
                // ignore - this was inserted by us so that cddl's parsing succeeds
                // see comments in main.rs
            } else {
                // (1) is_type_choice_alternate is ignored here because no rule reaching this point
                //     needs it. A LONE `/=` statement is the initial definition of its identifier,
                //     valid cddl (the shelley precedent — `b /= tstr` is equivalent to `b = tstr`).
                //     A `/=` statement that EXTENDS an already-defined identifier never arrives as
                //     its own rule at all: `merge_incremental_type_choice_extensions` has already
                //     appended its arms to the first statement's, so what we see is one type rule
                //     holding every arm in statement source order — byte-identical to the folded
                //     spelling. The extension shapes that CANNOT merge (`//=`, mixed type/group,
                //     generics) are rejected upstream in `api::with_types` (via
                //     `repeated_rule_definition_rejections`), so no repeated name reaches here.
                // (2) ignores control operators - only used in shelley spec to limit string length for application metadata

                let generic_params = rule.generic_params.as_ref().map(|gp| {
                    gp.params
                        .iter()
                        .map(|id| RustIdent::new(CDDLIdent::new(id.param.to_string())))
                        .collect::<Vec<_>>()
                });
                if rule.value.type_choices.len() == 1 {
                    let choice = &rule.value.type_choices.first().unwrap();
                    parse_type(
                        types,
                        parent_visitor,
                        &rust_ident,
                        choice,
                        None,
                        generic_params,
                        &RuleMetadata::default(),
                        cli,
                    );
                } else {
                    parse_type_choices(
                        types,
                        parent_visitor,
                        &rust_ident,
                        &rule.value.type_choices,
                        None,
                        generic_params,
                        cli,
                    );
                }
            }
        }
        cddl::ast::Rule::Group { rule, .. } => {
            // RE-EARNING GUARD, not the refusal: `parsing::generic_plain_group_def_rejection`
            // refuses this shape in the `api::with_types` pre-scan, ahead of every reach. Kept so a
            // NEW path that gets past the pre-scan fails loudly here instead of silently proceeding
            // (and because the matrix anchors this message text).
            assert_eq!(
                rule.generic_params, None,
                "{}: Generics not supported on plain groups",
                rule.name
            );
            // Freely defined group - the group body itself is already handled in `api::with_types`
            // (`mark_plain_group`). This arm reaches NEITHER `parse_type` nor `parse_type_choices`,
            // so every rule-position directive a plain group is meant to honor needs its own site
            // here or it is SILENTLY dropped — `@rust_name` was dropped that way once, and
            // `@no_json_schema_export` inherited the same hole (a spliced plain group DOES register a
            // rust struct, so it does get a schema-registration row to suppress). Both are read off
            // the one `group_rule_pin_metadata` extraction, which knows where cddl actually binds a
            // group rule's trailing comment. A directive added to this list must be one with NO
            // field-position meaning — see that fn's doc comment.
            match &rule.entry {
                cddl::ast::GroupEntry::InlineGroup {
                    group,
                    comments_after_group,
                    ..
                } => {
                    let rust_ident = RustIdent::new(CDDLIdent::new(rule.name.to_string()));
                    let pin_metadata =
                        group_rule_pin_metadata(group, comments_after_group.as_ref());
                    handle_rust_name_pin(types, &rust_ident, &pin_metadata);
                    if pin_metadata.no_json_schema_export {
                        types.mark_no_json_schema_export(rust_ident.clone());
                    }
                    // A SPLICED group mints a real struct, so both of these have somewhere to land:
                    // `@custom_json` suppresses its JSON derives and `@used_as_key` demands its
                    // comparison derives. Neither could reach it before — the struct is built from
                    // `PlainGroupInfo`'s metadata, read off the `comments_after_group` slot cddl
                    // leaves empty — so both were accepted and dropped. Marked per-ident here, the
                    // same carrier the type-rule seam uses for the kinds whose config is not their
                    // own.
                    if pin_metadata.custom_json {
                        types.mark_custom_json_rule(rust_ident.clone());
                    }
                    if let Some(demand) = pin_metadata.key_demand {
                        types.mark_key_demand(rust_ident.clone(), demand);
                    }
                    // Everything the author wrote, for the never-spliced refusal in `finalize`: a
                    // group no rule splices materializes neither struct nor field, so every directive
                    // in this slot is inert and the only honest outcome is to say so. Recorded rather
                    // than rejected here because splicedness is a whole-spec property, unknown until
                    // every rule has been walked.
                    types.mark_plain_group_rule_directives(
                        rust_ident,
                        pin_metadata.all_directives(),
                    );
                }
                x => panic!("Group rule with non-inline group? {:?}", x),
            }
        }
    }
}

/// A graceful-rejection message if `cddl_rule` carries a rule-position `@name` directive, else
/// `None`. `@name` renames a struct field, a type-choice variant, or a group-choice arm — never a
/// top-level rule or group, whose CDDL identifier IS the emitted Rust type name directly. On a
/// single-type-choice TYPE rule the directive is silently dropped (`parse_type` assembles the merged
/// rule-position metadata but never consults `.name` to rename the rule), a user-invisible surprise,
/// so we reject at the parse-walk seam (`api::with_types`) through the normal `record_rejection`
/// channel, mirroring `intermediate::reserved_ident_rejection`.
///
/// Multi-choice type rules are deliberately OUT of scope: there every `@name` attaches to a
/// type1/choice and legitimately names an enum variant (`parse_type_choices`), so we return `None`.
/// ONE multi-choice shape is back IN scope — the rule-name position carries a SHAPE axis (the
/// identifier-hazard lesson): a `T / null` two-choice rule collapses to an `Option<T>` alias
/// (`parse_type_choices`' optional-inner path) instead of an enum, so a `@name` on either arm has
/// no variant to name and was silently dropped; both comment placements reject here.
/// The check keys on `.name` ONLY — every other rule-level directive (`@newtype`, `@no_alias`,
/// `@used_as_key`, `@custom_json`, `@doc`, `@custom_serialize`/`@custom_deserialize`) attaches at
/// this same comment position and WORKS, so it must be left untouched.
///
/// For a plain GROUP rule the true rule-position slot is `comments_after_group`; we guard it for
/// symmetry with the reserved-name precedent. Note that a *trailing* `@name` (`grp = (a: uint) ;
/// @name x`) does NOT land there — cddl binds it to the last group entry's trailing comment, so it
/// is consumed by the field-naming site (`group_entry_to_field_name`) as a field rename, which the
/// rejection must not disturb. So in practice this branch only fires for the (currently unreachable)
/// case where `@name` reaches `comments_after_group` directly.
pub fn rule_position_name_rejection(cddl_rule: &cddl::ast::Rule) -> Option<String> {
    let has_rule_position_name = match cddl_rule {
        cddl::ast::Rule::Type { rule, .. } => {
            let choices = &rule.value.type_choices;
            // Single-type-choice rules, plus the T/null two-choice Option-collapse (no enum is
            // generated there, so no variant exists for a `@name` to name). Every other
            // multi-choice rule's `@name`s legitimately name enum variants.
            let in_scope: &[cddl::ast::TypeChoice] = match choices.len() {
                1 => std::slice::from_ref(&choices[0]),
                2 if type2_is_null(&choices[0].type1.type2)
                    || type2_is_null(&choices[1].type1.type2) =>
                {
                    choices.as_slice()
                }
                _ => return None,
            };
            // Mirror `parse_type`'s top-level metadata merge (inherited defaults to empty there):
            // the cddl parser can attach the rule's trailing comment to either the Type1 or the
            // enclosing TypeChoice, so read both.
            in_scope.iter().any(|tc| {
                merge_metadata(
                    &RuleMetadata::from(tc.type1.comments_after_type.as_ref()),
                    &RuleMetadata::from(tc.comments_after_type.as_ref()),
                )
                .name
                .is_some()
            })
        }
        cddl::ast::Rule::Group { rule, .. } => match &rule.entry {
            cddl::ast::GroupEntry::InlineGroup {
                comments_after_group,
                ..
            } => RuleMetadata::from(comments_after_group.as_ref())
                .name
                .is_some(),
            _ => false,
        },
    };
    if has_rule_position_name {
        Some(rule_position_name_message(&cddl_rule.name()))
    } else {
        None
    }
}

/// The one `@name`-at-rule-position message, shared by every seam that recognizes the misplacement
/// so they cannot drift apart. Two seams recognize it from the AST alone (`rule_position_name_rejection`
/// above, in the `api::with_types` pre-scan), and two only later, once a SHAPE that eats the variant
/// is known: the transparent tag-set collapse (`parse_type_choices` — the collapsed rule registers a
/// collection, not an enum, so no arm is a variant) and the never-spliced plain group
/// (`IntermediateTypes::finalize` — splicedness is a whole-spec property). The text is pinned by
/// four `dsl_position_tests` cells (`Expect::Reject("does not rename a top-level")`); do not reword
/// it.
pub fn rule_position_name_message(name: &str) -> String {
    format!(
        "rule `{name}`: `; @name` does not rename a top-level rule or group — the rule \
         identifier `{name}` is itself the emitted Rust type name. `@name` only renames a \
         struct field, a type-choice variant, or a group-choice arm; to change the emitted \
         type name, rename the `{name}` identifier."
    )
}

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

pub fn rule_ident(cddl_rule: &cddl::ast::Rule) -> RustIdent {
    match cddl_rule {
        cddl::ast::Rule::Type { rule, .. } => RustIdent::new(CDDLIdent::new(rule.name.to_string())),
        cddl::ast::Rule::Group { rule, .. } => match &rule.entry {
            cddl::ast::GroupEntry::InlineGroup { .. } => {
                RustIdent::new(CDDLIdent::new(rule.name.to_string()))
            }
            x => panic!("Group rule with non-inline group? {:?}", x),
        },
    }
}

/// Extract the literal tag number cddl-codegen needs. The cddl AST models a tag as a `TagConstraint`
/// (RFC 9682 allows type-valued `#6.<type>(...)` heads); cddl-codegen is tag-parametric and needs a
/// concrete number. Returns None for an absent tag; panics on a non-literal head — unsupported.
fn tag_literal(tag: &Option<token::TagConstraint<'_>>) -> Option<usize> {
    tag.as_ref().map(|t| {
        t.as_literal()
            .expect("non-literal tag heads (#6.<type>(...)) are not supported") as usize
    })
}

/// The transparent tag-set idiom: two type-choice arms whose built `RustType`s are equal but for
/// exactly one extra `Tagged(N)` encoding operation on one arm — the conceptual type (element type
/// included) AND the occurrence bounds match, arm order is irrelevant, and the tag number is taken
/// from the arm (never hardcoded). This is the degenerate type choice whose arms denote the same
/// logical value and differ only in whether the CBOR tag is present, e.g. the Cardano ledger set
/// idiom `set<a> = #6.258([* a]) / [* a]`. Returns `(tag, base)` where `base` is the untagged arm's
/// `RustType` (an `Array`/`Map`), which `parse_type_choices` collapses into one transparent
/// collection carrying an OPTIONALLY-present tag, instead of a two-variant enum whose variants leak
/// the encoding into the type. Near misses — mismatched bounds (`#6.258([+ a]) / [* a]`), different
/// element types, both arms tagged, a non-collection inner, or 3+ arms — return `None` and keep
/// today's enum behavior.
fn recognize_optional_tag_set(variants: &[EnumVariant]) -> Option<(usize, RustType)> {
    if variants.len() != 2 {
        return None;
    }
    let a = variants[0].rust_type();
    let b = variants[1].rust_type();
    for (tagged, untagged) in [(a, b), (b, a)] {
        // the collapse target must be a collection (the idiom is a set/array or a map)
        if !matches!(
            untagged.conceptual_type,
            ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
        ) {
            continue;
        }
        // equality of everything BUT the tag: conceptual type (element type included) and the value
        // config (occurrence bounds, defaults, …) must match exactly.
        if tagged.conceptual_type != untagged.conceptual_type || tagged.config != untagged.config {
            continue;
        }
        if tagged.encodings.len() != untagged.encodings.len() + 1 {
            continue;
        }
        // removing exactly one `Tagged(N)` op from the tagged arm must reconstruct the untagged
        // arm's encoding stack verbatim (order-preserving), and that removed op must be the tag.
        for i in 0..tagged.encodings.len() {
            if let CBOREncodingOperation::Tagged(n) = tagged.encodings[i] {
                let mut reduced = tagged.encodings.clone();
                reduced.remove(i);
                if reduced == untagged.encodings {
                    return Some((n, untagged.clone()));
                }
            }
        }
    }
    None
}

/// `@duplicates` on a rule where the policy can NEVER apply — a non-collection rule (a text/int
/// alias, a struct/group/record, a union, an extern marker, …). Permanent graceful rejection in the
/// house style of the other comment-DSL misuse rejections (`@raw_bytes_flavor`), never a panic and
/// never a silent no-op.
fn reject_duplicates_not_applicable(types: &mut IntermediateTypes, name: &RustIdent) {
    let source_name = types
        .source_rule_name(name)
        .map(str::to_owned)
        .unwrap_or_else(|| name.to_string());
    types.record_rejection(format!(
        "@duplicates on rule `{source_name}`: this directive only applies to set/array collection \
         rules (`[* a]` / `[+ a]`, including the tag-258 set idiom) and table rules \
         (`{{ * k => v }}`); a union's map arm must be a named rule to carry it. Remove it from \
         this rule."
    ));
}

/// Reject a misplaced `@ignore`. The tolerate-and-drop flavor is valid ONLY on a recognized open
/// struct-map rest row (`* k => v` after fixed keys), where it is read from the ENTRY-trailing
/// comment slot — never from a rule's or field's metadata. So any `@ignore` reaching a rule/field
/// metadata consumer is a misplacement, rejected loudly (never silently dropped), naming the one
/// valid placement.
fn reject_ignore_not_applicable(types: &mut IntermediateTypes, name: &RustIdent) {
    let source_name = types
        .source_rule_name(name)
        .map(str::to_owned)
        .unwrap_or_else(|| name.to_string());
    types.record_rejection(format!(
        "@ignore on rule `{source_name}`: this directive is only valid on an open struct-map rest \
         row (`* k => v ; @ignore`) or an open-array rest tail (`* t ; @ignore`), written in the \
         ROW's / TAIL's own trailing comment on its own line inside the container, where it selects \
         the tolerate-and-drop flavor. It does not apply at a rule, alias, union, table, \
         whole-array, or field position. Remove it, or move it onto the `* k => v` row / `* t` tail \
         of an open struct-map / open array."
    ));
}

/// Strip the `Alias` node a TRANSPARENT-ALIAS REGISTRATION cannot store, carrying the stripped
/// rule's wire-codec metadata across the strip. Returns the stripped base and the rule the metadata
/// was inherited from (`None` when nothing was inherited).
///
/// The strip is mandatory at a registration seam and nowhere else: `resolve_alias` re-wraps an
/// entry's `base_type` in `Alias(<this rule>, …)` on the way out, so a stored `Alias` would
/// double-wrap, and `register_type_alias` refuses one for that reason. The member/arm and WRAPPER
/// seams keep their node instead, because the node is the ONLY thing
/// `generate_serialize`/`generate_deserialize`'s `Alias` arms lift an aliased rule's
/// `@custom_serialize`/`@custom_deserialize` pair from: a stripped node silently re-derives the
/// built-in wire, and one CDDL type ends up with two wire forms in one crate depending on which name
/// reached it. Where the node cannot survive, the FACTS travel instead — the registering rule's own
/// metadata answers the emitter's ident lookup exactly as the stripped rule's would have.
///
/// The wire-facts family moves as ONE declaration or not at all. `@custom_encodings` and
/// `@custom_wire_major` are legal only beside the pair, so a rule that writes its own pair also
/// writes (or deliberately omits) its own framing facts and inherits nothing — outer wins, whole.
/// Naming, doc and structural directives never travel: they describe the rule they are written on.
///
/// Chains cascade because each link inherits at its OWN registration and the rule graph registers a
/// rule after everything it references (`dep_graph::topological_rule_order` pushes in DFS post-order,
/// so source order is irrelevant), which is what
/// `dsl_position_tests::transparent_realias_and_member_agree_on_the_aliass_custom_pair` pins with a
/// deliberately adversarial declaration order.
fn strip_alias_for_registration(
    types: &IntermediateTypes,
    mut base_type: RustType,
    rule_metadata: &mut RuleMetadata,
) -> (RustType, Option<AliasIdent>) {
    let ConceptualRustType::Alias(stripped_ident, inner) = base_type.conceptual_type else {
        return (base_type, None);
    };
    base_type.conceptual_type = *inner;
    // Outer wins: a rule with its own pair describes its own wire completely.
    if rule_metadata.custom_serialize.is_some() || rule_metadata.custom_deserialize.is_some() {
        return (base_type, None);
    }
    let Some(source) = types.type_aliases().get(&stripped_ident) else {
        return (base_type, None);
    };
    let Some(source_metadata) = source.rule_metadata.as_ref() else {
        return (base_type, None);
    };
    if source_metadata.custom_serialize.is_none() && source_metadata.custom_deserialize.is_none() {
        return (base_type, None);
    }
    rule_metadata.custom_serialize = source_metadata.custom_serialize.clone();
    rule_metadata.custom_deserialize = source_metadata.custom_deserialize.clone();
    rule_metadata.custom_encodings = source_metadata.custom_encodings.clone();
    rule_metadata.custom_wire_major = source_metadata.custom_wire_major;
    // The ORIGIN, not the previous link: the author's rule is what a no-silent-directive check has
    // to be able to reach in one hop.
    let origin = source
        .wire_metadata_inherited_from
        .clone()
        .unwrap_or(stripped_ident);
    (base_type, Some(origin))
}

/// The `@custom_serialize` / `@custom_deserialize` directive names present in `metadata`, in a
/// stable order. Every placement rejection for the pair reports it one directive at a time, so the
/// message names exactly the spelling the author wrote (and both fire when both are present).
fn custom_codec_directives(metadata: &RuleMetadata) -> Vec<&'static str> {
    let mut found = Vec::new();
    if metadata.custom_serialize.is_some() {
        found.push("@custom_serialize");
    }
    if metadata.custom_deserialize.is_some() {
        found.push("@custom_deserialize");
    }
    found
}

/// The RULE-SCOPED directives, refused at a MEMBER position — the one list of "which directives
/// does a member position refuse, and with what remedy", shared by every member position rather
/// than restated at each. Two positions call it today: the record field walk
/// (`parse_record_from_group_choice`) and the SINGLE-ENTRY group-choice arm
/// (`reject_field_directives_on_single_entry_arm`), which mints no record and so never reaches the
/// field walk at all. Sharing is what makes "the arm validates a member's directives the way the
/// field walk does" a fact rather than a resemblance: a directive added to this list is refused at
/// both, and neither can quietly grow a position-specific verdict for one of them.
///
/// The list has two halves. The one written out below is the half a ROW-ENTRY slot cannot take
/// wholesale (`@ignore` and `@duplicates` are honored there), so the row seams call the other half —
/// [`reject_type_scoped_directives`] — on its own.
///
/// `site` names the slot the way that slot's other rejections do (`field \`f\` of rule \`t\``), and
/// `position_noun` is the phrase the three "…, not X" messages end on — the only text that varies
/// between positions, because the other three already word themselves position-generically
/// (`a field/member position`).
///
/// `rule_slot_shared` marks the ONE member slot the cddl parser also binds a RULE's trailing comment
/// to — a plain group rule's LAST entry — and suppresses only the [`reject_type_scoped_directives`]
/// half; see that function's doc comment for why the split is exactly there.
fn reject_member_scoped_directives(
    types: &mut IntermediateTypes,
    site: &str,
    position_noun: &str,
    metadata: &RuleMetadata,
    rule_slot_shared: bool,
) {
    // `@raw_bytes_flavor` only applies to a `_CDDL_CODEGEN_EXTERN_TYPE_` rule definition, never a
    // field/member position — reject loudly instead of silently ignoring it.
    if metadata.raw_bytes_flavor {
        types.record_rejection(format!(
            "@raw_bytes_flavor on {site}: this tag is only valid on a {EXTERN_MARKER} rule \
             definition, not {position_noun}. Remove it from this entry."
        ));
    }
    // `@used_as_elem` names the TYPE whose loose-list wasm wrapper to mint, so it is rule-scoped and
    // never applies at a field/member position — reject loudly instead of silently dropping it.
    // Honoring it here would need a sub-ruling per member shape (an optional field, an inline
    // `[* x]`, a primitive): the tag is only unambiguous when the field's type is a bare named
    // reference. The remedy is exact and already proven, so refuse and name it.
    if metadata.used_as_elem {
        types.record_rejection(format!(
            "@used_as_elem on {site}: this directive is rule-scoped — it mints the loose-list wasm \
             wrapper for the TYPE it tags — and does not apply to a field/member position. Put it \
             on the rule that defines the element type (`<type> = … ; @used_as_elem`)."
        ));
    }
    // `@copy` only applies to a `_CDDL_CODEGEN_EXTERN_TYPE_` / `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule
    // definition, never a field/member position — reject loudly instead of ignoring it.
    if metadata.copy {
        types.record_rejection(format!(
            "@copy on {site}: this tag is only valid on a {EXTERN_MARKER} or {RAW_BYTES_MARKER} \
             rule definition, not {position_noun}. Remove it from this entry."
        ));
    }
    // `@extern_companions` only applies to a `_CDDL_CODEGEN_EXTERN_TYPE_` rule definition, never a
    // field/member position — reject loudly instead of silently ignoring it. This is also the slot a
    // plain-GROUP rule's TRAILING comment binds to (`grp = (a: uint) ; @extern_companions …`, the
    // `@name plain-group-trailing` seam), so it covers that spelling too.
    if metadata.extern_companions.is_some() {
        types.record_rejection(format!(
            "@extern_companions on {site}: this tag is only valid on a {EXTERN_MARKER} rule \
             definition, not {position_noun}. Remove it from this entry."
        ));
    }
    // `@duplicates` is per-rule and never applies at a field/member position — reject loudly instead
    // of silently ignoring it. The remedy names the collection as its own rule.
    if metadata.duplicates.is_some() {
        types.record_rejection(format!(
            "@duplicates on {site}: this directive is per-rule and does not apply to a field/member \
             position. Name the collection as its own rule and put `; @duplicates \
             <preserve|reject>` on that rule. (An inline `#6.258` array in this position already \
             defaults to `@duplicates reject` via the well-known-tag registry — hoisting it to a \
             named rule with `; @duplicates preserve` is exactly how to opt out.)"
        ));
    }
    // `@ignore` is the open struct-map rest-row tolerate-and-drop flavor and never applies at a
    // field/member position — reject loudly instead of silently ignoring it.
    if metadata.ignore {
        types.record_rejection(format!(
            "@ignore on {site}: this directive is only valid on an open struct-map rest row, \
             written in the ROW's own trailing comment (`* k => v ; @ignore`, on the row's own line \
             inside the braces), not at a field/member position. Remove it from this entry."
        ));
    }
    if !rule_slot_shared {
        reject_type_scoped_directives(types, site, metadata);
    }
}

/// The TYPE-SCOPED directives, refused at a MEMBER position. Split out of
/// [`reject_member_scoped_directives`] rather than folded into its list because the two halves have
/// different reach: these six describe the TYPE a name denotes (how it lowers, what it derives, what
/// the json faces do with it), so no member position reads them and every member position refuses
/// them, while the other half includes directives a row-entry slot legitimately HONORS
/// (`@ignore`, `@duplicates`). A row seam can therefore call this one alone.
///
/// The six were read into a member's `RuleMetadata` and dropped at exit 0 — `@newtype` on an
/// ordinary array-record field, `@used_as_key` on a map-record field, and their four siblings, all
/// byte-identical to the undirected spec under default, `--preserve-encodings` and `--wasm`.
///
/// Deliberately NOT called for the one member slot the cddl parser also binds a RULE's trailing
/// comment to: a plain group rule's LAST entry (`pg = (a: uint, b: uint) ; @used_as_key`). That
/// slot is the group rule's documented directive slot — `@used_as_key` there IS honored at rule
/// level, and `comment_dsl.mdx` documents the dual read — so refusing it here would refuse the rule
/// position through the member seam. The OTHER half still fires there, unchanged: a plain group's
/// trailing `@duplicates` has always been refused by member site (`field \`b\` of rule \`pg\``).
/// Non-last plain-group entries carry no rule reading and refuse like any member.
///
/// `site` names the slot the way that slot's other rejections do. Each remedy names the rule that
/// defines the member's type, because that is where the directive's own reader lives.
fn reject_type_scoped_directives(
    types: &mut IntermediateTypes,
    site: &str,
    metadata: &RuleMetadata,
) {
    // `@rust_name` pins the FINAL derived Rust type name of a rule in an extern-deps scope, so it is
    // keyed on a rule's type and has no member reading at all.
    if metadata.rust_name.is_some() {
        types.record_rejection(format!(
            "@rust_name on {site}: this directive is rule-scoped — it pins the final Rust type name \
             of a rule in a {EXTERN_DEPS_DIR} scope — and does not apply to a field/member \
             position. Put it on the rule that defines the member's type (`<type> = … ; @rust_name \
             <Pinned>`)."
        ));
    }
    // `@newtype` asks a rule that would lower to a transparent alias to mint a wrapper struct
    // instead. A member declares no rule, so there is no lowering here to change.
    if metadata.newtype.is_some() {
        types.record_rejection(format!(
            "@newtype on {site}: this directive is rule-scoped — it makes a rule mint a wrapper \
             struct instead of a transparent alias — and does not apply to a field/member position. \
             Put it on the rule that defines the member's type (`<type> = … ; @newtype`)."
        ));
    }
    // `@no_alias` suppresses a rule's own `pub type` line. A member emits no alias line.
    if metadata.no_alias {
        types.record_rejection(format!(
            "@no_alias on {site}: this directive is rule-scoped — it suppresses the `pub type` line \
             a rule of its own emits — and does not apply to a field/member position. Put it on the \
             rule that defines the member's type (`<type> = … ; @no_alias`)."
        ));
    }
    // `@used_as_key` is the Ord/Hash derive demand on a TYPE; the derives land on that type's
    // definition, never on a position that uses it.
    if metadata.key_demand.is_some() {
        types.record_rejection(format!(
            "@used_as_key on {site}: this directive is rule-scoped — it demands the comparison \
             derives on the TYPE it tags — and does not apply to a field/member position. Put it on \
             the rule that defines the member's type (`<type> = … ; @used_as_key`)."
        ));
    }
    // `@custom_json` suppresses the serde/schemars derives on a TYPE.
    if metadata.custom_json {
        types.record_rejection(format!(
            "@custom_json on {site}: this directive is rule-scoped — it suppresses the \
             serde/schemars derives on the TYPE it tags — and does not apply to a field/member \
             position. Put it on the rule that defines the member's type (`<type> = … ; \
             @custom_json`)."
        ));
    }
    // `@no_json_schema_export` suppresses a TYPE's schema-registration row in the json-gen crate.
    if metadata.no_json_schema_export {
        types.record_rejection(format!(
            "@no_json_schema_export on {site}: this directive is rule-scoped — it suppresses the \
             json-gen schema-registration row of the TYPE it tags — and does not apply to a \
             field/member position. Put it on the rule that defines the member's type (`<type> = … \
             ; @no_json_schema_export`)."
        ));
    }
}

/// Reject a `@custom_encodings` declaration that is not accompanied by BOTH halves of the pair it
/// describes, at the same position. Returns whether anything was rejected.
///
/// The declaration states the codec-visible encoding variables of the wire a
/// `@custom_serialize`/`@custom_deserialize` pair writes and reads, so it is meaningful only where
/// that pair is. With ONE half the other direction is generated code deriving the REPLACED type's
/// encoding demand, which declared slots would contradict by construction (one side would pass
/// what the other never binds — E0061/E0308 in the generated crate, if it were honored at all).
/// With NO half there is no codec to describe and the declaration would be read into the position's
/// metadata and dropped — the silent-drop class this DSL rejects everywhere.
///
/// `position` names the slot the way that slot's other rejections do.
fn reject_custom_encodings_without_pair(
    types: &mut IntermediateTypes,
    position: &str,
    metadata: &RuleMetadata,
) -> bool {
    let declarations: [(&str, bool, &str); 2] = [
        (
            "@custom_encodings",
            metadata.custom_encodings.is_some(),
            "@custom_serialize <fn> @custom_deserialize <fn> @custom_encodings <kinds>",
        ),
        (
            "@custom_wire_major",
            metadata.custom_wire_major.is_some(),
            "@custom_serialize <fn> @custom_deserialize <fn> @custom_wire_major <major>",
        ),
    ];
    if !declarations.iter().any(|(_, written, _)| *written) {
        return false;
    }
    let present = custom_codec_directives(metadata);
    if present.len() == 2 {
        return false;
    }
    let found = if present.is_empty() {
        "no `@custom_serialize`/`@custom_deserialize` is written there".to_owned()
    } else {
        format!("only `{}` is written there", present[0])
    };
    let mut rejected = false;
    for (directive, written, remedy) in declarations {
        if !written {
            continue;
        }
        types.record_rejection(format!(
            "{directive} on {position}: the declaration describes the wire of the custom \
             (de)serializer pair written BESIDE it, and {found}. Both halves are required: with one \
             half the other direction is generated code deriving the replaced type\u{2019}s own \
             inferred facts, which the declaration would contradict. Write the pair here \
             (`{remedy}`), or drop the declaration."
        ));
        rejected = true;
    }
    rejected
}

/// Reject a custom (de)serializer pair sitting in a collection ROW-ENTRY comment slot — a table row,
/// an open struct-map rest row, an open-array rest tail. The pair is a TYPE-level override keyed on
/// the type whose codec it replaces, and a row entry declares no type of its own, so the directives
/// are read into the row's `RuleMetadata` and dropped. (`@name`, `@duplicates` and `@ignore` are the
/// spellings that slot legitimately carries — they are row-scoped by construction, which is exactly
/// what the pair is not.) `position` names the row the way that row's other rejections do (the named
/// rows spell themselves `<shape> of rule `<src>``, an anonymous inline table has no rule to name) and
/// `remedy` names the rule to move the pair onto. Returns whether anything was rejected.
fn reject_custom_codec_on_row_entry(
    types: &mut IntermediateTypes,
    position: &str,
    remedy: &str,
    metadata: &RuleMetadata,
) -> bool {
    let found = custom_codec_directives(metadata);
    for directive in &found {
        types.record_rejection(format!(
            "{directive} on the {position}: the custom (de)serializer pair is a \
             TYPE-level override keyed on the type whose codec it replaces, and a row entry \
             declares no type of its own, so it is not honored in this slot. {remedy}"
        ));
    }
    !found.is_empty()
}

/// Read the ROW-ENTRY comment slot of an ANONYMOUS inline table (`{ * k => v ; @directive }` used as
/// a member, element, `.cbor` payload or type-choice-arm type) and apply what it declares to the map
/// type just built for it.
///
/// The anonymous twin of a NAMED table's row slot (`register_rust_struct`'s `HomogenousMap` arm):
/// the same entry, read the same way. What the two slots DO with `@duplicates` differs by design and
/// leaves exactly one honored spelling per shape — an anonymous table has no rule slot, so its row
/// is where the policy has to live; a named table has one, so its row REJECTS the directive and
/// points there. `@duplicates preserve` here swaps the member to the `PairMap`/`NonEmptyPairMap`
/// vec-of-pairs twin, the same representation a named table's rule slot selects. An explicit
/// `reject` is that policy's accepted default (a loose table is key-unique by construction), but
/// is still stored exactly as a named table stores it. The WIT boundary decides whether a reject
/// policy carries an invariant from both policy and resolved container shape, so a table remains a
/// plain map rather than being mistaken for an `OrderedSet` despecialization.
///
/// Every other directive the slot can carry declares something a row entry has no place for, so it
/// is rejected with the spelling that works: nothing written here is accepted-and-inert.
///
/// SCOPE — inline shapes exactly. A NAMED table referenced by name never reaches this seam
/// (`Type2::Typename` handles it), so a per-site policy can never fork a shared type's identity,
/// which is why the general member-position `@duplicates` on named references stays unsupported.
fn apply_inline_table_row_metadata(
    types: &mut IntermediateTypes,
    group_choice: &GroupChoice,
    map_type: RustType,
) -> RustType {
    let entries = flatten_group_entries(&group_choice.group_entries, Representation::Map);
    // A parenthesized row (`{ * (k => v) }`) has no entry slot of its own, and
    // `group_entry_rule_metadata` panics on an `InlineGroup` — the guard the named seam takes.
    // Probed: the cddl AST binds a comment written there to NOTHING this seam can reach (the
    // `InlineGroup` variant carries no trailing-comment field and the entry's `OptionalComma` comes
    // back `trailing_comments: None`), so there is no directive here to honor OR to reject — the
    // spelling that carries one is the unparenthesized row.
    let [(row_ge, row_comma)] = entries[..] else {
        return map_type;
    };
    if matches!(row_ge, GroupEntry::InlineGroup { .. }) {
        return map_type;
    }
    let metadata = group_entry_rule_metadata(row_ge, row_comma);
    let position = "row entry of an inline table (`{ * k => v }`)";
    reject_custom_codec_on_row_entry(
        types,
        position,
        "Name the table's key or value type as its own rule and put the pair there (`k = text ; \
         @custom_serialize <fn> @custom_deserialize <fn>`, then `{ * k => v }`).",
        &metadata,
    );
    // …and a `@custom_encodings` declaration with no pair to describe is dropped the same way.
    reject_custom_encodings_without_pair(types, &format!("the {position}"), &metadata);
    reject_inert_inline_table_row_directives(types, position, &metadata);
    match metadata.duplicates {
        Some(DuplicatesPolicy::Preserve) => {
            map_type.with_duplicates_policy(Some(DuplicatesPolicy::Preserve))
        }
        Some(DuplicatesPolicy::Reject) => {
            map_type.with_duplicates_policy(Some(DuplicatesPolicy::Reject))
        }
        _ => map_type,
    }
}

/// Reject every directive an inline table's row-entry slot can carry that the slot does not honor.
///
/// The slot became live when `@duplicates` started being read there, and a live slot must not also
/// be a silent-drop slot — so this enumerates `RuleMetadata` EXHAUSTIVELY (the destructure is the
/// enforcement: a new directive field fails to compile here until it is classified as honored or
/// rejected). The custom-codec pair and its `@custom_encodings`/`@custom_wire_major` declarations are
/// owned by the two helpers that run before this one, so they are excluded here rather than reported
/// twice.
fn reject_inert_inline_table_row_directives(
    types: &mut IntermediateTypes,
    position: &str,
    metadata: &RuleMetadata,
) -> bool {
    let RuleMetadata {
        name,
        rust_name,
        newtype,
        no_alias,
        key_demand,
        used_as_elem,
        copy,
        raw_bytes_flavor,
        ignore,
        // honored here — the whole reason the slot is live
        duplicates: _,
        custom_json,
        no_json_schema_export,
        // owned by `reject_custom_codec_on_row_entry` / `reject_custom_encodings_without_pair`
        custom_serialize: _,
        custom_deserialize: _,
        custom_encodings: _,
        custom_wire_major: _,
        extern_companions,
        comment,
    } = metadata;
    // `@name` is the one with a real alternative spelling worth naming: on a type-choice arm the
    // variant name lives in the slot AFTER the closing brace, which is a different comment entirely.
    let name_remedy = "To name a type-choice variant put `; @name <n>` AFTER the closing brace \
                       (`{ * k => v } ; @name <n> / int`); to rename a field put it on the field \
                       entry. To carry any other directive, name the table as its own rule \
                       (`t = { * k => v } ; @<directive>`) and reference `t`.";
    let rule_remedy = "Name the table as its own rule (`t = { * k => v } ; @<directive>`) and \
                       reference `t`, or remove it.";
    let found: [(&str, bool, &str); 12] = [
        ("@name", name.is_some(), name_remedy),
        ("@rust_name", rust_name.is_some(), rule_remedy),
        ("@newtype", newtype.is_some(), rule_remedy),
        ("@no_alias", *no_alias, rule_remedy),
        ("@used_as_key", key_demand.is_some(), rule_remedy),
        ("@used_as_elem", *used_as_elem, rule_remedy),
        ("@copy", *copy, rule_remedy),
        ("@raw_bytes_flavor", *raw_bytes_flavor, rule_remedy),
        ("@ignore", *ignore, rule_remedy),
        ("@custom_json", *custom_json, rule_remedy),
        (
            "@no_json_schema_export",
            *no_json_schema_export,
            rule_remedy,
        ),
        (
            "@extern_companions",
            extern_companions.is_some(),
            rule_remedy,
        ),
    ];
    let mut rejected = false;
    for (directive, written, remedy) in found {
        if !written {
            continue;
        }
        types.record_rejection(format!(
            "{directive} on the {position}: the row entry declares no rule, field or type of its \
             own, so only `@duplicates` (which selects the row's container) is honored there. \
             {remedy}"
        ));
        rejected = true;
    }
    // `@doc` last: it is the one spelling an author reaches for reflexively, and its own message
    // says where the documentation would have to live to be emitted.
    if comment.is_some() {
        types.record_rejection(format!(
            "@doc on the {position}: an anonymous inline table emits no type of its own to \
             document (it renders as the container type at each use site). Put the `@doc` on the \
             field or element that holds the table, or name the table as its own rule \
             (`t = {{ * k => v }} ; @doc <text>`) and reference `t`."
        ));
        rejected = true;
    }
    rejected
}

/// The CDDL source name a rule ident was registered under, falling back to the ident itself for a
/// struct synthesized during IR build (which has no source rule).
fn source_rule_name_of(types: &IntermediateTypes, name: &RustIdent) -> String {
    types
        .source_rule_name(name)
        .map(str::to_owned)
        .unwrap_or_else(|| name.to_string())
}

/// What already claims the Rust struct ident a multi-arm group-choice arm wants, if anything.
enum ArmIdentClaimant {
    /// The arm's ident is the ident of the very rule the arm belongs to.
    OwnRule,
    /// The ident belongs to another top-level rule (source name).
    Rule(String),
    /// The ident is already taken by an emitted group-choice arm in another rule (its source name).
    Arm(String),
}

/// Whether `arm_ident` — the struct name a multi-arm group-choice arm of rule `enum_name` wants —
/// is already spoken for.
///
/// Both claimant kinds are order-independent by construction: every top-level rule is scope-marked
/// before the parse loop runs (so `is_toplevel_rule` is complete from the first rule onward), and an
/// arm claim is registered only by a non-embeddable arm, which is exactly an arm whose struct gets
/// emitted under that name.
fn arm_ident_collision(
    types: &IntermediateTypes,
    enum_name: &RustIdent,
    arm_ident: &RustIdent,
) -> Option<ArmIdentClaimant> {
    if arm_ident == enum_name {
        return Some(ArmIdentClaimant::OwnRule);
    }
    if types.is_toplevel_rule(arm_ident) {
        return Some(ArmIdentClaimant::Rule(source_rule_name_of(
            types, arm_ident,
        )));
    }
    types
        .group_choice_arm_claimant(arm_ident)
        .map(|owner| ArmIdentClaimant::Arm(owner.to_owned()))
}

/// Two types demanding one generated name. Rejected gracefully rather than resolved by renaming
/// either side: the arm's struct is emitted under its own name and is public API of the generated
/// crate, so any automatic disambiguation would silently rename a shipped type — and a positional
/// one (`Shared` vs `Shared2`) would additionally re-derive that name from rule ORDER, so an
/// unrelated reference edge added elsewhere in the spec could swap which claimant keeps which name.
/// The author picks, via `@name`.
fn reject_group_choice_arm_ident_collision(
    types: &mut IntermediateTypes,
    enum_name: &RustIdent,
    arm_source_name: &str,
    arm_ident: &RustIdent,
    claimant: &ArmIdentClaimant,
) {
    let owner = source_rule_name_of(types, enum_name);
    let conflict = match claimant {
        ArmIdentClaimant::OwnRule => format!(
            "rule `{owner}`: its own group-choice arm `{arm_source_name}` generates a struct named \
             `{arm_ident}`, the same name as the rule itself"
        ),
        ArmIdentClaimant::Rule(rule) => format!(
            "rule `{owner}`: the group-choice arm `{arm_source_name}` generates a struct named \
             `{arm_ident}`, which is already the type generated by rule `{rule}`"
        ),
        // Both arms in the SAME rule — two arms of one group choice spelled `@name` alike.
        ArmIdentClaimant::Arm(other) if *other == owner => format!(
            "rule `{owner}`: two of its group-choice arms (including `{arm_source_name}`) each \
             generate a struct named `{arm_ident}`"
        ),
        // Phrased symmetrically (names sorted) so the message does not depend on which of the two
        // arms the rule order happened to reach first.
        ArmIdentClaimant::Arm(other) => {
            let (a, b) = if owner <= *other {
                (&owner, other)
            } else {
                (other, &owner)
            };
            format!(
                "rules `{a}` and `{b}`: a group-choice arm in each generates a struct named \
                 `{arm_ident}`"
            )
        }
    };
    types.record_rejection(format!(
        "{conflict}. Two types cannot share one name. Rename the arm with `; @name <new_name>` \
         (this renames the generated variant too, e.g. `{enum_name}::<NewName>`)."
    ));
}

/// Settle ONE group-choice arm's variant name against the names its enum has already committed to.
///
/// This is the variant-namespace counterpart of `arm_ident_collision`, and the two genuinely cannot
/// be one check: an EMBEDDABLE arm is inlined and registers no struct, so it claims nothing in the
/// struct namespace while still declaring a variant, and two arms that share a single struct by
/// structural equality still declare two variants. Both shapes emitted an enum with a repeated
/// variant (Rust `E0428`) until this ran.
///
/// The policy splits on whether the author WROTE the name. An explicit `; @name` is public API of
/// the generated crate, so it is never renamed and a second arm spelling it is rejected — the author
/// picks, exactly as for the struct namespace. A DERIVED name (an arm's member key, its type, or its
/// `{rule}{index}` position) carries no authorial intent, so it yields and takes a numeric suffix,
/// which is what the type-choice path already does for its own derived names. Callers reserve every
/// arm's explicit name before the loop, so the authored name wins from either source position.
fn settle_arm_variant_name(
    types: &mut IntermediateTypes,
    enum_name: &RustIdent,
    used: &mut BTreeSet<String>,
    explicit_seen: &mut BTreeMap<String, String>,
    base: String,
    arm_source_name: &str,
    explicit: bool,
) -> String {
    if explicit {
        // Already reserved by the caller's pre-pass, so `used` says nothing here — a second
        // claimant is detected by the source name recorded against the generated one. Two
        // DIFFERENT spellings can camel-case to one variant (`my_arm` / `myArm`), so the message
        // names both arms as the author wrote them.
        if let Some(first) = explicit_seen.get(&base) {
            reject_group_choice_arm_variant_name_collision(
                types,
                enum_name,
                first,
                arm_source_name,
                &base,
            );
        } else {
            explicit_seen.insert(base.clone(), arm_source_name.to_owned());
        }
        return base;
    }
    if used.insert(base.clone()) {
        return base;
    }
    // Search for the first free suffix rather than appending a single counter: the pool holds the
    // arms' EXPLICIT names too, so a one-shot `Name2` could land straight onto an arm the author
    // spelled `; @name name2` — turning one duplicate into another.
    let mut n = 2u32;
    loop {
        let candidate = format!("{base}{n}");
        if used.insert(candidate.clone()) {
            return candidate;
        }
        n += 1;
    }
}

/// Two arms of ONE group choice, each explicitly `@name`d onto the same generated variant. Rejected
/// gracefully rather than renamed, for the same reason as its struct-namespace sibling
/// `reject_group_choice_arm_ident_collision`: a variant name is public API of the generated crate,
/// so any automatic disambiguation silently ships a name the author never wrote — and a positional
/// one would derive that public name from arm ORDER. The author picks, via `@name`.
fn reject_group_choice_arm_variant_name_collision(
    types: &mut IntermediateTypes,
    enum_name: &RustIdent,
    first_arm_source_name: &str,
    second_arm_source_name: &str,
    variant_name: &str,
) {
    let owner = source_rule_name_of(types, enum_name);
    types.record_rejection(format!(
        "rule `{owner}`: its group-choice arms `{first_arm_source_name}` and \
         `{second_arm_source_name}` both generate the variant `{enum_name}::{variant_name}`. Two \
         variants cannot share one name. Rename one of them with `; @name <new_name>`."
    ));
}

/// Two explicitly named arms of ONE type choice land on the same emitted Rust variant.
///
/// This deliberately remains a per-kind sibling of
/// `reject_group_choice_arm_variant_name_collision`. The type-choice builder also serves anonymous
/// nested choices, so the no-owner message names only what that context can honestly identify: the
/// two arms and their generated variant. Derived names carry no authorial API promise and continue
/// to take numeric suffixes.
fn reject_type_choice_arm_variant_name_collision(
    types: &mut IntermediateTypes,
    enum_name: Option<&RustIdent>,
    first_arm_ordinal: usize,
    first_arm_source_name: &str,
    second_arm_ordinal: usize,
    second_arm_source_name: &str,
    variant_name: &str,
) {
    let message = match enum_name {
        Some(enum_name) => {
            let owner = source_rule_name_of(types, enum_name);
            format!(
                "rule `{owner}`: its type-choice arm {first_arm_ordinal} (`@name {first_arm_source_name}`) \
                 and arm {second_arm_ordinal} (`@name {second_arm_source_name}`) both generate the variant \
                 `{enum_name}::{variant_name}`. Two variants cannot share one name. Give the two arms \
                 distinct `; @name` values."
            )
        }
        None => format!(
            "an inline type choice: its type-choice arm {first_arm_ordinal} (`@name {first_arm_source_name}`) \
             and arm {second_arm_ordinal} (`@name {second_arm_source_name}`) both generate the variant \
             `{variant_name}`. Two variants cannot share one name. Give the two arms distinct `; @name` \
             values."
        ),
    };
    types.record_rejection(message);
}

/// Settle a generator-derived type-choice variant against the complete emitted namespace.
///
/// Explicit names are pre-reserved before the arm walk, so this must search for the first free
/// numeric suffix rather than assume `Base2` remains available: an author may explicitly own it.
/// The same globally-used set also preserves ordinary derived-only suffixing.
fn settle_derived_type_choice_variant_name(used: &mut BTreeSet<String>, base: String) -> String {
    if used.insert(base.clone()) {
        return base;
    }
    let mut n = 2u32;
    loop {
        let candidate = format!("{base}{n}");
        if used.insert(candidate.clone()) {
            return candidate;
        }
        n += 1;
    }
}

/// Whether a MINTED variant name can be emitted verbatim as a Rust variant identifier.
///
/// Two conditions, both read off the STRING: it must be lexically an identifier
/// (`is_valid_rust_ident`), and it must not be a Rust KEYWORD — which is what Rust's own grammar
/// means by `IDENTIFIER` (`IDENTIFIER_OR_KEYWORD` minus the keywords). The keyword half is not a
/// special case for `Self`: the emitter writes minted names verbatim and never raw-escapes them
/// (`r#..`), so EVERY keyword lexeme is unspellable in the position, and checking the list keeps
/// this guard a predicate on the string exactly like its lexical half. Only `Self` is reachable
/// from today's minters — `convert_to_camel_case` upper-cases the first character and `Self` is
/// the one capitalized keyword — but a minter that ever emitted a lower-cased name is covered
/// without a second repair.
fn is_spellable_variant_name(name: &str) -> bool {
    is_valid_rust_ident(name) && !RUST_KEYWORDS.contains(&name)
}

/// An arm whose variant name was DERIVED from the arm's own type and came out unspellable as Rust.
///
/// The derivation that produces these is `FixedValue::for_variant`, which mints from the value's
/// LEXEME: `1.5` → `F1.5` and `-1` → `U-1` are not identifier-shaped at all, and a fixed TEXT arm
/// camel-cases straight through, so `"self"` → `Self` is identifier-SHAPED but a keyword. Both are
/// caught by `is_spellable_variant_name`, a predicate on the minted STRING rather than on the
/// value's kind, so any future lexeme-derived name is covered by the same seam instead of shipping
/// invalid Rust to rustfmt — which is what these used to do, dying with an error that named
/// rustfmt's confusion rather than the arm the author wrote.
///
/// Rejected rather than auto-sanitized (`F1_5`): a variant name is public API of the generated
/// crate, so the sanitization scheme would be a naming decision the author never made. `@name` is
/// the supported route, exactly as for the two collision rejections above.
///
/// Shared by both arm-naming consumers — `create_variants_from_type_choices` (type choices, bare
/// and nested-anonymous) and the group-choice arm loop's bare-member fallback — so the two cannot
/// drift in what they refuse or in how they spell the remedy.
///
/// `naming_slot` is the one thing the two consumers do NOT share: a type-choice arm's naming slot
/// is its own trailing comment, a group-choice arm's is the one that follows the `//` opening it —
/// and a group-choice arm has a SECOND, adjacent slot (the entry's own line) that names nothing,
/// which is exactly where an author reading a bare `; @name <new_name>` remedy lands. So the slot
/// is named, not implied.
fn unnameable_arm_variant_name_rejection(
    owner_desc: &str,
    arm_source: &str,
    minted_name: &str,
    naming_slot: &str,
) -> String {
    format!(
        "{owner_desc}: its arm `{arm_source}` generates the variant name `{minted_name}`, which is \
         not a valid Rust identifier. Name the arm with `; @name <new_name>` to choose the variant \
         name yourself — {naming_slot}."
    )
}

fn reject_unnameable_arm_variant_name(
    types: &mut IntermediateTypes,
    owner_desc: &str,
    arm_source: &str,
    minted_name: &str,
    naming_slot: &str,
) {
    types.record_rejection(unnameable_arm_variant_name_rejection(
        owner_desc,
        arm_source,
        minted_name,
        naming_slot,
    ));
}

/// An occurrence marker on the single entry of a single-entry group-choice arm is REFUSED — unless
/// DROPPING it is sound, which is exactly the question `inline_group_occurrence_flattens` already
/// answers, so this asks it there rather than restating the boundary.
///
/// A one-entry arm never registers a record: its entry's TYPE goes straight into the enum variant,
/// and a variant holds exactly one value. There is nowhere for a repetition count to live, so the
/// marker was read by nothing at all — `[ x: uint // ? kv ]`, `// * kv`, `// + kv` and `// 2*3 kv`
/// each generated output BYTE-IDENTICAL to the unmarked `// kv`, at exit 0. Where that byte
/// identity is WRONG, it is wrong on the wire: the emitted decoder rejects the counts the spec
/// admits, so the empty encoding a `?` / `*` / `0*n` arm allows comes back as `No variant matched …
/// Definite length mismatch: found 0`, and (in an ARRAY) every 2-or-more encoding a `*` / `+` /
/// `n*m` arm allows fails the same way.
///
/// Where it is RIGHT, it is the shared predicate's map-side carve-out: under unique map keys a
/// second repetition of a fixed-key alternative would duplicate its keys, so every lower-bound-≥1
/// marker (`+`, `2*3`, `2*`) admits count 1 and nothing else — dropping it is the honored
/// semantics, not narrowing, and `{ x: uint // + kv }` keeps generating the mandatory arm's bytes.
/// Refusing those would remove correct surface AND would have to claim a 2-or-more encoding that
/// does not exist in a map, which is why the message below is rep-scoped rather than uniform.
///
/// Honoring the markers that DO reach the refusal is not a guard's work — a zero-case variant has
/// to be TELLABLE on the wire, which means the sibling arms' own length checks must exclude the
/// empty form, and that is the occurrence/bounds program's scope (the queue's "unify non-final
/// optional/repeated array decoding"). So this is a refusal, and it is an honest one because the
/// remedy it names is verified to generate in both representations: a TYPE choice over one named
/// rule per count (`xarr = [x: uint]`, `kvarr = [kv]`, `empty = []`, `t = xarr / kvarr / empty` for
/// `?`; `kvs = [* kv]` in place of `kvarr` for `*`). The named-array WRAPPER (`w = [kv]` referenced
/// from the arm) is deliberately NOT the remedy named here: it nests the group in an array of its
/// own and so cannot express the empty case at all.
///
/// Read off the ENTRY's own occurrence, so it covers every entry shape the arm can take — a plain
/// group, an alias to one, a tagged one, and a plain keyed or bare member (the defect is not
/// group-specific: `[ x: uint // ? a: tstr ]` was byte-identical to its unmarked twin too). Three
/// deliberate non-firings, the first two being the shared predicate's own:
/// - `1*1` (and an absent marker) already mean exactly once, in EITHER representation, so dropping
///   them narrows nothing. Same pedantic-exactly-once carve-out the array record-field loop's
///   `narrows` guard makes.
/// - a lower-bound-≥1 marker in a MAP arm, per the collapse above.
/// - an `InlineGroup` entry, which the entry-position refusal in `group_entry_to_type` already
///   rejects on its own terms for EVERY marker including none — one message per problem.
fn reject_occurrence_on_single_entry_arm(
    types: &mut IntermediateTypes,
    name: &RustIdent,
    group_entry: &GroupEntry,
    rep: Representation,
) -> bool {
    let occur = match group_entry {
        GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref(),
        GroupEntry::TypeGroupname { ge, .. } => ge.occur.as_ref(),
        GroupEntry::InlineGroup { .. } => None,
    };
    // THE one boundary, shared with the inline-group splice: an arm that can be spelled without
    // its marker keeps generating, everything else refuses. Never duplicate the match here — two
    // spellings of "is dropping this sound?" is how the seams come to disagree.
    if occur.is_none() || inline_group_occurrence_flattens(occur, rep) {
        return false;
    }
    let site = rejection_site(types, Some(name), "anonymous group choice");
    let source_name = source_rule_name_of(types, name);
    // Rep-scoped in three places, all for the same reason — a map's keys are unique, so a repeated
    // fixed-key alternative has no 2-or-more encoding at all: the markers that can REACH this
    // message differ, the wire consequence to claim differs, and the remedy differs. The remedy
    // also differs in more than its brackets: an array alternative can reference the plain group
    // directly (`kvarr = [kv]`, verified) and spell a repeating count as a homogeneous array
    // (`kvs = [* kv]`, verified), while a MAP-rep record refuses a keyless plain-group member
    // outright, so the map alternative spells the members out.
    let (markers, consequence, remedy, still_supported) = match rep {
        Representation::Array => (
            "carries an occurrence marker (`?` / `*` / `+` / `n*m`)",
            "Every count the marker admits and that variant cannot hold — the EMPTY encoding under \
             `?` / `*` / `0*n`, every 2-or-more encoding under `*` / `+` / `n*m` — is then \
             rejected by a decoder the spec says must accept it."
                .to_owned(),
            format!(
                "Give each count its own alternative as a named rule and select between them with \
                 a TYPE choice (`/`), which is where a per-alternative count CAN be spelled: \
                 `one = [ … ]` holding the alternative's contents, `none = []` for the empty case, \
                 `many = [* … ]` for a repeating one, then `{source_name} = one / none / many` — \
                 one rule per arm of the original `//` choice."
            ),
            "(An arm with NO marker is supported as it stands, as is the pedantic `1*1`, which \
             already means exactly once.)",
        ),
        Representation::Map => (
            "carries a zero-permitting occurrence marker (`?` / `*` / `0*n` / `*n`)",
            "The EMPTY encoding the marker admits is then rejected by a decoder the spec says must \
             accept it — and that is the whole of it here, because a map's keys are unique, so a \
             repeated fixed-key alternative has no 2-or-more encoding in the first place."
                .to_owned(),
            format!(
                "Give each count its own alternative as a named rule and select between them with \
                 a TYPE choice (`/`), which is where a per-alternative count CAN be spelled: \
                 `one = {{ … }}` spelling the alternative's own members and `none = {{}}` for the \
                 empty case, then `{source_name} = one / none` — one rule per arm of the original \
                 `//` choice."
            ),
            "(An arm with NO marker is supported as it stands, as are the pedantic `1*1` and every \
             lower-bound-≥1 marker — `+`, `2*3`, `2*` — which admit exactly one repetition under \
             unique map keys and so generate the mandatory arm.)",
        ),
    };
    types.record_rejection(format!(
        "{site}: the group-choice arm `{group_entry}` {markers}, which is unsupported — a \
         single-entry arm becomes ONE enum variant holding exactly one value, so the marker is \
         dropped and the emitted codec writes, and accepts, exactly one repetition of it. \
         {consequence} {remedy} {still_supported}"
    ));
    true
}

/// The field/member DIRECTIVE validation for the SINGLE-ENTRY group-choice arm — the member
/// position that mints no record, and therefore never reaches the record field walk that honors or
/// refuses a member's directives. Without this the whole family was read into the entry's trailing
/// metadata and dropped at exit 0: `[ a: uint // f: bytes ; @custom_serialize ws_only ]` generated
/// the arm's codec in both directions with `ws_only` called nowhere, and the COMPLETE pair,
/// `@raw_bytes_flavor`, `@doc` and the rest behaved identically, in both representations. A silent
/// drop is the one outcome this DSL refuses everywhere else, so every directive the field walk
/// READS is either honored here or refused here.
///
/// Three groups, from the field walk's own reads (a code enumeration of what
/// `parse_record_from_group_choice` does with a member's `RuleMetadata`, not a keyword sweep):
/// - The RULE-SCOPED ones go through `reject_member_scoped_directives`, the list the field walk
///   itself calls — same directives, same remedies, one shared source. That list carries both the
///   directives whose only valid home is a marker/collection RULE (`@raw_bytes_flavor`, `@copy`,
///   `@used_as_elem`, `@extern_companions`, `@duplicates`, `@ignore`) and the TYPE-SCOPED six
///   (`@rust_name`, `@newtype`, `@no_alias`, `@used_as_key`, `@custom_json`,
///   `@no_json_schema_export`).
/// - The custom-codec family (`@custom_serialize` / `@custom_deserialize` and the
///   `@custom_encodings` / `@custom_wire_major` wire facts that describe their wire) is HONORED at
///   an ordinary field and cannot be here: the arm registers no `RustField` for the pair to ride,
///   and the variant's codec is generated by the enum. The remedy is asserted to route — a complete
///   pair on the member's own TYPE rule emits `ws_only(serializer, f)` / `rd_only(raw)` inside this
///   very arm's serialize/deserialize — which is what makes the refusal honest rather than a
///   deferral. ANY presence refuses, one message: at a field a lone half is its own defect (two wire
///   forms at one position) while the complete pair is fine, but here neither reaches emission, so
///   splitting the verdict would report a completeness problem the position does not have.
/// - `@doc` is honored at a field as the field's doc comment; here the arm's own slot
///   (`// ; @doc <text>`, from `comments_before_grpchoice`) already documents the variant and IS
///   read, so the entry slot is refused with that slot as the remedy rather than given a second,
///   colliding meaning.
///
/// `@name` splits, because this slot HAS a reader for exactly one member shape: the
/// anonymous-inline-array minting path takes the member's struct name from it, so
/// `[ a: uint // f: [x: uint] ; @name Inner ]` mints `pub struct Inner`. That naming door is what
/// the "Anonymous groups not allowed" error advertises and `comment_dsl.mdx` documents, so it is
/// kept; everywhere else the name was read by nothing (`// f: bytes ; @name renamed` still emitted
/// variant `F`), and that is refused with the arm's OWN naming slot as the remedy. Naming the
/// VARIANT from this slot instead was the rejected alternative: it renames variants in specs that
/// generate today (`F(Inner)` → `Inner(Inner)`) and mints a second naming slot beside the arm's
/// documented `// ; @name <n>`.
///
/// Which member shapes the reader covers is NOT restated here. It is OBSERVED, through the reader's
/// only effect — the member's parsed type IS the struct the name mints — which is why this seam runs
/// AFTER the entry's type parse and takes `member_type`. A second spelling of the reader's scope
/// ("sole type choice, operator-free, heterogeneous inline array") is precisely the drift the
/// observation avoids: change the reader and this verdict changes with it, in the same commit and by
/// construction. The one spelling the observation reads as consumed without the reader running is a
/// member whose type is a rule ALREADY named what the directive asks for (`f: inner ; @name inner`),
/// where the name describes the type the member already has and nothing is lost.
fn reject_field_directives_on_single_entry_arm(
    types: &mut IntermediateTypes,
    name: &RustIdent,
    group_entry: &GroupEntry,
    optional_comma: &OptionalComma,
    member_type: &RustType,
) {
    // An `InlineGroup` entry panics `group_entry_rule_metadata` and is already refused on its own
    // terms by the entry-position rejection in `group_entry_to_type` — one message per problem, the
    // same carve-out the occurrence guard above makes.
    if matches!(group_entry, GroupEntry::InlineGroup { .. }) {
        return;
    }
    let metadata = group_entry_rule_metadata(group_entry, optional_comma);
    // Name the arm by its MEMBER key where it has one, and by the entry as written where it does
    // not (`// kv`), with the directive comment the entry renders back trimmed off — the author is
    // looking at their CDDL, and the comment is the thing being talked about, not part of the site.
    // A real member KEY (`f: bytes`), as opposed to the typename a keyless `// kv` arm is named
    // after — the two share `group_entry_to_raw_field_name`, but only the first can be re-spelled
    // with a `: inner` in the remedy below.
    let member_key = match group_entry {
        GroupEntry::ValueMemberKey { ge, .. } if ge.member_key.is_some() => {
            group_entry_to_raw_field_name(group_entry)
        }
        _ => None,
    };
    let arm_desc = group_entry_to_raw_field_name(group_entry).unwrap_or_else(|| {
        group_entry
            .to_string()
            .split(';')
            .next()
            .unwrap_or_default()
            .trim()
            .to_owned()
    });
    let site = format!(
        "the single-entry group-choice arm `{arm_desc}` of rule `{}`",
        source_rule_name_of(types, name)
    );
    let codec_directives = {
        let mut found = custom_codec_directives(&metadata);
        if metadata.custom_encodings.is_some() {
            found.push("@custom_encodings");
        }
        if metadata.custom_wire_major.is_some() {
            found.push("@custom_wire_major");
        }
        found
    };
    if !codec_directives.is_empty() {
        let written = codec_directives
            .iter()
            .map(|d| format!("`{d}`"))
            .collect::<Vec<_>>()
            .join(" / ");
        // The remedy is spelled for the arm as WRITTEN: a keyed member keeps its key, a keyless one
        // (`// kv`) references the new rule directly. Both spellings were asserted to route the
        // pair into this very arm's serialize/deserialize before this text shipped.
        let remedy_arm = match &member_key {
            Some(key) => format!("{key}: inner"),
            None => "inner".to_owned(),
        };
        types.record_rejection(format!(
            "{written} on {site}: the custom (de)serializer pair cannot be honored on a \
             single-entry arm — the arm registers no record, so the entry's type goes straight into \
             the enum variant and the variant's codec is generated by the enum, leaving the \
             directive nothing to replace. Name the member's type as its OWN rule and put the \
             COMPLETE pair there (`inner = <type> ; @custom_serialize <fn> @custom_deserialize \
             <fn>`, then `// {remedy_arm}`), which does route both directions at this arm."
        ));
    }
    if metadata.comment.is_some() {
        types.record_rejection(format!(
            "@doc on {site}: a single-entry arm registers no record, so there is no field for the \
             entry's doc comment to land on. Write it in the ARM's own slot instead, which \
             documents the enum variant the arm becomes: put `; @doc <text>` on the line that OPENS \
             the arm (`// ; @doc <text>`), before the entry."
        ));
    }
    // The `@name` fork: honored where the anon-array reader consumed it, refused where it did not.
    // The condition is the reader's own effect, read off the parsed member type rather than
    // re-derived from the AST — see this function's doc comment.
    if let Some(written) = metadata.name.as_ref() {
        let consumed = matches!(
            &member_type.conceptual_type,
            ConceptualRustType::Rust(ident) if ident.to_string() == convert_to_camel_case(written)
        );
        if !consumed {
            types.record_rejection(format!(
                "@name `{written}` on {site}: this slot names a member-position anonymous inline \
                 array (`// f: [x: uint] ; @name Inner` mints `pub struct Inner` and holds it in \
                 the variant), and this member's type is not one — so the name is read by nothing \
                 here. The arm's naming slot is the one that follows the `//` opening it: write \
                 `// ; @name {written}` on that line to name the enum variant the arm becomes."
            ));
        }
    }
    // Never the dual-read slot: a group-choice arm belongs to a TYPE rule (a multi-choice plain
    // group body is refused before this walk runs), so no rule's trailing comment lands here.
    reject_member_scoped_directives(
        types,
        &site,
        "a group-choice arm's member",
        &metadata,
        false,
    );
}

/// The well-known-tag semantics registry: THE single place mapping a CBOR tag number to the
/// duplicate-handling policy its IANA-registered semantics imply, applied wherever the tag directly
/// wraps a homogeneous occurrence collection (the shape the array/table construction sites already
/// guard — a record-shaped `#6.258([uint, text])` becomes a Record and a primitive `#6.258(text)` a
/// Wrapper, so neither reaches those sites). This is the extension point for future well-known-tag
/// entries (e.g. the bignum tags 2/3): add an arm here rather than scattering tag-number checks
/// through the parser.
///
/// `is_array` distinguishes a set-shaped inner (`#6.258([* a])`) from a map-shaped inner
/// (`#6.258({* k => v})`). Tag 258 is the IANA set tag, so it implies `Reject` (uniqueness) ONLY on
/// an array inner — a map is not a set and gets nothing. The default returned here applies only when
/// the author wrote no explicit `@duplicates` directive; an explicit directive always wins (explicit
/// `reject` is an accepted self-documenting no-op, explicit `preserve` is the per-rule opt-out back
/// to today's plain `Vec`/`NonEmptyVec` behavior verbatim on the wire).
fn well_known_tag_default_duplicates(tag: usize, is_array: bool) -> Option<DuplicatesPolicy> {
    match (tag, is_array) {
        (258, true) => Some(DuplicatesPolicy::Reject),
        _ => None,
    }
}

/// Return `rule_metadata` with the well-known-tag registry default injected into its `duplicates`
/// field when (a) the author wrote no explicit directive and (b) the registry has a default for this
/// `(tag, is_array)`. When the default applies and `notice` is `Some`, print it (a one-line
/// generation-time notice; no notice when the directive is explicit, either value). The returned
/// metadata is what the RustStruct constructor reads, so `config().duplicates` reflects the
/// EFFECTIVE policy for every downstream consumer (embed sites, generic use-site re-resolution, the
/// wasm collision detectors, the extern-interface projection).
fn with_well_known_tag_default(
    rule_metadata: &RuleMetadata,
    tag: usize,
    is_array: bool,
    notice: Option<&str>,
) -> RuleMetadata {
    let mut effective = rule_metadata.clone();
    if effective.duplicates.is_none()
        && let Some(default) = well_known_tag_default_duplicates(tag, is_array)
    {
        effective.duplicates = Some(default);
        if let Some(notice) = notice {
            // A diagnostic, not progress: it announces a decode-behaviour change the spec did not ask
            // for (loose historical bytes with duplicate elements now fail), so stderr and the default
            // level.
            crate::warn!("{notice}");
        }
    }
    effective
}

/// Effective metadata for a single-arm tagged ARRAY rule (`foo = #6.258([* a])`, mandatory tag):
/// inject the registry's set-semantics default (258 → reject) when no explicit `@duplicates`, and
/// print the single-arm defaulting notice if it applies. The tag stays a mandatory `Option<Sz>`
/// (grammar decides the encoding record); only the inner element type gains uniqueness.
fn single_arm_array_effective_metadata(
    rule_metadata: &RuleMetadata,
    tag: Option<usize>,
    name: &RustIdent,
) -> RuleMetadata {
    match tag {
        Some(t) => with_well_known_tag_default(
            rule_metadata,
            t,
            true,
            Some(&format!(
                "Rule `{name}` (single-arm tag {t} set) defaulting to @duplicates reject (IANA set semantics) — write `; @duplicates preserve` on the rule to opt out"
            )),
        ),
        None => rule_metadata.clone(),
    }
}

/// The RULE-POSITION metadata of a multi-arm type rule: the LAST arm's trailing comment, merged
/// across the `TypeChoice` and `Type1` levels because the cddl parser may bind a rule's trailing
/// comment to either. (In the pinned fork it lands on the `TypeChoice` slot in every spelling
/// dumped from the AST; the `Type1` half is kept for robustness against a parser change and is what
/// makes this identical to the merge every other rule-position site performs.)
///
/// Both branches of `parse_type_choices` read through here — the `T / null` Option collapse and the
/// enum-registering remainder — so the two cannot drift over which slot IS the rule position. They
/// drifted once: the collapse branch read the INNER arm's `Type1` slot, which is never populated,
/// and every rule-position directive on a `T / null` rule was silently dropped as a result.
fn rule_position_metadata(type_choices: &[TypeChoice]) -> RuleMetadata {
    merge_metadata(
        &RuleMetadata::from(
            type_choices
                .last()
                .and_then(|tc| tc.comments_after_type.as_ref()),
        ),
        &RuleMetadata::from(
            type_choices
                .last()
                .and_then(|tc| tc.type1.comments_after_type.as_ref()),
        ),
    )
}

/// A range / `.size` control operator whose HEAD is a named type with no rust primitive behind it.
/// The range machinery lowers a constraint onto the primitive that backs the constrained type, so a
/// head `ident_to_primitive` does not map has nothing to lower onto — it used to abort at a bare
/// `.unwrap()`. Recorded gracefully instead, for ANY such ident, so the next reserved-but-unmapped
/// name class (the narrower float prelude names were the first) cannot re-earn the panic.
fn unmapped_control_head_rejection(type_name: &RustIdent, cddl_ident: &CDDLIdent) -> String {
    format!(
        "rule `{type_name}`: a range or `.size` control operator on `{cddl_ident}` is unsupported — \
         the constraint is lowered onto the rust primitive backing the constrained type, and \
         `{cddl_ident}` has no such primitive. Apply the constraint to a concrete numeric, text or \
         byte-string type (`uint`, `int`, `float64`, `tstr`, `bstr`), or remove it."
    )
}

/// A `.default` whose value cannot be lowered onto the head it was written on.
///
/// The default substitutes for an ABSENT value at deserialization, so it is written into the rust
/// primitive backing the constrained type: a head with no such primitive (a named type like `tdate`,
/// or the inert placeholder a refused prelude name already left behind) has nowhere to put it, and a
/// primitive of the wrong CBOR class would encode a value the head cannot hold. Recorded at the
/// APPLICATION — both the rule-position and member-position routes — so the refusal a name seam
/// already made survives to be reported instead of being destroyed by an abort one step later.
///
/// `head` is the head as WRITTEN, so the message points at the CDDL the user has in front of them.
///
/// The remedy list names the heads a default ACTUALLY lands on, which is why bare `int` is not among
/// them: `int` is bignum-capable and resolves to the hand-written `Int` struct, not to a rust
/// primitive, so it is one of the heads this very check refuses. A signed default belongs on `nint`
/// or on a signed integer RANGE, which does collapse onto a primitive (`si = -128..127`, then
/// `? n: si .default -2` → `i8`).
fn unmappable_default_head_rejection(
    rule_name: Option<&RustIdent>,
    head: &Type2,
    default_value: &FixedValue,
) -> String {
    format!(
        "{}`.default {}` cannot be applied to `{head}` — a default substitutes for an absent value \
         and is written into the rust primitive backing the constrained type, which `{head}` either \
         has none of or cannot hold a value of this kind. Apply the default to a concrete type of \
         the value's own kind (`uint`, `nint`, `float64`, `tstr`, `bool`), or — for a signed value — \
         to an integer RANGE, which does collapse onto a signed primitive (`si = -128..127`, then \
         `? n: si .default -2`). Bare `int` is not such a head: it is bignum-capable and has no rust \
         primitive behind it.",
        reject_rule_prefix(rule_name),
        fixed_value_as_written(default_value)
    )
}

/// A `.default` value rendered the way it was WRITTEN in the CDDL, for the message above (the `Debug`
/// spelling would print the IR's `Uint(1)` at a user who wrote `1`).
fn fixed_value_as_written(value: &FixedValue) -> String {
    match value {
        FixedValue::Null => "null".to_owned(),
        FixedValue::Undefined => "undefined".to_owned(),
        FixedValue::Bool(b) => b.to_string(),
        FixedValue::Nint(i) => i.to_string(),
        FixedValue::Uint(u) => u.to_string(),
        FixedValue::Float(f) => f.to_string(),
        FixedValue::Text(t) => format!("\"{t}\""),
        FixedValue::Bytes(bytes) => format!(
            "h'{}'",
            bytes
                .iter()
                .map(|byte| format!("{byte:02X}"))
                .collect::<String>()
        ),
    }
}

/// Render a type in a diagnostic without asking the upstream AST to display a byte literal.
///
/// Its byte-string display implementation tries to interpret arbitrary bytes as UTF-8, so
/// formatting `h'CAFE'` can itself fail before the parser reaches the real support/rejection
/// decision. Literal types already have an owned, byte-safe IR spelling; all other types retain
/// the AST's source display.
fn type_source_desc_for_diagnostic(ty: &Type) -> String {
    if ty.type_choices.len() == 1
        && let Some(fixed) = type2_to_fixed_value(&ty.type_choices[0].type1.type2)
    {
        return fixed.cddl_source_desc();
    }
    ty.to_string()
}

/// The fixed-occurrence diagnostic only needs an entry's source spelling after the entry has
/// already been classified as fixed. Keep that formatting lazy: an unrelated supported type must
/// never be exposed to an upstream AST display implementation merely because it occupies a
/// one-entry array.
fn group_entry_source_desc_for_diagnostic(entry: &GroupEntry) -> String {
    match entry {
        GroupEntry::ValueMemberKey { ge, .. } => type_source_desc_for_diagnostic(&ge.entry_type),
        GroupEntry::TypeGroupname { ge, .. } => ge.name.to_string(),
        GroupEntry::InlineGroup { .. } => unreachable!("inline groups do not lower to Fixed"),
    }
}

/// Register the nominal owner required when a fixed value must itself be a Rust value.  Member
/// fixed values remain unstored and use the existing inline path; this seam is only for a named
/// rule and the synthesized inner of the `T / null` collapse.
#[allow(clippy::too_many_arguments)]
fn register_fixed_singleton(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    owner: RustIdent,
    fixed_type: RustType,
    tag: Option<usize>,
    rule_metadata: Option<&RuleMetadata>,
    generic_params: Option<&[RustIdent]>,
    cli: &Cli,
    synthesized: bool,
) -> RustType {
    let fixed = match fixed_type.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Fixed(fixed) => fixed.clone(),
        other => panic!("fixed singleton owner `{owner}` must resolve to Fixed, got {other:?}"),
    };

    if generic_params.is_some() {
        types.record_rejection(format!(
            "generic rule `{owner}`: a fixed-value body has no occurrence of its generic \
             parameter to substitute, and a singleton TypeChoice is a concrete nominal type rather \
             than a generic definition. Remove `<…>` from this constant rule, or put the parameter \
             in a supported structural body. {SUPPORTED_GENERIC_DEF_BODIES}"
        ));
        return RustType::new(ConceptualRustType::Rust(owner));
    }

    if !synthesized && rule_metadata.is_some_and(|metadata| metadata.newtype.is_some()) {
        types.record_rejection(format!(
            "@newtype on `{owner}` is redundant and unsupported: a fixed-value rule is already a nominal singleton TypeChoice with its own codec. Remove @newtype."
        ));
    }

    // `api::with_types` predeclares every authored rule's scope before parsing begins.  Consult it
    // rather than parse order so a later `fixed_bool_true = uint` cannot silently overwrite the
    // earlier synthesized owner (or vice versa).
    if synthesized && types.is_toplevel_rule(&owner) {
        let claimant = types
            .source_rule_name(&owner)
            .map(str::to_owned)
            .unwrap_or_else(|| owner.to_string());
        types.record_rejection(format!(
            "fixed singleton `{owner}` for {} collides with the authored rule `{}`. Rename the rule; synthesized fixed/null owners reserve this deterministic name.",
            fixed.cddl_source_desc(),
            claimant,
        ));
        return RustType::new(ConceptualRustType::Rust(owner));
    }

    if let Some(existing) = types.rust_struct(&owner) {
        let same_singleton = matches!(existing.variant(), RustStructType::TypeChoice { variants }
            if variants.len() == 1
                && matches!(variants[0].rust_type().conceptual_type.resolve_alias_shallow(),
                    ConceptualRustType::Fixed(existing_fixed)
                        if existing_fixed.singleton_name_fragment() == fixed.singleton_name_fragment()
                            && variants[0].rust_type().fixed_singleton_name_fragment()
                                == fixed_type.fixed_singleton_name_fragment()));
        if !same_singleton {
            types.record_rejection(format!(
                "fixed singleton `{owner}` for {} collides with an existing generated type. Rename the authored rule that caused the collision.",
                fixed.cddl_source_desc()
            ));
        }
        return RustType::new(ConceptualRustType::Rust(owner));
    }

    types.register_rust_struct(
        parent_visitor,
        RustStruct::new_fixed_singleton(owner.clone(), tag, rule_metadata, fixed_type),
        cli,
    );
    RustType::new(ConceptualRustType::Rust(owner))
}

fn synthesized_fixed_singleton_ident(fixed_type: &RustType) -> RustIdent {
    RustIdent::new(CDDLIdent::new(format!(
        "fixed_{}",
        fixed_type.fixed_singleton_name_fragment()
    )))
}

/// A range bound (`a..b` / `a...b`) that is not a numeric LITERAL.
///
/// A range lowers a `(min, max)` pair of VALUES onto the rust primitive backing the constrained
/// type, so a bound that is a named type or an expression has no value to lower — the bound is read
/// before any name is resolved, which is why this is a shape refusal and never a name one. Both
/// bounds route here, so `x = foo..10` and `x = 0..foo` refuse identically instead of one panicking
/// and the other `unimplemented!`ing.
fn non_literal_range_bound_rejection(
    rule_name: Option<&RustIdent>,
    which: &str,
    bound: &Type2,
) -> String {
    format!(
        "{}the range {which} bound `{bound}` is not a numeric literal — a range lowers a (min, max) \
         pair of values onto the rust primitive backing the constrained type, so a named type or \
         expression as a bound has no value to lower. Write numeric literal bounds (`0..255`), or \
         remove the range.",
        reject_rule_prefix(rule_name)
    )
}

/// `.cbor` written on a head that is not `bytes`.
///
/// RFC 8610 §3.8.4 restricts `.cbor` to byte strings — the payload IS the bytes' content — so
/// refusing the shape is right; refusing it by aborting is not, and the abort is name-independent
/// (`uint .cbor uint` aborts exactly as a refused prelude name does).
fn non_bytes_cbor_head_rejection(rule_name: Option<&RustIdent>, head: &str) -> String {
    format!(
        "{}`.cbor` is only allowed on a byte string (RFC 8610 §3.8.4) — its payload is the content \
         of those bytes — and `{head}` is not one. Write the head as `bytes` (`bytes .cbor \
         <payload>`), or remove the control operator.",
        reject_rule_prefix(rule_name)
    )
}

/// The generic-definition body shapes the generator CAN monomorphize, named in every rejection that
/// refuses one it cannot, so each message carries the remedy and not only the diagnosis. Generic
/// support works by substituting the instance's arguments into a registered `RustStruct`, so a body
/// that registers an alias (or nothing) has nowhere for a parameter to live.
const SUPPORTED_GENERIC_DEF_BODIES: &str = "A generic definition's body must be a shape that \
    registers a struct to substitute into: an array (`foo<T> = [* T]`), a map or record \
    (`foo<T> = {a: T}`), or the transparent tag-set idiom (`foo<T> = #6.258([* T]) / [* T]`).";

/// A generic definition whose body is a PLAIN GROUP — `set<a> = (* a)`, and the bare-paren
/// group-choice spelling `g<T> = ((a: T) // (b: uint))`, which the `cddl` AST also gives us as a
/// `Rule::Group`. A plain group registers no struct of its own (its contents are SPLICED into each
/// rule that references it), so an instance's arguments have nowhere to substitute.
///
/// Refused from the `api::with_types` pre-scan rather than where it is reached, because every site
/// that reaches it is an `assert_eq!` abort with no rejection channel: `dep_graph::find_references`
/// (rule ordering, which runs before the IR exists) and this file's own `Rule::Group` arm. Both
/// stay in place as re-earning guards — the pre-scan is what makes them unreachable, and an assert
/// that fires again means a new path got past it.
///
/// One caller reaches `find_references` EARLIER than the pre-scan and so consults this predicate
/// directly to skip the rule: `extern_narrow::scan_consumer`, which runs on every generation
/// (imports or not) during input assembly, before the checked parse the pre-scan walks.
pub(crate) fn generic_plain_group_def_rejection(cddl_rule: &cddl::ast::Rule) -> Option<String> {
    match cddl_rule {
        cddl::ast::Rule::Group { rule, .. } if rule.generic_params.is_some() => Some(format!(
            "generic rule `{name}`: a plain-group body (`{name}<…> = (…)`) registers no struct of \
             its own — a plain group's contents are spliced into each rule that references it — so \
             `{name}<…>` instances have nowhere to substitute their arguments into. \
             {SUPPORTED_GENERIC_DEF_BODIES}",
            name = rule.name
        )),
        _ => None,
    }
}

/// A group RULE whose body carries two or more group choices — `pg = (a: uint // f: bytes)`, which
/// RFC 8610 admits (`grpchoice *(S "//" S grpchoice)`), so this is a refusal on VALID CDDL and its
/// message has to carry a path rather than only a diagnosis.
///
/// A plain group is SPLICED into each rule that references it, so its body has to name ONE sequence
/// of members. Honoring alternatives is a real feature and not a detail this guard could settle: a
/// reference in group-choice context would concatenate the alternatives, while every other
/// placement would have to mint a CHOICE OF BODIES whose arms stay tellable apart on the wire
/// exactly as a named group-choice rule's are — a naming/registration design. Until that exists the
/// refusal is the contract, and it is made honest by two remedies verified to generate and build on
/// the default, `--preserve-encodings` and `--wasm` profiles: write the alternatives as the
/// referencing container's own group choices, or split the body into single-choice group rules
/// referenced as separate arms.
///
/// Refused from the `api::with_types` pre-scan, beside `generic_plain_group_def_rejection` and for
/// the same reason: the shape's only reach is an `assert_eq!` with no rejection channel (the
/// plain-group marking loop's `group_choices.len() == 1`), which stays as a re-earning guard the
/// pre-scan makes unreachable. Rule position is also where the DEFECT's trigger is — the assert
/// fired on the definition alone, with no reference to the rule anywhere — so refusing per RULE is
/// what makes the message land once however many references exist.
///
/// A GENERIC multi-choice body defers to `generic_plain_group_def_rejection`: that refusal already
/// disposes of the whole rule (a plain group registers no struct for an argument to substitute
/// into, whatever its choice count), and one problem gets one message.
pub(crate) fn multi_choice_group_def_rejection(cddl_rule: &cddl::ast::Rule) -> Option<String> {
    let cddl::ast::Rule::Group { rule, .. } = cddl_rule else {
        return None;
    };
    if rule.generic_params.is_some() {
        return None;
    }
    let cddl::ast::GroupEntry::InlineGroup { group, .. } = &rule.entry else {
        return None;
    };
    let count = group.group_choices.len();
    (count > 1).then(|| {
        format!(
            "group rule `{name}`: its body carries {count} group choices (`{name} = ( … // … )`), \
             which is unsupported. A plain group is SPLICED into each rule that references it, so \
             its body has to name ONE sequence of members — alternatives would have to mint a \
             choice of bodies at every reference, with arms a decoder can tell apart, which is a \
             named-choice design rather than a splice. Write the alternatives where the choice is \
             actually made: as the referencing container's own group choices (`h = [ x: uint // a: \
             uint // f: bytes ]`), or give each alternative its own single-choice group rule and \
             reference those as separate arms (`pga = (a: uint)`, `pgf = (f: bytes)`, `h = [ x: \
             uint // pga // pgf ]`).",
            name = rule.name
        )
    })
}

/// `@raw_bytes_flavor` on a rule that is not an extern marker. Shared verbatim by every
/// type-rule seam that can reach the misplacement so the pinned wording cannot drift between them.
fn raw_bytes_flavor_not_extern_rejection(type_name: &RustIdent) -> String {
    format!(
        "@raw_bytes_flavor on `{type_name}`: this tag is only valid on a {EXTERN_MARKER} \
         rule — it selects the `<ExternName>RawBytes` wrapper flavor for generic instances \
         whose argument is a {RAW_BYTES_MARKER} type. Remove it from this rule."
    )
}

/// `@raw_bytes_flavor` on an extern marker rule that declares no generic parameters. The companion
/// half of `raw_bytes_flavor_not_extern_rejection`: that one gates the rule KIND (extern), this one
/// gates the extern arm itself on generic-ness. The tag names a per-INSTANCE flavor
/// (`uses_raw_bytes_flavor` is keyed by the generic instance being lowered), so a base that declares
/// no parameters has no instances to flavor and the mark is inert — there is no coherent honoring to
/// fall back on, hence a rejection rather than a warning. Wording is a load-bearing test key
/// (`dsl_position_tests` cell `@raw_bytes_flavor` @ `non-generic-extern-rule` pins
/// "declares no generic parameters"); the anchor deliberately does NOT reuse the `only valid on`
/// phrasing above, so the two seams stay distinguishable in a test's expectation.
fn raw_bytes_flavor_non_generic_extern_rejection(type_name: &RustIdent) -> String {
    format!(
        "@raw_bytes_flavor on `{type_name}`: this tag selects the `<ExternName>RawBytes` wrapper \
         flavor for GENERIC instances of an extern whose argument is a {RAW_BYTES_MARKER} type, \
         but `{type_name}` declares no generic parameters — there are no instances to flavor, so \
         the tag would be silently inert. Declare the rule's generic parameters \
         (`{type_name}<T> = {EXTERN_MARKER} ; @raw_bytes_flavor`) or remove the tag."
    )
}

/// `@copy` on a rule that is neither an extern nor a raw-bytes marker. Shared for the same reason
/// as `raw_bytes_flavor_not_extern_rejection`.
fn copy_not_extern_rejection(type_name: &RustIdent) -> String {
    format!(
        "@copy on `{type_name}`: this tag is only valid on a {EXTERN_MARKER} or \
         {RAW_BYTES_MARKER} rule — it declares that the externally-defined rust type derives \
         `Copy` so the generator stops cloning it at boundaries. Remove it from this rule."
    )
}

fn parse_type_choices(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    type_choices: &[TypeChoice],
    tag: Option<usize>,
    generic_params: Option<Vec<RustIdent>>,
    cli: &Cli,
) {
    let optional_inner_type = if type_choices.len() == 2 {
        let a = &type_choices[0].type1;
        let b = &type_choices[1].type1;
        if type2_is_null(&a.type2) {
            Some(b)
        } else if type2_is_null(&b.type2) {
            Some(a)
        } else {
            None
        }
    } else {
        None
    };
    if let Some(inner_type2) = optional_inner_type {
        if generic_params.is_some() {
            // Generic support relies on having a RustStruct to swap the argument types into, and a
            // `T / null` rule collapses to a transparent `Option<T>` ALIAS instead — so an instance
            // has nothing to substitute into. Refused at parse time rather than aborted.
            types.record_rejection(format!(
                "generic rule `{name}`: a `T / null` body collapses to a transparent `Option<T>` \
                 alias, which registers no struct for `{name}<…>` instances to substitute their \
                 arguments into. Spell the `/ null` at each use site instead (`x = [f: uint / \
                 null]`), or give the rule a body that registers a struct. \
                 {SUPPORTED_GENERIC_DEF_BODIES}"
            ));
            return;
        }
        let raw_inner_rust_type = rust_type_from_type1(types, parent_visitor, inner_type2, cli);
        // A fixed inner needs a nominal value before the ordinary Option lowering can carry it.
        // The degenerate null/null shape has no presence bit at all, so normalize it directly to
        // the singleton-null owner rather than exposing Option<FixedNull> (two Rust states for one
        // CBOR value).
        let (inner_rust_type, null_singleton) = if let ConceptualRustType::Fixed(fixed) =
            raw_inner_rust_type.conceptual_type.resolve_alias_shallow()
        {
            if matches!(fixed, FixedValue::Null) && raw_inner_rust_type.encodings.is_empty() {
                // Do not register yet: the rest of this branch is the one rule-position directive
                // reader for `T / null`.  Returning here used to make directives on `null / null`
                // silently inert.  It has a TypeChoice owner rather than an Option alias, but it
                // still needs the common validation before that owner is registered.
                (raw_inner_rust_type.clone(), Some(raw_inner_rust_type))
            } else {
                let singleton_ident = synthesized_fixed_singleton_ident(&raw_inner_rust_type);
                let inner_rust_type = register_fixed_singleton(
                    types,
                    parent_visitor,
                    singleton_ident,
                    raw_inner_rust_type,
                    None,
                    None,
                    None,
                    cli,
                    true,
                );
                // Replace the unstorable fixed value with its nominal singleton before the
                // ordinary Option lowering below.
                (inner_rust_type, None)
            }
        } else {
            (raw_inner_rust_type, None)
        };
        // The collapse's PLAIN-GROUP inner (`u = kv / null`), sibling of the fixed guard above and
        // refused for the reason `reject_plain_group_type_choice_arm` carries. This site is the one
        // that mattered most: the other six spellings of a plain-group arm panicked, while this one
        // exited 0 emitting `pub type U = Option<Kv>;` over a `Kv` the crate never defines — a
        // non-compiling output the tool reported as success. Registering NOTHING is correct for the
        // same reason the fixed guard gives: this rule registers no rust struct either way, and
        // finalize's registered-nothing check cannot outrun the rejection recorded here.
        let collapse_site = rejection_site(types, Some(name), "anonymous");
        if reject_plain_group_type_choice_arm(types, &inner_rust_type, &collapse_site) {
            return;
        }
        let final_type = match tag {
            Some(tag) => {
                RustType::new(ConceptualRustType::Optional(Box::new(inner_rust_type))).tag(tag)
            }
            None => RustType::new(ConceptualRustType::Optional(Box::new(inner_rust_type))),
        };
        // The RULE-POSITION metadata slots, read exactly as the non-collapse branch below reads
        // them: the LAST arm's trailing comment, merged across the `TypeChoice` and `Type1` levels
        // because the cddl parser may bind a rule's trailing comment to either. Reading the INNER
        // arm's `Type1` slot instead — as this branch used to — dropped EVERY rule-position
        // directive on a `T / null` rule silently: the pinned fork never populates
        // `Type1::comments_after_type` for a type-choice arm in any spelling (dumped from the AST
        // for all four placements), so that slot is dead and the `@duplicates` / `@ignore`
        // rejections written right below could not fire, nor could `@no_json_schema_export` mark.
        let rule_metadata = rule_position_metadata(type_choices);
        if let Some(doc) = &rule_metadata.comment {
            types.mark_rule_doc(name.clone(), doc.clone());
        }
        if rule_metadata.custom_json {
            types.mark_custom_json_rule(name.clone());
        }
        // A `T / null` rule collapses to an `Option<T>` alias — a non-collection, so `@duplicates`
        // can never apply here (and `@ignore` never applies at a rule position).
        if rule_metadata.duplicates.is_some() {
            reject_duplicates_not_applicable(types, name);
        }
        if rule_metadata.ignore {
            reject_ignore_not_applicable(types, name);
        }
        // Recorded (not rejected) here: the `T / null` collapse registers a transparent
        // `Option<T>` ALIAS and no rust struct, so finalize's registered-nothing check is what turns
        // this placement into a loud rejection — one site for every struct-less rule shape.
        if rule_metadata.no_json_schema_export {
            types.mark_no_json_schema_export(name.clone());
        }
        // Sibling parity: the same three "valid only on an extern / raw-bytes marker rule"
        // rejections the multi-arm branch records, for the same reason — a `T / null` rule can never
        // be either marker, so each of these is a misplacement, and the silent flavor re-mints the
        // classes the directives exist to suppress.
        if rule_metadata.raw_bytes_flavor {
            types.record_rejection(raw_bytes_flavor_not_extern_rejection(name));
        }
        if rule_metadata.copy {
            types.record_rejection(copy_not_extern_rejection(name));
        }
        if rule_metadata.extern_companions.is_some() {
            types.record_rejection(extern_companions_not_extern_rejection(name));
        }
        handle_rust_name_pin(types, name, &rule_metadata);
        // A `@custom_encodings` declaration is a property OF the pair — with one half or none, it
        // describes nothing (the rule-position sites all check this the same way).
        reject_custom_encodings_without_pair(types, &format!("rule `{name}`"), &rule_metadata);
        // `@used_as_key` / `@used_as_elem` ask for a wasm surface keyed on the rule's own type. The
        // collapse target is `Option<T>`, which is not a class the wasm boundary can key a map or a
        // list on (it is exposed as a nullable of T's own wasm spelling, so the wrapper would either
        // duplicate T's or name no class at all). Reject rather than mark: marking would put the
        // rule into the demand/elem sets and let the wrapper minters silently produce nothing.
        // Both are scoped to the UNTAGGED collapse, which is the one that registers a transparent
        // alias. A TAGGED `T / null` rule wraps (below), so it mints a real class — but wiring the
        // wasm map-key / loose-list minters for a wrapper over an `Optional` inner is its own work,
        // and until it exists the honest answer is still a refusal, with the reason the tagged shape
        // actually has rather than the untagged one's.
        if null_singleton.is_none() && rule_metadata.key_demand.is_some() {
            types.record_rejection(if tag.is_some() {
                format!(
                    "@used_as_key on `{name}`: a tagged `T / null` rule wraps into a struct over an \
                     `Option<T>` inner, and the wasm map-key wrapper minter has no boundary for an \
                     optional key type. Put the directive on the rule for the inner type `T` instead."
                )
            } else {
                format!(
                    "@used_as_key on `{name}`: a `T / null` rule collapses to a transparent \
                     `Option<T>` alias, which mints no wasm class of its own, so there is nothing \
                     for a map-key wrapper to key on. Put the directive on the rule for the inner \
                     type `T` instead."
                )
            });
        }
        if null_singleton.is_none() && rule_metadata.used_as_elem {
            types.record_rejection(if tag.is_some() {
                format!(
                    "@used_as_elem on `{name}`: a tagged `T / null` rule wraps into a struct over an \
                     `Option<T>` inner, and the wasm loose-list wrapper minter has no boundary for \
                     an optional element type. Put the directive on the rule for the inner type `T` \
                     instead."
                )
            } else {
                format!(
                    "@used_as_elem on `{name}`: a `T / null` rule collapses to a transparent \
                     `Option<T>` alias, which mints no wasm class of its own, so there is no \
                     element type for a loose-list wrapper to hold. Put the directive on the rule \
                     for the inner type `T` instead."
                )
            });
        }
        // `@newtype` mints a wrapper STRUCT around the rule's body; this branch registers a
        // transparent alias and no struct, so the directive has nothing to wrap and would silently
        // do nothing (on every other alias-producing rule body it does mint the wrapper, which is
        // what makes the drop a surprise rather than a documented shape).
        // Scoped to the UNTAGGED collapse. A TAGGED `T / null` rule wraps unconditionally (below),
        // so there the directive is redundant-but-honored exactly as on a single-type tag rule and
        // a `.cbor` rule body — and this rejection was never reached on the tagged path anyway, so
        // `#6.10(uint / null) ; @newtype` used to be a SILENT drop (exit 0, empty stderr, no
        // wrapper). Force-wrapping dissolves that drop by construction.
        if null_singleton.is_none() && tag.is_none() && rule_metadata.newtype.is_some() {
            types.record_rejection(format!(
                "@newtype on `{name}`: a `T / null` rule collapses to a transparent `Option<T>` \
                 alias, and no wrapper struct is generated for it, so the directive would silently \
                 do nothing. Wrap the inner type instead (`{name}_inner = <T> ; @newtype` and \
                 `{name} = {name}_inner / null`), or drop the directive."
            ));
        }
        // A directive on the NON-rule-position arm (`opt = uint ; @x` / `null`) is built and thrown
        // away — the rule slot is the LAST arm's trailing comment (read above). The sibling branch
        // rejects the same misplacement for the same reason; the difference is that a collapse has
        // no VARIANTS, so `@name` and `@doc` have nothing to name or document here either and are
        // caught too (`@name` additionally rejects at the parse-walk seam,
        // `rule_position_name_rejection`, which covers both arms of this shape).
        for choice in &type_choices[..type_choices.len() - 1] {
            let arm_metadata = merge_metadata(
                &RuleMetadata::from(choice.type1.comments_after_type.as_ref()),
                &RuleMetadata::from(choice.comments_after_type.as_ref()),
            );
            let mut misplaced = arm_metadata.non_variant_directives();
            if arm_metadata.comment.is_some() {
                misplaced.push("@doc");
            }
            if !misplaced.is_empty() {
                types.record_rejection(format!(
                    "{} on a non-last arm of the `T / null` rule `{name}`: the rule collapses to a \
                     transparent `Option<T>` alias, so its arms are not variants and carry no \
                     directives of their own — a rule-level directive attaches through the LAST \
                     arm's trailing comment. Move it there (`{name} = … / … ; @…`).",
                    misplaced.join(" / ")
                ));
            }
        }
        if let Some(fixed_null) = null_singleton {
            // `null / null` has one CBOR and Rust state.  It is a named singleton, not a nullable
            // alias, so rule-scoped class directives remain meaningful just as on `x = null`.
            if let Some(demand) = rule_metadata.key_demand {
                types.mark_key_demand(name.clone(), demand);
            }
            if rule_metadata.used_as_elem {
                types.mark_used_as_elem(name.clone());
            }
            register_fixed_singleton(
                types,
                parent_visitor,
                name.clone(),
                fixed_null,
                tag,
                Some(&rule_metadata),
                None,
                cli,
                false,
            );
            return;
        }
        // A TAGGED `T / null` rule WRAPS: the tag rides `final_type`, and a transparent
        // `pub type Topt = Option<u64>;` mints no type to hang it on — `Topt::to_cbor_bytes` would
        // be `Option<u64>`'s, writing the payload with NO tag, while every embed site of the rule
        // writes `write_tag(n)` first and every embed site's decoder requires it. Same reasoning,
        // and the same byte-identical-to-`@newtype` outcome, as the tagged-collection and `.cbor`
        // rule bodies; `register_type_alias`'s wire-facts assert makes the alias spelling
        // unrepresentable rather than merely unused. The UNTAGGED collapse is unaffected and stays
        // the transparent `Option<T>` alias it has always been.
        if tag.is_some() {
            types.register_rust_struct(
                parent_visitor,
                RustStruct::new_wrapper(name.clone(), None, Some(&rule_metadata), final_type, None),
                cli,
            );
            return;
        }
        types.register_type_alias(
            name.clone(),
            AliasInfo::new_from_metadata(final_type, rule_metadata),
        );
    } else {
        let rule_metadata = rule_position_metadata(type_choices);
        if let Some(demand) = rule_metadata.key_demand {
            types.mark_key_demand(name.clone(), demand);
        }
        if rule_metadata.used_as_elem {
            types.mark_used_as_elem(name.clone());
        }
        // Same unconditional record as the single-choice path: a multi-choice rule normally
        // registers an enum, but the tag-258 collapse can instead register a transparent alias, which
        // finalize's registered-nothing check rejects.
        if rule_metadata.no_json_schema_export {
            types.mark_no_json_schema_export(name.clone());
        }
        // A multi-choice type rule can never be an extern marker, so `@raw_bytes_flavor` cannot
        // apply here — reject loudly rather than silently ignore it.
        if rule_metadata.raw_bytes_flavor {
            types.record_rejection(raw_bytes_flavor_not_extern_rejection(name));
        }
        // A multi-choice type rule can never be an extern / raw-bytes marker, so `@copy` cannot apply
        // here — reject loudly rather than silently ignore it.
        if rule_metadata.copy {
            types.record_rejection(copy_not_extern_rejection(name));
        }
        // A multi-choice type rule can never be an extern marker, so `@extern_companions` cannot
        // apply here — reject loudly rather than silently ignore it (the silent flavor re-mints the
        // very classes the directive exists to suppress, and only a distant link fails).
        if rule_metadata.extern_companions.is_some() {
            types.record_rejection(extern_companions_not_extern_rejection(name));
        }
        handle_rust_name_pin(types, name, &rule_metadata);
        // A `@custom_encodings` declaration is a property OF the pair — with one half or none, it
        // describes nothing (the rule-position sites all check this the same way).
        reject_custom_encodings_without_pair(types, &format!("rule `{name}`"), &rule_metadata);
        // A rule-level directive on a NON-LAST arm is built and thrown away: the rule slot is
        // `type_choices.last()` (read above), and `create_variants_from_type_choices` consumes only
        // `.name` and `.comment` from each choice. So on any other arm the directive generates
        // exit-0 output identical to omitting it — the silent-drop class, and the worst instance of
        // it, because the arms of a type choice are a thing people reorder. Reject instead, naming
        // the directive and the remedy. Deliberately NOT applied to the `T / null` optional-inner
        // branch above: that branch's metadata slot is the INNER arm's comment, which for
        // `foo = uint ; @x / null` is the non-last one, so the same rule would be wrong there.
        for choice in &type_choices[..type_choices.len() - 1] {
            let arm_metadata = merge_metadata(
                &RuleMetadata::from(choice.type1.comments_after_type.as_ref()),
                &RuleMetadata::from(choice.comments_after_type.as_ref()),
            );
            let misplaced = arm_metadata.non_variant_directives();
            if !misplaced.is_empty() {
                types.record_rejection(format!(
                    "{} on a non-last arm of the multi-choice type rule `{name}`: a rule-level \
                     directive attaches through the LAST arm's trailing comment, which is the \
                     rule-position slot — on any other arm only `@name` and `@doc` are read (they \
                     name and document that variant). Move it to the last arm, or reorder the arms \
                     so the one carrying it is last.",
                    misplaced.join(" / ")
                ));
            }
        }
        // Build the arms. The tag-258 collapse recognizer below compares the raw arm types for
        // structural equality and then DISCARDS these builds; the surviving product (the nominal set
        // wrapper or a transparent alias) is what registers. Inline `#6.258` occurrences NESTED inside
        // an arm (`foo = #6.258([* #6.258([* uint])]) / …`) carry no registry default here — the single
        // post-collapse seam (`nominalize_inline_sets`, run in `finalize`) nominalizes them on the
        // registered product, so a discarded arm never mints a spurious nominal.
        let variants =
            create_variants_from_type_choices(types, parent_visitor, type_choices, Some(name), cli);
        // A BARE `any` type-choice arm (conceptual `Any`, no CBOR encoding operations) accepts every
        // CBOR item, so it overlaps every other arm and can only ever be a LAST catch-all: any earlier
        // position leaves the arms after it unreachable. We allow it last and reject it
        // elsewhere. The dispatch is forced backtracking (a typed arm matching on wire type but failing
        // on *content* — bounds, inner structure — must fall through to `any`); the strategy selector
        // in generation/enums.rs auto-selects it because `Any::cbor_types` spans all 8 major types, so
        // the non-overlap analysis can never pick the `cbor_type()`-dispatch form for an `any`-armed
        // choice (asserted at that site). A TAGGED `any` arm (`#6.n(any)`) is NOT a catch-all — its
        // `cbor_types()` is `[Tag]`, so it type-dispatches like any other tagged arm and is allowed in
        // ANY position; it flows through the ordinary machinery below. A CONTAINER-of-any arm
        // (`[* any]` = `Array(Any)`, `{* any => any}` = `Map(..)`) has conceptual type Array/Map, not
        // Any, and is not caught here either.
        let is_bare_any = |v: &EnumVariant| {
            matches!(
                &v.data,
                EnumVariantData::RustType(ty)
                    if ty.encodings.is_empty()
                        && matches!(
                            ty.conceptual_type.resolve_alias_shallow(),
                            ConceptualRustType::Any
                        )
            )
        };
        if let Some((bad_pos, _)) = variants
            .iter()
            .enumerate()
            .find(|(i, v)| is_bare_any(v) && *i != variants.len() - 1)
        {
            types.record_rejection(format!(
                "`any` arm makes later arms unreachable — move it last (`{name} = … / any`). A bare \
                 `any` type-choice arm accepts every CBOR item, so it can only be the final \
                 catch-all; here arm {} of {} is `any` but not the last arm. (A tagged `any` arm — \
                 `#6.n(any)` — is not a catch-all and may appear in any position.)",
                bad_pos + 1,
                variants.len()
            ));
            return;
        }
        // Transparent tag-set collapse: a bare (no OUTER tag) two-arm choice differing only in tag
        // presence is not two types — it is one collection whose tag is an encoding detail. Collapse
        // it into the SAME registration a bare `#6.N([* a])` array rule gets (transparent alias +
        // Array/Table-variant RustStruct), with the tag flagged OPTIONAL so it rides an encoding var
        // rather than being mandatory. Recognition is structural + unconditional (no directive):
        // the arm distinction carries no type-level information, so the collapse is the correct
        // default and the enum was the accident. See `recognize_optional_tag_set` and
        // docs/docs/current_capacities.mdx. Happens at parse time, BEFORE the generic machinery, so
        // the generic def stores the already-collapsed collection body (a type-choice-bodied generic
        // def would otherwise panic at `is_enum` during finalize).
        if tag.is_none()
            && let Some((set_tag, base)) = recognize_optional_tag_set(&variants)
        {
            // The collapse target is an array-shaped collection (or a table). `@duplicates` is LIVE
            // for both: an array `reject` swaps to the `OrderedSet` twin, a table `preserve` swaps to
            // the `PairMap` twin — each rides the alias built below. An array `preserve` and a table
            // `reject` are today's defaults (accepted no-op, self-documentation). Nothing is refused.
            // Tag 258 additionally acquires a registry default: a no-directive 258 SET (array inner)
            // defaults to `@duplicates reject`. Extend the collapse notice to state it and the opt-out
            // when that default applies; no such wording when the directive is explicit (either value).
            let is_array = matches!(base.conceptual_type, ConceptualRustType::Array(_));
            // A named non-generic 258 SET rule (array inner) NOMINALIZES into a `Wrapper` struct that
            // owns its `{tag, len, elem}` encodings (Phase 2.2) — both `reject` and `preserve` flavors
            // (policy selects the inner type only). Grammar decides the tag record: the two-arm idiom's
            // OPTIONAL tag rides `OptionallyTagged(258)` (a `TagPresenceEncoding` under
            // --preserve-encodings), attached to the wrapped array type by the dispatch. `@newtype`
            // now carries a custom getter on the wrapper (Phase 2.1's gap-3 rejection is SUBSUMED — a
            // bare `@newtype` emits no getter, `@newtype <name>` a custom one; neither shadows
            // `OrderedSet::get(index)`). A non-258 collapse (or a table inner) stays a transparent
            // optionally-tagged alias, byte-identical to today.
            // NOMINALIZATION covers BOTH non-generic set rules (Phase 2.2) and generic set DEFS
            // (`set<a0> = #6.258([* a0]) / [* a0]`, Phase 2.3). A generic def's wrapper stores the
            // generic PARAM as its wrapped element and is registered as a `GenericDef`; each
            // instantiation then mints ONE nominal wrapper per distinct `<def>_<args>` (`SetKeyHash`)
            // in `GenericInstance::resolve`. A named binding of an instance aliases the instantiation
            // nominal.
            // The collapse registers a COLLECTION (a nominal set wrapper, or a transparent
            // optionally-tagged alias) — never the enum a two-arm choice would otherwise mint — so
            // the arms are not variants and the two directives a variant position consumes have
            // nothing to attach to. `@name` gets the same message every other rule position gives
            // it (`rule_position_name_rejection` cannot reach this shape: it recognizes the T/null
            // collapse from the AST alone, while the tag-set collapse is only known once the arms
            // are BUILT and compared). `@ignore` is the rule-position misplacement its own message
            // already covers — recorded here because the collapse returns before the multi-arm
            // branch's copy of that check.
            if rule_metadata.name.is_some() {
                types.record_rejection(rule_position_name_message(&source_rule_name_of(
                    types, name,
                )));
            }
            if rule_metadata.ignore {
                reject_ignore_not_applicable(types, name);
            }
            let is_set_nominal =
                is_array && well_known_tag_default_duplicates(set_tag, true).is_some();
            let defaulted = rule_metadata.duplicates.is_none()
                && well_known_tag_default_duplicates(set_tag, is_array).is_some();
            let collapse_desc = if is_set_nominal {
                "a nominal set wrapper owning its encodings"
            } else {
                "a transparent optionally-tagged collection"
            };
            // The two branches are different KINDS, so they take different macros. The defaulted one
            // announces a decode-behaviour change the spec did not ask for — a diagnostic, on stderr
            // at the default level. The other reports only what the collapse did, with nothing
            // changed behind the user's back: progress, on stdout at `info`.
            if defaulted {
                crate::warn!(
                    "Collapsing rule `{name}` (tag {set_tag} set idiom) into {collapse_desc}; defaulting to @duplicates reject (IANA set semantics) — write `; @duplicates preserve` on the rule to opt out"
                );
            } else {
                crate::info!(
                    "Collapsing rule `{name}` (tag {set_tag} set idiom) into {collapse_desc}"
                );
            }
            let effective_metadata =
                with_well_known_tag_default(&rule_metadata, set_tag, is_array, None);
            let bounds = base.config.bounds;
            let rust_struct = match base.conceptual_type {
                ConceptualRustType::Array(element_type) if is_set_nominal => {
                    let mut array_type: RustType = ConceptualRustType::Array(element_type).into();
                    if let Some(bounds) = bounds {
                        array_type = array_type.with_bounds(bounds);
                    }
                    RustStruct::new_wrapper(
                        name.clone(),
                        Some(set_tag),
                        Some(&effective_metadata),
                        array_type,
                        None,
                    )
                    .as_optionally_tagged()
                    .as_set_nominal()
                }
                // A NON-258 optional-tag idiom does not nominalize (no set semantics — that is
                // the 258 registry entry's alone, and the branch above owns it), but it still
                // WRAPS: the tag is a wire-affecting property, and a transparent
                // `pub type Foo = Vec<u64>;` carrying `OptionallyTagged(42)` would make
                // `Foo::from_cbor_bytes` refuse the tagged half of the wire the idiom exists to
                // admit while every embed site accepts both. The inner stays a plain `Vec` — the
                // `OrderedSet` twin belongs to the set-nominal branch, not to wrapping.
                ConceptualRustType::Array(element_type) => {
                    let mut array_type: RustType = ConceptualRustType::Array(element_type).into();
                    if let Some(bounds) = bounds {
                        array_type = array_type.with_bounds(bounds);
                    }
                    RustStruct::new_wrapper(
                        name.clone(),
                        Some(set_tag),
                        Some(&effective_metadata),
                        array_type,
                        None,
                    )
                    .as_optionally_tagged()
                }
                // The MAP flavor of the optional-tag idiom WRAPS, for the same reason its array
                // sibling nominalizes above and the mandatory-tag table body does: a transparent
                // `pub type Sm = BTreeMap<..>;` carrying `OptionallyTagged(258)` mints no type to
                // hang the tag on, so `Sm::from_cbor_bytes` would REFUSE the tagged half of the very
                // wire the idiom exists to admit while every embed site accepts both. Every
                // `@duplicates` policy wraps, `preserve` included: the register-side `Wrapper` arm
                // threads the policy onto the stored inner map, so the wrapper's member is the
                // `PairMap`/`NonEmptyPairMap` twin and its wasm boundary names the `PairMapKToV`
                // class the wasm struct walk mints for exactly this inner.
                ConceptualRustType::Map(key_type, value_type) => {
                    let mut map_type: RustType =
                        ConceptualRustType::Map(key_type, value_type).into();
                    if let Some(bounds) = bounds {
                        map_type = map_type.with_bounds(bounds);
                    }
                    RustStruct::new_wrapper(
                        name.clone(),
                        Some(set_tag),
                        Some(&effective_metadata),
                        map_type,
                        None,
                    )
                    .as_optionally_tagged()
                }
                // `recognize_optional_tag_set` only ever returns an Array/Map base
                _ => unreachable!(),
            };
            match generic_params {
                Some(params) => types.register_generic_def(GenericDef::new(params, rust_struct)),
                None => types.register_rust_struct(parent_visitor, rust_struct, cli),
            };
            return;
        }
        // A real multi-arm type choice is a union enum — a non-collection, so `@duplicates` can
        // never apply (its map arm, if any, must be a named rule); `@ignore` never applies here.
        if rule_metadata.duplicates.is_some() {
            reject_duplicates_not_applicable(types, name);
        }
        if rule_metadata.ignore {
            reject_ignore_not_applicable(types, name);
        }
        // A choice-bodied generic DEF that neither collapse recognized — not the `T / null` Option
        // collapse above, not the transparent tag-set idiom just above — would register a union
        // ENUM as the generic def's body. Monomorphizing that is unimplemented: an instance carries
        // the unresolved parameter into generation and aborts there with no diagnosis (`xs<a0> =
        // #6.258([+ a0]) / [* a0]`, instanced and used, died at an `Option::unwrap` in the
        // encoding/serialize walk at exit 101). Refuse here, where the shape is still in hand and
        // the message can name the one choice-bodied idiom that IS supported.
        if generic_params.is_some() {
            types.record_rejection(format!(
                "generic rule `{name}`: a type-choice body is supported only for the transparent \
                 tag-set idiom (`{name}<T> = #6.258([* T]) / [* T]` — two arms differing ONLY in \
                 the tag), which these arms do not form. Any other choice-bodied generic \
                 definition mints a union enum the generic machinery cannot substitute into. Give \
                 each arm its own named rule and choose between them at the use site, or make the \
                 arms match the idiom. {SUPPORTED_GENERIC_DEF_BODIES}"
            ));
            return;
        }
        let rust_struct =
            RustStruct::new_type_choice(name.clone(), tag, Some(&rule_metadata), variants, cli);
        match generic_params {
            Some(params) => types.register_generic_def(GenericDef::new(params, rust_struct)),
            None => types.register_rust_struct(parent_visitor, rust_struct, cli),
        };
    }
}

fn ident_to_primitive(ident: &CDDLIdent) -> Option<Primitive> {
    // TODO: what about aliases that resolve to these? is it even possible to know this at this stage?
    match ident.to_string().as_str() {
        "tstr" | "text" => Some(Primitive::Str),
        "bstr" | "bytes" => Some(Primitive::Bytes),
        "int" => Some(Primitive::I64),
        "uint" => Some(Primitive::U64),
        "nint" => Some(Primitive::N64),
        // One primitive per float prelude name: each names a different set of float VALUES.
        "float16" => Some(Primitive::F16),
        "float32" => Some(Primitive::F32),
        "float64" => Some(Primitive::F64),
        "float16-32" => Some(Primitive::F16To32),
        "float32-64" => Some(Primitive::F32To64),
        "float" => Some(Primitive::Float),
        _other => None,
    }
}

fn type2_to_number_literal(type2: &Type2) -> i128 {
    match type2 {
        Type2::UintValue { value, .. } => *value as i128,
        Type2::IntValue { value, .. } => *value as i128,
        Type2::FloatValue { value, .. } => {
            // FloatToInt trait still experimental so just directly check
            let as_int = *value as i128;
            assert_eq!(
                as_int as f64, *value,
                "decimal not supported. Issue: https://github.com/dcSpark/cddl-codegen/issues/178"
            );
            as_int
        }
        _ => panic!(
            "Value specified: {:?} must be a number literal to be used here",
            type2
        ),
    }
}

/// The numeric value of a literal `Type2` (uint/int/float) as f64, or `None` if it isn't a number
/// literal. Used to build float windows without truncation (ints promote to f64 losslessly here).
fn type2_to_f64(type2: &Type2) -> Option<f64> {
    match type2 {
        Type2::UintValue { value, .. } => Some(*value as f64),
        Type2::IntValue { value, .. } => Some(*value as f64),
        Type2::FloatValue { value, .. } => Some(*value),
        _ => None,
    }
}

/// Whether a literal `Type2` is a DECIMAL float (non-integer-valued), e.g. `10.5`. A whole float
/// (`10.0`) or an int literal is not decimal — those keep the integer window path.
fn type2_is_decimal_float(type2: &Type2) -> bool {
    matches!(type2, Type2::FloatValue { value, .. } if value.fract() != 0.0)
}

/// Numeric classification of a range/control HEAD (the `type2` left of the operator).
#[derive(Clone, Copy, PartialEq)]
enum HeadNumeric {
    /// a float primitive typename (`float16`/`float32`/`float64`) or a float literal (`0.5`)
    Float,
    /// an integer primitive typename (`uint`/`int`/`nint`)
    NamedInt,
    /// an integer literal (`0`, `-3`) — the head of a top-level literal range rule
    IntLiteral,
    Other,
}

fn head_numeric(type2: &Type2) -> HeadNumeric {
    match type2 {
        Type2::Typename { ident, .. } => {
            match ident_to_primitive(&CDDLIdent::new(ident.to_string())) {
                Some(p) if p.is_float() => HeadNumeric::Float,
                Some(Primitive::U64) | Some(Primitive::N64) | Some(Primitive::I64) => {
                    HeadNumeric::NamedInt
                }
                _ => HeadNumeric::Other,
            }
        }
        Type2::FloatValue { .. } => HeadNumeric::Float,
        Type2::UintValue { .. } | Type2::IntValue { .. } => HeadNumeric::IntLiteral,
        _ => HeadNumeric::Other,
    }
}

/// Message naming the offending rule for a graceful rejection on the rule-position control-operator
/// path — float windows, `.ne`, nested `.cbor`. `None` (member position) omits the rule name; the op
/// + remedy still make the message actionable.
fn reject_rule_prefix(rule_name: Option<&RustIdent>) -> String {
    match rule_name {
        Some(r) => format!("rule `{r}`: "),
        None => String::new(),
    }
}

/// Intercepts the float-window and graceful-rejection cases of a numeric range/control operator,
/// BEFORE the integer arms of `parse_control_operator` run (so `type2_to_number_literal`'s decimal
/// assert is never reached from a range/control path). Returns:
/// - `Some(RangeFloat(window))` when the constraint is a float window (float-typed head, or a
///   literal-headed range promoted by a decimal-float endpoint);
/// - `Some(Range((None, None)))` (a harmless placeholder) after RECORDING a graceful rejection for
///   an unsupported shape (`.ne` over a float; a decimal float bound on an integer-typed head);
/// - `None` when this is a genuine integer constraint (or a non-value op like `.size`/`.cbor`),
///   which the caller then handles on the existing integer path.
fn try_float_or_reject(
    types: &mut IntermediateTypes,
    type2: &Type2,
    operator: &Operator,
    rule_name: Option<&RustIdent>,
) -> Option<ControlOperator> {
    let head = head_numeric(type2);
    match operator.operator {
        RangeCtlOp::RangeOp { is_inclusive, .. } => {
            let decimal_endpoint =
                type2_is_decimal_float(type2) || type2_is_decimal_float(&operator.type2);
            let is_float = head == HeadNumeric::Float || decimal_endpoint;
            if !is_float {
                return None;
            }
            match (type2_to_f64(type2), type2_to_f64(&operator.type2)) {
                (Some(start), Some(end)) => Some(ControlOperator::RangeFloat((
                    // range lower endpoint is always included; upper is excluded only for `a...b`
                    Some((start, false)),
                    Some((end, !is_inclusive)),
                ))),
                _ => {
                    // a decimal float endpoint against a non-literal (e.g. named-int) head has no
                    // representable numeric partner — reject gracefully instead of panicking.
                    types.record_rejection(format!(
                        "{}decimal float bound in a range against a non-numeric-literal head is unsupported — use a float head (float64) or integer bounds",
                        reject_rule_prefix(rule_name)
                    ));
                    Some(ControlOperator::Range((None, None)))
                }
            }
        }
        RangeCtlOp::CtlOp { ctrl, .. } => {
            use token::ControlOperator as Ctl;
            // only the value-comparison control ops map onto a float window
            if !matches!(
                ctrl,
                Ctl::EQ | Ctl::NE | Ctl::LE | Ctl::LT | Ctl::GE | Ctl::GT
            ) {
                return None;
            }
            let operand = &operator.type2;
            if head == HeadNumeric::Float {
                if matches!(ctrl, Ctl::NE) {
                    // single-value exclusion has no principled float window (the integer min>max
                    // ±1 hack is meaningless in dense float space) — reject gracefully.
                    types.record_rejection(format!(
                        "{}`.ne` on a float value is unsupported — single-value float exclusion has no representable window; use a range or remove the constraint",
                        reject_rule_prefix(rule_name)
                    ));
                    return Some(ControlOperator::Range((None, None)));
                }
                let v =
                    type2_to_f64(operand).expect("float control operand must be a numeric literal");
                let window = match ctrl {
                    Ctl::EQ => (Some((v, false)), Some((v, false))),
                    Ctl::LE => (None, Some((v, false))),
                    Ctl::LT => (None, Some((v, true))),
                    Ctl::GE => (Some((v, false)), None),
                    Ctl::GT => (Some((v, true)), None),
                    _ => unreachable!(),
                };
                return Some(ControlOperator::RangeFloat(window));
            }
            // integer-typed head with a DECIMAL float bound: do not silently floor — reject.
            if type2_is_decimal_float(operand) {
                types.record_rejection(format!(
                    "{}decimal float bound `{}` on an integer-typed head is unsupported — use an integer bound or a float head (float64)",
                    reject_rule_prefix(rule_name),
                    match operand {
                        Type2::FloatValue { value, .. } => *value,
                        _ => 0.0,
                    }
                ));
                return Some(ControlOperator::Range((None, None)));
            }
            None
        }
    }
}

/// The literal a control operand denotes, or `None` when the operand is not a literal at all.
///
/// `None` is an ordinary user input (`? f: uint .default some_rule`), not a tool bug, so the single
/// caller records a graceful rejection over it. `true`/`false`/`null`/`nil`/`undefined` are CDDL
/// prelude CONSTANTS spelled as typenames rather than as their own `Type2` kinds — the same
/// classification the fixed map-key path makes — so they are lowered here instead of falling into
/// the `None` arm and reading as "not a value".
fn type2_to_fixed_value(type2: &Type2) -> Option<FixedValue> {
    match type2 {
        Type2::UintValue { value, .. } => Some(FixedValue::Uint(*value as u64)),
        Type2::IntValue { value, .. } => Some(FixedValue::Nint(*value as i128)),
        Type2::FloatValue { value, .. } => Some(FixedValue::Float(*value)),
        Type2::TextValue { value, .. } => Some(FixedValue::Text(value.to_string())),
        Type2::B16ByteString { value, .. }
        | Type2::B64ByteString { value, .. }
        | Type2::UTF8ByteString { value, .. } => Some(FixedValue::Bytes(value.to_vec())),
        Type2::Typename { ident, .. } if ident.ident == "true" => Some(FixedValue::Bool(true)),
        Type2::Typename { ident, .. } if ident.ident == "false" => Some(FixedValue::Bool(false)),
        Type2::Typename { ident, .. } if ident.ident == "undefined" => Some(FixedValue::Undefined),
        // Lowered so the head check owns the verdict: `FixedValue::Null` never matches a primitive,
        // so `try_default` refuses it with the message that names the head and the value — which is
        // the accurate account of `? f: uint .default null`, where the value IS a literal and the
        // head simply cannot hold it.
        Type2::Typename { ident, .. } if ident.ident == "null" || ident.ident == "nil" => {
            Some(FixedValue::Null)
        }
        _ => None,
    }
}

/// A `.default` whose OPERAND is not a literal value at all (a type name, a group reference, a
/// nested expression). Distinct from [`unmappable_default_head_rejection`], which is about a literal
/// the HEAD cannot hold: here there is no value to lower in the first place.
fn non_literal_default_operand_rejection(rule_name: Option<&RustIdent>, operand: &Type2) -> String {
    format!(
        "{}`.default {operand}` is not a default VALUE — a default substitutes for an absent value \
         at deserialization, so it must be a literal the head can hold: an integer (`0`, `-2`), a \
         float (`1.5`), a text string (`\"hi\"`), a byte string (`h'CAFE'`), `true`/`false`, or `null`. Spell the value \
         literally, or remove the control.",
        reject_rule_prefix(rule_name)
    )
}

fn parse_control_operator(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type2: &Type2,
    operator: &Operator,
    // The enclosing rule name, when available (top-level rule position), for graceful-rejection
    // messages naming the offending rule. `None` in member position (`rust_type_from_type1`).
    rule_name: Option<&RustIdent>,
    cli: &Cli,
) -> ControlOperator {
    // Float windows and graceful rejections (`.ne` over float, decimal bound on an int head) are
    // decided first, so the integer arms below only ever see genuine integer operands.
    if let Some(result) = try_float_or_reject(types, type2, operator, rule_name) {
        return result;
    }
    let lower_bound = match type2 {
        Type2::Typename { ident, .. } if ident.to_string() == "uint" => Some(0),
        _ => None,
    };
    //todo: read up on other range control operators in CDDL RFC
    // (rangeop / ctlop) S type2
    match operator.operator {
        RangeCtlOp::RangeOp { is_inclusive, .. } => {
            // Both bounds are read as VALUES here, before any name in them resolves, so a
            // non-literal bound is refused on the SHAPE axis: `record_rejection` + the inert
            // full-range placeholder every other graceful arm of this function returns, drained
            // into a graceful `Err` by finalize before generation runs.
            let range_start = match type2 {
                Type2::UintValue { value, .. } => *value as i128,
                Type2::IntValue { value, .. } => *value as i128,
                Type2::FloatValue { value, .. } => *value as i128,
                _ => {
                    types.record_rejection(non_literal_range_bound_rejection(
                        rule_name, "start", type2,
                    ));
                    return ControlOperator::Range((None, None));
                }
            };
            let range_end = match operator.type2 {
                Type2::UintValue { value, .. } => value as i128,
                Type2::IntValue { value, .. } => value as i128,
                Type2::FloatValue { value, .. } => value as i128,
                _ => {
                    types.record_rejection(non_literal_range_bound_rejection(
                        rule_name,
                        "end",
                        &operator.type2,
                    ));
                    return ControlOperator::Range((None, None));
                }
            };
            ControlOperator::Range((
                Some(range_start),
                Some(if is_inclusive {
                    range_end
                } else {
                    // `a...b` is the exclusive range: it EXCLUDES b, so the max valid value
                    // is b-1 (RFC 8610 §3.2). The inclusive `a..b` path keeps range_end as-is.
                    range_end - 1
                }),
            ))
        }
        RangeCtlOp::CtlOp { ctrl, .. } => match ctrl {
            token::ControlOperator::CBORSEQ
            | token::ControlOperator::WITHIN
            | token::ControlOperator::AND => {
                // `.within` / `.and` are LIVE (`uint .within int`, `uint .and (0..9)`);
                // `.cbor-seq` is unreachable — the cddl parser rejects it at parse/lex (matrix
                // `ctl.cborseq` evidence), so no red fixture is constructible for it, but it
                // converts alongside for one graceful arm. Follows the `.size`-on-`int` sibling
                // below: `record_rejection` + an inert full-range placeholder, drained into a
                // graceful `Err` by finalize before generation ever runs.
                types.record_rejection(format!(
                    "{}the `{ctrl}` control operator is unsupported",
                    reject_rule_prefix(rule_name)
                ));
                ControlOperator::Range((None, None))
            }
            token::ControlOperator::DEFAULT => match type2_to_fixed_value(&operator.type2) {
                Some(value) => ControlOperator::Default(value),
                // Same graceful shape as the `.within`/`.and` arm above: record the rejection and
                // hand back the inert full-range placeholder, which `finalize` drains into an
                // `Err` before generation runs. One message — the operand never reaches the head
                // check, which has nothing to say about a non-value.
                None => {
                    types.record_rejection(non_literal_default_operand_rejection(
                        rule_name,
                        &operator.type2,
                    ));
                    ControlOperator::Range((None, None))
                }
            },
            // The `.cbor` TARGET is handed on WHOLE — an `Alias` node included. This seam serves
            // every position the operator can be written at, and the two of them want opposite
            // things from a strip, so neither is done here:
            //
            // * a MEMBER or type-choice arm keeps the node as the field/variant type. The node is
            //   the ONLY thing `generate_serialize`/`generate_deserialize`'s `Alias` arms lift an
            //   aliased rule's `@custom_serialize`/`@custom_deserialize` pair (and its
            //   `@custom_encodings`) from, so stripping it silently re-derives the built-in wire
            //   while a plain member of the same alias routes through the pair — one CDDL type, two
            //   wire forms in one crate. It also decides the field's SPELLING: the `.cbor` here is
            //   the MEMBER expression's, so the alias still denotes the value read inside the byte
            //   string and `output_format.mdx` § "Type spelling at member positions" says the field
            //   is typed by the alias.
            // * a rule BODY registering a transparent alias must strip, because
            //   `register_type_alias` refuses an already-`Alias`-wrapped base — and it does so at
            //   its own seam, through `strip_alias_for_registration`, which carries the wire facts
            //   across the strip so the payload's codec survives the flattening. The rule-body
            //   WRAPPER spelling (`@newtype`, or a tag head) keeps the node, exactly as the
            //   member/arm positions do.
            //
            // Aliases are the only shape this distinction is about; a target naming a real
            // struct/collection is already a `Rust` ident and was never touched.
            //
            // Chain DEPTH is not a variable at either seam (`a = b`, `b = c`, `c = uint`): the alias
            // table never stores a nested alias — `register_type_alias` refuses one — so
            // `resolve_alias`/`new_type` can never hand back more than a single level, and each link
            // flattens (facts and all) as it registers. A four-link chain and a one-link chain reach
            // this seam as the identical `RustType`. Pinned by
            // tests/robustness/cbor_ref_alias_chain.cddl.
            token::ControlOperator::CBOR => ControlOperator::CBOR(rust_type_from_type2(
                types,
                parent_visitor,
                &operator.type2,
                cli,
            )),
            token::ControlOperator::EQ => ControlOperator::Range((
                Some(type2_to_number_literal(&operator.type2)),
                Some(type2_to_number_literal(&operator.type2)),
            )),
            // TODO: this would be MUCH nicer (for error displaying, etc) to handle this in its own dedicated way
            //       which might be necessary once we support other control operators anyway
            token::ControlOperator::NE => ControlOperator::Range((
                Some(type2_to_number_literal(&operator.type2) + 1),
                Some(type2_to_number_literal(&operator.type2) - 1),
            )),
            token::ControlOperator::LE => ControlOperator::Range((
                lower_bound,
                Some(type2_to_number_literal(&operator.type2)),
            )),
            token::ControlOperator::LT => ControlOperator::Range((
                lower_bound,
                Some(type2_to_number_literal(&operator.type2) - 1),
            )),
            token::ControlOperator::GE => {
                ControlOperator::Range((Some(type2_to_number_literal(&operator.type2)), None))
            }
            token::ControlOperator::GT => {
                ControlOperator::Range((Some(type2_to_number_literal(&operator.type2) + 1), None))
            }
            token::ControlOperator::SIZE => {
                let base_range = match &operator.type2 {
                    Type2::UintValue { value, .. } => {
                        ControlOperator::Range((None, Some(*value as i128)))
                    }
                    Type2::IntValue { value, .. } => {
                        ControlOperator::Range((None, Some(*value as i128)))
                    }
                    Type2::FloatValue { value, .. } => {
                        ControlOperator::Range((None, Some(*value as i128)))
                    }
                    Type2::ParenthesizedType { pt, .. } => {
                        assert_eq!(pt.type_choices.len(), 1);
                        let inner_type = &pt.type_choices.first().unwrap().type1;
                        let min = match inner_type.type2 {
                            Type2::UintValue { value, .. } => Some(value as i128),
                            Type2::IntValue { value, .. } => Some(value as i128),
                            Type2::FloatValue { value, .. } => Some(value as i128),
                            _ => unimplemented!(
                                "unsupported type in range control operator: {:?}",
                                operator
                            ),
                        };
                        match &inner_type.operator {
                            // if there was only one value instead of a range, we take that value to be the max
                            // ex: uint .size (1)
                            None => ControlOperator::Range((None, min)),
                            Some(op) => match op.operator {
                                RangeCtlOp::RangeOp { is_inclusive, .. } => {
                                    let value = match op.type2 {
                                        Type2::UintValue { value, .. } => value as i128,
                                        Type2::IntValue { value, .. } => value as i128,
                                        Type2::FloatValue { value, .. } => value as i128,
                                        _ => unimplemented!(
                                            "unsupported type in range control operator: {:?}",
                                            operator
                                        ),
                                    };
                                    let max = Some(if is_inclusive { value } else { value + 1 });
                                    ControlOperator::Range((min, max))
                                }
                                RangeCtlOp::CtlOp { .. } => panic!(""),
                            },
                        }
                    }
                    _ => {
                        unimplemented!("unsupported type in range control operator: {:?}", operator)
                    }
                };
                match type2 {
                    Type2::Typename { ident, .. } if ident.to_string() == "uint" => {
                        // .size 3 means 24 bits
                        match &base_range {
                            ControlOperator::Range((Some(l), Some(h))) => ControlOperator::Range((
                                Some(i128::pow(2, 8 * *l as u32)),
                                Some(i128::pow(2, 8 * *h as u32) - 1),
                            )),
                            ControlOperator::Range((None, Some(h))) => ControlOperator::Range((
                                Some(0),
                                Some(i128::pow(2, 8 * *h as u32) - 1),
                            )),
                            _ => panic!(
                                "unexpected partial range in size control operator: {:?}",
                                operator
                            ),
                        }
                    }
                    Type2::Typename { ident, .. } if ident.to_string() == "int" => {
                        // Rejected, not mapped: per the RFC author (cbor-wg/cddl#32) a control
                        // distributes over `int = uint / nint` and is undefined on `nint`
                        // (per-value non-match), so `int .size N` matches exactly the
                        // `uint .size N` window — the historical signed i{8N} mapping
                        // mis-enforced it in both directions. The rust cddl oracle also
                        // hard-errors on the construct, so an aligned window would be
                        // uncertifiable; revisit when upstream ships the per-value semantics
                        // (ledgered in cddl-matrix/roadmap.toml).
                        types.record_rejection(format!(
                            "{}`.size` on a signed `int` is unsupported — its spec meaning is the `uint .size` window (cbor-wg/cddl#32), which the signed reading mis-enforces; use `uint .size N`, or an explicit range for an N-byte signed int",
                            reject_rule_prefix(rule_name)
                        ));
                        ControlOperator::Range((None, None))
                    }
                    _ => {
                        match base_range {
                            // for strings & byte arrays, specifying an upper value means an exact value (.size 3 means a 3 char string)
                            ControlOperator::Range((None, Some(h))) => {
                                ControlOperator::Range((Some(h), Some(h)))
                            }
                            range => range,
                        }
                    }
                }
            }
            _ => panic!(
                "Unknown (not seen in RFC-8610) range control operator: {}",
                ctrl
            ),
        },
    }
}

fn range_to_primitive(low: Option<i128>, high: Option<i128>, primitive: Primitive) -> RustType {
    match (low, high) {
        (Some(l), Some(h)) if l == u8::MIN as i128 && h == u8::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::U8).into()
        }
        (Some(l), Some(h)) if l == i8::MIN as i128 && h == i8::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::I8).into()
        }
        (Some(l), Some(h)) if l == u16::MIN as i128 && h == u16::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::U16).into()
        }
        (Some(l), Some(h)) if l == i16::MIN as i128 && h == i16::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::I16).into()
        }
        (Some(l), Some(h)) if l == u32::MIN as i128 && h == u32::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::U32).into()
        }
        (Some(l), Some(h)) if l == i32::MIN as i128 && h == i32::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::I32).into()
        }
        (Some(l), Some(h)) if l == u64::MIN as i128 && h == u64::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::U64).into()
        }
        (Some(l), Some(h)) if l == i64::MIN as i128 && h == i64::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::I64).into()
        }
        (Some(l), Some(h)) if l == f32::MIN as i128 && h == f32::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::F32).into()
        }
        (Some(l), Some(h)) if l == f64::MIN as i128 && h == f64::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::F64).into()
        }
        // TODO: use minimal primitive or check here? e.g. uint .le 8 -> U8 instead of U64
        bounds => RustType::from(ConceptualRustType::Primitive(primitive)).with_bounds(bounds),
    }
}

/// Builds the ranged `RustType` for a FLOAT window: the given float primitive with the window
/// attached as `float_bounds`. Unlike `range_to_primitive` there is no "collapses exactly onto a
/// rust primitive" case — a float window is always a genuine sub-domain constraint (a both-`None`
/// window, which never arises from a real op, drops to no bound via `with_float_bounds`).
fn float_range_to_primitive(window: FloatWindow, primitive: Primitive) -> RustType {
    RustType::from(ConceptualRustType::Primitive(primitive)).with_float_bounds(window)
}

/// Registers a top-level FLOAT range/control rule (`c = 0.5..10.5`, `#6.5(0.5..10.5)`,
/// `float64 .le 10.5`). Mirrors `register_literal_range`'s three-way split for float windows:
/// narrow window (or `@newtype`) → bounds-enforcing float wrapper; tag-only → wrapper that writes
/// the tag; otherwise a transparent alias.
#[allow(clippy::too_many_arguments)]
fn register_float_range(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    mut ranged_type: RustType,
    window: FloatWindow,
    outer_tag: Option<usize>,
    rule_metadata: RuleMetadata,
    cli: &Cli,
) {
    if ranged_type.config.float_bounds.is_some() || rule_metadata.newtype.is_some() {
        // window carried in the wrapper's dedicated slot, not on the inner type
        ranged_type.config.float_bounds = None;
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_wrapper_float(
                type_name.clone(),
                outer_tag,
                Some(&rule_metadata),
                ranged_type,
                Some(window),
            ),
            cli,
        );
    } else if outer_tag.is_some() {
        // full-domain float (no residual window) but tagged: wrap so the tag is written/checked
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_wrapper_float(
                type_name.clone(),
                None,
                Some(&rule_metadata),
                ranged_type.tag_if(outer_tag),
                None,
            ),
            cli,
        );
    } else {
        types.register_type_alias(
            type_name.clone(),
            AliasInfo::new_from_metadata(ranged_type.tag_if(outer_tag), rule_metadata),
        );
    }
}

/// Registers a top-level literal-headed range rule (`c = -10..-3`, `e = #6.5(3..10)`) using the same
/// three-way split as the `Type2::Typename` Range arm, so literal-headed ranges wrap identically to
/// their `int .op`-headed equivalents. `ranged_type` is the collapsed primitive (with any residual
/// `config.bounds`); `min_max` is the original window carried into the wrapper's full-window check.
#[allow(clippy::too_many_arguments)]
fn register_literal_range(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    mut ranged_type: RustType,
    min_max: (Option<i128>, Option<i128>),
    outer_tag: Option<usize>,
    rule_metadata: RuleMetadata,
    cli: &Cli,
) {
    if ranged_type.config.bounds.is_some() || rule_metadata.newtype.is_some() {
        // without bounds since passed in other param
        ranged_type.config.bounds = None;
        // has non-rust-primitive matching bounds
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_wrapper(
                type_name.clone(),
                outer_tag,
                Some(&rule_metadata),
                ranged_type,
                Some(min_max),
            ),
            cli,
        );
    } else if outer_tag.is_some() {
        // The range collapses exactly onto a rust primitive (no residual bound to check), but a
        // top-level `#6.n(0..255)` tag rule must still wrap so its standalone `to/from_cbor_bytes`
        // writes/checks the tag — a transparent `pub type` alias would drop it from the wire. The tag
        // rides on `ranged_type` (`.tag_if(outer_tag)`) and there's no `min_max` since the primitive
        // already covers the whole domain.
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_wrapper(
                type_name.clone(),
                None,
                Some(&rule_metadata),
                ranged_type.tag_if(outer_tag),
                None,
            ),
            cli,
        );
    } else {
        // matches to known rust type e.g. u32, i16, etc so just make an alias
        types.register_type_alias(
            type_name.clone(),
            AliasInfo::new_from_metadata(ranged_type.tag_if(outer_tag), rule_metadata),
        );
    }
}

/// The `@extern_companions`-on-a-non-extern-rule rejection message. One owner, because the directive
/// is reachable from three rule shapes (single-choice type rule, multi-choice type rule, plain group)
/// and the text is what a spec author acts on.
fn extern_companions_not_extern_rejection(type_name: &RustIdent) -> String {
    format!(
        "@extern_companions on `{type_name}`: this tag is only valid on a {EXTERN_MARKER} or \
         {RAW_BYTES_MARKER} rule — it declares that the STRUCTURAL wasm companion classes of an \
         externally-defined type (its `<Name>List`, `Map<Name>To…`, …) already exist in a sibling \
         wasm crate, so this crate references them instead of minting duplicate `#[wasm_bindgen]` \
         classes. A rule this crate GENERATES owns its own companions. Remove it from this rule."
    )
}

/// Validate and record an `@extern_companions` declaration for the marker rule `type_name` — either
/// user-supplied flavor (`_CDDL_CODEGEN_EXTERN_TYPE_` or `_CDDL_CODEGEN_RAW_BYTES_TYPE_`; the scope
/// check below is orthogonal to which marker the rule spells, and both name a type this crate does
/// not define while its structural wrappers are named from the rule's ident). Valid
/// only on a LOCALLY-scoped (exported) marker: a DEP-scoped extern (a rule in an
/// `EXTERN_DEPS_DIR` scope) is already served by `--extern-wrapper-index` / `--workspace-dep`, which
/// key on the constituents' owning dependency and can consult the dep's committed index — so a
/// directive there would be a second, weaker authority for the same decision. Graceful
/// `record_rejection` throughout, following the `@rust_name` / `@raw_bytes_flavor` precedent.
///
/// The rule's scope is known here for the same reason it is in `handle_rust_name_pin`:
/// `api::with_types` calls `mark_scope` for every rule before the parse walk runs.
fn handle_extern_companions(
    types: &mut IntermediateTypes,
    type_name: &RustIdent,
    rule_metadata: &RuleMetadata,
) {
    let Some(companions) = rule_metadata.extern_companions.as_ref() else {
        return;
    };
    if !types.scope(type_name).export() {
        types.record_rejection(format!(
            "@extern_companions on `{type_name}`: this rule lives in a {EXTERN_DEPS_DIR} scope, so \
             its collection wrappers are already owned by the dependency-keyed mechanisms — pass \
             `--extern-wrapper-index=<dep>=<dep>/wasm/src/generated/collections.rs` (defer to the \
             classes the dependency's own generation committed) or `--workspace-dep=<dep>` (defer \
             unconditionally). `@extern_companions` exists for a LOCAL marker \
             (`x = {EXTERN_MARKER}` or `x = {RAW_BYTES_MARKER}` in this crate's own spec), which \
             has no dependency edge for those flags to key on. Remove it from this rule."
        ));
        return;
    }
    types.mark_extern_companions(type_name.clone(), companions.clone());
}

/// Validate and record a `@rust_name` pin for the rule `type_name`, following the
/// `@raw_bytes_flavor`-only-on-extern precedent (graceful `record_rejection`, never a panic).
///
/// `@rust_name` pins the dependency's FINAL Rust type name so a consumer reads it across the crate
/// boundary instead of re-deriving it (killing the cross-version naming-skew class). It is therefore
/// valid ONLY on a rule in a non-exported (`EXTERN_DEPS_DIR`) scope — an extern-interface / stub
/// file. On a normally-generated (exported) rule the consumer IS the version that spells the name,
/// so a pin there would silently do nothing: reject it. A pin that camel-cases to a reserved Rust
/// std/prelude type (or a CDDL keyword) is rejected exactly as a derived name would be, so a
/// `@rust_name Option` pin can't slip past the reserved-ident bar that guards derived names.
///
/// The rule's scope is known here: `api::with_types` calls `mark_scope` for every rule before the
/// parse walk runs, so `types.scope(type_name)` is already populated.
fn handle_rust_name_pin(
    types: &mut IntermediateTypes,
    type_name: &RustIdent,
    rule_metadata: &RuleMetadata,
) {
    let Some(pin) = rule_metadata.rust_name.as_ref() else {
        return;
    };
    if types.scope(type_name).export() {
        types.record_rejection(format!(
            "@rust_name on `{type_name}`: reserved for extern-interface / stub files (rules in a \
             {EXTERN_DEPS_DIR} scope). It pins a dependency's final Rust name so a consumer reads it \
             across the crate boundary instead of deriving it; on a normally-generated (exported) \
             rule it would silently do nothing. Remove it."
        ));
    } else if let Some(msg) = reserved_pin_rejection(pin, type_name.as_ref()) {
        types.record_rejection(msg);
    } else {
        types.mark_rust_name_pin(type_name.clone(), pin.clone());
    }
}

/// The rule-position metadata for a plain GROUP rule. cddl binds a group rule's TRAILING comment
/// (`grp = (a: uint) ; @rust_name X`) to the LAST group entry's trailing comment slot, not
/// `comments_after_group` (empirically verified — that slot is `None` for a single-line group rule),
/// the same slot `group_entry_to_field_name` reads for a field-position `@name`. So read from there,
/// falling back to `comments_after_group` for robustness.
///
/// The whole slot is returned; the CALLER decides what to do with each directive, under two
/// different bars. To be **honored** on the rule, a directive must have **no field-position
/// meaning**, because that shared slot makes the two positions indistinguishable here: `@rust_name`
/// (pins the rule's Rust type name), `@no_json_schema_export` (suppresses the rule's
/// schema-registration row), `@custom_json` (suppresses the derives on the struct a SPLICED group
/// mints) and `@used_as_key` (demands comparison derives on that same struct) all qualify — none has
/// any effect at a field. A field-position `@name` sharing the slot legitimately renames that field
/// and is left to the field-naming site, so it must NOT be honored on the rule; the same bar applies
/// to any directive honored here later. To be **reported** — the never-spliced refusal in
/// `IntermediateTypes::finalize` — no bar applies: a group nothing splices emits neither a struct
/// nor a field, so every directive in this slot is inert under BOTH readings and naming it steals no
/// meaning a field would otherwise have had.
///
/// `comments_after_group` is empirically always `None` (see above), so the merge degenerates to the
/// trailing slot; it is written as a merge for the same reason `parse_type` merges its two slots —
/// the cddl parser chooses where to bind, and a directive found in either counts.
///
/// ONE spelling puts a group rule's trailing comment beyond any slot this fn reads: a closing paren
/// on its own line (`grp = (\n a: uint\n) ; @x`). At the pinned fork rev the comment is NOT
/// discarded — the pest bridge's comment binding is a source-position trivia merge, and with no
/// trailing anchor of the group rule's own on that line, the merge binds the comment to the
/// FOLLOWING rule's `comments_before_rule` (or orphans it when the group rule is last). Nothing
/// reads that position, so a directive written there would be lost on formatting alone — which is
/// why [`multiline_group_trailing_directive_rejection`] REFUSES the spelling pre-IR, from the source
/// buffer, before this fn is ever reached for such a rule. Everything below therefore only ever sees
/// a spelling the parser does bind (whole group on one line, or closing paren on the last entry's
/// line). `Rule::Group`'s own `comments_after_rule` is not an escape hatch at this pin: the merge
/// emits no anchor for it (the construction sites' `None`s are pre-merge defaults, not the
/// mechanism), so reading it here is dead code until the fork-side fix is adopted. There is NO
/// second lossy spelling: a last entry's slot cannot be "contended" by a rule-trailing comment on
/// one line, because a CDDL comment runs to end of line — that spelling comments out the closing
/// paren and fails to parse. The fork-side fix (an additive `RuleTrailing` merge fallback) exists on
/// the dcSpark fork's `local-fixes` branch, unadopted by maintainer ruling, and is what would make
/// the refused spelling HONORED rather than merely loud; state and design constraints are tracked in
/// `tests/testing-roadmap.toml` ("Adopt the parser's `RuleTrailing` anchor for multi-line group
/// rules").
fn group_rule_pin_metadata(group: &Group, comments_after_group: Option<&Comments>) -> RuleMetadata {
    let mut metadata = RuleMetadata::from(comments_after_group);
    if let Some((entry, optional_comma)) = group
        .group_choices
        .last()
        .and_then(|gc| gc.group_entries.last())
    {
        // An inline-group last entry has no trailing-comment slot in this position (its members are
        // flattened before a record forms) — unreachable in practice; fall back to the empty slot.
        let empty: Option<Comments> = None;
        let entry_trailing = match entry {
            GroupEntry::ValueMemberKey {
                trailing_comments, ..
            } => trailing_comments,
            GroupEntry::TypeGroupname {
                trailing_comments, ..
            } => trailing_comments,
            GroupEntry::InlineGroup { .. } => &empty,
        };
        let combined = combine_comments(entry_trailing, &optional_comma.trailing_comments);
        let trailing = metadata_from_comments(&combined.unwrap_or_default());
        metadata = merge_metadata(&metadata, &trailing);
    }
    metadata
}

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

/// The one multi-line group-rule refusal message. `tags` is the non-empty directive list, in the
/// stable order `RuleMetadata::all_directives` produces; the first is reused as the example
/// spelling. Pinned by the `robustness_tests` vectors
/// (`multiline_group_rule_trailing_directive_is_refused_not_dropped` and the
/// `KNOWN_RULE_METADATA_TAGS` sweep beside it), which assert the rule ident, the directive spelling
/// and BOTH remedies as substrings; do not reword it.
fn multiline_group_trailing_directive_message(name: &str, tags: &[&str]) -> String {
    let found = tags
        .iter()
        .map(|tag| format!("`{tag}`"))
        .collect::<Vec<_>>()
        .join(", ");
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

#[allow(clippy::too_many_arguments)]
fn parse_type(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    type_choice: &TypeChoice,
    outer_tag: Option<usize>,
    generic_params: Option<Vec<RustIdent>>,
    // Metadata carried in from an enclosing single-type wrapper of the SAME rule (a `#6.n(...)` tag
    // head or a parenthesized type). The cddl AST attaches the rule's trailing comment DSL (e.g.
    // `@newtype`) to the OUTER type1, so recursing into the inner type without threading this would
    // silently drop it — making `tagged = #6.42(text) ; @newtype` a no-op. Empty at the top level.
    inherited_metadata: &RuleMetadata,
    cli: &Cli,
) {
    let type1 = &type_choice.type1;
    let mut rule_metadata = merge_metadata(
        &merge_metadata(
            inherited_metadata,
            &RuleMetadata::from(type1.comments_after_type.as_ref()),
        ),
        &RuleMetadata::from(type_choice.comments_after_type.as_ref()),
    );
    // The recursive-type boundary's auto-`@newtype` repair enters HERE, at the one seam where a
    // rule's directives are settled, so an auto-nominalized collection is indistinguishable from
    // one the spec spelled `; @newtype` on — same wrapper struct, same wasm class, same encoding
    // sidecars, same emit-tests minting. The set is decided by `crate::recursion_boundary` from a
    // FINALIZED IR and seeded before this pass runs (see `IntermediateTypes::set_auto_newtype_rules`);
    // it is empty for every spec with no alias-expansion cycle, so this is inert there. A rule that
    // already carries the directive is left exactly as written — the boundary never overrides a
    // custom getter name the author chose.
    if rule_metadata.newtype.is_none() && types.is_auto_newtype_rule(type_name) {
        rule_metadata.newtype = Some(None);
    }
    let rule_metadata = rule_metadata;
    if let Some(demand) = rule_metadata.key_demand {
        types.mark_key_demand(type_name.clone(), demand);
    }
    if rule_metadata.used_as_elem {
        types.mark_used_as_elem(type_name.clone());
    }
    // `@no_json_schema_export` is valid on any rule that registers a rust type, whatever its shape
    // (extern, record, enum, wrapper, collection typedef), so it is recorded unconditionally here.
    // The "registers no rust struct at all" misplacement cannot be decided from the rule BODY — a
    // generic instance (`my_foo = foo<uint>`) only materializes its struct during finalize's generic
    // resolution — so that rejection is deferred to `IntermediateTypes::finalize`.
    if rule_metadata.no_json_schema_export {
        types.mark_no_json_schema_export(type_name.clone());
    }
    // `@no_alias` is recorded per-ident for the same reason, and at the same seam: the rule kinds
    // that register their transparent alias from `finalize` (a table, an array typedef, a named
    // binding to a generic set nominal) build it without this metadata, so a flag threaded only
    // through `AliasInfo::new_from_metadata` was silently dropped on exactly the shapes that DO emit
    // a `pub type`. Recorded unconditionally — a rule that registers a struct instead has no alias
    // entry for the mark to reach, so it is inert there rather than wrong.
    if rule_metadata.no_alias {
        types.mark_no_alias_rule(type_name.clone());
    }
    // `@doc` likewise: a generic instance's struct config comes from the generic DEFINITION, and a
    // named binding to a set nominal registers its alias without metadata, so both emitted a
    // documentable construct while discarding the rule's own doc.
    if let Some(doc) = &rule_metadata.comment {
        types.mark_rule_doc(type_name.clone(), doc.clone());
    }
    // `@custom_json` likewise: a generic INSTANCE binding mints a struct whose `RustStructConfig` is
    // the generic DEFINITION's, so the binding rule's own flag had no route into the derives it asks
    // to suppress. Recorded unconditionally — a rule that mints no struct has nothing to apply it to,
    // and the transparent-alias family refuses it at `register_type_alias` / the finalize kind-walk.
    if rule_metadata.custom_json {
        types.mark_custom_json_rule(type_name.clone());
    }
    // `@raw_bytes_flavor` is valid ONLY on a `_CDDL_CODEGEN_EXTERN_TYPE_` rule (the extern-marker
    // branch below marks it). Anywhere else it would silently do nothing, so reject loudly here in
    // the house style of the other comment-DSL misuse rejections.
    let is_extern_marker = matches!(
        &type1.type2,
        Type2::Typename { ident, .. } if ident.ident == EXTERN_MARKER
    );
    let is_raw_bytes_marker = matches!(
        &type1.type2,
        Type2::Typename { ident, .. } if ident.ident == RAW_BYTES_MARKER
    );
    // A rule whose whole body is a generic INSTANTIATION (`foo = base<uint>`) — including a named
    // binding to a generic set nominal. Its type is minted during finalize's generic resolution,
    // from the DEFINITION's config, so a directive whose only carrier is that config is written on
    // one rule and read from another.
    let is_generic_instantiation = matches!(
        &type1.type2,
        Type2::Typename {
            generic_args: Some(_),
            ..
        }
    );
    // `@custom_json` on an extern / raw-bytes marker names a type this crate does not define:
    // `new_extern` / `new_raw_bytes` build with `RustStructConfig::default()`, and the named type
    // owns its own JSON impls, so there is no derive list here to suppress and nothing for
    // hand-written impls to be written against that this crate could reach. One class with `@copy`'s
    // "valid only on X" family, but phrased as "invalid HERE" — the message names the marker the
    // rule actually spells, like the custom-codec pair's extern rejection below.
    if rule_metadata.custom_json
        && let Some(marker) = is_extern_marker
            .then_some(EXTERN_MARKER)
            .or(is_raw_bytes_marker.then_some(RAW_BYTES_MARKER))
    {
        types.record_rejection(format!(
            "@custom_json on `{type_name}`: a {marker} rule names a type this crate does not \
             define, so that type already owns its JSON impls — there are no generated \
             serde/schemars derives here to suppress, and the impls you would hand-write belong \
             with the type itself. Give the rule a real CDDL body and put `@custom_json` there \
             (`{type_name} = <body> ; @newtype @custom_json`), or drop the directive and write the \
             impls beside the externally-defined type."
        ));
    }
    if rule_metadata.raw_bytes_flavor && !is_extern_marker {
        types.record_rejection(format!(
            "@raw_bytes_flavor on `{type_name}`: this tag is only valid on a {EXTERN_MARKER} \
             rule — it selects the `<ExternName>RawBytes` wrapper flavor for generic instances \
             whose argument is a {RAW_BYTES_MARKER} type. Remove it from this rule."
        ));
    }
    // `@copy` is valid ONLY on a `_CDDL_CODEGEN_EXTERN_TYPE_` or `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule
    // (the marker branches below record it). Anywhere else it would silently do nothing, so reject
    // loudly here in the house style of the other comment-DSL misuse rejections.
    if rule_metadata.copy && !is_extern_marker && !is_raw_bytes_marker {
        types.record_rejection(format!(
            "@copy on `{type_name}`: this tag is only valid on a {EXTERN_MARKER} or \
             {RAW_BYTES_MARKER} rule — it declares that the externally-defined rust type derives \
             `Copy` so the generator stops cloning it at boundaries. Remove it from this rule."
        ));
    }
    // `@extern_companions` is valid ONLY on a `_CDDL_CODEGEN_EXTERN_TYPE_` or
    // `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule (each marker branch below records it, after the
    // local-vs-dep scope check). Both name a type this crate does not define while the generator
    // still mints that type's STRUCTURAL wasm companion classes from the shapes it is used in
    // (`<Name>List` from list elements and table keys, `Map<K>To<V>`, the preserve flavors) — so
    // both can collide with a sibling crate's hand-written class of the same name. Anywhere else the
    // rule is one this crate GENERATES, which owns its own companions, and the directive would
    // silently do nothing — reject loudly here in the house style of the other comment-DSL misuse
    // rejections.
    if rule_metadata.extern_companions.is_some() && !is_extern_marker && !is_raw_bytes_marker {
        types.record_rejection(extern_companions_not_extern_rejection(type_name));
    }
    // The custom (de)serializer pair is a TYPE-LEVEL override: it replaces the codec of the rust type
    // the rule resolves to, keyed on that type's alias node. Three rule-level spellings delete or
    // bypass the thing it keys on, and each one used to accept the directives and generate as if they
    // were absent — the silent-wire-divergence class the extern-interface guard exists to prevent. Each
    // is a graceful rejection naming the spelling that DOES work. (Field position and the row-entry
    // slots are handled where their metadata is read; the ENUM and single-half-RECORD rule placements
    // are decided by the minted struct's KIND, so they reject in `IntermediateTypes::finalize`.)
    // A `@custom_encodings` declaration is a property OF the pair: it declares the wire the pair's
    // codec writes and reads, so without both halves at this same position there is no codec for it
    // to describe (and it would be read into the rule's metadata and dropped).
    reject_custom_encodings_without_pair(types, &format!("rule `{type_name}`"), &rule_metadata);
    let custom_directives = custom_codec_directives(&rule_metadata);
    for directive in &custom_directives {
        // An extern / raw-bytes rule names a type this crate does not define — `new_extern` and
        // `new_raw_bytes` both store `RustStructConfig::default()`, so the pair never reaches
        // generation and BOTH directions emit the named type's own impls. One class, like `@copy`
        // above treats them; the message names the marker the rule actually spells, since this is
        // "invalid HERE" rather than `@copy`'s "valid only on X or Y". The rejection is scoped to
        // the rule ITSELF spelling a marker: a pair on an ALIAS whose body REFERENCES this rule is
        // the honored "this rule is that type, written differently on the wire" spelling (the
        // general type-level override, applied to a type the crate does not define), which is why
        // the message advertises it as the second remedy.
        if let Some(marker) = is_extern_marker
            .then_some(EXTERN_MARKER)
            .or(is_raw_bytes_marker.then_some(RAW_BYTES_MARKER))
        {
            types.record_rejection(format!(
                "{directive} on `{type_name}`: a {marker} rule names a type this crate does \
                 not define, so that type owns its own serialization impls and the custom \
                 (de)serializer pair never reaches generation. Two spellings work. Give the rule a \
                 real CDDL body and put the pair there (`<rule> = text ; @custom_serialize <fn> \
                 @custom_deserialize <fn>`), which states the wire type in the spec; or keep this \
                 rule as the marker and put the pair on an ALIAS of it (`<alias> = <rule> ; \
                 @custom_serialize <fn> @custom_deserialize <fn>`), which keeps the marker's rust \
                 type and overrides only how that type is written here — under \
                 `--preserve-encodings` an alias of {EXTERN_MARKER} must also declare its wire \
                 with `@custom_encodings`, while an alias of {RAW_BYTES_MARKER} infers it. Both \
                 route the pair through the type-level alias override."
            ));
        }
        // A generic INSTANTIATION binding mints its type during finalize's generic resolution, from
        // the DEFINITION's `RustStructConfig` — so the pair written on the binding rule never
        // reaches generation and both directions keep the definition's generated codec. Covers both
        // shapes the binding takes: a struct-bodied instance (`foo = base<uint>`), and a named
        // binding to a generic set NOMINAL (`foo = gset<uint>`), which additionally lowers to a
        // transparent `pub type` through `AliasInfo::new_manual`. Not fixable by moving the pair to
        // the definition — a generic DEF is refused for its own reason (it names no concrete type).
        if is_generic_instantiation {
            types.record_rejection(format!(
                "{directive} on `{type_name}`: this rule binds a generic instantiation, and the \
                 type it mints is built from the generic DEFINITION's config during generic \
                 resolution — so the pair written here never reaches generation and both \
                 directions keep the definition's generated codec. Give the rule a CDDL body of \
                 its own and put the pair there (`{type_name} = <body> ; @custom_serialize <fn> \
                 @custom_deserialize <fn>`), or declare `{type_name}` as a {EXTERN_MARKER} rule \
                 and hand-write the type in full."
            ));
        }
        // `@no_alias` beside the pair is ACCEPTED and redundant, not refused. It used to be refused
        // because `resolve_alias` STRIPPED the alias node when the rule emitted no `pub type`, and
        // the node is what the emitters look the pair up by — so the pair went with it and both
        // directions silently fell back to the default wire. Node survival is now keyed on
        // `AliasInfo::keeps_alias_node` (emits-a-type OR carries-a-pair) rather than on emission
        // alone, so the pair keeps its routing key with or without the directive. What remains is a
        // request the pair already grants: a pair-carrying alias suppresses its own type projection,
        // because a `pub type` here would carry the aliased type's built-in codec as a standalone
        // wire contradicting the one every embed site writes. Both are honored, and the spelling
        // generates byte-identically either way.
        // B3-026 audits one wrapper owner only: the implicit, untagged homogeneous-table map wrapper
        // made by a complete pair. Explicit `@newtype` asks for the general wrapper surface instead,
        // whose tag/range/set/preserve and Rust/WASM/JSON/WIT contracts were not defined by that
        // delivery, so keep this placement refused rather than extending the table result by analogy.
        if rule_metadata.newtype.is_some() {
            types.record_rejection(format!(
                "{directive} together with `@newtype` on `{type_name}`: this delivery supports and \
                 audits a complete pair only on the implicit homogeneous-table map owner; it does not \
                 define the custom-codec contract for an explicit wrapper (including its tag, range, \
                 set, preserve-encoding, or cross-face behavior). Drop `@newtype` and use the plain \
                 alias spelling (`<rule> = \
                 <body> ; @custom_serialize <fn> @custom_deserialize <fn>`), or declare the type \
                 `{EXTERN_MARKER}` and hand-write it in full."
            ));
        }
    }
    // `@duplicates` is a collection concept. A `Map`/`Array` body (and a tag-head / parenthesized
    // wrapper of one) delegates to `parse_group` / a recursion that performs the shape-aware routing
    // (a `[a, b]` record vs a `[* a]` collection is only distinguishable there, and the tag-set
    // collapse only in `parse_type_choices`), so skip those here and reject only the leaf
    // non-collection rule bodies (aliases, extern/raw-bytes markers, literals, …) permanently.
    if rule_metadata.duplicates.is_some()
        && !matches!(
            &type1.type2,
            Type2::Map { .. }
                | Type2::Array { .. }
                | Type2::TaggedData { .. }
                | Type2::ParenthesizedType { .. }
        )
    {
        reject_duplicates_not_applicable(types, type_name);
    }
    // `@ignore` on a leaf non-collection type rule (`x = uint ; @ignore`) is a misplacement — it is
    // valid only on an open struct-map rest row. Map/Array/Tagged/Paren bodies route to the
    // group/collection arms (or the heterogenous record arm), which reject a rule-position `@ignore`
    // there, so exclude them here exactly as `@duplicates` does.
    if rule_metadata.ignore
        && !matches!(
            &type1.type2,
            Type2::Map { .. }
                | Type2::Array { .. }
                | Type2::TaggedData { .. }
                | Type2::ParenthesizedType { .. }
        )
    {
        reject_ignore_not_applicable(types, type_name);
    }
    handle_rust_name_pin(types, type_name, &rule_metadata);
    match &type1.type2 {
        Type2::Typename {
            ident,
            generic_args,
            ..
        } => {
            if ident.ident == EXTERN_MARKER {
                types.register_rust_struct(
                    parent_visitor,
                    RustStruct::new_extern(type_name.clone()),
                    cli,
                );
                // A GENERIC extern base (`foo<T> = _CDDL_CODEGEN_EXTERN_TYPE_`) registers above as a
                // plain `Extern` struct that drops its generic params, so record its generic-ness
                // here (the only surviving signal) — the bare base names no concrete type and must be
                // skipped by the json-gen schema-row emitter and the extern-interface self-check even
                // when no `foo<uint>` instance exists.
                if generic_params.is_some() {
                    types.mark_generic_extern_base(type_name.clone());
                }
                if rule_metadata.raw_bytes_flavor {
                    // Gated on generic-ness for the same reason `mark_generic_extern_base` above is:
                    // the flavor is a property of a generic INSTANCE, not of the base. On a
                    // non-generic extern there are no instances, so the mark can never be read back
                    // — refuse instead of accepting an inert tag.
                    if generic_params.is_some() {
                        types.mark_raw_bytes_flavor(type_name.clone());
                    } else {
                        types.record_rejection(raw_bytes_flavor_non_generic_extern_rejection(
                            type_name,
                        ));
                    }
                }
                if rule_metadata.copy {
                    types.mark_copy_extern(type_name.clone());
                }
                // `@extern_companions` defers the wasm companion classes minted for a LOCAL extern's
                // collection uses. Every such class is named from the ident at the USE site, and for
                // a generic extern base that ident is the INSTANCE (`i = foo<uint>` used as
                // `[* i]` mints `IList`, never `FooList`), so a deferral declared on the base is
                // looked up under a name nothing ever asks for. Gated on generic-ness for the same
                // reason `@raw_bytes_flavor` above is, in the opposite direction — the flavor is a
                // property of instances, the deferral of the concrete type.
                if generic_params.is_some() && rule_metadata.extern_companions.is_some() {
                    types.record_rejection(format!(
                        "@extern_companions on `{type_name}`: a generic extern BASE names no \
                         concrete type, and every wasm companion class is named from the ident at \
                         the USE site — an instance `i = {type_name}<uint>` used as `[* i]` mints \
                         `IList`, never `{type_name}List` — so a deferral declared on the base is \
                         never consulted. Declare the concrete shape as its own non-generic extern \
                         rule and put the deferral there (`i = {EXTERN_MARKER} ; \
                         @extern_companions <prefix>=IList`), or remove the directive."
                    ));
                } else {
                    handle_extern_companions(types, type_name, &rule_metadata);
                }
            } else if ident.ident == RAW_BYTES_MARKER {
                // A GENERIC raw-bytes base (`foo<T> = _CDDL_CODEGEN_RAW_BYTES_TYPE_`) is refused,
                // where the extern marker above merely RECORDS its generic-ness: an extern names an
                // arbitrary hand-written type, which can legitimately be parameterized, but a
                // raw-bytes type IS its own bytes and has no element for a parameter to name. The
                // registration below drops the params (a `RawBytesType` struct has none), so the
                // base then emits rows spelling a BARE `Foo` — the extern-interface self-check's
                // `_assert_raw_bytes::<crate::generated::Foo>()` and, under
                // `--json-schema-export`, the json-gen `reg.add::<cddl_lib::Foo>()` — each E0107
                // against the parameterized `Foo<T>` the marker promised, at exit 0 with empty
                // stderr. Return-early (no registration), following the control-op-on-`any`
                // rejection below.
                if generic_params.is_some() {
                    types.record_rejection(format!(
                        "generic rule `{type_name}`: a {RAW_BYTES_MARKER} rule cannot take generic \
                         parameters — a raw-bytes type is exactly its own bytes and carries no \
                         element type for a parameter to name, so `{type_name}<…>` would emit \
                         self-check and schema rows naming a bare `{type_name}` that cannot compile \
                         against the parameterized type the marker declares. Declare it \
                         non-generic (`{type_name} = {RAW_BYTES_MARKER}`)."
                    ));
                    return;
                }
                types.register_rust_struct(
                    parent_visitor,
                    RustStruct::new_raw_bytes(type_name.clone()),
                    cli,
                );
                if rule_metadata.copy {
                    types.mark_copy_extern(type_name.clone());
                }
                // Same recording as the extern-marker arm above, and for the same reason: a
                // raw-bytes type is user-defined too, and the collection wrappers minted from the
                // shapes it appears in are named from its ident — so a sibling wasm crate that
                // already publishes `<Name>List` collides with a local mint exactly as an extern's
                // does. No generic-ness gate is needed here (unlike the extern arm's): a generic
                // raw-bytes base returned above.
                handle_extern_companions(types, type_name, &rule_metadata);
            } else {
                // Note: this handles bool constants too, since we apply the type aliases and they resolve
                // and there's no Type2::BooleanValue
                let cddl_ident = CDDLIdent::new(ident.to_string());
                let control = type1.operator.as_ref().map(|op| {
                    parse_control_operator(
                        types,
                        parent_visitor,
                        &type1.type2,
                        op,
                        Some(type_name),
                        cli,
                    )
                });
                // A control operator on `any` (`.size`, `.cbor`, ranges, `.lt`/`.le`/…) is
                // semantically empty — `any` already accepts every CBOR item — and `any` is not a
                // primitive, so the range/size machinery below would panic unwrapping
                // `ident_to_primitive`. Reject it gracefully; allow on demand once
                // a fixture proves a meaningful semantics.
                if control.is_some() && cddl_ident.to_string() == "any" {
                    types.record_rejection(format!(
                        "a control operator (`.size`/`.cbor`/range/`.lt`…) on `{type_name} = any …` \
                         is not supported: `any` already accepts every CBOR item, so the constraint \
                         is empty. Remove it (`{type_name} = any`) or apply it to a concrete type."
                    ));
                    return;
                }
                match control {
                    Some(control) => {
                        assert!(
                            generic_params.is_none(),
                            "Generics combined with range specifiers not supported"
                        );
                        match control {
                            ControlOperator::Range(min_max) => {
                                // when declared top-level we make a new type as the default behavior like before
                                let Some(primitive) = ident_to_primitive(&cddl_ident) else {
                                    types.record_rejection(unmapped_control_head_rejection(
                                        type_name,
                                        &cddl_ident,
                                    ));
                                    return;
                                };
                                let mut ranged_type =
                                    range_to_primitive(min_max.0, min_max.1, primitive);
                                if ranged_type.config.bounds.is_some()
                                    || rule_metadata.newtype.is_some()
                                {
                                    // without bounds since passed in other param
                                    ranged_type.config.bounds = None;
                                    // has non-rust-primitive matching bounds
                                    types.register_rust_struct(
                                        parent_visitor,
                                        RustStruct::new_wrapper(
                                            type_name.clone(),
                                            outer_tag,
                                            Some(&rule_metadata),
                                            ranged_type,
                                            Some(min_max),
                                        ),
                                        cli,
                                    );
                                } else if outer_tag.is_some() {
                                    // The range collapses exactly onto a rust primitive (no residual
                                    // bound to check), but a top-level `#6.n(uint .le 255)` tag rule
                                    // must still wrap so its standalone `to/from_cbor_bytes`
                                    // writes/checks the tag — a transparent `pub type` alias would drop
                                    // it from the wire. Same shape as the primitive tag arm below: the
                                    // tag rides on `ranged_type` (`.tag_if(outer_tag)`) and there's no
                                    // `min_max` since the primitive already covers the whole domain.
                                    types.register_rust_struct(
                                        parent_visitor,
                                        RustStruct::new_wrapper(
                                            type_name.clone(),
                                            None,
                                            Some(&rule_metadata),
                                            ranged_type.tag_if(outer_tag),
                                            None,
                                        ),
                                        cli,
                                    );
                                } else {
                                    // matches to known rust type e.g. u32, i16, etc so just make an alias
                                    types.register_type_alias(
                                        type_name.clone(),
                                        AliasInfo::new_from_metadata(
                                            ranged_type.tag_if(outer_tag),
                                            rule_metadata,
                                        ),
                                    );
                                }
                            }
                            ControlOperator::RangeFloat(window) => {
                                // `float64 .le 10.5` (float typename head): wrap into a float
                                // bounds-enforcing newtype (or tag/alias) — same three-way split.
                                // The same unmapped-head guard as the integer arm above: a float
                                // WINDOW is only built for a head `ident_to_primitive` maps, so
                                // this is the sibling backstop rather than a reachable shape
                                // today — it exists so the next unmapped name class rejects
                                // instead of re-earning the panic.
                                let Some(primitive) = ident_to_primitive(&cddl_ident) else {
                                    types.record_rejection(unmapped_control_head_rejection(
                                        type_name,
                                        &cddl_ident,
                                    ));
                                    return;
                                };
                                let ranged_type = float_range_to_primitive(window, primitive);
                                register_float_range(
                                    types,
                                    parent_visitor,
                                    type_name,
                                    ranged_type,
                                    window,
                                    outer_tag,
                                    rule_metadata,
                                    cli,
                                );
                            }
                            ControlOperator::CBOR(ty) => match ident_to_primitive(&cddl_ident) {
                                Some(Primitive::Bytes) => {
                                    // A second `CBORBytes` on this chain (the INLINE spelling
                                    // `bytes .cbor (bytes .cbor T)`) is applied like any other: each
                                    // `.cbor` level owns its own depth-suffixed staging buffer,
                                    // reader and encoding member (`cbor_bytes_infix` and its
                                    // siblings), so the levels no longer contend for one name.
                                    let cbor_bytes_type = ty.as_bytes().tag_if(outer_tag);
                                    // A `.cbor` rule body ALWAYS wraps, `@newtype` or not: the
                                    // byte-string framing (and any outer tag riding on
                                    // `cbor_bytes_type` via `.tag_if(outer_tag)`) is a wire-affecting
                                    // property of the rule, and a transparent `pub type X = T` alias
                                    // mints no type to hang it on — `X::to_cbor_bytes` would be `T`'s,
                                    // writing the BARE inner form while every embed site of `X`
                                    // writes the wrapped one. So the `@newtype` spelling is redundant
                                    // here exactly as it is on a single-type tag rule: both spellings
                                    // produce the identical wrapper struct, and
                                    // `register_type_alias`'s wire-facts assert keeps the alias
                                    // spelling unrepresentable rather than merely unused.
                                    //
                                    // The payload's `Alias` node is KEPT (as a member or arm keeps
                                    // it): it is the only thing the emitter's `Alias` arms lift a
                                    // wrapped rule's `@custom_serialize`/`@custom_deserialize` pair
                                    // from.
                                    //
                                    // A fixed payload needs the same direct-codec owner as a bare
                                    // literal rule.  Keep the complete `.cbor`/tag operation chain
                                    // on its one arm, so preserve encoding metadata belongs to this
                                    // nominal owner rather than to a non-existent wrapper member.
                                    if matches!(
                                        cbor_bytes_type.conceptual_type.resolve_alias_shallow(),
                                        ConceptualRustType::Fixed(_)
                                    ) {
                                        register_fixed_singleton(
                                            types,
                                            parent_visitor,
                                            type_name.clone(),
                                            cbor_bytes_type,
                                            None,
                                            Some(&rule_metadata),
                                            generic_params.as_deref(),
                                            cli,
                                            false,
                                        );
                                        return;
                                    }
                                    types.register_rust_struct(
                                        parent_visitor,
                                        RustStruct::new_wrapper(
                                            type_name.clone(),
                                            None,
                                            Some(&rule_metadata),
                                            cbor_bytes_type,
                                            None,
                                        ),
                                        cli,
                                    );
                                }
                                // Not a byte-string head: refuse the shape (RFC 8610 restricts
                                // `.cbor` to byte strings) rather than aborting, and register
                                // nothing — the same return-early shape the unmapped-head guards
                                // above use, with `finalize` draining the rejection before any
                                // reference to the unregistered rule can be emitted.
                                _ => types.record_rejection(non_bytes_cbor_head_rejection(
                                    Some(type_name),
                                    &cddl_ident.to_string(),
                                )),
                            },
                            ControlOperator::Default(default_value) => {
                                let inner_type =
                                    rust_type_from_type2(types, parent_visitor, &type1.type2, cli);
                                // Same reasoning as the primitive tag arm below: a top-level
                                // `#6.n(uint .default 5)` must wrap so its standalone
                                // `to/from_cbor_bytes` writes/checks the tag (a transparent alias drops
                                // it from the wire). The `.default` is dropped inside the wrapper: a
                                // default substitutes for an *absent* value, and a standalone tagged
                                // value is always present, so it has no meaning here (the preserve path's
                                // per-field default-present encoding tracking has no struct field to hang
                                // off either). The tag rides on the inner type (`.tag_if(outer_tag)`).
                                if rule_metadata.newtype.is_some() || outer_tag.is_some() {
                                    types.register_rust_struct(
                                        parent_visitor,
                                        RustStruct::new_wrapper(
                                            type_name.clone(),
                                            None,
                                            Some(&rule_metadata),
                                            inner_type.tag_if(outer_tag),
                                            None,
                                        ),
                                        cli,
                                    );
                                } else {
                                    // The head may be one the default cannot be lowered onto — a
                                    // named type with no rust primitive (`tdate`), or the inert
                                    // placeholder a refused prelude name already left behind. Refuse
                                    // at the APPLICATION, and register the rule as the UNDEFAULTED
                                    // type it would otherwise have been, so later references still
                                    // resolve while `finalize` drains both this rejection and any
                                    // the head's own seam recorded first.
                                    let aliased =
                                        match inner_type.try_default(default_value.clone()) {
                                            Ok(defaulted) => defaulted,
                                            Err(undefaulted) => {
                                                types.record_rejection(
                                                    unmappable_default_head_rejection(
                                                        Some(type_name),
                                                        &type1.type2,
                                                        &default_value,
                                                    ),
                                                );
                                                undefaulted
                                            }
                                        };
                                    types.register_type_alias(
                                        type_name.clone(),
                                        AliasInfo::new_from_metadata(
                                            aliased.tag_if(outer_tag),
                                            rule_metadata,
                                        ),
                                    );
                                }
                            }
                        }
                    }
                    None => {
                        let concrete_type = types.new_type(&cddl_ident, cli);
                        if matches!(
                            concrete_type.conceptual_type.resolve_alias_shallow(),
                            ConceptualRustType::Fixed(_)
                        ) {
                            register_fixed_singleton(
                                types,
                                parent_visitor,
                                type_name.clone(),
                                concrete_type,
                                outer_tag,
                                Some(&rule_metadata),
                                generic_params.as_deref(),
                                cli,
                                false,
                            );
                            return;
                        }
                        let concrete_type = concrete_type.tag_if(outer_tag);
                        // Remember the aliased ident after stripping its `Alias` wrapper. The wasm
                        // alias can point at its wrapper struct if it has one (resolved at emission
                        // via `has_wasm_wrapper`, so forward references work), while the recursive
                        // boundary retains the original source edge instead of only its structural
                        // base. Read-only: the strip itself belongs to the ALIAS branch alone (see
                        // there).
                        let mut stripped_alias_target = None;
                        if let ConceptualRustType::Alias(AliasIdent::Rust(rust_ident), _) =
                            &concrete_type.conceptual_type
                        {
                            stripped_alias_target = Some(rust_ident.clone());
                        }
                        match &generic_params {
                            Some(_params) => {
                                // A generic def whose body is another NAMED type
                                // (`bar<V> = foo<V, uint>`) forwards its parameters into a second
                                // definition; nothing registers a struct here for `bar<…>` to
                                // substitute into, so the parameters would be unbound. Refused at
                                // parse time rather than aborted. (Resolving the target here and
                                // storing the resolved struct as this rule's own `GenericDef` is
                                // the shape that would support it.)
                                types.record_rejection(format!(
                                    "generic rule `{type_name}`: a body that is another named type \
                                     (`{type_name}<…> = <other>`) is not supported — it forwards \
                                     the parameters into a second definition and registers no \
                                     struct of its own, so they would be unbound. Spell the \
                                     structure out in this rule's own body. \
                                     {SUPPORTED_GENERIC_DEF_BODIES}"
                                ));
                            }
                            None => {
                                match generic_args {
                                    Some(arg) => {
                                        // This is for named generic instances such as:
                                        // foo = bar<text>
                                        let generic_args: Vec<RustType> = arg
                                            .args
                                            .iter()
                                            .map(|a| {
                                                rust_type_from_type1(
                                                    types,
                                                    parent_visitor,
                                                    &a.arg,
                                                    cli,
                                                )
                                            })
                                            .collect();
                                        // The instantiation nominal a set binding aliases TO
                                        // (`named_set = set<key_hash>` → `SetKeyHash`); identical to
                                        // the anonymous-use spelling so both dedup to one nominal.
                                        let canonical_ident =
                                            RustIdent::new(generic_instance_canonical_cddl_ident(
                                                &cddl_ident,
                                                &generic_args,
                                            ));
                                        types.register_generic_instance(GenericInstance::new(
                                            type_name.clone(),
                                            RustIdent::new(cddl_ident.clone()),
                                            generic_args,
                                            // author-declared rule name (`foo = bar<text>`), not
                                            // synthesized — keeps its own wasm class / criterion-8 name.
                                            false,
                                            canonical_ident,
                                        ))
                                    }
                                    None => {
                                        // A top-level single-type tag rule (`x = #6.n(<primitive|named>)`)
                                        // must emit the tag-writing/tag-checking wrapper, not a transparent
                                        // `pub type` alias whose standalone `to/from_cbor_bytes` would drop
                                        // the tag from the wire (a CBOR conformance bug). `outer_tag` is set
                                        // exactly when we descended through a tag head, so it forces the same
                                        // wrapper `@newtype` opts into — making `@newtype` redundant (not a
                                        // double wrapper) on a tag rule. The tag rides on `concrete_type`
                                        // (`.tag_if(outer_tag)` above), so the wrapper writes it.
                                        // `@newtype` on a bare `any` rule is a graceful rejection:
                                        // the wrapper is unproven through the
                                        // surface machinery and cheap to allow later once a fixture
                                        // proves it. A TAGGED any (`#6.n(any)`, `outer_tag` set) is a
                                        // supported position whose wrapper the tag forces (@newtype
                                        // redundant there), so only the newtype-driven untagged case
                                        // is caught here.
                                        if rule_metadata.newtype.is_some()
                                            && outer_tag.is_none()
                                            && matches!(
                                                concrete_type
                                                    .conceptual_type
                                                    .resolve_alias_shallow(),
                                                ConceptualRustType::Any
                                            )
                                        {
                                            types.record_rejection(format!(
                                                "@newtype on `{type_name} = any` is not supported in \
                                                 this phase: use a transparent alias \
                                                 (`{type_name} = any`, no @newtype) — `any` lowers to \
                                                 the AnyCbor runtime type directly. (Newtype-wrapping \
                                                 `any` is planned once a fixture proves the surface.)"
                                            ));
                                            return;
                                        }
                                        // A PRELUDE CONSTANT body (`true`/`false`/`null`/`nil`)
                                        // resolves to a bare `Fixed`, which has no member Rust
                                        // representation. The untagged spelling reaches
                                        // `register_type_alias`'s guard below and is rejected there;
                                        // a tag head (or `@newtype`) diverts it to the WRAPPER seam
                                        // instead, which would render the `Fixed` as the wrapper's
                                        // inner member type and panic `for_rust_member` during
                                        // generation. Reject at both seams through the one shared
                                        // message, so `#6.11(true)` is classified exactly like the
                                        // literal-inner sibling `#6.5(5)` the alias seam already
                                        // rejects. Registering the wrapper anyway (rather than
                                        // returning early) matches the alias guard's reasoning: a
                                        // sibling rule may reference this one, and a dropped
                                        // registration would dangle that lookup during the parse
                                        // walk — before `finalize` surfaces the graceful `Err`. The
                                        // wrapper is harmless because generation never runs once a
                                        // rejection is recorded.
                                        if rule_metadata.newtype.is_some() || outer_tag.is_some() {
                                            if let ConceptualRustType::Fixed(fixed) = concrete_type
                                                .conceptual_type
                                                .resolve_alias_shallow()
                                            {
                                                let fixed = fixed.clone();
                                                types.record_bare_fixed_rule_rejection(
                                                    type_name, &fixed,
                                                );
                                            }
                                            types.register_rust_struct(
                                                parent_visitor,
                                                RustStruct::new_wrapper(
                                                    type_name.clone(),
                                                    None,
                                                    Some(&rule_metadata),
                                                    concrete_type,
                                                    None,
                                                ),
                                                cli,
                                            );
                                        } else {
                                            // Stripping the alias inlines the type for serialization
                                            // (the rust side stays a transparent `pub type`), and is
                                            // REQUIRED here: `register_type_alias` refuses a base type
                                            // already wrapped in `Alias`. The WRAPPER branch above must
                                            // NOT strip: `generate_serialize`/`generate_deserialize`'s
                                            // `Alias` arms are what lift the aliased rule's
                                            // `@custom_serialize`/`@custom_deserialize` pair (and its
                                            // `@custom_encodings` declaration) into the emitted codec, so
                                            // a stripped wrapper silently re-derives the built-in wire and
                                            // `x = #6.n(annotated_alias)` disagrees with a plain member of
                                            // the same alias about one type's wire form. Where the node
                                            // cannot be kept, the FACTS travel instead — that is what
                                            // makes `re = annotated_alias` agree with the alias it
                                            // re-names rather than silently re-deriving the built-in
                                            // wire for every member declared through it.
                                            let mut alias_metadata = rule_metadata.clone();
                                            let (concrete_type, inherited_from) =
                                                strip_alias_for_registration(
                                                    types,
                                                    concrete_type,
                                                    &mut alias_metadata,
                                                );
                                            types.register_type_alias(
                                                type_name.clone(),
                                                AliasInfo::new_from_metadata(
                                                    concrete_type,
                                                    alias_metadata,
                                                )
                                                .with_stripped_alias_target(stripped_alias_target)
                                                .with_inherited_wire_metadata(inherited_from),
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        Type2::Map { group, .. } => {
            parse_group(
                types,
                parent_visitor,
                group,
                type_name,
                Representation::Map,
                outer_tag,
                generic_params,
                &rule_metadata,
                cli,
            );
        }
        Type2::Array { group, .. } => {
            // TODO: We could potentially generate an array-wrapper type around this
            // possibly based on the occurency specifier.
            parse_group(
                types,
                parent_visitor,
                group,
                type_name,
                Representation::Array,
                outer_tag,
                generic_params,
                &rule_metadata,
                cli,
            );
        }
        Type2::TaggedData { tag, t, .. } => {
            if outer_tag.is_some() {
                panic!("doubly nested tags are not supported");
            }
            let tag_unwrap =
                tag_literal(tag).expect("not sure what empty tag here would mean - unsupported");
            match t.type_choices.len() {
                1 => {
                    let inner_type = &t.type_choices.first().unwrap();
                    parse_type(
                        types,
                        parent_visitor,
                        type_name,
                        inner_type,
                        Some(tag_unwrap),
                        generic_params,
                        // same rule: carry the outer rule's DSL (e.g. `@newtype`) inward
                        &rule_metadata,
                        cli,
                    );
                }
                _ => {
                    parse_type_choices(
                        types,
                        parent_visitor,
                        type_name,
                        &t.type_choices,
                        tag_literal(tag),
                        generic_params,
                        cli,
                    );
                }
            };
        }
        // Note: bool constants are handled via Type2::Typename
        Type2::IntValue { value, .. } => {
            let fallback_type = ConceptualRustType::Fixed(FixedValue::Nint(*value as i128));

            let control = type1.operator.as_ref().map(|op| {
                parse_control_operator(
                    types,
                    parent_visitor,
                    &type1.type2,
                    op,
                    Some(type_name),
                    cli,
                )
            });
            // We end up here with ranges like foo = 0..5 which is why we're not just reporting a fixed value
            match control {
                Some(ControlOperator::Range(min_max)) => {
                    // Mirror the Typename Range arm's three-way split (see above): a literal-headed
                    // top-level range rule must WRAP when a residual bound (or @newtype) survives the
                    // primitive collapse, or when it carries a tag, so its standalone to/from_cbor_bytes
                    // enforces the window / writes the tag. A transparent `pub type` alias has no
                    // ctor/deserialize to check the bounds (spec-invalid data accepted standalone) and
                    // drops the tag from the wire (a CBOR conformance bug).
                    register_literal_range(
                        types,
                        parent_visitor,
                        type_name,
                        range_to_primitive(min_max.0, min_max.1, Primitive::I64),
                        min_max,
                        outer_tag,
                        rule_metadata,
                        cli,
                    );
                }
                Some(ControlOperator::RangeFloat(window)) => {
                    // `foo = 0..10.5` (int-literal head promoted by a decimal endpoint): wraps as a
                    // float bounds-enforcing newtype, primitive f64.
                    register_float_range(
                        types,
                        parent_visitor,
                        type_name,
                        float_range_to_primitive(window, Primitive::Float),
                        window,
                        outer_tag,
                        rule_metadata,
                        cli,
                    );
                }
                _ => {
                    register_fixed_singleton(
                        types,
                        parent_visitor,
                        type_name.clone(),
                        RustType::from(fallback_type),
                        outer_tag,
                        Some(&rule_metadata),
                        generic_params.as_deref(),
                        cli,
                        false,
                    );
                }
            }
        }
        Type2::UintValue { value, .. } => {
            let fallback_type = ConceptualRustType::Fixed(FixedValue::Uint(*value as u64));

            let control = type1.operator.as_ref().map(|op| {
                parse_control_operator(
                    types,
                    parent_visitor,
                    &type1.type2,
                    op,
                    Some(type_name),
                    cli,
                )
            });
            // We end up here with ranges like foo = 0..5 which is why we're not just reporting a fixed value
            match control {
                Some(ControlOperator::Range(min_max)) => {
                    // Same three-way split as the IntValue range arm above (mirrors Typename Range).
                    register_literal_range(
                        types,
                        parent_visitor,
                        type_name,
                        range_to_primitive(min_max.0, min_max.1, Primitive::U64),
                        min_max,
                        outer_tag,
                        rule_metadata,
                        cli,
                    );
                }
                Some(ControlOperator::RangeFloat(window)) => {
                    // `foo = 0..10.5` (uint-literal head promoted by a decimal endpoint): float f64.
                    register_float_range(
                        types,
                        parent_visitor,
                        type_name,
                        float_range_to_primitive(window, Primitive::Float),
                        window,
                        outer_tag,
                        rule_metadata,
                        cli,
                    );
                }
                _ => {
                    register_fixed_singleton(
                        types,
                        parent_visitor,
                        type_name.clone(),
                        RustType::from(fallback_type),
                        outer_tag,
                        Some(&rule_metadata),
                        generic_params.as_deref(),
                        cli,
                        false,
                    );
                }
            }
        }
        Type2::TextValue { value, .. } => {
            register_fixed_singleton(
                types,
                parent_visitor,
                type_name.clone(),
                RustType::new(ConceptualRustType::Fixed(FixedValue::Text(
                    value.to_string(),
                ))),
                outer_tag,
                Some(&rule_metadata),
                generic_params.as_deref(),
                cli,
                false,
            );
        }
        Type2::B16ByteString { value, .. }
        | Type2::B64ByteString { value, .. }
        | Type2::UTF8ByteString { value, .. } => {
            register_fixed_singleton(
                types,
                parent_visitor,
                type_name.clone(),
                RustType::new(ConceptualRustType::Fixed(FixedValue::Bytes(value.to_vec()))),
                outer_tag,
                Some(&rule_metadata),
                generic_params.as_deref(),
                cli,
                false,
            );
        }
        Type2::FloatValue { value, .. } => {
            let fallback_type = ConceptualRustType::Fixed(FixedValue::Float(*value));

            let control = type1.operator.as_ref().map(|op| {
                parse_control_operator(
                    types,
                    parent_visitor,
                    &type1.type2,
                    op,
                    Some(type_name),
                    cli,
                )
            });
            // We end up here with float ranges like foo = 0.5..10.5 which is why we're not just
            // reporting a fixed value.
            match control {
                Some(ControlOperator::RangeFloat(window)) => {
                    // Top-level literal float range (`foo = 0.5..10.5`, `#6.5(0.5..10.5)`): WRAP into
                    // a bounds-enforcing float newtype so its standalone to/from_cbor_bytes enforces
                    // the window (and writes the tag). Pre-fix this dropped the window into a bare
                    // `pub type Foo = f64;` alias — silent non-enforcement.
                    register_float_range(
                        types,
                        parent_visitor,
                        type_name,
                        float_range_to_primitive(window, Primitive::Float),
                        window,
                        outer_tag,
                        rule_metadata,
                        cli,
                    );
                }
                // an integer window can arise if a whole-float range were routed here, but the float
                // head always takes the RangeFloat path above; keep the alias fallback for the bare
                // constant (no operator) case.
                Some(ControlOperator::Range(min_max)) => {
                    let base_type = range_to_primitive(min_max.0, min_max.1, Primitive::Float);
                    types.register_type_alias(
                        type_name.clone(),
                        AliasInfo::new_from_metadata(base_type.tag_if(outer_tag), rule_metadata),
                    );
                }
                _ => {
                    register_fixed_singleton(
                        types,
                        parent_visitor,
                        type_name.clone(),
                        RustType::from(fallback_type),
                        outer_tag,
                        Some(&rule_metadata),
                        generic_params.as_deref(),
                        cli,
                        false,
                    );
                }
            }
        }
        Type2::ParenthesizedType { pt, .. } => {
            // The cddl parser keeps a parenthesized single type as a ParenthesizedType. Unwrap it here
            // so `foo = (uint)` behaves exactly like `foo = uint` (rust_type_from_type2 already unwraps
            // the same way for non-rule positions).
            match pt.type_choices.len() {
                1 => parse_type(
                    types,
                    parent_visitor,
                    type_name,
                    pt.type_choices.first().unwrap(),
                    outer_tag,
                    generic_params,
                    // same rule: carry the outer rule's DSL inward through the parens
                    &rule_metadata,
                    cli,
                ),
                _ => parse_type_choices(
                    types,
                    parent_visitor,
                    type_name,
                    &pt.type_choices,
                    outer_tag,
                    generic_params,
                    cli,
                ),
            }
        }
        x => {
            // Unsupported `type2` as a rule body (a bare major-type constraint `#N.M`, a `~name`
            // unwrap, a `&group` / `&( ... )` choice-from-group, the `any` type `#`, …). None has
            // a storable representation at the rule level — reject gracefully, naming the rule by
            // its SOURCE spelling and the offending construct (with an honest hint where one
            // exists), instead of panicking. `finalize` drains the recorded rejection into a
            // graceful `Err` before any generation runs.
            let source_name = types
                .source_rule_name(type_name)
                .map(str::to_owned)
                .unwrap_or_else(|| type_name.to_string());
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
    }
}

// TODO: Also generates individual choices if required, ie for a / [foo] / c would generate Foos
pub fn create_variants_from_type_choices(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_choices: &[TypeChoice],
    // The owning rule, for the duplicate-arm diagnostics. `None` at the member-position site
    // (`rust_type_from_type` builds an ANONYMOUS choice whose name is derived from the arms), where
    // no rule name exists to blame.
    owner: Option<&RustIdent>,
    cli: &Cli,
) -> Vec<EnumVariant> {
    let owner_desc = match owner {
        Some(name) => format!("rule `{name}`"),
        None => "an inline type choice".to_owned(),
    };
    // `owner_desc` with the rule named as the AUTHOR spelled it, which is what a REJECTION quotes
    // back (the warnings above print the rust ident instead, and the unnameable-variant rejection
    // below re-derives this same string for the same reason).
    let source_owner_desc = match owner {
        Some(name) => format!("rule `{}`", source_rule_name_of(types, name)),
        None => "an inline type choice".to_owned(),
    };
    let mut variant_names_used = BTreeSet::<String>::new();
    // Reserve every explicit emitted name BEFORE walking arms. An explicit `@name` is public API,
    // so it must keep that spelling even if a colliding generator-derived arm appears first; the
    // derived arm is the one that takes `2`. This is the type-choice equivalent of the group-choice
    // pre-reservation in `settle_arm_variant_name`.
    //
    // The same pre-pass sees two explicit names whose source spellings camel-case to one Rust
    // variant (`my_arm` / `myArm`) in either a named or inline choice. The latter has no source rule
    // or enum name to cite, but it still has arms and a generated variant, so it receives the
    // role-generic diagnostic rather than keeping an invalid repeated variant.
    let mut explicit_seen = BTreeMap::<String, (usize, String)>::new();
    for (arm_idx, choice) in type_choices.iter().enumerate() {
        let metadata = merge_metadata(
            &RuleMetadata::from(choice.type1.comments_after_type.as_ref()),
            &RuleMetadata::from(choice.comments_after_type.as_ref()),
        );
        if let Some(source_name) = metadata.name {
            let emitted_name = convert_to_camel_case(&source_name);
            if let Some((first_ordinal, first_source_name)) = explicit_seen.get(&emitted_name) {
                reject_type_choice_arm_variant_name_collision(
                    types,
                    owner,
                    *first_ordinal,
                    first_source_name,
                    arm_idx + 1,
                    &source_name,
                    &emitted_name,
                );
            } else {
                explicit_seen.insert(emitted_name.clone(), (arm_idx + 1, source_name));
                variant_names_used.insert(emitted_name);
            }
        }
    }
    let mut variants: Vec<EnumVariant> = Vec::new();
    // What each kept variant was built from, plus the 1-based SOURCE arm ordinal it came from — the
    // dedup key and what the diagnostics name. Parallel to `variants` (an `EnumVariant` built here
    // always carries a `RustType`, but reading it back through `rust_type()` would panic on the
    // inlined flavor other builders produce, so the types are kept beside it instead).
    let mut kept: Vec<(RustType, usize)> = Vec::new();
    for (arm_idx, choice) in type_choices.iter().enumerate() {
        let rejections_before = types.rejection_count();
        let mut rust_type = rust_type_from_type1(types, parent_visitor, &choice.type1, cli);
        // A PLAIN GROUP arm (`u = kv / tstr`, `x: kv / tstr`). This is the seam every NON-collapsing
        // arm routes through — rule position and member position, two arms or twenty — so one check
        // here covers all of them; the `T / null` collapse is the only fork that bypasses it, and
        // both of ITS branches carry the same guard. Swapping in the inert `Fixed(Null)` placeholder
        // is what the `rejected` read below already expects of a refused arm, and it is what keeps
        // the arm out of the dedup keys and away from the `rust_struct` unwrap in `cbor_types` that
        // this shape used to abort on.
        if reject_plain_group_type_choice_arm(types, &rust_type, &source_owner_desc) {
            rust_type = ConceptualRustType::Fixed(FixedValue::Null).into();
        }
        // An arm the walk REJECTED carries the inert `Fixed(Null)` placeholder every graceful
        // rejection returns, not the type it spelled — so it is neither a dedup candidate nor a
        // dedup key (`[int] / [tstr]`, two rejected anonymous groups, would otherwise read as one
        // arm twice and get a diagnostic that says something false about a spec that is already on
        // its way to a graceful `Err`).
        let rejected = types.rejection_count() > rejections_before;
        // The cddl parser attaches a type-choice element's trailing comment to
        // TypeChoice.comments_after_type, not Type1.comments_after_type, so merge both — otherwise
        // @name/@doc on a variant is silently dropped. Mirrors parse_type's merge for single types.
        let rule_metadata = merge_metadata(
            &RuleMetadata::from(choice.type1.comments_after_type.as_ref()),
            &RuleMetadata::from(choice.comments_after_type.as_ref()),
        );
        // Two arms that build the SAME `RustType` are one arm on the wire: the dispatch tries arms in
        // order and always first-matches the earlier one, so every later twin mints a variant no
        // decode can ever produce (`c = tstr / tstr` minted `C::Text` + an undecodable `C::Text2`,
        // and `--emit-tests` then asserted a round-trip identity the wire cannot carry). Drop the
        // twin — loudly, never silently, because dropping an arm changes the generated API.
        //
        // An explicitly `@name`d twin is KEPT: naming a variant is a deliberate API request, and the
        // emitted round-trip stays honest about it via the first-match assertion (`emit_tests.rs`'s
        // `choice_roundtrip`), which asserts wire fidelity rather than variant identity when the
        // decoder first-matches an earlier arm. It is still announced, since "constructible but
        // never decoded" is not what the spelling suggests.
        //
        // A BARE `any` arm is never collapsed, and not as a special case for its own sake: a bare
        // `any` accepts every CBOR item, so it can only ever be the LAST arm and `parse_type_choices`
        // rejects it anywhere else. Two `any` arms therefore always put one in a non-last position —
        // a spec error the tool already refuses loudly — and collapsing them would ERASE that
        // refusal, leaving a one-armed `any` enum whose all-8-major-types span silently selects the
        // wrong deserializer strategy (the `debug_assert!` on the backtracking form in
        // `generation/enums.rs`). Dedup must never be able to delete a rejection. A TAGGED `any`
        // (`#6.5(any)`) is not a catch-all and collapses normally.
        let bare_any = rust_type.encodings.is_empty()
            && matches!(
                rust_type.conceptual_type.resolve_alias_shallow(),
                ConceptualRustType::Any
            );
        let dup_of = (!rejected && !bare_any)
            .then(|| {
                kept.iter()
                    .find(|(ty, _)| *ty == rust_type)
                    .map(|(_, o)| *o)
            })
            .flatten();
        let base_name = match &rule_metadata {
            RuleMetadata {
                name: Some(name), ..
            } => convert_to_camel_case(name),
            // Only the DERIVED spelling is checked for identifier validity: an explicit `@name` is
            // the author's own, and is the remedy the rejection points at.
            _ => {
                let minted = rust_type.for_variant().to_string();
                if !is_spellable_variant_name(&minted) {
                    // The rejection names the rule as the AUTHOR spelled it (`owner_desc` above
                    // carries the rust ident, which the arm-dedup warnings already print).
                    types.record_rejection_once_at(
                        choice,
                        "unnameable-type-choice-arm",
                        unnameable_arm_variant_name_rejection(
                            &source_owner_desc,
                            &choice.type1.type2.to_string(),
                            &minted,
                            "a type-choice arm's naming slot is its own trailing \
                             comment (`<arm> ; @name <new_name>`)",
                        ),
                    );
                }
                minted
            }
        };
        if let Some(dup_ordinal) = dup_of {
            let arm_ordinal = arm_idx + 1;
            if rule_metadata.name.is_none() {
                if types.claim_diagnostic_node(choice, "duplicate-type-choice-arm-warning") {
                    crate::warn!(
                        "Dropping arm {arm_ordinal} of {owner_desc}: it has the same representation as arm {dup_ordinal}, so the decoder first-matches arm {dup_ordinal} and the variant it would have minted (`{base_name}`) could never be decoded. Write `; @name <Name>` on it to keep it anyway."
                    );
                }
                continue;
            }
            if types.claim_diagnostic_node(choice, "duplicate-named-type-choice-arm-warning") {
                crate::warn!(
                    "Arm {arm_ordinal} of {owner_desc} (`@name {base_name}`) has the same representation as arm {dup_ordinal}: the variant is kept because it is explicitly named, but the decoder first-matches arm {dup_ordinal}, so nothing on the wire ever decodes to it."
                );
            }
        }
        // Explicit names were reserved before this arm walk and must be emitted verbatim. Only a
        // generator-derived name may be disambiguated with a numeric suffix.
        let variant_name = if rule_metadata.name.is_some() {
            base_name
        } else {
            settle_derived_type_choice_variant_name(&mut variant_names_used, base_name)
        };
        variants.push(EnumVariant::new(
            VariantIdent::new_custom(variant_name),
            rust_type.clone(),
            false,
            rule_metadata.comment.clone(),
        ));
        if !rejected {
            kept.push((rust_type, arm_idx + 1));
        }
    }
    variants
}

/// Possible special cases for groups that can be handled to generate much nicer code
/// instead of treating all groups as structs.
// internal, produced once per group during detection and matched immediately (never stored in
// bulk), so the inter-variant size gap doesn't matter; boxing a `RustType` field here would only
// obscure the arms.
#[allow(clippy::large_enum_variant)]
enum GroupParsingType {
    /// Fields are the same e.g. field: [* uint]. The second field is the occurrence-count bounds
    /// (`+` / `n*m`) — a LENGTH constraint belonging to the enclosing array type, kept separate
    /// from the element so it can never be misread as an element VALUE bound.
    HomogenousArray(RustType, Option<(Option<i128>, Option<i128>)>),
    /// Pairs are the same e.g. field:{ *text => uint }. The third field is the occurrence-count
    /// bounds (a cardinality constraint on the table itself). `None` is the unbounded `*` table;
    /// `+` / `1*` retains `NonEmptyMap` and every other representable window uses `BoundedMap`.
    HomogenousMap(RustType, RustType, Option<(Option<i128>, Option<i128>)>),
    /// Fields are different - needs new struct created e.g. field: [a: uint, b: bstr]
    /// This case covers both maps and arrays
    Heterogenous,
    /// Special case for single basic group e.g. field: [basic_group], field: {basic_group}
    /// The tuple type will already have the basic override set so can be directly used
    /// to generate (de)serialiation codegen.
    WrappedBasicGroup(RustType),
}

/// `BoundedVec` and `BoundedMap` carry occurrence endpoints as `u64` const arguments. Reject a
/// parser value that cannot fit that target-independent carrier before later codegen reaches a
/// narrowing conversion (where an `expect` would turn malformed input into a panic).
fn reject_out_of_range_occurrence_bounds(
    types: &mut IntermediateTypes,
    bounds: Option<(Option<i128>, Option<i128>)>,
) {
    for bound in bounds
        .into_iter()
        .flat_map(|(lower, upper)| [lower, upper])
        .flatten()
    {
        if bound > i128::from(u64::MAX) {
            types.record_rejection(format!("Occurrence bound out of range: {bound}"));
        }
    }
}

/// Whether a single-choice inline group carrying this occurrence marker may be spliced into the
/// parent entry list (pure grouping) rather than kept unflattened for downstream rejection.
///
/// Splicing DISCARDS the marker, narrowing the group to exactly-once. That is only sound when the
/// marker already means exactly-once — `None` or `1*1` (any representation) — OR, on the MAP side,
/// when the lower bound is ≥ 1: under unique map keys `+` / `n*m` collapse to exactly-one, so a
/// mandatory field is the honored semantics (the f18d764 boundary). Every zero-permitting marker
/// (`*`, `?`, `0*n`) and every array marker admitting 2+ reps (`+`, `2*5`) is kept unflattened so
/// the caller can reject it instead of silently generating a decoder that rejects valid CBOR.
///
/// SECOND CONSUMER, asking the identical question:
/// `reject_occurrence_on_single_entry_arm`. A single-entry group-choice arm has nowhere to put a
/// repetition count either — the entry's TYPE goes straight into the enum variant, and a variant
/// holds exactly one value — so "is dropping this marker sound?" is the same question there, and
/// it asks it HERE rather than restating the boundary. One predicate is what stops the two seams
/// from disagreeing about `{ x: uint // + kv }`: honored-as-mandatory on both, because a second
/// repetition would duplicate `kv`'s fixed keys.
fn inline_group_occurrence_flattens(occur: Option<&Occurrence>, rep: Representation) -> bool {
    match occur.map(|o| &o.occur) {
        // no marker, or an explicit exactly-once bound: splicing preserves the semantics.
        None
        | Some(Occur::Exact {
            lower: Some(1),
            upper: Some(1),
            ..
        }) => true,
        // MAP side only: a lower bound ≥ 1 collapses to exactly-one under unique map keys, so
        // dropping the marker (a mandatory field) is the honored semantics, not narrowing.
        Some(Occur::OneOrMore { .. }) => rep == Representation::Map,
        Some(Occur::Exact { lower: Some(l), .. }) => rep == Representation::Map && *l >= 1,
        // zero-permitting (`*`, `?`, `0*n`) or 2+-admitting array markers: keep unflattened.
        Some(_) => false,
    }
}

/// Flatten single-choice `GroupEntry::InlineGroup`s into the parent entry list.
///
/// A parenthesized group in entry position — `[(a, b)]` — is pure grouping, semantically `[a, b]`
/// the cddl parser represents it as a `GroupEntry::InlineGroup` (which the downstream codegen has no support for)
/// so we splice single-choice inline groups in before the struct/array/map dispatch.
///
/// The inline group's OWN occurrence marker (`[* (a, b)]`) is honored: splicing drops it, so we
/// only splice when dropping it is sound (see `inline_group_occurrence_flattens`). A marker that
/// would be narrowed away is kept unflattened so `parse_group_type` / `parse_record_from_group_choice`
/// can reject it gracefully rather than silently emit a wrong decoder.
///
/// Multi-choice inline groups are left as-is (unsupported).
/// A no-op for entries with no inline groups, so other output is unchanged.
fn flatten_group_entries<'a>(
    entries: &'a [(GroupEntry<'a>, OptionalComma<'a>)],
    rep: Representation,
) -> Vec<&'a (GroupEntry<'a>, OptionalComma<'a>)> {
    let mut out = Vec::new();
    for entry in entries {
        match &entry.0 {
            GroupEntry::InlineGroup { occur, group, .. }
                if group.group_choices.len() == 1
                    && inline_group_occurrence_flattens(occur.as_ref(), rep) =>
            {
                out.extend(flatten_group_entries(
                    &group.group_choices[0].group_entries,
                    rep,
                ));
            }
            _ => out.push(entry),
        }
    }
    out
}

/// Parses which type of group it is for various common special cases to handle
///
/// How a rejection message names the composite it complains about: the enclosing rule by its
/// SOURCE spelling when there is one (the user is looking at their CDDL, not our camel-cased
/// output), and `anonymous` for a nested composite that has no rule of its own.
fn rejection_site(
    types: &IntermediateTypes,
    rule_name: Option<&RustIdent>,
    anonymous: &str,
) -> String {
    match rule_name {
        Some(name) => format!(
            "rule `{}`",
            types
                .source_rule_name(name)
                .map(str::to_owned)
                .unwrap_or_else(|| name.to_string())
        ),
        None => anonymous.to_owned(),
    }
}

/// A TYPE-choice arm whose RESOLVED type is a plain group (`kv = (a: uint, b: uint)`, then
/// `x: kv / null` or `u = kv / tstr`). Returns whether the arm was refused, so each caller can put
/// its own inert placeholder in the arm's slot.
///
/// This is a refusal that is also the durable contract, not a support branch deferred. A type
/// choice denotes exactly ONE data item — the decoder's whole job at a choice is to tell the arms
/// apart from each other on the wire — while a plain group has no type of its own and can only be
/// SPLICED, writing its members flat into the enclosing collection. There is no one-item form of a
/// splice, so there is nothing an arm could hold and nothing for the dispatch to tell apart; the
/// array framing the message names is not a workaround for a missing feature but the shape the spec
/// author has to choose in order to mean anything here. What the shape reached instead was two
/// panics and one silently broken crate: the arm never stamps the group's rep, so the group is never
/// materialized, and the walks that then look it up abort on the `rust_struct` expect/unwrap in
/// `intermediate/rust_type.rs` (or, under `--wasm`, earlier on the plain-group registry assert in
/// `intermediate/mod.rs`) — except at RULE position under the `/ null` collapse, which exited 0
/// emitting `pub type U = Option<Kv>;` with `Kv` defined nowhere in the crate.
///
/// Guarded on `is_basic` over the RESOLVED arm type — the same predicate and the same
/// resolved-type placement as the record-field and rest-tail twins — so the bare, the ALIAS
/// (`kv_alias / null`) and the TAGGED (`#6.10(kv) / null`) spellings land on ONE message. The
/// array-WRAPPED forms keep their own (supported) verdicts: `w = [kv]` is a Record, not a plain
/// group, and an inline `[kv]` arm carries `basic_override`, so neither is `is_basic`.
fn reject_plain_group_type_choice_arm(
    types: &mut IntermediateTypes,
    arm_type: &RustType,
    site: &str,
) -> bool {
    if !arm_type.is_basic(types) {
        return false;
    }
    let ConceptualRustType::Rust(group_ident) = arm_type.conceptual_type.resolve_alias_shallow()
    else {
        return false;
    };
    let group_name = types
        .source_rule_name(group_ident)
        .map(str::to_owned)
        .unwrap_or_else(|| group_ident.to_string());
    types.record_rejection(format!(
        "{site}: a type-choice arm cannot be the plain group `{group_name}` — a plain group has no \
         type of its own, it splices its members flat into the enclosing array or map, while a \
         choice arm denotes exactly ONE data item that the decoder has to tell apart from the other \
         arms. A splice has no one-item form, so there is nothing for the arm to hold. Give the \
         group its own array framing and put THAT in the arm, which makes the arm exactly one data \
         item: `w = [{group_name}]`, then `w` in place of `{group_name}` here (`x: w / null`, `u = \
         w / null`). (A tag belongs on the framed reference — `#6.10(w)` — not on the group. \
         Splicing the group with no choice around it is supported as it stands: as a mandatory \
         array member, `t = [ c: uint, {group_name} ]`, as a keyless GROUP-choice arm, `t = [ x: \
         uint // {group_name} ]`, and as a plain alias, `u = {group_name}`.)"
    ));
    true
}

/// The rejection for a table entry whose VALUE domain is a bare fixed value (`{ * uint => 5 }`).
/// Shared by the plain and the parenthesized (`{ * (uint => 5) }`) table arms so the two spellings
/// of the same shape can't be told apart by their message.
fn record_fixed_table_value_rejection(
    types: &mut IntermediateTypes,
    site: &str,
    entry_src: &str,
    fixed: &FixedValue,
) {
    let value_desc = fixed.cddl_source_desc();
    types.record_rejection(format!(
        "{site}: the table entry `{entry_src}` has a bare fixed value ({value_desc}) as its VALUE \
         domain, which is unsupported — a fixed value has no type to store per table row, it only \
         has meaning as a single (unstored) member whose value the schema fixes. If a repeated \
         nominal singleton is wanted, name the constant in its own rule (for example `five = \
         {value_desc}`) and use that rule as the VALUE domain (`{{ * uint => five }}`); it preserves \
         the wire constant but gives the generated API a stored singleton wrapper. Widening the \
         value to the CDDL type the constant inhabits (`uint` / `bool` / `tstr` / …) generates, \
         but it no longer constrains the value to {value_desc}, so it is a different spec, not an \
         equivalent one."
    ));
}

/// The source name of the bare plain group a table KEY/VALUE domain resolves to, if it is one.
///
/// Keyed on `RustType::is_basic` — the SAME predicate `generate_serialize` uses to pick the
/// `serialize_as_embedded_group` (member-splicing) emission over a real `serialize`, so this
/// refuses exactly the domains that would splice and nothing else. In particular an array-WRAPPED
/// group (`[coords]`, inline or as a named rule) carries `basic_override`, serializes as one
/// nested array item, and is therefore the remedy rather than another instance of the defect.
/// Aliases resolve through, so `c2 = coords` is caught alongside a direct reference.
fn plain_group_table_domain(types: &IntermediateTypes, ty: &RustType) -> Option<String> {
    match ty.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Rust(ident) if ty.is_basic(types) => {
            Some(source_rule_name_of(types, ident))
        }
        _ => None,
    }
}

/// The innermost TYPENAME a table domain's source spelling references, if it has one — the token
/// the array-wrapping remedy has to wrap, which is not always the whole domain expression. A tag
/// belongs OUTSIDE the array: the array is the group's single-item carrier and the tag wraps that
/// carrier, so `#6.5(coords)` becomes `#6.5([coords])`, never `[#6.5(coords)]` (both generate, but
/// only the first keeps the tag on the item the spec tagged).
fn innermost_typename_src(t2: &Type2) -> Option<String> {
    match t2 {
        Type2::Typename { ident, .. } => Some(ident.to_string()),
        Type2::TaggedData { t, .. } => match t.type_choices.as_slice() {
            [choice] => innermost_typename_src(&choice.type1.type2),
            _ => None,
        },
        _ => None,
    }
}

/// A table domain's source spelling with its group reference wrapped in an array — the remedy the
/// rejection below prints back. Falls back to wrapping the whole expression when no single
/// typename is identifiable, which is never worse than the spelling the author already wrote.
fn array_wrapped_domain_src(src: &str, t2: &Type2) -> String {
    match innermost_typename_src(t2) {
        // The typename occurs once in its own domain's rendering, and a tag's only other token is
        // digits, so the first-occurrence replacement cannot land on anything else.
        Some(name) => src.replacen(&name, &format!("[{name}]"), 1),
        None => format!("[{src}]"),
    }
}

/// The single `Type2` a table domain's `Type` carries, when it is not itself a choice.
fn single_type2<'a>(t: &'a Type<'a>) -> Option<&'a Type2<'a>> {
    match t.type_choices.as_slice() {
        [choice] => Some(&choice.type1.type2),
        _ => None,
    }
}

/// The rejection for a table entry whose KEY or VALUE domain is a bare plain group
/// (`coords = (uint, uint)`, `{ * uint => coords }`).
///
/// A CBOR map entry holds EXACTLY ONE item in each of its two slots, and a keyless group has no
/// single-item form — the only thing a serializer can do with it is splice its members in flat,
/// which writes N items where the map's own header promised one. That is what this used to emit:
/// `{ * uint => coords }` with one entry wrote `a2 01 07 08 02 09 0a` inside its holder, which any
/// other CBOR implementation reads as a 2-entry map `{1: 7, 8: 2}` plus trailing bytes — wire only
/// this crate's own mirrored decoder could read back. Refused at BOTH spellings (the named rule and
/// the inline `[{ * uint => coords }]`, which reached a raw `unwrap` at generation instead), since
/// a graceful refusal has to replace a broken acceptance rather than sit beside it.
///
/// The remedy is not a new wire: `[coords]` — an ARRAY rule or the inline array spelling — already
/// gives the group the single nested item the slot needs, with real nested-array semantics. Giving
/// the bare spelling a wire of its own would only mint a second spelling of that.
///
/// Shared by the plain and the parenthesized (`{ * (uint => coords) }`) table arms, and by the key
/// and value roles, so no spelling of the same shape can be told apart by its message.
fn record_plain_group_table_domain_rejection(
    types: &mut IntermediateTypes,
    site: &str,
    entry_src: &str,
    role: &str,
    group_name: &str,
    remedy_entry: &str,
) {
    types.record_rejection(format!(
        "{site}: the table entry `{entry_src}` uses the bare plain group `{group_name}` as its \
         {role} domain, which is unsupported — a CBOR map entry holds exactly one item in each \
         slot, and a keyless group has no single-item form, so it could only be spliced in with \
         its members written flat. That contradicts the map's own entry count and emits bytes no \
         other CBOR implementation reads back as the spec says. Wrap the group in an array, which \
         gives the slot the one item it needs and has real nested-array semantics: \
         `{{ * {remedy_entry} }}`."
    ));
}

/// The rejection for an open struct-map REST ROW (`{ c: uint, * k => v }`) whose key or value slot
/// is a plain group — the fixed-prefix sibling of `record_plain_group_table_domain_rejection`,
/// refused for exactly the same reason and kept as its own message because the shapes are told
/// apart by their fixed prefix, not by their problem (a table entry is not a rest row, and an
/// author reading either message has to recognize the line they wrote).
///
/// A CBOR map entry holds exactly one item in each of its two slots and a keyless group has no
/// single-item form, so the row has no wire: every spelling of it aborted on every profile before
/// producing usable output — on the plain-group registry assert in `is_enum` (reached from
/// `finalize`'s wrapper-name collision walk) whenever wasm surfaces are enabled, and under
/// `--wasm=false` later, on raw generation-time `Option::unwrap()`s (`encoding_var_is_copy` on the
/// default profile; the preserve emitter's sidecar lookup under `--preserve-encodings`).
///
/// The remedy is the same array framing the table twin names — the array is the group's single-item
/// carrier — and it is verified to generate and build on the default, `--preserve-encodings` and
/// `--wasm` profiles for both slots and for the tagged spelling of each. A tag stays OUTSIDE the
/// framing (`#6.10([kv])`, not `[#6.10(kv)]`): the array is the item the map slot holds, and the
/// tag wraps that item, so only that placement keeps the tag on what the spec tagged.
fn record_plain_group_rest_row_domain_rejection(
    types: &mut IntermediateTypes,
    src: &str,
    entry_src: &str,
    role: &str,
    group_name: &str,
    remedy_entry: &str,
) {
    types.record_rejection(format!(
        "rule `{src}`: the open struct-map rest row `{entry_src}` uses the bare plain group \
         `{group_name}` as its {role} domain, which is unsupported — a CBOR map entry holds \
         exactly one item in each slot, and a keyless group has no single-item form, so it could \
         only be spliced in with its members written flat. That contradicts the map's own entry \
         count and emits bytes no other CBOR implementation reads back as the spec says. Wrap the \
         group in an array, which gives the slot the one item it needs and has real nested-array \
         semantics: `* {remedy_entry}` in place of this row. (A tag on the slot belongs OUTSIDE \
         the framing, on the framed reference — `#6.10([{group_name}])`, not \
         `[#6.10({group_name})]`.)"
    ));
}

/// The rejection for a KEYED map-record member whose type is a plain group
/// (`kv = (a: uint, b: uint)`, `t = { c: kv }`) — the struct-map twin of
/// `record_plain_group_table_domain_rejection`, refused for the same reason.
///
/// The key claims one map entry, and that entry's VALUE slot holds exactly one item. A keyless
/// group has no single-item form, so the only emission available is `serialize_as_embedded_group`,
/// which splices the group's members in flat: `t = { c: kv }` wrote
/// `write_map(Len(1))`, the key `"c"`, then Kv's four items — five items after a one-entry map
/// header, which an interoperating decoder reads as `{'c': 'a'}` plus trailing bytes. Every keyed
/// spelling of the shape reaches that same splice (the bare member, a tag around it, `?` on it, an
/// alias to the group, and a group-choice arm carrying it), so the guard is on the member's
/// resolved type rather than on any one spelling.
///
/// Deserialize was never generated for it either — `map_record_deser_refusals` declined the whole
/// record, because a map's members may arrive in ANY order (`foo = {a, b, bar}, bar = (c, d)`
/// admits `{a, d, c, b}`) and `deserialize_as_embedded_group` reads a fixed sequence. That left a
/// serialize-only crate emitting bytes only this crate could interpret, which is worse than a
/// refusal: exit 0, a crate that compiles, and no way for the author to learn the wire is wrong.
///
/// The remedy is a NAMED array rule (`w = [kv]`, then `c: w`), which gives the slot the single
/// nested item it needs with real nested-array semantics. The INLINE spelling (`c: [kv]`) is NOT
/// the remedy — it collapses to the group ident carrying `basic_override`, and stamping the outer
/// Map rep onto the already-Array group is refused separately by
/// `set_rep_if_plain_group`'s conflicting-representations arm.
fn record_plain_group_map_member_rejection(
    types: &mut IntermediateTypes,
    site: &str,
    field_name: &str,
    group_name: &str,
) {
    types.record_rejection(format!(
        "{site}: map field `{field_name}` uses the plain group `{group_name}` as its type, which \
         is unsupported — a CBOR map entry holds exactly one item in its value slot, and a keyless \
         group has no single-item form, so it could only be spliced in with its members written \
         flat. That contradicts the map's own entry count and emits bytes no other CBOR \
         implementation reads back as the spec says. Give the array framing its own rule and \
         reference that, which gives the slot the one item it needs and has real nested-array \
         semantics: `w = [{group_name}]`, then `{field_name}: w`. (Writing the array inline \
         (`{field_name}: [{group_name}]`) is not the remedy — it is refused separately, as a \
         conflicting representation on `{group_name}` itself.)"
    ));
}

/// `rule_name` is the enclosing rule when there is one (the named-rule path through
/// `parse_group_choice`); `None` for anonymous nested composites (`rust_type_from_type2`'s
/// `Type2::Array` / `Type2::Map` arms), where rejection messages describe the entry instead of
/// citing a rule.
fn parse_group_type<'a>(
    types: &mut IntermediateTypes,
    parent_visitor: &'a ParentVisitor,
    group_choice: &'a GroupChoice<'a>,
    rep: Representation,
    rule_name: Option<&RustIdent>,
    cli: &Cli,
) -> GroupParsingType {
    let entries = flatten_group_entries(&group_choice.group_entries, rep);
    match rep {
        Representation::Array => {
            // An unflattened `InlineGroup` here is a parenthesized group carrying an occurrence
            // marker that would be silently narrowed (`[* (int, tstr)]`), or a multi-choice group.
            // Fall through to `Heterogenous` so `parse_record_from_group_choice` rejects it
            // gracefully rather than panicking on the unsupported element.
            if entries.len() == 1 && !matches!(entries[0].0, GroupEntry::InlineGroup { .. }) {
                let (entry, _has_comma) = entries[0];
                let (elem_type, occur) = match entry {
                    GroupEntry::ValueMemberKey { ge, .. } => (
                        rust_type(types, parent_visitor, &ge.entry_type, cli),
                        &ge.occur,
                    ),
                    GroupEntry::TypeGroupname { ge, .. } => (
                        // Route through the shared helper so a generic instantiation used as a
                        // homogeneous array element (`[* pair<uint, tstr>]`) registers its generic
                        // instance instead of dropping `ge.generic_args` and emitting a reference to
                        // the never-emitted bare generic base. `generic_args == None` stays
                        // byte-identical to the previous `types.new_type(...)` call.
                        generic_instance_or_new_type(
                            types,
                            parent_visitor,
                            CDDLIdent::new(ge.name.to_string()),
                            &ge.generic_args,
                            cli,
                        ),
                        &ge.occur,
                    ),
                    GroupEntry::InlineGroup { .. } => unreachable!("guarded above"),
                };
                let bounds = occur.as_ref().map(|o| match o.occur {
                    Occur::ZeroOrMore { .. } => (None, None),
                    Occur::Exact { lower, upper, .. } => (
                        lower.filter(|l| *l != 0).map(|i| i as i128),
                        upper.map(|i| i as i128),
                    ),
                    Occur::Optional { .. } => (None, Some(1)),
                    Occur::OneOrMore { .. } => (Some(1), None),
                });
                reject_out_of_range_occurrence_bounds(types, bounds);
                // `[* 5]` / `[+ 5]` / `[? 5]` / `[2*5 5]`: a bare fixed value as the target of a
                // COUNT-PERMITTING occurrence. The homogeneous-array path stores its elements in a
                // `Vec<T>`, and a `Fixed` has no `T` — it exists only as an unstored member whose
                // value the schema pins, so there is nothing to store per repetition and
                // `for_rust_member` panics on it during generation. Reject it here, at the parse
                // walk, so `finalize` turns it into a graceful `Err` before generation runs.
                //
                // Recording the rejection IS the deliverable: the tempting alternative — falling
                // through to the record path — would silently drop the marker and emit a
                // one-element record, so the generated decoder would accept a single-element array
                // for a `0..N` spec (a certified over-acceptance). The `HomogenousArray` returns
                // below are therefore left untouched: parse behaviour stays byte-identical to
                // before, and the `Fixed` element is inert because generation never runs once a
                // rejection is recorded. The EXACTLY-ONCE placement (`[5]`, `[v: true]`) is
                // supported and is deliberately outside this guard — it lands on the record path
                // where a fixed member is stored nowhere but checked on the wire.
                if !matches!(bounds, None | Some((Some(1), Some(1))))
                    && let ConceptualRustType::Fixed(fixed) =
                        elem_type.conceptual_type.resolve_alias_shallow()
                {
                    let value_desc = fixed.cddl_source_desc();
                    let elem_src = group_entry_source_desc_for_diagnostic(entry);
                    let site = rejection_site(types, rule_name, "inline array");
                    types.record_rejection(format!(
                        "{site}: the array element `{elem_src}` is a bare fixed value \
                         ({value_desc}) under a count-permitting occurrence marker (`*` / `+` / \
                         `?` / `n*m`), which is unsupported — a fixed value has no element type to \
                         store per repetition, it only has meaning as a single (unstored) member \
                         whose value the schema fixes. If a repeated nominal singleton is wanted, \
                         name the constant in its own rule and use that rule as the element type; \
                         it preserves the wire constant but gives the generated API a stored \
                         singleton wrapper. If exactly one element is meant, drop the marker \
                         (`[{elem_src}]`) — that placement IS supported. Widening the element to \
                         the CDDL type the constant inhabits (`uint` / `bool` / `tstr` / …) \
                         generates, but it no longer constrains the element to {value_desc}, so \
                         it is a different spec, not an equivalent one."
                    ));
                }
                match bounds {
                    // no bounds
                    Some((None, None)) => {
                        return GroupParsingType::HomogenousArray(elem_type, None);
                    }
                    None | Some((Some(1), Some(1))) => {
                        // if the only element is a basic group we don't need to create a new group but can just
                        // change how it is (de)serialized
                        if let ConceptualRustType::Rust(elem_ident) =
                            elem_type.conceptual_type.resolve_alias_shallow()
                            && types.is_plain_group(elem_ident)
                        {
                            return GroupParsingType::WrappedBasicGroup(elem_type.not_basic());
                        }
                        // fall-through generic case. this is a general 1-element struct that needs creating
                    }
                    Some(bounds) => {
                        return GroupParsingType::HomogenousArray(elem_type, Some(bounds));
                    }
                }
            }
        }
        Representation::Map => {
            // Here we test if this is a struct vs a table.
            // struct: { x: int, y: int }, etc
            // table: { * int => tstr }, etc
            // A literal-key arrow entry (`{ 1 => uint }`, `{ "a" => uint }`) is NOT a table: per RFC
            // 8610 a fixed-value key `k => v` is the same wire entry as the colon spelling `k: v`, so
            // it is a 1-field struct. Table detection therefore requires a NON-fixed key type; a fixed
            // key falls through to `parse_record_from_group_choice` (where f49d862's classification
            // generates uint/text and gracefully rejects nint/float/bool), avoiding a `Fixed`-domain
            // table that panics in `for_rust_member`.
            // this assumes that all maps representing tables are homogenous
            // and contain no other fields. I am not sure if this is a guarantee in
            // cbor but I would hope that the cddl specs we are using follow this.
            if entries.len() == 1 {
                match &entries[0].0 {
                    GroupEntry::ValueMemberKey { ge, .. } => {
                        match &ge.member_key {
                            Some(MemberKey::Type1 { t1, .. }) => {
                                // TODO: Do we need to handle cuts for what we're doing?
                                // Does the range control operator matter?
                                let key_type = rust_type_from_type1(types, parent_visitor, t1, cli);
                                // Resolve through aliases so an aliased literal (`one = 1`) also
                                // diverts to the record path instead of table-detecting a Fixed domain.
                                if matches!(
                                    key_type.conceptual_type.resolve_alias_shallow(),
                                    ConceptualRustType::Fixed(_)
                                ) {
                                    // fixed-value key: fall through to the 1-element struct path
                                    // (identical to the `MemberKey::Value` arm below).
                                } else {
                                    // A NON-fixed arrow map entry's occurrence marker determines the
                                    // table cardinality. Every window is preserved: omitted means
                                    // exact-once, `*` is loose, `+` uses NonEmptyMap, and the other
                                    // finite/one-sided windows use BoundedMap. NOT applied in the
                                    // InlineGroup table arm below: there
                                    // the semantic occurrence is the inline group's own marker
                                    // (`{ * (k => v) }`), and the inner entry's missing occur means
                                    // nothing. Fixed keys above keep falling through — `{ 1 => uint }`
                                    // is RFC-equal to the colon spelling and routes to the record path.
                                    //
                                    //   (none)   — RFC 8610 exactly-once; BoundedMap<_,_,1,1>
                                    //   `*`/`0*` — unbounded 0..N table (bounds `None`), unchanged
                                    //   `+`/`1*` — non-empty table (`NonEmptyMap`), bounds (Some(1),None)
                                    //   else     — bounded (`?` / `n*m` / `*n` / `n*` / `0*n`): BoundedMap
                                    //
                                    // cite the rule by its SOURCE spelling when we have one (the user
                                    // is looking at their CDDL, not our output); anonymous nested maps
                                    // describe the entry instead.
                                    let site = rejection_site(types, rule_name, "inline map");
                                    let value = &ge.entry_type;
                                    let occ_bounds = ge.occur.as_ref().map(|o| match o.occur {
                                        Occur::ZeroOrMore { .. } => (None, None),
                                        Occur::Exact { lower, upper, .. } => (
                                            lower.filter(|l| *l != 0).map(|i| i as i128),
                                            upper.map(|i| i as i128),
                                        ),
                                        Occur::Optional { .. } => (None, Some(1)),
                                        Occur::OneOrMore { .. } => (Some(1), None),
                                    });
                                    let table_bounds = match occ_bounds {
                                        // RFC 8610 gives an omitted occurrence the exact `1..=1`
                                        // window. Preserve it rather than widening it to `*`.
                                        None => Some((Some(1), Some(1))),
                                        // `*` / `0*`: the unbounded table this crate has always
                                        // generated (bounds carry no min/max).
                                        Some((None, None)) => None,
                                        // `+` / `1*`: the older dedicated non-empty sibling.
                                        Some((Some(1), None)) => Some((Some(1), None)),
                                        // finite, optional, and lower-bounded windows enter the
                                        // type-level BoundedMap door.
                                        Some(bounds) => Some(bounds),
                                    };
                                    reject_out_of_range_occurrence_bounds(types, table_bounds);
                                    // keep parsing on the harmless table path — any rejection above
                                    // surfaces as a graceful Err at `finalize`, and nothing may panic
                                    // in between.
                                    let value_type =
                                        rust_type(types, parent_visitor, &ge.entry_type, cli);
                                    // The map sibling of the array-element fixed guard: a table's
                                    // VALUE domain lands in the `BTreeMap<K, V>`'s `V`, and a
                                    // `Fixed` has no `V` — `{ * uint => 5 }` reaches the same
                                    // `for_rust_member` panic as `[* 5]`. (A fixed KEY is a
                                    // different story and is handled above: per RFC 8610 `1 => v`
                                    // is the same wire entry as `1: v`, so it diverts to the record
                                    // path. A fixed VALUE has no such re-reading — the entry really
                                    // is a table row whose value is pinned.)
                                    if let ConceptualRustType::Fixed(fixed) =
                                        value_type.conceptual_type.resolve_alias_shallow()
                                    {
                                        let fixed = fixed.clone();
                                        record_fixed_table_value_rejection(
                                            types,
                                            &site,
                                            &format!("{t1} => {value}"),
                                            &fixed,
                                        );
                                    }
                                    // A bare plain group in EITHER domain has no single-item CBOR
                                    // form to occupy a map slot — see
                                    // `record_plain_group_table_domain_rejection`. Both roles are
                                    // checked here, so the named-rule and inline consumers of this
                                    // one seam refuse identically.
                                    if let Some(group_name) =
                                        plain_group_table_domain(types, &key_type)
                                    {
                                        record_plain_group_table_domain_rejection(
                                            types,
                                            &site,
                                            &format!("{t1} => {value}"),
                                            "KEY",
                                            &group_name,
                                            &format!(
                                                "{} => {value}",
                                                array_wrapped_domain_src(
                                                    &t1.to_string(),
                                                    &t1.type2,
                                                )
                                            ),
                                        );
                                    }
                                    if let Some(group_name) =
                                        plain_group_table_domain(types, &value_type)
                                    {
                                        record_plain_group_table_domain_rejection(
                                            types,
                                            &site,
                                            &format!("{t1} => {value}"),
                                            "VALUE",
                                            &group_name,
                                            &format!(
                                                "{t1} => {}",
                                                match single_type2(value) {
                                                    Some(t2) => array_wrapped_domain_src(
                                                        &value.to_string(),
                                                        t2,
                                                    ),
                                                    None => format!("[{value}]"),
                                                }
                                            ),
                                        );
                                    }
                                    return GroupParsingType::HomogenousMap(
                                        key_type,
                                        value_type,
                                        table_bounds,
                                    );
                                }
                            }
                            Some(MemberKey::Value { .. }) => {
                                // has a fixed value - this is just a 1-element struct
                            }
                            Some(MemberKey::Bareword { .. }) => {
                                // a bareword key is sugar for the equivalent text-string value key,
                                // so a single bareword-keyed entry is a 1-field struct, not a table
                                // (identical wire shape to the multi-field `{ a: uint, b: text }` form)
                            }
                            None => {
                                // a keyless map entry (e.g. `{ bytes }`) is unsupported by design;
                                // fall through to the Heterogenous path so it funnels into
                                // `parse_record_from_group_choice`'s graceful rejection rather than
                                // panicking here.
                            }
                            Some(MemberKey::NonMemberKey { .. }) => {
                                panic!("unsupported table map key (1): {:?}", ge)
                            }
                        }
                    }
                    // a single keyless group reference (e.g. `{ bytes }` = a `TypeGroupname`) is
                    // unsupported by design; fall through to the Heterogenous path where it is
                    // rejected gracefully. A multi-choice inline group here is out of scope.
                    GroupEntry::TypeGroupname { .. } => {}
                    GroupEntry::InlineGroup { group, .. } => {
                        // `{ * (int => tstr) }` — a parenthesized table. The occurrence-aware
                        // flatten leaves this `*` inline group unspliced (lower bound 0 on the map
                        // side), so it surfaces here. If it wraps exactly one `k => v` table entry,
                        // treat it like the unparenthesized table arm above. Anything else falls
                        // through to `Heterogenous`, where the record path rejects it gracefully
                        // (a multi-choice group, an occurrence-bearing struct group, …) rather than
                        // panicking on an unsupported map key.
                        if group.group_choices.len() == 1 {
                            let inner = flatten_group_entries(
                                &group.group_choices[0].group_entries,
                                Representation::Array,
                            );
                            if inner.len() == 1
                                && let GroupEntry::ValueMemberKey { ge, .. } = &inner[0].0
                                && let Some(MemberKey::Type1 { t1, .. }) = &ge.member_key
                            {
                                let key_type = rust_type_from_type1(types, parent_visitor, t1, cli);
                                // same Fixed-domain guard as the single-entry table arm: a
                                // parenthesized fixed-value key (`{ * (1 => uint) }`) must fall
                                // through to Heterogenous → graceful record-path rejection, not build
                                // a Fixed-domain table that panics in `for_rust_member`.
                                if !matches!(
                                    key_type.conceptual_type.resolve_alias_shallow(),
                                    ConceptualRustType::Fixed(_)
                                ) {
                                    let value_type =
                                        rust_type(types, parent_visitor, &ge.entry_type, cli);
                                    // same fixed-VALUE guard as the single-entry table arm:
                                    // `{ * (uint => 5) }` puts a `Fixed` in the map's value
                                    // position, which has no type to store per row.
                                    if let ConceptualRustType::Fixed(fixed) =
                                        value_type.conceptual_type.resolve_alias_shallow()
                                    {
                                        let fixed = fixed.clone();
                                        let site = rejection_site(types, rule_name, "inline map");
                                        let entry_src = format!("{t1} => {}", ge.entry_type);
                                        record_fixed_table_value_rejection(
                                            types, &site, &entry_src, &fixed,
                                        );
                                    }
                                    // same bare-plain-group domain guard as the single-entry table
                                    // arm, for both roles: `{ * (uint => coords) }` puts a keyless
                                    // group in a map slot that holds exactly one item.
                                    let key_group = plain_group_table_domain(types, &key_type);
                                    let value_group = plain_group_table_domain(types, &value_type);
                                    if key_group.is_some() || value_group.is_some() {
                                        let site = rejection_site(types, rule_name, "inline map");
                                        let value = &ge.entry_type;
                                        let entry_src = format!("{t1} => {value}");
                                        if let Some(group_name) = key_group {
                                            record_plain_group_table_domain_rejection(
                                                types,
                                                &site,
                                                &entry_src,
                                                "KEY",
                                                &group_name,
                                                &format!(
                                                    "{} => {value}",
                                                    array_wrapped_domain_src(
                                                        &t1.to_string(),
                                                        &t1.type2,
                                                    )
                                                ),
                                            );
                                        }
                                        if let Some(group_name) = value_group {
                                            record_plain_group_table_domain_rejection(
                                                types,
                                                &site,
                                                &entry_src,
                                                "VALUE",
                                                &group_name,
                                                &format!(
                                                    "{t1} => {}",
                                                    match single_type2(value) {
                                                        Some(t2) => array_wrapped_domain_src(
                                                            &value.to_string(),
                                                            t2,
                                                        ),
                                                        None => format!("[{value}]"),
                                                    }
                                                ),
                                            );
                                        }
                                    }
                                    // `{ * (k => v) }`: the inline group's own `*` marker is the
                                    // cardinality (unbounded); the inner entry carries no honored
                                    // bound of its own here.
                                    return GroupParsingType::HomogenousMap(
                                        key_type, value_type, None,
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    // must be a heterogenous struct or 1-element fixed struct
    GroupParsingType::Heterogenous
}

// would use rust_type_from_type1 but that requires IntermediateTypes which we shouldn't
fn type2_is_null(t2: &Type2) -> bool {
    match t2 {
        Type2::Typename { ident, .. } => ident.ident == "null" || ident.ident == "nil",
        _ => false,
    }
}

fn type_to_field_name(t: &Type) -> Option<String> {
    let type2_to_field_name = |t2: &Type2| match t2 {
        Type2::Typename { ident, .. } => Some(ident.to_string()),
        Type2::TextValue { value, .. } => Some(value.to_string()),
        Type2::Array { group, .. } => match group.group_choices.len() {
            1 => {
                let entries = &group.group_choices.first().unwrap().group_entries;
                match entries.len() {
                    1 => {
                        match &entries.first().unwrap().0 {
                            // should we do this? here it possibly allows [[foo]] -> fooss
                            GroupEntry::ValueMemberKey { ge, .. } => {
                                Some(format!("{}s", type_to_field_name(&ge.entry_type)?))
                            }
                            GroupEntry::TypeGroupname { ge, .. } => Some(format!("{}s", ge.name)),
                            GroupEntry::InlineGroup { .. } => None,
                        }
                    }
                    // only supports homogenous arrays for now
                    _ => None,
                }
            }
            // no group choice support here
            _ => None,
        },
        // non array/text/identifier types not supported here - value keys are caught earlier anyway
        _ => None,
    };
    match t.type_choices.len() {
        1 => type2_to_field_name(&t.type_choices.first().unwrap().type1.type2),
        2 => {
            // special case for T / null -> maps to Option<T> so field name should be same as just T
            let a = &t.type_choices[0].type1.type2;
            let b = &t.type_choices[1].type1.type2;
            if type2_is_null(a) {
                type2_to_field_name(b)
            } else if type2_is_null(b) {
                type2_to_field_name(a)
            } else {
                // neither are null - we do not support type choices here
                None
            }
        }
        // no type choice support here
        _ => None,
    }
}

fn combine_comments<'a>(a: &'a Option<Comments>, b: &'a Option<Comments>) -> Option<Vec<&'a str>> {
    match (
        a.as_ref().map(|comment| comment.0.clone()),
        b.as_ref().map(|comment| comment.0.clone()),
    ) {
        (Some(a), Some(b)) => Some([a, b].concat()),
        (opt_a, opt_b) => opt_a.or(opt_b),
    }
}

// Attempts to use the style-converted type name as a field name, and if we have already
// generated one, then we simply add numerals starting at 2, 3, 4...
// If you wish to only check if there is an explicitly stated field name,
// then use group_entry_to_raw_field_name()
fn group_entry_to_field_name(
    entry: &GroupEntry,
    index: usize,
    already_generated: &mut BTreeMap<String, u32>,
    optional_comma: &OptionalComma,
) -> String {
    //println!("group_entry_to_field_name() = {:#?}", entry);
    let field_name = convert_to_snake_case(&match entry {
        GroupEntry::ValueMemberKey {
            trailing_comments,
            ge,
            ..
        } => match ge.member_key.as_ref() {
            Some(member_key) => match member_key {
                MemberKey::Value { value, .. } => {
                    let combined_comments =
                        combine_comments(trailing_comments, &optional_comma.trailing_comments);
                    match metadata_from_comments(&combined_comments.unwrap_or_default()) {
                        RuleMetadata {
                            name: Some(name), ..
                        } => name,
                        // a quoted text key `"a":` is sugar for the bareword key `a:` (same wire
                        // key), so it must converge on the bareword field name, not `key_"a"`
                        // (which is invalid Rust). Non-text values keep the `key_{value}` fallback.
                        _ => match value {
                            cddl::token::Value::TEXT(t) => t.to_string(),
                            _ => format!("key_{value}"),
                        },
                    }
                }
                MemberKey::Bareword { ident, .. } => {
                    // Honor a `@name` directive the same way the Value/Type1 arms do; otherwise the
                    // directive is silently dropped on bareword-keyed entries (the same directive-drop
                    // bug class the Type1 arm below fixes for arrow keys).
                    let combined_comments =
                        combine_comments(trailing_comments, &optional_comma.trailing_comments);
                    match metadata_from_comments(&combined_comments.unwrap_or_default()) {
                        RuleMetadata {
                            name: Some(name), ..
                        } => name,
                        _ => ident.to_string(),
                    }
                }
                MemberKey::Type1 { t1, .. } => {
                    // An integer arrow key `0 => x` is the Type1 spelling of the value key `0: x`, so
                    // honor a @name directive the same way the Value arm above does (falling back to
                    // key_{value}); otherwise the directive is silently dropped on arrow-keyed entries.
                    let combined_comments =
                        combine_comments(trailing_comments, &optional_comma.trailing_comments);
                    match metadata_from_comments(&combined_comments.unwrap_or_default()) {
                        RuleMetadata {
                            name: Some(name), ..
                        } => name,
                        _ => match &t1.type2 {
                            Type2::UintValue { value, .. } => format!("key_{value}"),
                            // A quoted-text arrow key `"a" => v` is the Type1 spelling of the value
                            // key `"a": v` / bareword `a:` (same wire key), so it must converge on the
                            // same field name. Nint/float Type1 keys never reach naming — they are
                            // rejected during key classification first — so no cases for them here.
                            Type2::TextValue { value, .. } => value.to_string(),
                            _ => panic!(
                                "Encountered Type1 member key in multi-field map - not supported: {:?}",
                                entry
                            ),
                        },
                    }
                }
                MemberKey::NonMemberKey { .. } => {
                    panic!("Please open a github issue with repro steps")
                }
            },
            None => type_to_field_name(&ge.entry_type).unwrap_or_else(|| {
                let combined_comments =
                    combine_comments(trailing_comments, &optional_comma.trailing_comments);
                match metadata_from_comments(&combined_comments.unwrap_or_default()) {
                    RuleMetadata {
                        name: Some(name), ..
                    } => name,
                    _ => format!("index_{index}"),
                }
            }),
        },
        GroupEntry::TypeGroupname {
            trailing_comments,
            ge: TypeGroupnameEntry { name, .. },
            ..
        } => match !is_identifier_user_defined(&name.to_string()) {
            true => {
                let combined_comments =
                    combine_comments(trailing_comments, &optional_comma.trailing_comments);
                match metadata_from_comments(&combined_comments.unwrap_or_default()) {
                    RuleMetadata {
                        name: Some(name), ..
                    } => name,
                    _ => format!("index_{index}"),
                }
            }
            false => name.to_string(),
        },
        GroupEntry::InlineGroup { group, .. } => panic!(
            "not implemented (define a new struct for this!) = {}\n\n {:?}",
            group, group
        ),
    });
    append_number_if_duplicate(already_generated, field_name)
}

// Only returns Some(String) if there was an explicit field name provided, otherwise None.
// If you need to try and make one using the type/etc, then try group_entry_to_field_name()
// Also does not do any CamelCase or snake_case formatting.
fn group_entry_to_raw_field_name(entry: &GroupEntry) -> Option<String> {
    match entry {
        GroupEntry::ValueMemberKey { ge, .. } => match ge.member_key.as_ref() {
            Some(MemberKey::Bareword { ident, .. }) => Some(ident.to_string()),
            // a quoted text key is sugar for the bareword key, so enum-variant naming (group
            // choices) must converge with the bareword path rather than treat it as nameless
            Some(MemberKey::Value {
                value: cddl::token::Value::TEXT(t),
                ..
            }) => Some(t.to_string()),
            _ => None,
        },
        GroupEntry::TypeGroupname {
            ge: TypeGroupnameEntry { name, .. },
            ..
        } => match !is_identifier_user_defined(&name.to_string()) {
            true => None,
            false => Some(name.to_string()),
        },
        // An inline group has no explicit field name — which is exactly what `None` means here, and
        // what the sole caller (the one-entry group-choice arm in `parse_group`) already handles by
        // falling back to a type-derived variant name. It reaches this only for a shape
        // `group_entry_to_type` rejected one line earlier (`t = [ (uint, tstr) // bytes ]`), so the
        // derived name is inert: `finalize` short-circuits on the recorded rejection before any
        // emission. Panicking here instead would abort the run AFTER the graceful rejection was
        // already recorded, which is the abort this seam exists to avoid.
        GroupEntry::InlineGroup { .. } => None,
    }
}

fn group_entry_rule_metadata(entry: &GroupEntry, optional_comma: &OptionalComma) -> RuleMetadata {
    let entry_trailing_comments = match entry {
        GroupEntry::ValueMemberKey {
            trailing_comments, ..
        } => trailing_comments,
        GroupEntry::TypeGroupname {
            trailing_comments, ..
        } => trailing_comments,
        GroupEntry::InlineGroup { group, .. } => panic!(
            "not implemented (define a new struct for this!) = {}\n\n {:?}",
            group, group
        ),
    };
    let combined_comments =
        combine_comments(entry_trailing_comments, &optional_comma.trailing_comments);
    metadata_from_comments(&combined_comments.unwrap_or_default())
}

/// The `@name` that names a MEMBER-position anonymous inline array, read from the one comment slot
/// that spelling puts it in: the enclosing group entry's trailing comments (plus the trailing-comma
/// slot), exactly the pair `group_entry_rule_metadata` reads for the field rename.
///
/// `get_comment_after(type2)` deliberately cannot reach that slot — its documented rule is that a
/// type does not inherit the comment of a parent it merely happens to end, and a general
/// `Type -> GroupEntry` ascent would leak every field-level directive into every type2 read. So the
/// naming site asks for the slot by itself, under the narrowest scope that keeps the name
/// unambiguous: the anonymous array must be the member's WHOLE type, UP TO tag wrappers. The
/// ascent is therefore required to be
/// `Type2 -> (Type1 -> TypeChoice -> Type -> Type2::TaggedData)* -> Type1 -> TypeChoice -> Type ->
/// ValueMemberKeyEntry -> GroupEntry`, with EVERY rung over an operator-free `Type1` and a
/// single-choice `Type`. Every other spelling — a `.cbor` payload (whose `Type2`'s parent is the
/// `Operator`), a choice arm, a parenthesized type, an array nested inside another anonymous array
/// — keeps the anonymous-group rejection rather than guessing which construct the name was meant
/// for.
///
/// Tag layers are walked (any number of them, `#6.42([x: uint])` and `#6.1(#6.42([x: uint]))`
/// alike) because a tag mints NO type of its own: it wraps whatever its payload parses to, so the
/// anonymous array remains the sole nameable referent and the name can only mean the struct. The
/// name mints the struct and the tag wraps it, which is byte-for-byte the named-rule remedy
/// (`inner = [x: uint]` / `f: #6.42(inner)`) with a different identifier. Without this the
/// rejection would advertise an `@name` door that the tagged spelling cannot open. The per-rung
/// operator-free and single-choice requirements are what keep it unambiguous: an operator makes the
/// array the operator's target rather than the tag's whole payload, and a multi-choice `Type` at
/// any rung reintroduces the arm-vs-member ambiguity the untagged spelling already refuses.
///
/// Only `.name` is consumed. The same comment is ALSO the field-rename slot, so one `@name` here
/// names both the field and the struct that field holds; every other directive on it keeps the
/// field-level meaning it already had.
fn anon_array_member_name<'a>(
    parent_visitor: &'a ParentVisitor<'a, 'a>,
    type2: &'a Type2<'a>,
) -> Option<String> {
    // The ascent is a loop only because of tag layers: each iteration climbs one
    // `Type2 -> Type1 -> TypeChoice -> Type` rung and then asks what that `Type` belongs to. A
    // `ValueMemberKeyEntry` ends the climb (the member slot we want); a `Type2::TaggedData` means
    // we were inside a tag's payload, so the tag becomes the new `Type2` and the same rung repeats.
    let mut current = type2;
    let value_member_key = loop {
        let type1 = match CDDLType::from(current).parent(parent_visitor)? {
            CDDLType::Type1(type1) => *type1,
            _ => return None,
        };
        // A control/range operator means the array is the operator's target (`bytes .cbor [..]`),
        // not the member's own type, and the comment after it is the operator chain's, not the
        // array's.
        if type1.operator.is_some() {
            return None;
        }
        let type_choice = match CDDLType::from(type1).parent(parent_visitor)? {
            CDDLType::TypeChoice(type_choice) => *type_choice,
            _ => return None,
        };
        let entry_type = match CDDLType::from(type_choice).parent(parent_visitor)? {
            CDDLType::Type(entry_type) => *entry_type,
            _ => return None,
        };
        // A choice arm's name would be ambiguous between the arm and the member, and the arm
        // spelling already has its own reachable slot (`TypeChoice::comments_after_type`).
        if entry_type.type_choices.len() != 1 {
            return None;
        }
        match CDDLType::from(entry_type).parent(parent_visitor)? {
            CDDLType::ValueMemberKeyEntry(value_member_key) => break *value_member_key,
            // A tag layer. The tag mints no type of its own — it wraps whatever its payload parses
            // to — so the anonymous array is still the only nameable referent, and climbing past it
            // introduces no ambiguity. Repeat the rung with the tag as the new `Type2`.
            CDDLType::Type2(tagged @ Type2::TaggedData { .. }) => current = tagged,
            _ => return None,
        }
    };
    let entry = match CDDLType::from(value_member_key).parent(parent_visitor)? {
        CDDLType::GroupEntry(entry) => *entry,
        _ => return None,
    };
    let group_choice = match CDDLType::from(entry).parent(parent_visitor)? {
        CDDLType::GroupChoice(group_choice) => *group_choice,
        _ => return None,
    };
    let (_, optional_comma) = group_choice
        .group_entries
        .iter()
        .find(|(candidate, _)| std::ptr::eq(candidate, entry))?;
    group_entry_rule_metadata(entry, optional_comma).name
}

fn rust_type_from_type1(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type1: &Type1,
    cli: &Cli,
) -> RustType {
    let control = type1
        .operator
        .as_ref()
        .map(|op| parse_control_operator(types, parent_visitor, &type1.type2, op, None, cli));
    let base_type = rust_type_from_type2(types, parent_visitor, &type1.type2, cli);
    // println!("type1: {:#?}", type1);
    match control {
        Some(ControlOperator::CBOR(ty)) => {
            // The MEMBER route to the rule-position `.cbor` head check in `parse_type`: RFC 8610
            // restricts `.cbor` to byte strings, and a head that is not one is refused rather than
            // asserted. The already-parsed payload `ty` is the inert placeholder — it is the type a
            // `bytes .cbor <payload>` member would have carried, so the walk continues over a shape
            // every later step already handles, and `finalize` drains the rejection before any of it
            // is emitted.
            if !matches!(
                base_type.conceptual_type.resolve_alias_shallow(),
                ConceptualRustType::Primitive(Primitive::Bytes)
            ) {
                types.record_rejection(non_bytes_cbor_head_rejection(
                    None,
                    &type1.type2.to_string(),
                ));
                return ty;
            }
            ty.as_bytes()
        }
        Some(ControlOperator::Range((low, high))) => match &type1.type2 {
            Type2::Typename { ident, .. } => {
                match ident_to_primitive(&CDDLIdent::new(ident.to_string())) {
                    Some(p) => range_to_primitive(low, high, p),
                    None => base_type.with_bounds((low, high)),
                }
            }
            // the base value will be a constant due to incomplete parsing earlier for explicit ranges
            // e.g. foo = 0..255
            Type2::IntValue { .. } => range_to_primitive(low, high, Primitive::I64),
            Type2::UintValue { .. } => range_to_primitive(low, high, Primitive::U64),
            _ => base_type.with_bounds((low, high)),
        },
        // member-position float window (`[f: 0.5..10.5]`, `[g: float64 .lt 10.5]`): attach the
        // NaN-safe window to the primitive so the field's ctor/setter/deserialize enforce it.
        Some(ControlOperator::RangeFloat(window)) => match &type1.type2 {
            Type2::Typename { ident, .. } => {
                match ident_to_primitive(&CDDLIdent::new(ident.to_string())) {
                    Some(p) => float_range_to_primitive(window, p),
                    None => base_type.with_float_bounds(window),
                }
            }
            // literal-headed member range promoted to float by a decimal endpoint (`[f: 0.5..10.5]`,
            // `[f: 0..10.5]`) — the base value is a Fixed constant, so use an f64 primitive.
            Type2::IntValue { .. } | Type2::UintValue { .. } | Type2::FloatValue { .. } => {
                float_range_to_primitive(window, Primitive::Float)
            }
            _ => base_type.with_float_bounds(window),
        },
        // The member route to the same `.default` application as the rule-position arm in
        // `parse_type` — refused identically, with the UNDEFAULTED type as the inert placeholder the
        // walk continues over. This seam has no rule name to prefix (it serves every member /
        // element / choice-arm position), so the message stands alone.
        Some(ControlOperator::Default(default_value)) => {
            match base_type.try_default(default_value.clone()) {
                Ok(defaulted) => defaulted,
                Err(undefaulted) => {
                    types.record_rejection(unmappable_default_head_rejection(
                        None,
                        &type1.type2,
                        &default_value,
                    ));
                    undefaulted
                }
            }
        }
        None => base_type,
    }
}

/// Resolve a type/group name that may carry generic arguments into a `RustType`.
///
/// With `generic_args == None` this is exactly `types.new_type(&cddl_ident, cli)`, so callers that
/// previously did that directly stay byte-identical. With generic args present it registers an
/// anonymous generic instance under the synthesized name `<name>_<arg-variants>` (e.g. a
/// `pair<uint, tstr>` element becomes the `PairU64Text` instance) and resolves *that* instance — otherwise the
/// args are silently dropped and the emitted code references the bare, never-emitted generic base.
///
/// Shared by every member/element position that can carry a generic instantiation
/// (`rust_type_from_type2`'s `Type2::Typename` arm and `parse_group_type`'s single-entry
/// `TypeGroupname` array arm) so the two paths cannot drift.
/// The INSTANTIATION-derived canonical CDDL ident of a generic invocation:
/// `<def-name>_<args' canonical identity names>` (`set` + `[key_hash]` → `set_KeyHash`, camel-cased
/// to `SetKeyHash` by `RustIdent::new`). The argument fragments preserve that historic spelling for
/// ordinary unconstrained arguments, but include occurrence/config/codec differences recursively:
/// `set<([* uint])>` and `set<([*5 uint])>` cannot both register as `SetArrU64`.
///
/// The ONE owner of this spelling so every call site — anonymous use
/// (`generic_instance_or_new_type`) and named binding (`foo = bar<text>`) — derives the SAME
/// instantiation identity, which the Phase 2.3 set-nominal dedup keys on.
fn generic_instance_canonical_cddl_ident(
    cddl_ident: &CDDLIdent,
    generic_args: &[RustType],
) -> CDDLIdent {
    let args_name = generic_args
        .iter()
        .map(RustType::generic_argument_identity_fragment)
        .collect::<Vec<String>>()
        .join("_");
    CDDLIdent::new(format!("{cddl_ident}_{args_name}"))
}

fn generic_instance_or_new_type(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    cddl_ident: CDDLIdent,
    generic_args: &Option<GenericArgs>,
    cli: &Cli,
) -> RustType {
    match generic_args {
        Some(args) => {
            // This is for anonymous instances (i.e. members) such as:
            // foo = [a: bar<text, bool>]
            // so to be able to expose it to wasm, we create a new generic instance
            // under the name bar_string_bool in this case.
            let generic_args = args
                .args
                .iter()
                .map(|a| rust_type_from_type1(types, parent_visitor, &a.arg, cli))
                .collect::<Vec<_>>();
            let instance_cddl_ident =
                generic_instance_canonical_cddl_ident(&cddl_ident, &generic_args);
            let instance_ident = RustIdent::new(instance_cddl_ident.clone());
            let generic_ident = RustIdent::new(cddl_ident);
            types.register_generic_instance(GenericInstance::new(
                instance_ident.clone(),
                generic_ident,
                generic_args,
                // synthesized name for an anonymous use site (`[a: bar<text>]` → `BarText`): when
                // this resolves to a transparent collection, its wasm wrapper lowers to the
                // STRUCTURAL name, not this synthesized ident (see the anonymous-collapse convergence).
                true,
                // an anonymous use site's ident IS the instantiation canonical (`SetKeyHash`).
                instance_ident,
            ));
            types.new_type(&instance_cddl_ident, cli)
        }
        None => types.new_type(&cddl_ident, cli),
    }
}

fn rust_type_from_type2(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type2: &Type2,
    cli: &Cli,
) -> RustType {
    // TODO: socket plugs (used in hash type)
    match &type2 {
        Type2::UintValue { value, .. } => {
            ConceptualRustType::Fixed(FixedValue::Uint(*value as u64)).into()
        }
        Type2::IntValue { value, .. } => {
            ConceptualRustType::Fixed(FixedValue::Nint(*value as i128)).into()
        }
        Type2::FloatValue { value, .. } => {
            ConceptualRustType::Fixed(FixedValue::Float(*value)).into()
        }
        Type2::TextValue { value, .. } => {
            ConceptualRustType::Fixed(FixedValue::Text(value.to_string())).into()
        }
        Type2::B16ByteString { value, .. }
        | Type2::B64ByteString { value, .. }
        | Type2::UTF8ByteString { value, .. } => {
            ConceptualRustType::Fixed(FixedValue::Bytes(value.to_vec())).into()
        }
        Type2::Typename {
            ident,
            generic_args,
            ..
        } => generic_instance_or_new_type(
            types,
            parent_visitor,
            CDDLIdent::new(ident.ident),
            generic_args,
            cli,
        ),
        Type2::Array { group, .. } => {
            // TODO: support for group choices in arrays?
            match group.group_choices.len() {
                1 => {
                    let group_choice = &group.group_choices.first().unwrap();
                    match parse_group_type(
                        types,
                        parent_visitor,
                        group_choice,
                        Representation::Array,
                        None,
                        cli,
                    ) {
                        GroupParsingType::HomogenousArray(element_type, bounds) => {
                            // Resolve aliases before stamping: an alias is transparent (one wire
                            // form, a value IS the aliased type), so `[* kv_alias]` must materialize
                            // the plain group exactly like `[* kv]` does. Matching the bare
                            // `Rust(ident)` only would leave the group unregistered and the emitted
                            // alias chain dangling on a struct that was never defined. Same pattern
                            // as the `WrappedBasicGroup` sibling below.
                            if let ConceptualRustType::Rust(element_ident) =
                                element_type.conceptual_type.resolve_alias_shallow()
                            {
                                types.set_rep_if_plain_group(
                                    parent_visitor,
                                    element_ident,
                                    Representation::Array,
                                    cli,
                                );
                            }
                            let array_type: RustType =
                                ConceptualRustType::Array(Box::new(element_type)).into();
                            match bounds {
                                Some(bounds) => array_type.with_bounds(bounds),
                                None => array_type,
                            }
                        }
                        GroupParsingType::HomogenousMap(_, _, _) => unreachable!(),
                        GroupParsingType::Heterogenous => {
                            let mut rule_metadata = RuleMetadata::from(
                                get_comment_after(parent_visitor, &CDDLType::from(type2), None)
                                    .as_ref(),
                            );
                            // At MEMBER position the naming comment lands one level further out than
                            // `get_comment_after(type2)` reaches, so ask for that slot explicitly.
                            if rule_metadata.name.is_none() {
                                rule_metadata.name = anon_array_member_name(parent_visitor, type2);
                            }
                            // A heterogeneous inline array in a type position becomes a struct, and a
                            // struct needs a name. The `@name` comment on the type2 is the naming
                            // door — but it only reaches here from positions whose comment slot
                            // `get_comment_after(type2)` (plus the member-position slot above) can
                            // reach, so at the others there is no
                            // name to be had. That is a rejection, not an abort: record it with the
                            // remedy the message has always advertised and continue with an inert
                            // placeholder, so `finalize` reports it alongside anything else the walk
                            // finds. Wording is unchanged from the panic it replaces (both halves of
                            // the remedy are still the working ones) minus the AST dump, which was
                            // never actionable for a user reading their own CDDL.
                            let name = match rule_metadata.name.as_ref() {
                                Some(name) => name,
                                None => {
                                    types.record_rejection_once_at(
                                        type2,
                                        "anonymous-inline-array",
                                        format!(
                                            "Anonymous groups not allowed: the inline array `[{group}]` \
                                             is used where a type is required. Either create an explicit \
                                             rule (`foo = [0, bytes]`, then reference `foo`) or give it \
                                             a name using the `@name` notation."
                                        ),
                                    );
                                    return ConceptualRustType::Fixed(FixedValue::Null).into();
                                }
                            };
                            let cddl_ident = CDDLIdent::new(name);
                            let rust_ident = RustIdent::new(cddl_ident.clone());
                            parse_group(
                                types,
                                parent_visitor,
                                group,
                                &rust_ident,
                                Representation::Array,
                                None,
                                None,
                                &rule_metadata,
                                cli,
                            );
                            // we aren't returning an array, but rather a struct where the fields are ordered
                            types.new_type(&cddl_ident, cli)
                        }
                        GroupParsingType::WrappedBasicGroup(basic_type) => {
                            // A member-position anonymous array wrapping a plain-group reference
                            // (e.g. `bytes .cbor [coords]`, or a field `x = [coords]`) must promote
                            // the referenced plain group to an Array-rep Record struct, exactly like
                            // the `HomogenousArray` sibling above. Without this the group is never
                            // emitted and the returned type dangles on a bare, non-existent struct.
                            if let ConceptualRustType::Rust(element_ident) =
                                basic_type.conceptual_type.resolve_alias_shallow()
                            {
                                types.set_rep_if_plain_group(
                                    parent_visitor,
                                    element_ident,
                                    Representation::Array,
                                    cli,
                                );
                            }
                            basic_type
                        }
                    }
                }
                // array of elements with choices: enums?
                _ => {
                    // An inline array with group choices in member/element position has no
                    // anonymous representation here — but the NAMED form (a top-level rule
                    // `t = [ a // b ]` referenced by name) IS supported (verified to generate).
                    // Reject gracefully and point at it, rather than panicking.
                    types.record_rejection(
                        "an inline array with group choices (`[ a // b ]`) used as a member or \
                         element type is unsupported — name it as its own rule (`t = [ a // b ]`) \
                         and reference `t`"
                            .to_string(),
                    );
                    ConceptualRustType::Fixed(FixedValue::Null).into()
                }
            }
        }
        Type2::Map { group, .. } => {
            match group.group_choices.len() {
                1 => {
                    let group_choice = group.group_choices.first().unwrap();
                    match parse_group_type(
                        types,
                        parent_visitor,
                        group_choice,
                        Representation::Map,
                        None,
                        cli,
                    ) {
                        // Table map - homogenous key/value types
                        GroupParsingType::HomogenousMap(key_type, value_type, bounds) => {
                            // Generate a MapTToV for a { t => v } table-type map as we are an anonymous type
                            // defined as part of another type if we're in this level of parsing.
                            // We also can't have plain groups unlike arrays, so don't try and generate those
                            // for general map types we can though but not for tables
                            //let table_type_ident = RustIdent::new(CDDLIdent::new(format!("Map{}To{}", key_type.for_wasm_member(), value_type.for_wasm_member())));
                            //types.register_rust_struct(RustStruct::new_table(table_type_ident, None, key_type.clone(), value_type.clone()));
                            // An inline `{+ k => v}` field carries the non-empty bound on its own
                            // RustType (mirroring the inline `[+ T]` array arm), so `for_rust_member`
                            // renders `NonEmptyMap<K, V>` and deserialize routes through its TryFrom.
                            let map_type: RustType =
                                ConceptualRustType::Map(Box::new(key_type), Box::new(value_type))
                                    .into();
                            let map_type = match bounds {
                                Some(bounds) => map_type.with_bounds(bounds),
                                None => map_type,
                            };
                            // The row entry's own comment slot (`{ * k => v ; @duplicates preserve }`)
                            // is read here, giving the inline spelling the same row-scoped directives
                            // the NAMED table has — and rejecting everything else it can carry, so a
                            // live slot is never also a silent-drop slot.
                            apply_inline_table_row_metadata(types, group_choice, map_type)
                        }
                        // Every non-table inline map — `{ a: int, b: uint }`, `{ g }`, `{ * uint }`,
                        // `{}`. For `Representation::Map` `parse_group_type` returns only
                        // `HomogenousMap` (handled above) or `Heterogenous`, so this arm IS the
                        // whole non-table remainder; the `WrappedBasicGroup` / `HomogenousArray`
                        // returns live in its `Representation::Array` half.
                        //
                        // The `Type2::Array` sibling above turns `Heterogenous` into a struct by
                        // reading a `@name` off the type2's trailing comment. There is no such
                        // naming door on the map side, so an anonymous nested map has no
                        // representation here: reject gracefully and point at the named form,
                        // exactly like the multi-group-choice sibling below.
                        //
                        // The remedy is probed to generate under BOTH the default and the
                        // `--preserve-encodings` profile for every keyed shape that reaches this
                        // arm (array element, map value, `.cbor` payload, `/` choice alternative,
                        // generic argument, occurrence target, group-choice arm). A KEYLESS inline
                        // map (`{ g }`, `{ * uint }`) also lands here and its named form is
                        // rejected for a separate, self-describing reason (a map member needs a
                        // key), so the message points at the supported spelling rather than
                        // promising that naming alone fixes every shape.
                        _ => {
                            types.record_rejection_once_at(
                                type2,
                                "non-table-inline-map",
                                 "an inline map (`{ a: int, b: uint }`) used as a member or element \
                                  type is unsupported unless it is a table (`{ * k => v }`) — name \
                                 it as its own rule (`m = { a: int, b: uint }`) and reference `m`"
                                    .to_string(),
                            );
                            ConceptualRustType::Fixed(FixedValue::Null).into()
                        }
                    }
                }
                _ => {
                    // An inline map with group choices in member/element position has no
                    // anonymous representation here — but the NAMED form (a top-level rule
                    // `t = { a // b }` referenced by name) IS supported (verified to generate).
                    // Reject gracefully and point at it, rather than panicking.
                    types.record_rejection(
                        "an inline map with group choices (`{ a // b }`) used as a member or \
                         element type is unsupported — name it as its own rule (`t = { a // b }`) \
                         and reference `t`"
                            .to_string(),
                    );
                    ConceptualRustType::Fixed(FixedValue::Null).into()
                }
            }
        }
        // unsure if we need to handle the None case - when does this happen?
        Type2::TaggedData { tag, t, .. } => {
            let tag_unwrap = tag_literal(tag).expect("tagged data without tag not supported");
            // Build the plain tagged inline occurrence — NO registry default is applied here. An inline
            // `#6.258([* a])` nominalizes into a shape-derived `Set<Elem>` wrapper (Phase 2.4), but that
            // minting happens at the ONE post-collapse seam (`IntermediateTypes::nominalize_inline_sets`,
            // run in `finalize` over the construction PRODUCTS), never inside this arm: the arm is also
            // traversed for the DISCARDED transient arms of a named two-arm rule's collapse recognition,
            // and minting here would register spurious nominals from arms that are thrown away. The
            // finalize seam sees only registered products, so the transient-arm interference class
            // cannot arise (this is why the old `SUPPRESS_INLINE_TAG_DEFAULT` thread-local is gone). The
            // seam recognizes an inline occurrence structurally: a `ConceptualRustType::Array` carrying a
            // mandatory `Tagged(258)` in its `encodings`.
            rust_type(types, parent_visitor, t, cli).tag(tag_unwrap)
        }
        Type2::ParenthesizedType { pt, .. } => rust_type(types, parent_visitor, pt, cli),
        x => {
            // Unsupported `type2` in MEMBER / ELEMENT position — the role-sibling of the rule-body
            // catch-all in `parse_type`. This function is only ever reached from a position that
            // needs a TYPE to store (an array element, a map key/value, a `.cbor` payload, a choice
            // arm, a generic argument, an occurrence target); rule bodies go through `parse_type`
            // and never arrive here, so "as a member or element type" is honest wording at this
            // seam. None of these constructs has a storable representation there, so reject
            // gracefully with an inert `Fixed(FixedValue::Null)` placeholder — exactly like the
            // inline-array / inline-map sibling arms above — instead of aborting the whole run on
            // otherwise-valid CDDL. `finalize` drains the recorded rejection into a graceful `Err`
            // before any generation runs.
            //
            // The (construct, hint) table below deliberately MIRRORS the rule-body one rather than
            // sharing it: the two sites say different things about the same construct (the remedies
            // differ by role), and the rule-body texts are matrix `code_anchor`s that must not move
            // when this one is reworded. Parallel per-site siblings are this repo's pattern for
            // exactly that reason (see the wasm wrapper-name collision detectors).
            let (construct, hint) = match x {
                Type2::Unwrap { .. } => (
                    "an unwrap (`~name`)".to_string(),
                    " — inline the referenced rule's definition manually".to_string(),
                ),
                Type2::DataMajorType { .. } => (
                    "a bare major-type constraint (`#N` / `#N.M`)".to_string(),
                    String::new(),
                ),
                // The grammar's `#` sigil, NOT the prelude NAME `any` — the latter is supported in
                // this position (it lowers to the `AnyCbor` runtime type) and never reaches here.
                Type2::Any { .. } => (
                    "the `any` type (`#`)".to_string(),
                    " — the prelude name `any` is supported in this position; write `any` instead"
                        .to_string(),
                ),
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
            let kind = match x {
                Type2::Unwrap { .. } => "unsupported-type2-unwrap",
                Type2::DataMajorType { .. } => "unsupported-type2-major",
                Type2::Any { .. } => "unsupported-type2-any",
                Type2::ChoiceFromGroup { .. } => "unsupported-type2-choice-from-group",
                Type2::ChoiceFromInlineGroup { .. } => "unsupported-type2-choice-from-inline-group",
                _ => "unsupported-type2-other",
            };
            types.record_rejection_once_at(
                type2,
                kind,
                format!("{construct} used as a member or element type is unsupported{hint}"),
            );
            ConceptualRustType::Fixed(FixedValue::Null).into()
        }
    }
}

fn rust_type(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    t: &Type,
    cli: &Cli,
) -> RustType {
    if t.type_choices.len() == 1 {
        rust_type_from_type1(
            types,
            parent_visitor,
            &t.type_choices.first().unwrap().type1,
            cli,
        )
    } else {
        let rule_metadata = RuleMetadata::from(
            get_comment_after(parent_visitor, &CDDLType::from(t), None).as_ref(),
        );
        if t.type_choices.len() == 2 {
            // A non-last arm's directives belong to a would-be variant.  The `/ null` collapse
            // has no variants, so leaving them for the fixed-singleton early return would accept
            // and discard them.  The LAST arm is the containing field's normal directive slot and
            // is consumed by `parse_record_from_group_choice`; do not reject it here.
            for choice in &t.type_choices[..1] {
                let arm_metadata = merge_metadata(
                    &RuleMetadata::from(choice.type1.comments_after_type.as_ref()),
                    &RuleMetadata::from(choice.comments_after_type.as_ref()),
                );
                let mut misplaced = arm_metadata.non_variant_directives();
                if arm_metadata.name.is_some() {
                    misplaced.push("@name");
                }
                if arm_metadata.comment.is_some() {
                    misplaced.push("@doc");
                }
                if !misplaced.is_empty() {
                    types.record_rejection(format!(
                        "{} on a non-last arm of an inline `T / null` choice: the choice lowers \
                         to an optional field rather than variants, so that arm owns no directive \
                         slot. Move a field directive to the entry's trailing comment, or put the \
                         annotation on a named type rule.",
                        misplaced.join(" / ")
                    ));
                }
            }
            // T / null   or   null / T   should map to Option<T>
            let a = &t.type_choices[0].type1;
            let b = &t.type_choices[1].type1;
            let collapse_inner = if type2_is_null(&a.type2) {
                Some(b)
            } else if type2_is_null(&b.type2) {
                Some(a)
            } else {
                None
            };
            if let Some(inner_type1) = collapse_inner {
                let inner_rust_type = rust_type_from_type1(types, parent_visitor, inner_type1, cli);
                // Member/element twin of the rule-level fixed/null lowering.  The singleton is
                // synthesized once per exact fixed identity and then the established Optional
                // lowering stores that nominal value.
                if let ConceptualRustType::Fixed(fixed) =
                    inner_rust_type.conceptual_type.resolve_alias_shallow()
                {
                    let fixed = fixed.clone();
                    let is_bare_null =
                        matches!(fixed, FixedValue::Null) && inner_rust_type.encodings.is_empty();
                    let singleton = register_fixed_singleton(
                        types,
                        parent_visitor,
                        synthesized_fixed_singleton_ident(&inner_rust_type),
                        inner_rust_type,
                        None,
                        None,
                        None,
                        cli,
                        true,
                    );
                    if is_bare_null {
                        return singleton;
                    }
                    return ConceptualRustType::Optional(Box::new(singleton)).into();
                }
                // The member-position sibling of the rule-level plain-group arm guard in
                // `parse_type_choices` (`x: kv / null`). Both collapse branches need it because the
                // `/ null` fork returns an `Option<T>` before `create_variants_from_type_choices` —
                // where every NON-collapsing arm is judged — is ever reached. Role-generic wording:
                // no rule name is available here, same as the fixed guard above.
                if reject_plain_group_type_choice_arm(
                    types,
                    &inner_rust_type,
                    "a two-arm `T / null` choice used as a member or element type",
                ) {
                    return ConceptualRustType::Fixed(FixedValue::Null).into();
                }
                return ConceptualRustType::Optional(Box::new(inner_rust_type)).into();
            }
        }
        let variants =
            create_variants_from_type_choices(types, parent_visitor, &t.type_choices, None, cli);
        let mut combined_name = String::new();
        // one caveat: nested types can leave ambiguous names and cause problems like
        // (a / b) / c and a / (b / c) would both be AOrBOrC
        for variant in &variants {
            if !combined_name.is_empty() {
                combined_name.push_str("Or");
            }
            // due to undercase primitive names, we need to convert here
            combined_name.push_str(&variant.rust_type().for_variant().to_string());
        }
        let base_ident = RustIdent::new(CDDLIdent::new(&combined_name));
        // Same carrier names do not prove the same enum: a value window, encoding operation, or
        // variant directive can make two `I64OrText` candidates incompatible. Reuse the existing
        // anonymous owner only for the full structural fingerprint; otherwise mint a deterministic
        // sibling. An AUTHOR who owns the base stays on the established global-registration seam:
        // its structurally identical anonymous use shares that type, and an incompatible one rejects
        // rather than renaming either public claimant. Looking across the anonymous carrier family's
        // prior siblings (rather than only at the bare name) preserves reuse when A, B, A are
        // encountered in one spec.
        let candidate = RustStruct::new_type_choice(
            base_ident.clone(),
            None,
            Some(&rule_metadata),
            variants.clone(),
            cli,
        );
        let sibling_base = format!("{combined_name}_inline_choice");
        let sibling_prefix = RustIdent::new(CDDLIdent::new(&sibling_base)).to_string();
        let combined_ident = if types.is_toplevel_rule(&base_ident) {
            base_ident
        } else {
            types
                .rust_structs()
                .iter()
                .find(|(ident, existing)| {
                    !types.is_toplevel_rule(ident)
                        && (ident.as_ref() == base_ident.as_ref()
                            || ident.as_ref().starts_with(&sibling_prefix))
                        && existing.structurally_equivalent(&candidate)
                })
                .map(|(ident, _)| ident.clone())
                .unwrap_or_else(|| {
                    if types.rust_struct(&base_ident).is_some() {
                        types.fresh_synthesized_ident(&sibling_base)
                    } else {
                        base_ident
                    }
                })
        };
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_type_choice(
                combined_ident.clone(),
                None,
                Some(&rule_metadata),
                variants,
                cli,
            ),
            cli,
        );
        types.new_type(&CDDLIdent::new(combined_ident.to_string()), cli)
    }
}

fn group_entry_optional(entry: &GroupEntry) -> bool {
    let occur = match entry {
        GroupEntry::ValueMemberKey { ge, .. } => &ge.occur,
        GroupEntry::TypeGroupname { ge, .. } => &ge.occur,
        // The only caller (`parse_record_from_group_choice`) rejects EVERY `InlineGroup` entry
        // gracefully before its field loop reads optionality, and nothing else calls this — so an
        // inline group cannot arrive. Kept as an assertion rather than converted to a rejection: a
        // rejection here would be untestable dead code, whereas a future caller that does reach it
        // fails loudly as a NEW panic class in the recombination sweep.
        GroupEntry::InlineGroup { .. } => unreachable!(
            "an inline group entry is rejected by the record path before optionality is read"
        ),
    };
    occur
        .as_ref()
        .map(|o| matches!(o.occur, Occur::Optional { .. }))
        .unwrap_or(false)
}

fn group_entry_to_type(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    entry: &GroupEntry,
    cli: &Cli,
) -> RustType {
    match entry {
        GroupEntry::ValueMemberKey { ge, .. } => {
            rust_type(types, parent_visitor, &ge.entry_type, cli)
        }
        GroupEntry::TypeGroupname { ge, .. } => {
            // A bare TypeGroupname member can carry generic args (`[pair<uint, tstr>]`) — route
            // through the shared helper so the anonymous instance (`PairU64Text`) is registered
            // and emitted instead of dropping `ge.generic_args` and referencing the never-emitted
            // bare generic base. `generic_args == None` stays byte-identical to the previous
            // `types.new_type(...)` call (the helper's documented contract), so non-generic
            // TypeGroupname members are unaffected. This shares the exact registration path used by
            // keyed members (`foo: bar<uint>` via ValueMemberKey), rule RHSes, and homogeneous array
            // elements (`[* pair<uint, tstr>]`), so the positions cannot drift.
            generic_instance_or_new_type(
                types,
                parent_visitor,
                CDDLIdent::new(ge.name.to_string()),
                &ge.generic_args,
                cli,
            )
        }
        // An inline group as the sole entry of a group-choice arm — `t = [ (uint, tstr) // bytes ]`
        // and its map-rep spelling `t = { (a: uint) // b: tstr }`. That arm (`parse_group`'s
        // one-entry variant branch) is the only path that can deliver an `InlineGroup` here: the
        // record path rejects every inline group before calling us, and an open-array rest tail
        // never selects one (an inline group carries no `ge.occur`, so it is never a candidate).
        // Reject gracefully and name the group; the remedy is probed to generate under BOTH the
        // default and the `--preserve-encodings` profile for the array-rep, map-rep, `?`-marked and
        // both-arms spellings.
        GroupEntry::InlineGroup { .. } => {
            types.record_rejection(
                "an inline group (`(uint, tstr)`) in entry position is unsupported. Name the group \
                 instead (e.g. `pair = (uint, tstr)`, then reference `pair`)."
                    .to_string(),
            );
            ConceptualRustType::Fixed(FixedValue::Null).into()
        }
    }
}

/// Classification of a single map-entry's key, used at both the group-choice collapse site and the
/// record map path. It never `panic!`s: unsupported/non-literal keys are reported as `NonFixed` (and
/// keyless entries as `Keyless`) so the caller can record a graceful rejection instead of aborting
/// the whole run. This matters on the record path because `group_entry_to_field_name` itself panics
/// on non-uint Type1 (arrow) member keys, so the key must be classified through here BEFORE field
/// naming runs.
enum MapKeyKind {
    /// `k: v` / `k => 5` — a literal/bareword key we can write and verify.
    Fixed(FixedValue),
    /// `k => v` with a non-literal key type (e.g. `uint => tstr`) — a real table entry. Collapsing
    /// it into an enum variant would silently drop the key type, so it is unsupported here.
    NonFixed,
    /// No member key present on this entry: a bare value (`{ uint // ... }`) or a (plain-)group
    /// reference (`{ foo // ... }`) whose referenced struct owns its own keys.
    Keyless,
}

fn group_entry_map_key_kind(entry: &GroupEntry) -> MapKeyKind {
    match entry {
        GroupEntry::ValueMemberKey { ge, .. } => match ge.member_key.as_ref() {
            None => MapKeyKind::Keyless,
            Some(MemberKey::Value { value, .. }) => match value {
                cddl::token::Value::UINT(x) => MapKeyKind::Fixed(FixedValue::Uint(*x as u64)),
                cddl::token::Value::INT(x) => MapKeyKind::Fixed(FixedValue::Nint(*x as i128)),
                cddl::token::Value::TEXT(x) => MapKeyKind::Fixed(FixedValue::Text(x.to_string())),
                cddl::token::Value::FLOAT(x) => MapKeyKind::Fixed(FixedValue::Float(*x)),
                _ => MapKeyKind::NonFixed,
            },
            Some(MemberKey::Bareword { ident, .. }) => {
                MapKeyKind::Fixed(FixedValue::Text(ident.to_string()))
            }
            // Share the literal lowering with `.default`: it owns every literal-shaped Type2,
            // including the fixed prelude singletons whose literals are spelled as typenames.
            // This prevents a new fixed kind from getting a misleading non-fixed-map-key verdict
            // merely because this seam's duplicate list was not extended. Other typename keys and
            // non-literal Type2 shapes stay NonFixed.
            Some(MemberKey::Type1 { t1, .. }) => type2_to_fixed_value(&t1.type2)
                .map(MapKeyKind::Fixed)
                .unwrap_or(MapKeyKind::NonFixed),
            Some(MemberKey::NonMemberKey { .. }) => MapKeyKind::NonFixed,
        },
        _ => MapKeyKind::Keyless,
    }
}

/// Rust strict and reserved keywords (plus the 2024 additions `gen`/`try`). A struct field named by
/// any of these is invalid Rust; `parse_record_from_group_choice` rejects such fields with the
/// `@name` remedy rather than emitting source that only the rustfmt gate would catch.
pub(crate) const RUST_KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "dyn", "else", "enum", "extern", "false", "fn",
    "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
    "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe",
    "use", "where", "while", "async", "await", "try", "abstract", "become", "box", "do", "final",
    "macro", "override", "priv", "typeof", "unsized", "virtual", "yield", "gen",
];

/// Field names reserved because the generated `serialize`/`deserialize` bodies bind a FIXED local (or
/// take a parameter) by the same name, and the field's own local then collides with it — the crate
/// generates at exit 0 and does not compile, two build steps from the CDDL line that caused it.
///
/// **Evidence-based membership**: every entry breaks at least one shape × profile, measured by
/// generating the shape and `cargo check`ing the emitted rust crate; the second element records what
/// broke. Emitter locals that were swept and did NOT break anywhere stay OUT (refusing a name that
/// works is a gratuitous break) — they are listed in `GENERATED_LOCAL_PROBED_SAFE` instead, so a
/// newly-added emitter local joins the sweep rather than being guessed at.
///
/// Probe scope (2026-08-01, `b5b6283b`): five shapes — array-rep record, map-rep record, tagged
/// record (`#6.42([…])`), embedded plain group, group-choice arm — × three profiles — default,
/// `--preserve-encodings`, `--preserve-encodings --canonical-form` — × two field types (`bytes`,
/// `uint`), plus a `--wasm=true` pass over the same matrix (which surfaced no name the rust-only
/// pass had not). NOT probed: `--json-serde-derives`/`--json-schema-export`, `--component`, a field
/// whose type is a named rule/newtype, `.cbor`-payload and bounded-type member positions.
///
/// **Uniform across PROFILES, scoped by SHAPE.** No FLAG may rescue a refused field — a name that
/// breaks under `--preserve-encodings` is refused under the default profile too, because the spec
/// author does not choose their consumer's flags (`tag` and `len_encoding` compile by default and
/// break the moment `--preserve-encodings` is passed). But a name is only reserved where the emitter
/// actually BINDS the colliding local: the loop counter `read` exists only in a map-record
/// deserializer and the tag read only in a tagged type's, so refusing them in an array record would
/// break working specs for a collision that shape cannot have. The `tag: 0` group-choice
/// discriminant — this repo's own `tests/core` and `tests/preserve-encodings` fixtures, and an
/// idiom across real specs — is exactly such a spec, and it keeps generating. Each entry's
/// `ReservedScope` states where it applies; `generated_local_hazard_robustness_catalog` pins both
/// halves (refused inside the scope, accepted outside it).
///
/// LOCKSTEP with `identifier_hazard_tests::generated_local_registry_covers_emitter_locals`, which
/// re-derives the emitter-local vocabulary from the emitter sources and fails until a new local is
/// verdicted into this list or into `GENERATED_LOCAL_PROBED_SAFE`.
pub(crate) const GENERATED_LOCAL_RESERVED: &[(&str, ReservedScope, &str)] = &[
    (
        "len",
        ReservedScope::Any,
        "the array/map length read (`let len = raw.array_sz()?`): array-rep, map-rep and tagged \
         records under every profile (E0308), plus the embedded-plain-group and group-choice-arm \
         shapes under `--preserve-encodings` (the minted `len_encoding` companion also collides \
         with the container's own — E0062/E0124/E0599)",
    ),
    (
        "len_encoding",
        ReservedScope::Any,
        "the container's own length-encoding local and `<Type>Encoding::len_encoding` field: all \
         five probed shapes under `--preserve-encodings` (E0308)",
    ),
    (
        "orig_deser_order",
        ReservedScope::MapRep,
        "the map-record deserializer's `let mut orig_deser_order = Vec::new()`: map-rep records \
         under `--preserve-encodings` (E0308/E0599); array records emit no such local",
    ),
    (
        "raw",
        ReservedScope::Any,
        "the deserializer parameter itself (`fn deserialize(raw: &mut Deserializer)`): array-rep, \
         map-rep and tagged records and embedded plain groups under every profile (E0599 — every \
         later field's read calls a `Deserializer` method on the shadowed binding)",
    ),
    (
        "read",
        ReservedScope::MapRep,
        "the map-record deserialize loop counter (`let mut read = 0`): map-rep records under every \
         profile (E0308/E0599); array records emit no such local",
    ),
    (
        "tag",
        ReservedScope::Tagged,
        "the tag read in a tagged type's deserializer (`let tag = raw.tag()?`): `#6.n(…)` records \
         under `--preserve-encodings` (E0062/E0124/E0308/E0599); an untagged record emits no tag \
         read, which is why the `tag: 0` group-choice discriminant is untouched",
    ),
    (
        "text_key",
        ReservedScope::MapRep,
        "the map-record unknown-key path's `let text_key`: map-rep records under \
         `--preserve-encodings` (E0308/E0599); array records emit no such local",
    ),
];

/// Where a `GENERATED_LOCAL_RESERVED` entry applies — the shapes whose emitted body binds the
/// colliding local. Never a PROFILE condition: see the registry's doc comment.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum ReservedScope {
    /// Every record's serialization body binds it.
    Any,
    /// Only a map-representation record's body.
    MapRep,
    /// Only a record that is the body of a tagged type (`#6.n([…])` / `#6.n({…})`).
    Tagged,
}

impl ReservedScope {
    pub(crate) fn applies(self, rep: Representation, tagged: bool) -> bool {
        match self {
            Self::Any => true,
            Self::MapRep => rep == Representation::Map,
            Self::Tagged => tagged,
        }
    }
}

/// Emitter locals that WERE swept (same matrix as `GENERATED_LOCAL_RESERVED`) and broke nothing, so
/// they are deliberately NOT reserved. This list exists for the LOCKSTEP test, not for the parser:
/// it is the "probed non-colliding" verdict that lets the test tell a name we have judged apart from
/// a name a new emitter just introduced.
// Consumed only by the bin-only LOCKSTEP test (`src/tests/` is not in the library's module tree),
// so every non-bin build sees it as dead.
#[allow(dead_code)]
pub(crate) const GENERATED_LOCAL_PROBED_SAFE: &[&str] = &[
    "_depth_guard",
    "_e",
    "_k",
    "_rest_elem",
    "_rest_key",
    "_rest_value",
    "buf",
    "byte",
    "bytes",
    // THE OPEN-TABLE JSON FAMILY — `captured_range`, `ks`, `out`, `seen`, `typed_range` (the other
    // four point back here). All five are bound ONLY by `emit_open_table_json`, i.e. only in an open
    // table's hand-written `Serialize`/`Deserialize`/`JsonSchema`, and only under a json flag.
    //
    // Swept 2026-08-02 over the five registry shapes — array-rep record, map-rep record, tagged
    // record (`#6.4n([…])`), embedded plain group, group-choice arm — plus a `bytes`-typed map-rep
    // shape, × default / `--preserve-encodings` / `--preserve-encodings --canonical-form` × json
    // flags OFF and ON (the profile axis the registry's original sweep left unprobed, and the one
    // these locals actually live in), plus the OPEN TABLE shape itself with each name `@name`d onto
    // the typed row and onto the catch-all row, × the same three profiles × json off/on. 42 bundled
    // crates, each name in its own rule, every one `cargo check` clean.
    //
    // Two structural reasons behind that result, both worth stating because they bound what a future
    // emitter change could break. An open table has ZERO fixed fields, so the only user-controlled
    // name reaching these bodies is a ROW's (`@name`) — and every reference to a row's field is
    // qualified (`self.<row>` on the write side, `out.<row>` on the read side), so a local can never
    // shadow one: the swept crates really do emit `out.seen.insert(…)` beside `let mut seen`, and
    // `self.ks.iter()` beside `while let Some(ks)`. `typed_range`/`captured_range` are bound inside
    // the `json_schema` body, which references no field at all.
    "captured_range",
    // THE NONEMPTY OPEN-TABLE FAMILY — `captured_staged`, `seed`, `typed_staged` (the other two
    // point back here). All three are bound ONLY where a `{ + K_t => V_t, * K_r => V_r }` rule is
    // present: `seed` in the seeded `new(first_key, first_value)` door's struct literal, the two
    // `_staged` vectors in the JSON visitor that assembles through that door (json flags only).
    //
    // Swept 2026-08-02 over the five registry shapes — array-rep record, map-rep record, tagged
    // record (`#6.42([…])`), embedded plain group, group-choice arm — plus a `bytes`-typed map-rep
    // shape, EACH carrying a field of the name, × default / `--preserve-encodings` /
    // `--preserve-encodings --canonical-form` × json flags OFF and ON; plus the open table itself
    // with each name `@name`d onto the typed row AND onto the catch-all row, in both the `*` and the
    // `+` flavor, over the same six profiles. Every crate `cargo check` clean, and non-vacuously so
    // (the json+preserve cell emits `let mut seed = OrderedHashMap::new()` inside a struct literal
    // whose sibling rules carry `pub seed` fields, and `typed_staged.push(…)` in the same crate as
    // a `pub typed_staged` field).
    //
    // The structural reason, worth stating because it bounds what a future emitter change could
    // break, is the open table's zero-fixed-field shape (the same reason the JSON family above is
    // safe): the ONLY user-controlled name reaching these bodies is a ROW's (`@name`), every
    // reference to a row's field is qualified (`self.<row>` / `out.<row>`), and the seeding block
    // deliberately binds a FIXED local rather than the field's name — so a row named `seed` emits
    // `seed: { let mut seed = …; }` where the inner binding shadows nothing the block reads.
    "captured_staged",
    "deser_order",
    "deser_variant",
    "deserializer",
    "e",
    "elem",
    "element",
    "elements",
    "encs",
    "errs",
    "f",
    "field",
    "field_index",
    "first",
    "first_key",
    "first_value",
    "force_canonical",
    "generator",
    // The open table's `deser_order` fallback closure parameter (`(0..self.<row>.len()).map(|i| 2 *
    // i)`) and the canonical merge's enumerate binding. Swept 2026-08-01 over the array-rep /
    // map-rep / tagged-record / embedded-plain-group / group-choice-arm / open-struct-map /
    // open-array / optional-field shapes × default / `--preserve-encodings` /
    // `--preserve-encodings --canonical-form` × wasm off and on: every crate compiles. The closure
    // body references no field, so a field named `i` (reached as `self.i`) cannot be shadowed by it.
    "i",
    "index",
    "initial_position",
    "inner",
    "k",
    "key",
    "key_order",
    // Open-table JSON local; verdict + sweep evidence at `captured_range` above.
    "ks",
    "list",
    "map",
    "native",
    "opt",
    // Open-table JSON local; verdict + sweep evidence at `captured_range` above.
    "out",
    "pairs",
    "present",
    "read_len",
    "rest_entries",
    "rest_i",
    "rest_key",
    "rest_value",
    "ret",
    "s",
    // NonEmpty open-table local; verdict + sweep evidence at `captured_staged` above.
    "seed",
    // Open-table JSON local; verdict + sweep evidence at `captured_range` above.
    "seen",
    "serializer",
    "special",
    "string",
    "tag_sz",
    "ty",
    // Open-table JSON local; verdict + sweep evidence at `captured_range` above.
    "typed_range",
    // NonEmpty open-table local; verdict + sweep evidence at `captured_staged` above.
    "typed_staged",
    "unknown_key",
    "v",
    "value",
    "variant_deser",
    "wrapper",
    "x",
];

/// The `--preserve-encodings` encoding-companion locals a record mints PER FIELD. A second field
/// whose own name is one of these spellings collides with the companion (or, for the `_key` case,
/// two fields mint the SAME companion name), emitting a crate that does not compile.
///
/// Measured at `b5b6283b`, `--preserve-encodings` (and the `--canonical-form` flavor):
/// - `<f>` + `<f>_encoding` — the field local shadows the value-encoding companion in the
///   struct-init shorthand, E0308. Breaks in array-rep, map-rep, embedded-plain-group and
///   group-choice-arm shapes, so it is checked in BOTH representations.
/// - `<f>` + `<f>_key_encoding` — same shadowing against the KEY-encoding companion, E0308. Map rep
///   only (array records mint no key encodings — probed clean in the array shape).
/// - `<f>` + `<f>_key` — both fields mint `<f>_key_encoding` into the encoding struct: E0124
///   (duplicate field) + E0062. Map rep only (probed clean in the array shape).
///
/// Checked uniformly across profiles for the same reason the single-name registry is: the default
/// profile compiles only because it mints no companions at all.
///
/// MAINTENANCE: this list is a hand-carried mirror of DERIVED naming — the emitters mint the
/// companions via `format!("{}_encoding", …)` / `…_key_encoding` (generation/serialize.rs,
/// deserialize.rs, mod.rs, enums.rs), and the LOCKSTEP scan
/// (`generated_local_registry_covers_emitter_locals`) verdicts FIXED locals only, so it cannot
/// see a companion-naming change. Renaming the companion scheme moves BOTH: these suffixes and
/// the emitters' format strings, or the pairwise refusal rots in both directions (gratuitous
/// refusals of the old spellings, unrefused collisions on the new ones).
const ENCODING_COMPANION_SUFFIXES: &[(&str, bool, &str)] = &[
    // (suffix, map-rep only, what collides)
    (
        "_encoding",
        false,
        "the value-encoding companion minted for `{base}` is spelled `{other}`, so the field's own \
         local shadows it in the struct-init shorthand (E0308)",
    ),
    (
        "_key_encoding",
        true,
        "the key-encoding companion minted for `{base}` is spelled `{other}`, so the field's own \
         local shadows it in the struct-init shorthand (E0308)",
    ),
    (
        "_key",
        true,
        "both fields mint the same encoding companion `{other}_encoding` (`{base}`'s KEY encoding \
         and `{other}`'s VALUE encoding), so the encoding struct declares that field twice \
         (E0124/E0062)",
    ),
];

/// A graceful-rejection message if `field_name` (already RESOLVED — post-`@name`, post-snake_case)
/// is a reserved generated-local name, else `None`. The message names the field, the rule and the
/// reserved word, and points at the `; @name <other>` remedy, which renames the Rust field WITHOUT
/// touching the CBOR wire key (probed: a bareword key `rawx` with `; @name payload` emits
/// `serializer.write_text("rawx")` and a `payload` struct field). Array-rep keys never reach the
/// wire at all, so `@name` is unconditionally safe there.
fn generated_local_field_rejection(
    field_name: &str,
    source_name: &str,
    rep: Representation,
    tagged: bool,
) -> Option<String> {
    let (word, _, evidence) = GENERATED_LOCAL_RESERVED
        .iter()
        .find(|(word, scope, _)| *word == field_name && scope.applies(rep, tagged))?;
    Some(format!(
        "rule `{source_name}`: field `{field_name}` is a reserved name — the generated \
         serialization code binds its own `{word}`: {evidence}. The field's local would shadow it \
         and the emitted crate would not compile. Rename the field with a `; @name <other>` comment \
         directive on that entry — the CBOR wire key is unchanged (a bareword/text key stays the \
         same text; array positions never put the name on the wire)."
    ))
}

#[allow(clippy::too_many_arguments)]
fn parse_record_from_group_choice(
    types: &mut IntermediateTypes,
    rep: Representation,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    group_choice: &GroupChoice,
    // Whether this record is one arm of a multi-arm group choice (`{ a } // { b }`). A rest row in
    // that position is rejected in v1 (collapsing an open map into an enum variant is unspecified),
    // so recognition is suppressed and the guard fires instead.
    in_choice_arm: bool,
    // Whether this record is the body of a TAGGED type (`#6.n([…])`). The tag read (`let tag =
    // raw.tag()?`) is emitted into this record's own deserializer, so it is the shape condition for
    // the `tag` entry of `GENERATED_LOCAL_RESERVED`.
    tagged: bool,
    cli: &Cli,
) -> RustRecord {
    let mut generated_fields = BTreeMap::<String, u32>::new();
    let flattened = flatten_group_entries(&group_choice.group_entries, rep);
    let entry_count = flattened.len();
    // Open struct-map recognition (loose CBOR): a trailing `* K => V` arrow row after ≥1 fixed
    // entry becomes the record's `rest` capture instead of a rejected mixed non-fixed key. A
    // single-entry `{ * K => V }` never reaches here — table detection in `parse_group_type`
    // diverts it — so any non-fixed entry here is part of a multi-entry map. `rest_skip` marks the
    // recognized (or CANDIDATE-then-rejected) rows so the field loop skips them.
    let DynamicRows {
        typed_row,
        rest,
        skip: rest_skip,
    } = recognize_dynamic_rows(
        types,
        rep,
        parent_visitor,
        name,
        &flattened,
        entry_count,
        in_choice_arm,
        cli,
    );
    let fields: Vec<RustField> = flattened
        .into_iter()
        .enumerate()
        .filter_map(|(index, (group_entry, optional_comma))| {
            // The dynamic-row entries (recognized, or rejected as a candidate) are handled by
            // `recognize_dynamic_rows`; never build a fixed field for one. An open table skips
            // BOTH of its rows, which is why this is an index SET rather than one index.
            if rest_skip.contains(&index) {
                return None;
            }
            // An unflattened `InlineGroup` reaching the record loop is a parenthesized group whose
            // own occurrence marker would be silently narrowed to exactly-once (`[* (int, tstr)]`,
            // `{ * (k: int) }`), or a bare multi-choice group in entry position. All three panic in
            // `group_entry_to_field_name` / `group_entry_to_type` / `group_entry_optional`; reject
            // gracefully here BEFORE they run, citing the rule's SOURCE spelling.
            if let GroupEntry::InlineGroup { occur, .. } = group_entry {
                let source_name = types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string());
                if occur.is_some() {
                    // the remedy differs by representation: naming the group only helps arrays —
                    // a plain-group reference inside a map record is itself unsupported (it hits
                    // the "map field has no key" rejection), so don't send map users there.
                    let remedy = match rep {
                        Representation::Array => {
                            "Name the group instead: `pair = (int, tstr)`, `a = [* pair]` — or \
                             drop the parentheses for a single-element group (`[* int]`)."
                        }
                        Representation::Map => {
                            "Use `?` on each field for optionality, or a table `{ * k => v }`."
                        }
                    };
                    types.record_rejection(format!(
                        "rule `{source_name}`: an occurrence marker on an inline group (`* (…)`) \
                         would be silently narrowed to exactly-once (generated decoders would \
                         reject valid CBOR with other repetition counts). {remedy}"
                    ));
                } else {
                    // The remedy names spellings that GENERATE. Lifting the alternatives into a
                    // named group (`g = (a // b)`) is NOT one of them — a group rule's body may
                    // carry only one choice (`multi_choice_group_def_rejection`) — so point at the
                    // container's own group choices, or at one named group per alternative.
                    types.record_rejection(format!(
                        "rule `{source_name}`: an inline group choice (`(a // b)`) in entry \
                         position is unsupported. Write the alternatives as the container's own \
                         group choices (`h = [ a: uint // f: bytes ]`), or give each alternative \
                         its own single-choice group rule and reference those as separate arms \
                         (`pga = (a: uint)`, `pgf = (f: bytes)`, `h = [ pga // pgf ]`)."
                    ));
                }
                return None;
            }
            // For a map record, classify the member key BEFORE field naming: only uint/text fixed
            // keys are implemented (the map-key write path and, under --preserve-encodings,
            // `key_encoding_field`), and `group_entry_to_field_name` PANICS at parsing.rs:1278 on
            // non-uint Type1 (arrow) member keys — so an unsupported key must be rejected here,
            // before naming runs. `group_entry_map_key_kind` never panics.
            let map_key = if rep == Representation::Map {
                match group_entry_map_key_kind(group_entry) {
                    // supported: carry the classified key forward (no separate key lookup needed).
                    MapKeyKind::Fixed(key @ (FixedValue::Uint(_) | FixedValue::Text(_))) => {
                        Some(key)
                    }
                    // cite the rule by its SOURCE spelling (`neg`), not the camel-cased RustIdent —
                    // the user is looking at their CDDL, not our output.
                    MapKeyKind::Fixed(other) => {
                        let source_name = types
                            .source_rule_name(name)
                            .map(str::to_owned)
                            .unwrap_or_else(|| name.to_string());
                        // The table remedy must not be advertised for a FLOAT key: a float-family
                        // table key domain is itself rejected (floats have no total order, so they
                        // cannot key a BTreeMap) — pointing there would send the user to a second
                        // rejection instead of a fix.
                        let remedy = if matches!(other, FixedValue::Float(_)) {
                            "Floats cannot key a map in either form (a float table key domain is \
                             rejected too) — use an integer or text key."
                        } else {
                            "Use a uint or text key, or a table `{ * k => v }` in its own rule."
                        };
                        types.record_rejection(format!(
                            "rule `{source_name}`: unsupported fixed map key {other:?} — only uint \
                             and text fixed keys are implemented on the record path (the map-key \
                             write path and `{{name}}_key_encoding`). {remedy}"
                        ));
                        return None;
                    }
                    MapKeyKind::NonFixed => {
                        // A non-fixed arrow entry (`* k => v` / `k => v`) in a map record is owned by
                        // `recognize_rest_row` (run before this loop): a supported trailing `* k => v`
                        // becomes the record's rest capture, and every unsupported placement/shape
                        // already recorded a graceful rejection there. Either way the entry never
                        // becomes a fixed field — skip it here without a second (duplicate) rejection.
                        return None;
                    }
                    // keyless: fall through — the existing "map field has no key" rejection below
                    // (which needs the field name) handles it exactly as before.
                    MapKeyKind::Keyless => None,
                }
            } else {
                None
            };
            let field_name = group_entry_to_field_name(
                group_entry,
                index,
                &mut generated_fields,
                optional_comma,
            );
            // A field whose EMITTED identifier is a Rust keyword (a bareword `if` key, or `If` which
            // snake_cases to `if`) would emit invalid Rust caught only by the rustfmt gate. Reject it
            // gracefully at parse time in BOTH representations (the array shape `[if: uint]` is equally
            // affected). `field_name` is already the snake_cased emitted form, so checking it directly
            // catches the case-converted hazards too. The remedy renames the field without touching
            // the CBOR wire key (which stays the bareword text).
            if RUST_KEYWORDS.contains(&field_name.as_str()) {
                let source_name = types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string());
                types.record_rejection(format!(
                    "rule `{source_name}`: field `{field_name}` is a Rust keyword and cannot be a \
                     struct field identifier. Rename the field with a `; @name <other>` comment \
                     directive on that entry — the CBOR wire key is unchanged (it stays the bareword \
                     text)."
                ));
                return None;
            }
            // A field whose EMITTED identifier is one of the fixed locals the generated
            // serialization bodies bind (`raw`, `len`, `read`, …) shadows that local: the crate
            // generates at exit 0 and fails `cargo check` two build steps from this CDDL line.
            // Checked on the RESOLVED name for the same reason the keyword guard above is — `Raw:`
            // snake_cases to `raw` and `; @name raw` renames INTO the hazard, while
            // `raw: bytes ; @name raw2` renames OUT of it and must pass.
            if let Some(msg) = generated_local_field_rejection(
                &field_name,
                &types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string()),
                rep,
                tagged,
            ) {
                types.record_rejection(msg);
                return None;
            }
            let rule_metadata = group_entry_rule_metadata(group_entry, optional_comma);
            // The RULE-SCOPED directives, refused at this member position by the shared seam the
            // single-entry group-choice arm also calls — that arm is a member position which mints
            // no record, so without one shared list the two spellings of "which directives does a
            // member position refuse" drift apart.
            // A plain GROUP rule's LAST entry is the one member slot the parser also binds the
            // RULE's trailing comment to (`pg = (a: uint, b: uint) ; @used_as_key` is the group
            // rule's documented directive slot, honored at rule level), so the type-scoped half is
            // suppressed exactly there and nowhere else — including non-last entries of the same
            // group, which no rule reading reaches.
            let rule_slot_shared = types.is_plain_group(name) && index + 1 == entry_count;
            reject_member_scoped_directives(
                types,
                &format!(
                    "field `{field_name}` of rule `{}`",
                    source_rule_name_of(types, name)
                ),
                "a field",
                &rule_metadata,
                rule_slot_shared,
            );
            // A wire-facts declaration (`@custom_encodings` / `@custom_wire_major`) is a property OF
            // the pair, and a field carries its own pair — so a declaration here with one half (or
            // none) describes no codec.
            if rule_metadata.custom_encodings.is_some() || rule_metadata.custom_wire_major.is_some()
            {
                let source_name = types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string());
                reject_custom_encodings_without_pair(
                    types,
                    &format!("field `{field_name}` of rule `{source_name}`"),
                    &rule_metadata,
                );
                // A field can carry the codec pair and its encoding tuple, but no reader consumes a
                // declared wire MAJOR there: major dispatch exists only before an open TABLE typed
                // row's key deserializer runs. Without this refusal the token parsed successfully
                // and then vanished from emitted code.
                if rule_metadata.custom_wire_major.is_some()
                    && rule_metadata.custom_serialize.is_some()
                    && rule_metadata.custom_deserialize.is_some()
                {
                    types.record_rejection(format!(
                        "@custom_wire_major on field `{field_name}` of rule `{source_name}`: nothing consumes the declared major. It is read only when a transparent alias rule keys an OPEN TABLE's typed row. Put the pair and declaration on a named key alias (`<key> = <inner> ; @custom_serialize <fn> @custom_deserialize <fn> @custom_wire_major <major>`), or remove the declaration."
                    ));
                }
            }
            // A LONE half of the pair at a field/member position — the field twin of the record-rule
            // and transparent-alias single-half rejections, refused for their stated reason: one
            // position ends up with two wire forms. `generate_serialize`/`generate_deserialize` lift
            // each half independently, so the declared direction routes the named function while the
            // opposite direction keeps the FIELD TYPE's own generated codec, and the crate compiles
            // and ships that asymmetry silently. The complete pair stays accepted — it owns both
            // directions of this field. (A rule-TRAILING comment on a plain-group rule binds to that
            // group's last entry, the `@extern_companions` neighbour's seam, so this names the entry
            // the comment actually reached rather than the rule the author wrote it after.)
            if let Some((directive, declared, kept, missing)) = match (
                &rule_metadata.custom_serialize,
                &rule_metadata.custom_deserialize,
            ) {
                (Some(_), None) => Some((
                    "@custom_serialize",
                    "serialize path writes through the named function",
                    "deserialize path keeps",
                    "@custom_deserialize",
                )),
                (None, Some(_)) => Some((
                    "@custom_deserialize",
                    "deserialize path reads through the named function",
                    "serialize path keeps",
                    "@custom_serialize",
                )),
                _ => None,
            } {
                let source_name = types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string());
                types.record_rejection(format!(
                    "{directive} alone on field `{field_name}` of rule `{source_name}`: the field's \
                     {declared} while its {kept} the field type's own generated codec — so the bytes \
                     this field writes are not the bytes it reads back. Write both halves on this \
                     entry (`; @custom_serialize <fn> @custom_deserialize <fn>`), adding the missing \
                     {missing}, or move the pair to the member's TYPE rule if the format belongs to \
                     the type."
                ));
            }
            // does not exist for fixed values importantly
            let mut field_type = group_entry_to_type(types, parent_visitor, group_entry, cli);
            // Resolve aliases before stamping: an alias is transparent (one wire form, a value IS
            // the aliased type), so a field spelled through one (`t = [ c: uint, kv_alias ]`) must
            // materialize the plain group exactly like the direct `kv` reference does. Matching the
            // bare `Rust(ident)` only left the group unregistered while `is_basic` — which DOES
            // shallow-resolve — still selected the splicing emission downstream, so the run aborted
            // on a struct that was never defined. Same pattern as the `WrappedBasicGroup` arm in
            // `rust_type_from_type2`.
            if let ConceptualRustType::Rust(ident) =
                field_type.conceptual_type.resolve_alias_shallow()
            {
                types.set_rep_if_plain_group(parent_visitor, ident, rep, cli);
            }
            let optional_field = group_entry_optional(group_entry);
            // A count-permitting occurrence (`*`, `+`, `n*m` with bounds ≠ 1*1) on an ARRAY-record
            // field would be silently narrowed to a single mandatory item — a generated decoder
            // that rejects spec-valid CBOR with any other repetition count (invisible to
            // round-trip tests; only cross-producer data exposes it — the array analogue of the
            // map-path guard below). Unlike unique map keys, `+` does not collapse to exactly-one
            // in an array, so every marker except `?` and the pedantic `1*1` rejects.
            if rep == Representation::Array {
                let narrows = match group_entry {
                    GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref(),
                    GroupEntry::TypeGroupname { ge, .. } => ge.occur.as_ref(),
                    GroupEntry::InlineGroup { .. } => None,
                }
                .map(|o| {
                    !matches!(
                        o.occur,
                        Occur::Optional { .. }
                            | Occur::Exact {
                                lower: Some(1),
                                upper: Some(1),
                                ..
                            }
                    )
                })
                .unwrap_or(false);
                if narrows {
                    let source_name = types
                        .source_rule_name(name)
                        .map(str::to_owned)
                        .unwrap_or_else(|| name.to_string());
                    types.record_rejection(format!(
                        "rule `{source_name}`: array field `{field_name}` has an occurrence \
                         (`*` / `+` / `n*m`), which would be silently narrowed to a single \
                         mandatory item (generated decoders would reject valid CBOR with a \
                         different repetition count). Use `?` for an optional item, a final-position \
                         `* t` rest tail after the fixed members, a homogeneous array (`[* t]`), or \
                         name the repeated part as its own array rule."
                    ));
                    return None;
                }
                // An OPTIONAL (`?`) plain-group field in an ARRAY-rep record. A plain group SPLICES
                // its members flat into the enclosing array, so nothing on the wire marks where the
                // optional group begins, and the embedded decoder length-checks only the members it
                // consumed — telling present from absent needs the group's mandatory member count
                // charged to the ENCLOSING read length before the group is read (either that, or a
                // second embedded deserialize method). That is the occurrence/bounds program's
                // territory, not a guard's; until it lands the shape must not reach emission, where
                // it aborted on `assertion failed: !config.optional_field` naming neither the
                // construct nor a remedy. The named-array remedy IS verified to generate, which is
                // what makes a refusal honest here.
                //
                // Guarded on `is_basic` over the RESOLVED member type, the same predicate and the
                // same one-seam placement as the map twin below: that is what makes the bare, the
                // ALIAS (`? kv_alias`) and the TAGGED (`? #6.1(kv)`) spellings hit ONE message. The
                // tagged one is not a widening — it exited 0 emitting a codec whose own decoder
                // rejects its own bytes with a definite-length mismatch. Deliberately blanket over
                // the group's own shape: a group whose members are ALL optional is reachable and
                // still refused, because the remedy serves it identically and a narrower guard would
                // buy a special case nothing has asked for. The array-WRAPPED forms keep their own
                // verdicts — `w = [kv]` is a Record, not a plain group, and an inline `[kv]` member
                // carries `basic_override` — so both fall outside `is_basic` untouched.
                if optional_field
                    && field_type.is_basic(types)
                    && let ConceptualRustType::Rust(group_ident) =
                        field_type.conceptual_type.resolve_alias_shallow()
                {
                    let source_name = types
                        .source_rule_name(name)
                        .map(str::to_owned)
                        .unwrap_or_else(|| name.to_string());
                    let group_name = types
                        .source_rule_name(group_ident)
                        .map(str::to_owned)
                        .unwrap_or_else(|| group_ident.to_string());
                    types.record_rejection(format!(
                        "rule `{source_name}`: array field `{field_name}` is an OPTIONAL (`?`) \
                         reference to the plain group `{group_name}`, which is unsupported — a plain \
                         group splices its members flat into the enclosing array, so nothing on the \
                         wire marks where the optional group starts, and an embedded decoder \
                         length-checks only the members it consumed. Telling present from absent \
                         would need the group's mandatory member count charged to the enclosing \
                         read length before the group is read. Give the group its own array framing \
                         and reference that, which makes the optional item exactly ONE array element \
                         the decoder can test for: `w = [{group_name}]`, then `? w` in place of \
                         `? {group_name}`. (Dropping the `?` — splicing the group as a MANDATORY \
                         field — is supported as it stands.)"
                    ));
                    return None;
                }
            }
            let key = match rep {
                Representation::Map => {
                    // cite the rule by its SOURCE spelling (`m`), not the camel-cased
                    // RustIdent (`M`) — the user is looking at their CDDL, not our output
                    let source_name = types
                        .source_rule_name(name)
                        .map(str::to_owned)
                        .unwrap_or_else(|| name.to_string());
                    // `map_key` was classified before field naming (unsupported/non-fixed keys
                    // already returned None); `Some` is a supported uint/text key, `None` is a
                    // keyless entry that falls to the "map field has no key" rejection below.
                    match map_key {
                        Some(key) => {
                            // A ZERO-permitting occurrence (`*`, `0*n`, `*n`) on a keyed map field
                            // means the entry may be ABSENT (RFC 8610), but the record path would
                            // silently narrow it to a MANDATORY field — a generated decoder that
                            // rejects valid CBOR omitting the entry (invisible to round-trip tests;
                            // only cross-producer data exposes it). Reject gracefully instead.
                            // Lower bounds >= 1 (`+`, `1*2`) stay mandatory: under unique map keys
                            // they collapse to exactly-one, so mandatory IS the honored semantics.
                            let permits_zero = match group_entry {
                                GroupEntry::ValueMemberKey { ge, .. } => matches!(
                                    ge.occur.as_ref().map(|o| &o.occur),
                                    Some(Occur::ZeroOrMore { .. })
                                        | Some(Occur::Exact { lower: None, .. })
                                        | Some(Occur::Exact { lower: Some(0), .. })
                                ),
                                _ => false,
                            };
                            if permits_zero {
                                types.record_rejection(format!(
                                    "rule `{source_name}`: map field `{field_name}` has a \
                                     zero-permitting occurrence (`*` / `0*n` / `*n`), which would \
                                     be silently narrowed to a mandatory field (generated decoders \
                                     would reject valid CBOR that omits it). Use `?` for an \
                                     optional field, or a table `{{ * k => v }}`."
                                ));
                                return None;
                            }
                            // A keyed member whose type resolves to a plain group can only be
                            // emitted as a flat splice, which writes more items than the key's own
                            // entry promised — refuse every spelling of it here, at the one seam
                            // the named / tagged / optional / alias / multi-entry-choice-arm
                            // members all pass through. `is_basic` is the same predicate
                            // `generate_serialize` uses to pick the splicing emission, so an
                            // array-WRAPPED group (`c: [kv]`, `basic_override`) keeps its own
                            // conflicting-representations refusal and the named-array remedy
                            // (`w = [kv]`, `c: w`) stays green.
                            if field_type.is_basic(types)
                                && let ConceptualRustType::Rust(group_ident) =
                                    field_type.conceptual_type.resolve_alias_shallow()
                            {
                                let group_name = types
                                    .source_rule_name(group_ident)
                                    .map(str::to_owned)
                                    .unwrap_or_else(|| group_ident.to_string());
                                record_plain_group_map_member_rejection(
                                    types,
                                    &format!("rule `{source_name}`"),
                                    &field_name,
                                    &group_name,
                                );
                                return None;
                            }
                            Some(key)
                        }
                        // A map-representation field without a key is unsupported by design (each
                        // map field needs a key). This also catches a plain-group reference embedded
                        // in a map record, which surfaces here as a keyless `TypeGroupname`. Record a
                        // graceful rejection (drained by `finalize`) and drop the field rather than
                        // `panic!` — nothing downstream runs on this record once a rejection exists.
                        None => {
                            types.record_rejection(format!(
                                "rule `{source_name}`: map field `{field_name}` has no key. Each map field \
                                 needs a key: use `k: v` / `k => v`, or a table `{{ * k => v }}`. \
                                 (A plain-group reference embedded in a map-representation record hits \
                                 this too — it is unsupported today.)"
                            ));
                            return None;
                        }
                    }
                }
                Representation::Array => None,
            };
            // RFC 8610 §3.8.2: a `.default` is what a decoder substitutes when the member is
            // ABSENT, so it is meaningful only for an OPTIONAL occurrence — on a mandatory member
            // there is no absent case for it to fill. Drop it here, at the one seam where a
            // field's optionality is known (and the seam a plain group's spliced entries arrive
            // through), so a mandatory member emits as a PLAIN mandatory field on every face:
            // the rust `new()` keeps its argument, and the wasm and WIT constructors that mirror
            // `new()` stay in agreement with it. (Left in place, the inert control moved the field
            // out of `new()` on the rust face only, and the mirrored constructors called it with
            // an argument it no longer took.)
            //
            // A warning rather than a refusal, because the default may be legitimately CARRIED
            // rather than spelled: `d = uint .default 0` is well-formed and useful at its optional
            // use sites (`? y: d`), while a mandatory `x: d` reference picks the same type up. This
            // seam cannot tell the two apart, and warning on both is the honest reading — the
            // control is inert either way.
            if !optional_field && field_type.config.default.is_some() {
                let source_name = types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string());
                crate::warn!(
                    "rule `{source_name}`: `.default` on the mandatory member `{field_name}` has \
                     no effect (RFC 8610: a default substitutes for an ABSENT value, which is \
                     meaningful only for an optional occurrence) — ignored. Mark the member \
                     optional (`? {field_name}: …`) if the default was meant to apply."
                );
                field_type.config.default = None;
            }
            Some(RustField::new(
                field_name,
                field_type,
                optional_field,
                key,
                rule_metadata,
            ))
        })
        .collect();
    reject_encoding_companion_collisions(types, rep, name, &fields);
    RustRecord {
        rep,
        fields,
        rest,
        typed_row,
    }
}

/// The PAIRWISE half of the generated-local collision class: a record whose fields are individually
/// fine but whose NAMES stand in an `<f>` / `<f>_encoding` relation collide once
/// `--preserve-encodings` mints the per-field encoding companions (see
/// `ENCODING_COMPANION_SUFFIXES` for the three measured spellings and their error classes). Checked
/// on the RESOLVED names, after the whole field list is built, and uniformly across profiles — the
/// default profile compiles only because it mints no companions at all, so accepting the pair there
/// would hand back a spec that one flag breaks.
fn reject_encoding_companion_collisions(
    types: &mut IntermediateTypes,
    rep: Representation,
    name: &RustIdent,
    fields: &[RustField],
) {
    let mut collisions = Vec::new();
    for base in fields.iter() {
        for (suffix, map_only, why) in ENCODING_COMPANION_SUFFIXES {
            if *map_only && rep != Representation::Map {
                continue;
            }
            let companion = format!("{}{suffix}", base.name);
            if fields.iter().any(|f| f.name == companion) {
                collisions.push((
                    base.name.clone(),
                    companion,
                    why.replace("{base}", &base.name),
                ));
            }
        }
    }
    if collisions.is_empty() {
        return;
    }
    let source_name = types
        .source_rule_name(name)
        .map(str::to_owned)
        .unwrap_or_else(|| name.to_string());
    for (base, other, why) in collisions {
        let why = why.replace("{other}", &other);
        types.record_rejection(format!(
            "rule `{source_name}`: fields `{base}` and `{other}` collide under \
             `--preserve-encodings` — {why} — so the emitted crate does not compile. Rename one of \
             them with a `; @name <other>` comment directive on that entry — the CBOR wire key is \
             unchanged."
        ));
    }
}

/// The DYNAMIC (non-fixed) rows a record recognized, plus the flattened indices its fixed-field
/// loop must skip. One row for an open struct-map / open array, TWO for an open table, none for a
/// closed struct. `skip` names every CANDIDATE row — recognized or gracefully rejected — so a
/// rejected row never also becomes a bogus fixed field.
struct DynamicRows {
    typed_row: Option<Box<RestRow>>,
    rest: Option<Box<RestRow>>,
    skip: Vec<usize>,
}

/// Route a record's non-fixed rows to the OPEN TABLE recognizer (`t = { * K_t => V_t, * K_r => V_r }`
/// — two dynamic rows and no fixed key) or to the single-trailing-rest-row recognizer (everything
/// else). The two shapes are disjoint by construction, so this is a pure fork: an open table is not
/// an open struct-map with an extra row, it is a rule of its own kind (ZERO fixed fields, a typed
/// row claiming one wire major and a catch-all seeing the complement).
#[allow(clippy::too_many_arguments)]
fn recognize_dynamic_rows(
    types: &mut IntermediateTypes,
    rep: Representation,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    flattened: &[&(GroupEntry, OptionalComma)],
    entry_count: usize,
    in_choice_arm: bool,
    cli: &Cli,
) -> DynamicRows {
    if rep == Representation::Map
        && entry_count == 2
        && flattened
            .iter()
            .all(|(ge, _)| matches!(group_entry_map_key_kind(ge), MapKeyKind::NonFixed))
    {
        return recognize_open_table(types, parent_visitor, name, flattened, in_choice_arm, cli);
    }
    let (rest, rest_index) = recognize_rest_row(
        types,
        rep,
        parent_visitor,
        name,
        flattened,
        entry_count,
        in_choice_arm,
        cli,
    );
    DynamicRows {
        typed_row: None,
        rest,
        skip: rest_index.into_iter().collect(),
    }
}

/// Recognize an OPEN TABLE — a NAMED rule spelled `t = { * K_t => V_t, * K_r => V_r }`: one typed
/// table row plus one trailing typed catch-all rest row, and nothing else. The typed row claims
/// exactly its key's single statically-known CBOR major; the catch-all sees only the complement.
///
/// Only the SHAPE is decided here. Whether `K_t`'s major is statically knowable at all — the
/// two-stage staticness rule, and the `@custom_wire_major` declaration a custom-codec key needs — is
/// decided in `IntermediateTypes::finalize`, because it needs `cbor_types()` (which panics on an
/// unregistered ident) and must run after generic resolution. Parse decides SHAPE, finalize decides
/// STATICNESS.
///
/// Both rows are `RestRow`s: the typed row is a dynamic sequence in exactly the sense the delivered
/// capture engine already handles (its own `@duplicates`, its own container, its own encoding
/// sidecars), so it reuses that machinery verbatim rather than minting a new struct kind.
fn recognize_open_table(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    flattened: &[&(GroupEntry, OptionalComma)],
    in_choice_arm: bool,
    cli: &Cli,
) -> DynamicRows {
    let skip = vec![0usize, 1usize];
    let rejected = || DynamicRows {
        typed_row: None,
        rest: None,
        skip: vec![0usize, 1usize],
    };
    let src = source_rule_name_of(types, name);
    // An INLINE anonymous open table (`f: { * k1 => v1, * k2 => v2 }`) is rejected: the shape mints a
    // struct with two container members and a keys-list wrapper, all named off the rule ident, and a
    // synthesized structural name for those would be a new name family with no user-visible source
    // spelling. The named-rule concession is also what keeps the wasm collision story to legs on the
    // existing detectors rather than a new sibling. Point at the named-rule form.
    if !types.is_toplevel_rule(name) {
        types.record_rejection(format!(
            "rule `{src}`: an INLINE open table (`f: {{ * k1 => v1, * k2 => v2 }}`) is unsupported. \
             Give the open table its own named rule (`t = {{ * k1 => v1, * k2 => v2 }}`) and \
             reference it by name — the generated struct, its two containers and its keys list are \
             all named off that rule."
        ));
        return rejected();
    }
    // A group-choice arm and a plain group are rejected for the same reasons the single rest row is
    // (an arm collapses into an enum variant, dropping the open semantics; a materialized plain group
    // exports transparently as a CLOSED group body across a crate boundary).
    if in_choice_arm {
        types.record_rejection(format!(
            "rule `{src}`: an open table (`{{ * k1 => v1, * k2 => v2 }}`) inside a group-choice arm \
             (`{{ … }} // {{ … }}`) is unsupported. Give the open table its own named rule and \
             reference it from the arm."
        ));
        return rejected();
    }
    if types.is_plain_group(name) {
        types.record_rejection(format!(
            "rule `{src}`: an open table (`* k1 => v1, * k2 => v2`) inside a plain group (`{src} = \
             ( … )`, embedded elsewhere) is unsupported. Give the open table its own named rule \
             (`{src} = {{ * k1 => v1, * k2 => v2 }}`) and reference it by name."
        ));
        return rejected();
    }
    let Some(typed) = open_table_row(
        types,
        parent_visitor,
        &src,
        flattened[0],
        OpenTableRowKind::Typed,
        cli,
    ) else {
        return rejected();
    };
    let Some(catch_all) = open_table_row(
        types,
        parent_visitor,
        &src,
        flattened[1],
        OpenTableRowKind::CatchAll,
        cli,
    ) else {
        return rejected();
    };
    // The two rows become two `pub` fields on one struct, so their names must differ. Only reachable
    // by `@name`-ing one row onto the other's name (the defaults are distinct).
    if typed.field_name == catch_all.field_name {
        types.record_rejection(format!(
            "rule `{src}`: the open table's typed row and catch-all row would both emit a field \
             named `{}` — the two rows are two separate containers on one struct, so their names \
             must differ. Rename one with a `; @name <other>` directive on that row.",
            typed.field_name
        ));
        return rejected();
    }
    DynamicRows {
        typed_row: Some(Box::new(typed)),
        rest: Some(Box::new(catch_all)),
        skip,
    }
}

/// Which of an open table's two rows is being built — they differ in the occurrence they accept
/// (only the TYPED row takes `+`/`1*`, the NonEmpty twin's spelling, because the min-1 counts typed
/// entries), in their default field name, and in every rejection message's wording.
#[derive(Copy, Clone, PartialEq, Eq)]
enum OpenTableRowKind {
    Typed,
    CatchAll,
}

impl OpenTableRowKind {
    /// The slot's name in rejection messages.
    fn slot(self) -> &'static str {
        match self {
            OpenTableRowKind::Typed => "open table typed row (`* k1 => v1`)",
            OpenTableRowKind::CatchAll => "open table catch-all row (`* k2 => v2`)",
        }
    }

    /// The captured field's default Rust name (`@name`-overridable). The catch-all keeps the open
    /// struct-map's `rest` so the two capture surfaces read alike across the two shapes.
    fn default_field_name(self) -> &'static str {
        match self {
            OpenTableRowKind::Typed => "entries",
            OpenTableRowKind::CatchAll => "rest",
        }
    }
}

/// Build ONE row of an open table from its group entry, or record a graceful rejection and return
/// `None`. Shape-level only (see `recognize_open_table`).
fn open_table_row(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    src: &str,
    entry: &(GroupEntry, OptionalComma),
    kind: OpenTableRowKind,
    cli: &Cli,
) -> Option<RestRow> {
    let (ge_entry, comma) = entry;
    let slot = kind.slot();
    let occur = match ge_entry {
        GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
        _ => None,
    };
    // The row's occurrence marker, read exactly as a TABLE's is (`parsing.rs`'s inline-map arm): `*`
    // and `0*` are the same unbounded row, `+` and `1*` the same min-1 row. Every other marker names
    // a real bounded cardinality this shape does not honor, and widening it to 0..N would silently
    // over-accept.
    let bound = match occur {
        Some(Occur::ZeroOrMore { .. }) => Some(false),
        Some(Occur::OneOrMore { .. }) => Some(true),
        Some(Occur::Exact { lower, upper, .. }) if upper.is_none() => match lower {
            None | Some(0) => Some(false),
            Some(1) => Some(true),
            _ => None,
        },
        _ => None,
    };
    let Some(non_empty) = bound else {
        types.record_rejection(format!(
            "rule `{src}`: the {slot} must use the `*` occurrence (unbounded: `* k => v`) — or, on \
             the typed row only, `+` (at least one TYPED entry). `n*m`, `*n`, `n*` with n≥2, and \
             `?` are not supported on an open table's rows."
        ));
        return None;
    };
    // The min-1 bound counts TYPED entries only (a map of purely captured entries is not a non-empty
    // table), so it is a statement about the typed row and has no reading on the catch-all: a
    // `{ * k1 => v1, + k2 => v2 }` would demand at least one entry no arm of the rule is about.
    if non_empty && kind == OpenTableRowKind::CatchAll {
        types.record_rejection(format!(
            "rule `{src}`: the `+` occurrence is supported only on an open table's TYPED row \
             (`{{ + k1 => v1, * k2 => v2 }}`), where the minimum of 1 counts TYPED entries. On the \
             catch-all row it would demand at least one entry the rule says nothing about. Use `*` \
             on the catch-all row."
        ));
        return None;
    }
    let (domain, range) = match ge_entry {
        GroupEntry::ValueMemberKey { ge, .. } => {
            let domain = match &ge.member_key {
                Some(MemberKey::Type1 { t1, .. }) => {
                    rust_type_from_type1(types, parent_visitor, t1, cli)
                }
                _ => {
                    types.record_rejection(format!(
                        "rule `{src}`: unsupported {slot} key spelling (expected `* k => v`)."
                    ));
                    return None;
                }
            };
            let range = rust_type(types, parent_visitor, &ge.entry_type, cli);
            (domain, range)
        }
        _ => {
            types.record_rejection(format!(
                "rule `{src}`: unsupported {slot} spelling (expected `* k => v`)."
            ));
            return None;
        }
    };
    // A null-admitting key domain collides with the break that ends an indefinite-length map (both
    // are CBOR major 7) — the same reason the open struct-map rest row rejects it.
    if matches!(
        domain.conceptual_type.resolve_alias_shallow(),
        ConceptualRustType::Optional(_)
    ) {
        types.record_rejection(format!(
            "rule `{src}`: the {slot} cannot take a null-admitting key domain (`* (t / null) => \
             v`): a `null` key and the break that ends an indefinite-length map are both CBOR \
             special values, so the row's key dispatch cannot tell them apart. Drop the `null` arm \
             from the key type."
        ));
        return None;
    }
    // A bare `any` TYPED key is a shape error, not a staticness one: it would claim all eight
    // majors, leaving the catch-all nothing to see. (The catch-all is exactly the position `any`
    // belongs in.)
    if kind == OpenTableRowKind::Typed
        && matches!(
            domain.conceptual_type.resolve_alias_shallow(),
            ConceptualRustType::Any
        )
    {
        types.record_rejection(format!(
            "rule `{src}`: the {slot} cannot be keyed on `any` — the typed row claims exactly one \
             CBOR major type and `any` admits all eight, so the catch-all row would never see an \
             entry. Key the typed row on a concrete type and let the catch-all take `any`."
        ));
        return None;
    }
    let metadata = group_entry_rule_metadata(ge_entry, comma);
    if reject_custom_codec_on_row_entry(
        types,
        &format!("{slot} of rule `{src}`"),
        "Name the row's key or value type as its own rule and put the pair there (`k = text ; \
         @custom_serialize <fn> @custom_deserialize <fn>`, then `* k => v`).",
        &metadata,
    ) {
        return None;
    }
    if reject_custom_encodings_without_pair(
        types,
        &format!("the {slot} of rule `{src}`"),
        &metadata,
    ) {
        return None;
    }
    // `@ignore` (tolerate-and-drop) has no meaning on either row of an open table: the whole rule IS
    // its two containers, so ignoring one leaves a struct that silently drops half the map — and
    // ignoring the typed one leaves a rule with nothing typed about it.
    if metadata.ignore {
        types.record_rejection(format!(
            "rule `{src}`: `@ignore` (tolerate-and-drop) is not supported on the {slot} — an open \
             table's rows ARE the rule's content, so dropping one would silently discard half the \
             map. Drop the `@ignore` to capture both rows (the default), or use an open struct-map \
             (`{{ 1: a, * k => v ; @ignore }}`) if you want the drop."
        ));
        return None;
    }
    let field_name = metadata
        .name
        .clone()
        .unwrap_or_else(|| kind.default_field_name().to_owned());
    Some(RestRow {
        kind: RestKind::MapEntries {
            domain,
            range,
            duplicates: metadata.duplicates,
        },
        semantics: RestSemantics::Capture,
        field_name,
        // Derived in `finalize` for the typed row (see the field doc); the catch-all never has one.
        dispatch_major: None,
        non_empty,
    })
}

/// Recognize a trailing open-map rest row (`* K => V`) in a map-rep record, or reject an
/// unsupported placement/shape gracefully. Returns the built `RestRow` (if recognized and every
/// guard passes) and the flattened index of the rest-CANDIDATE row (so the caller's field loop
/// skips it — whether recognized or rejected). Non-map reps and maps with no non-fixed entry
/// return `(None, None)`.
#[allow(clippy::too_many_arguments)]
fn recognize_rest_row(
    types: &mut IntermediateTypes,
    rep: Representation,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    flattened: &[&(GroupEntry, OptionalComma)],
    entry_count: usize,
    in_choice_arm: bool,
    cli: &Cli,
) -> (Option<Box<RestRow>>, Option<usize>) {
    // The array analog of a map rest row is a final-position `* T` tail (`[a, b, * t]`), recognized by
    // a dedicated sibling (no keys → no key dispatch / duplicate policy / domain typing, so the
    // key-specific map body does not fit). Everything below is map-only.
    if rep == Representation::Array {
        return recognize_array_rest_tail(
            types,
            parent_visitor,
            name,
            flattened,
            entry_count,
            in_choice_arm,
            cli,
        );
    }
    if rep != Representation::Map {
        return (None, None);
    }
    let source_name = || {
        types
            .source_rule_name(name)
            .map(str::to_owned)
            .unwrap_or_else(|| name.to_string())
    };
    let nonfixed_indices: Vec<usize> = flattened
        .iter()
        .enumerate()
        .filter(|(_, (ge, _))| matches!(group_entry_map_key_kind(ge), MapKeyKind::NonFixed))
        .map(|(i, _)| i)
        .collect();
    let Some(&candidate) = nonfixed_indices.last() else {
        // No non-fixed entry: an ordinary closed struct. Byte-identical to pre-feature output.
        return (None, None);
    };
    let src = source_name();
    // A rest row cannot be collapsed into an enum variant (it would drop the open-map semantics),
    // so reject it in a group-choice arm; the row is still skipped so no fixed field is built.
    if in_choice_arm {
        types.record_rejection(format!(
            "rule `{src}`: an open struct-map rest row (`* k => v`) inside a group-choice arm \
             (`{{ … }} // {{ … }}`) is unsupported. Give the open map its own named rule and \
             reference it from the arm."
        ));
        return (None, Some(candidate));
    }
    // A rest row inside a PLAIN GROUP (`g = ( 1: a, * k => v )`, embedded via `{ g }`) is rejected.
    // The rest row is NOT a `RustField`, but a materialized plain
    // group exports TRANSPARENTLY as an extern-interface group-body row rendered from `fields` only
    // (`project_plain_group`) — so recognizing a rest here would silently project a CLOSED group
    // across the crate boundary (the silent-lossy cross-crate class). Reject explicitly instead of
    // relying on the incidental "map field has no key" embed rejection; point at the named-rule form.
    if types.is_plain_group(name) {
        types.record_rejection(format!(
            "rule `{src}`: an open struct-map rest row (`* k => v`) inside a plain group \
             (`{src} = ( … * k => v )`, embedded elsewhere) is unsupported. Give the open map its \
             own named rule (`{src} = {{ … * k => v }}`) and reference it by name."
        ));
        return (None, Some(candidate));
    }
    // Multiple non-fixed rows: only a single trailing rest row is supported.
    if nonfixed_indices.len() > 1 {
        types.record_rejection(format!(
            "rule `{src}`: a map supports at most a single trailing rest row (`* k => v`) after \
             its fixed keys, or — with NO fixed keys — an open table's two rows (`{{ * k1 => v1, * \
             k2 => v2 }}`: one typed row plus one trailing catch-all). This map has {} non-fixed \
             rows. Keep one `* k => v` row (last), drop the fixed keys to spell an open table, or \
             move the extras into their own table rules.",
            nonfixed_indices.len()
        ));
        return (None, Some(candidate));
    }
    // Non-final placement: the rest row must be the LAST entry (fixed keys are dispatched first).
    if candidate != entry_count - 1 {
        types.record_rejection(format!(
            "rule `{src}`: an open struct-map rest row (`* k => v`) must be the LAST entry of the \
             map (fixed keys are matched first, then the rest captures the remainder). Move it to \
             the end."
        ));
        return (None, Some(candidate));
    }
    // An open struct-map needs ≥1 fixed key before the rest row (a rest row IS the "open" part of an
    // open MAP). A single `* k => v` entry with nothing before it is a TABLE, which is recognized by
    // `parse_group_type` before this function ever runs — so a lone non-fixed entry here (e.g. an
    // alias-to-literal arrow key) is a degenerate shape, not an open struct.
    if candidate == 0 {
        types.record_rejection(format!(
            "rule `{src}`: an open struct-map rest row (`* k => v`) must follow at least one fixed \
             key (`{{ 1: a, * k => v }}`). A map whose only entry is `* k => v` is a table — give it \
             its own rule (`t = {{ * k => v }}`)."
        ));
        return (None, Some(candidate));
    }
    let (candidate_ge, candidate_comma) = flattened[candidate];
    // Occurrence must be exactly `*` (unbounded capture). `+` / `n*m` / `?` are rejected: "at least
    // one unknown entry" and other bounded cardinalities on a rest row are ill-specified and would
    // break the empty-rest ≡ closed-struct byte invariant.
    let occur = match candidate_ge {
        GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
        _ => None,
    };
    if !matches!(occur, Some(Occur::ZeroOrMore { .. })) {
        types.record_rejection(format!(
            "rule `{src}`: an open struct-map rest row must use the `*` occurrence (unbounded \
             capture: `* k => v`). `+`, `n*m`, and `?` are not supported on a rest row."
        ));
        return (None, Some(candidate));
    }
    // Extract the key (domain) and value (range) types from the arrow entry, keeping the SOURCE
    // spellings of both slots — the slot-shape rejections below print the author's own row back.
    let (domain, range, domain_src, range_src) = match candidate_ge {
        GroupEntry::ValueMemberKey { ge, .. } => {
            let (domain, domain_src) = match &ge.member_key {
                Some(MemberKey::Type1 { t1, .. }) => {
                    (rust_type_from_type1(types, parent_visitor, t1, cli), t1)
                }
                // A non-fixed key that is not a Type1 arrow (`NonMemberKey`) — unreachable for a
                // classified NonFixed arrow row, but reject rather than panic if it ever appears.
                _ => {
                    types.record_rejection(format!(
                        "rule `{src}`: unsupported rest-row key spelling (expected `* k => v`)."
                    ));
                    return (None, Some(candidate));
                }
            };
            let range = rust_type(types, parent_visitor, &ge.entry_type, cli);
            (domain, range, domain_src, &ge.entry_type)
        }
        _ => {
            types.record_rejection(format!(
                "rule `{src}`: unsupported rest-row spelling (expected `* k => v`)."
            ));
            return (None, Some(candidate));
        }
    };
    // A general key domain is supported: bare `uint`/`text`/`any` keep the fast peeked-key dispatch,
    // everything else takes the typed seek path (`RestRow::map_key_uses_peeked_path` routes them, and
    // is the ONE predicate parsing/IR/generation share). Three shapes stay rejected, for reasons the
    // slot type itself carries rather than the row's plumbing:
    //
    //   * a NULL-ADMITTING domain (`k = text / null` → `Optional<..>`), rejected here — a `null` key
    //     arrives as CBOR major type 7, the same dispatch arm that carries the indefinite-map BREAK,
    //     so the row cannot tell "the map ended" from "the next key is null" without deciding one of
    //     them wrong.
    //   * a FLOAT-containing domain, rejected in `IntermediateTypes::finalize` beside the table/set
    //     float instruments (floats have no total order, so they can key nothing) — the one place
    //     that also sees a float hidden behind a resolved generic instance.
    //   * a PLAIN GROUP in EITHER slot, rejected here — the only one of the three that also applies
    //     to the VALUE slot, because it is a property of the map ENTRY (each of its two slots holds
    //     exactly one item) rather than of key dispatch.
    //
    // Fixed-value domains (`* 5 => v`, or an alias to one) never reach here: the zero-permitting
    // occurrence guard and the bare-fixed-value rule guard reject them first.
    if matches!(
        domain.conceptual_type.resolve_alias_shallow(),
        ConceptualRustType::Optional(_)
    ) {
        types.record_rejection(format!(
            "rule `{src}`: an open struct-map rest row cannot take a null-admitting key domain (`* \
             (t / null) => v`): a `null` key and the break that ends an indefinite-length map are \
             both CBOR special values, so the row's key dispatch cannot tell them apart. Drop the \
             `null` arm from the key type (a missing entry already means absent)."
        ));
        return (None, Some(candidate));
    }
    // A plain group in EITHER slot — see `record_plain_group_rest_row_domain_rejection`. The
    // fixed-prefix sibling of the table twin's guard, sharing its `plain_group_table_domain`
    // predicate (`is_basic` over the RESOLVED type), so the bare (`* kv => uint`), the ALIAS and the
    // TAGGED (`* uint => #6.10(kv)`) spellings land on ONE message on every profile, and the
    // array-WRAPPED forms keep their supported verdicts (an inline `[kv]` carries `basic_override`,
    // so it is not `is_basic`). Both roles are reported when both offend — a silent slot would be
    // the worse failure — and the guard sits BEFORE the row's directive reads, because a directive
    // cannot be judged on a row that has no representable slot.
    let key_group = plain_group_table_domain(types, &domain);
    let value_group = plain_group_table_domain(types, &range);
    if key_group.is_some() || value_group.is_some() {
        let entry_src = format!("{domain_src} => {range_src}");
        if let Some(group_name) = key_group {
            record_plain_group_rest_row_domain_rejection(
                types,
                &src,
                &entry_src,
                "KEY",
                &group_name,
                &format!(
                    "{} => {range_src}",
                    array_wrapped_domain_src(&domain_src.to_string(), &domain_src.type2)
                ),
            );
        }
        if let Some(group_name) = value_group {
            record_plain_group_rest_row_domain_rejection(
                types,
                &src,
                &entry_src,
                "VALUE",
                &group_name,
                &format!(
                    "{domain_src} => {}",
                    match single_type2(range_src) {
                        Some(t2) => array_wrapped_domain_src(&range_src.to_string(), t2),
                        None => format!("[{range_src}]"),
                    }
                ),
            );
        }
        return (None, Some(candidate));
    }
    // Both open struct-map flavors are now wired end-to-end. CAPTURE (default) has every generated
    // surface: the JSON flattened rest surface (captured entries render at the same object level as
    // declared fields, with the write-side collision check and key-coercing read wrapper), the
    // --preserve-encodings / --canonical-form fidelity path, and the wasm rest accessor (a getter
    // returning the captured entries as the wasm map wrapper). IGNORE (`@ignore` on the row)
    // tolerate-and-drops: the deserialize arms typed-consume each unknown entry and discard it (no
    // field, serialize emits declared members only, JSON/schemars/wasm are a closed struct's), and it
    // is rejected under --preserve-encodings. No front door remains here for either flavor.
    // Read entry-level directives (`@name`, `@duplicates`, `@ignore`) from the rest row's own trailing
    // slot —
    // NOT rule-position handling, and the two slots are DISJOINT here: on a map TYPE rule the parser
    // binds a trailing comment written after the closing brace to the RULE slot alone
    // (`u = { 1: a, * k => v } ; @ignore` is refused as a rule-position `@ignore`, and this walk
    // never sees it), so anything read below is the comment the author put on the ROW's own line.
    // The dual-read slot is a plain GROUP rule's last entry, which is a different construct.
    let rest_metadata = group_entry_rule_metadata(candidate_ge, candidate_comma);
    // The TYPE-SCOPED directives describe the type a name denotes, so a row entry — which declares
    // no type of its own — reads none of them. The row's own honored family (`@name`, `@duplicates`,
    // `@ignore`) is why this seam calls the type-scoped list ALONE rather than the whole member list.
    // Placement/shape guards above fire FIRST, per this seam's convention.
    reject_type_scoped_directives(
        types,
        &format!("the open struct-map rest row (`* k => v`) of rule `{src}`"),
        &rest_metadata,
    );
    // A custom (de)serializer pair in this slot is inert (the row declares no type of its own) —
    // reject it here rather than generating default wire in both directions.
    if reject_custom_codec_on_row_entry(
        types,
        &format!("open struct-map rest row (`* k => v`) of rule `{src}`"),
        "Name the row's key or value type as its own rule and put the pair there (`k = text ; \
         @custom_serialize <fn> @custom_deserialize <fn>`, then `* k => v`).",
        &rest_metadata,
    ) {
        return (None, Some(candidate));
    }
    // …and a `@custom_encodings` declaration with no pair to describe is dropped the same way.
    if reject_custom_encodings_without_pair(
        types,
        &format!("the open struct-map rest row (`* k => v`) of rule `{src}`"),
        &rest_metadata,
    ) {
        return (None, Some(candidate));
    }
    // `@ignore` selects the tolerate-and-DROP flavor: unknown entries are typed-deserialized and
    // discarded (no field, serialize emits declared members only). It combines with nothing else on
    // the row, and it is incompatible with `--preserve-encodings`. Each combination is a graceful
    // rejection naming the remedy (never a silent drop). Placement/domain guards above fire FIRST, so
    // `@ignore` on an unsupported placement gets the placement rejection, not one of these.
    if rest_metadata.ignore {
        // `@ignore` + `--preserve-encodings`: a preserve crate's contract is byte-exact round-trips,
        // which a deliberately-lossy type undermines crate-wide. `--canonical-form` implies preserve
        // (enforced in `api.rs`), so this covers it transitively. Point at the default capture flavor
        // and at `@custom_serialize`/`@custom_deserialize` for genuine view types.
        if cli.preserve_encodings {
            types.record_rejection(format!(
                "rule `{src}`: `@ignore` (tolerate-and-drop) on an open struct-map rest row is not \
                 supported under --preserve-encodings, because a preserve crate's contract is \
                 byte-exact round-trips and a silently-lossy type undermines it. Drop the `@ignore` \
                 to capture the unknown entries (the default), or use `@custom_serialize` / \
                 `@custom_deserialize` for a genuine view type."
            ));
            return (None, Some(candidate));
        }
        // `@ignore` + `@duplicates`: a duplicates policy governs a captured container, which `@ignore`
        // does not create (unknown entries are dropped, and dropped entries have no duplicate story).
        if rest_metadata.duplicates.is_some() {
            types.record_rejection(format!(
                "rule `{src}`: `@ignore` and `@duplicates` cannot both apply to an open struct-map \
                 rest row — `@ignore` drops unknown entries, so there is no container for a \
                 duplicates policy to govern. Keep one: `@ignore` to drop, or `@duplicates` (with \
                 capture) to retain."
            ));
            return (None, Some(candidate));
        }
        // `@ignore` + `@name`: `@name` renames the captured field, which an ignore row does not emit.
        if rest_metadata.name.is_some() {
            types.record_rejection(format!(
                "rule `{src}`: `@ignore` and `@name` cannot both apply to an open struct-map rest \
                 row — `@ignore` emits no field to name. Drop `@name`, or drop `@ignore` to capture \
                 the entries into the named field."
            ));
            return (None, Some(candidate));
        }
    }
    let semantics = if rest_metadata.ignore {
        RestSemantics::Ignore
    } else {
        RestSemantics::Capture
    };
    // `@duplicates` policy on the rest row (CAPTURE flavor): default (reject) uses the loose container
    // (value-equality dup check — accept/reject keyed on the wire VALUE, not the domain's spelling);
    // `preserve` uses the vec-of-pairs twin (`PairMap`), matching what `@duplicates preserve` TABLES do
    // — duplicate keys accepted and re-emitted in wire order. `reject` explicit is the same as default.
    // Carried on the `RestRow` for the emitters to select the container. (Rejected above for `@ignore`.)
    let field_name = rest_metadata
        .name
        .clone()
        .unwrap_or_else(|| "rest".to_owned());
    let rest_row = RestRow {
        kind: RestKind::MapEntries {
            domain,
            range,
            duplicates: rest_metadata.duplicates,
        },
        semantics,
        field_name,
        // Only an open table's TYPED row claims a single major; a catch-all sees the complement.
        dispatch_major: None,
        // Only an open table's TYPED row carries the min-1 (`+`) bound; a single trailing rest row is
        // recognized only under `*` (its own occurrence guard rejects everything else).
        non_empty: false,
    };
    (Some(Box::new(rest_row)), Some(candidate))
}

/// The array-rep analog of `recognize_rest_row`: recognize a final-position `* T` / `+ T` tail
/// (`[a, b, * t]` / `[a, b, + t]`)
/// after ≥1 fixed member as an open-array rest tail (the positional sibling of the map rest row), or
/// reject an unsupported placement/shape gracefully. Returns the built `RestRow` (if recognized) and
/// the flattened index of the tail CANDIDATE (so the caller's field loop skips it — whether recognized
/// or rejected). Arrays have no keys, so there is no key dispatch / duplicate policy / domain typing:
/// the genuinely new content is one tail loop in the array deserializer. `(None, None)` when no
/// count-permitting entry exists (an ordinary closed array).
#[allow(clippy::too_many_arguments)]
fn recognize_array_rest_tail(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    flattened: &[&(GroupEntry, OptionalComma)],
    entry_count: usize,
    in_choice_arm: bool,
    cli: &Cli,
) -> (Option<Box<RestRow>>, Option<usize>) {
    let source_name = || {
        types
            .source_rule_name(name)
            .map(str::to_owned)
            .unwrap_or_else(|| name.to_string())
    };
    // Count-permitting occurrences are exactly the markers the field-loop narrowing guard matches:
    // anything present that is NOT `?` (optional) or the pedantic `1*1` (exactly-once). `*` / `+` /
    // `n*m` all qualify as tail CANDIDATES here (only `*` and min-one are ultimately honored — the rest reject
    // below naming the supported spelling). Only `ValueMemberKey`/`TypeGroupname` carry `ge.occur`;
    // an inline group has none (never count-permitting → never a candidate → its later `* (…)`
    // narrowing rejection in the field loop stands).
    let count_permits = |ge: &GroupEntry| {
        let occur = match ge {
            GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
            GroupEntry::TypeGroupname { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
            GroupEntry::InlineGroup { .. } => None,
        };
        occur
            .map(|o| {
                !matches!(
                    o,
                    Occur::Optional { .. }
                        | Occur::Exact {
                            lower: Some(1),
                            upper: Some(1),
                            ..
                        }
                )
            })
            .unwrap_or(false)
    };
    let candidate_indices: Vec<usize> = flattened
        .iter()
        .enumerate()
        .filter(|(_, (ge, _))| count_permits(ge))
        .map(|(i, _)| i)
        .collect();
    let Some(&candidate) = candidate_indices.last() else {
        // No count-permitting entry: an ordinary closed array. Byte-identical to pre-feature output.
        return (None, None);
    };
    let src = source_name();
    // A rest tail cannot be collapsed into an enum variant (it would drop the open-array semantics),
    // so reject it in a group-choice arm; the candidate is still skipped so no fixed field is built.
    if in_choice_arm {
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail (`* t`) inside a group-choice arm \
             (`[ … ] // [ … ]`) is unsupported. Give the open array its own named rule and reference \
             it from the arm."
        ));
        return (None, Some(candidate));
    }
    // A rest tail inside a PLAIN GROUP (`g = ( a, * t )`, embedded via `[ g ]`) is rejected: a
    // materialized plain group exports TRANSPARENTLY as an extern-interface group-body row rendered
    // from `fields` only, so recognizing a tail here would silently project a CLOSED group across the
    // crate boundary (the silent-lossy cross-crate class). Point at the named-rule form.
    if types.is_plain_group(name) {
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail (`* t`) inside a plain group \
             (`{src} = ( … * t )`, embedded elsewhere) is unsupported. Give the open array its own \
             named rule (`{src} = [ … * t ]`) and reference it by name."
        ));
        return (None, Some(candidate));
    }
    // Multiple count-permitting entries: only a single trailing rest tail is supported. (The
    // field-loop narrowing guard additionally rejects each earlier one by design — never silent.)
    if candidate_indices.len() > 1 {
        types.record_rejection(format!(
            "rule `{src}`: an open array supports a single trailing rest tail (`* t`), but this array \
             has {} occurrence-bearing members. Keep one `* t` member (last), use `?` for optional \
             members, or name the repeated part as its own array rule.",
            candidate_indices.len()
        ));
        return (None, Some(candidate));
    }
    // Non-final placement: the rest tail must be the LAST entry (the fixed prefix is read first, then
    // the tail captures the remainder). A leading/mid `*` keeps rejecting (position-dependent now).
    if candidate != entry_count - 1 {
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail (`* t`) must be the LAST member of the array (the \
             fixed prefix is read first, then the tail captures the remaining elements). Move it to \
             the end."
        ));
        return (None, Some(candidate));
    }
    // An open array needs ≥1 fixed member before the rest tail (a `* t` with nothing before it is a
    // homogeneous array `[* t]`, recognized by `parse_group_type` before this runs and never reaching
    // the record path). A lone count-permitting entry here is therefore a degenerate shape.
    if candidate == 0 {
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail (`* t`) must follow at least one fixed member \
             (`[ a, * t ]`). An array whose only member is `* t` is a homogeneous array — write it \
             as `[* t]` (or give it its own rule)."
        ));
        return (None, Some(candidate));
    }
    let (candidate_ge, candidate_comma) = flattened[candidate];
    // Occurrence must be exactly `*` (loose capture) or `+` / `1*` (the min-one restricted capture).
    // Other bounded cardinalities are deliberately still unimplemented.
    let candidate_occur = match candidate_ge {
        GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
        GroupEntry::TypeGroupname { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
        GroupEntry::InlineGroup { .. } => None,
    };
    let non_empty = matches!(
        candidate_occur,
        Some(Occur::OneOrMore { .. })
            | Some(Occur::Exact {
                lower: Some(1),
                upper: None,
                ..
            })
    );
    if !matches!(candidate_occur, Some(Occur::ZeroOrMore { .. })) && !non_empty {
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail must use `*` (unbounded capture) or `+` / `1*` \
             (one-or-more capture). Other `n*m` bounds are not supported on a rest tail."
        ));
        return (None, Some(candidate));
    }
    // A member KEY on the tail entry (`* 1: uint` in array rep) is nonsense — an array tail is
    // positional. Reject rather than silently dropping the label. (An inline group never reaches here:
    // it carries no `ge.occur`, so it is never count-permitting → never a candidate.)
    if let GroupEntry::ValueMemberKey { ge, .. } = candidate_ge
        && ge.member_key.is_some()
    {
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail (`* t`) is positional and cannot carry a member \
             key. Drop the `key:` label."
        ));
        return (None, Some(candidate));
    }
    let element_type = group_entry_to_type(types, parent_visitor, candidate_ge, cli);
    // A fixed-value tail element (`* 5` / `* null` / `* true`) has no Rust representation (a
    // `Vec<FixedValue>` is not a type). Reject BEFORE the homogeneous-array fixed-value panic class.
    if element_type.is_fixed_value() {
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail cannot be a fixed value (`* 5`, `* null`, \
             `* true`) — there is no Rust representation for a captured tail of fixed values. Use a \
             typed element (`* uint`, `* t`) or `* any` to capture arbitrary items."
        ));
        return (None, Some(candidate));
    }
    // A PLAIN GROUP tail element (`* kv`, where `kv = (a: uint, b: uint)`). Sibling of the
    // fixed-value guard above and for the same reason: a rest tail collects one Rust value per
    // remaining array element, and a plain group is not one — it splices its members flat into the
    // enclosing array and is never materialized as a type of its own, so the tail emitter's
    // `rust_struct` lookup came back empty and generation aborted on a raw `Option::unwrap()` in
    // `encoding_var_is_copy` (and, under `--preserve-encodings` / `--wasm`, on the plain-group
    // registry assert reached earlier). Honoring the shape means a SPLICING tail that consumes the
    // group's arity worth of elements per repetition — the occurrence/bounds program's territory,
    // not a guard's — so this is a refusal, and one made honest by a remedy verified to generate.
    //
    // Guarded on `is_basic` over the RESOLVED element type — the same predicate and the same
    // one-seam placement as the record-field twin — so the bare, the ALIAS (`* kv_alias`) and the
    // TAGGED (`* #6.10(kv)`) spellings land on ONE message on every profile. The array-WRAPPED
    // forms keep their own (supported) verdicts: `w = [kv]` is a Record, not a plain group, and an
    // inline `* [kv]` element carries `basic_override`, so neither is `is_basic`.
    if element_type.is_basic(types)
        && let ConceptualRustType::Rust(group_ident) =
            element_type.conceptual_type.resolve_alias_shallow()
    {
        let group_name = types
            .source_rule_name(group_ident)
            .map(str::to_owned)
            .unwrap_or_else(|| group_ident.to_string());
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail cannot capture the plain group `{group_name}` \
             — a plain group has no type of its own, it splices its members flat into the \
             enclosing array, while a rest tail collects ONE value per remaining element, so there \
             is nothing for it to collect. Give the group its own array framing and capture that, \
             which makes each repetition exactly ONE array element: `w = [{group_name}]`, then \
             `* w` in place of this tail. (A tag belongs on the framed reference — `* #6.10(w)` — \
             not on the group. A single MANDATORY splice of the group, `{group_name}` with no \
             occurrence, is supported as it stands.)"
        ));
        return (None, Some(candidate));
    }
    // Entry-level directives on the tail (`@name`, `@ignore`; `@duplicates` is rejected — no keys),
    // read from the row's own trailing slot (NOT rule-position handling, and the two slots are
    // DISJOINT here: on an array TYPE rule the parser binds a trailing comment written after the
    // closing bracket to the RULE slot alone — `u = [ a, * t ] ; @ignore` is refused as a
    // rule-position `@ignore`, and this walk never sees it — so anything read below is the comment
    // the author put on the TAIL's own line; the dual-read slot is a plain GROUP rule's last entry,
    // a different construct). Placement/shape guards above fire FIRST, so `@ignore` on a rejected
    // placement gets the placement rejection, not one of these.
    let tail_metadata = group_entry_rule_metadata(candidate_ge, candidate_comma);
    // The TYPE-SCOPED directives describe the type a name denotes, so a tail entry — which declares
    // no type of its own — reads none of them. Called ALONE (not the whole member list) because this
    // slot legitimately honors `@name` and `@ignore`, which that list refuses.
    reject_type_scoped_directives(
        types,
        &format!("the open-array rest tail (`* t`) of rule `{src}`"),
        &tail_metadata,
    );
    // A custom (de)serializer pair in this slot is inert (the tail declares no type of its own) —
    // reject it here rather than generating default wire in both directions.
    if reject_custom_codec_on_row_entry(
        types,
        &format!("open-array rest tail (`* t`) of rule `{src}`"),
        "Name the tail element type as its own rule and put the pair there (`e = uint ; \
         @custom_serialize <fn> @custom_deserialize <fn>`, then `* e`).",
        &tail_metadata,
    ) {
        return (None, Some(candidate));
    }
    // …and a `@custom_encodings` declaration with no pair to describe is dropped the same way.
    if reject_custom_encodings_without_pair(
        types,
        &format!("the open-array rest tail (`* t`) of rule `{src}`"),
        &tail_metadata,
    ) {
        return (None, Some(candidate));
    }
    // `@duplicates` on an array tail is meaningless — there are no keys for a duplicates policy to
    // govern (distinct from the map row's `@ignore`+`@duplicates` combination message).
    if tail_metadata.duplicates.is_some() {
        types.record_rejection(format!(
            "rule `{src}`: `@duplicates` does not apply to an open-array rest tail — an array tail \
             has no keys, so there is no duplicate policy to govern. Remove `@duplicates`."
        ));
        return (None, Some(candidate));
    }
    if tail_metadata.ignore {
        if non_empty {
            types.record_rejection(format!(
                "rule `{src}`: `@ignore` cannot apply to a one-or-more open-array rest tail (`+ t` / \
                 `1* t`), because dropping every captured element would re-serialize zero occurrences \
                 and violate the rule's own minimum. Drop `@ignore` to capture the non-empty tail."
            ));
            return (None, Some(candidate));
        }
        // `@ignore` + `--preserve-encodings`: PERMANENTLY rejected (a preserve crate's contract is
        // byte-exact round-trips, which a deliberately-lossy tolerate-and-drop tail undermines
        // crate-wide). `--canonical-form` implies preserve (enforced in `api.rs`), so this covers it
        // transitively. Distinct message naming the array shape (not a reword of the map text).
        if cli.preserve_encodings {
            types.record_rejection(format!(
                "rule `{src}`: `@ignore` (tolerate-and-drop) on an open-array rest tail is not \
                 supported under --preserve-encodings, because a preserve crate's contract is \
                 byte-exact round-trips and a silently-lossy type undermines it. Drop the `@ignore` \
                 to capture the trailing elements (the default), or use `@custom_serialize` / \
                 `@custom_deserialize` for a genuine view type."
            ));
            return (None, Some(candidate));
        }
        // `@ignore` + `@name`: `@name` renames the captured field, which an ignore tail does not emit.
        if tail_metadata.name.is_some() {
            types.record_rejection(format!(
                "rule `{src}`: `@ignore` and `@name` cannot both apply to an open-array rest tail — \
                 `@ignore` emits no field to name. Drop `@name`, or drop `@ignore` to capture the \
                 elements into the named field."
            ));
            return (None, Some(candidate));
        }
    }
    let semantics = if tail_metadata.ignore {
        RestSemantics::Ignore
    } else {
        RestSemantics::Capture
    };
    let field_name = tail_metadata
        .name
        .clone()
        .unwrap_or_else(|| "rest".to_owned());
    let rest_row = RestRow {
        kind: RestKind::ArrayTail {
            element: element_type,
        },
        semantics,
        field_name,
        // An array tail has no keys, so no major-type dispatch and no claimed major.
        dispatch_major: None,
        non_empty,
    };
    (Some(Box::new(rest_row)), Some(candidate))
}

#[allow(clippy::too_many_arguments)]
fn parse_group_choice(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    group_choice: &GroupChoice,
    name: &RustIdent,
    rep: Representation,
    tag: Option<usize>,
    generic_params: Option<Vec<RustIdent>>,
    parent_rule_metadata: Option<&RuleMetadata>,
    // Whether this group choice is one arm of a multi-arm choice (`{ a } // { b }`) — threaded to
    // rest-row recognition (a rest row is rejected in a choice arm in v1).
    in_choice_arm: bool,
    cli: &Cli,
) {
    let rule_metadata = RuleMetadata::from(
        get_comment_after(parent_visitor, &CDDLType::from(group_choice), None).as_ref(),
    );
    let rule_metadata = if let Some(parent_rule_metadata) = parent_rule_metadata {
        merge_metadata(&rule_metadata, parent_rule_metadata)
    } else {
        rule_metadata
    };
    let rust_struct =
        match parse_group_type(types, parent_visitor, group_choice, rep, Some(name), cli) {
            GroupParsingType::HomogenousArray(element_type, bounds) => {
                // Array-shaped collection: `@duplicates reject` is LIVE (rides the alias built in
                // `register_rust_struct`), `preserve` is the default (accepted no-op). Nothing to
                // reject here. `@ignore` never applies to an array collection (it is the open
                // struct-MAP rest-row flavor) — reject a rule-position `@ignore` loudly.
                if rule_metadata.ignore {
                    reject_ignore_not_applicable(types, name);
                }
                // A plain group used as the array element (`pair = (int, tstr)`, `a = [* pair]`) must be
                // registered as a concrete Array-rep rust struct, exactly like the anonymous member-array
                // path (`rust_type_from_type2`'s `Type2::Array` arm) and the record path both do. Without
                // this the element ident stays an unregistered plain group and `is_enum`/`for_rust_member`
                // trip their "must be a struct or a generic instance" assert at generation time.
                // Aliases resolve first — an alias is transparent, so `a = [* kv_alias]` materializes
                // the group exactly like `a = [* kv]`. Without the resolution the run exited 0 having
                // emitted `pub type KvAlias = Kv;` with no `Kv` anywhere: a crate that does not compile.
                if let ConceptualRustType::Rust(element_ident) =
                    element_type.conceptual_type.resolve_alias_shallow()
                {
                    types.set_rep_if_plain_group(
                        parent_visitor,
                        element_ident,
                        Representation::Array,
                        cli,
                    );
                }
                // Covers non-generic set rules (Phase 2.2) and generic single-arm set DEFS (Phase 2.3):
                // a generic def stores the wrapper (param element) as a `GenericDef`, and each
                // instantiation mints one nominal per `<def>_<args>` in `GenericInstance::resolve`.
                let is_set_nominal =
                    tag.is_some_and(|t| well_known_tag_default_duplicates(t, true).is_some());
                if is_set_nominal {
                    // A single-arm mandatory-tag 258 SET rule (`#6.258([* a])`) NOMINALIZES into a
                    // `Wrapper` struct owning its `{tag, len, elem}` encodings (Phase 2.2), exactly like
                    // the two-arm idiom but with a MANDATORY tag (grammar decides the record: `Option<Sz>`,
                    // NOT the two-arm `TagPresenceEncoding`). The registry set-semantics default (reject)
                    // rides `single_arm_array_effective_metadata` and the `Wrapper` register arm threads it
                    // onto the stored inner array type, selecting the `OrderedSet`/`NonEmptyOrderedSet`
                    // twin. `@newtype` carries a custom getter on the wrapper; a bare set nominal emits no
                    // inherent `get()` (it would shadow `OrderedSet::get(index)` through `Deref`).
                    let effective_metadata =
                        single_arm_array_effective_metadata(&rule_metadata, tag, name);
                    let mut array_type: RustType =
                        ConceptualRustType::Array(Box::new(element_type)).into();
                    if let Some(bounds) = bounds {
                        array_type = array_type.with_bounds(bounds);
                    }
                    RustStruct::new_wrapper(
                        name.clone(),
                        tag,
                        Some(&effective_metadata),
                        array_type,
                        None,
                    )
                    .as_set_nominal()
                } else if rule_metadata.newtype.is_some() || tag.is_some() {
                    // generate newtype over array — on `@newtype`, and UNCONDITIONALLY when the rule
                    // carries a TAG. A tagged transparent alias (`pub type TaggedArr = Vec<u64>;` with
                    // the tag riding the alias entry) mints no type to hang the tag on, so
                    // `TaggedArr::to_cbor_bytes` would be `Vec<u64>`'s — writing the BARE array while
                    // every embed site of the rule writes `write_tag(n)` first and every embed site's
                    // decoder requires it. Same reasoning as the single-type tag rule and the `.cbor`
                    // rule body; `register_type_alias`'s wire-facts assert makes the alias spelling
                    // unrepresentable rather than merely unused, and `@newtype` is redundant here.
                    // Route through the SAME effective-metadata helper the
                    // plain single-arm array path uses so a single-arm tag-258 `@newtype` wrapper
                    // (`#6.258([* a]) ; @newtype`) picks up the registry's set-semantics default
                    // (reject) and fires the single-arm defaulting notice, exactly as the non-newtype
                    // flavor does — no-op for a non-258 tag or an explicit directive. The effective
                    // `@duplicates` policy lands in the wrapper's struct config; the register-side
                    // `Wrapper` arm then threads it onto the stored inner collection type so generation
                    // selects the `OrderedSet` twin.
                    let effective_metadata =
                        single_arm_array_effective_metadata(&rule_metadata, tag, name);
                    let mut array_type: RustType =
                        ConceptualRustType::Array(Box::new(element_type)).into();
                    if let Some(bounds) = bounds {
                        array_type = array_type.with_bounds(bounds);
                    }
                    RustStruct::new_wrapper(
                        name.clone(),
                        tag,
                        Some(&effective_metadata),
                        array_type,
                        None,
                    )
                } else {
                    // Array - homogeneous element type with proper occurence operator. A single-arm
                    // tag-258 set picks up the registry's reject default via the helper (no-op for a
                    // non-258 tag or an explicit directive).
                    let effective_metadata =
                        single_arm_array_effective_metadata(&rule_metadata, tag, name);
                    RustStruct::new_array(
                        name.clone(),
                        tag,
                        Some(&effective_metadata),
                        element_type,
                        bounds,
                    )
                }
            }
            GroupParsingType::HomogenousMap(key_type, value_type, bounds) => {
                // `@ignore` is the open struct-map rest-row flavor and does not apply to a TABLE rule
                // (`{ * k => v }`, no fixed keys) — reject a rule-position `@ignore` loudly.
                if rule_metadata.ignore {
                    reject_ignore_not_applicable(types, name);
                }
                // A table's single row carries a trailing comment slot DISJOINT from the rule's own (a
                // rule-trailing `@duplicates` reaches `rule_metadata`; the same directive spelled on the
                // row does not reach it). Nothing a named table's row slot can carry is honored, so the
                // slot's whole job here is to refuse loudly rather than swallow: a custom (de)serializer
                // pair (a TYPE-level override; a row declares no type), its `@custom_encodings`
                // declarations, and `@duplicates` (whose honored spelling is the rule slot).
                // (`InlineGroup` is skipped: `group_entry_rule_metadata` panics on one, and a
                // parenthesized table row `{ * (k => v) }` has no entry slot of its own anyway.)
                if let [(row_ge, row_comma)] =
                    flatten_group_entries(&group_choice.group_entries, Representation::Map)[..]
                    && !matches!(row_ge, GroupEntry::InlineGroup { .. })
                {
                    let row_metadata = group_entry_rule_metadata(row_ge, row_comma);
                    let src = types
                        .source_rule_name(name)
                        .map(str::to_owned)
                        .unwrap_or_else(|| name.to_string());
                    reject_custom_codec_on_row_entry(
                        types,
                        &format!("table row (`* k => v`) of rule `{src}`"),
                        "Name the table's key or value type as its own rule and put the pair there \
                     (`k = text ; @custom_serialize <fn> @custom_deserialize <fn>`, then \
                     `{ * k => v }`).",
                        &row_metadata,
                    );
                    // …and a `@custom_encodings` declaration with no pair to describe is dropped the
                    // same way.
                    reject_custom_encodings_without_pair(
                        types,
                        &format!("the table row (`* k => v`) of rule `{src}`"),
                        &row_metadata,
                    );
                    // A `@duplicates` written on the row is read into `row_metadata` and dropped —
                    // BOTH policies, `preserve` and the explicit `reject` alike (the rule slot is what
                    // `register_rust_struct` reads). An ANONYMOUS inline table honors this slot
                    // precisely because it has no rule slot to carry the policy; a named table has one,
                    // so a second honored spelling would only invite the two to drift. Reject it and
                    // point at the rule slot.
                    if row_metadata.duplicates.is_some() {
                        types.record_rejection(format!(
                            "@duplicates on the table row (`* k => v`) of rule `{src}`: a named \
                         table's duplicates policy is read from the RULE's own trailing slot, not \
                         from the row's, so it is not honored here. Move it after the closing \
                         brace (`{src} = {{ * k => v }} ; @duplicates <policy>`). (An ANONYMOUS \
                         inline table — one written directly at a member, element or union-arm \
                         type — does carry the policy on its row, because it has no rule slot.)"
                        ));
                    }
                }
                // Table collection: `reject` is today's default (accepted no-op) and `preserve` is
                // LIVE — the policy rides the transparent alias built in `register_rust_struct`,
                // swapping the member to the `PairMap`/`NonEmptyPairMap` vec-of-pairs twin. That is the
                // RULE slot's reading; the row slot's is rejected above.
                // Same registration gap as the array arm above: a plain group used as a table key or
                // value (`pair = (int, tstr)`, `a = { * int => pair }`) must be registered as a concrete
                // Array-rep rust struct — a CBOR map value can only be one item, so the group is encoded
                // as a nested array, exactly the interpretation the table alias (`BTreeMap<Int, Pair>`)
                // already commits to. Without this the ident stays an unregistered plain group and
                // `is_enum` trips its "must be a struct or a generic instance" assert at generation time.
                for member in [&key_type, &value_type] {
                    if let ConceptualRustType::Rust(member_ident) = &member.conceptual_type {
                        types.set_rep_if_plain_group(
                            parent_visitor,
                            member_ident,
                            Representation::Array,
                            cli,
                        );
                    }
                }
                // A tag forces the wrapper for the reason the array sibling above states: a tagged
                // transparent map alias drops the tag from the rule's own standalone
                // `to/from_cbor_bytes` while every embed site writes and requires it. This holds for
                // EVERY duplicates policy, `preserve` included: the register-side `Wrapper` arm threads
                // the policy onto the stored inner map type, so the wrapper's member is the
                // `PairMap`/`NonEmptyPairMap` vec-of-pairs twin and its wasm boundary names the
                // `PairMapKToV` structural class (minted by the config-aware wasm walk beside the
                // default-flavored `MapKToV`).
                if rule_metadata.newtype.is_some()
                    || tag.is_some()
                    // A complete pair owns the WHOLE table item, not either entry position. A
                    // transparent table alias has no trait-impl site, so it cannot truthfully own
                    // that wire: direct `T::to_cbor_bytes()` would otherwise write the built-in map
                    // while a holder routes through the named pair. Self-nominalize the map through
                    // the existing wrapper path instead. This implicit nominal owner is the pair's
                    // representation (and its only accepted table spelling); explicit `@newtype`
                    // remains a separately rejected wrapper placement. Its other wrapper contracts
                    // (tags, ranges, sets, wire facts, and cross-face behavior) are not broadened
                    // by this table-only ownership seam.
                    || (rule_metadata.custom_serialize.is_some()
                        && rule_metadata.custom_deserialize.is_some())
                {
                    // generate a nominal owner over map
                    let mut map_type: RustType =
                        ConceptualRustType::Map(Box::new(key_type), Box::new(value_type)).into();
                    if let Some(bounds) = bounds {
                        map_type = map_type.with_bounds(bounds);
                    }
                    RustStruct::new_wrapper(name.clone(), tag, Some(&rule_metadata), map_type, None)
                } else {
                    // Table map - homogeneous key/value types
                    RustStruct::new_table(
                        name.clone(),
                        tag,
                        Some(&rule_metadata),
                        key_type,
                        value_type,
                        bounds,
                    )
                }
            }
            GroupParsingType::Heterogenous | GroupParsingType::WrappedBasicGroup(_) => {
                // A heterogenous struct/record (or a single wrapped basic group) is not a collection,
                // so `@duplicates` can never apply here. A rule-position `@ignore` is a misplacement too:
                // the valid `@ignore` sits on the `* k => v` ENTRY (read in `recognize_rest_row` off the
                // entry-trailing slot), NOT on the rule (the two slots are disjoint — a rule directive is
                // never stolen by the last entry, nor an entry directive by the rule).
                if rule_metadata.duplicates.is_some() {
                    reject_duplicates_not_applicable(types, name);
                }
                if rule_metadata.ignore {
                    reject_ignore_not_applicable(types, name);
                }
                assert!(
                    rule_metadata.newtype.is_none(),
                    "Can only use @newtype on primtives + heterogenious arrays/maps"
                );
                // Heterogenous map or array with defined key/value pairs in the cddl like a struct
                let record = parse_record_from_group_choice(
                    types,
                    rep,
                    parent_visitor,
                    name,
                    group_choice,
                    in_choice_arm,
                    tag.is_some(),
                    cli,
                );
                // We need to store this in IntermediateTypes so we can refer from one struct to another.
                RustStruct::new_record(name.clone(), tag, Some(&rule_metadata), record)
            }
        };
    match generic_params {
        Some(params) => types.register_generic_def(GenericDef::new(params, rust_struct)),
        None => types.register_rust_struct(parent_visitor, rust_struct, cli),
    };
}

#[allow(clippy::too_many_arguments)]
pub fn parse_group(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    group: &Group,
    name: &RustIdent,
    rep: Representation,
    tag: Option<usize>,
    generic_params: Option<Vec<RustIdent>>,
    parent_rule_metadata: &RuleMetadata,
    cli: &Cli,
) {
    if group.group_choices.len() == 1 {
        // Handle simple (no choices) group.
        parse_group_choice(
            types,
            parent_visitor,
            group.group_choices.first().unwrap(),
            name,
            rep,
            tag,
            generic_params,
            Some(parent_rule_metadata),
            // A single-choice group is not a choice arm — rest rows are recognized here.
            false,
            cli,
        );
    } else {
        // A generic definition whose body carries GROUP choices (`g<T> = [ (a: T) // (b: uint) ]`)
        // mints an enum of one struct per arm, and the generic machinery substitutes into exactly
        // ONE registered struct — there is nowhere to thread the parameters. Refused at parse time
        // rather than aborted; unlike the type-choice sibling this has no supported idiom, so the
        // remedy is the use-site selection.
        if generic_params.is_some() {
            types.record_rejection(format!(
                "generic rule `{name}`: group choices (`//`) in a generic definition are not \
                 supported — each arm becomes its own struct behind an enum, and a generic \
                 definition substitutes its arguments into exactly one registered struct. Give each \
                 arm its own named group and choose between them at the use site \
                 (`{name}_a = (…)`, `{name}_b = (…)`, `x = [ {name}_a // {name}_b ]`). \
                 {SUPPORTED_GENERIC_DEF_BODIES}"
            ));
            return;
        }
        assert!(parent_rule_metadata.newtype.is_none());
        // Generate Enum object that is not exposed to wasm, since wasm can't expose
        // fully featured rust enums via wasm_bindgen

        // TODO: We don't support generating SerializeEmbeddedGroup for group choices which is necessary for plain groups
        // It would not be as trivial to add as we do the outer group's array/map tag writing inside the variant match
        // to avoid having to always generate SerializeEmbeddedGroup when not necessary.
        assert!(!types.is_plain_group(name));

        // Handle group with choices by generating an enum then generating a group for every choice
        //
        // Every arm names a variant of the ONE enum being built here, so all the arms' names share a
        // single namespace and two arms landing on the same one is a Rust `E0428` — a generated
        // crate that does not compile. `settle_arm_variant_name` below owns that namespace for all
        // three naming branches: an EXPLICIT `@name` is never renamed (it is public API of the
        // generated crate, so a rename would silently ship a name nobody asked for) and a second one
        // spelling it rejects; a DERIVED name — from the arm's member key, its type, or its position
        // — carries no authorial intent, so it yields and takes a numeric suffix.
        //
        // The arms' explicit names are reserved BEFORE the loop so that which side of a colliding
        // explicit/derived pair keeps the plain name never depends on the order the author happened
        // to write the arms in: the authored name wins from either position.
        let mut explicit_variant_names = BTreeMap::<String, String>::new();
        let mut variant_names_used = BTreeSet::<String>::new();
        for group_choice in group.group_choices.iter() {
            if let Some(explicit) =
                RuleMetadata::from(group_choice.comments_before_grpchoice.as_ref()).name
            {
                variant_names_used.insert(convert_to_camel_case(&explicit));
            }
        }
        let variants: Vec<EnumVariant> = group
            .group_choices
            .iter()
            .enumerate()
            .map(|(i, group_choice)| {
                let rule_metadata =
                    RuleMetadata::from(group_choice.comments_before_grpchoice.as_ref());
                // If we're a 1-element we should just wrap that type in the variant rather than
                // define a new struct just for each variant.
                // TODO: handle map-based enums? It would require being able to extract the key logic
                // We might end up doing this anyway to support table-maps in choices though.
                if group_choice.group_entries.len() == 1 {
                    let (group_entry, entry_comma) = group_choice.group_entries.first().unwrap();
                    // An occurrence-carrying arm is refused as a SHAPE, so its member's directives
                    // describe a member that will not exist — one message per problem, and the one
                    // to give is the one whose remedy rewrites the arm. In MAP rep a lower-bound-≥1
                    // marker is honored by collapse (the shared `inline_group_occurrence_flattens`
                    // boundary) and refuses nothing, so a directive on `{ x: uint // + kv }` still
                    // reaches the validation below — which is exactly right: that arm generates.
                    let occurrence_refused =
                        reject_occurrence_on_single_entry_arm(types, name, group_entry, rep);
                    let ty = group_entry_to_type(types, parent_visitor, group_entry, cli);
                    // The directive validation runs AFTER the member's type parse, because the
                    // `@name` verdict is the anon-array reader's own effect observed on `ty` rather
                    // than a second derivation of that reader's scope.
                    if !occurrence_refused {
                        reject_field_directives_on_single_entry_arm(
                            types,
                            name,
                            group_entry,
                            entry_comma,
                            &ty,
                        );
                    }
                    // Resolve aliases first: an alias is transparent, so an arm spelled
                    // `kv_alias` must materialize and embed the plain group exactly like the
                    // direct `kv` arm. Reading the bare `Rust(ident)` skipped both the
                    // registration and the embedded classification for the alias spelling, which
                    // aborted the ARRAY rep on an unmaterialized struct and pushed the MAP rep's
                    // keyless arm — a supported shape whose referenced struct owns its own keys —
                    // into the no-key rejection.
                    let serialize_as_embedded =
                        if let ConceptualRustType::Rust(ident) =
                            ty.conceptual_type.resolve_alias_shallow()
                        {
                            // we might need to generate it if not used elsewhere
                            types.set_rep_if_plain_group(parent_visitor, ident, rep, cli);
                            // manual match in case we expand operaitons later
                            types.is_plain_group(ident)
                                && !ty.encodings.iter().any(|enc| match enc {
                                    CBOREncodingOperation::Tagged(_) => true,
                                    CBOREncodingOperation::OptionallyTagged(_) => true,
                                    CBOREncodingOperation::CBORBytes => true,
                                })
                        } else {
                            false
                        };
                    // A single-entry arm registers no record at all — its type goes straight into
                    // the variant — so the name settled here is the ONLY name it ever claims.
                    let (ident_name, explicit_name) = match rule_metadata.name.clone() {
                        Some(explicit) => (explicit, true),
                        None => match group_entry_to_raw_field_name(group_entry) {
                            Some(field_name) => (field_name, false),
                            // A BARE member (`[ true // 1.5 ]`) has no key to name the variant
                            // after, so the name is minted from the member's TYPE — the same
                            // lexeme-derived spelling the type-choice seam guards, reached through
                            // a second consumer. Refuse it here too rather than emitting `F1.5`.
                            None => {
                                let minted = ty.for_variant().to_string();
                                if !is_spellable_variant_name(&minted) {
                                    let owner_desc =
                                        format!("rule `{}`", source_rule_name_of(types, name));
                                    reject_unnameable_arm_variant_name(
                                        types,
                                        &owner_desc,
                                        &group_entry.to_string(),
                                        &minted,
                                        "a group-choice arm's naming slot is the one that \
                                         FOLLOWS the `//` opening it \
                                         (`// ; @name <new_name>`), not the entry's own line",
                                    );
                                }
                                (minted, false)
                            }
                        },
                    };
                    let variant_ident = VariantIdent::new_custom(settle_arm_variant_name(
                        types,
                        name,
                        &mut variant_names_used,
                        &mut explicit_variant_names,
                        convert_to_camel_case(&ident_name),
                        &ident_name,
                        explicit_name,
                    ));
                    // For a MAP-representation arm the single entry carries a member key that must
                    // be written+verified on the wire (dropping it produces malformed CBOR). Carry
                    // the fixed key on the variant; reject non-fixed/keyless entries gracefully
                    // rather than silently miscompiling.
                    let variant_key = if rep == Representation::Map {
                        match group_entry_map_key_kind(group_entry) {
                            // only uint/text keys are supported (parity with the record map path,
                            // which also rejects other fixed key kinds gracefully at parsing)
                            MapKeyKind::Fixed(key @ (FixedValue::Uint(_) | FixedValue::Text(_)))
                                if !ty.is_basic(types) =>
                            {
                                Some(key)
                            }
                            // A KEYED single-entry arm whose type resolves to a plain group is the
                            // record path's member refusal reached through the enum seam: the key
                            // claims one entry and the group can only splice. (A KEYLESS arm is a
                            // different shape and stays supported — the referenced struct owns its
                            // own keys, so `{ x: uint // kv }` writes a conformant 2-entry map.)
                            MapKeyKind::Fixed(FixedValue::Uint(_) | FixedValue::Text(_)) => {
                                let source_name = types
                                    .source_rule_name(name)
                                    .map(str::to_owned)
                                    .unwrap_or_else(|| name.to_string());
                                let group_name =
                                    match ty.conceptual_type.resolve_alias_shallow() {
                                        ConceptualRustType::Rust(group_ident) => types
                                            .source_rule_name(group_ident)
                                            .map(str::to_owned)
                                            .unwrap_or_else(|| group_ident.to_string()),
                                        // unreachable while `is_basic` is the guard, which only
                                        // says true for a `Rust` ident — kept total rather than
                                        // asserted, since the message is the whole point here.
                                        _ => ty.for_variant().to_string(),
                                    };
                                record_plain_group_map_member_rejection(
                                    types,
                                    &format!("rule `{source_name}`"),
                                    &ident_name,
                                    &group_name,
                                );
                                None
                            }
                            MapKeyKind::Fixed(other) => {
                                let source_name = types
                                    .source_rule_name(name)
                                    .map(str::to_owned)
                                    .unwrap_or_else(|| name.to_string());
                                types.record_rejection(format!(
                                    "rule `{source_name}`: unsupported map key kind in a group-choice \
                                     arm (only uint/text keys are supported): {other:?}"
                                ));
                                None
                            }
                            MapKeyKind::Keyless if serialize_as_embedded => {
                                // plain-group reference: the referenced struct owns its own keys.
                                None
                            }
                            MapKeyKind::Keyless => {
                                let source_name = types
                                    .source_rule_name(name)
                                    .map(str::to_owned)
                                    .unwrap_or_else(|| name.to_string());
                                types.record_rejection(format!(
                                    "rule `{source_name}`: a map group-choice arm has an entry with \
                                     no key. Each map entry needs a key: use `k: v` / `k => v`, or a \
                                     table `{{ * k => v }}`."
                                ));
                                None
                            }
                            MapKeyKind::NonFixed => {
                                let source_name = types
                                    .source_rule_name(name)
                                    .map(str::to_owned)
                                    .unwrap_or_else(|| name.to_string());
                                types.record_rejection(format!(
                                    "rule `{source_name}`: a map group-choice arm has a non-fixed key \
                                     (`k => v`). Collapsing it into an enum variant would drop the key \
                                     type; this is unsupported. Use a fixed key (`k: v`) or a table \
                                     `{{ * k => v }}` in its own rule."
                                ));
                                None
                            }
                        }
                    } else {
                        None
                    };
                    EnumVariant::new(
                        variant_ident,
                        ty,
                        serialize_as_embedded,
                        rule_metadata.comment.clone(),
                    )
                    .with_key(variant_key)
                    // None => {
                    //     // TODO: Weird case, group choice with only one fixed-value field.
                    //     // What should we do here? In the future we could make this a
                    //     // non-value-taking enum then handle this in the serialization code.
                    //     // However, for now we just default to default behavior:
                    //     let variant_name = format!("{}{}", name, i);
                    //     // TODO: Should we generate these within their own namespace?
                    //     codegen_group_choice(global, group_choice, &variant_name, rep, None);
                    //     EnumVariant::new(variant_name.clone(), RustType::Rust(variant_name), true)
                    // },
                } else {
                    let (ident_name, explicit_name) = match rule_metadata.name.clone() {
                        Some(explicit) => (explicit, true),
                        None => (format!("{name}{i}"), false),
                    };
                    // General case, GroupN type identifiers and generate group choice since it's inlined here
                    let arm_ident = RustIdent::new(CDDLIdent::new(ident_name.clone()));
                    // The arm's record is built through the normal registration path, so it must
                    // occupy `arm_ident` in the global maps while `parse_group_choice` runs. If
                    // something else already claims that name, borrowing it would OVERWRITE the real
                    // owner — and for an embeddable arm the `remove_rust_struct` below would then
                    // delete it outright, so a rule referenced elsewhere silently vanishes from the
                    // IR. Test for that up front and, when it fires, build the arm under a
                    // synthesized name instead.
                    //
                    // The test is order-INDEPENDENT, which is the whole point: every rule ident is
                    // scope-marked before the parse loop starts, and the two-arms-one-name case
                    // rejects from whichever arm is parsed second regardless of which that is. An
                    // order-DEPENDENT test would make the same spec pass or fail on the reference
                    // edges that happen to exist elsewhere in it.
                    let collision = arm_ident_collision(types, name, &arm_ident);
                    let register_under = match &collision {
                        // Named after the owning rule AND the arm, not an opaque counter: a
                        // rejection raised deeper in the arm's own parse (a keyword field name, say)
                        // reports the struct it is building, and that name has to lead the author
                        // back to the arm they wrote.
                        Some(_) => types.fresh_synthesized_ident(&format!(
                            "{name}_group_choice_arm_{ident_name}"
                        )),
                        None => arm_ident.clone(),
                    };
                    types.mark_plain_group(
                        register_under.clone(),
                        PlainGroupInfo::new(None, RuleMetadata::default()),
                    );
                    parse_group_choice(
                        types,
                        parent_visitor,
                        group_choice,
                        &register_under,
                        rep,
                        None,
                        generic_params.clone(),
                        None,
                        // This record IS a multi-arm group-choice arm — reject a rest row in it.
                        true,
                        cli,
                    );
                    // The variant's DISPLAY name always comes from the arm's own ident, never from
                    // whatever the record was registered under: `Credential::Script` is public API of
                    // the generated crate and must survive a synthesized registration. It is settled
                    // against the ENUM's namespace, which is a different namespace from the struct
                    // one `arm_ident_collision` above guards — an embeddable arm registers no struct
                    // at all, and even two arms sharing one struct by structural equality still
                    // declare two variants.
                    let variant_name = settle_arm_variant_name(
                        types,
                        name,
                        &mut variant_names_used,
                        &mut explicit_variant_names,
                        arm_ident.to_string(),
                        &ident_name,
                        explicit_name,
                    );
                    let variant_display = if variant_name == arm_ident.as_ref() {
                        VariantIdent::new_rust(arm_ident.clone())
                    } else {
                        VariantIdent::new_custom(variant_name)
                    };
                    let variant_ident = ConceptualRustType::Rust(register_under.clone());
                    if EnumVariant::can_embed_fields(types, &variant_ident) {
                        // Embeddable: the record is pulled back out and inlined into the variant, so
                        // it is never emitted under a name of its own and a collision here is
                        // harmless once the registration stopped borrowing the contested one.
                        let embedded_record =
                            match types.remove_rust_struct(&register_under).unwrap().variant {
                                RustStructType::Record(record) => record,
                                _ => unreachable!(),
                            };
                        EnumVariant::new_embedded(
                            variant_display,
                            embedded_record,
                            rule_metadata.comment.clone(),
                        )
                    } else {
                        // Non-embeddable: the record SURVIVES and is emitted as a real type under
                        // `arm_ident`. Settle what it is finally named.
                        let final_ident = match &collision {
                            None => {
                                types.claim_group_choice_arm_ident(
                                    arm_ident.clone(),
                                    source_rule_name_of(types, name),
                                );
                                register_under.clone()
                            }
                            // Two arms wanting one name is only a CONFLICT if they are actually
                            // different types. Generic arm names (`first`/`second`, `key`/`value`)
                            // recur across rules by nature, and identical arms are one type spelled
                            // twice — they share the single struct the first claimant registered,
                            // which is also what the pre-check generator emitted for them. This stays
                            // order-independent: the shapes match (or don't) regardless of which arm
                            // the rule order reaches first.
                            Some(ArmIdentClaimant::Arm(_))
                                if types
                                    .rust_struct(&arm_ident)
                                    .zip(types.rust_struct(&register_under))
                                    .is_some_and(|(claimed, ours)| {
                                        claimed.structurally_equivalent(ours)
                                    }) =>
                            {
                                types.remove_rust_struct(&register_under);
                                arm_ident.clone()
                            }
                            // A real conflict: differing arms, or an arm against a RULE's name. There
                            // is no rename here that isn't a silent change to the generated public
                            // API, so the author picks. (A rule collision is never shared onto, even
                            // for a matching shape: a rule the arm is aliasing onto may not be parsed
                            // yet, so comparing shapes there WOULD depend on rule order.)
                            Some(claimant) => {
                                reject_group_choice_arm_ident_collision(
                                    types,
                                    name,
                                    &ident_name,
                                    &arm_ident,
                                    claimant,
                                );
                                register_under.clone()
                            }
                        };
                        EnumVariant::new(
                            variant_display,
                            ConceptualRustType::Rust(final_ident).into(),
                            true,
                            rule_metadata.comment.clone(),
                        )
                    }
                }
            })
            .collect();
        let rule_metadata = merge_metadata(
            &RuleMetadata::from(
                get_comment_after(parent_visitor, &CDDLType::from(group), None).as_ref(),
            ),
            parent_rule_metadata,
        );
        // A group-choice rule generates an enum — a non-collection, so `@duplicates` can never apply
        // (nor `@ignore`, which is only valid on an open struct-map rest row).
        if rule_metadata.duplicates.is_some() {
            reject_duplicates_not_applicable(types, name);
        }
        if rule_metadata.ignore {
            reject_ignore_not_applicable(types, name);
        }
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_group_choice(name.clone(), tag, Some(&rule_metadata), variants, rep),
            cli,
        );
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
fn get_comment_after<'a>(
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

#[allow(unused)]
fn get_rule_name<'a>(
    parent_visitor: &'a ParentVisitor,
    cddl_type: &CDDLType<'a, '_>,
) -> Identifier<'a> {
    match cddl_type {
        CDDLType::CDDL(_) => panic!("Cannot get the rule name of a top-level CDDL node"),
        CDDLType::Rule(t) => match t {
            Rule::Type { rule, .. } => get_rule_name(parent_visitor, &CDDLType::from(rule)),
            Rule::Group { rule, .. } => {
                get_rule_name(parent_visitor, &CDDLType::from(rule.as_ref()))
            }
        },
        CDDLType::TypeRule(t) => t.name.clone(),
        CDDLType::GroupRule(t) => t.name.clone(),
        other => get_rule_name(parent_visitor, other.parent(parent_visitor).unwrap()),
    }
}
