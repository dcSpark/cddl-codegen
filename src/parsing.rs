use crate::cli::Cli;
use cddl::ast::parent::ParentVisitor;
use cddl::{ast::*, token};
use std::collections::BTreeMap;

use crate::comment_ast::{RuleMetadata, merge_metadata, metadata_from_comments};
use crate::intermediate::{
    AliasIdent, AliasInfo, CBOREncodingOperation, CDDLIdent, ConceptualRustType, EnumVariant,
    FixedValue, FloatWindow, GenericDef, GenericInstance, IntermediateTypes, ModuleScope,
    PlainGroupInfo, Primitive, Representation, RustField, RustIdent, RustRecord, RustStruct,
    RustStructType, RustType, VariantIdent, reserved_pin_rejection,
};
use crate::utils::{
    append_number_if_duplicate, convert_to_camel_case, convert_to_snake_case,
    is_identifier_user_defined,
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
                // (1) is_type_choice_alternate is ignored here because only the INITIAL definition
                //     of an identifier via `/=` reaches this point. That case is valid cddl (the
                //     shelley precedent — a lone `b /= tstr` is equivalent to `b = tstr`), so the
                //     flag carries no extra meaning. The other case — a `/=` rule that EXTENDS an
                //     already-defined identifier with another choice arm — is rejected upstream in
                //     `api::with_types` (via `incremental_choice_extension_rejection`) before it can
                //     reach here and silently drop every arm but the last.
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
            assert_eq!(
                rule.generic_params, None,
                "{}: Generics not supported on plain groups",
                rule.name
            );
            // Freely defined group - the group body itself is already handled in `api::with_types`
            // (`mark_plain_group`); the only per-rule work here is honoring a rule-position
            // `@rust_name` pin, so a consumer reads the dependency's final Rust name across the crate
            // boundary (the extern-interface export appends this pin to every exported group-body
            // row). It was never called on the group-rule path before, so a pin on a plain group was
            // silently dropped — the type-rule paths already call `handle_rust_name_pin`.
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
        let name = cddl_rule.name();
        Some(format!(
            "rule `{name}`: `; @name` does not rename a top-level rule or group — the rule \
             identifier `{name}` is itself the emitted Rust type name. `@name` only renames a \
             struct field, a type-choice variant, or a group-choice arm; to change the emitted \
             type name, rename the `{name}` identifier."
        ))
    } else {
        None
    }
}

/// A graceful-rejection message if `cddl_rule` EXTENDS an already-defined identifier with an
/// incremental choice-extension operator (`/=` type-choice, `//=` group-choice), else `None`.
///
/// Incremental extension is unsupported: `parse_rule` re-registers the rule identifier on each
/// statement, so the LAST definition wins and every earlier arm is silently dropped (`a = int` /
/// `a /= tstr` generates a `tstr`-only type, discarding the `int` base arm). Rather than narrow
/// silently — a decoder that rejects spec-valid CBOR, invisible to round-trip tests — we reject at
/// the parse-walk seam (`api::with_types`), which is the ONLY caller and owns the "already defined"
/// bookkeeping (source-order seen-set). This function classifies the operator (so the message and
/// remedy match) but does NOT itself decide whether the identifier was previously defined: an
/// alternate rule whose identifier is the FIRST definition of that name is valid CDDL (the shelley
/// precedent — equivalent to `=`) and must keep generating, so the caller only invokes this on a
/// repeat.
///
/// Remedies are the supported spellings that model the same shape and are asserted to generate in
/// `incremental_choice_extension_rejects_gracefully`: for `/=`, fold the arms into one type-choice
/// rule (`a = int / tstr`); for `//=`, a plain group rule cannot itself carry a group choice
/// (`api::with_types`' `mark_plain_group` asserts a single group choice), so give each arm its own
/// named group and select between them at the use site (`t = [ grpA // grpB ]`).
pub fn incremental_choice_extension_rejection(cddl_rule: &cddl::ast::Rule) -> Option<String> {
    let name = cddl_rule.name();
    match cddl_rule {
        cddl::ast::Rule::Type { rule, .. } if rule.is_type_choice_alternate => Some(format!(
            "rule `{name}`: incremental type-choice extension (`/=`) is not supported — \
             re-defining an already-defined identifier with `/=` silently drops every arm but the \
             last, generating a type that models only the final extension arm. Fold the arms into a \
             single type-choice rule instead, e.g. `{name} = <arm1> / <arm2>`."
        )),
        cddl::ast::Rule::Group { rule, .. } if rule.is_group_choice_alternate => Some(format!(
            "rule `{name}`: incremental group-choice extension (`//=`) is not supported — \
             re-defining an already-defined identifier with `//=` silently drops every arm but the \
             last, generating a group that models only the final extension arm. A plain group rule \
             cannot itself carry a group choice, so give each arm its own named group and select \
             between them at the use site, e.g. `{name}_a = (...)`, `{name}_b = (...)`, \
             `t = [ {name}_a // {name}_b ]`."
        )),
        _ => None,
    }
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

/// `@duplicates` on a rule where the policy WILL apply (an array-shaped collection or a table,
/// including the tag-set collapse arms), but the behavior is not yet built. Graceful rejection so no
/// placement is ever silently ignored while the feature is in flight. The literal `@duplicates`
/// token is kept in the message for greppability when the final work packet prunes the plan-file
/// citation.
fn reject_duplicates_not_yet_built(types: &mut IntermediateTypes, name: &RustIdent) {
    let source_name = types
        .source_rule_name(name)
        .map(str::to_owned)
        .unwrap_or_else(|| name.to_string());
    types.record_rejection(format!(
        "@duplicates on rule `{source_name}`: recognized, deliberately not yet built — \
         @duplicates lands in a later work packet of draft/plan-2026-07-20-duplicates-policy.md."
    ));
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
            // the current generic support relies on having a RustStruct to swap out the types with
            // but that won't happen with T / null types since we generate an alias instead
            todo!("support foo<T> = T / null");
        }
        let inner_rust_type = rust_type_from_type1(types, parent_visitor, inner_type2, cli);
        let final_type = match tag {
            Some(tag) => {
                RustType::new(ConceptualRustType::Optional(Box::new(inner_rust_type))).tag(tag)
            }
            None => RustType::new(ConceptualRustType::Optional(Box::new(inner_rust_type))),
        };
        let rule_metadata = RuleMetadata::from(inner_type2.comments_after_type.as_ref());
        // A `T / null` rule collapses to an `Option<T>` alias — a non-collection, so `@duplicates`
        // can never apply here.
        if rule_metadata.duplicates.is_some() {
            reject_duplicates_not_applicable(types, name);
        }
        types.register_type_alias(
            name.clone(),
            AliasInfo::new_from_metadata(final_type, rule_metadata),
        );
    } else {
        let rule_metadata = merge_metadata(
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
        );
        if let Some(demand) = rule_metadata.key_demand {
            types.mark_key_demand(name.clone(), demand);
        }
        if rule_metadata.used_as_elem {
            types.mark_used_as_elem(name.clone());
        }
        // A multi-choice type rule can never be an extern marker, so `@raw_bytes_flavor` cannot
        // apply here — reject loudly rather than silently ignore it.
        if rule_metadata.raw_bytes_flavor {
            types.record_rejection(format!(
                "@raw_bytes_flavor on `{name}`: this tag is only valid on a {EXTERN_MARKER} \
                 rule — it selects the `<ExternName>RawBytes` wrapper flavor for generic instances \
                 whose argument is a {RAW_BYTES_MARKER} type. Remove it from this rule."
            ));
        }
        handle_rust_name_pin(types, name, &rule_metadata);
        let variants = create_variants_from_type_choices(types, parent_visitor, type_choices, cli);
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
            println!(
                "Collapsing rule `{name}` (tag {set_tag} set idiom) into a transparent optionally-tagged collection"
            );
            // The collapse target is an array-shaped collection (or a table) — exactly where
            // `@duplicates` WILL apply, but the behavior is not yet built.
            if rule_metadata.duplicates.is_some() {
                reject_duplicates_not_yet_built(types, name);
            }
            let bounds = base.config.bounds;
            let rust_struct = match base.conceptual_type {
                ConceptualRustType::Array(element_type) => RustStruct::new_array(
                    name.clone(),
                    Some(set_tag),
                    Some(&rule_metadata),
                    *element_type,
                    bounds,
                )
                .as_optionally_tagged(),
                ConceptualRustType::Map(key_type, value_type) => RustStruct::new_table(
                    name.clone(),
                    Some(set_tag),
                    Some(&rule_metadata),
                    *key_type,
                    *value_type,
                    bounds,
                )
                .as_optionally_tagged(),
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
        // never apply (its map arm, if any, must be a named rule).
        if rule_metadata.duplicates.is_some() {
            reject_duplicates_not_applicable(types, name);
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
        "float16" | "float32" => Some(Primitive::F32),
        "float64" => Some(Primitive::F64),
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
                Some(Primitive::F32) | Some(Primitive::F64) => HeadNumeric::Float,
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

/// Message naming the offending rule for a graceful float-constraint rejection. `None` (member
/// position) omits the rule name; the op + remedy still make the message actionable.
fn float_reject_rule_prefix(rule_name: Option<&RustIdent>) -> String {
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
                        float_reject_rule_prefix(rule_name)
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
                        float_reject_rule_prefix(rule_name)
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
                    float_reject_rule_prefix(rule_name),
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

fn type2_to_fixed_value(type2: &Type2) -> FixedValue {
    match type2 {
        Type2::UintValue { value, .. } => FixedValue::Uint(*value as u64),
        Type2::IntValue { value, .. } => FixedValue::Nint(*value as i128),
        Type2::FloatValue { value, .. } => FixedValue::Float(*value),
        Type2::TextValue { value, .. } => FixedValue::Text(value.to_string()),
        _ => panic!(
            "Type2: {:?} does not correspond to a supported FixedValue",
            type2
        ),
    }
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
            let range_start = match type2 {
                Type2::UintValue { value, .. } => *value as i128,
                Type2::IntValue { value, .. } => *value as i128,
                Type2::FloatValue { value, .. } => *value as i128,
                _ => panic!("Number expected as range start. Found {:?}", type2),
            };
            let range_end = match operator.type2 {
                Type2::UintValue { value, .. } => value as i128,
                Type2::IntValue { value, .. } => value as i128,
                Type2::FloatValue { value, .. } => value as i128,
                _ => unimplemented!("unsupported type in range control operator: {:?}", operator),
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
                    float_reject_rule_prefix(rule_name)
                ));
                ControlOperator::Range((None, None))
            }
            token::ControlOperator::DEFAULT => {
                ControlOperator::Default(type2_to_fixed_value(&operator.type2))
            }
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
                        // (ledgered in cddl-matrix/ROADMAP.md).
                        types.record_rejection(format!(
                            "{}`.size` on a signed `int` is unsupported — its spec meaning is the `uint .size` window (cbor-wg/cddl#32), which the signed reading mis-enforces; use `uint .size N`, or an explicit range for an N-byte signed int",
                            float_reject_rule_prefix(rule_name)
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

/// The rule-position `@rust_name` metadata for a plain GROUP rule. cddl binds a group rule's TRAILING
/// comment (`grp = (a: uint) ; @rust_name X`) to the LAST group entry's trailing comment slot, not
/// `comments_after_group` (empirically verified — that slot is `None` for a single-line group rule),
/// the same slot `group_entry_to_field_name` reads for a field-position `@name`. So read the pin from
/// there, falling back to `comments_after_group` for robustness. ONLY `.rust_name` is lifted onto the
/// rule: a field-position `@name` sharing the last entry's slot legitimately renames that field and is
/// left to the field-naming site, so it must not leak onto the rule here.
fn group_rule_pin_metadata(group: &Group, comments_after_group: Option<&Comments>) -> RuleMetadata {
    let mut metadata = RuleMetadata::from(comments_after_group);
    if metadata.rust_name.is_some() {
        return metadata;
    }
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
        metadata.rust_name = metadata_from_comments(&combined.unwrap_or_default()).rust_name;
    }
    metadata
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
    let rule_metadata = merge_metadata(
        &merge_metadata(
            inherited_metadata,
            &RuleMetadata::from(type1.comments_after_type.as_ref()),
        ),
        &RuleMetadata::from(type_choice.comments_after_type.as_ref()),
    );
    if let Some(demand) = rule_metadata.key_demand {
        types.mark_key_demand(type_name.clone(), demand);
    }
    if rule_metadata.used_as_elem {
        types.mark_used_as_elem(type_name.clone());
    }
    // `@raw_bytes_flavor` is valid ONLY on a `_CDDL_CODEGEN_EXTERN_TYPE_` rule (the extern-marker
    // branch below marks it). Anywhere else it would silently do nothing, so reject loudly here in
    // the house style of the other comment-DSL misuse rejections.
    let is_extern_marker = matches!(
        &type1.type2,
        Type2::Typename { ident, .. } if ident.ident == EXTERN_MARKER
    );
    if rule_metadata.raw_bytes_flavor && !is_extern_marker {
        types.record_rejection(format!(
            "@raw_bytes_flavor on `{type_name}`: this tag is only valid on a {EXTERN_MARKER} \
             rule — it selects the `<ExternName>RawBytes` wrapper flavor for generic instances \
             whose argument is a {RAW_BYTES_MARKER} type. Remove it from this rule."
        ));
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
                    types.mark_raw_bytes_flavor(type_name.clone());
                }
            } else if ident.ident == RAW_BYTES_MARKER {
                types.register_rust_struct(
                    parent_visitor,
                    RustStruct::new_raw_bytes(type_name.clone()),
                    cli,
                );
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
                match control {
                    Some(control) => {
                        assert!(
                            generic_params.is_none(),
                            "Generics combined with range specifiers not supported"
                        );
                        match control {
                            ControlOperator::Range(min_max) => {
                                // when declared top-level we make a new type as the default behavior like before
                                let mut ranged_type = range_to_primitive(
                                    min_max.0,
                                    min_max.1,
                                    ident_to_primitive(&cddl_ident).unwrap(),
                                );
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
                                let ranged_type = float_range_to_primitive(
                                    window,
                                    ident_to_primitive(&cddl_ident).unwrap(),
                                );
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
                                    let cbor_bytes_type = ty.as_bytes().tag_if(outer_tag);
                                    // Same reasoning as the primitive tag rule below: a top-level
                                    // `x = #6.n(bytes .cbor T)` must wrap so its standalone
                                    // `to/from_cbor_bytes` writes/checks the tag (a transparent
                                    // `pub type X = T` alias drops it from the wire). The tag rides on
                                    // `cbor_bytes_type` (`.tag_if(outer_tag)`), so the wrapper emits it.
                                    if rule_metadata.newtype.is_some() || outer_tag.is_some() {
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
                                    } else {
                                        types.register_type_alias(
                                            type_name.clone(),
                                            AliasInfo::new_from_metadata(
                                                cbor_bytes_type,
                                                rule_metadata,
                                            ),
                                        );
                                    }
                                }
                                _ => panic!(".cbor is only allowed on bytes as per CDDL spec"),
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
                                    types.register_type_alias(
                                        type_name.clone(),
                                        AliasInfo::new_from_metadata(
                                            inner_type.default(default_value).tag_if(outer_tag),
                                            rule_metadata,
                                        ),
                                    );
                                }
                            }
                        }
                    }
                    None => {
                        let mut concrete_type = types.new_type(&cddl_ident, cli).tag_if(outer_tag);
                        // Stripping the alias inlines the type for serialization (the rust side stays a
                        // transparent `pub type`). Remember the aliased ident so the WASM alias can point
                        // at its wrapper struct if it has one (resolved at emission via `has_wasm_wrapper`,
                        // so forward references work) — otherwise `for_wasm_member` on the stripped bare
                        // `Map`/`Vec` would emit the inline-only `MapU64To…`/`…List` name (E0425).
                        let mut wasm_alias_target = None;
                        if let ConceptualRustType::Alias(alias_ident, ty) =
                            concrete_type.conceptual_type
                        {
                            if let AliasIdent::Rust(rust_ident) = &alias_ident {
                                wasm_alias_target = Some(rust_ident.clone());
                            }
                            concrete_type.conceptual_type = *ty;
                        };
                        match &generic_params {
                            Some(_params) => {
                                // this should be the only situation where you need this as otherwise the params would be unbound
                                todo!(
                                    "generics on defined types e.g. foo<T, U> = [T, U], bar<V> = foo<V, uint>"
                                );
                                // TODO: maybe you could do this by resolving it here then storing the resolved one as GenericDef
                            }
                            None => {
                                match generic_args {
                                    Some(arg) => {
                                        // This is for named generic instances such as:
                                        // foo = bar<text>
                                        let generic_args = arg
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
                                        types.register_generic_instance(GenericInstance::new(
                                            type_name.clone(),
                                            RustIdent::new(cddl_ident.clone()),
                                            generic_args,
                                            // author-declared rule name (`foo = bar<text>`), not
                                            // synthesized — keeps its own wasm class / criterion-8 name.
                                            false,
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
                                        if rule_metadata.newtype.is_some() || outer_tag.is_some() {
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
                                            types.register_type_alias(
                                                type_name.clone(),
                                                AliasInfo::new_from_metadata(
                                                    concrete_type,
                                                    rule_metadata,
                                                )
                                                .with_wasm_alias_target(wasm_alias_target),
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
                        float_range_to_primitive(window, Primitive::F64),
                        window,
                        outer_tag,
                        rule_metadata,
                        cli,
                    );
                }
                _ => {
                    types.register_type_alias(
                        type_name.clone(),
                        AliasInfo::new_from_metadata(
                            RustType::from(fallback_type).tag_if(outer_tag),
                            rule_metadata,
                        ),
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
                        float_range_to_primitive(window, Primitive::F64),
                        window,
                        outer_tag,
                        rule_metadata,
                        cli,
                    );
                }
                _ => {
                    types.register_type_alias(
                        type_name.clone(),
                        AliasInfo::new_from_metadata(
                            RustType::from(fallback_type).tag_if(outer_tag),
                            rule_metadata,
                        ),
                    );
                }
            }
        }
        Type2::TextValue { value, .. } => {
            types.register_type_alias(
                type_name.clone(),
                AliasInfo::new_from_metadata(
                    RustType::new(ConceptualRustType::Fixed(FixedValue::Text(
                        value.to_string(),
                    )))
                    .tag_if(outer_tag),
                    rule_metadata,
                ),
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
                        float_range_to_primitive(window, Primitive::F64),
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
                    let base_type = range_to_primitive(min_max.0, min_max.1, Primitive::F64);
                    types.register_type_alias(
                        type_name.clone(),
                        AliasInfo::new_from_metadata(base_type.tag_if(outer_tag), rule_metadata),
                    );
                }
                _ => {
                    types.register_type_alias(
                        type_name.clone(),
                        AliasInfo::new_from_metadata(
                            RustType::from(fallback_type).tag_if(outer_tag),
                            rule_metadata,
                        ),
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
    cli: &Cli,
) -> Vec<EnumVariant> {
    let mut variant_names_used = BTreeMap::<String, u32>::new();
    type_choices
        .iter()
        .map(|choice| {
            let rust_type = rust_type_from_type1(types, parent_visitor, &choice.type1, cli);
            // The cddl parser attaches a type-choice element's trailing comment to
            // TypeChoice.comments_after_type, not Type1.comments_after_type, so merge both — otherwise
            // @name/@doc on a variant is silently dropped. Mirrors parse_type's merge for single types.
            let rule_metadata = merge_metadata(
                &RuleMetadata::from(choice.type1.comments_after_type.as_ref()),
                &RuleMetadata::from(choice.comments_after_type.as_ref()),
            );
            let base_name = match &rule_metadata {
                RuleMetadata {
                    name: Some(name), ..
                } => convert_to_camel_case(name),
                _ => rust_type.for_variant().to_string(),
            };
            let variant_name = append_number_if_duplicate(&mut variant_names_used, base_name);
            EnumVariant::new(
                VariantIdent::new_custom(variant_name),
                rust_type,
                false,
                rule_metadata.comment.clone(),
            )
        })
        .collect()
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
    /// bounds (a min-cardinality constraint on the table itself). Only `None` (unbounded `*` table)
    /// and `Some((Some(1), None))` (non-empty `+` / `1*` table → `NonEmptyMap`) ever occur; every
    /// other count-permitting marker is rejected gracefully at the detection arm (silent widening is
    /// the bug being removed), so this never carries an unhonored bound.
    HomogenousMap(RustType, RustType, Option<(Option<i128>, Option<i128>)>),
    /// Fields are different - needs new struct created e.g. field: [a: uint, b: bstr]
    /// This case covers both maps and arrays
    Heterogenous,
    /// Special case for single basic group e.g. field: [basic_group], field: {basic_group}
    /// The tuple type will already have the basic override set so can be directly used
    /// to generate (de)serialiation codegen.
    WrappedBasicGroup(RustType),
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
                                    // table cardinality. Only two markers are honored; every other
                                    // count-permitting marker is rejected gracefully rather than
                                    // silently WIDENED to a 0..N table (the generated decoder would
                                    // wrongly accept out-of-window maps — a certified over-acceptance
                                    // class). NOT applied in the InlineGroup table arm below: there
                                    // the semantic occurrence is the inline group's own marker
                                    // (`{ * (k => v) }`), and the inner entry's missing occur means
                                    // nothing. Fixed keys above keep falling through — `{ 1 => uint }`
                                    // is RFC-equal to the colon spelling and routes to the record path.
                                    //
                                    //   (none)   — RFC 8610 exactly-once; widening to 0..N is the bug
                                    //   `*`/`0*` — unbounded 0..N table (bounds `None`), unchanged
                                    //   `+`/`1*` — non-empty table (`NonEmptyMap`), bounds (Some(1),None)
                                    //   else     — bounded (`?` / `n*m` / `*n` / `n*` / `0*n`): reject
                                    //
                                    // cite the rule by its SOURCE spelling when we have one (the user
                                    // is looking at their CDDL, not our output); anonymous nested maps
                                    // describe the entry instead.
                                    let site = match rule_name {
                                        Some(name) => format!(
                                            "rule `{}`",
                                            types
                                                .source_rule_name(name)
                                                .map(str::to_owned)
                                                .unwrap_or_else(|| name.to_string())
                                        ),
                                        None => "inline map".to_owned(),
                                    };
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
                                        None => {
                                            types.record_rejection(format!(
                                                "{site}: the map entry `{t1} => {value}` has no \
                                                 occurrence indicator, which per RFC 8610 means the \
                                                 entry occurs exactly once; treating it as a 0..N \
                                                 table would silently widen that occurrence (the \
                                                 generated decoder would wrongly accept e.g. an \
                                                 empty map). For a table, spell the occurrence \
                                                 explicitly: `{{ * {t1} => {value} }}` (unbounded) \
                                                 or `{{ + {t1} => {value} }}` (at least one entry)."
                                            ));
                                            None
                                        }
                                        // `*` / `0*`: the unbounded table this crate has always
                                        // generated (bounds carry no min/max).
                                        Some((None, None)) => None,
                                        // `+` / `1*`: a non-empty table — `NonEmptyMap`, enforced via
                                        // the single `TryFrom` door (wire + API report identical
                                        // errors), exactly like `[+ T]` arrays.
                                        Some((Some(1), None)) => Some((Some(1), None)),
                                        // `?` / `n*m` / `*n` / `n*` (n≥2) / `0*n`: a real bounded
                                        // cardinality this phase does not honor. Widening it to 0..N
                                        // is the over-acceptance bug being removed; reject gracefully.
                                        Some(_) => {
                                            types.record_rejection(format!(
                                                "{site}: the map entry `{t1} => {value}` has a \
                                                 bounded occurrence marker (`?` / `n*m` / `*n` / \
                                                 `n*` with n≥2 / `0*n`), which this version does not \
                                                 honor as a real table cardinality; treating it as a \
                                                 0..N table would silently widen the bound (the \
                                                 generated decoder would wrongly accept out-of-window \
                                                 maps). Use `*` for an unbounded table \
                                                 (`{{ * {t1} => {value} }}`), or `+` for a non-empty \
                                                 table (`{{ + {t1} => {value} }}`)."
                                            ));
                                            None
                                        }
                                    };
                                    // keep parsing on the harmless table path — any rejection above
                                    // surfaces as a graceful Err at `finalize`, and nothing may panic
                                    // in between.
                                    let value_type =
                                        rust_type(types, parent_visitor, &ge.entry_type, cli);
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
        GroupEntry::InlineGroup { group, .. } => panic!(
            "not implemented (define a new struct for this!) = {}\n\n {:?}",
            group, group
        ),
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
            assert!(matches!(
                base_type.conceptual_type.resolve_alias_shallow(),
                ConceptualRustType::Primitive(Primitive::Bytes)
            ));
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
                float_range_to_primitive(window, Primitive::F64)
            }
            _ => base_type.with_float_bounds(window),
        },
        Some(ControlOperator::Default(default_value)) => base_type.default(default_value),
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
            let args_name = generic_args
                .iter()
                .map(|t| t.for_variant().to_string())
                .collect::<Vec<String>>()
                .join("_");
            let instance_cddl_ident = CDDLIdent::new(format!("{cddl_ident}_{args_name}"));
            let instance_ident = RustIdent::new(instance_cddl_ident.clone());
            let generic_ident = RustIdent::new(cddl_ident);
            types.register_generic_instance(GenericInstance::new(
                instance_ident,
                generic_ident,
                generic_args,
                // synthesized name for an anonymous use site (`[a: bar<text>]` → `BarText`): when
                // this resolves to a transparent collection, its wasm wrapper lowers to the
                // STRUCTURAL name, not this synthesized ident (see the anonymous-collapse convergence).
                true,
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
                            if let ConceptualRustType::Rust(element_ident) =
                                &element_type.conceptual_type
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
                            let rule_metadata = RuleMetadata::from(
                                get_comment_after(parent_visitor, &CDDLType::from(type2), None)
                                    .as_ref(),
                            );
                            let name = match rule_metadata.name.as_ref() {
                                Some(name) => name,
                                None => panic!(
                                    "Anonymous groups not allowed. Either create an explicit rule (foo = [0, bytes]) or give it a name using the @name notation. Group: {:#?}",
                                    group
                                ),
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
                            match bounds {
                                Some(bounds) => map_type.with_bounds(bounds),
                                None => map_type,
                            }
                        }
                        _ => unimplemented!("TODO: non-table types as types: {:?}", group),
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
            rust_type(types, parent_visitor, t, cli).tag(tag_unwrap)
        }
        Type2::ParenthesizedType { pt, .. } => rust_type(types, parent_visitor, pt, cli),
        _ => {
            panic!("Ignoring Type2: {:?}", type2);
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
            // T / null   or   null / T   should map to Option<T>
            let a = &t.type_choices[0].type1;
            let b = &t.type_choices[1].type1;
            if type2_is_null(&a.type2) {
                return ConceptualRustType::Optional(Box::new(rust_type_from_type1(
                    types,
                    parent_visitor,
                    b,
                    cli,
                )))
                .into();
            }
            if type2_is_null(&b.type2) {
                return ConceptualRustType::Optional(Box::new(rust_type_from_type1(
                    types,
                    parent_visitor,
                    a,
                    cli,
                )))
                .into();
            }
        }
        let variants =
            create_variants_from_type_choices(types, parent_visitor, &t.type_choices, cli);
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
        let combined_ident = RustIdent::new(CDDLIdent::new(&combined_name));
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_type_choice(combined_ident, None, Some(&rule_metadata), variants, cli),
            cli,
        );
        types.new_type(&CDDLIdent::new(combined_name), cli)
    }
}

fn group_entry_optional(entry: &GroupEntry) -> bool {
    let occur = match entry {
        GroupEntry::ValueMemberKey { ge, .. } => &ge.occur,
        GroupEntry::TypeGroupname { ge, .. } => &ge.occur,
        GroupEntry::InlineGroup { .. } => panic!("inline group entries are not implemented"),
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
        GroupEntry::InlineGroup { .. } => panic!("inline group entries are not implemented"),
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
            Some(MemberKey::Type1 { t1, .. }) => match &t1.type2 {
                Type2::UintValue { value, .. } => {
                    MapKeyKind::Fixed(FixedValue::Uint(*value as u64))
                }
                Type2::IntValue { value, .. } => {
                    MapKeyKind::Fixed(FixedValue::Nint(*value as i128))
                }
                Type2::TextValue { value, .. } => {
                    MapKeyKind::Fixed(FixedValue::Text(value.to_string()))
                }
                Type2::FloatValue { value, .. } => MapKeyKind::Fixed(FixedValue::Float(*value)),
                // `true`/`false` are boolean literals spelled as typenames; classify them as fixed
                // Bool so a routed `{ true => uint }` gets the honest "unsupported fixed map key
                // Bool(true)" rejection instead of the misleading non-fixed message (this also
                // upgrades the group-choice-arm message for bool keys). Other typename keys stay
                // NonFixed.
                Type2::Typename { ident, .. } if ident.ident == "true" => {
                    MapKeyKind::Fixed(FixedValue::Bool(true))
                }
                Type2::Typename { ident, .. } if ident.ident == "false" => {
                    MapKeyKind::Fixed(FixedValue::Bool(false))
                }
                _ => MapKeyKind::NonFixed,
            },
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

fn parse_record_from_group_choice(
    types: &mut IntermediateTypes,
    rep: Representation,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    group_choice: &GroupChoice,
    cli: &Cli,
) -> RustRecord {
    let mut generated_fields = BTreeMap::<String, u32>::new();
    let fields = flatten_group_entries(&group_choice.group_entries, rep)
        .into_iter()
        .enumerate()
        .filter_map(|(index, (group_entry, optional_comma))| {
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
                    types.record_rejection(format!(
                        "rule `{source_name}`: an inline group choice (`(a // b)`) in entry \
                         position is unsupported. Name the group instead (e.g. `g = (a // b)`, \
                         then reference `g`)."
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
                        let source_name = types
                            .source_rule_name(name)
                            .map(str::to_owned)
                            .unwrap_or_else(|| name.to_string());
                        types.record_rejection(format!(
                            "rule `{source_name}`: a non-fixed key (`k => v`) mixed into a record \
                             map is unsupported. Use a fixed uint/text key (`k: v`), or a table \
                             `{{ * k => v }}` in its own rule."
                        ));
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
            let rule_metadata = group_entry_rule_metadata(group_entry, optional_comma);
            // `@raw_bytes_flavor` only applies to a `_CDDL_CODEGEN_EXTERN_TYPE_` rule definition,
            // never a field/member position — reject loudly instead of silently ignoring it.
            if rule_metadata.raw_bytes_flavor {
                let source_name = types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string());
                types.record_rejection(format!(
                    "@raw_bytes_flavor on field `{field_name}` of rule `{source_name}`: this tag \
                     is only valid on a {EXTERN_MARKER} rule definition, not a field. Remove it \
                     from this entry."
                ));
            }
            // `@duplicates` is per-rule and never applies at a field/member position — reject loudly
            // instead of silently ignoring it. The remedy names the collection as its own rule.
            if rule_metadata.duplicates.is_some() {
                let source_name = types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string());
                types.record_rejection(format!(
                    "@duplicates on field `{field_name}` of rule `{source_name}`: this directive \
                     is per-rule and does not apply to a field/member position. Name the collection \
                     as its own rule and put `; @duplicates <preserve|reject>` on that rule."
                ));
            }
            // does not exist for fixed values importantly
            let field_type = group_entry_to_type(types, parent_visitor, group_entry, cli);
            if let ConceptualRustType::Rust(ident) = &field_type.conceptual_type {
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
                         different repetition count). Use `?` for an optional item, a homogeneous \
                         array (`[* t]`), or name the repeated part as its own array rule."
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
            Some(RustField::new(
                field_name,
                field_type,
                optional_field,
                key,
                rule_metadata,
            ))
        })
        .collect();
    RustRecord { rep, fields }
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
                // Array-shaped collection — exactly where `@duplicates` WILL apply, not yet built.
                if rule_metadata.duplicates.is_some() {
                    reject_duplicates_not_yet_built(types, name);
                }
                // A plain group used as the array element (`pair = (int, tstr)`, `a = [* pair]`) must be
                // registered as a concrete Array-rep rust struct, exactly like the anonymous member-array
                // path (`rust_type_from_type2`'s `Type2::Array` arm) and the record path both do. Without
                // this the element ident stays an unregistered plain group and `is_enum`/`for_rust_member`
                // trip their "must be a struct or a generic instance" assert at generation time.
                if let ConceptualRustType::Rust(element_ident) = &element_type.conceptual_type {
                    types.set_rep_if_plain_group(
                        parent_visitor,
                        element_ident,
                        Representation::Array,
                        cli,
                    );
                }
                if rule_metadata.newtype.is_some() {
                    // generate newtype over array
                    let mut array_type: RustType =
                        ConceptualRustType::Array(Box::new(element_type)).into();
                    if let Some(bounds) = bounds {
                        array_type = array_type.with_bounds(bounds);
                    }
                    RustStruct::new_wrapper(
                        name.clone(),
                        tag,
                        Some(&rule_metadata),
                        array_type,
                        None,
                    )
                } else {
                    // Array - homogeneous element type with proper occurence operator
                    RustStruct::new_array(
                        name.clone(),
                        tag,
                        Some(&rule_metadata),
                        element_type,
                        bounds,
                    )
                }
            }
            GroupParsingType::HomogenousMap(key_type, value_type, bounds) => {
                // Table collection — where `@duplicates` WILL apply (phase 2), not yet built.
                if rule_metadata.duplicates.is_some() {
                    reject_duplicates_not_yet_built(types, name);
                }
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
                if rule_metadata.newtype.is_some() {
                    // generate newtype over map
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
                // so `@duplicates` can never apply here.
                if rule_metadata.duplicates.is_some() {
                    reject_duplicates_not_applicable(types, name);
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
            cli,
        );
    } else {
        if generic_params.is_some() {
            todo!("{}: generic group choices not supported", name);
        }
        assert!(parent_rule_metadata.newtype.is_none());
        // Generate Enum object that is not exposed to wasm, since wasm can't expose
        // fully featured rust enums via wasm_bindgen

        // TODO: We don't support generating SerializeEmbeddedGroup for group choices which is necessary for plain groups
        // It would not be as trivial to add as we do the outer group's array/map tag writing inside the variant match
        // to avoid having to always generate SerializeEmbeddedGroup when not necessary.
        assert!(!types.is_plain_group(name));

        // Handle group with choices by generating an enum then generating a group for every choice
        let mut variants_names_used = BTreeMap::<String, u32>::new();
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
                    let group_entry = &group_choice.group_entries.first().unwrap().0;
                    let ty = group_entry_to_type(types, parent_visitor, group_entry, cli);
                    let serialize_as_embedded =
                        if let ConceptualRustType::Rust(ident) = &ty.conceptual_type {
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
                    let ident_name = rule_metadata.name.unwrap_or_else(|| {
                        match group_entry_to_raw_field_name(group_entry) {
                            Some(name) => name,
                            None => append_number_if_duplicate(
                                &mut variants_names_used,
                                ty.for_variant().to_string(),
                            ),
                        }
                    });
                    let variant_ident =
                        VariantIdent::new_custom(convert_to_camel_case(&ident_name));
                    // For a MAP-representation arm the single entry carries a member key that must
                    // be written+verified on the wire (dropping it produces malformed CBOR). Carry
                    // the fixed key on the variant; reject non-fixed/keyless entries gracefully
                    // rather than silently miscompiling.
                    let variant_key = if rep == Representation::Map {
                        match group_entry_map_key_kind(group_entry) {
                            // only uint/text keys are supported (parity with the record map path,
                            // which also rejects other fixed key kinds gracefully at parsing)
                            MapKeyKind::Fixed(key @ (FixedValue::Uint(_) | FixedValue::Text(_))) => {
                                Some(key)
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
                    let ident_name = rule_metadata.name.unwrap_or_else(|| format!("{name}{i}"));
                    // General case, GroupN type identifiers and generate group choice since it's inlined here
                    let variant_name = RustIdent::new(CDDLIdent::new(ident_name));
                    types.mark_plain_group(
                        variant_name.clone(),
                        PlainGroupInfo::new(None, RuleMetadata::default()),
                    );
                    parse_group_choice(
                        types,
                        parent_visitor,
                        group_choice,
                        &variant_name,
                        rep,
                        None,
                        generic_params.clone(),
                        None,
                        cli,
                    );
                    let name = VariantIdent::new_rust(variant_name.clone());
                    let variant_ident = ConceptualRustType::Rust(variant_name.clone());
                    if EnumVariant::can_embed_fields(types, &variant_ident) {
                        let embedded_record =
                            match types.remove_rust_struct(&variant_name).unwrap().variant {
                                RustStructType::Record(record) => record,
                                _ => unreachable!(),
                            };
                        EnumVariant::new_embedded(
                            name,
                            embedded_record,
                            rule_metadata.comment.clone(),
                        )
                    } else {
                        EnumVariant::new(
                            name,
                            variant_ident.into(),
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
        // A group-choice rule generates an enum — a non-collection, so `@duplicates` can never apply.
        if rule_metadata.duplicates.is_some() {
            reject_duplicates_not_applicable(types, name);
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
