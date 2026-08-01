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
    head_constrained_float_rejection, nested_cbor_payload_rejection, reserved_pin_rejection,
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
         row (`{{ 1: a, * k => v }} ; @ignore`) or an open-array rest tail (`[ a, * t ] ; @ignore`), \
         where it selects the tolerate-and-drop flavor. It does not apply at a rule, alias, union, \
         table, whole-array, or field position. Remove it, or move it onto the `* k => v` row / `* t` \
         tail of an open struct-map / open array."
    ));
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

/// Reject a custom (de)serializer pair sitting in a collection ROW-ENTRY comment slot — a table row,
/// an open struct-map rest row, an open-array rest tail. The pair is a TYPE-level override keyed on
/// the type whose codec it replaces, and a row entry declares no type of its own, so the directives
/// are read into the row's `RuleMetadata` and dropped. (`@name`, `@duplicates` and `@ignore` are the
/// spellings that slot legitimately carries — they are row-scoped by construction, which is exactly
/// what the pair is not.) `slot` names the row shape and `remedy` names the rule to move the pair
/// onto, both of which differ per call site. Returns whether anything was rejected.
fn reject_custom_codec_on_row_entry(
    types: &mut IntermediateTypes,
    src: &str,
    slot: &str,
    remedy: &str,
    metadata: &RuleMetadata,
) -> bool {
    let found = custom_codec_directives(metadata);
    for directive in &found {
        types.record_rejection(format!(
            "{directive} on the {slot} of rule `{src}`: the custom (de)serializer pair is a \
             TYPE-level override keyed on the type whose codec it replaces, and a row entry \
             declares no type of its own, so it is not honored in this slot. {remedy}"
        ));
    }
    !found.is_empty()
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
/// name class (the head-constrained float names were the first) cannot re-earn the panic.
fn unmapped_control_head_rejection(type_name: &RustIdent, cddl_ident: &CDDLIdent) -> String {
    format!(
        "rule `{type_name}`: a range or `.size` control operator on `{cddl_ident}` is unsupported — \
         the constraint is lowered onto the rust primitive backing the constrained type, and \
         `{cddl_ident}` has no such primitive. Apply the constraint to a concrete numeric, text or \
         byte-string type (`uint`, `int`, `float64`, `tstr`, `bstr`), or remove it."
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
        let inner_rust_type = rust_type_from_type1(types, parent_visitor, inner_type2, cli);
        // A collapse over a FIXED inner (`t = true / null`, `5 / null`, `"v1" / null`, `null /
        // null`) has nowhere to put the value: the collapse builds `Optional(Fixed(..))`, and a
        // `Fixed` in member position is a hard panic at render time (`for_rust_member_ct`), under
        // every profile. Refuse gracefully instead. `resolve_alias_shallow` is the value-carrying
        // form of `is_fixed_value` — both unwrap aliases, so a prelude spelling that arrives
        // alias-wrapped is still recognized (same guard shape as the fixed-array-element one).
        //
        // Registering NOTHING is correct here, unlike the bare-fixed-rule seam in
        // `register_type_alias`, which must keep its alias so a sibling reference can still resolve
        // during parse: this rule registers no rust struct either way, and finalize's
        // registered-nothing check is what already turns a struct-less rule into a loud rejection —
        // so a reference to this rule cannot outrun the rejection recorded here.
        if let ConceptualRustType::Fixed(fixed) =
            inner_rust_type.conceptual_type.resolve_alias_shallow()
        {
            // The `anonymous` fallback is unreachable at this site — a rule-level collapse always
            // has its rule name.
            let site = rejection_site(types, Some(name), "anonymous");
            let message = format!(
                "{site}: the two-arm choice `{} / null` is unsupported {}",
                fixed.cddl_source_desc(),
                fixed_null_collapse_reason(fixed)
            );
            types.record_rejection(message);
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
        // `@used_as_key` / `@used_as_elem` ask for a wasm surface keyed on the rule's own type. The
        // collapse target is `Option<T>`, which is not a class the wasm boundary can key a map or a
        // list on (it is exposed as a nullable of T's own wasm spelling, so the wrapper would either
        // duplicate T's or name no class at all). Reject rather than mark: marking would put the
        // rule into the demand/elem sets and let the wrapper minters silently produce nothing.
        if rule_metadata.key_demand.is_some() {
            types.record_rejection(format!(
                "@used_as_key on `{name}`: a `T / null` rule collapses to a transparent `Option<T>` \
                 alias, which mints no wasm class of its own, so there is nothing for a map-key \
                 wrapper to key on. Put the directive on the rule for the inner type `T` instead."
            ));
        }
        if rule_metadata.used_as_elem {
            types.record_rejection(format!(
                "@used_as_elem on `{name}`: a `T / null` rule collapses to a transparent \
                 `Option<T>` alias, which mints no wasm class of its own, so there is no element \
                 type for a loose-list wrapper to hold. Put the directive on the rule for the inner \
                 type `T` instead."
            ));
        }
        // `@newtype` mints a wrapper STRUCT around the rule's body; this branch registers a
        // transparent alias and no struct, so the directive has nothing to wrap and would silently
        // do nothing (on every other alias-producing rule body it does mint the wrapper, which is
        // what makes the drop a surprise rather than a documented shape).
        if rule_metadata.newtype.is_some() {
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
        let variants = create_variants_from_type_choices(types, parent_visitor, type_choices, cli);
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
                ConceptualRustType::Array(element_type) => RustStruct::new_array(
                    name.clone(),
                    Some(set_tag),
                    Some(&effective_metadata),
                    *element_type,
                    bounds,
                )
                .as_optionally_tagged(),
                ConceptualRustType::Map(key_type, value_type) => RustStruct::new_table(
                    name.clone(),
                    Some(set_tag),
                    Some(&effective_metadata),
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
            // The `.cbor` TARGET is alias-resolved here, so `bytes .cbor bar` (with `bar = uint`)
            // lowers exactly as `bytes .cbor uint` does. Both consumers of this type need that.
            // A rule BODY registers it as a transparent alias, and `register_type_alias`'s first
            // assertion forbids an already-`Alias`-wrapped base — the shape would abort there.
            // A MEMBER or type-choice arm keeps it as the field/variant type, where an alias ident
            // naming no registered struct panics every lookup that assumes one. Resolving loses
            // nothing: a CDDL alias rule over a primitive generates a TRANSPARENT `pub type`, so
            // the alias and its target are the SAME Rust type — only the spelling in the emitted
            // source differs. Aliases are the only case that needs this; a target naming a real
            // struct/collection is already a `Rust` ident and passes through untouched.
            //
            // ONE strip is enough at ANY chain depth (`a = b`, `b = c`, `c = uint`), and that is an
            // invariant rather than a coincidence of the shapes tested: `register_type_alias`
            // refuses an already-`Alias`-wrapped base, so the alias table never stores a nested
            // alias and `resolve_alias`/`new_type` can never hand one back. Each link flattens as
            // it registers — the rule-body registration site strips a single level for the same
            // reason — so a four-link chain and a one-link chain resolve to the identical
            // `RustType` here. Pinned by tests/robustness/cbor_ref_alias_chain.cddl.
            token::ControlOperator::CBOR => {
                let mut target = rust_type_from_type2(types, parent_visitor, &operator.type2, cli);
                if let ConceptualRustType::Alias(_, inner) = target.conceptual_type {
                    target.conceptual_type = *inner;
                }
                ControlOperator::CBOR(target)
            }
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
/// Two spellings put a group rule's trailing comment beyond ANY slot: a closing paren on its own
/// line (`grp = (\n a: uint\n) ; @x`), and a last entry whose slot is already occupied by a
/// field-position `@name`. In both, the parser discards the comment — every AST comment slot is
/// `None` (verified by dumping the AST), so no extraction here can recover it. `Rule::Group`'s own
/// `comments_after_rule` is not an escape hatch either: the parser we pin has exactly two
/// construction sites for it (`pest_bridge.rs`) and BOTH hardcode `None`, so reading it would be
/// dead code. Both directives are silently dropped in those spellings; use the single-line form.
/// Lifting this restriction needs an upstream parser fix — upstream here is the **dcSpark fork** of
/// `cddl` pinned by git rev in `Cargo.toml` (version 0.10.6 at the pinned rev), not the crates.io
/// crate. Tracked in `tests/TESTING_ROADMAP.md`.
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
    let custom_directives = custom_codec_directives(&rule_metadata);
    for directive in &custom_directives {
        // An extern / raw-bytes rule names a type this crate does not define — `new_extern` and
        // `new_raw_bytes` both store `RustStructConfig::default()`, so the pair never reaches
        // generation and BOTH directions emit the named type's own impls. One class, like `@copy`
        // above treats them; the message names the marker the rule actually spells, since this is
        // "invalid HERE" rather than `@copy`'s "valid only on X or Y".
        if let Some(marker) = is_extern_marker
            .then_some(EXTERN_MARKER)
            .or(is_raw_bytes_marker.then_some(RAW_BYTES_MARKER))
        {
            types.record_rejection(format!(
                "{directive} on `{type_name}`: a {marker} rule names a type this crate does \
                 not define, so that type owns its own serialization impls and the custom \
                 (de)serializer pair never reaches generation. Give the rule a real CDDL body and \
                 put the pair there (`<rule> = text ; @custom_serialize <fn> @custom_deserialize \
                 <fn>`) — that states the wire type in the spec and routes the pair through the \
                 type-level alias override."
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
        // `@no_alias` strips the alias node the override is keyed on, so the pair goes with it and
        // both directions fall back to the default wire format — symmetric, and therefore invisible
        // to a round-trip test. Excluded body shapes (`Map`/`Array`, and the tag-head/parenthesized
        // wrappers that recurse back into this function carrying the metadata) route to
        // `parse_group`, where `@no_alias` is inert for a different reason and a named RECORD rule
        // legitimately carries the pair.
        if rule_metadata.no_alias
            && !matches!(
                &type1.type2,
                Type2::Map { .. }
                    | Type2::Array { .. }
                    | Type2::TaggedData { .. }
                    | Type2::ParenthesizedType { .. }
            )
        {
            types.record_rejection(format!(
                "{directive} together with `@no_alias` on `{type_name}`: `@no_alias` removes the \
                 type-alias node the custom (de)serializer override is keyed on, so the pair goes \
                 with it and BOTH directions silently fall back to the default wire format. Drop \
                 `@no_alias` to keep the alias the pair overrides, or drop the pair."
            ));
        }
        // `@newtype` mints a wrapper struct whose `Serialize` impl is generated unconditionally
        // (`wrappers.rs` has no custom handling) while the DESERIALIZE call sites do route through
        // the custom reader — so the pair here is not a drop but a round-trip asymmetry: the wrapper
        // reads one wire format and writes another.
        if rule_metadata.newtype.is_some() {
            types.record_rejection(format!(
                "{directive} together with `@newtype` on `{type_name}`: a `@newtype` wrapper writes \
                 through its own generated serialize impl while the deserialize CALL SITES do route \
                 through the custom reader, so the pair would make the wrapper read one wire format \
                 and write another. Drop `@newtype` and use the plain alias spelling (`<rule> = \
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
                // The head-constrained float prelude names never reach `new_type` on THIS path: a
                // control operator routes the ident through `ident_to_primitive` instead, so a
                // constraint was a side door around the refusal recorded at that fallback —
                // `x = float16 .size 4` generated an `f32`-backed codec at exit 0 (the exact
                // wrong-head-width class the refusal exists to stop), while `float16-32 .size 4`
                // aborted at a bare unwrap. Checked BEFORE the operator is parsed so the type is
                // refused once, rather than alongside a second complaint about the constraint on a
                // type that is not supported anyway. Same message, prefixed with the rule name the
                // way every other constraint rejection on this path is.
                if type1.operator.is_some()
                    && let Some(msg) = head_constrained_float_rejection(&cddl_ident.to_string())
                {
                    types.record_rejection(format!(
                        "{}{msg}",
                        float_reject_rule_prefix(Some(type_name))
                    ));
                    return;
                }
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
                                    // A second `CBORBytes` in ONE chain emits a crate that cannot
                                    // build — see `nested_cbor_payload_rejection`. Record it and
                                    // drop the outer application rather than returning early: the
                                    // rule still registers, as the single-payload type it would
                                    // have been, so every later reference to it resolves the way
                                    // it always did and no second complaint is raised about a
                                    // shape already refused. `finalize` drains the rejection
                                    // before any of it is emitted.
                                    let nested =
                                        ty.encodings.contains(&CBOREncodingOperation::CBORBytes);
                                    if nested {
                                        types.record_rejection(format!(
                                            "{}{}",
                                            float_reject_rule_prefix(Some(type_name)),
                                            nested_cbor_payload_rejection()
                                        ));
                                    }
                                    let cbor_bytes_type = if nested {
                                        ty.tag_if(outer_tag)
                                    } else {
                                        ty.as_bytes().tag_if(outer_tag)
                                    };
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
                                                concrete_type.conceptual_type,
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

/// The shared explanation behind both `T / null` fixed-inner refusals — the tail that follows
/// "… is unsupported" at each of the TWO sites where a two-arm null choice collapses to an
/// `Option<T>` (the rule-level one in `parse_type_choices`, the member-level one in `rust_type`).
/// The sites word their own subject differently because their roles differ (one has a rule name to
/// quote back, the other is a member/element position), but WHY the shape has no representation and
/// WHAT to write instead are one text, so the two spellings of the same defect can't drift apart in
/// what they teach.
///
/// The advertised remedy is probed, not assumed: `bool / null` and `uint / null` generate (exit 0)
/// under both the default and `--preserve-encodings` profiles, and `bool / null` lowers to
/// `Option<bool>`. It carries the same honesty caveat every fixed-value message in this file
/// carries — widening drops the constraint, so the result is a different spec.
fn fixed_null_collapse_reason(fixed: &FixedValue) -> String {
    // `null / null` is a genuinely different sentence: there is no fixed value sitting in the `T`
    // slot, there is no non-`null` arm AT ALL. Sharing the widening remedy would be dishonest —
    // `null` names no CDDL type to widen to (a rule body of bare `null` is itself rejected).
    match fixed {
        FixedValue::Null => "— a two-arm choice with a `null` arm collapses to an `Option<T>` \
             rather than an enum, and with `null` on both arms there is no type left to put in the \
             `T` slot. If a nullable value was meant, one arm must be a non-`null` type (`bool / \
             null` lowers to `Option<bool>`)."
            .to_owned(),
        _ => {
            let value_desc = fixed.cddl_source_desc();
            format!(
                "— a two-arm choice with a `null` arm collapses to an `Option<T>` rather than an \
                 enum, and a fixed value is unstored (it has meaning only as a member whose value \
                 the schema fixes), so there is nothing to put in the `T` slot. Widening the fixed \
                 arm to the CDDL type the constant inhabits generates (`bool / null` lowers to \
                 `Option<bool>`), but it no longer constrains that arm to {value_desc}, so it is a \
                 different spec, not an equivalent one."
            )
        }
    }
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
         has meaning as a single (unstored) member whose value the schema fixes. Naming it as its \
         own rule does not help: a top-level bare fixed value is rejected for the same reason. \
         Widening the value to the CDDL type the constant inhabits (`uint` / `bool` / `tstr` / …) \
         generates, but it no longer constrains the value to {value_desc}, so it is a different \
         spec, not an equivalent one."
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
                let (elem_type, occur, elem_src) = match entry {
                    GroupEntry::ValueMemberKey { ge, .. } => (
                        rust_type(types, parent_visitor, &ge.entry_type, cli),
                        &ge.occur,
                        ge.entry_type.to_string(),
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
                        ge.name.to_string(),
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
                    let site = rejection_site(types, rule_name, "inline array");
                    types.record_rejection(format!(
                        "{site}: the array element `{elem_src}` is a bare fixed value \
                         ({value_desc}) under a count-permitting occurrence marker (`*` / `+` / \
                         `?` / `n*m`), which is unsupported — a fixed value has no element type to \
                         store per repetition, it only has meaning as a single (unstored) member \
                         whose value the schema fixes. Naming it as its own rule does not help: a \
                         top-level bare fixed value is rejected for the same reason. If exactly \
                         one element is meant, drop the marker (`[{elem_src}]`) — that placement \
                         IS supported. Widening the element to the CDDL type the constant \
                         inhabits (`uint` / `bool` / `tstr` / …) generates, but it no longer \
                         constrains the element to {value_desc}, so it is a different spec, not \
                         an equivalent one."
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
/// unambiguous: the anonymous array must be the member's WHOLE type. The ascent is therefore
/// required to be exactly `Type2 -> Type1 -> TypeChoice -> Type -> ValueMemberKeyEntry ->
/// GroupEntry`, over an operator-free `Type1` and a single-choice `Type`. Every other spelling — a
/// `.cbor` payload (whose `Type2`'s parent is the `Operator`), a choice arm, an array nested inside
/// another anonymous array — keeps the anonymous-group rejection rather than guessing which
/// construct the name was meant for.
///
/// Only `.name` is consumed. The same comment is ALSO the field-rename slot, so one `@name` here
/// names both the field and the struct that field holds; every other directive on it keeps the
/// field-level meaning it already had.
fn anon_array_member_name<'a>(
    parent_visitor: &'a ParentVisitor<'a, 'a>,
    type2: &'a Type2<'a>,
) -> Option<String> {
    let type1 = match CDDLType::from(type2).parent(parent_visitor)? {
        CDDLType::Type1(type1) => *type1,
        _ => return None,
    };
    // A control/range operator means the array is the operator's target (`bytes .cbor [..]`), not
    // the member's own type, and the comment after it is the operator chain's, not the array's.
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
    // A choice arm's name would be ambiguous between the arm and the member, and the arm spelling
    // already has its own reachable slot (`TypeChoice::comments_after_type`).
    if entry_type.type_choices.len() != 1 {
        return None;
    }
    let value_member_key = match CDDLType::from(entry_type).parent(parent_visitor)? {
        CDDLType::ValueMemberKeyEntry(value_member_key) => *value_member_key,
        _ => return None,
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
            assert!(matches!(
                base_type.conceptual_type.resolve_alias_shallow(),
                ConceptualRustType::Primitive(Primitive::Bytes)
            ));
            // A second `CBORBytes` in ONE chain emits a crate that cannot build — see
            // `nested_cbor_payload_rejection`. This seam has no rule name to prefix (it serves
            // every member / element / choice-arm position), so the message stands alone. The
            // already-single-payload `ty` is returned unwrapped as the inert placeholder: the walk
            // continues over a type that is shaped like every other `.cbor` member, and `finalize`
            // drains the rejection before it can be emitted.
            if ty.encodings.contains(&CBOREncodingOperation::CBORBytes) {
                types.record_rejection(nested_cbor_payload_rejection());
                ty
            } else {
                ty.as_bytes()
            }
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
/// The INSTANTIATION-derived canonical CDDL ident of a generic invocation:
/// `<def-name>_<args' for_variant() names>` (`set` + `[key_hash]` → `set_KeyHash`, camel-cased to
/// `SetKeyHash` by `RustIdent::new`). The ONE owner of this spelling so every call site — anonymous
/// use (`generic_instance_or_new_type`) and named binding (`foo = bar<text>`) — derives the SAME
/// instantiation identity, which the Phase 2.3 set-nominal dedup keys on.
fn generic_instance_canonical_cddl_ident(
    cddl_ident: &CDDLIdent,
    generic_args: &[RustType],
) -> CDDLIdent {
    let args_name = generic_args
        .iter()
        .map(|t| t.for_variant().to_string())
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
                                    types.record_rejection(format!(
                                        "Anonymous groups not allowed: the inline array `[{group}]` \
                                         is used where a type is required. Either create an explicit \
                                         rule (`foo = [0, bytes]`, then reference `foo`) or give it \
                                         a name using the `@name` notation."
                                    ));
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
                            match bounds {
                                Some(bounds) => map_type.with_bounds(bounds),
                                None => map_type,
                            }
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
                            types.record_rejection(
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
                // A byte-string literal. All three spellings the grammar has are one class: the
                // value is fixed by the schema, and a fixed byte string has no generated
                // representation in a member (`FixedValue` has no bytes variant — a fixed member
                // is unstored, and there is nothing to unstore here). NB `b64'…'` currently fails
                // in the upstream parser before reaching us; it is listed so the class stays
                // complete if that gap closes.
                Type2::B16ByteString { .. }
                | Type2::B64ByteString { .. }
                | Type2::UTF8ByteString { .. } => (
                    "a byte-string literal (`h'…'` / `b64'…'` / `'…'`)".to_string(),
                    " — widening the member to `bytes` generates, but it no longer constrains the \
                     value to that literal, so it is a different spec, not an equivalent one"
                        .to_string(),
                ),
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
            types.record_rejection(format!(
                "{construct} used as a member or element type is unsupported{hint}"
            ));
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
                // The member-position sibling of the rule-level collapse guard in
                // `parse_type_choices`: `a = [v: true / null, x: uint]` builds the same
                // `Optional(Fixed(..))` and panics at the same render site. Reject gracefully with
                // an inert `Fixed(FixedValue::Null)` placeholder — exactly like the other graceful
                // arms in `rust_type_from_type2` — so `finalize` drains it into one clean `Err`.
                // Role-generic wording: no rule name is available here.
                if let ConceptualRustType::Fixed(fixed) =
                    inner_rust_type.conceptual_type.resolve_alias_shallow()
                {
                    let message = format!(
                        "a two-arm `{} / null` choice used as a member or element type is \
                         unsupported {}",
                        fixed.cddl_source_desc(),
                        fixed_null_collapse_reason(fixed)
                    );
                    types.record_rejection(message);
                    return ConceptualRustType::Fixed(FixedValue::Null).into();
                }
                return ConceptualRustType::Optional(Box::new(inner_rust_type)).into();
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
    "index",
    "initial_position",
    "inner",
    "k",
    "key",
    "key_order",
    "list",
    "map",
    "native",
    "opt",
    "pairs",
    "present",
    "read_len",
    "rest_entries",
    "rest_i",
    "rest_key",
    "rest_value",
    "ret",
    "s",
    "serializer",
    "special",
    "string",
    "tag_sz",
    "ty",
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
    // diverts it — so any non-fixed entry here is part of a multi-entry map. `rest_index` marks the
    // recognized (or rest-CANDIDATE-then-rejected) row so the field loop skips it.
    let (rest, rest_index) = recognize_rest_row(
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
            // The rest-row entry (recognized or rejected as a rest candidate) is handled by
            // `recognize_rest_row`; never build a fixed field for it.
            if Some(index) == rest_index {
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
            // `@used_as_elem` names the TYPE whose loose-list wasm wrapper to mint, so it is
            // rule-scoped and never applies at a field/member position — reject loudly instead of
            // silently dropping it. Honoring it here would need a sub-ruling per member shape (an
            // optional field, an inline `[* x]`, a primitive): the tag is only unambiguous when the
            // field's type is a bare named reference. The remedy is exact and already proven, so
            // refuse and name it.
            if rule_metadata.used_as_elem {
                let source_name = types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string());
                types.record_rejection(format!(
                    "@used_as_elem on field `{field_name}` of rule `{source_name}`: this directive \
                     is rule-scoped — it mints the loose-list wasm wrapper for the TYPE it tags — \
                     and does not apply to a field/member position. Put it on the rule that defines \
                     the element type (`<type> = … ; @used_as_elem`)."
                ));
            }
            // `@copy` only applies to a `_CDDL_CODEGEN_EXTERN_TYPE_` / `_CDDL_CODEGEN_RAW_BYTES_TYPE_`
            // rule definition, never a field/member position — reject loudly instead of ignoring it.
            if rule_metadata.copy {
                let source_name = types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string());
                types.record_rejection(format!(
                    "@copy on field `{field_name}` of rule `{source_name}`: this tag is only valid \
                     on a {EXTERN_MARKER} or {RAW_BYTES_MARKER} rule definition, not a field. \
                     Remove it from this entry."
                ));
            }
            // `@extern_companions` only applies to a `_CDDL_CODEGEN_EXTERN_TYPE_` rule definition,
            // never a field/member position — reject loudly instead of silently ignoring it. This is
            // also the slot a plain-GROUP rule's TRAILING comment binds to (`grp = (a: uint) ;
            // @extern_companions …`, the `@name plain-group-trailing` seam), so it covers that
            // spelling too.
            if rule_metadata.extern_companions.is_some() {
                let source_name = types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string());
                types.record_rejection(format!(
                    "@extern_companions on field `{field_name}` of rule `{source_name}`: this tag \
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
                     as its own rule and put `; @duplicates <preserve|reject>` on that rule. \
                     (An inline `#6.258` array in this position already defaults to `@duplicates \
                     reject` via the well-known-tag registry — hoisting it to a named rule with `; \
                     @duplicates preserve` is exactly how to opt out.)"
                ));
            }
            // `@ignore` is the open struct-map rest-row tolerate-and-drop flavor and never applies at
            // a field/member position — reject loudly instead of silently ignoring it.
            if rule_metadata.ignore {
                let source_name = types
                    .source_rule_name(name)
                    .map(str::to_owned)
                    .unwrap_or_else(|| name.to_string());
                types.record_rejection(format!(
                    "@ignore on field `{field_name}` of rule `{source_name}`: this directive is only \
                     valid on an open struct-map rest row (`{{ 1: a, * k => v }} ; @ignore`), not at \
                     a field/member position. Remove it from this entry."
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
                         different repetition count). Use `?` for an optional item, a final-position \
                         `* t` rest tail after the fixed members, a homogeneous array (`[* t]`), or \
                         name the repeated part as its own array rule."
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
    reject_encoding_companion_collisions(types, rep, name, &fields);
    RustRecord { rep, fields, rest }
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
            "rule `{src}`: an open struct-map supports a single trailing rest row (`* k => v`), but \
             this map has {}. Keep one `* k => v` row (last), or move the extras into their own \
             table rules.",
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
    // Extract the key (domain) and value (range) types from the arrow entry.
    let (domain, range) = match candidate_ge {
        GroupEntry::ValueMemberKey { ge, .. } => {
            let domain = match &ge.member_key {
                Some(MemberKey::Type1 { t1, .. }) => {
                    rust_type_from_type1(types, parent_visitor, t1, cli)
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
            (domain, range)
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
    // is the ONE predicate parsing/IR/generation share). Two shapes stay rejected, for reasons the
    // key type itself carries rather than the row's plumbing:
    //
    //   * a NULL-ADMITTING domain (`k = text / null` → `Optional<..>`), rejected here — a `null` key
    //     arrives as CBOR major type 7, the same dispatch arm that carries the indefinite-map BREAK,
    //     so the row cannot tell "the map ended" from "the next key is null" without deciding one of
    //     them wrong.
    //   * a FLOAT-containing domain, rejected in `IntermediateTypes::finalize` beside the table/set
    //     float instruments (floats have no total order, so they can key nothing) — the one place
    //     that also sees a float hidden behind a resolved generic instance.
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
    // NOT rule-position handling (the rest row is by definition the map's last entry, whose slot the
    // cddl parser also binds a rule's trailing comment to; a map/record rule reads its own metadata
    // via `get_comment_after` on the group choice, so the two do not collide — a rest-row directive
    // stays entry-level and a rule directive stays rule-level).
    let rest_metadata = group_entry_rule_metadata(candidate_ge, candidate_comma);
    // A custom (de)serializer pair in this slot is inert (the row declares no type of its own) —
    // reject it here rather than generating default wire in both directions.
    if reject_custom_codec_on_row_entry(
        types,
        &src,
        "open struct-map rest row (`* k => v`)",
        "Name the row's key or value type as its own rule and put the pair there (`k = text ; \
         @custom_serialize <fn> @custom_deserialize <fn>`, then `* k => v`).",
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
    };
    (Some(Box::new(rest_row)), Some(candidate))
}

/// The array-rep analog of `recognize_rest_row`: recognize a final-position `* T` tail (`[a, b, * t]`)
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
    // `n*m` all qualify as tail CANDIDATES here (only `*` is ultimately honored — the rest reject
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
    // Occurrence must be exactly `*` (unbounded capture). `+` / `n*m` are rejected: a `+` tail (at
    // least one unknown element) breaks the empty-tail ≡ closed-struct byte invariant, and bounded
    // cardinalities on a tail are ill-specified.
    let candidate_occur = match candidate_ge {
        GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
        GroupEntry::TypeGroupname { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
        GroupEntry::InlineGroup { .. } => None,
    };
    if !matches!(candidate_occur, Some(Occur::ZeroOrMore { .. })) {
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail must use the `*` occurrence (unbounded capture: \
             `* t`). `+` and `n*m` are not supported on a rest tail."
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
    // Entry-level directives on the tail (`@name`, `@ignore`; `@duplicates` is rejected — no keys),
    // read from the row's own trailing slot (NOT rule-position handling — the tail is by definition
    // the array's last entry, whose slot the cddl parser also binds a rule's trailing comment to; the
    // rule reads its own metadata via `get_comment_after` on the group choice, so the two do not
    // collide). Placement/shape guards above fire FIRST, so `@ignore` on a rejected placement gets the
    // placement rejection, not one of these.
    let tail_metadata = group_entry_rule_metadata(candidate_ge, candidate_comma);
    // A custom (de)serializer pair in this slot is inert (the tail declares no type of its own) —
    // reject it here rather than generating default wire in both directions.
    if reject_custom_codec_on_row_entry(
        types,
        &src,
        "open-array rest tail (`* t`)",
        "Name the tail element type as its own rule and put the pair there (`e = uint ; \
         @custom_serialize <fn> @custom_deserialize <fn>`, then `* e`).",
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
    let rust_struct = match parse_group_type(
        types,
        parent_visitor,
        group_choice,
        rep,
        Some(name),
        cli,
    ) {
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
            if let ConceptualRustType::Rust(element_ident) = &element_type.conceptual_type {
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
            } else if rule_metadata.newtype.is_some() {
                // generate newtype over array. Route through the SAME effective-metadata helper the
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
            // A table's single row carries a trailing comment slot that nothing reads — disjoint from
            // the rule's own slot (a rule-trailing `@duplicates` reaches `rule_metadata`; the same
            // directive spelled on the row does not). A custom (de)serializer pair written there is
            // therefore inert; reject it and point at the key/value rule spelling that works.
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
                    &src,
                    "table row (`* k => v`)",
                    "Name the table's key or value type as its own rule and put the pair there \
                     (`k = text ; @custom_serialize <fn> @custom_deserialize <fn>`, then \
                     `{ * k => v }`).",
                    &row_metadata,
                );
            }
            // Table collection: `reject` is today's default (accepted no-op) and `preserve` is
            // LIVE — the policy rides the transparent alias built in `register_rust_struct`,
            // swapping the member to the `PairMap`/`NonEmptyPairMap` vec-of-pairs twin. Nothing
            // to reject here.
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
                // `@duplicates` on a `@newtype` TABLE is not yet supported: a `preserve` policy
                // would swap the wrapper's inner to the `PairMap` twin, but the synthesized
                // structural map wasm wrapper class wraps `BTreeMap`, not `PairMap`, so the wasm
                // crate would not compile (a preserve-table transparent ALIAS works under wasm only
                // because the named rule itself becomes the `PairMap` wasm class). Rather than emit
                // a broken wasm crate or silently drop the directive, reject loudly and point at the
                // transparent-table-alias workaround. draft/set-architecture-plan.md
                // "Phase 2.2 — named set rules nominalize" wires the PairMap-aware wasm wrapper and
                // subsumes this rejection.
                if rule_metadata.duplicates.is_some() {
                    types.record_rejection(format!(
                            "@duplicates on rule `{name}`: a `@duplicates` policy on a `@newtype` table \
                             (`{{ * k => v }} ; @newtype`) is not yet supported — the PairMap wasm \
                             boundary for the wrapped inner is unwired. Use a transparent table alias \
                             (drop `@newtype`) to carry the policy, or remove `@duplicates` from this \
                             rule."
                        ));
                }
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
                    // A single-entry arm registers no record at all — its type goes straight into
                    // the variant — so the name settled here is the ONLY name it ever claims.
                    let (ident_name, explicit_name) = match rule_metadata.name.clone() {
                        Some(explicit) => (explicit, true),
                        None => match group_entry_to_raw_field_name(group_entry) {
                            Some(field_name) => (field_name, false),
                            None => (ty.for_variant().to_string(), false),
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
