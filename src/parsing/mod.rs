mod comments;
mod control;
mod directives;
mod dynamic_rows;
mod groups;
mod numeric_collection_controls;
mod prepass;
mod records;
mod source_scan;
mod types;

pub use groups::parse_group;
use groups::{
    GroupParsingType, MapKeyKind, flatten_group_entries, group_entry_map_key_kind,
    group_entry_occur, group_entry_optional, group_entry_to_field_name,
    group_entry_to_raw_field_name, group_entry_to_type, inline_group_occurrence_flattens,
    lower_anonymous_record, materialize_plain_group_ref,
    normalized_dynamic_sequence_occurrence_window, occurrence_permits_count, parse_group_type,
};

// Preserve the library facade; the binary reaches the defining child directly.
#[allow(unused_imports)]
pub use types::create_variants_from_type_choices;
pub(crate) use types::generic_instance_canonical_cddl_ident;
use types::{
    anon_composite_member_name, exact_homogeneous_array_length_rejection,
    generic_instance_or_new_type, length_window, null_collapse_inner, parse_type,
    parse_type_choices, rust_type, rust_type_from_type1, rust_type_from_type2, type_to_field_name,
};

use dynamic_rows::{DynamicRows, recognize_dynamic_rows};
use records::parse_record_from_group_choice;

pub(crate) use numeric_collection_controls::rejections as numeric_collection_control_rejections;
pub use prepass::{merge_incremental_type_choice_extensions, repeated_rule_definition_rejections};
pub(crate) use source_scan::{
    inline_group_occurrence_trailing_directive_rejection,
    multiline_group_trailing_directive_rejection,
    named_plain_group_occurrence_trailing_directive_rejection,
};

use comments::{combine_comments, get_comment_after};
use control::{
    ControlOperator, float_range_to_primitive, ident_to_primitive, integer_primitive_domain,
    is_uint_primitive, parse_control_operator, range_to_primitive, register_float_range,
    register_ranged_type, reject_rule_prefix, resolved_head_primitive, type2_to_fixed_value,
    with_optional_bounds,
};
use directives::{
    NonLastArmOwner, RuleBodyShape, apply_inline_table_row_metadata,
    apply_rule_position_directives, group_entry_rule_metadata, group_rule_pin_metadata,
    handle_extern_companions, raw_bytes_flavor_non_generic_extern_rejection,
    reject_custom_codec_on_row_entry, reject_custom_encodings_without_pair,
    reject_duplicates_not_applicable, reject_exact_zero_field_only_metadata,
    reject_field_directives_on_single_entry_arm, reject_ignore_not_applicable,
    reject_member_scoped_directives, reject_newtype_on_nominal_rule,
    reject_non_last_arm_directives, reject_type_scoped_directives, rule_position_metadata,
    single_arm_array_effective_metadata, strip_alias_for_registration, type_choice_metadata,
    well_known_tag_default_duplicates, with_well_known_tag_default,
};

use crate::cli::Cli;
use cddl::ast::parent::ParentVisitor;
use cddl::{ast::*, token};
use std::collections::{BTreeMap, BTreeSet};

use crate::comment_ast::RuleMetadata;
use crate::intermediate::{
    CBOREncodingOperation, CDDLIdent, ConceptualRustType, EnumVariant, FixedValue,
    GenericParamBinding, IntermediateTypes, ModuleScope, Primitive, Representation, RustIdent,
    RustStruct, RustStructType, RustType,
};

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

                let generic_param_scope = rule.generic_params.as_ref().map(|gp| {
                    gp.params
                        .iter()
                        .enumerate()
                        .map(|(ordinal, id)| {
                            (id.param.to_string(), GenericParamBinding::new(ordinal))
                        })
                        .collect::<Vec<_>>()
                });
                let generic_params = generic_param_scope.as_ref().map(|scope| {
                    scope
                        .iter()
                        .map(|(_, binding)| *binding)
                        .collect::<Vec<_>>()
                });
                let parse_body = |types: &mut IntermediateTypes| {
                    if rule.value.type_choices.len() == 1 {
                        let choice = &rule.value.type_choices.first().unwrap();
                        parse_type(
                            types,
                            parent_visitor,
                            &rust_ident,
                            choice,
                            None,
                            generic_params.clone(),
                            &RuleMetadata::default(),
                            None,
                            cli,
                        );
                    } else {
                        parse_type_choices(
                            types,
                            parent_visitor,
                            &rust_ident,
                            &rule.value.type_choices,
                            None,
                            generic_params.clone(),
                            &RuleMetadata::default(),
                            cli,
                        );
                    }
                };
                if let Some(params) = generic_param_scope {
                    types.with_generic_param_scope(params.clone(), |types| {
                        types.with_generic_inline_choice_scope(rust_ident.clone(), parse_body)
                    });
                } else {
                    parse_body(types);
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
            // Read the plain group's rule metadata from the final entry's shared comment slot.
            // group_rule_pin_metadata also reads comments_after_group; field directives retain their field meaning.
            // RuleBodyShape::PlainGroup applies the rule marks and returns before ordinary rule-only checks.
            match &rule.entry {
                cddl::ast::GroupEntry::InlineGroup {
                    group,
                    comments_after_group,
                    ..
                } => {
                    let rust_ident = RustIdent::new(CDDLIdent::new(rule.name.to_string()));
                    let pin_metadata =
                        group_rule_pin_metadata(group, comments_after_group.as_ref());
                    apply_rule_position_directives(
                        types,
                        &rust_ident,
                        &pin_metadata,
                        RuleBodyShape::PlainGroup,
                    );
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

/// Reject a rule-position `@name`: field and variant naming cannot rename a CDDL rule.
/// The type_choices_carry_rule_position_name scan looks through wrappers and nullable collapses;
/// multi-choice enum arms keep their variant names.
/// A plain group's comments_after_group slot is checked independently: the parser normally binds
/// a trailing name to its last entry, where it renames that field instead.
pub fn rule_position_name_rejection(cddl_rule: &cddl::ast::Rule) -> Option<String> {
    let has_rule_position_name = match cddl_rule {
        cddl::ast::Rule::Type { rule, .. } => {
            type_choices_carry_rule_position_name(&rule.value.type_choices)
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

/// Inspect rule slots through tag heads and parentheses. Nullable collapses have no variants;
/// other multi-choice bodies reserve their arm metadata for variant names.
fn type_choices_carry_rule_position_name(choices: &[TypeChoice]) -> bool {
    let in_scope = match choices.len() {
        1 => choices,
        2 if null_collapse_inner(choices).is_some() => choices,
        _ => return false,
    };
    if in_scope
        .iter()
        .any(|choice| type_choice_metadata(choice).name.is_some())
    {
        return true;
    }
    match choices {
        [only] => match &only.type1.type2 {
            Type2::TaggedData { t, .. } => type_choices_carry_rule_position_name(&t.type_choices),
            Type2::ParenthesizedType { pt, .. } => {
                type_choices_carry_rule_position_name(&pt.type_choices)
            }
            _ => false,
        },
        _ => false,
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

/// The literal tag number of a `#6.N(…)` head, or the graceful-rejection message for a head that
/// names no literal number: `#6(…)` (any tag) or a type-valued `#6.<t>(…)` (RFC 9682).
fn tag_number(
    tag: &Option<token::TagConstraint<'_>>,
    rule_name: Option<&RustIdent>,
) -> Result<usize, String> {
    match tag {
        Some(token::TagConstraint::Literal(n)) => Ok(*n as usize),
        Some(token::TagConstraint::Type(raw)) => Err(format!(
            "{}a type-valued tag number (`#6.<{raw}>(…)`, RFC 9682) is unsupported — the \
             generated codec writes and checks one literal tag number. Write the tag number as a \
             literal (`#6.24(…)`).",
            reject_rule_prefix(rule_name)
        )),
        None => Err(format!(
            "{}a tag with no tag number (`#6(…)`, which matches any tag) is unsupported — the \
             generated codec writes and checks one literal tag number. Write the tag number \
             (`#6.24(…)`).",
            reject_rule_prefix(rule_name)
        )),
    }
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
    context: &crate::intermediate::VariantMintContext,
    arm_ordinal: usize,
    base: String,
    arm_source_name: &str,
    explicit: bool,
) -> String {
    if explicit {
        // The shared IR registry pre-reserved this authored spelling before the arm walk. Its
        // collision diagnostic was emitted there, so this is deliberately verbatim.
        return base;
    }
    types.settle_derived_variant_mint(context, arm_ordinal, arm_source_name.to_owned(), base)
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

/// The namespace key a type choice's variant names are reserved and settled under: the owning rule
/// when there is one, otherwise the inline choice itself. `create_variants_from_type_choices`
/// passes it to both the explicit-name pre-reservation and `settle_derived_variant_mint`.
fn choice_variant_context(
    owner: Option<&RustIdent>,
    type_choices: &[TypeChoice],
) -> crate::intermediate::VariantMintContext {
    match owner {
        Some(owner) => crate::intermediate::VariantMintContext::TypeChoice(owner.clone()),
        // The AST address is in-process provenance only (never emitted); unlike the human-facing
        // diagnostic it distinguishes two independent inline enum namespaces in one rule.
        None => {
            crate::intermediate::VariantMintContext::InlineTypeChoice(type_choices.as_ptr().addr())
        }
    }
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
    let occur = group_entry_occur(group_entry);
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
    generic_params: Option<&[GenericParamBinding]>,
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
        let claimant = source_rule_name_of(types, &owner);
        types.record_rejection(format!(
            "fixed singleton `{owner}` for {} collides with the authored rule `{}`. Rename the rule; synthesized fixed/null owners reserve this deterministic name.",
            fixed.cddl_source_desc(),
            claimant,
        ));
        return RustType::new(ConceptualRustType::Rust(owner));
    }

    // This minter has an intentional early owner lookup below. Claim its COMPLETE fixed-value wire
    // shape before that lookup, otherwise a later tagged/bare claimant can be discarded with no
    // trace left for finalized IR validation to inspect.
    let singleton =
        RustStruct::new_fixed_singleton(owner.clone(), tag, rule_metadata, fixed_type.clone());
    types.claim_nominal_mint(
        &singleton,
        format!("fixed singleton for {}", fixed.cddl_source_desc()),
    );

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

    types.register_rust_struct(parent_visitor, singleton, cli);
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

/// RFC 8610 defines ranges only between endpoints of the same numeric kind.
fn mixed_int_float_range_rejection(
    rule_name: Option<&RustIdent>,
    start: &Type2,
    end: &Type2,
    is_inclusive: bool,
) -> String {
    let spell = |t: &Type2| match t {
        Type2::FloatValue { value, .. } => format!("{value:?}"),
        other => other.to_string(),
    };
    format!(
        "{}the range `{}{}{}` mixes an integer and a float endpoint, which is unsupported — RFC \
         8610 §2.2.2.1 defines a range only between two integers (`0..10`) or two floats \
         (`0.0..10.0`). Spell both endpoints as the same kind.",
        reject_rule_prefix(rule_name),
        spell(start),
        if is_inclusive { ".." } else { "..." },
        spell(end),
    )
}

/// A value-comparison control (`.eq`/`.ne`/`.le`/`.lt`/`.ge`/`.gt`) whose operand is not a
/// numeric literal. Rule and member routes, integer and float heads share it.
fn non_literal_control_operand_rejection(
    rule_name: Option<&RustIdent>,
    ctrl: token::ControlOperator,
    operand: &Type2,
) -> String {
    format!(
        "{}the `{ctrl}` operand `{operand}` is not a numeric literal — a value comparison lowers \
         onto a (min, max) window over the rust primitive backing the constrained type, so a named \
         type or expression as the operand has no value to lower. Write a numeric literal operand \
         (`uint .le 255`), or remove the control.",
        reject_rule_prefix(rule_name)
    )
}

/// A `.size` operand that is neither an integer literal nor a parenthesized integer literal /
/// literal range: a name (`.size foo`), a text value, a type choice (`.size (1 / 2)`), or a control
/// (`.size (1 .le 3)`).
fn non_literal_size_operand_rejection(rule_name: Option<&RustIdent>, operand: &Type2) -> String {
    format!(
        "{}the `.size` operand `{operand}` is not an integer literal or an integer literal range — \
         a size is a whole number of bytes, so spell it as a literal (`.size 4`) or a literal range \
         (`.size (1..63)`).",
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

/// `.size` written on a head RFC 8610 §3.8.1 gives no size to, anywhere in `cddl_rule`: a float
/// or `nint` prelude type, or a literal value. Refused in the `api::with_types` pre-scan rather
/// than at a parse seam because the construct is reachable by routes that never consult the
/// operator (a text/bytes literal rule body registers a fixed singleton directly; a literal map
/// key is classified without it), and in member positions a float head reaches generation with an
/// integer window and aborts. One message per offending node, in source order.
pub(crate) fn unsizable_size_head_rejections(cddl_rule: &cddl::ast::Rule) -> Vec<String> {
    struct Scan {
        rule: String,
        found: Vec<String>,
    }
    impl<'a, 'b> cddl::visitor::Visitor<'a, 'b, std::fmt::Error> for Scan {
        fn visit_control_operator(
            &mut self,
            target: &'b Type2<'a>,
            ctrl: token::ControlOperator,
            controller: &'b Type2<'a>,
        ) -> cddl::visitor::Result<std::fmt::Error> {
            if ctrl == token::ControlOperator::SIZE {
                if let Some(head) = unsizable_size_head(target) {
                    self.found.push(format!(
                        "rule `{}`: `.size` on `{head}` is unsupported — RFC 8610 §3.8.1 defines \
                         `.size` for `uint` and for byte and text strings (`uint .size 2`, `bytes \
                         .size 4`, `tstr .size (1..63)`); a float or negative-integer type has no \
                         size to control, and a literal value is already exactly one value. Remove \
                         the control, or apply it to one of those types.",
                        self.rule
                    ));
                } else if let Some(choice) = size_head_type_choice(target) {
                    self.found.push(format!(
                        "rule `{}`: `.size` on the type choice `{choice}` is unsupported — the \
                         control is not distributed over the choice's arms, so it would be \
                         dropped. Write `.size` on each arm that takes one (`tstr .size 3 / bytes \
                         .size 3`), or remove the control.",
                        self.rule
                    ));
                }
            }
            cddl::visitor::walk_control_operator(self, target, controller)
        }
        // The stock walk visits only a typename's identifier; a generic argument
        // (`g<float64 .size 3>`) is a Type1 like any other.
        fn visit_type2(&mut self, t2: &'b Type2<'a>) -> cddl::visitor::Result<std::fmt::Error> {
            if let Type2::Typename {
                generic_args: Some(args),
                ..
            } = t2
            {
                cddl::visitor::walk_generic_args(self, args)?;
            }
            cddl::visitor::walk_type2(self, t2)
        }
    }
    let mut scan = Scan {
        rule: cddl_rule.name(),
        found: Vec::new(),
    };
    // The visitor never returns `Err`: `Scan` only collects.
    let _ = cddl::visitor::Visitor::visit_rule(&mut scan, cddl_rule);
    scan.found
}

/// The spelling of a `.size` head that has no size, or `None` for a sizable (or not
/// syntactically classifiable) head. `uint`, `int` (refused by its own `.size` arm message),
/// `bytes`/`tstr` and every non-prelude name are `None`.
fn unsizable_size_head(head: &Type2) -> Option<String> {
    if let Some(literal) = literal_head_spelling(head) {
        return Some(literal);
    }
    match head {
        // `(float64) .size 3` is `float64 .size 3`: look through a bare single-type parenthesis.
        Type2::ParenthesizedType { pt, .. } => match pt.type_choices.as_slice() {
            [only] if only.type1.operator.is_none() => unsizable_size_head(&only.type1.type2),
            _ => None,
        },
        Type2::Typename { ident, .. } => {
            match ident_to_primitive(&CDDLIdent::new(ident.to_string())) {
                Some(p) if p.is_float() => Some(ident.to_string()),
                Some(Primitive::N64) => Some(ident.to_string()),
                _ => None,
            }
        }
        _ => None,
    }
}

/// The spelling of a literal VALUE head (`3`, `-1`, `1.5`, `"a"`, `h'00'`), looking through bare
/// single-type parentheses (`(3)`), or `None` for any other head.
fn literal_head_spelling(head: &Type2) -> Option<String> {
    match head {
        Type2::FloatValue { value, .. } => Some(format!("{value:?}")),
        Type2::UintValue { .. }
        | Type2::IntValue { .. }
        | Type2::TextValue { .. }
        | Type2::UTF8ByteString { .. } => Some(head.to_string()),
        // Display writes the decoded bytes raw; spell them as the hex literal instead.
        Type2::B16ByteString { value, .. } | Type2::B64ByteString { value, .. } => Some(format!(
            "h'{}'",
            value.iter().map(|b| format!("{b:02X}")).collect::<String>()
        )),
        Type2::ParenthesizedType { pt, .. } => match pt.type_choices.as_slice() {
            [only] if only.type1.operator.is_none() => literal_head_spelling(&only.type1.type2),
            _ => None,
        },
        _ => None,
    }
}

/// A `.size` head that is a parenthesized type CHOICE (`(tstr / float64) .size 3`), looking through
/// bare single-type parentheses (`((tstr / bytes)) .size 3`). Returns the choice as written.
fn size_head_type_choice(head: &Type2) -> Option<String> {
    let Type2::ParenthesizedType { pt, .. } = head else {
        return None;
    };
    match pt.type_choices.as_slice() {
        [only] if only.type1.operator.is_none() => size_head_type_choice(&only.type1.type2),
        [_] => None,
        _ => Some(head.to_string()),
    }
}

// TODO: Also generates individual choices if required, ie for a / [foo] / c would generate Foos

/// How a rejection message names the composite it complains about: the enclosing rule by its
/// SOURCE spelling when there is one (the user is looking at their CDDL, not our camel-cased
/// output), and `anonymous` for a nested composite that has no rule of its own.
fn rejection_site(
    types: &IntermediateTypes,
    rule_name: Option<&RustIdent>,
    anonymous: &str,
) -> String {
    match rule_name {
        Some(name) => format!("rule `{}`", source_rule_name_of(types, name)),
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
/// Guarded on `is_basic` over the RESOLVED arm type — the same predicate and the same resolved-type
/// placement as the record-field and rest-tail twins — so the bare and ALIAS (`kv_alias / null`)
/// spellings land on ONE message. A TAGGED spelling (`#6.10(kv) / null`) reaches the earlier
/// tag-payload semantic refusal instead: the tag requires a one-item TYPE. The array-WRAPPED forms
/// keep their own (supported) verdicts: `w = [kv]` is a Record, not a plain group, and an inline
/// `[kv]` arm carries `basic_override`, so neither is `is_basic`.
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
    let group_name = source_rule_name_of(types, group_ident);
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

/// The source name of the bare plain group a resolved type denotes, if it is one.
///
/// Keyed on `RustType::is_basic` — the SAME predicate `generate_serialize` uses to pick the
/// `serialize_as_embedded_group` (member-splicing) emission over a real `serialize` — plus the
/// authored-group bit. The latter excludes internal group-choice-arm registrations whose generated
/// name can temporarily coincide with a generic parameter (`A`): that parameter is still a TYPE,
/// not an authored group reference. In particular an array-WRAPPED group (`[coords]`, inline or as
/// a named rule) carries `basic_override`, serializes as one nested array item, and is therefore not
/// a bare group here. Aliases resolve through, so `c2 = coords` is caught alongside a direct
/// reference.
fn resolved_plain_group_source_name(types: &IntermediateTypes, ty: &RustType) -> Option<String> {
    match ty.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Rust(ident)
            if ty.is_basic(types) && types.is_directly_defined_plain_group(ident) =>
        {
            Some(source_rule_name_of(types, ident))
        }
        _ => None,
    }
}

/// Reject a plain group used as the payload of a CBOR tag. Syntactically the ambiguous typename
/// node is admitted by the parser; semantically RFC 8610's tag production takes a TYPE, and groups
/// need array/map framing before they become one. Keeping this one resolved-type seam prevents the
/// same invalid spelling from being silently dropped in a homogeneous array, half-honored in an
/// array record, or reported as an unrelated map/table/choice placement error.
fn reject_tagged_plain_group_payload(
    types: &mut IntermediateTypes,
    ty: &RustType,
    tag: usize,
) -> bool {
    let Some(group_name) = resolved_plain_group_source_name(types, ty) else {
        return false;
    };
    types.record_rejection(format!(
        "a CBOR tag payload cannot be the plain group `{group_name}` — a tag wraps a TYPE that \
         denotes exactly one data item, while a plain group has no type of its own and only \
         splices its members into an enclosing array or map. Give the group array framing first \
         (`wrapped = [{group_name}]`), then tag that type (`#6.{tag}(wrapped)` or \
         `#6.{tag}([{group_name}])`)."
    ));
    true
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

/// The domain guards shared by the plain (`{ * k => v }`) and parenthesized (`{ * (k => v) }`)
/// table arms, in this order:
/// - a fixed VALUE (`{ * uint => 5 }`): a `Fixed` has no type to store per row in the map's `V`,
///   and would reach the same `for_rust_member` panic as `[* 5]`. (A fixed KEY is handled at each
///   arm: per RFC 8610 `1 => v` is the same wire entry as `1: v`, so it diverts to the record path.)
/// - a bare plain group as the KEY, then as the VALUE (`record_plain_group_table_domain_rejection`).
///
/// Rejections cite the rule by its SOURCE spelling when there is one; anonymous nested maps
/// describe the entry instead.
fn reject_table_domains(
    types: &mut IntermediateTypes,
    rule_name: Option<&RustIdent>,
    t1: &Type1,
    value: &Type,
    key_type: &RustType,
    value_type: &RustType,
) {
    let site = rejection_site(types, rule_name, "inline map");
    let entry_src = format!("{t1} => {value}");
    if let ConceptualRustType::Fixed(fixed) = value_type.conceptual_type.resolve_alias_shallow() {
        let fixed = fixed.clone();
        record_fixed_table_value_rejection(types, &site, &entry_src, &fixed);
    }
    if let Some(group_name) = resolved_plain_group_source_name(types, key_type) {
        record_plain_group_table_domain_rejection(
            types,
            &site,
            &entry_src,
            "KEY",
            &group_name,
            &format!(
                "{} => {value}",
                array_wrapped_domain_src(&t1.to_string(), &t1.type2)
            ),
        );
    }
    if let Some(group_name) = resolved_plain_group_source_name(types, value_type) {
        record_plain_group_table_domain_rejection(
            types,
            &site,
            &entry_src,
            "VALUE",
            &group_name,
            &format!(
                "{t1} => {}",
                match single_type2(value) {
                    Some(t2) => array_wrapped_domain_src(&value.to_string(), t2),
                    None => format!("[{value}]"),
                }
            ),
        );
    }
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
/// `--wasm` profiles for both slots, including a tag on the array-framed remedy. A tag stays
/// OUTSIDE the framing (`#6.10([kv])`, not `[#6.10(kv)]`): the array is the item the map slot holds,
/// and the tag wraps that item, so only that placement keeps the tag on what the spec tagged. A tag
/// directly over the group reaches the earlier tag-payload semantic refusal.
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
         semantics: `{remedy_entry}` in place of this row. (A tag on the slot belongs OUTSIDE \
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

// would use rust_type_from_type1 but that requires IntermediateTypes which we shouldn't

// Attempts to use the style-converted type name as a field name, and if we have already
// generated one, then we simply add numerals starting at 2, 3, 4...
// If you wish to only check if there is an explicitly stated field name,
// then use group_entry_to_raw_field_name()

// Only returns Some(String) if there was an explicit field name provided, otherwise None.
// If you need to try and make one using the type/etc, then try group_entry_to_field_name()
// Also does not do any CamelCase or snake_case formatting.

// Retain the existing crate-visible parsing facade while rust_reserved owns the fixed registry.
#[cfg(test)]
#[allow(unused_imports)]
pub(crate) use crate::rust_reserved::GENERATED_LOCAL_PROBED_SAFE;
use crate::rust_reserved::generated_local_field_rejection;
#[allow(unused_imports)]
pub(crate) use crate::rust_reserved::{GENERATED_LOCAL_RESERVED, RUST_KEYWORDS, ReservedScope};
