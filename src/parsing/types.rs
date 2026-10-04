use super::{
    ControlOperator, EXTERN_MARKER, GroupParsingType, NonLastArmOwner, RAW_BYTES_MARKER,
    RuleBodyShape, SUPPORTED_GENERIC_DEF_BODIES, apply_inline_table_row_metadata,
    apply_rule_position_directives, choice_variant_context, float_range_to_primitive,
    get_comment_after, group_entry_rule_metadata, handle_extern_companions, ident_to_primitive,
    integer_primitive_domain, is_uint_primitive, lower_anonymous_record,
    materialize_plain_group_ref, non_bytes_cbor_head_rejection, parse_control_operator,
    parse_group, parse_group_type, range_to_primitive,
    raw_bytes_flavor_non_generic_extern_rejection, recognize_optional_tag_set,
    register_fixed_singleton, register_float_range, register_ranged_type,
    reject_duplicates_not_applicable, reject_ignore_not_applicable, reject_non_last_arm_directives,
    reject_plain_group_type_choice_arm, reject_rule_prefix, reject_tagged_plain_group_payload,
    reject_type_choice_arm_variant_name_collision, rejection_site, resolved_head_primitive,
    rule_position_metadata, rule_position_name_message, source_rule_name_of,
    strip_alias_for_registration, synthesized_fixed_singleton_ident, tag_number,
    type_choice_metadata, unmappable_default_head_rejection, unmapped_control_head_rejection,
    well_known_tag_default_duplicates, with_optional_bounds, with_well_known_tag_default,
};
use crate::cli::Cli;
use crate::comment_ast::{RuleMetadata, merge_metadata};
use crate::intermediate::{
    AliasIdent, AliasInfo, CDDLIdent, ConceptualRustType, EnumVariant, EnumVariantData, FixedValue,
    GenericDef, GenericInstance, GenericParamBinding, IntBounds, IntWindow, IntermediateTypes,
    Primitive, Representation, RustIdent, RustStruct, RustType, VariantIdent,
};
use crate::utils::convert_to_camel_case;
use cddl::ast::parent::ParentVisitor;
use cddl::ast::{
    CDDLType, GenericArgs, Group, GroupEntry, MemberKey, Operator, RangeCtlOp, Type, Type1, Type2,
    TypeChoice,
};
use cddl::token;

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

#[allow(clippy::too_many_arguments)]
pub(super) fn parse_type_choices(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    type_choices: &[TypeChoice],
    tag: Option<usize>,
    generic_params: Option<Vec<GenericParamBinding>>,
    // Metadata from an enclosing tag head or parentheses of this same rule.
    inherited_metadata: &RuleMetadata,
    cli: &Cli,
) {
    let optional_inner_type = null_collapse_inner(type_choices);
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
                // Register after common directive validation. Returning here would leave the
                // singleton's rule slot unread; null / null owns one concrete state, not an alias.
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
        // Refuse plain-group inners before registration: an optional alias would otherwise name
        // a Rust type the group never defines. The recorded rejection precedes finalization.
        let collapse_site = rejection_site(types, Some(name), "anonymous");
        if reject_plain_group_type_choice_arm(types, &inner_rust_type, &collapse_site) {
            return;
        }
        let final_type =
            RustType::new(ConceptualRustType::Optional(Box::new(inner_rust_type))).tag_if(tag);
        // Read the last arm's rule slot and merge the enclosing wrapper's slot once.
        let local_metadata = rule_position_metadata(type_choices);
        let rule_metadata = merge_metadata(inherited_metadata, &local_metadata);
        apply_rule_position_directives(types, name, &rule_metadata, RuleBodyShape::NullCollapse);
        // Nullable aliases mint no class for key or element wrappers. Inside tagged demands
        // retain their existing refusal, while the accepted outside spelling stays available
        // pending the maintainer decision. A null / null singleton retains both directives.
        if null_singleton.is_none()
            && rule_metadata.key_demand.is_some()
            && !(tag.is_some()
                && inherited_metadata.key_demand.is_some()
                && local_metadata.key_demand.is_none())
        {
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
        if null_singleton.is_none()
            && rule_metadata.used_as_elem
            && !(tag.is_some() && inherited_metadata.used_as_elem && !local_metadata.used_as_elem)
        {
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
        // An untagged nullable collapse stays a transparent alias, so it cannot honor newtype.
        // Tagged collapses already wrap, making the directive redundant and honored there.
        if null_singleton.is_none() && tag.is_none() && rule_metadata.newtype.is_some() {
            types.record_rejection(format!(
                "@newtype on `{name}`: a `T / null` rule collapses to a transparent `Option<T>` \
                 alias, and no wrapper struct is generated for it, so the directive would silently \
                 do nothing. Wrap the inner type instead (`{name}_inner = <T> ; @newtype` and \
                 `{name} = {name}_inner / null`), or drop the directive."
            ));
        }
        // A nullable collapse has no variants; classify its preceding arms by that owner.
        reject_non_last_arm_directives(
            types,
            type_choices,
            NonLastArmOwner::NullCollapseRule(name),
        );
        if let Some(fixed_null) = null_singleton {
            // `null / null` has one CBOR and Rust state.  It is a named singleton, not a nullable
            // alias, so rule-scoped class directives remain meaningful just as on `x = null`.

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
        // Tagged nullable bodies wrap so the rule's own codec carries the same tag as embed sites.
        // A transparent alias would use Option's untagged codec and violate those wire facts.
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
        let local_metadata = rule_position_metadata(type_choices);
        let rule_metadata = merge_metadata(inherited_metadata, &local_metadata);
        apply_rule_position_directives(types, name, &rule_metadata, RuleBodyShape::TypeChoice);
        // Enum variants consume names and docs; other non-last-arm directives belong on the rule.
        reject_non_last_arm_directives(types, type_choices, NonLastArmOwner::TypeChoiceRule(name));
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
            // Array-backed 258 sets nominalize into wrappers owning tag, length and element
            // encodings. The policy selects the inner collection; the optional tag belongs to
            // OptionallyTagged. A named getter is honored, while bare newtype adds none.
            // Generic definitions register a GenericDef wrapper and each distinct argument list
            // resolves to one nominal instance. Other collapsed collections wrap without this
            // set nominalization. No collapse has enum variants, so arm names and docs are refused.
            if rule_metadata.name.is_some() {
                types.record_rejection(rule_position_name_message(&source_rule_name_of(
                    types, name,
                )));
            }
            if rule_metadata.ignore {
                reject_ignore_not_applicable(types, name);
            }
            // A collapsed tag-set has no enum variants to own first-arm names or docs.
            reject_non_last_arm_directives(types, type_choices, NonLastArmOwner::TagSetRule(name));
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
            let bounds = base.config.occurrence_bounds();
            // Every flavor WRAPS: the tag is a wire-affecting property, and a transparent
            // `pub type Foo = Vec<u64>;` (or `BTreeMap<..>`) carrying `OptionallyTagged(n)` mints no
            // type to hang the tag on, so `Foo::from_cbor_bytes` would REFUSE the tagged half of the
            // very wire the idiom exists to admit while every embed site accepts both.
            // - A 258 array SET additionally nominalizes (`as_set_nominal` below).
            // - A NON-258 array does not nominalize (no set semantics — that is the 258 registry
            //   entry's alone); its inner stays a plain `Vec`, since the `OrderedSet` twin belongs
            //   to the set-nominal flavor, not to wrapping.
            // - A MAP never nominalizes. Every `@duplicates` policy wraps, `preserve` included: the
            //   register-side `Wrapper` arm threads the policy onto the stored inner map, so the
            //   wrapper's member is the `PairMap`/`NonEmptyPairMap` twin and its wasm boundary names
            //   the `PairMapKToV` class the wasm struct walk mints for exactly this inner.
            let collection_type: RustType = match base.conceptual_type {
                collection @ (ConceptualRustType::Array(_) | ConceptualRustType::Map(..)) => {
                    collection.into()
                }
                // `recognize_optional_tag_set` only ever returns an Array/Map base
                _ => unreachable!(),
            };
            let wrapper = RustStruct::new_wrapper(
                name.clone(),
                Some(set_tag),
                Some(&effective_metadata),
                with_optional_bounds(collection_type, bounds),
                None,
            )
            .as_optionally_tagged();
            // `is_set_nominal` implies an Array base.
            let rust_struct = if is_set_nominal {
                wrapper.as_set_nominal()
            } else {
                wrapper
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
        // Refuse other choice-bodied generic definitions: enum monomorphization cannot resolve
        // their parameters. This site can name the supported collapsed collection spelling.
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

#[allow(clippy::too_many_arguments)]
fn lower_numeric_literal_rule(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    type1: &Type1,
    outer_tag: Option<usize>,
    generic_params: Option<Vec<GenericParamBinding>>,
    rule_metadata: RuleMetadata,
    cli: &Cli,
) {
    // The literal a bare rule body denotes, and the primitive an integer window between
    // two such literals collapses onto (`foo = 0..255`, `foo = -10..-3`).
    let (literal, int_primitive) = match &type1.type2 {
        Type2::IntValue { value, .. } => (FixedValue::Nint(*value as i128), Primitive::I64),
        Type2::UintValue { value, .. } => (FixedValue::Uint(*value as u64), Primitive::U64),
        Type2::FloatValue { value, .. } => (FixedValue::Float(*value), Primitive::Float),
        _ => unreachable!("guarded by the enclosing arm"),
    };
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
        // A float head only ever gets the inert integer placeholder a refusal leaves
        // (`try_float_or_reject` builds every float window), so the alias is never emitted.
        Some(ControlOperator::Range(min_max)) if int_primitive == Primitive::Float => {
            let base_type = range_to_primitive(min_max.0, min_max.1, Primitive::Float);
            types.register_type_alias(
                type_name.clone(),
                AliasInfo::new_from_metadata(base_type.tag_if(outer_tag), rule_metadata),
            );
        }
        // A literal-headed top-level range rule must WRAP when a residual bound (or
        // @newtype) survives the primitive collapse, or when it carries a tag, so its
        // standalone to/from_cbor_bytes enforces the window / writes the tag: the same
        // registration as the `Type2::Typename` range arm.
        Some(ControlOperator::Range(min_max)) => register_ranged_type(
            types,
            parent_visitor,
            type_name,
            range_to_primitive(min_max.0, min_max.1, int_primitive),
            min_max,
            outer_tag,
            rule_metadata,
            cli,
        ),
        // Top-level literal float range (`foo = 0.5..10.5`, `#6.5(0.5..10.5)`): WRAP into a
        // bounds-enforcing float newtype so its standalone to/from_cbor_bytes enforces the
        // window (and writes the tag).
        Some(ControlOperator::RangeFloat(window)) if int_primitive == Primitive::Float => {
            register_float_range(
                types,
                parent_visitor,
                type_name,
                float_range_to_primitive(window, Primitive::Float),
                window,
                outer_tag,
                rule_metadata,
                cli,
            )
        }
        Some(ControlOperator::RangeFloat(_)) => unreachable!(
            "a float window over an integer-literal head is a mixed-kind range, refused in try_float_or_reject"
        ),
        _ => {
            register_fixed_singleton(
                types,
                parent_visitor,
                type_name.clone(),
                RustType::from(ConceptualRustType::Fixed(literal)),
                outer_tag,
                Some(&rule_metadata),
                generic_params.as_deref(),
                cli,
                false,
            );
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_tagged_rule(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    tag: &Option<token::TagConstraint<'_>>,
    t: &Type,
    outer_tag: Option<usize>,
    generic_params: Option<Vec<GenericParamBinding>>,
    rule_metadata: &RuleMetadata,
    cli: &Cli,
) {
    if outer_tag.is_some() {
        types.record_rejection(format!(
            "{}a tag directly inside a rule body's tag (`#6.1(#6.2(…))`) is unsupported — the \
             rule's wrapper owns exactly one tag head. Name the inner tagged value as its own rule \
             and tag that (`inner = #6.2(uint)`, then `outer = #6.1(inner)`).",
            reject_rule_prefix(Some(type_name))
        ));
        return;
    }
    let tag_unwrap = match tag_number(tag, Some(type_name)) {
        Ok(n) => n,
        Err(msg) => {
            types.record_rejection(msg);
            return;
        }
    };
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
                rule_metadata,
                None,
                cli,
            );
        }
        _ => {
            parse_type_choices(
                types,
                parent_visitor,
                type_name,
                &t.type_choices,
                Some(tag_unwrap),
                generic_params,
                rule_metadata,
                cli,
            );
        }
    };
}

#[allow(clippy::too_many_arguments)]
fn lower_parenthesized_rule(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    type1: &Type1,
    pt: &Type,
    outer_tag: Option<usize>,
    generic_params: Option<Vec<GenericParamBinding>>,
    rule_metadata: &RuleMetadata,
    size_override: Option<&Operator>,
    cli: &Cli,
) {
    // Carry only an authored outer SIZE through bare single-choice parentheses.
    // Other outer controls retain their existing route in this narrow repair.
    let outer_size = size_override.or_else(|| {
        type1.operator.as_ref().filter(|op| {
            matches!(
                op.operator,
                RangeCtlOp::CtlOp {
                    ctrl: token::ControlOperator::SIZE,
                    ..
                }
            )
        })
    });
    match pt.type_choices.as_slice() {
        [only] => parse_type(
            types,
            parent_visitor,
            type_name,
            only,
            outer_tag,
            generic_params,
            rule_metadata,
            outer_size,
            cli,
        ),
        _ if outer_size.is_some() => {
            // The API SIZE choice pre-scan normally refuses this before lowering.
            types.record_rejection(format!(
                "rule `{type_name}`: an outer `.size` requires one parenthesized head — \
                 write the size separately on each supported choice arm"
            ));
        }
        _ => parse_type_choices(
            types,
            parent_visitor,
            type_name,
            &pt.type_choices,
            outer_tag,
            generic_params,
            rule_metadata,
            cli,
        ),
    }
}

fn lower_extern_marker_rule(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    generic_params: Option<Vec<GenericParamBinding>>,
    rule_metadata: RuleMetadata,
    cli: &Cli,
) {
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
            types.record_rejection(raw_bytes_flavor_non_generic_extern_rejection(type_name));
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
}

fn lower_raw_bytes_marker_rule(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    generic_params: Option<Vec<GenericParamBinding>>,
    rule_metadata: RuleMetadata,
    cli: &Cli,
) {
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
}

#[allow(clippy::too_many_arguments)]
fn lower_controlled_typename_rule(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    type1: &Type1,
    cddl_ident: CDDLIdent,
    operator: Option<&Operator>,
    control: ControlOperator,
    outer_tag: Option<usize>,
    generic_params: Option<Vec<GenericParamBinding>>,
    rule_metadata: RuleMetadata,
    cli: &Cli,
) {
    if generic_params.is_some() {
        types.record_rejection(format!(
            "generic rule `{type_name}`: a control operator or range (`.size`/`.le`/`.cbor`/`.default`/…) \
             as the whole body of a generic definition is not supported — such a body registers no \
             struct for the generic arguments to substitute into. Name the constrained type as its own \
             non-generic rule and reference it from a supported generic body. \
             {SUPPORTED_GENERIC_DEF_BODIES}"
        ));
        return;
    }
    match control {
        ControlOperator::Range(min_max) => {
            // when declared top-level we make a new type as the default behavior like before
            // Only SIZE gains a transparent unsigned-alias route. Other
            // controls retain their existing named-head refusal.
            let alias_primitive = operator.and_then(|op| {
                if !matches!(
                    op.operator,
                    RangeCtlOp::CtlOp {
                        ctrl: token::ControlOperator::SIZE,
                        ..
                    }
                ) {
                    return None;
                }
                let alias = types
                    .type_aliases()
                    .get(&AliasIdent::new(cddl_ident.clone()))?;
                // Custom wire codecs are not transparent primitive contracts.
                if alias.carries_custom_pair() || !alias.base_type.encodings.is_empty() {
                    return None;
                }
                resolved_head_primitive(types, &type1.type2)
                    .filter(|primitive| is_uint_primitive(*primitive))
            });
            let Some(primitive) = ident_to_primitive(&cddl_ident).or(alias_primitive) else {
                types.record_rejection(unmapped_control_head_rejection(type_name, &cddl_ident));
                return;
            };
            // A wider size cannot widen an already sized alias.
            let min_max = match (alias_primitive, integer_primitive_domain(primitive)) {
                (Some(_), Some((min, max))) => (
                    Some(min_max.0.unwrap_or(min).max(min)),
                    Some(min_max.1.unwrap_or(max).min(max)),
                ),
                _ => length_window(primitive, min_max),
            };
            let ranged_type = range_to_primitive(min_max.0, min_max.1, primitive);
            // An exact byte `.size` becomes a Rust array length.  Validate at
            // the parse boundary so generation never truncates an authored
            // CDDL integer or emits a target-dependent array const.
            if let Some(Err(length)) = ranged_type.exact_byte_array_len() {
                types.record_rejection(exact_byte_array_length_rejection(length));
                return;
            }
            register_ranged_type(
                types,
                parent_visitor,
                type_name,
                ranged_type,
                min_max,
                outer_tag,
                rule_metadata,
                cli,
            );
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
                types.record_rejection(unmapped_control_head_rejection(type_name, &cddl_ident));
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
            let inner_type = rust_type_from_type2(types, parent_visitor, &type1.type2, cli);
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
                let aliased = match inner_type.try_default(default_value.clone()) {
                    Ok(defaulted) => defaulted,
                    Err(undefaulted) => {
                        types.record_rejection(unmappable_default_head_rejection(
                            Some(type_name),
                            &type1.type2,
                            &default_value,
                        ));
                        undefaulted
                    }
                };
                types.register_type_alias(
                    type_name.clone(),
                    AliasInfo::new_from_metadata(aliased.tag_if(outer_tag), rule_metadata),
                );
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_ordinary_typename_rule(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    cddl_ident: CDDLIdent,
    generic_args: &Option<GenericArgs>,
    outer_tag: Option<usize>,
    generic_params: Option<Vec<GenericParamBinding>>,
    rule_metadata: RuleMetadata,
    cli: &Cli,
) {
    let mut concrete_type = types.new_type(&cddl_ident, cli);
    // A tag payload is a TYPE and therefore denotes exactly one data item. A
    // plain group has no type of its own: its only meaning is to splice a
    // sequence of members into an enclosing array or map. The cddl parser's
    // intentionally-ambiguous identifier node lets `#6.n(pg)` reach this arm
    // even when `pg` resolves to a group, so enforce the semantic boundary
    // after resolution. Keep an inert primitive in the registration path after
    // recording the rejection; sibling rules may still reference this rule
    // before `finalize` drains the error, and a missing registration would turn
    // the graceful refusal back into an order-dependent panic.
    if let Some(tag) = outer_tag
        && reject_tagged_plain_group_payload(types, &concrete_type, tag)
    {
        concrete_type = ConceptualRustType::Primitive(Primitive::U64).into();
    }
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
                        .map(|a| rust_type_from_type1(types, parent_visitor, &a.arg, cli))
                        .collect();
                    // The instantiation nominal a set binding aliases TO
                    // (`named_set = set<key_hash>` → `SetKeyHash`); identical to
                    // the anonymous-use spelling so both dedup to one nominal.
                    let canonical_ident = RustIdent::new(generic_instance_canonical_cddl_ident(
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
                    ));
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
                            concrete_type.conceptual_type.resolve_alias_shallow(),
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
                        if let ConceptualRustType::Fixed(fixed) =
                            concrete_type.conceptual_type.resolve_alias_shallow()
                        {
                            let fixed = fixed.clone();
                            types.record_bare_fixed_rule_rejection(type_name, &fixed);
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
                            strip_alias_for_registration(types, concrete_type, &mut alias_metadata);
                        types.register_type_alias(
                            type_name.clone(),
                            AliasInfo::new_from_metadata(concrete_type, alias_metadata)
                                .with_stripped_alias_target(stripped_alias_target)
                                .with_inherited_wire_metadata(inherited_from),
                        );
                    }
                }
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_text_literal_rule(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    type1: &Type1,
    value: &str,
    outer_tag: Option<usize>,
    generic_params: Option<Vec<GenericParamBinding>>,
    rule_metadata: RuleMetadata,
    cli: &Cli,
) {
    // A text literal takes no operator; `parse_control_operator` records the refusal (a
    // control) or the non-numeric range bound (a range), and the singleton still
    // registers so a sibling reference resolves until `finalize` drains the error.
    if let Some(op) = &type1.operator {
        parse_control_operator(
            types,
            parent_visitor,
            &type1.type2,
            op,
            Some(type_name),
            cli,
        );
    }
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

#[allow(clippy::too_many_arguments)]
fn lower_bytes_literal_rule(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    type1: &Type1,
    value: &[u8],
    outer_tag: Option<usize>,
    generic_params: Option<Vec<GenericParamBinding>>,
    rule_metadata: RuleMetadata,
    cli: &Cli,
) {
    // Same as the text literal arm above.
    if let Some(op) = &type1.operator {
        parse_control_operator(
            types,
            parent_visitor,
            &type1.type2,
            op,
            Some(type_name),
            cli,
        );
    }
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

#[allow(clippy::too_many_arguments)]
pub(super) fn parse_type(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    type_choice: &TypeChoice,
    outer_tag: Option<usize>,
    generic_params: Option<Vec<GenericParamBinding>>,
    // Metadata carried in from an enclosing single-type wrapper of the SAME rule (a `#6.n(...)` tag
    // head or a parenthesized type). The cddl AST attaches the rule's trailing comment DSL (e.g.
    // `@newtype`) to the OUTER type1, so recursing into the inner type without threading this would
    // silently drop it — making `tagged = #6.42(text) ; @newtype` a no-op. Empty at the top level.
    inherited_metadata: &RuleMetadata,
    // Borrowed from the original outer AST; never a temporary cloned TypeChoice. SIZE only.
    size_override: Option<&Operator>,
    cli: &Cli,
) {
    let type1 = &type_choice.type1;
    let operator = size_override.or(type1.operator.as_ref());
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
    // The inner call checks the merged slot against its own body shape.
    let defers_to_inner = match &type1.type2 {
        Type2::TaggedData { tag, .. } => {
            size_override.is_none()
                && outer_tag.is_none()
                && tag_number(tag, Some(type_name)).is_ok()
        }
        Type2::ParenthesizedType { .. } => size_override.is_none() || type1.operator.is_none(),
        _ => false,
    };
    if !defers_to_inner {
        apply_rule_position_directives(
            types,
            type_name,
            &rule_metadata,
            RuleBodyShape::single_type(type1),
        );
    }
    // Marker rules name an externally defined type; they own no tag-bearing wrapper here.
    if outer_tag.is_some()
        && let Type2::Typename { ident, .. } = &type1.type2
        && matches!(ident.ident, EXTERN_MARKER | RAW_BYTES_MARKER)
    {
        types.record_rejection(format!(
            "rule `{type_name}`: a tag around `{}` would be ignored because the marker names an \
             externally defined type. Define the tagged wrapper as a separate type with a real \
             CDDL body, or drop the tag.",
            ident.ident
        ));
        return;
    }
    if let Some(outer_size) = size_override {
        // Keep outer operand diagnostics before the narrow head refusal. An inner control
        // is never overwritten or implicitly intersected with this one.
        if type1.operator.is_some() {
            parse_control_operator(
                types,
                parent_visitor,
                &type1.type2,
                outer_size,
                Some(type_name),
                cli,
            );
            types.record_rejection(format!(
                "rule `{type_name}`: an outer `.size` around an already-controlled parenthesized \
                 head is unsupported — put a single size control on the head, or name the \
                 constrained type before applying another control"
            ));
            return;
        }
        let supported_head = matches!(&type1.type2, Type2::ParenthesizedType { .. })
            || matches!(&type1.type2, Type2::Typename { ident, .. }
                if !matches!(ident.ident, EXTERN_MARKER | RAW_BYTES_MARKER));
        if !supported_head {
            parse_control_operator(
                types,
                parent_visitor,
                &type1.type2,
                outer_size,
                Some(type_name),
                cli,
            );
            types.record_rejection(format!(
                "rule `{type_name}`: an outer `.size` on parenthesized head `{}` is unsupported — \
                 apply it to uint, bytes, text, or a supported transparent unsigned alias",
                type1.type2,
            ));
            return;
        }
    }
    match &type1.type2 {
        Type2::Typename {
            ident,
            generic_args,
            ..
        } => {
            if ident.ident == EXTERN_MARKER {
                lower_extern_marker_rule(
                    types,
                    parent_visitor,
                    type_name,
                    generic_params,
                    rule_metadata,
                    cli,
                );
            } else if ident.ident == RAW_BYTES_MARKER {
                lower_raw_bytes_marker_rule(
                    types,
                    parent_visitor,
                    type_name,
                    generic_params,
                    rule_metadata,
                    cli,
                );
            } else {
                // Note: this handles bool constants too, since we apply the type aliases and they resolve
                // and there's no Type2::BooleanValue
                let cddl_ident = CDDLIdent::new(ident.to_string());
                let control = operator.map(|op| {
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
                        lower_controlled_typename_rule(
                            types,
                            parent_visitor,
                            type_name,
                            type1,
                            cddl_ident,
                            operator,
                            control,
                            outer_tag,
                            generic_params,
                            rule_metadata,
                            cli,
                        );
                    }
                    None => {
                        lower_ordinary_typename_rule(
                            types,
                            parent_visitor,
                            type_name,
                            cddl_ident,
                            generic_args,
                            outer_tag,
                            generic_params,
                            rule_metadata,
                            cli,
                        );
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
            lower_tagged_rule(
                types,
                parent_visitor,
                type_name,
                tag,
                t,
                outer_tag,
                generic_params,
                &rule_metadata,
                cli,
            );
        }
        // Note: bool constants are handled via Type2::Typename
        Type2::IntValue { .. } | Type2::UintValue { .. } | Type2::FloatValue { .. } => {
            lower_numeric_literal_rule(
                types,
                parent_visitor,
                type_name,
                type1,
                outer_tag,
                generic_params,
                rule_metadata,
                cli,
            );
        }
        Type2::TextValue { value, .. } => {
            lower_text_literal_rule(
                types,
                parent_visitor,
                type_name,
                type1,
                value.as_ref(),
                outer_tag,
                generic_params,
                rule_metadata,
                cli,
            );
        }
        Type2::B16ByteString { value, .. }
        | Type2::B64ByteString { value, .. }
        | Type2::UTF8ByteString { value, .. } => {
            lower_bytes_literal_rule(
                types,
                parent_visitor,
                type_name,
                type1,
                value.as_ref(),
                outer_tag,
                generic_params,
                rule_metadata,
                cli,
            );
        }
        Type2::ParenthesizedType { pt, .. } => {
            lower_parenthesized_rule(
                types,
                parent_visitor,
                type_name,
                type1,
                pt,
                outer_tag,
                generic_params,
                &rule_metadata,
                size_override,
                cli,
            );
        }
        x => reject_unsupported_rule_body(types, type_name, x),
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
    // `source_owner_desc` keeps the rule name as the AUTHOR spelled it for the remaining
    // plain-group arm REJECTION; the warnings above print the Rust ident instead.
    let source_owner_desc = match owner {
        Some(name) => format!("rule `{}`", source_rule_name_of(types, name)),
        None => "an inline type choice".to_owned(),
    };
    let choice_context = choice_variant_context(owner, type_choices);
    // Reserve every explicit emitted name BEFORE walking arms. An explicit `@name` is public API,
    // so it must keep that spelling even if a colliding generator-derived arm appears first; the
    // derived arm is the one that takes `2`. This is the type-choice equivalent of the group-choice
    // pre-reservation in `settle_arm_variant_name`.
    //
    // The same pre-pass sees two explicit names whose source spellings camel-case to one Rust
    // variant (`my_arm` / `myArm`) in either a named or inline choice. The latter has no source rule
    // or enum name to cite, but it still has arms and a generated variant, so it receives the
    // role-generic diagnostic rather than keeping an invalid repeated variant.
    for (arm_idx, choice) in type_choices.iter().enumerate() {
        let metadata = type_choice_metadata(choice);
        if let Some(source_name) = metadata.name {
            let emitted_name = convert_to_camel_case(&source_name);
            if let Some(first) = types.reserve_explicit_variant_mint(
                &choice_context,
                arm_idx + 1,
                source_name.clone(),
                emitted_name.clone(),
            ) {
                reject_type_choice_arm_variant_name_collision(
                    types,
                    owner,
                    first.arm_ordinal,
                    &first.source_name,
                    arm_idx + 1,
                    &source_name,
                    &emitted_name,
                );
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
        let rule_metadata = type_choice_metadata(choice);
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
            _ => rust_type.conceptual_type.for_variant().to_string(),
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
            // `Type2`'s Display is not total for every parser-owned literal spelling (notably
            // byte syntax), while this provenance is diagnostic-only. The already-minted base is
            // total and identifies the derived arm without re-entering that foreign formatter.
            let derived_source = base_name.clone();
            types.settle_derived_variant_mint(
                &choice_context,
                arm_idx + 1,
                derived_source,
                base_name,
            )
        };
        let variant = EnumVariant::new(
            VariantIdent::new_custom(variant_name),
            rust_type.clone(),
            false,
            rule_metadata.doc.clone(),
        );
        variants.push(if rule_metadata.name.is_none() {
            variant.with_derived_name()
        } else {
            variant
        });
        if !rejected {
            kept.push((rust_type, arm_idx + 1));
        }
    }
    variants
}

/// The non-null arm of a two-arm `T / null` (or `null / T`) type choice, which collapses to
/// `Option<T>`; `None` for any other choice.
pub(super) fn null_collapse_inner<'a, 'b>(
    type_choices: &'b [TypeChoice<'a>],
) -> Option<&'b Type1<'a>> {
    let [a, b] = type_choices else {
        return None;
    };
    if type2_is_null(&a.type1.type2) {
        Some(&b.type1)
    } else if type2_is_null(&b.type1.type2) {
        Some(&a.type1)
    } else {
        None
    }
}

// would use rust_type_from_type1 but that requires IntermediateTypes which we shouldn't
fn type2_is_null(t2: &Type2) -> bool {
    match t2 {
        Type2::Typename { ident, .. } => ident.ident == "null" || ident.ident == "nil",
        _ => false,
    }
}

pub(super) fn type_to_field_name(t: &Type) -> Option<String> {
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
            // special case for T / null -> maps to Option<T> so field name should be same as just T;
            // any other two-arm type choice is unsupported here
            null_collapse_inner(&t.type_choices).and_then(|inner| type2_to_field_name(&inner.type2))
        }
        // no type choice support here
        _ => None,
    }
}

/// The `@name` that names a MEMBER-position anonymous heterogeneous inline composite (array or
/// map), read from the one comment slot
/// that spelling puts it in: the enclosing group entry's trailing comments (plus the trailing-comma
/// slot), exactly the pair `group_entry_rule_metadata` reads for the field rename.
///
/// `get_comment_after(type2)` deliberately cannot reach that slot — its documented rule is that a
/// type does not inherit the comment of a parent it merely happens to end, and a general
/// `Type -> GroupEntry` ascent would leak every field-level directive into every type2 read. So the
/// naming site asks for the slot by itself, under the narrowest scope that keeps the name
/// unambiguous: the anonymous composite must be the member's WHOLE type, UP TO tag wrappers. The
/// ascent is therefore required to be
/// `Type2 -> (Type1 -> TypeChoice -> Type -> Type2::TaggedData)* -> Type1 -> TypeChoice -> Type ->
/// ValueMemberKeyEntry -> GroupEntry`, with EVERY rung over an operator-free `Type1` and a
/// single-choice `Type`. Every other spelling — a `.cbor` payload (whose `Type2`'s parent is the
/// `Operator`), a choice arm, a parenthesized type, or a composite nested inside another anonymous
/// composite — keeps the anonymous-group rejection rather than guessing which construct the name
/// was meant for.
///
/// Tag layers are walked (any number of them, `#6.42([x: uint])`, `#6.42({x: uint})`, and nested
/// forms alike) because a tag mints NO type of its own: it wraps whatever its payload parses to, so
/// the anonymous composite remains the sole nameable referent and the name can only mean the
/// struct. The name mints the struct and the tag wraps it, which is byte-for-byte the named-rule
/// remedy (`inner = [x: uint]` / `f: #6.42(inner)`, and likewise for a map) with a different
/// identifier. Without this the
/// rejection would advertise an `@name` door that the tagged spelling cannot open. The per-rung
/// operator-free and single-choice requirements are what keep it unambiguous: an operator makes the
/// composite the operator's target rather than the tag's whole payload, and a multi-choice `Type` at
/// any rung reintroduces the arm-vs-member ambiguity the untagged spelling already refuses.
///
/// Only `.name` is consumed. The same comment is ALSO the field-rename slot, so one `@name` here
/// names both the field and the struct that field holds; every other directive on it keeps the
/// field-level meaning it already had.
pub(super) fn anon_composite_member_name<'a>(
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

/// `.size` in member position on a NAMED head (`f = float64`, `[a: f .size 3]`; also through bare
/// parentheses, `[a: (f) .size 3]`) whose type is neither a byte or text string nor a uint.
/// A byte/text head takes the length reading and a uint head takes the `uint` reading instead
/// (`u = uint`, `[a: u .size 2]`). On any other type the value window aborted at generation
/// (float), did not compile (`bool`), or was dropped (records, arrays, choices). Prelude
/// heads are the pre-scan's and the `.size` arm's; a generic parameter keeps its own refusal at
/// substitution; an operand already refused left the inert `(None, None)` window.
fn member_size_named_head_rejection(
    types: &IntermediateTypes,
    type1: &Type1,
    base_type: &RustType,
    window: IntWindow,
) -> Option<String> {
    if window == (None, None) {
        return None;
    }
    let op = type1.operator.as_ref()?;
    if !matches!(
        op.operator,
        RangeCtlOp::CtlOp {
            ctrl: token::ControlOperator::SIZE,
            ..
        }
    ) {
        return None;
    }
    let mut head = &type1.type2;
    while let Type2::ParenthesizedType { pt, .. } = head
        && let [only] = pt.type_choices.as_slice()
        && only.type1.operator.is_none()
    {
        head = &only.type1.type2;
    }
    let Type2::Typename { ident, .. } = head else {
        return None;
    };
    if ident_to_primitive(&CDDLIdent::new(ident.to_string())).is_some()
        || base_type.generic_param_binding.is_some()
        || resolved_head_primitive(types, head).is_some_and(is_uint_primitive)
        || matches!(
            base_type.conceptual_type.resolve_alias_shallow(),
            ConceptualRustType::Primitive(Primitive::Bytes | Primitive::Str)
        )
    {
        return None;
    }
    Some(format!(
        "`.size` on `{ident}` is unsupported — `{ident}` is neither `uint` nor a byte or text \
         string (nor an alias of one), the only types RFC 8610 §3.8.1 gives a size to. Apply \
         `.size` to one of those types (`uint .size 2`, `bytes .size 4`, or `t = tstr` and then \
         `t .size 3`), or remove the control."
    ))
}

pub(super) fn rust_type_from_type1(
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
    if let Some(ControlOperator::Range(window)) = &control
        && let Some(msg) = member_size_named_head_rejection(types, type1, &base_type, *window)
    {
        types.record_rejection(msg);
        return base_type;
    }
    let result = match control {
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
                    None => with_resolved_head_window(base_type, (low, high)),
                }
            }
            // the base value will be a constant due to incomplete parsing earlier for explicit ranges
            // e.g. foo = 0..255
            Type2::IntValue { .. } => range_to_primitive(low, high, Primitive::I64),
            Type2::UintValue { .. } => range_to_primitive(low, high, Primitive::U64),
            _ => with_resolved_head_window(base_type, (low, high)),
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
            // A float-literal member range (`[f: 0.5..10.5]`) uses an f64 primitive.
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
    };
    if let Some(Err(length)) = result.exact_byte_array_len() {
        types.record_rejection(exact_byte_array_length_rejection(length));
    }
    if let Some(Err(length)) = result.exact_homogeneous_array_len() {
        types.record_rejection(exact_homogeneous_array_length_rejection(length));
    }
    result
}

/// Attach an integer window to a head that is not a prelude name (`u = uint`, `[a: u .size 2]`;
/// `[a: (uint) .le 5]`), reading it against the type the head RESOLVES to, as `range_to_primitive`
/// reads a prelude head's: a byte/text length goes through `length_window`, and on an integer
/// primitive a side its domain already implies is dropped (`u8_alias .size 2` checks nothing,
/// `u .size 9` spans every uint) instead of emitting a comparison the carrier cannot fail or a
/// literal it cannot hold. An exclusion and every other type keep the window as written.
fn with_resolved_head_window(base_type: RustType, window: IntWindow) -> RustType {
    let window = match base_type.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Primitive(primitive @ (Primitive::Bytes | Primitive::Str)) => {
            length_window(*primitive, window)
        }
        ConceptualRustType::Primitive(primitive)
            if let Some((min, max)) = integer_primitive_domain(*primitive)
                && matches!(IntBounds::read(window), IntBounds::Window(..)) =>
        {
            (
                window.0.filter(|low| *low > min),
                window.1.filter(|high| *high < max),
            )
        }
        _ => window,
    };
    match base_type.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _) => {
            base_type.with_occurrence_bounds(window)
        }
        _ => base_type.with_value_bounds(window),
    }
}

/// The window a byte/text `.size` checks. A CBOR length never exceeds `u64::MAX`, so an upper
/// bound there is no constraint (and `len > u64::MAX` is an absurd comparison on every target); a
/// zero minimum left without it checks nothing either. An exact window keeps both bounds, so exact
/// bytes still reach the `[u8; N]` path and its own length floor. Every other primitive's window
/// passes through unchanged.
pub(super) fn length_window(primitive: Primitive, (low, high): IntWindow) -> IntWindow {
    match (primitive, high) {
        (Primitive::Bytes | Primitive::Str, Some(h)) if h == u64::MAX as i128 && low != Some(h) => {
            (low.filter(|l| *l != 0), None)
        }
        _ => (low, high),
    }
}

/// The shared graceful refusal for an exact byte-string length that cannot become a Rust array on
/// every supported target (notably wasm32). Both rule and member parsing reach this helper.
fn exact_byte_array_length_rejection(length: i128) -> String {
    format!(
        "exact byte-string length `{length}` cannot be represented as a Rust array length on every supported target"
    )
}

/// The shared graceful refusal for an exact homogeneous-array occurrence that cannot become a
/// Rust static array on every supported target. Both direct/member and nested array routes pass
/// through `rust_type_from_type1` after occurrence bounds are attached.
pub(super) fn exact_homogeneous_array_length_rejection(length: i128) -> String {
    format!(
        "exact homogeneous array length `{length}` cannot be represented as a Rust array length on every supported target"
    )
}

/// The INSTANTIATION-derived canonical CDDL ident of a generic invocation:
/// `<def-name>_<args' canonical identity names>` (`set` + `[key_hash]` → `set_KeyHash`, camel-cased
/// to `SetKeyHash` by `RustIdent::new`). The argument fragments preserve that historic spelling for
/// ordinary unconstrained arguments, but include occurrence/config/codec differences recursively:
/// `set<([* uint])>` and `set<([*5 uint])>` cannot both register as `SetArrU64`.
///
/// The ONE owner of this spelling so every call site — anonymous use
/// (`generic_instance_or_new_type`) and named binding (`foo = bar<text>`) — derives the SAME
/// instantiation identity used by set-nominal deduplication.
pub(crate) fn generic_instance_canonical_cddl_ident(
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

/// Resolve a type/group name that may carry generic arguments into a `RustType`.
///
/// Without arguments, resolve through `types.new_type(&cddl_ident, cli)`.
/// With concrete arguments, register and resolve the identity from `generic_instance_canonical_cddl_ident`.
/// That identity retains occurrence, configuration and codec differences in each argument.
/// Resolving the bare generic base would discard the arguments and reference a type that is not emitted.
///
/// Shared by every member/element position that can carry a generic instantiation
/// (`rust_type_from_type2`'s `Type2::Typename` arm and `parse_group_type`'s single-entry
/// `TypeGroupname` array arm) so the two paths cannot drift.
pub(super) fn generic_instance_or_new_type(
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
            if types.generic_child_instance_has_inline_choice_argument(&generic_args) {
                types.record_rejection(
                    "a generic application whose argument is a definition-owned inline type choice is unsupported: the child instance and anonymous choice would need a shared cross-template concrete identity. Move the choice to a concrete use site or give it a concrete named rule.".to_owned(),
                );
                return ConceptualRustType::Fixed(FixedValue::Null).into();
            }
            // A child application such as `inner<p>` belongs to the surrounding generic
            // definition, not to the parser's global instance registry. Its concrete identity is
            // unknowable until an outer instance supplies the exact lexical binding for `p`.
            if types.generic_child_instance_is_deferred(&generic_args) {
                let placeholder = types.register_generic_child_instance_template(
                    args as *const GenericArgs as usize,
                    RustIdent::new(cddl_ident),
                    generic_args,
                );
                return RustType::new(ConceptualRustType::Rust(placeholder));
            }
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

pub(super) fn rust_type_from_type2(
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
                    let classification_mark = types.rejection_mark();
                    match parse_group_type(
                        types,
                        parent_visitor,
                        group_choice,
                        Representation::Array,
                        None,
                        false,
                        cli,
                    ) {
                        GroupParsingType::HomogenousArray(element_type, bounds) => {
                            materialize_plain_group_ref(
                                types,
                                parent_visitor,
                                &element_type,
                                Representation::Array,
                                cli,
                            );
                            with_optional_bounds(
                                ConceptualRustType::Array(Box::new(element_type)).into(),
                                bounds,
                            )
                        }
                        GroupParsingType::FlatGroupArray(_, _)
                        | GroupParsingType::HomogenousMap(_, _, _) => unreachable!(),
                        GroupParsingType::Heterogenous => lower_anonymous_record(
                            types,
                            parent_visitor,
                            type2,
                            group,
                            Representation::Array,
                            "anonymous-inline-array",
                            "Anonymous groups not allowed: an inline array is used where a type is \
                             required. Either create an explicit rule (`foo = [0, bytes]`, then \
                             reference `foo`) or give it a name using the `@name` notation.",
                            // The classification walk above visited these entries once already.
                            Some(classification_mark),
                            cli,
                        ),
                        GroupParsingType::WrappedBasicGroup(basic_type) => {
                            // A member-position anonymous array wrapping a plain-group reference
                            // (e.g. `bytes .cbor [coords]`, or a field `x = [coords]`) must promote
                            // the referenced plain group to an Array-rep Record struct, exactly like
                            // the `HomogenousArray` sibling above. Without this the group is never
                            // emitted and the returned type dangles on a bare, non-existent struct.
                            materialize_plain_group_ref(
                                types,
                                parent_visitor,
                                &basic_type,
                                Representation::Array,
                                cli,
                            );
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
                        false,
                        cli,
                    ) {
                        // Table map - homogenous key/value types
                        GroupParsingType::HomogenousMap(key_type, value_type, bounds) => {
                            // An inline `{+ k => v}` field carries the non-empty bound on its own
                            // RustType (mirroring the inline `[+ T]` array arm), so `for_rust_member`
                            // renders `NonEmptyMap<K, V>` and deserialize routes through its TryFrom.
                            let map_type = with_optional_bounds(
                                ConceptualRustType::Map(Box::new(key_type), Box::new(value_type))
                                    .into(),
                                bounds,
                            );
                            // The row entry's own comment slot (`{ * k => v ; @duplicates preserve }`)
                            // is read here, giving the inline spelling the same row-scoped directives
                            // the NAMED table has — and rejecting everything else it can carry, so a
                            // live slot is never also a silent-drop slot.
                            apply_inline_table_row_metadata(types, group_choice, map_type)
                        }
                        // A heterogeneous inline map is a record, so it needs a nominal name.
                        // The entry-slot `@name` is available only where this anonymous composite
                        // is the member's whole type up to operator-free tag wrappers; elsewhere it
                        // remains an honest rejection rather than leaking parent metadata inward.
                        GroupParsingType::Heterogenous => lower_anonymous_record(
                            types,
                            parent_visitor,
                            type2,
                            group,
                            Representation::Map,
                            "anonymous-inline-map",
                            "Anonymous groups not allowed: a heterogeneous inline map is used where \
                             a type is required. Give the map a nominal owner: create an explicit \
                             rule (`m = { a: int, b: uint }`, then reference `m`). A valid \
                             fixed-field record whose map is the member's whole type can instead \
                             use the scoped `@name` notation; dynamic/keyless bodies are validated \
                             separately and may still require another supported spelling.",
                            None,
                            cli,
                        ),
                        GroupParsingType::WrappedBasicGroup(_)
                        | GroupParsingType::FlatGroupArray(_, _)
                        | GroupParsingType::HomogenousArray(_, _) => unreachable!(),
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
            let tag_unwrap = match tag_number(tag, None) {
                Ok(n) => n,
                Err(msg) => {
                    types.record_rejection_once_at(type2, "tag-head", msg);
                    return ConceptualRustType::Fixed(FixedValue::Null).into();
                }
            };
            // Construct the tagged inline type here without applying a registry default or minting a nominal.
            // IntermediateTypes::nominalize_inline_sets runs in finalize over registered construction products.
            // It recognizes ConceptualRustType::Array with mandatory Tagged(258) in its encodings.
            // Deferring minting keeps transient arms discarded by named two-arm collapse out of the registry.
            // Named set rules retain their rule-derived nominal identity; inline sets derive identity from their shape.
            let inner = rust_type(types, parent_visitor, t, cli);
            if reject_tagged_plain_group_payload(types, &inner, tag_unwrap) {
                // An inert, storable placeholder keeps later parsing order-independent. The
                // recorded rejection prevents generation, so no bytes can be emitted from it.
                ConceptualRustType::Primitive(Primitive::U64).into()
            } else {
                inner.tag(tag_unwrap)
            }
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

pub(super) fn rust_type(
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
        // The last arm is the containing field's slot; classify only preceding arms here.
        let owner = if null_collapse_inner(&t.type_choices).is_some() {
            NonLastArmOwner::InlineNullCollapse
        } else {
            NonLastArmOwner::InlineTypeChoice
        };
        reject_non_last_arm_directives(types, &t.type_choices, owner);
        if t.type_choices.len() == 2 {
            // T / null   or   null / T   should map to Option<T>
            let collapse_inner = null_collapse_inner(&t.type_choices);
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
        // An inline choice directly owned by a generic definition cannot be registered while its
        // variants still name lexical parameters.  Retain the parsed variants in that definition's
        // parser-only sidecar and leave a private placeholder in the containing record/collection;
        // `GenericInstance::resolve` substitutes the exact bindings and finalization then gives the
        // concrete enum the normal anonymous-choice identity and collision semantics.
        let generic_inline_choice = inline_choice_scoped_parameter(types, t).is_some();
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
            combined_name.push_str(
                &variant
                    .rust_type()
                    .conceptual_type
                    .for_variant()
                    .to_string(),
            );
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
        if generic_inline_choice {
            let placeholder = types.register_generic_inline_type_choice_template(
                t.type_choices.as_ptr() as usize,
                candidate,
            );
            return RustType::new(ConceptualRustType::Rust(placeholder));
        }
        let combined_ident = types.anonymous_type_choice_ident(&base_ident, &candidate);
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

/// Return an exact-source generic parameter used anywhere beneath an inline choice.  Every AST
/// child is walked here rather than waiting for generation to encounter an unresolved `Rust(P)`:
/// collection and generic-argument arms need the same graceful refusal as a bare parameter arm.
/// The lookup itself remains exact-source so an outer `A` beside parameter `a` is never swept into
/// the refusal merely because their emitted Rust identifiers collide.
fn inline_choice_scoped_parameter(types: &IntermediateTypes, ty: &Type) -> Option<String> {
    fn exact_parameter(types: &IntermediateTypes, raw: &str) -> Option<String> {
        types
            .active_generic_param_binding(raw)
            .map(|_| raw.to_owned())
    }

    fn generic_args_parameter(
        types: &IntermediateTypes,
        args: Option<&GenericArgs>,
    ) -> Option<String> {
        args?
            .args
            .iter()
            .find_map(|arg| type1_parameter(types, &arg.arg))
    }

    fn member_key_parameter(types: &IntermediateTypes, key: &MemberKey) -> Option<String> {
        match key {
            MemberKey::Type1 { t1, .. } => type1_parameter(types, t1),
            // Bareword/value keys cannot bind a type parameter. `NonMemberKey` is parser-internal
            // recovery state whose children are intentionally private to the upstream AST crate.
            _ => None,
        }
    }

    fn group_entry_parameter(types: &IntermediateTypes, entry: &GroupEntry) -> Option<String> {
        match entry {
            GroupEntry::ValueMemberKey { ge, .. } => ge
                .member_key
                .as_ref()
                .and_then(|key| member_key_parameter(types, key))
                .or_else(|| type_parameter(types, &ge.entry_type)),
            GroupEntry::TypeGroupname { ge, .. } => exact_parameter(types, &ge.name.to_string())
                .or_else(|| generic_args_parameter(types, ge.generic_args.as_ref())),
            GroupEntry::InlineGroup { group, .. } => group_parameter(types, group),
        }
    }

    fn group_parameter(types: &IntermediateTypes, group: &Group) -> Option<String> {
        group.group_choices.iter().find_map(|choice| {
            choice
                .group_entries
                .iter()
                .find_map(|(entry, _)| group_entry_parameter(types, entry))
        })
    }

    fn type2_parameter(types: &IntermediateTypes, type2: &Type2) -> Option<String> {
        match type2 {
            Type2::Typename {
                ident,
                generic_args,
                ..
            }
            | Type2::Unwrap {
                ident,
                generic_args,
                ..
            }
            | Type2::ChoiceFromGroup {
                ident,
                generic_args,
                ..
            } => exact_parameter(types, &ident.to_string())
                .or_else(|| generic_args_parameter(types, generic_args.as_ref())),
            Type2::TaggedData { tag, t, .. } => tag
                .as_ref()
                .and_then(|constraint| match constraint {
                    token::TagConstraint::Type(raw) => exact_parameter(types, raw),
                    token::TagConstraint::Literal(_) => None,
                })
                .or_else(|| type_parameter(types, t)),
            Type2::DataMajorType { constraint, .. } => {
                constraint.as_ref().and_then(|constraint| match constraint {
                    token::TagConstraint::Type(raw) => exact_parameter(types, raw),
                    token::TagConstraint::Literal(_) => None,
                })
            }
            Type2::ParenthesizedType { pt, .. } => type_parameter(types, pt),
            Type2::Map { group, .. }
            | Type2::Array { group, .. }
            | Type2::ChoiceFromInlineGroup { group, .. } => group_parameter(types, group),
            _ => None,
        }
    }

    fn type1_parameter(types: &IntermediateTypes, type1: &Type1) -> Option<String> {
        type2_parameter(types, &type1.type2).or_else(|| {
            type1
                .operator
                .as_ref()
                .and_then(|operator| type2_parameter(types, &operator.type2))
        })
    }

    fn type_parameter(types: &IntermediateTypes, ty: &Type) -> Option<String> {
        ty.type_choices
            .iter()
            .find_map(|choice| type1_parameter(types, &choice.type1))
    }

    type_parameter(types, ty)
}
