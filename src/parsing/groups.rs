use super::directives::{
    group_entry_rule_metadata, inline_group_occurrence_metadata,
    reject_inline_group_occurrence_directives, reject_named_plain_group_occurrence_directives,
};
use super::{
    ArmIdentClaimant, SUPPORTED_GENERIC_DEF_BODIES, anon_composite_member_name,
    arm_ident_collision, combine_comments, exact_homogeneous_array_length_rejection,
    generic_instance_or_new_type, get_comment_after, group_entry_source_desc_for_diagnostic,
    parse_record_from_group_choice, record_plain_group_map_member_rejection,
    reject_custom_codec_on_row_entry, reject_custom_encodings_without_pair,
    reject_duplicates_not_applicable, reject_field_directives_on_single_entry_arm,
    reject_group_choice_arm_ident_collision, reject_group_choice_arm_variant_name_collision,
    reject_ignore_not_applicable, reject_newtype_on_nominal_rule,
    reject_occurrence_on_single_entry_arm, reject_table_domains, rejection_site,
    resolved_plain_group_source_name, rust_type, rust_type_from_type1, settle_arm_variant_name,
    single_arm_array_effective_metadata, source_rule_name_of, type_to_field_name,
    type2_to_fixed_value, well_known_tag_default_duplicates, with_optional_bounds,
};
use crate::cli::Cli;
use crate::comment_ast::{RuleMetadata, merge_metadata, metadata_from_comments};
use crate::intermediate::{
    CBOREncodingOperation, CDDLIdent, ConceptualRustType, EnumVariant, EnumVariantData, FixedValue,
    GenericDef, GenericParamBinding, IntWindow, IntermediateTypes, PlainGroupInfo, Primitive,
    Representation, RustIdent, RustStruct, RustStructType, RustType, VariantIdent,
};
use crate::utils::{
    append_number_if_duplicate, convert_to_camel_case, convert_to_snake_case,
    is_identifier_reserved,
};
use cddl::ast::parent::ParentVisitor;
use cddl::ast::{
    CDDLType, Comments, Group, GroupChoice, GroupEntry, MemberKey, Occur, OptionalComma, Type2,
    TypeGroupnameEntry,
};
use std::collections::BTreeMap;

/// Possible special cases for groups that can be handled to generate much nicer code
/// instead of treating all groups as structs.
// internal, produced once per group during detection and matched immediately (never stored in
// bulk), so the inter-variant size gap doesn't matter; boxing a `RustType` field here would only
// obscure the arms.
#[allow(clippy::large_enum_variant)]
pub(super) enum GroupParsingType {
    /// Fields are the same e.g. field: [* uint]. The second field is the occurrence-count bounds
    /// (`+` / `n*m`) — a LENGTH constraint belonging to the enclosing array type, kept separate
    /// from the element so it can never be misread as an element VALUE bound.
    HomogenousArray(RustType, Option<IntWindow>),
    /// An RFC 8610 repeated plain group in a named ARRAY. The element still uses the ordinary
    /// Array representation (and therefore its existing flat embedded-group codec), but the
    /// owning rule must be a `Wrapper` rather than a transparent collection alias: only the
    /// wrapper owns a standalone codec for the flattened wire shape.
    FlatGroupArray(RustType, Option<IntWindow>),
    /// Pairs are the same e.g. field:{ *text => uint }. The third field is the occurrence-count
    /// bounds (a cardinality constraint on the table itself). `None` is the unbounded `*` table;
    /// `+` / `1*` retains `NonEmptyMap` and every other representable window uses `BoundedMap`.
    HomogenousMap(RustType, RustType, Option<IntWindow>),
    /// Fields are different - needs new struct created e.g. field: [a: uint, b: bstr]
    /// This case covers both maps and arrays
    Heterogenous,
    /// Special case for single basic group e.g. field: `[basic_group]`, field: `{basic_group}`
    /// The tuple type will already have the basic override set so can be directly used
    /// to generate (de)serialiation codegen.
    WrappedBasicGroup(RustType),
}

/// `BoundedVec` and `BoundedMap` carry occurrence endpoints as `u64` const arguments. Reject a
/// parser value that cannot fit that target-independent carrier before later codegen reaches a
/// narrowing conversion (where an `expect` would turn malformed input into a panic).
fn reject_out_of_range_occurrence_bounds(types: &mut IntermediateTypes, bounds: Option<IntWindow>) {
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

/// Normalize one dynamic sequence's occurrence to the inclusive carrier window used by a
/// homogeneous collection. `None` is RFC 8610's exact-once `1..=1`; `*` / `0*` are loose; `+` /
/// `1*` retain the established `1..` NonEmpty carrier; every other spelling retains both endpoints
/// for its Bounded carrier. Dynamic map rows and array tails store the already-normalized `u64`
/// window in the IR because that is the public carrier's const-generic domain — no emitter may
/// reinterpret an occurrence marker or silently widen it later.
pub(super) fn normalized_dynamic_sequence_occurrence_window(
    types: &mut IntermediateTypes,
    occur: Option<&Occur>,
) -> Option<(u64, u64)> {
    // Omitted occurrences are exactly once, never an implicit loose row.
    let source_bounds = occur.map_or((Some(1), Some(1)), occur_bounds);
    reject_out_of_range_occurrence_bounds(types, Some(source_bounds));
    let min = source_bounds
        .0
        .unwrap_or(0)
        .try_into()
        // `reject_out_of_range_occurrence_bounds` recorded the real diagnostic.  Keep parsing on a
        // harmless carrier so finalize can return that graceful aggregate error rather than letting
        // a narrowing conversion turn malformed CDDL into a generator abort.
        .unwrap_or(0);
    let max = source_bounds
        .1
        .map(|upper| upper.try_into().unwrap_or(u64::MAX))
        .unwrap_or(u64::MAX);
    ((min, max) != (0, u64::MAX)).then_some((min, max))
}

/// The `(min, max)` window an occurrence marker admits: `*` is `(None, None)`, `+` is
/// `(Some(1), None)`, `?` is `(None, Some(1))`, and `n*m` keeps its bounds with a zero lower bound
/// dropped.
fn occur_bounds(occur: &Occur) -> IntWindow {
    match occur {
        Occur::ZeroOrMore { .. } => (None, None),
        Occur::Exact { lower, upper, .. } => (
            lower.filter(|lower| *lower != 0).map(|lower| lower as i128),
            upper.map(|upper| upper as i128),
        ),
        Occur::Optional { .. } => (None, Some(1)),
        Occur::OneOrMore { .. } => (Some(1), None),
    }
}

/// The occurrence marker on a group entry. An inline group's own marker is not an entry
/// occurrence, so it reads as none.
pub(super) fn group_entry_occur<'a>(entry: &'a GroupEntry) -> Option<&'a Occur> {
    match entry {
        GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
        GroupEntry::TypeGroupname { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
        GroupEntry::InlineGroup { .. } => None,
    }
}

/// Whether a group entry's occurrence admits a count other than zero-or-one: any marker except `?`
/// and the pedantic `1*1`.
pub(super) fn occurrence_permits_count(entry: &GroupEntry) -> bool {
    group_entry_occur(entry).is_some_and(|occur| {
        !matches!(
            occur,
            Occur::Optional { .. }
                | Occur::Exact {
                    lower: Some(1),
                    upper: Some(1),
                    ..
                }
        )
    })
}

/// Whether a single-choice inline group carrying this occurrence marker may be spliced into the
/// parent entry list (pure grouping) rather than kept unflattened for downstream rejection.
///
/// Splicing DISCARDS the marker, narrowing the group to exactly-once. That is only sound when the
/// marker already means exactly-once — `None` or `1*1` (any representation) — OR, on the MAP side,
/// when the lower bound is ≥ 1: under unique map keys `+` / `n*m` collapse to exactly-one, so a
/// mandatory field preserves the map-side collapse implemented below.
/// Every zero-permitting marker
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
pub(super) fn inline_group_occurrence_flattens(occur: Option<&Occur>, rep: Representation) -> bool {
    match occur {
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
pub(super) fn flatten_group_entries<'a>(
    entries: &'a [(GroupEntry<'a>, OptionalComma<'a>)],
    rep: Representation,
) -> Vec<&'a (GroupEntry<'a>, OptionalComma<'a>)> {
    let mut out = Vec::new();
    for entry in entries {
        match &entry.0 {
            GroupEntry::InlineGroup { occur, group, .. }
                if group.group_choices.len() == 1
                    && inline_group_occurrence_flattens(occur.as_ref().map(|o| &o.occur), rep) =>
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

/// Parses which type of group it is for various common special cases to handle.
///
/// `rule_name` is the enclosing rule when there is one (the named-rule path through
/// `parse_group_choice`); `None` for anonymous nested composites (`rust_type_from_type2`'s
/// `Type2::Array` / `Type2::Map` arms), where rejection messages describe the entry instead of
/// citing a rule.
pub(super) fn parse_group_type<'a>(
    types: &mut IntermediateTypes,
    parent_visitor: &'a ParentVisitor,
    group_choice: &'a GroupChoice<'a>,
    rep: Representation,
    rule_name: Option<&RustIdent>,
    generic_definition: bool,
    cli: &Cli,
) -> GroupParsingType {
    let entries = flatten_group_entries(&group_choice.group_entries, rep);
    match rep {
        Representation::Array => {
            // RFC 8610 repeats a parenthesized GROUP by concatenating its members into the outer
            // array: `[* (a: uint, b: tstr)]` writes `[a, b, a, b]`, not `[[a, b], [a, b]]`.
            // The existing structural Array codec already selects `serialize_as_embedded_group` /
            // `deserialize_as_embedded_group` for a materialized plain-group element, including
            // its fixed-width header arithmetic. What it cannot provide through an alias is a
            // standalone codec: `pub type Pairs = Vec<Pair>` would dispatch to Vec and nest each
            // Pair. Materialize one internal plain group and tell the named-rule seam to make the
            // outer owner a Wrapper over that ordinary structural Array.
            //
            // This is intentionally only a named ARRAY rule. An anonymous occurrence has no
            // nominal codec owner, and a repeated inline group mixed with other entries remains a
            // source-level boundary rather than silently reinterpreting the surrounding record.
            if let (
                Some(owner),
                [
                    (
                        GroupEntry::InlineGroup {
                            occur: Some(occur),
                            group,
                            comments_after_group,
                            ..
                        },
                        optional_comma,
                    ),
                ],
            ) = (rule_name, entries.as_slice())
                && group.group_choices.len() == 1
            {
                // The synthesized `OwnerItem` is a concrete record. A generic definition would
                // have to retain it as a definition-owned template and substitute the outer
                // parameters into every instance; materializing it here leaves unresolved
                // generic parameters in a non-generic struct and later aborts in generation.
                // Keep the boundary at the source seam until that template ownership exists.
                if generic_definition {
                    types.record_rejection(format!(
                        "generic rule `{}`: a repeated inline group (`[* (…)]` / `+` / `?` / `n*m`) \
                         is unsupported because generic substitution for its synthesized repeated-group \
                         item is not supported. Name a non-generic concrete rule at the use site instead.",
                        source_rule_name_of(types, owner)
                    ));
                    return GroupParsingType::HomogenousArray(
                        ConceptualRustType::Primitive(Primitive::U64).into(),
                        None,
                    );
                }
                let bounds = occur_bounds(&occur.occur);
                reject_out_of_range_occurrence_bounds(types, Some(bounds));
                let item_ident = RustIdent::new(CDDLIdent::new(format!("{owner}Item")));
                let entry_metadata =
                    inline_group_occurrence_metadata(comments_after_group, optional_comma);
                if reject_inline_group_occurrence_directives(
                    types,
                    owner,
                    &item_ident,
                    &entry_metadata,
                ) {
                    return GroupParsingType::HomogenousArray(
                        ConceptualRustType::Primitive(Primitive::U64).into(),
                        Some(bounds),
                    );
                }
                // Every authored rule ident is scope-marked before parsing begins, which makes this
                // check source-order independent. A derived public item name must never take a
                // numeric suffix: an unrelated rule edit must not change the generated API.
                if types.generated_type_ident_is_claimed(&item_ident) {
                    let owner_source = source_rule_name_of(types, owner);
                    let claimant = types
                        .source_rule_name(&item_ident)
                        .unwrap_or(item_ident.as_ref());
                    types.record_rejection(format!(
                        "rule `{owner_source}`: repeated inline group materialization needs the generated \
                         item type `{item_ident}`, but that name is already claimed by `{claimant}`. Rename \
                         the authored claimant; generated flat-group item names are stable public API and \
                         cannot take an order-dependent numeric suffix."
                    ));
                    return GroupParsingType::HomogenousArray(
                        ConceptualRustType::Primitive(Primitive::U64).into(),
                        Some(bounds),
                    );
                }
                // A directory input owns every file's generated module independently. The flat
                // group's public item is part of its owner's API, so give it the owner's module
                // scope before it materializes; otherwise it falls back to generated root while
                // the wrapper that names it lives in (say) `generated/flat`.
                let item_scope = types.scope(owner).clone();
                types.mark_scope(item_ident.clone(), item_scope);
                types.mark_plain_group(
                    item_ident.clone(),
                    // This internal item is already being materialized below, unlike a freely
                    // defined group rule whose AST body must be retained until a later use picks
                    // its representation. `None` is the existing "already materialized" plain
                    // group marker used by group-choice arms.
                    PlainGroupInfo::new(None, RuleMetadata::default()),
                );
                parse_group(
                    types,
                    parent_visitor,
                    group,
                    &item_ident,
                    Representation::Array,
                    None,
                    None,
                    &RuleMetadata::default(),
                    cli,
                );
                let item_type: RustType = ConceptualRustType::Rust(item_ident.clone()).into();
                // The zero-width repetition check runs in `parse_group_choice`, which covers this
                // inline spelling and the named one alike.
                return GroupParsingType::FlatGroupArray(item_type, Some(bounds));
            }
            // An unflattened `InlineGroup` here is a parenthesized group carrying an occurrence
            // marker that would be silently narrowed (`[* (int, tstr)]`), or a multi-choice group.
            // Fall through to `Heterogenous` so `parse_record_from_group_choice` rejects it
            // gracefully rather than panicking on the unsupported element.
            if entries.len() == 1 && !matches!(entries[0].0, GroupEntry::InlineGroup { .. }) {
                let (entry, optional_comma) = entries[0];
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
                let bounds = occur.as_ref().map(|o| occur_bounds(&o.occur));
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
                // The named plain-group spelling (`pair = (a, b)`, `pairs = [* pair]`) shares the
                // inline form's flat wire algorithm. Keep exact once on the existing splice path;
                // every other occurrence is a nominal wrapper so `pairs` owns its standalone codec.
                let repeated_plain_group_source =
                    (!matches!(bounds, None | Some((Some(1), Some(1)))))
                        .then(|| resolved_plain_group_source_name(types, &elem_type))
                        .flatten();
                let is_repeated_plain_group = repeated_plain_group_source.is_some();
                if let Some(group_source) = repeated_plain_group_source {
                    let Some(owner) = rule_name else {
                        types.record_rejection(format!(
                            "inline array: the repeated plain group `{group_source}` has RFC 8610 flat \
                             concatenation semantics, but this anonymous array has no nominal owner for the \
                             standalone flat-group codec. Name this array as its own rule (for example \
                             `pairs = [* {group_source}]`) or frame each group as an array item before repeating it."
                        ));
                        return GroupParsingType::HomogenousArray(elem_type, bounds);
                    };
                    let entry_metadata = group_entry_rule_metadata(entry, optional_comma);
                    if reject_named_plain_group_occurrence_directives(
                        types,
                        owner,
                        &group_source,
                        &entry_metadata,
                    ) {
                        return GroupParsingType::HomogenousArray(
                            ConceptualRustType::Primitive(Primitive::U64).into(),
                            bounds,
                        );
                    }
                }
                match bounds {
                    // no bounds
                    Some((None, None)) => {
                        return if is_repeated_plain_group {
                            GroupParsingType::FlatGroupArray(elem_type, None)
                        } else {
                            GroupParsingType::HomogenousArray(elem_type, None)
                        };
                    }
                    None => {
                        // if the only element is a basic group we don't need to create a new group but can just
                        // change how it is (de)serialized
                        if elem_type.is_basic(types)
                            && matches!(
                                elem_type.conceptual_type.resolve_alias_shallow(),
                                ConceptualRustType::Rust(_)
                            )
                        {
                            return GroupParsingType::WrappedBasicGroup(elem_type.not_basic());
                        }
                        // fall-through generic case. this is a general 1-element struct that needs creating
                    }
                    // An explicit `1*1` is semantically an occurrence collection even though its
                    // wire count equals an unmarked one-item record. Preserve that authored
                    // collection identity: a named `one = [1*1 uint]` consequently registers the
                    // same exact static `[u64; 1]` alias as every other exact homogeneous window.
                    // Two shapes intentionally retain the record/splice path: a fixed value has
                    // no storable array element type, and a plain group must serialize flat rather
                    // than as a nested `[Group; 1]`. An exactly-once member in a heterogeneous
                    // record likewise remains scalar.
                    Some((Some(1), Some(1))) => {
                        if matches!(
                            elem_type.conceptual_type.resolve_alias_shallow(),
                            ConceptualRustType::Fixed(_)
                        ) {
                            // Fall through to the one-member record lowering below.
                        } else if elem_type.is_basic(types)
                            && matches!(
                                elem_type.conceptual_type.resolve_alias_shallow(),
                                ConceptualRustType::Rust(_)
                            )
                        {
                            return GroupParsingType::WrappedBasicGroup(elem_type.not_basic());
                        } else {
                            return GroupParsingType::HomogenousArray(
                                elem_type,
                                Some((Some(1), Some(1))),
                            );
                        }
                    }
                    Some(bounds) => {
                        return if is_repeated_plain_group {
                            GroupParsingType::FlatGroupArray(elem_type, Some(bounds))
                        } else {
                            GroupParsingType::HomogenousArray(elem_type, Some(bounds))
                        };
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
            // it is a 1-field struct. Table detection therefore requires a NON-fixed key type.
            // Fixed keys lower through `parse_record_from_group_choice` and `group_entry_map_key_kind`.
            // That classification accepts uint/text and gracefully rejects nint/float/bool.
            // This avoids a `Fixed`-domain table that panics in `for_rust_member`.
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
                                    let occ_bounds =
                                        ge.occur.as_ref().map(|o| occur_bounds(&o.occur));
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
                                    reject_table_domains(
                                        types,
                                        rule_name,
                                        t1,
                                        &ge.entry_type,
                                        &key_type,
                                        &value_type,
                                    );
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
                                unreachable!(
                                    "the cddl parser never constructs MemberKey::NonMemberKey: {ge:?}"
                                )
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
                                    reject_table_domains(
                                        types,
                                        rule_name,
                                        t1,
                                        &ge.entry_type,
                                        &key_type,
                                        &value_type,
                                    );
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

pub(super) fn group_entry_to_field_name(
    entry: &GroupEntry,
    index: usize,
    already_generated: &mut BTreeMap<String, u32>,
    optional_comma: &OptionalComma,
) -> String {
    // An explicit `@name` from the entry's own trailing comment and the comma's.
    let explicit_name = |trailing_comments: &Option<Comments>| {
        let combined_comments =
            combine_comments(trailing_comments, &optional_comma.trailing_comments);
        metadata_from_comments(&combined_comments.unwrap_or_default()).name
    };
    let field_name = convert_to_snake_case(&match entry {
        GroupEntry::ValueMemberKey {
            trailing_comments,
            ge,
            ..
        } => match ge.member_key.as_ref() {
            Some(member_key) => match member_key {
                MemberKey::Value { value, .. } => explicit_name(trailing_comments)
                    // a quoted text key `"a":` is sugar for the bareword key `a:` (same wire
                    // key), so it must converge on the bareword field name, not `key_"a"`
                    // (which is invalid Rust). Non-text values keep the `key_{value}` fallback.
                    .unwrap_or_else(|| match value {
                        cddl::token::Value::TEXT(t) => t.to_string(),
                        _ => format!("key_{value}"),
                    }),
                MemberKey::Bareword { ident, .. } => {
                    // Honor a `@name` directive the same way the Value/Type1 arms do; otherwise the
                    // directive is silently dropped on bareword-keyed entries (the same directive-drop
                    // bug class the Type1 arm below fixes for arrow keys).
                    explicit_name(trailing_comments).unwrap_or_else(|| ident.to_string())
                }
                MemberKey::Type1 { t1, .. } => {
                    // An integer arrow key `0 => x` is the Type1 spelling of the value key `0: x`, so
                    // honor a @name directive the same way the Value arm above does (falling back to
                    // key_{value}); otherwise the directive is silently dropped on arrow-keyed entries.
                    explicit_name(trailing_comments).unwrap_or_else(|| match &t1.type2 {
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
                    })
                }
                MemberKey::NonMemberKey { .. } => {
                    unreachable!(
                        "the cddl parser never constructs MemberKey::NonMemberKey: {entry:?}"
                    )
                }
            },
            None => type_to_field_name(&ge.entry_type).unwrap_or_else(|| {
                explicit_name(trailing_comments).unwrap_or_else(|| format!("index_{index}"))
            }),
        },
        GroupEntry::TypeGroupname {
            trailing_comments,
            ge: TypeGroupnameEntry { name, .. },
            ..
        } => match is_identifier_reserved(&name.to_string()) {
            true => explicit_name(trailing_comments).unwrap_or_else(|| format!("index_{index}")),
            false => name.to_string(),
        },
        GroupEntry::InlineGroup { group, .. } => panic!(
            "not implemented (define a new struct for this!) = {}\n\n {:?}",
            group, group
        ),
    });
    append_number_if_duplicate(already_generated, field_name)
}

pub(super) fn group_entry_to_raw_field_name(entry: &GroupEntry) -> Option<String> {
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
        } => match is_identifier_reserved(&name.to_string()) {
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

/// A heterogeneous inline array or map in a TYPE position (`f: [a: uint, b: tstr]`, `f: { a:
/// uint }`) is a record, and a record needs a nominal name. The `@name` comment on the composite is
/// the naming door; it reaches here from the type2's own comment slot or, at member position, from
/// the entry slot one level further out (`anon_composite_member_name`). With no name the position
/// is refused once per node (`rejection_kind`, `rejection`) with an inert placeholder, so
/// `finalize` reports it beside anything else the walk finds. `rewalk_from` is the rejection mark
/// taken before a classification walk over the same entries (the array arm's `parse_group_type`),
/// so a diagnostic both walks record is reported once.
#[allow(clippy::too_many_arguments)]
pub(super) fn lower_anonymous_record(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type2: &Type2,
    group: &Group,
    rep: Representation,
    rejection_kind: &'static str,
    rejection: &str,
    rewalk_from: Option<crate::intermediate::RejectionMark>,
    cli: &Cli,
) -> RustType {
    let mut rule_metadata = RuleMetadata::from(
        get_comment_after(parent_visitor, &CDDLType::from(type2), None).as_ref(),
    );
    if rule_metadata.name.is_none() {
        rule_metadata.name = anon_composite_member_name(parent_visitor, type2);
    }
    let Some(name) = rule_metadata.name.as_ref() else {
        types.record_rejection_once_at(type2, rejection_kind, rejection.to_owned());
        return ConceptualRustType::Fixed(FixedValue::Null).into();
    };
    let cddl_ident = CDDLIdent::new(name);
    let rust_ident = RustIdent::new(cddl_ident.clone());
    let construction_mark = types.rejection_mark();
    parse_group(
        types,
        parent_visitor,
        group,
        &rust_ident,
        rep,
        None,
        None,
        &rule_metadata,
        cli,
    );
    if let Some(classification_mark) = rewalk_from {
        types.drop_rewalk_repeats(classification_mark, construction_mark);
    }
    types.new_type(&cddl_ident, cli)
}

/// Materialize the plain group a type references with representation `rep` (`[* kv]`, `[kv]`, a
/// record field `kv`), resolving an alias first: an alias is transparent, so `kv_alias` must
/// register the group exactly like `kv` — matching the bare `Rust(ident)` only left the group
/// unregistered and the emitted alias chain dangling on a struct that was never defined. A generic
/// parameter is substituted later and left alone. Returns the resolved ident when the type names
/// one, for a caller that classifies it further.
pub(super) fn materialize_plain_group_ref<'t>(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    ty: &'t RustType,
    rep: Representation,
    cli: &Cli,
) -> Option<&'t RustIdent> {
    if ty.generic_param_binding.is_some() {
        return None;
    }
    let ConceptualRustType::Rust(ident) = ty.conceptual_type.resolve_alias_shallow() else {
        return None;
    };
    types.set_rep_if_plain_group(parent_visitor, ident, rep, cli);
    Some(ident)
}

pub(super) fn group_entry_optional(entry: &GroupEntry) -> bool {
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

pub(super) fn group_entry_to_type(
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
        // Reject gracefully and name the group. A count marker is different: merely naming the
        // group preserves it (`? pair`), which reaches the single-entry-arm occurrence refusal.
        // Optional has an executable type-choice count rewrite; the other count forms have no
        // equivalent supported flat-wire carrier yet, so they must be an explicit boundary rather
        // than share the unmarked group's dead `pair = (...)` advice.
        GroupEntry::InlineGroup { occur, .. } => {
            let message = if matches!(
                occur.as_ref().map(|o| &o.occur),
                Some(Occur::Optional { .. })
            ) {
                "an optional inline group (`? (uint, tstr)`) in entry position is unsupported. \
                 Naming it and writing `? pair` does not repair the empty-wire case. Give the \
                 one-count, empty, and bytes forms their own array rules and select them with a \
                 TYPE choice (`pair_one = [uint, tstr]`, `pair_empty = []`, `bytes_one = [bytes]`, \
                 then `t = pair_one / pair_empty / bytes_one`)."
            } else {
                match occur {
                    Some(_) => {
                        "a count-marked inline group (`* (uint, tstr)`) in entry position is \
                         unsupported. Naming it alone does not repair a repeated flat group, and \
                         cddl-codegen has no equivalent supported carrier for that wire shape yet."
                    }
                    None => {
                        "an inline group (`(uint, tstr)`) in entry position is unsupported. Name \
                         the group instead (e.g. `pair = (uint, tstr)`, then reference `pair`)."
                    }
                }
            };
            types.record_rejection(message.to_string());
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
pub(super) enum MapKeyKind {
    /// `k: v` / `k => 5` — a literal/bareword key we can write and verify.
    Fixed(FixedValue),
    /// `k => v` with a non-literal key type (e.g. `uint => tstr`) — a real table entry. Collapsing
    /// it into an enum variant would silently drop the key type, so it is unsupported here.
    NonFixed,
    /// No member key present on this entry: a bare value (`{ uint // ... }`) or a (plain-)group
    /// reference (`{ foo // ... }`) whose referenced struct owns its own keys.
    Keyless,
}

pub(super) fn group_entry_map_key_kind(entry: &GroupEntry) -> MapKeyKind {
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

#[allow(clippy::too_many_arguments)]
fn parse_group_choice(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    group_choice: &GroupChoice,
    name: &RustIdent,
    rep: Representation,
    tag: Option<usize>,
    generic_params: Option<Vec<GenericParamBinding>>,
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
    let classification_mark = types.rejection_mark();
    let group_parsing_type = parse_group_type(
        types,
        parent_visitor,
        group_choice,
        rep,
        Some(name),
        generic_params.is_some(),
        cli,
    );
    let construction_mark = types.rejection_mark();
    if let GroupParsingType::FlatGroupArray(element_type, bounds) = &group_parsing_type {
        // A transparent collection alias would inherit Vec's standalone codec and nest each group
        // value. The wrapper owns the structural Array's existing flat embedded-group codec.
        if rule_metadata.ignore {
            reject_ignore_not_applicable(types, name);
        }
        // The named and aliased plain-group spellings reach this seam without the member-position
        // walker that normally materializes a plain group, so materialize it here.
        materialize_plain_group_ref(
            types,
            parent_visitor,
            element_type,
            Representation::Array,
            cli,
        );
        // Every repetition must advance the outer decoder. The materialized IR, rather than a
        // syntactic optionality guess, decides whether an embedded decode consumes an item.
        if element_type.expanded_mandatory_field_count(types) == 0 {
            types.record_rejection(format!(
                "rule `{}`: a zero-width repeated group is unsupported — each successful \
                 repetition must consume at least one CBOR item so the flat array decoder can \
                 advance. Make one group member mandatory or use a separately framed array item.",
                source_rule_name_of(types, name)
            ));
        }
        let effective_metadata = single_arm_array_effective_metadata(&rule_metadata, tag, name);
        let array_type = with_optional_bounds(
            ConceptualRustType::Array(Box::new(element_type.clone())).into(),
            *bounds,
        );
        if let Some(Err(length)) = array_type.exact_homogeneous_array_len() {
            types.record_rejection(exact_homogeneous_array_length_rejection(length));
        }
        let rust_struct = RustStruct::new_wrapper(
            name.clone(),
            tag,
            Some(&effective_metadata),
            array_type,
            None,
        );
        match generic_params {
            Some(params) => types.register_generic_def(GenericDef::new(params, rust_struct)),
            None => types.register_rust_struct(parent_visitor, rust_struct, cli),
        };
        return;
    }
    let rust_struct = match group_parsing_type {
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
            // `a = [* kv_alias]` materializes the group exactly like `a = [* kv]`; unresolved, the
            // run exited 0 having emitted `pub type KvAlias = Kv;` with no `Kv` anywhere.
            materialize_plain_group_ref(
                types,
                parent_visitor,
                &element_type,
                Representation::Array,
                cli,
            );
            // A named homogeneous-array rule does not travel through the member `Type1`
            // walker below, so validate its exact occurrence count here as well. Keep the
            // effective rule policy: exact reject sets stay BoundedOrderedSet rather than
            // becoming Rust arrays and therefore have no static-array object-size demand.
            let effective_metadata = single_arm_array_effective_metadata(&rule_metadata, tag, name);
            let array_type =
                RustType::new(ConceptualRustType::Array(Box::new(element_type.clone())))
                    .with_occurrence_bounds(bounds.unwrap_or((None, None)))
                    .with_duplicates_policy(effective_metadata.duplicates);
            if let Some(Err(length)) = array_type.exact_homogeneous_array_len() {
                types.record_rejection(exact_homogeneous_array_length_rejection(length));
            }
            // Covers non-generic set rules and generic single-arm set definitions:
            // a generic def stores the wrapper (param element) as a `GenericDef`, and each
            // instantiation mints one nominal per `<def>_<args>` in `GenericInstance::resolve`.
            let is_set_nominal =
                tag.is_some_and(|t| well_known_tag_default_duplicates(t, true).is_some());
            if rule_metadata.newtype.is_some() || tag.is_some() {
                // generate newtype over array — on `@newtype`, and UNCONDITIONALLY when the rule
                // carries a TAG. A tagged transparent alias (`pub type TaggedArr = Vec<u64>;` with
                // the tag riding the alias entry) mints no type to hang the tag on, so
                // `TaggedArr::to_cbor_bytes` would be `Vec<u64>`'s — writing the BARE array while
                // every embed site of the rule writes `write_tag(n)` first and every embed site's
                // decoder requires it. Same reasoning as the single-type tag rule and the `.cbor`
                // rule body; `register_type_alias`'s wire-facts assert makes the alias spelling
                // unrepresentable rather than merely unused, and `@newtype` is redundant here.
                // The wrapper takes the SAME effective metadata the
                // plain single-arm array path uses so a single-arm tag-258 `@newtype` wrapper
                // (`#6.258([* a]) ; @newtype`) picks up the registry's set-semantics default
                // (reject) and fires the single-arm defaulting notice, exactly as the non-newtype
                // flavor does — no-op for a non-258 tag or an explicit directive. The effective
                // `@duplicates` policy lands in the wrapper's struct config; the register-side
                // `Wrapper` arm then threads it onto the stored inner collection type so generation
                // selects the `OrderedSet` twin.
                let wrapper = RustStruct::new_wrapper(
                    name.clone(),
                    tag,
                    Some(&effective_metadata),
                    with_optional_bounds(
                        ConceptualRustType::Array(Box::new(element_type)).into(),
                        bounds,
                    ),
                    None,
                );
                if is_set_nominal {
                    // A single-arm mandatory-tag 258 SET rule (`#6.258([* a])`) NOMINALIZES into a
                    // `Wrapper` struct owning its `{tag, len, elem}` encodings, exactly
                    // like the two-arm idiom but with a MANDATORY tag (grammar decides the record:
                    // `Option<Sz>`, NOT the two-arm `TagPresenceEncoding`). The registry
                    // set-semantics default (reject) rides `single_arm_array_effective_metadata`
                    // and the `Wrapper` register arm threads it onto the stored inner array type,
                    // selecting the `OrderedSet`/`NonEmptyOrderedSet` twin. `@newtype` carries a
                    // custom getter on the wrapper; a bare set nominal emits no inherent `get()`
                    // (it would shadow `OrderedSet::get(index)` through `Deref`).
                    wrapper.as_set_nominal()
                } else {
                    wrapper
                }
            } else {
                // Array - homogeneous element type with proper occurence operator. A single-arm
                // tag-258 set picks up the registry's reject default via the helper (no-op for a
                // non-258 tag or an explicit directive).
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
                let src = source_rule_name_of(types, name);
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
                let map_type = with_optional_bounds(
                    ConceptualRustType::Map(Box::new(key_type), Box::new(value_type)).into(),
                    bounds,
                );
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
            if rule_metadata.newtype.is_some() {
                reject_newtype_on_nominal_rule(
                    types,
                    name,
                    "a record rule (an array or map of members, `[a: uint, b: tstr]`)",
                );
            }
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
            types.drop_rewalk_repeats(classification_mark, construction_mark);
            // We need to store this in IntermediateTypes so we can refer from one struct to another.
            RustStruct::new_record(name.clone(), tag, Some(&rule_metadata), record)
        }
        GroupParsingType::FlatGroupArray(_, _) => unreachable!("handled above"),
    };
    match generic_params {
        Some(params) => types.register_generic_def(GenericDef::new(params, rust_struct)),
        None => types.register_rust_struct(parent_visitor, rust_struct, cli),
    };
}

#[allow(clippy::too_many_arguments)]
fn lower_single_entry_group_choice_arm(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    group_choice: &GroupChoice,
    name: &RustIdent,
    rep: Representation,
    rule_metadata: &RuleMetadata,
    choice_context: &crate::intermediate::VariantMintContext,
    i: usize,
    cli: &Cli,
) -> EnumVariant {
    let (group_entry, entry_comma) = group_choice.group_entries.first().unwrap();
    // An occurrence-carrying arm is refused as a SHAPE, so its member's directives
    // describe a member that will not exist — one message per problem, and the one
    // to give is the one whose remedy rewrites the arm. In MAP rep a lower-bound-≥1
    // marker is honored by collapse (the shared `inline_group_occurrence_flattens`
    // boundary) and refuses nothing, so a directive on `{ x: uint // + kv }` still
    // reaches the validation below — which is exactly right: that arm generates.
    let occurrence_refused = reject_occurrence_on_single_entry_arm(types, name, group_entry, rep);
    let ty = group_entry_to_type(types, parent_visitor, group_entry, cli);
    // The directive validation runs AFTER the member's type parse, because the
    // `@name` verdict is the anon-array reader's own effect observed on `ty` rather
    // than a second derivation of that reader's scope.
    if !occurrence_refused {
        reject_field_directives_on_single_entry_arm(types, name, group_entry, entry_comma, &ty);
    }
    // Resolve aliases first: an alias is transparent, so an arm spelled
    // `kv_alias` must materialize and embed the plain group exactly like the
    // direct `kv` arm. Reading the bare `Rust(ident)` skipped both the
    // registration and the embedded classification for the alias spelling, which
    // aborted the ARRAY rep on an unmaterialized struct and pushed the MAP rep's
    // keyless arm — a supported shape whose referenced struct owns its own keys —
    // into the no-key rejection.
    let serialize_as_embedded =
        match materialize_plain_group_ref(types, parent_visitor, &ty, rep, cli) {
            // manual match in case we expand operaitons later
            Some(ident) => {
                types.is_plain_group(ident)
                    && !ty.encodings.iter().any(|enc| match enc {
                        CBOREncodingOperation::Tagged(_) => true,
                        CBOREncodingOperation::OptionallyTagged(_) => true,
                        CBOREncodingOperation::CBORBytes => true,
                    })
            }
            None => false,
        };
    // A single-entry arm registers no record at all — its type goes straight into
    // the variant — so the name settled here is the ONLY name it ever claims.
    let (ident_name, explicit_name) = match rule_metadata.name.clone() {
        Some(explicit) => (explicit, true),
        None => match group_entry_to_raw_field_name(group_entry) {
            Some(field_name) => (field_name, false),
            // A BARE member has no key to name the variant after, so the shared
            // fixed-value minter supplies its legacy spelling or canonical fallback.
            None => (ty.conceptual_type.for_variant().to_string(), false),
        },
    };
    let variant_ident = VariantIdent::new_custom(settle_arm_variant_name(
        types,
        choice_context,
        i + 1,
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
                let source_name = source_rule_name_of(types, name);
                let group_name = match ty.conceptual_type.resolve_alias_shallow() {
                    ConceptualRustType::Rust(group_ident) => {
                        source_rule_name_of(types, group_ident)
                    }
                    // unreachable while `is_basic` is the guard, which only
                    // says true for a `Rust` ident — kept total rather than
                    // asserted, since the message is the whole point here.
                    _ => ty.conceptual_type.for_variant().to_string(),
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
                let source_name = source_rule_name_of(types, name);
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
                let source_name = source_rule_name_of(types, name);
                types.record_rejection(format!(
                    "rule `{source_name}`: a map group-choice arm has an entry with \
                     no key. Each map entry needs a key: use `k: v` / `k => v`, or a \
                     table `{{ * k => v }}`."
                ));
                None
            }
            MapKeyKind::NonFixed => {
                let source_name = source_rule_name_of(types, name);
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
        rule_metadata.doc.clone(),
    )
    .with_key(variant_key)
}

#[allow(clippy::too_many_arguments)]
fn lower_record_group_choice_arm(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    group_choice: &GroupChoice,
    name: &RustIdent,
    rep: Representation,
    generic_params: &Option<Vec<GenericParamBinding>>,
    rule_metadata: &RuleMetadata,
    choice_context: &crate::intermediate::VariantMintContext,
    i: usize,
    cli: &Cli,
) -> EnumVariant {
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
        Some(_) => types.fresh_synthesized_ident(&format!("{name}_group_choice_arm_{ident_name}")),
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
        choice_context,
        i + 1,
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
        let embedded_record = match types.remove_rust_struct(&register_under).unwrap().variant {
            RustStructType::Record(record) => record,
            _ => unreachable!(),
        };
        EnumVariant::new_embedded(variant_display, embedded_record, rule_metadata.doc.clone())
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
                    .is_some_and(|(claimed, ours)| claimed.structurally_equivalent(ours)) =>
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
            rule_metadata.doc.clone(),
        )
    }
}

#[allow(clippy::too_many_arguments)]
pub fn parse_group(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    group: &Group,
    name: &RustIdent,
    rep: Representation,
    tag: Option<usize>,
    generic_params: Option<Vec<GenericParamBinding>>,
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
        if parent_rule_metadata.newtype.is_some() {
            reject_newtype_on_nominal_rule(
                types,
                name,
                "a group-choice rule (`[ a // b ]`, `{ a // b }`)",
            );
            return;
        }
        // Generate Enum object that is not exposed to wasm, since wasm can't expose
        // fully featured rust enums via wasm_bindgen

        // TODO: We don't support generating SerializeEmbeddedGroup for group choices which is necessary for plain groups
        // It would not be as trivial to add as we do the outer group's array/map tag writing inside the variant match
        // to avoid having to always generate SerializeEmbeddedGroup when not necessary.
        assert!(
            !types.is_plain_group(name),
            "a plain group with group choices is refused by the plain-group pre-scan before parsing"
        );

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
        let choice_context = crate::intermediate::VariantMintContext::GroupChoice(name.clone());
        for (arm_idx, group_choice) in group.group_choices.iter().enumerate() {
            if let Some(explicit) =
                RuleMetadata::from(group_choice.comments_before_grpchoice.as_ref()).name
            {
                let emitted = convert_to_camel_case(&explicit);
                if let Some(first) = types.reserve_explicit_variant_mint(
                    &choice_context,
                    arm_idx + 1,
                    explicit.clone(),
                    emitted.clone(),
                ) {
                    reject_group_choice_arm_variant_name_collision(
                        types,
                        name,
                        &first.source_name,
                        &explicit,
                        &emitted,
                    );
                }
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
                    lower_single_entry_group_choice_arm(
                        types,
                        parent_visitor,
                        group_choice,
                        name,
                        rep,
                        &rule_metadata,
                        &choice_context,
                        i,
                        cli,
                    )
                } else {
                    lower_record_group_choice_arm(
                        types,
                        parent_visitor,
                        group_choice,
                        name,
                        rep,
                        &generic_params,
                        &rule_metadata,
                        &choice_context,
                        i,
                        cli,
                    )
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
        reject_wasm_group_choice_getter_collisions(types, cli, name, &variants);
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_group_choice(name.clone(), tag, Some(&rule_metadata), variants, rep),
            cli,
        );
    }
}

/// LOCKSTEP with `generation::enums::add_wasm_enum_getters`: this claim inventory must mirror every
/// legacy base and materialized field-qualified getter that emitter adds. Field-qualified WASM enum
/// doors use `as_<variant>_<field>`. Variant names and inlined-field
/// names are both author-controllable (`@name`), and their normalized concatenations occupy one
/// inherent-method namespace on the enum wrapper.  Rust's variant-name ledger proves only the
/// *variant* segment unique; it cannot prove `Foo` + `Bar` differs from `FooBar`'s legacy
/// `as_foo_bar()`.  Reject that WASM-only API collision before code emission instead of silently
/// suffixing a public door or leaving wasm-bindgen to report duplicate methods.
fn reject_wasm_group_choice_getter_collisions(
    types: &mut IntermediateTypes,
    cli: &Cli,
    name: &RustIdent,
    variants: &[EnumVariant],
) {
    if !cli.wasm {
        return;
    }
    let mut claimed: BTreeMap<String, String> = BTreeMap::new();
    let mut claim = |method: String, owner: String| {
        if let Some(first) = claimed.insert(method.clone(), owner.clone()) {
            let source_name = source_rule_name_of(types, name);
            types.record_rejection(format!(
                "rule `{source_name}`: group-choice arm getter `{method}()` for {owner} collides \
                 with the getter for {first}. Rename one arm or member with `; @name <other>`; the \
                 CBOR wire form is unchanged. The generator does not suffix this public WASM door \
                 automatically."
            ));
        }
    };
    for variant in variants {
        let variant_name = variant.name_as_var();
        match &variant.data {
            EnumVariantData::RustType(ty) if !ty.conceptual_type.is_fixed_value() => {
                claim(
                    format!("as_{variant_name}"),
                    format!("arm `{}`", variant.name),
                );
            }
            EnumVariantData::Inlined(record) => {
                let fields = record
                    .fields
                    .iter()
                    .filter(|field| {
                        !field.rust_type.conceptual_type.is_fixed_value() || field.optional
                    })
                    .collect::<Vec<_>>();
                if let Some(field) = fields
                    .iter()
                    .copied()
                    .find(|field| !field.rust_type.conceptual_type.is_fixed_value())
                    .or_else(|| (fields.len() == 1).then(|| fields[0]))
                {
                    claim(
                        format!("as_{variant_name}"),
                        format!("arm `{}` field `{}`", variant.name, field.name),
                    );
                }
                if fields.len() > 1 {
                    for field in fields {
                        claim(
                            format!("as_{variant_name}_{}", field.name),
                            format!("arm `{}` field `{}`", variant.name, field.name),
                        );
                    }
                }
            }
            EnumVariantData::RustType(_) => {}
        }
    }
}
